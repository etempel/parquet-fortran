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

  6. print_stat's columns match its documentation.
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

Enforces sixteen structural invariants that no compiler and no
runtime test can see -- each one's violation compiles cleanly, passes the whole suite, and fails
somewhere else entirely. Each names the feature_risks.md (feature_risks.md) entry it protects, in
its own docstring and in the message it prints:

  * `parquet_table` must gain no allocatable component. The type is finalizable, and this project
    has three confirmed compiler bugs in exactly the `intent(out)`/`FINAL` machinery on exactly this
    type, so new table state goes on `parquet_table_cache` instead (see New `parquet_table` state
    goes on the CACHE (CLAUDE.md#new-parquet_table-state-goes-on-the-cache--never-as-an-allocatable-
    component-of-the-type)).
  * Every pointer a table accessor hands out must be reached through `self%cache`. Pointing at
    `self` directly makes the caller's table need the `target` attribute, and `target` is a
    requirement on the caller -- so the library still compiles, every test still passes, and only a
    user's program corrupts memory.
  * The generated files must carry the doc-comments and `! GCOVR_EXCL_LINE` markers their generators
    are supposed to emit. The generators' own `--check` modes compare the committed file against the
    generator, so a template that drops a convention produces output that matches perfectly and is
    wrong in every kind it emits at once.
  * A schema-less `parquet_write_table` must declare `col_size:`/`array_size:` as `auto`, never a
    measured value. Declaring `auto` is what makes the generated schema unable to get a size wrong,
    since the writer resolves both from the data. A round-trip test cannot see a change here: the
    sidecar MAML is emitted at close, after the writer has resolved the real numbers, so it still
    looks correct.
  * Every procedure that calls `check_row_group_valid` must also call `check_reader_no_sort`. A sort
    permutation destroys row-group locality, so a row-group-scoped read that forgets the guard
    returns the right number of rows and the wrong ones. Pairing the two turns "did we guard all ~19
    sites?" into something mechanical, and it extends itself -- a new row-group-scoped read
    validates its row group as a matter of course.
  * `print_stat`'s columns must match the table documenting them in `doc/pages/io/reading.md`. That
    table is the format's only contract: exactly one assertion in the whole suite touches the output
    text, so a renamed column breaks no test. The two had already drifted when this check was added.
  * `parquet_print_settings`'s rows must all be named in `doc/pages/operating/settings.md`. Same
    reasoning as `print_stat` above. Rows are matched by the shape of the call rather than against a
    list of helper names, because the list version went blind twice as new row helpers were added --
    the second time reporting three of five new rows as documented when the check simply could not
    see them.
  * Every `cfg_*` setting must be read somewhere other than where it is written. A setting nothing
    consults still round-trips through its own getter and still reports the right factory value,
    while doing nothing at all (Risk-41 (feature_risks.md)). This catches "nothing reads it"; the
    observed-effect tests in `test/test_settings.f90` catch "reads it wrongly".
  * Nothing in `src/` writes to a unit directly outside the three emit channels. Routing every
    message through `parquet_emit_info`/`_warning`/`_error_context` is what makes `verbosity` and
    `message_stream` apply everywhere. A print written the old way still appears at default
    settings, so the suite stays green and only users who changed a setting are affected.
  * The row-group sizing arithmetic must exist once. Its two callers serve different writers (a
    whole-table write and the streaming path's estimate), so a re-inlined copy takes the built-in
    constant instead of the setting: `parquet_set_target_row_group_bytes` then governs one kind of
    write and not the other, with every row correct, the row-group count wrong, and nothing failing
    (Risk-43 (feature_risks.md)).
  * `src/` must hold exactly one C++ translation unit. Every process-global in `parquet_wrapper.cpp`
    is a file-scope `static`, so a second `.cpp` would silently get its own copy of each -- breaking
    both the `parquet_debug_*` test overrides and the settings mirrored from `parquet_settings`. Not
    a ban on splitting the file: it is CLAUDE.md's TU-split note (CLAUDE.md#if-
    srcparquet_wrappercpp-is-ever-split-into-multiple-translation-units) firing at the moment
    somebody does.
  * Every setting must have an environment variable. `parquet_settings_from_env` is the only way a
    setting can be exercised without recompiling, and a knob it forgets is unreachable that way with
    nothing to say so. The knob list is taken from `parquet_print_settings`' own printed rows,
    shared with the documentation check above, so a new setting fails both at once rather than
    needing two lists remembered separately.
  * An `allocate` extent taken from `size(...)` must ask for `kind=int64`. A default-kind `size()`
    wraps above 2³¹ elements, and in an allocate extent that is worse than the short fill it causes
    elsewhere, because the loop that follows usually gets its bound right: ten sites in the
    generated table accessors read `allocate(arr(size(rows)))` on one line and `do k = 1, size(rows,
    kind=int64)` on the next, so a slice past 2³¹ rows would have allocated a zero-length array and
    then written the full count into it. Element assignment does not reallocate, so that is an out-
    of-bounds write on a valid call rather than a wrong answer -- unlike the sibling `arr = p`
    shape, where intrinsic assignment to an allocatable resizes and hides the mistake. Matched by
    shape with no exemption list, since `kind=int64` costs nothing even where the extent is provably
    small (Risk-105 (feature_risks.md)).
  * No per-element helper in `parquet_wrapper.cpp` may take an Arrow array by `const
    std::shared_ptr<arrow::Array> &`. A `shared_ptr` parameter looks free and is not: every
    `std::static_pointer_cast` inside such a helper builds a new one, i.e. an atomic increment and
    decrement, and in a helper called once per row that becomes the dominant cost of the whole
    operation -- `real_family_value_at` measured at 13.6 ns per row to read one double that way
    against 1.8 ns with a raw pointer, when the clause evaluation it serves was 75-93% of the cost
    of installing a row filter. Nothing fails when this is undone: every answer stays identical and
    the suite stays green, only the clock moves. Matched by shape (an array parameter next to an
    element index), so it cannot go blind to the next helper added.
  * Every scenario `test/error_scenarios.f90` dispatches on must be named in
    `tools/run_error_scenarios.sh`. That array is what CI runs and what `prime_error_scenarios` pre-
    runs in parallel, but nothing proved the reverse direction: a scenario added to the `select
    case` and driven from a test, yet never listed, is invisible to the script and silently loses
    its priming -- it just falls back to spawning on demand, so nothing fails and nothing says
    anything. Names are derived by shape from the `select case` itself, and an empty result is a
    failure, so the check cannot go blind the way an enumerated list would.
  * Every `omp_*` reference in `src/` and `test/` must sit inside `#ifdef _OPENMP`. A build without
    OpenMP compiles the `!$omp` directives away -- they are comments -- but not the ordinary Fortran
    around them, so an unguarded `use omp_lib`, `omp_get_thread_num()` or `omp_lock_kind` is an
    undeclared name and the file stops compiling. Nothing in the ordinary fleet can see it: fpm's
    `openmp = "*"` metapackage supplies `-fopenmp` for gfortran and ifx, so `_OPENMP` is defined in
    CI and in every local `fpm test`, and only a toolchain the metapackage does not cover reaches
    the other arm. It has bitten twice -- once in `src/`, once in a threading test that left its
    file uncompilable serially for two days with every check green. Note a runtime `skip_test` guard
    does not cover this: skipping is a decision made after the file has already had to compile.
Run it after touching the table layer or a generator template (it is also part of
`tools/run_lint_check.sh` and CI's `lint` stage):

    tools/check_source_conventions.py
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


#: `src/parquet_strings.f90` -- the module that owns `parquet_string_column` and its typed tier.
STRINGS_FILE = SRC / "parquet_strings.f90"

#: Prefix of `parquet_string_column`'s typed (non-polymorphic) tier -- see feature_ifx.md.
STRING_TYPED_PREFIX = "parquet_string_column_"


#: The files holding `parquet_column`'s own implementation, typed tier included.
COLUMN_FILES = sorted(SRC.glob("parquet_columns*.f90"))

#: The CONTAINER column modules -- `parquet_list` today, `parquet_map`/`parquet_struct` from
#: Phases 4-5. Each holds a `type(parquet_column)` payload component and reaches it per ELEMENT,
#: so `parquet_column`'s typed-tier rule applies to them exactly as it does to `parquet_tables`.
#: Globbed rather than enumerated, so a sibling container module is covered the day it lands --
#: this file's own "a static check that enumerates names goes stale silently" rule.
CONTAINER_FILES = sorted(
    set(SRC.glob("parquet_list*.f90")) | set(SRC.glob("parquet_map*.f90"))
    | set(SRC.glob("parquet_struct*.f90"))
)

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


def check_container_accessors_resolve():
    """Every container-column accessor must route through `table_resolve` / `col_resolve`.

    feature_container_phase6.md's D6. A row-structural mutation SKIPS a column that is not
    resident, leaving it RES_EMPTY and the table detached from its file; the detach guard inside
    `table_resolve` is the ONLY thing that then reports a read of it. An accessor written any other
    way -- reaching `self%cache%cols(idx)` from a name it looked up itself -- would hand back the
    skipped column instead of aborting, and every existing test would still pass, because the
    column's row COUNT is not what is wrong with it.

    Matched by shape rather than from a list of names: any `module procedure` in
    `parquet_tables_container.f90` whose body mentions `self%cache%cols(` must also call one of the
    two resolvers. The downcast helpers (`container_as_*`) touch no cache at all and are covered by
    that same rule for free.

    **`%add_column` is exempt, and that is a real distinction rather than a carve-out**: it CREATES
    a slot (`table_new_slot`) instead of resolving an existing one, so there is no column for a
    detach guard to be in front of. The exemption is keyed on the creation call actually being
    there, so an `%add_column` specific that stopped creating and started reading would fail this
    check rather than slip through it.

    An empty derivation fails rather than passing vacuously, per this file's own "a static check
    that enumerates names goes stale silently" rule.
    """
    problems = []
    path = SRC / "parquet_tables_container.f90"
    if not path.exists():
        return ["src/parquet_tables_container.f90 is missing -- this check can no longer see the "
                "container accessors and is passing vacuously"]
    bodies = {}
    current = None
    for raw in path.read_text().split("\n"):
        code = strip_comment(raw)
        header = re.match(r"^\s*module\s+procedure\s+(\w+)\s*$", code, re.I)
        if header:
            current = header.group(1)
            bodies[current] = []
            continue
        if re.match(r"^\s*end\s+procedure\b", code, re.I):
            current = None
            continue
        if current:
            bodies[current].append(code)
    if not bodies:
        problems.append("src/parquet_tables_container.f90: found no `module procedure` body -- "
                        "this check can no longer see the accessors and is passing vacuously")
    for name, lines in sorted(bodies.items()):
        text = "\n".join(lines)
        if "%cache%cols(" not in text:
            continue
        if re.search(r"\bcall\s+(table_resolve|col_resolve)\s*\(", text, re.I):
            continue
        if re.search(r"\bcall\s+table_new_slot\s*\(", text, re.I):
            continue
        problems.append(
            "src/parquet_tables_container.f90: `%s` reaches `%%cache%%cols(...)` without going "
            "through table_resolve/col_resolve. That is what puts table_check_not_detached in "
            "front of the access -- without it, a container column a row mutation SKIPPED is read "
            "rather than reported (feature_container_phase6.md D6). Nothing fails if this "
            "regresses: the row count is still right." % name
        )
    return problems


def check_no_type_bound_column_access():
    """feature_ifx.md -- no per-cell path may reach a `type(parquet_column)` through a binding.

    Two halves, because the cost comes back through either one and NOTHING fails when it does:
    every answer stays correct, every test stays green, and only an ifx `objdump` store count
    shows the ~35 ns per call returning.

      1. The TABLE layer must call the typed generics, never `%values%<binding>`.
      2. The typed tier itself must call the TYPED guards and helpers, never a `class`-dummy
         procedure and never a binding on its own `col` dummy.
      3. The CONTAINER column modules must do the same for their `parquet_column` payload --
         a set neither of the other two halves can see (see the comment on that half).

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

    # 3. The CONTAINER column modules, which reach a `type(parquet_column)` payload per element.
    #
    # Neither half below would have caught this on its own: half 1 matches the literal `%values%`
    # component name and scans TABLE_FILES, and half 2 scans submodules of `parquet_columns`. A
    # container module is in neither set and matches neither pattern, so the rule would have been
    # enforced everywhere except the newest place it applies.
    #
    # Matched on the BINDING name rather than on a component name, so it holds whatever a future
    # container calls its payload -- `%payload%get_at(...)`, `%inner%is_null(...)` and
    # `%values%set_at(...)` are all caught, while a per-COLUMN binding that has no typed twin
    # (`%payload%append_values(...)`, `%payload%gather(...)`) is deliberately not: CLAUDE.md's
    # rule bans the conversion on a per-cell path and explicitly permits it once per column.
    container_re = re.compile(r"%\s*\w+\s*%\s*(" + "|".join(TYPED_TWINNED_BINDINGS) + r")\s*\(", re.I)
    for path in CONTAINER_FILES:
        for lineno, raw in enumerate(path.read_text().split("\n"), start=1):
            match = container_re.search(strip_comment(raw))
            if match:
                problems.append(
                    "%s:%d: reaches a `parquet_column` payload through the type-bound `%%%s(...)`. "
                    "Call `%s%s(<designator>, ...)` instead -- a `type(parquet_column)` actual "
                    "passed to a `class` dummy makes ifx build a runtime type descriptor in this "
                    "procedure's prologue, unconditionally, on every call (~35 ns; feature_ifx.md). "
                    "Nothing fails if this regresses:\n    %s"
                    % (path.relative_to(REPO_ROOT), lineno, match.group(1), TYPED_PREFIX,
                       match.group(1), raw.strip())
                )

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
            # A `%`-qualified name is a component or a BINDING reference, never a reference to a
            # module procedure -- so it cannot be the bare call this clause is about, and the
            # lookbehind is what keeps it out. It matters as soon as a typed procedure delegates
            # to a container: `col%container%is_null_row(i)` names a binding on a
            # `class(parquet_container_column)` designator, and a `class` actual passed to a
            # `class` dummy builds no descriptor at all -- the cost this tier exists to remove
            # comes only from a `type` actual. Type-bound calls on `col` itself are a real problem
            # and are caught by the clause below.
            for called in re.findall(r"(?<![%\w])(\w+)\s*\(", code):
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


def _string_column_bindings():
    """Binding names on `parquet_string_column`, i.e. the `%<name>(` forms a caller can write."""
    text = STRINGS_FILE.read_text()
    start = re.search(r"^\s*type\s*::\s*parquet_string_column\b", text, re.M)
    if not start:
        return set()
    end = re.search(r"^\s*end\s+type\s+parquet_string_column\b", text[start.end():], re.M)
    body = text[start.end():start.end() + end.start()] if end else ""
    names = set()
    for raw in body.split("\n"):
        line = strip_comment(raw).strip()
        m = re.match(r"^(?:procedure|generic)\b[^:]*::\s*(.+)$", line, re.I)
        if not m:
            continue
        spec = m.group(1)
        # `name => impl` binds `name`; a bare list binds each name to itself.
        names.update(part.strip().lower() for part in spec.split("=>")[0].split(","))
    return {n for n in names if n.isidentifier()}


def check_no_type_bound_string_column_access():
    """feature_ifx.md -- `parquet_columns` must not reach `%str` through a binding.

    The same defect as `check_no_type_bound_column_access` above, one type further down. A
    `parquet_column`'s string storage is a `type(parquet_string_column)` component, so
    `col%str%is_null(i)` hands a non-polymorphic actual to a `class` passed-object dummy and ifx
    builds the runtime type descriptor -- three records, 24 stores -- in the CALLER's prologue,
    unconditionally, ahead of the `select case` that decides whether the string arm runs at all.

    **Those stores go to STATIC storage, which is what makes this one worse than a per-call cost.**
    Every thread writes the same `.bss` cache lines, so a per-element scan that costs 9 ns per call
    on one thread costs 1.4 us on sixteen. Measured on one 200000-row `%has_nulls` scan at 384
    threads: 38.9 s before, 0.26 s after; the whole `fpm test` run went from 93 to 9 minutes of CPU.

    Two halves, as above:

      1. No file in `parquet_columns*` may write `<designator>%str%<binding>(...)`.
      2. The typed tier itself must not reach a binding on a `type` dummy of its own.

    Nothing fails when either regresses: every answer stays correct and every test stays green.
    """
    problems = []
    bindings = _string_column_bindings()
    if not bindings:
        problems.append("src/parquet_strings.f90: found no type-bound procedure on "
                        "parquet_string_column -- this check can no longer see the type and is "
                        "passing vacuously")
        return problems
    published = {n.lower() for n in re.findall(
        r"^\s*public\s*::\s*(" + STRING_TYPED_PREFIX + r"\w+)\s*$",
        STRINGS_FILE.read_text(), re.M)}
    if not published:
        problems.append("src/parquet_strings.f90: no public `%s*` name -- the typed string tier "
                        "is gone, or this check has gone stale (feature_ifx.md)"
                        % STRING_TYPED_PREFIX)

    # 1. Every consumer of a column's string storage.
    site = re.compile(r"(?<![\w%])(\w+%str)%(\w+)\s*\(")
    for path in COLUMN_FILES:
        for lineno, raw in enumerate(path.read_text().split("\n"), start=1):
            match = site.search(strip_comment(raw))
            if not match or match.group(2).lower() not in bindings:
                continue
            problems.append(
                "%s:%d: reaches the string store through the type-bound `%%str%%%s(...)`. Call "
                "`%s%s(%s, ...)` instead -- a `type(parquet_string_column)` actual passed to a "
                "`class` dummy makes ifx build a runtime type descriptor in STATIC storage in "
                "this procedure's prologue, on every call, and every thread then writes the same "
                "cache lines (feature_ifx.md). Nothing fails if this regresses:\n    %s"
                % (path.relative_to(REPO_ROOT), lineno, match.group(2), STRING_TYPED_PREFIX,
                   match.group(2), match.group(1), raw.strip()))

    # 2. The typed tier itself: a `type` dummy of parquet_string_column must not be used as the
    #    passed object of a binding. Only procedures whose passed-object-shaped dummies are `type`
    #    are scanned, so the ordinary `class`-dummy bindings above them are untouched.
    text = STRINGS_FILE.read_text().split("\n")
    typed_dummies, current, header = set(), None, 0
    for lineno, raw in enumerate(text, start=1):
        code = strip_comment(raw)
        m = re.match(r"^\s*(?:pure |elemental |impure |recursive )*"
                     r"(?:[\w()=:,* ]+?\s)?(?:function|subroutine)\s+(\w+)\s*\(", code)
        if m:
            current, typed_dummies, header = m.group(1), set(), lineno
            continue
        d = re.match(r"^\s*type\(parquet_string_column\)[^:]*::\s*(\w+)", code)
        if d and current:
            typed_dummies.add(d.group(1).lower())
            continue
        if not current or not typed_dummies:
            continue
        for designator, bound in re.findall(r"(?<![\w%])(\w+)\s*%\s*(\w+)\s*\(", code):
            if designator.lower() in typed_dummies and bound.lower() in bindings:
                problems.append(
                    "src/parquet_strings.f90:%d: `%s` makes the type-bound call `%s%%%s(...)` on a "
                    "`type(parquet_string_column)` dummy, which is the conversion the typed tier "
                    "exists to remove. Call `%s%s(%s, ...)` instead (feature_ifx.md):\n    %s"
                    % (lineno, current, designator, bound, STRING_TYPED_PREFIX, bound,
                       designator, text[lineno - 1].strip()))
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
    """print_stat's column set and its documentation must agree.

    `parquet_reader_print_stat`'s output is documented column by column in
    `doc/pages/io/reading.md`, and that table is the only contract it has: the format is otherwise
    asserted in exactly one place in the whole suite (a `sample:` substring), so a renamed or added
    column breaks no test at all. It had already drifted when this check was written -- the docs
    named a `prefetc` column that the code calls `fetched`, and omitted `qcmin`, `qcmax`, `qcmiss`
    and `filter` entirely -- which is the failure this exists to stop repeating.

    Comparing the two SETS rather than asserting the printed header line is deliberate: the header
    is padded to each column's widest cell, so its exact text depends on the data, and a test that
    matched it literally would be brittle in a way that teaches people to delete it.

    `parquet_table`'s own `%print_stat` is deliberately out of scope, and must stay out: it was not
    modelled on the reader's, and its test asserts BEHAVIOUR (a lazy table stays lazy, `all=.true.`
    still reads nothing) rather than text. A change that started asserting its exact output would
    import the fragility this check exists to contain.
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
            "describe -- add a row for it" % column)
    for column in extra:
        problems.append(
            "doc/pages/io/reading.md: the documentation describes a `%s` column that print_stat does "
            "not print -- it was renamed or removed" % column)
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
    # BOTH settings files, because a knob's state lives beside the setter that writes it and an
    # Arrow-free module's knobs live in the leaf. Scanning only parquet_settings.f90 would go
    # silently blind to every knob that moved there -- the failure direction this check exists to
    # prevent, one level up (CLAUDE.md, "A static check that enumerates names goes stale silently").
    settings_files = [SRC / "parquet_settings.f90", SRC / "parquet_settings_base.f90"]
    names = []
    for settings in settings_files:
        if not settings.exists():
            return ["%s: expected settings file is missing -- this check needs updating"
                    % settings.relative_to(REPO_ROOT)]
        found = re.findall(
            r"^\s*(?:integer|logical|character\([^)]*\)|real\([^)]*\))(?:\([^)]*\))?\s*,\s*save\s*::\s*(cfg_\w+)",
            settings.read_text(), re.M)
        if not found:
            return ["%s: found no `cfg_* ` settings variables -- this check needs updating"
                    % settings.relative_to(REPO_ROOT)]
        names.extend(found)
    # Where each name is assigned, so an assignment does not count as a read of itself.
    sources = sorted(SRC.glob("*.f90"))
    for name in names:
        read_somewhere = False
        for path in sources:
            for line in path.read_text().split("\n"):
                code = strip_comment(line)
                if name not in code:
                    continue
                # The DECLARATION is not a read of itself. Without this the check is vacuous:
                # `integer, save :: cfg_x = 0` mentions the name, does not match the assignment
                # pattern below, and so was counted as a read -- which meant a knob nothing reads
                # (the one thing this check exists to catch) reported [ok]. Confirmed by adding a
                # dead knob and watching the check pass.
                if re.match(r"^\s*(?:integer|logical|character|real|type)\b[^:]*::\s*%s\b"
                            % re.escape(name), code):
                    continue
                # A `public ::`/`private ::` accessibility statement is not a read either.
                if re.match(r"^\s*(?:public|private)\s*::", code):
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
                "src/parquet_settings*.f90: `%s` is written but never read -- the setting it backs "
                "does nothing, and a set/get test would not notice (feature_risks.md Risk-41)" % name)
    return problems


#: Procedures allowed to write to a unit directly: the emit channels themselves, and the solicited
#: printers, which format many lines to a caller-chosen unit and ask parquet_output_is_suppressed
#: rather than routing through a channel. Every other write in src/ has to go through a channel, or
#: the two output settings silently do not apply to it.
DIRECT_PRINT_ALLOWED = {
    "parquet_emit_info", "parquet_emit_warning", "parquet_emit_error_context",
    # parquet_logging is a separate system with a separate audience: it carries the USER's
    # program's output, not the library's, so it must NOT route through the emit channels --
    # `verbosity` and `message_stream` govern what this library says, not what its caller says.
    # `machinery_warning` is the one place it writes to a unit by name (every sink write goes to a
    # unit held in a variable), and it reports a failure of the logging machinery itself, which is
    # why it deliberately bypasses sinks, layout and the output critical section alike.
    "machinery_warning",
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


def check_env_table_matches_the_source():
    """The guide's environment-variable table and `parquet_settings_from_env` must name the same set.

    `check_env_covers_every_setting` keeps the SOURCE self-consistent -- every printed knob has a
    variable -- and `check_print_settings_documented` keeps the sample dump and the limits table in
    step with the SOURCE. Between them they leave one gap, and it is the one that actually shipped:
    nothing compared the guide's `PARQUET_FORTRAN_*` **table** against the variables the code
    reads. `PARQUET_FORTRAN_RANDOM_THREADS` and `PARQUET_FORTRAN_RANDOM_PARALLEL_MIN_ELEMENTS` were
    both applied by `parquet_settings_from_env`, both printed by `parquet_print_settings`, both
    covered by `test_env_every_variable` -- and absent from the table a user reads to find out that
    they exist. Every check passed throughout.

    Both directions are checked. A variable in the source and not in the table is undiscoverable; a
    variable in the table and not in the source is a documented knob that silently does nothing,
    which is the same failure `check_env_covers_every_setting` exists to prevent one layer down.

    Read as SETS from the table's own rows, not from the whole page: the prose around it names
    `PARQUET_FORTRAN_THREADS` and `PARQUET_FORTRAN_SORT_THREADS` in a worked example and in a shell
    block, so scanning the page as a whole would accept a table missing every other row.
    """
    problems = []
    src = SRC / "parquet_settings.f90"
    doc = REPO_ROOT / "doc" / "pages" / "operating" / "settings.md"
    text = src.read_text()
    body = re.search(r"subroutine parquet_settings_from_env\b(.*?)end subroutine parquet_settings_from_env",
                     text, re.S)
    if body is None:
        return ["src/parquet_settings.f90: could not find parquet_settings_from_env -- this check "
                "needs updating"]
    # The variable each `call env_value("NAME", ...)` applies. Matching the call rather than every
    # occurrence of the name keeps the abort-message mentions inside env_int64/env_require_token
    # out of the set.
    in_source = set(re.findall(r'call\s+env_value\s*\(\s*"(PARQUET_FORTRAN_\w+)"', body.group(1)))
    if not in_source:
        return ["src/parquet_settings.f90: found no `call env_value(\"PARQUET_FORTRAN_...\"` in "
                "parquet_settings_from_env -- this check needs updating"]
    doc_text = doc.read_text()
    # Only rows of a Markdown table: a line starting with `|` whose first cell is a code span
    # holding the variable name.
    in_doc = set()
    for line in doc_text.split("\n"):
        row = re.match(r"\|\s*`(PARQUET_FORTRAN_\w+)`\s*\|", line.strip())
        if row:
            in_doc.add(row.group(1))
    if not in_doc:
        return ["doc/pages/operating/settings.md: found no `| `PARQUET_FORTRAN_...` |` table row -- "
                "either the environment table was reshaped or it is gone; this check needs updating"]
    for var in sorted(in_source - in_doc):
        problems.append(
            "doc/pages/operating/settings.md: parquet_settings_from_env applies `%s` but the "
            "environment-variable table does not list it -- a user has no way to discover it" % var)
    for var in sorted(in_doc - in_source):
        problems.append(
            "doc/pages/operating/settings.md: the environment-variable table lists `%s` but "
            "parquet_settings_from_env never reads it -- the variable would silently do nothing" % var)
    return problems


def check_log_env_table_matches_the_source():
    """The logging guide's `<prefix>*` table and `pf_log_configure_from_env` must name the same set.

    The same gap `check_env_table_matches_the_source` closes for `parquet_settings`, one module
    over. That one exists because two variables shipped applied-by-the-code, printed-by-the-dump,
    covered-by-a-test and absent from the table a user reads to discover them; nothing about that
    failure was specific to settings, and `pf_log_configure_from_env` has exactly the same shape --
    a short list of names read from the environment, mirrored by hand in a Markdown table.

    Both directions, for the same two reasons: a variable in the source and not in the table is
    undiscoverable, and one in the table and not in the source is a documented knob that silently
    does nothing.

    The suffixes are compared, not whole names. The source spells each as `trim(pre) // "LEVEL"`
    because the prefix is the caller's, and the guide writes `<prefix>LEVEL` for the same reason,
    so the prefix itself is the one part that cannot be cross-checked and the one part that cannot
    go stale.
    """
    problems = []
    src = SRC / "parquet_logging.f90"
    doc = REPO_ROOT / "doc" / "pages" / "utilities" / "logging.md"
    text = src.read_text()
    body = re.search(r"subroutine pf_log_configure_from_env\b(.*?)end subroutine pf_log_configure_from_env",
                     text, re.S)
    if body is None:
        return ["src/parquet_logging.f90: could not find pf_log_configure_from_env -- this check "
                "needs updating"]
    # The suffix each `get_environment_variable(trim(pre) // "NAME", ...)` reads. Matching the call
    # keeps the abort messages, which name the same variables, out of the set.
    in_source = set(re.findall(r'get_environment_variable\s*\(\s*trim\(pre\)\s*//\s*"(\w+)"',
                               body.group(1)))
    if not in_source:
        return ['src/parquet_logging.f90: found no `get_environment_variable(trim(pre) // "..."` '
                "in pf_log_configure_from_env -- this check needs updating"]
    in_doc = set()
    for line in doc.read_text().split("\n"):
        row = re.match(r"\|\s*`<prefix>(\w+)`\s*\|", line.strip())
        if row:
            in_doc.add(row.group(1))
    if not in_doc:
        return ["doc/pages/utilities/logging.md: found no `| `<prefix>...` |` table row -- either "
                "the environment table was reshaped or it is gone; this check needs updating"]
    for var in sorted(in_source - in_doc):
        problems.append(
            "doc/pages/utilities/logging.md: pf_log_configure_from_env reads `<prefix>%s` but the "
            "environment-variable table does not list it -- a user has no way to discover it" % var)
    for var in sorted(in_doc - in_source):
        problems.append(
            "doc/pages/utilities/logging.md: the environment-variable table lists `<prefix>%s` but "
            "pf_log_configure_from_env never reads it -- the variable would silently do nothing" % var)
    return problems


def check_one_random_number_generator():
    """The library draws every random number from parquet_random -- src/parquet_wrapper.cpp may not
    reach for C++'s own generators.

    `parquet_open_reader(..., sample_fraction=)` used to draw its row mask with std::mt19937_64 fed
    through std::uniform_real_distribution, which made this library ship TWO generators: one frozen,
    golden-vectored and tested against an arbitrary-precision oracle, and one specified nowhere.
    Two things followed, and both were invisible to `fpm test`. std::uniform_real_distribution's
    mapping from engine output to a double is implementation-defined, so "the same seed selects the
    same rows" was a promise the C++ standard did not underwrite across libstdc++ and libc++. And a
    sequential engine made row i's decision depend on how many draws preceded it, so a pruned row
    group had to be drawn for and discarded or the same seed would silently select different rows.

    The draw is Fortran's now (parquet_apply_sample, src/parquet_read.f90), and this check is what
    stops the second generator coming back. It matches by CONSTRUCT rather than by a remembered list
    of names, so a future `std::ranlux48` or `std::bernoulli_distribution` is caught too -- see
    CLAUDE.md's "A static check that enumerates names goes stale silently".

    If C++ ever genuinely needs a random number, the answer is the same one the OpenMP thread counts
    already use: resolve it in Fortran and pass the resolved VALUE across the bind(C) boundary.
    """
    path = SRC / "parquet_wrapper.cpp"
    if not path.exists():
        return ["src/parquet_wrapper.cpp not found -- this check cannot have run"]
    banned = re.compile(
        r"\b(?:std::)?(?:mt19937(?:_64)?|minstd_rand\w*|ranlux\d+\w*|knuth_b|default_random_engine"
        r"|random_device|seed_seq|linear_congruential_engine|subtract_with_carry_engine"
        r"|mersenne_twister_engine|\w+_distribution)\b|#\s*include\s*<random>"
        r"|\b(?:std::)?(?:rand|srand|drand48|lrand48|random|srandom|arc4random)\s*\(")
    problems = []
    for n, raw in enumerate(path.read_text().splitlines(), start=1):
        line = raw.split("//", 1)[0]                      # a comment may name what was removed
        m = banned.search(line)
        if m:
            problems.append(
                "src/parquet_wrapper.cpp:%d: `%s` -- this library has ONE generator and it is "
                "parquet_random. A C++-side draw is unspecified across standard libraries and "
                "cannot be golden-vectored; resolve the value in Fortran and pass it across the "
                "bind(C) boundary, exactly as parquet_apply_sample does for the row sample."
                % (n, m.group(0)))
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


#: Cached map of module name -> every file fpm compiles as part of it: its own file plus every
#: submodule descended from it, at any depth.
_MODULE_FILES = None


def _module_file_map():
    """Which source files belong to each module, submodules included.

    **A module's SUBMODULES are separate files, and fpm compiles every one of them whenever the
    module is used** -- pruning granularity is the module, never the submodule. So a `use` written
    inside a submodule is every bit as much a dependency of the parent module as one in its own
    file, and a walk that opens only `<module>.f90` is blind to it. That blindness is not
    hypothetical: the argsort tier's engine kept a `use parquet_settings` for three settings
    getters, which put parquet_bindings back into every consumer's build while a module-file-only
    walk reported the tier Arrow-free.
    """
    global _MODULE_FILES
    if _MODULE_FILES is not None:
        return _MODULE_FILES
    sub_re = re.compile(r"^\s*submodule\s*\(\s*([A-Za-z_]\w*)\s*(?::\s*[A-Za-z_]\w*\s*)?\)"
                        r"\s*([A-Za-z_]\w*)", re.M | re.I)
    parent, files = {}, {}
    for path in sorted(SRC.glob("*.f90")):
        m = sub_re.search(path.read_text())
        if m:
            parent[path.stem.lower()] = m.group(1).lower()
        files.setdefault(path.stem.lower(), []).append(path)
    # Resolve each submodule to its ROOT module, following the chain up.
    out = {}
    for unit, paths in files.items():
        root, guard = unit, 0
        while root in parent and guard < 50:
            root = parent[root]
            guard += 1
        out.setdefault(root, []).extend(paths)
    _MODULE_FILES = out
    return out


def _module_use_closure(root):
    """Every module reachable from `root` by `use`, plus the edges, for a shortest-path message."""
    use_re = re.compile(r"^\s*use\s*(?:,\s*intrinsic\s*)?(?:::)?\s*([A-Za-z_]\w*)", re.M)
    fmap = _module_file_map()
    seen, queue, edges = set(), [root], {}
    while queue:
        mod = queue.pop()
        if mod in seen:
            continue
        seen.add(mod)
        for path in fmap.get(mod, ()):        # the module's own file AND every submodule of it
            for used in use_re.findall(path.read_text()):
                used = used.lower()
                if used == mod:
                    continue                   # a submodule naming its own parent
                edges.setdefault(mod, set()).add(used)
                queue.append(used)
    return seen, edges


def _shortest_use_chain(root, target, edges):
    """One shortest `use` chain from root to target, as `a -> b -> c`."""
    trail, frontier = {root: [root]}, [root]
    while frontier:
        mod = frontier.pop(0)
        for used in sorted(edges.get(mod, ())):
            if used not in trail:
                trail[used] = trail[mod] + [used]
                frontier.append(used)
    return " -> ".join(trail.get(target, [root, target]))


def _check_stays_arrow_free(module, why):
    """Shared body of the three tier checks below: `module` must not reach parquet_bindings."""
    seen, edges = _module_use_closure(module)
    if "parquet_bindings" not in seen:
        return []
    chain = _shortest_use_chain(module, "parquet_bindings", edges)
    return [
        "src/%s.f90: reaches parquet_bindings through %s. %s The C++ sort engine is TEST-ONLY and "
        "must stay behind the procedure pointers parquet_sorting_oracle binds -- naming a bind(C) "
        "entry point from any of these modules is what this check exists to catch, and adding the "
        "offending module to some allow-list would destroy the property rather than fix it."
        % (module, chain, why)
    ]


def check_parquet_argsort_stays_arrow_free():
    """`use parquet_argsort` must not drag the Arrow/Parquet C++ stack into a consumer's build.

    The argsort tier exists so that a project wanting a sort -- or wanting parquet_sampling's
    weighted draws -- compiles this tier and stops, instead of the whole reader/writer stack. That
    is a property of the `use` graph and nothing in `fpm test` can see it: the library obviously has
    Arrow, so a stray import compiles and tests perfectly well here and fails in the consumer's
    build, or silently inflates it. See feature_modules.md section 4.
    """
    return _check_stays_arrow_free(
        "parquet_argsort",
        "That is the tier's whole purpose: an Arrow-free Fortran graph for a consumer that wants "
        "nothing but a sort.")


def check_parquet_sorting_stays_arrow_free():
    """`use parquet_sorting` must not reach parquet_bindings either.

    The full tier adds the five element types that need a column, a packed string store or a
    temporal element -- none of which needs C++. Its Arrow-freedom is what makes the whole sorting
    API importable on its own.
    """
    return _check_stays_arrow_free(
        "parquet_sorting",
        "The full sorting tier is Arrow-free by design; only parquet_sorting_oracle may name the "
        "C++ engine.")


def check_parquet_sampling_stays_arrow_free():
    """`use parquet_sampling` must not reach parquet_bindings.

    Sampling needs exactly one thing from the sort -- `pf_argsort(real64 array, int64 perm)` for
    `pf_weighted_permutation` -- and it takes it from `parquet_argsort` rather than
    `parquet_sorting` for that reason. Importing the facade instead compiles and passes every test
    while quietly restoring the dependency this split removed.
    """
    return _check_stays_arrow_free(
        "parquet_sampling",
        "A weighted draw needs one argsort, which parquet_argsort provides.")


def check_parquet_columns_stays_arrow_free():
    """`use parquet_columns` must not reach parquet_bindings.

    The column container is advertised in doc/pages/operating/choosing-a-module.md as a ten-file,
    Arrow-free import: a project that wants a typed, null-aware column and nothing else can depend
    on it alone. It was covered before this check existed, but only BY ACCIDENT -- `parquet_sorting`
    happens to import it, so check_parquet_sorting_stays_arrow_free walked it on the way past. That
    is real coverage right up until the day the sorting tier stops needing the column container, at
    which point it evaporates with nothing to announce it. A tier the guide advertises deserves a
    check of its own rather than a side effect of someone else's.
    """
    return _check_stays_arrow_free(
        "parquet_columns",
        "The column container is advertised as an Arrow-free import in its own right.")


def check_parquet_list_stays_arrow_free():
    """`use parquet_list` must not reach parquet_bindings.

    A list column is pure in-memory storage: offsets, a row bitmap and one `parquet_column`
    payload. Nothing in it needs Arrow, and a program that builds list-shaped data without ever
    touching a file should pay for none. Pinned in its own right rather than through
    `parquet_columns` (which it imports and which is itself checked) for the reason recorded on
    that check: coverage inherited from a neighbour's import evaporates the day that import moves,
    and does so silently.

    It is also the first module that could acquire the dependency by accident rather than by
    design -- Phase 2 gives a list column a reader, and the natural place to reach for
    `parquet_bindings` is here rather than in `parquet_core` where it belongs.
    """
    return _check_stays_arrow_free(
        "parquet_list",
        "A list column is in-memory storage and is advertised as an Arrow-free import.")


def check_parquet_struct_stays_arrow_free():
    """`use parquet_struct` must not reach parquet_bindings.

    A struct column is pure in-memory storage: one `parquet_column` per declared field, a packed
    name list and a row bitmap. Nothing in it needs Arrow, and a program that builds struct-shaped
    data without ever touching a file should pay for none. Pinned in its own right rather than
    through `parquet_columns` (which it imports and which is itself checked) for the reason
    recorded on that check: coverage inherited from a neighbour's import evaporates the day that
    import moves, and does so silently.

    The one dependency beyond `parquet_columns` is `parquet_settings_base`, a leaf, for the single
    warning `%field(name, warn=.true.)` can emit -- so the Arrow-free property is unaffected, and
    the re-export rule that forces the import is what lets a narrow consumer silence that warning.
    """
    return _check_stays_arrow_free(
        "parquet_struct",
        "A struct column is in-memory storage and is advertised as an Arrow-free import.")


def check_parquet_map_stays_arrow_free():
    """`use parquet_map` must not reach parquet_bindings.

    A map column is pure in-memory storage: offsets, a flattened keys column, a flattened values
    column and a row bitmap. Nothing in it needs Arrow, and a program that builds map-shaped data
    without ever touching a file should pay for none. Pinned in its own right rather than through
    `parquet_columns` or `parquet_strings` (both of which it imports and both of which are
    themselves checked) for the reason recorded on those checks: coverage inherited from a
    neighbour's import evaporates the day that import moves, and does so silently.

    The one dependency beyond those two is `parquet_settings_base`, a leaf, for the warnings its
    soft-fail lookups can emit -- so the Arrow-free property is unaffected, and the re-export rule
    that forces the import is what lets a narrow consumer silence those warnings.
    """
    return _check_stays_arrow_free(
        "parquet_map",
        "A map column is in-memory storage and is advertised as an Arrow-free import.")


def check_parquet_temporal_stays_arrow_free():
    """`use parquet_temporal` must not reach parquet_bindings.

    The smallest import in the library -- one file, and today no `use` statement at all -- which is
    exactly why it is worth pinning: there is nothing here to break, so a future import would be
    the whole of the change and would otherwise pass unremarked. Same accidental-coverage story as
    parquet_columns above: it was reached only through parquet_sorting -> parquet_columns.
    """
    return _check_stays_arrow_free(
        "parquet_temporal",
        "The date/time/timestamp elements are advertised as a one-file, Arrow-free import.")


def check_parquet_spatial_stays_arrow_free():
    """`use parquet_spatial` must not drag the Arrow/Parquet C++ stack into a consumer's build.

    The spatial index is a grid over plain Fortran coordinate arrays and reaches nothing but
    `parquet_argsort` and `parquet_settings_base`, so a consumer wanting neighbour search compiles
    five files rather than the sixty-odd the reader/writer stack costs. That is a property of the
    `use` graph and nothing in `fpm test` can see it -- the library obviously has Arrow, so a stray
    import compiles and tests perfectly well here and inflates every downstream build.

    One check per tier rather than a few for the group is deliberate: a tier covered only
    transitively loses its coverage silently the day the import it was riding on moves.
    """
    return _check_stays_arrow_free(
        "parquet_spatial",
        "The spatial tier exists so that neighbour search costs a consumer five files; an import "
        "reaching the reader/writer stack would make it sixty.")


def check_parquet_healpix_stays_arrow_free():
    """`use parquet_healpix` must not drag the Arrow/Parquet C++ stack into a consumer's build.

    The pixelisation is arithmetic over plain reals and integers and reaches nothing but
    `parquet_settings_base`, so a consumer wanting sphere pixels compiles six files rather than
    the sixty-odd the reader/writer stack costs. That is the whole reason this module exists here
    rather than as a dependency on `libhealpix`, and it is a property of the `use` graph that
    nothing in `fpm test` can see -- the library obviously has Arrow, so a stray import compiles
    and tests perfectly well here and inflates every downstream build.

    One check per tier rather than a few for the group is deliberate: a tier covered only
    transitively loses its coverage silently the day the import it was riding on moves.
    """
    return _check_stays_arrow_free(
        "parquet_healpix",
        "The healpix tier exists so that a downstream project can drop its GPL libhealpix "
        "dependency without acquiring an Arrow one; an import reaching the reader/writer stack "
        "would defeat that.")


def check_parquet_version_stays_arrow_free():
    """`use parquet_version` must not reach parquet_bindings.

    This one was covered by NOTHING. Every other Arrow-free tier fell inside some checked module's
    closure; parquet_version is imported by no tier at all -- only by the `parquet` facade, which
    reaches Arrow by design and so is not walked -- so the page's promise that its graph stays clear
    of the C++ boundary rested on nobody having added an import. It is two files and the obvious
    import to add is the very one that would break it: `parquet_settings`, for the output knobs.
    Those come from `parquet_settings_base` instead, which is the whole point of that leaf existing.
    """
    return _check_stays_arrow_free(
        "parquet_version",
        "Reporting a compile-time string must not require the Arrow stack.")


def check_parquet_logging_stays_arrow_free():
    """`use parquet_logging` must not reach parquet_bindings.

    The module is a leaf on purpose: `iso_fortran_env`, and `omp_lib` under `#ifdef _OPENMP`. That
    is what makes it one compiled Fortran file for a downstream project, and it is the property
    most easily lost by reflex -- the obvious import to add is `parquet_settings`, for the output
    knobs, which is exactly the edge that would drag in the C++ boundary. It does not need them:
    every knob this module has belongs to a `pf_logger` object, not to the library.

    One check per tier rather than one for the group, per the established pattern: no other checked
    module imports `parquet_logging`, so nothing else would notice this tier acquiring an edge.
    """
    return _check_stays_arrow_free(
        "parquet_logging",
        "A logging module must not require the Arrow stack to print a line.")


def check_parquet_stats_stays_arrow_free():
    """`use parquet_stats` must not reach parquet_bindings.

    The statistics tier is advertised as an Arrow-free import: a program that has arrays in hand
    and wants to summarise them should not compile the C++ wrapper's dependencies to do it. Its one
    library edge is `parquet_sorting` -- for `pf_argsort`, `pf_nth_element` and, later, the
    `parquet_column` entry points -- and that tier is itself Arrow-free, which is the whole reason
    the edge is affordable.

    One check per tier rather than one for the group, per the established pattern: no other checked
    module imports `parquet_stats`, so nothing else would notice this tier acquiring an edge.
    """
    return _check_stays_arrow_free(
        "parquet_stats",
        "Summarising an array a caller already has must not require the Arrow stack.")


def check_stats_optional_argument_order():
    """Every `parquet_stats` procedure declares its optionals in one canonical order.

    In Fortran the ORDER of optional dummy arguments is a permanent compatibility contract: a
    caller may pass them positionally, so inserting one in the middle silently changes what an
    existing call means. `parquet_stats` therefore fixes one canonical sequence and requires every
    procedure's optional list to be a SUBSEQUENCE of it, which is one rule rather than a
    hand-maintained signature table -- and a table of that shape is exactly what this repository
    has watched go stale in the direction that stops checking.

    Matching is by SHAPE, not by an enumerated procedure list, so a procedure added later is
    covered without editing this check. Omission is what a subsequence permits, which is what lets
    the integer and logical specifics carry no `skipnan` (they have no NaN to skip) while still
    passing.

    **Verify this check by breaking it, not by watching it pass.** It reports success against a
    module with no procedures at all, so on the day it is written it proves nothing: swap two
    optionals in one interface body and confirm it fails before trusting a green run.
    """
    # Three blocks, in this order. The tier-A OUTPUT prefix, which `pf_moments` and anything
    # shaped like it declares before the input block; then the OBJECT-LIFECYCLE pair `retain` and
    # `consume`, which only `pf_stats`' own bindings take; then the population arguments every
    # reduction shares.
    #
    # Six blocks, in this order, and each is a fixed order of its own -- so the subsequence rule
    # still generates every signature. The tier-A OUTPUT prefix, which `pf_moments` and anything
    # shaped like it declares before the input block (`count` joins it as `pf_mode`'s own such
    # output); then the OBJECT-LIFECYCLE pair `retain` and `consume`, which only `pf_stats`' own
    # bindings take; then the SIGMA-CLIP block, which only `pf_sigma_clipped_stats` takes -- its
    # rule arguments first and its own outputs after them, so that the tuning a caller reaches for
    # comes before the population plumbing; then the population arguments every reduction shares;
    # and finally the OUTPUT DESTINATION pair `unit`/`name`, which only `%print` takes and which
    # describes where the answer goes rather than what it is.
    #
    # A block used by ONE procedure is not a contradiction of "one sequence": every other
    # procedure omits it, and omission is exactly what a subsequence permits.
    #
    # `kind` sits immediately after `method` because the two are the same kind of argument -- a
    # token naming a rule -- and no procedure takes both, so their relative order is never
    # observed. Inserting a name into this sequence is safe exactly when it leaves every existing
    # procedure's list a subsequence of it, which appending inside an unused block does. See
    # feature_pandas_S4.md's signature matrix.
    canonical = ["n_valid", "mean", "variance", "stddev", "sem", "skewness", "kurtosis",
                 "vsum", "vmin", "vmax", "count",
                 "retain", "consume",
                 "sigma", "sigma_lower", "sigma_upper", "maxiters", "cenfunc", "stdfunc",
                 "n_clipped", "keep", "converged", "right",
                 "is_valid", "weights", "weight_type", "ddof", "bias", "excess", "skipnan",
                 "method", "kind", "scale", "center", "out_valid", "n_null", "n_nan",
                 "n_outside", "ok", "threads",
                 "unit", "name"]
    rank = {name: i for i, name in enumerate(canonical)}
    path = SRC / "parquet_stats.f90"
    if not path.exists():
        return ["%s: expected file is missing" % path.name]

    bad = []
    # Continuation lines are JOINED first. Without this the check matches only single-line
    # declarations and silently skips every procedure whose argument list wraps -- which is every
    # procedure long enough for the order to matter, and is exactly how a check of this shape goes
    # blind: it keeps reporting [ok] against a file it can no longer read.
    lines, buf = [], ""
    for raw in path.read_text().split("\n"):
        code = raw.split("!")[0].rstrip()
        if code.endswith("&"):
            buf += code[:-1]
            continue
        lines.append((buf + raw) if buf else raw)
        buf = ""
    seen = 0
    i = 0
    while i < len(lines):
        m = re.match(r"\s*module (?:subroutine|function) (\w+)\((.*?)\)\s*(?:result\(\w+\))?\s*$",
                     lines[i].split("!")[0].rstrip())
        if not m:
            i += 1
            continue
        seen += 1
        proc, args = m.group(1), [a.strip() for a in m.group(2).split(",") if a.strip()]
        optional = []
        j = i + 1
        while j < len(lines) and not re.match(r"\s*end (?:subroutine|function) ", lines[j]):
            if "optional" in lines[j] and "::" in lines[j]:
                for nm in lines[j].split("::", 1)[1].split("!!")[0].split(","):
                    nm = nm.strip().split("(")[0].strip()
                    if nm in args:
                        optional.append(nm)
            j += 1
        i = j + 1

        unknown = [nm for nm in optional if nm not in rank]
        if unknown:
            bad.append("%s: optional argument(s) %s are not in the canonical sequence; add them "
                       "there (and to feature_pandas_S4.md's signature matrix) or rename them"
                       % (proc, ", ".join(unknown)))
            continue
        # Declaration order need not match the argument list, so compare against the dummy order.
        in_arg_order = [nm for nm in args if nm in optional]
        ranks = [rank[nm] for nm in in_arg_order]
        if ranks != sorted(ranks):
            bad.append("%s: optionals appear as (%s); the canonical order is (%s)"
                       % (proc, ", ".join(in_arg_order),
                          ", ".join(sorted(in_arg_order, key=lambda nm: rank[nm]))))
    if seen == 0:
        bad.append("src/parquet_stats.f90: no `module subroutine`/`module function` declaration "
                   "was recognised -- this check has gone blind and is passing vacuously")
    return bad


def check_facade_inventory_matches_its_use_lines():
    """`src/parquet.f90`'s doc-comment inventory must name every module it bare-`use`s.

    The facade's `!>` header lists the modules it re-exports. That is a list the code owns written
    out in prose, and it had drifted by SIX modules -- parquet_list, parquet_struct, parquet_map,
    parquet_sampling, parquet_spatial and parquet_healpix were all imported and none was listed --
    before anyone noticed, which is exactly the failure this repository's own rules warn about.
    Adding a seventh entry by hand does nothing to stop the eighth going missing; this does.

    **One direction only, deliberately**, for the same reason the landing-page check is: a listed
    module that the facade imports with `use ..., only:` is legitimate -- `parquet_maml_base` is
    listed because the three types it re-exports really are user API -- and a both-directions rule
    would fail on that deliberate entry. A check that fails on purpose gets switched off. What is
    asserted is the direction that actually drifted: every module re-exported by a BARE `use` is
    named, so a new sibling cannot be added to the facade and left out of the header.
    """
    path = SRC / "parquet.f90"
    text = path.read_text()
    body = text.split("module parquet", 1)
    header = body[0]
    used = []
    for line in text.split("\n"):
        m = re.match(r"^\s*use\s+(parquet_\w+)\s*$", line)
        if m and m.group(1) != "parquet_bindings":
            used.append(m.group(1))
    listed = set(re.findall(r"^!>\s+\*\s+`(parquet_\w+)`", header, re.M))
    problems = []
    for mod in used:
        if mod not in listed:
            problems.append(
                "src/parquet.f90: the module doc-comment's inventory does not mention `%s`, which "
                "the facade re-exports with a bare `use`. A reader of that header is told it lists "
                "every re-exported module." % mod)
    return problems


def check_get_version_has_one_home():
    """`parquet_get_version` must be `public ::` in exactly one module.

    doc/pages/operating/choosing-a-module.md, README.md and src/parquet.f90's own header all say
    parquet_version is the ONLY route to it apart from `use parquet` -- and nothing asserted the
    "only" half. test_module_surface_version proves the module provides it and
    test_facade_covers_every_layer proves the facade re-exports it; a stray `public ::` added to
    some tier for convenience would pass both while quietly making that tier grow a file, and the
    claim would go on being printed on the page.

    The rule it protects is not tidiness: a version string is fixed at compile time and nothing in
    the library reads it, so a tier carrying it grows its graph for a name none of its callers need.
    Re-exporting it from the facade is done with a bare `use parquet_version`, which names nothing
    and so does not match here.
    """
    pat = re.compile(r"^\s*public\s*::.*\bparquet_get_version\b", re.M)
    homes = sorted(f.name for f in SRC.glob("*.f90") if pat.search(f.read_text()))
    if homes == ["parquet_version.f90"]:
        return []
    if not homes:
        return ["src/: no module declares `public :: parquet_get_version`. Either it was renamed "
                "-- update this check and the three documents that name it -- or the facade is the "
                "only thing still exporting it, which the guide says it is not."]
    return ["src/: `parquet_get_version` is public from %s. It belongs to parquet_version alone: "
            "doc/pages/operating/choosing-a-module.md, README.md and src/parquet.f90's header all "
            "promise that, and a tier re-exporting it grows that tier's compile footprint for a "
            "compile-time constant none of its callers read. Use a second `use parquet_version` "
            "line in the consumer instead." % ", ".join(homes)]


def check_no_submodule_oracle_pointer_call():
    """No `src/` SUBMODULE may name one of the sort oracle's `p_*` procedure pointers.

    gfortran 15.2 ICEs -- `internal compiler error: in write_symbol, at lto-streamer-out.cc:3086`,
    during `IPA pass: modref` -- when a submodule calls a module-level procedure pointer under
    `-flto`, which is what `--profile release` builds with. Every ingredient was bisected: the
    optimisation level is irrelevant, so are `save`, `=> null()` and accessibility, copying the
    pointer to a local first does not help, and a submodule of a DIFFERENT module that
    use-associates the pointer fails identically. Calling from the owning module's own `contains`
    is clean, which is what `parquet_argsort`'s seven `oracle_*` relays do.

    Even a bare reference is unsafe: passing `associated(p_argsort)` as an actual argument
    reproduces it, though the `associated` test alone does not. So the rule is that no submodule
    mentions a `p_*` name at all, and `check_oracle` is folded into each relay for that reason.

    This needs a static check rather than a test because NOTHING in CI or in a plain `fpm test`
    builds with `-flto`: a reintroduced call would compile, pass every test and sit in the tree
    until someone next asked for a release build. See CLAUDE.md's "Compiler & language gotchas".
    """
    pointers = ("p_argsort", "p_partial", "p_nth", "p_is_sorted", "p_runs", "p_search", "p_merge")
    problems = []
    seen_any = False
    for path in sorted(SRC.glob("*.f90")):
        text = path.read_text(encoding="utf-8")
        if not re.search(r"(?im)^\s*submodule\s*\(", text):
            continue
        for lineno, line in enumerate(text.split("\n"), start=1):
            code = line.split("!", 1)[0]
            for ptr in pointers:
                if re.search(r"\b" + ptr + r"\b", code):
                    seen_any = True
                    problems.append(
                        f"{path.name}:{lineno}: a submodule names `{ptr}`; gfortran ICEs under "
                        f"-flto on this. Call `oracle_{ptr[2:]}` (parquet_argsort's relay) instead")
    # The relays themselves must exist, or this check is passing against a tree where the whole
    # mechanism has been removed and every submodule call would be legal again.
    argsort = SRC / "parquet_argsort.f90"
    if argsort.is_file():
        body = argsort.read_text(encoding="utf-8")
        missing = [p for p in pointers
                   if f"subroutine oracle_{p[2:]}(" not in body]
        if missing:
            problems.append(
                "parquet_argsort.f90: these oracle relays are gone: "
                + ", ".join("oracle_" + m[2:] for m in missing)
                + " -- without them there is nothing for a submodule to call, and the -flto ICE "
                  "workaround has been undone")
    return problems


def check_facades_hide_the_same_names():
    """`use parquet_io` must not expose a name `use parquet` deliberately hides.

    There are two facades over `parquet_core`: `src/parquet.f90` (the whole library) and
    `src/parquet_io.f90` (the read/write surface alone). Both use the same mechanism -- a bare
    `use` under default-PUBLIC accessibility, which re-exports everything, plus `private ::`
    statements naming the cross-module plumbing that `parquet_core` and `parquet_settings` are
    forced to make public for want of package scope.

    That mechanism has one failure mode, and nothing else can see it: a name added to
    `src/parquet.f90`'s private list and not to `src/parquet_io.f90`'s stays public from
    `parquet_io`, so the narrower import exposes MORE than the wider one. It compiles, it passes
    every test, and it silently makes a piece of internal plumbing part of the versioned surface --
    which is the expensive direction, since removing it again is then a breaking change.

    The check is deliberately one-directional. `parquet_io` may privatise a name `parquet.f90` does
    not, and in fact must: `parquet_split_name_list` and `parquet_parse_sort_key` are hidden here
    and CANNOT be repeated in the facade, because `parquet_io` has already made them inaccessible
    there and a `private ::` naming an inaccessible symbol is an error (nagfor reports it as an
    implicitly-typed local; gfortran as an unknown symbol). Only names that both facades can see are
    compared.
    """
    problems = []
    facade_path = SRC / "parquet.f90"
    io_path = SRC / "parquet_io.f90"
    for path in (facade_path, io_path, SRC / "parquet_core.f90", SRC / "parquet_settings.f90"):
        if not path.exists():
            return ["%s not found -- this check cannot have run" % path]

    # Both source sets are required to be non-empty, not merely their union: parquet_settings
    # alone would keep the union populated while parquet_core contributed nothing, and the check
    # would then silently compare against half the surface it is supposed to cover.
    core_public = _access_names((SRC / "parquet_core.f90").read_text(), "public")
    settings_public = _access_names((SRC / "parquet_settings.f90").read_text(), "public")
    for name, names in (("parquet_core", core_public), ("parquet_settings", settings_public)):
        if not names:
            return ["src/%s.f90 declares no public names -- this check cannot have run" % name]
    reachable = core_public | settings_public

    facade_private = _access_names(facade_path.read_text(), "private")
    io_private = _access_names(io_path.read_text(), "private")
    if not facade_private or not io_private:
        return ["a facade declares no `private ::` names -- this check cannot have run"]

    # A name the facade hides that came from parquet_core/parquet_settings must be hidden by
    # parquet_io too -- unless parquet_io does not import it at all, which cannot happen, since it
    # imports both modules whole.
    for name in sorted(facade_private & reachable):
        if name not in io_private:
            problems.append(
                "src/parquet_io.f90: `%s` is private in src/parquet.f90 but public here, so "
                "`use parquet_io` exposes a name `use parquet` deliberately hides. Add a "
                "`private :: %s` with the reason, next to its siblings." % (name, name))
    return problems


def check_facade_hides_healpix_run_query():
    """`pf_query_disc_runs` is public in `parquet_healpix` for one caller, and must stay hidden
    from `use parquet`.

    `parquet_healpix` is an advertised entry module whose public surface is a HEALPix API a user
    is offered. `pf_query_disc_runs` is not part of that offer: it exists so `parquet_spatial`'s
    HEALPix sky backend can turn one disc into contiguous slices of its bucketed point array
    without re-deriving a decomposition the module already computes, and a program holding a pixel
    list has `pf_query_disc` for everything else. It is public only because Fortran has no package
    scope -- the same reason `parquet_strings`' typed accessors and `parquet_list`'s offset/payload
    accessors are public and then privatised again in the facade.

    **Nothing else can see a regression here.** The facade re-exports `parquet_healpix` with a
    bare `use`, so deleting the `private ::` compiles, links, passes every test, and silently puts
    a plumbing procedure into every `use parquet` program's namespace -- at which point removing it
    again is a breaking change.

    The first clause is what stops this check going stale: if the procedure is ever removed or
    renamed, the check fails saying so rather than passing vacuously against a name that no longer
    exists.
    """
    name = "pf_query_disc_runs"
    healpix = SRC / "parquet_healpix.f90"
    facade = SRC / "parquet.f90"
    for path in (healpix, facade):
        if not path.exists():
            return ["%s not found -- this check cannot have run" % path]
    if name not in _access_names(healpix.read_text(), "public"):
        return [
            "src/parquet_healpix.f90 no longer declares `public :: %s`. If it was renamed, "
            "update this check and src/parquet.f90's `private ::` together; if it was removed, "
            "delete both." % name
        ]
    if name not in _access_names(facade.read_text(), "private"):
        return [
            "src/parquet.f90: `%s` is public in parquet_healpix but not privatised here, so "
            "`use parquet` now exposes it. Add a `private :: %s` with the reason, next to its "
            "siblings." % (name, name)
        ]
    return []


def _access_names(text, keyword):
    names = set()
    for raw in text.splitlines():
        line = raw.split("!", 1)[0].strip()
        if not line.lower().startswith(keyword + " ::"):
            continue
        body = line[len(keyword) + 3:]
        # A generic-binding or renaming form is not a plain accessibility statement.
        if "=>" in body:
            continue
        for name in body.split(","):
            name = name.strip()
            if name and name.isidentifier():
                names.add(name.lower())
    return names


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
            # Rule 6: that list's own introductory sentence STATES its length ("All 24 pages, in
            # reading order"), and until this check existed nothing compared the two. It said 23
            # while the list held 24 and the tree held 24, stale since operating/choosing-a-module.md
            # was inserted -- and the same list's length was written down in two further places that
            # had each drifted to a different number. A count in prose beside the thing it counts is
            # the cheapest form of the hazard CLAUDE.md records under "A static check that enumerates
            # names goes stale silently"; here the list is machine-readable, so the sentence can
            # simply be held to it.
            #
            # Matched by SHAPE rather than by the exact sentence, so rewording the line does not
            # silently switch the check off -- and a missing number FAILS rather than passes, since
            # an absent count is indistinguishable from a check that has gone blind.
            stated = re.search(r"\ball\s+(\d+)\s+pages\b", body, re.I)
            if stated is None:
                problems.append(
                    "doc/pages/index.md: the every-page-at-a-glance list no longer states its own "
                    "length (\"All N pages\") -- either the sentence was reworded past this check "
                    "or the count was dropped; restore it or remove this rule deliberately"
                )
            elif int(stated.group(1)) != len(actual):
                problems.append(
                    "doc/pages/index.md: the list says \"All %s pages\" but holds %d entries -- the "
                    "count is hand-written prose beside a hand-written list and goes stale the "
                    "moment a page is added" % (stated.group(1), len(actual))
                )
    return problems


def check_landing_page_names_every_entry_module():
    """doc/pages/index.md's orientation must name every entry module the authority lists.

    `operating/choosing-a-module.md` says outright that its table is the authority -- "anywhere else
    in the repository that appears to enumerate entry modules is describing it, not defining it" --
    and `tools/check_module_footprints.sh`'s ENTRY_MODULES is the same list, measured. The guide's
    landing page is one of those "anywhere else" enumerations, and it had drifted: it omitted
    `parquet_version`, which is in the table, is measured by the footprint tool, and is the only
    route to `parquet_get_version` outside `use parquet` -- i.e. exactly the case the sentence is
    addressed to, a reader taking a narrow import.

    **One direction only, deliberately.** The page legitimately names one module the table does not:
    `parquet_maml_base`, which `choosing-a-module.md` describes as "importable and promised but
    deliberately absent from the table". A both-directions rule would fail on that deliberate entry,
    and a check that fails on purpose gets switched off.

    ENTRY_MODULES is read from the shell script rather than re-listed here, so this check cannot
    drift from the thing it is checking against -- and an empty parse FAILS rather than passes.
    """
    problems = []
    doc = REPO_ROOT / "doc" / "pages" / "index.md"
    tool = REPO_ROOT / "tools" / "check_module_footprints.sh"
    m = re.search(r'ENTRY_MODULES="(.*?)"', tool.read_text(), re.S)
    if m is None:
        return ["tools/check_module_footprints.sh: could not find ENTRY_MODULES -- this check "
                "needs updating"]
    modules = [w for w in m.group(1).replace("\\\n", " ").split() if w and w != "parquet"]
    if not modules:
        return ["tools/check_module_footprints.sh: ENTRY_MODULES parsed empty -- this check needs "
                "updating"]
    text = doc.read_text()
    # The orientation prose only: everything before the group entries. A module named further down
    # (in a group description, say) does not make it discoverable as an import.
    cut = text.find("\nThe guide is organised into six groups")
    orientation = text[:cut] if cut > 0 else text
    for mod in modules:
        if "`%s`" % mod not in orientation:
            problems.append(
                "doc/pages/index.md: `%s` is an advertised entry module (it is in "
                "tools/check_module_footprints.sh's ENTRY_MODULES and in "
                "doc/pages/operating/choosing-a-module.md's table) but the landing page's "
                "orientation never names it -- a reader looking for a narrower import cannot "
                "discover it" % mod)
    return problems


def check_page_titles_match_their_list_entries():
    """A page's title, its flat-list entry and its group-index bullet must line up.

    Two clauses, because the three places are not in the same position. The frontmatter `title:` is
    the page's CANONICAL name -- FORD uses it for the browser tab, the `<h1>` and its own self-links
    -- so:

    1. the top-level flat list carries NO descriptions, so its entry must be the whole title;
    2. a group-index bullet carries a one-line description after the dash, so a shorter label is
       legitimate there, as long as it is an initial prefix of the title.

    Settled at row 29 of feature_doc.md's guide review after being parked twice. Before it, six flat
    entries said more than their page's own `<h1>` did -- `utilities/random.md` was titled "Random
    numbers" while its list entry said "Random numbers and sampling with pf_random_at", for a page
    half about sampling. The fix was to lengthen the titles, not to shorten the entries.

    Backticks are ignored on both sides: no frontmatter `title:` in the guide carries one, which is
    itself a settled convention.
    """
    problems = []
    pages = REPO_ROOT / "doc" / "pages"
    top = pages / "index.md"
    if not top.is_file():
        return ["doc/pages/index.md: missing -- this check needs updating"]
    top_text = top.read_text()

    def norm(t):
        return re.sub(r"\s+", " ", t.replace("`", "")).strip()

    seen = 0
    for group in sorted(d for d in pages.iterdir() if d.is_dir()):
        index = group / "index.md"
        if not index.is_file():
            continue                      # reported by check_doc_page_index_consistency
        index_text = index.read_text()
        for page in sorted(group.glob("*.md")):
            if page.name == "index.md":
                continue
            rel = "doc/pages/%s/%s" % (group.name, page.name)
            title_m = re.search(r"^title:\s*(.+)$", page.read_text(), re.M)
            if title_m is None:
                problems.append("%s: no frontmatter title:" % rel)
                continue
            title = norm(title_m.group(1))
            html = page.name[: -len(".md")] + ".html"
            flat_m = re.search(r"- \[([^\]]+)\]\(%s/%s\)"
                               % (re.escape(group.name), re.escape(html)), top_text)
            bullet_m = re.search(r"- \[([^\]]+)\]\(%s\)" % re.escape(html), index_text)
            if flat_m is None or bullet_m is None:
                continue                  # reported by check_doc_page_index_consistency
            seen += 1
            flat, bullet = norm(flat_m.group(1)), norm(bullet_m.group(1))
            if flat != title:
                problems.append(
                    "%s: the top-level flat list calls it %r but its title is %r -- the flat list "
                    "carries no description, so its entry must be the whole title" % (rel, flat, title))
            if bullet != title and not (
                    title.startswith(bullet) and title[len(bullet):][:1] in (" ", ":", ",")):
                problems.append(
                    "doc/pages/%s/index.md: the bullet for %s reads %r, which is neither its title "
                    "%r nor an initial prefix of it" % (group.name, page.name, bullet, title))
    if seen == 0:
        return ["doc/pages/: matched no page against both lists -- either the guide moved or this "
                "check needs updating"]
    return problems


def _footprint_counts():
    """Fortran-file count per entry module, from the committed measured footprints.

    `tools/module_footprints.txt` lists every file fpm compiles for a single-module import,
    which includes `src/parquet_wrapper.cpp` in EVERY section and always will (`link` is a
    package-level key in fpm.toml). The two published tables count Fortran files, so the C++
    translation unit is excluded here rather than subtracted -- if a second one is ever added,
    this keeps working, and `check_single_cpp_translation_unit` is what forbids that anyway.
    """
    text = (TOOLS / "module_footprints.txt").read_text()
    counts, section = {}, None
    for line in text.splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        m = re.match(r"^\[(.+)\]$", line)
        if m:
            section = m.group(1)
            counts[section] = 0
            continue
        if section is not None and not line.endswith(".cpp"):
            counts[section] += 1
    return counts


def _module_table_rows(path):
    """The `| `module` | N | ... |` rows of a published entry-module table, as {module: N}."""
    rows = {}
    for line in path.read_text().splitlines():
        m = re.match(r"^\|\s*`([a-z_]+)`\s*\|\s*(\d+)\s*\|", line)
        if m:
            rows[m.group(1)] = int(m.group(2))
    return rows


def check_module_tables_match_the_measured_footprints():
    """README.md's and choosing-a-module.md's Files columns must equal the measured footprints.

    Both pages carry the same twelve-row entry-module table, and its Files column is a MEASURED
    quantity: `tools/check_module_footprints.sh` builds a throwaway consumer per module and diffs
    the result against `tools/module_footprints.txt`. That script never reads either page
    (`grep -c README` on it returns 0), and until this check existed nothing else did either -- so
    the number a reader uses to choose an import was maintained by hand against a file the build
    already measures.

    It had already gone wrong. Row 22a found **3 of the 11 rows then present** wrong -- `parquet_io`
    43 -> 44, `parquet_tables` 64 -> 62, `parquet` 65 -> 66 -- and fixed them by hand; README was not
    even in that review's plan and was caught only because it carries the same table. The counts move
    whenever a `use` line is added anywhere in the library, which is the change least likely to
    prompt anyone to open README.

    Both tables are checked against the measurement INDEPENDENTLY rather than against each other, so
    a one-sided edit cannot pass by making the two agree on a wrong number.

    The guide page's opening PROSE count is checked too, and it is the reason this check grew: the
    page said `use parquet` "compiles **66** of this library's Fortran files" while its own table
    two screens below said 73. It had been written once and never revisited while the table moved
    twice, and nothing looked at it -- exactly CLAUDE.md's "A count written out beside the list it
    counts ... needs a check". Only the bolded numeric form is covered; the page's other prose
    count spells its number as a word, and a regex for that would cost more than it protects.
    """
    problems = []
    measured = _footprint_counts()
    if not measured:
        return ["tools/module_footprints.txt: parsed no sections -- this check needs updating"]
    tables = {
        "README.md": REPO_ROOT / "README.md",
        "doc/pages/operating/choosing-a-module.md":
            REPO_ROOT / "doc" / "pages" / "operating" / "choosing-a-module.md",
    }
    for label, path in tables.items():
        rows = _module_table_rows(path)
        if not rows:
            problems.append("%s: found no entry-module table rows -- either the table moved or its "
                            "shape changed, and this check needs updating" % label)
            continue
        for mod, stated in sorted(rows.items()):
            if mod not in measured:
                problems.append(
                    "%s: the table has a row for `%s`, which has no section in "
                    "tools/module_footprints.txt. Add it to ENTRY_MODULES in "
                    "tools/check_module_footprints.sh and re-measure, or drop the row." % (label, mod))
                continue
            if stated != measured[mod]:
                problems.append(
                    "%s: `%s` is listed as %d Fortran file(s); tools/module_footprints.txt measures "
                    "%d. Re-run tools/check_module_footprints.sh and correct the table -- a `use` "
                    "line added anywhere in the library moves these counts."
                    % (label, mod, stated, measured[mod]))
        for mod in sorted(set(measured) - set(rows)):
            problems.append(
                "%s: `%s` is measured in tools/module_footprints.txt but has no row in the table. "
                "Every advertised entry module needs one, or a reader cannot compare imports."
                % (label, mod))

    page = REPO_ROOT / "doc" / "pages" / "operating" / "choosing-a-module.md"
    prose = re.search(r"compiles \*\*(\d+)\*\* of this library's Fortran files", page.read_text())
    if not prose:
        problems.append("doc/pages/operating/choosing-a-module.md: the opening sentence's "
                        "\"compiles **N** of this library's Fortran files\" no longer matches -- "
                        "either it moved or its wording changed, and this check needs updating")
    elif "parquet" in measured and int(prose.group(1)) != measured["parquet"]:
        problems.append(
            "doc/pages/operating/choosing-a-module.md: the opening sentence says `use parquet` "
            "compiles %s Fortran files; tools/module_footprints.txt measures %d. This prose count "
            "is separate from the table below it and had already drifted by seven before it was "
            "checked -- correct it together with the table."
            % (prose.group(1), measured["parquet"]))
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
    static check is the cheap guard, and `bench/random_large_fill.sh` is the end-to-end proof
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


def check_threads_are_forwarded():
    """A sorting procedure that takes `threads` must hand it to every callee that takes one.

    **This is a lint check because no test can be one.** Dropping a `threads=threads` from a call
    does not change any answer -- the callee simply resolves the AUTOMATIC count instead of the
    requested one -- so the sort still returns a bit-identical permutation and the whole suite
    stays green. What breaks is the documented contract that `threads=1` forces serial and that
    `threads=n` is a ceiling: both silently become "use the machine". Confirmed by mutation while
    `pf_partial_argsort` was given its own `threads=` (feature_sort.md's P10): stripping the
    plumbing from all 24 generated bodies left every test in the `sorting` suite passing.

    Matched by SHAPE, in both directions, so it cannot go stale the way an enumerated list does:
    the set of procedures that take `threads` is read from the two spec files rather than listed
    here, and a call is only checked when its CALLEE is in that set. That is what keeps the
    extraction kernels out of it -- `extract_i32_par` takes a resolved `team`, not a request -- and
    what makes a new threaded helper covered on the day it is added.

    Forwarding positionally counts: `resolve_thread_count(threads, n, nth)` is how every extractor
    does it. Passing a LITERAL (`threads=1`) does not -- that is the mutation this exists to catch.
    """
    specs = [SRC / "parquet_sorting.f90", SRC / "parquet_argsort.f90"]
    missing = [s for s in specs if not s.is_file()]
    if missing:
        return ["tools/check_source_conventions.py: %s not found -- this check has gone stale and "
                "is silently testing nothing" % ", ".join(m.name for m in missing)]
    takes = set()
    for spec in specs:
        text = spec.read_text(encoding="utf-8", errors="replace")
        for m in re.finditer(r"^\s*module subroutine (\w+)\((.*?)\)\s*$", text, re.M):
            if re.search(r"\bthreads\b", m.group(2)):
                takes.add(m.group(1))
    problems = []
    if not takes:
        return ["no sorting procedure declaring a `threads` dummy was found at all -- either the "
                "argument was removed library-wide or this check's pattern has gone stale; either "
                "way it must not report success"]
    bodies = sorted(SRC.glob("parquet_sorting_*.f90")) + [SRC / "parquet_argsort_kernel.f90"]
    checked = 0
    for path in bodies:
        if not path.is_file():
            continue
        text = path.read_text(encoding="utf-8", errors="replace")
        for m in re.finditer(r"^    module procedure (\w+)\s*$(.*?)^    end procedure \1\s*$",
                             text, re.M | re.S):
            name, body = m.group(1), m.group(2)
            if name not in takes:
                continue
            for call in re.finditer(r"call (\w+)\(((?:[^()]|\([^()]*\))*)\)", body, re.S):
                callee, args = call.group(1), call.group(2)
                # A procedure calling ITSELF is a recursive forward, not a drop.
                if callee not in takes or callee == name:
                    continue
                checked += 1
                flat = " ".join(args.split())
                parts = [a.strip() for a in re.split(r",(?![^()]*\))", flat)]
                if not any(a == "threads" or a == "threads=threads" for a in parts):
                    problems.append(
                        "%s: %s calls %s without forwarding `threads` -- the callee will resolve "
                        "the automatic count, so an explicit threads= is silently ignored (%s)"
                        % (path.name, name, callee, flat[:90]))
    if checked == 0:
        problems.append(
            "no call from one threads-taking sorting procedure to another was found -- the "
            "`module procedure` pattern has probably gone stale, so this check is testing nothing")
    return problems


def check_openmp_calls_are_guarded():
    """Every reference to an `omp_*` entity sits inside `#ifdef _OPENMP`.

    A build without OpenMP compiles every `!$omp` directive away -- they are comments -- but NOT
    the ordinary Fortran that surrounds them. So `use omp_lib`, `omp_get_thread_num()` and
    `omp_lock_kind` are undeclared names there, and the file fails to compile rather than falling
    back to the serial path the guards exist to provide.

    **Nothing in the ordinary fleet can see this.** fpm's `openmp = "*"` metapackage supplies
    `-fopenmp` for gfortran and ifx, so `_OPENMP` is always defined in CI and in every local
    `fpm test`; only a toolchain the metapackage does not cover (flang, per CLAUDE.md) or a
    deliberately disabled build reaches the other arm. It has bitten twice -- once in
    `materialize_marked_parallel` (src) and once in `test_table_parallel.f90`, where a threading
    test added an unguarded `omp_get_num_threads()` and the file stopped compiling serially for
    two days with every check green.

    Directives need no guard and are not flagged: `!$omp ...` is a comment, so `strip_comment`
    removes it before this check ever sees it. What is flagged is the ordinary code -- the import,
    the runtime call, the kind parameter.

    String LITERALS are blanked as well as comments. Several error scenarios print a skip message
    naming `omp_get_max_threads()` in prose, and `strip_comment` respects quotes without removing
    what is inside them -- so without this the check reports six violations that are text.
    """
    def blank_strings(text):
        """`text` with the contents of every quoted literal replaced by spaces."""
        out = []
        quote = None
        for ch in text:
            if quote:
                out.append(" ")
                if ch == quote:
                    out[-1] = ch
                    quote = None
            elif ch in ("'", '"'):
                quote = ch
                out.append(ch)
            else:
                out.append(ch)
        return "".join(out)

    problems = []
    open_re = re.compile(r"^\s*#\s*if(n?)def\s+_OPENMP\b|^\s*#\s*if\s+.*\bdefined\s*\(\s*_OPENMP\s*\)", re.I)
    any_if_re = re.compile(r"^\s*#\s*if", re.I)
    else_re = re.compile(r"^\s*#\s*el(se|if)\b", re.I)
    endif_re = re.compile(r"^\s*#\s*endif\b", re.I)
    omp_re = re.compile(r"\bomp_\w+", re.I)
    for path in sorted(list(SRC.glob("*.f90")) + list(TEST.glob("*.f90"))):
        # Stack of per-#if states: True while the enclosing branch is one where _OPENMP is known
        # defined. An unrelated #if contributes None -- neither guarding nor un-guarding, so a
        # nested `#ifdef _OPENMP` inside it still counts.
        stack = []
        for lineno, raw in enumerate(path.read_text().split("\n"), start=1):
            m = open_re.match(raw)
            if m:
                # `#ifdef _OPENMP` guards; `#ifndef _OPENMP` is the arm where it is NOT defined.
                stack.append(False if m.group(1) else True)
                continue
            if any_if_re.match(raw):
                stack.append(None)
                continue
            if else_re.match(raw):
                if stack and stack[-1] is not None:
                    stack[-1] = not stack[-1]
                continue
            if endif_re.match(raw):
                if stack:
                    stack.pop()
                continue
            if any(state is True for state in stack):
                continue
            code = blank_strings(strip_comment(raw))
            hit = omp_re.search(code)
            if hit:
                problems.append(
                    "%s:%d: `%s` is referenced outside `#ifdef _OPENMP`, so this file does not "
                    "compile without OpenMP -- the `!$omp` directives vanish there but this line "
                    "does not. Guard it (and give the serial arm a value, or skip the test):\n    %s"
                    % (path.relative_to(REPO_ROOT), lineno, hit.group(0), raw.strip())
                )
    return problems


def check_statement_continuation_lines():
    """No Fortran statement may carry more than 255 continuation lines.

    F2008 C1003 caps a statement at 255 continuation lines. **Only nagfor enforces it** --
    gfortran, ifx and flang all accept more without a word -- so a violation compiles clean on
    every compiler CI and an ordinary `fpm test` use, and is found only when someone runs a
    nagfor build. That is the same "invisible outside one toolchain" shape as the OpenMP-guard
    and 132-column rules, and it earns a static check for the same reason.

    Confirmed instance: `collect_tests_parquet_errors` (`test/test_errors.f90`) builds its
    `unittest_type` array in parts precisely because one constructor would blow the limit, and
    a part still reached **258** and broke the nagfor build with
    `Error: Too many continuation lines`. The fix is to split the part, not to reflow it -- see
    that subroutine's own comment, which records the two alternatives that do not work.

    Matched by SHAPE rather than against a list of known statements, so a new long constructor
    anywhere in the tree is covered without editing this check. Comment lines inside a continued
    statement are not counted: they are not continuation lines, and counting them would report a
    conforming statement as a violation.
    """
    problems = []
    roots = [SRC, REPO_ROOT / "test", REPO_ROOT / "app", REPO_ROOT / "bench"]
    scanned = 0
    for root in roots:
        if not root.is_dir():
            continue
        for path in sorted(root.glob("*.f90")) + sorted(root.glob("*.F90")):
            scanned += 1
            run_start, run = 0, 0
            for lineno, line in enumerate(path.read_text(encoding="utf-8",
                                                         errors="replace").splitlines(), 1):
                bare = line.strip()
                if not bare or bare.startswith("!"):
                    continue          # a comment line is not a continuation line
                code = strip_comment(line).rstrip()
                if run == 0:
                    run_start = lineno
                if code.endswith("&"):
                    run += 1
                    continue
                if run > 255:
                    problems.append("%s:%d: a statement carries %d continuation lines (max 255)"
                                    % (path.relative_to(REPO_ROOT), run_start, run))
                run = 0
            if run > 255:
                problems.append("%s:%d: a statement carries %d continuation lines (max 255)"
                                % (path.relative_to(REPO_ROOT), run_start, run))
    if not scanned:
        return ["tools/check_source_conventions.py: no Fortran source found -- this check has "
                "gone stale and is testing nothing"]
    if problems:
        return ["a Fortran statement may carry at most 255 continuation lines (F2008 C1003).",
                "ONLY nagfor enforces this, so a violation builds clean under gfortran/ifx/flang",
                "and breaks only a nagfor run. Split the statement -- for a test-suite array,",
                "close the part and open a new one rather than growing the existing part:"] + problems
    return []


def check_contributing_is_an_index():
    """CONTRIBUTING.md names each tool ONCE, in its index table -- never in a paragraph.

    CONTRIBUTING.md is project-wide workflow; how one tool works belongs in that tool's own header
    comment, where whoever changes the tool will see it. See CLAUDE.md, "CONTRIBUTING.md is
    project-wide workflow ONLY -- a tool's own detail goes in its header".

    The repository's scripts live in TWO directories, split by contract -- `tools/` for anything
    something automated depends on, `bench/` for measurement run only by hand -- and both are
    indexed in the same section, one table per directory. This check walks both.

    Three clauses, and the first two keep the index honest in both directions:

    1. Every executable script under tools/ and bench/ has exactly one row in the index. Without
       this a new tool is simply absent and nobody finds out, which is how one benchmark wrapper
       stayed undocumented until this check existed.
    2. Every index row resolves to a file that exists in one of the two directories, so a deleted
       or renamed tool cannot leave a row behind.
    3. Inside the section every tools/ or bench/ mention is a table row, the section stays at most
       a quarter prose, and elsewhere in the file no single path is named more than
       MAX_PROSE_MENTIONS times. That is what stops a paragraph about one tool growing back: a
       genuine workflow reference ("run tools/run_lint_check.sh before pushing") is one or two
       mentions, an essay is many.

    Why it exists: that section had reached 909 lines, 64% of the whole file, and had drifted into
    contradicting two of the script headers it duplicated -- benchmark_table.sh's about which output
    line to read, and benchmark_sort_engine.sh's about which sort engine ships.
    """
    MAX_PROSE_MENTIONS = 3
    DIRS = ("tools", "bench")
    path = REPO_ROOT / "CONTRIBUTING.md"
    if not path.exists():
        return ["CONTRIBUTING.md: missing -- this check needs updating"]
    lines = path.read_text(encoding="utf-8").splitlines()

    scripts = set()
    for d in DIRS:
        base = REPO_ROOT / d
        if not base.is_dir():
            return ["%s/: missing -- this check needs updating" % d]
        for f in sorted(base.rglob("*")):
            if not f.is_file():
                continue
            rel = f.relative_to(REPO_ROOT).as_posix()
            if f.suffix in (".sh", ".py") or (f.suffix == "" and f.read_bytes()[:2] == b"#!"):
                scripts.add(rel)
            elif f.suffix == ".md":
                scripts.add(rel)          # a run-sheet template is a tool too

    start = end = None
    for i, ln in enumerate(lines):
        if ln.startswith("### Other tools/ helpers"):
            start = i
        elif start is not None and ln.startswith("### ") and i > start:
            end = i
            break
    if start is None:
        return ["CONTRIBUTING.md: no '### Other tools/ helpers' section -- this check needs updating"]
    if end is None:
        end = len(lines)

    def resolve(name):
        """An index row may name a tool bare or with its directory; resolve it to a real path."""
        if "/" in name and name.split("/")[0] in DIRS:
            return name
        for d in DIRS:
            if (REPO_ROOT / d / name).exists():
                return "%s/%s" % (d, name)
        return None

    problems = []
    indexed = {}
    prose_lines = body_lines = 0
    in_fence = False
    for i in range(start, end):
        ln = lines[i]
        if ln.lstrip().startswith("```"):
            in_fence = not in_fence
            continue
        if ln.startswith("|"):
            body_lines += 1
            first = ln.split("|")[1] if ln.count("|") >= 2 else ""
            m = re.search(r"`((?:tools/|bench/)?[A-Za-z0-9_./-]+\.(?:sh|py|cpp|md)|nagfor_fpm_shim/nagfor)`",
                          first)
            if m:
                rel = resolve(m.group(1))
                if rel is None:
                    problems.append(
                        "CONTRIBUTING.md:%d: index row names %s, which exists in neither %s"
                        % (i + 1, m.group(1), " nor ".join(d + "/" for d in DIRS))
                    )
                else:
                    indexed.setdefault(rel, []).append(i + 1)
            continue
        if not ln.strip() or ln.startswith("#") or in_fence:
            continue
        body_lines += 1
        prose_lines += 1

    if body_lines and prose_lines * 4 > body_lines:
        problems.append(
            "CONTRIBUTING.md: the 'Other tools/ helpers' section is %d%% prose (%d of %d lines); it "
            "must stay an index -- move per-tool detail into that tool's own header (CLAUDE.md)"
            % (round(100 * prose_lines / body_lines), prose_lines, body_lines)
        )

    for rel, where in sorted(indexed.items()):
        if len(where) > 1:
            problems.append(
                "CONTRIBUTING.md: %s has %d index rows (lines %s); it must have exactly one"
                % (rel, len(where), ", ".join(str(w) for w in where))
            )
        if not (REPO_ROOT / rel).exists():
            problems.append(
                "CONTRIBUTING.md:%d: index row names %s, which does not exist" % (where[0], rel)
            )

    for rel in sorted(scripts - set(indexed)):
        problems.append(
            "%s: no row in CONTRIBUTING.md's tools index -- add one line naming what it is for" % rel
        )

    counts = {}
    for i, ln in enumerate(lines):
        if start <= i < end:
            continue
        for h in re.findall(r"`((?:tools|bench|app)/[A-Za-z0-9_./-]+\.(?:sh|py|cpp|f90|md))`", ln):
            counts.setdefault(h, []).append(i + 1)
    for rel, where in sorted(counts.items()):
        if len(where) > MAX_PROSE_MENTIONS:
            problems.append(
                "CONTRIBUTING.md: %s is named %d times outside the index (lines %s); at most %d "
                "workflow references are allowed -- move the detail into its own header"
                % (rel, len(where), ", ".join(str(w) for w in where), MAX_PROSE_MENTIONS)
            )
    return problems


#: The column types whose `%view`/`%view_all` hand back a handle holding a pointer to the passed
#: object. F2018 15.5.2.4 leaves that pointer UNDEFINED on return unless the actual argument has
#: the TARGET attribute, so every call site must declare its column `target`.
VIEWED_COLUMN_TYPES = (
    "parquet_list_column",
    "parquet_map_column",
    "parquet_struct_column",
    "parquet_string_column",
)


def _procedure_scopes(lines):
    """Yield `(first, last)` 0-based line index pairs, one per procedure body in `lines`."""
    opener = re.compile(r"^\s*(?:(?:pure|impure|elemental|recursive|module)\s+)*"
                        r"(?:[\w()=:,*\s]+?\s)??(?:subroutine|function)\s+\w+\s*\(", re.I)
    closer = re.compile(r"^\s*end\s*(?:subroutine|function)\b", re.I)
    stack, scopes = [], []
    for i, raw in enumerate(lines):
        code = strip_comment(raw)
        if opener.match(code):
            stack.append(i)
        elif closer.match(code) and stack:
            scopes.append((stack.pop(), i))
    return scopes


def check_view_call_sites_declare_target():
    """CLAUDE.md -- a `%view` call site must declare its column `target`.

    `parquet_list_column%view`, its map/struct twins and `parquet_string_column%view`/`%view_all`
    return a handle whose stored pointer is associated with the passed-object dummy. F2018
    15.5.2.4 leaves such a pointer UNDEFINED on return whenever the actual argument does not have
    the TARGET attribute -- so the handle is unusable, and the `associated(self%col)` guard inside
    every handle binding is itself non-conforming.

    **gfortran, ifx and flang all execute the non-conforming form perfectly happily**, so nothing
    in CI, in an ordinary `fpm test` or in a coverage run can see this. Only nagfor's `-C=dangling`
    reports it, as an abort naming the handle binding rather than the call that produced the
    handle. That is what makes it worth a static check: the failure mode is a silent one on every
    compiler the project builds with by default.

    The check is scope-aware -- a declaration is only reported when `%view` is called on that name
    inside the same procedure -- so a column merely declared beside an unrelated handle costs
    nothing.
    """
    problems = []
    call = re.compile(r"(?<![\w%])([A-Za-z_]\w*)%view(?:_all)?\s*\(")
    decl = re.compile(r"^\s*(?:type|class)\s*\(\s*(%s)\s*\)(.*?)::(.*)$"
                      % "|".join(VIEWED_COLUMN_TYPES), re.I)
    roots = [SRC, TEST, REPO_ROOT / "app", REPO_ROOT / "bench"]
    paths = sorted(p for root in roots if root.is_dir() for p in root.glob("*.f90"))
    seen_any_call = False
    for path in paths:
        lines = path.read_text().split("\n")
        for first, last in _procedure_scopes(lines):
            body = lines[first:last + 1]
            viewed = set()
            for raw in body:
                for m in call.finditer(strip_comment(raw)):
                    viewed.add(m.group(1).lower())
            if not viewed:
                continue
            seen_any_call = True
            for offset, raw in enumerate(body):
                m = decl.match(strip_comment(raw))
                if not m:
                    continue
                attrs = m.group(2).lower()
                if "target" in attrs or "pointer" in attrs:
                    continue
                names = [re.sub(r"\(.*", "", n).strip().lower() for n in m.group(3).split(",")]
                hit = sorted(n for n in names if n in viewed)
                if not hit:
                    continue
                problems.append(
                    "%s:%d: `%s` is a %s that `%%view` is called on in this procedure, but it is "
                    "not declared `target`. F2018 15.5.2.4 leaves the returned handle's pointer "
                    "undefined; gfortran, ifx and flang run it anyway and only nagfor's "
                    "`-C=dangling` reports it. Add `, target` to the declaration:\n    %s"
                    % (path.relative_to(REPO_ROOT), first + offset + 1, ", ".join(hit),
                       m.group(1), raw.strip()))
    if not seen_any_call:
        problems.append(
            "no `%view`/`%view_all` call site found under src/, test/, app/ or bench/ -- the "
            "handle API has been renamed, or this check's scope detection has gone stale and it "
            "is passing vacuously")
    return problems


CHECKS = (
    ("threads= is forwarded to every callee that takes it", check_threads_are_forwarded),
    ("omp_* references are guarded by #ifdef _OPENMP", check_openmp_calls_are_guarded),
    ("benchmark build-tree names carry the compiler", check_build_tree_names_carry_the_compiler),
    ("parquet_table has no allocatable component", check_no_allocatable_component),
    ("MAML block headers are matched case-insensitively", check_maml_keys_case_insensitive),
    ("table pointers are reached through %cache", check_pointers_go_through_cache),
    ("no per-cell path reaches a column through a binding", check_no_type_bound_column_access),
    ("container accessors resolve through table_resolve", check_container_accessors_resolve),
    ("no path reaches a column's string store through a binding",
     check_no_type_bound_string_column_access),
    ("generated files carry their conventions", check_generated_file_conventions),
    ("the schema-less write declares auto sizes", check_schemaless_write_declares_auto),
    ("row-group reads guard against a sort", check_row_group_reads_guard_against_sort),
    ("print_stat's columns match its documentation", check_print_stat_columns_documented),
    ("print_settings matches its documentation", check_print_settings_documented),
    ("every setting is actually read", check_settings_are_read),
    ("no direct printing outside the emit channels", check_no_direct_printing),
    ("the row-group sizing arithmetic exists once", check_row_group_sizing_not_duplicated),
    ("src/ is a single C++ translation unit", check_single_cpp_translation_unit),
    ("the C++ side draws no random numbers of its own", check_one_random_number_generator),
    ("every setting has an environment variable", check_env_covers_every_setting),
    ("the guide's environment table matches the source", check_env_table_matches_the_source),
    ("the logging guide's environment table matches the source",
     check_log_env_table_matches_the_source),
    ("parquet_strings does not reach parquet_bindings", check_parquet_strings_stays_leaf),
    ("parquet_argsort stays Arrow-free", check_parquet_argsort_stays_arrow_free),
    ("parquet_sorting stays Arrow-free", check_parquet_sorting_stays_arrow_free),
    ("parquet_sampling stays Arrow-free", check_parquet_sampling_stays_arrow_free),
    ("parquet_columns stays Arrow-free", check_parquet_columns_stays_arrow_free),
    ("parquet_list stays Arrow-free", check_parquet_list_stays_arrow_free),
    ("parquet_struct stays Arrow-free", check_parquet_struct_stays_arrow_free),
    ("parquet_temporal stays Arrow-free", check_parquet_temporal_stays_arrow_free),
    ("parquet_version stays Arrow-free", check_parquet_version_stays_arrow_free),
    ("parquet_spatial stays Arrow-free", check_parquet_spatial_stays_arrow_free),
    ("parquet_healpix stays Arrow-free", check_parquet_healpix_stays_arrow_free),
    ("parquet_logging stays Arrow-free", check_parquet_logging_stays_arrow_free),
    ("parquet_stats stays Arrow-free", check_parquet_stats_stays_arrow_free),
    ("parquet_stats optionals follow one canonical order", check_stats_optional_argument_order),
    ("the facade inventory names every re-exported module",
     check_facade_inventory_matches_its_use_lines),
    ("parquet_get_version has exactly one home", check_get_version_has_one_home),
    ("parquet_random imports nothing from src/", check_parquet_random_stays_leaf),
    ("no submodule calls a sort-oracle procedure pointer",
     check_no_submodule_oracle_pointer_call),
    ("the two facades hide the same names", check_facades_hide_the_same_names),
    ("the facade hides pf_query_disc_runs", check_facade_hides_healpix_run_query),
    ("no per-element helper takes a shared_ptr", check_no_per_element_shared_ptr),
    ("no per-element string allocation in a bulk loop", check_no_per_element_string_alloc),
    ("every error scenario is named in the shell runner", check_scenario_list_is_complete),
    ("every intent(inout) temporal setter assigns all components", check_temporal_setters_assign_all),
    ("doc/pages index files agree with the page tree", check_doc_page_index_consistency),
    ("the landing page names every entry module", check_landing_page_names_every_entry_module),
    ("page titles match their list entries", check_page_titles_match_their_list_entries),
    ("the entry-module tables match the measured footprints",
     check_module_tables_match_the_measured_footprints),
    ("no doc/pages code fence is indented", check_no_indented_code_fence),
    ("no call aliases one variable onto a writable dummy", check_no_aliased_output_argument),
    ("parquet_random takes array lengths as int64", check_fill_size_kind),
    ("allocate extents from size() use int64", check_allocate_extent_kind),
    ("noinline directives carry both spellings", check_noinline_directives_are_paired),
    ("CONTRIBUTING.md names each tool once, in its index", check_contributing_is_an_index),
    ("no statement exceeds 255 continuation lines", check_statement_continuation_lines),
    ("every %view call site declares its column target",
     check_view_call_sites_declare_target),
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
