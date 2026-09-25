#!/usr/bin/env python3
"""Checks structural invariants of this repository that no compiler and no runtime test sees.

Each one protects a property whose violation compiles cleanly, passes the whole test suite, and
fails somewhere else entirely -- in a user's program, on another compiler, or in a coverage report.
That is what makes them worth a static check rather than a test:

  1. `parquet_table` must have NO allocatable component.
     The type is finalizable, so every allocatable component it gains makes the compiler generate a
     deeper recursive walk for its `intent(out)` entry and its FINAL -- and this project has three
     confirmed compiler bugs in exactly that machinery on exactly this type (gfortran leaving an
     OpenMP `private()` copy uninitialized; `%detached` surviving an `intent(out)` reset; ifx
     segfaulting inside its own runtime on a nested derived-type component). New table state goes on
     `parquet_table_cache`, reached through the pointer. See `.claude/rules/columns-tables.md`, "New `parquet_table` state
     goes on the CACHE".

  2. Every pointer a table accessor hands out must be reached through `self%cache`
     (`.claude/rules/columns-tables.md`, "`parquet_table` pointers and row-structural mutation").
     `%col` needs no `target` attribute on the caller's table only because the pointer targets heap
     owned by the cache rather than the dummy argument, so F2018 15.5.2.4's "pointer to a dummy's
     target becomes undefined on return" never applies. An accessor that points at `self` directly
     reintroduces the `target` requirement with no compiler diagnostic and no runtime check: the
     library still compiles, every test still passes, and a caller's program corrupts memory.
     `target` is a requirement on the CALLER, which is why nothing on this side can catch it.

  3. Generated files must carry the conventions their generator is supposed to emit
     (`.claude/rules/code-style.md`, "Generated files: edit the generator, never the output").
     `--check` on the generators compares the committed file against the generator, so a TEMPLATE
     that omits a `!>` doc-comment or a `! GCOVR_EXCL_LINE` produces output that matches perfectly
     and is wrong in every kind it emits at once. Only `ford --warn docs.md` would reveal the first
     (and it is not run in CI, and is drowned in thousands of expected warnings -- see `.claude/rules/documentation.md`'s
     "FORD doc-comment conventions"), and nothing at all reveals the second. A third generator
     inherits this check for free by adding its output to GENERATED_FILES below.

  4. The schema-less write declares `auto` sizes (this check is the only enforcement; see its docstring).
     `build_table_schema` must never MEASURE a `col_size`/`array_size` -- declaring `auto` is what
     makes it unable to get one wrong, since the writer then resolves both from the data exactly as
     it would with no schema at all. A change that measures here gets string widths wrong first, and
     the sidecar MAML still looks right because it is emitted at close, after the writer resolved
     them -- so a round-trip test cannot see the difference.

  5. Row-group-scoped reads guard against a sort
     (`.claude/rules/reader-writer.md`, "Row transforms: a MASK or a PERMUTATION").
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

  7. print_settings matches its documentation
     (`.claude/rules/api-conventions.md`, "Settings (`parquet_settings`)").
     Every row parquet_print_settings emits must be named in doc/pages/operating/settings.md. Matched by the
     SHAPE of a row call rather than by a list of helper names, because the list went blind twice --
     see check_print_settings_documented.

  8. Every setting is actually read
     (`.claude/rules/api-conventions.md`, "Settings (`parquet_settings`)").
     A `cfg_*` that nothing consults still round-trips through its own getter, still reports the
     right factory value, and silently does nothing. This finds the "nothing reads it" half; the
     observed-effect tests in test/test_settings.f90 find the "reads it wrongly" half.

  9. No direct printing outside the emit channels.
     Every message goes through parquet_emit_info/_warning/_error_context, which is what makes
     `verbosity` and `message_stream` apply everywhere. A print written the old way still appears at
     default settings, so the suite stays green and only users who changed a setting are affected.

 10. The row-group sizing arithmetic exists once (test/test_settings.f90's two
     `target_row_group_bytes sizes ...` tests pin one caller each).
     Its two callers serve different writers. A re-inlined copy takes the built-in constant instead
     of the setting, so parquet_set_target_row_group_bytes governs one kind of write and not the
     other -- every row correct, the row-group count wrong, nothing failing.

 11. Every settable knob has an environment variable
     (`.claude/rules/api-conventions.md`, "Settings (`parquet_settings`)").
     `parquet_settings_from_env` applies one PARQUET_FORTRAN_* variable per knob. A knob left out of
     that sequence makes its variable silently do nothing -- indistinguishable, from the user's side,
     from the setting being broken. Reuses parquet_print_settings' rows as the knob list, so a new
     knob fails this, the documentation check and the is-it-read check together.

 12. src/ is a single C++ translation unit (CLAUDE.md's TU-split note).
     Every process-global in parquet_wrapper.cpp is a file-scope `static`, so a second `.cpp` would
     get its own copy of each. Not a ban on splitting -- the note firing at the moment someone does.

 13. Every `intent(inout)` temporal setter assigns every component (`.claude/rules/columns-tables.md`,
     "`parquet_temporal` and the containers").
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

 16. feature_risks.md is a short register of OPEN risks (`check_risk_register_shape`).
     At most RISK_REGISTER_MAX_ENTRIES entries of at most RISK_REGISTER_MAX_BODY_LINES body lines,
     every `### ` heading an entry heading, no repeated number, one `Next number:` line above them
     all. CLAUDE.md, CONTRIBUTING.md, .gitlab-ci.yml, `.claude/**/*.md` and this script cite only
     open entries; a closed entry's requirement is cited by the rule or test that states it.
     Source, test and tool comments elsewhere are exempt (a closed number there is attribution).

The numbered notes above are the ones whose rationale needs more than a line; they are NOT the
complete list, and deliberately carry no count, because a hardcoded one drifts silently every time a
check is added (this docstring said "twelve" while CHECKS held fifteen). `--list` prints all of them.

Usage:
    tools/check_source_conventions.py            # run every check
    tools/check_source_conventions.py --list     # print what each check covers, then exit

Exits nonzero and prints one line per violation, each naming file:line and what to do instead.

Enforces sixteen structural invariants that no compiler and no
runtime test can see -- each one's violation compiles cleanly, passes the whole suite, and fails
somewhere else entirely. Each names the rule or the test that states its requirement, in its own
docstring and in the message it prints:

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
    while doing nothing at all (`.claude/rules/api-conventions.md`, "Settings (`parquet_settings`)").
    This catches "nothing reads it"; the observed-effect tests in `test/test_settings.f90` catch
    "reads it wrongly".
  * Nothing in `src/` writes to a unit directly outside the three emit channels. Routing every
    message through `parquet_emit_info`/`_warning`/`_error_context` is what makes `verbosity` and
    `message_stream` apply everywhere. A print written the old way still appears at default
    settings, so the suite stays green and only users who changed a setting are affected.
  * The row-group sizing arithmetic must exist once. Its two callers serve different writers (a
    whole-table write and the streaming path's estimate), so a re-inlined copy takes the built-in
    constant instead of the setting: `parquet_set_target_row_group_bytes` then governs one kind of
    write and not the other, with every row correct, the row-group count wrong, and nothing failing
    (the two `target_row_group_bytes` tests in `test/test_settings.f90` pin one caller each).
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
    small (`.claude/rules/code-style.md`, "Files and program units").
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
  * No scenario is named twice in `tools/run_error_scenarios.sh`. A repeat cannot be seen by
    reading a two-thousand-entry array, and it costs a duplicated out-of-process run in every
    driver of that list: CI's whole-array pass, `prime_error_scenarios`' parallel priming, and
    `tools/coverage.sh`'s pass over what priming did not reach.
  * `strip_comment`'s fast paths must agree with `strip_comment_by_loop` on every line in the
    tree. Almost every check in this file matches against comment-stripped source, so a fast path
    that gets a line wrong does not fail -- it quietly removes that line from another check's view
    and the run still prints `[ok]` for it. The loop is the definition; the fast paths are why a
    full run takes seconds rather than a minute, and this is what keeps them honest.
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
import difflib
import functools
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
#: generate_parquet_maml_base.sh. Add a new generator's output here; see the module docstring for why
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
    # Generated TEST vectors. Not source, but the banner matters more here than anywhere: a
    # reference table that has quietly stopped being generated is a table nothing checks, and
    # an edited value in one is a lie the implementation is then written to match.
    TEST / "test_path_vectors.f90",
    TEST / "test_interpolate_golden.f90",
    TEST / "test_sphere_vectors.f90",
    TEST / "test_skycoord_vectors.f90",
    TEST / "test_kde_golden.f90",
    TEST / "test_cosmology_vectors.f90",
    # test/test_healpix_vectors.f90 BELONGS here and is deliberately not listed yet. Adding it
    # fails immediately on a real gap -- its `end module` line carries no `! GCOVR_EXCL_LINE`,
    # which is fixed in tools/generate_healpix_reference.py but only reaches the file when
    # someone regenerates. That regeneration is a maintainer action rather than a routine one:
    # the emitted directions come from math.acos/cos/atan2, so a fresh run on a different libm
    # legitimately rewrites 46 lines in their last digits, and the values want
    # `--verify-oracle` against healpy afterwards. Add this line in the same change as that
    # regeneration.
]

#: Marker every generated file carries, checked so that a file dropping out of generation (or being
#: listed here by mistake) is caught rather than silently checked as if it were still generated.
GENERATED_MARKERS = ("GENERATED FILE -- DO NOT EDIT BY HAND", "automatically generated")


def strip_comment_by_loop(line):
    """The DEFINITION of "strip a Fortran comment": scan for a `!` that is not inside a string.

    `strip_comment` is what every caller uses; this is the answer it must give.
    `check_comment_stripper_fast_paths_agree` runs both over every line in the repository.
    """
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


def strip_comment(line):
    """Return `line` with any trailing Fortran comment removed, respecting quoted strings.

    The two fast paths are not a different answer, they are `strip_comment_by_loop`'s answer
    reached without the character loop, and they carry this whole file's run time: stripping is
    called once per line per check, tens of millions of times in a full run, and 95% of this
    repository's lines take one of them. A line with no `!` has no comment to strip whatever its
    quoting; a line with no quote character at all cannot have a `!` inside a string, so the first
    one found is the comment marker. A line carrying both falls through to the loop.

    Do not trust a review of this by eye -- a wrong answer here does not fail anything, it
    silently stops some other check from seeing a line. `check_comment_stripper_fast_paths_agree`
    is what holds the three arms together.
    """
    cut = line.find("!")
    if cut < 0:
        return line
    if "'" not in line and '"' not in line:
        return line[:cut]
    return strip_comment_by_loop(line)


def check_comment_stripper_fast_paths_agree():
    """`strip_comment`'s fast paths must answer exactly what `strip_comment_by_loop` answers.

    Every check in this file matches against comment-stripped source, so a fast path that gets one
    line wrong does not fail anything -- it removes that line from some other check's view, or
    hands it a fragment of a string literal to match against, and the check goes on reporting
    `[ok]`. There is no symptom to notice, which is why the equivalence is asserted rather than
    reasoned about.

    The corpus is every Fortran line in the repository, so the check is only as strong as the tree
    is varied; it covers the shapes that actually occur, which is what the fast paths have to be
    right about. The three synthetic lines below pin the cases the tree may not contain today: a
    bang inside a string with no comment after it, a bang inside a string followed by a real
    comment, and an apostrophe inside a double-quoted string.
    """
    problems = []
    probes = [
        """    call sub("a!b")""",
        """    call sub("a!b")  ! real comment""",
        """    msg = "it's here" ! tail""",
        """    msg = 'say "hi"!' ! tail""",
        """    x = 1""",
        """! whole-line comment""",
    ]
    for line in probes:
        if strip_comment(line) != strip_comment_by_loop(line):
            problems.append("strip_comment disagrees with strip_comment_by_loop on the probe %r: "
                            "%r vs %r" % (line, strip_comment(line), strip_comment_by_loop(line)))
    # Both functions are pure functions of the line, so a line repeated in the tree cannot tell us
    # anything the first copy did not: comparing the DISTINCT lines is the same assertion over
    # every line, for 40% less work. The first place each was seen is kept so that a failure still
    # names a file and a line rather than only a string.
    where = {}
    roots = [SRC, TEST, TOOLS, REPO_ROOT / "app", REPO_ROOT / "bench"]
    for root in roots:
        if not root.is_dir():
            continue
        for path in sorted(root.glob("*.f90")):
            for number, line in enumerate(source_lines(path), 1):
                where.setdefault(line, (path.name, number))
    for line, (name, number) in where.items():
        if strip_comment(line) != strip_comment_by_loop(line):
            problems.append(
                "%s:%d: strip_comment's fast path answers %r where the loop answers %r"
                % (name, number, strip_comment(line), strip_comment_by_loop(line)))
    if len(where) < 1000:
        problems.append("only %d distinct source lines were compared -- this check has gone blind, "
                        "and the comment stripper every other check depends on is now unverified"
                        % len(where))
    return problems


@functools.lru_cache(maxsize=None)
def source_text(path):
    """`path`'s text, read once per run.

    Every check here is a pure reader of a tree nothing writes while it runs, and a full run
    otherwise reads the same files a hundred times over (1.4 GB of `read_text` for a 12 MB tree).
    Cached by path object, so a caller passing a `Path` built two different ways still shares the
    entry as long as both are absolute -- which every caller here does, since they all come from
    the `SRC`/`TEST`/`TOOLS` constants above or a `glob` of one.
    """
    # errors="replace" matches the more forgiving of the two spellings this file used before the
    # readers were shared. Nothing in the tree needs it today (every source file is strict UTF-8),
    # so it only decides what a future mojibake commit does: report a findable oddity on one line
    # rather than take the whole lint stage down with a UnicodeDecodeError.
    return path.read_text(encoding="utf-8", errors="replace")


@functools.lru_cache(maxsize=None)
def source_lines(path):
    """`path`'s lines, split once per run. A tuple, because callers share the one list."""
    return tuple(source_text(path).split("\n"))


@functools.lru_cache(maxsize=None)
def stripped_lines(path):
    """`path`'s lines with every Fortran comment removed. Index-for-index with `source_lines`."""
    return tuple(strip_comment(ln) for ln in source_lines(path))


@functools.lru_cache(maxsize=None)
def stripped_text(path):
    """`path`'s text with every Fortran comment removed, derived once per run.

    The comment-free form of a file is what most checks here actually match against, and deriving
    it is the single most expensive thing this file does. `strip_comment` never joins or drops a
    line, so line numbers are preserved and a match's line index means the same here as in
    `source_text`.
    """
    return "\n".join(stripped_lines(path))


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
    """`parquet_table`'s own body must hold only scalars and the pointer.

    `.claude/rules/columns-tables.md`, "New `parquet_table` state goes on the CACHE".
    """
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
                "rule; see `.claude/rules/columns-tables.md`, \"New `parquet_table` state goes on the CACHE\"):\n    %s"
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
    """A returned pointer must target cache-owned heap, never `self`.

    `.claude/rules/columns-tables.md`, "`parquet_table` pointers and row-structural mutation".
    """
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
                "on their table and nothing diagnoses its absence (.claude/rules/columns-tables.md, "
                "\"`parquet_table` pointers and row-structural mutation\"):\n    %s"
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
    """A template omission is invisible to the generators' --check modes.

    `.claude/rules/code-style.md`, "Generated files: edit the generator, never the output".
    """
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
                        "generator's template, not here "
                        "(.claude/rules/code-style.md, \"Generated files\"):\n    %s"
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
                        "(.claude/rules/code-style.md, \"Generated files\"):\n    %s"
                        % (path.relative_to(REPO_ROOT), i + 1, stripped[:100])
                    )
    return problems


#: The two MAML generators, and the markers delimiting the region they must carry identically.
#: The consumer-facing one is COPIED into downstream projects, so it cannot source a shared file;
#: the region is duplicated on purpose and this check is what keeps the duplicate honest.
MAML_GENERATORS = (
    TOOLS / "generate_parquet_maml_base.sh",
    TOOLS / "generate_parquet_maml.sh",
)
MAML_EMITTER_BEGIN = "# ---- shared emitter (begin)"
MAML_EMITTER_END = "# ---- shared emitter (end)"


def shared_emitter_region(path):
    """Return (lines, first_line_number) for `path`'s shared-emitter region, or (None, message)."""
    lines = path.read_text().split("\n")
    starts = [i for i, line in enumerate(lines) if line.startswith(MAML_EMITTER_BEGIN)]
    ends = [i for i, line in enumerate(lines) if line.startswith(MAML_EMITTER_END)]
    if len(starts) != 1 or len(ends) != 1:
        return None, ("%s: expected exactly one `%s` line and one `%s` line, found %d and %d"
                      % (path.relative_to(REPO_ROOT), MAML_EMITTER_BEGIN, MAML_EMITTER_END,
                         len(starts), len(ends)))
    if ends[0] <= starts[0]:
        return None, ("%s:%d: the shared-emitter `(end)` marker is above its `(begin)`"
                      % (path.relative_to(REPO_ROOT), ends[0] + 1))
    return lines[starts[0]:ends[0] + 1], starts[0] + 1


def check_maml_generators_share_their_emitter():
    """The two MAML generators carry one emitter, byte for byte.

    tools/generate_parquet_maml.sh is consumer-facing: a downstream project copies that one file
    into its own tools/ and runs it there, so it cannot `source` anything from this repository and
    the emitter it shares with tools/generate_parquet_maml_base.sh has to be a copy. A fix made to
    one copy and not the other is invisible -- the base generator's `--check` only sees its own
    output, and nothing in this repository regenerates a downstream module. So compare the two
    regions here and fail on any difference, including whitespace.
    """
    problems = []
    regions = []
    for path in MAML_GENERATORS:
        if not path.exists():
            problems.append("%s: MAML generator not found" % path.relative_to(REPO_ROOT))
            continue
        region, extra = shared_emitter_region(path)
        if region is None:
            problems.append(extra)
            continue
        regions.append((path, region, extra))
    if problems or len(regions) != len(MAML_GENERATORS):
        return problems
    (first_path, first, first_at), (second_path, second, second_at) = regions
    if first == second:
        return problems
    diff = list(difflib.unified_diff(
        first, second, lineterm="",
        fromfile="%s:%d" % (first_path.relative_to(REPO_ROOT), first_at),
        tofile="%s:%d" % (second_path.relative_to(REPO_ROOT), second_at)))
    problems.append(
        "%s:%d: the shared emitter differs from %s -- the consumer-facing generator is a copy "
        "this repository cannot regenerate, so both files must carry this region identically. "
        "Apply the change to both:\n    %s"
        % (first_path.relative_to(REPO_ROOT), first_at,
           second_path.relative_to(REPO_ROOT), "\n    ".join(diff[:40]))
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
    """The generated schema must never carry a MEASURED size.

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
                            "size computed here is one the generator can get wrong:\n    %s"
                            % (path.relative_to(REPO_ROOT), start + offset + 1, keyword,
                               code.strip())
                        )
    return problems


def check_row_group_reads_guard_against_sort():
    """A row-group-scoped read must refuse while a sort is active.

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
                "the right row count and the wrong rows -- add `call check_reader_no_sort` beside "
                "the row-group check (.claude/rules/reader-writer.md, \"Row transforms\")"
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


#: Arrow type IDs that `arrow_leaf_family` maps but `doc/pages/io/reading.md`'s physical-type table
#: deliberately leaves out, each with the reason the page gives instead of a row.
#:
#: STRING_VIEW is the only member and is expected to stay the only one. It is absent from
#: `arrow_leaf_family` on purpose -- the query answers "unknown" for it -- while every read path
#: DOES read such a column, so the page documents it in prose ("One readable type answers
#: `"unknown"` here") rather than as a mapping row. A row would be wrong; silence would be worse.
#: `scenario_string_view_roundtrip` (test/error_scenarios.f90) asserts that answer from the other
#: side. CLAUDE.md's "a static check that enumerates names goes stale silently" applies to this
#: tuple: a second exemption is added here and to the page's prose, or to neither.
COLUMN_TYPE_TABLE_EXEMPT = ("STRING_VIEW",)

#: The mirror image: rows the page carries that `arrow_leaf_family` does not map, because the
#: behaviour they describe is produced earlier in the read path.
#:
#: The two dictionary rows are the members. `decode_dictionary_chunks` (parquet_wrapper.cpp) unwraps
#: a DICTIONARY column to its value type at `combine_column_chunks`, so `arrow_leaf_family` is only
#: ever handed the value type and has no DICTIONARY arm to find -- while a caller very much needs
#: the page to say what a pandas `category` reports. Same standing rule as the tuple above: a third
#: row is added here with its reason, or the table gains no row at all.
COLUMN_TYPE_TABLE_EXTRA_ROWS = ("DICTIONARY<STRING>", "DICTIONARY<LARGE_STRING>")


def check_column_type_mapping_documented():
    """`parquet_get_column_type`'s physical-type mapping and its documentation must agree.

    `arrow_leaf_family` (src/parquet_wrapper.cpp) is the single source of truth for what Fortran
    kind a column reads into, and `doc/pages/io/reading.md` reproduces its whole switch as a
    "stored physical type -> reported as" table. That table is a hand-written enumeration of a
    machine-readable list, which is exactly the shape `.claude/rules/documentation.md` says needs a
    check rather than careful review -- and the page next door already has one
    (`check_print_stat_columns_documented`, above), for the same reason.

    Compares the two SETS of Arrow type IDs. The Fortran-side family names are not compared: the
    page spells them as the tokens `parquet_get_column_type` prints (`int32`, `boolean`, ...) while
    the C++ spells them `kElemFamilyInt32`, and mapping one to the other here would mean a second
    copy of the vocabulary -- the thing this check exists to prevent. What it catches is a physical
    type gained or lost, which is the drift that actually happens when Arrow adds a type.

    An intentional omission goes in COLUMN_TYPE_TABLE_EXEMPT above, never by deleting a row here.
    """
    problems = []
    cpp = SRC / "parquet_wrapper.cpp"
    doc = REPO_ROOT / "doc" / "pages" / "io" / "reading.md"
    text = cpp.read_text()
    body = cpp_function_body(text, "arrow_leaf_family")
    if body is None:
        return ["%s: could not find arrow_leaf_family's body -- this check needs updating"
                % cpp.relative_to(REPO_ROOT)]
    code_types = set(re.findall(r"case\s+arrow::Type::(\w+)\s*:", body))
    if not code_types:
        return ["%s: arrow_leaf_family matched no `case arrow::Type::X:` arms at all -- the switch "
                "was rewritten and this check has gone blind" % cpp.relative_to(REPO_ROOT)]

    # The documented set: the first cell of every row of the page's physical-type table, anchored on
    # its own `| stored physical type | reported as |` header (the same anchoring lesson as
    # check_print_stat_columns_documented: the row shape alone is not distinctive on this page).
    # A cell lists one or more backticked type names, comma-separated, and may carry a trailing
    # em-dash aside; the final `anything else` row carries no backticks and is skipped by that.
    lines = doc.read_text().split("\n")
    start = None
    for i, line in enumerate(lines):
        if re.match(r"^\s*\|\s*stored physical type\s*\|\s*reported as\s*\|\s*$", line):
            start = i + 1
            break
    if start is None:
        return ["doc/pages/io/reading.md: could not find the `| stored physical type | reported as |` "
                "table header -- the documentation moved and this check has gone blind on it"]
    documented = set()
    for line in lines[start:]:
        if not line.strip().startswith("|"):
            break
        cell = line.split("|")
        if len(cell) > 1:
            # Only the type names, which lead the cell as a comma-separated backticked list. An
            # em-dash aside may follow them ("-- what pandas writes for a `category`"), and its own
            # backticked words are prose, not rows: reading them made this check report `category`
            # as a stale row the first time it ran.
            names = cell[1].split("\u2014")[0]
            documented.update(name.upper() for name in re.findall(r"`([A-Za-z0-9_<>]+)`", names))

    exempt = set(COLUMN_TYPE_TABLE_EXEMPT)
    extra_rows = set(COLUMN_TYPE_TABLE_EXTRA_ROWS)
    for type_id in sorted(code_types - documented - exempt):
        problems.append(
            "doc/pages/io/reading.md: arrow_leaf_family maps `arrow::Type::%s` but the physical-type "
            "table has no row for it -- add one, or exempt it in COLUMN_TYPE_TABLE_EXEMPT with the "
            "reason the page gives instead" % type_id)
    for type_id in sorted(documented - code_types - exempt - extra_rows):
        problems.append(
            "doc/pages/io/reading.md: the physical-type table has a row for `%s`, which "
            "arrow_leaf_family does not map -- it was renamed or removed" % type_id.lower())
    for type_id in sorted(exempt & code_types):
        problems.append(
            "tools/check_source_conventions.py: `arrow::Type::%s` is in COLUMN_TYPE_TABLE_EXEMPT but "
            "arrow_leaf_family now maps it -- give it a table row and drop the exemption" % type_id)
    for type_id in sorted(extra_rows - documented):
        problems.append(
            "tools/check_source_conventions.py: `%s` is in COLUMN_TYPE_TABLE_EXTRA_ROWS but the "
            "physical-type table no longer has a row for it -- drop the entry" % type_id.lower())
    return problems


def _printed_settings_rows():
    """Every row name `parquet_print_settings` emits, in printed order; `[]` if the shape moved.

    Matched by SHAPE (`call print_<anything>(u, "name"`), not against a list of helper names. An
    earlier version named them, and silently went blind twice: once when print_text arrived for the
    non-integer rows, and again when print_big arrived for the int64 ones -- on that occasion it
    reported two of five new rows as undocumented and passed the other three, which is worse than
    failing outright. A new row helper is picked up with no edit here.

    One home for the list, because two checks need it (`.claude/rules/testing.md`, "Two checks
    needing the same list derive it from one place"): `check_print_settings_documented` compares it
    with the sample dump, and `check_module_settings_reexports_documented` uses the knob half of it
    to resolve "all of them" on the entry-module settings table. Callers that want knobs only drop
    the `parquet_max_*` rows, which are the read-only limits rather than settings.
    """
    return re.findall(r'call\s+print_\w+\s*\(\s*u\s*,\s*"([^"]+)"',
                      (SRC / "parquet_settings.f90").read_text())


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
    printed = _printed_settings_rows()
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
    """A setting that is stored but never read passes every test.

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
                "does nothing, and a set/get test would not notice -- add its read site, or remove the "
                "knob (.claude/rules/api-conventions.md, \"Settings\")" % name)
    return problems


#: Fortran procedures allowed to decide for themselves where a line goes. Re-derived from the
#: source each time this list is edited -- and asserted live below, so an entry that stops matching
#: anything is a failure rather than a silent widening of the rule.
#:
#: The four emit channels and `parquet_message_unit` are the mechanism itself. The solicited
#: printers resolve through `parquet_message_unit` and then write many lines to a caller-chosen
#: unit, asking `parquet_output_is_suppressed` rather than routing each line through a channel.
#: `parquet_logging` is a separate system with a separate audience: it carries the USER's program's
#: output, not the library's, so it must NOT route through the emit channels -- `verbosity` and
#: `message_stream` govern what this library says, not what its caller says. src/parquet_toml.f90
#: emits through THAT system rather than through the channels, deliberately: configuration
#: diagnostics are read by the operator of the calling program, in that program's log. It needs no
#: entry here because it names no unit -- every line it produces goes through
#: pf_log_error/pf_log_warning/pf_log_fatal.
DIRECT_PRINT_ALLOWED = {
    # ---- The mechanism: the four channels, and the one resolver they and every printer share ----
    "parquet_emit_info", "parquet_emit_advice", "parquet_emit_warning",
    "parquet_emit_error_context", "parquet_message_unit",
    # ---- The solicited printers, one entry per procedure that actually writes ----
    # `display_rows` (%print_rows) was invisible to the pre-S6 pattern, which matched only a
    # literal `output_unit`/`error_unit`/`*` unit: it takes its unit from `resolve_unit`, so no
    # stream is named anywhere in it.
    "parquet_print_settings", "print_one", "print_text", "print_big",
    "table_print_stat", "display_rows", "schema_print_schema_info", "col_print", "psv_print",
    "obj_print", "kde_print", "grid_print",
    # ---- parquet_logging: the separate system described above ----
    # `machinery_warning` reports a failure of the logging machinery ITSELF, which is why it
    # bypasses sinks, layout and the output critical section alike and writes straight to
    # error_unit. The rest are the console sink: it is defined by the two standard streams, so
    # naming them is what the code is for.
    "machinery_warning", "default_console_sink", "is_console_unit", "logger_add_console",
    "logger_print", "pf_log_print", "deliver",
}

#: C++ functions in src/parquet_wrapper.cpp allowed to write to a stream directly. Same rule, same
#: liveness assertion, the other side of the `bind(C)` boundary. The requirement -- any new C++-side
#: print needs either the shared `emit_warning_cpp` helper or the `output_is_suppressed()` query --
#: is the C++ half of `.claude/rules/api-conventions.md`, "Errors and diagnostics", and this is
#: what enforces it.
#:
#: `parquet_reader_print_stat` is on the list even though it resolves through `message_stream_file`,
#: for the same reason the Fortran printers are: resolving the STREAM correctly is not the same as
#: honouring `verbosity`, and a function that writes its own lines has to ask about that itself.
CPP_DIRECT_PRINT_ALLOWED = {
    "emit_warning_cpp",           # the one place a C++-side warning is printed
    "report_fatal_error",         # the abort path, always stderr, by design
    "ConcurrencyGuard",           # its constructor's message, on the same always-stderr path
    "parquet_reader_print_stat",  # the solicited reader report
}


#: Opens a Fortran procedure. Shared by the printing and context checks, and deliberately NOT the
#: pattern this file used before S6: that one's third alternative was anchored on `::`, so it read
#: a `character(len=12), parameter :: x(9) =` DECLARATION as a procedure opening -- and matched no
#: typed function at all, since a function statement carries no `::`. Both errors misattribute
#: every site below them to a name that is not a procedure, which is how an allow-list acquires
#: entries nobody can find again.
FORTRAN_PROC_START = re.compile(
    r"^\s*(?:module\s+procedure\s+(\w+)"
    r"|(?:(?:recursive|pure|impure|elemental|module)\s+)*"
    r"(?:(?:integer|logical|real|character|type|class)\s*(?:\([^)]*\))?\s+)?"
    r"(?:subroutine|function)\s+(\w+))")


def _fortran_procedures(path):
    """Yield (proc_name, [(lineno, code), ...]) for `path`, comments already stripped."""
    out, current, body = [], "", []
    for lineno, raw in enumerate(path.read_text().split("\n"), start=1):
        m = FORTRAN_PROC_START.match(raw)
        if m and any(m.groups()):
            out.append((current, body))
            current, body = next(g for g in m.groups() if g), []
        body.append((lineno, strip_comment(raw)))
    out.append((current, body))
    return out


#: A `print` in any of its forms: `print '(a)'`, `print *,`, `print fmt,`, `print "(a)"`.
_PRINT_STMT = re.compile(r"^\s*print\s*(?:[\'\"(*]|\w)")
#: `write(*, ...)` and `write(unit=*, ...)` -- the process's standard output, whatever is set.
_WRITE_STAR = re.compile(r"^\s*write\s*\(\s*(?:unit\s*=\s*)?\*\s*[,)]", re.I)
#: A `write` whose unit is a stream named outright, or the shared resolver called inline.
_WRITE_RESOLVED = re.compile(
    r"^\s*write\s*\(\s*(?:unit\s*=\s*)?(?:output_unit|error_unit|parquet_message_unit\s*\()", re.I)
#: A `write` to a variable. Whether that is a unit or a character buffer is decided per procedure.
_WRITE_VAR = re.compile(r"^\s*write\s*\(\s*(?:unit\s*=\s*)?([A-Za-z]\w*)\s*[,)]", re.I)
#: `integer[(kind)][, attrs] :: a, b(3), c = 0` -- the names, whatever follows them.
_INTEGER_DECL = re.compile(r"^\s*integer\b[^:]*::\s*(.+)$", re.I)
#: A unit this procedure opened itself: a FILE, not one of the two streams.
_OPEN_NEWUNIT = re.compile(r"^\s*open\s*\([^)]*\bnewunit\s*=\s*(\w+)", re.I)
#: A local taking its value from a stream or from the shared resolver.
_UNIT_ASSIGN = re.compile(
    r"^\s*(\w+)\s*=\s*(?:output_unit|error_unit|parquet_message_unit\s*\()", re.I)
#: Any other mention of a stream by name: `u = output_unit`, `unit == error_unit`, a default. A
#: `use` line is not one -- importing the name decides nothing about where a line goes.
_NAMES_STREAM = re.compile(r"\b(?:output_unit|error_unit)\b", re.I)
_USE_STMT = re.compile(r"^\s*use\b", re.I)


def _declared_integers(body):
    """The names declared `integer` anywhere in one procedure body."""
    names = set()
    for _, code in body:
        m = _INTEGER_DECL.match(code)
        if not m:
            continue
        for token in m.group(1).split(","):
            token = token.split("(")[0].split("=")[0].strip()
            if re.fullmatch(r"\w+", token):
                names.add(token)
    return names


def _fortran_direct_print_sites():
    """Every site in src/ that decides for itself where a line goes, as (path, lineno, proc, code).

    Five shapes, and each is a way of resolving a destination rather than being handed one:

    * any `print` statement -- always the process's standard output;
    * `write(*, ...)` -- the same;
    * a `write` naming `output_unit`/`error_unit`, or calling `parquet_message_unit` inline;
    * a `write` to a local declared `integer` -- the printers' own shape, `u = <resolver>` followed
      by many `write (u, ...)` lines. A unit the procedure opened itself with `newunit=` does not
      count, unless the same variable ALSO takes a stream somewhere (schema_print_schema_info, which
      writes either to a file it opened or to the message unit, through one variable);
    * any other mention of a stream by name -- `u = output_unit`, `unit == error_unit`, a sink's
      default. This is the one that catches "resolved its own destination and then wrote through a
      helper", which no pattern over `write` statements can see.

    A `write` to a unit the procedure was GIVEN is deliberately not a shape here: whoever passed it
    resolved it, and that caller is checked. The unit of enforcement is the resolution, not the I/O.
    """
    sites = []
    for path in sorted(SRC.glob("*.f90")):
        for proc, body in _fortran_procedures(path):
            ints = _declared_integers(body)
            opened, from_stream = set(), set()
            for _, code in body:
                m = _OPEN_NEWUNIT.match(code)
                if m:
                    opened.add(m.group(1))
                m = _UNIT_ASSIGN.match(code)
                if m:
                    from_stream.add(m.group(1))
            for lineno, code in body:
                if _PRINT_STMT.match(code) or _WRITE_STAR.match(code) or _WRITE_RESOLVED.match(code):
                    sites.append((path, lineno, proc, code))
                    continue
                m = _WRITE_VAR.match(code)
                if m and m.group(1) in ints and (m.group(1) not in opened
                                                 or m.group(1) in from_stream):
                    sites.append((path, lineno, proc, code))
                    continue
                if _NAMES_STREAM.search(code) and not _USE_STMT.match(code):
                    sites.append((path, lineno, proc, code))
    return sites


def check_no_direct_printing():
    """A message printed directly ignores both output settings, and nothing else would notice.

    Every message the library emits goes through one of the four channels in parquet_settings_base,
    and every solicited printer resolves its destination through `parquet_message_unit`. That is
    what makes `verbosity` and `message_stream` apply everywhere without each call site testing
    them. A new warning written the old way -- `print '(a)', "WARNING: ..."` -- still appears at
    default settings, so the test suite stays green; it is only wrong for the users who changed a
    setting, and only in a way nobody runs into until they do.

    That makes it a static-check problem rather than a test problem: the property is "no site does
    this", which a test cannot express.

    **The allow-list is asserted live.** Every name in it must still match a flagged site, or this
    reports the stale entry. Without that, the list only ever grows: it reached nine names of which
    SEVEN had stopped matching anything, which meant seven procedures could have started printing
    directly with nothing to say so. That is the failure this half exists to prevent, and it is the
    same mechanism `CONTEXT_SUFFIX_EXEMPT` uses one check along.
    """
    problems, matched = [], set()
    for path, lineno, proc, code in _fortran_direct_print_sites():
        if proc in DIRECT_PRINT_ALLOWED:
            matched.add(proc)
            continue
        problems.append(
            "%s:%d: `%s` decides for itself where a line goes -- route it through "
            "parquet_emit_info/_advice/_warning/_error_context, or take the destination from "
            "parquet_message_unit(), or neither output setting will apply to it:\n    %s"
            % (path.relative_to(REPO_ROOT), lineno, proc or "<file scope>", code.strip()))
    if not matched:
        return ["src/: this check flagged nothing at all -- the emit channels and the printers "
                "should every one of them match, so the patterns have gone blind"]
    for stale in sorted(DIRECT_PRINT_ALLOWED - matched):
        problems.append(
            "tools/check_source_conventions.py: DIRECT_PRINT_ALLOWED names `%s`, which no longer "
            "matches a site this check would flag -- the procedure was renamed, removed, or has "
            "since been routed through a channel. Delete the entry: an allow-list nobody "
            "re-derives is how this check went half blind before (feature_message_stream.md C6)."
            % stale)
    return problems


#: Opens a C++ function definition. Anchored at one tab of indentation at most, so a call
#: continued over several lines -- `describe_parquet_type(field),` inside an argument list -- is
#: not read as a definition and does not misattribute every site below it.
CPP_FUNC_START = re.compile(
    r"^(?P<indent>\t{0,1})(?:\[\[noreturn\]\]\s*)?"
    r"(?:(?:static|inline|constexpr|extern\s+\"C\"|virtual)\s+)*"
    r"(?:[A-Za-z_][\w:<>,*&\s]*?[\s*&]+)?(?P<name>[A-Za-z_]\w*)\s*\([^;]*$")
#: Writing to a stream. `snprintf` writes to a buffer and is not one, which is why every pattern
#: here is anchored on a word boundary.
CPP_PRINTS = re.compile(
    r"\bstd::(?:fprintf|printf|puts|fputs|cout|cerr|clog)\b"
    r"|(?<!\w)(?<!s)(?:fprintf|printf|puts|fputs)\s*\(")
_CPP_KEYWORDS = frozenset(("if", "for", "while", "switch", "return", "catch", "else", "do",
                           "sizeof", "throw"))


def check_no_direct_printing_cpp():
    """The same rule on the other side of the `bind(C)` boundary, which the Fortran arm cannot see.

    src/parquet_wrapper.cpp prints in its own right -- three warnings, the fatal-error report, the
    concurrency guard's message and the whole reader report -- and every one of those has to honour
    the two settings mirrored across the boundary
    (`.claude/rules/cpp-wrapper.md`, "Mirrored settings";
    `.claude/rules/api-conventions.md`, "Errors and diagnostics"). Before this check nothing
    enforced it, and the file had accumulated ten `std::fprintf(stdout, ...)` calls in one function
    that ignored `message_stream` completely.

    Same liveness rule as the Fortran arm: an allow-list entry that stops matching is a failure.
    """
    path = SRC / "parquet_wrapper.cpp"
    if not path.exists():
        return ["src/parquet_wrapper.cpp is missing -- this check needs updating"]
    problems, matched, current = [], set(), ""
    for lineno, raw in enumerate(path.read_text().split("\n"), start=1):
        code = raw.split("//")[0]
        m = CPP_FUNC_START.match(code)
        if m and m.group("name") not in _CPP_KEYWORDS:
            current = m.group("name")
        if not CPP_PRINTS.search(code):
            continue
        if current in CPP_DIRECT_PRINT_ALLOWED:
            matched.add(current)
            continue
        problems.append(
            "src/parquet_wrapper.cpp:%d: `%s` writes to a stream directly -- route it through "
            "emit_warning_cpp, or take the stream from message_stream_file() and ask "
            "output_is_suppressed(), or neither mirrored output setting will apply to it:\n    %s"
            % (lineno, current or "<file scope>", code.strip()))
    if not matched:
        return ["src/parquet_wrapper.cpp: this check flagged nothing at all -- emit_warning_cpp "
                "and report_fatal_error should both match, so the patterns have gone blind"]
    for stale in sorted(CPP_DIRECT_PRINT_ALLOWED - matched):
        problems.append(
            "tools/check_source_conventions.py: CPP_DIRECT_PRINT_ALLOWED names `%s`, which no "
            "longer matches a site this check would flag -- the function was renamed, removed, or "
            "has since been routed through emit_warning_cpp. Delete the entry." % stale)
    return problems


#: The four constants that make up the row-group sizing arithmetic in src/parquet_wrapper.cpp. Each
#: may be DEFINED exactly once, however many times it is read.
ROW_GROUP_SIZING_CONSTANTS = (
    "kTargetRowGroupBytes", "kMinAutoChunkSizeRows", "kMaxAutoChunkSizeRows",
    "kMaxFloorOvershootFactor",
)


def check_row_group_sizing_not_duplicated():
    """A second copy of the sizing arithmetic ignores the setting.

    Row groups are sized from a byte target by chunk_size_from_bytes_per_row, and it has two callers
    that serve DIFFERENT writers: close_parquet_writer's whole-table write, and
    estimate_chunk_size_from_schema's estimate for the streaming parquet_new_row_group path. Until
    S4 the first of those restated the whole function body inline, four constants included.

    That is a silent failure rather than a visible one. A re-inlined copy takes the built-in
    constant instead of g_target_row_group_bytes, so parquet_set_target_row_group_bytes governs one
    kind of write and not the other: the files come out with the wrong number of row groups, every
    row is present and correct, and nothing fails.

    The rule is therefore about DEFINITIONS, not uses -- reading a constant in ten places is fine,
    declaring it in two is the bug. The two callers are pinned by `target_row_group_bytes sizes the
    row groups of a whole-table write` and `... also sizes the streaming path's estimate`
    (test/test_settings.f90); a third caller needs its own test, since this check counts definitions.
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
                "-- delete the copy and call chunk_size_from_bytes_per_row" % (name, len(hits)))
        elif not hits:
            problems.append(
                "src/parquet_wrapper.cpp: `%s` is not defined at all -- if the row-group sizing "
                "constants were renamed, update ROW_GROUP_SIZING_CONSTANTS in this script" % name)
    return problems


def check_env_covers_every_setting():
    """A knob with no environment variable is silently unreachable.

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
                "tell apart from the setting being broken; add it (.claude/rules/api-conventions.md, "
                "\"Settings\")" % (knob, var))
    return problems


def _quoted_args_of_calls(text, name):
    """Every quoted argument of every `name(...)` call in `text`, across nested parentheses.

    A plain `name\\s*\\([^)]*?"([^"]+)"` regex is the obvious way to do this and is WRONG in the
    direction that keeps a check green: `[^)]` stops at the first `)`, so a call whose earlier
    argument is itself a call -- `parquet_auto_thread_count(parquet_get_sort_threads(), "sorting")`
    -- never matches at all. Written that way this check found five of the seven area names and
    passed, missing `sorting` and `random draws`, the two most likely to appear in a warning. Scan
    with a depth counter instead.
    """
    found = set()
    pat = re.compile(r"\b" + re.escape(name) + r"\s*\(")
    for m in pat.finditer(text):
        depth, i, n = 1, m.end(), len(text)
        while i < n and depth > 0:
            c = text[i]
            if c == "(":
                depth += 1
            elif c == ")":
                depth -= 1
            elif c == "!" and depth == 1:
                break
            i += 1
        found |= set(re.findall(r'"([^"]+)"', text[m.end():i]))
    return found


def check_affinity_areas_documented():
    """Every subsystem the affinity clamp can name must appear on the performance page.

    `parquet_clamp_to_affinity(n, area)` lowers a resolved thread count to what this process's CPU
    affinity allows and warns once per process when it bites, and the FIRST WORD of that warning is
    the `area` string its caller passed. `doc/pages/operating/performance.md` enumerates those
    strings so a reader who sees the warning can tell which subsystem noticed it.

    That enumeration is exactly the shape CLAUDE.md warns about -- a list written in one place
    describing a set owned by another -- and it went stale in the direction that keeps passing: the
    page named four areas while the source passed seven, and every check in this repository stayed
    green, because nothing compared the two. The clamp's own doc-comment already says the call sites
    are the authoritative list and that an enumeration written elsewhere has gone stale before.

    Matching by SHAPE rather than from a list here, so a subsystem added later is picked up with no
    edit to this check. An empty match is a failure, not a pass: it means the call shape moved and
    this check has gone blind.

    **Both directions.** The reverse half was added after a review found there was only one: a
    subsystem DELETED from the source would have sat on the page forever, promising a reader a
    warning that can no longer fire -- the same silent staleness the forward half exists to stop,
    pointing the other way. It is anchored on the page's ENUMERATION SENTENCE rather than on any
    backticked word, because the page names `sorting` and `index` in ordinary prose several times
    over and a whole-page reverse scan would report every one of those. A sentence that no longer
    matches is a FAILURE, not a pass: this check has then gone half blind and must be re-anchored
    deliberately -- as it was when the clamp's message moved to the advice channel and gained a
    `NOTE: ` prefix, so that the area name stopped being the line's first word.
    """
    problems = []
    areas = set()
    for path in sorted(SRC.glob("*.f90")):
        for name in ("parquet_clamp_to_affinity", "parquet_auto_thread_count"):
            areas |= _quoted_args_of_calls(path.read_text(), name)
    if not areas:
        return ["src/: no parquet_clamp_to_affinity/parquet_auto_thread_count call passing a "
                "quoted area was found -- the call shape has moved and this check has gone blind"]
    page = REPO_ROOT / "doc" / "pages" / "operating" / "performance.md"
    text = page.read_text()
    for area in sorted(areas):
        if "`%s`" % area not in text:
            problems.append(
                "doc/pages/operating/performance.md: the affinity clamp can report the subsystem "
                "`%s`, but the page never names it -- a reader who sees that warning has no way to "
                "tell which subsystem noticed. Add it to the list of area names (re-derive the set "
                "with `grep -rn parquet_clamp_to_affinity src/`)." % area)
    listed = re.search(r"names whichever subsystem noticed[^.]*\.", text, re.S)
    if listed is None:
        return problems + [
            "doc/pages/operating/performance.md: could not find the sentence containing \"names "
            "whichever subsystem noticed\", which is where the area names are "
            "enumerated -- the reverse half of this check cannot tell that list from the page's "
            "ordinary prose any more; re-anchor it deliberately"]
    for name in sorted(set(re.findall(r"`([a-z][a-z ]*)`", listed.group(0)))):
        if name not in areas:
            problems.append(
                "doc/pages/operating/performance.md: the area list names `%s`, which no "
                "parquet_clamp_to_affinity/parquet_auto_thread_count call passes -- the subsystem "
                "was renamed or removed, so the page promises a warning that cannot fire." % name)
    return problems


def check_set_threads_fanout_documented():
    """`parquet_set_threads`' fan-out and the count on the settings page must agree.

    `parquet_set_threads(n)` is the "give this library N threads and no more" convenience: its body
    calls `parquet_set_arrow_threads` and every per-area setter in turn. Its own doc-comment refuses
    to write the number down -- "It NAMES them rather than counting them, so the next one needs one
    line here and nowhere else" -- and then names the one place that does carry a count:

        The environment table on the settings guide page does carry a count, and that one has to
        move.

    Nothing made it move. The `PARQUET_FORTRAN_THREADS` row reads "sets the nine below at once", and
    a tenth cap would leave the sentence explaining one environment variable stating a wrong number
    about code a reader is about to write. That is the FALSE-making class, and it is the shape
    CLAUDE.md's "A static check that enumerates names goes stale silently" warns about with the
    roles reversed: here the source knew, said so beside the code, and nothing enforced it.

    Three quantities must agree, all three derived rather than stored:

      1. the `call parquet_set_*` count inside `parquet_set_threads`' body;
      2. the number spelled out in the `PARQUET_FORTRAN_THREADS` row of the guide's table;
      3. that each of those setters has its own row BELOW that one, which is what "below" means --
         the mapping being mechanical (`parquet_set_sort_threads` -> `PARQUET_FORTRAN_SORT_THREADS`).

    Clause 3 overlaps `check_env_covers_every_setting`, deliberately: a cap added to the body and
    nowhere else should fail with the fan-out in the message, not with a printed-row mismatch two
    checks away.

    **Clause 3 does not count rows by suffix, and a first version that did was wrong on the shipped
    tree**: `PARQUET_FORTRAN_DEFAULT_USE_THREADS` ends in `_THREADS` and is not a thread cap, so a
    generic `PARQUET_FORTRAN_*_THREADS` tally reads 10 against a fan-out of 9. The variables are
    derived from the setters instead, which is the only spelling of "the nine below" that means what
    the sentence means.

    **What it deliberately does NOT check**, so a later reviewer does not read the absence as an
    oversight: the page's three PROSE enumerations of the same body ("sorting, the table prefetch,
    the table rewrite, one string column's bulk work, ..."). Those are English phrases, not setter
    names, and a phrase-to-name table is the brittle shape that gets a check deleted rather than
    fixed. They stay a hand count.
    """
    problems = []
    src = SRC / "parquet_settings.f90"
    body = re.search(r"\n    subroutine parquet_set_threads\(n\)\n(.*?)\n    end subroutine "
                     r"parquet_set_threads\b", src.read_text(), re.S)
    if body is None:
        return ["src/parquet_settings.f90: could not find parquet_set_threads' body -- it was "
                "renamed or its layout changed, and this check has gone blind"]
    setters = re.findall(r"call\s+(parquet_set_\w+)\s*\(", body.group(1))
    if not setters:
        return ["src/parquet_settings.f90: parquet_set_threads calls no parquet_set_* setter -- "
                "the call shape moved and this check has gone blind"]
    doc = REPO_ROOT / "doc" / "pages" / "operating" / "settings.md"
    text = doc.read_text()
    row = re.search(r"\|\s*`PARQUET_FORTRAN_THREADS`\s*\|([^|]*)\|", text)
    if row is None:
        return ["doc/pages/operating/settings.md: no `PARQUET_FORTRAN_THREADS` table row found -- "
                "the environment table moved or its shape changed, and this check needs updating"]
    # Spelled out rather than digits, which is the guide's house style for a small count in prose.
    # The map stops at twenty: a library with twenty-one per-area thread caps has a bigger problem
    # than this check, and an unspellable number fails loudly below rather than passing.
    words = {"one": 1, "two": 2, "three": 3, "four": 4, "five": 5, "six": 6, "seven": 7,
             "eight": 8, "nine": 9, "ten": 10, "eleven": 11, "twelve": 12, "thirteen": 13,
             "fourteen": 14, "fifteen": 15, "sixteen": 16, "seventeen": 17, "eighteen": 18,
             "nineteen": 19, "twenty": 20}
    spelled = re.search(r"sets the (\w+) below at once", row.group(1))
    if spelled is None or spelled.group(1) not in words:
        return ["doc/pages/operating/settings.md: the `PARQUET_FORTRAN_THREADS` row no longer says "
                "\"sets the <number> below at once\" with the number spelled out (it reads `%s`) -- "
                "either reword it back or re-anchor this check deliberately; it cannot see the "
                "count any more" % row.group(1).strip()]
    stated = words[spelled.group(1)]
    below = re.findall(r"\|\s*`(PARQUET_FORTRAN_\w+)`\s*\|", text[row.end():])
    if stated != len(setters):
        problems.append(
            "doc/pages/operating/settings.md: the `PARQUET_FORTRAN_THREADS` row says it sets the "
            "%s below at once, but parquet_set_threads calls %d setters (%s). Correct the row -- "
            "its own doc-comment says this count has to move when a cap is added."
            % (spelled.group(1), len(setters), ", ".join(setters)))
    if not below:
        return problems + [
            "doc/pages/operating/settings.md: no `PARQUET_FORTRAN_*` row follows the "
            "`PARQUET_FORTRAN_THREADS` row -- the table was reordered and this check can no longer "
            "tell what \"below\" means; re-anchor it deliberately"]
    for setter in setters:
        var = "PARQUET_FORTRAN_" + setter[len("parquet_set_"):].upper()
        if var not in below:
            problems.append(
                "doc/pages/operating/settings.md: parquet_set_threads calls `%s`, so `%s` should be "
                "one of the rows below `PARQUET_FORTRAN_THREADS`, and it is not." % (setter, var))
    return problems


def check_module_settings_reexports_documented():
    """`choosing-a-module.md`'s "Settings it re-exports" table must match what each module exports.

    `.claude/rules/module-structure.md`: "A module re-exports, getter and setter, every settings
    knob its own code reads (including `verbosity`/`message_stream` when it can emit)". The guide
    turns that rule into a per-module table, so a program that imports one module for one capability
    can see, without reading source, whether it can configure that capability from the same import.
    `test/test_module_surface.f90` pins the SOURCE side of the rule. Nothing looked at the table.

    **Class, stated because the two halves differ** (the U3/U4 distinction): the rows reading
    "none -- it reads none" are FALSE-making -- a module that gains a knob makes that row a lie
    about code a reader is about to write. The rows that list knobs are only incomplete-making,
    which is the class this repository has declined before. Both are checked, because the derivation
    is identical and a one-directional check here is exactly the blind spot that was just fixed in
    `check_affinity_areas_documented`.

    **Nothing is spelled out here.** The knob vocabulary is `parquet_print_settings`' own rows
    (`_printed_settings_rows`), and each module's re-export set is its `public ::` lines. The two
    ENGLISH phrases the table uses are expanded from source as well, not from a literal list:

      * "the four sorting knobs" -> what `parquet_argsort` re-exports, less the output pair; the
        spelled count is checked against it, so a fifth sorting knob fails here too;
      * "the same six as `parquet_argsort`" -> that module's whole set, count likewise checked.

    A cell that matches none of the recognised shapes, or that names a backticked token which is not
    a knob, FAILS rather than being skipped: a silently unparsed row is a row nobody is checking.
    """
    problems = []
    knobs = set(k for k in _printed_settings_rows() if not k.startswith("parquet_max_"))
    if not knobs:
        return ["src/parquet_settings.f90: no printed settings rows found -- this check cannot "
                "build the knob vocabulary and has gone blind"]

    def reexports(module):
        path = SRC / (module + ".f90")
        if not path.is_file():
            return None
        names = set()
        for line in path.read_text().splitlines():
            stripped = line.strip()
            if not stripped.startswith("public ::"):
                continue
            for name in stripped[len("public ::"):].split(","):
                match = re.match(r"parquet_(?:set|get)_(\w+)$", name.strip())
                if match and match.group(1) in knobs:
                    names.add(match.group(1))
        return names

    page = REPO_ROOT / "doc" / "pages" / "operating" / "choosing-a-module.md"
    text = page.read_text()
    table = re.search(r"\| Import \| Settings it re-exports \|\n\|[-| ]+\|\n((?:\|.*\n)+)", text)
    if table is None:
        return ["doc/pages/operating/choosing-a-module.md: could not find the \"Settings it "
                "re-exports\" table -- it moved or its header changed, and this check has gone blind"]
    output_pair = {"verbosity", "message_stream"}
    argsort = reexports("parquet_argsort")
    if not argsort:
        return ["src/parquet_argsort.f90: no settings re-exports found -- the two English phrases "
                "on the page are expanded from this module, so this check has gone blind"]
    sorting_knobs = argsort - output_pair
    words = {"one": 1, "two": 2, "three": 3, "four": 4, "five": 5, "six": 6, "seven": 7,
             "eight": 8, "nine": 9, "ten": 10}
    rows = 0
    for line in table.group(1).splitlines():
        cell = [c.strip() for c in line.strip().strip("|").split("|")]
        if len(cell) != 2:
            continue
        module = re.fullmatch(r"`(parquet\w*)`", cell[0])
        if module is None:
            # The combined row names four modules at once ("`parquet_settings`, and so ...").
            # It is checked against parquet_settings, the module that owns them.
            if "parquet_settings" not in cell[0]:
                problems.append(
                    "doc/pages/operating/choosing-a-module.md: settings table row `%s` names no "
                    "single module and is not the combined `parquet_settings` row -- this check "
                    "cannot tell what to compare it against; re-anchor it deliberately" % cell[0])
                continue
            module_name = "parquet_settings"
        else:
            module_name = module.group(1)
        rows += 1
        have = reexports(module_name)
        if have is None:
            problems.append(
                "doc/pages/operating/choosing-a-module.md: the settings table has a row for `%s`, "
                "but src/%s.f90 does not exist" % (module_name, module_name))
            continue
        body = re.sub(r"\([^)]*\)", "", cell[1].split("—")[0])
        low = body.strip().lower()
        if "all of them" in low:
            want = set(knobs)
        elif low.startswith("none"):
            want = set()
        else:
            want = set()
            phrase = re.search(r"the (\w+) sorting knobs", body)
            if phrase:
                if words.get(phrase.group(1)) != len(sorting_knobs):
                    problems.append(
                        "doc/pages/operating/choosing-a-module.md: `%s`'s row says \"the %s sorting "
                        "knobs\"; `parquet_argsort` re-exports %d of them (%s)."
                        % (module_name, phrase.group(1), len(sorting_knobs),
                           ", ".join(sorted(sorting_knobs))))
                want |= sorting_knobs
                body = body[:phrase.start()] + body[phrase.end():]
            phrase = re.search(r"the same (\w+) as `(parquet\w*)`", body)
            if phrase:
                other = reexports(phrase.group(2))
                if not other:
                    problems.append(
                        "doc/pages/operating/choosing-a-module.md: `%s`'s row refers to `%s`, which "
                        "re-exports no knob this check can find"
                        % (module_name, phrase.group(2)))
                    continue
                if words.get(phrase.group(1)) != len(other):
                    problems.append(
                        "doc/pages/operating/choosing-a-module.md: `%s`'s row says \"the same %s as "
                        "`%s`\"; that module re-exports %d (%s)."
                        % (module_name, phrase.group(1), phrase.group(2), len(other),
                           ", ".join(sorted(other))))
                want |= other
                body = body[:phrase.start()] + body[phrase.end():]
            named = re.findall(r"`([a-z][a-z0-9_]*)`", body)
            stray = [n for n in named if n not in knobs]
            if stray:
                problems.append(
                    "doc/pages/operating/choosing-a-module.md: `%s`'s settings cell names `%s`, "
                    "which is not a row parquet_print_settings prints -- either the cell gained a "
                    "shape this check cannot read, or the knob was renamed. A cell it cannot parse "
                    "is a cell nobody is checking, so this fails rather than skipping."
                    % (module_name, "`, `".join(stray)))
                continue
            want |= set(named)
        for knob in sorted(want - have):
            problems.append(
                "doc/pages/operating/choosing-a-module.md: the table says `%s` re-exports `%s`, but "
                "src/%s.f90 has no `public :: parquet_set_%s`/`parquet_get_%s` pair."
                % (module_name, knob, module_name, knob, knob))
        for knob in sorted(have - want):
            problems.append(
                "doc/pages/operating/choosing-a-module.md: src/%s.f90 re-exports `%s`, which the "
                "table's row for that module does not mention -- a reader taking that import "
                "cannot discover a knob the module offers." % (module_name, knob))
    if rows < 2:
        return problems + [
            "doc/pages/operating/choosing-a-module.md: parsed %d row(s) from the \"Settings it "
            "re-exports\" table -- its shape changed and this check has gone blind" % rows]
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
        (`.claude/rules/api-conventions.md`, "Settings (`parquet_settings`)").

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


def check_parquet_index_stays_arrow_free():
    """`use parquet_index` must not drag the Arrow/Parquet C++ stack into a consumer's build.

    One check per tier is the standing rule, and this tier needs its own for a reason worth
    stating: it is covered by nothing else. `parquet_index` is reached by no other checked module,
    so a `use parquet_core` added to it -- or to any of its four submodules -- would be invisible
    to every other closure walk here while multiplying what a `use parquet_index` consumer
    compiles from a handful of files to the whole reader/writer stack.

    Nothing in `fpm test` can see it either: the library obviously has Arrow, so the stray import
    compiles and tests perfectly well here and only inflates the consumer's build. The walk covers
    the submodules, which is where such an import is most likely to be added -- the sorted
    backend already imports `parquet_argsort`, so the file that would most naturally reach for
    `parquet_sorting` (and through it `parquet_bindings`) is one of the four rather than the spec.
    """
    return _check_stays_arrow_free(
        "parquet_index",
        "The index tier is Arrow-free by design: a map or a pool over plain integer keys needs no "
        "C++ at all, and the sorted backend takes its sort from the argsort tier for exactly that "
        "reason.")


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


def check_parquet_sphere_stays_arrow_free():
    """`use parquet_sphere` must not drag the Arrow/Parquet C++ stack into a consumer's build.

    The region samplers and the RA/Dec geometry sit on `parquet_random`, `parquet_healpix` and
    `parquet_utils` and nothing else, so a program drawing a mock catalogue in a sky footprint
    compiles a handful of files. It reaches two other Arrow-free tiers, and this check is its own
    rather than theirs: a stray import added here would pass both of theirs.
    """
    return _check_stays_arrow_free(
        "parquet_sphere",
        "The sphere tier exists so that sampling a sky footprint needs no reader, no writer and no "
        "Arrow; an import reaching the reader/writer stack would defeat that.")


def check_parquet_skycoord_stays_arrow_free():
    """`use parquet_skycoord` must not drag the Arrow/Parquet C++ stack into a consumer's build.

    Converting a position between coordinate systems is arithmetic over plain reals: the module
    reaches `parquet_utils` and nothing else, so a program turning an RA/Dec column into Galactic
    coordinates compiles a handful of files. It sits below `parquet_sphere` and `parquet_healpix`
    and imports neither, and this check is its own rather than theirs: a stray import added here
    would pass both of theirs.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_skycoord",
        "Converting a sky position between coordinate systems must not require the Arrow stack.")


def check_parquet_cosmology_stays_arrow_free():
    """`use parquet_cosmology` must not drag the Arrow/Parquet C++ stack into a consumer's build.

    Turning a redshift column into comoving distances is quadrature and interpolation over plain
    reals: the module reaches `parquet_integrate`, `parquet_interpolate` and `parquet_utils`, and
    nothing else. `parquet_sphere` is deliberately NOT imported -- the comoving Cartesian
    coordinates a survey wants are three multiplications the guide shows, against the 15-file
    graph, `parquet_random` included, that importing it would add to every consumer.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_cosmology",
        "Turning a redshift into a distance must not require the Arrow stack.")


def check_parquet_cosmology_config_stays_arrow_free():
    """`use parquet_cosmology_config` must not drag the Arrow/Parquet C++ stack into a build.

    This tier joins `parquet_cosmology` to `parquet_toml` and is the one place in the library where
    a numerical tier meets the configuration reader, so it is the natural place for an import that
    would undo both promises at once: `parquet_core` for "just to read the table's metadata too",
    or `parquet_settings` for an output knob. Neither parent reaches `parquet_bindings`, and the
    whole point of putting these two procedures in a module of their own is that nothing anyone
    already imports grows -- which is a property of the `use` graph that no test can see.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_cosmology_config",
        "Reading a cosmology out of a configuration file must not require the Arrow stack.")


def _fortran_char_array_literal(path, name):
    """The string elements of a `character(...), parameter :: <name>(...) = [... :: "a", "b"]`.

    Returns `None` when the declaration is not found, so a caller can fail loudly rather than
    compare against an empty list -- an enumeration check that silently matches nothing is the
    failure mode this file's own comments keep returning to.
    """
    text = path.read_text(encoding="utf-8")
    lines = text.split("\n")
    start = None
    for i, raw in enumerate(lines):
        code = strip_comment(raw)
        if re.search(r"parameter\s*::\s*%s\s*\(" % re.escape(name), code) and "[" in code:
            start = i
            break
    if start is None:
        return None
    buf = []
    for raw in lines[start:]:
        code = strip_comment(raw)
        buf.append(code)
        if "]" in code:
            break
    joined = " ".join(buf)
    body = joined[joined.index("["):joined.index("]")]
    return re.findall(r'"([^"]*)"', body)


def _fortran_dummy_names(path, proc):
    """The dummy-argument names of `subroutine <proc>(...)`, continuations followed.

    Returns `None` when the declaration is not found.
    """
    text = path.read_text(encoding="utf-8")
    lines = text.split("\n")
    start = None
    for i, raw in enumerate(lines):
        code = strip_comment(raw)
        if re.search(r"\bsubroutine\s+%s\s*\(" % re.escape(proc), code):
            start = i
            break
    if start is None:
        return None
    buf = []
    for raw in lines[start:]:
        code = strip_comment(raw).rstrip()
        buf.append(code.rstrip("&"))
        if ")" in code:
            break
    joined = " ".join(buf)
    body = joined[joined.index("(") + 1:joined.rindex(")")]
    return [w.strip() for w in body.split(",") if w.strip()]


def check_cosmology_config_keys_match_init():
    """The `[cosmology]` section's key table is `%init`'s own argument list.

    `src/parquet_cosmology_config.f90` names every key the section carries, and
    `cosmology_init_params` (`src/parquet_cosmology.f90`) names every parameter a caller can set.
    The two lists are in different files, are maintained by different changes, and nothing ties
    them: `%init` gaining an argument leaves a section that cannot set it, and a key added here
    with no argument behind it is read and then silently dropped. Neither shows up as a test
    failure, because a key nobody writes is a key nobody round-trips.

    `this` and `context` are excluded and named here rather than filtered by shape: `this` is the
    object being built and `context` is the caller's own, composed by the reader from the file and
    the section rather than read from it. Any OTHER argument must have a key.

    The comparison is by set rather than by order -- the key table follows `%init`'s order today
    and nothing depends on that -- and a parse that finds nothing FAILS, because an enumeration
    check that matches nothing passes everything.
    """
    cfg = REPO_ROOT / "src" / "parquet_cosmology_config.f90"
    cos = REPO_ROOT / "src" / "parquet_cosmology.f90"
    keys = _fortran_char_array_literal(cfg, "CFG_KEYS")
    args = _fortran_dummy_names(cos, "cosmology_init_params")
    if not keys:
        return ["src/parquet_cosmology_config.f90: CFG_KEYS could not be read -- this check needs "
                "updating, and until it is nothing compares the section's keys with %init's "
                "arguments"]
    if not args:
        return ["src/parquet_cosmology.f90: cosmology_init_params' argument list could not be read "
                "-- this check needs updating, and until it is nothing compares it with the "
                "[cosmology] section's keys"]
    NOT_KEYS = {"this", "context"}
    want = set(args) - NOT_KEYS
    got = set(keys)
    problems = []
    for name in sorted(want - got):
        problems.append(
            "src/parquet_cosmology_config.f90: CFG_KEYS has no `%s`, which cosmology_init_params "
            "takes -- a configuration file cannot set it, and nothing else would say so. Add the "
            "key, the writer's line for it, its row in the guide page's key table, and a test."
            % name)
    for name in sorted(got - want):
        problems.append(
            "src/parquet_cosmology_config.f90: CFG_KEYS has `%s`, which cosmology_init_params does "
            "not take -- the reader would read it and drop it. Remove the key, or add the argument."
            % name)
    return problems


def check_cosmology_config_names_match_the_library():
    """The eight named cosmologies `parquet_cosmology_config` copies are the library's own.

    `pfc_named_name` is PRIVATE to `parquet_cosmology`, and this feature deliberately adds no public
    name to that module, so the configuration tier carries its own copy of the eight spellings --
    it needs them to tell a section that SELECTS a cosmology from one that merely labels a custom
    model, and the writer needs them to know which names it must not write beside parameters.

    A copy with nothing checking it is a drift hazard of the worst kind here: a ninth cosmology
    added to the library would simply not be selectable from a file, and a section naming it would
    fail with "h0 and om0 are required" rather than with anything that points at the cause.

    Compared without regard to case, because the match itself is: the library folds a caller's
    token before comparing, and so does the copy. An empty parse FAILS.
    """
    cfg = REPO_ROOT / "src" / "parquet_cosmology_config.f90"
    cos = REPO_ROOT / "src" / "parquet_cosmology.f90"
    mine = _fortran_char_array_literal(cfg, "CFG_NAMED")
    theirs = _fortran_char_array_literal(cos, "pfc_named_name")
    if not mine:
        return ["src/parquet_cosmology_config.f90: CFG_NAMED could not be read -- this check needs "
                "updating, and until it is nothing ties the copy to the library's own list"]
    if not theirs:
        return ["src/parquet_cosmology.f90: pfc_named_name could not be read -- this check needs "
                "updating, and until it is nothing ties parquet_cosmology_config's copy to it"]
    if sorted(x.strip().lower() for x in mine) != sorted(x.strip().lower() for x in theirs):
        return [
            "src/parquet_cosmology_config.f90: CFG_NAMED is %s but src/parquet_cosmology.f90's "
            "pfc_named_name is %s. The copy exists because the original is private; when the "
            "library gains or renames a named cosmology, this list moves with it -- otherwise a "
            "configuration file cannot select the new one and says so in a message about h0."
            % (sorted(mine), sorted(theirs))]
    return []


def check_cosmology_config_keys_documented():
    """The `[cosmology]` key table on the guide page is `CFG_KEYS`, in both directions.

    `check_cosmology_config_keys_match_init` ties `CFG_KEYS` to `cosmology_init_params`' arguments,
    and its own failure text asks whoever fixes it to update "its row in the guide page's key
    table" -- but nothing reads that table. A key added to both source lists and not to the page is
    then undocumented with every check green, and a row left on the page for a key the reader no
    longer accepts sends a caller to write a key that is read and dropped.

    Reads the NARROWEST part of the page that carries the claim: the table's FIRST column only. The
    `note` column legitimately names `%is_flat()`, `%ob0()` and `%init`, and the prose around the
    table names `ode0`, `ob0` and `m_nu` again when it explains which three have no absent value --
    matching on the whole row or the whole section would accept a table that had lost a row.

    FAILS rather than passes when it goes blind: a missing page, a missing heading, a table that
    cannot be found under it, or an unreadable `CFG_KEYS` are each reported as needing this check
    updated.
    """
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "configuration-files.md"
    cfg = REPO_ROOT / "src" / "parquet_cosmology_config.f90"
    if not page.exists():
        return ["doc/pages/utilities/configuration-files.md is missing -- this check needs "
                "updating, and until it is nothing ties the [cosmology] key table to CFG_KEYS"]
    keys = _fortran_char_array_literal(cfg, "CFG_KEYS")
    if not keys:
        return ["src/parquet_cosmology_config.f90: CFG_KEYS could not be read -- this check needs "
                "updating, and until it is nothing ties it to the guide page's key table"]

    lines = page.read_text(encoding="utf-8").split("\n")
    anchor = "## A cosmology in a configuration file"
    start = next((i for i, l in enumerate(lines) if l.strip() == anchor), None)
    if start is None:
        return ["doc/pages/utilities/configuration-files.md: the heading '%s' is gone -- this "
                "check reads the key table under it and can no longer find it, so nothing compares "
                "the table with CFG_KEYS. Update the check with the new heading." % anchor]
    # the section runs to the next `## ` heading
    end = next((i for i in range(start + 1, len(lines))
                if lines[i].startswith("## ")), len(lines))

    documented, in_table = [], False
    for line in lines[start:end]:
        stripped = line.strip()
        if stripped.startswith("| key |"):
            in_table = True
            continue
        if in_table:
            if not stripped.startswith("|"):
                break
            cell = stripped.split("|")[1].strip()
            if set(cell) <= set("-: "):          # the delimiter row
                continue
            m = re.fullmatch(r"`([a-z_0-9]+)`", cell)
            if m:
                documented.append(m.group(1))
    if not documented:
        return ["doc/pages/utilities/configuration-files.md: no `| key | ...` table was found "
                "under '%s', or its first column holds no `key` cells -- this check needs "
                "updating, and until it is nothing compares the page with CFG_KEYS." % anchor]

    want, got = set(k.strip() for k in keys), set(documented)
    problems = []
    for name in sorted(want - got):
        problems.append(
            "doc/pages/utilities/configuration-files.md: the [cosmology] key table has no row for "
            "`%s`, which CFG_KEYS carries -- a caller cannot learn the key exists. Add the row, "
            "with what an absent key means." % name)
    for name in sorted(got - want):
        problems.append(
            "doc/pages/utilities/configuration-files.md: the [cosmology] key table has a row for "
            "`%s`, which CFG_KEYS does not carry -- the reader would drop it. Remove the row, or "
            "add the key to CFG_KEYS and to %%init." % name)
    return problems


def check_cosmology_named_documented():
    """The eight named cosmologies: the one written-out list, and every page that counts them.

    `utilities/cosmology.md` is the only page that writes the names out;
    `utilities/configuration-files.md` says "the eight" six times and lists them nowhere, which is
    why it links to `cosmology.md`. Two hand-written claims the code owns, and neither is tied to
    it: a ninth cosmology, or a renamed one, leaves the written-out list quietly wrong and every
    "the eight" quietly miscounting. `check_cosmology_config_names_match_the_library` ties
    `CFG_NAMED` to `pfc_named_name` and reads no page.

    Reads the NARROWEST part that carries each claim. For the list, the ONE sentence that
    introduces it, found by its own anchor phrase plus its continuation -- not the page, because
    every one of the eight appears elsewhere on it, so a page that had LOST a name would still
    match. For the count, only the number word immediately before a "named cosmolog..."/"of the"
    phrase, so ordinary prose containing a number is not read as a count.

    An unreadable `CFG_NAMED`, a missing page, an anchor phrase that no longer matches, or a
    sentence that turns out to name none of them each FAIL.
    """
    NUMBER_WORD = {1: "one", 2: "two", 3: "three", 4: "four", 5: "five", 6: "six", 7: "seven",
                   8: "eight", 9: "nine", 10: "ten", 11: "eleven", 12: "twelve"}
    cfg = REPO_ROOT / "src" / "parquet_cosmology_config.f90"
    named = _fortran_char_array_literal(cfg, "CFG_NAMED")
    if not named:
        return ["src/parquet_cosmology_config.f90: CFG_NAMED could not be read -- this check needs "
                "updating, and until it is nothing ties the guide pages to it"]
    want = set(n.strip() for n in named)
    word = NUMBER_WORD.get(len(want))
    if word is None:
        return ["src/parquet_cosmology_config.f90: CFG_NAMED holds %d names and this check only "
                "spells counts up to %d -- update it." % (len(want), max(NUMBER_WORD))]

    problems = []

    # (1) the one written-out list
    rel, anchor = "doc/pages/utilities/cosmology.md", "The named cosmologies are"
    page = REPO_ROOT / rel
    if not page.exists():
        problems.append("%s is missing -- this check needs updating, and until it is nothing ties "
                        "the written-out list of the eight to CFG_NAMED" % rel)
    else:
        lines = page.read_text(encoding="utf-8").split("\n")
        hit = next((i for i, l in enumerate(lines) if anchor in l), None)
        if hit is None:
            problems.append(
                "%s: the phrase '%s' is gone -- this check reads the list of named cosmologies "
                "from that sentence and can no longer find it, so nothing compares the page with "
                "CFG_NAMED. Update the check with the new wording." % (rel, anchor))
        else:
            blob = []
            for line in lines[hit:]:
                if not line.strip():
                    break
                blob.append(line)
            found = set(re.findall(r"`(WMAP[0-9]+|Planck[0-9]+)`", " ".join(blob)))
            if not found:
                problems.append(
                    "%s: the sentence at '%s' names no cosmology in backticks -- this check needs "
                    "updating, and until it is nothing compares the page with CFG_NAMED."
                    % (rel, anchor))
            for name in sorted(want - found):
                problems.append("%s: the sentence introducing the named cosmologies omits `%s`, "
                                "which CFG_NAMED carries -- a reader cannot learn it can be "
                                "selected." % (rel, name))
            for name in sorted(found - want):
                problems.append("%s: the sentence introducing the named cosmologies names `%s`, "
                                "which CFG_NAMED does not carry -- selecting it fails with a "
                                "message about h0." % (rel, name))

    # (2) every page that COUNTS them without listing them
    counted = 0
    for rel in ("doc/pages/utilities/cosmology.md",
                "doc/pages/utilities/configuration-files.md"):
        page = REPO_ROOT / rel
        if not page.exists():
            problems.append("%s is missing -- this check needs updating, and until it is nothing "
                            "ties its count of the named cosmologies to CFG_NAMED" % rel)
            continue
        text = page.read_text(encoding="utf-8")
        for m in re.finditer(r"\b(?:the|of the)\s+([a-z]+)\s+(?=named cosmolog|are ever|\*\*beside\*\*|selects that|is written)",
                             text):
            counted += 1
            if m.group(1) != word:
                problems.append(
                    "%s: '%s' counts the named cosmologies, but CFG_NAMED holds %d (`%s`). Every "
                    "page that says how many there are moves with the list."
                    % (rel, m.group(0).strip(), len(want), word))
        for m in re.finditer(r"\bnot one of the ([a-z]+)\b", text):
            counted += 1
            if m.group(1) != word:
                problems.append(
                    "%s: 'not one of the %s' counts the named cosmologies, but CFG_NAMED holds %d "
                    "(`%s`)." % (rel, m.group(1), len(want), word))
    if counted == 0:
        problems.append(
            "no page was found counting the named cosmologies -- this check reads phrases of the "
            "form 'the <number> named cosmologies' and 'not one of the <number>' and matched none, "
            "so nothing ties those counts to CFG_NAMED. Update the check with the new wording.")
    return problems


def check_cosmology_drag_bracket_matches_the_oracle():
    """`%z_drag`'s bracket is the same two redshifts in the library and in its oracle.

    `%z_drag` solves `r_s(z) = r_drag` over `z` in `[100, 1e5]` and answers a quiet NaN for a model
    whose `r_d` is not attained between them. `tools/generate_cosmology_reference.py` makes the
    SAME refusal when it emits the reference `z_drag`, so that a disagreement between the two is
    about the root and never about which models have a drag epoch at all.

    Two copies of a bound, in two languages, with nothing tying them together: widening the
    library's bracket alone would leave the reference carrying NaN for a model the library now
    answers for, and the row test would fail pointing at the answer rather than at the bound.
    Widening the oracle's alone is worse -- the reference would carry a number the library refuses.
    An empty parse FAILS.
    """
    src = (REPO_ROOT / "src" / "parquet_cosmology_eval.f90").read_text()
    gen = (REPO_ROOT / "tools" / "generate_cosmology_reference.py").read_text()
    out = []
    for fortran, python in (("PFC_DRAG_Z_LO", "DRAG_Z_LO"), ("PFC_DRAG_Z_HI", "DRAG_Z_HI")):
        m = re.search(r"::\s*%s\s*=\s*([0-9eE.+_-]+?)_real64" % fortran, src)
        p = re.search(r'^%s\s*=\s*mp\.mpf\("([^"]+)"\)' % python, gen, re.M)
        if m is None:
            out.append("src/parquet_cosmology_eval.f90: %s could not be read -- this check needs "
                       "updating, and until it is nothing ties the bracket to its oracle" % fortran)
            continue
        if p is None:
            out.append("tools/generate_cosmology_reference.py: %s could not be read -- this check "
                       "needs updating, and until it is nothing ties the oracle to the bracket"
                       % python)
            continue
        if float(m.group(1)) != float(p.group(1)):
            out.append(
                "src/parquet_cosmology_eval.f90's %s is %s but "
                "tools/generate_cosmology_reference.py's %s is %s. %%z_drag refuses a model whose "
                "r_d falls outside this bracket, and the reference must refuse the same models: "
                "move the two together."
                % (fortran, m.group(1), python, p.group(1)))
    return out


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


def check_parquet_toml_takes_the_guard():
    """Every public `parquet_toml` entry takes the module guard; no private worker does.

    `parquet_toml` is safe to call from inside an OpenMP parallel region because each public entry
    wraps its work in one module-wide named critical section. Both halves of that arrangement fail
    silently if broken, and in opposite directions:

      * **A public procedure added WITHOUT the guard** races on the document's accumulator and on
        toml-f's `intent(inout)` table, and -- worst -- on the hidden length temporary gfortran
        uses to receive `toml_context%report`'s `character(:), allocatable` result, which is the
        PR113797 shape this project forbids in its own code and has already been bitten by once
        (`top_level_of`, found by ThreadSanitizer). Nothing fails on an ordinary run: the corruption
        needs concurrency and a particular allocator state.
      * **A private worker that TAKES the guard** self-deadlocks the moment a guarded entry calls
        it, because a named critical section is not reentrant. That is a hang rather than a wrong
        answer, on one thread, with no diagnostic.

    Two clauses, and the exemptions are DERIVED rather than listed, so they cannot go stale:

      1. every public procedure of the module takes the guard, unless it is a `pf_toml_strings`
         binding (that type is a self-contained copy referencing no document and no module state,
         so there is nothing to serialise and a lock inside `%get` would sit in a per-element
         loop), or unless its body contains no `call` at all and never touches `%doc` (a pure
         pointer or scalar query such as `pf_toml_is_open`);
      2. no procedure that is not public contains the directive.

    See feature_toml.md section 9.
    """
    path = SRC / "parquet_toml.f90"
    if not path.is_file():
        return ["src/parquet_toml.f90: missing -- this check needs updating"]
    lines = path.read_text(encoding="utf-8").splitlines()

    # ---- the public surface: `public ::` lists plus every generic's `module procedure` members --
    public = set()
    for raw in lines:
        code = strip_comment(raw)
        m = re.match(r"^\s*public\s*::\s*(.*)$", code, re.I)
        if m:
            for name in re.split(r"[,\s]+", m.group(1).replace("&", "")):
                if name:
                    public.add(name.lower())
        m = re.match(r"^\s*module\s+procedure\s*(?:::)?\s*(.*)$", code, re.I)
        if m:
            for name in re.split(r"[,\s]+", m.group(1).replace("&", "")):
                if name:
                    public.add(name.lower())
    if not public:
        return ["src/parquet_toml.f90: parsed no public names -- this check has gone blind"]

    # ---- walk the procedure bodies ---------------------------------------------------------
    proc_re = re.compile(
        r"^\s*(?:pure\s+|elemental\s+|impure\s+|recursive\s+)*"
        r"(?:subroutine|(?:integer|logical|real|character)[^:]*function|function)\s+(\w+)", re.I)
    end_re = re.compile(r"^\s*end\s+(?:subroutine|function)\b", re.I)
    problems = []
    name, body, raw_body, inside = "", [], [], False
    bodies = []
    for raw in lines:
        code = strip_comment(raw)
        if not inside:
            m = proc_re.match(code)
            if m:
                name, body, raw_body, inside = m.group(1), [], [], True
            continue
        if end_re.match(code):
            bodies.append((name, body, raw_body))
            inside = False
            continue
        body.append(code)
        raw_body.append(raw)

    GUARD = "critical (parquet_toml_guard)"
    for name, body, raw_body in bodies:
        text = "\n".join(body)
        # The guard is a `!$omp` DIRECTIVE, i.e. a comment -- so it is looked for in the raw lines,
        # which strip_comment has not touched.
        guarded = any(GUARD in ln for ln in raw_body)
        is_public = name.lower() in public
        if is_public:
            strings_binding = re.search(r"class\s*\(\s*pf_toml_strings\s*\)", text, re.I)
            trivial = not re.search(r"^\s*call\s", text, re.M | re.I) and "%doc" not in text.lower()
            if not guarded and not strings_binding and not trivial:
                problems.append(
                    "src/parquet_toml.f90: public procedure `%s` does not take "
                    "`!$omp critical (parquet_toml_guard)` -- without it a concurrent call races on "
                    "the accumulator and on toml-f's own state, silently (feature_toml.md 9)" % name)
        elif guarded:
            problems.append(
                "src/parquet_toml.f90: private worker `%s` takes the module guard -- a named "
                "critical section is not reentrant, so the first guarded entry that calls it "
                "self-deadlocks, on one thread, with no diagnostic (feature_toml.md 9)" % name)
    return problems


def check_parquet_toml_stays_arrow_free():
    """`use parquet_toml` must not reach parquet_bindings.

    Its Fortran graph is `tomlf` plus `parquet_logging`, and that is the whole of it. A program
    that only wants to read its own configuration file should not compile the reader, the writer
    or the C++ wrapper to do it -- and the edge that would break this is an inviting one, because
    `parquet_settings` is where every OTHER module in this library gets its output knobs from.
    This module deliberately does not want them: it emits through `parquet_logging`, whose levels
    and sinks belong to the calling program, so `parquet_verbosity` governs nothing here (which
    the guide page says out loud, for the reader who changes it and sees no effect).

    One check per tier rather than one for the group, per the established pattern: the only module
    that imports `parquet_toml` is the `parquet` facade, which reaches Arrow anyway, so nothing
    else would notice this tier acquiring an edge.
    """
    return _check_stays_arrow_free(
        "parquet_toml",
        "A configuration-file reader must not require the Arrow stack to read a config file.")


def check_parquet_utils_stays_arrow_free():
    """`use parquet_utils` must not reach parquet_bindings.

    Stricter than a tier rule: this module imports the INTRINSIC modules `iso_fortran_env` and
    `ieee_arithmetic` and NOTHING else -- no module of this library, not even
    `parquet_settings_base` -- and that is load-bearing rather than tidy. An intrinsic module is
    not a compiled file, a tier edge or a footprint entry, so it does not weaken the property;
    a library module would. `parquet_settings_base`
    used to carry a private ASCII fold with a doc-comment explaining that it could not call
    `parquet_core`'s copy without creating a circular dependency; a module strictly below
    everything is what removes that cycle. The obvious import to add here is `parquet_settings`,
    for a verbosity knob -- which is the exact edge that would put the cycle back AND drag in the
    C++ boundary. It needs none: nothing in this module prints, validates or aborts.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_utils",
        "Joining two path components must not require the Arrow stack.")


def check_parquet_integrate_stays_arrow_free():
    """`use parquet_integrate` must not reach parquet_bindings.

    Quadrature reaches no reader, no writer and no setting: the module imports the INTRINSIC
    modules `iso_fortran_env` and `ieee_arithmetic` and nothing else, which is what makes
    `use parquet_integrate` cost three Fortran files. The obvious import to add is
    `parquet_settings`, for a verbosity knob -- and the module has nothing to print, by design
    (it reports through `converged=`, `info=` and the `PF_INT_*` codes), so it needs none.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_integrate",
        "Integrating a function of one variable must not require the Arrow stack.")


def check_parquet_root_stays_arrow_free():
    """`use parquet_root` must not reach parquet_bindings.

    Root finding reaches no reader, no writer and no setting: the module imports the INTRINSIC
    modules `iso_fortran_env` and `ieee_arithmetic` and nothing else, which is what makes
    `use parquet_root` cost two Fortran files. The obvious import to add is `parquet_settings`,
    for a verbosity knob -- and the module has nothing to print, by design (it reports through
    `converged=`, `info=` and the `PF_ROOT_*` codes), so it needs none.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_root",
        "Finding a root of a function of one variable must not require the Arrow stack.")


def check_parquet_transform_stays_arrow_free():
    """`use parquet_transform` must not reach parquet_bindings.

    The cosine and sine transforms reach no reader, no writer and no setting: the module imports
    the INTRINSIC module `iso_fortran_env` and nothing else, which is what makes
    `use parquet_transform` cost two Fortran files. It has nothing to print -- its only output path
    is `error stop` -- so the obvious import to add, `parquet_settings` for a verbosity knob, has
    nothing to govern.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_transform",
        "Transforming a sequence must not require the Arrow stack.")


def check_parquet_transform_holds_no_state():
    """`parquet_transform` declares no module-level variable that is not a `parameter`.

    `doc/pages/utilities/transforms.md`'s "Thread safety" section tells the reader to transform
    "from as many threads as you like, each on its own arrays", and gives the mechanism rather than
    a promise: the module "has no variable that is not a compile-time constant, and each call
    allocates its own workspace". `doc/pages/operating/thread-safety.md` repeats it by listing this
    tier among those that "share no state between calls", which is why that page carries no rule
    for it.

    **The regression this catches is a plausible one, and it would be silent.** The engine builds a
    twiddle table of `n/2` cosines on every call and the page says in its own words that "nothing is
    cached between calls"; caching that table in a module `save` variable is the obvious
    optimisation and would make every concurrent call race over it. Nothing else in the tree would
    notice: the answer is unchanged single-threaded, no test calls a transform from inside a
    parallel region, and a data race is a flaky detector even where one does.

    Checked over the module and its submodule, across the SPECIFICATION part only -- the interface
    bodies in `parquet_transform.f90` are full of dummy-argument declarations, which are not module
    state, so every `interface`/`end interface` region is skipped.
    """
    problems = []
    for name in ("parquet_transform.f90", "parquet_transform_core.f90"):
        path = SRC / name
        if not path.is_file():
            return ["check_parquet_transform_holds_no_state: src/%s is missing -- this check has "
                    "gone blind" % name]
        lines = stripped_lines(path)

        # The specification part: up to the module-level `contains`, or to `end module` /
        # `end submodule` where there is none -- `parquet_transform.f90` is a spec-only module
        # whose every body lives in the submodule, so it has no `contains` at all.
        end = None
        for i, code in enumerate(lines):
            if re.match(r"^\s*contains\s*$", code, re.I) or \
               re.match(r"^\s*end\s*(sub)?module\b", code, re.I):
                end = i
                break
        if end is None:
            return ["check_parquet_transform_holds_no_state: found neither a module-level "
                    "`contains` nor an `end module` in src/%s -- this check has gone blind" % name]

        depth, parameters = 0, 0
        for i, code in enumerate(lines[:end]):
            if re.match(r"^\s*(abstract\s+)?interface\b", code, re.I):
                depth += 1
                continue
            if re.match(r"^\s*end\s*interface\b", code, re.I):
                depth -= 1
                continue
            if depth > 0 or "::" not in code:
                continue
            attrs = code.split("::", 1)[0]
            # A declaration, as opposed to an access or binding statement (`public ::`, and the
            # `procedure ::`/`generic ::` of a type's contains block).
            if not re.match(r"^\s*(integer|real|double\s+precision|complex|logical|character"
                            r"|type\s*\(|class\s*\()", attrs, re.I):
                continue
            if re.search(r"\bparameter\b", attrs, re.I):
                parameters += 1
                continue
            problems.append(
                "    src/%s:%d declares module-level state, which this module promises it has "
                "none of:\n      %s" % (name, i + 1, code.strip()))
        if depth != 0:
            return ["check_parquet_transform_holds_no_state: interface nesting in src/%s did not "
                    "close (depth %d) -- this check has gone blind" % (name, depth)]
        if parameters == 0:
            return ["check_parquet_transform_holds_no_state: found no module-level `parameter` in "
                    "src/%s, so the scan never reached its declarations -- this check has gone "
                    "blind" % name]

    if problems:
        return ["`parquet_transform` must hold no state between calls: the guide page promises "
                "reentrancy and gives 'no variable that is not a compile-time constant' as the "
                "reason, and nothing else would catch a cached twiddle table\n"
                "(check_parquet_transform_holds_no_state):\n" + "\n".join(problems)]
    return []


def check_parquet_kde_stays_arrow_free():
    """`use parquet_kde` must not reach parquet_bindings.

    Kernel density estimation reaches no reader, no writer and no C++ setting: its one library
    tier edge is `parquet_stats` (the rules' scale and the family's population rules), and what it
    prints it prints through `parquet_settings_base`, which is Arrow-free by construction. The
    obvious import to add is `parquet_settings` for the verbosity pair -- which would put the C++
    boundary under every density estimate; `parquet_settings_base` already carries the pair.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_kde",
        "Estimating a density must not require the Arrow stack.")


def check_parquet_interpolate_stays_arrow_free():
    """`use parquet_interpolate` must not reach parquet_bindings.

    Interpolation reaches no reader, no writer and no setting: the module imports the INTRINSIC
    modules `iso_fortran_env` and `ieee_arithmetic` and nothing else, which is what makes
    `use parquet_interpolate` cost four Fortran files. The obvious import to add is
    `parquet_settings`, for a verbosity knob -- and the module has nothing to print, by design
    (every caller mistake is an `error stop`, and every answer a value), so it needs none.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_interpolate",
        "Interpolating a table of numbers must not require the Arrow stack.")


def check_parquet_optimize_stays_arrow_free():
    """`use parquet_optimize` must not reach parquet_bindings.

    Minimisation reaches no reader, no writer and no setting: the local engines import the
    INTRINSIC modules `iso_fortran_env` and `ieee_arithmetic` and nothing else, and the population
    tier adds `parquet_sampling` for its Latin-hypercube designs -- itself Arrow-free. The obvious
    import to add is `parquet_settings`, for a verbosity knob, and the module has nothing to print
    by design: it reports through `info=` and the `PF_OPT_*` codes, and a caller mistake is an
    `error stop`. The one emitter it can reach is `parquet_clamp_to_affinity`'s once-per-process
    notice, which lives in `parquet_settings_base`, below the Arrow boundary.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_optimize",
        "Minimising a function of one or many variables must not require the Arrow stack.")


def check_parquet_prima_stays_arrow_free():
    """`use parquet_prima` must not reach parquet_bindings.

    The vendored PRIMA engines import `iso_fortran_env` and `ieee_arithmetic` and, through
    `parquet_optimize`, `parquet_sampling` and the argsort tier -- all Arrow-free. PRIMA's own
    tree reaches nothing at all: no reader, no writer, no setting, not even a print, because its
    `iprint` layer is not vendored. The import that would break it is the same one that would
    break `parquet_optimize`: `parquet_settings`, for a knob to quieten something. There is
    nothing to quieten.

    One check per tier rather than one for the group, per the established pattern.
    """
    return _check_stays_arrow_free(
        "parquet_prima",
        "Powell's derivative-free methods must not require the Arrow stack.")


def check_prima_sums_are_the_ordered_sum():
    """Every `sum(...)` in the vendored PRIMA tier reaches `parquet_prima_linalg`'s ordered `sum`.

    `parquet_prima_linalg` extends the intrinsic `sum` over real vectors, and real matrices with
    `dim=`, by specifics adding in index order (its header, deviation 9). A file that calls `sum`
    without importing it gets the intrinsic, whose order is the processor's: flang's runtime `SUM`
    compensates its rounding at `-O0`, so that one call site rounds differently there, and a
    trust-region method's search path turns on the last bit. Nothing else notices -- only a flang
    run of a path-sensitive reproducer in `test/test_prima.f90` does. A `mask=` form is not
    extended, so it is refused in every file of the tier, `parquet_prima_linalg` included.

    See `.claude/rules/fortran-gotchas.md`, "flang-specific gotchas".
    """
    problems = []
    callers = 0
    for path in sorted((REPO_ROOT / "src").glob("parquet_prima*.f90")):
        rel = path.relative_to(REPO_ROOT)
        first_call, imports_sum = None, False
        for n, stmt in _joined_statements(path):
            use = re.match(r"\s*use\s+parquet_prima_linalg\s*,\s*only\s*:(.*)$", stmt, re.I)
            if use and any(name.strip().lower() == "sum" for name in use.group(1).split(",")):
                imports_sum = True
            for call in re.finditer(r"\bsum\s*\(", stmt, re.I):
                if first_call is None:
                    first_call = n
                depth, args = 0, ""
                for ch in stmt[call.end() - 1:]:
                    depth += (ch == "(") - (ch == ")")
                    args += ch
                    if depth == 0:
                        break
                top, depth = "", 0
                for ch in args[1:-1]:
                    depth += (ch == "(") - (ch == ")")
                    top += ch if depth == 0 else " "
                if re.search(r"\bmask\s*=", top, re.I):
                    problems.append(
                        "%s:%d: `sum(..., mask=)` is the intrinsic, whose order is the processor's; "
                        "parquet_prima_linalg's ordered `sum` has no mask form (header deviation 9)"
                        % (rel, n))
        if first_call is None:
            continue
        callers += 1
        if path.name != "parquet_prima_linalg.f90" and not imports_sum:
            problems.append(
                "%s:%d: calls `sum` without importing it from parquet_prima_linalg, so it gets the "
                "intrinsic, which flang sums in another order at -O0 -- add `sum` to the only: list "
                "(parquet_prima_linalg's header, deviation 9)" % (rel, first_call))
    if callers == 0:
        problems.append("no src/parquet_prima*.f90 calls `sum`: this check has come up empty -- "
                        "re-aim it before trusting its silence")
    return problems


def check_parquet_utils_is_total():
    """Every `parquet_utils` procedure is `pure`, and the module contains no `error stop`.

    The module's central claim -- stated in its own header and in doc/pages/utilities/utils.md --
    is a NEGATIVE: "nothing validates, nothing aborts, nothing prints". It is the reason the module
    needs no out-of-process error scenarios at all, and no test can assert it: an in-process test
    cannot observe an abort that never happens, and a scenario harness has nothing to run.

    **Two thirds of it are enforced by the compiler, because every procedure is `pure`.** Measured
    with three-line programs under gfortran 15.2: a `pure` procedure containing `print` is rejected
    ("PRINT statement at (1) not allowed within PURE procedure"), one containing `stop` is rejected
    ("STOP statement not allowed in PURE procedure"), and one containing `error stop` COMPILES
    FINE. So `pure` is what makes "nothing prints" and "nothing stops" build-time facts rather than
    promises, and only the `error stop` third rests on there being none in the file.

    Hence two clauses, neither of which covers the other. Clause (a) keeps the compiler doing that
    work: a future procedure added without `pure` silently removes the guarantee for itself while
    every test stays green. Clause (b) is the third the compiler declines to check.

    An empty scan FAILS (CLAUDE.md, "A static check that enumerates names goes stale silently"): no
    procedures found means the regex or the file moved, not that the module has none.
    """
    problems = []
    path = REPO_ROOT / "src" / "parquet_utils.f90"
    if not path.exists():
        return ["src/parquet_utils.f90: not found -- this check has gone blind on the module"]

    # A definition statement, not `end subroutine` and not a call. The part before the keyword is
    # the prefix, which is where `pure` has to be (a function may also carry a type there).
    define_re = re.compile(r"^\s*(?P<prefix>[^!]*?)\b(?P<kind>subroutine|function)\s+(?P<name>\w+)")
    found = 0
    for lineno, raw in enumerate(path.read_text().split("\n"), start=1):
        code = strip_comment(raw)
        stripped = code.strip()
        if not stripped or stripped.lower().startswith("end "):
            continue
        if "error stop" in code.lower():
            problems.append(
                "src/parquet_utils.f90:%d: `error stop` -- this module documents that nothing in "
                "it aborts, which is why it has no error scenarios. Report the condition through a "
                "result the caller can test, or the guarantee has to come off the page and out of "
                "the module header." % lineno
            )
        match = define_re.match(code)
        if match is None:
            continue
        found += 1
        if not re.search(r"\bpure\b", match.group("prefix"), re.IGNORECASE):
            problems.append(
                "src/parquet_utils.f90:%d: %s %s is not `pure` -- `pure` is what makes the "
                "module's \"nothing prints, nothing stops\" guarantee a compile-time fact rather "
                "than a promise, since Fortran forbids both inside a pure procedure."
                % (lineno, match.group("kind"), match.group("name"))
            )
    if found == 0:
        problems.append(
            "src/parquet_utils.f90: no procedure definitions matched -- this check has gone blind")
    return problems


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


#: The canonical order of `parquet_stats`' optional arguments, in the five blocks
#: `doc/pages/utilities/statistics.md` describes under "Optional arguments are in one fixed order".
#: TWO checks read this one copy (.claude/rules/testing.md, "Two checks needing the same list
#: derive it from one place"): `check_stats_optional_argument_order` enforces it against the
#: source, and `check_stats_optional_order_documented` compares it with the guide page that
#: publishes it. Keep the block boundaries -- the page names them, and the second check reads them.
STATS_OPTIONAL_ORDER = (
    # the OUTPUT prefix, declared before the inputs by pf_moments, pf_mode and pf_probit_fit
    ["n_valid", "mean", "variance", "stddev", "sem", "skewness", "kurtosis",
     "vsum", "vmin", "vmax", "count", "modes", "corr"]
    # the object-lifecycle pair, taken only by pf_stats' %compute, %init and %merge
    + ["retain", "consume"]
    # the RULE block: entries that say what the operation IS
    + ["sigma", "sigma_lower", "sigma_upper", "maxiters", "cenfunc", "stdfunc",
       "n_clipped", "keep", "converged", "right", "density", "prob"]
    # the MAIN sequence, which is what most procedures draw from
    + ["is_valid", "weights", "weight_type", "ddof", "bias", "excess", "skipnan",
       "method", "kind", "scale", "center", "out_valid", "n_null", "n_nan",
       "n_outside", "ok", "threads"]
    # the %print pair
    + ["unit", "name"]
)

#: `parquet_kde`'s own canonical sequence (feature_kde.md, 5.2): the estimator's settings, then the
#: queries' range and output arguments, then the `pf_*` family's population run from `is_valid` to
#: `threads` in the family's own order, then `%print`'s `unit`. A later stage's argument is inserted
#: where it belongs here, which is safe exactly when every existing procedure stays a subsequence.
KDE_OPTIONAL_ORDER = (
    ["bandwidth", "rule", "adjust", "kernel", "adaptive", "pilot", "alpha", "bandwidth_max",
     "spread_max", "hlo", "hhi", "lower", "upper", "boundary"]
    + ["xmin", "xmax", "cut", "x", "normalise", "stream"]
    + ["is_valid", "weights", "weight_type", "skipnan", "n_null", "n_nan", "n_outside",
       "n_overreach", "rule_used", "ok", "threads"]
    + ["method"]
    + ["finish"]
    + ["unit"]
)

#: Where each block of STATS_OPTIONAL_ORDER starts, by name, so the documentation check can slice
#: it without repeating the names. Derived positions, never hardcoded indices.
STATS_OPTIONAL_BLOCKS = ("n_valid", "retain", "sigma", "is_valid", "unit")



#: The only tolerance names a solver entry point may take. An ALLOW-LIST, so a new spelling fails
#: loudly rather than being absorbed: that is the safe direction for a list of this kind.
SOLVER_TOLERANCES = ("rtol", "atol", "ctol", "merge_tol")


def check_solver_vocabulary():
    """The four callback-driven solver modules share one vocabulary, and it is derived, not listed.

    `parquet_integrate`, `parquet_root`, `parquet_optimize` and `parquet_prima` take a function the
    caller supplies and work on it until a stopping rule fires. A caller who has learned one should
    have learned the shape of the others, and the names below are the shape. The design record is
    the solver-vocabulary feature document; the durable statement of the rule is
    `.claude/rules/api-conventions.md`, "Solver entry points".

    **The scope is DERIVED rather than enumerated**, so a solver entry point added later -- in these
    four modules or in a fifth -- is covered without editing this check. A solver entry point is a
    procedure declared in `src/*.f90` that has BOTH an optional `info` dummy of a `pf_*_info` type
    AND a `context` dummy. The `info` clause finds the solvers: the only `pf_*_info` types in the
    library are the three solver outcome types, and it catches `pf_minimize_multistart`, which has
    no `max_neval` of its own. `pf_local_run`, whose `info` is required rather than
    optional, is outside the scope this way, and falls under clause 5 alone.

    **Two further conditions keep the private WORKERS out**, because "public" in the Fortran sense
    is not the same as "public API" and both kinds of worker are reachable by the naive rule:

    * a `context` dummy. `finish_run` in `parquet_prima_common` is listed in a `public ::` so the
      three PRIMA drivers can call it, and it takes an optional `pf_optimize_info`; but it is an
      internal worker with no caller-facing context, and clause 3 would fail on it.
    * the `module` prefix on the declaration, or the name in a `public ::` of the same file. Every
      entry point is declared `module subroutine`/`module function` in its spec's interface block
      and restated the same way in its submodule. `integrate_impl`, the driver's own worker, is a
      plain `subroutine` inside a submodule's `contains` -- private to the submodule tree -- and
      takes both a `context` and an optional `pf_integrate_info`, so nothing else would exclude it.

    The clauses:

    1. a dummy whose name ends in `tol` is one of `rtol`, `atol`, `ctol`, `merge_tol`;
    2. `rtol` precedes `atol` where both exist;
    3. `converged` and `info` are present, in that order, and `context` is the LAST dummy;
    4. `max_neval`, where an entry point has one, is a default `integer`, `intent(in)`, `optional`;
    5. every abstract interface named by a `deferred` binding, in a module that declares a solver
       entry point, names its first dummy `self`. The plain-function interfaces have no passed
       object and are outside this clause.

    **It fails when it finds no solver entry point at all**, as
    `check_prima_sums_are_the_ordered_sum` does: a renamed type or a reworded declaration must not
    be able to turn this into a check that passes by reading nothing.

    **Verify it by breaking it, not by watching it pass.** Three mutations it must catch: `tol` back
    as the name of one `pf_find_root` specific's `atol`; `context` moved before `info` in one
    optimiser specific; `this` back in one abstract interface.
    """
    problems = []
    entry_points = 0
    interfaces_seen = 0

    info_decl = re.compile(
        r"type\s*\(\s*pf_\w*_info\s*\)(.*?)::\s*(.*)$", re.IGNORECASE)

    for path in sorted(SRC.glob("*.f90")):
        text = path.read_text()
        rel = path.name
        logical = _logical_lines(path)

        # Names this file declares public, for the scope test below.
        public_here = set()
        for _, code in logical:
            m = re.match(r"public\s*::\s*(.*)$", code, re.IGNORECASE)
            if m:
                public_here.update(n.strip().lower() for n in m.group(1).split(",") if n.strip())

        # ---- the solver entry points in this file ----
        declares_entry_point = False
        for index, (lineno, code) in enumerate(logical):
            m = PROC_DECL.match(code)
            if not m:
                continue
            name, arglist = m.group(1), m.group(2)
            args = [a.strip().lower() for a in arglist.split(",") if a.strip()]
            if "context" not in args:
                continue
            # the declaration block is every logical line up to this procedure's `end`
            body = []
            for _, later in logical[index + 1:]:
                if re.match(r"end\s+(subroutine|function)\b", later, re.IGNORECASE):
                    break
                body.append(later)
            has_optional_info = False
            for decl in body:
                d = info_decl.search(decl)
                if d and "optional" in d.group(1).lower() and "info" in d.group(2).lower():
                    has_optional_info = True
                    break
            if not has_optional_info:
                continue
            # A submodule-local worker is neither `module`-prefixed nor public; see the note above.
            if not code.lower().startswith("module ") and name.lower() not in public_here:
                continue

            entry_points += 1
            declares_entry_point = True
            where = "%s:%d: %s" % (rel, lineno, name)

            # 1. the tolerance allow-list
            for arg in args:
                if arg.endswith("tol") and arg not in SOLVER_TOLERANCES:
                    problems.append(
                        "%s: takes `%s`; a solver entry point's tolerances are %s "
                        "(api-conventions.md, Solver entry points)"
                        % (where, arg, ", ".join("`%s`" % n for n in SOLVER_TOLERANCES)))

            # 2. rtol before atol
            if "rtol" in args and "atol" in args and args.index("rtol") > args.index("atol"):
                problems.append("%s: `atol` precedes `rtol`; the relative tolerance comes first"
                                % where)

            # 3. converged, then info, and context last
            for required in ("converged", "info"):
                if required not in args:
                    problems.append("%s: has no `%s` dummy; every solver entry point answers both "
                                    "the short way and the long way" % (where, required))
            if "converged" in args and "info" in args \
                    and args.index("converged") > args.index("info"):
                problems.append("%s: `info` precedes `converged`; the short answer comes first"
                                % where)
            if args[-1] != "context":
                problems.append("%s: `context` is not the last dummy (found `%s`)"
                                % (where, args[-1]))

            # 4. the budget's kind
            if "max_neval" in args:
                budget = [d for d in body if re.search(r"::\s*max_neval\b", d, re.IGNORECASE)]
                if not budget:
                    problems.append("%s: takes `max_neval` but declares no such dummy" % where)
                else:
                    decl = budget[0]
                    head = decl.split("::")[0].lower()
                    if not re.match(r"\s*integer\s*(,|$)", head):
                        problems.append("%s: `max_neval` is not a DEFAULT integer (%s)"
                                        % (where, head.strip()))
                    for attr in ("intent(in)", "optional"):
                        if attr not in head.replace(" ", ""):
                            problems.append("%s: `max_neval` is not `%s`" % (where, attr))

        # ---- 5. the passed object of every deferred binding's interface ----
        if not declares_entry_point:
            continue
        deferred = set(re.findall(r"procedure\s*\(\s*(\w+)\s*\)\s*,\s*deferred",
                                  text, re.IGNORECASE))
        for _, code in logical:
            m = PROC_DECL.match(code)
            if not m or m.group(1).lower() not in {d.lower() for d in deferred}:
                continue
            interfaces_seen += 1
            first = [a.strip().lower() for a in m.group(2).split(",") if a.strip()]
            if first and first[0] != "self":
                problems.append(
                    "%s: abstract interface `%s` names its passed object `%s`; an extension has to "
                    "repeat that name, so it is `self` (api-conventions.md, Solver entry points)"
                    % (rel, m.group(1), first[0]))

    if entry_points == 0:
        problems.append("no solver entry point found in src/*.f90: this check has come up empty -- "
                        "re-aim it before trusting its silence")
    if interfaces_seen == 0:
        problems.append("no deferred-binding interface found beside a solver entry point: "
                        "this check has come up empty -- re-aim it before trusting its silence")
    return problems


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
    #
    # `corr` closes the tier-A OUTPUT prefix and `prob` closes the RULE run, and both positions
    # are load-bearing rather than alphabetical:
    #
    # * `corr` is `pf_probit_fit`'s optional third output, declared -- like every output-prefix
    #   name here -- ahead of the whole population block, so it goes at the end of that block
    #   rather than beside the other real64 optionals further down.
    # * `prob` is `pf_probit_scale`'s tuning, declared BEFORE `is_valid`, so it belongs at the
    #   head of the population block and NOT next to `method`. `method` sits after `is_valid`,
    #   `weights`, `weight_type` and `skipnan`; putting `prob` there would make
    #   `pf_probit_scale`'s own list non-monotone and this check would fail on it. The precedent
    #   is the sigma-clip block above: the tuning a caller reaches for comes before the
    #   population plumbing.
    #
    # Neither name was taken by any existing procedure when it was inserted, so every signature
    # that passed before still passes -- which is the condition an insertion has to meet.
    bad = []
    # `parquet_kde` keeps the same contract with a sequence of its own (KDE_OPTIONAL_ORDER), whose
    # population run is the family's; the same subsequence rule is applied to its spec file.
    for path, canonical, matrix in ((SRC / "parquet_stats.f90", STATS_OPTIONAL_ORDER,
                                     "feature_pandas_S4.md's signature matrix"),
                                    (SRC / "parquet_kde.f90", KDE_OPTIONAL_ORDER,
                                     "feature_kde.md, section 5.2")):
        bad += _optional_order_problems(path, canonical, matrix)
    return bad


def _optional_order_problems(path, canonical, matrix):
    """The subsequence rule of `check_stats_optional_argument_order`, over one spec file."""
    rank = {name: i for i, name in enumerate(canonical)}
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
                       "there (and to %s) or rename them"
                       % (proc, ", ".join(unknown), matrix))
            continue
        # Declaration order need not match the argument list, so compare against the dummy order.
        in_arg_order = [nm for nm in args if nm in optional]
        ranks = [rank[nm] for nm in in_arg_order]
        if ranks != sorted(ranks):
            bad.append("%s: optionals appear as (%s); the canonical order is (%s)"
                       % (proc, ", ".join(in_arg_order),
                          ", ".join(sorted(in_arg_order, key=lambda nm: rank[nm]))))
    if seen == 0:
        bad.append("src/%s: no `module subroutine`/`module function` declaration "
                   "was recognised -- this check has gone blind and is passing vacuously"
                   % path.name)
    return bad



#: How `doc/pages/utilities/statistics.md` introduces each block of `STATS_OPTIONAL_ORDER`:
#: (block's first name, the pattern that finds the block's own name list on the page, a name the
#: prose appends to that list rather than listing inline). The fenced main sequence has no prose
#: anchor and is found by the code fence itself.
STATS_DOC_BLOCKS = (
    ("n_valid", r"\*output\* prefix\s*`([^`]+)`", "corr"),
    ("retain", r"object-lifecycle pair\s*`([^`]+)`", None),
    ("sigma", r"the rule block\s*`([^`]+)`", None),
    ("is_valid", None, None),
    ("unit", r"the `(unit)`/`(name)` pair", None),
)


def _stats_optional_blocks():
    """`STATS_OPTIONAL_ORDER` cut into its five blocks, keyed by each block's first name."""
    starts = [STATS_OPTIONAL_ORDER.index(n) for n in STATS_OPTIONAL_BLOCKS]
    bounds = list(zip(starts, starts[1:] + [len(STATS_OPTIONAL_ORDER)]))
    return {STATS_OPTIONAL_BLOCKS[i]: STATS_OPTIONAL_ORDER[a:b]
            for i, (a, b) in enumerate(bounds)}


def check_stats_optional_order_documented():
    """The guide's canonical optional-argument sequence must match the one the checker enforces.

    `doc/pages/utilities/statistics.md`, under "Optional arguments are in one fixed order", writes
    out all **46** of `parquet_stats`' optional arguments in order -- a fenced main sequence plus
    four blocks named in the prose around it. `STATS_OPTIONAL_ORDER` is the same sequence, and
    `check_stats_optional_argument_order` enforces it against `src/parquet_stats.f90`. Nothing
    compared the two, so the page could drift from the rule it publishes.

    **Why this is worth a check.** The page says why itself: "In Fortran the order of optional
    arguments is part of the public contract -- a caller may pass them positionally -- so this is a
    compatibility promise, not a style preference." A reader who orders a positional call by a
    stale page writes a call that compiles and means something else. That is `feature_doc.md`
    section 8's FALSE-making class rather than U3's merely-incomplete one.

    **Order, not membership.** Unlike `check_agg_vocabulary_matches_its_documentation`, whose token
    vocabularies are sets, this list IS its order: it is compared block by block, element by
    element, and a reordering inside any block is a finding. Set equality over all 46 is checked
    too, so a name added to one side and not the other is caught even if it lands at a block edge.

    The scan refuses to pass when a block's anchor stops matching, when a block comes back empty,
    or when the page is missing: all three mean it has gone blind rather than that the page is
    clean (CLAUDE.md, "A static check that enumerates names goes stale silently").
    """
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "statistics.md"
    if not page.is_file():
        return ["check_stats_optional_order_documented: %s is missing -- this check has gone blind"
                % page.name]
    text = page.read_text()
    head = "## Optional arguments are in one fixed order"
    if head not in text:
        return ["doc/pages/utilities/statistics.md: the heading %r is gone, so this check can no "
                "longer find the sequence it compares. Restore it or update this check." % head]
    start = text.index(head)
    rest = text.find("\n## ", start + 1)
    section = text[start:rest if rest != -1 else len(text)]

    # The fenced block is the MAIN sequence; everything else is prose around it.
    fence = re.search(r"```\n(.*?)```", section, re.S)
    if fence is None:
        return ["doc/pages/utilities/statistics.md: the fenced canonical sequence under %r is "
                "gone -- this check has gone blind on the main block" % head]
    fenced_names = re.findall(r"[a-z_0-9]+", fence.group(1))
    prose = re.sub(r"\s+", " ", section[:fence.start()] + section[fence.end():])

    want = _stats_optional_blocks()
    problems, got = [], {}
    for first, pattern, tail in STATS_DOC_BLOCKS:
        if pattern is None:                       # the fenced main sequence
            got[first] = fenced_names
            continue
        m = re.search(pattern, prose)
        if m is None:
            problems.append(
                "doc/pages/utilities/statistics.md: the %r block's anchor no longer matches, so "
                "this check can no longer see it. Either restore the wording or update "
                "STATS_DOC_BLOCKS; do not leave it unmatched." % first)
            continue
        names = [n for g in m.groups() for n in re.findall(r"[a-z_0-9]+", g)]
        if tail is not None:
            if re.search(r"`%s` closes that block" % tail, prose) is None:
                problems.append(
                    "doc/pages/utilities/statistics.md: the %r block no longer says that `%s` "
                    "closes it, so this check cannot place `%s`." % (first, tail, tail))
            else:
                names = names + [tail]
        got[first] = names
    if problems:
        return problems

    for first in want:
        if not got.get(first):
            problems.append("doc/pages/utilities/statistics.md: the %r block came back empty -- "
                            "this check has gone blind on it" % first)
        elif got[first] != want[first]:
            problems.append(
                "doc/pages/utilities/statistics.md: the %r block disagrees with "
                "STATS_OPTIONAL_ORDER (order matters -- a caller may pass these positionally).\n"
                "    page: %s\n    src : %s"
                % (first, ", ".join(got[first]), ", ".join(want[first])))
    if problems:
        return problems

    flat = [n for first in STATS_OPTIONAL_BLOCKS for n in got[first]]
    if set(flat) != set(STATS_OPTIONAL_ORDER):
        only_page = sorted(set(flat) - set(STATS_OPTIONAL_ORDER))
        only_src = sorted(set(STATS_OPTIONAL_ORDER) - set(flat))
        problems.append(
            "doc/pages/utilities/statistics.md: the documented sequence and STATS_OPTIONAL_ORDER "
            "do not name the same arguments.\n    only on the page: %s\n    only in the source: %s"
            % (", ".join(only_page) or "-", ", ".join(only_src) or "-"))
    return problems


def check_stats_weights_documented():
    """The guide's two `weights` lists must name exactly the procedures the source gives one.

    `doc/pages/utilities/statistics.md`, under "Which procedures take `weights`, and why the rest
    do not", writes out both sides of a partition of `parquet_stats`' generics: the ones that take
    a `weights` dummy and the ones that deliberately do not. Both read as exhaustive -- the
    section's own heading promises "the rest" -- and a reader uses the pair to decide whether a
    call will compile.

    **Why this is worth a check.** It had drifted by FOUR procedures and stayed wrong for three
    weeks: `pf_probit_mean` and `pf_probit_scale` take `weights` and were on neither list, and
    `pf_normal_scores` and `pf_probit_fit` take none and were on neither either. The page's own
    prose describes all four correctly in their own sections, so the enumeration disagreed with
    the page around it and nothing compared either with the source. That is CLAUDE.md's
    "A static check that enumerates names goes stale silently", met in the wild.

    **Membership, not order**, unlike `check_stats_optional_order_documented` next door: these are
    two sets, and the page groups them for reading rather than in any fixed sequence.

    Two shapes the page uses and this check must tolerate, both present today: the "does not" side
    names `pf_corr(method="spearman")`, which is a token of an already-listed generic rather than
    a generic of its own, and one row names two procedures at once. So every `pf_*` name is read
    out of each side and anything that is not a generic of `parquet_stats` is ignored --
    `pf_corr` included on that side, since it appears on both for different reasons.

    **Verify this check by breaking it, not by watching it pass**: delete one name from either
    list and confirm it fails before trusting a green run. It refuses to pass when either anchor
    stops matching, when either side comes back empty, or when the page or the spec is missing,
    since all of those mean it has gone blind rather than that the page is clean.
    """
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "statistics.md"
    spec = SRC / "parquet_stats.f90"
    if not page.is_file():
        return ["check_stats_weights_documented: %s is missing -- this check has gone blind"
                % page.name]
    if not spec.is_file():
        return ["check_stats_weights_documented: %s is missing -- this check has gone blind"
                % spec.name]
    text = page.read_text()
    head = "### Which procedures take `weights`, and why the rest do not"
    if head not in text:
        return ["doc/pages/utilities/statistics.md: the heading %r is gone, so this check can no "
                "longer find the lists it compares. Restore it or update this check." % head]
    start = text.index(head)
    rest = text.find("\n### ", start + 1)
    if rest == -1:
        rest = text.find("\n## ", start + 1)
    section = text[start:rest if rest != -1 else len(text)]

    # The two sides, each introduced by its own bold lead-in. "Takes" runs to the "Does not"
    # lead-in; "does not" runs to the end of its table, which the next blank line after the last
    # table row ends.
    marks = [("takes", r"\*\*Takes `weights`\*\*"), ("lacks", r"\*\*Does not, and the argument")]
    spans, problems = {}, []
    for (name, pattern), nxt in zip(marks, marks[1:] + [(None, None)]):
        m = re.search(pattern, section)
        if m is None:
            problems.append(
                "doc/pages/utilities/statistics.md: the %r list's anchor no longer matches, so "
                "this check can no longer see it. Either restore the wording or update "
                "check_stats_weights_documented; do not leave it unmatched." % name)
            continue
        end = len(section)
        if nxt[1] is not None:
            m2 = re.search(nxt[1], section)
            if m2 is not None:
                end = m2.start()
        spans[name] = section[m.end():end]
    if problems:
        return problems

    # Every generic of the module, and which of them declares a `weights` dummy. The spec is the
    # authority; the interface bodies are read with their continuations joined.
    body = re.sub(r"&\s*\n\s*", "", spec.read_text())
    args = dict((m.group(1), [a.strip() for a in m.group(2).split(",")])
                for m in re.finditer(r"module (?:subroutine|function) (\w+)\(([^)]*)\)", body))
    generic, current = {}, None
    for line in spec.read_text().splitlines():
        g = re.match(r"\s*interface (pf_\w+)", line)
        if g:
            current = g.group(1)
        p = re.match(r"\s*module procedure (\w+)", line)
        if p and current and p.group(1) in args:
            generic.setdefault(current, set()).add("weights" in args[p.group(1)])
    if not generic:
        return ["check_stats_weights_documented: no `pf_*` generic was recognised in %s -- this "
                "check has gone blind" % spec.name]
    mixed = sorted(g for g, v in generic.items() if len(v) > 1)
    if mixed:
        return ["src/parquet_stats.f90: %s declare `weights` on some specifics and not others, "
                "which the guide's two lists cannot express. Say so on the page and teach this "
                "check the exception." % ", ".join(mixed)]
    src_takes = set(g for g, v in generic.items() if True in v)
    src_lacks = set(generic) - src_takes

    # The "takes" side is one prose list and every name in it is a claim. The "does not" side is a
    # TABLE, and only its first column names procedures -- the `why` column legitimately cites
    # others (`pf_cumsum(w*x)`, and `pf_probit_scale`, which does take them), so reading the whole
    # row would make the page contradict itself for saying why.
    documented = {"takes": set(re.findall(r"`(pf_\w+)", spans["takes"]))}
    first_cells = []
    for line in spans["lacks"].splitlines():
        line = line.strip()
        if not line.startswith("|") or set(line) <= set("|-: "):
            continue
        first_cells.append(line.strip("|").split("|")[0])
    documented["lacks"] = set(re.findall(r"`(pf_\w+)", " ".join(first_cells)))
    for name in ("takes", "lacks"):
        documented[name] = set(n for n in documented[name] if n in generic)
        if not documented[name]:
            problems.append("doc/pages/utilities/statistics.md: the %r list came back empty -- "
                            "this check has gone blind on it" % name)
    if problems:
        return problems

    # `pf_corr` is named on both sides on purpose (Pearson takes weights, Spearman refuses them),
    # so it is judged by the source alone and excused from the "listed twice" test below.
    both = (documented["takes"] & documented["lacks"]) - {"pf_corr"}
    if both:
        problems.append("doc/pages/utilities/statistics.md: %s appear on BOTH `weights` lists"
                        % ", ".join(sorted(both)))
    for name, want in (("takes", src_takes), ("lacks", src_lacks)):
        got = documented[name] if name == "takes" else documented[name] - {"pf_corr"}
        want = want if name == "takes" else want - {"pf_corr"}
        missing = sorted(want - got)
        extra = sorted(got - want)
        if missing or extra:
            problems.append(
                "doc/pages/utilities/statistics.md: the %r `weights` list disagrees with "
                "src/parquet_stats.f90.\n    on the page and not in the source: %s\n"
                "    in the source and not on the page: %s"
                % (name, ", ".join(extra) or "-", ", ".join(missing) or "-"))
    return problems


def check_kde_merge_refusals_documented():
    """The guide's two `%merge` refusal lists must name exactly what `%merge` refuses.

    `pf_kde_grid%merge` refuses another grid that differs in any of a fixed set of settings, one
    `kde_abort` per difference in `src/parquet_kde_grid.f90`. `doc/pages/utilities/kernel-density.md`
    writes that set out TWICE -- once in the "One grid per thread" prose bullet and once in the
    "What aborts" table's `%merge` row -- and both read as exhaustive.

    **Why this is worth a check.** The set grows whenever a grid gains a setting, and that is
    exactly how it drifted: `method` was added to the grid, `%merge` learned to refuse a differing
    one, and NEITHER list was updated. The page carried seven of the eight differences for as long
    as the binned method has existed, in two places, with nothing comparing either to the source.
    CLAUDE.md, "A static check that enumerates names goes stale silently".

    **Membership, not order.** The source tests the differences in a fixed sequence, but a reader
    uses the lists to answer "will this merge be refused?", which is a set question.

    **The narrowest part of the page that carries the claim**, per the campaign's SD9: the prose
    sentence is read only as far as its first parenthesis, because the parenthetical that follows
    legitimately names `alpha` and `bandwidth_max` -- settings a differing pilot is detected
    THROUGH, not differences `%merge` reports by name. Reading the whole sentence would make the
    page contradict itself. The table row is read as its message plus the `(or ...)` parenthetical
    that continues it, and nothing else in the row.

    **Verify this check by breaking it, not by watching it pass**: delete a token from either list
    and confirm it fails before trusting a green run. It refuses to pass when either anchor stops
    matching, when either list comes back empty, or when the page or the source is missing, since
    all of those mean it has gone blind rather than that the page is clean.
    """
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "kernel-density.md"
    src = SRC / "parquet_kde_grid.f90"
    for f in (page, src):
        if not f.is_file():
            return ["check_kde_merge_refusals_documented: %s is missing -- this check has gone "
                    "blind" % f.name]

    want = set(re.findall(r'"the two grids differ in ([a-z_]+)"', src.read_text()))
    if not want:
        return ["check_kde_merge_refusals_documented: found no \"the two grids differ in ...\" "
                "aborts in %s -- this check has gone blind" % src.name]

    text = page.read_text()
    problems = []

    # -- site 1: the prose bullet, read only as far as its first parenthesis ----------------------
    lead = "refuses a grid that differs in its"
    if lead not in text:
        problems.append("doc/pages/utilities/kernel-density.md: the sentence %r is gone, so this "
                        "check can no longer find the prose list. Restore it or update this "
                        "check." % lead)
    else:
        start = text.index(lead)
        stop = text.find("(", start)
        prose = text[start:stop if stop != -1 else start + 400]
        got = set(re.findall(r"`([a-z_]+)`", prose))
        if not got:
            problems.append("doc/pages/utilities/kernel-density.md: the `%merge` prose list came "
                            "back empty -- this check has gone blind")
        else:
            problems += _kde_merge_diff("the prose bullet", got, want)

    # -- site 2: the abort table's %merge row ----------------------------------------------------
    row = None
    for line in text.splitlines():
        if line.startswith("|") and "the two grids differ in" in line:
            row = line
            break
    if row is None:
        problems.append("doc/pages/utilities/kernel-density.md: the `%merge` row of the aborts "
                        "table is gone, so this check can no longer find the table list. Restore "
                        "it or update this check.")
    else:
        # the token inside the quoted message, plus every code span of the "(or ...)" tail
        got = set(re.findall(r"the two grids differ in ([a-z_]+)", row))
        got |= set(re.findall(r"`([a-z_]+)`", row[row.find("(or "):] if "(or " in row else ""))
        if not got:
            problems.append("doc/pages/utilities/kernel-density.md: the `%merge` table list came "
                            "back empty -- this check has gone blind")
        else:
            problems += _kde_merge_diff("the aborts table row", got, want)

    return problems


def check_integrate_aborts_documented():
    """The abort table of `integration.md` must carry exactly the messages `validate_call` aborts with.

    `pf_integrate` refuses every call it cannot answer from one procedure, `validate_call`
    (`src/parquet_integrate_driver.f90`), as a run of `integrate_abort` calls in a fixed order.
    `doc/pages/utilities/integration.md`'s "What aborts" table writes every one of those messages
    out verbatim, and a reader uses it as the complete list of what is refused.

    **Why this is worth a check.** Both sides are machine-readable and the page's copy is exact
    text, so a message reworded in the source, or a refusal added, leaves the table wrong with
    nothing comparing the two. The campaign has already met one abort table that had drifted
    (`kernel-density.md`, one row missing for as long as the binned method had existed).
    CLAUDE.md, "A static check that enumerates names goes stale silently".

    **The narrowest part of the page that carries the claim**, per the campaign's SD9: the MESSAGE
    column only. The condition column is prose about when the refusal fires and legitimately names
    things the message does not (`50*epsilon`, `huge(1)/42`, "two finite bounds").

    **The `internal:` aborts are not in the table and must not be**: they guard against a code the
    engine cannot return and a tolerance pair the driver cannot build, neither reachable by a
    caller, so they are excluded here by prefix rather than listed on a user-facing page.

    **Verify this check by breaking it, not by watching it pass**: reword a message on either side
    and confirm it fails before trusting a green run. It refuses to pass when the table heading is
    gone, when either list comes back empty, or when the page or the source is missing.
    """
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "integration.md"
    src = SRC / "parquet_integrate_driver.f90"
    for f in (page, src):
        if not f.is_file():
            return ["check_integrate_aborts_documented: %s is missing -- this check has gone "
                    "blind" % f.name]

    want = {m for m in re.findall(r'integrate_abort\("([^"]+)"', src.read_text())
            if not m.startswith("internal:")}
    if not want:
        return ["check_integrate_aborts_documented: found no caller-facing integrate_abort "
                "messages in %s -- this check has gone blind" % src.name]

    text = page.read_text()
    heading = "## What aborts"
    if heading not in text:
        return ["doc/pages/utilities/integration.md: the %r heading is gone, so this check can no "
                "longer find the abort table. Restore it or update this check." % heading]

    got = set()
    for line in text[text.index(heading):].splitlines():
        if not line.startswith("|"):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if len(cells) != 2 or cells[-1].startswith("---"):
            continue
        cell = cells[-1]
        if cell.startswith("`") and cell.endswith("`"):
            got.add(cell[1:-1])

    if not got:
        return ["doc/pages/utilities/integration.md: the abort table's message column came back "
                "empty -- this check has gone blind"]

    extra = sorted(got - want)
    missing = sorted(want - got)
    if not extra and not missing:
        return []
    return ["doc/pages/utilities/integration.md: the abort table disagrees with validate_call in "
            "src/parquet_integrate_driver.f90.\n    on the page and not aborted by the source: %s\n"
            "    aborted by the source and not on the page: %s"
            % ("; ".join(extra) or "-", "; ".join(missing) or "-")]


def check_interpolate_aborts_documented():
    """The two abort tables of `interpolation.md` must carry the messages `interp_abort` builds.

    `parquet_interpolate` refuses a call it cannot answer through one helper, `interp_abort`, from
    `interp_1d_build` (`src/parquet_interpolate_1d.f90`) and `interp_2d_build`
    (`src/parquet_interpolate_2d.f90`). `doc/pages/utilities/interpolation.md`'s "What aborts"
    section writes both runs of messages out, sixteen rows for a table and seventeen for a grid, and
    a reader uses them as the complete list of what is refused.

    **Why this is worth a check.** Thirty-three rows of exact text, hand-kept, against two sources
    that can be reworded independently; the campaign has already met one abort table that had
    drifted (`kernel-density.md`, one row missing for as long as the binned method had existed).
    CLAUDE.md, "A static check that enumerates names goes stale silently".

    **Why this is not `check_integrate_aborts_documented` again.** That one compares whole string
    literals, which works because `integrate_abort` is handed a complete message. Here a message is
    BUILT -- `"is_valid has "//trim(interp_i2s(n))//" elements for "...` -- so the page's cells carry
    `<m>` placeholders where a runtime value goes, and no whole literal exists to compare. This
    check compares the LITERAL FRAGMENTS around those values instead: it joins each call's adjacent
    `//` literals into fragments, splits each page cell on its placeholders, and asks that the
    fragments appear in the cell in order. A fragment list is strictly more general than one
    literal, so this shape would also cover `integration.md` if the two are ever merged.

    **The narrowest part of the page that carries the claim**, per the campaign's SD9: the MESSAGE
    column of the two tables, and within a cell only its first code span -- the message template.
    The Refused column is prose about when a refusal fires, and a cell's later spans are prose about
    the variants ("or the same of `y`"), neither of which the source owns.

    **Why the axis is normalised.** A grid checks `x` and then `y` with the same message, and the
    page deliberately writes the `x` form out and says "or the same of `y`" rather than repeating
    seventeen rows. Requiring the `y` text verbatim would be requiring the page to say something it
    has decided not to say, so both sides have their axis words folded to `x` before they are
    compared. Folding does NOT blind the check to a reworded `y` message, which was the worry and
    was measured: rewording only `y must be finite` to `y has to be finite` no longer matches the
    fold, so the fragment is looked for as it stands and is reported. What survives the fold is a
    `y` message reworded into some OTHER message the page already documents, which takes crossing
    the two axes deliberately.

    **Verify this check by breaking it, not by watching it pass**: reword a message on either side
    and confirm it fails before trusting a green run. It refuses to pass when the heading is gone,
    when either side comes back empty, or when a file is missing.
    """
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "interpolation.md"
    srcs = [SRC / "parquet_interpolate_1d.f90", SRC / "parquet_interpolate_2d.f90"]
    for f in [page] + srcs:
        if not f.is_file():
            return ["check_interpolate_aborts_documented: %s is missing -- this check has gone "
                    "blind" % f.name]

    # `<h>`, `"<token>"` (the quotes come from `interp_quote`, not from a literal), and a rendered
    # number such as `6.704E+153`, which `interp_r2s` produces at run time.
    PLACEHOLDER = r'"?<[a-z_]+>"?|[0-9]\.[0-9]{3}E[+-][0-9]+'

    def fold_axis(text):
        """The y-axis spelling of every paired message, folded onto its x twin."""
        for a, b in ((" along y ", " along x "), ("y must be ", "x must be "),
                     ("y has a spacing of ", "x has a spacing of "), ("rescale y", "rescale x")):
            text = text.replace(a, b)
        return text

    # ---- the source side: each interp_abort call's literal fragments, in order ----
    want = set()
    for src in srcs:
        text = src.read_text()
        # Join continuations so a message split over several lines is one string to scan.
        joined = re.sub(r"&\s*\n\s*", "", text)
        for call in re.findall(r"interp_abort\((.*?)\n", joined):
            # Drop the first argument (the entry point) and keep what follows.
            body = call.split(",", 1)[1] if "," in call else ""
            frags, cur = [], ""
            # A quoted literal continues the current fragment; anything else between two literals
            # is a runtime value and ends it.
            for piece in re.findall(r'"[^"]*"|\'[^\']*\'|[^"\']+', body):
                if len(piece) >= 2 and piece[0] == piece[-1] and piece[0] in "\"'":
                    cur += piece[1:-1]
                elif piece.strip(" /&\n") == "":
                    continue          # just the // between two literals
                else:
                    if cur:
                        frags.append(cur)
                    cur = ""
            if cur:
                frags.append(cur)
            frags = [fold_axis(f) for f in frags if f.strip()]
            if frags:
                want.add(tuple(frags))
    if not want:
        return ["check_interpolate_aborts_documented: found no interp_abort messages in "
                "src/parquet_interpolate_{1d,2d}.f90 -- this check has gone blind"]

    # ---- the page side: the first code span of each message cell ----
    text = page.read_text()
    heading = "## What aborts"
    if heading not in text:
        return ["doc/pages/utilities/interpolation.md: the %r heading is gone, so this check can no "
                "longer find the abort tables. Restore it or update this check." % heading]

    section = fold_axis(text[text.index(heading):])
    cells = []
    for line in text[text.index(heading):].splitlines():
        if line.startswith("##") and heading not in line:
            break
        if not line.startswith("|"):
            continue
        parts = [c.strip() for c in line.strip().strip("|").split("|")]
        if len(parts) != 2 or parts[-1].startswith("---") or parts[-1] == "Message":
            continue
        span = re.match(r"`([^`]+)`", parts[-1])
        if span:
            cells.append(fold_axis(span.group(1)))
    if not cells:
        return ["doc/pages/utilities/interpolation.md: the abort tables' message column came back "
                "empty -- this check has gone blind"]

    def covers(frags, cell):
        """Every fragment present in `cell`, in order."""
        at = 0
        for f in frags:
            at = cell.find(f, at)
            if at < 0:
                return False
            at += len(f)
        return True

    problems = []
    # Source to page, over the whole section: `pf_interp`'s mismatched query sizes are documented in
    # the prose under the tables rather than in a row, which documents them just as well.
    for frags in sorted(want):
        if not covers(frags, section):
            problems.append("    aborted by the source and nowhere in the section: %s"
                            % "<...>".join(frags))
    # Page to source, over the table rows only: that column is what the source owns. The pieces are
    # joined with nothing, because a source fragment is exactly the text between two runtime values.
    for cell in cells:
        pieces = [p for p in re.split(PLACEHOLDER, cell) if p.strip()]
        if not any(covers(tuple(pieces), "".join(frags)) for frags in want):
            problems.append("    on the page and aborted by neither source: %s" % cell)
    if not problems:
        return []
    return ["doc/pages/utilities/interpolation.md: the abort tables disagree with the interp_abort "
            "calls in src/parquet_interpolate_1d.f90 and src/parquet_interpolate_2d.f90.\n"
            + "\n".join(problems)]



def check_optimize_aborts_documented():
    """Both optimisation pages must name the arguments and the limits their refusals are about.

    `parquet_optimize` and `parquet_prima` refuse a call they cannot answer through one helper each
    -- `optimize_abort` (`src/parquet_optimize_support.f90`) and `prima_abort`, reached as
    `abort_here` or `state_abort` (`src/parquet_prima_common.f90`). `optimization.md` lists those
    refusals in the "Caller mistakes abort too" bullet of "Important behaviour" and `prima.md` in
    the bullet list under "When a call is refused"; a reader uses each as the complete account of
    what is refused.

    **Why this is worth a check.** The U6 review found two refusals missing from `prima.md`'s list
    and one from `optimization.md`'s, with nothing comparing either side to the source.
    CLAUDE.md, "A static check that enumerates names goes stale silently".

    **Why this is not `check_interpolate_aborts_documented` again.** That one compares message TEXT,
    which works because `interpolation.md` carries the messages in a table. These two pages
    deliberately do not: they paraphrase, in prose, so that the list reads as advice rather than as
    a transcript. There is no text to compare. What the two sides DO share is names and numbers, so
    this check compares those:

    1. **The subject of a refusal.** Every abort message of the shape `<name> must ...` names the
       argument it is about, and `validate_tolerance` is handed its argument's name as a literal.
       Each such name, where it is one of the arguments the two modules' `pf_minimize_*` interfaces
       declare, must appear in that page's refusal list -- so a reader scanning for `max_neval` or
       `rtol` finds the refusal that governs it.
    2. **The limit a refusal states.** The tail after `must be in`, `must not exceed`,
       `by more than` or `must be at least` is the bound itself -- `(0, 2]`, `huge(1)/2`,
       `[n+2, (n+1)(n+2)/2]`, `2*epsilon` -- and must appear on the page. Not only in the refusal
       list: a bound is often stated where its argument is described, which documents it just as
       well.
    3. **The reverse.** Every argument-shaped code span in a page's refusal list must be an argument
       or a binding the two modules actually declare, so a renamed or deleted one is caught.

    **What this check does NOT cover, deliberately.** A refusal whose message names no argument --
    "at least one variable is required", "the start point must not contain NaN", the refusal of a
    `pf_constrained_objective` -- has no mechanical handle on the page's prose, and inventing one
    would mean an enumerated list of paraphrases, which goes stale exactly the way the page does.
    Those are what step 2 of a page review is for. The check is a floor, not a ceiling.

    **The narrowest part of the page that carries the claim**, per the campaign's SD9: the refusal
    list alone for directions 1 and 3. The whole page for direction 2, because a bound belongs
    wherever its argument is explained.

    **Verify this check by breaking it, not by watching it pass**: drop an argument from a page's
    refusal list, rename one in a message, or change a stated bound, and confirm it fails before
    trusting a green run. It refuses to pass when a heading or bullet is gone, when either side
    comes back empty, or when a file is missing.
    """
    pages = {
        "optimize": REPO_ROOT / "doc" / "pages" / "utilities" / "optimization.md",
        "prima": REPO_ROOT / "doc" / "pages" / "utilities" / "prima.md",
    }
    specs = {
        "optimize": SRC / "parquet_optimize.f90",
        "prima": SRC / "parquet_prima.f90",
    }
    sources = {
        "optimize": sorted(SRC.glob("parquet_optimize*.f90")),
        "prima": sorted(SRC.glob("parquet_prima*.f90")),
    }
    # The two files holding the abort helpers are named, not globbed: a glob that comes back one
    # file short reads as a tier with fewer refusals, which is exactly how this check would go
    # blind without saying so.
    helpers = [SRC / "parquet_optimize_support.f90", SRC / "parquet_prima_common.f90"]
    for f in list(pages.values()) + list(specs.values()) + helpers:
        if not f.is_file():
            return ["check_optimize_aborts_documented: %s is missing -- this check has gone blind"
                    % f.name]
    for tier, files in sources.items():
        if not files:
            return ["check_optimize_aborts_documented: no %s sources found -- this check has gone "
                    "blind" % tier]

    # ---- the names a page may use: the two modules' documented call surface ----
    #
    # Derived, never listed: the dummies of every `pf_minimize_*` generic, the components of every
    # `pf_local_solver` extension, and the bindings of the two objective types. Single-character
    # names (`a`, `b`, `x`, `f`) are dropped -- they match inside ordinary English ("a linear
    # constraint must not ...") and would make direction 1 fire on prose.
    arguments = set()
    bindings = set()
    for spec in specs.values():
        text = spec.read_text()
        # Every specific of every entry point, whether it sits inside a generic
        # (`minimize_bobyqa_obj`) or stands alone (`pf_minimize_cobyla`, which takes an object and
        # only an object and so has no generic to be a specific of).
        for block in re.findall(r"\n        module subroutine (?:pf_)?minimize_\w+\(.*?"
                                r"\n        end subroutine", text, re.S):
            for line in block.split("\n"):
                hit = re.match(r"\s*(?:class|type|real|integer|logical|character|procedure)\b"
                               r"[^:]*::\s*([a-z_][a-z0-9_]*)", line)
                if hit:
                    arguments.add(hit.group(1))
        for block in re.findall(r"\n    type, extends\(pf_local_solver\) :: \w+\n(.*?)\n    contains",
                                text, re.S):
            for line in block.split("\n"):
                hit = re.match(r"\s*(?:real|integer|logical)\b[^:]*::\s*([a-z_][a-z0-9_]*)", line)
                if hit:
                    arguments.add(hit.group(1))
        for line in text.split("\n"):
            hit = re.match(r"\s*procedure\([a-z_]+\), deferred\s*::\s*([a-z_][a-z0-9_]*)", line)
            if hit:
                bindings.add(hit.group(1))
    arguments = {n for n in arguments if len(n) > 1}
    if not arguments:
        return ["check_optimize_aborts_documented: no pf_minimize_* arguments were found in the "
                "two spec files -- this check has gone blind"]
    # A PARTIAL loss blinds it as thoroughly as an empty set, and reads as a tier with fewer
    # arguments. Every entry point the two modules make public must have an interface block here.
    public_entries, read_entries = set(), set()
    for spec in specs.values():
        text = spec.read_text()
        for line in text.split("\n"):
            if line.strip().startswith("public ::"):
                public_entries |= {n.strip() for n in line.split("::", 1)[1].split(",")
                                   if n.strip().startswith("pf_minimize_")}
        read_entries |= set(re.findall(r"\n    interface (pf_minimize_\w+)\n", text))
        read_entries |= set(re.findall(r"module subroutine (pf_minimize_\w+)\(", text))
    if public_entries != read_entries:
        return ["check_optimize_aborts_documented: %d of the %d public pf_minimize_* entry points "
                "have an interface block this check can read (%s) -- it has gone blind"
                % (len(read_entries), len(public_entries),
                   ", ".join(sorted(public_entries ^ read_entries)))]

    # ---- the source side: each tier's abort messages, their subjects and their limits ----
    LIMIT_PHRASES = (" must be in ", " must not exceed ", " by more than ", " must be at least ")

    def messages_of(files):
        found = set()
        for path in files:
            text = re.sub(r"&\s*\n\s*&?", "", path.read_text())
            for call in re.finditer(r"\b(?:optimize_abort|prima_abort|abort_here|state_abort)\s*\(",
                                    text):
                at, depth, body = call.end(), 1, ""
                while at < len(text) and depth:
                    ch = text[at]
                    if ch == '"':
                        close = text.index('"', at + 1)
                        body += text[at:close + 1]
                        at = close + 1
                        continue
                    if ch == "(":
                        depth += 1
                    elif ch == ")":
                        depth -= 1
                        if not depth:
                            break
                    body += ch
                    at += 1
                literals = re.findall(r'"([^"]*)"', body)
                # The first literal of an `optimize_abort`/`prima_abort` call is the entry point.
                if literals and literals[0].startswith(("pf_", "parquet_")):
                    literals = literals[1:]
                text_of = "".join(literals)
                if " " in text_of:
                    found.add(text_of)
            # `validate_tolerance` is handed the argument's own name, so its message has no
            # subject of its own to read.
            for hit in re.finditer(r"validate_tolerance\(\s*\"[^\"]*\"\s*,\s*\"([a-z_][a-z0-9_]*)\"",
                                   text):
                found.add(hit.group(1) + " must be a finite, non-negative number")
        return found

    problems = []
    for tier in ("optimize", "prima"):
        msgs = messages_of(sources[tier])
        if not msgs:
            return ["check_optimize_aborts_documented: found no abort messages in the %s sources "
                    "-- this check has gone blind" % tier]

        page_text = pages[tier].read_text()
        if tier == "prima":
            heading = "## When a call is refused"
            if heading not in page_text:
                return ["doc/pages/utilities/prima.md: the %r heading is gone, so this check can no "
                        "longer find the refusal list. Restore it or update this check." % heading]
            # The BULLET LIST alone, not the prose under it: the paragraphs that follow are
            # about what PRIMA adjusts where this library refuses, and they name upstream's own
            # `honour_x0` and its sentinel, which the source does not declare as arguments.
            lines, started = [], False
            for line in page_text[page_text.index(heading) + len(heading):].splitlines():
                if line.startswith("- "):
                    started = True
                elif started and not line.startswith(("  ", "- ")):
                    break
                if started:
                    lines.append(line)
            section = "\n".join(lines)
        else:
            opener = "- **Caller mistakes abort too**"
            if opener not in page_text:
                return ["doc/pages/utilities/optimization.md: the %r bullet is gone, so this check "
                        "can no longer find the refusal list. Restore it or update this check."
                        % opener]
            tail = page_text[page_text.index(opener):]
            end = tail.find("\n- ", 1)
            section = tail if end < 0 else tail[:end]
        if not section.strip():
            return ["check_optimize_aborts_documented: the %s page's refusal list came back empty "
                    "-- this check has gone blind" % tier]

        # 1. every argument a refusal is about, named in the refusal list
        subjects = set()
        limits = set()
        for msg in msgs:
            head = msg.split(" must ", 1)[0] if " must " in msg else ""
            subjects |= {w for w in re.findall(r"[a-z_][a-z0-9_]*", head) if w in arguments}
            for phrase in LIMIT_PHRASES:
                if phrase in msg:
                    limits.add(msg.split(phrase, 1)[1].rstrip(".").strip())
        if not subjects:
            return ["check_optimize_aborts_documented: no refusal of the %s tier named one of its "
                    "own arguments -- this check has gone blind" % tier]
        for name in sorted(subjects):
            if not re.search(r"(?<![a-z0-9_])%s(?![a-z0-9_])" % re.escape(name), section):
                problems.append("    %s: `%s` is refused by the source and is not named in the "
                                "page's refusal list" % (pages[tier].name, name))

        # 2. every limit a refusal states, somewhere on that page
        for limit in sorted(limits):
            if len(limit) > 1 and limit not in page_text:
                problems.append("    %s: the source refuses against %r and the page never states it"
                                % (pages[tier].name, limit))

        # 3. every argument-shaped code span in the refusal list must be a real name
        for span in re.findall(r"`([^`]+)`", section):
            if not re.fullmatch(r"[a-z][a-z0-9_]+", span):
                continue
            if span.startswith(("pf_", "parquet_")):
                continue
            if span not in arguments and span not in bindings:
                problems.append("    %s: the refusal list names `%s`, which is not an argument or "
                                "a binding either module declares" % (pages[tier].name, span))

    if problems:
        return ["the optimisation pages' refusal lists have drifted from what the source refuses "
                "(check_optimize_aborts_documented):\n" + "\n".join(sorted(set(problems)))]
    return []

def check_root_aborts_documented():
    """The abort table of `root-finding.md` must carry `pf_find_root`'s messages, IN ORDER.

    `pf_find_root` refuses every call it cannot answer from one procedure, `validate_call`
    (`src/parquet_root_solve.f90`), as a run of `root_abort` calls in a fixed order, and aborts
    once more from `evaluate` when the caller's function returns a NaN.
    `doc/pages/utilities/root-finding.md`'s "What aborts" table writes every one of those messages
    out verbatim, and a reader uses it as the complete list of what is refused.

    **Why this is worth a check, and why it checks ORDER.** The source's own header says "The
    validation order is the order of the guide page's table", so the table claims to be the order a
    caller who breaks two contracts at once meets them in -- and the U7 review found it was not:
    the page listed `rtol` above `atol` while `validate_call` checks `atol` first. Set equality
    would have passed that page. Order is the narrowest reading of the claim the source makes, so
    it is what is compared. CLAUDE.md, "A static check that enumerates names goes stale silently".

    **The NaN row is the one exception, and the page says so** ("checked before the first
    evaluation -- bar the last row, which is about your function"): it is aborted from `evaluate`,
    not from `validate_call`, so it is required to be exactly the LAST row rather than to sit in
    the validation run.

    **The narrowest part of the page that carries the claim**, per the campaign's SD9: the MESSAGE
    column only. The Condition column is prose about when a refusal fires and legitimately names
    things the message does not (`b - a` overflows, "a limit NaN or infinite").

    **Why this is not `check_integrate_aborts_documented` again.** That one compares an unordered
    SET of whole literals, and `integrate_abort` is handed each message on one line. Here two
    messages are split across a continuation with `//`, so the literals are joined first, and the
    comparison is a sequence rather than a set.

    **Verify this check by breaking it, not by watching it pass**: reword a message on either side,
    delete a row, or swap two rows, and confirm it fails before trusting a green run. It refuses to
    pass when the heading is gone, when either procedure can no longer be found, when either side
    comes back empty, or when the page or the source is missing.
    """
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "root-finding.md"
    src = SRC / "parquet_root_solve.f90"
    for f in (page, src):
        if not f.is_file():
            return ["check_root_aborts_documented: %s is missing -- this check has gone blind"
                    % f.name]

    # Join continuations so a message split over two lines is one string to scan.
    joined = re.sub(r"&\s*\n\s*", "", src.read_text())

    def messages_of(unit, kind):
        """Every `root_abort` message inside one named program unit, in source order."""
        m = re.search(r"\n *%s +%s\(.*?\n *end +%s +%s\b" % (kind, unit, kind, unit),
                      joined, re.S)
        if not m:
            return None
        out = []
        for call in re.findall(r"root_abort\((.*?), *context\)", m.group(0), re.S):
            # Adjacent literals joined by `//` are one message.
            out.append("".join(re.findall(r'"([^"]*)"', call)))
        return out

    want = messages_of("validate_call", "subroutine")
    if want is None:
        return ["check_root_aborts_documented: no validate_call subroutine in %s -- this check has "
                "gone blind" % src.name]
    nan = messages_of("evaluate", "subroutine")
    if nan is None:
        return ["check_root_aborts_documented: no evaluate subroutine in %s -- this check has gone "
                "blind" % src.name]
    if not want:
        return ["check_root_aborts_documented: found no root_abort messages in validate_call (%s) "
                "-- this check has gone blind" % src.name]
    if len(nan) != 1:
        return ["check_root_aborts_documented: expected exactly one root_abort in evaluate (%s), "
                "found %d -- this check has gone blind" % (src.name, len(nan))]
    want = want + nan

    text = page.read_text()
    heading = "## What aborts"
    if heading not in text:
        return ["doc/pages/utilities/root-finding.md: the %r heading is gone, so this check can no "
                "longer find the abort table. Restore it or update this check." % heading]

    got = []
    for line in text[text.index(heading):].splitlines():
        if line.startswith("##") and heading not in line:
            break
        if not line.startswith("|"):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if len(cells) != 2 or cells[-1].startswith("---") or cells[-1] == "Message":
            continue
        cell = cells[-1]
        if cell.startswith("`") and cell.endswith("`"):
            got.append(cell[1:-1])

    if not got:
        return ["doc/pages/utilities/root-finding.md: the abort table's message column came back "
                "empty -- this check has gone blind"]
    if got == want:
        return []

    problems = []
    for m in sorted(set(got) - set(want)):
        problems.append("    on the page and aborted by neither procedure: %s" % m)
    for m in sorted(set(want) - set(got)):
        problems.append("    aborted by the source and not on the page: %s" % m)
    if not problems:
        # Same messages, wrong sequence: name the first row that is out of place.
        for i, (g, w) in enumerate(zip(got, want), start=1):
            if g != w:
                problems.append("    row %d of the table is %r, but the source refuses %r there. "
                                "The table's order is the validation order (the header of %s says "
                                "so), so a caller breaking two contracts at once meets them in "
                                "this order." % (i, g, w, src.name))
                break
    return ["doc/pages/utilities/root-finding.md: the abort table disagrees with the root_abort "
            "calls in src/parquet_root_solve.f90.\n" + "\n".join(problems)]


def check_integrate_status_codes_documented():
    """The status table of `integration.md` must list exactly the `PF_INT_*` codes the module has.

    `src/parquet_integrate.f90` declares seven status codes and exports all seven;
    `doc/pages/utilities/integration.md`'s "Budget and outcome" table gives one row per code, and
    `solvers.md` sends a reader here for the full set.

    **Why this is worth a check.** An eighth code, or a renamed one, leaves the page short with
    nothing comparing it to the source -- the page reads as exhaustive and nothing makes it so.
    CLAUDE.md, "A static check that enumerates names goes stale silently".

    **The narrowest part of the page that carries the claim**: the table's FIRST column. The other
    two are prose about what a code means and what to do, and a future row may well name another
    code there while explaining a distinction.

    **Verify this check by breaking it, not by watching it pass**: delete a row, or add a code to
    the module, and confirm it fails before trusting a green run. It refuses to pass when either
    list comes back empty or when the page or the source is missing.
    """
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "integration.md"
    src = SRC / "parquet_integrate.f90"
    for f in (page, src):
        if not f.is_file():
            return ["check_integrate_status_codes_documented: %s is missing -- this check has "
                    "gone blind" % f.name]

    want = set(re.findall(r"^\s*integer, parameter :: (PF_INT_[A-Z_]+) =", src.read_text(),
                          re.MULTILINE))
    if not want:
        return ["check_integrate_status_codes_documented: found no PF_INT_* parameters in %s -- "
                "this check has gone blind" % src.name]

    got = set()
    for line in page.read_text().splitlines():
        if not line.startswith("|"):
            continue
        cells = [c.strip() for c in line.strip().strip("|").split("|")]
        if len(cells) != 3:
            continue
        m = re.fullmatch(r"`(PF_INT_[A-Z_]+)`", cells[0])
        if m:
            got.add(m.group(1))

    if not got:
        return ["doc/pages/utilities/integration.md: the status table's code column came back "
                "empty -- this check has gone blind"]

    extra = sorted(got - want)
    missing = sorted(want - got)
    if not extra and not missing:
        return []
    return ["doc/pages/utilities/integration.md: the status table disagrees with "
            "src/parquet_integrate.f90.\n    on the page and not declared by the module: %s\n"
            "    declared by the module and not on the page: %s"
            % (", ".join(extra) or "-", ", ".join(missing) or "-")]


def _kde_merge_diff(where, got, want):
    """One side's disagreement with the source, reported in both directions."""
    extra = sorted(got - want)
    missing = sorted(want - got)
    if not extra and not missing:
        return []
    return ["doc/pages/utilities/kernel-density.md: %s disagrees with src/parquet_kde_grid.f90 "
            "about what `%%merge` refuses.\n    on the page and not refused by the source: %s\n"
            "    refused by the source and not on the page: %s"
            % (where, ", ".join(extra) or "-", ", ".join(missing) or "-")]


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

    doc/pages/operating/choosing-a-module.md and src/parquet.f90's own header both say
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
                "-- update this check and the two documents that name it -- or the facade is the "
                "only thing still exporting it, which the guide says it is not."]
    return ["src/: `parquet_get_version` is public from %s. It belongs to parquet_version alone: "
            "doc/pages/operating/choosing-a-module.md and src/parquet.f90's header both "
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
    until someone next asked for a release build. See `.claude/rules/fortran-gotchas.md`'s "General Fortran & language gotchas".
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
    `check_no_per_element_shared_ptr` above. The direct-copy paths it protects are pinned by
    `to_character matches %get element for element, padded with blanks` (test/test_parquet_string.f90).

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
    #
    # ONE narrowing, and it is written so that it lapses by itself. `toml_table` and `toml_array`
    # both carry a binding called `get` -- toml-f's, which hands back a POINTER and allocates
    # nothing -- so a file importing `tomlf` produces `call tbl%get(key, ptr)` hits that are not
    # this hazard at all. Such a file is skipped ONLY while it also has no `parquet_string` in it,
    # i.e. only while it provably cannot hold the storage this rule protects. The day
    # src/parquet_toml.f90 imports parquet_strings, the exemption stops applying on its own,
    # which an allowlist of file names could not do (CLAUDE.md, "A static check that enumerates
    # names goes stale silently").
    for path in sorted(SRC.glob("*.f90")):
        lines = path.read_text(encoding="utf-8", errors="replace").splitlines()
        # CODE only, never comments: src/parquet_toml.f90's own header cites `parquet_string` as
        # the precedent for its borrowed-handle lifetime rule, and a whole-text search would read
        # that prose as evidence the file handles string columns.
        code_text = "\n".join(strip_comment(ln) for ln in lines)
        if re.search(r"^\s*use\s+tomlf\b", code_text, re.M | re.I) \
                and not re.search(r"parquet_string", code_text, re.I):
            continue
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
                    "`elem_bounds` inside parquet_strings"
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

    The names are derived by SHAPE rather than from a list kept here: in each dispatch file the
    `select case (trim(scenario))` block is located and its `case ("...")` labels read until `case
    default`, so a new scenario is picked up with no edit (CLAUDE.md, "A static check that
    enumerates names goes stale silently"). An empty result from any of them is treated as a
    failure for the same reason -- it means that dispatch moved, not that the invariant holds.

    THE DISPATCH IS SPREAD OVER FIVE FILES: `error_scenarios.f90` (the control scenario only) and
    the four `error_scenarios_*.f90` group modules it walks in turn. Each file is read separately
    and the labels are unioned, and a name dispatched by two of them is reported: the program stops
    at the first group that claims a name, so the second copy would never run and nothing else
    would say so.

    `concurrency_scenarios=(...)` counts as listed too: those genuinely need a real OpenMP race and
    are deliberately excluded from priming, but they are still named in the script and still run.
    """
    dispatch_files = sorted(TEST.glob("error_scenarios*.f90"))
    runner = TOOLS / "run_error_scenarios.sh"
    if len(dispatch_files) < 2:
        return ["test/error_scenarios*.f90: expected the driver plus its group modules, found %d "
                "-- this check needs updating" % len(dispatch_files)]
    if not runner.is_file():
        return ["%s: not found -- this check needs updating" % runner.relative_to(REPO_ROOT)]

    dispatched, seen_in, problems = [], {}, []
    for path in dispatch_files:
        rel = path.relative_to(REPO_ROOT)
        lines = path.read_text(encoding="utf-8").splitlines()
        start = None
        for i, line in enumerate(lines):
            if re.match(r"\s*select case\s*\(\s*trim\(\s*scenario\s*\)\s*\)\s*$", line):
                start = i
                break
        if start is None:
            if path.stem == "error_scenarios_support":
                continue               # fixtures only, it dispatches nothing
            problems.append(
                "%s: no `select case (trim(scenario))` dispatch found -- the scenario dispatch "
                "moved or was renamed, so this check can no longer see it" % rel)
            continue
        here = []
        for line in lines[start + 1:]:
            if re.match(r"\s*case\s+default\s*$", line):
                break
            found = re.match(r'\s*case\s*\(\s*"([a-z0-9_]+)"\s*\)\s*$', line)
            if found:
                here.append(found.group(1))
        if not here:
            problems.append(
                "%s: the `select case (trim(scenario))` dispatch yielded no `case (\"...\")` "
                "labels -- its shape changed, so this check is now blind" % rel)
            continue
        for name in here:
            if name in seen_in:
                problems.append(
                    "scenario '%s' is dispatched by both %s and %s -- error_scenarios.f90 stops at "
                    "the first group that claims a name, so the second copy can never run"
                    % (name, seen_in[name], rel))
            else:
                seen_in[name] = rel
        dispatched += here
    if problems:
        return problems

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


def check_scenario_list_has_no_duplicates():
    """No scenario is named twice in run_error_scenarios.sh's two arrays.

    A repeat is invisible by inspection in a two-thousand-line array and costs a pointless extra
    process on every run, but the reason it is worth a check is that two counts stop agreeing:
    test_errors.f90's prime_error_scenarios writes ONE capture triple per scenario NAME, so a list
    with N entries and N-1 distinct names leaves N-1 files behind. tools/coverage.sh compares those
    two numbers to decide whether priming covered the list -- an undercount there silently sends it
    back to the eight-minute full replay, which is exactly the shape of failure that gets diagnosed
    as "the coverage script hangs".

    Both arrays are read, since a name repeated ACROSS them would run under two different sets of
    expectations.
    """
    runner = TOOLS / "run_error_scenarios.sh"
    if not runner.is_file():
        return ["%s: not found -- this check needs updating" % runner.relative_to(REPO_ROOT)]

    text = runner.read_text(encoding="utf-8")
    names = re.findall(r'"([a-z0-9_]+):[01]"', text)
    names += re.findall(r'^\s*"([a-z0-9_]+)"\s*$', text, re.M)
    if not names:
        return [
            "tools/run_error_scenarios.sh: neither scenarios=() nor concurrency_scenarios=() "
            "yielded a scenario name -- the arrays moved or changed shape, so this check is blind"
        ]

    seen = set()
    repeated = []
    for name in names:
        if name in seen and name not in repeated:
            repeated.append(name)
        seen.add(name)
    if not repeated:
        return []
    return [
        "tools/run_error_scenarios.sh: %d scenario name(s) listed more than once, so the entry "
        "count and the number of distinct scenarios disagree (tools/coverage.sh compares the "
        "latter against test_run/.primed): %s" % (len(repeated), ", ".join(repeated))
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
    """An `intent(inout)` setter must leave no component stale.

    `.claude/rules/fortran-gotchas.md`, "`class(t), intent(out)` is expensive per element", enforced
    for the temporal types per
    `.claude/rules/columns-tables.md`, "`parquet_temporal` and the containers".
    """
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
                "that does (.claude/rules/columns-tables.md, \"`parquet_temporal` and the containers\")."
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
    #
    # The anchor is REQUIRED, not best-effort. Falling back to the whole page on a miss is the
    # silent-widening failure CLAUDE.md records under "A static check that enumerates names goes
    # stale silently": measured on the page as it stands, the orientation is 2254 characters and
    # the whole page 6708, so a reworded sentence would triple the search area and defeat the very
    # restriction the comment above states, with no message and exit 0. Note the anchor deliberately
    # stops before the group count, which is the word most likely to change.
    cut = text.find("\nThe guide is organised into")
    if cut <= 0:
        return ["doc/pages/index.md: the orientation ends at the sentence beginning \"The guide is "
                "organised into\", and that sentence is gone -- this check cannot tell the "
                "orientation from the group descriptions any more and would silently start "
                "accepting a module named anywhere on the page; re-anchor it deliberately"]
    orientation = text[:cut]
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

    Settled while reviewing `doc/pages/utilities/random.md`, after being parked twice. Before it,
    six flat entries said more than their page's own `<h1>` did -- that page was titled "Random
    numbers" while its list entry said "Random numbers and sampling with pf_random_at", for a page
    half about sampling. The fix was to lengthen the titles, not to shorten the entries. (Cite the
    page and the decision, never a row number in a git-ignored campaign document: this docstring
    said "row 29 of feature_doc.md" for months, and by the time anyone read it that campaign was
    over and a later one's row 29 was a different page entirely.)

    **Backticks are ignored when comparing, and forbidden in a title.** A list entry legitimately
    carries them -- the flat list says "Compact string columns with `parquet_string_column`" for a
    page titled without them -- so the comparison strips them from both sides. The title itself may
    not: a `<title>` element is plain text, so a backtick reaches the browser tab literally. The
    convention was stated as fact in CLAUDE.md and in this docstring while nothing enforced it, and
    it had drifted -- `doc/pages/utilities/healpix.md` carried one, alone among the guide's pages,
    invisible to this check precisely because the comparison strips them first.
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
            if "`" in title_m.group(1):
                problems.append(
                    "%s: its frontmatter title: carries a backtick -- FORD puts the title in the "
                    "browser tab's plain-text <title>, where it shows literally. Write the name "
                    "bare; the list entries may keep their backticks, since the comparison below "
                    "strips them from both sides" % rel)
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
    """choosing-a-module.md's Files column must equal the measured footprints.

    The page's entry-module table has a Files column that is a MEASURED quantity:
    `tools/check_module_footprints.sh` builds a throwaway consumer per module and diffs the result
    against `tools/module_footprints.txt`. That script never reads the page, and until this check
    existed nothing else did either -- so the number a reader uses to choose an import was
    maintained by hand against a file the build already measures. (The
    row count is deliberately not written down here: the check derives the rows itself, and a count
    in a docstring beside a table that grows is the very hazard the paragraph below is about. This
    one said "twelve-row" while both tables carried twenty.)

    It had already gone wrong. A review of `choosing-a-module.md` found **3 of the 11 rows then
    present** wrong -- `parquet_io` 43 -> 44, `parquet_tables` 64 -> 62, `parquet` 65 -> 66 -- and
    fixed them by hand. README carried a second copy of the same table at the time and was not in
    that review's plan at all; it was caught only because the copy existed, and the copy has since
    been removed as duplication. The counts move whenever a `use` line is added anywhere in the
    library, which is the change least likely to prompt anyone to open the page. (Cite the page and
    the decision, never a row number in a git-ignored campaign document -- this said "Row 22a", and
    by the time anyone read it that campaign was over and a later one's row 22 was a different
    page.)

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
    # One page carries the table today. Kept as a mapping so a second copy, should one ever be
    # wanted again, is checked against the MEASUREMENT rather than against the first copy -- two
    # tables agreeing on a wrong number is exactly how the three rows above went unnoticed.
    tables = {
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


def _standalone_mentions(text, name, universe):
    """How often `name` appears in `text` other than as part of a longer name in `universe`."""
    total = text.count(name)
    for other in universe:
        if other != name and name in other:
            total -= text.count(other) * other.count(name)
    return total


def check_prose_footprint_counts():
    """A module's file count written in PROSE, on any guide page, must match the measurement.

    `check_module_tables_match_the_measured_footprints` above covers the entry-module TABLES on
    README.md and choosing-a-module.md, plus that page's one bolded `compiles **N** of this
    library's Fortran files` sentence. It looks at no other page and at no other phrasing, and that
    gap let three prose counts drift at once, in three different forms, from one cause -- the
    addition of `src/parquet_sorting_match.f90`, `parquet_sorting`'s eighth submodule:

      * `utilities/statistics.md`          "-- 27 of this library's Fortran files --"   (28)
      * `operating/choosing-a-module.md`   "the whole 21-file graph"                    (22)
      * `utilities/sorting.md`             "against 21 for `use parquet_sorting`"       (22)

    None was caught by anything. The tables on two other pages said 22 throughout, so the guide
    disagreed with itself in four places while every check stayed green -- and `statistics.md` was
    not in the campaign that found this at all. It was reached only by cross-checking every figure
    in the guide at once, which is what this now does on every run.

    **What it matches, and why not more.** A phrasing regex over the three forms above is
    impossible -- they share no wording -- so the anchor is a number IMMEDIATELY followed by a
    file-word (`22 files`, `21-file`, `28 of this library's Fortran files`), attributed to any
    entry module named on the same line, and reported only when it is already within
    `WINDOW` of that module's true count.

    **Both halves of that are load-bearing, and a looser version was tried first.** Scanning every
    integer on a line naming a module reported 19 violations on a correct tree -- version strings,
    years, and the `8` in "took `use parquet_sampling` from 24 files to 8". The window is what
    separates a stale count from an unrelated number in the same sentence, and it has to stay
    small: at 5 a wrong `27` beside two modules was attributed to both.

    **Two things it therefore does NOT catch**, stated so nobody reads a pass as proof:

      * a count not adjacent to a file-word -- `utilities/sorting.md`'s "against 22 for
        `use parquet_sorting`" is checked by nothing, and was one of the three that drifted;
      * a count whose module is not named in its own SENTENCE. The same page's "That import
        compiles **4** of this library's Fortran files" is about `parquet_argsort`, which the
        sentence never names -- verified by breaking it to 5 and watching this check pass;
      * a count that has drifted by more than `WINDOW`. These figures move by one or two when a
        file is added, which is how all three above went wrong, so that is the right trade -- and
        a large drift on a TABLE is still caught exactly by the sibling check.

    All three limits were established by control, not by reading: reintroducing each of the three
    real defects makes this fire, and the two forms listed above leave it silent.
    """
    problems = []
    measured = _footprint_counts()
    if not measured:
        return ["tools/module_footprints.txt: parsed no sections -- this check needs updating"]
    WINDOW = 3
    # A number immediately followed by a file-word. The published forms all match.
    claim = re.compile(r"\b(\d{1,3})(?:-file\b|\s+(?:of\s+this\s+library's\s+)?"
                       r"(?:Fortran\s+)?files?\b)")
    pages = sorted((REPO_ROOT / "doc" / "pages").rglob("*.md")) + [REPO_ROOT / "README.md"]
    seen = 0
    for path in pages:
        label = path.relative_to(REPO_ROOT).as_posix()
        # **Scanned per PARAGRAPH, not per line, and that is not a refinement.** The guide is
        # hard-wrapped, so a claim routinely straddles a line break -- statistics.md's read
        # "28 of this library's" / "Fortran files" across two lines, and a per-line version of
        # this check silently matched nothing there while passing. Table rows are dropped first:
        # they are the sibling check's business and are exact there.
        para, first = [], 0
        blocks = []
        for lineno, line in enumerate(path.read_text().splitlines(), 1):
            if line.lstrip().startswith("|") or not line.strip():
                if para:
                    blocks.append((first, " ".join(para)))
                para, first = [], 0
                continue
            if not para:
                first = lineno
            para.append(line.strip())
        if para:
            blocks.append((first, " ".join(para)))
        for lineno, para_text in blocks:
            # **Scoped to the SENTENCE, not the paragraph.** A paragraph routinely names several
            # modules, and choosing-a-module.md has one naming three: "taking it from
            # `parquet_sorting` cost the whole sorting graph ... took `use parquet_sampling` from
            # 24 files to 8". At paragraph scope that 24 is within the window of
            # `parquet_sorting`'s 22 and reports a defect in correct prose.
            for text in re.split(r"(?<=\.)\s+", para_text):
                # **A module name that only occurs INSIDE a longer one is not named here.**
                # `parquet_cosmology` is a substring of `parquet_cosmology_config`, so a sentence
                # stating the latter's count was reported as a wrong count for the former -- a
                # false positive on correct prose, which is how a check gets switched off. The
                # occurrences a longer name accounts for are subtracted; what is left is how often
                # the module is named in its own right.
                named = [m for m in measured
                         if _standalone_mentions(text, m, measured) > 0]
                if not named:
                    continue
                for m in claim.finditer(text):
                    stated = int(m.group(1))
                    for mod in named:
                        truth = measured[mod]
                        if stated == truth:
                            seen += 1
                        elif abs(stated - truth) <= WINDOW:
                            problems.append(
                                "%s:%d: `%s` is described as %d file(s); "
                                "tools/module_footprints.txt measures %d. A `use` line added "
                                "anywhere in the library moves these counts, and prose carrying "
                                "one is not covered by the table checks. Correct it, or reword to "
                                "drop the number if the sentence describes a past state -- %s"
                                % (label, lineno, mod, stated, truth, text[:80]))
    if seen == 0:
        problems.append("no guide prose states a correct module file count -- either the wording "
                        "changed everywhere or this check needs updating")
    return problems


#: The three searches `doc/pages/utilities/sorting.md` names COLLECTIVELY rather than by name, in
#: its "take no `threads=` at all" sentence: "`pf_permute`, `pf_is_sorted`, the three searches,
#: `pf_minmax`, `pf_argminmax` and `pf_merge`". They are written out here rather than dropped from
#: the comparison, so that a FOURTH search arriving fails this check instead of disappearing behind
#: the phrase. If the page ever names them individually this constant becomes redundant, not wrong.
SORTING_THREADS_PROSE_SEARCHES = ("pf_lower_bound", "pf_upper_bound", "pf_equal_range")

#: Number words the "Those <n> are the whole list" sentence may spell its count with. The sentence
#: has always used a word rather than a digit; digits are accepted too so that changing the style
#: does not silently blind the check.
SORTING_THREADS_NUMBER_WORDS = {
    "eight": 8, "nine": 9, "ten": 10, "eleven": 11, "twelve": 12, "thirteen": 13,
    "fourteen": 14, "fifteen": 15, "sixteen": 16, "seventeen": 17, "eighteen": 18,
    "nineteen": 19, "twenty": 20, "twenty-one": 21, "twenty-two": 22,
}


def check_sorting_threads_inventory_documented():
    """`sorting.md`'s two `threads=` lists must partition `parquet_sorting`'s generics exactly.

    The page tells a reader which operations parallelise, and it does so as a closed inventory --
    fourteen generics take `threads=`, eight take none, and "**passing one is a compile error
    rather than a silently ignored argument**". That last clause is why this list is worth a check
    where a merely incomplete one would not be: a new `pf_*` generic gaining `threads=` does not
    leave the page vague, it leaves the page stating something false about code a reader is about
    to write. `pf_value_counts` and `pf_remap` were added to `parquet_sorting` in one release and
    the sentence's count went from twelve to fourteen by hand; nothing would have reported it had
    it not.

    **What is compared.** Every `interface pf_<name>` block in `src/parquet_sorting.f90` and
    `src/parquet_argsort.f90` is resolved to its `module procedure` specifics, each specific's
    interface body is found, and its dummy list is tested for a `threads` argument. That partitions
    the generics into three sets, and all three are load-bearing:

      * **takes** -- every specific has it. Must equal the names the page lists before its
        "Those <n> are the whole list" sentence, and `<n>` must be how many there are.
      * **none** -- no specific has it. Must equal the names in the sentence after it, plus
        `SORTING_THREADS_PROSE_SEARCHES` above.
      * **mixed** -- some specifics have it and some do not. **Must be empty**, and this is the
        direction a two-way check would miss. The page's claim is about a GENERIC; a generic whose
        specifics disagree makes "the whole list" meaningless in both directions at once, because
        the same call spelling would accept `threads=` for one element type and reject it for
        another. No such generic exists today and none should.

    `pf_argsort` is the reason two files are read: its intrinsic-type specifics live in the
    `parquet_argsort` tier and its five extension types in `parquet_sorting`, so a one-file scan
    would report the generic on half its specifics and could call a genuinely mixed one clean.

    **Why the page is parsed by REGION rather than by sentence.** The paragraph naming the
    fourteen is interrupted by a sentence about run detection that re-mentions five of them, and
    the four selection operations are named in a separate sentence after that. Any per-sentence
    rule would have to know which sentences count. The regions are delimited by three literal
    anchors, each asserted present -- losing one blinds the check, so each is reported by name
    rather than quietly yielding an empty scan.

    **What it does not do.** It says nothing about whether an operation SHOULD thread, nor about
    `threads=`'s meaning, nor about the two non-`pf_` entries the same paragraph names
    (`parquet_open_reader(..., sort_by=)` and `parquet_table%sort_by`) -- those are not generics in
    these files and are outside what this can see. A name in a region that is not a known generic
    is ignored rather than reported, so ordinary prose may mention anything it likes.
    """
    problems = []
    sources = ["src/parquet_sorting.f90", "src/parquet_argsort.f90"]
    generics, dummies = {}, {}
    for rel in sources:
        path = REPO_ROOT / rel
        if not path.exists():
            return ["%s: not found -- this check has gone blind" % rel]
        text = path.read_text()
        for block in re.finditer(r'^\s*interface (pf_\w+)\s*\n(.*?)^\s*end interface',
                                 text, re.S | re.M):
            generics.setdefault(block.group(1), set()).update(
                re.findall(r'module procedure (\w+)', block.group(2)))
        # Continuations folded first: an interface body's dummy list routinely spans several lines.
        folded = re.sub(r'&\s*\n\s*', ' ', text)
        for body in re.finditer(r'module\s+(?:subroutine|function)\s+(\w+)\s*\(([^)]*)\)', folded):
            dummies[body.group(1)] = {a.strip().lower() for a in body.group(2).split(",")}
    if not generics:
        return ["src/parquet_sorting.f90: found no `interface pf_*` blocks -- this check has "
                "gone blind"]

    takes, takes_none, mixed = set(), set(), set()
    for name, specs in sorted(generics.items()):
        missing = sorted(s for s in specs if s not in dummies)
        if missing:
            problems.append(
                "src/parquet_sorting.f90: `%s` names specific(s) %s whose interface body this "
                "check could not find, so its `threads=` status is unknown. The interface-body "
                "shape this parses has changed, and the check needs updating rather than the page."
                % (name, ", ".join(missing[:3])))
            continue
        flags = {"threads" in dummies[s] for s in specs}
        (takes if flags == {True} else takes_none if flags == {False} else mixed).add(name)
    for name in sorted(mixed):
        with_it = sorted(s for s in generics[name] if "threads" in dummies[s])
        without = sorted(s for s in generics[name] if "threads" not in dummies[s])
        # The minority is named, whichever side it is on: that is the specific to look at.
        odd = without if len(without) <= len(with_it) else with_it
        problems.append(
            "src/parquet_sorting.f90: `%s` has specifics that disagree about `threads=` -- %d of "
            "%d take it, and the odd one(s) out are %s. doc/pages/utilities/sorting.md states the "
            "argument per GENERIC, so one call spelling would accept it for one element type and "
            "reject it for another. Give it to every specific or to none."
            % (name, len(with_it), len(generics[name]), ", ".join(odd[:3])))

    page_rel = "doc/pages/utilities/sorting.md"
    page_path = REPO_ROOT / page_rel
    if not page_path.exists():
        return problems + ["%s: not found -- this check has gone blind" % page_rel]
    page = page_path.read_text()
    opening = "**Sorting is parallel by default.**"
    closing = "take no `threads=` at all"
    total = re.search(r'Those ([\w-]+) are the whole list\.', page)
    for label, found in (("the `%s` paragraph opening" % opening, opening in page),
                         ('the "Those <n> are the whole list." sentence', total is not None),
                         ('the "%s" sentence' % closing, closing in page)):
        if not found:
            problems.append("%s: %s is gone, so this check can no longer locate the two lists it "
                            "compares. Re-anchor it on the new wording." % (page_rel, label))
    if problems:
        return problems

    listed = page[page.index(opening):total.start()]
    unlisted = page[total.end():page.index(closing)]

    def named(region):
        return {n for n in re.findall(r'pf_[a-z_]+', region) if n in generics}

    page_takes = named(listed)
    page_none = named(unlisted) | set(SORTING_THREADS_PROSE_SEARCHES)

    stated = SORTING_THREADS_NUMBER_WORDS.get(total.group(1).lower())
    if stated is None and total.group(1).isdigit():
        stated = int(total.group(1))
    if stated is None:
        problems.append("%s: \"Those %s are the whole list\" -- this check does not recognise that "
                        "count. Add it to SORTING_THREADS_NUMBER_WORDS."
                        % (page_rel, total.group(1)))
    elif stated != len(takes):
        problems.append(
            "%s: \"Those %s are the whole list\" claims %d operations take `threads=`; "
            "%d generics in src/ actually do. Correct the count together with the list."
            % (page_rel, total.group(1), stated, len(takes)))

    for name in sorted(takes - page_takes):
        problems.append("%s: `%s` takes `threads=` on every specific but is not named among the "
                        "operations that use the machine automatically -- and the page says "
                        "passing `threads=` to anything outside that list is a compile error."
                        % (page_rel, name))
    for name in sorted(page_takes - takes):
        problems.append("%s: `%s` is named as taking `threads=`, but no specific of it does."
                        % (page_rel, name))
    for name in sorted(takes_none - page_none):
        problems.append("%s: `%s` takes no `threads=` on any specific and is not named in the "
                        "\"%s\" sentence." % (page_rel, name, closing))
    for name in sorted(page_none - takes_none):
        problems.append("%s: `%s` is named as taking no `threads=`, but its specifics do take it."
                        % (page_rel, name))
    return problems



#: The two ways a guide page can spell an optional argument WRONGLY, as (pattern, what it looks
#: like, why it is banned). `.claude/rules/documentation.md` fixes one spelling, `%f(a, [b])`.
#: Both patterns are safe to ban outright: neither can occur in a Fortran array constructor
#: (`[1_int32, 2_int32]`), an interval (`[0, 1)`, `[lo, hi]`, `[first, last]`) or an Arrow type
#: string (`timestamp[ns,utc]`), which is what every other bracket on these pages is.
BRACKET_SPELLINGS = (
    (" [, ", "%f(a [, b])", "the comma belongs outside the bracket, as `%f(a, [b])`"),
    (",]", "%f(a, [b=,] [c=])", "the comma is INSIDE the bracket; write `%f(a, [b=], [c=])`"),
)


def check_bracket_convention():
    """A guide page writes an optional argument as `%f(a, [b])` and no other way.

    `.claude/rules/documentation.md` fixes one spelling for an optional argument in a written-out
    call form, with **the comma outside the bracket**. This check enforces it across the guide and
    README.md.

    **Why a check rather than the grep the rule used to name.** That grep was
    `grep -rn ' \\[, ' doc/pages/`, and it matches exactly one of the two ways to get this wrong.
    `doc/pages/utilities/spatial.md` carried thirteen occurrences of the other -- the comma inside
    the bracket, `[cell=,]` -- and declared the variant in its own bracket note, so it was
    internally consistent and invisible to the audit. The campaign read that grep's "0" as proof
    the convention was complete and recorded it as finished; it was not. A partial audit that
    reports success is worse than no audit, which is this repository's own rule about a static
    check that enumerates names.

    **This is a consistency rule, not a correctness one**, and it was proposed on that footing
    during a documentation review: a mis-spelled bracket makes a page inconsistent rather than
    false, which is the class such a review otherwise declines to act on. What carries it is that
    the convention consumed hand work in four consecutive review units and its documented audit
    was demonstrably partial.

    **Deliberately NOT scanned: `CHANGELOG.md`.** Its `[Unreleased]` section carries six
    occurrences of the old spelling. They are left alone because a changelog entry is governed by
    its own rules and `/review-doc` B8 allows only a factual correction there -- a notation change
    is not one. Widening this check to that file is a separate decision, and is left to one rather
    than taken here.
    """
    problems = []
    pages = sorted((REPO_ROOT / "doc" / "pages").rglob("*.md"))
    # Guard on the GUIDE specifically, not on the combined list. Appending README.md first and
    # then testing `if not targets` looks equivalent and is not: with doc/pages/ gone the list is
    # still non-empty, so the check would scan one file and report [ok] on a guide it can no
    # longer see. Found by mutation, which is the only way this shape is ever found.
    if not pages:
        return ["check_bracket_convention: no pages under doc/pages/ -- this check has gone blind "
                "on the guide; it scans doc/pages/**/*.md and README.md"]
    targets = pages
    readme = REPO_ROOT / "README.md"
    if readme.is_file():
        targets = targets + [readme]
    for path in targets:
        rel = path.relative_to(REPO_ROOT)
        for lineno, line in enumerate(path.read_text().split("\n"), start=1):
            for pattern, looks_like, why in BRACKET_SPELLINGS:
                if pattern in line:
                    problems.append(
                        "%s:%d: optional argument spelled `%s` -- %s\n    %s"
                        % (rel, lineno, looks_like, why, line.strip()[:100]))
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


#: Number words a guide page may open a counted list with, and what each counts. Deliberately
#: stops at twelve: past that a page should be using a table, and a longer word list would start
#: matching prose that merely begins with a number ("Twenty-four bits of ...").
COUNTED_LIST_WORDS = {
    "two": 2, "three": 3, "four": 4, "five": 5, "six": 6, "seven": 7,
    "eight": 8, "nine": 9, "ten": 10, "eleven": 11, "twelve": 12,
}

#: A counted list's lead-in: a line that IS a sentence opening with one of the words above and
#: ending in a colon, optionally opened with `**`. The colon and the bullet that must follow are
#: what keep this off ordinary prose.
COUNTED_LIST_LEAD = re.compile(
    r"^(?:\*\*)?(%s)\b.*:\s*$" % "|".join(COUNTED_LIST_WORDS), re.IGNORECASE)


def check_counted_lists_match_their_bullets():
    """A guide page that says "Four things follow:" must have four bullets under it.

    `.claude/rules/documentation.md`: "A count written beside the list it counts, or a list the
    code also owns, needs a check, not careful review". This is the first half of that rule -- the
    page owns BOTH halves, so unlike its sibling checks this one reads no source and cannot go
    stale against a moving target.

    Why it is worth a check rather than a careful read: drift here does not leave the page vague,
    it leaves it stating something false. "Four things follow" above five bullets tells a reader
    there are four and invites them to stop after the fourth. It had already drifted in three
    places when this was written -- `tables/table.md` ("Four things follow from strings having no
    fixed-width storage" above five), `tables/table-open.md` ("Five things to know" above six) and
    `utilities/statistics.md` ("Three things worth knowing" above four) -- none of them recent, and
    all three had survived every previous read of those pages.

    **What counts as a counted list.** A line that is a whole sentence, opens with a number word,
    and ends in a colon, whose next non-blank line starts a top-level `- ` bullet. That shape is
    unambiguous; two nearby shapes are deliberately NOT matched, because counting them needs
    per-site judgement and both are correct today:

      * a count above a TABLE ("Four calls control it explicitly"), whose rows are the list;
      * a count above prose ("Three name-taking calls are deliberately outside this"), where the
        items are named in the sentence itself.

    **A counted list is not necessarily one `<ul>`.** A fenced example between two bullets sits at
    column 0 -- `check_no_indented_code_fence` above requires exactly that -- which closes the list
    and opens a new one in the rendered HTML. `tables/table.md`'s five-bullet list renders as two
    `<ul>` blocks for this reason. So the scan continues across fences and indented continuations
    and stops only at a line that is neither, which is what makes the count match what a reader
    sees rather than what the HTML happens to nest.

    **Both Markdown bullet markers are accepted, and a list is counted with the one it opened
    with.** The guide uses `- ` almost everywhere, but `tables/table-open.md`'s "Three rules are
    worth knowing" opens with `* `, and a check that knew only `- ` skipped that list silently --
    found by the unit after the one that added this check, which is exactly the blindness the
    empty-scan guard below exists to prevent and did not catch, because the scan was not empty.
    Counting with the marker the list opened with, rather than either marker, keeps a `- ` line
    inside a `* ` list's continuation from being counted as a top-level bullet.

    The scan is by shape rather than by a page list, and an empty scan FAILS (CLAUDE.md, "A static
    check that enumerates names goes stale silently").
    """
    problems = []
    pages = sorted((REPO_ROOT / "doc" / "pages").glob("**/*.md"))
    if not pages:
        return ["doc/pages/: no .md pages found -- this check has gone blind on the guide"]
    checked = 0
    for page in pages:
        rel = page.relative_to(REPO_ROOT)
        lines = page.read_text().split("\n")
        in_fence = False
        for i, line in enumerate(lines):
            if line.startswith("```"):
                in_fence = not in_fence
                continue
            if in_fence:
                continue
            m = COUNTED_LIST_LEAD.match(line.strip())
            if not m:
                continue
            j = i + 1
            while j < len(lines) and lines[j].strip() == "":
                j += 1
            marker = next((mk for mk in ("- ", "* ") if lines[j].startswith(mk)), None)
            if marker is None:
                continue      # a count above a table, a code block or prose: not this shape
            checked += 1
            said = COUNTED_LIST_WORDS[m.group(1).lower()]
            got, k, fenced = 0, j, False
            while k < len(lines):
                s = lines[k]
                if s.startswith("```"):
                    fenced = not fenced
                elif fenced or s.strip() == "" or s.startswith((" ", "\t")):
                    pass      # a fenced example, a blank line or a bullet's continuation
                elif s.startswith(marker):
                    got += 1
                else:
                    break     # ordinary prose at column 0: the list has ended
                k += 1
            if said != got:
                problems.append(
                    "%s:%d: says %d but %d bullets follow -- fix whichever is wrong, and note "
                    "that a bullet added at the end of a list is the usual cause:\n    %s"
                    % (rel, i + 1, said, got, line.strip())
                )
    if not checked:
        return ["doc/pages/: no counted lists found -- this check has gone blind on the guide"]
    return problems


#: Matches a procedure declaration in `src/`, after `_logical_lines` has folded its `&`
#: continuations. The prefixes are the ones this project actually uses; a new one silently drops
#: that procedure out of `check_documented_signatures_match_source`'s comparison set rather than
#: causing a false failure, which is why the check also refuses an empty scan.
PROC_DECL = re.compile(
    r"^(?:module\s+)?(?:pure\s+|elemental\s+|impure\s+|recursive\s+)*"
    r"(?:subroutine|function)\s+([a-z_0-9]+)\s*\((.*?)\)", re.IGNORECASE)


def _documented_call_signatures(text):
    """Yield (name, args, offset) for every `call NAME(...)` on a page carrying a `[optional]`.

    The `[` is the discriminator between a SIGNATURE and a call EXAMPLE, and it is exact: a
    signature written under `.claude/rules/documentation.md`'s bracket convention always has at
    least one optional argument in brackets, and a runnable example never contains a bracket (the
    same rule says so). Without it, `generated-tables.md`'s
    `call parquet_write_table(t, "out.parquet")` -- a two-argument example of a fifteen-argument
    procedure -- is a false positive.

    Fortran `&` continuations are folded first, so the fenced multi-line form counts the same as a
    single-line one. Both forms are in the guide and neither is preferred here.
    """
    folded = re.sub(r"&\s*\n\s*", " ", text)
    for m in re.finditer(r"call ([a-z_0-9]+)\(", folded):
        depth, start = 0, m.end() - 1
        close = -1
        for j in range(start, len(folded)):
            if folded[j] == "(":
                depth += 1
            elif folded[j] == ")":
                depth -= 1
                if depth == 0:
                    close = j
                    break
        if close < 0:
            continue          # an unbalanced span: not a signature we can read
        args = [a.strip() for a in folded[start + 1:close].split(",") if a.strip()]
        if not any(a.startswith("[") for a in args):
            continue          # a call example, not a signature
        yield m.group(1).lower(), [a.strip("[]").lower() for a in args], m.start()


def check_documented_signatures_match_source():
    """A call form written out on a guide page must name the procedure's own arguments, in order.

    `doc/pages/tables/table-write.md` writes `parquet_write_table`'s **fifteen** argument names out
    by hand, and `parquet_open_writer_like`'s and `parquet_open_table_writer`'s thirteen each,
    under a heading saying "this is the one place they appear together". Nothing but this compares
    them with the source. A renamed or reordered argument leaves the page telling a reader to type
    a keyword that does not exist, or to expect one at a position it no longer occupies.

    **Where this sits on the campaign's own rule** (`feature_doc.md` section 8): drift makes the
    page affirmatively wrong rather than merely incomplete, which is the class worth checking --
    but the resulting failure is LOUD, since a wrong keyword will not compile. That makes it the
    weaker half of that class, and it was proposed and approved on that footing rather than on a
    silent-failure argument. What carries it is breadth: the scan is guide-wide, so every page that
    adopts the bracket convention is covered from the day it does, at no extra cost.

    **What is compared.** Every `call NAME(...)` span on any `doc/pages/**/*.md` page that carries
    at least one `[optional]` argument, against `NAME`'s dummy list read from `src/*.f90`. Names
    and ORDER both, since a positional call depends on the order and the page's own bracket
    convention is written to be read positionally.

    **Deliberately not covered: a type-bound call.** `doc/pages/tables/table-join.md`'s
    `call t%join(other, on, [other_on], ...)` is a generic with six specifics and no single dummy
    list to compare against, and `%`-rooted spans are not matched at all. Those signatures stay a
    hand check.

    A procedure declared twice in `src/` with two different dummy lists is reported as ambiguous
    and not compared, rather than guessed at. The scan refuses to pass on an empty result in either
    direction -- no pages, or no signatures found -- because both mean it has gone blind rather
    than that the guide is clean (CLAUDE.md, "A static check that enumerates names goes stale
    silently").
    """
    problems = []
    sigs, ambiguous = {}, set()
    for path in sorted(SRC.glob("*.f90")):
        for _, line in _logical_lines(path):
            m = PROC_DECL.match(line)
            if not m:
                continue
            name = m.group(1).lower()
            args = [a.strip().lower() for a in m.group(2).split(",") if a.strip()]
            if name in sigs and sigs[name] != args:
                ambiguous.add(name)
            sigs[name] = args
    if not sigs:
        return ["src/: no procedure declarations found -- this check has gone blind on the source"]

    pages = sorted((REPO_ROOT / "doc" / "pages").glob("**/*.md"))
    if not pages:
        return ["doc/pages/: no .md pages found -- this check has gone blind on the guide"]

    checked = 0
    for page in pages:
        rel = page.relative_to(REPO_ROOT)
        text = page.read_text()
        for name, args, off in _documented_call_signatures(text):
            if name not in sigs:
                continue          # not a procedure of this library
            lineno = text[:off].count("\n") + 1
            if name in ambiguous:
                problems.append(
                    "%s:%d: `%s` is declared with two different argument lists in src/, so its "
                    "documented signature cannot be checked -- give the check one declaration to "
                    "compare against, or drop the page's bracketed form" % (rel, lineno, name))
                continue
            checked += 1
            if args != sigs[name]:
                problems.append(
                    "%s:%d: the documented call form of `%s` does not match its declaration:\n"
                    "    page: %s\n    src : %s" % (rel, lineno, name,
                                                    ", ".join(args), ", ".join(sigs[name])))
    if not checked:
        return ["doc/pages/: no documented call signatures found -- this check has gone blind; "
                "it matches `call NAME(...)` spans carrying a [optional] argument"]
    return problems


#: `%agg`'s vocabulary claims on `doc/pages/tables/table-group.md`, each as
#: (key, page anchor, how the page span ends). The page writes every token as a `"..."` code span,
#: which is what makes both halves machine-readable. `q`/`method`/`ddof`/`scale` share one
#: sentence and are cut apart on their own option markers, so they carry no anchor of their own.
AGG_DOC_ANCHORS = (
    ("exact", r"and keeps them exact:(.*?), on an integer or logical column"),
    ("options", r"An option a token does not take is refused rather than ignored\*\*:(.*?)\.\s"),
    ("weights", r"weights on a statistic they cannot affect \((.*?)\)"),
    ("keepsunit", r"unit for the statistics that keep its dimension —(.*?)— and none for"),
    ("nounit", r"— and none for(.*?), since a count has no unit"),
)

#: The options `%agg` refuses, in the order their refusals appear in `agg_check_options`.
AGG_OPTIONS = ("q", "method", "ddof", "scale")


def _fortran_string_text(literal):
    """The text a Fortran string-concatenation chain produces: the quoted runs, joined.

    `'a "x" and ' // '"y"'` gives `a "x" and "y"`. Written as a scanner rather than a regex
    because the messages carry `"` inside `'`-delimited literals and `'` is legal inside
    `"`-delimited ones, which no single pattern gets right.
    """
    out, i, n = [], 0, len(literal)
    while i < n:
        ch = literal[i]
        if ch in "'\"":
            j = i + 1
            while j < n and literal[j] != ch:
                j += 1
            out.append(literal[i + 1:j])
            i = j + 1
        else:
            i += 1
    return "".join(out)


def check_agg_vocabulary_matches_its_documentation():
    """`%agg`'s exact family and its option refusals must match what the source says they are.

    `doc/pages/tables/table-group.md` writes out, by hand, three lists the source owns:

    * the **exact int64 family** -- `AG_INT_TOKENS` (`src/parquet_tables_group.f90`);
    * which statistics **`q=`, `method=`, `ddof=` and `scale=` belong to** -- the four refusal
      messages in `agg_check_options`, which are the authority on those sets because they are what
      a caller is told;
    * the statistics **weights cannot affect** -- the fifth refusal message in the same procedure.

    **Why these three and not `%agg`'s 17-row token table.** `feature_doc.md` section 8 ("A check is
    worth proposing when drift makes the page state something FALSE") splits this class in two, and
    only these three fall on the checkable side. The page's sentence says declaring `out` as
    `integer(int64)` "selects the statistics that are exact", and says `method=` "belongs to" three
    named tokens -- closed claims, so a token added to or removed from either set makes the page
    tell a reader something false about code they are about to write. Drift in the 17-row table
    leaves the page merely INCOMPLETE, which is the class that was proposed and declined on U3, so
    that table is deliberately outside this check and stays a hand count.

    **Sets, not sequences.** Unlike a signature's argument list, none of these is positional, so
    reordering the page's list is an editorial choice rather than a defect; only membership is
    compared, and a mismatch prints both sides.

    The scan refuses to pass when an anchor does not match or a source list comes back empty: both
    mean it has gone blind rather than that the page is clean (CLAUDE.md, "A static check that
    enumerates names goes stale silently").
    """
    src = SRC / "parquet_tables_group.f90"
    page = REPO_ROOT / "doc" / "pages" / "tables" / "table-group.md"
    if not src.is_file() or not page.is_file():
        return ["check_agg_vocabulary_matches_its_documentation: %s or %s is missing -- this "
                "check has gone blind" % (src.name, page.name)]

    # ---- the source half -------------------------------------------------------------------
    want, every = {}, set()
    for _, line in _logical_lines(src):
        m = re.search(r"AG_INT_TOKENS\s*=\s*(.+)$", line)
        if m:
            want["exact"] = {t.strip() for t in
                             _fortran_string_text(m.group(1)).split(",") if t.strip()}
        m = re.search(r"AG_REAL_TOKENS\s*=\s*(.+)$", line)
        if m:
            every = {t.strip() for t in
                     _fortran_string_text(m.group(1)).split(",") if t.strip()}
        if "error stop" not in line:
            continue
        msg = _fortran_string_text(line[line.index("error stop") + len("error stop"):])
        for m in re.finditer(r"\b(%s)= belongs to (.*?)(?:;|$)" % "|".join(AGG_OPTIONS), msg):
            want[m.group(1)] = set(re.findall(r'"([a-z]+)"', m.group(2)))
        m = re.search(r"weights have no effect on (.*?)(?:;|$)", msg)
        if m:
            want["weights"] = set(re.findall(r'"([a-z]+)"', m.group(1)))

    # The unit partition, from `agg_unit`'s one `select case` arm. The AG_* constant names ARE the
    # token names uppercased, which is what makes this derivable at all; scoped to that procedure
    # so a `case (AG_...)` anywhere else in the file cannot be read as this one.
    body = re.search(r"subroutine agg_unit\(.*?end subroutine agg_unit", src.read_text(), re.S)
    if body is not None:
        arm = re.search(r"case \(([^)]*AG_[A-Z_, ]*)\)", body.group(0))
        if arm is not None:
            want["nounit"] = {t.lower() for t in re.findall(r"AG_([A-Z]+)", arm.group(1))}
    if every and want.get("nounit"):
        # The complement, so that a statistic in NEITHER of the page's two lists is caught as well
        # -- U4's third direction, the one a two-way comparison cannot see.
        want["keepsunit"] = every - want["nounit"]

    missing_src = [k for k in ("exact", "weights", "nounit", "keepsunit") + AGG_OPTIONS
                   if not want.get(k)]
    if missing_src:
        return ["src/parquet_tables_group.f90: could not read %s out of the source -- this check "
                "has gone blind. It reads AG_INT_TOKENS, AG_REAL_TOKENS, agg_unit's `case (AG_...)` "
                "arm, and the `X= belongs to \"...\"` and `weights have no effect on \"...\"` "
                "refusal messages in agg_check_options." % ", ".join(missing_src)]

    # ---- the page half ---------------------------------------------------------------------
    flat = re.sub(r"\s+", " ", page.read_text())
    got, problems = {}, []
    for key, anchor in AGG_DOC_ANCHORS:
        m = re.search(anchor, flat)
        if m is None:
            problems.append("doc/pages/tables/table-group.md: the '%s' sentence no longer matches "
                            "its anchor, so this check can no longer see it. Either restore the "
                            "wording or update AGG_DOC_ANCHORS; do not leave it unmatched."
                            % key)
            continue
        span = m.group(1)
        if key != "options":
            got[key] = set(re.findall(r'`"([a-z]+)"`', span))
            continue
        marks = [(mm.start(), mm.group(1))
                 for mm in re.finditer(r"`(%s)=`" % "|".join(AGG_OPTIONS), span)]
        for i, (pos, name) in enumerate(marks):
            end = marks[i + 1][0] if i + 1 < len(marks) else len(span)
            got[name] = set(re.findall(r'`"([a-z]+)"`', span[pos:end]))
    if problems:
        return problems

    # ---- compare ---------------------------------------------------------------------------
    labels = {"exact": "the exact int64 family (AG_INT_TOKENS)",
              "weights": "the statistics weights cannot affect",
              "nounit": "the statistics %add_agg gives NO unit (agg_unit's case arm)",
              "keepsunit": "the statistics %add_agg gives the source column's unit"}
    for key in ("exact", "weights", "nounit", "keepsunit") + AGG_OPTIONS:
        page_set = got.get(key)
        if not page_set:
            problems.append("doc/pages/tables/table-group.md: names no statistic for %s, so this "
                            "check has gone blind on it; the source says: %s"
                            % (labels.get(key, "`%s=`" % key), ", ".join(sorted(want[key]))))
            continue
        if page_set != want[key]:
            problems.append(
                "doc/pages/tables/table-group.md: %s disagrees with the source.\n"
                "    page: %s\n    src : %s"
                % (labels.get(key, "`%s=`" % key),
                   ", ".join(sorted(page_set)), ", ".join(sorted(want[key]))))
    return problems


def check_maml_keys_case_insensitive():
    """A MAML block header must never be recognized by a literal case-SENSITIVE comparison.

    Every MAML key is case-insensitive -- `parquet_find_maml_section` lowercases both sides, so
    `Extra:` validates exactly like `extra:`. The block LOCATORS did not: they compared
    `trim(adjustl(line)) == "extra:"` against a lowercase literal, so a MAML spelling its section
    `Extra:` passed validation while its `protected_cols:`, `nullable_cols:`, `col_map:`, `remap:`,
    `filter:` and `sort:` were silently never found -- no warning, and a file that looks right. The
    sharpest case is `protected_cols:`, where the Null protection a user asked for simply disappears.

    So a header is matched with `parquet_maml_key_matches` (parquet_core's subtree) or
    `maml_key_matches` (parquet_tables_maml.f90's own twin -- that file deliberately carries its
    own parsing primitives). Two copies of the predicate cannot drift in any harmful way; a NEW
    site forgetting both is the real hazard, and it is what this check catches. The run-time half is
    `test_maml_block_headers_case_insensitive` (test/test_maml.f90) and the paired
    `extra_section_capitalized`/`extra_section_lowercase_control` error scenarios.

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


@functools.lru_cache(maxsize=None)
def _logical_lines(path):
    """(lineno of the first physical line, joined code) with comments and `&` folded away.

    Cached: six checks ask for the same file's logical lines, and folding them is not cheap.
    The result is a tuple so that the six cannot tread on each other.
    """
    out = []
    buf = ""
    start = None
    for lineno, code in enumerate(stripped_lines(path), start=1):
        code = code.strip()
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
    return tuple(out)


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


#: A function statement, folded free of continuations by `_logical_lines`, with or without a type
#: prefix and with or without `result(...)`.  Subroutines are tracked too, so that a procedure
#: CONTAINED in a function cannot have its own locals read as the outer function's result.
_FUNC_OPEN = re.compile(
    r"^" + _PROC_PREFIX + _TYPE_PREFIX + r"function\s+([a-z_]\w*)\s*\(([^)]*)\)(.*)$", re.I)
_SUB_OPEN = re.compile(r"^" + _PROC_PREFIX + r"subroutine\s+([a-z_]\w*)\b", re.I)
_END_FUNC = re.compile(r"^end\s*function\b|^end\s*$", re.I)
_END_SUB = re.compile(r"^end\s*subroutine\b", re.I)
_RESULT_CLAUSE = re.compile(r"\bresult\s*\(\s*([a-z_]\w*)\s*\)", re.I)
#: `character(len=:), allocatable` in either spelling of the length.
_DEFERRED_CHAR = re.compile(r"^character\s*\(\s*(?:len\s*=\s*)?:\s*\)\s*,(.*?)::(.*)$", re.I)


def check_no_deferred_length_character_result():
    """No function anywhere may return a `character(len=:), allocatable`.

    gfortran keeps such a result's hidden length in a STATIC variable rather than a thread-local
    one (GCC PR113797, PR97977), and emits it per CALL SITE -- `slen.<n>.<m>` in the object's
    `.bss`, not `.tbss`. The caller then reads that one global three times for a single
    assignment: once to size the `realloc`, once to store as the target's length, and once to
    size the `memmove`. Any other thread calling the same function in between supplies its own
    length to all three.

    What that produces is a wrong string first and a heap overflow second. `pf_cosmology%init`
    set its label through `capped(name)`, a five-line helper of exactly this shape; two `%init`
    calls on different threads -- which is what test-drive's `!$omp parallel do` makes of a
    parallel suite -- left the label holding `"WMAP9"` stamped at `"Planck18"`'s length, and
    `test_second_init_replaces_the_first` failed on `%get_name` about once in a full run. When
    the second read is the LARGER, the `memmove` writes past what the `realloc` reserved, so the
    same defect can land anywhere in the process.

    **Nothing on this side can catch it.** It compiles clean under every compiler here, the suite
    passes, and the shape reads as ordinary idiomatic Fortran -- the failure is a race whose
    window is a few instructions wide, so a green run is not evidence. Only the generated code
    shows it, which is why this is a static check on the DECLARATION rather than a test.

    The remedy is a subroutine with a `character(len=:), allocatable, intent(out)` argument, as
    `pf_to_str` (`src/parquet_utils.f90`) already is; `parquet_skycoord_object` shows the call
    shape. For an `x = obj%get(x)` accessor use one `intent(inout)` argument instead. A result of
    a length the caller can see -- `character(len=32)`, or `character(len=len(text))` -- needs no
    hidden variable and is unaffected, as is any non-`character` allocatable result.

    Scope is every `.f90` in src/, test/, app/ and bench/: the rule applies to test and benchmark
    code too, and `test_cosmology`'s own `itoa` had the same shape inside the same parallel suite.
    **Interface bodies are NOT skipped.** Every `module function` in this library is DECLARED in
    an interface block and implemented as a bare `module procedure`, so the result declaration
    lives in the interface and nowhere else -- skipping those would blind the check to the one
    shape the library actually uses. Headers are read from `_logical_lines`, so a header split
    over continuations (104 of the 1706 in the tree) is folded before it is matched rather than
    silently missed.
    """
    problems = []
    roots = [SRC, TEST, REPO_ROOT / "app", REPO_ROOT / "bench"]
    paths = sorted(q for root in roots if root.is_dir() for q in root.glob("*.f90"))
    if not paths:
        return ["src/, test/, app/, bench/: no .f90 sources found -- this check has gone blind"]
    seen_any_function = False
    for path in paths:
        rel = path.relative_to(REPO_ROOT)
        stack = []  # one frame per open procedure: (result name or None, header line)
        for lineno, code in _logical_lines(path):
            code = code.strip()
            if _END_FUNC.match(code) or _END_SUB.match(code):
                if stack:
                    stack.pop()
                continue
            m = _SUB_OPEN.match(code)
            if m:
                stack.append((None, lineno))
                continue
            m = _FUNC_OPEN.match(code)
            if m:
                seen_any_function = True
                r = _RESULT_CLAUSE.search(m.group(3))
                stack.append(((r.group(1) if r else m.group(1)).lower(), lineno))
                continue
            if not stack or stack[-1][0] is None:
                continue
            d = _DEFERRED_CHAR.match(code)
            if not d:
                continue
            # A dummy argument, not the result: `intent(...)` is what separates them, and a
            # function result may not carry one.
            if re.search(r"\bintent\s*\(", d.group(1), re.I):
                continue
            declared = [n.strip().split("(")[0].lower() for n in d.group(2).split(",")]
            result, header = stack[-1]
            if result in declared:
                problems.append(
                    "%s:%d: the function opened at line %d returns `%s` as a "
                    "`character(len=:), allocatable` -- make it a subroutine with a "
                    "`character(len=:), allocatable, intent(out)` argument (gfortran shares that "
                    "result's hidden length between threads; see this check's docstring)"
                    % (rel, lineno, header, result)
                )
    if not seen_any_function:
        return ["no function header parsed anywhere in src/, test/, app/, bench/ -- "
                "this check has gone blind"]
    return problems


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
    cover -- see `.claude/rules/fortran-gotchas.md`'s "A component and its parent cannot both be actual arguments of one
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
    `objdump`. `int_reduce_retry`'s doc-comment in src/parquet_random.f90 carries the one-command
    disassembly check.

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

    **Not only the sorting tier.** `parquet_optimize`'s population engines take `threads` and hand
    it to one shared resolver, which is the single place either clamps or validates a count; a drop
    between the entry point and that resolver leaves the engine serial with every answer unchanged,
    since both engines are designed to answer identically at any team size. The spec list is what
    widens the check to a module; everything else is matched by shape.

    Two shapes are read rather than one. Interface declarations are joined across `&` continuations
    before the dummy lists are read -- a multi-line declaration is how every entry point of
    `parquet_optimize` is written, and reading only single-line ones would silently leave them out
    of the set. Bodies are matched in both the abbreviated `module procedure` form and the fully
    restated `module subroutine` form, which is what a body taking a `procedure(...)` dummy must
    use (`fortran-gotchas.md`, gfortran): those bodies are exactly the plain-function specifics
    that forward to their object twins.
    """
    specs = [SRC / "parquet_sorting.f90", SRC / "parquet_argsort.f90", SRC / "parquet_optimize.f90"]
    missing = [s for s in specs if not s.is_file()]
    if missing:
        return ["tools/check_source_conventions.py: %s not found -- this check has gone stale and "
                "is silently testing nothing" % ", ".join(m.name for m in missing)]
    takes = set()
    for spec in specs:
        text = spec.read_text(encoding="utf-8", errors="replace")
        # Join `&` continuations first: an interface declaration spread over three lines is not a
        # different declaration, and reading only the single-line ones would drop a whole module's
        # entry points out of the set without failing anything.
        joined = re.sub(r"&[ \t]*\n[ \t]*", " ", text)
        for m in re.finditer(r"^\s*module subroutine (\w+)\((.*?)\)\s*$", joined, re.M):
            if re.search(r"\bthreads\b", m.group(2)):
                takes.add(m.group(1))
    problems = []
    if not takes:
        return ["no sorting procedure declaring a `threads` dummy was found at all -- either the "
                "argument was removed library-wide or this check's pattern has gone stale; either "
                "way it must not report success"]
    bodies = (sorted(SRC.glob("parquet_sorting_*.f90"))
              + sorted(SRC.glob("parquet_optimize_*.f90"))
              + [SRC / "parquet_argsort_kernel.f90"])
    forms = [r"^    module procedure (\w+)\s*$(.*?)^    end procedure \1\s*$",
             r"^    module subroutine (\w+)\b(.*?)^    end subroutine \1\s*$"]
    checked = 0
    for path in bodies:
        if not path.is_file():
            continue
        text = path.read_text(encoding="utf-8", errors="replace")
        for m in [m for form in forms for m in re.finditer(form, text, re.M | re.S)]:
            name, body = m.group(1), m.group(2)
            if name not in takes:
                continue
            for call in re.finditer(r"call (\w+)\(((?:[^()]|\([^()]*\))*)\)", body, re.S):
                callee, args = call.group(1), call.group(2)
                # A procedure calling ITSELF is a recursive forward, not a drop.
                if callee not in takes or callee == name:
                    continue
                checked += 1
                # The continuation `&` is dropped, not merely collapsed with the whitespace around
                # it: an argument list broken across lines just before `threads=threads` otherwise
                # flattens to a parameter spelled `& threads=threads`, which matches neither
                # accepted spelling. Fortran has no `&` operator, so nothing else can be losing.
                flat = " ".join(args.replace("&", " ").split())
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
        """`text` with the contents of every quoted literal replaced by spaces.

        A line with no quote character has no literal to blank, which is most of them; the
        character loop below is the answer, and the early return is that same answer.
        """
        if "'" not in text and '"' not in text:
            return text
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

    1. Every executable script under tools/ and bench/ has exactly one row in the index, as does
       every `bench/` run-sheet template. Without this a new tool is simply absent and nobody finds
       out, which is how one benchmark wrapper stayed undocumented until this check existed. A
       `tools/*.md` is deliberately exempt -- those are internal development notes rather than
       tools, and CONTRIBUTING.md is for people arriving from outside; see the comment at the
       enumeration below.
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
            elif f.suffix == ".md" and d == "bench":
                # A `bench/` .md is a run-sheet template -- something a campaign RUNS -- so it is
                # a tool and needs its row. A `tools/` .md is NOT, and that asymmetry is the whole
                # point: those are the maintainer's own internal development notes (the machine
                # environments, the doc-review procedure), written for someone already working on
                # this repository. CONTRIBUTING.md's reader is the opposite person -- someone who
                # has just arrived and has none of those machines -- so indexing a private note
                # there would advertise it as part of the contribution workflow and charge its
                # length to every reader. An existing row for one is harmless and none is required.
                scripts.add(rel)

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
#: What `_handle_returning_bindings` is expected to derive, as `type -> {binding, ...}`.
#:
#: This is an EXPECTATION, checked against the source on every run, not a list the check works
#: from -- the call-site scan uses whatever the derivation actually finds, so a new binding is
#: covered the moment it exists. The expectation is what stops the derivation failing silently:
#: a regex that stopped matching would otherwise leave the check passing over an empty set.
#: A difference in either direction is reported, so adding a fifth handle-returning type is a
#: one-line edit here that the check asks for by name rather than one nobody remembers.
EXPECTED_HANDLE_BINDINGS = {
    "parquet_list_column": {"view"},
    "parquet_map_column": {"view"},
    "parquet_struct_column": {"view"},
    "parquet_string_column": {"view", "view_all", "view_slice"},
}


#: `_procedure_scopes`' two patterns, compiled once. The opener's leading `[\w()=:,*\s]+?\s??`
#: backtracks hard, so it is kept off every line that cannot possibly match (see below).
_SCOPE_OPENER = re.compile(r"^\s*(?:(?:pure|impure|elemental|recursive|module)\s+)*"
                           r"(?:[\w()=:,*\s]+?\s)??(?:subroutine|function)\s+\w+\s*\(", re.I)
_SCOPE_CLOSER = re.compile(r"^\s*end\s*(?:subroutine|function)\b", re.I)


def _procedure_scopes(lines):
    """Yield `(first, last)` 0-based line index pairs, one per procedure body in `lines`.

    Both patterns require the literal `subroutine` or `function`, so a line containing neither
    cannot match either of them and the substring test decides it without running a regex. That
    matters because the opener is the most expensive pattern in this file and about 95% of lines
    are not procedure headers; `str.lower` plus two `in` tests are C-speed where the regex is not.
    """
    stack, scopes = [], []
    for i, code in enumerate(lines):
        low = code.lower()
        if "subroutine" not in low and "function" not in low:
            continue
        if _SCOPE_OPENER.match(code):
            stack.append(i)
        elif _SCOPE_CLOSER.match(code) and stack:
            scopes.append((stack.pop(), i))
    return scopes


@functools.lru_cache(maxsize=None)
def procedure_scopes(path):
    """`_procedure_scopes` over `path`, derived once per run.

    Takes comment-stripped lines, which is what every caller passed anyway.
    """
    return tuple(_procedure_scopes(stripped_lines(path)))


def check_open_table_arguments_are_forwarded():
    """A generated table type's `%init` forwards every `parquet_open_table` argument it promises.

    `tools/generate_user_table_code.py` emits a `%init`/`%init_slice` wrapper whose own doc-comment
    says *"Every optional argument is forwarded to parquet_open_table unchanged, except `exact`"*.
    It forwards them BY NAME, in a hard-coded call, so an argument added to `parquet_open_table` is
    not a compile error there -- the wrapper keeps building, silently stops offering the new
    argument, and the promise quietly becomes false. `list_columns=` reached a release that way.

    **Matched by SHAPE in both directions, from two files the repository already keeps in step.**
    The expected set is read from `parquet_open_table`'s own interface bodies in
    `src/parquet_tables.f90`, so the day the library gains an argument is the day this fails. The
    actual set is read from `src/parquet_table_example.f90` -- the generator's committed OUTPUT,
    which `generate_user_table_code.py --check` (lint stage) already proves is what the generator
    emits today. Reading the emitted Fortran rather than the emitting Python is deliberate: the
    call is assembled there from several adjacent string literals, and a regex over that is a
    second thing to get wrong.

    `table`/`filename` (and a slice's `row_lo`/`row_hi`) are excluded because the wrapper supplies
    them itself. `exact` is not a `parquet_open_table` argument at all, so it never appears here.
    """
    spec = SRC / "parquet_tables.f90"
    emitted = SRC / "parquet_table_example.f90"
    missing = [p for p in (spec, emitted) if not p.is_file()]
    if missing:
        return ["tools/check_source_conventions.py: %s not found -- this check has gone stale and "
                "is silently testing nothing" % ", ".join(m.name for m in missing)]
    spec_text = spec.read_text(encoding="utf-8", errors="replace")

    def arg_list(text, start):
        """The comma-separated argument names of the call/declaration opening at `start`."""
        depth, out = 1, []
        for ch in text[start:]:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    break
            out.append(ch)
        raw = "".join(out).replace("&", " ").replace("\n", " ")
        return [a.strip() for a in raw.split(",") if a.strip()]

    def declared(proc):
        m = re.search(r"^\s*module subroutine %s\(" % re.escape(proc), spec_text, re.M)
        return arg_list(spec_text, m.end()) if m else None

    # The generated wrapper's own forwarding calls: the whole-file one takes `filename` next, a
    # slice one takes `row_lo`. Both are matched by shape rather than by which procedure encloses
    # them, so renaming the generated type changes nothing here.
    emitted_text = emitted.read_text(encoding="utf-8", errors="replace")
    full_calls, slice_calls = [], []
    for m in re.finditer(r"call parquet_open_table\(self%parquet_table,", emitted_text):
        args = arg_list(emitted_text, emitted_text.index("(", m.start()) + 1)
        if len(args) > 2 and args[2] in ("row_lo",):
            slice_calls.append(args)
        else:
            full_calls.append(args)
    if not full_calls or not slice_calls:
        return ["expected both a whole-file and a slice `call parquet_open_table(self%%parquet_table, "
                "...)` in src/parquet_table_example.f90, found %d and %d -- the generated wrapper "
                "has been restructured and this check no longer sees it"
                % (len(full_calls), len(slice_calls))]

    problems = []
    for proc, supplied, calls, label in (
            ("open_table_full", {"table", "filename"}, full_calls, "%init"),
            ("open_table_slice_i32", {"table", "filename", "row_lo", "row_hi"}, slice_calls,
             "%init_slice")):
        args = declared(proc)
        if not args:
            problems.append(
                "could not read %s's dummy arguments from src/parquet_tables.f90 -- the interface "
                "body has moved or been renamed, so this check is testing nothing" % proc)
            continue
        want = [a for a in args if a not in supplied]
        for call in calls:
            for a in want:
                if a not in call:
                    problems.append(
                        "src/parquet_table_example.f90: %s does not forward `%s` to "
                        "parquet_open_table, which declares it -- generate_user_table_code.py's "
                        "wrapper promises every optional argument is forwarded, so a caller of a "
                        "generated table type cannot reach it" % (label, a))
    return problems


def check_generated_init_signature_is_documented():
    """`generated-tables.md`'s `%init` row must list exactly the dummies the generator emits.

    The sibling check above proves the generated wrapper FORWARDS every `parquet_open_table`
    argument. Nothing proved the guide SAYS SO, and the same two arguments went missing in both
    places from one cause: `list_columns=` and `bounded=` were absent from the wrapper (which
    `check_open_table_arguments_are_forwarded` was then written for), and absent from the page's
    `%init` row -- where they stayed after the code was fixed, because nothing reads the page.

    What makes that damaging rather than merely incomplete is the sentence directly beneath the
    table: *"Every one of `%init`'s is forwarded to `parquet_open_table` unchanged, except
    `exact`"*. It is CORRECT as a rule, and it tells a reader the list above it is exhaustive -- so
    an argument missing from the row reads as an argument the generated type does not offer.

    **Three things are compared, all from one parse of each side:**

      * the argument NAMES, in both directions -- a dummy the row omits, and a row token no dummy
        matches (the second catches a rename that only half landed);
      * their ORDER, because the row is written as a call and Fortran's optional arguments are
        positional until a caller starts using keywords;
      * their BRACKETS -- `[x]` must mean `optional` and a bare token must mean required. This is
        the page's own stated convention, so a bracket that disagrees with the declaration is
        wrong in the way a reader would actually act on.

    **Matched by shape on both sides; neither the generated type's name nor the page's wording is
    hard-coded.** The emitted side is reached through `procedure :: init => <name>` in the type
    body rather than by naming `parquet_table_test_init`, because that name is derived from the
    Role-A MAML (`table_types/maml_example4.maml` -> `parquet_table_test`) and a downstream project's
    is different. The page side is anchored on a table row opening with the call in backticks, which
    is the one form the bracket convention is written in, and EVERY matching row is checked rather
    than the first -- a second one added later would otherwise drift unchecked, the check narrowing
    silently with nothing to report it.

    Reading the generator's committed OUTPUT rather than the emitting Python is the same choice
    `check_open_table_arguments_are_forwarded` makes and for the same reason -- the declaration is
    assembled there from adjacent string literals, and a regex over that is a second thing to get
    wrong. `generate_user_table_code.py --check` (lint stage) already proves the output is current.

    **`%init_slice` is deliberately NOT checked.** Its row ends in `...`, an intentional
    abbreviation that says "and the rest of `%init`'s"; making it match would mean expanding a row
    the page shortened on purpose. `%init_empty([nrows])` is likewise out of scope: its optionality
    comes from three separate specifics under one generic, not from an `optional` dummy, so there is
    no dummy list to compare against.

    Every failure path returns a problem rather than passing quietly -- if the binding, the
    subroutine or the table row stops being found, this check has gone blind and says so. That is
    the failure mode a first version of the sibling page-check shipped with.
    """
    emitted = SRC / "parquet_table_example.f90"
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "generated-tables.md"
    missing = [p for p in (emitted, page) if not p.is_file()]
    if missing:
        return ["tools/check_source_conventions.py: %s not found -- this check has gone stale and "
                "is silently testing nothing" % ", ".join(m.name for m in missing)]

    emitted_text = emitted.read_text(encoding="utf-8", errors="replace")

    # `procedure :: init => <name>` in the generated type body. The type's name comes from the
    # Role-A MAML, so the binding is the anchor, never the procedure's own name.
    m = re.search(r"^\s*procedure\s*::\s*init\s*=>\s*(\w+)", emitted_text, re.M)
    if not m:
        return ["src/parquet_table_example.f90: no `procedure :: init => ...` binding found -- the "
                "generator's type body has been restructured and this check is testing nothing"]
    impl = m.group(1)

    decl = re.search(r"^\s*subroutine %s\(" % re.escape(impl), emitted_text, re.M)
    if not decl:
        return ["src/parquet_table_example.f90: `%s` is bound as %%init but never defined -- this "
                "check is testing nothing" % impl]

    # The dummy list, and then each dummy's own declaration, for the `optional` attribute.
    depth, buf = 1, []
    for ch in emitted_text[decl.end():]:
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                break
        buf.append(ch)
    dummies = [a.strip() for a in "".join(buf).replace("&", " ").replace("\n", " ").split(",")
               if a.strip()]
    if len(dummies) < 2:
        return ["src/parquet_table_example.f90: %s takes %d dummy argument(s) -- expected a passed "
                "object and at least a filename, so this check is testing nothing"
                % (impl, len(dummies))]
    dummies = dummies[1:]                      # drop the passed-object dummy

    body = emitted_text[decl.end():emitted_text.index("end subroutine %s" % impl, decl.end())]
    optional = set()
    for line in body.split("\n"):
        if "::" not in line or "!" == line.strip()[:1]:
            continue
        attrs, _, names = line.partition("::")
        if not re.search(r"\boptional\b", attrs, re.I):
            continue
        names = names.split("!")[0]
        optional.update(n.strip() for n in names.split(",") if n.strip())

    # Every `%init` row on the page, not merely the first -- a second one added later would
    # otherwise drift unchecked. `%init_slice`'s rows do not match (the literal `(` after `init`
    # separates them), and a row abbreviated with a trailing `...` is skipped for the same reason
    # that one is: it says "and the rest of %init's", which is not a claim to check.
    rows = [m.group(1) for m in re.finditer(
        r"^\|\s*`%init\((.*?)\)`", page.read_text(encoding="utf-8", errors="replace"), re.M)]
    rows = [r for r in rows if "..." not in r]
    if not rows:
        return ["doc/pages/utilities/generated-tables.md: no unabbreviated `| `%init(...)`` table "
                "row found -- the page has been restructured and this check is testing nothing"]

    problems = []
    for tokens in [[t.strip() for t in r.split(",") if t.strip()] for r in rows]:
        problems += _init_row_problems(tokens, dummies, optional)
    return problems


def _init_row_problems(tokens, dummies, optional):
    """Compare one rendered `%init(...)` row against the emitted dummy list. See the caller."""
    problems, shown = [], []
    for tok in tokens:
        bare = tok[1:-1].strip() if tok.startswith("[") and tok.endswith("]") else tok
        shown.append(bare)
        if bare not in dummies:
            problems.append(
                "doc/pages/utilities/generated-tables.md: %%init's row lists `%s`, which the "
                "generator does not emit -- the page describes an argument a generated table type "
                "does not have" % bare)
            continue
        want_bracket = bare in optional
        if tok.startswith("[") != want_bracket:
            problems.append(
                "doc/pages/utilities/generated-tables.md: %%init's row shows `%s` as %s, but "
                "src/parquet_table_example.f90 declares it %s -- the page's own convention is that "
                "square brackets mean optional"
                % (bare, "optional" if tok.startswith("[") else "required",
                   "optional" if want_bracket else "required"))

    for arg in dummies:
        if arg not in shown:
            problems.append(
                "doc/pages/utilities/generated-tables.md: %%init's row omits `%s`, which the "
                "generator emits and forwards -- the sentence under that table says every one of "
                "%%init's arguments is forwarded, so a reader takes the row as exhaustive" % arg)

    if not problems and shown != dummies:
        problems.append(
            "doc/pages/utilities/generated-tables.md: %%init's row lists the right arguments in the "
            "wrong order (page: %s; generator: %s) -- the row is written as a call, and a caller "
            "may pass them positionally" % (", ".join(shown), ", ".join(dummies)))
    return problems


def _handle_returning_bindings():
    """`(type -> {binding, ...})` for every binding that hands back a pointer AT its own dummy.

    Derived from the source by SHAPE, never enumerated. The hazard has one signature -- a pointer
    assignment whose right-hand side is the bare passed-object dummy, `h%col => self` -- so this
    walks `src/` for exactly that, takes the enclosing procedure's `class(T) ... :: self`
    declaration, and maps the procedure's name back to whatever binding(s) of `T` reach it.

    Deriving rather than listing is the point. The previous version of this check carried four
    type names and a `view|view_all` regex, and `parquet_string_column%view_slice` -- which points
    at `self` in exactly the same way, one screen further down the same file -- was never checked
    at all. A list of names cannot report that it has gone narrow; a derivation can.

    Returns `(bindings, spellings, notes)`. `bindings` is the PUBLIC face -- a specific that is
    reachable through a `generic ::` is reported under the generic's name only, since that is what
    a caller writes and what the expectation should read as. `spellings` is every name the call
    can be made under, generic and specific alike, and is what the call-site scan uses: a private
    specific is still callable from inside its own module, and checking it costs nothing.
    `notes` carries any structural complaint about the derivation itself.
    """
    assign = re.compile(r"=>\s*self\s*$")
    selfdecl = re.compile(r"^\s*(?:type|class)\s*\(\s*(\w+)\s*\)(.*?)::\s*self\b", re.I)
    # `procedure :: bind => impl`, `procedure :: impl`, and `generic :: g => a, b`.
    bind_named = re.compile(r"^\s*procedure\s*(?:,[^:]*)?::\s*(\w+)\s*=>\s*(\w+)", re.I)
    bind_plain = re.compile(r"^\s*procedure\s*(?:,[^:]*)?::\s*(\w+)\s*(?:!.*)?$", re.I)
    generic = re.compile(r"^\s*generic\s*(?:,[^:]*)?::\s*(\w+)\s*=>\s*(.+)$", re.I)

    impls = {}          # type -> {implementing procedure name, ...}
    notes = []
    for path in sorted(SRC.glob("*.f90")):
        lines = stripped_lines(path)
        for first, last in procedure_scopes(path):
            body = lines[first:last + 1]
            if not any(assign.search(r) for r in body):
                continue
            m = re.match(r"^\s*(?:(?:pure|impure|elemental|recursive|module)\s+)*"
                         r"(?:[\w()=:,*\s]+?\s)??(?:subroutine|function)\s+(\w+)",
                         body[0], re.I)
            if not m:
                continue
            owner = next((d.group(1).lower() for d in
                          (selfdecl.match(r) for r in body) if d), None)
            if owner is None:
                notes.append(
                    "%s:%d: `%s` pointer-assigns the bare passed object but declares no `self` "
                    "this check can read -- the derivation has gone stale"
                    % (path.relative_to(REPO_ROOT), first + 1, m.group(1)))
                continue
            impls.setdefault(owner, set()).add(m.group(1).lower())

    bindings, spellings = {}, {}
    for path in sorted(SRC.glob("*.f90")):
        text = path.read_text(encoding="utf-8", errors="replace")
        for owner, wanted in impls.items():
            tm = re.search(r"^\s*type\s*(?:,[^:]*)?::\s*%s\b(.*?)^\s*end\s+type\b" % owner,
                           text, re.I | re.S | re.M)
            if not tm:
                continue
            body = [strip_comment(r) for r in tm.group(1).split("\n")]
            # Names that a `generic ::` covers, whether or not this type is the one that
            # implements them: those are the specifics, and the generic is their public face.
            covered = set()
            for code in body:
                g = generic.match(code)
                if g:
                    covered |= {s.strip().lower() for s in g.group(2).split(",")}
            for code in body:
                g = generic.match(code)
                if g:
                    if {s.strip().lower() for s in g.group(2).split(",")} & wanted:
                        bindings.setdefault(owner, set()).add(g.group(1).lower())
                        spellings.setdefault(owner, set()).add(g.group(1).lower())
                    continue
                b = bind_named.match(code)
                if b:
                    name, impl = b.group(1).lower(), b.group(2).lower()
                else:
                    b = bind_plain.match(code)
                    if not b:
                        continue
                    # `procedure :: view_i64` -- the binding IS its own implementation.
                    name = impl = b.group(1).lower()
                if impl not in wanted:
                    continue
                spellings.setdefault(owner, set()).add(name)
                if name not in covered:
                    bindings.setdefault(owner, set()).add(name)
    return bindings, spellings, notes


def check_view_call_sites_declare_target():
    """CLAUDE.md -- a handle-returning call site must declare its column `target`.

    `parquet_list_column%view`, its map/struct twins and `parquet_string_column`'s
    `%view`/`%view_all`/`%view_slice` return a handle whose stored pointer is associated with the
    passed-object dummy. F2018 15.5.2.4 leaves such a pointer UNDEFINED on return whenever the
    actual argument does not have the TARGET attribute -- so the handle is unusable, and the
    `associated(self%col)` guard inside every handle binding is itself non-conforming.

    **gfortran, ifx and flang all execute the non-conforming form perfectly happily**, so nothing
    in CI, in an ordinary `fpm test` or in a coverage run can see this. Only nagfor's `-C=dangling`
    reports it, as an abort naming the handle binding rather than the call that produced the
    handle. That is what makes it worth a static check: the failure mode is a silent one on every
    compiler the project builds with by default.

    **The rule is about the pointer's TARGET, not about handing back a pointer.** A binding that
    reaches through a POINTER component -- `parquet_table%col`, whose `p => self%cache%cols(...)`
    lands in the heap object `self%cache` points at -- is not in this class at all, because that
    object is not the dummy and does not stop existing when the call returns. That is why ~120
    `%col` call sites correctly declare no `target`, and why the derivation below matches only a
    bare `=> self`.

    Both the type list and the binding names are DERIVED (`_handle_returning_bindings`) and then
    compared against `EXPECTED_HANDLE_BINDINGS`, so the check reports its own narrowing. The
    call-site scan uses the derived set, so a newly added binding is covered before anyone updates
    the expectation.

    The check is scope-aware -- a declaration is only reported when the binding is called on that
    name inside the same procedure -- so a column merely declared beside an unrelated handle costs
    nothing.
    """
    bindings, spellings, problems = _handle_returning_bindings()
    problems = list(problems)
    if bindings != {k: set(v) for k, v in EXPECTED_HANDLE_BINDINGS.items()}:
        for t in sorted(set(bindings) | set(EXPECTED_HANDLE_BINDINGS)):
            got = bindings.get(t, set())
            want = set(EXPECTED_HANDLE_BINDINGS.get(t, set()))
            if got != want:
                problems.append(
                    "handle-returning bindings for `%s`: source has {%s}, "
                    "EXPECTED_HANDLE_BINDINGS in tools/check_source_conventions.py says {%s}. "
                    "A binding that pointer-assigns its bare passed object (`h%%col => self`) puts "
                    "every call site under F2018 15.5.2.4; update the expectation and check the "
                    "new binding's call sites declare `target`."
                    % (t, ", ".join(sorted(got)) or "-", ", ".join(sorted(want)) or "-"))
    if not bindings:
        return problems + [
            "no `h%col => self` binding found under src/ -- the handle API has been renamed, or "
            "this check's derivation has gone stale and it is passing vacuously"]

    # Longest first, so `%view_slice(` is not matched as `%view` with a stray suffix.
    names = sorted({b for v in spellings.values() for b in v}, key=len, reverse=True)
    call = re.compile(r"(?<![\w%%])([A-Za-z_]\w*)%%(?:%s)\s*\(" % "|".join(names))
    decl = re.compile(r"^\s*(?:type|class)\s*\(\s*(%s)\s*\)(.*?)::(.*)$"
                      % "|".join(sorted(spellings)), re.I)
    roots = [SRC, TEST, REPO_ROOT / "app", REPO_ROOT / "bench"]
    paths = sorted(p for root in roots if root.is_dir() for p in root.glob("*.f90"))
    seen_any_call = False
    for path in paths:
        lines = stripped_lines(path)
        for first, last in procedure_scopes(path):
            body = lines[first:last + 1]
            viewed = set()
            for code in body:
                for m in call.finditer(code):
                    viewed.add(m.group(1).lower())
            if not viewed:
                continue
            seen_any_call = True
            for offset, code in enumerate(body):
                m = decl.match(code)
                if not m:
                    continue
                attrs = m.group(2).lower()
                if "target" in attrs or "pointer" in attrs:
                    continue
                declared = [re.sub(r"\(.*", "", n).strip().lower() for n in m.group(3).split(",")]
                hit = sorted(n for n in declared if n in viewed)
                if not hit:
                    continue
                problems.append(
                    "%s:%d: `%s` is a %s that a handle-returning binding is called on in this "
                    "procedure, but it is not declared `target`. F2018 15.5.2.4 leaves the "
                    "returned handle's pointer undefined; gfortran, ifx and flang run it anyway "
                    "and only nagfor's `-C=dangling` reports it. Add `, target`:\n    %s"
                    % (path.relative_to(REPO_ROOT), first + offset + 1, ", ".join(hit),
                       m.group(1), source_lines(path)[first + offset].strip()))
    if not seen_any_call:
        problems.append(
            "no handle-returning call site found under src/, test/, app/ or bench/ -- the handle "
            "API has been renamed, or this check's scope detection has gone stale and it is "
            "passing vacuously")
    return problems



@functools.lru_cache(maxsize=None)
def _src_import_graph():
    """{top-level module: frozenset of modules its whole subtree uses}, built once per run.

    fpm never prunes a submodule separately from the module it belongs to (the rule
    `tools/module_footprints.txt` documents), so a submodule's imports count as its ancestor's.
    Without that attachment the walk misses exactly the imports that matter -- most of this
    library's `use parquet_bindings` lines live in submodule files, not in module specs.

    Split out of `_module_import_closure` and cached because it is a property of `src/` alone,
    while the closure it serves is asked for once per test suite: rebuilding the graph per seed
    set re-parsed the whole of `src/` some forty times and was the slowest thing in this file by a
    wide margin. The values are frozen so that a caller cannot mutate the shared graph.
    """
    owner = {}          # program unit -> the top-level module it belongs to
    imports = {}        # top-level module -> set of modules used, anywhere in its subtree
    for path in sorted(SRC.glob("*.f90")):
        code = stripped_text(path)
        m = re.search(r"^\s*submodule\s*\(\s*([a-z0-9_]+)", code, re.M | re.I)
        if m:
            root = m.group(1).lower()
            unit = re.search(r"^\s*submodule\s*\([^)]*\)\s*([a-z0-9_]+)", code, re.M | re.I)
        else:
            m2 = re.search(r"^\s*module\s+([a-z0-9_]+)\s*$", code, re.M | re.I)
            if not m2:
                continue
            root = m2.group(1).lower()
            unit = m2
        owner[unit.group(1).lower() if unit else path.stem] = root
        used = {u.lower() for u in re.findall(r"^\s*use\s+([a-z0-9_]+)", code, re.M | re.I)}
        imports.setdefault(root, set()).update(used)
    # a submodule names its ancestor via `submodule (root:parent)`; resolve roots transitively
    changed = True
    while changed:
        changed = False
        for root, used in list(imports.items()):
            for u in list(used):
                r = owner.get(u, u)
                if r != u and r not in used:
                    used.add(r)
                    changed = True
    return {root: frozenset(used) for root, used in imports.items()}


def _module_import_closure(seed_modules):
    """Every src/ module reachable from `seed_modules`, submodules attached to their ancestor."""
    imports = _src_import_graph()
    seen, stack = set(), [m.lower() for m in seed_modules]
    while stack:
        n = stack.pop()
        if n in seen:
            continue
        seen.add(n)
        for u in imports.get(n, ()):
            if u not in seen:
                stack.append(u)
    return seen


def _runner_registrations():
    """{runner stem: [(suite name, collector)]} read from every test/run_tester*.f90."""
    out = {}
    for path in sorted(TEST.glob("run_tester*.f90")):
        code = "\n".join(strip_comment(ln) for ln in path.read_text(encoding="utf-8").splitlines())
        out[path.stem] = re.findall(r'new_testsuite\(\s*"([a-z0-9_]+)"\s*,\s*([a-z0-9_]+)\s*\)', code)
    return out


def check_test_fixtures_live_under_test_run():
    """A fixture a test WRITES goes under `test_run/`, never into the repository root.

    The convention was universal and unenforced, which is the combination that lets it break: a
    test writing `"my_fixture.parquet"` passes every check, passes its own assertions, and leaves
    a file in the working tree that only a human running `ls` notices. Twenty such files appeared
    in one change before anyone looked.

    **Keyed on what CREATES a file, not on what looks like a path**, because `test/` is full of
    path strings that are test DATA rather than filenames -- `pf_basename("/data/run3/cat.parquet")`,
    `pv_split_path`'s whole table, and error messages quoting a path. A first version matched every
    `.parquet` literal and reported twelve of those as findings against a correct tree, which is
    what a real defect looks like. Three shapes are checked instead:

      a. a scalar `character(len=*), parameter :: <name> = "....parquet"` -- how nearly every
         suite here names its own fixture;
      b. a `.parquet` literal on a line calling one of the file-opening entry points;
      c. a path ASSEMBLED by a `write` statement ending in the `".parquet"` fragment, which must
         carry a `"test_run/..."` prefix literal on the same line.

    Deliberately NOT covered: a path built across several statements, or one handed in from a
    variable this file cannot follow. Those exist (`test_openmp.f90` builds per-thread names) and
    are correct today; the check would need to follow data flow to see them, which is more machinery
    than the hazard warrants.
    """
    openers = re.compile(r"\b(parquet_open_writer|parquet_open_reader|parquet_open_table|"
                         r"parquet_write_table|parquet_table_row_group_bounds)\s*\(", re.I)
    decl = re.compile(r"parameter\s*::\s*\w+\s*=\s*\"([^\"]*\.parquet)\"", re.I)
    literal = re.compile(r'\"([^\"]*\.parquet)\"')
    problems = []
    seen = 0
    for path in sorted(TEST.glob("*.f90")):
        for lineno, raw in enumerate(path.read_text(encoding="utf-8").split("\n"), start=1):
            code = strip_comment(raw)
            names = []
            m = decl.search(code)
            if m:
                names.append(m.group(1))
            elif openers.search(code):
                names.extend(literal.findall(code))
            elif '".parquet"' in code and "write" in code.lower():
                # An assembled path: the prefix literal is what has to carry the directory.
                parts = [x for x in literal.findall(code) if x != ".parquet"]
                pre = re.findall(r'\"([^\"]*)\"', code)
                if not any(x.startswith("test_run/") for x in pre):
                    names.extend(parts or ["<assembled>" + code.strip()[:40]])
            for name in names:
                seen += 1
                if name == ".parquet" or name.startswith("test_run/") or name.startswith("test/fixtures/"):
                    continue
                problems.append(
                    "%s:%d: fixture path '%s' is not under test_run/ -- a test that writes it "
                    "leaves the file in the repository root, where nothing but a human notices it:"
                    "\n    %s" % (path.relative_to(REPO_ROOT), lineno, name, raw.strip()))
    if not seen:
        problems.append("test/*.f90: matched no fixture path at all -- this check is blind")
    return problems


def check_test_runner_partition():
    """The five test runners partition the suites, and the undef-safe ones reach no C++.

    **Nothing in CI can run the check this partition exists for.** The pipeline installs
    `gfortran gcc g++` and no nagfor, so the `-C=undefined` build that gives the split its whole
    purpose is a manual, machine-specific run. If membership were enforced only by that run, the
    first `use parquet` added to `test_spatial.f90` would sit in the tree until someone next
    happened to build with NAG -- and would then look like a NAG portability bug rather than a
    partition violation, which is the most expensive way to find it. This check runs in the lint
    stage, needs only python3, and fails at the moment the violation is introduced.

    Five clauses, per `feature_tests.md` section 10:

      1. every suite is registered in exactly one runner, and every `collect_tests_*` in `test/`
         is registered somewhere -- the failure mode with no symptom, since a suite that stops
         being registered simply stops running while every count still looks plausible;
      2. no file feeding an undef-safe runner reaches `parquet_bindings`, transitively;
      3. no such file declares a `bind(C)` interface of its own;
      4. no file outside `run_tester_errors` drives an error scenario;
      5. `run_tester_noundef` is held to 2 and 3 and NOTHING MORE -- its membership is decided by
         a person after running the checked build (decision 8), and nothing in the source
         distinguishes "Arrow-free and undef-safe" from "Arrow-free and not";
      6. every suite that drives a scenario is NAMED so that `suite_is_safe_to_parallelize`
         (`test/test_runner_support.f90`) runs it serially -- `errors`, or a name ending in
         `_errors`. That function matches the suffix by shape precisely so a future split
         inherits the exclusion for free, and a suite named the other way round
         (`errors_table` rather than `table_errors`) silently defeats it: its tests then fork
         `error_scenarios` subprocesses from inside test-drive's `!$omp parallel do`, which is a
         deterministic SIGSEGV under libiomp5 and merely lucky under libgomp. A primed cache
         hides it, because a primed test forks nothing -- so the full suite passes and the
         hazard is latent.

    Clause 3 is easy to get wrong in the direction that makes the check useless: the Arrow-free
    modules are full of doc-comments containing the phrase `bind(C)`, so a naive case-insensitive
    grep reports every one of them and nothing else. Comments are stripped first.
    """
    UNDEF_SAFE = ("run_tester_pf", "run_tester", "run_tester_noundef")
    regs = _runner_registrations()
    problems = []
    if not regs:
        return ["test/run_tester*.f90: no runner found -- this check is blind"]
    for want in UNDEF_SAFE + ("run_tester_errors", "run_tester_cpp"):
        if want not in regs:
            problems.append("test/%s.f90: expected runner is missing" % want)
    if problems:
        return problems

    # ---- clause 1: exactly one runner per suite, and nothing unregistered -------------------
    where = {}
    for runner, pairs in regs.items():
        for suite, coll in pairs:
            where.setdefault(suite, []).append((runner, coll))
    for suite, hits in sorted(where.items()):
        if len(hits) > 1:
            problems.append("suite '%s' is registered in %s -- it must appear in exactly one"
                            % (suite, ", ".join(sorted(r for r, _ in hits))))
    declared = {}
    for path in sorted(TEST.glob("*.f90")):
        code = "\n".join(strip_comment(ln) for ln in path.read_text(encoding="utf-8").splitlines())
        for m in re.finditer(r"^\s*subroutine\s+(collect_tests_[a-z0-9_]+)\s*\(", code, re.M | re.I):
            declared[m.group(1).lower()] = path
    registered = {c.lower() for pairs in regs.values() for _, c in pairs}
    for coll, path in sorted(declared.items()):
        if coll not in registered:
            problems.append("%s: %s is declared but registered in no runner -- its tests do not run"
                            % (path.name, coll))

    # ---- which test file defines each collector --------------------------------------------
    file_of = {c: p for c, p in declared.items()}

    # ---- clauses 2 and 3: undef-safe runners reach no C++ ----------------------------------
    for runner in UNDEF_SAFE:
        for suite, coll in regs[runner]:
            path = file_of.get(coll.lower())
            if path is None:
                continue
            code = "\n".join(strip_comment(ln) for ln in path.read_text(encoding="utf-8").splitlines())
            seeds = {u.lower() for u in re.findall(r"^\s*use\s+([a-z0-9_]+)", code, re.M | re.I)}
            closure = _module_import_closure(seeds)
            if "parquet_bindings" in closure:
                problems.append(
                    "%s (suite '%s', %s): reaches parquet_bindings -- an undef-safe runner may "
                    "not, because an executed bind(C) call is miscompiled under -C=undefined"
                    % (path.name, suite, runner))
            if re.search(r"bind\s*\(\s*c\s*,\s*name\s*=", code, re.I):
                problems.append(
                    "%s (suite '%s', %s): declares a bind(C) interface -- an undef-safe runner "
                    "may not" % (path.name, suite, runner))

    # ---- clause 4: only run_tester_errors drives scenarios ---------------------------------
    for runner, pairs in regs.items():
        if runner == "run_tester_errors":
            continue
        for suite, coll in pairs:
            path = file_of.get(coll.lower())
            if path is None:
                continue
            code = "\n".join(strip_comment(ln) for ln in path.read_text(encoding="utf-8").splitlines())
            hit = re.search(r"\b(run_error_scenario|check_scenario_[a-z_]*|use\s+test_errors)\b", code)
            if hit:
                problems.append(
                    "%s (suite '%s', %s): references %s -- only run_tester_errors may drive an "
                    "error_scenarios subprocess, so that every other runner forks nothing"
                    % (path.name, suite, runner, hit.group(1)))

    # ---- clause 5: a scenario-driving suite is named so the serial rule catches it -----------
    support = TEST / "test_runner_support.f90"
    if not support.is_file():
        return problems + ["test/test_runner_support.f90: not found -- this check needs updating"]
    rule = "\n".join(strip_comment(ln) for ln in support.read_text(encoding="utf-8").splitlines())
    if '== "_errors"' not in rule or 'name == "errors"' not in rule:
        return problems + [
            "test/test_runner_support.f90: suite_is_safe_to_parallelize no longer excludes by the "
            "`_errors` suffix plus the bare name `errors`, so this check can no longer tell which "
            "suite names are run serially -- update it together with that function"]
    for suite, coll in regs.get("run_tester_errors", []):
        path = file_of.get(coll.lower())
        if path is None:
            continue
        code = "\n".join(strip_comment(ln) for ln in path.read_text(encoding="utf-8").splitlines())
        if not re.search(r"\b(run_error_scenario|check_scenario_[a-z_]*)\b", code):
            continue
        if suite != "errors" and not suite.endswith("_errors"):
            problems.append(
                "%s: suite '%s' drives error scenarios but its name does not end in '_errors', so "
                "suite_is_safe_to_parallelize runs its tests CONCURRENTLY and they fork from "
                "inside an OpenMP team. Rename it '<area>_errors' (the file and its collector to "
                "match); a primed cache hides this, so a green run is no evidence"
                % (path.name, suite))
    return problems


JOIN_SPEC_FILE = SRC / "parquet_tables.f90"
JOIN_BODY_FILE = SRC / "parquet_tables_join.f90"


def _join_generic_specifics():
    """The specific names behind `generic :: join => ...`, from the type body that declares it."""
    if not JOIN_SPEC_FILE.exists():
        return []
    flat = re.sub(r"&\s*\n\s*", "", JOIN_SPEC_FILE.read_text())
    found = re.search(r"generic :: join =>([^\n!]*)", flat)
    if not found:
        return []
    return [n.strip() for n in found.group(1).split(",") if n.strip()]


def _join_specific_interfaces(names):
    """The named `module subroutine`s' dummy lists, as {name: [dummy names]}."""
    flat = re.sub(r"&\s*\n\s*", "", JOIN_SPEC_FILE.read_text())
    out = {}
    for name in names:
        found = re.search(r"module subroutine %s\(([^)]*)\)" % name, flat)
        if found:
            out[name] = [a.strip() for a in found.group(1).split(",") if a.strip()]
    return out


def _forwarding_calls(body):
    """Each `call join_impl(...)`/`call join_from_strings(...)`'s argument text in one body.

    Scanned with a paren counter rather than a `[^)]*` regex: `max_rows=int(max_rows, int64)`
    nests, and a regex that stops at the first `)` reports the arguments after it as missing --
    which is exactly the false alarm this check exists not to raise.
    """
    flat = re.sub(r"&\s*\n\s*", "", body)
    out = []
    for found in re.finditer(r"call (?:join_impl|join_from_strings)\(", flat):
        k, depth = found.end(), 1
        while k < len(flat) and depth > 0:
            if flat[k] == "(":
                depth += 1
            elif flat[k] == ")":
                depth -= 1
            k += 1
        out.append(flat[found.end():k - 1])
    return out


def check_join_specifics_forward_every_argument():
    """`%join`'s six specifics must each pass on every argument they take.

    They exist only because `max_rows=` needs both integer kinds and `on` has two spellings --
    none of the six differs in BEHAVIOUR, so each is a three-line forward onto `join_impl` (or
    onto `join_from_strings`, which forwards again). An argument silently dropped from one of
    those lists is the hardest defect in the area to test for: `require=` and `max_rows=` are
    observable only when they ABORT, so a ceiling that never arrives refuses nothing and every
    in-process assertion about the result still holds. There are eight forwarding sites and one
    error scenario can observe one abort, so covering them all by scenario is not proportionate.
    Comparing each forward against its own interface is, and it does not go stale: both sides are
    read out of the source.
    """
    if not JOIN_SPEC_FILE.exists() or not JOIN_BODY_FILE.exists():
        return ["%s / %s: not found -- this check needs updating"
                % (JOIN_SPEC_FILE, JOIN_BODY_FILE)]
    names = _join_generic_specifics()
    if len(names) < 2:
        return ["%s: found %d specific(s) behind `generic :: join` -- this check has gone blind"
                % (JOIN_SPEC_FILE.relative_to(REPO_ROOT), len(names))]
    interfaces = _join_specific_interfaces(names)
    missing = [n for n in names if n not in interfaces]
    if missing:
        return ["%s: no `module subroutine %s` interface found -- this check has gone blind"
                % (JOIN_SPEC_FILE.relative_to(REPO_ROOT), n) for n in missing]
    text = JOIN_BODY_FILE.read_text()
    problems = []
    seen = set()
    for name, args in sorted(interfaces.items()):
        found = re.search(r"^    module procedure %s\n(.*?)^    end procedure %s$"
                          % (name, name), text, re.S | re.M)
        if not found:
            problems.append("%s: no `module procedure %s` body found -- this check has gone blind"
                            % (JOIN_BODY_FILE.relative_to(REPO_ROOT), name))
            continue
        seen.add(name)
        calls = _forwarding_calls(found.group(1))
        if len(calls) != 1:
            problems.append("%s: %s must forward through exactly one join_impl/join_from_strings "
                            "call, found %d" % (JOIN_BODY_FILE.relative_to(REPO_ROOT), name,
                                                len(calls)))
            continue
        for dummy in args:
            if dummy in ("self", "other", "on"):
                continue          # passed positionally, and checked by the compiler
            if not re.search(r"\b%s\s*=" % dummy, calls[0]):
                problems.append("%s: %s takes `%s` and does not forward it -- an argument dropped "
                                "here is silent, see this check's docstring"
                                % (JOIN_BODY_FILE.relative_to(REPO_ROOT), name, dummy))
    if len(seen) != len(names):
        problems.append("%s: matched %d of the %d specifics behind `generic :: join` -- this "
                        "check has gone blind"
                        % (JOIN_BODY_FILE.relative_to(REPO_ROOT), len(seen), len(names)))
    # join_from_strings forwards a second time, in two branches, and both must be complete.
    worker = re.search(r"^    subroutine join_from_strings\(([^)]*(?:&\s*\n\s*[^)]*)*)\)\n"
                       r"(.*?)^    end subroutine join_from_strings$", text, re.S | re.M)
    if not worker:
        problems.append("%s: no `join_from_strings` found -- this check has gone blind"
                        % JOIN_BODY_FILE.relative_to(REPO_ROOT))
    else:
        args = [a.strip() for a in re.sub(r"&\s*\n\s*", "", worker.group(1)).split(",")
                if a.strip()]
        calls = _forwarding_calls(worker.group(2))
        if len(calls) != 2:
            problems.append("%s: join_from_strings must forward in both of its other_on branches, "
                            "found %d call(s)" % (JOIN_BODY_FILE.relative_to(REPO_ROOT), len(calls)))
        for k, call in enumerate(calls):
            for dummy in args:
                if dummy in ("self", "other", "on", "other_on"):
                    continue      # `on` is split into names first; `other_on` only in one branch
                if not re.search(r"\b%s\s*=" % dummy, call):
                    problems.append("%s: join_from_strings' branch %d does not forward `%s`"
                                    % (JOIN_BODY_FILE.relative_to(REPO_ROOT), k + 1, dummy))
    return problems



def check_spatial_kind_specifics_forward_every_argument():
    """Every kind-carrying `pf_spatial_index` specific passes on every argument it takes.

    The `int32` and `int64` forms of `%pairs_within`, `%all_within`, `%count_all_within` and their
    sky and line-of-sight siblings differ in no BEHAVIOUR: each is a forward onto one bulk worker,
    or onto a shared `_impl` that forwards again. An argument dropped from one of those lists is
    the hardest defect in the area to see -- `combine=`, `r_inner=` and `sorted=` all change the
    ANSWER rather than aborting, so a specific that silently drops one still returns a plausible
    list, and only the sibling kind would have disagreed. There are two dozen forwarding sites and
    the parity tests compare one fixture each, so covering every optional by fixture is not
    proportionate; comparing each forward against its own argument list is, and it does not go
    stale because both sides are read out of the source.
    """
    path = SRC / "parquet_spatial.f90"
    if not path.exists():
        return ["%s: not found -- this check needs updating" % path]
    text = path.read_text()
    wanted = re.compile(r"^(?:bind_(?:pairs|all|count)_\w+|components_\w+|\w+_impl)$")
    problems = []
    found = 0
    for m in re.finditer(r"^    subroutine ([a-z0-9_]+)\(([^)]*(?:&\s*\n\s*[^)]*)*)\)\n"
                         r"(.*?)^    end subroutine \1$", text, re.S | re.M):
        name, arglist, body = m.group(1), m.group(2), m.group(3)
        if not wanted.match(name):
            continue
        found += 1
        args = [a.strip() for a in re.sub(r"&\s*\n\s*", "", arglist).split(",") if a.strip()]
        # The executable part only: a dummy named in its own declaration proves nothing.
        # The executable part starts after the blank line that closes the declarations.
        parts = body.split("\n\n", 1)
        exec_part = parts[1] if len(parts) > 1 else ""
        for dummy in args:
            if dummy == "self":
                continue          # passed positionally, and checked by the compiler
            if not re.search(r"\b%s\b" % re.escape(dummy), exec_part):
                problems.append("src/parquet_spatial.f90: %s takes `%s` and never uses it -- an "
                                "argument dropped from a kind specific is silent, see this "
                                "check's docstring" % (name, dummy))
    if found < 20:
        problems.append("src/parquet_spatial.f90: matched only %d kind-carrying specifics -- this "
                        "check has gone blind" % found)
    return problems


def check_index_map_components_are_adopted_and_reset():
    """`ix_adopt` moves and `ix_reset_storage` releases EVERY component of `pf_index_map`.

    A `%build` fills a LOCAL map on the calling thread and `ix_adopt` (src/parquet_index_map.f90)
    swaps its storage into the caller's object under the guard, one `move_alloc` per allocatable
    component and one assignment per scalar; `ix_reset_storage` is the matching release. A
    component added to the type and forgotten in either would leave every built map without it,
    silently -- nothing aborts, the map simply answers as if the component were at its default --
    and no run-based test can see a component that is not there. So the rule is held from the
    source: every component declared in the type's body must be named in both procedures, in the
    form each uses. The concurrency the swap buys is pinned by `test_concurrent_builds_overlap`
    (test/test_index_omp.f90).
    """
    problems = []
    spec = SRC / "parquet_index.f90"
    body = type_body_lines(spec, "pf_index_map")
    if body is None:
        return ["%s: could not find `type :: pf_index_map` -- this check needs updating"
                % spec.relative_to(REPO_ROOT)]
    components = []
    for lineno, line in body:
        code = strip_comment(line).strip()
        if not code or code.lower().startswith("private"):
            continue
        match = re.match(r"^(integer|logical|character|type|real)\b(.*?)::\s*(\w+)", code, re.IGNORECASE)
        if match:
            components.append((match.group(3), "allocatable" in match.group(2).lower()))
    if len(components) < 10:
        return ["%s: found only %d components of pf_index_map -- this check needs updating"
                % (spec.relative_to(REPO_ROOT), len(components))]
    map_path = SRC / "parquet_index_map.f90"
    bodies = {name: text for name, _, text in procedure_bodies(map_path)}
    for proc in ("ix_adopt", "ix_reset_storage"):
        if proc not in bodies:
            return ["%s: no `%s` body found -- this check needs updating"
                    % (map_path.relative_to(REPO_ROOT), proc)]
    for name, allocatable in components:
        if allocatable:
            want_adopt = re.search(r"move_alloc\(fresh%%%s,\s*self%%%s\)" % (name, name), bodies["ix_adopt"])
            want_reset = re.search(r"deallocate\(self%%%s\)" % name, bodies["ix_reset_storage"])
        else:
            want_adopt = re.search(r"self%%%s\s*=\s*fresh%%%s\b" % (name, name), bodies["ix_adopt"])
            want_reset = re.search(r"self%%%s\s*=" % name, bodies["ix_reset_storage"])
        if not want_adopt:
            problems.append("%s: ix_adopt does not %s pf_index_map's component `%s` -- every "
                            "%%build would silently lose it"
                            % (map_path.relative_to(REPO_ROOT),
                               "move_alloc" if allocatable else "assign", name))
        if not want_reset:
            problems.append("%s: ix_reset_storage does not %s pf_index_map's component `%s`"
                            % (map_path.relative_to(REPO_ROOT),
                               "deallocate" if allocatable else "reset", name))
    return problems


#: The parquet_index submodules whose impure aborts must go through the serialised reporter.
INDEX_SUBMODULES = ("parquet_index_map.f90", "parquet_index_hash.f90", "parquet_index_sorted.f90",
                    "parquet_index_multi.f90", "parquet_index_str.f90", "parquet_index_pool.f90")
INDEX_PROC_HEAD = re.compile(
    r"^\s*((?:pure|impure|elemental|recursive|module)\s+)*(subroutine|function)\s+(\w+)", re.IGNORECASE)
INDEX_MODPROC = re.compile(r"^\s*module\s+procedure\s+(\w+)", re.IGNORECASE)
INDEX_PROC_END = re.compile(r"^\s*end\s+(subroutine|function|procedure)\b", re.IGNORECASE)


def check_index_aborts_go_through_reporter():
    """Every `error stop` in an IMPURE procedure of the parquet_index submodules is `ix_abort`.

    A `%build` runs OUTSIDE `pf_index_map_guard` (it fills a local map and `ix_adopt` swaps it in
    under the guard), so two threads building two maps can fail at the same moment -- and several
    threads terminating at once leaves the exit status undefined under ifx and interleaves the
    messages. `ix_abort` (declared in src/parquet_index.f90, one process-wide critical around the
    `error stop`) is what keeps at most one thread on the fatal path, for every index type. It
    only works if every impure abort goes through it, and a new `error stop` written the natural
    way compiles and passes every scenario. A `pure` procedure cannot call it (an OpenMP directive
    may not appear in a pure procedure), so those keep a bare `error stop`; a `module procedure`
    body is pure when the spec declares its interface `pure`. `.claude/rules/api-conventions.md`,
    "Errors and diagnostics"; `scenario_index_concurrent_abort` races two aborts through it.
    """
    problems = []
    spec = (SRC / "parquet_index.f90").read_text()

    def spec_is_pure(name):
        return re.search(r"\bpure\s+(?:elemental\s+)?module\s+(?:function|subroutine)\s+%s\b" % name,
                         spec, re.IGNORECASE) is not None

    for filename in INDEX_SUBMODULES:
        path = SRC / filename
        stack = []
        for lineno, line in enumerate(path.read_text().split("\n"), start=1):
            code = strip_comment(line)
            head = INDEX_PROC_HEAD.match(code)
            if head and not re.match(r"^\s*end\b", code, re.IGNORECASE):
                prefixes = head.group(0).lower()
                stack.append((head.group(3), "pure" in prefixes or "elemental" in prefixes))
            else:
                modproc = INDEX_MODPROC.match(code)
                if modproc:
                    stack.append((modproc.group(1), spec_is_pure(modproc.group(1))))
            if INDEX_PROC_END.match(code) and stack:
                stack.pop()
            if re.search(r"\berror\s+stop\b", code, re.IGNORECASE) and stack:
                name, pure = stack[-1]
                if not pure and name.lower() != "ix_abort":
                    problems.append("%s:%d: `error stop` in the impure procedure `%s` -- route it "
                                    "through `call ix_abort(...)`, the serialised reporter:\n    %s"
                                    % (path.relative_to(REPO_ROOT), lineno, name, line.strip()))
    return problems


def check_no_leadz():
    """`LEADZ` must not appear in this project's Fortran at all -- `trailz` is the way.

    nagfor 7.2 MISCOMPILES `LEADZ` on an `integer(int64)` at `-O1` and above, including the bare
    `-O` and every `--profile release` build: the answer is exactly 2 too small whenever the true
    answer is 2 or more, so 62 of the 64 single-bit values come back wrong. `leadz` on `int32`,
    `trailz` and `popcnt` are all correct, and `-O0` is correct, which is why every other nagfor
    profile here passes and only a release build sees it.

    Both failure modes are bad and one is silent. `pool_do_compact` wrote the natural
    `j = 63 - leadz(w)` to walk a block's set bits downward; `j` came back 63 instead of 61, so
    `ibclr` cleared a bit that was already clear, `w` stopped shrinking and the loop never ended --
    inside the `pf_index_pool_guard` critical region, so every other caller hung behind it. The
    same file's watermark scan had the same defect with nothing to announce it: a `max_used` up to
    2 too high, and a `topblk` that can then index past the bitmap.

    So the rule is a blanket one rather than "not on int64": a reader cannot see a variable's kind
    at the call site, and the wrong answer is silent. Every highest-set-bit question this project
    has had is answerable by walking `trailz` and keeping the last index reached, which is what
    `pool_do_compact` now does. If an `int32`-only use is ever genuinely wanted, record the
    exemption HERE with the reason -- do not delete the check.

    See `.claude/rules/fortran-gotchas.md`, "nagfor-specific gotchas", and
    `src/parquet_index_pool.f90`'s `pool_do_compact` header.
    """
    problems = []
    for directory in ("src", "test", "app", "bench", "tools"):
        base = REPO_ROOT / directory
        if not base.is_dir():
            continue
        for path in sorted(base.glob("*.f90")):
            for n, line in enumerate(path.read_text().split("\n"), 1):
                if re.search(r"\bleadz\s*\(", strip_comment(line), re.I):
                    problems.append(
                        "%s:%d: LEADZ is banned in this project -- nagfor 7.2 miscompiles it on "
                        "int64 at -O1 and above (silently, by 2). Walk `trailz` and keep the last "
                        "index reached; see this check's docstring."
                        % (path.relative_to(REPO_ROOT), n))
    return problems


def check_no_shape_nagfor_undefined_cannot_compile():
    """No source shape that nagfor 7.2 cannot compile under `-C=undefined`.

    Only `tools/check_nag_undefined.sh` builds that profile and nothing in CI runs it, while `fpm test`
    compiles every file of `src/`, `test/`, `app/` and `bench/`: one such shape anywhere stops the
    whole gate, and this check is the one place it is seen before someone runs that script. Three
    shapes, each matched by shape (`.claude/rules/fortran-gotchas.md`, "nagfor-specific gotchas"):

    1. the abbreviated `module procedure NAME` body of a separate module FUNCTION whose result is
       `allocatable` and which has an array dummy other than an assumed-size one (the C names the
       dummy's descriptor and definedness map without declaring them). The header is read from the
       interface block, the body from outside one.
    2. a FUNCTION whose result is a procedure pointer (the compiler panics, `No mapinfo.sym?`).
    3. an unsaved LOCAL array -- explicit-shape, automatic or allocatable, in a procedure or a
       `block` -- of a derived type with a non-allocatable, non-pointer component that is
       finalizable, at any depth (the C finalizing it at `end` mixes pointer types). The set of such
       types is derived from every type definition scanned; a type with no such component is not in
       it, whether or not it is finalizable itself.

    Every procedure frame must close by its own name, and none may be left open: a header this
    walker cannot read would otherwise misattribute every later declaration in the file.
    """
    prefix_word = (r"(?:pure|impure|elemental|recursive|non_recursive|module|integer|real|logical"
                   r"|complex|character|double\s+precision|type|class)(?:\s*\([^()]*\))?")
    procedure_start = re.compile(r"^((?:" + prefix_word + r"\s+)*)(function|subroutine)\s+(\w+)\s*(\(|$)",
                                 re.I)
    header_suffix = re.compile(r"^(?:\s*(?:result\s*\(\s*\w+\s*\)|bind\s*\((?:[^()\"']|\"[^\"]*\"|'[^']*')*\)))*\s*$",
                               re.I)
    procedure_close = re.compile(r"^end\s*(function|subroutine|procedure)\b\s*(\w*)", re.I)
    interface_open = re.compile(r"^(?:abstract\s+)?interface\b", re.I)
    interface_close = re.compile(r"^end\s*interface\b", re.I)
    body_open = re.compile(r"^module\s+procedure\s+(\w+)\s*$", re.I)
    block_open = re.compile(r"^(?:\w+\s*:\s*)?block\s*$", re.I)
    block_close = re.compile(r"^end\s*block\b", re.I)
    type_open = re.compile(r"^type\s*(?:,\s*(.*?))?\s*::\s*(\w+)\s*$|^type\s+(?!is\b)(\w+)\s*$", re.I)
    type_close = re.compile(r"^end\s*type\b", re.I)
    derived = re.compile(r"^(?:type|class)\s*\(\s*(\w+)\s*\)(.*)$", re.I)
    paths = []
    for directory in ("src", "test", "app", "bench"):
        base = REPO_ROOT / directory
        if base.is_dir():
            paths.extend(sorted(base.glob("*.f90")))

    def statements(path):
        """Every statement of `path`, a `;`-separated logical line split outside quotes."""
        for lineno, line in _logical_lines(path):
            quote, start = None, 0
            for i, ch in enumerate(line + ";"):
                if quote:
                    quote = None if ch == quote else quote
                elif ch in ("'", '"'):
                    quote = ch
                elif ch == ";":
                    if line[start:i].strip():
                        yield lineno, line[start:i].strip()
                    start = i + 1

    def procedure_header(code):
        """(prefix, kind, name, dummy list, result name or None) for a procedure header, else None."""
        opened = procedure_start.match(code)
        if not opened:
            return None
        if opened.group(4) != "(":
            tail, dummies = code[opened.end():], ""
        else:
            depth = 1
            for i in range(opened.end(), len(code)):
                depth += {"(": 1, ")": -1}.get(code[i], 0)
                if depth == 0:
                    break
            else:
                return None
            tail, dummies = code[i + 1:], code[opened.end():i]
        if not header_suffix.match(tail):
            return None
        result = re.search(r"\bresult\s*\(\s*(\w+)\s*\)", tail, re.I)
        return (opened.group(1), opened.group(2).lower(), opened.group(3).lower(), dummies,
                result.group(1).lower() if result else None)

    problems = []
    types = {}       # type name -> {"final", "parent", "components"}
    shaped = {}      # function name -> array dummies of a `module` interface with an allocatable result
    interfaces = 0
    bodies = []      # (where, lineno, name): every abbreviated body
    locals_ = []     # (where, lineno, type name, entity): every unsaved local array of a derived type
    for path in paths:
        where = path.relative_to(REPO_ROOT)
        depth = 0
        type_def = None
        in_contains = False
        stack = []   # frames: {kind, name, line, interface, dummies, result, allocatable, arrays}
        for lineno, code in statements(path):
            if type_def is not None:
                if type_close.match(code):
                    type_def = None
                elif re.match(r"^contains\s*$", code, re.I):
                    in_contains = True
                elif in_contains:
                    type_def["final"] = type_def["final"] or bool(re.match(r"^final\b", code, re.I))
                else:
                    component = derived.match(code)
                    if component and "::" in component.group(2) and not re.search(
                            r"\b(?:allocatable|pointer)\b", component.group(2).split("::", 1)[0], re.I):
                        type_def["components"].append(component.group(1).lower())
                continue
            opened = type_open.match(code)
            if opened:
                name = (opened.group(2) or opened.group(3)).lower()
                parent = re.search(r"\bextends\s*\(\s*(\w+)\s*\)", opened.group(1) or "", re.I)
                type_def = types.setdefault(name, {"final": False, "parent": None, "components": []})
                if parent:
                    type_def["parent"] = parent.group(1).lower()
                in_contains = False
                continue
            closed = procedure_close.match(code)
            if closed:
                kind = {"function": "function", "subroutine": "subroutine", "procedure": "body"}[
                    closed.group(1).lower()]
                if not stack or stack[-1]["kind"] != kind or (
                        closed.group(2) and closed.group(2).lower() != stack[-1]["name"]):
                    problems.append("%s:%d: `%s` closes no procedure this walker opened -- a header "
                                    "shape it cannot read; fix the walker" % (where, lineno, code))
                    break
                frame = stack.pop()
                if frame["kind"] == "function" and frame["interface"] and frame["module"]:
                    interfaces += 1
                    if frame["allocatable"] and frame["arrays"]:
                        shaped.setdefault(frame["name"], []).extend(frame["arrays"])
                continue
            if block_close.match(code):
                if not stack or stack[-1]["kind"] != "block":
                    problems.append("%s:%d: `end block` closes no block this walker opened -- fix the "
                                    "walker" % (where, lineno))
                    break
                stack.pop()
                continue
            header = procedure_header(code)
            if header:
                prefix, kind, name, dummies, result = header
                stack.append({
                    "kind": kind, "name": name, "line": lineno, "interface": depth > 0,
                    "module": bool(re.search(r"\bmodule\b", prefix, re.I)),
                    "dummies": [d.strip().lower() for d in _split_args(dummies) if d.strip()],
                    "result": result or name, "allocatable": False, "arrays": []})
                continue
            if interface_open.match(code):
                depth += 1
                continue
            if interface_close.match(code):
                depth = max(depth - 1, 0)
                continue
            if depth > 0 and not stack:
                continue    # a generic's `module procedure` list
            opened = body_open.match(code)
            if opened:
                bodies.append((where, lineno, opened.group(1).lower()))
                stack.append({"kind": "body", "name": opened.group(1).lower(), "line": lineno,
                              "interface": False, "module": True, "dummies": [], "result": None,
                              "allocatable": False, "arrays": []})
                continue
            if block_open.match(code) and stack:
                stack.append({"kind": "block", "name": "", "line": lineno, "interface": stack[-1]["interface"],
                              "module": False, "dummies": [], "result": None, "allocatable": False,
                              "arrays": []})
                continue
            if not stack or "::" not in code:
                continue
            frame = stack[-1]
            attrs, entities = code.split("::", 1)
            attr_dims = re.search(r"\bdimension\s*\(([^)]*)\)", attrs, re.I)
            declared_type = derived.match(attrs)
            for entity in _split_args(entities):
                found = re.match(r"\s*(\w+)\s*(?:\(([^)]*)\))?", entity)
                if not found:
                    continue
                name = found.group(1).lower()
                dims = found.group(2) if found.group(2) is not None else (
                    attr_dims.group(1) if attr_dims else None)
                if frame["kind"] == "function" and name == frame["result"]:
                    if re.search(r"\ballocatable\b", attrs, re.I):
                        frame["allocatable"] = True
                    if re.search(r"^\s*procedure\s*\(", attrs, re.I) and re.search(r"\bpointer\b", attrs, re.I):
                        problems.append(
                            "%s:%d: function `%s` returns a procedure pointer -- nagfor -C=undefined "
                            "panics on it (`No mapinfo.sym?`); hand the pointer back through an "
                            "intent(out) dummy of a subroutine (fortran-gotchas.md, nagfor-specific "
                            "gotchas)" % (where, frame["line"], frame["name"]))
                if name in frame["dummies"]:
                    if dims is not None and not dims.strip().endswith("*"):
                        frame["arrays"].append(name)
                    continue
                if (declared_type and dims is not None and not frame["interface"]
                        and name != frame["result"]
                        and not re.search(r"\b(?:save|pointer|parameter|intent)\b", attrs, re.I)):
                    locals_.append((where, lineno, declared_type.group(1).lower(), name))
        else:
            if stack:
                problems.append("%s: %d procedure or block frame(s) left open (%s) -- fix the walker"
                                % (where, len(stack), ", ".join(f["name"] or "block" for f in stack[:3])))

    def finalizable(name, seen=()):
        entry = types.get(name)
        if entry is None or name in seen:
            return False
        seen = seen + (name,)
        return (entry["final"] or bool(entry["parent"] and finalizable(entry["parent"], seen))
                or any(finalizable(c, seen) for c in entry["components"]))

    def through_a_component(name, seen=()):
        entry = types.get(name)
        if entry is None or name in seen:
            return False
        seen = seen + (name,)
        return (any(finalizable(c, seen) for c in entry["components"])
                or bool(entry["parent"] and through_a_component(entry["parent"], seen)))

    if interfaces == 0 or not bodies or not any(finalizable(t) for t in types):
        problems.append(
            "found %d separate module function interface(s), %d abbreviated body(ies) and %d "
            "finalizable type(s) -- a pattern has gone stale, so this check is testing nothing"
            % (interfaces, len(bodies), sum(1 for t in types if finalizable(t))))
    for where, lineno, name in bodies:
        if name in shaped:
            problems.append(
                "%s:%d: `module procedure %s` abbreviates a function with an allocatable result and "
                "an array dummy (%s) -- nagfor -C=undefined cannot compile that form; restate the "
                "interface in full (fortran-gotchas.md, nagfor-specific gotchas)"
                % (where, lineno, name, ", ".join(sorted(set(shaped[name])))))
    for where, lineno, type_name, entity in locals_:
        if through_a_component(type_name):
            problems.append(
                "%s:%d: local array `%s` of `%s`, a type with a finalizable component -- nagfor "
                "-C=undefined cannot compile its finalization; hold the array in an allocatable "
                "component of a local scalar (fortran-gotchas.md, nagfor-specific gotchas)"
                % (where, lineno, entity, type_name))
    return problems


def _pure_callable_names():
    """Names a scenario can call that a compiler is entitled to DELETE when the result is unused.

    Three groups, all read out of `src/` so none goes stale: every `pure`/`elemental` FUNCTION
    declared there; every type-bound binding whose target is one of them; and every NAMED generic
    interface with such a specific behind it. The last two matter because a call site writes
    `m%get(...)` or `parquet_slice_range(...)` -- the binding or the interface, never the specific.
    """
    pure = set()
    for path in sorted(SRC.glob("*.f90")):
        for line in path.read_text().split("\n"):
            found = re.match(r"^\s*(?:pure|elemental)(?:\s+(?:pure|elemental))?\s+"
                             r"(?:recursive\s+)?(?:module\s+)?function\s+(\w+)", line)
            if found:
                pure.add(found.group(1))
    bindings = set()
    for path in sorted(SRC.glob("*.f90")):
        text = re.sub(r"&\s*\n\s*", "", path.read_text())
        # A NAMED generic interface: a call site writes the interface's name, never a specific's.
        for block in re.finditer(r"^\s*interface\s+(\w+)\s*$(.*?)^\s*end interface\b",
                                 text, re.S | re.M):
            # `[\w, \t]` rather than `[\w,\s]`: `\s` includes the newline, and with re.M a
            # greedy match then swallows every following line into one "member".
            members = re.findall(r"^[ \t]*(?:module[ \t]+)?procedure[ \t]*(?:::)?[ \t]*"
                                 r"([\w,][\w, \t]*)$", block.group(2), re.M)
            targets = [t.strip() for group in members for t in group.split(",") if t.strip()]
            if any(t in pure for t in targets):
                bindings.add(block.group(1))
        for line in text.split("\n"):
            line = strip_comment(line)
            found = re.match(r"^\s*generic\s*::\s*(\w+)\s*=>\s*(.+)$", line)
            if found:
                targets = [t for t in re.split(r"[,\s]+", found.group(2)) if t]
                if any(t in pure for t in targets):
                    bindings.add(found.group(1))
                continue
            found = re.match(r"^\s*procedure\s*(?:,[^:]*)?::\s*(\w+)\s*=>\s*(\w+)", line)
            if found:
                if found.group(2) in pure:
                    bindings.add(found.group(1))
                continue
            found = re.match(r"^\s*procedure\s*(?:,[^:]*)?::\s*([\w,\s]+)$", line)
            if found:
                for nm in [t.strip() for t in found.group(1).split(",") if t.strip()]:
                    if nm in pure:
                        bindings.add(nm)
    return pure | bindings


def _joined_statements(path):
    """Yield (first_lineno, joined_statement) for `path`, with `&` continuations folded together."""
    out = []
    buf, first = "", None
    for n, raw in enumerate(path.read_text().split("\n"), 1):
        line = strip_comment(raw).rstrip()
        if first is None:
            if not line.strip():
                continue
            first = n
        if line.endswith("&"):
            buf += line[:-1]
            continue
        buf += line
        out.append((first, buf))
        buf, first = "", None
    return out


def check_scenario_uses_a_pure_result():
    """A scenario whose abort is inside a `pure` function must USE the result, not just assign it.

    Fortran has no way to call a function and discard the result, so an error scenario always has
    to put it somewhere -- and putting it in a variable nothing reads is not enough. A compiler may
    delete a call to a `pure` function whose result is never used, and gfortran does, from `-O1`
    upward. The abort goes with it: the scenario runs to its "was accepted" print and exits 0,
    and the test reports the LIBRARY as having failed to refuse something.

    Confirmed on `scenario_index_tuple_width_mismatch` and `scenario_index_scalar_on_composite`,
    whose `got = m%get(...)` vanished under `--profile release` (`%get` is `pure`) while every
    unoptimised build passed -- so nothing in CI, in a plain `fpm test` or in any nagfor profile
    could see it. Printing `got` is the whole fix.

    Only `pure` callees are flagged, because only they are deletable. That makes the check
    self-maintaining in the direction that matters: the day someone marks an existing procedure
    `pure` -- `parquet_slice_range` and `%view` are the obvious candidates -- every scenario that
    discards its result starts failing here rather than silently testing nothing.

    Every `test/error_scenarios*.f90` is scanned, not just the driver: the scenarios live in the
    four group modules, so naming one file here would leave the other three unchecked.
    """
    paths = sorted(TEST.glob("error_scenarios*.f90"))
    if not paths:
        return ["test/error_scenarios*.f90: not found -- this check needs updating"]
    names = _pure_callable_names()
    if len(names) < 50:
        return ["test/error_scenarios*.f90: found only %d pure callable name(s) in src/ -- this "
                "check has gone blind" % len(names)]
    alternatives = "|".join(sorted(map(re.escape, names), key=len, reverse=True))
    call = re.compile(r"(?<!\w)(?:" + alternatives + r")\s*\(")
    problems = []
    scanned = 0
    for path in paths:
        name, body = None, []
        for n, stmt in _joined_statements(path):
            opened = re.match(r"^\s*subroutine\s+(scenario_\w+)\s*\(", stmt)
            if opened:
                name, body = opened.group(1), []
                continue
            if name and re.match(r"^\s*end subroutine\s+%s\s*$" % name, stmt):
                assigned = {}
                for ln, line in body:
                    found = re.match(r"^\s*([A-Za-z]\w*)\s*=\s*(.+)$", line)
                    if found and call.search(found.group(2)):
                        assigned.setdefault(found.group(1), (ln, found.group(2).strip()))
                for var, (ln, rhs) in sorted(assigned.items()):
                    read = False
                    for _, line in body:
                        if re.match(r"^\s*(?:type|class|integer|real|logical|character|procedure)\b"
                                    r".*::", line):
                            continue           # a declaration is not a read
                        rest = re.sub(r"^(\s*)%s\s*=" % re.escape(var), r"\1", line)
                        if re.search(r"\b%s\b" % re.escape(var), rest):
                            read = True
                            break
                    if not read:
                        problems.append(
                            "%s:%d: %s assigns `%s` from a pure call and never reads it -- the "
                            "optimiser may delete the call, and the abort with it. Print it. See "
                            "this check's docstring."
                            % (path.relative_to(REPO_ROOT), ln, name, var))
                name = None
                continue
            if name:
                body.append((n, stmt))
        scanned += 1
    if not scanned:
        return ["test/error_scenarios*.f90: no dispatch file was scanned -- this check is blind"]
    return problems


#: The four helpers that build a message's file/schema context suffix, one per path the library
#: raises messages from. `.claude/rules/api-conventions.md` requires an `error stop` in a read,
#: write or schema path to append one; a WARNING from the same path says the same thing about the
#: same data and must append one too.
CONTEXT_SUFFIX_HELPERS = (
    "writer_context_suffix", "reader_filename_suffix", "maml_name_suffix", "table_context_suffix",
)

#: Procedures allowed to raise a warning with no suffix even though their file uses a helper,
#: keyed by procedure name and valued by the reason. Each entry must still match a real site --
#: asserted below, because an allow-list nothing re-derives is exactly how the `no direct
#: printing` check went half blind (seven of its nine entries had stopped matching anything).
CONTEXT_SUFFIX_EXEMPT = {
    "parquet_metadata_warn_duplicate":
        "names its table INLINE (\"key 'k' in table 't'\"), which is the schema context at that "
        "point: the procedure takes a parquet_table_metadata and no parquet_maml_file is in "
        "scope, so maml_name_suffix -- the helper its file uses elsewhere -- cannot be called "
        "here at all.",
}


def check_spatial_suite_split_is_by_observability():
    """No test in the parallel `spatial` suite may observe a process-global debug hook.

    `test_spatial.f90` carries two collectors: `collect_tests_parquet_spatial` (the `spatial`
    suite, run concurrently) and `collect_tests_parquet_spatial_serial` (`spatial_serial`, which
    `suite_is_safe_to_parallelize` runs one test at a time). The split exists for speed -- as one
    suite all 130 tests paid the serial cost of the 38 that need it -- and it is only safe while
    membership follows what a test can OBSERVE.

    Reading a counter is as disqualifying as writing a setting. A test that only reads
    `parquet_debug_spatial_probe_count` is charged by every sibling doing spatial work, so run
    concurrently it asserts a number nobody produced -- which PASSES, against a measurement that
    never happened. That is this project's worst failure mode, and it is invisible: the suite
    stays green.

    Derived by SHAPE, not from a list here: every `parquet_debug_*`, `parquet_set_*`, `pf_log_*`
    and `omp_set_*` reference is followed through the file's own call graph, so a test reaching a
    hook through a helper is caught too, and a new hook needs no edit here.
    """
    path = TEST / "test_spatial.f90"
    if not path.is_file():
        return ["test/test_spatial.f90: not found -- this check needs updating"]
    text = path.read_text(encoding="utf-8")
    lines = [strip_comment(ln) for ln in text.splitlines()]

    procs, cur = {}, None
    for ln in lines:
        opened = re.match(r"^    (?:pure |elemental )?(?:subroutine|function) (\w+)", ln)
        if opened:
            cur = opened.group(1)
            procs[cur] = []
        if cur is not None:
            procs[cur].append(ln)
        if re.match(r"^    end (?:subroutine|function)\b", ln):
            cur = None
    if len(procs) < 50:
        return ["test/test_spatial.f90: found only %d procedures -- this check has gone blind"
                % len(procs)]

    hook = re.compile(r"\bparquet_debug_\w+|\bparquet_set_\w+|\bpf_log_\w+|\bomp_set_\w+")
    direct = {n: sorted({m.group(0) for m in hook.finditer("\n".join(b))}) for n, b in procs.items()}
    names = set(procs)
    calls = {n: {w for w in re.findall(r"\b(\w+)\b", "\n".join(b)) if w in names and w != n}
             for n, b in procs.items()}

    def reaches(name, seen=None):
        seen = seen if seen is not None else set()
        if name in seen:
            return []
        seen.add(name)
        found = list(direct.get(name, []))
        for callee in sorted(calls.get(name, ())):
            found += reaches(callee, seen)
        return found

    flat = re.sub(r"&\s*\n\s*", "", "\n".join(lines))
    collectors = {}
    for coll in ("collect_tests_parquet_spatial", "collect_tests_parquet_spatial_serial"):
        found = re.search(r"subroutine %s\(.*?end subroutine %s" % (coll, coll), flat, re.S)
        if not found:
            return ["test/test_spatial.f90: %s not found -- the suite split moved, so this check "
                    "is now blind" % coll]
        collectors[coll] = re.findall(r'new_unittest\(\s*"([^"]*)"\s*,\s*(\w+)', found.group(0))
    if not collectors["collect_tests_parquet_spatial"] or \
            not collectors["collect_tests_parquet_spatial_serial"]:
        return ["test/test_spatial.f90: one of the two spatial collectors registered no test -- "
                "the split's shape changed, so this check is now blind"]

    problems = []
    for desc, sub in collectors["collect_tests_parquet_spatial"]:
        hooks = sorted(set(reaches(sub)))
        if hooks:
            problems.append(
                "test/test_spatial.f90: '%s' (%s) is in the concurrent `spatial` suite but reaches "
                "%s -- a process-global hook. Move it to collect_tests_parquet_spatial_serial: run "
                "beside a sibling it asserts a count nobody produced, and that PASSES."
                % (desc, sub, ", ".join(hooks[:3])))
    return problems


def check_warnings_carry_their_context():
    """A warning from a path that names its file when it ABORTS must name it when it warns.

    The same code path used to identify itself two ways: `parquet_write.f90:205` appended the
    writer's file and maml to its warning, and the qc report four hundred lines later did not --
    so a soft qc violation printed the column and never the file, while a HARD violation on the
    identical data named it. Over a batch of two hundred files that is two hundred warning lines
    nobody can tell apart.

    The rule is anchored on the HELPERS rather than on a list of files or paths, deliberately. A
    file list is a list written here describing a set owned by src/, which goes stale silently in
    the direction that keeps passing -- a new file full of warnings is simply not on it. The
    helper set is small, each entry is grep-able, and its liveness is asserted below.

    "A helper is in scope" is approximated by "this file calls one somewhere", which is an
    over-approximation: a procedure may sit in a file that uses a helper and still have nothing to
    name. That is why CONTEXT_SUFFIX_EXEMPT exists and why it carries a reason per entry rather
    than a bare name. The over-approximation is the cheap half of P4's bargain: a false positive
    costs one reviewed line, a false negative costs an undiagnosable warning.
    """
    problems = []
    defined = set()
    for path in sorted(SRC.glob("*.f90")):
        text = path.read_text()
        for helper in CONTEXT_SUFFIX_HELPERS:
            if re.search(r"^\s*(?:module\s+)?subroutine\s+%s\s*\(" % helper, text, re.M):
                defined.add(helper)
    missing = [h for h in CONTEXT_SUFFIX_HELPERS if h not in defined]
    if missing:
        return ["src/: context-suffix helper(s) %s are named by this check but defined nowhere in "
                "src/ -- they were renamed or removed and this check is now enforcing a rule "
                "against a helper that no longer exists" % ", ".join(sorted(missing))]

    helper_call = re.compile(r"\bcall\s+(?:%s)\s*\(" % "|".join(CONTEXT_SUFFIX_HELPERS))
    used_exemptions = set()
    checked = 0
    for path in sorted(SRC.glob("*.f90")):
        text = path.read_text()
        if "call parquet_emit_warning" not in text:
            continue
        suffix_vars, emits, current = set(), [], ""
        for lineno, stmt in _joined_statements(path):
            m = FORTRAN_PROC_START.match(stmt)
            if m and any(m.groups()):
                current = next(g for g in m.groups() if g)
            if helper_call.search(stmt):
                # The helper's LAST argument is the variable it fills. Taken from the joined
                # statement rather than by a `([^)]*)` match, because two call sites pass an
                # expression of their own -- `table_context_suffix(cache, trim(name), sfx)`.
                tail = stmt.rstrip().rstrip(")")
                if "," in tail:
                    suffix_vars.add(tail.rsplit(",", 1)[1].strip())
            if "call parquet_emit_warning" in stmt:
                emits.append((lineno, current, stmt))
        # No helper is called anywhere in this file, so there is nothing here to unify onto and
        # nothing this check can say. parquet_map/parquet_struct (container columns with no file
        # behind them) and parquet_settings_base/parquet_spatial_* (advice, which names nothing)
        # are the files this skips, and each is a deliberate class rather than an oversight.
        if not suffix_vars:
            continue
        for lineno, proc, stmt in emits:
            checked += 1
            if any(re.search(r"//\s*%s\b" % re.escape(v), stmt) for v in suffix_vars):
                continue
            if proc in CONTEXT_SUFFIX_EXEMPT:
                used_exemptions.add(proc)
                continue
            problems.append(
                "%s:%d: `%s` raises a warning with no file/schema context, in a file whose "
                "messages carry one (%s) -- an `error stop` from this path names the file and "
                "this does not, so the two say different things about the same data. Append the "
                "suffix, or add `%s` to CONTEXT_SUFFIX_EXEMPT with the reason it has nothing to "
                "name."
                % (path.relative_to(REPO_ROOT), lineno, proc or "<file scope>",
                   ", ".join(sorted(suffix_vars)), proc or "<file scope>"))
    if checked == 0:
        return ["src/: this check examined no parquet_emit_warning call at all -- either the emit "
                "channel was renamed or the statement folding stopped working, and the rule is "
                "now enforcing nothing"]
    stale = sorted(set(CONTEXT_SUFFIX_EXEMPT) - used_exemptions)
    if stale:
        problems.append(
            "tools/check_source_conventions.py: CONTEXT_SUFFIX_EXEMPT names %s, which no longer "
            "matches a warning that would otherwise be flagged -- the procedure was renamed, "
            "removed, or has since gained its suffix. Delete the entry: an exemption nobody "
            "re-derives is how the `no direct printing` allow-list went half blind."
            % ", ".join("`%s`" % s for s in stale))
    return problems


def check_no_doc_block_opens_with_a_ford_metadata_key():
    """A MULTI-line doc-comment must not open with a bare `word:` -- FORD eats the line.

    FORD parses the first lines of every entity's doc block as `key: value` metadata
    (`ford/utils.py`'s `META_RE = ^[ ]{0,3}(?P<key>[A-Za-z0-9_-]+):\\s*(?P<value>.*)`, applied by
    `meta_preprocessor` from `ford/sourceform.py`). A key is a SINGLE token -- letters, digits,
    underscore, hyphen, no spaces. An unrecognised one is warned about and then `settings.pop`ped
    (`ford/settings.py`), which removes the line from the documentation entirely.

    Confirmed damage rather than a theoretical one: `parquet_debug_index_threads_used` opened
    `!> Test-only: threads the last %build resolved ...`, and the rendered
    `interface/parquet_debug_index_threads_used.html` began at the SECOND sentence -- a public
    procedure whose page never said what it was for. Fixed by writing `Test-only.` instead.

    **Only a multi-line block is at risk, and that is FORD's own rule, not a simplification.**
    `sourceform.py` guards the single-line case explicitly: when a doc block is one line
    containing a colon and the text before it is not a real settings field, FORD inserts a blank
    line in front, so `meta_preprocessor` stops before consuming anything. That is why
    `src/parquet_tables_access.f90`'s fourteen `!> PK_INT32: smallest and largest value ...`
    headlines are safe and must NOT be reported -- they are one line each. Flagging them would be
    a false positive against a deliberate FORD behaviour, and would also send someone editing a
    GENERATED file. `tools/generate_parquet_tables.py` states the same rule at the template that
    emits them, and states it correctly.

    The hazard is therefore invisible twice over: the rendered page merely lacks a sentence, and
    FORD only warns for an entity it documents -- so the same defect on a private helper is
    silent. CI does not run `ford docs.md` at all, which is why this belongs in the lint stage.
    """
    # FORD's own regex, transcribed from ford/utils.py. It is transcribed rather than imported
    # because FORD is not a dependency of the lint stage; re-check it against a new FORD release.
    meta_re = re.compile(r"^[ ]{0,3}(?P<key>[A-Za-z0-9_-]+):")
    doc_re = re.compile(r"^\s*!([!>])(.*)$")
    # Captures the doc text as well as the marker, because a trailing `!!` OPENS the block it
    # belongs to rather than merely preceding one -- see the loop below.
    trailing_doc_re = re.compile(r"^\s*[^!\s].*!([!>])(.*)$")

    problems, blocks_seen = [], 0
    for path in sorted((REPO_ROOT / "src").glob("*.f90")):
        block, prev_was_doc = [], False
        lines = path.read_text(encoding="utf-8").split("\n")

        def close(block, start_line, path=path):
            """A finished block: flag it when it is multi-line and opens with a metadata key."""
            if not block:
                return
            first = block[0]
            if len(block) >= 2 and meta_re.match(first):
                problems.append(
                    "%s:%d: this doc-comment opens with %r, which FORD parses as a metadata key "
                    "and DELETES from the rendered page -- the single-line exemption does not "
                    "apply because the block runs to %d lines. Reword so the first line has no "
                    "bare `word:` (\"Test-only.\" rather than \"Test-only:\")"
                    % (path.relative_to(REPO_ROOT), start_line,
                       meta_re.match(first).group("key"), len(block)))

        start = 0
        for n, raw in enumerate(lines, 1):
            m = doc_re.match(raw)
            if m:
                marker, body = m.group(1), m.group(2)
                # `!>` always opens a new block; `!!` continues one, and only opens a block when
                # nothing documentable precedes it -- including a trailing `!!` on a code line,
                # which is what makes `src/parquet_core.f90`'s wrapped argument docs mid-block.
                if marker == ">" or not prev_was_doc:
                    close(block, start)
                    blocks_seen += 1
                    block, start = [], n
                block.append(body[1:] if body.startswith(" ") else body)
                prev_was_doc = True
                continue
            close(block, start)
            block = []
            tm = trailing_doc_re.match(raw)
            prev_was_doc = bool(tm)
            if tm:
                # **A trailing `!!` on a code line OPENS the block it belongs to.** FORD reads a
                # declaration's documentation from here, so THIS text is the block's first line
                # and is where a bare `word:` does its damage. Treating the line as merely
                # preceding a block -- which is what setting the flag alone did -- made the check
                # read from the SECOND line, so it could not see the opener at all: that is how
                # `src/parquet_kde.f90`'s `ok` argument kept a `present:` first line, and with it
                # the campaign's one standing FORD warning, while this check passed.
                blocks_seen += 1
                body = tm.group(2)
                block = [body[1:] if body.startswith(" ") else body]
                start = n
        close(block, start)

    if blocks_seen == 0:
        return ["src/: parsed no doc-comment blocks at all -- the comment format moved and this "
                "check has gone blind; re-anchor it deliberately rather than deleting it"]
    return problems


#: The filter operators' four CONSUMING sites, as (label, path, spans). `spans` names the C++
#: procedures whose bodies together form the site; an empty tuple means the whole file; a Markdown
#: path means the guide's grammar-table **Operators** row.
#:
#: Nothing here names an OPERATOR. The vocabulary is derived from the tokenizer that defines it, so
#: a new operator cannot make the check pass by being left out of a list -- which is the whole
#: reason this check can exist at all. What IS enumerated is these four entries, and CLAUDE.md's
#: "A static check that enumerates names goes stale silently" applies to exactly them:
#: `feature_filter.md` section 2.1 tabulates every site that knows the vocabulary and points back
#: here, so a fifth consuming site is added in both places or in neither.
FILTER_OPERATOR_SITES = (
    ("the reader's per-row evaluation", "src/parquet_wrapper.cpp",
     ("eval_filter_clause", "cmp_op_of")),
    ("the row-group statistics screen", "src/parquet_wrapper.cpp",
     ("resolve_screen_leaf", "screen_compare_from_bounds")),
    ("the in-memory (parquet_table) evaluation", "src/parquet_read_eval.f90",
     ("parquet_eval_filter_leaf", "parquet_filter_cmp_of")),
    ("the published vocabulary", "doc/pages/io/filter-sort-sample.md", ()),
)


def cpp_function_body(text, name):
    """Return the text of C++ function `name`'s body, braces matched, or None if it is not found.

    Brace counting skips string and character literals and both comment forms, so a `{` inside a
    diagnostic message cannot end a body early. Matched on the DEFINITION -- a signature line
    followed by a body -- rather than on every mention of the name.
    """
    for m in re.finditer(r"^[ \t]*(?:static\s+)?[A-Za-z_][\w:<>,&* \t]*?\b%s\s*\(" % re.escape(name),
                         text, re.M):
        open_brace = text.find("{", m.end())
        if open_brace < 0:
            continue
        # A declaration without a body (`;` before the next `{`) is not the definition.
        semicolon = text.find(";", m.end())
        if 0 <= semicolon < open_brace:
            continue
        i = open_brace
        depth = 0
        while i < len(text):
            ch = text[i]
            if ch in ('"', "'"):
                quote = ch
                i += 1
                while i < len(text) and text[i] != quote:
                    i += 2 if text[i] == "\\" else 1
            elif text.startswith("//", i):
                i = text.find("\n", i)
                if i < 0:
                    break
            elif text.startswith("/" + "*", i):
                i = text.find("*" + "/", i)
                if i < 0:
                    break
                i += 1
            elif ch == "{":
                depth += 1
            elif ch == "}":
                depth -= 1
                if depth == 0:
                    return text[open_brace:i + 1]
            i += 1
        return None
    return None


def fortran_procedure_body(text, name):
    """Return the text of Fortran procedure `name`'s body, or None if it is not found.

    Matched from its opening statement to the matching `end subroutine <name>` / `end function
    <name>`, so a helper further down the file that happens to mention the same literal is outside
    the span. That precision is the point: a whole-file search would still find an operator's name
    in a worker the dispatch no longer calls.
    """
    # The abbreviated separate-module-procedure form first: `module procedure NAME` has no
    # argument list to match on, and closes with `end procedure NAME`.
    abbreviated = re.search(r"^[ \t]*module procedure\s+%s\s*$" % re.escape(name), text, re.M)
    if abbreviated is not None:
        closing = re.search(r"^[ \t]*end procedure\s+%s\s*$" % re.escape(name),
                            text[abbreviated.end():], re.M)
        if closing is None:
            return None
        return text[abbreviated.start():abbreviated.end() + closing.end()]
    opening = re.search(r"^[ \t]*(?:module\s+)?(?:pure\s+|elemental\s+|recursive\s+)*"
                        r"(?:[\w()=\s,*]*?\s)?(subroutine|function)\s+%s\s*\(" % re.escape(name),
                        text, re.M)
    if opening is None:
        return None
    kind = opening.group(1)
    closing = re.search(r"^[ \t]*end\s+%s\s+%s\s*$" % (kind, re.escape(name)),
                        text[opening.end():], re.M)
    if closing is None:
        return None
    return text[opening.start():opening.end() + closing.end()]


def check_filter_operators_are_handled_everywhere():
    """Every operator the filter grammar accepts must be handled at each site that consumes one.

    The filter vocabulary is known in six places (`feature_filter.md` section 2.1). One of them --
    `parquet_tokenize_filter_rule` -- DEFINES it: an operator exists exactly when that subroutine's
    `select case (trim(op))` has an arm for it. The other five consume it, and they fail very
    differently when one is missed. Four refuse what they do not know, loudly and at the right
    moment. **One guesses**: `cmp_op_of` maps everything that is not one of the five ordering
    spellings to `CmpOp::Ne`, so an operator that reaches it turns into `/=` and the read returns a
    plausible, complete and wrong row set with no diagnostic anywhere (pinned at run time by
    `test_ab_string_match`, test/test_table_verbs.f90, and `test_starts_with_equals_range_oracle`,
    test/test_filter.f90).

    That asymmetry is what makes this a static check rather than a test. A test can only exercise
    an operator someone remembered to write a test for, and the failure being guarded against is
    precisely the one where a site was forgotten -- so the test would be forgotten with it.

    The vocabulary is DERIVED, never listed here. Adding an operator to the tokenizer therefore
    makes this check fail until every consuming site handles it, which is the opposite of the usual
    static-check failure mode where a new name is silently not covered.

    Set-valued operators are exempt from the two C++ sites, and the exemption is derived too, from
    `is_set_valued_op`: an `in`/`not_in` clause is answered in Fortran by `pf_index_map` before the
    reader ever sees it, arriving at the screen as a pre-evaluated verdict (`ScreenLeaf::pre_index`)
    rather than as an operator. Neither C++ site can name them, and requiring it would be wrong.

    The third requirement has nothing to do with coverage: `convert_temporal_filter_values` must
    decide which operators take an ISO-8601 literal with a POSITIVE test
    (`parquet_op_takes_a_temporal_literal`), never a skip list. A skip list is the shape a new
    operator falls THROUGH -- `s starts_with "S18"` on a date column then reports that `"S18"` is
    not a valid ISO-8601 value instead of that `starts_with` needs a string column. Loud, but
    naming the wrong thing, and nothing about it fails.
    """
    problems = []
    read_src = SRC / "parquet_read.f90"
    text = read_src.read_text()

    # (1) The vocabulary, from the one site that defines it.
    tok = re.search(r"subroutine parquet_tokenize_filter_rule\b(.*?)"
                    r"end subroutine parquet_tokenize_filter_rule", text, re.S)
    if tok is None:
        return ["src/parquet_read.f90: could not find parquet_tokenize_filter_rule -- this check "
                "needs updating"]
    body = tok.group(1)
    select = re.search(r"^([ \t]*)select case \(trim\(op\)\)\s*$", body, re.M)
    if select is None:
        return ["src/parquet_read.f90: parquet_tokenize_filter_rule has no "
                "`select case (trim(op))` -- the operator vocabulary is no longer defined where "
                "this check reads it, so this check needs updating"]
    indent = select.group(1)
    arms = body[select.end():]
    stop = re.search(r"^%scase default\s*$" % re.escape(indent), arms, re.M)
    if stop is None:
        return ["src/parquet_read.f90: parquet_tokenize_filter_rule's `select case (trim(op))` has "
                "no `case default` -- this check needs updating"]
    operators = set()
    for line in arms[:stop.start()].split("\n"):
        if re.match(r"^%scase \(" % re.escape(indent), line):
            operators.update(re.findall(r'"([^"]*)"', line))
    if not operators:
        return ["src/parquet_read.f90: read no operator from parquet_tokenize_filter_rule's "
                "`select case (trim(op))` arms -- this check needs updating"]

    # (2) The set-valued operators, derived from the lexer's own classifier: these are answered in
    # Fortran before the C++ side sees them, so neither C++ site names them.
    lexer = (SRC / "parquet_read_filter.f90").read_text()
    setop = re.search(r"function is_set_valued_op\b(.*?)end function is_set_valued_op", lexer, re.S)
    if setop is None:
        return ["src/parquet_read_filter.f90: could not find is_set_valued_op -- this check needs "
                "updating"]
    set_valued = set(re.findall(r'last == "([^"]*)"', setop.group(1)))
    if not set_valued:
        return ["src/parquet_read_filter.f90: is_set_valued_op names no operator in the "
                "`last == \"...\"` shape this check reads -- this check needs updating"]
    if not set_valued <= operators:
        return ["src/parquet_read_filter.f90: is_set_valued_op names %s, which the tokenizer does "
                "not accept -- one of the two is wrong"
                % ", ".join(sorted(set_valued - operators))]

    # (3) Every consuming site handles every operator it can see.
    for label, relpath, spans in FILTER_OPERATOR_SITES:
        path = REPO_ROOT / relpath
        is_cpp = relpath.endswith(".cpp")
        wanted = operators - set_valued if is_cpp else operators
        if relpath.endswith(".md"):
            row = None
            for line in path.read_text().split("\n"):
                if line.strip().startswith("| **Operators** |"):
                    row = line
                    break
            if row is None:
                problems.append(
                    "%s: found no `| **Operators** |` row in the grammar table -- either the table "
                    "was reshaped or it is gone; this check needs updating" % relpath)
                continue
            present = set(re.findall(r"`([^`]*)`", row))
        else:
            source = path.read_text()
            if spans:
                extract = cpp_function_body if is_cpp else fortran_procedure_body
                chunks = []
                for name in spans:
                    chunk = extract(source, name)
                    if chunk is None:
                        problems.append(
                            "%s: could not find the body of `%s`, one of the procedures that make "
                            "up %s -- this check needs updating" % (relpath, name, label))
                        chunks = None
                        break
                    chunks.append(chunk)
                if chunks is None:
                    continue
                source = "\n".join(chunks)
            present = set(re.findall(r'"([^"]*)"', source))
            present.update(re.findall(r"'([^']*)'", source))
        for op in sorted(wanted - present):
            problems.append(
                "%s: the filter grammar accepts `%s`, but %s never names it%s. Every operator "
                "%s accepts must be handled at all four sites in feature_filter.md section 2.1."
                % (relpath, op, label,
                   " (searched %s)" % ", ".join(spans) if spans else "",
                   "parquet_tokenize_filter_rule"))

    # (4) The temporal-literal decision is a positive test, not a skip list.
    conv = re.search(r"subroutine convert_temporal_filter_values\b(.*?)"
                     r"end subroutine convert_temporal_filter_values", text, re.S)
    if conv is None:
        problems.append("src/parquet_read.f90: could not find convert_temporal_filter_values -- "
                        "this check needs updating")
    else:
        conv_body = "\n".join(strip_comment(line) for line in conv.group(1).split("\n"))
        if "parquet_op_takes_a_temporal_literal" not in conv_body:
            problems.append(
                "src/parquet_read.f90: convert_temporal_filter_values does not call "
                "parquet_op_takes_a_temporal_literal -- it must decide which operators take an "
                "ISO-8601 literal with a positive test, so an operator added later is skipped by "
                "default rather than falling through into the temporal conversion")
        for literal in sorted(set(re.findall(r'\bop == "([^"]*)"', conv_body))):
            problems.append(
                "src/parquet_read.f90: convert_temporal_filter_values compares `op == \"%s\"` "
                "directly. A skip list here is the shape a new operator falls THROUGH: it would be "
                "handed to the ISO-8601 parser and reported as an invalid date rather than as an "
                "operator the column type does not accept. Extend "
                "parquet_op_takes_a_temporal_literal instead." % literal)
    return problems


def check_instruction_citations_resolve():
    """A comment citing an instruction file by topic must name the file that still carries it.

    Source, test, tooling and CI comments cite the working rules by file and quoted title. The
    rules moved out of CLAUDE.md into
    `.claude/rules/*.md`, and a citation left naming the old home sends a reader to a file that no
    longer holds the text. `tools/check_doc_anchors.py` cannot see any of this: it resolves
    Markdown links, and these are prose inside `.py`/`.sh`/`.f90`/`.cpp`/`.yml`.

    A citation resolves when the named file exists and the quoted text matches one of its headings
    or appears in its body. Quoting is what makes a citation checkable, so an unquoted pointer
    ("see CLAUDE.md") is deliberately out of scope -- it stays correct whatever moves.

    SCOPE, so a green run is not read as more than it is: the citation must sit on ONE line. A
    quoted topic wrapped across two comment lines is invisible here, and a stale one will not be
    reported; audit those by hand with
    `grep -rn 'CLAUDE.md' tools/ bench/ test/ src/ .gitlab-ci.yml`. Prefer citing the FILE without
    a quoted title in new comments -- a title duplicates wording the rule owns, and drifts the
    first time the rule is reworded, which is what left the wrapped ones behind.

    When a rule is reworded, this check fires on every comment quoting the old wording: repoint the
    comment, or quote the new heading. Moving a rule between files fires it the same way.
    """
    instr = {}
    for rel in ["CLAUDE.md"] + sorted(str(p.relative_to(REPO_ROOT))
                                      for p in (REPO_ROOT / ".claude").rglob("*.md")):
        path = REPO_ROOT / rel
        if path.is_file():
            instr[rel] = path.read_text()
    if not instr:
        return ["no instruction files found -- this check proves nothing; fix its file list"]

    def norm(text):
        text = text.lower().replace("`", "").replace("*", "")
        return re.sub(r"\s+", " ", re.sub(r"[^a-z0-9 ]+", " ", text)).strip()

    heads, bodies = {}, {}
    for rel, text in instr.items():
        bodies[rel] = norm(text)
        for line in text.split("\n"):
            if line.startswith("#"):
                heads.setdefault(norm(line.lstrip("# ")), set()).add(rel)

    # (?<![\"']) keeps the pattern off its own source: a bare "CLAUDE.md" string literal in this
    # file is not a citation, only a mention inside prose is.
    cite = re.compile(r"(?<![\"'])(CLAUDE\.md|\.claude/(?:rules|skills)/[\w.-]+\.md)"
                      r"`?\s*(?:'s)?\s*[,:]?\s*(?:own\s+)?[\"\u201c]([^\"\u201d\n]{4,90})[\"\u201d]")
    # Only what the repository carries: the root-level `feature_*.md` planning documents are
    # git-ignored scratch, so scanning them would give CI and a working tree different answers.
    scanned = [REPO_ROOT / name for name in
               (".gitlab-ci.yml", "README.md", "CONTRIBUTING.md", "CHANGELOG.md", "feature_risks.md")]
    for directory in ("src", "test", "app", "bench", "tools", "doc"):
        base = REPO_ROOT / directory
        if base.is_dir():
            scanned.extend(sorted(base.rglob("*")))
    problems, seen = [], 0
    for path in scanned:
        if not path.is_file():
            continue
        rel = str(path.relative_to(REPO_ROOT))
        if not rel.endswith((".py", ".sh", ".f90", ".cpp", ".yml", ".md")):
            continue
        try:
            text = path.read_text()
        except (OSError, UnicodeDecodeError):
            continue
        for n, line in enumerate(text.split("\n"), 1):
            for match in cite.finditer(line):
                target, topic = match.group(1), match.group(2)
                seen += 1
                if target not in instr:
                    problems.append("%s:%d: cites `%s`, which does not exist" % (rel, n, target))
                    continue
                key = norm(topic)
                if target in heads.get(key, ()) or key in bodies[target]:
                    continue
                elsewhere = sorted(heads.get(key, set()) | {f for f, b in bodies.items() if key in b})
                where = (" -- it is in %s" % ", ".join(elsewhere)) if elsewhere else ""
                problems.append(
                    "%s:%d: cites %s's \"%s\", which that file does not carry%s"
                    % (rel, n, target, topic, where))
    if not seen:
        return ["no instruction citations found at all -- the pattern has gone stale, so a "
                "green result here proves nothing; fix this check before trusting it"]
    return problems


#: The tracked open-risks register.
RISK_REGISTER = REPO_ROOT / "feature_risks.md"
#: Most open entries the register may hold; past it, entries are closed or replaced, not added.
RISK_REGISTER_MAX_ENTRIES = 15
#: Most body lines one entry may carry (blank edges stripped); past it, the entry is cut down.
RISK_REGISTER_MAX_BODY_LINES = 15
#: Where the register's rules live, named in every message this check prints:
#: .claude/rules/workflow.md, "The `feature_risks.md` open-risks register".
RISK_REGISTER_RULE = '.claude/rules/workflow.md, "The `feature_risks.md` open-risks register"'
#: An entry heading: `### Risk-<n> — <title>`.
RISK_ENTRY_HEADING = re.compile(r"^### Risk-(\d+) — \S")
#: The register's one next-number line.
RISK_NEXT_NUMBER = re.compile(r"^Next number: Risk-(\d+)\s*$")
#: A citation of a register entry anywhere in prose.
RISK_CITATION = re.compile(r"\bRisk-(\d+)\b")


def risk_register_problems(text, name="feature_risks.md"):
    """Parse an open-risks register's TEXT; return (open entry numbers, problems).

    Split from `check_risk_register_shape` so the parsing can be exercised on a synthetic register.
    Headings inside a fenced code block are body text, not structure.
    """
    lines = text.split("\n")
    problems = []
    entries = []          # (number, heading line number, body line count)
    next_numbers = []     # (value, line number)
    current = None        # [number, heading line number, body lines]
    in_fence = False

    def close(entry):
        if entry is None:
            return
        body = entry[2]
        while body and not body[0].strip():
            body.pop(0)
        while body and not body[-1].strip():
            body.pop()
        entries.append((entry[0], entry[1], len(body)))

    for lineno, line in enumerate(lines, start=1):
        if line.lstrip().startswith("```"):
            in_fence = not in_fence
        if not in_fence:
            found = RISK_NEXT_NUMBER.match(line)
            if found:
                next_numbers.append((int(found.group(1)), lineno))
            if line.startswith("## ") or line.startswith("### "):
                close(current)
                current = None
                if line.startswith("### "):
                    heading = RISK_ENTRY_HEADING.match(line)
                    if heading:
                        current = [int(heading.group(1)), lineno, []]
                    else:
                        problems.append(
                            "%s:%d: a `### ` heading that is not a register entry -- every `### ` "
                            "heading must read `### Risk-<n> — <title>`; fold this text into an "
                            "entry or the header prose (%s):\n    %s"
                            % (name, lineno, RISK_REGISTER_RULE, line.strip()[:100]))
                continue
        if current is not None:
            current[2].append(line)
    close(current)

    seen = {}
    for number, lineno, body_lines in entries:
        if number in seen:
            problems.append(
                "%s:%d: Risk-%d is already the entry at line %d -- a number names one entry; merge "
                "the two or give the new one the next number (%s)"
                % (name, lineno, number, seen[number], RISK_REGISTER_RULE))
        else:
            seen[number] = lineno
        if body_lines > RISK_REGISTER_MAX_BODY_LINES:
            problems.append(
                "%s:%d: Risk-%d's body is %d lines, over the limit of %d -- cut it to the risk, "
                "why it is silent, and what would close it (%s)"
                % (name, lineno, number, body_lines, RISK_REGISTER_MAX_BODY_LINES, RISK_REGISTER_RULE))
    if len(entries) > RISK_REGISTER_MAX_ENTRIES:
        problems.append(
            "%s: %d open entries, over the limit of %d -- close or replace an entry (%s)"
            % (name, len(entries), RISK_REGISTER_MAX_ENTRIES, RISK_REGISTER_RULE))
    if len(next_numbers) != 1:
        problems.append(
            "%s: the line `Next number: Risk-<n>` occurs %d times, not once -- keep exactly one, "
            "in the header before the entries (%s)" % (name, len(next_numbers), RISK_REGISTER_RULE))
    elif entries and next_numbers[0][0] <= max(number for number, _, _ in entries):
        problems.append(
            "%s:%d: `Next number: Risk-%d` is not above every entry number (highest: Risk-%d) -- "
            "numbers are never reused; set it past the highest ever issued (%s)"
            % (name, next_numbers[0][1], next_numbers[0][0],
               max(number for number, _, _ in entries), RISK_REGISTER_RULE))
    return set(seen), problems


def risk_citation_problems(open_numbers, sources):
    """Report every `Risk-<n>` citation in SOURCES ((name, text) pairs) naming no open entry."""
    problems = []
    for name, text in sources:
        for lineno, line in enumerate(text.split("\n"), start=1):
            for found in RISK_CITATION.finditer(line):
                if int(found.group(1)) not in open_numbers:
                    problems.append(
                        "%s:%d: cites %s, which is not an open entry of feature_risks.md -- cite the "
                        "rule (`.claude/rules/<file>.md`, \"<section>\") or the test that states the "
                        "requirement instead (%s)" % (name, lineno, found.group(0), RISK_REGISTER_RULE))
    return problems


def check_risk_register_shape():
    """`feature_risks.md` stays a short register of OPEN risks, and nothing cites a closed one.

    The register holds at most `RISK_REGISTER_MAX_ENTRIES` entries of at most
    `RISK_REGISTER_MAX_BODY_LINES` body lines each; every `### ` heading is an entry heading
    (`### Risk-<n> — <title>`), no number repeats, and exactly one `Next number: Risk-<n>` line
    sits above every entry number. The instruction and CI files (CLAUDE.md, CONTRIBUTING.md,
    .gitlab-ci.yml, `.claude/**/*.md`, and this script) cite only open entries: a closed entry's
    requirement lives in the rule or the test that states it, and a citation of it leads nowhere.
    Comments in `src/`, `test/`, `tools/` and `bench/` are exempt -- a closed number there is
    attribution. Rules for the register: `.claude/rules/workflow.md`.
    """
    if not RISK_REGISTER.is_file():
        return ["feature_risks.md: not found -- the register is tracked and must exist (%s)"
                % RISK_REGISTER_RULE]
    open_numbers, problems = risk_register_problems(RISK_REGISTER.read_text(encoding="utf-8"))
    paths = [REPO_ROOT / "CLAUDE.md", REPO_ROOT / "CONTRIBUTING.md", REPO_ROOT / ".gitlab-ci.yml",
             Path(__file__).resolve()]
    paths += sorted((REPO_ROOT / ".claude").rglob("*.md"))
    sources = [(str(path.relative_to(REPO_ROOT)), path.read_text(encoding="utf-8", errors="replace"))
               for path in paths if path.is_file()]
    if len(sources) < 4:
        problems.append("check_risk_register_shape found only %d of the files it scans -- its file "
                        "list has gone stale, so a green citation result proves nothing" % len(sources))
    problems += risk_citation_problems(open_numbers, sources)
    return problems


#: A declaration with an entity list: the type spec and attributes, then the names after `::`.
#: The type spec's own parentheses (`character(len=*)`, `real(real64)`) are part of the attribute
#: half and never read as a shape -- reading them as one is what would make every `character(len=*)`
#: dummy look like an array.
_SHAPED_DECL = re.compile(
    r"^((?:integer|real|logical|complex|character|double\s+precision|type|class)"
    r"\s*(?:\([^)]*\))?[^:]*)::(.+)$", re.I)
_DIMENSION_ATTR = re.compile(r"\bdimension\s*\(([^)]*)\)", re.I)
#: An actual that is an array constructor: `[...]` or the older `(/ ... /)`.
_ARRAY_CONSTRUCTOR = re.compile(r"^(\[|\(/)")
#: An actual that opens with a procedure reference or a subscript -- which of the two is decided
#: against the names in scope, never from the spelling.
_OPENS_WITH_CALL = re.compile(r"^([a-z_]\w*)\s*\(", re.I)
#: A reference at a call site: `call name(`, `call obj%binding(`, or a function reference.
_CALL_OR_REFERENCE = re.compile(r"(?<![\w%])(?:call\s+)?([a-z_]\w*(?:%[a-z_]\w*)*)\s*\(", re.I)
#: Statement keywords that take a parenthesis and are not calls.
_NOT_CALLABLE = frozenset(("if", "elseif", "while", "where", "elsewhere", "forall", "select",
                           "selectcase", "case", "do", "allocate", "deallocate", "nullify",
                           "associate", "type", "class", "read", "write", "open", "close",
                           "inquire", "print", "format", "data", "common", "namelist"))


def _declared_shape(entity, attributes):
    """('name', 'explicit'|'assumed'|'scalar') for one declared entity.

    `assumed` covers assumed-shape, deferred-shape and assumed-size alike: none of them is this
    rule's business, because an actual reaching one is passed by descriptor or by address.
    """
    name, _, rest = entity.partition("(")
    if not rest:
        found = _DIMENSION_ATTR.search(attributes)
        spec = found.group(1) if found else None
    else:
        depth, spec = 1, ""
        for ch in rest:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    break
            spec += ch
    name = name.strip().lower()
    if not spec or not spec.strip():
        return name, "scalar"
    if "*" in spec or ":" in spec:
        return name, "assumed"
    return name, "explicit"


def _entity_shapes(code):
    """Yield (name, shape) for every entity a declaration line declares."""
    found = _SHAPED_DECL.match(code)
    if not found:
        return
    for entity in _split_args(found.group(2)):
        entity = entity.split("=>")[0]
        if "=" in entity:
            entity = entity.split("=")[0]
        name, shape = _declared_shape(entity, found.group(1))
        if name:
            yield name, shape


def _shape_index(paths):
    """Index every procedure's dummy shapes, and which functions return an array.

    Returns (shapes, array_functions, ambiguous):
      shapes           name -> (ordered dummy names, {dummy: shape})
      array_functions  names whose RESULT is an explicit-shape array
      ambiguous        names declared with two different signatures somewhere in the tree

    An `ambiguous` name is resolved to nothing at all: two procedures of the same name in
    different scopes cannot be told apart here, and guessing would report the wrong one.
    """
    shapes, array_functions, ambiguous = {}, set(), set()
    for path in paths:
        stack = []
        for _, code in _logical_lines(path):
            opened = _PROC_OPEN.match(code)
            if opened and not re.match(r"^end\b", code, re.IGNORECASE):
                name = (opened.group(1) or opened.group(3) or opened.group(6)).lower()
                dummies = [d.strip().lower()
                           for d in _split_args(opened.group(2) or opened.group(4) or "")]
                dummies = [d for d in dummies if re.fullmatch(r"\w+", d)]
                result = re.search(r"\bresult\s*\(\s*(\w+)\s*\)", code, re.IGNORECASE)
                stack.append({
                    "name": name,
                    "dummies": dummies,
                    "shape": {},
                    "result": result.group(1).lower() if result else
                              (name if opened.group(3) else None),
                    "abbreviated": bool(opened.group(5)),
                })
                continue
            if _PROC_CLOSE.match(code):
                if stack:
                    frame = stack.pop()
                    if frame["abbreviated"]:
                        continue
                    shaped = {d: frame["shape"].get(d, "scalar") for d in frame["dummies"]}
                    record = (frame["dummies"], shaped)
                    if frame["name"] in shapes and shapes[frame["name"]] != record:
                        ambiguous.add(frame["name"])
                    shapes[frame["name"]] = record
                    if frame["result"] and frame["shape"].get(frame["result"]) == "explicit":
                        array_functions.add(frame["name"])
                continue
            if stack and "::" in code:
                for name, shape in _entity_shapes(code):
                    stack[-1]["shape"][name] = shape
    return shapes, array_functions, ambiguous


def _explicit_shape_dummies(callee, shapes, generics, bindings, ambiguous):
    """(positions, names) of the callee's explicit-shape ARRAY dummies; empty when unresolvable.

    A generic or a type-bound binding is resolved against every candidate specific and only what
    they AGREE on is reported, so a name whose specifics differ costs nothing. A binding's
    candidates are indexed by their implementation, whose passed object is the first dummy, so
    positions are shifted by one.
    """
    if "%" in callee:
        candidates = bindings.get(callee.rsplit("%", 1)[1])
        offset = 1
    else:
        candidates = generics.get(callee) or ([callee] if callee in shapes else None)
        offset = 0
    if not candidates:
        return {}, set()
    per_candidate = []
    for candidate in candidates:
        if candidate in ambiguous or candidate not in shapes:
            return {}, set()
        dummies, shaped = shapes[candidate]
        dummies = dummies[offset:]
        if len(dummies) != len(shapes[candidate][0]) - offset:
            return {}, set()
        per_candidate.append({i: d for i, d in enumerate(dummies)
                              if shaped.get(d) == "explicit"})
    if not per_candidate:
        return {}, set()
    positions = {i: d for i, d in per_candidate[0].items()
                 if all(p.get(i) == d for p in per_candidate)}
    names = set.intersection(*[set(p.values()) for p in per_candidate])
    return positions, names


def _call_argument_text(code, open_paren):
    """The text between `code[open_paren]` and its matching `)`, quotes respected."""
    depth, out, quote = 0, "", None
    for ch in code[open_paren:]:
        if quote:
            out += ch
            if ch == quote:
                quote = None
            continue
        if ch in ("'", '"'):
            quote = ch
        elif ch == "(":
            depth += 1
            if depth == 1:
                continue
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return out
        out += ch
    return None                                     # unbalanced: a continuation this line lost


def _bare_operands(text):
    """(names referenced bare at nesting depth 0, whether an operator sits there).

    A name followed by `(` is dropped: `v(3)`, `a(i:j)` and `f(x)` are a subscript, a section and
    a reference, none of which says anything about the whole actual's rank on its own.
    """
    depth, current, names, has_operator, quote = 0, "", [], False, None
    for ch in text:
        if quote:
            if ch == quote:
                quote = None
            continue
        if ch in ("'", '"'):
            quote = ch
        elif ch in "([":
            if depth == 0:
                current = ""
            depth += 1
        elif ch in ")]":
            depth -= 1
        elif depth == 0:
            if ch.isalnum() or ch in "_%":
                current += ch
            else:
                if ch in "-+*/":
                    has_operator = True
                if current:
                    names.append(current.lower())
                current = ""
    if current:
        names.append(current.lower())
    return names, has_operator


def _needs_a_temporary(actual, in_scope, array_functions):
    """Why this actual reaches an explicit-shape dummy through a temporary, or None.

    Three shapes have no address of their own, and only these three are reported:
    an array constructor, the result of an array-valued function, and an array expression. A plain
    name, a section (`v(:, k)`, `a(i:j)`), a component and anything unresolvable are left alone --
    a section's own copy, when it is strided, is a different rule with a different fix.
    """
    text = actual.strip()
    if _ARRAY_CONSTRUCTOR.match(text):
        return "an array constructor"
    names, has_operator = _bare_operands(text)
    opening = _OPENS_WITH_CALL.match(text)
    if opening and not has_operator:
        name = opening.group(1).lower()
        if name in array_functions and name not in in_scope:
            return "the array-valued result of `%s`" % name
    if has_operator:
        for name in names:
            if in_scope.get(name) in ("explicit", "assumed"):
                return "an array expression over `%s`" % name
    return None


def check_no_array_temporary_at_an_explicit_shape_dummy():
    """No call hands an array temporary to an explicit-shape array dummy.

    `.claude/rules/fortran-gotchas.md`, ifx. An array constructor, an array-valued function result
    and an array expression have no address of their own, so a compiler passing one to an
    explicit-shape dummy copies it into a temporary first. ifx reports every such copy at run time
    under `--profile debug` (`-check all`), one `forrtl: warning (406)` line per CALL: in a sampler
    that is one per draw, and six such actuals inside `pf_random_*` once put two million lines
    through a single `fpm test --profile debug`. gfortran makes the same copy silently, so nothing
    but an ifx debug run shows it, and by then the real output is buried.

    The fix is always at the CALL: name the value in a local (or a `parameter` when it is
    constant) and pass that.

    DELIBERATELY NARROW, because a check that cried wolf here would be turned off:

    * only the three shapes above are reported. An array SECTION is not one of them -- `v(:, k)`
      and `a(i:j)` have addresses, and the copy a strided one costs is a different rule with a
      different fix -- and neither is a plain name, a component, or a `parameter`.
    * a dummy is reported only when its shape is EXPLICIT (`vec(3)`, `mat(3, 3)`). Assumed-shape,
      deferred-shape and assumed-size dummies all take a descriptor or an address and are skipped.
    * a name whose signature is not indexed here (an intrinsic, a `use`d third-party procedure) is
      skipped, as is one declared twice with different signatures, and a generic or type-bound
      binding reports only what EVERY candidate specific agrees on.
    * an expression is reported only when one of its top-level operands is a name declared an
      array IN THE CALLING PROCEDURE, so a scalar expression over array elements (`a(i) * 2`) is
      not mistaken for an array one.

    The shapes are derived from the source on both sides -- the dummies from the callee's
    declarations, the arrays in scope from the caller's -- so a new procedure is covered the day it
    is written, with no list to update. Both directions are guarded against passing vacuously: the
    index must find explicit-shape dummies, and the scan must resolve call sites.
    """
    roots = [SRC, TEST, REPO_ROOT / "app", REPO_ROOT / "bench"]
    paths = sorted(p for root in roots if root.is_dir() for p in root.glob("*.f90"))
    if not paths:
        return ["no Fortran source found under src/, test/, app/ or bench/ -- this check is "
                "passing vacuously; fix its file list"]
    shapes, array_functions, ambiguous = _shape_index(paths)
    _, generics, bindings, _ = _fortran_signatures(paths)
    # A generic counts as array-valued when EVERY specific behind it is: `pf_random_disc_at` is
    # what a caller writes, and only its `_i32`/`_i64` specifics carry the declaration.
    array_functions |= {name for name, specifics in generics.items()
                        if specifics and all(s in array_functions for s in specifics)}
    explicit_dummies = sum(1 for _, shaped in shapes.values()
                           for shape in shaped.values() if shape == "explicit")
    if not explicit_dummies or not array_functions:
        return ["the shape index found %d explicit-shape dummy/dummies and %d array-valued "
                "function(s) across %d file(s) -- the declaration parser has gone stale and this "
                "check proves nothing" % (explicit_dummies, len(array_functions), len(paths))]
    problems, resolved = [], 0
    for path in paths:
        lines = stripped_lines(path)
        for first, last in procedure_scopes(path):
            in_scope = {}
            for code in lines[first:last + 1]:
                for name, shape in _entity_shapes(code.strip()):
                    in_scope[name] = shape
            for lineno, code in _joined_lines(lines, first, last):
                for found in _CALL_OR_REFERENCE.finditer(code):
                    callee = found.group(1).lower()
                    if callee in _NOT_CALLABLE:
                        continue
                    positions, names = _explicit_shape_dummies(
                        callee, shapes, generics, bindings, ambiguous)
                    if not positions and not names:
                        continue
                    argument_text = _call_argument_text(code, found.end() - 1)
                    if argument_text is None:
                        continue
                    resolved += 1
                    for index, actual in enumerate(_split_args(argument_text)):
                        keyword = _KEYWORD_ACTUAL.match(actual)
                        if keyword:
                            if keyword.group(1).lower() not in names:
                                continue
                            actual, dummy = keyword.group(2), keyword.group(1).lower()
                        elif index in positions:
                            dummy = positions[index]
                        else:
                            continue
                        reason = _needs_a_temporary(actual, in_scope, array_functions)
                        if reason:
                            problems.append(
                                "%s:%d: `%s` is passed %s as its `%s` argument, which is "
                                "explicit-shape -- ifx copies it into an array temporary and "
                                "reports `warning (406)` on every call under --profile debug. "
                                "Name the value in a local (a `parameter` when it is constant) "
                                "and pass that (.claude/rules/fortran-gotchas.md, ifx)."
                                % (path.relative_to(REPO_ROOT), lineno, callee, reason, dummy))
    if not resolved:
        return problems + [
            "no call site reached a procedure with an explicit-shape array dummy -- the call-site "
            "scan has gone stale and this check proves nothing"]
    return problems


def _joined_lines(lines, first, last):
    """(lineno, code) over `lines[first:last]`, `&` continuations folded.

    `lines` is already comment-stripped (`stripped_lines`), so this only folds.
    """
    out, buffer, start = [], "", None
    for index in range(first, last + 1):
        code = lines[index].strip()
        if not code and not buffer:
            continue
        if start is None:
            start = index + 1
        if code.endswith("&"):
            buffer += code[:-1] + " "
            continue
        out.append((start, buffer + code))
        buffer, start = "", None
    return out



#: Real literals whose value is SUBNORMAL for the kind they are written in, kept deliberately.
#: One entry per (file, literal), each carrying what a build with abrupt underflow makes of it,
#: because that is the thing an entry has to be justified against.
#:
#: Nothing else in `src/` or `test/` may write one. Where a subnormal value IS the fixture, build
#: it from `tiny`/`epsilon` or from its bit pattern (`transfer(1_int64, 0.0_real64)`) so the
#: intent survives the read, and guard the assertion with the test file's own
#: `subnormals_are_flushed()`; where it is only a small number, move it into the normal range.
SUBNORMAL_LITERAL_ALLOWED = {
    # Empty, and it has been since the one entry it was written with was fixed rather than
    # excused: `test_the_smallest_matter_density_builds_quietly` (`test/test_cosmology.f90`) used
    # to pass `om0 = 1.0e-320_real64` and, under abrupt underflow, never reach the
    # `sqrt(or0)/sqrt(om0)` line it exists to pin -- `om0` arrived at `cosmology_prepare_sound` as
    # 0 and its `if (om0 > 0.0_real64 ...)` guard was false. `tiny(1.0_real64)` still overflows
    # `or0/om0` there and is positive under every model, so the line is reached everywhere; proved
    # by mutating the library to the root-of-ratio form, which the old fixture let through under
    # ifx and the new one fails on.
}


def check_no_subnormal_literals():
    """No `src/` or `test/` source may write a real literal that is subnormal for its own kind.

    **ifx turns flush-to-zero and denormals-are-zero on at `-O1` and above, and a flagless
    `fpm test` builds at ifx's own default `-O2`.** A subnormal literal therefore reaches the
    library as `0.0` there, on the comparison that reads it, whatever the compiler folded it to.
    Three things follow, and this project has now seen all three:

    - the fixture is silently a different one (`test_the_smallest_matter_density_builds_quietly`
      asked about a matter density of ZERO, and the branch it was written to pin was not reached
      -- a library mutation it exists to catch survived under ifx and was caught under gfortran);
    - a guard fires that the test was not asking about (`kde_grid_cell_width_underflow`'s control
      call was refused for a range of zero width, so the abort it does assert proved nothing);
    - an `error stop` is raised from inside a CONCURRENT suite, which takes the runner down on
      SIGSEGV naming no test and leaves every suite after it unrun -- how `a reflecting grid
      whose mass underflows poisons itself` presented, and the reason this check exists.

    ifx is not in CI (`.gitlab-ci.yml` builds gfortran only), so nothing else catches any of them.

    Matched by SHAPE: every exponent-bearing real literal in the tree is read and compared against
    `tiny` for the kind it is written in -- `_real32`/`_sp` against the single-precision floor, a
    `d` exponent or `_real64`/`_dp` against the double one, and a bare `e` literal against the
    single floor, since that is the default-real value the standard gives it.
    """
    lit = re.compile(r"(?<![\w.])(\d+\.?\d*|\.\d+)[eEdD]([-+]?\d+)(_(\w+))?")
    quoted = re.compile(r"'[^']*'|\"[^\"]*\"")
    tiny32, tiny64 = 1.1754943508222875e-38, 2.2250738585072014e-308

    problems, matched, scanned = [], set(), 0
    for path in sorted(list(SRC.glob("*.f90")) + list(TEST.glob("*.f90"))):
        rel = str(path.relative_to(REPO_ROOT))
        for lineno, raw in enumerate(stripped_lines(path), 1):
            for m in lit.finditer(quoted.sub("", raw)):
                scanned += 1
                mantissa, exponent, kind = m.group(1), m.group(2), (m.group(4) or "").lower()
                letter = m.group(0)[len(mantissa)].lower()
                if kind in ("real32", "sp"):
                    floor, floor_name = tiny32, "tiny(1.0_real32)"
                elif kind in ("real64", "dp") or letter == "d":
                    floor, floor_name = tiny64, "tiny(1.0_real64)"
                else:
                    floor, floor_name = tiny32, "tiny(1.0) -- a bare exponent is DEFAULT REAL"
                value = float(mantissa + "e" + exponent)
                if not 0.0 < value < floor:
                    continue
                if (rel, m.group(0)) in SUBNORMAL_LITERAL_ALLOWED:
                    matched.add((rel, m.group(0)))
                    continue
                problems.append(
                    "%s:%d: `%s` is subnormal for its kind (below %s), so a build with abrupt "
                    "underflow -- ifx at `-O1` and above, which is what a flagless `fpm test` "
                    "selects -- reads it as 0.0 and the fixture is not the one written here. "
                    "Build the value from `tiny`/`epsilon` or from its bit pattern and guard the "
                    "assertion with `subnormals_are_flushed()`, or move the fixture into the "
                    "normal range." % (rel, lineno, m.group(0), floor_name))
    if scanned < 1000:
        return ["src/ and test/: only %d exponent-bearing real literals were read -- this check "
                "has gone blind and is no longer looking at the tree" % scanned]
    for stale in sorted(SUBNORMAL_LITERAL_ALLOWED.keys() - matched):
        problems.append(
            "tools/check_source_conventions.py: SUBNORMAL_LITERAL_ALLOWED names `%s` in %s, which "
            "is no longer written there. Delete the entry -- an allow-list nobody re-derives is "
            "how a check goes half blind." % (stale[1], stale[0]))
    return problems


def check_every_serial_suite_records_its_reason():
    """Every suite excluded from test-drive's per-test parallelism says in the file WHY.

    `suite_is_safe_to_parallelize` (`test/test_runner_support.f90`) is a list of suite names that
    run one test at a time. The list is load-bearing in a way that is invisible when it is wrong:
    a suite that belongs on it and is not on it runs its tests concurrently and PASSES, against a
    counter a sibling moved or a parallel arm that was really the serial one -- and a suite that is
    on it for no reason anybody recorded is a second of wall time nobody can argue with, because
    the argument was never written down.

    So the rule is that an entry carries a reason in the same file, and this check holds the list
    to it: every `name == "<suite>"` must appear, quoted or in backticks, in a comment there. It
    does not read the reason -- no check can -- but a paragraph naming the suite is what makes the
    next reader able to challenge it.

    Written after an audit claimed two entries were undocumented and the count was actually TEN:
    `sorting_cpp`, `columns_parallel`, `index_omp`, `optimize_omp`, `prima_omp`, `sphere_omp`,
    `random_perm`, `stats`, `module_surface` and `cosmology_serial`. Nothing had gone wrong,
    which is the point -- a list nobody re-derives drifts quietly, and the drift is only ever
    found by someone counting by hand.
    """
    path = TEST / "test_runner_support.f90"
    if not path.is_file():
        return ["test/test_runner_support.f90: not found -- this check needs updating"]
    text = stripped_text(path)
    marker = "logical function suite_is_safe_to_parallelize"
    if marker not in text:
        return ["test/test_runner_support.f90: suite_is_safe_to_parallelize is gone -- this check "
                "has gone blind"]
    names = sorted(set(re.findall(r'name == "(\w+)"', text[text.index(marker):])))
    if len(names) < 20:
        return ["test/test_runner_support.f90: only %d excluded suite names were read from "
                "suite_is_safe_to_parallelize -- this check has gone blind" % len(names)]
    prose = "\n".join(ln for ln in source_lines(path) if ln.lstrip().startswith("!"))
    problems = []
    for name in names:
        if not re.search(r'["`]%s["`]' % re.escape(name), prose):
            problems.append(
                'test/test_runner_support.f90: the suite "%s" is excluded from per-test '
                "parallelism and no comment in this file names it. Add a paragraph to the block "
                "above suite_is_safe_to_parallelize saying which process-global it writes or "
                "reads, or which comparison would collapse to the serial path inside test-drive's "
                "own region -- an entry whose reason is not written down cannot be challenged, "
                "and cannot be removed when the reason goes away." % name)
    return problems


def check_spatial_cell_caps_are_named_from_their_parameters():
    """The two cells-per-point ceilings read the same in the parameter, the advice and the page.

    `spatial_max_cells_per_point` (occupied cells per point) and `spatial_max_box_cells_per_point`
    (cells of the bounding box per point) are the two ceilings a coarsened grid obeys. Three places
    state them and only ONE computes with them: the parameter declarations in
    `src/parquet_spatial.f90`; the coarsening advice in `src/parquet_spatial_build.f90`, which
    writes both figures into its message as literal text; and `doc/pages/utilities/spatial.md`,
    which tells the reader they are "the figures the coarsening warning names when it fires".

    **Change either parameter and the other two go quietly wrong.** The grid obeys the new ceiling,
    the user is told the old number, and nothing fails: `test_two_ceilings_bind` hardcodes its own
    `3/10` and `4` as well, so the suite agrees with the message rather than with the parameter.
    Nobody reads an advice string against a declaration two files away.

    Written to read the NARROWEST source -- the two parameter declarations -- and to FAIL rather
    than pass when it cannot find them, the advice, or the page's ceiling bullet. A check that
    enumerates figures and goes blind is worse than no check.
    """
    decl = SRC / "parquet_spatial.f90"
    build = SRC / "parquet_spatial_build.f90"
    page = REPO_ROOT / "doc" / "pages" / "utilities" / "spatial.md"
    for path in (decl, build, page):
        if not path.is_file():
            return ["%s: not found -- this check needs updating" % path.relative_to(REPO_ROOT)]

    text = decl.read_text(encoding="utf-8")
    wanted = {}
    for name in ("spatial_max_cells_per_point", "spatial_max_box_cells_per_point"):
        found = re.search(r"::\s*%s\s*=\s*([0-9]+(?:\.[0-9]*)?)_real64" % name, text)
        if found is None:
            return ["src/parquet_spatial.f90: %s is not declared as a real64 parameter -- this "
                    "check has gone blind" % name]
        wanted[name] = "%g" % float(found.group(1))

    # Each figure is matched where it is SAID, never merely somewhere nearby: the page carries
    # "0.3" in five places, so a window-wide search passes a page whose ceiling bullet was changed.
    def named_before(text, anchor, value, where):
        found = re.search(r"([0-9]+(?:\.[0-9]*)?) %s" % re.escape(anchor), text)
        if found is None:
            return ["%s: no %r to read a ceiling from -- this check has gone blind and needs "
                    "re-pointing at the wording that replaced it" % (where, anchor)]
        if found.group(1) != value:
            return ["%s: says %s before %r, but the parameter is %s -- a reader is told a ceiling "
                    "the grid no longer obeys" % (where, found.group(1), anchor, value)]
        return []

    problems = []
    advice = re.search(r'"([^"]*occupied cells[^"]*)"', build.read_text(encoding="utf-8"))
    if advice is None:
        return ["src/parquet_spatial_build.f90: no coarsening advice naming occupied cells -- "
                "this check has gone blind"]
    said = advice.group(1)
    where = "src/parquet_spatial_build.f90 (the coarsening advice)"
    problems += named_before(said, "occupied cells", wanted["spatial_max_cells_per_point"], where)
    problems += named_before(said, "cells of the bounding box per point",
                             wanted["spatial_max_box_cells_per_point"], where)

    flat = " ".join(page.read_text(encoding="utf-8").split())
    where = "doc/pages/utilities/spatial.md"
    problems += named_before(flat, "occupied cells per point",
                             wanted["spatial_max_cells_per_point"], where)
    problems += named_before(flat, "cells of the bounding box per point",
                             wanted["spatial_max_box_cells_per_point"], where)
    problems += named_before(flat, "per point, counting every pixel",
                             wanted["spatial_max_cells_per_point"], where)
    return problems


CHECKS = (
    ("the spatial cell caps are named from their parameters",
     check_spatial_cell_caps_are_named_from_their_parameters),
    ("the comment stripper's fast paths agree with its loop",
     check_comment_stripper_fast_paths_agree),
    ("every instruction citation names the file that carries the topic",
     check_instruction_citations_resolve),
    ("feature_risks.md is a short register of open risks, and only open ones are cited",
     check_risk_register_shape),
    ("LEADZ is not used anywhere (nagfor miscompiles it on int64)", check_no_leadz),
    ("no call hands an array temporary to an explicit-shape dummy",
     check_no_array_temporary_at_an_explicit_shape_dummy),
    ("no source shape nagfor -C=undefined cannot compile",
     check_no_shape_nagfor_undefined_cannot_compile),
    ("pf_index_map components are adopted and reset",
     check_index_map_components_are_adopted_and_reset),
    ("every impure parquet_index abort goes through ix_abort",
     check_index_aborts_go_through_reporter),
    ("every error scenario uses the pure result it computes",
     check_scenario_uses_a_pure_result),
    ("threads= is forwarded to every callee that takes it", check_threads_are_forwarded),
    ("generated table types forward every parquet_open_table argument",
     check_open_table_arguments_are_forwarded),
    ("generated-tables.md's %init row matches the generated signature",
     check_generated_init_signature_is_documented),
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
    ("the two MAML generators share one emitter", check_maml_generators_share_their_emitter),
    ("the schema-less write declares auto sizes", check_schemaless_write_declares_auto),
    ("row-group reads guard against a sort", check_row_group_reads_guard_against_sort),
    ("print_stat's columns match its documentation", check_print_stat_columns_documented),
    ("the column-type mapping matches its documentation", check_column_type_mapping_documented),
    ("sorting.md's threads= inventory matches the generics",
     check_sorting_threads_inventory_documented),
    ("a guide page's counted list has that many bullets",
     check_counted_lists_match_their_bullets),
    ("a documented call signature matches the procedure's arguments",
     check_documented_signatures_match_source),
    ("%agg's exact family and option refusals match the source",
     check_agg_vocabulary_matches_its_documentation),
    ("the guide's canonical optional-argument order matches the checker's",
     check_stats_optional_order_documented),
    ("the guide's two `weights` lists match the source", check_stats_weights_documented),
    ("the guide's two `pf_kde_grid%merge` refusal lists match the source",
     check_kde_merge_refusals_documented),
    ("print_settings matches its documentation", check_print_settings_documented),
    ("every setting is actually read", check_settings_are_read),
    ("no direct printing outside the emit channels", check_no_direct_printing),
    ("no direct printing in the C++ half either", check_no_direct_printing_cpp),
    ("every warning carries the context its path's aborts carry",
     check_warnings_carry_their_context),
    ("the row-group sizing arithmetic exists once", check_row_group_sizing_not_duplicated),
    ("src/ is a single C++ translation unit", check_single_cpp_translation_unit),
    ("the C++ side draws no random numbers of its own", check_one_random_number_generator),
    ("every setting has an environment variable", check_env_covers_every_setting),
    ("the guide's environment table matches the source", check_env_table_matches_the_source),
    ("the logging guide's environment table matches the source",
     check_log_env_table_matches_the_source),
    ("parquet_strings does not reach parquet_bindings", check_parquet_strings_stays_leaf),
    ("parquet_argsort stays Arrow-free", check_parquet_argsort_stays_arrow_free),
    ("parquet_index stays Arrow-free", check_parquet_index_stays_arrow_free),
    ("parquet_sorting stays Arrow-free", check_parquet_sorting_stays_arrow_free),
    ("parquet_sampling stays Arrow-free", check_parquet_sampling_stays_arrow_free),
    ("parquet_columns stays Arrow-free", check_parquet_columns_stays_arrow_free),
    ("parquet_list stays Arrow-free", check_parquet_list_stays_arrow_free),
    ("parquet_struct stays Arrow-free", check_parquet_struct_stays_arrow_free),
    ("parquet_map stays Arrow-free", check_parquet_map_stays_arrow_free),
    ("parquet_utils stays Arrow-free", check_parquet_utils_stays_arrow_free),
    ("parquet_temporal stays Arrow-free", check_parquet_temporal_stays_arrow_free),
    ("parquet_version stays Arrow-free", check_parquet_version_stays_arrow_free),
    ("parquet_spatial stays Arrow-free", check_parquet_spatial_stays_arrow_free),
    ("parquet_healpix stays Arrow-free", check_parquet_healpix_stays_arrow_free),
    ("parquet_sphere stays Arrow-free", check_parquet_sphere_stays_arrow_free),
    ("parquet_skycoord stays Arrow-free", check_parquet_skycoord_stays_arrow_free),
    ("parquet_cosmology stays Arrow-free", check_parquet_cosmology_stays_arrow_free),
    ("parquet_cosmology_config stays Arrow-free", check_parquet_cosmology_config_stays_arrow_free),
    ("the [cosmology] section's keys are %init's arguments", check_cosmology_config_keys_match_init),
    ("the [cosmology] key table is documented", check_cosmology_config_keys_documented),
    ("the named cosmologies are listed once and counted right", check_cosmology_named_documented),
    ("parquet_cosmology_config's named list is the library's",
     check_cosmology_config_names_match_the_library),
    ("z_drag's bracket is the same in the library and its oracle",
     check_cosmology_drag_bracket_matches_the_oracle),
    ("parquet_logging stays Arrow-free", check_parquet_logging_stays_arrow_free),
    ("parquet_toml stays Arrow-free", check_parquet_toml_stays_arrow_free),
    ("every parquet_toml entry takes the module guard", check_parquet_toml_takes_the_guard),
    ("parquet_utils is total: every procedure pure, no error stop",
     check_parquet_utils_is_total),
    ("parquet_stats stays Arrow-free", check_parquet_stats_stays_arrow_free),
    ("parquet_integrate stays Arrow-free", check_parquet_integrate_stays_arrow_free),
    ("integrate aborts documented", check_integrate_aborts_documented),
    ("integrate status codes documented", check_integrate_status_codes_documented),
    ("parquet_interpolate stays Arrow-free", check_parquet_interpolate_stays_arrow_free),
    ("interpolate aborts are documented", check_interpolate_aborts_documented),
    ("optimisation aborts are documented", check_optimize_aborts_documented),
    ("root-finding aborts are documented", check_root_aborts_documented),
    ("parquet_optimize stays Arrow-free", check_parquet_optimize_stays_arrow_free),
    ("parquet_prima stays Arrow-free", check_parquet_prima_stays_arrow_free),
    ("parquet_root stays Arrow-free", check_parquet_root_stays_arrow_free),
    ("the four solver modules share one vocabulary", check_solver_vocabulary),
    ("parquet_transform stays Arrow-free", check_parquet_transform_stays_arrow_free),
    ("parquet_transform holds no state", check_parquet_transform_holds_no_state),
    ("parquet_kde stays Arrow-free", check_parquet_kde_stays_arrow_free),
    ("every sum in the vendored PRIMA tier is the ordered one", check_prima_sums_are_the_ordered_sum),
    ("parquet_stats and parquet_kde optionals each follow one canonical order",
     check_stats_optional_argument_order),
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
    ("no error scenario is named twice", check_scenario_list_has_no_duplicates),
    ("the test runners partition the suites", check_test_runner_partition),
    ("test fixtures live under test_run/", check_test_fixtures_live_under_test_run),
    ("every intent(inout) temporal setter assigns all components", check_temporal_setters_assign_all),
    ("doc/pages index files agree with the page tree", check_doc_page_index_consistency),
    ("the landing page names every entry module", check_landing_page_names_every_entry_module),
    ("page titles match their list entries", check_page_titles_match_their_list_entries),
    ("the entry-module tables match the measured footprints",
     check_module_tables_match_the_measured_footprints),
    ("prose footprint counts match the measured footprints", check_prose_footprint_counts),
    ("no doc/pages code fence is indented", check_no_indented_code_fence),
    ("a guide page spells an optional argument one way", check_bracket_convention),
    ("no multi-line doc block opens with a FORD metadata key",
     check_no_doc_block_opens_with_a_ford_metadata_key),
    ("no function returns a deferred-length allocatable character",
     check_no_deferred_length_character_result),
    ("no call aliases one variable onto a writable dummy", check_no_aliased_output_argument),
    ("parquet_random takes array lengths as int64", check_fill_size_kind),
    ("allocate extents from size() use int64", check_allocate_extent_kind),
    ("noinline directives carry both spellings", check_noinline_directives_are_paired),
    ("CONTRIBUTING.md names each tool once, in its index", check_contributing_is_an_index),
    ("no statement exceeds 255 continuation lines", check_statement_continuation_lines),
    ("the affinity clamp's area names are documented", check_affinity_areas_documented),
    ("parquet_set_threads' fan-out count is documented", check_set_threads_fanout_documented),
    ("the entry-module settings table matches the re-exports",
     check_module_settings_reexports_documented),
    ("every %view call site declares its column target",
     check_view_call_sites_declare_target),
    ("every %join specific forwards every argument it takes",
     check_join_specifics_forward_every_argument),
    ("every kind-carrying spatial specific forwards every argument it takes",
     check_spatial_kind_specifics_forward_every_argument),
    ("every filter operator is handled at every consuming site",
     check_filter_operators_are_handled_everywhere),
    ("no source writes a subnormal real literal", check_no_subnormal_literals),
    ("every serially-run suite records why", check_every_serial_suite_records_its_reason),
    ("the spatial suite split follows what a test can observe",
     check_spatial_suite_split_is_by_observability),
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
