#!/usr/bin/env bash
#
# Assert that tools/generate_parquet_maml.sh still emits a module that compiles against this
# library.
#
# WHY THIS EXISTS. There are two MAML generators. tools/generate_parquet_maml_base.sh emits
# src/parquet_maml_base.f90 for this repository and is checked on every pipeline run
# (.gitlab-ci.yml's lint stage, and tools/run_lint_check.sh, both `--check`). The other,
# tools/generate_parquet_maml.sh, is the CONSUMER-FACING one documented in
# doc/pages/utilities/embedding-maml-schemas.md: a downstream project copies the script, points it
# at its own .maml files, and commits the src/parquet_maml.f90 it emits.
#
# Nothing in this repository compiles that second generator's output, and the two do not emit the
# same text. The base module carries NO `use` statement at all; the consumer-facing one emits
#
#     use parquet_io, only: parquet_schema, parquet_load_maml_file
#     use parquet_io, only: parquet_parse_maml, parquet_validate_user_maml
#
# plus a `call parquet_parse_maml(schema)` in every accessor and a whole `set_maml` body. Rename any
# of those four, move parquet_schema between modules, stop parquet_io re-exporting one, or change
# parquet_load_maml_file's signature, and the base generator's `--check` stays green, `fpm test`
# stays green, the lint stage stays green -- and every downstream project's next regeneration fails
# to compile, with an error naming a file this script wrote. The library would have no way to know
# it had broken them.
#
# That is not hypothetical here: the schema/building-schema-in-code.md review removed the
# parquet_parse_maml ordering requirement and swept 141 call sites, and generate_parquet_maml.sh
# still emits such a call into every downstream accessor.
#
# IT COMPILES, IT DOES NOT RUN. Compiling is what catches the failure mode above, and it needs
# nothing but the library's own .mod files -- no linking, no Arrow link line, no dependency resolve.
# Running the emitted module would additionally exercise parquet_parse_maml at run time, and costs a
# full link against Arrow; test/test_reading.f90's test_get_parquet_maml_examples already covers the
# shared select-case template through the base module.
#
# WHAT THAT LEAVES UNCOVERED, since a check is only useful if its limits are stated: the CONTENT of
# the extracted schema. An invalid data_type in the guide page's ```yaml block would be caught by
# parquet_parse_maml at run time and is invisible here -- confirmed by mutating it and watching
# every check stay green. What is covered is every Fortran claim the page makes: that the emitted
# module compiles, and that the page's own example program typechecks against it (mutating a single
# argument in that example does fail this script, with the compiler's own message).
#
# IF THIS FAILS, the fix is in the library or in the generator's template text -- never in this
# script's expectations. A downstream project cannot patch around a renamed public name.
#
# Usage:  tools/check_downstream_maml_module.sh
#   FC=<compiler>   Fortran compiler to use (default: $FPM_FC, else gfortran). Must be the same
#                   compiler whose .mod files are under build/, since .mod format is per-compiler.
#
# bash 3.2 only (macOS ships 3.2): no associative arrays, no mapfile, no ${var,,}.
# ---------------------------------------------------------------------------------------------
# Protects the consumer-facing `tools/generate_parquet_maml.sh`. There are two MAML generators:
# `generate_parquet_maml_base.sh`, which emits `src/parquet_maml_base.f90` for this repository and
# is checked on every pipeline run (`--check`, in the `lint` stage), and `generate_parquet_maml.sh`,
# which a downstream project copies and runs on its own schemas -- see "Embedding your own MAML
# schemas in your own project" (doc/pages/utilities/embedding-maml-schemas.md). The two do not emit
# the same text: the base module carries no `use` statement at all, while the consumer-facing one
# imports four names from `parquet_io` (`parquet_schema`, `parquet_load_maml_file`,
# `parquet_parse_maml`, `parquet_validate_user_maml`), calls `parquet_parse_maml` in every accessor,
# and carries a whole `set_maml` body. Rename any of those four and the base `--check`, `fpm test`
# and the lint stage all stay green while every downstream project's next regeneration fails to
# compile -- this library would have no way to know it had broken them. The script builds a
# throwaway project in a temporary directory (nothing is written inside this repository), generates
# from it and compiles the result against the library's own `.mod` files. It compiles rather than
# runs, which is what makes it cheap enough for every pipeline: no linking, no Arrow link line, no
# dependency resolve. Its eight checks also cover the generator's own refusals -- a duplicate
# `.maml` filename, a non-ASCII schema, `--check` drift in both directions, `--module=<name>` --
# the exact SET of modules the emitted code imports (compiling can only show that an
# import resolves, never that one was added), and the page agreeing with itself about what its
# example prints. Two of the eight are deliberate negative controls:
# one renames an imported name and requires the compile to fail, the other adds `use parquet` and
# requires the import-set assertion to notice. So a run that had silently stopped compiling
# anything, or stopped matching any `use` line, cannot report success. It runs in CI's `test` job
# rather than `lint`, because it needs a Fortran compiler and a built library and the lint image
# has neither.
#
#     tools/check_downstream_maml_module.sh          # builds the library first if needed
#     FC=ifx tools/check_downstream_maml_module.sh   # follows FPM_FC when FC is unset
# ---------------------------------------------------------------------------------------------
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

FC="${FC:-${FPM_FC:-gfortran}}"

if ! command -v "$FC" >/dev/null 2>&1; then
    echo "check_downstream_maml_module.sh: compiler '$FC' not found on PATH" >&2
    exit 2
fi
if ! command -v fpm >/dev/null 2>&1; then
    echo "check_downstream_maml_module.sh: fpm not found on PATH (needed to build the .mod files)" >&2
    exit 2
fi

WORK="$(mktemp -d)"
# `finished` guards against the script stopping early and still exiting 0. A check whose failure
# mode is a green report is worse than no check, because it also removes the doubt that would have
# prompted a look. See `.claude/rules/testing.md`, "`tools/*.sh` checks".
finished=0
trap '[ "$finished" = "1" ] || { echo "check_downstream_maml_module.sh: TERMINATED EARLY -- this run proves nothing" >&2; rm -rf "$WORK"; exit 2; }' EXIT

fail=0
report() {  # report <ok|FAIL> <description>
    if [ "$1" = "ok" ]; then
        echo "  [ok]   $2"
    else
        echo "  [FAIL] $2" >&2
        fail=1
    fi
}

# ---- The library's own .mod files -------------------------------------------------------------
# fpm build is idempotent and fast when the tree is already built, which it is in CI (the test
# stage runs `fpm test` first). Several build trees can exist on a dev machine that has built with
# more than one FPM_FFLAGS; take the most recently written parquet_io.mod -- the module the emitted
# code actually imports -- and SAY WHICH, because a stale one is the only way this check could pass
# against a library it did not describe.
echo "check_downstream_maml_module.sh: building the library ($FC) ..."
if ! fpm build >"$WORK/fpm-build.log" 2>&1; then
    echo "check_downstream_maml_module.sh: fpm build failed; see below" >&2
    tail -n 20 "$WORK/fpm-build.log" >&2
    exit 2
fi

MOD_DIR=""
NEWEST=""
for m in $(find "${FPM_BUILD_DIR:-build}" -name 'parquet_io.mod' -type f 2>/dev/null); do
    if [ -z "$NEWEST" ] || [ "$m" -nt "$NEWEST" ]; then
        NEWEST="$m"
    fi
done
if [ -z "$NEWEST" ]; then
    echo "check_downstream_maml_module.sh: no parquet_io.mod found under ${FPM_BUILD_DIR:-build}/" >&2
    exit 2
fi
MOD_DIR="$(dirname "$NEWEST")"
echo "check_downstream_maml_module.sh: using .mod files from $MOD_DIR"

# ---- A downstream project, in a temporary directory --------------------------------------------
# Nothing is written inside this repository. The generator resolves everything from its cwd, so the
# whole fixture lives under $WORK.
PROJ="$WORK/proj"
mkdir -p "$PROJ/schemas/dr2"

# THE FIXTURE IS THE GUIDE PAGE'S OWN EXAMPLE, lifted out of it rather than copied. The page tells
# a downstream reader to write that schema and that program; taking both from the page means the
# example itself is what gets compiled, so it cannot drift from the library the way a copy here
# silently would. This is the same both-or-neither rule test/test_examples.f90 applies to the
# mirrored examples -- that file cannot host this one, because the module it imports (parquet_maml)
# exists only in a downstream project.
PAGE="doc/pages/utilities/embedding-maml-schemas.md"
python3 - "$PAGE" "$PROJ/schemas/mysurvey.maml" "$PROJ/example.f90" <<'PY'
import pathlib, sys

page = pathlib.Path(sys.argv[1]).read_text().splitlines()


def first_block(tag):
    out, inside = [], False
    for line in page:
        if line.startswith('```'):
            if inside:
                return out
            inside = (line.strip() == '```' + tag)
            continue
        if inside:
            out.append(line)
    raise SystemExit(f'{sys.argv[1]}: no ```{tag} block found')


for tag, dest in (('yaml', sys.argv[2]), ('fortran', sys.argv[3])):
    body = first_block(tag)
    if not body:
        raise SystemExit(f'{sys.argv[1]}: the first ```{tag} block is empty')
    pathlib.Path(dest).write_text('\n'.join(body) + '\n')
PY
if [ ! -s "$PROJ/schemas/mysurvey.maml" ] || [ ! -s "$PROJ/example.f90" ]; then
    echo "check_downstream_maml_module.sh: could not extract the example from $PAGE" >&2
    exit 2
fi
# A nested schema too, so the recursive scan and the relative-path case label are compiled, not
# merely generated.
cp "$PROJ/schemas/mysurvey.maml" "$PROJ/schemas/dr2/downstream_nested.maml"

GEN="$ROOT_DIR/tools/generate_parquet_maml.sh"

compile_module() {  # compile_module <file> <objdir>; echoes nothing, returns the compiler's status
    mkdir -p "$2"
    "$FC" -c -I"$ROOT_DIR/$MOD_DIR" -J"$2" -o "$2/mod.o" "$1" 2>"$2/err.txt"
}

echo "check_downstream_maml_module.sh: checks"

# 1. The documented recipe: run it with no options, compile what it emits.
( cd "$PROJ" && "$GEN" >"$WORK/gen1.log" 2>&1 )
if [ ! -f "$PROJ/src/parquet_maml.f90" ]; then
    report FAIL "the generator wrote src/parquet_maml.f90"
    cat "$WORK/gen1.log" >&2
else
    report ok "the generator wrote src/parquet_maml.f90 (src/ created for it)"
    if compile_module "$PROJ/src/parquet_maml.f90" "$WORK/o1"; then
        report ok "the emitted module compiles against the library"
        # And the page's own example program compiles against that module. Compiling, not linking,
        # so no Arrow link line is needed -- which is still enough to typecheck every call the
        # example makes, including the writer calls.
        mkdir -p "$WORK/oex"
        if "$FC" -c -I"$ROOT_DIR/$MOD_DIR" -I"$WORK/o1" -J"$WORK/oex" \
                 -o "$WORK/oex/example.o" "$PROJ/example.f90" 2>"$WORK/oex/err.txt"; then
            report ok "the guide page's own example program compiles against it"
        else
            report FAIL "the guide page's own example program compiles against it"
            sed -n '1,25p' "$WORK/oex/err.txt" >&2
        fi
    else
        report FAIL "the emitted module compiles against the library"
        sed -n '1,25p' "$WORK/o1/err.txt" >&2
    fi
fi

# 2. THE IMPORT SET, EXACTLY -- not merely that the four names resolve.
#    Check 1 proves those four names EXIST in parquet_io. Nothing in it would notice one being
#    ADDED: if the shared emitter ever grew a `use parquet` line, the emitted module would compile
#    BETTER rather than worse, every check in this repository would stay green, and every
#    downstream project would silently go back to compiling the whole library surface -- the very
#    failure this script exists for, in the one direction that compiling cannot see. It is also
#    the only thing holding up the guide page's "It does not import parquet_maml_base at all",
#    which is a claimed absence and so has no compile that can demonstrate it.
modules_used() {  # modules_used <file>; the modules it imports, one per line, sorted, unique
    sed -n 's/^[[:space:]]*use[[:space:]]*,*[[:space:]]*\([A-Za-z_][A-Za-z_0-9]*\).*/\1/p' "$1" \
        | sort -u
}
USED="$(modules_used "$PROJ/src/parquet_maml.f90")"
if [ "$USED" = "parquet_io" ]; then
    report ok "the emitted module imports parquet_io and nothing else"
else
    report FAIL "the emitted module imports parquet_io and nothing else (found: $(echo "$USED" | tr '\n' ' '))"
fi
#    THE NEGATIVE CONTROL for that assertion, for the same reason check 5 below has one: a
#    `modules_used` whose pattern had stopped matching would report the empty set, which is not
#    "parquet_io", so THAT direction fails safe -- but a pattern matching too little on ONE added
#    line would not. Add the import this check exists to catch and require it to be seen.
awk '{ print }
     /^[ ]*use parquet_io, only: parquet_schema/ && !seen { print "    use parquet"; seen = 1 }' \
    "$PROJ/src/parquet_maml.f90" > "$WORK/widened.f90"
if cmp -s "$PROJ/src/parquet_maml.f90" "$WORK/widened.f90"; then
    report FAIL "negative control: the emitted module should carry the parquet_io import line"
elif [ "$(modules_used "$WORK/widened.f90")" = "parquet_io" ]; then
    report FAIL "negative control: an added \`use parquet\` must NOT go unseen"
else
    report ok "negative control: an added \`use parquet\` is seen"
fi

# 3. --module=<name>: same content, different module and filename.
( cd "$PROJ" && "$GEN" --module=probe_pkg_maml >"$WORK/gen2.log" 2>&1 )
if [ -f "$PROJ/src/probe_pkg_maml.f90" ] && compile_module "$PROJ/src/probe_pkg_maml.f90" "$WORK/o2"; then
    report ok "--module=<name> emits and compiles src/<name>.f90"
else
    report FAIL "--module=<name> emits and compiles src/<name>.f90"
    [ -f "$WORK/o2/err.txt" ] && sed -n '1,25p' "$WORK/o2/err.txt" >&2
fi

# 4. --check round-trips, and notices an edited schema. Both directions: a --check that always
#    passed would be indistinguishable from one that had stopped finding the file.
if ( cd "$PROJ" && "$GEN" --check >/dev/null 2>&1 ); then
    report ok "--check passes against a freshly generated module"
else
    report FAIL "--check passes against a freshly generated module"
fi
printf 'comments:\n- edited by check_downstream_maml_module.sh\n' >> "$PROJ/schemas/mysurvey.maml"
if ( cd "$PROJ" && "$GEN" --check >/dev/null 2>&1 ); then
    report FAIL "--check reports drift after a schema is edited"
else
    report ok "--check reports drift after a schema is edited"
fi
( cd "$PROJ" && "$GEN" >/dev/null 2>&1 )

# 5. THE NEGATIVE CONTROL, and the reason the whole script is evidence rather than decoration.
#    Break exactly what this check exists to catch -- one of the four names the emitted module
#    imports from `parquet_io` -- and require the compile to FAIL. Without this, a check that had
#    silently stopped compiling anything (a moved .mod directory, an -I that resolves to nothing)
#    would report success forever.
sed 's/parquet_validate_user_maml/parquet_validate_user_maml_x/g' \
    "$PROJ/src/parquet_maml.f90" > "$WORK/broken.f90"
if cmp -s "$PROJ/src/parquet_maml.f90" "$WORK/broken.f90"; then
    report FAIL "negative control: the emitted module names parquet_validate_user_maml"
elif compile_module "$WORK/broken.f90" "$WORK/o3"; then
    report FAIL "negative control: a renamed import must NOT compile"
else
    report ok "negative control: a renamed import does not compile"
fi

# 6. The generator refuses a filename collision instead of emitting duplicate Fortran, and writes
#    nothing when it refuses. Two .maml files sharing a filename would otherwise produce a
#    duplicate `public ::`, a duplicate function and a duplicate `case` label, with the generator
#    reporting success and the consumer's build failing on a line naming neither.
mkdir -p "$PROJ/schemas/dr3"
cp "$PROJ/schemas/dr2/downstream_nested.maml" "$PROJ/schemas/dr3/downstream_nested.maml"
rm -f "$PROJ/src/parquet_maml.f90"
if ( cd "$PROJ" && "$GEN" >"$WORK/gen5.log" 2>&1 ); then
    report FAIL "a duplicate .maml filename is refused"
elif [ -f "$PROJ/src/parquet_maml.f90" ]; then
    report FAIL "a refused run writes nothing"
elif grep -q 'dr2/downstream_nested.maml' "$WORK/gen5.log" &&
     grep -q 'dr3/downstream_nested.maml' "$WORK/gen5.log"; then
    report ok "a duplicate .maml filename is refused, naming both files, writing nothing"
else
    report FAIL "the refusal names both colliding files"
    cat "$WORK/gen5.log" >&2
fi
rm -rf "$PROJ/schemas/dr3"

# 7. A non-ASCII schema is refused, and the refusal writes nothing. This one is here because it
#    once did the opposite: the emitted module is written with encoding='ascii', so a byte above
#    127 came out as a UnicodeEncodeError traceback from write_text -- AFTER pathlib had truncated
#    src/parquet_maml.f90 to zero bytes. A downstream project was left an empty module, a Python
#    stack trace and no indication of which schema was at fault. The zero-length test below is the
#    half that regressed, so it is asserted separately from the exit status.
printf 'dataset: probe_bad\ntable: bad\nfields:\n- name: ra\n  data_type: float64\n  unit: (deg\xc2\xb0)\n' \
    > "$PROJ/schemas/nonascii.maml"
rm -f "$PROJ/src/parquet_maml.f90"
if ( cd "$PROJ" && "$GEN" >"$WORK/gen7.log" 2>&1 ); then
    report FAIL "a non-ASCII schema is refused"
elif [ -e "$PROJ/src/parquet_maml.f90" ]; then
    report FAIL "a refused non-ASCII run leaves no file behind (found one of \
$(wc -c < "$PROJ/src/parquet_maml.f90") bytes)"
elif grep -q 'nonascii.maml:6' "$WORK/gen7.log" && grep -q 'non-ASCII' "$WORK/gen7.log"; then
    report ok "a non-ASCII schema is refused, naming file and line, writing nothing"
else
    report FAIL "the non-ASCII refusal names the file and line"
    cat "$WORK/gen7.log" >&2
fi
rm -f "$PROJ/schemas/nonascii.maml"

# 8. THE PAGE AGREES WITH ITSELF about how many fields its example has. Its prose says the program
#    prints `parsed=T fields=3`, and the ```yaml block above declares those fields. The LIBRARY
#    half of that line is covered in process -- test/test_maml.f90 asserts that %is_parsed is
#    .true. after a MAML parse and that %get_num_fields counts what a schema holds, and
#    test/test_reading.f90's test_get_parquet_maml_examples drives the shared select-case template
#    through the base module. What nothing covered is the page agreeing with itself, which is the
#    half that drifts: add a field to the example schema and the printed line goes stale silently.
#    This script already extracts both, so the check is one comparison and no compile. It is why
#    "IT COMPILES, IT DOES NOT RUN" above costs nothing here.
if python3 - "$ROOT_DIR/$PAGE" >"$WORK/fields.log" 2>&1 <<'PY'
import pathlib, re, sys

page = pathlib.Path(sys.argv[1]).read_text()
block = re.search(r'```yaml\n(.*?)```', page, re.S)
if block is None:
    raise SystemExit('no ```yaml block on the page')
declared = len(re.findall(r'^- name:', block.group(1), re.M))
said = re.search(r'parsed=T fields=(\d+)', page)
if said is None:
    raise SystemExit('the page states no `parsed=T fields=N` result for its example')
if int(said.group(1)) != declared:
    raise SystemExit('the page says the example prints fields=%s; its own schema declares %d'
                     % (said.group(1), declared))
print('fields=%d, as the page says' % declared)
PY
then
    report ok "the example's printed field count matches its own schema ($(cat "$WORK/fields.log"))"
else
    report FAIL "the example's printed field count matches its own schema"
    cat "$WORK/fields.log" >&2
fi

# ---- Verdict -----------------------------------------------------------------------------------
finished=1
rm -rf "$WORK"
if [ "$fail" -ne 0 ]; then
    echo "check_downstream_maml_module.sh: FAILED" >&2
    exit 1
fi
echo "check_downstream_maml_module.sh: all checks passed"
exit 0
