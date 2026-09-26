#!/usr/bin/env bash
#
# Embed this project's .maml schemas into compiled Fortran source, so nothing has to locate or
# ship a .maml file at run time.
#
# Every .maml file under the scanned directory becomes one accessor function returning a
# parquet_schema that is ALREADY PARSED, plus an entry in get_parquet_maml's lookup by name. The
# generated module is written to src/<module>.f90 and is meant to be COMMITTED: it is regenerated
# on demand rather than at build time, which keeps the build itself free of any dependency on
# Python or on this script. Re-run it whenever a .maml changes, and commit the result alongside.
#
# Copy this script into your own project (e.g. under tools/) and run it from your project's root.
# It needs python3 and nothing else.
#
# Usage:  tools/generate_parquet_maml.sh [--dir=NAME] [--module=NAME] [--check]
#
#   --dir=NAME     directory to scan for .maml files, relative to the project root
#                  (default: schemas). Scanned RECURSIVELY: subdirectories organise your schemas
#                  without changing how any of them is addressed, which is why every .maml
#                  filename must be unique across the whole tree.
#   --module=NAME  name of the generated module, and therefore of the file it is written to,
#                  src/NAME.f90 (default: parquet_maml). Reach for it when your project requires
#                  every module to carry its own package prefix, or when you want more than one
#                  embedded-schema module -- a second run with the default would overwrite the
#                  first, since both the module name and the output path come from it.
#   --check        regenerate in memory and compare against the committed file, writing nothing;
#                  exit 1 on drift, naming the file. A one-line addition to your own CI. A --check
#                  run must repeat whatever --dir/--module the generating run used; with
#                  different options it is comparing against a different file.
#
# Both long-option spellings work: --dir=NAME and --dir NAME.
#
# The generated module imports parquet_schema, parquet_load_maml_file, parquet_parse_maml and
# parquet_validate_user_maml from parquet_io -- the I/O entry module -- so a project embedding
# schemas does not pull in the whole library surface. Alongside get_parquet_maml it makes
# set_maml(maml_default, [maml_file]) public: the embedded default when maml_file is absent or
# blank, otherwise that file loaded, validated against the default, and parsed.
#
# It REFUSES, with a message and a nonzero exit, before writing anything: when the scanned
# directory holds no .maml file, when two schemas share a filename (anywhere in the tree, or
# differing only in case or punctuation), when a schema holds a non-ASCII character (the emitted
# literals are ASCII), when --module is not a valid Fortran module name, and when an argument is
# not one of the three above (exit 2, with the usage line).
set -euo pipefail

work_dir="$(pwd)"
maml_dir="schemas"
check=""
module_override=""

while [ $# -gt 0 ]; do
    case "$1" in
        --dir=*)
            maml_dir="${1#--dir=}"
            shift
            ;;
        --dir)
            maml_dir="${2:?--dir requires a value}"
            shift 2
            ;;
        --module=*)
            module_override="${1#--module=}"
            shift
            ;;
        --module)
            module_override="${2:?--module requires a value}"
            shift 2
            ;;
        --check)
            check="1"
            shift
            ;;
        *)
            echo "generate_parquet_maml.sh: unknown argument '$1'" >&2
            echo "Usage: generate_parquet_maml.sh [--dir=NAME] [--module=NAME] [--check]" >&2
            exit 2
            ;;
    esac
done

python3 - "$work_dir" "$maml_dir" "$check" "$module_override" <<'PY'
from pathlib import Path
import re
import sys

work_dir = Path(sys.argv[1]).resolve()
maml_dir_name = sys.argv[2]
check = bool(sys.argv[3])
module_override = sys.argv[4]

if module_override:
    # A Fortran module name, and one short enough to survive as a filename and as
    # the `end module <name>` line: letters/digits/underscore, leading letter, and
    # the standard's own 63-character identifier limit.
    if not re.fullmatch(r'[A-Za-z][A-Za-z0-9_]*', module_override):
        raise SystemExit(
            f"--module: '{module_override}' is not a valid Fortran module name (letters, digits "
            'and underscores only, starting with a letter).')
    if len(module_override) > 63:
        raise SystemExit(
            f"--module: '{module_override}' is {len(module_override)} characters; a Fortran "
            'identifier may be at most 63.')
    module_name = module_override
else:
    module_name = 'parquet_maml'

out_path = work_dir / 'src' / f'{module_name}.f90'
maml_source_dir = work_dir / maml_dir_name
maml_files = sorted(maml_source_dir.rglob('*.maml'))
if not maml_files:
    raise SystemExit(f'No .maml files found under {maml_dir_name}/')


# ---- shared emitter (begin) -------------------------------------------------------------------
# Everything between this marker and its (end) twin turns a set of .maml files into the Fortran
# that embeds them: the per-file accessor functions, the select-case lookup they share, and the
# line-chunking that keeps both inside Fortran's source limits. It is deliberately free of any
# decision about WHICH module is being written -- every such difference arrives through `spec`.


def make_identifier(name: str) -> str:
    """The name of the public accessor emitted for one .maml file."""
    stem = Path(name).stem.lower()
    ident = re.sub(r'[^a-z0-9]+', '_', stem).strip('_')
    if not ident:
        ident = 'maml'
    if ident[0].isdigit():
        ident = 'maml_' + ident
    return 'parquet_maml_' + ident


def fstr(s: str) -> str:
    """One Fortran character literal holding `s`."""
    return '"' + s.replace('"', '""') + '"'


# The names get_parquet_maml matches for one embedded file. A file directly in
# the scanned directory answers to its filename and to that filename without the
# extension; a file in a subdirectory answers to those two as well, plus its full
# relative path. The scan is recursive (rglob below), so subdirectories are an
# organisational choice with no effect on how a schema is addressed -- which
# holds only because two .maml files may not share a filename (checked below).
def case_labels(rel_name: str) -> list:
    labels = [Path(rel_name).name, Path(rel_name).stem]
    if rel_name != labels[0]:
        labels.insert(0, rel_name)
    return labels


# REFUSE BEFORE WRITING ANYTHING, and name both files. Two .maml files whose
# names collide -- the same filename in two subdirectories, or names differing
# only in case or punctuation, since make_identifier lowercases and collapses
# every run of non-alphanumerics -- would otherwise emit a duplicate `public ::`,
# a duplicate function and a duplicate `case` label. The generator would report
# success and the failure would surface at compile time as "ACCESS specification
# at (1) was already specified", naming neither this script nor either schema.
def check_for_collisions(paths: list, source_dir, dir_name: str) -> None:
    seen = {}
    for path in paths:
        rel_name = path.relative_to(source_dir).as_posix()
        claims = [('accessor name', make_identifier(path.name))]
        claims += [('name', label) for label in case_labels(rel_name)]
        for what, key in claims:
            previous = seen.get((what, key))
            if previous is not None:
                raise SystemExit(
                    f"Two MAML files under {dir_name}/ share the {what} '{key}':\n"
                    f'    {previous}\n'
                    f'    {rel_name}\n'
                    'Every .maml filename must be unique across that directory, including its '
                    'subdirectories, and two filenames differing only in case or punctuation '
                    'count as the same.')
            seen[(what, key)] = rel_name


# Wraps a leading '!>' doc-comment to Fortran's 132-column free-form limit, splitting on word
# boundaries -- needed here (unlike the mostly-static doc blocks elsewhere, hand-wrapped and
# verified once) because this helper's callers interpolate runtime data (a .maml filename) of
# arbitrary length into the comment text.
def emit_doc(indent: str, *sentences: str) -> list:
    prefix = f'{indent}!> '
    text = ' '.join(sentences)
    avail = 132 - len(prefix)
    out = []
    cur = ''
    for word in text.split():
        cand = (cur + ' ' + word) if cur else word
        if len(cand) > avail and cur:
            out.append(prefix + cur)
            cur = word
        else:
            cur = cand
    if cur:
        out.append(prefix + cur)
    return out


# Free-form Fortran source lines are limited to 132 characters and a single
# statement may span at most 255 continuation lines (F2008+). Emitting one
# assignment statement per MAML line (rather than one big array constructor
# chained with "&") keeps the number of continuation lines per statement
# small and independent of the total MAML file length. Individual MAML
# lines are chunked here too, so even a very long single line stays safely
# under both limits.
CHUNK_LEN = 100


def emit_assignment(target: str, text: str) -> list:
    quoted = fstr(text)
    if len(quoted) <= CHUNK_LEN + 2:
        return [f'        {target} = {quoted}']
    chunks = [text[i:i + CHUNK_LEN] for i in range(0, len(text), CHUNK_LEN)] or ['']
    stmt = [f'        {target} = &']
    for idx, chunk in enumerate(chunks):
        suffix = ' // &' if idx < len(chunks) - 1 else ''
        stmt.append(f'            {fstr(chunk)}{suffix}')
    return stmt


# One accessor function per .maml file, plus the select-case arms naming it. `spec` carries
# everything that depends on which module is being written:
#
#   var          the function result variable's name
#   type         its Fortran type
#   name_field   component of the result that holds the fixture's own name
#   lines_field  component that holds the raw MAML lines
#   doc          tail of the accessor's !> sentence
#   result_doc   tail of the !! text on the per-file result declaration
#   lookup_doc   the same for get_parquet_maml's result declaration
#   trailer      statements emitted after the lines, before `end function`
def emit_accessors(maml_files: list, source_dir, spec: dict) -> tuple:
    public_names = []
    function_blocks = []
    case_blocks = []
    name_target = spec['var'] + spec['name_field']
    lines_target = spec['var'] + spec['lines_field']
    for path in maml_files:
        identifier = make_identifier(path.name)
        public_names.append(identifier)
        file_lines = path.read_text(encoding='utf-8').splitlines()
        max_len = max((len(x) for x in file_lines), default=1)
        rel_name = path.relative_to(source_dir).as_posix()

        block = emit_doc('    ', f'Returns the embedded {rel_name} MAML fixture, {spec["doc"]}')
        block.append(f'    function {identifier}() result({spec["var"]})')
        block.append(f'        type({spec["type"]}) :: {spec["var"]} !! the {rel_name} MAML, {spec["result_doc"]}')
        block.append('')
        block.append(f'        {name_target} = {fstr(rel_name)}')
        if file_lines:
            block.append(f'        allocate(character(len={max_len}) :: {lines_target}({len(file_lines)}))')
            for idx, text in enumerate(file_lines):
                block.extend(emit_assignment(f'{lines_target}({idx + 1})', text))
        else:
            block.append(f'        allocate(character(len=1) :: {lines_target}(0))')
        block.extend(spec['trailer'])
        block.append(f'    end function {identifier}')
        function_blocks.append('\n'.join(block))

        for label in case_labels(rel_name):
            case_blocks.append(f'        case ({fstr(label)})')
            case_blocks.append(f'            {spec["var"]} = {identifier}()')
    return public_names, function_blocks, case_blocks


# get_parquet_maml itself: the select case over every name the accessors above answer to. The
# caller emits its !> doc block, which is the one part that differs. No validation call belongs
# here in either form -- each accessor has already done whatever parsing its module promises.
def emit_lookup(case_blocks: list, spec: dict) -> list:
    out = [f'    function get_parquet_maml(name) result({spec["var"]})']
    out.append('        character(len=*), intent(in) :: name !! embedded fixture name, with or without .maml.')
    out.append(f'        type({spec["type"]}) :: {spec["var"]} !! the matching MAML, {spec["lookup_doc"]}')
    out.append('')
    out.append('        select case (trim(name))')
    out.extend(case_blocks)
    out.append('        case default')
    out.append('            error stop "get_parquet_maml: unknown internal MAML file: " // trim(name)')
    out.append('        end select')
    out.append('    end function get_parquet_maml')
    return out


# REFUSE A NON-ASCII SCHEMA, before anything is written, and name the file, the line and the
# character. The emitted module is written as ASCII -- Fortran's own character set, and what the
# length arithmetic above assumes, since `max_len` counts CHARACTERS while a Fortran literal's
# length is BYTES. Without this, a byte above 127 surfaces as a UnicodeEncodeError traceback out
# of write_text, and pathlib has by then already truncated src/<module>.f90 to zero bytes: the
# project is left an empty module and a Python stack trace in place of one of the clean refusals
# this script otherwise gives, and nothing says which schema was at fault.
def check_for_non_ascii(paths: list, source_dir) -> None:
    for path in paths:
        rel_name = path.relative_to(source_dir).as_posix()
        for lineno, line in enumerate(path.read_text(encoding='utf-8').splitlines(), start=1):
            for ch in line:
                if ord(ch) > 127:
                    raise SystemExit(
                        f"{rel_name}:{lineno}: non-ASCII character '{ch}' (U+{ord(ch):04X}). The "
                        'generated module embeds every MAML line as a Fortran character literal, '
                        'which is ASCII, so a schema compiled in this way may use ASCII only. '
                        "Replace the character -- 'deg' for a degree sign, 'um' for a micron.")
# ---- shared emitter (end) ---------------------------------------------------------------------

check_for_non_ascii(maml_files, maml_source_dir)
check_for_collisions(maml_files, maml_source_dir, maml_dir_name)

# Each accessor returns a fully parsed parquet_schema: parquet_parse_maml populates %cinfo and
# %metadata from %maml right here, so a caller gets a ready-to-use schema with no separate parse
# step. That is the whole of `trailer`.
SPEC = {
    'var': 'schema',
    'type': 'parquet_schema',
    'name_field': '%maml%name',
    'lines_field': '%maml%lines',
    'doc': 'already parsed into a schema.',
    'result_doc': 'parsed into a schema.',
    'lookup_doc': 'already parsed into a schema.',
    'trailer': ['        call parquet_parse_maml(schema)'],
}

accessor_names, function_blocks, case_blocks = emit_accessors(maml_files, maml_source_dir, SPEC)

lines = []
lines.append('!===========================================')
lines.append('! This file is automatically generated! Do not edit it directly!')
lines.append(f'! Instead, edit the MAML files under {maml_dir_name}/ and run generate_parquet_maml.sh to regenerate this file.')
lines.append('! Generator script: generate_parquet_maml.sh')
lines.append('!===========================================')
lines.append(f'!> Embedded MAML schemas: every .maml file under {maml_dir_name}/ compiled in as a')
lines.append('!> string array, addressable by name via get_parquet_maml/set_maml and already')
lines.append('!> parsed into a ready-to-use parquet_schema.')
lines.append(f'module {module_name}')
lines.append('    use parquet_io, only: parquet_schema, parquet_load_maml_file')
lines.append('    use parquet_io, only: parquet_parse_maml, parquet_validate_user_maml')
lines.append('    implicit none')
lines.append('    private')
lines.append('')
for public_name in ['get_parquet_maml', 'set_maml'] + accessor_names:
    lines.append(f'    public :: {public_name}')
lines.append('')
lines.append('contains')
lines.append('')
lines.append('    !> Returns the named embedded MAML fixture, already parsed into a ready-to-use')
lines.append('    !> parquet_schema; error stops on an unknown name. A fixture is matched by its')
lines.append('    !> filename, with or without the .maml extension; one held in a subdirectory is')
lines.append('    !> also matched by its full relative path.')
lines.extend(emit_lookup(case_blocks, SPEC))
lines.append('')
lines.append('    !> Resolves to a parquet_schema for either a user-supplied MAML file (parsed')
lines.append('    !> and validated against the maml_default embedded schema) or, if maml_file')
lines.append('    !> is absent/blank, straight to the embedded maml_default fixture itself.')
lines.append('    function set_maml(maml_default, maml_file) result(schema)')
lines.append("        character(len=*),intent(in) :: maml_default       !! default MAML name (input to get_parquet_maml)")
lines.append("        character(len=*),intent(in),optional :: maml_file !! MAML file name (if provided, overrides default)")
lines.append('        type(parquet_schema) :: schema !! resulting parsed schema.')
lines.append('        type(parquet_schema) :: schema_base')
lines.append('        logical :: has_maml_file')
lines.append('        !')
lines.append('        has_maml_file = present(maml_file)')
lines.append('        if (has_maml_file) then')
lines.append("            has_maml_file = trim(maml_file) /= ''")
lines.append('        end if')
lines.append('        !')
lines.append('        if (has_maml_file) then')
lines.append('            ! read MAML from file')
lines.append('            schema%maml = parquet_load_maml_file(trim(maml_file))')
lines.append('            ! validate user defined MAML file against the default (base) schema')
lines.append('            schema_base = get_parquet_maml(trim(maml_default))')
lines.append('            call parquet_validate_user_maml(schema_base%maml, schema%maml)')
lines.append('            call parquet_parse_maml(schema)')
lines.append('        else')
lines.append('            ! no MAML provided, use default MAML')
lines.append('            schema = get_parquet_maml(trim(maml_default))')
lines.append('        end if')
lines.append('        !')
lines.append('    end function set_maml')
lines.append('')
lines.extend(function_blocks)
lines.append('')
lines.append(f'end module {module_name} ! GCOVR_EXCL_LINE')
lines.append('')

text = '\n'.join(lines)

if check:
    current = out_path.read_text(encoding='ascii') if out_path.exists() else ''
    if current != text:
        print(f'generate_parquet_maml.sh: {out_path.name} is out of date', file=sys.stderr)
        print('Re-run tools/generate_parquet_maml.sh and commit the result.', file=sys.stderr)
        sys.exit(1)
    print(f'generate_parquet_maml.sh: {out_path.name} is up to date (module {module_name})')
else:
    # Create src/ rather than dying in write_text with a FileNotFoundError traceback:
    # a project that has not added Fortran sources yet has no src/, and every other
    # failure in this script is a clean one-line message.
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(text, encoding='ascii')
    print(f'Generated {out_path} from {len(maml_files)} MAML files (module {module_name})')
PY
