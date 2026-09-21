#!/usr/bin/env bash
#
# Regenerate src/parquet_maml_base.f90: every .maml file under schemas/ compiled into this library
# as a string array addressable by filename, plus the parquet_maml_file type family the rest of the
# library shares.
#
# THIS IS THE LIBRARY'S OWN generator and never leaves this repository. Its sibling,
# tools/generate_parquet_maml.sh, is the CONSUMER-FACING one a project depending on this library
# copies into its own tools/ to embed its own schemas -- see
# doc/pages/utilities/embedding-maml-schemas.md. The two emit different modules: that one imports
# four names from parquet_io and hands back a parsed parquet_schema, while this one emits no `use`
# statement at all (parquet_maml_base sits below parquet_core in the module stack, so it cannot
# name parquet_schema without a cycle) and hands back the raw parquet_maml_file its own callers
# parse themselves.
#
# What the two DO share is the region marked `shared emitter` below, which must stay BYTE FOR BYTE
# identical in both files: check_maml_generators_share_their_emitter
# (tools/check_source_conventions.py) fails the lint stage on any drift, so a fix made to one
# lands in the other. Everything outside that region is free to differ, and does.
#
# Usage:  tools/generate_parquet_maml_base.sh [--check]
#
#   --check   regenerate in memory and compare against the committed src/parquet_maml_base.f90,
#             writing nothing; exit 1 on drift. This is what the lint stage runs.
#
# Runs from anywhere: the repository root is resolved from this script's own path, not from $PWD.
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
check=""

while [ $# -gt 0 ]; do
    case "$1" in
        --check)
            check="1"
            shift
            ;;
        *)
            echo "generate_parquet_maml_base.sh: unknown argument '$1' (only --check is accepted)" >&2
            echo "For a downstream project's own schemas, use tools/generate_parquet_maml.sh." >&2
            exit 2
            ;;
    esac
done

python3 - "$root_dir" "$check" <<'PY'
from pathlib import Path
import re
import sys

work_dir = Path(sys.argv[1]).resolve()
check = bool(sys.argv[2])

# Both are fixed: the rest of the library refers to parquet_maml_base by name, and schemas/ is
# where this repository keeps the MAMLs describing the files it writes. The consumer-facing
# generator is the one carrying --dir/--module, because a downstream project may need either.
MAML_DIR = 'schemas'
MODULE_NAME = 'parquet_maml_base'

out_path = work_dir / 'src' / f'{MODULE_NAME}.f90'
maml_source_dir = work_dir / MAML_DIR
maml_files = sorted(maml_source_dir.rglob('*.maml'))
if not maml_files:
    raise SystemExit(f'No .maml files found under {MAML_DIR}/')


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
# ---- shared emitter (end) ---------------------------------------------------------------------

lines = []
lines.append('!===========================================')
lines.append('! This file is automatically generated! Do not edit it directly!')
lines.append(f'! Instead, edit the MAML files under {MAML_DIR}/ and run generate_parquet_maml_base.sh to regenerate this file.')
lines.append('! Generator script: parquet-fortran/tools/generate_parquet_maml_base.sh')
lines.append('!===========================================')
lines.append(f'!> Base-library MAML fixtures: embeds every .maml file under {MAML_DIR}/ bundled')
lines.append('!> with this library as a compiled-in string array, addressable by filename via')
lines.append('!> get_parquet_maml, plus the shared parquet_maml_file type and its')
lines.append('!> add_col_qc/set_col_qc qc-maml builders.')
lines.append(f'module {MODULE_NAME}')
lines.append('    implicit none')
lines.append('    private')
lines.append('')
lines.append('    !> One schema field the base MAML declares but a user-supplied MAML omits;')
lines.append('    !> populated by parquet_validate_user_maml, one entry per missing field.')
lines.append('    type, public :: parquet_maml_missing_column')
lines.append('        character(len=:), allocatable :: name      !! The name of the field [required].')
lines.append('        character(len=:), allocatable :: unit      !! The unit of measurement for the field.')
lines.append('        character(len=:), allocatable :: info      !! A short description of the field.')
lines.append('        character(len=:), allocatable :: ucd       !! Unified Content Descriptor for IVOA (can have many).')
lines.append('        character(len=:), allocatable :: data_type !! The data type of the field [required]; base token')
lines.append('        !! only for a temporal column (unit/utc are in time_unit/is_utc).')
lines.append('        integer :: time_unit = 0 !! time/timestamp stored unit (a parquet_unit_* selector; 0 if not temporal).')
lines.append('        logical :: is_utc = .false. !! timestamp UTC-adjusted flag.')
lines.append('        integer :: array_size = 1 !! Maximum length of character strings.')
lines.append('        integer :: col_size = 1   !! The number of elements in the vector column.')
lines.append('    end type parquet_maml_missing_column')
lines.append('')
lines.append('    !> One col_map: entry: `- <internal_name>: <output_name>`, i.e. the field')
lines.append('    !> declared as `output_name` in this MAML fields: actually corresponds to')
lines.append('    !> the internal/canonical column named `internal_name`.')
lines.append('    type, public :: parquet_maml_col_map_entry')
lines.append('        character(len=:), allocatable :: internal_name !! Canonical (internal) column name.')
lines.append('        character(len=:), allocatable :: output_name   !! Renamed name as declared in fields:.')
lines.append('    end type parquet_maml_col_map_entry')
lines.append('')
lines.append('    !> One embedded or user-supplied MAML file: its raw source lines plus, once')
lines.append('    !> parsed/validated, the columns missing relative to the base schema and any')
lines.append('    !> col_map: renames it declares. add_col_qc/set_col_qc build a qc-maml')
lines.append('    !> incrementally (see parquet_maml_base_add_col_qc.f90).')
lines.append('    type, public :: parquet_maml_file')
lines.append('        logical :: user_maml = .false. !! true if this is a user defined MAML file')
lines.append('        character(len=:), allocatable :: name !! This MAML file name (embedded fixture name or path).')
lines.append('        character(len=:), allocatable :: lines(:) !! Raw MAML source, one array element per line.')
lines.append('        !> Columns present in the base MAML but missing from this (user) MAML;')
lines.append('        !> populated by parquet_validate_user_maml, consumed by parquet_read_maml.')
lines.append('        type(parquet_maml_missing_column), allocatable :: missing_columns(:)')
lines.append('        !> Parsed col_map: section (see parquet_maml_col_map_entry): exposes the')
lines.append('        !> renames this MAML declares, for inspection; populated by')
lines.append('        !> parquet_validate_user_maml. Renames are applied automatically whenever')
lines.append('        !> this MAML lines are parsed, independent of whether this is set.')
lines.append('        type(parquet_maml_col_map_entry), allocatable :: col_map(:)')
lines.append('    contains')
lines.append("        procedure :: add_col_qc => parquet_maml_add_col_qc !! Appends one qc: field entry.")
lines.append("        procedure :: set_col_qc => parquet_maml_set_col_qc !! In-place form; parses the name into its argument.")
lines.append('    end type parquet_maml_file')
lines.append('')
# add_col_qc/set_col_qc are two forms of the same type-bound builder for
# read-time qc-mamls: both append one qc: field entry from a compact
# "col, min, max, miss" string. add_col_qc is a subroutine returning the
# parsed name via an optional out-argument; set_col_qc is also a
# subroutine, but takes its single col_name argument as intent(inout): it
# holds the compact string on entry and the parsed name on exit, so a
# caller reuses one variable (call maml%set_col_qc(col)) instead of
# assigning a function result back into it -- a subroutine can't alias the
# same actual argument to separate intent(in)/intent(out) dummies, and a
# function returning character(len=:), allocatable is never used in this
# codebase (see "Build and compiler notes" in CLAUDE.md for why). Both
# bodies are hand-written (string parsing + validation) in the submodule
# src/parquet_maml_base_add_col_qc.f90 rather than embedded here, so they
# are deferred via module-procedure interfaces: parquet_maml_base is at the
# bottom of the module stack, so the body must stay self-contained (it
# cannot use parquet_metadata's validation helpers without a cycle).
lines.append('    interface')
lines.append('        !> Appends one qc: field entry to this MAML from a compact')
lines.append('        !> "col, min, max, miss" string. col_name (optional) returns the')
lines.append('        !> parsed column name. Implemented in the submodule')
lines.append('        !> src/parquet_maml_base_add_col_qc.f90.')
lines.append('        module subroutine parquet_maml_add_col_qc(self, qc_input, col_name)')
lines.append('            class(parquet_maml_file), intent(inout) :: self !! qc-maml being built; gains a fields: entry.')
lines.append('            character(len=*), intent(in) :: qc_input !! compact "col, min, max, miss" string.')
lines.append('            character(len=:), allocatable, intent(out), optional :: col_name !! parsed column name.')
lines.append('        end subroutine parquet_maml_add_col_qc')
lines.append('        !> In-place form of add_col_qc: appends the same qc: field entry. `col_name` is')
lines.append('        !> `intent(inout)`: it holds the compact "col, min, max, miss" string on entry and')
lines.append('        !> the parsed column name on exit, so a caller reuses one variable')
lines.append('        !> (call maml%set_col_qc(col)) instead of naming a separate input and output.')
lines.append('        module subroutine parquet_maml_set_col_qc(self, col_name)')
lines.append('            class(parquet_maml_file), intent(inout) :: self !! qc-maml being built; gains a fields: entry.')
lines.append('            character(len=:), allocatable, intent(inout) :: col_name !! compact "col, min, max, miss" string on')
lines.append('            !! entry; parsed column name on exit.')
lines.append('        end subroutine parquet_maml_set_col_qc')
lines.append('    end interface')
lines.append('')

check_for_collisions(maml_files, maml_source_dir, MAML_DIR)

# Base mode hands back the RAW parquet_maml_file: this module is the library's own internal
# fixture accessor and sits below parquet_core.f90/parquet_schema in the module stack, so it
# cannot return a parquet_schema without a circular dependency. Its callers parse the result
# themselves via parquet_parse_maml, which is why `trailer` is empty here.
SPEC = {
    'var': 'maml',
    'type': 'parquet_maml_file',
    'name_field': '%name',
    'lines_field': '%lines',
    'doc': 'unparsed (raw lines only).',
    'result_doc': 'unparsed.',
    'lookup_doc': 'unparsed (raw lines only).',
    'trailer': [],
}

accessor_names, function_blocks, case_blocks = emit_accessors(maml_files, maml_source_dir, SPEC)

for public_name in ['get_parquet_maml'] + accessor_names:
    lines.append(f'    public :: {public_name}')
lines.append('')
lines.append('contains')
lines.append('')
lines.append(f'    !> Returns the named embedded .maml fixture from {MAML_DIR}/ as a raw parquet_maml_file')
lines.append('    !> (unparsed lines only); error stops on an unknown name. A fixture is matched')
lines.append('    !> by its filename, with or without the .maml extension; one held in a')
lines.append('    !> subdirectory is also matched by its full relative path.')
lines.extend(emit_lookup(case_blocks, SPEC))
lines.append('')
lines.extend(function_blocks)
lines.append('')
lines.append(f'end module {MODULE_NAME} ! GCOVR_EXCL_LINE')
lines.append('')

text = '\n'.join(lines)

if check:
    current = out_path.read_text(encoding='ascii') if out_path.exists() else ''
    if current != text:
        print(f'generate_parquet_maml_base.sh: {out_path.name} is out of date', file=sys.stderr)
        print('Re-run tools/generate_parquet_maml_base.sh and commit the result.', file=sys.stderr)
        sys.exit(1)
    print(f'generate_parquet_maml_base.sh: {out_path.name} is up to date (module {MODULE_NAME})')
else:
    out_path.write_text(text, encoding='ascii')
    print(f'Generated {out_path} from {len(maml_files)} MAML files (module {MODULE_NAME})')
PY
