#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

python3 - <<'PY'
from pathlib import Path
import re

repo = Path('.').resolve()
docs_dir = repo / 'docs'
out_path = repo / 'src' / 'parquet_maml.f90'

maml_files = sorted(docs_dir.rglob('*.maml'))
if not maml_files:
    raise SystemExit('No .maml files found under docs/')


def make_identifier(name: str) -> str:
    stem = Path(name).stem.lower()
    ident = re.sub(r'[^a-z0-9]+', '_', stem).strip('_')
    if not ident:
        ident = 'maml'
    if ident[0].isdigit():
        ident = 'maml_' + ident
    return 'parquet_maml_' + ident


def fstr(s: str) -> str:
    return '"' + s.replace('"', '""') + '"'

lines = []
lines.append('!=========================')
lines.append('! Author: Elmo Tempel (elmo.tempel@ut.ee)')
lines.append('!=========================')
lines.append('!')
lines.append('module parquet_maml')
lines.append('    implicit none')
lines.append('    private')
lines.append('')
lines.append('    type, public :: parquet_maml_file')
lines.append('        character(len=:), allocatable :: name')
lines.append('        character(len=:), allocatable :: lines(:)')
lines.append('    end type parquet_maml_file')
lines.append('')
public_names = ['get_parquet_maml']
function_blocks = []
case_blocks = []

for path in maml_files:
    identifier = make_identifier(path.name)
    public_names.append(identifier)
    file_lines = path.read_text(encoding='utf-8').splitlines()
    max_len = max((len(x) for x in file_lines), default=1)
    rel_name = path.relative_to(docs_dir).as_posix()

    block = []
    block.append(f'    function {identifier}() result(maml)')
    block.append('        type(parquet_maml_file) :: maml')
    block.append('')
    block.append(f'        maml%name = {fstr(rel_name)}')
    if file_lines:
        block.append(f'        allocate(character(len={max_len}) :: maml%lines({len(file_lines)}))')
        block.append(f'        maml%lines = [ character(len={max_len}) :: &')
        for idx, text in enumerate(file_lines):
            suffix = ', &' if idx < len(file_lines) - 1 else ' ]'
            block.append(f'            {fstr(text)}{suffix}')
    else:
        block.append('        allocate(character(len=1) :: maml%lines(0))')
    block.append(f'    end function {identifier}')
    function_blocks.append('\n'.join(block))

    case_blocks.append(f'        case ({fstr(rel_name)})')
    case_blocks.append(f'            maml = {identifier}()')
    case_blocks.append(f'        case ({fstr(Path(rel_name).stem)})')
    case_blocks.append(f'            maml = {identifier}()')

for public_name in public_names:
    lines.append(f'    public :: {public_name}')
lines.append('')
lines.append('contains')
lines.append('')
lines.append('    function get_parquet_maml(name) result(maml)')
lines.append('        character(len=*), intent(in) :: name')
lines.append('        type(parquet_maml_file) :: maml')
lines.append('')
lines.append('        select case (trim(name))')
lines.extend(case_blocks)
lines.append('        case default')
lines.append('            error stop "get_parquet_maml: unknown internal MAML file: " // trim(name)')
lines.append('        end select')
lines.append('    end function get_parquet_maml')
lines.append('')
lines.extend(function_blocks)
lines.append('')
lines.append('end module parquet_maml')
lines.append('')

out_path.write_text('\n'.join(lines), encoding='ascii')
print(f'Generated {out_path} from {len(maml_files)} MAML files')
PY
