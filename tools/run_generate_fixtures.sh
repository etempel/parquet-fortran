#!/usr/bin/env bash
# Compiles and runs tools/generate_fixtures.cpp, which (re)writes every
# hand-built Arrow/Parquet fixture under test/fixtures/ that this library's
# own writer cannot produce itself (genuine Parquet Nulls, an unsupported
# physical column type, ...), plus any exploratory fixture under test_run/
# (gitignored, not consumed by any Fortran test). Run this whenever
# generate_fixtures.cpp changes, or a fixture file needs regenerating.
set -euo pipefail

cd "$(dirname "$0")/.."

: "${FPM_CXXFLAGS:?FPM_CXXFLAGS must be set (Arrow/Parquet -I flags, -std=c++20, see README's Environment variables section) -- same as building this project itself.}"
: "${FPM_LDFLAGS:?FPM_LDFLAGS must be set (Arrow/Parquet -L flags, see README's Environment variables section) -- same as building this project itself.}"

mkdir -p test/fixtures test_run

out="$(mktemp -t generate_fixtures_XXXXXX)"
trap 'rm -f "$out"' EXIT

echo "Building tools/generate_fixtures.cpp..."
# shellcheck disable=SC2086
clang++ ${FPM_CXXFLAGS} tools/generate_fixtures.cpp -o "$out" ${FPM_LDFLAGS} -lparquet -larrow

echo "Running..."
"$out"
