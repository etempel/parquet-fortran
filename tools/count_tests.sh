#!/usr/bin/env bash
# Counts test-drive unit tests per testsuite at the source level, with no
# build or run required. test/run_tester.f90 registers each testsuite as
# new_testsuite("<name>", <collect_tests_parquet_*>) -- this script reads
# that list, locates each collect_tests_parquet_* subroutine among
# test/test_*.f90, and counts the new_unittest(...) entries inside its body
# (one per actual test-drive test). This mirrors -- and was cross-checked
# against -- the ground-truth count from an actual `fpm test run_tester`
# run's PASSED/FAILED line count.
#
# Usage:
#   tools/count_tests.sh
# ---------------------------------------------------------------------------------------------
# Counts test-drive unit tests per suite directly from source (no build or
# run required): it reads `test/run_tester.f90`'s `new_testsuite(...)` registrations, locates each
# suite's `collect_tests_parquet_*` subroutine, and counts the `new_unittest(...)` entries inside it
# -- cross-checked against an actual `fpm test run_tester` run's PASSED/FAILED line count.
# Maintainer-only (stripped from the fpm-published package, see `tools/prep_fpm_publish.sh`).
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

RUN_TESTER="test/run_tester.f90"

if [ ! -f "$RUN_TESTER" ]; then
    echo "error: $RUN_TESTER not found" >&2
    exit 1
fi

# Counts new_unittest( occurrences strictly within fn's own subroutine body
# in file (from its "subroutine fn(" header to its matching "end subroutine
# fn" line), so two collect_tests_parquet_* subroutines sharing one file
# (test_openmp.f90's _write/_read split) are never double-counted.
count_suite_tests() {
    local fn="$1" file="$2"
    awk -v fn="$fn" '
        BEGIN { in_sub = 0; count = 0 }
        in_sub == 0 && $0 ~ ("^[[:space:]]*subroutine[[:space:]]+" fn "[[:space:]]*\\(") { in_sub = 1; next }
        in_sub == 1 && $0 ~ ("^[[:space:]]*end[[:space:]]+subroutine[[:space:]]+" fn "([[:space:]]|$)") { exit }
        in_sub == 1 { count += gsub(/new_unittest\(/, "&") }
        END { print count }
    ' "$file"
}

printf '%-16s %8s   %s\n' "SUITE" "TESTS" "SOURCE"
printf '%-16s %8s   %s\n' "--------------" "-----" "------"

total=0
while IFS= read -r pair; do
    suite_name="$(sed -E 's/^new_testsuite\("([^"]+)",.*/\1/' <<< "$pair")"
    fn="$(sed -E 's/^new_testsuite\("[^"]+",[[:space:]]*([A-Za-z0-9_]+).*/\1/' <<< "$pair")"

    file="$(grep -lE "^[[:space:]]*subroutine[[:space:]]+${fn}[[:space:]]*\(" test/test_*.f90 | head -1)"
    if [ -z "$file" ]; then
        echo "error: no 'subroutine ${fn}(...)' found under test/test_*.f90 (referenced by $RUN_TESTER" \
            "for testsuite \"$suite_name\")" >&2
        exit 1
    fi

    count="$(count_suite_tests "$fn" "$file")"
    total=$((total + count))

    printf '%-16s %8d   %s\n' "$suite_name" "$count" "$file"
done < <(grep -oE 'new_testsuite\("[^"]+",[[:space:]]*[A-Za-z0-9_]+' "$RUN_TESTER")

printf '%-16s %8s\n' "--------------" "-----"
printf '%-16s %8d\n' "TOTAL" "$total"
