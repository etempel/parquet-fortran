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
# Maintainer-only: nothing outside this repository runs it.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

# Every runner, not just one. `test/run_tester.f90` is now group (b) alone -- the suites are
# partitioned across five programs (see feature_tests.md section 6), so reading one by name would
# silently report a fifth of the suite as the whole of it. Globbing keeps a sixth runner counted
# with no edit here; `check_test_runner_partition` is what proves nothing is registered twice or
# nowhere at all.
RUN_TESTERS=()
while IFS= read -r f; do RUN_TESTERS+=("$f"); done < <(ls test/run_tester*.f90 2>/dev/null | sort)

if [ ${#RUN_TESTERS[@]} -eq 0 ]; then
    echo "error: no test/run_tester*.f90 found" >&2
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

total=0
nsuites=0
for RUN_TESTER in "${RUN_TESTERS[@]}"; do
    runner="$(basename "$RUN_TESTER" .f90)"
    printf '\n=== %s\n' "$runner"
    printf '%-18s %8s   %s\n' "SUITE" "TESTS" "SOURCE"
    printf '%-18s %8s   %s\n' "----------------" "-----" "------"
    subtotal=0
    while IFS= read -r pair; do
        suite_name="$(sed -E 's/^new_testsuite\("([^"]+)",.*/\1/' <<< "$pair")"
        fn="$(sed -E 's/^new_testsuite\("[^"]+",[[:space:]]*([A-Za-z0-9_]+).*/\1/' <<< "$pair")"

        file="$(grep -lE "^[[:space:]]*subroutine[[:space:]]+${fn}[[:space:]]*\(" test/test_*.f90 | head -1)"
        if [ -z "$file" ]; then
            echo "error: no 'subroutine ${fn}(...)' found under test/test_*.f90 (referenced by" \
                "$RUN_TESTER for testsuite \"$suite_name\")" >&2
            exit 1
        fi

        count="$(count_suite_tests "$fn" "$file")"
        subtotal=$((subtotal + count))
        nsuites=$((nsuites + 1))

        printf '%-18s %8d   %s\n' "$suite_name" "$count" "$file"
    done < <(grep -oE 'new_testsuite\("[^"]+",[[:space:]]*[A-Za-z0-9_]+' "$RUN_TESTER")
    printf '%-18s %8s\n' "----------------" "-----"
    printf '%-18s %8d\n' "subtotal" "$subtotal"
    total=$((total + subtotal))
done

printf '\n%-18s %8s\n' "================" "====="
printf '%-18s %8d   (%d suites across %d runners)\n' "TOTAL" "$total" "$nsuites" "${#RUN_TESTERS[@]}"
