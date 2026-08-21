#!/usr/bin/env bash
# Drives app/benchmark_sort_comparator.f90 -- feature_sort.md Stage 1e.
#
# Answers ONE question: what does a single comparison cost, in the new Fortran comparator core and
# in the shipped C++ one? That is feature_sort.md section 11.3's open question O3, and
# tools/benchmark_sort_engine.sh cannot answer it -- it measures whole sorts, so at Stage 2 a slow
# comparator and a slow introsort would arrive together and be indistinguishable.
#
# This is a SEPARATE harness rather than a mode of the sort-engine one, deliberately: its arms are
# per-CALL nanoseconds where that one's are per-element, and mixing the two invites a reader to
# compare figures that are not the same kind of number.
#
# Usage:
#   tools/benchmark_sort_comparator.sh
#   tools/benchmark_sort_comparator.sh --rows=8192 --reps=8192
#   ROUNDS=9 tools/benchmark_sort_comparator.sh
#
# Config (env-overridable; the matching --flag wins):
#   ROWS=4096     Rows per pass. Keep the fixture IN CACHE -- this measures a comparator, not the
#                  memory system. 4096 int64 keys is 32 KB. Raising this until it spills L2 turns
#                  the benchmark into a bandwidth test that will look like a comparator regression.
#   REPS=4096     Passes. ROWS*REPS is the comparison count per timed sweep (~16.7M by default).
#   ROUNDS=5      Timed rounds per arm; the MINIMUM is reported, being the round least disturbed.
#
# Output goes to stdout; redirect it per the run sheet. The build tree goes under test_run/, which
# is already git-ignored.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

ROWS="${ROWS:-4096}"
REPS="${REPS:-4096}"
ROUNDS="${ROUNDS:-5}"

for arg in "$@"; do
    case "$arg" in
        --rows=*)   ROWS="${arg#*=}" ;;
        --reps=*)   REPS="${arg#*=}" ;;
        --rounds=*) ROUNDS="${arg#*=}" ;;
        -h|--help)
            awk 'NR>1 { if ($0 !~ /^#/) exit; sub(/^# ?/, ""); print }' "${BASH_SOURCE[0]}"
            exit 0
            ;;
        *)
            echo "benchmark_sort_comparator.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done

# One build tree per compiler. The compiler goes in the NAME, not just the configuration:
# tools/benchmark_stage7.sh names its trees for the configuration alone, and running it under a
# second FPM_FC drops that compiler's binary into the same tree, after which a `find ... | head -1`
# lookup picks between them arbitrarily. Machine B runs two toolchains, so that trap is live here.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="${FC_TAG//[^A-Za-z0-9._-]/_}"
if [[ -z "${FPM_BUILD_DIR:-}" ]]; then
    export FPM_BUILD_DIR="test_run/cmpbench-${FC_TAG}"
else
    echo "benchmark_sort_comparator.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

echo "=============================================================================="
echo "benchmark_sort_comparator.sh"
echo "  build tree  : $FPM_BUILD_DIR"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}"
echo "  rows        : $ROWS      reps: $REPS      rounds: $ROUNDS"
echo "  date        : $(date -u '+%Y-%m-%dT%H:%M:%SZ')  host: $(hostname)"
echo "=============================================================================="
echo

# Some compilers optimise by DEFAULT, so "no -O in the flags" does not imply -O0 for them. Keep the
# list short and evidence-based: a wrong entry turns this check into the silent -O0 run it exists to
# prevent, and an -O0 run of this program would report a comparator cost several times the truth.
compiler_defaults_to_optimised() {
    case "$(basename "${FPM_FC:-gfortran}")" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}

FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]]; then
    echo "benchmark_sort_comparator.sh: could not read fortran_compile_flags from --show-model." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers." >&2
    echo "  Set SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_sort_comparator.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

fpm has no release profile for some compilers (flang, as of fpm 0.13.0 alpha), so this would be an
-O0 run reported as a release one.

If THIS compiler optimises by default (ifx does, at -O2), add it to compiler_defaults_to_optimised()
in this script rather than reaching for SKIP_OPT_CHECK -- an override and an appended -O3 are two
different configurations, and mixing them across machines is a comparability trap.

Otherwise append the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" tools/benchmark_sort_comparator.sh ...

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi

# THE SAME CHECK FOR THE C++ HALF, and it is not redundant: fpm derives the C/C++ compiler from the
# FORTRAN one's family, and for a family it does not recognise as a C family it emits NO profile
# flags for that half at all. Measured on this repository: `--profile release` gives
# cxx_compile_flags `-O3 -funroll-loops` under gfortran and NOTHING under nagfor or flang -- so the
# Arrow layer in src/parquet_wrapper.cpp compiles at the C++ compiler's own default, which is -O0
# for gcc and clang. The `_cpp` sweeps this benchmark exists to compare against are compiled from that file.
#     The failure mode is the one this project's tooling rules exist to prevent: the run passes the
# Fortran check above, prints a plausible table, and every C++-side figure in it is wrong.
# Demonstrated on benchmark_sort_readtime under nagfor at n = 10**6: the `bind` phase reported
# 25.9% of the open with an unoptimised C++ half and 2.9% with -O3, and the total went 97.45 ms to
# 36.91 ms. Nothing in the output distinguished the two.
CXX_FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
                  | grep -o 'cxx_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$CXX_FLAGS_LINE" ]]; then
    echo "benchmark_sort_comparator.sh: could not read cxx_compile_flags from --show-model;" >&2
    echo "  refusing to produce numbers. SKIP_OPT_CHECK=1 overrides, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$CXX_FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    echo "benchmark_sort_comparator.sh: '--profile release' produced NO optimisation flag for the C++ half:" >&2
    echo "  $CXX_FLAGS_LINE" >&2
    echo "  This is normal for nagfor and flang -- fpm gives their C/C++ half no profile flags." >&2
    echo "  Append one yourself (append, never assign -- FPM_CXXFLAGS carries Arrow's paths):" >&2
    echo "      FPM_CXXFLAGS=\"\${FPM_CXXFLAGS:-} -O3\" tools/benchmark_sort_comparator.sh" >&2
    echo "  and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi

fpm build --profile release >/dev/null
fpm run benchmark_sort_comparator --profile release -- \
    --rows="$ROWS" --reps="$REPS" --rounds="$ROUNDS"

# The ifx descriptor-block check (feature_sort.md Stage 1e). It is x86-64-specific: the relocation
# name it greps for does not exist on arm64, where it reports 0 whatever the code does. So it is
# printed as evidence only where it can mean something, and labelled where it cannot.
echo
echo "---- descriptor-block check (feature_sort.md Stage 1e) ----"
ENGINE_OBJ="$(find "$FPM_BUILD_DIR" -name "src_parquet_argsort_engine.f90.o" | head -n 1 || true)"
if [[ -z "$ENGINE_OBJ" ]]; then
    echo "  engine object not found under $FPM_BUILD_DIR -- cannot check."
elif [[ "$(uname -m)" != "x86_64" ]]; then
    echo "  SKIPPED -- $(uname -m), not x86_64. The R_X86_64 relocation this greps for cannot"
    echo "  exist here, so a 0 would prove nothing. Run this arm on machine B."
else
    for p in sort_row_less sort_compare_key sort_tier_of sort_keys_compare; do
        n="$(objdump -dr --no-show-raw-insn "$ENGINE_OBJ" 2>/dev/null \
             | awk "/<.*_MOD_${p}>:/,/^\$/" | grep -c 'R_X86_64.*\.bss' || true)"
        printf '  %-20s %s   (must be 0)\n' "$p" "$n"
    done
fi

echo
echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/cmpbench-* to clean up)."
