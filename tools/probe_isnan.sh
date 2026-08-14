#!/usr/bin/env bash
# Drives app/probe_isnan.f90 -- a diagnostic for the Stage 1e comparator campaign.
#
# Question: does `ieee_is_nan` cost the same as the alternatives on THIS toolchain? On machine A
# (arm64, gfortran 15.2) it compiles to a single `fcmp d,d`; the campaign's machine B figures imply
# it costs ~2.7 ns there, which would explain the whole f64-specific half of the Fortran comparator's
# penalty. See app/probe_isnan.f90's own header for the decomposition.
#
# Touches no library code -- a result here cannot be blamed on parquet_sorting.
#
# Usage: tools/probe_isnan.sh
set -euo pipefail
ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="${FC_TAG//[^A-Za-z0-9._-]/_}"
export FPM_BUILD_DIR="${FPM_BUILD_DIR:-test_run/isnan-${FC_TAG}}"

compiler_defaults_to_optimised() {
    case "$(basename "${FPM_FC:-gfortran}")" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}
FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]] || { [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; }; then
    echo "probe_isnan.sh: no optimisation flag for this compiler -- refusing to produce numbers." >&2
    echo "  $FLAGS_LINE" >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi

echo "fortran : ${FPM_FC:-gfortran (fpm default)}   tree: $FPM_BUILD_DIR"
fpm build --profile release >/dev/null
fpm run probe_isnan --profile release --

echo
echo "---- how the compiler emitted each arm ----"
OBJ="$(find "$FPM_BUILD_DIR" -name "app_probe_isnan.f90.o" | head -n 1 || true)"
if [[ -z "$OBJ" ]]; then
    echo "  probe object not found -- skipped."
else
    echo "  undefined libm-ish symbols referenced (a CALL per test would show here):"
    nm -u "$OBJ" 2>/dev/null | grep -iE "isnan|fpclassify|__nan" | sed 's/^/    /' \
        || echo "    none -- every arm is inlined to instructions"
fi
