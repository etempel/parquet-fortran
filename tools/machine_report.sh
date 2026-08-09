#!/usr/bin/env bash
#
# Prints everything needed to identify a machine and its toolchain, for the
# provenance section of a benchmarking report (see tools/benchmark_template.md).
#
# Read-only: it inspects and prints, and builds nothing. Safe to run anywhere.
#
# Usage:
#   tools/machine_report.sh              # toolchain report only
#   tools/machine_report.sh --lto-probe  # also test that LTO can LINK here
#
# Run it AFTER activating whatever environment the machine needs (module load,
# an activation script, a conda env), so it reports what a build would really
# see rather than what a bare login shell has.
#
# --lto-probe compiles and links a two-file mixed Fortran/C++ program with
# link-time optimisation, mirroring how this library is built (Fortran calling
# C++ across bind(C), pulling in libstdc++). It answers "does LTO link on this
# toolchain at all" in about fifteen seconds, which is worth knowing before a
# multi-minute benchmark run discovers it the hard way -- one machine's linker
# could not read the compiler's own bitcode and produced 8087 undefined
# references, which looks like a defect in this project and is not.
#
set -uo pipefail
cd "$(dirname "$0")/.."

want_lto=0
for a in "$@"; do
    case "$a" in
        --lto-probe) want_lto=1 ;;
        *) echo "unknown argument: $a" >&2; exit 2 ;;
    esac
done

echo "=================================================================="
echo " machine and toolchain report"
echo "=================================================================="
echo "date        : $(date -u '+%Y-%m-%d %H:%M UTC')"
echo "host        : $(hostname 2>/dev/null || echo '?')"
echo "uname       : $(uname -srm)"
echo "repo commit : $(git rev-parse --short HEAD 2>/dev/null || echo '(not a git checkout)')$(git diff --quiet 2>/dev/null || echo ' + UNCOMMITTED CHANGES')"

echo
echo "--- CPU ---"
if command -v lscpu >/dev/null 2>&1; then
    lscpu | grep -E "^(Model name|Architecture|Socket|Core\(s\)|Thread\(s\)|CPU\(s\)|NUMA node\(s\))"
else
    sysctl -n machdep.cpu.brand_string 2>/dev/null
    echo "physical cores : $(sysctl -n hw.physicalcpu 2>/dev/null || echo '?')"
    echo "logical cores  : $(sysctl -n hw.logicalcpu 2>/dev/null || echo '?')"
fi

echo
echo "--- SIMD (x86: which AVX-512 subsets, if any) ---"
if command -v lscpu >/dev/null 2>&1; then
    avx=$(lscpu | tr ' ' '\n' | grep -oE "avx512[a-z0-9_]*" | sort -u | tr '\n' ' ')
    echo "${avx:-none reported (check for avx2 / NEON as appropriate)}"
else
    sysctl -n machdep.cpu.leaf7_features 2>/dev/null || echo "(arm64: NEON, 128-bit)"
fi

echo
echo "--- memory ---"
if command -v free >/dev/null 2>&1; then
    free -g | head -2
else
    b=$(sysctl -n hw.memsize 2>/dev/null || echo 0); echo "total GB: $((b / 1024 / 1024 / 1024))"
fi

echo
echo "=== Fortran compilers on PATH ==="
for fc in ifx ifort gfortran flang flang-new lfortran nvfortran; do
    if command -v "$fc" >/dev/null 2>&1; then
        printf '%-10s %s\n    -> %s\n' "$fc" "$(command -v "$fc")" "$("$fc" --version 2>&1 | head -1)"
    fi
done

echo
echo "=== C / C++ compilers on PATH ==="
for cxx in icpx icx g++ c++ clang++ gcc clang; do
    if command -v "$cxx" >/dev/null 2>&1; then
        printf '%-10s %s\n    -> %s\n' "$cxx" "$(command -v "$cxx")" "$("$cxx" --version 2>&1 | head -1)"
    fi
done

echo
echo "=== what FPM will actually use (authoritative -- fpm derives C/C++ from the"
echo "=== Fortran compiler's family when FPM_CXX/FPM_CC are unset) ==="
if command -v fpm >/dev/null 2>&1; then
    echo "fpm         : $(command -v fpm) -- $(fpm --version 2>&1 | sed -n 's/^Version: *//p' | head -1)"
    model=$(fpm build --show-model 2>/dev/null)
    echo "fc          : $(printf '%s' "$model" | grep -oE 'fc="[^"]*"' | head -1 | sed 's/fc="//;s/"//')"
    echo "cc          : $(printf '%s' "$model" | grep -oE 'cc="[^"]*"' | head -1 | sed 's/cc="//;s/"//')"
else
    echo "fpm         : NOT FOUND -- nothing can be built or measured here"
fi

echo
echo "=== Arrow / Parquet C++ ==="
for pkg in arrow parquet arrow-compute; do
    printf '%-14s %s\n' "$pkg" "$(pkg-config --modversion "$pkg" 2>/dev/null || echo '(not found via pkg-config)')"
done

echo
echo "=== build environment ALREADY exported (must be APPENDED to, never replaced) ==="
for v in FPM_FC FPM_CXX FPM_CC FPM_AR FPM_FFLAGS FPM_CXXFLAGS FPM_LDFLAGS FPM_BUILD_DIR \
         OMP_NUM_THREADS OMP_PROC_BIND OMP_PLACES; do
    if [ -n "${!v+x}" ]; then printf '%-16s = %s\n' "$v" "${!v}"; else printf '%-16s (unset)\n' "$v"; fi
done

echo
echo "=== load (a benchmark run needs an idle machine) ==="
uptime 2>/dev/null || echo "?"

if [ "$want_lto" -eq 1 ]; then
    echo
    echo "=================================================================="
    echo " LTO link probe -- can this toolchain link a mixed Fortran/C++ program"
    echo " with link-time optimisation at all?"
    echo "=================================================================="
    d=$(mktemp -d) || exit 1
    cat > "$d/m.f90" <<'EOF'
program m
  use iso_c_binding, only: c_int
  implicit none
  interface
     function cxx_add(a, b) bind(C, name="cxx_add") result(r)
       import :: c_int
       integer(c_int), value :: a, b
       integer(c_int) :: r
     end function
  end interface
  print *, "lto probe ok:", cxx_add(20, 22)
end program
EOF
    cat > "$d/c.cpp" <<'EOF'
#include <string>
#include <vector>
extern "C" int cxx_add(int a, int b) {
    std::vector<std::string> v{"a", "b"};
    return a + b + static_cast<int>(v.size()) - 2;
}
EOF
    probe_one() {  # $1=fortran $2=c++ $3=flag $4..=extra link flags
        local f="$1" x="$2" flag="$3"; shift 3
        command -v "$f" >/dev/null 2>&1 || return 0
        command -v "$x" >/dev/null 2>&1 || return 0
        echo "-- $f + $x, $flag $* --"
        ( cd "$d" \
          && "$f" -O2 "$flag" -c m.f90 -o m.o \
          && "$x" -O2 "$flag" -std=c++20 -c c.cpp -o c.o \
          && "$f" -O2 "$flag" "$@" m.o c.o -lstdc++ -o probe \
          && ./probe ) 2>&1 | tail -4
        echo "   exit: $?  (expect 'lto probe ok: 42' and 0)"
        rm -f "$d"/*.o "$d/probe"
    }
    # ifx emits LLVM bitcode under -ipo; the system linker may be unable to read it, so the
    # Intel-supplied lld is tried as well. Report BOTH outcomes -- which one works is the thing a
    # later benchmark run needs to know.
    probe_one ifx icpx -ipo
    command -v ld.lld >/dev/null 2>&1 && probe_one ifx icpx -ipo -fuse-ld=lld
    probe_one gfortran g++ -flto
    probe_one flang clang++ -flto
    rm -rf "$d"
fi

echo
echo "=================================================================="
echo " end of report -- paste this into the benchmarking instructions file"
echo "=================================================================="
