#!/usr/bin/env bash
#
# Prints everything needed to identify a machine and its toolchain, for the
# provenance section of a benchmarking report (see .claude/skills/plan-benchmark.md).
#
# Read-only: it inspects and prints, and builds nothing. Safe to run anywhere.
#
# Usage:
#   tools/machine_report.sh              # toolchain report only
#   tools/machine_report.sh --lto-probe  # also test that LTO can LINK here
#
# Run it AFTER activating whatever environment the machine needs (module load,
# an activation script, a conda env), so it reports what a build would really
# see rather than what a bare login shell has -- and once PER TOOLCHAIN when a
# campaign uses more than one. It describes the shell it is invoked from, so an
# un-activated run reports a default environment nothing is measured in, and a
# runner quoting it faithfully appears to have found something about the machine
# that is really a property of their shell. That has happened twice in one
# report: a system gfortran below this project's version floor, and an Arrow
# version from /usr/lib64 that neither activated toolchain links against.
#
# --lto-probe compiles and links a two-file mixed Fortran/C++ program with
# link-time optimisation, mirroring how this library is built (Fortran calling
# C++ across bind(C), pulling in libstdc++). It answers "does LTO link on this
# toolchain at all" in about fifteen seconds, which is worth knowing before a
# multi-minute benchmark run discovers it the hard way -- one machine's linker
# could not read the compiler's own bitcode and produced 8087 undefined
# references, which looks like a defect in this project and is not.
#
# ---------------------------------------------------------------------------------------------
# Prints everything needed to identify a machine and its toolchain -- CPU,
# SIMD, memory, every compiler on `PATH`, which compilers fpm will actually use, Arrow's version,
# the already-exported `FPM_*` variables, and the load -- and with `--lto-probe` additionally link-
# tests a minimal mixed Fortran/C++ program per toolchain in a few seconds. It builds nothing and is
# safe to run anywhere. `.claude/skills/plan-benchmark.md` is the template for a run on another machine:
# copy it to a `feature_*.md` file, fill in the campaign, and the machine that runs it writes its
# report back into that same file.
# ---------------------------------------------------------------------------------------------
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
echo "THIS DESCRIBES THE SHELL IT RAN IN, NOT THE MACHINE."
echo "Activate your environment FIRST, and run this once per toolchain."
echo "Un-activated, the compiler and library lines below are a DEFAULT"
echo "environment nothing is measured in -- quoting them as facts about"
echo "the machine has produced two false findings in one report."
echo "Cross-check: if the fc/cc/Arrow lines disagree with the FPM_* flags"
echo "printed further down, the FLAGS are what built the binary."
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

# List every executable on PATH named BASE, BASE-mp-*, or BASE-<version>, most specific first.
#
# A LIST OF UNSUFFIXED NAMES IS NOT AN INVENTORY. MacPorts, Homebrew and most distributions ship
# compilers under a suffixed name -- flang-mp-22, g++-mp-15, gcc-13, clang++-18 -- so probing only
# for `flang`/`g++` reports a machine that HAS them as though it did not. That is worse than
# printing nothing, because a report naming no flang reads as "no flang here" and gets quoted as
# such. Confirmed on machine C: flang 22.1.8 sat on PATH as flang-mp-22 while this script called it
# absent, and a benchmarking write-up recorded the machine as having no flang and this project's
# own machine table as stale. Both were wrong.
#
# This is CLAUDE.md's "a static check that enumerates names goes stale silently" -- so match by
# SHAPE (base plus the suffix conventions) rather than by an enumeration that has to be remembered.
compiler_variants() {
    local base="$1" dir f name out="" oldifs="$IFS"
    IFS=:
    set -- $PATH
    IFS="$oldifs"
    for dir in "$@"; do
        [ -d "$dir" ] || continue
        for f in "$dir/$base" "$dir/$base"-mp-* "$dir/$base"-[0-9]*; do
            [ -x "$f" ] && [ ! -d "$f" ] || continue
            name=$(basename "$f")
            case " $out " in *" $name "*) continue ;; esac   # first on PATH wins
            out="$out $name"
        done
    done
    printf '%s' "$out"
}

echo
echo "=== Fortran compilers on PATH (including -mp-/-version suffixed variants) ==="
for base in ifx ifort gfortran flang flang-new lfortran nvfortran; do
    for fc in $(compiler_variants "$base"); do
        printf '%-16s %s\n    -> %s\n' "$fc" "$(command -v "$fc")" "$("$fc" --version 2>&1 | head -1)"
    done
done

echo
echo "=== C / C++ compilers on PATH (including -mp-/-version suffixed variants) ==="
for base in icpx icx g++ c++ clang++ gcc clang; do
    for cxx in $(compiler_variants "$base"); do
        printf '%-16s %s\n    -> %s\n' "$cxx" "$(command -v "$cxx")" "$("$cxx" --version 2>&1 | head -1)"
    done
done

# The archiver decides whether -flto/-ipo can do anything ACROSS a static library, so it belongs in
# the provenance next to the compilers. Under LTO an object holds intermediate representation, and
# an archiver with no LTO plugin produces an archive the linker cannot optimise across -- the build
# then either fails or, worse, silently does no interprocedural optimisation and its numbers are a
# measurement of nothing. Apple's cctools `ar` (the default `ar` on macOS) has no plugin support;
# GCC ships gcc-ar, Intel ships xiar.
echo
echo "=== archivers (an LTO build across a static library needs a plugin-capable one) ==="
# NOTE: Apple's cctools `ar` has no --version and answers with a usage message on stderr, so this
# deliberately inspects the first line rather than trusting an exit status (`set -o pipefail` makes
# the obvious `... || echo fallback` print BOTH the usage text and the fallback).
ar_line=$(ar --version 2>&1 | head -1)
case "$ar_line" in
    *usage:*|"") ar_line="(no --version -- Apple cctools ar: NO LTO plugin, cannot archive IR)" ;;
esac
printf '%-16s %s\n    -> %s\n' "ar" "$(command -v ar 2>/dev/null || echo '(none)')" "$ar_line"
for base in gcc-ar xiar llvm-ar; do
    for a in $(compiler_variants "$base"); do
        printf '%-16s %s\n' "$a" "$(command -v "$a")"
    done
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
    # Pick the archiver the REAL build would use for this compiler family, so the probe's archive is
    # built the same way an LTO build of the library is archived. Plain `ar` is the fallback
    # and is itself informative: on macOS it is Apple cctools ar, which has no LTO plugin, so a probe
    # that passes with it has demonstrated linking and NOT interprocedural optimisation.
    probe_archiver() {  # $1 = fortran compiler
        local f="$1" maj cand
        maj="$("$f" -dumpversion 2>/dev/null | cut -d. -f1)"
        case "$("$f" --version 2>&1 | head -1)" in
            *ifx*|*Intel*|*flang*|*clang*) set -- "llvm-ar-mp-$maj" "llvm-ar-$maj" "llvm-ar" ;;
            *)                             set -- "gcc-ar-mp-$maj" "gcc-ar-$maj" "gcc-ar" ;;
        esac
        for cand in "$@"; do
            if command -v "$cand" >/dev/null 2>&1; then printf '%s' "$cand"; return 0; fi
        done
        printf 'ar'
    }

    # A SKIPPED PROBE AND A PASSED PROBE MUST NOT LOOK ALIKE. This used to `return 0` silently when
    # either compiler was missing, so an arm that never ran was indistinguishable from one that was
    # never written -- and on machine C the flang arm was quietly skipped (the compiler was there
    # under a suffixed name) while the report was read as "only one toolchain here". Say so instead.
    probe_one() {  # $1=fortran $2=c++ $3=flag $4..=extra link flags
        local f="$1" x="$2" flag="$3"; shift 3
        if ! command -v "$f" >/dev/null 2>&1; then
            echo "-- $f + $x, $flag $*: SKIPPED -- '$f' not found on PATH"; echo; return 0
        fi
        if ! command -v "$x" >/dev/null 2>&1; then
            echo "-- $f + $x, $flag $*: SKIPPED -- '$x' not found on PATH"; echo; return 0
        fi
        # THE PROBE ARCHIVES, because linking loose objects is not what the real build does and
        # cannot see the failure that actually occurs. fpm builds a STATIC LIBRARY and links that;
        # under -ipo/-flto its members hold IR, and it is the archive-plus-system-linker pair that
        # breaks. An earlier version of this probe linked m.o and c.o directly, passed on machine B,
        # and the real -ipo build of the library then died with 8087 undefined references -- a pass
        # that was read as clearance. Reproduce the structure or the probe answers a different
        # question than the one being asked.
        local arname
        arname="$(probe_archiver "$f")"
        echo "-- $f + $x, $flag $*  (archiving with $arname) --"
        ( cd "$d" \
          && "$f" -O2 "$flag" -c m.f90 -o m.o \
          && "$x" -O2 "$flag" -std=c++20 -c c.cpp -o c.o \
          && "$arname" -rs libprobe.a m.o c.o >/dev/null 2>&1 \
          && "$f" -O2 "$flag" "$@" -o probe -L. -lprobe -lstdc++ \
          && ./probe ) 2>&1 | tail -4
        echo "   exit: $?  (expect 'lto probe ok: 42' and 0)"
        rm -f "$d"/*.o "$d/libprobe.a" "$d/probe"
    }
    # ifx emits LLVM bitcode under -ipo; the system linker may be unable to read it, so the
    # Intel-supplied lld is tried as well. Report BOTH outcomes -- which one works is the thing a
    # later benchmark run needs to know.
    probe_one ifx icpx -ipo
    command -v ld.lld >/dev/null 2>&1 && probe_one ifx icpx -ipo -fuse-ld=lld
    # Probe every variant found above, pairing each Fortran compiler with the C++ compiler of its
    # OWN family and version -- gfortran-mp-15 with g++-mp-15, flang-mp-22 with clang++-mp-22. The
    # unsuffixed pair is probed too and is not the same experiment: on macOS a bare `g++` is Apple
    # clang, so `gfortran + g++` is a MIXED-family probe, which is worth knowing but is not what
    # "gfortran + g++" suggests. Both are reported, each under the names actually used.
    for fcv in $(compiler_variants gfortran); do
        cxxv=$(printf '%s' "$fcv" | sed 's/^gfortran/g++/')
        command -v "$cxxv" >/dev/null 2>&1 || cxxv=g++
        probe_one "$fcv" "$cxxv" -flto
    done
    for base in flang flang-new; do
        for fcv in $(compiler_variants "$base"); do
            cxxv=$(printf '%s' "$fcv" | sed "s/^$base/clang++/")
            command -v "$cxxv" >/dev/null 2>&1 || cxxv=clang++
            probe_one "$fcv" "$cxxv" -flto
        done
    done
    rm -rf "$d"
fi

echo
echo "=================================================================="
echo " end of report -- paste this into the benchmarking instructions file"
echo "=================================================================="
