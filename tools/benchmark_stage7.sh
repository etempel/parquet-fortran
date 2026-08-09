#!/usr/bin/env bash
#
# Runs the stage 7 gate measurements (app/benchmark_stage7.f90) and prints, ahead of
# them, the toolchain provenance every number has to be read against.
#
# Usage:
#   tools/benchmark_stage7.sh [--both|--lto] [--only=s7-2,s7-4] [--test]
#
#   (no flags)   build and measure normally
#   --lto        build and measure with link-time optimisation instead
#   --both       measure BOTH ways and print them one after the other (this is
#                what a run on another machine should use -- it answers S7-8 and
#                every other gate in a single invocation, which is the point)
#   --only=LIST  restrict to some items, e.g. --only=s7-2 (default: all)
#   --test       additionally run the full test suite under the same build
#                flags. Slow, and required for the LTO measurement: a faster
#                build that fails the suite is not a result, and this project
#                has four documented ifx codegen bugs that appear only at -O1+.
#
# Environment:
#   FPM_FC, FPM_CXX and the FPM_*FLAGS variables are read from the environment
#   and APPENDED to, never replaced -- on every machine this project is built on
#   they already carry Arrow's include and link paths, and overwriting them
#   produces "fatal error: 'arrow/api.h' file not found", which reads like a
#   missing dependency rather than a flag mistake.
#
# The LTO flag is chosen from the Fortran compiler: -ipo for ifx, -flto
# otherwise. They are the same idea with different spellings; passing the wrong
# one is silently accepted by some drivers and ignored.
#
# ifx additionally needs a different LINKER, and that is the load-bearing part:
# under -ipo an Intel object holds LLVM bitcode rather than finished code, and the
# system GNU ld's plugin usually cannot read it, so the link dies with thousands
# of undefined references. oneAPI ships a matching ld.lld -- beside the compiler
# rather than on PATH -- and this script finds it there and fails outright if it
# cannot, because an -ipo run that silently fell back is not a slower run, it is a
# different one.
#
# The ARCHIVER is a refinement rather than a requirement on Intel: machine B
# measured -ipo with plain `ar` and still got real interprocedural optimisation.
# Note `xiar` no longer exists in oneAPI 2026.1 (retired in favour of llvm-ar), so
# any advice to source setvars.sh until it appears cannot be followed. GCC is the
# opposite case -- there a plugin-less archiver silently disables LTO with no error
# at all, which is why gcc-ar is chosen when available. fpm takes FPM_AR, so this
# script sets it for the LTO build only; the plain build keeps fpm's own choice.
#
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p test_run

MODE="plain"
ONLY=""
RUN_TESTS=0
for arg in "$@"; do
    case "$arg" in
        --both) MODE="both" ;;
        --lto)  MODE="lto" ;;
        --test) RUN_TESTS=1 ;;
        --only=*) ONLY="$arg" ;;
        *) echo "unknown argument: $arg" >&2; exit 2 ;;
    esac
done

# ---------------------------------------------------------------- provenance --
# ASK FPM which compilers it will use rather than guessing from FPM_FC/FPM_CXX. fpm derives the C
# (and C++) compiler from the Fortran compiler's FAMILY when FPM_CXX is unset, so `c++ --version`
# is not the compiler that will build parquet_wrapper.cpp -- under ifx it reports the system g++
# while fpm actually uses icpx. A provenance header that is wrong about the compiler is worse than
# no header, because every number under it gets filed against the wrong toolchain.
model_fc="$(fpm build --show-model 2>/dev/null | grep -oE 'fc="[^"]*"' | head -1 | sed 's/fc="//;s/"//')"
model_cc="$(fpm build --show-model 2>/dev/null | grep -oE 'cc="[^"]*"' | head -1 | sed 's/cc="//;s/"//')"
fc="${model_fc:-${FPM_FC:-gfortran}}"
# Map fpm's C driver to the matching C++ driver of the same family.
if [ -n "${FPM_CXX:-}" ]; then
    cxx="$FPM_CXX"
else
    case "$model_cc" in
        icx)   cxx="icpx" ;;
        gcc)   cxx="g++" ;;
        clang) cxx="clang++" ;;
        *)     cxx="c++" ;;
    esac
fi
echo "=================================================================="
echo " parquet-fortran stage 7 -- toolchain provenance"
echo "=================================================================="
echo " host        : $(uname -srm)"
echo " commit      : $(git rev-parse --short HEAD 2>/dev/null || echo '(not a git checkout)')$(git diff --quiet 2>/dev/null || echo ' + UNCOMMITTED CHANGES')"
echo " FPM_FC      : ${FPM_FC:-(unset)}   -> fpm will use: ${model_fc:-?}"
echo " fortran     : $("$fc" --version 2>&1 | head -1 || echo '?')"
echo " FPM_CXX     : ${FPM_CXX:-(unset)}   -> fpm derives C as: ${model_cc:-?}, so C++ is: $cxx"
echo " c++         : $("$cxx" --version 2>&1 | head -1 || echo '?')"
echo " arrow       : $(pkg-config --modversion arrow 2>/dev/null || echo '?')"
echo " parquet     : $(pkg-config --modversion parquet 2>/dev/null || echo '?')"
echo " fpm         : $(fpm --version 2>&1 | sed -n 's/^Version: *//p' | head -1)"
echo " cores       : $(nproc 2>/dev/null || sysctl -n hw.logicalcpu 2>/dev/null || echo '?')"
echo " mode        : $MODE"
echo "=================================================================="
echo

# Refuse to run if the environment ALREADY carries an LTO flag. This matters more than it looks:
# the "plain" arm of --both builds with --profile release, so an -ipo/-flto exported in the shell
# (or added by a wrapper function keyed on --profile release) would silently make the baseline an
# LTO build too. Both columns would then agree, and the honest-looking conclusion -- "LTO changes
# nothing" -- would be an artefact of measuring the same build twice. Fail loudly instead.
for v in "${FPM_FFLAGS:-}" "${FPM_CXXFLAGS:-}" "${FPM_LDFLAGS:-}"; do
    case " $v " in
        *" -ipo "*|*" -flto "*|*"-flto="*)
            echo "ERROR: an LTO flag is already present in the environment:" >&2
            echo "  FPM_FFLAGS  : ${FPM_FFLAGS:-(unset)}" >&2
            echo "  FPM_CXXFLAGS: ${FPM_CXXFLAGS:-(unset)}" >&2
            echo "  FPM_LDFLAGS : ${FPM_LDFLAGS:-(unset)}" >&2
            echo "This script builds a NON-LTO baseline to compare against, and cannot do that" >&2
            echo "while the environment forces LTO on every build -- the comparison would be" >&2
            echo "between two identical builds and would look like 'LTO changes nothing'." >&2
            echo "Clear the flag for this run, e.g.:" >&2
            echo "  FPM_FFLAGS= FPM_CXXFLAGS= FPM_LDFLAGS= tools/benchmark_stage7.sh --both" >&2
            echo "...but note that also drops Arrow's paths on machines that keep them there;" >&2
            echo "prefer disabling whatever adds the LTO flag (see BENCHMARKING/CONTRIBUTING)." >&2
            exit 2
            ;;
    esac
done

# Pick the right link-time-optimisation flag, and the archiver/linker that go with it.
LTO_AR=""
LTO_LD=""
fc_major="$("$fc" -dumpversion 2>/dev/null | cut -d. -f1)"
if "$fc" --version 2>&1 | head -1 | grep -qi "ifx\|intel"; then
    LTO_FLAG="-ipo"
    # -ipo makes ifx emit LLVM BITCODE objects, and the system GNU ld's gold plugin is built
    # against the distro's LLVM. When that is older than the LLVM inside oneAPI, it cannot parse
    # the bitcode: every archive member reads as containing no symbols and the link dies with
    # thousands of undefined <module>_mp_<proc>_ references, which looks like a vtable-emission
    # bug in this project and is not. oneAPI ships a matching ld.lld next to ifx, so selecting it
    # is what makes -ipo usable at all. Measured on machine B: 8087 undefined references without
    # this, clean link and a full passing test suite with it.
    #
    # RESOLVE IT RELATIVE TO THE COMPILER, not from PATH. Machine B's own documented activation
    # script (activate_qmost_env.sh) puts .../compiler/2026.1/bin on PATH while ld.lld lives one
    # directory further down, in .../bin/compiler -- so a `command -v ld.lld` gate declines there,
    # -fuse-ld=lld is silently never passed, and the run reproduces the exact failure this flag
    # exists to remove. That happened, and the warning printed hundreds of lines earlier was no
    # defence at all.
    lld_dir=""
    if command -v ld.lld >/dev/null 2>&1; then
        LTO_LD="-fuse-ld=lld"
    else
        fc_bin="$(dirname "$(command -v "$fc" 2>/dev/null || echo /nonexistent)")"
        for cand in "$fc_bin/compiler" "$fc_bin/../bin/compiler" "$fc_bin/../bin-llvm"; do
            if [ -x "$cand/ld.lld" ]; then lld_dir="$(cd "$cand" && pwd)"; break; fi
        done
        if [ -n "$lld_dir" ]; then
            echo "note: ld.lld found beside the compiler but not on PATH; adding $lld_dir" >&2
            PATH="$lld_dir:$PATH"; export PATH
            LTO_LD="-fuse-ld=lld"
        fi
    fi
    # FAIL rather than degrade. A run that could not assemble the configuration it was asked for is
    # not a slower run, it is a different one -- and its output is indistinguishable from a result.
    if [ -z "$LTO_LD" ] && [ "$MODE" != "plain" ]; then
        echo "ERROR: ifx detected, LTO requested, and 'ld.lld' could not be found." >&2
        echo "  -ipo emits LLVM bitcode objects that the system linker's plugin is typically too" >&2
        echo "  old to read, giving thousands of undefined <module>_mp_<proc>_ references -- which" >&2
        echo "  reads as a defect in this library and is not." >&2
        echo "  Looked on PATH and beside the compiler ($fc_bin/compiler, ../bin/compiler)." >&2
        echo "  Put oneAPI's linker directory on PATH and rerun, e.g.:" >&2
        echo "    export PATH=\"/opt/intel/oneapi/compiler/<ver>/bin/compiler:\$PATH\"" >&2
        echo "  Refusing to measure an -ipo build that cannot link. Use --plain to skip LTO." >&2
        exit 2
    fi
    # NOT xiar: it does not exist in oneAPI 2026.1 at all (Intel retired it), so warning about its
    # absence names a fix that cannot work. llvm-ar in the same bin/compiler directory replaces it.
    # This is a refinement rather than a requirement on Intel -- machine B measured -ipo with plain
    # `ar` archiving and still got real interprocedural optimisation (the temporal-validity figure
    # halved), so ld.lld above is the load-bearing half and this is not.
    for cand in llvm-ar xiar; do
        if command -v "$cand" >/dev/null 2>&1; then LTO_AR="$cand"; break; fi
    done
    if [ -z "$LTO_AR" ]; then
        echo "note: neither llvm-ar nor xiar found; archiving -ipo objects with plain 'ar'." >&2
        echo "  Measured as working on machine B; the link, not the archive, is what -ipo needs." >&2
    fi
else
    LTO_FLAG="-flto"
    # GCC HAS THE SAME ARCHIVER PROBLEM AS INTEL, and it is easy to miss because the build succeeds.
    # Under -flto a GCC object carries GIMPLE IR; an archiver with no LTO plugin indexes only what it
    # can see, so the linker takes the machine-code half of a fat object and does NO cross-module
    # optimisation -- the LTO arm then measures a normal build with bigger objects. It does not fail
    # and it does not warn. GCC ships gcc-ar (a thin wrapper passing -plugin liblto_plugin) exactly
    # for this. On Linux, binutils ar usually loads the plugin itself; on macOS the default `ar` is
    # Apple cctools ar, which cannot, which is where this was found: machine C's -flto arm produced a
    # 14.2 MB archive against 5.2 MB plain, still full of ordinary text symbols, and measured no gain
    # anywhere. Prefer a versioned gcc-ar matching the compiler (MacPorts ships gcc-ar-mp-15 beside
    # gfortran-mp-15), then a plain gcc-ar, and warn rather than silently measuring nothing.
    # flang is LLVM, not GCC: its IR is LLVM bitcode and llvm-ar is what reads it, so asking for
    # gcc-ar there would warn about the wrong missing tool and name a fix that cannot work.
    if "$fc" --version 2>&1 | head -1 | grep -qi "flang\|clang"; then
        ar_family="llvm-ar"
        ar_cands="llvm-ar-mp-${fc_major} llvm-ar-${fc_major} llvm-ar"
    else
        ar_family="gcc-ar"
        ar_cands="gcc-ar-mp-${fc_major} gcc-ar-${fc_major} gcc-ar"
    fi
    for cand in $ar_cands; do
        if command -v "$cand" >/dev/null 2>&1; then LTO_AR="$cand"; break; fi
    done
    if [ -z "$LTO_AR" ]; then
        echo "WARNING: no $ar_family found; the LTO build will archive with '$(command -v ar)'." >&2
        echo "  If that archiver has no LTO plugin (Apple's cctools ar has none), the static" >&2
        echo "  library keeps IR the linker will not use, so the build quietly does NO" >&2
        echo "  interprocedural optimisation and its numbers are a measurement of nothing." >&2
        echo "  Install/expose $ar_family (MacPorts: ${ar_family}-mp-<N>) and rerun before trusting -flto." >&2
    fi
fi

# A tag identifying the TOOLCHAIN, so a second compiler cannot land in the first one's tree. See
# run_one's own comment for why sharing a tree between compilers is a false-green rather than a mess.
FC_TAG="$(basename "$fc")"
[ -n "$fc_major" ] && FC_TAG="$FC_TAG-$fc_major"
FC_TAG="$(printf '%s' "$FC_TAG" | tr -c 'A-Za-z0-9._-' '_')"

run_one() {  # $1 = "plain" | "lto"
    local label="$1"
    local ff="${FPM_FFLAGS:-}" cf="${FPM_CXXFLAGS:-}" lf="${FPM_LDFLAGS:-}"
    # A build tree PER CONFIGURATION. Not tidiness: tools/run_error_scenarios.sh resolves its
    # executable with `find "${FPM_BUILD_DIR:-build}" -name error_scenarios | head -n 1`, so with
    # both configurations sharing one tree the LTO phase can silently re-test the PLAIN binary and
    # print "error scenarios : PASS" -- a false green, and precisely the stale-binary trap CLAUDE.md
    # documents for that script. Separate trees leave exactly one candidate each. It also keeps the
    # caller's own build/ untouched.
    # Under test_run/, which this project already git-ignores and already uses for test output --
    # so no new ignore rule is needed. Deliberately NOT under build/: run_error_scenarios.sh (and
    # CLAUDE.md's stale-binary guidance) do a `find build -name error_scenarios`, so a tree nested
    # inside build/ would make the STANDALONE runner ambiguous long after this script exited,
    # trading one false-green for another. The suite writes its fixtures to test_run/ top level,
    # so a subdirectory here cannot collide with them.
    #
    # THE TAG ALSO CARRIES THE COMPILER, because per-configuration alone was not enough. fpm keeps
    # different compilers' OBJECTS apart in its own per-compiler subdirectory, but the `find … |
    # head -n 1` lookup above does not -- so a second toolchain run into the same tree leaves two
    # error_scenarios binaries and one is chosen arbitrarily, which is a false green rather than an
    # error. Machine B hit this and deleted the trees by hand between ifx and gfortran; machine C
    # declined to run a second compiler at all because of it.
    local bdir="test_run/s7-$FC_TAG-$label"
    if [ "$label" = "lto" ]; then
        ff="$ff $LTO_FLAG"; cf="$cf $LTO_FLAG"; lf="$lf $LTO_FLAG${LTO_LD:+ $LTO_LD}"
        echo "###################### BUILD: $label ($LTO_FLAG${LTO_LD:+ $LTO_LD}${LTO_AR:+, FPM_AR=$LTO_AR}) ######################"
    else
        echo "###################### BUILD: $label (no LTO) ######################"
    fi
    echo "  build tree: $bdir"
    # A subshell, so the caller's environment is untouched and the conditional FPM_AR needs no
    # array. (An array would be the obvious way to carry an optional env assignment, but macOS
    # ships bash 3.2, where expanding an EMPTY array under `set -u` is an "unbound variable"
    # error -- so it would have worked on the Linux machine and failed on the macOS one.)
    (
        export FPM_BUILD_DIR="$bdir"
        export FPM_FFLAGS="$ff" FPM_CXXFLAGS="$cf" FPM_LDFLAGS="$lf"
        if [ "$label" = "lto" ] && [ -n "$LTO_AR" ]; then
            export FPM_AR="$LTO_AR"
        fi
        fpm run --profile release benchmark_stage7 -- ${ONLY:+"$ONLY"}
        if [ "$RUN_TESTS" -eq 1 ]; then
            echo
            echo "-------- full test suite under this build ($label) --------"
            if fpm test > "test_run/s7-test-$FC_TAG-$label.log" 2>&1; then
                echo "fpm test            : PASS ($(grep -c PASSED "test_run/s7-test-$FC_TAG-$label.log") assertions)"
            else
                echo "fpm test            : *** FAIL *** -- last 40 lines:"
                tail -40 "test_run/s7-test-$FC_TAG-$label.log"
            fi
            if ./tools/run_error_scenarios.sh > "test_run/s7-scen-$FC_TAG-$label.log" 2>&1; then
                echo "error scenarios     : PASS"
            else
                echo "error scenarios     : *** FAIL *** -- last 30 lines:"
                tail -30 "test_run/s7-scen-$FC_TAG-$label.log"
            fi
        fi
    )
    echo
}

case "$MODE" in
    plain) run_one plain ;;
    lto)   run_one lto ;;
    both)  run_one plain; run_one lto ;;
esac

echo "Left in place for inspection, all under the already-git-ignored test_run/:"
echo "  test_run/s7-$FC_TAG-plain/  test_run/s7-$FC_TAG-lto/   (build trees, as applicable)"
echo "  test_run/s7-test-*.log  test_run/s7-scen-*.log   (suite output)"
echo "They are outside build/, so the ordinary fpm build and tools/run_error_scenarios.sh are"
echo "unaffected. Remove them with: rm -rf test_run/s7-*"

