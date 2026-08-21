#!/usr/bin/env bash
#
# Build `parquet_random` BOTH ways -- with the route (e) 128-bit kernel and with the wrapping
# kernel -- across several optimisation settings including LTO, and assert the contract holds in
# every one.
#
# Why this exists, and why it is not an `fpm test` arm:
#
#   * The wrapping kernel (`#else` in src/parquet_random.f90's fork) ships wherever the compiler
#     has no 128-bit integer kind, which today means ifx. Every other compiler in the fleet takes
#     the protected path, so nothing routine ever BUILDS the wrapping arithmetic, let alone runs
#     it. It was found miscompiled under LTO on two separate gfortran releases.
#   * The two releases broke DIFFERENT, non-overlapping sets of call shapes -- 15.2 broke
#     literal-constant-seed calls while all-variable calls stayed correct, 14.2.1 did the exact
#     reverse. So the check sweeps both shapes, and no single shape may be taken as representative.
#   * fpm cannot express this. Forcing the other kernel needs `-U__GFORTRAN__`, which also flips
#     `src/parquet.f90`'s stringify branch (it selects between gfortran's traditional-cpp hack and
#     standard cpp), so the package will not compile with it. A standalone compile of the one
#     module, its reference and its vectors is the only form that works.
#
# The vacuity guard matters more than the assertions: if `-U__GFORTRAN__` ever stops defeating the
# allowlist, this script would happily build the SAME kernel twice and report green. So the driver
# prints which kernel it compiled and the script requires the two builds to disagree.
#
# Usage:  tools/check_random_kernels.sh [--quick] [--shipped-only]
#   FC=<compiler>    Fortran compiler to use (default: gfortran). gfortran, flang, ifx and
#                    nagfor are all understood; run it under nagfor too, since that is the only
#                    compiler that SHIPS the safe64 arm.
#   --quick          Only the LTO configurations, which are the ones that have ever failed.
#   --shipped-only   Skip the forced-kernel half; check only the kernel this compiler ships.
#
# `--shipped-only` is what CI runs, and the reason is worth stating plainly rather than leaving as
# a flag someone later "tidies up". On gfortran the forced half builds the wrapping kernel, which
# gfortran NEVER SHIPS -- it has a 128-bit kind and always takes the protected arm. That half
# FAILS at `-O3 -flto` and `-Ofast -flto`, reproducibly, because gfortran miscompiles the wrapping
# arithmetic under LTO (see feature_risks.md Risk-101). That is a real hazard and a real gfortran
# bug, but it is not a defect in any configuration this library ships: gfortran ships the int128
# kernel and is clean, ifx ships the wrapping kernel and is clean at every setting measured
# including `-ipo`. CI checks the shipped kernel -- which nothing else covers under LTO -- and the
# forced half stays the default for anyone running this by hand.
#
# **Those two arms are now listed as KNOWN EXPOSURES (`xfail_reason` below) rather than simply
# failing the run**, and the reason is that a check which always fails is a check nobody reads. It
# had already gone wrong exactly that way: a hand run was written up as "2 of 12 arms failing" as
# though it were an open problem, when this header already said it was expected. The list is
# checked in both directions -- a listed arm that PASSES is a hard error -- so it cannot decay into
# a way of silencing failures.
#
# Exits nonzero on any UNLISTED mismatch, on a listed arm that unexpectedly passes, on a build that
# fails, or if the two kernels cannot be told apart. Exits zero when only listed exposures fail.

set -uo pipefail

cd "$(dirname "$0")/.."

# Follow the activated environment rather than assuming gfortran. This is not a convenience: on a
# machine whose ifx environment is active, a bare `gfortran` resolves to the SYSTEM compiler, which
# on the reference machine is 11.5.0 -- below this project's floor of 13 and documented to
# miscompile it. A first version of this script defaulted to `gfortran` and produced a confident,
# entirely spurious set of failures from exactly that compiler.
FC="${FC:-${FPM_FC:-gfortran}}"
QUICK=0
SHIPPED_ONLY=0
for arg in "$@"; do
    case "$arg" in
        --quick)        QUICK=1 ;;
        --shipped-only) SHIPPED_ONLY=1 ;;
        *) echo "check_random_kernels.sh: unknown argument '$arg'" >&2; exit 2 ;;
    esac
done

if ! command -v "$FC" >/dev/null 2>&1; then
    echo "check_random_kernels.sh: compiler '$FC' not found on PATH" >&2
    exit 2
fi

# Refuse a below-floor gfortran outright. README.md's prerequisites put the minimum at 13, because
# 11 miscompiles an optional allocatable-character argument elsewhere in the library; a run here
# would report failures that say nothing about the code under test.
case "$FC" in
    *gfortran*|*gcc*)
        ver="$("$FC" -dumpfullversion 2>/dev/null || "$FC" -dumpversion 2>/dev/null)"
        major="${ver%%.*}"
        if [ -n "$major" ] && [ "$major" -lt 13 ] 2>/dev/null; then
            echo "check_random_kernels.sh: $FC is $ver; this project requires gfortran >= 13." >&2
            echo "  A run with this compiler proves nothing. Activate the intended toolchain, or" >&2
            echo "  set FC/FPM_FC explicitly to the compiler you meant." >&2
            exit 2
        fi
        ;;
esac

# src/parquet_random.f90 compiles ALONE, and that is the whole reason this check can exist: it
# imports nothing but iso_fortran_env, so a plain ordered compile with no dependency resolver can
# build the kernel against several compilers and flag sets without an Arrow install anywhere.
#
# It has not always been so, and the failure is loud rather than subtle. When the weighted draw
# briefly lived in that module it brought `parquet_sorting` with it -- hence `parquet_bindings`,
# hence the whole of parquet_wrapper.cpp and Arrow -- and every configuration here died on a
# missing parquet_sorting.mod, leaving the script unable to read a kernel out of the driver at
# all. Everything needing more than the generator now lives in src/parquet_sampling.f90, which is
# deliberately not compiled here. If a `use` is ever added to parquet_random, adding its module to
# SRC is the WRONG fix; the right one is to move whatever needed it into parquet_sampling.
SRC="src/parquet_expkey.f90 src/parquet_ziggurat.f90 src/parquet_random.f90 test/test_random_reference.f90 test/test_random_vectors.f90 tools/check_random_kernels.f90"
REPO="$PWD"
ABS_SRC=""
for f in $SRC; do ABS_SRC="$ABS_SRC $REPO/$f"; done
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# The flag that defeats the allowlist depends on which compiler defines what. Keep this in step
# with the `#if defined(...)` line at the top of src/parquet_random.f90.
case "$FC" in
    *flang*) UNDEF="-U__flang__ -U__FLANG" ; CPP="-cpp" ; FAMILY="flang" ;;
    *ifx*|*ifort*) UNDEF="" ; CPP="-fpp" ; FAMILY="intel" ;;  # already wrapping; nothing to defeat
    # nagfor is the ONLY compiler here that ships the safe64 arm, so it is the only one that can
    # test that arm as its own build rather than as a simulation. It spells the preprocessor flag
    # `-fpp`, and `-cpp` is not merely ignored -- it is a hard `Option error`, so before this case
    # existed EVERY configuration failed to build and the script exited saying it proved nothing.
    # That is exactly how a wrong-answer defect in `ult` reached the golden vectors: the arm was
    # correct under a gfortran forced onto it, and no run ever compiled it with the compiler that
    # actually uses it. Both NAG macros have to go to defeat the allowlist -- the `#elif` names
    # them both, and undefining one leaves the other selecting safe64 again.
    # nagfor has no `-U` at ALL -- its `-u` means IMPLICIT NONE, and `-Wp,-U...` does not reach
    # fpp either (both tried) -- so the allowlist cannot be defeated with a flag here. It is
    # defeated by PRE-EXPANDING the source with an external cpp instead (see NAG_FORCED_SRC
    # below), which is what lets a nagfor run cover two arms rather than one. Deliberately not
    # solved by adding a `-DPF_FORCE_*` escape hatch to the fork: src/parquet_random.f90's header
    # says such a macro is absent on purpose, because a consumer-settable kernel switch is exactly
    # the silent wrong-kernel hazard the allowlist exists to close. A test harness may preprocess
    # the source it is testing; the shipped fork may not grow a door.
    *nagfor*) UNDEF="" ; CPP="-fpp" ; FAMILY="nag" ;;
    *)       UNDEF="-U__GFORTRAN__" ; CPP="-cpp" ; FAMILY="gnu" ;;
esac

# nagfor's forced half: the same source with every allowlist macro expanded away, so the compiler
# is handed a file that already selects the wrapping kernel. `-P` suppresses cpp's linemarkers,
# which Fortran cannot parse, and `-traditional-cpp` is the mode this project preprocesses with
# everywhere else (fpm's [preprocess.cpp], FORD's own preprocessor line). The undefines are passed
# explicitly rather than relying on this cpp not defining them, so the result does not depend on
# whether `cpp` here is clang's or GCC's.
#
# What this cannot reach is the int128 arm: nagfor has no 128-bit integer kind, so that arm does
# not compile there at all, and its coverage stays with the families that have one.
NAG_FORCED_SRC=""
if [ "$FAMILY" = "nag" ]; then
    if ! command -v cpp >/dev/null 2>&1; then
        echo "check_random_kernels.sh: no external 'cpp' found; nagfor cannot reach the wrapping" >&2
        echo "  kernel without one (it has no -U). Install cpp, or accept that this run covers" >&2
        echo "  only the arm nagfor ships." >&2
        exit 2
    fi
    NAG_FORCED_SRC="$WORK/parquet_random_forced.f90"
    # Redirect rather than `-o`: Apple's cpp ignores `-o` in that position and writes the whole
    # expansion to stdout, which leaves an EMPTY output file and a compile that fails several
    # steps later with "no input files" -- a message that says nothing about its cause.
    if ! cpp -traditional-cpp -E -P \
            -U__GFORTRAN__ -U__flang__ -U__FLANG -U__NAG_COMPILER_RELEASE -UNAGFOR \
            "$REPO/src/parquet_random.f90" > "$NAG_FORCED_SRC" 2>"$WORK/cpp.err"; then
        echo "check_random_kernels.sh: pre-expanding parquet_random.f90 for the forced half failed:" >&2
        sed 's/^/  /' "$WORK/cpp.err" >&2
        exit 2
    fi
    # cpp warns about every apostrophe in a Fortran comment, so its stderr is noise; its OUTPUT is
    # what matters. Assert the expansion actually produced the module before handing it on.
    if ! grep -q '^ *module parquet_random' "$NAG_FORCED_SRC"; then
        echo "check_random_kernels.sh: the pre-expanded source does not contain parquet_random --" >&2
        echo "  the forced half would test nothing. Check the cpp invocation above." >&2
        exit 2
    fi
fi

# Flag spellings are per family. The interprocedural configurations matter most -- they are the
# only ones that have ever failed here -- so each family must contribute one.
case "$FC" in
    *ifx*|*ifort*)
        # `-ipo` cannot link without ld.lld, which oneAPI puts one directory BELOW the one its
        # activation script adds to PATH. Resolve it relative to the compiler rather than trusting
        # PATH, and say so if it is missing instead of quietly dropping the flag.
        ipo_dir="$(dirname "$(command -v "$FC")")/compiler"
        if [ -x "$ipo_dir/ld.lld" ]; then
            PATH="$ipo_dir:$PATH"
            export PATH
        else
            echo "check_random_kernels.sh: ld.lld not found at $ipo_dir; -ipo cannot link." >&2
            exit 2
        fi
        if [ "$QUICK" = "1" ]; then
            CONFIGS=("-O3 -ipo")
        else
            CONFIGS=("-O0" "-O2" "-O3" "-O3 -xHost" "-O3 -ipo" "-O3 -xHost -ipo")
        fi
        ;;
    *nagfor*)
        # NAG has no `-flto` equivalent to offer here (it compiles through C and has no
        # interprocedural mode this script can drive), so the sweep is the optimisation ladder
        # alone -- `-O4` is its most aggressive. That is a weaker sweep than the other families
        # get, and it is still the only sweep that compiles the safe64 arm as a real build.
        if [ "$QUICK" = "1" ]; then
            CONFIGS=("-O4")
        else
            CONFIGS=("-O0" "-O2" "-O3" "-O4")
        fi
        ;;
    *)
        if [ "$QUICK" = "1" ]; then
            CONFIGS=("-O3 -flto")
        else
            CONFIGS=("-O0" "-O2" "-O3" "-O3 -funroll-loops" "-O3 -flto" "-Ofast -flto")
        fi
        ;;
esac

fail=0
xfailed=0
xpass=0

# ---- known exposures: arms that are EXPECTED to fail, and why -------------------------------
#
# **A check that always fails is a check nobody reads.** The forced half on gfortran builds the
# wrapping kernel, which gfortran never ships, and gfortran miscompiles it under LTO -- so before
# this list existed the default invocation exited 1 on every run, on every machine, for a reason
# that is not a defect in anything this library ships. That is not a loud warning; it is noise
# that hides a real regression, and it has already caused one: a run was reported as "2 of 12 arms
# failing" as though it were an open problem, when the script's own header said it was expected.
#
# So each known exposure is listed here with its reason, and the list is checked in BOTH
# directions. A listed arm that fails is XFAIL and does not fail the run. **A listed arm that
# PASSES is a hard error**, because it means the exposure is gone -- a fixed compiler, or a source
# change that removed the undefined behaviour -- and the next person deserves to be told that the
# check just got stronger rather than to have it silently absorbed.
#
# **This is not a licence to silence a failure by adding a line here.** An entry may be added only
# for a configuration this library does not ship, with a `feature_risks.md` entry behind it.
xfail_reason() {
    case "$FAMILY|$1" in
        'gnu|forced -O3 -flto'|'gnu|forced -Ofast -flto')
            echo "gfortran miscompiles the WRAPPING kernel under LTO (feature_risks.md Risk-101)."
            echo "        gfortran has a 128-bit kind and always ships the protected arm, so this"
            echo "        configuration exists only inside this check. ifx ships the wrapping"
            echo "        kernel and is clean at every setting including -ipo."
            echo "        If this arm ever PASSES: the exposure is gone -- confirm whether the"
            echo "        compiler was fixed or the source stopped overflowing, then delete this"
            echo "        entry and update Risk-101."
            ;;
    esac
}

# The vacuity guard below needs one kernel name per half, so two scalars are all this has to
# carry. It was an associative array once, and that made the whole script a no-op on macOS:
# `declare -A` is bash 4, macOS ships bash 3.2, and `/usr/bin/env bash` finds 3.2 there. The
# declaration failed, the first `seen_kernel[...]` assignment then died under `set -u`, and the
# script EXITED 0 having run one configuration of twelve -- with the vacuity guard, the part this
# file's own header calls more important than the assertions, never reached. Two of the three
# machines in the fleet are macOS, so that was precisely where a hand run said nothing. Keep this
# script free of bash-4 syntax; `finished`/`trap` below is the backstop for the same class of
# failure arriving by some other route.
shipped_kernel=""
forced_kernel=""
safe64_kernel=""

# A run that dies partway through must never look like a clean one. `set -e` is not usable here --
# build_and_run deliberately inspects non-zero exits -- so instead every deliberate exit happens
# after `finished=1`, and anything else is reported and forced non-zero.
finished=0
trap '[ "$finished" = "1" ] || { echo "check_random_kernels: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

build_and_run () {
    local label="$1" ; shift
    local d="$WORK/$(echo "$label" | tr -c 'A-Za-z0-9' '_')"
    mkdir -p "$d"
    local out
    # Compile FROM the scratch directory with absolute sources, rather than from the repo root with
    # a module-output flag. The flag spelling is per family -- gfortran wants -J, ifx wants -module
    # -- and getting it wrong does not fail the build: the compiler simply writes its .mod files
    # into the current directory, which here is the repository root. Those files then shadow the
    # real modules and break every later fpm build with "Reading module ... Unexpected EOF", a
    # failure that looks nothing like its cause. Running from the scratch directory makes the
    # question moot for every compiler, present and future.
    if ! out=$(cd "$d" && $FC $CPP "$@" $ABS_SRC -o drv 2>&1); then
        printf "  %-38s BUILD-FAIL\n" "$label"
        echo "$out" | head -5 | sed 's/^/        /'
        fail=1
        return
    fi
    local res rc verdict
    res=$(cd "$d" && ./drv 2>&1) ; rc=$?
    # Find the driver's verdict by its MARKER, never by position. It is not the last line when the
    # run fails: gfortran's `stop 1` prints "STOP 1" after it, and a `tail -1` therefore reported
    # "STOP 1" in place of "KERNEL=wrapping RESULT=FAIL FAILED=240" -- hiding the failure count on
    # every failing configuration, and, worse, leaving the kernel name below empty. That made
    # `--quick` on gfortran exit 2 with "the forced half produced no kernel name -- the check proves
    # nothing" on a run that had worked perfectly and found the real bug, because --quick's single
    # configuration is the failing one. The full run escaped only because its first configuration
    # succeeds and sets the name.
    verdict=$(echo "$res" | grep '^KERNEL=' | tail -1)
    [ -n "$verdict" ] || verdict=$(echo "$res" | tail -1)
    printf "  %-38s %s\n" "$label" "$verdict"
    local xr
    xr=$(xfail_reason "$label")
    if [ $rc -ne 0 ]; then
        if [ -n "$xr" ]; then
            printf "        XFAIL (expected) -- %s\n" "$xr"
            xfailed=$((xfailed + 1))
        else
            echo "$res" | grep '\[FAIL\]' | head -5 | sed 's/^/        /'
            fail=1
        fi
    elif [ -n "$xr" ]; then
        printf "        XPASS -- this arm is listed as a KNOWN FAILURE and it just passed.\n"
        printf "        %s\n" "$xr"
        xpass=1
    fi
    # Record which kernel this build actually compiled, for the vacuity guard below. Only the
    # first configuration of each half is kept: every configuration in a half compiles the same
    # kernel, so one name per half is what the guard compares. Taken from the marker line above, so
    # a FAILING build still reports which kernel it was -- which is exactly the case the guard has
    # to be able to tell apart from "the flag stopped working".
    local k
    k=$(echo "$verdict" | sed -n 's/^KERNEL=\([a-z0-9]*\).*/\1/p')
    case "$label" in
        shipped\ *) [ -z "$shipped_kernel" ] && shipped_kernel="$k" ;;
        safe64\ *) [ -z "$safe64_kernel" ]  && safe64_kernel="$k" ;;
        forced\ *)  [ -z "$forced_kernel" ]  && forced_kernel="$k" ;;
    esac
    return 0
}

echo "=== check_random_kernels: $FC ($($FC --version 2>&1 | head -1)) ==="

echo "--- as shipped (whichever kernel this compiler selects) ---"
for cfg in "${CONFIGS[@]}"; do
    build_and_run "shipped $cfg" $cfg
done

if [ "$SHIPPED_ONLY" = "1" ]; then
    echo "--- forced-kernel half skipped (--shipped-only) ---"
    # BOTH forcing mechanisms have to be cleared here, not just the -U one. The vacuity guard below
    # fires when a forcing mechanism is configured and produced no kernel name -- which is exactly
    # the state --shipped-only creates deliberately. Clearing UNDEF alone left nagfor, whose forcing
    # mechanism is the pre-expanded source rather than -U, exiting 2 with "the forced half produced
    # no kernel name -- the check proves nothing" after every shipped configuration had passed.
    UNDEF=""
    NAG_FORCED_SRC=""
elif [ -n "$UNDEF" ]; then
    echo "--- allowlist defeated ($UNDEF): the kernel this compiler does NOT ship ---"
    for cfg in "${CONFIGS[@]}"; do
        build_and_run "forced $cfg" $UNDEF $cfg
    done
elif [ -n "$NAG_FORCED_SRC" ]; then
    # Same half, reached the only way nagfor allows: the source pre-expanded with every allowlist
    # macro undefined, so the file itself already selects the wrapping kernel. Swapping ABS_SRC is
    # what build_and_run compiles, and it is restored immediately afterwards so the safe64 half
    # below (and any future arm) still sees the real source.
    echo "--- allowlist defeated (source pre-expanded by cpp): the kernel this compiler does NOT ship ---"
    saved_abs_src="$ABS_SRC"
    ABS_SRC="$(echo "$ABS_SRC" | sed "s#$REPO/src/parquet_random.f90#$NAG_FORCED_SRC#")"
    for cfg in "${CONFIGS[@]}"; do
        build_and_run "forced $cfg" $cfg
    done
    ABS_SRC="$saved_abs_src"
else
    echo "--- $FC already compiles the wrapping kernel; nothing to force ---"
fi

# ---- the third arm: PF_SAFE64, overflow-free, which only nagfor ships --------------------------
# It has to be forced here for the same reason the wrapping arm does: no compiler in this fleet's
# default configuration selects it except nagfor, so without this half its agreement with the other
# two rests on someone happening to run a NAG build -- and never on the interprocedural settings,
# which are the ones that have actually caught miscompilations here.
if [ "$SHIPPED_ONLY" = "1" ]; then
    echo "--- safe64 half skipped (--shipped-only) ---"
elif [ "$shipped_kernel" = "safe64" ]; then
    echo "--- $FC already compiles the safe64 kernel; nothing to force ---"
else
    echo "--- safe64 forced ($UNDEF -D__NAG_COMPILER_RELEASE=1): the overflow-free kernel ---"
    for cfg in "${CONFIGS[@]}"; do
        build_and_run "safe64 $cfg" $UNDEF -D__NAG_COMPILER_RELEASE=1 $cfg
    done
fi

# Every build has run; from here every exit is a deliberate verdict rather than a crash.
finished=1

# ---- vacuity guard -------------------------------------------------------------------------
# Without this the whole script is worthless the moment the -U flag stops working: it would build
# one kernel twice, pass, and say nothing. Assert the two halves really did differ.
if [ -z "$shipped_kernel" ]; then
    echo "ERROR: could not read the kernel from the driver's output -- the check proves nothing." >&2
    exit 2
fi
if [ -n "$UNDEF" ] || [ -n "$NAG_FORCED_SRC" ]; then
    if [ -z "$forced_kernel" ]; then
        echo "ERROR: the forced half produced no kernel name -- the check proves nothing." >&2
        exit 2
    fi
    if [ "$shipped_kernel" = "$forced_kernel" ]; then
        echo "ERROR: both builds compiled the '$shipped_kernel' kernel -- ${UNDEF:-the pre-expansion}" >&2
        echo "       no longer defeats the allowlist, so the second half of this check tested" >&2
        echo "       nothing. Fix it before trusting a green run here." >&2
        exit 2
    fi
    echo "--- both kernels exercised: shipped=$shipped_kernel forced=$forced_kernel ---"
else
    echo "--- single kernel exercised: $shipped_kernel ---"
fi

# Same guard for the safe64 half: a name that is not "safe64" means the -D stopped selecting that
# arm, and the half then re-tested a kernel already covered while still reporting green.
if [ "$SHIPPED_ONLY" != "1" ] && [ "$shipped_kernel" != "safe64" ]; then
    if [ -z "$safe64_kernel" ]; then
        echo "ERROR: the safe64 half produced no kernel name -- the check proves nothing." >&2
        exit 2
    fi
    if [ "$safe64_kernel" != "safe64" ]; then
        echo "ERROR: the safe64 half compiled the '$safe64_kernel' kernel --" >&2
        echo "       '-D__NAG_COMPILER_RELEASE=1' no longer selects PF_SAFE64, so that half tested" >&2
        echo "       nothing. Keep this in step with the #if fork at the top of parquet_random.f90." >&2
        exit 2
    fi
    echo "--- safe64 kernel exercised as well ---"
fi

# A nagfor run exists to exercise safe64 as nagfor's OWN build. If it compiled something else the
# fork has changed under it, and the run says nothing about the arm it was started for.
if [ "$FAMILY" = "nag" ] && [ "$shipped_kernel" != "safe64" ]; then
    echo "ERROR: nagfor compiled the '$shipped_kernel' kernel, not safe64 -- the one arm this" >&2
    echo "       compiler is here to cover was not built. Check the #if fork at the top of" >&2
    echo "       src/parquet_random.f90 against nagfor's own predefined macros." >&2
    exit 2
fi

if [ $xpass -ne 0 ]; then
    echo "check_random_kernels: an arm listed as a KNOWN FAILURE passed -- see XPASS above." >&2
    echo "       This is reported rather than absorbed: the list is what makes a real regression" >&2
    echo "       visible, and an entry that no longer describes reality quietly weakens it." >&2
    exit 1
fi
if [ $fail -ne 0 ]; then
    echo "check_random_kernels: FAILED"
    exit 1
fi
if [ $xfailed -ne 0 ]; then
    echo "check_random_kernels: all configurations behaved as expected" \
         "($xfailed known exposure(s) still failing, listed above)"
else
    echo "check_random_kernels: all configurations passed"
fi
