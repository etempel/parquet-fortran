#!/usr/bin/env bash
#
# Assert the frozen `-log(u)` transform gives the SAME BITS under every IEEE-conforming build.
#
# Why this exists, and why it is not an `fpm test` arm:
#
#   * `pf_weighted_permutation` orders items by `-log(u)/w`. If two builds disagree about one key
#     by a single ulp, they disagree about the permutation whenever that key crosses its
#     neighbour. Nothing fails, no test goes red, and two machines simply return different items.
#     Only a cross-BUILD comparison can see it, and a unit test runs in one build by construction.
#   * The threat is real and was found by this check rather than predicted. An earlier design note
#     concluded FMA contraction was harmless, on the strength of four builds -- none of which
#     enabled FMA on gfortran. `gfortran -O3 -march=native` changes the fingerprint of an
#     unbarriered Horner chain, and `-march=native` is an entirely ordinary thing to build with.
#     `exp_key` rounds every product through a `volatile` to prevent the fusion; this script is
#     what keeps that true when a compiler version moves.
#   * It has now caught the same class a SECOND time, which is the reason to keep running it on
#     every new compiler. The barrier was a `noinline` identity helper for a while, so that
#     `exp_key` could stay `pure`; flang 22.1.8 ignores both directive spellings, inlines it and
#     fuses, and `flang -O3 -march=native` on machine C moved the fingerprint. Four of the 13824
#     swept inputs differed, by 1 ulp each. The barrier is a `volatile` local again -- standard,
#     and not advice a compiler may decline -- and `exp_key` is no longer `pure`.
#     Note the runtime guard `exp_key_contract_ok` did NOT see that divergence: it samples 32
#     inputs and none of the four were among them. This script is the only thing that catches it.
#   * A THIRD instance, and the one that makes the pairing below load-bearing. `flang -O3
#     -ffast-math` on machine A (arm64) moved the fingerprint to 7108262605301466839, on 148 of
#     13824 inputs by 1 ulp each: under `reassoc` the final `g * (t + 1.0)` distributes to
#     `g*t + g` -- free, because the last Horner coefficient is exactly 1.0 -- and that form is a
#     single FMA. The barriers on the two products either side did not help, because the
#     unbarriered add BETWEEN them was the target. Fixed by barriering it; see `exp_key`.
#     **It is ARCHITECTURE-GATED and that is why one machine can pass while another fails on the
#     same compiler and flags.** The rewrite fires only where the result becomes one instruction:
#     the same `-O3 -ffast-math` emits the FMA on aarch64, where FMA is baseline, and on x86-64
#     only under an FMA-bearing `-march=`. Machine C reported clean purely because its fast-math
#     arms were baseline x86-64, which has no FMA to contract into. **That is why every fast-math
#     arm below is ALSO swept with `-march=native`** -- without it the whole class is invisible on
#     x86, which is exactly how it reached a shipped build.
#
# FAST-MATH IS NOW IN SCOPE AND IS SWEPT LIKE EVERYTHING ELSE. Both `-Ofast`/`-ffast-math` and
# ifx's default `-fp-model=fast` used to differ, and this file used to record them as unfixable on
# the grounds that fast-math may compute `(m-1)/(m+1)` by reciprocal approximation. That diagnosis
# was WRONG, and it is worth knowing how it was falsified, because the same mistake is easy to
# repeat: on ifx at `-O2` with the default model, `-prec-div` does not fix it and `-no-fma` does
# not fix it, but `-assume protect_parens` does. The transformation was REASSOCIATION, not
# division and not FMA -- and it had one place to bite, `e = big + (small - logm)`, where the
# parentheses were the only thing holding a large term and a tiny correction apart. Barriering
# that subtraction fixed every configuration below on both compilers, `-Ofast` included, and
# repaired gfortran `-Ofast`, which had been silently wrong.
#
# So a FAILURE in any arm below is a real finding now, not a known exposure. There is no
# expected-to-differ list any more; if one is ever needed again, it must name the transformation
# that causes it and the flag that proves the diagnosis, not merely the flag that fails.
#
# Usage:  tools/check_exp_key.sh
#   FC=<compiler>   Fortran compiler to use (default: gfortran).
#
# Exits 0 only if every IEEE-conforming configuration reproduces EXPECTED_FP below. A new expected
# value may only be adopted deliberately: it is the frozen contract, and changing it changes every
# weighted permutation this library has ever produced.
# ---------------------------------------------------------------------------------------------
# Is the same idea for a different contract: it standalone-compiles
# `src/parquet_expkey.f90` -- the frozen `-log(u)` transform behind `pf_weighted_permutation` --
# across seven gfortran settings, four ifx ones and six flang ones, and requires every build to
# reproduce one fingerprint. It exists because a weighted permutation is decided by the order of
# `-log(u)/w`, so a single differing key changes which items are drawn, silently, and only a cross-
# BUILD comparison can see that; a unit test runs in one build by construction. It is not
# hypothetical: an earlier design note concluded FMA contraction was harmless on the strength of
# four builds that never enabled FMA, and `gfortran -O3 -march=native` duly changed the answer. It
# has since caught the same class a second time: the rounding barrier had been a `noinline`
# directive pair rather than a `volatile` local, and flang 22.1.8 -- which ignores both spellings --
# inlined it and fused, moving the fingerprint under `-march=native`. Run it under any compiler
# newly added to the fleet before trusting that build. That is also why `parquet_expkey` is a leaf
# module depending on `iso_fortran_env` alone -- the same property `parquet_random` has and for the
# same reason, so that neither check can be disabled by an import added somewhere else. Anything
# needing more than `iso_fortran_env` belongs in `parquet_sampling`, which is where the weighted
# draw lives and which is deliberately compiled by neither script. Nothing is excluded: fast-math is
# swept like everything else, including gfortran `-Ofast`/`-ffast-math` and ifx's own no-flag
# defaults, so a failure in any arm is a real finding rather than a known exposure. That was not
# always so -- both were once recorded as unfixable, on the diagnosis that fast-math computes
# `(m-1)/(m+1)` by reciprocal approximation. The diagnosis was wrong: `-prec-div` does not fix it,
# `-no-fma` does not fix it, and `-assume protect_parens` does, so the transformation was
# reassociation of `e = big + (small - logm)` -- a large term plus a tiny correction, held apart
# only by parentheses that fast-math may ignore. Barriering that subtraction brought every
# configuration into line and repaired gfortran `-Ofast`, which had been silently wrong.
# `pf_weighted_permutation` still re-derives the transform at run time, now as a backstop for a
# compiler or flag combination nobody has swept rather than for fast-math specifically.
#
#     tools/check_exp_key.sh                        # gfortran, nine settings on x86 (eight elsewhere)
#     FC=ifx tools/check_exp_key.sh                 # ifx, seven settings incl. its own defaults and -Ofast
#     FC=flang tools/check_exp_key.sh               # flang, eight settings (no -mfma; its driver has none)
# ---------------------------------------------------------------------------------------------
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

# The contract. Reproduced by gfortran 15.2.1 at -O0/-O2/-O3/-march=native/-mfma/-flto (machines
# B and C, both x86-64), by gfortran 15.2.0 at -O0/-O2/-O3/-march=native/-flto/-ffp-contract=fast
# (machine A, arm64 -- no -mfma arm there; see the note on the generic branch below for why its
# absence costs no coverage), by ifx 2026.1.1 at -fp-model=precise (machine B), by flang
# 22.1.8 at -O0/-O2/-funroll-loops/-march=native/-flto/-ffp-contract=fast (machines A and C), and
# by nagfor 7.2 at -O0/-O2/-O3/-O4/-O4 -Ounsafe/-float-store and with the backend pushed hard
# (-Wc,-ffast-math, -Wc,-march=native, and both together) on machine C.
EXPECTED_FP="-9123008136727752159"

# Follow FPM_FC when FC is unset, as tools/check_random_kernels.sh does. Without this, running
# from an activated ifx environment silently selects the bare `gfortran` on PATH -- which on the
# reference machine is the SYSTEM 11.5.0, below this project's floor, and the run then reports a
# green result for a compiler nothing ships.
FC="${FC:-${FPM_FC:-gfortran}}"

# Refuse a below-floor gfortran outright. README.md's prerequisites put the minimum at 13; a run
# with 11 reports findings about a compiler this project does not support.
case "$FC" in
    *gfortran*|*gcc*)
        ver="$("$FC" -dumpfullversion 2>/dev/null || "$FC" -dumpversion 2>/dev/null)"
        major="${ver%%.*}"
        if [ -n "$major" ] && [ "$major" -lt 13 ] 2>/dev/null; then
            echo "check_exp_key.sh: $FC is $ver; this project requires gfortran >= 13." >&2
            echo "  A run with this compiler proves nothing. Activate the intended toolchain, or" >&2
            echo "  set FC/FPM_FC explicitly to the compiler you meant." >&2
            exit 2
        fi
        ;;
esac
if ! command -v "$FC" >/dev/null 2>&1; then
    echo "check_exp_key.sh: compiler '$FC' not found on PATH" >&2
    exit 2
fi

SRC="src/parquet_expkey.f90 tools/check_exp_key.f90"
# ABSOLUTE paths, because each build runs with the WORK directory as its cwd. gfortran searches the
# CURRENT DIRECTORY for `.mod` files, so a stray parquet_expkey.mod left in the repository root --
# by any hand-run `gfortran src/parquet_expkey.f90` that forgot `-J` -- shadows this script's own
# `-J$WORK` output. Built by a different gfortran, it makes every configuration fail with "created
# by a different version of GNU Fortran", which reads as a defect in the library and is not one.
# A global `*.mod` gitignore hides such a file from `git status`, so it can sit there unnoticed.
ABS_SRC=""
for f in $SRC; do ABS_SRC="$ABS_SRC $ROOT_DIR/$f"; done
WORK="$(mktemp -d)"
# `finished` guards against the script stopping early and still exiting 0 -- a check that dies
# quietly is worse than no check, because it also removes the doubt that would prompt a look.
finished=0
trap '[ "$finished" = "1" ] || { echo "check_exp_key.sh: TERMINATED EARLY -- this run proves nothing" >&2; rm -rf "$WORK"; exit 2; }' EXIT

SKIP_NOTE=""
case "$($FC --version 2>&1 | head -1)" in
    *ifx*|*ifort*) PP="-fpp"; MD="-module $WORK"
                   # The four `-fp-model=precise` arms are what fpm's release profile builds. The
                   # last three are ifx's OWN defaults -- no fp-model flag at all, which is what a
                   # bare `fpm build`/`fpm test` gives it, plus `-Ofast`. Those are the arms that
                   # exposed the reassociation described above, so they are swept, not excused.
                   CONFIGS=("-O0 -fp-model=precise" "-O2 -fp-model=precise" \
                            "-O3 -xHost -fp-model=precise" "-O3 -fp-model=precise -ipo" \
                            "-O2" "-O3 -xHost" "-Ofast") ;;
    # flang's driver has no `-mfma` at all (`unknown argument`). gfortran has it only on x86 --
    # it is one of GCC's x86 options, not a generic one -- so the generic branch below gates it on
    # the TARGET rather than assuming every gfortran takes it. Dropping it costs no coverage
    # either way: `-march=native` already implies FMA wherever the hardware has it, and is exactly
    # the configuration that exposed the lost barrier described above.
    *flang*)       PP="-cpp"; MD="-J$WORK"
                   # The last two pair fast-math with an FMA-bearing target on purpose: on
                   # baseline x86-64 the distribution described above cannot become one
                   # instruction, so it never fires and the arm proves nothing there.
                   CONFIGS=("-O0" "-O2" "-O3 -funroll-loops" "-O3 -march=native" \
                            "-O3 -flto" "-O3 -ffp-contract=fast" \
                            "-O3 -ffast-math" "-Ofast" \
                            "-O3 -ffast-math -march=native" "-Ofast -march=native") ;;
    # nagfor generates C and hands it to a C compiler, so its optimiser is only half the story:
    # FMA contraction and reassociation happen in the BACKEND, and are reached with `-Wc,`.
    # `-Ounsafe` is NAG's own unsafe-optimisation switch, the nearest thing it has to fast-math.
    #
    # The `-Wc,-march=native` pairing is not optional here, for a reason specific to this compiler:
    # nagfor's default backend line already carries **`-march=nocona`** (confirmed with `-dryrun`),
    # a pre-FMA x86-64 target. So on this compiler the whole FMA-contraction class is invisible
    # WITHOUT the pairing -- more so than on the families above, where the default target at least
    # follows the host. `-Wc,` options land after NAG's own on the backend command line, so they
    # win. `-mdir` is NAG's module-output flag; neither `-J` nor `-module` exists here.
    #
    # `-ieee=nonstd` is deliberately absent: it selects a non-conforming float mode, and the
    # contract this script freezes is over IEEE-CONFORMING builds. `-ieee=full` changes only
    # trapping, so it would add an arm that cannot differ.
    #
    # Which of these arms actually have TEETH was measured, not assumed, by defeating `ek_rnd`'s
    # `volatile` barrier and re-running: `-O0`, `-O2`, `-O3`, `-O4`, `-float-store` and the bare
    # `-Wc,-march=native` all still reproduced the frozen value, i.e. they are blind to the class
    # this script exists for. `-O4 -Ounsafe` and the `-Wc,-ffast-math` arms caught it, and the
    # fast-math-plus-native pairing caught it with a DIFFERENT wrong fingerprint again -- two
    # distinct transformations, so both spellings earn their place. Do not prune this list down to
    # the optimisation ladder: on this compiler the ladder alone proves almost nothing.
    *NAG*)         PP="-fpp"; MD="-mdir $WORK"
                   CONFIGS=("-O0" "-O2" "-O3" "-O4" "-O4 -Ounsafe" "-float-store" \
                            "-O3 -Wc,-ffast-math" "-O4 -Ounsafe -Wc,-ffast-math")
                   # Same architecture gate as the `-mfma` arm below, and the same reasoning: on a
                   # target where FMA is baseline every arm above is already an FMA-capable build,
                   # so pairing buys nothing; on x86-64 it is the only way the class can fire at
                   # all, because of the `-march=nocona` default noted above.
                   case "$(uname -m)" in
                       x86_64|i?86)
                           CONFIGS+=("-O3 -Wc,-march=native" \
                                     "-O3 -Wc,-ffast-math -Wc,-march=native" \
                                     "-O4 -Ounsafe -Wc,-ffast-math -Wc,-march=native") ;;
                       *) SKIP_NOTE="  [skip] the -Wc,-march=native arms -- x86-only pairing; on $(uname -m) FMA is baseline, so every arm above is already FMA-capable" ;;
                   esac ;;
    *)             PP="-cpp"; MD="-J$WORK"
                   # As on flang, the fast-math arms are swept both bare and with an FMA-bearing
                   # target -- the rewrite that needs the pairing is a property of the TARGET, not
                   # of the compiler.
                   CONFIGS=("-O0" "-O2" "-O3 -funroll-loops" "-O3 -march=native" \
                            "-O3 -flto" "-O3 -ffp-contract=fast" \
                            "-O3 -ffast-math" "-Ofast" \
                            "-O3 -ffast-math -march=native" "-Ofast -march=native")
                   # `-mfma` is an x86-only GCC option, so this arm is gated on the target rather
                   # than on the compiler: the aarch64 backend has no such flag and the driver
                   # rejects it outright, which reports as a [BUILD FAIL] and reads like the
                   # frozen transform having moved when it is nothing of the kind. Skipping it
                   # there costs no coverage -- FMA is baseline ARMv8-A, so plain `-O2` already
                   # emits `fmadd` and EVERY arm above is an FMA-capable build, making arm64 a
                   # stricter test of the rounding barrier than the x86 `-mfma` arm it stands in
                   # for. The skip is announced below: a probe that did not run must never look
                   # like one that passed.
                   case "$($FC -dumpmachine 2>/dev/null)" in
                       i?86-*|x86_64-*) CONFIGS+=("-O3 -mfma") ;;
                       *) SKIP_NOTE="  [skip] -O3 -mfma -- x86-only flag, unavailable on target $($FC -dumpmachine 2>/dev/null)" ;;
                   esac ;;
esac

echo "check_exp_key.sh: $FC -- $($FC --version 2>&1 | head -1)"
echo "expected fingerprint: $EXPECTED_FP"
echo
if [ -n "$SKIP_NOTE" ]; then echo "$SKIP_NOTE"; fi

fails=0
ran=0
for cfg in "${CONFIGS[@]}"; do
    rm -f "$WORK"/*.mod
    if ! (cd "$WORK" && $FC $cfg $PP $MD -o "$WORK/ek" $ABS_SRC) >"$WORK/log" 2>&1; then
        echo "  [BUILD FAIL] $cfg"
        sed 's/^/      /' "$WORK/log" | head -5
        fails=$((fails + 1))
        continue
    fi
    got="$("$WORK/ek" | awk '/EXPKEY_FINGERPRINT/ {print $2}')"
    ran=$((ran + 1))
    if [ "$got" = "$EXPECTED_FP" ]; then
        printf "  [ok]   %-28s %s\n" "$cfg" "$got"
    else
        printf "  [FAIL] %-28s %s\n" "$cfg" "$got"
        fails=$((fails + 1))
    fi
done

echo
# A run that built nothing must not pass. Without this the script reports green when the compiler
# cannot build the module at all, which is exactly when someone most needs to be told.
if [ "$ran" -eq 0 ]; then
    echo "check_exp_key.sh: NO configuration built -- this run proves nothing" >&2
    rm -rf "$WORK"
    finished=1
    exit 2
fi

rm -rf "$WORK"
finished=1
if [ "$fails" -eq 0 ]; then
    echo "check_exp_key.sh: all $ran configuration(s) reproduce the frozen transform."
    exit 0
fi
echo "check_exp_key.sh: $fails configuration(s) FAILED -- the frozen transform is not frozen." >&2
exit 1
