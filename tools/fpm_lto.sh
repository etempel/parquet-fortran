#!/usr/bin/env bash
#
# Opt-in link-time optimisation for `fpm`, as a shell function.
#
# ============================================================================
#  HOW TO ACTIVATE
# ============================================================================
#
# This file is SOURCED, not executed -- it defines a shell function named `fpm`
# that wraps the real one, and a function cannot be installed into your shell by
# running a script in a child process. Add two lines to whichever startup file
# your shell already uses (`~/.zprofile` or `~/.zshrc` for zsh, `~/.bash_profile`
# or `~/.bashrc` for bash):
#
#     source /path/to/parquet-fortran/tools/fpm_lto.sh
#     export PF_LTO=1        # opt in; omit this line and the wrapper is inert
#
# Open a new shell, or `source` that startup file, and it is live. Verify with:
#
#     tools/fpm_lto.sh       # executing it prints what it WOULD select here
#
# Sourcing this file WITHOUT setting PF_LTO=1 changes nothing at all: the wrapper
# hands straight through to the real `fpm`. That is deliberate -- installing it
# is a separate decision from turning it on, so it can live in a startup file
# permanently and be enabled per-project or per-session with `PF_LTO=1`.
#
# To turn it off again for one command:  PF_LTO=0 fpm build --profile release
#
# ============================================================================
#  WHAT IT DOES, AND WHY IT IS NOT JUST A FLAG
# ============================================================================
#
# `fpm` has no LTO profile, so LTO has to arrive through FPM_FFLAGS/FPM_CXXFLAGS/
# FPM_LDFLAGS. Exporting those permanently would apply LTO to EVERY build
# including `--profile debug`, which is the opposite of what a debug build wants
# -- slow to link, and inlined across translation units exactly where a
# `-fcheck=bounds` backtrace needs things left alone. So this wrapper adds the
# flags only when it sees `--profile release`, and appends to whatever those
# variables already hold (on most machines they carry Arrow's include and link
# paths, and replacing them produces "fatal error: 'arrow/api.h' file not found",
# which reads like a missing dependency rather than a flag mistake).
#
# Two companion tools matter as much as the flag, and getting either wrong is
# silent -- which is why this is a script rather than a one-line alias:
#
#   * A PLUGIN-CAPABLE ARCHIVER. Under LTO an object holds intermediate
#     representation; an archiver that cannot read it indexes only the
#     machine-code half, so the build SUCCEEDS, passes its tests, and does no
#     interprocedural optimisation whatsoever. No error, no warning. GCC ships
#     `gcc-ar`, LLVM/Intel ship `llvm-ar`; fpm archives with plain `ar` unless
#     FPM_AR says otherwise, and on macOS plain `ar` is Apple cctools `ar`, which
#     has no plugin at all. This wrapper REFUSES to build rather than hand back a
#     measurement of nothing.
#
#   * ON ifx, THE LINKER. `-ipo` emits LLVM bitcode the system `ld` generally
#     cannot read, and the link dies with thousands of undefined
#     <module>_mp_<proc>_ references -- which reads as a defect in this library
#     and is not. oneAPI ships a matching `ld.lld`, but it sits in
#     <oneapi>/compiler/<ver>/bin/COMPILER/, one directory below what the usual
#     environment scripts put on PATH. This wrapper looks there and puts it on
#     PATH itself, and refuses to build if it cannot find it.
#
# ============================================================================
#  WHAT TO EXPECT FROM IT
# ============================================================================
#
# Measured on three machines: LTO is SAFE everywhere it was tried (full suite and
# every error scenario pass under it, on gfortran/arm64, gfortran and ifx on
# x86-64), and worth very little. The only clear gain observed was ~2x on
# temporal per-element validity dispatch, on same-family x86-64 toolchains.
#
# ON macOS THE GAIN MAY BE STRUCTURALLY ZERO, however carefully this is set up.
# The normal macOS build is mixed-family (gfortran + Apple clang), so the Fortran
# objects carry GCC GIMPLE while the C++ object is LLVM bitcode -- two IRs no
# linker can optimise across. Re-measured on arm64 with `gcc-ar` genuinely in
# effect and a 3% noise floor: no change on any item.
#
# So this is an EXTRA a builder may choose to enable, never something this
# library's own performance work is allowed to depend on. See CONTRIBUTING.md's
# "Building with link-time optimisation" for the fuller writeup.
#
# ============================================================================

# ---- executed rather than sourced: explain, and dry-run the selection --------
# Detected per shell: bash exposes BASH_SOURCE, zsh exposes ZSH_EVAL_CONTEXT
# (which contains ":file" only while sourcing). Anything else falls through to
# "sourced", since a false negative here costs only a missing hint.
_pf_executed=0
if [ -n "${BASH_SOURCE:-}" ]; then
    [ "${BASH_SOURCE[0]}" = "$0" ] && _pf_executed=1
elif [ -n "${ZSH_VERSION:-}" ]; then
    case "${ZSH_EVAL_CONTEXT:-}" in *:file*) ;; *) _pf_executed=1 ;; esac
fi

# ---- the wrapper ------------------------------------------------------------

# First of its ARGUMENTS that is on PATH. The candidates are passed as separate
# arguments rather than as one space-separated string on purpose: zsh does not
# word-split an unquoted parameter expansion the way bash does, so
# `for c in $list` sees a single word there and every lookup fails -- reported as
# "no archiver found" on a machine that has one. Found by testing in both shells.
pf_first_on_path() {
    local c
    for c in "$@"; do
        if command -v "$c" >/dev/null 2>&1; then printf '%s\n' "$c"; return 0; fi
    done
    return 1
}

# Resolves the LTO flag, archiver and (Intel only) linker for a Fortran compiler.
# Sets PF_LTO_FLAG / PF_LTO_AR / PF_LTO_LD / PF_LTO_ERR; returns 1 with PF_LTO_ERR
# set when the configuration cannot be assembled. Kept separate from the wrapper
# so that executing this file can show what it would pick without building.
pf_lto_resolve() {
    local fc="${1:-${FPM_FC:-gfortran}}" maj artool fcbin
    PF_LTO_FLAG=""; PF_LTO_AR=""; PF_LTO_LD=""; PF_LTO_ERR=""
    maj=$("$fc" -dumpversion 2>/dev/null | cut -d. -f1)
    case "$("$fc" --version 2>&1 | head -1)" in
        *ifx*|*Intel*|*IFX*) PF_LTO_FLAG="-ipo";  artool="llvm-ar" ;;
        *flang*|*clang*)     PF_LTO_FLAG="-flto"; artool="llvm-ar" ;;
        *)                   PF_LTO_FLAG="-flto"; artool="gcc-ar"  ;;
    esac
    PF_LTO_AR=$(pf_first_on_path "$artool-mp-$maj" "$artool-$maj" "$artool") || PF_LTO_AR=""
    if [ -z "$PF_LTO_AR" ]; then
        PF_LTO_ERR="no plugin-capable archiver found (tried $artool-mp-$maj, $artool-$maj, $artool).
     Plain 'ar' archives IR the linker will not use, so the build would SUCCEED
     having done no interprocedural optimisation at all, with no warning."
        return 1
    fi
    if [ "$PF_LTO_FLAG" = "-ipo" ]; then
        if command -v ld.lld >/dev/null 2>&1; then
            PF_LTO_LD="-fuse-ld=lld"
        else
            fcbin=$(dirname "$(command -v "$fc" 2>/dev/null || echo /nonexistent)")
            if [ -x "$fcbin/compiler/ld.lld" ]; then
                PATH="$fcbin/compiler:$PATH"; export PATH; PF_LTO_LD="-fuse-ld=lld"
            fi
        fi
        if [ -z "$PF_LTO_LD" ]; then
            PF_LTO_ERR="'ld.lld' not found on PATH or beside the compiler.
     The link would fail with thousands of undefined <module>_mp_<proc>_
     references, which looks like a defect in this library and is not.
     Add <oneapi>/compiler/<ver>/bin/compiler to PATH and retry."
            return 1
        fi
    fi
    return 0
}

fpm() {
    local a prev="" use=0
    if [ "${PF_LTO:-0}" != "1" ]; then command fpm "$@"; return; fi
    for a in "$@"; do
        if [ "$prev" = "--profile" ] && [ "$a" = "release" ]; then use=1; fi
        if [ "$a" = "--profile=release" ]; then use=1; fi
        prev="$a"
    done
    if [ "$use" -eq 0 ]; then command fpm "$@"; return; fi
    if ! pf_lto_resolve; then
        echo "fpm: LTO requested but $PF_LTO_ERR" >&2
        return 1
    fi
    # `env` runs the real binary, so the function cannot recurse into itself.
    env FPM_AR="$PF_LTO_AR" \
        FPM_FFLAGS="${FPM_FFLAGS:-} $PF_LTO_FLAG" \
        FPM_CXXFLAGS="${FPM_CXXFLAGS:-} $PF_LTO_FLAG" \
        FPM_LDFLAGS="${FPM_LDFLAGS:-} $PF_LTO_FLAG $PF_LTO_LD" fpm "$@"
}

# ---- when executed: print how to activate, and what it would select here ----
if [ "$_pf_executed" = "1" ]; then
    self="$0"; case "$self" in /*) ;; *) self="$PWD/${self#./}" ;; esac
    echo "tools/fpm_lto.sh must be SOURCED, not executed -- it defines a shell function."
    echo
    echo "Add these two lines to your shell startup file (~/.zprofile, ~/.bashrc, ...):"
    echo
    echo "    source $self"
    echo "    export PF_LTO=1        # opt in; omit and the wrapper is inert"
    echo
    echo "Dry run for the current toolchain (FPM_FC=${FPM_FC:-gfortran}):"
    if pf_lto_resolve; then
        echo "    flag     : $PF_LTO_FLAG"
        echo "    FPM_AR   : $PF_LTO_AR"
        echo "    linker   : ${PF_LTO_LD:-(compiler default)}"
        echo "  -> 'fpm <cmd> --profile release' would build with LTO; every other"
        echo "     invocation, including --profile debug, would be untouched."
    else
        echo "    flag     : ${PF_LTO_FLAG:-?}"
        echo "  -> would REFUSE to build: $PF_LTO_ERR"
    fi
    exit 0
fi
unset _pf_executed
