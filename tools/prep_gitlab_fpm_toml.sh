#!/usr/bin/env bash
#
# Make fpm.toml parseable by the RELEASED fpm that GitLab CI installs, without changing anything
# about how this project is developed locally.
#
# Why this exists:
#
#   `fpm.toml` uses `[features]` and the feature-list form of `[profiles]` (`nag = ["nagfor"]`),
#   which are fpm 0.13.0-alpha constructs -- the development build this project is maintained
#   with. CI installs fpm with a bare `pipx install fpm`, and the newest RELEASE on PyPI is
#   0.12.0, which does not understand either table. fpm rejects unknown manifest keys outright, so
#   the pipeline dies before compiling anything:
#
#       <ERROR> *cmd_run* Package error: Key features is not allowed in package file
#
#   Both tables have to go, and that is measured rather than assumed: with `[features]` removed
#   but `[profiles]` left in place, fpm 0.12.0 does not report an error at all -- it SEGFAULTS.
#   Trading a clear message for a crash would be the worst of the three outcomes, so the strip
#   list covers both, and the verification below fails if either survives.
#
# Deliberately scoped to CI. The local workflow keeps `--profile nag`/`nagdeb`/`nagundef`, which
# are what make a NAG build usable at all (see fpm.toml's own `[features]` comments), and `main`
# keeps a manifest that says what this project actually builds with. Only the ephemeral CI
# checkout is rewritten.
#
# Usage:
#   tools/prep_gitlab_fpm_toml.sh              strip, keeping the original in fpm_original.toml
#   tools/prep_gitlab_fpm_toml.sh --restore    put fpm_original.toml back and delete it
#
# Idempotent: a second run with nothing left to strip is a no-op, and an existing
# fpm_original.toml is NEVER overwritten -- otherwise a re-run would back up the already-stripped
# file over the only copy of the real one.
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR" || exit 2

MANIFEST="fpm.toml"
BACKUP="fpm_original.toml"
# Top-level tables the released fpm cannot parse. Each is removed from its header to the line
# before the next top-level table (or end of file).
STRIP_TABLES="features profiles"

# A run that dies partway through must not look like a clean one: this script REWRITES a tracked
# file, so a half-finished strip that still exits 0 would hand fpm a manifest nobody has checked.
finished=0
trap '[ "$finished" = "1" ] || { echo "prep_gitlab_fpm_toml.sh: TERMINATED EARLY -- fpm.toml may be half-rewritten; restore it with --restore or git checkout" >&2; exit 2; }' EXIT

if [ "${1:-}" = "--restore" ]; then
    if [ ! -f "$BACKUP" ]; then
        echo "prep_gitlab_fpm_toml.sh: no $BACKUP to restore from" >&2
        finished=1
        exit 2
    fi
    cp "$BACKUP" "$MANIFEST" || exit 2
    rm -f "$BACKUP"
    echo "prep_gitlab_fpm_toml.sh: restored $MANIFEST from $BACKUP"
    finished=1
    exit 0
elif [ "$#" -gt 0 ]; then
    echo "prep_gitlab_fpm_toml.sh: unknown argument '$1' (expected none, or --restore)" >&2
    finished=1
    exit 2
fi

if [ ! -f "$MANIFEST" ]; then
    echo "prep_gitlab_fpm_toml.sh: $MANIFEST not found (run this from anywhere in the repository)" >&2
    finished=1
    exit 2
fi

# What is actually present right now. A table listed here but absent is fine -- a future fpm
# release may make one of them legal, and this script must not fail because there is less to do.
present=""
for t in $STRIP_TABLES; do
    n=$(grep -c "^\[$t\]$" "$MANIFEST")
    if [ "$n" -gt 1 ]; then
        echo "prep_gitlab_fpm_toml.sh: [$t] appears $n times in $MANIFEST; refusing to guess which" >&2
        echo "  block to remove. Fix the manifest, or extend this script deliberately." >&2
        finished=1
        exit 2
    fi
    [ "$n" -eq 1 ] && present="$present $t"
done

if [ -z "$present" ]; then
    echo "prep_gitlab_fpm_toml.sh: nothing to strip ($MANIFEST has none of: $STRIP_TABLES)"
    finished=1
    exit 0
fi

if [ -f "$BACKUP" ]; then
    echo "prep_gitlab_fpm_toml.sh: $BACKUP already exists and $MANIFEST still carries$present." >&2
    echo "  Refusing to overwrite the backup -- that would lose the only copy of the original." >&2
    echo "  Run --restore first, or delete $BACKUP if you know it is stale." >&2
    finished=1
    exit 2
fi

cp "$MANIFEST" "$BACKUP" || exit 2

for t in $present; do
    awk -v hdr="[$t]" '
        BEGIN { skip = 0 }
        {
            if ($0 == hdr)          { skip = 1; next }   # the table header itself
            if (skip && /^\[/)      { skip = 0 }         # the next top-level table ends the block
            if (!skip) print
        }
    ' "$MANIFEST" > "$MANIFEST.tmp" || exit 2
    mv "$MANIFEST.tmp" "$MANIFEST" || exit 2
done

# ---- verification -------------------------------------------------------------------------
# Two things must hold, and neither is obvious from a successful awk run: the tables really are
# gone, and NOTHING ELSE changed. The second is what stops a mis-anchored block boundary from
# quietly eating a neighbouring table -- `diff` must show deletions and no additions at all.
for t in $present; do
    if grep -q "^\[$t\]$" "$MANIFEST"; then
        echo "prep_gitlab_fpm_toml.sh: [$t] survived the strip -- refusing to report success" >&2
        finished=1
        exit 2
    fi
done
added=$(diff "$BACKUP" "$MANIFEST" | grep -c '^>')
if [ "$added" -ne 0 ]; then
    echo "prep_gitlab_fpm_toml.sh: the strip ADDED or CHANGED $added line(s), which it must never" >&2
    echo "  do. $MANIFEST is suspect; restore it with --restore." >&2
    finished=1
    exit 2
fi
removed=$(diff "$BACKUP" "$MANIFEST" | grep -c '^<')

echo "prep_gitlab_fpm_toml.sh: removed$present from $MANIFEST ($removed line(s)); original kept as $BACKUP"
finished=1
exit 0
