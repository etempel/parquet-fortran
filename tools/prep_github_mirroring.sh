#!/usr/bin/env bash
# Rewrites README.md/doc/pages/*.md between this project's two remotes --
# gitlab.4most.eu (canonical: origin, where these files are normally
# committed and browsed) and github.com (a manually-synced, read-only
# mirror -- see CONTRIBUTING.md's "Mirroring to GitHub"). Two independent
# rewrites:
#
# 1. Cross-repo-file links. README.md/doc/pages/*.md link to a handful of
#    files FORD doesn't copy into its generated docs (LICENSE,
#    CONTRIBUTING.md, CHANGELOG.md, schemas/*.maml) using an absolute blob
#    URL, since a bare relative link doesn't resolve once FORD embeds a
#    page's content into its own generated HTML. The committed baseline
#    always targets gitlab.4most.eu, so these links are correct both
#    browsing the repository directly on GitLab and in the FORD docs GitLab
#    publishes; this retargets them at github.com so they're correct on the
#    mirror too.
# 2. README.md's badges. The CI-results/test-coverage/API-documentation
#    badges report on gitlab.4most.eu-only services (GitLab CI, GitLab's
#    docserver) that don't exist for the GitHub mirror, so they're swapped
#    for a single GitHub Pages documentation badge instead.
#
# Right before mirroring the repository to GitHub, this script applies both
# rewrites, so the commit that lands there is correct -- both for browsing
# the mirror directly and for its own separately published FORD docs
# (GitHub Pages).
#
# Normally invoked via tools/mirror_to_github.sh, which also handles the git
# side (a disposable branch, so main itself never carries GitHub-targeted
# content even transiently -- there is no need to run --reverse as part of
# that flow). Run directly with --reverse only if you need to restore a
# working tree that was left in the GitHub-targeted state some other way.
#
# Usage:
#   tools/prep_github_mirroring.sh            # gitlab -> github
#   tools/prep_github_mirroring.sh --reverse   # github -> gitlab (restore)
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

GITLAB_PREFIX="https://gitlab.4most.eu/etempel/parquet-fortran/-/blob/main/"
GITHUB_PREFIX="https://github.com/etempel/parquet-fortran/blob/main/"

GITLAB_BADGES='[![CI results](https://gitlab.4most.eu/etempel/parquet-fortran/badges/main/pipeline.svg)](https://gitlab.4most.eu/etempel/parquet-fortran)
[![Test coverage](https://gitlab.4most.eu/etempel/parquet-fortran/badges/main/coverage.svg)](https://gitlab.4most.eu/etempel/parquet-fortran)
[![API documentation](https://gitlab.4most.eu/ole/docserver/-/raw/master/API-documentation-blue.svg)](https://www.4most.eu/readthedocs/etempel/parquet-fortran/main)'
GITHUB_BADGE='[![Documentation](https://img.shields.io/badge/docs-GitHub%20Pages-blue.svg)](https://etempel.github.io/parquet-fortran/index.html)'

FROM_PREFIX="$GITLAB_PREFIX"
TO_PREFIX="$GITHUB_PREFIX"
FROM_BADGES="$GITLAB_BADGES"
TO_BADGES="$GITHUB_BADGE"
if [ "${1:-}" = "--reverse" ]; then
    FROM_PREFIX="$GITHUB_PREFIX"
    TO_PREFIX="$GITLAB_PREFIX"
    FROM_BADGES="$GITHUB_BADGE"
    TO_BADGES="$GITLAB_BADGES"
elif [ $# -gt 0 ]; then
    echo "tools/prep_github_mirroring.sh: unknown argument: $1 (only --reverse is supported)" >&2
    exit 1
fi

count=0
while IFS= read -r file; do
    content="$(<"$file")"
    printf '%s\n' "${content//$FROM_PREFIX/$TO_PREFIX}" > "$file"
    count=$((count + 1))
# find rather than a doc/pages/*.md glob: the page tree is nested
# (doc/pages/<group>/<name>.md), and a flat glob would silently skip every
# nested page that carries an absolute blob URL. find handles the recursion
# portably (BSD and GNU alike), and README.md as a path operand matches
# -name '*.md' itself.
done < <(find README.md doc/pages -name '*.md' -exec grep -lF -- "$FROM_PREFIX" {} +)

echo "tools/prep_github_mirroring.sh: rewrote $count file(s) ($FROM_PREFIX -> $TO_PREFIX)"

# Bash's ${var/pattern/replacement} treats [ and ] as glob metacharacters,
# which the badge markdown is full of, so a literal multi-line swap needs
# python3's str.replace instead (already a tool dependency -- see
# tools/coverage.sh).
python3 - "$FROM_BADGES" "$TO_BADGES" <<'PY'
import sys
import pathlib

from_text, to_text = sys.argv[1], sys.argv[2]
path = pathlib.Path("README.md")
content = path.read_text()
if from_text in content:
    path.write_text(content.replace(from_text, to_text))
    print("tools/prep_github_mirroring.sh: swapped README.md's badges")
else:
    print("tools/prep_github_mirroring.sh: README.md's badges already in the target state, left as-is")
PY
