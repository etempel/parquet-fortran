#!/usr/bin/env bash
# Pushes the current local main branch to this project's GitHub mirror
# (github.com/etempel/parquet-fortran) -- see CONTRIBUTING.md's "Mirroring
# to GitHub". Retargets the cross-repo-file links in README.md/
# doc/pages/*.md at GitHub first (tools/prep_github_mirroring.sh), so the
# pushed commit is correct there too, both for browsing the mirror directly
# and for its own separately published FORD docs (GitHub Pages).
#
# Does this from a disposable local branch rather than committing the link
# rewrite onto main directly, so main (and origin/gitlab.4most.eu) never
# carries GitHub-targeted links even transiently -- the branch is
# force-pushed to GitHub's main and then deleted locally, so there is
# nothing to remember to reverse afterward.
#
# Requires a "github" remote (see CONTRIBUTING.md's one-time
# `git remote add github ...` setup) and a clean working tree.
#
# Force-pushes to a real remote, so it only runs when explicitly asked:
# pass --github. Called with no arguments (or anything else), it does
# nothing.
#
# Usage:
#   tools/mirror_to_github.sh --github
set -euo pipefail

if [ "${1:-}" != "--github" ]; then
    echo "tools/mirror_to_github.sh: no action taken -- pass --github to actually push to the" >&2
    echo "GitHub mirror (this force-pushes to a real remote, so it's not run by accident)." >&2
    exit 1
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

if ! git remote get-url github >/dev/null 2>&1; then
    echo "tools/mirror_to_github.sh: no 'github' remote configured -- see CONTRIBUTING.md's" >&2
    echo "'Mirroring to GitHub' section for the one-time \`git remote add github ...\` setup." >&2
    exit 1
fi

if [ -n "$(git status --porcelain)" ]; then
    echo "tools/mirror_to_github.sh: working tree is not clean; commit or stash first" >&2
    exit 1
fi

original_branch="$(git rev-parse --abbrev-ref HEAD)"

git checkout -B github-mirror main
tools/prep_github_mirroring.sh
git commit -am "Rewrite doc links for GitHub mirror"
git push --force github github-mirror:main
git checkout "$original_branch"
git branch -D github-mirror

echo "tools/mirror_to_github.sh: pushed main to github (mirror branch discarded, back on $original_branch)"
