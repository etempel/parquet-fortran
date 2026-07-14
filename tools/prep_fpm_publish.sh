#!/usr/bin/env bash
# Prepares a disposable *local* branch for running `fpm publish` manually -- see
# CONTRIBUTING.md's "Publishing to the fpm registry". `fpm publish` packages its tarball from
# git HEAD (a real commit), not the working tree or the staged index -- confirmed by testing:
# uncommitted and even staged edits are silently ignored, only a committed tree is reflected.
# So unlike tools/prep_github_mirroring.sh's on-disk rewrite, this needs an actual commit to
# take effect -- it just never leaves main or gets pushed anywhere, mirroring
# tools/mirror_to_github.sh's own disposable-branch pattern (minus the push).
#
# The commit combines:
#
#   1. tools/prep_github_mirroring.sh's README.md/doc/pages/*.md link+badge rewrite, so a
#      published package doesn't embed gitlab.4most.eu-only links unreachable by outside
#      registry consumers.
#   2. The fpm.toml edits needed to get past the registry's mandatory module-naming
#      enforcement: comments out the `test-drive` dev-dependency (its own modules don't
#      comply, and fpm has no per-dependency exemption -- fortran-lang/fpm#828/#883, confirmed
#      by testing) and switches `module-naming` from `false` to this project's
#      registry-registered custom prefix, `"parquet"`.
#   3. Removing maintainer/CI-only files that a downstream library consumer has no use for --
#      see REMOVE_PATHS below. `tools/generate_parquet_maml.sh` is deliberately kept: it's
#      consumer-facing (see doc/pages/embedding-maml-schemas.md). This list must stay in sync
#      by hand -- see CLAUDE.md's "Keeping tools/prep_fpm_publish.sh in sync" for the rule.
#
# After committing, this also runs fpm's own read-only preview commands -- none of them need a
# token or touch the registry -- and self-checks the tarball they produce: extracts it and
# confirms every REMOVE_PATHS entry is actually absent, fpm.toml's edits actually landed, and a
# handful of files that must NOT have been stripped are still present. This is exactly the class
# of bug that was only caught earlier by hand-extracting a tarball and reading it (the
# working-tree-vs.-HEAD confusion this script's design already works around) -- automating it
# here means a future REMOVE_PATHS/fpm.toml drift fails loudly instead of shipping silently in a
# permanent, undeletable published version.
#
# `module-naming` stays `false` (and `test-drive` stays a real dependency, and all REMOVE_PATHS
# files stay) on main by default, since enabling naming locally breaks `fpm build`/`fpm test`
# via the test-drive dependency -- this script's commit is only ever meant to be local scratch
# work, never merged into main and never pushed to any remote.
#
# Requires a clean working tree first (like tools/mirror_to_github.sh). All REMOVE_PATHS
# entries are validated to exist *before* the disposable branch is even created, so a
# stale/out-of-sync list fails immediately with zero side effects.
#
# Deliberately NOT run here: `fpm publish --token TOKEN [--dry-run]`. That's the one genuinely
# consequential step (needs a real token, and a non-dry-run upload is permanent), so it stays a
# manual, deliberate command -- see CONTRIBUTING.md.
#
# Revert everything afterward with:
#
#   git checkout <original-branch> && git branch -D fpm-publish-prep
#   rm -f fpm_model.json
#
# Usage:
#   tools/prep_fpm_publish.sh
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

if [ -n "$(git status --porcelain)" ]; then
    echo "tools/prep_fpm_publish.sh: working tree is not clean; commit or stash first" >&2
    exit 1
fi

# Maintainer/CI-only paths, not relevant to a downstream consumer of the published library.
# tools/prep_fpm_publish.sh (this file) is deliberately NOT in this list: deleting a running
# shell script out from under its own interpreter is a fragile pattern, so it's left in place --
# one small maintainer-only script left in the tarball is an accepted trade-off.
REMOVE_PATHS=(
    CLAUDE.md
    CONTRIBUTING.md
    .gitlab-ci.yml
    .github
    docs.md
    app/benchmark_threads.f90
    tools/benchmark_threads.sh
    tools/check_doc_anchors.py
    tools/count_lines.py
    tools/coverage.sh
    tools/fix_ford_page_links.sh
    tools/generate_fixtures.cpp
    tools/mirror_to_github.sh
    tools/prep_github_mirroring.sh
    tools/run_error_scenarios.sh
    tools/run_generate_fixtures.sh
)

# Files that must survive into the tarball -- a sanity check in the opposite direction from
# REMOVE_PATHS, catching an over-broad future edit that strips something it shouldn't.
KEEP_PATHS=(
    fpm.toml
    LICENSE
    README.md
    VERSION.txt
    src/parquet.f90
    tools/generate_parquet_maml.sh
)

for path in "${REMOVE_PATHS[@]}"; do
    if [ ! -e "$path" ]; then
        echo "tools/prep_fpm_publish.sh: expected path '$path' does not exist -- REMOVE_PATHS is" >&2
        echo "out of sync with the repository (renamed/moved file?); fix the list before continuing." >&2
        exit 1
    fi
done

original_branch="$(git rev-parse --abbrev-ref HEAD)"

git checkout -B fpm-publish-prep "$original_branch"

tools/prep_github_mirroring.sh

python3 - <<'PY'
import pathlib

path = pathlib.Path("fpm.toml")
content = path.read_text()

old_dep = 'test-drive.git = "https://github.com/fortran-lang/test-drive"\n'
new_dep = '#test-drive.git = "https://github.com/fortran-lang/test-drive"\n'
if old_dep not in content:
    raise SystemExit("tools/prep_fpm_publish.sh: test-drive dependency line not found as expected")
content = content.replace(old_dep, new_dep, 1)

old_naming = "module-naming = false\n"
new_naming = 'module-naming = "parquet"\n'
if old_naming not in content:
    raise SystemExit("tools/prep_fpm_publish.sh: 'module-naming = false' line not found as expected")
content = content.replace(old_naming, new_naming, 1)

pathlib.Path("fpm.toml").write_text(content)
print('tools/prep_fpm_publish.sh: fpm.toml -- test-drive dependency commented out, '
      'module-naming set to "parquet"')
PY

for path in "${REMOVE_PATHS[@]}"; do
    rm -rf -- "$path"
done
echo "tools/prep_fpm_publish.sh: removed ${#REMOVE_PATHS[@]} maintainer/CI-only path(s)"

git commit -am "Prepare fpm publish tarball (local-only, never pushed)" -q
echo "tools/prep_fpm_publish.sh: committed to disposable local branch 'fpm-publish-prep'"

echo "tools/prep_fpm_publish.sh: fpm publish --show-package-version:"
fpm publish --show-package-version

echo "tools/prep_fpm_publish.sh: fpm build --dump fpm_model.json:"
fpm build --dump fpm_model.json

echo "tools/prep_fpm_publish.sh: fpm publish --show-upload-data:"
upload_data="$(fpm publish --show-upload-data)"
printf '%s\n' "$upload_data"

tarball_path="$(printf '%s\n' "$upload_data" | grep -oE 'tarball=@"[^"]+"' | sed -E 's/tarball=@"(.+)"/\1/')"
if [ -z "$tarball_path" ] || [ ! -f "$tarball_path" ]; then
    echo "tools/prep_fpm_publish.sh: could not locate the generated tarball in 'fpm publish" >&2
    echo "--show-upload-data' output above -- skipping the tarball self-check." >&2
    exit 1
fi

inspect_dir="$(mktemp -d)"
tar xzf "$tarball_path" -C "$inspect_dir"

self_check_failed=0

for path in "${REMOVE_PATHS[@]}"; do
    if [ -e "$inspect_dir/$path" ]; then
        echo "tools/prep_fpm_publish.sh: TARBALL SELF-CHECK FAILED -- '$path' should have been" >&2
        echo "stripped but is present in the published tarball" >&2
        self_check_failed=1
    fi
done

for path in "${KEEP_PATHS[@]}"; do
    if [ ! -e "$inspect_dir/$path" ]; then
        echo "tools/prep_fpm_publish.sh: TARBALL SELF-CHECK FAILED -- expected '$path' to be" >&2
        echo "present in the published tarball but it's missing" >&2
        self_check_failed=1
    fi
done

if ! grep -q '^module-naming = "parquet"$' "$inspect_dir/fpm.toml"; then
    echo 'tools/prep_fpm_publish.sh: TARBALL SELF-CHECK FAILED -- fpm.toml inside the tarball' >&2
    echo 'does not have module-naming = "parquet"' >&2
    self_check_failed=1
fi
if grep -q '^test-drive\.git = ' "$inspect_dir/fpm.toml"; then
    echo "tools/prep_fpm_publish.sh: TARBALL SELF-CHECK FAILED -- fpm.toml inside the tarball" >&2
    echo "still has an active (uncommented) test-drive dependency" >&2
    self_check_failed=1
fi

rm -rf "$inspect_dir"

if [ "$self_check_failed" -ne 0 ]; then
    echo "tools/prep_fpm_publish.sh: tarball self-check FAILED -- do NOT run 'fpm publish" >&2
    echo "--token ...' against this branch. The disposable branch is left in place for" >&2
    echo "debugging; revert with: git checkout $original_branch && git branch -D fpm-publish-prep" >&2
    exit 1
fi

echo "tools/prep_fpm_publish.sh: tarball self-check passed (REMOVE_PATHS absent, KEEP_PATHS" \
     "present, fpm.toml edits landed)"
echo "tools/prep_fpm_publish.sh: on disposable local branch 'fpm-publish-prep', ready for the" \
     "'fpm publish --token TOKEN [--dry-run]' steps in CONTRIBUTING.md's 'Publishing to the fpm" \
     "registry' section."
echo "Revert everything with: git checkout $original_branch && git branch -D fpm-publish-prep"
echo "                        rm -f fpm_model.json"
