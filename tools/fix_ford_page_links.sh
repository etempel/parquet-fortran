#!/usr/bin/env bash
# Rewrites doc/pages/*.md links (and doc/media/* asset references) to their
# FORD-generated targets across a directory of already-generated FORD HTML
# output.
#
# README.md links to the user guide using its source-relative path (e.g.
# `doc/pages/reading.md`, sometimes with a `#anchor`) -- correct when
# README.md is browsed directly on GitLab/GitHub, since that's the actual
# file. FORD's front page (docs.md is `{!README.md!}`, embedding README.md's
# raw markdown verbatim) does not resolve these into its own page/<name>.html
# scheme the way it resolves them internally: FORD's own generated navbar
# correctly links doc/pages/index.md to page/index.html, but the same path
# written out in README.md's body text is passed through unresolved, so it
# 404s once published. This doesn't affect doc/pages/*.md files linking to
# each other -- they already use FORD's own page/*.html-relative convention
# directly in source, since those pages are FORD narrative pages, not meant
# to be browsed raw.
#
# The same problem applies to any doc/media/* asset reference (e.g. an
# `<img src="doc/media/logo.svg">` in README.md's own body): FORD copies
# media_dir's contents to ford-doc/media/, not ford-doc/doc/media/, so the
# source-relative path is one directory level off once embedded, exactly
# like the doc/pages/*.md case above.
#
# Host-independent (unlike tools/prep_github_mirroring.sh's GitLab/GitHub
# link rewriting) -- this is a FORD-rendering quirk, not a GitLab-vs-GitHub
# one, so it must run against both doc-publish CI jobs
# (.github/workflows/docs.yml and .gitlab-ci.yml's readthedocs job), right
# after `ford docs.md`.
#
# Usage:
#   tools/fix_ford_page_links.sh <dir>
set -euo pipefail

DIR="${1:?usage: tools/fix_ford_page_links.sh <dir>}"

if [ ! -d "$DIR" ]; then
    echo "tools/fix_ford_page_links.sh: no such directory: $DIR" >&2
    exit 1
fi

python3 - "$DIR" <<'PY'
import re
import sys
import pathlib

root = pathlib.Path(sys.argv[1])
page_pattern = re.compile(r'href="doc/pages/([A-Za-z0-9_-]+)\.md(#[A-Za-z0-9_-]+)?"')
media_pattern = re.compile(r'(src|href)="doc/media/([^"]+)"')

count = 0
for path in sorted(root.rglob("*.html")):
    content = path.read_text()
    content, n_page = page_pattern.subn(
        lambda m: f'href="page/{m.group(1)}.html{m.group(2) or ""}"', content
    )
    content, n_media = media_pattern.subn(
        lambda m: f'{m.group(1)}="media/{m.group(2)}"', content
    )
    n = n_page + n_media
    if n:
        path.write_text(content)
        count += n

print(f"tools/fix_ford_page_links.sh: rewrote {count} link(s) under {root}")
PY
