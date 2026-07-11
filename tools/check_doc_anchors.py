#!/usr/bin/env python3
"""Validates every in-repo Markdown link that targets a heading anchor --
both same-file (`#some-heading`) and cross-file (`other.md#some-heading`)
links -- against the anchors GitHub itself would generate for that file's
headings.

Anchors are computed with GitHub's actual slugging rules (see
https://github.com/Flet/github-slugger), not a guess: lowercase, strip a
fixed set of ASCII punctuation (but keep `-` and `_`), turn spaces into
hyphens, and suffix `-1`/`-2`/... on repeated headings within the same file.
This class of bug has bitten this repository before -- a heading like
`### Foo (..., print_stat=.true.)` slugs to `foo-print_stattrue` (note the
hyphen before `print_stattrue`, not two words run together), which is easy
to get wrong by hand when writing a `[link](#anchor)`.

Scope: this implements GitHub's stripped-character set precisely for ASCII,
which is all this repository's headings currently use. Non-ASCII headings
fall back to stripping punctuation via unicodedata, which is a reasonable
approximation but not a byte-for-byte match to GitHub's Unicode ranges.

Usage:
    tools/check_doc_anchors.py

Scans every *.md file in the repository root. Exits nonzero and prints one
line per unresolved link if any anchor doesn't match a heading in its
target file.
"""
import re
import sys
import unicodedata
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

HEADING_RE = re.compile(r"^(#{1,6})\s+(.*?)\s*#*\s*$", re.MULTILINE)
LINK_RE = re.compile(r"\]\(([^)#\s]*)#([^)\s]+)\)")
MD_LINK_RE = re.compile(r"\[([^\]]*)\]\([^)]*\)")

# GitHub's slugger (see regex.js in the github-slugger package) strips these
# ASCII ranges outright (control chars, then punctuation), but deliberately
# keeps '-' (0x2D) and '_' (0x5F) as word characters.
_STRIP_ASCII = "".join(
    chr(c) for c in list(range(0x00, 0x20))
    + list(range(0x21, 0x2D)) + list(range(0x2E, 0x30))
    + list(range(0x3A, 0x41)) + list(range(0x5B, 0x5F))
    + [0x60] + list(range(0x7B, 0x7F))
)
_STRIP_ASCII_TABLE = str.maketrans("", "", _STRIP_ASCII)


def github_slug(heading, occurrences):
    """Reproduces github-slugger's slug() + per-document uniquing."""
    text = MD_LINK_RE.sub(r"\1", heading).lower()
    ascii_part = "".join(ch for ch in text if ord(ch) < 128)
    if len(ascii_part) == len(text):
        stripped = text.translate(_STRIP_ASCII_TABLE)
    else:
        # Non-ASCII heading: fall back to stripping punctuation/symbols by
        # Unicode category, keeping '-' and '_' -- see module docstring.
        stripped = "".join(
            ch for ch in text
            if ch in "-_ " or not unicodedata.category(ch).startswith(("P", "S", "C"))
        )
    slug = stripped.replace(" ", "-")

    original = slug
    if slug in occurrences:
        occurrences[original] += 1
        slug = f"{original}-{occurrences[original]}"
        occurrences[slug] = 0
    else:
        occurrences[slug] = 0
    return slug


def heading_slugs(text):
    occurrences = {}
    slugs = set()
    for match in HEADING_RE.finditer(text):
        slugs.add(github_slug(match.group(2), occurrences))
    return slugs


def check_file(path, cache):
    text = path.read_text()
    problems = []
    for match in LINK_RE.finditer(text):
        target_file, anchor = match.group(1), match.group(2)
        target_path = (path.parent / target_file).resolve() if target_file else path
        if target_path not in cache:
            if not target_path.is_file():
                problems.append((match.group(0), f"target file not found: {target_file}"))
                continue
            cache[target_path] = heading_slugs(target_path.read_text())
        if anchor not in cache[target_path]:
            where = target_file if target_file else "(this file)"
            problems.append((match.group(0), f"no heading in {where} slugs to '#{anchor}'"))
    return problems


def main():
    md_files = sorted(REPO_ROOT.glob("*.md"))
    cache = {}
    total_problems = 0

    print("Checking:", ", ".join(str(p.relative_to(REPO_ROOT)) for p in md_files))

    for path in md_files:
        problems = check_file(path, cache)
        if problems:
            print(f"{path.relative_to(REPO_ROOT)}:")
            for link, reason in problems:
                print(f"  {link}  -- {reason}")
            total_problems += len(problems)

    if total_problems:
        print(f"\n{total_problems} broken anchor link(s) found.")
        return 1

    print(f"All anchor links resolve OK ({len(md_files)} file(s) checked).")
    return 0


if __name__ == "__main__":
    sys.exit(main())
