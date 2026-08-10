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

Also scans `doc/pages/**/*.md` (the page tree is nested one level deep:
group directories, each with its own index.md). Those files link to each
other (and to the README-derived FORD front page) using FORD's own
rendered `page/*.html` form rather than raw `.md` paths -- e.g.
`sibling.html#a` for a page in the same group, `../<group>/name.html#a`
for a page in another group, `../index.html#a` for the guide's own
top-level index, and `../../index.html#a` for README.md -- since that is
the correct form for FORD's generated output (see CLAUDE.md's "FORD does
not resolve doc/pages/*.md links written in README.md's body text" note
for why README.md itself keeps the raw `doc/pages/<group>/<name>.md` form
instead). Rendered-form links are resolved by modelling the generated
site: each doc/pages/<rel>.md renders at <site>/page/<rel>.html, README.md
at <site>/index.html, and the generated listings under <site>/lists/ --
so a link is resolved lexically against its own page's rendered location
and mapped back to the Markdown source on disk. `<site>/lists/*` targets
are structurally valid but generated, so they are accepted without anchor
validation.

Anchor-LESS relative links (`[text](other.md)`, `[text](sibling.html)`)
are validated too, for target-file existence only -- a broken anchor-less
link is otherwise completely silent, which matters most for the rendered
`.html` form where nothing else ever checks the path. Links to git-ignored
`feature_*.md` scratch documents are exempt from the existence check
(they legitimately come and go, and never exist in CI at all);
`feature_risks.md` is tracked and is NOT exempt.

Usage:
    tools/check_doc_anchors.py

Scans every *.md file in the repository root plus every *.md file under
doc/pages/ (recursively). Exits nonzero and prints one line per
unresolved link if any anchor doesn't match a heading in its target file,
or any relative link's target file does not exist.
"""
import re
import sys
import unicodedata
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

HEADING_RE = re.compile(r"^(#{1,6})\s+(.*?)\s*#*\s*$", re.MULTILINE)
LINK_RE = re.compile(r"\]\(([^)#\s]*)#([^)\s]+)\)")
FILE_LINK_RE = re.compile(r"\]\(([^)#\s]+)\)")  # anchor-less: no '#' anywhere in the target
MD_LINK_RE = re.compile(r"\[([^\]]*)\]\([^)]*\)")

# A fenced code block opener/closer: three or more backticks or tildes, at the
# start of a line (indentation allowed).
CODE_FENCE_RE = re.compile(r"^[ \t]*(`{3,}|~{3,})")
# An inline code span: a run of N backticks, the shortest run of non-newline
# characters not containing that same run, then N backticks again. Restricted to
# one line on purpose -- CommonMark allows a span to wrap, but masking across
# lines risks swallowing real links if a stray backtick is ever unbalanced.
INLINE_CODE_RE = re.compile(r"(`+)(?:(?!\1)[^\n])+?\1")


def mask_code(text):
    """Blanks out fenced code blocks and inline code spans, preserving length.

    Link scanning must not look inside code. Two things go wrong otherwise, and
    the second is what prompted this: a fenced block *showing* Markdown gets its
    example links validated as if they were real, and ordinary prose containing
    something like `CASTS['int32'](value)` in backticks matches the `](target)`
    link pattern and is reported as a link to a file named `value`.

    Every masked character becomes a space and every newline is kept, so offsets
    and line structure are unchanged -- which matters because the caller reports
    `match.group(0)` and because nothing else about the text may shift.

    **Only link scanning is masked, never heading extraction.** This repository's
    headings are full of backticks (`### The `parquet_strings` module`), so
    slugging masked text would produce the wrong anchors for most of them. Safe
    here because `heading_slugs` always re-reads the target file itself.
    """
    lines = text.split("\n")
    fence = None
    for i, line in enumerate(lines):
        m = CODE_FENCE_RE.match(line)
        if fence is None:
            if m:
                fence = m.group(1)[0]
                lines[i] = " " * len(line)
        else:
            # A closing fence is the same character, at least as long. Anything
            # else inside the block is blanked and does not end it.
            lines[i] = " " * len(line)
            if m and m.group(1)[0] == fence:
                fence = None
    masked = "\n".join(lines)
    return INLINE_CODE_RE.sub(lambda mm: " " * len(mm.group(0)), masked)

PAGES_ROOT = (Path(__file__).resolve().parent.parent / "doc" / "pages").resolve()

# Sentinel: the link resolved to something structurally valid that has no
# Markdown source to validate against (FORD's generated lists/ pages).
SKIP_TARGET = object()

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


def resolve_link_target(path, target_file):
    """Resolves a link's target file to the real Markdown source on disk.

    Handles three forms: a same-file `#anchor` (empty target_file), an
    ordinary relative `.md` path, and the FORD-rendered `.html` form used
    between doc/pages/**/*.md files. The rendered form is resolved by
    modelling the generated site (doc/pages/<rel>.md renders at
    <site>/page/<rel>.html; README.md at <site>/index.html; the generated
    listings under <site>/lists/): the link is normalised lexically
    against the linking page's own rendered directory and mapped back to
    source. This is what distinguishes `../index.html` (the guide's
    top-level index, one level up from a nested page) from
    `../../index.html` (the README front page, at the site root) -- one
    character apart, entirely different pages.

    Returns a Path, or SKIP_TARGET for a structurally valid target with no
    Markdown source (lists/), or None for a target that cannot be resolved
    at all (a rendered-form link outside doc/pages/, or one that escapes
    the site root).
    """
    if not target_file:
        return path
    if target_file.endswith(".html"):
        resolved = path.resolve()
        try:
            rel_dir = resolved.parent.relative_to(PAGES_ROOT)
        except ValueError:
            return None  # rendered-form link in a file FORD never renders
        rendered_dir = ("page",) + rel_dir.parts
        stack = list(rendered_dir)
        for part in Path(target_file).parts:
            if part == "..":
                if not stack:
                    return None  # escapes the rendered site root
                stack.pop()
            elif part != ".":
                stack.append(part)
        if not stack:
            return None
        if tuple(stack) == ("index.html",):
            return (REPO_ROOT / "README.md").resolve()
        if stack[0] == "lists":
            return SKIP_TARGET
        if stack[0] == "page":
            rel = Path(*stack[1:])
            return (PAGES_ROOT / rel).with_suffix(".md").resolve()
        return None
    return (path.parent / target_file).resolve()


def check_file(path, cache):
    # Masked for LINK scanning only -- see mask_code. Heading slugs come from a
    # fresh read of the target file, so they are unaffected.
    text = mask_code(path.read_text())
    problems = []
    for match in LINK_RE.finditer(text):
        target_file, anchor = match.group(1), match.group(2)
        if re.match(r"^[a-z][a-z0-9+.-]*://", target_file):
            continue  # external URL (e.g. a full gitlab.4most.eu link) -- not ours to validate
        # A LINE reference, not a heading anchor: `src/foo.f90#L42` / `#L42-L58`, which GitLab,
        # GitHub and the IDE all render as "jump to that line". Resolving it as a heading slug
        # would report every such link as broken. The line NUMBER is deliberately not validated --
        # it goes stale on any edit above it, and a stale line number is a navigation nuisance
        # rather than the broken cross-reference this tool exists to catch.
        if re.fullmatch(r"L\d+(-L\d+)?", anchor):
            continue
        target_path = resolve_link_target(path, target_file)
        if target_path is SKIP_TARGET:
            continue
        if target_path is None:
            problems.append((match.group(0), f"cannot resolve rendered-form target: {target_file}"))
            continue
        if target_path not in cache:
            if not target_path.is_file():
                problems.append((match.group(0), f"target file not found: {target_file}"))
                continue
            cache[target_path] = heading_slugs(target_path.read_text())
        if anchor not in cache[target_path]:
            where = target_file if target_file else "(this file)"
            problems.append((match.group(0), f"no heading in {where} slugs to '#{anchor}'"))

    # Anchor-less relative links: validate that the target file exists at all.
    # Without this, a mistyped path in a `[text](sibling.html)` or
    # `[text](doc/pages/<group>/name.md)` link is completely silent.
    for match in FILE_LINK_RE.finditer(text):
        target_file = match.group(1)
        if re.match(r"^[a-z][a-z0-9+.-]*:", target_file):
            continue  # external URL / mailto: -- not ours to validate
        target_path = resolve_link_target(path, target_file)
        if target_path is SKIP_TARGET:
            continue
        if target_path is None:
            problems.append((match.group(0), f"cannot resolve rendered-form target: {target_file}"))
            continue
        if target_path.exists():
            continue
        # Git-ignored scratch documents come and go by design (and never
        # exist in CI); a dangling link to one is not an error. The tracked
        # feature_risks.md always exists, so it never reaches this exemption.
        if target_path.name.startswith("feature_"):
            continue
        problems.append((match.group(0), f"target file not found: {target_file}"))
    return problems


def main():
    md_files = sorted(REPO_ROOT.glob("*.md")) + sorted((REPO_ROOT / "doc" / "pages").rglob("*.md"))
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
