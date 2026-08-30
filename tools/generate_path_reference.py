#!/usr/bin/env python3
"""Emit the CPython reference table `test/test_utils.f90` asserts `parquet_utils` against.

`parquet_utils`' path procedures are specified as "what CPython's `posixpath` does", so the values
they are tested against should come from CPython rather than from someone transcribing them into a
Fortran array. There are around fifty rows across two families, and a transcription error in a
*reference* table is invisible: the implementation is then written to match the wrong value and
every test passes.

Emits `test/test_path_vectors.f90`, a committed Fortran module of `parameter` arrays -- source
rather than a data file the test reads at run time, so the suite stays free of fixture files and of
the shared-fixture-path hazard that comes with tests running concurrently.

  generate_path_reference.py             rewrite test/test_path_vectors.f90
  generate_path_reference.py --check     exit nonzero if the committed copy has drifted
  generate_path_reference.py --self-test check this interpreter really has POSIX path rules

**`posixpath` is imported explicitly and `os.path` is never used.** `os.path` is an alias that
resolves to `ntpath` on Windows, so a generator written against it would emit Windows rules --
backslash separators included -- when run on a Windows host, making `--check` host-dependent and
regenerating a reference the Fortran implementation cannot satisfy. The module's contract is POSIX
lexical handling on every platform. `--self-test` runs before every emission so that the guarantee
does not rest on a reviewer noticing the import line.

**What this file CANNOT express, and why those rows live in `test/test_utils.f90` by hand.** Three
of `parquet_utils`' rules are Fortran-specific extensions of the Python contract rather than
transcriptions of it, so Python cannot be asked about them: `pf_join_path` over a zero-size array
(`posixpath.join()` raises `TypeError`), a blank-padded fixed-length `suffix`, and a component with
trailing blanks. Putting any of them here would make it look as though CPython had been consulted.
"""

import argparse
import posixpath
import sys
from pathlib import Path

OUT = Path(__file__).resolve().parent.parent / "test" / "test_path_vectors.f90"

# ---------------------------------------------------------------------------
# The fixtures. Every row of the two tables in feature_utils.md is here, plus the cases the
# design calls out as the ones a hand-rolled implementation gets wrong.
# ---------------------------------------------------------------------------

# Join fixtures: each is a tuple of components, 2 to 5 of them.
JOIN_CASES = [
    ("a", "b"),
    ("a/", "b"),
    ("a", "/b"),
    ("a/", "/b"),
    ("", "b"),
    ("a", ""),
    ("a", "b/"),
    ("./", "b"),
    ("/", "b"),
    ("a/b", "../c"),
    ("a//", "b"),
    ("a", "b//"),
    (" a", "b"),
    ("/", "/"),
    ("", ""),
    ("a", "b", "c"),
    ("a", "", "b"),
    ("a/", "b", "c/"),
    ("a", "/b", "c"),
    ("", "", ""),
    ("/x", "y", "z"),
    ("a", "b", "c", "d"),
    ("a", "", "", "d"),
    ("/", "a", "b", "c"),
    ("a", "b", "/c", "d"),
    ("a", "b", "c", "d", "e"),
    ("a/", "/b", "c/", "d", ""),
    ("", "a", "", "b", ""),
    ("/a", "b", "c", "d", "e"),
    ("x", "y", "z", "w", "/v"),
]

# Split fixtures: one path each, exercising dirname/basename/splitext together.
SPLIT_CASES = [
    "/data/run3/cat.parquet",
    "myfile.txt",
    "/a/b/",
    "/x",
    "/",
    "",
    "./x",
    ".bashrc",
    "a.tar.gz",
    "/a.b/c",
    "a.",
    "..",
    "...",
    ".a.b",
    "a//b",
    "a//b.txt",
    "//",
    "//a",
    "dir/.hidden",
    "dir/.hidden.txt",
    "/data/",
    "x/y/z.tar.gz",
    " leading/space.txt",
    "no_ext_at_all",
    "/.",
    "trailing.dot.",
]

# Suffix fixtures: (path, suffix). The expected value is derived the way pf_path_add_suffix is
# specified -- the literal prefix up to and including the last separator, then stem, suffix, ext.
SUFFIX_CASES = [
    ("/data/myfile.txt", "_stat"),
    ("myfile.txt", "_stat"),
    ("myfile", "_stat"),
    ("a.tar.gz", "_stat"),
    (".bashrc", "_stat"),
    ("/a/b/", "_stat"),
    ("a//b.txt", "_stat"),
    ("/x", "_2"),
    ("", "_x"),
    ("dir/.hidden", "_s"),
]


def self_test():
    """Refuse to emit unless this interpreter really has POSIX path rules.

    Three discriminators, each of which `ntpath` answers differently: the separator itself, and
    two backslash cases -- `ntpath` treats a backslash as a separator, so it both joins and splits
    around one where `posixpath` treats it as an ordinary filename character.
    """
    failures = []
    if posixpath.sep != "/":
        failures.append(f"posixpath.sep is {posixpath.sep!r}, expected '/'")
    if posixpath.join("a", "b\\c") != "a/b\\c":
        failures.append("posixpath.join treats a backslash as a separator -- this is not posixpath")
    if posixpath.basename("a\\b.txt") != "a\\b.txt":
        failures.append("posixpath.basename split on a backslash -- this is not posixpath")
    if failures:
        for f in failures:
            print(f"generate_path_reference.py: SELF-TEST FAILED: {f}", file=sys.stderr)
        return False
    return True


def add_suffix(path, suffix):
    """The reference implementation of `pf_path_add_suffix`, in terms of posixpath only.

    Deliberately written from the same two positions the Fortran scanner finds -- the last
    separator and the extension dot -- rather than from `dirname`, so that an interior `a//b` is
    preserved instead of being collapsed by a rebuild through `join`.
    """
    i = path.rfind("/") + 1
    head, tail = path[:i], path[i:]
    stem, ext = posixpath.splitext(tail)
    return head + stem + suffix + ext


def quote(s):
    """One Fortran character literal, with the apostrophe convention this project uses."""
    return '"' + s.replace('"', '""') + '"'


def reject_trailing_blank(name, values):
    """Refuse any value whose trailing blank the fixed-length storage would silently eat.

    Every array here is `character(len=W)`, so a shorter value is blank-padded and the test has to
    `trim` the reference to recover it. That recovery is exact only while no reference value ends
    in a blank of its own -- one that did would be tested against a value with the blank removed,
    silently, and the row would assert something no one wrote. Leading blanks are unaffected and
    one fixture deliberately has one.
    """
    bad = [v for v in values if v != v.rstrip(" ")]
    if bad:
        raise SystemExit(
            f"generate_path_reference.py: {name} contains a value with a trailing blank, which "
            f"fixed-length storage cannot represent: {bad!r}")


def fortran_array(name, values, width, indent="    "):
    """A `character(len=width), parameter :: name(n) = [...]` declaration, wrapped at 132 columns."""
    reject_trailing_blank(name, values)
    lines = []
    lines.append(f"{indent}character(len={width}), parameter :: {name}(*) = [ &")
    body = [f"{indent}    character(len={width}) :: {quote(v)}" if i == 0 else f"{indent}    {quote(v)}"
            for i, v in enumerate(values)]
    for i, item in enumerate(body):
        tail = ", &" if i < len(body) - 1 else " &"
        lines.append(item + tail)
    lines.append(f"{indent}    ]")
    return "\n".join(lines)


def build():
    """Return the whole generated file as one string."""
    join_arity = [len(c) for c in JOIN_CASES]
    # Every join case is stored padded to five components; the arity says how many are real.
    join_parts = [[c[i] if i < len(c) else "" for c in JOIN_CASES] for i in range(5)]
    join_want = [posixpath.join(*c) for c in JOIN_CASES]

    split_dir = [posixpath.dirname(p) for p in SPLIT_CASES]
    split_base = [posixpath.basename(p) for p in SPLIT_CASES]
    split_ext = [posixpath.splitext(p)[1] for p in SPLIT_CASES]
    split_stem = [posixpath.splitext(posixpath.basename(p))[0] for p in SPLIT_CASES]

    suffix_path = [c[0] for c in SUFFIX_CASES]
    suffix_sfx = [c[1] for c in SUFFIX_CASES]
    suffix_want = [add_suffix(*c) for c in SUFFIX_CASES]

    def w(seqs):
        return max(1, max(len(s) for seq in seqs for s in seq))

    jw = w([*join_parts, join_want])
    sw = w([SPLIT_CASES, split_dir, split_base, split_ext, split_stem])
    fw = w([suffix_path, suffix_sfx, suffix_want])

    out = []
    a = out.append
    a("!> CPython reference values for `parquet_utils`' path procedures.")
    a("!!")
    a("!! **GENERATED FILE -- DO NOT EDIT BY HAND.** Emitted by `tools/generate_path_reference.py`;")
    a("!! `tools/generate_path_reference.py --check` fails if this file has drifted from what the")
    a("!! generator produces, and CI runs it. **Never hand-edit a value here**: these rows are what")
    a("!! turn \"we follow Python\" into something checked rather than claimed, so an edited one is a")
    a("!! lie that nothing else would catch.")
    a("!!")
    a("!! Every value is `posixpath.join`, `posixpath.dirname`, `posixpath.basename` or")
    a("!! `posixpath.splitext` applied to the fixture beside it, by the CPython this was last")
    a("!! regenerated with. `posixpath` is named explicitly and `os.path` never is: the latter")
    a("!! resolves to `ntpath` on Windows, which would emit Windows rules from a Windows host.")
    a("!!")
    a("!! **Three of `parquet_utils`' rules are NOT here and must not be added.** `pf_join_path`")
    a("!! over a zero-size array, a blank-padded fixed-length `suffix`, and a component with")
    a("!! trailing blanks are Fortran-specific extensions of the Python contract -- CPython either")
    a("!! raises or has no equivalent -- so they are asserted by hand in `test/test_utils.f90`.")
    a("!! Putting them here would make it look as though CPython had been consulted about them.")
    a("module test_path_vectors")
    a("    implicit none")
    a("    public")
    a("")
    a(f"    !> Join cases recorded.")
    a(f"    integer, parameter :: pv_n_join = {len(JOIN_CASES)}")
    a(f"    !> Path-splitting cases recorded.")
    a(f"    integer, parameter :: pv_n_split = {len(SPLIT_CASES)}")
    a(f"    !> Suffix-insertion cases recorded.")
    a(f"    integer, parameter :: pv_n_suffix = {len(SUFFIX_CASES)}")
    a("")
    a("    !> How many of the five component slots each join case actually uses.")
    a("    integer, parameter :: pv_join_arity(*) = [ &")
    for i in range(0, len(join_arity), 20):
        chunk = ", ".join(str(x) for x in join_arity[i:i + 20])
        tail = ", &" if i + 20 < len(join_arity) else " &"
        a(f"        {chunk}{tail}")
    a("        ]")
    a("")
    for k in range(5):
        a(f"    !> Component {k + 1} of each join case; blank past that case's arity.")
        a(fortran_array(f"pv_join_p{k + 1}", join_parts[k], jw))
        a("")
    a("    !> What `posixpath.join` returns for each case.")
    a(fortran_array("pv_join_want", join_want, jw))
    a("")
    a("    !> The path each splitting case is applied to.")
    a(fortran_array("pv_split_path", SPLIT_CASES, sw))
    a("")
    a("    !> `posixpath.dirname` of each.")
    a(fortran_array("pv_split_dir", split_dir, sw))
    a("")
    a("    !> `posixpath.basename` of each.")
    a(fortran_array("pv_split_base", split_base, sw))
    a("")
    a("    !> `posixpath.splitext(path)[1]` of each -- the extension, including its dot.")
    a(fortran_array("pv_split_ext", split_ext, sw))
    a("")
    a("    !> `posixpath.splitext(posixpath.basename(path))[0]` of each.")
    a(fortran_array("pv_split_stem", split_stem, sw))
    a("")
    a("    !> The path each suffix-insertion case is applied to.")
    a(fortran_array("pv_suffix_path", suffix_path, fw))
    a("")
    a("    !> The suffix inserted before the extension.")
    a(fortran_array("pv_suffix_sfx", suffix_sfx, fw))
    a("")
    a("    !> The rebuilt path: prefix through the last separator, then stem, suffix, extension.")
    a(fortran_array("pv_suffix_want", suffix_want, fw))
    a("")
    a("    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.")
    a("end module test_path_vectors ! GCOVR_EXCL_LINE")
    text = "\n".join(out) + "\n"

    over = [(i + 1, len(ln)) for i, ln in enumerate(text.splitlines()) if len(ln) > 132]
    if over:
        raise SystemExit(f"generate_path_reference.py: emitted lines exceed 132 columns: {over[:5]}")
    return text


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--check", action="store_true", help="exit nonzero if the committed file has drifted")
    ap.add_argument("--self-test", action="store_true", help="verify POSIX path rules and exit")
    args = ap.parse_args()

    if not self_test():
        return 1
    if args.self_test:
        print("generate_path_reference.py: self-test passed (posixpath rules confirmed)")
        return 0

    text = build()
    if args.check:
        if not OUT.exists():
            print(f"generate_path_reference.py: {OUT} does not exist", file=sys.stderr)
            return 1
        if OUT.read_text() != text:
            print(f"generate_path_reference.py: {OUT} has drifted; re-run the generator", file=sys.stderr)
            return 1
        print(f"generate_path_reference.py: {OUT.name} is up to date")
        return 0

    OUT.write_text(text)
    print(f"generate_path_reference.py: wrote {OUT}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
