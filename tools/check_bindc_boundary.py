#!/usr/bin/env python3
"""Cross-checks every Fortran `bind(C)` interface against its C++ `extern "C"` definition in
src/parquet_wrapper.cpp, verifying arity, base type, by-value-vs-by-reference, and (for
functions) return type all agree.

Why this matters: a `bind(C)` interface has no compile-time link to the C++ side it describes --
gfortran/gcc each compile their own half against the interface/definition text alone. A kind
mismatch (e.g. a dummy silently changed from `integer(c_int32_t)` to `integer(c_int64_t)` without
the matching C++ parameter changing too) compiles cleanly on both sides and corrupts memory
silently at runtime instead of failing to build. This script makes that failure mode visible
immediately instead of only via a review.

Two Fortran declaration shapes matter, both used throughout src/parquet_bindings.f90 -- a naive
regex misses one or both (see CLAUDE.md's "Why the earlier check found only 113 of 121" note):

  1. Five bindings are named after their C symbol directly, without the `parquet_` prefix
     (create_parquet_writer, create_parquet_reader, close_parquet_writer, close_parquet_reader,
     abandon_parquet_writer) -- a checker that filters candidate C++ definitions on `^parquet_`
     would silently drop all five.
  2. Every one of the 20 functions writes `result(...)` *after* `bind(C, name="...")`, not before
     -- `function NAME(args) &\n    bind(C, name="...") result(r)`.

Also scans test/error_scenarios.f90 and test/test_temporal.f90: these declare their own local
`bind(C)` debug-hook interfaces (hand-written per scenario, duplicated across several call sites),
declared far from parquet_bindings.f90 and so, per CLAUDE.md, the most likely place for a future
mismatch to slip in unnoticed. A handful of those bind to the C RUNTIME rather than to this
project's own C++ (see LIBC_SYMBOLS below); they are allow-listed by name, not by "ignore anything
unmatched", so a typo'd parquet_* binding still fails.

What this does NOT check (see CLAUDE.md's "What this check does not cover" note): length/ownership
contracts (does the C++ side write exactly as many elements as Fortran allocated?), array
rank/assumed-size shape, or NUL-termination/sentinel conventions. Those need a buffer-contract
audit, not a signature checker -- see CLAUDE.md's "The parquet_strings module" notes and feature_doc.md's
point 4 for the one confirmed instance of that class of bug found so far.

Usage:
    tools/check_bindc_boundary.py

Exits nonzero and prints one line per mismatch (missing C++ definition, arity mismatch, base-type
mismatch, by-value-vs-by-reference mismatch, or return-type mismatch) if any is found.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
FORTRAN_FILES = [
    REPO_ROOT / "src" / "parquet_bindings.f90",
    REPO_ROOT / "test" / "error_scenarios.f90",
    REPO_ROOT / "test" / "test_temporal.f90",
]
CPP_FILE = REPO_ROOT / "src" / "parquet_wrapper.cpp"

# Fortran kind -> (base_type, is_pointer_kind). is_pointer_kind is True only for c_ptr, which
# represents one level of pointer indirection all by itself -- see normalize_fortran_dummy below
# for how that shifts the by-value/by-reference arithmetic relative to every other kind.
FORTRAN_KIND_MAP = {
    "c_int8_t": "i8",
    "c_int16_t": "i16",
    "c_int32_t": "i32",
    "c_int": "i32",
    "c_int64_t": "i64",
    "c_long_long": "i64",
    "c_float": "f32",
    "real32": "f32",
    "c_double": "f64",
    "real64": "f64",
}

# C++ base type token -> normalized bucket. unsigned widths collapse onto their signed
# counterpart (same width/passing convention, so no arity/reference mismatch -- see CLAUDE.md's
# "Signedness" residual-risk note; a positive-only value reinterpreted above the signed max is a
# separate, narrower risk this script deliberately does not chase).
CPP_BASE_MAP = {
    "int8_t": "i8", "uint8_t": "i8", "char": "i8", "unsigned char": "i8", "bool": "i8",
    "int16_t": "i16", "uint16_t": "i16", "short": "i16",
    "int32_t": "i32", "uint32_t": "i32", "int": "i32", "unsigned int": "i32", "unsigned": "i32",
    "int64_t": "i64", "uint64_t": "i64", "long long": "i64", "unsigned long long": "i64",
    "float": "f32",
    "double": "f64",
    "void": "ptr",
}


#: C symbols a scanned file may bind to that are NOT defined in src/parquet_wrapper.cpp, because they
#: come from the C runtime. Kept as a short explicit allow-list rather than "ignore anything not
#: found", which would silently excuse a genuine typo in a parquet_* binding -- the failure this
#: whole script exists to catch.
#:
#: `setenv`/`unsetenv` are used only by the settings tests: Fortran cannot set an environment
#: variable, and parquet_settings_from_env has to be driven with one. They are test-only, so no
#: src/ file gains a POSIX dependency.
LIBC_SYMBOLS = {"setenv", "unsetenv"}


def strip_fortran_comments(text):
    """Strips `!`-to-end-of-line comments, but not `!` inside a quoted string (bind(C,
    name="...") literals are the only quoted strings that matter here, and never contain `!`, but
    this stays correct regardless)."""
    out_lines = []
    for line in text.splitlines():
        result = []
        in_quote = None
        i = 0
        while i < len(line):
            ch = line[i]
            if in_quote:
                result.append(ch)
                if ch == in_quote:
                    in_quote = None
            elif ch in "\"'":
                in_quote = ch
                result.append(ch)
            elif ch == "!":
                break
            else:
                result.append(ch)
            i += 1
        out_lines.append("".join(result))
    return "\n".join(out_lines)


def join_continuations(text):
    """Joins a trailing `&` line continuation with the next line (stripping the next line's own
    leading `&`, if present) -- nearly every declaration in these files wraps across lines."""
    lines = text.split("\n")
    out = []
    buf = ""
    for line in lines:
        stripped = line.rstrip()
        if stripped.endswith("&"):
            buf += stripped[:-1] + " "
        else:
            buf += line
            out.append(buf)
            buf = ""
    if buf:
        out.append(buf)
    joined = "\n".join(out)
    # A continued line's own leading '&' (marking where the previous line's text resumes) is now
    # a stray leading '&' on some lines -- drop one per line, only right after a join boundary.
    return re.sub(r"\n\s*&", "\n", joined)


FORTRAN_BINDC_RE = re.compile(
    r"\b(subroutine|function)\s+(\w+)\s*\(([^)]*)\)\s*"
    r"(?:result\s*\(\s*(\w+)\s*\)\s*)?"
    r"bind\s*\(\s*C\s*,\s*name\s*=\s*[\"'](\w+)[\"']\s*\)\s*"
    r"(?:result\s*\(\s*(\w+)\s*\))?",
    re.IGNORECASE,
)


def parse_fortran_dummy_decls(body, dummy_names):
    """Scans a `subroutine`/`function` interface body's `::` declaration lines, returning
    {dummy_name: (kind_token, has_value_attribute)}. `body` is the joined-continuation,
    comment-stripped text between the signature and the matching `end subroutine`/`end function`.
    """
    decls = {}
    for line in body.splitlines():
        if "::" not in line:
            continue
        lhs, rhs = line.split("::", 1)
        names = [n.strip().split("(")[0] for n in rhs.split(",")]
        lhs_low = lhs.lower()
        has_value = bool(re.search(r"\bvalue\b", lhs_low))
        if "type(c_ptr)" in lhs_low or "type (c_ptr)" in lhs_low:
            kind = "c_ptr"
        elif "character" in lhs_low:
            kind = "char"
        else:
            m = re.search(r"\(([a-z0-9_]+)\)", lhs_low)
            kind = m.group(1) if m else None
        for name in names:
            name = name.strip()
            if name in dummy_names:
                decls[name] = (kind, has_value)
    return decls


def normalize_fortran_dummy(kind, has_value):
    """Returns (base_type, expected_cpp_pointer_depth). A c_ptr dummy is itself already one level
    of indirection -- `value` on it yields a plain C pointer (depth 1); without `value` (the usual
    case for an intent(out) buffer-handoff argument) it is passed by reference, i.e. a pointer to
    the opaque pointer (depth 2). Every other kind is a plain scalar: `value` means passed
    directly (depth 0); without it, Fortran passes by reference (depth 1)."""
    if kind == "c_ptr":
        return "ptr", (1 if has_value else 2)
    if kind == "char":
        return "i8", 1  # character(kind=c_char) dummies are always assumed-size arrays (depth 1).
    base = FORTRAN_KIND_MAP.get(kind, kind)
    return base, (0 if has_value else 1)


def collect_fortran_symbols(path):
    """Returns {c_symbol_name: [(base_type, pointer_depth), ...], "__return__": (...)} per
    declared interface, keyed by the bind(C) name= (the actual linked C symbol, which can differ
    from the local Fortran procedure name -- see the module docstring's shape 1)."""
    text = strip_fortran_comments(path.read_text())
    text = join_continuations(text)
    results = {}
    for m in FORTRAN_BINDC_RE.finditer(text):
        kind_word, local_name, params, result_before, c_name, result_after = m.groups()
        result_var = result_before or result_after
        dummy_names = [p.strip().split("(")[0] for p in params.split(",") if p.strip()]
        start = m.end()
        end_re = re.compile(r"\bend\s+(?:subroutine|function)\b", re.IGNORECASE)
        end_m = end_re.search(text, start)
        body = text[start: end_m.start()] if end_m else text[start:start + 2000]
        wanted = set(dummy_names) | ({result_var} if result_var else set())
        decls = parse_fortran_dummy_decls(body, wanted)
        args = []
        for dn in dummy_names:
            if dn not in decls:
                args.append((None, None))  # unresolved -- reported as a mismatch downstream
                continue
            kind, has_value = decls[dn]
            args.append(normalize_fortran_dummy(kind, has_value))
        ret = None
        if kind_word.lower() == "function":
            if result_var and result_var in decls:
                kind, _has_value = decls[result_var]
                # A function result is inherently "the value" (never passed by reference), so
                # compute its expected depth as if `value` had been declared -- a scalar result
                # is depth 0, a c_ptr result (e.g. create_parquet_writer's opaque handle) is
                # depth 1, exactly like a `value` dummy of the same kind would be.
                ret = normalize_fortran_dummy(kind, has_value=True)
            else:
                ret = (None, None)
        results.setdefault(c_name, []).append({"args": args, "ret": ret, "is_function": kind_word.lower() == "function",
                                                "source": f"{path.relative_to(REPO_ROOT)}:{local_name}"})
    return results


def strip_cpp_comments(text):
    out = []
    i = 0
    n = len(text)
    while i < n:
        two = text[i:i + 2]
        if two == "//":
            j = text.find("\n", i)
            i = n if j == -1 else j
            continue
        if two == "/*":
            j = text.find("*/", i + 2)
            i = n if j == -1 else j + 2
            continue
        out.append(text[i])
        i += 1
    return "".join(out)


def find_extern_c_blocks(text):
    """Returns a list of (start, end) character offsets, each the {...} body of one `extern "C"`
    block -- there are several in this file, interspersed with template helpers that cannot have C
    language linkage (see parquet_wrapper.cpp's own comments on this)."""
    blocks = []
    for m in re.finditer(r'extern\s*"C"\s*\{', text):
        depth = 1
        i = m.end()
        while i < len(text) and depth > 0:
            if text[i] == "{":
                depth += 1
            elif text[i] == "}":
                depth -= 1
            i += 1
        blocks.append((m.end(), i - 1))
    return blocks


CPP_SIG_RE = re.compile(
    r"(?:static\s+|inline\s+)*"
    r"([A-Za-z_][\w:]*(?:\s*\*+)?(?:\s+[A-Za-z_][\w:]*(?:\s*\*+)?)*)\s+"
    r"(\**)\s*([A-Za-z_]\w*)\s*\(([^;{}]*)\)\s*$",
    re.DOTALL,
)


def parse_cpp_type(type_text):
    """Normalizes a C++ parameter/return type into (base_type, pointer_depth)."""
    t = type_text.strip()
    t = re.sub(r"\bconst\b", "", t)
    t = re.sub(r"\s+", " ", t).strip()
    depth = t.count("*")
    t = t.replace("*", "").strip()
    base = CPP_BASE_MAP.get(t, t)
    return base, depth


def collect_cpp_definitions(text):
    """Returns {name: (args=[(base,depth),...], ret=(base,depth))} for every function definition
    found at the top (file-scope) level of any `extern "C"` block -- collected by taking the last
    `name(params)` group immediately before each block-top-level `{`, which naturally excludes
    struct/class bodies (no trailing `(...)` before their `{`) and any control-flow statement
    (if/for/while/... only ever appear nested inside a function body, never at this depth)."""
    clean = strip_cpp_comments(text)
    defs = {}
    for block_start, block_end in find_extern_c_blocks(clean):
        depth = 0
        pending_start = block_start
        i = block_start
        while i < block_end:
            ch = clean[i]
            if ch == "{":
                if depth == 0:
                    candidate = clean[pending_start:i]
                    sig = CPP_SIG_RE.search(candidate)
                    if sig:
                        ret_type, extra_star, name, params = sig.groups()
                        ret = parse_cpp_type(ret_type + extra_star)
                        args = []
                        params = params.strip()
                        if params and params != "void":
                            for p in split_top_level_commas(params):
                                p = p.strip()
                                pm = re.match(r"^(.*?)([A-Za-z_]\w*)\s*(\[[^\]]*\])?$", p)
                                type_part = pm.group(1) if pm else p
                                if not type_part.strip():
                                    # An unnamed parameter (e.g. "int64_t," with no identifier) --
                                    # the regex above consumed the whole thing as "name" since
                                    # there was nothing left before it; treat it as a bare type.
                                    type_part = p
                                args.append(parse_cpp_type(type_part))
                        defs[name] = {"args": args, "ret": ret}
                    # Skip to this construct's matching close brace before resuming top-level scan.
                    depth = 1
                    j = i + 1
                    while j < block_end and depth > 0:
                        if clean[j] == "{":
                            depth += 1
                        elif clean[j] == "}":
                            depth -= 1
                        j += 1
                    i = j
                    pending_start = i
                    continue
                depth += 1
            elif ch == "}":
                depth -= 1
            elif ch == ";" and depth == 0:
                pending_start = i + 1
            i += 1
    return defs


def split_top_level_commas(s):
    parts = []
    depth = 0
    buf = ""
    for ch in s:
        if ch in "(<":
            depth += 1
        elif ch in ")>":
            depth -= 1
        if ch == "," and depth == 0:
            parts.append(buf)
            buf = ""
        else:
            buf += ch
    if buf.strip():
        parts.append(buf)
    return parts


def compare(fortran_defs, cpp_defs):
    problems = []
    checked_pairs = 0
    for c_name, decl_list in sorted(fortran_defs.items()):
        cpp = cpp_defs.get(c_name)
        if cpp is None:
            if c_name in LIBC_SYMBOLS:
                continue
            problems.append(f"{c_name}: no extern \"C\" definition found in {CPP_FILE.name} "
                             f"(declared at {decl_list[0]['source']})")
            continue
        for decl in decl_list:
            source = decl["source"]
            f_args, c_args = decl["args"], cpp["args"]
            if len(f_args) != len(c_args):
                problems.append(f"{c_name} ({source}): arity mismatch -- Fortran has {len(f_args)} "
                                 f"argument(s), C++ has {len(c_args)}")
                continue
            for idx, ((fbase, fdepth), (cbase, cdepth)) in enumerate(zip(f_args, c_args), start=1):
                checked_pairs += 1
                if fbase is None:
                    problems.append(f"{c_name} ({source}): argument #{idx} could not be resolved "
                                     f"on the Fortran side (missing '::' declaration?)")
                elif fbase == "ptr":
                    if cdepth != fdepth:
                        problems.append(f"{c_name} ({source}): argument #{idx} is a c_ptr expecting "
                                         f"C++ pointer depth {fdepth}, found depth {cdepth} ({cbase})")
                elif fbase != cbase or fdepth != cdepth:
                    problems.append(f"{c_name} ({source}): argument #{idx} mismatch -- Fortran "
                                     f"'{fbase}'x{fdepth} vs C++ '{cbase}'x{cdepth}")
            if decl["is_function"]:
                fbase, fdepth = decl["ret"]
                cbase, cdepth = cpp["ret"]
                checked_pairs += 1
                if fbase is None:
                    problems.append(f"{c_name} ({source}): function result could not be resolved "
                                     f"on the Fortran side")
                elif fbase == "ptr":
                    if cdepth != fdepth:
                        problems.append(f"{c_name} ({source}): return type is a c_ptr expecting "
                                         f"C++ pointer depth {fdepth}, found depth {cdepth} ({cbase})")
                elif fbase != cbase or fdepth != cdepth:
                    problems.append(f"{c_name} ({source}): return type mismatch -- Fortran "
                                     f"'{fbase}'x{fdepth} vs C++ '{cbase}'x{cdepth}")
    return problems, checked_pairs


def main():
    fortran_defs = {}
    for path in FORTRAN_FILES:
        for name, decls in collect_fortran_symbols(path).items():
            fortran_defs.setdefault(name, []).extend(decls)

    cpp_text = CPP_FILE.read_text()
    cpp_defs = collect_cpp_definitions(cpp_text)

    problems, checked_pairs = compare(fortran_defs, cpp_defs)

    n_interfaces = sum(len(v) for v in fortran_defs.values())
    print(f"Checked {n_interfaces} Fortran bind(C) interface declaration(s) against "
          f"{len(cpp_defs)} C++ extern \"C\" definition(s) ({checked_pairs} argument/return pair(s)).")

    if problems:
        print(f"\n{len(problems)} bind(C) boundary mismatch(es) found:")
        for p in problems:
            print(f"  {p}")
        return 1

    print("All bind(C) interfaces match their C++ definitions.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
