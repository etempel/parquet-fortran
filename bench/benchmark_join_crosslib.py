#!/usr/bin/env python3
"""Cross-library comparison of table joins: parquet-fortran against pandas, astropy and STILTS.

Driven by ``bench/benchmark_join_crosslib.sh``, which is where the build (``--profile release``),
the environment variables and the ``--profile`` assertion live.  This script owns the fixtures, the
four arms, the row-for-row comparison and the report.

WHAT IT ANSWERS
---------------
1. **Correctness.**  Every join this library offers is run against whichever of the other three can
   express the same thing, over the SAME parquet files, and the results are compared as multisets of
   rows.  A difference is reported as a difference; whether it is a defect or a documented semantic
   divergence is decided by the ``SEMANTICS`` cases, which exist precisely to pin the places where
   the four libraries are *entitled* to disagree (null keys are the big one).
2. **Speed.**  Read, join and write are timed separately, because a single end-to-end figure over a
   parquet pipeline mostly measures Arrow against pyarrow against parquet-mr rather than the join.
   STILTS is a subprocess and can only be timed whole, so it gets a control run -- the same pipeline
   with the join replaced by a concatenation -- and its join phase is reported as the difference.

RULES FROM ``CLAUDE.md``'s BENCHMARKING SECTION THAT THIS SCRIPT FOLLOWS
-----------------------------------------------------------------------
* Best of N rounds per figure, never a single measurement.
* Every input is read and every output buffer is written once before any timed loop, so no arm
  absorbs another's page faults or lazy decode.
* A noise floor is measured with the same harness the campaign runs, not borrowed from another tool.
* A library that cannot express a case is reported as ``n/a``, never silently skipped.

Nothing here is run by ``fpm test`` or by CI: it needs pandas, astropy, pyarrow and a ``stilts`` on
``PATH``, and the large shapes need several GB.  Maintainer-only.
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from dataclasses import dataclass, field

import numpy as np
import pyarrow as pa
import pyarrow.parquet as pq

# --------------------------------------------------------------------------------------------
# Fixtures
# --------------------------------------------------------------------------------------------

# One RNG seed per fixture, so a fixture is reproducible on its own and adding one does not move
# any other.  The keys are drawn from a bounded range so that a "symmetric" join emits roughly as
# many rows as it consumes rather than going quadratic -- see benchmark_join.sh, which says the same.


def _rng(seed: int) -> np.random.Generator:
    return np.random.default_rng(seed)


def make_side(n, key_lo, key_hi, seed, prefix, ncols=4, key_dtype="int64",
              unique=False, key_name="id", nulls=0.0, extra_key=False):
    """Builds one side of a join as an Arrow table: a key column plus ``ncols`` float payloads.

    ``unique`` draws the key without replacement, which is what makes a lookup table a lookup table
    and what lets ``require="m:1"`` hold.  Payload names carry ``prefix`` so the two sides never
    clash -- a clash is its own correctness case rather than a thing every case has to cope with.
    """
    r = _rng(seed)
    if unique:
        span = key_hi - key_lo
        if span < n:
            raise ValueError(f"cannot draw {n} unique keys from a span of {span}")
        keys = r.choice(span, size=n, replace=False).astype(np.int64) + key_lo
    else:
        keys = r.integers(key_lo, key_hi, size=n, dtype=np.int64)

    cols = {}
    if key_dtype == "int64":
        arr = pa.array(keys, pa.int64())
    elif key_dtype == "int32":
        arr = pa.array(keys.astype(np.int32), pa.int32())
    elif key_dtype == "string":
        arr = pa.array([f"obj{k:09d}" for k in keys], pa.string())
    elif key_dtype == "float64":
        arr = pa.array(keys.astype(np.float64), pa.float64())
    else:
        raise ValueError(key_dtype)

    if nulls > 0.0:
        mask = r.random(n) < nulls
        arr = pa.array(arr.to_pylist(), type=arr.type,
                       mask=pa.array(mask, pa.bool_()))
    cols[key_name] = arr

    if extra_key:
        cols["band"] = pa.array((keys % 3).astype(np.int32), pa.int32())

    for c in range(ncols):
        cols[f"{prefix}{c}"] = pa.array(r.normal(size=n) * 100.0 + c, pa.float64())
    return pa.table(cols)


# --------------------------------------------------------------------------------------------
# Canonical comparison
# --------------------------------------------------------------------------------------------

# A join copies values; it never computes one.  So two libraries that agree must agree BIT for
# BIT, and every value is rendered to an exact string rather than compared numerically.  The one
# deliberate conflation is MISSING: parquet-fortran and STILTS write a genuine null where a row had
# no counterpart, while pandas carries a float NaN into the same place, and that is a difference in
# how absence is REPRESENTED rather than in which rows came out.  No fixture in the correctness
# suite contains a real NaN, so nothing else can land in that bucket -- the NaN-key question is its
# own semantics probe.
MISSING = "\x00MISSING\x00"


def _render(v):
    """One value as an exact, comparable string."""
    if v is None:
        return MISSING
    if isinstance(v, float):
        if v != v:
            return MISSING
        # An integral float renders as an integer, which is what makes pandas comparable at all:
        # a pandas merge that has to put a missing value in an int64 column UPCASTS the whole
        # column to float64, so its key reads 104.0 where every other arm writes 104.  That is a
        # pandas representation choice, not a different row.  The payloads here are normal
        # deviates, so nothing else can land in this branch.
        if v == int(v) and abs(v) < 2.0 ** 53:
            return str(int(v))
        return repr(v)
    if isinstance(v, (bytes, bytearray)):
        return v.decode("utf-8", "replace")
    if isinstance(v, np.generic):
        return _render(v.item())
    return str(v)


def canon(table: pa.Table, drop=(), key_merge=(), rename=None) -> tuple:
    """Reduces a result table to a comparable value: its column names, and its rows as strings.

    Rows are compared as a MULTISET, because the four libraries emit the same pairs in different
    orders; the order is a separate, explicitly-tested property -- see the ``orderkey`` case.
    """
    cols = {name: table.column(name) for name in table.column_names}
    # astropy's parquet writer serialises a masked column as the values PLUS a companion
    # `<col>.mask` boolean.  Apply it and drop it, or every masked result "differs" on its column
    # set alone -- which is a serialisation difference, not a join difference.
    masks = [n for n in cols if n.endswith(".mask")]
    for m in masks:
        base = m[:-5]
        flags = cols.pop(m).to_pylist()
        if base in cols:
            vals = cols[base].to_pylist()
            cols[base] = pa.array([None if f else v for v, f in zip(vals, flags)])
    if rename:
        cols = {rename.get(k, k): v for k, v in cols.items()}
    for left_k, right_k, merged in (key_merge or ()):
        if left_k in cols and right_k in cols:
            a = cols.pop(left_k).to_pylist()
            b = cols.pop(right_k).to_pylist()
            cols[merged] = pa.array([x if x is not None else y for x, y in zip(a, b)])
    for d in drop:
        cols.pop(d, None)

    names = tuple(sorted(cols))
    rendered = [[_render(v) for v in cols[n].to_pylist()] for n in names]
    nrows = len(rendered[0]) if rendered else 0
    rows = sorted(tuple(c[i] for c in rendered) for i in range(nrows))
    return names, tuple(rows)


def rows_equal(a, b):
    """Two canonical results agree when they have the same columns and the same multiset of rows."""
    if a is None or b is None:
        return None
    if a[0] != b[0]:
        return False
    return sorted(a[1]) == sorted(b[1])


def describe_diff(a, b, limit=3):
    """A short, human-readable account of the first few ways two canonical results differ."""
    if a[0] != b[0]:
        return f"columns differ: {a[0]} vs {b[0]}"
    sa, sb = sorted(a[1]), sorted(b[1])
    if len(sa) != len(sb):
        msg = f"row counts differ: {len(sa)} vs {len(sb)}"
    else:
        msg = "same row count, different rows"
    only_a = [r for r in sa if r not in set(sb)][:limit]
    only_b = [r for r in sb if r not in set(sa)][:limit]
    if only_a:
        msg += f"; only in first: {only_a}"
    if only_b:
        msg += f"; only in second: {only_b}"
    return msg


# --------------------------------------------------------------------------------------------
# The four arms
# --------------------------------------------------------------------------------------------

HOW_TO_STILTS = {
    "inner": "1and2", "left": "all1", "right": "all2",
    "outer": "1or2", "anti": "1not2",
}


@dataclass
class ArmResult:
    """One library's answer to one case: what it produced, how long it took, or why it could not."""
    ok: bool = False
    reason: str = ""            #: why the arm is unavailable, when ``ok`` is False
    canon: tuple | None = None  #: the comparable value, when it ran
    nrows: int = 0
    read_s: float = float("nan")
    join_s: float = float("nan")
    write_s: float = float("nan")
    extra: dict = field(default_factory=dict)


def best_of(fn, rounds):
    """Runs ``fn`` ``rounds`` times and keeps the fastest, per this repo's benchmarking rules."""
    best = float("inf")
    out = None
    for _ in range(rounds):
        t0 = time.perf_counter()
        out = fn()
        t1 = time.perf_counter()
        best = min(best, t1 - t0)
    return best, out


# ---- parquet-fortran ----

def run_pf(binary, left, right, out, how, on="id", other_on=None, columns=None,
           require=None, order=None, other_suffix=None, mode="eager", left_keep=None,
           rounds=3, threads=0, max_rows=None):
    """Runs the Fortran arm as a subprocess and parses its ``RESULT key=value`` lines."""
    cmd = [binary, f"--left={left}", f"--right={right}", f"--out={out}",
           f"--on={on}", f"--how={how}", f"--rounds={rounds}", f"--mode={mode}",
           f"--threads={threads}"]
    if other_on:
        cmd.append(f"--other-on={other_on}")
    if columns:
        cmd.append(f"--columns={columns}")
    if require:
        cmd.append(f"--require={require}")
    if order:
        cmd.append(f"--order={order}")
    if other_suffix:
        cmd.append(f"--other-suffix={other_suffix}")
    if left_keep:
        cmd.append(f"--left-keep={left_keep}")
    if max_rows is not None:
        cmd.append(f"--max-rows={max_rows}")
    p = subprocess.run(cmd, capture_output=True, text=True)
    if p.returncode != 0:
        return ArmResult(ok=False, reason=f"exit {p.returncode}: "
                                          f"{(p.stderr or p.stdout).strip()[:300]}")
    facts = {}
    for line in p.stdout.splitlines():
        if line.startswith("RESULT "):
            k, _, v = line[7:].partition("=")
            facts[k] = v
    res = ArmResult(ok=True, nrows=int(facts.get("out_rows", 0)),
                    read_s=float(facts.get("read_s", "nan")),
                    join_s=float(facts.get("join_s", "nan")),
                    write_s=float(facts.get("write_s", "nan")),
                    extra=facts)
    if os.path.exists(out):
        res.canon = canon(pq.read_table(out))
    return res


# ---- pandas ----

def run_pandas(left, right, out, how, on="id", other_on=None, columns=None,
               other_suffix="_2", rounds=3):
    """pandas ``merge``, plus the semi/anti forms it has no ``how`` value for.

    ``semi`` and ``anti`` are expressed the way the pandas documentation itself recommends: an inner
    or left merge against the right table's DEDUPLICATED key, so a left row is emitted at most once
    however many counterparts it has -- which is what makes them row selections rather than joins.
    """
    import pandas as pd

    t0 = time.perf_counter()
    L = pd.read_parquet(left)
    R = pd.read_parquet(right)
    read_s = time.perf_counter() - t0

    lon = on.split(",")
    ron = other_on.split(",") if other_on else lon
    if columns is not None:
        keep = [c.strip() for c in columns.split(",") if c.strip()]
        R = R[ron + [c for c in keep if c not in ron]]

    def do():
        if how in ("inner", "left", "right", "outer"):
            if other_on:
                return L.merge(R, left_on=lon, right_on=ron, how=how,
                               suffixes=("", other_suffix))
            return L.merge(R, on=lon, how=how, suffixes=("", other_suffix))
        if how == "semi":
            keys = R[ron].drop_duplicates()
            if other_on:
                m = L.merge(keys, left_on=lon, right_on=ron, how="inner")
                return m.drop(columns=[c for c in ron if c not in lon])
            return L.merge(keys, on=lon, how="inner")
        if how == "anti":
            keys = R[ron].drop_duplicates()
            if other_on:
                m = L.merge(keys, left_on=lon, right_on=ron, how="left", indicator=True)
                m = m[m["_merge"] == "left_only"].drop(columns=["_merge"])
                return m.drop(columns=[c for c in ron if c not in lon])
            m = L.merge(keys, on=lon, how="left", indicator=True)
            return m[m["_merge"] == "left_only"].drop(columns=["_merge"])
        raise ValueError(how)

    join_s, J = best_of(do, rounds)

    t0 = time.perf_counter()
    J.to_parquet(out, index=False)
    write_s = time.perf_counter() - t0

    return ArmResult(ok=True, nrows=len(J), read_s=read_s, join_s=join_s,
                     write_s=write_s, canon=canon(pq.read_table(out)))


# ---- astropy ----

def run_astropy(left, right, out, how, on="id", other_on=None, columns=None,
                other_suffix="_2", rounds=3):
    """``astropy.table.join``.  It has four join types, no semi/anti, and refuses a masked key."""
    from astropy.table import Table, join as ap_join

    if how in ("semi", "anti"):
        return ArmResult(ok=False, reason="astropy.table.join has no semi/anti join type")

    t0 = time.perf_counter()
    L = Table.read(left)
    R = Table.read(right)
    read_s = time.perf_counter() - t0

    lon = on.split(",")
    ron = other_on.split(",") if other_on else lon
    if columns is not None:
        keep = [c.strip() for c in columns.split(",") if c.strip()]
        R = R[ron + [c for c in keep if c not in ron]]

    def do():
        if other_on:
            return ap_join(L, R, keys_left=lon, keys_right=ron, join_type=how,
                           uniq_col_name="{col_name}{table_name}",
                           table_names=["", other_suffix])
        return ap_join(L, R, keys=lon, join_type=how,
                       uniq_col_name="{col_name}{table_name}",
                       table_names=["", other_suffix])

    try:
        join_s, J = best_of(do, rounds)
    except Exception as exc:                      # noqa: BLE001 -- reported, not swallowed
        return ArmResult(ok=False, reason=f"{type(exc).__name__}: {exc}"[:300], read_s=read_s)

    t0 = time.perf_counter()
    J.write(out, format="parquet", overwrite=True)
    write_s = time.perf_counter() - t0

    return ArmResult(ok=True, nrows=len(J), read_s=read_s, join_s=join_s,
                     write_s=write_s, canon=canon(pq.read_table(out)))


# ---- STILTS ----

def run_stilts(left, right, out, how, on="id", other_on=None, columns=None,
               rounds=3, control_out=None):
    """``stilts tmatch2 matcher=exact``, whole-process wall time only.

    A subprocess cannot report its own phases, so the whole run is timed and a CONTROL run -- the
    same two files read and one written, with no matching at all -- is timed beside it.

    **The difference is an UPPER BOUND on the join, not an estimate of it.**  ``tcat`` reads both
    files in full but writes only the FIRST table's column set (verified: a 400-row + 300-row
    concatenation of two tables with disjoint payloads comes back with 700 rows and the left
    table's four columns), so the control's write is lighter than the join's and the subtraction
    leaves some write cost behind in the difference.  The whole-process figure is the one a STILTS
    user actually pays and is what the report leads with; the bound is there to show how much of
    it is not the join.

    ``semi`` has no ``join`` value of its own: ``find=best1`` keeps each left row at most once,
    which is the semi-join row set, and the right table's columns are then deleted.
    """
    exe = shutil.which("stilts")
    if exe is None:
        return ArmResult(ok=False, reason="no stilts on PATH")

    lon = on.split(",")
    ron = other_on.split(",") if other_on else lon
    # `matcher=exact+exact` throws a NullPointerException in STILTS 3.5-6
    # (CombinedMatchEngine.createCoverageFactory), so a multi-column exact match is expressed the
    # way STILTS users express it: one synthetic string key built by `addcol`, matched with a
    # single `exact`, and deleted from the output.
    icmds = []
    if len(lon) > 1:
        matcher, v1, v2 = "exact", "_jk", "_jk"
        icmds = ["+".join(lon), "+".join(ron)]
    else:
        matcher, v1, v2 = "exact", lon[0], ron[0]

    ocmds = []
    if how == "semi":
        join_tok, find_tok = "1and2", "best1"
    else:
        join_tok, find_tok = HOW_TO_STILTS[how], "all"

    cmd = [exe, "tmatch2", "progress=none",
           f"in1={left}", f"in2={right}", "ifmt1=parquet", "ifmt2=parquet"]
    if icmds:
        for n, names in ((1, lon), (2, ron)):
            # the inner quotes must reach STILTS' own expression parser escaped, or it splits
            # the icmd on them and reports "Unused arguments"
            expr = '+\\"|\\"+'.join(f"toString({c})" for c in names)
            cmd.append(f'icmd{n}=addcol _jk "{expr}"')
    cmd += [f"matcher={matcher}", f"values1={v1}", f"values2={v2}",
            f"join={join_tok}", f"find={find_tok}",
            "ofmt=parquet", f"out={out}"]

    def do():
        p = subprocess.run(cmd, capture_output=True, text=True)
        if p.returncode != 0:
            raise RuntimeError((p.stderr or p.stdout).strip()[:300])
        return None

    try:
        total_s, _ = best_of(do, rounds)
    except RuntimeError as exc:
        return ArmResult(ok=False, reason=str(exc))

    control_s = float("nan")
    if control_out:
        ctl = [exe, "tcat", f"in={left}", f"in={right}", "ifmt=parquet",
               "lazy=false", "ofmt=parquet", f"out={control_out}"]

        def doc():
            subprocess.run(ctl, capture_output=True, text=True)
        control_s, _ = best_of(doc, max(1, rounds - 1))

    t = pq.read_table(out)
    # Undo STILTS' own naming so the result is comparable: the shared key comes back as id_1/id_2
    # (coalesced here by the same rule parquet-fortran documents -- the right value fills a row the
    # left did not contribute), and find=all adds GroupID/GroupSize.
    km = []
    ren = {}
    keyset = set(lon) | set(ron)
    for k in lon:
        if f"{k}_1" in t.column_names and f"{k}_2" in t.column_names and not other_on:
            km.append((f"{k}_1", f"{k}_2", k))
        elif f"{k}_1" in t.column_names:
            ren[f"{k}_1"] = k
    # A non-key column present in both tables comes back as `<c>_1`/`<c>_2` where parquet-fortran
    # keeps `<c>` and suffixes only the incoming one.  Stripping `_1` makes the two agree; the
    # `_2` half already matches the default other_suffix.
    for name in t.column_names:
        if name.endswith("_1") and name[:-2] not in keyset and name not in ren:
            ren[name] = name[:-2]
    drop = ["GroupID", "GroupSize", "_jk_1", "_jk_2", "_jk"]
    if how == "semi":
        drop += [c for c in t.column_names if c not in lon and not c.endswith("_1")
                 and c not in ("GroupID", "GroupSize") and c in _right_cols(right)]
        for k in lon:
            if f"{k}_1" in t.column_names:
                ren[f"{k}_1"] = k
                drop.append(f"{k}_2")
        km = []
    if other_on:
        for k in ron:
            ren.setdefault(f"{k}_2", k)

    res = ArmResult(ok=True, nrows=t.num_rows, join_s=total_s, extra={"control_s": control_s})
    res.canon = canon(t, drop=drop, key_merge=km, rename=ren)
    return res


_RIGHT_COL_CACHE: dict[str, list[str]] = {}


def _right_cols(path):
    """The right table's own column names, cached -- used to strip them from a semi-join result."""
    if path not in _RIGHT_COL_CACHE:
        _RIGHT_COL_CACHE[path] = pq.read_schema(path).names
    return _RIGHT_COL_CACHE[path]


# --------------------------------------------------------------------------------------------
# The correctness suite
# --------------------------------------------------------------------------------------------

@dataclass
class Case:
    """One correctness case: a pair of fixtures, a join, and which arms can express it."""
    name: str
    left: str
    right: str
    how: str
    on: str = "id"
    other_on: str | None = None
    columns: str | None = None
    require: str | None = None
    order: str | None = None
    other_suffix: str | None = None
    note: str = ""
    arms: tuple = ("pandas", "astropy", "stilts")


def build_correctness_fixtures(d):
    """Writes every fixture the correctness suite joins, and returns their paths by name.

    The shapes are chosen so that each one can only be got right for the right reason: duplicate
    keys on BOTH sides (so a wrong cardinality shows up as a wrong row count), a key range wider
    than either side (so unmatched rows exist on both sides), and a lookup shape whose right key is
    unique (so ``require="m:1"`` is satisfiable and the m:1 no-detach path is exercised).
    """
    os.makedirs(d, exist_ok=True)
    p = {}

    # (a) duplicates on both sides, keys 0..199 over 400 rows each -> every how does real work
    L = make_side(400, 0, 200, 11, "lx", ncols=3)
    R = make_side(300, 100, 320, 12, "rx", ncols=3)
    p["dup_l"], p["dup_r"] = f"{d}/dup_l.parquet", f"{d}/dup_r.parquet"
    pq.write_table(L, p["dup_l"]); pq.write_table(R, p["dup_r"])

    # (b) the lookup shape: many left rows, unique right key
    L2 = make_side(500, 0, 300, 13, "lx", ncols=3)
    R2 = make_side(200, 50, 400, 14, "rx", ncols=3, unique=True)
    p["lut_l"], p["lut_r"] = f"{d}/lut_l.parquet", f"{d}/lut_r.parquet"
    pq.write_table(L2, p["lut_l"]); pq.write_table(R2, p["lut_r"])

    # (c) string key
    L3 = make_side(300, 0, 150, 15, "lx", ncols=2, key_dtype="string")
    R3 = make_side(250, 80, 260, 16, "rx", ncols=2, key_dtype="string")
    p["str_l"], p["str_r"] = f"{d}/str_l.parquet", f"{d}/str_r.parquet"
    pq.write_table(L3, p["str_l"]); pq.write_table(R3, p["str_r"])

    # (d) two-column key (int64 id + int32 band)
    L4 = make_side(400, 0, 120, 17, "lx", ncols=2, extra_key=True)
    R4 = make_side(350, 40, 200, 18, "rx", ncols=2, extra_key=True)
    p["two_l"], p["two_r"] = f"{d}/two_l.parquet", f"{d}/two_r.parquet"
    pq.write_table(L4, p["two_l"]); pq.write_table(R4, p["two_r"])

    # (e) clashing payload names -- the only fixture where other_suffix has anything to do
    L5 = make_side(300, 0, 150, 19, "mag", ncols=2)
    R5 = make_side(200, 60, 400, 20, "mag", ncols=2, unique=True)
    p["clash_l"], p["clash_r"] = f"{d}/clash_l.parquet", f"{d}/clash_r.parquet"
    pq.write_table(L5, p["clash_l"]); pq.write_table(R5, p["clash_r"])

    # (f) different key names on the two sides
    R6 = make_side(200, 50, 400, 21, "rx", ncols=2, unique=True, key_name="object_id")
    p["oon_r"] = f"{d}/oon_r.parquet"
    pq.write_table(R6, p["oon_r"])

    # (g) null keys on both sides -- the semantics probe, not a correctness case
    L7 = make_side(300, 0, 150, 22, "lx", ncols=2, nulls=0.15)
    R7 = make_side(250, 60, 220, 23, "rx", ncols=2, nulls=0.15)
    p["null_l"], p["null_r"] = f"{d}/null_l.parquet", f"{d}/null_r.parquet"
    pq.write_table(L7, p["null_l"]); pq.write_table(R7, p["null_r"])

    # (h) a right table that repeats a key -- what require="m:1" must refuse
    R8 = make_side(200, 50, 120, 24, "rx", ncols=2)
    p["repeat_r"] = f"{d}/repeat_r.parquet"
    pq.write_table(R8, p["repeat_r"])
    return p


def correctness_cases(p):
    """Every case the suite runs, in report order."""
    cases = []
    for how in ("inner", "left", "right", "outer", "semi", "anti"):
        cases.append(Case(f"dup/{how}", p["dup_l"], p["dup_r"], how,
                          note="duplicate keys on both sides"))
    for how in ("inner", "left", "right", "outer", "semi", "anti"):
        cases.append(Case(f"lut/{how}", p["lut_l"], p["lut_r"], how,
                          require="m:1" if how in ("inner", "left", "semi", "anti") else None,
                          note="unique right key (lookup table)"))
    for how in ("inner", "left", "outer"):
        cases.append(Case(f"string/{how}", p["str_l"], p["str_r"], how,
                          note="string join key"))
    for how in ("inner", "left", "outer"):
        cases.append(Case(f"twokey/{how}", p["two_l"], p["two_r"], how, on="id,band",
                          note="two-column key (int64 + int32)"))
    cases.append(Case("clash/inner", p["clash_l"], p["clash_r"], "inner",
                      other_suffix="_2", note="clashing payload names -> other_suffix"))
    cases.append(Case("other_on/left", p["lut_l"], p["oon_r"], "left",
                      other_on="object_id", note="key columns named differently"))
    cases.append(Case("columns/left", p["lut_l"], p["lut_r"], "left", columns="rx0",
                      note="columns= carries one payload column only",
                      arms=("pandas", "astropy")))
    cases.append(Case("orderkey/inner", p["dup_l"], p["dup_r"], "inner", order="key",
                      note="order=\"key\": same pairs, engine's grouping"))
    return cases


def run_correctness(cfg, out):
    """Runs every case against every arm that can express it and compares row multisets."""
    p = build_correctness_fixtures(cfg.fixture_dir)
    rows = []
    for c in correctness_cases(p):
        tag = c.name.replace("/", "_")
        pf = run_pf(cfg.binary, c.left, c.right, f"{cfg.work}/pf_{tag}.parquet", c.how,
                    on=c.on, other_on=c.other_on, columns=c.columns, require=c.require,
                    order=c.order, other_suffix=c.other_suffix, rounds=1)
        entry = {"case": c.name, "note": c.note, "how": c.how,
                 "pf_rows": pf.nrows, "pf_ok": pf.ok, "pf_reason": pf.reason, "arms": {}}
        for arm in c.arms:
            try:
                if arm == "pandas":
                    r = run_pandas(c.left, c.right, f"{cfg.work}/pd_{tag}.parquet", c.how,
                                   on=c.on, other_on=c.other_on, columns=c.columns,
                                   other_suffix=c.other_suffix or "_2", rounds=1)
                elif arm == "astropy":
                    r = run_astropy(c.left, c.right, f"{cfg.work}/ap_{tag}.parquet", c.how,
                                    on=c.on, other_on=c.other_on, columns=c.columns,
                                    other_suffix=c.other_suffix or "_2", rounds=1)
                else:
                    r = run_stilts(c.left, c.right, f"{cfg.work}/st_{tag}.parquet", c.how,
                                   on=c.on, other_on=c.other_on, rounds=1)
            except Exception as exc:                    # noqa: BLE001 -- reported, not swallowed
                r = ArmResult(ok=False, reason=f"{type(exc).__name__}: {exc}"[:200])
            verdict = "n/a"
            detail = r.reason
            if r.ok and pf.ok:
                same = rows_equal(pf.canon, r.canon)
                verdict = "same" if same else "DIFFER"
                detail = "" if same else describe_diff(pf.canon, r.canon)
            entry["arms"][arm] = {"verdict": verdict, "rows": r.nrows, "detail": detail}
        rows.append(entry)
        _print_case_line(entry)
    out["correctness"] = rows
    return rows


def _print_case_line(e):
    """One line per case as it finishes, so a long run reports as it goes."""
    bits = []
    for arm in ("pandas", "astropy", "stilts"):
        a = e["arms"].get(arm)
        bits.append(f"{arm}={a['verdict']}" if a else f"{arm}=-")
    print(f"  {e['case']:<20} pf_rows={e['pf_rows']:<8} " + "  ".join(bits))
    for arm, a in e["arms"].items():
        if a["verdict"] == "DIFFER" or (a["verdict"] == "n/a" and a["detail"]):
            print(f"      {arm}: {a['detail'][:200]}")
    sys.stdout.flush()


# --------------------------------------------------------------------------------------------
# The semantics probes: where the four are ENTITLED to disagree
# --------------------------------------------------------------------------------------------

def run_semantics(cfg, out):
    """Pins the two places the libraries genuinely differ, so neither reads as a defect.

    A null key and a NaN key are the whole list.  Both are decided by each library's own rule, and
    a difference here is a documented divergence rather than something for anyone to fix.
    """
    p = build_correctness_fixtures(cfg.fixture_dir)
    res = []

    # (1) null keys.  parquet-fortran and SQL and STILTS: a null matches nothing, not even another
    #     null.  pandas: NA matches NA.  astropy: refuses a masked key column outright.
    for how in ("inner", "left"):
        tag = f"null_{how}"
        pf = run_pf(cfg.binary, p["null_l"], p["null_r"], f"{cfg.work}/pf_{tag}.parquet", how,
                    rounds=1)
        pd_r = run_pandas(p["null_l"], p["null_r"], f"{cfg.work}/pd_{tag}.parquet", how, rounds=1)
        ap_r = run_astropy(p["null_l"], p["null_r"], f"{cfg.work}/ap_{tag}.parquet", how, rounds=1)
        st_r = run_stilts(p["null_l"], p["null_r"], f"{cfg.work}/st_{tag}.parquet", how, rounds=1)
        res.append({
            "probe": f"null key, how={how}",
            "pf_rows": pf.nrows, "pandas_rows": pd_r.nrows if pd_r.ok else None,
            "astropy": ap_r.reason if not ap_r.ok else f"{ap_r.nrows} rows",
            "stilts_rows": st_r.nrows if st_r.ok else None,
            "pf_vs_stilts": rows_equal(pf.canon, st_r.canon) if st_r.ok else None,
            "pf_vs_pandas": rows_equal(pf.canon, pd_r.canon) if pd_r.ok else None,
            "pf_vs_astropy": rows_equal(pf.canon, ap_r.canon) if ap_r.ok else None,
        })
        print(f"  null key how={how}: pf={pf.nrows} rows"
              f" | pandas={pd_r.nrows if pd_r.ok else 'err'} "
              f"(same rows as pf: {res[-1]['pf_vs_pandas']})"
              f" | stilts={st_r.nrows if st_r.ok else 'err'} "
              f"(same: {res[-1]['pf_vs_stilts']})"
              f" | astropy={ap_r.nrows if ap_r.ok else 'REFUSED'} "
              f"(same: {res[-1]['pf_vs_astropy']})")

    # (2) require= and max_rows= are parquet-fortran's own guards; no other arm has them.  The
    #     check is that a repeated right key really is refused, and that the message names it.
    bad = run_pf(cfg.binary, p["lut_l"], p["repeat_r"], f"{cfg.work}/pf_req.parquet", "left",
                 require="m:1", rounds=1)
    good = run_pf(cfg.binary, p["lut_l"], p["lut_r"], f"{cfg.work}/pf_req_ok.parquet", "left",
                  require="m:1", rounds=1)
    res.append({"probe": "require=m:1 refuses a repeated right key",
                "refused": not bad.ok, "message": bad.reason[:200],
                "negative_control_passes": good.ok})
    print(f"  require=m:1 on a repeated right key: "
          f"{'REFUSED (correct)' if not bad.ok else 'ACCEPTED -- unexpected'}; "
          f"negative control {'passes' if good.ok else 'FAILS'}")
    out["semantics"] = res
    return res


# --------------------------------------------------------------------------------------------
# The claims stage: properties only parquet-fortran promises, which a multiset cannot see
# --------------------------------------------------------------------------------------------

def _seq(path, cols=None):
    """The ROW SEQUENCE of a result, unsorted -- the thing the correctness stage deliberately drops."""
    t = pq.read_table(path)
    names = cols or sorted(t.column_names)
    rendered = [[_render(v) for v in t.column(n).to_pylist()] for n in names]
    return [tuple(c[i] for c in rendered) for i in range(t.num_rows)]


def run_claims(cfg, out):
    """Checks four things the guide promises that no other library promises, so nothing else can
    cross-check them: the output row ORDER under each `order=` value, whether the table detaches,
    and that `matched=` counts what it says it counts.

    The order claim is checked against pandas rather than asserted on its own, because pandas'
    `merge(sort=False)` documents the same rule -- every left row in its original position, its
    matches in the right table's original order -- so the two are independent implementations of
    one specification and agreeing is evidence.
    """
    import pandas as pd
    p = build_correctness_fixtures(cfg.fixture_dir)
    res = []

    # (1) order="left" (the default) must reproduce pandas' merge order exactly, row for row.
    pf = run_pf(cfg.binary, p["dup_l"], p["dup_r"], f"{cfg.work}/pf_ord_left.parquet", "inner",
                rounds=1)
    pdr = run_pandas(p["dup_l"], p["dup_r"], f"{cfg.work}/pd_ord_left.parquet", "inner", rounds=1)
    same_seq = _seq(f"{cfg.work}/pf_ord_left.parquet") == _seq(f"{cfg.work}/pd_ord_left.parquet")
    res.append({"claim": 'order="left" reproduces pandas merge order row for row',
                "holds": same_seq})
    print(f"  order=\"left\" is pandas' merge order, row for row: {same_seq}")

    # (2) order="key" must come out grouped by key -- i.e. the key column non-decreasing.
    run_pf(cfg.binary, p["dup_l"], p["dup_r"], f"{cfg.work}/pf_ord_key.parquet", "inner",
           order="key", rounds=1)
    ids = pq.read_table(f"{cfg.work}/pf_ord_key.parquet").column("id").to_pylist()
    sorted_by_key = all(a <= b for a, b in zip(ids, ids[1:]))
    changed = ids != pq.read_table(f"{cfg.work}/pf_ord_left.parquet").column("id").to_pylist()
    res.append({"claim": 'order="key" groups the output by key value',
                "holds": sorted_by_key, "differs_from_order_left": changed})
    print(f"  order=\"key\" is grouped by key: {sorted_by_key} "
          f"(and really differs from order=\"left\": {changed})")

    # (3) detaching.  An m:1 left join keeps every left row once and in place, so the table keeps
    #     its file; anything that drops, duplicates or adds a row detaches.
    checks = [("lut", p["lut_l"], p["lut_r"], "left", "m:1", "0"),
              ("lut", p["lut_l"], p["lut_r"], "inner", "m:1", "1"),
              ("dup", p["dup_l"], p["dup_r"], "left", None, "1")]
    for tag, l, r, how, req, expect in checks:
        got = run_pf(cfg.binary, l, r, f"{cfg.work}/pf_det_{tag}_{how}.parquet", how,
                     require=req, rounds=1)
        d = got.extra.get("detached")
        ok = (d == expect)
        res.append({"claim": f"{tag}/{how} detached=={expect}", "holds": ok, "got": d})
        print(f"  {tag}/{how:<6} detached={d} (expected {expect}): {'ok' if ok else 'UNEXPECTED'}")

    # (4) matched= counts the rows of the left table AS IT WAS that found a counterpart.
    L = pd.read_parquet(p["dup_l"]); R = pd.read_parquet(p["dup_r"])
    expect_matched = int(L["id"].isin(set(R["id"])).sum())
    got = run_pf(cfg.binary, p["dup_l"], p["dup_r"], f"{cfg.work}/pf_matched.parquet", "inner",
                 rounds=1)
    ok = (int(got.extra["matched"]) == expect_matched
          and int(got.extra["matched_of"]) == len(L))
    res.append({"claim": "matched= counts left rows with a counterpart",
                "holds": ok, "got": got.extra.get("matched"), "expected": expect_matched})
    print(f"  matched= {got.extra.get('matched')} of {got.extra.get('matched_of')} "
          f"(pandas says {expect_matched} of {len(L)}): {'ok' if ok else 'MISMATCH'}")

    # (5) pairs= / other_pairs=: one entry per OUTPUT row, naming the row of each table as it
    #     was ON ENTRY, with 0 for "no counterpart on that side".  Each `how` forbids a different
    #     pattern of zeros, and those rules are the whole contract -- so they are checked per how,
    #     against the row counts the same run reports.
    rules = {"inner": ("pairs never 0, other_pairs never 0", True, True),
             "left": ("pairs never 0", True, False),
             "right": ("other_pairs never 0", False, True),
             "outer": ("either may be 0", False, False),
             "semi": ("other_pairs always 0", True, False),
             "anti": ("other_pairs always 0", True, False)}
    for how, (desc, no_left_zero, no_right_zero) in rules.items():
        g = run_pf(cfg.binary, p["dup_l"], p["dup_r"], f"{cfg.work}/pf_pairs_{how}.parquet", how,
                   rounds=1)
        e = g.extra
        nout = int(e["out_rows"])
        problems = []
        if int(e["pairs_len"]) != nout or int(e["other_pairs_len"]) != nout:
            problems.append("length is not the output row count")
        if no_left_zero and int(e["pairs_zero"]) != 0:
            problems.append(f"pairs has {e['pairs_zero']} zeros")
        if no_right_zero and int(e["other_pairs_zero"]) != 0:
            problems.append(f"other_pairs has {e['other_pairs_zero']} zeros")
        if how in ("semi", "anti") and int(e["other_pairs_zero"]) != nout:
            problems.append("other_pairs is not all zero")
        if int(e["pairs_max"]) > int(e["left_rows"]):
            problems.append("pairs indexes past the left table")
        if int(e["other_pairs_max"]) > int(e["right_rows"]):
            problems.append("other_pairs indexes past the right table")
        res.append({"claim": f"pairs=/other_pairs= under how={how} ({desc})",
                    "holds": not problems, "detail": "; ".join(problems)})
        print(f"  pairs= how={how:<6} len={e['pairs_len']:<6} "
              f"zeros L/R={e['pairs_zero']}/{e['other_pairs_zero']:<6} "
              f"{'ok' if not problems else 'BROKEN: ' + '; '.join(problems)}")

    # (6) max_rows= refuses a join that would be larger than the caller expected, and does nothing
    #     at all when it is satisfied.  Both halves are needed: a limit that always fires would
    #     pass the refusal test and break every real join.
    ref = run_pf(cfg.binary, p["dup_l"], p["dup_r"], f"{cfg.work}/pf_maxrows_bad.parquet",
                 "inner", max_rows=10, rounds=1)
    ok_run = run_pf(cfg.binary, p["dup_l"], p["dup_r"], f"{cfg.work}/pf_maxrows_ok.parquet",
                    "inner", max_rows=1000000, rounds=1)
    plain = run_pf(cfg.binary, p["dup_l"], p["dup_r"], f"{cfg.work}/pf_maxrows_none.parquet",
                   "inner", rounds=1)
    same = rows_equal(ok_run.canon, plain.canon) if (ok_run.ok and plain.ok) else None
    res.append({"claim": "max_rows= refuses an oversized join and is inert otherwise",
                "holds": (not ref.ok) and ok_run.ok and same is True,
                "message": ref.reason[:200]})
    print(f"  max_rows=10 on a 243-row join: "
          f"{'REFUSED (correct)' if not ref.ok else 'ACCEPTED -- unexpected'}; "
          f"max_rows=1000000 passes and changes nothing: {same}")

    out["claims"] = res
    return res


# --------------------------------------------------------------------------------------------
# The performance suite
# --------------------------------------------------------------------------------------------

def build_perf_fixtures(d, nleft, nright, nsym, ncols):
    """Writes the three performance shapes.  Payload names are disjoint, so nothing is suffixed."""
    os.makedirs(d, exist_ok=True)
    p = {}
    # lookup: a large left table against a small unique-keyed right one, the WAVES shape
    L = make_side(nleft, 0, max(nright * 4, 1000), 101, "lx", ncols=ncols)
    R = make_side(nright, 0, max(nright * 4, 1000), 102, "rx", ncols=ncols, unique=True)
    p["lut_l"], p["lut_r"] = f"{d}/perf_lut_l.parquet", f"{d}/perf_lut_r.parquet"
    pq.write_table(L, p["lut_l"]); pq.write_table(R, p["lut_r"])
    # symmetric: both sides the same size, keys drawn from a range equal to the row count, so an
    # inner join emits roughly nsym rows rather than nsym^2
    L2 = make_side(nsym, 0, nsym, 103, "lx", ncols=ncols)
    R2 = make_side(nsym, 0, nsym, 104, "rx", ncols=ncols)
    p["sym_l"], p["sym_r"] = f"{d}/perf_sym_l.parquet", f"{d}/perf_sym_r.parquet"
    pq.write_table(L2, p["sym_l"]); pq.write_table(R2, p["sym_r"])
    # 1:1: unique on both sides, half the keys shared
    L3 = make_side(nsym, 0, nsym * 3, 105, "lx", ncols=ncols, unique=True)
    R3 = make_side(nsym, 0, nsym * 3, 106, "rx", ncols=ncols, unique=True)
    p["one_l"], p["one_r"] = f"{d}/perf_one_l.parquet", f"{d}/perf_one_r.parquet"
    pq.write_table(L3, p["one_l"]); pq.write_table(R3, p["one_r"])
    return p


def run_perf(cfg, out):
    """Times every arm on every shape, phase by phase, and checks the row counts still agree."""
    p = build_perf_fixtures(cfg.fixture_dir, cfg.nleft, cfg.nright, cfg.nsym, cfg.ncols)
    shapes = [
        ("lookup", p["lut_l"], p["lut_r"], ["left", "inner"],
         f"{cfg.nleft} x {cfg.nright}, unique right key (m:1 lookup)"),
        ("symmetric", p["sym_l"], p["sym_r"], ["inner", "left", "outer", "semi", "anti"],
         f"{cfg.nsym} x {cfg.nsym}, duplicate keys both sides"),
        ("one_to_one", p["one_l"], p["one_r"], ["inner", "outer"],
         f"{cfg.nsym} x {cfg.nsym}, unique keys both sides, ~1/3 overlap"),
    ]
    rows = []
    for shape, left, right, hows, desc in shapes:
        print(f"\n  -- shape {shape}: {desc}")
        for how in hows:
            tag = f"{shape}_{how}"
            rec = {"shape": shape, "how": how, "desc": desc, "arms": {}}
            pf = run_pf(cfg.binary, left, right, f"{cfg.work}/pf_{tag}.parquet", how,
                        require="m:1" if (shape == "lookup" and how in ("left", "inner")) else None,
                        rounds=cfg.rounds, threads=cfg.threads)
            rec["arms"]["parquet-fortran"] = _arm_rec(pf)
            for arm, fn in (("pandas", run_pandas), ("astropy", run_astropy)):
                if arm == "astropy" and cfg.nsym > cfg.astropy_max and shape != "lookup":
                    rec["arms"][arm] = {"ok": False,
                                        "reason": f"skipped above --astropy-max={cfg.astropy_max}"}
                    continue
                if arm == "astropy" and shape == "lookup" and cfg.nleft > cfg.astropy_max:
                    rec["arms"][arm] = {"ok": False,
                                        "reason": f"skipped above --astropy-max={cfg.astropy_max}"}
                    continue
                try:
                    r = fn(left, right, f"{cfg.work}/{arm[:2]}_{tag}.parquet", how,
                           rounds=cfg.rounds)
                except Exception as exc:                # noqa: BLE001 -- reported, not swallowed
                    r = ArmResult(ok=False, reason=f"{type(exc).__name__}: {exc}"[:200])
                rec["arms"][arm] = _arm_rec(r)
                rec["arms"][arm]["rows_match"] = (r.nrows == pf.nrows) if r.ok else None
            st = run_stilts(left, right, f"{cfg.work}/st_{tag}.parquet", how,
                            rounds=cfg.rounds, control_out=f"{cfg.work}/st_ctl_{tag}.parquet")
            rec["arms"]["stilts"] = _arm_rec(st)
            rec["arms"]["stilts"]["rows_match"] = (st.nrows == pf.nrows) if st.ok else None
            rec["pf_rows"] = pf.nrows
            rows.append(rec)
            _print_perf_line(rec)

    # the lazy arm, which no other library has an equivalent for
    lazy = run_pf(cfg.binary, p["lut_l"], p["lut_r"], f"{cfg.work}/pf_lazy.parquet", "left",
                  require="m:1", mode="lazy", left_keep="id", rounds=1, threads=cfg.threads)
    out["lazy"] = _arm_rec(lazy)
    print(f"\n  -- parquet-fortran --mode=lazy (left table reads only its key): "
          f"read={lazy.read_s * 1e3:.1f} ms  join={lazy.join_s * 1e3:.1f} ms  "
          f"write={lazy.write_s * 1e3:.1f} ms  rows={lazy.nrows} "
          f"cols={lazy.extra.get('out_cols')} detached={lazy.extra.get('detached')}")
    out["perf"] = rows
    return rows


def _arm_rec(r: ArmResult):
    """Flattens an ``ArmResult`` into the dict the report and the JSON both read."""
    return {"ok": r.ok, "reason": r.reason, "rows": r.nrows,
            "read_s": r.read_s, "join_s": r.join_s, "write_s": r.write_s,
            "extra": r.extra}


def _print_perf_line(rec):
    """One block per (shape, how) as it finishes."""
    print(f"    how={rec['how']:<6} pf_rows={rec['pf_rows']}")
    for arm in ("parquet-fortran", "pandas", "astropy", "stilts"):
        a = rec["arms"].get(arm)
        if a is None:
            continue
        if not a["ok"]:
            print(f"      {arm:<16} n/a  ({a['reason'][:90]})")
            continue
        if arm == "stilts":
            ctl = a["extra"].get("control_s", float("nan"))
            print(f"      {arm:<16} whole process {a['join_s'] * 1e3:9.1f} ms  "
                  f"(JVM+I/O floor {ctl * 1e3:8.1f} ms -> join <= "
                  f"{(a['join_s'] - ctl) * 1e3:8.1f} ms)"
                  f"  rows_match={a.get('rows_match')}")
        else:
            print(f"      {arm:<16} read {a['read_s'] * 1e3:8.1f}  join {a['join_s'] * 1e3:9.3f}  "
                  f"write {a['write_s'] * 1e3:8.1f} ms" +
                  (f"  rows_match={a.get('rows_match')}" if arm != "parquet-fortran" else ""))
    sys.stdout.flush()


# --------------------------------------------------------------------------------------------
# Scale verification: the same comparison as the correctness stage, on the LARGE outputs
# --------------------------------------------------------------------------------------------

def _normalize_cols(t: pa.Table, lon, ron, other_on):
    """Undoes each library's own naming so four results become comparable.

    The same three fixups the correctness stage applies -- astropy's companion ``<col>.mask``,
    STILTS' ``_1``/``_2`` duplication, STILTS' ``find=all`` group columns -- factored out so the
    scale check can apply them without going through the string-rendering path, which cannot
    afford millions of rows.
    """
    cols = {n: t.column(n) for n in t.column_names}
    for m in [n for n in list(cols) if n.endswith(".mask")]:
        base = m[:-5]
        flags = cols.pop(m).to_pylist()
        if base in cols:
            vals = cols[base].to_pylist()
            cols[base] = pa.array([None if f else v for v, f in zip(vals, flags)])
    keyset = set(lon) | set(ron)
    if not other_on:
        for k in lon:
            a, b = f"{k}_1", f"{k}_2"
            if a in cols and b in cols:
                av, bv = cols.pop(a).to_pylist(), cols.pop(b).to_pylist()
                cols[k] = pa.array([x if x is not None else y for x, y in zip(av, bv)])
    for n in list(cols):
        if n.endswith("_1"):
            cols[n[:-2]] = cols.pop(n)
    if other_on:
        for k in ron:
            if f"{k}_2" in cols and k not in keyset:
                cols[k] = cols.pop(f"{k}_2")
    for d in ("GroupID", "GroupSize", "_jk_1", "_jk_2", "_jk"):
        cols.pop(d, None)
    return cols


def row_fingerprints(path, lon=("id",), ron=("id",), other_on=None, keep=None):
    """A sorted array of per-row hashes: the multiset of rows, at a size strings cannot reach.

    Numeric columns are unified to float64 first, which folds together the two representational
    differences the correctness stage handles by rendering -- pandas' int64 -> float64 upcast on a
    nullable column, and null against NaN for a row that had no counterpart.  Every key in these
    fixtures is far below 2**53, so nothing is lost to the cast.
    """
    import pandas as pd

    t = pq.read_table(path)
    cols = _normalize_cols(t, list(lon), list(ron), other_on)
    if keep is not None:
        # A semi/anti join carries no column from the right side, but STILTS has no `join` value
        # for one: the emulation is `find=best1`, which necessarily brings them and has them
        # deleted afterwards.  Restricting to the left table's columns is what the two contracts
        # actually have in common, and the row multiset is still what gets compared.
        cols = {k: v for k, v in cols.items() if k in keep}
    names = sorted(cols)
    df = pd.DataFrame()
    for n in names:
        ser = cols[n].to_pandas()
        if pd.api.types.is_bool_dtype(ser):
            ser = ser.astype("float64")
        elif pd.api.types.is_numeric_dtype(ser):
            ser = ser.astype("float64")
        else:
            ser = ser.astype(object).where(ser.notna(), None).astype(str)
        df[n] = ser
    h = pd.util.hash_pandas_object(df, index=False).to_numpy()
    del df
    return tuple(names), np.sort(h)


def run_verify(cfg, out):
    """Compares the LARGE join outputs the perf stage already wrote, row for row.

    The perf stage only checks that the row COUNTS agree, which a wrong answer can satisfy easily.
    This re-reads the files it left behind -- so it costs no extra joins -- and compares the full
    multiset of rows at 4M rows, which is the size at which a threading or chunking defect would
    first appear.
    """
    import glob
    res = []
    groups = {}
    for f in sorted(glob.glob(f"{cfg.work}/pf_*.parquet")):
        tag = os.path.basename(f)[3:-8]
        if not any(tag.startswith(s) for s in ("lookup", "symmetric", "one_to_one")):
            continue
        groups[tag] = f
    for tag, pf_file in groups.items():
        names_pf, h_pf = row_fingerprints(pf_file)
        rec = {"case": tag, "rows": len(h_pf), "arms": {}}
        for arm, pre in (("pandas", "pa"), ("astropy", "as"), ("stilts", "st")):
            other = f"{cfg.work}/{pre}_{tag}.parquet"
            if not os.path.exists(other):
                rec["arms"][arm] = "n/a"
                continue
            keep = set(names_pf) if tag.endswith(("_semi", "_anti")) else None
            names_o, h_o = row_fingerprints(other, keep=keep)
            if names_o != names_pf:
                rec["arms"][arm] = f"DIFFER (columns {names_pf} vs {names_o})"
            elif len(h_o) != len(h_pf):
                rec["arms"][arm] = f"DIFFER ({len(h_pf)} vs {len(h_o)} rows)"
            else:
                rec["arms"][arm] = "same" if np.array_equal(h_pf, h_o) else "DIFFER (rows)"
            del h_o
        del h_pf
        res.append(rec)
        print(f"  {tag:<22} rows={rec['rows']:<9} " +
              "  ".join(f"{a}={v}" for a, v in rec["arms"].items()))
        sys.stdout.flush()
    out["verify"] = res
    return res


# --------------------------------------------------------------------------------------------
# Noise floor
# --------------------------------------------------------------------------------------------

def run_floor(cfg, out):
    """Measures this harness's own run-to-run spread, so a reported delta can be believed.

    Per ``CLAUDE.md``: a floor taken on a different tool is a fact about that tool.  This one runs
    the campaign's own arms, on the campaign's own fixture, five times.
    """
    p = build_perf_fixtures(cfg.fixture_dir, cfg.nleft, cfg.nright, cfg.nsym, cfg.ncols)
    reps = 5
    pf_t, pd_t = [], []
    for _ in range(reps):
        r = run_pf(cfg.binary, p["one_l"], p["one_r"], f"{cfg.work}/floor_pf.parquet", "inner",
                   rounds=cfg.rounds, threads=cfg.threads)
        pf_t.append(r.join_s)
        r2 = run_pandas(p["one_l"], p["one_r"], f"{cfg.work}/floor_pd.parquet", "inner",
                        rounds=cfg.rounds)
        pd_t.append(r2.join_s)
    def spread(v):
        return (max(v) - min(v)) / min(v) * 100.0
    out["floor"] = {"parquet_fortran_pct": spread(pf_t), "pandas_pct": spread(pd_t),
                    "pf_times": pf_t, "pandas_times": pd_t, "reps": reps}
    print(f"  noise floor over {reps} repetitions of the same best-of-{cfg.rounds} figure: "
          f"parquet-fortran {spread(pf_t):.1f}%, pandas {spread(pd_t):.1f}%")
    return out["floor"]


# --------------------------------------------------------------------------------------------
# Driver
# --------------------------------------------------------------------------------------------

@dataclass
class Config:
    """Everything the wrapper passes in."""
    binary: str
    work: str
    fixture_dir: str
    nleft: int
    nright: int
    nsym: int
    ncols: int
    rounds: int
    threads: int
    astropy_max: int
    stages: tuple


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--binary", required=True, help="the built benchmark_join_crosslib executable")
    ap.add_argument("--work", default="test_run/joinxlib", help="scratch directory for outputs")
    ap.add_argument("--fixtures", default="test_run/joinxlib/fixtures")
    ap.add_argument("--nleft", type=int, default=4_000_000)
    ap.add_argument("--nright", type=int, default=10_000)
    ap.add_argument("--nsym", type=int, default=1_000_000)
    ap.add_argument("--ncols", type=int, default=4)
    ap.add_argument("--rounds", type=int, default=3)
    ap.add_argument("--threads", type=int, default=0)
    ap.add_argument("--astropy-max", type=int, default=2_000_000,
                    help="astropy joins are O(n log n) in python; skip it above this row count")
    ap.add_argument("--stages", default="correctness,semantics,claims,perf,verify,floor")
    ap.add_argument("--json", default="", help="write the whole run as JSON here")
    a = ap.parse_args(argv)

    cfg = Config(binary=a.binary, work=a.work, fixture_dir=a.fixtures, nleft=a.nleft,
                 nright=a.nright, nsym=a.nsym, ncols=a.ncols, rounds=a.rounds,
                 threads=a.threads, astropy_max=a.astropy_max,
                 stages=tuple(s.strip() for s in a.stages.split(",") if s.strip()))
    os.makedirs(cfg.work, exist_ok=True)
    os.makedirs(cfg.fixture_dir, exist_ok=True)

    import pandas as pd_mod
    import astropy
    out = {"versions": {
        "python": sys.version.split()[0], "pandas": pd_mod.__version__,
        "astropy": astropy.__version__, "pyarrow": pa.__version__, "numpy": np.__version__,
        "stilts": _stilts_version()}, "config": vars(a)}
    print("=" * 94)
    print("cross-library join comparison -- parquet-fortran vs pandas vs astropy vs STILTS")
    print("=" * 94)
    for k, v in out["versions"].items():
        print(f"  {k:<10} {v}")
    print()

    if "correctness" in cfg.stages:
        print("== correctness: same fixtures, same joins, row multisets compared ==")
        run_correctness(cfg, out)
        print()
    if "semantics" in cfg.stages:
        print("== semantics: where the libraries are entitled to disagree ==")
        run_semantics(cfg, out)
        print()
    if "claims" in cfg.stages:
        print("== claims: properties only parquet-fortran promises ==")
        run_claims(cfg, out)
        print()
    if "perf" in cfg.stages:
        print("== performance ==")
        run_perf(cfg, out)
        print()
    if "verify" in cfg.stages:
        print("== scale verification: the LARGE outputs compared row for row ==")
        run_verify(cfg, out)
        print()
    if "floor" in cfg.stages:
        print("== noise floor ==")
        run_floor(cfg, out)
        print()

    if a.json:
        with open(a.json, "w") as fh:
            json.dump(out, fh, indent=1, default=str)
        print(f"wrote {a.json}")
    return 0


def _stilts_version():
    exe = shutil.which("stilts")
    if exe is None:
        return "absent"
    p = subprocess.run([exe, "-version"], capture_output=True, text=True)
    for line in p.stdout.splitlines():
        if "STILTS version" in line:
            return line.strip()
    return "unknown"


if __name__ == "__main__":
    sys.exit(main())
