#!/usr/bin/env python3
"""Cross-library comparison of ``pf_spatial_index`` against scipy's cKDTree and scikit-learn's trees.

Driven by ``bench/benchmark_spatial_crosslib.sh``, which owns the build (``--profile release``) and
the ``-O`` assertion.  This script owns the fixtures, the Python arms, the row-for-row comparison
and the report; ``bench/benchmark_spatial_crosslib.f90`` is the Fortran arm it drives.

WHAT IT ANSWERS
---------------
1. **Correctness.**  Every query family this library offers that another library can also express
   is run against it over the SAME bytes -- this script writes the coordinates and the query list,
   both arms read them -- and the answers are compared as sets of rows.  The three axis shapes have
   no counterpart anywhere, so they are cross-checked through scipy's own tree instead: the ball
   that contains the shape supplies the candidates and numpy applies the shape's predicate, which
   is the same experiment ``test_axis_is_the_covering_ball_filtered`` runs inside the suite, with
   the candidates coming from a different library's walk.
2. **Speed.**  Build, single queries, the bulk self-join, the pair list and k-nearest, each timed
   separately, because a single end-to-end figure would hide that this library wins the radius
   queries and loses k-nearest on a clustered field.
3. **Shape.**  Three fixtures -- uniform, mildly clustered and tightly clumped -- because the
   ranking inverts between them: a uniform grid answers an empty region without descending
   anything, and pays for a cell that lands on a clump.

RULES FROM CLAUDE.md's BENCHMARKING SECTION THAT THIS SCRIPT FOLLOWS
--------------------------------------------------------------------
* Best of N rounds per figure, never a single measurement.
* Every buffer is allocated and every input written before any timed loop.
* A noise floor measured with this harness, on this campaign's own fixture (``--stages=floor``).
* A library that cannot express a case is reported ``n/a``, never silently skipped.

Nothing here runs under ``fpm test`` or in CI: it needs scipy and scikit-learn.  Maintainer-only.
"""
from __future__ import annotations

import argparse
import os
import subprocess
import sys
import time

import numpy as np
from scipy.spatial import cKDTree


# --------------------------------------------------------------------------------------------
# Fixtures.  One seed per shape, so a shape is reproducible on its own and adding one moves none
# of the others.  Sizes come from the command line; the box side is fixed so that a given radius
# means the same neighbour count at every size unless --stages=scale says otherwise.
# --------------------------------------------------------------------------------------------
def gen_points(kind: str, n: int, side: float, seed: int) -> np.ndarray:
    rng = np.random.default_rng(seed)
    if kind == "uniform":
        return rng.random((n, 3)) * side
    if kind == "clustered":
        nc = max(1, n // 100)
        centres = rng.random((nc, 3)) * side
        return centres[rng.integers(0, nc, n)] + rng.normal(0.0, 0.01 * side, (n, 3))
    if kind == "tight":
        # A thousand points per clump at a five-hundredth of the box: a cell that lands on one is
        # tested in full however little of it the query reaches, which is the shape a uniform grid
        # is worst at and a kd-tree adapts to.
        nc = max(1, n // 1000)
        centres = rng.random((nc, 3)) * side
        return centres[rng.integers(0, nc, n)] + rng.normal(0.0, 0.002 * side, (n, 3))
    if kind == "sphere":
        v = rng.normal(size=(n, 3))
        v /= np.linalg.norm(v, axis=1)[:, None]
        return 0.5 * side * (1.0 + v)
    raise SystemExit(f"benchmark_spatial_crosslib.py: unknown fixture '{kind}'")


def write_pts(path: str, pts: np.ndarray) -> None:
    np.ascontiguousarray(pts, dtype="<f8").tofile(path)


def write_queries(path: str, q: np.ndarray) -> None:
    assert q.shape[1] == 8, "a query row is p1(3), p2(3), r1, r2"
    np.ascontiguousarray(q, dtype="<f8").tofile(path)


def run_fortran(cfg, mode: str, ptsfile: str, qfile: str, n: int, nq: int, out: str,
                rounds: int | None = None, threads: int = 0, k: int = 10) -> dict:
    cmd = [cfg.binary, f"--mode={mode}", f"--pts={ptsfile}", f"--queries={qfile}",
           f"--n={n}", f"--nq={nq}", f"--out={out}",
           f"--rounds={rounds if rounds is not None else cfg.rounds}",
           f"--threads={threads}", f"--k={k}"]
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        print(r.stdout)
        print(r.stderr, file=sys.stderr)
        raise SystemExit(f"benchmark_spatial_crosslib.py: the Fortran arm failed on --mode={mode}")
    info = {}
    with open(out + ".time") as fh:
        for line in fh:
            key, _, val = line.strip().partition(" ")
            info[key] = float(val) if ("." in val or "e" in val.lower()) else int(val)
    return info


def read_rows(path: str, nq: int) -> list[np.ndarray]:
    raw = np.fromfile(path, dtype="<i8")
    out, pos = [], 0
    for _ in range(nq):
        m = int(raw[pos])
        pos += 1
        out.append(raw[pos:pos + m] - 1)          # the Fortran arm answers in 1-based rows
        pos += m
    return out


def read_counts(path: str, n: int) -> np.ndarray:
    return np.fromfile(path, dtype="<i8", count=n)


def compare_sets(a: list[np.ndarray], b, label: str, margin=None) -> int:
    """Row-for-row set comparison; returns how many queries differed."""
    diff, worst = 0, 0.0
    for i, (ra, rb) in enumerate(zip(a, b)):
        sa, sb = set(ra.tolist()), set(np.asarray(rb).tolist())
        if sa != sb:
            diff += 1
            if margin is not None:
                for idx in sa ^ sb:
                    worst = max(worst, margin(i, idx))
            if diff <= 5:
                print(f"    {label}: query {i} differs: only-fortran={sorted(sa - sb)[:6]} "
                      f"only-python={sorted(sb - sa)[:6]}")
    if diff and margin is not None:
        print(f"    {label}: worst relative margin over the disputed points: {worst:.3e}")
    return diff


def _time(fn) -> float:
    t0 = time.perf_counter()
    fn()
    return time.perf_counter() - t0


def _best(fn, rounds: int) -> float:
    return min(_time(fn) for _ in range(rounds))


# --------------------------------------------------------------------------------------------
# Stages
# --------------------------------------------------------------------------------------------
def stage_correct(cfg) -> None:
    print("=" * 96)
    print("CORRECTNESS -- pf_spatial_index against scipy.spatial.cKDTree, on identical bytes")
    print("=" * 96)
    n, nq, side = cfg.n_correct, cfg.nq_correct, 100.0
    ptsfile = os.path.join(cfg.work, "pts.bin")
    qfile = os.path.join(cfg.work, "q.bin")
    for kind in ("uniform", "clustered", "sphere"):
        pts = gen_points(kind, n, side, seed=7 + ("uniform", "clustered", "sphere").index(kind))
        write_pts(ptsfile, pts)
        tree = cKDTree(pts)
        rng = np.random.default_rng(99)

        q = np.zeros((nq, 8))
        q[:, 0:3] = rng.random((nq, 3)) * side * 1.1 - 0.05 * side
        q[:, 6] = np.resize([0.5, 1.0, 2.0, 4.0], nq)
        q[:, 7] = q[:, 6]
        write_queries(qfile, q)
        run_fortran(cfg, "ball", ptsfile, qfile, n, nq, os.path.join(cfg.work, "ball"), rounds=1)
        rows_f = read_rows(os.path.join(cfg.work, "ball.bin"), nq)
        rows_p = [tree.query_ball_point(q[i, 0:3], q[i, 6]) for i in range(nq)]

        def ball_margin(i, idx):
            d = float(np.linalg.norm(pts[idx] - q[i, 0:3]))
            return abs(d - q[i, 6]) / max(q[i, 6], 1e-300)

        diff = compare_sets(rows_f, rows_p, f"ball/{kind}", ball_margin)
        print(f"  ball      {kind:<10} {nq} queries, {sum(len(r) for r in rows_f)} rows: "
              f"{'IDENTICAL' if diff == 0 else str(diff) + ' DIFFER'}")

        # The three axis shapes, through scipy's own ball query: the covering ball supplies the
        # candidates and numpy applies the shape's predicate.  No library has these shapes, so
        # this is the only cross-library check available for them.
        for mode, clamp in (("seg", True), ("cyl", False), ("cone", False)):
            m = max(nq // 4, 1)
            qa = np.zeros((m, 8))
            qa[:, 0:3] = rng.random((m, 3)) * side
            qa[:, 3:6] = qa[:, 0:3] + (rng.random((m, 3)) - 0.5) * 40.0
            qa[:, 6] = np.resize([0.5, 1.0, 2.0, 4.0], m)
            qa[:, 7] = qa[:, 6] if mode != "cone" else qa[:, 6] * 2.0
            write_queries(qfile, qa)
            run_fortran(cfg, mode, ptsfile, qfile, n, m, os.path.join(cfg.work, mode), rounds=1)
            rows_f = read_rows(os.path.join(cfg.work, mode + ".bin"), m)
            rows_p, blocks = [], []
            for i in range(m):
                p1, p2, r1, r2 = qa[i, 0:3], qa[i, 3:6], qa[i, 6], qa[i, 7]
                v = p2 - p1
                dd = float(v @ v)
                cand = np.asarray(tree.query_ball_point(0.5 * (p1 + p2),
                                                        0.5 * np.sqrt(dd) + max(r1, r2)),
                                  dtype=np.int64)
                if cand.size == 0:
                    rows_p.append(np.empty(0, dtype=np.int64))
                    blocks.append(None)
                    continue
                qv = pts[cand] - p1
                t = (qv @ v) / dd
                keep = np.ones(cand.size, dtype=bool) if clamp else (t >= 0.0) & (t <= 1.0)
                if clamp:
                    t = np.clip(t, 0.0, 1.0)
                w = qv - t[:, None] * v
                d = np.sqrt(np.einsum("ij,ij->i", w, w))
                rad = r1 + t * (r2 - r1)
                rows_p.append(cand[keep & (d <= rad)])
                blocks.append((cand, d, rad))

            def axis_margin(i, idx, blocks=blocks):
                blk = blocks[i]
                if blk is None:
                    return float("inf")
                cand, d, rad = blk
                j = np.where(cand == idx)[0]
                return float("inf") if j.size == 0 else abs(d[j[0]] - rad[j[0]]) / max(rad[j[0]], 1e-300)

            diff = compare_sets(rows_f, rows_p, f"{mode}/{kind}", axis_margin)
            print(f"  {mode:<9} {kind:<10} {m} queries, {sum(len(r) for r in rows_f)} rows: "
                  f"{'IDENTICAL' if diff == 0 else str(diff) + ' DIFFER'}   "
                  f"(covering ball + numpy filter, through scipy's tree)")

        for r in (1.0, 2.0):
            q0 = np.zeros((1, 8))
            q0[0, 6] = r
            write_queries(qfile, q0)
            run_fortran(cfg, "selfjoin", ptsfile, qfile, n, 1, os.path.join(cfg.work, "sj"), rounds=1)
            cf = read_counts(os.path.join(cfg.work, "sj.bin"), n)
            cp = tree.query_ball_point(pts, r, return_length=True, workers=-1)
            same = int(np.sum(cf == cp))
            print(f"  selfjoin  {kind:<10} r={r}: {same}/{n} row counts identical"
                  f"{'' if same == n else '   MISMATCH'}")
            info = run_fortran(cfg, "pairs", ptsfile, qfile, n, 1, os.path.join(cfg.work, "pr"), rounds=1)
            df = read_counts(os.path.join(cfg.work, "pr.bin"), n)
            pr = tree.query_pairs(r, output_type="ndarray")
            dp = np.bincount(pr.ravel(), minlength=n)
            same = int(np.sum(df == dp))
            print(f"  pairs     {kind:<10} r={r}: {info['total_rows']} pairs vs {len(pr)} scipy; "
                  f"{same}/{n} degrees identical{'' if same == n else '   MISMATCH'}")

        kk = 10
        qn = np.zeros((nq, 8))
        qn[:, 0:3] = rng.random((nq, 3)) * side
        qn[:, 6] = 1.0
        qn[:, 7] = 1.0
        write_queries(qfile, qn)
        run_fortran(cfg, "knn", ptsfile, qfile, n, nq, os.path.join(cfg.work, "knn"), rounds=1, k=kk)
        rows_f = read_rows(os.path.join(cfg.work, "knn.bin"), nq)
        dp, ip = tree.query(qn[:, 0:3], k=kk, workers=-1)
        diff = sum(1 for i in range(nq) if set(rows_f[i].tolist()) != set(ip[i].tolist()))
        dmax = max(float(np.max(np.abs(np.sort(np.linalg.norm(pts[rows_f[i]] - qn[i, 0:3], axis=1))
                                       - np.sort(dp[i])))) for i in range(nq))
        print(f"  nearest   {kind:<10} k={kk}, {nq} queries: "
              f"{'IDENTICAL sets' if diff == 0 else str(diff) + ' DIFFER'}, "
              f"largest distance difference {dmax:.3e}")


def stage_perf(cfg) -> None:
    print()
    print("=" * 96)
    print(f"SPEED -- n={cfg.n}, nq={cfg.nq}, best of {cfg.rounds}, "
          f"loadavg {open('/proc/loadavg').read().split()[0] if os.path.exists('/proc/loadavg') else 'n/a'}")
    print("  pf/pf count are SERIAL; scipy vec is one vectorised call, vec xN uses workers=N.")
    print("=" * 96)
    side = 100.0
    ptsfile = os.path.join(cfg.work, "pts.bin")
    qfile = os.path.join(cfg.work, "q.bin")
    for kind in ("uniform", "clustered", "tight"):
        pts = gen_points(kind, cfg.n, side, seed=11)
        write_pts(ptsfile, pts)
        rng = np.random.default_rng(5)
        qp = rng.random((cfg.nq, 3)) * side
        tb = _best(lambda: cKDTree(pts), cfg.rounds)
        tbu = _best(lambda: cKDTree(pts, balanced_tree=False), cfg.rounds)
        tree = cKDTree(pts)
        print(f"\n  [{kind}]  build: scipy cKDTree {tb * 1e3:8.1f} ms   "
              f"(balanced_tree=False {tbu * 1e3:8.1f} ms)")

        for r in cfg.radii:
            q = np.zeros((cfg.nq, 8))
            q[:, 0:3] = qp
            q[:, 6] = r
            q[:, 7] = r
            write_queries(qfile, q)
            info = run_fortran(cfg, "ball", ptsfile, qfile, cfg.n, cfg.nq, os.path.join(cfg.work, "ball"))
            infc = run_fortran(cfg, "countball", ptsfile, qfile, cfg.n, cfg.nq, os.path.join(cfg.work, "cb"))
            tv = _best(lambda: tree.query_ball_point(qp, r, workers=1), cfg.rounds)
            tvw = _best(lambda: tree.query_ball_point(qp, r, workers=cfg.threads), cfg.rounds)
            tcnt = _best(lambda: tree.query_ball_point(qp, r, return_length=True, workers=1), cfg.rounds)
            print(f"    r={r:<5} {info['total_rows'] / cfg.nq:8.1f} nb/query   "
                  f"pf {info['query_s'] * 1e3:8.1f} ms | pf count {infc['query_s'] * 1e3:8.1f} ms | "
                  f"scipy vec {tv * 1e3:8.1f} ms | vec x{cfg.threads} {tvw * 1e3:8.1f} ms | "
                  f"scipy count {tcnt * 1e3:8.1f} ms")
            if r == cfg.radii[0]:
                print(f"           build: pf {info['build_s'] * 1e3:8.1f} ms   "
                      f"cell {info['cell']:.4f}   cells {info['cells']}")

        for r in cfg.radii[:2]:
            q0 = np.zeros((1, 8))
            q0[0, 6] = r
            write_queries(qfile, q0)
            i1 = run_fortran(cfg, "selfjoin", ptsfile, qfile, cfg.n, 1,
                             os.path.join(cfg.work, "sj"), threads=1)
            iN = run_fortran(cfg, "selfjoin", ptsfile, qfile, cfg.n, 1,
                             os.path.join(cfg.work, "sj"), threads=cfg.threads)
            t1 = _best(lambda: tree.query_ball_point(pts, r, return_length=True, workers=1), cfg.rounds)
            tN = _best(lambda: tree.query_ball_point(pts, r, return_length=True, workers=cfg.threads),
                       cfg.rounds)
            print(f"    self-join counts r={r}: pf serial {i1['query_s'] * 1e3:9.1f} ms | "
                  f"pf x{cfg.threads} {iN['query_s'] * 1e3:8.1f} ms | scipy serial {t1 * 1e3:9.1f} ms | "
                  f"scipy x{cfg.threads} {tN * 1e3:8.1f} ms")

        if kind == "uniform":
            r = cfg.radii[1]
            q0 = np.zeros((1, 8))
            q0[0, 6] = r
            write_queries(qfile, q0)
            ip = run_fortran(cfg, "pairs", ptsfile, qfile, cfg.n, 1,
                             os.path.join(cfg.work, "pr"), threads=cfg.threads)
            i1 = run_fortran(cfg, "pairs", ptsfile, qfile, cfg.n, 1,
                             os.path.join(cfg.work, "pr"), threads=1)
            tp = _best(lambda: tree.query_pairs(r, output_type="ndarray"), cfg.rounds)
            print(f"    pair list r={r}: pf serial {i1['query_s'] * 1e3:9.1f} ms | "
                  f"pf x{cfg.threads} {ip['query_s'] * 1e3:9.1f} ms ({ip['total_rows']} pairs) | "
                  f"scipy query_pairs {tp * 1e3:9.1f} ms (no threading)")

        for kk in (1, 10, 50):
            qn = np.zeros((cfg.nq, 8))
            qn[:, 0:3] = qp
            qn[:, 6] = cfg.radii[1]
            qn[:, 7] = cfg.radii[1]
            write_queries(qfile, qn)
            info = run_fortran(cfg, "knn", ptsfile, qfile, cfg.n, cfg.nq,
                               os.path.join(cfg.work, "knn"), k=kk)
            t1 = _best(lambda: tree.query(qp, k=kk, workers=1), cfg.rounds)
            tN = _best(lambda: tree.query(qp, k=kk, workers=cfg.threads), cfg.rounds)
            print(f"    nearest k={kk:<3}: pf {info['query_s'] * 1e3:9.1f} ms | "
                  f"scipy serial {t1 * 1e3:8.1f} ms | scipy x{cfg.threads} {tN * 1e3:8.1f} ms | "
                  f"pf ball expansions {info['shell_rounds'] / cfg.nq:6.2f} per query")


def stage_sky(cfg) -> None:
    try:
        from sklearn.neighbors import BallTree
    except ImportError:
        print("\nSKY: n/a -- scikit-learn is not installed")
        return
    print()
    print("=" * 96)
    print("SKY -- %build_sky against sklearn BallTree(metric='haversine'), on identical bytes")
    print("=" * 96)
    rng = np.random.default_rng(2026)
    n, nq = cfg.n_sky, cfg.nq_sky
    for kind in ("uniform", "polar"):
        if kind == "uniform":
            ra = rng.random(n) * 360.0
            dec = np.degrees(np.arcsin(2.0 * rng.random(n) - 1.0))
        else:
            # Every point within two degrees of a pole, half of them straddling 0h: the fixture a
            # scheme working in (ra, dec) rather than in vectors gets wrong.
            ra = np.where(rng.random(n) < 0.5, rng.random(n) * 3.0, 357.0 + rng.random(n) * 3.0)
            dec = np.where(rng.random(n) < 0.5, 88.0 + rng.random(n) * 2.0, -90.0 + rng.random(n) * 2.0)
        pts = np.zeros((n, 3))
        pts[:, 0] = ra
        pts[:, 1] = dec
        ptsfile = os.path.join(cfg.work, "skypts.bin")
        qfile = os.path.join(cfg.work, "skyq.bin")
        write_pts(ptsfile, pts)
        X = np.radians(np.column_stack([dec, ra]))
        tb = _best(lambda: BallTree(X, metric="haversine"), cfg.rounds)
        tree = BallTree(X, metric="haversine")
        qra = rng.random(nq) * 360.0
        qdec = np.degrees(np.arcsin(2.0 * rng.random(nq) - 1.0))
        if kind == "polar":
            pick = rng.integers(0, n, nq)
            qra, qdec = ra[pick], dec[pick]
        Q = np.radians(np.column_stack([qdec, qra]))
        for rdeg in cfg.sky_radii:
            q = np.zeros((nq, 8))
            q[:, 0] = qra
            q[:, 1] = qdec
            q[:, 6] = rdeg
            q[:, 7] = rdeg
            write_queries(qfile, q)
            info = run_fortran(cfg, "sky", ptsfile, qfile, n, nq, os.path.join(cfg.work, "skyout"))
            rows_f = read_rows(os.path.join(cfg.work, "skyout.bin"), nq)
            t1 = _best(lambda: tree.query_radius(Q, r=np.radians(rdeg)), cfg.rounds)
            diff = compare_sets(rows_f, list(tree.query_radius(Q, r=np.radians(rdeg))),
                                f"sky/{kind}/{rdeg}")
            print(f"  {kind:<8} r={rdeg:<5} {sum(len(r) for r in rows_f) / nq:9.1f} nb/query   "
                  f"{'IDENTICAL' if diff == 0 else str(diff) + ' DIFFER'}   "
                  f"pf {info['query_s'] * 1e3:8.1f} ms | sklearn {t1 * 1e3:8.1f} ms | "
                  f"build pf {info['build_s'] * 1e3:7.1f} ms vs sklearn {tb * 1e3:7.1f} ms")


def stage_scale(cfg) -> None:
    print()
    print("=" * 96)
    print("SCALING -- density held fixed, so each query returns the same count at every size")
    print("=" * 96)
    ptsfile = os.path.join(cfg.work, "sc.bin")
    qfile = os.path.join(cfg.work, "scq.bin")
    for n in cfg.scale_sizes:
        side = float(n) ** (1.0 / 3.0)
        pts = gen_points("uniform", n, side, seed=3)
        write_pts(ptsfile, pts)
        rng = np.random.default_rng(17)
        qp = rng.random((cfg.nq, 3)) * side
        tree = cKDTree(pts)
        row = f"  n={n:>9}  side={side:7.2f}  "
        info = {}
        for r in (1.0, 3.0):
            q = np.zeros((cfg.nq, 8))
            q[:, 0:3] = qp
            q[:, 6] = r
            q[:, 7] = r
            write_queries(qfile, q)
            info = run_fortran(cfg, "ball", ptsfile, qfile, n, cfg.nq, os.path.join(cfg.work, "scout"))
            tv = _best(lambda: tree.query_ball_point(qp, r, workers=1), cfg.rounds)
            row += (f"| r={r}: {info['total_rows'] / cfg.nq:6.1f} nb  "
                    f"pf {info['query_s'] / cfg.nq * 1e9:7.0f} ns/q  "
                    f"scipy {tv / cfg.nq * 1e9:8.0f} ns/q  ")
        print(row + f"| build pf {info['build_s'] * 1e3:8.1f} ms")


def stage_floor(cfg) -> None:
    """This harness's own run-to-run spread, on this campaign's own fixture."""
    print()
    print("=" * 96)
    print("FLOOR -- the same measurement repeated, with nothing changed between repeats")
    print("=" * 96)
    side = 100.0
    pts = gen_points("uniform", cfg.n, side, seed=11)
    ptsfile = os.path.join(cfg.work, "pts.bin")
    qfile = os.path.join(cfg.work, "q.bin")
    write_pts(ptsfile, pts)
    rng = np.random.default_rng(5)
    qp = rng.random((cfg.nq, 3)) * side
    q = np.zeros((cfg.nq, 8))
    q[:, 0:3] = qp
    q[:, 6] = cfg.radii[1]
    q[:, 7] = cfg.radii[1]
    write_queries(qfile, q)
    tree = cKDTree(pts)
    pf, sp, bd = [], [], []
    for _ in range(5):
        info = run_fortran(cfg, "ball", ptsfile, qfile, cfg.n, cfg.nq, os.path.join(cfg.work, "fl"))
        pf.append(info["query_s"] * 1e3)
        bd.append(info["build_s"] * 1e3)
        sp.append(_best(lambda: tree.query_ball_point(qp, cfg.radii[1], workers=1), cfg.rounds) * 1e3)
    for name, v in (("pf query", pf), ("pf build", bd), ("scipy query", sp)):
        print(f"  {name:<12} min {min(v):8.1f}  max {max(v):8.1f} ms   spread "
              f"{100.0 * (max(v) / min(v) - 1.0):5.1f} %")


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--binary", required=True, help="the built benchmark_spatial_crosslib")
    ap.add_argument("--work", default="test_run/spatialxlib/work")
    ap.add_argument("--stages", default="correct,perf,sky,scale,floor")
    ap.add_argument("--n", type=int, default=1000000)
    ap.add_argument("--nq", type=int, default=20000)
    ap.add_argument("--n-correct", type=int, default=200000)
    ap.add_argument("--nq-correct", type=int, default=20000)
    ap.add_argument("--n-sky", type=int, default=500000)
    ap.add_argument("--nq-sky", type=int, default=5000)
    ap.add_argument("--rounds", type=int, default=3)
    ap.add_argument("--threads", type=int, default=64)
    cfg = ap.parse_args()
    cfg.radii = [0.5, 1.0, 2.0, 4.0]
    cfg.sky_radii = [0.1, 1.0, 5.0]
    cfg.scale_sizes = [100000, 400000, 1600000, 6400000]
    os.makedirs(cfg.work, exist_ok=True)
    import scipy
    print(f"# scipy {scipy.__version__}, numpy {np.__version__}")
    stages = [s.strip() for s in cfg.stages.split(",") if s.strip()]
    for s in stages:
        fn = {"correct": stage_correct, "perf": stage_perf, "sky": stage_sky,
              "scale": stage_scale, "floor": stage_floor}.get(s)
        if fn is None:
            raise SystemExit(f"benchmark_spatial_crosslib.py: unknown stage '{s}'")
        fn(cfg)


if __name__ == "__main__":
    main()
