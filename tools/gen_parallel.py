#!/usr/bin/env python3
"""One process-parallel `map` for the generators whose oracles take minutes.

`tools/generate_kde_vectors.py` and `tools/generate_cosmology_reference.py` derive their golden
expectations at 50 and 30 decimal digits, case by case, and both run their `--check` in the lint
stage -- where the whole suite is meant to finish in about a minute. Between them they were three
quarters of that stage's run time, entirely in `mpmath` arithmetic that cannot be cheapened
without changing what the oracle certifies.

What CAN be changed is how many cores it runs on. Every case in both generators is an independent
derivation with no shared state, so mapping them across processes leaves each one computing
exactly the arithmetic it computed before, at the same precision, and leaves the output in the
same order. Both callers verify that by construction: their `--check` compares the WHOLE emitted
text against the committed file, so any divergence fails the check rather than passing quietly.

This lives in its own module because two generators use it and a rule of this repository is that
two places needing the same thing derive it from one (`.claude/rules/code-style.md`, "Sibling
generators share one source of truth").

A faster route exists and is NOT a substitute for this one: `mpmath` uses a pure-Python bignum
backend unless `gmpy2` is importable, and `gmpy2` makes the same arithmetic several times faster.
Check with `python3 -c "import mpmath.libmp; print(mpmath.libmp.BACKEND)"` -- `python` means the
slow one. Installing it is an environment change, so it is the maintainer's to make; the two are
cumulative.
"""
import concurrent.futures
import multiprocessing
import os


def map_cases(fn, cases):
    """`[fn(case) for case in cases]`, computed across processes when that is available.

    `fn` must be importable by name (a module-level function, or a bound method of a picklable
    object) and every case and result must pickle. Order is preserved.

    `fork` is required rather than merely preferred: a spawned worker re-imports the caller as
    `__main__`, which would re-enter its `main`. The map runs serially when there is one core, one
    case, no `fork` (Windows), or `PARQUET_GEN_JOBS=1` -- which is also how to get a clean
    traceback or a profile out of one of these generators. A pool that cannot start falls back to
    serial rather than failing a lint check; an exception raised inside `fn` is not caught here and
    surfaces from the serial re-run.
    """
    cases = list(cases)
    jobs = jobs_requested()
    if jobs <= 0:
        jobs = os.cpu_count() or 1
    jobs = max(1, min(jobs, len(cases)))
    if jobs == 1 or "fork" not in multiprocessing.get_all_start_methods():
        return [fn(case) for case in cases]
    try:
        context = multiprocessing.get_context("fork")
        with concurrent.futures.ProcessPoolExecutor(max_workers=jobs,
                                                    mp_context=context) as pool:
            return list(pool.map(fn, cases))
    except (OSError, concurrent.futures.process.BrokenProcessPool):
        return [fn(case) for case in cases]


def jobs_requested():
    """`PARQUET_GEN_JOBS` as an int, or 0 for "decide from the machine". Never raises."""
    try:
        return int(os.environ.get("PARQUET_GEN_JOBS", "") or 0)
    except ValueError:
        return 0
