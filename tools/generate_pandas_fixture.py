#!/usr/bin/env python3
"""Write `test/fixtures/pandas_written.parquet` -- a Parquet file produced by pandas itself.

Every other fixture under `test/fixtures/` is built against the Arrow C++ API by
`tools/generate_fixtures.cpp`, which can imitate any Arrow type this library has to read. What it
cannot imitate is PROVENANCE: that a real pandas release, writing an ordinary DataFrame with no
Parquet-specific arguments, produces exactly these shapes. This file exists so the reader tests
assert against pandas' actual output rather than against what this project believes pandas does.

Four properties of that output are what the fixture pins, all of them measured rather than assumed
(pandas 3.0.5 / pyarrow 25.0.0, and unchanged since the pandas 2 series):

  * a `Categorical` column is written as an Arrow dictionary over strings, with the NARROWEST index
    type that fits its categories (int8 here, not int32);
  * an integer `Categorical` is written as its values, and Arrow hands it straight back as int64 --
    only string and binary dictionaries are restored (`IsDictionaryReadSupported` in Arrow's
    `parquet/arrow/schema.cc`), so this library never sees a dictionary for one;
  * a plain string column is `large_string` from pandas 3 onwards, where pandas 2 wrote `string`;
  * a non-default index becomes an ordinary data column named `__index_level_0__`, and the whole
    frame's dtype description is stored under the schema metadata key `pandas`.

Committed, like every other fixture, so `fpm test` never regenerates it and the test suite needs
neither pandas nor Python. Run this by hand only when the fixture has to be recreated:

    conda activate astro && tools/generate_pandas_fixture.py

Usage:  tools/generate_pandas_fixture.py [--check]

  --check   write to a temporary file and compare the SCHEMA and the column values with the
            committed fixture, reporting any difference and exiting 1. Not a byte comparison: two
            pandas releases can write the same frame with different compression or metadata and
            still agree on everything the tests read.
"""

import argparse
import os
import sys
import tempfile

FIXTURE = "test/fixtures/pandas_written.parquet"

# Six rows, small enough to state every expected value in a test. `cls` is the categorical the
# library has to decode: one row is missing (pandas writes a null index for a NaN category), and
# "galaxy" repeats so the dictionary is genuinely shorter than the column.
ROWS = {
    "obs_id": [1, 2, 3, 4, 5, 6],
    "cls": ["galaxy", "star", None, "galaxy", "qso", "star"],
    "survey": ["4MOST", "SDSS", "DESI", "4MOST", "SDSS", "4MOST"],
    "mag": [18.5, 19.25, 20.0, 17.75, 21.5, 16.0],
    "prio": [1, 2, 1, 3, 2, 1],
}
INDEX = [10, 20, 30, 40, 50, 60]


def build_frame():
    """The DataFrame the fixture is written from, with no Parquet-specific arguments anywhere."""
    import numpy as np
    import pandas as pd

    frame = pd.DataFrame(
        {
            "obs_id": np.array(ROWS["obs_id"], dtype="int32"),
            "cls": pd.Categorical(ROWS["cls"]),
            "survey": ROWS["survey"],
            "mag": ROWS["mag"],
            "prio": pd.Categorical(ROWS["prio"]),
        }
    )
    # A non-default index, which is what makes pandas write the __index_level_0__ column. A default
    # RangeIndex is recorded in the `pandas` metadata only and writes no column at all.
    frame.index = pd.Index(INDEX)
    return frame


def describe(path):
    """The schema and column values a test actually reads, as comparable Python objects."""
    import pyarrow.parquet as pq

    table = pq.read_table(path)
    schema = [(field.name, str(field.type)) for field in table.schema]
    has_pandas_key = b"pandas" in (table.schema.metadata or {})
    values = {name: table.column(name).to_pylist() for name in table.schema.names}
    return schema, has_pandas_key, values


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--check", action="store_true", help="compare with the committed fixture instead of writing it")
    args = parser.parse_args()

    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    os.chdir(root)

    try:
        frame = build_frame()
    except ImportError as exc:  # pragma: no cover -- environment gap, not a defect
        print(f"needs pandas and pyarrow ({exc}); run under the astro environment", file=sys.stderr)
        return 1

    if not args.check:
        frame.to_parquet(FIXTURE)
        schema, has_pandas_key, _ = describe(FIXTURE)
        print(f"wrote {FIXTURE}")
        for name, type_name in schema:
            print(f"  {name:20s} {type_name}")
        print(f"  pandas metadata key present: {has_pandas_key}")
        return 0

    if not os.path.exists(FIXTURE):
        print(f"{FIXTURE} does not exist -- run this script without --check first", file=sys.stderr)
        return 1
    with tempfile.TemporaryDirectory() as tmpdir:
        fresh = os.path.join(tmpdir, "fresh.parquet")
        frame.to_parquet(fresh)
        want = describe(fresh)
        have = describe(FIXTURE)
    if want == have:
        print(f"{FIXTURE} is current (schema, pandas metadata key and every value agree)")
        return 0
    print(f"{FIXTURE} DIFFERS from what this environment's pandas writes:", file=sys.stderr)
    for label, w, h in zip(("schema", "pandas metadata key", "values"), want, have):
        if w != h:
            print(f"  {label}:\n    committed: {h}\n    fresh:     {w}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
