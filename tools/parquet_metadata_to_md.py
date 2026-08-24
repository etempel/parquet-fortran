#!/usr/bin/env python3
"""Reads a Parquet file's metadata (via pyarrow, no data columns loaded -- except a variable-length
list column's own data, read on its own to compute its per-row min/max length; see section 5) and
writes a Markdown report summarizing it. General-purpose: works on any Parquet file, not just
parquet-fortran's own output -- sections that depend on parquet-fortran/MAML-specific metadata
(VOTable header, flat "column.<name>.*" keys) are simply omitted when absent, and the
schema/statistics sections fall back to plain pyarrow/Parquet introspection otherwise.

The report has five sections:

  1. File-level metadata -- format version, `created_by`, row/column counts, file size, and a
     row-group size summary (count plus min/max/avg rows and compressed bytes per row group; no
     per-row-group table, since a file can have many row groups).
  2. VOTable metadata -- if the file carries parquet-fortran's own VOTable-style header (the
     "IVOA.VOTable-Parquet.content" key-value metadata entry, see build_votable_xml() in
     src/parquet_wrapper.cpp), it is parsed and rendered as a table-level parameter list plus a
     per-column unit/ucd/description table. Omitted if the key isn't present.
  3. Parquet key-value metadata -- every raw key/value pair in the file's key-value metadata map
     (this includes the VOTable XML from section 2 and any flat "column.<name>.*" keys, but also
     anything a *different* writer added, e.g. an "ARROW:schema" or "pandas" entry). Long/opaque
     values are collapsed to a byte-count note rather than dumped in full.
  4. Column statistics -- per column (aggregated across all row groups: min of mins, max of
     maxes, summed null_count/distinct_count), for columns where statistics are actually
     populated. Columns with no statistics at all are omitted.
  5. Column definitions -- one merged table of every top-level column: its type (rendered from
     the file's Arrow schema, so nested list/struct/map columns show their full nested structure,
     not just a flattened leaf list), its per-row list length for a top-level list-like column
     (the constant size for a fixed_size_list, read straight off the schema with no data access;
     min/max for a genuine variable-length list/large_list, which -- unlike every other figure in
     this report -- does require reading that one column's actual data, since Parquet's own
     column statistics don't record list length), nullability, compression codec(s) and
     encoding(s) actually used, compressed/uncompressed size and the column's share of the file's
     total compressed size (all summed/aggregated across row groups), and unit/ucd/description
     filled in from parquet-fortran's flat "column.<name>.*" metadata keys when present.

Usage:
    tools/parquet_metadata_to_md.py input.parquet [output.md] [options]

Examples:
    tools/parquet_metadata_to_md.py test_run/example_1.parquet
    tools/parquet_metadata_to_md.py test_run/example_1.parquet report.md --overwrite

Exit status: 0 on success; 1 on a usage/read error; 2 if the output file already exists and
OVERWRITE_DEFAULT has been changed to False (it is True by default, so this script overwrites an
existing output file without asking).

Requires pyarrow (not part of this repository's own Fortran toolchain).

Reads a `.parquet` file's metadata (via `pyarrow`, without
loading any column data) and writes a Markdown report: file-level info (format version,
`created_by`, row/column counts, a row-group count/size summary), this library's own VOTable-style
header if present (parsed and rendered human-readably, not just dumped as XML), every raw Parquet
key-value metadata entry (long/opaque values like `ARROW:schema` collapsed to a byte-count note),
per-column statistics aggregated across row groups (only for columns where they're actually
populated), and a merged column-definitions table (type/nesting rendered from the file's Arrow
schema, so nested `list`/`struct`/`map` columns show their real structure, plus compression
codec(s)/encoding(s) actually used and any `unit`/`ucd`/description this library's own metadata
provides). General-purpose -- works on any Parquet file, not just this library's own output (a
standalone Python tool, requires `pyarrow`, not part of this repository's own Fortran toolchain).
Not part of the public Fortran library or its API, but consumer-facing rather than maintainer/CI-
only, so it's kept in the fpm-published package (see its `KEEP_PATHS` entry in
`tools/prep_fpm_publish.sh`):

    tools/parquet_metadata_to_md.py data.parquet                 # -> data.md, overwritten if it exists
    tools/parquet_metadata_to_md.py data.parquet report.md
"""
import argparse
import struct
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.parquet as pq

# ---------------------------------------------------------------------------
# Config: default values for the CLI options below. Change here to change this
# script's defaults without touching the argparse wiring in main().
# ---------------------------------------------------------------------------
OUTPUT_SUFFIX_DEFAULT = ".md"   # output path, when omitted: input path with this suffix
OVERWRITE_DEFAULT = True        # True = replace an existing output file; False = refuse and exit nonzero
LONG_VALUE_THRESHOLD = 200      # key-value metadata values longer than this are shown as a note,
                                 # not dumped in full (section 3), by length alone
KNOWN_OPAQUE_KEYS = {"ARROW:schema", "pandas"}  # ...and these specific keys always collapse too,
                                 # regardless of length -- both carry binary/JSON blobs not
                                 # meant to be read as a flat value, even when short

EXIT_OK = 0
EXIT_ERROR = 1
EXIT_OUTPUT_EXISTS = 2

VOTABLE_METADATA_KEY = "IVOA.VOTable-Parquet.content"


def human_readable_bytes(n):
    size = float(n)
    for unit in ("B", "KiB", "MiB", "GiB", "TiB"):
        if size < 1024 or unit == "TiB":
            return f"{size:.1f} {unit}" if unit != "B" else f"{int(size)} B"
        size /= 1024
    return f"{size:.1f} TiB"  # pragma: no cover -- unreachable, loop's own TiB branch always returns first


def decode_metadata(raw_metadata):
    """pyarrow hands back a schema's key-value metadata as bytes -> bytes; decode to str -> str,
    tolerating non-UTF8 values (rare, but this script must not crash on an arbitrary third-party
    file) by falling back to a placeholder."""
    decoded = {}
    for key, value in (raw_metadata or {}).items():
        k = key.decode("utf-8", errors="replace") if isinstance(key, bytes) else str(key)
        if isinstance(value, bytes):
            try:
                v = value.decode("utf-8")
            except UnicodeDecodeError:
                v = f"<{len(value)} bytes, not valid UTF-8>"
        else:
            v = str(value)
        decoded[k] = v
    return decoded


def strip_namespace(elem):
    for el in elem.iter():
        if "}" in el.tag:
            el.tag = el.tag.split("}", 1)[1]
    return elem


def md_escape(s):
    return str(s).replace("|", "\\|").replace("\n", " ")


def md_table(headers, rows):
    """Renders headers/rows as a Markdown table with every column padded to its widest cell, so
    the raw .md source itself reads as a lined-up table, not just its rendered HTML."""
    if not rows:
        return "*(none)*\n"
    str_rows = [[md_escape(c) for c in row] for row in rows]
    widths = [len(h) for h in headers]
    for row in str_rows:
        for i, cell in enumerate(row):
            widths[i] = max(widths[i], len(cell))

    def fmt_row(cells):
        return "| " + " | ".join(cell.ljust(widths[i]) for i, cell in enumerate(cells)) + " |"

    lines = [fmt_row(headers), "| " + " | ".join("-" * widths[i] for i in range(len(headers))) + " |"]
    for row in str_rows:
        lines.append(fmt_row(row))
    return "\n".join(lines) + "\n"


def type_tree_lines(data_type, name=None, indent=0):
    """Recursively renders a pyarrow DataType as indented tree lines, so a nested list/struct/map
    column reads as an actual tree instead of one long flattened type string like
    'list<item: struct<a: int32, b: string>>'."""
    prefix = "  " * indent
    label = f"{name}: " if name else ""
    if pa.types.is_struct(data_type):
        lines = [f"{prefix}{label}struct"]
        for i in range(data_type.num_fields):
            field = data_type.field(i)
            lines.extend(type_tree_lines(field.type, field.name, indent + 1))
        return lines
    if pa.types.is_fixed_size_list(data_type):
        lines = [f"{prefix}{label}fixed_size_list[{data_type.list_size}]"]
        lines.extend(type_tree_lines(data_type.value_type, "element", indent + 1))
        return lines
    if pa.types.is_list(data_type) or pa.types.is_large_list(data_type):
        kind = "large_list" if pa.types.is_large_list(data_type) else "list"
        lines = [f"{prefix}{label}{kind}"]
        lines.extend(type_tree_lines(data_type.value_type, "item", indent + 1))
        return lines
    if pa.types.is_map(data_type):
        lines = [f"{prefix}{label}map"]
        lines.extend(type_tree_lines(data_type.key_type, "key", indent + 1))
        lines.extend(type_tree_lines(data_type.item_type, "value", indent + 1))
        return lines
    return [f"{prefix}{label}{data_type}"]


def decode_stat_value(value, logical_type):
    """pyarrow's column statistics decode most physical types automatically (even DECIMAL stored
    as FIXED_LEN_BYTE_ARRAY comes back as a proper Decimal), but not Float16 -- it hands back the
    raw 2-byte little-endian buffer instead of the actual float, so decode that one case here."""
    if isinstance(value, bytes) and logical_type is not None and str(logical_type) == "Float16":
        return struct.unpack("<e", value)[0]
    return value


def collect_leaf_column_stats(metadata):
    """One entry per physical (leaf) column path, aggregated across all row groups."""
    leaves = {}
    for rg_idx in range(metadata.num_row_groups):
        row_group = metadata.row_group(rg_idx)
        for col_idx in range(row_group.num_columns):
            col = row_group.column(col_idx)
            logical_type = metadata.schema.column(col_idx).logical_type
            entry = leaves.setdefault(col.path_in_schema, {
                "physical_type": col.physical_type,
                "codecs": set(), "encodings": set(),
                "min": None, "max": None, "null_count": 0, "distinct_count": 0,
                "has_min_max": False, "has_null_count": False, "has_distinct_count": False,
                "compressed_size": 0, "uncompressed_size": 0,
            })
            entry["codecs"].add(col.compression)
            entry["encodings"].update(col.encodings)
            entry["compressed_size"] += col.total_compressed_size
            entry["uncompressed_size"] += col.total_uncompressed_size
            stats = col.statistics
            if stats is None:
                continue
            if stats.has_min_max:
                entry["has_min_max"] = True
                min_val = decode_stat_value(stats.min, logical_type)
                max_val = decode_stat_value(stats.max, logical_type)
                entry["min"] = min_val if entry["min"] is None else min(entry["min"], min_val)
                entry["max"] = max_val if entry["max"] is None else max(entry["max"], max_val)
            if stats.has_null_count and stats.null_count is not None:
                entry["has_null_count"] = True
                entry["null_count"] += stats.null_count
            if stats.has_distinct_count and stats.distinct_count is not None:
                entry["has_distinct_count"] = True
                entry["distinct_count"] += stats.distinct_count
    return leaves


def render_file_level_section(pf, file_path):
    metadata = pf.metadata
    lines = ["## 1. File-level metadata\n"]
    rows = [
        ("File", str(file_path)),
        ("File size", human_readable_bytes(Path(file_path).stat().st_size)),
        ("Format version", metadata.format_version),
        ("Created by", metadata.created_by or "*(not set)*"),
        ("Rows", f"{metadata.num_rows:,}"),
        ("Columns (leaf)", str(metadata.num_columns)),
        ("Row groups", str(metadata.num_row_groups)),
    ]
    lines.append(md_table(("Field", "Value"), rows))

    row_counts = [metadata.row_group(i).num_rows for i in range(metadata.num_row_groups)]
    byte_sizes = [metadata.row_group(i).total_byte_size for i in range(metadata.num_row_groups)]
    lines.append("\n### Row group sizes\n")
    if row_counts:
        rg_rows = [
            ("Rows per row group", f"min {min(row_counts):,}, max {max(row_counts):,}, "
                                    f"avg {sum(row_counts) / len(row_counts):,.0f}"),
            ("Bytes per row group", f"min {human_readable_bytes(min(byte_sizes))}, "
                                     f"max {human_readable_bytes(max(byte_sizes))}, "
                                     f"avg {human_readable_bytes(sum(byte_sizes) / len(byte_sizes))}"),
        ]
        lines.append(md_table(("Metric", "Value"), rg_rows))
    else:
        lines.append("*(file has no row groups)*\n")  # pragma: no cover -- degenerate empty file
    return "\n".join(lines)


def render_votable_section(table_metadata):
    lines = ["## 2. VOTable metadata\n"]
    xml_text = table_metadata.get(VOTABLE_METADATA_KEY)
    if not xml_text:
        lines.append(f"No VOTable-style metadata (`{VOTABLE_METADATA_KEY}` key) found in this file.\n")
        return "\n".join(lines)

    root = strip_namespace(ET.fromstring(xml_text))
    table = root.find(".//TABLE")
    if table is None:
        lines.append("*(VOTable content present but could not be parsed as expected -- see raw "
                      "value in section 3)*\n")
        return "\n".join(lines)

    params = [(p.get("name", ""), p.get("value", "")) for p in table.findall("PARAM")]
    lines.append(f"Table name: `{table.get('name', '')}`\n")
    lines.append("\n### Table-level parameters\n")
    lines.append(md_table(("Name", "Value"), params))

    field_rows = []
    for field in table.findall("FIELD"):
        desc_elem = field.find("DESCRIPTION")
        description = desc_elem.text.strip() if desc_elem is not None and desc_elem.text else ""
        field_rows.append((
            field.get("name", ""), field.get("datatype", ""),
            field.get("unit", ""), field.get("ucd", ""), description,
        ))
    lines.append("\n### Column attributes\n")
    lines.append(md_table(("Name", "Datatype", "Unit", "UCD", "Description"), field_rows))
    return "\n".join(lines)


def render_key_value_section(table_metadata):
    lines = ["## 3. Parquet key-value metadata\n"]
    rows = []
    for key, value in table_metadata.items():
        if key == VOTABLE_METADATA_KEY:
            rows.append((key, "*(see 'VOTable metadata' section above)*"))
        elif key in KNOWN_OPAQUE_KEYS or len(value) > LONG_VALUE_THRESHOLD:
            rows.append((key, f"*({len(value)} bytes, not decoded)*"))
        else:
            rows.append((key, value))
    lines.append(md_table(("Key", "Value"), rows))
    return "\n".join(lines)


def render_statistics_section(leaves):
    lines = ["## 4. Column statistics\n"]
    rows = []
    for path in sorted(leaves):
        entry = leaves[path]
        if not (entry["has_min_max"] or entry["has_null_count"] or entry["has_distinct_count"]):
            continue
        rows.append((
            path,
            str(entry["min"]) if entry["has_min_max"] else "",
            str(entry["max"]) if entry["has_min_max"] else "",
            f"{entry['null_count']:,}" if entry["has_null_count"] else "",
            f"{entry['distinct_count']:,}" if entry["has_distinct_count"] else "",
        ))
    if not rows:
        lines.append("*(no columns have populated statistics)*\n")
        return "\n".join(lines)
    lines.append("Aggregated across all row groups (min of mins, max of maxes, summed counts).\n")
    lines.append(md_table(("Column", "Min", "Max", "Null count", "Distinct count (approx.)"), rows))
    return "\n".join(lines)


def list_length_summary(pf, field):
    """For a top-level list-like column: the constant length for a fixed_size_list (read straight
    off the schema, no data access needed), or "min X, max Y" for a genuine variable-length
    list/large_list -- Parquet's own column statistics don't record list length, so this is the
    one figure in this report that requires reading a column's actual data (just this one column,
    not the whole file). Returns "" for any other type."""
    if pa.types.is_fixed_size_list(field.type):
        return f"{field.type.list_size} (fixed)"
    if not (pa.types.is_list(field.type) or pa.types.is_large_list(field.type)):
        return ""
    try:
        column = pf.read(columns=[field.name]).column(field.name)
        lengths = pc.list_value_length(column)
        min_max = pc.min_max(lengths, skip_nulls=True)
        min_len, max_len = min_max["min"].as_py(), min_max["max"].as_py()
    except Exception:
        return "*(could not be computed)*"
    if min_len is None:
        return "*(all rows null)*"
    return str(min_len) if min_len == max_len else f"min {min_len}, max {max_len}"


def render_schema_section(pf, leaves, table_metadata):
    lines = ["## 5. Column definitions\n"]

    arrow_fields = None
    try:
        arrow_fields = list(pf.schema_arrow)
    except Exception:  # pragma: no cover -- pyarrow always provides schema_arrow in practice
        pass

    if arrow_fields is not None:
        # A nested type's full str() (e.g. "list<element: struct<a: int32, b: string>>") makes
        # the Type column unreadably wide -- point at the column's own name instead, which is
        # exactly the heading used for it in the "Nested column structure" subsection below.
        top_level = [(f.name, f"**{f.name}** (see below)" if pa.types.is_nested(f.type) else str(f.type),
                      f.nullable, list_length_summary(pf, f)) for f in arrow_fields]
    else:
        top_level = [(pf.schema.names[i], pf.schema.column(i).physical_type, True, "")
                     for i in range(len(pf.schema.names))]

    total_compressed_size = sum(e["compressed_size"] for e in leaves.values())

    rows = []
    for name, type_str, nullable, list_length in top_level:
        group_leaves = {p: e for p, e in leaves.items() if p == name or p.startswith(name + ".")}
        codecs = sorted({c for e in group_leaves.values() for c in e["codecs"]})
        encodings = sorted({enc for e in group_leaves.values() for enc in e["encodings"]})
        compressed_size = sum(e["compressed_size"] for e in group_leaves.values())
        uncompressed_size = sum(e["uncompressed_size"] for e in group_leaves.values())
        pct_of_total = f"{100 * compressed_size / total_compressed_size:.1f}%" if total_compressed_size else ""
        unit = table_metadata.get(f"column.{name}.unit", "")
        ucd = table_metadata.get(f"column.{name}.ucd", "")
        description = table_metadata.get(f"column.{name}.description", "")
        rows.append((
            name, type_str, list_length, "yes" if nullable else "no",
            ", ".join(codecs), ", ".join(encodings),
            human_readable_bytes(compressed_size), human_readable_bytes(uncompressed_size), pct_of_total,
            unit, ucd, description,
        ))
    lines.append(md_table(
        ("Name", "Type", "List length", "Nullable", "Codec(s)", "Encoding(s)", "Compressed size",
         "Uncompressed size", "% of total", "Unit", "UCD", "Description"), rows))

    if arrow_fields is not None:
        nested_blocks = [
            f"**{field.name}**\n```\n" + "\n".join(type_tree_lines(field.type, field.name)) + "\n```"
            for field in arrow_fields if pa.types.is_nested(field.type)
        ]
        if nested_blocks:
            lines.append("\n### Nested column structure\n")
            lines.append("\n\n".join(nested_blocks))

    return "\n".join(lines)


def build_report(input_path):
    pf = pq.ParquetFile(input_path)
    table_metadata = decode_metadata(pf.metadata.metadata)
    leaves = collect_leaf_column_stats(pf.metadata)

    sections = [
        f"# Parquet metadata report: {Path(input_path).name}\n",
        render_file_level_section(pf, input_path),
        render_votable_section(table_metadata),
        render_key_value_section(table_metadata),
        render_statistics_section(leaves),
        render_schema_section(pf, leaves, table_metadata),
    ]
    return "\n\n".join(sections) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("input", help="input Parquet file")
    parser.add_argument("output", nargs="?", default=None,
                         help="output Markdown file (default: input path with .md extension)")
    parser.add_argument("--overwrite", action="store_true", default=OVERWRITE_DEFAULT,
                         help="overwrite the output file if it already exists (default: on -- "
                              "this flag is a no-op unless OVERWRITE_DEFAULT is changed to False)")
    args = parser.parse_args()

    input_path = Path(args.input)
    if not input_path.is_file():
        sys.exit(f"parquet_metadata_to_md.py: input file not found: {input_path}")

    output_path = Path(args.output) if args.output else input_path.with_suffix(OUTPUT_SUFFIX_DEFAULT)
    if output_path.exists() and not args.overwrite:
        print(f"WARNING: output file already exists, not overwriting: {output_path} "
              f"(pass --overwrite to replace it)", file=sys.stderr)
        sys.exit(EXIT_OUTPUT_EXISTS)

    try:
        report = build_report(str(input_path))
    except Exception as exc:
        sys.exit(f"parquet_metadata_to_md.py: failed to read '{input_path}': {exc}")

    output_path.write_text(report, encoding="utf-8")
    print(f"parquet_metadata_to_md.py: wrote {output_path}")
    sys.exit(EXIT_OK)


if __name__ == "__main__":
    main()
