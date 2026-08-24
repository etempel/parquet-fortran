#!/usr/bin/env python3
"""Converts a FITS binary table (astropy) into a Parquet file (pyarrow) carrying
parquet-fortran's own VOTable-style key-value metadata, so parquet_get_metadata can read it
back without going through this library's MAML/Fortran path at all (there is no Python binding
-- this script authors the physical key-value metadata by hand, matching
build_file_metadata() in src/parquet_wrapper.cpp). This includes the embedded VOTable 1.4 XML
sidecar under the "IVOA.VOTable-Parquet.content" key (mirroring build_votable_xml() in the same
source file) -- this, not the flat "column.<name>.*" keys, is what VO-aware tools such as
TOPCAT/STILTS actually read for per-column name/unit/ucd/description.

Only int32/int64/float32/float64/boolean/string are ever written -- the same six types
parquet-fortran's own writer supports (see doc/pages/types/supported-data-types.md). FITS types that
don't map onto one of those cleanly are handled on a best-effort basis:

  - unsigned 8/16-bit integers (B/I, or J/K widened via the TZERO convention) -> int32/int64
  - complex64/complex128 (C/M)   -> split into two columns, "<name>_re"/"<name>_im"
  - bit arrays (X)               -> a boolean vector column, col_size = bit count
  - uint64 values that don't fit in a signed int64 -> the WHOLE column is dropped (there is no
    wider integer type to hold it), a warning is printed, and the column is listed in the
    output file's "not_converted_columns" metadata key

Two FITS features are out of scope entirely (dropped with a warning, listed the same way):
  - variable-length array columns (P/Q descriptors) -- this library only has fixed col_size
  - genuinely multi-dimensional per-row arrays (a TDIM with more than one axis) -- only 1-D
    vector columns (col_size) exist here

Column unit/description/ucd come from TUNITn / TCOMMn (falling back to TTYPEn's own header
comment only if it isn't the generic FITS boilerplate "label for field N") / TUCDn. Other
non-structural header keywords (from both the primary HDU and the table HDU) become flat
table-level metadata entries, one per keyword, using the keyword's own name as the key --
mirroring how a MAML keyarray: entry becomes a metadata entry named by its own key. COMMENT
cards become comment_1, comment_2, ... entries, same as MAML's comments:.

Usage:
    tools/convert_fits_to_parquet.py input.fits [output.parquet] [options]

Examples:
    tools/convert_fits_to_parquet.py test_run/example_1.fits
    tools/convert_fits_to_parquet.py test_run/example_1.fits out.parquet --compression zstd
    tools/convert_fits_to_parquet.py multi.fits out.parquet --hdu 2
    tools/convert_fits_to_parquet.py multi.fits out.parquet --hdu SPECTRA --overwrite

Exit status: 0 on success; 1 on a usage/FITS-structure error (bad --hdu, no binary table HDU
found, no column could be converted at all); 2 if the output file already exists and
--overwrite was not given.

Requires astropy and pyarrow (not part of this repository's own Fortran toolchain).

Converts a FITS binary table into a `.parquet` file carrying
this library's own VOTable-style key-value metadata (`column.<name>.unit`/`description`/`ucd`/
`data_type`/`array_size`/`col_size`, plus flat table-level keys), so `parquet_get_metadata` can
read it back -- a standalone Python tool (requires `astropy` and `pyarrow`, neither part of this
repository's own Fortran toolchain). Not part of the public Fortran library or its API, but
consumer-facing rather than maintainer/CI-only, so it's kept in the fpm-published package (see its
`KEEP_PATHS` entry in `tools/prep_fpm_publish.sh`). Only
int32/int64/float32/float64/boolean/string are ever written; unsigned integers/complex/bit-array
FITS columns are converted on a best-effort basis (widened, split into `_re`/`_im`, or unpacked
into a boolean vector column, respectively -- see the script's own header comment for the full
mapping), and variable-length array columns (FITS `P`/`Q` descriptors) or genuinely multi-
dimensional per-row arrays (`TDIM` with more than one axis) are skipped with a warning and listed
in the output file's `not_converted_columns` metadata entry, rather than converted:

    tools/convert_fits_to_parquet.py data.fits                                  # -> data.parquet, snappy
    tools/convert_fits_to_parquet.py data.fits out.parquet --compression zstd
    tools/convert_fits_to_parquet.py multi_table.fits out.parquet --hdu SPECTRA --overwrite
"""
import argparse
import re
import sys
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
from astropy.io import fits
import pyarrow as pa
import pyarrow.parquet as pq

# ---------------------------------------------------------------------------
# Config: default values for the CLI options below. Change here to change this
# script's defaults without touching the argparse wiring in main().
# ---------------------------------------------------------------------------
OUTPUT_SUFFIX_DEFAULT = ".parquet"    # output path, when omitted: input path with this suffix
COMPRESSION_DEFAULT = "zstd"          # matches this library's own writer default (see writing.md)
COMPRESSION_LEVEL_DEFAULT = 3         # ditto -- Arrow's own zstd default is level 1, not 3
HDU_DEFAULT = None                    # None = first binary table HDU found in the file
OVERWRITE_DEFAULT = False             # False = refuse and exit nonzero if output already exists

EXIT_OK = 0
EXIT_ERROR = 1
EXIT_OUTPUT_EXISTS = 2

# Maps this library's own --compression tokens (doc/pages/types/supported-data-types.md's
# "Compression and row group size") onto the codec names pyarrow's write_table expects.
COMPRESSION_MAP = {
    "uncompressed": "NONE",
    "snappy": "SNAPPY",
    "gzip": "GZIP",
    "zstd": "ZSTD",
    "brotli": "BROTLI",
    "lz4": "LZ4",
}

INT64_MAX = 2**63 - 1

# FITS header keywords that describe file/column *structure* rather than science metadata --
# never carried over as a flat table-level metadata entry (either because they're meaningless
# once translated to Parquet, or because they're already consumed elsewhere, e.g. TUNITn/TCOMMn).
STRUCTURAL_HEADER_PREFIXES = (
    "TTYPE", "TFORM", "TUNIT", "TDIM", "TDISP", "TSCAL", "TZERO", "TNULL", "TCOMM", "TUCD",
    "NAXIS",
)
STRUCTURAL_HEADER_KEYS = {
    "SIMPLE", "BITPIX", "EXTEND", "PCOUNT", "GCOUNT", "TFIELDS", "XTENSION",
    "EXTNAME", "EXTVER", "BSCALE", "BZERO", "CHECKSUM", "DATASUM", "",
    "COMMENT", "HISTORY",
}
# Metadata keys this script (mirroring the real writer) always injects itself -- a same-named
# FITS header keyword is skipped from the generic passthrough rather than silently colliding.
RESERVED_METADATA_KEYS = {"date", "name", "table", "ivoa.votable-parquet.version"}

TFORM_RE = re.compile(r"^(\d*)([PQ])?([A-Z])(\(\d+\))?$")
BOILERPLATE_TTYPE_COMMENT_RE = re.compile(r"^label for field\s+\d+$", re.IGNORECASE)


class SkippedColumn(Exception):
    """Raised to drop a column from the output, carrying the human-readable reason."""

    def __init__(self, reason):
        super().__init__(reason)
        self.reason = reason


def parse_tform(fmt):
    m = TFORM_RE.match(fmt.strip())
    if not m:
        raise SkippedColumn(f"unrecognized TFORM {fmt!r}")
    repeat = int(m.group(1)) if m.group(1) else 1
    varlen = m.group(2)
    code = m.group(3)
    return repeat, varlen, code


def resolve_description(header, col_index):
    tcomm_key = f"TCOMM{col_index}"
    if tcomm_key in header:
        return str(header[tcomm_key]).strip()
    ttype_key = f"TTYPE{col_index}"
    comment = header.comments[ttype_key].strip() if ttype_key in header else ""
    if comment and not BOILERPLATE_TTYPE_COMMENT_RE.match(comment):
        return comment
    return ""


def resolve_ucd(header, col_index):
    key = f"TUCD{col_index}"
    return str(header[key]).strip() if key in header else ""


def integer_target_kind(dtype):
    """Picks int32/int64 for an integer-family FITS column from its actual numpy dtype,
    which already reflects any TZERO-based unsigned emulation astropy applied on open."""
    kind, itemsize = dtype.kind, dtype.itemsize
    if itemsize <= 2:
        return "int32"
    if itemsize == 4:
        return "int32" if kind == "i" else "int64"  # unsigned 32-bit needs the wider type
    return "int64"  # itemsize == 8: signed is a direct fit; unsigned is checked by the caller


def null_mask_for(data, code, header, col_index):
    """Best-effort null detection: NaN for float/complex, TNULL for integers, none otherwise.
    Known limitation: a column combining TNULL with TZERO-based unsigned emulation is checked
    against the already-scaled value, not the original pre-scale sentinel -- vanishingly rare
    in practice, not worth a second unscaled read for."""
    if code in ("E", "D"):
        return np.isnan(data)
    if code in ("C", "M"):
        return np.isnan(data.real) | np.isnan(data.imag)
    if code in ("B", "I", "J", "K"):
        tnull_key = f"TNULL{col_index}"
        if tnull_key in header:
            return data == header[tnull_key]
    return None


def to_native_byteorder(arr):
    """FITS data is big-endian on disk; astropy hands it back byte-swapped on any little-endian
    machine, which pyarrow's zero-copy array constructor refuses outright ("Byte-swapped arrays
    not supported")."""
    if arr.dtype.kind in "iufc" and arr.dtype.byteorder not in ("=", "|"):
        return arr.astype(arr.dtype.newbyteorder("="))
    return arr


def to_pyarrow_scalar_array(values, mask, pa_type):
    if mask is not None and mask.any():
        return pa.array(values, type=pa_type, mask=mask)
    return pa.array(values, type=pa_type)


def to_pyarrow_vector_array(values_2d, mask_2d, pa_elem_type, col_size):
    flat = values_2d.reshape(-1)
    flat_mask = mask_2d.reshape(-1) if mask_2d is not None else None
    child = to_pyarrow_scalar_array(flat, flat_mask, pa_elem_type)
    return pa.FixedSizeListArray.from_arrays(child, col_size)


def convert_column(col, data, header, col_index):
    """Returns a list of (output_name, pa_array, meta) for one FITS column, or raises
    SkippedColumn if it can't be represented in this library's supported type set at all."""
    name = col.name
    repeat, varlen, code = parse_tform(col.format)

    if varlen:
        raise SkippedColumn("variable-length array (P/Q descriptor) not supported")

    dim = col.dim
    if dim is not None and len(dim) > 1:
        raise SkippedColumn("multi-dimensional array (TDIM) not supported")

    unit = str(col.unit).strip() if col.unit else ""
    description = resolve_description(header, col_index)
    ucd = resolve_ucd(header, col_index)
    col_size = repeat

    def meta(data_type, array_size=1):
        return {
            "unit": unit, "description": description, "ucd": ucd,
            "data_type": data_type, "array_size": array_size, "col_size": col_size,
        }

    if code == "L":
        mask = None
        if repeat == 1:
            return [(name, to_pyarrow_scalar_array(data, mask, pa.bool_()), meta("boolean"))]
        return [(name, to_pyarrow_vector_array(data, mask, pa.bool_(), col_size), meta("boolean"))]

    if code == "X":
        # astropy already unpacks bit fields into a boolean ndarray of shape (nrows, repeat).
        if repeat == 1:
            return [(name, to_pyarrow_scalar_array(data.reshape(-1), None, pa.bool_()), meta("boolean"))]
        return [(name, to_pyarrow_vector_array(data, None, pa.bool_(), col_size), meta("boolean"))]

    if code == "A":
        values = [str(v) for v in data]
        return [(name, pa.array(values, type=pa.string()), meta("string", array_size=max(repeat, 1)))]

    if code in ("B", "I", "J", "K"):
        target = integer_target_kind(data.dtype)
        if data.dtype.itemsize == 8 and data.dtype.kind == "u":
            if data.max(initial=0) > INT64_MAX:
                raise SkippedColumn("uint64 value exceeds int64 range (overflow)")
        pa_type = pa.int32() if target == "int32" else pa.int64()
        mask = null_mask_for(data, code, header, col_index)
        values = data.astype(np.int32 if target == "int32" else np.int64)
        if repeat == 1:
            return [(name, to_pyarrow_scalar_array(values, mask, pa_type), meta(target))]
        return [(name, to_pyarrow_vector_array(values, mask, pa_type, col_size), meta(target))]

    if code in ("E", "D"):
        target = "float32" if code == "E" else "float64"
        pa_type = pa.float32() if code == "E" else pa.float64()
        mask = null_mask_for(data, code, header, col_index)
        if repeat == 1:
            return [(name, to_pyarrow_scalar_array(data, mask, pa_type), meta(target))]
        return [(name, to_pyarrow_vector_array(data, mask, pa_type, col_size), meta(target))]

    if code in ("C", "M"):
        target = "float32" if code == "C" else "float64"
        pa_type = pa.float32() if code == "C" else pa.float64()
        mask = null_mask_for(data, code, header, col_index)
        out = []
        for part, suffix, label in ((data.real, "_re", "real part"), (data.imag, "_im", "imaginary part")):
            part_meta = meta(target)
            part_meta["description"] = (description + f" ({label})").strip()
            if repeat == 1:
                out.append((name + suffix, to_pyarrow_scalar_array(part, mask, pa_type), part_meta))
            else:
                out.append((name + suffix, to_pyarrow_vector_array(part, mask, pa_type, col_size), part_meta))
        return out

    raise SkippedColumn(f"unsupported FITS TFORM code {code!r}")


def select_table_hdu(hdul, hdu_arg):
    bintable_indices = [i for i, h in enumerate(hdul) if isinstance(h, fits.BinTableHDU)]
    if not bintable_indices:
        sys.exit("convert_fits_to_parquet.py: no binary table HDU found in input FITS file")
    if hdu_arg is None:
        return hdul[bintable_indices[0]]
    try:
        idx = int(hdu_arg)
    except ValueError:
        for i in bintable_indices:
            if hdul[i].name == hdu_arg:
                return hdul[i]
        sys.exit(f"convert_fits_to_parquet.py: --hdu {hdu_arg!r} does not match any binary "
                 f"table HDU's EXTNAME")
    else:
        if idx not in bintable_indices:
            sys.exit(f"convert_fits_to_parquet.py: --hdu {idx} is not a binary table HDU")
        return hdul[idx]


def build_table_metadata(primary_header, table_header, table_name, not_converted, date):
    meta = {
        "IVOA.VOTable-Parquet.version": "1.0",
        "DATE": date,
        "name": table_name,
        "table": table_name,
    }

    comment_idx = 0
    for header in (primary_header, table_header):
        for card in header.cards:
            if card.keyword == "COMMENT":
                comment_idx += 1
                meta[f"comment_{comment_idx}"] = str(card.value)

    for header in (primary_header, table_header):
        for card in header.cards:
            kw = card.keyword
            if kw in STRUCTURAL_HEADER_KEYS or kw.startswith(STRUCTURAL_HEADER_PREFIXES):
                continue
            if kw.lower() in RESERVED_METADATA_KEYS:
                continue
            meta[kw] = str(card.value)

    if not_converted:
        meta["not_converted_columns"] = "; ".join(f"{n} ({r})" for n, r in not_converted)

    return meta


_XML_ESCAPES = {"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&apos;"}


def xml_escape(s):
    return "".join(_XML_ESCAPES.get(c, c) for c in s)


def build_votable_xml(table_name, column_records, table_metadata, date):
    """Python port of build_votable_xml() in src/parquet_wrapper.cpp -- byte-for-byte the same
    VOTable 1.4 structure parquet-fortran's own writer embeds under the
    "IVOA.VOTable-Parquet.content" key. This, not the flat "column.<name>.*" keys, is what
    VO-aware tools like TOPCAT/STILTS actually parse for column name/unit/ucd/description; the
    flat keys are this library's own private shorthand for its own parquet_get_metadata API."""
    xml = [
        "<?xml version='1.0'?>\n",
        '<VOTABLE version="1.4" xmlns="http://www.ivoa.net/xml/VOTable/v1.3">\n',
        "<RESOURCE>\n",
        f'<TABLE name="{xml_escape(table_name)}">\n',
        f'<PARAM arraysize="19" datatype="char" name="DATE" value="{xml_escape(date)}">\n',
        "<DESCRIPTION>file creation date (YYYY-MM-DDThh:mm:ss UT)</DESCRIPTION>\n",
        "</PARAM>\n",
    ]

    skip_keys = {"ivoa.votable-parquet.version", "date", "name"}
    for key, value in table_metadata.items():
        if key.lower() in skip_keys:
            continue
        xml.append(f'<PARAM datatype="char" arraysize="*" name="{xml_escape(key)}" '
                   f'value="{xml_escape(value)}"/>\n')

    for col in column_records:
        xml.append(f'<FIELD datatype="{xml_escape(col["data_type"])}" name="{xml_escape(col["name"])}"')
        if col["unit"]:
            xml.append(f' unit="{xml_escape(col["unit"])}"')
        if col["ucd"]:
            xml.append(f' ucd="{xml_escape(col["ucd"])}"')
        xml.append(">\n")
        if col["description"]:
            xml.append(f'<DESCRIPTION>{xml_escape(col["description"])}</DESCRIPTION>\n')
        xml.append("</FIELD>\n")

    xml.append("<!-- Dummy VOTable - no DATA element -->\n</TABLE>\n</RESOURCE>\n</VOTABLE>\n")
    return "".join(xml)


def convert(input_path, output_path, compression, compression_level, hdu_arg):
    with fits.open(input_path, uint=True) as hdul:
        table_hdu = select_table_hdu(hdul, hdu_arg)
        header = table_hdu.header
        data = table_hdu.data

        table_name = header.get("EXTNAME", "").strip() or Path(output_path).stem

        arrays = []
        fields = []
        column_meta = {}
        column_records = []
        not_converted = []
        float_columns = []

        for col_index, col in enumerate(table_hdu.columns, start=1):
            try:
                converted = convert_column(col, to_native_byteorder(data[col.name]), header, col_index)
            except SkippedColumn as exc:
                print(f"WARNING: column '{col.name}' not converted: {exc.reason}", file=sys.stderr)
                not_converted.append((col.name, exc.reason))
                continue
            for out_name, arr, meta in converted:
                fields.append(pa.field(out_name, arr.type))
                arrays.append(arr)
                if meta["data_type"] in ("float32", "float64"):
                    float_columns.append(out_name)
                for meta_key, meta_val in meta.items():
                    column_meta[f"column.{out_name}.{meta_key}"] = str(meta_val)
                column_records.append({
                    "name": out_name, "unit": meta["unit"], "description": meta["description"],
                    "ucd": meta["ucd"], "data_type": meta["data_type"],
                })

        if not arrays:
            sys.exit("convert_fits_to_parquet.py: no column could be converted -- nothing to write")

        date = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S")
        table_meta = build_table_metadata(hdul[0].header, header, table_name, not_converted, date)
        table_meta["IVOA.VOTable-Parquet.content"] = build_votable_xml(table_name, column_records, table_meta, date)
        schema = pa.schema(fields, metadata={**table_meta, **column_meta})
        table = pa.Table.from_arrays(arrays, schema=schema)

        # Mirrors apply_float_byte_stream_split() in src/parquet_wrapper.cpp: every float32/
        # float64 column gets BYTE_STREAM_SPLIT with dictionary encoding disabled (dictionary
        # encoding is a no-op unless disabled for that same column -- see CLAUDE.md's "Automatic
        # BYTE_STREAM_SPLIT for float columns in the writer"); every other column keeps pyarrow's
        # normal defaults (dictionary enabled, no BSS).
        dictionary_columns = [f.name for f in fields if f.name not in float_columns]
        pq.write_table(
            table, output_path,
            compression=COMPRESSION_MAP[compression],
            compression_level=compression_level,
            use_dictionary=dictionary_columns,
            use_byte_stream_split=float_columns,
        )

        print(f"convert_fits_to_parquet.py: wrote {output_path} "
              f"({table.num_rows} rows, {table.num_columns} columns"
              f"{', ' + str(len(not_converted)) + ' column(s) skipped' if not_converted else ''})")


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("input", help="input FITS file")
    parser.add_argument("output", nargs="?", default=None,
                         help="output Parquet file (default: input path with .parquet extension)")
    parser.add_argument("--compression", default=COMPRESSION_DEFAULT, type=str.lower,
                         choices=sorted(COMPRESSION_MAP),
                         help=f"Parquet compression codec (default: {COMPRESSION_DEFAULT})")
    parser.add_argument("--compression-level", type=int, default=COMPRESSION_LEVEL_DEFAULT,
                         help=f"compression level, if the codec supports one "
                              f"(default: {COMPRESSION_LEVEL_DEFAULT})")
    parser.add_argument("--hdu", default=HDU_DEFAULT,
                         help="binary table HDU to convert: an integer index or an EXTNAME "
                              "(default: the first binary table HDU in the file)")
    parser.add_argument("--overwrite", action="store_true", default=OVERWRITE_DEFAULT,
                         help="overwrite the output file if it already exists (default: refuse and exit nonzero)")
    args = parser.parse_args()

    input_path = Path(args.input)
    if not input_path.is_file():
        sys.exit(f"convert_fits_to_parquet.py: input file not found: {input_path}")

    output_path = Path(args.output) if args.output else input_path.with_suffix(OUTPUT_SUFFIX_DEFAULT)
    if output_path.exists() and not args.overwrite:
        print(f"WARNING: output file already exists, not overwriting: {output_path} "
              f"(pass --overwrite to replace it)", file=sys.stderr)
        sys.exit(EXIT_OUTPUT_EXISTS)

    convert(str(input_path), str(output_path), args.compression, args.compression_level, args.hdu)
    sys.exit(EXIT_OK)


if __name__ == "__main__":
    main()
