#!/usr/bin/env python3
"""Converts a FITS binary table (astropy) into a Parquet file (pyarrow) carrying
parquet-fortran's own VOTable-style key-value metadata, so parquet_get_metadata can read it
back without going through this library's MAML/Fortran path at all (there is no Python binding
-- this script authors the physical key-value metadata by hand, matching
build_file_metadata() in src/parquet_wrapper.cpp). This includes the embedded VOTable 1.4 XML
sidecar under the "IVOA.VOTable-Parquet.content" key (mirroring build_votable_xml() in the same
source file) -- this, not the flat "column.<name>.*" keys, is what VO-aware tools such as
TOPCAT/STILTS actually read for per-column name/unit/ucd/description.

The conversion is *streamed*: the table is read, converted and written one row-group-sized chunk
at a time (--chunk-size, default 1048576 rows), so peak memory stays proportional to the chunk
rather than to the whole table, and the FITS file is read once front to back instead of once per
column. Whole-table materialisation was the single biggest robustness problem with the earlier
version of this script -- a 30 GB FITS table expands to tens of GB of simultaneously live numpy
and Arrow buffers, which on a large file is enough to push a machine into swap and expose the
conversion to memory corruption (see --verify below).

Streaming alone does not bound resident memory, though, because astropy serves the table through
a *memory map* of the whole file: every page the conversion touches stays resident until the file
is closed, so RSS climbs to the full FITS size (~29 GB for a 29 GB table) no matter how small
--chunk-size is, and climbs a second time during verification. Each pass therefore releases the
pages it has finished with (madvise(MADV_DONTNEED) over the already-consumed byte range), which
holds RSS at roughly one chunk instead. --keep-pages disables that if it is ever unwanted; it is
a caching hint only, and the data is re-read from the file if anything touches those rows again.

Bulk values are taken straight from the mapped record array wherever that is exactly equivalent
to what astropy's own per-field access would return -- i.e. for every column except the ones
astropy has real work to do on (logical L, bit X, and anything carrying TSCAL/TZERO scaling,
which includes TZERO-based unsigned emulation). This skips a full decode/copy per column per
chunk. Character (A) columns benefit most: astropy decodes them to UTF-32 (4 bytes per character,
so a 64A column costs 256 bytes/row) only for the value to be re-encoded to UTF-8 by Arrow, and
building the Arrow string array from the raw fixed-width bytes instead avoids both.

Every conversion is verified by default: once the output has been closed it is re-opened and
compared, value by value, against a fresh independent read of the source FITS rows (--verify /
--no-verify). Any difference -- a wrong value, a lost or spurious null, a row-count or column
mismatch -- is reported with the exact column, absolute row number and both values (integers
additionally with their hex representations and XOR, so a single-bit flip is obvious) and the
script exits 3. Verification costs roughly a second full pass over both files; it is on by
default because a silently corrupt 8 GB Parquet file is far more expensive than the pass.
--verify-only runs that same check on its own, against a Parquet file some earlier run wrote.
--jobs splits it across processes, one whole Parquet row group at a time, which is what makes it
cheap enough not to think about on a large file; it also decides how many threads convert a
chunk's columns while writing. Neither pass is limited by disk throughput -- on a 29 GB table
this writes at a small fraction of what the storage will take, and the time goes on converting
values and on Parquet encoding/compression, both of which are CPU work.

What "independent" means for verification, precisely: the expected values are re-derived from a
fresh read of the FITS file without going through convert_column(), so a mistake in the
conversion path's type/mask/ordering decisions shows up as a mismatch rather than being
reproduced verbatim. The two paths do share the small leaf helpers that define what a FITS value
*is* (trailing-blank stripping, byte-order/type coercion); those are recomputed from the re-read
bytes on every pass, so they still catch the failure the check exists for -- a value that changed
between being written and being read back.

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
    tools/convert_fits_to_parquet.py huge.fits out.parquet --chunk-size 262144 --no-verify
    tools/convert_fits_to_parquet.py huge.fits out.parquet --jobs 16
    tools/convert_fits_to_parquet.py data.fits existing.parquet --verify-only

Exit status: 0 on success; 1 on a usage/FITS-structure error (bad --hdu, no binary table HDU
found, no column could be converted at all); 2 if the output file already exists and
--overwrite was not given; 3 if the file failed verification against the source FITS.

Requires astropy and pyarrow (not part of this repository's own Fortran toolchain).

Converts a FITS binary table into a `.parquet` file carrying
this library's own VOTable-style key-value metadata (`column.<name>.unit`/`description`/`ucd`/
`data_type`/`array_size`/`col_size`, plus flat table-level keys), so `parquet_get_metadata` can
read it back -- a standalone Python tool (requires `astropy` and `pyarrow`, neither part of this
repository's own Fortran toolchain). Not part of the public Fortran library or its API, but
consumer-facing rather than maintainer/CI-only: it is meant to be run by users of this library, and
CONTRIBUTING.md's index table marks it so. Only
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
import atexit
import mmap
import os
import pickle
import re
import sys
from concurrent.futures import ThreadPoolExecutor
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
CHUNK_SIZE_DEFAULT = 1 << 20          # rows per read/convert/write step, = one Parquet row group
VERIFY_DEFAULT = True                 # re-read the written file and compare it against the FITS
JOBS_DEFAULT = min(4, os.cpu_count() or 1)   # threads while converting, processes while verifying
# Rows the verification pass reads back at a time. Deliberately not --chunk-size: that sets the
# row group size, and reading a whole row group at once costs noticeably more memory (and is
# slightly slower) than streaming it in smaller batches, which matters once there is one such
# buffer per worker. Measured on a 27-column table, 8 workers: 6.9 GiB at 1048576 rows against
# 4.6 GiB at 262144, the smaller batch also being the faster of the two.
VERIFY_BATCH_ROWS = 1 << 18
RELEASE_PAGES_DEFAULT = True          # drop already-consumed FITS mmap pages as each pass moves on

EXIT_OK = 0
EXIT_ERROR = 1
EXIT_OUTPUT_EXISTS = 2
EXIT_VERIFY_FAILED = 3

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

# Trailing bytes a fixed-width FITS character field is padded with, and which therefore are not
# part of the value. Space is what the standard mandates; NUL turns up in real files and is what
# numpy's own 'S' dtype drops; the rest are the remaining ASCII whitespace numpy's str.rstrip()
# removes, listed explicitly so this agrees exactly with the decode-then-rstrip path below.
FITS_PAD_BYTES = (0, 9, 10, 11, 12, 13, 32)
_IS_PAD_BYTE = np.zeros(256, dtype=bool)
_IS_PAD_BYTE[list(FITS_PAD_BYTES)] = True

# Arrow's StringArray addresses its value bytes with int32 offsets, so one chunk's characters
# must stay under 2 GiB for the raw-bytes fast path to be usable at all.
ARROW_STRING_OFFSET_MAX = 2**31 - 1


class SkippedColumn(Exception):
    """Raised to drop a column from the output, carrying the human-readable reason."""

    def __init__(self, reason):
        super().__init__(reason)
        self.reason = reason


class VerificationError(Exception):
    """Raised when the written Parquet file does not match the source FITS table."""


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


def as_native(arr, np_dtype):
    """Coerces one column's values to a native-byte-order numpy dtype in a single pass.

    FITS data is big-endian on disk, so on any little-endian machine an untouched column is
    byte-swapped, which pyarrow's zero-copy array constructor refuses outright ("Byte-swapped
    arrays not supported"). numpy does the byte swap and the type conversion together, so this
    must stay one astype() -- doing them separately (swap, then cast) copies the whole column
    twice, which on a chunked conversion is the single largest avoidable cost in the writer.
    copy=False then makes it free for a column that already has the wanted dtype."""
    return arr.astype(np_dtype, copy=False)


def to_native_byteorder(arr):
    """Byte-swaps to native order without changing the type. Kept for the paths that must not
    choose a target type themselves (bit/logical columns, and the complex split)."""
    if arr.dtype.kind in "iufc" and arr.dtype.byteorder not in ("=", "|"):
        return arr.astype(arr.dtype.newbyteorder("="))
    return arr


def stripped_lengths(byte_matrix):
    """Returns, for each row of an (nrows, width) uint8 view of a fixed-width FITS character
    column, the length of the value once trailing pad bytes are removed."""
    nrows, width = byte_matrix.shape
    if width == 0:
        return np.zeros(nrows, dtype=np.int32)
    kept_reversed = ~_IS_PAD_BYTE[byte_matrix[:, ::-1]]
    trailing_pad = kept_reversed.argmax(axis=1)
    all_pad = ~kept_reversed.any(axis=1)
    lengths = (width - trailing_pad).astype(np.int32)
    lengths[all_pad] = 0
    return lengths


def decode_fits_text(data):
    """Returns a plain numpy unicode array for a FITS 'A' (character) column.

    astropy hands character columns back either already decoded ('<U', what a row slice of a
    FITS_rec gives) or as the raw fixed-width bytes ('|S', what whole-column access gives),
    depending on the access path. Both have to end up as real text: calling str() on a numpy
    bytes scalar yields its *repr* ("b'1234'"), not its contents, which silently wraps every
    value in the file in a b'...' shell. Decode explicitly instead, and strip the trailing
    blanks FITS pads fixed-width character fields with (they are not significant per the FITS
    standard, and astropy's own decoded path strips them too, so both access paths agree)."""
    arr = np.asarray(data)
    if arr.dtype.kind == "S":
        try:
            arr = np.asarray(np.char.decode(arr, "utf-8"))
        except UnicodeDecodeError:
            arr = np.asarray(np.char.decode(arr, "latin-1"))
    elif arr.dtype.kind != "U":
        arr = arr.astype("U")
    return np.asarray(np.char.rstrip(arr))


def string_buffers_from_fixed_bytes(raw_bytes):
    """Packs a fixed-width FITS character column into Arrow's own string layout -- an int32
    offsets array plus the concatenated value bytes -- straight from the bytes on disk.

    This is the shape Arrow wants anyway, so building it here avoids the decode-to-UTF-32,
    rstrip, and re-encode-to-UTF-8 round trip that going via numpy unicode text costs (a 64A
    column momentarily needs 256 bytes/row as '<U64', against 64 on disk). Returns None if the
    column cannot take this path -- non-ASCII bytes, which need the encoding fallbacks in
    decode_fits_text(), or more value bytes than Arrow's int32 offsets can address."""
    values = np.ascontiguousarray(raw_bytes)
    nrows = values.shape[0]
    width = values.dtype.itemsize
    byte_matrix = values.view(np.uint8).reshape(nrows, width) if nrows else \
        np.zeros((0, width), dtype=np.uint8)
    lengths = stripped_lengths(byte_matrix)

    offsets = np.empty(nrows + 1, dtype=np.int64)
    offsets[0] = 0
    np.cumsum(lengths, out=offsets[1:])
    if offsets[nrows] > ARROW_STRING_OFFSET_MAX:
        return None

    if nrows and lengths.min() == lengths.max():
        # Every value the same length after stripping (a fixed-format identifier, say): the
        # packed bytes are a plain slice, which skips building the (nrows, width) mask below.
        packed = np.ascontiguousarray(byte_matrix[:, :int(lengths[0])]).reshape(-1)
    else:
        keep = np.arange(width, dtype=np.int32)[None, :] < lengths[:, None]
        packed = byte_matrix[keep]
    if packed.size and packed.max() > 127:
        return None                      # not ASCII -- decode_fits_text() knows what to do
    return offsets.astype(np.int32), packed, lengths


def to_pyarrow_string_array(data):
    """Builds an Arrow string array for one FITS 'A' column, from the raw fixed-width bytes
    where possible and via an explicit decode otherwise."""
    arr = np.asarray(data)
    if arr.dtype.kind == "S" and arr.ndim == 1:
        built = string_buffers_from_fixed_bytes(arr)
        if built is not None:
            offsets, packed, _lengths = built
            return pa.StringArray.from_buffers(len(arr), pa.py_buffer(offsets),
                                               pa.py_buffer(packed), None, 0)
    return pa.array(decode_fits_text(arr), type=pa.string())


def to_pyarrow_scalar_array(values, mask, pa_type):
    if mask is not None and mask.any():
        return pa.array(values, type=pa_type, mask=mask)
    return pa.array(values, type=pa_type)


def to_pyarrow_vector_array(values_2d, mask_2d, pa_elem_type, col_size):
    flat = np.ascontiguousarray(values_2d).reshape(-1)
    flat_mask = np.ascontiguousarray(mask_2d).reshape(-1) if mask_2d is not None else None
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
        data = to_native_byteorder(data)
        mask = None
        if repeat == 1:
            return [(name, to_pyarrow_scalar_array(data, mask, pa.bool_()), meta("boolean"))]
        return [(name, to_pyarrow_vector_array(data, mask, pa.bool_(), col_size), meta("boolean"))]

    if code == "X":
        # astropy already unpacks bit fields into a boolean ndarray of shape (nrows, repeat).
        data = to_native_byteorder(data)
        if repeat == 1:
            return [(name, to_pyarrow_scalar_array(np.ascontiguousarray(data).reshape(-1),
                                                   None, pa.bool_()), meta("boolean"))]
        return [(name, to_pyarrow_vector_array(data, None, pa.bool_(), col_size), meta("boolean"))]

    if code == "A":
        return [(name, to_pyarrow_string_array(data), meta("string", array_size=max(repeat, 1)))]

    if code in ("B", "I", "J", "K"):
        target = integer_target_kind(data.dtype)
        if data.dtype.itemsize == 8 and data.dtype.kind == "u":
            if data.max(initial=0) > INT64_MAX:
                raise SkippedColumn("uint64 value exceeds int64 range (overflow)")
        pa_type = pa.int32() if target == "int32" else pa.int64()
        values = as_native(data, np.int32 if target == "int32" else np.int64)
        mask = null_mask_for(values, code, header, col_index)
        if repeat == 1:
            return [(name, to_pyarrow_scalar_array(values, mask, pa_type), meta(target))]
        return [(name, to_pyarrow_vector_array(values, mask, pa_type, col_size), meta(target))]

    if code in ("E", "D"):
        target = "float32" if code == "E" else "float64"
        pa_type = pa.float32() if code == "E" else pa.float64()
        values = as_native(data, np.float32 if code == "E" else np.float64)
        mask = null_mask_for(values, code, header, col_index)
        if repeat == 1:
            return [(name, to_pyarrow_scalar_array(values, mask, pa_type), meta(target))]
        return [(name, to_pyarrow_vector_array(values, mask, pa_type, col_size), meta(target))]

    if code in ("C", "M"):
        target = "float32" if code == "C" else "float64"
        pa_type = pa.float32() if code == "C" else pa.float64()
        values = as_native(data, np.complex64 if code == "C" else np.complex128)
        mask = null_mask_for(values, code, header, col_index)
        out = []
        for part, suffix, label in ((values.real, "_re", "real part"),
                                    (values.imag, "_im", "imaginary part")):
            part_meta = meta(target)
            part_meta["description"] = (description + f" ({label})").strip()
            if repeat == 1:
                out.append((name + suffix,
                            to_pyarrow_scalar_array(np.ascontiguousarray(part), mask, pa_type),
                            part_meta))
            else:
                out.append((name + suffix,
                            to_pyarrow_vector_array(part, mask, pa_type, col_size), part_meta))
        return out

    raise SkippedColumn(f"unsupported FITS TFORM code {code!r}")


# ---------------------------------------------------------------------------
# Reading the FITS table: raw record access where it is equivalent, and page release
# ---------------------------------------------------------------------------

# Codes whose on-disk bytes are already exactly what convert_column() wants to see, so the
# mapped record array can be read directly. L (logical 'T'/'F' bytes) and X (packed bits) are
# deliberately absent: astropy does real decoding work for those and is the only thing that
# knows how, and they are cheap enough that it does not matter.
RAW_READABLE_CODES = frozenset(("A", "B", "I", "J", "K", "E", "D", "C", "M"))


def unbind_fits_rec_columns(fits_rec):
    """Detaches a FITS_rec from the Column objects it has served values to.

    astropy's FITS_rec.__del__ hands every such Column a private copy of its own data, so that a
    Column outliving the mapping still works. Nothing in this script keeps one, so on a large
    table that copy is pure waste -- and it is not small: it allocates the whole column and
    re-reads the file to fill it, for every column at once. Unbinding first turns the destructor
    back into a no-op. Only ever called when the mapping is being abandoned anyway."""
    weakrefs = getattr(fits_rec, "_col_weakrefs", None)
    if not weakrefs:
        return
    for col in list(weakrefs):
        try:
            col.array = None
        except (AttributeError, TypeError, ValueError):
            pass
    weakrefs.clear()


class ChunkSource:
    """Serves row ranges of one FITS table to the converter and the verifier.

    Two things happen here that a plain `hdu.data[start:stop][name]` does not do. Columns that
    need no interpretation are read straight from the mapped record array, skipping astropy's
    per-field conversion and its cache; and the mapped pages behind each finished row range are
    released, so a sequential pass over a 29 GB table does not end up with 29 GB resident."""

    def __init__(self, table_hdu, release_pages=RELEASE_PAGES_DEFAULT):
        self.hdu = table_hdu
        self.header = table_hdu.header
        self.data = table_hdu.data
        self.raw = self._raw_record_view()
        self._raw_ok = {}
        self._slice = None               # astropy's own view of the current row range, built
        self._slice_key = None           # lazily and only for columns that need its decoding

        self._mmap = getattr(getattr(table_hdu, "_file", None), "_mmap", None)
        self._data_offset = getattr(table_hdu, "_data_offset", None)
        self._itemsize = self.raw.dtype.itemsize if self.raw is not None else None
        self._released_to = 0
        self._release = bool(release_pages) and None not in (
            self._mmap, self._data_offset, self._itemsize) and hasattr(mmap, "MADV_DONTNEED")

    def _raw_record_view(self):
        """The mapped record array behind the FITS_rec, or None if it cannot be trusted to
        line up field-for-field with the column definitions."""
        try:
            raw = self.data.view(np.ndarray)
        except (AttributeError, TypeError, ValueError):
            return None
        names = getattr(raw.dtype, "names", None)
        if not names or len(set(names)) != len(names):
            return None
        return raw

    def use_raw(self, col, code):
        """Whether this column's on-disk bytes can be handed to convert_column() unchanged."""
        cached = self._raw_ok.get(col.name)
        if cached is not None:
            return cached
        ok = False
        if self.raw is not None and code in RAW_READABLE_CODES and col.name in self.raw.dtype.names:
            scaled = (col.bscale is not None and col.bscale != 1) or \
                     (col.bzero is not None and col.bzero != 0)
            field = self.raw.dtype.fields[col.name][0]
            base = field.subdtype[0] if field.subdtype else field
            ok = not scaled and base.kind in "iufcS"
        self._raw_ok[col.name] = ok
        return ok

    def field(self, col, code, start, stop):
        """One column's values for rows [start, stop)."""
        if self.use_raw(col, code):
            return self.raw[col.name][start:stop]
        if self._slice_key != (start, stop):
            self._slice = self.data[start:stop]
            self._slice_key = (start, stop)
        return self._slice[col.name]

    def close(self):
        """Finishes with the mapped table, without paying for astropy's teardown copy.

        This is where the run's memory high-water mark used to be: dropping the table's FITS_rec
        makes astropy copy every column into ordinary memory (see unbind_fits_rec_columns), so a
        29 GB table briefly needed 29 GB of anonymous memory *plus* the mapping it was reading it
        back through -- and it happened after the last row was written, once per pass, which is
        why it looked like the memory doubled the moment the conversion said it was done."""
        for rec in (self._slice, self.data):
            if rec is not None:
                unbind_fits_rec_columns(rec)
        self._slice = self._slice_key = self.raw = self.data = None

    def begin_at(self, row):
        """Marks every page before `row` as already finished with, so a pass that starts part
        way into the table (one verification worker's share of the row groups) does not hand
        madvise a range covering rows it never touches."""
        if not self._release:
            return
        start = self._data_offset + row * self._itemsize
        self._released_to = max(self._released_to, (start // mmap.PAGESIZE) * mmap.PAGESIZE)

    def release_through(self, row):
        """Drops the mapped pages holding every row before `row`.

        MADV_DONTNEED is a caching hint on a file-backed mapping: it costs the pages their
        resident status, and anything touching those rows again simply reads them from the file
        once more. Each pass here is strictly sequential, so nothing does."""
        if not self._release:
            return
        end = self._data_offset + row * self._itemsize
        page_end = (end // mmap.PAGESIZE) * mmap.PAGESIZE
        if page_end <= self._released_to:
            return
        start = max(self._released_to, (self._data_offset // mmap.PAGESIZE) * mmap.PAGESIZE)
        try:
            self._mmap.madvise(mmap.MADV_DONTNEED, start, page_end - start)
        except (OSError, ValueError):
            self._release = False        # not supported here; stop trying
            return
        self._released_to = page_end


# ---------------------------------------------------------------------------
# Streaming conversion
# ---------------------------------------------------------------------------

# Codecs that accept a --compression-level. The others (snappy, lz4, uncompressed) raise
# outright if handed one, so the default level must not be passed along with them.
LEVELLED_CODECS = frozenset(name for name in COMPRESSION_MAP
                            if name != "uncompressed"
                            and pa.Codec.supports_compression_level(name))


def effective_compression_level(compression, compression_level):
    return compression_level if compression in LEVELLED_CODECS else None


def iter_chunks(nrows, chunk_size):
    """Yields (start, stop) row ranges covering the table. A zero-row table still yields one
    empty range, so the output gets a schema (and an empty row group) rather than nothing."""
    if nrows == 0:
        yield 0, 0
        return
    for start in range(0, nrows, chunk_size):
        yield start, min(start + chunk_size, nrows)


def progress(message, final=False):
    if not sys.stderr.isatty():
        if final:
            print(message, file=sys.stderr)
        return
    sys.stderr.write("\r\033[K" + message + ("\n" if final else ""))
    sys.stderr.flush()


def scan_uint64_overflow(source, columns, nrows, chunk_size):
    """Returns the set of uint64 column names holding a value too large for int64.

    Whether such a column can be converted at all is a property of the *whole* column, but the
    streaming writer has to fix the output schema before it has seen every row -- so the few
    columns that are actually uint64 get a cheap read-only pre-pass. Columns of any other type
    are never touched, so on the common case this costs nothing."""
    if nrows == 0:
        return set()
    probe = source.data[0:1]
    candidates = [c for c in columns if probe[c.name].dtype.kind == "u"
                  and probe[c.name].dtype.itemsize == 8]
    if not candidates:
        return set()
    overflowing = set()
    for start, stop in iter_chunks(nrows, chunk_size):
        chunk = source.data[start:stop]
        for col in list(candidates):
            if chunk[col.name].max(initial=0) > INT64_MAX:
                overflowing.add(col.name)
                candidates.remove(col)
        if not candidates:
            break
    return overflowing


def plan_conversion(source, nrows, chunk_size):
    """First pass over the table: works out, from the header and a single probe row, which
    columns convert and to what, without reading the bulk of the file.

    Returns (plan, fields, column_meta, column_records, float_columns, not_converted) where
    plan is one entry per *output* column, carrying everything both the writer and the
    verifier need."""
    table_hdu = source.hdu
    header = table_hdu.header
    overflowing = scan_uint64_overflow(source, table_hdu.columns, nrows, chunk_size)

    plan, fields, column_records, float_columns, not_converted = [], [], [], [], []
    column_meta = {}

    for col_index, col in enumerate(table_hdu.columns, start=1):
        try:
            if col.name in overflowing:
                raise SkippedColumn("uint64 value exceeds int64 range (overflow)")
            repeat, _varlen, code = parse_tform(col.format)
            probe = source.field(col, code, 0, 1 if nrows else 0)
            converted = convert_column(col, probe, header, col_index)
        except SkippedColumn as exc:
            print(f"WARNING: column '{col.name}' not converted: {exc.reason}", file=sys.stderr)
            not_converted.append((col.name, exc.reason))
            continue
        for out_name, arr, meta in converted:
            fields.append(pa.field(out_name, arr.type))
            if meta["data_type"] in ("float32", "float64"):
                float_columns.append(out_name)
            for meta_key, meta_val in meta.items():
                column_meta[f"column.{out_name}.{meta_key}"] = str(meta_val)
            column_records.append({
                "name": out_name, "unit": meta["unit"], "description": meta["description"],
                "ucd": meta["ucd"], "data_type": meta["data_type"],
            })
            plan.append({
                "name": out_name, "source": col.name, "col_index": col_index, "code": code,
                "repeat": repeat, "data_type": meta["data_type"],
                "part": "im" if out_name.endswith("_im") and code in ("C", "M") else
                        ("re" if code in ("C", "M") else None),
            })

    return plan, fields, column_meta, column_records, float_columns, not_converted


def convert_chunk(source, start, stop, plan_columns, pool=None):
    """Converts one row range into the list of Arrow arrays making up a single row group.

    The per-column work is independent, and nearly all of it (byte-swapping, type conversion,
    string packing, Arrow array construction) happens in numpy and Arrow with the GIL released,
    so handing the columns to a thread pool actually overlaps. Reading the values out of the
    table stays on this thread: it is cheap for the columns served straight from the mapping,
    and for the ones astropy has to decode it touches the shared per-range slice."""
    header = source.header
    inputs = [(col_index, col, source.field(col, code, start, stop))
              for col_index, col, code in plan_columns]
    if pool is None:
        results = [convert_column(col, values, header, ci) for ci, col, values in inputs]
    else:
        futures = [pool.submit(convert_column, col, values, header, ci)
                   for ci, col, values in inputs]
        results = [f.result() for f in futures]
    arrays = []
    for converted in results:
        arrays.extend(arr for _name, arr, _meta in converted)
    return arrays


# ---------------------------------------------------------------------------
# HDU selection and file-level metadata
# ---------------------------------------------------------------------------

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


def convert(input_path, output_path, compression, compression_level, hdu_arg, chunk_size,
            release_pages=RELEASE_PAGES_DEFAULT, jobs=1):
    """Streams the table into the output file one chunk at a time. Returns (plan, nrows)."""
    with fits.open(input_path, uint=True) as hdul:
        source = ChunkSource(select_table_hdu(hdul, hdu_arg), release_pages=release_pages)
        try:
            return _convert_streaming(source, hdul[0].header, output_path, compression,
                                      compression_level, chunk_size, jobs)
        finally:
            # Always before the mapping is dropped -- see ChunkSource.close().
            source.close()


def _convert_streaming(source, primary_header, output_path, compression, compression_level,
                       chunk_size, jobs=1):
    """The body of convert(), with the table already open and owned by the caller."""
    table_hdu = source.hdu
    header = source.header
    nrows = int(header.get("NAXIS2", len(source.data)))

    table_name = header.get("EXTNAME", "").strip() or Path(output_path).stem

    (plan, fields, column_meta, column_records,
     float_columns, not_converted) = plan_conversion(source, nrows, chunk_size)

    if not plan:
        sys.exit("convert_fits_to_parquet.py: no column could be converted -- nothing to write")

    converted_sources = {entry["source"] for entry in plan}
    code_of = {entry["source"]: entry["code"] for entry in plan}
    plan_columns = [(i, col, code_of[col.name])
                    for i, col in enumerate(table_hdu.columns, start=1)
                    if col.name in converted_sources]

    date = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S")
    table_meta = build_table_metadata(primary_header, header, table_name, not_converted, date)
    table_meta["IVOA.VOTable-Parquet.content"] = build_votable_xml(table_name, column_records,
                                                                   table_meta, date)
    schema = pa.schema(fields, metadata={**table_meta, **column_meta})

    # Mirrors apply_float_byte_stream_split() in src/parquet_wrapper.cpp: every float32/
    # float64 column gets BYTE_STREAM_SPLIT with dictionary encoding disabled (dictionary
    # encoding is a no-op unless disabled for that same column -- see CLAUDE.md's "Automatic
    # BYTE_STREAM_SPLIT for float columns in the writer"); every other column keeps pyarrow's
    # normal defaults (dictionary enabled, no BSS).
    dictionary_columns = [f.name for f in fields if f.name not in float_columns]

    written = 0
    pool = ThreadPoolExecutor(max_workers=jobs) if jobs > 1 else None
    try:
        with pq.ParquetWriter(
            output_path, schema,
            compression=COMPRESSION_MAP[compression],
            compression_level=effective_compression_level(compression, compression_level),
            use_dictionary=dictionary_columns,
            use_byte_stream_split=float_columns,
        ) as writer:
            for start, stop in iter_chunks(nrows, chunk_size):
                arrays = convert_chunk(source, start, stop, plan_columns, pool)
                batch = pa.Table.from_arrays(arrays, schema=schema)
                if batch.num_rows != stop - start:
                    raise VerificationError(
                        f"chunk starting at row {start} produced {batch.num_rows} rows, "
                        f"expected {stop - start}")
                writer.write_table(batch, row_group_size=max(stop - start, 1))
                written += stop - start
                del arrays, batch
                source.release_through(stop)
                progress(f"  writing: {written}/{nrows} rows "
                         f"({100.0 * written / nrows if nrows else 100.0:.1f}%)")
        progress(f"  writing: {written}/{nrows} rows (100.0%)", final=True)
    finally:
        if pool is not None:
            pool.shutdown()

    print(f"convert_fits_to_parquet.py: wrote {output_path} "
          f"({nrows} rows, {len(fields)} columns"
          f"{', ' + str(len(not_converted)) + ' column(s) skipped' if not_converted else ''})")

    return plan, nrows


# ---------------------------------------------------------------------------
# Verification: read the written file back and compare it against the FITS source
# ---------------------------------------------------------------------------

def arrow_values_and_nulls(arr):
    """Flattens one Arrow column of a read-back batch into (values, null_mask) numpy arrays.
    Fixed-size list (vector) columns are flattened to their elements in row-major order, the
    same order the writer built them in. null_mask is None when the column holds no nulls at
    all, which is the usual case and saves materialising a full-length boolean array."""
    if pa.types.is_fixed_size_list(arr.type):
        arr = arr.flatten()
    if arr.null_count == 0:
        return arr.to_numpy(zero_copy_only=False), None
    nulls = arr.is_null().to_numpy(zero_copy_only=False)
    return arr.fill_null(0).to_numpy(zero_copy_only=False), nulls


def fits_expected_values(entry, raw, header):
    """Independently re-derives what one output column's values should be, straight from the
    FITS rows -- deliberately *not* by calling convert_column(), so that a mistake in the
    conversion path's type/mask/ordering decisions shows up as a mismatch here instead of being
    reproduced verbatim. Returns (values, null_mask), null_mask None when there are none."""
    code, col_index = entry["code"], entry["col_index"]

    if code in ("L", "X"):
        return np.ascontiguousarray(to_native_byteorder(raw)).reshape(-1).astype(bool,
                                                                                 copy=False), None

    if code in ("B", "I", "J", "K"):
        values = as_native(raw, np.int32 if entry["data_type"] == "int32" else np.int64).reshape(-1)
        tnull_key = f"TNULL{col_index}"
        nulls = None
        if tnull_key in header:
            nulls = values == header[tnull_key]
            if not nulls.any():
                nulls = None
        return values, nulls

    if code in ("E", "D"):
        values = as_native(raw, np.float32 if code == "E" else np.float64).reshape(-1)
        nulls = np.isnan(values)
        return values, (nulls if nulls.any() else None)

    if code in ("C", "M"):
        values = as_native(raw, np.complex64 if code == "C" else np.complex128)
        nulls = np.isnan(values.real) | np.isnan(values.imag)
        part = values.imag if entry["part"] == "im" else values.real
        return (np.ascontiguousarray(part).reshape(-1),
                (nulls.reshape(-1) if nulls.any() else None))

    raise VerificationError(f"column '{entry['name']}': cannot verify TFORM code {code!r}")


def describe_value(value):
    """Renders one mismatching value; numbers also in hex (the raw IEEE bits for floats), so a
    bit flip reads at a glance rather than hiding behind decimal rounding."""
    if isinstance(value, (np.integer, int)) and not isinstance(value, bool):
        return f"{int(value)} (0x{int(value) & 0xFFFFFFFFFFFFFFFF:016x})"
    if isinstance(value, np.float32):
        return f"{float(value)!r} (0x{value.view(np.uint32):08x})"
    if isinstance(value, (np.float64, float)):
        return f"{float(value)!r} (0x{np.float64(value).view(np.uint64):016x})"
    return repr(value)


def describe_mismatch(entry, index, row_offset, expected, actual, kind):
    """Formats one mismatch. Integers get their hex form and XOR too, so a single-bit flip --
    the signature of memory corruption rather than a conversion mistake -- is visible at once."""
    col_size = max(entry["repeat"], 1) if entry["code"] != "A" else 1
    row = row_offset + index // col_size
    element = index % col_size
    where = f"row {row}" + (f", element {element}" if col_size > 1 else "")
    detail = f"    FITS:    {describe_value(expected)}\n    Parquet: {describe_value(actual)}"
    if isinstance(expected, (np.integer, int)) and isinstance(actual, (np.integer, int)) \
            and not isinstance(expected, bool):
        xor = int(expected) ^ int(actual)
        bits = bin(xor & 0xFFFFFFFFFFFFFFFF).count("1")
        detail += f"\n    XOR:     0x{xor & 0xFFFFFFFFFFFFFFFF:016x} ({bits} bit(s) differ)"
    return f"column '{entry['name']}': {kind} mismatch at {where}\n{detail}"


def first_true(mask):
    """Index of the first set element of a boolean array, or -1. argmax stops caring after the
    first hit and allocates nothing, unlike flatnonzero over a whole column."""
    if not mask.any():
        return -1
    return int(mask.argmax())


def compare_string_column(entry, arr, raw, row_offset):
    """Compares one FITS character column against the read-back Arrow string array.

    Both sides are handled as bytes: the Arrow array's own offsets/data buffers are read
    in place, and the expected side is packed into the same layout, so a 64A column is compared
    without ever building a Python string or a UTF-32 copy of itself."""
    nrows = len(raw)
    if arr.null_count:
        idx = first_true(arr.is_null().to_numpy(zero_copy_only=False))
        return describe_mismatch(entry, idx, row_offset, "not null", "null", "null")

    built = string_buffers_from_fixed_bytes(raw)
    if built is None:
        # Non-ASCII or an oversized chunk: fall back to comparing decoded text object by object.
        expected = decode_fits_text(raw).astype(object).reshape(-1)
        actual = np.asarray(arr.to_pylist(), dtype=object)
        if len(actual) != len(expected):
            return (f"column '{entry['name']}': read back {len(actual)} values for rows "
                    f"{row_offset}..{row_offset + len(expected)}, expected {len(expected)}")
        idx = first_true(actual != expected)
        if idx < 0:
            return None
        return describe_mismatch(entry, idx, row_offset, expected[idx], actual[idx], "value")

    exp_offsets, exp_packed, exp_lengths = built
    if len(arr) != nrows:
        return (f"column '{entry['name']}': read back {len(arr)} values for rows "
                f"{row_offset}..{row_offset + nrows}, expected {nrows}")

    buffers = arr.buffers()
    act_offsets = np.frombuffer(buffers[1], dtype=np.int32, count=nrows + 1, offset=arr.offset * 4)
    act_lengths = act_offsets[1:] - act_offsets[:-1]
    idx = first_true(act_lengths != exp_lengths)
    if idx >= 0:
        return describe_mismatch(entry, idx, row_offset, raw[idx].rstrip().decode("latin-1"),
                                 arr[idx].as_py(), "value")

    begin, end = int(act_offsets[0]), int(act_offsets[nrows])
    act_packed = np.frombuffer(buffers[2], dtype=np.uint8, count=end - begin, offset=begin) \
        if end > begin else np.zeros(0, dtype=np.uint8)
    if np.array_equal(act_packed, exp_packed):
        return None
    byte_idx = first_true(act_packed != exp_packed)
    idx = int(np.searchsorted(exp_offsets, byte_idx, side="right")) - 1
    return describe_mismatch(entry, idx, row_offset, raw[idx].rstrip().decode("latin-1"),
                             arr[idx].as_py(), "value")


def compare_column(entry, arrow_array, raw, header, row_offset):
    """Returns a description of the first difference in this chunk, or None if it matches."""
    if entry["code"] == "A":
        return compare_string_column(entry, arrow_array, np.ascontiguousarray(raw), row_offset)

    expected, expected_nulls = fits_expected_values(entry, raw, header)
    actual, actual_nulls = arrow_values_and_nulls(arrow_array)

    if len(actual) != len(expected):
        return (f"column '{entry['name']}': read back {len(actual)} values for rows "
                f"{row_offset}..{row_offset + len(expected)}, expected {len(expected)}")

    if expected_nulls is not None or actual_nulls is not None:
        exp_n = expected_nulls if expected_nulls is not None else np.zeros(len(expected), bool)
        act_n = actual_nulls if actual_nulls is not None else np.zeros(len(actual), bool)
        idx = first_true(act_n != exp_n)
        if idx >= 0:
            return describe_mismatch(entry, idx, row_offset,
                                     "null" if exp_n[idx] else "not null",
                                     "null" if act_n[idx] else "not null", "null")
        valid = ~exp_n
        if not valid.any():
            return None
    else:
        valid = None

    if entry["data_type"] in ("float32", "float64"):
        # Compare the raw IEEE bit patterns, so 0.0/-0.0 and any NaN payload difference count
        # as a difference too -- Parquet stores the bits verbatim, so anything else is a bug.
        int_view = np.uint32 if entry["data_type"] == "float32" else np.uint64
        exp_cmp = np.ascontiguousarray(expected).view(int_view)
        act_cmp = np.ascontiguousarray(actual).view(int_view)
    else:
        exp_cmp, act_cmp = expected, actual

    if valid is None:
        if np.array_equal(exp_cmp, act_cmp):
            return None
        idx = first_true(exp_cmp != act_cmp)
    else:
        idx = first_true(valid & (exp_cmp != act_cmp))
    if idx < 0:
        return None
    return describe_mismatch(entry, idx, row_offset, expected[idx], actual[idx], "value")


def row_group_spans(parquet_file):
    """(row_group_index, first_row, last_row+1) for every row group, in file order."""
    metadata = parquet_file.metadata
    spans, start = [], 0
    for i in range(metadata.num_row_groups):
        nrows = metadata.row_group(i).num_rows
        spans.append((i, start, start + nrows))
        start += nrows
    return spans


def verify_spans(source, parquet_file, plan, columns_by_name, spans, batch_rows,
                 on_progress=None):
    """Compares the given row groups against the FITS source. Returns (rows, failures), where
    failures maps a column name to (first_failing_row, message)."""
    failures = {}
    checked = 0
    header = source.header
    if spans:
        source.begin_at(spans[0][1])
    for _rg, start, stop in spans:
        offset = start
        for batch in parquet_file.iter_batches(batch_size=batch_rows, row_groups=[_rg]):
            nrows = batch.num_rows
            for position, entry in enumerate(plan):
                name = entry["name"]
                if name in failures:
                    continue
                col_index, col = columns_by_name[entry["source"]]
                raw = source.field(col, entry["code"], offset, offset + nrows)
                problem = compare_column(entry, batch.column(position), raw, header, offset)
                if problem is not None:
                    failures[name] = (offset, problem)
            offset += nrows
            checked += nrows
            del batch
            if on_progress is not None:
                on_progress(checked, failures)
        source.release_through(stop)
    return checked, failures


# One-time per-worker state for the parallel verification pass: each worker opens the FITS and
# the Parquet file once and then handles whole row groups, so nothing large is ever pickled.
_WORKER = {}


def _verify_worker_init(input_path, output_path, hdu_arg, plan, release_pages):
    hdul = fits.open(input_path, uint=True)
    table_hdu = select_table_hdu(hdul, hdu_arg)
    source = ChunkSource(table_hdu, release_pages=release_pages)
    # Without this every worker would pay astropy's teardown copy of the whole table as it
    # exits, all of them at once, right at the end of an otherwise cheap verification.
    atexit.register(source.close)
    _WORKER.update(
        hdul=hdul,
        source=source,
        parquet_file=pq.ParquetFile(output_path),
        plan=plan,
        columns_by_name={col.name: (i, col)
                         for i, col in enumerate(table_hdu.columns, start=1)},
    )


def _verify_worker(task):
    spans, batch_rows = task
    checked, failures = verify_spans(_WORKER["source"], _WORKER["parquet_file"], _WORKER["plan"],
                                     _WORKER["columns_by_name"], spans, batch_rows)
    return checked, failures


def plan_only(input_path, hdu_arg, chunk_size, release_pages=RELEASE_PAGES_DEFAULT):
    """Works out the same (plan, nrows) convert() would, without writing anything -- what
    --verify-only needs to check a Parquet file some earlier run produced."""
    with fits.open(input_path, uint=True) as hdul:
        table_hdu = select_table_hdu(hdul, hdu_arg)
        source = ChunkSource(table_hdu, release_pages=release_pages)
        nrows = int(table_hdu.header.get("NAXIS2", len(source.data)))
        try:
            plan, *_rest = plan_conversion(source, nrows, chunk_size)
        finally:
            source.close()
        if not plan:
            sys.exit("convert_fits_to_parquet.py: no column could be converted -- nothing to verify")
        return plan, nrows


def verify(input_path, output_path, hdu_arg, plan, expected_rows, chunk_size, jobs=1,
           release_pages=RELEASE_PAGES_DEFAULT):
    """Re-reads the written Parquet file and compares every value against a fresh, independent
    read of the source FITS table.

    A column that has already failed is dropped from the remaining comparison, so one broken
    column cannot mask a different problem in another one -- the check reports the first
    difference in each affected column rather than stopping at the very first difference in the
    file. Structural problems (row count, column set) still abort immediately, since nothing
    below them can be compared meaningfully.

    With jobs > 1 the row groups are shared out over worker processes. Each one re-reads its own
    rows from both files, so the comparison is exactly the same; only the order in which the
    columns fail (and so which row a column is first reported at) depends on the split."""
    parquet_file = pq.ParquetFile(output_path)
    metadata = parquet_file.metadata
    if metadata.num_rows != expected_rows:
        raise VerificationError(
            f"row count mismatch: FITS has {expected_rows} rows, Parquet has {metadata.num_rows}")

    expected_names = [entry["name"] for entry in plan]
    actual_names = [parquet_file.schema_arrow.field(i).name
                    for i in range(len(parquet_file.schema_arrow))]
    if actual_names != expected_names:
        raise VerificationError(
            f"column mismatch: expected {expected_names}, Parquet has {actual_names}")

    batch_rows = max(1, min(chunk_size, VERIFY_BATCH_ROWS))
    spans = row_group_spans(parquet_file)
    total_rows = spans[-1][2] if spans else 0
    if total_rows != expected_rows:
        raise VerificationError(
            f"row group sizes sum to {total_rows} rows, but the file header says {expected_rows}")

    def report(checked, failures):
        progress(f"  verifying: {checked}/{expected_rows} rows "
                 f"({100.0 * checked / expected_rows if expected_rows else 100.0:.1f}%)"
                 + (f", {len(failures)} column(s) failed" if failures else ""))

    jobs = max(1, min(int(jobs), len(spans)))
    if jobs > 1:
        try:
            checked, failures = _verify_parallel(input_path, output_path, hdu_arg, plan, spans,
                                                 batch_rows, jobs, expected_rows, report,
                                                 release_pages)
        except (AttributeError, ImportError, OSError, pickle.PicklingError) as exc:
            # Worker processes are only available when this module can be imported by name in
            # the child (running it as a script, or importing it normally, both qualify). Where
            # it cannot be, verification still has to happen -- just in this process.
            print(f"WARNING: --jobs {jobs} unavailable ({exc}); verifying in a single process",
                  file=sys.stderr)
            jobs = 1
    if jobs == 1:
        with fits.open(input_path, uint=True) as hdul:
            table_hdu = select_table_hdu(hdul, hdu_arg)
            source = ChunkSource(table_hdu, release_pages=release_pages)
            columns_by_name = {col.name: (i, col)
                               for i, col in enumerate(table_hdu.columns, start=1)}
            try:
                checked, failures = verify_spans(source, parquet_file, plan, columns_by_name,
                                                 spans, batch_rows, on_progress=report)
            finally:
                source.close()

    if not failures and checked != expected_rows:
        raise VerificationError(f"verified only {checked} of {expected_rows} rows")

    progress(f"  verifying: {checked}/{expected_rows} rows"
             + (f", {len(failures)} column(s) failed" if failures else " (100.0%)"), final=True)

    if failures:
        raise VerificationError(
            f"{len(failures)} of {len(plan)} column(s) do not match the FITS source:\n"
            + "\n".join(failures[name][1] for name in expected_names if name in failures))


def _verify_parallel(input_path, output_path, hdu_arg, plan, spans, batch_rows, jobs,
                     expected_rows, report, release_pages):
    """Runs verify_spans() over worker processes, one task per row group."""
    import concurrent.futures as cf

    failures, checked = {}, 0
    tasks = [([span], batch_rows) for span in spans]
    with cf.ProcessPoolExecutor(max_workers=jobs, initializer=_verify_worker_init,
                                initargs=(input_path, output_path, hdu_arg, plan,
                                          release_pages)) as pool:
        for rows, found in pool.map(_verify_worker, tasks):
            checked += rows
            for name, (row, message) in found.items():
                if name not in failures or row < failures[name][0]:
                    failures[name] = (row, message)
            report(checked, failures)
    return checked, failures


def report_verification_failure(output_path, exc):
    """Prints the failure and exits 3. The re-run advice matters: a difference that reappears at
    the same row is a bug in this script, whereas one that moves or vanishes on a second run is
    the signature of memory or storage corruption on the machine doing the conversion."""
    print(f"ERROR: convert_fits_to_parquet.py: verification FAILED for {output_path}\n"
          f"  {exc}\n"
          f"  The file does not faithfully represent the input and must not be used. Re-run the "
          f"conversion; if the difference reappears at the same row, it is a conversion bug, and "
          f"if it moves or disappears it points at memory or storage corruption on this machine.",
          file=sys.stderr)
    sys.exit(EXIT_VERIFY_FAILED)


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
    parser.add_argument("--chunk-size", type=int, default=CHUNK_SIZE_DEFAULT,
                         help=f"rows converted and written at a time, one Parquet row group each; "
                              f"caps peak memory use (default: {CHUNK_SIZE_DEFAULT})")
    parser.add_argument("--jobs", type=int, default=JOBS_DEFAULT,
                         help=f"parallelism: threads converting a chunk's columns while "
                              f"writing, worker processes (one Parquet row group each) while "
                              f"verifying. A verification worker holds its own read buffers, so "
                              f"peak memory grows by roughly half a GiB per worker on a wide "
                              f"table; 1 keeps both passes serial (default: {JOBS_DEFAULT})")
    parser.add_argument("--keep-pages", dest="release_pages", action="store_false",
                         default=RELEASE_PAGES_DEFAULT,
                         help="do not release already-read FITS memory-map pages as each pass "
                              "moves through the file (raises peak RSS to the whole file size)")
    parser.add_argument("--verify", dest="verify", action="store_true", default=VERIFY_DEFAULT,
                         help="re-read the written file and compare it against the FITS source "
                              "(default: on)")
    parser.add_argument("--no-verify", dest="verify", action="store_false",
                         help="skip the read-back verification pass (faster, but the output is "
                              "then unchecked)")
    parser.add_argument("--verify-only", action="store_true",
                         help="do not convert anything: check an existing Parquet file against "
                              "the FITS input and report the first difference")
    args = parser.parse_args()

    if args.chunk_size < 1:
        sys.exit("convert_fits_to_parquet.py: --chunk-size must be at least 1")
    if args.jobs < 1:
        sys.exit("convert_fits_to_parquet.py: --jobs must be at least 1")

    input_path = Path(args.input)
    if not input_path.is_file():
        sys.exit(f"convert_fits_to_parquet.py: input file not found: {input_path}")

    output_path = Path(args.output) if args.output else input_path.with_suffix(OUTPUT_SUFFIX_DEFAULT)

    if args.verify_only:
        if not output_path.is_file():
            sys.exit(f"convert_fits_to_parquet.py: file to verify not found: {output_path}")
        plan, nrows = plan_only(str(input_path), args.hdu, args.chunk_size, args.release_pages)
        try:
            verify(str(input_path), str(output_path), args.hdu, plan, nrows, args.chunk_size,
                   args.jobs, args.release_pages)
        except VerificationError as exc:
            report_verification_failure(output_path, exc)
        print(f"convert_fits_to_parquet.py: verified {output_path} against {input_path} "
              f"({nrows} rows, {len(plan)} columns) -- all values match")
        sys.exit(EXIT_OK)

    if output_path.exists() and not args.overwrite:
        print(f"WARNING: output file already exists, not overwriting: {output_path} "
              f"(pass --overwrite to replace it)", file=sys.stderr)
        sys.exit(EXIT_OUTPUT_EXISTS)

    try:
        plan, nrows = convert(str(input_path), str(output_path), args.compression,
                              args.compression_level, args.hdu, args.chunk_size,
                              args.release_pages, args.jobs)
    except VerificationError as exc:
        print(f"ERROR: convert_fits_to_parquet.py: {exc}", file=sys.stderr)
        sys.exit(EXIT_VERIFY_FAILED)

    if args.verify:
        try:
            verify(str(input_path), str(output_path), args.hdu, plan, nrows, args.chunk_size,
                   args.jobs, args.release_pages)
        except VerificationError as exc:
            report_verification_failure(output_path, exc)
        print(f"convert_fits_to_parquet.py: verified {output_path} against {input_path} "
              f"({nrows} rows, {len(plan)} columns) -- all values match")

    sys.exit(EXIT_OK)


if __name__ == "__main__":
    main()
