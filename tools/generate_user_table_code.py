#!/usr/bin/env python3
"""Generate a named `parquet_table` extension type from a MAML schema.

A "Role-A" MAML -- one whose `fields:` describe the columns a program always wants -- becomes a
Fortran module holding `parquet_table_<table>`, an extension of `parquet_table` with one accessor
per declared column:

    call t%init("catalogue.parquet")   ! opens, checks, converts, materializes
    ra => t%ra()                       ! zero-copy pointer into the live storage
    print *, t%ra(3), t%ra(3, 6)       ! one row; a row range -- also zero copy

This script is CONSUMER-FACING, exactly like tools/generate_parquet_maml.sh: a project depending
on parquet-fortran copies it into its own tools/, keeps its Role-A MAMLs in `table_types/`, and
COMMITS the generated .f90 so its own fpm build stays dependency-free. It is stdlib-only for the
same reason the other generators are -- CI's lint job installs python3 and nothing else.

  Usage:  tools/generate_user_table_code.py [--dir=DIR] [MAML ...] [--out-dir=DIR]
                                            [--check] [--self-test]

    --dir       directory of Role-A MAML files (default: table_types); every *.maml in it is
                processed, one module each. Ignored when explicit MAML paths are given.
    --out-dir   where to write <dataset>.f90 (default: src)
    --check     regenerate in memory and compare; write nothing; exit 1 on any drift
    --self-test run this script's own tests and exit

THE GENERATED FILE IS USER-EDITABLE, in six clearly marked windows (see USER_WINDOWS below).
Everything outside them is regenerated from the MAML; the windows are lifted out of the existing
file first and put back verbatim, so a regeneration never destroys user code. `--check` is what
makes that safe to rely on: it fails the build if a generated region was hand-edited, or if the
committed file is stale relative to its MAML.

Two windows are also READ rather than merely preserved. The `components` window is parsed, and one
assignment per component is emitted into `clone_extra` (so %clone carries the user's own state
without them having to remember) and one reset into `init_extra` (so every constructor starts from
a known value). Anything the parser declines to handle is reported, by name, and left to the user.

Turns a Role-A MAML (one under `table_types/`) into a named
`parquet_table` extension type with one accessor per declared column -- see Generated table types
(doc/pages/utilities/generated-tables.md) for the user-facing story. This project's own
`table_types/maml_example4.maml` generates `src/parquet_table_example.f90` (module
`parquet_table_example`, type `parquet_table_test`), which ships with the library as a worked
example -- nothing else in the library uses it -- and is exercised by the `table_codegen` test
suite; regenerate it with `tools/generate_user_table_code.py` and commit the result. Unlike the
two generators below it is consumer-facing (like `tools/generate_parquet_maml.sh`) and ships in
the fpm-published package, so a downstream project can copy it. `--self-test` runs its own tests,
including a cross-check that its baked-in list of reserved `parquet_table` binding names still
matches `src/parquet_tables.f90` -- so adding a type-bound procedure to `parquet_table` fails the
lint stage until that list is updated.
"""

import argparse
import hashlib
import pathlib
import re
import sys

# --------------------------------------------------------------------------------------
# MAML data_type -> the Fortran facts the emitted code needs.
#
#   pk       PK_* discriminator for a scalar column (col_size == 1)
#   pk_vec   PK_* discriminator for a vector column (col_size > 1)
#   decl     the Fortran declaration of ONE value
#   family   'num'  ordinary array storage, so %col hands back a pointer
#            'str'  a parquet_string_column, which has its own accessor shapes
# --------------------------------------------------------------------------------------
TYPES = {
    "int32":     ("PK_INT32",     "PK_INT32_VEC",     "integer(int32)",           "num"),
    "int64":     ("PK_INT64",     "PK_INT64_VEC",     "integer(int64)",           "num"),
    "float32":   ("PK_FLOAT32",   "PK_FLOAT32_VEC",   "real(real32)",             "num"),
    "float64":   ("PK_FLOAT64",   "PK_FLOAT64_VEC",   "real(real64)",             "num"),
    "boolean":   ("PK_LOGICAL",   "PK_LOGICAL_VEC",   "logical",                  "num"),
    "string":    ("PK_STRING",    "PK_STRING_VEC",    "character",                "str"),
    "date":      ("PK_DATE",      "PK_DATE_VEC",      "type(parquet_date)",       "num"),
    "time":      ("PK_TIME",      "PK_TIME_VEC",      "type(parquet_time)",       "num"),
    "timestamp": ("PK_TIMESTAMP", "PK_TIMESTAMP_VEC", "type(parquet_timestamp)",  "num"),
}

# The three container data_types the LIBRARY accepts but a GENERATED table type declines, so the
# rejection can say which of the two drew the line. Deliberately NOT in TYPES: a field reaching
# that map would get five array-shaped accessors emitted for it. See feature_map_list_struct.md
# T12 -- this refusal is a decision, not a gap, and lifting it means a new emission path rather
# than another row above.
CONTAINER_TYPES = {"list", "map", "struct"}

# Every type-bound procedure name `parquet_table` already occupies, INCLUDING the private
# per-kind specifics -- a field called `col_ptr_i32` would collide just as surely as one called
# `get`. A field name matching any of these cannot become an accessor.
#
# Baked in rather than parsed out of src/parquet_tables.f90, because a downstream project that
# copied this script has no such file to read. `--self-test` cross-checks the list against that
# file WHEN IT EXISTS, which is always in parquet-fortran's own tree and never downstream -- so
# adding a binding to parquet_table fails the lint stage here until this list is updated.
RESERVED = {
    "add_column", "add_column_bool", "add_column_boolv", "add_column_chr", "add_column_chrv",
    "add_column_col", "add_column_date", "add_column_datev", "add_column_f32", "add_column_f32v",
    "add_column_f64", "add_column_f64v", "add_column_i32", "add_column_i32v", "add_column_i64",
    "add_column_i64v", "add_column_listcol", "add_column_mapcol", "add_column_strcol",
    "add_column_structcol", "add_column_time", "add_column_timev", "add_column_ts",
    "add_column_tsv", "append", "append_null_rows", "argsort_by", "argsort_by_values",
    "argsort_partial", "assign_guard", "bfill", "bind_predefined", "build_index", "cast", "clear_null",
    "clear_null_e32", "clear_null_e64", "clear_null_i32", "clear_null_i64", "clone", "clone_extra",
    "clone_structure", "col", "col_ptr_bool", "col_ptr_boolv", "col_ptr_date", "col_ptr_datev",
    "col_ptr_f32", "col_ptr_f32v", "col_ptr_f64", "col_ptr_f64v", "col_ptr_i32", "col_ptr_i32v",
    "col_ptr_i64", "col_ptr_i64v", "col_ptr_listcol", "col_ptr_mapcol", "col_ptr_strcol",
    "col_ptr_structcol", "col_ptr_time", "col_ptr_timev", "col_ptr_ts", "col_ptr_tsv", "column",
    "column_by_index", "column_by_name", "column_capacity", "column_index", "column_name",
    "column_names", "compact", "compact_validity", "copy_column", "delete_rows", "drop_column",
    "drop_columns", "drop_duplicates", "dropna", "duplicated", "ensure_validity", "evict_column",
    "explode", "ffill", "filename", "fillna", "filter_rows", "format_column", "generation", "get",
    "get_arr_bool", "get_arr_boolv", "get_arr_chr", "get_arr_chrv", "get_arr_date",
    "get_arr_datev", "get_arr_f32", "get_arr_f32v", "get_arr_f64", "get_arr_f64v", "get_arr_i32",
    "get_arr_i32v", "get_arr_i64", "get_arr_i64v", "get_arr_listcol", "get_arr_mapcol",
    "get_arr_str", "get_arr_structcol", "get_arr_time", "get_arr_timev", "get_arr_ts",
    "get_arr_tsv", "get_element", "get_element_bool_i32", "get_element_bool_i64",
    "get_element_boolv_i32", "get_element_boolv_i64", "get_element_chr_i32", "get_element_chr_i64",
    "get_element_chrv_i32", "get_element_chrv_i64", "get_element_date_i32", "get_element_date_i64",
    "get_element_datev_i32", "get_element_datev_i64", "get_element_f32_i32", "get_element_f32_i64",
    "get_element_f32v_i32", "get_element_f32v_i64", "get_element_f64_i32", "get_element_f64_i64",
    "get_element_f64v_i32", "get_element_f64v_i64", "get_element_i32_i32", "get_element_i32_i64",
    "get_element_i32v_i32", "get_element_i32v_i64", "get_element_i64_i32", "get_element_i64_i64",
    "get_element_i64v_i32", "get_element_i64v_i64", "get_element_time_i32", "get_element_time_i64",
    "get_element_timev_i32", "get_element_timev_i64", "get_element_ts_i32", "get_element_ts_i64",
    "get_element_tsv_i32", "get_element_tsv_i64", "get_file_metadata", "get_matrix",
    "get_matrix_bool", "get_matrix_f32", "get_matrix_f64", "get_matrix_i32", "get_matrix_i64",
    "get_matrix_string_bool", "get_matrix_string_f32", "get_matrix_string_f64",
    "get_matrix_string_i32", "get_matrix_string_i64", "get_slice", "get_slice_bool",
    "get_slice_boolv", "get_slice_chr", "get_slice_chrv", "get_slice_date", "get_slice_datev",
    "get_slice_f32", "get_slice_f32v", "get_slice_f64", "get_slice_f64v", "get_slice_i32",
    "get_slice_i32v", "get_slice_i64", "get_slice_i64v", "get_slice_str", "get_slice_time",
    "get_slice_timev", "get_slice_ts", "get_slice_tsv", "get_valid_mask", "group_by",
    "has_column",
    "has_nulls", "has_nulls_at", "has_nulls_name", "is_detached", "is_null", "is_null_at_e32",
    "is_null_at_e64", "is_null_at_i32", "is_null_at_i64", "is_null_e32", "is_null_e64",
    "is_null_i32", "is_null_i64", "is_sorted_by", "is_supported", "is_supported_at",
    "is_supported_name", "is_user_populated", "join", "keep_columns", "kind", "kind_at",
    "kind_name", "materialize", "materialize_all", "missing_columns", "missing_columns_array",
    "missing_columns_string", "ncols", "nrows", "nrows_unfiltered", "parse_column", "prefetch",
    "prefetch_array", "prefetch_string", "print_rows", "print_rows_all", "print_rows_array",
    "print_rows_string", "print_stat", "reload", "rename_column",
    "require_columns", "require_columns_array", "require_columns_string", "reserve",
    "reserve_columns", "residency", "residency_at", "residency_name", "row", "row_at_i32",
    "row_at_i64", "row_group_bounds", "row_group_extent", "row_mask", "set", "set_arr_bool",
    "set_arr_boolv", "set_arr_chr", "set_arr_chrv", "set_arr_date", "set_arr_datev", "set_arr_f32",
    "set_arr_f32v", "set_arr_f64", "set_arr_f64v", "set_arr_i32", "set_arr_i32v", "set_arr_i64",
    "set_arr_i64v", "set_arr_listcol", "set_arr_mapcol", "set_arr_strcol", "set_arr_structcol",
    "set_arr_time", "set_arr_timev", "set_arr_ts", "set_arr_tsv", "set_element",
    "set_element_bool_i32", "set_element_bool_i64", "set_element_boolv_i32",
    "set_element_boolv_i64", "set_element_chr_i32", "set_element_chr_i64", "set_element_chrv_i32",
    "set_element_chrv_i64", "set_element_date_i32", "set_element_date_i64",
    "set_element_datev_i32", "set_element_datev_i64", "set_element_f32_i32", "set_element_f32_i64",
    "set_element_f32v_i32", "set_element_f32v_i64", "set_element_f64_i32", "set_element_f64_i64",
    "set_element_f64v_i32", "set_element_f64v_i64", "set_element_i32_i32", "set_element_i32_i64",
    "set_element_i32v_i32", "set_element_i32v_i64", "set_element_i64_i32", "set_element_i64_i64",
    "set_element_i64v_i32", "set_element_i64v_i64", "set_element_time_i32", "set_element_time_i64",
    "set_element_timev_i32", "set_element_timev_i64", "set_element_ts_i32", "set_element_ts_i64",
    "set_element_tsv_i32", "set_element_tsv_i64", "set_matrix", "set_matrix_bool",
    "set_matrix_f32", "set_matrix_f64", "set_matrix_i32", "set_matrix_i64",
    "set_matrix_string_bool", "set_matrix_string_f32", "set_matrix_string_f64",
    "set_matrix_string_i32", "set_matrix_string_i64", "set_null", "set_null_e32", "set_null_e64",
    "set_null_i32", "set_null_i64", "set_null_mask", "set_null_mask_elem", "set_slice",
    "set_slice_bool", "set_slice_boolv", "set_slice_chr", "set_slice_chrv", "set_slice_date",
    "set_slice_datev", "set_slice_f32", "set_slice_f32v", "set_slice_f64", "set_slice_f64v",
    "set_slice_i32", "set_slice_i32v", "set_slice_i64", "set_slice_i64v", "set_slice_time",
    "set_slice_timev", "set_slice_ts", "set_slice_tsv", "set_user_populated", "sort_by",
    "sort_by_values", "table_append_null_rows_i32", "table_append_null_rows_i64",
    "table_append_row", "table_append_table", "table_argsort_by_i32", "table_argsort_by_i64",
    "table_argsort_by_string_i32", "table_argsort_by_string_i64",
    "table_argsort_by_values_chr_i32", "table_argsort_by_values_chr_i64",
    "table_argsort_by_values_f32_i32", "table_argsort_by_values_f32_i64",
    "table_argsort_by_values_f64_i32", "table_argsort_by_values_f64_i64",
    "table_argsort_by_values_i32_i32", "table_argsort_by_values_i32_i64",
    "table_argsort_by_values_i64_i32", "table_argsort_by_values_i64_i64",
    "table_argsort_partial_i32", "table_argsort_partial_i64", "table_argsort_partial_string_i32",
    "table_argsort_partial_string_i64", "table_bfill", "table_bfill_limit_i32",
    "table_bfill_limit_i64", "table_bfill_string", "table_bfill_string_limit_i32",
    "table_bfill_string_limit_i64", "table_delete_rows_i32", "table_delete_rows_i64",
    "table_drop_columns", "table_drop_columns_string", "table_drop_duplicates",
    "table_drop_duplicates_all", "table_drop_duplicates_string", "table_dropna",
    "table_dropna_all", "table_dropna_string", "table_duplicated", "table_duplicated_all",
    "table_duplicated_string", "table_explode_i32", "table_explode_i64", "table_ffill",
    "table_ffill_limit_i32", "table_ffill_limit_i64", "table_ffill_string",
    "table_ffill_string_limit_i32", "table_ffill_string_limit_i64", "table_fillna_bool",
    "table_fillna_chr", "table_fillna_date", "table_fillna_f32", "table_fillna_f64",
    "table_fillna_i32", "table_fillna_i64", "table_fillna_string_bool", "table_fillna_string_chr",
    "table_fillna_string_date", "table_fillna_string_f32", "table_fillna_string_f64",
    "table_fillna_string_i32", "table_fillna_string_i64", "table_fillna_string_time",
    "table_fillna_string_ts", "table_fillna_time", "table_fillna_ts", "table_filter_rows",
    "table_filter_rows_expr", "table_filter_rows_filter", "table_get_valid_mask",
    "table_get_valid_mask_elem", "table_group_by", "table_group_by_string", "table_is_sorted_by",
    "table_is_sorted_by_string", "table_join",
    "table_join_max_i32", "table_join_max_i64", "table_join_string", "table_join_string_max_i32",
    "table_join_string_max_i64", "table_keep_columns", "table_keep_columns_string",
    "table_reserve_i32", "table_reserve_i64", "table_row_mask_expr", "table_row_mask_filter",
    "table_sort_by", "table_sort_by_string", "table_sort_by_values_chr",
    "table_sort_by_values_f32", "table_sort_by_values_f64", "table_sort_by_values_i32",
    "table_sort_by_values_i64", "table_top_n", "table_top_n_string", "table_truncate_i32",
    "table_truncate_i64", "top_n", "truncate", "unit", "unit_at", "unit_name", "validate_qc",
    "value_counts",
    "width", "width_at", "width_name"
}

# The names THIS generator adds to the type on top of the inherited ones.
GENERATED_BINDINGS = {"init", "init_slice", "init_empty", "init_extra"}

# The six user-editable windows, in the order they appear in the emitted file. Their content is
# lifted out of an existing file and put back verbatim; the marker lines themselves are generated,
# so their guidance text can be improved without a user having to do anything.
USER_WINDOWS = ["uses", "components", "bindings", "init_extra", "clone_extra", "procedures"]

WINDOW_START = re.compile(r"^\s*!\s*>>>>>\s*USER SECTION \((\w+)\)")
WINDOW_END = re.compile(r"^\s*!\s*>>>>>\s*END USER SECTION \((\w+)\)")

MAX_LINE = 132
IDENT = re.compile(r"^[A-Za-z][A-Za-z0-9_]*$")


class MamlError(Exception):
    """A Role-A MAML this generator will not guess about."""


class Field:
    """One `fields:` entry, reduced to what the emitted code needs."""

    def __init__(self, name, data_type, col_size, array_size, unit, info, from_file):
        self.name = name
        self.data_type = data_type      # base token, e.g. "timestamp" for "timestamp[us,utc]"
        self.col_size = col_size
        self.array_size = array_size
        self.unit = unit
        self.info = info
        self.from_file = from_file

    @property
    def is_vector(self):
        return self.col_size > 1

    @property
    def family(self):
        return TYPES[self.data_type][3]

    @property
    def pk(self):
        t = TYPES[self.data_type]
        return t[1] if self.is_vector else t[0]

    @property
    def decl(self):
        return TYPES[self.data_type][2]


class Schema:
    """A parsed Role-A MAML: the module and type it names, plus its fields."""

    def __init__(self, dataset, table, author, fields, source_path, digest):
        self.dataset = dataset
        self.table = table
        self.author = author
        self.fields = fields
        self.source_path = source_path
        self.digest = digest

    @property
    def type_name(self):
        return "parquet_table_" + self.table


# --------------------------------------------------------------------------------------
# MAML parsing.
#
# Deliberately a small, strict reader for the same line-oriented subset src/parquet_metadata_maml.f90
# accepts, NOT a YAML parser: this script must stay stdlib-only, and guessing at input it does not
# understand is how a generator produces plausible-looking wrong code. Only `dataset`, `table`,
# `author` and `fields` are read; every other section is ignored, so a Role-A MAML stays an
# ordinary MAML that parquet_parse_maml can also use as a write schema.
# --------------------------------------------------------------------------------------
def _strip_comment(text):
    """Removes a trailing `# ...` comment that is not inside quotes."""
    out, quote = [], None
    for ch in text:
        if quote:
            out.append(ch)
            if ch == quote:
                quote = None
        elif ch in "\"'":
            quote = ch
            out.append(ch)
        elif ch == "#":
            break
        else:
            out.append(ch)
    return "".join(out).rstrip()


def _unquote(v):
    v = v.strip()
    if len(v) >= 2 and v[0] == v[-1] and v[0] in "\"'":
        return v[1:-1]
    return v


def _positive_int(raw, what, where):
    v = _unquote(raw)
    if v.lower() == "auto":
        raise MamlError(f"{where}: `{what}: auto` cannot be used in a table-type schema -- a "
                        f"generated accessor's kind and rank must be known when the code is "
                        f"generated, and `auto` is resolved at write time")
    try:
        n = int(v)
    except ValueError:
        raise MamlError(f"{where}: `{what}: {v}` is not an integer")
    if n < 1:
        raise MamlError(f"{where}: `{what}: {v}` must be a positive integer")
    return n


def parse_maml(text, path):
    """Parses a Role-A MAML into a Schema, or raises MamlError."""
    where = str(path)
    top = {}
    fields = []
    cur = None
    in_fields = False

    for lineno, raw in enumerate(text.splitlines(), start=1):
        if "\t" in raw.split("#")[0]:
            raise MamlError(f"{where}:{lineno}: tab used for indentation; MAML is space-indented")
        line = _strip_comment(raw)
        if not line.strip():
            continue
        indent = len(line) - len(line.lstrip(" "))
        body = line.strip()

        if indent == 0 and not body.startswith("-"):
            key, _, value = body.partition(":")
            key = key.strip().lower()
            if not _:
                raise MamlError(f"{where}:{lineno}: expected `key: value`, got {body!r}")
            in_fields = (key == "fields")
            if in_fields:
                cur = None
                continue
            if key in ("dataset", "table", "author"):
                if key in top:
                    raise MamlError(f"{where}:{lineno}: `{key}:` given twice")
                top[key] = _unquote(value)
            continue

        if not in_fields:
            continue  # inside some other section; not this generator's business

        if body.startswith("-"):
            cur = {"_line": lineno}
            fields.append(cur)
            body = body[1:].strip()
            if not body:
                continue
        if cur is None:
            continue
        key, _, value = body.partition(":")
        if not _:
            raise MamlError(f"{where}:{lineno}: expected `key: value` in a fields: entry, "
                            f"got {body!r}")
        key = key.strip().lower()
        if key in cur:
            raise MamlError(f"{where}:{lineno}: `{key}:` given twice in one fields: entry")
        cur[key] = value.strip()

    for req in ("dataset", "table"):
        if not top.get(req):
            raise MamlError(f"{where}: `{req}:` is required -- it names the "
                            f"{'module' if req == 'dataset' else 'type'} to generate")

    out_fields = []
    seen = {}
    for f in fields:
        ln = f["_line"]
        name = _unquote(f.get("name", ""))
        if not name:
            raise MamlError(f"{where}:{ln}: this fields: entry has no `name:`")
        dtype_raw = _unquote(f.get("data_type", ""))
        if not dtype_raw:
            raise MamlError(f"{where}:{ln}: field '{name}' has no `data_type:`")
        base = dtype_raw.split("[")[0].strip().lower()
        if base in CONTAINER_TYPES:
            raise MamlError(f"{where}:{ln}: field '{name}' declares a container type "
                            f"(`data_type: {dtype_raw}`). That is a valid MAML data_type, but a "
                            f"GENERATED table type cannot give it a named accessor: an accessor "
                            f"for a container column hands back a handle or a standalone column "
                            f"object rather than an array, which is a different shape from the "
                            f"five specifics this script emits per field. The column itself is "
                            f"still reachable -- drop the field from this schema and use the "
                            f"inherited %col/%get/%set under its file name")
        if base not in TYPES:
            raise MamlError(f"{where}:{ln}: field '{name}' has an unsupported "
                            f"`data_type: {dtype_raw}`; expected one of "
                            f"{', '.join(sorted(TYPES))} (with an optional [unit] suffix on "
                            f"time/timestamp)")
        col_size = _positive_int(f["col_size"], "col_size", f"{where}:{ln}") if "col_size" in f else 1
        array_size = (_positive_int(f["array_size"], "array_size", f"{where}:{ln}")
                      if "array_size" in f else 0)
        src = _unquote(f.get("source", "file")).lower()
        if src not in ("file", "computed"):
            raise MamlError(f"{where}:{ln}: field '{name}' has `source: {src}`; expected "
                            f"`file` (the default) or `computed`")
        low = name.lower()
        if not IDENT.match(name):
            raise MamlError(f"{where}:{ln}: column name '{name}' is not a valid Fortran "
                            f"identifier, so no accessor can be named after it; rename the field "
                            f"or give it a table-facing name with `extra: remap:`")
        if low in seen:
            raise MamlError(f"{where}:{ln}: column '{name}' and '{seen[low]}' differ only in "
                            f"case, and Fortran identifiers do not -- they cannot both have an "
                            f"accessor")
        seen[low] = name
        if low in RESERVED or low in GENERATED_BINDINGS:
            raise MamlError(f"{where}:{ln}: column '{name}' collides with a procedure "
                            f"`parquet_table` already has, so its accessor cannot be generated; "
                            f"rename the field, or give it a table-facing name with "
                            f"`extra: remap:`")
        if base == "string" and len(name) + 4 > 63:
            raise MamlError(f"{where}:{ln}: column '{name}' is too long -- its `{name}_chr` "
                            f"accessor would exceed Fortran's 63-character identifier limit")
        if len(name) > 63:
            raise MamlError(f"{where}:{ln}: column '{name}' exceeds Fortran's 63-character "
                            f"identifier limit")
        out_fields.append(Field(name, base, col_size, array_size,
                                _unquote(f.get("unit", "")), _unquote(f.get("info", "")), src == "file"))

    for what, value in (("dataset", top["dataset"]), ("table", top["table"])):
        if not IDENT.match(value):
            raise MamlError(f"{where}: `{what}: {value}` is not a valid Fortran identifier")
    if len("parquet_table_" + top["table"]) > 63:
        raise MamlError(f"{where}: `table: {top['table']}` makes a type name longer than "
                        f"Fortran's 63-character identifier limit")

    schema = Schema(top["dataset"], top["table"], top.get("author", ""), out_fields, path,
                    hashlib.sha256(text.encode("utf-8")).hexdigest())
    _check_module_name_free(schema, where)
    return schema


def _check_module_name_free(schema, where):
    """Refuses a `dataset:` that collides with something the generated module declares.

    A module's own name is a local entity inside it, so **a module cannot declare a type or a
    procedure of the same name** -- gfortran rejects it outright ("PUBLIC attribute applied to
    MODULE ..."). The case that actually bites is `dataset:` naming the same thing `table:`
    derives, i.e. `dataset: parquet_table_x` with `table: x`, which is the natural thing to write
    and produces a module that cannot compile. This project hit exactly that collision once
    before, between the `parquet_strings` module and its `parquet_string` type, which is why that
    module is plural.

    Note the rule is NOT "dataset and table must differ": `dataset: foo` with `table: foo` is
    perfectly fine, since the type is `parquet_table_foo`. The comparison has to be against the
    DERIVED names, and it is case-insensitive because Fortran identifiers are.
    """
    module = schema.dataset.lower()
    if module == schema.type_name.lower():
        raise MamlError(
            f"{where}: `dataset: {schema.dataset}` names the module, and `table: {schema.table}` "
            f"makes the type `{schema.type_name}` -- a module cannot declare a type of its own "
            f"name, so this would not compile. Give `dataset:` a different name (this project's "
            f"own precedent is a plural: module `parquet_strings` holds type `parquet_string`)")
    for name in _module_level_names(schema):
        if module == name.lower():
            raise MamlError(
                f"{where}: `dataset: {schema.dataset}` names the module, which would also declare "
                f"a procedure called `{name}` -- a module cannot declare a procedure of its own "
                f"name. Rename `dataset:`, or rename the field it comes from")


def _module_level_names(schema):
    """Every module-level procedure name the generator emits, for the collision check above."""
    t = schema.type_name
    names = [f"{t}_init", f"{t}_init_slice_i32", f"{t}_init_slice_i64", f"{t}_init_empty_none",
             f"{t}_init_empty_i32", f"{t}_init_empty_i64", f"{t}_init_extra", f"{t}_clone_extra",
             f"{t}_check_row", f"{t}_check_range"]
    for f in schema.fields:
        if f.family == "str":
            names.append(f"{t}_{f.name}_chr")
            if f.is_vector:
                continue
        for suffix in ("all", "at_i32", "at_i64", "rng_i32", "rng_i64"):
            names.append(f"{t}_{f.name}_{suffix}")
    return names


# --------------------------------------------------------------------------------------
# The user windows: lifting them out of an existing file, and reading the components one.
# --------------------------------------------------------------------------------------
def extract_windows(text, path):
    """Returns {id: [lines]} for every user window in `text`, or raises MamlError.

    Checked BEFORE anything else a regeneration does, because a malformed marker set is the one
    state in which rewriting the file would silently destroy user code.
    """
    found, open_id, buf = {}, None, []
    for lineno, line in enumerate(text.splitlines(), start=1):
        ms, me = WINDOW_START.match(line), WINDOW_END.match(line)
        if ms:
            if open_id is not None:
                raise MamlError(f"{path}:{lineno}: USER SECTION ({ms.group(1)}) starts while "
                                f"({open_id}) is still open")
            open_id = ms.group(1)
            if open_id not in USER_WINDOWS:
                raise MamlError(f"{path}:{lineno}: unknown USER SECTION ({open_id}); expected one "
                                f"of {', '.join(USER_WINDOWS)}")
            if open_id in found:
                raise MamlError(f"{path}:{lineno}: USER SECTION ({open_id}) appears twice")
            buf = []
        elif me:
            if open_id is None:
                raise MamlError(f"{path}:{lineno}: END USER SECTION ({me.group(1)}) with no "
                                f"matching start")
            if me.group(1) != open_id:
                raise MamlError(f"{path}:{lineno}: END USER SECTION ({me.group(1)}) closes "
                                f"({open_id})")
            found[open_id] = buf
            open_id, buf = None, []
        elif open_id is not None:
            buf.append(line)
    if open_id is not None:
        raise MamlError(f"{path}: USER SECTION ({open_id}) is never closed")
    missing = [w for w in USER_WINDOWS if w not in found]
    if missing and found:
        raise MamlError(f"{path}: these USER SECTIONs are missing: {', '.join(missing)}; restore "
                        f"them (or delete the file to regenerate it from scratch) rather than "
                        f"letting a regeneration decide where your code belongs")
    return found


def _split_top_level(text, sep=","):
    """Splits on `sep` at paren depth 0."""
    parts, depth, cur = [], 0, ""
    for ch in text:
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
        if ch == sep and depth == 0:
            parts.append(cur)
            cur = ""
        else:
            cur += ch
    parts.append(cur)
    return [p.strip() for p in parts if p.strip()]


def parse_components(lines):
    """Reads the `components` window into (copies, resets, skipped).

    * `copies`  -- names to assign in `clone_extra`.
    * `resets`  -- (name, statement) to run in `init_extra`.
    * `skipped` -- (name, why) the parser declined to handle, reported to the user.

    Deliberately conservative: anything this does not recognise with confidence is skipped and
    named, never guessed at. Emitting a wrong assignment into generated code would be worse than
    emitting none, since the user cannot edit it.
    """
    copies, resets, skipped = [], [], []
    joined, acc = [], ""
    for raw in lines:
        s = _strip_fortran_comment(raw).strip()
        if not s:
            continue
        if s.endswith("&"):
            acc += s[:-1] + " "
            continue
        joined.append((acc + s).strip())
        acc = ""
    if acc.strip():
        joined.append(acc.strip())

    for decl in joined:
        if "::" not in decl:
            skipped.append((decl[:40], "not a `<type> :: <name>` declaration"))
            continue
        spec, _, rest = decl.partition("::")
        spec_low = spec.lower()
        is_pointer = re.search(r"\bpointer\b", spec_low) is not None
        is_alloc = re.search(r"\ballocatable\b", spec_low) is not None
        for entry in _split_top_level(rest):
            m = re.match(r"^([A-Za-z][A-Za-z0-9_]*)\s*(\([^)]*\))?\s*(?:=>?\s*(.+))?$", entry)
            if not m:
                skipped.append((entry[:40], "could not read the component name"))
                continue
            name, _dims, init = m.group(1), m.group(2), m.group(3)
            if is_pointer:
                skipped.append((name, "a pointer component -- only you can say whether a clone "
                                      "should alias or copy its target"))
                continue
            copies.append(name)
            if init:
                resets.append((name, f"self%{name} = {init.strip()}"))
            elif is_alloc:
                resets.append((name, f"if (allocated(self%{name})) deallocate(self%{name})"))
            else:
                zero = _zero_for(spec_low)
                if zero is None:
                    skipped.append((name, "a derived-type component with no default initializer; "
                                          "give it one, or reset it in the init_extra window"))
                else:
                    resets.append((name, f"self%{name} = {zero}"))
    return copies, resets, skipped


def _strip_fortran_comment(line):
    out, quote = [], None
    for ch in line:
        if quote:
            out.append(ch)
            if ch == quote:
                quote = None
        elif ch in "\"'":
            quote = ch
            out.append(ch)
        elif ch == "!":
            break
        else:
            out.append(ch)
    return "".join(out)


def _zero_for(spec_low):
    """The zero of an intrinsic type, or None when 'zero' is not defined for it.

    A derived-type component with no initializer returns None on purpose: the literal reset would
    be `self%x = <typename>()`, the default structure constructor CLAUDE.md records ifx as
    rejecting when the type embeds another module's private-component type -- which both
    parquet_string_column and parquet_column do.
    """
    if re.match(r"^\s*integer\b", spec_low):
        return "0"
    if re.match(r"^\s*(real|double\s+precision)\b", spec_low):
        return "0.0"
    if re.match(r"^\s*logical\b", spec_low):
        return ".false."
    if re.match(r"^\s*character\b", spec_low):
        return '""'
    if re.match(r"^\s*complex\b", spec_low):
        return "(0.0, 0.0)"
    return None


# --------------------------------------------------------------------------------------
# Emission.
# --------------------------------------------------------------------------------------
def wrap_doc(indent, marker, text, width=MAX_LINE):
    """Wraps a doc-comment to the project's 132-column limit, on word boundaries.

    Needed because column `info:`/`unit:` text is arbitrary-length user data interpolated into a
    comment -- exactly what blows the limit. Mirrors the MAML generators' own emit_doc.
    """
    # `!>` opens the block and `!!` continues it, which is this project's convention (see
    # `.claude/rules/documentation.md`'s "FORD doc-comment conventions"): a leading `!>` is a predoc, and every
    # following line of the same block carries `!!`.
    first = f"{indent}{marker} "
    rest = f"{indent}!! " if marker == "!>" else f"{indent}{marker} "
    out, cur = [], ""
    for word in text.split():
        prefix = first if not out else rest
        cand = (cur + " " + word) if cur else word
        if len(prefix) + len(cand) > width and cur:
            out.append(prefix + cur)
            cur = word
        else:
            cur = cand
    if cur:
        out.append((first if not out else rest) + cur)
    return out


def field_summary(f):
    """The one-line description a field's binding and procedure doc-comments share."""
    bits = []
    if f.info:
        bits.append(f.info.rstrip(".") + ".")
    else:
        bits.append(f"Column '{f.name}'.")
    if f.unit:
        bits.append(f"Unit: {f.unit}.")
    shape = f"{f.data_type}"
    if f.is_vector:
        shape += f", width {f.col_size}"
    bits.append(f"({shape})")
    if not f.from_file:
        bits.append("Computed: no file column is read for it.")
    return " ".join(bits)


def window(indent, wid, note):
    """Emits an EMPTY user window, with its guidance on the marker line."""
    return [f"{indent}! >>>>> USER SECTION ({wid}) -- {note}",
            f"{indent}! >>>>> END USER SECTION ({wid})"]


def fill_window(indent, wid, note, content):
    return ([f"{indent}! >>>>> USER SECTION ({wid}) -- {note}"] + list(content) +
            [f"{indent}! >>>>> END USER SECTION ({wid})"])


def render(schema, windows):
    """Builds the whole .f90 text for one schema, re-inserting `windows` verbatim."""
    o = []
    w = o.append
    t = schema.type_name
    copies, resets, skipped = parse_components(windows.get("components", []))

    author = schema.author.strip()
    w("!===========================================")
    if author:
        w(f"! Author: {author}")
    else:
        w("! Generated by parquet-fortran; the source MAML declares no author:.")
    w("!===========================================")
    w("!")
    w("! GENERATED FILE -- everything outside the marked USER SECTIONs is regenerated.")
    w("! Regenerate with:  tools/generate_user_table_code.py")
    w(f"! source-maml: {schema.source_path.as_posix()}")
    w(f"! source-maml-sha256: {schema.digest}")
    w("!")
    o.extend(wrap_doc("", "!>", f"Table type generated from the `{schema.table}` MAML schema: "
                                f"`{t}`, with one accessor per predefined column."))
    w("!!")
    o.extend(wrap_doc("", "!!", "It extends `parquet_table`, so every inherited operation "
                                "(%nrows, %get, %filter_rows, %clone, %append, %row, ...) "
                                "applies unchanged. Open it with %init, %init_slice or "
                                "%init_empty -- a plain parquet_open_table will not compile for "
                                "this type, deliberately, since it would skip the predefined "
                                "columns."))
    w(f"module {schema.dataset}")
    w("    use parquet")
    # The four kinds every constructor signature needs (row bounds, sample_fraction,
    # sample_seed), plus whatever the accessors declare. `use parquet` does not re-export them.
    w("    use iso_fortran_env, only : int32, int64, real32, real64")
    w("    implicit none")
    w("    private")
    w(f"    public :: {t}")
    o.extend(fill_window("    ", "uses",
                         "your own use/parameter declarations; preserved on regeneration",
                         windows.get("uses", [])))
    w("")
    o.extend(wrap_doc("    ", "!>", f"Predefined-column table generated from the "
                                    f"`{schema.table}` schema."))
    w(f"    type, extends(parquet_table) :: {t}")
    w("        ! Components you declare below are copied by %clone and reset by %init "
      "automatically:")
    w("        ! the generator writes those statements into clone_extra/init_extra for you.")
    o.extend(fill_window("        ", "components",
                         "your own table parameters; preserved on regeneration",
                         windows.get("components", [])))
    w("    contains")
    w(f"        procedure :: init => {t}_init !! Opens a file and binds every predefined column.")
    w(f"        procedure, private :: {t}_init_slice_i32 !! %init_slice specific, int32 bounds.")
    w(f"        procedure, private :: {t}_init_slice_i64 !! %init_slice specific, int64 bounds.")
    o.extend(wrap_doc("        ", "!>", "Opens one contiguous row range of a file, binding the "
                                        "same columns %init does. There is no `sort` argument: a "
                                        "sort reorders rows across the whole file, so a slice "
                                        "could not be cut along it."))
    o.extend(_continued("        ",
                        f"generic :: init_slice => {t}_init_slice_i32, {t}_init_slice_i64"))
    w(f"        procedure, private :: {t}_init_empty_none !! %init_empty specific, no row count.")
    w(f"        procedure, private :: {t}_init_empty_i32 !! %init_empty specific, int32 row count.")
    w(f"        procedure, private :: {t}_init_empty_i64 !! %init_empty specific, int64 row count.")
    o.extend(wrap_doc("        ", "!>", "Builds this table in memory, with no file behind it: "
                                        "every predefined column is created empty, or with "
                                        "`nrows` all-null rows. The output-catalogue shape."))
    o.extend(_continued("        ", f"generic :: init_empty => {t}_init_empty_none, "
                                    f"{t}_init_empty_i32, {t}_init_empty_i64"))
    w(f"        procedure :: init_extra => {t}_init_extra !! Resets/initializes YOUR components.")
    o.extend(wrap_doc("        ", "!>", "Copies this type's own components on "
                                        "%clone/%clone_structure. Mostly written for you."))
    w(f"        procedure :: clone_extra => {t}_clone_extra")
    for f in schema.fields:
        o.extend(emit_bindings(schema, f))
    o.extend(fill_window("        ", "bindings",
                         "your own type-bound procedures; preserved on regeneration",
                         windows.get("bindings", [])))
    w(f"    end type {t}")
    w("")
    w("contains")
    w("")
    o.extend(emit_init(schema))
    o.extend(emit_init_slice(schema))
    o.extend(emit_init_empty(schema))
    o.extend(emit_init_extra(schema, resets, windows))
    o.extend(emit_clone_extra(schema, copies, windows))
    for f in schema.fields:
        o.extend(emit_accessors(schema, f))
    if any(f.family == "num" for f in schema.fields):
        o.extend(emit_index_guards(schema))
    o.extend(fill_window("    ", "procedures",
                         "your own procedure bodies; preserved on regeneration",
                         windows.get("procedures", [])))
    w("")
    # Tagged for the same reason the same line is tagged in tools/generate_parquet_tables.py: an
    # `end module` line has no executable statement, but gcov gives it a positive hit count, so an
    # untagged exclusion is reported forever as a candidate stale one. See `.claude/rules/coverage.md`, "Fortran
    # gcov attribution artifacts".
    w(f"end module {schema.dataset} ! GCOVR_EXCL_LINE -- gcov attribution artifact")
    w("")
    return "\n".join(o), skipped


def emit_bindings(schema, f):
    """The type-bound bindings for one field."""
    t = schema.type_name
    o = []
    summary = field_summary(f)
    if f.family == "str":
        if not f.is_vector:
            o.extend(wrap_doc("        ", "!>", f"{summary} `%{f.name}()` is the packed string "
                                                f"store, `%{f.name}(i)` a handle to row i, "
                                                f"`%{f.name}(lo,hi)` one handle per row of the "
                                                f"range."))
            o.append(f"        procedure, private :: {t}_{f.name}_all")
            o.append(f"        procedure, private :: {t}_{f.name}_at_i32")
            o.append(f"        procedure, private :: {t}_{f.name}_at_i64")
            o.append(f"        procedure, private :: {t}_{f.name}_rng_i32")
            o.append(f"        procedure, private :: {t}_{f.name}_rng_i64")
            o.extend(_continued("        ",
                                f"generic :: {f.name} => {t}_{f.name}_all, {t}_{f.name}_at_i32, "
                                f"{t}_{f.name}_at_i64, {t}_{f.name}_rng_i32, "
                                f"{t}_{f.name}_rng_i64"))
        o.extend(wrap_doc("        ", "!>", f"{summary} Copied out as a character array sized to "
                                            f"the longest value present."))
        o.append(f"        procedure :: {f.name}_chr => {t}_{f.name}_chr")
        return o
    o.extend(wrap_doc("        ", "!>", f"{summary} `%{f.name}()` is the whole column, "
                                        f"`%{f.name}(i)` row i, `%{f.name}(lo,hi)` a row range -- "
                                        f"all zero copy. Take `%{f.name}()` once outside a loop; "
                                        f"the indexed forms repeat the column lookup."))
    o.append(f"        procedure, private :: {t}_{f.name}_all")
    o.append(f"        procedure, private :: {t}_{f.name}_at_i32")
    o.append(f"        procedure, private :: {t}_{f.name}_at_i64")
    o.append(f"        procedure, private :: {t}_{f.name}_rng_i32")
    o.append(f"        procedure, private :: {t}_{f.name}_rng_i64")
    o.extend(_continued("        ",
                        f"generic :: {f.name} => {t}_{f.name}_all, {t}_{f.name}_at_i32, "
                        f"{t}_{f.name}_at_i64, {t}_{f.name}_rng_i32, {t}_{f.name}_rng_i64"))
    return o


def bind_call(schema, indent, from_file_all_false=False):
    """The single `%bind_predefined` call, as a data table."""
    fs = schema.fields
    if not fs:
        return []
    nlen = max(len(f.name) for f in fs)
    names = ", ".join(f'"{f.name}"' for f in fs)
    kinds = ", ".join(f.pk for f in fs)
    widths = ", ".join(str(f.col_size) for f in fs)
    if from_file_all_false:
        flags = ", ".join(".false." for _ in fs)
    else:
        flags = ", ".join(".true." if f.from_file else ".false." for f in fs)
    o = [f"{indent}call self%bind_predefined( &"]
    for chunk, tail in ((f"[character(len={nlen}) :: {names}]", " &"),
                        (f"[{kinds}]", " &"),
                        (f"[{widths}]", " &"),
                        (f"[{flags}]", " &")):
        o.extend(_continued(indent + "    ", chunk + "," + tail))
    tail = f'context="{schema.source_path.name}"'
    if not from_file_all_false:
        tail += ", exact=exact"
    # The declared units, only when the schema declares any: they fill in a unit a column does
    # not already have (a computed column has no other source of one at all), and never overwrite
    # one a read-in MAML supplied.
    if any(f.unit for f in schema.fields):
        ulen = max(max((len(f.unit) for f in schema.fields), default=1), 1)
        units = ", ".join(f'"{f.unit}"' for f in schema.fields)
        o.extend(_continued(indent + "    ", tail + ", &"))
        o.extend(_continued(indent + "    ", f"units=[character(len={ulen}) :: {units}])"))
    else:
        o.extend(_continued(indent + "    ", tail + ")"))
    return o


def _continued(indent, text):
    """Emits `text` as one or more continued Fortran lines within the 132-column limit."""
    if len(indent) + len(text) <= MAX_LINE:
        return [indent + text]
    out, cur = [], ""
    for tok in text.split(" "):
        cand = (cur + " " + tok) if cur else tok
        if len(indent) + len(cand) + 2 > MAX_LINE and cur:
            out.append(indent + cur + " &")
            cur = "    " + tok
        else:
            cur = cand
    out.append(indent + cur)
    return out


# Every optional argument %init forwards to parquet_open_table, in the order that call takes
# them. `exact` is deliberately absent: it belongs to the kind conversion below, not to the open.
# check_open_table_arguments_are_forwarded (tools/check_source_conventions.py) compares the
# forwarding calls emitted below against parquet_open_table's OWN interface bodies, so an argument
# added to the library fails the lint stage here rather than being quietly dropped -- which is how
# `list_columns` came to be missing from a promise this file's own doc-comment makes.
INIT_ARGS = ("maml", "filter", "sort", "qc", "qc_soft", "use_threads", "sample_fraction",
             "sample_seed", "list_columns", "bounded")

INIT_DECLS = [
    '        character(len=*), intent(in), optional :: maml !! read-in (Role-B) MAML describing the file.',
    '        type(parquet_filter), intent(in), optional :: filter !! row filter, in INTERNAL column names.',
    '        type(parquet_sortkey), intent(in), optional :: sort !! sort keys, in INTERNAL column names.',
    '        type(parquet_read_qc), intent(in), optional :: qc !! read-time qc, in INTERNAL column names.',
    '        logical, intent(in), optional :: qc_soft !! warn on a qc violation instead of aborting.',
    '        logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.',
    '        real(real64), intent(in), optional :: sample_fraction !! keep each row with this probability.',
    '        integer(int64), intent(in), optional :: sample_seed !! seed for that draw; `42_int64`.',
    '        character(len=*), intent(in), optional :: list_columns !! `"auto"` (default) or `"container"`.',
    '        logical, intent(in), optional :: bounded !! read one row group at a time; see parquet_open_table.',
    '        logical, intent(in), optional :: exact !! refuse a kind conversion that would lose a value;',
    '        !! forwarded verbatim to %cast, whose own default applies when this is absent.',
]


def _init_decls_without(drop):
    """INIT_DECLS with the named dummy argument's declaration removed.

    Selects by the DECLARED NAME, not by a substring of the line. The previous form -- `"sort" not
    in d`, a substring test over the whole line including its `!!` doc text -- worked only because
    exactly one entry happens to contain those letters: a future declaration whose comment merely
    mentioned a sort would have been dropped silently, giving %init_slice a constructor missing an
    argument, with nothing anywhere to report it.

    An entry with no `::` is a continuation of the doc-comment on the entry above it, so it travels
    with that declaration and is dropped with it.
    """
    out, keep = [], True
    for decl in INIT_DECLS:
        if "::" in decl:
            keep = decl.split("::", 1)[1].split("!!", 1)[0].strip() != drop
        if keep:
            out.append(decl)
    return out


def emit_init(schema):
    t = schema.type_name
    o = []
    o.extend(wrap_doc("    ", "!>", "Opens the file and binds every predefined column this "
                                    "schema declares: each is checked for presence, kind and "
                                    "width, converted to its declared kind where a conversion "
                                    "rule applies, and fully materialized before this returns. "
                                    "Every optional argument is forwarded to parquet_open_table "
                                    "unchanged, except `exact`, which belongs to the kind "
                                    "conversion."))
    o.append(f"    subroutine {t}_init(self, filename, maml, filter, sort, qc, qc_soft, "
             f"use_threads, &")
    o.append("            sample_fraction, sample_seed, list_columns, bounded, exact)")
    o.append(f"        class({t}), intent(inout) :: self !! the table to fill.")
    o.append("        character(len=*), intent(in) :: filename !! parquet file to open.")
    o.extend(INIT_DECLS)
    o.append("        !")
    o.append("        call parquet_open_table(self%parquet_table, filename, maml, filter, sort, "
             "qc, qc_soft, &")
    o.append("            use_threads, sample_fraction, sample_seed, list_columns, bounded)")
    o.extend(bind_call(schema, "        "))
    o.append("        call self%init_extra()")
    o.append(f"    end subroutine {t}_init")
    o.append("    !")
    return o


def emit_init_slice(schema):
    t = schema.type_name
    o = []
    for kind, suffix in (("integer(int32)", "i32"), ("integer(int64)", "i64")):
        o.extend(wrap_doc("    ", "!>", f"%init_slice specific taking {suffix} row bounds; see "
                                        f"the generic binding."))
        o.append(f"    subroutine {t}_init_slice_{suffix}(self, filename, row_lo, row_hi, maml, "
                 f"filter, qc, &")
        o.append("            qc_soft, use_threads, sample_fraction, sample_seed, list_columns, &")
        o.append("            bounded, exact)")
        o.append(f"        class({t}), intent(inout) :: self !! the table to fill.")
        o.append("        character(len=*), intent(in) :: filename !! parquet file to open.")
        o.append(f"        {kind}, intent(in) :: row_lo !! first file row to cover (1-based).")
        o.append(f"        {kind}, intent(in) :: row_hi !! last file row to cover (inclusive).")
        o.extend(_init_decls_without("sort"))
        o.append("        !")
        o.append("        call parquet_open_table(self%parquet_table, filename, row_lo, row_hi, "
                 "maml, filter, qc, &")
        o.append("            qc_soft, use_threads, sample_fraction, sample_seed, list_columns, &")
        o.append("            bounded)")
        o.extend(bind_call(schema, "        "))
        o.append("        call self%init_extra()")
        o.append(f"    end subroutine {t}_init_slice_{suffix}")
        o.append("    !")
    return o


def emit_init_empty(schema):
    t = schema.type_name
    o = []
    o.extend(wrap_doc("    ", "!>", "%init_empty specific taking no row count: every predefined "
                                    "column is created with zero rows, to be grown with %append."))
    o.append(f"    subroutine {t}_init_empty_none(self)")
    o.append(f"        class({t}), intent(inout) :: self !! the table to build.")
    o.append("        !")
    o.append("        call parquet_new_table(self%parquet_table)")
    o.extend(bind_call(schema, "        ", from_file_all_false=True))
    o.append("        call self%init_extra()")
    o.append(f"    end subroutine {t}_init_empty_none")
    o.append("    !")
    for kind, suffix in (("integer(int32)", "i32"), ("integer(int64)", "i64")):
        o.extend(wrap_doc("    ", "!>", f"%init_empty specific taking an {suffix} row count: "
                                        f"every predefined column is created with `nrows` "
                                        f"all-null rows, ready to be filled in place."))
        o.append(f"    subroutine {t}_init_empty_{suffix}(self, nrows)")
        o.append(f"        class({t}), intent(inout) :: self !! the table to build.")
        o.append(f"        {kind}, intent(in) :: nrows !! rows to create, all null.")
        o.append("        !")
        o.append("        call parquet_new_table(self%parquet_table)")
        # bind FIRST, so the columns exist; append_null_rows then grows every one of them at
        # once. The other order would try to add rows to a table with no columns.
        o.extend(bind_call(schema, "        ", from_file_all_false=True))
        o.append("        if (nrows > 0) call self%append_null_rows(nrows)")
        o.append("        call self%init_extra()")
        o.append(f"    end subroutine {t}_init_empty_{suffix}")
        o.append("    !")
    return o


def emit_init_extra(schema, resets, windows):
    t = schema.type_name
    o = []
    o.extend(wrap_doc("    ", "!>", "Resets and initializes this type's own components. Called by "
                                    "every constructor, so whatever you write here happens "
                                    "exactly once however the table was built -- and so your own "
                                    "parameters must be set AFTER %init returns, not before."))
    o.append(f"    subroutine {t}_init_extra(self)")
    o.append(f"        class({t}), intent(inout) :: self !! the table just constructed.")
    o.append("        !")
    for _name, stmt in resets:
        o.append(f"        {stmt}")
    o.extend(fill_window("        ", "init_extra",
                         "your own initialization; preserved on regeneration",
                         windows.get("init_extra", [])))
    o.append(f"    end subroutine {t}_init_extra")
    o.append("    !")
    return o


def emit_clone_extra(schema, copies, windows):
    t = schema.type_name
    o = []
    o.extend(wrap_doc("    ", "!>", "Copies this type's own components when the table is cloned. "
                                    "`out` is guaranteed to have the same dynamic type as `self`. "
                                    "`structure_only` is .true. when called from "
                                    "%clone_structure, i.e. when `out` has the same columns but "
                                    "no rows."))
    o.append(f"    subroutine {t}_clone_extra(self, out, structure_only)")
    o.append(f"        class({t}), intent(in) :: self !! the table being copied.")
    o.append("        class(parquet_table), intent(inout) :: out !! the copy, already holding the "
             "base state.")
    o.append("        logical, intent(in) :: structure_only !! .true. when called from "
             "%clone_structure.")
    o.append("        !")
    o.append("        select type (out)")
    o.append(f"        class is ({t})")
    for name in copies:
        o.append(f"            out%{name} = self%{name}")
    o.extend(fill_window("            ", "clone_extra",
                         "anything the generator could not copy for you",
                         windows.get("clone_extra", [])))
    o.append("        end select")
    o.append(f"    end subroutine {t}_clone_extra")
    o.append("    !")
    return o


def emit_accessors(schema, f):
    """Every procedure body for one field."""
    t = schema.type_name
    o = []
    summary = field_summary(f)
    ptr_note = ("Points into the live storage: zero copy, writable, and invalidated by any "
                "row-structural mutation (%filter_rows, %sort_by, %top_n, %delete_rows, %truncate, "
                "%append, %append_null_rows).")

    if f.family == "str":
        if not f.is_vector:
            o.extend(wrap_doc("    ", "!>", f"{summary} The packed string store itself, not a "
                                            f"character array -- read it and edit its values in "
                                            f"place, but do not change its length or element "
                                            f"count through it."))
            o.append(f"    function {t}_{f.name}_all(self) result(p)")
            o.append(f"        class({t}), intent(in), target :: self !! the table.")
            o.append("        type(parquet_string_column), pointer :: p !! alias to the packed "
                     "store.")
            o.append("        !")
            o.append(f'        call self%col("{f.name}", p)')
            o.append(f"    end function {t}_{f.name}_all")
            o.append("    !")
            for kind, suffix in (("integer(int32)", "i32"), ("integer(int64)", "i64")):
                # MAX requires every argument to share one kind (F2018 16.9.147). A bare `0`
                # beside an int64 `hi - lo + 1` is a gfortran/ifx extension that nagfor reports
                # as Extension(NAG); spell the literal in the index's own kind instead.
                zero = "0_int64" if suffix == "i64" else "0"
                o.extend(wrap_doc("    ", "!>", f"{summary} A zero-copy handle to row `i`. "
                                                f"Repeats the column lookup on every call."))
                o.append(f"    function {t}_{f.name}_at_{suffix}(self, i) result(h)")
                o.append(f"        class({t}), intent(in), target :: self !! the table.")
                o.append(f"        {kind}, intent(in) :: i !! 1-based row index within this table.")
                o.append("        type(parquet_string) :: h !! handle to that row's value.")
                o.append("        type(parquet_string_column), pointer :: p")
                o.append("        !")
                o.append(f'        call self%col("{f.name}", p)')
                o.append("        h = p%view(i)")
                o.append(f"    end function {t}_{f.name}_at_{suffix}")
                o.append("    !")
                o.extend(wrap_doc("    ", "!>", f"{summary} One zero-copy handle per row of "
                                                f"`[lo, hi]`, in order."))
                o.append(f"    function {t}_{f.name}_rng_{suffix}(self, lo, hi) result(h)")
                o.append(f"        class({t}), intent(in), target :: self !! the table.")
                o.append(f"        {kind}, intent(in) :: lo !! first row of the range (1-based).")
                o.append(f"        {kind}, intent(in) :: hi !! last row of the range (inclusive).")
                o.append("        type(parquet_string), allocatable :: h(:) !! one handle per row.")
                o.append("        type(parquet_string_column), pointer :: p")
                o.append("        !")
                o.append(f'        call self%col("{f.name}", p)')
                o.append(f"        allocate(h(max(hi - lo + 1, {zero})))")
                o.append("        call p%view_slice(lo, hi, h)")
                o.append(f"    end function {t}_{f.name}_rng_{suffix}")
                o.append("    !")
        rank = "(:,:)" if f.is_vector else "(:)"
        shape = "(element, row)" if f.is_vector else "one value per row"
        o.extend(wrap_doc("    ", "!>", f"{summary} Copied out as a character array sized to the "
                                        f"longest value present. A null element comes back blank "
                                        f"-- gate on %is_null to tell a null from a genuinely "
                                        f"empty string."))
        o.append(f"    subroutine {t}_{f.name}_chr(self, arr)")
        o.append(f"        class({t}), intent(in) :: self !! the table.")
        o.append(f"        character(len=:), allocatable, intent(out) :: arr{rank} !! {shape}.")
        o.append("        !")
        o.append(f'        call self%get("{f.name}", arr)')
        o.append(f"    end subroutine {t}_{f.name}_chr")
        o.append("    !")
        return o

    whole = "(:,:)" if f.is_vector else "(:)"
    one = "(:)" if f.is_vector else ""
    rng = "(:,:)" if f.is_vector else "(:)"
    decl = f.decl

    o.extend(wrap_doc("    ", "!>", f"{summary} The whole column. {ptr_note}"))
    o.append(f"    function {t}_{f.name}_all(self) result(p)")
    o.append(f"        class({t}), intent(in), target :: self !! the table.")
    o.append(f"        {decl}, pointer :: p{whole} !! alias to the column's storage.")
    o.append("        !")
    o.append(f'        call self%col("{f.name}", p)')
    o.append(f"    end function {t}_{f.name}_all")
    o.append("    !")

    for kind, suffix in (("integer(int32)", "i32"), ("integer(int64)", "i64")):
        # The guards take int64; the int32 specifics widen on the way in.
        gi = "int(i, int64)" if suffix == "i32" else "i"
        glo = "int(lo, int64)" if suffix == "i32" else "lo"
        ghi = "int(hi, int64)" if suffix == "i32" else "hi"
        what = (f"row `i`'s {f.col_size} values" if f.is_vector else "row `i` alone")
        o.extend(wrap_doc("    ", "!>", f"{summary} {what.capitalize()}. Repeats the column "
                                        f"lookup on every call -- take `%{f.name}()` once "
                                        f"instead when iterating."))
        o.append(f"    function {t}_{f.name}_at_{suffix}(self, i) result(p)")
        o.append(f"        class({t}), intent(in), target :: self !! the table.")
        o.append(f"        {kind}, intent(in) :: i !! 1-based row index within this table.")
        o.append(f"        {decl}, pointer :: p{one} !! alias to that row's storage.")
        o.append(f"        {decl}, pointer :: a{whole}")
        o.append("        !")
        o.append(f'        call self%col("{f.name}", a)')
        o.append(f'        call {t}_check_row(self, {gi}, "{f.name}")')
        o.append(f"        p => a({':, ' if f.is_vector else ''}i)")
        o.append(f"    end function {t}_{f.name}_at_{suffix}")
        o.append("    !")
        o.extend(wrap_doc("    ", "!>", f"{summary} Rows `lo` through `hi` inclusive"
                                        f"{', full width' if f.is_vector else ''}. {ptr_note}"))
        o.append(f"    function {t}_{f.name}_rng_{suffix}(self, lo, hi) result(p)")
        o.append(f"        class({t}), intent(in), target :: self !! the table.")
        o.append(f"        {kind}, intent(in) :: lo !! first row of the range (1-based).")
        o.append(f"        {kind}, intent(in) :: hi !! last row of the range (inclusive).")
        o.append(f"        {decl}, pointer :: p{rng} !! alias to that range's storage.")
        o.append(f"        {decl}, pointer :: a{whole}")
        o.append("        !")
        o.append(f'        call self%col("{f.name}", a)')
        o.append(f'        call {t}_check_range(self, {glo}, {ghi}, "{f.name}")')
        o.append(f"        p => a({':, ' if f.is_vector else ''}lo:hi)")
        o.append(f"    end function {t}_{f.name}_rng_{suffix}")
        o.append("    !")
    return o


def emit_index_guards(schema):
    """The two bounds guards every indexed accessor shares.

    Emitted once per module rather than inlined per accessor: with five specifics per field the
    inlined form would repeat the same four lines hundreds of times in a wide schema.
    """
    t = schema.type_name
    o = []
    o.extend(wrap_doc("    ", "!>", "error stops unless `i` is a row this table has. A pointer to "
                                    "an out-of-range element would be undefined behaviour rather "
                                    "than a wrong answer, so this is checked rather than trusted."))
    o.append(f"    subroutine {t}_check_row(self, i, name)")
    o.append(f"        class({t}), intent(in) :: self !! the table.")
    o.append("        integer(int64), intent(in) :: i !! the 1-based row index to check.")
    o.append("        character(len=*), intent(in) :: name !! column name, for the message.")
    o.append("        !")
    o.append("        if (i < 1_int64 .or. i > self%nrows()) then")
    o.append(f'            error stop "{schema.type_name}: row index out of range for column \'" '
             f'// trim(name) // "\'"')
    o.append("        end if")
    o.append(f"    end subroutine {t}_check_row")
    o.append("    !")
    o.extend(wrap_doc("    ", "!>", "error stops unless `[lo, hi]` is a row range this table has. "
                                    "An empty range (`hi < lo`) is accepted and yields a "
                                    "zero-length pointer, matching Fortran's own section rules."))
    o.append(f"    subroutine {t}_check_range(self, lo, hi, name)")
    o.append(f"        class({t}), intent(in) :: self !! the table.")
    o.append("        integer(int64), intent(in) :: lo !! first row of the range.")
    o.append("        integer(int64), intent(in) :: hi !! last row of the range.")
    o.append("        character(len=*), intent(in) :: name !! column name, for the message.")
    o.append("        !")
    o.append("        if (hi < lo) return")
    o.append("        if (lo < 1_int64 .or. hi > self%nrows()) then")
    o.append(f'            error stop "{schema.type_name}: row range out of range for column \'" '
             f'// trim(name) // "\'"')
    o.append("        end if")
    o.append(f"    end subroutine {t}_check_range")
    o.append("    !")
    return o


# --------------------------------------------------------------------------------------
# Driving.
# --------------------------------------------------------------------------------------
def build(maml_path, out_dir, existing_text=None):
    """Parses one MAML and renders its module. Returns (out_path, text, skipped)."""
    text = maml_path.read_text(encoding="utf-8")
    schema = parse_maml(text, maml_path)
    out_path = out_dir / f"{schema.dataset}.f90"
    windows = extract_windows(existing_text, out_path) if existing_text else {}
    if windows.get("bindings"):
        _check_binding_collisions(schema, windows["bindings"], out_path)
    rendered, skipped = render(schema, windows)
    long_lines = [(n, len(ln)) for n, ln in enumerate(rendered.splitlines(), 1)
                  if len(ln) > MAX_LINE]
    if long_lines:
        raise MamlError(f"{out_path}: emitted lines exceed the {MAX_LINE}-column limit: " +
                        ", ".join(f"line {n} ({c} columns)" for n, c in long_lines[:5]))
    return out_path, rendered, skipped


def _check_binding_collisions(schema, binding_lines, out_path):
    """Refuses a user binding whose name an accessor is about to take.

    The generator can see both windows, so it reports this rather than emitting a file that fails
    to compile with a duplicate-binding error pointing at neither cause.
    """
    generated = {f.name.lower() for f in schema.fields}
    for f in schema.fields:
        if f.family == "str":
            generated.add(f"{f.name.lower()}_chr")
    generated |= GENERATED_BINDINGS | {"clone_extra"}
    for line in binding_lines:
        m = re.match(r"\s*(?:procedure|generic)\s*(?:,[^:]*)?::\s*([A-Za-z][A-Za-z0-9_]*)",
                     _strip_fortran_comment(line))
        if m and m.group(1).lower() in generated:
            raise MamlError(f"{out_path}: your USER SECTION (bindings) declares "
                            f"`{m.group(1)}`, which this schema also generates; rename yours, or "
                            f"rename the field")


def collect_inputs(args):
    if args.maml:
        paths = [pathlib.Path(p) for p in args.maml]
        for p in paths:
            if not p.is_file():
                raise MamlError(f"{p}: no such file")
        return paths
    d = pathlib.Path(args.dir)
    if not d.is_dir():
        raise MamlError(f"{d}/: no such directory -- put your table-type MAML files there, or "
                        f"pass --dir=<name> / explicit file paths")
    paths = sorted(d.glob("*.maml"))
    if not paths:
        raise MamlError(f"{d}/: no .maml files found")
    return paths


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("maml", nargs="*", help="Role-A MAML files (default: every *.maml in --dir)")
    ap.add_argument("--dir", default="table_types", help="directory of Role-A MAML files")
    ap.add_argument("--out-dir", default="src", help="where to write <dataset>.f90")
    ap.add_argument("--check", action="store_true",
                    help="verify the committed output is current; write nothing")
    ap.add_argument("--self-test", action="store_true", help="run this script's own tests")
    args = ap.parse_args(argv)

    if args.self_test:
        return self_test()

    try:
        inputs = collect_inputs(args)
        out_dir = pathlib.Path(args.out_dir)
        seen = {}
        results = []
        for path in inputs:
            existing = None
            probe = out_dir / (parse_maml(path.read_text(encoding="utf-8"), path).dataset + ".f90")
            if probe.exists():
                existing = probe.read_text(encoding="utf-8")
            out_path, text, skipped = build(path, out_dir, existing)
            if out_path in seen:
                raise MamlError(f"{path} and {seen[out_path]} both name module "
                                f"'{out_path.stem}' in their `dataset:`")
            seen[out_path] = path
            results.append((path, out_path, text, skipped))
    except MamlError as exc:
        print(f"generate_user_table_code.py: {exc}", file=sys.stderr)
        return 1

    if args.check:
        drift = False
        for path, out_path, text, _skipped in results:
            if not out_path.exists():
                print(f"generate_user_table_code.py: {out_path} does not exist; run the "
                      f"generator", file=sys.stderr)
                drift = True
                continue
            current = out_path.read_text(encoding="utf-8")
            if current == text:
                continue
            drift = True
            stale = _digest_of(current) not in (None, _sha_of(path))
            if stale:
                print(f"generate_user_table_code.py: {out_path} is stale -- {path} has changed "
                      f"since it was generated. Re-run the generator and commit the result.",
                      file=sys.stderr)
            else:
                print(f"generate_user_table_code.py: {out_path} has been edited outside its USER "
                      f"SECTIONs. Move your changes into a USER SECTION and re-run the "
                      f"generator.", file=sys.stderr)
                _report_first_difference(current, text, out_path)
        if drift:
            return 1
        print(f"generate_user_table_code.py: committed output is up to date "
              f"({len(results)} schema(s))")
        return 0

    out_dir = pathlib.Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    for path, out_path, text, skipped in results:
        out_path.write_text(text, encoding="utf-8")
        print(f"generate_user_table_code.py: wrote {out_path} from {path} "
              f"({len(text.splitlines())} lines)")
        # Keep this warning. It is the only notice that a component was not handled, and a
        # component `clone_extra` does not copy arrives default-initialized in every `%clone`, with
        # nothing else reporting it (`test_clone_carries_user_state`). Do not widen
        # parse_components to guess at what it skips instead.
        for name, why in skipped:
            print(f"  note: component '{name}' was left for you -- {why}", file=sys.stderr)
    return 0


def _sha_of(path):
    return hashlib.sha256(path.read_text(encoding="utf-8").encode("utf-8")).hexdigest()


def _digest_of(text):
    m = re.search(r"^! source-maml-sha256: ([0-9a-f]{64})$", text, re.M)
    return m.group(1) if m else None


def _report_first_difference(current, wanted, out_path):
    a, b = current.splitlines(), wanted.splitlines()
    for n, (x, y) in enumerate(zip(a, b), start=1):
        if x != y:
            print(f"  first difference at {out_path}:{n}", file=sys.stderr)
            print(f"    committed: {x.strip()[:90]}", file=sys.stderr)
            print(f"    generated: {y.strip()[:90]}", file=sys.stderr)
            return
    print(f"  the two differ in length ({len(a)} vs {len(b)} lines)", file=sys.stderr)


# --------------------------------------------------------------------------------------
# Self-tests. Run in CI's lint stage, and they travel with the script to any project that
# copies it -- which is why they are here rather than in a separate test file.
# --------------------------------------------------------------------------------------
MINIMAL = """dataset: gen_mod
table: demo
author: A Tester <t@example.org>
fields:
- name: id
  data_type: int64
- name: ra
  unit: deg
  data_type: float64
- name: label
  data_type: string
  array_size: 8
- name: crd
  data_type: float32
  col_size: 3
- name: flux
  data_type: float64
  source: computed
"""


def _capture_main(argv):
    """Runs main(argv) with stderr captured. Returns (exit code, stderr text).

    `--check`'s two messages and the duplicate-module refusal are all produced inside main() and
    printed to stderr, so a self-test that only calls build() cannot see any of them.
    """
    import io
    import contextlib
    buf = io.StringIO()
    # stdout too, or a self-test run prints this generator's own "wrote ..." lines into CI's lint
    # log, where they read as a real generation having happened.
    with contextlib.redirect_stderr(buf), contextlib.redirect_stdout(io.StringIO()):
        rc = main(argv)
    return rc, buf.getvalue()


def _tmp_schema(tmp, text=MINIMAL, name="demo.maml"):
    p = tmp / name
    p.write_text(text, encoding="utf-8")
    return p


def self_test():
    import tempfile
    failures = []

    def check(cond, what):
        if not cond:
            failures.append(what)

    def expect_error(fn, fragment, what):
        try:
            fn()
        except MamlError as exc:
            check(fragment in str(exc), f"{what}: message was {exc!r}")
            return
        failures.append(f"{what}: expected a MamlError, got none")

    with tempfile.TemporaryDirectory() as td:
        tmp = pathlib.Path(td)
        out = tmp / "out"
        out.mkdir()
        maml = _tmp_schema(tmp)

        # --- a plain generation round -------------------------------------------------
        path, text, skipped = build(maml, out)
        check(path.name == "gen_mod.f90", "dataset: should name the file")
        check("type, extends(parquet_table) :: parquet_table_demo" in text,
              "table: should name the type")
        check("! Author: A Tester <t@example.org>" in text, "author: should reach the header")
        check(text.count("USER SECTION (") == 2 * len(USER_WINDOWS),
              "every user window should be emitted, open and close")
        check("integer(int64), pointer :: p(:)" in text, "int64 field -> int64 pointer")
        check("real(real32), pointer :: p(:,:)" in text, "a width-3 float32 field -> rank-2")
        check("type(parquet_string_column), pointer :: p" in text, "string -> the packed store")
        check("subroutine parquet_table_demo_label_chr" in text, "string -> a _chr accessor")
        check(".true., .true., .true., .true., .false." in text,
              "source: computed should reach bind_predefined as .false.")
        check("call self%init_extra()" in text, "every constructor should call init_extra")
        check(text.count("call self%init_extra()") == 6,
              "all six constructors (init, init_slice x2, init_empty x3) should call init_extra")
        check(not skipped, "an empty components window should skip nothing")
        for n, line in enumerate(text.splitlines(), 1):
            check(len(line) <= MAX_LINE, f"emitted line {n} is {len(line)} columns")

        # --- idempotence and round-tripping -------------------------------------------
        path.write_text(text, encoding="utf-8")
        _p2, text2, _s = build(maml, out, text)
        check(text2 == text, "regenerating an unchanged file should be byte-identical")

        # --- user windows survive, and are READ ---------------------------------------
        edited = text.replace(
            "        ! >>>>> USER SECTION (components) -- your own table parameters; "
            "preserved on regeneration",
            "        ! >>>>> USER SECTION (components) -- your own table parameters; "
            "preserved on regeneration\n        real(real64) :: zeropoint = 1.5_real64\n"
            "        integer :: nbad")
        _p3, text3, skipped3 = build(maml, out, edited)
        check("real(real64) :: zeropoint = 1.5_real64" in text3, "a user component should survive")
        check("out%zeropoint = self%zeropoint" in text3,
              "a user component should get a generated clone assignment")
        check("self%zeropoint = 1.5_real64" in text3,
              "a component with an initializer should reset to it")
        check("self%nbad = 0" in text3, "an intrinsic component with no initializer should zero")
        check(not skipped3, "both components should have been handled")

        # --- a derived-type component with no initializer is reported, not guessed at --
        edited4 = text.replace(
            "        ! >>>>> END USER SECTION (components)",
            "        type(parquet_string_column) :: extra\n"
            "        ! >>>>> END USER SECTION (components)")
        _p4, text4, skipped4 = build(maml, out, edited4)
        check("out%extra = self%extra" in text4, "a derived component should still be cloned")
        check(any(n == "extra" for n, _ in skipped4),
              "a derived component with no initializer should be reported for init_extra")
        check("self%extra =" not in text4, "and must not be reset by a guessed constructor")

        # --- malformed markers are refused before anything is rewritten ---------------
        broken = text.replace("    ! >>>>> END USER SECTION (procedures)", "")
        expect_error(lambda: build(maml, out, broken), "is never closed",
                     "a deleted end marker should be a hard error")
        broken2 = text.replace("        ! >>>>> END USER SECTION (components)", "")
        expect_error(lambda: build(maml, out, broken2), "starts while",
                     "an unbalanced marker should be a hard error")
        gone = "\n".join(ln for ln in text.splitlines() if "USER SECTION (uses)" not in ln)
        expect_error(lambda: build(maml, out, gone), "are missing",
                     "a deleted window should be a hard error")

        # --- a user binding colliding with a generated accessor is refused ------------
        clash = text.replace(
            "        ! >>>>> END USER SECTION (bindings)",
            "        procedure :: ra => my_ra\n        ! >>>>> END USER SECTION (bindings)")
        expect_error(lambda: build(maml, out, clash), "which this schema also generates",
                     "a colliding user binding should be refused")

        # --- MAML-level rejections ----------------------------------------------------
        for text_in, fragment, what in (
            ("dataset: m\nfields:\n- name: a\n  data_type: int32\n", "`table:` is required",
             "a missing table:"),
            ("dataset: m\ntable: t\nfields:\n- name: get\n  data_type: int32\n",
             "collides with a procedure", "a field named after an inherited binding"),
            ("dataset: m\ntable: t\nfields:\n- name: RA\n  data_type: int32\n- name: ra\n"
             "  data_type: int32\n", "differ only in case", "two fields differing only in case"),
            ("dataset: m\ntable: t\nfields:\n- name: a\n  data_type: banana\n",
             "unsupported", "an unknown data_type"),
            # The three container tokens are refused by their OWN arm, not by the unknown-token
            # one above: they are valid MAML, so the message has to say the limit is this
            # script's rather than the format's. Both cases are kept so a future edit cannot
            # collapse one into the other without a test noticing.
            ("dataset: m\ntable: t\nfields:\n- name: a\n  data_type: list[int32]\n",
             "declares a container type", "a list[] data_type"),
            ("dataset: m\ntable: t\nfields:\n- name: a\n  data_type: map[int32]\n",
             "declares a container type", "a map[] data_type"),
            ("dataset: m\ntable: t\nfields:\n- name: a\n  data_type: struct\n",
             "declares a container type", "a struct data_type"),
            ("dataset: m\ntable: t\nfields:\n- name: a\n  data_type: int32\n  col_size: auto\n",
             "cannot be used in a table-type schema", "col_size: auto"),
            ("dataset: m\ntable: t\nfields:\n- name: a b\n  data_type: int32\n",
             "not a valid Fortran identifier", "a non-identifier column name"),
            ("dataset: m\ntable: t\nfields:\n- name: a\n", "has no `data_type:`",
             "a field with no data_type"),
            ("dataset: m\ntable: t\nfields:\n- name: a\n  data_type: int32\n  source: guess\n",
             "expected", "an unknown source:"),
            # The module/type name collision: `dataset:` naming what `table:` derives. A module
            # cannot declare a type of its own name, so this would not compile.
            ("dataset: parquet_table_x\ntable: x\nfields:\n- name: a\n  data_type: int32\n",
             "cannot declare a type of its own name", "dataset: colliding with the derived type"),
            ("dataset: PARQUET_TABLE_X\ntable: x\nfields:\n- name: a\n  data_type: int32\n",
             "cannot declare a type of its own name",
             "the same collision in a different case"),
            # And the sibling case: `dataset:` naming a procedure the module declares.
            ("dataset: parquet_table_x_a_all\ntable: x\nfields:\n- name: a\n"
             "  data_type: int32\n", "cannot declare a procedure of its own name",
             "dataset: colliding with a generated procedure"),
        ):
            expect_error(lambda ti=text_in: parse_maml(ti, pathlib.Path("t.maml")), fragment, what)

        # --- `dataset:` equal to `table:` is NOT the collision, and must stay accepted ---
        same = _tmp_schema(tmp, "dataset: foo\ntable: foo\nfields:\n- name: a\n"
                                "  data_type: int32\n", "same.maml")
        _p6, text6, _s6 = build(same, out)
        check("module foo" in text6 and "type, extends(parquet_table) :: parquet_table_foo" in text6,
              "dataset: and table: sharing a name is fine -- the type is parquet_table_<table>")

        # --- an empty fields: is a valid bare template --------------------------------
        empty = _tmp_schema(tmp, "dataset: bare_mod\ntable: bare\n", "bare.maml")
        _p5, text5, _s5 = build(empty, out)
        check("type, extends(parquet_table) :: parquet_table_bare" in text5,
              "an empty fields: should still emit the type")
        check("bind_predefined" not in text5,
              "an empty fields: should emit no bind_predefined call at all")
        check(text5.count("USER SECTION (") == 2 * len(USER_WINDOWS),
              "an empty fields: should still have every user window")

        # --- the name-length limits, and the tighter one a `string` column carries ------
        #
        # The pair is the point: 60 characters is fine on an int64 column and refused on a string
        # one, because only the string column also gets a `<name>_chr` accessor. Asserting only the
        # refusal would pass just as happily against a generator that refused every 60-character
        # name, which is why the accepted half is here as its own check.
        long60 = "a" * 60
        expect_error(lambda: parse_maml("dataset: m\ntable: t\nfields:\n- name: %s\n"
                                        "  data_type: string\n" % long60,
                                        pathlib.Path("t.maml")),
                     "would exceed Fortran's 63-character identifier limit",
                     "a 60-character string field (its _chr accessor would not fit)")
        ok60 = parse_maml("dataset: m\ntable: t\nfields:\n- name: %s\n  data_type: int64\n"
                          % long60, pathlib.Path("t.maml"))
        check(ok60.fields[0].name == long60,
              "the same 60-character name must be ACCEPTED on a non-string column")
        expect_error(lambda: parse_maml("dataset: m\ntable: t\nfields:\n- name: %s\n"
                                        "  data_type: int64\n" % ("a" * 64),
                                        pathlib.Path("t.maml")),
                     "exceeds Fortran's 63-character identifier limit",
                     "a 64-character field name")
        expect_error(lambda: parse_maml("dataset: m\ntable: %s\nfields:\n- name: a\n"
                                        "  data_type: int64\n" % ("t" * 60),
                                        pathlib.Path("t.maml")),
                     "longer than Fortran's 63-character identifier limit",
                     "a table: making the type name too long")

        # --- two schemas naming the same module are refused ----------------------------
        #
        # That check lives in main(), not in build(), so none of the build()-level cases above can
        # reach it: it needs a real two-file run.
        dupdir = tmp / "dup"
        dupdir.mkdir()
        (dupdir / "one.maml").write_text("dataset: same_mod\ntable: one\nfields:\n- name: a\n"
                                         "  data_type: int32\n", encoding="utf-8")
        (dupdir / "two.maml").write_text("dataset: same_mod\ntable: two\nfields:\n- name: b\n"
                                         "  data_type: int32\n", encoding="utf-8")
        dupout = tmp / "dupout"
        dupout.mkdir()
        rc, err = _capture_main(["--dir", str(dupdir), "--out-dir", str(dupout)])
        check(rc == 1 and "both name module" in err,
              "two schemas sharing a dataset: should be refused; got rc=%r err=%r" % (rc, err))
        # The negative control: the same two schemas with distinct module names must succeed.
        (dupdir / "two.maml").write_text("dataset: other_mod\ntable: two\nfields:\n- name: b\n"
                                         "  data_type: int32\n", encoding="utf-8")
        rc2, err2 = _capture_main(["--dir", str(dupdir), "--out-dir", str(dupout)])
        check(rc2 == 0, "distinct dataset: names should be accepted; got rc=%r err=%r" % (rc2, err2))

        # --- --check tells a STALE file apart from a HAND-EDITED one --------------------
        #
        # Both are exit 1, so asserting the status proves nothing about the half a user acts on.
        # Each case therefore asserts the message that must appear AND the one that must not.
        cdir = tmp / "chk"
        cdir.mkdir()
        cout = tmp / "chkout"
        cout.mkdir()
        cmaml = cdir / "c.maml"
        cmaml.write_text(MINIMAL, encoding="utf-8")
        rc3, _ = _capture_main(["--dir", str(cdir), "--out-dir", str(cout)])
        check(rc3 == 0, "the fixture schema should generate cleanly")
        rc4, err4 = _capture_main(["--dir", str(cdir), "--out-dir", str(cout), "--check"])
        check(rc4 == 0, "a freshly generated file should be up to date; got %r" % (err4,))
        # (a) a generated region edited by hand: the digest still matches its MAML.
        gen = cout / "gen_mod.f90"
        body = gen.read_text(encoding="utf-8")
        gen.write_text(body.replace("    implicit none", "    implicit none\n    ! hand-edited",
                                    1), encoding="utf-8")
        rc5, err5 = _capture_main(["--dir", str(cdir), "--out-dir", str(cout), "--check"])
        check(rc5 == 1 and "edited outside its USER SECTION" in err5 and "is stale" not in err5,
              "a hand-edited generated region should report EDITED, not stale; got %r" % (err5,))
        # (b) the MAML changed under a file that is otherwise exactly as generated.
        gen.write_text(body, encoding="utf-8")
        cmaml.write_text(MINIMAL + "- name: extra\n  data_type: int32\n", encoding="utf-8")
        rc6, err6 = _capture_main(["--dir", str(cdir), "--out-dir", str(cout), "--check"])
        check(rc6 == 1 and "is stale" in err6 and "edited outside its USER SECTION" not in err6,
              "a changed MAML should report STALE, not edited; got %r" % (err6,))

    # --- the baked-in reserved list must match parquet_table, when the source is here --
    src = pathlib.Path(__file__).resolve().parent.parent / "src" / "parquet_tables.f90"
    if src.is_file():
        actual = _reserved_from_source(src.read_text(encoding="utf-8"))
        missing = actual - RESERVED
        extra = RESERVED - actual
        check(not missing, f"RESERVED is missing {sorted(missing)} -- parquet_table gained a "
                           f"binding; add it to the list in this script")
        check(not extra, f"RESERVED lists {sorted(extra)}, which parquet_table no longer has")

    for f in failures:
        print(f"generate_user_table_code.py: SELF-TEST FAILED: {f}", file=sys.stderr)
    if failures:
        return 1
    print("generate_user_table_code.py: self-test passed")
    return 0


def _reserved_from_source(src):
    """Every type-bound procedure name declared in `type :: parquet_table`."""
    m = re.search(r"^    type :: parquet_table\b(.*?)^    end type parquet_table\b", src,
                  re.S | re.M)
    if not m:
        return set()
    names = set()
    for line in m.group(1).splitlines():
        s = line.strip()
        mm = re.match(r"(?:procedure|generic)\s*(?:,\s*[a-z]+\s*)*::\s*([a-z_0-9]+)\s*=>", s, re.I)
        if mm:
            names.add(mm.group(1).lower())
        mm = re.match(r"procedure\s*(?:,\s*[a-z]+\s*)*::\s*([a-z_0-9]+)\s*(?:!!.*)?$", s, re.I)
        if mm:
            names.add(mm.group(1).lower())
    names.discard("assignment")
    return names


if __name__ == "__main__":
    sys.exit(main())
