---
title: Combined examples: schemas, vector columns, metadata and quality control
---

## MAML schema, vector columns and metadata

This example ties together MAML-driven column definitions, a vector column, dropping an optional
column at runtime, and adding extra table metadata not present in the MAML file.

Dropping the columns you have no data for is a requirement rather than tidiness:
`parquet_close_writer` checks that every *enabled* column was actually written, and otherwise fails
with `parquet_close_writer: missing write for enabled column: <name>`. The rule is set out in full
under [writing parquet files](../io/writing.html).

The written file carries the MAML's own table metadata as well as the runtime key added here — its
header keys (`table:`, `author:`, ...) and every `keyarray:` entry are all readable afterwards with
`parquet_get_metadata`.

The schema file is this repository's own `schemas/maml_example.maml`, which declares thirteen
fields. Only the two the program writes matter here:

```yaml
table: input_table
fields:
- name: id0
  data_type: int32
- name: idarr
  data_type: int64
  col_size: 2
```

`col_size: 2` is what makes `idarr` a vector column two elements wide, and so what makes the
program's `idarr(2, 3)` mean two elements across three rows.

```fortran
program write_parquet_combined_example
    use parquet
    use iso_fortran_env, only: int32, int64, real64
    implicit none

    type(parquet_writer) :: writer
    type(parquet_schema) :: schema
    integer(int32) :: id0(3)
    integer(int64) :: idarr(2, 3)   ! (col_size, nrows) for the "idarr" vector column

    ! Parse column definitions + table metadata from the MAML file.
    call parquet_parse_maml("schemas/maml_example.maml", schema)

    ! This schema defines more columns than we have data for in this example;
    ! disable everything, then re-enable only the columns we are about to write.
    call schema%set_column_unavailable()
    call schema%set_column_available("id0")
    call schema%set_column_available("idarr")

    ! Add an extra, run-time-only piece of metadata not present in the MAML file.
    call schema%add_metadata("generated_by", "write_parquet_combined_example")

    id0 = [1_int32, 2_int32, 3_int32]
    idarr = reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64], [2, 3])

    call parquet_open_writer(writer, "data.parquet", schema)
    call parquet_write_column(writer, "id0", id0)
    call parquet_write_column(writer, "idarr", idarr)
    call parquet_close_writer(writer)
end program write_parquet_combined_example
```

## Nulls, quality control and compression together

This ties together `is_valid` (writing a genuine Null), qc range-check warnings, and a non-default
compression codec in one small program.

`qc=.true.` is passed explicitly below so the intent is visible at the call site, but it is not what
switches the checks on: qc is already active whenever `parquet_open_writer` is given a `schema=`.
Pass `qc=.false.` to turn it off. See [quality control](quality-control.html#write-side-enforcement)
for what each check does.

```fortran
program write_parquet_qc_example
    use parquet
    use iso_fortran_env, only: int32
    implicit none

    type(parquet_schema) :: schema
    type(parquet_writer) :: writer
    type(parquet_reader) :: reader
    integer(int32) :: ra(4) = [10_int32, 400_int32, 90_int32, 200_int32]  ! 400 is out of range
    integer(int32) :: ra_read(4)
    logical :: is_valid(4) = [.true., .true., .false., .true.]           ! row 3 will be written as Null
    logical :: is_valid_read(4)

    call schema%init("qc_example_table")
    call schema%add_field("ra", "int32", qc_min=">= 0", qc_max="< 360")

    call parquet_open_writer(writer, "data.parquet", schema, qc=.true., compression="zstd")
    call parquet_write_column(writer, "ra", ra, is_valid=is_valid)
    ! prints: WARNING: qc violation for column 'ra': declared min >= 0, max < 360, ...
    call parquet_close_writer(writer)

    call parquet_open_reader(reader, "data.parquet")
    call parquet_read_column(reader, "ra", ra_read, is_valid=is_valid_read)
    call parquet_close_reader(reader)
end program write_parquet_qc_example
```

The WARNING the write prints in full:

```
WARNING: qc violation for column 'ra': declared min >= 0, max < 360, data range [10, 400], 1 of 3 valid element(s) out of range
```

Note the count: **1 of 3**, not 1 of 4. The Null written at row 3 is not a value, so the range check
neither passes nor fails it — it is simply not among the elements checked.

`is_valid_read` comes back as the mask that was written, so row 3 is reported Null. The *value* slot
beside it, `ra_read(3)`, holds `0` — not the original `90` — because no `null_value=` was passed on
the read; see [Null values](../types/supported-data-types.html#null-values).
