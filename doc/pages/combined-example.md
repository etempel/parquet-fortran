---
title: Combined example: MAML schema, vector columns and metadata
---

This example ties together MAML-driven column definitions, a vector column, dropping an optional column at runtime, and adding extra table metadata not present in the MAML file.

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
