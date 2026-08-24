!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Maintainer demo for reviewing `schema%print_schema_info`'s output by eye. No fixed contract and
!! nothing asserts it -- it exists so a change to that procedure's formatting can be looked at.
program demo_print_schema_info
    use parquet
    implicit none
    type(parquet_schema) :: schema1, schema2, schema3
    integer :: unit
    character(len=*), parameter :: out_file = "test_run/schema_info_demo.txt"

    call parquet_parse_maml("schemas/maml_example.maml", schema1)
    call parquet_parse_maml("schemas/maml_example2.maml", schema2)
    call parquet_parse_maml("schemas/maml_example3.maml", schema3)

    open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")

    write(unit, '(a)') "=== schemas/maml_example.maml (prefix=""# "", all three dash lines) ==="
    call schema1%print_schema_info(unit=unit, prefix="# ", dash_before_header=.true., dash_after_fields=.true.)

    write(unit, '(a)') ""
    write(unit, '(a)') "=== schemas/maml_example2.maml (defaults) ==="
    call schema2%print_schema_info(unit=unit)

    write(unit, '(a)') ""
    write(unit, '(a)') "=== schemas/maml_example3.maml (defaults) ==="
    call schema3%print_schema_info(unit=unit, table_name=.false.)

    close(unit)
    print '(a)', "Wrote " // out_file
end program demo_print_schema_info
