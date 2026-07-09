!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Standalone helper program that deliberately triggers `error stop` paths in
!> the parquet module. It is invoked as a subprocess (via `fpm test
!> error_scenarios -- <scenario>`) from test_errors.f90, because a Fortran
!> `error stop` aborts the whole process and cannot be caught in-process by
!> test-drive. The exit code (0 = no error stop reached, nonzero = aborted)
!> is the observable result.
program error_scenarios
    use parquet
    use parquet_maml_base, only: parquet_maml_file, get_parquet_maml
    use iso_fortran_env, only : int32, int64
    !$ use omp_lib, only : omp_get_max_threads, omp_get_thread_num
    implicit none

    character(len=64) :: scenario
    integer :: nargs

    nargs = command_argument_count()
    if (nargs < 1) then
        ! No scenario requested: this happens when `fpm test` auto-runs every
        ! test target with no arguments. Exit cleanly and silently rather than
        ! failing, since this program is only meant to be driven (with an
        ! explicit scenario) as a subprocess from test_errors.f90.
        stop
    end if
    call get_command_argument(1, scenario)

    select case (trim(scenario))
    case ("ok")
        continue
    case ("print_stat_smoke")
        call scenario_print_stat_smoke()
    case ("write_undeclared_column")
        call scenario_write_undeclared_column()
    case ("write_type_mismatch")
        call scenario_write_type_mismatch()
    case ("write_column_twice")
        call scenario_write_column_twice()
    case ("write_column_twice_no_schema")
        call scenario_write_column_twice_no_schema()
    case ("validate_bad_data_type")
        call scenario_validate_bad_data_type()
    case ("validate_excluded_date_type")
        call scenario_validate_bad_data_type_named("date")
    case ("validate_excluded_timestamp_type")
        call scenario_validate_bad_data_type_named("timestamp")
    case ("validate_excluded_decimal_type")
        call scenario_validate_bad_data_type_named("decimal")
    case ("validate_duplicate_name")
        call scenario_validate_duplicate_name()
    case ("validate_missing_table")
        call scenario_validate_missing_table()
    case ("validate_no_fields")
        call scenario_validate_no_fields()
    case ("validate_unknown_top_level_section")
        call scenario_validate_unknown_top_level_section()
    case ("validate_unknown_field_subkey")
        call scenario_validate_unknown_field_subkey()
    case ("validate_unknown_qc_subkey")
        call scenario_validate_unknown_qc_subkey()
    case ("validate_user_maml_unknown_column")
        call scenario_validate_user_maml_unknown_column()
    case ("validate_col_map_unknown_internal")
        call scenario_validate_col_map_unknown_internal()
    case ("validate_col_map_duplicate_internal")
        call scenario_validate_col_map_duplicate_internal()
    case ("validate_col_map_output_collision")
        call scenario_validate_col_map_output_collision()
    case ("validate_col_map_output_not_declared")
        call scenario_validate_col_map_output_not_declared()
    case ("validate_col_map_internal_also_in_fields")
        call scenario_validate_col_map_internal_also_in_fields()
    case ("validate_col_map_output_matches_other_field")
        call scenario_validate_col_map_output_matches_other_field()
    case ("get_column_index_not_found")
        call scenario_get_column_index_not_found()
    case ("write_maml_without_metadata")
        call scenario_write_maml_without_metadata()
    case ("read_column_with_nulls")
        call scenario_read_column_with_nulls()
    case ("read_unsupported_physical_type")
        call scenario_read_unsupported_physical_type()
    case ("prefetch_unknown_column")
        call scenario_prefetch_unknown_column()
    case ("filter_unknown_column")
        call scenario_filter_unknown_column()
    case ("filter_vector_column")
        call scenario_filter_vector_column()
    case ("filter_malformed_rule")
        call scenario_filter_malformed_rule()
    case ("filter_bad_numeric_value")
        call scenario_filter_bad_numeric_value()
    case ("filter_unquoted_string_value")
        call scenario_filter_unquoted_string_value()
    case ("filter_bad_boolean_value")
        call scenario_filter_bad_boolean_value()
    case ("filter_bool_ordering_not_supported")
        call scenario_filter_bool_ordering_not_supported()
    case ("qc_range_violation_warns")
        call scenario_qc_range_violation_warns()
    case ("qc_null_violation_warns")
        call scenario_qc_null_violation_warns()
    case ("qc_range_violation_hard_aborts")
        call scenario_qc_range_violation_hard_aborts()
    case ("qc_null_violation_hard_aborts")
        call scenario_qc_null_violation_hard_aborts()
    case ("qc_miss_null_no_warning")
        call scenario_qc_miss_null_no_warning()
    case ("qc_existing_null_abort_unchanged")
        call scenario_qc_existing_null_abort_unchanged()
    case ("qc_column_not_in_file")
        call scenario_qc_column_not_in_file()
    case ("qc_disabled_explicit_no_warning")
        call scenario_qc_disabled_explicit_no_warning()
    case ("qc_maml_bad_miss_value")
        call scenario_qc_maml_bad_miss_value()
    case ("qc_maml_duplicate_field")
        call scenario_qc_maml_duplicate_field()
    case ("qc_maml_missing_name")
        call scenario_qc_maml_missing_name()
    case ("qc_maml_unknown_subkey")
        call scenario_qc_maml_unknown_subkey()
    case ("write_row_count_mismatch")
        call scenario_write_row_count_mismatch()
    case ("read_row_count_mismatch")
        call scenario_read_row_count_mismatch()
    case ("read_before_open")
        call scenario_read_before_open()
    case ("write_before_open")
        call scenario_write_before_open()
    case ("get_nrows_before_open")
        call scenario_get_nrows_before_open()
    case ("close_reader_before_open")
        call scenario_close_reader_before_open()
    case ("close_writer_before_open")
        call scenario_close_writer_before_open()
    case ("read_unknown_column")
        call scenario_read_unknown_column()
    case ("open_reader_missing_file")
        call scenario_open_reader_missing_file()
    case ("open_writer_bad_path")
        call scenario_open_writer_bad_path()
    case ("write_string_matrix_exceeds_array_size")
        call scenario_write_string_matrix_exceeds_array_size()
    case ("validate_protected_cols_unknown_name")
        call scenario_validate_protected_cols_unknown_name()
    case ("write_protected_column_with_null")
        call scenario_write_protected_column_with_null()
    case ("validate_qc_min_not_numeric")
        call scenario_validate_qc_min_not_numeric()
    case ("validate_qc_min_non_integral_for_int32")
        call scenario_validate_qc_min_non_integral_for_int32()
    case ("validate_qc_min_out_of_int32_range")
        call scenario_validate_qc_min_out_of_int32_range()
    case ("validate_qc_min_wrong_operator")
        call scenario_validate_qc_min_wrong_operator()
    case ("validate_qc_max_wrong_operator")
        call scenario_validate_qc_max_wrong_operator()
    case ("qc_maml_min_wrong_operator")
        call scenario_qc_maml_min_wrong_operator()
    case ("add_col_qc_min_reversed_operator")
        call scenario_add_col_qc_min_reversed_operator()
    case ("add_col_qc_operator_without_value")
        call scenario_add_col_qc_operator_without_value()
    case ("add_col_qc_bad_miss_value")
        call scenario_add_col_qc_bad_miss_value()
    case ("add_col_qc_too_many_fields")
        call scenario_add_col_qc_too_many_fields()
    case ("add_col_qc_empty_column_name")
        call scenario_add_col_qc_empty_column_name()
    case ("add_col_qc_duplicate_column")
        call scenario_add_col_qc_duplicate_column()
    case ("get_col_qc_reversed_operator")
        call scenario_get_col_qc_reversed_operator()
    case ("qc_warning_numeric")
        call scenario_qc_warning_numeric()
    case ("qc_warning_string")
        call scenario_qc_warning_string()
    case ("qc_silently_ignored_for_boolean")
        call scenario_qc_silently_ignored_for_boolean()
    case ("write_unknown_compression")
        call scenario_write_unknown_compression()
    case ("write_values_not_divisible_by_col_size")
        call scenario_write_values_not_divisible_by_col_size()
    case ("set_max_threads_below_one")
        call scenario_set_max_threads_below_one()
    case ("concurrent_calls_into_shared_reader")
        call scenario_concurrent_calls_into_shared_reader()
    case ("concurrent_calls_into_shared_writer")
        call scenario_concurrent_calls_into_shared_writer()
    case default
        ! Deliberately a distinctive, otherwise-unused exit code (not 0, and
        ! not the plain 1 that `error stop "message"` produces) -- callers
        ! checking exit status can tell "the scenario name doesn't exist
        ! (typo?)" apart from "the scenario ran and genuinely aborted",
        ! which a plain `stop 1` here could not be told apart from.
        print '(a)', "unknown scenario: "//trim(scenario)
        stop 97
    end select

contains

    subroutine scenario_write_undeclared_column()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_undeclared.parquet", schema)
        call parquet_write_column(writer, "not_a_real_column", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_undeclared_column

    subroutine scenario_write_type_mismatch()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: data(1) = [.true.]

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        ! "id0" is declared as int32 in the MAML schema; writing a logical is a type mismatch.
        call parquet_open_writer(writer, "test_run/error_scenario_type_mismatch.parquet", schema)
        call parquet_write_column(writer, "id0", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_type_mismatch

    subroutine scenario_write_column_twice()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        ! The "written more than once" check only applies when a schema (cinfo) is
        ! enforced; a schema-less writer silently allows writing the same column twice.
        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_twice.parquet", schema)
        call parquet_write_column(writer, "id0", data)
        call parquet_write_column(writer, "id0", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_column_twice

    subroutine scenario_write_maml_without_metadata()
        type(parquet_writer) :: writer
        integer(int32) :: data(1) = [1_int32]

        ! write_maml=.true. requires a schema populated by parquet_parse_maml;
        ! a schema-less writer has no source MAML content to save.
        call parquet_open_writer(writer, "test_run/error_scenario_write_maml.parquet", write_maml=.true.)
        call parquet_write_column(writer, "id", data)
        call parquet_close_writer(writer)
    end subroutine scenario_write_maml_without_metadata

    subroutine scenario_validate_bad_data_type()
        type(parquet_maml_file) :: maml

        maml%name = "bad_data_type.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: not_a_real_type" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_bad_data_type

    !> Locks in the specific type exclusions documented in the README's
    !> Limitations section (no date/timestamp/decimal support): unlike
    !> scenario_validate_bad_data_type's generic garbage token, this uses the
    !> real excluded type name, so a future accidental addition of one of
    !> these types to valid_maml_data_types would be caught here.
    subroutine scenario_validate_bad_data_type_named(type_name)
        character(len=*), intent(in) :: type_name
        type(parquet_maml_file) :: maml

        maml%name = "excluded_data_type.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: "//trim(type_name) ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_bad_data_type_named

    subroutine scenario_validate_duplicate_name()
        type(parquet_maml_file) :: maml

        maml%name = "duplicate_name.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_duplicate_name

    subroutine scenario_validate_missing_table()
        type(parquet_maml_file) :: maml

        maml%name = "missing_table.maml"
        maml%lines = [character(len=40) :: &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_missing_table

    subroutine scenario_validate_no_fields()
        type(parquet_maml_file) :: maml

        maml%name = "no_fields.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_no_fields

    subroutine scenario_validate_unknown_top_level_section()
        type(parquet_maml_file) :: maml

        maml%name = "unknown_top_level_section.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "not_a_real_section: something", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_unknown_top_level_section

    subroutine scenario_validate_unknown_field_subkey()
        type(parquet_maml_file) :: maml

        maml%name = "unknown_field_subkey.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  not_a_real_subkey: something" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_unknown_field_subkey

    subroutine scenario_validate_unknown_qc_subkey()
        type(parquet_maml_file) :: maml

        maml%name = "unknown_qc_subkey.maml"
        maml%lines = [character(len=40) :: &
            "table: bad_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: 1", &
            "    max: 100", &
            "    not_a_real_qc_key: something" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_unknown_qc_subkey

    subroutine scenario_validate_user_maml_unknown_column()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "fields:", &
            "- name: b", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_user_maml_unknown_column

    subroutine scenario_validate_col_map_unknown_internal()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - not_a_real_internal_column: b", &
            "fields:", &
            "- name: b", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_unknown_internal

    subroutine scenario_validate_col_map_duplicate_internal()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: b", &
            "  - a: c", &
            "fields:", &
            "- name: b", &
            "  data_type: int32", &
            "- name: c", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_duplicate_internal

    subroutine scenario_validate_col_map_output_collision()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: shared_name", &
            "  - b: shared_name", &
            "fields:", &
            "- name: shared_name", &
            "  data_type: int32" ]

        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_output_collision

    subroutine scenario_validate_col_map_output_not_declared()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: my_a", &
            "fields:", &
            "- name: not_my_a", &
            "  data_type: int32" ]

        ! col_map renames "a" to "my_a", but no field named "my_a" is
        ! actually declared in fields: -- the rename has nothing to apply to.
        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_output_not_declared

    subroutine scenario_validate_col_map_internal_also_in_fields()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: my_a", &
            "fields:", &
            "- name: my_a", &
            "  data_type: int32", &
            "- name: a", &
            "  data_type: int32" ]

        ! col_map remaps "a", but "a" is also directly (un-renamed) declared
        ! as its own field in fields: -- ambiguous.
        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_internal_also_in_fields

    subroutine scenario_validate_col_map_output_matches_other_field()
        type(parquet_maml_file) :: base_maml, user_maml

        base_maml%name = "base.maml"
        base_maml%lines = [character(len=40) :: &
            "table: base_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "- name: b", &
            "  data_type: int32" ]

        user_maml%name = "user.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - a: b", &
            "fields:", &
            "- name: b", &
            "  data_type: int32" ]

        ! col_map renames "a" to output name "b", but the base schema already
        ! has a *different*, unrelated column genuinely named "b" -- even
        ! though it isn't separately declared here, activating it later
        ! (e.g. via set_available) would collide with the renamed field.
        call parquet_validate_user_maml(base_maml, user_maml)
    end subroutine scenario_validate_col_map_output_matches_other_field

    subroutine scenario_get_column_index_not_found()
        type(parquet_schema) :: schema
        integer :: idx

        schema%maml = get_parquet_maml("maml_example.maml")
        call parquet_parse_maml(schema)

        idx = schema%get_column_index("not_a_real_column")
        print '(a,i0)', "unexpectedly found index: ", idx
    end subroutine scenario_get_column_index_not_found

    !> test/fixtures/has_null.parquet is a fixture this library cannot write
    !> itself (it never calls Arrow's AppendNull anywhere on the write path):
    !> it was produced by a standalone Arrow/Parquet C++ program with a
    !> genuine Null in row 2 of "id_with_null", to exercise the read-side
    !> Null guard against a real Parquet Null (a validity-bitmap Null, not a
    !> sentinel value) rather than just reasoning about it.
    subroutine scenario_read_column_with_nulls()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        call parquet_read_column(reader, "id_with_null", values)
        print '(a)', "unexpectedly read a column containing Null values without error"
    end subroutine scenario_read_column_with_nulls

    !> test/fixtures/unsupported_type.parquet has a column ("d") of Arrow's
    !> date32 type -- one of the physical types outside this library's six
    !> supported types (see README's Limitations). This exercises that
    !> documented failure mode: parquet_wrapper.cpp's scalar read functions
    !> now catch the resulting type-mismatch exception at their own
    !> extern "C" boundary and abort cleanly (see report_fatal_error), rather
    !> than letting an uncaught C++ exception reach std::terminate().
    subroutine scenario_read_unsupported_physical_type()
        type(parquet_reader) :: reader
        integer(int32) :: values(3)

        call parquet_open_reader(reader, "test/fixtures/unsupported_type.parquet")
        call parquet_read_column(reader, "d", values)
        print '(a)', "unexpectedly read a column of an unsupported physical type without error"
    end subroutine scenario_read_unsupported_physical_type

    !> Arrow/Parquet requires every column in a table to have the same number
    !> of rows. parquet_write_column now records the row count of the first
    !> column written and error stops with a dedicated message the moment a
    !> later column's row count disagrees -- rather than letting this reach
    !> parquet_close_writer, where it used to surface as Arrow's own uncaught
    !> "table.Validate()" exception inside WriteTable, aborting the process.
    subroutine scenario_write_row_count_mismatch()
        type(parquet_writer) :: writer

        call parquet_open_writer(writer, "test_run/row_count_mismatch.parquet")
        call parquet_write_column(writer, "a", [1, 2, 3, 4, 5])
        call parquet_write_column(writer, "b", [10, 20, 30])
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote columns with mismatched row counts without error"
    end subroutine scenario_write_row_count_mismatch

    !> reader%handle is c_null_ptr until parquet_open_reader is called; every
    !> C++ read entry point used to dereference it unconditionally (see
    !> ConcurrencyGuard in parquet_wrapper.cpp), so calling parquet_read_column
    !> on an unopened reader crashed with an unhelpful, message-less SIGSEGV.
    !> parquet_read_column now checks this itself first and error stops.
    subroutine scenario_read_before_open()
        type(parquet_reader) :: reader
        integer :: values(3)

        call parquet_read_column(reader, "a", values)
        print '(a)', "unexpectedly read from an unopened reader without error"
    end subroutine scenario_read_before_open

    !> Same issue as scenario_read_before_open, but for the write side:
    !> writer%handle is c_null_ptr until parquet_open_writer is called.
    subroutine scenario_write_before_open()
        type(parquet_writer) :: writer

        call parquet_write_column(writer, "a", [1, 2, 3])
        print '(a)', "unexpectedly wrote to an unopened writer without error"
    end subroutine scenario_write_before_open

    !> parquet_close_reader used to silently no-op on a reader that was never
    !> opened (or already closed) -- matching the automatic finalizer's own
    !> safe-no-op behavior, but leaving a real user mistake (closing something
    !> that was never opened) undetected. It now error stops instead; the
    !> finalizer itself (reader_finalize) is untouched and still no-ops, since
    !> that path legitimately runs on every never-opened reader that goes out
    !> of scope and must not crash the program.
    subroutine scenario_close_reader_before_open()
        type(parquet_reader) :: reader

        call parquet_close_reader(reader)
        print '(a)', "unexpectedly closed a never-opened reader without error"
    end subroutine scenario_close_reader_before_open

    !> Same issue as scenario_close_reader_before_open, but for the writer
    !> side.
    subroutine scenario_close_writer_before_open()
        type(parquet_writer) :: writer

        call parquet_close_writer(writer)
        print '(a)', "unexpectedly closed a never-opened writer without error"
    end subroutine scenario_close_writer_before_open

    !> parquet_read_column (and every other reader procedure naming a column:
    !> parquet_get_col_size, parquet_get_column_total_elements,
    !> parquet_get_string_length, parquet_read_array_row_mode,
    !> parquet_read_array_element_mode) now validates the column name against
    !> the file's actual schema before reading any data, and error stops with
    !> a dedicated message -- the same class of fix already applied to
    !> parquet_prefetch_columns. Previously this reached the underlying C++
    !> "Column not found" exception uncaught, aborting with SIGABRT.
    subroutine scenario_read_unknown_column()
        type(parquet_reader) :: reader
        integer :: values(3)

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        call parquet_read_column(reader, "not_a_real_column", values)
        print '(a)', "unexpectedly read an unknown column without error"
    end subroutine scenario_read_unknown_column

    !> The same "reader has not been opened" guard now covers every other
    !> reader-taking procedure too (parquet_prefetch_columns, parquet_get_nrows/
    !> parquet_get_col_size/parquet_get_column_total_elements/
    !> parquet_get_string_length, parquet_read_array_row_mode/
    !> parquet_read_array_element_mode), not just parquet_read_column.
    !> parquet_get_nrows here is just one representative of that group.
    subroutine scenario_get_nrows_before_open()
        type(parquet_reader) :: reader
        integer(int64) :: nrows

        call parquet_get_nrows(reader, nrows)
        print '(a,i0)', "unexpectedly read nrows from an unopened reader without error: ", nrows
    end subroutine scenario_get_nrows_before_open

    !> parquet_read_column now validates the given `values` array's row count
    !> against the file's actual row count before reading any data, and error
    !> stops with a dedicated message -- rather than letting the underlying
    !> C++ read call's own "nrows mismatch" check run, which reports a clean
    !> diagnostic but via std::abort() (see report_fatal_error in
    !> parquet_wrapper.cpp), not a Fortran error stop.
    subroutine scenario_read_row_count_mismatch()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer :: a_read(3) ! file has 5 rows

        call parquet_open_writer(writer, "test_run/read_row_count_mismatch.parquet")
        call parquet_write_column(writer, "a", [1, 2, 3, 4, 5])
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/read_row_count_mismatch.parquet")
        call parquet_read_column(reader, "a", a_read)
        print '(a)', "unexpectedly read a column into a wrong-size array without error"
    end subroutine scenario_read_row_count_mismatch

    !> parquet_prefetch_columns validates every requested name against the
    !> file's actual schema before doing any Arrow read, and reports an
    !> ordinary Fortran error stop naming the missing column -- rather than
    !> letting the underlying C++ "Column not found" exception escape
    !> uncaught across the Fortran/C++ boundary (which would abort the
    !> process with a raw libc++abi/SIGABRT message instead).
    subroutine scenario_prefetch_unknown_column()
        type(parquet_reader) :: reader

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        call parquet_prefetch_columns(reader, ["not_a_real_column"])
        print '(a)', "unexpectedly prefetched an unknown column without error"
    end subroutine scenario_prefetch_unknown_column

    !> print_stat=.true. always prints to stdout -- run out-of-process (like
    !> every other scenario here) specifically so that output lands in the
    !> subprocess's own captured stdout (check_scenario_exit_status redirects
    !> it to /dev/null) instead of interleaving with test-drive's own
    !> progress lines in the visible `fpm test` console output. Checks that
    !> print_stat=.true. (with a mix of a prefetched-only column, a column
    !> actually read, and a column nobody touched at all) doesn't disturb the
    !> close itself or the data already read back, and that the file is left
    !> in a normal, readable state afterwards -- error stops (a genuine
    !> failure, not just "printed something") if either check fails.
    subroutine scenario_print_stat_smoke()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: a_values(5), b_values(5), c_values(5)
        integer(int32) :: a_back(5)
        character(len=*), parameter :: out_file = "test_run/scenario_print_stat.parquet"
        integer :: i
        integer(int32) :: nrows

        a_values = [(i, i=1,5)]
        b_values = [(i*10, i=1,5)]
        c_values = [(i*100, i=1,5)]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "a", a_values)
        call parquet_write_column(writer, "b", b_values)
        call parquet_write_column(writer, "c", c_values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_prefetch_columns(reader, ["b"])
        call parquet_read_column(reader, "a", a_back)
        ! "c" is deliberately never prefetched or read, to exercise the
        ! "untouched columns are left out of the report" behavior.
        call parquet_close_reader(reader, print_stat=.true.)

        if (.not. all(a_back == a_values)) then
            error stop "print_stat=.true. disturbed a column already read back before the close"
        end if

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)

        if (nrows /= 5) then
            error stop "file was left in a bad state after parquet_close_reader(print_stat=.true.)"
        end if
    end subroutine scenario_print_stat_smoke

    !> parquet_open_reader(..., filter=) validates every filter column name
    !> against the file's actual schema before applying it, the same as
    !> parquet_prefetch_columns does for its own names -- an unknown column
    !> reports a clean Fortran error stop naming it, rather than reaching
    !> Arrow's own uncaught "Column not found" exception.
    subroutine scenario_filter_unknown_column()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("not_a_real_column > 5")
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a filter naming an unknown column"
    end subroutine scenario_filter_unknown_column

    !> Filtering only supports plain scalar columns (col_size == 1): a rule
    !> naming a vector/list column reports a clean error stop instead of
    !> silently picking (or crashing on) some undefined per-row semantics.
    subroutine scenario_filter_vector_column()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: vec(2,3)

        vec(:,1) = [1_int64, 2_int64]
        vec(:,2) = [3_int64, 4_int64]
        vec(:,3) = [5_int64, 6_int64]

        call parquet_open_writer(writer, "test_run/filter_vector_column.parquet")
        call parquet_write_column(writer, "vec", vec)
        call parquet_close_writer(writer)

        call filt%add("vec > 3")
        call parquet_open_reader(reader, "test_run/filter_vector_column.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a filter naming a vector column"
    end subroutine scenario_filter_vector_column

    !> parquet_tokenize_filter_rule (parquet_read.f90) rejects a rule that
    !> doesn't have the "<column> <op> [value]" shape (here: no operator at
    !> all) with a clean error stop, before ever reaching the C++ side.
    subroutine scenario_filter_malformed_rule()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("ra")
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a malformed filter rule"
    end subroutine scenario_filter_malformed_rule

    !> A rule whose shape is fine ("<column> <op> <value>") but whose value
    !> isn't a valid number for a numeric column reports a clean error stop
    !> naming the bad value and the column, from parquet_reader_set_filter
    !> (parquet_wrapper.cpp) -- distinct from scenario_filter_malformed_rule,
    !> which is a Fortran-side syntax/shape rejection before the value is
    !> ever inspected against the actual column type.
    subroutine scenario_filter_bad_numeric_value()
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call filt%add("id_with_null > abc")
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with a non-numeric value against a numeric filter column"
    end subroutine scenario_filter_bad_numeric_value

    !> A string column's filter value must be double-quoted; a bare,
    !> unquoted word is rejected rather than silently treated as a string.
    subroutine scenario_filter_unquoted_string_value()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_unquoted_string.parquet")
        call parquet_write_column(writer, "name", ["abc", "def"])
        call parquet_close_writer(writer)

        call filt%add("name == abc")
        call parquet_open_reader(reader, "test_run/filter_unquoted_string.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with an unquoted value against a string filter column"
    end subroutine scenario_filter_unquoted_string_value

    !> A boolean column's filter value must be the literal true/false; any
    !> other value (numeric, quoted, or otherwise) is rejected.
    subroutine scenario_filter_bad_boolean_value()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_bad_boolean.parquet")
        call parquet_write_column(writer, "flag", [.true., .false.])
        call parquet_close_writer(writer)

        call filt%add("flag == 5")
        call parquet_open_reader(reader, "test_run/filter_bad_boolean.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with an invalid boolean value in a filter rule"
    end subroutine scenario_filter_bad_boolean_value

    !> Ordering comparisons (>, >=, <, <=) don't have a meaningful definition
    !> for a boolean column -- only ==/=/= are accepted; an ordering operator
    !> against a boolean column is rejected.
    subroutine scenario_filter_bool_ordering_not_supported()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt

        call parquet_open_writer(writer, "test_run/filter_bool_ordering.parquet")
        call parquet_write_column(writer, "flag", [.true., .false.])
        call parquet_close_writer(writer)

        call filt%add("flag > true")
        call parquet_open_reader(reader, "test_run/filter_bool_ordering.parquet", filter=filt)
        print '(a)', "unexpectedly opened a reader with an ordering comparison against a boolean filter column"
    end subroutine scenario_filter_bool_ordering_not_supported

    !> Writes `lines` verbatim to `path`, one per record -- used by the
    !> read-time qc scenarios below to produce a throwaway qc-maml file
    !> (parquet_load_qc_maml_file only reads from disk, no in-memory
    !> constructor exists for a qc-maml, same as every other maml in this
    !> codebase).
    subroutine write_text_file(path, lines)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: lines(:)
        integer :: unit, i

        open(newunit=unit, file=path, status="replace", action="write")
        do i = 1, size(lines)
            write(unit, '(a)') trim(lines(i))
        end do
        close(unit)
    end subroutine write_text_file

    !> parquet_open_reader(..., schema=) with a qc: min:/max: declared for
    !> "ra" must print exactly one aggregate WARNING (matching the writer's
    !> own qc: wording) when parquet_read_column reads an out-of-range
    !> value, but must NOT abort -- with qc_soft=.true. qc is diagnostic-only.
    !> (The default, qc_soft=.false., aborts instead -- see
    !> scenario_qc_range_violation_hard_aborts.)
    subroutine scenario_qc_range_violation_warns()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4), ra_back(4)

        ra = [10, 400, -5, 300] ! 400 and -5 are outside [0, 360]

        call parquet_open_writer(writer, "test_run/qc_range.parquet")
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_range.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: 0", "    max: 360"])

        call parquet_open_reader(reader, "test_run/qc_range.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_range.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_range_violation_warns

    !> A column with a genuine Parquet Null, read with is_valid= (so the
    !> read itself doesn't abort), against a qc-maml field with no qc:
    !> miss: declared (Nulls unexpected by default) must print exactly one
    !> aggregate Null-presence WARNING with qc_soft=.true. (The default,
    !> qc_soft=.false., aborts instead -- see
    !> scenario_qc_null_violation_hard_aborts.)
    subroutine scenario_qc_null_violation_warns()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(4), id_back(4)
        logical :: is_valid_in(4), is_valid_out(4)

        id = [1, 2, 3, 4]
        is_valid_in = [.true., .false., .true., .true.]

        call parquet_open_writer(writer, "test_run/qc_null.parquet")
        call parquet_write_column(writer, "id", id, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_null.maml", [character(len=32) :: &
            "fields:", "- name: id", "  qc:", "    min: 0"])

        call parquet_open_reader(reader, "test_run/qc_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_null.maml"), qc_soft=.true.)
        call parquet_read_column(reader, "id", id_back, is_valid=is_valid_out)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_null_violation_warns

    !> Default (qc_soft=.false., hard): reading an out-of-range value with
    !> qc active aborts the process, via the report_fatal_error convention
    !> (stderr diagnostic + SIGABRT), the same class of clean read-side
    !> abort as the Null/type-mismatch checks.
    subroutine scenario_qc_range_violation_hard_aborts()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4), ra_back(4)

        ra = [10, 400, -5, 300] ! 400 and -5 are outside [0, 360]

        call parquet_open_writer(writer, "test_run/qc_range_hard.parquet")
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_range_hard.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: 0", "    max: 360"])

        call parquet_open_reader(reader, "test_run/qc_range_hard.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_range_hard.maml"))
        call parquet_read_column(reader, "ra", ra_back)
        print '(a)', "unexpectedly read an out-of-range value without aborting in hard qc mode"
    end subroutine scenario_qc_range_violation_hard_aborts

    !> Default (qc_soft=.false., hard): an unexpected Null (miss: not
    !> Null/NA) with qc active aborts the process, even when is_valid= was
    !> passed so the read itself would otherwise succeed.
    subroutine scenario_qc_null_violation_hard_aborts()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(4), id_back(4)
        logical :: is_valid_in(4), is_valid_out(4)

        id = [1, 2, 3, 4]
        is_valid_in = [.true., .false., .true., .true.]

        call parquet_open_writer(writer, "test_run/qc_null_hard.parquet")
        call parquet_write_column(writer, "id", id, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_null_hard.maml", [character(len=32) :: &
            "fields:", "- name: id", "  qc:", "    min: 0"])

        call parquet_open_reader(reader, "test_run/qc_null_hard.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_null_hard.maml"))
        call parquet_read_column(reader, "id", id_back, is_valid=is_valid_out)
        print '(a)', "unexpectedly read an unexpected Null without aborting in hard qc mode"
    end subroutine scenario_qc_null_violation_hard_aborts

    !> Same as scenario_qc_null_violation_warns, except this qc-maml field
    !> declares qc: miss: Null -- Nulls are expected here, so no WARNING
    !> should ever print (checked as an ABSENCE by the test, since this
    !> scenario's whole point is that nothing unusual happens).
    subroutine scenario_qc_miss_null_no_warning()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(4), id_back(4)
        logical :: is_valid_in(4), is_valid_out(4)

        id = [1, 2, 3, 4]
        is_valid_in = [.true., .false., .true., .true.]

        call parquet_open_writer(writer, "test_run/qc_miss_null.parquet")
        call parquet_write_column(writer, "id", id, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_miss_null.maml", [character(len=32) :: &
            "fields:", "- name: id", "  qc:", "    miss: Null"])

        call parquet_open_reader(reader, "test_run/qc_miss_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_miss_null.maml"))
        call parquet_read_column(reader, "id", id_back, is_valid=is_valid_out)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_miss_null_no_warning

    !> qc being active must never change the existing strict-by-default Null
    !> behavior: reading a column with a genuine Null and no null_value=/
    !> is_valid= still aborts with the same message as without any qc-maml
    !> at all (see scenario_read_column_with_nulls).
    subroutine scenario_qc_existing_null_abort_unchanged()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(4), id_back(4)
        logical :: is_valid_in(4)

        id = [1, 2, 3, 4]
        is_valid_in = [.true., .false., .true., .true.]

        call parquet_open_writer(writer, "test_run/qc_abort_unchanged.parquet")
        call parquet_write_column(writer, "id", id, is_valid=is_valid_in)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_abort_unchanged.maml", [character(len=32) :: &
            "fields:", "- name: id", "  qc:", "    miss: Null"])

        call parquet_open_reader(reader, "test_run/qc_abort_unchanged.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_abort_unchanged.maml"))
        call parquet_read_column(reader, "id", id_back) ! no null_value=/is_valid= -- still expected to abort
        print '(a)', "unexpectedly read a column with a genuine Null without error, even with qc active"
    end subroutine scenario_qc_existing_null_abort_unchanged

    !> A qc-maml is explicitly allowed to declare fields that don't exist in
    !> the actual parquet file -- parquet_open_reader must succeed cleanly,
    !> simply ignoring the unmatched field.
    subroutine scenario_qc_column_not_in_file()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(3)

        id = [1, 2, 3]

        call parquet_open_writer(writer, "test_run/qc_missing_col.parquet")
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_missing_col.maml", [character(len=32) :: &
            "fields:", "- name: does_not_exist", "  qc:", "    min: 0"])

        call parquet_open_reader(reader, "test_run/qc_missing_col.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_missing_col.maml"))
        call parquet_close_reader(reader)
    end subroutine scenario_qc_column_not_in_file

    !> qc=.false. always wins over a maml being present: no warning should
    !> print even for a column that would otherwise clearly violate its
    !> declared qc: bounds.
    subroutine scenario_qc_disabled_explicit_no_warning()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4), ra_back(4)

        ra = [10, 400, -5, 300]

        call parquet_open_writer(writer, "test_run/qc_disabled.parquet")
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call write_text_file("test_run/qc_disabled.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: 0", "    max: 360"])

        call parquet_open_reader(reader, "test_run/qc_disabled.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_disabled.maml"), qc=.false.)
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_close_reader(reader)
    end subroutine scenario_qc_disabled_explicit_no_warning

    !> An unrecognized qc: miss: value (anything other than Null/NA,
    !> case-insensitive, or empty) is rejected as invalid qc-maml syntax at
    !> parquet_open_reader time, before the parquet file is even touched.
    subroutine scenario_qc_maml_bad_miss_value()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_bad_miss.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    miss: garbage"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_bad_miss.maml"))
        print '(a)', "unexpectedly opened a reader with an unrecognized qc: miss: value"
    end subroutine scenario_qc_maml_bad_miss_value

    !> Two fields:  entries sharing the same name are ambiguous for qc
    !> purposes and rejected, the same as parquet_validate_maml_internal
    !> already rejects a duplicate field name for a schema-authoring maml.
    subroutine scenario_qc_maml_duplicate_field()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_dup_field.maml", [character(len=32) :: &
            "fields:", "- name: ra", "- name: ra"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_dup_field.maml"))
        print '(a)', "unexpectedly opened a reader with a duplicate qc-maml field name"
    end subroutine scenario_qc_maml_duplicate_field

    !> A qc-maml's qc: min: must be a lower bound (>= or >). A reversed
    !> '<'/'<=' operator is rejected when parquet_open_reader parses the
    !> qc-maml, the same rule parquet_validate_maml enforces on the write side.
    subroutine scenario_qc_maml_min_wrong_operator()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_min_wrong_op.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    min: '< 5'"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_min_wrong_op.maml"))
        print '(a)', "unexpectedly opened a reader with a reversed qc: min: operator"
    end subroutine scenario_qc_maml_min_wrong_operator

    !> maml%add_col_qc rejects a reversed min: operator ('<'/'<=' is an upper
    !> bound), the same rule the qc-maml parser and the write-side validator
    !> enforce.
    subroutine scenario_add_col_qc_min_reversed_operator()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, <5", col_name)
        print '(a)', "unexpectedly accepted a reversed qc min operator in add_col_qc"
    end subroutine scenario_add_col_qc_min_reversed_operator

    !> maml%add_col_qc rejects a bound that is only an operator with no value.
    subroutine scenario_add_col_qc_operator_without_value()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, >", col_name)
        print '(a)', "unexpectedly accepted an operator with no value in add_col_qc"
    end subroutine scenario_add_col_qc_operator_without_value

    !> maml%add_col_qc rejects a miss value other than Null/NA/empty.
    subroutine scenario_add_col_qc_bad_miss_value()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra,,, garbage", col_name)
        print '(a)', "unexpectedly accepted an invalid qc miss value in add_col_qc"
    end subroutine scenario_add_col_qc_bad_miss_value

    !> maml%add_col_qc rejects a qc_input with more than four comma-separated fields.
    subroutine scenario_add_col_qc_too_many_fields()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, 1, 2, Null, extra", col_name)
        print '(a)', "unexpectedly accepted more than four fields in add_col_qc"
    end subroutine scenario_add_col_qc_too_many_fields

    !> maml%add_col_qc rejects an empty first field (a leading comma / empty name).
    subroutine scenario_add_col_qc_empty_column_name()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc(", >0", col_name)
        print '(a)', "unexpectedly accepted an empty column name in add_col_qc"
    end subroutine scenario_add_col_qc_empty_column_name

    !> maml%add_col_qc rejects a column already declared earlier in the same maml.
    subroutine scenario_add_col_qc_duplicate_column()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        call maml%add_col_qc("ra, >0", col_name)
        call maml%add_col_qc("ra, <10", col_name)
        print '(a)', "unexpectedly accepted a duplicate column name in add_col_qc"
    end subroutine scenario_add_col_qc_duplicate_column

    !> The get_col_qc function form shares add_col_qc's worker, so it enforces
    !> the same validation -- e.g. a reversed min: operator aborts here too.
    subroutine scenario_get_col_qc_reversed_operator()
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: col_name

        col_name = maml%get_col_qc("ra, <5")
        print '(a)', "unexpectedly accepted a reversed qc min operator in get_col_qc"
    end subroutine scenario_get_col_qc_reversed_operator

    !> A fields: entry with no name: at all is rejected -- name is the one
    !> required attribute for a qc-maml field (everything else, including
    !> qc: itself, is optional).
    subroutine scenario_qc_maml_missing_name()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_missing_name.maml", [character(len=32) :: &
            "fields:", "- unit: cm"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_missing_name.maml"))
        print '(a)', "unexpectedly opened a reader with a qc-maml field missing 'name'"
    end subroutine scenario_qc_maml_missing_name

    !> parquet_parse_qc_maml reuses parquet_validate_maml_sections (the same
    !> section/sub-key schema every other maml validation path checks), so a
    !> typo'd qc: sub-key (here "minimum" instead of "min") is caught the
    !> same way it would be for a schema-authoring maml.
    subroutine scenario_qc_maml_unknown_subkey()
        type(parquet_reader) :: reader

        call write_text_file("test_run/qc_bad_subkey.maml", [character(len=32) :: &
            "fields:", "- name: ra", "  qc:", "    minimum: 5"])

        call parquet_open_reader(reader, "test/fixtures/has_null.parquet", &
            schema=parquet_load_qc_maml_file("test_run/qc_bad_subkey.maml"))
        print '(a)', "unexpectedly opened a reader with an unknown qc: sub-key"
    end subroutine scenario_qc_maml_unknown_subkey

    !> Opening a nonexistent file for reading previously called Arrow's
    !> ValueOrDie() with no status check first, which aborts the process
    !> directly (not a catchable C++ exception) with a generic message.
    !> create_parquet_reader (parquet_wrapper.cpp) now checks Arrow's status
    !> first and reports a clean, specific diagnostic before aborting.
    subroutine scenario_open_reader_missing_file()
        type(parquet_reader) :: reader

        call parquet_open_reader(reader, "test_run/does_not_exist_xyz_123.parquet")
        print '(a)', "unexpectedly opened a nonexistent file for reading without error"
    end subroutine scenario_open_reader_missing_file

    !> Same as scenario_open_reader_missing_file but for the write side: a
    !> path under a nonexistent directory can never be opened for writing.
    subroutine scenario_open_writer_bad_path()
        type(parquet_writer) :: writer

        call parquet_open_writer(writer, "test_run/no_such_directory_xyz/out.parquet")
        print '(a)', "unexpectedly opened a bad path for writing without error"
    end subroutine scenario_open_writer_bad_path

    !> parquet_write_string_column (scalar) checks a string's trimmed length
    !> against the schema's declared array_size and error stops if exceeded;
    !> parquet_write_string_matrix_column (this scenario) previously had no
    !> equivalent check, silently truncating an over-length string in a
    !> fixed-length string vector/matrix column instead of erroring.
    subroutine scenario_write_string_matrix_exceeds_array_size()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=20) :: values(2, 1)

        schema%maml%name = "string_matrix_array_size.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: string_matrix_table", &
            "fields:", &
            "- name: s", &
            "  data_type: string", &
            "  array_size: 5", &
            "  col_size: 2" ]

        call parquet_parse_maml(schema)

        values(1, 1) = "short"
        values(2, 1) = "this_is_way_too_long"

        call parquet_open_writer(writer, "test_run/error_scenario_string_matrix_array_size.parquet", schema)
        call parquet_write_column(writer, "s", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote an over-length string into a fixed-size string matrix column without error"
    end subroutine scenario_write_string_matrix_exceeds_array_size

    subroutine scenario_validate_protected_cols_unknown_name()
        type(parquet_maml_file) :: maml

        maml%name = "protected_unknown.maml"
        maml%lines = [character(len=40) :: &
            "table: protected_table", &
            "extra:", &
            "  protected_cols: not_a_real_column", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        ! "not_a_real_column" is not declared under fields: in this same
        ! MAML, so it must be rejected as a dangling reference.
        call parquet_validate_maml(maml)
    end subroutine scenario_validate_protected_cols_unknown_name

    subroutine scenario_write_protected_column_with_null()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        logical :: is_valid(3) = [.true., .false., .true.]

        schema%maml%name = "protected_write.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: protected_table", &
            "extra:", &
            "  protected_cols: a", &
            "fields:", &
            "- name: a", &
            "  data_type: int32" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_protected_write.parquet", schema)
        call parquet_write_column(writer, "a", values, is_valid=is_valid)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a Null into a protected column without error"
    end subroutine scenario_write_protected_column_with_null

    subroutine scenario_validate_qc_min_not_numeric()
        type(parquet_maml_file) :: maml

        maml%name = "qc_min_not_numeric.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: not_a_number" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_min_not_numeric

    subroutine scenario_validate_qc_min_non_integral_for_int32()
        type(parquet_maml_file) :: maml

        maml%name = "qc_min_non_integral.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: 1.5" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_min_non_integral_for_int32

    subroutine scenario_validate_qc_min_out_of_int32_range()
        type(parquet_maml_file) :: maml

        maml%name = "qc_min_out_of_range.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: 5000000000" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_min_out_of_int32_range

    !> qc: min: must be a lower bound: a '<'/'<=' operator on min: is a
    !> reversed, nonsensical bound and is rejected by parquet_validate_maml.
    subroutine scenario_validate_qc_min_wrong_operator()
        type(parquet_maml_file) :: maml

        maml%name = "qc_min_wrong_operator.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: '< 5'" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_min_wrong_operator

    !> qc: max: must be an upper bound: a '>'/'>=' operator on max: is a
    !> reversed, nonsensical bound and is rejected by parquet_validate_maml.
    subroutine scenario_validate_qc_max_wrong_operator()
        type(parquet_maml_file) :: maml

        maml%name = "qc_max_wrong_operator.maml"
        maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    max: '>= 5'" ]

        call parquet_validate_maml(maml)
    end subroutine scenario_validate_qc_max_wrong_operator

    !> Not an error scenario: qc=.true. only ever prints a WARNING and lets
    !> the write proceed. This scenario exits cleanly (exit 0); the
    !> corresponding test (test_writing.f90) captures stdout via a subprocess
    !> and checks for the WARNING text, since test-drive itself can't
    !> observe stdout produced by an in-process print statement reliably.
    subroutine scenario_qc_warning_numeric()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(5) = [1_int32, 5_int32, 1500_int32, -3_int32, 10_int32]

        schema%maml%name = "qc_warning.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: a", &
            "  data_type: int32", &
            "  qc:", &
            "    min: 1", &
            "    max: 1000" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_warning.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "a", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_warning_numeric

    subroutine scenario_qc_warning_string()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        character(len=8) :: values(3) = ["banana  ", "apple   ", "cherry  "]

        schema%maml%name = "qc_warning_string.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: s", &
            "  data_type: string", &
            "  array_size: 10", &
            "  qc:", &
            "    min: 'banana'" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_warning_string.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "s", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_warning_string

    !> qc: on a boolean field is accepted by validation but never enforced;
    !> this must write/close without error and without printing a WARNING.
    subroutine scenario_qc_silently_ignored_for_boolean()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        logical :: values(3) = [.true., .false., .true.]

        schema%maml%name = "qc_boolean.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_table", &
            "fields:", &
            "- name: b", &
            "  data_type: boolean", &
            "  qc:", &
            "    min: 0", &
            "    max: 0" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_qc_boolean.parquet", schema, qc=.true.)
        call parquet_write_column(writer, "b", values)
        call parquet_close_writer(writer)
    end subroutine scenario_qc_silently_ignored_for_boolean

    subroutine scenario_write_unknown_compression()
        type(parquet_writer) :: writer
        integer(int32) :: values(1) = [1_int32]

        call parquet_open_writer(writer, "test_run/error_scenario_unknown_compression.parquet", &
            compression="not_a_real_codec")
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly opened a writer with an unknown compression codec without error"
    end subroutine scenario_write_unknown_compression

    !> "v" is declared with col_size: 2 (a vector column), so a 1D values(:)
    !> array passed to parquet_write_column must have a length divisible by
    !> 2; length 3 is not, and used to hit a plain `stop` (exit code 0, no
    !> actual failure signaled) instead of `error stop` -- this scenario
    !> guards against that regressing.
    subroutine scenario_write_values_not_divisible_by_col_size()
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]

        schema%maml%name = "col_size_mismatch.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: col_size_mismatch_table", &
            "fields:", &
            "- name: v", &
            "  data_type: int32", &
            "  col_size: 2" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, "test_run/error_scenario_col_size_mismatch.parquet", schema)
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote a values(:) array whose length isn't divisible by col_size without error"
    end subroutine scenario_write_values_not_divisible_by_col_size

    !> n < 1 is not a valid thread pool capacity -- must error stop rather
    !> than silently passing an invalid value down to Arrow.
    subroutine scenario_set_max_threads_below_one()
        call parquet_set_max_threads(0)
        print '(a)', "unexpectedly accepted parquet_set_max_threads(0) without error"
    end subroutine scenario_set_max_threads_below_one

    !> A schema-enforced writer (cinfo given) already error stops on this via
    !> parquet_mark_column_written's write_counts tracking. A schema-less
    !> writer (no cinfo) previously had no such tracking at all: the C++ side
    !> only detects a duplicate name via column_metadata, which is only
    !> populated from cinfo -- so this used to silently write a file with two
    !> columns both named "id", and only fail much later, on read, with an
    !> uncaught "Column not found: id" exception (Arrow's GetFieldIndex
    !> returns -1 for an ambiguous/duplicate name). parquet_mark_column_written
    !> now tracks written names itself for the schema-less case too, so this
    !> is caught immediately, at the second parquet_write_column call.
    subroutine scenario_write_column_twice_no_schema()
        type(parquet_writer) :: writer

        call parquet_open_writer(writer, "test_run/write_column_twice_no_schema.parquet")
        call parquet_write_column(writer, "id", [1, 2, 3])
        call parquet_write_column(writer, "id", [10, 20, 30])
        call parquet_close_writer(writer)
        print '(a)', "unexpectedly wrote the same column twice on a schema-less writer without error"
    end subroutine scenario_write_column_twice_no_schema

    !> Deliberately violates the documented rule that each thread must use
    !> its own independent parquet_reader (see README's Thread safety
    !> section): every thread here calls parquet_read_column on the *same*
    !> shared reader instance. The reader's internal column cache has no
    !> synchronization, so this must be caught by the ConcurrencyGuard in
    !> parquet_wrapper.cpp rather than silently racing/corrupting memory.
    !> Uses !$omp parallel (not parallel do): every thread runs the *entire*
    !> loop itself, all hammering the same shared reader for many iterations.
    !> A work-shared "parallel do" split across threads turned out not to
    !> reliably overlap in practice (each thread only touching the reader a
    !> handful of times). Beyond that, a *single* barrier right at the start
    !> also turned out not to be reliable enough on its own: how simultaneous
    !> the very first round of calls actually is depends on details like
    !> compiler flags (e.g. -frecursive changes per-call overhead enough to
    !> visibly change contention odds) -- so this re-synchronizes every
    !> thread with a fresh barrier before *every* batch of calls, giving many
    !> repeated chances at genuine overlap throughout the run instead of
    !> just one, regardless of exactly how simultaneous any single batch is.
    subroutine scenario_concurrent_calls_into_shared_reader()
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: values(5) = [1_int32, 2_int32, 3_int32, 4_int32, 5_int32]
        integer(int32) :: read_back(5)
        integer, parameter :: batches = 200
        integer, parameter :: iterations_per_batch = 500
        integer :: b, i, nthreads

        ! Self-adapting: the race only exists under genuine multi-threading.
        ! Without an OpenMP flag (or with a single thread) the parallel region
        ! below runs serially, the guard cannot fire, and the 100k-iteration
        ! loop would be a pointless "unexpectedly finished" run. Skip cleanly.
        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) then
            print '(a)', "SKIPPED: OpenMP not active (omp_get_max_threads() <= 1); shared-reader race cannot occur"
            return
        end if

        call parquet_open_writer(writer, "test_run/error_scenario_shared_reader.parquet")
        call parquet_write_column(writer, "v", values)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/error_scenario_shared_reader.parquet")

        !$omp parallel default(shared) private(b, i, read_back)
        do b = 1, batches
            !$omp barrier
            do i = 1, iterations_per_batch
                call parquet_read_column(reader, "v", read_back)
            end do
        end do
        !$omp end parallel

        call parquet_close_reader(reader)
        print '(a)', "unexpectedly finished concurrent reads of a shared reader without the concurrency guard firing"
    end subroutine scenario_concurrent_calls_into_shared_reader

    !> Same idea as scenario_concurrent_calls_into_shared_reader (including
    !> the repeated-barrier-per-batch rationale above), but for the writer
    !> side: every thread calls parquet_write_column on the *same* shared
    !> writer instance, each writing its own column names (thread index +
    !> batch + iteration) so a successful call would never legitimately fail
    !> for an unrelated reason like a duplicate column name.
    subroutine scenario_concurrent_calls_into_shared_writer()
        type(parquet_writer) :: writer
        integer(int32) :: values(3) = [1_int32, 2_int32, 3_int32]
        integer, parameter :: batches = 50
        integer, parameter :: iterations_per_batch = 100
        integer :: b, i, tid, nthreads
        character(len=32) :: colname

        ! Self-adapting: see scenario_concurrent_calls_into_shared_reader.
        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) then
            print '(a)', "SKIPPED: OpenMP not active (omp_get_max_threads() <= 1); shared-writer race cannot occur"
            return
        end if

        call parquet_open_writer(writer, "test_run/error_scenario_shared_writer.parquet")

        !$omp parallel default(shared) private(b, i, tid, colname)
        tid = 0
        !$ tid = omp_get_thread_num()
        do b = 1, batches
            !$omp barrier
            do i = 1, iterations_per_batch
                write(colname, '(A,I0,A,I0,A,I0)') "col_", tid, "_", b, "_", i
                call parquet_write_column(writer, trim(colname), values)
            end do
        end do
        !$omp end parallel

        call parquet_close_writer(writer)
        print '(a)', "unexpectedly finished concurrent writes into a shared writer without the concurrency guard firing"
    end subroutine scenario_concurrent_calls_into_shared_writer

end program error_scenarios
