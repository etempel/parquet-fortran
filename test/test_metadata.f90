!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for parquet_get_metadata: reading back table-level key-value
!> metadata (written via add_metadata) from an opened parquet_reader. Cases
!> that must `error stop` (a missing key with no default, an unparsable
!> value with no default) run out-of-process via error_scenarios.f90, the
!> same convention test_errors.f90 uses -- see its module doc comment for why.
module test_metadata
    use parquet
    use parquet_maml_base
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    use test_errors, only : check_scenario_exit_status
    !
    implicit none
    private
    public :: collect_tests_parquet_metadata
    !
contains
    !
    subroutine collect_tests_parquet_metadata(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("round-trip scalar metadata of every type", test_scalar_round_trip), &
            new_unittest("round-trip array metadata of every type", test_array_round_trip), &
            new_unittest("string array with mixed element lengths round-trips exactly", &
                test_string_array_mixed_lengths), &
            new_unittest("reserved/internal metadata key is readable", test_reserved_key_readable), &
            new_unittest("missing key returns default (warn defaults to .true.)", test_missing_key_default_warns), &
            new_unittest("missing key with warn=.false. still returns default", test_missing_key_default_no_warn), &
            new_unittest("unparsable value falls back to default", test_conversion_failure_default), &
            new_unittest("int64-range value requested as int32 overflows and falls back to default", &
                test_int32_overflow_default), &
            new_unittest("unparsable scalar value falls back to default (int64/float32/float64/logical)", &
                test_scalar_conversion_failure_default_all_types), &
            new_unittest("missing scalar key returns default (int64/float32/float64/logical/string)", &
                test_scalar_missing_key_default_all_types), &
            new_unittest("array element conversion failure falls back to the whole default array", &
                test_array_conversion_failure_default), &
            new_unittest("unparsable array falls back to default (int64/float32/float64/logical)", &
                test_array_conversion_failure_default_all_types), &
            new_unittest("missing array key returns default (int64/float32/float64/logical/string)", &
                test_array_missing_key_default_all_types), &
            new_unittest("missing key with no default aborts", test_missing_key_no_default_aborts), &
            new_unittest("unparsable value with no default aborts", test_conversion_failure_no_default_aborts), &
            new_unittest("missing int64 key with no default aborts", test_missing_int64_no_default_aborts), &
            new_unittest("missing float32 key with no default aborts", test_missing_float32_no_default_aborts), &
            new_unittest("missing float64 key with no default aborts", test_missing_float64_no_default_aborts), &
            new_unittest("missing logical key with no default aborts", test_missing_logical_no_default_aborts), &
            new_unittest("missing string key with no default aborts", test_missing_string_no_default_aborts), &
            new_unittest("unparsable int64 value with no default aborts", test_conversion_int64_no_default_aborts), &
            new_unittest("unparsable float32 value with no default aborts", &
                test_conversion_float32_no_default_aborts), &
            new_unittest("unparsable float64 value with no default aborts", &
                test_conversion_float64_no_default_aborts), &
            new_unittest("unparsable logical value with no default aborts", &
                test_conversion_logical_no_default_aborts), &
            new_unittest("missing int32 array key with no default aborts", test_missing_int32_array_no_default_aborts), &
            new_unittest("unparsable int32 array value with no default aborts", &
                test_conversion_int32_array_no_default_aborts), &
            new_unittest("missing int64 array key with no default aborts", test_missing_int64_array_no_default_aborts), &
            new_unittest("unparsable int64 array value with no default aborts", &
                test_conversion_int64_array_no_default_aborts), &
            new_unittest("missing float32 array key with no default aborts", &
                test_missing_float32_array_no_default_aborts), &
            new_unittest("unparsable float32 array value with no default aborts", &
                test_conversion_float32_array_no_default_aborts), &
            new_unittest("missing float64 array key with no default aborts", &
                test_missing_float64_array_no_default_aborts), &
            new_unittest("unparsable float64 array value with no default aborts", &
                test_conversion_float64_array_no_default_aborts), &
            new_unittest("missing logical array key with no default aborts", &
                test_missing_logical_array_no_default_aborts), &
            new_unittest("unparsable logical array value with no default aborts", &
                test_conversion_logical_array_no_default_aborts), &
            new_unittest("missing string array key with no default aborts", &
                test_missing_string_array_no_default_aborts), &
            new_unittest("add_metadata with an empty key is a silent no-op", test_add_metadata_empty_key_noop), &
            new_unittest("VOTable XML sidecar escapes &, <, "", and ' in unit/description/ucd", &
                test_votable_xml_escapes_special_chars), &
            new_unittest("print_schema_info: header/dash/field rows are aligned to computed widths", &
                test_print_schema_info_alignment), &
            new_unittest("print_schema_info: prefix, all three dash lines, and is_set filtering", &
                test_print_schema_info_prefix_dashes_and_is_set), &
            new_unittest("print_schema_info: repeated calls append to the same open unit", &
                test_print_schema_info_multiple_schemas_same_unit), &
            new_unittest("print_schema_info: filename= opens/appends/closes across repeated calls", &
                test_print_schema_info_filename_append), &
            new_unittest("print_schema_info: no enabled columns and header=.false. writes nothing", &
                test_print_schema_info_no_cols_no_header), &
            new_unittest("print_schema_info: table_name line, default position, prefix, and suppression", &
                test_print_schema_info_table_name_line), &
            new_unittest("print_schema_info: neither unit nor filename given aborts", &
                test_print_schema_info_no_unit_no_filename_aborts), &
            new_unittest("print_schema_info: unit not already open aborts", &
                test_print_schema_info_unit_not_open_aborts), &
            new_unittest("print_schema_info: unit open for reading only aborts", &
                test_print_schema_info_unit_read_only_aborts), &
            new_unittest("print_schema_info: unit/filename mismatch aborts", &
                test_print_schema_info_unit_filename_mismatch_aborts), &
            new_unittest("print_schema_info: uninitialized schema aborts by default", &
                test_print_schema_info_uninitialized_schema_aborts), &
            new_unittest("print_schema_info: filename that cannot be opened for writing aborts", &
                test_print_schema_info_open_failure_aborts), &
            new_unittest("print_schema_info: allow_uninitialized=.true. is a complete no-op", &
                test_print_schema_info_allow_uninitialized_is_noop), &
            new_unittest("add_metadata before the schema has been parsed aborts", &
                test_add_metadata_before_parse_aborts), &
            new_unittest("clear_metadata keeps base (parsed) entries, discards user-added ones", &
                test_clear_metadata_keeps_base_entries), &
            new_unittest("clear_metadata with no user-added entries is a no-op", &
                test_clear_metadata_noop_when_no_user_entries), &
            new_unittest("clear_metadata works the same for an in-code-built schema", &
                test_clear_metadata_in_code_schema), &
            new_unittest("clear_metadata on a never-parsed table_metadata discards all items", &
                test_clear_metadata_never_parsed), &
            new_unittest("add_metadata duplicating a declared MAML top-level key still appends (category A)", &
                test_add_metadata_duplicate_maml_init_key_still_appends), &
            new_unittest("add_metadata duplicating a non-reserved key still appends (category B)", &
                test_add_metadata_duplicate_generic_key_still_appends), &
            new_unittest("add_metadata('DATE', ...) is shadowed by the writer's own DATE entry on read " // &
                "(category C)", test_add_metadata_duplicate_writer_key_shadowed_on_read), &
            new_unittest("add_metadata(warn=.false.) suppresses the warning but still appends the duplicate", &
                test_add_metadata_warn_false_still_appends) &
            ]
    end subroutine collect_tests_parquet_metadata

    !> Builds a schema (schemas/maml_example.maml, "id0" only) carrying one
    !> metadata entry of every add_metadata scalar/array type, writes
    !> `out_file`, and returns a freshly opened reader on it -- shared by
    !> every round-trip/default-fallback test below. Each caller passes its
    !> own uniquely-named out_file since test-drive runs tests within a
    !> suite concurrently by default.
    subroutine write_metadata_fixture(out_file, reader)
        character(len=*), intent(in) :: out_file
        type(parquet_reader), intent(out) :: reader
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        call schema%add_metadata("meta_i32", 42_int32)
        call schema%add_metadata("meta_i64", 123456789012_int64)
        call schema%add_metadata("meta_f32", 3.5_real32)
        call schema%add_metadata("meta_f64", 3.14159265358979_real64)
        call schema%add_metadata("meta_bool", .true.)
        call schema%add_metadata("meta_bool_false", .false.)
        call schema%add_metadata("meta_str", "hello world")
        call schema%add_metadata("meta_not_a_number", "not_a_number")
        call schema%add_metadata("meta_i32_arr", [1_int32, 2_int32, 3_int32])
        call schema%add_metadata("meta_i64_arr", [10_int64, 20_int64, 30_int64])
        call schema%add_metadata("meta_f32_arr", [1.5_real32, 2.5_real32], fmt='F0.2')
        call schema%add_metadata("meta_f64_arr", [1.25_real64, 2.25_real64])
        call schema%add_metadata("meta_bool_arr", [.true., .false., .true.])

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
    end subroutine write_metadata_fixture

    subroutine test_scalar_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32) :: i32
        integer(int64) :: i64
        real(real32) :: f32
        real(real64) :: f64
        logical :: lg
        character(len=:), allocatable :: sval

        call write_metadata_fixture("test_run/metadata_scalar.parquet", reader)

        call parquet_get_metadata(reader, "meta_i32", i32)
        call check(error, i32 == 42_int32, &
            "i32 == 42_int32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int32 metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_i64", i64)
        call check(error, i64 == 123456789012_int64, &
            "i64 == 123456789012_int64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int64 metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_f32", f32)
        call check(error, abs(f32 - 3.5_real32) < 1.0e-5_real32, &
            "abs(f32 - 3.5_real32) < 1.0e-5_real32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float32 metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_f64", f64)
        call check(error, abs(f64 - 3.14159265358979_real64) < 1.0e-10_real64, &
            "abs(f64 - 3.14159265358979_real64) < 1.0e-10_real64")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float64 metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_bool", lg)
        call check(error, lg .eqv. .true., &
            "lg .eqv. .true.")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "logical metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_bool_false", lg)
        call check(error, lg .eqv. .false., &
            "lg .eqv. .false.")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "logical .false. metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_str", sval)
        call check(error, trim(sval) == "hello world", &
            "trim(sval) == ""hello world""")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "string metadata did not round-trip")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_scalar_round_trip

    subroutine test_array_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32), allocatable :: i32arr(:)
        integer(int64), allocatable :: i64arr(:)
        real(real32), allocatable :: f32arr(:)
        real(real64), allocatable :: f64arr(:)
        logical, allocatable :: lgarr(:)

        call write_metadata_fixture("test_run/metadata_array.parquet", reader)

        call parquet_get_metadata(reader, "meta_i32_arr", i32arr)
        call check(error, size(i32arr) == 3, &
            "size(i32arr) == 3")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int32 array metadata has wrong size")
            return
        end if
        call check(error, all(i32arr == [1_int32, 2_int32, 3_int32]), &
            "all(i32arr == [1_int32, 2_int32, 3_int32])")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int32 array metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_i64_arr", i64arr)
        call check(error, all(i64arr == [10_int64, 20_int64, 30_int64]), &
            "all(i64arr == [10_int64, 20_int64, 30_int64])")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int64 array metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_f32_arr", f32arr)
        call check(error, size(f32arr) == 2, &
            "size(f32arr) == 2")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float32 array metadata has wrong size")
            return
        end if
        call check(error, all(abs(f32arr - [1.5_real32, 2.5_real32]) < 1.0e-5_real32), &
            "all(abs(f32arr - [1.5_real32, 2.5_real32]) < 1.0e-5_real32)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float32 array metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_f64_arr", f64arr)
        call check(error, all(abs(f64arr - [1.25_real64, 2.25_real64]) < 1.0e-10_real64), &
            "all(abs(f64arr - [1.25_real64, 2.25_real64]) < 1.0e-10_real64)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float64 array metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_bool_arr", lgarr)
        call check(error, size(lgarr) == 3, &
            "size(lgarr) == 3")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "logical array metadata has wrong size")
            return
        end if
        call check(error, lgarr(1) .eqv. .true. .and. lgarr(2) .eqv. .false. .and. lgarr(3) .eqv. .true., &
            "lgarr(1) .eqv. .true. .and. lgarr(2) .eqv. .false. .and. lgarr(3) .eqv. .true.")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "logical array metadata did not round-trip")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_array_round_trip

    !> Regression test: a string array whose elements have different lengths
    !> (here: a 2-character element alongside two longer ones) must
    !> round-trip exactly -- neither the short element wrongly padded with
    !> its neighbor's characters, nor a longer element truncated down to the
    !> shortest element's length. This is the same class of fixed-length
    !> character array pitfall documented on parquet_prefetch_columns_string
    !> elsewhere in this codebase, now checked for the metadata array
    !> split/parse path (parquet_metadata_split_array in parquet_metadata.f90).
    subroutine test_string_array_mixed_lengths(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_schema) :: schema
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        character(len=:), allocatable :: sarr(:)
        character(len=*), parameter :: out_file = "test_run/metadata_string_array_mixed.parquet"

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        call schema%add_metadata("mixed_tags", [character(len=14) :: "ab", "category_long", "final_tag_here"])

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_metadata(reader, "mixed_tags", sarr)

        call check(error, size(sarr) == 3, &
            "size(sarr) == 3")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "mixed-length string array metadata has wrong size")
            return
        end if

        call check(error, trim(sarr(1)) == "ab", &
            "trim(sarr(1)) == ""ab""")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, &
                "short (2-character) element of a mixed-length string array metadata did not round-trip")
            return
        end if

        call check(error, trim(sarr(2)) == "category_long", &
            "trim(sarr(2)) == ""category_long""")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "middle element of a mixed-length string array metadata did not round-trip")
            return
        end if

        call check(error, trim(sarr(3)) == "final_tag_here", &
            "trim(sarr(3)) == ""final_tag_here""")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, &
                "longest element of a mixed-length string array metadata was truncated on round-trip")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_string_array_mixed_lengths

    subroutine test_reserved_key_readable(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        character(len=:), allocatable :: sval

        call write_metadata_fixture("test_run/metadata_reserved.parquet", reader)

        ! "DATE" is one of the writer's own reserved keys (build_file_metadata
        ! in parquet_wrapper.cpp) -- parquet_get_metadata makes no distinction
        ! between reserved/internal keys and ones added via add_metadata.
        call parquet_get_metadata(reader, "DATE", sval)
        call check(error, len_trim(sval) > 0, &
            "len_trim(sval) > 0")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "reserved metadata key 'DATE' was not readable")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_reserved_key_readable

    !> build_votable_xml/xml_escape (parquet_wrapper.cpp) escapes 5 reserved XML characters
    !> (& < > " '), and no existing test's unit/description/ucd strings ever contained any of
    !> them (not even "&", despite this test's own doc-comment once assuming otherwise --
    !> verified empirically via tools/coverage_cpp.sh, not just inferred). The VOTable
    !> XML itself isn't exposed by any Fortran API directly; it's readable back as an ordinary
    !> string metadata value under the reserved key "IVOA.VOTable-Parquet.content" (see
    !> build_file_metadata's own comment), the same way test_reserved_key_readable reads "DATE".
    subroutine test_votable_xml_escapes_special_chars(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id0(1) = [1_int32]
        character(len=:), allocatable :: xml
        character(len=*), parameter :: out_file = "test_run/metadata_votable_escape.parquet"

        call schema%init(table="votable_escape_table")
        call schema%add_field("id0", "int32", unit="a<b", info="say ""hi"" & bye", ucd="it's_a_ucd")
        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_metadata(reader, "IVOA.VOTable-Parquet.content", xml)
        call parquet_close_reader(reader)

        call check(error, index(xml, "a&lt;b") > 0 .and. index(xml, "say &quot;hi&quot; &amp; bye") > 0 .and. &
            index(xml, "it&apos;s_a_ucd") > 0, &
            "VOTable XML sidecar did not escape &, <, "", and ' in unit/description/ucd")
    end subroutine test_votable_xml_escapes_special_chars

    subroutine test_missing_key_default_warns(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32) :: i32

        call write_metadata_fixture("test_run/metadata_missing_default.parquet", reader)

        call parquet_get_metadata(reader, "does_not_exist", i32, default=99_int32)
        call check(error, i32 == 99_int32, &
            "i32 == 99_int32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "missing key with a default did not return the default value")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_missing_key_default_warns

    subroutine test_missing_key_default_no_warn(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32) :: i32

        call write_metadata_fixture("test_run/metadata_missing_no_warn.parquet", reader)

        call parquet_get_metadata(reader, "does_not_exist", i32, default=7_int32, warn=.false.)
        call check(error, i32 == 7_int32, &
            "i32 == 7_int32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "missing key with warn=.false. did not return the default value")
            return
        end if

        call parquet_close_reader(reader)

        ! test/fixtures/has_null.parquet is written by generate_fixtures.cpp
        ! with no key-value metadata at all, so the reader never allocates its
        ! in-memory metadata table -- reading any key must still cleanly fall
        ! back to the default (exercises the "no metadata items" early return
        ! in parquet_metadata_find_index that library-written files, which
        ! always carry reserved metadata, can't reach).
        call parquet_open_reader(reader, "test/fixtures/has_null.parquet")
        call parquet_get_metadata(reader, "does_not_exist", i32, default=13_int32, warn=.false.)
        call parquet_close_reader(reader)

        call check(error, i32 == 13_int32, &
            "i32 == 13_int32")
        if (allocated(error)) then
            call test_failed(error, "missing key on a file with no metadata did not return the default value")
            return
        end if
    end subroutine test_missing_key_default_no_warn

    subroutine test_conversion_failure_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32) :: i32

        call write_metadata_fixture("test_run/metadata_conversion_failure.parquet", reader)

        call parquet_get_metadata(reader, "meta_not_a_number", i32, default=-1_int32)
        call check(error, i32 == -1_int32, &
            "i32 == -1_int32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "unparsable metadata value with a default did not fall back to it")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_conversion_failure_default

    subroutine test_int32_overflow_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32) :: i32

        call write_metadata_fixture("test_run/metadata_overflow.parquet", reader)

        ! meta_i64 (123456789012) is well outside int32 range: reading it as
        ! int32 must be treated as a conversion failure (falls back to
        ! default), never silently wrapped/truncated into some other int32.
        call parquet_get_metadata(reader, "meta_i64", i32, default=-2_int32)
        call check(error, i32 == -2_int32, &
            "i32 == -2_int32")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int64-range metadata value requested as int32 was not caught as an overflow")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_int32_overflow_default

    !> The int32 scalar conversion-failure-with-default path is covered above
    !> (test_conversion_failure_default); this exercises the same
    !> warn-then-fall-back-to-default branch in each of the other scalar
    !> variants (int64/float32/float64/logical), since they are near-identical
    !> copies and a per-type copy-paste slip (wrong parse fn / default handling)
    !> would otherwise go uncaught. string has no conversion-failure branch (any
    !> stored text is a valid string), so it is not included here.
    subroutine test_scalar_conversion_failure_default_all_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int64) :: i64
        real(real32) :: f32
        real(real64) :: f64
        logical :: lg

        call write_metadata_fixture("test_run/metadata_scalar_conv_fail_all.parquet", reader)

        ! meta_not_a_number holds "not_a_number", unparsable as any numeric or
        ! logical type -- every variant must warn and return the given default.
        call parquet_get_metadata(reader, "meta_not_a_number", i64, default=-21_int64)
        call parquet_get_metadata(reader, "meta_not_a_number", f32, default=-2.5_real32)
        call parquet_get_metadata(reader, "meta_not_a_number", f64, default=-3.5_real64)
        call parquet_get_metadata(reader, "meta_not_a_number", lg, default=.true.)
        call parquet_close_reader(reader)

        call check(error, i64 == -21_int64 .and. abs(f32 + 2.5_real32) < 1.0e-5_real32 .and. abs(f64 + 3.5_real64) < &
            1.0e-10_real64 .and. (lg .eqv. .true.), &
            "i64 == -21_int64 .and. abs(f32 + 2.5_real32) < 1.0e-5_real32 .and. abs(f64 + 3.5_real64) < 1.0e-10_real64 .and. (lg .")
        if (allocated(error)) then
            call test_failed(error, &
                "an unparsable scalar value with a default did not fall back to it for one or more types")
            return
        end if
    end subroutine test_scalar_conversion_failure_default_all_types

    !> Mirrors test_missing_key_default_warns/no_warn (int32) across the other
    !> scalar variants: a missing key with a default returns the default (and,
    !> with warn=.true. by default, prints the "using default" WARNING); a
    !> second read per type passes warn=.false. to also exercise each variant's
    !> present(warn) branch and confirm it suppresses the warning path.
    subroutine test_scalar_missing_key_default_all_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int64) :: i64, i64b
        real(real32) :: f32, f32b
        real(real64) :: f64, f64b
        logical :: lg, lgb
        character(len=:), allocatable :: sval, svalb

        call write_metadata_fixture("test_run/metadata_scalar_missing_all.parquet", reader)

        call parquet_get_metadata(reader, "does_not_exist", i64, default=-11_int64)
        call parquet_get_metadata(reader, "does_not_exist", i64b, default=-12_int64, warn=.false.)
        call parquet_get_metadata(reader, "does_not_exist", f32, default=-1.5_real32)
        call parquet_get_metadata(reader, "does_not_exist", f32b, default=-1.6_real32, warn=.false.)
        call parquet_get_metadata(reader, "does_not_exist", f64, default=-2.5_real64)
        call parquet_get_metadata(reader, "does_not_exist", f64b, default=-2.6_real64, warn=.false.)
        call parquet_get_metadata(reader, "does_not_exist", lg, default=.true.)
        call parquet_get_metadata(reader, "does_not_exist", lgb, default=.true., warn=.false.)
        call parquet_get_metadata(reader, "does_not_exist", sval, default="fallback")
        call parquet_get_metadata(reader, "does_not_exist", svalb, default="fallback2", warn=.false.)
        call parquet_close_reader(reader)

        call check(error, i64 == -11_int64 .and. i64b == -12_int64 .and. abs(f32 + 1.5_real32) < 1.0e-5_real32 .and. abs(f32b + &
            1.6_real32) < 1.0e-5_real32 .and. abs(f64 + 2.5_real64) < 1.0e-10_real64 .and. abs(f64b + 2.6_real64) < 1.0e-10_real64 &
            .and. (lg .eqv. .true.) .and. (lgb .eqv. .true.) .and. trim(sval) == "fallback" .and. trim(svalb) == "fallback2", &
            "i64 == -11_int64 .and. i64b == -12_int64 .and. abs(f32 + 1.5_real32) < 1.0e-5_real32 .and. abs(f32b + 1.6_real32) < 1")
        if (allocated(error)) then
            call test_failed(error, &
                "a missing scalar key with a default did not return the default for one or more types")
            return
        end if
    end subroutine test_scalar_missing_key_default_all_types

    subroutine test_array_conversion_failure_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_schema) :: schema
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        integer(int32), allocatable :: i32arr(:)
        integer(int32), parameter :: fallback(2) = [-9_int32, -8_int32]
        character(len=*), parameter :: out_file = "test_run/metadata_array_conversion_failure.parquet"

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        ! A string array stored via add_metadata_string_array, read back as
        ! an int32 array: the second element ("bad") cannot be parsed -- the
        ! whole array must fall back to `default`, not a partially-parsed one.
        call schema%add_metadata("bad_int_arr", ["1  ", "bad", "3  "])

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_metadata(reader, "bad_int_arr", i32arr, default=fallback)
        call check(error, all(i32arr == fallback), &
            "all(i32arr == fallback)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, &
                "int32 array metadata with an unparsable element did not fall back to the default array")
            return
        end if

        call parquet_close_reader(reader)
    end subroutine test_array_conversion_failure_default

    !> Same as test_array_conversion_failure_default (int32 array) but for the
    !> other array variants: a stored array with an unparsable element must
    !> warn and fall back to the whole default array, never a partially-parsed
    !> one. int64/float32/float64 reuse a bad numeric array; logical needs a
    !> non-true/false token, so it gets its own bad logical array.
    subroutine test_array_conversion_failure_default_all_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_schema) :: schema
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        integer(int64), allocatable :: i64arr(:)
        real(real32), allocatable :: f32arr(:)
        real(real64), allocatable :: f64arr(:)
        logical, allocatable :: lgarr(:)
        integer(int64), parameter :: i64_fb(2) = [-9_int64, -8_int64]
        real(real32), parameter :: f32_fb(2) = [-9.5_real32, -8.5_real32]
        real(real64), parameter :: f64_fb(2) = [-9.25_real64, -8.25_real64]
        logical, parameter :: lg_fb(2) = [.false., .true.]
        character(len=*), parameter :: out_file = "test_run/metadata_array_conv_fail_all.parquet"

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        ! "bad" is unparsable as any numeric type; "x" is unparsable as logical
        ! (which accepts only true/false) -- each read must reject the whole
        ! array and return the default.
        call schema%add_metadata("bad_num_arr", ["1  ", "bad", "3  "])
        call schema%add_metadata("bad_bool_arr", ["true ", "x    ", "false"])

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_metadata(reader, "bad_num_arr", i64arr, default=i64_fb)
        call parquet_get_metadata(reader, "bad_num_arr", f32arr, default=f32_fb)
        call parquet_get_metadata(reader, "bad_num_arr", f64arr, default=f64_fb)
        call parquet_get_metadata(reader, "bad_bool_arr", lgarr, default=lg_fb)
        call parquet_close_reader(reader)

        call check(error, all(i64arr == i64_fb) .and. all(abs(f32arr - f32_fb) < 1.0e-5_real32) .and. all(abs(f64arr - f64_fb) < &
            1.0e-10_real64) .and. all(lgarr .eqv. lg_fb), &
            "all(i64arr == i64_fb) .and. all(abs(f32arr - f32_fb) < 1.0e-5_real32) .and. all(abs(f64arr - f64_fb) < 1.0e-10_real64")
        if (allocated(error)) then
            call test_failed(error, &
                "an array with an unparsable element did not fall back to the default array for one or more types")
            return
        end if
    end subroutine test_array_conversion_failure_default_all_types

    !> Missing-key-with-default across every array variant (int64/float32/
    !> float64/logical/string): each returns the default array. int32 array's
    !> equivalent branch; the other variants are near-identical copies, so this
    !> guards against a per-type slip. int32 is included here too (its
    !> missing-with-default branch is otherwise only reached on the abort path).
    subroutine test_array_missing_key_default_all_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int32), allocatable :: i32arr(:)
        integer(int64), allocatable :: i64arr(:)
        real(real32), allocatable :: f32arr(:)
        real(real64), allocatable :: f64arr(:)
        logical, allocatable :: lgarr(:)
        character(len=:), allocatable :: sarr(:)
        integer(int32), parameter :: i32_fb(2) = [-3_int32, -4_int32]
        integer(int64), parameter :: i64_fb(2) = [-1_int64, -2_int64]
        real(real32), parameter :: f32_fb(2) = [-1.5_real32, -2.5_real32]
        real(real64), parameter :: f64_fb(2) = [-1.25_real64, -2.25_real64]
        logical, parameter :: lg_fb(2) = [.true., .false.]

        call write_metadata_fixture("test_run/metadata_array_missing_all.parquet", reader)

        call parquet_get_metadata(reader, "no_such_array", i32arr, default=i32_fb)
        call parquet_get_metadata(reader, "no_such_array", i64arr, default=i64_fb)
        call parquet_get_metadata(reader, "no_such_array", f32arr, default=f32_fb)
        call parquet_get_metadata(reader, "no_such_array", f64arr, default=f64_fb)
        call parquet_get_metadata(reader, "no_such_array", lgarr, default=lg_fb)
        call parquet_get_metadata(reader, "no_such_array", sarr, default=["aa", "bb"])
        call parquet_close_reader(reader)

        call check(error, all(i32arr == i32_fb) .and. all(i64arr == i64_fb) .and. all(abs(f32arr - f32_fb) < 1.0e-5_real32) .and. &
            all(abs(f64arr - f64_fb) < 1.0e-10_real64) .and. all(lgarr .eqv. lg_fb) .and. size(sarr) == 2 .and. trim(sarr(1)) == &
            "aa" .and. trim(sarr(2)) == "bb", &
            "all(i32arr == i32_fb) .and. all(i64arr == i64_fb) .and. all(abs(f32arr - f32_fb) < 1.0e-5_real32) .and. all(abs(f64ar")
        if (allocated(error)) then
            call test_failed(error, &
                "a missing array key with a default did not return the default array for one or more types")
            return
        end if
    end subroutine test_array_missing_key_default_all_types

    subroutine test_missing_key_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_key_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing metadata key with no default was expected to abort")
    end subroutine test_missing_key_no_default_aborts

    subroutine test_conversion_failure_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_failure_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable metadata value with no default was expected to abort")
    end subroutine test_conversion_failure_no_default_aborts

    ! The two int32 abort scenarios above cover the shared stop_missing/
    ! stop_conversion helpers; these per-type variants additionally exercise
    ! each scalar variant's own call site into them (a distinct line per type),
    ! mirroring how the int32 scalar variant is covered. Array variants are not
    ! given abort scenarios here -- matching the int32 array variant, which has
    ! only an in-process default-fallback test and no abort scenario.

    subroutine test_missing_int64_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_int64_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing int64 metadata key with no default was expected to abort")
    end subroutine test_missing_int64_no_default_aborts

    subroutine test_missing_float32_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_float32_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing float32 metadata key with no default was expected to abort")
    end subroutine test_missing_float32_no_default_aborts

    subroutine test_missing_float64_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_float64_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing float64 metadata key with no default was expected to abort")
    end subroutine test_missing_float64_no_default_aborts

    subroutine test_missing_logical_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_logical_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing logical metadata key with no default was expected to abort")
    end subroutine test_missing_logical_no_default_aborts

    subroutine test_missing_string_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_string_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing string metadata key with no default was expected to abort")
    end subroutine test_missing_string_no_default_aborts

    subroutine test_conversion_int64_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_int64_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable int64 metadata value with no default was expected to abort")
    end subroutine test_conversion_int64_no_default_aborts

    subroutine test_conversion_float32_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_float32_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable float32 metadata value with no default was expected to abort")
    end subroutine test_conversion_float32_no_default_aborts

    subroutine test_conversion_float64_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_float64_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable float64 metadata value with no default was expected to abort")
    end subroutine test_conversion_float64_no_default_aborts

    subroutine test_conversion_logical_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_logical_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable logical metadata value with no default was expected to abort")
    end subroutine test_conversion_logical_no_default_aborts

    !> Array-typed counterparts of the no-default abort tests above: every
    !> scalar getter's missing-key/conversion-failure abort was already
    !> tested, but the *_array getters (which call the same
    !> parquet_metadata_stop_missing/parquet_metadata_stop_conversion helpers)
    !> never were.

    subroutine test_missing_int32_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_int32_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing int32 array metadata key with no default was expected to abort")
    end subroutine test_missing_int32_array_no_default_aborts

    subroutine test_conversion_int32_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_int32_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable int32 array metadata value with no default was expected to abort")
    end subroutine test_conversion_int32_array_no_default_aborts

    subroutine test_missing_int64_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_int64_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing int64 array metadata key with no default was expected to abort")
    end subroutine test_missing_int64_array_no_default_aborts

    subroutine test_conversion_int64_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_int64_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable int64 array metadata value with no default was expected to abort")
    end subroutine test_conversion_int64_array_no_default_aborts

    subroutine test_missing_float32_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_float32_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing float32 array metadata key with no default was expected to abort")
    end subroutine test_missing_float32_array_no_default_aborts

    subroutine test_conversion_float32_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_float32_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable float32 array metadata value with no default was expected to abort")
    end subroutine test_conversion_float32_array_no_default_aborts

    subroutine test_missing_float64_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_float64_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing float64 array metadata key with no default was expected to abort")
    end subroutine test_missing_float64_array_no_default_aborts

    subroutine test_conversion_float64_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_float64_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable float64 array metadata value with no default was expected to abort")
    end subroutine test_conversion_float64_array_no_default_aborts

    subroutine test_missing_logical_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_logical_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing logical array metadata key with no default was expected to abort")
    end subroutine test_missing_logical_array_no_default_aborts

    subroutine test_conversion_logical_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_logical_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable logical array metadata value with no default was expected to abort")
    end subroutine test_conversion_logical_array_no_default_aborts

    subroutine test_missing_string_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_string_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing string array metadata key with no default was expected to abort")
    end subroutine test_missing_string_array_no_default_aborts

    !> add_metadata("", ...) with an empty key must be a silent no-op (no
    !> metadata entry appended), not an error -- see
    !> parquet_metadata_append_entry's own guard in parquet_metadata_base.f90.
    subroutine test_add_metadata_empty_key_noop(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema

        call schema%init(table="empty_key_test")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)
        call schema%add_metadata("", 1_int32)

        call check(error, size(schema%metadata%items) == schema%metadata%n_base_items, &
            "add_metadata with an empty key should not append any metadata entry")
    end subroutine test_add_metadata_empty_key_noop

    !> Reads every line of `filename` into `lines(1:n)`; n is the number of lines actually read
    !! (0 if the file doesn't exist or is empty). `lines` must be pre-allocated large enough by
    !! the caller -- these fixture files are always small.
    subroutine read_text_lines(filename, lines, n)
        character(len=*), intent(in) :: filename
        character(len=*), intent(inout) :: lines(:)
        integer, intent(out) :: n
        integer :: unit, ios

        n = 0
        open(newunit=unit, file=filename, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            if (n + 1 > size(lines)) exit
            read(unit, '(a)', iostat=ios) lines(n + 1)
            if (ios /= 0) exit
            n = n + 1
        end do
        close(unit)
    end subroutine read_text_lines

    !> A hand-verified fixture (3 fields with deliberately different name/unit/type/col_size/
    !! ucd/info lengths, including a field with an empty unit/ucd/info) whose exact expected
    !! output (widths, padding, header labels) was computed independently offline -- this test
    !! is the one place print_schema_info's column-alignment arithmetic itself is pinned down;
    !! the other print_schema_info tests below check structural/behavioral properties instead.
    subroutine test_print_schema_info_alignment(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: unit, n
        character(len=*), parameter :: out_file = "test_run/print_schema_info_alignment.txt"
        character(len=128) :: lines(10)

        call schema%init(table="align_test")
        call schema%add_field("id", "int32", info="ID field")
        call schema%add_field("ra_deg", "float64", unit="deg", ucd="pos.eq.ra", info="Right ascension")
        call schema%add_field("flags", "boolean", col_size=6)
        call parquet_parse_maml(schema)

        open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")
        call schema%print_schema_info(unit=unit, table_name=.false.)
        close(unit)

        call read_text_lines(out_file, lines, n)
        call check(error, n == 5, "expected 5 output lines (header + dash-after-header + 3 field rows)")
        if (allocated(error)) return

        call check(error, trim(lines(1)) == "name   unit type    len ucd       info", &
            "header line did not match the expected computed column widths")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == repeat("-", 49), &
            "default dash-after-header line had unexpected width")
        if (allocated(error)) return
        call check(error, trim(lines(3)) == "id          int32   1             ID field", &
            "'id' field row did not match the expected alignment")
        if (allocated(error)) return
        call check(error, trim(lines(4)) == "ra_deg deg  float64 1   pos.eq.ra Right ascension", &
            "'ra_deg' field row did not match the expected alignment")
        if (allocated(error)) return
        call check(error, trim(lines(5)) == "flags       boolean 6", &
            "'flags' field row (empty info) did not match the expected alignment")
    end subroutine test_print_schema_info_alignment

    !> schemas/maml_example.maml has many fields -- disabling all but id0/value exercises that
    !! only is_set columns are printed, together with a custom prefix and all three dash toggles.
    subroutine test_print_schema_info_prefix_dashes_and_is_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: unit, n
        character(len=*), parameter :: out_file = "test_run/print_schema_info_prefix_dashes.txt"
        character(len=128) :: lines(10)

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")
        call schema%set_column_available("value")

        open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")
        call schema%print_schema_info(unit=unit, prefix="# ", table_name=.false., &
            dash_before_header=.true., dash_after_fields=.true.)
        close(unit)

        call read_text_lines(out_file, lines, n)
        call check(error, n == 6, "expected 6 lines: dash, header, dash, id0 row, value row, dash")
        if (allocated(error)) return

        call check(error, lines(1)(1:2) == "# " .and. lines(2)(1:2) == "# " .and. lines(4)(1:2) == "# ", &
            "every emitted line should start with the given prefix")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == trim(lines(3)) .and. trim(lines(3)) == trim(lines(6)), &
            "all three dash lines (before header/after header/after fields) should be identical")
        if (allocated(error)) return
        call check(error, index(lines(2), "name") > 0 .and. index(lines(2), "info") > 0, &
            "line 2 should be the header row")
        if (allocated(error)) return
        call check(error, index(lines(4), "id0") > 0, "line 4 should be the 'id0' field row")
        if (allocated(error)) return
        call check(error, index(lines(5), "value") > 0 .and. index(lines(5), "value_64") == 0, &
            "line 5 should be the 'value' field row only -- no other (disabled) column should appear")
    end subroutine test_print_schema_info_prefix_dashes_and_is_set

    !> Calling print_schema_info repeatedly on the same already-open unit, for different
    !! schemas, must append each call's block -- the primary use case the caller-owned unit=
    !! argument exists for.
    subroutine test_print_schema_info_multiple_schemas_same_unit(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema1, schema2
        integer :: unit, n
        character(len=*), parameter :: out_file = "test_run/print_schema_info_multi_schema.txt"
        character(len=128) :: lines(10)

        call schema1%init(table="multi_a")
        call schema1%add_field("a", "int32")
        call schema1%add_field("b", "float64")
        call parquet_parse_maml(schema1)

        call schema2%init(table="multi_b")
        call schema2%add_field("c", "string", array_size=4)
        call parquet_parse_maml(schema2)

        open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")
        call schema1%print_schema_info(unit=unit, table_name=.false.)
        call schema2%print_schema_info(unit=unit, header=.false., table_name=.false., dash_after_header=.false.)
        close(unit)

        call read_text_lines(out_file, lines, n)
        ! schema1: header + dash + 2 rows = 4 lines; schema2 (headerless): 1 row.
        call check(error, n == 5, "expected schema1's 4-line block plus schema2's single headerless row")
        if (allocated(error)) return

        call check(error, index(lines(1), "name") > 0 .and. index(lines(1), "info") > 0, &
            "first block's header row is missing")
        if (allocated(error)) return
        call check(error, index(lines(3), "a") > 0 .and. index(lines(3), "int32") > 0, &
            "schema1's 'a' row not found where expected")
        if (allocated(error)) return
        call check(error, index(lines(4), "b") > 0 .and. index(lines(4), "float64") > 0, &
            "schema1's 'b' row not found where expected")
        if (allocated(error)) return
        call check(error, index(lines(5), "c") > 0 .and. index(lines(5), "string") > 0, &
            "schema2's headerless 'c' row was not appended to the same unit")
    end subroutine test_print_schema_info_multiple_schemas_same_unit

    !> filename= (no caller-owned unit) must open-append-close on every call, so repeated calls
    !! still accumulate into one file -- the convenience alternative to the unit= path above.
    subroutine test_print_schema_info_filename_append(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema1, schema2
        integer :: unit, n
        character(len=*), parameter :: out_file = "test_run/print_schema_info_filename_append.txt"
        character(len=128) :: lines(10)

        ! Pre-clean: print_schema_info(filename=) always appends, so start from a known-empty file.
        open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")
        close(unit)

        call schema1%init(table="append_a")
        call schema1%add_field("x", "int32")
        call parquet_parse_maml(schema1)

        call schema2%init(table="append_b")
        call schema2%add_field("y", "float32")
        call parquet_parse_maml(schema2)

        call schema1%print_schema_info(filename=out_file, table_name=.false.)
        call schema2%print_schema_info(filename=out_file, header=.false., table_name=.false., dash_after_header=.false.)

        call read_text_lines(out_file, lines, n)
        call check(error, n == 4, "expected schema1's header+dash+row (3 lines) plus schema2's single headerless row")
        if (allocated(error)) return

        call check(error, index(lines(3), "x") > 0 .and. index(lines(3), "int32") > 0, &
            "schema1's 'x' row not found where expected")
        if (allocated(error)) return
        call check(error, index(lines(4), "y") > 0 .and. index(lines(4), "float32") > 0, &
            "schema2's row was not appended to the file opened by filename=")
    end subroutine test_print_schema_info_filename_append

    !> With every column disabled and header=.false./all dash flags off, there is nothing to
    !! print at all -- the call must not write any line (and must not abort).
    subroutine test_print_schema_info_no_cols_no_header(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: unit, n
        character(len=*), parameter :: out_file = "test_run/print_schema_info_empty.txt"
        character(len=128) :: lines(10)

        call schema%init(table="empty_test")
        call schema%add_field("z", "int32")
        call parquet_parse_maml(schema)
        call schema%set_column_unavailable()

        open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")
        call schema%print_schema_info(unit=unit, header=.false., table_name=.false., dash_after_header=.false.)
        close(unit)

        call read_text_lines(out_file, lines, n)
        call check(error, n == 0, &
            "expected zero lines with no enabled columns, header/table_name off, and dashes off")
    end subroutine test_print_schema_info_no_cols_no_header

    !> table_name=.true. (the default) prints "Table name: <table>" using this schema's required
    !! MAML table: key, positioned after dash_before_header and before the header row -- checked
    !! both for an in-code schema (schema%init(table=...)) and a MAML-loaded one, where table:
    !! is not the first line in the source file (schemas/maml_example.maml declares dataset:
    !! before table:), to confirm the value is found by key, not by assumed line position.
    subroutine test_print_schema_info_table_name_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema, schema_maml
        integer :: unit, n
        character(len=*), parameter :: out_file = "test_run/print_schema_info_table_name.txt"
        character(len=*), parameter :: out_file2 = "test_run/print_schema_info_table_name_maml.txt"
        character(len=128) :: lines(10)

        call schema%init(table="my_table_x")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)

        ! Default: table_name=.true., dash_before_header=.false. -- "Table name:" is line 1.
        open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")
        call schema%print_schema_info(unit=unit)
        close(unit)

        call read_text_lines(out_file, lines, n)
        call check(error, n == 4, "expected Table name + header + dash-after-header + 1 field row")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "Table name: my_table_x", &
            "line 1 should be the default-positioned Table name line")
        if (allocated(error)) return
        call check(error, index(lines(2), "name") > 0, "line 2 should be the header row")
        if (allocated(error)) return

        ! With dash_before_header=.true.: dash, then "Table name:", then header.
        open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")
        call schema%print_schema_info(unit=unit, dash_before_header=.true., prefix="# ")
        close(unit)

        call read_text_lines(out_file, lines, n)
        call check(error, n == 5, "expected dash + Table name + header + dash-after-header + 1 field row")
        if (allocated(error)) return
        call check(error, index(lines(1), "-") > 0 .and. index(lines(1), "Table") == 0, &
            "line 1 should be the dash-before-header line, not the Table name line")
        if (allocated(error)) return
        call check(error, trim(lines(2)) == "# Table name: my_table_x", &
            "line 2 should be the prefixed Table name line, positioned after the leading dash")
        if (allocated(error)) return

        ! table_name=.false. suppresses the line entirely.
        open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")
        call schema%print_schema_info(unit=unit, table_name=.false.)
        close(unit)

        call read_text_lines(out_file, lines, n)
        call check(error, n == 3, "expected header + dash-after-header + 1 field row, no Table name line")
        if (allocated(error)) return
        call check(error, index(lines(1), "Table") == 0, "table_name=.false. should suppress the Table name line")
        if (allocated(error)) return

        ! A MAML-loaded schema where table: is not the first source line must still resolve correctly.
        call parquet_parse_maml("schemas/maml_example.maml", schema_maml)
        call schema_maml%set_column_unavailable()
        call schema_maml%set_column_available("id0")

        open(newunit=unit, file=out_file2, status="replace", action="write", form="formatted")
        call schema_maml%print_schema_info(unit=unit, header=.false., dash_after_header=.false.)
        close(unit)

        call read_text_lines(out_file2, lines, n)
        call check(error, n == 2, "expected Table name line + 1 field row")
        if (allocated(error)) return
        call check(error, trim(lines(1)) == "Table name: input_table", &
            "schemas/maml_example.maml's table: input_table was not resolved correctly")
    end subroutine test_print_schema_info_table_name_line

    subroutine test_print_schema_info_no_unit_no_filename_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_no_unit_no_filename", expect_abort=.true., &
            failure_message="print_schema_info with neither unit nor filename was expected to abort")
    end subroutine test_print_schema_info_no_unit_no_filename_aborts

    subroutine test_print_schema_info_unit_not_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_unit_not_open", expect_abort=.true., &
            failure_message="print_schema_info with an unopened unit was expected to abort")
    end subroutine test_print_schema_info_unit_not_open_aborts

    subroutine test_print_schema_info_unit_read_only_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_unit_read_only", expect_abort=.true., &
            failure_message="print_schema_info with a read-only unit was expected to abort")
    end subroutine test_print_schema_info_unit_read_only_aborts

    subroutine test_print_schema_info_unit_filename_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_unit_filename_mismatch", expect_abort=.true., &
            failure_message="print_schema_info with a unit/filename mismatch was expected to abort")
    end subroutine test_print_schema_info_unit_filename_mismatch_aborts

    subroutine test_print_schema_info_uninitialized_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_uninitialized_schema", expect_abort=.true., &
            failure_message="print_schema_info on a never-initialized schema was expected to abort by default")
    end subroutine test_print_schema_info_uninitialized_schema_aborts

    subroutine test_print_schema_info_open_failure_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_open_failure", expect_abort=.true., &
            failure_message="print_schema_info with a filename that cannot be opened was expected to abort")
    end subroutine test_print_schema_info_open_failure_aborts

    !> allow_uninitialized=.true. must make print_schema_info on a never-initialized schema a
    !! complete no-op: no error, and -- critically -- no file touched at all (not even an empty
    !! file created via filename=), distinguishing it from "prints nothing but still opens/closes".
    subroutine test_print_schema_info_allow_uninitialized_is_noop(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: unit, n
        logical :: file_exists
        character(len=*), parameter :: out_file = "test_run/print_schema_info_allow_uninitialized.txt"
        character(len=128) :: lines(10)

        ! unit= path: nothing should be written to the already-open unit.
        open(newunit=unit, file=out_file, status="replace", action="write", form="formatted")
        call schema%print_schema_info(unit=unit, allow_uninitialized=.true.)
        close(unit)

        call read_text_lines(out_file, lines, n)
        call check(error, n == 0, "allow_uninitialized=.true. on an uninitialized schema should write no lines")
        if (allocated(error)) return

        ! filename= path: the file must not even be created/touched.
        inquire(file="test_run/print_schema_info_allow_uninitialized_untouched.txt", exist=file_exists)
        call check(error, .not. file_exists, "sanity check: fixture file should not exist yet")
        if (allocated(error)) return

        call schema%print_schema_info(filename="test_run/print_schema_info_allow_uninitialized_untouched.txt", &
            allow_uninitialized=.true.)

        inquire(file="test_run/print_schema_info_allow_uninitialized_untouched.txt", exist=file_exists)
        call check(error, .not. file_exists, &
            "allow_uninitialized=.true. on an uninitialized schema should not create/touch the filename= file")
    end subroutine test_print_schema_info_allow_uninitialized_is_noop

    subroutine test_add_metadata_before_parse_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_metadata_before_parse", expect_abort=.true., &
            failure_message="add_metadata before the schema has been parsed was expected to abort")
    end subroutine test_add_metadata_before_parse_aborts

    !> clear_metadata truncates %items back to %n_base_items (the count recorded right after
    !! parsing), discarding only entries a later %add_metadata call added -- the base entries
    !! (every top-level header key + any real keyarray: entry) survive intact.
    subroutine test_clear_metadata_keeps_base_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: n_base, i
        logical :: found_table

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        n_base = schema%metadata%n_base_items

        call check(error, n_base > 0, &
            "sanity check: parsing schemas/maml_example.maml should populate base metadata items")
        if (allocated(error)) return

        call schema%add_metadata("runtime_key1", 1_int32)
        call schema%add_metadata("runtime_key2", "value2")

        call check(error, size(schema%metadata%items) == n_base + 2, &
            "sanity check: two add_metadata calls should have appended two entries")
        if (allocated(error)) return

        call schema%clear_metadata()

        call check(error, size(schema%metadata%items) == n_base, &
            "clear_metadata should discard the 2 user-added entries, keeping the n_base_items base entries")
        if (allocated(error)) return

        found_table = .false.
        do i = 1, size(schema%metadata%items)
            if (trim(schema%metadata%items(i)%key) == "table" .and. &
                trim(schema%metadata%items(i)%value) == "input_table") found_table = .true.
        end do
        call check(error, found_table, "the 'table' base metadata entry should survive clear_metadata intact")
    end subroutine test_clear_metadata_keeps_base_entries

    subroutine test_clear_metadata_noop_when_no_user_entries(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: n_before

        call parquet_parse_maml("schemas/maml_example.maml", schema)
        n_before = size(schema%metadata%items)

        call schema%clear_metadata()

        call check(error, size(schema%metadata%items) == n_before, &
            "clear_metadata with no user-added entries should be a no-op")
    end subroutine test_clear_metadata_noop_when_no_user_entries

    !> Same truncation behavior for a schema built in-code (%init/%add_field), not just one
    !! loaded from a .maml file -- n_base_items is set the same way regardless of source.
    subroutine test_clear_metadata_in_code_schema(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: n_base

        call schema%init(table="clear_meta_test")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)
        n_base = schema%metadata%n_base_items

        call schema%add_metadata("extra", 5_int32)
        call check(error, size(schema%metadata%items) == n_base + 1, &
            "sanity check: one add_metadata call should have appended one entry")
        if (allocated(error)) return

        call schema%clear_metadata()
        call check(error, size(schema%metadata%items) == n_base, &
            "clear_metadata should work identically for an in-code-built schema")
    end subroutine test_clear_metadata_in_code_schema

    !> A never-parsed parquet_table_metadata (used standalone, not via a parsed
    !! parquet_schema%metadata) has n_base_items == 0 by default -- clear_metadata on it
    !! takes the "nothing to keep" path, discarding %items entirely rather than truncating.
    subroutine test_clear_metadata_never_parsed(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_metadata) :: md

        call check(error, md%n_base_items == 0, &
            "sanity check: a never-parsed table_metadata should have n_base_items == 0")
        if (allocated(error)) return

        call md%add_metadata("k1", 1_int32)
        call md%add_metadata("k2", "v2")
        call check(error, allocated(md%items), &
            "sanity check: two add_metadata calls should have populated %items")
        if (allocated(error)) return

        call md%clear_metadata()

        call check(error, .not. allocated(md%items), &
            "clear_metadata on a never-parsed table_metadata should deallocate %items entirely")
    end subroutine test_clear_metadata_never_parsed

    !> Category A: add_metadata("author", ...) duplicates a MAML top-level key the schema
    !! actually declared (via schema%init's own author= keyword) -- warn=.true. (the default)
    !! still appends the new entry rather than rejecting or overwriting it; only the printed
    !! WARNING (not asserted here, matching this suite's existing convention of not capturing
    !! stdout for other WARNING-printing paths, e.g. test_missing_key_default_warns) differs
    !! from warn=.false.
    subroutine test_add_metadata_duplicate_maml_init_key_still_appends(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: n_before

        call schema%init(table="dup_a_test", author="Alice")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)
        n_before = size(schema%metadata%items)

        call schema%add_metadata("author", "Bob")

        call check(error, size(schema%metadata%items) == n_before + 1, &
            "add_metadata should still append even when the key duplicates a MAML-declared top-level key")
        if (allocated(error)) return
        call check(error, trim(schema%metadata%items(n_before + 1)%key) == "author", &
            "trim(schema%metadata%items(n_before + 1)%key) == ""author""")
        if (allocated(error)) return
        call check(error, trim(schema%metadata%items(n_before + 1)%value) == "Bob", &
            "the newly-appended duplicate entry should still carry the caller's own value, not overwrite the original")
    end subroutine test_add_metadata_duplicate_maml_init_key_still_appends

    !> Category B: two add_metadata calls sharing a non-reserved key both append (in call
    !! order), the same "duplicate, don't overwrite" behavior as category A but reached via
    !! the generic fallback (key not in schema%init's own keyword list).
    subroutine test_add_metadata_duplicate_generic_key_still_appends(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: n_before

        call schema%init(table="dup_b_test")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)

        call schema%add_metadata("custom_key", 1_int32)
        n_before = size(schema%metadata%items)
        call schema%add_metadata("custom_key", 2_int32)

        call check(error, size(schema%metadata%items) == n_before + 1, &
            "a duplicate non-reserved key should still append a new entry (category B), not overwrite")
        if (allocated(error)) return
        call check(error, trim(schema%metadata%items(n_before)%value) == "1", &
            "trim(schema%metadata%items(n_before)%value) == ""1""")
        if (allocated(error)) return
        call check(error, trim(schema%metadata%items(n_before + 1)%value) == "2", &
            "both the original and the duplicate entry should be preserved, in call order")
    end subroutine test_add_metadata_duplicate_generic_key_still_appends

    !> Category C, end-to-end: add_metadata("DATE", ...) collides with a key the writer
    !! always injects itself (build_file_metadata in parquet_wrapper.cpp writes its own "DATE"
    !! entry -- the file's real write timestamp -- before the schema's own table_metadata
    !! entries are appended). On read, parquet_get_metadata("DATE") returns the first match,
    !! so the writer's own entry silently shadows the caller's -- exactly the collision
    !! category C's warning exists to flag.
    subroutine test_add_metadata_duplicate_writer_key_shadowed_on_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_schema) :: schema
        integer(int32) :: id0(1) = [1_int32]
        character(len=:), allocatable :: dval

        call schema%init(table="dup_c_test")
        call schema%add_field("id0", "int32")
        call parquet_parse_maml(schema)
        call schema%add_metadata("DATE", "user-supplied-not-a-real-date")

        call parquet_open_writer(writer, "test_run/metadata_dup_writer_key.parquet", schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, "test_run/metadata_dup_writer_key.parquet")
        call parquet_get_metadata(reader, "DATE", dval)
        call check(error, dval /= "user-supplied-not-a-real-date", &
            "the writer's own auto-generated DATE entry should shadow a user add_metadata('DATE', ...) call " // &
            "(first match wins on read) -- the exact collision category C's warning flags")
        call parquet_close_reader(reader)
    end subroutine test_add_metadata_duplicate_writer_key_shadowed_on_read

    !> warn=.false. suppresses the printed WARNING but must not change what gets stored --
    !! the duplicate entry is still appended, same as the warn=.true. default (see
    !! test_add_metadata_duplicate_maml_init_key_still_appends).
    subroutine test_add_metadata_warn_false_still_appends(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        integer :: n_before

        call schema%init(table="dup_warn_false_test", author="Alice")
        call schema%add_field("x", "int32")
        call parquet_parse_maml(schema)
        n_before = size(schema%metadata%items)

        call schema%add_metadata("author", "Bob", warn=.false.)

        call check(error, size(schema%metadata%items) == n_before + 1, &
            "warn=.false. should suppress the warning print but not change append behavior")
    end subroutine test_add_metadata_warn_false_still_appends

end module test_metadata
