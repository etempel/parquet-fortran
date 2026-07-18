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
                test_votable_xml_escapes_special_chars) &
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
        call check(error, i32 == 42_int32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int32 metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_i64", i64)
        call check(error, i64 == 123456789012_int64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int64 metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_f32", f32)
        call check(error, abs(f32 - 3.5_real32) < 1.0e-5_real32)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float32 metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_f64", f64)
        call check(error, abs(f64 - 3.14159265358979_real64) < 1.0e-10_real64)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float64 metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_bool", lg)
        call check(error, lg .eqv. .true.)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "logical metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_bool_false", lg)
        call check(error, lg .eqv. .false.)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "logical .false. metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_str", sval)
        call check(error, trim(sval) == "hello world")
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
        call check(error, size(i32arr) == 3)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int32 array metadata has wrong size")
            return
        end if
        call check(error, all(i32arr == [1_int32, 2_int32, 3_int32]))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int32 array metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_i64_arr", i64arr)
        call check(error, all(i64arr == [10_int64, 20_int64, 30_int64]))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "int64 array metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_f32_arr", f32arr)
        call check(error, size(f32arr) == 2)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float32 array metadata has wrong size")
            return
        end if
        call check(error, all(abs(f32arr - [1.5_real32, 2.5_real32]) < 1.0e-5_real32))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float32 array metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_f64_arr", f64arr)
        call check(error, all(abs(f64arr - [1.25_real64, 2.25_real64]) < 1.0e-10_real64))
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "float64 array metadata did not round-trip")
            return
        end if

        call parquet_get_metadata(reader, "meta_bool_arr", lgarr)
        call check(error, size(lgarr) == 3)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "logical array metadata has wrong size")
            return
        end if
        call check(error, lgarr(1) .eqv. .true. .and. lgarr(2) .eqv. .false. .and. lgarr(3) .eqv. .true.)
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

        call check(error, size(sarr) == 3)
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "mixed-length string array metadata has wrong size")
            return
        end if

        call check(error, trim(sarr(1)) == "ab")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, &
                "short (2-character) element of a mixed-length string array metadata did not round-trip")
            return
        end if

        call check(error, trim(sarr(2)) == "category_long")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            call test_failed(error, "middle element of a mixed-length string array metadata did not round-trip")
            return
        end if

        call check(error, trim(sarr(3)) == "final_tag_here")
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
        call check(error, len_trim(sval) > 0)
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
        call check(error, i32 == 99_int32)
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
        call check(error, i32 == 7_int32)
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

        call check(error, i32 == 13_int32)
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
        call check(error, i32 == -1_int32)
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
        call check(error, i32 == -2_int32)
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

        call check(error, i64 == -21_int64 .and. abs(f32 + 2.5_real32) < 1.0e-5_real32 .and. &
            abs(f64 + 3.5_real64) < 1.0e-10_real64 .and. (lg .eqv. .true.))
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

        call check(error, i64 == -11_int64 .and. i64b == -12_int64 .and. &
            abs(f32 + 1.5_real32) < 1.0e-5_real32 .and. abs(f32b + 1.6_real32) < 1.0e-5_real32 .and. &
            abs(f64 + 2.5_real64) < 1.0e-10_real64 .and. abs(f64b + 2.6_real64) < 1.0e-10_real64 .and. &
            (lg .eqv. .true.) .and. (lgb .eqv. .true.) .and. &
            trim(sval) == "fallback" .and. trim(svalb) == "fallback2")
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
        call check(error, all(i32arr == fallback))
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

        call check(error, all(i64arr == i64_fb) .and. all(abs(f32arr - f32_fb) < 1.0e-5_real32) .and. &
            all(abs(f64arr - f64_fb) < 1.0e-10_real64) .and. all(lgarr .eqv. lg_fb))
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

        call check(error, all(i32arr == i32_fb) .and. all(i64arr == i64_fb) .and. &
            all(abs(f32arr - f32_fb) < 1.0e-5_real32) .and. &
            all(abs(f64arr - f64_fb) < 1.0e-10_real64) .and. all(lgarr .eqv. lg_fb) .and. &
            size(sarr) == 2 .and. trim(sarr(1)) == "aa" .and. trim(sarr(2)) == "bb")
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
        call schema%add_metadata("", 1_int32)

        call check(error, .not. allocated(schema%metadata%items), &
            "add_metadata with an empty key should not append any metadata entry")
    end subroutine test_add_metadata_empty_key_noop

end module test_metadata
