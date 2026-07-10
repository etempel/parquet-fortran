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
            new_unittest("array element conversion failure falls back to the whole default array", &
                test_array_conversion_failure_default), &
            new_unittest("missing key with no default aborts", test_missing_key_no_default_aborts), &
            new_unittest("unparsable value with no default aborts", test_conversion_failure_no_default_aborts) &
            ]
    end subroutine collect_tests_parquet_metadata

    !> Builds a schema (docs/maml_example.maml, "id0" only) carrying one
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

        call parquet_parse_maml("docs/maml_example.maml", schema)
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        call schema%add_metadata("meta_i32", 42_int32)
        call schema%add_metadata("meta_i64", 123456789012_int64)
        call schema%add_metadata("meta_f32", 3.5_real32)
        call schema%add_metadata("meta_f64", 3.14159265358979_real64)
        call schema%add_metadata("meta_bool", .true.)
        call schema%add_metadata("meta_str", "hello world")
        call schema%add_metadata("meta_not_a_number", "not_a_number")
        call schema%add_metadata("meta_i32_arr", [1_int32, 2_int32, 3_int32])
        call schema%add_metadata("meta_i64_arr", [10_int64, 20_int64, 30_int64])
        call schema%add_metadata("meta_f32_arr", [1.5_real32, 2.5_real32])
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

        call parquet_parse_maml("docs/maml_example.maml", schema)
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

    subroutine test_array_conversion_failure_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_schema) :: schema
        integer(int32) :: id0(3) = [1_int32, 2_int32, 3_int32]
        integer(int32), allocatable :: i32arr(:)
        integer(int32), parameter :: fallback(2) = [-9_int32, -8_int32]
        character(len=*), parameter :: out_file = "test_run/metadata_array_conversion_failure.parquet"

        call parquet_parse_maml("docs/maml_example.maml", schema)
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

end module test_metadata
