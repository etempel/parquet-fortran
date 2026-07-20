!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Compiles and runs the code examples shown in README.md and the doc/pages
!> user guide, so that a change to the library's public API that silently
!> breaks a documented example is caught here rather than by a user
!> copy-pasting the guide.
!>
!> Each subroutine below mirrors one documented example as closely as possible
!> (same `use` clauses, same variable declarations, same calls), except that
!> it writes to a path under test_run/ instead of the doc's "data.parquet",
!> and is wrapped as a subroutine instead of a standalone `program` so it can
!> be driven by test-drive and checked with a round-trip read-back.
module test_examples
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_examples
    !
contains
    !
    subroutine collect_tests_parquet_examples(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("README minimal writer/reader example", test_readme_minimal_example), &
            new_unittest("README MAML-schema writer example", test_readme_maml_schema_writer_example), &
            new_unittest("doc/pages/combined-example.md example", test_readme_combined_example), &
            new_unittest("maml_example2 writer produces a matching sidecar .maml", &
                test_maml_example2_sidecar_keyarray), &
            new_unittest("doc/pages/date-time.md datetime_quickstart example", test_datetime_quickstart_example), &
            new_unittest("doc/pages/performance.md write_parquet_qc_example", test_performance_qc_example), &
            new_unittest("doc/pages/string-columns.md strings_quickstart example", &
                test_strings_quickstart_example), &
            new_unittest("doc/pages/string-columns.md token_column example", test_token_column_example) &
            ]
    end subroutine collect_tests_parquet_examples

    !> "Minimal writer example" + "Minimal reader example"
    !> (README sections "Writing parquet files..." / "Reading parquet files...")
    subroutine test_readme_minimal_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/readme_minimal_example.parquet"
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer(int32), allocatable :: id(:)

        call readme_write_parquet_example(out_file)

        ! README "Minimal reader example", reading the file written above.
        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)

        allocate(id(nrows))
        call parquet_read_column(reader, "id", id)

        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. id(1) == 1_int32 .and. id(3) == 3_int32, &
            "README minimal writer/reader example did not round-trip the 'id' column correctly")
    end subroutine test_readme_minimal_example

    subroutine readme_write_parquet_example(out_file)
        character(len=*), intent(in) :: out_file
        type(parquet_writer) :: writer
        integer(int32) :: id(3)
        real(real64) :: value(3)

        id = [1_int32, 2_int32, 3_int32]
        value = [10.0_real64, 20.0_real64, 30.0_real64]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "value", value)
        call parquet_close_writer(writer)
    end subroutine readme_write_parquet_example

    !> The "explicit column definitions and table metadata" MAML-schema
    !> snippet from the README's "Writing parquet files..." section.
    subroutine test_readme_maml_schema_writer_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/readme_maml_schema_example.parquet"
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        type(parquet_reader) :: reader
        integer(int32) :: id0(3)
        integer(int32), allocatable :: id0_read(:)
        integer(int64) :: nrows

        ! README: call parquet_parse_maml("maml_example.maml", schema)
        call parquet_parse_maml("schemas/maml_example.maml", schema)

        ! Only write the one column this test provides data for.
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        id0 = [1_int32, 2_int32, 3_int32]

        ! README: call parquet_open_writer(writer, "data.parquet", schema)
        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        allocate(id0_read(nrows))
        call parquet_read_column(reader, "id0", id0_read)
        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. all(id0_read == id0), &
            "README MAML-schema writer example did not round-trip the 'id0' column correctly")
    end subroutine test_readme_maml_schema_writer_example

    !> README "Combined example: MAML schema, matrices and metadata".
    !> Mirrors the program's use clause and declarations verbatim (this is
    !> what caught a missing `int64` import in an earlier version of the
    !> README example).
    subroutine test_readme_combined_example(error)
        use iso_fortran_env, only: int32, int64, real64
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        integer(int32) :: id0(3)
        integer(int64) :: idarr(2, 3)   ! (col_size, nrows) for the "idarr" vector column

        character(len=*), parameter :: out_file = "test_run/readme_combined_example.parquet"
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer(int32), allocatable :: id0_read(:)
        integer(int64), allocatable :: idarr_read(:,:)

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

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_write_column(writer, "idarr", idarr)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        allocate(id0_read(nrows), idarr_read(2, nrows))
        call parquet_read_column(reader, "id0", id0_read)
        call parquet_read_column(reader, "idarr", idarr_read)
        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. all(id0_read == id0) .and. all(idarr_read == idarr), &
            "README combined example did not round-trip the 'id0'/'idarr' columns correctly")
    end subroutine test_readme_combined_example
    !
    !> doc/pages/date-time.md's "datetime_quickstart" example: writes a
    !> parquet_date column (with one null) and a parquet_timestamp column (set
    !> from civil fields, an ISO-8601 string, and set_unix), reads them back,
    !> and checks the round-tripped values/null state/to_string output.
    subroutine test_datetime_quickstart_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/readme_datetime_quickstart.parquet"
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_date) :: observed(3)
        type(parquet_timestamp) :: taken_at(3)
        character(len=:), allocatable :: s
        integer(int32) :: y, mo, d, h, mi, sec

        call observed(1)%set(2024, 7, 16)
        call observed(2)%set(2024, 7, 17)
        call observed(3)%set_null()                        ! a missing value

        call taken_at(1)%set(2024, 7, 16, 12, 34, 56)
        call taken_at(2)%parse("2024-07-17T08:00:00.5")     ! from an ISO-8601 string
        call taken_at(3)%set_unix(1721260800_8, parquet_unit_seconds)

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "observed", observed)
        call parquet_write_column(writer, "taken_at", taken_at)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "observed", observed)
        call parquet_read_column(reader, "taken_at", taken_at)
        call parquet_close_reader(reader)

        call check(error, .not. observed(1)%is_null() .and. observed(1)%year() == 2024 .and. &
            observed(1)%month() == 7 .and. observed(1)%day() == 16, &
            "datetime_quickstart example: 'observed' row 1 did not round-trip correctly")
        if (allocated(error)) return

        call check(error, observed(3)%is_null(), &
            "datetime_quickstart example: 'observed' row 3 should have round-tripped as null")
        if (allocated(error)) return

        call check(error, .not. taken_at(1)%is_null(), &
            "datetime_quickstart example: 'taken_at' row 1 should not be null")
        if (allocated(error)) return

        call taken_at(1)%to_string(s)
        call check(error, s == "2024-07-16T12:34:56", &
            "datetime_quickstart example: 'taken_at' row 1 to_string mismatch, got: " // s)
        if (allocated(error)) return

        call taken_at(2)%get(y, mo, d, h, mi, sec)
        call check(error, y == 2024 .and. mo == 7 .and. d == 17 .and. h == 8, &
            "datetime_quickstart example: 'taken_at' row 2 (parsed from ISO-8601) did not round-trip correctly")
        if (allocated(error)) return

        call check(error, taken_at(3)%to_unix(parquet_unit_seconds) == 1721260800_8, &
            "datetime_quickstart example: 'taken_at' row 3 (set_unix) did not round-trip correctly")
    end subroutine test_datetime_quickstart_example
    !
    !> doc/pages/performance.md's "write_parquet_qc_example": builds a qc-maml
    !> directly via schema%maml%name/schema%maml%lines (rather than a file or
    !> add_col_qc), writes an out-of-range column with is_valid (one Null) and
    !> qc=.true./compression="zstd", then reads it back and checks the
    !> round-tripped values/nulls -- the WARNING itself is not asserted on
    !> (test-drive can't capture stdout), only that the write/read still
    !> succeeds and round-trips correctly despite the qc violation.
    subroutine test_performance_qc_example(error)
        use iso_fortran_env, only: int32
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/readme_performance_qc_example.parquet"
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4) = [10_int32, 400_int32, 90_int32, 200_int32]  ! 400 is out of range
        integer(int32) :: ra_read(4)
        logical :: is_valid(4) = [.true., .true., .false., .true.]           ! row 3 will be written as Null
        logical :: is_valid_read(4)

        schema%maml%name = "qc_example.maml"
        schema%maml%lines = [character(len=40) :: &
            "table: qc_example_table", &
            "fields:", &
            "- name: ra", &
            "  data_type: int32", &
            "  qc:", &
            "    min: '>= 0'", &
            "    max: '< 360'" ]

        call parquet_parse_maml(schema)

        call parquet_open_writer(writer, out_file, schema, qc=.true., compression="zstd")
        call parquet_write_column(writer, "ra", ra, is_valid=is_valid)
        ! prints: WARNING: qc violation for column 'ra': declared min >= 0, max < 360, ...
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "ra", ra_read, is_valid=is_valid_read)
        call parquet_close_reader(reader)

        ! Row 3 was written as a genuine Null (is_valid(3)=.false.); with no null_value=
        ! passed on read, that slot comes back as the safe default (0), not the original
        ! ra(3) -- see doc/pages/supported-data-types.md's "Null values" section.
        call check(error, all(is_valid_read .eqv. is_valid), &
            "performance.md qc example did not round-trip the 'ra' column's is_valid mask correctly")
        if (allocated(error)) return
        call check(error, ra_read(1) == ra(1) .and. ra_read(2) == ra(2) .and. ra_read(3) == 0_int32 .and. &
            ra_read(4) == ra(4), &
            "performance.md qc example did not round-trip the 'ra' column values correctly")
    end subroutine test_performance_qc_example
    !
    !> doc/pages/string-columns.md's "strings_quickstart" example: appends two
    !> strings, a null, and an empty string to a parquet_string_column, then
    !> checks size/character_size/null_count and per-row is_null/get.
    subroutine test_strings_quickstart_example(error)
        use parquet_strings, only: parquet_string_column
        use iso_fortran_env, only: int64
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: names
        character(len=:), allocatable :: s

        call names%append_string("Alice")
        call names%append_string("Bob")
        call names%append_null()             ! a missing value (not the same as "")
        call names%append_string("")         ! an empty string

        call check(error, names%size() == 4_int64, "strings_quickstart example: unexpected size()")
        if (allocated(error)) return
        call check(error, names%character_size() == 8_int64, &
            "strings_quickstart example: unexpected character_size()")
        if (allocated(error)) return
        call check(error, names%null_count() == 1_int64, "strings_quickstart example: unexpected null_count()")
        if (allocated(error)) return

        call check(error, .not. names%is_null(1_int64), "strings_quickstart example: row 1 should not be null")
        if (allocated(error)) return
        call names%get(1_int64, s)
        call check(error, s == "Alice", "strings_quickstart example: row 1 get() mismatch, got: " // s)
        if (allocated(error)) return

        call check(error, names%is_null(3_int64), "strings_quickstart example: row 3 should be null")
        if (allocated(error)) return

        call check(error, .not. names%is_null(4_int64), "strings_quickstart example: row 4 should not be null")
        if (allocated(error)) return
        call names%get(4_int64, s)
        call check(error, s == "", "strings_quickstart example: row 4 get() should be an empty string")
    end subroutine test_strings_quickstart_example
    !
    !> doc/pages/string-columns.md's "token_column" example: reserves capacity,
    !> appends tokens (one stripped on append), and checks find/reverse-find
    !> and the empty-token count.
    subroutine test_token_column_example(error)
        use parquet_strings, only: parquet_string_column
        use iso_fortran_env, only: int64
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: tokens
        integer(int64) :: i, first_the, n_empty

        call tokens%reserve(1000_int64, 8000_int64)   ! rough estimate; avoids realloc churn
        call tokens%append_string("the")
        call tokens%append_string("quick")
        call tokens%append_string("the")
        call tokens%append_string("  fox  ", strip=.true.)   ! stored as "fox"

        first_the = tokens%find("the")           ! 1
        call check(error, first_the == 1_int64, "token_column example: first 'the' should be at row 1")
        if (allocated(error)) return

        call check(error, tokens%find("the", reverse=.true.) == 3_int64, &
            "token_column example: last 'the' should be at row 3")
        if (allocated(error)) return

        call check(error, tokens%find("fox") > 0, &
            "token_column example: 'fox' should be present (trimmed on append)")
        if (allocated(error)) return

        n_empty = 0
        do i = 1, tokens%size()
            if (.not. tokens%is_null(i) .and. tokens%is_empty(i)) n_empty = n_empty + 1
        end do
        call check(error, n_empty == 0_int64, "token_column example: no token should be empty")
    end subroutine test_token_column_example
    !
    !> Writes a four-column ("id", "name", "RA", "Dec") table using
    !> schemas/maml_example2.maml's schema, with write_maml=.true. so a sidecar
    !> .maml is produced alongside the parquet file. Checks that:
    !> - the schema (including list-form `ucd:` on "id"/"Dec" and the blank
    !>   array_size:/col_size: on "RA"/"Dec" defaulting to 1) parses correctly,
    !> - the keyarray: entries ("test_url"/"test_url2") come through correctly,
    !> - a few of the new top-level scalar metadata keys round-trip correctly,
    !> both in the in-memory metadata/cinfo and after reparsing the sidecar
    !> (with every field enabled and written, nothing should be pruned).
    subroutine test_maml_example2_sidecar_keyarray(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema, sidecar_schema
        integer(int32) :: id(3)
        character(len=24) :: name(3)
        real(real64) :: ra(3), dec(3)
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/maml_example2.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/maml_example2.maml"

        call parquet_parse_maml("schemas/maml_example2.maml", schema)

        call check_keyarray_entries(error, schema%metadata, "in-memory metadata parsed from schemas/maml_example2.maml")
        if (allocated(error)) return

        call check_field_schema(error, schema%cinfo, "in-memory cinfo parsed from schemas/maml_example2.maml")
        if (allocated(error)) return

        call check_scalar_metadata(error, schema%metadata, "in-memory metadata parsed from schemas/maml_example2.maml")
        if (allocated(error)) return

        id = [1_int32, 2_int32, 3_int32]
        name = ["Alice", "Bob  ", "Carol"]
        ra = [10.5_real64, 45.2_real64, 190.0_real64]
        dec = [-5.1_real64, 12.3_real64, 60.0_real64]

        call parquet_open_writer(writer, out_file, schema, write_maml=.true.)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "name", name)
        call parquet_write_column(writer, "RA", ra)
        call parquet_write_column(writer, "Dec", dec)
        call parquet_close_writer(writer)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        call parquet_parse_maml(sidecar_file, sidecar_schema)
        call check_keyarray_entries(error, sidecar_schema%metadata, "metadata reparsed from the written sidecar .maml")
        if (allocated(error)) return

        call check_field_schema(error, sidecar_schema%cinfo, "cinfo reparsed from the written sidecar .maml")
        if (allocated(error)) return

        ! Every field was enabled and written above, so write_maml's field
        ! pruning should be a no-op here: all 4 fields should still be listed.
        call check(error, size(sidecar_schema%cinfo%col) == size(schema%cinfo%col), &
            "sidecar .maml should still list all 4 fields since none were disabled")
    end subroutine test_maml_example2_sidecar_keyarray

    subroutine check_field_schema(error, cinfo, context)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column_info), intent(in) :: cinfo
        character(len=*), intent(in) :: context
        integer :: idx

        call check(error, size(cinfo%col) == 4, "expected 4 fields (" // context // ")")
        if (allocated(error)) return

        idx = cinfo%get_column_index("id")
        call check(error, trim(cinfo%col(idx)%data_type) == "int32" .and. &
            trim(cinfo%col(idx)%ucd) == "meta.id;meta.main", &
            "unexpected schema for field 'id' (list-form ucd: should join with ';') (" // context // ")")
        if (allocated(error)) return

        idx = cinfo%get_column_index("name")
        call check(error, trim(cinfo%col(idx)%data_type) == "string" .and. cinfo%col(idx)%array_size == 24, &
            "unexpected schema for field 'name' (" // context // ")")
        if (allocated(error)) return

        idx = cinfo%get_column_index("RA")
        call check(error, trim(cinfo%col(idx)%data_type) == "float64" .and. trim(cinfo%col(idx)%unit) == "deg" .and. &
            trim(cinfo%col(idx)%ucd) == "pos.eq.ra" .and. cinfo%col(idx)%array_size == 1 .and. &
            cinfo%col(idx)%col_size == 1, &
            "unexpected schema for field 'RA' (blank array_size:/col_size: should default to 1) (" // context // ")")
        if (allocated(error)) return

        idx = cinfo%get_column_index("Dec")
        call check(error, trim(cinfo%col(idx)%data_type) == "float64" .and. trim(cinfo%col(idx)%ucd) == "pos.eq.dec", &
            "unexpected schema for field 'Dec' (single-item list-form ucd:) (" // context // ")")
    end subroutine check_field_schema

    subroutine check_scalar_metadata(error, metadata, context)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_metadata), intent(in) :: metadata
        character(len=*), intent(in) :: context
        integer :: i
        logical :: found_survey, found_version, found_date, found_maml_version
        logical :: found_depends_1, found_depends_2, found_keywords

        found_survey = .false.
        found_version = .false.
        found_date = .false.
        found_maml_version = .false.
        found_depends_1 = .false.
        found_depends_2 = .false.
        found_keywords = .false.

        do i = 1, size(metadata%items)
            select case (trim(metadata%items(i)%key))
            case ("survey")
                found_survey = .true.
                call check(error, trim(metadata%items(i)%value) == "The Big Survey", &
                    "unexpected value for 'survey' (" // context // ")")
                if (allocated(error)) return
            case ("version")
                found_version = .true.
                call check(error, trim(metadata%items(i)%value) == "1.3", &
                    "unexpected value for 'version' (" // context // ")")
                if (allocated(error)) return
            case ("date")
                found_date = .true.
                ! date: '2025-09-01' -- quoted in the source, so parquet_unquote
                ! strips the wrapping quotes.
                call check(error, trim(metadata%items(i)%value) == "2025-09-01", &
                    "unexpected value for 'date' (" // context // ")")
                if (allocated(error)) return
            case ("maml_version")
                ! Top-level keys are lower-cased while parsing, so "MAML_version:"
                ! in the source becomes the "maml_version" metadata key.
                found_maml_version = .true.
                call check(error, trim(metadata%items(i)%value) == "1.2", &
                    "unexpected value for 'maml_version' (" // context // ")")
                if (allocated(error)) return
            case ("depends_1")
                ! Each depends: list entry's survey/dataset/table/version
                ! sub-keys are combined into one semicolon-separated string.
                found_depends_1 = .true.
                call check(error, trim(metadata%items(i)%value) == "The Medium Survey;SpecZ;Spec_field_01;3.7", &
                    "unexpected value for 'depends_1' (" // context // ")")
                if (allocated(error)) return
            case ("depends_2")
                found_depends_2 = .true.
                call check(error, trim(metadata%items(i)%value) == "The Tiny Survey;Stars;Phot_South;2", &
                    "unexpected value for 'depends_2' (" // context // ")")
                if (allocated(error)) return
            case ("keywords")
                ! keywords: is a plain-string list; all its items are combined
                ! into a single semicolon-separated "keywords" entry, rather
                ! than one entry per item.
                found_keywords = .true.
                call check(error, trim(metadata%items(i)%value) == "Optional keyword tag;TopCat", &
                    "unexpected value for 'keywords' (" // context // ")")
                if (allocated(error)) return
            end select
        end do

        call check(error, found_survey, "metadata key 'survey' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_version, "metadata key 'version' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_date, "metadata key 'date' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_maml_version, "metadata key 'maml_version' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_depends_1, "metadata key 'depends_1' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_depends_2, "metadata key 'depends_2' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_keywords, "metadata key 'keywords' not found (" // context // ")")
    end subroutine check_scalar_metadata

    subroutine check_keyarray_entries(error, metadata, context)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_metadata), intent(in) :: metadata
        character(len=*), intent(in) :: context
        integer :: i
        logical :: found_url, found_url2

        found_url = .false.
        found_url2 = .false.

        do i = 1, size(metadata%items)
            if (trim(metadata%items(i)%key) == "test_url") then
                found_url = .true.
                call check(error, trim(metadata%items(i)%value) == "//example.com/data #new", &
                    "unexpected value for keyarray entry 'test_url' (" // context // ")")
                if (allocated(error)) return
                call check(error, trim(metadata%items(i)%description) == "something: and else", &
                    "unexpected comment for keyarray entry 'test_url' (" // context // ")")
                if (allocated(error)) return
            else if (trim(metadata%items(i)%key) == "test_url2") then
                found_url2 = .true.
                call check(error, trim(metadata%items(i)%value) == "http://example.com/data :#new", &
                    "unexpected value for keyarray entry 'test_url2' (" // context // ")")
                if (allocated(error)) return
                call check(error, trim(metadata%items(i)%description) == "something: and else", &
                    "unexpected comment for keyarray entry 'test_url2' (" // context // ")")
                if (allocated(error)) return
            end if
        end do

        call check(error, found_url, "keyarray entry 'test_url' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_url2, "keyarray entry 'test_url2' not found (" // context // ")")
    end subroutine check_keyarray_entries
    !
end module test_examples
