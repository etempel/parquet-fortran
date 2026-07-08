!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for MAML parsing/validation and parquet_column_info helpers.
!> Only the "happy path" is covered here: every negative case in this module's
!> functions triggers `error stop`, which aborts the whole test process, so
!> those are covered as subprocess scenarios in test_errors.f90 instead.
!>
!> NB: always pass the failure message directly to `check(error, cond, message)`.
!> Do NOT follow a failed `check` with a separate `test_failed` call on the same
!> `error` variable: `check` already allocates `error` internally when `cond` is
!> false, and passing that already-allocated `error` into another `intent(out)`
!> argument (as `test_failed` also expects) triggers Fortran's automatic
!> finalization of the old value before reassignment, which for test-drive's
!> `error_type` calls its FINAL `escalate_error` and aborts the whole process.
module test_maml
    use parquet
    use parquet_maml_base, only : parquet_maml_file, get_parquet_maml
    use iso_fortran_env, only : real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_maml
    !
contains
    !
    subroutine collect_tests_parquet_maml(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("validate a well-formed MAML file", test_validate_maml_ok), &
            new_unittest("validate maml_example2.maml (extra top-level keys, depends:, extra:, " // &
                "list-form ucd:, keyarray:)", test_validate_maml_example2_ok), &
            new_unittest("validate maml_example2.maml by filename (parquet_validate_maml overload)", &
                test_validate_maml_by_filename_ok), &
            new_unittest("validate a user MAML that is a valid subset", test_validate_user_maml_ok), &
            new_unittest("col_map: renames a field to an internal name", test_validate_user_maml_col_map_ok), &
            new_unittest("load a MAML file from disk", test_load_maml_file), &
            new_unittest("get_column_index finds an existing column", test_get_column_index_found), &
            new_unittest("set_unavailable/set_available toggle is_set", test_set_available_unavailable), &
            new_unittest("add_col_qc builds a qc-maml from compact strings", test_add_col_qc_builds_maml), &
            new_unittest("add_col_qc result reads back through parquet_open_reader", test_add_col_qc_roundtrip), &
            new_unittest("add_col_qc with an empty input is a no-op", test_add_col_qc_empty_input_is_noop) &
            ]
    end subroutine collect_tests_parquet_maml

    subroutine test_validate_maml_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = get_parquet_maml("maml_example.maml")

        ! Should not error stop: this is a well-formed MAML file.
        call parquet_validate_maml(maml)

        call check(error, .true.)
    end subroutine test_validate_maml_ok

    !> docs/maml_example2.maml exercises several things maml_example.maml does
    !> not: extra top-level keys (survey, version, date, depends:, keywords:,
    !> MAML_version), an extra: block, list-form ucd: on some fields, and
    !> blank array_size:/col_size: values -- none of which parquet_validate_maml
    !> should object to, since it only checks field names/data_type/table:.
    subroutine test_validate_maml_example2_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = parquet_load_maml_file("docs/maml_example2.maml")

        ! Should not error stop: this is a well-formed MAML file.
        call parquet_validate_maml(maml)

        call check(error, .true.)
    end subroutine test_validate_maml_example2_ok

    !> parquet_validate_maml is generic: it also accepts a filename directly
    !> (loading the file from disk internally), instead of requiring the
    !> caller to first call parquet_load_maml_file themselves.
    subroutine test_validate_maml_by_filename_ok(error)
        type(error_type), allocatable, intent(out) :: error

        ! Should not error stop: this is a well-formed MAML file.
        call parquet_validate_maml("docs/maml_example2.maml")

        call check(error, .true.)
    end subroutine test_validate_maml_by_filename_ok

    subroutine test_validate_user_maml_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: base_maml, user_maml
        type(parquet_column_info) :: user_cinfo
        type(parquet_table_metadata) :: user_metadata
        integer :: idx

        base_maml = get_parquet_maml("maml_example.maml")

        user_maml%name = "user_subset.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "fields:", &
            "- name: id0", &
            "  data_type: int32", &
            "- name: name", &
            "  data_type: string", &
            "  array_size: 18" ]

        ! Should not error stop: every field in user_maml (id0, name) exists in base_maml.
        call parquet_validate_user_maml(base_maml, user_maml)

        call parquet_read_maml(user_maml, user_cinfo, user_metadata)

        ! Columns declared in base_maml but omitted from user_maml (11 of the 13) are
        ! merged back in as deactivated placeholders, so the schema still covers every
        ! base column; only "id0" and "name" should actually be active (is_set).
        call check(error, size(user_cinfo%col) == 13, &
            "expected the merged schema to cover all 13 base columns")
        if (allocated(error)) return

        idx = user_cinfo%get_column_index("id0")
        call check(error, user_cinfo%col(idx)%is_set .and. .not. user_cinfo%col(idx)%is_deactivated, &
            "expected 'id0' to be active (declared by the user MAML)")
        if (allocated(error)) return

        idx = user_cinfo%get_column_index("idarr")
        call check(error, (.not. user_cinfo%col(idx)%is_set) .and. user_cinfo%col(idx)%is_deactivated, &
            "expected 'idarr' to be merged in as a deactivated placeholder")
    end subroutine test_validate_user_maml_ok

    !> col_map: renames a field's internal (base) name to whatever name the
    !> user MAML's own fields: section declares. parquet_write_column etc.
    !> still address the column by its internal name ("id0"); only the
    !> resulting schema/parquet output uses the renamed name ("my_id").
    subroutine test_validate_user_maml_col_map_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: base_maml, user_maml
        type(parquet_column_info) :: user_cinfo
        type(parquet_table_metadata) :: user_metadata
        integer :: idx

        base_maml = get_parquet_maml("maml_example.maml")

        user_maml%name = "user_col_map.maml"
        user_maml%lines = [character(len=40) :: &
            "table: user_table", &
            "extra:", &
            "  col_map:", &
            "  - id0: my_id", &
            "fields:", &
            "- name: my_id", &
            "  data_type: int32", &
            "  info: renamed via col_map" ]

        ! Should not error stop: id0 (referenced by col_map) exists in base_maml.
        call parquet_validate_user_maml(base_maml, user_maml)

        call check(error, allocated(user_maml%col_map), "expected col_map to be populated on user_maml")
        if (allocated(error)) return
        call check(error, size(user_maml%col_map) == 1 .and. &
            trim(user_maml%col_map(1)%internal_name) == "id0" .and. &
            trim(user_maml%col_map(1)%output_name) == "my_id", &
            "unexpected user_maml%col_map contents")
        if (allocated(error)) return

        call parquet_read_maml(user_maml, user_cinfo, user_metadata)

        ! The renamed field is stored under its internal name ("id0"), same as
        ! every other lookup (parquet_write_column, set_available, ...);
        ! output_name carries the user-facing rename ("my_id").
        idx = user_cinfo%get_column_index("id0")
        call check(error, idx > 0, "expected 'id0' (internal name) to be found via get_column_index")
        if (allocated(error)) return
        call check(error, user_cinfo%col(idx)%is_set .and. .not. user_cinfo%col(idx)%is_deactivated, &
            "expected the renamed field to be active")
        if (allocated(error)) return
        call check(error, trim(user_cinfo%col(idx)%output_name) == "my_id", &
            "expected output_name to carry the col_map-declared name 'my_id'")
        if (allocated(error)) return
        call check(error, trim(user_cinfo%col(idx)%info) == "renamed via col_map", &
            "expected the renamed field's own fields: attributes (info) to be preserved")
    end subroutine test_validate_user_maml_col_map_ok

    subroutine test_load_maml_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml

        maml = parquet_load_maml_file("docs/maml_example.maml")

        call check(error, trim(maml%name) == "docs/maml_example.maml" .and. size(maml%lines) == 91, &
            "parquet_load_maml_file returned unexpected content")
    end subroutine test_load_maml_file

    subroutine test_get_column_index_found(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        integer :: idx

        maml = get_parquet_maml("maml_example.maml")
        call parquet_read_maml(maml, cinfo, metadata)

        idx = cinfo%get_column_index("id0")
        call check(error, idx == 1, "get_column_index('id0') did not return the expected index")
        if (allocated(error)) return

        idx = cinfo%get_column_index("idarr")
        call check(error, idx == 2, "get_column_index('idarr') did not return the expected index")
    end subroutine test_get_column_index_found

    subroutine test_set_available_unavailable(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: metadata
        integer :: idx

        maml = get_parquet_maml("maml_example.maml")
        call parquet_read_maml(maml, cinfo, metadata)

        idx = cinfo%get_column_index("id0")
        call check(error, cinfo%col(idx)%is_set, "expected id0 to be set by default after parquet_read_maml")
        if (allocated(error)) return

        call cinfo%set_unavailable("id0")
        call check(error, .not. cinfo%col(idx)%is_set, "set_unavailable('id0') did not clear is_set")
        if (allocated(error)) return

        call cinfo%set_available("id0")
        call check(error, cinfo%col(idx)%is_set, "set_available('id0') did not restore is_set")
        if (allocated(error)) return

        call cinfo%set_unavailable()
        call check(error, all(.not. cinfo%col(:)%is_set), &
            "set_unavailable() (no name) did not clear is_set for every column")
        if (allocated(error)) return

        call cinfo%set_available()
        call check(error, all(cinfo%col(:)%is_set), &
            "set_available() (no name) did not set is_set for every column")
    end subroutine test_set_available_unavailable

    !> maml%add_col_qc builds a read-time qc-maml incrementally from compact
    !> "col, min, max, miss" strings: positional fields, empty tokens skipped,
    !> the returned col_name, and the exact generated lines (fields: header
    !> created once, quoted min/max, name-only entry with no qc: block).
    subroutine test_add_col_qc_builds_maml(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: cn

        call maml%add_col_qc("ra, >0, <=360, Null", cn)
        call check(error, cn == "ra", "add_col_qc did not return the parsed column name")
        if (allocated(error)) return

        call maml%add_col_qc("dec, , <=90,", cn)   ! max only
        call check(error, cn == "dec", "add_col_qc did not return 'dec'")
        if (allocated(error)) return

        call maml%add_col_qc("mag, 5", cn)         ! bare numeric min
        if (allocated(error)) return
        call maml%add_col_qc("flag", cn)           ! name only, no qc: block

        ! Exact expected lines (order of appends preserved). The name-only
        ! "flag" entry adds just a "- name:" line, with no qc: block.
        call check(error, size(maml%lines) == 13, "add_col_qc produced an unexpected number of lines")
        if (allocated(error)) return
        call check(error, &
            trim(maml%lines(1))  == "fields:"           .and. &
            trim(maml%lines(2))  == "- name: ra"        .and. &
            trim(maml%lines(3))  == "  qc:"             .and. &
            trim(maml%lines(4))  == "    min: '>0'"     .and. &
            trim(maml%lines(5))  == "    max: '<=360'"  .and. &
            trim(maml%lines(6))  == "    miss: Null"    .and. &
            trim(maml%lines(7))  == "- name: dec"       .and. &
            trim(maml%lines(8))  == "  qc:"             .and. &
            trim(maml%lines(9))  == "    max: '<=90'"   .and. &
            trim(maml%lines(10)) == "- name: mag"       .and. &
            trim(maml%lines(11)) == "  qc:"             .and. &
            trim(maml%lines(12)) == "    min: '5'"      .and. &
            trim(maml%lines(13)) == "- name: flag", &
            "add_col_qc generated unexpected qc-maml lines")
    end subroutine test_add_col_qc_builds_maml

    !> A qc-maml built purely with add_col_qc is accepted by parquet_open_reader
    !> and drives the read-side qc checks: ra in the list_vector fixture is
    !> [1.5,2.5,3.5,4.5], within [>=0, <=10], so no violation occurs and the
    !> column reads back correctly.
    subroutine test_add_col_qc_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        type(parquet_reader) :: reader
        character(len=:), allocatable :: cn
        integer :: nrows
        real(real64), allocatable :: ra(:)

        call maml%add_col_qc("ra, >=0, <=10", cn)

        call parquet_open_reader(reader, "test/fixtures/list_vector.parquet", maml=maml)
        call parquet_get_nrows(reader, nrows)
        allocate(ra(nrows))
        call parquet_read_column(reader, cn, ra)
        call parquet_close_reader(reader)

        call check(error, nrows == 4 .and. abs(ra(1) - 1.5_real64) < 1.0e-12_real64 .and. &
            abs(ra(4) - 4.5_real64) < 1.0e-12_real64, &
            "add_col_qc-built qc-maml did not read back the ra column correctly")
    end subroutine test_add_col_qc_roundtrip

    !> An empty (or all-blank) qc_input is an explicit no-op: col_name comes
    !> back empty and the maml's lines are left untouched. A subsequent real
    !> add_col_qc call on the same maml still works and creates the fields:
    !> header itself (i.e. the no-op left nothing half-initialized).
    subroutine test_add_col_qc_empty_input_is_noop(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_maml_file) :: maml
        character(len=:), allocatable :: cn

        call maml%add_col_qc("", cn)
        call check(error, len(cn) == 0, "add_col_qc('') should return an empty col_name")
        if (allocated(error)) return
        call check(error, .not. allocated(maml%lines), "add_col_qc('') should not add any lines")
        if (allocated(error)) return

        ! blank (whitespace-only) input is treated the same way.
        call maml%add_col_qc("   ", cn)
        call check(error, len(cn) == 0 .and. .not. allocated(maml%lines), &
            "add_col_qc('   ') should be a no-op too")
        if (allocated(error)) return

        ! a real call afterwards behaves normally and builds from scratch.
        call maml%add_col_qc("ra, >0", cn)
        call check(error, cn == "ra" .and. allocated(maml%lines) .and. size(maml%lines) == 4 .and. &
            trim(maml%lines(1)) == "fields:" .and. trim(maml%lines(2)) == "- name: ra", &
            "a real add_col_qc after a no-op did not build correctly")
    end subroutine test_add_col_qc_empty_input_is_noop
    !
end module test_maml
