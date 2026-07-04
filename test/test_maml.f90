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
            new_unittest("load a MAML file from disk", test_load_maml_file), &
            new_unittest("get_column_index finds an existing column", test_get_column_index_found), &
            new_unittest("set_unavailable/set_available toggle is_set", test_set_available_unavailable) &
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
        call check(error, user_cinfo%col(idx)%is_set .and. .not. user_cinfo%col(idx)%deactivated, &
            "expected 'id0' to be active (declared by the user MAML)")
        if (allocated(error)) return

        idx = user_cinfo%get_column_index("idarr")
        call check(error, (.not. user_cinfo%col(idx)%is_set) .and. user_cinfo%col(idx)%deactivated, &
            "expected 'idarr' to be merged in as a deactivated placeholder")
    end subroutine test_validate_user_maml_ok

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
    !
end module test_maml
