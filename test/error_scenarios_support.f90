!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Fixtures and helpers used by more than one `error_scenarios_*` group module.
!!
!! Everything here was an internal procedure of `error_scenarios.f90` before that program
!! was split into four group modules (see `error_scenarios_io.f90` for why). A helper used
!! by exactly one group stayed with it and is private there; these eight have callers in
!! two or more groups, and a second copy of a fixture writer is how two groups start
!! disagreeing about what the fixture holds.
module error_scenarios_support
    use parquet
    use parquet_columns
    use parquet_tables
    use iso_fortran_env, only : int32, int64, real64
    implicit none
    private

    public :: join_fixture, multitype_vector_schema, scenario_setenv, spatial_sky_cloud, write_list_scenario_fixture, &
        write_print_rows_fixture, write_scenario_maml_file, write_text_file

contains

    !> Schema shared by the write_undeclared_column_*/write_not_divisible_*/
    !> write_array_mismatch_* scenarios: one col_size=3 vector field per
    !> supported type, so each scenario just needs to write badly-named or
    !> badly-shaped data to it (idx==0 / not-divisible / array-mismatch
    !> checks are otherwise identical copy-pasted logic per type, only ever
    !> exercised for int32 elsewhere).
    function multitype_vector_schema() result(schema)
        type(parquet_schema) :: schema

        call schema%init(table="multitype_table")
        call schema%add_field("i32", "int32", col_size=3)
        call schema%add_field("i64", "int64", col_size=3)
        call schema%add_field("f32", "float32", col_size=3)
        call schema%add_field("f64", "float64", col_size=3)
        call schema%add_field("lg", "boolean", col_size=3)
        call schema%add_field("str", "string", col_size=3, array_size=8)
    end function multitype_vector_schema

    !> A six-row table for the %print_rows guard scenarios to be called on. One fixture file per
    !> scenario, because the runner dispatches them concurrently.
    subroutine write_print_rows_fixture(file)
        character(len=*), intent(in) :: file !! this scenario's own fixture path.
        type(parquet_writer) :: w
        integer(int32) :: id(6)
        integer :: i

        do i = 1, 6
            id(i) = int(i, int32)
        end do
        call parquet_open_writer(w, file)
        call parquet_write_column(w, "id", id)
        call parquet_close_writer(w)
    end subroutine write_print_rows_fixture

    !> Writes the small numeric fixture the literal-list scenarios filter against. Each caller
    !> passes its own path: the scenarios run concurrently under xargs -P, so a shared fixture path
    !> is a truncated file for whichever process reads while another writes.
    subroutine write_list_scenario_fixture(file)
        character(len=*), intent(in) :: file !! this scenario's own fixture path.
        type(parquet_writer) :: writer

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", [1_int32, 2_int32, 3_int32, 4_int32])
        call parquet_write_column(writer, "x", [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64])
        call parquet_write_column(writer, "flag", [.true., .false., .true., .false.])
        call parquet_write_column(writer, "name", ["a   ", "b   ", "c   ", "d   "])
        call parquet_close_writer(writer)
    end subroutine write_list_scenario_fixture

    !> Writes `lines` verbatim to `path`, one per record -- used by the
    !> read-time qc scenarios to produce a throwaway qc-maml file
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

    !> Sets an environment variable for this process. Fortran cannot, so this is POSIX `setenv`
    !> through a local bind(C) interface -- test-only, exactly as every parquet_debug_* hook here is,
    !> so no src/ file gains a POSIX dependency. Shared by the six env scenarios.
    subroutine scenario_setenv(name, value)
        use iso_c_binding, only : c_int
        character(len=*), intent(in) :: name !! variable to set.
        character(len=*), intent(in) :: value !! its value.
        interface
            function c_setenv(nm, val, overwrite) bind(C, name="setenv") result(rc)
                use iso_c_binding, only : c_char, c_int
                character(kind=c_char), intent(in) :: nm(*) !! NUL-terminated name.
                character(kind=c_char), intent(in) :: val(*) !! NUL-terminated value.
                integer(c_int), value :: overwrite !! nonzero replaces an existing value.
                integer(c_int) :: rc !! 0 on success.
            end function c_setenv
        end interface
        integer :: rc

        rc = int(c_setenv(name // char(0), value // char(0), int(1, kind=c_int)))
        ! Checked rather than discarded: a failed setenv leaves the old value in place, and the
        ! scenario would then exercise an environment nobody set.
        if (rc /= 0) error stop "set_env: setenv failed for '" // name // "'"
    end subroutine scenario_setenv

    !> Writes `lines` to `fname`, one per record -- the read-in (Role-B) MAML fixtures the remap
    !! scenarios open a table with (parquet_open_table(maml=) takes a file path).
    subroutine write_scenario_maml_file(fname, lines)
        character(len=*), intent(in) :: fname    !! file to write.
        character(len=*), intent(in) :: lines(:) !! MAML source, one array element per line.
        integer :: unit, i
        open(newunit=unit, file=fname, status="replace", action="write")
        do i = 1, size(lines)
            write(unit, "(a)") trim(lines(i))
        end do
        close(unit)
    end subroutine write_scenario_maml_file

    subroutine spatial_sky_cloud(n, ra, dec)
        integer, intent(in) :: n !! how many points.
        real(real64), allocatable, intent(out) :: ra(:) !! right ascension, degrees.
        real(real64), allocatable, intent(out) :: dec(:) !! declination, degrees.
        integer :: i

        allocate (ra(n), dec(n))
        do i = 1, n
            ra(i) = 360.0_real64 * pf_random_at(4321_int64, i, 1_int64)
            dec(i) = -20.0_real64 + 40.0_real64 * pf_random_at(4321_int64, i, 2_int64)
        end do
    end subroutine spatial_sky_cloud

    !> Builds the two-column table every join_* scenario joins: an int64 "id" plus a payload.
    subroutine join_fixture(t, keys)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int64), intent(in) :: keys(:) !! the key values.
        integer(int64), allocatable :: payload(:)
        integer(int64) :: k
        allocate(payload(size(keys)))
        do k = 1_int64, size(keys, kind=int64)
            payload(k) = k
        end do
        call parquet_new_table(t)
        call t%add_column("id", keys)
        call t%add_column("payload", payload)
    end subroutine join_fixture

end module error_scenarios_support
