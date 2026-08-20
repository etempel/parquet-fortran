!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for a GENERATED table type: `parquet_table_test`, emitted from
!! `table_types/maml_example4.maml` by `tools/generate_user_table_code.py` into
!! `src/parquet_table_example.f90`.
!!
!! The generated module is committed and `--check`ed in CI, so this suite tests the code a
!! downstream project would actually compile, not a hand-written approximation of it. Two things
!! it is deliberately arranged to prove:
!!
!! * **every emitted accessor shape works** -- scalar and vector, all nine data types, the whole
!!   column / one row / a row range forms, both index kinds, and the two string forms. A generator
!!   bug that only affects one shape has nowhere to hide;
!! * **the user windows do their job** -- `src/parquet_table_example.f90`'s own `components` window
!!   carries a test-only parameter, so the round trip through `clone_extra`/`init_extra` that
!!   `%clone` and `%init` depend on is exercised rather than described.
!!
!! Abort paths live in test/error_scenarios.f90 as `codegen_*` scenarios, since they kill the
!! process.
!!
!! Every test writes its OWN fixture file: test-drive runs these concurrently, so a shared path
!! would let one test truncate the file another is reading (CLAUDE.md, "Tests run concurrently").
module test_table_codegen
    use parquet
    use parquet_table_example, only : parquet_table_test
    use parquet_columns, only : PK_INT64, PK_FLOAT64, PK_FLOAT32_VEC, PK_STRING
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_codegen
    !
    !> Rows every fixture in this suite writes.
    integer, parameter :: NROW = 6
    !
contains
    !
    subroutine collect_tests_table_codegen(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("init binds every predefined column and materializes it", &
                test_init_binds_everything), &
            new_unittest("the whole-column accessors have the right kind, rank and values", &
                test_whole_column_accessors), &
            new_unittest("the indexed and range forms agree with the whole-column form", &
                test_indexed_accessors), &
            new_unittest("writing through an accessor pointer reaches the column", &
                test_accessor_writes_through), &
            new_unittest("the string forms agree, and a string vector has only the _chr form", &
                test_string_accessors), &
            new_unittest("a computed column exists with every row null", &
                test_computed_column), &
            new_unittest("clone carries the columns and the user's own component", &
                test_clone_carries_user_state), &
            new_unittest("init resets the user's own component through init_extra", &
                test_init_resets_user_state), &
            new_unittest("init_empty builds the same columns with no file behind them", &
                test_init_empty), &
            new_unittest("init_slice covers its row range and binds the same columns", &
                test_init_slice), &
            new_unittest("a generated table writes itself and survives inherited mutations", &
                test_write_and_mutate), &
            new_unittest("every numeric/temporal field's five accessor forms agree", &
                test_every_field_every_form), &
            new_unittest("every constructor and both index kinds of init_empty are reachable", &
                test_every_constructor) &
            ]
    end subroutine collect_tests_table_codegen
    !
    !> Writes a fixture matching `table_types/maml_example4.maml`'s file columns.
    !!
    !! Schema-less on purpose: what this suite tests is the generated code, and a schema-less
    !! write produces exactly the kinds the MAML declares for every column here. `flux` is absent
    !! because it is `source: computed` -- the generated type creates it rather than reading it,
    !! which is the whole point of that declaration.
    subroutine write_codegen_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write (one per test).
        type(parquet_writer) :: w
        integer(int64) :: uberid(NROW)
        integer(int32) :: idx(NROW)
        logical :: flag(NROW)
        character(len=5) :: name(NROW)
        real(real64) :: ra(NROW), dec(NROW)
        real(real32) :: crd(3, NROW)
        integer(int32) :: counts(2, NROW)
        logical :: passed(2, NROW)
        type(parquet_date) :: obsdate(NROW)
        type(parquet_time) :: obstime(NROW)
        type(parquet_timestamp) :: obsstamp(NROW)
        character(len=4) :: tags(2, NROW)
        integer :: i, e
        !
        do i = 1, NROW
            uberid(i) = int(i, int64) * 1000000000_int64
            idx(i) = i
            flag(i) = mod(i, 2) == 1
            ra(i) = real(i, real64) * 15.0_real64
            dec(i) = real(i, real64) * (-3.5_real64)
            obsdate(i) = parquet_date(2026, 3, i)
            obstime(i) = parquet_time(10, 20, i)
            obsstamp(i) = parquet_timestamp(2026, 3, i, 1, 2, 3)
            do e = 1, 3
                crd(e, i) = real(i * 10 + e, real32) * 0.5_real32
            end do
            do e = 1, 2
                counts(e, i) = i * 100 + e
                passed(e, i) = mod(i + e, 2) == 0
            end do
        end do
        ! First element deliberately the shortest, in both string fixtures (CLAUDE.md).
        name = ["a    ", "bcd  ", "ef   ", "ghijk", "no   ", "p    "]
        tags(1, :) = "a"
        tags(2, :) = "bcde"
        !
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "uberid", uberid)
        call parquet_write_column(w, "idx", idx)
        call parquet_write_column(w, "flag", flag)
        call parquet_write_column(w, "name", name)
        call parquet_write_column(w, "ra", ra)
        call parquet_write_column(w, "dec", dec)
        call parquet_write_column(w, "crd", crd)
        call parquet_write_column(w, "counts", counts)
        call parquet_write_column(w, "passed", passed)
        call parquet_write_column(w, "obsdate", obsdate)
        call parquet_write_column(w, "obstime", obstime)
        call parquet_write_column(w, "obsstamp", obsstamp)
        call parquet_write_column(w, "tags", tags)
        call parquet_close_writer(w)
    end subroutine write_codegen_fixture
    !
    !> Every predefined column is present and fully read by the time `%init` returns.
    subroutine test_init_binds_everything(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        character(len=*), parameter :: f = "test_run/codegen_init.parquet"
        character(len=:), allocatable :: names(:)
        integer :: i
        !
        call write_codegen_fixture(f)
        call t%init(f)
        call check(error, t%nrows() == NROW, "the generated table should report the file's rows")
        if (allocated(error)) return
        ! 13 file columns plus the one computed column the schema declares.
        call check(error, t%ncols() == 14, "every declared column should have a slot")
        if (allocated(error)) return
        call t%column_names(names)
        do i = 1, size(names)
            call check(error, t%residency(trim(names(i))) == RES_FULL, &
                "%init should leave every predefined column materialized: " // trim(names(i)))
            if (allocated(error)) return
        end do
    end subroutine test_init_binds_everything
    !
    !> The whole-column accessor of every emitted shape returns the right kind, rank and values.
    subroutine test_whole_column_accessors(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        character(len=*), parameter :: f = "test_run/codegen_whole.parquet"
        integer(int64), pointer :: p_uberid(:)
        integer(int32), pointer :: p_idx(:), p_counts(:,:)
        logical, pointer :: p_flag(:), p_passed(:,:)
        real(real64), pointer :: p_ra(:), p_dec(:)
        real(real32), pointer :: p_crd(:,:)
        type(parquet_date), pointer :: p_date(:)
        type(parquet_time), pointer :: p_time(:)
        type(parquet_timestamp), pointer :: p_ts(:)
        character(len=:), allocatable :: s
        integer :: i
        !
        call write_codegen_fixture(f)
        call t%init(f)
        p_uberid => t%uberid()
        p_idx => t%idx()
        p_flag => t%flag()
        p_ra => t%ra()
        p_dec => t%dec()
        p_crd => t%crd()
        p_counts => t%counts()
        p_passed => t%passed()
        p_date => t%obsdate()
        p_time => t%obstime()
        p_ts => t%obsstamp()
        call check(error, size(p_uberid) == NROW .and. size(p_idx) == NROW, &
            "a scalar accessor should return one value per row")
        if (allocated(error)) return
        call check(error, all(shape(p_crd) == [3, NROW]), &
            "a width-3 accessor should return an (element, row) array")
        if (allocated(error)) return
        call check(error, all(shape(p_counts) == [2, NROW]) .and. all(shape(p_passed) == [2, NROW]), &
            "a width-2 accessor should return an (element, row) array")
        if (allocated(error)) return
        call check(error, all(p_uberid == [(int(i, int64) * 1000000000_int64, i = 1, NROW)]), &
            "the int64 accessor should hold the file's values")
        if (allocated(error)) return
        call check(error, all(p_idx == [(int(i, int32), i = 1, NROW)]), &
            "the int32 accessor should hold the file's values")
        if (allocated(error)) return
        call check(error, all(p_flag .eqv. [(mod(i, 2) == 1, i = 1, NROW)]), &
            "the boolean accessor should hold the file's values")
        if (allocated(error)) return
        call check(error, all(abs(p_ra - [(real(i, real64) * 15.0_real64, i = 1, NROW)]) &
            < 1.0e-12_real64), "the float64 accessor should hold the file's values")
        if (allocated(error)) return
        call check(error, all(abs(p_dec - [(real(i, real64) * (-3.5_real64), i = 1, NROW)]) &
            < 1.0e-12_real64), "the second float64 accessor should hold its own column")
        if (allocated(error)) return
        call check(error, abs(p_crd(2, 3) - 32.0_real32 * 0.5_real32) < 1.0e-5_real32, &
            "the float32 vector accessor should hold the file's values")
        if (allocated(error)) return
        call check(error, p_counts(2, 4) == 402, &
            "the int32 vector accessor should hold the file's values")
        if (allocated(error)) return
        call check(error, p_date(3)%year() == 2026 .and. p_date(3)%day() == 3, &
            "the date accessor should hold the file's values")
        if (allocated(error)) return
        call check(error, p_time(3)%second() == 3, "the time accessor should hold the file's values")
        if (allocated(error)) return
        call p_ts(3)%to_string(s)
        call check(error, s(1:10) == "2026-03-03", "the timestamp accessor should hold the file's values")
    end subroutine test_whole_column_accessors
    !
    !> `%x(i)` and `%x(lo,hi)` agree with `%x()`, in both index kinds, on scalar and vector columns.
    !!
    !! The index is a ROW index on every column: `%crd(3)` is row 3's three values, not element 3
    !! of every row. That is the rule the whole indexed family rests on -- the alternative cannot
    !! coexist with the range form, since `%crd(3,6)` would then mean two different things.
    subroutine test_indexed_accessors(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        character(len=*), parameter :: f = "test_run/codegen_indexed.parquet"
        real(real64), pointer :: whole(:), one, rng(:)
        real(real32), pointer :: vwhole(:,:), vone(:), vrng(:,:)
        !
        call write_codegen_fixture(f)
        call t%init(f)
        whole => t%ra()
        one => t%ra(3)
        call check(error, abs(one - whole(3)) < 1.0e-12_real64, &
            "%ra(3) should be the same value as %ra()(3)")
        if (allocated(error)) return
        one => t%ra(3_int64)
        call check(error, abs(one - whole(3)) < 1.0e-12_real64, &
            "the int64 index form should agree with the int32 one")
        if (allocated(error)) return
        rng => t%ra(2, 5)
        call check(error, size(rng) == 4, "%ra(2,5) should cover four rows")
        if (allocated(error)) return
        call check(error, all(abs(rng - whole(2:5)) < 1.0e-12_real64), &
            "%ra(2,5) should be the same values as %ra()(2:5)")
        if (allocated(error)) return
        rng => t%ra(2_int64, 5_int64)
        call check(error, all(abs(rng - whole(2:5)) < 1.0e-12_real64), &
            "the int64 range form should agree with the int32 one")
        if (allocated(error)) return
        ! On a vector column the index is still a ROW index: one row's full width.
        vwhole => t%crd()
        vone => t%crd(4)
        call check(error, size(vone) == 3, "%crd(4) should be row 4's three values")
        if (allocated(error)) return
        call check(error, all(abs(vone - vwhole(:, 4)) < 1.0e-5_real32), &
            "%crd(4) should be the same values as %crd()(:,4)")
        if (allocated(error)) return
        vrng => t%crd(2, 4)
        call check(error, all(shape(vrng) == [3, 3]), &
            "%crd(2,4) should be three full-width rows")
        if (allocated(error)) return
        call check(error, all(abs(vrng - vwhole(:, 2:4)) < 1.0e-5_real32), &
            "%crd(2,4) should be the same values as %crd()(:,2:4)")
        if (allocated(error)) return
        ! An EMPTY range is accepted and yields a zero-length pointer, matching Fortran's own
        ! section rules -- the bounds guard returns before it can complain about `hi`.
        rng => t%ra(3, 2)
        call check(error, size(rng) == 0, "%ra(3,2) should be an empty, not an invalid, range")
        if (allocated(error)) return
        vrng => t%crd(3, 2)
        call check(error, size(vrng, 2) == 0, &
            "an empty range on a vector column should be empty too")
    end subroutine test_indexed_accessors
    !
    !> An accessor aliases the live storage, so writing through it changes the column.
    subroutine test_accessor_writes_through(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        character(len=*), parameter :: f = "test_run/codegen_write_through.parquet"
        real(real64), allocatable :: got(:)
        real(real64), pointer :: one
        !
        call write_codegen_fixture(f)
        call t%init(f)
        t%ra() = 0.0_real64
        one => t%ra(2)
        one = 42.5_real64
        call t%get("ra", got)
        call check(error, abs(got(1)) < 1.0e-12_real64, &
            "assigning through the whole-column accessor should reach the column")
        if (allocated(error)) return
        call check(error, abs(got(2) - 42.5_real64) < 1.0e-12_real64, &
            "assigning through the indexed accessor should reach the column")
    end subroutine test_accessor_writes_through
    !
    !> The two string forms agree; a string VECTOR column has only the character form, because
    !! `%col` has no PK_STRING_VEC specific to alias.
    subroutine test_string_accessors(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        character(len=*), parameter :: f = "test_run/codegen_string.parquet"
        type(parquet_string_column), pointer :: store
        type(parquet_string) :: h
        type(parquet_string), allocatable :: hs(:)
        character(len=:), allocatable :: chr(:), chrv(:,:), s
        !
        call write_codegen_fixture(f)
        call t%init(f)
        store => t%name()
        call check(error, store%size() == NROW, "the packed store should hold every row")
        if (allocated(error)) return
        call t%name_chr(chr)
        call check(error, size(chr) == NROW, "the character form should hold every row")
        if (allocated(error)) return
        call check(error, trim(chr(4)) == "ghijk", &
            "the character form should hold the file's values, longest element included")
        if (allocated(error)) return
        h = t%name(4)
        call h%to_string(s)
        call check(error, s == "ghijk", "the indexed handle should view the same value")
        if (allocated(error)) return
        hs = t%name(2, 4)
        call check(error, size(hs) == 3, "the range form should return one handle per row")
        if (allocated(error)) return
        call hs(1)%to_string(s)
        call check(error, s == "bcd", "the range form's handles should be in row order")
        if (allocated(error)) return
        ! The string VECTOR column: only the _chr form exists for it.
        call t%tags_chr(chrv)
        call check(error, all(shape(chrv) == [2, NROW]), &
            "a string vector column's character form should be (element, row)")
        if (allocated(error)) return
        call check(error, trim(chrv(2, 1)) == "bcde", &
            "a string vector column should hold the file's values")
    end subroutine test_string_accessors
    !
    !> A `source: computed` column is created rather than read, with every row null.
    subroutine test_computed_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        character(len=*), parameter :: f = "test_run/codegen_computed.parquet"
        real(real64), pointer :: p(:)
        character(len=:), allocatable :: u
        integer :: i
        !
        call write_codegen_fixture(f)
        call t%init(f)
        call check(error, t%has_column("flux"), "the computed column should exist")
        if (allocated(error)) return
        p => t%flux()
        call check(error, size(p) == NROW, "the computed column should have one row per table row")
        if (allocated(error)) return
        do i = 1, NROW
            call check(error, t%is_null("flux", i), &
                "every row of an unfilled computed column should be null")
            if (allocated(error)) return
        end do
        ! It behaves like any other column once filled.
        p = 1.5_real64
        do i = 1, NROW
            call t%clear_null("flux", i)
        end do
        call check(error, .not. t%is_null("flux", 3), &
            "a filled computed column should stop reporting nulls")
        if (allocated(error)) return
        call t%unit("flux", u)
        call check(error, u == "Jy", "a computed column should carry the unit its schema declares")
    end subroutine test_computed_column
    !
    !> `%clone` carries the columns AND the parameter the generated type's user window declares.
    !!
    !! The assignment that makes this work is written by the generator into `clone_extra`, from
    !! the `components` window -- so this test covers the generator's component parser as much as
    !! the hook itself. Mutation-test it by deleting the generated assignment: this must fail.
    subroutine test_clone_carries_user_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t, c, batch
        character(len=*), parameter :: f = "test_run/codegen_clone.parquet"
        integer(int32), allocatable :: got(:)
        !
        call write_codegen_fixture(f)
        call t%init(f)
        t%zeropoint = 12.25_real64
        call t%clone(c)
        call check(error, c%nrows() == NROW, "the clone should hold the source's rows")
        if (allocated(error)) return
        call c%get("idx", got)
        call check(error, got(3) == 3_int32, "the clone's columns should hold the source's values")
        if (allocated(error)) return
        call check(error, abs(c%zeropoint - 12.25_real64) < 1.0e-12_real64, &
            "the generated clone_extra should carry the user's own component across")
        if (allocated(error)) return
        call t%clone_structure(batch)
        call check(error, batch%nrows() == 0, "a structure clone should have no rows")
        if (allocated(error)) return
        call check(error, abs(batch%zeropoint - 12.25_real64) < 1.0e-12_real64, &
            "a structure clone should carry the user's own component too")
    end subroutine test_clone_carries_user_state
    !
    !> `%init` RESETS the user's own components, through the generated `init_extra`.
    !!
    !! This is the opposite of Fortran's own behaviour for the parent-component open the
    !! constructor performs (which resets only `parquet_table`'s half), so it is a deliberate
    !! choice and needs a test of its own: set the parameter, re-init onto another file, and it
    !! must be back at its declared default.
    subroutine test_init_resets_user_state(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        character(len=*), parameter :: f = "test_run/codegen_reset_a.parquet"
        character(len=*), parameter :: g = "test_run/codegen_reset_b.parquet"
        !
        call write_codegen_fixture(f)
        call write_codegen_fixture(g)
        call t%init(f)
        t%zeropoint = 99.0_real64
        call t%init(g)
        call check(error, abs(t%zeropoint) < 1.0e-12_real64, &
            "%init should reset the user's own component to its declared default")
    end subroutine test_init_resets_user_state
    !
    !> `%init_empty` builds the same columns with no file behind them.
    subroutine test_init_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t, n
        real(real64), pointer :: p(:)
        integer :: i
        !
        call t%init_empty()
        call check(error, t%nrows() == 0, "an empty generated table should have no rows")
        if (allocated(error)) return
        call check(error, t%ncols() == 14, "an empty generated table should still have every column")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), &
            "a table that never had a file must not report itself detached")
        if (allocated(error)) return
        call check(error, t%kind("uberid") == PK_INT64 .and. t%kind("ra") == PK_FLOAT64 .and. &
            t%kind("crd") == PK_FLOAT32_VEC .and. t%kind("name") == PK_STRING, &
            "an empty generated table's columns should have their declared kinds")
        if (allocated(error)) return
        call check(error, t%width("crd") == 3, "a vector column should keep its declared width")
        if (allocated(error)) return
        ! The row-count form pre-creates every column with null rows, ready to be filled.
        call n%init_empty(4)
        call check(error, n%nrows() == 4, "%init_empty(n) should create n rows")
        if (allocated(error)) return
        p => n%ra()
        call check(error, size(p) == 4, "%init_empty(n) should size every column to n")
        if (allocated(error)) return
        do i = 1, 4
            call check(error, n%is_null("ra", i), "%init_empty(n) should leave every row null")
            if (allocated(error)) return
        end do
    end subroutine test_init_empty
    !
    !> `%init_slice` covers its row range and binds the same columns.
    subroutine test_init_slice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        character(len=*), parameter :: f = "test_run/codegen_slice.parquet"
        integer(int32), pointer :: p(:)
        !
        call write_codegen_fixture(f)
        call t%init_slice(f, 2, 4)
        call check(error, t%nrows() == 3, "a slice should cover its own row range")
        if (allocated(error)) return
        call check(error, t%ncols() == 14, "a slice should bind every declared column")
        if (allocated(error)) return
        p => t%idx()
        call check(error, all(p == [2_int32, 3_int32, 4_int32]), &
            "a slice's accessor should hold the slice's own rows")
        if (allocated(error)) return
        call t%init_slice(f, 3_int64, 5_int64)
        p => t%idx()
        call check(error, all(p == [3_int32, 4_int32, 5_int32]), &
            "the int64 slice form should behave like the int32 one")
    end subroutine test_init_slice
    !
    !> A generated table is an ordinary `parquet_table` for everything it inherits.
    subroutine test_write_and_mutate(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        type(parquet_table) :: back
        character(len=*), parameter :: f = "test_run/codegen_rw_src.parquet"
        character(len=*), parameter :: g = "test_run/codegen_rw_out.parquet"
        integer(int32), allocatable :: got(:)
        integer(int32), pointer :: p(:)
        logical :: keep(NROW)
        integer :: i
        !
        call write_codegen_fixture(f)
        call t%init(f)
        ! parquet_write_table takes class(parquet_table), so a generated table needs no unwrapping.
        call parquet_write_table(t, g, overwrite=.true.)
        call parquet_open_table(back, g)
        call back%get("idx", got)
        call check(error, all(got == [(int(i, int32), i = 1, NROW)]), &
            "a generated table should write itself unchanged")
        if (allocated(error)) return
        ! An inherited row-structural mutation works, and invalidates the accessor pointers --
        ! which is why the pointer is taken again afterwards rather than reused.
        keep = [(mod(i, 2) == 1, i = 1, NROW)]
        call t%filter_rows(keep)
        call check(error, t%nrows() == 3, "an inherited filter should reduce the row count")
        if (allocated(error)) return
        p => t%idx()
        call check(error, all(p == [1_int32, 3_int32, 5_int32]), &
            "an accessor taken after a mutation should see the surviving rows")
    end subroutine test_write_and_mutate
    !
    !> Calls all five accessor forms on EVERY field the schema declares, not one representative
    !! per shape class.
    !!
    !! The generated module lives in `src/`, so it is measured by `tools/coverage.sh` like the rest
    !! of the library -- and a generator bug that reaches only, say, the int64 range form of a
    !! logical vector column has nowhere to hide if every specific is called. Each form is checked
    !! against the whole-column form, which is the one an earlier test already pinned to the file's
    !! values, so this is a consistency sweep rather than a re-assertion of the data.
    subroutine test_every_field_every_form(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t
        character(len=*), parameter :: f = "test_run/codegen_all_forms.parquet"
        integer(int64), pointer :: w_i64(:), o_i64, r_i64(:)
        integer(int32), pointer :: w_i32(:), o_i32, r_i32(:)
        logical, pointer :: w_b(:), o_b, r_b(:)
        real(real64), pointer :: w_f64(:), o_f64, r_f64(:), o2_f64, r2_f64(:)
        real(real32), pointer :: w_f32v(:,:), o_f32v(:), r_f32v(:,:)
        integer(int32), pointer :: w_i32v(:,:), o_i32v(:), r_i32v(:,:)
        logical, pointer :: w_bv(:,:), o_bv(:), r_bv(:,:)
        type(parquet_date), pointer :: w_d(:), o_d, r_d(:)
        type(parquet_time), pointer :: w_tm(:), o_tm, r_tm(:)
        type(parquet_timestamp), pointer :: w_ts(:), o_ts, r_ts(:)
        integer, parameter :: LO = 2, HI = 5
        ! `parquet_timestamp` has no null-safe elemental value FUNCTION (`to_unix`/`to_mjd` abort on
        ! a null operand, as does `==`), so its values are compared through the elemental
        ! `%get_raw` SUBROUTINE, which yields zeros for a null element rather than aborting.
        integer(int64) :: ts_sec_a(HI - LO + 1), ts_sec_b(HI - LO + 1), ts_sec1, ts_sec2
        integer(int32) :: ts_ns_a(HI - LO + 1), ts_ns_b(HI - LO + 1), ts_ns1, ts_ns2
        integer :: i
        !
        call write_codegen_fixture(f)
        call t%init(f)
        !
        ! --- int64 scalar: uberid
        w_i64 => t%uberid()
        o_i64 => t%uberid(3);            call check(error, o_i64 == w_i64(3), "uberid(3)")
        if (allocated(error)) return
        o_i64 => t%uberid(3_int64);      call check(error, o_i64 == w_i64(3), "uberid(3_int64)")
        if (allocated(error)) return
        r_i64 => t%uberid(LO, HI);       call check(error, all(r_i64 == w_i64(LO:HI)), "uberid(lo,hi)")
        if (allocated(error)) return
        r_i64 => t%uberid(int(LO, int64), int(HI, int64))
        call check(error, all(r_i64 == w_i64(LO:HI)), "uberid(lo,hi) int64")
        if (allocated(error)) return
        !
        ! --- int32 scalar: idx
        w_i32 => t%idx()
        o_i32 => t%idx(3);               call check(error, o_i32 == w_i32(3), "idx(3)")
        if (allocated(error)) return
        o_i32 => t%idx(3_int64);         call check(error, o_i32 == w_i32(3), "idx(3_int64)")
        if (allocated(error)) return
        r_i32 => t%idx(LO, HI);          call check(error, all(r_i32 == w_i32(LO:HI)), "idx(lo,hi)")
        if (allocated(error)) return
        r_i32 => t%idx(int(LO, int64), int(HI, int64))
        call check(error, all(r_i32 == w_i32(LO:HI)), "idx(lo,hi) int64")
        if (allocated(error)) return
        !
        ! --- logical scalar: flag
        w_b => t%flag()
        o_b => t%flag(3);                call check(error, o_b .eqv. w_b(3), "flag(3)")
        if (allocated(error)) return
        o_b => t%flag(3_int64);          call check(error, o_b .eqv. w_b(3), "flag(3_int64)")
        if (allocated(error)) return
        r_b => t%flag(LO, HI);           call check(error, all(r_b .eqv. w_b(LO:HI)), "flag(lo,hi)")
        if (allocated(error)) return
        r_b => t%flag(int(LO, int64), int(HI, int64))
        call check(error, all(r_b .eqv. w_b(LO:HI)), "flag(lo,hi) int64")
        if (allocated(error)) return
        !
        ! --- float64 scalars: ra, dec, and the computed flux
        ! Each form is bound to a local pointer before the call, rather than passed inline as
        ! `t%ra()`. That matches how every other column is checked below, and it is also required:
        ! nagfor 7.2 emits invalid C under -C=undefined for a POINTER-valued function result used
        ! directly as an actual argument (16-line reproducer in feature_nag_ice_scope_id.md).
        w_f64  => t%ra()
        o_f64  => t%ra(3)
        o2_f64 => t%ra(3_int64)
        r_f64  => t%ra(LO, HI)
        r2_f64 => t%ra(int(LO, int64), int(HI, int64))
        call check_f64_forms(error, w_f64, o_f64, o2_f64, r_f64, r2_f64, "ra")
        if (allocated(error)) return
        w_f64  => t%dec()
        o_f64  => t%dec(3)
        o2_f64 => t%dec(3_int64)
        r_f64  => t%dec(LO, HI)
        r2_f64 => t%dec(int(LO, int64), int(HI, int64))
        call check_f64_forms(error, w_f64, o_f64, o2_f64, r_f64, r2_f64, "dec")
        if (allocated(error)) return
        ! `flux` is `source: computed`, so the generated type creates it with every row null and
        ! nothing has written a value into it. A null numeric row's VALUE bytes are unspecified by
        ! design (see `grow_rows` in parquet_columns_structural.f90), and every assertion below
        ! compares one accessor form against another -- i.e. undefined memory against ITSELF. That
        ! holds for ordinary garbage, since x - x is 0, but NOT for a NaN: NaN - NaN is NaN and
        ! every comparison against it is false, so the sweep failed whenever the allocator happened
        ! to hand back a page whose bytes read as NaN. Give it defined values first.
        !
        ! The values must be ROW-DISTINCT, not a single repeated constant: with every element equal
        ! the range forms would agree even if they returned the wrong rows, which would trade this
        ! problem for a silently weaker test.
        w_f64 => t%flux()
        w_f64 = [(0.5_real64*i, i = 1, NROW)]
        w_f64  => t%flux()
        o_f64  => t%flux(3)
        o2_f64 => t%flux(3_int64)
        r_f64  => t%flux(LO, HI)
        r2_f64 => t%flux(int(LO, int64), int(HI, int64))
        call check_f64_forms(error, w_f64, o_f64, o2_f64, r_f64, r2_f64, "flux")
        if (allocated(error)) return
        !
        ! --- float32 vector: crd
        w_f32v => t%crd()
        o_f32v => t%crd(3)
        call check(error, all(abs(o_f32v - w_f32v(:, 3)) < 1.0e-6_real32), "crd(3)")
        if (allocated(error)) return
        o_f32v => t%crd(3_int64)
        call check(error, all(abs(o_f32v - w_f32v(:, 3)) < 1.0e-6_real32), "crd(3_int64)")
        if (allocated(error)) return
        r_f32v => t%crd(LO, HI)
        call check(error, all(abs(r_f32v - w_f32v(:, LO:HI)) < 1.0e-6_real32), "crd(lo,hi)")
        if (allocated(error)) return
        r_f32v => t%crd(int(LO, int64), int(HI, int64))
        call check(error, all(abs(r_f32v - w_f32v(:, LO:HI)) < 1.0e-6_real32), "crd(lo,hi) int64")
        if (allocated(error)) return
        !
        ! --- int32 vector: counts
        w_i32v => t%counts()
        o_i32v => t%counts(3);           call check(error, all(o_i32v == w_i32v(:, 3)), "counts(3)")
        if (allocated(error)) return
        o_i32v => t%counts(3_int64);     call check(error, all(o_i32v == w_i32v(:, 3)), "counts(3_int64)")
        if (allocated(error)) return
        r_i32v => t%counts(LO, HI)
        call check(error, all(r_i32v == w_i32v(:, LO:HI)), "counts(lo,hi)")
        if (allocated(error)) return
        r_i32v => t%counts(int(LO, int64), int(HI, int64))
        call check(error, all(r_i32v == w_i32v(:, LO:HI)), "counts(lo,hi) int64")
        if (allocated(error)) return
        !
        ! --- logical vector: passed
        w_bv => t%passed()
        o_bv => t%passed(3);             call check(error, all(o_bv .eqv. w_bv(:, 3)), "passed(3)")
        if (allocated(error)) return
        o_bv => t%passed(3_int64);       call check(error, all(o_bv .eqv. w_bv(:, 3)), "passed(3_int64)")
        if (allocated(error)) return
        r_bv => t%passed(LO, HI)
        call check(error, all(r_bv .eqv. w_bv(:, LO:HI)), "passed(lo,hi)")
        if (allocated(error)) return
        r_bv => t%passed(int(LO, int64), int(HI, int64))
        call check(error, all(r_bv .eqv. w_bv(:, LO:HI)), "passed(lo,hi) int64")
        if (allocated(error)) return
        !
        ! --- the three temporal kinds, compared through their raw day/second counts
        w_d => t%obsdate()
        o_d => t%obsdate(3);             call check(error, o_d%raw() == w_d(3)%raw(), "obsdate(3)")
        if (allocated(error)) return
        o_d => t%obsdate(3_int64);       call check(error, o_d%raw() == w_d(3)%raw(), "obsdate(3_int64)")
        if (allocated(error)) return
        r_d => t%obsdate(LO, HI)
        call check(error, all(r_d%raw() == w_d(LO:HI)%raw()), "obsdate(lo,hi)")
        if (allocated(error)) return
        r_d => t%obsdate(int(LO, int64), int(HI, int64))
        call check(error, all(r_d%raw() == w_d(LO:HI)%raw()), "obsdate(lo,hi) int64")
        if (allocated(error)) return
        w_tm => t%obstime()
        o_tm => t%obstime(3);            call check(error, o_tm%second() == w_tm(3)%second(), "obstime(3)")
        if (allocated(error)) return
        o_tm => t%obstime(3_int64);      call check(error, o_tm%second() == w_tm(3)%second(), "obstime(3_int64)")
        if (allocated(error)) return
        ! The size is asserted first and separately, then the VALUES. Size alone would pass against
        ! a range form that returned entirely the wrong rows; and the two cannot share one `.and.`
        ! expression, because Fortran does not short-circuit and `all(a == b)` on mismatched shapes
        ! would be an out-of-bounds read before the size test could reject it.
        r_tm => t%obstime(LO, HI)
        call check(error, size(r_tm) == HI - LO + 1, "obstime(lo,hi) should span the row range")
        if (allocated(error)) return
        call check(error, all(r_tm%raw() == w_tm(LO:HI)%raw()), "obstime(lo,hi)")
        if (allocated(error)) return
        r_tm => t%obstime(int(LO, int64), int(HI, int64))
        call check(error, size(r_tm) == HI - LO + 1, "obstime(lo,hi) int64 should span the row range")
        if (allocated(error)) return
        call check(error, all(r_tm%raw() == w_tm(LO:HI)%raw()), "obstime(lo,hi) int64")
        if (allocated(error)) return
        w_ts => t%obsstamp()
        ! Comparing only `is_null()` here would reduce to `.false. .eqv. .false.` for this fixture,
        ! which passes for ANY row index -- so the raw (seconds, nanoseconds) pair is compared too.
        o_ts => t%obsstamp(3)
        call o_ts%get_raw(ts_sec1, ts_ns1)
        call w_ts(3)%get_raw(ts_sec2, ts_ns2)
        call check(error, ts_sec1 == ts_sec2 .and. ts_ns1 == ts_ns2, "obsstamp(3)")
        if (allocated(error)) return
        o_ts => t%obsstamp(3_int64)
        call o_ts%get_raw(ts_sec1, ts_ns1)
        call check(error, ts_sec1 == ts_sec2 .and. ts_ns1 == ts_ns2, "obsstamp(3_int64)")
        if (allocated(error)) return
        r_ts => t%obsstamp(LO, HI)
        call check(error, size(r_ts) == HI - LO + 1, "obsstamp(lo,hi) should span the row range")
        if (allocated(error)) return
        call r_ts%get_raw(ts_sec_a, ts_ns_a)
        call w_ts(LO:HI)%get_raw(ts_sec_b, ts_ns_b)
        call check(error, all(ts_sec_a == ts_sec_b) .and. all(ts_ns_a == ts_ns_b), "obsstamp(lo,hi)")
        if (allocated(error)) return
        r_ts => t%obsstamp(int(LO, int64), int(HI, int64))
        call check(error, size(r_ts) == HI - LO + 1, "obsstamp(lo,hi) int64 should span the row range")
        if (allocated(error)) return
        call r_ts%get_raw(ts_sec_a, ts_ns_a)
        call check(error, all(ts_sec_a == ts_sec_b) .and. all(ts_ns_a == ts_ns_b), "obsstamp(lo,hi) int64")
        if (allocated(error)) return
        !
        ! --- the string scalar's int64 index forms (the int32 ones are covered above)
        call check_string_forms(error, t)
    end subroutine test_every_field_every_form
    !
    !> The float64 accessor sweep, shared by `ra`, `dec` and the computed `flux`.
    subroutine check_f64_forms(error, whole, one32, one64, rng32, rng64, what)
        type(error_type), allocatable, intent(out) :: error
        real(real64), intent(in) :: whole(:)   !! %x()
        real(real64), intent(in) :: one32      !! %x(i), int32 index
        real(real64), intent(in) :: one64      !! %x(i), int64 index
        real(real64), intent(in) :: rng32(:)   !! %x(lo, hi), int32 bounds
        real(real64), intent(in) :: rng64(:)   !! %x(lo, hi), int64 bounds
        character(len=*), intent(in) :: what   !! column name, for the message
        integer, parameter :: LO = 2, HI = 5
        !
        call check(error, abs(one32 - whole(3)) < 1.0e-12_real64, what // "(3)")
        if (allocated(error)) return
        call check(error, abs(one64 - whole(3)) < 1.0e-12_real64, what // "(3_int64)")
        if (allocated(error)) return
        call check(error, all(abs(rng32 - whole(LO:HI)) < 1.0e-12_real64), what // "(lo,hi)")
        if (allocated(error)) return
        call check(error, all(abs(rng64 - whole(LO:HI)) < 1.0e-12_real64), what // "(lo,hi) int64")
    end subroutine check_f64_forms
    !
    !> The string column's int64 handle forms, which the shape-class test does not reach.
    subroutine check_string_forms(error, t)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test), intent(in), target :: t !! an initialized table.
        type(parquet_string) :: h
        type(parquet_string), allocatable :: hs(:)
        character(len=:), allocatable :: s
        !
        h = t%name(4_int64)
        call h%to_string(s)
        call check(error, s == "ghijk", "name(4_int64) should view the same value as name(4)")
        if (allocated(error)) return
        hs = t%name(2_int64, 4_int64)
        call check(error, size(hs) == 3, "name(lo,hi) int64 should return one handle per row")
        if (allocated(error)) return
        call hs(1)%to_string(s)
        call check(error, s == "bcd", "name(lo,hi) int64 should be in row order")
    end subroutine check_string_forms
    !
    !> Every constructor specific, including the int64 `%init_empty` the other tests do not reach.
    subroutine test_every_constructor(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: a, b, c
        character(len=*), parameter :: f = "test_run/codegen_ctors.parquet"
        !
        call write_codegen_fixture(f)
        call a%init_slice(f, 2_int64, 4_int64)
        call check(error, a%nrows() == 3, "the int64 slice constructor should cover its range")
        if (allocated(error)) return
        call b%init_empty(5_int64)
        call check(error, b%nrows() == 5, "the int64 init_empty should create its rows")
        if (allocated(error)) return
        call c%init_empty(0)
        call check(error, c%nrows() == 0, "init_empty(0) should create no rows")
    end subroutine test_every_constructor
    !
end module test_table_codegen
