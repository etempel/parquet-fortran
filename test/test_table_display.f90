!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `parquet_table%print_rows`: the formatted row display.
!!
!! **Every test here writes to its own scratch unit and reads the text back**, which is the only
!! way to assert on a printer at all -- the `pf_stats%print` pattern (`test/test_stats.f90`).
!! Each test owns its own file name: test-drive runs a suite's tests concurrently, so two tests
!! sharing one path would truncate it under each other.
!!
!! **What is asserted is CELLS AND TOKENS, never whole lines.** A column's width depends on the
!! data in it, so a test that pinned a rendered line would fail on any change to the fixture and
!! would say nothing about correctness -- `tools/check_source_conventions.py`'s
!! `check_print_stat_columns_documented` spells out the same rule for the table's other printer.
!! So these tests look for a value, a `<null>`, a `...` row, a gutter number or a heading word,
!! and never for a line.
!!
!! **The `row` gutter is the assertion of record for row selection.** `first=`, `last=`, `rows=`
!! and the clamping between them all reduce to "which row numbers appeared, in what order", which
!! `data_gutters` below reads straight back out of the printed text.
!!
!! `verbosity="silent"` is asserted in `test/test_settings.f90` instead, next to this library's
!! other silence tests: it writes a process-global setting, and `settings` is one of the suites
!! excluded from test-drive's per-test parallelism.
module test_table_display
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_table_display

    !> Rows in the fixture most tests here use. Chosen so that the 5+5 default leaves a gap: at
    !! 10 or fewer rows the two ends meet and no `...` row is printed at all, which would make
    !! every default-shape assertion vacuous.
    integer, parameter :: NROW = 12
    !> Elements per row of the vector column.
    integer, parameter :: NVEC = 3
    !> Longest printed line these tests read back. Wider than any fixture here produces, so a line
    !! is never silently truncated on the way into an assertion.
    integer, parameter :: LINEW = 1024

contains

    !> Registers every test in this suite.
    subroutine collect_tests_table_display(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("default 5+5 rows, both heading rows and exactly one ... row", &
                test_print_rows_defaults), &
            new_unittest("first=/last= choose one end, and clamp to the table", &
                test_print_rows_first_last), &
            new_unittest("rows= prints exactly the slice, in the slice's order", test_print_rows_slice), &
            new_unittest("no columns= reads nothing; columns= reads exactly what it names", &
                test_print_rows_columns_and_reading), &
            new_unittest("all 18 kinds render, and a null of every kind prints <null>", &
                test_print_rows_every_kind), &
            new_unittest("a list cell is [n items] and a map cell is {n pairs}", &
                test_print_rows_containers), &
            new_unittest("max_width cuts a cell, digits= changes a real, NaN is a value", &
                test_print_rows_truncation_and_digits), &
            new_unittest("max_columns caps the columns shown and says how many were dropped", &
                test_print_rows_max_columns), &
            new_unittest("printing changes nothing: generation, residency and a live pointer", &
                test_print_rows_changes_nothing), &
            new_unittest("the permitted edge of every argument bound", test_print_rows_argument_edges), &
            new_unittest("a real outside the fixed range prints in exponent form", &
                test_print_rows_scientific_reals), &
            new_unittest("a struct cell prints its field count", test_print_rows_struct_cell), &
            new_unittest("%print_stat(unit=) writes the listing to the given unit", &
                test_print_stat_unit) &
            ]
    end subroutine collect_tests_table_display

    !> **The default shape: five rows from each end, with one `...` between them.**
    !>
    !> The fixture has 12 rows, so 5+5 leaves two out and the gap row must appear exactly once --
    !> a display that dropped the tail would still print five rows and still look plausible, which
    !> is why the gutters are compared as a whole sequence rather than counted.
    subroutine test_print_rows_defaults(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_display_defaults.parquet"
        character(len=*), parameter :: out = "test_run/table_display_defaults.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer, allocatable :: g(:)
        integer :: n, ng, u

        call write_display_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(unit=u)
        close(u)
        call read_lines(out, lines, n)

        call check(error, find_token(lines, n, "parquet_table:") > 0, &
            "the display must name the table, as %print_stat does")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "rows: 12") > 0, &
            "the second header line must report the row count")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "shown)") > 0, &
            "the second header line must say how many columns are shown")
        if (allocated(error)) return
        ! The two heading rows: the names, then the kinds beneath them. The kind row is what tells
        ! a reader what each column BECAME here, so its absence is a real regression.
        call check(error, find_token(lines, n, "cls") > 0, "the column names must be printed")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "float64") > 0, &
            "the kind row must be printed under the names")
        if (allocated(error)) return
        ! A unit rides in the kind cell, as %print_stat shows it. Added here rather than read
        ! from the file: a parquet file records no unit, so a column only has one when a MAML or
        ! %add_column gave it one.
        block
            type(parquet_table) :: mem
            real(real64) :: mag(3)
            integer :: uu, nn
            character(len=LINEW), allocatable :: ml(:)
            character(len=*), parameter :: mout = "test_run/table_display_defaults_unit.txt"
            mag = [1.0_real64, 2.0_real64, 3.0_real64]
            call parquet_new_table(mem)
            call mem%add_column("mag", mag, unit="mag")
            open(newunit=uu, file=mout, status="replace", action="write")
            call mem%print_rows(unit=uu)
            close(uu)
            call read_lines(mout, ml, nn)
            call check(error, find_token(ml, nn, "float64 [mag]") > 0, &
                "a column's unit must ride in its kind cell, as %print_stat shows it")
            if (allocated(error)) return
            call check(error, find_token(ml, nn, "(built in memory)") > 0, &
                "and a table with no file must say so where the file name goes")
            if (allocated(error)) return
        end block

        call data_gutters(lines, n, g, ng)
        call check(error, ng == 10, "5 + 5 rows must be printed out of 12")
        if (allocated(error)) return
        call check(error, all(g(1:ng) == [1, 2, 3, 4, 5, 8, 9, 10, 11, 12]), &
            "the gutters must be the first five and the last five rows, in table order")
        if (allocated(error)) return
        call check(error, count_lines_starting(lines, n, "...") == 1, &
            "exactly one ... row must separate the head from the tail")
    end subroutine test_print_rows_defaults

    !> A real too large or too small for fixed notation renders in the exponent form.
    !!
    !! `real_text` decides between a fixed and a scientific rendering from the exponent it reads
    !! back out of an `ES` write: below 1e-4 or at or above `10**digits` it emits the `1.5e+30`
    !! form, trimming trailing zeros and padding the exponent to two digits as C does. Every
    !! other display test uses values in the single digits, so only the fixed half runs; the
    !! branch here is the one that has to assemble the exponent by hand.
    !!
    !! Both signs of the exponent are printed because the sign is chosen by a two-armed `if`, and
    !! a value between them pins that the branch is not simply taken for everything.
    subroutine test_print_rows_scientific_reals(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: out = "test_run/table_display_scientific.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer :: n, u

        call parquet_new_table(t)
        call t%add_column("x", [1.5e30_real64, 2.5e-8_real64, 3.25_real64])
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(unit=u)
        close(u)
        call read_lines(out, lines, n)

        call check(error, find_token(lines, n, "e+30") > 0, &
            "a value at or above 10**digits must render with a positive exponent")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "e-08") > 0, &
            "a value below 1e-4 must render with a negative exponent, padded to two digits")
        if (allocated(error)) return
        ! The control: the branch must not swallow an ordinary value.
        call check(error, find_token(lines, n, "3.25") > 0, &
            "and a value inside the fixed range must still render in fixed notation")
    end subroutine test_print_rows_scientific_reals

    !> A STRUCT cell prints its field count, as a list cell prints items and a map cell pairs.
    !!
    !! `container_text` narrows the container with a `select type` and has one arm per container
    !! kind. `test_print_rows_container_cells` covers the list and map arms; the struct arm is
    !! reached by nothing else, and an unmatched `select type` would leave the cell EMPTY rather
    !! than abort -- so what is asserted is the text, not that printing succeeded.
    subroutine test_print_rows_struct_cell(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        type(parquet_struct_column) :: sc
        character(len=*), parameter :: out = "test_run/table_display_struct_cell.txt"
        character(len=LINEW), allocatable :: lines(:)
        character(len=8) :: fields(3)
        integer :: kinds(3)
        integer :: n, u

        fields(1) = "f1"
        fields(2) = "f2"
        fields(3) = "f3"
        kinds(1) = PK_INT32
        kinds(2) = PK_FLOAT64
        kinds(3) = PK_INT64
        call sc%init(fields, kinds, 2_int64)
        call sc%clear_null_row(1_int64)
        call parquet_new_table(t)
        call t%add_column("st", sc)
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(unit=u)
        close(u)
        call read_lines(out, lines, n)

        ! Three fields were declared, so the cell must say three -- a count taken from the wrong
        ! place (the row count, say) would read "2 fields" here.
        call check(error, find_token(lines, n, "3 fields") > 0, &
            "a struct cell must print its FIELD count")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "struct") > 0, &
            "and the kind row must name the container kind")
    end subroutine test_print_rows_struct_cell

    !> **`first=` and `last=` each select one end, and both clamp to the table.**
    !>
    !> Naming either one sets the other to zero, so `first=3` is `head(3)` with no `...` row and
    !> `last=2` is `tail(2)`. `first=20` on 12 rows must print all 12 and still no gap: the clamp
    !> is also what keeps the row indices inside the table, which `--profile debug` would catch as
    !> an out-of-range read if it were missing.
    subroutine test_print_rows_first_last(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_display_firstlast.parquet"
        character(len=*), parameter :: out = "test_run/table_display_firstlast.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer, allocatable :: g(:)
        integer :: n, ng, u, i

        call write_display_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=3, unit=u)
        close(u)
        call read_lines(out, lines, n)
        call data_gutters(lines, n, g, ng)
        call check(error, ng == 3, "first=3 must print three rows")
        if (allocated(error)) return
        call check(error, all(g(1:ng) == [1, 2, 3]), "first=3 must print the FIRST three rows")
        if (allocated(error)) return
        call check(error, count_lines_starting(lines, n, "...") == 0, &
            "naming first= alone must not leave a tail behind a ... row")
        if (allocated(error)) return

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(last=2, unit=u)
        close(u)
        call read_lines(out, lines, n)
        call data_gutters(lines, n, g, ng)
        call check(error, ng == 2, "last=2 must print two rows")
        if (allocated(error)) return
        call check(error, all(g(1:ng) == [NROW - 1, NROW]), "last=2 must print the LAST two rows")
        if (allocated(error)) return

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=20, unit=u)
        close(u)
        call read_lines(out, lines, n)
        call data_gutters(lines, n, g, ng)
        call check(error, ng == NROW, "first=20 on a 12-row table must print all 12 rows")
        if (allocated(error)) return
        call check(error, all(g(1:ng) == [(i, i = 1, NROW)]), &
            "and they must be rows 1..12 in order, with nothing out of range")
        if (allocated(error)) return
        call check(error, count_lines_starting(lines, n, "...") == 0, &
            "a request covering the whole table must print no ... row")
        if (allocated(error)) return

        ! The exact boundary: six from each end of twelve rows leaves nothing between them, so the
        ! two ends must MERGE into one run rather than meet either side of a gap that marks no
        ! elided row. This is the case that tells `>=` from `>`, and neither of the requests above
        ! reaches it.
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=NROW/2, last=NROW/2, unit=u)
        close(u)
        call read_lines(out, lines, n)
        call data_gutters(lines, n, g, ng)
        call check(error, ng == NROW, "first= and last= that exactly cover the table must print it all")
        if (allocated(error)) return
        call check(error, all(g(1:ng) == [(i, i = 1, NROW)]), "in order, with no row printed twice")
        if (allocated(error)) return
        call check(error, count_lines_starting(lines, n, "...") == 0, &
            "and with no ... row, because no row was left out")
    end subroutine test_print_rows_first_last

    !> **`rows=` prints exactly the rows the slice names, in the slice's own order.**
    !>
    !> That is the form for showing what a lookup returned, so a repeated index must appear twice
    !> and a descending pair must stay descending -- a display that sorted or de-duplicated would
    !> be answering a different question. A one-element slice is the permitted edge.
    subroutine test_print_rows_slice(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_display_slice.parquet"
        character(len=*), parameter :: out = "test_run/table_display_slice.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer, allocatable :: g(:)
        integer :: n, ng, u

        call write_display_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(rows=parquet_slice_list([7_int32, 2_int32, 7_int32]), unit=u)
        close(u)
        call read_lines(out, lines, n)
        call data_gutters(lines, n, g, ng)
        call check(error, ng == 3, "a three-element slice must print three rows")
        if (allocated(error)) return
        call check(error, all(g(1:ng) == [7, 2, 7]), &
            "rows= must keep the slice's order and its repeats")
        if (allocated(error)) return
        call check(error, count_lines_starting(lines, n, "...") == 0, &
            "rows= names its rows explicitly, so there is no gap to mark")
        if (allocated(error)) return

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(rows=parquet_slice_range(4_int32, 4_int32), unit=u)
        close(u)
        call read_lines(out, lines, n)
        call data_gutters(lines, n, g, ng)
        call check(error, ng == 1, "a one-row slice must print one row")
        if (allocated(error)) return
        call check(error, g(1) == 4, "and it must be that row")
    end subroutine test_print_rows_slice

    !> **The residency contract: `%print_rows()` reads nothing; `columns=` reads what it names.**
    !>
    !> This is the pair the whole design turns on. Without `columns=`, a lazy table opened over a
    !> wide file must print its hint line and leave every column unread -- a display that quietly
    !> materialized the file to show ten rows is the trap the hint exists to make visible. With
    !> `columns=`, exactly the named columns become resident and no others.
    subroutine test_print_rows_columns_and_reading(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_display_columns.parquet"
        character(len=*), parameter :: out = "test_run/table_display_columns.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer, allocatable :: g(:)
        integer :: n, ng, u, i, nres

        call write_display_fixture(f)
        call parquet_open_table(t, f)
        ! Vacuity guard: if opening a table read the columns by itself, the assertion below would
        ! hold for the wrong reason and the mutation it exists to catch would go unnoticed.
        nres = 0
        do i = 1, t%ncols()
            if (t%residency(i) == RES_FULL) nres = nres + 1
        end do
        call check(error, nres == 0, "an opened table must start with nothing resident")
        if (allocated(error)) return

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(unit=u)
        close(u)
        call read_lines(out, lines, n)
        call check(error, find_token(lines, n, "no materialized columns") > 0, &
            "a lazy table must print the hint telling the caller how to see its rows")
        if (allocated(error)) return
        call data_gutters(lines, n, g, ng)
        call check(error, ng == 0, "and it must print no data rows")
        if (allocated(error)) return
        nres = 0
        do i = 1, t%ncols()
            if (t%residency(i) == RES_FULL) nres = nres + 1
        end do
        call check(error, nres == 0, &
            "%print_rows() without columns= must not have read anything")
        if (allocated(error)) return

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows("id,cls", unit=u)
        close(u)
        call read_lines(out, lines, n)
        call check(error, find_token(lines, n, "galaxy") > 0, &
            "columns= must read the columns it names and show their values")
        if (allocated(error)) return
        call check(error, t%residency("id") == RES_FULL .and. t%residency("cls") == RES_FULL, &
            "both named columns must be resident afterwards")
        if (allocated(error)) return
        call check(error, t%residency("flux") == RES_EMPTY, &
            "and a column that was not named must still be unread")
        if (allocated(error)) return
        ! The named order, not the table order: `cls` is column 2 in the file and is asked for
        ! second here, so a display that fell back to table order would look identical. Asking in
        ! the reverse order is what tells the two apart.
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows("cls,id", unit=u)
        close(u)
        call read_lines(out, lines, n)
        i = find_token(lines, n, "cls")
        call check(error, i > 0, "the heading row must carry the names asked for")
        if (allocated(error)) return
        call check(error, index(lines(i), "cls") < index(lines(i), "id"), &
            "columns= must show the columns in the order NAMED, not in table order")
    end subroutine test_print_rows_columns_and_reading

    !> **Every one of the 18 kinds renders, and a null of every kind prints `<null>`.**
    !>
    !> One arm of the cell renderer per kind, and a missing arm shows up as an absent value rather
    !> than as a failure -- so the null row is counted: 18 columns, all set null at row 2, must
    !> produce 18 `<null>` tokens on that one row. That also pins the rule that a null TEMPORAL is
    !> tested before `%to_string`, which aborts on one.
    subroutine test_print_rows_every_kind(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_display_kinds.parquet"
        character(len=*), parameter :: out = "test_run/table_display_kinds.txt"
        character(len=LINEW), allocatable :: lines(:)
        character(len=:), allocatable :: names(:)
        integer :: n, u, i, krow

        call write_kinds_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call check(error, t%ncols() == 18, "the fixture must still carry all 18 kinds")
        if (allocated(error)) return
        call t%column_names(names)
        do i = 1, size(names)
            call t%set_null(trim(names(i)), 2_int64)
        end do
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=4, max_width=64, unit=u)
        close(u)
        call read_lines(out, lines, n)

        ! Row 1 of each kind family, by the shape only that family produces.
        call check(error, find_token(lines, n, "true") > 0, "a logical cell must render as true/false")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "2026-03-01") > 0, &
            "a date cell must render as its ISO-8601 text")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "[11, 12, 13]") > 0, &
            "a vector cell must render as a bracketed element list")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "ghijklm") > 0, "a string cell must render verbatim")
        if (allocated(error)) return
        ! The kind row names what each column became, including its vector width.
        krow = find_token(lines, n, "timestamp_vec[3]")
        call check(error, krow > 0, "the kind row must carry a vector kind with its width")
        if (allocated(error)) return

        i = line_of_gutter(lines, n, 2)
        call check(error, i > 0, "row 2 must have been printed")
        if (allocated(error)) return
        call check(error, count_occurrences(lines(i), "<null>") == 18, &
            "a null must print as <null> on every one of the 18 kinds")
    end subroutine test_print_rows_every_kind

    !> **A container cell reports its row's size rather than its payload.**
    !>
    !> A list or map row can be arbitrarily long and is reachable only through its own handle, so
    !> the display shows the count. `ragged`'s lengths cycle 1,2,3,4, which is what makes a cell
    !> read off the wrong row visible here.
    subroutine test_print_rows_containers(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: lt, mt
        type(parquet_list_column), pointer :: lp
        character(len=*), parameter :: out = "test_run/table_display_containers.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer :: n, u

        call parquet_open_table(lt, "test/fixtures/list_widths.parquet", list_columns="container")
        call lt%prefetch("ragged")
        call lt%col("ragged", lp)
        ! Vacuity guard: the assertion below quotes the fixture's own first two row lengths, so it
        ! must fail loudly if a regenerated fixture no longer has them.
        call check(error, associated(lp), "the fixture's ragged column must be a container")
        if (allocated(error)) return
        call check(error, lp%length(1_int64) == 1 .and. lp%length(2_int64) == 2, &
            "the fixture's ragged lengths must still be 1 and 2 -- the cells asserted below")
        if (allocated(error)) return
        open(newunit=u, file=out, status="replace", action="write")
        call lt%print_rows("ragged", first=4, unit=u)
        close(u)
        call read_lines(out, lines, n)
        call check(error, find_token(lines, n, "[1 items]") > 0 .and. &
            find_token(lines, n, "[2 items]") > 0, &
            "a list cell must report its own row's element count")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "list") > 0, &
            "and the kind row must say the column is a list")
        if (allocated(error)) return

        call parquet_open_table(mt, "test/fixtures/map_payloads.parquet")
        open(newunit=u, file=out, status="replace", action="write")
        call mt%print_rows("m_int32", first=4, unit=u)
        close(u)
        call read_lines(out, lines, n)
        call check(error, find_token(lines, n, " pairs}") > 0, &
            "a map cell must report its own row's entry count")
    end subroutine test_print_rows_containers

    !> **`max_width` cuts a long cell, `digits=` changes a real, and a NaN is a value.**
    !>
    !> The cut is asserted on the CELL, not on the column: a display that cut the heading instead
    !> would leave the long value overflowing and still look tidy on the fixtures where the
    !> heading happens to be longest. `NaN` is the line the filter draws between a missing value
    !> and a value that is not a number, kept on the display side -- so it must NOT print
    !> `<null>`.
    subroutine test_print_rows_truncation_and_digits(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_display_trunc.parquet"
        character(len=*), parameter :: out = "test_run/table_display_trunc.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer :: n, u, i

        call write_wide_value_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=3, unit=u)
        close(u)
        call read_lines(out, lines, n)
        ! Row 2 carries both the long string and the NaN; row 1 carries the real.
        i = line_of_gutter(lines, n, 2)
        call check(error, i > 0, "row 2 must have been printed")
        if (allocated(error)) return
        ! 60 characters into a default max_width of 32: the first 29 characters of the value, then
        ! the marker. Quoted in full so a cut at the wrong offset cannot pass.
        call check(error, index(lines(i), "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa...") > 0, &
            "a cell longer than max_width must be cut to max_width-3 characters plus ...")
        if (allocated(error)) return
        call check(error, index(lines(i), "NaN") > 0, &
            "a NaN is a value, not a null, and must print as one")
        if (allocated(error)) return
        call check(error, count_occurrences(lines(i), "<null>") == 0, &
            "so nothing on that row may print as a null")
        if (allocated(error)) return
        i = line_of_gutter(lines, n, 1)
        call check(error, i > 0, "row 1 must have been printed")
        if (allocated(error)) return
        call check(error, index(lines(i), "0.0123457") > 0, &
            "a real must render at six significant digits by default, with no trailing zeros")
        if (allocated(error)) return

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=3, digits=3, unit=u)
        close(u)
        call read_lines(out, lines, n)
        i = line_of_gutter(lines, n, 1)
        call check(error, index(lines(i), "0.0123") > 0 .and. index(lines(i), "0.0123457") == 0, &
            "digits=3 must shorten the rendered real, not merely narrow the column")
        if (allocated(error)) return

        ! max_width reaches the cut itself, not only the default.
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=3, max_width=12, unit=u)
        close(u)
        call read_lines(out, lines, n)
        i = line_of_gutter(lines, n, 2)
        call check(error, index(lines(i), "aaaaaaaaa...") > 0, &
            "max_width=12 must cut the same cell to nine characters plus ...")
    end subroutine test_print_rows_truncation_and_digits

    !> **`max_columns` caps how many columns are shown, and says how many it dropped.**
    !>
    !> A display that silently stopped at twenty would look identical on a twenty-column table and
    !> would hide the other five here, so the `(+N more)` marker is the assertion of record.
    subroutine test_print_rows_max_columns(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_display_maxcols.parquet"
        character(len=*), parameter :: out = "test_run/table_display_maxcols.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer :: n, u, i

        call write_many_column_fixture(f, 25)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call check(error, t%ncols() == 25, "the fixture must carry 25 columns")
        if (allocated(error)) return

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=2, unit=u)
        close(u)
        call read_lines(out, lines, n)
        i = find_token(lines, n, "c01")
        call check(error, i > 0, "the heading row must carry the first column's name")
        if (allocated(error)) return
        call check(error, index(lines(i), "c20") > 0, "and the twentieth's")
        if (allocated(error)) return
        call check(error, index(lines(i), "c21") == 0, &
            "but not the twenty-first: the default cap is 20 columns")
        if (allocated(error)) return
        call check(error, index(lines(i), "(+5 more)") > 0, &
            "and the heading must say how many columns were left out")
        if (allocated(error)) return

        ! The cap is the caller's, not a constant: one column and a marker counting the other 24.
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=2, max_columns=1, unit=u)
        close(u)
        call read_lines(out, lines, n)
        i = find_token(lines, n, "c01")
        call check(error, i > 0 .and. index(lines(max(i, 1)), "c02") == 0, &
            "max_columns=1 must show exactly one column")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "(+24 more)") > 0, &
            "and count the other twenty-four")
    end subroutine test_print_rows_max_columns

    !> **The display changes nothing about the table it displays.**
    !>
    !> No structural change (so `%generation()` is unmoved), no detach, and a pointer taken before
    !> the call still points at the same values afterwards -- which is what makes it safe to put a
    !> `%print_rows` in the middle of a working program rather than only at the end of one.
    subroutine test_print_rows_changes_nothing(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_display_stable.parquet"
        character(len=*), parameter :: out = "test_run/table_display_stable.txt"
        integer(int32), pointer :: p(:)
        integer(int64) :: gen_before, gen_after
        integer(int32) :: first_before
        integer :: u

        call write_display_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%col("id", p)
        call check(error, associated(p), "the fixture's id column must hand back a pointer")
        if (allocated(error)) return
        first_before = p(1)
        gen_before = t%generation()

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(unit=u)
        call t%print_rows("id,cls", unit=u)
        call t%print_rows(rows=parquet_slice_list([3_int32]), unit=u)
        close(u)

        gen_after = t%generation()
        call check(error, gen_after == gen_before, &
            "printing must make no structural change, so the generation must not move")
        if (allocated(error)) return
        call check(error, associated(p), "a pointer taken before the display must still be live")
        if (allocated(error)) return
        call check(error, p(1) == first_before, "and must still see the same values")
        if (allocated(error)) return
        call check(error, t%nrows() == int(NROW, int64), "the table must still hold its rows")
    end subroutine test_print_rows_changes_nothing

    !> **The permitted edge of every argument bound, which the guards must NOT reject.**
    !>
    !> Each refusal has its own out-of-process scenario in `test/error_scenarios.f90` (an
    !> `error stop` cannot be asserted on in process). What those cannot show is that the guard
    !> stops exactly where it says it does, which is what this test is for: `first=0, last=0`,
    !> `digits=1`, `digits=17`, `max_width=8` and `max_columns=1` are all legal, and an off-by-one
    !> in any bound turns one of them into an abort that takes the whole suite with it.
    subroutine test_print_rows_argument_edges(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_display_edges.parquet"
        character(len=*), parameter :: out = "test_run/table_display_edges.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer, allocatable :: g(:)
        integer :: n, ng, u

        call write_display_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=0, last=0, unit=u)
        close(u)
        call read_lines(out, lines, n)
        call data_gutters(lines, n, g, ng)
        call check(error, ng == 0, "first=0, last=0 must print the headings and no data rows")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "cls") > 0, &
            "and the headings must still be there")
        if (allocated(error)) return
        call check(error, count_lines_starting(lines, n, "...") == 0, &
            "with nothing shown there is no gap to mark")
        if (allocated(error)) return

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_rows(first=1, digits=1, unit=u)
        call t%print_rows(first=1, digits=17, unit=u)
        call t%print_rows(first=1, max_width=8, unit=u)
        call t%print_rows(first=1, max_columns=1, unit=u)
        close(u)
        call read_lines(out, lines, n)
        call data_gutters(lines, n, g, ng)
        call check(error, ng == 4, &
            "each of the four permitted edges must print its one row rather than abort")
    end subroutine test_print_rows_argument_edges

    ! ---- fixtures ----

    !> The 12-row fixture most tests here use: one column per alignment and null behaviour.
    !!
    !! `cls`'s first element is deliberately the SHORTEST, so a length taken from the first row
    !! rather than the widest would truncate every later one (`.claude/rules/testing.md`).
    subroutine write_display_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer(int32) :: id(NROW)
        real(real64) :: flux(NROW)
        character(len=8) :: cls(NROW)
        logical :: flag(NROW)
        integer(int32) :: vec(NVEC, NROW)
        type(parquet_date) :: obs(NROW)
        integer :: i, e

        do i = 1, NROW
            id(i) = i * 1000
            flux(i) = real(i, real64) * 0.0123456789_real64
            flag(i) = mod(i, 2) == 1
            obs(i) = parquet_date(2026, 3, i)
            do e = 1, NVEC
                vec(e, i) = i * 10 + e
            end do
        end do
        cls = ["a       ", "galaxy  ", "star    ", "qso     ", "galaxy  ", "star    ", &
               "qso     ", "galaxy  ", "star    ", "qso     ", "galaxy  ", "star    "]
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "cls", cls)
        call parquet_write_column(w, "flux", flux)
        call parquet_write_column(w, "flag", flag)
        call parquet_write_column(w, "vec", vec)
        call parquet_write_column(w, "obs", obs)
        call parquet_close_writer(w)
    end subroutine write_display_fixture

    !> All 18 kinds, six rows -- one column per kind the cell renderer has an arm for.
    subroutine write_kinds_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer, parameter :: NR = 6
        integer(int32) :: a_i32(NR), v_i32(NVEC, NR)
        integer(int64) :: a_i64(NR), v_i64(NVEC, NR)
        real(real32) :: a_f32(NR), v_f32(NVEC, NR)
        real(real64) :: a_f64(NR), v_f64(NVEC, NR)
        logical :: a_bool(NR), v_bool(NVEC, NR)
        character(len=8) :: a_str(NR), v_str(NVEC, NR)
        type(parquet_date) :: a_date(NR), v_date(NVEC, NR)
        type(parquet_time) :: a_time(NR), v_time(NVEC, NR)
        type(parquet_timestamp) :: a_ts(NR), v_ts(NVEC, NR)
        integer :: i, e

        do i = 1, NR
            a_i32(i) = i
            a_i64(i) = int(i, int64) * 1000000000_int64
            a_f32(i) = real(i, real32) * 0.25_real32
            a_f64(i) = real(i, real64) * 1.75_real64
            a_bool(i) = mod(i, 2) == 1
            a_date(i) = parquet_date(2026, 3, i)
            a_time(i) = parquet_time(10, 20, i)
            a_ts(i) = parquet_timestamp(2026, 3, i, 1, 2, 3)
            do e = 1, NVEC
                v_i32(e, i) = i * 10 + e
                v_i64(e, i) = int(i * 10 + e, int64) * 1000000000_int64
                v_f32(e, i) = real(i * 10 + e, real32) * 0.5_real32
                v_f64(e, i) = real(i * 10 + e, real64) * 1.5_real64
                v_bool(e, i) = mod(i + e, 2) == 0
                v_date(e, i) = parquet_date(2026, 4, e)
                v_time(e, i) = parquet_time(5, 6, e)
                v_ts(e, i) = parquet_timestamp(2026, 4, e, 7, 8, 9)
            end do
        end do
        ! First element the shortest, in both the scalar and the vector fixture.
        a_str = ["a       ", "bcd     ", "ef      ", "ghijklm ", "no      ", "p       "]
        v_str(1, :) = "a"
        v_str(2, :) = "bcde"
        v_str(3, :) = "fg"

        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "s_i32", a_i32)
        call parquet_write_column(w, "s_i64", a_i64)
        call parquet_write_column(w, "s_f32", a_f32)
        call parquet_write_column(w, "s_f64", a_f64)
        call parquet_write_column(w, "s_bool", a_bool)
        call parquet_write_column(w, "s_str", a_str)
        call parquet_write_column(w, "s_date", a_date)
        call parquet_write_column(w, "s_time", a_time)
        call parquet_write_column(w, "s_ts", a_ts)
        call parquet_write_column(w, "v_i32", v_i32)
        call parquet_write_column(w, "v_i64", v_i64)
        call parquet_write_column(w, "v_f32", v_f32)
        call parquet_write_column(w, "v_f64", v_f64)
        call parquet_write_column(w, "v_bool", v_bool)
        call parquet_write_column(w, "v_str", v_str)
        call parquet_write_column(w, "v_date", v_date)
        call parquet_write_column(w, "v_time", v_time)
        call parquet_write_column(w, "v_ts", v_ts)
        call parquet_close_writer(w)
    end subroutine write_kinds_fixture

    !> A 60-character string, a real that needs its digits, and a NaN -- the three values the
    !! truncation and precision rules are about.
    subroutine write_wide_value_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer, parameter :: NR = 3
        character(len=60) :: long_s(NR)
        real(real64) :: x(NR)
        integer(int32) :: id(NR)
        integer :: i

        do i = 1, NR
            id(i) = i
            x(i) = real(i, real64) * 0.0123456789_real64
        end do
        ! An IEEE quiet NaN, constructed rather than written as a literal: gfortran rejects
        ! 0.0/0.0 as a constant expression, and this keeps the value a genuine NaN on every
        ! compiler rather than a large finite number.
        x(2) = nan_value()
        ! Shortest first, longest second -- so a width taken from row 1 truncates row 2.
        long_s(1) = "short"
        long_s(2) = repeat("a", 60)
        long_s(3) = "medium length value"
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "note", long_s)
        call parquet_write_column(w, "x", x)
        call parquet_close_writer(w)
    end subroutine write_wide_value_fixture

    !> A quiet NaN, as a value rather than a literal.
    !!
    !! Built with `ieee_value` rather than by dividing zero by zero: the division raises
    !! `IEEE_INVALID`, and nagfor unmasks that trap by default, so the arithmetic form aborts the
    !! runner before the value it produces is ever printed.
    real(real64) function nan_value() result(v)
        use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
        !
        v = ieee_value(0.0_real64, ieee_quiet_nan)
    end function nan_value

    !> `ncol` int32 columns named `c01`, `c02`, ... -- enough of them to exceed `max_columns`.
    subroutine write_many_column_fixture(fname, ncol)
        character(len=*), intent(in) :: fname !! file to write.
        integer, intent(in) :: ncol           !! how many columns to write.
        type(parquet_writer) :: w
        integer(int32) :: v(4)
        character(len=8) :: name
        integer :: j, i

        do i = 1, 4
            v(i) = int(i, int32)
        end do
        call parquet_open_writer(w, fname)
        do j = 1, ncol
            write(name, "('c', i2.2)") j
            call parquet_write_column(w, trim(name), v + int(j, int32))
        end do
        call parquet_close_writer(w)
    end subroutine write_many_column_fixture

    !> **`%print_stat(unit=)` writes the listing to the unit it is given.**
    !!
    !! The other printer on `doc/pages/tables/table.md`, and the only way to tell "wrote to the
    !! unit" from "wrote somewhere and returned" is to read the unit back -- which is this suite's
    !! method, and why the test lives here rather than beside the other `%print_stat` tests in
    !! `test/test_table.f90`, which assert behaviour rather than text.
    !!
    !! Two shapes are captured, not one. A `%print_stat` that ignored `unit=` entirely would leave
    !! the file empty and fail the first assertion -- but one that wrote a fixed header to the unit
    !! and the real listing elsewhere would pass it, so the second capture asserts that `stats=`
    !! changes what the unit receives. That is what makes the unit the actual destination rather
    !! than somewhere a first line happened to land.
    !!
    !! **Deliberately not asserted: that `message_stream` moves it.** `%print_stat` shipped writing
    !! to standard output and does not consult that setting; `%print_rows` does, and
    !! `scenario_print_rows_follows_message_stream` is where that is pinned. A test written here
    !! against the setting would be asserting a behaviour this procedure does not have.
    subroutine test_print_stat_unit(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed check.
        type(parquet_table) :: t
        character(len=*), parameter :: out = "test_run/table_display_print_stat_unit.txt"
        character(len=LINEW), allocatable :: lines(:)
        integer :: n, u, with_stats

        call parquet_new_table(t)
        call t%add_column("mag", [1.0_real64, 2.0_real64, 3.0_real64], unit="mag")
        call t%add_column("id", [1_int32, 2_int32, 3_int32])

        open(newunit=u, file=out, status="replace", action="write")
        call t%print_stat(unit=u)
        close(u)
        call read_lines(out, lines, n)
        call check(error, n > 0, "%print_stat(unit=) must write to the unit it was given")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "parquet_table:") > 0, &
            "and the unit must receive the header line")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "mag") > 0 .and. find_token(lines, n, "id") > 0, &
            "and the column rows, not just the header")
        if (allocated(error)) return
        ! The statistics header is what `stats=.false.` drops, so it is the token that has to be
        ! present here for its absence below to mean anything.
        with_stats = find_token(lines, n, "nulls")
        call check(error, with_stats > 0, &
            "the default listing must carry the statistics columns")
        if (allocated(error)) return

        ! The same table, same unit, one argument different: what the unit receives must follow it.
        open(newunit=u, file=out, status="replace", action="write")
        call t%print_stat(unit=u, stats=.false.)
        close(u)
        call read_lines(out, lines, n)
        call check(error, n > 0 .and. find_token(lines, n, "mag") > 0, &
            "stats=.false. must still list the columns on the given unit")
        if (allocated(error)) return
        call check(error, find_token(lines, n, "nulls") == 0, &
            "and stats=.false. must drop the statistics columns from what the unit receives")
    end subroutine test_print_stat_unit

    ! ---- reading the printed text back ----

    !> Reads every line of `path` into `lines(1:n)`.
    subroutine read_lines(path, lines, n)
        character(len=*), intent(in) :: path                        !! the captured output.
        character(len=LINEW), allocatable, intent(out) :: lines(:)  !! one element per line.
        integer, intent(out) :: n                                   !! how many lines were read.
        character(len=LINEW) :: buf
        integer :: u, ios

        allocate(lines(0))
        n = 0
        open(newunit=u, file=path, status="old", action="read")
        do
            read(u, "(a)", iostat=ios) buf
            if (ios /= 0) exit
            n = n + 1
            lines = [lines, buf]
        end do
        close(u)
    end subroutine read_lines

    !> The index of the first line containing `tok`, or 0.
    integer function find_token(lines, n, tok) result(k)
        character(len=*), intent(in) :: lines(:) !! the captured lines.
        integer, intent(in) :: n                 !! how many of them are filled.
        character(len=*), intent(in) :: tok      !! the text to look for.
        integer :: i

        k = 0
        do i = 1, n
            if (index(lines(i), tok) > 0) then
                k = i
                return
            end if
        end do
    end function find_token

    !> How many lines begin (after the display's own indent) with `tok`.
    integer function count_lines_starting(lines, n, tok) result(k)
        character(len=*), intent(in) :: lines(:) !! the captured lines.
        integer, intent(in) :: n                 !! how many of them are filled.
        character(len=*), intent(in) :: tok      !! the text a line must start with.
        character(len=LINEW) :: t
        integer :: i

        k = 0
        do i = 1, n
            t = adjustl(lines(i))
            if (t(1:len(tok)) == tok) k = k + 1
        end do
    end function count_lines_starting

    !> How many times `tok` occurs in one line.
    integer function count_occurrences(line, tok) result(k)
        character(len=*), intent(in) :: line !! the line to scan.
        character(len=*), intent(in) :: tok  !! the text to count.
        integer :: p, at

        k = 0
        p = 1
        do
            at = index(line(p:), tok)
            if (at == 0) exit
            k = k + 1
            p = p + at + len(tok) - 1
            if (p > len(line)) exit
        end do
    end function count_occurrences

    !> The `row` gutter of every DATA line, in printed order.
    !!
    !! A data line is exactly one whose first token reads as an integer: the two header lines
    !! start with `parquet_table:` and `rows:`, the heading rows with a name and a kind, and the
    !! gap row with `...`, so none of them parses. That makes the gutter sequence readable
    !! straight out of the text without the test knowing anything about the layout.
    subroutine data_gutters(lines, n, g, ng)
        character(len=*), intent(in) :: lines(:)         !! the captured lines.
        integer, intent(in) :: n                         !! how many of them are filled.
        integer, allocatable, intent(out) :: g(:)        !! the gutters, in order.
        integer, intent(out) :: ng                       !! how many there were.
        integer :: i, ios, v

        allocate(g(max(n, 1)))
        ng = 0
        do i = 1, n
            read(lines(i), *, iostat=ios) v
            if (ios /= 0) cycle
            ng = ng + 1
            g(ng) = v
        end do
    end subroutine data_gutters

    !> The index of the data line whose gutter is `want`, or 0.
    integer function line_of_gutter(lines, n, want) result(k)
        character(len=*), intent(in) :: lines(:) !! the captured lines.
        integer, intent(in) :: n                 !! how many of them are filled.
        integer, intent(in) :: want              !! the row number to find.
        integer :: i, ios, v

        k = 0
        do i = 1, n
            read(lines(i), *, iostat=ios) v
            if (ios /= 0) cycle
            if (v /= want) cycle
            k = i
            return
        end do
    end function line_of_gutter

end module test_table_display
