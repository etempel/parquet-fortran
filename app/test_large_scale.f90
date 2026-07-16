!> Maintainer/user-runnable manual check (not part of the public library API, and never run by
!> `fpm test`/CI -- see tools/test_large_scale.sh) that writes, reads back, and verifies one
!> column at a time, for every supported data type, as a scalar (col_size=1) column, at a row
!> count (NROWS) supplied by the caller. Exists to let a large-memory machine actually exercise
!> the >huge(1_int32) (2,147,483,647) row-count code path this library supports, which the
!> normal `fpm test` suite deliberately never does (see CLAUDE.md's "no new large-file-writing
!> tests in fpm test" policy). Every case is independent: its write buffer is deallocated before
!> any verification read happens, and its temp file is deleted before the next case starts, so
!> peak memory/disk usage is bounded by one column's own data, not by the whole run.
!>
!> Vector (col_size=NELEM) cases run too (see RUN_VECTOR_CASES below), one per type. A large
!> total nrows*NELEM is not a problem: Arrow/Parquet's real list-element-count ceiling (a plain
!> int32_t counter in Parquet's own repetition/definition-level generation, level_conversion.cc)
!> is scoped to one row group, not the whole file, and parquet_close_writer's row-group
!> auto-sizing already keeps every row group under it regardless of how large nrows gets -- see
!> check_chunk_size_fits_limit_for_col_size in parquet_wrapper.cpp and the README's Limitations
!> section. Only an explicit chunk_size= passed to parquet_open_writer that itself conflicts with
!> NELEM would abort; this program never passes one.
!>
!> To reach the >huge(1_int32) scalar row-count scenario:
!>   NROWS=3000000000
!> MAX_SIZE_GB (default 8) skips any case whose estimated uncompressed size would exceed it,
!> rather than let an accidental NROWS combination exhaust memory/disk.
program test_large_scale
    use iso_fortran_env, only : int32, int64, real32, real64, output_unit, error_unit
    use parquet
    implicit none

    integer(int64), parameter :: BYTES_INT32 = 4_int64
    integer(int64), parameter :: BYTES_INT64 = 8_int64
    integer(int64), parameter :: BYTES_FLOAT32 = 4_int64
    integer(int64), parameter :: BYTES_FLOAT64 = 8_int64
    integer(int64), parameter :: BYTES_LOGICAL = 4_int64
    integer(int64), parameter :: BYTES_STRING = 12_int64
    integer, parameter :: MAX_STRING_LEN = 12
    character(len=*), parameter :: TMPFILE = "test_run/test_large_scale_tmp.parquet"
    character(len=*), parameter :: COLNAME = "v"

    !> Set to .false. to skip the 6 vector cases outright (e.g. for a quick scalar-only run);
    !> normally left .true. -- a large nrows*NELEM is safe here (see the module doc-comment above:
    !> parquet_close_writer's row-group auto-sizing keeps each row group under Arrow's real,
    !> per-row-group list-element ceiling regardless of the total), so leaving this on is fine
    !> even for an NROWS chosen to stress the *scalar* >huge(1_int32) row-count case.
    logical, parameter :: RUN_VECTOR_CASES = .true.

    integer(int64) :: nrows, nelem
    real(real64) :: max_size_gb
    integer :: test_num, total_tests, cmdstat

    call parse_arguments(nrows, nelem, max_size_gb)
    call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)

    test_num = 0
    total_tests = merge(13, 7, RUN_VECTOR_CASES)

    call int32_scalar_case(test_num, total_tests, nrows, max_size_gb)
    call int64_scalar_case(test_num, total_tests, nrows, max_size_gb)
    call float32_scalar_case(test_num, total_tests, nrows, max_size_gb)
    call float64_scalar_case(test_num, total_tests, nrows, max_size_gb)
    call logical_scalar_case(test_num, total_tests, nrows, max_size_gb)
    call parquet_string_column_case(test_num, total_tests, nrows, max_size_gb)
    call string_scalar_case(test_num, total_tests, nrows, max_size_gb)

    if (RUN_VECTOR_CASES) then
        call int32_vector_case(test_num, total_tests, nrows, nelem, max_size_gb)
        call int64_vector_case(test_num, total_tests, nrows, nelem, max_size_gb)
        call float32_vector_case(test_num, total_tests, nrows, nelem, max_size_gb)
        call float64_vector_case(test_num, total_tests, nrows, nelem, max_size_gb)
        call logical_vector_case(test_num, total_tests, nrows, nelem, max_size_gb)
        call string_vector_case(test_num, total_tests, nrows, nelem, max_size_gb)
    end if

    if (.not. RUN_VECTOR_CASES) then
        write(output_unit, '(a)') "test_large_scale: vector-column cases were skipped " // &
            "(RUN_VECTOR_CASES = .false. in app/test_large_scale.f90)."
    end if
    write(output_unit, '(a)') "test_large_scale: all cases completed (each PASSED or was SKIPPED as too large)."

contains

    subroutine print_usage()
        write(output_unit, '(a)') &
            "Usage: test_large_scale [--nrows=N] [--nelem=N] [--max-size-gb=G]", &
            "  --nrows=N         Row count for every case (default 1000).", &
            "  --nelem=N         Vector-column width (col_size) for the vector cases (default 2).", &
            "  --max-size-gb=G   Skip any case whose estimated uncompressed size exceeds this many GB " // &
                "(default 8)."
    end subroutine print_usage

    subroutine parse_arguments(nrows, nelem, max_size_gb)
        integer(int64), intent(out) :: nrows, nelem
        real(real64), intent(out) :: max_size_gb
        integer :: i, nargs, eq_pos, ios
        character(len=256) :: arg, key, val

        nrows = 1000_int64
        nelem = 2_int64
        max_size_gb = 8.0_real64

        nargs = command_argument_count()
        do i = 1, nargs
            call get_command_argument(i, arg)
            if (trim(arg) == "--help" .or. trim(arg) == "-h") then
                call print_usage()
                stop
            end if
            eq_pos = index(arg, "=")
            if (eq_pos < 2) then
                write(error_unit, '(a)') "test_large_scale: bad argument '"//trim(arg)//"', expected --key=value"
                error stop 1
            end if
            key = arg(1:eq_pos - 1)
            val = arg(eq_pos + 1:)
            select case (trim(key))
            case ("--nrows")
                read(val, *, iostat=ios) nrows
                if (ios /= 0) then
                    write(error_unit, '(a)') "test_large_scale: --nrows must be an integer"
                    error stop 1
                end if
            case ("--nelem")
                read(val, *, iostat=ios) nelem
                if (ios /= 0) then
                    write(error_unit, '(a)') "test_large_scale: --nelem must be an integer"
                    error stop 1
                end if
            case ("--max-size-gb")
                read(val, *, iostat=ios) max_size_gb
                if (ios /= 0) then
                    write(error_unit, '(a)') "test_large_scale: --max-size-gb must be a number"
                    error stop 1
                end if
            case default
                write(error_unit, '(a)') "test_large_scale: unknown argument '"//trim(key)//"'"
                error stop 1
            end select
        end do

        if (nrows < 2_int64) then
            write(error_unit, '(a)') "test_large_scale: --nrows must be >= 2"
            error stop 1
        end if
        if (nelem < 2_int64) then
            write(error_unit, '(a)') "test_large_scale: --nelem must be >= 2"
            error stop 1
        end if
        if (max_size_gb <= 0.0_real64) then
            write(error_unit, '(a)') "test_large_scale: --max-size-gb must be > 0"
            error stop 1
        end if
    end subroutine parse_arguments

    !> Estimated uncompressed size, in GB, of nrows*col_size elements of bytes_per_elem bytes
    !> each -- a deliberately simple proxy for "expected file size" (Parquet's own compression
    !> is not modeled), used only to decide whether a case is safe to attempt.
    function estimated_gb(nrows, col_size, bytes_per_elem) result(gb)
        integer(int64), intent(in) :: nrows, col_size, bytes_per_elem
        real(real64) :: gb
        gb = real(nrows, kind=real64) * real(col_size, kind=real64) * real(bytes_per_elem, kind=real64) &
            / (1024.0_real64**3)
    end function estimated_gb

    function case_label(type_name, shape_kind, nrows, col_size) result(lbl)
        character(len=*), intent(in) :: type_name, shape_kind
        integer(int64), intent(in) :: nrows, col_size
        character(len=:), allocatable :: lbl
        character(len=32) :: nrows_s, col_size_s

        write(nrows_s, '(i0)') nrows
        if (col_size > 1_int64) then
            write(col_size_s, '(i0)') col_size
            lbl = trim(type_name)//" "//trim(shape_kind)//" column (nrows="//trim(nrows_s)// &
                ", col_size="//trim(col_size_s)//")"
        else
            lbl = trim(type_name)//" "//trim(shape_kind)//" column (nrows="//trim(nrows_s)//")"
        end if
    end function case_label

    subroutine print_start(test_num, total, label, start_time)
        integer, intent(in) :: test_num, total
        character(len=*), intent(in) :: label
        integer(int64), intent(out) :: start_time !! system_clock count captured here; pass through to print_done to
            !! report this case's wall-clock duration.
        character(len=32) :: num_s, total_s

        write(num_s, '(i0)') test_num
        write(total_s, '(i0)') total
        write(output_unit, '(a)') "Running test "//trim(num_s)//" of "//trim(total_s)//": "//label
        call system_clock(count=start_time)
    end subroutine print_start

    subroutine print_done(test_num, total, label, start_time)
        integer, intent(in) :: test_num, total
        character(len=*), intent(in) :: label
        integer(int64), intent(in) :: start_time !! system_clock count from the matching print_start call.
        character(len=32) :: num_s, total_s, elapsed_s
        integer(int64) :: end_time, count_rate

        write(num_s, '(i0)') test_num
        write(total_s, '(i0)') total
        call system_clock(count=end_time, count_rate=count_rate)
        write(elapsed_s, '(f0.3)') real(end_time - start_time, real64) / real(count_rate, real64)
        if (elapsed_s(1:1) == ".") elapsed_s = "0"//trim(elapsed_s)
        write(output_unit, '(a)') "Finished test "//trim(num_s)//" of "//trim(total_s)//": "//label// &
            " -- PASSED ("//trim(adjustl(elapsed_s))//"s)"
    end subroutine print_done

    subroutine print_skipped(test_num, total, label, gb, max_gb)
        integer, intent(in) :: test_num, total
        character(len=*), intent(in) :: label
        real(real64), intent(in) :: gb, max_gb
        character(len=32) :: num_s, total_s, gb_s, max_gb_s

        write(num_s, '(i0)') test_num
        write(total_s, '(i0)') total
        write(gb_s, '(es10.3)') gb
        write(max_gb_s, '(es10.3)') max_gb
        write(output_unit, '(a)') "Skipped test "//trim(num_s)//" of "//trim(total_s)//": "//label// &
            " -- expected size "//trim(adjustl(gb_s))//" GB exceeds max_size_gb "//trim(adjustl(max_gb_s))//" GB"
    end subroutine print_skipped

    subroutine delete_file(filename)
        character(len=*), intent(in) :: filename
        integer :: unit, ios

        open(newunit=unit, file=filename, status="old", iostat=ios)
        if (ios == 0) close(unit, status="delete")
    end subroutine delete_file

    !> True if `row` should be read via the integer(int64) row_index specific of
    !> parquet_read_array_row_mode (needed once `row` itself exceeds huge(1_int32)); false if the
    !> plain integer(int32) specific is safe/sufficient. Exercising both specifics (not just the
    !> new int64 one) across the row-mode spot checks below is deliberate regression coverage.
    function needs_int64_row_index(row) result(needed)
        integer(int64), intent(in) :: row
        logical :: needed
        needed = row > int(huge(1_int32), kind=int64)
    end function needs_int64_row_index

    function expected_int32(flat_idx) result(v)
        integer(int64), intent(in) :: flat_idx
        integer(int32) :: v
        v = int(mod(flat_idx - 1_int64, 1000000_int64), kind=int32)
    end function expected_int32

    function expected_int64(flat_idx) result(v)
        integer(int64), intent(in) :: flat_idx
        integer(int64) :: v
        v = flat_idx
    end function expected_int64

    function expected_float32(flat_idx) result(v)
        integer(int64), intent(in) :: flat_idx
        real(real32) :: v
        v = real(mod(flat_idx - 1_int64, 1000_int64), kind=real32) + 0.25_real32
    end function expected_float32

    function expected_float64(flat_idx) result(v)
        integer(int64), intent(in) :: flat_idx
        real(real64) :: v
        v = real(mod(flat_idx - 1_int64, 1000_int64), kind=real64) + 0.5_real64
    end function expected_float64

    function expected_logical(flat_idx) result(v)
        integer(int64), intent(in) :: flat_idx
        logical :: v
        v = (mod(flat_idx - 1_int64, 2_int64) == 0_int64)
    end function expected_logical

    !> Deterministic, variable-length string content for flat index `flat_idx`: cycles the
    !! (trimmed) length through 0..MAX_STRING_LEN (inclusive, so zero-length strings occur
    !! periodically, once every MAX_STRING_LEN+1 rows) rather than a fixed width, so the compact
    !! parquet_string_column path is genuinely exercised with variable-length rows rather than
    !! rows that all happen to fit a fixed slot. Remaining characters up to MAX_STRING_LEN are
    !! left blank (trimmed away by callers that want the exact, unpadded content).
    function expected_string(flat_idx) result(s)
        integer(int64), intent(in) :: flat_idx
        character(len=MAX_STRING_LEN) :: s
        integer(int64) :: v
        integer :: slen, k

        slen = int(mod(flat_idx - 1_int64, int(MAX_STRING_LEN, int64) + 1_int64))
        v = mod(flat_idx - 1_int64, 1000000_int64)
        s = ''
        do k = 1, slen
            s(k:k) = achar(iachar('0') + int(mod(v + int(k - 1, int64), 10_int64)))
        end do
    end function expected_string

    !==============================================================
    ! int32
    !==============================================================

    subroutine int32_vector_case(test_num, total, nrows, col_size, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows, col_size
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32), allocatable :: values(:,:), row_buf(:)
        integer(int64) :: i, j, flat, nrows_back, total_elems, check_rows(4)
        integer :: col_size_back, n_checks, k
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("int32", "vector", nrows, col_size)
        gb = estimated_gb(nrows, col_size, BYTES_INT32)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(col_size, nrows))
        do i = 1_int64, nrows
            do j = 1_int64, col_size
                values(j, i) = expected_int32((i - 1_int64) * col_size + j)
            end do
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch after read-back"
        call parquet_get_col_size(reader, COLNAME, col_size_back)
        if (int(col_size_back, kind=int64) /= col_size) error stop "test_large_scale: "//label//": col_size mismatch"
        call parquet_get_column_total_elements(reader, COLNAME, total_elems)
        if (total_elems /= nrows * col_size) error stop "test_large_scale: "//label//": total element count mismatch"

        n_checks = 3
        check_rows(1) = 1_int64
        check_rows(2) = nrows / 2_int64 + 1_int64
        check_rows(3) = nrows
        if (needs_int64_row_index(nrows)) then
            n_checks = 4
            check_rows(4) = int(huge(1_int32), kind=int64) + 1_int64
        end if

        allocate(row_buf(col_size))
        do k = 1, n_checks
            if (needs_int64_row_index(check_rows(k))) then
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, check_rows(k))
            else
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, int(check_rows(k), kind=int32))
            end if
            do j = 1_int64, col_size
                flat = (check_rows(k) - 1_int64) * col_size + j
                if (row_buf(j) /= expected_int32(flat)) then
                    error stop "test_large_scale: "//label//": value mismatch at a spot-checked row"
                end if
            end do
        end do
        deallocate(row_buf)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine int32_vector_case

    subroutine int32_scalar_case(test_num, total, nrows, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32), allocatable :: values(:), back(:)
        integer(int64) :: i, nrows_back
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("int32", "scalar", nrows, 1_int64)
        gb = estimated_gb(nrows, 1_int64, BYTES_INT32)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(nrows))
        do i = 1_int64, nrows
            values(i) = expected_int32(i)
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch"

        allocate(back(nrows))
        call parquet_read_column(reader, COLNAME, back)
        do i = 1_int64, nrows
            if (back(i) /= expected_int32(i)) error stop "test_large_scale: "//label//": value mismatch on full read-back"
        end do
        deallocate(back)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine int32_scalar_case

    !==============================================================
    ! int64
    !==============================================================

    subroutine int64_vector_case(test_num, total, nrows, col_size, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows, col_size
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int64), allocatable :: values(:,:), row_buf(:)
        integer(int64) :: i, j, flat, nrows_back, total_elems, check_rows(4)
        integer :: col_size_back, n_checks, k
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("int64", "vector", nrows, col_size)
        gb = estimated_gb(nrows, col_size, BYTES_INT64)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(col_size, nrows))
        do i = 1_int64, nrows
            do j = 1_int64, col_size
                values(j, i) = expected_int64((i - 1_int64) * col_size + j)
            end do
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch after read-back"
        call parquet_get_col_size(reader, COLNAME, col_size_back)
        if (int(col_size_back, kind=int64) /= col_size) error stop "test_large_scale: "//label//": col_size mismatch"
        call parquet_get_column_total_elements(reader, COLNAME, total_elems)
        if (total_elems /= nrows * col_size) error stop "test_large_scale: "//label//": total element count mismatch"

        n_checks = 3
        check_rows(1) = 1_int64
        check_rows(2) = nrows / 2_int64 + 1_int64
        check_rows(3) = nrows
        if (needs_int64_row_index(nrows)) then
            n_checks = 4
            check_rows(4) = int(huge(1_int32), kind=int64) + 1_int64
        end if

        allocate(row_buf(col_size))
        do k = 1, n_checks
            if (needs_int64_row_index(check_rows(k))) then
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, check_rows(k))
            else
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, int(check_rows(k), kind=int32))
            end if
            do j = 1_int64, col_size
                flat = (check_rows(k) - 1_int64) * col_size + j
                if (row_buf(j) /= expected_int64(flat)) then
                    error stop "test_large_scale: "//label//": value mismatch at a spot-checked row"
                end if
            end do
        end do
        deallocate(row_buf)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine int64_vector_case

    subroutine int64_scalar_case(test_num, total, nrows, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int64), allocatable :: values(:), back(:)
        integer(int64) :: i, nrows_back
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("int64", "scalar", nrows, 1_int64)
        gb = estimated_gb(nrows, 1_int64, BYTES_INT64)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(nrows))
        do i = 1_int64, nrows
            values(i) = expected_int64(i)
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch"

        allocate(back(nrows))
        call parquet_read_column(reader, COLNAME, back)
        do i = 1_int64, nrows
            if (back(i) /= expected_int64(i)) error stop "test_large_scale: "//label//": value mismatch on full read-back"
        end do
        deallocate(back)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine int64_scalar_case

    !==============================================================
    ! float32
    !==============================================================

    subroutine float32_vector_case(test_num, total, nrows, col_size, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows, col_size
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real32), allocatable :: values(:,:), row_buf(:)
        integer(int64) :: i, j, flat, nrows_back, total_elems, check_rows(4)
        integer :: col_size_back, n_checks, k
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("float32", "vector", nrows, col_size)
        gb = estimated_gb(nrows, col_size, BYTES_FLOAT32)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(col_size, nrows))
        do i = 1_int64, nrows
            do j = 1_int64, col_size
                values(j, i) = expected_float32((i - 1_int64) * col_size + j)
            end do
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch after read-back"
        call parquet_get_col_size(reader, COLNAME, col_size_back)
        if (int(col_size_back, kind=int64) /= col_size) error stop "test_large_scale: "//label//": col_size mismatch"
        call parquet_get_column_total_elements(reader, COLNAME, total_elems)
        if (total_elems /= nrows * col_size) error stop "test_large_scale: "//label//": total element count mismatch"

        n_checks = 3
        check_rows(1) = 1_int64
        check_rows(2) = nrows / 2_int64 + 1_int64
        check_rows(3) = nrows
        if (needs_int64_row_index(nrows)) then
            n_checks = 4
            check_rows(4) = int(huge(1_int32), kind=int64) + 1_int64
        end if

        allocate(row_buf(col_size))
        do k = 1, n_checks
            if (needs_int64_row_index(check_rows(k))) then
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, check_rows(k))
            else
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, int(check_rows(k), kind=int32))
            end if
            do j = 1_int64, col_size
                flat = (check_rows(k) - 1_int64) * col_size + j
                if (row_buf(j) /= expected_float32(flat)) then
                    error stop "test_large_scale: "//label//": value mismatch at a spot-checked row"
                end if
            end do
        end do
        deallocate(row_buf)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine float32_vector_case

    subroutine float32_scalar_case(test_num, total, nrows, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real32), allocatable :: values(:), back(:)
        integer(int64) :: i, nrows_back
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("float32", "scalar", nrows, 1_int64)
        gb = estimated_gb(nrows, 1_int64, BYTES_FLOAT32)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(nrows))
        do i = 1_int64, nrows
            values(i) = expected_float32(i)
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch"

        allocate(back(nrows))
        call parquet_read_column(reader, COLNAME, back)
        do i = 1_int64, nrows
            if (back(i) /= expected_float32(i)) error stop "test_large_scale: "//label//": value mismatch on full read-back"
        end do
        deallocate(back)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine float32_scalar_case

    !==============================================================
    ! float64
    !==============================================================

    subroutine float64_vector_case(test_num, total, nrows, col_size, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows, col_size
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real64), allocatable :: values(:,:), row_buf(:)
        integer(int64) :: i, j, flat, nrows_back, total_elems, check_rows(4)
        integer :: col_size_back, n_checks, k
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("float64", "vector", nrows, col_size)
        gb = estimated_gb(nrows, col_size, BYTES_FLOAT64)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(col_size, nrows))
        do i = 1_int64, nrows
            do j = 1_int64, col_size
                values(j, i) = expected_float64((i - 1_int64) * col_size + j)
            end do
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch after read-back"
        call parquet_get_col_size(reader, COLNAME, col_size_back)
        if (int(col_size_back, kind=int64) /= col_size) error stop "test_large_scale: "//label//": col_size mismatch"
        call parquet_get_column_total_elements(reader, COLNAME, total_elems)
        if (total_elems /= nrows * col_size) error stop "test_large_scale: "//label//": total element count mismatch"

        n_checks = 3
        check_rows(1) = 1_int64
        check_rows(2) = nrows / 2_int64 + 1_int64
        check_rows(3) = nrows
        if (needs_int64_row_index(nrows)) then
            n_checks = 4
            check_rows(4) = int(huge(1_int32), kind=int64) + 1_int64
        end if

        allocate(row_buf(col_size))
        do k = 1, n_checks
            if (needs_int64_row_index(check_rows(k))) then
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, check_rows(k))
            else
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, int(check_rows(k), kind=int32))
            end if
            do j = 1_int64, col_size
                flat = (check_rows(k) - 1_int64) * col_size + j
                if (row_buf(j) /= expected_float64(flat)) then
                    error stop "test_large_scale: "//label//": value mismatch at a spot-checked row"
                end if
            end do
        end do
        deallocate(row_buf)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine float64_vector_case

    subroutine float64_scalar_case(test_num, total, nrows, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real64), allocatable :: values(:), back(:)
        integer(int64) :: i, nrows_back
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("float64", "scalar", nrows, 1_int64)
        gb = estimated_gb(nrows, 1_int64, BYTES_FLOAT64)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(nrows))
        do i = 1_int64, nrows
            values(i) = expected_float64(i)
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch"

        allocate(back(nrows))
        call parquet_read_column(reader, COLNAME, back)
        do i = 1_int64, nrows
            if (back(i) /= expected_float64(i)) error stop "test_large_scale: "//label//": value mismatch on full read-back"
        end do
        deallocate(back)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine float64_scalar_case

    !==============================================================
    ! logical
    !==============================================================

    subroutine logical_vector_case(test_num, total, nrows, col_size, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows, col_size
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        logical, allocatable :: values(:,:), row_buf(:)
        integer(int64) :: i, j, flat, nrows_back, total_elems, check_rows(4)
        integer :: col_size_back, n_checks, k
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("logical", "vector", nrows, col_size)
        gb = estimated_gb(nrows, col_size, BYTES_LOGICAL)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(col_size, nrows))
        do i = 1_int64, nrows
            do j = 1_int64, col_size
                values(j, i) = expected_logical((i - 1_int64) * col_size + j)
            end do
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch after read-back"
        call parquet_get_col_size(reader, COLNAME, col_size_back)
        if (int(col_size_back, kind=int64) /= col_size) error stop "test_large_scale: "//label//": col_size mismatch"
        call parquet_get_column_total_elements(reader, COLNAME, total_elems)
        if (total_elems /= nrows * col_size) error stop "test_large_scale: "//label//": total element count mismatch"

        n_checks = 3
        check_rows(1) = 1_int64
        check_rows(2) = nrows / 2_int64 + 1_int64
        check_rows(3) = nrows
        if (needs_int64_row_index(nrows)) then
            n_checks = 4
            check_rows(4) = int(huge(1_int32), kind=int64) + 1_int64
        end if

        allocate(row_buf(col_size))
        do k = 1, n_checks
            if (needs_int64_row_index(check_rows(k))) then
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, check_rows(k))
            else
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, int(check_rows(k), kind=int32))
            end if
            do j = 1_int64, col_size
                flat = (check_rows(k) - 1_int64) * col_size + j
                if (row_buf(j) .neqv. expected_logical(flat)) then
                    error stop "test_large_scale: "//label//": value mismatch at a spot-checked row"
                end if
            end do
        end do
        deallocate(row_buf)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine logical_vector_case

    subroutine logical_scalar_case(test_num, total, nrows, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        logical, allocatable :: values(:), back(:)
        integer(int64) :: i, nrows_back
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("logical", "scalar", nrows, 1_int64)
        gb = estimated_gb(nrows, 1_int64, BYTES_LOGICAL)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(nrows))
        do i = 1_int64, nrows
            values(i) = expected_logical(i)
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch"

        allocate(back(nrows))
        call parquet_read_column(reader, COLNAME, back)
        do i = 1_int64, nrows
            if (back(i) .neqv. expected_logical(i)) then
                error stop "test_large_scale: "//label//": value mismatch on full read-back"
            end if
        end do
        deallocate(back)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine logical_scalar_case

    !==============================================================
    ! parquet_string_column (compact scalar string column type; scalar-only, no vector case)
    !==============================================================

    subroutine parquet_string_column_case(test_num, total, nrows, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_string_column) :: values, back
        integer(int64) :: i, nrows_back
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("parquet_string_column", "scalar", nrows, 1_int64)
        gb = estimated_gb(nrows, 1_int64, BYTES_STRING)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        call values%reserve(nrows, nrows * int(MAX_STRING_LEN, int64))
        do i = 1_int64, nrows
            call values%append_string(trim(expected_string(i)))
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        call values%clear()

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch"

        call parquet_read_column(reader, COLNAME, back)
        if (back%size() /= nrows) error stop "test_large_scale: "//label//": size mismatch on read-back"
        ! equals() compares directly against the column's own byte buffer (no per-row
        ! allocation, unlike get()), so a full nrows-row content check stays cheap at scale.
        do i = 1_int64, nrows
            if (back%is_null(i)) error stop "test_large_scale: "//label//": unexpected null on read-back"
            if (.not. back%equals(i, trim(expected_string(i)))) then
                error stop "test_large_scale: "//label//": value mismatch on full read-back"
            end if
        end do
        call back%clear()

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine parquet_string_column_case

    !==============================================================
    ! string
    !==============================================================

    subroutine string_vector_case(test_num, total, nrows, col_size, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows, col_size
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=MAX_STRING_LEN), allocatable :: values(:,:), row_buf(:)
        integer(int64) :: i, j, flat, nrows_back, total_elems, check_rows(4)
        integer :: col_size_back, n_checks, k
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("string", "vector", nrows, col_size)
        gb = estimated_gb(nrows, col_size, BYTES_STRING)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(col_size, nrows))
        do i = 1_int64, nrows
            do j = 1_int64, col_size
                values(j, i) = expected_string((i - 1_int64) * col_size + j)
            end do
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch after read-back"
        call parquet_get_col_size(reader, COLNAME, col_size_back)
        if (int(col_size_back, kind=int64) /= col_size) error stop "test_large_scale: "//label//": col_size mismatch"
        call parquet_get_column_total_elements(reader, COLNAME, total_elems)
        if (total_elems /= nrows * col_size) error stop "test_large_scale: "//label//": total element count mismatch"

        n_checks = 3
        check_rows(1) = 1_int64
        check_rows(2) = nrows / 2_int64 + 1_int64
        check_rows(3) = nrows
        if (needs_int64_row_index(nrows)) then
            n_checks = 4
            check_rows(4) = int(huge(1_int32), kind=int64) + 1_int64
        end if

        allocate(row_buf(col_size))
        do k = 1, n_checks
            if (needs_int64_row_index(check_rows(k))) then
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, check_rows(k))
            else
                call parquet_read_array_row_mode(reader, COLNAME, row_buf, int(check_rows(k), kind=int32))
            end if
            do j = 1_int64, col_size
                flat = (check_rows(k) - 1_int64) * col_size + j
                if (row_buf(j) /= expected_string(flat)) then
                    error stop "test_large_scale: "//label//": value mismatch at a spot-checked row"
                end if
            end do
        end do
        deallocate(row_buf)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine string_vector_case

    subroutine string_scalar_case(test_num, total, nrows, max_gb)
        integer, intent(inout) :: test_num
        integer, intent(in) :: total
        integer(int64), intent(in) :: nrows
        real(real64), intent(in) :: max_gb
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        character(len=MAX_STRING_LEN), allocatable :: values(:), back(:)
        integer(int64) :: i, nrows_back
        real(real64) :: gb
        integer(int64) :: start_time
        character(len=:), allocatable :: label

        test_num = test_num + 1
        label = case_label("string", "scalar", nrows, 1_int64)
        gb = estimated_gb(nrows, 1_int64, BYTES_STRING)
        if (gb > max_gb) then
            call print_skipped(test_num, total, label, gb, max_gb)
            return
        end if
        call print_start(test_num, total, label, start_time)

        allocate(values(nrows))
        do i = 1_int64, nrows
            values(i) = expected_string(i)
        end do

        call parquet_open_writer(writer, TMPFILE)
        call parquet_write_column(writer, COLNAME, values)
        call parquet_close_writer(writer)
        deallocate(values)

        call parquet_open_reader(reader, TMPFILE)
        call parquet_get_nrows(reader, nrows_back)
        if (nrows_back /= nrows) error stop "test_large_scale: "//label//": nrows mismatch"

        allocate(back(nrows))
        call parquet_read_column(reader, COLNAME, back)
        do i = 1_int64, nrows
            if (back(i) /= expected_string(i)) error stop "test_large_scale: "//label//": value mismatch on full read-back"
        end do
        deallocate(back)

        call parquet_close_reader(reader)
        call delete_file(TMPFILE)
        call print_done(test_num, total, label, start_time)
    end subroutine string_scalar_case

end program test_large_scale
