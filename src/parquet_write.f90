!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
submodule (parquet) parquet_write
contains

    !> Every parquet_write_column variant calls this first: writer%handle is
    !> c_null_ptr until parquet_open_writer sets it, and every C++ entry point
    !> dereferences the handle immediately (see ConcurrencyGuard in
    !> parquet_wrapper.cpp) with no null check of its own -- calling in with an
    !> unopened writer previously crashed with an unhelpful SIGSEGV instead of
    !> a clean, diagnosable error.
    subroutine check_writer_open(writer)
        type(parquet_writer), intent(in) :: writer
        if (.not. c_associated(writer%handle)) then
            error stop "parquet_write_column: writer has not been opened (call parquet_open_writer first)"
        end if
    end subroutine check_writer_open

    !> The name actually written to the parquet file/VOTable header for
    !> `col`: its output_name if set, else its (internal) name. Falling back
    !> to name defensively handles a parquet_column_type built without going
    !> through parquet_read_maml (output_name left unallocated).
    function parquet_column_output_name(col) result(output_name)
        type(parquet_column_type), intent(in) :: col
        character(len=:), allocatable :: output_name

        output_name = col%name
        if (allocated(col%output_name)) then
            if (len_trim(col%output_name) > 0) output_name = col%output_name
        end if
    end function parquet_column_output_name

    !> Resolves the caller-facing (internal) column `name` used in a
    !> parquet_write_column call to the name that should actually be passed
    !> to the C++ append_* calls -- its col_map:-renamed output_name, or
    !> `name` itself if there's no schema (schema-less writer) or no match.
    function parquet_resolve_output_name(writer, name) result(output_name)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: output_name
        integer :: idx

        idx = parquet_get_enabled_column_index(writer, name)
        if (idx > 0) then
            output_name = parquet_column_output_name(writer%enabled_columns(idx))
        else
            output_name = name
        end if
    end function parquet_resolve_output_name

    module procedure parquet_get_enabled_column_index
        integer :: i

        parquet_get_enabled_column_index = 0
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                parquet_get_enabled_column_index = i
                return
            end if
        end do
    end procedure parquet_get_enabled_column_index

    module procedure parquet_get_defined_column_index
        integer :: i

        parquet_get_defined_column_index = 0
        if (.not. allocated(writer%all_columns)) return ! GCOVR_EXCL_LINE

        do i = 1, size(writer%all_columns)
            if (trim(writer%all_columns(i)%name) == trim(name)) then
                parquet_get_defined_column_index = i
                return
            end if
        end do
    end procedure parquet_get_defined_column_index

    module procedure parquet_is_type_compatible
        character(len=:), allocatable :: schema_type, write_type

        schema_type = trim(actual_type)
        write_type = trim(expected_type)

        select case (schema_type)
        case ("boolean")
            parquet_is_type_compatible = write_type == "boolean" .or. write_type == "bool8"
        case ("float32", "float64")
            parquet_is_type_compatible = write_type == "float32" .or. write_type == "float64" .or. &
                                          write_type == "int32" .or. write_type == "int64"
        case ("int32", "int64")
            parquet_is_type_compatible = write_type == "int32" .or. write_type == "int64" .or. &
                                          write_type == "float32" .or. write_type == "float64"
        case default
            parquet_is_type_compatible = write_type == schema_type
        end select
    end procedure parquet_is_type_compatible

    function parquet_get_schema_type(writer, name) result(schema_type)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: schema_type
        integer :: idx

        schema_type = ""
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        schema_type = trim(writer%all_columns(idx)%data_type)
    end function parquet_get_schema_type

    function parquet_narrow_int64_to_int32(name, src) result(dst)
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: src(:)
        integer(int32), allocatable :: dst(:)
        integer :: i

        allocate(dst(size(src)))
        do i = 1, size(src)
            if (src(i) < -huge(0_int32) - 1_int64 .or. src(i) > huge(0_int32)) then
                error stop "parquet_write_column: int64 value out of int32 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int32)
        end do
    end function parquet_narrow_int64_to_int32

    function parquet_float64_to_int32(name, src) result(dst)
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: src(:)
        integer(int32), allocatable :: dst(:)
        integer :: i

        allocate(dst(size(src)))
        do i = 1, size(src)
            if (src(i) /= anint(src(i))) then
                error stop "parquet_write_column: non-integral float value written to int column " // trim(name)
            end if
            if (src(i) < -real(huge(0_int32), real64) - 1.0_real64 .or. src(i) > real(huge(0_int32), real64)) then
                error stop "parquet_write_column: float value out of int32 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int32)
        end do
    end function parquet_float64_to_int32

    function parquet_float64_to_int64(name, src) result(dst)
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: src(:)
        integer(int64), allocatable :: dst(:)
        integer :: i

        allocate(dst(size(src)))
        do i = 1, size(src)
            if (src(i) /= anint(src(i))) then
                error stop "parquet_write_column: non-integral float value written to int column " // trim(name)
            end if
            if (src(i) < -real(huge(0_int64), real64) .or. src(i) >= real(huge(0_int64), real64)) then
                error stop "parquet_write_column: float value out of int64 range for column " // trim(name)
            end if
            dst(i) = int(src(i), kind=int64)
        end do
    end function parquet_float64_to_int64

    subroutine parquet_append_as_schema_int32(writer, name, values, nrows, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        integer(int32), intent(in) :: values(:)
        integer(c_long_long), intent(in) :: nrows, asize
        type(c_ptr), intent(in) :: valid_ptr
        character(len=:), allocatable :: schema_type
        integer(int64), allocatable :: i64values(:)
        real(real32), allocatable :: f32values(:)
        real(real64), allocatable :: f64values(:)

        schema_type = parquet_get_schema_type(writer, name)
        select case (schema_type)
        case ("int64")
            allocate(i64values(size(values)))
            i64values = int(values, kind=int64)
            call parquet_append_int64_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), i64values, nrows, asize, valid_ptr)
        case ("float32")
            allocate(f32values(size(values)))
            f32values = real(values, kind=real32)
            call parquet_append_float32_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), f32values, nrows, asize, valid_ptr)
        case ("float64")
            allocate(f64values(size(values)))
            f64values = real(values, kind=real64)
            call parquet_append_float64_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), f64values, nrows, asize, valid_ptr)
        case default
            call parquet_append_int32_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), values, nrows, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_int32

    subroutine parquet_append_as_schema_int64(writer, name, values, nrows, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: values(:)
        integer(c_long_long), intent(in) :: nrows, asize
        type(c_ptr), intent(in) :: valid_ptr
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        real(real32), allocatable :: f32values(:)
        real(real64), allocatable :: f64values(:)

        schema_type = parquet_get_schema_type(writer, name)
        select case (schema_type)
        case ("int32")
            i32values = parquet_narrow_int64_to_int32(name, values)
            call parquet_append_int32_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), i32values, nrows, asize, valid_ptr)
        case ("float32")
            allocate(f32values(size(values)))
            f32values = real(values, kind=real32)
            call parquet_append_float32_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), f32values, nrows, asize, valid_ptr)
        case ("float64")
            allocate(f64values(size(values)))
            f64values = real(values, kind=real64)
            call parquet_append_float64_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), f64values, nrows, asize, valid_ptr)
        case default
            call parquet_append_int64_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), values, nrows, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_int64

    subroutine parquet_append_as_schema_float32(writer, name, values, nrows, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        real(real32), intent(in) :: values(:)
        integer(c_long_long), intent(in) :: nrows, asize
        type(c_ptr), intent(in) :: valid_ptr
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        integer(int64), allocatable :: i64values(:)
        real(real64), allocatable :: f64values(:)

        schema_type = parquet_get_schema_type(writer, name)
        select case (schema_type)
        case ("int32")
            i32values = parquet_float64_to_int32(name, real(values, kind=real64))
            call parquet_append_int32_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), i32values, nrows, asize, valid_ptr)
        case ("int64")
            i64values = parquet_float64_to_int64(name, real(values, kind=real64))
            call parquet_append_int64_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), i64values, nrows, asize, valid_ptr)
        case ("float64")
            allocate(f64values(size(values)))
            f64values = real(values, kind=real64)
            call parquet_append_float64_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), f64values, nrows, asize, valid_ptr)
        case default
            call parquet_append_float32_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), values, nrows, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_float32

    subroutine parquet_append_as_schema_float64(writer, name, values, nrows, asize, valid_ptr)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values(:)
        integer(c_long_long), intent(in) :: nrows, asize
        type(c_ptr), intent(in) :: valid_ptr
        character(len=:), allocatable :: schema_type
        integer(int32), allocatable :: i32values(:)
        integer(int64), allocatable :: i64values(:)
        real(real32), allocatable :: f32values(:)

        schema_type = parquet_get_schema_type(writer, name)
        select case (schema_type)
        case ("int32")
            i32values = parquet_float64_to_int32(name, values)
            call parquet_append_int32_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), i32values, nrows, asize, valid_ptr)
        case ("int64")
            i64values = parquet_float64_to_int64(name, values)
            call parquet_append_int64_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), i64values, nrows, asize, valid_ptr)
        case ("float32")
            allocate(f32values(size(values)))
            f32values = real(values, kind=real32)
            call parquet_append_float32_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), f32values, nrows, asize, valid_ptr)
        case default
            call parquet_append_float64_column(writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), values, nrows, asize, valid_ptr)
        end select
    end subroutine parquet_append_as_schema_float64

    !> "" for a schema-less writer with no output filename set either;
    !> otherwise " (file: X)", " (maml: Y)", or " (file: X, maml: Y)"
    !> depending on which of writer%filename/writer%maml_name are set
    !> (writer%filename is set by parquet_open_writer as soon as the writer
    !> is created, so it is virtually always present here; writer%maml_name
    !> is unset only for a schema assembled by hand without going through
    !> schema%init/parquet_schema(...), which always set it to
    !> "internal:<table>"). Appended to parquet_write_column's
    !> schema-mismatch and completeness errors so they're diagnosable
    !> without needing to know which parquet_open_writer call produced them.
    function writer_context_suffix(writer) result(suffix)
        type(parquet_writer), intent(in) :: writer
        character(len=:), allocatable :: suffix
        character(len=:), allocatable :: parts

        parts = ""
        if (allocated(writer%filename)) then
            if (len_trim(writer%filename) > 0) parts = "file: " // trim(writer%filename)
        end if
        if (allocated(writer%maml_name)) then
            if (len_trim(writer%maml_name) > 0) then
                if (len(parts) > 0) then
                    parts = parts // ", maml: " // trim(writer%maml_name)
                else
                    parts = "maml: " // trim(writer%maml_name) ! GCOVR_EXCL_LINE
                end if
            end if
        end if

        suffix = ""
        if (len(parts) > 0) suffix = " (" // parts // ")"
    end function writer_context_suffix

    module procedure parquet_assert_column_type
        integer :: idx

        if (.not. writer%is_schema_enforced) return

        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) then
            error stop "parquet_write_column: column not defined in parquet_open_writer: " // &
                                  trim(name) // writer_context_suffix(writer)
        end if

        if (.not. parquet_is_type_compatible(writer%all_columns(idx)%data_type, expected_type)) then
            error stop "parquet_write_column: type mismatch for column " // trim(name) // &
                                  " (expected " // trim(expected_type) // ", got " // &
                                  trim(writer%all_columns(idx)%data_type) // ")" // &
                                  writer_context_suffix(writer)
        end if
    end procedure parquet_assert_column_type

    module procedure parquet_mark_column_written
        integer :: idx

        if (.not. allocated(writer%enabled_columns)) then
            call parquet_check_and_mark_written_name(writer, name)
            return
        end if

        idx = parquet_get_enabled_column_index(writer, name)
        if (idx == 0) return

        if (writer%write_counts(idx) > 0) then
            error stop "parquet_write_column: column written more than once: " // trim(name) // writer_context_suffix(writer)
        end if
        writer%write_counts(idx) = writer%write_counts(idx) + 1
    end procedure parquet_mark_column_written

    !> Schema-less writer (no cinfo, so enabled_columns/write_counts above
    !> don't exist and can't track this): checks `name` against every name
    !> already written by this writer, error stopping on a repeat, then
    !> records `name` as written. Grows written_names by one each call --
    !> fine here since it only holds as many entries as columns actually
    !> written (typically a handful to a few dozen), unlike a per-row buffer.
    subroutine parquet_check_and_mark_written_name(writer, name)
        type(parquet_writer), intent(inout) :: writer
        character(len=*), intent(in) :: name
        character(len=256), allocatable :: tmp(:)
        integer :: n, i

        if (allocated(writer%written_names)) then
            do i = 1, size(writer%written_names)
                if (trim(writer%written_names(i)) == trim(name)) then
                    error stop "parquet_write_column: column written more than once: " // trim(name) // &
                        writer_context_suffix(writer)
                end if
            end do
            n = size(writer%written_names)
            allocate(tmp(n + 1))
            tmp(1:n) = writer%written_names
            tmp(n + 1) = trim(name)
            call move_alloc(tmp, writer%written_names)
        else
            allocate(writer%written_names(1))
            writer%written_names(1) = trim(name)
        end if
    end subroutine parquet_check_and_mark_written_name

    module procedure parquet_is_column_enabled
        parquet_is_column_enabled = .true.
        if (.not. allocated(writer%enabled_columns)) return

        parquet_is_column_enabled = parquet_get_enabled_column_index(writer, name) > 0
    end procedure parquet_is_column_enabled

    module procedure parquet_get_column_col_size
        integer :: i

        parquet_get_column_col_size = 1
        if (.not. allocated(writer%enabled_columns)) return

        do i = 1, size(writer%enabled_columns)
            if (trim(writer%enabled_columns(i)%name) == trim(name)) then
                parquet_get_column_col_size = max(1, writer%enabled_columns(i)%col_size)
                return
            end if
        end do
    end procedure parquet_get_column_col_size

    module procedure parquet_check_row_count
        character(len=32) :: expected_str, got_str

        if (writer%expected_nrows < 0) then
            writer%expected_nrows = nrows
            return
        end if

        if (writer%expected_nrows /= nrows) then
            write(expected_str, '(i0)') writer%expected_nrows
            write(got_str, '(i0)') nrows
            error stop "parquet_write_column: row count mismatch for column " // trim(name) // &
                ": expected " // trim(expected_str) // " rows (from an earlier column) but got " // trim(got_str) // &
                writer_context_suffix(writer)
        end if
    end procedure parquet_check_row_count

    module procedure parquet_open_writer
        integer :: i, k, n_enabled, jc
        character(len=:), allocatable :: compression_name
        integer :: level_value, chunk_size_value
        logical :: comp_ok
        logical :: use_threads_value
        character(len=12), parameter :: valid_compressions(6) = [character(len=12) :: &
            "uncompressed", "snappy", "gzip", "zstd", "brotli", "lz4"]

        writer%handle = create_parquet_writer(trim(filename)//char(0))
        writer%filename = trim(filename)
        writer%is_schema_enforced = present(schema)
        if (present(qc)) writer%qc = qc

        ! Defaults to "snappy" -- Parquet-the-library's own built-in default is
        ! actually "uncompressed" (confirmed in parquet/properties.h), but the
        ! ecosystem convention on top of it (pyarrow, Spark, ...) is snappy,
        ! and this library intentionally follows that convention rather than
        ! the raw library default.
        compression_name = "snappy"
        if (present(compression)) compression_name = parquet_to_lower(trim(compression))

        comp_ok = .false.
        do jc = 1, size(valid_compressions)
            if (trim(compression_name) == trim(valid_compressions(jc))) then
                comp_ok = .true.
                exit
            end if
        end do
        if (.not. comp_ok) then
            error stop "parquet_open_writer: unknown compression codec '" // trim(compression_name) // &
                "' (expected one of: uncompressed, snappy, gzip, zstd, brotli, lz4) (file: " // trim(filename) // ")"
        end if

        level_value = -huge(level_value) - 1 ! Arrow's kUseDefaultCompressionLevel sentinel (INT_MIN):
                                              ! "use the codec's own default".
        if (present(compression_level)) level_value = compression_level

        chunk_size_value = -1 ! <= 0 tells the C++ side "not set": auto-size from the final row count at close time.
        if (present(chunk_size)) chunk_size_value = chunk_size

        use_threads_value = .true.
        if (present(use_threads)) use_threads_value = use_threads

        call parquet_set_writer_options(writer%handle, trim(compression_name)//char(0), &
            int(level_value, kind=c_int), int(chunk_size_value, kind=c_long_long), &
            merge(1_c_int, 0_c_int, use_threads_value))

        if (present(schema)) then
            if (allocated(schema%maml%name)) writer%maml_name = schema%maml%name

            allocate(writer%all_columns(size(schema%cinfo%col)))
            writer%all_columns = schema%cinfo%col

            n_enabled = 0
            do i = 1, size(schema%cinfo%col)
                if (schema%cinfo%col(i)%is_set) n_enabled = n_enabled + 1
            end do

            if (n_enabled > 0) then
                allocate(writer%enabled_columns(n_enabled))
                allocate(writer%write_counts(n_enabled))
                writer%write_counts = 0
                k = 0
            end if

            do i = 1, size(schema%cinfo%col)
                if (schema%cinfo%col(i)%is_set) then
                    k = k + 1
                    writer%enabled_columns(k) = schema%cinfo%col(i)
                    ! Registers the schema under output_name (the column's
                    ! display/file name -- equal to name unless a col_map:
                    ! entry renamed it), not the internal name, since that's
                    ! what append_column's own arguments must also use for
                    ! Arrow to line up the field with its data.
                    call parquet_add_column_info(&
                        writer, &
                        parquet_column_output_name(schema%cinfo%col(i)), &
                        schema%cinfo%col(i)%unit, &
                        schema%cinfo%col(i)%info, &
                        schema%cinfo%col(i)%ucd, &
                        schema%cinfo%col(i)%data_type, &
                        schema%cinfo%col(i)%array_size, &
                        schema%cinfo%col(i)%col_size )
                end if
            end do

            if (allocated(schema%metadata%items)) then
                do i = 1, size(schema%metadata%items)
                    call parquet_add_table_metadata(writer%handle, &
                        trim(schema%metadata%items(i)%key)//char(0), &
                        trim(schema%metadata%items(i)%value)//char(0), &
                        trim(schema%metadata%items(i)%description)//char(0))
                end do
            end if
        end if

        if (present(write_maml)) then
            if (write_maml) then
                if (.not. present(schema)) then
                    error stop "parquet_open_writer: write_maml=.true. requires a schema " // &
                        "(prepared by parquet_parse_maml) to be present (file: " // trim(filename) // ")"
                end if
                if (.not. allocated(schema%metadata%source_maml_lines)) then ! GCOVR_EXCL_START
                    error stop "parquet_open_writer: write_maml=.true. requires a schema " // &
                        "obtained from parquet_parse_maml (no source MAML content found) (file: " // trim(filename) // ")"
                end if ! GCOVR_EXCL_STOP
                block
                    character(len=:), allocatable :: sidecar_lines(:)
                    sidecar_lines = schema%metadata%source_maml_lines
                    call parquet_prune_disabled_fields(sidecar_lines, schema%cinfo)
                    call parquet_write_maml_sidecar(filename, sidecar_lines)
                end block
            end if
        end if
    end procedure parquet_open_writer

    !> Removes, from `lines` (a working copy of metadata%source_maml_lines),
    !> the `fields:` entries whose column is disabled (is_set = .false.) in
    !> `cinfo`, so that a sidecar .maml written via write_maml=.true. only
    !> lists the columns actually present in the .parquet file. Matches source
    !> MAML field blocks to cinfo%col by output_name (the name the source MAML
    !> text itself declares under fields:/name: -- equal to cinfo%col%name
    !> unless a col_map: rename applies); a block runs from its top-level
    !> "- ..." line up to (but not including) the next top-level line. If every
    !> column in cinfo is disabled, lines is left untouched instead of emptying
    !> out fields: entirely, since a MAML file with no fields fails
    !> parquet_validate_maml on the next read.
    subroutine parquet_prune_disabled_fields(lines, cinfo)
        character(len=:), allocatable, intent(inout) :: lines(:)
        type(parquet_column_info), intent(in) :: cinfo
        logical, allocatable :: keep(:)
        character(len=:), allocatable :: tline, key, cvalue, field_name
        character(len=:), allocatable :: new_lines(:)
        integer :: i, j, k, n, idx_fields, block_start, block_end, col_idx, n_keep

        if (.not. allocated(cinfo%col)) return
        if (size(cinfo%col) == 0) return
        if (.not. any(cinfo%col(:)%is_set)) return

        n = size(lines)
        idx_fields = 0
        do i = 1, n
            if (lines(i)(1:1) /= " " .and. trim(adjustl(lines(i))) == "fields:") then
                idx_fields = i
                exit
            end if
        end do
        if (idx_fields == 0) return

        allocate(keep(n))
        keep = .true.

        i = idx_fields + 1
        do while (i <= n)
            if (len_trim(lines(i)) == 0) then
                i = i + 1
                cycle
            end if

            if (lines(i)(1:1) /= " " .and. index(trim(adjustl(lines(i))), "-") /= 1) exit

            if (lines(i)(1:1) /= " ") then
                ! Top-level "- ..." line: start of a new field block. It runs
                ! until the next top-level (non-indented) line.
                block_start = i
                block_end = i
                j = i + 1
                do while (j <= n)
                    if (len_trim(lines(j)) == 0) exit
                    if (lines(j)(1:1) /= " ") exit
                    block_end = j
                    j = j + 1
                end do

                field_name = ""
                do k = block_start, block_end
                    if (k == block_start) then
                        tline = trim(adjustl(lines(k)))
                        tline = trim(adjustl(tline(2:)))
                        if (len_trim(tline) == 0) cycle
                    else
                        tline = trim(adjustl(lines(k)))
                    end if
                    call parquet_split_key_value(tline, key, cvalue)
                    if (len_trim(key) == 0) cycle
                    if (parquet_to_lower(trim(key)) == "name") then
                        field_name = parquet_unquote(cvalue)
                        exit
                    end if
                end do

                col_idx = 0
                if (len_trim(field_name) > 0) then
                    do k = 1, size(cinfo%col)
                        if (trim(parquet_column_output_name(cinfo%col(k))) == trim(field_name)) then
                            col_idx = k
                            exit
                        end if
                    end do
                end if

                if (col_idx > 0) then
                    if (.not. cinfo%col(col_idx)%is_set) keep(block_start:block_end) = .false.
                end if

                i = block_end + 1
                cycle
            end if

            i = i + 1
        end do

        n_keep = count(keep)
        if (n_keep == n) return

        allocate(character(len=len(lines)) :: new_lines(n_keep))
        j = 0
        do i = 1, n
            if (keep(i)) then
                j = j + 1
                new_lines(j) = lines(i)
            end if
        end do

        call move_alloc(new_lines, lines)
    end subroutine parquet_prune_disabled_fields

    !> Writes `lines` to a sidecar .maml file next to `parquet_filename`: the same
    !> path with a trailing ".parquet" replaced by ".maml", or ".maml" appended if
    !> there is no ".parquet" suffix.
    subroutine parquet_write_maml_sidecar(parquet_filename, lines)
        character(len=*), intent(in) :: parquet_filename
        character(len=*), intent(in) :: lines(:)
        character(len=:), allocatable :: maml_filename
        integer :: unit, i, n

        n = len(parquet_filename)
        if (n >= 8) then
            if (parquet_filename(n-7:n) == ".parquet") then
                maml_filename = parquet_filename(1:n-8) // ".maml"
            else
                maml_filename = parquet_filename // ".maml"
            end if
        else
            maml_filename = parquet_filename // ".maml"
        end if

        open(newunit=unit, file=maml_filename, status="replace", action="write", form="formatted")
        do i = 1, size(lines)
            write(unit, '(a)') trim(lines(i))
        end do
        close(unit)
    end subroutine parquet_write_maml_sidecar

    module procedure parquet_add_column_info
        call parquet_add_column_metadata(&
            writer%handle, &
            trim(name)//char(0), &
            trim(unit)//char(0), &
            trim(description)//char(0), &
            trim(ucd)//char(0), &
            trim(data_type)//char(0), &
            int(array_size, kind=c_long_long), &
            int(col_size, kind=c_long_long) )
    end procedure parquet_add_column_info

    logical function parquet_qc_numeric_satisfies(value, bound, op) result(ok)
        real(real64), intent(in) :: value, bound
        character(len=*), intent(in) :: op

        select case (trim(op))
        case (">=")
            ok = value >= bound
        case ("<=")
            ok = value <= bound
        case (">")
            ok = value > bound
        case ("<")
            ok = value < bound
        case default
            ok = .true.
        end select
    end function parquet_qc_numeric_satisfies

    logical function parquet_qc_string_satisfies(value, bound, op) result(ok)
        character(len=*), intent(in) :: value, bound
        character(len=*), intent(in) :: op

        select case (trim(op))
        case (">=")
            ok = value >= bound
        case ("<=")
            ok = value <= bound
        case (">")
            ok = value > bound
        case ("<")
            ok = value < bound
        case default
            ok = .true.
        end select
    end function parquet_qc_string_satisfies

    function parquet_qc_format_real(value) result(text)
        real(real64), intent(in) :: value
        character(len=:), allocatable :: text
        character(len=64) :: buf

        if (value == anint(value) .and. abs(value) < 1.0e15_real64) then
            write(buf, '(i0)') nint(value, kind=int64)
        else
            write(buf, '(g0.7)') value
        end if
        text = trim(adjustl(buf))
    end function parquet_qc_format_real

    function parquet_qc_format_int(value) result(text)
        integer, intent(in) :: value
        character(len=:), allocatable :: text
        character(len=32) :: buf

        write(buf, '(i0)') value
        text = trim(adjustl(buf))
    end function parquet_qc_format_int

    !> If writer%qc is set and the column has a schema-declared qc: min:
    !> and/or max:, checks every element of `values64` where `is_valid_flat`
    !> is .true. against those bounds (absent is_valid means every element
    !> counts, matching the write-side is_valid convention elsewhere) and
    !> prints a single WARNING to stdout naming the column, its declared
    !> bound(s), the observed data range among valid elements, and how many
    !> of them violate at least one bound. Never errors -- writing proceeds
    !> regardless. No-op for a schema-less writer, a column without qc:, or
    !> when there are no valid elements to check at all.
    subroutine parquet_check_qc_numeric(writer, name, values64, is_valid_flat)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        real(real64), intent(in) :: values64(:)
        logical, intent(in) :: is_valid_flat(:)
        integer :: idx, i, n_valid, n_violate
        real(real64) :: min_bound, max_bound, data_min, data_max
        logical :: any_valid, have_min_bound, have_max_bound, ok
        character(len=:), allocatable :: bounds_desc

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. (writer%all_columns(idx)%has_qc_min .or. writer%all_columns(idx)%has_qc_max)) return

        have_min_bound = .false.
        have_max_bound = .false.
        if (writer%all_columns(idx)%has_qc_min) then
            have_min_bound = parquet_qc_numeric_bound( &
                writer%all_columns(idx)%qc_min_raw, writer%all_columns(idx)%data_type, min_bound)
        end if
        if (writer%all_columns(idx)%has_qc_max) then
            have_max_bound = parquet_qc_numeric_bound( &
                writer%all_columns(idx)%qc_max_raw, writer%all_columns(idx)%data_type, max_bound)
        end if
        if (.not. (have_min_bound .or. have_max_bound)) return

        any_valid = .false.
        n_valid = 0
        n_violate = 0
        do i = 1, size(values64)
            if (.not. is_valid_flat(i)) cycle
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = values64(i)
                data_max = values64(i)
                any_valid = .true.
            else
                data_min = min(data_min, values64(i))
                data_max = max(data_max, values64(i))
            end if

            ok = .true.
            if (have_min_bound) ok = ok .and. &
                parquet_qc_numeric_satisfies(values64(i), min_bound, writer%all_columns(idx)%qc_min_op)
            if (have_max_bound) ok = ok .and. &
                parquet_qc_numeric_satisfies(values64(i), max_bound, writer%all_columns(idx)%qc_max_op)
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return

        bounds_desc = ""
        if (have_min_bound) bounds_desc = "min " // trim(writer%all_columns(idx)%qc_min_op) // " " // &
            parquet_qc_format_real(min_bound)
        if (have_max_bound) then
            if (len_trim(bounds_desc) > 0) bounds_desc = bounds_desc // ", "
            bounds_desc = bounds_desc // "max " // trim(writer%all_columns(idx)%qc_max_op) // " " // &
                parquet_qc_format_real(max_bound)
        end if

        print '(a)', "WARNING: qc violation for column '" // trim(name) // "': declared " // bounds_desc // &
            ", data range [" // parquet_qc_format_real(data_min) // ", " // parquet_qc_format_real(data_max) // "], " // &
            parquet_qc_format_int(n_violate) // " of " // parquet_qc_format_int(n_valid) // " valid element(s) out of range"
    end subroutine parquet_check_qc_numeric

    !> Same as parquet_check_qc_numeric but for a "string" column: bounds are
    !> compared as literal Fortran character strings (lexicographic, via the
    !> intrinsic relational operators) rather than parsed as numbers.
    subroutine parquet_check_qc_string(writer, name, values, is_valid_flat)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: values(:)
        logical, intent(in) :: is_valid_flat(:)
        integer :: idx, i, n_valid, n_violate
        logical :: any_valid, ok
        character(len=:), allocatable :: data_min, data_max, bounds_desc

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. (writer%all_columns(idx)%has_qc_min .or. writer%all_columns(idx)%has_qc_max)) return

        any_valid = .false.
        n_valid = 0
        n_violate = 0
        do i = 1, size(values)
            if (.not. is_valid_flat(i)) cycle
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = trim(values(i))
                data_max = trim(values(i))
                any_valid = .true.
            else
                if (trim(values(i)) < data_min) data_min = trim(values(i))
                if (trim(values(i)) > data_max) data_max = trim(values(i))
            end if

            ok = .true.
            if (writer%all_columns(idx)%has_qc_min) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    trim(values(i)), trim(writer%all_columns(idx)%qc_min_raw), writer%all_columns(idx)%qc_min_op)
            end if
            if (writer%all_columns(idx)%has_qc_max) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    trim(values(i)), trim(writer%all_columns(idx)%qc_max_raw), writer%all_columns(idx)%qc_max_op)
            end if
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return

        bounds_desc = ""
        if (writer%all_columns(idx)%has_qc_min) then
            bounds_desc = "min " // trim(writer%all_columns(idx)%qc_min_op) // " '" // &
                trim(writer%all_columns(idx)%qc_min_raw) // "'"
        end if
        if (writer%all_columns(idx)%has_qc_max) then
            if (len_trim(bounds_desc) > 0) bounds_desc = bounds_desc // ", "
            bounds_desc = bounds_desc // "max " // trim(writer%all_columns(idx)%qc_max_op) // " '" // &
                trim(writer%all_columns(idx)%qc_max_raw) // "'"
        end if

        print '(a)', "WARNING: qc violation for column '" // trim(name) // "': declared " // bounds_desc // &
            ", data range ['" // data_min // "', '" // data_max // "'], " // &
            parquet_qc_format_int(n_violate) // " of " // parquet_qc_format_int(n_valid) // " valid element(s) out of range"
    end subroutine parquet_check_qc_string

    !> Errors out if `name`'s column is listed under extra: protected_cols:
    !> (see parquet_parse_protected_cols) and `is_valid_flat` contains any
    !> .false. entry. A no-op for a schema-less writer or an unlisted column.
    subroutine parquet_check_protected(writer, name, is_valid_flat)
        type(parquet_writer), intent(in) :: writer
        character(len=*), intent(in) :: name
        logical, intent(in) :: is_valid_flat(:)
        integer :: idx

        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (writer%all_columns(idx)%is_protected .and. .not. all(is_valid_flat)) then
            error stop "parquet_write_column: column '" // trim(name) // &
                "' is protected (extra: protected_cols:) and cannot contain Null values"
        end if
    end subroutine parquet_check_protected

    !> Builds the int8 validity buffer and c_ptr passed down to the C++
    !> append_* functions from a caller's flattened `is_valid` mask (1 =
    !> valid, 0 = Null); `valid_ptr` stays c_null_ptr (no validity buffer
    !> passed to C++ at all) when `is_valid` was not supplied by the caller,
    !> which keeps every column non-nullable by default -- exactly today's
    !> behavior.
    subroutine parquet_make_valid_buf_write(is_valid_flat, valid_buf, valid_ptr)
        logical, intent(in), optional :: is_valid_flat(:)
        integer(c_int8_t), allocatable, target, intent(out) :: valid_buf(:)
        type(c_ptr), intent(out) :: valid_ptr
        integer :: i

        if (present(is_valid_flat)) then
            allocate(valid_buf(size(is_valid_flat)))
            do i = 1, size(is_valid_flat)
                valid_buf(i) = merge(1_c_int8_t, 0_c_int8_t, is_valid_flat(i))
            end do
            valid_ptr = c_loc(valid_buf)
        else
            valid_ptr = c_null_ptr
        end if
    end subroutine parquet_make_valid_buf_write

    module procedure parquet_write_int32_column
        integer :: asize, nrows, idx
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(values), asize) /= 0) error stop &
            "parquet_write_int32_column: values size is not divisible by col_size for column " // &
            trim(name) // writer_context_suffix(writer)
        nrows = size(values) / asize

        if (present(is_valid)) then
            call parquet_check_protected(writer, name, is_valid)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values, kind=real64), is_valid)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values, kind=real64), spread(.true., 1, size(values)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_as_schema_int32(&
            writer, name, values, int(nrows, kind=c_long_long), int(asize, kind=c_long_long), valid_ptr)
    end procedure parquet_write_int32_column

    module procedure parquet_write_int32_matrix_column
        integer :: asize, nrows, idx
        integer(int32), allocatable :: packed(:)
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        asize = size(values, 1)
        nrows = size(values, 2)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "int32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(values)))
        packed = reshape(values, [size(values)])

        if (present(is_valid)) then
            valid_flat = reshape(is_valid, [size(is_valid)])
            call parquet_check_protected(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), spread(.true., 1, size(packed)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_as_schema_int32(&
            writer, name, packed, int(nrows, kind=c_long_long), int(asize, kind=c_long_long), valid_ptr)
    end procedure parquet_write_int32_matrix_column

    module procedure parquet_write_int64_column
        integer :: asize, nrows, idx
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "int64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(values), asize) /= 0) error stop &
            "parquet_write_int64_column: values size is not divisible by col_size for column " // &
            trim(name) // writer_context_suffix(writer)
        nrows = size(values) / asize

        if (present(is_valid)) then
            call parquet_check_protected(writer, name, is_valid)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values, kind=real64), is_valid)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values, kind=real64), spread(.true., 1, size(values)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_as_schema_int64(&
            writer, name, values, int(nrows, kind=c_long_long), int(asize, kind=c_long_long), valid_ptr)
    end procedure parquet_write_int64_column

    module procedure parquet_write_int64_matrix_column
        integer :: asize, nrows, idx
        integer(int64), allocatable :: packed(:)
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        asize = size(values, 1)
        nrows = size(values, 2)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "int64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(values)))
        packed = reshape(values, [size(values)])

        if (present(is_valid)) then
            valid_flat = reshape(is_valid, [size(is_valid)])
            call parquet_check_protected(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), spread(.true., 1, size(packed)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_as_schema_int64(&
            writer, name, packed, int(nrows, kind=c_long_long), int(asize, kind=c_long_long), valid_ptr)
    end procedure parquet_write_int64_matrix_column

    module procedure parquet_write_float32_column
        integer :: asize, nrows, idx
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(values), asize) /= 0) error stop &
            "parquet_write_float32_column: values size is not divisible by col_size for column " // &
            trim(name) // writer_context_suffix(writer)
        nrows = size(values) / asize

        if (present(is_valid)) then
            call parquet_check_protected(writer, name, is_valid)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values, kind=real64), is_valid)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values, kind=real64), spread(.true., 1, size(values)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_as_schema_float32(&
            writer, name, values, int(nrows, kind=c_long_long), int(asize, kind=c_long_long), valid_ptr)
    end procedure parquet_write_float32_column

    module procedure parquet_write_float32_matrix_column
        integer :: asize, nrows, idx
        real(real32), allocatable :: packed(:)
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        asize = size(values, 1)
        nrows = size(values, 2)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "float32")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(values)))
        packed = reshape(values, [size(values)])

        if (present(is_valid)) then
            valid_flat = reshape(is_valid, [size(is_valid)])
            call parquet_check_protected(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), spread(.true., 1, size(packed)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_as_schema_float32(&
            writer, name, packed, int(nrows, kind=c_long_long), int(asize, kind=c_long_long), valid_ptr)
    end procedure parquet_write_float32_matrix_column

    module procedure parquet_write_float64_column
        integer :: asize, nrows, idx
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "float64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(values), asize) /= 0) error stop &
            "parquet_write_float64_column: values size is not divisible by col_size for column " // &
            trim(name) // writer_context_suffix(writer)
        nrows = size(values) / asize

        if (present(is_valid)) then
            call parquet_check_protected(writer, name, is_valid)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values, kind=real64), is_valid)
            end if
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(values, kind=real64), spread(.true., 1, size(values)))
            end if
        end if
        call parquet_make_valid_buf_write(is_valid, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_as_schema_float64(&
            writer, name, values, int(nrows, kind=c_long_long), int(asize, kind=c_long_long), valid_ptr)
    end procedure parquet_write_float64_column

    module procedure parquet_write_float64_matrix_column
        integer :: asize, nrows, idx
        real(real64), allocatable :: packed(:)
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        asize = size(values, 1)
        nrows = size(values, 2)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "float64")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(packed(size(values)))
        packed = reshape(values, [size(values)])

        if (present(is_valid)) then
            valid_flat = reshape(is_valid, [size(is_valid)])
            call parquet_check_protected(writer, name, valid_flat)
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), valid_flat)
            end if
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            if (writer%qc .and. writer%is_schema_enforced) then
                call parquet_check_qc_numeric(writer, name, real(packed, kind=real64), spread(.true., 1, size(packed)))
            end if
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_as_schema_float64(&
            writer, name, packed, int(nrows, kind=c_long_long), int(asize, kind=c_long_long), valid_ptr)
    end procedure parquet_write_float64_matrix_column

    module procedure parquet_write_logical_column
        integer :: asize, nrows, i, idx
        integer(c_int8_t), allocatable :: bool_data(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "boolean")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        if (mod(size(values), asize) /= 0) error stop &
            "parquet_write_logical_column: values size is not divisible by col_size for column " // &
            trim(name) // writer_context_suffix(writer)
        nrows = size(values) / asize

        allocate(bool_data(size(values)))
        do i = 1, size(values)
            if (values(i)) then
                bool_data(i) = 1_c_int8_t
            else
                bool_data(i) = 0_c_int8_t
            end if
        end do

        if (present(is_valid)) call parquet_check_protected(writer, name, is_valid)
        call parquet_make_valid_buf_write(is_valid, valid_buf, valid_ptr)
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_bool8_column(&
            writer%handle, &
            trim(parquet_resolve_output_name(writer, name))//char(0), &
            bool_data, &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long), &
            valid_ptr )
    end procedure parquet_write_logical_column

    module procedure parquet_write_logical_matrix_column
        integer :: asize, nrows, idx
        integer(c_int8_t), allocatable :: bool_data(:)
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        asize = size(values, 1)
        nrows = size(values, 2)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "boolean")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        allocate(bool_data(size(values)))
        bool_data = merge(1_c_int8_t, 0_c_int8_t, reshape(values, [size(values)]))

        if (present(is_valid)) then
            valid_flat = reshape(is_valid, [size(is_valid)])
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        call parquet_append_bool8_column(&
            writer%handle, &
            trim(parquet_resolve_output_name(writer, name))//char(0), &
            bool_data, &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long), &
            valid_ptr )
    end procedure parquet_write_logical_matrix_column

    module procedure parquet_write_string_column
        character(kind=c_char), allocatable :: packed(:)
        integer :: i, j, k, nrows, item_len, idx, asize, nitems, max_item_len, max_string_len
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = parquet_get_column_col_size(writer, name)
        nitems = size(values)
        if (nitems <= 0) return
        if (mod(nitems, asize) /= 0) error stop &
            "parquet_write_string_column: values size is not divisible by col_size for column " // &
            trim(name) // writer_context_suffix(writer)

        if (writer%is_schema_enforced) then
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval([(len_trim(values(i)), i=1,nitems)])
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_column: string length exceeds declared array_size for column: " // trim(name)
            end if
        end if

        nrows = nitems / asize
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        item_len = len(values(1))
        allocate(packed(item_len*nitems))

        k = 0
        do i = 1, nitems
            do j = 1, item_len
                k = k + 1
                packed(k) = achar(iachar(values(i)(j:j)), kind=c_char)
            end do
        end do

        if (present(is_valid)) then
            call parquet_check_protected(writer, name, is_valid)
            call parquet_check_qc_string(writer, name, values, is_valid)
        else
            call parquet_check_qc_string(writer, name, values, spread(.true., 1, size(values)))
        end if
        call parquet_make_valid_buf_write(is_valid, valid_buf, valid_ptr)

        if (asize == 1) then
            call parquet_append_string_column(&
                writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), &
                packed, &
                int(item_len, kind=c_long_long), &
                int(nrows, kind=c_long_long), &
                valid_ptr )
        else
            call parquet_append_string_array_column(&
                writer%handle, &
                trim(parquet_resolve_output_name(writer, name))//char(0), &
                packed, &
                int(item_len, kind=c_long_long), &
                int(nrows, kind=c_long_long), &
                int(asize, kind=c_long_long), &
                valid_ptr )
        end if
    end procedure parquet_write_string_column

    module procedure parquet_write_string_matrix_column
        character(kind=c_char), allocatable :: packed(:)
        integer :: i, j, k, l, nrows, asize, item_len, idx
        integer :: nitems, max_item_len, max_string_len
        logical, allocatable :: valid_flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        call check_writer_open(writer)

        asize = size(values, 1)
        nrows = size(values, 2)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // writer_context_suffix(writer)
            if (.not. writer%all_columns(idx)%is_set) return
            if (writer%all_columns(idx)%col_size /= asize) then
                error stop "parquet_write_column: array size mismatch for column " // trim(name)
            end if

            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval(len_trim(values))
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_matrix_column: string length exceeds declared array_size for column: " &
                    // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        nitems = size(values)
        if (nitems <= 0) return
        call parquet_check_row_count(writer, name, int(nrows, kind=c_long_long))

        item_len = len(values(1, 1))
        allocate(packed(item_len * nitems))

        k = 0
        do i = 1, nrows
            do j = 1, asize
                do l = 1, item_len
                    k = k + 1
                    packed(k) = achar(iachar(values(j, i)(l:l)), kind=c_char)
                end do
            end do
        end do

        if (present(is_valid)) then
            valid_flat = reshape(is_valid, [size(is_valid)])
            call parquet_check_protected(writer, name, valid_flat)
            call parquet_check_qc_string(writer, name, reshape(values, [size(values)]), valid_flat)
            call parquet_make_valid_buf_write(valid_flat, valid_buf, valid_ptr)
        else
            call parquet_check_qc_string(writer, name, reshape(values, [size(values)]), spread(.true., 1, size(values)))
            call parquet_make_valid_buf_write(valid_buf=valid_buf, valid_ptr=valid_ptr)
        end if

        call parquet_append_string_array_column(&
            writer%handle, &
            trim(parquet_resolve_output_name(writer, name))//char(0), &
            packed, &
            int(item_len, kind=c_long_long), &
            int(nrows, kind=c_long_long), &
            int(asize, kind=c_long_long), &
            valid_ptr )
    end procedure parquet_write_string_matrix_column

    module procedure parquet_close_writer
        integer :: i

        if (.not. c_associated(writer%handle)) then
            error stop "parquet_close_writer: writer has not been opened, or was already closed"
        end if

        if (writer%is_schema_enforced .and. allocated(writer%enabled_columns)) then
            do i = 1, size(writer%enabled_columns)
                if (writer%write_counts(i) == 0) then
                    ! Filename/schema name go on their own print lines rather
                    ! than into the error stop text, to keep that text short.
                    if (allocated(writer%filename)) print '(a)', "parquet_close_writer: output file: " // trim(writer%filename)
                    if (allocated(writer%maml_name)) then
                        print '(a)', "parquet_close_writer: schema: " // trim(writer%maml_name)
                    else
                        print '(a)', "parquet_close_writer: schema: (unnamed, built in-memory)"
                    end if
                    error stop "parquet_close_writer: missing write for enabled column: " // trim(writer%enabled_columns(i)%name)
                end if
            end do
        end if

        call close_parquet_writer(writer%handle)
        writer%handle = c_null_ptr
        if (allocated(writer%all_columns)) deallocate(writer%all_columns)
        if (allocated(writer%write_counts)) deallocate(writer%write_counts)
        if (allocated(writer%enabled_columns)) deallocate(writer%enabled_columns)
        writer%is_schema_enforced = .false.
    end procedure parquet_close_writer

    !> Safety net for a writer whose handle is still open when it goes out of
    !> scope or is overwritten (e.g. reassigned, or an early RETURN between
    !> parquet_open_writer and parquet_close_writer): frees the underlying
    !> C++ object so the process doesn't leak it. This intentionally skips
    !> parquet_close_writer's is_schema_enforced check (erroring from an implicit
    !> finalizer on an incompletely-written file would be surprising) --
    !> always prefer calling parquet_close_writer explicitly.
    module procedure writer_finalize
        if (c_associated(this%handle)) then
            call close_parquet_writer(this%handle)
            this%handle = c_null_ptr
        end if
    end procedure writer_finalize

end submodule parquet_write
