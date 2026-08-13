!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Generic parquet_column_info/parquet_table_metadata plumbing shared by both
!> the write (MAML-build) and read sides: the base add_metadata family,
!> per-field column_info lookups/toggles, and low-level MAML string helpers
!> (parquet_split_key_value/parquet_unquote/parquet_to_lower) reused across
!> submodules.
submodule (parquet_core:parquet_metadata) parquet_metadata_base
    implicit none
contains

    module procedure parquet_append_line
        character(len=maml_max_line_len), allocatable :: tmp(:)

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        if (.not. allocated(lines)) then
            allocate(lines(1))
            lines(1) = line
            call parquet_maml_unlock()
            return
        end if

        allocate(tmp(n))
        tmp(1:n-1) = lines
        tmp(n) = line
        call move_alloc(tmp, lines)
        call parquet_maml_unlock()
    end procedure parquet_append_line

    module procedure parquet_metadata_append_entry
        type(parquet_metadata_entry), allocatable :: tmp(:)
        integer :: n
        character(len=:), allocatable :: desc_val
        character(len=:), allocatable :: dt_val

        if (len_trim(key) == 0) return

        call parquet_metadata_warn_duplicate(metadata, key, warn)

        desc_val = ""
        if (present(description)) desc_val = trim(description)
        dt_val = ""
        if (present(datatype)) dt_val = trim(datatype)

        ! See g_maml_mutex in parquet_wrapper.cpp / parquet_parse_maml_lines
        ! above: growing metadata%items this way is not safely reentrant
        ! under genuine concurrent threads even with -frecursive.
        call parquet_maml_lock()
        if (.not. allocated(metadata%items)) then
            allocate(metadata%items(1))
            metadata%items(1)%key = trim(key)
            metadata%items(1)%value = trim(value)
            metadata%items(1)%description = desc_val
            metadata%items(1)%datatype = dt_val
        else
            n = size(metadata%items)
            allocate(tmp(n+1))
            tmp(1:n) = metadata%items
            tmp(n+1)%key = trim(key)
            tmp(n+1)%value = trim(value)
            tmp(n+1)%description = desc_val
            tmp(n+1)%datatype = dt_val
            call move_alloc(tmp, metadata%items)
        end if
        call parquet_maml_unlock()

        ! metadata%source_maml_lines is only allocated once parquet_read_maml has
        ! finished parsing (see parquet_read_maml_file/_internal below), so this
        ! never fires for the add_metadata calls the parser itself makes while
        ! building up metadata%items above -- only for calls made by external
        ! code after parquet_read_maml has returned.
        if (allocated(metadata%source_maml_lines)) then
            call parquet_append_keyarray_line(metadata%source_maml_lines, trim(key), trim(value), desc_val)
        end if
    end procedure parquet_metadata_append_entry

    module procedure add_metadata_int32
        character(len=64) :: cval

        write(cval, '(I0)') value
        call parquet_metadata_append_entry(this, key, trim(cval), description, warn, datatype="int32")
    end procedure add_metadata_int32

    module procedure add_metadata_int64
        character(len=64) :: cval

        write(cval, '(I0)') value
        call parquet_metadata_append_entry(this, key, trim(cval), description, warn, datatype="int64")
    end procedure add_metadata_int64

    module procedure add_metadata_float32
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') value
        else
            write(cval, '(ES15.7E3)') value
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)), description, warn, datatype="float32")
    end procedure add_metadata_float32

    module procedure add_metadata_float64
        character(len=64) :: cval

        if (present(fmt)) then
            write(cval, '(' // trim(fmt) // ')') value
        else
            write(cval, '(ES24.16E3)') value
        end if
        call parquet_metadata_append_entry(this, key, trim(adjustl(cval)), description, warn, datatype="float64")
    end procedure add_metadata_float64

    module procedure add_metadata_logical
        if (value) then
            call parquet_metadata_append_entry(this, key, "true", description, warn, datatype="boolean")
        else
            call parquet_metadata_append_entry(this, key, "false", description, warn, datatype="boolean")
        end if
    end procedure add_metadata_logical

    module procedure add_metadata_string
        call parquet_metadata_append_entry(this, key, trim(value), description, warn)
    end procedure add_metadata_string

    module procedure add_metadata_int32_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            write(cval, '(I0)') value(i)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(cval)
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description, warn, datatype="int32[]")
    end procedure add_metadata_int32_array

    module procedure add_metadata_int64_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            write(cval, '(I0)') value(i)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(cval)
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description, warn, datatype="int64[]")
    end procedure add_metadata_int64_array

    module procedure add_metadata_float32_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (present(fmt)) then
                write(cval, '(' // trim(fmt) // ')') value(i)
            else
                write(cval, '(ES15.7E3)') value(i)
            end if
            if (i > 1) joined = joined // ", "
            joined = joined // trim(adjustl(cval))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description, warn, datatype="float32[]")
    end procedure add_metadata_float32_array

    module procedure add_metadata_float64_array
        character(len=64) :: cval
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (present(fmt)) then
                write(cval, '(' // trim(fmt) // ')') value(i)
            else
                write(cval, '(ES24.16E3)') value(i)
            end if
            if (i > 1) joined = joined // ", "
            joined = joined // trim(adjustl(cval))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description, warn, datatype="float64[]")
    end procedure add_metadata_float64_array

    module procedure add_metadata_logical_array
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (i > 1) joined = joined // ", "
            if (value(i)) then
                joined = joined // "true"
            else
                joined = joined // "false"
            end if
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description, warn, datatype="boolean[]")
    end procedure add_metadata_logical_array

    module procedure add_metadata_string_array
        character(len=:), allocatable :: joined
        integer :: i

        joined = "["
        do i = 1, size(value)
            if (i > 1) joined = joined // ", "
            joined = joined // trim(value(i))
        end do
        joined = joined // "]"
        call parquet_metadata_append_entry(this, key, joined, description, warn, datatype="string[]")
    end procedure add_metadata_string_array

    module procedure metadata_clear_metadata
        type(parquet_metadata_entry), allocatable :: tmp(:)

        if (.not. allocated(this%items)) return
        if (size(this%items) <= this%n_base_items) return

        if (this%n_base_items <= 0) then
            deallocate(this%items)
            return
        end if

        allocate(tmp(this%n_base_items))
        tmp = this%items(1:this%n_base_items)
        call move_alloc(tmp, this%items)
    end procedure metadata_clear_metadata

    module procedure get_column_index
        integer :: i

        get_column_index = 0
        if (allocated(this%col)) then
            do i = 1, size(this%col)
                if (allocated(this%col(i)%name)) then
                    if (trim(this%col(i)%name) == trim(name)) then
                        get_column_index = i
                        exit
                    end if
                end if
            end do
        end if

        if (get_column_index == 0) then
            error stop "parquet_column_info%get_column_index: column not found: " // trim(name)
        end if
    end procedure get_column_index

    module procedure is_column_set
        integer :: idx

        idx = this%get_column_index(name)
        is_column_set = this%col(idx)%is_set
    end procedure is_column_set

    module procedure get_num_fields
        if (allocated(this%col)) then
            get_num_fields = size(this%col)
        else
            get_num_fields = 0
        end if
    end procedure get_num_fields

    module procedure get_field_name
        character(len=16) :: index_str, count_str

        if (index < 1 .or. index > this%get_num_fields()) then
            write(index_str, '(I0)') index
            write(count_str, '(I0)') this%get_num_fields()
            error stop "parquet_column_info%get_field_name: index " // trim(index_str) // &
                " out of range (1.." // trim(count_str) // ")"
        end if

        name = this%col(index)%name
    end procedure get_field_name

    module procedure get_field_by_name
        integer :: idx

        idx = this%get_column_index(name)
        call fill_field_definition_outputs(this%col(idx), data_type, unit, info, ucd, array_size, col_size, &
            qc_min, qc_max, qc_miss)
    end procedure get_field_by_name

    module procedure get_field_by_index
        character(len=16) :: index_str, count_str

        if (index < 1 .or. index > this%get_num_fields()) then
            write(index_str, '(I0)') index
            write(count_str, '(I0)') this%get_num_fields()
            error stop "parquet_column_info%get_field: index " // trim(index_str) // &
                " out of range (1.." // trim(count_str) // ")"
        end if

        name = this%col(index)%name
        call fill_field_definition_outputs(this%col(index), data_type, unit, info, ucd, array_size, col_size, &
            qc_min, qc_max, qc_miss)
    end procedure get_field_by_index

    !> Shared output-filling worker for get_field_by_name/get_field_by_index: copies col's
    !! stored definition into whichever optional outputs the caller actually requested,
    !! reconstructing qc_min/qc_max into the same operator-prefixed string form %add_field
    !! accepts, and qc_miss into "Null"/"" (see get_field_by_name's own doc comment for the
    !! round-trip caveats on both).
    subroutine fill_field_definition_outputs(col, data_type, unit, info, ucd, array_size, col_size, &
            qc_min, qc_max, qc_miss)
        type(parquet_column_type), intent(in) :: col !! source field definition.
        character(len=:), allocatable, intent(out), optional :: data_type !! field's data type.
        character(len=:), allocatable, intent(out), optional :: unit !! unit of measurement, if declared.
        character(len=:), allocatable, intent(out), optional :: info !! short description, if declared.
        character(len=:), allocatable, intent(out), optional :: ucd !! IVOA Unified Content Descriptor, if declared.
        integer, intent(out), optional :: array_size !! maximum string length (string fields only).
        integer, intent(out), optional :: col_size !! vector-column element count.
        character(len=:), allocatable, intent(out), optional :: qc_min !! qc: min: bound, operator-prefixed.
        character(len=:), allocatable, intent(out), optional :: qc_max !! qc: max: bound, operator-prefixed.
        character(len=:), allocatable, intent(out), optional :: qc_miss !! "Null" or "".

        ! The four "else" branches below (data_type/unit/info/ucd) are unreachable through the
        ! public API: parquet_parse_maml_lines (parquet_metadata.f90) error-stops if a parsed
        ! field lacks data_type, and unconditionally backfills unit/info/ucd to "" for every
        ! field before cinfo%col is ever populated -- the only place col entries reaching
        ! get_field come from. Kept as a defensive fallback rather than an assumed invariant;
        ! GCOVR_EXCL_LINE tags below reflect that these are confirmed-dead, not a coverage gap.
        if (present(data_type)) then
            if (allocated(col%data_type)) then
                data_type = col%data_type
            else
                data_type = "" ! GCOVR_EXCL_LINE
            end if
        end if
        if (present(unit)) then
            if (allocated(col%unit)) then
                unit = col%unit
            else
                unit = "" ! GCOVR_EXCL_LINE
            end if
        end if
        if (present(info)) then
            if (allocated(col%info)) then
                info = col%info
            else
                info = "" ! GCOVR_EXCL_LINE
            end if
        end if
        if (present(ucd)) then
            if (allocated(col%ucd)) then
                ucd = col%ucd
            else
                ucd = "" ! GCOVR_EXCL_LINE
            end if
        end if
        if (present(array_size)) array_size = col%array_size
        if (present(col_size)) col_size = col%col_size

        if (present(qc_min)) then
            if (col%has_qc_min) then
                qc_min = trim(col%qc_min_op) // " " // trim(col%qc_min_raw)
            else
                qc_min = ""
            end if
        end if
        if (present(qc_max)) then
            if (col%has_qc_max) then
                qc_max = trim(col%qc_max_op) // " " // trim(col%qc_max_raw)
            else
                qc_max = ""
            end if
        end if
        if (present(qc_miss)) then
            if (col%qc_allow_null) then
                qc_miss = "Null"
            else
                qc_miss = ""
            end if
        end if
    end subroutine fill_field_definition_outputs

    module procedure set_unavailable
        integer :: idx, i

        if (present(name)) then
            idx = this%get_column_index(name)
            if (this%col(idx)%is_deactivated) then
                error stop "parquet_column_info%set_column_unavailable: column is deactivated: " // trim(name)
            end if
            this%col(idx)%is_set = .false.
        else if (allocated(this%col)) then
            do i = 1, size(this%col)
                if (.not. this%col(i)%is_deactivated) this%col(i)%is_set = .false.
            end do
        end if
    end procedure set_unavailable

    module procedure set_available
        integer :: idx, i

        if (present(name)) then
            idx = this%get_column_index(name)
            if (this%col(idx)%is_deactivated) then
                error stop "parquet_column_info%set_column_available: column is deactivated: " // trim(name)
            end if
            this%col(idx)%is_set = .true.
        else if (allocated(this%col)) then
            do i = 1, size(this%col)
                if (.not. this%col(i)%is_deactivated) this%col(i)%is_set = .true.
            end do
        end if
    end procedure set_available

    module procedure set_col_size
        integer :: idx
        logical :: do_force

        do_force = .false.
        if (present(force)) do_force = force

        if (col_size < 1) then
            error stop "parquet_column_info%set_col_size: col_size must be a positive integer: " // trim(name)
        end if

        idx = this%get_column_index(name)

        if (this%col(idx)%col_size /= parquet_size_auto .and. .not. do_force) then
            error stop "parquet_column_info%set_col_size: col_size for column '" // trim(name) // &
                "' is not 'auto' (already resolved); pass force=.true. to override"
        end if

        this%col(idx)%col_size = col_size
    end procedure set_col_size

    module procedure set_protected
        integer :: idx
        logical :: want

        want = .true.
        if (present(protected)) want = protected

        idx = this%get_column_index(name)
        ! Relaxing a protection the MAML declared is permitted but never silent: someone wrote
        ! protected_cols: on purpose, and a program overriding that should say so in its own
        ! output. A warning, not an abort -- see the interface's doc-comment in parquet_core.f90.
        if (this%col(idx)%is_protected .and. .not. want) then
            call parquet_emit_warning("set_protected: column '" // trim(name) // &
                "' was declared protected by its MAML (extra: protected_cols:) and is being " // &
                "unprotected in code; it may now be written with Null values")
        end if
        this%col(idx)%is_protected = want
    end procedure set_protected

    module procedure set_array_size
        integer :: idx
        logical :: do_force

        do_force = .false.
        if (present(force)) do_force = force

        if (array_size < 1) then
            error stop "parquet_column_info%set_array_size: array_size must be a positive integer: " // trim(name)
        end if

        idx = this%get_column_index(name)

        if (trim(this%col(idx)%data_type) /= "string") then
            error stop "parquet_column_info%set_array_size: column '" // trim(name) // &
                "' is not a string column (array_size only applies to string fields)"
        end if

        if (this%col(idx)%array_size /= parquet_size_auto .and. .not. do_force) then
            error stop "parquet_column_info%set_array_size: array_size for column '" // trim(name) // &
                "' is not 'auto' (already resolved); pass force=.true. to override"
        end if

        this%col(idx)%array_size = array_size
    end procedure set_array_size

    module procedure parquet_append_empty_cinfo
        type(parquet_column_type), allocatable :: tmp(:)

        ! See g_maml_mutex in parquet_wrapper.cpp.
        call parquet_maml_lock()
        if (.not. allocated(columns)) then
            allocate(columns(1))
            n = 1
        else
            allocate(tmp(size(columns) + 1))
            if (size(columns) > 0) tmp(1:size(columns)) = columns
            call move_alloc(tmp, columns)
            n = size(columns)
        end if

        columns(n)%is_set = .true.
        columns(n)%array_size = 1
        columns(n)%col_size = 1
        call parquet_maml_unlock()
    end procedure parquet_append_empty_cinfo

    module procedure parquet_split_key_value
        integer :: p

        p = index(line, ":")
        if (p <= 0) then
            key = ""
            value = ""
            return
        end if

        key = trim(adjustl(line(1:p-1)))
        if (p < len_trim(line)) then
            value = trim(adjustl(line(p+1:len_trim(line))))
        else
            value = ""
        end if
    end procedure parquet_split_key_value

    module procedure parquet_unquote
        integer :: n

        out = trim(adjustl(s))
        n = len_trim(out)
        if (n >= 2) then
            if ((out(1:1) == '"' .and. out(n:n) == '"') .or. (out(1:1) == "'" .and. out(n:n) == "'")) then
                out = out(2:n-1)
            end if
        end if
    end procedure parquet_unquote

    module procedure parquet_to_lower
        integer :: i, c

        out = s
        do i = 1, len(out)
            c = iachar(out(i:i))
            if (c >= iachar('A') .and. c <= iachar('Z')) out(i:i) = achar(c + 32)
        end do
    end procedure parquet_to_lower

end submodule parquet_metadata_base
