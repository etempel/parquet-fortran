!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Everything here backs the parquet_get_metadata generic (declared in
!> parquet.f90): reader%metadata is populated once, by parquet_open_reader
!> (parquet_read.f90) copying the Arrow schema's flat key-value metadata,
!> so every call here only scans that in-memory array -- it never touches
!> the file. Messages follow the "<proc>: <message>" error stop convention
!> used throughout this module.
submodule (parquet) parquet_metadata_get
    implicit none
contains

    !> 1-based index of `key` in metadata%items, or 0 if not found.
    integer function parquet_metadata_find_index(metadata, key) result(idx)
        type(parquet_table_metadata), intent(in) :: metadata !! table metadata to search.
        character(len=*), intent(in) :: key !! metadata key to look up.
        integer :: i

        idx = 0
        if (.not. allocated(metadata%items)) return
        do i = 1, size(metadata%items)
            if (.not. allocated(metadata%items(i)%key)) cycle
            if (trim(metadata%items(i)%key) == trim(key)) then
                idx = i
                return
            end if
        end do
    end function parquet_metadata_find_index

    !> Parses `str` as an integer(int64); .false. (val undefined) if it isn't
    !> a valid integer literal.
    logical function parquet_metadata_parse_int64(str, val) result(ok)
        character(len=*), intent(in) :: str !! stored metadata value text.
        integer(int64), intent(out) :: val !! parsed value; only meaningful when ok is .true.
        integer :: ios

        read(str, *, iostat=ios) val
        ok = (ios == 0)
    end function parquet_metadata_parse_int64

    !> Parses `str` as a real64; .false. (val undefined) if it isn't a valid
    !> real literal.
    logical function parquet_metadata_parse_real64(str, val) result(ok)
        character(len=*), intent(in) :: str !! stored metadata value text.
        real(real64), intent(out) :: val !! parsed value; only meaningful when ok is .true.
        integer :: ios

        read(str, *, iostat=ios) val
        ok = (ios == 0)
    end function parquet_metadata_parse_real64

    !> Parses `str` (case-insensitive, trimmed) as "true"/"false"; .false.
    !> (val undefined) for anything else.
    logical function parquet_metadata_parse_logical(str, val) result(ok)
        character(len=*), intent(in) :: str !! stored metadata value text.
        logical, intent(out) :: val !! parsed value; only meaningful when ok is .true.
        character(len=:), allocatable :: t

        call parquet_to_lower(trim(adjustl(str)), t)
        if (t == "true") then
            val = .true.
            ok = .true.
        else if (t == "false") then
            val = .false.
            ok = .true.
        else
            ok = .false.
        end if
    end function parquet_metadata_parse_logical

    !> True if `v` is within integer(int32)'s representable range.
    logical function parquet_metadata_int32_fits(v) result(ok)
        integer(int64), intent(in) :: v !! value to check.

        ok = (v >= -int(huge(0_int32), int64) - 1_int64) .and. (v <= int(huge(0_int32), int64))
    end function parquet_metadata_int32_fits

    !> Splits an add_metadata_*_array-formatted string ("[v1, v2, ...]") into
    !> its comma-separated, trimmed elements. tokens(:) is a uniform-length
    !> (deferred-length) character array sized to the longest element; n==0
    !> for an empty "[]".
    subroutine parquet_metadata_split_array(raw, tokens, n)
        character(len=*), intent(in) :: raw !! stored add_metadata_*_array value text, "[v1, v2, ...]".
        character(len=:), allocatable, intent(out) :: tokens(:) !! trimmed, uniform-length elements.
        integer, intent(out) :: n !! number of elements actually parsed (0 for "[]").
        character(len=:), allocatable :: inner, tok
        integer :: L, i, start, maxlen
        logical :: at_boundary

        inner = trim(adjustl(raw))
        L = len(inner)
        if (L >= 2) then
            if (inner(1:1) == '[' .and. inner(L:L) == ']') inner = inner(2:L-1)
        end if
        inner = trim(adjustl(inner))
        L = len(inner)

        n = 0
        maxlen = 0
        if (L > 0) then
            start = 1
            do i = 1, L + 1
                at_boundary = (i > L)
                if (.not. at_boundary) at_boundary = (inner(i:i) == ',')
                if (at_boundary) then
                    tok = trim(adjustl(inner(start:i-1)))
                    n = n + 1
                    maxlen = max(maxlen, len(tok))
                    start = i + 1
                end if
            end do
        end if

        allocate(character(len=max(maxlen, 1)) :: tokens(n))
        if (n > 0) then
            start = 1
            n = 0
            do i = 1, L + 1
                at_boundary = (i > L)
                if (.not. at_boundary) at_boundary = (inner(i:i) == ',')
                if (at_boundary) then
                    tok = trim(adjustl(inner(start:i-1)))
                    n = n + 1
                    tokens(n) = tok
                    start = i + 1
                end if
            end do
        end if
    end subroutine parquet_metadata_split_array

    !> Prints a "using default value" WARNING for a missing metadata key.
    subroutine parquet_metadata_warn_default_used(key, filename)
        character(len=*), intent(in) :: key !! metadata key that was missing.
        character(len=*), intent(in) :: filename !! file the key was looked up in.
        print '(a)', "WARNING: parquet_get_metadata: metadata key '" // trim(key) // "' not found in file '" // &
            trim(filename) // "', using default value"
    end subroutine parquet_metadata_warn_default_used

    !> Prints a "cannot be converted" WARNING before falling back to a default
    !> (or error-stopping, if no default was given).
    subroutine parquet_metadata_warn_conversion_failed(key, filename, raw, target_desc)
        character(len=*), intent(in) :: key !! metadata key whose value failed to convert.
        character(len=*), intent(in) :: filename !! file the key was looked up in.
        character(len=*), intent(in) :: raw !! stored value text that failed to convert.
        character(len=*), intent(in) :: target_desc !! human-readable target type description.
        print '(a)', "WARNING: parquet_get_metadata: metadata key '" // trim(key) // "' in file '" // trim(filename) // &
            "' has value '" // trim(raw) // "' that cannot be converted to " // trim(target_desc)
    end subroutine parquet_metadata_warn_conversion_failed

    !> Error stops for a missing metadata key with no default given.
    subroutine parquet_metadata_stop_missing(key, filename)
        character(len=*), intent(in) :: key !! metadata key that was missing.
        character(len=*), intent(in) :: filename !! file the key was looked up in.
        error stop "parquet_get_metadata: metadata key '" // trim(key) // "' not found in file '" // trim(filename) // "'"
    end subroutine parquet_metadata_stop_missing

    !> Error stops for a metadata value that failed to convert, with no
    !> default given.
    subroutine parquet_metadata_stop_conversion(key, filename, raw, target_desc)
        character(len=*), intent(in) :: key !! metadata key whose value failed to convert.
        character(len=*), intent(in) :: filename !! file the key was looked up in.
        character(len=*), intent(in) :: raw !! stored value text that failed to convert.
        character(len=*), intent(in) :: target_desc !! human-readable target type description.
        error stop "parquet_get_metadata: metadata key '" // trim(key) // "' in file '" // trim(filename) // &
            "' has value '" // trim(raw) // "' that cannot be converted to " // trim(target_desc) // &
            " and no default was given"
    end subroutine parquet_metadata_stop_conversion

    module procedure parquet_get_metadata_int32
        integer :: idx
        integer(int64) :: parsed
        logical :: warn_value

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            if (parquet_metadata_parse_int64(reader%metadata%items(idx)%value, parsed)) then
                if (parquet_metadata_int32_fits(parsed)) then
                    value = int(parsed, kind=int32)
                    return
                end if
            end if
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, "a 32-bit integer")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a 32-bit integer")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_int32

    module procedure parquet_get_metadata_int64
        integer :: idx
        integer(int64) :: parsed
        logical :: warn_value

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            if (parquet_metadata_parse_int64(reader%metadata%items(idx)%value, parsed)) then
                value = parsed
                return
            end if
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, "a 64-bit integer")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a 64-bit integer")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_int64

    module procedure parquet_get_metadata_float32
        integer :: idx
        real(real64) :: parsed
        logical :: warn_value

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            if (parquet_metadata_parse_real64(reader%metadata%items(idx)%value, parsed)) then
                value = real(parsed, kind=real32)
                return
            end if
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, "a 32-bit real")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a 32-bit real")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_float32

    module procedure parquet_get_metadata_float64
        integer :: idx
        real(real64) :: parsed
        logical :: warn_value

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            if (parquet_metadata_parse_real64(reader%metadata%items(idx)%value, parsed)) then
                value = parsed
                return
            end if
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, "a 64-bit real")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a 64-bit real")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_float64

    module procedure parquet_get_metadata_logical
        integer :: idx
        logical :: parsed, warn_value

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            if (parquet_metadata_parse_logical(reader%metadata%items(idx)%value, parsed)) then
                value = parsed
                return
            end if
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, "a logical")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a logical")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_logical

    module procedure parquet_get_metadata_string
        integer :: idx
        logical :: warn_value

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            value = reader%metadata%items(idx)%value
            return
        end if

        if (present(default)) then
            value = trim(default)
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_string

    module procedure parquet_get_metadata_int32_array
        integer :: idx, i, n
        character(len=:), allocatable :: tokens(:)
        integer(int64) :: parsed
        logical :: warn_value, ok

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            call parquet_metadata_split_array(reader%metadata%items(idx)%value, tokens, n)
            allocate(value(n))
            ok = .true.
            do i = 1, n
                ok = parquet_metadata_parse_int64(tokens(i), parsed)
                if (ok) ok = parquet_metadata_int32_fits(parsed)
                if (.not. ok) exit
                value(i) = int(parsed, kind=int32)
            end do
            if (ok) return
            deallocate(value)
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, &
                "a 32-bit integer array")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a 32-bit integer array")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_int32_array

    module procedure parquet_get_metadata_int64_array
        integer :: idx, i, n
        character(len=:), allocatable :: tokens(:)
        integer(int64) :: parsed
        logical :: warn_value, ok

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            call parquet_metadata_split_array(reader%metadata%items(idx)%value, tokens, n)
            allocate(value(n))
            ok = .true.
            do i = 1, n
                ok = parquet_metadata_parse_int64(tokens(i), parsed)
                if (.not. ok) exit
                value(i) = parsed
            end do
            if (ok) return
            deallocate(value)
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, &
                "a 64-bit integer array")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a 64-bit integer array")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_int64_array

    module procedure parquet_get_metadata_float32_array
        integer :: idx, i, n
        character(len=:), allocatable :: tokens(:)
        real(real64) :: parsed
        logical :: warn_value, ok

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            call parquet_metadata_split_array(reader%metadata%items(idx)%value, tokens, n)
            allocate(value(n))
            ok = .true.
            do i = 1, n
                ok = parquet_metadata_parse_real64(tokens(i), parsed)
                if (.not. ok) exit
                value(i) = real(parsed, kind=real32)
            end do
            if (ok) return
            deallocate(value)
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, &
                "a 32-bit real array")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a 32-bit real array")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_float32_array

    module procedure parquet_get_metadata_float64_array
        integer :: idx, i, n
        character(len=:), allocatable :: tokens(:)
        real(real64) :: parsed
        logical :: warn_value, ok

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            call parquet_metadata_split_array(reader%metadata%items(idx)%value, tokens, n)
            allocate(value(n))
            ok = .true.
            do i = 1, n
                ok = parquet_metadata_parse_real64(tokens(i), parsed)
                if (.not. ok) exit
                value(i) = parsed
            end do
            if (ok) return
            deallocate(value)
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, &
                "a 64-bit real array")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a 64-bit real array")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_float64_array

    module procedure parquet_get_metadata_logical_array
        integer :: idx, i, n
        character(len=:), allocatable :: tokens(:)
        logical :: parsed, warn_value, ok

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            call parquet_metadata_split_array(reader%metadata%items(idx)%value, tokens, n)
            allocate(value(n))
            ok = .true.
            do i = 1, n
                ok = parquet_metadata_parse_logical(tokens(i), parsed)
                if (.not. ok) exit
                value(i) = parsed
            end do
            if (ok) return
            deallocate(value)
            call parquet_metadata_warn_conversion_failed(key, reader%filename, reader%metadata%items(idx)%value, "a logical array")
            if (present(default)) then
                value = default
                return
            end if
            call parquet_metadata_stop_conversion(key, reader%filename, reader%metadata%items(idx)%value, "a logical array")
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_logical_array

    module procedure parquet_get_metadata_string_array
        integer :: idx, n
        character(len=:), allocatable :: tokens(:)
        logical :: warn_value

        warn_value = .true.
        if (present(warn)) warn_value = warn

        idx = parquet_metadata_find_index(reader%metadata, key)
        if (idx > 0) then
            call parquet_metadata_split_array(reader%metadata%items(idx)%value, tokens, n)
            value = tokens
            return
        end if

        if (present(default)) then
            value = default
            if (warn_value) call parquet_metadata_warn_default_used(key, reader%filename)
            return
        end if
        call parquet_metadata_stop_missing(key, reader%filename)
    end procedure parquet_get_metadata_string_array

end submodule parquet_metadata_get
