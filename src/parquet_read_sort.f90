!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Sort-key parsing: turns one parquet_sortkey%add key ("ra asc", "-dec", "main.inner.age") into
!> the column name plus a direction flag the C++ engine takes (see parquet_reader_set_sort in
!> parquet_wrapper.cpp). Purely syntactic -- no schema access at all, so nothing here can tell
!> whether a column exists or whether its type is orderable; that validation stays C++-side,
!> where the schema and the decoded array are.
!>
!> Split out of parquet_read.f90 for the same reasons parquet_read_filter is: a syntax error is
!> reported through the same error-context helpers every other Fortran-side failure uses, the
!> parser is unit-testable in process with no file and no reader, and the bind(C) boundary keeps
!> carrying fixed-width packed strings rather than raw user text.
!>
!> The grammar is deliberately tiny -- one key is one column and at most one direction word:
!>
!>     key       := [ '-' ] NAME [ direction ]
!>     direction := 'asc' | 'ascending' | 'desc' | 'descending'
!>
!> A leading '-' on the name is shorthand for descending, so "-dec" and "dec desc" are the same
!> key. Direction words are case-insensitive. Giving both forms at once ("-dec desc") is a
!> mistake worth reporting rather than silently resolving, since the two could equally be read as
!> agreeing or as cancelling out.
submodule (parquet:parquet_read) parquet_read_sort
    implicit none
    !
contains
    !
    module procedure parquet_parse_sort_key
        character(len=:), allocatable :: text, word, lowered
        integer :: sep, i

        name = ""
        descending = .false.
        ok = .false.
        errmsg = ""

        text = adjustl(trim(key))
        if (len_trim(text) == 0) then
            errmsg = "empty sort key"
            return
        end if

        ! The '-' shorthand binds to the name, so it is stripped before anything else looks at it.
        if (text(1:1) == "-") then
            descending = .true.
            text = adjustl(text(2:))
            if (len_trim(text) == 0) then
                errmsg = "sort key '" // trim(key) // "' names no column"
                return
            end if
        end if

        ! Split on the first run of blanks: everything before it is the column name, everything
        ! after it must be exactly one direction word.
        sep = index(trim(text), " ")
        if (sep == 0) then
            name = trim(text)
            word = ""
        else
            name = text(1:sep - 1)
            word = adjustl(text(sep + 1:))
        end if

        if (index(trim(word), " ") /= 0) then
            errmsg = "sort key '" // trim(key) // "' has more than one direction word"
            return
        end if

        if (len_trim(word) > 0) then
            if (descending) then
                errmsg = "sort key '" // trim(key) // "' combines the '-' shorthand with an " // &
                    "explicit direction; use one or the other"
                return
            end if
            lowered = trim(word)
            do i = 1, len(lowered)
                if (lowered(i:i) >= "A" .and. lowered(i:i) <= "Z") then
                    lowered(i:i) = achar(iachar(lowered(i:i)) + 32)
                end if
            end do
            select case (lowered)
            case ("asc", "ascending")
                descending = .false.
            case ("desc", "descending")
                descending = .true.
            case default
                errmsg = "sort key '" // trim(key) // "' has an unrecognized direction '" // &
                    trim(word) // "' (expected asc, ascending, desc or descending)"
                return
            end select
        end if

        ! The `if` line below is a gcov attribution artifact: its condition is evaluated on every
        ! call, so gcov counts that line as hit even though the guarded body (verified) never runs
        ! -- the same shape CLAUDE.md's "Fortran gcov attribution artifacts" documents.
        ! GCOVR_EXCL_START -- defensive, and unreachable through any key this parser accepts: the
        ! text is adjustl'd and checked non-empty on entry (and again after a leading '-' is
        ! stripped), so it can never begin with a blank, which means the name half of the split
        ! below is always at least one character. Kept as a backstop against a future change to
        ! the splitting above rather than deleted.
        if (len_trim(name) == 0) then
            errmsg = "sort key '" // trim(key) // "' names no column"
            return
        end if
        ! GCOVR_EXCL_STOP
        if (len_trim(name) > sort_key_name_len) then
            errmsg = "sort key column name is too long (maximum " // trim(int_to_text(sort_key_name_len)) // &
                " characters): " // trim(name)
            return
        end if
        ok = .true.
    end procedure parquet_parse_sort_key
    !
    module procedure parquet_render_sort_keys
        integer :: i

        text = ""
        do i = 1, nkeys
            if (i > 1) text = text // ", "
            text = text // trim(key_name(i))
            if (descending(i) /= 0_int8) then
                text = text // " desc"
            else
                text = text // " asc"
            end if
            if (nulls_first(i) /= 0_int8) text = text // " nulls_first"
        end do
    end procedure parquet_render_sort_keys
    !
    !> Small local helper: an integer as trimmed text, so a message can be built inline without a
    !> scratch character variable at every site.
    function int_to_text(value) result(text)
        integer, intent(in) :: value !! value to render.
        character(len=32) :: text !! `value` written left-justified, blank-padded.
        write(text, '(i0)') value
    end function int_to_text
    !
    !> Renames the column each sort key orders by, by parsing the key, substituting the name, and
    !> re-emitting it in the canonical "<column> asc"/"<column> desc" form. The sort twin of
    !> parquet_filter%remap_column_names -- see parquet_filter's doc comment for what this is for
    !> and for the two deliberate non-failures (an unmatched name, an unparseable key).
    !>
    !> nulls_first lives in its own component rather than in the key text, so it is untouched here
    !> by construction. A key whose column did not change keeps its original text, for the same
    !> reason the filter side does: it keeps the re-render off every key that had no need of it.
    module procedure parquet_sortkey_remap_column_names
        character(len=:), allocatable :: name, errmsg, text, tmp(:)
        character(len=32) :: cap_str
        logical :: ok, descending
        integer :: i, k

        if (size(from) /= size(to)) error stop "parquet_sortkey%remap_column_names: from and to " // &
            "must have the same size"
        if (size(from) == 0 .or. this%n == 0) return

        do i = 1, this%n
            call parquet_parse_sort_key(this%keys(i), name, descending, ok, errmsg)
            ! As on the filter side: the reader that applies this key reports the parse failure
            ! with the file named, which is the better message.
            if (.not. ok) cycle
            do k = 1, size(from)
                if (trim(name) /= trim(from(k))) cycle
                if (len_trim(to(k)) > sort_key_name_len) then
                    write(cap_str, '(i0)') sort_key_name_len
                    error stop "parquet_sortkey%remap_column_names: replacement column name '" // &
                        trim(to(k)) // "' exceeds the maximum supported length (" // &
                        trim(cap_str) // " characters)"
                end if
                if (descending) then
                    text = trim(to(k)) // " desc"
                else
                    text = trim(to(k)) // " asc"
                end if
                ! No sortkey_max_key_len check here, unlike the filter side's filter_max_rule_len
                ! one: a rewritten key is at most sort_key_name_len + len(" desc") characters, and
                ! the name-length check above has already bounded the first term, so it cannot
                ! reach sortkey_max_key_len (64 + 5 against 320). Revisit if either constant moves.
                if (len(text) <= len(this%keys)) then
                    this%keys(i) = text
                else
                    ! Every entry shares one length (the %add convention), so a longer rewritten
                    ! key re-lengthens the whole array.
                    allocate(character(len=len(text)) :: tmp(this%n))
                    tmp(1:this%n) = this%keys(1:this%n)
                    tmp(i) = text
                    call move_alloc(tmp, this%keys)
                end if
                exit
            end do
        end do
    end procedure parquet_sortkey_remap_column_names
    !
end submodule parquet_read_sort
