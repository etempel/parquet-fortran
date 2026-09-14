!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `%print_rows`: the table's own rows, aligned, with the column names and their kinds above
!! them. Where `%print_stat` (in `parquet_tables_query`) describes what a table holds, this shows
!! it -- the call a reader arriving from pandas reaches for first.
!!
!! Three conventions run through the whole file:
!!
!! * **Nothing here reads unless a caller named a column.** `%print_rows()` shows the resident
!!   columns and no others; only the `columns=` forms take a first touch, and they take exactly
!!   the one a `%get` would (`table_resolve`), so every residency, concurrency and detach rule
!!   applies unchanged.
!! * **A cell is text, and text is bounded.** Every value is rendered into a string no longer
!!   than `max_width` before anything is measured or printed, so the layout arithmetic never has
!!   to reason about a value's kind and a pathological value cannot widen the display.
!! * **The arguments are checked before `verbosity` is consulted.** A wrong call is a mistake
!!   worth reporting whether or not anything would have been printed -- the same ordering
!!   `%print_stat` uses for `table_check_open`.
submodule (parquet_tables) parquet_tables_display
    ! Every import here is a NAMESPACE import rather than a new dependency: parquet_settings and
    ! parquet_utils are both already in this module's compile footprint (through
    ! parquet_output_is_suppressed and pf_to_str respectively), so nothing downstream compiles
    ! more because this file exists.
    ! A real cell is rendered by normalizing the value, which NaN and the infinities have no
    ! normal form for -- so both are tested for and handed straight to the compiler's own
    ! spelling. Not a hot loop (at most max_columns by the rows shown), so the intrinsic inquiry
    ! is used rather than the `x /= x` idiom.
    use ieee_arithmetic, only : ieee_is_nan, ieee_is_finite
    ! The library's one destination resolver, shared with the emit channels and with every other
    ! solicited printer, so `message_stream` cannot mean one thing to a warning and another to a
    ! listing. Hidden from the user by both facades' `private ::` lines.
    use parquet_utils, only : pf_to_lower
    implicit none
    !
    !> Rows shown from each end when neither `first=` nor `last=` is given -- pandas' truncated
    !! repr, and the shape that says the most about a sorted table.
    integer, parameter :: DEF_ROWS = 5
    !> Significant digits in a real cell by default: the precision the reader's own `print_stat`
    !! already reports doubles at, so the two printers agree on what a value looks like.
    integer, parameter :: DEF_DIGITS = 6
    !> Longest cell text by default. Wide enough for an ISO-8601 timestamp with room to spare.
    integer, parameter :: DEF_WIDTH = 32
    !> Most columns shown by default.
    integer, parameter :: DEF_COLUMNS = 20
    !> Shortest `max_width=` accepted: below this the "..." marker is most of the cell.
    integer, parameter :: MIN_WIDTH = 8
    !> Most significant digits accepted -- enough for a real64 round trip.
    integer, parameter :: MAX_DIGITS = 17
    !> What a null prints as, on every kind. No string value can be confused with it: a string
    !! column holding the text `null` prints that text without the brackets.
    character(len=*), parameter :: NULL_TOKEN = "<null>"
    !> The marker used for an elided row, an elided column, and a cell that has nothing to show.
    character(len=*), parameter :: ELLIPSIS = "..."
    !
contains
    !
    module procedure print_rows_all
        integer, allocatable :: sel(:)
        integer :: i, n
        !
        call table_check_open(self, "print_rows")
        call check_display_args(self%cache, first, last, rows, digits, max_width, max_columns)
        if (parquet_output_is_suppressed()) return
        ! The resident columns, in table order, and nothing else -- this form never reads. A lazy
        ! table therefore selects nothing and display_rows prints the hint line instead.
        n = 0
        do i = 1, self%cache%ncols
            if (self%cache%cols(i)%residency == RES_FULL) n = n + 1
        end do
        allocate(sel(n))
        n = 0
        do i = 1, self%cache%ncols
            if (self%cache%cols(i)%residency /= RES_FULL) cycle
            n = n + 1
            sel(n) = i
        end do
        call display_rows(self, sel, first, last, rows, unit, digits, max_width, max_columns)
    end procedure print_rows_all
    !
    module procedure print_rows_string
        character(len=:), allocatable :: toks(:)
        !
        ! Checked before tokenizing so an unopened table reports THAT rather than a missing
        ! column, exactly as %prefetch orders the two.
        call table_check_open(self, "print_rows")
        ! One tokenizer for the whole library, so this and %prefetch cannot disagree about
        ! punctuation. A single name with no separator comes back as one token.
        call parquet_split_name_list(columns, toks)
        call self%print_rows(toks, first=first, last=last, rows=rows, unit=unit, &
            digits=digits, max_width=max_width, max_columns=max_columns)
    end procedure print_rows_string
    !
    module procedure print_rows_array
        integer, allocatable :: sel(:)
        integer :: j
        !
        call table_check_open(self, "print_rows")
        call check_display_args(self%cache, first, last, rows, digits, max_width, max_columns)
        ! EVERY missing name in one message, before any of the present ones is read: a display
        ! that read three columns and then aborted on the fourth would have changed the table it
        ! was asked to show. %require_columns is the library's own name-list resolver, so the
        ! message is the one a caller already knows.
        call self%require_columns(columns)
        if (parquet_output_is_suppressed()) return
        allocate(sel(size(columns, kind=int64)))
        do j = 1, size(columns)
            ! The ordinary first touch, through the one lookup every value accessor uses: it
            ! reads a column that is not resident, refuses an unsupported type, materializes the
            ! automatic row-index column, and names `print_rows` in whichever message it emits.
            call table_resolve(self, trim(columns(j)), "print_rows", sel(j))
        end do
        call display_rows(self, sel, first, last, rows, unit, digits, max_width, max_columns)
    end procedure print_rows_array
    !
    !> Rejects a display argument that cannot mean anything, before `verbosity` is consulted.
    !!
    !! Placed above the suppression check for the reason `%print_stat` puts `table_check_open`
    !! there: a wrong call is a mistake worth reporting, and silently doing nothing for the wrong
    !! reason is worse than saying so. Every bound here is a display bound rather than a data one,
    !! which is why each message quotes the offending value back.
    subroutine check_display_args(cache, first, last, rows, digits, max_width, max_columns)
        type(parquet_table_cache), intent(in) :: cache    !! the table's store, for the context suffix.
        integer, intent(in), optional :: first            !! rows from the top.
        integer, intent(in), optional :: last             !! rows from the bottom.
        type(parquet_slice), intent(in), optional :: rows !! explicit row selection.
        integer, intent(in), optional :: digits           !! significant digits in a real cell.
        integer, intent(in), optional :: max_width        !! longest cell text.
        integer, intent(in), optional :: max_columns      !! most columns shown.
        character(len=:), allocatable :: sfx
        character(len=32) :: buf, lim
        !
        call table_context_suffix(cache, "", sfx)
        if (present(first)) then
            if (first < 0) then
                write(buf, "(I0)") first
                error stop EP // "print_rows: first= and last= must not be negative (got first=" // &
                    trim(buf) // ")" // sfx
            end if
        end if
        if (present(last)) then
            if (last < 0) then
                write(buf, "(I0)") last
                error stop EP // "print_rows: first= and last= must not be negative (got last=" // &
                    trim(buf) // ")" // sfx
            end if
        end if
        ! Refused rather than silently ranked, because there is no reading of the two together
        ! that is obviously right -- and a caller who wrote both meant one of them.
        if (present(rows) .and. (present(first) .or. present(last))) then
            error stop EP // "print_rows: rows= selects the rows explicitly, so first= and " // &
                "last= cannot also be given" // sfx
        end if
        ! Each bound is quoted from the parameter that enforces it, so a changed default cannot
        ! leave the message describing the old one.
        if (present(digits)) then
            if (digits < 1 .or. digits > MAX_DIGITS) then
                write(lim, "(I0)") MAX_DIGITS
                write(buf, "(I0)") digits
                error stop EP // "print_rows: digits= must be between 1 and " // trim(lim) // &
                    " (got " // trim(buf) // ")" // sfx
            end if
        end if
        if (present(max_width)) then
            if (max_width < MIN_WIDTH) then
                write(lim, "(I0)") MIN_WIDTH
                write(buf, "(I0)") max_width
                error stop EP // "print_rows: max_width= must be at least " // trim(lim) // &
                    " (got " // trim(buf) // ")" // sfx
            end if
        end if
        if (present(max_columns)) then
            if (max_columns < 1) then
                write(buf, "(I0)") max_columns
                error stop EP // "print_rows: max_columns= must be at least 1 (got " // &
                    trim(buf) // ")" // sfx
            end if
        end if
    end subroutine check_display_args
    !
    !> Renders and prints the selected columns' selected rows.
    !!
    !! Everything is rendered into text FIRST and measured second: a column is as wide as its
    !! widest shown cell or heading, which cannot be known until every cell exists. That is also
    !! what bounds the work -- at most `max_columns` columns times the rows actually shown, never
    !! the table.
    subroutine display_rows(self, sel, first, last, rows, unit, digits, max_width, max_columns)
        class(parquet_table), intent(in) :: self          !! the table.
        integer, intent(in) :: sel(:)                     !! selected column slots, in display order.
        integer, intent(in), optional :: first            !! rows from the top.
        integer, intent(in), optional :: last             !! rows from the bottom.
        type(parquet_slice), intent(in), optional :: rows !! explicit row selection.
        integer, intent(in), optional :: unit             !! where to write.
        integer, intent(in), optional :: digits           !! significant digits in a real cell.
        integer, intent(in), optional :: max_width        !! longest cell text.
        integer, intent(in), optional :: max_columns      !! most columns shown.
        integer :: u, dg, mw, mc, nshow, extra, j, i, nrow, gw
        integer(int64), allocatable :: rowlist(:)
        integer, allocatable :: w(:)
        logical, allocatable :: rt(:)
        logical :: gap
        character(len=:), allocatable :: cells(:,:), head(:), kinds(:), fname, line, txt
        character(len=32) :: buf
        !
        dg = DEF_DIGITS
        if (present(digits)) dg = digits
        mw = DEF_WIDTH
        if (present(max_width)) mw = max_width
        mc = DEF_COLUMNS
        if (present(max_columns)) mc = max_columns
        call resolve_unit(unit, u)
        !
        nshow = min(size(sel), mc)
        extra = size(sel) - nshow
        ! Resolved BEFORE anything is printed, and before the no-columns exit below: a `rows=`
        ! slice naming a row the table does not have is a caller mistake whatever happens to be
        ! resident, and reporting it only when there was something to show would make the guard
        ! depend on the residency of columns it says nothing about.
        call build_row_list(self, first, last, rows, rowlist, gap)
        nrow = int(size(rowlist))
        call self%filename(fname)
        if (len_trim(fname) > 0) then
            write(u, "(a)") "parquet_table: " // trim(fname)
        else
            write(u, "(a)") "parquet_table: (built in memory)"
        end if
        write(buf, "(I0)") self%row_count
        line = "  rows: " // trim(buf)
        write(buf, "(I0)") self%cache%ncols
        line = line // "   columns: " // trim(buf)
        write(buf, "(I0)") size(sel)
        write(u, "(a)") line // " (" // trim(buf) // " shown)"
        if (nshow == 0) then
            write(u, "(a)") "  (no materialized columns; name them with columns= or call " // &
                "%materialize first)"
            return
        end if
        !
        call build_headings(self%cache, sel(1:nshow), head, kinds)
        allocate(character(len=mw) :: cells(max(nrow, 1), nshow))
        do j = 1, nshow
            do i = 1, nrow
                call render_cell(self%cache%cols(sel(j))%values, rowlist(i), dg, mw, txt)
                cells(i, j) = txt
            end do
        end do
        !
        allocate(w(nshow), rt(nshow))
        do j = 1, nshow
            rt(j) = kind_is_numeric(self%cache%cols(sel(j))%values%kindof())
            w(j) = max(len_trim(head(j)), len_trim(kinds(j)))
            do i = 1, nrow
                w(j) = max(w(j), len_trim(cells(i, j)))
            end do
            if (gap) w(j) = max(w(j), len(ELLIPSIS))
        end do
        gw = len("row")
        do i = 1, nrow
            write(buf, "(I0)") rowlist(i)
            gw = max(gw, len_trim(buf))
        end do
        !
        ! The column names, then the kinds under them. The kind row is what tells a reader coming
        ! from another tool what each column became here -- `string` where the file said
        ! `dictionary`, say -- and it costs one line.
        line = "  " // pad("row", gw, right=.true.)
        do j = 1, nshow
            line = line // "  " // pad(head(j), w(j), right=rt(j))
        end do
        if (extra > 0) then
            write(buf, "(I0)") extra
            line = line // "  " // ELLIPSIS // " (+" // trim(buf) // " more)"
        end if
        write(u, "(a)") trim(line)
        line = "  " // pad(" ", gw)
        do j = 1, nshow
            line = line // "  " // pad(kinds(j), w(j), right=rt(j))
        end do
        write(u, "(a)") trim(line)
        !
        do i = 1, nrow
            ! The one "..." row, printed where the head stops and the tail starts.
            if (gap .and. i == nrow - trailing_count(first, last) + 1) then
                line = "  " // pad(ELLIPSIS, gw, right=.true.)
                do j = 1, nshow
                    line = line // "  " // pad(ELLIPSIS, w(j), right=rt(j))
                end do
                write(u, "(a)") trim(line)
            end if
            write(buf, "(I0)") rowlist(i)
            line = "  " // pad(trim(buf), gw, right=.true.)
            do j = 1, nshow
                line = line // "  " // pad(cells(i, j), w(j), right=rt(j))
            end do
            write(u, "(a)") trim(line)
        end do
    end subroutine display_rows
    !
    !> How many of the shown rows came from the BOTTOM of the table, for placing the "..." row.
    !!
    !! Only meaningful when `build_row_list` reported a gap, which it does only for the
    !! `first`/`last` regime -- a `rows=` slice is shown exactly as it was given, with no gap.
    integer function trailing_count(first, last) result(n)
        integer, intent(in), optional :: first !! rows from the top, as the caller gave it.
        integer, intent(in), optional :: last  !! rows from the bottom, as the caller gave it.
        !
        n = DEF_ROWS
        if (present(first) .or. present(last)) then
            n = 0
            if (present(last)) n = last
        end if
    end function trailing_count
    !
    !> The rows to show, in the order to show them, and whether a "..." row goes between the head
    !! and the tail.
    !!
    !! `rows=` is taken literally, including a repeated or descending index: the point of that
    !! form is to show the rows a lookup returned, in the order it returned them. Otherwise the
    !! head and the tail are clamped to the table and, when together they cover it, merged into
    !! one run with no gap -- so a 7-row table under the defaults prints all 7 rows and no "...".
    subroutine build_row_list(self, first, last, rows, rowlist, gap)
        class(parquet_table), intent(in) :: self               !! the table.
        integer, intent(in), optional :: first                 !! rows from the top.
        integer, intent(in), optional :: last                  !! rows from the bottom.
        type(parquet_slice), intent(in), optional :: rows      !! explicit row selection.
        integer(int64), allocatable, intent(out) :: rowlist(:) !! the rows, in display order.
        logical, intent(out) :: gap                            !! .true.: print one "..." row between them.
        integer(int64) :: n, k
        integer :: nf, nl, i
        !
        gap = .false.
        n = self%row_count
        if (present(rows)) then
            ! Range-checked by the slice machinery itself, so an out-of-range row is reported by
            ! the message every other slice consumer produces rather than a second copy of it.
            call slice_resolve(rows, n, rowlist, "print_rows")
            return
        end if
        nf = DEF_ROWS
        nl = DEF_ROWS
        ! Naming either end means the caller wants that end: %print_rows(first=20) is head(20)
        ! and %print_rows(last=3) is tail(3), rather than either of them plus five of the other.
        if (present(first) .or. present(last)) then
            nf = 0
            nl = 0
            if (present(first)) nf = first
            if (present(last)) nl = last
        end if
        ! The two ends meeting or overlapping is also what clamps them: either count exceeding
        ! the table makes the sum exceed it too, so the whole table is shown once, in order, and
        ! no gap is marked. An explicit min() against `n` before this test would be dead code --
        ! confirmed by mutation, by deleting one and watching nothing fail.
        if (int(nf, int64) + int(nl, int64) >= n) then
            allocate(rowlist(n))
            do k = 1_int64, n
                rowlist(k) = k
            end do
            return
        end if
        ! Below here both counts are strictly smaller than `n`, so every index built is in range.
        ! The arithmetic is int64 throughout: `nf` and `nl` are default integers a caller chose,
        ! and their SUM can overflow one on a table with more rows than an int32 holds, which is
        ! exactly the table where this branch is reachable with both of them large.
        gap = nf > 0 .and. nl > 0
        allocate(rowlist(int(nf, int64) + int(nl, int64)))
        do i = 1, nf
            rowlist(i) = int(i, int64)
        end do
        do i = 1, nl
            rowlist(int(nf, int64) + int(i, int64)) = n - int(nl, int64) + int(i, int64)
        end do
    end subroutine build_row_list
    !
    !> The two heading rows: each column's name, and its kind token with any unit and width.
    !!
    !! Measured, then filled. A deferred-length array has ONE length for every element, and a kind
    !! token carries the column's unit -- caller-supplied text of no knowable length -- so the
    !! tokens have to be built once to size the array and once to fill it. Two passes over at most
    !! `max_columns` columns, against carrying a fixed length that a long unit would truncate.
    subroutine build_headings(cache, sel, head, kinds)
        type(parquet_table_cache), intent(in) :: cache          !! the column store.
        integer, intent(in) :: sel(:)                           !! selected column slots.
        character(len=:), allocatable, intent(out) :: head(:)   !! the column names.
        character(len=:), allocatable, intent(out) :: kinds(:)  !! the kind tokens.
        character(len=:), allocatable :: txt
        integer :: j, hw, kw
        !
        hw = 1
        kw = 1
        do j = 1, size(sel)
            hw = max(hw, len_trim(cache%cols(sel(j))%name))
            call kind_token(cache%cols(sel(j)), txt)
            kw = max(kw, len_trim(txt))
        end do
        allocate(character(len=hw) :: head(size(sel, kind=int64)))
        allocate(character(len=kw) :: kinds(size(sel, kind=int64)))
        do j = 1, size(sel)
            head(j) = cache%cols(sel(j))%name
            call kind_token(cache%cols(sel(j)), txt)
            kinds(j) = txt
        end do
    end subroutine build_headings
    !
    !> One column's kind as the heading row shows it: `parquet_kind_name`'s spelling without the
    !! `PK_` prefix and lower-cased, `[width]` for a vector kind, then the unit in brackets.
    !!
    !! Taken from the column's OWN storage rather than from `declared_kind`, which is `PK_NONE`
    !! while a plain-`LIST` column's width is still pending -- and every column reaching here is
    !! resident, so its storage is the authority on what it turned out to be.
    subroutine kind_token(slot, text)
        type(parquet_table_column), intent(in) :: slot     !! the column slot.
        character(len=:), allocatable, intent(out) :: text !! the kind token.
        character(len=:), allocatable :: unit_s
        character(len=32) :: buf
        !
        call parquet_kind_name(slot%values%kindof(), text)
        if (len(text) > 3) then
            if (text(1:3) == "PK_") text = text(4:)
        end if
        call pf_to_lower(text)
        if (slot%values%colwidth() > 1) then
            write(buf, "(I0)") slot%values%colwidth()
            text = text // "[" // trim(buf) // "]"
        end if
        call slot%values%unit_string(unit_s)
        if (allocated(slot%unit)) unit_s = slot%unit
        if (len_trim(unit_s) > 0) text = text // " [" // trim(unit_s) // "]"
    end subroutine kind_token
    !
    !> `.true.` for the kinds whose cells are right-aligned -- the four scalar numeric ones.
    !!
    !! A vector cell is bracketed text and a temporal one is a fixed-width ISO string; both read
    !! better left-aligned, and neither lines up on a decimal point in any case.
    logical function kind_is_numeric(kind) result(res)
        integer, intent(in) :: kind !! a PK_* discriminator.
        !
        res = kind == PK_INT32 .or. kind == PK_INT64 .or. kind == PK_FLOAT32 .or. kind == PK_FLOAT64
    end function kind_is_numeric
    !
    !> Where the text goes: `unit=` when given, otherwise whatever `message_stream` names.
    subroutine resolve_unit(unit, u)
        integer, intent(in), optional :: unit !! the caller's unit, if any.
        integer, intent(out) :: u             !! the unit to write to.
        !
        if (present(unit)) then
            u = unit
            return
        end if
        u = parquet_message_unit()
    end subroutine resolve_unit
    !
    !> One cell as text, never longer than `maxw`.
    !!
    !! **A null is tested first, on every kind**, which is also what keeps a null temporal away
    !! from `%to_string` -- that would abort. A vector row counts as null when any of its elements
    !! is, exactly as `%is_null` reports it, so the whole cell becomes `<null>` rather than a
    !! bracket list with a hole in it.
    !!
    !! **`NaN` is a value, not a null**, and prints as the compiler spells it: the line the filter
    !! draws between the two, kept on the display side.
    subroutine render_cell(values, k, digits, maxw, text)
        type(parquet_column), intent(in) :: values         !! the column's storage.
        integer(int64), intent(in) :: k                    !! 1-based row.
        integer, intent(in) :: digits                      !! significant digits for a real.
        integer, intent(in) :: maxw                        !! longest text to produce.
        character(len=:), allocatable, intent(out) :: text !! the rendered cell.
        integer(int32), pointer :: p_i32(:)
        integer(int64), pointer :: p_i64(:)
        real(real32), pointer :: p_f32(:)
        real(real64), pointer :: p_f64(:)
        logical, pointer :: p_bool(:)
        type(parquet_date), pointer :: p_date(:)
        type(parquet_time), pointer :: p_time(:)
        type(parquet_timestamp), pointer :: p_ts(:)
        type(parquet_string_column), pointer :: store
        !
        text = NULL_TOKEN
        if (parquet_column_is_null(values, k)) return
        select case (values%kindof())
        case (PK_INT32)
            call values%data_ptr(p_i32)
            call pf_to_str(p_i32(k), text)
        case (PK_INT64)
            call values%data_ptr(p_i64)
            call pf_to_str(p_i64(k), text)
        case (PK_FLOAT32)
            call values%data_ptr(p_f32)
            call real_text(real(p_f32(k), real64), digits, text)
        case (PK_FLOAT64)
            call values%data_ptr(p_f64)
            call real_text(p_f64(k), digits, text)
        case (PK_LOGICAL)
            call values%data_ptr(p_bool)
            call pf_to_str(p_bool(k), text)
        case (PK_STRING)
            call values%string_column(store)
            call store%get(k, text)
            text = trim(text)
        case (PK_DATE)
            call values%data_ptr(p_date)
            call p_date(k)%to_string(text)
        case (PK_TIME)
            call values%data_ptr(p_time)
            call p_time(k)%to_string(text)
        case (PK_TIMESTAMP)
            call values%data_ptr(p_ts)
            call p_ts(k)%to_string(text)
        case (PK_INT32_VEC, PK_INT64_VEC, PK_FLOAT32_VEC, PK_FLOAT64_VEC, PK_LOGICAL_VEC, &
              PK_STRING_VEC, PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC)
            call vector_text(values, k, digits, maxw, text)
        case (PK_LIST, PK_MAP, PK_STRUCT)
            call container_text(values, k, text)
        case default
            ! PK_NONE: a slot whose type this library cannot hold. Selecting one is refused by
            ! table_resolve long before here, so this is the empty-slot case only.
            text = "-"
        end select
        if (len(text) > maxw) text = text(1:maxw - len(ELLIPSIS)) // ELLIPSIS
    end subroutine render_cell
    !
    !> One real value at `digits` significant digits, in the shape C's `%g` produces.
    !!
    !! **Fortran's own `G0.d` is not that shape, in two ways that both hurt a table.** It keeps
    !! every trailing zero, so `0.5` renders as `0.500000` and a column of round numbers becomes a
    !! column of noise; and it leaves fixed-point notation below 0.1 rather than below 1e-4, so
    !! `0.0123456789` renders as `0.123457E-1`. C switches at 1e-4 and strips the zeros, which is
    !! also what `print_stat` shows -- the reader's own doubles come from `%.6g` in the wrapper's
    !! `format_stat_double` -- so matching it here is what makes one value look the same in both
    !! of this library's printers.
    !!
    !! The rule, from C: with `x` written as `m * 10**xp` and `1 <= m < 10`, fixed-point form is
    !! used when `-4 <= xp < digits` and exponential otherwise; either way trailing zeros in the
    !! fraction go, and the point with them if nothing is left. `NaN` and the infinities have no
    !! `xp` at all and are left to the compiler to spell.
    subroutine real_text(x, digits, text)
        real(real64), intent(in) :: x                      !! the value.
        integer, intent(in) :: digits                      !! significant digits, 1..17.
        character(len=:), allocatable, intent(out) :: text !! the rendered value.
        character(len=64) :: buf, fmt
        integer :: xp, epos
        !
        if (ieee_is_nan(x) .or. .not. ieee_is_finite(x)) then
            call pf_to_str(x, text, fmt="(g0)")
            return
        end if
        ! Scientific first, and at an EXPLICIT width: gfortran drops the exponent field entirely
        ! from an `ES0.dE4` rendering whose exponent happens to be zero (`9.99990`, no `E`), so a
        ! zero-width descriptor cannot be parsed. Reading `xp` back from the rendering rather than
        ! computing it with log10 also accounts for the rounding -- 9.9999 at 3 digits is 1.00E+01,
        ! whose exponent is 1, not 0.
        write(fmt, "('(ES40.', i0, 'E4)')") max(digits - 1, 0)
        write(buf, fmt) x
        epos = index(buf, "E")
        read(buf(epos + 1:), *) xp
        if (xp < -4 .or. xp >= digits) then
            text = trim(adjustl(buf(1:epos - 1)))
            call strip_trailing_zeros(text)
            write(buf, "(i0)") abs(xp)
            if (xp < 0) then
                text = text // "e-"
            else
                text = text // "e+"
            end if
            ! At least two exponent digits, as C writes them.
            if (abs(xp) < 10) text = text // "0"
            text = text // trim(buf)
            return
        end if
        write(fmt, "('(F0.', i0, ')')") max(digits - 1 - xp, 0)
        write(buf, fmt) x
        text = trim(adjustl(buf))
        ! gfortran's `F0.d` omits the leading zero of a value below 1 (`.012346`), which C writes
        ! and which a display is much easier to read with -- a bare leading point reads as a typo.
        if (text(1:1) == ".") then
            text = "0" // text
        else if (len(text) > 1) then
            if (text(1:2) == "-.") text = "-0" // text(2:)
        end if
        call strip_trailing_zeros(text)
    end subroutine real_text
    !
    !> Drops the trailing zeros of a fixed-point rendering, and the decimal point with them when
    !! nothing is left after it -- `12.250000` becomes `12.25`, `1.000000` becomes `1`.
    !!
    !! Only ever called on text that came from an `F` or `ES` descriptor, so it is looking at
    !! digits, one point and an optional sign; a string with no point is returned untouched, which
    !! is what makes it safe to call unconditionally.
    subroutine strip_trailing_zeros(s)
        character(len=:), allocatable, intent(inout) :: s !! the rendering, shortened in place.
        integer :: n
        !
        if (index(s, ".") == 0) return
        n = len(s)
        do while (n > 1)
            if (s(n:n) /= "0") exit
            n = n - 1
        end do
        if (s(n:n) == ".") n = n - 1
        s = s(1:n)
    end subroutine strip_trailing_zeros
    !
    !> One vector row as `[v1, v2, v3]`, its elements rendered exactly as scalars are.
    !!
    !! Cut with `...]` rather than a bare `...` when it does not fit, so a truncated list still
    !! reads as a list. The row is known to be non-null before this is called.
    subroutine vector_text(values, k, digits, maxw, text)
        type(parquet_column), intent(in) :: values         !! the column's storage.
        integer(int64), intent(in) :: k                    !! 1-based row.
        integer, intent(in) :: digits                      !! significant digits for a real.
        integer, intent(in) :: maxw                        !! longest text to produce.
        character(len=:), allocatable, intent(out) :: text !! the rendered cell.
        integer(int32), pointer :: p_i32(:,:)
        integer(int64), pointer :: p_i64(:,:)
        real(real32), pointer :: p_f32(:,:)
        real(real64), pointer :: p_f64(:,:)
        logical, pointer :: p_bool(:,:)
        type(parquet_date), pointer :: p_date(:,:)
        type(parquet_time), pointer :: p_time(:,:)
        type(parquet_timestamp), pointer :: p_ts(:,:)
        type(parquet_string_column), pointer :: store
        character(len=:), allocatable :: one, sbuf
        integer(int64) :: base
        integer :: e, wdt, slen
        !
        wdt = values%colwidth()
        base = (k - 1_int64)*int(wdt, int64)
        ! A vector string column stores its width elements per row back to back, blank-padded to
        ! the widest of them -- the indexing stat_strv uses. The row's longest element is measured
        ! with %length, which allocates nothing, and ONE scratch slot is sized from it, so the loop
        ! below copies into a fixed-length buffer rather than taking a fresh deferred-length string
        ! out of the column per element (feature_risks.md Risk-60, and
        ! tools/check_source_conventions.py's "no per-element string allocation in a bulk loop").
        if (values%kindof() == PK_STRING_VEC) then
            call values%string_column(store)
            slen = 1
            do e = 1, wdt
                slen = max(slen, int(store%length(base + int(e, int64))))
            end do
            allocate(character(len=slen) :: sbuf)
        end if
        text = "["
        do e = 1, wdt
            select case (values%kindof())
            case (PK_INT32_VEC)
                call values%data_ptr(p_i32)
                call pf_to_str(p_i32(e, k), one)
            case (PK_INT64_VEC)
                call values%data_ptr(p_i64)
                call pf_to_str(p_i64(e, k), one)
            case (PK_FLOAT32_VEC)
                call values%data_ptr(p_f32)
                call real_text(real(p_f32(e, k), real64), digits, one)
            case (PK_FLOAT64_VEC)
                call values%data_ptr(p_f64)
                call real_text(p_f64(e, k), digits, one)
            case (PK_LOGICAL_VEC)
                call values%data_ptr(p_bool)
                call pf_to_str(p_bool(e, k), one)
            case (PK_DATE_VEC)
                call values%data_ptr(p_date)
                call p_date(e, k)%to_string(one)
            case (PK_TIME_VEC)
                call values%data_ptr(p_time)
                call p_time(e, k)%to_string(one)
            case (PK_TIMESTAMP_VEC)
                call values%data_ptr(p_ts)
                call p_ts(e, k)%to_string(one)
            case default
                ! PK_STRING_VEC, into the scratch slot sized above.
                call store%copy_to(base + int(e, int64), sbuf)
                one = trim(sbuf)
            end select
            if (e > 1) text = text // ", "
            text = text // one
            ! Stopped as soon as the text cannot fit, so a wide vector costs a few elements rather
            ! than all of them before being cut.
            if (len(text) >= maxw) exit
        end do
        text = text // "]"
        if (len(text) > maxw) text = text(1:maxw - len(ELLIPSIS) - 1) // ELLIPSIS // "]"
    end subroutine vector_text
    !
    !> A container row as `[n items]` / `{n pairs}` / `{n fields}`.
    !!
    !! A table display is not the place to walk a container: its payload is reachable only through
    !! its own handle, and one row of it can be arbitrarily long. The count is what a reader
    !! scanning the display can act on.
    subroutine container_text(values, k, text)
        type(parquet_column), intent(in) :: values         !! the column's storage.
        integer(int64), intent(in) :: k                    !! 1-based row.
        character(len=:), allocatable, intent(out) :: text !! the rendered cell.
        class(parquet_container_column), pointer :: c
        character(len=:), allocatable :: one
        !
        text = "-"
        call parquet_column_container(values, c)
        if (.not. associated(c)) return
        select type (c)
        type is (parquet_list_column)
            call pf_to_str(c%length(k), one)
            text = "[" // one // " items]"
        type is (parquet_map_column)
            call pf_to_str(c%length(k), one)
            text = "{" // one // " pairs}"
        type is (parquet_struct_column)
            call pf_to_str(c%field_count(), one)
            text = "{" // one // " fields}"
        end select
    end subroutine container_text
    !
end submodule parquet_tables_display
