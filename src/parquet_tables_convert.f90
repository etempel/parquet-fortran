!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> String to number and back: `%parse_column` and `%format_column`.
!!
!! The two directions of the one conversion `%cast` refuses. `%cast` moves between numeric
!! kinds, where every value has a representation in the target and the only question is
!! precision; text does not, so the interesting part of this pair is not the conversion at all
!! but what happens to the row that cannot be converted.
!!
!! **The failure policy is the design.** `invalid="error"` names the row, the column and the
!! offending text and stops; `invalid="null"` marks that row missing and carries on. There is
!! no third, silent policy, and that absence is exactly why `%cast` was right to leave this
!! conversion out rather than adopt it with a default. `%format_column` has no such argument
!! because rendering cannot fail on the value -- only on a `fmt` the runtime rejects, which
!! `pf_to_str` answers with its own asterisk marker.
!!
!! **The parse itself is `parquet_utils`', not this file's.** `pf_from_str` is strict for the
!! five numeric and logical targets and each temporal type's own `%parse` is strict for the
!! three temporal ones, so `"5 6"` is refused here for the same reason it is refused in a
!! settings variable. This file decides only *which* parser applies and what to do when it
!! says no.
!!
!! **Both verbs replace a column's storage and neither changes a row.** So both advance
!! `%generation()` and neither detaches -- the union rule CLAUDE.md records, met on the
!! reallocation side alone, exactly as `%cast` meets it. A column the table has not read yet is
!! still readable afterwards; an outstanding `%col` pointer into the converted column is not.
!!
!! **The converted column is marked as holding written values.** Its values are a function of
!! the file's rather than the file's own, and the reader cannot reproduce them -- it has no
!! string-to-number conversion of its own -- so `%reload` and `%evict_column` must refuse it
!! without `force=`. That is one flag rather than a new rule: `user_populated` already means
!! precisely "re-reading this from the file would lose something".
submodule (parquet_tables) parquet_tables_convert
    implicit none

    !> Longest offending text interpolated into an `error stop` message, before the ellipsis.
    !! ifx's ERROR STOP runtime corrupts the heap once the composed message reaches 8192 bytes,
    !! and a "this text is not a number" guard is guaranteed to be reached with caller-controlled
    !! text -- see CLAUDE.md's rule and `parquet_filter_add`, which caps at the same width.
    integer, parameter :: CV_PREVIEW = 100

contains

    ! ---- %parse_column -------------------------------------------------------------------------

    module procedure table_parse_column
        integer :: idx
        logical :: null_ok
        type(parquet_column), target :: out
        character(len=:), allocatable :: sfx, unit, kname
        !
        call convert_prepare(self, name, "parse_column", idx)
        call parse_policy(self, name, invalid, null_ok)
        if (self%cache%cols(idx)%declared_kind /= PK_STRING) then
            call parquet_kind_name(self%cache%cols(idx)%declared_kind, kname)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "parse_column: this column holds " // kname // &
                ", not text, so there is nothing to parse; %cast converts between numeric " // &
                "kinds and %format_column is the other direction" // sfx
        end if
        ! Refused BEFORE anything is built, so a rejected target leaves the table untouched.
        select case (to_kind)
        case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_DATE, PK_TIME, PK_TIMESTAMP)
            continue
        case default
            call parquet_kind_name(to_kind, kname)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "parse_column: " // kname // " is not a target this verb can " // &
                "parse into; the targets are int32, int64, float32, float64, logical, date, " // &
                "time and timestamp" // sfx
        end select
        !
        call convert_check_to_name(self, to_name, "parse_column")
        call convert_check_predefined(self, idx, to_name, force, "parse_column")
        ! Through %unit rather than off the descriptor, because a column's unit can live in
        ! EITHER place -- the slot when a MAML declared it, the values when %add_column was
        ! given one -- and that precedence is implemented once, in slot_unit. Reading the
        ! descriptor directly here silently lost the unit of every in-memory column. "" is what
        ! %init and %adopt already document as "no unit", so no `if` is needed on the way out.
        call self%unit(name, unit)
        call parse_build(self, idx, name, to_kind, null_ok, unit, out)
        call convert_install(self, idx, to_name, out, to_kind)
    end procedure table_parse_column
    !
    !> Reads `invalid=` into a flag, refusing anything but the two documented tokens.
    !!
    !! A silently-ignored misspelling would be the worst outcome available here: `invalid="skip"`
    !! accepted as the default would abort on the first bad row, which is the opposite of what
    !! the caller asked for.
    subroutine parse_policy(self, name, invalid, null_ok)
        class(parquet_table), intent(in) :: self             !! the table, for the message context.
        character(len=*), intent(in) :: name                 !! the column, for the message context.
        character(len=*), intent(in), optional :: invalid    !! the caller's token, if any.
        logical, intent(out) :: null_ok                      !! .true. for "null", .false. for "error".
        character(len=:), allocatable :: sfx
        !
        null_ok = .false.
        if (.not. present(invalid)) return
        select case (trim(adjustl(invalid)))
        case ("error")
            null_ok = .false.
        case ("null")
            null_ok = .true.
        case default
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "parse_column: invalid=""" // trim(adjustl(invalid)) // """ is " // &
                "not a policy; it is either ""error"" (abort on the first unreadable row, the " // &
                "default) or ""null"" (mark that row missing and carry on)" // sfx
        end select
    end subroutine parse_policy
    !
    !> Builds the converted column, one target kind per arm.
    !!
    !! Each arm is the same three steps -- a buffer of the target type, a walk that parses each
    !! non-null row or rejects it, and an `%adopt` that hands the buffer over rather than copying
    !! it -- differing only in the buffer's type and the parser called. They are written out
    !! rather than factored because Fortran has no way to be generic over the buffer's type;
    !! everything that is not per-type is already out of them (`parse_source`, `parse_reject`,
    !! `parse_nulls`, and the one scratch buffer below).
    !!
    !! **The parsed value lands in a local scalar first and in the buffer only on success**, in
    !! the five arms that use `pf_from_str`. Its `value` is `intent(out)` and is deliberately not
    !! assigned when the parse fails, so reading straight into `buf(i)` would leave a rejected
    !! row's element UNDEFINED -- and the buffer is adopted whole, so those bytes would become the
    !! column's. The three temporal arms parse into the element in place, which is safe for the
    !! opposite reason: `%parse` with `success=` leaves the element NULL, which is exactly the
    !! state a rejected row is about to be given anyway.
    subroutine parse_build(self, idx, name, to_kind, null_ok, unit, out)
        class(parquet_table), intent(in) :: self       !! the table.
        integer, intent(in) :: idx                     !! the source column's slot.
        character(len=*), intent(in) :: name           !! its name, for every message.
        integer, intent(in) :: to_kind                 !! the target PK_* kind.
        logical, intent(in) :: null_ok                 !! .true. to null an unreadable row.
        character(len=:), allocatable, intent(in) :: unit !! the unit to carry across, "" for none.
        type(parquet_column), intent(inout) :: out     !! receives the converted column.
        type(parquet_string_column), pointer :: store
        logical, allocatable :: bad(:)
        integer(int64) :: n, i, wid
        logical :: ok
        character(len=:), allocatable :: txt
        integer(int32), allocatable :: b32(:)
        integer(int64), allocatable :: b64(:)
        real(real32), allocatable :: r32(:)
        real(real64), allocatable :: r64(:)
        logical, allocatable :: blg(:)
        type(parquet_date), allocatable :: bdt(:)
        type(parquet_time), allocatable :: btm(:)
        type(parquet_timestamp), allocatable :: bts(:)
        integer(int32) :: v32
        integer(int64) :: v64
        real(real32) :: x32
        real(real64) :: x64
        logical :: vlg
        !
        call parse_source(self, idx, store, n, wid, bad)
        ! ONE scratch buffer, sized to the column's longest element and allocated once, which
        ! every arm below fills with `%copy_to` -- the primitive
        ! check_no_per_element_string_alloc names for exactly this, where a per-row `%get` would
        ! allocate once per element.
        !
        ! Allocatable rather than an automatic `character(len=wid)`, deliberately: `wid` is a
        ! property of the DATA, so a column holding one very long element would put a very long
        ! automatic on the stack -- and a stack overflow there is a segfault in a callee's
        ! prologue with no usable backtrace, which is the failure CLAUDE.md records for flang's
        ! character temporaries. One heap allocation for the whole walk has none of that and
        ! costs nothing beside the read that produced the column.
        allocate(character(len=wid) :: txt)
        select case (to_kind)
        case (PK_INT32)
            allocate(b32(n), source=0_int32)
            do i = 1, n
                if (bad(i)) cycle
                call store%copy_to(i, txt)
                call pf_from_str(txt, v32, ok)
                if (ok) then
                    b32(i) = v32
                else
                    call parse_reject(self, name, i, txt, "int32", null_ok, bad(i))
                end if
            end do
            call out%adopt(b32, unit)
        case (PK_INT64)
            allocate(b64(n), source=0_int64)
            do i = 1, n
                if (bad(i)) cycle
                call store%copy_to(i, txt)
                call pf_from_str(txt, v64, ok)
                if (ok) then
                    b64(i) = v64
                else
                    call parse_reject(self, name, i, txt, "int64", null_ok, bad(i))
                end if
            end do
            call out%adopt(b64, unit)
        case (PK_FLOAT32)
            allocate(r32(n), source=0.0_real32)
            do i = 1, n
                if (bad(i)) cycle
                call store%copy_to(i, txt)
                call pf_from_str(txt, x32, ok)
                if (ok) then
                    r32(i) = x32
                else
                    call parse_reject(self, name, i, txt, "float32", null_ok, bad(i))
                end if
            end do
            call out%adopt(r32, unit)
        case (PK_FLOAT64)
            allocate(r64(n), source=0.0_real64)
            do i = 1, n
                if (bad(i)) cycle
                call store%copy_to(i, txt)
                call pf_from_str(txt, x64, ok)
                if (ok) then
                    r64(i) = x64
                else
                    call parse_reject(self, name, i, txt, "float64", null_ok, bad(i))
                end if
            end do
            call out%adopt(r64, unit)
        case (PK_LOGICAL)
            allocate(blg(n), source=.false.)
            do i = 1, n
                if (bad(i)) cycle
                call store%copy_to(i, txt)
                call pf_from_str(txt, vlg, ok)
                if (ok) then
                    blg(i) = vlg
                else
                    call parse_reject(self, name, i, txt, "logical", null_ok, bad(i))
                end if
            end do
            call out%adopt(blg, unit)
        case (PK_DATE)
            allocate(bdt(n))
            do i = 1, n
                if (bad(i)) cycle
                call store%copy_to(i, txt)
                ! trim: %parse takes the whole argument, so the blank padding %copy_to leaves
                ! would itself be a parse failure. pf_from_str trims for the same reason,
                ! inside itself.
                call bdt(i)%parse(trim(txt), success=ok)
                if (.not. ok) call parse_reject(self, name, i, txt, "date", null_ok, bad(i))
            end do
            call out%adopt(bdt, unit)
        case (PK_TIME)
            allocate(btm(n))
            do i = 1, n
                if (bad(i)) cycle
                call store%copy_to(i, txt)
                call btm(i)%parse(trim(txt), success=ok)
                if (.not. ok) call parse_reject(self, name, i, txt, "time", null_ok, bad(i))
            end do
            call out%adopt(btm, unit)
        ! PK_TIMESTAMP, by elimination: %parse_column has already refused every kind that is not
        ! one of the eight, so a named eighth arm would be unreachable rather than defensive.
        case default
            allocate(bts(n))
            do i = 1, n
                if (bad(i)) cycle
                call store%copy_to(i, txt)
                call bts(i)%parse(trim(txt), success=ok)
                if (.not. ok) call parse_reject(self, name, i, txt, "timestamp", null_ok, bad(i))
            end do
            call out%adopt(bts, unit)
        end select
        call parse_nulls(out, bad)
    end subroutine parse_build
    !
    !> Resolves the source column's string store, its row count, its longest element and the
    !> rows that are already Null.
    !!
    !! The longest element is measured with `%length`, which allocates nothing, and it sizes the
    !! one scratch buffer `parse_build` fills once per row. A null row is entered in
    !! `bad` here rather than tested inside each arm, which is also what keeps `%copy_to` off a
    !! null element -- it would otherwise have to be told to allow one.
    subroutine parse_source(self, idx, store, n, wid, bad)
        class(parquet_table), intent(in) :: self                    !! the table.
        integer, intent(in) :: idx                                  !! the source column's slot.
        type(parquet_string_column), pointer, intent(out) :: store   !! its string storage.
        integer(int64), intent(out) :: n                            !! its row count.
        integer(int64), intent(out) :: wid                          !! its longest element, at least 1.
        logical, allocatable, intent(out) :: bad(:)                 !! .true. for a row that is already Null.
        integer(int64) :: i, longest
        !
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! The COLUMN's own row count, not the table's. They agree -- a resident column holds
        ! exactly the table's rows, a slice's included -- but this is the one every index below
        ! is bounds-checked against, so taking it from here cannot drift from what `%copy_to`
        ! and `parquet_column_is_null` will accept. `%get` on a string column does the same.
        n = self%cache%cols(idx)%values%length()
        allocate(bad(n), source=.false.)
        longest = 0_int64
        do i = 1, n
            if (parquet_column_is_null(self%cache%cols(idx)%values, i)) then
                bad(i) = .true.
                cycle
            end if
            longest = max(longest, store%length(i))
        end do
        ! At least 1, because a zero-length scratch buffer is legal but pointless and an all-null
        ! column would otherwise produce one. int64 throughout: a string element longer than
        ! huge(int32) bytes is possible here (the store's offsets are int64), and narrowing it
        ! would wrap into a SHORT buffer -- which truncates the text and hands the parser a
        ! different number rather than refusing it.
        wid = max(longest, 1_int64)
    end subroutine parse_source
    !
    !> Handles one row whose text could not be read: aborts under `invalid="error"`, marks the
    !> row Null under `invalid="null"`.
    subroutine parse_reject(self, name, row, text, target, null_ok, flag)
        class(parquet_table), intent(in) :: self  !! the table, for the message context.
        character(len=*), intent(in) :: name      !! the column.
        integer(int64), intent(in) :: row         !! the 1-based row.
        character(len=*), intent(in) :: text      !! the offending text, blank-padded.
        character(len=*), intent(in) :: target    !! the target kind, in words.
        logical, intent(in) :: null_ok            !! .true. to null the row instead of aborting.
        logical, intent(out) :: flag              !! set .true. when the row is to be nulled.
        character(len=:), allocatable :: sfx, shown
        character(len=32) :: rowtxt
        !
        flag = null_ok
        if (null_ok) return
        call preview_text(text, shown)
        write (rowtxt, '(i0)') row
        call table_context_suffix(self%cache, name, sfx)
        error stop EP // "parse_column: row " // trim(rowtxt) // " holds """ // shown // &
            """, which is not readable as " // target // "; pass invalid=""null"" to mark " // &
            "such rows missing instead of stopping" // sfx
    end subroutine parse_reject
    !
    !> Replays the collected nulls onto the freshly adopted column.
    !!
    !! One loop for all eight targets. For the three temporal kinds an untouched buffer element
    !! is already Null -- that is what a default-initialised `parquet_date` is -- so this is a
    !! no-op there; doing it anyway keeps one loop rather than two and does not rest on that.
    subroutine parse_nulls(out, bad)
        type(parquet_column), intent(inout) :: out !! the adopted column.
        logical, intent(in) :: bad(:)              !! .true. for each row to mark Null.
        integer(int64) :: i
        !
        do i = 1, size(bad, kind=int64)
            if (bad(i)) call parquet_column_set_null(out, i)
        end do
    end subroutine parse_nulls

    ! ---- %format_column ------------------------------------------------------------------------

    module procedure table_format_column
        integer :: idx
        type(parquet_column), target :: out
        type(parquet_string_column), pointer :: dest
        type(parquet_string_column) :: text
        character(len=:), allocatable :: sfx, unit, kname
        !
        call convert_prepare(self, name, "format_column", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_DATE, PK_TIME, PK_TIMESTAMP)
            continue
        case (PK_STRING)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "format_column: this column is already text, so there is nothing " // &
                "to render; %parse_column is the other direction" // sfx
        case default
            call parquet_kind_name(self%cache%cols(idx)%declared_kind, kname)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "format_column: a " // kname // " column cannot be rendered as " // &
                "one string per row; the sources are the scalar numeric, logical and " // &
                "temporal kinds" // sfx
        end select
        if (present(fmt)) then
            select case (self%cache%cols(idx)%declared_kind)
            case (PK_DATE, PK_TIME, PK_TIMESTAMP)
                call parquet_kind_name(self%cache%cols(idx)%declared_kind, kname)
                call table_context_suffix(self%cache, name, sfx)
                error stop EP // "format_column: fmt= is not accepted for a " // kname // &
                    " column, which renders as ISO-8601 and has no format to vary" // sfx
            end select
        end if
        !
        call convert_check_to_name(self, to_name, "format_column")
        call convert_check_predefined(self, idx, to_name, force, "format_column")
        call self%unit(name, unit)   ! see %parse_column's own call for why not off the descriptor
        call format_build(self, idx, fmt, text)
        call out%init(PK_STRING, text%size(), 1_int32, unit)
        call parquet_column_string_column(out, dest)
        call dest%move_from(text)
        call convert_install(self, idx, to_name, out, PK_STRING)
    end procedure table_format_column
    !
    !> Renders every row of a numeric, logical or temporal column into a fresh string column.
    !!
    !! `pf_to_str` and `%to_string` both hand back a `character(len=:), allocatable`, so this is
    !! one allocation per row -- and unlike a per-row `%get` on a STRING column, which
    !! `check_no_per_element_string_alloc` exists to keep out of `src/`, there is nothing here it
    !! could be traded for: the allocation sits beside an internal `write`, which costs an order
    !! of magnitude more, and the width a fixed buffer would need is set by a caller's `fmt`
    !! rather than by the data. Reserving the store up front is what removes the other half of
    !! the cost, the repeated regrowth.
    subroutine format_build(self, idx, fmt, text)
        class(parquet_table), intent(in) :: self            !! the table.
        integer, intent(in) :: idx                          !! the source column's slot.
        character(len=*), intent(in), optional :: fmt       !! the caller's format, if any.
        type(parquet_string_column), intent(inout) :: text  !! receives one element per row.
        character(len=:), allocatable :: rendered
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        real(real32), pointer :: q32(:)
        real(real64), pointer :: q64(:)
        logical, pointer :: plg(:)
        type(parquet_date), pointer :: pdt(:)
        type(parquet_time), pointer :: ptm(:)
        type(parquet_timestamp), pointer :: pts(:)
        integer(int64) :: n, i
        !
        n = self%cache%cols(idx)%values%length()   ! the column's own, per parse_source's note
        ! 8 bytes per row is a guess at the payload, not a bound: the store grows geometrically
        ! past it, so this only removes the first several regrowths. Deliberately on the low
        ! side -- under-reserving costs a regrowth, over-reserving costs memory that is never
        ! handed back, and a `logical` column renders to 4 or 5 bytes a row.
        call text%reserve(n, 8_int64 * n)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p32)
            do i = 1, n
                if (format_null(self, idx, i, text)) cycle
                call pf_to_str(p32(i), rendered, fmt=fmt)
                call text%append_string(rendered)
            end do
        case (PK_INT64)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p64)
            do i = 1, n
                if (format_null(self, idx, i, text)) cycle
                call pf_to_str(p64(i), rendered, fmt=fmt)
                call text%append_string(rendered)
            end do
        case (PK_FLOAT32)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, q32)
            do i = 1, n
                if (format_null(self, idx, i, text)) cycle
                call pf_to_str(q32(i), rendered, fmt=fmt)
                call text%append_string(rendered)
            end do
        case (PK_FLOAT64)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, q64)
            do i = 1, n
                if (format_null(self, idx, i, text)) cycle
                call pf_to_str(q64(i), rendered, fmt=fmt)
                call text%append_string(rendered)
            end do
        case (PK_LOGICAL)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, plg)
            do i = 1, n
                if (format_null(self, idx, i, text)) cycle
                call pf_to_str(plg(i), rendered, fmt=fmt)
                call text%append_string(rendered)
            end do
        case (PK_DATE)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pdt)
            do i = 1, n
                if (format_null(self, idx, i, text)) cycle
                call pdt(i)%to_string(rendered)
                call text%append_string(rendered)
            end do
        case (PK_TIME)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, ptm)
            do i = 1, n
                if (format_null(self, idx, i, text)) cycle
                call ptm(i)%to_string(rendered)
                call text%append_string(rendered)
            end do
        case default
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pts)
            do i = 1, n
                if (format_null(self, idx, i, text)) cycle
                call pts(i)%to_string(rendered)
                call text%append_string(rendered)
            end do
        end select
    end subroutine format_build
    !
    !> Appends a null element for a Null source row and answers `.true.`, so that each arm of
    !> `format_build` opens with one line rather than an `if`/`else` around its whole body.
    !!
    !! It is also what keeps `%to_string` off a null temporal element, which aborts rather than
    !! rendering -- so the guard is load-bearing for three of the eight arms and merely tidy for
    !! the other five.
    logical function format_null(self, idx, row, text) result(was_null)
        class(parquet_table), intent(in) :: self            !! the table.
        integer, intent(in) :: idx                          !! the source column's slot.
        integer(int64), intent(in) :: row                   !! the 1-based row.
        type(parquet_string_column), intent(inout) :: text  !! the column being built.
        !
        was_null = parquet_column_is_null(self%cache%cols(idx)%values, row)
        if (was_null) call text%append_null()
    end function format_null

    ! ---- shared plumbing -------------------------------------------------------------------------

    !> Runs both verbs' guards and resolves the source column, reading it if it is not resident.
    !!
    !! `writing=.false.`: neither verb writes into the column it is given -- both build a new one
    !! beside it and install that -- so the string-column rule `table_resolve` carries for a write
    !! does not apply, and a shared table is refused outright one line above by
    !! `table_check_not_shared` instead.
    subroutine convert_prepare(self, name, proc, idx)
        class(parquet_table), intent(in) :: self  !! the table.
        character(len=*), intent(in) :: name      !! the column named by the caller.
        character(len=*), intent(in) :: proc      !! calling procedure, for every message.
        integer, intent(out) :: idx               !! its slot.
        !
        call table_check_not_shared(self, proc)
        call table_check_open(self, proc)
        call table_resolve(self, name, proc, idx, writing=.false.)
        if (.not. self%cache%cols(idx)%supported) call table_unsupported_column_abort(self%cache, idx, trim(proc))
    end subroutine convert_prepare
    !
    !> Refuses a `to_name` that is already a column, before any work is done.
    !!
    !! `table_new_slot` would refuse it anyway -- but its message names `%add_column` and offers
    !! a `force=` neither of these verbs has, and it fires only AFTER the whole column has been
    !! converted. Checking here costs one lookup and makes a rejected call both free and
    !! actionable.
    subroutine convert_check_to_name(self, to_name, proc)
        class(parquet_table), intent(in) :: self            !! the table.
        character(len=*), intent(in), optional :: to_name   !! the caller's new-column name, if any.
        character(len=*), intent(in) :: proc                !! calling procedure, for the message.
        character(len=:), allocatable :: sfx
        !
        if (.not. present(to_name)) return
        if (.not. self%has_column(trim(to_name))) return
        call table_context_suffix(self%cache, trim(to_name), sfx)
        error stop EP // trim(proc) // ": to_name=""" // trim(to_name) // """ is already a " // &
            "column of this table; this verb never replaces one, so drop or rename it first" // sfx
    end subroutine convert_check_to_name
    !
    !> Refuses an IN-PLACE conversion of a predefined column unless `force` is .true.
    !!
    !! The mirror of `%drop_column`'s guard and of `column_keep_apply`'s
    !! (src/parquet_tables_matrix.f90), one step further along: those protect a generated table
    !! type's accessor from losing its COLUMN, this one from losing its KIND. A generated
    !! accessor is declared at the kind its schema gave it, so a converted column leaves it
    !! aborting inside `cache_require_ptr_kind` on the next call -- at a site other than the one
    !! that caused it, which is the diagnostic problem `%rename_column`'s own guard exists to
    !! prevent.
    !!
    !! `to_name` is never refused: it writes a NEW column and leaves the predefined one exactly
    !! as it was, so there is nothing to protect.
    !!
    !! Impure by design, as every guard-only subroutine here is: ifx deletes a `pure` one at
    !! -O0 (.claude/rules/api-conventions.md).
    subroutine convert_check_predefined(self, idx, to_name, force, proc)
        class(parquet_table), intent(in) :: self           !! the table.
        integer, intent(in) :: idx                         !! the source column's slot.
        character(len=*), intent(in), optional :: to_name  !! the caller's new-column name, if any.
        logical, intent(in), optional :: force             !! the caller's override, if any.
        character(len=*), intent(in) :: proc               !! calling procedure, for the message.
        character(len=:), allocatable :: sfx
        logical :: forced
        !
        if (present(to_name)) return
        if (.not. self%cache%cols(idx)%predefined) return
        forced = .false.
        if (present(force)) forced = force
        if (forced) return
        call table_context_suffix(self%cache, self%cache%cols(idx)%name, sfx)
        error stop EP // trim(proc) // ": this is a predefined column, whose generated " // &
            "accessor has its kind compiled in, so converting it in place would leave that " // &
            "accessor aborting on its next call; pass to_name= to write the result into a new " // &
            "column beside it, or force=.true. if you really mean to convert this one" // sfx
    end subroutine convert_check_predefined
    !
    !> Caps caller-controlled text for an `error stop` message. See `CV_PREVIEW`.
    subroutine preview_text(text, shown)
        character(len=*), intent(in) :: text                 !! the text, blank-padded.
        character(len=:), allocatable, intent(out) :: shown  !! at most CV_PREVIEW characters, plus "...".
        integer :: n
        !
        n = len_trim(text)
        if (n > CV_PREVIEW) then
            shown = text(1:CV_PREVIEW) // "..."
        else
            shown = text(1:n)
        end if
    end subroutine preview_text
    !
    !> Puts the converted column where the caller asked for it -- over the source, or in a new
    !> slot -- and updates the descriptor to match.
    !!
    !! The two paths differ only in which slot receives the column, which is why they share this
    !! procedure rather than being written out twice: every flag below has to be set the same way
    !! on both, and a `to_name` path that forgot `user_populated` would leave a new column
    !! `%reload` believes it can re-read from a file that holds text.
    !!
    !! `move_from`, never an assignment: the built column is a local about to be discarded, so
    !! its storage is handed over rather than deep-copied.
    subroutine convert_install(self, idx, to_name, out, kind)
        class(parquet_table), intent(inout) :: self          !! the table.
        integer, intent(in) :: idx                           !! the source column's slot.
        character(len=*), intent(in), optional :: to_name    !! name for a new column, if any.
        type(parquet_column), intent(inout) :: out           !! the converted column; left empty.
        integer, intent(in) :: kind                          !! the kind it now holds.
        integer :: dst                                       !! the slot it lands in.
        !
        if (present(to_name)) then
            ! table_new_slot may reallocate the descriptor array, which is why `out` is built in
            ! full before this point rather than into the destination slot: `idx` survives that
            ! (it is an index) but a pointer into a slot would not.
            call table_new_slot(self, to_name, .false., dst)
            ! Carried over so the two paths are identical in every field, which is the whole
            ! reason they share this procedure. See the note below for what the resolution is
            ! doing on a column that may not be temporal at all.
            self%cache%cols(dst)%time_unit = self%cache%cols(idx)%time_unit
            self%cache%cols(dst)%time_utc = self%cache%cols(idx)%time_utc
        else
            dst = idx
        end if
        call self%cache%cols(dst)%values%move_from(out)
        self%cache%cols(dst)%declared_kind = kind
        self%cache%cols(dst)%width = 1
        self%cache%cols(dst)%residency = RES_FULL
        self%cache%cols(dst)%cast_pending = .false.
        ! The values are this call's, not the file's, and the reader cannot reproduce them: it has
        ! no string-to-number conversion. So %reload and %evict_column must refuse the column
        ! without force=, which is exactly what this flag already means.
        self%cache%cols(dst)%user_populated = .true.
        ! The recorded TIME/TIMESTAMP resolution is KEPT rather than cleared, which is the
        ! opposite of the instinct. It is recoverable from nowhere else, and clearing it would
        ! make %format_column followed by %parse_column back to the same kind lose it -- after
        ! which a schema-less parquet_write_table does not merely round a nanosecond column, it
        ! ABORTS, because the writer defaults to microseconds and %to_unix refuses to truncate.
        ! Keeping it is safe because schema_type_token reads the field for PK_TIME and
        ! PK_TIMESTAMP and for nothing else -- not for PK_STRING, not for PK_DATE -- so a
        ! resolution riding along on a column that is currently text is unread until the column
        ! is temporal again, when it is exactly right.
        self%cache%generation = self%cache%generation + 1_int64
    end subroutine convert_install

end submodule parquet_tables_convert
