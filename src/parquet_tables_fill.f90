!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The missing-data family: `%fillna`, `%ffill`, `%bfill` and `%dropna`.
!!
!! Three of the four write VALUES and change no row; `%dropna` changes only rows and writes no
!! value. That split is why they share a file: everything here starts from one validity mask and
!! either fills what it marks or drops it.
!!
!! **A null lives in one of THREE places, and every procedure here has to know which.** A numeric
!! or logical column keeps it in a packed bitmap beside the values; a string column keeps it in
!! the `parquet_string_column`'s own validity; a temporal column keeps it INSIDE the element,
!! which is why `%clear_null` refuses a temporal kind outright ("a temporal element becomes valid
!! by writing a value to it"). A fill that only writes the value leaves a bitmap column reading
!! back as the sentinel AND as Null -- which is the workaround `%fillna` exists to replace, and
!! the one failure in this file that no `%get` can see. See feature_risks.md Risk-201.
!!
!! **Nothing here reallocates**, so no `%col` pointer dies and no table detaches -- except
!! `%dropna`, which computes a mask and hands it to `table_apply_keep`, inheriting that
!! procedure's detach rule, its all-`.true.` no-op and its generation bump rather than repeating
!! any of them. That is the same division of labour `parquet_tables_filter.f90` follows: this file
!! decides which rows go, `parquet_tables_rowmutate.f90` owns what happens to them.
submodule (parquet_tables) parquet_tables_fill
    ! The string store's two bulk forms the string arms below are written on. A submodule-level
    ! import, so `parquet_tables`' own footprint is unchanged (the module already compiles
    ! parquet_strings for the type).
    use parquet_strings, only : parquet_string_column_set_where, parquet_string_column_gather
    implicit none

    !> What kind of thing a caller handed `%fillna`, independent of which of the eighteen
    !! specifics they reached. Resolving the value to one of these first is what lets ONE
    !! compatibility rule and ONE writer serve every entry point.
    integer, parameter :: FV_INT = 1   !! an integer value, of either kind.
    integer, parameter :: FV_REAL = 2  !! a real value, of either kind.
    integer, parameter :: FV_BOOL = 3  !! a logical value.
    integer, parameter :: FV_STR = 4   !! a character value.
    integer, parameter :: FV_DATE = 5  !! a parquet_date.
    integer, parameter :: FV_TIME = 6  !! a parquet_time.
    integer, parameter :: FV_TS = 7    !! a parquet_timestamp.

    !> Which rows `%dropna` keeps, resolved from `how`/`min_valid` before the columns are counted.
    integer, parameter :: POLICY_ANY = 1     !! how="any": drop a row null in any named column.
    integer, parameter :: POLICY_ALL = 2     !! how="all": drop it only when every one is null.
    integer, parameter :: POLICY_THRESH = 3  !! min_valid=k: keep a row with k non-null columns.

    !> One `%fillna` value, normalised out of whichever specific received it.
    !!
    !! Both integer kinds arrive in `ival` and both real kinds in `rval`, which loses nothing: an
    !! `int32` widens into `int64` exactly, and a `real32` into `real64` exactly. What it buys is
    !! that the int32-into-an-int32-column range check below is written once and is correct for a
    !! value that arrived as either kind.
    type :: fill_value
        integer :: vclass = 0                     !! FV_*; 0 until one of the constructors runs.
        integer(int64) :: ival = 0_int64          !! the value when vclass is FV_INT.
        real(real64) :: rval = 0.0_real64         !! the value when vclass is FV_REAL.
        logical :: lval = .false.                 !! the value when vclass is FV_BOOL.
        character(len=:), allocatable :: sval     !! the value when vclass is FV_STR.
        type(parquet_date) :: dval                !! the value when vclass is FV_DATE.
        type(parquet_time) :: tval                !! the value when vclass is FV_TIME.
        type(parquet_timestamp) :: tsval          !! the value when vclass is FV_TS.
    end type fill_value

contains

    ! ---- %fillna: the eighteen entry points ---------------------------------------------------
    !
    ! Each is three lines, and deliberately so: the value is normalised, and everything that could
    ! differ between them -- which columns are legal, what the message says, how the three storage
    ! classes are written -- happens once, further down. The `_string` half splits and calls the
    ! generic, which is how every other name-list pair in this layer is built (%require_columns,
    ! %sort_by, %join).

    module procedure table_fillna_i32
        type(fill_value) :: fv
        !
        fv%vclass = FV_INT
        fv%ival = int(value, int64)
        call fillna_apply(self, names, fv)
    end procedure table_fillna_i32
    !
    module procedure table_fillna_string_i32
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%fillna(toks, value)
    end procedure table_fillna_string_i32
    !
    module procedure table_fillna_i64
        type(fill_value) :: fv
        !
        fv%vclass = FV_INT
        fv%ival = value
        call fillna_apply(self, names, fv)
    end procedure table_fillna_i64
    !
    module procedure table_fillna_string_i64
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%fillna(toks, value)
    end procedure table_fillna_string_i64
    !
    module procedure table_fillna_f32
        type(fill_value) :: fv
        !
        fv%vclass = FV_REAL
        fv%rval = real(value, real64)
        call fillna_apply(self, names, fv)
    end procedure table_fillna_f32
    !
    module procedure table_fillna_string_f32
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%fillna(toks, value)
    end procedure table_fillna_string_f32
    !
    module procedure table_fillna_f64
        type(fill_value) :: fv
        !
        fv%vclass = FV_REAL
        fv%rval = value
        call fillna_apply(self, names, fv)
    end procedure table_fillna_f64
    !
    module procedure table_fillna_string_f64
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%fillna(toks, value)
    end procedure table_fillna_string_f64
    !
    module procedure table_fillna_bool
        type(fill_value) :: fv
        !
        fv%vclass = FV_BOOL
        fv%lval = value
        call fillna_apply(self, names, fv)
    end procedure table_fillna_bool
    !
    module procedure table_fillna_string_bool
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%fillna(toks, value)
    end procedure table_fillna_string_bool
    !
    module procedure table_fillna_chr
        type(fill_value) :: fv
        !
        fv%vclass = FV_STR
        ! Stored VERBATIM, blanks included, because `value` is a scalar: CLAUDE.md's rule is that
        ! a character ARRAY is trimmed on the way into a column (its elements share one declared
        ! length, so the padding cannot be what the caller meant) and a character SCALAR is not.
        ! `%set_at`'s scalar form does the same, and this is that write repeated over the nulls.
        fv%sval = value
        call fillna_apply(self, names, fv)
    end procedure table_fillna_chr
    !
    module procedure table_fillna_string_chr
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%fillna(toks, value)
    end procedure table_fillna_string_chr
    !
    module procedure table_fillna_date
        type(fill_value) :: fv
        !
        fv%vclass = FV_DATE
        fv%dval = value
        call fillna_apply(self, names, fv)
    end procedure table_fillna_date
    !
    module procedure table_fillna_string_date
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%fillna(toks, value)
    end procedure table_fillna_string_date
    !
    module procedure table_fillna_time
        type(fill_value) :: fv
        !
        fv%vclass = FV_TIME
        fv%tval = value
        call fillna_apply(self, names, fv)
    end procedure table_fillna_time
    !
    module procedure table_fillna_string_time
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%fillna(toks, value)
    end procedure table_fillna_string_time
    !
    module procedure table_fillna_ts
        type(fill_value) :: fv
        !
        fv%vclass = FV_TS
        fv%tsval = value
        call fillna_apply(self, names, fv)
    end procedure table_fillna_ts
    !
    module procedure table_fillna_string_ts
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%fillna(toks, value)
    end procedure table_fillna_string_ts
    !
    ! ---- %ffill / %bfill: the twelve entry points ---------------------------------------------
    !
    ! `limit` is optional and comes in both integer kinds, so each direction is a base specific
    ! plus two kinded ones, per CLAUDE.md's rule that an optional dummy differing only by kind
    ! cannot disambiguate a generic. %join's max_rows is the same shape.

    module procedure table_ffill
        call scan_fill_apply(self, names, .true., "ffill")
    end procedure table_ffill
    !
    module procedure table_ffill_limit_i32
        call scan_fill_apply(self, names, .true., "ffill", int(limit, int64))
    end procedure table_ffill_limit_i32
    !
    module procedure table_ffill_limit_i64
        call scan_fill_apply(self, names, .true., "ffill", limit)
    end procedure table_ffill_limit_i64
    !
    module procedure table_ffill_string
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call scan_fill_apply(self, toks, .true., "ffill")
    end procedure table_ffill_string
    !
    module procedure table_ffill_string_limit_i32
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call scan_fill_apply(self, toks, .true., "ffill", int(limit, int64))
    end procedure table_ffill_string_limit_i32
    !
    module procedure table_ffill_string_limit_i64
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call scan_fill_apply(self, toks, .true., "ffill", limit)
    end procedure table_ffill_string_limit_i64
    !
    module procedure table_bfill
        call scan_fill_apply(self, names, .false., "bfill")
    end procedure table_bfill
    !
    module procedure table_bfill_limit_i32
        call scan_fill_apply(self, names, .false., "bfill", int(limit, int64))
    end procedure table_bfill_limit_i32
    !
    module procedure table_bfill_limit_i64
        call scan_fill_apply(self, names, .false., "bfill", limit)
    end procedure table_bfill_limit_i64
    !
    module procedure table_bfill_string
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call scan_fill_apply(self, toks, .false., "bfill")
    end procedure table_bfill_string
    !
    module procedure table_bfill_string_limit_i32
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call scan_fill_apply(self, toks, .false., "bfill", int(limit, int64))
    end procedure table_bfill_string_limit_i32
    !
    module procedure table_bfill_string_limit_i64
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call scan_fill_apply(self, toks, .false., "bfill", limit)
    end procedure table_bfill_string_limit_i64
    !
    ! ---- %dropna: the three entry points ------------------------------------------------------

    module procedure table_dropna
        integer, allocatable :: slots(:)
        !
        call fill_prepare(self, names, "dropna", slots, writing=.false.)
        call dropna_apply(self, slots, min_valid, how)
    end procedure table_dropna
    !
    module procedure table_dropna_string
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%dropna(toks, min_valid, how)
    end procedure table_dropna_string
    !
    module procedure table_dropna_all
        integer, allocatable :: slots(:)
        !
        call table_check_not_shared(self, "dropna")
        call table_check_open(self, "dropna")
        ! Resident columns, not every column -- see the interface's own doc-comment for why, and
        ! doc/pages/tables/table-mutate.md for the same rule stated to a user. table_mutable_slots
        ! is exactly this set (resident AND supported) and is what every row-structural operation
        ! already rewrites, so "the columns %dropna looks at" and "the columns a drop would
        ! rewrite" cannot come apart.
        call table_mutable_slots(self, slots)
        call dropna_apply(self, slots, min_valid, how)
    end procedure table_dropna_all
    !
    ! ---- shared plumbing ----------------------------------------------------------------------
    !
    !> Runs the guards, resolves every name to a slot, and refuses the columns none of these verbs
    !> can act on -- once, for all four.
    !>
    !> `table_resolve` READS a column that is not resident yet, which is the documented cost of
    !> naming one. `writing=` carries `table_check_shared_write`'s string-column rule for the three
    !> verbs that write values; `%dropna` passes `.false.` because it writes none.
    subroutine fill_prepare(self, names, proc, slots, writing)
        class(parquet_table), intent(in) :: self       !! the table.
        character(len=*), intent(in) :: names(:)       !! the columns named by the caller.
        character(len=*), intent(in) :: proc           !! calling procedure, for every message.
        integer, allocatable, intent(out) :: slots(:)  !! one slot index per name, in order.
        logical, intent(in) :: writing                 !! .true. when values are about to be written.
        character(len=:), allocatable :: name
        integer :: k
        !
        ! Storage changes (a dropped bitmap, a rebuilt string store, a deleted row) are refused on
        ! a table another thread may be using, exactly as %compact_validity and every row-
        ! structural verb refuse them.
        call table_check_not_shared(self, proc)
        call table_check_open(self, proc)
        allocate(slots(size(names, kind=int64)))
        do k = 1, size(names)
            name = trim(names(k))
            call table_resolve(self, name, proc, slots(k), writing=writing)
            associate (slot => self%cache%cols(slots(k)))
                if (.not. slot%supported) then
                    ! GCOVR_EXCL_START -- a gcov ARTIFACT, not an untested line. The call below
                    ! IS reached: test/error_scenarios.f90's `fillna_unsupported_column` runs
                    ! `%fillna` over the map fixture's `m_intkey`, and the message it prints --
                    ! "fillna: this column's type is not supported by parquet_table" -- is raised
                    ! by table_unsupported_column_abort and by nothing else, with `proc` spelled
                    ! by THIS call site alone. gcov still reports the line unexecuted, because the
                    ! callee ends in `error stop` and the block never completes.
                    call table_unsupported_column_abort(self%cache, slots(k), trim(proc))
                    ! GCOVR_EXCL_STOP
                end if
            end associate
        end do
    end subroutine fill_prepare
    !
    !> Names a `%fillna` value class in words, for the message a rejected column raises.
    subroutine fill_value_name(vclass, text)
        integer, intent(in) :: vclass                       !! FV_*.
        character(len=:), allocatable, intent(out) :: text  !! the word for it.
        !
        select case (vclass)
        case (FV_INT);  text = "an integer"
        case (FV_REAL); text = "a real"
        case (FV_BOOL); text = "a logical"
        case (FV_STR);  text = "a character"
        case (FV_DATE); text = "a parquet_date"
        case (FV_TIME); text = "a parquet_time"
        case (FV_TS);   text = "a parquet_timestamp"
        case default
            ! Unreachable: every one of the eighteen specifics sets vclass before this can be
            ! reached, and `fill_value`'s default of 0 is never passed on.
            text = "an unknown" ! GCOVR_EXCL_LINE
        end select
    end subroutine fill_value_name
    !
    !> Whether `%fillna`'s value can be written into a column of this kind without changing it.
    !>
    !> **The one place the rule lives**, so that the eighteen entry points cannot come to disagree
    !> about it and so that the writer below needs no second opinion. An integer widens into a
    !> wider integer or into either real kind; a real fills only a real column; and the logical,
    !> character and three temporal classes each fill their own family and nothing else. A vector
    !> column is accepted wherever its scalar twin is, because the value fills every null ELEMENT.
    !>
    !> Refusing a real value for an integer column is deliberate and is not `%cast`'s rule:
    !> `%cast` is told a target kind and rounds towards it on purpose, whereas a real sentinel
    !> reaching an integer column in a mixed name list is a mistake worth naming. The reverse --
    !> a real value for a `real32` column -- IS accepted and may round, exactly as assigning the
    !> same literal to a `real32` variable would.
    pure logical function fill_value_accepts(kind, vclass) result(ok)
        integer, intent(in) :: kind    !! the column's PK_* kind.
        integer, intent(in) :: vclass  !! FV_*.
        !
        select case (vclass)
        case (FV_INT)
            ok = kind == PK_INT32 .or. kind == PK_INT64 .or. kind == PK_FLOAT32 .or. &
                 kind == PK_FLOAT64 .or. kind == PK_INT32_VEC .or. kind == PK_INT64_VEC .or. &
                 kind == PK_FLOAT32_VEC .or. kind == PK_FLOAT64_VEC
        case (FV_REAL)
            ok = kind == PK_FLOAT32 .or. kind == PK_FLOAT64 .or. kind == PK_FLOAT32_VEC .or. &
                 kind == PK_FLOAT64_VEC
        case (FV_BOOL)
            ok = kind == PK_LOGICAL .or. kind == PK_LOGICAL_VEC
        case (FV_STR)
            ok = kind == PK_STRING .or. kind == PK_STRING_VEC
        case (FV_DATE)
            ok = kind == PK_DATE .or. kind == PK_DATE_VEC
        case (FV_TIME)
            ok = kind == PK_TIME .or. kind == PK_TIME_VEC
        case (FV_TS)
            ok = kind == PK_TIMESTAMP .or. kind == PK_TIMESTAMP_VEC
        case default
            ok = .false. ! GCOVR_EXCL_LINE
        end select
    end function fill_value_accepts
    !
    !> Aborts unless `fv` can fill this column, naming the column and both types.
    subroutine fill_value_check(cache, idx, name, fv)
        type(parquet_table_cache), intent(in) :: cache !! the store, for the message context.
        integer, intent(in) :: idx                     !! slot index.
        character(len=*), intent(in) :: name           !! column name, for the message.
        type(fill_value), intent(in) :: fv             !! the value the caller supplied.
        character(len=:), allocatable :: sfx, kind_txt, val_txt
        character(len=32) :: num
        !
        call table_context_suffix(cache, name, sfx)
        call fill_value_name(fv%vclass, val_txt)
        call parquet_kind_name(cache%cols(idx)%declared_kind, kind_txt)
        if (parquet_kind_is_container(cache%cols(idx)%declared_kind)) then
            error stop EP // "fillna: a " // kind_txt // " column cannot be filled with a " // &
                "scalar value -- there is no meaning to replacing a missing list with one" // sfx
        end if
        if (.not. fill_value_accepts(cache%cols(idx)%declared_kind, fv%vclass)) then
            error stop EP // "fillna: " // val_txt // " value cannot fill a " // kind_txt // &
                " column" // sfx
        end if
        ! The one check that is about the VALUE rather than the families: an int64 fill value that
        ! does not fit an int32 column would wrap silently, which is the class of quiet wrong
        ! answer this library refuses to produce. Any value that arrived as an int32 passes it.
        if (fv%vclass == FV_INT .and. (cache%cols(idx)%declared_kind == PK_INT32 .or. &
                                       cache%cols(idx)%declared_kind == PK_INT32_VEC)) then
            ! Both bounds are formed in int64, so nothing here can overflow while checking for
            ! an overflow -- the shape CLAUDE.md records as a conformance defect in its own right.
            if (fv%ival < -int(huge(1_int32), int64) - 1_int64 .or. &
                fv%ival > int(huge(1_int32), int64)) then
                write (num, "(I0)") fv%ival
                error stop EP // "fillna: " // trim(num) // " does not fit an int32 column" // sfx
            end if
        end if
    end subroutine fill_value_check
    !
    !> `%fillna` over a resolved name list: check every column, then fill every column.
    !>
    !> The two passes are the point. A list with one incompatible column fills NOTHING, so a
    !> caller that gets the abort has a table in the state they left it rather than one that was
    !> half filled at whichever name came first.
    subroutine fillna_apply(self, names, fv)
        class(parquet_table), intent(inout) :: self !! the table.
        character(len=*), intent(in) :: names(:)    !! the columns to fill.
        type(fill_value), intent(in) :: fv          !! the value every null takes.
        integer, allocatable :: slots(:)
        integer :: k
        logical :: changed
        !
        call fill_prepare(self, names, "fillna", slots, writing=.true.)
        do k = 1, size(slots)
            call fill_value_check(self%cache, slots(k), trim(names(k)), fv)
        end do
        do k = 1, size(slots)
            call fillna_column(self%cache%cols(slots(k))%values, fv, changed)
            ! Only a column that was actually written is claimed as holding the caller's values.
            ! Marking one that had no nulls would make a fill that changed nothing start refusing
            ! %evict_column and %reload, which is a change to the table by a call documented as
            ! making none.
            if (changed) self%cache%cols(slots(k))%user_populated = .true.
        end do
    end subroutine fillna_apply
    !
    !> Writes `fv` into every null of one column and clears the null with it.
    !>
    !> `col` is a TARGET because `parquet_column_data_ptr`'s own dummy is one: F2018 15.5.2.4
    !> leaves a pointer returned through a non-target actual UNDEFINED on return, which is the
    !> class nagfor's `-C=dangling` exists to catch.
    subroutine fillna_column(col, fv, changed)
        type(parquet_column), intent(inout), target :: col !! the column to fill.
        type(fill_value), intent(in) :: fv                 !! the value every null takes.
        logical, intent(out) :: changed                    !! .false. if the column had no nulls.
        logical, allocatable :: valid(:,:)
        integer(int32), pointer :: p_i32(:), p_i32v(:,:)
        integer(int64), pointer :: p_i64(:), p_i64v(:,:)
        real(real32), pointer :: p_f32(:), p_f32v(:,:)
        real(real64), pointer :: p_f64(:), p_f64v(:,:)
        logical, pointer :: p_b(:), p_bv(:,:)
        type(parquet_date), pointer :: p_dt(:), p_dtv(:,:)
        type(parquet_time), pointer :: p_tm(:), p_tmv(:,:)
        type(parquet_timestamp), pointer :: p_ts(:), p_tsv(:,:)
        type(parquet_string_column), pointer :: sc
        real(real64) :: x
        integer(int64) :: i, e, n, w
        !
        ! Unallocated means the column holds no null at all, and then a fill has nothing to do:
        ! not one value is written, not one bit is touched, and an outstanding %col pointer sees
        ! exactly what it saw before. This is the "changes nothing" rule, applied per column, and
        ! it is also the fast path for the common case.
        changed = .false.
        call col%element_validity(valid)
        if (.not. allocated(valid)) return
        changed = .true.
        n = col%length()
        w = int(col%colwidth(), int64)
        x = fv%rval
        if (fv%vclass == FV_INT) x = real(fv%ival, real64)
        select case (col%kindof())
        case (PK_INT32)
            call parquet_column_data_ptr(col, p_i32)
            where (.not. valid(1, 1:n)) p_i32(1:n) = int(fv%ival, int32)
        case (PK_INT64)
            call parquet_column_data_ptr(col, p_i64)
            where (.not. valid(1, 1:n)) p_i64(1:n) = fv%ival
        case (PK_FLOAT32)
            call parquet_column_data_ptr(col, p_f32)
            where (.not. valid(1, 1:n)) p_f32(1:n) = real(x, real32)
        case (PK_FLOAT64)
            call parquet_column_data_ptr(col, p_f64)
            where (.not. valid(1, 1:n)) p_f64(1:n) = x
        case (PK_LOGICAL)
            call parquet_column_data_ptr(col, p_b)
            where (.not. valid(1, 1:n)) p_b(1:n) = fv%lval
        case (PK_INT32_VEC)
            call parquet_column_data_ptr(col, p_i32v)
            where (.not. valid(1:w, 1:n)) p_i32v(1:w, 1:n) = int(fv%ival, int32)
        case (PK_INT64_VEC)
            call parquet_column_data_ptr(col, p_i64v)
            where (.not. valid(1:w, 1:n)) p_i64v(1:w, 1:n) = fv%ival
        case (PK_FLOAT32_VEC)
            call parquet_column_data_ptr(col, p_f32v)
            where (.not. valid(1:w, 1:n)) p_f32v(1:w, 1:n) = real(x, real32)
        case (PK_FLOAT64_VEC)
            call parquet_column_data_ptr(col, p_f64v)
            where (.not. valid(1:w, 1:n)) p_f64v(1:w, 1:n) = x
        case (PK_LOGICAL_VEC)
            call parquet_column_data_ptr(col, p_bv)
            where (.not. valid(1:w, 1:n)) p_bv(1:w, 1:n) = fv%lval
        case (PK_STRING, PK_STRING_VEC)
            ! Storage class 2, written through the store's own ONE-PASS `set_where`, which marks
            ! each written element valid as it goes -- so this needs no companion pass, and
            ! clear_nulls_where below must NOT run for it. **A `%set` per null element is the
            ! quadratic form and must not come back**: every `set` that changes an element's
            ! length shifts the whole payload tail, so a column with millions of nulls is a hang
            ! in practice. No unit test can assert a complexity class;
            ! `bench/benchmark_join.sh --mode=nullfill` times this verb on a 16-byte string
            ! column beside the same verb on a float64 one. The store is flat in element order,
            ! (i-1)*w+e, which is `valid(w, n)`'s own column-major order, so the mask is handed
            ! over as it is. The store rebuilds its own buffers behind the same object, which is
            ! why nothing a caller can hold on to is invalidated.
            call parquet_column_string_column(col, sc)
            call parquet_string_column_set_where(sc, reshape(.not. valid, [w*n]), fv%sval)
            return
        case (PK_DATE)
            ! Storage class 3. The null is INSIDE the element, so writing a value is what clears
            ! it -- %clear_null refuses a temporal kind outright. What the write does not update
            ! is the column's cached "does this hold a null" answer, which compact_validity
            ! rescans for exactly this reason.
            call parquet_column_data_ptr(col, p_dt)
            do i = 1_int64, n
                if (.not. valid(1, i)) p_dt(i) = fv%dval
            end do
            call col%compact_validity()
            return
        case (PK_TIME)
            call parquet_column_data_ptr(col, p_tm)
            do i = 1_int64, n
                if (.not. valid(1, i)) p_tm(i) = fv%tval
            end do
            call col%compact_validity()
            return
        case (PK_TIMESTAMP)
            call parquet_column_data_ptr(col, p_ts)
            do i = 1_int64, n
                if (.not. valid(1, i)) p_ts(i) = fv%tsval
            end do
            call col%compact_validity()
            return
        case (PK_DATE_VEC)
            call parquet_column_data_ptr(col, p_dtv)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (.not. valid(e, i)) p_dtv(e, i) = fv%dval
                end do
            end do
            call col%compact_validity()
            return
        case (PK_TIME_VEC)
            call parquet_column_data_ptr(col, p_tmv)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (.not. valid(e, i)) p_tmv(e, i) = fv%tval
                end do
            end do
            call col%compact_validity()
            return
        case (PK_TIMESTAMP_VEC)
            call parquet_column_data_ptr(col, p_tsv)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (.not. valid(e, i)) p_tsv(e, i) = fv%tsval
                end do
            end do
            call col%compact_validity()
            return
        case default
            ! Unreachable: fill_value_check has already refused every kind that is not one of the
            ! arms above -- the containers by name, and PK_NONE because an unsupported column is
            ! refused by fill_prepare before this is called.
            error stop EP // "fillna: this column's kind cannot be filled" ! GCOVR_EXCL_LINE
        end select
        ! Storage class 1, and the half that a fill written by hand always misses: the values are
        ! in place but every one of those rows is still FLAGGED null, so it reads back as the
        ! sentinel AND as Null and is written to a file as Null.
        valid = .not. valid
        call clear_nulls_where(col, valid)
        call col%compact_validity()
    end subroutine fillna_column
    !
    !> Clears the null of every element `flag` marks, on a bitmap-backed column.
    !>
    !> Never called for a string or temporal column: `%clear_null` refuses a temporal kind, and on
    !> a string column it would write `""` over the value just written.
    subroutine clear_nulls_where(col, flag)
        type(parquet_column), intent(inout) :: col !! the column.
        logical, intent(in) :: flag(:,:)           !! (width, nrows); .true. = clear this element.
        integer(int64) :: i, e
        !
        do i = 1_int64, size(flag, 2, kind=int64)
            do e = 1_int64, size(flag, 1, kind=int64)
                if (flag(e, i)) call parquet_column_clear_null(col, i, e)
            end do
        end do
    end subroutine clear_nulls_where
    !
    ! ---- %ffill / %bfill ----------------------------------------------------------------------
    !
    !> `%ffill`/`%bfill` over a resolved name list.
    subroutine scan_fill_apply(self, names, forward, proc, limit)
        class(parquet_table), intent(inout) :: self !! the table.
        character(len=*), intent(in) :: names(:)    !! the columns to fill.
        logical, intent(in) :: forward              !! .true. for %ffill, .false. for %bfill.
        character(len=*), intent(in) :: proc        !! "ffill" or "bfill", for every message.
        !> consecutive-null cap. ABSENT is how "no cap" is expressed all the way down to here,
        !! deliberately: the scan's own sentinel for it is -1, and a caller who wrote `limit=-1`
        !! must get the refusal below rather than silently the opposite of what they asked for.
        integer(int64), intent(in), optional :: limit
        character(len=:), allocatable :: sfx, kind_txt
        integer, allocatable :: slots(:)
        integer(int64) :: cap
        integer :: k
        logical :: changed
        !
        cap = -1_int64
        if (present(limit)) then
            if (limit < 1_int64) then
                error stop EP // trim(proc) // ": limit must be at least 1 (leave it out for no limit)"
            end if
            cap = limit
        end if
        call fill_prepare(self, names, proc, slots, writing=.true.)
        do k = 1, size(slots)
            if (.not. parquet_kind_is_container(self%cache%cols(slots(k))%declared_kind)) cycle
            call table_context_suffix(self%cache, trim(names(k)), sfx)
            call parquet_kind_name(self%cache%cols(slots(k))%declared_kind, kind_txt)
            error stop EP // trim(proc) // ": a " // kind_txt // " column cannot be filled from " // &
                "its neighbouring rows" // sfx
        end do
        do k = 1, size(slots)
            call scan_fill_column(self%cache%cols(slots(k))%values, forward, cap, changed)
            if (changed) self%cache%cols(slots(k))%user_populated = .true.
        end do
    end subroutine scan_fill_apply
    !
    !> Carries a neighbouring row's value into one column's nulls.
    !>
    !> Two phases, and the split is what keeps eighteen kinds from carrying eighteen copies of the
    !> scan. Phase 1 decides, from the validity mask alone, which row each null takes its value
    !> from -- that answer depends on the direction and the limit and on nothing about the values,
    !> so it is written once. Phase 2 copies one element per filled position, which is the only
    !> part that has to know the kind.
    subroutine scan_fill_column(col, forward, limit, changed)
        type(parquet_column), intent(inout), target :: col !! the column to fill.
        logical, intent(in) :: forward                     !! .true. for %ffill, .false. for %bfill.
        integer(int64), intent(in) :: limit                !! consecutive-null cap, or -1 for none.
        !> .false. if the column had no nulls, or had no value to carry into the ones it had.
        logical, intent(out) :: changed
        logical, allocatable :: valid(:,:)
        integer(int64), allocatable :: src(:,:)
        integer(int32), pointer :: p_i32(:), p_i32v(:,:)
        integer(int64), pointer :: p_i64(:), p_i64v(:,:)
        real(real32), pointer :: p_f32(:), p_f32v(:,:)
        real(real64), pointer :: p_f64(:), p_f64v(:,:)
        logical, pointer :: p_b(:), p_bv(:,:)
        type(parquet_date), pointer :: p_dt(:), p_dtv(:,:)
        type(parquet_time), pointer :: p_tm(:), p_tmv(:,:)
        type(parquet_timestamp), pointer :: p_ts(:), p_tsv(:,:)
        type(parquet_string_column), pointer :: sc
        integer(int64), allocatable :: idx(:)
        integer(int64) :: i, e, n, w, k, last, run, step, first
        !
        changed = .false.
        call col%element_validity(valid)
        if (.not. allocated(valid)) return
        n = col%length()
        w = int(col%colwidth(), int64)
        allocate(src(w, n))
        src = 0_int64
        ! Phase 1: the source row for every null that is to be filled, 0 for one that stays null.
        ! `run` counts how far into the current run of nulls this element is, so `limit` caps the
        ! RUN rather than the distance from the last value -- which is the same thing here and is
        ! how pandas defines it.
        if (forward) then
            first = 1_int64
            step = 1_int64
        else
            first = n
            step = -1_int64
        end if
        do e = 1_int64, w
            last = 0_int64
            run = 0_int64
            i = first
            do while (i >= 1_int64 .and. i <= n)
                if (valid(e, i)) then
                    last = i
                    run = 0_int64
                else if (last /= 0_int64) then
                    run = run + 1_int64
                    if (limit < 0_int64 .or. run <= limit) src(e, i) = last
                end if
                i = i + step
            end do
        end do
        ! An all-null column, or one whose only nulls lead (under %ffill) or trail (under
        ! %bfill), has nothing to carry and is left exactly as it was.
        if (.not. any(src /= 0_int64)) return
        changed = .true.
        ! Phase 2: one element copy per filled position.
        select case (col%kindof())
        case (PK_INT32)
            call parquet_column_data_ptr(col, p_i32)
            do i = 1_int64, n
                if (src(1, i) /= 0_int64) p_i32(i) = p_i32(src(1, i))
            end do
        case (PK_INT64)
            call parquet_column_data_ptr(col, p_i64)
            do i = 1_int64, n
                if (src(1, i) /= 0_int64) p_i64(i) = p_i64(src(1, i))
            end do
        case (PK_FLOAT32)
            call parquet_column_data_ptr(col, p_f32)
            do i = 1_int64, n
                if (src(1, i) /= 0_int64) p_f32(i) = p_f32(src(1, i))
            end do
        case (PK_FLOAT64)
            call parquet_column_data_ptr(col, p_f64)
            do i = 1_int64, n
                if (src(1, i) /= 0_int64) p_f64(i) = p_f64(src(1, i))
            end do
        case (PK_LOGICAL)
            call parquet_column_data_ptr(col, p_b)
            do i = 1_int64, n
                if (src(1, i) /= 0_int64) p_b(i) = p_b(src(1, i))
            end do
        case (PK_INT32_VEC)
            call parquet_column_data_ptr(col, p_i32v)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (src(e, i) /= 0_int64) p_i32v(e, i) = p_i32v(e, src(e, i))
                end do
            end do
        case (PK_INT64_VEC)
            call parquet_column_data_ptr(col, p_i64v)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (src(e, i) /= 0_int64) p_i64v(e, i) = p_i64v(e, src(e, i))
                end do
            end do
        case (PK_FLOAT32_VEC)
            call parquet_column_data_ptr(col, p_f32v)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (src(e, i) /= 0_int64) p_f32v(e, i) = p_f32v(e, src(e, i))
                end do
            end do
        case (PK_FLOAT64_VEC)
            call parquet_column_data_ptr(col, p_f64v)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (src(e, i) /= 0_int64) p_f64v(e, i) = p_f64v(e, src(e, i))
                end do
            end do
        case (PK_LOGICAL_VEC)
            call parquet_column_data_ptr(col, p_bv)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (src(e, i) /= 0_int64) p_bv(e, i) = p_bv(e, src(e, i))
                end do
            end do
        case (PK_STRING, PK_STRING_VEC)
            ! Storage class 2, as ONE gather of the store over its own elements: flat element
            ! k = (i-1)*w+e keeps itself where `src` is 0 and takes the source row's SAME element,
            ! (src-1)*w+e, otherwise. `gather` rebuilds the payload once, may name an element
            ! more than once, carries each kept element's own null across and recounts -- so a
            ! position past `limit` (which the scan left at 0) stays null with nothing further to
            ! do, and the storage-class-1 clearing pass below must NOT run for it. **A get-then-
            ! set per filled element is the quadratic form and must not come back**: every `set`
            ! that changes an element's length shifts the whole payload tail.
            ! `bench/benchmark_join.sh --mode=nullfill` times this verb beside a float64 column.
            call parquet_column_string_column(col, sc)
            allocate(idx(w*n))
            do i = 1_int64, n
                do e = 1_int64, w
                    k = (i - 1_int64)*w + e
                    if (src(e, i) == 0_int64) then
                        idx(k) = k
                    else
                        idx(k) = (src(e, i) - 1_int64)*w + e
                    end if
                end do
            end do
            call parquet_string_column_gather(sc, idx)
            return
        case (PK_DATE)
            call parquet_column_data_ptr(col, p_dt)
            do i = 1_int64, n
                if (src(1, i) /= 0_int64) p_dt(i) = p_dt(src(1, i))
            end do
            call col%compact_validity()
            return
        case (PK_TIME)
            call parquet_column_data_ptr(col, p_tm)
            do i = 1_int64, n
                if (src(1, i) /= 0_int64) p_tm(i) = p_tm(src(1, i))
            end do
            call col%compact_validity()
            return
        case (PK_TIMESTAMP)
            call parquet_column_data_ptr(col, p_ts)
            do i = 1_int64, n
                if (src(1, i) /= 0_int64) p_ts(i) = p_ts(src(1, i))
            end do
            call col%compact_validity()
            return
        case (PK_DATE_VEC)
            call parquet_column_data_ptr(col, p_dtv)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (src(e, i) /= 0_int64) p_dtv(e, i) = p_dtv(e, src(e, i))
                end do
            end do
            call col%compact_validity()
            return
        case (PK_TIME_VEC)
            call parquet_column_data_ptr(col, p_tmv)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (src(e, i) /= 0_int64) p_tmv(e, i) = p_tmv(e, src(e, i))
                end do
            end do
            call col%compact_validity()
            return
        case (PK_TIMESTAMP_VEC)
            call parquet_column_data_ptr(col, p_tsv)
            do i = 1_int64, n
                do e = 1_int64, w
                    if (src(e, i) /= 0_int64) p_tsv(e, i) = p_tsv(e, src(e, i))
                end do
            end do
            call col%compact_validity()
            return
        case default
            ! Unreachable: scan_fill_apply refuses the container kinds by name and fill_prepare
            ! refuses an unsupported column, which leaves only the arms above.
            error stop EP // "ffill: this column's kind cannot be filled" ! GCOVR_EXCL_LINE
        end select
        ! Storage class 1 again, and only for the positions actually filled -- a leading run under
        ! %ffill, or anything past `limit`, keeps its null.
        valid = src /= 0_int64
        call clear_nulls_where(col, valid)
        call col%compact_validity()
    end subroutine scan_fill_column
    !
    ! ---- %dropna ------------------------------------------------------------------------------
    !
    !> Counts each row's non-null named columns and drops the rows that fall short.
    subroutine dropna_apply(self, slots, min_valid, how)
        class(parquet_table), intent(inout) :: self       !! the table.
        integer, intent(in) :: slots(:)                   !! the columns whose nulls drop a row.
        integer, intent(in), optional :: min_valid        !! keep a row with this many non-null columns.
        character(len=*), intent(in), optional :: how     !! "any" (default) or "all".
        logical, allocatable :: rowvalid(:), keep(:)
        integer, allocatable :: nvalid(:)
        character(len=:), allocatable :: sfx
        character(len=32) :: num
        integer :: k, need, policy
        !
        if (present(how) .and. present(min_valid)) then
            error stop EP // "dropna: pass how= or min_valid=, not both -- min_valid replaces " // &
                "how rather than refining it"
        end if
        ! Both arguments are validated BEFORE the empty-column-set return below, so that a typo in
        ! `how` is refused whether or not anything happens to be resident. A guard that fires only
        ! when there is work to do is a guard that reports a mistake on some tables and not on
        ! others, which is worse than not having it.
        policy = POLICY_ANY
        if (present(how)) then
            select case (trim(how))
            case ("any")
                policy = POLICY_ANY
            case ("all")
                policy = POLICY_ALL
            case default
                call table_context_suffix(self%cache, "", sfx)
                error stop EP // "dropna: how must be ""any"" or ""all"", not """ // &
                    trim(how(1:min(len_trim(how), 32))) // """" // sfx
            end select
        end if
        if (present(min_valid)) then
            policy = POLICY_THRESH
            if (min_valid < 0) then
                write (num, "(I0)") min_valid
                call table_context_suffix(self%cache, "", sfx)
                error stop EP // "dropna: min_valid must not be negative, got " // trim(num) // sfx
            end if
        end if
        ! No columns to look at means no row can be shown to be missing anything, so nothing is
        ! dropped. This is the answer for `t%dropna()` on a table nothing has read yet, and it is
        ! also what stops `min_valid=1` from emptying such a table: with no columns, no row could
        ! ever reach a positive threshold.
        if (size(slots) == 0) return
        select case (policy)
        case (POLICY_ANY)
            need = size(slots)
        case (POLICY_ALL)
            need = 1
        case default
            ! A threshold above the number of columns named can never be met, so it would empty
            ! the table -- a mistake rather than a request, and refused naming both numbers.
            if (min_valid > size(slots)) then
                write (num, "(I0,A,I0)") min_valid, " of ", size(slots)
                call table_context_suffix(self%cache, "", sfx)
                error stop EP // "dropna: min_valid is out of range (" // trim(num) // &
                    " columns named)" // sfx
            end if
            need = min_valid
        end select
        allocate(nvalid(self%row_count), source=0)
        do k = 1, size(slots)
            ! Unallocated means the column has no nulls at all, so every row scores it.
            call self%cache%cols(slots(k))%values%row_validity(rowvalid)
            if (allocated(rowvalid)) then
                where (rowvalid) nvalid = nvalid + 1
            else
                nvalid = nvalid + 1
            end if
        end do
        allocate(keep(self%row_count))
        keep = nvalid >= need
        call table_apply_keep(self, keep, "dropna")
    end subroutine dropna_apply
end submodule parquet_tables_fill ! GCOVR_EXCL_LINE
