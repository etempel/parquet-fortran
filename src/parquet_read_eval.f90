!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> In-memory evaluation of a parsed filter expression: the twin of the C++ evaluator, run against
!> `parquet_column` storage a caller already holds rather than against a file.
!>
!> This is what `parquet_table`'s `%row_mask` and `%filter_rows(expr)`/`%filter_rows(filter)` are
!> built on. The reader's engine and this one are two implementations, deliberately -- decision 6
!> of feature_pandas_S7.md accepted a second evaluator so that one grammar serves both -- but they
!> are two implementations of ONE specification, and everything that could make them disagree is
!> shared rather than reimplemented:
!>
!>  * the PARSER is the same. `parquet_parse_filter_rules` produces the postfix node list and the
!>    packed leaves for both, so precedence, the AND-fold between `%add` calls, the keyword set and
!>    every syntax error message have one source.
!>  * the KLEENE vocabulary is the same. `KL_FALSE`/`KL_TRUE`/`KL_UNKNOWN` are the numbers the C++
!>    side uses (`kFalse`/`kTrue`/`kUnknown`) and `ND_*` the node kinds it switches on; both are
!>    host-associated from parquet_read.f90 rather than respelled here, because two spellings of a
!>    three-state value would be a silent wrong answer the first time either side changed.
!>  * the SET leaf is the same. `parquet_resolve_set_payload` turns `@name` or `(1, 2, 3)` into the
!>    element family, the `pf_index_map` and the string column for BOTH engines, and
!>    `parquet_set_verdict` applies the null rule and the `not_in` negation for both -- so a NaN
!>    matches nothing here for the same reason it does there, by no NaN pattern ever being a key.
!>  * the LITERAL parsers are the same. `parquet_parse_set_int`/`parquet_parse_set_real` reproduce
!>    `strtoll`/`strtod` (plus the NaN refusal), and are used here for the bare literal in `x == 1`
!>    exactly as they are for a list's elements.
!>  * the TEMPORAL rule is the same. `temporal_literal_to_raw` converts an ISO-8601 literal at the
!>    column's own stored unit, so a literal finer than that unit is refused on this engine for the
!>    same reason and with the same wording as on the reader's.
!>
!> What remains genuinely separate is the per-row comparison itself, and the two places it is easy
!> to get subtly wrong are called out at their own sites: a STRING compares byte-lexicographically
!> rather than by Fortran's blank-padding rules, and a NaN is an ordinary VALUE (false under every
!> ordering comparison and under `==`, true under `/=`) where a NULL is unknown.
!>
!> Nothing here aborts. Every failure is reported through `ok`/`errmsg` so the caller can attach
!> its own context -- the table appends "(file 'x.parquet', column 'y')" exactly as the reader
!> appends its file name.
!>
!> A LEAF submodule on purpose: it references `PK_*`, which parquet_core use-associates from
!> parquet_columns, and CLAUDE.md records that an INTERMEDIATE submodule referencing such a name
!> makes nagfor 7.2 fail while compiling an unrelated sibling. Putting the code in a leaf is the
!> fix that note prefers.
submodule (parquet_core:parquet_read) parquet_read_eval
    ! Two DIRECT imports, where every other child of parquet_read takes what it needs by host
    ! association from parquet_core. Both are names parquet_core does not itself import, and a
    ! directly imported name is the form CLAUDE.md records as always safe under nagfor's
    ! intermediate-submodule trap -- so importing here rather than widening parquet_core's own
    ! `only:` list keeps that trap out of reach for the whole subtree.
    !
    ! The two string accessors are the TYPED generics rather than `sc%is_null(i)`/`sc%get(i, v)`
    ! on the pointer: a type(parquet_string_column) actual passed to a `class` passed-object dummy
    ! makes ifx build a runtime type descriptor in the caller's prologue on EVERY call, and this
    ! is a per-row path (CLAUDE.md's typed-accessor tier, feature_ifx.md).
    use parquet_strings, only: parquet_string_column_is_null, parquet_string_column_get
    use ieee_arithmetic, only: ieee_is_finite
    implicit none

    !> The six comparison operators, resolved ONCE per leaf rather than compared as text per row.
    !!
    !! The same move C++'s CmpOp enum makes, and for the same measured reason: a string comparison
    !! per row against up to five spellings is the dominant cost of an otherwise trivial `>`.
    integer, parameter :: CMP_NONE = 0
    integer, parameter :: CMP_EQ = 1
    integer, parameter :: CMP_NE = 2
    integer, parameter :: CMP_LT = 3
    integer, parameter :: CMP_LE = 4
    integer, parameter :: CMP_GT = 5
    integer, parameter :: CMP_GE = 6

contains

    !> Answers one parsed leaf against one resident column. See the interface in parquet_core.f90.
    module procedure parquet_eval_filter_leaf
        character(len=:), allocatable :: type_token, shape_token, low
        integer(int64) :: i
        integer(int64) :: ival, raw, ns_per_unit
        real(real64) :: rval
        logical :: parsed, bval, want, negate
        integer :: cmp, use_unit
        integer(int8) :: fam
        type(pf_index_map) :: keys

        ok = .false.
        errmsg = ""
        call parquet_lower_op(op, low)
        call parquet_filter_column_tokens(kind, width, type_token, shape_token)

        ! A non-scalar column is refused for the reason every filter column is: there is no single
        ! value per row to compare. Same wording as parquet_check_set_column_shape, which the
        ! reader's engine reaches through the schema instead.
        if (shape_token /= "scalar") then
            call parquet_check_set_column_shape(column, shape_token, errmsg)
            return
        end if
        if (type_token == "") then
            errmsg = "column '" // trim(column) // "' has a type that filtering does not support"
            return
        end if

        ! ---- The two nullness operators: the ONLY ones a Null row can answer true or false for --
        if (low == "is_null" .or. low == "is_not_null") then
            want = (low == "is_null")
            do i = 1_int64, nrows
                out(i) = parquet_kleene_of(parquet_column_is_null(col, i) .eqv. want)
            end do
            ok = .true.
            return
        end if

        ! ---- The four VALUE-CLASS operators, floating-point columns only --------------------------
        if (low == "is_nan" .or. low == "is_not_nan" .or. low == "is_finite" .or. low == "is_not_finite") then
            call parquet_eval_value_class_leaf(col, kind, nrows, column, low, out, ok, errmsg)
            return
        end if

        ! ---- The set-valued operators, over the filter's own bound set or literal list -----------
        if (low == "in" .or. low == "not_in") then
            call parquet_resolve_set_payload(filter, column, value, type_token, shape_token, fam, &
                keys, ok, errmsg)
            if (.not. ok) return
            negate = (low == "not_in")
            call parquet_eval_set_leaf_column(col, kind, nrows, fam, keys, negate, out)
            ok = .true.
            return
        end if

        ! ---- Everything else is a comparison, so resolve the operator once ------------------------
        call parquet_filter_cmp_of(low, cmp)
        if (cmp == 0) then
            errmsg = "unsupported operator '" // trim(op) // "' on column '" // trim(column) // "'"
            return
        end if

        select case (kind)
        case (PK_INT32, PK_INT64)
            if (is_string) then
                errmsg = "value '" // trim(value) // "' is not a valid integer for column '" // trim(column) // "'"
                return
            end if
            call parquet_parse_set_int(trim(value), ival, parsed)
            if (.not. parsed) then
                errmsg = "value '" // trim(value) // "' is not a valid integer for column '" // trim(column) // "'"
                return
            end if
            ! An int32 column refuses an out-of-range literal rather than answering .false. for
            ! every row, exactly as the C++ arm does -- a bound no value can reach is a mistake in
            ! the rule, not a filter that matches nothing.
            if (kind == PK_INT32) then
                if (ival < int(-huge(0_int32) - 1_int32, int64) .or. ival > int(huge(0_int32), int64)) then
                    errmsg = "value '" // trim(value) // "' is out of int32 range for column '" // &
                        trim(column) // "'"
                    return
                end if
            end if
            call parquet_eval_int_leaf(col, kind, nrows, ival, cmp, out)
        case (PK_FLOAT32, PK_FLOAT64)
            if (is_string) then
                errmsg = "value '" // trim(value) // "' is not a valid number for column '" // trim(column) // "'"
                return
            end if
            ! parquet_parse_set_real refuses `nan` along with everything it does not recognise, and
            ! the C++ arm refuses it separately after strtod accepts it -- so the two agree, and
            ! the message says the same thing about the same spelling.
            call parquet_parse_set_real(trim(value), rval, parsed)
            if (.not. parsed) then
                if (parquet_text_is_nan_literal(trim(value))) then
                    errmsg = "value '" // trim(value) // "' for column '" // trim(column) // &
                        "' is not a comparable number; use the 'is_nan'/'is_not_nan' operators instead"
                else
                    errmsg = "value '" // trim(value) // "' is not a valid number for column '" // &
                        trim(column) // "'"
                end if
                return
            end if
            call parquet_eval_real_leaf(col, kind, nrows, rval, cmp, out)
        case (PK_LOGICAL)
            if (cmp /= CMP_EQ .and. cmp /= CMP_NE) then
                errmsg = "ordering comparisons ('>', '>=', '<', '<=') are not supported for boolean " // &
                    "column '" // trim(column) // "'"
                return
            end if
            if (is_string) then
                errmsg = "value for boolean column '" // trim(column) // "' must be true or false (unquoted)"
                return
            end if
            call parquet_parse_bool_literal(trim(value), bval, parsed)
            if (.not. parsed) then
                errmsg = "value '" // trim(value) // "' is not true/false for boolean column '" // &
                    trim(column) // "'"
                return
            end if
            call parquet_eval_bool_leaf(col, nrows, bval, cmp == CMP_EQ, out)
        case (PK_STRING)
            if (.not. is_string) then
                errmsg = "value for string column '" // trim(column) // "' must be double-quoted"
                return
            end if
            call parquet_eval_string_leaf(col, nrows, value, cmp, out)
        case (PK_DATE, PK_TIME, PK_TIMESTAMP)
            ! The reader converts a temporal literal to the column's own stored unit BEFORE the
            ! comparison engine sees it (convert_temporal_filter_values), and this does the same
            ! conversion with the same helper -- so both engines refuse a literal finer than the
            ! column's unit, and both compare integers at that unit afterwards.
            if (.not. is_string) then
                errmsg = "value '" // trim(value) // "' for " // type_token // " column '" // &
                    trim(column) // "' must be a double-quoted ISO-8601 literal (e.g. " // &
                    '"2024-01-31", "12:30:00", "2024-01-31T12:30:00")'
                return
            end if
            use_unit = unit
            ! A column with no file behind it records no unit, and nanoseconds is the resolution a
            ! parquet_time/parquet_timestamp element actually holds -- so comparing there is exact
            ! and refuses nothing a stored column would have accepted.
            if (use_unit == 0) use_unit = parquet_unit_nanos
            call temporal_literal_to_raw(type_token, trim(value), use_unit, raw, parsed)
            if (.not. parsed) then
                errmsg = "value '" // trim(value) // "' is not a valid ISO-8601 " // type_token // &
                    " for column '" // trim(column) // "', or is more precise than that column's " // &
                    "stored unit can represent"
                return
            end if
            ns_per_unit = parquet_ns_per_sec/unit_scale_for(use_unit)
            call parquet_eval_temporal_leaf(col, kind, nrows, raw, ns_per_unit, use_unit, cmp, out)
        case default
            errmsg = "column '" // trim(column) // "' has a type that filtering does not support"
            return
        end select
        ok = .true.
    end procedure parquet_eval_filter_leaf

    !> Folds the per-leaf verdicts through the postfix node list. See the interface in parquet_core.f90.
    module procedure parquet_eval_filter_program
        integer(int8), allocatable :: stack(:, :)
        integer :: k, top, li, nd
        integer(int64) :: i

        ok = .false.
        errmsg = ""
        keep = .false.
        ! One slot per node is always enough: a postfix walk pushes at most once per node and every
        ! operator pops before it pushes.
        nd = max(nnodes, 1)
        allocate(stack(nrows, nd))
        top = 0
        do k = 1, nnodes
            select case (int(node_kind(k)))
            case (ND_LEAF)
                li = int(node_leaf(k))
                if (li < 1 .or. li > size(verdicts, 2)) then
                    errmsg = "malformed expression"
                    return
                end if
                top = top + 1
                stack(1:nrows, top) = verdicts(1:nrows, li)
            case (ND_NOT)
                do i = 1_int64, nrows
                    if (stack(i, top) == KL_FALSE) then
                        stack(i, top) = KL_TRUE
                    else if (stack(i, top) == KL_TRUE) then
                        stack(i, top) = KL_FALSE
                    end if
                end do
            case default
                ! ND_AND / ND_OR. Kleene, not boolean: an unknown row must survive as unknown so
                ! that a later NOT cannot resurrect it and an OR cannot let it through on the
                ! strength of its other operand being false.
                call parquet_kleene_combine(stack(:, top - 1), stack(:, top), nrows, &
                    int(node_kind(k)) == ND_AND)
                top = top - 1
            end select
        end do
        if (top /= 1) then
            errmsg = "malformed expression"
            return
        end if
        ! The single collapse, at the very end: an unknown row is dropped. This is what makes the
        ! whole expression agree with SQL's WHERE, and it happens exactly once -- collapsing inside
        ! a combinator instead would let `not (x > 3)` keep a Null row.
        do i = 1_int64, nrows
            keep(i) = (stack(i, 1) == KL_TRUE)
        end do
        ok = .true.
    end procedure parquet_eval_filter_program

    !> The `parquet_get_column_type`/`parquet_get_column_shape` tokens a resident column of PK kind
    !> `kind` would have been reported with, so that the checks and messages the reader's engine
    !> raises from the schema can be raised here from a column instead.
    !>
    !> The INVERSE of table_kind_from_type (parquet_tables_read.f90), which maps the same nine
    !> element tokens the other way. The two are separate because they run in different modules on
    !> different inputs, and the pairing is protected by its failure DIRECTION rather than by a
    !> check: a kind missing here yields an empty type token, which refuses the clause outright.
    !> A refusal is loud, so the in-memory-versus-reader A/B tests over every column type -- which
    !> cover all nine -- report it immediately; there is no way for a missing entry to become a
    !> wrong answer.
    module procedure parquet_filter_column_tokens
        shape_token = "scalar"
        select case (kind)
        case (PK_INT32)
            type_token = "int32"
        case (PK_INT64)
            type_token = "int64"
        case (PK_FLOAT32)
            type_token = "float32"
        case (PK_FLOAT64)
            type_token = "float64"
        case (PK_LOGICAL)
            type_token = "boolean"
        case (PK_STRING)
            type_token = "string"
        case (PK_DATE)
            type_token = "date"
        case (PK_TIME)
            type_token = "time"
        case (PK_TIMESTAMP)
            type_token = "timestamp"
        case (PK_LIST)
            type_token = ""
            shape_token = "list"
        case (PK_MAP)
            type_token = ""
            shape_token = "map"
        case (PK_STRUCT)
            type_token = ""
            shape_token = "struct"
        case default
            ! Every *_VEC kind, and PK_NONE. A vector column is named by its shape rather than its
            ! element type, because that is the half of the answer the refusal is about.
            type_token = ""
            shape_token = "vector"
        end select
        ! A width above 1 is a vector column whatever the discriminator says -- belt and braces, so
        ! that a scalar kind carrying a width could never be compared element-wise by accident.
        if (width /= 1) then
            type_token = ""
            shape_token = "vector"
        end if
    end procedure parquet_filter_column_tokens

    !> KL_TRUE / KL_FALSE from a plain comparison result -- the twin of C++'s kleene_of.
    pure integer(int8) function parquet_kleene_of(b) result(res)
        logical, intent(in) :: b !! the comparison's result for a non-null row.
        if (b) then
            res = KL_TRUE
        else
            res = KL_FALSE
        end if
    end function parquet_kleene_of

    !> Folds `rhs` into `lhs` under Kleene AND or OR -- the twin of C++'s kleene_combine.
    pure subroutine parquet_kleene_combine(lhs, rhs, nrows, is_and)
        integer(int8), intent(inout) :: lhs(:) !! left operand, overwritten with the result.
        integer(int8), intent(in) :: rhs(:) !! right operand.
        integer(int64), intent(in) :: nrows !! rows in use.
        logical, intent(in) :: is_and !! .true. for AND, .false. for OR.
        integer(int64) :: i

        if (is_and) then
            do i = 1_int64, nrows
                if (lhs(i) == KL_FALSE .or. rhs(i) == KL_FALSE) then
                    lhs(i) = KL_FALSE
                else if (lhs(i) == KL_UNKNOWN .or. rhs(i) == KL_UNKNOWN) then
                    lhs(i) = KL_UNKNOWN
                else
                    lhs(i) = KL_TRUE
                end if
            end do
        else
            do i = 1_int64, nrows
                if (lhs(i) == KL_TRUE .or. rhs(i) == KL_TRUE) then
                    lhs(i) = KL_TRUE
                else if (lhs(i) == KL_UNKNOWN .or. rhs(i) == KL_UNKNOWN) then
                    lhs(i) = KL_UNKNOWN
                else
                    lhs(i) = KL_FALSE
                end if
            end do
        end if
    end subroutine parquet_kleene_combine

    !> The CMP_* code for a lower-cased comparison operator, or CMP_NONE for anything else.
    pure subroutine parquet_filter_cmp_of(low, cmp)
        character(len=*), intent(in) :: low !! the operator, already trimmed and lower-cased.
        integer, intent(out) :: cmp !! the CMP_* code.
        select case (low)
        case ("==")
            cmp = CMP_EQ
        case ("/=", "!=")
            cmp = CMP_NE
        case ("<")
            cmp = CMP_LT
        case ("<=")
            cmp = CMP_LE
        case (">")
            cmp = CMP_GT
        case (">=")
            cmp = CMP_GE
        case default
            cmp = CMP_NONE
        end select
    end subroutine parquet_filter_cmp_of

    !> Whether `text` spells a NaN the way strtod would accept it, so that the message can say what
    !> to write instead. Only reached once parquet_parse_set_real has already refused the text.
    pure logical function parquet_text_is_nan_literal(text) result(res)
        character(len=*), intent(in) :: text !! the value as written.
        character(len=:), allocatable :: low
        integer :: k, first

        low = text
        do k = 1, len(low)
            if (low(k:k) >= "A" .and. low(k:k) <= "Z") low(k:k) = achar(iachar(low(k:k)) + 32)
        end do
        first = 1
        if (len(low) > 0) then
            if (low(1:1) == "+" .or. low(1:1) == "-") first = 2
        end if
        res = .false.
        if (len(low) >= first) res = (low(first:) == "nan")
    end function parquet_text_is_nan_literal

    !> Parses `true`/`false` case-insensitively, the way ascii_to_lower plus the two string
    !> comparisons in the C++ boolean arm do.
    pure subroutine parquet_parse_bool_literal(text, value, ok)
        character(len=*), intent(in) :: text !! the value as written, unquoted.
        logical, intent(out) :: value !! the parsed value; .false. when ok is .false.
        logical, intent(out) :: ok !! .true. if the text is true or false.
        character(len=:), allocatable :: low
        integer :: k

        low = text
        do k = 1, len(low)
            if (low(k:k) >= "A" .and. low(k:k) <= "Z") low(k:k) = achar(iachar(low(k:k)) + 32)
        end do
        value = .false.
        ok = .true.
        if (low == "true") then
            value = .true.
        else if (low /= "false") then
            ok = .false.
        end if
    end subroutine parquet_parse_bool_literal

    !> The four value-class operators over a floating-point column: is_nan, is_not_nan, is_finite
    !> and is_not_finite.
    !>
    !> Kleene-honest about nullness -- a Null row is unknown for all four, exactly as it is for a
    !> comparison -- so that nullness stays governed solely by is_null/is_not_null and none of these
    !> becomes a back door for a Null row into a result that never mentions nullness.
    subroutine parquet_eval_value_class_leaf(col, kind, nrows, column, low, out, ok, errmsg)
        type(parquet_column), intent(in), target :: col !! the resident column.
        integer, intent(in) :: kind !! its PK_* kind.
        integer(int64), intent(in) :: nrows !! rows to answer.
        character(len=*), intent(in) :: column !! the column's name, for the message.
        character(len=*), intent(in) :: low !! the operator, already lower-cased.
        integer(int8), intent(out) :: out(:) !! one KL_* verdict per row.
        logical, intent(out) :: ok !! .false. when the column is not floating-point.
        character(len=:), allocatable, intent(out) :: errmsg !! that refusal; "" when ok.
        real(real32), pointer :: p32(:)
        real(real64), pointer :: p64(:)
        logical :: nan_test, want, hit
        integer(int64) :: i
        real(real64) :: v

        ok = .false.
        errmsg = ""
        if (kind /= PK_FLOAT32 .and. kind /= PK_FLOAT64) then
            errmsg = "operator '" // trim(low) // "' is only supported for floating-point columns, " // &
                "and column '" // trim(column) // "' is not one"
            return
        end if
        nan_test = (low == "is_nan" .or. low == "is_not_nan")
        want = (low == "is_nan" .or. low == "is_finite")
        nullify(p32)
        nullify(p64)
        if (kind == PK_FLOAT32) then
            call parquet_column_data_ptr(col, p32)
        else
            call parquet_column_data_ptr(col, p64)
        end if
        do i = 1_int64, nrows
            if (parquet_column_is_null(col, i)) then
                out(i) = KL_UNKNOWN
                cycle
            end if
            if (kind == PK_FLOAT32) then
                v = real(p32(i), real64)
            else
                v = p64(i)
            end if
            if (nan_test) then
                hit = ieee_is_nan(v)
            else
                hit = ieee_is_finite(v)
            end if
            out(i) = parquet_kleene_of(hit .eqv. want)
        end do
        ok = .true.
    end subroutine parquet_eval_value_class_leaf

    !> An integer column against an int64 bound.
    subroutine parquet_eval_int_leaf(col, kind, nrows, bound, cmp, out)
        type(parquet_column), intent(in), target :: col !! the resident column.
        integer, intent(in) :: kind !! PK_INT32 or PK_INT64.
        integer(int64), intent(in) :: nrows !! rows to answer.
        integer(int64), intent(in) :: bound !! the parsed literal.
        integer, intent(in) :: cmp !! the CMP_* code.
        integer(int8), intent(out) :: out(:) !! one KL_* verdict per row.
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        integer(int64) :: i, v

        nullify(p32)
        nullify(p64)
        if (kind == PK_INT32) then
            call parquet_column_data_ptr(col, p32)
        else
            call parquet_column_data_ptr(col, p64)
        end if
        do i = 1_int64, nrows
            if (parquet_column_is_null(col, i)) then
                out(i) = KL_UNKNOWN
                cycle
            end if
            if (kind == PK_INT32) then
                v = int(p32(i), int64)
            else
                v = p64(i)
            end if
            out(i) = parquet_kleene_of(parquet_cmp_int(v, bound, cmp))
        end do
    end subroutine parquet_eval_int_leaf

    !> A floating-point column against a real64 bound.
    !>
    !> A NaN row needs no special case and gets none: IEEE makes every comparison against it false
    !> and `/=` true, which is exactly the filter's rule that a NaN is a VALUE rather than a missing
    !> one -- so it is excluded by `>`/`>=`/`<`/`<=`/`==` and SURVIVES `/=` and any negated
    !> comparison, where a Null does the opposite. Do not "fix" that asymmetry.
    subroutine parquet_eval_real_leaf(col, kind, nrows, bound, cmp, out)
        type(parquet_column), intent(in), target :: col !! the resident column.
        integer, intent(in) :: kind !! PK_FLOAT32 or PK_FLOAT64.
        integer(int64), intent(in) :: nrows !! rows to answer.
        real(real64), intent(in) :: bound !! the parsed literal.
        integer, intent(in) :: cmp !! the CMP_* code.
        integer(int8), intent(out) :: out(:) !! one KL_* verdict per row.
        real(real32), pointer :: p32(:)
        real(real64), pointer :: p64(:)
        integer(int64) :: i
        real(real64) :: v

        nullify(p32)
        nullify(p64)
        if (kind == PK_FLOAT32) then
            call parquet_column_data_ptr(col, p32)
        else
            call parquet_column_data_ptr(col, p64)
        end if
        do i = 1_int64, nrows
            if (parquet_column_is_null(col, i)) then
                out(i) = KL_UNKNOWN
                cycle
            end if
            if (kind == PK_FLOAT32) then
                ! Widened to real64 before the comparison, exactly as real_family_value_at does,
                ! so that `y > 0.1` selects the same rows whichever width the column is stored at.
                v = real(p32(i), real64)
            else
                v = p64(i)
            end if
            out(i) = parquet_kleene_of(parquet_cmp_real(v, bound, cmp))
        end do
    end subroutine parquet_eval_real_leaf

    !> A boolean column against true/false. Equality only; ordering was refused by the caller.
    subroutine parquet_eval_bool_leaf(col, nrows, bound, want_equal, out)
        type(parquet_column), intent(in), target :: col !! the resident column.
        integer(int64), intent(in) :: nrows !! rows to answer.
        logical, intent(in) :: bound !! the parsed literal.
        logical, intent(in) :: want_equal !! .true. for `==`, .false. for `/=`.
        integer(int8), intent(out) :: out(:) !! one KL_* verdict per row.
        logical, pointer :: p(:)
        integer(int64) :: i

        nullify(p)
        call parquet_column_data_ptr(col, p)
        do i = 1_int64, nrows
            if (parquet_column_is_null(col, i)) then
                out(i) = KL_UNKNOWN
            else
                out(i) = parquet_kleene_of((p(i) .eqv. bound) .eqv. want_equal)
            end if
        end do
    end subroutine parquet_eval_bool_leaf

    !> A string column against a literal, compared BYTE-lexicographically.
    !>
    !> Not with Fortran's own `<`/`==`, which blank-pad the shorter operand to the longer: they make
    !> `"ab"` and `"ab "` equal, where C++'s std::string_view -- and therefore the reader -- orders
    !> `"ab" < "ab "`. Comparing bytes and then lengths is what makes a trailing space in a value or
    !> in a rule mean the same thing on both engines.
    subroutine parquet_eval_string_leaf(col, nrows, bound, cmp, out)
        type(parquet_column), intent(in), target :: col !! the resident column.
        integer(int64), intent(in) :: nrows !! rows to answer.
        character(len=*), intent(in) :: bound !! the literal, quotes already stripped.
        integer, intent(in) :: cmp !! the CMP_* code.
        integer(int8), intent(out) :: out(:) !! one KL_* verdict per row.
        type(parquet_string_column), pointer :: sc
        character(len=:), allocatable :: v
        integer(int64) :: i

        nullify(sc)
        call parquet_column_string_column(col, sc)
        do i = 1_int64, nrows
            if (parquet_string_column_is_null(sc, i)) then
                out(i) = KL_UNKNOWN
                cycle
            end if
            call parquet_string_column_get(sc, i, v)
            out(i) = parquet_kleene_of(parquet_cmp_int(int(parquet_bytes_compare(v, bound), int64), &
                0_int64, cmp))
        end do
    end subroutine parquet_eval_string_leaf

    !> -1, 0 or +1 for `a` before, equal to or after `b` in byte-lexicographic order -- what
    !> std::string_view's own comparison gives, and what Fortran's blank-padding operators do not.
    !>
    !> `ichar` rather than `iachar`: the collating sequence of the default character kind is the
    !> byte value on every compiler this library supports, which is what memcmp compares, whereas
    !> `iachar` is defined only over ASCII and is processor-dependent above 127.
    pure integer function parquet_bytes_compare(a, b) result(res)
        character(len=*), intent(in) :: a !! left operand.
        character(len=*), intent(in) :: b !! right operand.
        integer :: k, n

        n = min(len(a), len(b))
        do k = 1, n
            if (a(k:k) /= b(k:k)) then
                if (ichar(a(k:k)) < ichar(b(k:k))) then
                    res = -1
                else
                    res = 1
                end if
                return
            end if
        end do
        if (len(a) < len(b)) then
            res = -1
        else if (len(a) > len(b)) then
            res = 1
        else
            res = 0
        end if
    end function parquet_bytes_compare

    !> A date/time/timestamp column against a literal already converted to `raw` at the column's
    !> own stored unit -- so the comparison is between two integers at one resolution, which is
    !> what the reader's engine does once convert_temporal_filter_values has rewritten the leaf.
    subroutine parquet_eval_temporal_leaf(col, kind, nrows, raw, ns_per_unit, unit, cmp, out)
        type(parquet_column), intent(in), target :: col !! the resident column.
        integer, intent(in) :: kind !! PK_DATE, PK_TIME or PK_TIMESTAMP.
        integer(int64), intent(in) :: nrows !! rows to answer.
        integer(int64), intent(in) :: raw !! the literal at the column's stored unit.
        integer(int64), intent(in) :: ns_per_unit !! nanoseconds in one of those units.
        integer, intent(in) :: unit !! the parquet_unit_* selector `raw` is expressed in.
        integer, intent(in) :: cmp !! the CMP_* code.
        integer(int8), intent(out) :: out(:) !! one KL_* verdict per row.
        type(parquet_date), pointer :: pd(:)
        type(parquet_time), pointer :: pt(:)
        type(parquet_timestamp), pointer :: pts(:)
        integer(int64) :: i, v, bound

        nullify(pd)
        nullify(pt)
        nullify(pts)
        bound = raw
        select case (kind)
        case (PK_DATE)
            call parquet_column_data_ptr(col, pd)
        case (PK_TIME)
            call parquet_column_data_ptr(col, pt)
            ! A time element holds nanoseconds since midnight whatever the file's unit, so the
            ! BOUND is scaled up rather than every element scaled down: one multiply per leaf, and
            ! no division that could round a row's value onto the bound.
            bound = raw*ns_per_unit
        case default
            call parquet_column_data_ptr(col, pts)
        end select
        do i = 1_int64, nrows
            if (parquet_column_is_null(col, i)) then
                out(i) = KL_UNKNOWN
                cycle
            end if
            select case (kind)
            case (PK_DATE)
                v = int(pd(i)%raw(), int64)
            case (PK_TIME)
                v = pt(i)%raw()
            case default
                ! The element came from a column stored at this unit, so to_unix is exact here; a
                ! table built in memory carries no unit and is compared at nanoseconds, which is
                ! the resolution the element actually holds.
                v = pts(i)%to_unix(unit)
            end select
            out(i) = parquet_kleene_of(parquet_cmp_int(v, bound, cmp))
        end do
    end subroutine parquet_eval_temporal_leaf

    !> `in`/`not_in` over a resident column, against the payload parquet_resolve_set_payload built.
    !>
    !> The read-side twin is parquet_answer_set_chunk, which does the identical thing one row group
    !> at a time; both end at parquet_set_verdict, so the null rule, the membership answer and the
    !> negation are decided in ONE place for both engines.
    subroutine parquet_eval_set_leaf_column(col, kind, nrows, fam, keys, negate, out)
        type(parquet_column), intent(in), target :: col !! the resident column.
        integer, intent(in) :: kind !! its PK_* kind.
        integer(int64), intent(in) :: nrows !! rows to answer.
        integer(int8), intent(in) :: fam !! the set's FSET_* element family.
        type(pf_index_map), intent(in) :: keys !! the built key index, string-keyed for a string set.
        logical, intent(in) :: negate !! .true. for `not_in`.
        integer(int8), intent(out) :: out(:) !! one KL_* verdict per row.
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        real(real32), pointer :: r32(:)
        real(real64), pointer :: r64(:)
        type(parquet_date), pointer :: pd(:)
        type(parquet_time), pointer :: pt(:)
        type(parquet_timestamp), pointer :: pts(:)
        type(parquet_string_column), pointer :: sc
        integer(int64), allocatable :: lookup(:), found(:), pairs(:, :)
        integer(int64) :: i

        nullify(p32)
        nullify(p64)
        nullify(r32)
        nullify(r64)
        nullify(pd)
        nullify(pt)
        nullify(pts)
        nullify(sc)
        if (fam == FSET_STRING) then
            ! The column's own string store, read in place by the map's string %get_many, which
            ! keys each element by its exact bytes and answers 0 for a null one -- the same call
            ! the read-side engine makes per row group, and every other family makes here.
            call parquet_column_string_column(col, sc)
            allocate(found(nrows))
            call keys%get_many(sc, found)
            do i = 1_int64, nrows
                out(i) = parquet_set_verdict(.not. parquet_string_column_is_null(sc, i), found(i) > 0_int64, &
                    negate)
            end do
            return
        end if
        ! One %get_many for the whole column rather than a lookup per row: the map's own bulk form,
        ! and the same call the read-side pre-evaluator makes once per row group. The temporal
        ! kinds key exactly as parquet_answer_set_chunk keys them -- through the same three
        ! helpers -- and a null element, whose key is 0, is masked before the lookup so that it
        ! can never meet a genuine 1970-01-01 or midnight in the set. A timestamp column looks up
        ! `(n, 2)` tuples in a composite map.
        allocate(found(nrows))
        select case (kind)
        case (PK_TIMESTAMP)
            call parquet_column_data_ptr(col, pts)
            allocate(pairs(nrows, 2))
            call parquet_timestamp_key(pts(1_int64:nrows), pairs(:, 1), pairs(:, 2))
            call keys%get_many(pairs, found, valid=.not. pts(1_int64:nrows)%is_null())
        case default
            allocate(lookup(nrows))
            select case (kind)
            case (PK_INT32)
                call parquet_column_data_ptr(col, p32)
                lookup = int(p32(1_int64:nrows), int64)
            case (PK_INT64)
                call parquet_column_data_ptr(col, p64)
                lookup = p64(1_int64:nrows)
            case (PK_FLOAT32)
                call parquet_column_data_ptr(col, r32)
                lookup = parquet_filter_real_key(real(r32(1_int64:nrows), real64))
            case (PK_DATE)
                call parquet_column_data_ptr(col, pd)
                lookup = parquet_date_key(pd(1_int64:nrows))
            case (PK_TIME)
                call parquet_column_data_ptr(col, pt)
                lookup = parquet_time_key(pt(1_int64:nrows))
            case default
                call parquet_column_data_ptr(col, r64)
                lookup = parquet_filter_real_key(r64(1_int64:nrows))
            end select
            if (kind == PK_DATE) then
                call keys%get_many(lookup, found, valid=.not. pd(1_int64:nrows)%is_null())
            else if (kind == PK_TIME) then
                call keys%get_many(lookup, found, valid=.not. pt(1_int64:nrows)%is_null())
            else
                call keys%get_many(lookup, found)
            end if
        end select
        do i = 1_int64, nrows
            out(i) = parquet_set_verdict(.not. parquet_column_is_null(col, i), found(i) > 0_int64, negate)
        end do
    end subroutine parquet_eval_set_leaf_column

    !> One integer comparison under a CMP_* code.
    pure logical function parquet_cmp_int(a, b, cmp) result(res)
        integer(int64), intent(in) :: a !! the row's value.
        integer(int64), intent(in) :: b !! the bound.
        integer, intent(in) :: cmp !! the CMP_* code.
        select case (cmp)
        case (CMP_EQ)
            res = (a == b)
        case (CMP_NE)
            res = (a /= b)
        case (CMP_LT)
            res = (a < b)
        case (CMP_LE)
            res = (a <= b)
        case (CMP_GT)
            res = (a > b)
        case default
            res = (a >= b)
        end select
    end function parquet_cmp_int

    !> One floating-point comparison under a CMP_* code. Every one of the six is false for a NaN
    !> operand except CMP_NE, which IEEE makes true -- the filter's NaN rule, inherited rather
    !> than implemented.
    pure logical function parquet_cmp_real(a, b, cmp) result(res)
        real(real64), intent(in) :: a !! the row's value.
        real(real64), intent(in) :: b !! the bound.
        integer, intent(in) :: cmp !! the CMP_* code.
        select case (cmp)
        case (CMP_EQ)
            res = (a == b)
        case (CMP_NE)
            res = (a /= b)
        case (CMP_LT)
            res = (a < b)
        case (CMP_LE)
            res = (a <= b)
        case (CMP_GT)
            res = (a > b)
        case default
            res = (a >= b)
        end select
    end function parquet_cmp_real
end submodule parquet_read_eval
