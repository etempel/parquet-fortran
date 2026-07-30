!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Filter-expression parsing: turns one parquet_filter%add rule ("(ra > 180 and dec <= 0) or
!> id is_null") into the packed leaf arrays plus the postfix (RPN) node list the C++ evaluator
!> runs (see parquet_reader_set_filter in parquet_wrapper.cpp). Purely syntactic -- no schema
!> access at all, so nothing here can tell whether a column exists or whether a value suits its
!> type; that validation stays C++-side, where the schema is.
!>
!> Parsing runs Fortran-side, rather than sending the raw text across the bind(C) boundary, for
!> three reasons: a syntax error is reported through the same error-context helpers every other
!> Fortran-side failure uses, the parser is unit-testable in process with no file and no reader
!> (which is why the single-clause tokenizer has always been Fortran), and the boundary keeps
!> carrying fixed-width packed strings, the convention tools/check_bindc_boundary.py already
!> checks.
!>
!> The grammar (precedence not > and > or; keywords case-insensitive):
!>
!>     expr     := or_expr
!>     or_expr  := and_expr { or and_expr }
!>     and_expr := not_expr { and not_expr }
!>     not_expr := [ not ] not_expr | primary
!>     primary  := '(' expr ')' | clause
!>     clause   := NAME OP [ VALUE ]
!>
!> A clause is exactly what it has always been, and is still tokenized by
!> parquet_tokenize_filter_rule -- so every clause-level error message (and therefore every
!> error scenario asserting one) is unchanged by the arrival of the expression layer.
!>
!> The lexer reports token boundaries into the caller's own rule text rather than copying each
!> token out, so lexing a rule costs three small integer arrays regardless of how long the rule
!> is (a filter_max_rule_len-sized rule would otherwise need a token array of that length
!> squared, in the tens of megabytes).
submodule (parquet:parquet_read) parquet_read_filter
    implicit none

    !> Token kinds lex_filter_expr produces. TK_TEXT covers everything that is neither
    !> punctuation nor a keyword -- a column name, an operator, an unquoted value -- because
    !> which of those a text token is depends on its position within a clause, which the clause
    !> tokenizer decides, not the lexer.
    integer, parameter :: TK_TEXT = 1
    integer, parameter :: TK_QUOTED = 2 !! a double-quoted value; the quotes stay in, for the clause tokenizer.
    integer, parameter :: TK_LPAREN = 3
    integer, parameter :: TK_RPAREN = 4
    integer, parameter :: TK_AND = 5
    integer, parameter :: TK_OR = 6
    integer, parameter :: TK_NOT = 7

contains

    !> Lexes `rule` into parallel kind/start/end arrays, where start/end index into `rule`
    !> itself. Splits on whitespace and on the parentheses that are the grammar's only
    !> punctuation, recognizes the three keywords case-insensitively, and consumes a
    !> double-quoted run as ONE token (so a quoted value may contain spaces, parentheses, or the
    !> words and/or/not without being mis-lexed). Reports failure rather than aborting, so the
    !> caller can attach file context to the message.
    subroutine lex_filter_expr(rule, kinds, tok_lo, tok_hi, ntok, ok, errmsg)
        character(len=*), intent(in) :: rule !! raw filter expression text.
        integer, allocatable, intent(out) :: kinds(:) !! one TK_* per token.
        integer, allocatable, intent(out) :: tok_lo(:) !! each token's first character position in `rule`.
        integer, allocatable, intent(out) :: tok_hi(:) !! each token's last character position in `rule`.
        integer, intent(out) :: ntok !! number of tokens produced.
        logical, intent(out) :: ok !! .true. if the text lexed cleanly.
        character(len=:), allocatable, intent(out) :: errmsg !! failure message; "" when ok.
        integer :: i, n, start, cap
        character(len=1) :: c

        ok = .false.
        errmsg = ""
        ntok = 0
        n = len(rule)
        ! Every token is at least one character long and "((((" is a legal, maximally dense run of
        ! them, so the token count cannot exceed the character count. (One token per TWO characters
        ! looks like the tighter bound -- it is what a space-separated expression needs -- but
        ! parentheses need no separator, and under-sizing here overruns the arrays below.)
        cap = n + 2
        allocate(kinds(cap), tok_lo(cap), tok_hi(cap))

        i = 1
        do while (i <= n)
            c = rule(i:i)
            if (c == " " .or. c == char(9)) then
                i = i + 1
            else if (c == "(") then
                call push_token(TK_LPAREN, i, i)
                i = i + 1
            else if (c == ")") then
                call push_token(TK_RPAREN, i, i)
                i = i + 1
            else if (c == '"') then
                start = i
                i = i + 1
                do while (i <= n)
                    if (rule(i:i) == '"') exit
                    i = i + 1
                end do
                if (i > n) then
                    errmsg = "filter expression '" // trim(rule) // "' has an unterminated quoted value"
                    return
                end if
                call push_token(TK_QUOTED, start, i)
                i = i + 1
            else
                start = i
                do while (i <= n)
                    c = rule(i:i)
                    if (c == " " .or. c == char(9) .or. c == "(" .or. c == ")" .or. c == '"') exit
                    i = i + 1
                end do
                call push_token(keyword_kind(rule(start:i-1)), start, i - 1)
            end if
        end do

        if (ntok == 0) then
            errmsg = "empty filter rule"
            return
        end if
        ok = .true.
    contains
        !> Appends one token; the arrays are pre-sized to a bound the loop cannot exceed.
        subroutine push_token(kind, lo, hi)
            integer, intent(in) :: kind !! the token's TK_* kind.
            integer, intent(in) :: lo !! first character position.
            integer, intent(in) :: hi !! last character position.
            ntok = ntok + 1
            kinds(ntok) = kind
            tok_lo(ntok) = lo
            tok_hi(ntok) = hi
        end subroutine push_token
        !> TK_AND/TK_OR/TK_NOT for a bare keyword (any casing), TK_TEXT otherwise -- so a column
        !> named "android" or "nothing" is an ordinary text token, not a keyword.
        pure integer function keyword_kind(word) result(res)
            character(len=*), intent(in) :: word !! the whole token's text.
            select case (ascii_lower(word))
            case ("and")
                res = TK_AND
            case ("or")
                res = TK_OR
            case ("not")
                res = TK_NOT
            case default
                res = TK_TEXT
            end select
        end function keyword_kind
    end subroutine lex_filter_expr

    !> Lowercases an ASCII string; the three keywords are pure ASCII, so no locale-aware folding
    !> is needed. A character-valued function is fine here despite this project's ban on them:
    !> that ban covers deferred-length ALLOCATABLE results, whose gfortran codegen is not
    !> reliably thread-safe (GCC PR113797) -- this result's length is assumed from the argument.
    pure function ascii_lower(s) result(res)
        character(len=*), intent(in) :: s !! text to fold.
        character(len=len(s)) :: res !! `s` with every A-Z mapped to a-z.
        integer :: i, code
        do i = 1, len(s)
            code = iachar(s(i:i))
            if (code >= iachar("A") .and. code <= iachar("Z")) then
                res(i:i) = achar(code - iachar("A") + iachar("a"))
            else
                res(i:i) = s(i:i)
            end if
        end do
    end function ascii_lower

    !> Parses one rule and APPENDS its postfix node list, plus one packed leaf per clause, to
    !> the accumulators. Several calls therefore build one expression program; the caller folds
    !> them together by appending an ND_AND node after the second and each later rule (see
    !> parquet_apply_filter), which is what makes two %add calls mean (expr1) and (expr2).
    module procedure parquet_parse_filter_expr
        integer, allocatable :: kinds(:), tok_lo(:), tok_hi(:)
        integer :: ntok, pos, depth
        character(len=:), allocatable :: unexpected_tok

        call lex_filter_expr(rule, kinds, tok_lo, tok_hi, ntok, ok, errmsg)
        if (.not. ok) return

        pos = 1
        depth = 0
        call parse_or_expr(ok, errmsg)
        if (.not. ok) return
        if (pos <= ntok) then
            ok = .false.
            call tok_text(pos, unexpected_tok)
            errmsg = "filter expression '" // trim(rule) // "': unexpected '" // unexpected_tok // &
                "' (a missing and/or?)"
            return
        end if
    contains
        !> The token at `p`, as it appears in the rule. A subroutine (not a `character(len=...)`
        !> function whose length is a specification expression over host-associated arrays) --
        !> that shape reliably segfaults ifx 2026.1.1 when called from a sibling contained
        !> procedure such as parse_clause/parse_primary; see the ifx internal-compiler-error
        !> report filed alongside this fix for the minimal reproducer.
        pure subroutine tok_text(p, res)
            integer, intent(in) :: p !! 1-based token index.
            character(len=:), allocatable, intent(out) :: res !! that token's own text.
            res = rule(tok_lo(p):tok_hi(p))
        end subroutine tok_text
        !> or_expr := and_expr { or and_expr } -- lowest precedence, so it is the entry point.
        recursive subroutine parse_or_expr(sub_ok, sub_err)
            logical, intent(out) :: sub_ok !! .true. if this production parsed.
            character(len=:), allocatable, intent(out) :: sub_err !! failure message; "" when sub_ok.
            call parse_and_expr(sub_ok, sub_err)
            if (.not. sub_ok) return
            do while (pos <= ntok)
                if (kinds(pos) /= TK_OR) exit
                pos = pos + 1
                call parse_and_expr(sub_ok, sub_err)
                if (.not. sub_ok) return
                call emit_node(ND_OR, 0, sub_ok, sub_err)
                if (.not. sub_ok) return
            end do
        end subroutine parse_or_expr
        !> and_expr := not_expr { and not_expr }.
        recursive subroutine parse_and_expr(sub_ok, sub_err)
            logical, intent(out) :: sub_ok !! .true. if this production parsed.
            character(len=:), allocatable, intent(out) :: sub_err !! failure message; "" when sub_ok.
            call parse_not_expr(sub_ok, sub_err)
            if (.not. sub_ok) return
            do while (pos <= ntok)
                if (kinds(pos) /= TK_AND) exit
                pos = pos + 1
                call parse_not_expr(sub_ok, sub_err)
                if (.not. sub_ok) return
                call emit_node(ND_AND, 0, sub_ok, sub_err)
                if (.not. sub_ok) return
            end do
        end subroutine parse_and_expr
        !> not_expr := [ not ] not_expr | primary -- right-recursive, so "not not a" is legal
        !> (and idempotent) with no special case. Each `not` counts as one nesting level, so a
        !> pathological "not not not ..." hits the depth cap rather than the call stack.
        recursive subroutine parse_not_expr(sub_ok, sub_err)
            logical, intent(out) :: sub_ok !! .true. if this production parsed.
            character(len=:), allocatable, intent(out) :: sub_err !! failure message; "" when sub_ok.
            if (pos <= ntok) then
                if (kinds(pos) == TK_NOT) then
                    pos = pos + 1
                    call enter_level(sub_ok, sub_err)
                    if (.not. sub_ok) return
                    call parse_not_expr(sub_ok, sub_err)
                    depth = depth - 1
                    if (.not. sub_ok) return
                    call emit_node(ND_NOT, 0, sub_ok, sub_err)
                    return
                end if
            end if
            call parse_primary(sub_ok, sub_err)
        end subroutine parse_not_expr
        !> primary := '(' expr ')' | clause.
        recursive subroutine parse_primary(sub_ok, sub_err)
            logical, intent(out) :: sub_ok !! .true. if this production parsed.
            character(len=:), allocatable, intent(out) :: sub_err !! failure message; "" when sub_ok.
            character(len=:), allocatable :: cur_tok
            sub_ok = .false.
            sub_err = ""
            if (pos > ntok) then
                sub_err = "filter expression '" // trim(rule) // "' ends after an operator; a clause is missing"
                return
            end if
            select case (kinds(pos))
            case (TK_LPAREN)
                pos = pos + 1
                call enter_level(sub_ok, sub_err)
                if (.not. sub_ok) return
                if (pos <= ntok) then
                    if (kinds(pos) == TK_RPAREN) then
                        sub_ok = .false.
                        sub_err = "filter expression '" // trim(rule) // "' has an empty '()' group"
                        return
                    end if
                end if
                call parse_or_expr(sub_ok, sub_err)
                depth = depth - 1
                if (.not. sub_ok) return
                if (pos > ntok) then
                    sub_ok = .false.
                    sub_err = "filter expression '" // trim(rule) // "' has an unbalanced '('"
                    return
                end if
                if (kinds(pos) /= TK_RPAREN) then
                    sub_ok = .false.
                    call tok_text(pos, cur_tok)
                    sub_err = "filter expression '" // trim(rule) // "': expected ')' but found '" // &
                        cur_tok // "'"
                    return
                end if
                pos = pos + 1
            case (TK_RPAREN)
                sub_err = "filter expression '" // trim(rule) // "' has an unbalanced ')'"
            case (TK_AND, TK_OR)
                call tok_text(pos, cur_tok)
                sub_err = "filter expression '" // trim(rule) // "' has a dangling '" // cur_tok // &
                    "' with no clause on one side of it"
            case default
                call parse_clause(sub_ok, sub_err)
            end select
        end subroutine parse_primary
        !> clause := NAME OP [ VALUE ] -- collects the (up to three) text/quoted tokens that make
        !> up one clause, reassembles them into the single string parquet_tokenize_filter_rule
        !> already understands, and stores the result as one leaf. Reassembling rather than
        !> reimplementing is deliberate: the clause-level syntax, and every clause-level error
        !> message, then still has exactly one definition in the code base.
        subroutine parse_clause(sub_ok, sub_err)
            logical, intent(out) :: sub_ok !! .true. if the clause parsed.
            character(len=:), allocatable, intent(out) :: sub_err !! failure message; "" when sub_ok.
            character(len=:), allocatable :: clause_text, pname, pop, pvalue, cur_tok
            logical :: pis_string
            integer :: taken

            sub_ok = .false.
            sub_err = ""
            clause_text = ""
            taken = 0
            do while (pos <= ntok)
                if (taken >= 3) exit
                if (kinds(pos) /= TK_TEXT .and. kinds(pos) /= TK_QUOTED) exit
                if (taken > 0) clause_text = clause_text // " "
                call tok_text(pos, cur_tok)
                clause_text = clause_text // cur_tok
                taken = taken + 1
                pos = pos + 1
                ! A no-value operator ends the clause immediately, so a following bare name
                ! ("a is_null b > 1", missing its combinator) is reported as an unexpected token
                ! rather than silently swallowed as this clause's value.
                if (taken == 2) then
                    if (is_valueless_op(clause_text)) exit
                end if
            end do

            call parquet_tokenize_filter_rule(clause_text, pname, pop, pvalue, pis_string, sub_ok, sub_err)
            if (.not. sub_ok) return
            call emit_leaf(pname, pop, pvalue, pis_string, sub_ok, sub_err)
            if (.not. sub_ok) return
            call emit_node(ND_LEAF, nleaves, sub_ok, sub_err)
        end subroutine parse_clause
        !> Whether the clause text collected so far ends in an operator that takes no value.
        pure logical function is_valueless_op(clause_text) result(res)
            character(len=*), intent(in) :: clause_text !! the "<name> <op>" text collected so far.
            character(len=:), allocatable :: last
            last = ascii_lower(clause_text(index(clause_text, " ", back=.true.) + 1:))
            res = last == "is_null" .or. last == "is_not_null"
        end function is_valueless_op
        !> Counts one nesting level, refusing to go deeper than filter_max_depth. The cap keeps
        !> adversarial input ("((((((...") a clean error stop rather than a stack overflow, and
        !> bounds the C++ evaluator's peak memory, which is (live operands) * nrows bytes.
        subroutine enter_level(sub_ok, sub_err)
            logical, intent(out) :: sub_ok !! .true. if the new level is within the cap.
            character(len=:), allocatable, intent(out) :: sub_err !! failure message; "" when sub_ok.
            character(len=32) :: cap_str
            depth = depth + 1
            sub_ok = depth <= filter_max_depth
            sub_err = ""
            if (.not. sub_ok) then
                write(cap_str, '(i0)') filter_max_depth
                sub_err = "filter expression is nested deeper than the supported limit (" // trim(cap_str) // &
                    " levels)"
            end if
        end subroutine enter_level
        !> Appends one leaf to the packed leaf accumulators, growing them as needed and enforcing
        !> the per-leaf fixed widths the bind(C) packing convention imposes.
        subroutine emit_leaf(pname, pop, pvalue, pis_string, sub_ok, sub_err)
            character(len=*), intent(in) :: pname !! the clause's column name (may be a dotted struct path).
            character(len=*), intent(in) :: pop !! the clause's operator.
            character(len=*), intent(in) :: pvalue !! the clause's value, unquoted; "" for a valueless operator.
            logical, intent(in) :: pis_string !! .true. if the value was double-quoted in the source.
            logical, intent(out) :: sub_ok !! .true. if the leaf fits and was stored.
            character(len=:), allocatable, intent(out) :: sub_err !! failure message; "" when sub_ok.

            sub_ok = .true.
            sub_err = ""
            if (len(pname) > filter_leaf_name_len .or. len(pop) > filter_leaf_op_len .or. &
                    len(pvalue) > filter_leaf_value_len) then
                sub_ok = .false.
                sub_err = "filter rule exceeds an internal length limit: " // trim(rule)
                return
            end if
            call grow_leaves(nleaves + 1, leaf_name, leaf_op, leaf_value, leaf_is_string)
            nleaves = nleaves + 1
            leaf_name(nleaves) = pname
            leaf_op(nleaves) = pop
            leaf_value(nleaves) = pvalue
            leaf_is_string(nleaves) = merge(1_int8, 0_int8, pis_string)
        end subroutine emit_leaf
        !> Appends one postfix node, enforcing filter_max_nodes across the whole filter (the
        !> accumulator is shared by every %add call, so the cap is per filter, not per rule).
        subroutine emit_node(kind, leaf_index, sub_ok, sub_err)
            integer, intent(in) :: kind !! ND_* node kind.
            integer, intent(in) :: leaf_index !! 1-based leaf index for ND_LEAF; 0 otherwise.
            logical, intent(out) :: sub_ok !! .true. if the node fits and was stored.
            character(len=:), allocatable, intent(out) :: sub_err !! failure message; "" when sub_ok.
            call append_node(kind, leaf_index, node_kind, node_leaf, nnodes, sub_ok, sub_err)
        end subroutine emit_node
    end procedure parquet_parse_filter_expr

    !> Appends one operator node to an already-built node list -- how parquet_apply_filter folds
    !> several %add rules together with ND_AND without re-entering the parser.
    module procedure parquet_append_filter_node
        call append_node(kind, 0, node_kind, node_leaf, nnodes, ok, errmsg)
    end procedure parquet_append_filter_node

    !> The one place a node is appended: grows the accumulators, enforces filter_max_nodes, and
    !> reports (rather than aborts) when the cap is hit.
    subroutine append_node(kind, leaf_index, node_kind, node_leaf, nnodes, ok, errmsg)
        integer, intent(in) :: kind !! ND_* node kind.
        integer, intent(in) :: leaf_index !! 1-based leaf index for ND_LEAF; 0 otherwise.
        integer(int8), allocatable, intent(inout) :: node_kind(:) !! node-kind accumulator.
        integer(int32), allocatable, intent(inout) :: node_leaf(:) !! node-leaf-index accumulator.
        integer, intent(inout) :: nnodes !! nodes in use; incremented on success.
        logical, intent(out) :: ok !! .true. if the node fits and was stored.
        character(len=:), allocatable, intent(out) :: errmsg !! failure message; "" when ok.
        integer(int8), allocatable :: tmp_kind(:)
        integer(int32), allocatable :: tmp_leaf(:)
        character(len=32) :: cap_str
        integer :: cap

        ok = .true.
        errmsg = ""
        if (nnodes + 1 > filter_max_nodes) then
            write(cap_str, '(i0)') filter_max_nodes
            ok = .false.
            errmsg = "filter expression has more terms than the supported limit (" // trim(cap_str) // " nodes)"
            return
        end if
        if (.not. allocated(node_kind)) then
            allocate(node_kind(8), node_leaf(8))
        else if (size(node_kind) < nnodes + 1) then
            ! Double rather than fit exactly, so building a large expression stays linear.
            cap = 2 * size(node_kind)
            allocate(tmp_kind(cap), tmp_leaf(cap))
            tmp_kind(1:nnodes) = node_kind(1:nnodes)
            tmp_leaf(1:nnodes) = node_leaf(1:nnodes)
            call move_alloc(tmp_kind, node_kind)
            call move_alloc(tmp_leaf, node_leaf)
        end if
        nnodes = nnodes + 1
        node_kind(nnodes) = int(kind, int8)
        node_leaf(nnodes) = int(leaf_index, int32)
    end subroutine append_node

    !> Grows the four parallel leaf accumulators to hold at least `needed` entries; see
    !> append_node for the doubling rationale.
    subroutine grow_leaves(needed, leaf_name, leaf_op, leaf_value, leaf_is_string)
        integer, intent(in) :: needed !! entries the caller is about to require.
        character(len=filter_leaf_name_len), allocatable, intent(inout) :: leaf_name(:) !! column names.
        character(len=filter_leaf_op_len), allocatable, intent(inout) :: leaf_op(:) !! operators.
        character(len=filter_leaf_value_len), allocatable, intent(inout) :: leaf_value(:) !! values.
        integer(int8), allocatable, intent(inout) :: leaf_is_string(:) !! "value was quoted" flags.
        character(len=filter_leaf_name_len), allocatable :: tmp_name(:)
        character(len=filter_leaf_op_len), allocatable :: tmp_op(:)
        character(len=filter_leaf_value_len), allocatable :: tmp_value(:)
        integer(int8), allocatable :: tmp_flag(:)
        integer :: cap, n

        if (.not. allocated(leaf_name)) then
            allocate(leaf_name(8), leaf_op(8), leaf_value(8), leaf_is_string(8))
            return
        end if
        if (size(leaf_name) >= needed) return
        n = size(leaf_name)
        cap = max(2 * n, needed)
        allocate(tmp_name(cap), tmp_op(cap), tmp_value(cap), tmp_flag(cap))
        tmp_name(1:n) = leaf_name
        tmp_op(1:n) = leaf_op
        tmp_value(1:n) = leaf_value
        tmp_flag(1:n) = leaf_is_string
        call move_alloc(tmp_name, leaf_name)
        call move_alloc(tmp_op, leaf_op)
        call move_alloc(tmp_value, leaf_value)
        call move_alloc(tmp_flag, leaf_is_string)
    end subroutine grow_leaves

    !> Re-renders a parsed expression from its node list, in canonical form: one space between
    !> tokens, parentheses only where precedence needs them, and a value re-quoted when the
    !> source quoted it. Used for parquet_reader_print_stat's "filter:" line, so what was
    !> actually applied stays visible even though the per-column view cannot represent a
    !> non-flat expression.
    !>
    !> Walks the postfix list with a stack of already-rendered fragments, tracking each
    !> fragment's precedence so a child is parenthesised exactly when its own operator binds
    !> more loosely than its parent's.
    module procedure parquet_render_filter_expr
        character(len=:), allocatable :: stack_text(:)
        integer, allocatable :: stack_prec(:)
        character(len=:), allocatable :: lhs, rhs, rendered
        integer :: i, top, frag_len, stack_cap, prec, kind, li
        ! Precedence levels, used only to decide parentheses: a leaf never needs them, and a
        ! child is wrapped when its level is above its parent's.
        integer, parameter :: PREC_LEAF = 0, PREC_NOT = 1, PREC_AND = 2, PREC_OR = 3

        text = ""
        if (nnodes <= 0) return

        ! One fragment can grow to the whole expression, so every stack slot is sized for that:
        ! each leaf contributes its three fields plus quotes/spaces, and each operator node at
        ! most " and " plus a pair of parentheses.
        frag_len = nleaves * (filter_leaf_name_len + filter_leaf_op_len + filter_leaf_value_len + 4) + &
            nnodes * 7 + 1
        ! Stack depth is bounded by the recursion that produced the list: between two nesting
        ! levels the parser holds at most one pending operand per production (or/and/not), so
        ! three per level, plus the operand being built.
        stack_cap = min(nnodes, 3 * (filter_max_depth + 1) + 2)
        allocate(character(len=frag_len) :: stack_text(stack_cap))
        allocate(stack_prec(stack_cap))
        top = 0

        do i = 1, nnodes
            kind = int(node_kind(i))
            select case (kind)
            case (ND_LEAF)
                li = int(node_leaf(i))
                rendered = trim(leaf_name(li)) // " " // trim(leaf_op(li))
                if (leaf_is_string(li) /= 0_int8) then
                    rendered = rendered // ' "' // trim(leaf_value(li)) // '"'
                else if (len_trim(leaf_value(li)) > 0) then
                    rendered = rendered // " " // trim(leaf_value(li))
                end if
                top = top + 1
                if (top > stack_cap) then ! GCOVR_EXCL_START -- assertion: the bound above is
                    ! derived from the parser's own recursion, so exceeding it would mean the
                    ! node list did not come from parquet_parse_filter_expr.
                    text = ""
                    return
                end if ! GCOVR_EXCL_STOP
                stack_text(top) = rendered
                stack_prec(top) = PREC_LEAF
            case (ND_NOT)
                lhs = trim(wrap(trim(stack_text(top)), stack_prec(top), PREC_NOT))
                stack_text(top) = "not " // lhs
                stack_prec(top) = PREC_NOT
            case (ND_AND, ND_OR)
                prec = PREC_OR
                if (kind == ND_AND) prec = PREC_AND
                rhs = trim(wrap(trim(stack_text(top)), stack_prec(top), prec))
                lhs = trim(wrap(trim(stack_text(top-1)), stack_prec(top-1), prec))
                top = top - 1
                if (kind == ND_AND) then
                    stack_text(top) = lhs // " and " // rhs
                else
                    stack_text(top) = lhs // " or " // rhs
                end if
                stack_prec(top) = prec
            end select
        end do

        text = trim(stack_text(1))
    contains
        !> Parenthesises `frag` iff its own precedence level binds more loosely than the parent
        !> operator's -- exactly when dropping the parentheses would change the parse.
        pure function wrap(frag, frag_prec, parent_prec) result(res)
            character(len=*), intent(in) :: frag !! already-rendered child fragment.
            integer, intent(in) :: frag_prec !! the child's own precedence level.
            integer, intent(in) :: parent_prec !! the parent operator's precedence level.
            character(len=len(frag)+2) :: res !! `frag`, parenthesised if needed (blank-padded if not).
            if (frag_prec > parent_prec) then
                res = "(" // frag // ")"
            else
                res = frag
            end if
        end function wrap
    end procedure parquet_render_filter_expr

end submodule parquet_read_filter
