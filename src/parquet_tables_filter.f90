!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Answering a filter EXPRESSION against the rows already in memory: `%row_mask` and the two
!! expression forms of `%filter_rows`.
!!
!! `t%filter_rows("n_obs >= 8 and score > 3")` selects the same rows a read-time
!! `parquet_open_reader(..., filter=)` would, and that is a property this file exists to keep
!! rather than a coincidence. It is held by sharing everything that could differ:
!!
!! * the RULES are parsed by `parquet_parse_filter_rules`, the reader's own parser, so precedence,
!!   the AND-fold between `%add` calls, the keyword set and every syntax error message are one
!!   implementation;
!! * each LEAF is answered by `parquet_eval_filter_leaf`, which lives beside the reader's engine
!!   and reproduces it clause by clause -- the null rule, the NaN rule, the byte-lexicographic
!!   string order, the temporal unit rule and the whole set-valued path;
!! * the FOLD is `parquet_eval_filter_program`, the same postfix walk with the same three-valued
!!   stack and the single collapse of "unknown" to "excluded" at the end.
!!
!! What is left here is the part only the table can do: resolve each clause's column name to a
!! slot, READ it if it is not resident yet, and hand the evaluator the kind, width and stored
!! temporal unit the table recorded when it classified the column. Then apply, or hand the mask
!! back.
!!
!! **`%row_mask` never detaches and never mutates**; `%filter_rows` goes through
!! `table_apply_keep`, so it inherits the detach rule, the all-`.true.` no-op and the generation
!! bump from the mask form rather than repeating any of them. That is why the two `%filter_rows`
!! specifics live here and the mask specific stays in `parquet_tables_rowmutate.f90`: this file
!! computes a mask, that one owns what happens to the rows.
submodule (parquet_tables) parquet_tables_filter
    implicit none

contains

    module procedure table_row_mask_expr
        type(parquet_filter) :: filt
        !
        call filt%add(expr)
        call table_build_row_mask(self, filt, "row_mask", keep)
    end procedure table_row_mask_expr
    !
    module procedure table_row_mask_filter
        call table_build_row_mask(self, filter, "row_mask", keep)
    end procedure table_row_mask_filter
    !
    module procedure table_filter_rows_expr
        type(parquet_filter) :: filt
        !
        call filt%add(expr)
        call self%filter_rows(filt)
    end procedure table_filter_rows_expr
    !
    module procedure table_filter_rows_filter
        logical, allocatable :: keep(:)
        !
        ! The same two guards the mask form runs, and run HERE rather than left to it, because the
        ! mask is built first: refusing a shared or closed table only after reading every column
        ! the expression names would do the work and then throw it away.
        call table_check_not_shared(self, "filter_rows")
        call table_check_open(self, "filter_rows")
        allocate(keep(self%row_count))
        call table_build_row_mask(self, filter, "filter_rows", keep)
        call table_apply_keep(self, keep, "filter_rows")
    end procedure table_filter_rows_filter
    !
    !> Builds the row mask one filter selects: parse, answer each leaf against its column, fold.
    !>
    !> The whole of `%row_mask` and the first half of `%filter_rows`. `proc` names the caller so
    !> that a message reads `row_mask:` or `filter_rows:` according to what the user actually
    !> wrote.
    !>
    !> **A column named by the expression is READ if it is not resident**, by the ordinary lazy
    !> touch `table_resolve` performs for every value accessor -- so a rule may name a column
    !> nothing has read yet, exactly as `%get` may. That also means this is a read of the table's
    !> DATA, not of its metadata, and on a detached table a clause naming an unread column fails
    !> with the detach message rather than silently selecting nothing.
    subroutine table_build_row_mask(self, filter, proc, keep)
        class(parquet_table), intent(in) :: self !! the table.
        type(parquet_filter), intent(in) :: filter !! the filter whose rules select the rows.
        character(len=*), intent(in) :: proc !! calling procedure, for every message.
        logical, intent(out) :: keep(:) !! one entry per row; .true. for a selected row.
        integer(int8), allocatable :: node_kind(:), leaf_is_string(:)
        integer(int32), allocatable :: node_leaf(:)
        character(len=filter_leaf_name_len), allocatable :: leaf_name(:)
        character(len=filter_leaf_op_len), allocatable :: leaf_op(:)
        character(len=filter_leaf_value_len), allocatable :: leaf_value(:)
        integer(int8), allocatable :: verdicts(:, :)
        character(len=:), allocatable :: errmsg, sfx, name
        character(len=32) :: got, want
        integer :: i, idx, nnodes, nleaves
        logical :: ok
        !
        call table_check_open(self, proc)
        if (size(keep, kind=int64) /= self%row_count) then
            write (got, "(I0)") size(keep, kind=int64)
            write (want, "(I0)") self%row_count
            error stop EP // trim(proc) // ": the mask has " // trim(got) // " entries but the " // &
                "table has " // trim(want) // " rows"
        end if
        call parquet_parse_filter_rules(filter, node_kind, node_leaf, nnodes, leaf_name, leaf_op, &
            leaf_value, leaf_is_string, nleaves, ok, errmsg)
        if (.not. ok) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // trim(proc) // ": invalid filter rule: " // errmsg // sfx
        end if
        ! A filter with no rules selects every row -- the same thing an absent `filter=` does at the
        ! reader. Worth stating rather than falling out of the walk, because the walk would leave
        ! nothing on its stack and report a malformed expression instead.
        if (nleaves == 0) then
            keep = .true.
            return
        end if
        ! Every leaf is answered up front, into one column of `verdicts` each, and the walk then
        ! reads them rather than evaluating as it goes. That is what makes the fold a pure
        ! three-valued stack machine over int8 arrays with no column access in it at all, and it
        ! costs one int8 per row per leaf -- 4 MB for a million rows and four clauses.
        allocate(verdicts(max(self%row_count, 1_int64), nleaves))
        do i = 1, nleaves
            name = trim(leaf_name(i))
            call table_resolve(self, name, proc, idx)
            ! `trim(leaf_value(i))` drops a rule literal's trailing spaces exactly as the reader's
            ! `trim_right_spaces_and_nuls` (parquet_wrapper.cpp) does: change the two together or
            ! the engines disagree on such a literal (`test_string_match_is_byte_exact`).
            associate (slot => self%cache%cols(idx))
                call parquet_eval_filter_leaf(filter, slot%values, slot%declared_kind, slot%width, &
                    slot%time_unit, self%row_count, name, trim(leaf_op(i)), trim(leaf_value(i)), &
                    leaf_is_string(i) /= 0_int8, verdicts(:, i), ok, errmsg)
            end associate
            if (.not. ok) then
                call table_context_suffix(self%cache, name, sfx)
                error stop EP // trim(proc) // ": " // errmsg // sfx
            end if
        end do
        call parquet_eval_filter_program(node_kind, node_leaf, nnodes, verdicts, self%row_count, &
            keep, ok, errmsg)
        if (.not. ok) then
            ! Not reachable through the public API: the node list came from the library's own
            ! parser, which emits each leaf before the node referring to it and leaves exactly one
            ! result on the stack. Reported rather than ignored because the alternative is a mask
            ! built from an unwalked expression.
            call table_context_suffix(self%cache, "", sfx) ! GCOVR_EXCL_LINE
            error stop EP // trim(proc) // ": " // errmsg // sfx ! GCOVR_EXCL_LINE
        end if
    end subroutine table_build_row_mask
end submodule parquet_tables_filter
