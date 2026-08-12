!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Reader lifecycle (open/close), filter/qc machinery, metadata population,
!> row-group resolution, and the type-generic shared helpers every
!> read-specifics child (parquet_read_numeric/_string/_temporal) reaches by
!> host association: validity-buffer construction, reader/column/row-group
!> existence checks, fixed-width text packing for the filter/qc C++ API, and
!> the whole-column-read-avoidance row-group helpers.
submodule (parquet_core) parquet_read
    use ieee_arithmetic, only: ieee_is_nan
    ! The read-time sort threads automatically, on the same rule pf_argsort follows -- see
    ! pf_sort_threads' own doc-comment for why that rule lives in parquet_sorting and is asked
    ! for here rather than reimplemented. parquet_sorting does not use parquet_core, so this
    ! import is acyclic.
    use parquet_sorting, only: pf_sort_threads
    implicit none

    !> Expression-node kinds in the postfix (RPN) node list a parsed filter becomes: one LEAF per
    !> clause, plus the boolean combinators. These values are part of the bind(C) contract -- the
    !> C++ evaluator (parquet_reader_set_filter in parquet_wrapper.cpp) switches on exactly these
    !> numbers, so changing one means changing both sides.
    integer, parameter :: ND_LEAF = 1
    integer, parameter :: ND_AND = 2
    integer, parameter :: ND_OR = 3
    integer, parameter :: ND_NOT = 4

    !> Fixed widths of the three packed per-leaf string arrays crossing the bind(C) boundary
    !> (see pack_fixed_width_strings). Declared once here and host-associated to
    !> parquet_read_filter, so the parser and the packer cannot disagree about them; the raw rule
    !> text itself never crosses the boundary, so it is not constrained by these.
    integer, parameter :: filter_leaf_name_len = 64
    integer, parameter :: filter_leaf_op_len = 16
    integer, parameter :: filter_leaf_value_len = 512

    !> Fixed width of the packed sort-key column-name array crossing the bind(C) boundary, the
    !> sort counterpart of filter_leaf_name_len above. Declared once here and host-associated to
    !> parquet_read_sort, so the parser and the packer cannot disagree about it.
    integer, parameter :: sort_key_name_len = 64

    !> The `row_group_lo` value meaning "the caller named no row-group range at all", passed by
    !> parquet_open_reader(..., filter=) and by the two-argument parquet_reader_set_filter.
    !!
    !! Part of the bind(C) contract: parquet_reader_set_filter in parquet_wrapper.cpp tests
    !! `rg_lo != -1` to choose between the whole-file engine (filter columns decoded once, in one
    !! batched pass, and left in the column cache) and the bounded-memory engine (row group at a
    !! time, nothing cached). The distinction cannot be made from the VALUE of a row-group range,
    !! because 0 is a legitimate one: it means "all row groups", the bounded-memory engine over the
    !! whole file. Every caller that does supply a range goes through clamp_row_group_lo, so this
    !! value can only ever originate here.
    integer(int64), parameter :: no_row_group_scope = -1_int64

    ! ---- Sort key parsing (parquet_read_sort) ----
    interface
        !> Re-renders the whole parsed key list in canonical form ("ra asc, dec desc"), for
        !> parquet_reader_print_stat's own "sort:" line. Never parsed again by anything.
        module subroutine parquet_render_sort_keys(key_name, descending, nulls_first, nkeys, text)
            character(len=sort_key_name_len), intent(in) :: key_name(:) !! per-key column name.
            integer(int8), intent(in) :: descending(:) !! per key: 1 for descending.
            integer(int8), intent(in) :: nulls_first(:) !! per key: 1 to place nulls first.
            integer, intent(in) :: nkeys !! keys in use.
            character(len=:), allocatable, intent(out) :: text !! rendered key list.
        end subroutine parquet_render_sort_keys
    end interface

    ! ---- Filter expression parsing (parquet_read_filter) ----
    interface
        !> Parses one parquet_filter%add rule (a clause, or clauses combined with and/or/not and
        !> parentheses) and APPENDS its postfix node list plus one packed leaf per clause to the
        !> accumulators, which several calls therefore build up together. `nnodes`/`nleaves` say
        !> how much of each (over-allocated) accumulator is in use. Purely syntactic: reports a
        !> parse failure via ok/errmsg -- never aborts, so the caller can attach the reader's
        !> file context to the message -- and leaves every schema-dependent check (column exists,
        !> value suits its type) to the C++ side.
        module subroutine parquet_parse_filter_expr(rule, node_kind, node_leaf, nnodes, leaf_name, leaf_op, &
                leaf_value, leaf_is_string, nleaves, ok, errmsg)
            character(len=*), intent(in) :: rule !! raw filter expression text from one %add call.
            integer(int8), allocatable, intent(inout) :: node_kind(:) !! ND_* kind per node, appended to.
            integer(int32), allocatable, intent(inout) :: node_leaf(:) !! 1-based leaf index per ND_LEAF node, else 0.
            integer, intent(inout) :: nnodes !! nodes in use; grows by this rule's own node count.
            character(len=filter_leaf_name_len), allocatable, intent(inout) :: leaf_name(:) !! per-leaf column name.
            character(len=filter_leaf_op_len), allocatable, intent(inout) :: leaf_op(:) !! per-leaf operator.
            character(len=filter_leaf_value_len), allocatable, intent(inout) :: leaf_value(:) !! per-leaf value, unquoted.
            integer(int8), allocatable, intent(inout) :: leaf_is_string(:) !! 1 if the value was double-quoted.
            integer, intent(inout) :: nleaves !! leaves in use; grows by this rule's own clause count.
            logical, intent(out) :: ok !! .true. if the rule parsed.
            character(len=:), allocatable, intent(out) :: errmsg !! parse-failure message; "" when ok.
        end subroutine parquet_parse_filter_expr
        !> Appends one operator node (ND_AND/ND_OR/ND_NOT) to an already-built node list -- how
        !> parquet_apply_filter AND-folds several %add rules into one expression without
        !> re-entering the parser. Reports the node-count cap via ok/errmsg, as above.
        module subroutine parquet_append_filter_node(kind, node_kind, node_leaf, nnodes, ok, errmsg)
            integer, intent(in) :: kind !! ND_AND, ND_OR or ND_NOT.
            integer(int8), allocatable, intent(inout) :: node_kind(:) !! ND_* kind per node, appended to.
            integer(int32), allocatable, intent(inout) :: node_leaf(:) !! 0 for an operator node.
            integer, intent(inout) :: nnodes !! nodes in use; incremented on success.
            logical, intent(out) :: ok !! .true. if the node fit within filter_max_nodes.
            character(len=:), allocatable, intent(out) :: errmsg !! failure message; "" when ok.
        end subroutine parquet_append_filter_node
        !> Re-renders a parsed expression from its node list in canonical form ("(ra > 180 and
        !> dec <= 0) or id is_null"): one space between tokens, parentheses only where precedence
        !> needs them, values re-quoted where the source quoted them. Used for
        !> parquet_reader_print_stat's whole-expression line, which stays meaningful for an
        !> expression the per-column view cannot represent.
        module subroutine parquet_render_filter_expr(node_kind, node_leaf, nnodes, leaf_name, leaf_op, &
                leaf_value, leaf_is_string, nleaves, text)
            integer(int8), intent(in) :: node_kind(:) !! ND_* kind per node.
            integer(int32), intent(in) :: node_leaf(:) !! 1-based leaf index per ND_LEAF node, else 0.
            integer, intent(in) :: nnodes !! nodes in use.
            character(len=filter_leaf_name_len), intent(in) :: leaf_name(:) !! per-leaf column name.
            character(len=filter_leaf_op_len), intent(in) :: leaf_op(:) !! per-leaf operator.
            character(len=filter_leaf_value_len), intent(in) :: leaf_value(:) !! per-leaf value, unquoted.
            integer(int8), intent(in) :: leaf_is_string(:) !! 1 if the value was double-quoted.
            integer, intent(in) :: nleaves !! leaves in use.
            character(len=:), allocatable, intent(out) :: text !! the canonical rendering; "" if nnodes <= 0.
        end subroutine parquet_render_filter_expr
    end interface

contains

    !> Allocates `valid_buf(n)` and points `valid_ptr` at it via c_loc only when
    !> the caller actually asked for null-tolerant reading (null_value and/or
    !> is_valid present); otherwise valid_ptr stays c_null_ptr so the C++ side
    !> keeps its default strict (error-on-null) behavior.
    subroutine make_valid_buf(want_report, n, valid_buf, valid_ptr)
        logical, intent(in) :: want_report !! .true. if null_value and/or is_valid was given by the caller.
        integer(int64), intent(in) :: n !! number of elements to allocate.
        integer(c_int8_t), allocatable, target, intent(out) :: valid_buf(:) !! int8 validity buffer backing valid_ptr.
        type(c_ptr), intent(out) :: valid_ptr !! c_loc(valid_buf), or c_null_ptr if want_report is .false.

        if (want_report) then
            allocate(valid_buf(n))
            valid_ptr = c_loc(valid_buf)
        else
            valid_ptr = c_null_ptr
        end if
    end subroutine make_valid_buf
    !> Every parquet_read_column variant calls this first: reader%handle is
    !> c_null_ptr until parquet_open_reader sets it, and every C++ entry point
    !> dereferences the handle immediately (see ConcurrencyGuard in
    !> parquet_wrapper.cpp) with no null check of its own -- calling in with an
    !> unopened reader previously crashed with an unhelpful SIGSEGV instead of
    !> a clean, diagnosable error.
    subroutine check_reader_open(reader, context)
        type(parquet_reader), intent(in) :: reader !! reader to check.
        character(len=*), intent(in) :: context !! calling procedure's name, used in the error-stop message.
        if (.not. c_associated(reader%handle)) then
            error stop trim(context) // ": reader has not been opened (call parquet_open_reader first)"
        end if
    end subroutine check_reader_open
    !> "" if reader%filename was never set; otherwise " (file: X)". Every
    !> caller of this is reached only once the reader is open, at which
    !> point parquet_open_reader has already set %filename, so this is
    !> effectively always populated in practice -- the allocated() guard is
    !> just defensive. Appended to post-open read errors so they name which
    !> parquet file failed, without needing that file to be threaded through
    !> every intermediate call.
    subroutine reader_filename_suffix(reader, suffix)
        type(parquet_reader), intent(in) :: reader !! reader whose filename is reported.
        character(len=:), allocatable, intent(out) :: suffix !! " (file: X)"-style suffix, or "".

        suffix = ""
        if (allocated(reader%filename)) then
            if (len_trim(reader%filename) > 0) suffix = " (file: " // trim(reader%filename) // ")"
        end if
    end subroutine reader_filename_suffix
    !> Every reader-taking procedure that also names a specific column calls
    !> this right after check_reader_open: an unrecognized column name used
    !> to reach the C++ side's own "Column not found" exception uncaught
    !> (get_single_chunk_array/get_column_index in parquet_wrapper.cpp),
    !> crashing with an unhelpful SIGABRT instead of a clean, diagnosable
    !> error -- the same class of bug parquet_prefetch_columns already
    !> guarded against (it validates every name up front for the same
    !> reason). Reuses parquet_reader_has_column, the same non-throwing
    !> schema lookup parquet_prefetch_columns uses.
    subroutine check_column_exists(reader, name, context)
        type(parquet_reader), intent(in) :: reader !! open reader to check against.
        character(len=*), intent(in) :: name !! column name to look up.
        character(len=*), intent(in) :: context !! calling procedure's name, used in the error-stop message.
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).
        if (parquet_reader_has_column(reader%handle, trim(name)//char(0)) == 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop trim(context) // ": column not found in parquet file: " // trim(name) // name_suffix
        end if
    end subroutine check_column_exists
    !> parquet_read_column_chunk's own extra guard, called right after check_column_exists:
    !> row_group must be within [1, num_row_groups], checked here (Fortran-side) rather than
    !> relying on Arrow's own ReadRowGroup bounds check, which throws an uncaught
    !> std::runtime_error (libc++abi terminate/SIGABRT with no diagnostic message reaching
    !> stderr cleanly) instead of this project's usual clean, diagnosable error_stop.
    subroutine check_row_group_valid(reader, row_group, context)
        type(parquet_reader), intent(in) :: reader !! open reader to check against.
        integer(int64), intent(in) :: row_group !! 1-based row group requested.
        character(len=*), intent(in) :: context !! calling procedure's name, used in the error-stop message.
        integer(int64) :: num_row_groups
        character(len=32) :: rg_str, total_str
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        num_row_groups = int(parquet_reader_get_num_row_groups(reader%handle), kind=int64)
        if (row_group < 1 .or. row_group > num_row_groups) then
            write(rg_str, '(i0)') row_group
            write(total_str, '(i0)') num_row_groups
            call reader_filename_suffix(reader, name_suffix)
            error stop trim(context) // ": row_group " // trim(rg_str) // " out of range (file has " // &
                trim(total_str) // " row group(s))" // name_suffix
        end if
    end subroutine check_row_group_valid
    !> Splits one parquet_filter%add rule ("<column> <op> [value]") into its
    !> three parts, purely by syntax -- no schema access here, so this cannot
    !> check that `name` is a real column or that `value` is well-formed for
    !> that column's actual type (parquet_reader_set_filter does that, once
    !> the schema is available). Deliberately NOT a general boolean-expression
    !> parser: exactly one clause per rule, no AND/OR/parens inside the string
    !> itself -- see the parquet_filter type's own doc comment.
    subroutine parquet_tokenize_filter_rule(rule, name, op, value, is_string, ok, errmsg)
        character(len=*), intent(in) :: rule !! raw "<column> <op> [value]" rule text.
        character(len=:), allocatable, intent(out) :: name !! parsed column name.
        character(len=:), allocatable, intent(out) :: op !! parsed operator.
        character(len=:), allocatable, intent(out) :: value !! parsed value (unquoted); "" if the operator takes none.
        character(len=:), allocatable, intent(out) :: errmsg !! parse-failure message; "" if ok is .true.
        logical, intent(out) :: is_string !! .true. if value was double-quoted in the source rule.
        logical, intent(out) :: ok !! .true. if rule parsed successfully.
        character(len=:), allocatable :: t, rest
        integer :: p

        ok = .false.
        is_string = .false.
        value = ""
        name = ""
        op = ""
        errmsg = ""
        t = trim(adjustl(rule))
        if (len(t) == 0) then ! GCOVR_EXCL_START -- gcov attribution artifact
            errmsg = "empty filter rule"
            return
        end if ! GCOVR_EXCL_STOP

        p = index(t, " ")
        if (p == 0) then
            errmsg = "filter rule '" // t // "' is missing an operator"
            return
        end if
        name = t(1:p-1)
        rest = trim(adjustl(t(p+1:)))
        if (len(rest) == 0) then ! GCOVR_EXCL_START -- gcov attribution artifact
            errmsg = "filter rule '" // t // "' is missing an operator"
            return
        end if ! GCOVR_EXCL_STOP

        p = index(rest, " ")
        if (p == 0) then
            op = rest
            rest = ""
        else
            op = rest(1:p-1)
            rest = trim(adjustl(rest(p+1:)))
        end if

        select case (trim(op))
        case ("is_null", "is_not_null", "is_nan", "is_not_nan")
            if (len(rest) > 0) then ! GCOVR_EXCL_START -- gcov attribution artifact
                errmsg = "filter rule '" // t // "': " // trim(op) // " takes no value"
                return
            end if ! GCOVR_EXCL_STOP
        case (">", ">=", "<", "<=", "==", "/=")
            if (len(rest) == 0) then ! GCOVR_EXCL_START -- gcov attribution artifact
                errmsg = "filter rule '" // t // "' is missing a value after '" // trim(op) // "'"
                return
            end if ! GCOVR_EXCL_STOP
            if (rest(1:1) == '"') then
                ! gcov attribution artifact: evaluated whenever a quoted value is seen, regardless of
                ! whether the closing quote is missing.
                if (len(rest) < 2 .or. rest(len(rest):len(rest)) /= '"') then ! GCOVR_EXCL_START
                    errmsg = "filter rule '" // t // "' has an unterminated quoted value"
                    return
                end if ! GCOVR_EXCL_STOP
                value = rest(2:len(rest)-1)
                is_string = .true.
            else
                value = rest
                is_string = .false.
            end if
        case default
            ! gcov attribution artifact: gfortran/gcov mis-attributes hit counts to this bare `return`
            ! from elsewhere in the subroutine's epilogue -- the errmsg assignment directly above it
            ! (same never-taken branch) reliably shows 0 hits, proving this arm is never actually reached.
            errmsg = "filter rule '" // t // "' has an unknown operator '" // trim(op) // "'" ! GCOVR_EXCL_LINE
            return ! GCOVR_EXCL_LINE -- gcov attribution artifact
        end select

        ok = .true.
    end subroutine parquet_tokenize_filter_rule
    !> Packs a fixed-width character array into the same "n fixed-width items
    !> back to back" convention used for names_packed elsewhere in this file
    !> (see parquet_prefetch_columns) -- shared here since parquet_apply_filter
    !> needs it for three separate arrays (names/ops/values).
    subroutine pack_fixed_width_strings(strs, packed)
        character(len=*), intent(in) :: strs(:) !! fixed-width strings to pack.
        character(kind=c_char), allocatable, intent(out) :: packed(:) !! flattened "n fixed-width items back to back" buffer.
        integer :: i, j, k, item_len, n

        item_len = len(strs)
        n = size(strs)
        allocate(packed(item_len * n))
        k = 0
        do i = 1, n
            do j = 1, item_len
                k = k + 1
                packed(k) = achar(iachar(strs(i)(j:j)), kind=c_char)
            end do
        end do
    end subroutine pack_fixed_width_strings
    !> Draws a Bernoulli(sample_fraction) row mask (see parquet_reader_set_sample in
    !> parquet_wrapper.cpp) and applies it to `reader` -- called from parquet_open_reader right
    !> after the reader itself is created and before any filter=/qc setup. sample_fraction must
    !> already be validated by the caller (not negative, not NaN, < 1.0); sample_seed <= 0 (or
    !> absent) draws a fresh entropy seed. The seed actually used is always reported back via
    !> parquet_reader_print_stat, whether caller-supplied or entropy-drawn, so a non-deterministic
    !> run's seed can be read back and reused later.
    !>
    !> filter_will_follow must be .true. iff parquet_open_reader_base is also about to call
    !> parquet_apply_filter right after this (i.e. present(filter) .and. filter%n > 0) -- when it
    !> is, the C++ side defers installing this draw as the reader's active mask until
    !> parquet_reader_set_filter folds it in, so the filter's own clause evaluation still sees raw,
    !> unmasked column data; installing it here immediately would otherwise make those columns come
    !> back already sample-compacted mid-evaluation (see parquet_reader_set_sample's own comment in
    !> parquet_wrapper.cpp for the crash this avoids).
    subroutine parquet_apply_sample(reader, sample_fraction, sample_seed, filter_will_follow)
        type(parquet_reader), intent(inout) :: reader !! open reader gaining the sample mask.
        real(real64), intent(in) :: sample_fraction !! fraction of rows to keep, already validated to be in [0.0, 1.0).
        integer(int32), intent(in), optional :: sample_seed !! >0 for a reproducible draw; absent/<=0 draws from entropy.
        logical, intent(in) :: filter_will_follow !! .true. iff parquet_apply_filter also runs right after this.
        integer(c_int32_t) :: seed_value, actual_seed
        integer(c_int8_t) :: has_seed_flag
        character(len=1024) :: c_err
        integer(c_long_long) :: status
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        seed_value = 0_c_int32_t
        has_seed_flag = 0_c_int8_t
        if (present(sample_seed)) then
            seed_value = int(sample_seed, kind=c_int32_t)
            has_seed_flag = 1_c_int8_t
        end if

        c_err = ""
        status = parquet_reader_set_sample(reader%handle, real(sample_fraction, kind=c_double), seed_value, &
            has_seed_flag, merge(1_c_int8_t, 0_c_int8_t, filter_will_follow), actual_seed, c_err, &
            int(len(c_err), kind=c_long_long))

        if (status /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_open_reader: " // trim(c_err) // name_suffix
        end if
    end subroutine parquet_apply_sample
    !> Validates+parses `maml` (parquet_parse_qc_maml) and hands the
    !> resulting per-column qc: rules to parquet_reader_set_qc -- called from
    !> parquet_open_reader BEFORE parquet_apply_filter, so that any column
    !> the filter itself touches while evaluating its clauses is already
    !> covered by qc (see run_qc_checks/parquet_reader_set_filter in
    !> parquet_wrapper.cpp). A field with no qc: block at all is already
    !> excluded by parquet_parse_qc_maml, so every entry in `rules` here
    !> really does need to reach parquet_reader_set_qc.
    subroutine parquet_apply_qc(reader, maml, qc_soft)
        type(parquet_reader), intent(inout) :: reader !! open reader gaining qc: enforcement.
        type(parquet_maml_file), intent(in) :: maml !! schema/qc-maml whose qc: rules are applied.
        logical, intent(in) :: qc_soft !! .true. warns on a qc violation instead of error-stopping.
        type(parquet_qc_rule), allocatable :: rules(:)
        character(len=64), allocatable :: names(:), min_ops(:), max_ops(:)
        character(len=256), allocatable :: min_texts(:), max_texts(:)
        integer(c_int8_t), allocatable :: has_min_flags(:), has_max_flags(:), null_allowed_flags(:)
        character(kind=c_char), allocatable :: names_packed(:), min_ops_packed(:), max_ops_packed(:)
        character(kind=c_char), allocatable :: min_texts_packed(:), max_texts_packed(:)
        integer :: i, n

        call parquet_parse_qc_maml(maml, rules)
        n = size(rules)

        allocate(names(max(n, 1)), min_ops(max(n, 1)), max_ops(max(n, 1)))
        allocate(min_texts(max(n, 1)), max_texts(max(n, 1)))
        allocate(has_min_flags(max(n, 1)), has_max_flags(max(n, 1)), null_allowed_flags(max(n, 1)))

        do i = 1, n
            names(i) = rules(i)%name
            has_min_flags(i) = merge(1_c_int8_t, 0_c_int8_t, rules(i)%has_min)
            min_ops(i) = rules(i)%min_op
            min_texts(i) = rules(i)%min_text
            has_max_flags(i) = merge(1_c_int8_t, 0_c_int8_t, rules(i)%has_max)
            max_ops(i) = rules(i)%max_op
            max_texts(i) = rules(i)%max_text
            null_allowed_flags(i) = merge(1_c_int8_t, 0_c_int8_t, rules(i)%null_values_allowed)
        end do

        call pack_fixed_width_strings(names, names_packed)
        call pack_fixed_width_strings(min_ops, min_ops_packed)
        call pack_fixed_width_strings(min_texts, min_texts_packed)
        call pack_fixed_width_strings(max_ops, max_ops_packed)
        call pack_fixed_width_strings(max_texts, max_texts_packed)

        call parquet_reader_set_qc(reader%handle, names_packed, int(len(names), kind=c_long_long), &
            has_min_flags, min_ops_packed, int(len(min_ops), kind=c_long_long), &
            min_texts_packed, int(len(min_texts), kind=c_long_long), &
            has_max_flags, max_ops_packed, int(len(max_ops), kind=c_long_long), &
            max_texts_packed, int(len(max_texts), kind=c_long_long), &
            null_allowed_flags, int(n, kind=c_long_long), merge(1_c_int8_t, 0_c_int8_t, qc_soft))
    end subroutine parquet_apply_qc
    !> Tokenizes and applies every rule in `filter` to `reader` -- called from
    !> parquet_open_reader right after the reader itself is created, so
    !> parquet_get_nrows and every column read afterward already reflect the
    !> filtered row set (see parquet_reader_set_filter in parquet_wrapper.cpp
    !> for the actual validation/masking).
    subroutine parquet_apply_filter(reader, filter, context, row_group_lo, row_group_hi, row_lo, row_hi)
        type(parquet_reader), intent(inout) :: reader !! open reader the filter is applied to.
        type(parquet_filter), intent(in) :: filter !! filter whose rules are parsed, validated, and applied.
        character(len=*), intent(in) :: context !! calling procedure's name, used in every error-stop message.
        integer(int64), intent(in) :: row_group_lo !! first row group, 0 for all of them, or no_row_group_scope.
        integer(int64), intent(in) :: row_group_hi !! last row group; ignored when row_group_lo is 0.
        integer(int64), intent(in) :: row_lo !! first physical row that may match, or 0 for no row bound.
        integer(int64), intent(in) :: row_hi !! last physical row that may match, or 0 for no row bound.
        integer(int8), allocatable :: node_kind(:), leaf_is_string(:)
        integer(int32), allocatable :: node_leaf(:)
        character(len=filter_leaf_name_len), allocatable :: leaf_name(:)
        character(len=filter_leaf_op_len), allocatable :: leaf_op(:)
        character(len=filter_leaf_value_len), allocatable :: leaf_value(:)
        character(kind=c_char), allocatable :: names_packed(:), ops_packed(:), values_packed(:)
        character(len=:), allocatable :: errmsg, expr_text
        logical :: ok
        character(len=1024) :: c_err
        integer(c_long_long) :: status
        integer :: i, nnodes, nleaves
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        nnodes = 0
        nleaves = 0
        do i = 1, filter%n
            call parquet_parse_filter_expr(filter%rules(i), node_kind, node_leaf, nnodes, leaf_name, &
                leaf_op, leaf_value, leaf_is_string, nleaves, ok, errmsg)
            if (.not. ok) then
                call reader_filename_suffix(reader, name_suffix)
                error stop trim(context) // ": invalid filter rule: " // errmsg // name_suffix
            end if
            ! Several %add calls are AND-combined -- (expr1) and (expr2) and ... -- so every rule
            ! after the first folds onto whatever is already on the stack.
            if (i > 1) then
                call parquet_append_filter_node(ND_AND, node_kind, node_leaf, nnodes, ok, errmsg)
                if (.not. ok) then
                    call reader_filename_suffix(reader, name_suffix)
                    error stop trim(context) // ": invalid filter rule: " // errmsg // name_suffix
                end if
            end if
        end do
        ! A rule-less filter still has something to install when a row-group scope or a physical
        ! row range was given: an all-true-within-range mask, which is how a slice-regime table
        ! carrying only sample_fraction= expresses its own bounds. With no rules AND no bounds
        ! there is genuinely nothing to do. `row_group_lo <= 0` covers both no_row_group_scope
        ! (-1, no range named) and 0 (all row groups): with no clauses, "every row of every row
        ! group" is what an absent filter already gives, whichever engine would have run.
        if (nleaves == 0 .and. row_group_lo <= 0 .and. row_group_hi <= 0 .and. &
                row_lo <= 0 .and. row_hi <= 0) return

        ! A rule-less filter (nleaves == nnodes == 0) never enters the loop above, so
        ! parquet_parse_filter_expr never gets a chance to allocate these -- but the row-bound-only
        ! path above still falls through to the array-section references below, and referencing
        ! even a zero-trip section of an unallocated allocatable is invalid (ifx's runtime checks
        ! catch this; gfortran silently tolerates it).
        if (.not. allocated(node_kind)) allocate(node_kind(0), node_leaf(0))
        if (.not. allocated(leaf_name)) allocate(leaf_name(0), leaf_op(0), leaf_value(0), leaf_is_string(0))

        call convert_temporal_filter_values(reader, context, leaf_name, leaf_op, leaf_value, leaf_is_string, nleaves)
        call parquet_render_filter_expr(node_kind, node_leaf, nnodes, leaf_name, leaf_op, leaf_value, &
            leaf_is_string, nleaves, expr_text)

        call pack_fixed_width_strings(leaf_name(1:nleaves), names_packed)
        call pack_fixed_width_strings(leaf_op(1:nleaves), ops_packed)
        call pack_fixed_width_strings(leaf_value(1:nleaves), values_packed)

        c_err = ""
        status = c_reader_set_filter(reader%handle, names_packed, int(filter_leaf_name_len, kind=c_long_long), &
            ops_packed, int(filter_leaf_op_len, kind=c_long_long), values_packed, &
            int(filter_leaf_value_len, kind=c_long_long), leaf_is_string(1:nleaves), &
            int(nleaves, kind=c_long_long), node_kind(1:nnodes), node_leaf(1:nnodes), &
            int(nnodes, kind=c_long_long), expr_text//char(0), row_group_lo, row_group_hi, &
            row_lo, row_hi, c_err, int(len(c_err), kind=c_long_long))

        call reader_filename_suffix(reader, name_suffix)
        if (status /= 0) error stop trim(context) // ": " // trim(c_err) // name_suffix
    end subroutine parquet_apply_filter
    !> Parses every key of `sort_by`, packs them into the fixed-width arrays the bind(C) boundary
    !> carries, and installs the sort -- called from parquet_open_reader(..., sort_by=) and from
    !> parquet_reader_set_sort, which is why the abort messages take their `context` from the
    !> caller. Mirrors parquet_apply_filter's shape exactly.
    !>
    !> Runs AFTER any filter/sample mask is installed, which is what makes "filter first, then
    !> sort within the survivors" true: each key column is read through the normal path, so it
    !> arrives already filtered, and the permutation covers the surviving rows only.
    subroutine parquet_apply_sort(reader, sort_by, context)
        type(parquet_reader), intent(inout) :: reader !! open reader the sort is applied to.
        type(parquet_sortkey), intent(in) :: sort_by !! keys to parse, validate and apply.
        character(len=*), intent(in) :: context !! calling procedure's name, used in every error-stop message.
        character(len=sort_key_name_len), allocatable :: key_name(:)
        integer(int8), allocatable :: descending(:), nulls_first(:)
        character(kind=c_char), allocatable :: names_packed(:)
        character(len=:), allocatable :: name, errmsg, key_text, name_suffix
        logical :: ok, desc
        character(len=1024) :: c_err
        integer(c_long_long) :: status
        integer :: i

        if (sort_by%n == 0) return
        allocate(key_name(sort_by%n), descending(sort_by%n), nulls_first(sort_by%n))
        do i = 1, sort_by%n
            call parquet_parse_sort_key(sort_by%keys(i), name, desc, ok, errmsg)
            if (.not. ok) then
                call reader_filename_suffix(reader, name_suffix)
                error stop trim(context) // ": invalid sort key: " // errmsg // name_suffix
            end if
            key_name(i) = name
            descending(i) = merge(1_int8, 0_int8, desc)
            nulls_first(i) = merge(1_int8, 0_int8, sort_by%nulls_first(i))
        end do

        call parquet_render_sort_keys(key_name, descending, nulls_first, sort_by%n, key_text)
        call pack_fixed_width_strings(key_name(1:sort_by%n), names_packed)

        c_err = ""
        status = c_reader_set_sort(reader%handle, names_packed, int(sort_key_name_len, kind=c_long_long), &
            descending(1:sort_by%n), nulls_first(1:sort_by%n), int(sort_by%n, kind=c_long_long), &
            key_text//char(0), int(pf_sort_threads(), kind=c_long_long), c_err, &
            int(len(c_err), kind=c_long_long))

        call reader_filename_suffix(reader, name_suffix)
        if (status /= 0) error stop trim(context) // ": " // trim(c_err) // name_suffix
    end subroutine parquet_apply_sort
    !> Refuses any row-group-scoped operation while a read-time sort is active. A sort permutation
    !> destroys row-group locality outright -- sorted row 5 may come from row group 47 and row 6
    !> from row group 3 -- so there is no coherent "row group N of the sorted output" to serve.
    !>
    !> This is NOT the guard the filter/sample mask once had, and the difference is the whole point
    !> (see reader_has_sort_permutation in parquet_wrapper.cpp): a mask only ever REMOVES rows, so
    !> row groups stay contiguous and every chunked read works under one. Only a permutation
    !> reorders. Keep every new "not while sorted" check keyed on this one predicate rather than
    !> testing the handle directly, so a future row transform cannot be added without the guards
    !> noticing it.
    subroutine check_reader_no_sort(reader, context)
        type(parquet_reader), intent(in) :: reader !! reader to check.
        character(len=*), intent(in) :: context !! calling procedure's name, used in the error-stop message.
        character(len=:), allocatable :: name_suffix
        if (parquet_reader_has_sort(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop trim(context) // ": not supported on a reader with an active sort, since a sorted row " // &
                "can come from any row group; read the column whole (parquet_read_column) instead" // name_suffix
        end if
    end subroutine check_reader_no_sort
    !> Rewrites every temporal (date/time/timestamp) leaf's value from the ISO-8601 text the
    !> caller wrote into the raw integer that column actually stores, so the C++ evaluator can
    !> compare it directly against the column's own values -- no ISO parsing and no unit
    !> arithmetic on that side. Doing the conversion here reuses parquet_temporal's own tested
    !> parsers (rather than re-deriving days_from_civil in C++), and is the only place the
    !> column's stored unit and the literal's precision can be compared, which is what makes the
    !> precision check below possible at all.
    !>
    !> Leaves naming a non-temporal column, an unknown column (so the C++ side still reports it
    !> with its own message) or a column of a type outside the nine canonical tokens are left
    !> exactly as they were.
    !>
    !> Timezones are deliberately not interpreted: a parquet_timestamp holds the stored epoch
    !> offset verbatim (see parquet_temporal's own module comment), so the literal is read as a
    !> civil date/time and compared against the same stored instants a read would return.
    subroutine convert_temporal_filter_values(reader, context, leaf_name, leaf_op, leaf_value, leaf_is_string, nleaves)
        type(parquet_reader), intent(in) :: reader !! open reader the filter is being applied to.
        character(len=*), intent(in) :: context !! calling procedure's name, used in error-stop messages.
        character(len=filter_leaf_name_len), intent(in) :: leaf_name(:) !! per-leaf column name.
        character(len=filter_leaf_op_len), intent(in) :: leaf_op(:) !! per-leaf operator.
        character(len=filter_leaf_value_len), intent(inout) :: leaf_value(:) !! per-leaf value; rewritten in place.
        integer(int8), intent(inout) :: leaf_is_string(:) !! per-leaf "was quoted" flag; cleared once converted.
        integer, intent(in) :: nleaves !! leaves in use.
        character(len=:), allocatable :: name, op, type_name, text, name_suffix
        character(len=32) :: raw_str
        logical :: recognized, parse_ok
        integer :: i, unit
        integer(int64) :: raw

        do i = 1, nleaves
            name = trim(leaf_name(i))
            op = trim(leaf_op(i))
            ! Every valueless operator is skipped, not just the two null tests: is_nan/is_not_nan
            ! carry no literal to convert, and letting one through here would report a temporal
            ! column's missing ISO-8601 literal instead of the real reason (the C++ side rejects
            ! is_nan on any non-floating-point column, with a message naming that).
            if (op == "is_null" .or. op == "is_not_null" .or. op == "is_nan" .or. op == "is_not_nan") cycle
            if (parquet_reader_has_column(reader%handle, name//char(0)) == 0) cycle
            call resolve_column_type(reader, name, type_name, recognized)
            if (.not. recognized) cycle
            if (type_name /= "date" .and. type_name /= "time" .and. type_name /= "timestamp") cycle

            call reader_filename_suffix(reader, name_suffix)
            if (leaf_is_string(i) == 0_int8) then
                error stop trim(context) // ": filter rule: value '" // trim(leaf_value(i)) // "' for " // &
                    type_name // " column '" // name // "' must be a double-quoted ISO-8601 literal (e.g. " // &
                    '"2024-01-31", "12:30:00", "2024-01-31T12:30:00")' // name_suffix
            end if
            text = trim(leaf_value(i))
            unit = parquet_unit_nanos
            ! Only a time/timestamp column has a stored unit; a date column is always whole days,
            ! and asking for its unit aborts inside parquet_reader_get_column_time_unit.
            if (type_name /= "date") unit = int(parquet_reader_get_column_time_unit(reader%handle, name//char(0)))
            call temporal_literal_to_raw(type_name, text, unit, raw, parse_ok)
            if (.not. parse_ok) then
                error stop trim(context) // ": filter rule: value '" // text // "' is not a valid ISO-8601 " // &
                    type_name // " for column '" // name // "', or is more precise than that column's " // &
                    "stored unit can represent" // name_suffix
            end if
            write(raw_str, '(i0)') raw
            leaf_value(i) = trim(raw_str)
            leaf_is_string(i) = 0_int8
        end do
    end subroutine convert_temporal_filter_values
    !> Parses one ISO-8601 filter literal into the raw integer a `date`/`time`/`timestamp` column
    !> of stored unit `unit` holds: days since the epoch, unit-of-day, or units since the epoch.
    !> Reports failure (ok = .false.) both for an unparseable literal and for one carrying finer
    !> precision than `unit` can represent -- a literal with a time part against a date column,
    !> or sub-millisecond digits against a timestamp[ms] column. Truncating instead would silently
    !> answer a question the caller did not ask.
    subroutine temporal_literal_to_raw(type_name, text, unit, raw, ok)
        character(len=*), intent(in) :: type_name !! "date", "time" or "timestamp".
        character(len=*), intent(in) :: text !! the ISO-8601 literal, quotes already stripped.
        integer, intent(in) :: unit !! the column's stored unit (a parquet_unit_* selector).
        integer(int64), intent(out) :: raw !! the value as that column stores it.
        logical, intent(out) :: ok !! .true. if the literal parsed and fits the unit exactly.
        type(parquet_date) :: d
        type(parquet_time) :: t
        type(parquet_timestamp) :: ts
        integer(int64) :: ns_per_unit, ns, secs
        integer(int32) :: nanos

        raw = 0_int64
        ok = .false.
        select case (type_name)
        case ("date")
            ! A date column stores whole days, so any time-of-day in the literal is unrepresentable
            ! -- parquet_date%parse rejects the "T..." form itself, which is exactly the wanted answer.
            call d%parse(text, ok)
            if (ok) raw = int(d%raw(), int64)
        case ("time")
            call t%parse(text, ok)
            if (.not. ok) return
            ns = t%raw()
            ns_per_unit = parquet_ns_per_sec/unit_scale_for(unit)
            ok = mod(ns, ns_per_unit) == 0_int64
            if (ok) raw = ns/ns_per_unit
        case ("timestamp")
            call ts%parse(text, ok)
            if (.not. ok) then
                ! A date-only literal is a legal, less-precise way to name an instant: it means
                ! midnight of that date. (More precision than the column's unit is the case that
                ! is rejected, below -- less is not.) parquet_timestamp%parse itself requires a
                ! full date-time, so the date form is re-parsed here rather than there.
                call d%parse(text, ok)
                if (.not. ok) return
                call t%set(0, 0, 0)
                call ts%set(d, t)
            end if
            call ts%get_raw(secs, nanos)
            ns_per_unit = parquet_ns_per_sec/unit_scale_for(unit)
            ! Checked here rather than left to %to_unix's own precision abort, so the message can
            ! name the column and the rule instead of ending the process from inside parquet_temporal.
            ok = mod(int(nanos, int64), ns_per_unit) == 0_int64
            if (ok) raw = ts%to_unix(unit)
        end select
    end subroutine temporal_literal_to_raw
    !> Units of one second for a parquet_unit_* selector (1 for seconds, 1000 for millis, ...) --
    !> the same scale parquet_temporal applies internally, needed here to convert a parsed
    !> literal into a column's own stored unit.
    pure integer(int64) function unit_scale_for(unit) result(res)
        integer, intent(in) :: unit !! a parquet_unit_* selector.
        select case (unit)
        case (parquet_unit_seconds) ! GCOVR_EXCL_LINE -- unreachable: a seconds-unit time/timestamp
            ! column cannot exist. Parquet's physical format has no seconds-resolution TIME or
            ! TIMESTAMP encoding at all, so apply_temporal_unit_token rejects a time[s]/timestamp[s]
            ! MAML token outright and no file can present one here. Kept for completeness of the
            ! parquet_unit_* selector set.
            res = 1_int64 ! GCOVR_EXCL_LINE
        case (parquet_unit_millis)
            res = 1000_int64
        case (parquet_unit_micros)
            res = 1000000_int64
        case default
            res = 1000000000_int64
        end select
    end function unit_scale_for
    !> Called once by parquet_open_reader, right after the handle is created:
    !> copies the file's flat key-value table metadata (whatever add_metadata
    !> wrote on the write side) into reader%metadata, so every later
    !> parquet_get_metadata call only scans this in-memory copy instead of
    !> re-reading the file. See parquet_reader_get_table_metadata_count and
    !> friends in parquet_bindings.f90/parquet_wrapper.cpp; `index` there is
    !> 0-based.
    subroutine populate_reader_metadata(reader)
        type(parquet_reader), intent(inout) :: reader !! open reader whose %metadata is populated.
        integer(c_long_long) :: count, klen, vlen, i
        character(len=:), allocatable :: kbuf, vbuf

        count = parquet_reader_get_table_metadata_count(reader%handle)
        if (count <= 0) return

        allocate(reader%metadata%items(int(count)))
        do i = 1, count
            klen = parquet_reader_get_table_metadata_key_length(reader%handle, i - 1)
            vlen = parquet_reader_get_table_metadata_value_length(reader%handle, i - 1)
            allocate(character(len=int(klen)) :: kbuf)
            allocate(character(len=int(vlen)) :: vbuf)
            if (klen > 0) call parquet_reader_get_table_metadata_key(reader%handle, i - 1, kbuf, klen)
            if (vlen > 0) call parquet_reader_get_table_metadata_value(reader%handle, i - 1, vbuf, vlen)
            reader%metadata%items(int(i))%key = kbuf
            reader%metadata%items(int(i))%value = vbuf
            deallocate(kbuf, vbuf)
        end do
    end subroutine populate_reader_metadata
    module procedure parquet_open_reader_base
        logical :: use_threads_value, qc_effective, qc_soft_value, filter_will_apply
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        use_threads_value = parquet_get_default_use_threads()
        if (present(use_threads)) use_threads_value = use_threads

        reader%handle = create_parquet_reader(trim(filename)//char(0), merge(1_c_int, 0_c_int, use_threads_value))
        reader%filename = trim(filename)
        call populate_reader_metadata(reader)

        filter_will_apply = .false.
        if (present(filter)) filter_will_apply = (filter%n > 0)

        ! Random downsampling (sample_fraction=): applied first, before any filter=/qc setup.
        ! filter_will_apply tells parquet_apply_sample whether parquet_apply_filter (below) is
        ! about to run right after this, so it can defer
        ! installing the draw as the reader's active mask until parquet_reader_set_filter folds it
        ! in -- see that subroutine's own doc-comment for why (installing it immediately here would
        ! make the filter's own referenced columns come back already sample-compacted mid-evaluation).
        ! NaN is checked first, before any relational comparison: NaN compares false against every
        ! threshold, so checking "< 0.0"/"< 1.0" first would let a NaN silently fall through as a
        ! no-op (same as >= 1.0) instead of reaching this error stop.
        if (present(sample_fraction)) then
            if (ieee_is_nan(sample_fraction)) then
                call reader_filename_suffix(reader, name_suffix)
                error stop "parquet_open_reader: sample_fraction must not be NaN" // name_suffix
            else if (sample_fraction < 0.0_real64) then
                call reader_filename_suffix(reader, name_suffix)
                error stop "parquet_open_reader: sample_fraction must not be negative" // name_suffix
            else if (sample_fraction < 1.0_real64) then
                call parquet_apply_sample(reader, sample_fraction, sample_seed, filter_will_apply)
            end if
        end if

        ! qc setup must happen before the filter is applied: the filter's own
        ! clause evaluation already counts as "touching" a column (see
        ! run_qc_checks/parquet_reader_set_filter in parquet_wrapper.cpp), so
        ! qc rules need to already be in place by then, not applied after.
        ! qc_soft defaults to .false. (hard: a violation aborts) and only ever
        ! matters when qc is on -- see run_qc_checks in parquet_wrapper.cpp.
        qc_effective = present(schema)
        if (present(qc)) qc_effective = qc
        qc_soft_value = .false.
        if (present(qc_soft)) qc_soft_value = qc_soft
        if (present(schema) .and. qc_effective) call parquet_apply_qc(reader, schema%maml, qc_soft_value)

        ! no_row_group_scope, not 0: an open-time filter names no row-group range, so it takes the
        ! whole-file engine and leaves its columns cached. A 0 here would ask for the
        ! bounded-memory, nothing-cached engine over all row groups instead -- see
        ! no_row_group_scope's own comment.
        if (filter_will_apply) call parquet_apply_filter(reader, filter, "parquet_open_reader", &
            no_row_group_scope, no_row_group_scope, 0_int64, 0_int64)

        ! Strictly after the filter: the sort orders the SURVIVING rows, so every key column has
        ! to arrive already masked (see parquet_apply_sort). Before the prefetch below, so a
        ! prefetched column is cached in sorted order rather than needing a second pass.
        if (present(sort_by)) call parquet_apply_sort(reader, sort_by, "parquet_open_reader")

        ! Must run AFTER parquet_apply_filter: a column cached before the
        ! filter mask exists would stay raw/unfiltered forever, since
        ! set_filter only re-masks the filter clauses' own columns, not the
        ! whole cache (see parquet_reader_prefetch_all_columns's own comment
        ! in parquet_wrapper.cpp). Filter columns are skipped here -- set_filter
        ! read and cached them itself, over the live row groups, and re-masked
        ! them -- so this only reads the remaining columns.
        if (present(prefetch)) then
            if (prefetch) call parquet_reader_prefetch_all_columns(reader%handle)
        end if
    end procedure parquet_open_reader_base
    !> The post-open counterpart of parquet_open_reader(..., filter=): same prefetch-then-apply
    !> sequence, same validation, same resulting reader state -- only the moment differs. Exists
    !> because a caller that does not own the parquet_open_reader call (a higher-level type that
    !> opens its own reader) otherwise has no way to filter at all.
    !>
    !> Both refusals below are checked before anything is read or mutated, so a rejected call
    !> leaves the reader exactly as it was:
    !>
    !>  - An already-filtered reader. Its mask is indexed by the file's physical rows, while a
    !>    second mask built now would be indexed by the surviving rows of the first, so composing
    !>    them is not a matter of ANDing two vectors of the same length. Several %add calls on one
    !>    parquet_filter are already AND-combined, which is what a caller wanting both should use.
    !>  - A reader that has already decoded a column. That column was returned (or cached) over
    !>    the unfiltered row set, and nothing read afterwards could be aligned with it.
    !>
    !> A reader opened with sample_fraction= is accepted: the sample mask is already installed, and
    !> these clauses AND onto it exactly as they would have at open time.
    module procedure parquet_reader_set_filter_base
        call parquet_reader_set_filter_impl(reader, filter, no_row_group_scope, no_row_group_scope, 0_int64, 0_int64)
    end procedure parquet_reader_set_filter_base
    module procedure parquet_reader_set_filter_scoped_int32
        call parquet_reader_set_filter_impl(reader, filter, clamp_row_group_lo(int(row_group_lo, int64)), &
            int(row_group_hi, int64), 0_int64, 0_int64)
    end procedure parquet_reader_set_filter_scoped_int32
    module procedure parquet_reader_set_filter_scoped_int64
        call parquet_reader_set_filter_impl(reader, filter, clamp_row_group_lo(row_group_lo), row_group_hi, &
            0_int64, 0_int64)
    end procedure parquet_reader_set_filter_scoped_int64
    module procedure parquet_reader_set_filter_rows_int32
        call parquet_reader_set_filter_impl(reader, filter, clamp_row_group_lo(int(row_group_lo, int64)), &
            int(row_group_hi, int64), int(row_lo, int64), int(row_hi, int64))
    end procedure parquet_reader_set_filter_rows_int32
    module procedure parquet_reader_set_filter_rows_int64
        call parquet_reader_set_filter_impl(reader, filter, clamp_row_group_lo(row_group_lo), row_group_hi, &
            row_lo, row_hi)
    end procedure parquet_reader_set_filter_rows_int64
    !> Maps any non-positive row-group lower bound onto 0 ("all row groups"), keeping the
    !> `no_row_group_scope` sentinel private to the forms that take no row-group arguments.
    !!
    !! This clamp is what makes the sentinel safe. Without it a caller passing -1 explicitly to the
    !! four-argument form would land on the *unscoped* path -- the whole-file, column-caching engine
    !! -- which is not what any negative bound means anywhere else in this library: every sibling
    !! procedure that takes a row-group range (parquet_measure_list_width, parquet_column_has_nulls)
    !! reads a non-positive lower bound as "all row groups". Here that answer is "all row groups,
    !! bounded-memory engine", because the arguments were supplied.
    pure function clamp_row_group_lo(row_group_lo) result(resolved)
        integer(int64), intent(in) :: row_group_lo !! caller's lower bound, possibly non-positive.
        integer(int64) :: resolved !! `row_group_lo`, or 0 when it was non-positive.

        resolved = row_group_lo
        if (resolved < 0_int64) resolved = 0_int64
    end function clamp_row_group_lo
    !> The one implementation behind every parquet_reader_set_filter form. row_group_lo/hi are
    !> `no_row_group_scope` when the caller named no row-group range (the whole-file, caching
    !> engine), 0 for "all row groups" on the bounded-memory engine, and an inclusive 1-based range
    !> otherwise; row_lo/hi likewise bound the PHYSICAL rows that may match, or are 0 for no row
    !> bound. Every range is validated C++-side against the file's own row-group and row counts,
    !> including that a row range lies inside the rows its row groups span.
    subroutine parquet_reader_set_filter_impl(reader, filter, row_group_lo, row_group_hi, row_lo, row_hi)
        type(parquet_reader), intent(inout) :: reader !! open, unfiltered reader with no column decoded yet.
        type(parquet_filter), intent(in) :: filter !! filter whose rules are parsed, validated, and applied.
        integer(int64), intent(in) :: row_group_lo !! first row group, 0 for all of them, or no_row_group_scope.
        integer(int64), intent(in) :: row_group_hi !! last row group; ignored when row_group_lo is 0.
        integer(int64), intent(in) :: row_lo !! first physical row that may match, or 0 for no row bound.
        integer(int64), intent(in) :: row_hi !! last physical row that may match, or 0 for no row bound.
        character(len=:), allocatable :: name_suffix

        call check_reader_open(reader, "parquet_reader_set_filter")
        if (parquet_reader_has_filter_clauses(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_reader_set_filter: this reader already has an active filter; combine the " // &
                "clauses into one parquet_filter instead (several %add calls are AND-combined)" // name_suffix
        end if
        ! BEFORE the decoded-columns guard below, deliberately. Ordering, not merely state:
        ! apply_row_transform (parquet_wrapper.cpp) masks first and permutes second, so a sort
        ! permutation's length is the POST-filter row count, and installing a filter under an
        ! existing sort would leave the two describing different row sets. The decoded-columns
        ! guard already refuses this in practice -- applying a sort decodes its key columns into
        ! the cache -- but only as a side effect, and it names the wrong mistake. Moving this
        ! check ahead of it is what makes the caller's actual error the one reported.
        if (parquet_reader_has_sort(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_reader_set_filter: this reader already has an active sort; apply the " // &
                "filter before the sort" // name_suffix
        end if
        if (parquet_reader_has_decoded_columns(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_reader_set_filter: a column has already been read on this reader; a filter " // &
                "must be applied before any column is read" // name_suffix
        end if
        ! As for parquet_reader_set_sort: a chunked read leaves the column cache empty, so the
        ! guard above cannot see it.
        if (parquet_reader_has_chunk_reads(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_reader_set_filter: a chunked read has already been done on this reader; a " // &
                "filter must be applied before any column is read" // name_suffix
        end if
        ! A rule-less filter still installs a mask when a row range was given (that range is the
        ! whole point of the call); with no rules and no range there is nothing to do.
        if (filter%n == 0 .and. row_lo <= 0 .and. row_hi <= 0) return

        call parquet_apply_filter(reader, filter, "parquet_reader_set_filter", row_group_lo, row_group_hi, &
            row_lo, row_hi)
    end subroutine parquet_reader_set_filter_impl
    module procedure parquet_reader_set_sort
        character(len=:), allocatable :: name_suffix

        call check_reader_open(reader, "parquet_reader_set_sort")
        if (parquet_reader_has_sort(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_reader_set_sort: this reader already has an active sort; add every key to " // &
                "one parquet_sortkey instead" // name_suffix
        end if
        if (parquet_reader_has_decoded_columns(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_reader_set_sort: a column has already been read on this reader; a sort " // &
                "must be applied before any column is read" // name_suffix
        end if
        ! A CHUNKED read caches nothing, so the guard above cannot see it -- see
        ! parquet_reader_has_chunk_reads. Rows already handed back are in physical row-group order
        ! and could not be reconciled with anything read after the permutation is installed.
        if (parquet_reader_has_chunk_reads(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_reader_set_sort: a chunked read has already been done on this reader; a " // &
                "sort must be applied before any column is read" // name_suffix
        end if
        if (sort_by%n == 0) return
        call parquet_apply_sort(reader, sort_by, "parquet_reader_set_sort")
    end procedure parquet_reader_set_sort
    module procedure parquet_reader_adopt_transform
        character(len=:), allocatable :: name_suffix
        character(len=1024) :: c_err
        integer(c_long_long) :: status
        !
        call check_reader_open(reader, "parquet_reader_adopt_transform")
        call check_reader_open(source, "parquet_reader_adopt_transform")
        c_err = ""
        ! Every precondition is checked on the C++ side rather than duplicated here: they are all
        ! about state this module cannot see (whether a mask is installed, whether a column has been
        ! decoded, whether the source's deferred sample draw has completed), and a Fortran-side copy
        ! of any of them could only go stale.
        status = c_reader_adopt_transform(reader%handle, source%handle, c_err, int(len(c_err), kind=c_long_long))
        if (status /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_reader_adopt_transform: " // trim(c_err) // name_suffix
        end if
    end procedure parquet_reader_adopt_transform
    module procedure parquet_open_reader_nrows_int64
        call parquet_open_reader_base(reader, filename, use_threads, filter, sample_fraction, sample_seed, schema, qc, &
            qc_soft, prefetch, sort_by)
        call parquet_get_nrows(reader, nrows, check_positive=.true.)
    end procedure parquet_open_reader_nrows_int64
    module procedure parquet_open_reader_nrows_int32
        call parquet_open_reader_base(reader, filename, use_threads, filter, sample_fraction, sample_seed, schema, qc, &
            qc_soft, prefetch, sort_by)
        call parquet_get_nrows(reader, nrows, check_positive=.true.)
    end procedure parquet_open_reader_nrows_int32
    module procedure parquet_close_reader
        logical :: do_check_complete, do_check_hard
        if (.not. c_associated(reader%handle)) then
            error stop "parquet_close_reader: reader has not been opened, or was already closed"
        end if
        if (present(print_stat)) then
            if (print_stat) call parquet_reader_print_stat(reader%handle)
        end if
        do_check_complete = .false.
        if (present(check_complete)) do_check_complete = check_complete
        if (do_check_complete) then
            do_check_hard = .true.
            if (present(check_hard)) do_check_hard = check_hard
            call parquet_reader_check_complete(reader%handle, merge(1_c_int, 0_c_int, do_check_hard))
        end if
        call close_parquet_reader(reader%handle)
        reader%handle = c_null_ptr
    end procedure parquet_close_reader
    !> Warms the cache for every column in `names` with a single, parallelizable
    !> (use_threads is on -- see create_parquet_reader) Arrow read, instead of
    !> one lazy single-column read per name. Purely additive: parquet_read_column
    !> (or any other read call) still works for any column, prefetched or not --
    !> a non-prefetched name simply falls through to the existing lazy,
    !> read-on-first-request path exactly as if this was never called.
    module procedure parquet_prefetch_columns_array
        character(kind=c_char), allocatable :: packed(:)
        integer :: i, j, k, item_len, n
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        call check_reader_open(reader, "parquet_prefetch_columns")
        n = size(names)
        if (n <= 0) return

        do i = 1, n
            if (parquet_reader_has_column(reader%handle, trim(names(i))//char(0)) == 0) then
                call reader_filename_suffix(reader, name_suffix)
                error stop "parquet_prefetch_columns: column not found in parquet file: " // trim(names(i)) // &
                    name_suffix
            end if
        end do

        item_len = len(names(1))
        allocate(packed(item_len * n))

        k = 0
        do i = 1, n
            do j = 1, item_len
                k = k + 1
                packed(k) = achar(iachar(names(i)(j:j)), kind=c_char)
            end do
        end do

        call parquet_reader_prefetch_columns(reader%handle, packed, int(item_len, kind=c_long_long), int(n, kind=c_long_long))
    end procedure parquet_prefetch_columns_array
    !> Scalar-string form of parquet_prefetch_columns: splits `names` on commas
    !> and/or semicolons ("ra;dec, mag"), trims each token, drops empty tokens
    !> (so trailing/repeated delimiters are harmless), packs them into a
    !> uniform-length array, and delegates to the array form -- which does the
    !> per-name existence check and the actual prefetch. Building the array
    !> here from the split tokens sizes its length to the longest name, so it
    !> cannot silently truncate a name the way a hand-declared fixed-length
    !> array can.
    module procedure parquet_prefetch_columns_string
        character(len=:), allocatable :: name_arr(:)

        ! The tokenizer lives in parquet_core (parquet_split_name_list) rather than here,
        ! because parquet_tables' own name- and key-list forms split identically and two
        ! copies of one punctuation convention is exactly the drift that would let
        ! t%prefetch("a,,b") and parquet_prefetch_columns(rdr, "a,,b") disagree.
        !
        ! The array form still runs even for zero tokens: it does the reader-open and
        ! column-existence checks, which an empty list must not skip.
        call parquet_split_name_list(names, name_arr)
        call parquet_prefetch_columns_array(reader, name_arr)
    end procedure parquet_prefetch_columns_string
    !> Safety net for a reader whose handle is still open when it goes out of
    !> scope or is overwritten -- frees the underlying C++ object so the
    !> process doesn't leak it. Always prefer calling parquet_close_reader
    !> explicitly.
    module procedure reader_finalize
        if (c_associated(this%handle)) then
            call close_parquet_reader(this%handle)
            this%handle = c_null_ptr
        end if
    end procedure reader_finalize
    !> Shared by both parquet_get_nrows overloads' check_positive=.true. path.
    !> nrows64 is always <= 0 here (never negative in practice, but the check
    !> itself is written against <= 0 to also cover that impossible case).
    !> If a filter narrowed the row count, parquet_reader_get_total_nrows
    !> differs from nrows64 (the post-filter count), so that case names the
    !> unfiltered total too, rather than just repeating "zero" twice.
    subroutine check_nrows_positive(reader, nrows64)
        type(parquet_reader), intent(in) :: reader !! open reader; named in the error-stop message.
        integer(int64), intent(in) :: nrows64 !! post-filter row count; error stops here since it is <= 0.
        integer(int64) :: total_nrows
        character(len=32) :: buf

        if (nrows64 > 0) return

        total_nrows = int(parquet_reader_get_total_nrows(reader%handle), kind=int64)
        if (total_nrows /= nrows64) then
            write(buf, '(I0)') total_nrows
            error stop "parquet_get_nrows: file " // trim(reader%filename) // &
                " has zero rows after filtering (" // trim(buf) // " total)"
        else
            error stop "parquet_get_nrows: file " // trim(reader%filename) // " has zero rows" ! GCOVR_EXCL_LINE
        end if
    end subroutine check_nrows_positive
    module procedure parquet_get_nrows_int64
        call check_reader_open(reader, "parquet_get_nrows")
        nrows = int(parquet_reader_get_nrows(reader%handle), kind=int64)
        if (present(check_positive)) then
            if (check_positive) call check_nrows_positive(reader, nrows)
        end if
    end procedure parquet_get_nrows_int64
    module procedure parquet_get_nrows_int32
        integer(int64) :: nrows64
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).
        call check_reader_open(reader, "parquet_get_nrows")
        nrows64 = int(parquet_reader_get_nrows(reader%handle), kind=int64)
        if (present(check_positive)) then
            if (check_positive) call check_nrows_positive(reader, nrows64)
        end if
        if (nrows64 > huge(0_int32)) then ! GCOVR_EXCL_START -- gcov attribution artifact
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_get_nrows: number of rows exceeds int32 range" // name_suffix
        end if ! GCOVR_EXCL_STOP
        nrows = int(nrows64, kind=int32)
    end procedure parquet_get_nrows_int32
    module procedure parquet_get_num_row_groups_int64
        call check_reader_open(reader, "parquet_get_num_row_groups")
        num_row_groups = int(parquet_reader_get_num_row_groups(reader%handle), kind=int64)
    end procedure parquet_get_num_row_groups_int64
    module procedure parquet_get_num_row_groups_int32
        integer(int64) :: num_row_groups64
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).
        call check_reader_open(reader, "parquet_get_num_row_groups")
        num_row_groups64 = int(parquet_reader_get_num_row_groups(reader%handle), kind=int64)
        if (num_row_groups64 > huge(0_int32)) then ! GCOVR_EXCL_START -- gcov attribution artifact
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_get_num_row_groups: number of row groups exceeds int32 range" // &
                name_suffix
        end if ! GCOVR_EXCL_STOP
        num_row_groups = int(num_row_groups64, kind=int32)
    end procedure parquet_get_num_row_groups_int32
    !> Shared body of parquet_get_chunk_size_reader_int32/_int64 -- see the generic interface's
    !> own doc comment in parquet_core.f90. `row_group` is 0 if the caller omitted its own optional
    !> row_group argument (both kind-specifics translate "absent" to 0 before calling in),
    !> resolved here to 1 (the first row group) -- 0 can never be a valid 1-based row_group, so
    !> it is unambiguous as an "absent" sentinel.
    !> Shared worker behind parquet_column_has_nulls's two kind specifics.
    function parquet_column_has_nulls_impl(reader, name, row_group_lo, row_group_hi) result(has_nulls)
        type(parquet_reader), intent(in) :: reader  !! open reader.
        character(len=*), intent(in) :: name       !! column name.
        integer(int64), intent(in) :: row_group_lo !! first row group (1-based); <= 0 means all.
        integer(int64), intent(in) :: row_group_hi !! last row group (1-based, inclusive).
        logical :: has_nulls                       !! .true. if it has Nulls, or if the file cannot say.
        !
        call check_reader_open(reader, "parquet_column_has_nulls")
        call check_column_exists(reader, name, "parquet_column_has_nulls")
        has_nulls = parquet_reader_column_has_nulls(reader%handle, trim(name)//char(0), &
            row_group_lo, row_group_hi) /= 0
    end function parquet_column_has_nulls_impl
    !
    !> Shared worker behind parquet_measure_list_width's two kind specifics.
    subroutine parquet_measure_list_width_impl(reader, name, row_group_lo, row_group_hi, proven, width)
        type(parquet_reader), intent(in) :: reader        !! open reader.
        character(len=*), intent(in) :: name             !! column name.
        integer(int64), intent(in) :: row_group_lo       !! first row group (1-based); <= 0 means all.
        integer(int64), intent(in) :: row_group_hi       !! last row group (1-based, inclusive).
        logical, intent(in) :: proven                    !! .true.: prove by reading; .false.: footer only.
        integer, intent(out) :: width                    !! uniform element count per row, or 1.
        integer(int64) :: w
        !
        call check_reader_open(reader, "parquet_measure_list_width")
        call check_column_exists(reader, name, "parquet_measure_list_width")
        if (proven) then
            w = parquet_reader_list_width_verified(reader%handle, trim(name)//char(0), row_group_lo, row_group_hi)
        else
            w = parquet_reader_list_width_candidate(reader%handle, trim(name)//char(0), row_group_lo, row_group_hi)
        end if
        ! Deliberately NOT clamped to 1: a column with no rows reports 0, matching what
        ! parquet_get_col_size has always answered for an empty list column.
        width = int(w)
    end subroutine parquet_measure_list_width_impl
    !
    subroutine parquet_get_chunk_size_reader_impl(reader, chunk_size, row_group)
        type(parquet_reader), intent(in) :: reader !! open reader.
        integer(int64), intent(out) :: chunk_size !! row group's own physical row count.
        integer(int64), intent(in) :: row_group !! 1-based row group, or 0 for "use the first row group".
        integer(int64) :: rg, num_row_groups
        character(len=32) :: rg_str, total_str
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        call check_reader_open(reader, "parquet_get_chunk_size")
        ! A row group's row count is only meaningful while row groups still correspond to
        ! contiguous result rows, which a sort permutation ends -- see check_reader_no_sort.
        call check_reader_no_sort(reader, "parquet_get_chunk_size")
        rg = merge(row_group, 1_int64, row_group > 0)
        num_row_groups = int(parquet_reader_get_num_row_groups(reader%handle), kind=int64)
        if (rg < 1 .or. rg > num_row_groups) then
            write(rg_str, '(i0)') rg
            write(total_str, '(i0)') num_row_groups
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_get_chunk_size: row_group " // trim(rg_str) // " out of range (file has " // &
                trim(total_str) // " row group(s))" // name_suffix
        end if
        chunk_size = int(parquet_reader_get_chunk_size_at(reader%handle, rg), kind=int64)
    end subroutine parquet_get_chunk_size_reader_impl
    module procedure parquet_get_chunk_size_reader_int32
        integer(int64) :: chunk_size64, row_group64
        row_group64 = 0_int64
        if (present(row_group)) row_group64 = int(row_group, kind=int64)
        call parquet_get_chunk_size_reader_impl(reader, chunk_size64, row_group64)
        chunk_size = int(chunk_size64, kind=int32)
    end procedure parquet_get_chunk_size_reader_int32
    module procedure parquet_get_chunk_size_reader_int64
        integer(int64) :: row_group64
        row_group64 = 0_int64
        if (present(row_group)) row_group64 = row_group
        call parquet_get_chunk_size_reader_impl(reader, chunk_size, row_group64)
    end procedure parquet_get_chunk_size_reader_int64
    module procedure parquet_get_col_size
        call check_reader_open(reader, "parquet_get_col_size")
        call check_column_exists(reader, name, "parquet_get_col_size")
        col_size = int(parquet_reader_get_column_col_size(reader%handle, trim(name)//char(0)))
    end procedure parquet_get_col_size
    module procedure parquet_measure_list_width_int32
        call parquet_measure_list_width_impl(reader, name, int(row_group_lo, int64), &
            int(row_group_hi, int64), proven, width)
    end procedure parquet_measure_list_width_int32
    module procedure parquet_measure_list_width_int64
        call parquet_measure_list_width_impl(reader, name, row_group_lo, row_group_hi, proven, width)
    end procedure parquet_measure_list_width_int64
    module procedure parquet_column_has_nulls_int32
        has_nulls = parquet_column_has_nulls_impl(reader, name, int(row_group_lo, int64), int(row_group_hi, int64))
    end procedure parquet_column_has_nulls_int32
    module procedure parquet_column_has_nulls_int64
        has_nulls = parquet_column_has_nulls_impl(reader, name, row_group_lo, row_group_hi)
    end procedure parquet_column_has_nulls_int64
    module procedure parquet_column_width_needs_data
        call check_reader_open(reader, "parquet_column_width_needs_data")
        call check_column_exists(reader, name, "parquet_column_width_needs_data")
        needs_data = parquet_reader_column_width_is_deferred(reader%handle, trim(name)//char(0)) /= 0
    end procedure parquet_column_width_needs_data
    module procedure parquet_get_column_total_elements_int64
        call check_reader_open(reader, "parquet_get_column_total_elements")
        call check_column_exists(reader, name, "parquet_get_column_total_elements")
        total_elements = int(parquet_reader_get_column_total_elements(reader%handle, trim(name)//char(0)), kind=int64)
    end procedure parquet_get_column_total_elements_int64
    module procedure parquet_get_column_total_elements_int32
        integer(int64) :: nelem64
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).
        call check_reader_open(reader, "parquet_get_column_total_elements")
        call check_column_exists(reader, name, "parquet_get_column_total_elements")
        nelem64 = int(parquet_reader_get_column_total_elements(reader%handle, trim(name)//char(0)), kind=int64)
        if (nelem64 > huge(0_int32)) then ! GCOVR_EXCL_START -- gcov attribution artifact
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_get_column_total_elements: number of elements exceeds int32 range for column: " // &
                trim(name) // name_suffix
        end if ! GCOVR_EXCL_STOP
        total_elements = int(nelem64, kind=int32)
    end procedure parquet_get_column_total_elements_int32
    module procedure parquet_get_string_length
        call check_reader_open(reader, "parquet_get_string_length")
        call check_column_exists(reader, name, "parquet_get_string_length")
        max_string_length = int(parquet_reader_get_string_length(reader%handle, trim(name)//char(0)))
    end procedure parquet_get_string_length
    !> Every parquet_read_column variant calls this with its own `values`
    !> array's row count (size(values) for a scalar column, size(values, 2)
    !> for a vector column) before reading any data. A mismatch against the
    !> file's actual row count fails immediately with error stop, instead of
    !> reaching the underlying C++ read call, whose own "nrows mismatch"
    !> check reports a clean diagnostic but aborts the process rather than
    !> returning control to Fortran (see report_fatal_error in
    !> parquet_wrapper.cpp).
    !> Plain contained subroutine (not a module procedure): its body already
    !> lived here in the parquet_read parent, not a descendant submodule, so
    !> keeping it a module procedure after relocating the interface into this
    !> same file's own spec would mean parquet_read implementing its own
    !> spec-declared interface -- not the ancestor/descendant relationship
    !> module procedures require (same reasoning as parquet_metadata's
    !> parquet_parse_col_map, see Phase 2). Descendants reach it by host
    !> association (fact 3.6).
    subroutine parquet_check_read_row_count(reader, name, given_nrows)
        type(parquet_reader), intent(in) :: reader !! open reader.
        character(len=*), intent(in) :: name !! column being read; named only in the error-stop message.
        integer(c_long_long), intent(in) :: given_nrows !! row count of this read call's own `values` array.
        integer(c_long_long) :: file_nrows
        character(len=32) :: expected_str, got_str
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        file_nrows = parquet_reader_get_nrows(reader%handle)
        if (given_nrows /= file_nrows) then
            write(expected_str, '(i0)') file_nrows
            write(got_str, '(i0)') given_nrows
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_read_column: row count mismatch for column " // trim(name) // &
                ": file has " // trim(expected_str) // " rows but the values array implies " // trim(got_str) // &
                name_suffix
        end if
    end subroutine parquet_check_read_row_count
    module procedure parquet_get_column_time_info
        integer(c_long_long) :: tzlen

        call check_reader_open(reader, "parquet_get_column_time_info")
        call check_column_exists(reader, name, "parquet_get_column_time_info")
        if (present(unit)) then
            unit = int(parquet_reader_get_column_time_unit(reader%handle, trim(name)//char(0)))
        end if
        if (present(timezone)) then
            tzlen = parquet_reader_get_column_timezone_length(reader%handle, trim(name)//char(0))
            allocate(character(len=int(tzlen)) :: timezone)
            if (tzlen > 0) call parquet_reader_get_column_timezone(reader%handle, trim(name)//char(0), timezone, tzlen)
        end if
    end procedure parquet_get_column_time_info
    !> Resolves `name`'s canonical physical data type (see valid_query_data_types in parquet_core.f90),
    !> without checking that `name` exists first -- every caller (parquet_column_exists/
    !> parquet_get_column_type) already validated existence via parquet_reader_has_column/
    !> check_column_exists beforehand. `recognized` is .false. if the physical type falls outside
    !> the nine canonical tokens, in which case `type_name` instead holds a raw Arrow type
    !> description for a diagnostic message.
    subroutine resolve_column_type(reader, name, type_name, recognized)
        type(parquet_reader), intent(in) :: reader !! open reader whose column is queried.
        character(len=*), intent(in) :: name !! existing column name.
        character(len=:), allocatable, intent(out) :: type_name !! canonical token, or raw type description.
        logical, intent(out) :: recognized !! .true. if type_name is one of the nine canonical tokens.
        character(len=32) :: buf

        recognized = parquet_reader_get_column_type_name(reader%handle, trim(name)//char(0), buf, &
            int(len(buf), kind=c_long_long)) /= 0
        type_name = trim(buf)
    end subroutine resolve_column_type
    !> Splits `types` (known non-blank) on commas into trimmed, lowercased tokens.
    subroutine split_type_filter_tokens(types, tokens)
        character(len=*), intent(in) :: types !! comma-separated filter string.
        character(len=9), allocatable, intent(out) :: tokens(:) !! trimmed, lowercased tokens.
        integer :: filter_len, start_pos, comma_pos, comma_count, i
        character(len=:), allocatable :: token, token_lower

        filter_len = len_trim(types)
        comma_count = 0
        do i = 1, filter_len
            if (types(i:i) == ",") comma_count = comma_count + 1
        end do
        allocate(tokens(comma_count + 1))

        start_pos = 1
        i = 0
        do
            i = i + 1
            comma_pos = index(types(start_pos:filter_len), ",")
            if (comma_pos == 0) then
                token = types(start_pos:filter_len)
            else
                token = types(start_pos:start_pos + comma_pos - 2)
            end if
            call parquet_to_lower(trim(adjustl(token)), token_lower)
            tokens(i) = token_lower
            if (comma_pos == 0) exit
            start_pos = start_pos + comma_pos
        end do
    end subroutine split_type_filter_tokens
    !> .true. if `token` (already trimmed/lowercased) is one of valid_query_data_types's nine
    !> single tokens, or one of the group aliases "int"/"float"/"temporal".
    pure function type_filter_token_valid(token) result(valid)
        character(len=*), intent(in) :: token !! candidate token, already trimmed and lowercased.
        logical :: valid !! .true. if recognized.
        integer :: j

        valid = trim(token) == "int" .or. trim(token) == "float" .or. trim(token) == "temporal"
        if (valid) return
        do j = 1, size(valid_query_data_types)
            if (trim(token) == trim(valid_query_data_types(j))) then
                valid = .true.
                return
            end if
        end do
    end function type_filter_token_valid
    !> .true. if `resolved_type` (one of valid_query_data_types's nine tokens) satisfies `token`,
    !> a single already-validated filter token (a canonical type name or a group alias).
    pure function type_filter_token_matches(token, resolved_type) result(matches)
        character(len=*), intent(in) :: token !! single filter token, already validated.
        character(len=*), intent(in) :: resolved_type !! column's resolved canonical type.
        logical :: matches !! .true. if resolved_type satisfies token.

        select case (trim(token))
        case ("int")
            matches = trim(resolved_type) == "int32" .or. trim(resolved_type) == "int64"
        case ("float")
            ! Every numeric column, integers included -- an integer column IS readable into a
            ! float array, which is the question this alias asks. That makes an alias deliberately
            ! NOT the union of its member tokens: `float` matches an int32 column while `float64`
            ! does not, because the two ask different things ("can I read this as a float at all?"
            ! against "is float64 the right declaration?"). Both are useful; see this procedure's
            ! own doc-comment in parquet_core.f90.
            matches = trim(resolved_type) == "float32" .or. trim(resolved_type) == "float64" .or. &
                trim(resolved_type) == "int32" .or. trim(resolved_type) == "int64"
        case ("temporal")
            matches = trim(resolved_type) == "date" .or. trim(resolved_type) == "time" .or. &
                trim(resolved_type) == "timestamp"
        case default
            matches = trim(token) == trim(resolved_type)
        end select
    end function type_filter_token_matches
    module procedure parquet_column_exists
        character(len=9), allocatable :: tokens(:)
        character(len=:), allocatable :: resolved_type, name_suffix
        logical :: recognized
        integer :: i

        call check_reader_open(reader, "parquet_column_exists")

        if (present(types)) then
            if (len_trim(types) == 0) then
                call reader_filename_suffix(reader, name_suffix)
                error stop "parquet_column_exists: types= must not be empty" // name_suffix
            end if
            call split_type_filter_tokens(types, tokens)
            do i = 1, size(tokens)
                if (.not. type_filter_token_valid(tokens(i))) then
                    call reader_filename_suffix(reader, name_suffix)
                    error stop "parquet_column_exists: unrecognized data type token '" // trim(tokens(i)) // &
                        "' in types= (valid: int32, int64, float32, float64, boolean, string, date, time, " // &
                        "timestamp, or the group aliases int/float/temporal)" // name_suffix
                end if
            end do
        end if

        exists = parquet_reader_has_column(reader%handle, trim(name)//char(0)) /= 0
        if (.not. exists .or. .not. present(types)) return

        call resolve_column_type(reader, name, resolved_type, recognized)
        exists = .false.
        if (.not. recognized) return

        do i = 1, size(tokens)
            if (type_filter_token_matches(tokens(i), resolved_type)) then
                exists = .true.
                return
            end if
        end do
    end procedure parquet_column_exists
    module procedure parquet_get_column_type
        logical :: recognized

        call check_reader_open(reader, "parquet_get_column_type")
        ! Still aborts on a name that does not exist -- that is a caller mistake, and
        ! parquet_column_exists is the query for "is it there?". An unreadable TYPE is not a
        ! mistake, though: answering "unknown" is the whole point of asking, so `recognized` is
        ! deliberately unused beyond documenting that resolve_column_type already wrote the token.
        call check_column_exists(reader, name, "parquet_get_column_type")
        call resolve_column_type(reader, name, type_name, recognized)
    end procedure parquet_get_column_type
    module procedure parquet_get_column_names
        integer(c_int32_t) :: ncols, i
        integer(c_long_long) :: name_len, max_len
        character(len=:), allocatable :: buf

        call check_reader_open(reader, "parquet_get_column_names")
        ncols = parquet_reader_get_column_count(reader%handle)
        ! Two passes: the first finds the longest name so `names` can be allocated at exactly
        ! that length (a fixed-length array cannot hold ragged names, and guessing a maximum
        ! would silently truncate a long dotted struct path). Both passes are pure schema
        ! lookups into a cache built at open time -- neither reads any column data.
        max_len = 0
        do i = 0, ncols - 1
            name_len = parquet_reader_get_column_name_length(reader%handle, i)
            if (name_len > max_len) max_len = name_len
        end do
        allocate(character(len=int(max_len)) :: names(ncols))
        if (ncols == 0) return
        allocate(character(len=int(max_len)) :: buf)
        do i = 0, ncols - 1
            call parquet_reader_get_column_name(reader%handle, i, buf, max_len)
            names(i + 1) = buf
        end do
    end procedure parquet_get_column_names
    !
    module procedure parquet_get_physical_row_indices
        integer(c_long_long) :: n
        !
        call check_reader_open(reader, "parquet_get_physical_row_indices")
        n = parquet_reader_get_nrows(reader%handle)
        allocate(rows(n))
        if (n == 0) return
        call parquet_reader_physical_row_indices(reader%handle, rows, n)
    end procedure parquet_get_physical_row_indices
    !
    module procedure parquet_get_metadata_items
        integer :: n, i, klen, vlen
        !
        call check_reader_open(reader, "parquet_get_metadata_items")
        ! The reader already holds the whole key/value table in memory (populate_reader_metadata
        ! copies it at open), so this is a reshape of what is there, not a read.
        n = 0
        if (allocated(reader%metadata%items)) n = size(reader%metadata%items)
        ! Same two-pass shape as parquet_get_column_names: one length for the whole array, found
        ! first, because a fixed-length array cannot hold ragged entries and a guessed maximum
        ! would truncate silently. Keys and values are sized independently -- a long value should
        ! not widen every key.
        klen = 0
        vlen = 0
        do i = 1, n
            klen = max(klen, len(reader%metadata%items(i)%key))
            vlen = max(vlen, len(reader%metadata%items(i)%value))
        end do
        allocate(character(len=klen) :: keys(n))
        allocate(character(len=vlen) :: values(n))
        do i = 1, n
            keys(i) = reader%metadata%items(i)%key
            values(i) = reader%metadata%items(i)%value
        end do
    end procedure parquet_get_metadata_items
    module procedure parquet_release_column
        call check_reader_open(reader, "parquet_release_column")
        call parquet_reader_release_column(reader%handle, trim(name)//char(0))
    end procedure parquet_release_column

end submodule parquet_read
