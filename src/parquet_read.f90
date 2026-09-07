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
    use ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_positive_inf
    ! The read-time sort BUILDS ITS PERMUTATION HERE, with the same radix engine pf_argsort gives
    ! every other caller -- so `parquet_open_reader(..., sort_by=)`, `parquet_reader_set_sort` and
    ! `parquet_table%sort_by` all run one comparator, and cannot disagree about null placement, NaN
    ! placement or tie order. The C++ side still owns the decode and the Arrow type reduction; only
    ! the ordering moved. parquet_sorting does not use parquet_core, so this import is acyclic.
    ! iso_c_binding is NOT imported here: parquet_core does an unrestricted `use iso_c_binding`,
    ! so c_loc/c_null_ptr/c_int/c_int8_t all arrive by host association, and naming them again is
    ! a symbol conflict rather than a clarification.
    use parquet_sorting, only: pf_sort_keys, pf_argsort
    ! The bound sets an `in`/`not_in` leaf is evaluated against are answered by the library's own
    ! key index rather than by a second lookup engine in C++ -- see parquet_evaluate_set_leaf.
    ! Every element family, the string family included since the map gained string keys (S7's
    ! P8b): a string set is one %get_many over the row group's string column, in place, where it
    ! used to be a `pf_in` sort-merge that re-sorted the set once per row group.
    ! pf_index_map is host-associated from parquet_core, which imports it; naming it AGAIN here is
    ! not redundancy but a requirement of nagfor 7.2, which cannot compile any child of an
    ! INTERMEDIATE submodule that references a name it reaches by host association from the module
    ! above (see CLAUDE.md, "An INTERMEDIATE submodule must not reference a name it reaches by HOST
    ! ASSOCIATION"). Every sibling of this file would fail to compile without this line, naming a
    ! module none of them mentions.
    use parquet_index, only: pf_index_map
    ! The row sample DRAWS ITS OWN MASK HERE, from the library's own counter-based generator, so
    ! that parquet_open_reader(..., sample_fraction=) and a caller's own pf_random_* draws come
    ! from one specified, tested generator instead of two. parquet_random is a leaf importing only
    ! iso_fortran_env, so this import is acyclic and costs the graph three files; see
    ! parquet_sample_algorithm (parquet_core.f90) for the mapping it composes.
    use parquet_random, only: pf_random_key, pf_random_fill_draws, pf_random_seed
    use iso_c_binding
    use iso_fortran_env, only: int8, int32, int64, real64
    use parquet_bindings
    use parquet_settings, only: parquet_get_default_use_threads, parquet_push_settings_to_cpp
    use parquet_maml_base, only: parquet_maml_file
    use parquet_strings, only: parquet_string_column
    use parquet_temporal, only: parquet_date, parquet_time, parquet_timestamp, parquet_unit_seconds, parquet_unit_millis, &
        parquet_unit_micros, parquet_unit_nanos, parquet_ns_per_sec
    implicit none

    !> Expression-node kinds in the postfix (RPN) node list a parsed filter becomes: one LEAF per
    !> clause, plus the boolean combinators. These values are part of the bind(C) contract -- the
    !> C++ evaluator (parquet_reader_set_filter in parquet_wrapper.cpp) switches on exactly these
    !> numbers, so changing one means changing both sides.
    integer, parameter :: ND_LEAF = 1
    integer, parameter :: ND_AND = 2
    integer, parameter :: ND_OR = 3
    integer, parameter :: ND_NOT = 4

    !> The three Kleene values a pre-evaluated (`in`/`not_in`) leaf hands to C++, one per PHYSICAL
    !> row of the file.
    !>
    !> Deliberately the same numbers the C++ evaluator uses for its own per-row Kleene values
    !> (kFalse/kTrue/kUnknown in parquet_wrapper.cpp), because these bytes are copied straight into
    !> the stack machine in place of an eval_filter_clause result. Two spellings of one three-state
    !> value would be a silent wrong answer the first time either side changed.
    integer(int8), parameter :: KL_FALSE = 0_int8
    integer(int8), parameter :: KL_TRUE = 1_int8
    integer(int8), parameter :: KL_UNKNOWN = 2_int8

    !> How many uniforms parquet_apply_sample materialises at a time while building its row mask.
    !>
    !! A bound on SCRATCH, never on the answer. `pf_random_fill_draws` is a prefix-consistent view
    !! of one stream -- filling `v(1:3)` alone gives the first three of `v(1:6)` -- so chunking at
    !! any size yields the identical mask. That is `parquet_sample_algorithm`'s "keep(r) depends
    !! only on (seed, r)" applied to this loop, and it is why the constant may be changed freely.
    !! 1024 `real64` is 8 KiB: small enough to stay an ordinary automatic array, large enough that
    !! the per-call overhead of the fill disappears against the per-row work.
    integer, parameter :: sample_fill_chunk = 1024

    !> Fixed width of the packed sort-key column-name array crossing the bind(C) boundary, the
    !> sort counterpart of parquet_core's filter_leaf_name_len. Declared here and host-associated to
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
            ! Allocated even when n is 0, because every caller's `is_valid = valid_buf /= 0` is a
            ! whole-array assignment that needs an allocated (if zero-sized) source.
            allocate(valid_buf(n))
            if (n > 0_int64) then
                valid_ptr = c_loc(valid_buf)
            else
                ! A ZERO-ROW read: `C_LOC` requires a nonzero-sized array (F2018 18.2.3.6), so the
                ! obvious `c_loc(valid_buf)` here is not conforming -- nagfor's -C=pointer rejects
                ! it at run time ("Argument VALID_BUF to C_LOC is a zero-sized array") while
                ! gfortran accepts it silently. A null pointer says the same thing anyway: there is
                ! no element for C++ to report validity for, and the zero-sized assignment above
                ! is a no-op either way.
                valid_ptr = c_null_ptr
            end if
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
    module procedure check_reader_open
        if (.not. c_associated(reader%handle)) then
            error stop trim(context) // ": reader has not been opened (call parquet_open_reader first)"
        end if
    end procedure check_reader_open
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
        case ("is_null", "is_not_null", "is_nan", "is_not_nan", "is_finite", "is_not_finite")
            if (len(rest) > 0) then ! GCOVR_EXCL_START -- gcov attribution artifact
                errmsg = "filter rule '" // t // "': " // trim(op) // " takes no value"
                return
            end if ! GCOVR_EXCL_STOP
        case ("in", "not_in")
            ! A SET-VALUED clause, whose value is a SET rather than a single literal. It is written
            ! either as the NAME of a set bound to the filter with %bind (`@name`, pandas'
            ! query("ID in @ids") spelling), or as a parenthesised LIST written out in the rule
            ! (`(1, 2, 3)`, `("a", "b")`). Both are checked here for shape only: whether a set of
            ! that name exists, what a list's elements parse to, and whether either suits the
            ! column, are questions for parquet_apply_filter, where the filter object and the file
            ! are both in hand.
            !
            ! A list arrives here already reassembled by the expression parser, which lifts it out
            ! of `rule` verbatim rather than from its tokens (see capture_literal_list in
            ! parquet_read_filter.f90) -- so the caller's own spacing survives into expr_text and
            ! print_stat, and a quoted element may contain a comma, a parenthesis or a keyword.
            if (len(rest) == 0) then
                errmsg = "filter rule '" // t // "' is missing a set after '" // trim(op) // &
                    "' (write it as '" // trim(op) // " @name' for a set attached by %bind, or as '" // &
                    trim(op) // " (1, 2, 3)' for a list written out here)"
                return
            end if
            if (rest(1:1) == "(") then
                ! The expression parser only produces this shape with both parentheses in place;
                ! the check is kept because the tokenizer is also reachable directly, and a value
                ! that opened a list without closing it would otherwise be parsed as one element.
                if (rest(len(rest):len(rest)) /= ")") then ! GCOVR_EXCL_START -- gcov attribution artifact
                    errmsg = "filter rule '" // t // "' has a list that is missing its closing ')'"
                    return
                end if ! GCOVR_EXCL_STOP
            else if (rest(1:1) /= "@") then
                errmsg = "filter rule '" // t // "': '" // trim(op) // "' takes either a bound set " // &
                    "named with a leading '@' (as in '" // trim(op) // " @wanted') or a list in " // &
                    "parentheses (as in '" // trim(op) // " (1, 2, 3)'), not the literal '" // &
                    trim(rest) // "'"
                return
            else if (len(rest) < 2) then
                errmsg = "filter rule '" // t // "': '@' must be followed by a set name"
                return
            end if
            value = rest
            is_string = .false.
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
    !> Builds this reader's Bernoulli(sample_fraction) row mask and hands it to the C++ side --
    !> called from parquet_open_reader right after the reader itself is created and before any
    !> filter=/qc setup. sample_fraction must already be validated by the caller (not negative, not
    !> NaN, < 1.0); sample_seed <= 0 (or absent) settles a fresh entropy seed. The seed actually
    !> used is always reported back via parquet_reader_print_stat, whether caller-supplied or
    !> drawn, so a non-deterministic run's seed can be read back and reused later.
    !>
    !> **The draw happens here, in Fortran, not in C++**, and the mapping is the frozen one
    !> `parquet_sample_algorithm` (parquet_core.f90) states. Its one property everything else rests
    !> on: row r's keep/drop depends on nothing but `(seed, r)`. That is what makes the chunked
    !> fill below, a pruned row group, and the deferral described next all incapable of moving a
    !> single decision -- and it is why the C++ side can simply index the mask by physical row.
    !>
    !> filter_will_follow must be .true. iff parquet_open_reader_base is also about to call
    !> parquet_apply_filter right after this (i.e. present(filter) .and. filter%n > 0) -- when it
    !> is, the C++ side holds the mask rather than installing it, so the filter's own clause
    !> evaluation still sees raw, unmasked column data; installing it immediately would otherwise
    !> make those columns come back already sample-compacted mid-evaluation (see
    !> parquet_reader_set_sample's own comment in parquet_wrapper.cpp for the crash this avoids).
    !> WHICH rows are selected is identical either way; the deferral is about ordering alone.
    subroutine parquet_apply_sample(reader, sample_fraction, sample_seed, filter_will_follow)
        type(parquet_reader), intent(inout) :: reader !! open reader gaining the sample mask.
        real(real64), intent(in) :: sample_fraction !! fraction of rows to keep, already validated to be in [0.0, 1.0).
        integer(int64), intent(in), optional :: sample_seed !! >0 for a reproducible draw; absent/<=0 settles a fresh one.
        logical, intent(in) :: filter_will_follow !! .true. iff parquet_apply_filter also runs right after this.
        integer(int64) :: seed_used, key, nrows, lo, n, k
        integer(c_int8_t), allocatable :: keep(:)
        real(real64) :: u(sample_fill_chunk)
        character(len=1024) :: c_err
        integer(c_long_long) :: status
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        ! A fraction of exactly 0.0 keeps nothing whatever the seed is, so it settles no seed and
        ! reports 0 -- long-documented behaviour, and what makes "there are no rows to reproduce"
        ! exactly true. Note this is a SEPARATE special case from the draw itself, which needs
        ! none: u is in [0, 1), so `u < 0.0` is never true and the loop below would keep nothing
        ! anyway. Do not fold the two together.
        seed_used = 0_int64
        if (sample_fraction > 0.0_real64) then
            ! `sample_seed <= 0` is parquet_open_reader's own spelling of "settle a fresh one", and
            ! pf_random_seed is always in [1, huge(int64)], so a settled seed can never read back
            ! through sample_seed= as "no seed".
            seed_used = pf_random_seed()
            if (present(sample_seed)) then
                if (sample_seed > 0_int64) seed_used = sample_seed
            end if
        end if

        nrows = int(parquet_reader_get_total_nrows(reader%handle), int64)
        ! max(.., 1) only so the actual argument is never zero-sized: keep_len still says 0, and
        ! the C++ side then reads nothing. A zero-sized actual passed to an assumed-size dummy is
        ! what nagfor's -C=array objects to, and it costs one byte to avoid.
        allocate(keep(max(nrows, 1_int64)))
        keep = 0_c_int8_t

        if (sample_fraction > 0.0_real64 .and. nrows > 0_int64) then
            key = pf_random_key(seed_used, parquet_sample_label)
            lo = 1_int64
            do while (lo <= nrows)
                n = min(int(sample_fill_chunk, int64), nrows - lo + 1_int64)
                ! Row r is draw r of stream 0. The fill is a prefix-consistent view of that one
                ! stream, so this chunk boundary cannot change any element -- see
                ! sample_fill_chunk's own note.
                call pf_random_fill_draws(key, 0_int64, u(1:n), lo)
                do k = 1, n
                    if (u(k) < sample_fraction) keep(lo + k - 1_int64) = 1_c_int8_t
                end do
                lo = lo + n
            end do
        end if

        c_err = ""
        status = parquet_reader_set_sample(reader%handle, real(sample_fraction, kind=c_double), &
            int(seed_used, kind=c_int64_t), keep, int(nrows, kind=c_int64_t), &
            merge(1_c_int8_t, 0_c_int8_t, filter_will_follow), c_err, int(len(c_err), kind=c_long_long))

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
    !> The int64 key a real64 value takes in a bound set; see the interface in parquet_core.f90 for
    !> the two exclusions that make bit-pattern comparison exact IEEE equality.
    !>
    !> Implemented HERE rather than in parquet_core itself because gfortran does not emit a private
    !> module-contained ELEMENTAL procedure whose in-module calls it has inlined, and this submodule
    !> references it -- an `undefined symbol` at link time, invisible until something links. See
    !> CLAUDE.md's "A private procedure contained directly in a module".
    module procedure parquet_filter_real_key
        if (v == 0.0_real64) then
            key = 0_int64
        else
            key = transfer(v, 0_int64)
        end if
    end procedure parquet_filter_real_key

    !> The canonicalising twin of parquet_filter_real_key, for a table index and the join; see the
    !> interface in parquet_core.f90 for why the two split on NaN and nowhere else.
    !>
    !> The one NaN key is the positive quiet NaN with an empty payload, written as a constant
    !> rather than as `transfer(ieee_value(...))` so that it is the same bit pattern on every
    !> compiler and never depends on how a NaN was produced. Detected with ieee_is_nan rather
    !> than `v /= v`, which -ffast-math would fold away (CLAUDE.md).
    module procedure parquet_index_real_key
        integer(int64), parameter :: canonical_nan = int(z'7FF8000000000000', int64)
        if (ieee_is_nan(v)) then
            key = canonical_nan
        else
            key = parquet_filter_real_key(v)
        end if
    end procedure parquet_index_real_key

    !> A date's key is its raw day count, widened; see the interface in parquet_core.f90.
    module procedure parquet_date_key
        key = int(d%raw(), int64)
    end procedure parquet_date_key

    !> A time's key is its raw nanoseconds since midnight; see the interface in parquet_core.f90.
    module procedure parquet_time_key
        key = t%raw()
    end procedure parquet_time_key

    !> A timestamp's key is its raw (seconds, nanoseconds) pair, both widened to int64 so that
    !> the two components share one array; see the interface in parquet_core.f90.
    module procedure parquet_timestamp_key
        integer(int32) :: ns
        call ts%get_raw(seconds, ns)
        nanoseconds = int(ns, int64)
    end procedure parquet_timestamp_key

    !> Prepares every set-valued (`in`/`not_in`) leaf of an already-parsed filter for the boundary:
    !> marks which leaves are pre-evaluated, and fills the packed verdict and screen-flag arrays
    !> the C++ stack machine and screen read in place of calling eval_filter_clause.
    !>
    !> **Why the membership test runs HERE and not in C++.** It is exactly what parquet_index was
    !> built for, so evaluating it in Fortran leaves the library with ONE lookup engine instead of a
    !> second sorted vector or hash set inside eval_filter_clause. What crosses the bind(C) boundary
    !> is then a copy of a mask -- the same shape and size as the sample mask
    !> parquet_reader_set_sample already hands over -- rather than a set and a search.
    !>
    !> **Why a PRIVATE reader, and only one.** The pre-evaluation must not populate the filtered
    !> reader's column cache, mark any row group as read (parquet_reader_check_complete would then
    !> see rows the caller never asked for), or trip the guard that refuses a filter after a read. A
    !> second parquet_reader over the same file, reading through the public
    !> parquet_read_column_chunk, has none of those side effects and needs no new internal entry
    !> point. One reader serves every leaf, so the cost is one extra footer parse per apply rather
    !> than one per clause. It carries no transform of its own, so its row count and its per-row-
    !> group chunk sizes are the file's PHYSICAL ones -- which is the coordinate system the verdict
    !> array must be in (feature_risks.md R-b).
    !>
    !> Nothing is allocated and no file is opened when the filter has no set-valued leaf, which is
    !> every filter written before this feature existed.
    subroutine parquet_prepare_set_leaves(reader, filter, context, leaf_name, leaf_op, leaf_value, &
            nleaves, leaf_pre, n_pre, pre_rows, pre_groups, verdicts, flags)
        type(parquet_reader), intent(in) :: reader !! the reader the filter is being applied to.
        type(parquet_filter), intent(in) :: filter !! the filter, whose bound sets are the payload.
        character(len=*), intent(in) :: context !! calling procedure's name, used in every error-stop message.
        character(len=*), intent(in) :: leaf_name(:) !! each parsed leaf's column name.
        character(len=*), intent(in) :: leaf_op(:) !! each parsed leaf's operator.
        character(len=*), intent(in) :: leaf_value(:) !! each parsed leaf's value; `@name` for a set clause.
        integer, intent(in) :: nleaves !! how many leaves the parse produced.
        integer(int32), allocatable, intent(out) :: leaf_pre(:) !! per leaf: 0, or its 1-based pre-evaluated index.
        integer(int64), intent(out) :: n_pre !! how many leaves are pre-evaluated.
        integer(int64), intent(out) :: pre_rows !! the file's physical row count (0 when n_pre is 0).
        integer(int64), intent(out) :: pre_groups !! the file's row-group count (0 when n_pre is 0).
        integer(int8), allocatable, intent(out) :: verdicts(:) !! n_pre * pre_rows Kleene values, leaf-major.
        integer(int8), allocatable, intent(out) :: flags(:) !! n_pre * pre_groups * 3 screen flags, leaf-major.
        type(parquet_reader) :: rdr
        type(pf_index_map) :: keys
        character(len=:), allocatable :: op_low, col_type, col_shape, errmsg, name_suffix, set_text
        integer(int8) :: fam
        integer :: i, set_index
        integer(int64) :: p, base
        logical :: negate, ok

        allocate(leaf_pre(max(nleaves, 1)))
        leaf_pre = 0_int32
        n_pre = 0_int64
        pre_rows = 0_int64
        pre_groups = 0_int64
        do i = 1, nleaves
            if (parquet_op_is_set_valued(leaf_op(i))) then
                n_pre = n_pre + 1_int64
                leaf_pre(i) = int(n_pre, int32)
            end if
        end do
        if (n_pre == 0_int64) then
            allocate(verdicts(0), flags(0))
            return
        end if

        call parquet_open_reader(rdr, reader%filename, use_threads=parquet_get_default_use_threads())
        call parquet_get_nrows(rdr, pre_rows)
        call parquet_get_num_row_groups(rdr, pre_groups)
        allocate(verdicts(n_pre * pre_rows), flags(n_pre * pre_groups * 3_int64))
        verdicts = KL_FALSE
        flags = 0_int8

        do i = 1, nleaves
            if (leaf_pre(i) == 0_int32) cycle
            p = int(leaf_pre(i), int64)
            ! An UNBOUND NAME is reported before the file is consulted at all -- see
            ! parquet_resolve_set_name's own note for why that order matters.
            set_text = trim(adjustl(leaf_value(i)))
            if (len(set_text) > 0) then
                if (set_text(1:1) /= "(") then
                    call parquet_resolve_set_name(filter, trim(leaf_name(i)), set_text, set_index, errmsg)
                    if (set_index == 0) then
                        call reader_filename_suffix(reader, name_suffix)
                        error stop trim(context) // ": " // errmsg // name_suffix
                    end if
                end if
            end if
            ! The shape and type come from the private reader; the resolution itself is engine-free
            ! and reports rather than aborts, so the file name is appended HERE and the identical
            ! message reaches a %row_mask caller with the table's own context instead.
            call parquet_get_column_shape(rdr, trim(leaf_name(i)), col_shape)
            call parquet_get_column_type(rdr, trim(leaf_name(i)), col_type)
            call parquet_resolve_set_payload(filter, trim(leaf_name(i)), leaf_value(i), col_type, &
                col_shape, fam, keys, ok, errmsg)
            if (.not. ok) then
                call reader_filename_suffix(reader, name_suffix)
                error stop trim(context) // ": " // errmsg // name_suffix
            end if
            call parquet_lower_op(leaf_op(i), op_low)
            negate = (op_low == "not_in")
            base = (p - 1_int64) * pre_rows
            call parquet_evaluate_set_leaf(rdr, context, trim(leaf_name(i)), fam, keys, negate, &
                pre_rows, pre_groups, verdicts(base + 1_int64:base + pre_rows), &
                flags((p - 1_int64) * pre_groups * 3_int64 + 1_int64:p * pre_groups * 3_int64))
        end do

        call parquet_close_reader(rdr)
    end subroutine parquet_prepare_set_leaves

    !> Whether `op` is one of the two set-valued operators, matched case-insensitively.
    pure logical function parquet_op_is_set_valued(op) result(res)
        character(len=*), intent(in) :: op !! a parsed leaf's operator.
        character(len=:), allocatable :: low
        call parquet_lower_op(op, low)
        res = (low == "in" .or. low == "not_in")
    end function parquet_op_is_set_valued

    !> `op` trimmed and lowercased -- the filter grammar's keywords are case-insensitive.
    !>
    !> A SUBROUTINE with an allocatable `character` result argument, not a function returning
    !> `character(len=:), allocatable`: that shape is banned project-wide (CLAUDE.md), because
    !> gfortran's codegen for receiving such a result uses a hidden length temporary that is not
    !> reliably thread-local (GCC PR113797). The same applies to the two below.
    pure subroutine parquet_lower_op(op, low)
        character(len=*), intent(in) :: op !! a parsed leaf's operator.
        character(len=:), allocatable, intent(out) :: low !! the same text, trimmed and in lower case.
        integer :: i
        low = trim(op)
        do i = 1, len(low)
            if (low(i:i) >= "A" .and. low(i:i) <= "Z") low(i:i) = achar(iachar(low(i:i)) + 32)
        end do
    end subroutine parquet_lower_op

    !> The set name a set-valued leaf's value refers to: its text with the leading `@` removed. The
    !> tokenizer has already refused a value that does not start with one.
    pure subroutine parquet_set_name_of(value, name)
        character(len=*), intent(in) :: value !! the leaf's value text, `@name`.
        character(len=:), allocatable, intent(out) :: name !! that set's name, without the '@'.
        name = trim(value)
        if (len(name) > 1) then
            name = name(2:)
        else
            name = "" ! GCOVR_EXCL_LINE -- the tokenizer refuses a bare '@' before a leaf is built.
        end if
    end subroutine parquet_set_name_of

    !> Finds the bound set `name` refers to, or reports that nothing is bound under it.
    !>
    !> An unbound name is a validation error naming it; a bound set that no rule uses is harmless
    !> and is not reported, since %bind and %add are independent calls and binding a set that a
    !> later revision of a rule will use is a reasonable thing to do.
    pure subroutine parquet_find_filter_set(filter, name, set_index)
        type(parquet_filter), intent(in) :: filter !! the filter whose sets are searched.
        character(len=*), intent(in) :: name !! the set name, without the '@'.
        integer, intent(out) :: set_index !! 1-based index into the filter's sets, or 0 when absent.
        integer :: k
        set_index = 0
        do k = 1, filter%nsets
            if (trim(filter%set_name(k)) == trim(name)) then
                set_index = k
                return
            end if
        end do
    end subroutine parquet_find_filter_set

    !> Resolves a `@name` leaf value to the 1-based index of the set bound under that name, or
    !> reports that nothing is bound under it. `set_index` is 0 exactly when `errmsg` is set.
    !>
    !> Split out of parquet_resolve_set_payload so that a caller can run it BEFORE it queries the
    !> file, which is load-bearing rather than tidy: an unbound name is a property of the filter
    !> object alone, so a rule naming a set nobody bound must say so rather than report whatever
    !> the column turns out to be. Asking the schema first makes a filter naming both an unbound
    !> set and a column the file lacks report the missing COLUMN, which is the less useful half and
    !> is not what the caller got wrong. Regression-tested by filter_set_unbound_name.
    subroutine parquet_resolve_set_name(filter, column, value, set_index, errmsg)
        type(parquet_filter), intent(in) :: filter !! the filter whose bound sets are searched.
        character(len=*), intent(in) :: column !! the column the clause tests, for the message.
        character(len=*), intent(in) :: value !! the leaf's value text, `@name`, already trimmed.
        integer, intent(out) :: set_index !! the set's 1-based index, or 0 when nothing is bound.
        character(len=:), allocatable, intent(out) :: errmsg !! the refusal; "" when found.
        character(len=:), allocatable :: set_name

        errmsg = ""
        call parquet_set_name_of(value, set_name)
        call parquet_find_filter_set(filter, set_name, set_index)
        if (set_index /= 0) return
        errmsg = "filter rule refers to the set '" // trim(value) // &
            "' on column '" // trim(column) // "', but no set of that name is bound to this " // &
            "filter -- attach one with %bind before applying the filter"
    end subroutine parquet_resolve_set_name

    !> Resolves one set-valued leaf's payload -- whether it NAMES a set bound with %bind or writes
    !> its members out as a LITERAL LIST -- into the one representation the evaluator reads: an
    !> element family, a built key index for the two numeric families, and a string column for the
    !> string family.
    !>
    !> **Both spellings meet here and nowhere else.** `x in @wanted` and `x in (1, 2, 3)` differ only
    !> in where the members come from; the column-shape and column-type checks, the null and NaN
    !> rules, the per-row-group walk and the screen are then applied once, to one object. That is
    !> what stops the two drifting apart, which they would otherwise do silently -- a literal list
    !> answered by a second code path would still return a perfectly plausible row set.
    !>
    !> The column's shape and type are passed IN rather than resolved here because the literal path
    !> NEEDS the column type to know what its elements are -- `(1, 2, 3)` is an integer list against
    !> an integer column and a floating-point one against a float column, exactly as the bare
    !> literal in `x == 1` is, and deciding that from the text alone would make `x in (1, 2)` fail
    !> on a column where `x == 1` works -- while WHERE those two tokens come from differs between
    !> the engines: the reader asks its file, the in-memory evaluator reads them off a column the
    !> table already classified.
    !>
    !> Reports every failure through ok/errmsg rather than aborting, for the same reason: the
    !> reader's caller appends its file name and the table's appends its own context, and the two
    !> messages are otherwise identical because they are the same string.
    subroutine parquet_resolve_set_payload(filter, column, value, col_type, col_shape, fam, keys, &
            ok, errmsg)
        type(parquet_filter), intent(in) :: filter !! the filter whose bound sets a `@name` refers to.
        character(len=*), intent(in) :: column !! the column the clause tests.
        character(len=*), intent(in) :: value !! the leaf's value text: `@name` or `(...)`.
        character(len=*), intent(in) :: col_type !! parquet_get_column_type's token for that column.
        character(len=*), intent(in) :: col_shape !! parquet_get_column_shape's token for that column.
        integer(int8), intent(out) :: fam !! the resolved FSET_* element family.
        type(pf_index_map), intent(out) :: keys !! the built key index, string-keyed for a string set.
        logical, intent(out) :: ok !! .false. on any resolution failure.
        character(len=:), allocatable, intent(out) :: errmsg !! that failure's message; "" when ok.
        type(parquet_string_column) :: members
        character(len=:), allocatable :: text
        integer :: set_index
        integer(int64) :: i, lo, hi
        logical :: is_literal

        ok = .false.
        errmsg = ""
        set_index = 0
        text = trim(adjustl(value))
        ! A leading '(' is what distinguishes the two spellings. The tokenizer refuses an empty
        ! value for `in`/`not_in`, so the length test can only fail if this is ever reached from
        ! somewhere that has not been through it.
        is_literal = .false.
        if (len(text) > 0) is_literal = (text(1:1) == "(")

        ! An UNBOUND NAME is reported before the file is consulted at all: it is a property of the
        ! filter object alone, so a rule naming a set nobody bound should say so rather than report
        ! whatever the column turns out to be. The literal path cannot be checked this early, since
        ! what its elements ARE depends on the column's type.
        if (.not. is_literal) then
            call parquet_resolve_set_name(filter, column, text, set_index, errmsg)
            if (set_index == 0) return
        end if

        call parquet_check_set_column_shape(column, col_shape, errmsg)
        if (len(errmsg) > 0) return
        if (is_literal) then
            call parquet_parse_literal_set(column, text, col_type, fam, keys, ok, errmsg)
            return
        end if

        fam = filter%set_family(set_index)
        call parquet_check_set_column_type(column, col_type, fam, errmsg)
        if (len(errmsg) > 0) return
        lo = filter%set_lo(set_index)
        hi = filter%set_hi(set_index)

        ! One build per leaf, not one per row group. The keys are unique by construction (%bind
        ! deduplicated them through %get_or_add), which is exactly %build's contract; a duplicate
        ! reaching here would abort inside pf_index_map naming the key -- loud, and the reason the
        ! dedup is at bind rather than tolerated here. Threading follows parquet_index's own rules.
        !
        ! An EMPTY set is built like any other: pf_index_map%build accepts a zero-length key array
        ! and every subsequent %get_many answers 0, so there is one code path rather than a
        ! short-circuit whose only effect would be to skip a lookup that already answers correctly.
        ! What the empty set then means falls out of the ordinary rules -- every non-null row is
        ! false under `in` and true under `not_in`, and every row group prunes -- which is exactly
        ! the behaviour test_in_empty_set and test_empty_set_prunes_everything pin.
        if (fam == FSET_TIMESTAMP) then
            ! The pairs were stored column-major by parquet_filter_store_pair_keys, so the reshape
            ! recovers exactly the `(n, 2)` tuples %get_or_add_many deduplicated at %bind.
            call keys%build(reshape(filter%set_keys(lo:hi), [(hi - lo + 1_int64) / 2_int64, 2_int64]))
        else if (fam /= FSET_STRING) then
            call keys%build(filter%set_keys(lo:hi))
        else
            ! The string set's members, gathered ONCE for the whole leaf into the map's own string
            ! build, which keys each by its exact bytes -- the same rule %bind deduplicated them
            ! under, so the strict `%build` contract holds here as it does for every other family.
            ! %append_from copies each member's bytes across without materialising a deferred-
            ! length string for it (feature_risks.md Risk-60).
            do i = lo, hi
                call members%append_from(filter%set_text, i)
            end do
            call keys%build(members)
        end if
        ok = .true.
    end subroutine parquet_resolve_set_payload

    !> Turns a literal list -- the `(1, 2, 3)` or `("a", "b")` written in the rule itself -- into the
    !> same family/key-index/string-column payload a bound set resolves to.
    !>
    !> **What the elements ARE is decided by the COLUMN, not by the text**, exactly as it is for the
    !> bare literal in `x == 1`: `(1, 2, 3)` is an integer list against an integer column and a
    !> floating-point one against a `float32`/`float64` column. Only quoting is read off the text,
    !> and it must agree with the column: every element quoted for a string column, none quoted for
    !> a numeric one.
    !>
    !> **The element grammar is deliberately NARROWER than the bare literal's**, which C++ parses
    !> with `strtoll`/`strtod`. An integer element is an optional sign then digits, which is exactly
    !> what `strtoll(.., 10)` accepts. A real element is an optional sign, then digits with an
    !> optional fraction and an optional `e` exponent, or `inf`/`infinity`; `strtod` also
    !> accepts a C99 hex float (`0x1p3`), which this does not. Narrower in the safe direction: a
    !> spelling this refuses is a clean parse error naming the element, never a different value.
    !> `nan` is refused for the reason `x == nan` is -- it could only ever match nothing.
    subroutine parquet_parse_literal_set(column, text, col_type, fam, keys, ok, errmsg)
        character(len=*), intent(in) :: column !! the column the clause tests.
        character(len=*), intent(in) :: text !! the list as written, parentheses included.
        character(len=*), intent(in) :: col_type !! parquet_get_column_type's token for that column.
        integer(int8), intent(out) :: fam !! the resolved FSET_* element family.
        type(pf_index_map), intent(out) :: keys !! the built key index, string-keyed for a string list.
        logical, intent(out) :: ok !! .false. on any parse or type failure.
        character(len=:), allocatable, intent(out) :: errmsg !! that failure's message; "" when ok.
        character(len=:), allocatable :: inner, elem
        character(len=32) :: pos_str
        integer :: lo, hi, nelem, depth, k
        integer(int64) :: ikey, idx
        real(real64) :: rval
        logical :: parsed, quoted

        ok = .false.
        errmsg = ""
        call parquet_set_family_for_column(col_type, fam)
        if (fam == FSET_NONE) then
            errmsg = "the list in the filter clause on column '" // trim(column) // &
                "' cannot be compared against that column, whose values are read as '" // &
                trim(col_type) // "'"
            return
        end if
        ! A temporal set is bound from ELEMENTS (%bind over a parquet_date/parquet_time/
        ! parquet_timestamp array), not written out: a literal member would need the column's
        ! stored unit to convert, which is the text path's job and not this list's. Refused with
        ! the route named rather than half-supported.
        if (fam == FSET_DATE .or. fam == FSET_TIME .or. fam == FSET_TIMESTAMP) then
            errmsg = "the list in the filter clause on column '" // trim(column) // &
                "' is not supported on a " // trim(col_type) // " column -- bind the members as " // &
                "an array of parquet_" // trim(col_type) // " elements with %bind and write '" // &
                trim(column) // " in @name' instead"
            return
        end if
        ! A string list starts a STRING-keyed map: %get_or_add over a string is the same
        ! dictionary-encoding primitive as over an integer, so every family below deduplicates
        ! the same way and leaves behind the map the evaluator looks up in.
        if (fam == FSET_STRING) then
            call keys%init(strings=.true.)
        else
            call keys%init()
        end if

        inner = text(2:len(text) - 1)
        if (len_trim(inner) == 0) then
            errmsg = "the filter clause on column '" // trim(column) // &
                "' has an empty list '()' -- a set that matches nothing is written by binding a " // &
                "zero-length array with %bind, so an empty list here is almost certainly a mistake"
            return
        end if

        ! One pass, splitting on the commas OUTSIDE any quoted element -- a quoted member may
        ! contain a comma, a parenthesis or a keyword, which is what makes the quotes load-bearing
        ! rather than decoration.
        nelem = 0
        lo = 1
        depth = 0
        do k = 1, len(inner) + 1
            if (k <= len(inner)) then
                if (inner(k:k) == '"') depth = 1 - depth
                if (inner(k:k) /= "," .or. depth /= 0) cycle
            end if
            hi = k - 1
            nelem = nelem + 1
            elem = trim(adjustl(inner(lo:hi)))
            write (pos_str, '(i0)') nelem
            if (len(elem) == 0) then
                errmsg = "element " // trim(pos_str) // " of the list in the " // &
                    "filter clause on column '" // trim(column) // "' is empty (a stray or trailing " // &
                    "comma?)"
                return
            end if
            quoted = (elem(1:1) == '"')
            if (quoted .neqv. (fam == FSET_STRING)) then
                if (quoted) then
                    errmsg = "element " // trim(pos_str) // " of the list in the " // &
                        "filter clause on column '" // trim(column) // "' is quoted, but that column's " // &
                        "values are read as '" // trim(col_type) // "' -- write each member as a bare " // &
                        "number"
                    return
                end if
                errmsg = "element " // trim(pos_str) // " of the list in the " // &
                    "filter clause on column '" // trim(column) // "' is not quoted, but that column's " // &
                    "values are read as '" // trim(col_type) // "' -- write each member in double " // &
                    "quotes"
                return
            end if
            select case (fam)
            case (FSET_INT)
                call parquet_parse_set_int(elem, ikey, parsed)
                if (.not. parsed) then
                    errmsg = "element " // trim(pos_str) // " of the list in the " // &
                        "filter clause on column '" // trim(column) // "' is '" // elem // &
                        "', which is not a whole number"
                    return
                end if
                call keys%get_or_add(ikey, idx)
            case (FSET_REAL)
                call parquet_parse_set_real(elem, rval, parsed)
                if (.not. parsed) then
                    errmsg = "element " // trim(pos_str) // " of the list in the " // &
                        "filter clause on column '" // trim(column) // "' is '" // elem // &
                        "', which is not a number (a hexadecimal float is not accepted here, and " // &
                        "'nan' can never match -- say 'is_nan' instead)"
                    return
                end if
                call keys%get_or_add(parquet_filter_real_key(rval), idx)
            case default
                ! The quotes are stripped here and nowhere else, so an element is keyed exactly as
                ! the caller wrote it between them -- spaces included, and with no trimming, which
                ! is what makes a quoted member comparable against a string column's own bytes
                ! (the map's scalar string forms take a key as written).
                if (len(elem) < 2 .or. elem(len(elem):len(elem)) /= '"') then
                    errmsg = "element " // trim(pos_str) // " of the list in the " // &
                        "filter clause on column '" // trim(column) // "' is missing its closing quote"
                    return
                end if
                call keys%get_or_add(elem(2:len(elem) - 1), idx)
            end select
            lo = k + 1
        end do
        ok = .true.
    end subroutine parquet_parse_literal_set

    !> Parses a strictly-formatted whole number: an optional sign, then digits, then nothing else.
    !>
    !> That is exactly the grammar `strtoll(.., 10)` accepts for the bare literal in `x == 1`, so
    !> `x in (1, 2)` and `x == 1 or x == 2` agree by construction rather than by luck.
    !>
    !> A list-directed `read(text, *, iostat=)` is NOT strict enough to do this on its own -- it
    !> accepts "5 6" with iostat 0, quietly yielding 5 (see CLAUDE.md and env_int64) -- so the shape
    !> is checked by hand and the `read` only runs once it is known to be sound, which also leaves
    !> `iostat` free to report the one thing the hand check cannot: an out-of-range value.
    pure subroutine parquet_parse_set_int(text, value, ok)
        character(len=*), intent(in) :: text !! one element of a literal list, already trimmed.
        integer(int64), intent(out) :: value !! the parsed number; 0 when ok is .false.
        logical, intent(out) :: ok !! .true. if the whole string is a whole number.
        integer :: k, first, ios

        value = 0_int64
        first = 1
        if (text(1:1) == "+" .or. text(1:1) == "-") first = 2
        ok = len(text) >= first
        do k = first, len(text)
            if (text(k:k) < "0" .or. text(k:k) > "9") then
                ok = .false.
                exit
            end if
        end do
        if (.not. ok) return
        read (text, *, iostat=ios) value
        ok = (ios == 0)
        if (.not. ok) value = 0_int64
    end subroutine parquet_parse_set_int

    !> Parses a strictly-formatted real: an optional sign, then either `inf`/`infinity` or digits
    !> with an optional fraction and an optional `e`/`E` exponent, then nothing else.
    !>
    !> Refuses `nan` along with everything else it does not recognise, for the reason the text path
    !> refuses `x == nan`: every comparison against a NaN is false, so a NaN member could only ever
    !> match nothing. The caller's message says so; there is no separate NaN outcome here, because
    !> a set that cannot contain a NaN is exactly what makes the NaN row rule hold by construction
    !> (feature_risks.md R-j).
    pure subroutine parquet_parse_set_real(text, value, ok)
        character(len=*), intent(in) :: text !! one element of a literal list, already trimmed.
        real(real64), intent(out) :: value !! the parsed number; 0 when ok is .false.
        logical, intent(out) :: ok !! .true. if the whole string is a real number or an infinity.
        character(len=:), allocatable :: low
        integer :: k, first, ndigit, ios
        logical :: negative

        value = 0.0_real64
        ok = .false.
        low = text
        do k = 1, len(low)
            if (low(k:k) >= "A" .and. low(k:k) <= "Z") low(k:k) = achar(iachar(low(k:k)) + 32)
        end do
        first = 1
        negative = .false.
        if (low(1:1) == "+" .or. low(1:1) == "-") then
            negative = (low(1:1) == "-")
            first = 2
        end if
        if (len(low) < first) return

        if (low(first:) == "inf" .or. low(first:) == "infinity") then
            value = ieee_value(0.0_real64, ieee_positive_inf)
            if (negative) value = -value
            ok = .true.
            return
        end if

        ! digits [ . digits ] [ e [sign] digits ] -- and at least one digit in the mantissa, so
        ! ".", "+" and "e5" are all refused rather than read as 0. The exponent letter is `e`
        ! only, never Fortran's own `d`: strtod does not accept `1d3` for the bare literal in
        ! `x == 1d3`, and accepting it here would make the list the more permissive of the two.
        !
        ! Neither check is redundant with `read`'s own iostat, and the digit one was measured
        ! rather than assumed: gfortran 15.2 and nagfor 7.2 both reject "." with a nonzero iostat,
        ! and FLANG 22.1.8 accepts it as 0.0 with iostat 0 -- so without this the element `.`
        ! would silently mean 0.0 on one compiler in the fleet and abort on the others. That is
        ! exactly why CLAUDE.md forbids leaning on a list-directed read for strictness.
        k = first
        ndigit = 0
        do while (k <= len(low))
            if (low(k:k) < "0" .or. low(k:k) > "9") exit
            ndigit = ndigit + 1
            k = k + 1
        end do
        if (k <= len(low)) then
            if (low(k:k) == ".") then
                k = k + 1
                do while (k <= len(low))
                    if (low(k:k) < "0" .or. low(k:k) > "9") exit
                    ndigit = ndigit + 1
                    k = k + 1
                end do
            end if
        end if
        if (ndigit == 0) return
        if (k <= len(low)) then
            if (low(k:k) /= "e") return
            k = k + 1
            if (k <= len(low)) then
                if (low(k:k) == "+" .or. low(k:k) == "-") k = k + 1
            end if
            if (k > len(low)) return
            do while (k <= len(low))
                if (low(k:k) < "0" .or. low(k:k) > "9") return
                k = k + 1
            end do
        end if

        read (low, *, iostat=ios) value
        ok = (ios == 0)
        if (.not. ok) value = 0.0_real64
    end subroutine parquet_parse_set_real

    !> Evaluates one `in`/`not_in` leaf against `rdr`'s key column, ONE ROW GROUP AT A TIME, writing
    !> one Kleene verdict per PHYSICAL row into `verdicts` and this leaf's three screen flags per
    !> row group into `flags`.
    !>
    !> **One row group at a time is what keeps `bounded=.true.` bounded**: peak memory here is one
    !> row group of one column, plus the set and the verdict array. A whole-column read would defeat
    !> the entire purpose of the clause.
    !>
    !> **`not_in` is negated HERE, not in C++.** The verdicts written for a `not_in` leaf are already
    !> its own answer, so the C++ side treats a pre-evaluated leaf as an opaque source of Kleene
    !> values whichever operator produced it, and the screen flags -- read off these same verdicts --
    !> are consistent with them for free. An UNKNOWN row stays unknown under the negation, which is
    !> the whole point of the third state: a null row is not admitted by `not_in` any more than by
    !> `in`. An enclosing `not` node still negates on the C++ side through the ordinary Kleene
    !> negation, so `not (x not_in @s)` and `x in @s` agree.
    !>
    !> **The screen flags are exact and need no statistics.** Every verdict is known before the
    !> screen runs, so a row group's KleenePossible is read off its own segment rather than inferred
    !> from min/max: may_true if any row of the segment is true, and so on. That prunes on a string
    !> column, on a file written without statistics at all, and on a scattered set whose members
    !> fall in the gaps between row groups' ranges -- none of which a bound-based leaf can do. See
    !> feature_risks.md R-a: a MIS-SLICED segment here prunes a row group that holds members, and
    !> the answer stays a perfectly valid row set with nothing to report it.
    subroutine parquet_evaluate_set_leaf(rdr, context, column, fam, keys, negate, &
            total_nrows, num_row_groups, verdicts, flags)
        type(parquet_reader), intent(in) :: rdr !! the private, transform-free reader over the file.
        character(len=*), intent(in) :: context !! calling procedure's name, used in every error-stop message.
        character(len=*), intent(in) :: column !! the column the clause tests.
        integer(int8), intent(in) :: fam !! the set's FSET_* element family.
        type(pf_index_map), intent(in) :: keys !! the built key index, string-keyed for a string set.
        logical, intent(in) :: negate !! .true. for a `not_in` leaf: true and false are exchanged.
        integer(int64), intent(in) :: total_nrows !! the file's physical row count.
        integer(int64), intent(in) :: num_row_groups !! the file's row-group count.
        integer(int8), intent(out) :: verdicts(:) !! one Kleene value per physical row; length total_nrows.
        integer(int8), intent(out) :: flags(:) !! (may_true, may_false, may_unknown) per row group, in that order.
        integer(int64) :: rg, base, rows, i
        logical :: any_t, any_f, any_u

        base = 0_int64
        do rg = 1_int64, num_row_groups
            call parquet_get_chunk_size(rdr, rows, rg)
            if (rows > 0_int64) then
                call parquet_answer_set_chunk(rdr, column, rg, rows, fam, keys, &
                    negate, verdicts(base + 1_int64:base + rows))
            end if
            ! This row group's three screen flags, read off the segment just written. A row group
            ! with no rows can be nothing, so all three stay false and it prunes -- which is what an
            ! empty row group already did on every other path.
            any_t = .false.
            any_f = .false.
            any_u = .false.
            do i = base + 1_int64, base + rows
                select case (verdicts(i))
                case (KL_TRUE)
                    any_t = .true.
                case (KL_FALSE)
                    any_f = .true.
                case default
                    any_u = .true.
                end select
                if (any_t .and. any_f .and. any_u) exit
            end do
            flags(3_int64 * (rg - 1_int64) + 1_int64) = merge(1_int8, 0_int8, any_t)
            flags(3_int64 * (rg - 1_int64) + 2_int64) = merge(1_int8, 0_int8, any_f)
            flags(3_int64 * (rg - 1_int64) + 3_int64) = merge(1_int8, 0_int8, any_u)
            base = base + rows
        end do

        if (base /= total_nrows) then
            ! Unreachable: a file's row groups sum to its row count by construction. Kept because
            ! the whole design rests on this array being indexed by PHYSICAL row, and a silent short
            ! fill would misalign every row group after the shortfall -- exactly the coordinate-
            ! system failure feature_risks.md R-b describes, whose symptom is a plausible wrong
            ! row set rather than an error.
            error stop trim(context) // ": internal error -- the row groups of '" // trim(column) // &
                "' do not sum to the file's row count while evaluating a set-valued filter clause"
        end if
    end subroutine parquet_evaluate_set_leaf

    !> Refuses a set clause on a column that is not a plain scalar, BEFORE any chunk is read.
    !>
    !> parquet_reader_set_filter refuses the same columns from the schema, but it does so after this
    !> pre-evaluation has already run -- so without this check the caller's abort comes from the
    !> chunk reader ("type mismatch for column: vec"), which says nothing about the filter, and
    !> arrives as a C++ fatal error rather than a clean Fortran one. The wording matches
    !> container_shape_word's in parquet_wrapper.cpp so the message reads the same whichever guard
    !> reports it.
    subroutine parquet_check_set_column_shape(column, col_shape, errmsg)
        character(len=*), intent(in) :: column !! the column the clause tests.
        character(len=*), intent(in) :: col_shape !! parquet_get_column_shape's token for that column.
        character(len=:), allocatable, intent(out) :: errmsg !! the refusal; "" when the column is scalar.
        character(len=:), allocatable :: word

        errmsg = ""
        if (col_shape == "scalar") return
        select case (col_shape)
        case ("vector")
            word = "vector"
        case ("list")
            word = "variable-length list"
        case ("map")
            word = "map"
        case ("struct")
            word = "struct"
        case default
            word = trim(col_shape)
        end select
        errmsg = "filter column '" // trim(column) // "' is a " // word // &
            " column; filtering only supports scalar columns"
    end subroutine parquet_check_set_column_shape

    !> Refuses a bound set whose element family cannot be compared against `column`'s type, with the
    !> same reasoning the text path applies to a mistyped literal.
    !>
    !> An integer set is accepted against any integer column (widened to int64); a real set against
    !> the families the text path also compares as doubles; a string set against a string column; a
    !> date, time or timestamp set against a column of exactly that temporal type (a date set
    !> against a timestamp column is refused, as a too-imprecise literal would not be -- the set's
    !> members are elements, not text, and an element of the wrong type has no instant to convert).
    !> A boolean column is refused because a boolean set is `==` with extra steps; a vector, list,
    !> map or struct column is refused for the reason every filter column is -- there is no single
    !> value per row to compare, which parquet_reader_set_filter also refuses from the schema.
    subroutine parquet_check_set_column_type(column, col_type, family, errmsg)
        character(len=*), intent(in) :: column !! the column the clause tests.
        character(len=*), intent(in) :: col_type !! parquet_get_column_type's token for that column.
        integer(int8), intent(in) :: family !! the bound set's FSET_* family.
        character(len=:), allocatable, intent(out) :: errmsg !! the refusal; "" when the families agree.
        character(len=:), allocatable :: word
        integer(int8) :: want

        errmsg = ""
        call parquet_set_family_for_column(col_type, want)
        if (family == want) return
        call parquet_set_family_word(family, word)
        errmsg = "the '" // word // &
            "' set in the filter clause on column '" // trim(column) // "' cannot be compared " // &
            "against that column, whose values are read as '" // trim(col_type) // "'"
    end subroutine parquet_check_set_column_type

    !> The one FSET_* family a column of type `col_type` can hold a set clause on, or FSET_NONE;
    !> see the interface in parquet_core.f90 for why this is the SINGLE definition of the rule.
    module procedure parquet_set_family_for_column
        select case (trim(col_type))
        case ("int32", "int64")
            family = FSET_INT
        case ("float32", "float64")
            family = FSET_REAL
        case ("string")
            family = FSET_STRING
        case ("date")
            family = FSET_DATE
        case ("time")
            family = FSET_TIME
        case ("timestamp")
            family = FSET_TIMESTAMP
        case default
            ! Boolean is refused because a boolean set is `==` with extra steps; a vector, list,
            ! map or struct column is refused for the reason every filter column is -- there is no
            ! single value per row to compare, which parquet_reader_set_filter also refuses from
            ! the schema.
            family = FSET_NONE
        end select
    end procedure parquet_set_family_for_column

    !> The word an error message uses for a bound set's element family. A subroutine for the same
    !> reason parquet_lower_op is.
    pure subroutine parquet_set_family_word(family, word)
        integer(int8), intent(in) :: family !! the set's FSET_* family.
        character(len=:), allocatable, intent(out) :: word !! a short adjective naming that family.
        select case (family)
        case (FSET_INT)
            word = "integer"
        case (FSET_REAL)
            word = "floating-point"
        case (FSET_DATE)
            word = "date"
        case (FSET_TIME)
            word = "time"
        case (FSET_TIMESTAMP)
            word = "timestamp"
        case default
            word = "string"
        end select
    end subroutine parquet_set_family_word

    !> Answers one row group's rows of a set-valued leaf: reads the key column's chunk, converts each
    !> row to the set's own key, and writes one Kleene verdict per row.
    !>
    !> A NULL row is KL_UNKNOWN, exactly as under every comparison -- `is_null` stays the only way a
    !> null row enters a result, and the negation `not_in` applies leaves unknown alone. A NaN row
    !> answers KL_FALSE with no special case at all, because no NaN pattern is ever a key (%bind
    !> refuses one), so its lookup finds nothing: the filter's NaN rule holding by construction
    !> rather than by a check that could be forgotten on one engine. See feature_risks.md R-j.
    subroutine parquet_answer_set_chunk(rdr, column, row_group, rows, fam, keys, &
            negate, out)
        type(parquet_reader), intent(in) :: rdr !! the private reader over the file.
        character(len=*), intent(in) :: column !! the column the clause tests.
        integer(int64), intent(in) :: row_group !! 1-based row group to answer.
        integer(int64), intent(in) :: rows !! that row group's row count.
        integer(int8), intent(in) :: fam !! the set's FSET_* element family.
        type(pf_index_map), intent(in) :: keys !! the built key index, string-keyed for a string set.
        logical, intent(in) :: negate !! .true. for a `not_in` leaf: true and false are exchanged.
        integer(int8), intent(out) :: out(:) !! this row group's Kleene verdicts, one per row.
        integer(int64), allocatable :: ivals(:), found(:), pairs(:, :)
        real(real64), allocatable :: rvals(:)
        type(parquet_string_column) :: svals
        type(parquet_date), allocatable :: dvals(:)
        type(parquet_time), allocatable :: tvals(:)
        type(parquet_timestamp), allocatable :: tsvals(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i

        select case (fam)
        case (FSET_INT)
            allocate(ivals(rows), valid(rows), found(rows))
            call parquet_read_column_chunk(rdr, column, row_group, ivals, null_value=0_int64, is_valid=valid)
            call keys%get_many(ivals, found)
            do i = 1_int64, rows
                out(i) = parquet_set_verdict(valid(i), found(i) > 0_int64, negate)
            end do
        case (FSET_REAL)
            allocate(rvals(rows), valid(rows), found(rows))
            call parquet_read_column_chunk(rdr, column, row_group, rvals, null_value=0.0_real64, is_valid=valid)
            call keys%get_many(parquet_filter_real_key(rvals), found)
            do i = 1_int64, rows
                out(i) = parquet_set_verdict(valid(i), found(i) > 0_int64, negate)
            end do
        case (FSET_DATE)
            ! A temporal element carries its own null, so the mask is read off the elements rather
            ! than through an is_valid= argument; a null keys as 0 and is masked before the lookup,
            ! so it can never meet a genuine 1970-01-01 in the set.
            allocate(dvals(rows), valid(rows), found(rows))
            call parquet_read_column_chunk(rdr, column, row_group, dvals)
            valid = .not. dvals%is_null()
            call keys%get_many(parquet_date_key(dvals), found, valid=valid)
            do i = 1_int64, rows
                out(i) = parquet_set_verdict(valid(i), found(i) > 0_int64, negate)
            end do
        case (FSET_TIME)
            allocate(tvals(rows), valid(rows), found(rows))
            call parquet_read_column_chunk(rdr, column, row_group, tvals)
            valid = .not. tvals%is_null()
            call keys%get_many(parquet_time_key(tvals), found, valid=valid)
            do i = 1_int64, rows
                out(i) = parquet_set_verdict(valid(i), found(i) > 0_int64, negate)
            end do
        case (FSET_TIMESTAMP)
            ! The (seconds, nanoseconds) pair is unit-free, so a set bound from elements at one
            ! unit matches this column at another by instant -- S7's F4 property, held here by
            ! the element normalising the unit away before the key is ever taken.
            allocate(tsvals(rows), valid(rows), found(rows), pairs(rows, 2))
            call parquet_read_column_chunk(rdr, column, row_group, tsvals)
            valid = .not. tsvals%is_null()
            call parquet_timestamp_key(tsvals, pairs(:, 1), pairs(:, 2))
            call keys%get_many(pairs, found, valid=valid)
            do i = 1_int64, rows
                out(i) = parquet_set_verdict(valid(i), found(i) > 0_int64, negate)
            end do
        case default
            ! The string family: one %get_many over the row group's string column, read in place
            ! by the map's own string form, which keys each element by its exact bytes and answers
            ! 0 for a null one -- the same call every other family makes, where this used to be a
            ! pf_in sort-merge that re-sorted the set once per row group.
            allocate(found(rows))
            call parquet_read_column_chunk(rdr, column, row_group, svals)
            call keys%get_many(svals, found)
            do i = 1_int64, rows
                out(i) = parquet_set_verdict(.not. svals%is_null(i), found(i) > 0_int64, negate)
            end do
        end select
    end subroutine parquet_answer_set_chunk

    !> One row's Kleene verdict under a set-valued clause. The one place the null rule, the
    !> membership answer and the `not_in` negation meet, so the three cannot disagree between the
    !> element families that each reach it by a different route.
    pure integer(int8) function parquet_set_verdict(valid, found, negate) result(res)
        logical, intent(in) :: valid !! .false. when the row is Null.
        logical, intent(in) :: found !! .true. when the row's value is a member of the set.
        logical, intent(in) :: negate !! .true. for a `not_in` leaf.
        if (.not. valid) then
            res = KL_UNKNOWN
        else if (found .neqv. negate) then
            res = KL_TRUE
        else
            res = KL_FALSE
        end if
    end function parquet_set_verdict

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
        integer :: nnodes, nleaves
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).
        integer(int32), allocatable :: leaf_pre(:) !! per leaf: 0, or its 1-based pre-evaluated index.
        integer(int8), allocatable :: pre_verdicts(:), pre_flags(:) !! the pre-evaluated leaves' payload.
        integer(int64) :: n_pre, pre_rows, pre_groups !! that payload's shape; all 0 with no set clause.

        call parquet_parse_filter_rules(filter, node_kind, node_leaf, nnodes, leaf_name, &
            leaf_op, leaf_value, leaf_is_string, nleaves, ok, errmsg)
        if (.not. ok) then
            call reader_filename_suffix(reader, name_suffix)
            error stop trim(context) // ": invalid filter rule: " // errmsg // name_suffix
        end if
        ! A rule-less filter still has something to install when a row-group scope or a physical
        ! row range was given: an all-true-within-range mask, which is how a slice-regime table
        ! carrying only sample_fraction= expresses its own bounds. With no rules AND no bounds
        ! there is genuinely nothing to do. `row_group_lo <= 0` covers both no_row_group_scope
        ! (-1, no range named) and 0 (all row groups): with no clauses, "every row of every row
        ! group" is what an absent filter already gives, whichever engine would have run.
        if (nleaves == 0 .and. row_group_lo <= 0 .and. row_group_hi <= 0 .and. &
                row_lo <= 0 .and. row_hi <= 0) return

        call convert_temporal_filter_values(reader, context, leaf_name, leaf_op, leaf_value, leaf_is_string, nleaves)
        call parquet_render_filter_expr(node_kind, node_leaf, nnodes, leaf_name, leaf_op, leaf_value, &
            leaf_is_string, nleaves, expr_text)
        ! Every `in`/`not_in` leaf is answered HERE, before the filter is installed and before C++
        ! reads anything: one row group of the key column at a time, through the library's own key
        ! index. What crosses the boundary below is the result -- one Kleene value per PHYSICAL row
        ! per such leaf, plus its three screen flags per row group -- never the set itself. The
        ! leaf's TEXT still crosses unchanged, so expr_text, print_stat and %remap_column_names see
        ! an ordinary clause. A filter with no set clause allocates nothing and opens no file.
        call parquet_prepare_set_leaves(reader, filter, context, leaf_name, leaf_op, leaf_value, &
            nleaves, leaf_pre, n_pre, pre_rows, pre_groups, pre_verdicts, pre_flags)

        call pack_fixed_width_strings(leaf_name(1:nleaves), names_packed)
        call pack_fixed_width_strings(leaf_op(1:nleaves), ops_packed)
        call pack_fixed_width_strings(leaf_value(1:nleaves), values_packed)

        c_err = ""
        status = c_reader_set_filter(reader%handle, names_packed, int(filter_leaf_name_len, kind=c_long_long), &
            ops_packed, int(filter_leaf_op_len, kind=c_long_long), values_packed, &
            int(filter_leaf_value_len, kind=c_long_long), leaf_is_string(1:nleaves), &
            int(nleaves, kind=c_long_long), node_kind(1:nnodes), node_leaf(1:nnodes), &
            int(nnodes, kind=c_long_long), expr_text//char(0), row_group_lo, row_group_hi, &
            row_lo, row_hi, leaf_pre(1:max(nleaves, 1)), n_pre, pre_rows, pre_groups, &
            pre_verdicts, pre_flags, c_err, int(len(c_err), kind=c_long_long))

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
    subroutine parquet_apply_sort(reader, sort_by, context, keep_cache)
        type(parquet_reader), intent(inout) :: reader !! open reader the sort is applied to.
        type(parquet_sortkey), intent(in) :: sort_by !! keys to parse, validate and apply.
        character(len=*), intent(in) :: context !! calling procedure's name, used in every error-stop message.
        logical, intent(in), optional :: keep_cache !! .true. keeps the decoded key columns, sorted in place.
        character(len=sort_key_name_len), allocatable :: key_name(:)
        integer(int8), allocatable :: descending(:), nulls_first(:)
        character(len=:), allocatable :: name, errmsg, key_text, name_suffix
        logical :: ok, desc
        character(len=1024) :: c_err
        integer(c_long_long) :: status
        integer :: i
        type(pf_sort_keys) :: skeys
        integer(int64), allocatable :: perm(:)
        integer(int64) :: nrows
        integer(c_long_long) :: keep

        if (sort_by%n == 0) return
        ! Default 0: the key columns this sort is about to decode are RELEASED once the permutation
        ! exists, so nothing pays to reorder a column the caller may never read. Only an open that
        ! also prefetches asks for them to be kept -- see parquet_reader_sort_install's own note.
        keep = 0_c_long_long
        if (present(keep_cache)) then
            if (keep_cache) keep = 1_c_long_long
        end if
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
        call reader_filename_suffix(reader, name_suffix)

        ! Every key is pulled across, reduced, and appended in order of precedence; the C++ side
        ! still owns the decode and the Arrow type reduction, so which columns are sortable and
        ! what a refusal says are unchanged by the engine swap.
        nrows = -1_int64
        do i = 1, sort_by%n
            call add_read_sort_key(reader, trim(key_name(i)), descending(i), nulls_first(i), &
                trim(context) // name_suffix, skeys, nrows)
        end do

        allocate(perm(nrows))
        call pf_argsort(skeys, perm)
        ! pf_argsort produces 1-based indices; Arrow's Take consumes 0-based ones. Done here rather
        ! than in C++ so the conversion sits next to the call that creates the obligation.
        perm = perm - 1_int64

        c_err = ""
        status = parquet_reader_sort_install(reader%handle, perm, int(nrows, kind=c_long_long), &
            key_text//char(0), keep, c_err, int(len(c_err), kind=c_long_long))
        if (status /= 0) error stop trim(context) // ": " // trim(c_err) // name_suffix
    end subroutine parquet_apply_sort
    !> Pulls one read-time sort key across the bind(C) boundary and appends it to `skeys`.
    !>
    !> **Two calls per key, `_info` then `_fetch`.** Only the C++ side knows which of the three
    !> reduced families a column lands in -- boolean and every temporal type arrive as integers --
    !> so the sizes have to come back before the buffers can exist. Both calls bind the key; the
    !> decode behind them does not repeat, because `get_single_chunk_array` serves the second from
    !> the reader's column cache. See `parquet_wrapper.cpp` for why that repeat is preferred over
    !> staging the bound key on the reader handle.
    !>
    !> **The local buffers do not have to outlive this call**: `%add` EXTRACTS into the key list's
    !> own `sort_key_buf` rather than retaining what it was handed.
    subroutine add_read_sort_key(reader, name, descending, nulls_first, context, skeys, nrows)
        type(parquet_reader), intent(in) :: reader !! open reader the key column is read from.
        character(len=*), intent(in) :: name !! key column, possibly a dotted struct-leaf path.
        integer(int8), intent(in) :: descending !! nonzero for descending order.
        integer(int8), intent(in) :: nulls_first !! nonzero to place this key's nulls first.
        character(len=*), intent(in) :: context !! caller's name plus file suffix, for messages.
        type(pf_sort_keys), intent(inout) :: skeys !! key list this key is appended to.
        integer(int64), intent(inout) :: nrows !! row count; set from the first key, then reused.
        integer(c_int) :: family
        integer(c_long_long) :: nk, nbytes, status
        integer(c_int8_t) :: has_nulls
        character(len=1024) :: c_err
        integer(int64), allocatable, target :: iv(:), off(:)
        real(real64), allocatable, target :: rv(:)
        integer(int8), allocatable, target :: valid(:)
        character(kind=c_char), allocatable, target :: dat(:)
        logical, allocatable :: mask(:)
        type(parquet_string_column) :: scol
        integer(int64) :: k
        logical :: desc, nlo

        desc = descending /= 0_int8
        nlo = nulls_first /= 0_int8

        c_err = ""
        status = parquet_reader_sort_key_info(reader%handle, name//char(0), descending, nulls_first, &
            family, nk, nbytes, has_nulls, c_err, int(len(c_err), kind=c_long_long))
        if (status /= 0) error stop trim(context) // ": " // trim(c_err)
        if (nrows < 0_int64) nrows = int(nk, int64)

        ! Allocated unconditionally, because c_loc of an unallocated array is not a thing that can
        ! be passed; the C++ side writes it only when the key really carries nulls. One byte per
        ! row, against the alternative of branching the fetch call four ways.
        allocate(valid(max(nk, 1_c_long_long)))

        select case (family)
        case (0)
            allocate(iv(max(nk, 1_c_long_long)))
            status = parquet_reader_sort_key_fetch(reader%handle, name//char(0), descending, nulls_first, &
                c_loc(iv), c_null_ptr, c_null_ptr, c_null_ptr, c_loc(valid), c_err, &
                int(len(c_err), kind=c_long_long))
            if (status /= 0) error stop trim(context) // ": " // trim(c_err)
            call build_valid_mask(valid, nk, has_nulls, mask)
            ! `mask` is unallocated when the key has no nulls, and an unallocated allocatable
            ! passed to an optional dummy is ABSENT (F2018 15.5.2.12) -- which is what puts the
            ! engine on its no-mask fast path instead of walking an all-true mask.
            call skeys%add(iv(1:nk), descending=desc, nulls_first=nlo, is_valid=mask)
        case (1)
            allocate(rv(max(nk, 1_c_long_long)))
            status = parquet_reader_sort_key_fetch(reader%handle, name//char(0), descending, nulls_first, &
                c_null_ptr, c_loc(rv), c_null_ptr, c_null_ptr, c_loc(valid), c_err, &
                int(len(c_err), kind=c_long_long))
            if (status /= 0) error stop trim(context) // ": " // trim(c_err)
            call build_valid_mask(valid, nk, has_nulls, mask)
            call skeys%add(rv(1:nk), descending=desc, nulls_first=nlo, is_valid=mask)
        case default
            allocate(off(0:max(nk, 1_c_long_long)), dat(max(nbytes, 1_c_long_long)))
            status = parquet_reader_sort_key_fetch(reader%handle, name//char(0), descending, nulls_first, &
                c_null_ptr, c_null_ptr, c_loc(off), c_loc(dat), c_loc(valid), c_err, &
                int(len(c_err), kind=c_long_long))
            if (status /= 0) error stop trim(context) // ": " // trim(c_err)
            ! Built through the buffer handoff rather than a character array, so a value's exact
            ! bytes survive: a fixed-width character array would blank-pad, and a string with real
            ! trailing spaces would then order as though it did not have them.
            call scol%append_buffers(int(nk, int64), int(nbytes, int64), c_loc(off), c_loc(dat), &
                c_null_ptr, .false.)
            ! `add_strcol` takes no is_valid -- a string column carries its own null state -- so
            ! the nulls go onto the column instead of alongside it.
            if (has_nulls /= 0_c_int8_t) then
                do k = 1_int64, int(nk, int64)
                    if (valid(k) == 0_int8) call scol%set_null(k)
                end do
            end if
            call skeys%add(scol, descending=desc, nulls_first=nlo)
        end select
    end subroutine add_read_sort_key
    !> Turns the C++ side's int8 validity flags into the `logical` mask `%add` takes, leaving the
    !> result UNALLOCATED when the key carries no nulls -- see the call sites for why that matters.
    subroutine build_valid_mask(valid, nk, has_nulls, mask)
        integer(int8), intent(in) :: valid(:) !! per element: 0 marks a null.
        integer(c_long_long), intent(in) :: nk !! number of key elements.
        integer(c_int8_t), intent(in) :: has_nulls !! nonzero when the key carries any null.
        logical, allocatable, intent(out) :: mask(:) !! the mask, or unallocated when null-free.

        if (has_nulls == 0_c_int8_t) return
        allocate(mask(nk))
        mask = valid(1:nk) /= 0_int8
    end subroutine build_valid_mask
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
            ! Every valueless operator is skipped, not just the two null tests: the four
            ! value-class operators carry no literal to convert, and letting one through here would
            ! report a temporal column's missing ISO-8601 literal instead of the real reason (the
            ! C++ side rejects them on any non-floating-point column, with a message naming that).
            if (op == "is_null" .or. op == "is_not_null" .or. op == "is_nan" .or. op == "is_not_nan" .or. &
                op == "is_finite" .or. op == "is_not_finite") cycle
            ! A set-valued clause carries a set, not a literal, so there is nothing here to convert
            ! either -- and letting one through would report a temporal column's missing ISO-8601
            ! literal instead of the real reason, which parquet_check_set_column_type states
            ! (a temporal set arrives with the temporal index key, not before).
            if (parquet_op_is_set_valued(op)) cycle
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
        logical :: keep_sorted_cache !! whether the sort's key columns stay resident (prefetch only).
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        ! Refresh the C++ side's copy of every mirrored setting before any C++ state exists.
        ! No setter mirrors to C++ any more -- that is what let each knob's setter live beside its
        ! state in parquet_settings_base, where an Arrow-free module can re-export it -- so this is
        ! where the mirror is made current. See parquet_push_settings_to_cpp's own doc-comment.
        call parquet_push_settings_to_cpp()

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
        ! keep_cache is the prefetch flag: with a prefetch coming, releasing the key columns would
        ! only make that prefetch decode them again. Nested rather than `.and.`-ed, because Fortran
        ! does not short-circuit and `prefetch` may be absent.
        if (present(sort_by)) then
            keep_sorted_cache = .false.
            if (present(prefetch)) keep_sorted_cache = prefetch
            call parquet_apply_sort(reader, sort_by, "parquet_open_reader", keep_cache=keep_sorted_cache)
        end if

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
        ! existing sort would leave the two describing different row sets. **This check is now the
        ! ONLY one that refuses sort-then-filter**: it used to be backed up by the decoded-columns
        ! guard below, because applying a sort left its key columns in the cache, and
        ! parquet_reader_sort_install now releases them instead. That made a redundant guard into
        ! a load-bearing one -- do not reorder these two, and do not delete this one on the
        ! grounds that the next one would catch it.
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

        ! A DESCENT path names a container's CHILD array, whose length is the flattened element
        ! count rather than the file's row count -- `list_of_struct[]` over a 3-row list holding
        ! 0+1+2 elements has four entries. Comparing such a read against parquet_get_nrows would
        ! compare two different quantities and reject a correct read, so the length is asked for
        ! at the path itself. An ordinary name answers identically either way; the branch exists
        ! only to keep the common case free of a C++ crossing it never needed.
        ! See feature_container_phase7.md's D6.
        if (index(name, "[]") > 0 .or. index(name, "{value}") > 0 .or. index(name, "{key}") > 0) then
            file_nrows = parquet_reader_path_nrows(reader%handle, trim(name)//char(0), 0_c_long_long)
        else
            file_nrows = parquet_reader_get_nrows(reader%handle)
        end if
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
    module procedure parquet_get_column_shape
        character(len=16) :: buf

        call check_reader_open(reader, "parquet_get_column_shape")
        ! Same contract as parquet_get_column_type: a name that does not exist is a caller mistake
        ! and aborts, but a SHAPE this library cannot read is an ANSWER ("map", "struct",
        ! "unknown") rather than an error, so there is nothing for this call to report but the
        ! token itself.
        call check_column_exists(reader, name, "parquet_get_column_shape")
        call parquet_reader_get_column_shape_name(reader%handle, trim(name)//char(0), buf, &
            int(len(buf), kind=c_long_long))
        shape = trim(buf)
    end procedure parquet_get_column_shape
    module procedure parquet_get_map_value_type
        character(len=16) :: buf
        integer(c_long_long) :: recognized

        call check_reader_open(reader, "parquet_get_map_value_type")
        ! Same contract as its two siblings: a name that does not exist is a caller mistake and
        ! aborts, while a column this library cannot read is the ANSWER "unknown". `recognized` is
        ! deliberately unused beyond documenting that the C++ side has already written the token --
        ! there is nothing for a second channel to say that the token does not.
        call check_column_exists(reader, name, "parquet_get_map_value_type")
        recognized = parquet_reader_get_map_value_type_name(reader%handle, trim(name)//char(0), buf, &
            int(len(buf), kind=c_long_long))
        type_name = trim(buf)
    end procedure parquet_get_map_value_type
    module procedure parquet_get_column_nullable
        call check_reader_open(reader, "parquet_get_column_nullable")
        ! Same rule as parquet_get_column_type: a missing name is a caller mistake and aborts,
        ! while a column whose TYPE this library cannot read still has a meaningful flag and
        ! gets an answer.
        call check_column_exists(reader, name, "parquet_get_column_nullable")
        is_nullable = parquet_reader_get_column_nullable(reader%handle, trim(name)//char(0)) /= 0_c_long_long
    end procedure parquet_get_column_nullable
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
