!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Bodies of the read-path module procedures declared in parquet.f90's
!> interface block (parquet_read_column's specifics, parquet_open_reader,
!> parquet_get_metadata's specifics, ...), plus private helpers for row
!> filtering, qc: enforcement on read, and validity-buffer plumbing.
submodule (parquet) parquet_read
    implicit none
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

    !> parquet_read_column_chunk's own extra guard, called right after check_column_exists: a
    !> row-group-scoped read has no coherent way to apply reader%filter's own mask, which is a
    !> single flat mask sized to the whole unfiltered file with no row-group structure of its
    !> own (see get_row_group_chunk_array's own comment in parquet_wrapper.cpp) -- so chunk reads
    !> are disallowed outright on a reader opened with filter=, rather than silently ignoring it.
    subroutine check_reader_no_filter(reader, context)
        type(parquet_reader), intent(in) :: reader !! open reader to check.
        character(len=*), intent(in) :: context !! calling procedure's name, used in the error-stop message.
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).
        if (parquet_reader_has_filter(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop trim(context) // ": chunked reads are not supported on a reader opened with an " // &
                "active filter= -- open a second, unfiltered reader for chunked access" // name_suffix
        end if
    end subroutine check_reader_no_filter

    !> parquet_read_column_chunk's own extra guard, called right after check_reader_no_filter:
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
        if (len(t) == 0) then ! GCOVR_EXCL_START
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
        if (len(rest) == 0) then ! GCOVR_EXCL_START
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
        case ("is_null", "is_not_null")
            if (len(rest) > 0) then ! GCOVR_EXCL_START
                errmsg = "filter rule '" // t // "': " // trim(op) // " takes no value"
                return
            end if ! GCOVR_EXCL_STOP
        case (">", ">=", "<", "<=", "==", "/=")
            if (len(rest) == 0) then ! GCOVR_EXCL_START
                errmsg = "filter rule '" // t // "' is missing a value after '" // trim(op) // "'"
                return
            end if ! GCOVR_EXCL_STOP
            if (rest(1:1) == '"') then
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
            errmsg = "filter rule '" // t // "' has an unknown operator '" // trim(op) // "'" ! GCOVR_EXCL_LINE
            return ! GCOVR_EXCL_LINE
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

    !> Batch-prefetches the distinct columns named by `filter` in a single
    !> (thread-parallel, with use_threads) ReadTable, so parquet_reader_set_filter
    !> later reads each from column_cache instead of issuing a separate
    !> single-column ReadColumn per clause (see get_single_chunk_array).
    !>
    !> Two deliberate constraints:
    !>  - Only columns that actually exist are prefetched (checked via the
    !>    non-throwing parquet_reader_has_column). A filter naming an unknown
    !>    column is left untouched here so parquet_reader_set_filter still
    !>    reports it with its exact "unknown column in filter: ..." message
    !>    rather than this prefetch aborting first with a different one.
    !>  - Must be called from parquet_open_reader BEFORE parquet_apply_qc: while
    !>    qc is still disabled, prefetching does not run read-time qc on the
    !>    (still unfiltered) columns. The qc check for filter columns stays in
    !>    parquet_reader_set_filter, on the filtered rows, exactly as before.
    !>
    !> The column list is built as one comma-separated scalar string and passed
    !> to the string form of parquet_prefetch_columns, avoiding the fixed-width
    !> character-array pitfall where a too-short declared length would truncate
    !> a longer column name. Duplicate columns (same column in two clauses) are
    !> dropped with a delimiter-guarded membership test.
    subroutine prefetch_filter_columns(reader, filter)
        type(parquet_reader), intent(inout) :: reader !! open reader whose filter columns are warmed.
        type(parquet_filter), intent(in) :: filter !! filter whose distinct column names are prefetched.
        character(len=:), allocatable :: name, op, value, errmsg, list
        logical :: is_string, ok
        integer :: i

        list = ""
        do i = 1, filter%n
            call parquet_tokenize_filter_rule(filter%rules(i), name, op, value, is_string, ok, errmsg)
            ! A malformed rule is left for parquet_apply_filter to report.
            if (.not. ok) cycle
            ! Unknown columns: skip, so parquet_reader_set_filter owns the error.
            if (parquet_reader_has_column(reader%handle, trim(name)//char(0)) == 0) cycle
            ! Delimiter-guarded dedup (so "ra" is not matched inside "gal_ra").
            if (len(list) > 0) then
                if (index(","//list//",", ","//trim(name)//",") > 0) cycle
            end if
            if (len(list) == 0) then
                list = trim(name)
            else
                list = list//","//trim(name)
            end if
        end do

        if (len(list) > 0) call parquet_prefetch_columns(reader, list)
    end subroutine prefetch_filter_columns

    !> Tokenizes and applies every rule in `filter` to `reader` -- called from
    !> parquet_open_reader right after the reader itself is created, so
    !> parquet_get_nrows and every column read afterward already reflect the
    !> filtered row set (see parquet_reader_set_filter in parquet_wrapper.cpp
    !> for the actual validation/masking).
    subroutine parquet_apply_filter(reader, filter)
        type(parquet_reader), intent(inout) :: reader !! open reader the filter is applied to.
        type(parquet_filter), intent(in) :: filter !! filter whose rules are tokenized, validated, and applied.
        character(len=64), allocatable :: names(:)
        character(len=16), allocatable :: ops(:)
        character(len=512), allocatable :: values(:)
        integer(c_int8_t), allocatable :: is_string_flags(:)
        character(kind=c_char), allocatable :: names_packed(:), ops_packed(:), values_packed(:)
        character(len=:), allocatable :: parsed_name, parsed_op, parsed_value, errmsg
        logical :: parsed_is_string, ok
        character(len=1024) :: c_err
        integer(c_long_long) :: status
        integer :: i, n
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        n = filter%n
        allocate(names(n), ops(n), values(n), is_string_flags(n))

        do i = 1, n
            call parquet_tokenize_filter_rule(filter%rules(i), parsed_name, parsed_op, parsed_value, &
                parsed_is_string, ok, errmsg)
            call reader_filename_suffix(reader, name_suffix)
            if (.not. ok) error stop "parquet_open_reader: invalid filter rule: " // errmsg // name_suffix
            ! GCOVR_EXCL_START
            if (len(parsed_name) > len(names) .or. len(parsed_op) > len(ops) .or. len(parsed_value) > len(values)) then
                call reader_filename_suffix(reader, name_suffix)
                error stop "parquet_open_reader: filter rule exceeds an internal length limit: " // trim(filter%rules(i)) // &
                    name_suffix
            end if
            ! GCOVR_EXCL_STOP
            names(i) = parsed_name
            ops(i) = parsed_op
            values(i) = parsed_value
            is_string_flags(i) = merge(1_c_int8_t, 0_c_int8_t, parsed_is_string)
        end do

        call pack_fixed_width_strings(names, names_packed)
        call pack_fixed_width_strings(ops, ops_packed)
        call pack_fixed_width_strings(values, values_packed)

        c_err = ""
        status = parquet_reader_set_filter(reader%handle, names_packed, int(len(names), kind=c_long_long), &
            ops_packed, int(len(ops), kind=c_long_long), values_packed, int(len(values), kind=c_long_long), &
            is_string_flags, int(n, kind=c_long_long), c_err, int(len(c_err), kind=c_long_long))

        call reader_filename_suffix(reader, name_suffix)
        if (status /= 0) error stop "parquet_open_reader: " // trim(c_err) // name_suffix
    end subroutine parquet_apply_filter

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
        logical :: use_threads_value, qc_effective, qc_soft_value

        use_threads_value = .true.
        if (present(use_threads)) use_threads_value = use_threads

        reader%handle = create_parquet_reader(trim(filename)//char(0), merge(1_c_int, 0_c_int, use_threads_value))
        reader%filename = trim(filename)
        call populate_reader_metadata(reader)

        ! Warm the filter's columns in one batched (thread-parallel) read
        ! BEFORE qc is enabled, so parquet_reader_set_filter reads them from
        ! cache instead of a serial ReadColumn per clause, and so this prefetch
        ! does not run read-time qc on the still-unfiltered data -- qc for those
        ! columns still runs later, in set_filter, on the filtered rows. See
        ! prefetch_filter_columns for the ordering/error-handling rationale.
        if (present(filter)) then
            if (filter%n > 0) call prefetch_filter_columns(reader, filter)
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

        if (present(filter)) then
            if (filter%n > 0) call parquet_apply_filter(reader, filter)
        end if

        ! Must run AFTER parquet_apply_filter: a column cached before the
        ! filter mask exists would stay raw/unfiltered forever, since
        ! set_filter only re-masks the filter clauses' own columns, not the
        ! whole cache (see parquet_reader_prefetch_all_columns's own comment
        ! in parquet_wrapper.cpp). Filter columns prefetched earlier by
        ! prefetch_filter_columns are skipped here (already cached and
        ! correctly re-masked by set_filter), so this only reads the
        ! remaining columns.
        if (present(prefetch)) then
            if (prefetch) call parquet_reader_prefetch_all_columns(reader%handle)
        end if
    end procedure parquet_open_reader_base

    module procedure parquet_open_reader_nrows_int64
        call parquet_open_reader_base(reader, filename, use_threads, filter, schema, qc, qc_soft, prefetch)
        call parquet_get_nrows(reader, nrows, check_positive=.true.)
    end procedure parquet_open_reader_nrows_int64

    module procedure parquet_open_reader_nrows_int32
        call parquet_open_reader_base(reader, filename, use_threads, filter, schema, qc, qc_soft, prefetch)
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
        character(len=:), allocatable :: tok
        integer :: i, start, ntok, maxlen, idx
        logical :: at_boundary

        ! Pass 1: count non-empty tokens and find the longest, so the packed
        ! array's element length covers every name exactly.
        ntok = 0
        maxlen = 0
        start = 1
        do i = 1, len(names) + 1
            at_boundary = (i > len(names))
            if (.not. at_boundary) at_boundary = (names(i:i) == ',' .or. names(i:i) == ';')
            if (at_boundary) then
                tok = trim(adjustl(names(start:i-1)))
                if (len(tok) > 0) then
                    ntok = ntok + 1
                    maxlen = max(maxlen, len(tok))
                end if
                start = i + 1
            end if
        end do

        ! Pass 2: fill the array and hand off to the array form (which also
        ! does the reader-open and column-existence checks, even for ntok == 0).
        allocate(character(len=max(maxlen, 1)) :: name_arr(ntok))
        idx = 0
        start = 1
        do i = 1, len(names) + 1
            at_boundary = (i > len(names))
            if (.not. at_boundary) at_boundary = (names(i:i) == ',' .or. names(i:i) == ';')
            if (at_boundary) then
                tok = trim(adjustl(names(start:i-1)))
                if (len(tok) > 0) then
                    idx = idx + 1
                    name_arr(idx) = tok
                end if
                start = i + 1
            end if
        end do

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
        if (nrows64 > huge(0_int32)) then ! GCOVR_EXCL_START
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
        if (num_row_groups64 > huge(0_int32)) then ! GCOVR_EXCL_START
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_get_num_row_groups: number of row groups exceeds int32 range" // &
                name_suffix
        end if ! GCOVR_EXCL_STOP
        num_row_groups = int(num_row_groups64, kind=int32)
    end procedure parquet_get_num_row_groups_int32

    !> Shared body of parquet_get_chunk_size_reader_int32/_int64 -- see the generic interface's
    !> own doc comment in parquet.f90. `row_group` is 0 if the caller omitted its own optional
    !> row_group argument (both kind-specifics translate "absent" to 0 before calling in),
    !> resolved here to 1 (the first row group) -- 0 can never be a valid 1-based row_group, so
    !> it is unambiguous as an "absent" sentinel.
    subroutine parquet_get_chunk_size_reader_impl(reader, chunk_size, row_group)
        type(parquet_reader), intent(in) :: reader !! open reader.
        integer(int64), intent(out) :: chunk_size !! row group's own physical row count.
        integer(int64), intent(in) :: row_group !! 1-based row group, or 0 for "use the first row group".
        integer(int64) :: rg, num_row_groups
        character(len=32) :: rg_str, total_str
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        call check_reader_open(reader, "parquet_get_chunk_size")
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
        if (nelem64 > huge(0_int32)) then ! GCOVR_EXCL_START
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

    module procedure parquet_check_read_row_count
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
    end procedure parquet_check_read_row_count

    module procedure parquet_read_int32_column_1d
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int32_column(reader%handle, trim(name)//char(0), values, size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_int32_column_1d

    module procedure parquet_read_int64_column_1d
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int64_column(reader%handle, trim(name)//char(0), values, size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_int64_column_1d

    module procedure parquet_read_float32_column_1d
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float32_column(reader%handle, trim(name)//char(0), values, size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_float32_column_1d

    module procedure parquet_read_float64_column_1d
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float64_column(reader%handle, trim(name)//char(0), values, size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_float64_column_1d

    module procedure parquet_read_logical_column_1d
        integer(c_int8_t), allocatable :: tmp(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, size(values, kind=c_long_long))
        allocate(tmp(size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_bool8_column(reader%handle, trim(name)//char(0), tmp, size(tmp, kind=c_long_long), valid_ptr)
        do i = 1_int64, size(values, kind=int64)
            values(i) = tmp(i) /= 0_c_int8_t
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_logical_column_1d

    module procedure parquet_read_string_column_1d
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nrows, i, k
        integer :: item_len, j

        nrows = size(values, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        item_len = len(values(1))
        allocate(packed(item_len*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), nrows, valid_buf, valid_ptr)
        call parquet_read_string_column(reader%handle, trim(name)//char(0), packed, int(item_len, kind=c_long_long), &
            nrows, valid_ptr)

        k = 0_int64
        do i = 1_int64, nrows
            values(i) = ''
            do j = 1, item_len
                k = k + 1_int64
                values(i)(j:j) = achar(iachar(packed(k)))
            end do
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, nrows
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_string_column_1d

    module procedure parquet_read_int32_array_full
        integer(c_int32_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_int32_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k)
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_int32_array_full

    module procedure parquet_read_int64_array_full
        integer(c_int64_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_int64_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k)
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_int64_array_full

    module procedure parquet_read_float32_array_full
        real(c_float), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_float32_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k)
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_float32_array_full

    module procedure parquet_read_float64_array_full
        real(c_double), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_float64_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k)
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_float64_array_full

    module procedure parquet_read_logical_array_full
        integer(c_int8_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_bool8_array_column(reader%handle, trim(name)//char(0), flat, nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k) /= 0_c_int8_t
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_logical_array_full

    module procedure parquet_read_string_array_full
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k, p
        integer :: item_len, m

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column")
        call check_column_exists(reader, name, "parquet_read_column")
        call parquet_check_read_row_count(reader, name, nrows)
        item_len = len(values(1,1))
        allocate(packed(item_len*asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_string_array_column(reader%handle, trim(name)//char(0), packed, int(item_len, kind=c_long_long), &
            nrows, asize, valid_ptr)

        p = 0_int64
        do i = 1_int64, nrows
            do j = 1_int64, asize
                values(j, i) = ''
                do m = 1, item_len
                    p = p + 1_int64
                    values(j, i)(m:m) = achar(iachar(packed(p)))
                end do
                k = (i-1)*asize + j
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end procedure parquet_read_string_array_full

    !> Shared body of parquet_read_int32_array_row_mode/_row_index_int64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_index has two kind-specifics.
    subroutine parquet_read_int32_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int32), intent(out) :: values(:)
        integer(int64), intent(in) :: row_index
        integer(int32), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int32_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_int32_array_row_mode_impl

    module procedure parquet_read_int32_array_row_mode
        call parquet_read_int32_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, is_valid)
    end procedure parquet_read_int32_array_row_mode

    module procedure parquet_read_int32_array_row_mode_row_index_int64
        call parquet_read_int32_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_int32_array_row_mode_row_index_int64

    !> Shared body of parquet_read_int64_array_row_mode/_row_index_int64.
    subroutine parquet_read_int64_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(out) :: values(:)
        integer(int64), intent(in) :: row_index
        integer(int64), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int64_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_int64_array_row_mode_impl

    module procedure parquet_read_int64_array_row_mode
        call parquet_read_int64_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, is_valid)
    end procedure parquet_read_int64_array_row_mode

    module procedure parquet_read_int64_array_row_mode_row_index_int64
        call parquet_read_int64_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_int64_array_row_mode_row_index_int64

    !> Shared body of parquet_read_float32_array_row_mode/_row_index_int64.
    subroutine parquet_read_float32_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        real(real32), intent(out) :: values(:)
        integer(int64), intent(in) :: row_index
        real(real32), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float32_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_float32_array_row_mode_impl

    module procedure parquet_read_float32_array_row_mode
        call parquet_read_float32_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, &
            is_valid)
    end procedure parquet_read_float32_array_row_mode

    module procedure parquet_read_float32_array_row_mode_row_index_int64
        call parquet_read_float32_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_float32_array_row_mode_row_index_int64

    !> Shared body of parquet_read_float64_array_row_mode/_row_index_int64.
    subroutine parquet_read_float64_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        real(real64), intent(out) :: values(:)
        integer(int64), intent(in) :: row_index
        real(real64), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float64_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_float64_array_row_mode_impl

    module procedure parquet_read_float64_array_row_mode
        call parquet_read_float64_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, &
            is_valid)
    end procedure parquet_read_float64_array_row_mode

    module procedure parquet_read_float64_array_row_mode_row_index_int64
        call parquet_read_float64_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_float64_array_row_mode_row_index_int64

    !> Shared body of parquet_read_logical_array_row_mode/_row_index_int64.
    subroutine parquet_read_logical_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        logical, intent(out) :: values(:)
        integer(int64), intent(in) :: row_index
        logical, intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable :: tmp(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        allocate(tmp(size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_bool8_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), tmp, &
            size(values, kind=c_long_long), valid_ptr)
        do i = 1_int64, size(values, kind=int64)
            values(i) = tmp(i) /= 0_c_int8_t
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_logical_array_row_mode_impl

    module procedure parquet_read_logical_array_row_mode
        call parquet_read_logical_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, &
            is_valid)
    end procedure parquet_read_logical_array_row_mode

    module procedure parquet_read_logical_array_row_mode_row_index_int64
        call parquet_read_logical_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_logical_array_row_mode_row_index_int64

    !> Shared body of parquet_read_string_array_row_mode/_row_index_int64.
    subroutine parquet_read_string_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: values(:)
        integer(int64), intent(in) :: row_index
        character(len=*), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, p
        integer :: item_len, j

        call check_reader_open(reader, "parquet_read_array_row_mode")
        call check_column_exists(reader, name, "parquet_read_array_row_mode")
        item_len = len(values(1))
        allocate(packed(item_len*size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_string_array_row(reader%handle, trim(name)//char(0), int(row_index, kind=c_long_long), packed, &
            int(item_len, kind=c_long_long), size(values, kind=c_long_long), valid_ptr)
        p = 0_int64
        do i = 1_int64, size(values, kind=int64)
            values(i) = ''
            do j = 1, item_len
                p = p + 1_int64
                values(i)(j:j) = achar(iachar(packed(p)))
            end do
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_string_array_row_mode_impl

    module procedure parquet_read_string_array_row_mode
        call parquet_read_string_array_row_mode_impl(reader, name, values, int(row_index, kind=int64), null_value, &
            is_valid)
    end procedure parquet_read_string_array_row_mode

    module procedure parquet_read_string_array_row_mode_row_index_int64
        call parquet_read_string_array_row_mode_impl(reader, name, values, row_index, null_value, is_valid)
    end procedure parquet_read_string_array_row_mode_row_index_int64

    module procedure parquet_read_int32_array_element_mode
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int32_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_int32_array_element_mode

    module procedure parquet_read_int64_array_element_mode
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int64_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_int64_array_element_mode

    module procedure parquet_read_float32_array_element_mode
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float32_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_float32_array_element_mode

    module procedure parquet_read_float64_array_element_mode
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float64_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), values, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_float64_array_element_mode

    module procedure parquet_read_logical_array_element_mode
        integer(c_int8_t), allocatable :: tmp(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        allocate(tmp(size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_bool8_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), tmp, &
            size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        do i = 1_int64, size(values, kind=int64)
            values(i) = tmp(i) /= 0_c_int8_t
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_logical_array_element_mode

    module procedure parquet_read_string_array_element_mode
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i, p
        integer :: item_len, j

        call check_reader_open(reader, "parquet_read_array_element_mode")
        call check_column_exists(reader, name, "parquet_read_array_element_mode")
        item_len = len(values(1))
        allocate(packed(item_len*size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_string_array_element(reader%handle, trim(name)//char(0), int(elem_index, kind=c_long_long), packed, &
            int(item_len, kind=c_long_long), size(values, kind=c_long_long), 0_c_long_long, valid_ptr)
        p = 0_int64
        do i = 1_int64, size(values, kind=int64)
            values(i) = ''
            do j = 1, item_len
                p = p + 1_int64
                values(i)(j:j) = achar(iachar(packed(p)))
            end do
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end procedure parquet_read_string_array_element_mode

    !> Shared body of parquet_read_int32_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_int32_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        integer(int32), intent(out) :: values(:)
        integer(int32), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int32_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_int32_column_chunk_impl

    !> Shared body of parquet_read_int32_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_int32_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        integer(int32), intent(out) :: values(:, :)
        integer(int32), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:, :)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_int32_array_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            nrows, asize, valid_ptr)
        if (present(is_valid) .or. present(null_value)) then
            do i = 1_int64, nrows
                do j = 1_int64, asize
                    k = (i-1)*asize + j
                    if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                    if (present(null_value)) then
                        if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                    end if
                end do
            end do
        end if
    end subroutine parquet_read_int32_array_column_chunk_impl

    !> Shared body of parquet_read_int64_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_int64_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        integer(int64), intent(out) :: values(:)
        integer(int64), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_int64_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_int64_column_chunk_impl

    !> Shared body of parquet_read_int64_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_int64_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        integer(int64), intent(out) :: values(:, :)
        integer(int64), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:, :)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_int64_array_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            nrows, asize, valid_ptr)
        if (present(is_valid) .or. present(null_value)) then
            do i = 1_int64, nrows
                do j = 1_int64, asize
                    k = (i-1)*asize + j
                    if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                    if (present(null_value)) then
                        if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                    end if
                end do
            end do
        end if
    end subroutine parquet_read_int64_array_column_chunk_impl

    !> Shared body of parquet_read_float32_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_float32_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        real(real32), intent(out) :: values(:)
        real(real32), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float32_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_float32_column_chunk_impl

    !> Shared body of parquet_read_float32_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_float32_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        real(real32), intent(out) :: values(:, :)
        real(real32), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:, :)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_float32_array_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            nrows, asize, valid_ptr)
        if (present(is_valid) .or. present(null_value)) then
            do i = 1_int64, nrows
                do j = 1_int64, asize
                    k = (i-1)*asize + j
                    if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                    if (present(null_value)) then
                        if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                    end if
                end do
            end do
        end if
    end subroutine parquet_read_float32_array_column_chunk_impl

    !> Shared body of parquet_read_float64_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_float64_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        real(real64), intent(out) :: values(:)
        real(real64), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_float64_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            size(values, kind=c_long_long), valid_ptr)
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_float64_column_chunk_impl

    !> Shared body of parquet_read_float64_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_float64_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        real(real64), intent(out) :: values(:, :)
        real(real64), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:, :)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_float64_array_column_chunk(reader%handle, trim(name)//char(0), row_group, values, &
            nrows, asize, valid_ptr)
        if (present(is_valid) .or. present(null_value)) then
            do i = 1_int64, nrows
                do j = 1_int64, asize
                    k = (i-1)*asize + j
                    if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                    if (present(null_value)) then
                        if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                    end if
                end do
            end do
        end if
    end subroutine parquet_read_float64_array_column_chunk_impl

    !> Shared body of parquet_read_logical_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_logical_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        logical, intent(out) :: values(:)
        logical, intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        integer(c_int8_t), allocatable :: tmp(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: i

        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        allocate(tmp(size(values, kind=int64)))
        call make_valid_buf(present(null_value) .or. present(is_valid), size(values, kind=int64), valid_buf, valid_ptr)
        call parquet_read_bool8_column_chunk(reader%handle, trim(name)//char(0), row_group, tmp, &
            size(tmp, kind=c_long_long), valid_ptr)
        do i = 1_int64, size(values, kind=int64)
            values(i) = tmp(i) /= 0_c_int8_t
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, size(values, kind=int64)
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_logical_column_chunk_impl

    !> Shared body of parquet_read_logical_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_logical_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        logical, intent(out) :: values(:, :)
        logical, intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:, :)
        integer(c_int8_t), allocatable :: flat(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        allocate(flat(asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_bool8_array_column_chunk(reader%handle, trim(name)//char(0), row_group, flat, &
            nrows, asize, valid_ptr)
        do i = 1_int64, nrows
            do j = 1_int64, asize
                k = (i-1)*asize + j
                values(j, i) = flat(k) /= 0_c_int8_t
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end subroutine parquet_read_logical_array_column_chunk_impl

    !> Shared body of parquet_read_string_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_string_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        character(len=*), intent(out) :: values(:)
        character(len=*), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:)
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: nrows, i, k
        integer :: item_len, j

        nrows = size(values, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        item_len = len(values(1))
        allocate(packed(item_len*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), nrows, valid_buf, valid_ptr)
        call parquet_read_string_column_chunk(reader%handle, trim(name)//char(0), row_group, packed, &
            int(item_len, kind=c_long_long), nrows, valid_ptr)

        k = 0_int64
        do i = 1_int64, nrows
            values(i) = ''
            do j = 1, item_len
                k = k + 1_int64
                values(i)(j:j) = achar(iachar(packed(k)))
            end do
        end do
        if (present(is_valid)) is_valid = valid_buf /= 0_c_int8_t
        if (present(null_value)) then
            do i = 1_int64, nrows
                if (valid_buf(i) == 0_c_int8_t) values(i) = null_value
            end do
        end if
    end subroutine parquet_read_string_column_chunk_impl

    !> Shared body of parquet_read_string_array_column_chunk_rg32/_rg64 -- see the generic
    !> interface's own doc comment in parquet.f90 for why row_group has two kind-specifics.
    subroutine parquet_read_string_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
        type(parquet_reader), intent(in) :: reader
        character(len=*), intent(in) :: name
        integer(int64), intent(in) :: row_group
        character(len=*), intent(out) :: values(:, :)
        character(len=*), intent(in), optional :: null_value
        logical, intent(out), optional :: is_valid(:, :)
        character(kind=c_char), allocatable :: packed(:)
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        integer(int64) :: asize, nrows, i, j, k, p
        integer :: item_len, m

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)
        call check_reader_open(reader, "parquet_read_column_chunk")
        call check_column_exists(reader, name, "parquet_read_column_chunk")
        call check_reader_no_filter(reader, "parquet_read_column_chunk")
        call check_row_group_valid(reader, row_group, "parquet_read_column_chunk")
        item_len = len(values(1,1))
        allocate(packed(item_len*asize*nrows))
        call make_valid_buf(present(null_value) .or. present(is_valid), asize*nrows, valid_buf, valid_ptr)
        call parquet_read_string_array_column_chunk(reader%handle, trim(name)//char(0), row_group, packed, &
            int(item_len, kind=c_long_long), nrows, asize, valid_ptr)

        p = 0_int64
        do i = 1_int64, nrows
            do j = 1_int64, asize
                values(j, i) = ''
                do m = 1, item_len
                    p = p + 1_int64
                    values(j, i)(m:m) = achar(iachar(packed(p)))
                end do
                k = (i-1)*asize + j
                if (present(is_valid)) is_valid(j, i) = valid_buf(k) /= 0_c_int8_t
                if (present(null_value)) then
                    if (valid_buf(k) == 0_c_int8_t) values(j, i) = null_value
                end if
            end do
        end do
    end subroutine parquet_read_string_array_column_chunk_impl

    module procedure parquet_read_int32_column_chunk_rg32
        call parquet_read_int32_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_int32_column_chunk_rg32

    module procedure parquet_read_int32_column_chunk_rg64
        call parquet_read_int32_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_int32_column_chunk_rg64

    module procedure parquet_read_int32_array_column_chunk_rg32
        call parquet_read_int32_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_int32_array_column_chunk_rg32

    module procedure parquet_read_int32_array_column_chunk_rg64
        call parquet_read_int32_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_int32_array_column_chunk_rg64

    module procedure parquet_read_int64_column_chunk_rg32
        call parquet_read_int64_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_int64_column_chunk_rg32

    module procedure parquet_read_int64_column_chunk_rg64
        call parquet_read_int64_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_int64_column_chunk_rg64

    module procedure parquet_read_int64_array_column_chunk_rg32
        call parquet_read_int64_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_int64_array_column_chunk_rg32

    module procedure parquet_read_int64_array_column_chunk_rg64
        call parquet_read_int64_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_int64_array_column_chunk_rg64

    module procedure parquet_read_float32_column_chunk_rg32
        call parquet_read_float32_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_float32_column_chunk_rg32

    module procedure parquet_read_float32_column_chunk_rg64
        call parquet_read_float32_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_float32_column_chunk_rg64

    module procedure parquet_read_float32_array_column_chunk_rg32
        call parquet_read_float32_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_float32_array_column_chunk_rg32

    module procedure parquet_read_float32_array_column_chunk_rg64
        call parquet_read_float32_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_float32_array_column_chunk_rg64

    module procedure parquet_read_float64_column_chunk_rg32
        call parquet_read_float64_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_float64_column_chunk_rg32

    module procedure parquet_read_float64_column_chunk_rg64
        call parquet_read_float64_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_float64_column_chunk_rg64

    module procedure parquet_read_float64_array_column_chunk_rg32
        call parquet_read_float64_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_float64_array_column_chunk_rg32

    module procedure parquet_read_float64_array_column_chunk_rg64
        call parquet_read_float64_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_float64_array_column_chunk_rg64

    module procedure parquet_read_logical_column_chunk_rg32
        call parquet_read_logical_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_logical_column_chunk_rg32

    module procedure parquet_read_logical_column_chunk_rg64
        call parquet_read_logical_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_logical_column_chunk_rg64

    module procedure parquet_read_logical_array_column_chunk_rg32
        call parquet_read_logical_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_logical_array_column_chunk_rg32

    module procedure parquet_read_logical_array_column_chunk_rg64
        call parquet_read_logical_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_logical_array_column_chunk_rg64

    module procedure parquet_read_string_column_chunk_rg32
        call parquet_read_string_column_chunk_impl(reader, name, int(row_group, kind=int64), values, null_value, &
            is_valid)
    end procedure parquet_read_string_column_chunk_rg32

    module procedure parquet_read_string_column_chunk_rg64
        call parquet_read_string_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_string_column_chunk_rg64

    module procedure parquet_read_string_array_column_chunk_rg32
        call parquet_read_string_array_column_chunk_impl(reader, name, int(row_group, kind=int64), values, &
            null_value, is_valid)
    end procedure parquet_read_string_array_column_chunk_rg32

    module procedure parquet_read_string_array_column_chunk_rg64
        call parquet_read_string_array_column_chunk_impl(reader, name, row_group, values, null_value, is_valid)
    end procedure parquet_read_string_array_column_chunk_rg64

end submodule parquet_read
