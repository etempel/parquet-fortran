!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Reader lifecycle (open/close), filter/qc machinery, metadata population,
!> row-group resolution, and the type-generic shared helpers every
!> read-specifics child (parquet_read_numeric/_string/_temporal) reaches by
!> host association: validity-buffer construction, reader/column/row-group
!> existence checks, fixed-width text packing for the filter/qc C++ API, and
!> the whole-column-read-avoidance row-group helpers.
submodule (parquet) parquet_read
    use ieee_arithmetic, only: ieee_is_nan
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
    !> are disallowed outright on a reader opened with filter= and/or sample_fraction < 1.0 (the
    !> two share this same mask -- see parquet_reader_has_filter's own doc-comment in
    !> parquet_bindings.f90), rather than silently ignoring it.
    subroutine check_reader_no_filter(reader, context)
        type(parquet_reader), intent(in) :: reader !! open reader to check.
        character(len=*), intent(in) :: context !! calling procedure's name, used in the error-stop message.
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).
        if (parquet_reader_has_filter(reader%handle) /= 0) then
            call reader_filename_suffix(reader, name_suffix)
            error stop trim(context) // ": chunked reads are not supported on a reader opened with an " // &
                "active filter=/sample_fraction= -- open a second, unfiltered reader for chunked access" // name_suffix
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
        case ("is_null", "is_not_null")
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
            ! GCOVR_EXCL_START -- gcov attribution artifact
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
        logical :: use_threads_value, qc_effective, qc_soft_value, filter_will_apply
        character(len=:), allocatable :: name_suffix !! scratch (reader_filename_suffix).

        use_threads_value = .true.
        if (present(use_threads)) use_threads_value = use_threads

        reader%handle = create_parquet_reader(trim(filename)//char(0), merge(1_c_int, 0_c_int, use_threads_value))
        reader%filename = trim(filename)
        call populate_reader_metadata(reader)

        filter_will_apply = .false.
        if (present(filter)) filter_will_apply = (filter%n > 0)

        ! Random downsampling (sample_fraction=): applied first, before any filter=/qc setup.
        ! filter_will_apply tells parquet_apply_sample whether prefetch_filter_columns/
        ! parquet_apply_filter (below) are about to run right after this, so it can defer
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

        ! Warm the filter's columns in one batched (thread-parallel) read
        ! BEFORE qc is enabled, so parquet_reader_set_filter reads them from
        ! cache instead of a serial ReadColumn per clause, and so this prefetch
        ! does not run read-time qc on the still-unfiltered data -- qc for those
        ! columns still runs later, in set_filter, on the filtered rows. See
        ! prefetch_filter_columns for the ordering/error-handling rationale.
        if (filter_will_apply) call prefetch_filter_columns(reader, filter)

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

        if (filter_will_apply) call parquet_apply_filter(reader, filter)

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
        call parquet_open_reader_base(reader, filename, use_threads, filter, sample_fraction, sample_seed, schema, qc, &
            qc_soft, prefetch)
        call parquet_get_nrows(reader, nrows, check_positive=.true.)
    end procedure parquet_open_reader_nrows_int64
    module procedure parquet_open_reader_nrows_int32
        call parquet_open_reader_base(reader, filename, use_threads, filter, sample_fraction, sample_seed, schema, qc, &
            qc_soft, prefetch)
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
    !> Resolves `name`'s canonical physical data type (see valid_query_data_types in parquet.f90),
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
            matches = trim(resolved_type) == "float32" .or. trim(resolved_type) == "float64"
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
        character(len=:), allocatable :: name_suffix

        call check_reader_open(reader, "parquet_get_column_type")
        call check_column_exists(reader, name, "parquet_get_column_type")
        call resolve_column_type(reader, name, type_name, recognized)
        if (.not. recognized) then
            call reader_filename_suffix(reader, name_suffix)
            error stop "parquet_get_column_type: column '" // trim(name) // "' has an unsupported data " // &
                "type for this query (" // type_name // "); expected one of: int32, int64, float32, " // &
                "float64, boolean, string, date, time, timestamp" // name_suffix
        end if
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
    module procedure parquet_release_column
        call check_reader_open(reader, "parquet_release_column")
        call parquet_reader_release_column(reader%handle, trim(name)//char(0))
    end procedure parquet_release_column

end submodule parquet_read
