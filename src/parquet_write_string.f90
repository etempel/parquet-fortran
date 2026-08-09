!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> String write specifics (scalar and matrix, whole-column and
!> row-group-chunked, plus the fixed-width "compact" variants): bodies of the
!> module procedures declared in parquet_core.f90's interface block, plus the
!> string-only private qc: enforcement/bound-satisfaction helpers they
!> depend on.
submodule (parquet_core:parquet_write) parquet_write_string
    implicit none
contains

    !> String form of parquet_qc_numeric_satisfies: compares `value` against
    !> `bound` lexicographically (Fortran's intrinsic character relational
    !> operators) under min:/max: operator `op`.
    logical function parquet_qc_string_satisfies(value, bound, op) result(ok)
        character(len=*), intent(in) :: value !! value being checked.
        character(len=*), intent(in) :: bound !! declared qc: min:/max: bound.
        character(len=*), intent(in) :: op !! comparison operator (">=", "<=", ">", "<").

        select case (trim(op))
        case (">=")
            ok = value >= bound
        case ("<=")
            ok = value <= bound
        case (">")
            ok = value > bound
        case ("<")
            ok = value < bound
        case default
            ok = .true.
        end select
    end function parquet_qc_string_satisfies
    !> Same as parquet_check_qc_numeric but for a "string" column: bounds are
    !> compared as literal Fortran character strings (lexicographic, via the
    !> intrinsic relational operators) rather than parsed as numbers.
    subroutine parquet_check_qc_string(writer, name, values, is_valid_flat)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! string column name.
        character(len=*), intent(in) :: values(:) !! flattened column values.
        logical, intent(in), optional :: is_valid_flat(:) !! flattened validity mask (.true. => checked);
            !! absent means every element counts, matching the write-side is_valid convention elsewhere.
        integer :: idx
        integer(int64) :: i, n_valid, n_violate
        logical :: any_valid, ok, use_mask
        character(len=:), allocatable :: data_min, data_max, bounds_desc, fmt_int, fmt_int2

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. (writer%all_columns(idx)%has_qc_min .or. writer%all_columns(idx)%has_qc_max)) return

        use_mask = present(is_valid_flat)
        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        do i = 1_int64, size(values, kind=int64)
            if (use_mask) then
                if (.not. is_valid_flat(i)) cycle
            end if
            n_valid = n_valid + 1
            if (.not. any_valid) then
                data_min = trim(values(i))
                data_max = trim(values(i))
                any_valid = .true.
            else
                if (trim(values(i)) < data_min) data_min = trim(values(i))
                if (trim(values(i)) > data_max) data_max = trim(values(i))
            end if

            ok = .true.
            if (writer%all_columns(idx)%has_qc_min) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    trim(values(i)), trim(writer%all_columns(idx)%qc_min_raw), writer%all_columns(idx)%qc_min_op)
            end if
            if (writer%all_columns(idx)%has_qc_max) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    trim(values(i)), trim(writer%all_columns(idx)%qc_max_raw), writer%all_columns(idx)%qc_max_op)
            end if
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return

        bounds_desc = ""
        if (writer%all_columns(idx)%has_qc_min) then
            bounds_desc = "min " // trim(writer%all_columns(idx)%qc_min_op) // " '" // &
                trim(writer%all_columns(idx)%qc_min_raw) // "'"
        end if
        if (writer%all_columns(idx)%has_qc_max) then
            if (len_trim(bounds_desc) > 0) bounds_desc = bounds_desc // ", "
            bounds_desc = bounds_desc // "max " // trim(writer%all_columns(idx)%qc_max_op) // " '" // &
                trim(writer%all_columns(idx)%qc_max_raw) // "'"
        end if

        call parquet_qc_format_int(n_violate, fmt_int)
        call parquet_qc_format_int(n_valid, fmt_int2)
        call parquet_emit_warning("qc violation for column '" // trim(name) // "': declared " // bounds_desc // &
            ", data range ['" // data_min // "', '" // data_max // "'], " // &
            fmt_int // " of " // fmt_int2 // " valid element(s) out of range")
    end subroutine parquet_check_qc_string
    !> Same as parquet_check_qc_string but reading from a parquet_string_column source directly
    !> (the compact write path, parquet_write_string_column_compact/_chunk_compact) instead of
    !> requiring a pre-materialized character(len=*) array -- so a compact write never needs a
    !> padded intermediate just to run this check. Unlike parquet_check_qc_string (which compares
    !> trimmed values, since a padded array's trailing blanks may just be padding), values here
    !> are compared verbatim: parquet_string_column stores content exactly as given, so a
    !> trailing space is always real data, never padding -- see "Trimming on append" in
    !> doc/pages/string-columns.md.
    subroutine parquet_check_qc_string_compact(writer, name, values)
        type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer.
        character(len=*), intent(in) :: name !! string column name.
        type(parquet_string_column), intent(in) :: values !! the compact column being written.
        integer :: idx
        integer(int64) :: i, n_valid, n_violate, nrows, imin, imax, widest, elen
        logical :: any_valid, ok
        character(len=:), allocatable :: data_min, data_max, bounds_desc, fmt_int, fmt_int2
        character(len=:), allocatable :: scratch

        if (.not. writer%qc) return
        if (.not. writer%is_schema_enforced) return
        idx = parquet_get_defined_column_index(writer, name)
        if (idx == 0) return
        if (.not. (writer%all_columns(idx)%has_qc_min .or. writer%all_columns(idx)%has_qc_max)) return

        any_valid = .false.
        n_valid = 0_int64
        n_violate = 0_int64
        nrows = values%size()
        ! **Nothing in this loop allocates.** It used to materialize every element through `%get`,
        ! which is one heap round trip per row on a check that runs over the whole column at write
        ! time. Two things replace it: the running min/max track INDICES and compare through
        ! `%compare`, which is Fortran's own `<` on the stored bytes; and the value the qc predicate
        ! needs is copied into one scratch buffer, sized once from the column's longest element and
        ! reused every row. See `feature_risks.md` Risk-60.
        call values%statistics(max_len=widest)
        allocate(character(len=max(widest, 0_int64)) :: scratch)
        imin = 0_int64
        imax = 0_int64
        do i = 1_int64, nrows
            if (values%is_null(i)) cycle
            n_valid = n_valid + 1
            if (.not. any_valid) then
                imin = i
                imax = i
                any_valid = .true.
            else
                if (values%compare(i, imin) < 0) imin = i
                if (values%compare(i, imax) > 0) imax = i
            end if

            elen = values%length(i)
            call values%copy_to(i, scratch)
            ok = .true.
            if (writer%all_columns(idx)%has_qc_min) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    scratch(1:elen), trim(writer%all_columns(idx)%qc_min_raw), &
                    writer%all_columns(idx)%qc_min_op)
            end if
            if (writer%all_columns(idx)%has_qc_max) then
                ok = ok .and. parquet_qc_string_satisfies( &
                    scratch(1:elen), trim(writer%all_columns(idx)%qc_max_raw), &
                    writer%all_columns(idx)%qc_max_op)
            end if
            if (.not. ok) n_violate = n_violate + 1
        end do
        if (.not. any_valid .or. n_violate == 0) return
        ! Only now are the two winning elements materialized -- two allocations for the whole column.
        call values%get(imin, data_min)
        call values%get(imax, data_max)

        bounds_desc = ""
        if (writer%all_columns(idx)%has_qc_min) then
            bounds_desc = "min " // trim(writer%all_columns(idx)%qc_min_op) // " '" // &
                trim(writer%all_columns(idx)%qc_min_raw) // "'"
        end if
        if (writer%all_columns(idx)%has_qc_max) then
            if (len_trim(bounds_desc) > 0) bounds_desc = bounds_desc // ", "
            bounds_desc = bounds_desc // "max " // trim(writer%all_columns(idx)%qc_max_op) // " '" // &
                trim(writer%all_columns(idx)%qc_max_raw) // "'"
        end if

        call parquet_qc_format_int(n_violate, fmt_int)
        call parquet_qc_format_int(n_valid, fmt_int2)
        call parquet_emit_warning("qc violation for column '" // trim(name) // "': declared " // bounds_desc // &
            ", data range ['" // data_min // "', '" // data_max // "'], " // &
            fmt_int // " of " // fmt_int2 // " valid element(s) out of range")
    end subroutine parquet_check_qc_string_compact
    ! ---- Flat write workers ----
    !
    ! The four space-padded string specifics (scalar/matrix x whole-column/chunked) share one
    ! worker each for the whole-column and chunked forms, and the two parquet_string_column
    ! ("compact") specifics share a tail. `flat` is an assumed-size dummy of assumed LENGTH, so a
    ! rank-2 actual argument sequence-associates with it directly: the matrix specifics no longer
    ! reshape, and the byte order they produce is unchanged because the flattened order is exactly
    ! the (element, row)-major order the nested loops used to walk.
    !
    ! THE UNMASKED FAST PATH is the caller's decision here rather than the worker's: the padded
    ! specifics pass `values` itself when no row mask is in force, and the packed copy only when
    ! one is. That copy is what a `pack` over a character array costs -- a fresh deferred-length
    ! array of every kept element -- so skipping it is the whole point. See the same note in
    ! parquet_write_numeric.f90 for the shared reasoning.

    !> Whole-column write worker for the four space-padded string specifics: packs `flat`'s bytes
    !> into the contiguous fixed-width buffer the C binding takes, runs the qc/validity checks,
    !> and appends. `flat` is already row-masked (or is the caller's own array when no mask is in
    !> force), so this sees only the elements that will actually be written.
    subroutine write_string_flat(writer, name, flat, nitems, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        character(len=*), intent(in) :: flat(*) !! the elements to write, (element, row)-major.
        integer(int64), intent(in) :: nitems !! number of elements in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! post-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nitems`-long validity mask, or absent.
        character(kind=c_char), allocatable :: packed(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(int64) :: i, k
        integer :: j, item_len

        call parquet_check_row_count(writer, name, nrows)

        item_len = len(flat)
        allocate(packed(item_len*nitems))
        k = 0_int64
        do i = 1_int64, nitems
            do j = 1, item_len
                k = k + 1_int64
                packed(k) = achar(iachar(flat(i)(j:j)), kind=c_char)
            end do
        end do

        nullify(vmask)
        if (present(valid)) vmask => valid(1:nitems)
        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        call parquet_check_qc_string(writer, name, flat(1:nitems), vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)

        call parquet_resolve_output_name(writer, name, outname)
        if (asize == 1) then
            call parquet_append_string_column(writer%handle, trim(outname)//char(0), packed, &
                int(item_len, kind=c_long_long), nrows, valid_ptr)
        else
            call parquet_append_string_array_column(writer%handle, trim(outname)//char(0), packed, &
                int(item_len, kind=c_long_long), nrows, asize, valid_ptr)
        end if
    end subroutine write_string_flat
    !> Row-group-chunked counterpart of write_string_flat: the row-count check is the row group's
    !> own (already done by parquet_check_row_group_row_count in the caller), the mark-written
    !> bookkeeping is the first-chunk one, and an empty chunk appends nothing.
    subroutine write_string_chunk_flat(writer, name, flat, nitems, asize, nrows, valid)
        type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        character(len=*), intent(in) :: flat(*) !! the elements to write, (element, row)-major.
        integer(int64), intent(in) :: nitems !! number of elements in `flat`, i.e. asize*nrows.
        integer(int64), intent(in) :: asize !! per-row element count (1 for a scalar column).
        integer(int64), intent(in) :: nrows !! this chunk's post-mask row count.
        logical, intent(in), optional, target :: valid(*) !! `nitems`-long validity mask, or absent.
        character(kind=c_char), allocatable :: packed(:)
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(c_int8_t), allocatable, target :: valid_buf(:)
        type(c_ptr) :: valid_ptr
        logical, pointer :: vmask(:) !! the validity mask actually written, or disassociated.
        integer(int64) :: i, k
        integer :: j, item_len

        item_len = len(flat)
        allocate(packed(item_len*nitems))
        k = 0_int64
        do i = 1_int64, nitems
            do j = 1, item_len
                k = k + 1_int64
                packed(k) = achar(iachar(flat(i)(j:j)), kind=c_char)
            end do
        end do

        nullify(vmask)
        if (present(valid)) vmask => valid(1:nitems)
        if (associated(vmask)) then
            call parquet_check_protected(writer, name, vmask)
            call parquet_check_qc_miss(writer, name, vmask)
        end if
        call parquet_check_qc_string(writer, name, flat(1:nitems), vmask)
        call parquet_make_valid_buf_write(vmask, valid_buf, valid_ptr)
        call parquet_chunk_mark_written_if_first(writer, name)

        if (nrows > 0) then
            call parquet_resolve_output_name(writer, name, outname)
            if (asize == 1) then
                call parquet_append_string_column_chunk(writer%handle, trim(outname)//char(0), packed, &
                    int(item_len, kind=c_long_long), valid_ptr)
            else
                call parquet_append_string_array_column_chunk(writer%handle, trim(outname)//char(0), packed, &
                    int(item_len, kind=c_long_long), asize, valid_ptr)
            end if
        end if
    end subroutine write_string_chunk_flat
    !> Whole-column tail for parquet_write_string_column_compact: the validity/qc checks and the
    !> buffer handoff, given whichever parquet_string_column is actually being written -- the
    !> caller's own when no row mask is in force, or the row-masked rebuild when one is.
    subroutine write_string_compact_tail(writer, name, col, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_string_column), intent(in), target :: col !! the column actually being written.
        integer(int64), intent(in) :: nrows !! its row count.
        logical, allocatable :: is_valid_flat(:)
        type(c_ptr) :: offsets_ptr, data_ptr, validity_ptr
        logical :: has_validity
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: i, nr, nchars

        call parquet_check_row_count(writer, name, nrows)

        if (col%null_count() > 0_int64) then
            allocate(is_valid_flat(nrows))
            do i = 1_int64, nrows
                is_valid_flat(i) = .not. col%is_null(i)
            end do
            call parquet_check_protected(writer, name, is_valid_flat)
            call parquet_check_qc_miss(writer, name, is_valid_flat)
        end if
        call parquet_check_qc_string_compact(writer, name, col)

        call col%raw_buffers(offsets_ptr, data_ptr, validity_ptr, nr, nchars, has_validity)
        call parquet_resolve_output_name(writer, name, outname)
        call parquet_append_string_column_buffers(writer%handle, trim(outname)//char(0), &
            nr, nchars, offsets_ptr, data_ptr, validity_ptr)
    end subroutine write_string_compact_tail
    !> Row-group-chunked counterpart of write_string_compact_tail.
    subroutine write_string_compact_chunk_tail(writer, name, col, nrows)
        type(parquet_writer), intent(inout) :: writer !! open writer with a row group open.
        character(len=*), intent(in) :: name !! column name.
        type(parquet_string_column), intent(in), target :: col !! the column actually being written.
        integer(int64), intent(in) :: nrows !! this chunk's post-mask row count.
        logical, allocatable :: is_valid_flat(:)
        type(c_ptr) :: offsets_ptr, data_ptr, validity_ptr
        logical :: has_validity
        character(len=:), allocatable :: outname !! parquet_resolve_output_name scratch.
        integer(int64) :: i, nr, nchars

        if (col%null_count() > 0_int64) then
            allocate(is_valid_flat(nrows))
            do i = 1_int64, nrows
                is_valid_flat(i) = .not. col%is_null(i)
            end do
            call parquet_check_protected(writer, name, is_valid_flat)
            call parquet_check_qc_miss(writer, name, is_valid_flat)
        end if
        call parquet_check_qc_string_compact(writer, name, col)
        call parquet_chunk_mark_written_if_first(writer, name)

        call col%raw_buffers(offsets_ptr, data_ptr, validity_ptr, nr, nchars, has_validity)
        call parquet_resolve_output_name(writer, name, outname)
        if (nrows > 0) call parquet_write_string_column_chunk_buffers(writer%handle, trim(outname)//char(0), &
            nr, nchars, offsets_ptr, data_ptr, validity_ptr)
    end subroutine write_string_compact_chunk_tail
    module procedure parquet_write_string_column
        integer(int64) :: i, nrows, asize, nitems, nkeep
        integer :: idx, max_item_len, max_string_len
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable :: is_valid_c(:) !! masked copy of is_valid; unallocated => absent downstream.
        logical :: masked
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        character(len=:), allocatable :: values_c(:) !! row-masked copy; unused on the fast path.
        call check_writer_open(writer)
        call lk%claim(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        nitems = size(values, kind=int64)
        if (nitems <= 0) return
        call writer_context_suffix(writer, ctx)
        if (mod(nitems, asize) /= 0) error stop &
            "parquet_write_string_column: values size is not divisible by col_size for column " // &
            trim(name) // ctx

        if (writer%is_schema_enforced) then
            call parquet_resolve_or_check_array_size(writer, name, idx, len(values(1)))
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval([(len_trim(values(i)), i=1_int64,nitems)])
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_column: string length exceeds declared array_size for column: " // trim(name)
            end if
        end if

        nrows = nitems / asize

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(values, elem_mask)
            nkeep = count(row_mask, kind=int64)
            if (present(is_valid)) is_valid_c = pack(is_valid, elem_mask)
            call write_string_flat(writer, name, values_c, size(values_c, kind=int64), asize, nkeep, is_valid_c)
        else
            call write_string_flat(writer, name, values, nitems, asize, nrows, is_valid)
        end if
    end procedure parquet_write_string_column
    module procedure parquet_write_string_matrix_column
        integer(int64) :: i, nrows, asize, nitems, nkeep
        integer :: idx, max_item_len, max_string_len
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable :: is_valid_c(:) !! masked copy of is_valid; unallocated => absent downstream.
        logical :: masked
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        character(len=:), allocatable :: values_c(:,:) !! row-masked copy; unused on the fast path.
        call check_writer_open(writer)
        call lk%claim(writer)

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
            call parquet_resolve_or_check_col_size(writer, name, idx, asize, "parquet_write_column")

            call parquet_resolve_or_check_array_size(writer, name, idx, len(values(1, 1)))
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval(len_trim(values))
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_matrix_column: string length exceeds declared array_size for column: " &
                    // trim(name)
            end if
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        nitems = size(values, kind=int64)
        if (nitems <= 0) return

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = reshape(pack(values, spread(row_mask, 1, asize)), [asize, count(row_mask, kind=int64)])
            nkeep = count(row_mask, kind=int64)
            if (present(is_valid)) is_valid_c = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call write_string_flat(writer, name, values_c, size(values_c, kind=int64), asize, nkeep, is_valid_c)
        else
            call write_string_flat(writer, name, values, nitems, asize, nrows, is_valid)
        end if
    end procedure parquet_write_string_matrix_column
    module procedure parquet_write_string_column_compact
        integer(int64) :: nrows, i
        integer :: idx
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:)
        logical :: masked
        type(parquet_string_column) :: values_c !! row-masked rebuild; unused on the fast path.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column: column not defined in parquet_open_writer: " // trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type(writer, name, "string")

        if (writer%is_schema_enforced) then
            if (parquet_get_column_col_size(writer, name) /= 1) then
                call writer_context_suffix(writer, ctx)
                error stop "parquet_write_column: a parquet_string_column write requires a scalar " // &
                    "(col_size=1) column: " // trim(name) // ctx
            end if
        end if

        if (.not. parquet_is_column_enabled(writer, name)) return
        call parquet_mark_column_written(writer, name)

        nrows = values%size()
        if (nrows <= 0_int64) return

        call parquet_writer_whole_column_mask(writer, name, nrows, row_mask, masked)
        if (masked) then
            do i = 1_int64, nrows
                if (.not. row_mask(i)) cycle
                ! `%append_from` carries the element's null state as well as its bytes, so the
                ! is_null fork this replaced is redundant, and no per-row string is materialized.
                call values_c%append_from(values, i)
            end do
            call write_string_compact_tail(writer, name, values_c, count(row_mask, kind=int64))
        else
            ! Nothing to remove, so the caller's own column is written as it stands -- the
            ! rebuild above used to copy every row of it on every unmasked write.
            call write_string_compact_tail(writer, name, values, nrows)
        end if
    end procedure parquet_write_string_column_compact
    module procedure parquet_write_string_column_chunk
        integer(int64) :: i, nrows, asize, nitems, nkeep
        integer :: idx, max_item_len, max_string_len
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable :: is_valid_c(:) !! masked copy of is_valid; unallocated => absent downstream.
        logical :: masked
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        character(len=:), allocatable :: values_c(:) !! row-masked copy; unused on the fast path.
        call check_writer_open(writer)
        call lk%claim(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type_exact(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return

        asize = int(parquet_get_column_col_size(writer, name), kind=int64)
        nitems = size(values, kind=int64)
        if (nitems <= 0) return
        call writer_context_suffix(writer, ctx)
        if (mod(nitems, asize) /= 0) error stop &
            "parquet_write_string_column_chunk: values size is not divisible by col_size for column " // &
            trim(name) // ctx

        if (writer%is_schema_enforced) then
            call parquet_resolve_or_check_array_size(writer, name, idx, len(values(1)))
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval([(len_trim(values(i)), i=1_int64,nitems)])
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_column_chunk: string length exceeds declared array_size " // &
                    "for column: " // trim(name)
            end if
        end if

        nrows = nitems / asize

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = pack(values, elem_mask)
            nkeep = count(row_mask, kind=int64)
            if (present(is_valid)) is_valid_c = pack(is_valid, elem_mask)
            call write_string_chunk_flat(writer, name, values_c, size(values_c, kind=int64), asize, nkeep, is_valid_c)
        else
            call write_string_chunk_flat(writer, name, values, nitems, asize, nrows, is_valid)
        end if
    end procedure parquet_write_string_column_chunk
    module procedure parquet_write_string_matrix_column_chunk
        integer(int64) :: i, nrows, asize, nitems, nkeep
        integer :: idx, max_item_len, max_string_len
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:), elem_mask(:)
        logical, allocatable :: is_valid_c(:) !! masked copy of is_valid; unallocated => absent downstream.
        logical :: masked
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        character(len=:), allocatable :: values_c(:,:) !! row-masked copy; unused on the fast path.
        call check_writer_open(writer)
        call lk%claim(writer)

        asize = size(values, 1, kind=int64)
        nrows = size(values, 2, kind=int64)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
            call parquet_resolve_or_check_col_size(writer, name, idx, asize, "parquet_write_column_chunk")

            call parquet_resolve_or_check_array_size(writer, name, idx, len(values(1, 1)))
            max_string_len = max(1, writer%all_columns(idx)%array_size)
            max_item_len = maxval(len_trim(values))
            if (max_item_len > max_string_len) then
                error stop "parquet_write_string_matrix_column_chunk: string length exceeds declared " // &
                    "array_size for column: " // trim(name)
            end if
        end if

        call parquet_assert_column_type_exact(writer, name, "string")

        if (.not. parquet_is_column_enabled(writer, name)) return

        nitems = size(values, kind=int64)
        if (nitems <= 0) return

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        if (masked) then
            elem_mask = parquet_mask_expand_block(row_mask, asize)
            values_c = reshape(pack(values, spread(row_mask, 1, asize)), [asize, count(row_mask, kind=int64)])
            nkeep = count(row_mask, kind=int64)
            if (present(is_valid)) is_valid_c = pack(reshape(is_valid, [size(is_valid, kind=int64)]), elem_mask)
            call write_string_chunk_flat(writer, name, values_c, size(values_c, kind=int64), asize, nkeep, is_valid_c)
        else
            call write_string_chunk_flat(writer, name, values, nitems, asize, nrows, is_valid)
        end if
    end procedure parquet_write_string_matrix_column_chunk
    module procedure parquet_write_string_column_chunk_compact
        integer(int64) :: nrows, i
        integer :: idx
        character(len=:), allocatable :: ctx !! writer_context_suffix scratch.
        logical, allocatable :: row_mask(:)
        logical :: masked
        type(parquet_string_column) :: values_c !! row-masked rebuild; unused on the fast path.
        type(writer_lock) :: lk !! Releases writer's concurrency guard on every exit path (FINAL).
        call check_writer_open(writer)
        call lk%claim(writer)

        if (writer%is_schema_enforced) then
            idx = parquet_get_defined_column_index(writer, name)
            call writer_context_suffix(writer, ctx)
            if (idx == 0) error stop &
                "parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                trim(name) // ctx
            if (.not. writer%all_columns(idx)%is_set) return
        end if

        call parquet_assert_column_type_exact(writer, name, "string")

        if (writer%is_schema_enforced) then
            if (parquet_get_column_col_size(writer, name) /= 1) then
                call writer_context_suffix(writer, ctx)
                error stop "parquet_write_column_chunk: a parquet_string_column write requires a " // &
                    "scalar (col_size=1) column: " // trim(name) // ctx
            end if
        end if

        if (.not. parquet_is_column_enabled(writer, name)) return

        nrows = values%size()
        if (nrows <= 0_int64) return

        call parquet_check_row_group_row_count(writer, name, nrows, row_mask, masked)
        if (masked) then
            do i = 1_int64, nrows
                if (.not. row_mask(i)) cycle
                ! `%append_from` carries the element's null state as well as its bytes, so the
                ! is_null fork this replaced is redundant, and no per-row string is materialized.
                call values_c%append_from(values, i)
            end do
            call write_string_compact_chunk_tail(writer, name, values_c, count(row_mask, kind=int64))
        else
            ! Nothing to remove, so the caller's own column is written as it stands -- the
            ! rebuild above used to copy every row of it on every unmasked write.
            call write_string_compact_chunk_tail(writer, name, values, nrows)
        end if
    end procedure parquet_write_string_column_chunk_compact

end submodule parquet_write_string
