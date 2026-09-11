!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Several columns at once: `%get_matrix`, `%set_matrix`, `%drop_columns` and `%keep_columns`.
!!
!! Two pairs that share nothing but their arity. `%get_matrix`/`%set_matrix` move a group of
!! scalar columns through one `(column, row)` array and change no row and no slot;
!! `%drop_columns`/`%keep_columns` change which slots exist and move no value. They live
!! together because both answer "these columns, as a group" and both take their name list
!! through the same tokenizer.
!!
!! **Column-major is the whole point of the matrix pair.** `arr(k, i)` is column `names(k)`'s
!! value at row `i`, so one ROW's values across the group are contiguous and
!! `count(arr > lim, dim=1)` counts per row. A loop of `%get` produces the transpose of that,
!! one allocation per column, and then needs a second pass to answer the same question.
!!
!! **The widening rule is `%get`'s and `%set`'s, borrowed rather than restated.** `%get_matrix`
!! accepts a column of the matrix's kind, plus `int32` into an `int64` matrix and `float32` into
!! a `real64` one -- and nothing else, because that is the set `get_arr_*` widens.
!! `%set_matrix` accepts the matrix's kind exactly, because `set_arr_*` narrows nothing. The two
!! are therefore an identity precisely when every named column is already the matrix's own kind,
!! which is stated on both bindings rather than left to be discovered.
!!
!! **Neither pair detaches.** The matrix pair writes in place, so no `%col` pointer dies either;
!! the drop pair moves slots without touching a row, so a table that has not read a column can
!! still read it afterwards -- but every outstanding `%col` pointer and column handle is dead,
!! since a slot that shifted down is a different column at that index. That is the union rule
!! CLAUDE.md records: changing the row set is one half of pointer invalidation and reallocating
!! storage is the other, and a slot shift is the second without the first, so both drop verbs
!! bump `%generation()` and neither calls `table_detach`.
submodule (parquet_tables) parquet_tables_matrix
    implicit none

contains

    ! ---- %get_matrix: five kinds x two name forms ---------------------------------------------
    !
    ! Each body is the same five steps -- resolve every name, admit or refuse every column's kind,
    ! size the output, copy one column per row of the matrix, and lift the validity mask beside it
    ! -- differing only in the kinds it admits. Refusing in its own pass is what lets both verbs
    ! promise that a list with one unusable column moves nothing, and what makes the copy loop's
    ! dispatch exhaustive rather than merely covered. The `_string` half splits and calls the
    ! generic, which is how every other name-list pair in this layer is built (%require_columns,
    ! %fillna, %sort_by, %join).

    module procedure get_matrix_i32
        integer, allocatable :: slots(:)
        integer(int32), pointer :: p(:)
        integer :: k
        !
        call matrix_prepare(self, names, "get_matrix", slots, writing=.false.)
        call matrix_check_kinds(self, slots, names, [PK_INT32], "int32", "get_matrix")
        allocate(arr(size(slots, kind=int64), self%row_count))
        if (present(is_valid)) allocate(is_valid(size(slots, kind=int64), self%row_count))
        do k = 1, size(slots)
            call parquet_column_data_ptr(self%cache%cols(slots(k))%values, p)
            arr(k, :) = p
            call matrix_lift_mask(self, slots(k), k, is_valid)
        end do
    end procedure get_matrix_i32
    !
    module procedure get_matrix_string_i32
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%get_matrix(toks, arr, is_valid)
    end procedure get_matrix_string_i32
    !
    module procedure get_matrix_i64
        integer, allocatable :: slots(:)
        integer(int64), pointer :: p(:)
        integer(int32), pointer :: p_i32(:)
        integer :: k
        !
        call matrix_prepare(self, names, "get_matrix", slots, writing=.false.)
        call matrix_check_kinds(self, slots, names, [PK_INT64, PK_INT32], "int64", "get_matrix")
        allocate(arr(size(slots, kind=int64), self%row_count))
        if (present(is_valid)) allocate(is_valid(size(slots, kind=int64), self%row_count))
        do k = 1, size(slots)
            ! No `case default`: matrix_check_kinds has already refused every other kind, so the
            ! widened arm IS the default and there is nothing left for a third one to catch.
            select case (self%cache%cols(slots(k))%values%kindof())
            case (PK_INT32)
                call parquet_column_data_ptr(self%cache%cols(slots(k))%values, p_i32)
                arr(k, :) = p_i32
            case default
                call parquet_column_data_ptr(self%cache%cols(slots(k))%values, p)
                arr(k, :) = p
            end select
            call matrix_lift_mask(self, slots(k), k, is_valid)
        end do
    end procedure get_matrix_i64
    !
    module procedure get_matrix_string_i64
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%get_matrix(toks, arr, is_valid)
    end procedure get_matrix_string_i64
    !
    module procedure get_matrix_f32
        integer, allocatable :: slots(:)
        real(real32), pointer :: p(:)
        integer :: k
        !
        call matrix_prepare(self, names, "get_matrix", slots, writing=.false.)
        call matrix_check_kinds(self, slots, names, [PK_FLOAT32], "float32", "get_matrix")
        allocate(arr(size(slots, kind=int64), self%row_count))
        if (present(is_valid)) allocate(is_valid(size(slots, kind=int64), self%row_count))
        do k = 1, size(slots)
            call parquet_column_data_ptr(self%cache%cols(slots(k))%values, p)
            arr(k, :) = p
            call matrix_lift_mask(self, slots(k), k, is_valid)
        end do
    end procedure get_matrix_f32
    !
    module procedure get_matrix_string_f32
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%get_matrix(toks, arr, is_valid)
    end procedure get_matrix_string_f32
    !
    module procedure get_matrix_f64
        integer, allocatable :: slots(:)
        real(real64), pointer :: p(:)
        real(real32), pointer :: p_f32(:)
        integer :: k
        !
        call matrix_prepare(self, names, "get_matrix", slots, writing=.false.)
        call matrix_check_kinds(self, slots, names, [PK_FLOAT64, PK_FLOAT32], "float64", "get_matrix")
        allocate(arr(size(slots, kind=int64), self%row_count))
        if (present(is_valid)) allocate(is_valid(size(slots, kind=int64), self%row_count))
        do k = 1, size(slots)
            ! No `case default`: matrix_check_kinds has already refused every other kind, so the
            ! widened arm IS the default and there is nothing left for a third one to catch.
            select case (self%cache%cols(slots(k))%values%kindof())
            case (PK_FLOAT32)
                call parquet_column_data_ptr(self%cache%cols(slots(k))%values, p_f32)
                arr(k, :) = p_f32
            case default
                call parquet_column_data_ptr(self%cache%cols(slots(k))%values, p)
                arr(k, :) = p
            end select
            call matrix_lift_mask(self, slots(k), k, is_valid)
        end do
    end procedure get_matrix_f64
    !
    module procedure get_matrix_string_f64
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%get_matrix(toks, arr, is_valid)
    end procedure get_matrix_string_f64
    !
    module procedure get_matrix_bool
        integer, allocatable :: slots(:)
        logical, pointer :: p(:)
        integer :: k
        !
        call matrix_prepare(self, names, "get_matrix", slots, writing=.false.)
        call matrix_check_kinds(self, slots, names, [PK_LOGICAL], "logical", "get_matrix")
        allocate(arr(size(slots, kind=int64), self%row_count))
        if (present(is_valid)) allocate(is_valid(size(slots, kind=int64), self%row_count))
        do k = 1, size(slots)
            call parquet_column_data_ptr(self%cache%cols(slots(k))%values, p)
            arr(k, :) = p
            call matrix_lift_mask(self, slots(k), k, is_valid)
        end do
    end procedure get_matrix_bool
    !
    module procedure get_matrix_string_bool
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%get_matrix(toks, arr, is_valid)
    end procedure get_matrix_string_bool

    ! ---- %set_matrix: the same ten, writing --------------------------------------------------
    !
    ! Every one runs the same three checks before it writes anything -- the shape of `arr` and of
    ! `is_valid` against the name list and the table, then every column's kind -- so a rejected
    ! call changes nothing at all, not even the columns that came before the offending one. The
    ! per-column write itself is `%set`'s, reached through the
    ! same `set_all` + `table_apply_valid` pair `set_arr_*` uses, in that order and for that
    ! procedure's stated reason: a whole-column `set_all` drops the null bitmap by default, so
    ! marking the nulls first would leave nothing behind.

    module procedure set_matrix_i32
        integer, allocatable :: slots(:)
        integer(int32), allocatable :: buf(:)
        integer :: k
        !
        call matrix_prepare_write(self, names, arr_shape(size(arr, 1), size(arr, 2, kind=int64)), &
            "set_matrix", slots, is_valid_shaped(is_valid))
        call matrix_check_kinds(self, slots, names, [PK_INT32], "int32", "set_matrix")
        do k = 1, size(slots)
            buf = arr(k, :)
            call self%cache%cols(slots(k))%values%set_all(buf, modify_nulls)
            call matrix_apply_mask(self, slots(k), k, trim(names(k)), is_valid)
            self%cache%cols(slots(k))%user_populated = .true.
        end do
    end procedure set_matrix_i32
    !
    module procedure set_matrix_string_i32
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%set_matrix(toks, arr, is_valid, modify_nulls)
    end procedure set_matrix_string_i32
    !
    module procedure set_matrix_i64
        integer, allocatable :: slots(:)
        integer(int64), allocatable :: buf(:)
        integer :: k
        !
        call matrix_prepare_write(self, names, arr_shape(size(arr, 1), size(arr, 2, kind=int64)), &
            "set_matrix", slots, is_valid_shaped(is_valid))
        call matrix_check_kinds(self, slots, names, [PK_INT64], "int64", "set_matrix")
        do k = 1, size(slots)
            buf = arr(k, :)
            call self%cache%cols(slots(k))%values%set_all(buf, modify_nulls)
            call matrix_apply_mask(self, slots(k), k, trim(names(k)), is_valid)
            self%cache%cols(slots(k))%user_populated = .true.
        end do
    end procedure set_matrix_i64
    !
    module procedure set_matrix_string_i64
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%set_matrix(toks, arr, is_valid, modify_nulls)
    end procedure set_matrix_string_i64
    !
    module procedure set_matrix_f32
        integer, allocatable :: slots(:)
        real(real32), allocatable :: buf(:)
        integer :: k
        !
        call matrix_prepare_write(self, names, arr_shape(size(arr, 1), size(arr, 2, kind=int64)), &
            "set_matrix", slots, is_valid_shaped(is_valid))
        call matrix_check_kinds(self, slots, names, [PK_FLOAT32], "float32", "set_matrix")
        do k = 1, size(slots)
            buf = arr(k, :)
            call self%cache%cols(slots(k))%values%set_all(buf, modify_nulls)
            call matrix_apply_mask(self, slots(k), k, trim(names(k)), is_valid)
            self%cache%cols(slots(k))%user_populated = .true.
        end do
    end procedure set_matrix_f32
    !
    module procedure set_matrix_string_f32
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%set_matrix(toks, arr, is_valid, modify_nulls)
    end procedure set_matrix_string_f32
    !
    module procedure set_matrix_f64
        integer, allocatable :: slots(:)
        real(real64), allocatable :: buf(:)
        integer :: k
        !
        call matrix_prepare_write(self, names, arr_shape(size(arr, 1), size(arr, 2, kind=int64)), &
            "set_matrix", slots, is_valid_shaped(is_valid))
        call matrix_check_kinds(self, slots, names, [PK_FLOAT64], "float64", "set_matrix")
        do k = 1, size(slots)
            buf = arr(k, :)
            call self%cache%cols(slots(k))%values%set_all(buf, modify_nulls)
            call matrix_apply_mask(self, slots(k), k, trim(names(k)), is_valid)
            self%cache%cols(slots(k))%user_populated = .true.
        end do
    end procedure set_matrix_f64
    !
    module procedure set_matrix_string_f64
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%set_matrix(toks, arr, is_valid, modify_nulls)
    end procedure set_matrix_string_f64
    !
    module procedure set_matrix_bool
        integer, allocatable :: slots(:)
        logical, allocatable :: buf(:)
        integer :: k
        !
        call matrix_prepare_write(self, names, arr_shape(size(arr, 1), size(arr, 2, kind=int64)), &
            "set_matrix", slots, is_valid_shaped(is_valid))
        call matrix_check_kinds(self, slots, names, [PK_LOGICAL], "logical", "set_matrix")
        do k = 1, size(slots)
            buf = arr(k, :)
            call self%cache%cols(slots(k))%values%set_all(buf, modify_nulls)
            call matrix_apply_mask(self, slots(k), k, trim(names(k)), is_valid)
            self%cache%cols(slots(k))%user_populated = .true.
        end do
    end procedure set_matrix_bool
    !
    module procedure set_matrix_string_bool
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%set_matrix(toks, arr, is_valid, modify_nulls)
    end procedure set_matrix_string_bool

    ! ---- %drop_columns / %keep_columns --------------------------------------------------------

    module procedure table_drop_columns
        logical, allocatable :: keep(:)
        logical :: lenient
        integer :: k, idx
        !
        call table_check_not_shared(self, "drop_columns")
        call table_check_open(self, "drop_columns")
        lenient = .false.
        if (present(ignore_missing)) lenient = ignore_missing
        ! Nothing is dropped until every name has been accounted for -- so a list carrying one
        ! typo leaves the table exactly as it was, rather than half-projected.
        if (.not. lenient) call matrix_require_all_present(self, names, "drop_columns")
        allocate(keep(self%cache%ncols))
        keep = .true.
        do k = 1, size(names)
            idx = table_find(self, trim(names(k)))
            if (idx > 0) keep(idx) = .false.
        end do
        call column_keep_apply(self, keep, force, "drop_columns")
    end procedure table_drop_columns
    !
    module procedure table_drop_columns_string
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%drop_columns(toks, force, ignore_missing)
    end procedure table_drop_columns_string
    !
    module procedure table_keep_columns
        logical, allocatable :: keep(:)
        integer :: k, idx
        !
        call table_check_not_shared(self, "keep_columns")
        call table_check_open(self, "keep_columns")
        ! No `ignore_missing` here, so this is unconditional: a projection that silently keeps
        ! fewer columns than it was asked for is one nothing downstream can check.
        call matrix_require_all_present(self, names, "keep_columns")
        allocate(keep(self%cache%ncols))
        keep = .false.
        do k = 1, size(names)
            idx = table_find(self, trim(names(k)))
            if (idx > 0) keep(idx) = .true.
        end do
        call column_keep_apply(self, keep, force, "keep_columns")
    end procedure table_keep_columns
    !
    module procedure table_keep_columns_string
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%keep_columns(toks, force)
    end procedure table_keep_columns_string

    ! ---- shared workers -----------------------------------------------------------------------

    !> Resolves every name to a slot, in order, and refuses a column whose type the table layer
    !! never read.
    !!
    !! The whole list is resolved BEFORE any value moves, which is what lets both matrix verbs
    !! promise that a rejected call changes nothing. Resolving is also what READS a column that
    !! is not resident yet -- the same lazy touch `%get` performs, so `%get_matrix` on a freshly
    !! opened table needs no `%prefetch` first.
    subroutine matrix_prepare(self, names, proc, slots, writing)
        class(parquet_table), intent(in) :: self       !! the table.
        character(len=*), intent(in) :: names(:)       !! the columns named by the caller.
        character(len=*), intent(in) :: proc           !! calling procedure, for every message.
        integer, allocatable, intent(out) :: slots(:)  !! one slot index per name, in order.
        logical, intent(in) :: writing                 !! .true. when values are about to be written.
        character(len=:), allocatable :: name
        integer :: k
        !
        call table_check_open(self, proc)
        allocate(slots(size(names, kind=int64)))
        do k = 1, size(names)
            name = trim(names(k))
            call table_resolve(self, name, proc, slots(k), writing=writing)
            if (.not. self%cache%cols(slots(k))%supported) then
                ! GCOVR_EXCL_START -- a gcov ARTIFACT, not an untested line, and the same one
                ! marked on parquet_tables_fill.f90's copy of this guard. The call IS reached:
                ! test/error_scenarios.f90's `get_matrix_unsupported_column` runs `%get_matrix`
                ! over the map fixture's `m_intkey` and prints "get_matrix: this column's type is
                ! not supported by parquet_table", which table_unsupported_column_abort raises and
                ! nothing else does, with `proc` spelled by this call site alone. gcov does not
                ! count the block, because the callee ends in `error stop` and never returns -- an
                ! inline `error stop` here would be counted, a CALL to one is not.
                call table_unsupported_column_abort(self%cache, slots(k), trim(proc))
                ! GCOVR_EXCL_STOP
            end if
        end do
    end subroutine matrix_prepare
    !
    !> `%set_matrix`'s preparation: the shapes first, then `matrix_prepare`.
    !!
    !! `ncol`/`nrow` arrive already extracted rather than as the array itself, because the five
    !! specifics differ only in that array's type and there is nothing else for a shared worker
    !! to do with it. `mask_shape` is `[0, 0]` when no mask was passed -- an absent optional
    !! cannot be forwarded through a non-optional dummy, and the alternative (five copies of the
    !! same two comparisons) is what this worker exists to avoid.
    subroutine matrix_prepare_write(self, names, arr_dims, proc, slots, mask_dims)
        class(parquet_table), intent(inout) :: self    !! the table.
        character(len=*), intent(in) :: names(:)       !! the columns named by the caller.
        integer(int64), intent(in) :: arr_dims(2)      !! (size(arr, 1), size(arr, 2)).
        character(len=*), intent(in) :: proc           !! calling procedure, for every message.
        integer, allocatable, intent(out) :: slots(:)  !! one slot index per name, in order.
        integer(int64), intent(in) :: mask_dims(2)     !! the mask's shape, or [0, 0] for absent.
        character(len=:), allocatable :: sfx
        character(len=64) :: got, want
        !
        call table_check_open(self, proc)
        if (arr_dims(1) /= size(names, kind=int64)) then
            write(got, "(I0)") arr_dims(1)
            write(want, "(I0)") size(names, kind=int64)
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // trim(proc) // ": the matrix has " // trim(got) // " rows but " // &
                trim(want) // " columns were named; arr is (column, row)" // sfx
        end if
        if (arr_dims(2) /= self%row_count) then
            write(got, "(I0)") arr_dims(2)
            write(want, "(I0)") self%row_count
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // trim(proc) // ": the matrix has " // trim(got) // " rows of " // &
                "values but the table has " // trim(want) // " rows" // sfx
        end if
        if (mask_dims(1) /= 0_int64 .or. mask_dims(2) /= 0_int64) then
            if (mask_dims(1) /= arr_dims(1) .or. mask_dims(2) /= arr_dims(2)) then
                write(got, "(I0,A,I0)") mask_dims(1), " x ", mask_dims(2)
                write(want, "(I0,A,I0)") arr_dims(1), " x ", arr_dims(2)
                call table_context_suffix(self%cache, "", sfx)
                error stop EP // trim(proc) // ": is_valid is shaped " // trim(got) // " but " // &
                    "the matrix is " // trim(want) // " (column x row)" // sfx
            end if
        end if
        call matrix_prepare(self, names, proc, slots, writing=.true.)
    end subroutine matrix_prepare_write
    !
    !> The two extents of `arr` as one array, so the five `%set_matrix` bodies can hand their
    !! differently-typed matrices to one shared worker.
    pure function arr_shape(n1, n2) result(d)
        integer, intent(in) :: n1      !! size(arr, 1).
        integer(int64), intent(in) :: n2 !! size(arr, 2).
        integer(int64) :: d(2)         !! the pair.
        d = [int(n1, int64), n2]
    end function arr_shape
    !
    !> An optional mask's shape, or `[0, 0]` when it was not passed.
    pure function is_valid_shaped(mask) result(d)
        logical, intent(in), optional :: mask(:,:) !! the caller's mask, possibly absent.
        integer(int64) :: d(2)                     !! its shape, or [0, 0].
        d = 0_int64
        if (present(mask)) d = [size(mask, 1, kind=int64), size(mask, 2, kind=int64)]
    end function is_valid_shaped
    !
    !> Aborts unless EVERY resolved column is one of `accepted`, before any value moves.
    !!
    !! A separate pass rather than a test inside the copy loop, so that both verbs can promise
    !! what `%fillna` promises: a list with one unusable column moves nothing at all. Without it a
    !! `%set_matrix` naming three columns would have written two of them before refusing the
    !! third.
    !!
    !! It is also what lets the loops that follow dispatch on the kind with no `case default` to
    !! refuse anything -- every kind that reaches them is one this pass admitted, so the arms are
    !! exhaustive rather than merely covered.
    subroutine matrix_check_kinds(self, slots, names, accepted, word, proc)
        class(parquet_table), intent(in) :: self !! the table.
        integer, intent(in) :: slots(:)          !! the resolved slots, one per name.
        character(len=*), intent(in) :: names(:) !! the caller's names, for the messages.
        integer, intent(in) :: accepted(:)       !! the PK_* kinds this matrix can carry.
        character(len=*), intent(in) :: word     !! the matrix's element kind, spelled out.
        character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        integer :: k
        !
        do k = 1, size(slots)
            if (any(accepted == self%cache%cols(slots(k))%values%kindof())) cycle
            call matrix_kind_error(self%cache, slots(k), trim(names(k)), word, proc)
        end do
    end subroutine matrix_check_kinds
    !
    !> The one message both matrix verbs raise for a column they cannot move.
    !!
    !! A VECTOR column gets its own sentence rather than falling into the general one, because it
    !! is the mistake worth naming: `%get` accepts it happily into a rank-2 array, so a caller who
    !! reaches for `%get_matrix` with one has asked a reasonable question in the wrong verb.
    subroutine matrix_kind_error(cache, idx, name, word, proc)
        type(parquet_table_cache), intent(in) :: cache !! the table's store.
        integer, intent(in) :: idx                     !! slot index.
        character(len=*), intent(in) :: name           !! column name, for the context suffix.
        character(len=*), intent(in) :: word           !! the matrix's element kind, spelled out.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        character(len=:), allocatable :: sfx, kname
        integer :: kd
        !
        kd = cache%cols(idx)%values%kindof()
        call parquet_kind_name(kd, kname)
        call table_context_suffix(cache, name, sfx)
        if (matrix_is_vector(kd)) then
            error stop EP // trim(proc) // ": this is a VECTOR column (" // kname // &
                "), and a matrix holds one value per column per row; read a vector column on " // &
                "its own with %get, which gives it its own (width, rows) array" // sfx
        end if
        error stop EP // trim(proc) // ": column kind (" // kname // ") does not match a " // &
            word // " matrix" // sfx
    end subroutine matrix_kind_error
    !
    !> Whether a PK_* discriminator is one of the nine per-row-width kinds.
    pure logical function matrix_is_vector(kind) result(ok)
        integer, intent(in) :: kind !! the PK_* discriminator to test.
        ok = kind == PK_INT32_VEC .or. kind == PK_INT64_VEC .or. kind == PK_FLOAT32_VEC &
            .or. kind == PK_FLOAT64_VEC .or. kind == PK_LOGICAL_VEC .or. kind == PK_STRING_VEC &
            .or. kind == PK_DATE_VEC .or. kind == PK_TIME_VEC .or. kind == PK_TIMESTAMP_VEC
    end function matrix_is_vector
    !
    !> Lifts one column's per-row validity into row `k` of the caller's mask, when they asked
    !! for one. A no-op otherwise, which is what keeps the null-free read free.
    subroutine matrix_lift_mask(self, idx, k, is_valid)
        class(parquet_table), intent(in) :: self  !! the table.
        integer, intent(in) :: idx                !! slot index.
        integer, intent(in) :: k                  !! this column's row in the mask.
        logical, allocatable, intent(inout), optional :: is_valid(:,:) !! the caller's mask.
        logical, allocatable :: m(:)
        !
        if (.not. present(is_valid)) return
        call table_valid_mask_of(self%cache, idx, m)
        is_valid(k, :) = m
    end subroutine matrix_lift_mask
    !
    !> Applies row `k` of the caller's mask to one column, when they passed one.
    !!
    !! The section is copied into a contiguous local first. `arr(k, :)` is strided, and handing a
    !! strided section to a dummy that requires contiguity makes the compiler copy it anyway --
    !! silently under gfortran, and with an eleven-line traceback per call under an ifx debug
    !! build (CLAUDE.md's warning 406 note). Naming the copy costs the same and says nothing.
    subroutine matrix_apply_mask(self, idx, k, name, is_valid)
        class(parquet_table), intent(inout) :: self !! the table.
        integer, intent(in) :: idx                  !! slot index.
        integer, intent(in) :: k                    !! this column's row in the mask.
        character(len=*), intent(in) :: name        !! column name, for the message.
        logical, intent(in), optional :: is_valid(:,:) !! the caller's mask.
        logical, allocatable :: m(:)
        !
        if (.not. present(is_valid)) return
        m = is_valid(k, :)
        call table_apply_valid(self, idx, m, name, "set_matrix")
    end subroutine matrix_apply_mask
    !
    !> Aborts unless the table has every one of `names`, naming **every** absent one.
    !!
    !! `%require_columns` is not reused directly only because its message names itself; the rule,
    !! the exact matching and the reason for reporting the whole list rather than the first name
    !! are the same, and both truncate to a preview because interpolating unbounded caller text
    !! into an `error stop` corrupts the heap on ifx.
    subroutine matrix_require_all_present(self, names, proc)
        class(parquet_table), intent(in) :: self !! the table.
        character(len=*), intent(in) :: names(:) !! the names to check.
        character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        character(len=:), allocatable :: absent(:), listing, sfx
        integer :: k
        !
        call self%missing_columns(names, absent)
        if (size(absent) == 0) return
        listing = ""
        do k = 1, size(absent)
            if (k > 1) listing = listing // ", "
            listing = listing // trim(absent(k))
            if (len(listing) > 100) then
                listing = listing(1:100) // "..."
                exit
            end if
        end do
        call table_context_suffix(self%cache, "", sfx)
        error stop EP // trim(proc) // ": the table has no column called " // listing // sfx
    end subroutine matrix_require_all_present
    !
    !> Compacts the slot array down to the columns `keep` marks, in one pass.
    !!
    !! The same three steps `%drop_column` performs -- clear a dropped column's values, MOVE the
    !! survivors down over the gaps (never assign them: intrinsic assignment on a
    !! `parquet_table_column` deep-copies its storage, so shifting a 40-column, 8 GB table would
    !! memcpy roughly 8 GB inside the procedure documented as the way to give memory back), and
    !! blank the vacated tail through `reset_column_slot`, the one field list every vacating path
    !! shares. Writing that list out here instead is how the singular verb came to be missing
    !! `unit` (feature_risks.md Risk-204).
    !!
    !! Dropping NOTHING returns before any of that, which is what makes `%keep_columns` naming
    !! every column, and `%drop_columns` with `ignore_missing=` and nothing to skip, leave the
    !! generation untouched -- the negative control both verbs' "changes nothing" test asserts.
    subroutine column_keep_apply(self, keep, force, proc)
        class(parquet_table), intent(inout) :: self !! the table.
        logical, intent(in) :: keep(:)              !! one entry per slot; .false. drops it.
        logical, intent(in), optional :: force      !! .true. to drop PREDEFINED columns.
        character(len=*), intent(in) :: proc        !! calling procedure, for every message.
        character(len=:), allocatable :: sfx
        logical :: forced
        integer :: i, w, n
        !
        n = self%cache%ncols
        if (all(keep(1:n))) return
        forced = .false.
        if (present(force)) forced = force
        ! R8, checked for the whole list before anything moves: a predefined column may be
        ! dropped only on purpose. %keep_columns reaches this by NOT naming one, which is why its
        ! own force= exists at all -- without it a projection would be the one way to lose a
        ! generated type's columns by omission.
        if (.not. forced) then
            do i = 1, n
                if (keep(i) .or. .not. self%cache%cols(i)%predefined) cycle
                call table_context_suffix(self%cache, self%cache%cols(i)%name, sfx)
                error stop EP // trim(proc) // ": this is a predefined column, which a program " // &
                    "using the generated accessors expects to be there; pass force=.true. if " // &
                    "you really mean to drop it (to reclaim memory)" // sfx
            end do
        end if
        w = 0
        do i = 1, n
            if (keep(i)) then
                w = w + 1
                if (w /= i) call move_table_column(self%cache%cols(w), self%cache%cols(i))
            else
                ! Cleared at the moment it is dropped, before any survivor can be moved on top of
                ! it: the write position `w` never runs ahead of `i`, so a slot overwritten later
                ! is always one this branch has already emptied.
                call self%cache%cols(i)%values%clear()
            end if
        end do
        do i = w + 1, n
            call reset_column_slot(self%cache%cols(i))
        end do
        self%cache%ncols = w
        ! Every slot above the first gap is renumbered, so the whole name index is rebuilt rather
        ! than patched -- an incremental fix-up would have to touch most of it anyway.
        call cache_name_index_rebuild(self%cache)
        ! A slot shift invalidates every outstanding %col pointer and column handle without
        ! changing a single row, which is exactly the half of the invalidation rule that is not
        ! a detach. %drop_column bumps for the same reason.
        self%cache%generation = self%cache%generation + 1_int64
    end subroutine column_keep_apply

end submodule parquet_tables_matrix
