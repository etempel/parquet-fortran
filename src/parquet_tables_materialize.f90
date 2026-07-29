!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_tables.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> Per-kind "read one file column into a `parquet_column`" for `parquet_table`.
!!
!! Every numeric/logical/string read passes `is_valid=`, which is what stops the reader from
!! aborting on a Null-containing column: the mask comes back here and is replayed onto the
!! column's own sparse validity. The temporal kinds take no mask -- their null state lives
!! inside each element, which the reader fills directly.
submodule (parquet_tables) parquet_tables_materialize
    implicit none
    !
contains
    !
    module procedure mat_i32
        integer(int32), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_INT32, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure mat_i32
    !
    module procedure mat_i64
        integer(int64), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_INT64, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure mat_i64
    !
    module procedure mat_f32
        real(real32), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_FLOAT32, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure mat_f32
    !
    module procedure mat_f64
        real(real64), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_FLOAT64, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure mat_f64
    !
    module procedure mat_bool
        logical, allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_LOGICAL, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure mat_bool
    !
    module procedure mat_str
        type(parquet_string_column) :: tmp
        type(parquet_string_column), pointer :: dest
        !
        ! The compact offsets+data+validity path: nulls travel with the buffers, so unlike
        ! every other kind this one needs no separate validity replay.
        call parquet_read_column(reader, name, tmp)
        call col%init(PK_STRING, nrows, wdt, unit)
        call col%string_column(dest)
        dest = tmp%clone()
    end procedure mat_str
    !
    module procedure mat_date
        type(parquet_date), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column(reader, name, tmp)
        call col%init(PK_DATE, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure mat_date
    !
    module procedure mat_time
        type(parquet_time), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column(reader, name, tmp)
        call col%init(PK_TIME, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure mat_time
    !
    module procedure mat_ts
        type(parquet_timestamp), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column(reader, name, tmp)
        call col%init(PK_TIMESTAMP, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure mat_ts
    !
    module procedure mat_i32v
        integer(int32), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_INT32_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure mat_i32v
    !
    module procedure mat_i64v
        integer(int64), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_INT64_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure mat_i64v
    !
    module procedure mat_f32v
        real(real32), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_FLOAT32_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure mat_f32v
    !
    module procedure mat_f64v
        real(real64), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_FLOAT64_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure mat_f64v
    !
    module procedure mat_boolv
        logical, allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_LOGICAL_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure mat_boolv
    !
    module procedure mat_strv
        character(len=:), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        integer :: slen
        !
        ! There is no compact buffer path for a rank-2 string column, so this goes through the
        ! legacy fixed-width reader -- which means trailing blanks cannot be distinguished from
        ! padding and are trimmed. Documented as a known limitation of vector string columns.
        call parquet_get_string_length(reader, name, slen)
        if (slen < 1) slen = 1
        allocate(character(len=slen) :: tmp(wdt, nrows))
        allocate(valid(wdt, nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_STRING_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        ! Row-granular, exactly like every other vector kind: set_null's index is bounded by
        ! nrows and nulls the whole row, so a per-element null is widened to the row. A flat
        ! (i-1)*width+e index instead runs straight past nrows and aborts the read.
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure mat_strv
    !
    module procedure mat_datev
        type(parquet_date), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column(reader, name, tmp)
        call col%init(PK_DATE_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure mat_datev
    !
    module procedure mat_timev
        type(parquet_time), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column(reader, name, tmp)
        call col%init(PK_TIME_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure mat_timev
    !
    module procedure mat_tsv
        type(parquet_timestamp), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column(reader, name, tmp)
        call col%init(PK_TIMESTAMP_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure mat_tsv
    !
    module procedure matchunk_i32
        integer(int32), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_INT32, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure matchunk_i32
    !
    module procedure matchunk_i64
        integer(int64), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_INT64, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure matchunk_i64
    !
    module procedure matchunk_f32
        real(real32), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_FLOAT32, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure matchunk_f32
    !
    module procedure matchunk_f64
        real(real64), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_FLOAT64, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure matchunk_f64
    !
    module procedure matchunk_bool
        logical, allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        allocate(tmp(nrows), valid(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_LOGICAL, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do
    end procedure matchunk_bool
    !
    module procedure matchunk_str
        type(parquet_string_column) :: tmp
        type(parquet_string_column), pointer :: dest
        !
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%init(PK_STRING, nrows, wdt, unit)
        call col%string_column(dest)
        dest = tmp%clone()
    end procedure matchunk_str
    !
    module procedure matchunk_date
        type(parquet_date), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%init(PK_DATE, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure matchunk_date
    !
    module procedure matchunk_time
        type(parquet_time), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%init(PK_TIME, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure matchunk_time
    !
    module procedure matchunk_ts
        type(parquet_timestamp), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%init(PK_TIMESTAMP, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure matchunk_ts
    !
    module procedure matchunk_i32v
        integer(int32), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_INT32_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure matchunk_i32v
    !
    module procedure matchunk_i64v
        integer(int64), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_INT64_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure matchunk_i64v
    !
    module procedure matchunk_f32v
        real(real32), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_FLOAT32_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure matchunk_f32v
    !
    module procedure matchunk_f64v
        real(real64), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_FLOAT64_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure matchunk_f64v
    !
    module procedure matchunk_boolv
        logical, allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        !
        allocate(tmp(wdt, nrows), valid(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_LOGICAL_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure matchunk_boolv
    !
    module procedure matchunk_strv
        character(len=:), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        integer :: slen
        !
        ! parquet_get_string_length is a whole-column question, so this asks for the longest
        ! element anywhere in the column rather than in this row group. That over-allocates the
        ! buffer slightly and is otherwise harmless: the padded path trims on the way in anyway.
        call parquet_get_string_length(reader, name, slen)
        if (slen < 1) slen = 1
        allocate(character(len=slen) :: tmp(wdt, nrows))
        allocate(valid(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
        call col%init(PK_STRING_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do
    end procedure matchunk_strv
    !
    module procedure matchunk_datev
        type(parquet_date), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%init(PK_DATE_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure matchunk_datev
    !
    module procedure matchunk_timev
        type(parquet_time), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%init(PK_TIME_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure matchunk_timev
    !
    module procedure matchunk_tsv
        type(parquet_timestamp), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%init(PK_TIMESTAMP_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure matchunk_tsv
    !
    module procedure table_materialize_kind
        select case (kind)
        case (PK_INT32)
            call mat_i32(reader, name, col, nrows, wdt, unit)
        case (PK_INT64)
            call mat_i64(reader, name, col, nrows, wdt, unit)
        case (PK_FLOAT32)
            call mat_f32(reader, name, col, nrows, wdt, unit)
        case (PK_FLOAT64)
            call mat_f64(reader, name, col, nrows, wdt, unit)
        case (PK_LOGICAL)
            call mat_bool(reader, name, col, nrows, wdt, unit)
        case (PK_STRING)
            call mat_str(reader, name, col, nrows, wdt, unit)
        case (PK_DATE)
            call mat_date(reader, name, col, nrows, wdt, unit)
        case (PK_TIME)
            call mat_time(reader, name, col, nrows, wdt, unit)
        case (PK_TIMESTAMP)
            call mat_ts(reader, name, col, nrows, wdt, unit)
        case (PK_INT32_VEC)
            call mat_i32v(reader, name, col, nrows, wdt, unit)
        case (PK_INT64_VEC)
            call mat_i64v(reader, name, col, nrows, wdt, unit)
        case (PK_FLOAT32_VEC)
            call mat_f32v(reader, name, col, nrows, wdt, unit)
        case (PK_FLOAT64_VEC)
            call mat_f64v(reader, name, col, nrows, wdt, unit)
        case (PK_LOGICAL_VEC)
            call mat_boolv(reader, name, col, nrows, wdt, unit)
        case (PK_STRING_VEC)
            call mat_strv(reader, name, col, nrows, wdt, unit)
        case (PK_DATE_VEC)
            call mat_datev(reader, name, col, nrows, wdt, unit)
        case (PK_TIME_VEC)
            call mat_timev(reader, name, col, nrows, wdt, unit)
        case (PK_TIMESTAMP_VEC)
            call mat_tsv(reader, name, col, nrows, wdt, unit)
        case default
            error stop EP // "internal: no materializer for this column kind"
        end select
    end procedure table_materialize_kind
    !
    module procedure table_materialize_chunk_kind
        select case (kind)
        case (PK_INT32)
            call matchunk_i32(reader, name, rg, col, nrows, wdt, unit)
        case (PK_INT64)
            call matchunk_i64(reader, name, rg, col, nrows, wdt, unit)
        case (PK_FLOAT32)
            call matchunk_f32(reader, name, rg, col, nrows, wdt, unit)
        case (PK_FLOAT64)
            call matchunk_f64(reader, name, rg, col, nrows, wdt, unit)
        case (PK_LOGICAL)
            call matchunk_bool(reader, name, rg, col, nrows, wdt, unit)
        case (PK_STRING)
            call matchunk_str(reader, name, rg, col, nrows, wdt, unit)
        case (PK_DATE)
            call matchunk_date(reader, name, rg, col, nrows, wdt, unit)
        case (PK_TIME)
            call matchunk_time(reader, name, rg, col, nrows, wdt, unit)
        case (PK_TIMESTAMP)
            call matchunk_ts(reader, name, rg, col, nrows, wdt, unit)
        case (PK_INT32_VEC)
            call matchunk_i32v(reader, name, rg, col, nrows, wdt, unit)
        case (PK_INT64_VEC)
            call matchunk_i64v(reader, name, rg, col, nrows, wdt, unit)
        case (PK_FLOAT32_VEC)
            call matchunk_f32v(reader, name, rg, col, nrows, wdt, unit)
        case (PK_FLOAT64_VEC)
            call matchunk_f64v(reader, name, rg, col, nrows, wdt, unit)
        case (PK_LOGICAL_VEC)
            call matchunk_boolv(reader, name, rg, col, nrows, wdt, unit)
        case (PK_STRING_VEC)
            call matchunk_strv(reader, name, rg, col, nrows, wdt, unit)
        case (PK_DATE_VEC)
            call matchunk_datev(reader, name, rg, col, nrows, wdt, unit)
        case (PK_TIME_VEC)
            call matchunk_timev(reader, name, rg, col, nrows, wdt, unit)
        case (PK_TIMESTAMP_VEC)
            call matchunk_tsv(reader, name, rg, col, nrows, wdt, unit)
        case default
            error stop EP // "internal: no row-group materializer for this column kind"
        end select
    end procedure table_materialize_chunk_kind
    !
end submodule parquet_tables_materialize
