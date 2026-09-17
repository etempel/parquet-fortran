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

        !
        allocate(tmp(nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure mat_i32
    !
    module procedure mat_i64
        integer(int64), allocatable :: tmp(:)
        logical, allocatable :: valid(:)

        !
        allocate(tmp(nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure mat_i64
    !
    module procedure mat_f32
        real(real32), allocatable :: tmp(:)
        logical, allocatable :: valid(:)

        !
        allocate(tmp(nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure mat_f32
    !
    module procedure mat_f64
        real(real64), allocatable :: tmp(:)
        logical, allocatable :: valid(:)

        !
        allocate(tmp(nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure mat_f64
    !
    module procedure mat_bool
        logical, allocatable :: tmp(:)
        logical, allocatable :: valid(:)

        !
        allocate(tmp(nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
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
        ! adopt, not init+set_all: the column takes over `tmp`'s allocation instead of allocating
        ! its own and copying into it, which saves a full pass over the data and a second live copy
        ! of the column. `tmp` is a temporary this procedure was about to discard, which is exactly
        ! what adopt is for.
        call col%adopt(tmp, unit)
    end procedure mat_date
    !
    module procedure mat_time
        type(parquet_time), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column(reader, name, tmp)
        ! adopt, not init+set_all: the column takes over `tmp`'s allocation instead of allocating
        ! its own and copying into it, which saves a full pass over the data and a second live copy
        ! of the column. `tmp` is a temporary this procedure was about to discard, which is exactly
        ! what adopt is for.
        call col%adopt(tmp, unit)
    end procedure mat_time
    !
    module procedure mat_ts
        type(parquet_timestamp), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column(reader, name, tmp)
        ! adopt, not init+set_all: the column takes over `tmp`'s allocation instead of allocating
        ! its own and copying into it, which saves a full pass over the data and a second live copy
        ! of the column. `tmp` is a temporary this procedure was about to discard, which is exactly
        ! what adopt is for.
        call col%adopt(tmp, unit)
    end procedure mat_ts
    !
    module procedure mat_i32v
        integer(int32), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)

        !
        allocate(tmp(wdt, nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure mat_i32v
    !
    module procedure mat_i64v
        integer(int64), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)

        !
        allocate(tmp(wdt, nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure mat_i64v
    !
    module procedure mat_f32v
        real(real32), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)

        !
        allocate(tmp(wdt, nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure mat_f32v
    !
    module procedure mat_f64v
        real(real64), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)

        !
        allocate(tmp(wdt, nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure mat_f64v
    !
    module procedure mat_boolv
        logical, allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)

        !
        allocate(tmp(wdt, nrows))
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure mat_boolv
    !
    module procedure mat_strv
        character(len=:), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer :: slen
        !
        ! There is no compact buffer path for a rank-2 string column, so this goes through the
        ! legacy fixed-width reader -- which means trailing blanks cannot be distinguished from
        ! padding and are trimmed. Documented as a known limitation of vector string columns.
        call parquet_get_string_length(reader, name, slen)
        if (slen < 1) slen = 1
        allocate(character(len=slen) :: tmp(wdt, nrows))
        ! No adopt for the string kinds (they own a parquet_string_column, not a plain array), but
        ! the validity work is skipped the same way every other kind skips it -- see mat_f64.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%init(PK_STRING_VEC, nrows, wdt, unit)
            call col%set_all(tmp)
            ! Per ELEMENT, exactly like every other vector kind: the mask is handed over whole
            ! rather than collapsed to one bit per row. set_all above wrote every element, which
            ! cleared the all-null state a freshly initialized string store starts in, so this
            ! records exactly the nulls Arrow reported and nothing else.
            if (.not. all(valid)) call col%set_validity(valid)
        else
            call parquet_read_column(reader, name, tmp)
            call col%init(PK_STRING_VEC, nrows, wdt, unit)
            call col%set_all(tmp)
        end if
    end procedure mat_strv
    !
    module procedure mat_datev
        type(parquet_date), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column(reader, name, tmp)
        ! adopt, not init+set_all: the column takes over `tmp`'s allocation instead of allocating
        ! its own and copying into it, which saves a full pass over the data and a second live copy
        ! of the column. `tmp` is a temporary this procedure was about to discard, which is exactly
        ! what adopt is for.
        call col%adopt(tmp, unit)
    end procedure mat_datev
    !
    module procedure mat_timev
        type(parquet_time), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column(reader, name, tmp)
        ! adopt, not init+set_all: the column takes over `tmp`'s allocation instead of allocating
        ! its own and copying into it, which saves a full pass over the data and a second live copy
        ! of the column. `tmp` is a temporary this procedure was about to discard, which is exactly
        ! what adopt is for.
        call col%adopt(tmp, unit)
    end procedure mat_timev
    !
    module procedure mat_tsv
        type(parquet_timestamp), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column(reader, name, tmp)
        ! adopt, not init+set_all: the column takes over `tmp`'s allocation instead of allocating
        ! its own and copying into it, which saves a full pass over the data and a second live copy
        ! of the column. `tmp` is a temporary this procedure was about to discard, which is exactly
        ! what adopt is for.
        call col%adopt(tmp, unit)
    end procedure mat_tsv
    !
    module procedure matchunk_i32
        integer(int32), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        !
        allocate(tmp(nrows))
        ! Scoped to THIS row group, unlike mat_i32's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_i32
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_i32
    !
    module procedure matchunk_i64
        integer(int64), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        !
        allocate(tmp(nrows))
        ! Scoped to THIS row group, unlike mat_i64's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_i64
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_i64
    !
    module procedure matchunk_f32
        real(real32), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        !
        allocate(tmp(nrows))
        ! Scoped to THIS row group, unlike mat_f32's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_f32
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_f32
    !
    module procedure matchunk_f64
        real(real64), allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        !
        allocate(tmp(nrows))
        ! Scoped to THIS row group, unlike mat_f64's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_f64
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_f64
    !
    module procedure matchunk_bool
        logical, allocatable :: tmp(:)
        logical, allocatable :: valid(:)
        !
        allocate(tmp(nrows))
        ! Scoped to THIS row group, unlike mat_bool's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_bool
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
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
        call col%adopt(tmp, unit)
    end procedure matchunk_date
    !
    module procedure matchunk_time
        type(parquet_time), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%adopt(tmp, unit)
    end procedure matchunk_time
    !
    module procedure matchunk_ts
        type(parquet_timestamp), allocatable :: tmp(:)
        !
        allocate(tmp(nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%adopt(tmp, unit)
    end procedure matchunk_ts
    !
    module procedure matchunk_i32v
        integer(int32), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        !
        allocate(tmp(wdt, nrows))
        ! Scoped to THIS row group, unlike mat_i32v's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_i32v
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_i32v
    !
    module procedure matchunk_i64v
        integer(int64), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        !
        allocate(tmp(wdt, nrows))
        ! Scoped to THIS row group, unlike mat_i64v's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_i64v
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_i64v
    !
    module procedure matchunk_f32v
        real(real32), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        !
        allocate(tmp(wdt, nrows))
        ! Scoped to THIS row group, unlike mat_f32v's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_f32v
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_f32v
    !
    module procedure matchunk_f64v
        real(real64), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        !
        allocate(tmp(wdt, nrows))
        ! Scoped to THIS row group, unlike mat_f64v's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_f64v
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_f64v
    !
    module procedure matchunk_boolv
        logical, allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        !
        allocate(tmp(wdt, nrows))
        ! Scoped to THIS row group, unlike mat_boolv's whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_boolv
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
                call col%set_validity(valid)
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_boolv
    !
    module procedure matchunk_strv
        character(len=:), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer :: slen
        !
        ! parquet_get_string_length is a whole-column question, so this asks for the longest
        ! element anywhere in the column rather than in this row group. That over-allocates the
        ! buffer slightly and is otherwise harmless: the padded path trims on the way in anyway.
        call parquet_get_string_length(reader, name, slen)
        if (slen < 1) slen = 1
        allocate(character(len=slen) :: tmp(wdt, nrows))
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%init(PK_STRING_VEC, nrows, wdt, unit)
            call col%set_all(tmp)
            ! Per element, as in mat_strv -- see its note.
            if (.not. all(valid)) call col%set_validity(valid)
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%init(PK_STRING_VEC, nrows, wdt, unit)
            call col%set_all(tmp)
        end if
    end procedure matchunk_strv
    !
    module procedure matchunk_datev
        type(parquet_date), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%adopt(tmp, unit)
    end procedure matchunk_datev
    !
    module procedure matchunk_timev
        type(parquet_time), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%adopt(tmp, unit)
    end procedure matchunk_timev
    !
    module procedure matchunk_tsv
        type(parquet_timestamp), allocatable :: tmp(:,:)
        !
        allocate(tmp(wdt, nrows))
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%adopt(tmp, unit)
    end procedure matchunk_tsv
    !
    module procedure mat_list
        class(parquet_container_column), allocatable :: tmp
        !
        ! Allocated as the ABSTRACT class holding a parquet_list_column, rather than read into a concrete
        ! local and copied into a box afterwards: adopt_container MOVES what it is given, so this
        ! shape hands the reader's own allocation straight to the column with no second live copy
        ! of the payload. `allocate(box, source=concrete)` would deep-copy the whole column.
        allocate(parquet_list_column :: tmp)
        select type (dest => tmp)
        type is (parquet_list_column)
            call parquet_read_column(reader, name, dest)
        end select
        ! nrows/wdt/unit are deliberately unread here -- see this procedure's interface.
        call col%adopt_container(tmp)
    end procedure mat_list
    !
    module procedure mat_map
        class(parquet_container_column), allocatable :: tmp
        !
        ! Allocated as the ABSTRACT class holding a parquet_map_column, rather than read into a concrete
        ! local and copied into a box afterwards: adopt_container MOVES what it is given, so this
        ! shape hands the reader's own allocation straight to the column with no second live copy
        ! of the payload. `allocate(box, source=concrete)` would deep-copy the whole column.
        allocate(parquet_map_column :: tmp)
        select type (dest => tmp)
        type is (parquet_map_column)
            call parquet_read_column(reader, name, dest)
        end select
        ! nrows/wdt/unit are deliberately unread here -- see this procedure's interface.
        call col%adopt_container(tmp)
    end procedure mat_map
    !
    module procedure matchunk_list
        class(parquet_container_column), allocatable :: tmp
        !
        allocate(parquet_list_column :: tmp)
        select type (dest => tmp)
        type is (parquet_list_column)
            call parquet_read_column_chunk(reader, name, rg, dest)
        end select
        call col%adopt_container(tmp)
    end procedure matchunk_list
    !
    module procedure matchunk_map
        class(parquet_container_column), allocatable :: tmp
        !
        allocate(parquet_map_column :: tmp)
        select type (dest => tmp)
        type is (parquet_map_column)
            call parquet_read_column_chunk(reader, name, rg, dest)
        end select
        call col%adopt_container(tmp)
    end procedure matchunk_map
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
        case (PK_LIST)
            call mat_list(reader, name, col, nrows, wdt, unit)
        case (PK_MAP)
            call mat_map(reader, name, col, nrows, wdt, unit)
        case default
            ! table_classify only ever assigns a slot one of the supported PK_* kinds handled
            ! above (an unsupported column stays PK_NONE and is never routed to a materializer),
            ! so this branch guards an internal invariant with no path reachable through the
            ! public API -- there is no way to feed it a value that would actually take it.
            !
            ! PK_STRUCT is the one supported kind deliberately absent from the list above, and it
            ! is unreachable here for a reason worth knowing before adding an arm: a struct column
            ! is never CLASSIFIED, because parquet_get_column_names expands a top-level struct into
            ! one dotted path per leaf and so never emits the struct's own name. A PK_STRUCT column
            ! therefore only ever exists in memory (%add_column, %set), where it is RES_FULL from
            ! birth and nothing materializes it.
            error stop EP // "internal: no materializer for this column kind" ! GCOVR_EXCL_LINE
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
        case (PK_LIST)
            call matchunk_list(reader, name, rg, col, nrows, wdt, unit)
        case (PK_MAP)
            call matchunk_map(reader, name, rg, col, nrows, wdt, unit)
        case default
            ! Same internal invariant as table_materialize_kind's own case default above -- not
            ! reachable through the public API.
            error stop EP // "internal: no row-group materializer for this column kind" ! GCOVR_EXCL_LINE
        end select
    end procedure table_materialize_chunk_kind
    !
end submodule parquet_tables_materialize ! GCOVR_EXCL_LINE
