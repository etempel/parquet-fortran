!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Turns a `parquet_table`'s key columns into a row permutation, using the library's own C++
!! `std::sort` engine -- the SAME engine the read-time `sort_by=` runs on.
!!
!! **That sharing is the whole point of this file.** A table can be ordered two ways: by reading
!! a file with `parquet_open_reader(..., sort_by=...)`, or by `%sort_by` on an already-assembled
!! table. If those two used different comparators they could disagree about null placement, NaN
!! placement or tie order, and nothing would report it -- the rows would simply come back in a
!! different order depending on which path a program happened to take. So the values are handed
!! over as plain typed vectors through `parquet_sort_builder_*` and every ordering decision is
!! made in one place (`sort_compare_key`, `parquet_wrapper.cpp`).
!!
!! What this file therefore does NOT do: decide anything about order. It extracts values, says
!! which rows are null, and passes the caller's `descending`/`nulls_first` flags through.
!! This is the one file under `parquet_tables` that reaches into `parquet_bindings` directly
!! rather than going through `parquet`'s public API. That is deliberate and narrow: the sort
!! engine is stateless -- it takes plain typed vectors and knows nothing about readers or files --
!! so there is no `parquet_reader` object for a public `parquet` procedure to take, and adding a
!! public raw-array sorting API here would pre-empt the separate `parquet_sort` module that is
!! already planned for exactly that job (`feature_sort.md`). The dependency is confined to this
!! file so it stays easy to redirect once that module exists.
submodule (parquet_tables) parquet_tables_sort
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_int8_t, c_char
    use parquet_bindings, only : parquet_sort_builder_new, parquet_sort_builder_add_key_int64, &
        parquet_sort_builder_add_key_double, parquet_sort_builder_add_key_string, &
        parquet_sort_builder_build, parquet_sort_builder_free
    implicit none
    !
contains
    !
    module procedure table_build_sort_permutation
        type(c_ptr) :: builder
        integer :: ik, idx
        integer(int64) :: n, status
        logical :: desc, nulls_lo
        !
        n = self%row_count
        builder = parquet_sort_builder_new(n)
        do ik = 1, size(keys)
            call sort_lookup_key(self, keys(ik), idx)
            desc = .false.
            if (present(descending)) desc = descending(ik)
            nulls_lo = .false.
            if (present(nulls_first)) nulls_lo = nulls_first(ik)
            call sort_add_key(builder, self%cache%cols(idx)%values, n, desc, nulls_lo)
        end do
        allocate(perm(max(n, 1_int64)))
        status = parquet_sort_builder_build(builder, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which sort_by rejects before it gets here --
            ! kept because the C++ side reports it and silently ignoring a nonzero status is how
            ! a real failure would go unnoticed later.
            error stop EP // "sort_by: the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure table_build_sort_permutation
    !
    !> Resolves one key name to its slot, refusing every column that cannot be a sort key.
    !!
    !! An unmaterialized key column is refused rather than read (Q3c-6). Sorting already forces
    !! whole-column reads, and letting it ALSO pull columns off disk would make the memory a
    !! `%sort_by` call costs depend on which columns happen to be resident -- worst of all on a
    !! slice-regime table over a large file. `%prefetch` is one line and says what it does.
    subroutine sort_lookup_key(self, name, idx)
        class(parquet_table), intent(in) :: self !! the table.
        character(len=*), intent(in) :: name     !! the key column's name.
        integer, intent(out) :: idx              !! its slot index.
        character(len=:), allocatable :: sfx, kname
        !
        call table_lookup_or_fail(self, name, "sort_by", idx)
        if (.not. self%cache%cols(idx)%supported) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "sort_by: this column's type is not supported by parquet_table, " // &
                "so it cannot be a sort key" // sfx
        end if
        if (self%cache%cols(idx)%residency /= RES_FULL) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "sort_by: this column has not been read yet, and sorting will not " // &
                "read it implicitly; call table%prefetch(...) on every key column first" // sfx
        end if
        if (.not. sort_kind_is_orderable(self%cache%cols(idx)%values%kindof())) then
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "sort_by: a " // kname // " column cannot be a sort key; there is " // &
                "no defined order on a whole vector row" // sfx
        end if
    end subroutine sort_lookup_key
    !
    !> Whether a PK_* kind can be a sort key: every SCALAR kind can, no *_VEC kind can.
    pure logical function sort_kind_is_orderable(kind) result(ok)
        integer, intent(in) :: kind !! the PK_* discriminator to test.
        ok = kind == PK_INT32 .or. kind == PK_INT64 .or. kind == PK_FLOAT32 .or. &
            kind == PK_FLOAT64 .or. kind == PK_LOGICAL .or. kind == PK_STRING .or. &
            kind == PK_DATE .or. kind == PK_TIME .or. kind == PK_TIMESTAMP
    end function sort_kind_is_orderable
    !
    !> Extracts one column into the builder as one key -- or, for a timestamp, as TWO.
    !!
    !! A `parquet_timestamp` holds (seconds, nanoseconds). Folding those into a single int64 as
    !! `s * 10**9 + ns` overflows outside roughly 1678-2262, which is well inside the range this
    !! library handles, so instead the seconds go in as one key and the nanoseconds as a second
    !! key with the same order flag. That is exact at every range, needs nothing new from the
    !! engine, and reuses the multi-key comparator that already exists. The null tier comes from
    !! the first of the pair and the second carries the same validity, so the two always agree.
    subroutine sort_add_key(builder, col, n, descending, nulls_first)
        type(c_ptr), intent(in) :: builder             !! the C++ builder handle.
        type(parquet_column), intent(inout) :: col     !! the key column (its null cache may refresh).
        integer(int64), intent(in) :: n                !! row count.
        logical, intent(in) :: descending              !! .true. for descending order.
        logical, intent(in) :: nulls_first             !! .true. to place nulls before values.
        integer(int64), allocatable, target :: ints(:), secs(:), nanos(:), offsets(:)
        real(real64), allocatable, target :: reals(:)
        integer(c_int8_t), allocatable, target :: valid(:)
        character(kind=c_char), allocatable, target :: data(:)
        type(c_ptr) :: valid_ptr
        integer(c_int8_t) :: desc_flag, nulls_flag
        !
        call sort_valid_flags(col, n, valid)
        ! c_loc is taken HERE, on this procedure's own local, rather than inside sort_valid_flags
        ! on a dummy: the pointer has to stay good for the whole of this procedure, and taking it
        ! from the variable that actually owns the storage makes that obvious rather than subtle.
        valid_ptr = c_null_ptr
        if (allocated(valid)) valid_ptr = c_loc(valid)
        desc_flag = merge(1_c_int8_t, 0_c_int8_t, descending)
        nulls_flag = merge(1_c_int8_t, 0_c_int8_t, nulls_first)
        select case (col%kindof())
        case (PK_TIMESTAMP)
            call sort_extract_timestamp(col, n, secs, nanos)
            call parquet_sort_builder_add_key_int64(builder, secs, valid_ptr, desc_flag, nulls_flag)
            call parquet_sort_builder_add_key_int64(builder, nanos, valid_ptr, desc_flag, nulls_flag)
        case (PK_FLOAT32, PK_FLOAT64)
            call sort_extract_real(col, n, reals)
            call parquet_sort_builder_add_key_double(builder, reals, valid_ptr, desc_flag, nulls_flag)
        case (PK_STRING)
            call sort_extract_string(col, n, offsets, data)
            call parquet_sort_builder_add_key_string(builder, offsets, data, valid_ptr, desc_flag, nulls_flag)
        case default
            call sort_extract_integer(col, n, ints)
            call parquet_sort_builder_add_key_int64(builder, ints, valid_ptr, desc_flag, nulls_flag)
        end select
    end subroutine sort_add_key
    !
    !> Builds the per-row validity flags the builder wants, leaving `valid` UNALLOCATED when the
    !! column has no nulls at all.
    !!
    !! That unallocated result is the whole point: the caller turns it into a null pointer, which
    !! the engine reads as "no nulls" and takes its own fast path for. `%row_validity` already
    !! reports a null-free column the same way, by leaving its own result unallocated, so the two
    !! conventions line up with no special case in between.
    subroutine sort_valid_flags(col, n, valid)
        type(parquet_column), intent(inout) :: col                  !! the key column.
        integer(int64), intent(in) :: n                             !! row count.
        integer(c_int8_t), allocatable, intent(out) :: valid(:)     !! 1 per valid row, or unallocated.
        logical, allocatable :: mask(:)
        integer(int64) :: k
        !
        call col%row_validity(mask)
        if (.not. allocated(mask)) return
        allocate(valid(max(n, 1_int64)))
        valid = 1_c_int8_t
        do k = 1_int64, n
            valid(k) = merge(1_c_int8_t, 0_c_int8_t, mask(k))
        end do
    end subroutine sort_valid_flags
    !
    !> Reads every integer-valued scalar kind (including logical and the two date/time kinds) as
    !! int64. Their stored values order exactly as the values they represent, which is the same
    !! reduction the Arrow-side binder makes.
    subroutine sort_extract_integer(col, n, ints)
        type(parquet_column), intent(in) :: col                  !! the key column.
        integer(int64), intent(in) :: n                          !! row count.
        integer(int64), allocatable, target, intent(out) :: ints(:) !! one key value per row.
        integer(int64) :: k
        integer(int32) :: v32
        logical :: b
        type(parquet_date) :: d
        type(parquet_time) :: tm
        !
        allocate(ints(max(n, 1_int64)))
        ints = 0_int64
        do k = 1_int64, n
            select case (col%kindof())
            case (PK_INT32)
                call col%get_at(k, v32)
                ints(k) = int(v32, int64)
            case (PK_INT64)
                call col%get_at(k, ints(k))
            case (PK_LOGICAL)
                call col%get_at(k, b)
                ints(k) = merge(1_int64, 0_int64, b)
            case (PK_DATE)
                call col%get_at(k, d)
                ints(k) = int(d%raw(), int64)
            case default
                call col%get_at(k, tm)
                ints(k) = tm%raw()
            end select
        end do
    end subroutine sort_extract_integer
    !
    !> Reads a float32 or float64 column as real64. NaNs are passed straight through: the engine
    !! tiers them itself, exactly as it does for a read-time sort.
    subroutine sort_extract_real(col, n, reals)
        type(parquet_column), intent(in) :: col                  !! the key column.
        integer(int64), intent(in) :: n                          !! row count.
        real(real64), allocatable, target, intent(out) :: reals(:) !! one key value per row.
        integer(int64) :: k
        real(real32) :: r32
        !
        allocate(reals(max(n, 1_int64)))
        reals = 0.0_real64
        do k = 1_int64, n
            if (col%kindof() == PK_FLOAT32) then
                call col%get_at(k, r32)
                reals(k) = real(r32, real64)
            else
                call col%get_at(k, reals(k))
            end if
        end do
    end subroutine sort_extract_real
    !
    !> Splits every timestamp into its (seconds, nanoseconds) pair -- see `sort_add_key` for why
    !! a timestamp becomes two integer keys rather than one.
    subroutine sort_extract_timestamp(col, n, secs, nanos)
        type(parquet_column), intent(in) :: col                   !! the key column.
        integer(int64), intent(in) :: n                           !! row count.
        integer(int64), allocatable, target, intent(out) :: secs(:)  !! whole seconds per row.
        integer(int64), allocatable, target, intent(out) :: nanos(:) !! sub-second part per row.
        integer(int64) :: k, s
        integer(int32) :: ns
        type(parquet_timestamp) :: ts
        !
        allocate(secs(max(n, 1_int64)), nanos(max(n, 1_int64)))
        secs = 0_int64
        nanos = 0_int64
        do k = 1_int64, n
            call col%get_at(k, ts)
            call ts%get_raw(s, ns)
            secs(k) = s
            nanos(k) = int(ns, int64)
        end do
    end subroutine sort_extract_timestamp
    !
    !> Packs a string column into the (offsets, data) pair the builder takes: row k occupies
    !! `data(offsets(k)+1 : offsets(k+1))`, with `offsets` 0-based because the C++ side indexes
    !! with it directly.
    !!
    !! The bytes are copied out through `%get_at` rather than borrowed from the string column's
    !! own buffers. A borrow would save one pass, but it would tie this file to the embedded
    !! store's internal offset width and layout, and the values are about to be permuted anyway
    !! -- so the copy buys independence from a detail that is not this file's business.
    subroutine sort_extract_string(col, n, offsets, data)
        type(parquet_column), intent(in) :: col                     !! the key column.
        integer(int64), intent(in) :: n                             !! row count.
        integer(int64), allocatable, target, intent(out) :: offsets(:) !! n+1 byte offsets.
        character(kind=c_char), allocatable, target, intent(out) :: data(:) !! the packed bytes.
        character(len=:), allocatable :: s
        integer(int64) :: k, total, pos, j
        !
        allocate(offsets(n + 1_int64))
        offsets(1) = 0_int64
        total = 0_int64
        do k = 1_int64, n
            call col%get_at(k, s)
            total = total + int(len(s), int64)
            offsets(k + 1_int64) = total
        end do
        allocate(data(max(total, 1_int64)))
        pos = 0_int64
        do k = 1_int64, n
            call col%get_at(k, s)
            do j = 1_int64, int(len(s), int64)
                data(pos + j) = s(j:j)
            end do
            pos = pos + int(len(s), int64)
        end do
    end subroutine sort_extract_string
    !
end submodule parquet_tables_sort
