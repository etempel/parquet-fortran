!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Row selection: building a `parquet_slice` and turning one into the explicit list of rows it
!! picks out. The per-kind copies that consume that list are generated into
!! `parquet_tables_access` next to the table's own `%get`.
!!
!! Everything here is deliberately resolved LATE. A slice stores what it was asked for
!! (`start`, `stop`, `step`, or a list) and nothing about any particular table, so the open-ended
!! `1:` form means "to the end of whatever table you use me on" rather than "to the end of the
!! table that happened to exist when I was built". Validation therefore also happens at use, in
!! `slice_resolve`, which is the single place a row index is checked before anything indexes with
!! it.
submodule (parquet_tables) parquet_tables_slice
    implicit none
    !
contains
    !
    module procedure slice_range_i32
        integer(int64) :: lo, hi, st
        !
        lo = int(start, int64)
        hi = -1_int64
        st = 1_int64
        if (present(stop)) hi = int(stop, int64)
        if (present(step)) st = int(step, int64)
        s = slice_range_i64(lo, hi, st)
    end procedure slice_range_i32
    !
    module procedure slice_range_i64
        s%strided = .true.
        s%start = start
        s%stop = -1_int64
        s%step = 1_int64
        if (present(stop)) s%stop = stop
        if (present(step)) s%step = step
        ! Rejected here rather than at use: a zero step is a mistake in the slice itself, and
        ! every use of it would loop forever or produce nothing.
        if (s%step == 0_int64) then
            error stop EP // "parquet_slice_range: step must not be zero"
        end if
    end procedure slice_range_i64
    !
    module procedure slice_list_i32
        s%strided = .false.
        s%indices = int(indices, int64)
    end procedure slice_list_i32
    !
    module procedure slice_list_i64
        s%strided = .false.
        s%indices = indices
    end procedure slice_list_i64
    !
    module procedure slice_resolve
        integer(int64) :: lo, hi, st, n, k, v
        character(len=32) :: v_s, n_s
        !
        if (.not. s%strided) then
            ! Not reachable through the public API: strided defaults to .true., and the only
            ! two places that ever set it .false. (slice_list_i32/slice_list_i64) always assign
            ! %indices in that same call, even for an empty list (a zero-size array is still
            ! allocated). There is no constructor that leaves a list-form slice's %indices
            ! unallocated.
            if (.not. allocated(s%indices)) then
                allocate(rows(0)) ! GCOVR_EXCL_LINE
                return ! GCOVR_EXCL_LINE
            end if
            rows = s%indices
        else
            lo = s%start
            st = s%step
            ! An omitted `stop` means "to the end", which depends on the table this slice is
            ! being used on -- hence -1 as the stored sentinel and the resolution here. Counting
            ! down, the open end is row 1 rather than the last row.
            hi = s%stop
            if (hi < 0_int64) then
                if (st > 0_int64) then
                    hi = nrows
                else
                    hi = 1_int64
                end if
            end if
            n = 0_int64
            if (st > 0_int64 .and. hi >= lo) n = (hi - lo) / st + 1_int64
            if (st < 0_int64 .and. hi <= lo) n = (lo - hi) / (-st) + 1_int64
            allocate(rows(n))
            do k = 1_int64, n
                rows(k) = lo + (k - 1_int64) * st
            end do
        end if
        ! Validated in one pass before the caller indexes with any of them, so an out-of-range
        ! entry is a clear message here rather than a bounds abort deep inside the value store.
        do k = 1_int64, size(rows, kind=int64)
            v = rows(k)
            if (v < 1_int64 .or. v > nrows) then
                write(v_s, "(I0)") v
                write(n_s, "(I0)") nrows
                error stop EP // trim(proc) // ": slice selects row " // trim(v_s) // &
                    ", outside the table's 1.." // trim(n_s) // " rows"
            end if
        end do
    end procedure slice_resolve
    !
    module procedure slice_kind_error
        character(len=:), allocatable :: sfx, kname
        !
        call table_context_suffix(self%cache, name, sfx)
        call parquet_kind_name(self%cache%cols(idx)%declared_kind, kname)
        error stop EP // "get_slice: column kind (" // kname // ") cannot be copied into this " // &
            "array" // sfx
    end procedure slice_kind_error
    !
end submodule parquet_tables_slice
