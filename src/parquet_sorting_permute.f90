!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> `pf_permute` (apply a permutation in place) and `pf_is_sorted` (test an existing order).
!!
!! **`pf_permute` gathers into a fresh array and moves it back**, rather than following cycles in
!! place. Cycle-following would save the temporary, but it needs a "visited" bit per element and
!! would have to be written once per type anyway; the gather is one obvious pass, and for the two
!! container types the work is delegated to their own `%reindex` regardless.
!!
!! **`perm` is validated before anything is written.** An invalid permutation does not fail -- it
!! silently duplicates some elements and drops others, which is precisely the wrong-answer-with-no-
!! symptom class this project guards hardest against. The check is O(n) in front of an O(n)
!! operation, so it is a constant factor rather than a change of complexity, which is why it is on
!! by default; `assume_valid=.true.` skips it for a permutation that came straight from
!! `pf_argsort`. It skips the CONTENTS check only -- `perm`'s length is checked either way, since a
!! short permutation makes the gather read past the end of `values`.
submodule (parquet_sorting) parquet_sorting_permute
    implicit none
    !
contains
    !
    module procedure permute_i32_i32
        integer(int64), allocatable :: p64(:)
        integer(int32), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_i32_i32
    !
    module procedure permute_i32_i64
        integer(int64), allocatable :: p64(:)
        integer(int32), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_i32_i64
    !
    module procedure permute_i64_i32
        integer(int64), allocatable :: p64(:)
        integer(int64), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_i64_i32
    !
    module procedure permute_i64_i64
        integer(int64), allocatable :: p64(:)
        integer(int64), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_i64_i64
    !
    module procedure permute_f32_i32
        integer(int64), allocatable :: p64(:)
        real(real32), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_f32_i32
    !
    module procedure permute_f32_i64
        integer(int64), allocatable :: p64(:)
        real(real32), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_f32_i64
    !
    module procedure permute_f64_i32
        integer(int64), allocatable :: p64(:)
        real(real64), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_f64_i32
    !
    module procedure permute_f64_i64
        integer(int64), allocatable :: p64(:)
        real(real64), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_f64_i64
    !
    module procedure permute_bool_i32
        integer(int64), allocatable :: p64(:)
        logical, allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_bool_i32
    !
    module procedure permute_bool_i64
        integer(int64), allocatable :: p64(:)
        logical, allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_bool_i64
    !
    module procedure permute_chr_i32
        integer(int64), allocatable :: p64(:)
        character(len=len(values)), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(character(len=len(values)) :: tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_chr_i32
    !
    module procedure permute_chr_i64
        integer(int64), allocatable :: p64(:)
        character(len=len(values)), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(character(len=len(values)) :: tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_chr_i64
    !
    module procedure permute_date_i32
        integer(int64), allocatable :: p64(:)
        type(parquet_date), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_date_i32
    !
    module procedure permute_date_i64
        integer(int64), allocatable :: p64(:)
        type(parquet_date), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_date_i64
    !
    module procedure permute_time_i32
        integer(int64), allocatable :: p64(:)
        type(parquet_time), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_time_i32
    !
    module procedure permute_time_i64
        integer(int64), allocatable :: p64(:)
        type(parquet_time), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_time_i64
    !
    module procedure permute_ts_i32
        integer(int64), allocatable :: p64(:)
        type(parquet_timestamp), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_ts_i32
    !
    module procedure permute_ts_i64
        integer(int64), allocatable :: p64(:)
        type(parquet_timestamp), allocatable :: tmp(:)
        integer(int64) :: k, n
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        n = size(values, kind=int64)
        ! The LENGTH is checked even under assume_valid=.true.: the gather below
        ! indexes values(p64(k)) for k = 1..size(values), so a short perm would read
        ! past its end. Only the O(n) contents walk is what the caller may skip.
        call check_permutation(p64, n, "pf_permute", scan=.not. skip)
        allocate(tmp(n))
        do k = 1_int64, n
            tmp(k) = values(p64(k))
        end do
        do k = 1_int64, n
            values(k) = tmp(k)
        end do
    end procedure permute_ts_i64
    !
    module procedure permute_strcol_i32
        integer(int64), allocatable :: p64(:)
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        ! `assume_valid` means the same thing here as for the nine array types:
        ! %reindex_trusted skips the O(n) contents walk and keeps the O(1) length
        ! check. Both column types validate unconditionally without it.
        if (skip) then
            call values%reindex_trusted(p64)
        else
            call values%reindex(p64)
        end if
    end procedure permute_strcol_i32
    !
    module procedure permute_strcol_i64
        integer(int64), allocatable :: p64(:)
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        ! `assume_valid` means the same thing here as for the nine array types:
        ! %reindex_trusted skips the O(n) contents walk and keeps the O(1) length
        ! check. Both column types validate unconditionally without it.
        if (skip) then
            call values%reindex_trusted(p64)
        else
            call values%reindex(p64)
        end if
    end procedure permute_strcol_i64
    !
    module procedure permute_col_i32
        integer(int64), allocatable :: p64(:)
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        ! `assume_valid` means the same thing here as for the nine array types:
        ! %reindex_trusted skips the O(n) contents walk and keeps the O(1) length
        ! check. Both column types validate unconditionally without it.
        if (skip) then
            call values%reindex_trusted(p64)
        else
            call values%reindex(p64)
        end if
    end procedure permute_col_i32
    !
    module procedure permute_col_i64
        integer(int64), allocatable :: p64(:)
        logical :: skip
        !
        skip = .false.
        if (present(assume_valid)) skip = assume_valid
        allocate(p64(size(perm, kind=int64)))
        p64 = int(perm, int64)
        ! `assume_valid` means the same thing here as for the nine array types:
        ! %reindex_trusted skips the O(n) contents walk and keeps the O(1) length
        ! check. Both column types validate unconditionally without it.
        if (skip) then
            call values%reindex_trusted(p64)
        else
            call values%reindex(p64)
        end if
    end procedure permute_col_i64
    !
    module procedure is_sorted_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i32(values, buf, desc, nlo, "pf_is_sorted", is_valid=is_valid)
        call engine_is_sorted(buf, size(values, kind=int64), "pf_is_sorted", answer)
    end procedure is_sorted_i32
    !
    module procedure is_sorted_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i64(values, buf, desc, nlo, "pf_is_sorted", is_valid=is_valid)
        call engine_is_sorted(buf, size(values, kind=int64), "pf_is_sorted", answer)
    end procedure is_sorted_i64
    !
    module procedure is_sorted_f32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f32(values, buf, desc, nlo, "pf_is_sorted", is_valid=is_valid)
        call engine_is_sorted(buf, size(values, kind=int64), "pf_is_sorted", answer)
    end procedure is_sorted_f32
    !
    module procedure is_sorted_f64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f64(values, buf, desc, nlo, "pf_is_sorted", is_valid=is_valid)
        call engine_is_sorted(buf, size(values, kind=int64), "pf_is_sorted", answer)
    end procedure is_sorted_f64
    !
    module procedure is_sorted_bool
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_bool(values, buf, desc, nlo, "pf_is_sorted", is_valid=is_valid)
        call engine_is_sorted(buf, size(values, kind=int64), "pf_is_sorted", answer)
    end procedure is_sorted_bool
    !
    module procedure is_sorted_chr
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_chr(values, buf, desc, nlo, "pf_is_sorted", is_valid=is_valid)
        call engine_is_sorted(buf, size(values, kind=int64), "pf_is_sorted", answer)
    end procedure is_sorted_chr
    !
    module procedure is_sorted_date
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_date(values, buf, desc, nlo, "pf_is_sorted")
        call engine_is_sorted(buf, size(values, kind=int64), "pf_is_sorted", answer)
    end procedure is_sorted_date
    !
    module procedure is_sorted_time
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_time(values, buf, desc, nlo, "pf_is_sorted")
        call engine_is_sorted(buf, size(values, kind=int64), "pf_is_sorted", answer)
    end procedure is_sorted_time
    !
    module procedure is_sorted_ts
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_ts(values, buf, desc, nlo, "pf_is_sorted")
        call engine_is_sorted(buf, size(values, kind=int64), "pf_is_sorted", answer)
    end procedure is_sorted_ts
    !
    module procedure is_sorted_strcol
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_strcol(values, buf, desc, nlo, "pf_is_sorted")
        call engine_is_sorted(buf, values%size(), "pf_is_sorted", answer)
    end procedure is_sorted_strcol
    !
    module procedure is_sorted_col
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_col(values, buf, desc, nlo, "pf_is_sorted")
        call engine_is_sorted(buf, values%length(), "pf_is_sorted", answer)
    end procedure is_sorted_col
    !
end submodule parquet_sorting_permute ! GCOVR_EXCL_LINE
