!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> `pf_match`, `pf_match_all` and `pf_in` -- which elements of one array occur in another.
!!
!! **One engine call, over the two arrays CONCATENATED.** Every specific below extracts both
!! sides into one sort key, appends the right one onto the left one, and hands the `nl+nr` rows
!! to `engine_build_runs`, which sorts and reports where the runs of EQUAL rows are in the same
!! pass. A run holding members from both halves is a match: a member at position `p <= nl` is
!! left element `p`, one at `p > nl` is right element `p - nl`.
!!
!! That is astropy's join algorithm, and it is why this family needed no new sorting code. The
!! comparator, the null tier, the collapsing of every NaN onto one value and of `-0.0` onto
!! `+0.0`, and the threading are all the engine's -- so "equal" here means exactly what it means
!! everywhere else in this module, which is the property a second engine would put at risk.
!!
!! **The two walks are written ONCE, not per type.** Once `match_keys_*` has run, the answer is
!! arithmetic on `perm`, `tie` and `isnull` and does not know the element type at all, so
!! `match_walk_first` and `match_walk_csr` are ordinary contained procedures shared by all
!! eleven types and only the key assembly is generated per type.
!!
!! **Correctness does not rest on the sort being stable.** Each run is split into its left and
!! right members by a linear scan against `nl`, never by binary-searching for a split point that
!! only a stable sort guarantees exists, and `pf_match` takes the minimum right index rather than
!! the first one listed. Stability decides only the ORDER within one element's `pf_match_all`
!! range, which is a documented output property with its own test.
!!
!! **The obvious alternative -- sort the right side, then binary-search each left element -- is
!! quadratic**, because every `search_impl_*` extracts the whole array on entry. See
!! `doc/pages/utilities/sorting.md`.
submodule (parquet_sorting) parquet_sorting_match
    implicit none
    !
contains
    !
    module procedure match_i32_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_i32(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_i32_i32
    !
    module procedure match_i32_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_i32(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_i32_i64
    !
    module procedure match_i64_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_i64(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_i64_i32
    !
    module procedure match_i64_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_i64(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_i64_i64
    !
    module procedure match_f32_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_f32(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_f32_i32
    !
    module procedure match_f32_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_f32(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_f32_i64
    !
    module procedure match_f64_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_f64(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_f64_i32
    !
    module procedure match_f64_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_f64(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_f64_i64
    !
    module procedure match_bool_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_bool(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_bool_i32
    !
    module procedure match_bool_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_bool(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_bool_i64
    !
    module procedure match_chr_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_chr(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_chr_i32
    !
    module procedure match_chr_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_chr(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_chr_i64
    !
    module procedure match_date_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_date(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_date_i32
    !
    module procedure match_date_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_date(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_date_i64
    !
    module procedure match_time_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_time(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_time_i32
    !
    module procedure match_time_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_time(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_time_i64
    !
    module procedure match_ts_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_ts(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_ts_i32
    !
    module procedure match_ts_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_ts(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_ts_i64
    !
    module procedure match_strcol_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_strcol(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_strcol_i32
    !
    module procedure match_strcol_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_strcol(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_strcol_i64
    !
    module procedure match_col_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_col(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call narrow_i64_array(first, "pf_match", "match index", match)
    end procedure match_col_i32
    !
    module procedure match_col_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_col(left, right, "pf_match", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        if (present(n_matched)) n_matched = count(first /= 0_int64, kind=int64)
        call move_alloc(first, match)
    end procedure match_col_i64
    !
    module procedure match_all_i32_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_i32(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_i32_i32
    !
    module procedure match_all_i32_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_i32(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_i32_i64
    !
    module procedure match_all_i64_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_i64(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_i64_i32
    !
    module procedure match_all_i64_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_i64(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_i64_i64
    !
    module procedure match_all_f32_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_f32(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_f32_i32
    !
    module procedure match_all_f32_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_f32(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_f32_i64
    !
    module procedure match_all_f64_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_f64(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_f64_i32
    !
    module procedure match_all_f64_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_f64(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_f64_i64
    !
    module procedure match_all_bool_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_bool(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_bool_i32
    !
    module procedure match_all_bool_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_bool(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_bool_i64
    !
    module procedure match_all_chr_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_chr(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_chr_i32
    !
    module procedure match_all_chr_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_chr(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid_left, is_valid_right=is_valid_right, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_chr_i64
    !
    module procedure match_all_date_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_date(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_date_i32
    !
    module procedure match_all_date_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_date(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_date_i64
    !
    module procedure match_all_time_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_time(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_time_i32
    !
    module procedure match_all_time_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_time(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_time_i64
    !
    module procedure match_all_ts_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_ts(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_ts_i32
    !
    module procedure match_all_ts_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_ts(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_ts_i64
    !
    module procedure match_all_strcol_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_strcol(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_strcol_i32
    !
    module procedure match_all_strcol_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_strcol(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_strcol_i64
    !
    module procedure match_all_col_i32
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        integer(int64), allocatable :: o64(:), m64(:)
        !
        call match_keys_col(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, o64, m64)
        call narrow_i64_array(o64, "pf_match_all", "CSR offset", offsets)
        call narrow_i64_array(m64, "pf_match_all", "match index", matches)
    end procedure match_all_col_i32
    !
    module procedure match_all_col_i64
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_col(left, right, "pf_match_all", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
    end procedure match_all_col_i64
    !
    module procedure isin_i32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_i32(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, is_valid_right=is_valid_set, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_i32
    !
    module procedure isin_i64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_i64(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, is_valid_right=is_valid_set, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_i64
    !
    module procedure isin_f32
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_f32(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, is_valid_right=is_valid_set, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_f32
    !
    module procedure isin_f64
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_f64(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, is_valid_right=is_valid_set, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_f64
    !
    module procedure isin_bool
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_bool(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, is_valid_right=is_valid_set, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_bool
    !
    module procedure isin_chr
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_chr(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, is_valid_right=is_valid_set, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_chr
    !
    module procedure isin_date
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_date(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_date
    !
    module procedure isin_time
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_time(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_time
    !
    module procedure isin_ts
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_ts(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_ts
    !
    module procedure isin_strcol
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_strcol(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_strcol
    !
    module procedure isin_col
        integer(int64), allocatable :: perm(:), first(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr
        !
        call match_keys_col(values, set, "pf_in", nl, nr, perm, tie, isnull, &
            threads=threads)
        call match_walk_first(perm, tie, isnull, nl, nr, first)
        allocate(mask(nl))
        if (nl > 0_int64) mask = first /= 0_int64
    end procedure isin_col
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over 32-bit integer
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_i32(left, right, proc, nl, nr, perm, tie, isnull, &
            is_valid_left, is_valid_right, threads)
        integer(int32), intent(in) :: left(:) !! the left array.
        integer(int32), intent(in) :: right(:) !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        logical, intent(in), optional :: is_valid_left(:) !! `left`'s validity; absent means none.
        logical, intent(in), optional :: is_valid_right(:) !! `right`'s validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        !
        nl = size(left, kind=int64)
        nr = size(right, kind=int64)
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_i32(left, bufl, .false., .false., proc, is_valid=is_valid_left, threads=threads)
        call extract_i32(right, bufr, .false., .false., proc, is_valid=is_valid_right, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_i32
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over 64-bit integer
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_i64(left, right, proc, nl, nr, perm, tie, isnull, &
            is_valid_left, is_valid_right, threads)
        integer(int64), intent(in) :: left(:) !! the left array.
        integer(int64), intent(in) :: right(:) !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        logical, intent(in), optional :: is_valid_left(:) !! `left`'s validity; absent means none.
        logical, intent(in), optional :: is_valid_right(:) !! `right`'s validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        !
        nl = size(left, kind=int64)
        nr = size(right, kind=int64)
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_i64(left, bufl, .false., .false., proc, is_valid=is_valid_left, threads=threads)
        call extract_i64(right, bufr, .false., .false., proc, is_valid=is_valid_right, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_i64
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over 32-bit real
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_f32(left, right, proc, nl, nr, perm, tie, isnull, &
            is_valid_left, is_valid_right, threads)
        real(real32), intent(in) :: left(:) !! the left array.
        real(real32), intent(in) :: right(:) !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        logical, intent(in), optional :: is_valid_left(:) !! `left`'s validity; absent means none.
        logical, intent(in), optional :: is_valid_right(:) !! `right`'s validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        !
        nl = size(left, kind=int64)
        nr = size(right, kind=int64)
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_f32(left, bufl, .false., .false., proc, is_valid=is_valid_left, threads=threads)
        call extract_f32(right, bufr, .false., .false., proc, is_valid=is_valid_right, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_f32
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over 64-bit real
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_f64(left, right, proc, nl, nr, perm, tie, isnull, &
            is_valid_left, is_valid_right, threads)
        real(real64), intent(in) :: left(:) !! the left array.
        real(real64), intent(in) :: right(:) !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        logical, intent(in), optional :: is_valid_left(:) !! `left`'s validity; absent means none.
        logical, intent(in), optional :: is_valid_right(:) !! `right`'s validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        !
        nl = size(left, kind=int64)
        nr = size(right, kind=int64)
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_f64(left, bufl, .false., .false., proc, is_valid=is_valid_left, threads=threads)
        call extract_f64(right, bufr, .false., .false., proc, is_valid=is_valid_right, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_f64
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over logical
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_bool(left, right, proc, nl, nr, perm, tie, isnull, &
            is_valid_left, is_valid_right, threads)
        logical, intent(in) :: left(:) !! the left array.
        logical, intent(in) :: right(:) !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        logical, intent(in), optional :: is_valid_left(:) !! `left`'s validity; absent means none.
        logical, intent(in), optional :: is_valid_right(:) !! `right`'s validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        !
        nl = size(left, kind=int64)
        nr = size(right, kind=int64)
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_bool(left, bufl, .false., .false., proc, is_valid=is_valid_left, threads=threads)
        call extract_bool(right, bufr, .false., .false., proc, is_valid=is_valid_right, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_bool
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over string
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_chr(left, right, proc, nl, nr, perm, tie, isnull, &
            is_valid_left, is_valid_right, threads)
        character(len=*), intent(in) :: left(:) !! the left array.
        character(len=*), intent(in) :: right(:) !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        logical, intent(in), optional :: is_valid_left(:) !! `left`'s validity; absent means none.
        logical, intent(in), optional :: is_valid_right(:) !! `right`'s validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        character(len=max(len(left), len(right))), allocatable :: pl(:), pr(:)
        integer(int64) :: k
        !
        nl = size(left, kind=int64)
        nr = size(right, kind=int64)
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        ! Both halves are widened to one common element length before extraction, or the
        ! packed keys would compare strings of two different widths. Element by element,
        ! because a whole-array assignment into an allocatable character array is the
        ! reallocation hazard CLAUDE.md documents.
        allocate(pl(nl), pr(nr))
        do k = 1_int64, nl
            pl(k) = left(k)
        end do
        do k = 1_int64, nr
            pr(k) = right(k)
        end do
        call extract_chr(pl, bufl, .false., .false., proc, is_valid=is_valid_left, threads=threads)
        call extract_chr(pr, bufr, .false., .false., proc, is_valid=is_valid_right, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_chr
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over date
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_date(left, right, proc, nl, nr, perm, tie, isnull, threads)
        type(parquet_date), intent(in) :: left(:) !! the left array.
        type(parquet_date), intent(in) :: right(:) !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        !
        nl = size(left, kind=int64)
        nr = size(right, kind=int64)
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_date(left, bufl, .false., .false., proc, threads=threads)
        call extract_date(right, bufr, .false., .false., proc, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_date
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over time
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_time(left, right, proc, nl, nr, perm, tie, isnull, threads)
        type(parquet_time), intent(in) :: left(:) !! the left array.
        type(parquet_time), intent(in) :: right(:) !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        !
        nl = size(left, kind=int64)
        nr = size(right, kind=int64)
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_time(left, bufl, .false., .false., proc, threads=threads)
        call extract_time(right, bufr, .false., .false., proc, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_time
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over timestamp
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_ts(left, right, proc, nl, nr, perm, tie, isnull, threads)
        type(parquet_timestamp), intent(in) :: left(:) !! the left array.
        type(parquet_timestamp), intent(in) :: right(:) !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        !
        nl = size(left, kind=int64)
        nr = size(right, kind=int64)
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_ts(left, bufl, .false., .false., proc, threads=threads)
        call extract_ts(right, bufr, .false., .false., proc, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_ts
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over packed string column
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_strcol(left, right, proc, nl, nr, perm, tie, isnull, threads)
        type(parquet_string_column), intent(in) :: left !! the left array.
        type(parquet_string_column), intent(in) :: right !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        !
        nl = left%size()
        nr = right%size()
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_strcol(left, bufl, .false., .false., proc, threads=threads)
        call extract_strcol(right, bufr, .false., .false., proc, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_strcol
    !
    !> Shared front half of every pf_match/pf_match_all/pf_in specific over type-erased column
    !! arrays: extracts both sides into one key, appends the right onto the left, and
    !! sorts the concatenation with its run boundaries reported. Everything after this
    !! point is index arithmetic, which is why the two walks are type-independent.
    subroutine match_keys_col(left, right, proc, nl, nr, perm, tie, isnull, threads)
        type(parquet_column), intent(in) :: left !! the left array.
        type(parquet_column), intent(in) :: right !! the right array.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: nl !! elements in `left`.
        integer(int64), intent(out) :: nr !! elements in `right`.
        integer(int64), allocatable, intent(out) :: perm(:) !! the concatenation's permutation.
        integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
        logical, allocatable, intent(out) :: isnull(:) !! .true. where a concatenated row is null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: bufl(:), bufr(:)
        integer(int64) :: n
        character(len=:), allocatable :: kl, kr
        !
        nl = left%length()
        nr = right%length()
        ! Checked BEFORE the extraction, and before the empty-input return, so the
        ! message names the two kinds rather than buf_append's internal-error one --
        ! and so that an int32 column never silently matches an int64 one on the raw
        ! value. A 64-bit catalogue identifier above 2**53 is what that would lose.
        if (left%kindof() /= right%kindof()) then
            call parquet_kind_name(left%kindof(), kl)
            call parquet_kind_name(right%kindof(), kr)
            error stop EP // proc // ": the two columns hold different kinds (" // kl // &
                " and " // kr // "); matching compares like with like, so cast one of " // &
                "them to the other's kind first"
        end if
        n = nl + nr
        if (n < 1_int64) then
            ! Both sides empty: there is no key to extract and the engine is never
            ! entered. Every caller's answer is an empty array, built from nl and nr.
            allocate(perm(0), tie(0), isnull(0))
            return
        end if
        ! `descending` cannot affect which elements are equal, and nulls_first=.false.
        ! keeps the nulls contiguous at the end -- where the walks skip them per element
        ! rather than relying on that placement.
        call extract_col(left, bufl, .false., .false., proc, threads=threads)
        call extract_col(right, bufr, .false., .false., proc, threads=threads)
        call buf_append(bufl, nl, bufr, nr, proc)
        call engine_build_runs(bufl, n, proc, perm, tie, threads=threads)
        call key_null_mask(bufl, n, isnull)
    end subroutine match_keys_col
    !
    !> One left element's FIRST match -- the walk behind `pf_match` and `pf_in`.
    !!
    !! `first(i)` is the SMALLEST index in `right` whose element equals `left(i)`, or 0 when
    !! there is none. Taken as a minimum over the run's right members rather than as "whichever
    !! the permutation lists first", so this answer does not depend on the sort being stable.
    subroutine match_walk_first(perm, tie, isnull, nl, nr, first)
        integer(int64), intent(in) :: perm(:)   !! the concatenation's permutation.
        integer(c_int8_t), intent(in) :: tie(:) !! 1 where a row ties the one before it.
        logical, intent(in) :: isnull(:)        !! .true. where a concatenated row is null.
        integer(int64), intent(in) :: nl        !! elements in the left half.
        integer(int64), intent(in) :: nr        !! elements in the right half.
        integer(int64), allocatable, intent(out) :: first(:) !! per left element: 0, or a right index.
        integer(int64) :: n, s, e, k, p, best
        !
        allocate(first(max(nl, 0_int64)))
        if (nl < 1_int64) return
        first = 0_int64
        n = nl + nr
        s = 1_int64
        do while (s <= n)
            ! The run is [s, e]. The bound test and the tie test are NESTED rather than `.and.`ed:
            ! Fortran does not short-circuit, and `tie(n+1)` would be out of bounds.
            e = s
            do while (e < n)
                if (tie(e + 1_int64) == 0_c_int8_t) exit
                e = e + 1_int64
            end do
            best = 0_int64
            do k = s, e
                p = perm(k)
                if (p > nl) then
                    ! THIS TEST AND THE ONE IN THE LOOP BELOW ARE INDIVIDUALLY REDUNDANT AND
                    ! JOINTLY LOAD-BEARING -- do not delete either on the strength of a coverage
                    ! report. Nulls are their own tier in the comparator, so a run is either all
                    ! null or all value and skipping at either end suffices; removing BOTH makes
                    ! nulls match each other, which is the one thing the contract forbids.
                    ! Confirmed by mutation: each alone survives the suite, the pair does not.
                    if (.not. isnull(p)) then
                        if (best == 0_int64 .or. p - nl < best) best = p - nl
                    end if
                end if
            end do
            if (best > 0_int64) then
                do k = s, e
                    p = perm(k)
                    if (p <= nl) then
                        if (.not. isnull(p)) first(p) = best
                    end if
                end do
            end if
            s = e + 1_int64
        end do
    end subroutine match_walk_first
    !
    !> EVERY match, as the CSR pair behind `pf_match_all`.
    !!
    !! Two passes over the runs: the first counts, so `matches` is allocated exactly once at its
    !! final size, and the second fills. Counting first is not an optimisation -- the pair count
    !! is a PRODUCT and can be far larger than either input, so growing the array would copy it
    !! repeatedly at exactly the sizes where that hurts most.
    subroutine match_walk_csr(perm, tie, isnull, nl, nr, offsets, matches)
        integer(int64), intent(in) :: perm(:)   !! the concatenation's permutation.
        integer(c_int8_t), intent(in) :: tie(:) !! 1 where a row ties the one before it.
        logical, intent(in) :: isnull(:)        !! .true. where a concatenated row is null.
        integer(int64), intent(in) :: nl        !! elements in the left half.
        integer(int64), intent(in) :: nr        !! elements in the right half.
        integer(int64), allocatable, intent(out) :: offsets(:) !! length nl+1, starting at 1.
        integer(int64), allocatable, intent(out) :: matches(:) !! the right indices, grouped by left.
        integer(int64), allocatable :: counts(:), rbuf(:)
        integer(int64) :: n, s, e, k, j, p, cr, o, total
        !
        allocate(offsets(nl + 1_int64))
        offsets = 1_int64
        if (nl < 1_int64) then
            allocate(matches(0))
            return
        end if
        allocate(counts(nl))
        counts = 0_int64
        n = nl + nr
        s = 1_int64
        do while (s <= n)
            e = s
            do while (e < n)
                if (tie(e + 1_int64) == 0_c_int8_t) exit
                e = e + 1_int64
            end do
            cr = 0_int64
            do k = s, e
                p = perm(k)
                if (p > nl) then
                    ! Redundant with the test three lines below, and load-bearing together with
                    ! it -- see match_walk_first, which carries the reasoning and the mutation
                    ! result. The same pairing appears in the fill pass.
                    if (.not. isnull(p)) cr = cr + 1_int64
                end if
            end do
            if (cr > 0_int64) then
                do k = s, e
                    p = perm(k)
                    if (p <= nl) then
                        if (.not. isnull(p)) counts(p) = cr
                    end if
                end do
            end if
            s = e + 1_int64
        end do
        do k = 1_int64, nl
            offsets(k + 1_int64) = offsets(k) + counts(k)
        end do
        total = offsets(nl + 1_int64) - 1_int64
        allocate(matches(max(total, 0_int64)))
        if (total < 1_int64) return
        allocate(rbuf(max(nr, 1_int64)))
        ! Each left element belongs to exactly one run, so `offsets(p)` is written through once
        ! and no cursor is needed. `rbuf` takes the run's right members in permutation order,
        ! which is ascending right index because the engine's sort is stable -- the one place in
        ! this file that relies on that, and the reason it is a tested output property.
        s = 1_int64
        do while (s <= n)
            e = s
            do while (e < n)
                if (tie(e + 1_int64) == 0_c_int8_t) exit
                e = e + 1_int64
            end do
            cr = 0_int64
            do k = s, e
                p = perm(k)
                if (p > nl) then
                    if (.not. isnull(p)) then
                        cr = cr + 1_int64
                        rbuf(cr) = p - nl
                    end if
                end if
            end do
            if (cr > 0_int64) then
                do k = s, e
                    p = perm(k)
                    if (p <= nl) then
                        if (.not. isnull(p)) then
                            o = offsets(p)
                            do j = 1_int64, cr
                                matches(o + j - 1_int64) = rbuf(j)
                            end do
                        end if
                    end if
                end do
            end if
            s = e + 1_int64
        end do
    end subroutine match_walk_csr
    !
end submodule parquet_sorting_match ! GCOVR_EXCL_LINE
