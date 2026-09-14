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
    module procedure remap_i32_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_i32_i32
    !
    module procedure remap_i32_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_i32_i64
    !
    module procedure remap_i32_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_i32_f32
    !
    module procedure remap_i32_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_i32_f64
    !
    module procedure remap_i32_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_i32_bool
    !
    module procedure remap_i32_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_i32_chr
    !
    module procedure remap_i64_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_i64_i32
    !
    module procedure remap_i64_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_i64_i64
    !
    module procedure remap_i64_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_i64_f32
    !
    module procedure remap_i64_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_i64_f64
    !
    module procedure remap_i64_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_i64_bool
    !
    module procedure remap_i64_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_i64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_i64_chr
    !
    module procedure remap_f32_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_f32_i32
    !
    module procedure remap_f32_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_f32_i64
    !
    module procedure remap_f32_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_f32_f32
    !
    module procedure remap_f32_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_f32_f64
    !
    module procedure remap_f32_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_f32_bool
    !
    module procedure remap_f32_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f32(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_f32_chr
    !
    module procedure remap_f64_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_f64_i32
    !
    module procedure remap_f64_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_f64_i64
    !
    module procedure remap_f64_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_f64_f32
    !
    module procedure remap_f64_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_f64_f64
    !
    module procedure remap_f64_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_f64_bool
    !
    module procedure remap_f64_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_f64(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_f64_chr
    !
    module procedure remap_bool_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_bool(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_bool_i32
    !
    module procedure remap_bool_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_bool(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_bool_i64
    !
    module procedure remap_bool_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_bool(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_bool_f32
    !
    module procedure remap_bool_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_bool(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_bool_f64
    !
    module procedure remap_bool_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_bool(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_bool_bool
    !
    module procedure remap_bool_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_bool(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_bool_chr
    !
    module procedure remap_chr_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_chr(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_chr_i32
    !
    module procedure remap_chr_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_chr(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_chr_i64
    !
    module procedure remap_chr_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_chr(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_chr_f32
    !
    module procedure remap_chr_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_chr(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_chr_f64
    !
    module procedure remap_chr_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_chr(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_chr_bool
    !
    module procedure remap_chr_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_chr(values, from_keys, size(to_values, kind=int64), &
            first, is_valid=is_valid, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_chr_chr
    !
    module procedure remap_date_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_date(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_date_i32
    !
    module procedure remap_date_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_date(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_date_i64
    !
    module procedure remap_date_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_date(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_date_f32
    !
    module procedure remap_date_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_date(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_date_f64
    !
    module procedure remap_date_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_date(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_date_bool
    !
    module procedure remap_date_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_date(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_date_chr
    !
    module procedure remap_time_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_time(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_time_i32
    !
    module procedure remap_time_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_time(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_time_i64
    !
    module procedure remap_time_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_time(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_time_f32
    !
    module procedure remap_time_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_time(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_time_f64
    !
    module procedure remap_time_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_time(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_time_bool
    !
    module procedure remap_time_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_time(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_time_chr
    !
    module procedure remap_ts_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_ts(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_ts_i32
    !
    module procedure remap_ts_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_ts(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_ts_i64
    !
    module procedure remap_ts_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_ts(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_ts_f32
    !
    module procedure remap_ts_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_ts(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_ts_f64
    !
    module procedure remap_ts_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_ts(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_ts_bool
    !
    module procedure remap_ts_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_ts(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_ts_chr
    !
    module procedure remap_strcol_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_strcol(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_strcol_i32
    !
    module procedure remap_strcol_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_strcol(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_strcol_i64
    !
    module procedure remap_strcol_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_strcol(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_strcol_f32
    !
    module procedure remap_strcol_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_strcol(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_strcol_f64
    !
    module procedure remap_strcol_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_strcol(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_strcol_bool
    !
    module procedure remap_strcol_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_strcol(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_strcol_chr
    !
    module procedure remap_col_i32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_col(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i32(first, to_values, out, default, found)
    end procedure remap_col_i32
    !
    module procedure remap_col_i64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_col(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_i64(first, to_values, out, default, found)
    end procedure remap_col_i64
    !
    module procedure remap_col_f32
        integer(int64), allocatable :: first(:)
        !
        call remap_match_col(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f32(first, to_values, out, default, found)
    end procedure remap_col_f32
    !
    module procedure remap_col_f64
        integer(int64), allocatable :: first(:)
        !
        call remap_match_col(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_f64(first, to_values, out, default, found)
    end procedure remap_col_f64
    !
    module procedure remap_col_bool
        integer(int64), allocatable :: first(:)
        !
        call remap_match_col(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_bool(first, to_values, out, default, found)
    end procedure remap_col_bool
    !
    module procedure remap_col_chr
        integer(int64), allocatable :: first(:)
        !
        call remap_match_col(values, from_keys, size(to_values, kind=int64), &
            first, threads=threads)
        call remap_fill_chr(first, to_values, out, default, found)
    end procedure remap_col_chr
    !
    !> The KEY half of every pf_remap specific over 32-bit integer keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_i32(values, from_keys, n_to, first, is_valid, threads)
        integer(int32), intent(in) :: values(:) !! the keys to look up.
        integer(int32), intent(in) :: from_keys(:) !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        logical, intent(in), optional :: is_valid(:) !! `values`' validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = size(from_keys, kind=int64)
        if (nr /= n_to) then
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
        end if
        call match_keys_i32(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_i32
    !
    !> The KEY half of every pf_remap specific over 64-bit integer keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_i64(values, from_keys, n_to, first, is_valid, threads)
        integer(int64), intent(in) :: values(:) !! the keys to look up.
        integer(int64), intent(in) :: from_keys(:) !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        logical, intent(in), optional :: is_valid(:) !! `values`' validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = size(from_keys, kind=int64)
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_i64(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_i64
    !
    !> The KEY half of every pf_remap specific over 32-bit real keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_f32(values, from_keys, n_to, first, is_valid, threads)
        real(real32), intent(in) :: values(:) !! the keys to look up.
        real(real32), intent(in) :: from_keys(:) !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        logical, intent(in), optional :: is_valid(:) !! `values`' validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = size(from_keys, kind=int64)
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_f32(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_f32
    !
    !> The KEY half of every pf_remap specific over 64-bit real keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_f64(values, from_keys, n_to, first, is_valid, threads)
        real(real64), intent(in) :: values(:) !! the keys to look up.
        real(real64), intent(in) :: from_keys(:) !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        logical, intent(in), optional :: is_valid(:) !! `values`' validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = size(from_keys, kind=int64)
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_f64(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_f64
    !
    !> The KEY half of every pf_remap specific over logical keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_bool(values, from_keys, n_to, first, is_valid, threads)
        logical, intent(in) :: values(:) !! the keys to look up.
        logical, intent(in) :: from_keys(:) !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        logical, intent(in), optional :: is_valid(:) !! `values`' validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = size(from_keys, kind=int64)
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_bool(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_bool
    !
    !> The KEY half of every pf_remap specific over string keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_chr(values, from_keys, n_to, first, is_valid, threads)
        character(len=*), intent(in) :: values(:) !! the keys to look up.
        character(len=*), intent(in) :: from_keys(:) !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        logical, intent(in), optional :: is_valid(:) !! `values`' validity; absent means none.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = size(from_keys, kind=int64)
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_chr(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            is_valid_left=is_valid, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_chr
    !
    !> The KEY half of every pf_remap specific over date keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_date(values, from_keys, n_to, first, threads)
        type(parquet_date), intent(in) :: values(:) !! the keys to look up.
        type(parquet_date), intent(in) :: from_keys(:) !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = size(from_keys, kind=int64)
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_date(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_date
    !
    !> The KEY half of every pf_remap specific over time keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_time(values, from_keys, n_to, first, threads)
        type(parquet_time), intent(in) :: values(:) !! the keys to look up.
        type(parquet_time), intent(in) :: from_keys(:) !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = size(from_keys, kind=int64)
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_time(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_time
    !
    !> The KEY half of every pf_remap specific over timestamp keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_ts(values, from_keys, n_to, first, threads)
        type(parquet_timestamp), intent(in) :: values(:) !! the keys to look up.
        type(parquet_timestamp), intent(in) :: from_keys(:) !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = size(from_keys, kind=int64)
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_ts(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_ts
    !
    !> The KEY half of every pf_remap specific over packed string column keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_strcol(values, from_keys, n_to, first, threads)
        type(parquet_string_column), intent(in) :: values !! the keys to look up.
        type(parquet_string_column), intent(in) :: from_keys !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = from_keys%size()
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_strcol(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_strcol
    !
    !> The KEY half of every pf_remap specific over type-erased column keys: refuses a lookup table
    !! that is the wrong length or that repeats a key, then matches every value against it.
    !!
    !! Split from the value half so that eleven key types times six value types costs
    !! eleven plus six workers rather than sixty-six -- and so that the two guards below
    !! are written once. Neither depends on what a key maps TO.
    subroutine remap_match_col(values, from_keys, n_to, first, threads)
        type(parquet_column), intent(in) :: values !! the keys to look up.
        type(parquet_column), intent(in) :: from_keys !! the lookup table's keys.
        integer(int64), intent(in) :: n_to !! size(to_values), checked against the key count.
        integer(int64), allocatable, intent(out) :: first(:) !! per value: key index, or 0.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: nl, nr, dup_at, dup_first
        character(len=32) :: a_str, b_str
        !
        ! Checked BEFORE the sort: a caller who paired the wrong two arrays should be told
        ! so at once rather than after O(n log n) of work they cannot use.
        nr = from_keys%length()
        if (nr /= n_to) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") nr
            write (b_str, "(i0)") n_to
            error stop EP // "pf_remap: from_keys has " // trim(a_str) // " keys but " // &
                "to_values has " // trim(b_str) // " values; a lookup table takes one " // &
                "value per key"
            ! GCOVR_EXCL_STOP
        end if
        call match_keys_col(values, from_keys, "pf_remap", nl, nr, perm, tie, isnull, &
            threads=threads)
        call remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        if (dup_at > 0_int64) then
            ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
            write (a_str, "(i0)") dup_first
            write (b_str, "(i0)") dup_at
            error stop EP // "pf_remap: from_keys repeats a key at positions " // &
                trim(a_str) // " and " // trim(b_str) // "; a lookup table must be " // &
                "distinct, since a repeated key has no defined value"
            ! GCOVR_EXCL_STOP
        end if
        call match_walk_first(perm, tie, isnull, nl, nr, first)
    end subroutine remap_match_col
    !
    !> The VALUE half of every pf_remap specific with 32-bit integer values: the gather, and the
    !! policy for an element that matched no key.
    subroutine remap_fill_i32(first, to_values, out, default, found)
        integer(int64), intent(in) :: first(:) !! per value: key index, or 0 when unmapped.
        integer(int32), intent(in) :: to_values(:) !! the value each key maps to.
        integer(int32), allocatable, intent(out) :: out(:) !! the mapped values.
        integer(int32), intent(in), optional :: default !! what an unmapped element becomes.
        logical, allocatable, intent(out), optional :: found(:) !! .true. where a key matched.
        integer(int64) :: i, n
        character(len=32) :: a_str
        !
        n = size(first, kind=int64)
        ! `found` means "a key matched", also when `default` is given: it comes from the match
        ! indices, before any substitution, and the fill loop below never touches it
        ! (`test_remap_unmapped_policies`, the both-arguments assertion).
        if (present(found)) then
            allocate(found(n))
            if (n > 0_int64) found = first /= 0_int64
        end if
        ! The abort is the DEFAULT, and it happens before `out` is allocated: with neither
        ! `default` nor `found` the caller has no way to learn an element was unmapped, so
        ! handing them one silently is the one behaviour this family refuses.
        if (.not. present(default) .and. .not. present(found)) then
            do i = 1_int64, n
                if (first(i) == 0_int64) then
                    write (a_str, "(i0)") i
                    error stop EP // "pf_remap: the value at position " // trim(a_str) // &
                        " matches no key in from_keys; pass default= for a fallback " // &
                        "value, or found= to be told which elements were unmapped"
                end if
            end do
        end if
        allocate(out(n))
        do i = 1_int64, n
            if (first(i) /= 0_int64) then
                out(i) = to_values(first(i))
            else if (present(default)) then
                out(i) = default
            else
                out(i) = 0_int32
            end if
        end do
    end subroutine remap_fill_i32
    !
    !> The VALUE half of every pf_remap specific with 64-bit integer values: the gather, and the
    !! policy for an element that matched no key.
    subroutine remap_fill_i64(first, to_values, out, default, found)
        integer(int64), intent(in) :: first(:) !! per value: key index, or 0 when unmapped.
        integer(int64), intent(in) :: to_values(:) !! the value each key maps to.
        integer(int64), allocatable, intent(out) :: out(:) !! the mapped values.
        integer(int64), intent(in), optional :: default !! what an unmapped element becomes.
        logical, allocatable, intent(out), optional :: found(:) !! .true. where a key matched.
        integer(int64) :: i, n
        character(len=32) :: a_str
        !
        n = size(first, kind=int64)
        ! `found` means "a key matched", also when `default` is given: it comes from the match
        ! indices, before any substitution, and the fill loop below never touches it
        ! (`test_remap_unmapped_policies`, the both-arguments assertion).
        if (present(found)) then
            allocate(found(n))
            if (n > 0_int64) found = first /= 0_int64
        end if
        ! The abort is the DEFAULT, and it happens before `out` is allocated: with neither
        ! `default` nor `found` the caller has no way to learn an element was unmapped, so
        ! handing them one silently is the one behaviour this family refuses.
        if (.not. present(default) .and. .not. present(found)) then
            do i = 1_int64, n
                if (first(i) == 0_int64) then
                    ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
                    write (a_str, "(i0)") i
                    error stop EP // "pf_remap: the value at position " // trim(a_str) // &
                        " matches no key in from_keys; pass default= for a fallback " // &
                        "value, or found= to be told which elements were unmapped"
                    ! GCOVR_EXCL_STOP
                end if
            end do
        end if
        allocate(out(n))
        do i = 1_int64, n
            if (first(i) /= 0_int64) then
                out(i) = to_values(first(i))
            else if (present(default)) then
                out(i) = default
            else
                out(i) = 0_int64
            end if
        end do
    end subroutine remap_fill_i64
    !
    !> The VALUE half of every pf_remap specific with 32-bit real values: the gather, and the
    !! policy for an element that matched no key.
    subroutine remap_fill_f32(first, to_values, out, default, found)
        integer(int64), intent(in) :: first(:) !! per value: key index, or 0 when unmapped.
        real(real32), intent(in) :: to_values(:) !! the value each key maps to.
        real(real32), allocatable, intent(out) :: out(:) !! the mapped values.
        real(real32), intent(in), optional :: default !! what an unmapped element becomes.
        logical, allocatable, intent(out), optional :: found(:) !! .true. where a key matched.
        integer(int64) :: i, n
        character(len=32) :: a_str
        !
        n = size(first, kind=int64)
        ! `found` means "a key matched", also when `default` is given: it comes from the match
        ! indices, before any substitution, and the fill loop below never touches it
        ! (`test_remap_unmapped_policies`, the both-arguments assertion).
        if (present(found)) then
            allocate(found(n))
            if (n > 0_int64) found = first /= 0_int64
        end if
        ! The abort is the DEFAULT, and it happens before `out` is allocated: with neither
        ! `default` nor `found` the caller has no way to learn an element was unmapped, so
        ! handing them one silently is the one behaviour this family refuses.
        if (.not. present(default) .and. .not. present(found)) then
            do i = 1_int64, n
                if (first(i) == 0_int64) then
                    ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
                    write (a_str, "(i0)") i
                    error stop EP // "pf_remap: the value at position " // trim(a_str) // &
                        " matches no key in from_keys; pass default= for a fallback " // &
                        "value, or found= to be told which elements were unmapped"
                    ! GCOVR_EXCL_STOP
                end if
            end do
        end if
        allocate(out(n))
        do i = 1_int64, n
            if (first(i) /= 0_int64) then
                out(i) = to_values(first(i))
            else if (present(default)) then
                out(i) = default
            else
                out(i) = 0.0_real32
            end if
        end do
    end subroutine remap_fill_f32
    !
    !> The VALUE half of every pf_remap specific with 64-bit real values: the gather, and the
    !! policy for an element that matched no key.
    subroutine remap_fill_f64(first, to_values, out, default, found)
        integer(int64), intent(in) :: first(:) !! per value: key index, or 0 when unmapped.
        real(real64), intent(in) :: to_values(:) !! the value each key maps to.
        real(real64), allocatable, intent(out) :: out(:) !! the mapped values.
        real(real64), intent(in), optional :: default !! what an unmapped element becomes.
        logical, allocatable, intent(out), optional :: found(:) !! .true. where a key matched.
        integer(int64) :: i, n
        character(len=32) :: a_str
        !
        n = size(first, kind=int64)
        ! `found` means "a key matched", also when `default` is given: it comes from the match
        ! indices, before any substitution, and the fill loop below never touches it
        ! (`test_remap_unmapped_policies`, the both-arguments assertion).
        if (present(found)) then
            allocate(found(n))
            if (n > 0_int64) found = first /= 0_int64
        end if
        ! The abort is the DEFAULT, and it happens before `out` is allocated: with neither
        ! `default` nor `found` the caller has no way to learn an element was unmapped, so
        ! handing them one silently is the one behaviour this family refuses.
        if (.not. present(default) .and. .not. present(found)) then
            do i = 1_int64, n
                if (first(i) == 0_int64) then
                    ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
                    write (a_str, "(i0)") i
                    error stop EP // "pf_remap: the value at position " // trim(a_str) // &
                        " matches no key in from_keys; pass default= for a fallback " // &
                        "value, or found= to be told which elements were unmapped"
                    ! GCOVR_EXCL_STOP
                end if
            end do
        end if
        allocate(out(n))
        do i = 1_int64, n
            if (first(i) /= 0_int64) then
                out(i) = to_values(first(i))
            else if (present(default)) then
                out(i) = default
            else
                out(i) = 0.0_real64
            end if
        end do
    end subroutine remap_fill_f64
    !
    !> The VALUE half of every pf_remap specific with logical values: the gather, and the
    !! policy for an element that matched no key.
    subroutine remap_fill_bool(first, to_values, out, default, found)
        integer(int64), intent(in) :: first(:) !! per value: key index, or 0 when unmapped.
        logical, intent(in) :: to_values(:) !! the value each key maps to.
        logical, allocatable, intent(out) :: out(:) !! the mapped values.
        logical, intent(in), optional :: default !! what an unmapped element becomes.
        logical, allocatable, intent(out), optional :: found(:) !! .true. where a key matched.
        integer(int64) :: i, n
        character(len=32) :: a_str
        !
        n = size(first, kind=int64)
        ! `found` means "a key matched", also when `default` is given: it comes from the match
        ! indices, before any substitution, and the fill loop below never touches it
        ! (`test_remap_unmapped_policies`, the both-arguments assertion).
        if (present(found)) then
            allocate(found(n))
            if (n > 0_int64) found = first /= 0_int64
        end if
        ! The abort is the DEFAULT, and it happens before `out` is allocated: with neither
        ! `default` nor `found` the caller has no way to learn an element was unmapped, so
        ! handing them one silently is the one behaviour this family refuses.
        if (.not. present(default) .and. .not. present(found)) then
            do i = 1_int64, n
                if (first(i) == 0_int64) then
                    ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
                    write (a_str, "(i0)") i
                    error stop EP // "pf_remap: the value at position " // trim(a_str) // &
                        " matches no key in from_keys; pass default= for a fallback " // &
                        "value, or found= to be told which elements were unmapped"
                    ! GCOVR_EXCL_STOP
                end if
            end do
        end if
        allocate(out(n))
        do i = 1_int64, n
            if (first(i) /= 0_int64) then
                out(i) = to_values(first(i))
            else if (present(default)) then
                out(i) = default
            else
                out(i) = .false.
            end if
        end do
    end subroutine remap_fill_bool
    !
    !> The VALUE half of every pf_remap specific with string values: the gather, and the
    !! policy for an element that matched no key.
    subroutine remap_fill_chr(first, to_values, out, default, found)
        integer(int64), intent(in) :: first(:) !! per value: key index, or 0 when unmapped.
        character(len=*), intent(in) :: to_values(:) !! the value each key maps to.
        character(len=:), allocatable, intent(out) :: out(:) !! the mapped values.
        character(len=*), intent(in), optional :: default !! what an unmapped element becomes.
        logical, allocatable, intent(out), optional :: found(:) !! .true. where a key matched.
        integer(int64) :: i, n
        integer :: wid
        character(len=32) :: a_str
        !
        n = size(first, kind=int64)
        ! `found` means "a key matched", also when `default` is given: it comes from the match
        ! indices, before any substitution, and the fill loop below never touches it
        ! (`test_remap_unmapped_policies`, the both-arguments assertion).
        if (present(found)) then
            allocate(found(n))
            if (n > 0_int64) found = first /= 0_int64
        end if
        ! The abort is the DEFAULT, and it happens before `out` is allocated: with neither
        ! `default` nor `found` the caller has no way to learn an element was unmapped, so
        ! handing them one silently is the one behaviour this family refuses.
        if (.not. present(default) .and. .not. present(found)) then
            do i = 1_int64, n
                if (first(i) == 0_int64) then
                    ! GCOVR_EXCL_START -- deliberately untested; see REMAP_GUARD_PROVEN_TAG.
                    write (a_str, "(i0)") i
                    error stop EP // "pf_remap: the value at position " // trim(a_str) // &
                        " matches no key in from_keys; pass default= for a fallback " // &
                        "value, or found= to be told which elements were unmapped"
                    ! GCOVR_EXCL_STOP
                end if
            end do
        end if
        ! Deferred-length, and sized from BOTH inputs -- pf_merge's rule, for the same
        ! reason: a `default` longer than the table's values would otherwise be truncated
        ! into the result silently.
        wid = len(to_values)
        if (present(default)) wid = max(wid, len(default))
        allocate(character(len=wid) :: out(n))
        do i = 1_int64, n
            if (first(i) /= 0_int64) then
                out(i) = to_values(first(i))
            else if (present(default)) then
                out(i) = default
            else
                out(i) = ""
            end if
        end do
    end subroutine remap_fill_chr
    !
    !> Whether the RIGHT half of the concatenation -- a `pf_remap` lookup table -- repeats a key.
    !!
    !! Reads the same sorted runs `match_walk_first` does, so the uniqueness guard costs a linear
    !! scan rather than a second sort of `from_keys`. `dup_at` comes back 0 when every non-null key
    !! is distinct; otherwise `dup_first` and `dup_at` are the two LOWEST positions of some group of
    !! equal keys, and naming both is what lets a caller find the pair in their own array.
    !!
    !! **Nulls are skipped, exactly as in `match_walk_first`.** A null key matches nothing, so two
    !! of them are not two entries competing to answer the same lookup -- they are two entries that
    !! answer nothing. Refusing them would make a lookup table with an unused null key unusable for
    !! no gain.
    !!
    !! The two lowest positions are taken as MINIMA over the run rather than as the first two the
    !! permutation lists, so the message does not depend on the sort being stable -- the reason
    !! `match_walk_first` takes a minimum too. Which GROUP is reported does depend on the sort
    !! order, but that order is deterministic, and every group it could name is a real duplicate.
    subroutine remap_check_unique(perm, tie, isnull, nl, nr, dup_at, dup_first)
        integer(int64), intent(in) :: perm(:)   !! the concatenation's permutation.
        integer(c_int8_t), intent(in) :: tie(:) !! 1 where a row ties the one before it.
        logical, intent(in) :: isnull(:)        !! .true. where a concatenated row is null.
        integer(int64), intent(in) :: nl        !! elements in the left half (the values).
        integer(int64), intent(in) :: nr        !! elements in the right half (the keys).
        integer(int64), intent(out) :: dup_at    !! the higher of two equal key positions, or 0.
        integer(int64), intent(out) :: dup_first !! the lower of them, or 0.
        integer(int64) :: n, s, e, k, p, q, lo1, lo2
        !
        dup_at = 0_int64
        dup_first = 0_int64
        if (nr < 2_int64) return
        n = nl + nr
        s = 1_int64
        do while (s <= n)
            ! The run is [s, e]. Nested rather than `.and.`ed, since Fortran does not short-circuit
            ! and `tie(n+1)` would be out of bounds -- match_walk_first's own note.
            e = s
            do while (e < n)
                if (tie(e + 1_int64) == 0_c_int8_t) exit
                e = e + 1_int64
            end do
            lo1 = 0_int64
            lo2 = 0_int64
            do k = s, e
                p = perm(k)
                if (p > nl) then
                    if (.not. isnull(p)) then
                        q = p - nl
                        if (lo1 == 0_int64 .or. q < lo1) then
                            lo2 = lo1
                            lo1 = q
                        else if (lo2 == 0_int64 .or. q < lo2) then
                            lo2 = q
                        end if
                    end if
                end if
            end do
            if (lo2 > 0_int64) then
                dup_first = lo1
                dup_at = lo2
                return
            end if
            s = e + 1_int64
        end do
    end subroutine remap_check_unique
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
