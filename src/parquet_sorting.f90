!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> Sorting for plain Fortran arrays and for this library's own column types.
!!
!! This module is the public face of the same C++ `std::sort` engine that orders a read-time
!! `parquet_open_reader(..., sort_by=)` and `parquet_table%sort_by`. Sharing one engine is the
!! point: a read-time sort, a table sort and a raw-array sort can never disagree about where
!! nulls go, where NaNs go, or how ties are broken.
!!
!! **Naming.** Everything public here carries the `pf_` prefix (parquet-fortran) rather than
!! `parquet_`, because the subject is not a parquet file -- see CLAUDE.md's "Naming
!! conventions". The module is `parquet_sorting` rather than `parquet_sort` because a module
!! cannot share its name with a procedure it declares.
!!
!! Four operations, over eleven element types:
!!
!! * `pf_argsort(values, perm)` -- the permutation that would sort `values`. Never modifies it.
!! * `pf_sort(values, sorted)` -- an independent sorted copy. Never modifies its input.
!! * `pf_permute(values, perm)` -- applies a permutation to `values` IN PLACE.
!! * `pf_is_sorted(values, answer)` -- whether `values` is already in the stated order.
!!
!! **Ordering reproduces `arrow::compute::SortIndices` exactly.** Null and NaN placement is
!! absolute: `descending` reverses the values, never the tiers. Ascending gives values, then
!! NaNs, then nulls; `nulls_first=.true.` gives nulls, then NaNs, then values. Ties always keep
!! their original order -- **every sort here is stable, unconditionally**, so there is no
!! `stable=` argument to pass.
!!
!! **Where nullness comes from depends on the type.** The six types with no null state of their
!! own (`integer`, `real`, `logical`, `character`) take an optional `is_valid(:)` mask; the
!! temporal types and the two column types carry their own and take no such argument.
!!
!! **Sorting one column of a table desynchronises it.** `%col` hands back a writable pointer
!! into a table's live storage, so `call pf_permute(p, perm)` on it reorders that column and
!! leaves every other column where it was, silently breaking row correspondence. Use
!! `parquet_table%sort_by`, which reorders every column together.
module parquet_sorting
    use, intrinsic :: iso_fortran_env, only : int8, int32, int64, real32, real64
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_int8_t, c_char, c_long_long
    use parquet_bindings, only : parquet_sort_builder_new, parquet_sort_builder_add_key_int64, &
        parquet_sort_builder_add_key_double, parquet_sort_builder_add_key_string, &
        parquet_sort_builder_build, parquet_sort_builder_is_sorted, parquet_sort_builder_free, &
        parquet_sort_argsort_int64, parquet_sort_argsort_double, parquet_sort_argsort_string, &
        parquet_sort_is_sorted_int64, parquet_sort_is_sorted_double, parquet_sort_is_sorted_string
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    use parquet_columns, only : parquet_column, parquet_kind_name, PK_INT32, PK_INT64, PK_FLOAT32, &
        PK_FLOAT64, PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP
    !
    implicit none
    private
    !
    public :: pf_sort_keys
    public :: pf_sort
    public :: pf_argsort
    public :: pf_permute
    public :: pf_is_sorted
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_sorting: "
    !
    ! ---- Internal key families ----
    integer, parameter :: SK_INT = 1  !! key values live in `ints`.
    integer, parameter :: SK_REAL = 2 !! key values live in `reals`.
    integer, parameter :: SK_STR = 3  !! key values live in `offsets`/`data`.
    !
    !> One extracted sort key, in the canonical form the C++ engine takes.
    !!
    !! Exactly one of `ints`/`reals`/(`offsets`,`data`) is allocated, matching `family`. `valid`
    !! is left UNALLOCATED when the key has no nulls at all, which is the engine's own fast path
    !! -- the same convention `parquet_column%row_validity` already uses.
    type :: sort_key_buf
        private
        integer :: family = SK_INT                          !! SK_INT / SK_REAL / SK_STR.
        logical :: descending = .false.                     !! .true. sorts high to low.
        logical :: nulls_first = .false.                    !! .true. places nulls before values.
        integer(int64), allocatable :: ints(:)              !! SK_INT values.
        real(real64), allocatable :: reals(:)               !! SK_REAL values.
        integer(int64), allocatable :: offsets(:)           !! SK_STR: n+1 byte offsets, 0-based.
        character(kind=c_char), allocatable :: data(:)      !! SK_STR: the packed bytes.
        integer(c_int8_t), allocatable :: valid(:)          !! 1 = valid; UNALLOCATED means no nulls.
    end type sort_key_buf
    !
    !> A list of sort keys, applied in the order added -- the first key added is the primary one.
    !!
    !! This is how a multi-key sort is expressed, because Fortran cannot offer "an optional
    !! second and third array, each of any type" as a generic: with eleven element types that
    !! would need over a thousand specific procedures. Add as many keys as needed, of any mix of
    !! types, then hand the object to `pf_argsort`:
    !!
    !! ```fortran
    !! type(pf_sort_keys) :: k
    !! call k%add(ra)                          ! primary
    !! call k%add(dec, descending=.true.)      ! breaks ties on ra
    !! call k%add(name)                        ! breaks ties on both
    !! call pf_argsort(k, perm)
    !! ```
    !!
    !! **It holds no C handle**, deliberately: its keys are ordinary allocatable Fortran arrays,
    !! and the C++ builder is created, used and freed entirely inside `pf_argsort`. That keeps
    !! this type free of a `FINAL`, free of an assignment guard, and -- because a finalizable
    !! type must never be given to OpenMP's `private()` -- usable per-thread in the obvious way.
    type :: pf_sort_keys
        private
        integer :: nkeys = 0                                !! keys added so far.
        integer(int64) :: nrows = -1                        !! rows every key must have; -1 until the first add.
        type(sort_key_buf), allocatable :: keys(:)          !! the keys, in precedence order.
    contains
        procedure, private :: add_i32 !! %add specific for a 32-bit integer key.
        procedure, private :: add_i64 !! %add specific for a 64-bit integer key.
        procedure, private :: add_f32 !! %add specific for a 32-bit real key.
        procedure, private :: add_f64 !! %add specific for a 64-bit real key.
        procedure, private :: add_bool !! %add specific for a logical key.
        procedure, private :: add_chr !! %add specific for a string key.
        procedure, private :: add_date !! %add specific for a date key.
        procedure, private :: add_time !! %add specific for a time key.
        procedure, private :: add_ts !! %add specific for a timestamp key.
        procedure, private :: add_strcol !! %add specific for a packed string column key.
        procedure, private :: add_col !! %add specific for a type-erased column key.
        !> Appends one sort key. Keys apply in the order added, the first being primary.
        generic :: add => add_i32, add_i64, add_f32, add_f64, add_bool, add_chr, add_date, add_time, add_ts, add_strcol, &
            add_col
        procedure :: nkeys_added => keys_count !! Number of keys added so far.
        procedure :: clear => keys_clear       !! Drops every key, leaving the object reusable.
    end type pf_sort_keys
    !
    !> The permutation that would sort `values`: `perm(k)` is the index of the element that
    !> belongs at position k. `values` is never modified.
    !>
    !> The permutation's integer kind is chosen by how the caller declares `perm`. The
    !> `integer(int32)` form aborts when the array is longer than `huge(1_int32)` rather than
    !> truncating; declare `perm` as `integer(int64)` for arrays that large.
    !>
    !> Also takes a `pf_sort_keys` object in place of `values`, for a multi-key sort.
    interface pf_argsort
        module procedure argsort_i32_i32
        module procedure argsort_i32_i64
        module procedure argsort_i64_i32
        module procedure argsort_i64_i64
        module procedure argsort_f32_i32
        module procedure argsort_f32_i64
        module procedure argsort_f64_i32
        module procedure argsort_f64_i64
        module procedure argsort_bool_i32
        module procedure argsort_bool_i64
        module procedure argsort_chr_i32
        module procedure argsort_chr_i64
        module procedure argsort_date_i32
        module procedure argsort_date_i64
        module procedure argsort_time_i32
        module procedure argsort_time_i64
        module procedure argsort_ts_i32
        module procedure argsort_ts_i64
        module procedure argsort_strcol_i32
        module procedure argsort_strcol_i64
        module procedure argsort_col_i32
        module procedure argsort_col_i64
        module procedure argsort_keys_i32
        module procedure argsort_keys_i64
    end interface pf_argsort
    !
    !> An independent sorted copy of `values`, leaving `values` untouched.
    !>
    !> Deliberately not defined for `parquet_string_column` or `parquet_column`: copying a
    !> whole column to sort it serves no purpose, and reordering one in place is
    !> `pf_argsort` followed by `pf_permute`, which says what it does at the call site.
    interface pf_sort
        module procedure sort_i32
        module procedure sort_i64
        module procedure sort_f32
        module procedure sort_f64
        module procedure sort_bool
        module procedure sort_chr
        module procedure sort_date
        module procedure sort_time
        module procedure sort_ts
    end interface pf_sort
    !
    !> Applies `perm` to `values` IN PLACE: afterwards element k is what was at `perm(k)`.
    !> `perm` itself is not modified.
    !>
    !> `perm` is validated as a true permutation of 1..n before anything is written, since an
    !> invalid one would silently duplicate some elements and drop others. Pass
    !> `assume_valid=.true.` to skip that check when the permutation came from `pf_argsort`
    !> and is known good. The two column types ignore it -- their own `%reindex` validates
    !> unconditionally.
    interface pf_permute
        module procedure permute_i32_i32
        module procedure permute_i32_i64
        module procedure permute_i64_i32
        module procedure permute_i64_i64
        module procedure permute_f32_i32
        module procedure permute_f32_i64
        module procedure permute_f64_i32
        module procedure permute_f64_i64
        module procedure permute_bool_i32
        module procedure permute_bool_i64
        module procedure permute_chr_i32
        module procedure permute_chr_i64
        module procedure permute_date_i32
        module procedure permute_date_i64
        module procedure permute_time_i32
        module procedure permute_time_i64
        module procedure permute_ts_i32
        module procedure permute_ts_i64
        module procedure permute_strcol_i32
        module procedure permute_strcol_i64
        module procedure permute_col_i32
        module procedure permute_col_i64
    end interface pf_permute
    !
    !> Whether `values` is already in the stated order. O(n) with an early exit, and no copy.
    !>
    !> Uses the same comparison `pf_sort` does, so the two can never disagree about nulls,
    !> NaNs or direction on one array. A run of equal values is sorted.
    interface pf_is_sorted
        module procedure is_sorted_i32
        module procedure is_sorted_i64
        module procedure is_sorted_f32
        module procedure is_sorted_f64
        module procedure is_sorted_bool
        module procedure is_sorted_chr
        module procedure is_sorted_date
        module procedure is_sorted_time
        module procedure is_sorted_ts
        module procedure is_sorted_strcol
        module procedure is_sorted_col
    end interface pf_is_sorted
    !
    ! ---- Key extraction and pf_sort_keys%add (parquet_sorting_keys) ----
    interface
        !> Extracts a 32-bit integer key into the canonical form the engine takes.
        module subroutine extract_i32(values, buf, descending, nulls_first, proc, is_valid)
        integer(int32), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine extract_i32
        !> Extracts a 64-bit integer key into the canonical form the engine takes.
        module subroutine extract_i64(values, buf, descending, nulls_first, proc, is_valid)
        integer(int64), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine extract_i64
        !> Extracts a 32-bit real key into the canonical form the engine takes.
        module subroutine extract_f32(values, buf, descending, nulls_first, proc, is_valid)
        real(real32), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine extract_f32
        !> Extracts a 64-bit real key into the canonical form the engine takes.
        module subroutine extract_f64(values, buf, descending, nulls_first, proc, is_valid)
        real(real64), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine extract_f64
        !> Extracts a logical key into the canonical form the engine takes.
        module subroutine extract_bool(values, buf, descending, nulls_first, proc, is_valid)
        logical, intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine extract_bool
        !> Extracts a string key into the canonical form the engine takes.
        module subroutine extract_chr(values, buf, descending, nulls_first, proc, is_valid)
        character(len=*), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine extract_chr
        !> Extracts a date key into the canonical form the engine takes.
        module subroutine extract_date(values, buf, descending, nulls_first, proc)
        type(parquet_date), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
        end subroutine extract_date
        !> Extracts a time key into the canonical form the engine takes.
        module subroutine extract_time(values, buf, descending, nulls_first, proc)
        type(parquet_time), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
        end subroutine extract_time
        !> Extracts a timestamp key into the canonical form the engine takes.
        module subroutine extract_ts(values, buf, descending, nulls_first, proc)
        type(parquet_timestamp), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
        end subroutine extract_ts
        !> Extracts a packed string column key into the canonical form the engine takes.
        module subroutine extract_strcol(values, buf, descending, nulls_first, proc)
        type(parquet_string_column), intent(in) :: values
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
        end subroutine extract_strcol
        !> Extracts a type-erased column key into the canonical form the engine takes.
        module subroutine extract_col(values, buf, descending, nulls_first, proc)
        type(parquet_column), intent(in) :: values
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
        end subroutine extract_col
        !> Appends a 32-bit integer sort key.
        module subroutine add_i32(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        integer(int32), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_i32
        !> Appends a 64-bit integer sort key.
        module subroutine add_i64(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        integer(int64), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_i64
        !> Appends a 32-bit real sort key.
        module subroutine add_f32(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        real(real32), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_f32
        !> Appends a 64-bit real sort key.
        module subroutine add_f64(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        real(real64), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_f64
        !> Appends a logical sort key.
        module subroutine add_bool(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        logical, intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_bool
        !> Appends a string sort key.
        module subroutine add_chr(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        character(len=*), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_chr
        !> Appends a date sort key.
        module subroutine add_date(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_date), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_date
        !> Appends a time sort key.
        module subroutine add_time(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_time), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_time
        !> Appends a timestamp sort key.
        module subroutine add_ts(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_timestamp), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_ts
        !> Appends a packed string column sort key.
        module subroutine add_strcol(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_string_column), intent(in) :: values
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_strcol
        !> Appends a type-erased column sort key.
        module subroutine add_col(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_column), intent(in) :: values
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_col
        !> Number of keys added so far.
        module function keys_count(self) result(n)
            class(pf_sort_keys), intent(in) :: self !! the key list.
            integer :: n                            !! keys added.
        end function keys_count
        !> Drops every key, leaving the object reusable.
        module subroutine keys_clear(self)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        end subroutine keys_clear
        !> Appends `buf` to `self`, checking every key describes the same number of rows.
        module subroutine keys_append(self, buf, proc)
            class(pf_sort_keys), intent(inout) :: self          !! the key list.
            type(sort_key_buf), allocatable, intent(inout) :: buf(:) !! keys to append; moved from.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
        end subroutine keys_append
        !> Runs the C++ engine over `keys`, returning a 1-based permutation.
        module subroutine drive_engine(keys, nrows, proc, perm)
            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.
            integer(int64), intent(in) :: nrows                 !! rows each key describes.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
        end subroutine drive_engine
        !> Whether every row is already in order under `keys`, using the same comparator
        !! `drive_engine` sorts with, so the two can never disagree.
        module subroutine engine_is_sorted(keys, nrows, proc, answer)
            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.
            integer(int64), intent(in) :: nrows                 !! rows each key describes.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
            logical, intent(out) :: answer                      !! .true. when already in order.
        end subroutine engine_is_sorted
        !> Builds the engine's int8 validity array from a logical mask, leaving `valid`
        !! UNALLOCATED when the mask marks nothing null (the engine's no-nulls fast path).
        module subroutine valid_from_mask(mask, n, proc, valid)
            logical, intent(in) :: mask(:)                          !! .false. marks a null.
            integer(int64), intent(in) :: n                         !! expected length.
            character(len=*), intent(in) :: proc                    !! calling procedure, for messages.
            integer(c_int8_t), allocatable, intent(out) :: valid(:) !! 1 per valid element.
        end subroutine valid_from_mask
        !> Aborts unless `perm` is a true permutation of 1..n. Uses a bit-packed seen-set, so
        !! the scratch is n/8 bytes rather than the 4n a default LOGICAL array would cost.
        module subroutine check_permutation(perm, n, proc)
            integer(int64), intent(in) :: perm(:) !! the permutation to validate.
            integer(int64), intent(in) :: n       !! expected length.
            character(len=*), intent(in) :: proc  !! calling procedure, for messages.
        end subroutine check_permutation
        !> Narrows a 1-based int64 permutation to int32, aborting rather than truncating.
        module subroutine narrow_perm(perm64, proc, perm32)
            integer(int64), intent(in) :: perm64(:)                !! the permutation.
            character(len=*), intent(in) :: proc                   !! calling procedure, for messages.
            integer(int32), allocatable, intent(out) :: perm32(:)  !! the narrowed copy.
        end subroutine narrow_perm
    end interface
    !
    ! ---- pf_argsort and pf_sort (parquet_sorting_argsort) ----
    interface
        !> pf_argsort over a 32-bit integer array, returning an int32 permutation.
        module subroutine argsort_i32_i32(values, perm, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_i32_i32
        !> pf_argsort over a 32-bit integer array, returning an int64 permutation.
        module subroutine argsort_i32_i64(values, perm, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_i32_i64
        !> pf_argsort over a 64-bit integer array, returning an int32 permutation.
        module subroutine argsort_i64_i32(values, perm, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_i64_i32
        !> pf_argsort over a 64-bit integer array, returning an int64 permutation.
        module subroutine argsort_i64_i64(values, perm, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_i64_i64
        !> pf_argsort over a 32-bit real array, returning an int32 permutation.
        module subroutine argsort_f32_i32(values, perm, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_f32_i32
        !> pf_argsort over a 32-bit real array, returning an int64 permutation.
        module subroutine argsort_f32_i64(values, perm, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_f32_i64
        !> pf_argsort over a 64-bit real array, returning an int32 permutation.
        module subroutine argsort_f64_i32(values, perm, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_f64_i32
        !> pf_argsort over a 64-bit real array, returning an int64 permutation.
        module subroutine argsort_f64_i64(values, perm, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_f64_i64
        !> pf_argsort over a logical array, returning an int32 permutation.
        module subroutine argsort_bool_i32(values, perm, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_bool_i32
        !> pf_argsort over a logical array, returning an int64 permutation.
        module subroutine argsort_bool_i64(values, perm, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_bool_i64
        !> pf_argsort over a string array, returning an int32 permutation.
        module subroutine argsort_chr_i32(values, perm, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_chr_i32
        !> pf_argsort over a string array, returning an int64 permutation.
        module subroutine argsort_chr_i64(values, perm, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argsort_chr_i64
        !> pf_argsort over a date array, returning an int32 permutation.
        module subroutine argsort_date_i32(values, perm, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_date_i32
        !> pf_argsort over a date array, returning an int64 permutation.
        module subroutine argsort_date_i64(values, perm, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_date_i64
        !> pf_argsort over a time array, returning an int32 permutation.
        module subroutine argsort_time_i32(values, perm, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_time_i32
        !> pf_argsort over a time array, returning an int64 permutation.
        module subroutine argsort_time_i64(values, perm, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_time_i64
        !> pf_argsort over a timestamp array, returning an int32 permutation.
        module subroutine argsort_ts_i32(values, perm, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_ts_i32
        !> pf_argsort over a timestamp array, returning an int64 permutation.
        module subroutine argsort_ts_i64(values, perm, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_ts_i64
        !> pf_argsort over a packed string column array, returning an int32 permutation.
        module subroutine argsort_strcol_i32(values, perm, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_strcol_i32
        !> pf_argsort over a packed string column array, returning an int64 permutation.
        module subroutine argsort_strcol_i64(values, perm, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_strcol_i64
        !> pf_argsort over a type-erased column array, returning an int32 permutation.
        module subroutine argsort_col_i32(values, perm, descending, nulls_first)
        type(parquet_column), intent(in) :: values
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_col_i32
        !> pf_argsort over a type-erased column array, returning an int64 permutation.
        module subroutine argsort_col_i64(values, perm, descending, nulls_first)
        type(parquet_column), intent(in) :: values
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine argsort_col_i64
        !> pf_argsort over a multi-key `pf_sort_keys`, returning an int32 permutation.
        module subroutine argsort_keys_i32(keys, perm)
            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
        end subroutine argsort_keys_i32
        !> pf_argsort over a multi-key `pf_sort_keys`, returning an int64 permutation.
        module subroutine argsort_keys_i64(keys, perm)
            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
        end subroutine argsort_keys_i64
        !> pf_sort over a 32-bit integer array: an independent sorted copy.
        module subroutine sort_i32(values, sorted, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine sort_i32
        !> pf_sort over a 64-bit integer array: an independent sorted copy.
        module subroutine sort_i64(values, sorted, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine sort_i64
        !> pf_sort over a 32-bit real array: an independent sorted copy.
        module subroutine sort_f32(values, sorted, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            real(real32), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine sort_f32
        !> pf_sort over a 64-bit real array: an independent sorted copy.
        module subroutine sort_f64(values, sorted, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            real(real64), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine sort_f64
        !> pf_sort over a logical array: an independent sorted copy.
        module subroutine sort_bool(values, sorted, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            logical, allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine sort_bool
        !> pf_sort over a string array: an independent sorted copy.
        module subroutine sort_chr(values, sorted, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            character(len=len(values)), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine sort_chr
        !> pf_sort over a date array: an independent sorted copy.
        module subroutine sort_date(values, sorted, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            type(parquet_date), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine sort_date
        !> pf_sort over a time array: an independent sorted copy.
        module subroutine sort_time(values, sorted, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            type(parquet_time), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine sort_time
        !> pf_sort over a timestamp array: an independent sorted copy.
        module subroutine sort_ts(values, sorted, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            type(parquet_timestamp), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine sort_ts
    end interface
    !
    ! ---- pf_permute and pf_is_sorted (parquet_sorting_permute) ----
    interface
        !> pf_permute over a 32-bit integer array, with an int32 permutation.
        module subroutine permute_i32_i32(values, perm, assume_valid)
        integer(int32), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_i32_i32
        !> pf_permute over a 32-bit integer array, with an int64 permutation.
        module subroutine permute_i32_i64(values, perm, assume_valid)
        integer(int32), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_i32_i64
        !> pf_permute over a 64-bit integer array, with an int32 permutation.
        module subroutine permute_i64_i32(values, perm, assume_valid)
        integer(int64), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_i64_i32
        !> pf_permute over a 64-bit integer array, with an int64 permutation.
        module subroutine permute_i64_i64(values, perm, assume_valid)
        integer(int64), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_i64_i64
        !> pf_permute over a 32-bit real array, with an int32 permutation.
        module subroutine permute_f32_i32(values, perm, assume_valid)
        real(real32), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_f32_i32
        !> pf_permute over a 32-bit real array, with an int64 permutation.
        module subroutine permute_f32_i64(values, perm, assume_valid)
        real(real32), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_f32_i64
        !> pf_permute over a 64-bit real array, with an int32 permutation.
        module subroutine permute_f64_i32(values, perm, assume_valid)
        real(real64), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_f64_i32
        !> pf_permute over a 64-bit real array, with an int64 permutation.
        module subroutine permute_f64_i64(values, perm, assume_valid)
        real(real64), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_f64_i64
        !> pf_permute over a logical array, with an int32 permutation.
        module subroutine permute_bool_i32(values, perm, assume_valid)
        logical, intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_bool_i32
        !> pf_permute over a logical array, with an int64 permutation.
        module subroutine permute_bool_i64(values, perm, assume_valid)
        logical, intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_bool_i64
        !> pf_permute over a string array, with an int32 permutation.
        module subroutine permute_chr_i32(values, perm, assume_valid)
        character(len=*), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_chr_i32
        !> pf_permute over a string array, with an int64 permutation.
        module subroutine permute_chr_i64(values, perm, assume_valid)
        character(len=*), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_chr_i64
        !> pf_permute over a date array, with an int32 permutation.
        module subroutine permute_date_i32(values, perm, assume_valid)
        type(parquet_date), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_date_i32
        !> pf_permute over a date array, with an int64 permutation.
        module subroutine permute_date_i64(values, perm, assume_valid)
        type(parquet_date), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_date_i64
        !> pf_permute over a time array, with an int32 permutation.
        module subroutine permute_time_i32(values, perm, assume_valid)
        type(parquet_time), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_time_i32
        !> pf_permute over a time array, with an int64 permutation.
        module subroutine permute_time_i64(values, perm, assume_valid)
        type(parquet_time), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_time_i64
        !> pf_permute over a timestamp array, with an int32 permutation.
        module subroutine permute_ts_i32(values, perm, assume_valid)
        type(parquet_timestamp), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_ts_i32
        !> pf_permute over a timestamp array, with an int64 permutation.
        module subroutine permute_ts_i64(values, perm, assume_valid)
        type(parquet_timestamp), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_ts_i64
        !> pf_permute over a packed string column array, with an int32 permutation.
        module subroutine permute_strcol_i32(values, perm, assume_valid)
        type(parquet_string_column), intent(inout) :: values
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_strcol_i32
        !> pf_permute over a packed string column array, with an int64 permutation.
        module subroutine permute_strcol_i64(values, perm, assume_valid)
        type(parquet_string_column), intent(inout) :: values
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_strcol_i64
        !> pf_permute over a type-erased column array, with an int32 permutation.
        module subroutine permute_col_i32(values, perm, assume_valid)
        type(parquet_column), intent(inout) :: values
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_col_i32
        !> pf_permute over a type-erased column array, with an int64 permutation.
        module subroutine permute_col_i64(values, perm, assume_valid)
        type(parquet_column), intent(inout) :: values
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.
        end subroutine permute_col_i64
        !> pf_is_sorted over a 32-bit integer array.
        module subroutine is_sorted_i32(values, answer, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_i32
        !> pf_is_sorted over a 64-bit integer array.
        module subroutine is_sorted_i64(values, answer, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_i64
        !> pf_is_sorted over a 32-bit real array.
        module subroutine is_sorted_f32(values, answer, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_f32
        !> pf_is_sorted over a 64-bit real array.
        module subroutine is_sorted_f64(values, answer, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_f64
        !> pf_is_sorted over a logical array.
        module subroutine is_sorted_bool(values, answer, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_bool
        !> pf_is_sorted over a string array.
        module subroutine is_sorted_chr(values, answer, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_chr
        !> pf_is_sorted over a date array.
        module subroutine is_sorted_date(values, answer, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_date
        !> pf_is_sorted over a time array.
        module subroutine is_sorted_time(values, answer, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_time
        !> pf_is_sorted over a timestamp array.
        module subroutine is_sorted_ts(values, answer, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_ts
        !> pf_is_sorted over a packed string column array.
        module subroutine is_sorted_strcol(values, answer, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_strcol
        !> pf_is_sorted over a type-erased column array.
        module subroutine is_sorted_col(values, answer, descending, nulls_first)
        type(parquet_column), intent(in) :: values
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_col
    end interface
    !
end module parquet_sorting ! GCOVR_EXCL_LINE
