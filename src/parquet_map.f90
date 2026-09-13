!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> MAP column storage: `parquet_map_column`, plus `parquet_map_row`, a non-owning handle to one
!! of its rows.
!!
!! A map column's rows each hold zero or more `key -> value` entries. Keys are **strings**;
!! values are one of the nine scalar kinds, fixed for the whole column. That is what the Parquet
!! `MAP` logical type describes, and physically it is a `LIST` of `struct<key, value>` -- which
!! is exactly how this type stores it.
!!
!! Five things are worth knowing before using it:
!!
!! * **Keys and values are TWO flattened `parquet_column`s**, and `offsets(:)` says where each
!!   row's entries begin in both. So the per-VALUE null bitmap, the value accessors and every
!!   maintenance operation come from a type that already exists and is already tested.
!! * **Entries keep the order they were appended, and DUPLICATE KEYS ARE PRESERVED.** Neither
!!   Arrow nor Parquet requires map keys to be unique, and this type adds no check the format
!!   does not make. `%get(key, ...)` returns the FIRST match; `%key_count(key)` says how many
!!   there are and `occurrence=` selects a later one.
!! * **A KEY IS NEVER NULL.** Arrow's `MapType` declares its key field non-nullable and offers no
!!   way to change that, so a map has exactly TWO null levels: the row (this map is absent) and
!!   the value (this entry's value is missing). An empty key string is a legitimate key and is
!!   not a null one. Do not look for a third level -- Arrow will not store it.
!! * **A null row and a present-but-EMPTY row are different**, and both report `%size() == 0`.
!!   `%is_null(i)` is what distinguishes them, exactly as it does for a list column.
!! * **The value kind is fixed at `%init` and cannot change.** Inferring it from the first row
!!   appended would be the "sized/typed from the first element" bug class, and would leave
!!   `%element_kind()` unable to answer for an empty column.
!!
!! **Storage layout.** `offsets` has `nrows+1` entries with `offsets(1) == 0`, and row `i` holds
!! entries `offsets(i)+1 .. offsets(i+1)` of both `keys` and `values`. That is Arrow's own
!! convention, so handing these buffers across is a copy rather than a translation.
!!
!! **A null row is ZERO-LENGTH.** `%set_null(i)` removes the row's entries and shifts every later
!! offset down, so the offsets never describe a span for a row the validity bitmap calls absent.
!! Arrow's Parquet writer refuses the other shape outright ("Lists with non-zero length null
!! components are not supported" -- a map is a list of key/value structs there), and the refusal
!! arrives as an uncatchable C++ exception, so a column that kept those entries could be built and
!! then never written. `%set_null` is therefore O(n) in the entry columns rather than O(1); row
!! indices do not move, only entry positions. The corollary is that `%clear_null(i)` brings back an
!! EMPTY row, never the entries the row held before.
!!
!! **Within-row counts and positions are `integer(int32)`, deliberately.** Arrow addresses a map's
!! entries with an int32 offsets buffer and provides no `large_map`, so no column -- let alone one
!! row -- can hold more entries than an int32 can index. `CLAUDE.md`'s rule that a public numeric
!! argument should offer both kinds explicitly exempts a value the format already bounds, and this
!! is one. Row INDICES are `integer(int64)` and generic over both kinds, as everywhere else.
!!
!! **By-key lookup is a linear scan of the row**, not an index. `%key_count` and `occurrence=`
!! both need the whole row walked anyway, and an index would be state that a missed invalidation
!! turns into a wrong answer rather than a slow one. A caller iterating every entry of a row
!! should use `%key_at`/`%get_at` rather than looking each key up by name.
!!
!! Depends only on `iso_fortran_env`, `parquet_columns`, the two element-domain modules that
!! brings in, and `parquet_settings_base` for the warning channel its soft-fail paths emit on.
!! It reaches no `bind(C)` interface -- see `parquet_map_column%summary`, which composes a string
!! instead of writing to a unit for exactly that reason.
module parquet_map
    use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
    ! The TYPED (non-polymorphic) tier of parquet_column. Per-entry access to the keys and values
    ! goes through these free generics, NEVER through a type-bound call on `self%keys` or
    ! `self%values`: passing a `type(parquet_column)` actual to a `class` passed-object dummy in
    ! another compilation unit makes ifx build a 21-record runtime type descriptor in STATIC
    ! storage, in the caller's prologue, on every call. Enforced by
    ! check_no_type_bound_column_access (tools/check_source_conventions.py).
    use parquet_columns, only : parquet_column, parquet_container_column, parquet_kind_name, &
        parquet_column_data_ptr, parquet_column_get_at, parquet_column_is_null, &
        parquet_column_set_null, parquet_column_string_column, &
        parquet_column_container, parquet_kind_is_container, &
        PK_NONE, PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, &
        PK_DATE, PK_TIME, PK_TIMESTAMP, PK_MAP, PK_LIST, PK_STRUCT
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    ! The keys are compared through parquet_strings' own TYPED tier, for the same reason and with
    ! the same rule: a key scan is per ENTRY, so it must not allocate per entry and must not reach
    ! the string store through a type-bound call. `parquet_strings` is already in this module's
    ! closure via `parquet_columns`, so naming it directly costs no compiled file.
    use parquet_strings, only : parquet_string_column, parquet_string_column_copy_to
    use parquet_settings_base, only : parquet_emit_warning, &
        parquet_get_verbosity, parquet_set_verbosity, &
        parquet_get_message_stream, parquet_set_message_stream
    !
    implicit none
    private
    !
    public :: parquet_map_column
    public :: parquet_map_row
    !
    ! Re-exported so that `use parquet_map` alone is enough to declare a map column, name its
    ! value kind and read a temporal value back -- the entry-module rule in CLAUDE.md, and what
    ! test_module_surface_map (test/test_module_surface.f90) asserts with a single import.
    public :: parquet_column, parquet_container_column, parquet_kind_name
    public :: PK_NONE, PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING
    public :: PK_DATE, PK_TIME, PK_TIMESTAMP, PK_MAP
    public :: parquet_date, parquet_time, parquet_timestamp
    ! Re-exported because this module's soft-fail paths EMIT: a module re-exports, getter and
    ! setter both, every settings knob its own code reads (CLAUDE.md, "Nested submodule tree").
    public :: parquet_get_verbosity, parquet_set_verbosity
    public :: parquet_get_message_stream, parquet_set_message_stream
    !
    ! INTERNAL API, on the same terms as parquet_columns' own `parquet_column_*` tier: a map
    ! column's offsets and its two flattened entry columns, as pointers, so that the WRITE path
    ! (src/parquet_write_map.f90) can hand contiguous buffers to Arrow without copying and without
    ! a per-row allocation. `parquet_map_column`'s components are private, so there is no other
    ! route.
    !
    ! `src/parquet.f90` privatises all four again, so the `use parquet` surface is unchanged. Each
    ! takes a `type(parquet_map_column)` dummy rather than a `class` one, for the reason the import
    ! comment above gives.
    public :: parquet_map_column_offsets
    public :: parquet_map_column_keys
    public :: parquet_map_column_values
    public :: parquet_map_column_row_validity
    !
    !> Error-message prefix for every `error stop` and warning raised by this module.
    character(len=*), parameter :: EP = "parquet_map: "
    !
    !> Bits per validity-bitmap block, matching `parquet_columns`' own bitmap layout.
    integer(int64), parameter :: BITS_PER_BLOCK = 64_int64
    !
    !> Smallest offsets allocation, so that a column built one row at a time does not reallocate
    !! on each of its first few appends.
    integer(int64), parameter :: MIN_ROW_CAP = 8_int64
    !
    !> A map column: `nrows` rows, each holding zero or more `key -> value` entries.
    !!
    !! Extends `parquet_container_column`, so a `parquet_column` can hold one through
    !! `%adopt_container` and reach it without naming this type. See the module doc for the
    !! storage layout, the two null levels and the duplicate-key rule.
    type, extends(parquet_container_column) :: parquet_map_column
        private
        !> `nrows+1` entries, `offsets(1) == 0`; row i is entries offsets(i)+1..offsets(i+1).
        !!
        !! Allocated with slack: row capacity is `size(offsets) - 1`, and growth is geometric
        !! (1.5x, `ensure_offsets_cap`) so appending row by row is amortised O(1). Only entries
        !! `1 .. nrows_+1` are meaningful; reading past that returns uninitialised memory.
        integer(int64), allocatable :: offsets(:)
        !> Row-level null bitmap, 1 = null, `BITS_PER_BLOCK` rows to a block. LAZY: a column with
        !! no null row allocates nothing at all and `has_nulls_` stays .false.
        integer(int64), allocatable :: validity(:)
        !> Every row's keys, flattened, in row order. Always `PK_STRING`, and never null -- see
        !! the module doc.
        type(parquet_column) :: keys
        !> Every row's values, flattened, in row order and index-aligned with `keys`. Carries the
        !! value kind, the per-VALUE validity and the unit string, so none of that is duplicated.
        type(parquet_column) :: values
        integer(int64) :: nrows_ = 0            !! rows stored.
        integer :: value_kind = PK_NONE         !! the values' PK_* kind, fixed at %init.
        logical :: has_nulls_ = .false.         !! .true. while the row bitmap is materialized.
    contains
        ! --- the deferred face parquet_columns reaches this type through (one per binding) ---
        procedure :: kindof => mc_kindof                 !! Always PK_MAP.
        procedure :: nrows => mc_nrows                   !! Rows stored.
        procedure :: clone_into => mc_clone_into         !! Allocate an independent copy.
        procedure :: gather_rows => mc_gather_rows       !! Rebuild so row k becomes old row idx(k).
        procedure :: append_from => mc_append_from       !! Append every row of another map column.
        procedure :: grow_rows => mc_grow_rows           !! Append n null rows.
        procedure :: reserve_rows => mc_reserve_rows     !! Reserve row capacity.
        procedure :: ensure_validity => mc_ensure_validity !! Materialize the row bitmap eagerly.
        procedure :: kind_text => mc_kind_text           !! e.g. "map<string,int32>".
        procedure :: is_null_row => mc_is_null_row       !! Whether row i is a null map.
        procedure :: set_null_row => mc_set_null_row     !! Mark row i a null map.
        procedure :: clear_null_row => mc_clear_null_row !! Mark row i present again.
        ! --- lifecycle ---
        procedure :: init                                !! Fix the value kind and (optionally) create null rows.
        procedure :: clear                               !! Release everything and reset to an uninitialized column.
        procedure :: deep_copy                           !! Independent copy of offsets, validity, keys and values.
        procedure :: move_from                           !! Take over another column's storage, leaving it empty.
        procedure :: adopt_rows                          !! Build from moved-in offsets, keys and values.
        ! --- queries ---
        procedure :: size => mc_nrows_public             !! Rows stored (alias of %nrows()).
        procedure :: is_init                             !! Whether %init has fixed a value kind.
        procedure :: element_kind                        !! The values' PK_* kind.
        procedure :: total_entries                       !! Entries stored across every row.
        procedure :: null_count                          !! Number of null rows.
        procedure :: capacity                            !! Rows the offsets allocation covers.
        procedure :: has_validity_storage                !! Whether nulling a row would still have to allocate.
        procedure, private :: length_i32                 !! int32 specific of length.
        procedure, private :: length_i64                 !! int64 specific of length.
        generic :: length => length_i32, length_i64      !! Entries in row i (0 for a null row).
        procedure, private :: is_null_i32                !! int32 specific of is_null.
        procedure, private :: is_null_i64                !! int64 specific of is_null.
        generic :: is_null => is_null_i32, is_null_i64   !! Whether row i is a null map.
        procedure, private :: is_empty_i32               !! int32 specific of is_empty.
        procedure, private :: is_empty_i64               !! int64 specific of is_empty.
        generic :: is_empty => is_empty_i32, is_empty_i64 !! Whether row i holds no entries (true for a null row).
        procedure, private :: view_i32                   !! int32 specific of view.
        procedure, private :: view_i64                   !! int64 specific of view.
        generic :: view => view_i32, view_i64            !! Zero-copy handle to row i.
        procedure :: validate                            !! Verify the class invariants.
        procedure :: summary                             !! One-line human-readable description.
        ! --- capacity ---
        procedure, private :: reserve_i32                !! int32 specific of reserve.
        procedure, private :: reserve_i64                !! int64 specific of reserve.
        generic :: reserve => reserve_i32, reserve_i64   !! Grow capacity to at least n rows.
        procedure :: shrink_to_fit                       !! Release capacity beyond the rows stored.
        ! --- mutation ---
        procedure, private :: append_row_i32             !! append_row specific for int32 values.
        procedure, private :: append_row_i64             !! append_row specific for int64 values.
        procedure, private :: append_row_f32             !! append_row specific for float32 values.
        procedure, private :: append_row_f64             !! append_row specific for float64 values.
        procedure, private :: append_row_bool            !! append_row specific for logical values.
        procedure, private :: append_row_str             !! append_row specific for string values.
        procedure, private :: append_row_date            !! append_row specific for date values.
        procedure, private :: append_row_time            !! append_row specific for time values.
        procedure, private :: append_row_ts               !! append_row specific for timestamp values.
        !> Appends one row holding `keys(k) -> values(k)`, optionally with a per-VALUE `is_valid`
        !! mask. The value type must match the kind fixed at `%init`, and `keys` and `values`
        !! must have the same length. Duplicate keys are stored as given.
        generic :: append_row => append_row_i32, append_row_i64, append_row_f32, append_row_f64, &
            append_row_bool, append_row_str, append_row_date, append_row_time, append_row_ts
        procedure :: append_null_row                     !! Append one null (absent) row.
        procedure :: append_empty_row                    !! Append one present row holding no entries.
        procedure, private :: set_null_i32               !! int32 specific of set_null.
        procedure, private :: set_null_i64               !! int64 specific of set_null.
        generic :: set_null => set_null_i32, set_null_i64 !! Mark row i null; see the module doc.
        procedure, private :: clear_null_i32             !! int32 specific of clear_null.
        procedure, private :: clear_null_i64             !! int64 specific of clear_null.
        generic :: clear_null => clear_null_i32, clear_null_i64 !! Mark row i present again.
        !
        ! THIS TYPE DELIBERATELY HAS NO `final` PROCEDURE, and one must not be added -- see the
        ! same note on parquet_list_column (src/parquet_list.f90), parquet_struct_column
        ! (src/parquet_struct.f90) and parquet_string (src/parquet_strings.f90). It owns nothing
        ! the language does not already free: three allocatable components and two plain
        ! derived-type components, no C++ handle and no OpenMP lock. Three shipped properties
        ! depend on the absence -- nagfor 7.2 emits invalid C when finalizing an ARRAY whose
        ! element type has a finalizable COMPONENT (and one of these will sit inside a
        ! parquet_column inside a parquet_table_column inside an array), gfortran refuses a
        ! finalizable type in an OpenMP `private()` clause, and intrinsic assignment to or from
        ! one runs the finalizer twice per iteration.
    end type parquet_map_column
    !
    !> A lightweight, non-owning handle to one row of a `parquet_map_column`.
    !!
    !! Holds a column pointer plus a 1-based row index and resolves lazily on access, so it stays
    !! valid across appends and reserves of the referenced column. It is invalidated by anything
    !! that changes the row set (`clear`, `move_from`, a `gather_rows` rebuild) or by the column
    !! going out of scope. **The referenced column must be declared with the `target` attribute
    !! and must outlive the handle** -- the same contract `parquet_string_column%view` documents,
    !! and F2018 15.5.2.4 leaves the stored pointer undefined otherwise.
    !!
    !! **The soft-fail convention on every lookup here** (`%get`, `%get_at`, `%key_at`): by
    !! default a missing key or an out-of-range position is a hard `error stop`. Pass
    !! `warn=.true.` to emit one warning and return instead, or supply `found` to be told without
    !! any output. With neither, the call aborts -- otherwise a caller could not learn that the
    !! lookup failed at all. **On a failed lookup `value` is left at the type's default and must
    !! not be used.** A wrong VALUE KIND is a different class and always aborts, with no soft
    !! option, exactly as reading a column as the wrong type does everywhere else in this library.
    type :: parquet_map_row
        private
        class(parquet_map_column), pointer :: col => null() !! referenced column (borrowed).
        integer(int64) :: idx = 0                           !! 1-based row index.
    contains
        procedure :: is_valid => pmr_is_valid       !! Whether this handle refers to a live row.
        procedure :: size => pmr_size               !! Entries in the referenced row.
        procedure :: is_null => pmr_is_null         !! Whether the referenced row is a null map.
        procedure :: is_empty => pmr_is_empty       !! Whether the referenced row holds no entries.
        procedure :: element_kind => pmr_element_kind !! The values' PK_* kind.
        procedure :: row_index => pmr_row_index     !! The 1-based row this handle refers to.
        procedure :: nested => pmr_nested           !! The inner container, when the value is one.
        procedure :: key_count => pmr_key_count     !! How many entries of this row carry `key`.
        procedure :: contains_key => pmr_contains_key !! Whether this row carries `key` at all.
        procedure :: key_at => pmr_key_at           !! The key at 1-based position `pos` in this row.
        procedure, private :: pmr_get_i32           !! get specific for an int32 value.
        procedure, private :: pmr_get_i64           !! get specific for an int64 value.
        procedure, private :: pmr_get_f32           !! get specific for a float32 value.
        procedure, private :: pmr_get_f64           !! get specific for a float64 value.
        procedure, private :: pmr_get_bool          !! get specific for a logical value.
        procedure, private :: pmr_get_str           !! get specific for a string value.
        procedure, private :: pmr_get_date          !! get specific for a date value.
        procedure, private :: pmr_get_time          !! get specific for a time value.
        procedure, private :: pmr_get_ts            !! get specific for a timestamp value.
        !> Reads the value stored under `key`, by default the FIRST entry carrying it.
        !! `occurrence=` selects a later one; see the type's own soft-fail note for `warn`/`found`.
        generic :: get => pmr_get_i32, pmr_get_i64, pmr_get_f32, pmr_get_f64, pmr_get_bool, &
            pmr_get_str, pmr_get_date, pmr_get_time, pmr_get_ts
        procedure, private :: pmr_at_i32            !! get_at specific for an int32 value.
        procedure, private :: pmr_at_i64            !! get_at specific for an int64 value.
        procedure, private :: pmr_at_f32            !! get_at specific for a float32 value.
        procedure, private :: pmr_at_f64            !! get_at specific for a float64 value.
        procedure, private :: pmr_at_bool           !! get_at specific for a logical value.
        procedure, private :: pmr_at_str            !! get_at specific for a string value.
        procedure, private :: pmr_at_date           !! get_at specific for a date value.
        procedure, private :: pmr_at_time           !! get_at specific for a time value.
        procedure, private :: pmr_at_ts             !! get_at specific for a timestamp value.
        !> Reads the value at 1-based position `pos` in this row, in stored order. `%key_at(pos)`
        !! gives the matching key; the pair is how a caller walks a row without a key lookup.
        generic :: get_at => pmr_at_i32, pmr_at_i64, pmr_at_f32, pmr_at_f64, pmr_at_bool, &
            pmr_at_str, pmr_at_date, pmr_at_time, pmr_at_ts
        !
        ! NO `final` HERE EITHER, and for the stronger of the two reasons: this is a NON-OWNING
        ! handle -- a borrowed pointer and an index -- so there is nothing to release. See
        ! parquet_string (src/parquet_strings.f90), which carried one, had it removed, and
        ! records the three properties that depend on its absence.
    end type parquet_map_row
    !
contains
    !
    ! ==================================================================================
    ! The deferred face: what parquet_columns reaches this type through
    ! ==================================================================================
    !
    !> A map column is always `PK_MAP`; see `parquet_container_column%kindof`.
    pure function mc_kindof(self) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer :: res                                !! always PK_MAP.
        res = PK_MAP
    end function mc_kindof
    !
    !> Number of rows stored; see `parquet_container_column%nrows`.
    pure function mc_nrows(self) result(n)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64) :: n                           !! rows stored.
        n = self%nrows_
    end function mc_nrows
    !
    !> Allocates `out` as an independent copy of this column; see
    !! `parquet_container_column%clone_into`.
    subroutine mc_clone_into(self, out)
        class(parquet_map_column), intent(in) :: self                   !! the source column.
        class(parquet_container_column), allocatable, intent(out) :: out !! the copy, allocated here.
        type(parquet_map_column), allocatable :: cp
        allocate(cp)
        call self%deep_copy(cp)
        call move_alloc(cp, out)
    end subroutine mc_clone_into
    !
    !> Rebuilds the column so row k becomes the row that was at `idx(k)`; see
    !! `parquet_container_column%gather_rows`.
    !!
    !! Keys and values are rebuilt by ONE `parquet_column%gather` each over the same flattened
    !! entry indices, which is what keeps the two index-aligned by construction rather than by
    !! two loops that could diverge. **Entries belonging to rows that `idx` does not name are
    !! dropped**, which is what compacts a permutation that omits or repeats rows.
    subroutine mc_gather_rows(self, idx)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: idx(:)             !! 1-based source row per destination row.
        integer(int64) :: n, k, src, lo, hi, m, pos, e
        integer(int64), allocatable :: new_offsets(:), entry_idx(:), new_valid(:)
        logical, allocatable :: src_null(:)
        n = size(idx, kind=int64)
        do k = 1_int64, n
            if (idx(k) < 1_int64 .or. idx(k) > self%nrows_) then
                error stop EP//"gather_rows: source row index out of range"
            end if
        end do
        if (self%value_kind == PK_NONE) then
            ! An uninitialized column has no rows, so `idx` is necessarily empty and there is
            ! nothing to rebuild. Returning here keeps the two entry columns out of
            ! parquet_column%gather, which would abort on a PK_NONE column.
            self%nrows_ = 0_int64
            return
        end if
        ! Row nullness is read BEFORE anything is overwritten: `self%validity` and `self%offsets`
        ! are both replaced below, so a second pass over them afterwards would be reading the
        ! DESTINATION while asking about the SOURCE.
        allocate(src_null(max(n, 1_int64)))
        do k = 1_int64, n
            src_null(k) = row_is_null(self, idx(k))
        end do
        m = 0_int64
        do k = 1_int64, n
            src = idx(k)
            if (.not. src_null(k)) m = m + (self%offsets(src + 1_int64) - self%offsets(src))
        end do
        allocate(new_offsets(n + 1_int64))
        allocate(entry_idx(max(m, 1_int64)))
        new_offsets(1) = 0_int64
        pos = 0_int64
        do k = 1_int64, n
            src = idx(k)
            if (.not. src_null(k)) then
                lo = self%offsets(src) + 1_int64
                hi = self%offsets(src + 1_int64)
                do e = lo, hi
                    pos = pos + 1_int64
                    entry_idx(pos) = e
                end do
            end if
            new_offsets(k + 1_int64) = pos
        end do
        call self%keys%gather(entry_idx(1:m))
        call self%values%gather(entry_idx(1:m))
        call move_alloc(new_offsets, self%offsets)
        ! The row bitmap is rebuilt from scratch rather than permuted in place: it is one bit per
        ! row, so a fresh map costs nrows/64 words and cannot inherit a stale bit belonging to a
        ! row the permutation dropped.
        if (allocated(self%validity)) deallocate(self%validity)
        self%has_nulls_ = .false.
        if (any(src_null(1:n))) then
            allocate(new_valid(max(blocks_for(n), 1_int64)))
            new_valid = 0_int64
            call move_alloc(new_valid, self%validity)
            self%has_nulls_ = .true.
            do k = 1_int64, n
                if (src_null(k)) call bit_set(self%validity, k)
            end do
        end if
        self%nrows_ = n
    end subroutine mc_gather_rows
    !
    !> Appends `n` null rows; see `parquet_container_column%grow_rows`.
    subroutine mc_grow_rows(self, n)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                  !! rows to append.
        integer(int64) :: k
        if (n < 0_int64) error stop EP//"grow_rows: negative row count"
        if (n == 0_int64) return
        call require_init(self, "grow_rows")
        do k = 1_int64, n
            call self%append_null_row()
        end do
    end subroutine mc_grow_rows
    !
    !> Appends every row of `src`; see `parquet_container_column%append_from`.
    !!
    !! The list column's own `append_from` explains the three rules (dynamic type, rebased
    !! offsets, merged validity); the only difference here is that TWO payload columns are
    !! concatenated instead of one, and they must stay index-aligned -- a key without its value,
    !! or the two appended in different orders, is a map that reads back with the wrong values
    !! against the right keys, which no length or row-count check would catch.
    subroutine mc_append_from(self, src)
        class(parquet_map_column), intent(inout) :: self  !! the destination column.
        class(parquet_container_column), intent(in) :: src !! rows to append, left unchanged.
        integer(int64) :: k, m, base, first, n0
        character(len=:), allocatable :: mine, theirs
        select type (src)
        type is (parquet_map_column)
            m = src%nrows_
            if (m == 0_int64) return
            call require_init(self, "append_from")
            if (src%value_kind /= self%value_kind) then
                call value_kind_text(self%value_kind, mine)
                call value_kind_text(src%value_kind, theirs)
                error stop EP//"append_from: cannot append a map<string,"//theirs// &
                    "> onto a map<string,"//mine//">"
            end if
            n0 = self%nrows_
            call ensure_offsets_cap(self, n0 + m)
            ! Both payloads, in the same order, so entry j of the appended block is the same
            ! entry in each.
            call self%keys%append(src%keys)
            call self%values%append(src%values)
            base = self%offsets(n0 + 1_int64)
            first = src%offsets(1)
            do k = 1_int64, m
                self%offsets(n0 + k + 1_int64) = base + (src%offsets(k + 1_int64) - first)
            end do
            self%nrows_ = n0 + m
            if (src%has_nulls_) then
                do k = 1_int64, m
                    if (row_is_null(src, k)) then
                        call ensure_validity_cap(self, self%nrows_)
                        self%has_nulls_ = .true.
                        call bit_set(self%validity, n0 + k)
                    end if
                end do
            end if
        class default
            call src%kind_text(theirs)
            call self%kind_text(mine)
            error stop EP//"append_from: cannot append a "//theirs//" onto a "//mine
        end select
    end subroutine mc_append_from
    !
    !> Reserves capacity for at least `n` rows; see `parquet_container_column%reserve_rows`.
    subroutine mc_reserve_rows(self, n)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                  !! rows to reserve for.
        if (n < 0_int64) error stop EP//"reserve_rows: negative row count"
        call ensure_offsets_cap(self, n)
        if (self%has_nulls_) call ensure_validity_cap(self, n)
    end subroutine mc_reserve_rows
    !
    !> Materializes the row bitmap and the values column's own validity storage eagerly; see
    !! `parquet_container_column%ensure_validity`.
    !!
    !! The concurrency escape hatch: several threads filling one column must not race on the lazy
    !! first allocation, and this is what a caller runs before the parallel region so they cannot.
    !! Both levels that can be null are covered -- the row here, and the value inside `values`.
    !! The keys column is untouched, because a key is never null.
    subroutine mc_ensure_validity(self)
        class(parquet_map_column), intent(inout) :: self !! the column.
        call ensure_validity_cap(self, max(self%nrows_, capacity_rows(self)))
        self%has_nulls_ = .true.
        call self%values%ensure_validity()
    end subroutine mc_ensure_validity
    !
    !> Writes a human-readable description of the column's kind, e.g. `"map<string,int32>"`; see
    !! `parquet_container_column%kind_text`.
    !!
    !! The key type is spelled out even though it is always `string`, because that is what the
    !! Arrow and Parquet type names look like and because a later version supporting other key
    !! types would otherwise change what this answers for a column that has not changed.
    subroutine mc_kind_text(self, out)
        class(parquet_map_column), intent(in) :: self      !! the column.
        character(len=:), allocatable, intent(out) :: out  !! the description.
        character(len=:), allocatable :: vk
        call nested_value_text(self%values, self%value_kind, vk)
        out = "map<string,"//vk//">"
    end subroutine mc_kind_text
    !
    !> Whether row `i` is a null map; see `parquet_container_column%is_null_row`.
    !!
    !! The three row-nullness bindings exist because `parquet_column`'s own bitmap is never
    !! allocated for a container kind, so `%is_null(i)` on the column would otherwise answer
    !! `.false.` for a row that really is null -- and `%set_null(i)` would write a bit nothing
    !! reads. Each forwards to this type's own public form, which does the bounds check.
    pure function mc_is_null_row(self, i) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i               !! 1-based row index.
        logical :: res                                !! whether the row is null.
        res = .false.
        if (i < 1_int64 .or. i > self%nrows_) return
        res = row_is_null(self, i)
    end function mc_is_null_row
    !
    !> Marks row `i` a null map; see `parquet_container_column%set_null_row`.
    subroutine mc_set_null_row(self, i)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index.
        call self%set_null_i64(i)
    end subroutine mc_set_null_row
    !
    !> Marks row `i` present again; see `parquet_container_column%clear_null_row`.
    subroutine mc_clear_null_row(self, i)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index.
        call self%clear_null_i64(i)
    end subroutine mc_clear_null_row
    !
    ! ==================================================================================
    ! Lifecycle
    ! ==================================================================================
    !
    !> Fixes the value kind and, optionally, creates `nrows` null rows.
    !!
    !! **The value kind is required and cannot change afterwards**, for the same reason
    !! `parquet_list_column%init` requires its payload kind: inferring it from the first
    !! `%append_row` would type the column from one element and would leave `%element_kind()`
    !! unable to answer for an empty column.
    !!
    !! There is no key-kind argument: v1 keys are always `string`.
    subroutine init(self, value_kind, nrows, unit)
        class(parquet_map_column), intent(inout) :: self   !! the column.
        integer, intent(in) :: value_kind                  !! PK_* kind of the values (required).
        integer(int64), intent(in), optional :: nrows      !! null rows to create (default 0).
        character(len=*), intent(in), optional :: unit     !! unit string for the values.
        integer(int64) :: n
        character(len=:), allocatable :: kname
        call self%clear()
        if (.not. is_supported_value(value_kind)) then
            call parquet_kind_name(value_kind, kname)
            if (parquet_kind_is_container(value_kind)) then
                ! A container value is nesting, and %init cannot express it: it is handed one PK_*
                ! discriminator, while a nested value is a kind PLUS an inner schema. Build the inner
                ! container, hand it to a parquet_column with %adopt_container, and pass that column
                ! to %adopt_rows. See feature_container_phase7.md's D1.
                error stop EP//"init: "//kname//" is a nested value and cannot be declared here; "// &
                    "build the inner container, hand it to a parquet_column with %adopt_container, "// &
                    "and pass that column to %adopt_rows"
            end if
            error stop EP//"init: "//kname//" is not a supported map value kind"
        end if
        n = 0_int64
        if (present(nrows)) n = nrows
        if (n < 0_int64) error stop EP//"init: negative row count"
        self%value_kind = value_kind
        ! Both entry columns start EMPTY whatever `nrows` says: the rows being created are null,
        ! and a null row holds no entries.
        call self%keys%init(PK_STRING, 0_int64)
        if (present(unit)) then
            call self%values%init(value_kind, 0_int64, 1_int32, unit)
        else
            call self%values%init(value_kind, 0_int64)
        end if
        call ensure_offsets_cap(self, max(n, MIN_ROW_CAP))
        if (n > 0_int64) call self%grow_rows(n)
    end subroutine init
    !
    !> Releases every buffer and resets to an uninitialized column with no value kind.
    subroutine clear(self)
        class(parquet_map_column), intent(inout) :: self !! the column.
        self%nrows_ = 0_int64
        self%value_kind = PK_NONE
        self%has_nulls_ = .false.
        if (allocated(self%offsets)) deallocate(self%offsets)
        if (allocated(self%validity)) deallocate(self%validity)
        call self%keys%clear()
        call self%values%clear()
    end subroutine clear
    !
    !> Produces a fully independent copy: offsets, row validity, the keys and the values with
    !! their own per-value validity and unit.
    subroutine deep_copy(self, out)
        class(parquet_map_column), intent(in) :: self  !! the source column.
        type(parquet_map_column), intent(out) :: out   !! receives the copy.
        integer(int64) :: n
        call out%clear()
        out%value_kind = self%value_kind
        out%nrows_ = self%nrows_
        out%has_nulls_ = self%has_nulls_
        if (allocated(self%offsets)) then
            ! Exact-fit, like every other rebuild here: the copy inherits the rows, not the slack.
            n = self%nrows_ + 1_int64
            allocate(out%offsets(n))
            out%offsets(1:n) = self%offsets(1:n)
        end if
        if (allocated(self%validity)) then
            allocate(out%validity(size(self%validity, kind=int64)))
            out%validity = self%validity
        end if
        call self%keys%deep_copy(out%keys)
        call self%values%deep_copy(out%values)
    end subroutine deep_copy
    !
    !> Takes ownership of a whole column at once: `offsets`, `keys` and `values` are all MOVED in
    !! (each comes back deallocated/empty), `row_valid` marks which rows are present, and
    !! everything else -- the value kind, the row count -- is derived from what it was given.
    !!
    !! The bulk counterpart of `%append_row`, and what the file reader is built on: a read knows
    !! its final row and entry counts before it has a single value, so appending row by row would
    !! mean re-deriving what it already knows, once per row.
    !!
    !! Replaces whatever this column held (it `%clear()`s first), so it is a build, not an append.
    !!
    !! Preconditions, all checked and all fatal, because every one of them would otherwise show
    !! up later as a wrong answer rather than as an error here:
    !!
    !! * `keys` must be a `PK_STRING` column of width 1.
    !! * `values` must have a kind this type accepts, and width 1.
    !! * `keys` and `values` must hold the same number of entries.
    !! * `offsets` must be allocated with at least one entry; `size(offsets) - 1` is the row
    !!   count, so a single entry builds a legitimate empty column.
    !! * `offsets(1)` must be 0 and the sequence must be non-decreasing.
    !! * `offsets(nrows+1)` must equal the entry count.
    !! * `row_valid`, if present, must have exactly `nrows` entries.
    !!
    !! Per-VALUE nullness travels inside `values`, so it needs no argument here; `row_valid` is
    !! the separate ROW level. A key is never null, so `keys` carries no null level at all.
    subroutine adopt_rows(self, offsets, keys, values, row_valid)
        class(parquet_map_column), intent(inout) :: self         !! the column being built.
        integer(int64), allocatable, intent(inout) :: offsets(:) !! nrows+1 offsets, moved in.
        type(parquet_column), intent(inout) :: keys              !! the flattened keys, moved in.
        type(parquet_column), intent(inout) :: values            !! the flattened values, moved in.
        logical, intent(in), optional :: row_valid(:)            !! per-ROW validity; absent = all present.
        integer(int64) :: n, i
        character(len=:), allocatable :: kname
        if (.not. allocated(offsets)) error stop EP//"adopt_rows: offsets is not allocated"
        n = size(offsets, kind=int64) - 1_int64
        if (n < 0_int64) error stop EP//"adopt_rows: offsets must hold at least one entry"
        if (keys%kindof() /= PK_STRING) then
            call parquet_kind_name(keys%kindof(), kname)
            error stop EP//"adopt_rows: map keys must be a string column, got "//kname
        end if
        if (.not. is_adoptable_value(values%kindof())) then
            call parquet_kind_name(values%kindof(), kname)
            error stop EP//"adopt_rows: "//kname//" is not a supported map value kind"
        end if
        ! **Unreachable, and kept as the belt to the two kind checks' braces.** Every kind that
        ! gets past them carries width 1 by construction: `%init` refuses a `width=` on a scalar
        ! kind, `%adopt_container` and `%adopt_string_column` both settle it at 1, and the only
        ! kinds that can hold a wider one are the `*_VEC` family -- which `is_adoptable_value` has
        ! already refused, and which a keys column could not be anyway. It stays because those
        ! facts are established in another module and a change there must not silently reach the
        ! offsets. `parquet_list_column%adopt_rows` carries the same guard for the same reason.
        if (keys%colwidth() /= 1_int32 .or. values%colwidth() /= 1_int32) then
            error stop EP//"adopt_rows: keys and values must be scalar (width 1) columns" ! GCOVR_EXCL_LINE
        end if
        if (keys%length() /= values%length()) then
            error stop EP//"adopt_rows: keys and values hold different entry counts"
        end if
        if (offsets(1) /= 0_int64) error stop EP//"adopt_rows: offsets(1) must be 0"
        do i = 1_int64, n
            if (offsets(i + 1_int64) < offsets(i)) error stop EP//"adopt_rows: offsets are not monotonic"
        end do
        if (offsets(n + 1_int64) /= values%length()) then
            error stop EP//"adopt_rows: the final offset does not match the entry count"
        end if
        if (present(row_valid)) then
            if (size(row_valid, kind=int64) /= n) then
                error stop EP//"adopt_rows: row_valid has a different length from the row count"
            end if
        end if
        call self%clear()
        self%value_kind = values%kindof()
        self%nrows_ = n
        call move_alloc(offsets, self%offsets)
        call self%keys%move_from(keys)
        call self%values%move_from(values)
        if (.not. present(row_valid)) return
        ! The bitmap stays LAZY: a column with no null row allocates nothing, exactly as one built
        ! by %append_row does. That is what keeps %has_validity_storage meaningful for a column
        ! that arrived this way.
        if (.not. any(.not. row_valid)) return
        call ensure_validity_cap(self, n)
        self%has_nulls_ = .true.
        do i = 1_int64, n
            if (.not. row_valid(i)) call bit_set(self%validity, i)
        end do
    end subroutine adopt_rows
    !
    !> Takes over `src`'s storage without copying it, leaving `src` empty and uninitialized.
    subroutine move_from(self, src)
        class(parquet_map_column), intent(inout) :: self !! the column receiving the storage.
        type(parquet_map_column), intent(inout) :: src   !! the column giving it up; left empty.
        call self%clear()
        self%nrows_ = src%nrows_
        self%value_kind = src%value_kind
        self%has_nulls_ = src%has_nulls_
        if (allocated(src%offsets)) call move_alloc(src%offsets, self%offsets)
        if (allocated(src%validity)) call move_alloc(src%validity, self%validity)
        call self%keys%move_from(src%keys)
        call self%values%move_from(src%values)
        ! `src` gave up its buffers but would otherwise still claim a kind and a row count, which
        ! would describe storage that is no longer there.
        call src%clear()
    end subroutine move_from
    !
    ! ==================================================================================
    ! Queries
    ! ==================================================================================
    !
    !> Number of rows stored (the public alias of the deferred `%nrows()`).
    pure function mc_nrows_public(self) result(n)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64) :: n                           !! rows stored.
        n = self%nrows_
    end function mc_nrows_public
    !
    !> Whether `%init` has fixed a value kind.
    pure function is_init(self) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        logical :: res                                !! .true. once %init has run.
        res = self%value_kind /= PK_NONE
    end function is_init
    !
    !> The values' `PK_*` kind, or `PK_NONE` before `%init`.
    !!
    !! Named `element_kind` rather than `value_kind` to match `parquet_list_column`: a caller
    !! writing generic code over both container types asks the same question the same way.
    pure function element_kind(self) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer :: res                                !! the values' PK_* discriminator.
        res = self%value_kind
    end function element_kind
    !
    !> Entries stored across every row.
    !!
    !! Bounded by `huge(1_int32)` in practice -- Arrow addresses a map's entries with an int32
    !! offsets buffer and has no `large_map` -- but reported as `integer(int64)` for symmetry
    !! with `parquet_list_column%total_elements` and so that a sum cannot overflow while it is
    !! being formed.
    pure function total_entries(self) result(n)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64) :: n                           !! entries across every row.
        n = 0_int64
        if (allocated(self%offsets) .and. self%nrows_ > 0_int64) n = self%offsets(self%nrows_ + 1_int64)
    end function total_entries
    !
    !> Number of null (absent) rows.
    pure function null_count(self) result(n)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64) :: n                           !! null rows.
        integer(int64) :: i
        n = 0_int64
        if (.not. self%has_nulls_) return
        if (.not. allocated(self%validity)) return
        do i = 1_int64, self%nrows_
            if (bit_test(self%validity, i)) n = n + 1_int64
        end do
    end function null_count
    !
    !> Rows the offsets allocation covers, which is at least `%size()`.
    pure function capacity(self) result(n)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64) :: n                           !! row capacity.
        n = capacity_rows(self)
    end function capacity
    !
    !> Whether the row bitmap is materialized, i.e. whether nulling a row would still allocate.
    pure function has_validity_storage(self) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        logical :: res                                !! .true. when the bitmap exists.
        res = allocated(self%validity)
    end function has_validity_storage
    !
    !> int32 specific of length; see the length generic.
    function length_i32(self, i) result(n)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i               !! 1-based row index.
        integer(int64) :: n                           !! entries in row i.
        n = self%length_i64(int(i, int64))
    end function length_i32
    !
    !> int64 specific of length: entries in row `i`, and 0 for a null row.
    function length_i64(self, i) result(n)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i               !! 1-based row index.
        integer(int64) :: n                           !! entries in row i.
        call check_row(self, i, "length")
        n = 0_int64
        if (row_is_null(self, i)) return
        n = self%offsets(i + 1_int64) - self%offsets(i)
    end function length_i64
    !
    !> int32 specific of is_null; see the is_null generic.
    function is_null_i32(self, i) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i               !! 1-based row index.
        logical :: res                                !! whether row i is a null map.
        res = self%is_null_i64(int(i, int64))
    end function is_null_i32
    !
    !> int64 specific of is_null: whether row `i` is a null (absent) map, as opposed to a present
    !! one holding no entries.
    function is_null_i64(self, i) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i               !! 1-based row index.
        logical :: res                                !! whether row i is a null map.
        call check_row(self, i, "is_null")
        res = row_is_null(self, i)
    end function is_null_i64
    !
    !> int32 specific of is_empty; see the is_empty generic.
    function is_empty_i32(self, i) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i               !! 1-based row index.
        logical :: res                                !! whether row i holds no entries.
        res = self%is_empty_i64(int(i, int64))
    end function is_empty_i32
    !
    !> int64 specific of is_empty: whether row `i` holds no entries.
    !!
    !! `.true.` for a NULL row too, matching `parquet_strings`' own handle model: the two states
    !! are not independently queryable without asking `%is_null(i)` first.
    function is_empty_i64(self, i) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i               !! 1-based row index.
        logical :: res                                !! whether row i holds no entries.
        res = self%length_i64(i) == 0_int64
    end function is_empty_i64
    !
    !> int32 specific of view; see the view generic.
    function view_i32(self, i) result(h)
        class(parquet_map_column), intent(in), target :: self !! the column; must be a TARGET.
        integer(int32), intent(in) :: i                       !! 1-based row index.
        type(parquet_map_row) :: h                            !! handle to row i.
        h = self%view_i64(int(i, int64))
    end function view_i32
    !
    !> int64 specific of view: a zero-copy handle to row `i`.
    !!
    !! **`self` must have the `TARGET` attribute at the call site.** F2018 15.5.2.4 leaves the
    !! pointer this stores undefined on return otherwise, and nothing diagnoses it on gfortran,
    !! ifx or flang -- nagfor's `-C=dangling` does.
    function view_i64(self, i) result(h)
        class(parquet_map_column), intent(in), target :: self !! the column; must be a TARGET.
        integer(int64), intent(in) :: i                       !! 1-based row index.
        type(parquet_map_row) :: h                            !! handle to row i.
        call check_row(self, i, "view")
        h%col => self
        h%idx = i
    end function view_i64
    !
    !> Verifies the class invariants, for tests and for debugging a column built by hand.
    !!
    !! **Every failure arm below is excluded from coverage, and the success arms are not.** This
    !! procedure IS the self-check: each arm names an invariant that the public API cannot break,
    !! so reaching one means a component was written past the type's own bindings -- which no test
    !! can do from outside the module, and which is exactly the state this exists to catch if a
    !! future change ever introduces it. Excluding them keeps that safety net from reading as a
    !! coverage gap. The two `ok = .true.` returns are reachable and deliberately left counted.
    !! `parquet_struct_column%validate` carries the same note for the same reason.
    function validate(self, message) result(ok)
        class(parquet_map_column), intent(in) :: self                   !! the column.
        character(len=:), allocatable, intent(out), optional :: message !! diagnostic on failure.
        logical :: ok                                                   !! .true. when every invariant holds.
        integer(int64) :: i
        ok = .false.
        if (present(message)) message = ""
        if (self%nrows_ < 0_int64) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "negative row count"
            return
            ! GCOVR_EXCL_STOP
        end if
        if (self%value_kind == PK_NONE) then
            if (self%nrows_ /= 0_int64) then
                ! GCOVR_EXCL_START -- unreachable; see the note above.
                if (present(message)) message = "rows stored in a column with no value kind"
                return
                ! GCOVR_EXCL_STOP
            end if
            ok = .true.
            return
        end if
        if (.not. allocated(self%offsets)) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "value kind is set but offsets are not allocated"
            return
            ! GCOVR_EXCL_STOP
        end if
        if (size(self%offsets, kind=int64) < self%nrows_ + 1_int64) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "offsets shorter than nrows+1"
            return
            ! GCOVR_EXCL_STOP
        end if
        if (self%offsets(1) /= 0_int64) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "offsets(1) is not 0"
            return
            ! GCOVR_EXCL_STOP
        end if
        do i = 1_int64, self%nrows_
            if (self%offsets(i + 1_int64) < self%offsets(i)) then
                ! GCOVR_EXCL_START -- unreachable; see the note above.
                if (present(message)) message = "offsets are not monotonic"
                return
                ! GCOVR_EXCL_STOP
            end if
        end do
        if (self%keys%length() /= self%values%length()) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "keys and values hold different entry counts"
            return
            ! GCOVR_EXCL_STOP
        end if
        if (self%offsets(self%nrows_ + 1_int64) /= self%values%length()) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "final offset does not match the entry count"
            return
            ! GCOVR_EXCL_STOP
        end if
        if (self%keys%kindof() /= PK_STRING) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "the keys column is not a string column"
            return
            ! GCOVR_EXCL_STOP
        end if
        if (self%values%kindof() /= self%value_kind) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "values kind does not match element_kind"
            return
            ! GCOVR_EXCL_STOP
        end if
        if (self%has_nulls_ .and. .not. allocated(self%validity)) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "has_nulls is set but the bitmap is not allocated"
            return
            ! GCOVR_EXCL_STOP
        end if
        ok = .true.
    end function validate
    !
    !> Writes a one-line description, e.g. `"map<string,int32>: 3 rows, 4 entries, 1 null"`.
    !!
    !! Composes a string rather than writing to a unit deliberately: the caller decides where it
    !! goes, and this module's only emit channel is the one its soft-fail warnings use.
    subroutine summary(self, out)
        class(parquet_map_column), intent(in) :: self     !! the column.
        character(len=:), allocatable, intent(out) :: out !! the description.
        character(len=:), allocatable :: kt, nr, ne, nn
        call self%kind_text(kt)
        call i2s(self%nrows_, nr)
        call i2s(self%total_entries(), ne)
        call i2s(self%null_count(), nn)
        out = kt//": "//nr//" rows, "//ne//" entries, "//nn//" null"
    end subroutine summary
    !
    ! ==================================================================================
    ! Capacity
    ! ==================================================================================
    !
    !> int32 specific of reserve; see the reserve generic.
    subroutine reserve_i32(self, n)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: n                  !! rows to reserve for.
        call self%reserve_i64(int(n, int64))
    end subroutine reserve_i32
    !
    !> int64 specific of reserve: grows capacity to hold at least `n` rows without adding any.
    subroutine reserve_i64(self, n)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                  !! rows to reserve for.
        call self%reserve_rows(n)
    end subroutine reserve_i64
    !
    !> Releases capacity beyond the rows stored, in the offsets and in both entry columns.
    !!
    !! A request, not an assertion: a column that has never been appended to has no slack to
    !! release and this does nothing.
    subroutine shrink_to_fit(self)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64) :: need
        integer(int64), allocatable :: tmp(:)
        if (allocated(self%offsets)) then
            need = self%nrows_ + 1_int64
            if (size(self%offsets, kind=int64) > need) then
                allocate(tmp(need))
                tmp(1:need) = self%offsets(1:need)
                call move_alloc(tmp, self%offsets)
            end if
        end if
        call self%keys%shrink_to_fit()
        call self%values%shrink_to_fit()
    end subroutine shrink_to_fit
    !
    ! ==================================================================================
    ! Mutation
    ! ==================================================================================
    !
    ! The nine `append_row` specifics are deliberately identical in shape: guard, append the keys,
    ! append the values, close the row. Everything that is not the value type lives in
    ! `check_append` and `close_row`, so a tenth value kind is four lines and a generic entry.
    !
    !> `append_row` specific for int32 values: appends one row of `keys(k) -> values(k)`.
    subroutine append_row_i32(self, keys, values, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: keys(:)          !! the row's keys, in order.
        integer(int32), intent(in) :: values(:)          !! the row's values, index-aligned with keys.
        logical, intent(in), optional :: is_valid(:)     !! per-VALUE validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_INT32, keys, k, is_valid)
        base = self%values%length()
        call self%keys%append_values(keys)
        call self%values%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_i32
    !
    !> `append_row` specific for int64 values: appends one row of `keys(k) -> values(k)`.
    subroutine append_row_i64(self, keys, values, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: keys(:)          !! the row's keys, in order.
        integer(int64), intent(in) :: values(:)          !! the row's values, index-aligned with keys.
        logical, intent(in), optional :: is_valid(:)     !! per-VALUE validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_INT64, keys, k, is_valid)
        base = self%values%length()
        call self%keys%append_values(keys)
        call self%values%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_i64
    !
    !> `append_row` specific for float32 values: appends one row of `keys(k) -> values(k)`.
    subroutine append_row_f32(self, keys, values, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: keys(:)          !! the row's keys, in order.
        real(real32), intent(in) :: values(:)            !! the row's values, index-aligned with keys.
        logical, intent(in), optional :: is_valid(:)     !! per-VALUE validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_FLOAT32, keys, k, is_valid)
        base = self%values%length()
        call self%keys%append_values(keys)
        call self%values%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_f32
    !
    !> `append_row` specific for float64 values: appends one row of `keys(k) -> values(k)`.
    subroutine append_row_f64(self, keys, values, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: keys(:)          !! the row's keys, in order.
        real(real64), intent(in) :: values(:)            !! the row's values, index-aligned with keys.
        logical, intent(in), optional :: is_valid(:)     !! per-VALUE validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_FLOAT64, keys, k, is_valid)
        base = self%values%length()
        call self%keys%append_values(keys)
        call self%values%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_f64
    !
    !> `append_row` specific for logical values: appends one row of `keys(k) -> values(k)`.
    subroutine append_row_bool(self, keys, values, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: keys(:)          !! the row's keys, in order.
        logical, intent(in) :: values(:)                 !! the row's values, index-aligned with keys.
        logical, intent(in), optional :: is_valid(:)     !! per-VALUE validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_LOGICAL, keys, k, is_valid)
        base = self%values%length()
        call self%keys%append_values(keys)
        call self%values%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_bool
    !
    !> `append_row` specific for string values: appends one row of `keys(k) -> values(k)`.
    !!
    !! **Trailing blanks are trimmed from both the keys and the values**, as they are everywhere a
    !! `character` ARRAY enters a column: every element of a `character(len=*)` array shares one
    !! declared length, so the padding cannot be what the caller meant. See CLAUDE.md's rule; the
    !! entry columns apply it, this specific does not add or remove it.
    subroutine append_row_str(self, keys, values, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: keys(:)          !! the row's keys, in order.
        character(len=*), intent(in) :: values(:)        !! the row's values, index-aligned with keys.
        logical, intent(in), optional :: is_valid(:)     !! per-VALUE validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_STRING, keys, k, is_valid)
        base = self%values%length()
        call self%keys%append_values(keys)
        call self%values%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_str
    !
    !> `append_row` specific for date values: appends one row of `keys(k) -> values(k)`.
    subroutine append_row_date(self, keys, values, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: keys(:)          !! the row's keys, in order.
        type(parquet_date), intent(in) :: values(:)      !! the row's values, index-aligned with keys.
        logical, intent(in), optional :: is_valid(:)     !! per-VALUE validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_DATE, keys, k, is_valid)
        base = self%values%length()
        call self%keys%append_values(keys)
        call self%values%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_date
    !
    !> `append_row` specific for time values: appends one row of `keys(k) -> values(k)`.
    subroutine append_row_time(self, keys, values, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: keys(:)          !! the row's keys, in order.
        type(parquet_time), intent(in) :: values(:)      !! the row's values, index-aligned with keys.
        logical, intent(in), optional :: is_valid(:)     !! per-VALUE validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_TIME, keys, k, is_valid)
        base = self%values%length()
        call self%keys%append_values(keys)
        call self%values%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_time
    !
    !> `append_row` specific for timestamp values: appends one row of `keys(k) -> values(k)`.
    subroutine append_row_ts(self, keys, values, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: keys(:)          !! the row's keys, in order.
        type(parquet_timestamp), intent(in) :: values(:) !! the row's values, index-aligned with keys.
        logical, intent(in), optional :: is_valid(:)     !! per-VALUE validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_TIMESTAMP, keys, k, is_valid)
        base = self%values%length()
        call self%keys%append_values(keys)
        call self%values%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_ts
    !
    !> Appends one null (absent) row.
    !!
    !! The row holds no entries, so nothing is added to either entry column and the offsets simply
    !! repeat. This is what `%grow_rows` appends, and what `%init`'s `nrows` argument creates.
    subroutine append_null_row(self)
        class(parquet_map_column), intent(inout) :: self !! the column.
        call require_init(self, "append_null_row")
        call ensure_offsets_cap(self, self%nrows_ + 1_int64)
        self%offsets(self%nrows_ + 2_int64) = self%offsets(self%nrows_ + 1_int64)
        self%nrows_ = self%nrows_ + 1_int64
        call ensure_validity_cap(self, self%nrows_)
        self%has_nulls_ = .true.
        call bit_set(self%validity, self%nrows_)
    end subroutine append_null_row
    !
    !> Appends one PRESENT row holding no entries -- the empty map, distinct from the null one.
    !!
    !! Exists because the alternative spelling is a zero-sized array constructor for both the keys
    !! and the values, which a caller has to write with an explicit type-spec
    !! (`[character(len=1) ::]`) and gets wrong. `%is_null(i)` answers `.false.` for such a row
    !! and `%size()` answers 0.
    subroutine append_empty_row(self)
        class(parquet_map_column), intent(inout) :: self !! the column.
        call require_init(self, "append_empty_row")
        call ensure_offsets_cap(self, self%nrows_ + 1_int64)
        self%offsets(self%nrows_ + 2_int64) = self%offsets(self%nrows_ + 1_int64)
        self%nrows_ = self%nrows_ + 1_int64
        ! The new row is PRESENT, so the row bitmap needs nothing written: a grown bitmap is
        ! zero-filled and 0 means valid. It still has to COVER the new row, or a later %set_null
        ! on it would index past the allocation.
        if (self%has_nulls_) call ensure_validity_cap(self, self%nrows_)
    end subroutine append_empty_row
    !
    !> int32 specific of set_null; see the set_null generic.
    subroutine set_null_i32(self, i)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based row index.
        call self%set_null_i64(int(i, int64))
    end subroutine set_null_i32
    !
    !> int64 specific of set_null: marks row `i` a null (absent) map and drops its entries.
    !!
    !! O(n) in the entry columns, not O(1): the row's entries are removed and every later offset
    !! moves down by the row's former size, so that a null row really is zero-length (see the
    !! module doc for why Arrow requires it). Row indices, and therefore every outstanding row
    !! handle's index, are unaffected -- only entry positions shift.
    subroutine set_null_i64(self, i)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index.
        call check_row(self, i, "set_null")
        call ensure_validity_cap(self, self%nrows_)
        self%has_nulls_ = .true.
        call bit_set(self%validity, i)
        call drop_row_entries(self, i)
    end subroutine set_null_i64
    !
    !> Removes row `i`'s entries from both entry columns and closes the gap in `offsets`.
    !!
    !! Private, and the one place the "a null row is zero-length" invariant is enforced. `keys` and
    !! `values` are gathered over the SAME surviving index list, which is what keeps them aligned.
    !!
    !! (Coverage note: this header line never registers as "hit" in gcov even though every other
    !! line of the body does -- which is what proves the procedure runs. The same gcov attribution
    !! artifact is documented at length above `date_parse` in `src/parquet_temporal.f90`, where six
    !! headers behave this way and their bodies, `error stop` lines included, are all attributed
    !! normally. Excluded as an artifact, not as a gap.)
    subroutine drop_row_entries(self, i) ! GCOVR_EXCL_LINE -- gcov attribution artifact
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index, already bounds-checked.
        integer(int64) :: lo, hi, len, total, k, pos, e
        integer(int64), allocatable :: keep(:)

        lo = self%offsets(i) + 1_int64
        hi = self%offsets(i + 1_int64)
        len = hi - lo + 1_int64
        ! Already empty -- a row appended null, or one nulled twice. Returning here also keeps an
        ! uninitialized column out of parquet_column%gather, which aborts on a PK_NONE column.
        if (len <= 0_int64) return
        total = self%offsets(self%nrows_ + 1_int64)
        allocate(keep(max(total - len, 1_int64)))
        pos = 0_int64
        do e = 1_int64, total
            if (e < lo .or. e > hi) then
                pos = pos + 1_int64
                keep(pos) = e
            end if
        end do
        call self%keys%gather(keep(1:total - len))
        call self%values%gather(keep(1:total - len))
        do k = i + 1_int64, self%nrows_ + 1_int64
            self%offsets(k) = self%offsets(k) - len
        end do
    end subroutine drop_row_entries
    !
    !> int32 specific of clear_null; see the clear_null generic.
    subroutine clear_null_i32(self, i)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based row index.
        call self%clear_null_i64(int(i, int64))
    end subroutine clear_null_i32
    !
    !> int64 specific of clear_null: marks row `i` present again, EMPTY.
    !!
    !! Not an undo: it clears one bit. `%set_null` already dropped the row's entries to keep a null
    !! row zero-length (module doc), so the row comes back with no entries whatever it held before.
    !! Only `%append_row` gives a row entries.
    subroutine clear_null_i64(self, i)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index.
        call check_row(self, i, "clear_null")
        if (.not. allocated(self%validity)) return
        call bit_clear(self%validity, i)
    end subroutine clear_null_i64
    !
    ! ==================================================================================
    ! parquet_map_row -- the non-owning row handle
    ! ==================================================================================
    !
    !> Whether this handle refers to a live row.
    !!
    !! .false. for a default-constructed handle, which is what a caller gets from an array of
    !! handles it has not filled yet. Every other accessor here aborts on such a handle rather
    !! than returning a plausible answer.
    pure function pmr_is_valid(self) result(res)
        class(parquet_map_row), intent(in) :: self !! the handle.
        logical :: res                             !! .true. when the handle refers to a row.
        res = .false.
        if (.not. associated(self%col)) return
        res = self%idx >= 1_int64 .and. self%idx <= self%col%nrows_
    end function pmr_is_valid
    !
    !> The 1-based row this handle refers to.
    !!
    !! Guarded like every other accessor on this handle, and deliberately NOT `pure`: a pure
    !! procedure may not `error stop`, so a pure form could only answer 0 for a dead handle --
    !! exactly the plausible-looking wrong answer `check_handle` exists to prevent. Matches
    !! `parquet_struct_row%row_index`, which has always been guarded.
    function pmr_row_index(self) result(i)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer(int64) :: i                        !! the row index.
        call check_handle(self, "row_index")
        i = self%idx
    end function pmr_row_index
    !
    !> Entries in the referenced row, and 0 for a null row.
    !!
    !! `integer(int32)` rather than `integer(int64)`: Arrow addresses a map's entries with an int32
    !! offsets buffer and provides no `large_map`, so no row can hold more entries than an int32
    !! can index. That is the exemption `CLAUDE.md`'s dual-kind rule makes for a value the format
    !! already bounds, and it is what lets `%get_at`/`%key_at` take a plain `integer` position.
    function pmr_size(self) result(n)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer(int32) :: n                        !! entries in the referenced row.
        integer(int64) :: lo, cnt
        call check_handle(self, "size")
        call row_range(self%col, self%idx, lo, cnt)
        n = int(cnt, int32)
    end function pmr_size
    !
    !> Hands back the inner container behind a NESTED value, plus the entry rows this row owns.
    !!
    !! The map twin of `parquet_list_row%nested`, and it exists for the same reason: `%get`'s
    !! specifics cover the scalar and temporal value kinds and nothing else, so a
    !! `map<string, struct<...>>` row has no array shape to be copied into. The keys are unaffected
    !! and are still read with `%key_at(pos)`, whose `pos` is 1-based WITHIN the row while `lo`/`hi`
    !! index the flattened value column -- entry `pos` of this row is value row `lo + pos - 1`.
    !!
    !! `inner` comes back NULL, and `lo > hi`, when the value kind is not a container, so a caller
    !! that has not checked `%element_kind()` gets an empty loop rather than a wrong answer. The
    !! pointer is BORROWED and is invalidated by anything that rebuilds the value column.
    subroutine pmr_nested(self, inner, lo, hi)
        class(parquet_map_row), intent(in) :: self                     !! the handle.
        class(parquet_container_column), pointer, intent(out) :: inner !! the inner container, or null.
        integer(int64), intent(out) :: lo                              !! first value row of this row.
        integer(int64), intent(out) :: hi                              !! last value row; hi < lo when empty.
        call check_handle(self, "nested")
        inner => null()
        lo = 1_int64
        hi = 0_int64
        if (.not. parquet_kind_is_container(self%col%value_kind)) return
        call parquet_column_container(self%col%values, inner)
        if (self%col%is_null_row(self%idx)) return
        lo = self%col%offsets(self%idx) + 1_int64
        hi = self%col%offsets(self%idx + 1_int64)
    end subroutine pmr_nested
    !
    !> Whether the referenced row is a null (absent) map.
    function pmr_is_null(self) result(res)
        class(parquet_map_row), intent(in) :: self !! the handle.
        logical :: res                             !! .true. when the row is a null map.
        call check_handle(self, "is_null")
        res = row_is_null(self%col, self%idx)
    end function pmr_is_null
    !
    !> Whether the referenced row holds no entries; `.true.` for a null row too.
    function pmr_is_empty(self) result(res)
        class(parquet_map_row), intent(in) :: self !! the handle.
        logical :: res                             !! .true. when the row holds no entries.
        res = self%size() == 0_int32
    end function pmr_is_empty
    !
    !> The values' `PK_*` kind.
    function pmr_element_kind(self) result(res)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer :: res                             !! the values' PK_* discriminator.
        call check_handle(self, "element_kind")
        res = self%col%value_kind
    end function pmr_element_kind
    !
    !> How many entries of the referenced row carry `key`.
    !!
    !! 0 for a null row and for a key that is not present. Public precisely so that a caller can
    !! detect duplicates before choosing an `occurrence=`; this type never deduplicates.
    function pmr_key_count(self, key) result(n)
        class(parquet_map_row), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: key        !! the key to count.
        integer(int32) :: n                        !! entries carrying that key.
        integer(int64) :: lo, cnt
        call check_handle(self, "key_count")
        call row_range(self%col, self%idx, lo, cnt)
        n = count_key(self%col, lo, cnt, key)
    end function pmr_key_count
    !
    !> Whether the referenced row carries `key` at all; `.false.` for a null row.
    function pmr_contains_key(self, key) result(res)
        class(parquet_map_row), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: key        !! the key to look for.
        logical :: res                             !! whether any entry carries it.
        integer(int64) :: lo, cnt, p
        call check_handle(self, "contains_key")
        call row_range(self%col, self%idx, lo, cnt)
        call find_key(self%col, lo, cnt, key, 1_int32, p)
        res = p /= 0_int64
    end function pmr_contains_key
    !
    !> The key at 1-based position `pos` in the referenced row, in stored order.
    !!
    !! The companion of `%get_at`: together they walk a row without a key lookup, which is the
    !! shape to use when every entry is wanted rather than one. An out-of-range `pos` -- including
    !! any position at all in a null or empty row -- is a "not found" and follows the type's
    !! soft-fail convention; `key` comes back as `""` in that case and must not be used.
    subroutine pmr_key_at(self, pos, key, warn, found)
        class(parquet_map_row), intent(in) :: self        !! the handle.
        integer(int32), intent(in) :: pos                 !! 1-based position within the row.
        character(len=:), allocatable, intent(out) :: key !! receives the key at that position.
        logical, intent(in), optional :: warn             !! .true. warns instead of aborting.
        logical, intent(out), optional :: found           !! .false. when the position is out of range.
        integer(int64) :: p
        key = ""
        call locate_pos(self, "key_at", pos, warn, found, p)
        if (p == 0_int64) return
        call parquet_column_get_at(self%col%keys, p, key)
    end subroutine pmr_key_at
    !
    !> `get` specific reading an int32 value stored under `key`.
    subroutine pmr_get_i32(self, key, value, is_valid, occurrence, warn, found)
        class(parquet_map_row), intent(in) :: self      !! the handle.
        character(len=*), intent(in) :: key             !! the key to look up.
        integer(int32), intent(out) :: value            !! receives the value.
        logical, intent(out), optional :: is_valid      !! .false. when the entry's value is null.
        integer(int32), intent(in), optional :: occurrence !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn           !! .true. warns instead of aborting.
        logical, intent(out), optional :: found         !! .false. when the key is not present.
        integer(int32), pointer :: p(:)
        integer(int64) :: e
        value = 0_int32
        if (present(is_valid)) is_valid = .false.
        call locate_key(self, PK_INT32, "get", key, occurrence, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_get_i32
    !
    !> `get` specific reading an int64 value stored under `key`.
    subroutine pmr_get_i64(self, key, value, is_valid, occurrence, warn, found)
        class(parquet_map_row), intent(in) :: self      !! the handle.
        character(len=*), intent(in) :: key             !! the key to look up.
        integer(int64), intent(out) :: value            !! receives the value.
        logical, intent(out), optional :: is_valid      !! .false. when the entry's value is null.
        integer(int32), intent(in), optional :: occurrence !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn           !! .true. warns instead of aborting.
        logical, intent(out), optional :: found         !! .false. when the key is not present.
        integer(int64), pointer :: p(:)
        integer(int64) :: e
        value = 0_int64
        if (present(is_valid)) is_valid = .false.
        call locate_key(self, PK_INT64, "get", key, occurrence, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_get_i64
    !
    !> `get` specific reading a float32 value stored under `key`.
    subroutine pmr_get_f32(self, key, value, is_valid, occurrence, warn, found)
        class(parquet_map_row), intent(in) :: self      !! the handle.
        character(len=*), intent(in) :: key             !! the key to look up.
        real(real32), intent(out) :: value              !! receives the value.
        logical, intent(out), optional :: is_valid      !! .false. when the entry's value is null.
        integer(int32), intent(in), optional :: occurrence !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn           !! .true. warns instead of aborting.
        logical, intent(out), optional :: found         !! .false. when the key is not present.
        real(real32), pointer :: p(:)
        integer(int64) :: e
        value = 0.0_real32
        if (present(is_valid)) is_valid = .false.
        call locate_key(self, PK_FLOAT32, "get", key, occurrence, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_get_f32
    !
    !> `get` specific reading a float64 value stored under `key`.
    subroutine pmr_get_f64(self, key, value, is_valid, occurrence, warn, found)
        class(parquet_map_row), intent(in) :: self      !! the handle.
        character(len=*), intent(in) :: key             !! the key to look up.
        real(real64), intent(out) :: value              !! receives the value.
        logical, intent(out), optional :: is_valid      !! .false. when the entry's value is null.
        integer(int32), intent(in), optional :: occurrence !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn           !! .true. warns instead of aborting.
        logical, intent(out), optional :: found         !! .false. when the key is not present.
        real(real64), pointer :: p(:)
        integer(int64) :: e
        value = 0.0_real64
        if (present(is_valid)) is_valid = .false.
        call locate_key(self, PK_FLOAT64, "get", key, occurrence, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_get_f64
    !
    !> `get` specific reading a logical value stored under `key`.
    subroutine pmr_get_bool(self, key, value, is_valid, occurrence, warn, found)
        class(parquet_map_row), intent(in) :: self      !! the handle.
        character(len=*), intent(in) :: key             !! the key to look up.
        logical, intent(out) :: value                   !! receives the value.
        logical, intent(out), optional :: is_valid      !! .false. when the entry's value is null.
        integer(int32), intent(in), optional :: occurrence !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn           !! .true. warns instead of aborting.
        logical, intent(out), optional :: found         !! .false. when the key is not present.
        logical, pointer :: p(:)
        integer(int64) :: e
        value = .false.
        if (present(is_valid)) is_valid = .false.
        call locate_key(self, PK_LOGICAL, "get", key, occurrence, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_get_bool
    !
    !> `get` specific reading a string value stored under `key`.
    !!
    !! `value` comes back sized to the stored string. A failed lookup and a null value both yield
    !! `""` -- an allocated, zero-length string rather than an unallocated one, because an
    !! unallocated result is indistinguishable from one the callee never reached.
    subroutine pmr_get_str(self, key, value, is_valid, occurrence, warn, found)
        class(parquet_map_row), intent(in) :: self          !! the handle.
        character(len=*), intent(in) :: key                 !! the key to look up.
        character(len=:), allocatable, intent(out) :: value !! receives the value.
        logical, intent(out), optional :: is_valid          !! .false. when the entry's value is null.
        integer(int32), intent(in), optional :: occurrence  !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn               !! .true. warns instead of aborting.
        logical, intent(out), optional :: found             !! .false. when the key is not present.
        integer(int64) :: e
        value = ""
        if (present(is_valid)) is_valid = .false.
        call locate_key(self, PK_STRING, "get", key, occurrence, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_get_at(self%col%values, e, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_get_str
    !
    !> `get` specific reading a date value stored under `key`.
    subroutine pmr_get_date(self, key, value, is_valid, occurrence, warn, found)
        class(parquet_map_row), intent(in) :: self      !! the handle.
        character(len=*), intent(in) :: key             !! the key to look up.
        type(parquet_date), intent(out) :: value        !! receives the value.
        logical, intent(out), optional :: is_valid      !! .false. when the entry's value is null.
        integer(int32), intent(in), optional :: occurrence !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn           !! .true. warns instead of aborting.
        logical, intent(out), optional :: found         !! .false. when the key is not present.
        type(parquet_date), pointer :: p(:)
        integer(int64) :: e
        if (present(is_valid)) is_valid = .false.
        call locate_key(self, PK_DATE, "get", key, occurrence, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_get_date
    !
    !> `get` specific reading a time value stored under `key`.
    subroutine pmr_get_time(self, key, value, is_valid, occurrence, warn, found)
        class(parquet_map_row), intent(in) :: self      !! the handle.
        character(len=*), intent(in) :: key             !! the key to look up.
        type(parquet_time), intent(out) :: value        !! receives the value.
        logical, intent(out), optional :: is_valid      !! .false. when the entry's value is null.
        integer(int32), intent(in), optional :: occurrence !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn           !! .true. warns instead of aborting.
        logical, intent(out), optional :: found         !! .false. when the key is not present.
        type(parquet_time), pointer :: p(:)
        integer(int64) :: e
        if (present(is_valid)) is_valid = .false.
        call locate_key(self, PK_TIME, "get", key, occurrence, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_get_time
    !
    !> `get` specific reading a timestamp value stored under `key`.
    subroutine pmr_get_ts(self, key, value, is_valid, occurrence, warn, found)
        class(parquet_map_row), intent(in) :: self      !! the handle.
        character(len=*), intent(in) :: key             !! the key to look up.
        type(parquet_timestamp), intent(out) :: value   !! receives the value.
        logical, intent(out), optional :: is_valid      !! .false. when the entry's value is null.
        integer(int32), intent(in), optional :: occurrence !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn           !! .true. warns instead of aborting.
        logical, intent(out), optional :: found         !! .false. when the key is not present.
        type(parquet_timestamp), pointer :: p(:)
        integer(int64) :: e
        if (present(is_valid)) is_valid = .false.
        call locate_key(self, PK_TIMESTAMP, "get", key, occurrence, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_get_ts
    !
    !> `get_at` specific reading the int32 value at position `pos`.
    subroutine pmr_at_i32(self, pos, value, is_valid, warn, found)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer(int32), intent(in) :: pos          !! 1-based position within the row.
        integer(int32), intent(out) :: value       !! receives the value.
        logical, intent(out), optional :: is_valid !! .false. when the entry's value is null.
        logical, intent(in), optional :: warn      !! .true. warns instead of aborting.
        logical, intent(out), optional :: found    !! .false. when the position is out of range.
        integer(int32), pointer :: p(:)
        integer(int64) :: e
        value = 0_int32
        if (present(is_valid)) is_valid = .false.
        call locate_at(self, PK_INT32, "get_at", pos, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_at_i32
    !
    !> `get_at` specific reading the int64 value at position `pos`.
    subroutine pmr_at_i64(self, pos, value, is_valid, warn, found)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer(int32), intent(in) :: pos          !! 1-based position within the row.
        integer(int64), intent(out) :: value       !! receives the value.
        logical, intent(out), optional :: is_valid !! .false. when the entry's value is null.
        logical, intent(in), optional :: warn      !! .true. warns instead of aborting.
        logical, intent(out), optional :: found    !! .false. when the position is out of range.
        integer(int64), pointer :: p(:)
        integer(int64) :: e
        value = 0_int64
        if (present(is_valid)) is_valid = .false.
        call locate_at(self, PK_INT64, "get_at", pos, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_at_i64
    !
    !> `get_at` specific reading the float32 value at position `pos`.
    subroutine pmr_at_f32(self, pos, value, is_valid, warn, found)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer(int32), intent(in) :: pos          !! 1-based position within the row.
        real(real32), intent(out) :: value         !! receives the value.
        logical, intent(out), optional :: is_valid !! .false. when the entry's value is null.
        logical, intent(in), optional :: warn      !! .true. warns instead of aborting.
        logical, intent(out), optional :: found    !! .false. when the position is out of range.
        real(real32), pointer :: p(:)
        integer(int64) :: e
        value = 0.0_real32
        if (present(is_valid)) is_valid = .false.
        call locate_at(self, PK_FLOAT32, "get_at", pos, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_at_f32
    !
    !> `get_at` specific reading the float64 value at position `pos`.
    subroutine pmr_at_f64(self, pos, value, is_valid, warn, found)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer(int32), intent(in) :: pos          !! 1-based position within the row.
        real(real64), intent(out) :: value         !! receives the value.
        logical, intent(out), optional :: is_valid !! .false. when the entry's value is null.
        logical, intent(in), optional :: warn      !! .true. warns instead of aborting.
        logical, intent(out), optional :: found    !! .false. when the position is out of range.
        real(real64), pointer :: p(:)
        integer(int64) :: e
        value = 0.0_real64
        if (present(is_valid)) is_valid = .false.
        call locate_at(self, PK_FLOAT64, "get_at", pos, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_at_f64
    !
    !> `get_at` specific reading the logical value at position `pos`.
    subroutine pmr_at_bool(self, pos, value, is_valid, warn, found)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer(int32), intent(in) :: pos          !! 1-based position within the row.
        logical, intent(out) :: value              !! receives the value.
        logical, intent(out), optional :: is_valid !! .false. when the entry's value is null.
        logical, intent(in), optional :: warn      !! .true. warns instead of aborting.
        logical, intent(out), optional :: found    !! .false. when the position is out of range.
        logical, pointer :: p(:)
        integer(int64) :: e
        value = .false.
        if (present(is_valid)) is_valid = .false.
        call locate_at(self, PK_LOGICAL, "get_at", pos, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_at_bool
    !
    !> `get_at` specific reading the string value at position `pos`.
    subroutine pmr_at_str(self, pos, value, is_valid, warn, found)
        class(parquet_map_row), intent(in) :: self          !! the handle.
        integer(int32), intent(in) :: pos                   !! 1-based position within the row.
        character(len=:), allocatable, intent(out) :: value !! receives the value.
        logical, intent(out), optional :: is_valid          !! .false. when the entry's value is null.
        logical, intent(in), optional :: warn               !! .true. warns instead of aborting.
        logical, intent(out), optional :: found             !! .false. when the position is out of range.
        integer(int64) :: e
        value = ""
        if (present(is_valid)) is_valid = .false.
        call locate_at(self, PK_STRING, "get_at", pos, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_get_at(self%col%values, e, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_at_str
    !
    !> `get_at` specific reading the date value at position `pos`.
    subroutine pmr_at_date(self, pos, value, is_valid, warn, found)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer(int32), intent(in) :: pos          !! 1-based position within the row.
        type(parquet_date), intent(out) :: value   !! receives the value.
        logical, intent(out), optional :: is_valid !! .false. when the entry's value is null.
        logical, intent(in), optional :: warn      !! .true. warns instead of aborting.
        logical, intent(out), optional :: found    !! .false. when the position is out of range.
        type(parquet_date), pointer :: p(:)
        integer(int64) :: e
        if (present(is_valid)) is_valid = .false.
        call locate_at(self, PK_DATE, "get_at", pos, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_at_date
    !
    !> `get_at` specific reading the time value at position `pos`.
    subroutine pmr_at_time(self, pos, value, is_valid, warn, found)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer(int32), intent(in) :: pos          !! 1-based position within the row.
        type(parquet_time), intent(out) :: value   !! receives the value.
        logical, intent(out), optional :: is_valid !! .false. when the entry's value is null.
        logical, intent(in), optional :: warn      !! .true. warns instead of aborting.
        logical, intent(out), optional :: found    !! .false. when the position is out of range.
        type(parquet_time), pointer :: p(:)
        integer(int64) :: e
        if (present(is_valid)) is_valid = .false.
        call locate_at(self, PK_TIME, "get_at", pos, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_at_time
    !
    !> `get_at` specific reading the timestamp value at position `pos`.
    subroutine pmr_at_ts(self, pos, value, is_valid, warn, found)
        class(parquet_map_row), intent(in) :: self    !! the handle.
        integer(int32), intent(in) :: pos             !! 1-based position within the row.
        type(parquet_timestamp), intent(out) :: value !! receives the value.
        logical, intent(out), optional :: is_valid    !! .false. when the entry's value is null.
        logical, intent(in), optional :: warn         !! .true. warns instead of aborting.
        logical, intent(out), optional :: found       !! .false. when the position is out of range.
        type(parquet_timestamp), pointer :: p(:)
        integer(int64) :: e
        if (present(is_valid)) is_valid = .false.
        call locate_at(self, PK_TIMESTAMP, "get_at", pos, warn, found, e)
        if (e == 0_int64) return
        if (parquet_column_is_null(self%col%values, e)) return
        call parquet_column_data_ptr(self%col%values, p)
        value = p(e)
        if (present(is_valid)) is_valid = .true.
    end subroutine pmr_at_ts
    !
    ! ==================================================================================
    ! Private helpers
    ! ==================================================================================
    !
    !> Aborts unless `%init` has fixed a value kind.
    !!
    !! (Coverage note: this header line never registers as "hit" in gcov even though every other
    !! line of the body does -- which is what proves the procedure runs. The same gcov attribution
    !! artifact is documented at length above `date_parse` in `src/parquet_temporal.f90`, where six
    !! headers behave this way and their bodies, `error stop` lines included, are all attributed
    !! normally. Excluded as an artifact, not as a gap.)
    subroutine require_init(self, proc) ! GCOVR_EXCL_LINE -- gcov attribution artifact
        class(parquet_map_column), intent(in) :: self !! the column.
        character(len=*), intent(in) :: proc          !! calling procedure, for the message.
        if (self%value_kind == PK_NONE) then
            error stop EP//proc//": this map column has no value kind; call %init first"
        end if
    end subroutine require_init
    !
    !> Aborts unless `i` names an existing row.
    subroutine check_row(self, i, proc)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i               !! 1-based row index.
        character(len=*), intent(in) :: proc          !! calling procedure, for the message.
        if (i < 1_int64 .or. i > self%nrows_) then
            error stop EP//proc//": row index is out of range"
        end if
    end subroutine check_row
    !
    !> Aborts unless the handle refers to a live row of a live column.
    subroutine check_handle(self, proc)
        class(parquet_map_row), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: proc       !! calling procedure, for the message.
        if (.not. associated(self%col)) then
            error stop EP//proc//": this row handle is not associated with a column"
        end if
        if (self%idx < 1_int64 .or. self%idx > self%col%nrows_) then
            error stop EP//proc//": this row handle no longer refers to a valid row"
        end if
    end subroutine check_handle
    !
    !> Aborts unless the column's values are the kind the calling accessor expects.
    !!
    !! ALWAYS hard, with no `warn=`/`found=` escape: a wrong value kind is a type mismatch rather
    !! than a lookup failure, and the campaign's error-handling convention gives the two different
    !! treatment on purpose. Softening this would let a caller read an int32 map with a `real64`
    !! accessor and be told only that the key was "not found".
    subroutine check_value_kind(self, want_kind, proc)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer, intent(in) :: want_kind              !! the PK_* kind the caller's variable is.
        character(len=*), intent(in) :: proc          !! calling procedure, for the message.
        character(len=:), allocatable :: have_txt, want_txt
        if (self%value_kind == want_kind) return
        call value_kind_text(self%value_kind, have_txt)
        call value_kind_text(want_kind, want_txt)
        error stop EP//proc//": this is a map<string,"//have_txt//"> column; its values cannot be "// &
            "read into "//want_txt
    end subroutine check_value_kind
    !
    !> The guard every `append_row` specific shares: initialized, right value kind, and keys,
    !! values and any mask all the same length.
    subroutine check_append(self, want_kind, keys, k, is_valid)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer, intent(in) :: want_kind              !! the PK_* kind the caller's values are.
        character(len=*), intent(in) :: keys(:)       !! the caller's keys.
        integer(int64), intent(in) :: k               !! number of values in the row.
        logical, intent(in), optional :: is_valid(:)  !! the caller's per-value mask, if any.
        character(len=:), allocatable :: have_txt, want_txt
        call require_init(self, "append_row")
        if (self%value_kind /= want_kind) then
            call value_kind_text(self%value_kind, have_txt)
            call value_kind_text(want_kind, want_txt)
            error stop EP//"append_row: this is a map<string,"//have_txt//"> column; "//want_txt// &
                " values cannot be appended to it"
        end if
        if (size(keys, kind=int64) /= k) then
            error stop EP//"append_row: keys and values have different lengths"
        end if
        if (present(is_valid)) then
            if (size(is_valid, kind=int64) /= k) then
                error stop EP//"append_row: is_valid has a different length from values"
            end if
        end if
    end subroutine check_append
    !
    !> Closes the row every `append_row` specific has just written into the entry columns:
    !! extends the offsets, advances the row count, and applies the per-VALUE validity mask.
    !!
    !! The mask is applied through the free TYPED `parquet_column_set_null`, never through a
    !! type-bound call on the values column -- this loop is per ENTRY.
    subroutine close_row(self, base, k, is_valid)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: base               !! entry count before the values were appended.
        integer(int64), intent(in) :: k                  !! number of entries appended.
        logical, intent(in), optional :: is_valid(:)     !! per-value validity, if supplied.
        integer(int64) :: e
        call ensure_offsets_cap(self, self%nrows_ + 1_int64)
        self%offsets(self%nrows_ + 2_int64) = base + k
        self%nrows_ = self%nrows_ + 1_int64
        ! The new row is PRESENT, so the row bitmap needs nothing written: a grown bitmap is
        ! zero-filled and 0 means valid. It still has to COVER the new row, or a later %set_null
        ! on it would index past the allocation.
        if (self%has_nulls_) call ensure_validity_cap(self, self%nrows_)
        if (.not. present(is_valid)) return
        do e = 1_int64, k
            if (.not. is_valid(e)) call parquet_column_set_null(self%values, base + e)
        end do
    end subroutine close_row
    !
    !> Resolves row `i` to its flat entry range. A null row -- and an index out of range, which
    !! the callers have already rejected -- resolves to a zero-length range.
    pure subroutine row_range(self, i, lo, n)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i               !! 1-based row index.
        integer(int64), intent(out) :: lo             !! 1-based first entry of the row.
        integer(int64), intent(out) :: n              !! entries in the row.
        lo = 1_int64
        n = 0_int64
        if (i < 1_int64 .or. i > self%nrows_) return
        if (row_is_null(self, i)) return
        lo = self%offsets(i) + 1_int64
        n = self%offsets(i + 1_int64) - self%offsets(i)
    end subroutine row_range
    !
    !> The flat position of the `occ`-th entry carrying `key` in the range `lo .. lo+n-1`, or 0.
    !!
    !! A LINEAR SCAN, deliberately, and not an index: `%key_count` and `occurrence=` both need the
    !! whole row walked anyway, and an index would be state whose missed invalidation is a wrong
    !! answer rather than a slow one. See the module doc.
    !!
    !! **Allocation-free.** The keys are compared through a fixed scratch slot one character
    !! LONGER than `key`, never through `%get`/`parquet_column_get_at`, which would allocate once
    !! per entry -- the shape `check_no_per_element_string_alloc` exists to keep out of `src/`.
    !! The extra character is what makes the comparison exact: a stored key longer than `key`
    !! leaves a non-blank in that position, and Fortran's own blank-padding rule settles the rest.
    subroutine find_key(self, lo, n, key, occ, p)
        type(parquet_map_column), intent(in), target :: self !! the column.
        integer(int64), intent(in) :: lo                     !! first entry of the row.
        integer(int64), intent(in) :: n                      !! entries in the row.
        character(len=*), intent(in) :: key                  !! the key to match.
        integer(int32), intent(in) :: occ                    !! which match to take (1-based).
        integer(int64), intent(out) :: p                     !! flat position, 0 when not found.
        type(parquet_string_column), pointer :: sc
        character(len=len(key)+1) :: slot
        integer(int64) :: e
        integer(int32) :: seen
        p = 0_int64
        if (n <= 0_int64) return
        call parquet_column_string_column(self%keys, sc)
        seen = 0_int32
        do e = lo, lo + n - 1_int64
            call parquet_string_column_copy_to(sc, e, slot)
            if (slot == key) then
                seen = seen + 1_int32
                if (seen == occ) then
                    p = e
                    return
                end if
            end if
        end do
    end subroutine find_key
    !
    !> How many entries of the range `lo .. lo+n-1` carry `key`. Same scan and same scratch-slot
    !! comparison as `find_key`, without the early exit.
    function count_key(self, lo, n, key) result(c)
        type(parquet_map_column), intent(in), target :: self !! the column.
        integer(int64), intent(in) :: lo                     !! first entry of the row.
        integer(int64), intent(in) :: n                      !! entries in the row.
        character(len=*), intent(in) :: key                  !! the key to count.
        integer(int32) :: c                                  !! matching entries.
        type(parquet_string_column), pointer :: sc
        character(len=len(key)+1) :: slot
        integer(int64) :: e
        c = 0_int32
        if (n <= 0_int64) return
        call parquet_column_string_column(self%keys, sc)
        do e = lo, lo + n - 1_int64
            call parquet_string_column_copy_to(sc, e, slot)
            if (slot == key) c = c + 1_int32
        end do
    end function count_key
    !
    !> The soft-fail decision every "not found" guard in this module goes through.
    !!
    !! `warn=.true.` emits one warning and returns; otherwise, if the caller supplied `found`, it
    !! is set `.false.` and the call returns silently; with NEITHER the call aborts, because
    !! otherwise the caller would have no way to learn that the lookup failed at all. That is the
    !! campaign's decided convention, in one place so that every accessor inherits it identically.
    subroutine lookup_fail(msg, warn, found)
        character(len=*), intent(in) :: msg     !! the message, without this module's prefix.
        logical, intent(in), optional :: warn   !! .true. warns instead of aborting.
        logical, intent(out), optional :: found !! set .false. here whenever it is present.
        logical :: soft
        soft = .false.
        if (present(warn)) soft = warn
        if (present(found)) found = .false.
        if (soft) then
            ! The type-qualified procedure name rather than EP: `msg` opens with the bare binding
            ! name ("get", "get_at"), which identifies nothing on its own, and a warning names the
            ! procedure the caller called. EP stays on the `error stop` below, where it is doing its
            ! own job -- see .claude/rules/api-conventions.md.
            call parquet_emit_warning("parquet_map_row%"//msg)
            return
        end if
        if (present(found)) return
        error stop EP//msg
    end subroutine lookup_fail
    !
    !> Resolves a `%get` call to the flat entry position of its key, or 0 after a soft failure.
    subroutine locate_key(self, want_kind, proc, key, occurrence, warn, found, e)
        class(parquet_map_row), intent(in) :: self         !! the handle.
        integer, intent(in) :: want_kind                   !! the PK_* kind the caller's variable is.
        character(len=*), intent(in) :: proc               !! calling procedure, for the message.
        character(len=*), intent(in) :: key                !! the key to look up.
        integer(int32), intent(in), optional :: occurrence !! which match to take (1-based, default 1).
        logical, intent(in), optional :: warn              !! .true. warns instead of aborting.
        logical, intent(out), optional :: found            !! reports whether the key was found.
        integer(int64), intent(out) :: e                   !! flat entry position, 0 when not found.
        integer(int64) :: lo, n
        integer(int32) :: occ, have
        character(len=:), allocatable :: shown, cnt
        call check_handle(self, proc)
        call check_value_kind(self%col, want_kind, proc)
        occ = 1_int32
        if (present(occurrence)) occ = occurrence
        ! An occurrence below 1 is a caller mistake rather than a failed lookup, so it is hard
        ! whatever warn=/found= say -- the same split check_value_kind makes one line above.
        if (occ < 1_int32) error stop EP//proc//": occurrence must be 1 or greater"
        if (present(found)) found = .false.
        call row_range(self%col, self%idx, lo, n)
        call find_key(self%col, lo, n, key, occ, e)
        if (e /= 0_int64) then
            if (present(found)) found = .true.
            return
        end if
        ! The message distinguishes "that key is not here" from "it is, but not that many times",
        ! because the second is what an occurrence= loop walks off the end of and the first is a
        ! misspelling. Caller-supplied text is capped: ifx's ERROR STOP runtime corrupts the heap
        ! once the composed message reaches 8192 bytes (CLAUDE.md).
        call preview(key, shown)
        have = count_key(self%col, lo, n, key)
        if (have == 0_int32) then
            call lookup_fail(proc//": no entry with key '"//shown//"' in this map row", warn, found)
        else
            call i2s(int(have, int64), cnt)
            call lookup_fail(proc//": this map row carries key '"//shown//"' only "//cnt// &
                " time(s); a later occurrence was asked for", warn, found)
        end if
    end subroutine locate_key
    !
    !> Resolves a `%get_at` call to a flat entry position, or 0 after a soft failure.
    subroutine locate_at(self, want_kind, proc, pos, warn, found, e)
        class(parquet_map_row), intent(in) :: self !! the handle.
        integer, intent(in) :: want_kind           !! the PK_* kind the caller's variable is.
        character(len=*), intent(in) :: proc       !! calling procedure, for the message.
        integer(int32), intent(in) :: pos          !! 1-based position within the row.
        logical, intent(in), optional :: warn      !! .true. warns instead of aborting.
        logical, intent(out), optional :: found    !! reports whether the position exists.
        integer(int64), intent(out) :: e           !! flat entry position, 0 when out of range.
        call check_handle(self, proc)
        call check_value_kind(self%col, want_kind, proc)
        call locate_pos(self, proc, pos, warn, found, e)
    end subroutine locate_at
    !
    !> The position half of `locate_at`, without a value-kind check -- what `%key_at` needs, since
    !! a key is a string whatever the values are.
    subroutine locate_pos(self, proc, pos, warn, found, e)
        class(parquet_map_row), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: proc       !! calling procedure, for the message.
        integer(int32), intent(in) :: pos          !! 1-based position within the row.
        logical, intent(in), optional :: warn      !! .true. warns instead of aborting.
        logical, intent(out), optional :: found    !! reports whether the position exists.
        integer(int64), intent(out) :: e           !! flat entry position, 0 when out of range.
        integer(int64) :: lo, n
        character(len=:), allocatable :: ptxt, ntxt
        call check_handle(self, proc)
        if (present(found)) found = .false.
        e = 0_int64
        call row_range(self%col, self%idx, lo, n)
        if (pos >= 1_int32 .and. int(pos, int64) <= n) then
            e = lo + int(pos, int64) - 1_int64
            if (present(found)) found = .true.
            return
        end if
        call i2s(int(pos, int64), ptxt)
        call i2s(n, ntxt)
        call lookup_fail(proc//": position "//ptxt//" is out of range; this map row holds "// &
            ntxt//" entries", warn, found)
    end subroutine locate_pos
    !
    !> Whether row `i` is null, with no bounds check -- callers have already made one.
    pure function row_is_null(self, i) result(res)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i               !! 1-based row index.
        logical :: res                                !! whether row i is null.
        res = .false.
        if (.not. self%has_nulls_) return
        if (.not. allocated(self%validity)) return
        res = bit_test(self%validity, i)
    end function row_is_null
    !
    !> Rows the offsets allocation covers.
    pure function capacity_rows(self) result(n)
        class(parquet_map_column), intent(in) :: self !! the column.
        integer(int64) :: n                           !! row capacity.
        n = 0_int64
        if (allocated(self%offsets)) n = size(self%offsets, kind=int64) - 1_int64
    end function capacity_rows
    !
    !> Ensures `offsets` is allocated with room for at least `need_rows` rows, preserving what is
    !! already stored. Growth is geometric (1.5x), so appending row by row is amortised O(1).
    subroutine ensure_offsets_cap(self, need_rows)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: need_rows          !! required row capacity.
        integer(int64) :: need, newcap
        integer(int64), allocatable :: tmp(:)
        if (need_rows < 0_int64) error stop EP//"reserve: negative row capacity"
        need = need_rows + 1_int64
        if (.not. allocated(self%offsets)) then
            allocate(self%offsets(max(need, MIN_ROW_CAP)))
            self%offsets(1) = 0_int64
        else if (size(self%offsets, kind=int64) < need) then
            newcap = size(self%offsets, kind=int64)
            newcap = max(need, newcap + newcap/2_int64)
            allocate(tmp(newcap))
            tmp(1:self%nrows_ + 1_int64) = self%offsets(1:self%nrows_ + 1_int64)
            call move_alloc(tmp, self%offsets)
        end if
    end subroutine ensure_offsets_cap
    !
    !> Ensures the row bitmap exists and covers at least `need_rows` rows, zero-filling whatever
    !! it gains so that rows it newly covers start out valid.
    subroutine ensure_validity_cap(self, need_rows)
        class(parquet_map_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: need_rows          !! rows the bitmap must cover.
        integer(int64) :: want, have
        integer(int64), allocatable :: tmp(:)
        want = max(blocks_for(need_rows), 1_int64)
        if (.not. allocated(self%validity)) then
            allocate(self%validity(want))
            self%validity = 0_int64
            return
        end if
        have = size(self%validity, kind=int64)
        if (have >= want) return
        allocate(tmp(want))
        tmp = 0_int64
        tmp(1:have) = self%validity(1:have)
        call move_alloc(tmp, self%validity)
    end subroutine ensure_validity_cap
    !
    !> Blocks needed to hold `nbits` bits.
    pure function blocks_for(nbits) result(n)
        integer(int64), intent(in) :: nbits !! number of bits.
        integer(int64) :: n                 !! blocks required.
        n = (nbits + BITS_PER_BLOCK - 1_int64)/BITS_PER_BLOCK
    end function blocks_for
    !
    !> Sets bit `i` (1-based) of a bitmap.
    pure subroutine bit_set(map, i)
        integer(int64), intent(inout) :: map(:) !! the bitmap.
        integer(int64), intent(in) :: i         !! 1-based bit index.
        integer(int64) :: blk, off
        blk = (i - 1_int64)/BITS_PER_BLOCK + 1_int64
        off = mod(i - 1_int64, BITS_PER_BLOCK)
        map(blk) = ibset(map(blk), int(off))
    end subroutine bit_set
    !
    !> Clears bit `i` (1-based) of a bitmap.
    pure subroutine bit_clear(map, i)
        integer(int64), intent(inout) :: map(:) !! the bitmap.
        integer(int64), intent(in) :: i         !! 1-based bit index.
        integer(int64) :: blk, off
        blk = (i - 1_int64)/BITS_PER_BLOCK + 1_int64
        off = mod(i - 1_int64, BITS_PER_BLOCK)
        map(blk) = ibclr(map(blk), int(off))
    end subroutine bit_clear
    !
    !> Whether bit `i` (1-based) of a bitmap is set.
    pure function bit_test(map, i) result(res)
        integer(int64), intent(in) :: map(:) !! the bitmap.
        integer(int64), intent(in) :: i      !! 1-based bit index.
        logical :: res                       !! whether the bit is set.
        integer(int64) :: blk, off
        blk = (i - 1_int64)/BITS_PER_BLOCK + 1_int64
        off = mod(i - 1_int64, BITS_PER_BLOCK)
        res = .false.
        if (blk > size(map, kind=int64)) return
        res = btest(map(blk), int(off))
    end function bit_test
    !
    !> Whether a PK_* kind may be a map column's value.
    !!
    !! The nine SCALAR value kinds. A `*_VEC` kind is excluded because a fixed-width vector as a
    !! map value is `map<string,fixed_size_list<...>>`, which is a shape this library does not read or
    !! write at any depth, and is not
    !! expressible by giving the values column a width. `PK_LIST`/`PK_MAP`/`PK_STRUCT` are excluded
    !! for the same reason -- a container value is nesting, and `%init` would have no way to be
    !! told what the inner container holds.
    pure function is_supported_value(kind) result(res)
        integer, intent(in) :: kind !! a PK_* discriminator.
        logical :: res              !! whether it may be a map value.
        select case (kind)
        case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, &
              PK_DATE, PK_TIME, PK_TIMESTAMP)
            res = .true.
        case default
            res = .false.
        end select
    end function is_supported_value
    !
    !> Whether a PK_* kind may be ADOPTED as a map column's value.
    !!
    !! Wider than `is_supported_value` by exactly the three container kinds. `%init` fixes a value
    !! KIND; a nested value is a kind plus a whole inner schema, which `%init` has no argument shape
    !! for -- so nesting is reachable only by building the inner container and handing it over with
    !! `%adopt_container` + `%adopt_values`. The `*_VEC` kinds stay refused on both paths. See
    !! feature_container_phase7.md's D1 and D2.
    pure function is_adoptable_value(kind) result(res)
        integer, intent(in) :: kind !! a PK_* discriminator.
        logical :: res              !! whether it may be adopted as a map value.
        res = is_supported_value(kind) .or. parquet_kind_is_container(kind)
    end function is_adoptable_value
    !
    !> Writes the short lowercase name of a value kind, for `kind_text` and error messages.
    !!
    !! Deliberately NOT `parquet_kind_name`, which spells the discriminator (`"PK_INT32"`). This is
    !! the type spelling a MAML schema uses, so `%kind_text()` reads `"map<string,int32>"` -- the
    !! form a user writes and the one `map[<valuetype>]` is built from.
    !!
    !! A subroutine rather than a `character(len=:), allocatable` FUNCTION, per the project-wide
    !! rule in CLAUDE.md: gfortran PR113797 makes the hidden length temporary such a function needs
    !! unreliable to be thread-local, which has already caused silent memory corruption here once.
    pure subroutine value_kind_text(kind, out)
        integer, intent(in) :: kind                       !! a PK_* discriminator.
        character(len=:), allocatable, intent(out) :: out !! the short name.
        select case (kind)
        case (PK_INT32);     out = "int32"
        case (PK_INT64);     out = "int64"
        case (PK_FLOAT32);   out = "float32"
        case (PK_FLOAT64);   out = "float64"
        case (PK_LOGICAL);   out = "logical"
        case (PK_STRING);    out = "string"
        case (PK_DATE);      out = "date"
        case (PK_TIME);      out = "time"
        case (PK_TIMESTAMP); out = "timestamp"
        case (PK_NONE);      out = "none"
        case default;        out = "unsupported"
        end select
    end subroutine value_kind_text
    !
    !> Writes a map value's type spelling, recursing when the value is itself a container.
    !!
    !! `value_kind_text` is `pure` and answers from the discriminator alone, which is all an error message
    !! needs. A NESTED a map value's spelling additionally needs the inner container to describe itself --
    !! so it cannot be pure, and it cannot be derived from the kind at all. Keeping the two apart is
    !! what lets every error message stay pure while `%kind_text` reports the nested form. See
    !! feature_container_phase7.md's D3.
    !!
    !! `%kind_text` is the ONLY name in this library that recurses: `%kindof()` stays `PK_MAP` at every
    !! depth and `parquet_kind_name` stays `"PK_MAP"`. A caller that needs the inner shape descends and
    !! asks the inner object.
    subroutine nested_value_text(col, kind, out)
        type(parquet_column), intent(in), target :: col   !! the a map value column.
        integer, intent(in) :: kind                       !! its PK_* kind.
        character(len=:), allocatable, intent(out) :: out !! the type spelling.
        class(parquet_container_column), pointer :: inner
        if (parquet_kind_is_container(kind)) then
            call parquet_column_container(col, inner)
            if (associated(inner)) then
                call inner%kind_text(out)
                return
            end if
        end if
        call value_kind_text(kind, out)
    end subroutine nested_value_text
    !
    !> Caps caller-supplied text for an error message, appending `"..."` when it was truncated.
    !!
    !! ifx's `ERROR STOP` runtime corrupts the heap once the composed message reaches 8192 bytes
    !! (CLAUDE.md), and a map key's length is entirely the caller's choice -- so the one message
    !! that quotes a key back is exactly the shape that rule exists for.
    pure subroutine preview(text, out)
        character(len=*), intent(in) :: text              !! the caller's text.
        character(len=:), allocatable, intent(out) :: out !! at most 100 characters of it.
        integer, parameter :: CAP = 100
        if (len(text) <= CAP) then
            out = text
        else
            out = text(1:CAP)//"..."
        end if
    end subroutine preview
    !
    !> Writes decimal text for an integer, for `%summary` and the lookup messages. A subroutine
    !! for the same reason as `value_kind_text` above.
    pure subroutine i2s(n, out)
        integer(int64), intent(in) :: n                   !! the value.
        character(len=:), allocatable, intent(out) :: out !! its decimal text.
        character(len=32) :: buf
        write(buf, '(i0)') n
        out = trim(buf)
    end subroutine i2s
    !
    !> The live `offsets(1:nrows+1)` of `col`, as a pointer -- the internal accessor
    !! `src/parquet_write_map.f90` reaches a map column's shape through.
    !!
    !! Trimmed to the meaningful entries: the allocation carries slack (row capacity is
    !! `size(offsets) - 1` and growth is geometric), so handing back the whole array would hand
    !! back uninitialised memory past row `nrows`.
    subroutine parquet_map_column_offsets(col, p)
        type(parquet_map_column), intent(in), target :: col !! the column.
        integer(int64), pointer, intent(out) :: p(:)        !! alias to offsets(1:nrows+1).
        if (.not. allocated(col%offsets)) error stop EP//"offsets: this column has not been initialized"
        p => col%offsets(1_int64:col%nrows_ + 1_int64)
    end subroutine parquet_map_column_offsets
    !
    !> The live keys column of `col`, as a pointer -- every row's keys, flattened, always
    !! `PK_STRING` and never null.
    subroutine parquet_map_column_keys(col, p)
        type(parquet_map_column), intent(in), target :: col !! the column.
        type(parquet_column), pointer, intent(out) :: p     !! alias to the keys column.
        p => col%keys
    end subroutine parquet_map_column_keys
    !
    !> The live values column of `col`, as a pointer -- every row's values, flattened and
    !! index-aligned with the keys, carrying the value kind, the per-VALUE validity and the unit.
    !!
    !! Deliberately hands back the `parquet_column` itself rather than a per-kind value array: the
    !! caller then reaches the values through `parquet_column_data_ptr` /
    !! `parquet_column_string_column` and the value nulls through `parquet_column_is_null`, all of
    !! which already exist, so no new `parquet_columns` surface is needed to write a map column.
    subroutine parquet_map_column_values(col, p)
        type(parquet_map_column), intent(in), target :: col !! the column.
        type(parquet_column), pointer, intent(out) :: p     !! alias to the values column.
        p => col%values
    end subroutine parquet_map_column_values
    !
    !> Writes `col`'s per-ROW validity into `valid(1:nrows)` (`.true.` = a present map) and reports
    !! whether any row is null, in ONE call rather than `nrows` calls of `%is_null(i)`.
    !!
    !! That is the whole reason it exists: `%is_null(i)` is a binding, so calling it per row from
    !! another compilation unit hands a `type(parquet_map_column)` actual to a `class`
    !! passed-object dummy on every iteration, which is exactly the per-call runtime-descriptor
    !! cost the typed tier above is here to avoid. `any_null` comes back alongside because the
    !! write path needs it to decide the field's nullability and can then skip the buffer entirely.
    subroutine parquet_map_column_row_validity(col, valid, any_null)
        type(parquet_map_column), intent(in) :: col !! the column.
        logical, intent(out) :: valid(:)            !! receives nrows entries; must be at least that long.
        logical, intent(out) :: any_null            !! .true. if at least one row is a null map.
        integer(int64) :: i
        logical :: has_bitmap
        any_null = .false.
        if (size(valid, kind=int64) < col%nrows_) error stop EP//"row_validity: destination is too short"
        has_bitmap = col%has_nulls_
        if (has_bitmap) has_bitmap = allocated(col%validity)
        if (.not. has_bitmap) then
            ! No row was ever nulled, so the bitmap does not exist at all -- the lazy-allocation
            ! state the module doc describes, and the common case.
            valid(1_int64:col%nrows_) = .true.
            return
        end if
        do i = 1_int64, col%nrows_
            valid(i) = .not. bit_test(col%validity, i)
            if (.not. valid(i)) any_null = .true.
        end do
    end subroutine parquet_map_column_row_validity
    !
end module parquet_map ! GCOVR_EXCL_LINE
