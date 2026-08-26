!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Variable-length LIST column storage: `parquet_list_column`, plus `parquet_list`, a
!! non-owning handle to one of its rows.
!!
!! A list column is the first column type in this library whose rows do not all hold the same
!! number of values. A `*_VEC` column has a fixed `col_size` and every row is that wide; a list
!! column's row 1 may hold three elements, row 2 one, and row 3 none at all. That is what the
!! Parquet `LIST` logical type describes and what nothing here could represent before.
!!
!! Three things are worth knowing before using it:
!!
!! * **The payload is ONE `parquet_column`.** Every element of every row lives in a single
!!   flattened column, and `offsets(:)` says where each row begins. So each payload kind, the
!!   per-ELEMENT null bitmap, the unit string and every value accessor come from a type that
!!   already exists and is already tested, rather than being written again per payload kind.
!! * **Row nullness and element nullness are DIFFERENT things, and both exist.** A row can be
!!   null (the list itself is absent); an element inside a row can be null (the list is present
!!   and one of its values is missing). A row of three elements one of which is null is NOT a
!!   null row. `%is_null(i)` answers the first question and the `is_valid` arguments of
!!   `%append_row`/`%get` answer the second.
!! * **The payload kind is fixed at `%init` and cannot change.** `%init(PK_INT32)` makes an
!!   `list<int32>` column, and `%append_row` with any other element type aborts. Inferring the
!!   kind from the first row appended instead would be the "sized/typed from the first element"
!!   bug class, and would leave `%element_kind()` unable to answer for an empty column.
!!
!! **Storage layout.** `offsets` has `nrows+1` entries with `offsets(1) == 0`, and row `i` holds
!! payload elements `offsets(i)+1 .. offsets(i+1)`. That is Arrow's own convention and the one
!! `parquet_string_column` already uses, so handing these buffers to Arrow is a copy rather than
!! a translation.
!!
!! **Null rows keep their offsets.** Marking a row null does not move any payload element -- the
!! offsets stay monotonic and every later row keeps its position, which is what makes `%set_null`
!! O(1). A null row reports `%length(i) == 0` and yields a zero-size array from `%get`, matching
!! Arrow, which leaves the contents of a null slot undefined and expects readers to ignore them.
!! Whatever elements such a row held before it was nulled are unreachable from that moment on and
!! are dropped by the next rebuild (`%gather_rows`).
!!
!! Depends only on `iso_fortran_env` plus `parquet_columns` and the two element-domain modules it
!! already brings in. It reaches no `bind(C)` interface, reads no setting and prints nothing --
!! see `parquet_list_column%summary`, which composes a string instead of writing to a unit for
!! exactly that reason.
module parquet_list
    use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
    ! The TYPED (non-polymorphic) tier of parquet_column. Per-element access to the payload goes
    ! through these free generics, NEVER through a type-bound call on `self%payload`: passing a
    ! `type(parquet_column)` actual to a `class` passed-object dummy in another compilation unit
    ! makes ifx build a 21-record runtime type descriptor in STATIC storage, in the caller's
    ! prologue, on every call. Enforced by check_no_type_bound_column_access
    ! (tools/check_source_conventions.py).
    use parquet_columns, only : parquet_column, parquet_container_column, parquet_kind_name, &
        parquet_column_data_ptr, parquet_column_get_at, parquet_column_is_null, &
        parquet_column_set_null, &
        PK_NONE, PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, &
        PK_DATE, PK_TIME, PK_TIMESTAMP, PK_LIST
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    !
    implicit none
    private
    !
    public :: parquet_list_column
    public :: parquet_list_row
    !
    ! Re-exported so that `use parquet_list` alone is enough to declare a list column, name its
    ! payload kind and read a temporal payload back -- the entry-module rule in CLAUDE.md, and
    ! what test_module_surface_list (test/test_module_surface.f90) asserts with a single import.
    public :: parquet_column, parquet_container_column, parquet_kind_name
    public :: PK_NONE, PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING
    public :: PK_DATE, PK_TIME, PK_TIMESTAMP, PK_LIST
    public :: parquet_date, parquet_time, parquet_timestamp
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_list: "
    !
    !> Bits per validity-bitmap block, matching `parquet_columns`' own bitmap layout.
    integer(int64), parameter :: BITS_PER_BLOCK = 64_int64
    !
    !> Smallest offsets allocation, so that a column built one row at a time does not reallocate
    !! on each of its first few appends.
    integer(int64), parameter :: MIN_ROW_CAP = 8_int64
    !
    !> A variable-length list column: `nrows` rows, each holding zero or more payload elements.
    !!
    !! Extends `parquet_container_column`, so a `parquet_column` can hold one through
    !! `%adopt_container` and reach it without naming this type. See the module doc for the
    !! storage layout and the row/element null distinction.
    type, extends(parquet_container_column) :: parquet_list_column
        private
        !> `nrows+1` entries, `offsets(1) == 0`; row i is payload elements offsets(i)+1..offsets(i+1).
        !!
        !! Allocated with slack: row capacity is `size(offsets) - 1`, and growth is geometric
        !! (1.5x, `ensure_offsets_cap`) so appending row by row is amortised O(1). Only entries
        !! `1 .. nrows_+1` are meaningful; reading past that returns uninitialised memory.
        integer(int64), allocatable :: offsets(:)
        !> Row-level null bitmap, 1 = null, `BITS_PER_BLOCK` rows to a block. LAZY: a column with
        !! no null row allocates nothing at all and `has_nulls_` stays .false.
        integer(int64), allocatable :: validity(:)
        !> The flattened elements of every row, in row order. Carries the payload's own kind,
        !! per-ELEMENT validity and unit string, so none of that is duplicated here.
        type(parquet_column) :: payload
        integer(int64) :: nrows_ = 0            !! rows stored.
        integer :: elem_kind = PK_NONE          !! the payload's PK_* kind, fixed at %init.
        logical :: has_nulls_ = .false.         !! .true. while the row bitmap is materialized.
    contains
        ! --- the deferred face parquet_columns reaches this type through (one per binding) ---
        procedure :: kindof => lc_kindof                 !! Always PK_LIST.
        procedure :: nrows => lc_nrows                   !! Rows stored.
        procedure :: clone_into => lc_clone_into         !! Allocate an independent copy.
        procedure :: gather_rows => lc_gather_rows       !! Rebuild so row k becomes old row idx(k).
        procedure :: grow_rows => lc_grow_rows           !! Append n null rows.
        procedure :: reserve_rows => lc_reserve_rows     !! Reserve row capacity.
        procedure :: ensure_validity => lc_ensure_validity !! Materialize the row bitmap eagerly.
        procedure :: kind_text => lc_kind_text           !! e.g. "list<int32>".
        procedure :: is_null_row => lc_is_null_row       !! Whether row i is a null list.
        procedure :: set_null_row => lc_set_null_row     !! Mark row i a null list.
        procedure :: clear_null_row => lc_clear_null_row !! Mark row i present again.
        ! --- lifecycle ---
        procedure :: init                                !! Fix the payload kind and (optionally) create null rows.
        procedure :: clear                               !! Release everything and reset to an uninitialized column.
        procedure :: deep_copy                           !! Independent copy of offsets, validity and payload.
        procedure :: move_from                           !! Take over another column's storage, leaving it empty.
        procedure :: adopt_rows                          !! Build from moved-in offsets and a moved-in payload column.
        ! --- queries ---
        procedure :: size => lc_nrows_public             !! Rows stored (alias of %nrows()).
        procedure :: is_init                             !! Whether %init has fixed a payload kind.
        procedure :: element_kind                        !! The payload's PK_* kind.
        procedure :: total_elements                      !! Elements stored across every row.
        procedure :: null_count                          !! Number of null rows.
        procedure :: capacity                            !! Rows the offsets allocation covers.
        procedure :: has_validity_storage                !! Whether nulling a row would still have to allocate.
        procedure, private :: length_i32                 !! int32 specific of length.
        procedure, private :: length_i64                 !! int64 specific of length.
        generic :: length => length_i32, length_i64      !! Number of elements in row i (0 for a null row).
        procedure, private :: is_null_i32                !! int32 specific of is_null.
        procedure, private :: is_null_i64                !! int64 specific of is_null.
        generic :: is_null => is_null_i32, is_null_i64   !! Whether row i is a null list.
        procedure, private :: is_empty_i32               !! int32 specific of is_empty.
        procedure, private :: is_empty_i64               !! int64 specific of is_empty.
        generic :: is_empty => is_empty_i32, is_empty_i64 !! Whether row i holds no elements (true for a null row).
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
        procedure, private :: append_row_i32             !! append_row specific for an int32 payload.
        procedure, private :: append_row_i64             !! append_row specific for an int64 payload.
        procedure, private :: append_row_f32             !! append_row specific for a float32 payload.
        procedure, private :: append_row_f64             !! append_row specific for a float64 payload.
        procedure, private :: append_row_bool            !! append_row specific for a logical payload.
        procedure, private :: append_row_str             !! append_row specific for a string payload.
        procedure, private :: append_row_date            !! append_row specific for a date payload.
        procedure, private :: append_row_time            !! append_row specific for a time payload.
        procedure, private :: append_row_ts               !! append_row specific for a timestamp payload.
        !> Appends one row holding `values`, optionally with a per-ELEMENT `is_valid` mask.
        !! The element type must match the payload kind fixed at `%init`.
        generic :: append_row => append_row_i32, append_row_i64, append_row_f32, append_row_f64, &
            append_row_bool, append_row_str, append_row_date, append_row_time, append_row_ts
        procedure :: append_null_row                     !! Append one null (absent) row.
        procedure, private :: set_null_i32               !! int32 specific of set_null.
        procedure, private :: set_null_i64               !! int64 specific of set_null.
        generic :: set_null => set_null_i32, set_null_i64 !! Mark row i null; see the module doc.
        procedure, private :: clear_null_i32             !! int32 specific of clear_null.
        procedure, private :: clear_null_i64             !! int64 specific of clear_null.
        generic :: clear_null => clear_null_i32, clear_null_i64 !! Mark row i present again.
        !
        ! THIS TYPE DELIBERATELY HAS NO `final` PROCEDURE, and one must not be added -- see the
        ! same note on parquet_container_column (src/parquet_columns.f90) and on parquet_string
        ! (src/parquet_strings.f90). It owns nothing the language does not already free: three
        ! allocatable components and one plain derived-type component, no C++ handle and no
        ! OpenMP lock. Three shipped properties depend on the absence -- nagfor 7.2 emits invalid
        ! C when finalizing an ARRAY whose element type has a finalizable COMPONENT (and one of
        ! these will sit inside a parquet_column inside a parquet_table_column inside an array),
        ! gfortran refuses a finalizable type in an OpenMP `private()` clause, and intrinsic
        ! assignment to or from one runs the finalizer twice per iteration.
    end type parquet_list_column
    !
    !> A lightweight, non-owning handle to one row of a `parquet_list_column`.
    !!
    !! Holds a column pointer plus a 1-based row index and resolves lazily on access, so it stays
    !! valid across appends and reserves of the referenced column. It is invalidated by anything
    !! that changes the row set (`clear`, `move_from`, a `gather_rows` rebuild) or by the column
    !! going out of scope. **The referenced column must be declared with the `target` attribute
    !! and must outlive the handle** -- the same contract `parquet_string_column%view` documents,
    !! and F2018 15.5.2.4 leaves the stored pointer undefined otherwise.
    type :: parquet_list_row
        private
        class(parquet_list_column), pointer :: col => null() !! referenced column (borrowed).
        integer(int64) :: idx = 0                            !! 1-based row index.
    contains
        procedure :: is_valid => plv_is_valid       !! Whether this handle refers to a live row.
        procedure :: length => plv_length           !! Number of elements in the referenced row.
        procedure :: is_null => plv_is_null         !! Whether the referenced row is a null list.
        procedure :: is_empty => plv_is_empty       !! Whether the referenced row holds no elements.
        procedure :: element_kind => plv_element_kind !! The payload's PK_* kind.
        procedure :: row_index => plv_row_index     !! The 1-based row this handle refers to.
        procedure, private :: plv_get_i32           !! get specific for an int32 payload.
        procedure, private :: plv_get_i64           !! get specific for an int64 payload.
        procedure, private :: plv_get_f32           !! get specific for a float32 payload.
        procedure, private :: plv_get_f64           !! get specific for a float64 payload.
        procedure, private :: plv_get_bool          !! get specific for a logical payload.
        procedure, private :: plv_get_str           !! get specific for a string payload.
        procedure, private :: plv_get_date          !! get specific for a date payload.
        procedure, private :: plv_get_time          !! get specific for a time payload.
        procedure, private :: plv_get_ts            !! get specific for a timestamp payload.
        !> Copies the referenced row's elements into an allocatable array, optionally with a
        !! per-element validity mask. A null row yields a zero-size result.
        generic :: get => plv_get_i32, plv_get_i64, plv_get_f32, plv_get_f64, plv_get_bool, &
            plv_get_str, plv_get_date, plv_get_time, plv_get_ts
        !
        ! NO `final` HERE EITHER, and for the stronger of the two reasons: this is a NON-OWNING
        ! handle -- a borrowed pointer and an index -- so there is nothing to release. See
        ! parquet_string (src/parquet_strings.f90), which carried one, had it removed, and
        ! records the three properties that depend on its absence.
    end type parquet_list_row
    !
contains
    !
    ! ==================================================================================
    ! The deferred face: what parquet_columns reaches this type through
    ! ==================================================================================
    !
    !> A list column is always `PK_LIST`; see `parquet_container_column%kindof`.
    pure function lc_kindof(self) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer :: res                                 !! always PK_LIST.
        res = PK_LIST
    end function lc_kindof
    !
    !> Number of rows stored; see `parquet_container_column%nrows`.
    pure function lc_nrows(self) result(n)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64) :: n                            !! rows stored.
        n = self%nrows_
    end function lc_nrows
    !
    !> Allocates `out` as an independent copy of this column; see
    !! `parquet_container_column%clone_into`.
    !!
    !! `move_alloc` rather than `select type` on purpose: it is one statement, it needs no
    !! unreachable `class default` arm, and it keeps the copy's dynamic type correct by
    !! construction rather than by a cast the compiler cannot check.
    subroutine lc_clone_into(self, out)
        class(parquet_list_column), intent(in) :: self                  !! the source column.
        class(parquet_container_column), allocatable, intent(out) :: out !! the copy, allocated here.
        type(parquet_list_column), allocatable :: cp
        allocate(cp)
        call self%deep_copy(cp)
        call move_alloc(cp, out)
    end subroutine lc_clone_into
    !
    !> Rebuilds the column so row k becomes the row that was at `idx(k)`; see
    !! `parquet_container_column%gather_rows`.
    !!
    !! The payload is rebuilt by ONE call to `parquet_column%gather` over the flattened element
    !! indices, rather than row by row: that is a single kind-dispatched pass which also carries
    !! the per-element validity and works for a string payload, none of which this module would
    !! want to reimplement. **Elements belonging to rows that `idx` does not name are dropped**,
    !! which is what compacts away a nulled row's unreachable elements (see the module doc).
    subroutine lc_gather_rows(self, idx)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: idx(:)              !! 1-based source row per destination row.
        integer(int64) :: n, k, src, lo, hi, m, pos, e
        integer(int64), allocatable :: new_offsets(:), elem_idx(:), new_valid(:)
        logical, allocatable :: src_null(:)
        n = size(idx, kind=int64)
        do k = 1_int64, n
            if (idx(k) < 1_int64 .or. idx(k) > self%nrows_) then
                error stop EP//"gather_rows: source row index out of range"
            end if
        end do
        if (self%elem_kind == PK_NONE) then
            ! An uninitialized column has no rows, so `idx` is necessarily empty and there is
            ! nothing to rebuild. Returning here keeps the payload out of parquet_column%gather,
            ! which would abort on a PK_NONE column rather than treat it as empty.
            self%nrows_ = 0_int64
            return
        end if
        ! Row nullness is read BEFORE anything is overwritten. `self%validity` and `self%offsets`
        ! are both replaced below, so a second pass over them afterwards would be reading the
        ! DESTINATION while asking about the SOURCE -- correct only by accident when the
        ! permutation happens to be the identity.
        allocate(src_null(max(n, 1_int64)))
        do k = 1_int64, n
            src_null(k) = row_is_null(self, idx(k))
        end do
        ! Two passes: count the surviving elements, then list them. Counting first is what lets
        ! `elem_idx` be allocated exact-fit -- a gather is a rebuild, and this module's rebuilds
        ! are exact-fit for the same reason parquet_columns' are.
        m = 0_int64
        do k = 1_int64, n
            src = idx(k)
            if (.not. src_null(k)) m = m + (self%offsets(src + 1_int64) - self%offsets(src))
        end do
        allocate(new_offsets(n + 1_int64))
        allocate(elem_idx(max(m, 1_int64)))
        new_offsets(1) = 0_int64
        pos = 0_int64
        do k = 1_int64, n
            src = idx(k)
            if (.not. src_null(k)) then
                lo = self%offsets(src) + 1_int64
                hi = self%offsets(src + 1_int64)
                do e = lo, hi
                    pos = pos + 1_int64
                    elem_idx(pos) = e
                end do
            end if
            new_offsets(k + 1_int64) = pos
        end do
        call self%payload%gather(elem_idx(1:m))
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
    end subroutine lc_gather_rows
    !
    !> Appends `n` null rows; see `parquet_container_column%grow_rows`.
    subroutine lc_grow_rows(self, n)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                   !! rows to append.
        integer(int64) :: k
        if (n < 0_int64) error stop EP//"grow_rows: negative row count"
        if (n == 0_int64) return
        call require_init(self, "grow_rows")
        do k = 1_int64, n
            call self%append_null_row()
        end do
    end subroutine lc_grow_rows
    !
    !> Reserves capacity for at least `n` rows; see `parquet_container_column%reserve_rows`.
    subroutine lc_reserve_rows(self, n)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                   !! rows to reserve for.
        if (n < 0_int64) error stop EP//"reserve_rows: negative row count"
        call ensure_offsets_cap(self, n)
        if (self%has_nulls_) call ensure_validity_cap(self, n)
    end subroutine lc_reserve_rows
    !
    !> Materializes the row bitmap and the payload's own validity storage eagerly; see
    !! `parquet_container_column%ensure_validity`.
    !!
    !! The concurrency escape hatch: several threads filling one column must not race on the lazy
    !! first allocation, and this is what a caller runs before the parallel region so they cannot.
    !! Both levels are covered, because both allocate lazily -- row nullness here, element
    !! nullness inside the payload.
    subroutine lc_ensure_validity(self)
        class(parquet_list_column), intent(inout) :: self !! the column.
        call ensure_validity_cap(self, max(self%nrows_, capacity_rows(self)))
        self%has_nulls_ = .true.
        call self%payload%ensure_validity()
    end subroutine lc_ensure_validity
    !
    !> Writes a human-readable description of the column's kind, e.g. `"list<int32>"`; see
    !! `parquet_container_column%kind_text`.
    subroutine lc_kind_text(self, out)
        class(parquet_list_column), intent(in) :: self     !! the column.
        character(len=:), allocatable, intent(out) :: out  !! the description.
        character(len=:), allocatable :: pk
        call payload_kind_text(self%elem_kind, pk)
        out = "list<"//pk//">"
    end subroutine lc_kind_text
    !
    !> Whether row `i` is a null list; see `parquet_container_column%is_null_row`.
    !!
    !! The three row-nullness bindings exist because `parquet_column`'s own bitmap is never
    !! allocated for a container kind, so `%is_null(i)` on the column would otherwise answer
    !! `.false.` for a row that really is null -- and `%set_null(i)` would write a bit nothing
    !! reads. Each forwards to this type's own public form, which does the bounds check.
    pure function lc_is_null_row(self, i) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                !! 1-based row index.
        logical :: res                                 !! whether the row is null.
        res = .false.
        if (i < 1_int64 .or. i > self%nrows_) return
        res = row_is_null(self, i)
    end function lc_is_null_row
    !
    !> Marks row `i` a null list; see `parquet_container_column%set_null_row`.
    subroutine lc_set_null_row(self, i)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                   !! 1-based row index.
        call self%set_null_i64(i)
    end subroutine lc_set_null_row
    !
    !> Marks row `i` present again; see `parquet_container_column%clear_null_row`.
    subroutine lc_clear_null_row(self, i)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                   !! 1-based row index.
        call self%clear_null_i64(i)
    end subroutine lc_clear_null_row
    !
    ! ==================================================================================
    ! Lifecycle
    ! ==================================================================================
    !
    !> Fixes the payload kind and, optionally, creates `nrows` null rows.
    !!
    !! **The payload kind is required and cannot change afterwards.** Inferring it from the first
    !! `%append_row` instead would type the column from one element -- the bug class CLAUDE.md
    !! devotes a testing section to -- and would report the error at the second row rather than
    !! the first. It also leaves `%element_kind()` able to answer for an EMPTY column, which a
    !! metadata query has to be able to do before any row exists.
    !!
    !! Note the asymmetry with `parquet_string_column%init`, which needs no such argument: a
    !! string column has exactly one payload type and a list column has nine.
    subroutine init(self, payload_kind, nrows, unit)
        class(parquet_list_column), intent(inout) :: self  !! the column.
        integer, intent(in) :: payload_kind                !! PK_* kind of the elements (required).
        integer(int64), intent(in), optional :: nrows      !! null rows to create (default 0).
        character(len=*), intent(in), optional :: unit     !! unit string for the payload's values.
        integer(int64) :: n
        character(len=:), allocatable :: kname
        call self%clear()
        if (.not. is_supported_payload(payload_kind)) then
            call parquet_kind_name(payload_kind, kname)
            error stop EP//"init: "//kname//" is not a supported list payload kind"
        end if
        n = 0_int64
        if (present(nrows)) n = nrows
        if (n < 0_int64) error stop EP//"init: negative row count"
        self%elem_kind = payload_kind
        ! The payload starts EMPTY whatever `nrows` says: the rows being created are null, and a
        ! null row holds no elements. Sizing the payload from the row count instead would only be
        ! right for a column whose rows all hold exactly one element.
        if (present(unit)) then
            call self%payload%init(payload_kind, 0_int64, 1_int32, unit)
        else
            call self%payload%init(payload_kind, 0_int64)
        end if
        call ensure_offsets_cap(self, max(n, MIN_ROW_CAP))
        if (n > 0_int64) call self%grow_rows(n)
    end subroutine init
    !
    !> Releases every buffer and resets to an uninitialized column with no payload kind.
    subroutine clear(self)
        class(parquet_list_column), intent(inout) :: self !! the column.
        self%nrows_ = 0_int64
        self%elem_kind = PK_NONE
        self%has_nulls_ = .false.
        if (allocated(self%offsets)) deallocate(self%offsets)
        if (allocated(self%validity)) deallocate(self%validity)
        call self%payload%clear()
    end subroutine clear
    !
    !> Produces a fully independent copy: offsets, row validity, and the payload with its own
    !! per-element validity and unit.
    subroutine deep_copy(self, out)
        class(parquet_list_column), intent(in) :: self  !! the source column.
        type(parquet_list_column), intent(out) :: out   !! receives the copy.
        integer(int64) :: n
        call out%clear()
        out%elem_kind = self%elem_kind
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
        call self%payload%deep_copy(out%payload)
    end subroutine deep_copy
    !
    !> Takes ownership of a whole column at once: `offsets` and `payload` are MOVED in (both come
    !! back deallocated/empty), `row_valid` marks which rows are present, and everything else --
    !! the payload kind, the row count -- is derived from what it was given rather than declared.
    !!
    !! The bulk counterpart of `%append_row`, and what the file reader is built on: a read knows
    !! its final row and element counts before it has a single value, so appending row by row
    !! would mean re-deriving what it already knows, once per row. Deriving the kind from the
    !! payload rather than taking it as an argument is the same design `parquet_column`'s own
    !! `%adopt_container` uses, and for the same reason -- a discriminator passed alongside an
    !! object that already carries it is a second copy that can disagree.
    !!
    !! Replaces whatever this column held (it `%clear()`s first), so it is a build, not an append.
    !!
    !! Preconditions, all checked and all fatal, because every one of them would otherwise show up
    !! later as a wrong answer rather than as an error here:
    !!
    !! * `payload` must have a kind this type accepts as a payload, and width 1.
    !! * `offsets` must be allocated with at least one entry; `size(offsets) - 1` is the row
    !!   count, so a single entry builds a legitimate empty column.
    !! * `offsets(1)` must be 0 and the sequence must be non-decreasing.
    !! * `offsets(nrows+1)` must equal the payload's row count -- i.e. the rows must account for
    !!   exactly the elements handed over, with none left over and none missing.
    !! * `row_valid`, if present, must have exactly `nrows` entries.
    !!
    !! Per-ELEMENT nullness travels inside `payload` (a `parquet_column` carries its own validity),
    !! so it needs no argument here; `row_valid` is the separate ROW level. A row marked absent
    !! keeps whatever payload range its offsets describe -- exactly as `%set_null` leaves it --
    !! so the two null levels stay independent and a later `%clear_null` restores the row intact.
    subroutine adopt_rows(self, offsets, payload, row_valid)
        class(parquet_list_column), intent(inout) :: self         !! the column being built.
        integer(int64), allocatable, intent(inout) :: offsets(:)  !! nrows+1 offsets, moved in.
        type(parquet_column), intent(inout) :: payload            !! the flattened elements, moved in.
        logical, intent(in), optional :: row_valid(:)             !! per-ROW validity; absent = all present.
        integer(int64) :: n, i
        character(len=:), allocatable :: kname
        if (.not. allocated(offsets)) error stop EP//"adopt_rows: offsets is not allocated"
        n = size(offsets, kind=int64) - 1_int64
        if (n < 0_int64) error stop EP//"adopt_rows: offsets must hold at least one entry"
        if (.not. is_supported_payload(payload%kindof())) then
            call parquet_kind_name(payload%kindof(), kname)
            error stop EP//"adopt_rows: "//kname//" is not a supported list payload kind"
        end if
        if (payload%colwidth() /= 1_int32) then
            error stop EP//"adopt_rows: the payload must be a scalar (width 1) column"
        end if
        if (offsets(1) /= 0_int64) error stop EP//"adopt_rows: offsets(1) must be 0"
        do i = 1_int64, n
            if (offsets(i + 1_int64) < offsets(i)) error stop EP//"adopt_rows: offsets are not monotonic"
        end do
        if (offsets(n + 1_int64) /= payload%length()) then
            error stop EP//"adopt_rows: the final offset does not match the payload element count"
        end if
        if (present(row_valid)) then
            if (size(row_valid, kind=int64) /= n) then
                error stop EP//"adopt_rows: row_valid has a different length from the row count"
            end if
        end if
        call self%clear()
        self%elem_kind = payload%kindof()
        self%nrows_ = n
        call move_alloc(offsets, self%offsets)
        call self%payload%move_from(payload)
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
        class(parquet_list_column), intent(inout) :: self !! the column receiving the storage.
        type(parquet_list_column), intent(inout) :: src   !! the column giving it up; left empty.
        call self%clear()
        self%nrows_ = src%nrows_
        self%elem_kind = src%elem_kind
        self%has_nulls_ = src%has_nulls_
        if (allocated(src%offsets)) call move_alloc(src%offsets, self%offsets)
        if (allocated(src%validity)) call move_alloc(src%validity, self%validity)
        call self%payload%move_from(src%payload)
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
    pure function lc_nrows_public(self) result(n)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64) :: n                            !! rows stored.
        n = self%nrows_
    end function lc_nrows_public
    !
    !> Whether `%init` has fixed a payload kind. Every mutating operation requires it.
    pure function is_init(self) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        logical :: res                                 !! .true. once %init has run.
        res = (self%elem_kind /= PK_NONE)
    end function is_init
    !
    !> The payload's PK_* kind, or PK_NONE before `%init`. Answers for an empty column too.
    pure function element_kind(self) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer :: res                                 !! the payload's PK_* discriminator.
        res = self%elem_kind
    end function element_kind
    !
    !> Total elements stored across every row, i.e. `offsets(nrows+1)`.
    !!
    !! Counts elements belonging to rows that were nulled after being appended: those are
    !! unreachable but still physically present until the next rebuild (see the module doc).
    pure function total_elements(self) result(n)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64) :: n                            !! elements stored.
        n = 0_int64
        if (allocated(self%offsets)) n = self%offsets(self%nrows_ + 1_int64)
    end function total_elements
    !
    !> Number of null rows.
    pure function null_count(self) result(n)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64) :: n                            !! null rows.
        integer(int64) :: i
        n = 0_int64
        if (.not. self%has_nulls_) return
        if (.not. allocated(self%validity)) return
        do i = 1_int64, self%nrows_
            if (bit_test(self%validity, i)) n = n + 1_int64
        end do
    end function null_count
    !
    !> Rows the offsets allocation covers; always >= `%size()`.
    pure function capacity(self) result(n)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64) :: n                            !! row capacity.
        n = capacity_rows(self)
    end function capacity
    !
    !> Whether marking a row null would still have to allocate the bitmap.
    !!
    !! The question `%ensure_validity` exists to answer in advance -- see it for why that matters
    !! when several threads fill one column.
    pure function has_validity_storage(self) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        logical :: res                                 !! .true. when the bitmap already exists.
        res = allocated(self%validity)
    end function has_validity_storage
    !
    !> int32 specific of length; see the length generic.
    function length_i32(self, i) result(n)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                !! 1-based row index.
        integer(int64) :: n                            !! elements in row i.
        n = self%length_i64(int(i, int64))
    end function length_i32
    !
    !> int64 specific of length: the number of elements in row `i`, or 0 for a null row.
    function length_i64(self, i) result(n)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                !! 1-based row index.
        integer(int64) :: n                            !! elements in row i.
        call check_row(self, i, "length")
        if (row_is_null(self, i)) then
            n = 0_int64
        else
            n = self%offsets(i + 1_int64) - self%offsets(i)
        end if
    end function length_i64
    !
    !> int32 specific of is_null; see the is_null generic.
    function is_null_i32(self, i) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                !! 1-based row index.
        logical :: res                                 !! whether row i is a null list.
        res = self%is_null_i64(int(i, int64))
    end function is_null_i32
    !
    !> int64 specific of is_null: whether row `i` is a null (absent) list.
    !!
    !! This is ROW nullness. An element inside a present row being null is a different question,
    !! answered by the `is_valid` mask `%get` returns.
    function is_null_i64(self, i) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                !! 1-based row index.
        logical :: res                                 !! whether row i is a null list.
        call check_row(self, i, "is_null")
        res = row_is_null(self, i)
    end function is_null_i64
    !
    !> int32 specific of is_empty; see the is_empty generic.
    function is_empty_i32(self, i) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                !! 1-based row index.
        logical :: res                                 !! whether row i holds no elements.
        res = self%is_empty_i64(int(i, int64))
    end function is_empty_i32
    !
    !> int64 specific of is_empty: whether row `i` holds no elements.
    !!
    !! **A null row is empty**, matching `parquet_strings`' documented default for a null string.
    !! Check `%is_null(i)` first to tell "the list is absent" from "the list is present and has
    !! length zero" -- both are representable and they are not the same thing.
    function is_empty_i64(self, i) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                !! 1-based row index.
        logical :: res                                 !! whether row i holds no elements.
        res = (self%length_i64(i) == 0_int64)
    end function is_empty_i64
    !
    !> int32 specific of view; see the view generic.
    function view_i32(self, i) result(h)
        class(parquet_list_column), intent(in), target :: self !! the column (must be a target).
        integer(int32), intent(in) :: i                        !! 1-based row index.
        type(parquet_list_row) :: h                                !! handle to row i.
        h = self%view_i64(int(i, int64))
    end function view_i32
    !
    !> int64 specific of view: a zero-copy handle to row `i`.
    !!
    !! **`self` must have the `TARGET` attribute at the call site.** F2018 15.5.2.4 leaves the
    !! handle's stored pointer undefined otherwise, and only nagfor's `-C=dangling` reports it --
    !! gfortran, ifx and flang all run the non-conforming form perfectly happily.
    function view_i64(self, i) result(h)
        class(parquet_list_column), intent(in), target :: self !! the column (must be a target).
        integer(int64), intent(in) :: i                        !! 1-based row index.
        type(parquet_list_row) :: h                                !! handle to row i.
        call check_row(self, i, "view")
        h%col => self
        h%idx = i
    end function view_i64
    !
    !> Verifies the class invariants, returning .false. and a diagnostic when one is broken.
    !!
    !! Cheap enough to call from a test after any structural change, which is what it is for.
    function validate(self, message) result(ok)
        class(parquet_list_column), intent(in) :: self                  !! the column.
        character(len=:), allocatable, intent(out), optional :: message !! diagnostic on failure.
        logical :: ok                                                   !! .true. when every invariant holds.
        integer(int64) :: i
        ok = .false.
        if (present(message)) message = ""
        if (self%nrows_ < 0_int64) then
            if (present(message)) message = "negative row count"
            return
        end if
        if (self%elem_kind == PK_NONE) then
            if (self%nrows_ /= 0_int64) then
                if (present(message)) message = "rows stored in a column with no payload kind"
                return
            end if
            ok = .true.
            return
        end if
        if (.not. allocated(self%offsets)) then
            if (present(message)) message = "payload kind is set but offsets are not allocated"
            return
        end if
        if (size(self%offsets, kind=int64) < self%nrows_ + 1_int64) then
            if (present(message)) message = "offsets shorter than nrows+1"
            return
        end if
        if (self%offsets(1) /= 0_int64) then
            if (present(message)) message = "offsets(1) is not 0"
            return
        end if
        do i = 1_int64, self%nrows_
            if (self%offsets(i + 1_int64) < self%offsets(i)) then
                if (present(message)) message = "offsets are not monotonic"
                return
            end if
        end do
        if (self%offsets(self%nrows_ + 1_int64) /= self%payload%length()) then
            if (present(message)) message = "final offset does not match the payload row count"
            return
        end if
        if (self%payload%kindof() /= self%elem_kind) then
            if (present(message)) message = "payload kind does not match element_kind"
            return
        end if
        if (self%has_nulls_ .and. .not. allocated(self%validity)) then
            if (present(message)) message = "has_nulls is set but the bitmap is not allocated"
            return
        end if
        ok = .true.
    end function validate
    !
    !> Writes a one-line human-readable description, e.g. `"list<int32>: 3 rows, 4 elements, 1 null"`.
    !!
    !! Composes a string rather than writing to a unit deliberately: printing would mean reaching
    !! an emit channel, hence `parquet_settings_base`, hence a dependency this module does not
    !! have and should not acquire. The caller decides where it goes.
    subroutine summary(self, out)
        class(parquet_list_column), intent(in) :: self    !! the column.
        character(len=:), allocatable, intent(out) :: out !! the description.
        character(len=:), allocatable :: kt, nr, ne, nn
        call self%kind_text(kt)
        call i2s(self%nrows_, nr)
        call i2s(self%total_elements(), ne)
        call i2s(self%null_count(), nn)
        out = kt//": "//nr//" rows, "//ne//" elements, "//nn//" null"
    end subroutine summary
    !
    ! ==================================================================================
    ! Capacity
    ! ==================================================================================
    !
    !> int32 specific of reserve; see the reserve generic.
    subroutine reserve_i32(self, n)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: n                   !! rows to reserve for.
        call self%reserve_i64(int(n, int64))
    end subroutine reserve_i32
    !
    !> int64 specific of reserve: grows capacity to hold at least `n` rows without adding any.
    subroutine reserve_i64(self, n)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                   !! rows to reserve for.
        call self%reserve_rows(n)
    end subroutine reserve_i64
    !
    !> Releases capacity beyond the rows stored, in the offsets and in the payload alike.
    !!
    !! A request, not an assertion: a column that has never been appended to has no slack to
    !! release and this does nothing.
    subroutine shrink_to_fit(self)
        class(parquet_list_column), intent(inout) :: self !! the column.
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
        call self%payload%shrink_to_fit()
    end subroutine shrink_to_fit
    !
    ! ==================================================================================
    ! Mutation
    ! ==================================================================================
    !
    ! The nine `append_row` specifics are deliberately identical in shape: guard, append to the
    ! payload, close the row. Everything that is not the element type lives in `check_append` and
    ! `close_row`, so a tenth payload kind is three lines and a generic entry, and so the whole
    ! family can be lifted into a generator later without being redesigned first (Q1).
    !
    !> `append_row` specific for an int32 payload: appends one row holding `values`.
    subroutine append_row_i32(self, values, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: values(:)      !! the row's elements, in order.
        logical, intent(in), optional :: is_valid(:)      !! per-ELEMENT validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_INT32, k, is_valid)
        base = self%payload%length()
        call self%payload%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_i32
    !
    !> `append_row` specific for an int64 payload: appends one row holding `values`.
    subroutine append_row_i64(self, values, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: values(:)      !! the row's elements, in order.
        logical, intent(in), optional :: is_valid(:)      !! per-ELEMENT validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_INT64, k, is_valid)
        base = self%payload%length()
        call self%payload%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_i64
    !
    !> `append_row` specific for a float32 payload: appends one row holding `values`.
    subroutine append_row_f32(self, values, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        real(real32), intent(in) :: values(:)      !! the row's elements, in order.
        logical, intent(in), optional :: is_valid(:)      !! per-ELEMENT validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_FLOAT32, k, is_valid)
        base = self%payload%length()
        call self%payload%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_f32
    !
    !> `append_row` specific for a float64 payload: appends one row holding `values`.
    subroutine append_row_f64(self, values, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        real(real64), intent(in) :: values(:)      !! the row's elements, in order.
        logical, intent(in), optional :: is_valid(:)      !! per-ELEMENT validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_FLOAT64, k, is_valid)
        base = self%payload%length()
        call self%payload%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_f64
    !
    !> `append_row` specific for a logical payload: appends one row holding `values`.
    subroutine append_row_bool(self, values, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        logical, intent(in) :: values(:)      !! the row's elements, in order.
        logical, intent(in), optional :: is_valid(:)      !! per-ELEMENT validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_LOGICAL, k, is_valid)
        base = self%payload%length()
        call self%payload%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_bool
    !
    !> `append_row` specific for a string payload: appends one row holding `values`.    !!
    !! **Trailing blanks are trimmed**, as they are everywhere a `character` ARRAY enters a
    !! column: every element of a `character(len=*)` array shares one declared length, so the
    !! padding cannot be what the caller meant. See CLAUDE.md's rule; the payload column applies
    !! it, this specific does not add or remove it.
    subroutine append_row_str(self, values, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: values(:)      !! the row's elements, in order.
        logical, intent(in), optional :: is_valid(:)      !! per-ELEMENT validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_STRING, k, is_valid)
        base = self%payload%length()
        call self%payload%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_str
    !
    !> `append_row` specific for a date payload: appends one row holding `values`.
    subroutine append_row_date(self, values, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        type(parquet_date), intent(in) :: values(:)      !! the row's elements, in order.
        logical, intent(in), optional :: is_valid(:)      !! per-ELEMENT validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_DATE, k, is_valid)
        base = self%payload%length()
        call self%payload%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_date
    !
    !> `append_row` specific for a time payload: appends one row holding `values`.
    subroutine append_row_time(self, values, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        type(parquet_time), intent(in) :: values(:)      !! the row's elements, in order.
        logical, intent(in), optional :: is_valid(:)      !! per-ELEMENT validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_TIME, k, is_valid)
        base = self%payload%length()
        call self%payload%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_time
    !
    !> `append_row` specific for a timestamp payload: appends one row holding `values`.
    subroutine append_row_ts(self, values, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        type(parquet_timestamp), intent(in) :: values(:)      !! the row's elements, in order.
        logical, intent(in), optional :: is_valid(:)      !! per-ELEMENT validity; absent = all valid.
        integer(int64) :: base, k
        k = size(values, kind=int64)
        call check_append(self, PK_TIMESTAMP, k, is_valid)
        base = self%payload%length()
        call self%payload%append_values(values)
        call close_row(self, base, k, is_valid)
    end subroutine append_row_ts
    !
    !> Appends one null (absent) row.
    !!
    !! The row holds no elements, so nothing is added to the payload and the offsets simply
    !! repeat. This is what `%grow_rows` appends, and what `%init`'s `nrows` argument creates.
    subroutine append_null_row(self)
        class(parquet_list_column), intent(inout) :: self !! the column.
        call require_init(self, "append_null_row")
        call ensure_offsets_cap(self, self%nrows_ + 1_int64)
        self%offsets(self%nrows_ + 2_int64) = self%offsets(self%nrows_ + 1_int64)
        self%nrows_ = self%nrows_ + 1_int64
        call ensure_validity_cap(self, self%nrows_)
        self%has_nulls_ = .true.
        call bit_set(self%validity, self%nrows_)
    end subroutine append_null_row
    !
    !> int32 specific of set_null; see the set_null generic.
    subroutine set_null_i32(self, i)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                   !! 1-based row index.
        call self%set_null_i64(int(i, int64))
    end subroutine set_null_i32
    !
    !> int64 specific of set_null: marks row `i` a null (absent) list.
    !!
    !! O(1): the row's payload elements are NOT removed and no later row moves, which is what
    !! keeps the offsets monotonic and every outstanding handle's index correct. The nulled row
    !! reports `%length(i) == 0` and yields a zero-size `%get` from that moment on; its former
    !! elements are unreachable and are dropped by the next `%gather_rows` rebuild.
    subroutine set_null_i64(self, i)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                   !! 1-based row index.
        call check_row(self, i, "set_null")
        call ensure_validity_cap(self, self%nrows_)
        self%has_nulls_ = .true.
        call bit_set(self%validity, i)
    end subroutine set_null_i64
    !
    !> int32 specific of clear_null; see the clear_null generic.
    subroutine clear_null_i32(self, i)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                   !! 1-based row index.
        call self%clear_null_i64(int(i, int64))
    end subroutine clear_null_i32
    !
    !> int64 specific of clear_null: marks row `i` present again.
    !!
    !! The row comes back with whatever elements its offsets still describe -- which for a row
    !! that was appended null is none, and for a row nulled after being appended is the elements
    !! it had, provided no rebuild has happened in between. Nothing else can restore those, so
    !! this is deliberately not "undo": it clears one bit.
    subroutine clear_null_i64(self, i)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                   !! 1-based row index.
        call check_row(self, i, "clear_null")
        if (.not. allocated(self%validity)) return
        call bit_clear(self%validity, i)
    end subroutine clear_null_i64
    !
    ! ==================================================================================
    ! parquet_list -- the non-owning row handle
    ! ==================================================================================
    !
    !> Whether this handle refers to a live row.
    !!
    !! .false. for a default-constructed handle, which is what a caller gets from an array of
    !! handles it has not filled yet. Every other accessor here aborts on such a handle rather
    !! than returning a plausible answer.
    pure function plv_is_valid(self) result(res)
        class(parquet_list_row), intent(in) :: self !! the handle.
        logical :: res                          !! .true. when the handle refers to a row.
        res = .false.
        if (.not. associated(self%col)) return
        res = (self%idx >= 1_int64 .and. self%idx <= self%col%nrows_)
    end function plv_is_valid
    !
    !> The 1-based row index this handle refers to (0 for an unassigned handle).
    pure function plv_row_index(self) result(i)
        class(parquet_list_row), intent(in) :: self !! the handle.
        integer(int64) :: i                     !! the row index.
        i = self%idx
    end function plv_row_index
    !
    !> Number of elements in the referenced row (0 for a null row).
    function plv_length(self) result(n)
        class(parquet_list_row), intent(in) :: self !! the handle.
        integer(int64) :: n                     !! elements in the row.
        call check_handle(self, "length")
        n = self%col%length_i64(self%idx)
    end function plv_length
    !
    !> Whether the referenced row is a null (absent) list.
    function plv_is_null(self) result(res)
        class(parquet_list_row), intent(in) :: self !! the handle.
        logical :: res                          !! whether the row is a null list.
        call check_handle(self, "is_null")
        res = self%col%is_null_i64(self%idx)
    end function plv_is_null
    !
    !> Whether the referenced row holds no elements. A null row is empty; see
    !! `parquet_list_column%is_empty`.
    function plv_is_empty(self) result(res)
        class(parquet_list_row), intent(in) :: self !! the handle.
        logical :: res                          !! whether the row holds no elements.
        call check_handle(self, "is_empty")
        res = self%col%is_empty_i64(self%idx)
    end function plv_is_empty
    !
    !> The payload's PK_* kind, so a caller can decide which `%get` specific to call.
    function plv_element_kind(self) result(res)
        class(parquet_list_row), intent(in) :: self !! the handle.
        integer :: res                          !! the payload's PK_* discriminator.
        call check_handle(self, "element_kind")
        res = self%col%elem_kind
    end function plv_element_kind
    !
    !> `get` specific for an int32 payload: copies the referenced row's elements out.
    !!
    !! The values are read through `parquet_column_data_ptr` and copied in one array assignment
    !! rather than element by element -- the free TYPED generic, never a type-bound call on the
    !! payload, which on a per-element path costs a runtime type descriptor per call under ifx.
    subroutine plv_get_i32(self, values, is_valid)
        class(parquet_list_row), intent(in) :: self                    !! the handle.
        integer(int32), allocatable, intent(out) :: values(:)  !! receives the row's elements.
        logical, allocatable, intent(out), optional :: is_valid(:) !! receives per-element validity.
        integer(int32), pointer :: p(:)
        integer(int64) :: lo, n
        call begin_get(self, PK_INT32, "get", lo, n)
        allocate(values(n))
        if (n > 0_int64) then
            call parquet_column_data_ptr(self%col%payload, p)
            values = p(lo:lo + n - 1_int64)
        end if
        if (present(is_valid)) call row_element_validity(self%col, lo, n, is_valid)
    end subroutine plv_get_i32
    !
    !> `get` specific for an int64 payload: copies the referenced row's elements out.
    !!
    !! The values are read through `parquet_column_data_ptr` and copied in one array assignment
    !! rather than element by element -- the free TYPED generic, never a type-bound call on the
    !! payload, which on a per-element path costs a runtime type descriptor per call under ifx.
    subroutine plv_get_i64(self, values, is_valid)
        class(parquet_list_row), intent(in) :: self                    !! the handle.
        integer(int64), allocatable, intent(out) :: values(:)  !! receives the row's elements.
        logical, allocatable, intent(out), optional :: is_valid(:) !! receives per-element validity.
        integer(int64), pointer :: p(:)
        integer(int64) :: lo, n
        call begin_get(self, PK_INT64, "get", lo, n)
        allocate(values(n))
        if (n > 0_int64) then
            call parquet_column_data_ptr(self%col%payload, p)
            values = p(lo:lo + n - 1_int64)
        end if
        if (present(is_valid)) call row_element_validity(self%col, lo, n, is_valid)
    end subroutine plv_get_i64
    !
    !> `get` specific for a float32 payload: copies the referenced row's elements out.
    !!
    !! The values are read through `parquet_column_data_ptr` and copied in one array assignment
    !! rather than element by element -- the free TYPED generic, never a type-bound call on the
    !! payload, which on a per-element path costs a runtime type descriptor per call under ifx.
    subroutine plv_get_f32(self, values, is_valid)
        class(parquet_list_row), intent(in) :: self                    !! the handle.
        real(real32), allocatable, intent(out) :: values(:)  !! receives the row's elements.
        logical, allocatable, intent(out), optional :: is_valid(:) !! receives per-element validity.
        real(real32), pointer :: p(:)
        integer(int64) :: lo, n
        call begin_get(self, PK_FLOAT32, "get", lo, n)
        allocate(values(n))
        if (n > 0_int64) then
            call parquet_column_data_ptr(self%col%payload, p)
            values = p(lo:lo + n - 1_int64)
        end if
        if (present(is_valid)) call row_element_validity(self%col, lo, n, is_valid)
    end subroutine plv_get_f32
    !
    !> `get` specific for a float64 payload: copies the referenced row's elements out.
    !!
    !! The values are read through `parquet_column_data_ptr` and copied in one array assignment
    !! rather than element by element -- the free TYPED generic, never a type-bound call on the
    !! payload, which on a per-element path costs a runtime type descriptor per call under ifx.
    subroutine plv_get_f64(self, values, is_valid)
        class(parquet_list_row), intent(in) :: self                    !! the handle.
        real(real64), allocatable, intent(out) :: values(:)  !! receives the row's elements.
        logical, allocatable, intent(out), optional :: is_valid(:) !! receives per-element validity.
        real(real64), pointer :: p(:)
        integer(int64) :: lo, n
        call begin_get(self, PK_FLOAT64, "get", lo, n)
        allocate(values(n))
        if (n > 0_int64) then
            call parquet_column_data_ptr(self%col%payload, p)
            values = p(lo:lo + n - 1_int64)
        end if
        if (present(is_valid)) call row_element_validity(self%col, lo, n, is_valid)
    end subroutine plv_get_f64
    !
    !> `get` specific for a logical payload: copies the referenced row's elements out.
    !!
    !! The values are read through `parquet_column_data_ptr` and copied in one array assignment
    !! rather than element by element -- the free TYPED generic, never a type-bound call on the
    !! payload, which on a per-element path costs a runtime type descriptor per call under ifx.
    subroutine plv_get_bool(self, values, is_valid)
        class(parquet_list_row), intent(in) :: self                    !! the handle.
        logical, allocatable, intent(out) :: values(:)  !! receives the row's elements.
        logical, allocatable, intent(out), optional :: is_valid(:) !! receives per-element validity.
        logical, pointer :: p(:)
        integer(int64) :: lo, n
        call begin_get(self, PK_LOGICAL, "get", lo, n)
        allocate(values(n))
        if (n > 0_int64) then
            call parquet_column_data_ptr(self%col%payload, p)
            values = p(lo:lo + n - 1_int64)
        end if
        if (present(is_valid)) call row_element_validity(self%col, lo, n, is_valid)
    end subroutine plv_get_bool
    !
    !> `get` specific for a date payload: copies the referenced row's elements out.
    !!
    !! The values are read through `parquet_column_data_ptr` and copied in one array assignment
    !! rather than element by element -- the free TYPED generic, never a type-bound call on the
    !! payload, which on a per-element path costs a runtime type descriptor per call under ifx.
    subroutine plv_get_date(self, values, is_valid)
        class(parquet_list_row), intent(in) :: self                    !! the handle.
        type(parquet_date), allocatable, intent(out) :: values(:)  !! receives the row's elements.
        logical, allocatable, intent(out), optional :: is_valid(:) !! receives per-element validity.
        type(parquet_date), pointer :: p(:)
        integer(int64) :: lo, n
        call begin_get(self, PK_DATE, "get", lo, n)
        allocate(values(n))
        if (n > 0_int64) then
            call parquet_column_data_ptr(self%col%payload, p)
            values = p(lo:lo + n - 1_int64)
        end if
        if (present(is_valid)) call row_element_validity(self%col, lo, n, is_valid)
    end subroutine plv_get_date
    !
    !> `get` specific for a time payload: copies the referenced row's elements out.
    !!
    !! The values are read through `parquet_column_data_ptr` and copied in one array assignment
    !! rather than element by element -- the free TYPED generic, never a type-bound call on the
    !! payload, which on a per-element path costs a runtime type descriptor per call under ifx.
    subroutine plv_get_time(self, values, is_valid)
        class(parquet_list_row), intent(in) :: self                    !! the handle.
        type(parquet_time), allocatable, intent(out) :: values(:)  !! receives the row's elements.
        logical, allocatable, intent(out), optional :: is_valid(:) !! receives per-element validity.
        type(parquet_time), pointer :: p(:)
        integer(int64) :: lo, n
        call begin_get(self, PK_TIME, "get", lo, n)
        allocate(values(n))
        if (n > 0_int64) then
            call parquet_column_data_ptr(self%col%payload, p)
            values = p(lo:lo + n - 1_int64)
        end if
        if (present(is_valid)) call row_element_validity(self%col, lo, n, is_valid)
    end subroutine plv_get_time
    !
    !> `get` specific for a timestamp payload: copies the referenced row's elements out.
    !!
    !! The values are read through `parquet_column_data_ptr` and copied in one array assignment
    !! rather than element by element -- the free TYPED generic, never a type-bound call on the
    !! payload, which on a per-element path costs a runtime type descriptor per call under ifx.
    subroutine plv_get_ts(self, values, is_valid)
        class(parquet_list_row), intent(in) :: self                    !! the handle.
        type(parquet_timestamp), allocatable, intent(out) :: values(:)  !! receives the row's elements.
        logical, allocatable, intent(out), optional :: is_valid(:) !! receives per-element validity.
        type(parquet_timestamp), pointer :: p(:)
        integer(int64) :: lo, n
        call begin_get(self, PK_TIMESTAMP, "get", lo, n)
        allocate(values(n))
        if (n > 0_int64) then
            call parquet_column_data_ptr(self%col%payload, p)
            values = p(lo:lo + n - 1_int64)
        end if
        if (present(is_valid)) call row_element_validity(self%col, lo, n, is_valid)
    end subroutine plv_get_ts
    !
    !> `get` specific for a string payload: copies the referenced row's elements out.
    !!
    !! The result is sized to the LONGEST element of this row, not to the payload's widest
    !! string, so a row of short values costs what it holds. Each element is copied in
    !! separately rather than through a whole-array assignment, because a deferred-length
    !! allocatable character array reallocates every element on assignment from a differently
    !! sized right-hand side (CLAUDE.md).
    subroutine plv_get_str(self, values, is_valid)
        class(parquet_list_row), intent(in) :: self                     !! the handle.
        character(len=:), allocatable, intent(out) :: values(:)     !! receives the row's elements.
        logical, allocatable, intent(out), optional :: is_valid(:)  !! receives per-element validity.
        character(len=:), allocatable :: one
        integer(int64) :: lo, n, e
        integer :: wid
        call begin_get(self, PK_STRING, "get", lo, n)
        wid = 0
        do e = 1_int64, n
            call parquet_column_get_at(self%col%payload, lo + e - 1_int64, one)
            wid = max(wid, len(one))
        end do
        allocate(character(len=wid) :: values(n))
        do e = 1_int64, n
            call parquet_column_get_at(self%col%payload, lo + e - 1_int64, one)
            values(e) = one
        end do
        if (present(is_valid)) call row_element_validity(self%col, lo, n, is_valid)
    end subroutine plv_get_str
    !
    ! ==================================================================================
    ! Private helpers
    ! ==================================================================================
    !
    !> Aborts unless `%init` has fixed a payload kind.
    subroutine require_init(self, proc)
        class(parquet_list_column), intent(in) :: self !! the column.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        if (self%elem_kind == PK_NONE) then
            error stop EP//proc//": this list column has no payload kind; call %init first"
        end if
    end subroutine require_init
    !
    !> Aborts unless `i` names an existing row.
    subroutine check_row(self, i, proc)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                !! 1-based row index.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        if (i < 1_int64 .or. i > self%nrows_) then
            error stop EP//proc//": row index is out of range"
        end if
    end subroutine check_row
    !
    !> Aborts unless the handle refers to a live row of a live column.
    subroutine check_handle(self, proc)
        class(parquet_list_row), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: proc    !! calling procedure, for the message.
        if (.not. associated(self%col)) then
            error stop EP//proc//": this row handle is not associated with a column"
        end if
        if (self%idx < 1_int64 .or. self%idx > self%col%nrows_) then
            error stop EP//proc//": this row handle no longer refers to a valid row"
        end if
    end subroutine check_handle
    !
    !> The guard every `append_row` specific shares: initialized, right payload kind, and a mask
    !! of the right length if one was supplied.
    subroutine check_append(self, want_kind, k, is_valid)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer, intent(in) :: want_kind               !! the PK_* kind the caller's values are.
        integer(int64), intent(in) :: k                !! number of elements in the row.
        logical, intent(in), optional :: is_valid(:)   !! the caller's per-element mask, if any.
        character(len=:), allocatable :: have_txt, want_txt
        call require_init(self, "append_row")
        if (self%elem_kind /= want_kind) then
            call payload_kind_text(self%elem_kind, have_txt)
            call payload_kind_text(want_kind, want_txt)
            error stop EP//"append_row: this is a "//have_txt//" list column; "//want_txt// &
                " values cannot be appended to it"
        end if
        if (present(is_valid)) then
            if (size(is_valid, kind=int64) /= k) then
                error stop EP//"append_row: is_valid has a different length from values"
            end if
        end if
    end subroutine check_append
    !
    !> Closes the row every `append_row` specific has just written into the payload: extends the
    !! offsets, advances the row count, and applies the per-element validity mask.
    !!
    !! The mask is applied through the free TYPED `parquet_column_set_null`, never through a
    !! type-bound call on the payload -- this loop is per ELEMENT.
    subroutine close_row(self, base, k, is_valid)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: base                !! payload length before the values were appended.
        integer(int64), intent(in) :: k                   !! number of elements appended.
        logical, intent(in), optional :: is_valid(:)      !! per-element validity, if supplied.
        integer(int64) :: e
        call ensure_offsets_cap(self, self%nrows_ + 1_int64)
        self%offsets(self%nrows_ + 2_int64) = base + k
        self%nrows_ = self%nrows_ + 1_int64
        ! The new row is PRESENT, so the row bitmap needs nothing written: a grown bitmap is
        ! zero-filled and 0 means valid. It still has to COVER the new row, or a later
        ! %set_null on it would index past the allocation.
        if (self%has_nulls_) call ensure_validity_cap(self, self%nrows_)
        if (.not. present(is_valid)) return
        do e = 1_int64, k
            if (.not. is_valid(e)) call parquet_column_set_null(self%payload, base + e)
        end do
    end subroutine close_row
    !
    !> Resolves a handle to the payload range of its row, aborting unless the payload kind is the
    !! one the calling `%get` specific expects. A null row resolves to a zero-length range.
    subroutine begin_get(self, want_kind, proc, lo, n)
        class(parquet_list_row), intent(in) :: self !! the handle.
        integer, intent(in) :: want_kind        !! the PK_* kind the caller's array is.
        character(len=*), intent(in) :: proc    !! calling procedure, for the message.
        integer(int64), intent(out) :: lo       !! 1-based first payload element of the row.
        integer(int64), intent(out) :: n        !! number of elements in the row.
        character(len=:), allocatable :: have_txt, want_txt
        call check_handle(self, proc)
        if (self%col%elem_kind /= want_kind) then
            call payload_kind_text(self%col%elem_kind, have_txt)
            call payload_kind_text(want_kind, want_txt)
            error stop EP//proc//": this is a "//have_txt//" list column; it cannot be read into "// &
                want_txt//" values"
        end if
        if (row_is_null(self%col, self%idx)) then
            ! A null row yields a zero-size result rather than aborting: nulls are ordinary in a
            ! list column, and %is_null() is what distinguishes an absent list from an empty one.
            lo = 1_int64
            n = 0_int64
        else
            lo = self%col%offsets(self%idx) + 1_int64
            n = self%col%offsets(self%idx + 1_int64) - self%col%offsets(self%idx)
        end if
    end subroutine begin_get
    !
    !> Builds the per-ELEMENT validity mask for one row's payload range.
    subroutine row_element_validity(col, lo, n, is_valid)
        class(parquet_list_column), intent(in) :: col        !! the column.
        integer(int64), intent(in) :: lo                     !! first payload element of the row.
        integer(int64), intent(in) :: n                      !! elements in the row.
        logical, allocatable, intent(out) :: is_valid(:)     !! receives the mask.
        integer(int64) :: e
        allocate(is_valid(n))
        do e = 1_int64, n
            is_valid(e) = .not. parquet_column_is_null(col%payload, lo + e - 1_int64)
        end do
    end subroutine row_element_validity
    !
    !> Whether row `i` is null, with no bounds check -- callers have already made one.
    pure function row_is_null(self, i) result(res)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                !! 1-based row index.
        logical :: res                                 !! whether row i is null.
        res = .false.
        if (.not. self%has_nulls_) return
        if (.not. allocated(self%validity)) return
        res = bit_test(self%validity, i)
    end function row_is_null
    !
    !> Rows the offsets allocation covers.
    pure function capacity_rows(self) result(n)
        class(parquet_list_column), intent(in) :: self !! the column.
        integer(int64) :: n                            !! row capacity.
        n = 0_int64
        if (allocated(self%offsets)) n = size(self%offsets, kind=int64) - 1_int64
    end function capacity_rows
    !
    !> Ensures `offsets` is allocated with room for at least `need_rows` rows, preserving what is
    !! already stored. Growth is geometric (1.5x), matching `parquet_string_column`'s own offsets
    !! growth and `parquet_column`'s `ensure_capacity`, so appending row by row is amortised O(1).
    subroutine ensure_offsets_cap(self, need_rows)
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: need_rows           !! required row capacity.
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
        class(parquet_list_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: need_rows           !! rows the bitmap must cover.
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
    !> Whether a PK_* kind may be a list column's payload.
    !!
    !! The nine SCALAR value kinds. A `*_VEC` kind is excluded because a fixed-width vector inside
    !! a variable-length list is `list<fixed_size_list<...>>`, which is Phase 7's nesting question
    !! and not expressible by giving the payload a width. `PK_LIST`/`PK_MAP`/`PK_STRUCT` are
    !! excluded for the same reason -- a container payload is nesting, and `%init` would have no
    !! way to be told what the inner container holds.
    pure function is_supported_payload(kind) result(res)
        integer, intent(in) :: kind !! a PK_* discriminator.
        logical :: res              !! whether it may be a list payload.
        select case (kind)
        case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, &
              PK_DATE, PK_TIME, PK_TIMESTAMP)
            res = .true.
        case default
            res = .false.
        end select
    end function is_supported_payload
    !
    !> Writes the short lowercase name of a payload kind, for `kind_text` and error messages.
    !!
    !! Deliberately NOT `parquet_kind_name`, which spells the discriminator (`"PK_INT32"`). This
    !! is the type spelling a MAML schema uses, so `%kind_text()` reads `"list<int32>"` -- the
    !! form a user writes and a later phase will have to parse.
    !!
    !! A subroutine rather than a `character(len=:), allocatable` FUNCTION, per the project-wide
    !! rule in CLAUDE.md: gfortran PR113797 makes the hidden length temporary such a function
    !! needs unreliable to be thread-local, which has already caused silent memory corruption in
    !! this library once.
    pure subroutine payload_kind_text(kind, out)
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
    end subroutine payload_kind_text
    !
    !> Writes decimal text for an integer, for `%summary`. A subroutine for the same reason as
    !! `payload_kind_text` above.
    pure subroutine i2s(n, out)
        integer(int64), intent(in) :: n                   !! the value.
        character(len=:), allocatable, intent(out) :: out !! its decimal text.
        character(len=32) :: buf
        write(buf, '(i0)') n
        out = trim(buf)
    end subroutine i2s
    !
end module parquet_list ! GCOVR_EXCL_LINE
