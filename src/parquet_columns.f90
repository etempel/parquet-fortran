!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_columns.py
! The kind table lives in that script; edit it there, not here.
!
!> Type-erased, whole-column value storage shared by `parquet_table` and the
!! list/map/struct container column types.
!!
!! `parquet_column` holds the values of ONE column of ONE table: a `PK_*` kind discriminator
!! plus exactly one allocated storage array (the active kind's), a sparse null bitmap, an
!! optional unit string, and the row count. Only the active kind's array is ever allocated, so
!! a column costs one array plus a handful of scalars regardless of how many kinds exist.
!!
!! Three things are worth knowing before using it:
!!
!! * **Validity is kind-dispatched, not uniform.** Numeric and logical kinds carry a
!!   column-level null bitmap; the string kinds delegate to the embedded
!!   `parquet_string_column`'s own validity; the temporal kinds carry their null state INSIDE
!!   each element (matching `parquet_temporal`'s deliberate design). `is_null`/`set_null`/
!!   `any_null` hide this, and there is no public `has_nulls`.
!! * **Validity is sparse.** A column with no nulls allocates NO bitmap at all -- only a
!!   logical scalar. The bitmap appears on the first `set_null`/`append_nulls` and disappears
!!   again on a whole-column `set_all` (or an explicit `compact_validity`).
!! * **Validity is per ELEMENT, and a vector row is not one bit.** The bitmap is
!!   `width * nrows` bits, and every query and mutation comes in a row form and an `(i, e)`
!!   element form. The two are deliberately asymmetric where they differ: a row QUERY
!!   (`is_null(i)`, `row_validity`) answers "any element of the row is null", while a whole-row
!!   MUTATION (`set_null(i)`, `clear_null(i)`, `append_nulls`) acts on every element of it.
!!   `modify_nulls=.false.` skips individual null ELEMENTS, not whole rows.
!! * **Row indices are `integer(int64)` throughout.** This type is internal to the library and
!!   never sees a caller's default-kind `INTEGER`, so it deliberately does not carry the
!!   int32/int64 specific pairs the public API uses.
!!
!! Depends only on `iso_fortran_env` plus the two element-domain modules (`parquet_strings`,
!! `parquet_temporal`) -- never on `parquet` itself, so the container column types can consume
!! it symmetrically without a circular dependency.
module parquet_columns
    use, intrinsic :: iso_fortran_env, only : int8, int32, int64, real32, real64
    ! The TYPED tier of parquet_string_column (feature_ifx.md): every `str` access below goes
    ! through these, never through a binding -- a `type` actual passed to a `class` dummy makes
    ! ifx build a runtime type descriptor in STATIC storage in the caller's prologue, on every
    ! call, which turns a per-element scan into cross-thread cache-line contention.
    use parquet_strings, only : parquet_string_column, &
        parquet_string_column_append_column, parquet_string_column_append_from, parquet_string_column_append_nulls, &
        parquet_string_column_append_values, parquet_string_column_capacity, &
        parquet_string_column_character_capacity, parquet_string_column_character_size, &
        parquet_string_column_copy_to, parquet_string_column_delete_by_mask, parquet_string_column_gather_from, &
        parquet_string_column_get, parquet_string_column_has_validity, parquet_string_column_is_null, &
        parquet_string_column_move_from, &
        parquet_string_column_null_count, parquet_string_column_reindex, parquet_string_column_reindex_trusted, &
        parquet_string_column_reserve, parquet_string_column_reserve_validity, parquet_string_column_set, &
        parquet_string_column_set_null, parquet_string_column_set_validity, parquet_string_column_shrink_to_fit, &
        parquet_string_column_size
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    !
    implicit none
    private
    !
    public :: parquet_column
    public :: parquet_container_column
    public :: parquet_kind_name
    public :: parquet_kind_is_container
    public :: PK_NONE
    public :: PK_INT32
    public :: PK_INT64
    public :: PK_FLOAT32
    public :: PK_FLOAT64
    public :: PK_LOGICAL
    public :: PK_STRING
    public :: PK_DATE
    public :: PK_TIME
    public :: PK_TIMESTAMP
    public :: PK_INT32_VEC
    public :: PK_INT64_VEC
    public :: PK_FLOAT32_VEC
    public :: PK_FLOAT64_VEC
    public :: PK_LOGICAL_VEC
    public :: PK_STRING_VEC
    public :: PK_DATE_VEC
    public :: PK_TIME_VEC
    public :: PK_TIMESTAMP_VEC
    public :: PK_LIST
    public :: PK_MAP
    public :: PK_STRUCT
    !
    ! The typed per-cell accessor tier (feature_ifx.md). INTERNAL API: public only because
    ! `parquet_tables` is a different module and this type's components are private, so there is
    ! no other way for it to reach storage without a type-bound call -- which is the thing being
    ! avoided. `src/parquet.f90` privatises every one of these again, so none reaches a
    ! `use parquet` program, and none is covered by the library's semantic-versioning promise or
    ! listed in README's API overview. Only the generics are public; every specific is private.
    public :: parquet_column_get_at
    public :: parquet_column_set_at
    public :: parquet_column_get_elem
    public :: parquet_column_set_elem
    public :: parquet_column_data_ptr
    public :: parquet_column_is_null
    public :: parquet_column_set_null
    public :: parquet_column_clear_null
    public :: parquet_column_string_column
    public :: parquet_column_container
    !
    ! INTERNAL API, for the same reason and on the same terms as the tier above: a NON-MUTATING
    ! `any_null`, so that a table read accessor holding its table by `intent(in)` can ask the
    ! question without writing to the column. The type-bound `%any_null()` refreshes the temporal
    ! null cache and so cannot be called from a shared column -- see its own interface below.
    ! `src/parquet.f90` privatises this too; a user calls `%any_null()`.
    public :: parquet_column_any_null
    !
    ! INTERNAL API on the same terms: the ranged form of `row_validity`, which fills a caller's
    ! block-sized mask for a row range without allocating. `%print_stat`'s statistics scan walks
    ! a column with it; `src/parquet.f90` privatises this too, and a user calls
    ! `%row_validity_range`.
    public :: parquet_column_row_validity_range
    !
    ! Test-only, and public for the reason `parquet_debug_string_bulk_threads` is (parquet_strings):
    ! this tier is Arrow-free by contract, so the C++ hook every other `parquet_debug_*` observable
    ! uses is not available, and a test that re-derived the rule would assert against its own copy.
    ! No library code calls it. See its interface under "Lifecycle, queries and unit".
    public :: parquet_debug_column_gather_threads
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_columns: "
    !
    !> Bits per validity-bitmap block. The bitmap is a hand-rolled `integer(int64)` array
    !! (1 = null, 0 = valid): measured 1.6-2x faster than stdlib's `bitset_large` on random
    !! set/test and on the reindex rebuild, at identical memory, with no int32 bit-index
    !! ceiling, which is why this library takes no stdlib dependency for it.
    integer(int64), parameter :: BITS_PER_BLOCK = 64_int64
    !
    !> `BITS_PER_BLOCK`, published.
    !!
    !! **Exposed for exactly one reason: so that nobody has to copy it.** A caller filling one
    !! column from several threads -- `parquet_tables`' parallel single-column read is the one that
    !! exists -- must keep two threads off the same bitmap block, because updating a block is a
    !! read-modify-write and a lost update silently moves a null to the wrong row. Computing that
    !! alignment needs this number, and a second copy of it living in another module could drift
    !! from this one with nothing to report it. This is a fact about the storage layout, not a knob:
    !! it is a `parameter`, and changing it changes only how the same bits are grouped.
    integer(int64), parameter, public :: parquet_validity_block_bits = BITS_PER_BLOCK
    !
    ! ---- Column kind discriminators ----
    integer, parameter :: PK_NONE = 0 !! no kind assigned yet (a default-initialized column)
    integer, parameter :: PK_INT32 = 1 !! 32-bit integer scalar column
    integer, parameter :: PK_INT64 = 2 !! 64-bit integer scalar column
    integer, parameter :: PK_FLOAT32 = 3 !! 32-bit real scalar column
    integer, parameter :: PK_FLOAT64 = 4 !! 64-bit real scalar column
    integer, parameter :: PK_LOGICAL = 5 !! logical scalar column
    integer, parameter :: PK_STRING = 6 !! variable-length string scalar column
    integer, parameter :: PK_DATE = 7 !! date scalar column
    integer, parameter :: PK_TIME = 8 !! time scalar column
    integer, parameter :: PK_TIMESTAMP = 9 !! timestamp scalar column
    integer, parameter :: PK_INT32_VEC = 11 !! 32-bit integer vector column (width values per row)
    integer, parameter :: PK_INT64_VEC = 12 !! 64-bit integer vector column
    integer, parameter :: PK_FLOAT32_VEC = 13 !! 32-bit real vector column
    integer, parameter :: PK_FLOAT64_VEC = 14 !! 64-bit real vector column
    integer, parameter :: PK_LOGICAL_VEC = 15 !! logical vector column
    integer, parameter :: PK_STRING_VEC = 16 !! string vector column (one flat string store, stride width)
    integer, parameter :: PK_DATE_VEC = 17 !! date vector column
    integer, parameter :: PK_TIME_VEC = 18 !! time vector column
    integer, parameter :: PK_TIMESTAMP_VEC = 19 !! timestamp vector column
    integer, parameter :: PK_LIST = 21 !! variable-length list column, held as a `parquet_list_column`
    integer, parameter :: PK_MAP = 22 !! map column, held as a `parquet_map_column`
    integer, parameter :: PK_STRUCT = 23 !! struct column, held as a `parquet_struct_column`
    !
    !> The abstract face of a CONTAINER column -- `PK_LIST`, `PK_MAP` or `PK_STRUCT`.
    !!
    !! A container column's storage is not a Fortran array, so it cannot live in a component of
    !! `parquet_column` the way the eighteen array/string kinds do. It lives behind this type
    !! instead: `parquet_column` holds one `class(parquet_container_column), allocatable` slot and
    !! reaches whatever is in it through the deferred bindings below, never by naming the concrete
    !! type -- which it could not do anyway, since `parquet_list` and its siblings `use` this
    !! module rather than the other way round.
    !!
    !! **The binding set is deliberately minimal and is the phase's most load-bearing artefact.**
    !! A binding exists here only because a procedure in `parquet_columns` has to call it on a
    !! container whose type it cannot name. Everything a *user* does with a list column --
    !! `%append_row`, `%view`, `%length`, per-element access -- lives on the concrete type's own
    !! public API and is deliberately absent from this face. Two consequences worth keeping:
    !!
    !! * **Every binding here is per-COLUMN, never per-cell.** That is what makes polymorphic
    !!   dispatch acceptable at all -- CLAUDE.md's typed-accessor rule bans a `class` dummy on a
    !!   per-cell path (ifx builds a runtime type descriptor in the caller's prologue on every
    !!   call) and explicitly permits it once per column.
    !! * **Adding a binding later costs one implementation per concrete container type.** Prefer
    !!   solving a new need on the concrete type, and widen this face only when a procedure in
    !!   `parquet_columns` genuinely cannot proceed without it.
    type, abstract :: parquet_container_column
    contains
        procedure(container_kindof_i), deferred :: kindof            !! The PK_* discriminator this container is.
        procedure(container_nrows_i), deferred :: nrows              !! Rows the container holds.
        procedure(container_clone_into_i), deferred :: clone_into    !! Allocate `out` as an independent copy.
        procedure(container_gather_rows_i), deferred :: gather_rows  !! Rebuild so row k becomes old row idx(k).
        procedure(container_grow_rows_i), deferred :: grow_rows      !! Append n null rows.
        procedure(container_append_from_i), deferred :: append_from  !! Append every row of another container.
        procedure(container_reserve_rows_i), deferred :: reserve_rows !! Reserve capacity for n rows.
        procedure(container_ensure_validity_i), deferred :: ensure_validity !! Materialize validity storage.
        procedure(container_kind_text_i), deferred :: kind_text      !! Human-readable kind, e.g. "list<int32>".
        procedure(container_is_null_row_i), deferred :: is_null_row  !! Whether row i is a null container.
        procedure(container_set_null_row_i), deferred :: set_null_row !! Mark row i a null container.
        procedure(container_clear_null_row_i), deferred :: clear_null_row !! Mark row i present again.
        !
        ! THIS TYPE DELIBERATELY HAS NO `final` PROCEDURE, and an extending type must not add one
        ! either -- see the same note at the end of parquet_string (src/parquet_strings.f90) for
        ! the three shipped properties that depend on the absence. In short: nagfor 7.2 emits
        ! invalid C when finalizing an ARRAY whose element type has a finalizable COMPONENT, and a
        ! container column sits inside a parquet_column, which sits inside a parquet_table_column,
        ! which lives in an array; gfortran refuses a finalizable type in an OpenMP `private()`
        ! clause; and intrinsic assignment to or from one runs the finalizer twice per iteration.
        ! Nothing a container column owns needs freeing that the language does not already free.
    end type parquet_container_column
    !
    ! The deferred bindings' interfaces. Each is named `container_<binding>_i` so that the binding
    ! name itself stays free -- `kind_text` and `grow_rows` are both also ordinary module-level
    ! procedure names in this module, on different types and with different argument lists.
    abstract interface
        !> The PK_* discriminator this container is (`PK_LIST`, `PK_MAP` or `PK_STRUCT`).
        !!
        !! `adopt_container` cannot ask any other way: it takes a `class(parquet_container_column)`
        !! and this module may not name a concrete container type, so the discriminator has to come
        !! through the abstract face or `parquet_column%kind` could never be set at all.
        pure function container_kindof_i(self) result(res)
            import :: parquet_container_column
            class(parquet_container_column), intent(in) :: self !! the container column.
            integer :: res                                      !! the PK_* discriminator.
        end function container_kindof_i
        !
        !> Number of rows the container holds.
        pure function container_nrows_i(self) result(n)
            import :: parquet_container_column, int64
            class(parquet_container_column), intent(in) :: self !! the container column.
            integer(int64) :: n                                 !! rows stored.
        end function container_nrows_i
        !
        !> Allocates `out` with this container's dynamic type and fills it with an independent copy.
        !!
        !! `parquet_column%deep_copy` cannot allocate `out` itself -- only the dynamic type knows
        !! what to allocate -- so the copy is delegated whole rather than split into
        !! allocate-then-copy the way the array kinds' `init` + `copy_storage` is.
        subroutine container_clone_into_i(self, out)
            import :: parquet_container_column
            class(parquet_container_column), intent(in) :: self               !! the source container.
            class(parquet_container_column), allocatable, intent(out) :: out  !! the copy, allocated here.
        end subroutine container_clone_into_i
        !
        !> Rebuilds the container so that row k becomes the row that was at `idx(k)`.
        !!
        !! Serves `reindex`, `delete_by_mask`, `%filter_rows` and `%sort_by` alike, exactly as
        !! `gather_storage` does for the array kinds. `idx` may be any length and may repeat.
        subroutine container_gather_rows_i(self, idx)
            import :: parquet_container_column, int64
            class(parquet_container_column), intent(inout) :: self !! the container column.
            integer(int64), intent(in) :: idx(:)                   !! source row for each new row.
        end subroutine container_gather_rows_i
        !
        !> Appends `n` null rows.
        subroutine container_grow_rows_i(self, n)
            import :: parquet_container_column, int64
            class(parquet_container_column), intent(inout) :: self !! the container column.
            integer(int64), intent(in) :: n                        !! rows to append.
        end subroutine container_grow_rows_i
        !
        !> Appends every row of `src` onto this container.
        !!
        !! What `parquet_column%append` (and therefore a table's slice materialization and
        !! `%append`) reaches for a container kind. It is a deferred binding rather than anything
        !! `parquet_columns` could do itself because concatenation is entirely the concrete type's
        !! business: only it knows how a row is laid out, and the offsets have to be **rebased**
        !! onto the destination's own element count rather than copied.
        !!
        !! Four obligations on every implementation, each of which fails silently if skipped:
        !!
        !! * **Check the dynamic type**, and abort naming both kinds. Appending a map onto a list
        !!   must not reinterpret one offsets array as the other.
        !! * **Check the payload layout** -- a list's element kind, a map's value kind, a struct's
        !!   whole field list. A struct must name the first field that differs.
        !! * **Rebase, do not copy, the offsets.** `dst(n+k) = dst_end + (src(k) - src(1))`.
        !! * **MERGE validity, do not replace it.** The destination rows are fresh, so this is
        !!   `%append`'s rule and not `%paste`'s.
        subroutine container_append_from_i(self, src)
            import :: parquet_container_column
            class(parquet_container_column), intent(inout) :: self !! the destination container.
            class(parquet_container_column), intent(in) :: src     !! rows to append, unchanged.
        end subroutine container_append_from_i
        !
        !> Reserves capacity for at least `n` rows without changing the row count.
        subroutine container_reserve_rows_i(self, n)
            import :: parquet_container_column, int64
            class(parquet_container_column), intent(inout) :: self !! the container column.
            integer(int64), intent(in) :: n                        !! rows to reserve for.
        end subroutine container_reserve_rows_i
        !
        !> Materializes whatever validity storage the container keeps lazily.
        !!
        !! The concurrency escape hatch behind `parquet_column%ensure_validity`, and it must not be
        !! refused for a container kind: a table filled from several threads calls it before the
        !! region so that the first null does not race with a lazy allocation.
        subroutine container_ensure_validity_i(self)
            import :: parquet_container_column
            class(parquet_container_column), intent(inout) :: self !! the container column.
        end subroutine container_ensure_validity_i
        !
        !> Writes a human-readable description of the container's kind, e.g. `"list<int32>"`.
        !!
        !! A subroutine with an allocatable `intent(out)` argument rather than a
        !! `character(len=:), allocatable` FUNCTION, per the project-wide rule in CLAUDE.md
        !! (gfortran PR113797: the hidden length temporary is not reliably thread-local).
        subroutine container_kind_text_i(self, out)
            import :: parquet_container_column
            class(parquet_container_column), intent(in) :: self       !! the container column.
            character(len=:), allocatable, intent(out) :: out         !! the description.
        end subroutine container_kind_text_i
        !
        !> Whether row `i` is a null container -- an ABSENT list/map/struct, not a present but
        !! empty one, and not a present one holding a null element.
        !!
        !! `parquet_column`'s own bitmap is never allocated for a container kind, so row nullness
        !! has to be asked of the container or `%is_null(i)` would answer `.false.` for a row that
        !! is genuinely null. That is a silent wrong answer on an ordinary path, which is why
        !! these three are deferred at all -- the rest of the face exists for operations
        !! `parquet_columns` performs, and these exist for a question it must be able to ANSWER.
        pure function container_is_null_row_i(self, i) result(res)
            import :: parquet_container_column, int64
            class(parquet_container_column), intent(in) :: self !! the container column.
            integer(int64), intent(in) :: i                     !! 1-based row index.
            logical :: res                                      !! whether the row is null.
        end function container_is_null_row_i
        !
        !> Marks row `i` a null container.
        subroutine container_set_null_row_i(self, i)
            import :: parquet_container_column, int64
            class(parquet_container_column), intent(inout) :: self !! the container column.
            integer(int64), intent(in) :: i                        !! 1-based row index.
        end subroutine container_set_null_row_i
        !
        !> Marks row `i` present again.
        subroutine container_clear_null_row_i(self, i)
            import :: parquet_container_column, int64
            class(parquet_container_column), intent(inout) :: self !! the container column.
            integer(int64), intent(in) :: i                        !! 1-based row index.
        end subroutine container_clear_null_row_i
    end interface
    !
    !
    !> One column's values: a kind discriminator, one active storage array, sparse validity,
    !! an optional unit, and the row/width geometry.
    type :: parquet_column
        private
        integer :: kind = PK_NONE                      !! active PK_* discriminator.
        integer(int64) :: nrows = 0                    !! number of rows stored.
        !> Rows the active storage is ALLOCATED for; always >= nrows, 0 when nothing is allocated.
        !!
        !! Growth is geometric (1.5x, `ensure_capacity`), so appending row by row is amortised O(1)
        !! instead of the O(n^2) an exact-fit realloc per append would cost. The slack is invisible
        !! to every caller because every read of a storage array is bounded by `1:nrows` -- an
        !! UNBOUNDED read would return uninitialised tail elements, which is the one silent failure
        !! this component introduces (see feature_risks.md).
        integer(int64) :: cap = 0
        integer(int32) :: width = 1                    !! values per row; > 1 only for *_VEC kinds.
        logical :: has_nulls = .false.                 !! .true. while the bitmap is materialized.
        logical :: nulls_dirty = .true.                !! temporal kinds: the null cache needs a rescan.
        logical :: nulls_cached = .false.              !! temporal kinds: cached "column has >= 1 null".
        integer(int64), allocatable :: validity(:)     !! null bitmap (1 = null); allocated iff has_nulls.
        character(len=:), allocatable :: unit          !! unit string from the MAML `unit:` key (D10).
        type(parquet_string_column), allocatable :: str !! PK_STRING / PK_STRING_VEC storage (DD1).
        integer(int32), allocatable :: i32(:)   !! PK_INT32 storage, scalar.
        integer(int64), allocatable :: i64(:)   !! PK_INT64 storage, scalar.
        real(real32), allocatable :: f32(:)   !! PK_FLOAT32 storage, scalar.
        real(real64), allocatable :: f64(:)   !! PK_FLOAT64 storage, scalar.
        logical, allocatable :: bool(:)   !! PK_LOGICAL storage, scalar.
        type(parquet_date), allocatable :: dt(:)   !! PK_DATE storage, scalar.
        type(parquet_time), allocatable :: tm(:)   !! PK_TIME storage, scalar.
        type(parquet_timestamp), allocatable :: ts(:)   !! PK_TIMESTAMP storage, scalar.
        integer(int32), allocatable :: i32v(:,:)   !! PK_INT32_VEC storage, vector (width, nrows).
        integer(int64), allocatable :: i64v(:,:)   !! PK_INT64_VEC storage, vector (width, nrows).
        real(real32), allocatable :: f32v(:,:)   !! PK_FLOAT32_VEC storage, vector (width, nrows).
        real(real64), allocatable :: f64v(:,:)   !! PK_FLOAT64_VEC storage, vector (width, nrows).
        logical, allocatable :: boolv(:,:)   !! PK_LOGICAL_VEC storage, vector (width, nrows).
        type(parquet_date), allocatable :: dtv(:,:)   !! PK_DATE_VEC storage, vector (width, nrows).
        type(parquet_time), allocatable :: tmv(:,:)   !! PK_TIME_VEC storage, vector (width, nrows).
        type(parquet_timestamp), allocatable :: tsv(:,:)   !! PK_TIMESTAMP_VEC storage, vector (width, nrows).
        !> PK_LIST/PK_MAP/PK_STRUCT storage, behind the abstract face declared above.
        !!
        !! Allocated only for a container kind, and only ever by `adopt_container` -- `init`
        !! refuses the container kinds outright, because "a hundred rows of a list column" has no
        !! meaning until the payload type is known and the caller already owns the object.
        class(parquet_container_column), allocatable :: container
    contains
        ! --- lifecycle ---
        procedure :: init                              !! Set kind/geometry and allocate empty storage.
        procedure :: clear                             !! Release all storage and reset to PK_NONE.
        procedure :: deep_copy                         !! Independent copy of values, validity and unit.
        procedure :: move_from                         !! Take over another column's storage, leaving it empty.
        procedure :: adopt_container                   !! Take ownership of a container column (PK_LIST/MAP/STRUCT).
        procedure :: adopt_string_column                !! Take ownership of a parquet_string_column (PK_STRING).
        ! --- queries ---
        procedure :: kindof                            !! The active PK_* discriminator.
        procedure :: length                            !! Number of rows stored.
        procedure :: capacity                          !! Rows the storage is allocated for (>= length()).
        procedure :: colwidth                          !! Values per row (1 for scalar kinds).
        procedure :: validity_bytes                    !! Bytes the null bitmap occupies (0 when sparse).
        procedure :: has_validity_storage               !! Whether nulling would still have to allocate.
        procedure :: ensure_validity                    !! Allocate the validity storage up front.
        procedure :: unit_string                       !! Copy out the unit string ("" when unset).
        procedure :: set_unit                          !! Set (or clear) the unit string.
        procedure :: any_null                          !! Whether the column holds at least one null.
        procedure, private :: is_null_row               !! is_null specific taking a row index alone.
        procedure, private :: is_null_elem              !! is_null specific taking a row and an element.
        !> Whether row `i` (or, with `e`, element `e` of row `i`) is null. On a vector kind the
        !! row form answers "ANY element of the row is null"; see the module doc.
        generic :: is_null => is_null_row, is_null_elem
        procedure :: row_validity                      !! Build the whole per-row validity mask at once.
        procedure :: row_validity_range                !! Fill a caller's mask for a range of rows.
        procedure :: element_validity                  !! Build the whole per-ELEMENT validity mask at once.
        ! --- validity mutation (sparse: see the module doc) ---
        procedure, private :: set_null_row              !! set_null specific taking a row index alone.
        procedure, private :: set_null_elem             !! set_null specific taking a row and an element.
        !> Marks row `i` null, or with `e` just element `e` of it.
        generic :: set_null => set_null_row, set_null_elem
        procedure, private :: clear_null_row            !! clear_null specific taking a row index alone.
        procedure, private :: clear_null_elem           !! clear_null specific taking a row and an element.
        !> Marks row `i` valid, or with `e` just element `e` of it.
        generic :: clear_null => clear_null_row, clear_null_elem
        procedure, private :: set_validity_elems       !! set_validity specific, per-ELEMENT (width, nrows) mask.
        procedure, private :: set_validity_rows        !! set_validity specific, per-ROW mask.
        !> Writes a whole validity mask in one pass. A rank-2 `(width, nrows)` mask is per ELEMENT;
        !! a rank-1 `(nrows)` mask is per ROW, marking every element of a `.false.` row null -- the
        !! same row/element pairing `%set_null` and `%is_null` already use.
        generic :: set_validity => set_validity_elems, set_validity_rows
        procedure :: compact_validity                  !! Drop the bitmap when no nulls remain.
        ! --- capacity ---
        procedure, private :: reserve_i32              !! int32 specific of reserve.
        procedure, private :: reserve_i64              !! int64 specific of reserve.
        !> Grows the storage capacity to hold at least `n` rows without changing the row count.
        generic :: reserve => reserve_i32, reserve_i64
        procedure :: shrink_to_fit                     !! Release capacity beyond the rows stored.
        ! --- structural mutation ---
        procedure :: append                            !! Append another column of identical kind/width.
        procedure :: append_row_of                     !! Append one row of another column (internal; see below).
        procedure :: append_nulls                      !! Append n all-null rows.
        procedure :: paste                             !! Overwrite an existing row range from another column.
        procedure :: delete_by_mask                    !! Keep only rows whose mask entry is .true.
        procedure :: reindex                           !! Reorder rows by a permutation.
        !> INTERNAL -- reindex without the duplicate/range scan, for a permutation the caller has
        !! already established is one. Public only because Fortran offers no narrower visibility;
        !! see the interface below.
        procedure :: reindex_trusted
        procedure, private :: gather_i32               !! int32 specific of gather.
        procedure, private :: gather_i64               !! int64 specific of gather.
        !> Keeps the listed rows, in the listed order. Unlike `reindex` the list may be any length,
        !! and unlike `delete_by_mask` it may reorder -- see the interface below.
        generic :: gather => gather_i32, gather_i64
        procedure, private :: gather_from_i32          !! int32 specific of gather_from.
        procedure, private :: gather_from_i64          !! int64 specific of gather_from.
        !> Builds this column from the listed rows of ANOTHER, in one pass: `deep_copy` + `gather`
        !! + `set_validity` as one operation, with the source untouched, an optional null mask
        !! folded in and an optional team -- see the interface below.
        generic :: gather_from => gather_from_i32, gather_from_i64
        ! --- string-kind storage access (PK_STRING / PK_STRING_VEC) ---
        procedure :: string_column                     !! Pointer to the embedded string store.
        ! --- container-kind storage access (PK_LIST / PK_MAP / PK_STRUCT) ---
        procedure :: container_ptr                     !! Pointer to the embedded container column.
        ! --- get_at ---
        procedure, private :: get_at_i32   !! get_at specific for the i32 kind.
        procedure, private :: get_at_i64   !! get_at specific for the i64 kind.
        procedure, private :: get_at_f32   !! get_at specific for the f32 kind.
        procedure, private :: get_at_f64   !! get_at specific for the f64 kind.
        procedure, private :: get_at_bool   !! get_at specific for the bool kind.
        procedure, private :: get_at_str   !! get_at specific for the str kind.
        procedure, private :: get_at_date   !! get_at specific for the date kind.
        procedure, private :: get_at_time   !! get_at specific for the time kind.
        procedure, private :: get_at_ts   !! get_at specific for the ts kind.
        procedure, private :: get_at_i32v   !! get_at specific for the i32v kind.
        procedure, private :: get_at_i64v   !! get_at specific for the i64v kind.
        procedure, private :: get_at_f32v   !! get_at specific for the f32v kind.
        procedure, private :: get_at_f64v   !! get_at specific for the f64v kind.
        procedure, private :: get_at_boolv   !! get_at specific for the boolv kind.
        procedure, private :: get_at_strv   !! get_at specific for the strv kind.
        procedure, private :: get_at_datev   !! get_at specific for the datev kind.
        procedure, private :: get_at_timev   !! get_at specific for the timev kind.
        procedure, private :: get_at_tsv   !! get_at specific for the tsv kind.
        !> Read element i (or row i's vector) out.
        generic :: get_at => get_at_i32, get_at_i64, get_at_f32, get_at_f64, get_at_bool, get_at_str, &
            get_at_date, get_at_time, get_at_ts, get_at_i32v, get_at_i64v, get_at_f32v, get_at_f64v, &
            get_at_boolv, get_at_strv, get_at_datev, get_at_timev, get_at_tsv
        ! --- set_at ---
        procedure, private :: set_at_i32   !! set_at specific for the i32 kind.
        procedure, private :: set_at_i64   !! set_at specific for the i64 kind.
        procedure, private :: set_at_f32   !! set_at specific for the f32 kind.
        procedure, private :: set_at_f64   !! set_at specific for the f64 kind.
        procedure, private :: set_at_bool   !! set_at specific for the bool kind.
        procedure, private :: set_at_str   !! set_at specific for the str kind.
        procedure, private :: set_at_date   !! set_at specific for the date kind.
        procedure, private :: set_at_time   !! set_at specific for the time kind.
        procedure, private :: set_at_ts   !! set_at specific for the ts kind.
        procedure, private :: set_at_i32v   !! set_at specific for the i32v kind.
        procedure, private :: set_at_i64v   !! set_at specific for the i64v kind.
        procedure, private :: set_at_f32v   !! set_at specific for the f32v kind.
        procedure, private :: set_at_f64v   !! set_at specific for the f64v kind.
        procedure, private :: set_at_boolv   !! set_at specific for the boolv kind.
        procedure, private :: set_at_strv   !! set_at specific for the strv kind.
        procedure, private :: set_at_datev   !! set_at specific for the datev kind.
        procedure, private :: set_at_timev   !! set_at specific for the timev kind.
        procedure, private :: set_at_tsv   !! set_at specific for the tsv kind.
        !> Write element i (or row i's vector).
        generic :: set_at => set_at_i32, set_at_i64, set_at_f32, set_at_f64, set_at_bool, set_at_str, &
            set_at_date, set_at_time, set_at_ts, set_at_i32v, set_at_i64v, set_at_f32v, set_at_f64v, &
            set_at_boolv, set_at_strv, set_at_datev, set_at_timev, set_at_tsv
        ! --- get_elem ---
        procedure, private :: get_elem_i32v   !! get_elem specific for the i32v kind.
        procedure, private :: get_elem_i64v   !! get_elem specific for the i64v kind.
        procedure, private :: get_elem_f32v   !! get_elem specific for the f32v kind.
        procedure, private :: get_elem_f64v   !! get_elem specific for the f64v kind.
        procedure, private :: get_elem_boolv   !! get_elem specific for the boolv kind.
        procedure, private :: get_elem_strv   !! get_elem specific for the strv kind.
        procedure, private :: get_elem_datev   !! get_elem specific for the datev kind.
        procedure, private :: get_elem_timev   !! get_elem specific for the timev kind.
        procedure, private :: get_elem_tsv   !! get_elem specific for the tsv kind.
        !> Read ONE element of row i's vector, without materialising the row.
        generic :: get_elem => get_elem_i32v, get_elem_i64v, get_elem_f32v, get_elem_f64v, get_elem_boolv, &
            get_elem_strv, get_elem_datev, get_elem_timev, get_elem_tsv
        ! --- set_elem ---
        procedure, private :: set_elem_i32v   !! set_elem specific for the i32v kind.
        procedure, private :: set_elem_i64v   !! set_elem specific for the i64v kind.
        procedure, private :: set_elem_f32v   !! set_elem specific for the f32v kind.
        procedure, private :: set_elem_f64v   !! set_elem specific for the f64v kind.
        procedure, private :: set_elem_boolv   !! set_elem specific for the boolv kind.
        procedure, private :: set_elem_strv   !! set_elem specific for the strv kind.
        procedure, private :: set_elem_datev   !! set_elem specific for the datev kind.
        procedure, private :: set_elem_timev   !! set_elem specific for the timev kind.
        procedure, private :: set_elem_tsv   !! set_elem specific for the tsv kind.
        !> Write ONE element of row i's vector, without materialising the row.
        generic :: set_elem => set_elem_i32v, set_elem_i64v, set_elem_f32v, set_elem_f64v, set_elem_boolv, &
            set_elem_strv, set_elem_datev, set_elem_timev, set_elem_tsv
        ! --- set_all ---
        procedure, private :: set_all_i32   !! set_all specific for the i32 kind.
        procedure, private :: set_all_i64   !! set_all specific for the i64 kind.
        procedure, private :: set_all_f32   !! set_all specific for the f32 kind.
        procedure, private :: set_all_f64   !! set_all specific for the f64 kind.
        procedure, private :: set_all_bool   !! set_all specific for the bool kind.
        procedure, private :: set_all_str   !! set_all specific for the str kind.
        procedure, private :: set_all_date   !! set_all specific for the date kind.
        procedure, private :: set_all_time   !! set_all specific for the time kind.
        procedure, private :: set_all_ts   !! set_all specific for the ts kind.
        procedure, private :: set_all_i32v   !! set_all specific for the i32v kind.
        procedure, private :: set_all_i64v   !! set_all specific for the i64v kind.
        procedure, private :: set_all_f32v   !! set_all specific for the f32v kind.
        procedure, private :: set_all_f64v   !! set_all specific for the f64v kind.
        procedure, private :: set_all_boolv   !! set_all specific for the boolv kind.
        procedure, private :: set_all_strv   !! set_all specific for the strv kind.
        procedure, private :: set_all_datev   !! set_all specific for the datev kind.
        procedure, private :: set_all_timev   !! set_all specific for the timev kind.
        procedure, private :: set_all_tsv   !! set_all specific for the tsv kind.
        !> Replace every value in the column.
        generic :: set_all => set_all_i32, set_all_i64, set_all_f32, set_all_f64, set_all_bool, set_all_str, &
            set_all_date, set_all_time, set_all_ts, set_all_i32v, set_all_i64v, set_all_f32v, set_all_f64v, &
            set_all_boolv, set_all_strv, set_all_datev, set_all_timev, set_all_tsv
        ! --- adopt ---
        procedure, private :: adopt_i32   !! adopt specific for the i32 kind.
        procedure, private :: adopt_i64   !! adopt specific for the i64 kind.
        procedure, private :: adopt_f32   !! adopt specific for the f32 kind.
        procedure, private :: adopt_f64   !! adopt specific for the f64 kind.
        procedure, private :: adopt_bool   !! adopt specific for the bool kind.
        procedure, private :: adopt_date   !! adopt specific for the date kind.
        procedure, private :: adopt_time   !! adopt specific for the time kind.
        procedure, private :: adopt_ts   !! adopt specific for the ts kind.
        procedure, private :: adopt_i32v   !! adopt specific for the i32v kind.
        procedure, private :: adopt_i64v   !! adopt specific for the i64v kind.
        procedure, private :: adopt_f32v   !! adopt specific for the f32v kind.
        procedure, private :: adopt_f64v   !! adopt specific for the f64v kind.
        procedure, private :: adopt_boolv   !! adopt specific for the boolv kind.
        procedure, private :: adopt_datev   !! adopt specific for the datev kind.
        procedure, private :: adopt_timev   !! adopt specific for the timev kind.
        procedure, private :: adopt_tsv   !! adopt specific for the tsv kind.
        !> Take ownership of an array outright, without copying it.
        generic :: adopt => adopt_i32, adopt_i64, adopt_f32, adopt_f64, adopt_bool, adopt_date, adopt_time, &
            adopt_ts, adopt_i32v, adopt_i64v, adopt_f32v, adopt_f64v, adopt_boolv, adopt_datev, adopt_timev, &
            adopt_tsv
        ! --- data_ptr ---
        procedure, private :: data_ptr_i32   !! data_ptr specific for the i32 kind.
        procedure, private :: data_ptr_i64   !! data_ptr specific for the i64 kind.
        procedure, private :: data_ptr_f32   !! data_ptr specific for the f32 kind.
        procedure, private :: data_ptr_f64   !! data_ptr specific for the f64 kind.
        procedure, private :: data_ptr_bool   !! data_ptr specific for the bool kind.
        procedure, private :: data_ptr_date   !! data_ptr specific for the date kind.
        procedure, private :: data_ptr_time   !! data_ptr specific for the time kind.
        procedure, private :: data_ptr_ts   !! data_ptr specific for the ts kind.
        procedure, private :: data_ptr_i32v   !! data_ptr specific for the i32v kind.
        procedure, private :: data_ptr_i64v   !! data_ptr specific for the i64v kind.
        procedure, private :: data_ptr_f32v   !! data_ptr specific for the f32v kind.
        procedure, private :: data_ptr_f64v   !! data_ptr specific for the f64v kind.
        procedure, private :: data_ptr_boolv   !! data_ptr specific for the boolv kind.
        procedure, private :: data_ptr_datev   !! data_ptr specific for the datev kind.
        procedure, private :: data_ptr_timev   !! data_ptr specific for the timev kind.
        procedure, private :: data_ptr_tsv   !! data_ptr specific for the tsv kind.
        !> Zero-copy typed pointer to the active storage.
        generic :: data_ptr => data_ptr_i32, data_ptr_i64, data_ptr_f32, data_ptr_f64, data_ptr_bool, &
            data_ptr_date, data_ptr_time, data_ptr_ts, data_ptr_i32v, data_ptr_i64v, data_ptr_f32v, &
            data_ptr_f64v, data_ptr_boolv, data_ptr_datev, data_ptr_timev, data_ptr_tsv
        ! --- append_values ---
        procedure, private :: append_values_i32   !! append_values specific for the i32 kind.
        procedure, private :: append_values_i64   !! append_values specific for the i64 kind.
        procedure, private :: append_values_f32   !! append_values specific for the f32 kind.
        procedure, private :: append_values_f64   !! append_values specific for the f64 kind.
        procedure, private :: append_values_bool   !! append_values specific for the bool kind.
        procedure, private :: append_values_str   !! append_values specific for the str kind.
        procedure, private :: append_values_date   !! append_values specific for the date kind.
        procedure, private :: append_values_time   !! append_values specific for the time kind.
        procedure, private :: append_values_ts   !! append_values specific for the ts kind.
        procedure, private :: append_values_i32v   !! append_values specific for the i32v kind.
        procedure, private :: append_values_i64v   !! append_values specific for the i64v kind.
        procedure, private :: append_values_f32v   !! append_values specific for the f32v kind.
        procedure, private :: append_values_f64v   !! append_values specific for the f64v kind.
        procedure, private :: append_values_boolv   !! append_values specific for the boolv kind.
        procedure, private :: append_values_strv   !! append_values specific for the strv kind.
        procedure, private :: append_values_datev   !! append_values specific for the datev kind.
        procedure, private :: append_values_timev   !! append_values specific for the timev kind.
        procedure, private :: append_values_tsv   !! append_values specific for the tsv kind.
        !> Append values, growing the column.
        generic :: append_values => append_values_i32, append_values_i64, append_values_f32, &
            append_values_f64, append_values_bool, append_values_str, append_values_date, append_values_time, &
            append_values_ts, append_values_i32v, append_values_i64v, append_values_f32v, append_values_f64v, &
            append_values_boolv, append_values_strv, append_values_datev, append_values_timev, &
            append_values_tsv
    end type parquet_column
    !
    ! ---- Lifecycle, queries and unit (parquet_columns_structural) ----
    interface
        !> Sets the column's kind and geometry and allocates empty storage for that kind.
        !! Any previous contents are released first. `width` is required (and must be > 1) for a
        !! *_VEC kind and must be 1 (or absent) otherwise. `nrows` rows are allocated; their values
        !! are unspecified for numeric kinds and null for temporal/string kinds.
        module subroutine init(self, kind, nrows, width, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            integer, intent(in) :: kind                  !! PK_* discriminator to activate.
            integer(int64), intent(in) :: nrows          !! initial row count (>= 0).
            integer(int32), intent(in), optional :: width !! values per row for a *_VEC kind.
            character(len=*), intent(in), optional :: unit !! unit string to store (D10).
        end subroutine init
        !> Releases every storage array and resets the column to PK_NONE with zero rows.
        module subroutine clear(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine clear
        !> Produces an independent copy: values, validity, unit and geometry. Mutating either
        !! column afterwards leaves the other unchanged.
        module subroutine deep_copy(self, out)
            class(parquet_column), intent(in) :: self       !! the source column.
            type(parquet_column), intent(out) :: out        !! the copy.
        end subroutine deep_copy
        !> Takes over `src`'s storage outright, leaving `src` empty (PK_NONE, zero rows).
        !!
        !! The move counterpart of `deep_copy`: every allocatable component is handed over with
        !! `move_alloc` rather than copied, so relocating a column costs a handful of descriptor
        !! assignments instead of a full copy of its data. Use it wherever a column is being
        !! RELOCATED rather than duplicated -- `%drop_column` shifting the tail of a table's slot
        !! array down over the dropped one is the case that motivated it, where intrinsic
        !! assignment memcpy'd every remaining column's values.
        !!
        !! `self` is cleared first, so whatever it held is released.
        module subroutine move_from(self, src)
            class(parquet_column), intent(inout) :: self    !! the column receiving the storage.
            type(parquet_column), intent(inout) :: src      !! the column giving it up; left empty.
        end subroutine move_from
        !> Takes ownership of a container column, making this a `PK_LIST`/`PK_MAP`/`PK_STRUCT`
        !! column. The seventeenth sibling of the sixteen `adopt_*` array specifics, and the ONLY
        !! way a container kind enters a `parquet_column`.
        !!
        !! `init` deliberately still refuses the container kinds: it allocates the kind's storage
        !! from a row count, and `init(PK_LIST, 100)` would have to mean "a hundred rows of what?".
        !! A container's contents come from an object the caller has already built, so the entry
        !! point is `adopt`, not `init` -- and, exactly like the array `adopt_*` specifics, it
        !! MOVES rather than copies: `container` is left deallocated and the caller must not use it
        !! again.
        !!
        !! The column's kind and row count are taken from the container itself (`%kindof()`,
        !! `%nrows()`), so they cannot disagree with what was adopted. Width is 1: a container row
        !! holds a variable number of elements, which is precisely what `width` cannot express, and
        !! reporting anything else would make a `*_VEC` guard elsewhere believe it could stride.
        module subroutine adopt_container(self, container)
            class(parquet_column), intent(inout) :: self                     !! the column.
            class(parquet_container_column), allocatable, intent(inout) :: container !! moved in; left empty.
        end subroutine adopt_container
        !> Takes ownership of a `parquet_string_column`, making this a `PK_STRING` column. The
        !! eighteenth sibling of the sixteen array `adopt_*` specifics and of `adopt_container`,
        !! and the only one a string kind has.
        !!
        !! It exists because a `PK_STRING` column's storage is a packed variable-length store
        !! rather than an array, so there is no array for the `adopt` generic to take -- and
        !! without this, a caller who has built such a store (from a file's own buffers, say) had
        !! no way to hand it over that also settled the column's row count. Reaching the store
        !! through `parquet_column_string_column` and appending to it directly does NOT: the
        !! column keeps its own `nrows`, which then silently disagrees with what the store holds.
        !!
        !! MOVES rather than copies: `values` is left empty and must not be used again. Kind
        !! (`PK_STRING`), width (1) and row count are settled here from `values` itself, so this
        !! replaces `init` rather than following it.
        module subroutine adopt_string_column(self, values)
            class(parquet_column), intent(inout) :: self                !! the column.
            type(parquet_string_column), intent(inout) :: values        !! moved in; left empty.
        end subroutine adopt_string_column
        !> Copies the unit string out ("" when no unit is set).
        module subroutine unit_string(self, u)
            class(parquet_column), intent(in) :: self          !! the column.
            character(len=:), allocatable, intent(out) :: u    !! the unit string, or "".
        end subroutine unit_string
        !> Sets the unit string; passing "" clears it.
        module subroutine set_unit(self, u)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: u            !! the unit string ("" clears).
        end subroutine set_unit
        !> int32 form of `reserve`; converts and delegates.
        module subroutine reserve_i32(self, n)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), intent(in) :: n              !! rows to make room for.
        end subroutine reserve_i32
        !> Grows the storage capacity to hold at least `n` rows, without changing the row count or
        !! any value. A no-op when the capacity is already sufficient.
        !!
        !! The point of it is that the `n - length()` appends that follow perform **no** allocation
        !! at all, turning even the amortised geometric growth into a single up-front one. Pair it
        !! with `%shrink_to_fit` (reserve, fill, shrink) when the final row count is known ahead of
        !! time.
        !!
        !! **It does not allocate exactly `n`.** Capacity goes to `max(n, 1.5*capacity())`, the same
        !! rule an append follows, so reserving slightly more than the column already holds does not
        !! shrink the allocation -- `%shrink_to_fit` is what does that. This matches
        !! `parquet_string_column%reserve`, which shares the same growth primitive.
        !!
        !! Invalidates any pointer previously obtained from `%data_ptr` if it actually reallocates.
        module subroutine reserve_i64(self, n)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: n              !! rows to make room for.
        end subroutine reserve_i64
        !> Releases capacity beyond the rows actually stored, so the column occupies exactly what it
        !! holds. A no-op when there is no slack.
        !!
        !! Slack only ever comes from an append: every rebuild (`reindex`, `gather`,
        !! `delete_by_mask`) allocates exact-fit, and so does reading a column from a file. So on a
        !! column that has not been appended to this does nothing at all.
        !!
        !! Invalidates any pointer previously obtained from `%data_ptr` if it actually reallocates.
        !!
        !! `released` reports whether it did. The caller cannot work that out for itself without
        !! knowing which of the three storage mechanisms the kind uses -- a string column's spare
        !! capacity lives in its embedded store's offsets AND its byte payload, neither of which is
        !! `cap` -- so the answer is produced here, where the kind is known. `parquet_table%compact`
        !! needs it to decide whether to advance `%generation()`.
        module subroutine shrink_to_fit(self, released)
            class(parquet_column), intent(inout) :: self  !! the column.
            logical, intent(out), optional :: released    !! .true. if anything was reallocated.
        end subroutine shrink_to_fit
        !> Appends every row of `other`, which must have identical kind and width.
        module subroutine append(self, other)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: other    !! the source column (unchanged).
        end subroutine append
        !> Appends row `irow` of `other` as this column's next row, carrying that row's validity.
        !!
        !! **INTERNAL plumbing, public only because Fortran offers no narrower visibility** --
        !! `parquet_tables` is a different module and cannot reach `parquet_column`'s private
        !! components, and `%append(row)` needs exactly this to append one row without building a
        !! one-row column and a one-row table first. Same reasoning as `reindex_trusted`, and it is
        !! likewise absent from README.md's API overview.
        !!
        !! Kind and width are checked on every call even though the table layer has already
        !! validated them: two integer comparisons against a call that copies a whole row, on a
        !! procedure a caller who validated nothing can still reach.
        module subroutine append_row_of(self, other, irow)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: other    !! the source column (unchanged).
            integer(int64), intent(in) :: irow           !! 1-based row of `other` to append.
        end subroutine append_row_of
        !> Appends `n` all-null rows (allocating the bitmap if this is the first null).
        module subroutine append_nulls(self, n)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: n              !! number of null rows to append (>= 0).
        end subroutine append_nulls
        !> Overwrites the already-allocated rows `at .. at+count-1` with `count` rows of `src`,
        !! starting at `src` row `from`. Kind and width must match; nothing is reallocated and
        !! `nrows` does not change.
        !!
        !! This is the counterpart of `append` for a column whose final row count is known up
        !! front: `init` it once at full size, then paste each piece into place. `append` is not
        !! slow -- it grows geometrically through `ensure_capacity`, so appending k pieces is
        !! amortised O(1) per row, not O(k^2) -- but it copies each piece into a column that may
        !! reallocate under it, and it can only ever extend the end. `%paste` allocates once,
        !! copies each piece exactly once into its final position, and is the only form that lets
        !! independent pieces be written OUT OF ORDER and CONCURRENTLY, which is what
        !! `parquet_table`'s slice regime uses it for (see `paste_row_group_safely`).
        !!
        !! `from`/`count` default to 1 and `src%nrows`, i.e. all of `src`. Passing them copies a
        !! sub-range directly, so a caller trimming a piece to a row window does not need to build
        !! a mask and call `delete_by_mask` first.
        !!
        !! Validity follows the same kind-dispatched rules as `append`: bitmap-backed kinds copy
        !! `src`'s null bits into the destination positions (materializing this column's bitmap
        !! only if `src` actually has nulls), and the temporal kinds carry their null state inside
        !! the pasted elements. **The string kinds are not supported** and abort: their store is
        !! variable-length, so a row range cannot be overwritten in place -- and they already grow
        !! geometrically under `append`, so there is nothing to gain (see `parquet_string_column`).
        module subroutine paste(self, src, at, from, count)
            class(parquet_column), intent(inout) :: self       !! the destination column.
            type(parquet_column), intent(in) :: src            !! the source column (unchanged).
            integer(int64), intent(in) :: at                   !! 1-based first destination row.
            integer(int64), intent(in), optional :: from       !! 1-based first source row (default 1).
            integer(int64), intent(in), optional :: count      !! rows to copy (default all of `src`).
        end subroutine paste
        !> Keeps only the rows whose `keep` entry is .true., preserving order.
        module subroutine delete_by_mask(self, keep)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: keep(:)               !! .true. for every row to retain.
        end subroutine delete_by_mask
        !> Reorders rows so row k of the result is the row that was at `perm(k)`. `perm` must be
        !! a true permutation of 1..nrows and is fully validated before anything is modified.
        module subroutine reindex(self, perm)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: perm(:)        !! 1-based permutation of 1..nrows.
        end subroutine reindex
        !> **INTERNAL.** `reindex` without the O(nrows) range/duplicate scan, for a permutation the
        !! caller has already established is one. The O(1) length check still runs, since it guards
        !! a different invariant and costs nothing.
        !!
        !! Public only because Fortran has no narrower visibility: `parquet_column`'s components are
        !! private to this module, so `parquet_tables` cannot reach them, and `%sort_by` needs this
        !! to stop re-validating one permutation once per column -- measured at a third of its total
        !! time on a 24-column table. Deliberately absent from README.md's API overview. A caller who
        !! passes a non-permutation gets silently duplicated and dropped rows; a library can refuse
        !! accidents, not deliberate misuse of something documented as internal.
        module subroutine reindex_trusted(self, perm)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: perm(:)        !! 1-based permutation of 1..nrows, unchecked.
        end subroutine reindex_trusted
        !> int32 form of `gather` -- see the int64 form below, which does the work.
        module subroutine gather_i32(self, idx, valid, threads)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), intent(in) :: idx(:)         !! 1-based source row per destination row.
            logical, intent(in), optional :: valid(:)    !! per destination row; .false. nulls it (add-only).
            integer, intent(in), optional :: threads     !! team to split the rows across; absent or 1 is serial.
        end subroutine gather_i32
        !> Keeps the rows `idx` lists, in the order it lists them: row `k` of the result is the row
        !! that was at `idx(k)`, and the column ends up `size(idx)` rows long.
        !!
        !! This is the subset-and-reorder primitive the other two do not provide between them:
        !! `reindex` demands a permutation of the whole column, and `delete_by_mask` keeps the
        !! existing order. It is what makes `parquet_table%top_n` cost O(n) per column rather than
        !! O(nrows) -- a keep mask would make every column pay for a full scan to keep a handful of
        !! rows.
        !!
        !! **It is a gather, not a permutation.** Any row in 1..nrows, in any order, and a row may be
        !! named more than once, so the result may be shorter than, as long as, or longer than the
        !! column it replaces. Only the range is checked: refusing repeats would need a seen-set
        !! sized by the row count on every call, which is the very cost this exists to avoid, and a
        !! caller that needs distinctness can check it once for itself.
        !!
        !! Validity travels with the rows it belongs to, per element for a vector column. `valid`,
        !! when present, has one entry per DESTINATION row and marks every element of a `.false.`
        !! row null on top of whatever the source row carried -- `set_validity`'s add-only rule,
        !! applied in the same pass rather than afterwards. `threads` above 1 splits the rows
        !! across a team. Both are `gather_from`'s, which does the work: since stage 4 of
        !! `feature_join.md` this is a `gather_from` of the column's own rows into a fresh column,
        !! handed back over the original in O(1). A container column keeps its own route (the
        !! container rebuilds itself) and takes no mask.
        module subroutine gather_i64(self, idx, valid, threads)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: idx(:)         !! 1-based source row per destination row.
            logical, intent(in), optional :: valid(:)    !! per destination row; .false. nulls it (add-only).
            integer, intent(in), optional :: threads     !! team to split the rows across; absent or 1 is serial.
        end subroutine gather_i64
        !> int32 form of `gather_from` -- see the int64 form below, which does the work.
        module subroutine gather_from_i32(self, src, idx, valid, threads)
            class(parquet_column), intent(inout) :: self !! the destination; cleared first.
            type(parquet_column), intent(in) :: src      !! the source column, never written.
            integer(int32), intent(in) :: idx(:)         !! 1-based source row per destination row.
            logical, intent(in), optional :: valid(:)    !! per destination row; .false. nulls it (add-only).
            integer, intent(in), optional :: threads     !! team to split the rows across; absent or 1 is serial.
        end subroutine gather_from_i32
        !> Builds this column from the rows of `src` that `idx` lists, in the order it lists them,
        !! in ONE pass: row `k` of the result is `src`'s row `idx(k)`, with `src`'s kind, width and
        !! unit, `src`'s own nulls carried with their rows, and -- when `valid` is present -- every
        !! element of a row whose entry is `.false.` null as well. The destination is cleared first
        !! and allocated exact-fit at `size(idx)` rows; `src` is never written, and must be a
        !! different column from `self` (a column's own rows are gathered with `gather`).
        !!
        !! **This is `deep_copy` + `gather` + `set_validity` as one operation**, and it exists for
        !! `parquet_table%join`, which carries every incoming column this way: the three-call form
        !! copies the whole source and then rebuilds the copy, holding two transient copies where
        !! this holds one, and walks the rows three times where this walks them once. Everything
        !! the three calls promise holds here by construction. In particular the mask only ever
        !! ADDS nulls: a `.true.` entry leaves the row exactly as the source had it, so a mask may
        !! describe the unmatched rows alone and the source's own nulls survive untouched -- the
        !! add-only rule of `feature_risks.md` Risk-182, of which this is the second home.
        !!
        !! **`idx` is a gather, not a permutation**, on `gather`'s terms: any row of `src`, in any
        !! order, a row named more than once, a list shorter or longer than the source; only the
        !! range is checked. `valid` must have exactly `size(idx)` entries. A container column
        !! cannot be a source -- it is gathered in place with `gather`, which is how a join carries
        !! one (`feature_risks.md` Risk-188) -- and a source with no rows is gathered only by an
        !! empty list, since there is no row for `idx` to name.
        !!
        !! **`threads` above 1 splits the destination rows across a team.** Each thread copies its
        !! own range of rows and writes its own whole words of the validity bitmap: the ranges are
        !! cut on bitmap-word boundaries, so no two threads ever touch one word and no ragged end
        !! needs serialising (`feature_risks.md` Risk-64's rule, met by construction). The count is
        !! taken as given -- bounded by the affinity mask and by the row count -- and there is no
        !! automatic answer here, because whether to spend a team inside one column or across
        !! several is a decision for the caller that can see the other columns (`table_colwork`,
        !! `src/parquet_tables_parallel.f90`). Absent, or 1, the copy is serial. A string column
        !! hands the count to its own store, whose payload floor and break-even still apply.
        module subroutine gather_from_i64(self, src, idx, valid, threads)
            class(parquet_column), intent(inout) :: self !! the destination; cleared first.
            type(parquet_column), intent(in) :: src      !! the source column, never written.
            integer(int64), intent(in) :: idx(:)         !! 1-based source row per destination row.
            logical, intent(in), optional :: valid(:)    !! per destination row; .false. nulls it (add-only).
            integer, intent(in), optional :: threads     !! team to split the rows across; absent or 1 is serial.
        end subroutine gather_from_i64
        !> The team `gather`/`gather_from` would actually open for `nrows` destination rows of a
        !! column `width` elements wide when asked for `threads`: the request clamped to the
        !! affinity mask and lowered to the number of whole validity-word periods the rows hold,
        !! the one rule the gather itself applies. **Test-only**, and public for the reason
        !! `parquet_debug_string_bulk_threads` is: this tier reaches no C++ hook, and a test that
        !! re-derived the rule would assert against its own copy. No library code calls it.
        module function parquet_debug_column_gather_threads(nrows, width, threads) result(n)
            integer(int64), intent(in) :: nrows !! destination rows.
            integer(int32), intent(in) :: width !! elements per row.
            integer, intent(in) :: threads      !! the count asked for.
            integer :: n                        !! the team that would open; 1 for serial.
        end function parquet_debug_column_gather_threads
    end interface
    !
    ! ---- Validity, kind-dispatched (parquet_columns_validity) ----
    interface
        !> Whether the column holds at least one null. Cheap for bitmap-backed and string kinds;
        !! for temporal kinds (whose null state lives inside each element) the answer is cached,
        !! and a rescan is needed after a mutation -- which is why `self` is `intent(inout)`
        !! despite this being a query.
        !!
        !! **That `intent(inout)` is real: this WRITES to the column, so it must not be called on
        !! one another thread can reach.** The temporal rescan ends by clearing the dirty flag,
        !! which silently discards a `set_null` that another thread raised during the scan -- and
        !! the column then reports "no nulls" for good, with nothing to notice. Being reached
        !! through the table's `cache` POINTER is what makes that possible from a table accessor
        !! declared `intent(in)`, so the compiler cannot object either. Anything on a read path
        !! must call `parquet_column_any_null` below instead; `%has_nulls` learned this the hard
        !! way. See feature_risks.md Risk-136.
        module function any_null(self) result(res)
            class(parquet_column), intent(inout) :: self !! the column (null cache may be refreshed).
            logical :: res                               !! .true. when at least one row is null.
        end function any_null
        !> Whether the column holds at least one null, WITHOUT writing to it -- the read path's
        !! form of `any_null`, and the one every shared reader must use.
        !!
        !! Identical answer, and identical cost on every kind but one. A temporal column's cache
        !! is read when it is clean and simply re-scanned when it is dirty, rather than being
        !! refreshed: the memoisation is an optimisation for `any_null`, never part of the answer,
        !! so giving it up costs a dirty column one O(n) walk per call and buys the property that
        !! any number of threads may ask at once.
        !!
        !! `type(parquet_column)`, not `class`, for the reason the per-cell tier above records --
        !! and a `class` actual may be passed to it freely, which is how the two bulk validity
        !! walks in this module reach it while holding `self` by `intent(in)`.
        module function parquet_column_any_null(col) result(res)
            type(parquet_column), intent(in) :: col !! the column.
            logical :: res                          !! .true. when at least one row is null.
        end function parquet_column_any_null
        !> Whether row `i` is null. On a *_VEC kind that means **any element** of the row is null.
        !!
        !! The row forms of the validity API are deliberately asymmetric, and the asymmetry is the
        !! useful one: a QUERY answers about the row as a whole ("is anything here missing?"),
        !! while a whole-row MUTATION (`set_null(i)`/`clear_null(i)`) acts on every element. Use
        !! the `(i, e)` forms whenever a single element is what is meant.
        module function is_null_row(self, i) result(res)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            logical :: res                            !! .true. when any element of the row is null.
        end function is_null_row
        !> Whether element `e` of row `i` is null.
        !!
        !! Defined for every kind, scalar included -- on a scalar column `width` is 1, so `e` can
        !! only be 1 and the answer equals the row form. That is deliberate: generic code (the
        !! generated table accessors, a caller's own loop) can use one shape for both without
        !! branching on the kind.
        module function is_null_elem(self, i, e) result(res)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            logical :: res                            !! .true. when that element is null.
        end function is_null_elem
        !> Fills `valid` with one entry per row: `.true.` where the row is not null.
        !!
        !! The bulk counterpart of `is_null`, and worth having as its own entry point rather than
        !! a loop at the call site for two reasons. It is inside the module that owns the bitmap,
        !! so it can walk that bitmap a 64-bit word at a time and skip whole runs of valid rows,
        !! where an outside loop can only ask one row at a time through a call the compiler cannot
        !! inline. And a null-free column returns an UNALLOCATED `valid` -- which, passed on to an
        !! `optional` dummy such as `parquet_write_column`'s `is_valid=`, counts as an absent
        !! argument (F2018 15.5.2.12), so the common no-nulls case costs no allocation and no scan
        !! at all rather than a mask that is uniformly `.true.`. Callers must therefore test
        !! `allocated(valid)` and not assume a mask came back.
        !! **`intent(in)`, deliberately.** It used to be `intent(inout)` so that the temporal kinds'
        !! null cache could be refreshed in passing, and that single word made the whole procedure
        !! unreachable from anything holding a column by `intent(in)` -- which includes
        !! `pf_argsort(column)` and every other read-only consumer, each of which was left calling
        !! `is_null(i)` once per row instead. The cache is an optimisation for `any_null`, never a
        !! correctness requirement, so this reads it when it is clean and scans when it is dirty
        !! rather than writing it. Widening an argument's intent this way is source-compatible:
        !! every call that compiled before still does.
        module subroutine row_validity(self, valid)
            class(parquet_column), intent(in) :: self     !! the column.
            logical, allocatable, intent(out) :: valid(:) !! per-row mask, or unallocated when no nulls.
        end subroutine row_validity
        !> Fills `valid(1:last-first+1)` for rows `first..last`: `.true.` where the row is not null,
        !! with the row form's "any element null" rule on a vector kind.
        !!
        !! The ranged, NON-allocating form of `row_validity`, for a caller walking a column a block
        !! at a time and wanting a mask the size of its block rather than one the size of the column
        !! (`%print_stat`'s statistics scan, whose mask is a few kilobytes on the stack whatever the
        !! row count). The bitmap words the range touches are walked as `row_validity` walks the
        !! whole bitmap -- a zero word is skipped, a nonzero one costs its set bits -- and a column
        !! with no nulls costs one flag test. `valid` must have at least `last-first+1` entries;
        !! entries beyond that are left as they were. Aborts on a range outside `1..length()`.
        !!
        !! `intent(in)` on the column, for the reason `row_validity` is -- see its note.
        module subroutine row_validity_range(self, first, last, valid)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: first       !! first row of the range, 1-based.
            integer(int64), intent(in) :: last        !! last row of the range, inclusive.
            logical, intent(inout) :: valid(:)        !! receives one entry per row of the range.
        end subroutine row_validity_range
        !> Typed form of `row_validity_range`, for `parquet_tables` -- the `type` twin the per-cell
        !! tier above explains, and the one `%print_stat`'s scan calls per block.
        module subroutine parquet_column_row_validity_range(col, first, last, valid)
            type(parquet_column), intent(in) :: col  !! the column.
            integer(int64), intent(in) :: first      !! first row of the range, 1-based.
            integer(int64), intent(in) :: last       !! last row of the range, inclusive.
            logical, intent(inout) :: valid(:)       !! receives one entry per row of the range.
        end subroutine parquet_column_row_validity_range
        !> Fills `valid` with one entry per ELEMENT, shaped `(width, nrows)`: the column's true
        !! validity state, without the row summary `row_validity` applies.
        !!
        !! Same contract as `row_validity` in every other respect, and for the same reasons: a
        !! null-free column returns an UNALLOCATED `valid` (so it reaches an `optional` dummy as an
        !! absent argument and costs nothing), and the bitmap is walked a 64-bit word at a time
        !! rather than a call per element. This is the shape `parquet_write_column` takes for a
        !! vector column, so a table write can hand it straight on.
        !!
        !! **Note the memory**: `LOGICAL` is 4 bytes under gfortran, so this is `4*width*nrows`
        !! bytes -- for a wide column, orders of magnitude more than the bitmap it is built from.
        !! Ask for it when the whole mask is genuinely needed; use `is_null(i, e)` for a few
        !! elements.
        !!
        !! `intent(in)` for the same reason `row_validity` is -- see its note.
        module subroutine element_validity(self, valid)
            class(parquet_column), intent(in) :: self       !! the column.
            logical, allocatable, intent(out) :: valid(:,:) !! (element, row) mask, or unallocated when no nulls.
        end subroutine element_validity
        !> Writes a whole per-ELEMENT validity mask in one pass: `.false.` marks that element null.
        !!
        !! The bulk counterpart of `set_null(i, e)`, and the reason the read path can stop widening
        !! without paying for it: replaying a mask element by element would cost `width*nrows`
        !! type-bound calls, where this writes the bitmap directly. `valid` must be shaped
        !! `(width, nrows)` exactly.
        !!
        !! Only ever ADDS nulls -- an element whose entry is `.true.` is left exactly as it is, so
        !! this composes with a mask describing only part of what the caller knows, and never
        !! resurrects a value that was already null.
        module subroutine set_validity_elems(self, valid)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: valid(:,:)            !! (element, row); .false. marks that element null.
        end subroutine set_validity_elems
        !> Writes a whole per-ROW validity mask in one pass: `.false.` marks every element of that
        !! row null, exactly as `set_null(i)` does for one row.
        !!
        !! The rank-1 form of `set_validity`, and the bulk counterpart of `set_null(i)`. It exists
        !! because replaying a row mask with one type-bound call per null row is what every
        !! materializer and every `%set(..., is_valid=)` was doing; this writes the bitmap a word at
        !! a time instead. `valid` must have exactly `nrows` entries.
        !!
        !! Only ever ADDS nulls, on the same terms as the rank-2 form -- and `parquet_table%join`
        !! DEPENDS on that, one module away and invisible from here: it hands in a mask describing
        !! the unmatched output rows alone and relies on the incoming column's own nulls, carried
        !! across by the gather, surviving untouched. Making this assign rather than add would
        !! leave a join silently returning values for rows the source file said were null
        !! (`feature_risks.md` Risk-182).
        module subroutine set_validity_rows(self, valid)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: valid(:)              !! one entry per row; .false. marks the row null.
        end subroutine set_validity_rows
        !> Marks every element of row `i` null, allocating the bitmap on first use (R2).
        module subroutine set_null_row(self, i)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
        end subroutine set_null_row
        !> Marks element `e` of row `i` null, allocating the bitmap on first use (R2).
        module subroutine set_null_elem(self, i, e)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
        end subroutine set_null_elem
        !> Marks every element of row `i` valid. The values behind them are unspecified until written.
        module subroutine clear_null_row(self, i)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
        end subroutine clear_null_row
        !> Marks element `e` of row `i` valid. The value behind it is unspecified until written.
        module subroutine clear_null_elem(self, i, e)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
        end subroutine clear_null_elem
        !> Scans for remaining nulls and releases the bitmap when none are found (R2 iv).
        module subroutine compact_validity(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine compact_validity
    end interface
    !
    ! ---- String kinds, delegating to parquet_string_column (parquet_columns_string) ----
    interface
        !> Pointer to the embedded string store (PK_STRING / PK_STRING_VEC only). The column must
        !! outlive the pointer; any structural mutation invalidates it.
        module subroutine string_column(self, p)
            class(parquet_column), intent(in), target :: self       !! the column.
            type(parquet_string_column), pointer, intent(out) :: p  !! alias to the string store.
        end subroutine string_column
        !> Aliases the embedded container column (PK_LIST/PK_MAP/PK_STRUCT).
        !!
        !! A one-line forwarder onto `parquet_column_container`, which holds the implementation --
        !! never the other way round. See that procedure's own note for why the result is the
        !! abstract face and why this pair is outside the per-cell typed-accessor rule.
        module subroutine container_ptr(self, p)
            class(parquet_column), intent(in), target :: self           !! the column.
            class(parquet_container_column), pointer, intent(out) :: p  !! alias to the container.
        end subroutine container_ptr
        !> Reads string element `i` into an allocatable string (PK_STRING).
        module subroutine get_at_str(self, i, value)
            class(parquet_column), intent(in) :: self             !! the column.
            integer(int64), intent(in) :: i                       !! 1-based row index.
            character(len=:), allocatable, intent(out) :: value    !! the element's value.
        end subroutine get_at_str
        !> Reads row `i`'s whole string vector (PK_STRING_VEC), one array element per position.
        module subroutine get_at_strv(self, i, value)
            class(parquet_column), intent(in) :: self  !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            character(len=*), intent(out) :: value(:)  !! receives width values, blank-padded.
        end subroutine get_at_strv
        !> Reads ONE element of row `i`'s string vector (PK_STRING_VEC) into an allocatable string.
        !!
        !! Unlike `get_at_strv`, nothing here is blank-padded to a caller-declared width and no
        !! width-long array is built: the result is exactly as long as the stored value.
        module subroutine get_elem_strv(self, i, e, value)
            class(parquet_column), intent(in) :: self           !! the column.
            integer(int64), intent(in) :: i                     !! 1-based row index.
            integer(int64), intent(in) :: e                     !! 1-based element index within the row.
            character(len=:), allocatable, intent(out) :: value !! the element's value.
        end subroutine get_elem_strv
        !> Writes ONE element of row `i`'s string vector (PK_STRING_VEC), clearing THAT element's
        !! null. `value` is a SCALAR, so it is stored verbatim -- the array forms' trimming rule is
        !! about one declared length shared by every element, and one element has no such length.
        module subroutine set_elem_strv(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            character(len=*), intent(in) :: value        !! the new value.
        end subroutine set_elem_strv
        !> Writes string element `i` (PK_STRING). `value` is a SCALAR, so it is stored verbatim,
        !! trailing blanks included -- a scalar is exactly as long as the caller wrote it. Every
        !! character ARRAY entry point below trims instead; see `set_all_str`.
        module subroutine set_at_str(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            character(len=*), intent(in) :: value        !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_str
        !> Writes row `i`'s whole string vector (PK_STRING_VEC). Trailing blanks are trimmed --
        !! `value` is an array, so see `set_all_str` for why that differs from `set_at_str`.
        module subroutine set_at_strv(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            character(len=*), intent(in) :: value(:)     !! width values for row i.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_strv
        !> Replaces every value in a PK_STRING column. **Trailing blanks are trimmed.**
        !!
        !! Every element of a `character(len=*)` array shares one declared length, so a shorter
        !! value is blank-padded by Fortran and those blanks carry nothing the caller could have
        !! meant. That is why every character ARRAY entry point here trims while the SCALAR
        !! `set_at_str` stores its value verbatim, and why `parquet_string_column`'s own API --
        !! which takes bytes the caller controls exactly -- trims only when asked with `trim=`.
        module subroutine set_all_str(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:)    !! exactly nrows values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_str
        !> Replaces every value in a PK_STRING_VEC column, shaped (width, nrows). Trailing blanks
        !! are trimmed, as in `set_all_str`.
        module subroutine set_all_strv(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:,:)  !! (width, nrows) values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_strv
        !> Appends string rows to a PK_STRING column. Trailing blanks are trimmed, as in
        !! `set_all_str`, so a column filled by `%append` holds the same bytes as one filled by
        !! `%set_all`.
        module subroutine append_values_str(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:)    !! rows to append.
        end subroutine append_values_str
        !> Appends string vector rows to a PK_STRING_VEC column, shaped (width, n). Trailing
        !! blanks are trimmed, as in `set_all_str`.
        module subroutine append_values_strv(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:,:)  !! (width, n) rows to append.
        end subroutine append_values_strv
    end interface
    !
    ! ---- Value access per kind (parquet_columns_access, GENERATED) ----
    interface
        !> Reads row `i`'s element from a PK_INT32 column.
        module subroutine get_at_i32(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int32), intent(out) :: value   !! receives the value.
        end subroutine get_at_i32
        !> Writes row `i`'s element in a PK_INT32 column.
        module subroutine set_at_i32(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int32), intent(in) :: value       !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_i32
        !> Replaces every value in a PK_INT32 column.
        module subroutine set_all_i32(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), intent(in) :: values(:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_i32
        !> Makes `values` this column's storage as a PK_INT32 column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_i32(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), allocatable, intent(inout) :: values(:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_i32
        !> Zero-copy typed pointer to a PK_INT32 column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_i32(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            integer(int32), pointer, intent(out) :: p(:)      !! alias to the live storage.
        end subroutine data_ptr_i32
        !> Reads row `i`'s element from a PK_INT64 column.
        module subroutine get_at_i64(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(out) :: value   !! receives the value.
        end subroutine get_at_i64
        !> Writes row `i`'s element in a PK_INT64 column.
        module subroutine set_at_i64(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: value       !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_i64
        !> Replaces every value in a PK_INT64 column.
        module subroutine set_all_i64(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: values(:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_i64
        !> Makes `values` this column's storage as a PK_INT64 column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_i64(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), allocatable, intent(inout) :: values(:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_i64
        !> Zero-copy typed pointer to a PK_INT64 column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_i64(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            integer(int64), pointer, intent(out) :: p(:)      !! alias to the live storage.
        end subroutine data_ptr_i64
        !> Reads row `i`'s element from a PK_FLOAT32 column.
        module subroutine get_at_f32(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            real(real32), intent(out) :: value   !! receives the value.
        end subroutine get_at_f32
        !> Writes row `i`'s element in a PK_FLOAT32 column.
        module subroutine set_at_f32(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            real(real32), intent(in) :: value       !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_f32
        !> Replaces every value in a PK_FLOAT32 column.
        module subroutine set_all_f32(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real32), intent(in) :: values(:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_f32
        !> Makes `values` this column's storage as a PK_FLOAT32 column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_f32(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real32), allocatable, intent(inout) :: values(:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_f32
        !> Zero-copy typed pointer to a PK_FLOAT32 column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_f32(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            real(real32), pointer, intent(out) :: p(:)      !! alias to the live storage.
        end subroutine data_ptr_f32
        !> Reads row `i`'s element from a PK_FLOAT64 column.
        module subroutine get_at_f64(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            real(real64), intent(out) :: value   !! receives the value.
        end subroutine get_at_f64
        !> Writes row `i`'s element in a PK_FLOAT64 column.
        module subroutine set_at_f64(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            real(real64), intent(in) :: value       !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_f64
        !> Replaces every value in a PK_FLOAT64 column.
        module subroutine set_all_f64(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real64), intent(in) :: values(:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_f64
        !> Makes `values` this column's storage as a PK_FLOAT64 column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_f64(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real64), allocatable, intent(inout) :: values(:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_f64
        !> Zero-copy typed pointer to a PK_FLOAT64 column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_f64(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            real(real64), pointer, intent(out) :: p(:)      !! alias to the live storage.
        end subroutine data_ptr_f64
        !> Reads row `i`'s element from a PK_LOGICAL column.
        module subroutine get_at_bool(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            logical, intent(out) :: value   !! receives the value.
        end subroutine get_at_bool
        !> Writes row `i`'s element in a PK_LOGICAL column.
        module subroutine set_at_bool(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            logical, intent(in) :: value       !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_bool
        !> Replaces every value in a PK_LOGICAL column.
        module subroutine set_all_bool(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: values(:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_bool
        !> Makes `values` this column's storage as a PK_LOGICAL column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_bool(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, allocatable, intent(inout) :: values(:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_bool
        !> Zero-copy typed pointer to a PK_LOGICAL column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_bool(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            logical, pointer, intent(out) :: p(:)      !! alias to the live storage.
        end subroutine data_ptr_bool
        !> Reads row `i`'s element from a PK_DATE column.
        module subroutine get_at_date(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            type(parquet_date), intent(out) :: value   !! receives the value.
        end subroutine get_at_date
        !> Writes row `i`'s element in a PK_DATE column.
        module subroutine set_at_date(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            type(parquet_date), intent(in) :: value       !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_date
        !> Replaces every value in a PK_DATE column.
        module subroutine set_all_date(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_date), intent(in) :: values(:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_date
        !> Makes `values` this column's storage as a PK_DATE column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_date(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_date), allocatable, intent(inout) :: values(:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_date
        !> Zero-copy typed pointer to a PK_DATE column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_date(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            type(parquet_date), pointer, intent(out) :: p(:)      !! alias to the live storage.
        end subroutine data_ptr_date
        !> Reads row `i`'s element from a PK_TIME column.
        module subroutine get_at_time(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            type(parquet_time), intent(out) :: value   !! receives the value.
        end subroutine get_at_time
        !> Writes row `i`'s element in a PK_TIME column.
        module subroutine set_at_time(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            type(parquet_time), intent(in) :: value       !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_time
        !> Replaces every value in a PK_TIME column.
        module subroutine set_all_time(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_time), intent(in) :: values(:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_time
        !> Makes `values` this column's storage as a PK_TIME column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_time(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_time), allocatable, intent(inout) :: values(:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_time
        !> Zero-copy typed pointer to a PK_TIME column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_time(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            type(parquet_time), pointer, intent(out) :: p(:)      !! alias to the live storage.
        end subroutine data_ptr_time
        !> Reads row `i`'s element from a PK_TIMESTAMP column.
        module subroutine get_at_ts(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            type(parquet_timestamp), intent(out) :: value   !! receives the value.
        end subroutine get_at_ts
        !> Writes row `i`'s element in a PK_TIMESTAMP column.
        module subroutine set_at_ts(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            type(parquet_timestamp), intent(in) :: value       !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_ts
        !> Replaces every value in a PK_TIMESTAMP column.
        module subroutine set_all_ts(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_timestamp), intent(in) :: values(:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_ts
        !> Makes `values` this column's storage as a PK_TIMESTAMP column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_ts(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_timestamp), allocatable, intent(inout) :: values(:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_ts
        !> Zero-copy typed pointer to a PK_TIMESTAMP column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_ts(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            type(parquet_timestamp), pointer, intent(out) :: p(:)      !! alias to the live storage.
        end subroutine data_ptr_ts
        !> Reads row `i`'s row's vector from a PK_INT32_VEC column.
        module subroutine get_at_i32v(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int32), intent(out) :: value(:)   !! receives the values.
        end subroutine get_at_i32v
        !> Writes row `i`'s row's vector in a PK_INT32_VEC column.
        module subroutine set_at_i32v(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int32), intent(in) :: value(:)       !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_i32v
        !> Reads ONE element of row `i`'s vector from a PK_INT32_VEC column.
        !!
        !! The point is what it does NOT do: `get_at` fills a width-long array, so reading one
        !! element through it costs the caller an allocation per access.
        module subroutine get_elem_i32v(self, i, e, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            integer(int32), intent(out) :: value         !! receives the element's value.
        end subroutine get_elem_i32v
        !> Writes ONE element of row `i`'s vector in a PK_INT32_VEC column, clearing THAT element's null.
        !!
        !! Writing through `%data_ptr` instead would leave the column's own null bookkeeping
        !! behind -- for a temporal kind that is a cached answer this type recomputes lazily, so
        !! a bypassed write shows up later as a wrong `%any_null()` and nowhere near its cause.
        !! That bookkeeping is private to this type, which is why the operation belongs here.
        module subroutine set_elem_i32v(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            integer(int32), intent(in) :: value             !! the new value.
        end subroutine set_elem_i32v
        !> Replaces every value in a PK_INT32_VEC column.
        module subroutine set_all_i32v(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), intent(in) :: values(:,:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_i32v
        !> Makes `values` this column's storage as a PK_INT32_VEC column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_i32v(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), allocatable, intent(inout) :: values(:,:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_i32v
        !> Zero-copy typed pointer to a PK_INT32_VEC column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_i32v(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            integer(int32), pointer, intent(out) :: p(:,:)      !! alias to the live storage.
        end subroutine data_ptr_i32v
        !> Reads row `i`'s row's vector from a PK_INT64_VEC column.
        module subroutine get_at_i64v(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(out) :: value(:)   !! receives the values.
        end subroutine get_at_i64v
        !> Writes row `i`'s row's vector in a PK_INT64_VEC column.
        module subroutine set_at_i64v(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: value(:)       !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_i64v
        !> Reads ONE element of row `i`'s vector from a PK_INT64_VEC column.
        !!
        !! The point is what it does NOT do: `get_at` fills a width-long array, so reading one
        !! element through it costs the caller an allocation per access.
        module subroutine get_elem_i64v(self, i, e, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            integer(int64), intent(out) :: value         !! receives the element's value.
        end subroutine get_elem_i64v
        !> Writes ONE element of row `i`'s vector in a PK_INT64_VEC column, clearing THAT element's null.
        !!
        !! Writing through `%data_ptr` instead would leave the column's own null bookkeeping
        !! behind -- for a temporal kind that is a cached answer this type recomputes lazily, so
        !! a bypassed write shows up later as a wrong `%any_null()` and nowhere near its cause.
        !! That bookkeeping is private to this type, which is why the operation belongs here.
        module subroutine set_elem_i64v(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            integer(int64), intent(in) :: value             !! the new value.
        end subroutine set_elem_i64v
        !> Replaces every value in a PK_INT64_VEC column.
        module subroutine set_all_i64v(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: values(:,:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_i64v
        !> Makes `values` this column's storage as a PK_INT64_VEC column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_i64v(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), allocatable, intent(inout) :: values(:,:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_i64v
        !> Zero-copy typed pointer to a PK_INT64_VEC column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_i64v(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            integer(int64), pointer, intent(out) :: p(:,:)      !! alias to the live storage.
        end subroutine data_ptr_i64v
        !> Reads row `i`'s row's vector from a PK_FLOAT32_VEC column.
        module subroutine get_at_f32v(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            real(real32), intent(out) :: value(:)   !! receives the values.
        end subroutine get_at_f32v
        !> Writes row `i`'s row's vector in a PK_FLOAT32_VEC column.
        module subroutine set_at_f32v(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            real(real32), intent(in) :: value(:)       !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_f32v
        !> Reads ONE element of row `i`'s vector from a PK_FLOAT32_VEC column.
        !!
        !! The point is what it does NOT do: `get_at` fills a width-long array, so reading one
        !! element through it costs the caller an allocation per access.
        module subroutine get_elem_f32v(self, i, e, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            real(real32), intent(out) :: value         !! receives the element's value.
        end subroutine get_elem_f32v
        !> Writes ONE element of row `i`'s vector in a PK_FLOAT32_VEC column, clearing THAT element's null.
        !!
        !! Writing through `%data_ptr` instead would leave the column's own null bookkeeping
        !! behind -- for a temporal kind that is a cached answer this type recomputes lazily, so
        !! a bypassed write shows up later as a wrong `%any_null()` and nowhere near its cause.
        !! That bookkeeping is private to this type, which is why the operation belongs here.
        module subroutine set_elem_f32v(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            real(real32), intent(in) :: value             !! the new value.
        end subroutine set_elem_f32v
        !> Replaces every value in a PK_FLOAT32_VEC column.
        module subroutine set_all_f32v(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real32), intent(in) :: values(:,:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_f32v
        !> Makes `values` this column's storage as a PK_FLOAT32_VEC column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_f32v(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real32), allocatable, intent(inout) :: values(:,:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_f32v
        !> Zero-copy typed pointer to a PK_FLOAT32_VEC column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_f32v(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            real(real32), pointer, intent(out) :: p(:,:)      !! alias to the live storage.
        end subroutine data_ptr_f32v
        !> Reads row `i`'s row's vector from a PK_FLOAT64_VEC column.
        module subroutine get_at_f64v(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            real(real64), intent(out) :: value(:)   !! receives the values.
        end subroutine get_at_f64v
        !> Writes row `i`'s row's vector in a PK_FLOAT64_VEC column.
        module subroutine set_at_f64v(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            real(real64), intent(in) :: value(:)       !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_f64v
        !> Reads ONE element of row `i`'s vector from a PK_FLOAT64_VEC column.
        !!
        !! The point is what it does NOT do: `get_at` fills a width-long array, so reading one
        !! element through it costs the caller an allocation per access.
        module subroutine get_elem_f64v(self, i, e, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            real(real64), intent(out) :: value         !! receives the element's value.
        end subroutine get_elem_f64v
        !> Writes ONE element of row `i`'s vector in a PK_FLOAT64_VEC column, clearing THAT element's null.
        !!
        !! Writing through `%data_ptr` instead would leave the column's own null bookkeeping
        !! behind -- for a temporal kind that is a cached answer this type recomputes lazily, so
        !! a bypassed write shows up later as a wrong `%any_null()` and nowhere near its cause.
        !! That bookkeeping is private to this type, which is why the operation belongs here.
        module subroutine set_elem_f64v(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            real(real64), intent(in) :: value             !! the new value.
        end subroutine set_elem_f64v
        !> Replaces every value in a PK_FLOAT64_VEC column.
        module subroutine set_all_f64v(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real64), intent(in) :: values(:,:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_f64v
        !> Makes `values` this column's storage as a PK_FLOAT64_VEC column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_f64v(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real64), allocatable, intent(inout) :: values(:,:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_f64v
        !> Zero-copy typed pointer to a PK_FLOAT64_VEC column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_f64v(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            real(real64), pointer, intent(out) :: p(:,:)      !! alias to the live storage.
        end subroutine data_ptr_f64v
        !> Reads row `i`'s row's vector from a PK_LOGICAL_VEC column.
        module subroutine get_at_boolv(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            logical, intent(out) :: value(:)   !! receives the values.
        end subroutine get_at_boolv
        !> Writes row `i`'s row's vector in a PK_LOGICAL_VEC column.
        module subroutine set_at_boolv(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            logical, intent(in) :: value(:)       !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_boolv
        !> Reads ONE element of row `i`'s vector from a PK_LOGICAL_VEC column.
        !!
        !! The point is what it does NOT do: `get_at` fills a width-long array, so reading one
        !! element through it costs the caller an allocation per access.
        module subroutine get_elem_boolv(self, i, e, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            logical, intent(out) :: value         !! receives the element's value.
        end subroutine get_elem_boolv
        !> Writes ONE element of row `i`'s vector in a PK_LOGICAL_VEC column, clearing THAT element's null.
        !!
        !! Writing through `%data_ptr` instead would leave the column's own null bookkeeping
        !! behind -- for a temporal kind that is a cached answer this type recomputes lazily, so
        !! a bypassed write shows up later as a wrong `%any_null()` and nowhere near its cause.
        !! That bookkeeping is private to this type, which is why the operation belongs here.
        module subroutine set_elem_boolv(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            logical, intent(in) :: value             !! the new value.
        end subroutine set_elem_boolv
        !> Replaces every value in a PK_LOGICAL_VEC column.
        module subroutine set_all_boolv(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: values(:,:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_boolv
        !> Makes `values` this column's storage as a PK_LOGICAL_VEC column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_boolv(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, allocatable, intent(inout) :: values(:,:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_boolv
        !> Zero-copy typed pointer to a PK_LOGICAL_VEC column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_boolv(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            logical, pointer, intent(out) :: p(:,:)      !! alias to the live storage.
        end subroutine data_ptr_boolv
        !> Reads row `i`'s row's vector from a PK_DATE_VEC column.
        module subroutine get_at_datev(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            type(parquet_date), intent(out) :: value(:)   !! receives the values.
        end subroutine get_at_datev
        !> Writes row `i`'s row's vector in a PK_DATE_VEC column.
        module subroutine set_at_datev(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            type(parquet_date), intent(in) :: value(:)       !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_datev
        !> Reads ONE element of row `i`'s vector from a PK_DATE_VEC column.
        !!
        !! The point is what it does NOT do: `get_at` fills a width-long array, so reading one
        !! element through it costs the caller an allocation per access.
        module subroutine get_elem_datev(self, i, e, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            type(parquet_date), intent(out) :: value         !! receives the element's value.
        end subroutine get_elem_datev
        !> Writes ONE element of row `i`'s vector in a PK_DATE_VEC column, clearing THAT element's null.
        !!
        !! Writing through `%data_ptr` instead would leave the column's own null bookkeeping
        !! behind -- for a temporal kind that is a cached answer this type recomputes lazily, so
        !! a bypassed write shows up later as a wrong `%any_null()` and nowhere near its cause.
        !! That bookkeeping is private to this type, which is why the operation belongs here.
        module subroutine set_elem_datev(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            type(parquet_date), intent(in) :: value             !! the new value.
        end subroutine set_elem_datev
        !> Replaces every value in a PK_DATE_VEC column.
        module subroutine set_all_datev(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_date), intent(in) :: values(:,:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_datev
        !> Makes `values` this column's storage as a PK_DATE_VEC column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_datev(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_date), allocatable, intent(inout) :: values(:,:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_datev
        !> Zero-copy typed pointer to a PK_DATE_VEC column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_datev(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            type(parquet_date), pointer, intent(out) :: p(:,:)      !! alias to the live storage.
        end subroutine data_ptr_datev
        !> Reads row `i`'s row's vector from a PK_TIME_VEC column.
        module subroutine get_at_timev(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            type(parquet_time), intent(out) :: value(:)   !! receives the values.
        end subroutine get_at_timev
        !> Writes row `i`'s row's vector in a PK_TIME_VEC column.
        module subroutine set_at_timev(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            type(parquet_time), intent(in) :: value(:)       !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_timev
        !> Reads ONE element of row `i`'s vector from a PK_TIME_VEC column.
        !!
        !! The point is what it does NOT do: `get_at` fills a width-long array, so reading one
        !! element through it costs the caller an allocation per access.
        module subroutine get_elem_timev(self, i, e, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            type(parquet_time), intent(out) :: value         !! receives the element's value.
        end subroutine get_elem_timev
        !> Writes ONE element of row `i`'s vector in a PK_TIME_VEC column, clearing THAT element's null.
        !!
        !! Writing through `%data_ptr` instead would leave the column's own null bookkeeping
        !! behind -- for a temporal kind that is a cached answer this type recomputes lazily, so
        !! a bypassed write shows up later as a wrong `%any_null()` and nowhere near its cause.
        !! That bookkeeping is private to this type, which is why the operation belongs here.
        module subroutine set_elem_timev(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            type(parquet_time), intent(in) :: value             !! the new value.
        end subroutine set_elem_timev
        !> Replaces every value in a PK_TIME_VEC column.
        module subroutine set_all_timev(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_time), intent(in) :: values(:,:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_timev
        !> Makes `values` this column's storage as a PK_TIME_VEC column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_timev(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_time), allocatable, intent(inout) :: values(:,:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_timev
        !> Zero-copy typed pointer to a PK_TIME_VEC column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_timev(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            type(parquet_time), pointer, intent(out) :: p(:,:)      !! alias to the live storage.
        end subroutine data_ptr_timev
        !> Reads row `i`'s row's vector from a PK_TIMESTAMP_VEC column.
        module subroutine get_at_tsv(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            type(parquet_timestamp), intent(out) :: value(:)   !! receives the values.
        end subroutine get_at_tsv
        !> Writes row `i`'s row's vector in a PK_TIMESTAMP_VEC column.
        module subroutine set_at_tsv(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            type(parquet_timestamp), intent(in) :: value(:)       !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_tsv
        !> Reads ONE element of row `i`'s vector from a PK_TIMESTAMP_VEC column.
        !!
        !! The point is what it does NOT do: `get_at` fills a width-long array, so reading one
        !! element through it costs the caller an allocation per access.
        module subroutine get_elem_tsv(self, i, e, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            type(parquet_timestamp), intent(out) :: value         !! receives the element's value.
        end subroutine get_elem_tsv
        !> Writes ONE element of row `i`'s vector in a PK_TIMESTAMP_VEC column, clearing THAT element's null.
        !!
        !! Writing through `%data_ptr` instead would leave the column's own null bookkeeping
        !! behind -- for a temporal kind that is a cached answer this type recomputes lazily, so
        !! a bypassed write shows up later as a wrong `%any_null()` and nowhere near its cause.
        !! That bookkeeping is private to this type, which is why the operation belongs here.
        module subroutine set_elem_tsv(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            type(parquet_timestamp), intent(in) :: value             !! the new value.
        end subroutine set_elem_tsv
        !> Replaces every value in a PK_TIMESTAMP_VEC column.
        module subroutine set_all_tsv(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_timestamp), intent(in) :: values(:,:)      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_tsv
        !> Makes `values` this column's storage as a PK_TIMESTAMP_VEC column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_tsv(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_timestamp), allocatable, intent(inout) :: values(:,:) !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_tsv
        !> Zero-copy typed pointer to a PK_TIMESTAMP_VEC column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_tsv(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            type(parquet_timestamp), pointer, intent(out) :: p(:,:)      !! alias to the live storage.
        end subroutine data_ptr_tsv
    end interface
    !
    ! ---- Typed per-cell access: the NON-POLYMORPHIC implementation tier (feature_ifx.md) ----
    !
    ! Every per-cell accessor's body lives here, behind a `type(parquet_column)` dummy. The
    ! type-bound bindings declared above are one-line forwarders onto these, and `parquet_tables`
    ! calls these directly instead of going through a binding.
    !
    ! The direction is the whole point and must never be flipped. A `class` actual passed to a
    ! `type` dummy hands over the declared-type part for nothing; a `type` actual passed to a
    ! `class` dummy makes ifx construct a runtime type descriptor in the CALLER's prologue --
    ! 178 stores, emitted unconditionally ahead of any branch, ~35 ns on every call -- because
    ! `parquet_column` has 20 allocatable components and one finalizable component. Re-homing the
    ! bodies down here removes that conversion from every per-cell path in the table layer;
    ! re-homing them the other way would silently restore it, with no test failure and no warning.
    ! `check_no_type_bound_column_access` (tools/check_source_conventions.py) is what enforces it.
    !
    ! ---- Typed value access per kind (parquet_columns_access, GENERATED) ----
    interface
        !> Typed `get_at` for a PK_INT32 column: reads row `i`'s element.
        module subroutine parquet_column_get_at_i32(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int32), intent(out) :: value   !! receives the value.
        end subroutine parquet_column_get_at_i32
        !> Typed `set_at` for a PK_INT32 column: writes row `i`'s element.
        module subroutine parquet_column_set_at_i32(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int32), intent(in) :: value     !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_i32
        !> Typed `data_ptr` for a PK_INT32 column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_i32(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            integer(int32), pointer, intent(out) :: p(:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_i32
        !> Typed `get_at` for a PK_INT64 column: reads row `i`'s element.
        module subroutine parquet_column_get_at_i64(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(out) :: value   !! receives the value.
        end subroutine parquet_column_get_at_i64
        !> Typed `set_at` for a PK_INT64 column: writes row `i`'s element.
        module subroutine parquet_column_set_at_i64(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: value     !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_i64
        !> Typed `data_ptr` for a PK_INT64 column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_i64(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            integer(int64), pointer, intent(out) :: p(:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_i64
        !> Typed `get_at` for a PK_FLOAT32 column: reads row `i`'s element.
        module subroutine parquet_column_get_at_f32(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            real(real32), intent(out) :: value   !! receives the value.
        end subroutine parquet_column_get_at_f32
        !> Typed `set_at` for a PK_FLOAT32 column: writes row `i`'s element.
        module subroutine parquet_column_set_at_f32(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            real(real32), intent(in) :: value     !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_f32
        !> Typed `data_ptr` for a PK_FLOAT32 column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_f32(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            real(real32), pointer, intent(out) :: p(:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_f32
        !> Typed `get_at` for a PK_FLOAT64 column: reads row `i`'s element.
        module subroutine parquet_column_get_at_f64(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            real(real64), intent(out) :: value   !! receives the value.
        end subroutine parquet_column_get_at_f64
        !> Typed `set_at` for a PK_FLOAT64 column: writes row `i`'s element.
        module subroutine parquet_column_set_at_f64(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            real(real64), intent(in) :: value     !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_f64
        !> Typed `data_ptr` for a PK_FLOAT64 column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_f64(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            real(real64), pointer, intent(out) :: p(:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_f64
        !> Typed `get_at` for a PK_LOGICAL column: reads row `i`'s element.
        module subroutine parquet_column_get_at_bool(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            logical, intent(out) :: value   !! receives the value.
        end subroutine parquet_column_get_at_bool
        !> Typed `set_at` for a PK_LOGICAL column: writes row `i`'s element.
        module subroutine parquet_column_set_at_bool(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            logical, intent(in) :: value     !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_bool
        !> Typed `data_ptr` for a PK_LOGICAL column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_bool(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            logical, pointer, intent(out) :: p(:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_bool
        !> Typed `get_at` for a PK_DATE column: reads row `i`'s element.
        module subroutine parquet_column_get_at_date(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            type(parquet_date), intent(out) :: value   !! receives the value.
        end subroutine parquet_column_get_at_date
        !> Typed `set_at` for a PK_DATE column: writes row `i`'s element.
        module subroutine parquet_column_set_at_date(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            type(parquet_date), intent(in) :: value     !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_date
        !> Typed `data_ptr` for a PK_DATE column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_date(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            type(parquet_date), pointer, intent(out) :: p(:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_date
        !> Typed `get_at` for a PK_TIME column: reads row `i`'s element.
        module subroutine parquet_column_get_at_time(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            type(parquet_time), intent(out) :: value   !! receives the value.
        end subroutine parquet_column_get_at_time
        !> Typed `set_at` for a PK_TIME column: writes row `i`'s element.
        module subroutine parquet_column_set_at_time(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            type(parquet_time), intent(in) :: value     !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_time
        !> Typed `data_ptr` for a PK_TIME column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_time(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            type(parquet_time), pointer, intent(out) :: p(:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_time
        !> Typed `get_at` for a PK_TIMESTAMP column: reads row `i`'s element.
        module subroutine parquet_column_get_at_ts(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            type(parquet_timestamp), intent(out) :: value   !! receives the value.
        end subroutine parquet_column_get_at_ts
        !> Typed `set_at` for a PK_TIMESTAMP column: writes row `i`'s element.
        module subroutine parquet_column_set_at_ts(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            type(parquet_timestamp), intent(in) :: value     !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_ts
        !> Typed `data_ptr` for a PK_TIMESTAMP column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_ts(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            type(parquet_timestamp), pointer, intent(out) :: p(:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_ts
        !> Typed `get_at` for a PK_INT32_VEC column: reads row `i`'s row's vector.
        module subroutine parquet_column_get_at_i32v(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int32), intent(out) :: value(:)   !! receives the values.
        end subroutine parquet_column_get_at_i32v
        !> Typed `set_at` for a PK_INT32_VEC column: writes row `i`'s row's vector.
        module subroutine parquet_column_set_at_i32v(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int32), intent(in) :: value(:)     !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_i32v
        !> Typed `get_elem` for a PK_INT32_VEC column: reads ONE element of row `i`'s vector.
        module subroutine parquet_column_get_elem_i32v(col, i, e, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            integer(int32), intent(out) :: value       !! receives the element's value.
        end subroutine parquet_column_get_elem_i32v
        !> Typed `set_elem` for a PK_INT32_VEC column: writes ONE element of row `i`'s vector.
        module subroutine parquet_column_set_elem_i32v(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            integer(int32), intent(in) :: value           !! the new value.
        end subroutine parquet_column_set_elem_i32v
        !> Typed `data_ptr` for a PK_INT32_VEC column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_i32v(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            integer(int32), pointer, intent(out) :: p(:,:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_i32v
        !> Typed `get_at` for a PK_INT64_VEC column: reads row `i`'s row's vector.
        module subroutine parquet_column_get_at_i64v(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(out) :: value(:)   !! receives the values.
        end subroutine parquet_column_get_at_i64v
        !> Typed `set_at` for a PK_INT64_VEC column: writes row `i`'s row's vector.
        module subroutine parquet_column_set_at_i64v(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: value(:)     !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_i64v
        !> Typed `get_elem` for a PK_INT64_VEC column: reads ONE element of row `i`'s vector.
        module subroutine parquet_column_get_elem_i64v(col, i, e, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            integer(int64), intent(out) :: value       !! receives the element's value.
        end subroutine parquet_column_get_elem_i64v
        !> Typed `set_elem` for a PK_INT64_VEC column: writes ONE element of row `i`'s vector.
        module subroutine parquet_column_set_elem_i64v(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            integer(int64), intent(in) :: value           !! the new value.
        end subroutine parquet_column_set_elem_i64v
        !> Typed `data_ptr` for a PK_INT64_VEC column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_i64v(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            integer(int64), pointer, intent(out) :: p(:,:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_i64v
        !> Typed `get_at` for a PK_FLOAT32_VEC column: reads row `i`'s row's vector.
        module subroutine parquet_column_get_at_f32v(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            real(real32), intent(out) :: value(:)   !! receives the values.
        end subroutine parquet_column_get_at_f32v
        !> Typed `set_at` for a PK_FLOAT32_VEC column: writes row `i`'s row's vector.
        module subroutine parquet_column_set_at_f32v(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            real(real32), intent(in) :: value(:)     !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_f32v
        !> Typed `get_elem` for a PK_FLOAT32_VEC column: reads ONE element of row `i`'s vector.
        module subroutine parquet_column_get_elem_f32v(col, i, e, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            real(real32), intent(out) :: value       !! receives the element's value.
        end subroutine parquet_column_get_elem_f32v
        !> Typed `set_elem` for a PK_FLOAT32_VEC column: writes ONE element of row `i`'s vector.
        module subroutine parquet_column_set_elem_f32v(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            real(real32), intent(in) :: value           !! the new value.
        end subroutine parquet_column_set_elem_f32v
        !> Typed `data_ptr` for a PK_FLOAT32_VEC column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_f32v(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            real(real32), pointer, intent(out) :: p(:,:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_f32v
        !> Typed `get_at` for a PK_FLOAT64_VEC column: reads row `i`'s row's vector.
        module subroutine parquet_column_get_at_f64v(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            real(real64), intent(out) :: value(:)   !! receives the values.
        end subroutine parquet_column_get_at_f64v
        !> Typed `set_at` for a PK_FLOAT64_VEC column: writes row `i`'s row's vector.
        module subroutine parquet_column_set_at_f64v(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            real(real64), intent(in) :: value(:)     !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_f64v
        !> Typed `get_elem` for a PK_FLOAT64_VEC column: reads ONE element of row `i`'s vector.
        module subroutine parquet_column_get_elem_f64v(col, i, e, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            real(real64), intent(out) :: value       !! receives the element's value.
        end subroutine parquet_column_get_elem_f64v
        !> Typed `set_elem` for a PK_FLOAT64_VEC column: writes ONE element of row `i`'s vector.
        module subroutine parquet_column_set_elem_f64v(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            real(real64), intent(in) :: value           !! the new value.
        end subroutine parquet_column_set_elem_f64v
        !> Typed `data_ptr` for a PK_FLOAT64_VEC column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_f64v(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            real(real64), pointer, intent(out) :: p(:,:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_f64v
        !> Typed `get_at` for a PK_LOGICAL_VEC column: reads row `i`'s row's vector.
        module subroutine parquet_column_get_at_boolv(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            logical, intent(out) :: value(:)   !! receives the values.
        end subroutine parquet_column_get_at_boolv
        !> Typed `set_at` for a PK_LOGICAL_VEC column: writes row `i`'s row's vector.
        module subroutine parquet_column_set_at_boolv(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            logical, intent(in) :: value(:)     !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_boolv
        !> Typed `get_elem` for a PK_LOGICAL_VEC column: reads ONE element of row `i`'s vector.
        module subroutine parquet_column_get_elem_boolv(col, i, e, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            logical, intent(out) :: value       !! receives the element's value.
        end subroutine parquet_column_get_elem_boolv
        !> Typed `set_elem` for a PK_LOGICAL_VEC column: writes ONE element of row `i`'s vector.
        module subroutine parquet_column_set_elem_boolv(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            logical, intent(in) :: value           !! the new value.
        end subroutine parquet_column_set_elem_boolv
        !> Typed `data_ptr` for a PK_LOGICAL_VEC column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_boolv(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            logical, pointer, intent(out) :: p(:,:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_boolv
        !> Typed `get_at` for a PK_DATE_VEC column: reads row `i`'s row's vector.
        module subroutine parquet_column_get_at_datev(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            type(parquet_date), intent(out) :: value(:)   !! receives the values.
        end subroutine parquet_column_get_at_datev
        !> Typed `set_at` for a PK_DATE_VEC column: writes row `i`'s row's vector.
        module subroutine parquet_column_set_at_datev(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            type(parquet_date), intent(in) :: value(:)     !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_datev
        !> Typed `get_elem` for a PK_DATE_VEC column: reads ONE element of row `i`'s vector.
        module subroutine parquet_column_get_elem_datev(col, i, e, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            type(parquet_date), intent(out) :: value       !! receives the element's value.
        end subroutine parquet_column_get_elem_datev
        !> Typed `set_elem` for a PK_DATE_VEC column: writes ONE element of row `i`'s vector.
        module subroutine parquet_column_set_elem_datev(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            type(parquet_date), intent(in) :: value           !! the new value.
        end subroutine parquet_column_set_elem_datev
        !> Typed `data_ptr` for a PK_DATE_VEC column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_datev(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            type(parquet_date), pointer, intent(out) :: p(:,:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_datev
        !> Typed `get_at` for a PK_TIME_VEC column: reads row `i`'s row's vector.
        module subroutine parquet_column_get_at_timev(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            type(parquet_time), intent(out) :: value(:)   !! receives the values.
        end subroutine parquet_column_get_at_timev
        !> Typed `set_at` for a PK_TIME_VEC column: writes row `i`'s row's vector.
        module subroutine parquet_column_set_at_timev(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            type(parquet_time), intent(in) :: value(:)     !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_timev
        !> Typed `get_elem` for a PK_TIME_VEC column: reads ONE element of row `i`'s vector.
        module subroutine parquet_column_get_elem_timev(col, i, e, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            type(parquet_time), intent(out) :: value       !! receives the element's value.
        end subroutine parquet_column_get_elem_timev
        !> Typed `set_elem` for a PK_TIME_VEC column: writes ONE element of row `i`'s vector.
        module subroutine parquet_column_set_elem_timev(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            type(parquet_time), intent(in) :: value           !! the new value.
        end subroutine parquet_column_set_elem_timev
        !> Typed `data_ptr` for a PK_TIME_VEC column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_timev(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            type(parquet_time), pointer, intent(out) :: p(:,:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_timev
        !> Typed `get_at` for a PK_TIMESTAMP_VEC column: reads row `i`'s row's vector.
        module subroutine parquet_column_get_at_tsv(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            type(parquet_timestamp), intent(out) :: value(:)   !! receives the values.
        end subroutine parquet_column_get_at_tsv
        !> Typed `set_at` for a PK_TIMESTAMP_VEC column: writes row `i`'s row's vector.
        module subroutine parquet_column_set_at_tsv(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            type(parquet_timestamp), intent(in) :: value(:)     !! the new values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_tsv
        !> Typed `get_elem` for a PK_TIMESTAMP_VEC column: reads ONE element of row `i`'s vector.
        module subroutine parquet_column_get_elem_tsv(col, i, e, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            type(parquet_timestamp), intent(out) :: value       !! receives the element's value.
        end subroutine parquet_column_get_elem_tsv
        !> Typed `set_elem` for a PK_TIMESTAMP_VEC column: writes ONE element of row `i`'s vector.
        module subroutine parquet_column_set_elem_tsv(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            type(parquet_timestamp), intent(in) :: value           !! the new value.
        end subroutine parquet_column_set_elem_tsv
        !> Typed `data_ptr` for a PK_TIMESTAMP_VEC column: zero-copy pointer to the live storage.
        module subroutine parquet_column_data_ptr_tsv(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            type(parquet_timestamp), pointer, intent(out) :: p(:,:)    !! alias to the live storage.
        end subroutine parquet_column_data_ptr_tsv
    end interface
    !
    ! ---- Typed string-kind access (parquet_columns_string) ----
    interface
        !> Typed `string_column`: pointer to the embedded string store (PK_STRING/PK_STRING_VEC).
        module subroutine parquet_column_string_column(col, p)
            type(parquet_column), intent(in), target :: col         !! the column.
            type(parquet_string_column), pointer, intent(out) :: p  !! alias to the string store.
        end subroutine parquet_column_string_column
        !> Typed `container`: pointer to the embedded container column (PK_LIST/PK_MAP/PK_STRUCT).
        !!
        !! The read counterpart of `adopt_container`, and the only way out of a `parquet_column`
        !! for the thing it holds. `parquet_tables` needs it for `%col`, `%get` and a row handle's
        !! `%ref`, and cannot reach `col%container` itself -- the component is private to this
        !! module.
        !!
        !! **The result is the ABSTRACT face, so a caller wanting a concrete type must
        !! `select type` on it.** That is deliberate rather than an omission: `parquet_columns`
        !! must never name `parquet_list_column`, `parquet_map_column` or `parquet_struct_column`,
        !! or it acquires the very dependency the tier split exists to prevent (and its own
        !! footprint grows by three modules for every consumer, `check_module_footprints.sh` being
        !! what would notice).
        !!
        !! **Per-COLUMN, not per-cell**, so it is outside `check_no_type_bound_column_access`'s
        !! scope and must NOT be added to that check's twinned-binding list: a `class` dummy on a
        !! per-cell path is what that rule bans, and this is called once when a column is reached,
        !! not once per row.
        module subroutine parquet_column_container(col, p)
            type(parquet_column), intent(in), target :: col                !! the column.
            class(parquet_container_column), pointer, intent(out) :: p     !! alias to the container.
        end subroutine parquet_column_container
        !> Typed `get_at` for PK_STRING: reads element `i` into an allocatable string.
        module subroutine parquet_column_get_at_str(col, i, value)
            type(parquet_column), intent(in) :: col             !! the column.
            integer(int64), intent(in) :: i                     !! 1-based row index.
            character(len=:), allocatable, intent(out) :: value !! the element's value.
        end subroutine parquet_column_get_at_str
        !> Typed `get_at` for PK_STRING_VEC: reads row `i`'s whole string vector, blank-padded.
        module subroutine parquet_column_get_at_strv(col, i, value)
            type(parquet_column), intent(in) :: col   !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            character(len=*), intent(out) :: value(:) !! receives width values, blank-padded.
        end subroutine parquet_column_get_at_strv
        !> Typed `get_elem` for PK_STRING_VEC: reads ONE element, sized to the stored value.
        module subroutine parquet_column_get_elem_strv(col, i, e, value)
            type(parquet_column), intent(in) :: col             !! the column.
            integer(int64), intent(in) :: i                     !! 1-based row index.
            integer(int64), intent(in) :: e                     !! 1-based element index in the row.
            character(len=:), allocatable, intent(out) :: value !! the element's value.
        end subroutine parquet_column_get_elem_strv
        !> Typed `set_at` for PK_STRING. `value` is a SCALAR, so it is stored verbatim.
        module subroutine parquet_column_set_at_str(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            character(len=*), intent(in) :: value      !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_str
        !> Typed `set_at` for PK_STRING_VEC. `value` is an ARRAY, so trailing blanks are trimmed.
        module subroutine parquet_column_set_at_strv(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            character(len=*), intent(in) :: value(:)   !! width values for row i.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine parquet_column_set_at_strv
        !> Typed `set_elem` for PK_STRING_VEC. `value` is a SCALAR, so it is stored verbatim.
        module subroutine parquet_column_set_elem_strv(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            character(len=*), intent(in) :: value      !! the new value.
        end subroutine parquet_column_set_elem_strv
    end interface
    !
    ! ---- Typed validity access (parquet_columns_validity) ----
    interface
        !> Typed `is_null` row form: .true. when ANY element of row `i` is null.
        module function parquet_column_is_null_row(col, i) result(res)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            logical :: res                          !! .true. when any element of the row is null.
        end function parquet_column_is_null_row
        !> Typed `is_null` element form: the null state of element `e` of row `i` alone.
        module function parquet_column_is_null_elem(col, i, e) result(res)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            logical :: res                          !! .true. when that element is null.
        end function parquet_column_is_null_elem
        !> Typed `set_null` row form: marks EVERY element of row `i` null.
        module subroutine parquet_column_set_null_row(col, i)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
        end subroutine parquet_column_set_null_row
        !> Typed `set_null` element form: marks element `e` of row `i` null.
        module subroutine parquet_column_set_null_elem(col, i, e)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
        end subroutine parquet_column_set_null_elem
        !> Typed `clear_null` row form: clears the null flag of EVERY element of row `i`.
        module subroutine parquet_column_clear_null_row(col, i)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
        end subroutine parquet_column_clear_null_row
        !> Typed `clear_null` element form: clears the null flag of element `e` of row `i`.
        module subroutine parquet_column_clear_null_elem(col, i, e)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
        end subroutine parquet_column_clear_null_elem
    end interface
    !
    ! ---- Typed guards (parquet_columns_util) ----
    !
    ! PRIVATE, unlike the accessors above: only this module and its submodules call them. The
    ! `class`-dummy guards declared further up are one-line forwarders onto these, so a body that
    ! still takes a polymorphic passed object keeps working unchanged.
    interface
        !> Typed `check_kind`: aborts unless the column's active kind is `expected`.
        module subroutine parquet_column_check_kind(col, expected, proc)
            type(parquet_column), intent(in) :: col !! the column.
            integer, intent(in) :: expected         !! the PK_* kind the caller requires.
            character(len=*), intent(in) :: proc    !! calling procedure name (for the message).
        end subroutine parquet_column_check_kind
        !> Typed `check_index`: aborts unless `i` is a valid 1-based row index.
        module subroutine parquet_column_check_index(col, i, proc)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! the offending 1-based row index.
            character(len=*), intent(in) :: proc    !! calling procedure name (for the message).
        end subroutine parquet_column_check_index
        !> Typed `check_element`: aborts unless `1 <= e <= width`.
        module subroutine parquet_column_check_element(col, e, proc)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: e         !! the offending 1-based element index.
            character(len=*), intent(in) :: proc    !! calling procedure name (for the message).
        end subroutine parquet_column_check_element
        !> Typed `check_width`: aborts unless `n` matches the column's own vector width.
        module subroutine parquet_column_check_width(col, n, proc)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: n         !! the supplied element count per row.
            character(len=*), intent(in) :: proc    !! calling procedure name (for the message).
        end subroutine parquet_column_check_width
    end interface
    !
    ! ---- The typed tier's generics: the ONLY names parquet_tables uses (feature_ifx.md) ----
    interface parquet_column_get_at
        module procedure parquet_column_get_at_i32
        module procedure parquet_column_get_at_i64
        module procedure parquet_column_get_at_f32
        module procedure parquet_column_get_at_f64
        module procedure parquet_column_get_at_bool
        module procedure parquet_column_get_at_str
        module procedure parquet_column_get_at_date
        module procedure parquet_column_get_at_time
        module procedure parquet_column_get_at_ts
        module procedure parquet_column_get_at_i32v
        module procedure parquet_column_get_at_i64v
        module procedure parquet_column_get_at_f32v
        module procedure parquet_column_get_at_f64v
        module procedure parquet_column_get_at_boolv
        module procedure parquet_column_get_at_strv
        module procedure parquet_column_get_at_datev
        module procedure parquet_column_get_at_timev
        module procedure parquet_column_get_at_tsv
    end interface parquet_column_get_at
    interface parquet_column_set_at
        module procedure parquet_column_set_at_i32
        module procedure parquet_column_set_at_i64
        module procedure parquet_column_set_at_f32
        module procedure parquet_column_set_at_f64
        module procedure parquet_column_set_at_bool
        module procedure parquet_column_set_at_str
        module procedure parquet_column_set_at_date
        module procedure parquet_column_set_at_time
        module procedure parquet_column_set_at_ts
        module procedure parquet_column_set_at_i32v
        module procedure parquet_column_set_at_i64v
        module procedure parquet_column_set_at_f32v
        module procedure parquet_column_set_at_f64v
        module procedure parquet_column_set_at_boolv
        module procedure parquet_column_set_at_strv
        module procedure parquet_column_set_at_datev
        module procedure parquet_column_set_at_timev
        module procedure parquet_column_set_at_tsv
    end interface parquet_column_set_at
    interface parquet_column_get_elem
        module procedure parquet_column_get_elem_i32v
        module procedure parquet_column_get_elem_i64v
        module procedure parquet_column_get_elem_f32v
        module procedure parquet_column_get_elem_f64v
        module procedure parquet_column_get_elem_boolv
        module procedure parquet_column_get_elem_strv
        module procedure parquet_column_get_elem_datev
        module procedure parquet_column_get_elem_timev
        module procedure parquet_column_get_elem_tsv
    end interface parquet_column_get_elem
    interface parquet_column_set_elem
        module procedure parquet_column_set_elem_i32v
        module procedure parquet_column_set_elem_i64v
        module procedure parquet_column_set_elem_f32v
        module procedure parquet_column_set_elem_f64v
        module procedure parquet_column_set_elem_boolv
        module procedure parquet_column_set_elem_strv
        module procedure parquet_column_set_elem_datev
        module procedure parquet_column_set_elem_timev
        module procedure parquet_column_set_elem_tsv
    end interface parquet_column_set_elem
    interface parquet_column_data_ptr
        module procedure parquet_column_data_ptr_i32
        module procedure parquet_column_data_ptr_i64
        module procedure parquet_column_data_ptr_f32
        module procedure parquet_column_data_ptr_f64
        module procedure parquet_column_data_ptr_bool
        module procedure parquet_column_data_ptr_date
        module procedure parquet_column_data_ptr_time
        module procedure parquet_column_data_ptr_ts
        module procedure parquet_column_data_ptr_i32v
        module procedure parquet_column_data_ptr_i64v
        module procedure parquet_column_data_ptr_f32v
        module procedure parquet_column_data_ptr_f64v
        module procedure parquet_column_data_ptr_boolv
        module procedure parquet_column_data_ptr_datev
        module procedure parquet_column_data_ptr_timev
        module procedure parquet_column_data_ptr_tsv
    end interface parquet_column_data_ptr
    interface parquet_column_is_null
        module procedure parquet_column_is_null_row
        module procedure parquet_column_is_null_elem
    end interface parquet_column_is_null
    interface parquet_column_set_null
        module procedure parquet_column_set_null_row
        module procedure parquet_column_set_null_elem
    end interface parquet_column_set_null
    interface parquet_column_clear_null
        module procedure parquet_column_clear_null_row
        module procedure parquet_column_clear_null_elem
    end interface parquet_column_clear_null
    !
    ! ---- Value append per kind + storage helpers (parquet_columns_mutate, GENERATED) ----
    interface
        !> Appends rows to a PK_INT32 column, growing its storage.
        module subroutine append_values_i32(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), intent(in) :: values(:)      !! rows to append.
        end subroutine append_values_i32
        !> Appends rows to a PK_INT64 column, growing its storage.
        module subroutine append_values_i64(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: values(:)      !! rows to append.
        end subroutine append_values_i64
        !> Appends rows to a PK_FLOAT32 column, growing its storage.
        module subroutine append_values_f32(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real32), intent(in) :: values(:)      !! rows to append.
        end subroutine append_values_f32
        !> Appends rows to a PK_FLOAT64 column, growing its storage.
        module subroutine append_values_f64(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real64), intent(in) :: values(:)      !! rows to append.
        end subroutine append_values_f64
        !> Appends rows to a PK_LOGICAL column, growing its storage.
        module subroutine append_values_bool(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: values(:)      !! rows to append.
        end subroutine append_values_bool
        !> Appends rows to a PK_DATE column, growing its storage.
        module subroutine append_values_date(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_date), intent(in) :: values(:)      !! rows to append.
        end subroutine append_values_date
        !> Appends rows to a PK_TIME column, growing its storage.
        module subroutine append_values_time(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_time), intent(in) :: values(:)      !! rows to append.
        end subroutine append_values_time
        !> Appends rows to a PK_TIMESTAMP column, growing its storage.
        module subroutine append_values_ts(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_timestamp), intent(in) :: values(:)      !! rows to append.
        end subroutine append_values_ts
        !> Appends rows to a PK_INT32_VEC column, growing its storage.
        module subroutine append_values_i32v(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), intent(in) :: values(:,:)      !! rows to append.
        end subroutine append_values_i32v
        !> Appends rows to a PK_INT64_VEC column, growing its storage.
        module subroutine append_values_i64v(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: values(:,:)      !! rows to append.
        end subroutine append_values_i64v
        !> Appends rows to a PK_FLOAT32_VEC column, growing its storage.
        module subroutine append_values_f32v(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real32), intent(in) :: values(:,:)      !! rows to append.
        end subroutine append_values_f32v
        !> Appends rows to a PK_FLOAT64_VEC column, growing its storage.
        module subroutine append_values_f64v(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            real(real64), intent(in) :: values(:,:)      !! rows to append.
        end subroutine append_values_f64v
        !> Appends rows to a PK_LOGICAL_VEC column, growing its storage.
        module subroutine append_values_boolv(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: values(:,:)      !! rows to append.
        end subroutine append_values_boolv
        !> Appends rows to a PK_DATE_VEC column, growing its storage.
        module subroutine append_values_datev(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_date), intent(in) :: values(:,:)      !! rows to append.
        end subroutine append_values_datev
        !> Appends rows to a PK_TIME_VEC column, growing its storage.
        module subroutine append_values_timev(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_time), intent(in) :: values(:,:)      !! rows to append.
        end subroutine append_values_timev
        !> Appends rows to a PK_TIMESTAMP_VEC column, growing its storage.
        module subroutine append_values_tsv(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            type(parquet_timestamp), intent(in) :: values(:,:)      !! rows to append.
        end subroutine append_values_tsv
        !> Rebuilds the active storage so row k becomes the row that was at `idx(k)`. Serves both
        !! `reindex` (a permutation) and `delete_by_mask` (a subset, in order). Values only --
        !! validity is the caller's business.
        module subroutine gather_storage(self, idx)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: idx(:)         !! source row index per destination row.
        end subroutine gather_storage
        !> Copies one range of a gather between two columns, values only: for every `k` in
        !! `lo .. hi`, row `k` of `self` becomes row `idx(k)` of `src`. `self` already holds `src`'s
        !! kind and width and at least `hi` rows (`gather_from` sizes it), and is a different
        !! column from `src`. Serves `gather_from`, which cuts `1 .. size(idx)` into one such range
        !! per thread. The temporal kinds take the row mask here as well, because their null state
        !! lives inside the element being copied; every other kind's mask goes to the bitmap. Never
        !! called for the string kinds (their store rebuilds itself whole) or a container kind.
        module subroutine gather_storage_from(self, src, idx, lo, hi, valid)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: src      !! the source column (unchanged).
            integer(int64), intent(in) :: idx(:)         !! source row index per destination row.
            integer(int64), intent(in) :: lo             !! first destination row of this range.
            integer(int64), intent(in) :: hi             !! last destination row; < lo copies nothing.
            logical, intent(in), optional :: valid(:)    !! per destination row; the temporal kinds null a .false. row.
        end subroutine gather_storage_from
        !> Ensures the active storage is allocated for at least `need_rows` rows, preserving the
        !! rows already stored. A no-op when `cap` is already sufficient.
        !!
        !! **The ONLY place capacity ever grows**, which is what keeps one growth policy in the
        !! library rather than one per caller: `grow_storage` and `reserve` are both two lines on
        !! top of it. Growth is geometric at 1.5x (`max(need_rows, cap + cap/2)`), so appending row
        !! by row is amortised O(1) rather than the O(n^2) an exact-fit realloc per append costs.
        !! `cap + cap/2` rather than `(3*cap)/2` cannot overflow on a very large capacity, and is
        !! the same integer form `parquet_strings`' own `ensure_offsets_cap` uses.
        !!
        !! Does NOT touch `nrows`, and does not resize the validity bitmap -- `grow_storage` calls
        !! `ensure_bitmap` after updating `nrows`, and `ensure_bitmap` sizes itself from `cap`.
        module subroutine ensure_capacity(self, need_rows)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: need_rows      !! rows the storage must hold.
        end subroutine ensure_capacity
        !> Allocates the active storage at ZERO rows, so that a column of a known kind always has
        !! an allocated storage array even before it holds anything.
        !!
        !! **`ensure_capacity` cannot do this**, and that is the whole reason this exists: it
        !! returns early whenever `need_rows <= cap`, which for a fresh column is `0 <= 0`, so a
        !! zero-row column ends up live -- a real kind, a real width, answering `%length()` -- with
        !! no storage array at all. Everything downstream that names the storage then references an
        !! **unallocated allocatable**: `%data_ptr`'s `p => col%i32(1:col%nrows)` and `set_all`'s
        !! whole-array assignment both did, and both are non-conforming however empty the section
        !! is. gfortran, ifx and flang no-op them; nagfor's `-C=array` reports
        !! *"ALLOCATABLE COL%I32 is not currently allocated"* and stops the run. Reached by
        !! `%get` on any table a filter emptied, and by the documented `%add_column(name, empty)`
        !! shape -- ordinary use, not an edge case.
        !!
        !! Allocated at exactly zero rows, so `cap`, `%capacity()` and `size(storage)` all stay 0
        !! and nothing observable changes; the vector kinds keep their first extent, so
        !! `size(p, 1)` is still `width`. The string and container kinds allocate nothing here --
        !! their stores carry their own state and `init` has already created it.
        module subroutine allocate_empty_storage(self)
            class(parquet_column), intent(inout) :: self !! the column, with `kind`/`width` set.
        end subroutine allocate_empty_storage
        !> Reallocates the active storage down to exactly `nrows` rows, releasing any spare
        !! capacity. A no-op when there is none, and on the string kinds (whose own store carries
        !! its capacity). The counterpart of `ensure_capacity`, and the only place capacity shrinks.
        module subroutine shrink_storage(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine shrink_storage
        !> Grows the active storage by `n` rows, preserving existing values. New rows hold
        !! unspecified values for numeric kinds and null elements for temporal kinds.
        module subroutine grow_storage(self, n)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: n              !! number of rows to add (>= 0).
        end subroutine grow_storage
        !> Appends every row of `other`'s active storage to `self`'s (values only; the caller
        !! settles validity). Both columns must already have the same kind and width.
        module subroutine append_storage(self, other)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: other    !! the source column (unchanged).
        end subroutine append_storage
        !> Overwrites rows `at .. at+n-1` of `self`'s active storage with rows `from .. from+n-1`
        !! of `src`'s (values only; the caller settles validity). Both columns must already have
        !! the same kind and width, and every index must already be in range -- `paste` checks all
        !! of that before calling this. Aborts on the string kinds, which cannot be overwritten in
        !! place.
        module subroutine paste_storage(self, src, at, from, n)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: src      !! the source column (unchanged).
            integer(int64), intent(in) :: at             !! 1-based first destination row.
            integer(int64), intent(in) :: from           !! 1-based first source row.
            integer(int64), intent(in) :: n              !! rows to copy (> 0).
        end subroutine paste_storage
        !> Copies the active storage (values only) into `out`, which must already have the same
        !! kind and geometry.
        module subroutine copy_storage(self, out)
            class(parquet_column), intent(in) :: self  !! the source column.
            type(parquet_column), intent(inout) :: out !! the destination column.
        end subroutine copy_storage
    end interface
    !
    ! ---- Shared internal helpers (parquet_columns_util) ----
    ! Declared here rather than contained in this module so that EVERY submodule can call them:
    ! a procedure contained in one submodule is invisible to its siblings, and one contained in
    ! the module itself is reported as unused when this file is compiled on its own.
    interface
        !> Aborts unless the column's active kind is `expected`.
        module subroutine check_kind(self, expected, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer, intent(in) :: expected           !! the PK_* kind the caller requires.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_kind
        !> Aborts unless `i` is a valid 1-based row index.
        module subroutine check_index(self, i, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! the offending 1-based row index.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_index
        !> Aborts unless `e` is a valid 1-based element index WITHIN a row, i.e. `1 <= e <= width`.
        !!
        !! The companion to `check_index` for the element forms of the validity API. Keeping the
        !! two separate is what makes a flat element index passed where a row was meant fail with
        !! a message about the right axis: on a scalar column `width` is 1, so anything but 1 is
        !! rejected immediately rather than silently addressing another row's storage.
        module subroutine check_element(self, e, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: e           !! the offending 1-based element index.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_element
        !> Aborts unless `n` matches the column's own row count.
        module subroutine check_nrows(self, n, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: n           !! the supplied row count.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_nrows
        !> Aborts unless `n` matches the column's own vector width.
        module subroutine check_width(self, n, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: n           !! the supplied element count per row.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_width
        !> Number of validity bits the column needs: one per ELEMENT, so `nrows*width` for a
        !! vector kind and `nrows` for a scalar kind (RF8).
        !!
        !! `type`, not `class`, so that a procedure in the typed tier may call it -- a `class`
        !! dummy here would hand a `type` actual to a `class` one and rebuild the descriptor block
        !! that tier exists to remove (the same conversion `ensure_bitmap` needed). Widening it
        !! this way is source-compatible: a `class` actual passes to a `type` dummy freely, so
        !! every existing caller still compiles unchanged.
        pure module function bits_needed(col) result(res)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64) :: res                   !! required bit count.
        end function bits_needed
        !> Number of int64 blocks needed to hold `nbits` bits.
        pure module function blocks_for(nbits) result(res)
            integer(int64), intent(in) :: nbits !! bit count.
            integer(int64) :: res               !! block count.
        end function blocks_for
        !> Whether bit `b` (1-based) of a bitmap is set. An unallocated bitmap has no set bits.
        pure module function bit_test(map, b) result(res)
            integer(int64), allocatable, intent(in) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: b                   !! 1-based bit index.
            logical :: res                                    !! .true. when the bit is set.
        end function bit_test
        !> Sets bit `b` (1-based) of an allocated bitmap.
        !!
        !! **NOT `pure`, and that is load-bearing rather than an oversight.** The body updates the
        !! bitmap word under `!$omp atomic`, and gfortran rejects an OpenMP directive inside a
        !! `pure` procedure outright ("OpenMP directive is not pure and thus may not appear in a
        !! PURE procedure"). Restoring `pure` therefore means removing the atomic, which silently
        !! reintroduces a lost-update race: validity is packed `BITS_PER_BLOCK` elements to one
        !! `integer(int64)`, so two threads writing *different rows* of one column collide whenever
        !! those rows share a block -- see feature_risks.md Risk-135. Nothing here needs purity;
        !! no caller is `pure` or `elemental`, and no `do concurrent` reaches this.
        module subroutine bit_set(map, b)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: b         !! 1-based bit index.
        end subroutine bit_set
        !> Clears bit `b` (1-based) of an allocated bitmap. Not `pure`, for the reason `bit_set`
        !! records.
        module subroutine bit_clear(map, b)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: b         !! 1-based bit index.
        end subroutine bit_clear
        !> Sets every bit in the 1-based inclusive range `lo .. hi`, a word at a time.
        !!
        !! The bulk counterpart of `bit_set`, for the several places that mark a *contiguous run* of
        !! elements null. Doing it one bit at a time costs a call that cannot be inlined plus a
        !! divide and a `mod` per bit; here the interior words are stored whole and only the two
        !! ragged ends are masked. A range that does not start or end on a word boundary is the
        !! normal case, not an edge case -- `width` need not divide 64 -- so both ends are handled,
        !! and a range lying inside one word is handled by the first end alone.
        !!
        !! **Still `pure`, unlike `bit_set`/`bit_clear`, because nothing reaches it concurrently.**
        !! Its only callers are `grow_rows` and `insert_null_rows`
        !! (`src/parquet_columns_structural.f90`), both row-structural operations that the table
        !! layer refuses outright on a shared table. Give it the same atomic treatment as `bit_set`
        !! if that ever stops being true -- its two ragged-end words are read-modify-writes and
        !! would race exactly as a single bit does.
        pure module subroutine bits_set_range(map, lo, hi)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: lo        !! first 1-based bit to set.
            integer(int64), intent(in) :: hi        !! last 1-based bit to set; < lo sets nothing.
        end subroutine bits_set_range
        !> Clears every bit in the 1-based inclusive range `lo .. hi`, a word at a time. The
        !! counterpart of `bits_set_range`, kept separate for the same reason `bit_clear` is
        !! separate from `bit_set`.
        pure module subroutine bits_clear_range(map, lo, hi)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: lo        !! first 1-based bit to clear.
            integer(int64), intent(in) :: hi        !! last 1-based bit to clear; < lo clears nothing.
        end subroutine bits_clear_range
        !> Copies the bit run `src_lo .. src_lo+nbits-1` of `src` onto `dst_lo ..` of `dst`,
        !! REPLACING the destination bits rather than merging into them.
        !!
        !! The two runs need not be aligned to each other: the destination is walked a word at a
        !! time and each word is assembled from the one or two source words that overlap it, which
        !! is what makes this useful for `append`/`paste`, where the destination offset is whatever
        !! the existing row count happens to be. `merge_only_set` keeps the destination's own set
        !! bits (an OR rather than a replace), which is what `append` wants and `paste` does not --
        !! see their own doc-comments for why those two differ.
        pure module subroutine bits_copy_range(dst, dst_lo, src, src_lo, nbits, merge_only_set)
            integer(int64), intent(inout) :: dst(:) !! destination bitmap.
            integer(int64), intent(in) :: dst_lo    !! first 1-based destination bit.
            integer(int64), intent(in) :: src(:)    !! source bitmap.
            integer(int64), intent(in) :: src_lo    !! first 1-based source bit.
            integer(int64), intent(in) :: nbits     !! how many bits to copy; <= 0 copies nothing.
            logical, intent(in) :: merge_only_set   !! .true. ORs into the destination instead of replacing.
        end subroutine bits_copy_range
        !> Ensures the bitmap exists and covers every element, zero-filling new blocks (0 = valid),
        !! and marks the column bitmap-backed.
        !!
        !! Takes a NON-polymorphic dummy, unlike its neighbours here: the typed `set_null` forms
        !! call it, and a typed body handing a `type(parquet_column)` to a `class` dummy rebuilds
        !! the whole descriptor block the typed tier exists to remove (feature_ifx.md). It is
        !! private plumbing and never a binding, so it needs no polymorphic form at all -- every
        !! existing caller passes a `class` actual, which a `type` dummy accepts for free.
        module subroutine ensure_bitmap(col)
            type(parquet_column), intent(inout) :: col !! the column.
        end subroutine ensure_bitmap
        !> Whether this column's validity storage already exists, i.e. whether nulling an element
        !! would still have to ALLOCATE something.
        !!
        !! The answer depends on which of the three validity mechanisms the kind uses (see
        !! `set_null`): a bitmap kind answers whether its bitmap is allocated, a string kind asks
        !! its `parquet_string_column`, and a temporal kind is **always** `.true.` -- its null state
        !! lives in the element itself, so there is nothing to allocate and never was.
        !!
        !! Exists for concurrency: nulling elements of the same column from two threads races on
        !! that lazy allocation, and this plus `ensure_validity` is how a caller (or the table
        !! layer's own guard) removes the race rather than detecting it.
        module function has_validity_storage(self) result(res)
            class(parquet_column), intent(in) :: self !! the column.
            logical :: res                            !! .true. when nulling would allocate nothing.
        end function has_validity_storage
        !> Materializes this column's validity storage now, leaving every element valid.
        !!
        !! Idempotent, and a no-op for a temporal kind (which has nothing to allocate). Changes no
        !! value and no null state -- only *when* the allocation happens. See
        !! `has_validity_storage` for why that matters.
        module subroutine ensure_validity(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine ensure_validity
        !> Releases the bitmap in O(1): every row becomes valid and a null-free column costs one
        !! scalar again (R2).
        module subroutine drop_bitmap(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine drop_bitmap
        !> Whether a kind stores its null state inside each element (the temporal kinds).
        pure module function is_temporal_kind(kind) result(res)
            integer, intent(in) :: kind !! a PK_* discriminator.
            logical :: res              !! .true. for the date/time/timestamp kinds.
        end function is_temporal_kind
        !> Whether a kind delegates its storage and validity to parquet_string_column.
        pure module function is_string_kind(kind) result(res)
            integer, intent(in) :: kind !! a PK_* discriminator.
            logical :: res              !! .true. for PK_STRING and PK_STRING_VEC.
        end function is_string_kind
        !> Fixed-length kind name, used to build this module's error messages.
        pure module function kind_text(kind) result(res)
            integer, intent(in) :: kind !! a PK_* discriminator.
            character(len=16) :: res    !! the kind's name, blank-padded.
        end function kind_text
    end interface
    !
contains
    !
    ! ==================================================================================
    ! Shared private helpers -- host-associated by every submodule.
    ! ==================================================================================
    !
    !> The column's active kind discriminator. `PK_NONE` until `init` is called.
    pure function kindof(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer :: res                            !! the active PK_* constant.
        res = self%kind
    end function kindof
    !
    !> Number of rows the column currently holds.
    pure function length(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer(int64) :: res                     !! the row count.
        res = self%nrows
    end function length
    !
    !> Rows the active storage is allocated for. Always >= `length()`; the difference is spare
    !! capacity that `%append` can fill without reallocating, and that `%shrink_to_fit` releases.
    !!
    !! **Reported in ROWS for every kind**, which is what makes `capacity() >= length()` a
    !! meaningful comparison whatever the column holds. The two string kinds forward to the
    !! embedded `parquet_string_column`, whose own capacity counts ELEMENTS -- so it is divided by
    !! the width to get rows, exactly as `length()` counts rows rather than the `nrows*width`
    !! elements a vector string column stores (RF6). A column with no kind yet reports 0.
    !!
    !! **Not `pure`, unlike `length`**, only because the string store's own `%capacity` is not.
    function capacity(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer(int64) :: res                     !! rows the storage is allocated for.
        if (is_string_kind(self%kind)) then
            res = 0_int64
            if (allocated(self%str)) res = parquet_string_column_capacity(self%str)/int(self%width, int64)
        else
            res = self%cap
        end if
    end function capacity
    !
    !> Bytes currently occupied by the null bitmap — **0 for a null-free column**, which is the
    !! whole point of the sparse representation (R2): nothing is allocated until the column
    !! actually holds a null. Also the cheapest way for a test or a memory report to prove that.
    !! Always 0 for the string and temporal kinds, whose null state is not a column bitmap.
    pure function validity_bytes(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer(int64) :: res                     !! bitmap size in bytes.
        if (allocated(self%validity)) then
            res = size(self%validity, kind=int64)*8_int64
        else
            res = 0_int64
        end if
    end function validity_bytes
    !
    !> Values per row: 1 for every scalar kind, the vector width for a *_VEC kind.
    pure function colwidth(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer(int32) :: res                     !! the column width.
        res = self%width
    end function colwidth
    !
    !> Copies a human-readable name for a PK_* kind out (for error messages and callers that
    !! dispatch on `kindof`).
    subroutine parquet_kind_name(kind, name)
        integer, intent(in) :: kind                        !! a PK_* discriminator.
        character(len=:), allocatable, intent(out) :: name !! the kind's name.
        name = trim(kind_text(kind))
    end subroutine parquet_kind_name
    !
    !> Whether a PK_* discriminator names a CONTAINER kind, i.e. one whose storage is a
    !! `parquet_container_column` rather than a Fortran array.
    !!
    !! Published so that no call site anywhere in the library has to spell out the three-way test.
    !! There is deliberately one predicate rather than three: a fourth container kind would
    !! otherwise have to be found by grepping for whichever of `PK_LIST`/`PK_MAP`/`PK_STRUCT`
    !! someone happened to write first.
    pure function parquet_kind_is_container(kind) result(res)
        integer, intent(in) :: kind !! a PK_* discriminator.
        logical :: res              !! .true. for PK_LIST, PK_MAP and PK_STRUCT.
        res = (kind == PK_LIST .or. kind == PK_MAP .or. kind == PK_STRUCT)
    end function parquet_kind_is_container
    !
    !
end module parquet_columns ! GCOVR_EXCL_LINE
