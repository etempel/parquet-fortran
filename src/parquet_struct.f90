!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> STRUCT column storage: `parquet_struct_column`, plus `parquet_struct_row`, a non-owning
!! handle to one of its rows.
!!
!! A struct column's rows each hold one value per DECLARED FIELD, and different fields may have
!! different types -- `struct<name: string, age: int32>` is one column whose every row carries a
!! string and an integer. That is what the Parquet `STRUCT` logical type describes.
!!
!! Four things are worth knowing before using it:
!!
!! * **Each field is ONE `parquet_column`.** Field `k`'s values for every row live in
!!   `fields(k)`, so each field's kind, its per-ROW null bitmap, its unit string and every value
!!   accessor come from a type that already exists and is already tested. There is no separate
!!   array of field kinds to keep in step -- `fields(k)%kindof()` *is* the field's kind, so the
!!   two cannot disagree.
!! * **The field set is fixed at `%init` and cannot change.** `%init(["a","b"], [PK_INT32,
!!   PK_STRING])` makes a two-field column and every later operation is checked against it.
!!   Inferring the fields from the first row appended would be the "sized/typed from the first
!!   element" bug class, and would leave `%field_count()` unable to answer for an empty column --
!!   which the writer has to ask before any row exists.
!! * **Row nullness and field nullness are DIFFERENT things, and both exist.** A row can be null
!!   (the struct instance itself is absent); a field inside a present row can be null (the struct
!!   is there and one of its values is missing). A row whose every field is null is NOT a null
!!   row. `%is_null(i)` answers the first question and `%get`'s `is_valid` argument the second.
!! * **`%field(name)` narrows a handle; `%get` materializes.** A Fortran function has one
!!   compile-time return type, so `%field` cannot hand back the field's value -- it returns
!!   another handle, and `call h%field("age")%get(age)` is the one-line idiom. That is also what
!!   makes deeper navigation the same shape at every depth.
!!
!! **Storage layout.** `fields(k)` is `nrows` long for every `k`, whatever the nulls; the struct's
!! own row nullness is a separate lazy bitmap. That is Arrow's own layout -- a `StructArray` is a
!! validity bitmap over N equally-long child arrays -- so handing these buffers to Arrow is a copy
!! rather than a translation.
!!
!! **Null rows keep their field values.** Marking a row null does not touch any field's stored
!! value, exactly as `parquet_list_column%set_null` leaves a nulled row's elements in place. Such
!! values are unreachable from that moment on: `%get` on a null row reports `is_valid = .false.`
!! and yields the type's default. **They also do not survive a file round trip** -- Parquet's
!! definition levels cannot encode "the struct is absent but its field is present", so a written
!! and re-read column has every field of every null row null. Measured against Arrow 25.0.0.
!!
!! Depends only on `iso_fortran_env` plus `parquet_columns` (which brings the two element-domain
!! modules) and `parquet_settings_base` for the one warning it can emit. It reaches no `bind(C)`
!! interface -- see `%summary`, which composes a string instead of writing to a unit for exactly
!! that reason.
module parquet_struct
    use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
    ! The TYPED (non-polymorphic) tier of parquet_column. Per-CELL access to a field goes through
    ! these free generics, NEVER through a type-bound call on `self%fields(k)`: passing a
    ! `type(parquet_column)` actual to a `class` passed-object dummy in another compilation unit
    ! makes ifx build a 21-record runtime type descriptor in STATIC storage, in the caller's
    ! prologue, on every call. The same rule parquet_list follows, and for the same measurement.
    use parquet_columns, only : parquet_column, parquet_container_column, parquet_kind_name, &
        parquet_column_get_at, parquet_column_set_at, parquet_column_is_null, &
        parquet_column_set_null, parquet_column_clear_null, &
        parquet_column_container, parquet_kind_is_container, &
        PK_NONE, PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, &
        PK_DATE, PK_TIME, PK_TIMESTAMP, PK_STRUCT, PK_LIST, PK_MAP
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    ! The emit channel for the one soft-fail warning this module can print, plus the two knobs
    ! that govern it. A module re-exports every setting its own code reads -- CLAUDE.md's rule
    ! under "Nested submodule tree" -- so that `use parquet_struct` alone can silence it.
    use parquet_settings_base, only : parquet_emit_warning, &
        parquet_get_verbosity, parquet_set_verbosity, &
        parquet_get_message_stream, parquet_set_message_stream
    !
    implicit none
    private
    !
    public :: parquet_struct_column
    public :: parquet_struct_row
    !
    ! Re-exported so that `use parquet_struct` alone is enough to declare a struct column, name a
    ! field's kind and read a temporal field back -- the entry-module rule in CLAUDE.md, and what
    ! test_module_surface_struct (test/test_module_surface.f90) asserts with a single import.
    public :: parquet_column, parquet_container_column, parquet_kind_name
    public :: PK_NONE, PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING
    public :: PK_DATE, PK_TIME, PK_TIMESTAMP, PK_STRUCT
    public :: parquet_date, parquet_time, parquet_timestamp
    public :: parquet_get_verbosity, parquet_set_verbosity
    public :: parquet_get_message_stream, parquet_set_message_stream
    !
    ! INTERNAL API, on the same terms as parquet_list's own accessor tier: a struct column's
    ! fields, names and row validity, so that the READ and WRITE paths
    ! (src/parquet_read_struct.f90, src/parquet_write_struct.f90) can reach contiguous storage
    ! without a per-cell binding call and without a per-row allocation.
    ! `src/parquet.f90` privatises all four again, so the `use parquet` surface is unchanged.
    public :: parquet_struct_column_field
    public :: parquet_struct_column_names
    public :: parquet_struct_column_row_validity
    public :: parquet_struct_column_build
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_struct: "
    !
    !> Bits per validity-bitmap block, matching `parquet_columns`' own bitmap layout.
    integer(int64), parameter :: BITS_PER_BLOCK = 64_int64
    !
    !> Smallest row-bitmap allocation, so that a column filled row by row does not reallocate on
    !! each of its first few appends.
    integer(int64), parameter :: MIN_ROW_CAP = 8_int64
    !
    !> Largest field count Arrow can address: `arrow::StructType::num_fields()` returns `int`.
    !!
    !! Absurd in practice and guarded anyway, per `.claude/rules/cpp-wrapper.md`'s "Guarding a hard Arrow int32-only
    !! ceiling" -- the same constant the writer's column-count guard uses, so that a struct with
    !! an impossible field count fails here, naming the column, rather than inside Arrow.
    integer, parameter :: MAX_FIELDS = 2147483647
    !
    !> A struct column: `nrows` rows, each holding one value per declared field.
    !!
    !! Extends `parquet_container_column`, so a `parquet_column` can hold one through
    !! `%adopt_container` and reach it without naming this type. See the module doc for the
    !! storage layout and the row/field null distinction.
    type, extends(parquet_container_column) :: parquet_struct_column
        private
        !> One entry per DECLARED field, fixed at `%init`. `%size()` is the field count.
        !!
        !! A `parquet_string_column` rather than a `character(len=:), allocatable :: names(:)`
        !! for the same reason a list column's payload is a `parquet_column`: it is packed, it
        !! needs no common declared length across the field names, and it already has the
        !! lookup and copy-out operations this type would otherwise write again.
        type(parquet_string_column) :: field_names
        !> One column per declared field, each exactly `nrows_` long. Carries that field's own
        !! kind, per-row validity and unit string, so none of that is duplicated here.
        type(parquet_column), allocatable :: fields(:)
        !> ROW-level null bitmap, 1 = null, `BITS_PER_BLOCK` rows to a block. LAZY: a column with
        !! no null row allocates nothing at all and `has_nulls_` stays .false.
        integer(int64), allocatable :: validity(:)
        integer(int64) :: nrows_ = 0            !! rows stored.
        logical :: has_nulls_ = .false.         !! .true. while the row bitmap is materialized.
    contains
        ! --- the deferred face parquet_columns reaches this type through (one per binding) ---
        procedure :: kindof => sc_kindof                 !! Always PK_STRUCT.
        procedure :: nrows => sc_nrows                   !! Rows stored.
        procedure :: clone_into => sc_clone_into         !! Allocate an independent copy.
        procedure :: gather_rows => sc_gather_rows       !! Rebuild so row k becomes old row idx(k).
        procedure :: append_from => sc_append_from       !! Append every row of another struct column.
        procedure :: grow_rows => sc_grow_rows           !! Append n null rows.
        procedure :: reserve_rows => sc_reserve_rows     !! Reserve row capacity.
        procedure :: ensure_validity => sc_ensure_validity !! Materialize the row bitmap eagerly.
        procedure :: kind_text => sc_kind_text           !! e.g. "struct<name:string,age:int32>".
        procedure :: is_null_row => sc_is_null_row       !! Whether row i is a null struct.
        procedure :: set_null_row => sc_set_null_row     !! Mark row i a null struct.
        procedure :: clear_null_row => sc_clear_null_row !! Mark row i present again.
        ! --- lifecycle ---
        procedure :: init                                !! Fix the field set and (optionally) create null rows.
        procedure :: clear                               !! Release everything and reset to an uninitialized column.
        procedure :: deep_copy                           !! Independent copy of names, fields and validity.
        procedure :: move_from                           !! Take over another column's storage, leaving it empty.
        procedure :: adopt_fields                        !! Build from moved-in field columns and names.
        ! --- queries ---
        procedure :: size => sc_nrows_public             !! Rows stored (alias of %nrows()).
        procedure :: is_init                             !! Whether %init has fixed a field set.
        procedure :: field_count                         !! Number of declared fields.
        procedure :: field_name                          !! Name of the k-th declared field.
        procedure :: field_index                         !! 1-based position of a named field, or 0.
        procedure :: field_kind                          !! PK_* kind of the k-th declared field.
        procedure :: null_count                          !! Number of null rows.
        procedure :: capacity                            !! Rows the storage is allocated for.
        procedure :: has_validity_storage                !! Whether nulling a row would still have to allocate.
        procedure, private :: is_null_i32                !! int32 specific of is_null.
        procedure, private :: is_null_i64                !! int64 specific of is_null.
        generic :: is_null => is_null_i32, is_null_i64   !! Whether row i is a null struct.
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
        procedure :: append_row                          !! Append one PRESENT row with every field null.
        procedure :: append_null_row                     !! Append one null (absent) struct row.
        procedure, private :: set_null_i32               !! int32 specific of set_null.
        procedure, private :: set_null_i64               !! int64 specific of set_null.
        generic :: set_null => set_null_i32, set_null_i64 !! Mark row i null; see the module doc.
        procedure, private :: clear_null_i32             !! int32 specific of clear_null.
        procedure, private :: clear_null_i64             !! int64 specific of clear_null.
        generic :: clear_null => clear_null_i32, clear_null_i64 !! Mark row i present again.
        procedure, private :: sfn_k32                    !! set_field_null, int32 row + field index.
        procedure, private :: sfn_k64                    !! set_field_null, int64 row + field index.
        procedure, private :: sfn_n32                    !! set_field_null, int32 row + field name.
        procedure, private :: sfn_n64                    !! set_field_null, int64 row + field name.
        !> Marks one field of one row null, leaving the struct row itself present.
        generic :: set_field_null => sfn_k32, sfn_k64, sfn_n32, sfn_n64
        ! set_field specifics: int64 row index, field index.
        procedure, private :: sf_k64_i32
        procedure, private :: sf_k64_i64
        procedure, private :: sf_k64_f32
        procedure, private :: sf_k64_f64
        procedure, private :: sf_k64_bool
        procedure, private :: sf_k64_str
        procedure, private :: sf_k64_date
        procedure, private :: sf_k64_time
        procedure, private :: sf_k64_ts
        ! set_field specifics: int32 row index, field index.
        procedure, private :: sf_k32_i32
        procedure, private :: sf_k32_i64
        procedure, private :: sf_k32_f32
        procedure, private :: sf_k32_f64
        procedure, private :: sf_k32_bool
        procedure, private :: sf_k32_str
        procedure, private :: sf_k32_date
        procedure, private :: sf_k32_time
        procedure, private :: sf_k32_ts
        ! set_field specifics: int64 row index, field name.
        procedure, private :: sf_n64_i32
        procedure, private :: sf_n64_i64
        procedure, private :: sf_n64_f32
        procedure, private :: sf_n64_f64
        procedure, private :: sf_n64_bool
        procedure, private :: sf_n64_str
        procedure, private :: sf_n64_date
        procedure, private :: sf_n64_time
        procedure, private :: sf_n64_ts
        ! set_field specifics: int32 row index, field name.
        procedure, private :: sf_n32_i32
        procedure, private :: sf_n32_i64
        procedure, private :: sf_n32_f32
        procedure, private :: sf_n32_f64
        procedure, private :: sf_n32_bool
        procedure, private :: sf_n32_str
        procedure, private :: sf_n32_date
        procedure, private :: sf_n32_time
        procedure, private :: sf_n32_ts
        !> Writes one field of one row, dispatched by the value's type. The field may be named or
        !! addressed by its 1-based index; the index form is the primitive and the one to use in a
        !! loop, with %field_index used once outside it.
        generic :: set_field => sf_k64_i32, sf_k64_i64, sf_k64_f32, sf_k64_f64, sf_k64_bool, sf_k64_str, sf_k64_date, &
            sf_k64_time, sf_k64_ts, sf_k32_i32, sf_k32_i64, sf_k32_f32, sf_k32_f64, sf_k32_bool, sf_k32_str, &
            sf_k32_date, sf_k32_time, sf_k32_ts, sf_n64_i32, sf_n64_i64, sf_n64_f32, sf_n64_f64, sf_n64_bool, &
            sf_n64_str, sf_n64_date, sf_n64_time, sf_n64_ts, sf_n32_i32, sf_n32_i64, sf_n32_f32, sf_n32_f64, &
            sf_n32_bool, sf_n32_str, sf_n32_date, sf_n32_time, sf_n32_ts        !
        ! THIS TYPE DELIBERATELY HAS NO `final` PROCEDURE, and one must not be added -- see the
        ! same note on parquet_container_column (src/parquet_columns.f90), parquet_list_column
        ! (src/parquet_list.f90) and parquet_string (src/parquet_strings.f90). It owns nothing the
        ! language does not already free: two allocatable components and one plain derived-type
        ! component, no C++ handle and no OpenMP lock. Three shipped properties depend on the
        ! absence -- nagfor 7.2 emits invalid C when finalizing an ARRAY whose element type has a
        ! finalizable COMPONENT (and one of these will sit inside a parquet_column inside a
        ! parquet_table_column inside an array), gfortran refuses a finalizable type in an OpenMP
        ! `private()` clause, and intrinsic assignment to or from one runs the finalizer twice per
        ! iteration -- which `h = sc%view(i)` in a user loop would pay on every row.
    end type parquet_struct_column
    !
    !> A lightweight, non-owning handle to one row of a `parquet_struct_column`, optionally
    !! narrowed to one field of that row.
    !!
    !! Holds a column pointer, a 1-based row index and a field slot, and resolves lazily on
    !! access, so it stays valid across appends and reserves of the referenced column. It is
    !! invalidated by anything that changes the row set (`clear`, `move_from`, a `gather_rows`
    !! rebuild) or by the column going out of scope. **The referenced column must be declared with
    !! the `target` attribute and must outlive the handle** -- the same contract
    !! `parquet_list_column%view` documents, and F2018 15.5.2.4 leaves the stored pointer
    !! undefined otherwise. Only nagfor's `-C=dangling` reports a violation; gfortran, ifx and
    !! flang all run the non-conforming form perfectly happily.
    !!
    !! `field_idx` is 0 for a handle denoting the whole row -- what `%view(i)` returns, on which
    !! `%field`/`%is_null`/the introspection queries are valid and `%get` is not -- and >0 for one
    !! narrowed by `%field(name)`, on which `%get` is valid.
    type :: parquet_struct_row
        private
        class(parquet_struct_column), pointer :: col => null() !! referenced column (borrowed).
        integer(int64) :: idx = 0                              !! 1-based row index.
        integer :: field_idx = 0                               !! 0 = whole row; >0 = narrowed field.
    contains
        procedure :: is_valid => psr_is_valid       !! Whether this handle refers to a live row.
        procedure :: is_null => psr_is_null         !! Whether the referenced ROW is a null struct.
        procedure :: row_index => psr_row_index     !! The 1-based row this handle refers to.
        procedure :: nested => psr_nested           !! The inner container, when the field is one.
        procedure :: field_count => psr_field_count !! Number of declared fields.
        procedure :: field_name => psr_field_name   !! Name of the k-th declared field.
        procedure :: is_narrowed => psr_is_narrowed !! Whether %field has narrowed this handle.
        procedure :: field_kind => psr_field_kind   !! PK_* kind of the narrowed field.
        !> Narrows this handle to one named field, returning another handle.
        !!
        !! A FUNCTION returning `type(parquet_struct_row)`, never the field's own value: a Fortran
        !! function has one compile-time return type and cannot vary it by a runtime string. So
        !! `call h%field("age")%get(age)` is the idiom, and it is the same shape at every depth.
        !!
        !! An unknown name is a "not found" lookup, not a wrong-kind access, so it takes the
        !! campaign's soft-fail policy: it aborts by default, and with `warn=.true.` emits one
        !! warning and returns a handle whose `%is_valid()` is `.false.`. There is deliberately no
        !! `found=` argument -- `%is_valid()` already is one.
        procedure :: field => psr_field
        procedure, private :: psr_get_i32           !! get specific for an int32 field.
        procedure, private :: psr_get_i64           !! get specific for an int64 field.
        procedure, private :: psr_get_f32           !! get specific for a float32 field.
        procedure, private :: psr_get_f64           !! get specific for a float64 field.
        procedure, private :: psr_get_bool          !! get specific for a logical field.
        procedure, private :: psr_get_str           !! get specific for a string field.
        procedure, private :: psr_get_date          !! get specific for a date field.
        procedure, private :: psr_get_time          !! get specific for a time field.
        procedure, private :: psr_get_ts            !! get specific for a timestamp field.
        procedure, private :: psr_getn_i32          !! get specific naming an int32 field.
        procedure, private :: psr_getn_i64          !! get specific naming an int64 field.
        procedure, private :: psr_getn_f32          !! get specific naming a float32 field.
        procedure, private :: psr_getn_f64          !! get specific naming a float64 field.
        procedure, private :: psr_getn_bool         !! get specific naming a logical field.
        procedure, private :: psr_getn_str          !! get specific naming a string field.
        procedure, private :: psr_getn_date         !! get specific naming a date field.
        procedure, private :: psr_getn_time         !! get specific naming a time field.
        procedure, private :: psr_getn_ts           !! get specific naming a timestamp field.
        !> Materializes the NARROWED field's value, optionally reporting whether it is null.
        !!
        !! Valid only on a handle `%field` has narrowed, which is what deeper navigation ends in.
        !! To read a field of a whole-row handle in one call, use `%get_field(name, value)`.
        generic :: get => psr_get_i32, psr_get_i64, psr_get_f32, psr_get_f64, psr_get_bool, &
            psr_get_str, psr_get_date, psr_get_time, psr_get_ts
        !> Materializes a NAMED field's value in one call: `call h%get_field("age", age)`.
        !!
        !! A separate name rather than another `%get` form, for two reasons. The generic would be
        !! ambiguous -- `get(value_character, is_valid)` and `get(name, value_logical, is_valid)`
        !! are not distinguishable under F2018 15.4.3.4.5, and gfortran says so. And it pairs with
        !! `parquet_struct_column%set_field`, so reading and writing a named field read alike.
        generic :: get_field => psr_getn_i32, psr_getn_i64, psr_getn_f32, psr_getn_f64, &
            psr_getn_bool, psr_getn_str, psr_getn_date, psr_getn_time, psr_getn_ts
        !
        ! NO `final` HERE EITHER, and for the stronger of the two reasons: this is a NON-OWNING
        ! handle -- a borrowed pointer and two integers -- so there is nothing to release. See
        ! parquet_string (src/parquet_strings.f90), which carried one, had it removed, and records
        ! the three properties that depend on its absence.
    end type parquet_struct_row
    !
contains
    !
    ! ==================================================================================
    ! The deferred face: what parquet_columns reaches this type through
    ! ==================================================================================
    !
    !> A struct column is always `PK_STRUCT`; see `parquet_container_column%kindof`.
    pure function sc_kindof(self) result(res)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer :: res                                   !! always PK_STRUCT.
        res = PK_STRUCT
    end function sc_kindof
    !
    !> Number of rows stored; see `parquet_container_column%nrows`.
    pure function sc_nrows(self) result(n)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64) :: n                              !! rows stored.
        n = self%nrows_
    end function sc_nrows
    !
    !> Allocates `out` as an independent copy of this column; see
    !! `parquet_container_column%clone_into`.
    subroutine sc_clone_into(self, out)
        class(parquet_struct_column), intent(in) :: self                 !! the source column.
        class(parquet_container_column), allocatable, intent(out) :: out !! the copy, allocated here.
        type(parquet_struct_column), allocatable :: cp
        allocate(cp)
        call self%deep_copy(cp)
        call move_alloc(cp, out)
    end subroutine sc_clone_into
    !
    !> Rebuilds the column so row k becomes the row that was at `idx(k)`; see
    !! `parquet_container_column%gather_rows`.
    !!
    !! Every field is rebuilt by ONE call to `parquet_column%gather`, which is a single
    !! kind-dispatched pass carrying that field's own validity and working for a string field --
    !! none of which this module would want to reimplement once per kind.
    subroutine sc_gather_rows(self, idx)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: idx(:)                !! 1-based source row per destination row.
        integer(int64) :: n, k
        integer :: j
        integer(int64), allocatable :: new_valid(:)
        logical, allocatable :: src_null(:)
        n = size(idx, kind=int64)
        do k = 1_int64, n
            if (idx(k) < 1_int64 .or. idx(k) > self%nrows_) then
                error stop EP//"gather_rows: source row index out of range"
            end if
        end do
        if (.not. allocated(self%fields)) then
            ! An uninitialized column has no rows, so `idx` is necessarily empty and there is
            ! nothing to rebuild.
            self%nrows_ = 0_int64
            return
        end if
        ! Row nullness is read BEFORE anything is overwritten: `self%validity` is replaced below,
        ! so a second pass over it afterwards would be reading the DESTINATION while asking about
        ! the SOURCE -- correct only by accident when the permutation happens to be the identity.
        allocate(src_null(max(n, 1_int64)))
        do k = 1_int64, n
            src_null(k) = row_is_null(self, idx(k))
        end do
        do j = 1, size(self%fields)
            call self%fields(j)%gather(idx)
        end do
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
    end subroutine sc_gather_rows
    !
    !> Appends `n` null rows; see `parquet_container_column%grow_rows`.
    subroutine sc_grow_rows(self, n)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                     !! rows to append.
        integer(int64) :: k
        if (n < 0_int64) error stop EP//"grow_rows: negative row count"
        if (n == 0_int64) return
        call require_init(self, "grow_rows")
        do k = 1_int64, n
            call self%append_null_row()
        end do
    end subroutine sc_grow_rows
    !
    !> Appends every row of `src`; see `parquet_container_column%append_from`.
    !!
    !! **A struct has no offsets, so there is nothing to rebase** -- every field column is exactly
    !! `nrows_` long and concatenation is one `%append` per field. What it has instead is the
    !! strictest layout check of the three, because two structs can agree on field COUNT and
    !! disagree on everything else:
    !!
    !! * the field count must match;
    !! * field `j`'s NAME must match, in order -- `struct<a,b>` and `struct<b,a>` hold the same
    !!   fields and would silently transpose their columns;
    !! * field `j`'s KIND must match.
    !!
    !! The message names the first field that differs, because "these structs are incompatible"
    !! on a twenty-field type sends the reader looking through all twenty.
    subroutine sc_append_from(self, src)
        class(parquet_struct_column), intent(inout) :: self !! the destination column.
        class(parquet_container_column), intent(in) :: src  !! rows to append, left unchanged.
        integer(int64) :: k, m, n0
        integer :: j
        character(len=:), allocatable :: mine, theirs, nm_a, nm_b, kt_a, kt_b
        character(len=32) :: got, want
        select type (src)
        type is (parquet_struct_column)
            m = src%nrows_
            if (m == 0_int64) return
            call require_init(self, "append_from")
            if (.not. allocated(src%fields)) return
            if (size(self%fields) /= size(src%fields)) then
                write(got, "(I0)") size(src%fields)
                write(want, "(I0)") size(self%fields)
                error stop EP//"append_from: cannot append a struct with "//trim(got)// &
                    " fields onto one with "//trim(want)
            end if
            call name_slot(self, nm_a)
            call name_slot(src, nm_b)
            do j = 1, size(self%fields)
                call self%field_names%copy_to(j, nm_a)
                call src%field_names%copy_to(j, nm_b)
                if (trim(nm_a) /= trim(nm_b)) then
                    write(got, "(I0)") j
                    error stop EP//"append_from: field "//trim(got)//" is '"//trim(nm_b)// &
                        "' in the source and '"//trim(nm_a)//"' here"
                end if
                if (self%fields(j)%kindof() /= src%fields(j)%kindof()) then
                    call field_kind_text(self%fields(j)%kindof(), kt_a)
                    call field_kind_text(src%fields(j)%kindof(), kt_b)
                    error stop EP//"append_from: field '"//trim(nm_a)//"' is "//kt_b// &
                        " in the source and "//kt_a//" here"
                end if
            end do
            n0 = self%nrows_
            do j = 1, size(self%fields)
                call self%fields(j)%append(src%fields(j))
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
    end subroutine sc_append_from
    !
    !> Reserves capacity for at least `n` rows; see `parquet_container_column%reserve_rows`.
    subroutine sc_reserve_rows(self, n)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                     !! rows to reserve for.
        integer :: j
        if (n < 0_int64) error stop EP//"reserve_rows: negative row count"
        if (allocated(self%fields)) then
            do j = 1, size(self%fields)
                call self%fields(j)%reserve(n)
            end do
        end if
        if (self%has_nulls_) call ensure_validity_cap(self, n)
    end subroutine sc_reserve_rows
    !
    !> Materializes the row bitmap and every field's own validity storage eagerly; see
    !! `parquet_container_column%ensure_validity`.
    !!
    !! The concurrency escape hatch: several threads filling one column must not race on the lazy
    !! first allocation, and this is what a caller runs before the parallel region so they cannot.
    !! Both levels are covered, because both allocate lazily -- row nullness here, field nullness
    !! inside each field column.
    subroutine sc_ensure_validity(self)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer :: j
        call ensure_validity_cap(self, max(self%nrows_, MIN_ROW_CAP))
        self%has_nulls_ = .true.
        if (.not. allocated(self%fields)) return
        do j = 1, size(self%fields)
            call self%fields(j)%ensure_validity()
        end do
    end subroutine sc_ensure_validity
    !
    !> Writes a human-readable description of the column's kind, e.g.
    !! `"struct<name:string,age:int32>"`; see `parquet_container_column%kind_text`.
    subroutine sc_kind_text(self, out)
        class(parquet_struct_column), intent(in) :: self   !! the column.
        character(len=:), allocatable, intent(out) :: out  !! the description.
        character(len=:), allocatable :: nm, kt
        integer :: j
        if (.not. allocated(self%fields)) then
            out = "struct<>"
            return
        end if
        ! The names come out through %copy_to into ONE scratch slot sized from the longest of
        ! them, rather than through %get per field. %get allocates a deferred-length string per
        ! call, which check_no_per_element_string_alloc forbids in a loop.
        ! The loop here is over FIELDS rather than rows, so the saving is small; following the rule
        ! everywhere is what keeps it checkable.
        call name_slot(self, nm)
        out = "struct<"
        do j = 1, size(self%fields)
            call self%field_names%copy_to(j, nm)
            call nested_field_text(self%fields(j), self%fields(j)%kindof(), kt)
            if (j > 1) out = out//","
            out = out//trim(nm)//":"//kt
        end do
        out = out//">"
    end subroutine sc_kind_text
    !
    !> Whether row `i` is a null struct; see `parquet_container_column%is_null_row`.
    !!
    !! The three row-nullness bindings exist because `parquet_column`'s own bitmap is never
    !! allocated for a container kind, so `%is_null(i)` on the column would otherwise answer
    !! `.false.` for a row that really is null -- and `%set_null(i)` would write a bit nothing
    !! reads. Each forwards to this type's own public form, which does the bounds check.
    pure function sc_is_null_row(self, i) result(res)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index.
        logical :: res                                   !! whether the row is null.
        res = .false.
        if (i < 1_int64 .or. i > self%nrows_) return
        res = row_is_null(self, i)
    end function sc_is_null_row
    !
    !> Marks row `i` a null struct; see `parquet_container_column%set_null_row`.
    subroutine sc_set_null_row(self, i)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        call self%set_null_i64(i)
    end subroutine sc_set_null_row
    !
    !> Marks row `i` present again; see `parquet_container_column%clear_null_row`.
    subroutine sc_clear_null_row(self, i)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        call self%clear_null_i64(i)
    end subroutine sc_clear_null_row
    !
    ! ==================================================================================
    ! Lifecycle
    ! ==================================================================================
    !
    !> Fixes the field set and, optionally, creates `nrows` null rows.
    !!
    !! **The field set is required and cannot change afterwards.** Inferring it from the first
    !! `%append_row` instead would type the column from one row -- the bug class CLAUDE.md devotes
    !! a testing section to -- and would leave `%field_count()` unable to answer for an EMPTY
    !! column, which `parquet_write_column` has to ask before any row exists in order to build the
    !! Arrow field at all.
    !!
    !! Every field name is trimmed, and four rules are checked because each would otherwise show
    !! up later as a wrong answer rather than as an error here:
    !!
    !! * at least one field -- Arrow cannot construct a zero-field struct, and a struct column
    !!   carrying no data is a mistake in every case a caller could reach it;
    !! * `names` and `kinds` the same length;
    !! * every name non-blank, unique, and free of `.` -- a dot would make this column's own leaf
    !!   path ambiguous against the dotted-path struct reader, which addresses `col.field`;
    !! * every kind one this type accepts as a field.
    subroutine init(self, names, kinds, nrows)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        character(len=*), intent(in) :: names(:)            !! field names, in order (trimmed).
        integer, intent(in) :: kinds(:)                     !! PK_* kind per field, same length.
        integer(int64), intent(in), optional :: nrows       !! null rows to create (default 0).
        integer(int64) :: n
        integer :: nf, j, m
        character(len=:), allocatable :: kname
        call self%clear()
        nf = size(names)
        if (nf /= size(kinds)) error stop EP//"init: names and kinds have different lengths"
        if (nf < 1) error stop EP//"init: a struct column must declare at least one field"
        if (nf > MAX_FIELDS) error stop EP//"init: more fields than Arrow can address in a struct"
        do j = 1, nf
            if (len_trim(names(j)) == 0) error stop EP//"init: a field name is blank"
            if (index(trim(names(j)), ".") > 0) then
                error stop EP//"init: a field name contains '.', which would collide with the "// &
                    "dotted path a struct's fields are addressed by: "//trim(names(j))
            end if
            do m = 1, j - 1
                if (trim(names(j)) == trim(names(m))) then
                    error stop EP//"init: duplicate field name: "//trim(names(j))
                end if
            end do
            if (.not. is_supported_field(kinds(j))) then
                call parquet_kind_name(kinds(j), kname)
                if (parquet_kind_is_container(kinds(j))) then
                    ! A container field is nesting, and %init cannot express it: `kinds(:)` carries
                    ! one PK_* discriminator per field, while a nested field is a kind PLUS an inner
                    ! schema. Build the inner container, hand it to a parquet_column with
                    ! %adopt_container, and pass that column to %adopt_fields.
                    error stop EP//"init: field '"//trim(names(j))//"' is a nested "//kname// &
                        " field and cannot be declared here; build the inner container, hand it "// &
                        "to a parquet_column with %adopt_container, and pass that column to %adopt_fields"
                end if
                error stop EP//"init: "//kname//" is not a supported struct field kind (field '"// &
                    trim(names(j))//"')"
            end if
        end do
        n = 0_int64
        if (present(nrows)) n = nrows
        if (n < 0_int64) error stop EP//"init: negative row count"
        do j = 1, nf
            call self%field_names%append_string(trim(names(j)))
        end do
        allocate(self%fields(nf))
        do j = 1, nf
            ! Every field starts EMPTY whatever `nrows` says -- the rows being created are null,
            ! and %append_null_row grows each field by one null row of its own.
            call self%fields(j)%init(kinds(j), 0_int64)
        end do
        if (n > 0_int64) call self%grow_rows(n)
    end subroutine init
    !
    !> Releases every buffer and resets to an uninitialized column with no fields.
    subroutine clear(self)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer :: j
        self%nrows_ = 0_int64
        self%has_nulls_ = .false.
        if (allocated(self%validity)) deallocate(self%validity)
        if (allocated(self%fields)) then
            do j = 1, size(self%fields)
                call self%fields(j)%clear()
            end do
            deallocate(self%fields)
        end if
        call self%field_names%clear()
    end subroutine clear
    !
    !> Produces a fully independent copy: field names, every field column with its own per-row
    !! validity and unit, and the row bitmap.
    subroutine deep_copy(self, out)
        class(parquet_struct_column), intent(in) :: self !! the source column.
        type(parquet_struct_column), intent(out) :: out  !! receives the copy.
        integer :: j
        call out%clear()
        out%nrows_ = self%nrows_
        out%has_nulls_ = self%has_nulls_
        out%field_names = self%field_names%clone()
        if (allocated(self%fields)) then
            allocate(out%fields(size(self%fields, kind=int64)))
            do j = 1, size(self%fields)
                call self%fields(j)%deep_copy(out%fields(j))
            end do
        end if
        if (allocated(self%validity)) then
            allocate(out%validity(size(self%validity, kind=int64)))
            out%validity = self%validity
        end if
    end subroutine deep_copy
    !
    !> Takes over `src`'s storage without copying it, leaving `src` empty and uninitialized.
    subroutine move_from(self, src)
        class(parquet_struct_column), intent(inout) :: self !! the column receiving the storage.
        type(parquet_struct_column), intent(inout) :: src   !! the column giving it up; left empty.
        integer :: j
        call self%clear()
        self%nrows_ = src%nrows_
        self%has_nulls_ = src%has_nulls_
        self%field_names = src%field_names%clone()
        if (allocated(src%fields)) then
            allocate(self%fields(size(src%fields, kind=int64)))
            do j = 1, size(src%fields)
                call self%fields(j)%move_from(src%fields(j))
            end do
        end if
        if (allocated(src%validity)) call move_alloc(src%validity, self%validity)
        ! `src` gave up its buffers but would otherwise still claim a field set and a row count,
        ! which would describe storage that is no longer there.
        call src%clear()
    end subroutine move_from
    !
    !> Takes ownership of a whole column at once: `fields` is MOVED in (it comes back
    !! deallocated), `names` declares the field set, `row_valid` marks which rows are present, and
    !! the row count and every field kind are derived from what was given rather than declared.
    !!
    !! The bulk counterpart of `%append_row`, and what the file reader is built on: a read knows
    !! its row count and every field's values before it has a single struct row, so appending row
    !! by row would mean re-deriving what it already knows, once per row per field.
    !!
    !! Replaces whatever this column held (it `%clear()`s first), so it is a build, not an append.
    !!
    !! Preconditions, all checked and all fatal:
    !!
    !! * at least one field, and `names` the same length as `fields`;
    !! * every name non-blank, unique and free of `.`, as `%init` requires;
    !! * every field a kind this type accepts, with width 1;
    !! * every field exactly the same length, which becomes the row count;
    !! * `row_valid`, if present, exactly that long.
    !!
    !! Per-FIELD nullness travels inside each `parquet_column`, so it needs no argument here;
    !! `row_valid` is the separate ROW level.
    subroutine adopt_fields(self, names, fields, row_valid)
        class(parquet_struct_column), intent(inout) :: self          !! the column being built.
        character(len=*), intent(in) :: names(:)                     !! field names, in order.
        type(parquet_column), allocatable, intent(inout) :: fields(:) !! per-field columns, moved in.
        logical, intent(in), optional :: row_valid(:)                !! per-ROW validity; absent = all present.
        integer(int64) :: n, i
        integer :: nf, j, m
        character(len=:), allocatable :: kname
        if (.not. allocated(fields)) error stop EP//"adopt_fields: fields is not allocated"
        nf = size(fields)
        if (nf < 1) error stop EP//"adopt_fields: a struct column must declare at least one field"
        if (size(names) /= nf) error stop EP//"adopt_fields: names and fields have different lengths"
        do j = 1, nf
            if (len_trim(names(j)) == 0) error stop EP//"adopt_fields: a field name is blank"
            if (index(trim(names(j)), ".") > 0) then
                error stop EP//"adopt_fields: a field name contains '.': "//trim(names(j))
            end if
            do m = 1, j - 1
                if (trim(names(j)) == trim(names(m))) then
                    error stop EP//"adopt_fields: duplicate field name: "//trim(names(j))
                end if
            end do
            if (.not. is_adoptable_field(fields(j)%kindof())) then
                call parquet_kind_name(fields(j)%kindof(), kname)
                error stop EP//"adopt_fields: "//kname//" is not a supported struct field kind "// &
                    "(field '"//trim(names(j))//"')"
            end if
            ! Unreachable, and kept as a belt-and-braces check: parquet_column%init refuses a
            ! width above 1 for every kind that is not a *_VEC one, and is_adoptable_field above
            ! has already refused every *_VEC kind -- so nothing that reaches here can be wide.
            if (fields(j)%colwidth() /= 1_int32) then
                ! GCOVR_EXCL_START -- unreachable; see the comment above.
                error stop EP//"adopt_fields: field '"//trim(names(j))//"' must be a scalar (width 1) column"
                ! GCOVR_EXCL_STOP
            end if
        end do
        n = fields(1)%length()
        do j = 2, nf
            if (fields(j)%length() /= n) then
                error stop EP//"adopt_fields: field '"//trim(names(j))//"' has a different row count "// &
                    "from the first field"
            end if
        end do
        if (present(row_valid)) then
            if (size(row_valid, kind=int64) /= n) then
                error stop EP//"adopt_fields: row_valid has a different length from the row count"
            end if
        end if
        call self%clear()
        do j = 1, nf
            call self%field_names%append_string(trim(names(j)))
        end do
        call move_alloc(fields, self%fields)
        self%nrows_ = n
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
    end subroutine adopt_fields
    !
    ! ==================================================================================
    ! Queries
    ! ==================================================================================
    !
    !> Number of rows stored (the public alias of the deferred `%nrows()`).
    pure function sc_nrows_public(self) result(n)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64) :: n                              !! rows stored.
        n = self%nrows_
    end function sc_nrows_public
    !
    !> Whether `%init` has fixed a field set. Every mutating operation requires it.
    pure function is_init(self) result(res)
        class(parquet_struct_column), intent(in) :: self !! the column.
        logical :: res                                   !! .true. once %init has run.
        res = allocated(self%fields)
    end function is_init
    !
    !> Number of declared fields, or 0 before `%init`. Answers for an empty column too, which is
    !! what lets the writer build the Arrow field before any row exists.
    pure function field_count(self) result(n)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer :: n                                     !! declared fields.
        n = 0
        if (allocated(self%fields)) n = size(self%fields)
    end function field_count
    !
    !> Copies out the name of the `k`-th declared field.
    subroutine field_name(self, k, name)
        class(parquet_struct_column), intent(in) :: self   !! the column.
        integer, intent(in) :: k                           !! 1-based field index.
        character(len=:), allocatable, intent(out) :: name !! receives the field's name.
        call check_field(self, k, "field_name")
        call self%field_names%get(k, name)
    end subroutine field_name
    !
    !> The 1-based position of a named field, or **0** when there is none.
    !!
    !! Deliberately answers 0 rather than aborting: this is the query a caller hoists out of a
    !! loop to avoid `%set_field`'s per-call name comparison, and it is also how a caller tests
    !! for a field's presence without reaching for the `warn=` path on the handle.
    function field_index(self, name) result(k)
        class(parquet_struct_column), intent(in) :: self !! the column.
        character(len=*), intent(in) :: name             !! the field name to look for.
        integer :: k                                     !! 1-based position, or 0.
        k = 0
        if (.not. allocated(self%fields)) return
        k = int(self%field_names%find(trim(name)))
    end function field_index
    !
    !> The PK_* kind of the `k`-th declared field.
    function field_kind(self, k) result(res)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer, intent(in) :: k                         !! 1-based field index.
        integer :: res                                   !! the field's PK_* discriminator.
        call check_field(self, k, "field_kind")
        res = self%fields(k)%kindof()
    end function field_kind
    !
    !> Number of null rows.
    pure function null_count(self) result(n)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64) :: n                              !! null rows.
        integer(int64) :: i
        n = 0_int64
        if (.not. self%has_nulls_) return
        if (.not. allocated(self%validity)) return
        do i = 1_int64, self%nrows_
            if (bit_test(self%validity, i)) n = n + 1_int64
        end do
    end function null_count
    !
    !> Rows the field storage is allocated for; always >= `%size()`.
    !!
    !! The minimum over every field, because a struct row exists only where every field has one.
    function capacity(self) result(n)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64) :: n                              !! row capacity.
        integer :: j
        n = 0_int64
        if (.not. allocated(self%fields)) return
        n = self%fields(1)%capacity()
        do j = 2, size(self%fields)
            n = min(n, self%fields(j)%capacity())
        end do
    end function capacity
    !
    !> Whether marking a row null would still have to allocate the row bitmap.
    !!
    !! The question `%ensure_validity` exists to answer in advance -- see it for why that matters
    !! when several threads fill one column.
    pure function has_validity_storage(self) result(res)
        class(parquet_struct_column), intent(in) :: self !! the column.
        logical :: res                                   !! .true. when the bitmap already exists.
        res = allocated(self%validity)
    end function has_validity_storage
    !
    !> int32 specific of is_null; see the is_null generic.
    function is_null_i32(self, i) result(res)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int32), intent(in) :: i                  !! 1-based row index.
        logical :: res                                   !! whether row i is a null struct.
        res = self%is_null_i64(int(i, int64))
    end function is_null_i32
    !
    !> int64 specific of is_null: whether row `i` is a null (absent) struct instance.
    !!
    !! This is ROW nullness. A field inside a present row being null is a different question,
    !! answered by the `is_valid` argument of the handle's `%get`. A row whose every field is null
    !! is **not** a null row.
    function is_null_i64(self, i) result(res)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index.
        logical :: res                                   !! whether row i is a null struct.
        call check_row(self, i, "is_null")
        res = row_is_null(self, i)
    end function is_null_i64
    !
    !> int32 specific of view; see the view generic.
    function view_i32(self, i) result(h)
        class(parquet_struct_column), intent(in), target :: self !! the column (must be a target).
        integer(int32), intent(in) :: i                          !! 1-based row index.
        type(parquet_struct_row) :: h                            !! handle to row i.
        h = self%view_i64(int(i, int64))
    end function view_i32
    !
    !> int64 specific of view: a zero-copy handle to row `i`, denoting the whole row.
    !!
    !! **`self` must have the `TARGET` attribute at the call site.** F2018 15.5.2.4 leaves the
    !! handle's stored pointer undefined otherwise, and only nagfor's `-C=dangling` reports it --
    !! gfortran, ifx and flang all run the non-conforming form perfectly happily.
    function view_i64(self, i) result(h)
        class(parquet_struct_column), intent(in), target :: self !! the column (must be a target).
        integer(int64), intent(in) :: i                          !! 1-based row index.
        type(parquet_struct_row) :: h                            !! handle to row i.
        call check_row(self, i, "view")
        h%col => self
        h%idx = i
        h%field_idx = 0
    end function view_i64
    !
    !> Verifies the class invariants, returning .false. and a diagnostic when one is broken.
    !!
    !! Cheap enough to call from a test after any structural change, which is what it is for.
    !!
    !! **Every failure arm below is excluded from coverage, and the success arms are not.** This
    !! procedure IS the self-check: each arm names an invariant that the public API cannot break,
    !! so reaching one means a component was written past the type's own bindings -- which no test
    !! can do from outside the module, and which is exactly the state this exists to catch if a
    !! future change ever introduces it. Excluding them keeps that safety net from reading as a
    !! coverage gap. The two `ok = .true.` returns are reachable and deliberately left counted.
    function validate(self, message) result(ok)
        class(parquet_struct_column), intent(in) :: self                !! the column.
        character(len=:), allocatable, intent(out), optional :: message !! diagnostic on failure.
        logical :: ok                                                   !! .true. when every invariant holds.
        integer :: j
        ok = .false.
        if (present(message)) message = ""
        if (self%nrows_ < 0_int64) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "negative row count"
            return
            ! GCOVR_EXCL_STOP
        end if
        if (.not. allocated(self%fields)) then
            if (self%nrows_ /= 0_int64) then
                ! GCOVR_EXCL_START -- unreachable; see the note above.
                if (present(message)) message = "rows stored in a column with no fields"
                return
                ! GCOVR_EXCL_STOP
            end if
            ok = .true.
            return
        end if
        if (int(self%field_names%size()) /= size(self%fields)) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "field name count does not match the field column count"
            return
            ! GCOVR_EXCL_STOP
        end if
        do j = 1, size(self%fields)
            if (self%fields(j)%length() /= self%nrows_) then
                ! GCOVR_EXCL_START -- unreachable; see the note above.
                if (present(message)) message = "a field column has a different row count from the struct"
                return
                ! GCOVR_EXCL_STOP
            end if
            if (.not. is_adoptable_field(self%fields(j)%kindof())) then
                ! GCOVR_EXCL_START -- unreachable; see the note above.
                if (present(message)) message = "a field has an unsupported kind"
                return
                ! GCOVR_EXCL_STOP
            end if
        end do
        if (self%has_nulls_ .and. .not. allocated(self%validity)) then
            ! GCOVR_EXCL_START -- unreachable; see the note above.
            if (present(message)) message = "has_nulls is set but the bitmap is not allocated"
            return
            ! GCOVR_EXCL_STOP
        end if
        ok = .true.
    end function validate
    !
    !> Writes a one-line human-readable description, e.g.
    !! `"struct<name:string,age:int32>: 3 rows, 2 fields, 1 null"`.
    !!
    !! Composes a string rather than writing to a unit deliberately: the caller decides where it
    !! goes, and a metadata description should not be governed by the same knob that silences a
    !! warning.
    subroutine summary(self, out)
        class(parquet_struct_column), intent(in) :: self  !! the column.
        character(len=:), allocatable, intent(out) :: out !! the description.
        character(len=:), allocatable :: kt, nr, nf, nn
        call self%kind_text(kt)
        call i2s(self%nrows_, nr)
        call i2s(int(self%field_count(), int64), nf)
        call i2s(self%null_count(), nn)
        out = kt//": "//nr//" rows, "//nf//" fields, "//nn//" null"
    end subroutine summary
    !
    ! ==================================================================================
    ! Capacity
    ! ==================================================================================
    !
    !> int32 specific of reserve; see the reserve generic.
    subroutine reserve_i32(self, n)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: n                     !! rows to reserve for.
        call self%reserve_i64(int(n, int64))
    end subroutine reserve_i32
    !
    !> int64 specific of reserve: grows capacity to hold at least `n` rows without adding any.
    subroutine reserve_i64(self, n)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: n                     !! rows to reserve for.
        call self%reserve_rows(n)
    end subroutine reserve_i64
    !
    !> Releases capacity beyond the rows stored, in every field alike.
    !!
    !! A request, not an assertion: a column that has never been appended to has no slack to
    !! release and this does nothing.
    subroutine shrink_to_fit(self)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer :: j
        if (.not. allocated(self%fields)) return
        do j = 1, size(self%fields)
            call self%fields(j)%shrink_to_fit()
        end do
    end subroutine shrink_to_fit
    !
    ! ==================================================================================
    ! Mutation
    ! ==================================================================================
    !
    !> Appends one PRESENT row in which every field is null.
    !!
    !! That is the only sane default: a struct instance exists, and its values have not been set
    !! yet. Filling it is a separate `%set_field` step per field, which is what makes a struct
    !! column buildable at all -- its fields have up to nine different types, so there is no one
    !! call that could take them all.
    subroutine append_row(self)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer :: j
        call require_init(self, "append_row")
        do j = 1, size(self%fields)
            call self%fields(j)%append_nulls(1_int64)
        end do
        self%nrows_ = self%nrows_ + 1_int64
        ! The new row is PRESENT, so the row bitmap needs nothing written: a grown bitmap is
        ! zero-filled and 0 means valid. It still has to COVER the new row, or a later %set_null
        ! on it would index past the allocation.
        if (self%has_nulls_) call ensure_validity_cap(self, self%nrows_)
    end subroutine append_row
    !
    !> Appends one null (absent) struct row.
    !!
    !! Every field gains a null value too, which is what a Parquet round trip would have produced
    !! anyway -- the definition levels cannot encode "the struct is absent but its field is
    !! present". See the module doc.
    subroutine append_null_row(self)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        call require_init(self, "append_null_row")
        call self%append_row()
        call self%set_null_i64(self%nrows_)
    end subroutine append_null_row
    !
    !> int32 specific of set_null; see the set_null generic.
    subroutine set_null_i32(self, i)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        call self%set_null_i64(int(i, int64))
    end subroutine set_null_i32
    !
    !> int64 specific of set_null: marks row `i` a null (absent) struct instance.
    !!
    !! **The row's field values are left exactly where they are**, exactly as
    !! `parquet_list_column%set_null` leaves a nulled row's elements. They become unreachable
    !! (`%get` reports `is_valid = .false.`) and are dropped by the next `%gather_rows` rebuild;
    !! a later `%clear_null` restores the row with its values intact, which is what makes this
    !! O(1) rather than O(fields).
    subroutine set_null_i64(self, i)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        call check_row(self, i, "set_null")
        call ensure_validity_cap(self, max(self%nrows_, capacity_rows(self)))
        self%has_nulls_ = .true.
        call bit_set(self%validity, i)
    end subroutine set_null_i64
    !
    !> int32 specific of clear_null; see the clear_null generic.
    subroutine clear_null_i32(self, i)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        call self%clear_null_i64(int(i, int64))
    end subroutine clear_null_i32
    !
    !> int64 specific of clear_null: marks row `i` a present struct instance again.
    subroutine clear_null_i64(self, i)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        call check_row(self, i, "clear_null")
        if (.not. self%has_nulls_) return
        if (.not. allocated(self%validity)) return
        call bit_clear(self%validity, i)
    end subroutine clear_null_i64
    !
    !> `set_field_null` specific taking an int64 row index and a field index.
    subroutine sfn_k64(self, i, k)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        call check_row(self, i, "set_field_null")
        call check_field(self, k, "set_field_null")
        call parquet_column_set_null(self%fields(k), i)
    end subroutine sfn_k64
    !
    !> `set_field_null` specific taking an int32 row index and a field index.
    subroutine sfn_k32(self, i, k)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        call self%sfn_k64(int(i, int64), k)
    end subroutine sfn_k32
    !
    !> `set_field_null` specific taking an int64 row index and a field name.
    subroutine sfn_n64(self, i, name)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        call self%sfn_k64(i, require_field(self, name, "set_field_null"))
    end subroutine sfn_n64
    !
    !> `set_field_null` specific taking an int32 row index and a field name.
    subroutine sfn_n32(self, i, name)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        call self%sfn_k64(int(i, int64), require_field(self, name, "set_field_null"))
    end subroutine sfn_n32
    !
    ! The 36 `set_field` specifics are four families of nine: {name, index} x {int32, int64}
    ! row index. Only the (index, int64) family does any work -- the other 27 are one-line
    ! forwarders onto it, so there is exactly one place a rule about setting a field lives.
    ! The index form is the primitive: a name costs a string comparison per call, and filling a
    ! column is nrows x nfields calls.
    !
    !> `set_field` specific writing a int32 value into field `k` of row `i`.
    subroutine sf_k64_i32(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        integer(int32), intent(in) :: value          !! the value to store.
        call begin_set(self, i, k, PK_INT32, "set_field")
        call parquet_column_set_at(self%fields(k), i, value)
    end subroutine sf_k64_i32
    !
    !> `set_field` specific writing a int64 value into field `k` of row `i`.
    subroutine sf_k64_i64(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        integer(int64), intent(in) :: value          !! the value to store.
        call begin_set(self, i, k, PK_INT64, "set_field")
        call parquet_column_set_at(self%fields(k), i, value)
    end subroutine sf_k64_i64
    !
    !> `set_field` specific writing a float32 value into field `k` of row `i`.
    subroutine sf_k64_f32(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        real(real32), intent(in) :: value            !! the value to store.
        call begin_set(self, i, k, PK_FLOAT32, "set_field")
        call parquet_column_set_at(self%fields(k), i, value)
    end subroutine sf_k64_f32
    !
    !> `set_field` specific writing a float64 value into field `k` of row `i`.
    subroutine sf_k64_f64(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        real(real64), intent(in) :: value            !! the value to store.
        call begin_set(self, i, k, PK_FLOAT64, "set_field")
        call parquet_column_set_at(self%fields(k), i, value)
    end subroutine sf_k64_f64
    !
    !> `set_field` specific writing a logical value into field `k` of row `i`.
    subroutine sf_k64_bool(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        logical, intent(in) :: value                 !! the value to store.
        call begin_set(self, i, k, PK_LOGICAL, "set_field")
        call parquet_column_set_at(self%fields(k), i, value)
    end subroutine sf_k64_bool
    !
    !> `set_field` specific writing a string value into field `k` of row `i`.
    subroutine sf_k64_str(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        character(len=*), intent(in) :: value        !! the value to store.
        call begin_set(self, i, k, PK_STRING, "set_field")
        call parquet_column_set_at(self%fields(k), i, value)
    end subroutine sf_k64_str
    !
    !> `set_field` specific writing a date value into field `k` of row `i`.
    subroutine sf_k64_date(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        type(parquet_date), intent(in) :: value      !! the value to store.
        call begin_set(self, i, k, PK_DATE, "set_field")
        call parquet_column_set_at(self%fields(k), i, value)
    end subroutine sf_k64_date
    !
    !> `set_field` specific writing a time value into field `k` of row `i`.
    subroutine sf_k64_time(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        type(parquet_time), intent(in) :: value      !! the value to store.
        call begin_set(self, i, k, PK_TIME, "set_field")
        call parquet_column_set_at(self%fields(k), i, value)
    end subroutine sf_k64_time
    !
    !> `set_field` specific writing a timestamp value into field `k` of row `i`.
    subroutine sf_k64_ts(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        type(parquet_timestamp), intent(in) :: value !! the value to store.
        call begin_set(self, i, k, PK_TIMESTAMP, "set_field")
        call parquet_column_set_at(self%fields(k), i, value)
    end subroutine sf_k64_ts
    !
    !> `set_field` specific writing a int32 value into field `k` of row `i` (int32 index).
    subroutine sf_k32_i32(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        integer(int32), intent(in) :: value          !! the value to store.
        call self%sf_k64_i32(int(i, int64), k, value)
    end subroutine sf_k32_i32
    !
    !> `set_field` specific writing a int64 value into field `k` of row `i` (int32 index).
    subroutine sf_k32_i64(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        integer(int64), intent(in) :: value          !! the value to store.
        call self%sf_k64_i64(int(i, int64), k, value)
    end subroutine sf_k32_i64
    !
    !> `set_field` specific writing a float32 value into field `k` of row `i` (int32 index).
    subroutine sf_k32_f32(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        real(real32), intent(in) :: value            !! the value to store.
        call self%sf_k64_f32(int(i, int64), k, value)
    end subroutine sf_k32_f32
    !
    !> `set_field` specific writing a float64 value into field `k` of row `i` (int32 index).
    subroutine sf_k32_f64(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        real(real64), intent(in) :: value            !! the value to store.
        call self%sf_k64_f64(int(i, int64), k, value)
    end subroutine sf_k32_f64
    !
    !> `set_field` specific writing a logical value into field `k` of row `i` (int32 index).
    subroutine sf_k32_bool(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        logical, intent(in) :: value                 !! the value to store.
        call self%sf_k64_bool(int(i, int64), k, value)
    end subroutine sf_k32_bool
    !
    !> `set_field` specific writing a string value into field `k` of row `i` (int32 index).
    subroutine sf_k32_str(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        character(len=*), intent(in) :: value        !! the value to store.
        call self%sf_k64_str(int(i, int64), k, value)
    end subroutine sf_k32_str
    !
    !> `set_field` specific writing a date value into field `k` of row `i` (int32 index).
    subroutine sf_k32_date(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        type(parquet_date), intent(in) :: value      !! the value to store.
        call self%sf_k64_date(int(i, int64), k, value)
    end subroutine sf_k32_date
    !
    !> `set_field` specific writing a time value into field `k` of row `i` (int32 index).
    subroutine sf_k32_time(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        type(parquet_time), intent(in) :: value      !! the value to store.
        call self%sf_k64_time(int(i, int64), k, value)
    end subroutine sf_k32_time
    !
    !> `set_field` specific writing a timestamp value into field `k` of row `i` (int32 index).
    subroutine sf_k32_ts(self, i, k, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        integer, intent(in) :: k                            !! 1-based field index.
        type(parquet_timestamp), intent(in) :: value !! the value to store.
        call self%sf_k64_ts(int(i, int64), k, value)
    end subroutine sf_k32_ts
    !
    !> `set_field` specific writing a int32 value into field `name` of row `i`.
    subroutine sf_n64_i32(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        integer(int32), intent(in) :: value          !! the value to store.
        call self%sf_k64_i32(i, require_field(self, name, "set_field"), value)
    end subroutine sf_n64_i32
    !
    !> `set_field` specific writing a int64 value into field `name` of row `i`.
    subroutine sf_n64_i64(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        integer(int64), intent(in) :: value          !! the value to store.
        call self%sf_k64_i64(i, require_field(self, name, "set_field"), value)
    end subroutine sf_n64_i64
    !
    !> `set_field` specific writing a float32 value into field `name` of row `i`.
    subroutine sf_n64_f32(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        real(real32), intent(in) :: value            !! the value to store.
        call self%sf_k64_f32(i, require_field(self, name, "set_field"), value)
    end subroutine sf_n64_f32
    !
    !> `set_field` specific writing a float64 value into field `name` of row `i`.
    subroutine sf_n64_f64(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        real(real64), intent(in) :: value            !! the value to store.
        call self%sf_k64_f64(i, require_field(self, name, "set_field"), value)
    end subroutine sf_n64_f64
    !
    !> `set_field` specific writing a logical value into field `name` of row `i`.
    subroutine sf_n64_bool(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        logical, intent(in) :: value                 !! the value to store.
        call self%sf_k64_bool(i, require_field(self, name, "set_field"), value)
    end subroutine sf_n64_bool
    !
    !> `set_field` specific writing a string value into field `name` of row `i`.
    subroutine sf_n64_str(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        character(len=*), intent(in) :: value        !! the value to store.
        call self%sf_k64_str(i, require_field(self, name, "set_field"), value)
    end subroutine sf_n64_str
    !
    !> `set_field` specific writing a date value into field `name` of row `i`.
    subroutine sf_n64_date(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        type(parquet_date), intent(in) :: value      !! the value to store.
        call self%sf_k64_date(i, require_field(self, name, "set_field"), value)
    end subroutine sf_n64_date
    !
    !> `set_field` specific writing a time value into field `name` of row `i`.
    subroutine sf_n64_time(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        type(parquet_time), intent(in) :: value      !! the value to store.
        call self%sf_k64_time(i, require_field(self, name, "set_field"), value)
    end subroutine sf_n64_time
    !
    !> `set_field` specific writing a timestamp value into field `name` of row `i`.
    subroutine sf_n64_ts(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        type(parquet_timestamp), intent(in) :: value !! the value to store.
        call self%sf_k64_ts(i, require_field(self, name, "set_field"), value)
    end subroutine sf_n64_ts
    !
    !> `set_field` specific writing a int32 value into field `name` of row `i` (int32 index).
    subroutine sf_n32_i32(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        integer(int32), intent(in) :: value          !! the value to store.
        call self%sf_k64_i32(int(i, int64), require_field(self, name, "set_field"), value)
    end subroutine sf_n32_i32
    !
    !> `set_field` specific writing a int64 value into field `name` of row `i` (int32 index).
    subroutine sf_n32_i64(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        integer(int64), intent(in) :: value          !! the value to store.
        call self%sf_k64_i64(int(i, int64), require_field(self, name, "set_field"), value)
    end subroutine sf_n32_i64
    !
    !> `set_field` specific writing a float32 value into field `name` of row `i` (int32 index).
    subroutine sf_n32_f32(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        real(real32), intent(in) :: value            !! the value to store.
        call self%sf_k64_f32(int(i, int64), require_field(self, name, "set_field"), value)
    end subroutine sf_n32_f32
    !
    !> `set_field` specific writing a float64 value into field `name` of row `i` (int32 index).
    subroutine sf_n32_f64(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        real(real64), intent(in) :: value            !! the value to store.
        call self%sf_k64_f64(int(i, int64), require_field(self, name, "set_field"), value)
    end subroutine sf_n32_f64
    !
    !> `set_field` specific writing a logical value into field `name` of row `i` (int32 index).
    subroutine sf_n32_bool(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        logical, intent(in) :: value                 !! the value to store.
        call self%sf_k64_bool(int(i, int64), require_field(self, name, "set_field"), value)
    end subroutine sf_n32_bool
    !
    !> `set_field` specific writing a string value into field `name` of row `i` (int32 index).
    subroutine sf_n32_str(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        character(len=*), intent(in) :: value        !! the value to store.
        call self%sf_k64_str(int(i, int64), require_field(self, name, "set_field"), value)
    end subroutine sf_n32_str
    !
    !> `set_field` specific writing a date value into field `name` of row `i` (int32 index).
    subroutine sf_n32_date(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        type(parquet_date), intent(in) :: value      !! the value to store.
        call self%sf_k64_date(int(i, int64), require_field(self, name, "set_field"), value)
    end subroutine sf_n32_date
    !
    !> `set_field` specific writing a time value into field `name` of row `i` (int32 index).
    subroutine sf_n32_time(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        type(parquet_time), intent(in) :: value      !! the value to store.
        call self%sf_k64_time(int(i, int64), require_field(self, name, "set_field"), value)
    end subroutine sf_n32_time
    !
    !> `set_field` specific writing a timestamp value into field `name` of row `i` (int32 index).
    subroutine sf_n32_ts(self, i, name, value)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int32), intent(in) :: i                     !! 1-based row index.
        character(len=*), intent(in) :: name                !! field name.
        type(parquet_timestamp), intent(in) :: value !! the value to store.
        call self%sf_k64_ts(int(i, int64), require_field(self, name, "set_field"), value)
    end subroutine sf_n32_ts
    !    !
    ! ==================================================================================
    ! The row handle
    ! ==================================================================================
    !
    !> Whether this handle refers to a live row of a live column.
    !!
    !! The soft-fail signal `%field(name, warn=.true.)` returns on an unknown name, per the
    !! campaign's error-handling convention: a function returning a handle has no `found=`
    !! argument to write into, so an invalid handle is the report.
    pure function psr_is_valid(self) result(res)
        class(parquet_struct_row), intent(in) :: self !! the handle.
        logical :: res                                !! .true. when the handle is usable.
        res = .false.
        if (.not. associated(self%col)) return
        if (self%idx < 1_int64 .or. self%idx > self%col%nrows_) return
        res = .true.
    end function psr_is_valid
    !
    !> Whether the referenced ROW is a null (absent) struct instance.
    !!
    !! Always about the row, never about a narrowed field -- a narrowed handle reports its
    !! field's nullness through `%get`'s `is_valid` argument instead, so the two questions can
    !! never be confused for one another.
    function psr_is_null(self) result(res)
        class(parquet_struct_row), intent(in) :: self !! the handle.
        logical :: res                                !! whether the row is a null struct.
        call check_handle(self, "is_null")
        res = row_is_null(self%col, self%idx)
    end function psr_is_null
    !
    !> The 1-based row this handle refers to.
    function psr_row_index(self) result(i)
        class(parquet_struct_row), intent(in) :: self !! the handle.
        integer(int64) :: i                           !! the row index.
        call check_handle(self, "row_index")
        i = self%idx
    end function psr_row_index
    !
    !> Number of declared fields of the referenced column.
    function psr_field_count(self) result(n)
        class(parquet_struct_row), intent(in) :: self !! the handle.
        integer :: n                                  !! declared fields.
        call check_handle(self, "field_count")
        n = self%col%field_count()
    end function psr_field_count
    !
    !> Copies out the name of the `k`-th declared field.
    subroutine psr_field_name(self, k, name)
        class(parquet_struct_row), intent(in) :: self      !! the handle.
        integer, intent(in) :: k                           !! 1-based field index.
        character(len=:), allocatable, intent(out) :: name !! receives the field's name.
        call check_handle(self, "field_name")
        call self%col%field_name(k, name)
    end subroutine psr_field_name
    !
    !> Whether `%field` has narrowed this handle to one field slot.
    pure function psr_is_narrowed(self) result(res)
        class(parquet_struct_row), intent(in) :: self !! the handle.
        logical :: res                                !! .true. once narrowed.
        res = (self%field_idx > 0)
    end function psr_is_narrowed
    !
    !> The PK_* kind of the narrowed field, so a caller can pick the right `%get` specific.
    function psr_field_kind(self) result(res)
        class(parquet_struct_row), intent(in) :: self !! the narrowed handle.
        integer :: res                                !! the field's PK_* discriminator.
        call check_handle(self, "field_kind")
        if (self%field_idx <= 0) then
            error stop EP//"field_kind: this handle denotes a whole row; narrow it with %field first"
        end if
        res = self%col%fields(self%field_idx)%kindof()
    end function psr_field_kind
    !
    !> Hands back the inner container behind a NESTED field, plus this row's index within it.
    !!
    !! The struct twin of `parquet_list_row%nested`, and the simplest of the three: a struct field
    !! column holds exactly one entry per struct row, so there is no range -- `row` is just this
    !! handle's own row index, returned for symmetry so a caller never has to remember which of the
    !! two indices applies. Needs a NARROWED handle, the same as `%field_kind` and `%get_field`:
    !!
    !! ```fortran
    !! h = sc%view(i)
    !! f = h%field("vals")
    !! call f%nested(inner, row)
    !! select type (inner)
    !! type is (parquet_list_column)
    !!     r = inner%view(row)
    !!     call r%get(values)
    !! end select
    !! ```
    !!
    !! `inner` comes back NULL when the field is not a container, so a caller that has not checked
    !! `%field_kind()` gets a `select type` that matches nothing rather than a wrong answer. The
    !! pointer is BORROWED and is invalidated by anything that rebuilds the field column.
    subroutine psr_nested(self, inner, row)
        class(parquet_struct_row), intent(in) :: self                  !! the NARROWED handle.
        class(parquet_container_column), pointer, intent(out) :: inner !! the inner container, or null.
        integer(int64), intent(out) :: row                             !! this row's index within it.
        call check_handle(self, "nested")
        if (self%field_idx <= 0) then
            error stop EP//"nested: this handle denotes a whole row; narrow it with %field first"
        end if
        inner => null()
        row = self%idx
        if (.not. parquet_kind_is_container(self%col%fields(self%field_idx)%kindof())) return
        call parquet_column_container(self%col%fields(self%field_idx), inner)
    end subroutine psr_nested
    !
    !> Narrows this handle to the named field; see the binding's own documentation.
    function psr_field(self, name, warn) result(h)
        class(parquet_struct_row), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: name          !! the field to narrow to.
        logical, intent(in), optional :: warn         !! .true. warns and returns an invalid handle.
        type(parquet_struct_row) :: h                 !! handle narrowed to that field.
        integer :: k
        logical :: soft
        character(len=:), allocatable :: known
        call check_handle(self, "field")
        soft = .false.
        if (present(warn)) soft = warn
        k = self%col%field_index(name)
        if (k == 0) then
            ! Listing the declared names is by far the most useful thing this message can say --
            ! a misspelled or reordered field is the overwhelmingly common cause.
            call field_list_text(self%col, known)
            if (soft) then
                ! Named for the procedure, not for the module: EP is this file's `error stop`
                ! prefix and stays on the abort below (.claude/rules/api-conventions.md).
                call parquet_emit_warning("parquet_struct_row%field: no field named '"//trim(name)// &
                    "' in this struct column"//known)
                ! h is returned in its default-initialized state: %is_valid() is .false. and any
                ! %get on it aborts through check_handle.
                return
            end if
            error stop EP//"field: no field named '"//trim(name)//"' in this struct column"//known
        end if
        h%col => self%col
        h%idx = self%idx
        h%field_idx = k
    end function psr_field
    !> `get` specific materializing this narrowed handle's int32 value.
    !!
    !! Valid only on a handle narrowed by `%field(...)`, and only when the field's kind
    !! is `PK_INT32` -- both are wrong-KIND access and both abort, per the campaign's
    !! error-handling convention. A null field yields the type's default value with
    !! `is_valid` reporting `.false.`, exactly as a null row does.
    subroutine psr_get_i32(self, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the narrowed handle.
        integer(int32), intent(out) :: value        !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        integer :: k
        logical :: isnull
        call begin_get(self, PK_INT32, "get", k, isnull)
        if (isnull) then
            value = 0_int32
            if (present(is_valid)) is_valid = .false.
            return
        end if
        call parquet_column_get_at(self%col%fields(k), self%idx, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine psr_get_i32
    !
    !> `get` specific materializing this narrowed handle's int64 value.
    !!
    !! Valid only on a handle narrowed by `%field(...)`, and only when the field's kind
    !! is `PK_INT64` -- both are wrong-KIND access and both abort, per the campaign's
    !! error-handling convention. A null field yields the type's default value with
    !! `is_valid` reporting `.false.`, exactly as a null row does.
    subroutine psr_get_i64(self, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the narrowed handle.
        integer(int64), intent(out) :: value        !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        integer :: k
        logical :: isnull
        call begin_get(self, PK_INT64, "get", k, isnull)
        if (isnull) then
            value = 0_int64
            if (present(is_valid)) is_valid = .false.
            return
        end if
        call parquet_column_get_at(self%col%fields(k), self%idx, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine psr_get_i64
    !
    !> `get` specific materializing this narrowed handle's float32 value.
    !!
    !! Valid only on a handle narrowed by `%field(...)`, and only when the field's kind
    !! is `PK_FLOAT32` -- both are wrong-KIND access and both abort, per the campaign's
    !! error-handling convention. A null field yields the type's default value with
    !! `is_valid` reporting `.false.`, exactly as a null row does.
    subroutine psr_get_f32(self, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the narrowed handle.
        real(real32), intent(out) :: value          !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        integer :: k
        logical :: isnull
        call begin_get(self, PK_FLOAT32, "get", k, isnull)
        if (isnull) then
            value = 0.0_real32
            if (present(is_valid)) is_valid = .false.
            return
        end if
        call parquet_column_get_at(self%col%fields(k), self%idx, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine psr_get_f32
    !
    !> `get` specific materializing this narrowed handle's float64 value.
    !!
    !! Valid only on a handle narrowed by `%field(...)`, and only when the field's kind
    !! is `PK_FLOAT64` -- both are wrong-KIND access and both abort, per the campaign's
    !! error-handling convention. A null field yields the type's default value with
    !! `is_valid` reporting `.false.`, exactly as a null row does.
    subroutine psr_get_f64(self, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the narrowed handle.
        real(real64), intent(out) :: value          !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        integer :: k
        logical :: isnull
        call begin_get(self, PK_FLOAT64, "get", k, isnull)
        if (isnull) then
            value = 0.0_real64
            if (present(is_valid)) is_valid = .false.
            return
        end if
        call parquet_column_get_at(self%col%fields(k), self%idx, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine psr_get_f64
    !
    !> `get` specific materializing this narrowed handle's logical value.
    !!
    !! Valid only on a handle narrowed by `%field(...)`, and only when the field's kind
    !! is `PK_LOGICAL` -- both are wrong-KIND access and both abort, per the campaign's
    !! error-handling convention. A null field yields the type's default value with
    !! `is_valid` reporting `.false.`, exactly as a null row does.
    subroutine psr_get_bool(self, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the narrowed handle.
        logical, intent(out) :: value               !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        integer :: k
        logical :: isnull
        call begin_get(self, PK_LOGICAL, "get", k, isnull)
        if (isnull) then
            value = .false.
            if (present(is_valid)) is_valid = .false.
            return
        end if
        call parquet_column_get_at(self%col%fields(k), self%idx, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine psr_get_bool
    !
    !> `get` specific materializing this narrowed handle's string value.
    !!
    !! Valid only on a handle narrowed by `%field(...)`, and only when the field's kind
    !! is `PK_STRING` -- both are wrong-KIND access and both abort, per the campaign's
    !! error-handling convention. A null field yields the type's default value with
    !! `is_valid` reporting `.false.`, exactly as a null row does.
    subroutine psr_get_str(self, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the narrowed handle.
        character(len=:), allocatable, intent(out) :: value !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        integer :: k
        logical :: isnull
        call begin_get(self, PK_STRING, "get", k, isnull)
        if (isnull) then
            value = ""
            if (present(is_valid)) is_valid = .false.
            return
        end if
        call parquet_column_get_at(self%col%fields(k), self%idx, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine psr_get_str
    !
    !> `get` specific materializing this narrowed handle's date value.
    !!
    !! Valid only on a handle narrowed by `%field(...)`, and only when the field's kind
    !! is `PK_DATE` -- both are wrong-KIND access and both abort, per the campaign's
    !! error-handling convention. A null field yields the type's default value with
    !! `is_valid` reporting `.false.`, exactly as a null row does.
    subroutine psr_get_date(self, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the narrowed handle.
        type(parquet_date), intent(out) :: value    !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        integer :: k
        logical :: isnull
        type(parquet_date) :: default_value
        call begin_get(self, PK_DATE, "get", k, isnull)
        if (isnull) then
            value = default_value
            if (present(is_valid)) is_valid = .false.
            return
        end if
        call parquet_column_get_at(self%col%fields(k), self%idx, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine psr_get_date
    !
    !> `get` specific materializing this narrowed handle's time value.
    !!
    !! Valid only on a handle narrowed by `%field(...)`, and only when the field's kind
    !! is `PK_TIME` -- both are wrong-KIND access and both abort, per the campaign's
    !! error-handling convention. A null field yields the type's default value with
    !! `is_valid` reporting `.false.`, exactly as a null row does.
    subroutine psr_get_time(self, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the narrowed handle.
        type(parquet_time), intent(out) :: value    !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        integer :: k
        logical :: isnull
        type(parquet_time) :: default_value
        call begin_get(self, PK_TIME, "get", k, isnull)
        if (isnull) then
            value = default_value
            if (present(is_valid)) is_valid = .false.
            return
        end if
        call parquet_column_get_at(self%col%fields(k), self%idx, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine psr_get_time
    !
    !> `get` specific materializing this narrowed handle's timestamp value.
    !!
    !! Valid only on a handle narrowed by `%field(...)`, and only when the field's kind
    !! is `PK_TIMESTAMP` -- both are wrong-KIND access and both abort, per the campaign's
    !! error-handling convention. A null field yields the type's default value with
    !! `is_valid` reporting `.false.`, exactly as a null row does.
    subroutine psr_get_ts(self, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the narrowed handle.
        type(parquet_timestamp), intent(out) :: value !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        integer :: k
        logical :: isnull
        type(parquet_timestamp) :: default_value
        call begin_get(self, PK_TIMESTAMP, "get", k, isnull)
        if (isnull) then
            value = default_value
            if (present(is_valid)) is_valid = .false.
            return
        end if
        call parquet_column_get_at(self%col%fields(k), self%idx, value)
        if (present(is_valid)) is_valid = .true.
    end subroutine psr_get_ts
    !    ! The nine name-taking `get` specifics: navigate and materialize in ONE statement.
    !
    ! These exist because `call h%field("age")%get(age)` DOES NOT COMPILE. F2018 R1522 makes a
    ! procedure-designator a `data-ref % binding-name`, and a function reference is not a
    ! data-ref -- so a type-bound call cannot be chained onto a function result. Confirmed on
    ! gfortran 15.2 ("Junk after CALL") and nagfor 7.2 ("Component of function reference").
    ! An earlier design claimed the chaining was ordinary legal Fortran; it is not, and that claim
    ! is corrected there. Without these nine, every read would be two statements.
    !
    ! Each delegates to the narrowed specific through a local handle, so there is exactly one
    ! place a rule about reading a field lives and the two forms' messages cannot diverge.
    !
    !> `get_field` specific materializing the int32 value of the named field of this handle's row.
    subroutine psr_getn_i32(self, name, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the handle (a whole row).
        character(len=*), intent(in) :: name                !! the field to read.
        integer(int32), intent(out) :: value        !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        type(parquet_struct_row) :: slot
        slot = self%field(name)
        call slot%psr_get_i32(value, is_valid)
    end subroutine psr_getn_i32
    !
    !> `get_field` specific materializing the int64 value of the named field of this handle's row.
    subroutine psr_getn_i64(self, name, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the handle (a whole row).
        character(len=*), intent(in) :: name                !! the field to read.
        integer(int64), intent(out) :: value        !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        type(parquet_struct_row) :: slot
        slot = self%field(name)
        call slot%psr_get_i64(value, is_valid)
    end subroutine psr_getn_i64
    !
    !> `get_field` specific materializing the float32 value of the named field of this handle's row.
    subroutine psr_getn_f32(self, name, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the handle (a whole row).
        character(len=*), intent(in) :: name                !! the field to read.
        real(real32), intent(out) :: value          !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        type(parquet_struct_row) :: slot
        slot = self%field(name)
        call slot%psr_get_f32(value, is_valid)
    end subroutine psr_getn_f32
    !
    !> `get_field` specific materializing the float64 value of the named field of this handle's row.
    subroutine psr_getn_f64(self, name, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the handle (a whole row).
        character(len=*), intent(in) :: name                !! the field to read.
        real(real64), intent(out) :: value          !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        type(parquet_struct_row) :: slot
        slot = self%field(name)
        call slot%psr_get_f64(value, is_valid)
    end subroutine psr_getn_f64
    !
    !> `get_field` specific materializing the logical value of the named field of this handle's row.
    subroutine psr_getn_bool(self, name, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the handle (a whole row).
        character(len=*), intent(in) :: name                !! the field to read.
        logical, intent(out) :: value               !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        type(parquet_struct_row) :: slot
        slot = self%field(name)
        call slot%psr_get_bool(value, is_valid)
    end subroutine psr_getn_bool
    !
    !> `get_field` specific materializing the string value of the named field of this handle's row.
    subroutine psr_getn_str(self, name, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the handle (a whole row).
        character(len=*), intent(in) :: name                !! the field to read.
        character(len=:), allocatable, intent(out) :: value !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        type(parquet_struct_row) :: slot
        slot = self%field(name)
        call slot%psr_get_str(value, is_valid)
    end subroutine psr_getn_str
    !
    !> `get_field` specific materializing the date value of the named field of this handle's row.
    subroutine psr_getn_date(self, name, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the handle (a whole row).
        character(len=*), intent(in) :: name                !! the field to read.
        type(parquet_date), intent(out) :: value    !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        type(parquet_struct_row) :: slot
        slot = self%field(name)
        call slot%psr_get_date(value, is_valid)
    end subroutine psr_getn_date
    !
    !> `get_field` specific materializing the time value of the named field of this handle's row.
    subroutine psr_getn_time(self, name, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the handle (a whole row).
        character(len=*), intent(in) :: name                !! the field to read.
        type(parquet_time), intent(out) :: value    !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        type(parquet_struct_row) :: slot
        slot = self%field(name)
        call slot%psr_get_time(value, is_valid)
    end subroutine psr_getn_time
    !
    !> `get_field` specific materializing the timestamp value of the named field of this handle's row.
    subroutine psr_getn_ts(self, name, value, is_valid)
        class(parquet_struct_row), intent(in) :: self       !! the handle (a whole row).
        character(len=*), intent(in) :: name                !! the field to read.
        type(parquet_timestamp), intent(out) :: value !! receives the field's value.
        logical, intent(out), optional :: is_valid          !! .false. when the field is null.
        type(parquet_struct_row) :: slot
        slot = self%field(name)
        call slot%psr_get_ts(value, is_valid)
    end subroutine psr_getn_ts
    !    !
    ! ==================================================================================
    ! Private helpers
    ! ==================================================================================
    !
    !> Aborts unless `%init` has fixed a field set.
    subroutine require_init(self, proc)
        class(parquet_struct_column), intent(in) :: self !! the column.
        character(len=*), intent(in) :: proc             !! calling procedure, for the message.
        if (.not. allocated(self%fields)) then
            error stop EP//proc//": this struct column has no fields; call %init first"
        end if
    end subroutine require_init
    !
    !> Aborts unless `i` names an existing row.
    subroutine check_row(self, i, proc)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index.
        character(len=*), intent(in) :: proc             !! calling procedure, for the message.
        if (i < 1_int64 .or. i > self%nrows_) then
            error stop EP//proc//": row index is out of range"
        end if
    end subroutine check_row
    !
    !> Aborts unless `k` names a declared field.
    subroutine check_field(self, k, proc)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer, intent(in) :: k                         !! 1-based field index.
        character(len=*), intent(in) :: proc             !! calling procedure, for the message.
        call require_init(self, proc)
        if (k < 1 .or. k > size(self%fields)) then
            error stop EP//proc//": field index is out of range"
        end if
    end subroutine check_field
    !
    !> The 1-based position of a named field, aborting when there is none.
    !!
    !! The hard-fail counterpart of `%field_index`, which answers 0. Used by every `set_field`
    !! specific taking a name: writing into a field that does not exist is a caller bug with no
    !! sensible soft outcome, unlike a READ, which `%field(name, warn=)` can decline softly.
    function require_field(self, name, proc) result(k)
        class(parquet_struct_column), intent(in) :: self !! the column.
        character(len=*), intent(in) :: name             !! the field name.
        character(len=*), intent(in) :: proc             !! calling procedure, for the message.
        integer :: k                                     !! 1-based field position.
        character(len=:), allocatable :: known
        call require_init(self, proc)
        k = self%field_index(name)
        if (k == 0) then
            call field_list_text(self, known)
            error stop EP//proc//": no field named '"//trim(name)//"' in this struct column"//known
        end if
    end function require_field
    !
    !> Writes `" (declared fields: a, b, c)"` for an error message, or `""` for a column with none.
    !!
    !! A subroutine rather than a `character(len=:), allocatable` FUNCTION, per the project-wide
    !! rule in CLAUDE.md: gfortran PR113797 makes the hidden length temporary such a function
    !! needs unreliable to be thread-local, which has already caused silent memory corruption in
    !! this library once.
    subroutine field_list_text(self, out)
        class(parquet_struct_column), intent(in) :: self  !! the column.
        character(len=:), allocatable, intent(out) :: out !! the parenthesised list, or "".
        character(len=:), allocatable :: nm
        integer :: j
        out = ""
        if (.not. allocated(self%fields)) return
        call name_slot(self, nm)
        out = " (declared fields: "
        do j = 1, size(self%fields)
            call self%field_names%copy_to(j, nm)
            if (j > 1) out = out//", "
            out = out//trim(nm)
        end do
        out = out//")"
    end subroutine field_list_text

    !> Allocates a blank scratch slot wide enough for any of this column's field names.
    !!
    !! Sized with `%length`, which measures without allocating, so that every name-copying loop
    !! in this module can use `%copy_to` into one slot instead of `%get` per field. See
    !! `check_no_per_element_string_alloc` (tools/check_source_conventions.py) for the rule.
    subroutine name_slot(self, slot)
        class(parquet_struct_column), intent(in) :: self   !! the column.
        character(len=:), allocatable, intent(out) :: slot !! blank slot, as wide as the longest name.
        integer :: j, wid
        wid = 1
        if (allocated(self%fields)) then
            do j = 1, size(self%fields)
                wid = max(wid, int(self%field_names%length(j)))
            end do
        end if
        ! `slot = repeat(" ", wid)`, NOT `allocate(...)` then `slot = ""`: assigning a
        ! zero-length RHS to a deferred-length allocatable SCALAR reallocates it to length zero,
        ! which is the documented idiom for clearing one (CLAUDE.md) and exactly the wrong thing
        ! here -- %copy_to would then copy into no slot at all and every name would come back
        ! blank. A same-length RHS leaves the length alone.
        slot = repeat(" ", wid)
    end subroutine name_slot
    !
    !> Aborts unless the handle refers to a live row of a live column.
    subroutine check_handle(self, proc)
        class(parquet_struct_row), intent(in) :: self !! the handle.
        character(len=*), intent(in) :: proc          !! calling procedure, for the message.
        if (.not. associated(self%col)) then
            error stop EP//proc//": this row handle is not associated with a column"
        end if
        if (self%idx < 1_int64 .or. self%idx > self%col%nrows_) then
            error stop EP//proc//": this row handle no longer refers to a valid row"
        end if
    end subroutine check_handle
    !
    !> The guard every `set_field` specific shares: initialized, existing row, existing field, and
    !! the field's kind matching the value the caller is writing.
    subroutine begin_set(self, i, k, want_kind, proc)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index.
        integer, intent(in) :: k                         !! 1-based field index.
        integer, intent(in) :: want_kind                 !! the PK_* kind the caller's value is.
        character(len=*), intent(in) :: proc             !! calling procedure, for the message.
        character(len=:), allocatable :: have_txt, want_txt, nm
        call check_field(self, k, proc)
        call check_row(self, i, proc)
        if (self%fields(k)%kindof() /= want_kind) then
            call field_kind_text(self%fields(k)%kindof(), have_txt)
            call field_kind_text(want_kind, want_txt)
            call self%field_names%get(k, nm)
            error stop EP//proc//": field '"//nm//"' is "//have_txt//"; a "//want_txt// &
                " value cannot be written into it"
        end if
    end subroutine begin_set
    !
    !> The guard every handle `%get` specific shares: a live handle, narrowed to a field whose
    !! kind matches the caller's variable. Returns the field index.
    !!
    !! Both failures are wrong-KIND access rather than a lookup, so both abort with no soft-fail
    !! option -- the campaign's error-handling convention, and the same rule
    !! `parquet_list_row%get` follows for a mismatched payload.
    subroutine begin_get(self, want_kind, proc, k, isnull)
        class(parquet_struct_row), intent(in) :: self !! the handle.
        integer, intent(in) :: want_kind              !! the PK_* kind the caller's variable is.
        character(len=*), intent(in) :: proc          !! calling procedure, for the message.
        integer, intent(out) :: k                     !! the narrowed field's index.
        logical, intent(out) :: isnull                !! .true. when there is no value to read.
        character(len=:), allocatable :: have_txt, want_txt, nm
        call check_handle(self, proc)
        if (self%field_idx <= 0) then
            error stop EP//proc//": this handle denotes a whole struct row; narrow it to a field "// &
                "with %field(name) before reading a value"
        end if
        k = self%field_idx
        if (self%col%fields(k)%kindof() /= want_kind) then
            call field_kind_text(self%col%fields(k)%kindof(), have_txt)
            call field_kind_text(want_kind, want_txt)
            call self%col%field_names%get(k, nm)
            error stop EP//proc//": field '"//nm//"' is "//have_txt//"; it cannot be read into a "// &
                want_txt//" value"
        end if
        ! BOTH null levels answer here, and the ROW level is the one easy to omit: a row nulled by
        ! %set_null keeps its field values (that is what makes %set_null O(1) and %clear_null
        ! possible), so asking only the field would hand back a stale value with is_valid .true.
        ! for a row the caller has been told is absent. It is also what Parquet stores -- a null
        ! struct row's fields are null in the file whatever they were in memory -- so answering
        ! this way makes the in-memory and read-back columns agree.
        isnull = row_is_null(self%col, self%idx)
        if (.not. isnull) isnull = parquet_column_is_null(self%col%fields(k), self%idx)
    end subroutine begin_get
    !
    !> Whether row `i` is null, with no bounds check -- callers have already made one.
    pure function row_is_null(self, i) result(res)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64), intent(in) :: i                  !! 1-based row index.
        logical :: res                                   !! whether row i is null.
        res = .false.
        if (.not. self%has_nulls_) return
        if (.not. allocated(self%validity)) return
        res = bit_test(self%validity, i)
    end function row_is_null
    !
    !> Rows the field storage covers, or 0 for an uninitialized column.
    function capacity_rows(self) result(n)
        class(parquet_struct_column), intent(in) :: self !! the column.
        integer(int64) :: n                              !! row capacity.
        n = 0_int64
        if (allocated(self%fields)) n = self%capacity()
    end function capacity_rows
    !
    !> Ensures the row bitmap exists and covers at least `need_rows` rows, zero-filling whatever
    !! it gains so that rows it newly covers start out valid.
    subroutine ensure_validity_cap(self, need_rows)
        class(parquet_struct_column), intent(inout) :: self !! the column.
        integer(int64), intent(in) :: need_rows             !! rows the bitmap must cover.
        integer(int64) :: want, have
        integer(int64), allocatable :: tmp(:)
        want = max(blocks_for(max(need_rows, MIN_ROW_CAP)), 1_int64)
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
    !> Whether a PK_* kind may be a struct column's field.
    !!
    !! The nine SCALAR value kinds. A `*_VEC` kind is excluded because a fixed-width vector inside
    !! a struct is `struct<fixed_size_list<...>>`, which is a shape this library does not read or
    !! write at any depth, and is not
    !! expressible by giving the field a width. `PK_LIST`/`PK_MAP`/`PK_STRUCT` are excluded for
    !! the same reason -- a container field is nesting, and `%init` would have no way to be told
    !! what the inner container holds.
    pure function is_supported_field(kind) result(res)
        integer, intent(in) :: kind !! a PK_* discriminator.
        logical :: res              !! whether it may be a struct field.
        select case (kind)
        case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, &
              PK_DATE, PK_TIME, PK_TIMESTAMP)
            res = .true.
        case default
            res = .false.
        end select
    end function is_supported_field
    !
    !> Whether a PK_* kind may be ADOPTED as a struct field.
    !!
    !! Wider than `is_supported_field` by exactly the three container kinds. `%init` fixes each
    !! field's KIND; a nested field is a kind plus a whole inner schema, which `%init`'s
    !! `kinds(:)` array has no way to carry -- so nesting is reachable only by building the inner
    !! container and handing it over with `%adopt_container` + `%adopt_fields`. The `*_VEC` kinds
    !! stay refused on both paths.
    pure function is_adoptable_field(kind) result(res)
        integer, intent(in) :: kind !! a PK_* discriminator.
        logical :: res              !! whether it may be adopted as a struct field.
        res = is_supported_field(kind) .or. parquet_kind_is_container(kind)
    end function is_adoptable_field
    !
    !> Writes the short lowercase name of a field kind, for `kind_text` and error messages.
    !!
    !! Deliberately NOT `parquet_kind_name`, which spells the discriminator (`"PK_INT32"`). This
    !! is the type spelling a MAML schema uses, so `%kind_text()` reads `"struct<a:int32>"`.
    !!
    !! A subroutine rather than a `character(len=:), allocatable` FUNCTION, per the project-wide
    !! rule in CLAUDE.md.
    pure subroutine field_kind_text(kind, out)
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
        ! Unreachable: is_supported_field admits no PK_NONE, so no declared field has that kind.
        case (PK_NONE);      out = "none" ! GCOVR_EXCL_LINE
        case default;        out = "unsupported"
        end select
    end subroutine field_kind_text
    !
    !> Writes a field's type spelling, recursing when the field is itself a container.
    !!
    !! `field_kind_text` is `pure` and answers from the discriminator alone, which is all an error
    !! message needs. A NESTED field's spelling additionally needs the inner container to describe
    !! itself -- so it cannot be pure, and it cannot be derived from the kind at all. Keeping the two
    !! apart is what lets every error message stay pure while `%kind_text` reports the nested form.
    !!
    !! `%kind_text` is the ONLY name in this library that recurses: `%kindof()` stays `PK_STRUCT` at
    !! every depth and `parquet_kind_name` stays `"PK_STRUCT"`. A caller that needs the inner shape
    !! descends and asks the inner object.
    subroutine nested_field_text(col, kind, out)
        type(parquet_column), intent(in), target :: col   !! the field column.
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
        call field_kind_text(kind, out)
    end subroutine nested_field_text
    !
    !> Writes decimal text for an integer, for `%summary`. A subroutine for the same reason as
    !! `field_kind_text` above.
    pure subroutine i2s(n, out)
        integer(int64), intent(in) :: n                   !! the value.
        character(len=:), allocatable, intent(out) :: out !! its decimal text.
        character(len=32) :: buf
        write(buf, '(i0)') n
        out = trim(buf)
    end subroutine i2s
    !
    ! ==================================================================================
    ! The internal accessor tier: what the read and write paths reach storage through
    ! ==================================================================================
    !
    !> The live `parquet_column` backing field `k` of `col`, as a pointer.
    !!
    !! Hands back the `parquet_column` itself rather than a per-kind value array, exactly as
    !! `parquet_list_column_payload` does: the caller then reaches the values through
    !! `parquet_column_data_ptr` / `parquet_column_string_column` and the field nulls through
    !! `parquet_column_is_null`, all of which already exist, so no new `parquet_columns` surface
    !! is needed to read or write a struct column.
    subroutine parquet_struct_column_field(col, k, p)
        type(parquet_struct_column), intent(in), target :: col !! the column.
        integer, intent(in) :: k                               !! 1-based field index.
        type(parquet_column), pointer, intent(out) :: p        !! alias to that field's column.
        if (.not. allocated(col%fields)) error stop EP//"field: this column has not been initialized"
        if (k < 1 .or. k > size(col%fields)) error stop EP//"field: field index is out of range"
        p => col%fields(k)
    end subroutine parquet_struct_column_field
    !
    !> Copies out every declared field name of `col`, in order, into one blank-padded array.
    !!
    !! One call rather than `nfields` calls of `%field_name(k)`, and blank-padded rather than
    !! deferred-length per element because that is what a `bind(C)` name buffer wants anyway.
    subroutine parquet_struct_column_names(col, names)
        type(parquet_struct_column), intent(in) :: col          !! the column.
        character(len=:), allocatable, intent(out) :: names(:)  !! receives one entry per field.
        integer :: j, wid, nf
        nf = 0
        if (allocated(col%fields)) nf = size(col%fields)
        ! %length measures without allocating and %copy_to fills a fixed slot -- `names(j)` IS
        ! such a slot, so the copy lands straight in the destination with no per-field
        ! deferred-length allocation at all. See check_no_per_element_string_alloc.
        wid = 1
        do j = 1, nf
            wid = max(wid, int(col%field_names%length(j)))
        end do
        allocate(character(len=wid) :: names(nf))
        do j = 1, nf
            call col%field_names%copy_to(j, names(j))
        end do
    end subroutine parquet_struct_column_names
    !
    !> Writes `col`'s per-ROW validity into `valid(1:nrows)` (`.true.` = a present struct) and
    !! reports whether any row is null, in ONE call rather than `nrows` calls of `%is_null(i)`.
    !!
    !! That is the whole reason it exists: `%is_null(i)` is a binding, so calling it per row from
    !! another compilation unit hands a `type(parquet_struct_column)` actual to a `class`
    !! passed-object dummy on every iteration, which is exactly the per-call runtime-descriptor
    !! cost the typed tier is here to avoid. `any_null` comes back alongside because the write
    !! path needs it to decide the field's nullability and can then skip the buffer entirely.
    subroutine parquet_struct_column_row_validity(col, valid, any_null)
        type(parquet_struct_column), intent(in) :: col !! the column.
        logical, intent(out) :: valid(:)               !! receives nrows entries; must be at least that long.
        logical, intent(out) :: any_null               !! .true. if at least one row is a null struct.
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
    end subroutine parquet_struct_column_row_validity
    !
    !> Builds `col` from field columns the READER filled, one at a time.
    !!
    !! The reader cannot use `%adopt_fields` directly: it discovers the field set from the file
    !! and fills one `parquet_column` per field as it goes, so it wants to hand them over in a
    !! single move at the end. This is that move, with `%adopt_fields`' checks -- it is a thin
    !! alias kept separate so the reader's intent is visible at the call site and so a future
    !! change to either caller cannot silently change the other.
    subroutine parquet_struct_column_build(col, names, fields, row_valid)
        type(parquet_struct_column), intent(inout) :: col             !! the column being built.
        character(len=*), intent(in) :: names(:)                      !! field names, in order.
        type(parquet_column), allocatable, intent(inout) :: fields(:) !! per-field columns, moved in.
        logical, intent(in), optional :: row_valid(:)                 !! per-ROW validity.
        call col%adopt_fields(names, fields, row_valid)
    end subroutine parquet_struct_column_build
    !
end module parquet_struct ! GCOVR_EXCL_LINE
