!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the independent parquet_columns module (`parquet_column`, the type-erased
!> column-value foundation both parquet_table and the future container column types build on).
!> Runs in isolation from the rest of the library -- no reader, writer or parquet file involved.
!>
!> The suite is organised around the two things that are easy to get wrong in this type:
!>
!> * **the kind matrix** -- every kind must support every primitive, so the value round-trips
!>   are repeated per kind rather than spot-checked on one;
!> * **the three validity dispatch classes** (column bitmap / embedded string column / inside
!>   the element), which is where a "fixed the bitmap, forgot the temporal cache" bug lands, and
!>   which is therefore tested per class rather than per kind.
!>
!> Abort paths (kind mismatch, bad index, bad permutation, ...) cannot be exercised here because
!> `error stop` kills the process -- they live in test/error_scenarios.f90 as `columns_*`
!> scenarios, driven from test_errors.f90.
module test_columns
    use parquet_columns
    use parquet_list, only : parquet_list_column
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp, parquet_unit_millis
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_columns
    ! Fixture helpers shared with `test_columns_parallel`, whose threaded A/B of `gather_from`
    ! cannot run inside this suite's own parallel region (a nested team collapses to one thread).
    public :: EVERY_KIND, FIXW, make_any_fixture, expect_any_row
    !
    !> The sixteen ARRAY kinds, i.e. every `parquet_column` kind for which `parquet_columns_access`
    !! and `parquet_columns_mutate` generate a per-kind `case` arm. The two string kinds are absent
    !! on purpose: they keep their storage in an embedded `parquet_string_column` and so take a
    !! different path through every operation swept below.
    integer, parameter :: ARRAY_KINDS(16) = [ &
        PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_DATE, PK_TIME, PK_TIMESTAMP, &
        PK_INT32_VEC, PK_INT64_VEC, PK_FLOAT32_VEC, PK_FLOAT64_VEC, PK_LOGICAL_VEC, &
        PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC]
    !> Element count per row for every vector-kind fixture the sweeps build.
    integer(int32), parameter :: FIXW = 2_int32
    !> All eighteen storable kinds: the sixteen array kinds and the two string kinds, which is the
    !! set `gather_from` is generated for and swept over below.
    integer, parameter :: EVERY_KIND(18) = [ARRAY_KINDS, PK_STRING, PK_STRING_VEC]
    !
contains
    !
    subroutine collect_tests_parquet_columns(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("init sets kind, geometry and unit", test_init_geometry), &
            new_unittest("clear resets to an empty PK_NONE column", test_clear), &
            new_unittest("int32/int64 value round-trips", test_roundtrip_integer), &
            new_unittest("float32/float64 value round-trips", test_roundtrip_real), &
            new_unittest("logical value round-trips", test_roundtrip_logical), &
            new_unittest("string value round-trips (shortest element first)", test_roundtrip_string), &
            new_unittest("date/time/timestamp value round-trips", test_roundtrip_temporal), &
            new_unittest("vector kinds round-trip as (width, nrows)", test_roundtrip_vector), &
            new_unittest("string vector uses one flat store, stride width", test_roundtrip_string_vector), &
            new_unittest("data_ptr aliases live storage (writes show through)", test_data_ptr_aliases), &
            new_unittest("a zero-row column has storage, and data_ptr keeps its shape", &
                test_empty_column_has_storage), &
            new_unittest("a null-free column allocates no validity bitmap", test_sparse_validity_unallocated), &
            new_unittest("set_null allocates the bitmap lazily", test_set_null_allocates_bitmap), &
            new_unittest("whole-column set_all drops the bitmap in O(1)", test_set_all_drops_bitmap), &
            new_unittest("set_all/set_at honour modify_nulls=.false.", test_modify_nulls_false), &
            new_unittest("set_at clears only its own null bit", test_set_at_clears_one_bit), &
            new_unittest("compact_validity drops the bitmap only when null-free", test_compact_validity), &
            new_unittest("vector kinds carry width*nrows validity bits", test_vector_validity_bits), &
            new_unittest("string validity delegates to the embedded column", test_string_validity_delegates), &
            new_unittest("temporal validity lives in the element and is cached", test_temporal_validity_cache), &
            new_unittest("append_values grows every kind", test_append_values), &
            new_unittest("append concatenates two columns and their nulls", test_append_column), &
            new_unittest("append_nulls adds all-null rows", test_append_nulls), &
            new_unittest("capacity starts exact-fit on init and adopt", test_capacity_starts_exact), &
            new_unittest("row-at-a-time append grows capacity geometrically", test_capacity_growth_geometric), &
            new_unittest("a rebuild resets capacity to exact fit", test_capacity_rebuild_exact), &
            new_unittest("reserve removes the reallocations that follow it", test_reserve_prevents_realloc), &
            new_unittest("shrink_to_fit releases slack and reports it", test_shrink_to_fit), &
            new_unittest("capacity >= length holds through every operation", test_capacity_invariant), &
            new_unittest("spare capacity is invisible to every reader", test_capacity_invisible), &
            new_unittest("a null-carrying column stays geometric too", test_capacity_with_nulls), &
            new_unittest("append_row_of copies one row and its validity", test_append_row_of), &
            new_unittest("paste overwrites a row range in place", test_paste_values), &
            new_unittest("paste replaces the pasted range's validity", test_paste_validity), &
            new_unittest("reindex permutes values and validity together", test_reindex), &
            new_unittest("delete_by_mask keeps order and recompacts", test_delete_by_mask), &
            new_unittest("gather subsets and reorders in one pass", test_gather), &
            new_unittest("deep_copy is independent of its source", test_deep_copy), &
            new_unittest("zero-row and no-op edge cases", test_edge_cases), &
            new_unittest("unit string is stored, copied and cleared", test_unit_string), &
            new_unittest("kindof/parquet_kind_name report the active kind", test_kind_reporting), &
            new_unittest("every scalar kind supports data_ptr and append_values", test_matrix_scalar_kinds), &
            new_unittest("every vector kind round-trips through all primitives", test_matrix_vector_kinds), &
            new_unittest("set_null/is_null work on every kind", test_matrix_set_null_all_kinds), &
            new_unittest("string vector set_at/get_at and modify_nulls", test_string_vector_set_at), &
            new_unittest("clear_null marks a row valid again", test_clear_null), &
            new_unittest("every PK_* constant has a name", test_kind_names_complete), &
            new_unittest("modify_nulls= is honoured by every kind", test_matrix_modify_nulls_all_kinds), &
            new_unittest("move_from hands storage over for every kind", test_matrix_move_from_all_kinds), &
            new_unittest("element nulls are addressable on every vector kind", test_element_nulls_all_kinds), &
            new_unittest("row queries mean any element null", test_row_query_is_any_element), &
            new_unittest("element_validity reports the true per-element state", test_element_validity), &
            new_unittest("row_validity and element_validity agree on the temporal kinds", &
                test_temporal_validity_masks), &
            new_unittest("set_validity writes a whole mask and only adds nulls", test_set_validity), &
            new_unittest("modify_nulls= protects elements, not whole rows", test_modify_nulls_is_element_wise), &
            new_unittest("element nulls survive sort, delete and append", test_element_nulls_survive_mutation), &
            new_unittest("a character ARRAY trims, a scalar does not", test_string_array_trims_scalar_does_not), &
            new_unittest("set_all preserves null elements under modify_nulls=.false.", test_string_set_all_keeps_nulls), &
            new_unittest("bulk validity is exact at every width, across block boundaries", &
                test_bulk_validity_widths), &
            new_unittest("get_elem/set_elem address one element on every vector kind", &
                test_elem_access_every_vector_kind), &
            new_unittest("shrink_to_fit copies the values back on every kind", &
                test_shrink_to_fit_every_kind), &
            new_unittest("append_row_of copies a row and its nulls on every kind", &
                test_append_row_of_every_kind), &
            new_unittest("get_elem/set_elem address one element of a string vector", &
                test_string_vector_elem_access), &
            new_unittest("reserve accepts a default-kind integer and covers a string column", &
                test_reserve_int32_and_string), &
            new_unittest("shrink_to_fit trims a bitmap left oversized by the shrink", &
                test_shrink_to_fit_trims_bitmap), &
            new_unittest("paste clears a validity range spanning several bitmap blocks", &
                test_paste_clears_multi_block_range), &
            new_unittest("ensure_validity pre-allocates a string column's own validity", &
                test_ensure_validity_string), &
            new_unittest("the element form of the validity API works on a scalar temporal kind", &
                test_temporal_scalar_element_validity), &
            new_unittest("clear_null(i, e) marks one string element valid", &
                test_clear_null_string_element), &
            new_unittest("row_validity/element_validity cover every temporal kind", &
                test_temporal_bulk_validity_every_kind), &
            new_unittest("set_validity writes an element mask on every temporal kind", &
                test_set_validity_elems_temporal), &
            new_unittest("set_validity writes a row mask on the element-carried kinds", &
                test_set_validity_rows_element_carried), &
            new_unittest("set_validity on the string kinds is the store's one-pass rebuild", &
                test_set_validity_string_bulk), &
            new_unittest("a temporal column's null cache is read, not rescanned, while clean", &
                test_temporal_null_cache_is_read_when_clean), &
            new_unittest("an all-zero bitmap reports no nulls without dropping it", &
                test_all_zero_bitmap_reports_no_nulls), &
            new_unittest("every validity query agrees with is_null on a container column", &
                test_container_validity_queries_agree), &
            new_unittest("gather_from builds every kind from another column in one pass", &
                test_gather_from_every_kind), &
            new_unittest("gather_from's mask only adds nulls, and an all-true mask costs no bitmap", &
                test_gather_from_mask_add_only), &
            new_unittest("gather_from: a zero-row source, an empty list, a reused destination, int32", &
                test_gather_from_edges) &
            ]
    end subroutine collect_tests_parquet_columns
    !
    ! ==================================================================================
    ! Lifecycle and geometry
    ! ==================================================================================
    !
    subroutine test_init_geometry(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=:), allocatable :: u
        !
        call c%init(PK_FLOAT64, 7_int64, unit="m/s")
        call check(error, c%kindof() == PK_FLOAT64, "init should set kindof() to PK_FLOAT64")
        if (allocated(error)) return
        call check(error, c%length() == 7_int64, "init should set length() to the requested row count")
        if (allocated(error)) return
        call check(error, c%colwidth() == 1_int32, "a scalar kind should have width 1")
        if (allocated(error)) return
        call c%unit_string(u)
        call check(error, u == "m/s", "init should store the unit string")
        if (allocated(error)) return
        !
        call c%init(PK_INT32_VEC, 3_int64, width=4_int32)
        call check(error, c%colwidth() == 4_int32, "a vector kind should keep the requested width")
        if (allocated(error)) return
        call check(error, c%length() == 3_int64, "a vector column's length() counts ROWS, not elements")
        if (allocated(error)) return
        call c%unit_string(u)
        call check(error, u == "", "re-init should drop the previous unit")
    end subroutine test_init_geometry
    !
    subroutine test_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT64, 4_int64, unit="s")
        call c%set_null(1_int64)
        call c%clear()
        call check(error, c%kindof() == PK_NONE, "clear should reset the kind to PK_NONE")
        if (allocated(error)) return
        call check(error, c%length() == 0_int64, "clear should reset the row count to 0")
        if (allocated(error)) return
        call check(error, c%colwidth() == 1_int32, "clear should reset the width to 1")
        if (allocated(error)) return
        call check(error, c%validity_bytes() == 0_int64, "clear should release the validity bitmap")
    end subroutine test_clear
    !
    ! ==================================================================================
    ! Value round-trips, per kind
    ! ==================================================================================
    !
    subroutine test_roundtrip_integer(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v32
        integer(int64) :: v64
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_all([10_int32, 20_int32, 30_int32])
        call c%get_at(2_int64, v32)
        call check(error, v32 == 20_int32, "get_at should return the int32 value written by set_all")
        if (allocated(error)) return
        call c%set_at(3_int64, -7_int32)
        call c%get_at(3_int64, v32)
        call check(error, v32 == -7_int32, "set_at should overwrite a single int32 element")
        if (allocated(error)) return
        !
        call c%init(PK_INT64, 3_int64)
        call c%set_all([10_int64, 20_int64, 30_int64])
        call c%get_at(1_int64, v64)
        call check(error, v64 == 10_int64, "get_at should return the int64 value written by set_all")
        if (allocated(error)) return
        call c%set_at(1_int64, huge(1_int64))
        call c%get_at(1_int64, v64)
        call check(error, v64 == huge(1_int64), "set_at should store a full-range int64 value")
    end subroutine test_roundtrip_integer
    !
    subroutine test_roundtrip_real(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        real(real32) :: r32
        real(real64) :: r64
        !
        call c%init(PK_FLOAT32, 3_int64)
        call c%set_all([1.5_real32, 2.5_real32, 3.5_real32])
        call c%get_at(3_int64, r32)
        call check(error, r32 == 3.5_real32, "get_at should return the float32 value written by set_all")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64, 3_int64)
        call c%set_all([1.5_real64, 2.5_real64, 3.5_real64])
        call c%get_at(2_int64, r64)
        call check(error, r64 == 2.5_real64, "get_at should return the float64 value written by set_all")
        if (allocated(error)) return
        call c%set_at(2_int64, -0.25_real64)
        call c%get_at(2_int64, r64)
        call check(error, r64 == -0.25_real64, "set_at should overwrite a single float64 element")
    end subroutine test_roundtrip_real
    !
    subroutine test_roundtrip_logical(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        logical :: b
        !
        call c%init(PK_LOGICAL, 3_int64)
        call c%set_all([.true., .false., .true.])
        call c%get_at(2_int64, b)
        call check(error, .not. b, "get_at should return .false. for the element set_all wrote as .false.")
        if (allocated(error)) return
        call c%set_at(2_int64, .true.)
        call c%get_at(2_int64, b)
        call check(error, b, "set_at should overwrite a single logical element")
    end subroutine test_roundtrip_logical
    !
    !> String round-trip with the FIRST element deliberately the shortest (CLAUDE.md's
    !> "sized/typed from the first element" fixture rule) -- a fixture whose first element is
    !> longest would pass even if per-element length were derived from element 1.
    subroutine test_roundtrip_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: sp
        !
        call c%init(PK_STRING, 4_int64)
        call c%set_all(["a    ", "bcdef", "gh   ", "ijk  "])
        call c%get_at(1_int64, s)
        ! Compared with `==`, "a" and "a    " are equal -- Fortran blank-pads the shorter side --
        ! so only the LENGTH distinguishes a trimmed store from a padded one. See
        ! test_string_array_trims_scalar_does_not for the assertion that pins that.
        call check(error, s == "a", "get_at should return the first (shortest) string")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, s == "bcdef", "a later, LONGER element must not be truncated to the first one's length")
        if (allocated(error)) return
        call c%set_at(3_int64, "replacement")
        call c%get_at(3_int64, s)
        call check(error, s == "replacement", "set_at should replace a string element with a longer value")
        if (allocated(error)) return
        call c%string_column(sp)
        call check(error, sp%size() == 4_int64, "string_column should alias a store holding one element per row")
    end subroutine test_roundtrip_string
    !
    subroutine test_roundtrip_temporal(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_date) :: d(3), d1
        type(parquet_time) :: t(2), t1
        type(parquet_timestamp) :: ts(2), ts1
        integer :: i, yy, mm, dd, hh, mi, ss
        !
        do i = 1, 3
            call d(i)%set(2026, 7, 20 + i)
        end do
        call c%init(PK_DATE, 3_int64)
        call c%set_all(d)
        call c%get_at(3_int64, d1)
        call check(error, d1%year() == 2026 .and. d1%month() == 7 .and. d1%day() == 23, &
            "get_at should return the date written by set_all")
        if (allocated(error)) return
        !
        call t(1)%set(1, 2, 3)
        call t(2)%set(23, 59, 58)
        call c%init(PK_TIME, 2_int64)
        call c%set_all(t)
        call c%get_at(2_int64, t1)
        call check(error, t1%hour() == 23 .and. t1%minute() == 59 .and. t1%second() == 58, &
            "get_at should return the time written by set_all")
        if (allocated(error)) return
        !
        call ts(1)%set(2026, 1, 2, 3, 4, 5)
        call ts(2)%set(2026, 12, 31, 23, 59, 59)
        call c%init(PK_TIMESTAMP, 2_int64)
        call c%set_all(ts)
        call c%get_at(1_int64, ts1)
        call ts1%get(yy, mm, dd, hh, mi, ss)
        call check(error, yy == 2026 .and. dd == 2 .and. hh == 3, &
            "get_at should return the timestamp written by set_all")
    end subroutine test_roundtrip_temporal
    !
    subroutine test_roundtrip_vector(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        real(real64) :: mat(3, 4), row(3)
        integer(int32) :: imat(2, 3), irow(2)
        integer :: i
        !
        mat = reshape([(real(i, real64), i = 1, 12)], [3, 4])
        call c%init(PK_FLOAT64_VEC, 4_int64, width=3_int32)
        call c%set_all(mat)
        call c%get_at(3_int64, row)
        call check(error, all(row == [7.0_real64, 8.0_real64, 9.0_real64]), &
            "get_at on a vector column should return that ROW's whole vector, (element, row) order")
        if (allocated(error)) return
        call c%set_at(1_int64, [-1.0_real64, -2.0_real64, -3.0_real64])
        call c%get_at(1_int64, row)
        call check(error, all(row == [-1.0_real64, -2.0_real64, -3.0_real64]), &
            "set_at on a vector column should replace that row's whole vector")
        if (allocated(error)) return
        !
        imat = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [2, 3])
        call c%init(PK_INT32_VEC, 3_int64, width=2_int32)
        call c%set_all(imat)
        call c%get_at(2_int64, irow)
        call check(error, all(irow == [3_int32, 4_int32]), "int32 vector rows should round-trip")
    end subroutine test_roundtrip_vector
    !
    !> PK_STRING_VEC keeps ONE string store for the whole column, flat-indexed (i-1)*width + e
    !> (RF6/s1), so a row's vector is contiguous. This checks both the row view and the flat
    !> layout underneath it.
    subroutine test_roundtrip_string_vector(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=4) :: row(3)
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: sp
        !
        call c%init(PK_STRING_VEC, 2_int64, width=3_int32)
        call c%set_all(reshape(["p ", "qq", "r ", "s ", "tt", "u "], [3, 2]))
        call c%get_at(2_int64, row)
        call check(error, trim(row(1)) == "s" .and. trim(row(2)) == "tt" .and. trim(row(3)) == "u", &
            "get_at on a string vector column should return row 2's three elements in order")
        if (allocated(error)) return
        call check(error, c%length() == 2_int64, "a string vector column's length() counts rows, not strings")
        if (allocated(error)) return
        call c%string_column(sp)
        call check(error, sp%size() == 6_int64, "the flat string store should hold nrows*width elements")
        if (allocated(error)) return
        call sp%get(4_int64, s)
        call check(error, trim(s) == "s", "row 2 element 1 should sit at flat index (2-1)*3 + 1 = 4")
    end subroutine test_roundtrip_string_vector
    !
    !> A zero-row column's storage is ALLOCATED, and `data_ptr` over it keeps every shape a
    !! caller reads off the result.
    !!
    !! `init` allocates the storage at zero rows (`allocate_empty_storage`) precisely so that
    !! `p => col%i32(1:0)` is not a reference to an unallocated allocatable -- non-conforming
    !! however empty the section is, silent on gfortran/ifx/flang, and reported by nagfor's
    !! `-C=array`. **That conformance is not observable from Fortran**, so the checked build is
    !! its only test and this one guards the other half: the shapes the fix had to preserve.
    !! `%capacity()` must stay 0 (the array exists but reserves nothing) and a VECTOR column's
    !! `size(p, 1)` must stay `width` -- the two observables the alternative fixes would have
    !! moved, and the reason this one was chosen.
    subroutine test_empty_column_has_storage(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column), target :: c
        integer(int32), pointer :: p(:)
        integer(int32), pointer :: pv(:,:)
        !
        call c%init(PK_INT32, 0_int64)
        call check(error, c%length() == 0_int64, "a zero-row column holds no rows")
        if (allocated(error)) return
        call check(error, c%capacity() == 0_int64, &
            "and reserves none: the storage exists but its capacity is still 0")
        if (allocated(error)) return
        call c%data_ptr(p)
        call check(error, associated(p), "data_ptr on an empty column returns an associated pointer")
        if (allocated(error)) return
        call check(error, size(p) == 0, "of zero size")
        if (allocated(error)) return
        ! The vector case is the one that discriminates: a shared zero-size target, or a
        ! disassociated pointer, would each have moved this from `width` to 0.
        call c%init(PK_INT32_VEC, 0_int64, width=3_int32)
        call c%data_ptr(pv)
        call check(error, associated(pv), "data_ptr on an empty vector column is associated too")
        if (allocated(error)) return
        call check(error, size(pv, 1) == 3, &
            "an empty vector column's first extent is its width, not 0")
        if (allocated(error)) return
        call check(error, size(pv, 2) == 0, "and its second extent is the row count")
        if (allocated(error)) return
        ! Still usable afterwards: the zero-sized allocation must not confuse the growth path.
        call c%init(PK_INT32, 0_int64)
        call c%append_values([7_int32])
        call c%data_ptr(p)
        call check(error, size(p) == 1 .and. p(1) == 7_int32, &
            "a column that started empty must still grow normally")
    end subroutine test_empty_column_has_storage
    !
    subroutine test_data_ptr_aliases(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column), target :: c
        real(real64), pointer :: p(:)
        real(real64), pointer :: pm(:,:)
        real(real64) :: v
        !
        call c%init(PK_FLOAT64, 3_int64)
        call c%set_all([1.0_real64, 2.0_real64, 3.0_real64])
        call c%data_ptr(p)
        call check(error, size(p) == 3, "data_ptr should expose exactly the column's rows")
        if (allocated(error)) return
        call check(error, sum(p) == 6.0_real64, "data_ptr should alias the values already stored")
        if (allocated(error)) return
        p(2) = 20.0_real64
        call c%get_at(2_int64, v)
        call check(error, v == 20.0_real64, "a write through the data_ptr alias should show in the column")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]))
        call c%data_ptr(pm)
        call check(error, size(pm, 1) == 2 .and. size(pm, 2) == 2, &
            "a vector column's data_ptr should be shaped (width, nrows)")
    end subroutine test_data_ptr_aliases
    !
    ! ==================================================================================
    ! Validity -- one test per dispatch class, plus the sparse-allocation contract
    ! ==================================================================================
    !
    !> The central memory requirement of R2: a column that holds no nulls must not allocate a
    !> validity array at all. This is the easiest property to regress silently, since everything
    !> still *works* if a bitmap is allocated eagerly.
    subroutine test_sparse_validity_unallocated(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: big(1000)
        !
        call c%init(PK_INT32, 1000_int64)
        call check(error, c%validity_bytes() == 0_int64, &
            "a freshly initialized column must not allocate a validity bitmap")
        if (allocated(error)) return
        big = 1_int32
        call c%set_all(big)
        call check(error, c%validity_bytes() == 0_int64, &
            "writing values must not allocate a validity bitmap")
        if (allocated(error)) return
        call check(error, .not. c%any_null(), "a column with no nulls should report any_null() = .false.")
    end subroutine test_sparse_validity_unallocated
    !
    subroutine test_set_null_allocates_bitmap(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT32, 100_int64)
        call c%set_null(50_int64)
        call check(error, c%validity_bytes() > 0_int64, "the first set_null should allocate the bitmap")
        if (allocated(error)) return
        call check(error, c%any_null(), "any_null() should be .true. after set_null")
        if (allocated(error)) return
        call check(error, c%is_null(50_int64), "the row passed to set_null should read back as null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(49_int64), "set_null must not affect its neighbours")
        if (allocated(error)) return
        call check(error, .not. c%is_null(51_int64), "set_null must not affect its neighbours")
    end subroutine test_set_null_allocates_bitmap
    !
    !> RF9: writing every value clears every null bit, so a default set_all can release the
    !> bitmap outright instead of scanning for survivors.
    subroutine test_set_all_drops_bitmap(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT32, 4_int64)
        call c%set_null(2_int64)
        call check(error, c%validity_bytes() > 0_int64, "precondition: the bitmap exists after set_null")
        if (allocated(error)) return
        call c%set_all([1_int32, 2_int32, 3_int32, 4_int32])
        call check(error, c%validity_bytes() == 0_int64, &
            "a whole-column set_all should release the bitmap")
        if (allocated(error)) return
        call check(error, .not. c%any_null(), "a whole-column set_all should leave no nulls behind")
    end subroutine test_set_all_drops_bitmap
    !
    subroutine test_modify_nulls_false(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_all([1_int32, 2_int32, 3_int32])
        call c%set_null(2_int64)
        call c%set_all([10_int32, 20_int32, 30_int32], modify_nulls=.false.)
        call check(error, c%is_null(2_int64), "set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call check(error, c%validity_bytes() > 0_int64, &
            "set_all(modify_nulls=.false.) must not release the bitmap")
        if (allocated(error)) return
        call c%get_at(1_int64, v)
        call check(error, v == 10_int32, "set_all(modify_nulls=.false.) should still write non-null rows")
        if (allocated(error)) return
        call c%set_at(2_int64, 99_int32, modify_nulls=.false.)
        call check(error, c%is_null(2_int64), "set_at(modify_nulls=.false.) must leave a null row null")
    end subroutine test_modify_nulls_false
    !
    subroutine test_set_at_clears_one_bit(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_null(1_int64)
        call c%set_null(3_int64)
        call c%set_at(1_int64, 42_int32)
        call check(error, .not. c%is_null(1_int64), "set_at should clear the null bit of the row it writes")
        if (allocated(error)) return
        call c%get_at(1_int64, v)
        call check(error, v == 42_int32, "set_at should store the value it was given")
        if (allocated(error)) return
        call check(error, c%is_null(3_int64), "set_at must not clear any OTHER row's null bit")
        if (allocated(error)) return
        call check(error, c%validity_bytes() > 0_int64, &
            "a single-cell set_at must not release the bitmap (R2 iii: no scan on single-cell edits)")
    end subroutine test_set_at_clears_one_bit
    !
    subroutine test_compact_validity(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_null(1_int64)
        call c%set_null(2_int64)
        call c%set_at(1_int64, 1_int32)
        call c%compact_validity()
        call check(error, c%validity_bytes() > 0_int64, &
            "compact_validity must keep the bitmap while a null remains")
        if (allocated(error)) return
        call c%set_at(2_int64, 2_int32)
        call c%compact_validity()
        call check(error, c%validity_bytes() == 0_int64, &
            "compact_validity should release the bitmap once the last null is gone")
        if (allocated(error)) return
        call check(error, .not. c%any_null(), "the column should report no nulls after compaction")
        if (allocated(error)) return
        ! the bitmap must GROW when rows are appended to an already-null-bearing column
        call c%init(PK_INT32, 4_int64)
        call c%set_null(1_int64)
        call c%append_nulls(500_int64)
        call check(error, c%is_null(504_int64), "an appended row past the old bitmap size should be null")
        if (allocated(error)) return
        call check(error, c%is_null(1_int64), "growing the bitmap must preserve the existing null bits")
        if (allocated(error)) return
        ! compact_validity is defined for every kind, including those with no column bitmap
        call c%init(PK_DATE, 2_int64)
        call c%compact_validity()
        call check(error, c%any_null(), "compact_validity on a temporal column should refresh, not clear, its nulls")
        if (allocated(error)) return
        call c%init(PK_STRING, 2_int64)
        call c%compact_validity()
        call check(error, c%validity_bytes() == 0_int64, "compact_validity on a string column is a no-op")
    end subroutine test_compact_validity
    !
    !> A vector column needs one bit per ELEMENT (nrows*width), not per row -- sizing the bitmap
    !> by nrows would silently corrupt validity for every row past the first few.
    subroutine test_vector_validity_bits(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int64) :: need_blocks
        !
        call c%init(PK_FLOAT64_VEC, 100_int64, width=8_int32)
        call c%set_null(100_int64)
        need_blocks = (100_int64*8_int64 + 63_int64)/64_int64
        call check(error, c%validity_bytes() >= need_blocks*8_int64, &
            "a vector column's bitmap must cover nrows*width bits, not nrows bits")
        if (allocated(error)) return
        call check(error, c%is_null(100_int64), "the last row of a wide vector column should read back null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(99_int64), "its neighbour must stay valid")
    end subroutine test_vector_validity_bits
    !
    subroutine test_string_validity_delegates(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_string_column), pointer :: sp
        !
        call c%init(PK_STRING, 3_int64)
        call c%set_all(["a  ", "bcd", "ef "])
        call check(error, .not. c%any_null(), "a string column with values written should hold no nulls")
        if (allocated(error)) return
        call c%set_null(2_int64)
        call check(error, c%any_null(), "any_null() should see a null set in the embedded string store")
        if (allocated(error)) return
        call check(error, c%is_null(2_int64), "is_null should report the string element's own null state")
        if (allocated(error)) return
        call check(error, c%validity_bytes() == 0_int64, &
            "a string column must NOT allocate a column-level bitmap -- validity is delegated (DD1)")
        if (allocated(error)) return
        call c%string_column(sp)
        call check(error, sp%null_count() == 1_int64, "the embedded string column should own the null")
    end subroutine test_string_validity_delegates
    !
    !> Temporal kinds carry null state inside each element, and `any_null` caches the answer
    !> (RF8) -- so the interesting case is that the cache is invalidated by a mutation, in both
    !> directions.
    subroutine test_temporal_validity_cache(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_date) :: d(3), one
        integer :: i
        !
        do i = 1, 3
            call d(i)%set(2026, 7, 20 + i)
        end do
        call c%init(PK_DATE, 3_int64)
        call check(error, c%any_null(), "a freshly initialized date column holds null elements")
        if (allocated(error)) return
        call c%set_all(d)
        call check(error, .not. c%any_null(), &
            "the cached null flag must be refreshed after set_all writes real values")
        if (allocated(error)) return
        call c%set_null(2_int64)
        call check(error, c%any_null(), "the cached null flag must be refreshed after set_null")
        if (allocated(error)) return
        call check(error, c%is_null(2_int64), "is_null should read the element's own null state")
        if (allocated(error)) return
        call check(error, c%validity_bytes() == 0_int64, &
            "a temporal column must NOT allocate a column-level bitmap -- the element carries it")
        if (allocated(error)) return
        call one%set(2000, 1, 1)
        call c%set_at(2_int64, one)
        call check(error, .not. c%any_null(), &
            "overwriting the only null element should clear the cached null flag")
    end subroutine test_temporal_validity_cache
    !
    ! ==================================================================================
    ! Structural mutation
    ! ==================================================================================
    !
    subroutine test_append_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        character(len=:), allocatable :: s
        real(real64) :: row(2)
        type(parquet_date) :: d(2), d1
        integer :: i
        !
        call c%init(PK_INT32, 2_int64)
        call c%set_all([1_int32, 2_int32])
        call c%append_values([3_int32, 4_int32])
        call check(error, c%length() == 4_int64, "append_values should grow the column by the value count")
        if (allocated(error)) return
        call c%get_at(4_int64, v)
        call check(error, v == 4_int32, "append_values should store the appended values")
        if (allocated(error)) return
        call c%get_at(1_int64, v)
        call check(error, v == 1_int32, "append_values must preserve the existing values")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 1_int64)
        call c%set_all(["a"])
        call c%append_values(["bcd", "ef "])
        call check(error, c%length() == 3_int64, "append_values should grow a string column")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, s == "bcd", "append_values should store appended strings")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 1_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64], [2, 1]))
        call c%append_values(reshape([3.0_real64, 4.0_real64], [2, 1]))
        call check(error, c%length() == 2_int64, "append_values should grow a vector column by rows")
        if (allocated(error)) return
        call c%get_at(2_int64, row)
        call check(error, all(row == [3.0_real64, 4.0_real64]), "the appended vector row should round-trip")
        if (allocated(error)) return
        !
        do i = 1, 2
            call d(i)%set(2026, 7, 20 + i)
        end do
        call c%init(PK_DATE, 0_int64)
        call c%append_values(d)
        call check(error, c%length() == 2_int64, "append_values should grow a temporal column from empty")
        if (allocated(error)) return
        call c%get_at(2_int64, d1)
        call check(error, d1%day() == 22, "the appended date should round-trip")
    end subroutine test_append_values
    !
    subroutine test_append_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: a, b
        integer(int32) :: v
        character(len=:), allocatable :: s
        !
        call a%init(PK_INT32, 2_int64)
        call a%set_all([1_int32, 2_int32])
        call b%init(PK_INT32, 2_int64)
        call b%set_all([3_int32, 4_int32])
        call b%set_null(1_int64)
        call a%append(b)
        call check(error, a%length() == 4_int64, "append should extend the destination by the source's rows")
        if (allocated(error)) return
        call a%get_at(4_int64, v)
        call check(error, v == 4_int32, "append should copy the source's values")
        if (allocated(error)) return
        call check(error, a%is_null(3_int64), "append should carry the source's nulls across")
        if (allocated(error)) return
        call check(error, .not. a%is_null(1_int64), "append must not disturb the destination's own rows")
        if (allocated(error)) return
        call check(error, b%length() == 2_int64, "append must leave the source unchanged")
        if (allocated(error)) return
        !
        call a%init(PK_STRING, 1_int64)
        call a%set_all(["a"])
        call b%init(PK_STRING, 2_int64)
        call b%set_all(["bcd", "ef "])
        call a%append(b)
        call check(error, a%length() == 3_int64, "append should extend a string column")
        if (allocated(error)) return
        call a%get_at(3_int64, s)
        call check(error, s == "ef ", "append should copy the source's strings")
    end subroutine test_append_column
    !
    subroutine test_append_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_FLOAT64, 2_int64)
        call c%set_all([1.0_real64, 2.0_real64])
        call check(error, c%validity_bytes() == 0_int64, "precondition: no bitmap before any null exists")
        if (allocated(error)) return
        call c%append_nulls(3_int64)
        call check(error, c%length() == 5_int64, "append_nulls should add the requested number of rows")
        if (allocated(error)) return
        call check(error, c%is_null(5_int64), "the appended rows should be null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64), "the existing rows must stay non-null")
        if (allocated(error)) return
        call check(error, c%validity_bytes() > 0_int64, "append_nulls should materialize the bitmap")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 1_int64)
        call c%set_all(["a"])
        call c%append_nulls(2_int64)
        call check(error, c%length() == 3_int64, "append_nulls should grow a string column")
        if (allocated(error)) return
        call check(error, c%is_null(3_int64), "the appended string rows should be null")
    end subroutine test_append_nulls
    !
    !> `paste` writes into rows that already exist, so the things worth asserting are that the
    !! right rows changed, that the neighbours did NOT, and that the geometry is untouched --
    !! an off-by-one in the destination cursor is the failure this operation invites, and it is
    !! silent (the column still has the right length, just wrong values in it).
    subroutine test_paste_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: dst, src
        integer(int32) :: v
        real(real64) :: row(2)
        type(parquet_date) :: d(2), d1
        integer :: i
        !
        ! Whole source, into the middle: rows 2..4 change, 1 and 5 must not.
        call dst%init(PK_INT32, 5_int64)
        call dst%set_all([1_int32, 2_int32, 3_int32, 4_int32, 5_int32])
        call src%init(PK_INT32, 3_int64)
        call src%set_all([70_int32, 80_int32, 90_int32])
        call dst%paste(src, 2_int64)
        call check(error, dst%length() == 5_int64, "paste must not change the destination's row count")
        if (allocated(error)) return
        call dst%get_at(1_int64, v)
        call check(error, v == 1_int32, "paste must not touch the row before its destination range")
        if (allocated(error)) return
        call dst%get_at(2_int64, v)
        call check(error, v == 70_int32, "paste should write the source's first row at `at`")
        if (allocated(error)) return
        call dst%get_at(4_int64, v)
        call check(error, v == 90_int32, "paste should write the source's last row at at+count-1")
        if (allocated(error)) return
        call dst%get_at(5_int64, v)
        call check(error, v == 5_int32, "paste must not touch the row after its destination range")
        if (allocated(error)) return
        call check(error, src%length() == 3_int64, "paste must leave the source unchanged")
        if (allocated(error)) return
        !
        ! A sub-range of the source: from= skips its leading rows, count= bounds the copy. This
        ! is the shape the slice regime uses to trim a row group to the slice's own bounds.
        call dst%init(PK_INT32, 4_int64)
        call dst%set_all([0_int32, 0_int32, 0_int32, 0_int32])
        call src%init(PK_INT32, 4_int64)
        call src%set_all([11_int32, 22_int32, 33_int32, 44_int32])
        call dst%paste(src, 1_int64, 3_int64, 2_int64)
        call dst%get_at(1_int64, v)
        call check(error, v == 33_int32, "paste(from=3) should start at the source's third row")
        if (allocated(error)) return
        call dst%get_at(2_int64, v)
        call check(error, v == 44_int32, "paste(from=3, count=2) should copy exactly two rows")
        if (allocated(error)) return
        call dst%get_at(3_int64, v)
        call check(error, v == 0_int32, "paste(count=2) must stop after two rows")
        if (allocated(error)) return
        !
        ! count=0 is a no-op rather than an error, matching append's own empty-source behaviour.
        call dst%paste(src, 1_int64, 1_int64, 0_int64)
        call dst%get_at(1_int64, v)
        call check(error, v == 33_int32, "paste(count=0) should change nothing at all")
        if (allocated(error)) return
        !
        ! A vector kind: the copy is per row, carrying every element of the row with it.
        call dst%init(PK_FLOAT64_VEC, 3_int64, width=2_int32)
        call dst%set_all(reshape([0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, &
            0.0_real64, 0.0_real64], [2, 3]))
        call src%init(PK_FLOAT64_VEC, 1_int64, width=2_int32)
        call src%set_all(reshape([8.5_real64, 9.5_real64], [2, 1]))
        call dst%paste(src, 3_int64)
        call dst%get_at(3_int64, row)
        call check(error, all(row == [8.5_real64, 9.5_real64]), &
            "paste should carry every element of a vector row")
        if (allocated(error)) return
        call dst%get_at(2_int64, row)
        call check(error, all(row == [0.0_real64, 0.0_real64]), &
            "paste must not disturb a neighbouring vector row")
        if (allocated(error)) return
        !
        ! A temporal kind: the pasted elements carry their own validity, so a real date landing on
        ! an init-time null row has to make that row valid without any bitmap being involved.
        do i = 1, 2
            call d(i)%set(2026, 7, 20 + i)
        end do
        call dst%init(PK_DATE, 3_int64)
        call check(error, dst%is_null(2_int64), "precondition: a fresh temporal column is all null")
        if (allocated(error)) return
        call src%init(PK_DATE, 2_int64)
        call src%set_all(d)
        call dst%paste(src, 2_int64)
        call dst%get_at(2_int64, d1)
        call check(error, d1%year() == 2026 .and. d1%month() == 7 .and. d1%day() == 21, &
            "paste should copy a temporal element's value")
        if (allocated(error)) return
        call check(error, .not. dst%is_null(2_int64), &
            "a pasted temporal element should make its row valid again")
        if (allocated(error)) return
        call check(error, dst%is_null(1_int64), &
            "paste must not disturb a temporal row outside its range")
    end subroutine test_paste_values
    !
    !> Validity is *replaced* over the pasted range, not merged into it. Both directions matter:
    !! a null source element must null the destination row, and a valid source element must clear
    !! a null the destination already had. Merging (append's rule, where the destination rows are
    !! always fresh) would leave a stale null on top of a value that was really read.
    subroutine test_paste_validity(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: dst, src
        !
        ! An all-valid source must not materialize a bitmap on a null-free destination (R2).
        call dst%init(PK_INT32, 4_int64)
        call dst%set_all([1_int32, 2_int32, 3_int32, 4_int32])
        call src%init(PK_INT32, 2_int64)
        call src%set_all([9_int32, 9_int32])
        call dst%paste(src, 1_int64)
        call check(error, dst%validity_bytes() == 0_int64, &
            "pasting an all-valid source must not allocate a bitmap on a null-free column")
        if (allocated(error)) return
        call check(error, .not. dst%any_null(), "a null-free paste must leave the column null-free")
        if (allocated(error)) return
        !
        ! A null in the source nulls the destination row it lands on, and only that one.
        call src%set_null(2_int64)
        call dst%paste(src, 3_int64)
        call check(error, dst%is_null(4_int64), "paste should carry the source's null across")
        if (allocated(error)) return
        call check(error, .not. dst%is_null(3_int64), &
            "paste must not null a row whose source element was valid")
        if (allocated(error)) return
        !
        ! The other direction: an all-valid source over a row the destination had marked null
        ! must clear it. This is the assertion that distinguishes replace from merge.
        call dst%init(PK_INT32, 3_int64)
        call dst%set_all([1_int32, 2_int32, 3_int32])
        call dst%set_null(2_int64)
        call check(error, dst%is_null(2_int64), "precondition: the destination row starts null")
        if (allocated(error)) return
        call src%init(PK_INT32, 1_int64)
        call src%set_all([42_int32])
        call dst%paste(src, 2_int64)
        call check(error, .not. dst%is_null(2_int64), &
            "pasting a valid element over a null row must clear the null")
        if (allocated(error)) return
        !
        ! The same clearing has to happen on the OTHER code path -- when the source carries nulls
        ! of its own, so the per-element copy runs rather than the all-valid shortcut. Row 2 of
        ! the source is valid and lands on a destination row that is null, so that null must go.
        ! Without this case a "merge instead of replace" bug survives: the all-valid path above
        ! never reaches the branch that copies bits one at a time.
        call dst%init(PK_INT32, 3_int64)
        call dst%set_all([1_int32, 2_int32, 3_int32])
        call dst%set_null(2_int64)
        call src%init(PK_INT32, 2_int64)
        call src%set_all([51_int32, 52_int32])
        call src%set_null(1_int64)
        call dst%paste(src, 1_int64)
        call check(error, dst%is_null(1_int64), &
            "a null source element should null the row it is pasted onto")
        if (allocated(error)) return
        call check(error, .not. dst%is_null(2_int64), &
            "a valid source element must clear a null the destination already had")
        if (allocated(error)) return
        !
        ! ... and it must clear only the pasted range, leaving the column's other nulls alone.
        call dst%init(PK_INT32, 3_int64)
        call dst%set_all([1_int32, 2_int32, 3_int32])
        call dst%set_null(1_int64)
        call dst%set_null(3_int64)
        call src%init(PK_INT32, 1_int64)
        call src%set_all([42_int32])
        call dst%paste(src, 2_int64)
        call check(error, dst%is_null(1_int64) .and. dst%is_null(3_int64), &
            "paste must leave nulls outside its destination range untouched")
        if (allocated(error)) return
        !
        ! On a vector kind a row's null covers every element of that row, in both directions.
        call dst%init(PK_INT32_VEC, 2_int64, width=2_int32)
        call dst%set_all(reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2]))
        call src%init(PK_INT32_VEC, 1_int64, width=2_int32)
        call src%set_all(reshape([7_int32, 8_int32], [2, 1]))
        call src%set_null(1_int64)
        call dst%paste(src, 2_int64)
        call check(error, dst%is_null(2_int64), "a null vector row should paste as a null row")
        if (allocated(error)) return
        call check(error, .not. dst%is_null(1_int64), &
            "pasting a null vector row must not null its neighbour")
    end subroutine test_paste_validity
    !
    subroutine test_reindex(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        character(len=:), allocatable :: s
        real(real64) :: row(2)
        !
        call c%init(PK_INT32, 4_int64)
        call c%set_all([10_int32, 20_int32, 30_int32, 40_int32])
        call c%set_null(2_int64)
        call c%reindex([4_int64, 3_int64, 2_int64, 1_int64])
        call c%get_at(1_int64, v)
        call check(error, v == 40_int32, "reindex should move the row named by perm(1) to position 1")
        if (allocated(error)) return
        call check(error, c%is_null(3_int64), "reindex should move a null along with its row")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64), "reindex must not leave stale null bits behind")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 3_int64)
        call c%set_all(["a  ", "bcd", "ef "])
        call c%reindex([3_int64, 1_int64, 2_int64])
        call c%get_at(1_int64, s)
        call check(error, s == "ef ", "reindex should permute a string column's payload")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, s == "a  ", "reindex should keep every string element intact")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]))
        call c%reindex([2_int64, 1_int64])
        call c%get_at(1_int64, row)
        call check(error, all(row == [3.0_real64, 4.0_real64]), "reindex should permute whole vector rows")
    end subroutine test_reindex
    !
    subroutine test_delete_by_mask(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        character(len=:), allocatable :: s
        !
        call c%init(PK_INT32, 5_int64)
        call c%set_all([1_int32, 2_int32, 3_int32, 4_int32, 5_int32])
        call c%set_null(4_int64)
        call c%delete_by_mask([.true., .false., .true., .true., .false.])
        call check(error, c%length() == 3_int64, "delete_by_mask should keep exactly the .true. rows")
        if (allocated(error)) return
        call c%get_at(2_int64, v)
        call check(error, v == 3_int32, "delete_by_mask should preserve the surviving rows' order")
        if (allocated(error)) return
        call check(error, c%is_null(3_int64), "a surviving null row should still be null after compaction")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64), "a surviving non-null row must stay non-null")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 4_int64)
        call c%set_all(["a    ", "bcdef", "gh   ", "ijk  "])
        call c%delete_by_mask([.false., .true., .false., .true.])
        call check(error, c%length() == 2_int64, "delete_by_mask should shrink a string column")
        if (allocated(error)) return
        call c%get_at(1_int64, s)
        call check(error, s == "bcdef", "the surviving string payload must be intact, not truncated")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, s == "ijk  ", "the second surviving string should follow in order")
    end subroutine test_delete_by_mask
    !
    !> `%gather` is the subset-and-reorder primitive neither `reindex` (a permutation of the whole
    !> column) nor `delete_by_mask` (a subset in the existing order) provides between them.
    !>
    !> Both index kinds are exercised on purpose: the int32 form converts and delegates, and a
    !> generic whose thin specific is mis-wired compiles and runs perfectly well.
    subroutine test_gather(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        character(len=:), allocatable :: s
        real(real64) :: row(2)
        !
        ! Subset AND reorder in one call -- the thing that distinguishes this from its two siblings.
        call c%init(PK_INT32, 5_int64)
        call c%set_all([10_int32, 20_int32, 30_int32, 40_int32, 50_int32])
        call c%set_null(4_int64)
        call c%gather([4_int64, 1_int64])
        call check(error, c%length() == 2_int64, "gather must set the row count to the index count")
        if (allocated(error)) return
        call check(error, c%is_null(1_int64), "gather must carry a null along with the row it belongs to")
        if (allocated(error)) return
        call c%get_at(2_int64, v)
        call check(error, v == 10_int32, "gather must take rows in the order the index list gives")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64), "gather must not leave a stale null bit behind")
        if (allocated(error)) return
        !
        ! The int32 form must agree with the int64 one, on the same column and the same indices.
        call c%init(PK_INT32, 5_int64)
        call c%set_all([10_int32, 20_int32, 30_int32, 40_int32, 50_int32])
        call c%gather([4_int32, 1_int32])
        call c%get_at(1_int64, v)
        call check(error, v == 40_int32, "the int32 index form must gather the same rows")
        if (allocated(error)) return
        !
        ! Repeats are permitted -- it is a gather, not a permutation -- so a column can also GROW.
        call c%init(PK_INT32, 2_int64)
        call c%set_all([7_int32, 8_int32])
        call c%gather([2_int64, 2_int64, 1_int64])
        call check(error, c%length() == 3_int64, "a repeated index must lengthen the column")
        if (allocated(error)) return
        call c%get_at(1_int64, v)
        call check(error, v == 8_int32, "a repeated row must appear at each position naming it")
        if (allocated(error)) return
        call c%get_at(2_int64, v)
        call check(error, v == 8_int32, "the second copy of a repeated row must hold the same value")
        if (allocated(error)) return
        !
        ! An empty selection leaves a valid, empty column rather than a one-row remnant: the index
        ! expansion the string path uses pads its allocation to 1, so this case has to be sliced.
        call c%init(PK_INT32, 3_int64)
        call c%set_all([1_int32, 2_int32, 3_int32])
        call c%gather([integer(int64) ::])
        call check(error, c%length() == 0_int64, "an empty index list must empty the column")
        if (allocated(error)) return
        !
        ! The string store has its own gather, which must recount n_null rather than carry it over.
        call c%init(PK_STRING, 4_int64)
        call c%set_all(["a    ", "bcdef", "gh   ", "ijk  "])
        call c%set_null(2_int64)
        call c%gather([3_int64, 2_int64])
        call check(error, c%length() == 2_int64, "gather must shrink a string column")
        if (allocated(error)) return
        call c%get_at(1_int64, s)
        call check(error, s == "gh   ", "a gathered string payload must be intact, not truncated")
        if (allocated(error)) return
        call check(error, c%is_null(2_int64), "a gathered string null must land at its new position")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64), "a gathered non-null string must stay non-null")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 3_int64)
        call c%set_all(["a  ", "bcd", "ef "])
        call c%gather([integer(int64) ::])
        call check(error, c%length() == 0_int64, "an empty gather of a string column must empty it")
        if (allocated(error)) return
        !
        ! A vector column moves whole rows, and its validity is per ELEMENT.
        call c%init(PK_FLOAT64_VEC, 3_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, &
            6.0_real64], [2, 3]))
        call c%set_null(2_int64, 1_int64)
        call c%gather([3_int64, 2_int64])
        call c%get_at(1_int64, row)
        call check(error, all(row == [5.0_real64, 6.0_real64]), "gather must move whole vector rows")
        if (allocated(error)) return
        call check(error, c%is_null(2_int64, 1_int64), &
            "gather must move a per-element null to the element it belongs to")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 2_int64), &
            "gather must not widen a per-element null across the row")
        if (allocated(error)) return
        !
        ! A temporal column carries its null state INSIDE each element, so `any_null` answers from a
        ! cache that a mutation has to invalidate. Gathering only the non-null rows must therefore
        ! make the column report no nulls -- a gather that forgets to mark the cache dirty keeps
        ! answering .true. here while every value it returns is correct.
        call c%init(PK_DATE, 3_int64)
        call c%set_at(1_int64, parquet_date(2024, 1, 1))
        call c%set_at(3_int64, parquet_date(2024, 1, 3))
        call check(error, c%any_null(), "the fixture must start with its middle element null")
        if (allocated(error)) return
        call c%gather([3_int64, 1_int64])
        call check(error, .not. c%any_null(), &
            "gathering away every null must clear a temporal column's cached null state")
    end subroutine test_gather
    !
    subroutine test_deep_copy(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: a, b
        integer(int32) :: v
        character(len=:), allocatable :: s, u
        !
        call a%init(PK_INT32, 3_int64, unit="count")
        call a%set_all([1_int32, 2_int32, 3_int32])
        call a%set_null(2_int64)
        call a%deep_copy(b)
        call check(error, b%length() == 3_int64, "deep_copy should reproduce the row count")
        if (allocated(error)) return
        call check(error, b%kindof() == PK_INT32, "deep_copy should reproduce the kind")
        if (allocated(error)) return
        call check(error, b%is_null(2_int64), "deep_copy should reproduce the validity state")
        if (allocated(error)) return
        call b%unit_string(u)
        call check(error, u == "count", "deep_copy should reproduce the unit string")
        if (allocated(error)) return
        call b%set_at(1_int64, 99_int32)
        call a%get_at(1_int64, v)
        call check(error, v == 1_int32, "mutating the copy must not change the source")
        if (allocated(error)) return
        call a%set_at(3_int64, 77_int32)
        call b%get_at(3_int64, v)
        call check(error, v == 3_int32, "mutating the source must not change the copy")
        if (allocated(error)) return
        !
        call a%init(PK_STRING, 2_int64)
        call a%set_all(["a  ", "bcd"])
        call a%deep_copy(b)
        call b%set_at(1_int64, "changed")
        call a%get_at(1_int64, s)
        call check(error, s == "a  ", "a string column's deep copy must own its payload independently")
    end subroutine test_deep_copy
    !
    subroutine test_edge_cases(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: a, b
        integer(int32) :: v
        logical :: none_keep(3)
        !
        call a%init(PK_INT32, 0_int64)
        call check(error, a%length() == 0_int64, "a zero-row column should be legal")
        if (allocated(error)) return
        call check(error, .not. a%any_null(), "a zero-row column holds no nulls")
        if (allocated(error)) return
        call a%deep_copy(b)
        call check(error, b%length() == 0_int64, "deep_copy of an empty column should give an empty column")
        if (allocated(error)) return
        !
        call a%init(PK_INT32, 3_int64)
        call a%set_all([1_int32, 2_int32, 3_int32])
        call a%reindex([1_int64, 2_int64, 3_int64])
        call a%get_at(2_int64, v)
        call check(error, v == 2_int32, "the identity permutation should leave the column unchanged")
        if (allocated(error)) return
        call a%delete_by_mask([.true., .true., .true.])
        call check(error, a%length() == 3_int64, "a keep-everything mask should change nothing")
        if (allocated(error)) return
        none_keep = .false.
        call a%delete_by_mask(none_keep)
        call check(error, a%length() == 0_int64, "a keep-nothing mask should empty the column")
        if (allocated(error)) return
        call a%append_nulls(0_int64)
        call check(error, a%length() == 0_int64, "append_nulls(0) should be a no-op")
        if (allocated(error)) return
        call a%append_values([5_int32])
        call check(error, a%length() == 1_int64, "an emptied column should still accept appends")
    end subroutine test_edge_cases
    !
    subroutine test_unit_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=:), allocatable :: u
        !
        call c%init(PK_FLOAT64, 1_int64)
        call c%unit_string(u)
        call check(error, u == "", "a column with no unit should report an empty unit string")
        if (allocated(error)) return
        call c%set_unit("km/h")
        call c%unit_string(u)
        call check(error, u == "km/h", "set_unit should store the unit string")
        if (allocated(error)) return
        call c%set_unit("")
        call c%unit_string(u)
        call check(error, u == "", "set_unit with an empty string should clear the unit")
    end subroutine test_unit_string
    !
    subroutine test_kind_reporting(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=:), allocatable :: nm
        !
        call parquet_kind_name(PK_NONE, nm)
        call check(error, nm == "PK_NONE", "parquet_kind_name should name PK_NONE")
        if (allocated(error)) return
        call c%init(PK_TIMESTAMP_VEC, 1_int64, width=2_int32)
        call parquet_kind_name(c%kindof(), nm)
        call check(error, nm == "PK_TIMESTAMP_VEC", "parquet_kind_name should name the column's active kind")
        if (allocated(error)) return
        call parquet_kind_name(PK_LIST, nm)
        call check(error, nm == "PK_LIST", "the reserved container kinds should still have names")
    end subroutine test_kind_reporting
    !
    ! ==================================================================================
    ! Kind matrix -- every kind must support every primitive, so the remaining kinds are
    ! swept here rather than relying on the representative kinds used above. A gap in this
    ! matrix is exactly what a generator template bug looks like.
    ! ==================================================================================
    !
    subroutine test_matrix_scalar_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column), target :: c
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        real(real32), pointer :: pr32(:)
        real(real64), pointer :: pr64(:)
        logical, pointer :: pb(:)
        type(parquet_date), pointer :: pd(:)
        type(parquet_time), pointer :: pt(:)
        type(parquet_timestamp), pointer :: pts(:)
        type(parquet_date) :: d(2)
        type(parquet_time) :: t(2)
        type(parquet_timestamp) :: ts(2)
        integer :: i
        !
        call c%init(PK_INT32, 2_int64)
        call c%set_all([1_int32, 2_int32])
        call c%data_ptr(p32)
        call check(error, sum(p32) == 3_int32, "int32 data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([3_int32])
        call check(error, c%length() == 3_int64, "int32 append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_INT32")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_INT64, 2_int64)
        call c%set_all([1_int64, 2_int64])
        call c%data_ptr(p64)
        call check(error, sum(p64) == 3_int64, "int64 data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([3_int64])
        call check(error, c%length() == 3_int64, "int64 append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_INT64")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT32, 2_int64)
        call c%set_all([1.0_real32, 2.0_real32])
        call c%data_ptr(pr32)
        call check(error, sum(pr32) == 3.0_real32, "float32 data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([3.0_real32])
        call check(error, c%length() == 3_int64, "float32 append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_FLOAT32")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64, 2_int64)
        call c%set_all([1.0_real64, 2.0_real64])
        call c%data_ptr(pr64)
        call check(error, sum(pr64) == 3.0_real64, "float64 data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([3.0_real64])
        call check(error, c%length() == 3_int64, "float64 append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_FLOAT64")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_LOGICAL, 2_int64)
        call c%set_all([.true., .false.])
        call c%data_ptr(pb)
        call check(error, count(pb) == 1, "logical data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([.true.])
        call check(error, c%length() == 3_int64, "logical append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_LOGICAL")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        do i = 1, 2
            call d(i)%set(2026, 7, 20 + i)
            call t(i)%set(i, 30, 0)
            call ts(i)%set(2026, 7, 20 + i, 12, 0, 0)
        end do
        call c%init(PK_DATE, 2_int64)
        call c%set_all(d)
        call c%data_ptr(pd)
        call check(error, pd(2)%day() == 22, "date data_ptr should alias the stored elements")
        if (allocated(error)) return
        call c%append_values(d)
        call check(error, c%length() == 4_int64, "date append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_DATE")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_TIME, 2_int64)
        call c%set_all(t)
        call c%data_ptr(pt)
        call check(error, pt(2)%hour() == 2, "time data_ptr should alias the stored elements")
        if (allocated(error)) return
        call c%append_values(t)
        call check(error, c%length() == 4_int64, "time append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_TIME")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_TIMESTAMP, 2_int64)
        call c%set_all(ts)
        call c%data_ptr(pts)
        call check(error, .not. pts(1)%is_null(), "timestamp data_ptr should alias the stored elements")
        if (allocated(error)) return
        call c%append_values(ts)
        call check(error, c%length() == 4_int64, "timestamp append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_TIMESTAMP")
        if (allocated(error)) return
    end subroutine test_matrix_scalar_kinds
    !
    subroutine test_matrix_vector_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column), target :: c
        integer(int32), pointer :: p32(:,:)
        integer(int64), pointer :: p64(:,:)
        real(real32), pointer :: pr32(:,:)
        real(real64), pointer :: pr64(:,:)
        logical, pointer :: pb(:,:)
        type(parquet_date), pointer :: pd(:,:)
        type(parquet_time), pointer :: pt(:,:)
        type(parquet_timestamp), pointer :: pts(:,:)
        integer(int32) :: r32(2)
        integer(int64) :: r64(2)
        real(real32) :: rr32(2)
        real(real64) :: rr64(2)
        logical :: rb(2)
        type(parquet_date) :: dm(2, 2), rd(2)
        type(parquet_time) :: tm(2, 2), rt(2)
        type(parquet_timestamp) :: tsm(2, 2), rts(2)
        integer :: i, j
        !
        call c%init(PK_INT32_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2]))
        call c%get_at(2_int64, r32)
        call check(error, all(r32 == [3_int32, 4_int32]), "int32 vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [9_int32, 8_int32])
        call c%data_ptr(p32)
        call check(error, p32(1, 1) == 9_int32, "int32 vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([5_int32, 6_int32], [2, 1]))
        call check(error, c%length() == 3_int64, "int32 vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_INT32_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_INT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2]))
        call c%get_at(2_int64, r64)
        call check(error, all(r64 == [3_int64, 4_int64]), "int64 vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [9_int64, 8_int64])
        call c%data_ptr(p64)
        call check(error, p64(1, 1) == 9_int64, "int64 vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([5_int64, 6_int64], [2, 1]))
        call check(error, c%length() == 3_int64, "int64 vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_INT64_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT32_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32], [2, 2]))
        call c%get_at(2_int64, rr32)
        call check(error, all(rr32 == [3.0_real32, 4.0_real32]), "float32 vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [9.0_real32, 8.0_real32])
        call c%data_ptr(pr32)
        call check(error, pr32(1, 1) == 9.0_real32, "float32 vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([5.0_real32, 6.0_real32], [2, 1]))
        call check(error, c%length() == 3_int64, "float32 vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_FLOAT32_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]))
        call c%get_at(2_int64, rr64)
        call check(error, all(rr64 == [3.0_real64, 4.0_real64]), "float64 vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [9.0_real64, 8.0_real64])
        call c%data_ptr(pr64)
        call check(error, pr64(1, 1) == 9.0_real64, "float64 vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([5.0_real64, 6.0_real64], [2, 1]))
        call check(error, c%length() == 3_int64, "float64 vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_FLOAT64_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_LOGICAL_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([.true., .false., .true., .true.], [2, 2]))
        call c%get_at(2_int64, rb)
        call check(error, all(rb), "logical vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [.false., .false.])
        call c%data_ptr(pb)
        call check(error, .not. pb(1, 1), "logical vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([.true., .false.], [2, 1]))
        call check(error, c%length() == 3_int64, "logical vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_LOGICAL_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        do j = 1, 2
            do i = 1, 2
                call dm(i, j)%set(2026, 7, 10*j + i)
                call tm(i, j)%set(j, i, 0)
                call tsm(i, j)%set(2026, 7, 10*j + i, 6, 0, 0)
            end do
        end do
        call c%init(PK_DATE_VEC, 2_int64, width=2_int32)
        call c%set_all(dm)
        call c%get_at(2_int64, rd)
        call check(error, rd(1)%day() == 21, "date vector get_at should return that row's elements")
        if (allocated(error)) return
        call c%set_at(1_int64, rd)
        call c%data_ptr(pd)
        call check(error, pd(1, 1)%day() == 21, "date vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(dm(:, 1:1))
        call check(error, c%length() == 3_int64, "date vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_DATE_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_TIME_VEC, 2_int64, width=2_int32)
        call c%set_all(tm)
        call c%get_at(2_int64, rt)
        call check(error, rt(1)%hour() == 2, "time vector get_at should return that row's elements")
        if (allocated(error)) return
        call c%set_at(1_int64, rt)
        call c%data_ptr(pt)
        call check(error, pt(1, 1)%hour() == 2, "time vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(tm(:, 1:1))
        call check(error, c%length() == 3_int64, "time vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_TIME_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_TIMESTAMP_VEC, 2_int64, width=2_int32)
        call c%set_all(tsm)
        call c%get_at(2_int64, rts)
        call check(error, .not. rts(1)%is_null(), "timestamp vector get_at should return that row's elements")
        if (allocated(error)) return
        call c%set_at(1_int64, rts)
        call c%data_ptr(pts)
        call check(error, .not. pts(1, 1)%is_null(), "timestamp vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(tsm(:, 1:1))
        call check(error, c%length() == 3_int64, "timestamp vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_TIMESTAMP_VEC")
        if (allocated(error)) return
    end subroutine test_matrix_vector_kinds
    !
    !> Kind-agnostic structural sweep: reindex, deep_copy, delete_by_mask and append_nulls all
    !> dispatch internally on the column's kind, so calling this once per kind is what actually
    !> covers every arm of that dispatch. Expects a column with at least two rows.
    subroutine exercise_structural(c, error, label)
        class(parquet_column), intent(inout) :: c
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: label
        type(parquet_column) :: copy
        integer(int64) :: n, i
        integer(int64), allocatable :: perm(:)
        logical, allocatable :: keep(:)
        !
        n = c%length()
        allocate(perm(n), keep(n))
        do i = 1_int64, n
            perm(i) = n - i + 1_int64
            keep(i) = (mod(i, 2_int64) == 1_int64)
        end do
        call c%reindex(perm)
        call check(error, c%length() == n, label//": reindex must preserve the row count")
        if (allocated(error)) return
        call c%deep_copy(copy)
        call check(error, copy%length() == n .and. copy%kindof() == c%kindof(), &
            label//": deep_copy must reproduce the row count and kind")
        if (allocated(error)) return
        call c%append(copy)
        call check(error, c%length() == 2_int64*n, label//": append must add the source's rows")
        if (allocated(error)) return
        call c%delete_by_mask([(i <= n, i = 1_int64, 2_int64*n)])
        call check(error, c%length() == n, label//": the appended half should be droppable again")
        if (allocated(error)) return
        call c%delete_by_mask(keep)
        call check(error, c%length() == count(keep, kind=int64), &
            label//": delete_by_mask must keep exactly the .true. rows")
        if (allocated(error)) return
        call c%append_nulls(1_int64)
        call check(error, c%is_null(c%length()), label//": append_nulls must append a null row")
    end subroutine exercise_structural
    !
    !> set_null/is_null/any_null dispatch on the kind three different ways (column bitmap,
    !> embedded string column, inside the element), so every kind gets its own pass here --
    !> this is the sweep that would catch "fixed the bitmap, forgot the temporal cache".
    subroutine test_matrix_set_null_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        !
        call null_sweep(error, PK_INT32, 1_int32, "PK_INT32")
        if (allocated(error)) return
        call null_sweep(error, PK_INT64, 1_int32, "PK_INT64")
        if (allocated(error)) return
        call null_sweep(error, PK_FLOAT32, 1_int32, "PK_FLOAT32")
        if (allocated(error)) return
        call null_sweep(error, PK_FLOAT64, 1_int32, "PK_FLOAT64")
        if (allocated(error)) return
        call null_sweep(error, PK_LOGICAL, 1_int32, "PK_LOGICAL")
        if (allocated(error)) return
        call null_sweep(error, PK_STRING, 1_int32, "PK_STRING")
        if (allocated(error)) return
        call null_sweep(error, PK_DATE, 1_int32, "PK_DATE")
        if (allocated(error)) return
        call null_sweep(error, PK_TIME, 1_int32, "PK_TIME")
        if (allocated(error)) return
        call null_sweep(error, PK_TIMESTAMP, 1_int32, "PK_TIMESTAMP")
        if (allocated(error)) return
        call null_sweep(error, PK_INT32_VEC, 2_int32, "PK_INT32_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_INT64_VEC, 2_int32, "PK_INT64_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_FLOAT32_VEC, 2_int32, "PK_FLOAT32_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_FLOAT64_VEC, 2_int32, "PK_FLOAT64_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_LOGICAL_VEC, 2_int32, "PK_LOGICAL_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_STRING_VEC, 2_int32, "PK_STRING_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_DATE_VEC, 2_int32, "PK_DATE_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_TIME_VEC, 2_int32, "PK_TIME_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_TIMESTAMP_VEC, 2_int32, "PK_TIMESTAMP_VEC")
    end subroutine test_matrix_set_null_all_kinds
    !
    !> One kind's null round-trip: mark row 2 null, and check the column agrees -- while row 1,
    !> which was never touched, does not.
    !> %move_from hands storage over for EVERY kind, leaving the source empty.
    !!
    !! The component list inside `move_from` is written out by hand, exactly as `clear`'s is, so
    !! the failure this guards against is a kind whose storage array was forgotten there: the move
    !! would silently drop that array and the destination would come back with a kind and a row
    !! count but no values. Sweeping all 18 kinds is what makes that a test failure rather than a
    !! future surprise.
    subroutine test_matrix_move_from_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        !
        call move_sweep(error, PK_INT32, 1_int32, "PK_INT32")
        if (allocated(error)) return
        call move_sweep(error, PK_INT64, 1_int32, "PK_INT64")
        if (allocated(error)) return
        call move_sweep(error, PK_FLOAT32, 1_int32, "PK_FLOAT32")
        if (allocated(error)) return
        call move_sweep(error, PK_FLOAT64, 1_int32, "PK_FLOAT64")
        if (allocated(error)) return
        call move_sweep(error, PK_LOGICAL, 1_int32, "PK_LOGICAL")
        if (allocated(error)) return
        call move_sweep(error, PK_STRING, 1_int32, "PK_STRING")
        if (allocated(error)) return
        call move_sweep(error, PK_DATE, 1_int32, "PK_DATE")
        if (allocated(error)) return
        call move_sweep(error, PK_TIME, 1_int32, "PK_TIME")
        if (allocated(error)) return
        call move_sweep(error, PK_TIMESTAMP, 1_int32, "PK_TIMESTAMP")
        if (allocated(error)) return
        call move_sweep(error, PK_INT32_VEC, 2_int32, "PK_INT32_VEC")
        if (allocated(error)) return
        call move_sweep(error, PK_INT64_VEC, 2_int32, "PK_INT64_VEC")
        if (allocated(error)) return
        call move_sweep(error, PK_FLOAT32_VEC, 2_int32, "PK_FLOAT32_VEC")
        if (allocated(error)) return
        call move_sweep(error, PK_FLOAT64_VEC, 2_int32, "PK_FLOAT64_VEC")
        if (allocated(error)) return
        call move_sweep(error, PK_LOGICAL_VEC, 2_int32, "PK_LOGICAL_VEC")
        if (allocated(error)) return
        call move_sweep(error, PK_STRING_VEC, 2_int32, "PK_STRING_VEC")
        if (allocated(error)) return
        call move_sweep(error, PK_DATE_VEC, 2_int32, "PK_DATE_VEC")
        if (allocated(error)) return
        call move_sweep(error, PK_TIME_VEC, 2_int32, "PK_TIME_VEC")
        if (allocated(error)) return
        call move_sweep(error, PK_TIMESTAMP_VEC, 2_int32, "PK_TIMESTAMP_VEC")
    end subroutine test_matrix_move_from_all_kinds
    !
    !> One kind's move: the destination ends up with everything, the source with nothing.
    subroutine move_sweep(error, kind, width, label)
        type(error_type), allocatable, intent(inout) :: error
        integer, intent(in) :: kind
        integer(int32), intent(in) :: width
        character(len=*), intent(in) :: label
        type(parquet_column) :: src, dst
        character(len=:), allocatable :: u
        !
        call src%init(kind, 3_int64, width=width, unit="widget")
        call src%set_null(2_int64)
        ! The destination starts out holding something else, which the move must release.
        call dst%init(PK_INT32, 7_int64)
        call dst%move_from(src)
        call check(error, dst%kindof() == kind, label//": move_from should carry the kind over")
        if (allocated(error)) return
        call check(error, dst%length() == 3_int64, label//": move_from should carry the row count over")
        if (allocated(error)) return
        call check(error, dst%colwidth() == width, label//": move_from should carry the width over")
        if (allocated(error)) return
        call check(error, dst%is_null(2_int64), label//": move_from should carry the validity over")
        if (allocated(error)) return
        call dst%unit_string(u)
        call check(error, u == "widget", label//": move_from should carry the unit over")
        if (allocated(error)) return
        ! ...and the source is left as though it had just been declared -- NOT still claiming a
        ! kind and a row count for storage it no longer owns.
        call check(error, src%kindof() == PK_NONE, label//": move_from should leave the source empty")
        if (allocated(error)) return
        call check(error, src%length() == 0_int64, label//": move_from should leave the source with no rows")
    end subroutine move_sweep
    !
    subroutine null_sweep(error, kind, width, label)
        type(error_type), allocatable, intent(inout) :: error
        integer, intent(in) :: kind
        integer(int32), intent(in) :: width
        character(len=*), intent(in) :: label
        type(parquet_column) :: c
        !
        call c%init(kind, 3_int64, width=width)
        call c%set_null(2_int64)
        call check(error, c%is_null(2_int64), label//": the row passed to set_null should read back null")
        if (allocated(error)) return
        call check(error, c%any_null(), label//": any_null should be .true. after set_null")
        if (allocated(error)) return
        call check(error, c%length() == 3_int64, label//": set_null must not change the row count")
    end subroutine null_sweep
    !
    subroutine test_string_vector_set_at(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=6) :: row(2)
        !
        call c%init(PK_STRING_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape(["a  ", "bcd", "ef ", "gh "], [2, 2]))
        call c%set_at(1_int64, ["xy   ", "zzzzz"])
        call c%get_at(1_int64, row)
        call check(error, trim(row(1)) == "xy" .and. trim(row(2)) == "zzzzz", &
            "set_at on a string vector row should replace both of its elements")
        if (allocated(error)) return
        call c%set_null(2_int64)
        call c%set_at(2_int64, ["p", "q"], modify_nulls=.false.)
        call check(error, c%is_null(2_int64), &
            "set_at(modify_nulls=.false.) must leave a null string vector row null")
        if (allocated(error)) return
        call c%set_all(reshape(["m", "n", "o", "p"], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(2_int64), &
            "set_all(modify_nulls=.false.) must leave a null string vector row null")
        if (allocated(error)) return
        call c%append_values(reshape(["r ", "st"], [2, 1]))
        call check(error, c%length() == 3_int64, "append_values should add a string vector row")
        if (allocated(error)) return
        call c%get_at(3_int64, row)
        call check(error, trim(row(2)) == "st", "the appended string vector row should round-trip")
    end subroutine test_string_vector_set_at
    !
    subroutine test_clear_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_null(2_int64)
        call c%clear_null(2_int64)
        call check(error, .not. c%is_null(2_int64), "clear_null should mark a bitmap-backed row valid again")
        if (allocated(error)) return
        call check(error, c%validity_bytes() > 0_int64, &
            "clear_null must not release the bitmap on its own (that is compact_validity's job)")
        if (allocated(error)) return
        call c%compact_validity()
        call check(error, c%validity_bytes() == 0_int64, "compact_validity should then release it")
        if (allocated(error)) return
        !
        call c%init(PK_INT32, 2_int64)
        call c%clear_null(1_int64)
        call check(error, .not. c%is_null(1_int64), "clear_null on a bitmap-free column should be a no-op")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 2_int64)
        call c%set_all(["a  ", "bcd"])
        call c%set_null(1_int64)
        call c%clear_null(1_int64)
        call check(error, .not. c%is_null(1_int64), "clear_null should clear a string element's null state")
        if (allocated(error)) return
        !
        call c%init(PK_STRING_VEC, 2_int64, width=2_int32)
        call c%set_null(1_int64)
        call c%clear_null(1_int64)
        call check(error, .not. c%is_null(1_int64), "clear_null should clear a string vector row's null state")
    end subroutine test_clear_null
    !
    !> Every discriminator must map to a name -- a missing arm would silently produce
    !> "PK_UNKNOWN" inside an error message, exactly when the message matters most.
    subroutine test_kind_names_complete(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: kinds(22), k
        character(len=:), allocatable :: nm
        !
        kinds = [PK_NONE, PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, &
                 PK_DATE, PK_TIME, PK_TIMESTAMP, PK_INT32_VEC, PK_INT64_VEC, PK_FLOAT32_VEC, &
                 PK_FLOAT64_VEC, PK_LOGICAL_VEC, PK_STRING_VEC, PK_DATE_VEC, PK_TIME_VEC, &
                 PK_TIMESTAMP_VEC, PK_LIST, PK_MAP, PK_STRUCT]
        do k = 1, size(kinds)
            call parquet_kind_name(kinds(k), nm)
            call check(error, len(nm) > 0 .and. nm /= "PK_UNKNOWN", &
                "every PK_* constant should have its own name in parquet_kind_name")
            if (allocated(error)) return
        end do
        call parquet_kind_name(-1, nm)
        call check(error, nm == "PK_UNKNOWN", "an unrecognized discriminator should report PK_UNKNOWN")
    end subroutine test_kind_names_complete
    !
    !> R3/M8's `modify_nulls=` guard, swept over every kind: with `.false.` a null row keeps both
    !> its null state and its stored value, and with the default a write clears the row's null
    !> bit (RF9). Both branches are generated per kind, so this is the sweep that proves the
    !> template is right for all of them rather than for the two spot-checked above.
    subroutine test_matrix_modify_nulls_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_date) :: d(2)
        type(parquet_time) :: t(2)
        type(parquet_timestamp) :: ts(2)
        integer :: i
        !
        call c%init(PK_INT32, 2_int64)
        call c%set_all([1_int32, 2_int32])
        call c%set_null(1_int64)
        call c%set_at(1_int64, 9_int32, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT32: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([1_int32, 2_int32], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT32: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, 9_int32)
        call check(error, .not. c%is_null(1_int64), "PK_INT32: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_INT64, 2_int64)
        call c%set_all([1_int64, 2_int64])
        call c%set_null(1_int64)
        call c%set_at(1_int64, 9_int64, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT64: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([1_int64, 2_int64], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT64: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, 9_int64)
        call check(error, .not. c%is_null(1_int64), "PK_INT64: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT32, 2_int64)
        call c%set_all([1.0_real32, 2.0_real32])
        call c%set_null(1_int64)
        call c%set_at(1_int64, 9.0_real32, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT32: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([1.0_real32, 2.0_real32], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT32: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, 9.0_real32)
        call check(error, .not. c%is_null(1_int64), "PK_FLOAT32: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64, 2_int64)
        call c%set_all([1.0_real64, 2.0_real64])
        call c%set_null(1_int64)
        call c%set_at(1_int64, 9.0_real64, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT64: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([1.0_real64, 2.0_real64], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT64: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, 9.0_real64)
        call check(error, .not. c%is_null(1_int64), "PK_FLOAT64: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_LOGICAL, 2_int64)
        call c%set_all([.true., .false.])
        call c%set_null(1_int64)
        call c%set_at(1_int64, .true., modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_LOGICAL: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([.true., .false.], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_LOGICAL: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, .true.)
        call check(error, .not. c%is_null(1_int64), "PK_LOGICAL: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_INT32_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [9_int32, 9_int32], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT32_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT32_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [9_int32, 9_int32])
        call check(error, .not. c%is_null(1_int64), &
            "PK_INT32_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        call c%init(PK_INT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [9_int64, 9_int64], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT64_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT64_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [9_int64, 9_int64])
        call check(error, .not. c%is_null(1_int64), &
            "PK_INT64_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT32_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [9.0_real32, 9.0_real32], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT32_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT32_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [9.0_real32, 9.0_real32])
        call check(error, .not. c%is_null(1_int64), &
            "PK_FLOAT32_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [9.0_real64, 9.0_real64], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT64_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT64_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [9.0_real64, 9.0_real64])
        call check(error, .not. c%is_null(1_int64), &
            "PK_FLOAT64_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        call c%init(PK_LOGICAL_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([.true., .false., .true., .true.], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [.true., .true.], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_LOGICAL_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([.true., .false., .true., .true.], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_LOGICAL_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [.true., .true.])
        call check(error, .not. c%is_null(1_int64), &
            "PK_LOGICAL_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        do i = 1, 2
            call d(i)%set(2026, 7, 20 + i)
            call t(i)%set(i, 0, 0)
            call ts(i)%set(2026, 7, 20 + i, 6, 0, 0)
        end do
        call c%init(PK_DATE, 2_int64)
        call c%set_all(d)
        call c%set_null(1_int64)
        call c%set_at(1_int64, d(2), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_DATE: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(d, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_DATE: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        !
        call c%init(PK_TIME, 2_int64)
        call c%set_all(t)
        call c%set_null(1_int64)
        call c%set_all(t, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_TIME: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, t(2), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_TIME: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, t(2))
        call check(error, .not. c%is_null(1_int64), "PK_TIME: a default set_at should overwrite a null element")
        if (allocated(error)) return
        !
        call c%init(PK_TIMESTAMP, 2_int64)
        call c%set_all(ts)
        call c%set_null(1_int64)
        call c%set_all(ts, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIMESTAMP: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, ts(2), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIMESTAMP: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, ts(2))
        call check(error, .not. c%is_null(1_int64), &
            "PK_TIMESTAMP: a default set_at should overwrite a null element")
        if (allocated(error)) return
        !
        call c%init(PK_DATE_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([d(1), d(2), d(1), d(2)], [2, 2]))
        call c%set_null(1_int64)
        call c%set_all(reshape([d(1), d(2), d(1), d(2)], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_DATE_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [d(1), d(2)], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_DATE_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        !
        call c%init(PK_TIME_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([t(1), t(2), t(1), t(2)], [2, 2]))
        call c%set_null(1_int64)
        call c%set_all(reshape([t(1), t(2), t(1), t(2)], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIME_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [t(1), t(2)], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIME_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        !
        call c%init(PK_TIMESTAMP_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([ts(1), ts(2), ts(1), ts(2)], [2, 2]))
        call c%set_null(1_int64)
        call c%set_all(reshape([ts(1), ts(2), ts(1), ts(2)], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIMESTAMP_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [ts(1), ts(2)], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIMESTAMP_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 2_int64)
        call c%set_all(["a  ", "bcd"])
        call c%set_null(1_int64)
        call c%set_all(["xy ", "zzz"], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_STRING: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, "nope", modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_STRING: set_at(modify_nulls=.false.) must leave a null row null")
    end subroutine test_matrix_modify_nulls_all_kinds
    !
    !> One null ELEMENT must be addressable, and must not spread to its siblings, on a
    !> representative of each of the three storage classes (bitmap, string, temporal).
    !>
    !> This is the test the whole stage exists for: before element-granular validity, nulling one
    !> element of a row was simply not expressible, and reading one back reported the row.
    subroutine test_element_nulls_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_date) :: dv(3, 2)
        integer :: i, j
        !
        ! Bitmap class.
        call c%init(PK_FLOAT64_VEC, 3_int64, width=4_int32)
        call c%set_null(2_int64, 3_int64)
        call check(error, c%is_null(2_int64, 3_int64), "f64v: the nulled element must read back null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 1_int64), "f64v: element 1 of the same row must stay valid")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 4_int64), "f64v: element 4 of the same row must stay valid")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64, 3_int64), "f64v: the same element of another row must stay valid")
        if (allocated(error)) return
        call c%clear_null(2_int64, 3_int64)
        call check(error, .not. c%is_null(2_int64, 3_int64), "f64v: clear_null(i, e) must undo set_null(i, e)")
        if (allocated(error)) return
        !
        ! String class. Values must be written first: a freshly initialized string column starts
        ! out all-null (append_nulls on the embedded store), so nulling one element of a pristine
        ! one would prove nothing about its siblings.
        call c%init(PK_STRING_VEC, 2_int64, width=3_int32)
        call c%set_all(reshape(["aa", "bb", "cc", "dd", "ee", "ff"], [3, 2]))
        call c%set_null(1_int64, 2_int64)
        call check(error, c%is_null(1_int64, 2_int64), "strv: the nulled element must read back null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64, 1_int64), "strv: element 1 of the same row must stay valid")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64, 3_int64), "strv: element 3 of the same row must stay valid")
        if (allocated(error)) return
        !
        ! Temporal class. Values first, for the same reason as the string case above: a
        ! default-initialized parquet_date IS null, so a pristine column is entirely null.
        call c%init(PK_DATE_VEC, 2_int64, width=3_int32)
        do i = 1, 3
            do j = 1, 2
                call dv(i, j)%set(2026, 7, 10*j + i)
            end do
        end do
        call c%set_all(dv)
        call c%set_null(2_int64, 1_int64)
        call check(error, c%is_null(2_int64, 1_int64), "datev: the nulled element must read back null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 2_int64), "datev: element 2 of the same row must stay valid")
        if (allocated(error)) return
        !
        ! A scalar column defines the element form too, with e == 1 meaning the row.
        call c%init(PK_INT32, 2_int64)
        call c%set_null(1_int64, 1_int64)
        call check(error, c%is_null(1_int64), "scalar: set_null(i, 1) must be the same as set_null(i)")
        if (allocated(error)) return
        call check(error, c%is_null(1_int64, 1_int64), "scalar: is_null(i, 1) must agree with is_null(i)")
    end subroutine test_element_nulls_all_kinds
    !
    !> The row-level QUERY means "any element of the row is null" -- not "its first element is",
    !> which is what it meant before this stage and which a null in element 3 would hide.
    subroutine test_row_query_is_any_element(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        logical, allocatable :: rowmask(:)
        !
        call c%init(PK_INT32_VEC, 3_int64, width=4_int32)
        ! Deliberately NOT element 1: the old first-element convention answered .false. here.
        call c%set_null(2_int64, 3_int64)
        call check(error, c%is_null(2_int64), "is_null(i) must be .true. when any element of row i is null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64), "is_null(i) must stay .false. for a row with no null element")
        if (allocated(error)) return
        call check(error, c%any_null(), "any_null must see an element-only null")
        if (allocated(error)) return
        !
        call c%row_validity(rowmask)
        call check(error, allocated(rowmask), "row_validity must produce a mask when an element is null")
        if (allocated(error)) return
        call check(error, .not. rowmask(2), "row_validity must mark row 2 invalid from its element-3 null")
        if (allocated(error)) return
        call check(error, rowmask(1) .and. rowmask(3), "row_validity must leave the untouched rows valid")
        if (allocated(error)) return
        !
        ! A whole-row set_null still nulls every element -- the mutation stays whole-row even
        ! though the query became "any".
        call c%init(PK_INT32_VEC, 2_int64, width=3_int32)
        call c%set_null(1_int64)
        call check(error, c%is_null(1_int64, 1_int64) .and. c%is_null(1_int64, 2_int64) .and. &
            c%is_null(1_int64, 3_int64), "set_null(i) must still null every element of the row")
    end subroutine test_row_query_is_any_element
    !
    !> The TEMPORAL arms of row_validity/element_validity/set_validity, which nothing else reaches.
    !!
    !! These kinds keep their null state in the element rather than in the column's bitmap, so they
    !! take a separate branch from every other kind -- and that branch is now specialised per kind
    !! (six arms, scalar and vector) rather than routed through the generic per-element `%is_null`.
    !! Six near-identical arms is exactly the shape where one gets the wrong component or the wrong
    !! index order and nothing notices, so this asserts each one's actual answer rather than only
    !! that a mask came back.
    !!
    !! The vector case is the load-bearing half: a row is null when ANY of its elements is, so a row
    !! with exactly one null element must be marked invalid by `row_validity` while `element_validity`
    !! marks only that element. Substituting `all` for `any` leaves every scalar assertion green.
    subroutine test_temporal_validity_masks(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_date) :: d(3), dv(2, 3)
        type(parquet_time) :: tm(3)
        type(parquet_timestamp) :: ts(3), tsv(2, 3)
        logical, allocatable :: rowmask(:), emask(:,:)
        integer :: i, e
        !
        ! ---- scalar date: row 2 null ----
        do i = 1, 3
            call d(i)%set(2026, 7, 20 + i)
        end do
        call c%init(PK_DATE, 3_int64)
        call c%set_all(d)
        call c%set_null(2_int64)
        call c%row_validity(rowmask)
        call check(error, allocated(rowmask), "date: row_validity must allocate once a null exists")
        if (allocated(error)) return
        call check(error, rowmask(1) .and. .not. rowmask(2) .and. rowmask(3), &
            "date: row_validity must mark exactly the nulled row")
        if (allocated(error)) return
        call c%element_validity(emask)
        call check(error, size(emask, 1) == 1 .and. size(emask, 2) == 3, &
            "date: element_validity must be shaped (1, nrows) for a scalar column")
        if (allocated(error)) return
        call check(error, emask(1, 1) .and. .not. emask(1, 2) .and. emask(1, 3), &
            "date: element_validity must mark exactly the nulled row's only element")
        if (allocated(error)) return
        !
        ! ---- scalar time: row 3 null, so a wrong arm cannot pass by reusing date's answer ----
        do i = 1, 3
            call tm(i)%set(i, 2*i, 3*i)
        end do
        call c%init(PK_TIME, 3_int64)
        call c%set_all(tm)
        call c%set_null(3_int64)
        call c%row_validity(rowmask)
        call check(error, rowmask(1) .and. rowmask(2) .and. .not. rowmask(3), &
            "time: row_validity must mark exactly the nulled row")
        if (allocated(error)) return
        !
        ! ---- scalar timestamp: row 1 null ----
        do i = 1, 3
            call ts(i)%set(2026, 1, i, 3, 4, 5)
        end do
        call c%init(PK_TIMESTAMP, 3_int64)
        call c%set_all(ts)
        call c%set_null(1_int64)
        call c%row_validity(rowmask)
        call check(error, .not. rowmask(1) .and. rowmask(2) .and. rowmask(3), &
            "timestamp: row_validity must mark exactly the nulled row")
        if (allocated(error)) return
        !
        ! ---- vector date, width 2: ONE element of row 2 null ----
        do i = 1, 3
            do e = 1, 2
                call dv(e, i)%set(2026, 7, 10 + i + e)
            end do
        end do
        call c%init(PK_DATE_VEC, 3_int64, width=2_int32)
        call c%set_all(dv)
        call c%set_null(2_int64, 1_int64)
        call c%row_validity(rowmask)
        call check(error, rowmask(1) .and. .not. rowmask(2) .and. rowmask(3), &
            "date_vec: a row with ANY null element must be marked invalid (any, not all)")
        if (allocated(error)) return
        call c%element_validity(emask)
        call check(error, size(emask, 1) == 2 .and. size(emask, 2) == 3, &
            "date_vec: element_validity must be shaped (width, nrows)")
        if (allocated(error)) return
        call check(error, .not. emask(1, 2) .and. emask(2, 2), &
            "date_vec: element_validity must mark ONLY the nulled element of that row")
        if (allocated(error)) return
        call check(error, all(emask(:, 1)) .and. all(emask(:, 3)), &
            "date_vec: element_validity must leave untouched rows entirely valid")
        if (allocated(error)) return
        !
        ! ---- vector timestamp, width 2: the SECOND element of row 3 null, so an (e,i)/(i,e)
        !      index swap in the arm cannot pass by symmetry ----
        do i = 1, 3
            do e = 1, 2
                call tsv(e, i)%set(2026, 2, i, e, 4, 5)
            end do
        end do
        call c%init(PK_TIMESTAMP_VEC, 3_int64, width=2_int32)
        call c%set_all(tsv)
        call c%set_null(3_int64, 2_int64)
        call c%row_validity(rowmask)
        call check(error, rowmask(1) .and. rowmask(2) .and. .not. rowmask(3), &
            "timestamp_vec: a row with ANY null element must be marked invalid")
        if (allocated(error)) return
        call c%element_validity(emask)
        call check(error, emask(1, 3) .and. .not. emask(2, 3), &
            "timestamp_vec: element_validity must mark ONLY element 2 of row 3")
        if (allocated(error)) return
        !
        ! ---- set_validity's temporal arm: writing a mask back must reproduce it ----
        call c%init(PK_TIMESTAMP_VEC, 3_int64, width=2_int32)
        call c%set_all(tsv)
        if (allocated(emask)) deallocate(emask)
        allocate(emask(2, 3))
        emask = .true.
        emask(2, 1) = .false.
        emask(1, 3) = .false.
        call c%set_validity(emask)
        deallocate(emask)
        call c%element_validity(emask)
        call check(error, emask(1, 1) .and. .not. emask(2, 1), &
            "timestamp_vec: set_validity must null element 2 of row 1 and leave element 1 valid")
        if (allocated(error)) return
        call check(error, .not. emask(1, 3) .and. emask(2, 3), &
            "timestamp_vec: set_validity must null element 1 of row 3 and leave element 2 valid")
        if (allocated(error)) return
        call check(error, all(emask(:, 2)), "timestamp_vec: set_validity must leave row 2 untouched")
    end subroutine test_temporal_validity_masks
    !
    !> element_validity reports the per-element truth, where row_validity summarises it.
    subroutine test_element_validity(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        logical, allocatable :: emask(:,:)
        !
        call c%init(PK_FLOAT64_VEC, 3_int64, width=2_int32)
        call c%element_validity(emask)
        call check(error, .not. allocated(emask), &
            "element_validity must leave the mask unallocated for a null-free column")
        if (allocated(error)) return
        !
        call c%set_null(2_int64, 2_int64)
        call c%element_validity(emask)
        call check(error, allocated(emask), "element_validity must allocate once a null exists")
        if (allocated(error)) return
        call check(error, size(emask, 1) == 2 .and. size(emask, 2) == 3, &
            "element_validity must be shaped (width, nrows)")
        if (allocated(error)) return
        call check(error, .not. emask(2, 2), "element_validity must mark exactly the nulled element")
        if (allocated(error)) return
        call check(error, emask(1, 2), "element_validity must leave the row's other element valid")
        if (allocated(error)) return
        call check(error, all(emask(:, 1)) .and. all(emask(:, 3)), &
            "element_validity must leave untouched rows entirely valid")
        if (allocated(error)) return
        !
        ! The string class keeps its validity outside the bitmap, so it takes the other branch.
        call c%init(PK_STRING_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape(["aa", "bb", "cc", "dd"], [2, 2]))
        call c%set_null(1_int64, 2_int64)
        call c%element_validity(emask)
        call check(error, allocated(emask), "strv: element_validity must allocate once a null exists")
        if (allocated(error)) return
        call check(error, .not. emask(2, 1) .and. emask(1, 1), &
            "strv: element_validity must mark exactly the nulled element")
    end subroutine test_element_validity
    !
    !> set_validity writes a whole mask in one pass, and only ever ADDS nulls.
    subroutine test_set_validity(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        logical :: m(2, 3)
        logical, allocatable :: emask(:,:)
        !
        call c%init(PK_INT32_VEC, 3_int64, width=2_int32)
        m = .true.
        m(2, 1) = .false.
        m(1, 3) = .false.
        call c%set_validity(m)
        call check(error, c%is_null(1_int64, 2_int64), "set_validity must null element (2,1)")
        if (allocated(error)) return
        call check(error, c%is_null(3_int64, 1_int64), "set_validity must null element (1,3)")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64), "set_validity must leave an all-valid row untouched")
        if (allocated(error)) return
        !
        ! Only ever adds: a .true. entry must not resurrect a null already recorded.
        m = .true.
        call c%set_validity(m)
        call check(error, c%is_null(1_int64, 2_int64), &
            "set_validity must not clear an existing null from a .true. entry")
        if (allocated(error)) return
        !
        ! An all-valid mask on a clean column must not even allocate a bitmap.
        call c%init(PK_INT32_VEC, 3_int64, width=2_int32)
        m = .true.
        call c%set_validity(m)
        call check(error, c%validity_bytes() == 0_int64, &
            "set_validity with an all-valid mask must not allocate a bitmap")
        if (allocated(error)) return
        !
        ! Round trip: element_validity of what set_validity wrote must agree.
        call c%init(PK_INT32_VEC, 3_int64, width=2_int32)
        m = .true.
        m(2, 2) = .false.
        call c%set_validity(m)
        call c%element_validity(emask)
        call check(error, all(emask .eqv. m), "element_validity must round-trip what set_validity wrote")
    end subroutine test_set_validity
    !
    !> modify_nulls=.false. protects individual null ELEMENTS: the row's other elements are
    !> still written, where before this stage one null element vetoed the whole row.
    subroutine test_modify_nulls_is_element_wise(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: got(3)
        character(len=8) :: gots(3)
        !
        call c%init(PK_INT32_VEC, 2_int64, width=3_int32)
        call c%set_at(1_int64, [10_int32, 20_int32, 30_int32])
        call c%set_null(1_int64, 2_int64)
        call c%set_at(1_int64, [11_int32, 22_int32, 33_int32], modify_nulls=.false.)
        call c%get_at(1_int64, got)
        call check(error, got(1) == 11_int32, "set_at(modify_nulls=.false.) must write a valid element")
        if (allocated(error)) return
        call check(error, got(2) == 20_int32, "set_at(modify_nulls=.false.) must skip the null element")
        if (allocated(error)) return
        call check(error, got(3) == 33_int32, "set_at(modify_nulls=.false.) must write past the null element")
        if (allocated(error)) return
        call check(error, c%is_null(1_int64, 2_int64), "the protected element must still be null afterwards")
        if (allocated(error)) return
        !
        ! Same rule for the string class.
        call c%init(PK_STRING_VEC, 1_int64, width=3_int32)
        call c%set_at(1_int64, ["aa      ", "bb      ", "cc      "])
        call c%set_null(1_int64, 2_int64)
        call c%set_at(1_int64, ["xx      ", "yy      ", "zz      "], modify_nulls=.false.)
        call c%get_at(1_int64, gots)
        call check(error, trim(gots(1)) == "xx", "strv: modify_nulls=.false. must write a valid element")
        if (allocated(error)) return
        call check(error, trim(gots(3)) == "zz", "strv: modify_nulls=.false. must write past the null element")
        if (allocated(error)) return
        call check(error, c%is_null(1_int64, 2_int64), "strv: the protected element must still be null")
    end subroutine test_modify_nulls_is_element_wise
    !
    !> A row-structural mutation moves whole rows, so an element null must land on the same
    !> element of the row's new position -- not be widened, dropped or shifted.
    subroutine test_element_nulls_survive_mutation(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c, other
        !
        call c%init(PK_INT32_VEC, 3_int64, width=3_int32)
        call c%set_null(1_int64, 2_int64)
        call c%reindex([3_int64, 2_int64, 1_int64])
        call check(error, c%is_null(3_int64, 2_int64), "reindex must carry an element null to the row's new index")
        if (allocated(error)) return
        call check(error, .not. c%is_null(3_int64, 1_int64), "reindex must not widen an element null to its row")
        if (allocated(error)) return
        !
        call c%init(PK_INT32_VEC, 3_int64, width=3_int32)
        call c%set_null(3_int64, 3_int64)
        call c%delete_by_mask([.true., .false., .true.])
        call check(error, c%is_null(2_int64, 3_int64), "delete_by_mask must carry an element null to the kept row")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 1_int64), "delete_by_mask must not widen an element null")
        if (allocated(error)) return
        !
        call c%init(PK_INT32_VEC, 1_int64, width=3_int32)
        call other%init(PK_INT32_VEC, 1_int64, width=3_int32)
        call other%set_null(1_int64, 2_int64)
        call c%append(other)
        call check(error, c%is_null(2_int64, 2_int64), "append must carry the source's element null across")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 3_int64), "append must not widen the source's element null")
    end subroutine test_element_nulls_survive_mutation
    !
    !> Every character ARRAY entry point trims trailing blanks; the SCALAR one stores verbatim.
    !!
    !! **Asserts the stored LENGTH, not equality.** Fortran blank-pads the shorter operand of `==`,
    !! so a padded store and a trimmed one compare equal on every value -- the length is the only
    !! thing that can tell them apart, which is why this bug survived unnoticed until a benchmark
    !! hit the O(n^2) fill it shares a code path with. `get_at` hands back a deferred-length
    !! allocatable, so `len(s)` IS the stored length.
    !!
    !! The rule being pinned: an array's elements share one declared length, so a shorter value is
    !! padded by Fortran and those blanks mean nothing; a scalar is exactly as long as the caller
    !! wrote it, so trimming would destroy something the caller could express.
    subroutine test_string_array_trims_scalar_does_not(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c, v
        type(parquet_string_column), pointer :: sp
        character(len=:), allocatable :: s
        character(len=4) :: m(2, 2)
        !
        ! First element deliberately the shortest, per CLAUDE.md's fixture rule for this class.
        call c%init(PK_STRING, 3_int64)
        call c%set_all(["a  ", "bc ", "def"])
        call c%get_at(1_int64, s)
        call check(error, len(s) == 1, "set_all must store 'a  ' trimmed to 1 character, not padded")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, len(s) == 2 .and. s == "bc", "set_all must store 'bc ' as exactly 'bc'")
        if (allocated(error)) return
        call c%get_at(3_int64, s)
        call check(error, len(s) == 3, "an element needing the full declared width keeps all of it")
        if (allocated(error)) return
        !
        ! The scalar form is the deliberate exception.
        call c%set_at(1_int64, "zz   ")
        call c%get_at(1_int64, s)
        call check(error, len(s) == 5, "set_at takes a SCALAR, whose trailing blanks are the caller's own")
        if (allocated(error)) return
        !
        ! %append_values shares the rule, so a column filled either way holds the same bytes.
        call c%append_values(["q  ", "rs "])
        call c%get_at(4_int64, s)
        call check(error, len(s) == 1, "append must trim a character array exactly as set_all does")
        if (allocated(error)) return
        !
        ! The vector kind, read through the flat store so the stored length is visible at all --
        ! get_at blank-pads into the caller's fixed-width array and would hide this.
        m(1, 1) = "a"
        m(2, 1) = "bb"
        m(1, 2) = "ccc"
        m(2, 2) = "dddd"
        call v%init(PK_STRING_VEC, 2_int64, width=2_int32)
        call v%set_all(m)
        call v%string_column(sp)
        call sp%get(1_int64, s)
        call check(error, len(s) == 1, "set_all on a vector string column must trim element (1,1) too")
        if (allocated(error)) return
        call sp%get(4_int64, s)
        call check(error, len(s) == 4 .and. s == "dddd", "the widest element of a vector column keeps its full width")
    end subroutine test_string_array_trims_scalar_does_not
    !
    !> `modify_nulls=.false.` must survive the rebuild `set_all` now does: a null element keeps both
    !! its null state and its (empty) content, and every other element is replaced.
    !!
    !! This is the branch the obvious linear rewrite drops silently -- rebuilding the store from the
    !! caller's array alone would overwrite the nulls and no other assertion here would notice.
    subroutine test_string_set_all_keeps_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=:), allocatable :: s
        !
        call c%init(PK_STRING, 3_int64)
        call c%set_all(["aa", "bb", "cc"])
        call c%set_null(2_int64)
        call c%set_all(["xx", "yy", "zz"], modify_nulls=.false.)
        call check(error, c%is_null(2_int64), "modify_nulls=.false. must leave the null element null")
        if (allocated(error)) return
        call c%get_at(1_int64, s)
        call check(error, s == "xx", "modify_nulls=.false. must still replace the non-null elements")
        if (allocated(error)) return
        call c%get_at(3_int64, s)
        call check(error, s == "zz", "the element after a preserved null must come from the new array")
        if (allocated(error)) return
        ! The default replaces everything, nulls included.
        call c%set_all(["pp", "qq", "rr"])
        call check(error, .not. c%is_null(2_int64), "the default modify_nulls=.true. must clear the null")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, s == "qq", "the formerly null element must hold its new value")
    end subroutine test_string_set_all_keeps_nulls
    !
    ! ==================================================================================
    ! Capacity (A.1): geometric growth, reserve, shrink_to_fit
    !
    ! Capacity is deliberately invisible through the value API, so a round-trip test proves
    ! nothing here -- it passes just as happily against a column that reallocates on every
    ! append. Every test below asserts an EFFECT: how many distinct capacities a run visits, or
    ! that a reserve really did remove the reallocations that would otherwise follow it.
    ! ==================================================================================
    !
    !> Reading a column from a file must not over-allocate, and both routes into a sized column
    !! (`init` at a row count, `adopt` of a caller's array) are what the read paths use.
    subroutine test_capacity_starts_exact(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c, v
        integer(int32), allocatable :: a(:)
        !
        call c%init(PK_INT32, 100_int64)
        call check(error, c%capacity() == 100_int64, "init must allocate exactly nrows, not more")
        if (allocated(error)) return
        allocate(a(37))
        a = 7_int32
        call v%adopt(a)
        ! The adopted allocation IS the capacity: leaving it at 0 would break cap >= nrows and
        ! make the next append reallocate a column that already had room.
        call check(error, v%capacity() == 37_int64, "adopt must take the array's size as the capacity")
        if (allocated(error)) return
        call check(error, v%capacity() == v%length(), "an adopted column must start exact-fit")
    end subroutine test_capacity_starts_exact
    !
    !> The point of the whole change: appending one row at a time must visit only a handful of
    !! distinct capacities, not one per row.
    subroutine test_capacity_growth_geometric(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int64) :: k, seen, last
        character(len=32) :: got
        !
        call c%init(PK_INT32, 0_int64)
        seen = 0_int64
        last = -1_int64
        do k = 1_int64, 1000_int64
            call c%append_values([int(k, int32)])
            if (c%capacity() /= last) then
                seen = seen + 1_int64
                last = c%capacity()
            end if
        end do
        call check(error, c%length() == 1000_int64, "every appended row must be present")
        if (allocated(error)) return
        ! 1.5x growth from empty reaches 1000 in about 20 steps; exact-fit would be 1000. The
        ! bound is loose on purpose -- what is being asserted is the change of complexity, not a
        ! particular growth factor.
        write(got, "(I0)") seen
        call check(error, seen <= 40_int64, &
            "1000 single-row appends must reallocate O(log n) times, not once per row; saw " // trim(got))
        if (allocated(error)) return
        call check(error, c%capacity() >= c%length(), "capacity must still cover every row")
    end subroutine test_capacity_growth_geometric
    !
    !> Only `grow_storage` creates slack. Every rebuild allocates exact-fit, which is what hands
    !! the memory back after a filter or a sort without the caller asking -- and what makes
    !! `%shrink_to_fit` a no-op on anything but an appended-to column.
    subroutine test_capacity_rebuild_exact(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        logical :: keep(10)
        integer(int64) :: k
        !
        call c%init(PK_INT32, 0_int64)
        ! Ten rows, not six: growing from empty the capacity runs 1, 2, 3, 4, 6, 9, 13, because
        ! `cap/2` is 0 until the capacity reaches 2. Six appends therefore land exactly on a
        ! capacity of 6 with no slack at all, and the test would be asserting nothing.
        do k = 1_int64, 10_int64
            call c%append_values([int(k, int32)])
        end do
        call check(error, c%capacity() > c%length(), "the appends must have left some slack to release")
        if (allocated(error)) return
        keep = [.true., .false., .true., .false., .true., .false., .true., .false., .true., .false.]
        call c%delete_by_mask(keep)
        call check(error, c%capacity() == c%length(), "delete_by_mask must leave the column exact-fit")
        if (allocated(error)) return
        call c%reindex([3_int64, 1_int64, 2_int64, 5_int64, 4_int64])
        call check(error, c%capacity() == c%length(), "reindex must leave the column exact-fit")
        if (allocated(error)) return
        call c%gather([1_int64, 2_int64])
        call check(error, c%capacity() == c%length(), "gather must leave the column exact-fit")
    end subroutine test_capacity_rebuild_exact
    !
    !> A `capacity() >= n` assertion would pass against a `%reserve` that does nothing, because
    !! the capacity is often already large enough. What actually has to hold is that the appends
    !! which follow perform NO reallocation at all -- so that is what this asserts.
    subroutine test_reserve_prevents_realloc(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int64) :: k, before
        !
        call c%init(PK_INT32, 0_int64)
        call c%reserve(500_int64)
        call check(error, c%capacity() >= 500_int64, "reserve must make room for the rows asked for")
        if (allocated(error)) return
        call check(error, c%length() == 0_int64, "reserve must not change the row count")
        if (allocated(error)) return
        before = c%capacity()
        do k = 1_int64, 500_int64
            call c%append_values([int(k, int32)])
        end do
        call check(error, c%capacity() == before, &
            "the appends a reserve made room for must not reallocate at all")
        if (allocated(error)) return
        call check(error, c%length() == 500_int64, "every reserved row must still be appendable")
        if (allocated(error)) return
        ! Reserving below what is already held changes nothing.
        call c%reserve(10_int64)
        call check(error, c%capacity() == before, "a reserve below the current capacity must be a no-op")
    end subroutine test_reserve_prevents_realloc
    !
    !> `released` is what `parquet_table%compact` uses to decide whether to advance the
    !! generation counter, so it has to be exact rather than optimistic.
    subroutine test_shrink_to_fit(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int64) :: k
        integer(int32) :: v
        logical :: released
        !
        call c%init(PK_INT32, 0_int64)
        do k = 1_int64, 10_int64
            call c%append_values([int(k*3_int64, int32)])
        end do
        call check(error, c%capacity() > c%length(), "ten single-row appends must leave slack")
        if (allocated(error)) return
        call c%shrink_to_fit(released)
        call check(error, released, "shrink_to_fit must report that it released something")
        if (allocated(error)) return
        call check(error, c%capacity() == c%length(), "shrink_to_fit must leave the column exact-fit")
        if (allocated(error)) return
        ! Values survive the reallocation.
        call c%get_at(4_int64, v)
        call check(error, v == 12_int32, "shrink_to_fit must not disturb the values")
        if (allocated(error)) return
        ! A second call has nothing to do and must say so -- this is the no-op %compact relies on.
        call c%shrink_to_fit(released)
        call check(error, .not. released, "a shrink_to_fit with no slack must report released=.false.")
    end subroutine test_shrink_to_fit
    !
    !> Invariant 1. Checked after every operation that touches storage, because a single path
    !! that forgets `cap` leaves a column claiming room it does not have.
    subroutine test_capacity_invariant(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c, other, cp
        integer(int64) :: k
        !
        call c%init(PK_INT32, 4_int64)
        call check(error, c%capacity() >= c%length(), "after init")
        if (allocated(error)) return
        call c%append_nulls(3_int64)
        call check(error, c%capacity() >= c%length(), "after append_nulls")
        if (allocated(error)) return
        call other%init(PK_INT32, 5_int64)
        call c%append(other)
        call check(error, c%capacity() >= c%length(), "after append")
        if (allocated(error)) return
        call c%append_row_of(other, 2_int64)
        call check(error, c%capacity() >= c%length(), "after append_row_of")
        if (allocated(error)) return
        call c%deep_copy(cp)
        call check(error, cp%capacity() >= cp%length(), "after deep_copy (destination)")
        if (allocated(error)) return
        ! A copy is a fresh object and the caller has not asked for headroom, so a clone compacts
        ! implicitly rather than inheriting the source's slack.
        call check(error, cp%capacity() == cp%length(), "deep_copy must allocate exact-fit")
        if (allocated(error)) return
        call cp%move_from(c)
        call check(error, cp%capacity() >= cp%length(), "after move_from (destination)")
        if (allocated(error)) return
        call check(error, c%capacity() == 0_int64, "move_from must reset the source's capacity")
        if (allocated(error)) return
        call cp%clear()
        call check(error, cp%capacity() == 0_int64, "clear must reset the capacity to 0")
        if (allocated(error)) return
        ! And the string kinds, whose capacity comes from the embedded store rather than `cap`.
        call other%clear()
        call other%init(PK_STRING, 0_int64)
        do k = 1_int64, 20_int64
            call other%append_values(["ab"])
        end do
        call check(error, other%capacity() >= other%length(), "a string column's capacity must cover its rows")
    end subroutine test_capacity_invariant
    !
    !> Invariant 3. The slack must never reach a reader: every storage read is bounded by the row
    !! count, so an over-allocated column looks exactly like an exact-fit one.
    subroutine test_capacity_invisible(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32), pointer :: p(:)
        logical, allocatable :: valid(:)
        integer(int64) :: k
        !
        call c%init(PK_INT32, 0_int64)
        do k = 1_int64, 5_int64
            call c%append_values([int(k, int32)])
        end do
        call check(error, c%capacity() > c%length(), "the setup must actually leave slack to hide")
        if (allocated(error)) return
        call c%data_ptr(p)
        call check(error, size(p, kind=int64) == 5_int64, "data_ptr must expose the rows, not the capacity")
        if (allocated(error)) return
        call check(error, all(p == [1_int32, 2_int32, 3_int32, 4_int32, 5_int32]), &
            "the rows behind the pointer must be the ones appended")
        if (allocated(error)) return
        call c%set_null(3_int64)
        call c%row_validity(valid)
        call check(error, size(valid, kind=int64) == 5_int64, &
            "row_validity must be sized by the row count, not the capacity")
        if (allocated(error)) return
        call check(error, .not. valid(3), "the null row must read back as null")
    end subroutine test_capacity_invisible
    !
    !> The bitmap is sized from the CAPACITY, not the row count. Sizing it from `nrows` would
    !! reallocate it on every append -- reintroducing the quadratic behaviour on any column that
    !! happens to carry a null, with every value still correct and every other test still passing.
    subroutine test_capacity_with_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int64) :: k, seen, last
        logical, allocatable :: valid(:)
        !
        call c%init(PK_INT32, 0_int64)
        seen = 0_int64
        last = -1_int64
        do k = 1_int64, 400_int64
            call c%append_values([int(k, int32)])
            ! Every third row null, so the bitmap exists from the very first rows and has to grow
            ! alongside the storage rather than on its own schedule.
            if (mod(k, 3_int64) == 0_int64) call c%set_null(k)
            if (c%capacity() /= last) then
                seen = seen + 1_int64
                last = c%capacity()
            end if
        end do
        call check(error, seen <= 40_int64, &
            "a null-carrying column must grow geometrically too, not once per append")
        if (allocated(error)) return
        ! The bitmap must cover the CAPACITY, not merely the rows. This is the assertion that
        ! catches a bitmap sized from `nrows`: the growth sequence puts 400 rows at a capacity of
        ! 474, and those two fall in different 64-bit blocks -- so a bitmap sized from the row
        ! count is measurably too small here, while every value and every null bit still reads
        ! back correctly. Without this the mutation survives the whole suite.
        call check(error, c%validity_bytes()*8_int64 >= c%capacity()*int(c%colwidth(), int64), &
            "the validity bitmap must be sized from the capacity, not the row count")
        if (allocated(error)) return
        ! Every null bit must have survived every one of those reallocations.
        call c%row_validity(valid)
        call check(error, size(valid, kind=int64) == 400_int64, "the mask must cover every row")
        if (allocated(error)) return
        do k = 1_int64, 400_int64
            if (mod(k, 3_int64) == 0_int64) then
                if (valid(k)) then
                    call check(error, .false., "a row set null must still read back null after growth")
                    return
                end if
            else
                if (.not. valid(k)) then
                    call check(error, .false., "a row never nulled must not become null through growth")
                    return
                end if
            end if
        end do
        call check(error, .true., "null bits survive geometric growth")
    end subroutine test_capacity_with_nulls
    !
    !> `append_row_of` is the primitive that lets `%append(row)` copy one row instead of the whole
    !! source column. Validity has to come with it, or a null row would append as a valid one.
    subroutine test_append_row_of(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: src, dst
        integer(int32) :: v
        integer(int32) :: vec(3)
        type(parquet_column) :: vsrc, vdst
        character(len=:), allocatable :: s
        type(parquet_column) :: ssrc, sdst
        !
        call src%init(PK_INT32, 4_int64)
        call src%set_all([10_int32, 20_int32, 30_int32, 40_int32])
        call src%set_null(3_int64)
        call dst%init(PK_INT32, 0_int64)
        call dst%append_row_of(src, 2_int64)
        call dst%append_row_of(src, 3_int64)
        call check(error, dst%length() == 2_int64, "each append_row_of must add exactly one row")
        if (allocated(error)) return
        call dst%get_at(1_int64, v)
        call check(error, v == 20_int32, "the appended row must hold the source row's value")
        if (allocated(error)) return
        call check(error, .not. dst%is_null(1_int64), "a valid source row must append as valid")
        if (allocated(error)) return
        call check(error, dst%is_null(2_int64), "a null source row must append as null")
        if (allocated(error)) return
        ! The source is untouched.
        call check(error, src%length() == 4_int64, "append_row_of must not change the source")
        if (allocated(error)) return
        ! A vector kind copies the whole row's element vector.
        call vsrc%init(PK_INT32_VEC, 2_int64, 3_int32)
        call vsrc%set_at(2_int64, [7_int32, 8_int32, 9_int32])
        call vdst%init(PK_INT32_VEC, 0_int64, 3_int32)
        call vdst%append_row_of(vsrc, 2_int64)
        call vdst%get_at(1_int64, vec)
        call check(error, all(vec == [7_int32, 8_int32, 9_int32]), &
            "a vector row must append every element of the row")
        if (allocated(error)) return
        ! And a string kind, which goes through the embedded store rather than an array.
        call ssrc%init(PK_STRING, 3_int64)
        call ssrc%set_all(["a  ", "bb ", "ccc"])
        call sdst%init(PK_STRING, 0_int64)
        call sdst%append_row_of(ssrc, 3_int64)
        call sdst%append_row_of(ssrc, 1_int64)
        call sdst%get_at(1_int64, s)
        call check(error, s == "ccc", "a string row must append its own bytes")
        if (allocated(error)) return
        call sdst%get_at(2_int64, s)
        call check(error, s == "a", "the second appended string row must be the one named")
    end subroutine test_append_row_of
    !
    !> The bulk validity paths (A.8) write the null bitmap a 64-bit WORD at a time instead of a bit
    !! at a time, which makes the boundaries the thing to test: a run that starts or ends part-way
    !! through a word, a run that lies entirely inside one word, and a run that spans several.
    !!
    !! Widths 1, 3, 8, 16, 64 and 65 are chosen so that some DO divide 64 and some do not — an
    !! implementation that only handles whole blocks, or that assumes a row never straddles a word,
    !! passes every width-64 case and fails the others. Row counts are picked to put the operations
    !! across a block boundary rather than inside one. Every assertion compares against a mask this
    !! test computes itself, never against a second call into the same machinery.
    subroutine test_bulk_validity_widths(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        integer, parameter :: WIDTHS(6) = [1, 3, 8, 16, 64, 65]
        integer :: iw, w
        !
        do iw = 1, size(WIDTHS)
            w = WIDTHS(iw)
            call one_width(error, w)
            if (allocated(error)) return
        end do
    end subroutine test_bulk_validity_widths
    !
    !> One width's worth of `test_bulk_validity_widths`.
    subroutine one_width(error, w)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        integer, intent(in) :: w                            !! elements per row.
        type(parquet_column) :: c, other
        integer(int32), allocatable :: vals(:,:)
        logical, allocatable :: want(:,:), got(:,:)
        integer(int64) :: n, i, e
        character(len=16) :: ws
        !
        write(ws, "(A,I0)") "width=", w
        n = 100_int64                       ! 100*w elements: several words for every width here
        allocate(vals(w, n))
        do i = 1_int64, n
            do e = 1_int64, int(w, int64)
                vals(e, i) = int(i*100_int64 + e, int32)
            end do
        end do
        ! Width 1 is a SCALAR column, not a one-wide vector one -- `init` rejects a vector kind of
        ! width 1 outright. It still matters most here, being what almost every real column is, so
        ! it is swept alongside the rest and `fill` picks the kind.
        ! ---- 1. append_nulls: a contiguous run whose ends are ragged for most widths ----
        call c%clear()
        call fill(c, vals, n, w)
        call c%append_nulls(7_int64)
        allocate(want(w, n + 7_int64))
        want = .true.
        want(:, n + 1_int64:) = .false.
        call expect_mask(error, c, want, trim(ws)//" append_nulls marks exactly the appended rows")
        if (allocated(error)) return
        deallocate(want)
        ! ---- 2. set_validity, per ELEMENT: first and last rows null, plus a block-boundary row ----
        call c%clear()
        call fill(c, vals, n, w)
        allocate(want(w, n))
        want = .true.
        want(1, 1) = .false.                            ! first element of the first row
        want(w, n) = .false.                            ! last element of the last row
        want(:, 64_int64/max(int(w, int64), 1_int64) + 1_int64) = .false.  ! astride a word boundary
        call c%set_validity(want)
        call expect_mask(error, c, want, trim(ws)//" set_validity writes exactly the element mask")
        if (allocated(error)) return
        ! ---- 3. set_validity, per ROW: whole rows, including the first and the last ----
        call c%clear()
        call fill(c, vals, n, w)
        block
            logical, allocatable :: rowmask(:)
            allocate(rowmask(n))
            rowmask = .true.
            rowmask(1) = .false.
            rowmask(n) = .false.
            rowmask(65) = .false.
            call c%set_validity(rowmask)
            want = .true.
            do i = 1_int64, n
                if (.not. rowmask(i)) want(:, i) = .false.
            end do
        end block
        call expect_mask(error, c, want, trim(ws)//" a per-ROW mask nulls every element of its rows")
        if (allocated(error)) return
        ! ---- 4. append: the source's nulls land at the right offset in the destination ----
        call other%clear()
        call fill(other, vals, n, w)
        call other%set_null(1_int64)
        call other%set_null(n)
        call other%set_null(33_int64, 1_int64)
        call c%clear()
        call fill(c, vals, n, w)
        call c%set_null(2_int64)
        call c%append(other)
        deallocate(want)
        allocate(want(w, 2_int64*n))
        want = .true.
        want(:, 2) = .false.                            ! the destination's own null survives
        want(:, n + 1_int64) = .false.                  ! other's row 1
        want(:, 2_int64*n) = .false.                    ! other's last row
        want(1, n + 33_int64) = .false.                 ! other's single null element
        call expect_mask(error, c, want, trim(ws)//" append places the source's nulls at the right offset")
        if (allocated(error)) return
        ! ---- 5. paste REPLACES the pasted range's validity, it does not merge into it ----
        call c%clear()
        call fill(c, vals, n, w)
        call c%set_null(40_int64)                       ! inside the range about to be pasted over
        call c%set_null(5_int64)                        ! outside it, must survive
        call other%clear()
        call fill(other, vals, n, w)
        call other%set_null(3_int64)                    ! becomes destination row 32
        call c%paste(other, 30_int64, 1_int64, 20_int64)
        deallocate(want)
        allocate(want(w, n))
        want = .true.
        want(:, 5) = .false.
        want(:, 32) = .false.
        call expect_mask(error, c, want, trim(ws)//" paste replaces the pasted range's validity")
        if (allocated(error)) return
        ! ---- 6. reindex: a permutation carries each row's own element mask with it ----
        call c%clear()
        call fill(c, vals, n, w)
        call c%set_null(1_int64)
        call c%set_null(64_int64)
        call c%set_null(65_int64)
        call c%set_null(n, 1_int64)
        block
            integer(int64), allocatable :: perm(:)
            logical, allocatable :: before(:,:)
            allocate(perm(n))
            do i = 1_int64, n
                perm(i) = n - i + 1_int64               ! reverse
            end do
            call c%element_validity(before)
            call c%reindex(perm)
            want = .true.
            do i = 1_int64, n
                want(:, i) = before(:, perm(i))
            end do
        end block
        call expect_mask(error, c, want, trim(ws)//" reindex carries each row's element mask")
        if (allocated(error)) return
        if (allocated(got)) deallocate(got)
    end subroutine one_width
    !
    !> Initialises `c` to hold `vals` at width `w`, choosing the scalar kind at width 1 and the
    !! vector kind above it, so one sweep can cover both.
    subroutine fill(c, vals, n, w)
        type(parquet_column), intent(inout) :: c   !! the column to build.
        integer(int32), intent(in) :: vals(:,:)    !! (element, row) values.
        integer(int64), intent(in) :: n            !! row count.
        integer, intent(in) :: w                   !! elements per row.
        !
        if (w == 1) then
            call c%init(PK_INT32, n)
            call c%set_all(vals(1, :))
        else
            call c%init(PK_INT32_VEC, n, w)
            call c%set_all(vals)
        end if
    end subroutine fill
    !
    !> Asserts a column's per-element validity equals `want`, reporting the first disagreement by
    !! (element, row) rather than just "a mask differs" — with six widths and six operations, a
    !! bare failure would say almost nothing about which boundary broke.
    subroutine expect_mask(error, c, want, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_column), intent(inout) :: c            !! the column to inspect.
        logical, intent(in) :: want(:,:)                    !! expected (element, row) validity.
        character(len=*), intent(in) :: what                !! message prefix.
        logical, allocatable :: got(:,:)
        integer(int64) :: i, e
        character(len=64) :: where_s
        !
        call c%element_validity(got)
        if (.not. allocated(got)) then
            ! An unallocated mask is the documented "no nulls at all" answer.
            call check(error, all(want), what//" (column reports no nulls)")
            return
        end if
        if (size(got, 1) /= size(want, 1) .or. size(got, 2) /= size(want, 2)) then
            call check(error, .false., what//" (mask shape differs)")
            return
        end if
        do i = 1_int64, size(want, 2, kind=int64)
            do e = 1_int64, size(want, 1, kind=int64)
                if (got(e, i) .neqv. want(e, i)) then
                    write(where_s, "(A,I0,A,I0,A)") " at (element ", e, ", row ", i, ")"
                    call check(error, .false., what//trim(where_s))
                    return
                end if
            end do
        end do
        call check(error, .true., what)
    end subroutine expect_mask
    !
    !> Exercises `%get_elem`/`%set_elem` -- the type-bound element accessors -- on all eight
    !! non-string vector kinds. (`strv` has its own coverage in the string tests; its specifics
    !! live in `parquet_columns_string`, not `parquet_columns_access`.)
    !!
    !! These bindings are one-line forwarders onto the `parquet_column_get_elem_*` typed tier,
    !! and that tier is reached from `parquet_tables` -- but nothing called the bindings
    !! themselves, so a forwarder naming the wrong specific or transposing `i` and `e` would
    !! have gone unnoticed. Every fixture value therefore encodes BOTH its row and its element
    !! (`10*i + e`, or the temporal equivalent), which is what makes a transposed or off-by-one
    !! index a failure rather than a coincidence.
    !!
    !! Each kind also has one element marked null before `%set_elem` writes it, to assert the
    !! documented rule that writing an element clears THAT element's null bit and leaves its
    !! neighbour in the same row alone.
    subroutine test_elem_access_every_vector_kind(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), parameter :: NR = 4_int64
        integer(int32), parameter :: NW = 3_int32
        type(parquet_column) :: c
        integer(int32) :: m32(NW, NR), g32
        integer(int64) :: m64(NW, NR), g64
        real(real32) :: r32(NW, NR), h32
        real(real64) :: r64(NW, NR), h64
        logical :: mb(NW, NR), gb
        type(parquet_date) :: md(NW, NR), gd, wd
        type(parquet_time) :: mt(NW, NR), gt, wt
        type(parquet_timestamp) :: ms(NW, NR), gs, ws
        integer(int64) :: i, e
        !
        ! One fixture pattern, built once and reused per kind: value(e, i) distinguishes every
        ! (row, element) pair, so no two cells of the column hold the same value.
        do i = 1_int64, NR
            do e = 1_int64, int(NW, int64)
                m32(e, i) = int(10_int64*i + e, int32)
                m64(e, i) = 10_int64*i + e
                r32(e, i) = real(10_int64*i + e, real32) + 0.5_real32
                r64(e, i) = real(10_int64*i + e, real64) + 0.25_real64
                mb(e, i) = mod(i + e, 2_int64) == 0_int64
                call md(e, i)%set(2020 + int(i, int32), int(e, int32), 10 + int(i, int32))
                call mt(e, i)%set(int(i, int32), 10*int(e, int32), 0_int32)
                call ms(e, i)%set(2020 + int(i, int32), int(e, int32), 5, 12, 0, 0)
            end do
        end do
        !
        ! ---- PK_INT32_VEC ----
        call c%init(PK_INT32_VEC, NR, width=NW)
        call c%set_all(m32)
        do i = 1_int64, NR
            do e = 1_int64, int(NW, int64)
                call c%get_elem(i, e, g32)
                if (g32 /= m32(e, i)) then
                    call check(error, .false., "int32 vector get_elem should return element (e, i)")
                    return
                end if
            end do
        end do
        call c%set_null(2_int64, 3_int64)
        call c%set_elem(2_int64, 3_int64, 777_int32)
        call c%get_elem(2_int64, 3_int64, g32)
        call check(error, g32 == 777_int32, "int32 vector set_elem should store the new value")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 3_int64), &
            "int32 vector set_elem should clear that element's own null bit")
        if (allocated(error)) return
        call c%get_elem(2_int64, 2_int64, g32)
        call check(error, g32 == m32(2, 2), "int32 vector set_elem should leave its neighbour alone")
        if (allocated(error)) return
        !
        ! ---- PK_INT64_VEC ----
        call c%init(PK_INT64_VEC, NR, width=NW)
        call c%set_all(m64)
        do i = 1_int64, NR
            do e = 1_int64, int(NW, int64)
                call c%get_elem(i, e, g64)
                if (g64 /= m64(e, i)) then
                    call check(error, .false., "int64 vector get_elem should return element (e, i)")
                    return
                end if
            end do
        end do
        call c%set_null(2_int64, 3_int64)
        call c%set_elem(2_int64, 3_int64, 777_int64)
        call c%get_elem(2_int64, 3_int64, g64)
        call check(error, g64 == 777_int64, "int64 vector set_elem should store the new value")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 3_int64), &
            "int64 vector set_elem should clear that element's own null bit")
        if (allocated(error)) return
        call c%get_elem(2_int64, 2_int64, g64)
        call check(error, g64 == m64(2, 2), "int64 vector set_elem should leave its neighbour alone")
        if (allocated(error)) return
        !
        ! ---- PK_FLOAT32_VEC ----
        call c%init(PK_FLOAT32_VEC, NR, width=NW)
        call c%set_all(r32)
        do i = 1_int64, NR
            do e = 1_int64, int(NW, int64)
                call c%get_elem(i, e, h32)
                if (h32 /= r32(e, i)) then
                    call check(error, .false., "float32 vector get_elem should return element (e, i)")
                    return
                end if
            end do
        end do
        call c%set_null(2_int64, 3_int64)
        call c%set_elem(2_int64, 3_int64, 777.5_real32)
        call c%get_elem(2_int64, 3_int64, h32)
        call check(error, h32 == 777.5_real32, "float32 vector set_elem should store the new value")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 3_int64), &
            "float32 vector set_elem should clear that element's own null bit")
        if (allocated(error)) return
        call c%get_elem(2_int64, 2_int64, h32)
        call check(error, h32 == r32(2, 2), "float32 vector set_elem should leave its neighbour alone")
        if (allocated(error)) return
        !
        ! ---- PK_FLOAT64_VEC ----
        call c%init(PK_FLOAT64_VEC, NR, width=NW)
        call c%set_all(r64)
        do i = 1_int64, NR
            do e = 1_int64, int(NW, int64)
                call c%get_elem(i, e, h64)
                if (h64 /= r64(e, i)) then
                    call check(error, .false., "float64 vector get_elem should return element (e, i)")
                    return
                end if
            end do
        end do
        call c%set_null(2_int64, 3_int64)
        call c%set_elem(2_int64, 3_int64, 777.25_real64)
        call c%get_elem(2_int64, 3_int64, h64)
        call check(error, h64 == 777.25_real64, "float64 vector set_elem should store the new value")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 3_int64), &
            "float64 vector set_elem should clear that element's own null bit")
        if (allocated(error)) return
        call c%get_elem(2_int64, 2_int64, h64)
        call check(error, h64 == r64(2, 2), "float64 vector set_elem should leave its neighbour alone")
        if (allocated(error)) return
        !
        ! ---- PK_LOGICAL_VEC ----
        call c%init(PK_LOGICAL_VEC, NR, width=NW)
        call c%set_all(mb)
        do i = 1_int64, NR
            do e = 1_int64, int(NW, int64)
                call c%get_elem(i, e, gb)
                if (gb .neqv. mb(e, i)) then
                    call check(error, .false., "logical vector get_elem should return element (e, i)")
                    return
                end if
            end do
        end do
        call c%set_null(2_int64, 3_int64)
        call c%set_elem(2_int64, 3_int64, .not. mb(3, 2))
        call c%get_elem(2_int64, 3_int64, gb)
        call check(error, gb .neqv. mb(3, 2), "logical vector set_elem should store the new value")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 3_int64), &
            "logical vector set_elem should clear that element's own null bit")
        if (allocated(error)) return
        call c%get_elem(2_int64, 2_int64, gb)
        call check(error, gb .eqv. mb(2, 2), "logical vector set_elem should leave its neighbour alone")
        if (allocated(error)) return
        !
        ! ---- PK_DATE_VEC ----
        call wd%set(1999, 12, 31)
        call c%init(PK_DATE_VEC, NR, width=NW)
        call c%set_all(md)
        do i = 1_int64, NR
            do e = 1_int64, int(NW, int64)
                call c%get_elem(i, e, gd)
                if (gd /= md(e, i)) then
                    call check(error, .false., "date vector get_elem should return element (e, i)")
                    return
                end if
            end do
        end do
        call c%set_null(2_int64, 3_int64)
        call c%set_elem(2_int64, 3_int64, wd)
        call c%get_elem(2_int64, 3_int64, gd)
        call check(error, gd == wd, "date vector set_elem should store the new element")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 3_int64), &
            "date vector set_elem should clear that element's own null state")
        if (allocated(error)) return
        call c%get_elem(2_int64, 2_int64, gd)
        call check(error, gd == md(2, 2), "date vector set_elem should leave its neighbour alone")
        if (allocated(error)) return
        !
        ! ---- PK_TIME_VEC ----
        call wt%set(23, 59, 58)
        call c%init(PK_TIME_VEC, NR, width=NW)
        call c%set_all(mt)
        do i = 1_int64, NR
            do e = 1_int64, int(NW, int64)
                call c%get_elem(i, e, gt)
                if (gt /= mt(e, i)) then
                    call check(error, .false., "time vector get_elem should return element (e, i)")
                    return
                end if
            end do
        end do
        call c%set_null(2_int64, 3_int64)
        call c%set_elem(2_int64, 3_int64, wt)
        call c%get_elem(2_int64, 3_int64, gt)
        call check(error, gt == wt, "time vector set_elem should store the new element")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 3_int64), &
            "time vector set_elem should clear that element's own null state")
        if (allocated(error)) return
        call c%get_elem(2_int64, 2_int64, gt)
        call check(error, gt == mt(2, 2), "time vector set_elem should leave its neighbour alone")
        if (allocated(error)) return
        !
        ! ---- PK_TIMESTAMP_VEC ----
        call ws%set(1999, 12, 31, 23, 59, 58)
        call c%init(PK_TIMESTAMP_VEC, NR, width=NW)
        call c%set_all(ms)
        do i = 1_int64, NR
            do e = 1_int64, int(NW, int64)
                call c%get_elem(i, e, gs)
                if (gs /= ms(e, i)) then
                    call check(error, .false., "timestamp vector get_elem should return element (e, i)")
                    return
                end if
            end do
        end do
        call c%set_null(2_int64, 3_int64)
        call c%set_elem(2_int64, 3_int64, ws)
        call c%get_elem(2_int64, 3_int64, gs)
        call check(error, gs == ws, "timestamp vector set_elem should store the new element")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 3_int64), &
            "timestamp vector set_elem should clear that element's own null state")
        if (allocated(error)) return
        call c%get_elem(2_int64, 2_int64, gs)
        call check(error, gs == ms(2, 2), "timestamp vector set_elem should leave its neighbour alone")
        if (allocated(error)) return
        call check(error, .true., "every vector kind supports get_elem and set_elem")
    end subroutine test_elem_access_every_vector_kind
    !
    ! ==================================================================================
    ! Per-kind sweeps over the generated `case` arms
    !
    ! `shrink_storage` and `append_row_of` (`src/parquet_columns_mutate.f90`) are one big
    ! `select case (self%kind)` each, with a separate arm per kind that allocates and copies that
    ! kind's own storage. An arm that copied the wrong slice, or nothing at all, would be caught
    ! by no test that exercises one or two kinds -- and until these sweeps existed, exactly two
    ! arms of sixteen were reached. The fixture below is shared so that the expected values are
    ! written once, in `fixture_code`, rather than restated per kind per test.
    ! ==================================================================================
    !
    !> The value fixture row `i`, element `e` carries, as a plain integer every kind derives its
    !! own value from. Every cell of every fixture is distinct, so a row read from the wrong place
    !! -- or an element read from the wrong column of a vector row -- is a failure rather than a
    !! coincidence.
    !!
    !! **The multiplier is 11, not 10, and that is load-bearing for the LOGICAL kinds.** They
    !! derive a value from this code's parity, and `10*i + e` has the parity of `e` alone: every
    !! row of a `PK_LOGICAL` fixture would hold the same value, and a mutation that reads the
    !! wrong row would go undetected. It did -- an odd multiplier is what made the sweep able to
    !! see it. Any future kind deriving its value from part of this code should check the same way.
    pure integer function fixture_code(i, e) result(k)
        integer(int64), intent(in) :: i !! 1-based row index.
        integer(int64), intent(in) :: e !! 1-based element index (1 for a scalar kind).
        k = int(11_int64*i + e)
    end function fixture_code
    !
    !> The hour a time fixture holds at row `i`: the row index wrapped into 0..23, with
    !! `fixture_minute` carrying the rest, so a fixture of any row count under 1440 stays unique
    !! per row and legal for `parquet_time%set` (the row index alone was legal for 23 rows).
    pure integer(int32) function fixture_hour(i) result(h)
        integer(int64), intent(in) :: i !! 1-based row index.
        h = int(mod(i, 24_int64), int32)
    end function fixture_hour
    !
    !> The minute a time fixture holds at row `i`; see `fixture_hour`.
    pure integer(int32) function fixture_minute(i) result(m)
        integer(int64), intent(in) :: i !! 1-based row index.
        m = int(mod(i/24_int64, 60_int64), int32)
    end function fixture_minute
    !
    !> Initializes `c` as an empty column of `kind`, supplying `width` for the vector kinds only.
    subroutine init_kind(c, kind, nrows)
        type(parquet_column), intent(inout) :: c !! the column to initialize.
        integer, intent(in) :: kind              !! PK_* kind.
        integer(int64), intent(in) :: nrows      !! rows to allocate.
        !
        select case (kind)
        case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_DATE, PK_TIME, PK_TIMESTAMP)
            call c%init(kind, nrows)
        case default
            call c%init(kind, nrows, FIXW)
        end select
    end subroutine init_kind
    !
    !> Builds `nrows` rows of `kind` holding the `fixture_code` pattern. Every kind is written
    !! through `%set_at`, so this also keeps the sweeps honest about the row being addressed.
    subroutine make_fixture(c, kind, nrows)
        type(parquet_column), intent(inout) :: c !! receives the fixture.
        integer, intent(in) :: kind              !! PK_* kind to build.
        integer(int64), intent(in) :: nrows      !! rows to write.
        integer(int64) :: i
        type(parquet_date) :: d, dv(FIXW)
        type(parquet_time) :: t, tv(FIXW)
        type(parquet_timestamp) :: s, sv(FIXW)
        !
        call init_kind(c, kind, nrows)
        do i = 1_int64, nrows
            select case (kind)
            case (PK_INT32)
                call c%set_at(i, int(fixture_code(i, 1_int64), int32))
            case (PK_INT64)
                call c%set_at(i, int(fixture_code(i, 1_int64), int64))
            case (PK_FLOAT32)
                call c%set_at(i, real(fixture_code(i, 1_int64), real32) + 0.5_real32)
            case (PK_FLOAT64)
                call c%set_at(i, real(fixture_code(i, 1_int64), real64) + 0.25_real64)
            case (PK_LOGICAL)
                call c%set_at(i, mod(fixture_code(i, 1_int64), 2) == 0)
            case (PK_DATE)
                call d%set(2000 + fixture_code(i, 1_int64), 1, 1)
                call c%set_at(i, d)
            case (PK_TIME)
                call t%set(fixture_hour(i), fixture_minute(i), 0)
                call c%set_at(i, t)
            case (PK_TIMESTAMP)
                call s%set(2000 + fixture_code(i, 1_int64), 1, 1, 0, 0, 0)
                call c%set_at(i, s)
            case (PK_INT32_VEC)
                call c%set_at(i, [int(fixture_code(i, 1_int64), int32), int(fixture_code(i, 2_int64), int32)])
            case (PK_INT64_VEC)
                call c%set_at(i, [int(fixture_code(i, 1_int64), int64), int(fixture_code(i, 2_int64), int64)])
            case (PK_FLOAT32_VEC)
                call c%set_at(i, [real(fixture_code(i, 1_int64), real32) + 0.5_real32, &
                    real(fixture_code(i, 2_int64), real32) + 0.5_real32])
            case (PK_FLOAT64_VEC)
                call c%set_at(i, [real(fixture_code(i, 1_int64), real64) + 0.25_real64, &
                    real(fixture_code(i, 2_int64), real64) + 0.25_real64])
            case (PK_LOGICAL_VEC)
                call c%set_at(i, [mod(fixture_code(i, 1_int64), 2) == 0, mod(fixture_code(i, 2_int64), 2) == 0])
            case (PK_DATE_VEC)
                call dv(1)%set(2000 + fixture_code(i, 1_int64), 1, 1)
                call dv(2)%set(2000 + fixture_code(i, 2_int64), 1, 1)
                call c%set_at(i, dv)
            case (PK_TIME_VEC)
                call tv(1)%set(fixture_hour(i), fixture_minute(i), 5)
                call tv(2)%set(fixture_hour(i), fixture_minute(i), 10)
                call c%set_at(i, tv)
            case (PK_TIMESTAMP_VEC)
                call sv(1)%set(2000 + fixture_code(i, 1_int64), 1, 1, 0, 0, 0)
                call sv(2)%set(2000 + fixture_code(i, 2_int64), 1, 1, 0, 0, 0)
                call c%set_at(i, sv)
            end select
        end do
    end subroutine make_fixture
    !
    !> Asserts that row `i` of `c` holds whatever `make_fixture` wrote into row `want`. Reading the
    !! expectation back through the same `fixture_code` the builder used is what keeps a sixteen-arm
    !! sweep from needing sixteen hand-written expectations that could drift out of step with it.
    subroutine expect_fixture_row(c, kind, i, want, what, error)
        type(parquet_column), intent(in) :: c               !! the column to read.
        integer, intent(in) :: kind                         !! its PK_* kind.
        integer(int64), intent(in) :: i                     !! row of `c` to read.
        integer(int64), intent(in) :: want                  !! fixture row it should equal.
        character(len=*), intent(in) :: what                !! context, for the message.
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        integer(int32) :: g32, v32(FIXW)
        integer(int64) :: g64, v64(FIXW)
        real(real32) :: h32, w32(FIXW)
        real(real64) :: h64, w64(FIXW)
        logical :: gb, vb(FIXW)
        type(parquet_date) :: gd, ed, edv(FIXW)
        type(parquet_time) :: gt, et, etv(FIXW)
        type(parquet_timestamp) :: gs, es, esv(FIXW)
        logical :: ok
        !
        ok = .false.
        select case (kind)
        case (PK_INT32)
            call c%get_at(i, g32)
            ok = g32 == int(fixture_code(want, 1_int64), int32)
        case (PK_INT64)
            call c%get_at(i, g64)
            ok = g64 == int(fixture_code(want, 1_int64), int64)
        case (PK_FLOAT32)
            call c%get_at(i, h32)
            ok = h32 == real(fixture_code(want, 1_int64), real32) + 0.5_real32
        case (PK_FLOAT64)
            call c%get_at(i, h64)
            ok = h64 == real(fixture_code(want, 1_int64), real64) + 0.25_real64
        case (PK_LOGICAL)
            call c%get_at(i, gb)
            ok = gb .eqv. (mod(fixture_code(want, 1_int64), 2) == 0)
        case (PK_DATE)
            call c%get_at(i, gd)
            call ed%set(2000 + fixture_code(want, 1_int64), 1, 1)
            ok = gd == ed
        case (PK_TIME)
            call c%get_at(i, gt)
            call et%set(fixture_hour(want), fixture_minute(want), 0)
            ok = gt == et
        case (PK_TIMESTAMP)
            call c%get_at(i, gs)
            call es%set(2000 + fixture_code(want, 1_int64), 1, 1, 0, 0, 0)
            ok = gs == es
        case (PK_INT32_VEC)
            call c%get_at(i, v32)
            ok = all(v32 == [int(fixture_code(want, 1_int64), int32), int(fixture_code(want, 2_int64), int32)])
        case (PK_INT64_VEC)
            call c%get_at(i, v64)
            ok = all(v64 == [int(fixture_code(want, 1_int64), int64), int(fixture_code(want, 2_int64), int64)])
        case (PK_FLOAT32_VEC)
            call c%get_at(i, w32)
            ok = all(w32 == [real(fixture_code(want, 1_int64), real32) + 0.5_real32, &
                real(fixture_code(want, 2_int64), real32) + 0.5_real32])
        case (PK_FLOAT64_VEC)
            call c%get_at(i, w64)
            ok = all(w64 == [real(fixture_code(want, 1_int64), real64) + 0.25_real64, &
                real(fixture_code(want, 2_int64), real64) + 0.25_real64])
        case (PK_LOGICAL_VEC)
            call c%get_at(i, vb)
            ok = all(vb .eqv. [mod(fixture_code(want, 1_int64), 2) == 0, mod(fixture_code(want, 2_int64), 2) == 0])
        case (PK_DATE_VEC)
            call c%get_at(i, edv)
            call gd%set(2000 + fixture_code(want, 1_int64), 1, 1)
            call ed%set(2000 + fixture_code(want, 2_int64), 1, 1)
            ok = edv(1) == gd .and. edv(2) == ed
        case (PK_TIME_VEC)
            call c%get_at(i, etv)
            call gt%set(fixture_hour(want), fixture_minute(want), 5)
            call et%set(fixture_hour(want), fixture_minute(want), 10)
            ok = etv(1) == gt .and. etv(2) == et
        case (PK_TIMESTAMP_VEC)
            call c%get_at(i, esv)
            call gs%set(2000 + fixture_code(want, 1_int64), 1, 1, 0, 0, 0)
            call es%set(2000 + fixture_code(want, 2_int64), 1, 1, 0, 0, 0)
            ok = esv(1) == gs .and. esv(2) == es
        end select
        call check(error, ok, what)
    end subroutine expect_fixture_row
    !
    !> `shrink_storage` reallocates a column's own storage down to `nrows` and copies the live rows
    !! across, with a separate `select case` arm per kind. Only two of the sixteen were reached
    !! before this sweep, so an arm that allocated the new buffer and copied the wrong slice -- or
    !! forgot the copy -- would have shipped unnoticed on the other fourteen.
    !!
    !! **Asserting the capacity alone is not enough**, which is why every row is read back
    !! afterwards: `%capacity()` and `%length()` are bookkeeping the arm does not touch, so a
    !! capacity-only test passes against an arm that reallocates and copies nothing.
    subroutine test_shrink_to_fit_every_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        integer(int64), parameter :: NR = 3_int64
        type(parquet_column) :: c
        character(len=:), allocatable :: kn
        integer(int64) :: i
        integer :: ki
        logical :: released
        !
        do ki = 1, size(ARRAY_KINDS)
            call parquet_kind_name(ARRAY_KINDS(ki), kn)
            call make_fixture(c, ARRAY_KINDS(ki), NR)
            call c%reserve(8_int64*NR)
            call check(error, c%capacity() > c%length(), kn // ": reserve should leave slack to release")
            if (allocated(error)) return
            call c%shrink_to_fit(released)
            call check(error, released, kn // ": shrink_to_fit should report that it released slack")
            if (allocated(error)) return
            call check(error, c%capacity() == c%length(), kn // ": shrink_to_fit should leave an exact fit")
            if (allocated(error)) return
            do i = 1_int64, NR
                call expect_fixture_row(c, ARRAY_KINDS(ki), i, i, &
                    kn // ": shrink_to_fit should copy every row across unchanged", error)
                if (allocated(error)) return
            end do
            ! Nothing left to release: the no-op branch %compact depends on, on every kind.
            call c%shrink_to_fit(released)
            call check(error, .not. released, kn // ": a second shrink_to_fit should release nothing")
            if (allocated(error)) return
        end do
        call check(error, .true., "every array kind survives shrink_to_fit with its values intact")
    end subroutine test_shrink_to_fit_every_kind
    !
    !> `append_row_of` copies ONE row of another column, again through a per-kind `select case`,
    !! and again only two arms of sixteen were reached before this sweep.
    !!
    !! The rows are appended OUT of order (2, 1, 3) so that an arm reading a fixed row, or the
    !! destination's own last row, instead of the row it was given is a failure. The source row
    !! appended last is null, which covers the other half of the operation: a bitmap kind has to
    !! copy the validity bits across, while a temporal kind carries its null state inside the
    !! element and only has to invalidate the cached answer -- two paths reached from one call.
    !!
    !! **The two value-checked rows must be 1 and 2, not 1 and 3**, because the LOGICAL kinds have
    !! only two possible values and `fixture_code`'s parity makes rows 1 and 3 agree. A permutation
    !! reading them would let a wrong-row mutation through on those kinds -- as an earlier version
    !! of this test did. Whenever a row is added or the permutation changes, re-check that the
    !! rows being compared still differ on a two-valued kind.
    subroutine test_append_row_of_every_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        integer(int64), parameter :: NR = 3_int64
        type(parquet_column) :: src, dst
        character(len=:), allocatable :: kn
        integer :: ki
        !
        do ki = 1, size(ARRAY_KINDS)
            call parquet_kind_name(ARRAY_KINDS(ki), kn)
            call make_fixture(src, ARRAY_KINDS(ki), NR)
            call src%set_null(NR)
            call init_kind(dst, ARRAY_KINDS(ki), 0_int64)
            call dst%append_row_of(src, 2_int64)
            call dst%append_row_of(src, 1_int64)
            call dst%append_row_of(src, NR)
            call check(error, dst%length() == NR, kn // ": each append_row_of should add exactly one row")
            if (allocated(error)) return
            call expect_fixture_row(dst, ARRAY_KINDS(ki), 1_int64, 2_int64, &
                kn // ": the first appended row should be the source row named", error)
            if (allocated(error)) return
            call expect_fixture_row(dst, ARRAY_KINDS(ki), 2_int64, 1_int64, &
                kn // ": the second appended row should be the source row named", error)
            if (allocated(error)) return
            ! Row 3 came from the source's null row and is not read for a value: on a temporal kind
            ! the element itself is null, and asking a null element for its fields aborts.
            call check(error, .not. dst%is_null(1_int64), kn // ": a valid source row appends as valid")
            if (allocated(error)) return
            call check(error, .not. dst%is_null(2_int64), kn // ": the second valid source row appends as valid")
            if (allocated(error)) return
            call check(error, dst%is_null(3_int64), kn // ": a null source row appends as null")
            if (allocated(error)) return
            call check(error, src%length() == NR, kn // ": append_row_of should not change the source")
            if (allocated(error)) return
        end do
        call check(error, .true., "every array kind copies one row, and its null state, through append_row_of")
    end subroutine test_append_row_of_every_kind
    !
    ! ==================================================================================
    ! Paths reached only through the type-bound bindings, or only on one kind
    ! ==================================================================================
    !
    !> `%get_elem`/`%set_elem` on a PK_STRING_VEC column. The string kinds keep their storage in an
    !! embedded parquet_string_column indexed by a FLAT element position, so the element form has
    !! to compute `(i-1)*width + e` itself rather than indexing a rank-2 array the way every other
    !! vector kind does -- which is exactly the arithmetic a test that only ever asks for element 1,
    !! or only ever reads row 1, cannot tell apart from a stride of zero.
    !!
    !! Every element gets a distinct value for that reason, and the assertions cross both axes.
    subroutine test_string_vector_elem_access(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=6) :: vals(3, 4)
        character(len=:), allocatable :: s
        integer :: e, i
        !
        do i = 1, 4
            do e = 1, 3
                write(vals(e, i), '("r", i0, "e", i0)') i, e
            end do
        end do
        call c%init(PK_STRING_VEC, 4_int64, 3_int32)
        call c%set_all(vals)
        !
        call c%get_elem(4_int64, 3_int64, s)
        call check(error, s == "r4e3", "get_elem must reach the last element of the last row")
        if (allocated(error)) return
        call c%get_elem(2_int64, 1_int64, s)
        call check(error, s == "r2e1", "get_elem must reach the first element of an interior row")
        if (allocated(error)) return
        call c%get_elem(1_int64, 2_int64, s)
        call check(error, s == "r1e2", "get_elem must reach an interior element of the first row")
        if (allocated(error)) return
        !
        call c%set_elem(3_int64, 2_int64, "written")
        call c%get_elem(3_int64, 2_int64, s)
        call check(error, s == "written", "set_elem must write the element get_elem reads back")
        if (allocated(error)) return
        ! The neighbours on both axes must be untouched -- a wrong stride shows up here, not above.
        call c%get_elem(3_int64, 1_int64, s)
        call check(error, s == "r3e1", "set_elem must not disturb the previous element of the same row")
        if (allocated(error)) return
        call c%get_elem(3_int64, 3_int64, s)
        call check(error, s == "r3e3", "set_elem must not disturb the next element of the same row")
        if (allocated(error)) return
        call c%get_elem(2_int64, 2_int64, s)
        call check(error, s == "r2e2", "set_elem must not disturb the same element of the previous row")
        if (allocated(error)) return
        call c%get_elem(4_int64, 2_int64, s)
        call check(error, s == "r4e2", "set_elem must not disturb the same element of the next row")
        if (allocated(error)) return
        !
        ! A null element reads back as "" rather than aborting; %is_null is how the two are told
        ! apart, and %set_elem clears the null it lands on.
        call c%set_null(1_int64, 3_int64)
        call c%get_elem(1_int64, 3_int64, s)
        call check(error, s == "" .and. c%is_null(1_int64, 3_int64), &
            "a null string element must read back as an empty string, with is_null reporting it")
        if (allocated(error)) return
        call c%set_elem(1_int64, 3_int64, "back")
        call check(error, .not. c%is_null(1_int64, 3_int64), "set_elem must clear that element's null")
    end subroutine test_string_vector_elem_access
    !
    !> Two `%reserve` paths a caller reaches without meaning to: passing a plain default-kind
    !! `integer` (the int32 specific, which exists so that `call c%reserve(1000)` compiles at all),
    !! and reserving on a STRING column, whose store is indexed by element rather than by row, so
    !! the reservation has to be scaled by the width.
    !!
    !! The width scaling is the part worth asserting: reserving `n` rows of a width-`w` string
    !! column must make room for `n*w` elements, and a test on a scalar column cannot see the
    !! difference because `w` is 1 there.
    subroutine test_reserve_int32_and_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c, s, sv
        integer(int32) :: vals(4) = [1, 2, 3, 4]
        integer(int32) :: got
        !
        call c%init(PK_INT32, 4_int64)
        call c%set_all(vals)
        call c%reserve(64)              ! default-kind integer: the int32 specific
        call check(error, c%capacity() >= 64_int64, "reserve(default-kind integer) must grow the capacity")
        if (allocated(error)) return
        call check(error, c%length() == 4_int64, "reserve must not change the row count")
        if (allocated(error)) return
        call c%get_at(1_int64, got)
        call check(error, got == 1_int32, "reserve must not disturb the values already stored")
        if (allocated(error)) return
        call c%get_at(4_int64, got)
        call check(error, got == 4_int32, "reserve must not disturb the last value stored")
        if (allocated(error)) return
        !
        call s%init(PK_STRING, 0_int64)
        call s%reserve(32_int64)
        call check(error, s%length() == 0_int64, "reserve on a string column must not create rows")
        if (allocated(error)) return
        call check(error, s%capacity() >= 32_int64, "reserve on a string column must grow its capacity")
        if (allocated(error)) return
        !
        ! A width-4 string column indexes its store per ELEMENT, so 10 rows need 40 element slots.
        call sv%init(PK_STRING_VEC, 0_int64, 4_int32)
        call sv%reserve(10_int64)
        call check(error, sv%capacity() >= 10_int64, &
            "reserve on a string VECTOR column must scale the reservation by the width, not treat " // &
            "the store's element index as a row index")
    end subroutine test_reserve_int32_and_string
    !
    !> `%shrink_to_fit` releases the spare row capacity, and the validity bitmap is sized from that
    !! capacity rather than from the row count -- so shrinking leaves the bitmap oversized unless
    !! it is trimmed too. That trim is the one place the bitmap ever gets smaller (`ensure_bitmap`
    !! only grows), and nothing else in the suite reached it, because it needs a column that has
    !! BOTH a bitmap and enough slack for the trimmed size to differ by a whole 64-bit block.
    !!
    !! 200 rows reserved, shrunk back to 3: 200 bits is four blocks, 3 bits is one. A shrink that
    !! forgot the bitmap would leave three blocks of nothing, and the nulls must survive either way
    !! -- which is what stops the trim from being "free" by simply dropping the map.
    subroutine test_shrink_to_fit_trims_bitmap(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: vals(3) = [10, 20, 30]
        integer(int32) :: got
        integer(int64) :: before, after
        logical :: released
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_all(vals)
        call c%set_null(2_int64)
        call c%reserve(200_int64)
        before = c%validity_bytes()
        call check(error, before > 0_int64, "the fixture must actually have a bitmap to trim")
        if (allocated(error)) return
        call check(error, c%capacity() >= 200_int64, "the fixture must actually have slack to release")
        if (allocated(error)) return
        !
        call c%shrink_to_fit(released)
        after = c%validity_bytes()
        call check(error, released, "shrink_to_fit must report that it released capacity")
        if (allocated(error)) return
        call check(error, c%capacity() == 3_int64, "shrink_to_fit must bring the capacity back to the row count")
        if (allocated(error)) return
        call check(error, after < before, &
            "the bitmap is sized from the capacity, so shrinking the storage must trim it too")
        if (allocated(error)) return
        call check(error, after > 0_int64, "trimming the bitmap must not discard it -- the column still has a null")
        if (allocated(error)) return
        call check(error, c%is_null(2_int64), "the surviving null must still be reported after the trim")
        if (allocated(error)) return
        call check(error, (.not. c%is_null(1_int64)) .and. (.not. c%is_null(3_int64)), &
            "trimming the bitmap must not invent nulls on the valid rows")
        if (allocated(error)) return
        call c%get_at(1_int64, got)
        call check(error, got == 10_int32, "shrink_to_fit must not disturb the first value")
        if (allocated(error)) return
        call c%get_at(3_int64, got)
        call check(error, got == 30_int32, "shrink_to_fit must not disturb the last value")
    end subroutine test_shrink_to_fit_trims_bitmap
    !
    !> `%paste` from a null-free source into a column that has nulls must clear the pasted range's
    !! validity bits -- that is `%paste`'s REPLACE rule. The clearing goes through `bits_clear_range`,
    !! which has a single-word fast path and a multi-word one, and every existing paste test fits
    !! inside one 64-bit block, so only the fast path had ever run.
    !!
    !! The fixture spans three blocks deliberately: bits 11..160 of a 200-row column, so the range
    !! starts mid-block, ends mid-block, and has one whole block strictly between the two -- the
    !! only shape that reaches the interior-block loop at all. Nulls are placed immediately outside
    !! both ends so that a range that over-clears by even one bit is visible.
    subroutine test_paste_clears_multi_block_range(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: dst, src
        integer(int32) :: dvals(200), svals(150)
        integer(int32) :: got
        integer(int64) :: i
        !
        do i = 1_int64, 200_int64
            dvals(i) = int(i, int32)
        end do
        do i = 1_int64, 150_int64
            svals(i) = int(1000_int64 + i, int32)
        end do
        call dst%init(PK_INT32, 200_int64)
        call dst%set_all(dvals)
        ! Null every row of the destination, so that anything the paste fails to clear stays null.
        do i = 1_int64, 200_int64
            call dst%set_null(i)
        end do
        call src%init(PK_INT32, 150_int64)
        call src%set_all(svals)          ! null-free: set_all drops the bitmap in O(1)
        call check(error, .not. src%any_null(), "the source must be null-free for paste to take the clearing path")
        if (allocated(error)) return
        !
        call dst%paste(src, 11_int64)    ! rows 11..160, i.e. bits 11..160: blocks 1, 2 and 3
        !
        call check(error, dst%is_null(10_int64), &
            "the row immediately before the pasted range must keep its null -- the clear over-reached")
        if (allocated(error)) return
        call check(error, dst%is_null(161_int64), &
            "the row immediately after the pasted range must keep its null -- the clear over-reached")
        if (allocated(error)) return
        do i = 11_int64, 160_int64
            if (dst%is_null(i)) then
                call check(error, .false., "every pasted row must come back valid: a bit was left set in the range")
                return
            end if
        end do
        ! One assertion per block the range touches, so a failure names where it went wrong.
        call check(error, .not. dst%is_null(11_int64), "the first bit of the range (block 1) must be cleared")
        if (allocated(error)) return
        call check(error, .not. dst%is_null(100_int64), "an interior whole block (block 2) must be cleared")
        if (allocated(error)) return
        call check(error, .not. dst%is_null(160_int64), "the last bit of the range (block 3) must be cleared")
        if (allocated(error)) return
        call dst%get_at(11_int64, got)
        call check(error, got == 1001_int32, "paste must write the source values from the start of the range")
        if (allocated(error)) return
        call dst%get_at(160_int64, got)
        call check(error, got == 1150_int32, "paste must write the source values to the end of the range")
    end subroutine test_paste_clears_multi_block_range
    !
    !> `%ensure_validity` pre-allocates the validity storage so that the first null does not have to.
    !! It has three dispatch classes and the string one was unexercised: a string column's validity
    !! lives in the embedded parquet_string_column, not in this column's bitmap, so it has to be
    !! reserved through the store rather than by `ensure_bitmap`.
    !!
    !! `%validity_bytes` reports the COLUMN bitmap, which is 0 for a string column either way -- so
    !! `%has_validity_storage` is the observation that can tell the two apart, and the negative
    !! control is the same column before the call.
    subroutine test_ensure_validity_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=4) :: vals(3) = [character(len=4) :: "aa", "bb", "cc"]
        !
        call c%init(PK_STRING, 3_int64)
        call c%set_all(vals)
        call check(error, .not. c%has_validity_storage(), &
            "a null-free string column must start without validity storage -- otherwise the call below proves nothing")
        if (allocated(error)) return
        !
        call c%ensure_validity()
        call check(error, c%has_validity_storage(), &
            "ensure_validity on a string column must reserve the embedded store's own validity")
        if (allocated(error)) return
        call check(error, c%validity_bytes() == 0_int64, &
            "a string column must NOT gain a column-level bitmap -- its nulls live in the string store")
        if (allocated(error)) return
        call check(error, .not. c%any_null(), "reserving validity must not mark anything null")
        if (allocated(error)) return
        call c%set_null(2_int64)
        call check(error, c%is_null(2_int64) .and. (.not. c%is_null(1_int64)), &
            "the reserved validity must still record a null correctly afterwards")
    end subroutine test_ensure_validity_string
    !
    !> The element forms `%is_null(i, e)` and `%set_null(i, e)` are defined on SCALAR kinds too,
    !! where `width` is 1 so `e` can only be 1 and they mean the same as the row forms. That is
    !! deliberate -- it is what lets generic code use one shape for every kind -- and on the three
    !! scalar temporal kinds it had never been called, because those kinds carry their null inside
    !! the element rather than in a bitmap and so take their own arm of both dispatches.
    !!
    !! The pairing is the assertion: the element form and the row form must agree in both
    !! directions, on each of date, time and timestamp.
    subroutine test_temporal_scalar_element_validity(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: cd, ct, cs
        type(parquet_date) :: d(2)
        type(parquet_time) :: t(2)
        type(parquet_timestamp) :: ts(2)
        integer :: i
        !
        do i = 1, 2
            call d(i)%set(2026, 8, 10 + i)
            call t(i)%set(i, 30, 0)
            call ts(i)%set(2026, 8, 10 + i, i, 30, 0)
        end do
        call cd%init(PK_DATE, 2_int64)
        call cd%set_all(d)
        call ct%init(PK_TIME, 2_int64)
        call ct%set_all(t)
        call cs%init(PK_TIMESTAMP, 2_int64)
        call cs%set_all(ts)
        !
        call check(error, .not. cd%is_null(1_int64, 1_int64), "a valid date element must not report null")
        if (allocated(error)) return
        call check(error, .not. ct%is_null(1_int64, 1_int64), "a valid time element must not report null")
        if (allocated(error)) return
        call check(error, .not. cs%is_null(1_int64, 1_int64), "a valid timestamp element must not report null")
        if (allocated(error)) return
        !
        call cd%set_null(2_int64, 1_int64)
        call ct%set_null(2_int64, 1_int64)
        call cs%set_null(2_int64, 1_int64)
        !
        call check(error, cd%is_null(2_int64, 1_int64) .and. cd%is_null(2_int64), &
            "set_null(i, e) on a scalar date column must agree with the row form in both directions")
        if (allocated(error)) return
        call check(error, ct%is_null(2_int64, 1_int64) .and. ct%is_null(2_int64), &
            "set_null(i, e) on a scalar time column must agree with the row form in both directions")
        if (allocated(error)) return
        call check(error, cs%is_null(2_int64, 1_int64) .and. cs%is_null(2_int64), &
            "set_null(i, e) on a scalar timestamp column must agree with the row form in both directions")
        if (allocated(error)) return
        ! Row 1 is the negative control: nulling row 2's element must not touch it.
        call check(error, (.not. cd%is_null(1_int64, 1_int64)) .and. (.not. ct%is_null(1_int64, 1_int64)) &
            .and. (.not. cs%is_null(1_int64, 1_int64)), &
            "set_null(i, e) must mark only the element it names")
        if (allocated(error)) return
        ! The cached "has a null" flag has to see an element-form write as well as a row-form one.
        call check(error, cd%any_null() .and. ct%any_null() .and. cs%any_null(), &
            "the temporal null cache must be refreshed by the element form of set_null too")
    end subroutine test_temporal_scalar_element_validity
    !
    !> `%clear_null(i, e)` on a string column. The string kinds clear a null by writing an empty
    !! string into the store (there is no bitmap bit to clear), and only the whole-row form had
    !! been exercised -- so on a string VECTOR the element form's flat-index arithmetic was
    !! unchecked, which is where clearing the wrong element would hide.
    subroutine test_clear_null_string_element(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=4) :: vals(2, 3)
        integer :: e, i
        !
        do i = 1, 3
            do e = 1, 2
                write(vals(e, i), '("v", i0, i0)') i, e
            end do
        end do
        call c%init(PK_STRING_VEC, 3_int64, 2_int32)
        call c%set_all(vals)
        call c%set_null(2_int64, 1_int64)
        call c%set_null(2_int64, 2_int64)
        call check(error, c%is_null(2_int64, 1_int64) .and. c%is_null(2_int64, 2_int64), &
            "the fixture must actually start with both of row 2's elements null")
        if (allocated(error)) return
        !
        call c%clear_null(2_int64, 2_int64)
        call check(error, .not. c%is_null(2_int64, 2_int64), "clear_null(i, e) must mark that element valid")
        if (allocated(error)) return
        call check(error, c%is_null(2_int64, 1_int64), &
            "clear_null(i, e) must leave the row's other element null -- it names one element, not the row")
        if (allocated(error)) return
        ! Also on a scalar string column, where the flat index is the row index.
        call c%clear_null(2_int64, 1_int64)
        call check(error, .not. c%is_null(2_int64), &
            "clearing the last null element of a row must make the row itself report valid")
    end subroutine test_clear_null_string_element
    !
    !> `%row_validity` and `%element_validity` specialise on the kind ONCE and then use an elemental
    !! array expression per temporal kind, so each of the six has its own arm in each of the two
    !! procedures -- twelve arms that share no code. Several had never run.
    !!
    !! Both masks are built for all six kinds and cross-checked against `%is_null`, which is the
    !! independent oracle: the bulk form must agree with the per-element one everywhere. The vector
    !! fixtures null ONE element of a row rather than the whole row, because that is where the two
    !! shapes differ -- `row_validity` must report the row null while `element_validity` reports
    !! only that element null, and a mask built with the wrong `dim=` would swap them.
    subroutine test_temporal_bulk_validity_every_kind(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer, parameter :: TEMPORAL_KINDS(6) = [PK_DATE, PK_TIME, PK_TIMESTAMP, &
            PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC]
        logical, allocatable :: rowv(:), elemv(:, :)
        integer(int64) :: i, e, w
        integer :: ki
        character(len=:), allocatable :: kn
        !
        do ki = 1, 6
            call parquet_kind_name(TEMPORAL_KINDS(ki), kn)
            w = merge(int(FIXW, int64), 1_int64, ki > 3)
            call make_fixture(c, TEMPORAL_KINDS(ki), 4_int64)
            ! Null exactly one ELEMENT of row 3. On a scalar kind that is the whole row; on a
            ! vector kind it deliberately is not.
            call c%set_null(3_int64, w)
            !
            call c%row_validity(rowv)
            call check(error, allocated(rowv), kn // ": row_validity must build a mask for a column that has a null")
            if (allocated(error)) return
            call check(error, size(rowv, kind=int64) == 4_int64, kn // ": the row mask must be nrows long")
            if (allocated(error)) return
            call check(error, .not. rowv(3), kn // ": a row with a null element must report as not valid")
            if (allocated(error)) return
            call check(error, rowv(1) .and. rowv(2) .and. rowv(4), &
                kn // ": rows with no null element must report as valid")
            if (allocated(error)) return
            !
            call c%element_validity(elemv)
            call check(error, allocated(elemv), kn // ": element_validity must build a mask too")
            if (allocated(error)) return
            call check(error, size(elemv, 1, kind=int64) == w .and. size(elemv, 2, kind=int64) == 4_int64, &
                kn // ": the element mask must be shaped (width, nrows)")
            if (allocated(error)) return
            ! The oracle: the bulk mask must agree with %is_null element by element.
            do i = 1_int64, 4_int64
                do e = 1_int64, w
                    if (elemv(e, i) .eqv. c%is_null(i, e)) then
                        call check(error, .false., &
                            kn // ": element_validity disagrees with is_null on some element")
                        return
                    end if
                end do
            end do
            ! On a vector kind the two masks must genuinely differ: the row is not valid, but the
            ! element that was NOT nulled still is.
            if (w > 1_int64) then
                call check(error, elemv(1, 3), &
                    kn // ": nulling one element must leave the row's other element valid in the element mask")
                if (allocated(error)) return
            end if
        end do
        call check(error, .true., "every temporal kind builds both bulk validity masks consistently with is_null")
    end subroutine test_temporal_bulk_validity_every_kind
    !
    !> `%set_validity` with a rank-2 `(width, nrows)` mask writes the whole per-element null state in
    !! one pass. Each temporal kind has its own arm -- the setters are elemental but only a subset
    !! of elements is being nulled, and Fortran has no masked elemental call, so each arm is a
    !! hand-written loop -- and only the timestamp-vector one had ever run.
    !!
    !! `set_validity` only ADDS nulls (a `.true.` entry never clears one), so the fixture starts
    !! null-free and the assertion is an exact match against the mask, checked through `%is_null`.
    subroutine test_set_validity_elems_temporal(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer, parameter :: TEMPORAL_KINDS(6) = [PK_DATE, PK_TIME, PK_TIMESTAMP, &
            PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC]
        logical, allocatable :: mask(:, :)
        logical, allocatable :: rowv(:)
        integer(int64) :: i, e, w
        integer :: ki
        character(len=:), allocatable :: kn
        !
        do ki = 1, 6
            call parquet_kind_name(TEMPORAL_KINDS(ki), kn)
            w = merge(int(FIXW, int64), 1_int64, ki > 3)
            call make_fixture(c, TEMPORAL_KINDS(ki), 3_int64)
            call check(error, .not. c%any_null(), kn // ": the fixture must start null-free")
            if (allocated(error)) return
            !
            if (allocated(mask)) deallocate(mask)
            allocate(mask(w, 3_int64))
            mask = .true.
            mask(1, 2) = .false.                       ! first element of row 2
            if (w > 1_int64) mask(w, 3) = .false.      ! LAST element of row 3, on a vector kind
            call c%set_validity(mask)
            !
            do i = 1_int64, 3_int64
                do e = 1_int64, w
                    if (c%is_null(i, e) .eqv. mask(e, i)) then
                        call check(error, .false., kn // ": set_validity did not reproduce the mask it was given")
                        return
                    end if
                end do
            end do
            call check(error, c%any_null(), kn // ": a mask carrying a .false. entry must leave the column with a null")
            if (allocated(error)) return
            ! The consequence, not just the flag: a temporal column caches that answer, and every
            ! bulk validity path short-circuits on it. A stale cache therefore hands back an
            ! UNALLOCATED mask, which reaches an optional dummy as absent -- so a write would emit
            ! the column as non-nullable and lose every null this call just recorded, silently.
            if (allocated(rowv)) deallocate(rowv)
            call c%row_validity(rowv)
            call check(error, allocated(rowv), &
                kn // ": after set_validity the bulk row mask must be built, not skipped as null-free")
            if (allocated(error)) return
            call check(error, .not. rowv(2), kn // ": the bulk row mask must report the row the element mask nulled")
            if (allocated(error)) return
            if (w > 1_int64) then
                call check(error, .not. c%is_null(2_int64, w), &
                    kn // ": nulling element 1 of a row must not null its other elements")
                if (allocated(error)) return
            end if
        end do
        call check(error, .true., "every temporal kind writes an element-level validity mask correctly")
    end subroutine test_set_validity_elems_temporal
    !
    !> `%set_validity` with a rank-1 `(nrows)` mask means "this ROW is null", i.e. every element of
    !! it. The kinds that carry their null inside the element -- the two string kinds and the six
    !! temporal ones -- cannot use the bitmap shortcut the other kinds take and go through a
    !! per-row `%set_null(i)` loop instead, which nothing had exercised.
    !!
    !! A vector fixture is used so that "the whole row" is more than one element, which is what
    !! distinguishes the row form from the element form.
    subroutine test_set_validity_rows_element_carried(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer, parameter :: KINDS(3) = [PK_STRING_VEC, PK_DATE_VEC, PK_TIME_VEC]
        logical :: mask(3) = [.true., .false., .true.]
        integer(int64) :: e
        integer :: ki
        character(len=:), allocatable :: kn
        character(len=4) :: svals(2, 3)
        integer :: i, ee
        !
        do i = 1, 3
            do ee = 1, 2
                write(svals(ee, i), '("s", i0, i0)') i, ee
            end do
        end do
        do ki = 1, 3
            call parquet_kind_name(KINDS(ki), kn)
            if (KINDS(ki) == PK_STRING_VEC) then
                call c%init(PK_STRING_VEC, 3_int64, FIXW)
                call c%set_all(svals)
            else
                call make_fixture(c, KINDS(ki), 3_int64)
            end if
            call check(error, .not. c%any_null(), kn // ": the fixture must start null-free")
            if (allocated(error)) return
            !
            call c%set_validity(mask)
            do e = 1_int64, int(FIXW, int64)
                call check(error, c%is_null(2_int64, e), &
                    kn // ": a .false. row entry must null EVERY element of that row")
                if (allocated(error)) return
                call check(error, .not. c%is_null(1_int64, e), &
                    kn // ": a .true. row entry must leave every element of that row valid")
                if (allocated(error)) return
                call check(error, .not. c%is_null(3_int64, e), &
                    kn // ": the row after the nulled one must be untouched")
                if (allocated(error)) return
            end do
            call check(error, c%is_null(2_int64), kn // ": the nulled row must report null in the row form too")
            if (allocated(error)) return
        end do
        call check(error, .true., "a row mask nulls whole rows on every kind that carries its nulls in the element")
    end subroutine test_set_validity_rows_element_carried
    !
    !> The string kinds take both `set_validity` forms through `parquet_string_column%set_validity`
    !! -- one rebuild of the store rather than a `set_null` per element -- and this holds the routed
    !! forms to the per-element contract: a width-3 vector whose values differ in length (shortest
    !! first) and which already holds an element null before either call, then a scalar column the
    !! same way. The row form must null every element of a row and the element form only the
    !! elements named; neither may resurrect the null that was already there.
    subroutine test_set_validity_string_bulk(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_string_column), pointer :: sp
        character(len=6) :: svals(3, 4)
        character(len=4) :: flat(4)
        logical :: rmask(4), emask(3, 4)
        character(len=:), allocatable :: got
        !
        svals(:, 1) = ["a     ", "bb    ", "ccc   "]
        svals(:, 2) = ["dddd  ", "eeeee ", "ffffff"]
        svals(:, 3) = ["g     ", "hh    ", "iii   "]
        svals(:, 4) = ["jjjj  ", "k     ", "ll    "]
        call c%init(PK_STRING_VEC, 4_int64, 3_int32)
        call c%set_all(svals)
        call c%set_null(2_int64, 2_int64)
        rmask = [.true., .true., .true., .false.]
        call c%set_validity(rmask)
        call check(error, c%is_null(4_int64, 1_int64) .and. c%is_null(4_int64, 2_int64) .and. &
            c%is_null(4_int64, 3_int64), "a .false. row entry must null every element of that row")
        if (allocated(error)) return
        call check(error, c%is_null(2_int64, 2_int64), "the element null that was already there must survive")
        if (allocated(error)) return
        call check(error, .not. c%is_null(2_int64, 1_int64) .and. .not. c%is_null(2_int64, 3_int64), &
            "the other elements of a .true. row must stay valid")
        if (allocated(error)) return
        call c%get_elem(2_int64, 3_int64, got)
        call check(error, got == "ffffff", "the longest element must keep every byte after the rebuild")
        if (allocated(error)) return
        call c%get_elem(3_int64, 3_int64, got)
        call check(error, got == "iii", "the element just before the nulled row keeps its bytes")
        if (allocated(error)) return
        emask = .true.
        emask(1, 1) = .false.
        emask(3, 3) = .false.
        call c%set_validity(emask)
        call check(error, c%is_null(1_int64, 1_int64) .and. c%is_null(3_int64, 3_int64), &
            "the element form must null exactly the elements named")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64, 2_int64) .and. .not. c%is_null(3_int64, 2_int64), &
            "and leave their row neighbours valid")
        if (allocated(error)) return
        call c%get_elem(1_int64, 2_int64, got)
        call check(error, got == "bb", "a neighbour of a nulled element keeps its bytes")
        if (allocated(error)) return
        call c%string_column(sp)
        call check(error, sp%null_count() == 6_int64, &
            "the store must count the nulled row's three, the pre-existing one and the two named")
        if (allocated(error)) return
        call check(error, sp%validate(), "the store must satisfy its invariants after both rebuilds")
        if (allocated(error)) return
        !
        flat = ["a   ", "bb  ", "ccc ", "dddd"]
        call c%init(PK_STRING, 4_int64)
        call c%set_all(flat)
        call c%set_null(2_int64)
        call c%set_validity([.true., .true., .false., .true.])
        call check(error, c%is_null(3_int64) .and. c%is_null(2_int64) .and. .not. c%is_null(4_int64), &
            "the scalar form must null the named row, keep the existing null and leave the rest valid")
        if (allocated(error)) return
        call c%get_at(4_int64, got)
        call check(error, got == "dddd", "the last element keeps its bytes after an earlier one was dropped")
        if (allocated(error)) return
        call c%string_column(sp)
        call check(error, sp%null_count() == 2_int64 .and. sp%character_size() == 5_int64, &
            "the scalar store must hold two nulls and only the kept bytes")
    end subroutine test_set_validity_string_bulk
    !
    !> A temporal column caches "does this column hold a null?", because answering it means an O(n)
    !! element scan. The read-only view of that question (`parquet_column_any_null`, used by every
    !! bulk validity path that holds the column `intent(in)`) has two arms: READ the cache when it is
    !! clean, and rescan when it is dirty. Only the rescan had run -- so a cache that was never
    !! consulted would have looked exactly like one that was.
    !!
    !! `%any_null` is what marks the cache clean, so calling it first and then a bulk path is the
    !! sequence that reaches the read. Asserting the two agree is the point: a stale-cache read
    !! would disagree with the scan.
    subroutine test_temporal_null_cache_is_read_when_clean(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        logical, allocatable :: rowv(:)
        type(parquet_date) :: d(3)
        integer :: i
        !
        do i = 1, 3
            call d(i)%set(2026, 8, 10 + i)
        end do
        call c%init(PK_DATE, 3_int64)
        call c%set_all(d)
        call c%set_null(2_int64)
        !
        ! Marks the cache clean: any_null is the only entry point that writes it.
        call check(error, c%any_null(), "the fixture must hold a null for the cached answer to be interesting")
        if (allocated(error)) return
        !
        ! Now a bulk path, which reads the cache rather than rescanning.
        call c%row_validity(rowv)
        call check(error, allocated(rowv), &
            "row_validity must still build a mask when the null cache answers from a clean read")
        if (allocated(error)) return
        call check(error, (.not. rowv(2)) .and. rowv(1) .and. rowv(3), &
            "the mask built after a cache read must match the column's actual null state")
        if (allocated(error)) return
        !
        ! And the negative control on the same path: a column whose cache is clean and says
        ! "no nulls" must leave the mask unallocated rather than building an all-true one.
        call c%set_at(2_int64, d(2))
        call check(error, .not. c%any_null(), "overwriting the null must clear the cached answer")
        if (allocated(error)) return
        if (allocated(rowv)) deallocate(rowv)
        call c%row_validity(rowv)
        call check(error, .not. allocated(rowv), &
            "a null-free column must leave the mask unallocated, so it reaches an optional dummy as absent")
    end subroutine test_temporal_null_cache_is_read_when_clean
    !
    !> A bitmap column that has had its last null cleared one cell at a time keeps its bitmap --
    !! single-cell edits deliberately skip the O(n) scan that would let it be dropped. The question
    !! "does this column hold a null?" then has to be answered by walking the map and finding every
    !! block zero, which is the one exit from that walk nothing had taken: every other test either
    !! has no bitmap at all, or finds a set bit and returns early.
    !!
    !! `%compact_validity` is the negative control at the end: it performs the scan the cell edits
    !! skipped, so the bitmap goes away only when it is asked to.
    subroutine test_all_zero_bitmap_reports_no_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        logical, allocatable :: rowv(:)
        integer(int32) :: vals(3) = [7, 8, 9]
        integer(int32) :: got
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_all(vals)
        call c%set_null(1_int64)
        call c%set_null(3_int64)
        call check(error, c%validity_bytes() > 0_int64, "the fixture must have allocated a bitmap")
        if (allocated(error)) return
        !
        call c%clear_null(1_int64)
        call c%clear_null(3_int64)
        call check(error, c%validity_bytes() > 0_int64, &
            "clearing a null cell by cell must NOT drop the bitmap -- a single-cell edit does not pay for a scan")
        if (allocated(error)) return
        !
        call check(error, .not. c%any_null(), &
            "an allocated but all-zero bitmap must answer 'no nulls' -- the walk has to reach the end")
        if (allocated(error)) return
        call c%row_validity(rowv)
        call check(error, .not. allocated(rowv), &
            "the read-only view must agree: an all-zero bitmap leaves the row mask unallocated")
        if (allocated(error)) return
        !
        call c%compact_validity()
        call check(error, c%validity_bytes() == 0_int64, &
            "compact_validity performs the scan the cell edits skipped, so the all-zero bitmap is dropped here")
        if (allocated(error)) return
        call c%get_at(2_int64, got)
        call check(error, got == 8_int32, "none of this may disturb the values")
    end subroutine test_all_zero_bitmap_reports_no_nulls

    !> The FOURTH validity dispatch class: a container column's row nullness lives inside the
    !> container, and every query here has to ask it rather than the bitmap.
    !>
    !> This is a REGRESSION test for a shipped disagreement, not a feature test. `is_null_row`,
    !> `set_null_row` and `clear_null_row` delegated from the day container columns landed, while
    !> `any_null`, `row_validity`, `element_validity` and the element form of `is_null` fell
    !> through to a bitmap that is deliberately never allocated for a container kind -- so
    !> `%is_null(2)` answered `.true.` for the null row below while `%any_null()` answered
    !> `.false.` about the same column. Two public queries, one column, opposite answers, nothing
    !> announcing it (feature_risks.md Risk-159).
    !>
    !> **The assertions are written as AGREEMENT rather than as literals on purpose.** A test
    !> that merely checked `%any_null()` is `.true.` would pass against a query hard-wired to say
    !> so; tying each answer to the row form is what makes the four move together, and is what a
    !> fifth query added later should be held to as well.
    subroutine test_container_validity_queries_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_column) :: col, clean
        type(parquet_list_column), allocatable :: lc
        class(parquet_container_column), allocatable :: cc
        logical, allocatable :: rmask(:), emask(:,:)
        integer(int64) :: i
        logical :: ok
        !
        ! Rows 1 and 3 hold values; row 2 is an ABSENT list, not an empty one.
        allocate(lc)
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32, 3_int32])
        call lc%append_null_row()
        call lc%append_row([7_int32])
        call move_alloc(lc, cc)
        call col%adopt_container(cc)
        !
        call check(error, col%is_null(2_int64), "the row form sees the null container")
        if (allocated(error)) return
        call check(error, col%any_null(), "%any_null must agree that the column has a null")
        if (allocated(error)) return
        !
        ! The element form: a container column's width is 1, so element 1 is the only element
        ! there is and its answer must equal the row's (feature_container_phase6.md, Q3).
        ok = .true.
        do i = 1_int64, 3_int64
            if (col%is_null(i, 1_int64) .neqv. col%is_null(i)) ok = .false.
        end do
        call check(error, ok, "%is_null(i, 1) must equal %is_null(i) on a width-1 container column")
        if (allocated(error)) return
        !
        call col%row_validity(rmask)
        call check(error, allocated(rmask), "%row_validity must not report a null-free column")
        if (allocated(error)) return
        call check(error, size(rmask) == 3, "%row_validity covers every row")
        if (allocated(error)) return
        ok = .true.
        do i = 1_int64, 3_int64
            if (rmask(i) .eqv. col%is_null(i)) ok = .false.
        end do
        call check(error, ok, "%row_validity must be the negation of %is_null, row by row")
        if (allocated(error)) return
        !
        call col%element_validity(emask)
        call check(error, allocated(emask), "%element_validity must not report a null-free column")
        if (allocated(error)) return
        call check(error, size(emask, 1) == 1 .and. size(emask, 2) == 3, &
            "%element_validity is shaped (1, nrows) for a width-1 container column")
        if (allocated(error)) return
        ok = .true.
        do i = 1_int64, 3_int64
            if (emask(1, i) .eqv. col%is_null(i)) ok = .false.
        end do
        call check(error, ok, "%element_validity must agree with %is_null too")
        if (allocated(error)) return
        !
        ! The NEGATIVE CONTROL. Without it every assertion above passes against a query that
        ! answers "null" unconditionally for a container kind, which is the opposite defect and
        ! just as silent.
        deallocate(rmask)
        call build_null_free_list(clean)
        call check(error, .not. clean%is_null(1_int64), "the control column has no null row")
        if (allocated(error)) return
        call check(error, .not. clean%any_null(), "%any_null must still answer .false. with no nulls")
        if (allocated(error)) return
        call clean%row_validity(rmask)
        call check(error, .not. allocated(rmask), &
            "and %row_validity must leave the mask UNALLOCATED, which is its 'no nulls' contract")
    end subroutine test_container_validity_queries_agree

    !> A two-row list column with no null rows, for the negative control above.

    !> `gather_from` for every kind, against what it replaces on the same fixture: `deep_copy` +
    !! `gather` + `set_validity` is the oracle for the nulls, and the fixture's own code
    !! (`expect_any_row`) the oracle for the values -- independent of `gather`, which now runs the
    !! same rebuild. The index names one row twice, the source holds a null row and, on a vector
    !! kind, one null element, and the mask nulls two rows. The source is checked afterwards,
    !! untouched, and the destination must carry the source's kind, width and exact-fit size.
    subroutine test_gather_from_every_kind(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: src, dst, orc
        integer(int64), parameter :: NSRC = 9_int64
        integer(int64), parameter :: IDX(6) = [9_int64, 3_int64, 1_int64, 3_int64, 7_int64, 5_int64]
        logical, parameter :: MASK(6) = [.true., .true., .false., .true., .false., .true.]
        integer :: ki, kind
        integer(int64) :: k, e, w
        character(len=:), allocatable :: kn
        logical :: want_null
        !
        do ki = 1, size(EVERY_KIND)
            kind = EVERY_KIND(ki)
            call parquet_kind_name(kind, kn)
            call make_any_fixture(src, kind, NSRC)
            w = int(src%colwidth(), int64)
            call src%set_null(3_int64)                                ! a null row, named twice by IDX
            if (w > 1_int64) call src%set_null(5_int64, 2_int64)     ! one null ELEMENT of row 5
            !
            call src%deep_copy(orc)
            call orc%gather(IDX)
            call orc%set_validity(MASK)
            call dst%gather_from(src, IDX, valid=MASK)
            !
            call check(error, dst%length() == 6_int64, kn // ": gather_from must give the index list's length")
            if (allocated(error)) return
            call check(error, dst%kindof() == kind .and. dst%colwidth() == src%colwidth(), &
                kn // ": gather_from must carry the source's kind and width")
            if (allocated(error)) return
            if (kind /= PK_STRING .and. kind /= PK_STRING_VEC) then
                call check(error, dst%capacity() == 6_int64, kn // ": gather_from must allocate exact-fit")
                if (allocated(error)) return
            end if
            do k = 1_int64, 6_int64
                do e = 1_int64, w
                    want_null = src%is_null(IDX(k), e) .or. .not. MASK(k)
                    call check(error, dst%is_null(k, e) .eqv. want_null, &
                        kn // ": every element's null state must be the source's at idx(k) OR the mask's")
                    if (allocated(error)) return
                    call check(error, dst%is_null(k, e) .eqv. orc%is_null(k, e), &
                        kn // ": gather_from must agree with deep_copy + gather + set_validity on every null")
                    if (allocated(error)) return
                end do
                if (dst%is_null(k)) cycle
                call expect_any_row(dst, kind, k, IDX(k), kn // ": a gathered row must hold the source row idx(k)", &
                    error)
                if (allocated(error)) return
            end do
            call check(error, src%length() == NSRC .and. src%is_null(3_int64) .and. .not. src%is_null(1_int64), &
                kn // ": the source must come back untouched")
            if (allocated(error)) return
            call expect_any_row(src, kind, 9_int64, 9_int64, kn // ": the source's last row must still hold its value", &
                error)
            if (allocated(error)) return
        end do
    end subroutine test_gather_from_every_kind
    !
    !> The mask's contract, one clause per arm: an all-true mask on a null-free source allocates
    !! no bitmap; a `.true.` entry never clears a null the source row carried, while its row's
    !! other elements stay valid; a `.false.` entry nulls EVERY element of its row; a temporal
    !! kind takes the mask inside the element and its cached null answer sees it; a string kind's
    !! masked element takes a zero-width slot, so the payload holds only the kept bytes.
    subroutine test_gather_from_mask_add_only(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: src, dst
        type(parquet_string_column), pointer :: sp
        !
        call make_fixture(src, PK_INT64, 5_int64)
        call dst%gather_from(src, [2_int64, 4_int64], valid=[.true., .true.])
        call check(error, dst%validity_bytes() == 0_int64 .and. .not. dst%any_null(), &
            "an all-true mask on a null-free source must allocate no bitmap")
        if (allocated(error)) return
        !
        call make_fixture(src, PK_FLOAT64_VEC, 5_int64)
        call src%set_null(2_int64, 1_int64)
        call dst%gather_from(src, [2_int64, 3_int64], valid=[.true., .false.])
        call check(error, dst%is_null(1_int64, 1_int64) .and. .not. dst%is_null(1_int64, 2_int64), &
            "a .true. entry must carry the source's element null and leave its neighbour valid")
        if (allocated(error)) return
        call check(error, dst%is_null(2_int64, 1_int64) .and. dst%is_null(2_int64, 2_int64), &
            "a .false. entry must null every element of its row")
        if (allocated(error)) return
        call check(error, .not. src%is_null(3_int64), "the mask must not reach back into the source")
        if (allocated(error)) return
        !
        call make_fixture(src, PK_DATE, 4_int64)
        call dst%gather_from(src, [1_int64, 2_int64], valid=[.false., .true.])
        call check(error, dst%is_null(1_int64) .and. .not. dst%is_null(2_int64) .and. dst%any_null(), &
            "a temporal kind must take the mask inside the element, and its null cache must see it")
        if (allocated(error)) return
        call check(error, .not. src%is_null(1_int64), "a temporal source must come back untouched")
        if (allocated(error)) return
        !
        call src%init(PK_STRING, 3_int64)
        call src%set_all(["abc", "de ", "f  "])
        call dst%gather_from(src, [1_int64, 2_int64, 3_int64], valid=[.true., .false., .true.])
        call dst%string_column(sp)
        call check(error, dst%is_null(2_int64) .and. sp%length(2_int64) == 0_int64, &
            "a masked string element must be null and take a zero-width slot")
        if (allocated(error)) return
        call check(error, sp%character_size() == 4_int64, &
            "the string payload must hold the kept bytes only: 'abc' and 'f'")
        if (allocated(error)) return
        call src%string_column(sp)
        call check(error, sp%character_size() == 6_int64 .and. sp%null_count() == 0_int64, &
            "the string source must come back untouched")
    end subroutine test_gather_from_mask_add_only
    !
    !> The edges: a zero-row source with an empty list gives an empty column of the source's kind;
    !! a populated destination of another kind is cleared first; the int32 index form agrees with
    !! the int64 one; the unit travels; an empty list from a populated source empties the
    !! destination, mask or no mask.
    subroutine test_gather_from_edges(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: src, dst, d2
        character(len=:), allocatable :: u
        !
        call src%init(PK_INT32, 0_int64)
        call dst%gather_from(src, [integer(int64) ::])
        call check(error, dst%length() == 0_int64 .and. dst%kindof() == PK_INT32, &
            "a zero-row source and an empty list must give an empty column of the source's kind")
        if (allocated(error)) return
        !
        call make_fixture(dst, PK_FLOAT32, 7_int64)
        call make_fixture(src, PK_INT64, 4_int64)
        call dst%gather_from(src, [4_int64, 1_int64])
        call check(error, dst%kindof() == PK_INT64 .and. dst%length() == 2_int64, &
            "a populated destination of another kind must be cleared before the gather")
        if (allocated(error)) return
        call expect_fixture_row(dst, PK_INT64, 1_int64, 4_int64, "row 1 of the reused destination is source row 4", &
            error)
        if (allocated(error)) return
        call d2%gather_from(src, [4_int32, 1_int32])
        call expect_fixture_row(d2, PK_INT64, 2_int64, 1_int64, "the int32 index form must gather the same rows", &
            error)
        if (allocated(error)) return
        !
        call src%init(PK_FLOAT64, 3_int64, unit="deg")
        call src%set_all([1.0_real64, 2.0_real64, 3.0_real64])
        call dst%gather_from(src, [3_int64, 1_int64])
        call dst%unit_string(u)
        call check(error, u == "deg", "the unit must travel with the gather")
        if (allocated(error)) return
        call dst%gather_from(src, [integer(int64) ::])
        call check(error, dst%length() == 0_int64 .and. dst%kindof() == PK_FLOAT64, &
            "an empty list from a populated source must give an empty column of its kind")
        if (allocated(error)) return
        call dst%gather_from(src, [integer(int64) ::], valid=[logical ::])
        call check(error, dst%length() == 0_int64, "an empty list with an empty mask is accepted")
    end subroutine test_gather_from_edges
    !
    !> `make_fixture` for every storable kind: the string kinds are added, each element holding
    !! `fixture_text` of its row and element, so `expect_any_row` can read the expectation back.
    subroutine make_any_fixture(c, kind, nrows)
        type(parquet_column), intent(inout) :: c !! receives the fixture.
        integer, intent(in) :: kind              !! PK_* kind to build.
        integer(int64), intent(in) :: nrows      !! rows to write.
        character(len=12) :: s1, s2
        integer(int64) :: i
        !
        select case (kind)
        case (PK_STRING)
            call c%init(kind, nrows)
            do i = 1_int64, nrows
                call fixture_text(i, 1_int64, s1)
                call c%set_at(i, trim(s1))
            end do
        case (PK_STRING_VEC)
            call c%init(kind, nrows, FIXW)
            do i = 1_int64, nrows
                call fixture_text(i, 1_int64, s1)
                call fixture_text(i, 2_int64, s2)
                call c%set_at(i, [s1, s2])
            end do
        case default
            call make_fixture(c, kind, nrows)
        end select
    end subroutine make_any_fixture
    !
    !> `expect_fixture_row` for every storable kind; see `make_any_fixture`.
    subroutine expect_any_row(c, kind, i, want, what, error)
        type(parquet_column), intent(in) :: c               !! the column to read.
        integer, intent(in) :: kind                         !! its PK_* kind.
        integer(int64), intent(in) :: i                     !! row of `c` to read.
        integer(int64), intent(in) :: want                  !! fixture row it should equal.
        character(len=*), intent(in) :: what                !! context, for the message.
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=12) :: s1, s2
        character(len=:), allocatable :: got
        logical :: ok
        !
        select case (kind)
        case (PK_STRING)
            call fixture_text(want, 1_int64, s1)
            call c%get_at(i, got)
            ok = got == trim(s1)
            call check(error, ok, what)
        case (PK_STRING_VEC)
            call fixture_text(want, 1_int64, s1)
            call fixture_text(want, 2_int64, s2)
            call c%get_elem(i, 1_int64, got)
            ok = got == trim(s1)
            call c%get_elem(i, 2_int64, got)
            ok = ok .and. got == trim(s2)
            call check(error, ok, what)
        case default
            call expect_fixture_row(c, kind, i, want, what, error)
        end select
    end subroutine expect_any_row
    !
    !> The string a string fixture holds at row `i`, element `e`: `fixture_code` in text, with a
    !! length that varies with the row so that offsets are not uniform.
    subroutine fixture_text(i, e, s)
        integer(int64), intent(in) :: i        !! 1-based row index.
        integer(int64), intent(in) :: e        !! 1-based element index.
        character(len=12), intent(out) :: s    !! the text, blank-padded.
        write(s, "(a,i0)") repeat("s", int(mod(i, 3_int64)) + 1), fixture_code(i, e)
    end subroutine fixture_text
    !
    subroutine build_null_free_list(col)
        type(parquet_column), intent(out) :: col !! receives the container column.
        type(parquet_list_column), allocatable :: lc
        class(parquet_container_column), allocatable :: cc
        !
        allocate(lc)
        call lc%init(PK_INT32)
        call lc%append_row([4_int32, 5_int32])
        call lc%append_row([6_int32])
        call move_alloc(lc, cc)
        call col%adopt_container(cc)
    end subroutine build_null_free_list

end module test_columns
