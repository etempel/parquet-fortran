!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `parquet_table`'s container columns: a `parquet_list_column`, a `parquet_map_column` or a
!! `parquet_struct_column` held as a table column, read from a file or built in memory.
!!
!! **What this suite is for, beyond "does it work".** A container column is the first table column
!! whose rows differ in length, and almost every way of getting it wrong still produces a
!! plausible-looking answer: a row-structural mutation that rebuilds every other column and leaves
!! this one alone gives a table whose row COUNT is right and whose rows no longer line up, and no
!! assertion about `%nrows()` can see it. So the mutation tests here all carry an `orig_index`
!! oracle -- a scalar column whose row *k* records where row *k* came from -- and assert that the
!! container's own content still matches it. That is the one shape that distinguishes a real
!! reindex from a row-count-only pass.
!!
!! **Every refusal has a negative control beside it.** A guard that fires unconditionally passes
!! every abort test ever written for it while breaking the case it was meant to allow, so the one
!! refusal a container column still carries -- it may not be a sort key -- is asserted alongside
!! the sort by a scalar key that must still work and must still carry the container along.
!!
!! **The element-granular forms DELEGATE rather than refuse**, because a container column's
!! `width` is 1: `%is_null(name, i, e)`, `%clear_null`, `%set_null` (scalar and rank-2 mask forms)
!! and the two column-handle spellings all mean the row form. Two of those used to abort and two
!! used to succeed while changing nothing; `test_container_element_forms_delegate` pins all five,
!! asserting the state before each call as well as after.
module test_table_container
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet
    use iso_fortran_env, only : int32, int64, real64
    implicit none
    private

    public :: collect_tests_table_container

    !> The ragged plain-`LIST` fixture every read test here uses.
    !!
    !! 16 rows in 4 row groups, so a slice genuinely spans several of them, and its `scalar` column
    !! holds 0..15 -- which is the `orig_index` oracle the mutation tests need, already in the file.
    character(len=*), parameter :: LIST_FIXTURE = "test/fixtures/list_widths.parquet"

    !> The map fixture: one map column per value kind, plus `m_intkey`, whose keys are integers and
    !! which this library therefore cannot read at all.
    character(len=*), parameter :: MAP_FIXTURE = "test/fixtures/map_payloads.parquet"

    !> The NESTED fixture: a container inside a container, in every combination. Used only by
    !! `test_nested_list_is_not_a_table_column`, which pins which of those a table can hold.
    character(len=*), parameter :: NESTED_FIXTURE = "test/fixtures/map_list_types.parquet"

contains

    !> Registers every test in this suite.
    subroutine collect_tests_table_container(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("list_columns=container classifies a LIST column of an element type as PK_LIST", &
                test_list_columns_container), &
            new_unittest("list_columns=auto is unchanged, and is the default", test_list_columns_auto), &
            new_unittest("list_columns= leaves a FIXED_SIZE_LIST column alone", test_list_columns_ignores_vectors), &
            new_unittest("a slice and the whole file disagree under auto and agree under container", &
                test_slice_whole_file_agreement), &
            new_unittest("a map column is classified by its VALUE type", test_map_classification), &
            new_unittest("%col, %get, %ref and %set reach a list column", test_list_accessors), &
            new_unittest("%add_column takes all three container types", test_add_container_columns), &
            new_unittest("%get, %set and %ref reach a map and a struct column", &
                test_map_and_struct_accessors), &
            new_unittest("a container column survives the slice regime, row group by row group", &
                test_container_slice), &
            new_unittest("a map column read as a slice assembles row group by row group", &
                test_map_column_slice), &
            new_unittest("%print_stat reports a deferred-width list column as pending", &
                test_print_stat_deferred_width_list), &
            new_unittest("a container column assembles under bounded=, including emptied row groups", &
                test_container_bounded), &
            new_unittest("row-structural mutations keep a container column ALIGNED", &
                test_container_row_alignment), &
            new_unittest("%append concatenates two tables' container columns", test_container_append), &
            new_unittest("the row form of %is_null and %get_valid_mask answer for a container", &
                test_container_row_validity), &
            new_unittest("%ensure_validity is NOT refused for a container column", &
                test_container_ensure_validity), &
            new_unittest("%print_stat reports row-length extremes and stays lazy", &
                test_container_print_stat), &
            new_unittest("pf_permute reorders a container column, reached by both accessors", &
                         test_container_permute), &
            new_unittest("a table round-trips its container columns through a file", &
                test_container_write_round_trip), &
            new_unittest("a container payload's temporal resolution survives a table write", &
                test_container_write_keeps_unit), &
            new_unittest("%append_null_rows grows a container column with null rows", &
                test_container_append_null_rows), &
            new_unittest("a LIST of containers is not a table column under either token", &
                test_nested_list_is_not_a_table_column), &
            new_unittest("%copy_column and %clone carry a container column, independently", &
                test_container_copy_and_clone), &
            new_unittest("the element-granular forms delegate to the row form on a container", &
                test_container_element_forms_delegate) &
            ]
    end subroutine collect_tests_table_container

    !> Under `list_columns="container"` every plain-`LIST` column is `PK_LIST`, from the schema
    !! alone -- including one reached through a dotted struct path -- while a genuine scalar column
    !! beside them is untouched.
    !!
    !! The `scalar` clause is the negative control: without it this would pass against a
    !! classification pass that made EVERY column a container.
    subroutine test_list_columns_container(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call check(error, t%kind("ragged") == PK_LIST, "a ragged LIST column is PK_LIST")
        if (allocated(error)) return
        call check(error, t%kind("uniform") == PK_LIST, &
            "a UNIFORM LIST column is PK_LIST too: container= does not look at the data")
        if (allocated(error)) return
        call check(error, t%kind("nested.vals") == PK_LIST, "a LIST leaf of a struct is PK_LIST")
        if (allocated(error)) return
        call check(error, t%width("ragged") == 1, "a container column's width is 1")
        if (allocated(error)) return
        call check(error, t%kind("scalar") == PK_INT32, &
            "a scalar column is untouched by list_columns=container")
    end subroutine test_list_columns_container

    !> `list_columns="auto"` reproduces what every earlier release did, and is what an absent
    !! argument means.
    subroutine test_list_columns_auto(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: explicit_auto, defaulted
        !
        call parquet_open_table(explicit_auto, LIST_FIXTURE, list_columns="auto")
        call parquet_open_table(defaulted, LIST_FIXTURE)
        call check(error, explicit_auto%kind("uniform") == PK_INT32_VEC, &
            "auto: a uniform LIST column becomes a VECTOR column")
        if (allocated(error)) return
        call check(error, explicit_auto%width("uniform") == 3, "auto: and it is measured at width 3")
        if (allocated(error)) return
        call check(error, explicit_auto%kind("ragged") == PK_INT32, &
            "auto: a ragged LIST column resolves to a scalar kind")
        if (allocated(error)) return
        call check(error, defaulted%kind("uniform") == explicit_auto%kind("uniform"), &
            "the default is auto")
        if (allocated(error)) return
        call check(error, defaulted%width("uniform") == explicit_auto%width("uniform"), &
            "the default is auto, width too")
    end subroutine test_list_columns_auto

    !> `list_columns="container"` governs the plain-`LIST` case ONLY.
    !!
    !! Every column this library writes is a `FIXED_SIZE_LIST`, whose width is in the schema, so
    !! the token must not touch one -- otherwise turning it on would silently reshape a program's
    !! own files. Written here rather than read from a fixture precisely because that is what makes
    !! the column a fixed-size list.
    subroutine test_list_columns_ignores_vectors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: src, back
        integer(int32) :: v(2, 3)
        character(len=*), parameter :: f = "test_run/tc_vector_untouched.parquet"
        !
        v = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call parquet_new_table(src)
        call src%add_column("vec", v)
        call parquet_write_table(src, f)
        call parquet_open_table(back, f, list_columns="container")
        call check(error, back%kind("vec") == PK_INT32_VEC, &
            "a FIXED_SIZE_LIST column stays a vector column under list_columns=container")
        if (allocated(error)) return
        call check(error, back%width("vec") == 2, "and keeps its schema-declared width")
    end subroutine test_list_columns_ignores_vectors

    !> **feature_risks.md Risk-152.** Under `"auto"`, a slice and the whole file legitimately
    !! disagree about the same column's kind, because the width is measured over the rows each one
    !! covers. `"container"` is what makes them agree.
    !!
    !! `late` is uniform (length 3) in row groups 1-3 and ragged only in the last, so rows 1..12
    !! are a uniform vector column while the file as a whole is not -- which is exactly the shape
    !! the risk is about.
    !!
    !! **When the 3.0.0 default flip lands** (`"container"` becoming the default), the first two
    !! assertions become the behaviour of an explicit `list_columns="auto"` rather than of a plain
    !! open, and the last two become the default. Do not delete them: the disagreement is still
    !! real under `"auto"`, which is why Risk-152 is not closed by the flip either.
    subroutine test_slice_whole_file_agreement(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: whole, part
        !
        call parquet_open_table(whole, LIST_FIXTURE, list_columns="auto")
        call parquet_open_table(part, LIST_FIXTURE, 1_int64, 12_int64, list_columns="auto")
        call check(error, whole%kind("late") == PK_INT32, &
            "auto, whole file: `late` is ragged overall, so it resolves to a scalar kind")
        if (allocated(error)) return
        call check(error, part%kind("late") == PK_INT32_VEC .and. part%width("late") == 3, &
            "auto, rows 1..12: the same column is a uniform vector of width 3 -- they DISAGREE")
        if (allocated(error)) return
        call parquet_open_table(whole, LIST_FIXTURE, list_columns="container")
        call parquet_open_table(part, LIST_FIXTURE, 1_int64, 12_int64, list_columns="container")
        call check(error, whole%kind("late") == PK_LIST, "container, whole file: PK_LIST")
        if (allocated(error)) return
        call check(error, part%kind("late") == PK_LIST, &
            "container, rows 1..12: PK_LIST too -- the disagreement is gone")
    end subroutine test_slice_whole_file_agreement

    !> A map column is supported when its keys are strings and its value type is one of the nine,
    !! and unsupported otherwise -- decided from the schema, with no read.
    !!
    !! `m_intkey` is the negative control in both directions: it keeps this from passing against a
    !! pass that accepted every map, and the readable maps beside it keep it from passing against
    !! one that refused every map.
    subroutine test_map_classification(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        !
        call parquet_open_table(t, MAP_FIXTURE)
        call check(error, t%is_supported("m_int32"), "a string-keyed int32 map is supported")
        if (allocated(error)) return
        call check(error, t%kind("m_int32") == PK_MAP, "and reports PK_MAP")
        if (allocated(error)) return
        call check(error, t%kind("m_timestamp") == PK_MAP, "a timestamp-valued map reports PK_MAP")
        if (allocated(error)) return
        call check(error, .not. t%is_supported("m_intkey"), &
            "a map with integer keys is not readable, so it is not supported")
        if (allocated(error)) return
        call check(error, t%residency("m_int32") == RES_EMPTY, &
            "classification reads nothing: a map column starts empty like any other")
    end subroutine test_map_classification

    !> The five accessor families over a list column, and the independence `%get` promises.
    subroutine test_list_accessors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        type(parquet_list_column), pointer :: p
        type(parquet_list_column) :: copy
        type(parquet_table_col) :: h
        integer(int64) :: n
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%col("ragged", p)
        call check(error, associated(p), "%col hands back a live pointer")
        if (allocated(error)) return
        n = p%nrows()
        call check(error, n == t%nrows(), "%col's container holds one row per table row")
        if (allocated(error)) return
        ! ragged's lengths cycle 1,2,3,4 -- distinct within each group of four, which is what lets
        ! a shifted read be told from a correct one.
        call check(error, p%length(1_int64) == 1 .and. p%length(2_int64) == 2 .and. &
            p%length(3_int64) == 3 .and. p%length(4_int64) == 4, &
            "%col's container reports each row's own length")
        if (allocated(error)) return
        call t%get("ragged", copy)
        call check(error, copy%nrows() == n, "%get copies every row")
        if (allocated(error)) return
        call check(error, copy%length(4_int64) == 4, "%get's copy has the right lengths")
        if (allocated(error)) return
        ! INDEPENDENCE: mutating the copy must not touch the table.
        call copy%set_null(1_int64)
        call check(error, copy%is_null(1_int64), "the copy took the null")
        if (allocated(error)) return
        call check(error, .not. p%is_null(1_int64), "%get is a genuine copy: the table is untouched")
        if (allocated(error)) return
        call t%column("ragged", h)
        call h%ref(p)
        call check(error, p%nrows() == n, "%ref reaches the same column through a handle")
        if (allocated(error)) return
        ! %set replaces the values; the row count must already match.
        call t%set("ragged", copy)
        call t%col("ragged", p)
        call check(error, p%is_null(1_int64), "%set wrote the copy's nulls into the table")
        if (allocated(error)) return
        call check(error, p%nrows() == n, "%set does not change the row count")
    end subroutine test_list_accessors

    !> `%add_column` builds an in-memory table out of all three container types.
    !!
    !! A struct column can only ever arrive this way -- `parquet_get_column_names` expands a
    !! top-level struct into one dotted path per leaf, so a struct is never enumerated under its
    !! own name and no file-backed table ever classifies one.
    subroutine test_add_container_columns(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        type(parquet_list_column) :: lc
        type(parquet_map_column) :: mc
        type(parquet_struct_column) :: sc
        type(parquet_list_column), pointer :: lp
        type(parquet_struct_column), pointer :: sp
        character(len=8) :: fields(2)
        integer :: kinds(2)
        !
        call lc%init(PK_INT32)
        call lc%append_row([10_int32, 20_int32])
        call lc%append_null_row()
        call lc%append_row([30_int32, 40_int32, 50_int32])
        call mc%init(PK_FLOAT64)
        call mc%append_row(["a", "b"], [1.5_real64, 2.5_real64])
        call mc%append_null_row()
        call mc%append_row(["c"], [3.5_real64])
        fields(1) = "f1"
        fields(2) = "f2"
        kinds(1) = PK_INT32
        kinds(2) = PK_FLOAT64
        call sc%init(fields, kinds, 3_int64)
        !
        call parquet_new_table(t)
        call t%add_column("lst", lc)
        call t%add_column("mp", mc)
        call t%add_column("st", sc)
        call check(error, t%ncols() == 3, "three container columns were added")
        if (allocated(error)) return
        call check(error, t%nrows() == 3, "and they fixed the table's row count")
        if (allocated(error)) return
        call check(error, t%kind("lst") == PK_LIST .and. t%kind("mp") == PK_MAP .and. &
            t%kind("st") == PK_STRUCT, "each reports its own container kind")
        if (allocated(error)) return
        call check(error, t%residency("lst") == RES_FULL, "an added column is resident")
        if (allocated(error)) return
        call t%col("lst", lp)
        call check(error, lp%length(3_int64) == 3, "%add_column copied the rows in")
        if (allocated(error)) return
        call check(error, lp%is_null(2_int64), "and their nullness")
        if (allocated(error)) return
        ! INDEPENDENCE, the same promise %get makes in the other direction.
        call lc%set_null(1_int64)
        call check(error, .not. lp%is_null(1_int64), &
            "%add_column copies: mutating the source afterwards does not reach the table")
        if (allocated(error)) return
        call t%col("st", sp)
        call check(error, sp%nrows() == 3, "the struct column is reachable through %col too")
    end subroutine test_add_container_columns

    !> `%get`, `%set` and `%ref` over a MAP and a STRUCT column.
    !!
    !! `test_list_accessors` above proves the three bindings for the list container only, and the
    !! map and struct specifics are not that code reached with a different tag: each one names its
    !! own `PK_*` in the `table_require_kind` guard, its own `container_as_*` narrowing, and its
    !! own `select type` arm after `clone_into`. A specific narrowing to the wrong container type
    !! would leave the `select type` unmatched and hand back an EMPTY column rather than aborting
    !! -- a silent failure, which is why each assertion below checks the payload and not only that
    !! the call returned.
    !!
    !! The independence assertions are the other half: `%get` promises a genuine copy and `%set` a
    !! genuine write, so each is checked by mutating one side and reading the other.
    subroutine test_map_and_struct_accessors(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        type(parquet_map_column) :: mc, mcopy
        type(parquet_struct_column) :: sc, scopy
        type(parquet_map_column), pointer :: mp
        type(parquet_struct_column), pointer :: sp
        type(parquet_table_col) :: h
        character(len=8) :: fields(2)
        integer :: kinds(2)
        !
        call mc%init(PK_FLOAT64)
        call mc%append_row(["a", "b"], [1.5_real64, 2.5_real64])
        call mc%append_null_row()
        call mc%append_row(["c"], [3.5_real64])
        fields(1) = "f1"
        fields(2) = "f2"
        kinds(1) = PK_INT32
        kinds(2) = PK_FLOAT64
        call sc%init(fields, kinds, 3_int64)
        ! %init creates the rows NULL. Rows 1 and 3 are cleared and given a value so the
        ! source has MIXED nullness: against an all-null column the assertions below could
        ! not tell a faithful copy from one that carried no payload at all.
        call sc%clear_null_row(1_int64)
        call sc%clear_null_row(3_int64)
        call sc%set_field(1_int64, "f1", 7_int32)
        call sc%set_field(3_int64, "f1", 9_int32)
        !
        call parquet_new_table(t)
        call t%add_column("mp", mc)
        call t%add_column("st", sc)

        ! ---- %get: a copy that carries the payload ----
        call t%get("mp", mcopy)
        call check(error, mcopy%nrows() == 3_int64, "%get copies every row of a map column")
        if (allocated(error)) return
        call check(error, mcopy%is_null_row(2_int64) .and. .not. mcopy%is_null_row(1_int64), &
            "and its nullness -- an empty column would answer this differently")
        if (allocated(error)) return
        call t%get("st", scopy)
        call check(error, scopy%nrows() == 3_int64, "%get copies every row of a struct column")
        if (allocated(error)) return
        call check(error, scopy%is_null_row(2_int64) .and. .not. scopy%is_null_row(1_int64), &
            "and its nullness")
        if (allocated(error)) return

        ! ---- INDEPENDENCE: mutating the copy must not reach the table ----
        call mcopy%set_null_row(1_int64)
        call scopy%set_null_row(1_int64)
        call t%col("mp", mp)
        call t%col("st", sp)
        call check(error, .not. mp%is_null_row(1_int64), &
            "%get on a map column is a genuine copy: the table is untouched")
        if (allocated(error)) return
        call check(error, .not. sp%is_null_row(1_int64), &
            "%get on a struct column is a genuine copy too")
        if (allocated(error)) return

        ! ---- %ref through a column handle ----
        call t%column("mp", h)
        call h%ref(mp)
        call check(error, associated(mp) .and. mp%nrows() == 3_int64, &
            "%ref reaches the map column through a handle")
        if (allocated(error)) return
        call t%column("st", h)
        call h%ref(sp)
        call check(error, associated(sp) .and. sp%nrows() == 3_int64, &
            "%ref reaches the struct column through a handle")
        if (allocated(error)) return

        ! ---- %set: the copy's nulls land in the table ----
        call t%set("mp", mcopy)
        call t%set("st", scopy)
        call t%col("mp", mp)
        call t%col("st", sp)
        call check(error, mp%is_null_row(1_int64) .and. mp%nrows() == 3_int64, &
            "%set wrote the map copy's null in, without changing the row count")
        if (allocated(error)) return
        call check(error, sp%is_null_row(1_int64) .and. sp%nrows() == 3_int64, &
            "%set wrote the struct copy's null in, without changing the row count")
    end subroutine test_map_and_struct_accessors

    !> A container column in the SLICE regime, assembled from several row groups.
    !!
    !! `materialize_slice` cannot `%paste` a container (there are no fixed-width row slots to
    !! overwrite), so it takes a third assembly path: the first covering row group is moved in
    !! whole and the rest are appended. This asserts the result against the whole-file read of the
    !! same rows, which is the only oracle that catches a row group being dropped, doubled or
    !! mis-trimmed.
    subroutine test_container_slice(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: whole, part
        type(parquet_list_column), pointer :: pw, pp
        integer(int64) :: k, lo, hi
        logical :: same
        !
        call parquet_open_table(whole, LIST_FIXTURE, list_columns="container")
        call whole%col("ragged", pw)
        ! Rows 3..14 span all four row groups and start and end MID-row-group, so both trims run.
        lo = 3_int64
        hi = 14_int64
        call parquet_open_table(part, LIST_FIXTURE, lo, hi, list_columns="container")
        call part%col("ragged", pp)
        call check(error, pp%nrows() == hi - lo + 1_int64, "the slice holds exactly its own rows")
        if (allocated(error)) return
        same = .true.
        do k = 1_int64, pp%nrows()
            if (pp%length(k) /= pw%length(lo + k - 1_int64)) same = .false.
            if (pp%is_null(k) .neqv. pw%is_null(lo + k - 1_int64)) same = .false.
        end do
        call check(error, same, "every slice row matches the same row of the whole-file read")
    end subroutine test_container_slice

    !> A MAP column read as a SLICE, which is the row-group-at-a-time assembly path.
    !!
    !! `test_container_slice` above drives that path for a list column. A map column reaches a
    !! DIFFERENT chunk reader -- `matchunk_map`, which allocates a `parquet_map_column` for the
    !! chunk and narrows to it with its own `select type` -- and the kind switch that dispatches to
    !! it has its own `case (PK_MAP)` arm. Neither runs on the whole-file read the map tests above
    !! use, because that path assembles the column in one piece.
    !!
    !! The assertion is per row against the whole-file read, not a row count: a chunk reader that
    !! narrowed to the wrong container type would leave the `select type` unmatched and assemble a
    !! column of the right LENGTH holding nothing.
    subroutine test_map_column_slice(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: whole, part
        type(parquet_map_column), pointer :: pw, pp
        integer(int64) :: k, lo, hi, n
        logical :: same
        !
        call parquet_open_table(whole, MAP_FIXTURE)
        call whole%col("m_int32", pw)
        n = pw%nrows()
        call check(error, n >= 3_int64, "the fixture must have rows to slice")
        if (allocated(error)) return
        ! Two slices, each trimming one end, because the fixture's rows are not interchangeable:
        ! row 1 carries entries, row 2 is a null map and row 3 is present but empty. A slice that
        ! avoided row 1 would compare three rows that are all empty and pass against a chunk
        ! reader that assembled nothing at all.
        lo = 1_int64
        hi = n - 1_int64
        call parquet_open_table(part, MAP_FIXTURE, lo, hi)
        call part%col("m_int32", pp)
        call check(error, pp%nrows() == hi - lo + 1_int64, "the slice holds exactly its own rows")
        if (allocated(error)) return
        same = .true.
        do k = 1_int64, pp%nrows()
            if (pp%length(k) /= pw%length(lo + k - 1_int64)) same = .false.
            if (pp%is_null(k) .neqv. pw%is_null(lo + k - 1_int64)) same = .false.
        end do
        call check(error, same, "every slice row matches the same row of the whole-file read")
        if (allocated(error)) return
        call check(error, pp%length(1_int64) > 0_int64, &
            "and the carried row really holds its entries -- the guard against an empty assembly")
        if (allocated(error)) return
        ! The other end: a slice starting past row 1, so the LEADING trim runs.
        lo = 2_int64
        hi = n
        call parquet_open_table(part, MAP_FIXTURE, lo, hi)
        call part%col("m_int32", pp)
        call check(error, pp%nrows() == hi - lo + 1_int64, "the trailing slice holds its own rows")
        if (allocated(error)) return
        same = .true.
        do k = 1_int64, pp%nrows()
            if (pp%length(k) /= pw%length(lo + k - 1_int64)) same = .false.
            if (pp%is_null(k) .neqv. pw%is_null(lo + k - 1_int64)) same = .false.
        end do
        call check(error, same, "every row of the trailing slice matches the whole-file read too")
        if (allocated(error)) return
        call check(error, any([(pp%length(k) > 0_int64, k = 1_int64, pp%nrows())]), &
            "and it too carries a row with entries")
    end subroutine test_map_column_slice

    !> `%print_stat` reports a DEFERRED-WIDTH list column as pending, in both report layouts.
    !!
    !! A plain LIST column read without `list_columns="container"` has a width that only the data
    !! knows, and measuring it would read the column -- which a report must never do. So the slot
    !! is marked `width_pending` and the report prints "pending" for it. There are two layouts,
    !! with the min/max/nulls columns and without, each with its own `print` statement and its own
    !! column widths, and `stats=.false.` is the only way to reach the narrow one.
    !!
    !! The residency check afterwards is the point of the whole arm: reporting must leave the
    !! column exactly as unread as it found it.
    subroutine test_print_stat_deferred_width_list(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        !
        ! Deliberately WITHOUT list_columns="container": that token is what would give the column
        ! a known width and take it off this path entirely.
        call parquet_open_table(t, LIST_FIXTURE)
        call check(error, t%residency("ragged") == RES_EMPTY, &
            "the list column must start unread for its width to still be deferred")
        if (allocated(error)) return
        call t%print_stat(stats=.false., all=.true.)
        call t%print_stat(all=.true.)
        call check(error, t%residency("ragged") == RES_EMPTY, &
            "%print_stat must not read the column to report that its width is pending")
    end subroutine test_print_stat_deferred_width_list


    !> A container column under `bounded=.true.`, including the case a chunked assembly is most
    !! likely to get wrong: a covering row group the filter empties ENTIRELY.
    !!
    !! A container takes the third assembly shape -- the first covering row group is moved in whole
    !! and settles the payload kind, the rest are appended -- so an emptied row group anywhere in
    !! the file exercises a case the paste and append shapes do not. The FIRST row group being the
    !! emptied one is the sharpest of these, because that is where the column is built: it works
    !! because the list read fixes the payload kind from the file schema before appending any row,
    !! so a zero-row chunk is a TYPED empty container rather than an untyped one.
    !!
    !! The oracle is the whole-file read of the same rows through the default engine, which is the
    !! only one that catches a row group being dropped, doubled or mis-trimmed.
    subroutine test_container_bounded(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: whole
        type(parquet_list_column), pointer :: pw
        !
        ! The fixture is 4 row groups of 4 rows; `scalar` runs 0..15 and `ragged`'s lengths cycle
        ! 1,2,3,4 with the row index, so the survivors' own scalar values say which file rows they
        ! were and what length each must have.
        call parquet_open_table(whole, LIST_FIXTURE, list_columns="container")
        call whole%col("ragged", pw)
        !
        call one_bounded_container_case(error, "scalar >= 4", "first row group emptied")
        if (allocated(error)) return
        call one_bounded_container_case(error, "scalar < 4 or scalar >= 8", "middle row group emptied")
        if (allocated(error)) return
        call one_bounded_container_case(error, "scalar < 12", "last row group emptied")
        if (allocated(error)) return
        call one_bounded_container_case(error, "scalar >= 8", "first two row groups emptied")
        if (allocated(error)) return
        call one_bounded_container_case(error, "scalar == 5", "all but one row emptied")
    end subroutine test_container_bounded

    !> One filter of `test_container_bounded`, checked against the default engine on the same
    !! filter -- so a disagreement is the bounded assembly's, not the filter's.
    subroutine one_bounded_container_case(error, rule, what)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        character(len=*), intent(in) :: rule !! filter rule to apply.
        character(len=*), intent(in) :: what !! what this case is, for the failure message.
        type(parquet_table) :: plain, bnd
        type(parquet_filter) :: filt
        type(parquet_list_column), pointer :: pp, pb
        integer(int64) :: k
        logical :: same
        !
        call filt%add(rule)
        call parquet_open_table(plain, LIST_FIXTURE, filter=filt, list_columns="container")
        call parquet_open_table(bnd, LIST_FIXTURE, filter=filt, list_columns="container", &
            bounded=.true.)
        call check(error, bnd%nrows() == plain%nrows(), &
            "a bounded container table should hold the same rows as the default engine (" // what // ")")
        if (allocated(error)) return
        call plain%col("ragged", pp)
        call bnd%col("ragged", pb)
        call check(error, pb%nrows() == pp%nrows(), &
            "a bounded container column should hold the same row count (" // what // ")")
        if (allocated(error)) return
        same = .true.
        do k = 1_int64, pp%nrows()
            if (pb%length(k) /= pp%length(k)) same = .false.
            if (pb%is_null(k) .neqv. pp%is_null(k)) same = .false.
        end do
        call check(error, same, &
            "every bounded container row should match the default engine's (" // what // ")")
    end subroutine one_bounded_container_case

    !> **The alignment test.** Every row-structural mutation must rebuild a container column along
    !! with every other one, and asserting the ROW COUNT cannot see it when they do not.
    !!
    !! The oracle is the fixture's own `scalar` column, whose row *k* holds *k-1*. After each
    !! mutation the surviving rows' `scalar` values say where each row came from, and the
    !! container's row length must be what that original row's length was. A column that was left
    !! entirely unrebuilt still has the right number of rows and passes every count assertion.
    !!
    !! Mutation-test it by deleting `gather_storage`'s container arm: this must fail, and it is the
    !! only test that distinguishes a real reindex from a row-count-only pass.
    subroutine test_container_row_alignment(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        integer(int64), allocatable :: want(:)
        logical, allocatable :: keep(:)
        integer :: k
        !
        ! ragged's lengths cycle 1,2,3,4 with the row index, so row k (1-based) has length
        ! mod(k-1, 4) + 1 -- which `scalar` (0..15) indexes directly.
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%materialize_all()
        call check_alignment(error, t, "before any mutation")
        if (allocated(error)) return
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%materialize_all()
        call t%sort_by(["scalar"], descending=[.true.])
        call check_alignment(error, t, "after %sort_by(descending)")
        if (allocated(error)) return
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%materialize_all()
        allocate(keep(t%nrows()))
        keep = .false.
        do k = 1, int(t%nrows())
            if (mod(k, 3) == 0) keep(k) = .true.
        end do
        call t%filter_rows(keep)
        deallocate(keep)
        call check_alignment(error, t, "after %filter_rows")
        if (allocated(error)) return
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%materialize_all()
        call t%delete_rows([2_int64, 5_int64, 11_int64])
        call check_alignment(error, t, "after %delete_rows")
        if (allocated(error)) return
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%materialize_all()
        call t%truncate(6_int64)
        call check_alignment(error, t, "after %truncate")
        if (allocated(error)) return
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%materialize_all()
        call t%top_n(["scalar"], 5, descending=[.true.])
        call check_alignment(error, t, "after %top_n")
        if (allocated(error)) return
        allocate(want(0))
        deallocate(want)
    end subroutine test_container_row_alignment

    !> Asserts that every surviving row's container length still matches the row its `scalar`
    !! oracle says it came from.
    subroutine check_alignment(error, t, what)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table), intent(inout) :: t             !! the mutated table.
        character(len=*), intent(in) :: what                !! which mutation, for the message.
        type(parquet_list_column), pointer :: p
        integer(int32), pointer :: oracle(:)
        integer(int64) :: k
        integer :: want
        logical :: aligned
        !
        call t%col("ragged", p)
        call t%col("scalar", oracle)
        call check(error, p%nrows() == t%nrows(), &
            "the container has one row per table row " // what)
        if (allocated(error)) return
        call check(error, size(oracle, kind=int64) == t%nrows(), &
            "the oracle column has one row per table row " // what)
        if (allocated(error)) return
        aligned = .true.
        do k = 1_int64, t%nrows()
            ! Row (oracle+1) of the FILE had length mod(oracle, 4) + 1.
            want = int(mod(oracle(k), 4_int32)) + 1
            if (p%length(k) /= int(want, int64)) aligned = .false.
        end do
        call check(error, aligned, "every container row still matches its oracle " // what)
    end subroutine check_alignment

    !> `%append` concatenates two tables, container columns included -- the one storage primitive
    !! Phase 6 had to add (`append_storage` used to abort for a container kind).
    subroutine test_container_append(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: a, b
        type(parquet_list_column), pointer :: p
        integer(int64) :: n_a, n_b
        !
        call parquet_open_table(a, LIST_FIXTURE, 1_int64, 6_int64, list_columns="container")
        call a%materialize_all()
        call parquet_open_table(b, LIST_FIXTURE, 7_int64, 16_int64, list_columns="container")
        call b%materialize_all()
        n_a = a%nrows()
        n_b = b%nrows()
        call a%append(b)
        call check(error, a%nrows() == n_a + n_b, "the appended table holds both row sets")
        if (allocated(error)) return
        call a%col("ragged", p)
        call check(error, p%nrows() == n_a + n_b, "and so does the container column")
        if (allocated(error)) return
        ! Rows 1..16 of the file have lengths cycling 1,2,3,4 -- so the join at row 7 is exactly
        ! where an unrebased offset would show up.
        call check(error, p%length(6_int64) == 2 .and. p%length(7_int64) == 3 .and. &
            p%length(8_int64) == 4, &
            "the rows either side of the join keep their own lengths: the offsets were rebased")
    end subroutine test_container_append

    !> The ROW forms of `%is_null` and `%get_valid_mask` answer for a container column.
    !!
    !! **These are the negative controls** for the two refusals in `test/error_scenarios.f90`
    !! (`table_container_is_null_element`, `table_container_valid_mask_rank2`): without them, a
    !! guard that refused every form of both queries would pass both abort scenarios while making
    !! a container column's nullness unaskable.
    subroutine test_container_row_validity(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        logical, allocatable :: mask(:)
        logical, allocatable :: elem_mask(:,:)
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%materialize_all()
        ! with_null's row 6 is a NULL list; every other row is present.
        call check(error, t%is_null("with_null", 6_int64), &
            "the row form of %is_null answers .true. for a null container row")
        if (allocated(error)) return
        call check(error, .not. t%is_null("with_null", 1_int64), &
            "and .false. for a present one")
        if (allocated(error)) return
        call t%get_valid_mask("with_null", mask)
        call check(error, size(mask, kind=int64) == t%nrows(), &
            "the rank-1 mask has one entry per row")
        if (allocated(error)) return
        call check(error, .not. mask(6) .and. mask(1), &
            "and marks exactly the null row invalid")
        if (allocated(error)) return
        call check(error, count(.not. mask) == 1, "with_null holds exactly one null row")
        if (allocated(error)) return
        ! The element forms must still answer on an ORDINARY column in the same table, shaped by
        ! that column's real width. This half was written when the container's element forms were
        ! refused, as the control showing the guard did not fire for every column; it is kept now
        ! that they delegate, because it is what distinguishes a genuine (width, nrows) mask from
        ! the degenerate (1, nrows) one a container gets.
        call check(error, .not. t%is_null("scalar", 1_int64, 1_int64), &
            "the ELEMENT form of %is_null still answers on a scalar column")
        if (allocated(error)) return
        call t%get_valid_mask("scalar", elem_mask)
        call check(error, size(elem_mask, 1, kind=int64) == 1_int64 .and. &
            size(elem_mask, 2, kind=int64) == t%nrows(), &
            "and the rank-2 %get_valid_mask still answers, shaped (width, nrows)")
        if (allocated(error)) return
        call check(error, all(elem_mask), "with no nulls in it")
    end subroutine test_container_row_validity

    !> `%ensure_validity` must NOT refuse a container column.
    !!
    !! It is the documented escape hatch for the first-null race, so refusing it would leave a
    !! container column with no way to be made safe for concurrent nulling. A passing suite proves
    !! nothing about a refusal nobody wrote, which is why this is asserted rather than assumed.
    subroutine test_container_ensure_validity(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        type(parquet_list_column), pointer :: p
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%materialize_all()
        call t%col("ragged", p)
        call check(error, .not. p%has_validity_storage(), &
            "a null-free container column has no validity storage yet")
        if (allocated(error)) return
        call t%ensure_validity("ragged")
        call t%col("ragged", p)
        call check(error, p%has_validity_storage(), &
            "%ensure_validity allocated it, rather than refusing the column")
        if (allocated(error)) return
        call check(error, .not. p%is_null(1_int64), "and marked nothing null in doing so")
    end subroutine test_container_ensure_validity

    !> `%print_stat` reads nothing on a table whose container columns are still lazy.
    !!
    !! **This half only.** The min/max VALUES the container arm computes are asserted
    !! out-of-process, by `test/error_scenarios.f90`'s `container_print_stat_lengths` scenario and
    !! its `test/test_errors.f90` wrapper: `%print_stat` writes to stdout and there is no in-process
    !! way to capture that (`parquet_set_message_stream` takes `"stdout"`/`"stderr"` and no file),
    !! so a test here could only assert that calling it did not crash.
    !!
    !! Laziness IS assertable here, and is the property most easily broken by an edit: the lengths
    !! come from the container's own offsets, which are resident whenever the column is, and a stat
    !! routine that reached for a read instead would silently turn every `%print_stat` on a lazy
    !! table into a full materialization.
    subroutine test_container_print_stat(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        !
        call parquet_open_table(t, LIST_FIXTURE, list_columns="container")
        call t%print_stat()
        call check(error, t%residency("ragged") == RES_EMPTY, &
            "%print_stat read nothing: the column is still empty")
        if (allocated(error)) return
        call t%print_stat(all=.true.)
        call check(error, t%residency("ragged") == RES_EMPTY, &
            "%print_stat(all=.true.) reads nothing either")
        if (allocated(error)) return
        call t%materialize_all()
        call t%print_stat()
        call check(error, t%residency("ragged") == RES_FULL, &
            "and it summarizes a resident container column without disturbing it")
    end subroutine test_container_print_stat

    !> `pf_permute` reorders a container column, through the same `gather_storage` arm every
    !! row-structural mutation uses -- so this needed no implementation, only an assertion.
    !!
    !! Built as a standalone `parquet_column` rather than taken from a table: `pf_permute`'s
    !! column specific takes a `parquet_column`, and a table hands out the concrete container
    !! (`%col`) rather than the column wrapping it. The table's own reordering is covered by
    !! `test_container_row_alignment`, which goes through `%sort_by` and its siblings.
    !!
    !! The standalone column is also where the two routes to the embedded container meet --
    !! `parquet_column_container` and the `%container_ptr` binding that forwards onto it -- so
    !! their aliasing is asserted here rather than in a fixture of its own.
    subroutine test_container_permute(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        ! `target` because both routes below hand back a pointer INTO this variable: F2018
        ! 15.5.2.4 leaves such a pointer undefined on return when the actual has no `target`,
        ! whatever a given compiler happens to do with it.
        type(parquet_column), target :: col
        type(parquet_list_column) :: lc
        class(parquet_container_column), allocatable :: box
        class(parquet_container_column), pointer :: back, bound
        integer(int64) :: perm(4)
        logical :: same_row_count
        !
        ! Four rows of distinct lengths, so a permutation is fully observable -- a fixture whose
        ! rows were the same length could not tell a correct reorder from none at all.
        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        call lc%append_row([1_int32, 2_int32])
        call lc%append_row([1_int32, 2_int32, 3_int32])
        call lc%append_row([1_int32, 2_int32, 3_int32, 4_int32])
        call lc%clone_into(box)
        call col%adopt_container(box)
        call check(error, col%kindof() == PK_LIST, "the column holds the list")
        if (allocated(error)) return
        perm = [4_int64, 3_int64, 2_int64, 1_int64]
        call pf_permute(col, perm)
        call parquet_column_container(col, back)
        ! The type-bound spelling is a one-line forwarder onto the free procedure, so the two must
        ! alias the SAME storage rather than merely agree about it -- a forwarder that built a copy
        ! would answer every question below identically and still be wrong.
        call col%container_ptr(bound)
        call check(error, associated(bound, back), &
            "%container_ptr must alias exactly what parquet_column_container hands back")
        if (allocated(error)) return
        select type (bound)
        type is (parquet_list_column)
            same_row_count = bound%nrows() == 4_int64
        class default
            same_row_count = .false.
        end select
        call check(error, same_row_count, "and it is still the list column, through that route too")
        if (allocated(error)) return
        select type (back)
        type is (parquet_list_column)
            call check(error, back%nrows() == 4_int64, "pf_permute kept the row count")
            if (allocated(error)) return
            call check(error, back%length(1_int64) == 4 .and. back%length(2_int64) == 3 .and. &
                back%length(3_int64) == 2 .and. back%length(4_int64) == 1, &
                "and reversed the rows")
        class default
            call check(error, .false., "the permuted column is still a list column")
        end select
    end subroutine test_container_permute

    !> A table's container columns survive `parquet_write_table` and a reopen.
    subroutine test_container_write_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: src, back
        type(parquet_list_column), pointer :: p
        type(parquet_map_column), pointer :: mp
        integer(int64) :: k
        logical :: same
        character(len=*), parameter :: f = "test_run/tc_round_trip.parquet"
        character(len=*), parameter :: g = "test_run/tc_round_trip_map.parquet"
        !
        call parquet_open_table(src, LIST_FIXTURE, list_columns="container")
        call src%materialize_all()
        call parquet_write_table(src, f)
        call parquet_open_table(back, f, list_columns="container")
        call back%materialize_all()
        call check(error, back%nrows() == src%nrows(), "the round trip kept every row")
        if (allocated(error)) return
        call check(error, back%kind("ragged") == PK_LIST, "and the column is still a list")
        if (allocated(error)) return
        call back%col("ragged", p)
        same = .true.
        do k = 1_int64, back%nrows()
            if (p%length(k) /= int(mod(k - 1_int64, 4_int64) + 1_int64, int64)) same = .false.
        end do
        call check(error, same, "every row kept its own length")
        if (allocated(error)) return
        !
        call parquet_open_table(src, MAP_FIXTURE)
        call src%materialize_all()
        call parquet_write_table(src, g)
        call parquet_open_table(back, g)
        call back%materialize_all()
        call check(error, back%kind("m_int32") == PK_MAP, "a map column round-trips as a map")
        if (allocated(error)) return
        call back%col("m_int32", mp)
        call check(error, mp%nrows() == back%nrows(), "with one entry set per row")
        if (allocated(error)) return
        ! m_intkey is unreadable, so it is not written -- one column fewer, not an abort.
        call check(error, back%ncols() == src%ncols() - 1, &
            "the unreadable map column was skipped rather than aborting the write")
    end subroutine test_container_write_round_trip

    !> A container payload's TEMPORAL RESOLUTION survives a table write.
    !!
    !! The writer's default is microseconds, so a `list[timestamp[ms]]` column would silently come
    !! back as microseconds if the resolution were not recorded at classification and emitted in
    !! the schema token. `stamp` is milliseconds in the fixture, which is what makes this test able
    !! to fail; `clock` is microseconds and is the control that would pass either way.
    subroutine test_container_write_keeps_unit(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: src, back
        type(parquet_reader) :: r
        character(len=:), allocatable :: tz
        integer :: unit_before, unit_after
        character(len=*), parameter :: src_f = "test/fixtures/list_payloads.parquet"
        character(len=*), parameter :: out_f = "test_run/tc_unit_round_trip.parquet"
        !
        call parquet_open_reader(r, src_f)
        call parquet_get_column_time_info(r, "stamp", unit_before, tz)
        call parquet_close_reader(r)
        call check(error, unit_before == parquet_unit_millis, &
            "the fixture's `stamp` list is milliseconds, which is what makes this test able to fail")
        if (allocated(error)) return
        !
        call parquet_open_table(src, src_f, list_columns="container")
        call src%materialize_all()
        call parquet_write_table(src, out_f)
        call parquet_open_reader(r, out_f)
        call parquet_get_column_time_info(r, "stamp", unit_after, tz)
        call parquet_close_reader(r)
        call check(error, unit_after == parquet_unit_millis, &
            "the written list column kept milliseconds rather than defaulting to microseconds")
        if (allocated(error)) return
        call parquet_open_table(back, out_f, list_columns="container")
        call check(error, back%kind("stamp") == PK_LIST, "and it is still a list column")
    end subroutine test_container_write_keeps_unit

    !> **D1.** `%append_null_rows` is the seventh row-structural mutation and the one
    !! `test_container_row_alignment` does not reach: that test covers `%sort_by`, `%filter_rows`,
    !! `%delete_rows`, `%truncate` and `%top_n`, and `test_container_append` covers `%append`.
    !! `append_nulls` (`src/parquet_columns_structural.f90`) has an explicit container arm, and
    !! without this nothing asserted that it works.
    !!
    !! **Negative control**: the row count and the existing rows are asserted BEFORE the call as
    !! well as after, so this cannot pass against an implementation that nulled the whole column
    !! or that grew the scalar column and left the container behind -- the failure a row-count-only
    !! assertion cannot see.
    subroutine test_container_append_null_rows(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        type(parquet_list_column) :: lc
        type(parquet_list_column), pointer :: lp
        integer(int64) :: ids(3)
        !
        call lc%init(PK_FLOAT64)
        call lc%append_row([1.0_real64, 2.0_real64])
        call lc%append_row([4.0_real64])
        call lc%append_row([7.0_real64, 8.0_real64, 9.0_real64])
        ids = [1_int64, 2_int64, 3_int64]
        call parquet_new_table(t)
        call t%add_column("id", ids)
        call t%add_column("spec", lc)
        ! The control: the state this call must ADD to rather than replace.
        call check(error, t%nrows() == 3_int64, "the table holds 3 rows before the append")
        if (allocated(error)) return
        call check(error, .not. t%is_null("spec", 3_int64), "and row 3 is present before it")
        if (allocated(error)) return
        !
        call t%append_null_rows(2_int64)
        call check(error, t%nrows() == 5_int64, "%append_null_rows(2) takes the table to 5 rows")
        if (allocated(error)) return
        call t%col("spec", lp)
        call check(error, lp%nrows() == 5_int64, &
            "and the CONTAINER itself grew too, rather than the scalar column growing alone")
        if (allocated(error)) return
        call check(error, t%is_null("spec", 4_int64) .and. t%is_null("spec", 5_int64), &
            "the two appended rows are null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("spec", 3_int64), &
            "and the rows that were already there are untouched")
        if (allocated(error)) return
        call check(error, lp%length(3_int64) == 3_int64, &
            "row 3 still holds its own three elements")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), &
            "a table built in memory has no file to detach from")
    end subroutine test_container_append_null_rows

    !> **D2.** `list_columns="container"` decides from the schema, but NOT for every `LIST`:
    !! `table_classify` screens the element type with `parquet_column_exists(types=...)`, and
    !! `parquet_get_column_type` unwraps a list one level -- so `list<struct>` answers `"unknown"`
    !! there and never reaches the container branch. A `MAP` of the same shape DOES reach it,
    !! because the map arm uses `parquet_get_map_value_type` instead.
    !!
    !! This pins a documented restriction rather than a desirable behaviour
    !! (`doc/pages/tables/table-open.md`'s `list_columns=` section says so), so that widening it is
    !! a deliberate act with a failing test to change rather than a silent one.
    !!
    !! **Negative control**: `list_col` in the same open is `PK_LIST`, so the token demonstrably
    !! took effect and this is not passing against an open that ignored it.
    subroutine test_nested_list_is_not_a_table_column(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        !
        call parquet_open_table(t, NESTED_FIXTURE, list_columns="container")
        ! The control first: the token took effect on a list this library can classify.
        call check(error, t%kind("list_col") == PK_LIST, &
            "list_columns=container did take effect on an ordinary LIST column")
        if (allocated(error)) return
        call check(error, t%is_supported("list_col"), "which is therefore supported")
        if (allocated(error)) return
        !
        call check(error, t%kind("list_of_struct") == PK_NONE, &
            "a LIST whose elements are structs is not classified as a container")
        if (allocated(error)) return
        call check(error, .not. t%is_supported("list_of_struct"), "and reports unsupported")
        if (allocated(error)) return
        call check(error, t%kind("list_of_list") == PK_NONE .and. t%kind("list_of_map") == PK_NONE, &
            "nor is a LIST of lists or a LIST of maps")
        if (allocated(error)) return
        ! The asymmetry this test exists to record: the MAP arm classifies the same shape.
        call check(error, t%kind("map_of_struct") == PK_MAP, &
            "while a MAP whose values are structs IS a container column")
        if (allocated(error)) return
        call check(error, t%is_supported("map_of_struct"), "and is supported")
        if (allocated(error)) return
        call check(error, t%has_column("list_of_struct"), &
            "an unreadable container column still gets a slot rather than blocking the open")
    end subroutine test_nested_list_is_not_a_table_column

    !> **D3.** `%copy_column` with no `to_kind` and `%clone` both claim to carry any column;
    !! nothing asserted either for a container. Both go through the container's own `clone_into`,
    !! so a component added to a container type later is carried without this file knowing.
    !!
    !! **Negative control**: independence. The copy and the clone are mutated after they are made
    !! and the source is re-checked, so this cannot pass against a shallow copy that shares the
    !! container's storage -- which would be the interesting way to get this wrong.
    subroutine test_container_copy_and_clone(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t, c
        type(parquet_list_column) :: lc
        type(parquet_list_column), pointer :: lp
        !
        call lc%init(PK_FLOAT64)
        call lc%append_row([1.0_real64, 2.0_real64])
        call lc%append_null_row()
        call lc%append_row([7.0_real64])
        call parquet_new_table(t)
        call t%add_column("spec", lc)
        !
        call t%copy_column("spec", "spec2")
        call check(error, t%kind("spec2") == PK_LIST, "%copy_column copies a container column")
        if (allocated(error)) return
        call check(error, t%ncols() == 2, "leaving the original in place")
        if (allocated(error)) return
        call t%col("spec2", lp)
        call check(error, lp%nrows() == 3_int64 .and. lp%length(1_int64) == 2_int64, &
            "and the copy holds the same rows")
        if (allocated(error)) return
        call check(error, t%is_null("spec2", 2_int64), "including the null one")
        if (allocated(error)) return
        !
        call t%clone(c)
        call check(error, c%kind("spec") == PK_LIST .and. c%nrows() == 3_int64, &
            "%clone carries a container column too")
        if (allocated(error)) return
        call check(error, c%is_null("spec", 2_int64), "with its nulls")
        if (allocated(error)) return
        ! Independence: mutate the clone, then re-check the source.
        call c%set_null("spec", 1_int64)
        call check(error, c%is_null("spec", 1_int64), "the clone can be mutated")
        if (allocated(error)) return
        call check(error, .not. t%is_null("spec", 1_int64), &
            "and the source is unaffected -- the two do not share the container's storage")
        if (allocated(error)) return
        call t%set_null("spec2", 3_int64)
        call check(error, .not. t%is_null("spec", 3_int64), &
            "%copy_column's copy is independent of its source in the same way")
    end subroutine test_container_copy_and_clone

    !> **D4.** The element-granular forms DELEGATE to the row form on a container column, rather
    !! than refusing (which two of them used to) or silently doing nothing (which the two mutators
    !! used to -- they wrote into a bitmap nothing reads for a container kind, so the call reported
    !! success and changed nothing).
    !!
    !! A container column's `width` is 1, so `e` can only ever be 1 and the element axis is
    !! degenerate rather than absent. The contract this pins is that all five spellings mean the
    !! same thing as the row form.
    !!
    !! **Negative control**: every assertion is made against the state BEFORE the call as well as
    !! after, so a mutator that changed nothing -- the exact defect this replaced -- fails here.
    subroutine test_container_element_forms_delegate(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_table) :: t
        type(parquet_table_col) :: c
        type(parquet_list_column) :: lc
        type(parquet_list_column), pointer :: lp
        logical, allocatable :: m2(:,:)
        !
        call lc%init(PK_FLOAT64)
        call lc%append_row([1.0_real64, 2.0_real64])
        call lc%append_row([3.0_real64])
        call lc%append_row([7.0_real64])
        call parquet_new_table(t)
        call t%add_column("spec", lc)
        !
        call check(error, .not. t%is_null("spec", 1_int64), "row 1 is present to begin with")
        if (allocated(error)) return
        call check(error, .not. t%is_null("spec", 1_int64, 1_int64), &
            "and the ELEMENT form agrees with the row form rather than aborting")
        if (allocated(error)) return
        !
        call t%set_null("spec", 1_int64, 1_int64)
        call check(error, t%is_null("spec", 1_int64), &
            "the element form of %set_null nulls the row -- it used to change nothing at all")
        if (allocated(error)) return
        call check(error, t%is_null("spec", 1_int64, 1_int64), &
            "and the element form of %is_null reports it")
        if (allocated(error)) return
        call t%clear_null("spec", 1_int64, 1_int64)
        call check(error, .not. t%is_null("spec", 1_int64), &
            "the element form of %clear_null clears it again")
        if (allocated(error)) return
        !
        ! The column handle must agree with the name form; the two disagreed while the name form
        ! refused and the handle delegated.
        call t%column("spec", c)
        call check(error, c%is_null(1_int64, 1_int64) .eqv. t%is_null("spec", 1_int64, 1_int64), &
            "a column handle's element form agrees with the table's")
        if (allocated(error)) return
        call c%set_null(2_int64, 1_int64)
        call check(error, t%is_null("spec", 2_int64), "a handle's element %set_null nulls the row")
        if (allocated(error)) return
        call c%clear_null(2_int64, 1_int64)
        call check(error, .not. t%is_null("spec", 2_int64), "and its %clear_null clears it")
        if (allocated(error)) return
        ! The WHOLE-ROW spellings, which take a different path from the element ones above:
        ! `parquet_column_clear_null_row` has its own container arm, and a container is the one
        ! kind that can be made present again without writing a value -- the row comes back with
        ! whatever elements its offsets still describe. A temporal column refuses the same call.
        call c%set_null(2_int64)
        call check(error, t%is_null("spec", 2_int64), "the handle's whole-row %set_null nulls the row")
        if (allocated(error)) return
        call c%clear_null(2_int64)
        call check(error, .not. t%is_null("spec", 2_int64), &
            "and the whole-row %clear_null makes a container row present again")
        if (allocated(error)) return
        ! And it comes back EMPTY, which is the half of the contract worth pinning: %set_null
        ! dropped the row's elements to keep a null row zero-length, so %clear_null is not an undo
        ! -- it clears one bit, and the row's offsets now describe nothing. Only %append_row gives
        ! a row elements again. Row 2 held one element before it was nulled.
        call t%col("spec", lp)
        call check(error, lp%length(2_int64) == 0_int64, &
            "a container row made present again comes back empty, not with its old elements")
        if (allocated(error)) return
        !
        ! The rank-2 mask forms, on both halves: the setter used to be a no-op and the getter used
        ! to abort. width is 1, so the mask is (1, nrows).
        call t%set_null("spec", reshape([.true., .true., .false.], [1, 3]))
        call check(error, t%is_null("spec", 3_int64), &
            "a rank-2 (width, nrows) mask nulls the row it marks invalid")
        if (allocated(error)) return
        call t%get_valid_mask("spec", m2)
        call check(error, size(m2, 1) == 1 .and. size(m2, 2, kind=int64) == t%nrows(), &
            "and the rank-2 %get_valid_mask answers, shaped (1, nrows)")
        if (allocated(error)) return
        call check(error, m2(1, 1) .and. m2(1, 2) .and. .not. m2(1, 3), &
            "reporting exactly the row that is null")
    end subroutine test_container_element_forms_delegate

end module test_table_container
