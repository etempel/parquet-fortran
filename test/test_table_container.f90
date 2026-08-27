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
!! every abort test ever written for it while breaking the case it was meant to allow, so each
!! refused element-granular query is asserted alongside the ROW-granular form that must still
!! answer on the same column.
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

contains

    !> Registers every test in this suite.
    subroutine collect_tests_table_container(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("list_columns=container classifies every LIST column as PK_LIST", &
                test_list_columns_container), &
            new_unittest("list_columns=auto is unchanged, and is the default", test_list_columns_auto), &
            new_unittest("list_columns= leaves a FIXED_SIZE_LIST column alone", test_list_columns_ignores_vectors), &
            new_unittest("a slice and the whole file disagree under auto and agree under container", &
                test_slice_whole_file_agreement), &
            new_unittest("a map column is classified by its VALUE type", test_map_classification), &
            new_unittest("%col, %get, %ref and %set reach a list column", test_list_accessors), &
            new_unittest("%add_column takes all three container types", test_add_container_columns), &
            new_unittest("a container column survives the slice regime, row group by row group", &
                test_container_slice), &
            new_unittest("row-structural mutations keep a container column ALIGNED", &
                test_container_row_alignment), &
            new_unittest("%append concatenates two tables' container columns", test_container_append), &
            new_unittest("the row form of %is_null and %get_valid_mask answer for a container", &
                test_container_row_validity), &
            new_unittest("%ensure_validity is NOT refused for a container column", &
                test_container_ensure_validity), &
            new_unittest("%print_stat reports row-length extremes and stays lazy", &
                test_container_print_stat), &
            new_unittest("pf_permute reorders a container column", test_container_permute), &
            new_unittest("a table round-trips its container columns through a file", &
                test_container_write_round_trip), &
            new_unittest("a container payload's temporal resolution survives a table write", &
                test_container_write_keeps_unit) &
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
        ! THE OTHER HALF OF THE NEGATIVE CONTROL, and the one that has teeth for the refusal
        ! itself: the element forms must still answer on an ORDINARY column in the same table. The
        ! assertions above only show the refusal does not eat the container's ROW forms; without
        ! these, a guard rewritten to fire for every column would pass every one of them.
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
    subroutine test_container_permute(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first disagreement.
        type(parquet_column) :: col
        type(parquet_list_column) :: lc
        class(parquet_container_column), allocatable :: box
        class(parquet_container_column), pointer :: back
        integer(int64) :: perm(4)
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

end module test_table_container
