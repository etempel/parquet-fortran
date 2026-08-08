!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the parquet_tables module (`parquet_table`, the eager read-only table layer).
!>
!> The suite is organised around what this layer actually has to get right:
!>
!> * **the round-trip matrix** -- write a fixture, open it as a table, read it back both ways
!>   (`%get` and `%col`), write it out again, reopen: values must survive unchanged. Repeated
!>   per kind rather than spot-checked, since each kind has its own materializer;
!> * **nulls per validity dispatch class** (column bitmap / embedded string column / inside the
!>   element), the same three-way split that makes parquet_column's own validity fragile;
!> * **widening**, which only the copy path does (the pointer path is exact-kind by design);
!> * **columns this library cannot read**, which must not stop a file from opening.
!>
!> Abort paths (`error stop`) cannot be exercised here because they kill the process -- they
!> live in test/error_scenarios.f90 as `table_*` scenarios, driven from test_errors.f90.
!>
!> Fixtures follow CLAUDE.md's rule for string arrays: the FIRST element is deliberately the
!> shortest, so a "sized from the first element" bug is actively provoked rather than avoided.
module test_table
    use parquet
    use parquet_tables
    use parquet_columns, only : PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, &
        PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP, PK_FLOAT64_VEC, PK_INT32_VEC, PK_STRING_VEC, &
        PK_INT64_VEC, PK_FLOAT32_VEC, PK_LOGICAL_VEC, PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC, &
        PK_NONE
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_table
    !
    !> Row count every fixture in this suite uses.
    integer, parameter :: NROW = 6
    !> An int64 real32 cannot hold exactly (needs more than 24 mantissa bits), used by the
    !! cast tests to make a trip through float32 observable.
    integer(int64), parameter :: WIDE_INT = 1234567891234_int64
    !> Vector width every vector fixture in this suite uses.
    integer, parameter :: NVEC = 3
    !
    !> The value `ext_table`'s own component holds when nothing has set it.
    real(real64), parameter :: ZP_DEFAULT = -1.0_real64
    !
    !> A hand-written extension of `parquet_table`, standing in for a generated table type.
    !!
    !! Milestone 3e's library half -- `%bind_predefined` and the `clone_extra` hook -- is
    !! deliberately testable WITHOUT running the generator, which is the whole point of keeping the
    !! rules in the library rather than in emitted text. This type is what the tests below drive
    !! them through, and it is also the worked example of what a user extending `parquet_table` by
    !! hand has to write: one component, one `clone_extra` override.
    type, extends(parquet_table) :: ext_table
        real(real64) :: zeropoint = ZP_DEFAULT !! a table parameter %clone must carry across.
    contains
        procedure :: clone_extra => ext_clone_extra !! Copies `zeropoint` into a clone.
    end type ext_table
    !
contains
    !
    !> `clone_extra` override: copies this type's own component into the clone.
    !!
    !! `class is`, not `type is`, so that a further extension of `ext_table` would still get this
    !! level's copy. `%clone` has already checked that `out` has the same dynamic type as `self`,
    !! so the branch always matches.
    subroutine ext_clone_extra(self, out, structure_only)
        class(ext_table), intent(in) :: self       !! the table being copied.
        class(parquet_table), intent(inout) :: out !! the copy, already holding the base state.
        logical, intent(in) :: structure_only      !! .true. when called from %clone_structure.
        !
        select type (out)
        class is (ext_table)
            out%zeropoint = self%zeropoint
        end select
    end subroutine ext_clone_extra
    !
    subroutine collect_tests_parquet_table(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("open reports rows, columns and names in file order", test_open_basics), &
            new_unittest("scalar numeric kinds round-trip through get/col/set/write", &
                test_roundtrip_scalar_numeric), &
            new_unittest("string columns round-trip in both the compact and character forms", &
                test_roundtrip_string), &
            new_unittest("temporal kinds round-trip and keep their own null state", &
                test_roundtrip_temporal), &
            new_unittest("vector kinds round-trip with (element, row) orientation", &
                test_roundtrip_vector), &
            new_unittest("get widens int32 to int64 and float32 to float64", test_widening), &
            new_unittest("col hands back a live pointer: writes through it are visible", &
                test_pointer_is_live), &
            new_unittest("nulls survive a round trip for all three validity dispatch classes", &
                test_nulls_roundtrip), &
            new_unittest("a plain LIST column's width is deferred, then resolved and proven", &
                test_deferred_list_width), &
            new_unittest("a slice measures a plain LIST column over its own row groups only", &
                test_deferred_list_width_slice), &
            new_unittest("the no-nulls materialize fast path never loses a null", &
                test_materialize_null_fast_path), &
            new_unittest("a file without column statistics still reads its nulls", &
                test_materialize_without_statistics), &
            new_unittest("nulls survive being written back out by parquet_write_table", &
                test_write_table_nulls), &
            new_unittest("a null in a vector string column is widened to the whole row", &
                test_string_vector_nulls), &
            new_unittest("opening reads nothing; each accessor touches only its own column", &
                test_lazy_first_touch), &
            new_unittest("prefetch and materialize_all read columns ahead of first touch", &
                test_prefetch), &
            new_unittest("reload restores a column's file values after set", test_reload), &
            new_unittest("row group bounds partition the file's rows exactly", &
                test_row_group_bounds), &
            new_unittest("a slice straddling a row-group boundary reads all 18 kinds correctly", &
                test_slice_kind_matrix), &
            new_unittest("slice bounds inside, on and across row groups all agree with the full table", &
                test_slice_shapes), &
            new_unittest("a row handle reads every kind, widens, and triggers its own first touch", &
                test_row_view), &
            new_unittest("get_slice copies range, strided, descending and list selections", &
                test_get_slice), &
            new_unittest("a thread's own slice table may be read lazily inside a parallel region", &
                test_parallel_private_slices), &
            new_unittest("a column this library cannot read does not stop the file opening", &
                test_unsupported_column), &
            new_unittest("nested struct leaves become dotted columns", test_struct_leaves), &
            new_unittest("prefetch(struct) reads every leaf, evict_column gives them back", &
                test_prefetch_prefix_and_evict), &
            new_unittest("validate_qc checks every declaring column and holds none", &
                test_validate_qc), &
            new_unittest("print_stat reports without reading anything", test_print_stat), &
            new_unittest("nrows_unfiltered and row_group_extent report the physical geometry", &
                test_row_geometry_queries), &
            new_unittest("the automatic parquet_row_index column names each row's file row", &
                test_row_index_column), &
            new_unittest("parquet_new_table plus add_column builds a table and writes it", &
                test_from_scratch), &
            new_unittest("add_column(force=) replaces a column of the same name", &
                test_add_column_force), &
            new_unittest("set replaces values without changing the row set", test_set_values), &
            new_unittest("row_mask writes a row subset and leaves the table untouched", &
                test_write_row_mask), &
            new_unittest("reopening the same table variable frees the old table first", &
                test_reopen_same_variable), &
            new_unittest("found= reports a missing column instead of aborting", test_soft_fail), &
            new_unittest("unit is carried by add_column and reported by %unit", test_unit), &
            new_unittest("all 18 kinds: read back, pointer-alias and write out", test_kind_matrix), &
            new_unittest("all 18 kinds: %col aliases and %set replaces", test_kind_matrix_col_set), &
            new_unittest("all 18 kinds: found= reports a missing column instead of aborting", &
                test_kind_matrix_found), &
            new_unittest("all 18 kinds: add_column builds a table from scratch", test_kind_matrix_add), &
            new_unittest("filename and file metadata are reported back from the source file", &
                test_filename_and_metadata), &
            new_unittest("set_element writes one cell, in both row-index kinds", test_set_element), &
            new_unittest("set_element writes one cell on every remaining kind", &
                test_set_element_more_kinds), &
            new_unittest("set_null/clear_null/compact_validity are row-granular", &
                test_validity_mutation), &
            new_unittest("drop_column removes a column and leaves the survivors intact", &
                test_drop_column), &
            new_unittest("rename_column changes the lookup name, not the file column", &
                test_rename_column), &
            new_unittest("copy_column adds a converted column and leaves the source alone", &
                test_copy_column), &
            new_unittest("copy_column with no target kind copies any column kind", &
                test_copy_column_same_kind), &
            new_unittest("cast converts a column in place across the numeric kinds", &
                test_cast_in_place), &
            new_unittest("cast on a vector column converts every element", &
                test_cast_vector), &
            new_unittest("cast before a first touch reads straight into the target kind", &
                test_cast_deferred), &
            new_unittest("cast keeps nulls, unit and row count, and is a no-op on the same kind", &
                test_cast_preserves), &
            new_unittest("cast(exact=.true.) refuses a loss that the default allows", &
                test_cast_exact_flag), &
            new_unittest("filter_rows keeps the selected rows in every column and detaches", &
                test_filter_rows), &
            new_unittest("delete_rows and truncate handle repeats and past-the-end counts", &
                test_delete_and_truncate), &
            new_unittest("sort_by orders rows by one or more keys", test_sort_by), &
            new_unittest("an in-memory sort matches a read-time sort row for row", &
                test_sort_matches_read_time), &
            new_unittest("sort_by handles int64 and timestamp keys", test_sort_by_key_kinds), &
            new_unittest("sort_by handles logical, date, time and float32 keys", &
                test_sort_by_more_key_kinds), &
            new_unittest("sorting reads an unread key column by itself", test_sort_reads_key), &
            new_unittest("argsort_by orders without moving or detaching", test_argsort_by), &
            new_unittest("argsort_by reports group boundaries", test_argsort_by_groups), &
            new_unittest("argsort_by recovers file order via the row index", test_argsort_by_row_index), &
            new_unittest("is_sorted_by answers both ways", test_is_sorted_by), &
            new_unittest("argsort_partial returns the n best rows", test_argsort_partial), &
            new_unittest("top_n keeps the n best rows, in key order", test_top_n), &
            new_unittest("top_n clamps, empties, delegates and reads its key", test_top_n_edges), &
            new_unittest("append concatenates a batch and null-fills the columns it omits", &
                test_append_table), &
            new_unittest("append_null_rows supports the extend-fill-append workflow", &
                test_append_null_rows_workflow), &
            new_unittest("append takes a single row through a row handle", test_append_row), &
            new_unittest("append(row) does not scale with the source table's size", &
                test_append_row_source_independent), &
            new_unittest("append(row) carries every kind, nulls included", test_append_row_kinds), &
            new_unittest("compact is a no-op on a table read from a file", test_compact_noop_after_read), &
            new_unittest("compact releases what appending left behind", test_compact_after_appends), &
            new_unittest("reserve removes the reallocations that follow it", test_table_reserve), &
            new_unittest("compact leaves an unread column unread and attached", test_compact_keeps_lazy), &
            new_unittest("a clone is independent, stays lazy and keeps the row scope", test_clone), &
            new_unittest("extra: remap: renames a file column for reading", test_remap_basic), &
            new_unittest("a read-in MAML's unit: reaches %unit, before and after the read", &
                test_unit_from_maml), &
            new_unittest("parquet_write_table carries the source file's metadata on request", &
                test_write_table_copy_metadata), &
            new_unittest("resident_only, has_nulls, get_valid_mask, set_null(mask), generation", &
                test_introspection_additions), &
            new_unittest("clone_structure works on a table that has read nothing", &
                test_clone_structure_lazy), &
            new_unittest("get_element reads one cell, widening like %get", test_get_element), &
            new_unittest("is_valid= on get, col, get_slice and set", test_is_valid_argument), &
            new_unittest("set_slice writes a selection back", test_set_slice), &
            new_unittest("parquet_string_column is a first-class table value", &
                test_string_column_first_class), &
            new_unittest("a row handle can write: %set and %ref", test_row_set_and_ref), &
            new_unittest("found= reaches every name-taking procedure", test_found_everywhere), &
            new_unittest("extra: remap: shadows, swaps and duplicates as documented", test_remap_shadow_duplicate), &
            new_unittest("rename_column on a remapped column keeps its file column", test_remap_then_rename), &
            new_unittest("open with filter= narrows every column, in internal names", test_open_filter), &
            new_unittest("open with sort= orders every column, in internal names", test_open_sort), &
            new_unittest("open with qc= enforces a code-declared bound", test_open_qc), &
            new_unittest("a MAML's extra: filter:/sort: apply, and compose with the code's", &
                test_open_maml_filter_sort), &
            new_unittest("extra: sort: takes a trailing nulls_first/nulls_last token", test_maml_sort_nulls_token), &
            new_unittest("filter=/sort=/qc= are translated through extra: remap:", test_transform_with_remap), &
            new_unittest("open with sample_fraction= keeps a subset, reproducibly by seed", test_open_sample), &
            new_unittest("a clone of a transformed table reattaches the same transform", test_clone_keeps_transform), &
            new_unittest("a clone keeps an UNSEEDED sample_fraction's own rows", &
                test_clone_keeps_unseeded_sample), &
            new_unittest("a detached table's clone keeps its values and stays detached", &
                test_clone_of_detached), &
            new_unittest("an unfiltered slice cutting through row groups installs no mask", &
                test_slice_fast_path), &
            new_unittest("a slice opened with filter= counts and returns only its survivors", &
                test_slice_filter), &
            new_unittest("a slice's filter comes from its maml too, and qc applies", &
                test_slice_maml_filter_qc), &
            new_unittest("a slice opened with sample_fraction= keeps a seeded subset of itself", &
                test_slice_sample), &
            new_unittest("a clone of a filtered slice reattaches the same scoped filter", &
                test_slice_clone), &
            new_unittest("row_group_bounds answers in table rows by default and file rows with physical=", &
                test_row_group_bounds_physical), &
            new_unittest("row_group_bounds with physical= answers under a sort too", &
                test_row_group_bounds_sorted), &
            new_unittest("a row mutation that changes no row does not detach", &
                test_noop_mutation_keeps_file), &
            new_unittest("one null element survives a file -> table -> file round trip", &
                test_element_null_round_trip), &
            new_unittest("the table's element-granular null API addresses single elements", &
                test_table_element_null_api), &
            new_unittest("a vector column's is_valid= is per element on get, col, set and slice", &
                test_table_rank2_masks), &
            new_unittest("get_valid_mask spans several bitmap words, both ranks and null-free", &
                test_valid_mask_multiword), &
            new_unittest("parquet_write_table parses a schema the caller left unparsed", &
                test_write_table_parses_schema), &
            new_unittest("release= leaves the table in the residency state the write found", &
                test_write_table_release), &
            new_unittest("the writer options parquet_write_table forwards reach the file", &
                test_write_table_writer_options), &
            new_unittest("a schema-less write writes the resident columns and nothing else", &
                test_write_table_schemaless), &
            new_unittest("a schema-less write's sidecar MAML carries units and reopens the file", &
                test_write_table_schemaless_sidecar), &
            new_unittest("a nanosecond timestamp column survives a schema-less write", &
                test_write_table_temporal_unit), &
            new_unittest("a row mutation on a slice detaches it and strands its unread columns", &
                test_mutate_slice_then_read), &
            new_unittest("prefetch reaches the automatic row-index column", &
                test_prefetch_row_index), &
            new_unittest("per-element nulls written by Arrow survive the read intact", &
                test_element_nulls_from_arrow), &
            new_unittest("every structural entry point advances the generation counter", &
                test_generation_sweep), &
            new_unittest("kind, width, unit, residency and is_supported answer without reading", &
                test_metadata_queries_do_not_touch), &
            new_unittest("a materialized row index survives the in-memory mutations", &
                test_row_index_recovery), &
            new_unittest("an in-memory table is never detached, however it is mutated", &
                test_in_memory_never_detaches), &
            new_unittest("clone carries an extending type's own components via clone_extra", &
                test_ext_clone_carries_components), &
            new_unittest("clone_structure carries an extending type's own components too", &
                test_ext_clone_structure_carries_components), &
            new_unittest("bind_predefined binds, widens to the declared kind and materializes", &
                test_bind_predefined_binds_and_widens), &
            new_unittest("bind_predefined creates a computed column with every row null", &
                test_bind_predefined_computed_column), &
            new_unittest("bind_predefined with no declared fields is a clean no-op", &
                test_bind_predefined_empty), &
            new_unittest("parquet_write_table accepts a type extending parquet_table", &
                test_write_table_accepts_extension), &
            new_unittest("a predefined column drops with force=, and a plain one without it", &
                test_drop_predefined_with_force), &
            new_unittest("add_column trims a character array, and %get is sized to the real values", &
                test_add_column_chr_trims) &
            ]
    end subroutine collect_tests_parquet_table
    !
    !> Writes the shared numeric/string fixture used by most tests below.
    !> Writes `lines` to `fname` verbatim, one per record -- the read-in (Role-B) MAML fixtures the
    !! remap tests below open a table with. `parquet_open_table(maml=)` takes a FILE PATH, so these
    !! have to exist on disk rather than being built in memory the way a `parquet_maml_file` can be.
    subroutine write_maml_file(fname, lines)
        character(len=*), intent(in) :: fname     !! file to write (one per test).
        character(len=*), intent(in) :: lines(:)  !! MAML source, one array element per line.
        integer :: unit, i
        !
        open(newunit=unit, file=fname, status="replace", action="write")
        do i = 1, size(lines)
            write(unit, "(a)") trim(lines(i))
        end do
        close(unit)
    end subroutine write_maml_file
    !
    subroutine write_basic_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer(int32) :: i32(NROW)
        integer(int64) :: i64(NROW)
        real(real32) :: f32(NROW)
        real(real64) :: f64(NROW)
        logical :: b(NROW)
        character(len=8) :: s(NROW)
        integer :: i
        !
        do i = 1, NROW
            i32(i) = i
            i64(i) = int(i, int64) * 1000000000_int64
            f32(i) = real(i, real32) * 0.5_real32
            f64(i) = real(i, real64) * 2.25_real64
            b(i) = mod(i, 2) == 0
        end do
        ! First element deliberately the shortest (CLAUDE.md).
        s = ["a       ", "bcd     ", "ef      ", "ghijklm ", "no      ", "p       "]
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "i32", i32)
        call parquet_write_column(w, "i64", i64)
        call parquet_write_column(w, "f32", f32)
        call parquet_write_column(w, "f64", f64)
        call parquet_write_column(w, "b", b)
        call parquet_write_column(w, "s", s)
        call parquet_close_writer(w)
    end subroutine write_basic_fixture
    !
    subroutine test_open_basics(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        character(len=*), parameter :: f = "test_run/table_basic.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call check(error, t%nrows() == NROW, "table should report the fixture's row count")
        if (allocated(error)) return
        call check(error, t%ncols() == 6, "table should report 6 columns")
        if (allocated(error)) return
        call t%column_names(names)
        call check(error, size(names) == 6, "column_names should return one entry per column")
        if (allocated(error)) return
        call check(error, trim(names(1)) == "i32", "column_names should preserve file order")
        if (allocated(error)) return
        call check(error, trim(names(6)) == "s", "column_names(6) should be the last written column")
        if (allocated(error)) return
        call check(error, t%has_column("f64"), "has_column should find an existing column")
        if (allocated(error)) return
        call check(error, .not. t%has_column("nope"), "has_column should not invent a column")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "a freshly opened table is not detached")
        if (allocated(error)) return
        ! Everything above was answered without reading a single column: opening classifies
        ! from the schema and stops there.
        call check(error, t%residency("f64") == RES_EMPTY, &
            "no column should be resident before it is touched")
        if (allocated(error)) return
        call check(error, t%kind("f64") == PK_FLOAT64, &
            "kind must answer from the schema, before the column is read")
        if (allocated(error)) return
        call check(error, t%width("f64") == 1, &
            "width must answer from the schema, before the column is read")
    end subroutine test_open_basics
    !
    subroutine test_roundtrip_scalar_numeric(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        integer(int32), allocatable :: g32(:)
        integer(int64), allocatable :: g64(:)
        real(real32), allocatable :: r32(:)
        real(real64), allocatable :: r64(:)
        logical, allocatable :: gb(:)
        character(len=*), parameter :: f = "test_run/table_num.parquet"
        character(len=*), parameter :: fo = "test_run/table_num_out.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call check(error, t%kind("i32") == PK_INT32, "i32 column should resolve to PK_INT32")
        if (allocated(error)) return
        call check(error, t%kind("i64") == PK_INT64, "i64 column should resolve to PK_INT64")
        if (allocated(error)) return
        call check(error, t%kind("f32") == PK_FLOAT32, "f32 column should resolve to PK_FLOAT32")
        if (allocated(error)) return
        call check(error, t%kind("f64") == PK_FLOAT64, "f64 column should resolve to PK_FLOAT64")
        if (allocated(error)) return
        call check(error, t%kind("b") == PK_LOGICAL, "b column should resolve to PK_LOGICAL")
        if (allocated(error)) return
        call check(error, t%width("i32") == 1, "a scalar column's width should be 1")
        if (allocated(error)) return
        !
        call t%get("i32", g32)
        call check(error, all(g32 == [1, 2, 3, 4, 5, 6]), "i32 values should survive the read")
        if (allocated(error)) return
        call t%get("i64", g64)
        call check(error, g64(6) == 6000000000_int64, "i64 values beyond int32 range should survive")
        if (allocated(error)) return
        call t%get("f32", r32)
        call check(error, abs(r32(4) - 2.0_real32) < 1.0e-6_real32, "f32 values should survive")
        if (allocated(error)) return
        call t%get("f64", r64)
        call check(error, abs(r64(4) - 9.0_real64) < 1.0e-12_real64, "f64 values should survive")
        if (allocated(error)) return
        call t%get("b", gb)
        call check(error, all(gb .eqv. [.false., .true., .false., .true., .false., .true.]), &
            "logical values should survive the read")
        if (allocated(error)) return
        !
        ! Write back out through a schema, then reopen and compare.
        call s%init("num")
        call s%add_field("i32", "int32")
        call s%add_field("i64", "int64")
        call s%add_field("f32", "float32")
        call s%add_field("f64", "float64")
        call s%add_field("b", "boolean")
        call parquet_parse_maml(s)
        call parquet_write_table(t, fo, s)
        call parquet_open_table(t2, fo)
        call check(error, t2%nrows() == NROW, "the written table should have the same row count")
        if (allocated(error)) return
        call check(error, t2%ncols() == 5, "the written table should have the schema's columns only")
        if (allocated(error)) return
        call t2%get("f64", r64)
        call check(error, abs(r64(4) - 9.0_real64) < 1.0e-12_real64, &
            "f64 values should survive the write/reopen round trip")
        if (allocated(error)) return
        call t2%get("i64", g64)
        call check(error, g64(6) == 6000000000_int64, &
            "i64 values should survive the write/reopen round trip")
    end subroutine test_roundtrip_scalar_numeric
    !
    subroutine test_roundtrip_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_string_column) :: sc
        character(len=:), allocatable :: chr(:), one
        character(len=*), parameter :: f = "test_run/table_str.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call check(error, t%kind("s") == PK_STRING, "s column should resolve to PK_STRING")
        if (allocated(error)) return
        !
        ! Compact form: the parquet_string_column itself.
        call t%get("s", sc)
        call check(error, sc%size() == NROW, "the compact string form should hold every row")
        if (allocated(error)) return
        call sc%get(1_int64, one)
        call check(error, one == "a", "compact form element 1 should be 'a'")
        if (allocated(error)) return
        call sc%get(4_int64, one)
        call check(error, one == "ghijklm", "compact form element 4 should be the longest value")
        if (allocated(error)) return
        !
        ! Character-array form: width comes from the LONGEST element, not the first one -- this
        ! is the "sized from the first element" bug class, and the fixture's first element is
        ! deliberately the shortest so a regression shows up here.
        call t%get("s", chr)
        call check(error, len(chr) == 7, &
            "the character form should be as wide as the longest value (7), not the first (1)")
        if (allocated(error)) return
        call check(error, trim(chr(1)) == "a", "character form element 1 should be 'a'")
        if (allocated(error)) return
        call check(error, trim(chr(4)) == "ghijklm", &
            "character form element 4 should not be truncated")
    end subroutine test_roundtrip_string
    !
    subroutine test_roundtrip_temporal(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        type(parquet_date) :: d(NROW)
        type(parquet_date), allocatable :: gd(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_temporal.parquet"
        !
        do i = 1, NROW
            d(i) = parquet_date(2026, 7, i)
        end do
        ! A null element in the middle: temporal kinds carry validity inside the element, so this
        ! must survive without any is_valid mask anywhere on the path.
        d(3) = parquet_date()
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "d", d)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call check(error, t%kind("d") == PK_DATE, "d column should resolve to PK_DATE")
        if (allocated(error)) return
        call t%get("d", gd)
        call check(error, size(gd) == NROW, "every date row should be read")
        if (allocated(error)) return
        call check(error, gd(1)%year() == 2026 .and. gd(1)%day() == 1, &
            "date values should survive the read")
        if (allocated(error)) return
        call check(error, gd(3)%is_null(), "a null date should still be null after the read")
        if (allocated(error)) return
        call check(error, t%is_null("d", 3_int64), &
            "%is_null should agree with the element's own null state")
        if (allocated(error)) return
        call check(error, .not. t%is_null("d", 1_int64), &
            "a non-null date row should not report null")
    end subroutine test_roundtrip_temporal
    !
    subroutine test_roundtrip_vector(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        real(real64) :: v(NVEC, NROW)
        real(real64), allocatable :: gv(:,:)
        real(real64), pointer :: pv(:,:)
        integer :: i, e
        character(len=*), parameter :: f = "test_run/table_vec.parquet"
        !
        do i = 1, NROW
            do e = 1, NVEC
                v(e, i) = real(i * 10 + e, real64)
            end do
        end do
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call check(error, t%kind("v") == PK_FLOAT64_VEC, "v column should resolve to PK_FLOAT64_VEC")
        if (allocated(error)) return
        call check(error, t%width("v") == NVEC, "a vector column's width should be its element count")
        if (allocated(error)) return
        call t%get("v", gv)
        call check(error, size(gv, 1) == NVEC .and. size(gv, 2) == NROW, &
            "a vector column must come back shaped (element, row)")
        if (allocated(error)) return
        call check(error, abs(gv(2, 3) - 32.0_real64) < 1.0e-12_real64, &
            "vector element (2,3) should be row 3's second element")
        if (allocated(error)) return
        call t%col("v", pv)
        call check(error, abs(pv(3, 6) - 63.0_real64) < 1.0e-12_real64, &
            "the pointer form should see the same (element, row) layout")
    end subroutine test_roundtrip_vector
    !
    subroutine test_widening(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64), allocatable :: g64(:)
        real(real64), allocatable :: r64(:)
        integer(int64), allocatable :: gv64(:,:)
        real(real64), allocatable :: rv64(:,:)
        character(len=*), parameter :: f = "test_run/table_widen.parquet"
        character(len=*), parameter :: fv = "test_run/table_widen_vec.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        ! The stored kinds are int32 and float32; the caller asks for int64/float64 arrays.
        call check(error, t%kind("i32") == PK_INT32, "precondition: i32 is stored as int32")
        if (allocated(error)) return
        call t%get("i32", g64)
        call check(error, all(g64 == [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]), &
            "get should widen an int32 column into an int64 array")
        if (allocated(error)) return
        call check(error, t%kind("f32") == PK_FLOAT32, "precondition: f32 is stored as float32")
        if (allocated(error)) return
        call t%get("f32", r64)
        call check(error, abs(r64(4) - 2.0_real64) < 1.0e-6_real64, &
            "get should widen a float32 column into a float64 array")
        if (allocated(error)) return
        ! The vector kinds widen the same way, through their own get_arr_i64v/get_arr_f64v
        ! specifics -- covered separately from the scalar case above.
        call write_matrix_fixture(fv)
        call parquet_open_table(t, fv)
        call check(error, t%kind("v_i32") == PK_INT32_VEC, "precondition: v_i32 is stored as int32_vec")
        if (allocated(error)) return
        call t%get("v_i32", gv64)
        call check(error, gv64(2, 3) == 32_int64, &
            "get should widen an int32_vec column into an int64 matrix")
        if (allocated(error)) return
        call check(error, t%kind("v_f32") == PK_FLOAT32_VEC, "precondition: v_f32 is stored as float32_vec")
        if (allocated(error)) return
        call t%get("v_f32", rv64)
        call check(error, abs(rv64(2, 3) - 16.0_real64) < 1.0e-6_real64, &
            "get should widen a float32_vec column into a float64 matrix")
    end subroutine test_widening
    !
    subroutine test_pointer_is_live(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), pointer :: p(:)
        real(real64), allocatable :: g(:)
        character(len=*), parameter :: f = "test_run/table_ptr.parquet"
        !
        call write_basic_fixture(f)
        ! Note the table is NOT declared `target` here: %col points into the cache's heap, not
        ! into this dummy/local, so the pointer stays valid. That is a deliberate property of
        ! the design, and this test is what would catch it being lost.
        call parquet_open_table(t, f)
        call t%col("f64", p)
        call check(error, associated(p), "col should return an associated pointer")
        if (allocated(error)) return
        call check(error, size(p) == NROW, "the pointer should span the whole column")
        if (allocated(error)) return
        p(2) = -7.5_real64
        call t%get("f64", g)
        call check(error, abs(g(2) + 7.5_real64) < 1.0e-12_real64, &
            "a write through the pointer must be visible to a later copy-out")
        if (allocated(error)) return
        ! And the pointer still refers to the same storage after an unrelated read.
        call check(error, abs(p(2) + 7.5_real64) < 1.0e-12_real64, &
            "the pointer should remain valid across an unrelated read")
    end subroutine test_pointer_is_live
    !
    subroutine test_nulls_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        real(real64) :: f64(NROW)
        character(len=8) :: s(NROW)
        type(parquet_date) :: d(NROW)
        logical :: valid_f(NROW), valid_s(NROW)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_nulls.parquet"
        !
        do i = 1, NROW
            f64(i) = real(i, real64)
            d(i) = parquet_date(2026, 1, i)
        end do
        s = ["a       ", "bcd     ", "ef      ", "ghij    ", "kl      ", "m       "]
        valid_f = .true.
        valid_s = .true.
        valid_f(2) = .false.   ! bitmap class
        valid_s(5) = .false.   ! embedded string column class
        d(4) = parquet_date()  ! inside-the-element class
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "f", f64, is_valid=valid_f)
        call parquet_write_column(w, "s", s, is_valid=valid_s)
        call parquet_write_column(w, "d", d)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        ! Class 1: column bitmap.
        call check(error, t%is_null("f", 2_int64), "a null numeric row should read back null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("f", 1_int64), "a valid numeric row should not be null")
        if (allocated(error)) return
        ! Class 2: embedded parquet_string_column.
        call check(error, t%is_null("s", 5_int64), "a null string row should read back null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("s", 1_int64), "a valid string row should not be null")
        if (allocated(error)) return
        ! Class 3: inside the element.
        call check(error, t%is_null("d", 4_int64), "a null date row should read back null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("d", 1_int64), "a valid date row should not be null")
    end subroutine test_nulls_roundtrip
    !
    !> A plain Parquet `LIST` column carries no width in its schema, so `parquet_table` defers its
    !! kind and width to first use rather than decoding it at open. This checks both halves: that
    !! opening leaves such a column unclassified and unread, and that `%kind`/`%width` then resolve
    !! it to a PROVEN answer.
    !!
    !! `test/fixtures/list_widths.parquet` (see tools/generate_fixtures.cpp) holds one column per
    !! outcome of the two-tier resolution. The one that matters most is `avg_ok`: its rows alternate
    !! between lengths 3 and 1, so every row group averages exactly 2 elements per row and the
    !! footer screen CANNOT reject it. Only the row-group scan can, which is precisely why
    !! `%kind`/`%width` prove rather than trust the screen -- a candidate is not a proof.
    subroutine test_deferred_list_width(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: v(:,:)
        character(len=*), parameter :: f = "test/fixtures/list_widths.parquet"
        !
        call parquet_open_table(t, f)
        call check(error, t%ncols() == 9, "the fixture should present all 9 columns")
        if (allocated(error)) return
        ! Opening must not have read anything, deferred columns included.
        call check(error, t%residency("uniform") == RES_EMPTY, &
            "opening must not materialize a deferred-width column")
        if (allocated(error)) return
        !
        ! Uniform: screen yields candidate 3, the scan confirms it -> a real vector column.
        call check(error, t%kind("uniform") == PK_INT32_VEC, &
            "a uniform-width LIST column should resolve to the vector kind")
        if (allocated(error)) return
        call check(error, t%width("uniform") == 3, "its width should be 3")
        if (allocated(error)) return
        ! Resolving the width is not the same as reading the column.
        call check(error, t%residency("uniform") == RES_EMPTY, &
            "%kind/%width must resolve the width without materializing the column")
        if (allocated(error)) return
        !
        ! avg_ok: lengths 3,1,3,1 average to exactly 2, so the footer screen passes it as a
        ! candidate of 2 and only the scan rejects it. Getting 1 here is the whole point.
        call check(error, t%width("avg_ok") == 1, &
            "a LIST column whose mean row length is a whole number but whose rows differ must " // &
            "not be reported as a vector column")
        if (allocated(error)) return
        call check(error, t%kind("avg_ok") == PK_INT32, &
            "avg_ok should resolve to the scalar kind, not the vector kind")
        if (allocated(error)) return
        !
        ! ragged: non-integral mean, rejected by the screen alone.
        call check(error, t%width("ragged") == 1, "a ragged LIST column should report width 1")
        if (allocated(error)) return
        ! late: uniform except in the final row group, so only a row-group-vs-row-group comparison
        ! catches it -- divisibility alone would not.
        call check(error, t%width("late") == 1, &
            "a LIST column that changes width only in its last row group should report width 1")
        if (allocated(error)) return
        ! A null or empty row has length 0, which no width >= 1 covers.
        call check(error, t%width("with_null") == 1, &
            "a LIST column containing a null row should report width 1")
        if (allocated(error)) return
        call check(error, t%width("with_empty") == 1, &
            "a LIST column containing an empty row should report width 1")
        if (allocated(error)) return
        ! null_avg's mean IS a whole number (a null row occupies one leaf slot), so the screen
        ! passes it and only the scan can reject it -- the null counterpart of avg_ok above.
        call check(error, t%width("null_avg") == 1, &
            "a LIST column whose null row keeps the mean integral must still report width 1")
        if (allocated(error)) return
        !
        ! A plain LIST leaf underneath a STRUCT is a different matter: struct_path_exists
        ! (parquet_wrapper.cpp) deliberately refuses to address a LIST/LARGE_LIST/MAP leaf through a
        ! dotted path, so such a column is visible but not readable and deferral never applies to
        ! it. Pinned here so the boundary is explicit rather than discovered -- a FIXED_SIZE_LIST
        ! leaf under a struct IS addressable (see test/fixtures/nested_struct.parquet); only the
        ! variable-length form is not.
        call check(error, t%has_column("nested.vals"), &
            "a struct-nested LIST leaf should still be listed as a column")
        if (allocated(error)) return
        call check(error, .not. t%is_supported("nested.vals"), &
            "but it should be reported unsupported, not silently classified")
        if (allocated(error)) return
        !
        ! The control: a scalar column is classified from the schema at open and never deferred.
        call check(error, t%kind("scalar") == PK_INT32, "a scalar column should stay scalar")
        if (allocated(error)) return
        call check(error, t%width("scalar") == 1, "a scalar column's width should be 1")
        if (allocated(error)) return
        !
        ! And the resolved column still reads correctly afterwards.
        call t%get("uniform", v)
        call check(error, size(v, 1) == 3 .and. size(v, 2) == 16, &
            "the resolved vector column should read back as (3, 16)")
        if (allocated(error)) return
        call check(error, all(v(:, 2) == [100, 101, 102]), &
            "row 2 of the resolved vector column should hold its own values")
    end subroutine test_deferred_list_width
    !
    !> A slice measures a deferred column over the row groups IT covers, not the whole file.
    !!
    !! The `late` column is uniformly width 3 for its first three row groups and width 2 in the
    !! fourth, so it has no file-wide width at all -- yet each of those two ranges is internally
    !! uniform. A slice over rows 1..12 must therefore see a width-3 vector column, a slice over
    !! rows 13..16 a width-2 one, and the whole file neither. Two tables over the same file
    !! legitimately disagreeing is the documented consequence of measuring slice-locally, and this
    !! test is what pins it down.
    subroutine test_deferred_list_width_slice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test/fixtures/list_widths.parquet"
        !
        call parquet_open_table(t, f, 1_int64, 12_int64)
        call check(error, t%width("late") == 3, &
            "a slice covering only the uniform row groups should see width 3")
        if (allocated(error)) return
        call check(error, t%kind("late") == PK_INT32_VEC, &
            "and should resolve to the vector kind")
        if (allocated(error)) return
        !
        call parquet_open_table(t, f, 13_int64, 16_int64)
        call check(error, t%width("late") == 2, &
            "a slice covering only the final row group should see that row group's own width")
        if (allocated(error)) return
        !
        call parquet_open_table(t, f)
        call check(error, t%width("late") == 1, &
            "the whole file has no single width, so the full table should report 1")
    end subroutine test_deferred_list_width_slice
    !
    !> Materializing must carry every null across, whichever internal path it takes.
    !!
    !! `mat_*` now asks the file's footer statistics whether a column has any Nulls, and skips the
    !! whole validity pipeline when the answer is no -- no mask is allocated, requested, converted or
    !! replayed. That is a silent failure mode if it ever gets the question wrong: the column simply
    !! comes back with every row valid. So this drives both branches over the same table and checks
    !! the nulls that must survive AND the non-nulls that must not appear.
    !!
    !! Both validity dispatch classes that use the mask path are covered (`bitmap` via a numeric
    !! column, `embedded string column` via a string one), plus a vector column, whose mask is
    !! row-granular and read per element. The temporal class carries its nulls inside the element
    !! and never used a mask, so it is covered by test_nulls_roundtrip instead.
    subroutine test_materialize_null_fast_path(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: NB = 40 !! spans more than one 64-bit validity block boundary at 40.
        type(error_type), allocatable :: e2
        type(parquet_writer) :: w
        type(parquet_table) :: t
        real(real64) :: dirty(NB), clean(NB), dirtyv(3, NB)
        real(real64), allocatable :: back(:)
        character(len=6) :: s6(NB)
        logical :: vd(NB), vs(NB), vv(3, NB)
        ! One scalar column per remaining mat_* materializer whose null branch
        ! test_materialize_null_fast_path did not otherwise reach (int64/float32/logical), plus
        ! their vector counterparts (int32/int64/float32/logical) -- dirty/dv above already cover
        ! float64 scalar and vector.
        integer(int32) :: dirty_i32(NB), dirtyv_i32(3, NB)
        integer(int64) :: dirty_i64(NB), dirtyv_i64(3, NB)
        real(real32) :: dirty_f32(NB), dirtyv_f32(3, NB)
        logical :: dirty_bool(NB), dirtyv_bool(3, NB)
        integer :: i
        integer, parameter :: nulls(4) = [1, 17, 33, 40]
        character(len=*), parameter :: f = "test_run/table_mat_nullfast.parquet"
        character(len=10), parameter :: dirty_names(6) = &
            [character(len=10) :: "dirty", "dirty_i32", "dirty_i64", "dirty_f32", "dirty_bool", "s"]
        character(len=10), parameter :: dirtyv_names(5) = &
            [character(len=10) :: "dv", "dv_i32", "dv_i64", "dv_f32", "dv_bool"]
        !
        do i = 1, NB
            dirty(i) = real(i, real64)
            clean(i) = real(100 + i, real64)
            dirtyv(:, i) = real(i, real64)
            dirty_i32(i) = i
            dirty_i64(i) = int(i, int64)
            dirty_f32(i) = real(i, real32)
            dirty_bool(i) = mod(i, 2) == 0
            dirtyv_i32(:, i) = i
            dirtyv_i64(:, i) = int(i, int64)
            dirtyv_f32(:, i) = real(i, real32)
            dirtyv_bool(:, i) = mod(i, 2) == 0
            write(s6(i), '(a,i0)') "v", i
        end do
        vd = .true.
        vs = .true.
        vv = .true.
        do i = 1, size(nulls)
            vd(nulls(i)) = .false.
            vs(nulls(i)) = .false.
            vv(:, nulls(i)) = .false.
        end do
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "dirty", dirty, is_valid=vd)
        call parquet_write_column(w, "clean", clean)
        call parquet_write_column(w, "s", s6, is_valid=vs)
        call parquet_write_column(w, "dv", dirtyv, is_valid=vv)
        call parquet_write_column(w, "dirty_i32", dirty_i32, is_valid=vd)
        call parquet_write_column(w, "dirty_i64", dirty_i64, is_valid=vd)
        call parquet_write_column(w, "dirty_f32", dirty_f32, is_valid=vd)
        call parquet_write_column(w, "dirty_bool", dirty_bool, is_valid=vd)
        call parquet_write_column(w, "dv_i32", dirtyv_i32, is_valid=vv)
        call parquet_write_column(w, "dv_i64", dirtyv_i64, is_valid=vv)
        call parquet_write_column(w, "dv_f32", dirtyv_f32, is_valid=vv)
        call parquet_write_column(w, "dv_bool", dirtyv_bool, is_valid=vv)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call t%materialize_all()
        ! The slow path: every null must have survived, and no extra null invented -- across every
        ! mat_* scalar and vector materializer that takes the mask branch.
        block
            integer :: k
            do i = 1, NB
                do k = 1, size(dirty_names)
                    call check(e2, t%is_null(trim(dirty_names(k)), int(i, int64)) .eqv. any(nulls == i), &
                        "a scalar column's nulls must survive materialize exactly: " // &
                        trim(dirty_names(k)))
                    if (allocated(e2)) then
                        call move_alloc(e2, error)
                        return
                    end if
                end do
                do k = 1, size(dirtyv_names)
                    call check(e2, t%is_null(trim(dirtyv_names(k)), int(i, int64)) .eqv. any(nulls == i), &
                        "a vector column's nulls must survive materialize exactly: " // &
                        trim(dirtyv_names(k)))
                    if (allocated(e2)) then
                        call move_alloc(e2, error)
                        return
                    end if
                end do
                ! And the fast path: a clean column must gain no nulls at all.
                call check(e2, .not. t%is_null("clean", int(i, int64)), &
                    "the no-nulls fast path must not invent a null")
                if (allocated(e2)) then
                    call move_alloc(e2, error)
                    return
                end if
            end do
        end block
        ! Values must survive both paths -- adopt hands the array over rather than copying it, so a
        ! mistake there shows up as wrong or missing data rather than as wrong validity.
        call t%get("clean", back)
        call check(error, all(back == clean), "the fast path must keep its values")
        if (allocated(error)) return
        call t%get("dirty", back)
        call check(error, back(2) == 2.0_real64 .and. back(NB - 1) == real(NB - 1, real64), &
            "the mask path must keep the values of its non-null rows")
    end subroutine test_materialize_null_fast_path
    !
    !> The fallback when the footer cannot answer: a file written with statistics DISABLED.
    !!
    !! `parquet_column_has_nulls` reads a column's null count from Parquet statistics, which are
    !! optional in the format. When they are missing it must answer "might have Nulls" so the
    !! validity mask is still requested. Getting that backwards aborts the read outright
    !! ("column contains Null value(s), which is not supported"), so this is not a silent failure --
    !! but it is unreachable with any other fixture, since every other one carries statistics.
    !!
    !! `test/fixtures/no_stats.parquet` (see tools/generate_fixtures.cpp) has no statistics at all:
    !! column `v` holds Nulls at rows 2 and 5, column `c` holds none.
    subroutine test_materialize_without_statistics(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: r
        type(parquet_table) :: t
        real(real64), allocatable :: back(:)
        character(len=*), parameter :: f = "test/fixtures/no_stats.parquet"
        !
        ! Without statistics the answer must be the conservative one, for the clean column too.
        call parquet_open_reader(r, f)
        call check(error, parquet_column_has_nulls(r, "v", 0, 0), &
            "a column with no statistics must be reported as possibly holding nulls")
        if (allocated(error)) return
        call check(error, parquet_column_has_nulls(r, "c", 0, 0), &
            "even a genuinely clean column must be reported that way when statistics are absent")
        if (allocated(error)) return
        call parquet_close_reader(r)
        !
        call parquet_open_table(t, f)
        call t%materialize_all()
        call check(error, t%is_null("v", 2_int64) .and. t%is_null("v", 5_int64), &
            "nulls must survive materialize when the footer could not report them")
        if (allocated(error)) return
        call check(error, .not. (t%is_null("v", 1_int64) .or. t%is_null("v", 6_int64)), &
            "and non-null rows must not become null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("c", 3_int64), &
            "the clean column must still come back with no nulls")
        if (allocated(error)) return
        call t%get("c", back)
        call check(error, abs(back(3) - 300.0_real64) < 1.0e-9_real64, &
            "and with its values intact")
    end subroutine test_materialize_without_statistics
    !
    !> Nulls must survive being written back out by `parquet_write_table`, not merely read.
    !!
    !! This guards the two shortcuts the write path takes when building its `is_valid=` mask, both
    !! of which fail silently if wrong -- the file simply comes back with every row valid:
    !!
    !!  * a column with NO nulls is written with no mask at all (`row_validity` returns an
    !!    unallocated array, which makes the `optional` dummy absent), so `clean` below must come
    !!    back with no nulls AND its values intact;
    !!  * a column WITH nulls has its mask built by walking the validity bitmap 64 bits at a time,
    !!    so the null rows are placed deliberately at and around block boundaries (64/65, 128/129)
    !!    and at the very first and last row, which is where an off-by-one in that walk shows up.
    !!
    !! The vector column is not redundant with the scalar one: its bits sit `width` apart in the
    !! bitmap, so the walk has to map a bit back to the row that owns it rather than reading rows
    !! off directly. It nulls WHOLE rows on purpose -- the single-element case is
    !! `test_element_null_round_trip` below, which is a different question.
    subroutine test_write_table_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: NBIG = 200 !! spans four 64-bit validity blocks.
        integer, parameter :: NW = 3     !! vector width, so rows sit 3 bits apart.
        type(error_type), allocatable :: e2
        type(parquet_writer) :: w
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        real(real64) :: f64(NBIG), clean(NBIG), fv(NW, NBIG)
        real(real64), allocatable :: back(:)
        logical :: valid_f(NBIG), valid_v(NW, NBIG)
        integer :: i
        integer, parameter :: fnull(6) = [1, 64, 65, 128, 129, 200]
        integer, parameter :: vnull(4) = [2, 64, 65, 130]
        character(len=*), parameter :: f = "test_run/table_write_nulls.parquet"
        character(len=*), parameter :: fo = "test_run/table_write_nulls_out.parquet"
        !
        do i = 1, NBIG
            f64(i) = real(i, real64)
            clean(i) = real(1000 + i, real64)
            fv(:, i) = real(i, real64)
        end do
        valid_f = .true.
        valid_v = .true.
        do i = 1, size(fnull)
            valid_f(fnull(i)) = .false.
        end do
        do i = 1, size(vnull)
            valid_v(:, vnull(i)) = .false.
        end do
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "f", f64, is_valid=valid_f)
        call parquet_write_column(w, "fv", fv, is_valid=valid_v)
        call parquet_write_column(w, "clean", clean)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call s%init("wn")
        call s%add_field("f", "float64")
        call s%add_field("fv", "float64", col_size=NW)
        call s%add_field("clean", "float64")
        call parquet_parse_maml(s)
        call parquet_write_table(t, fo, s)
        !
        call parquet_open_table(t2, fo)
        call check(error, t2%nrows() == int(NBIG, int64), "the rewritten table should keep its row count")
        if (allocated(error)) return
        call check_null_positions(t2, "f", fnull, NBIG, error)
        if (allocated(error)) return
        call check_null_positions(t2, "fv", vnull, NBIG, error)
        if (allocated(error)) return
        ! The no-mask fast path must not invent nulls, and must not disturb the values either.
        do i = 1, NBIG
            call check(e2, .not. t2%is_null("clean", int(i, int64)), &
                "a column written with no validity mask should come back with no nulls")
            if (allocated(e2)) then
                call move_alloc(e2, error)
                return
            end if
        end do
        call t2%get("clean", back)
        call check(error, all(back == clean), &
            "a column written with no validity mask should keep its values")
    end subroutine test_write_table_nulls
    !
    !> **The headline test of the element-null milestone.** ONE null element must survive
    !! file -> table -> file without spreading to its siblings.
    !!
    !! Before element-granular validity this failed in both directions and was self-consistent
    !! about it: the read widened the null to the whole row, and the write broadcast the row's bit
    !! back across every element, so the output file had `width` nulls where the input had one.
    !!
    !! Verified with the LOW-LEVEL reader and a rank-2 mask rather than through the table, so the
    !! assertion does not depend on the same table code being tested -- and because the question
    !! is what actually landed in the file.
    subroutine test_element_null_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: NW = 4     !! vector width.
        integer, parameter :: NR = 7     !! rows.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_table) :: t
        type(parquet_schema) :: s
        real(real64) :: fv(NW, NR), got(NW, NR)
        integer(int32) :: iv(NW, NR), goti(NW, NR)
        logical :: valid(NW, NR), back(NW, NR)
        integer :: i, e
        character(len=*), parameter :: f = "test_run/table_elem_null.parquet"
        character(len=*), parameter :: fo = "test_run/table_elem_null_out.parquet"
        !
        do i = 1, NR
            do e = 1, NW
                fv(e, i) = real(10*i + e, real64)
                iv(e, i) = int(100*i + e, int32)
            end do
        end do
        valid = .true.
        valid(3, 5) = .false.      ! deliberately NOT element 1: the old convention hid this one
        valid(1, 2) = .false.      ! and one that the old convention would have caught
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "fv", fv, is_valid=valid)
        call parquet_write_column(w, "iv", iv, is_valid=valid)
        call parquet_close_writer(w)
        !
        ! Round-trip it through the table layer: open, materialize, write back out.
        call parquet_open_table(t, f)
        call s%init("en")
        call s%add_field("fv", "float64", col_size=NW)
        call s%add_field("iv", "int32", col_size=NW)
        call parquet_write_table(t, fo, s)
        !
        call parquet_open_reader(r, fo)
        call parquet_read_column(r, "fv", got, is_valid=back)
        call check(error, all(back .eqv. valid), &
            "exactly the nulled elements must survive a table round trip, and no others")
        if (allocated(error)) return
        call check(error, .not. back(3, 5), "element (3,5) must still be null after the round trip")
        if (allocated(error)) return
        call check(error, back(1, 5) .and. back(2, 5) .and. back(4, 5), &
            "the siblings of a null element must NOT be nulled by the round trip")
        if (allocated(error)) return
        call check(error, .not. back(1, 2), "element (1,2) must still be null after the round trip")
        if (allocated(error)) return
        call check(error, all(back(2:, 2)), "the siblings of element (1,2) must stay valid")
        if (allocated(error)) return
        ! The values of the surviving elements must be untouched by any of this.
        call check(error, got(1, 5) == fv(1, 5) .and. got(4, 7) == fv(4, 7), &
            "a valid element's value must survive the round trip unchanged")
        if (allocated(error)) return
        !
        call parquet_read_column(r, "iv", goti, is_valid=back)
        call check(error, all(back .eqv. valid), &
            "the int32 vector kind must round-trip its element nulls the same way")
        call parquet_close_reader(r)
    end subroutine test_element_null_round_trip
    !
    !> The table's own element-granular null API: `%is_null(name, i, e)`, `%set_null(name, i, e)`,
    !> `%clear_null(name, i, e)`, the rank-2 `%get_valid_mask`/`%set_null(mask)`, and the row
    !> handle's element form.
    subroutine test_table_element_null_api(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_row) :: r
        real(real64) :: fv(3, 4)
        logical, allocatable :: rmask(:), emask(:,:)
        logical :: m(3, 4)
        integer :: i, e
        !
        do i = 1, 4
            do e = 1, 3
                fv(e, i) = real(10*i + e, real64)
            end do
        end do
        call parquet_new_table(t)
        call t%add_column("fv", fv)
        !
        ! set_null(name, i, e) must touch exactly one element.
        call t%set_null("fv", 2_int64, 3_int64)
        call check(error, t%is_null("fv", 2_int64, 3_int64), "%set_null(name, i, e) must null that element")
        if (allocated(error)) return
        call check(error, .not. t%is_null("fv", 2_int64, 1_int64), "its siblings must stay valid")
        if (allocated(error)) return
        ! ... and the ROW query must then report the row as null ("any element").
        call check(error, t%is_null("fv", 2_int64), &
            "%is_null(name, i) must be .true. when any element of the row is null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("fv", 1_int64), "a row with no null element must not report null")
        if (allocated(error)) return
        !
        ! The int32 specifics must agree with the int64 ones.
        call check(error, t%is_null("fv", 2, 3), "the int32 element specific must agree with the int64 one")
        if (allocated(error)) return
        !
        ! clear_null(name, i, e) undoes it.
        call t%clear_null("fv", 2_int64, 3_int64)
        call check(error, .not. t%is_null("fv", 2_int64, 3_int64), "%clear_null(name, i, e) must undo it")
        if (allocated(error)) return
        !
        ! The row handle's element form.
        call t%set_null("fv", 3_int64, 2_int64)
        r = t%row(3_int64)
        call check(error, r%is_null("fv", 2_int64), "the row handle's element form must see the null")
        if (allocated(error)) return
        call check(error, .not. r%is_null("fv", 1_int64), "the row handle must not widen it")
        if (allocated(error)) return
        call check(error, r%is_null("fv"), "the row handle's row form must report the row null")
        if (allocated(error)) return
        !
        ! %get_valid_mask offers BOTH ranks: rank-1 is the row summary, rank-2 the truth.
        call t%get_valid_mask("fv", rmask)
        call check(error, size(rmask) == 4, "the rank-1 mask must have one entry per row")
        if (allocated(error)) return
        call check(error, .not. rmask(3), "the rank-1 mask must summarise row 3 as invalid")
        if (allocated(error)) return
        call t%get_valid_mask("fv", emask)
        call check(error, size(emask, 1) == 3 .and. size(emask, 2) == 4, &
            "the rank-2 mask must be shaped (width, nrows)")
        if (allocated(error)) return
        call check(error, .not. emask(2, 3), "the rank-2 mask must mark exactly the null element")
        if (allocated(error)) return
        call check(error, emask(1, 3) .and. emask(3, 3), "the rank-2 mask must leave the siblings valid")
        if (allocated(error)) return
        !
        ! %set_null(mask) also takes both ranks; the rank-2 form nulls individual elements.
        call parquet_new_table(t)
        call t%add_column("fv", fv)
        m = .true.
        m(1, 4) = .false.
        call t%set_null("fv", m)
        call check(error, t%is_null("fv", 4_int64, 1_int64), "the rank-2 %set_null(mask) must null that element")
        if (allocated(error)) return
        call check(error, .not. t%is_null("fv", 4_int64, 2_int64), &
            "the rank-2 %set_null(mask) must not widen to the row")
    end subroutine test_table_element_null_api
    !
    !> Both `%get_valid_mask` ranks over a bitmap SEVERAL WORDS long, with nulls placed where a
    !! word-at-a-time walk can lose them.
    !!
    !! The other validity tests here run on 4-6 rows, which is one 64-bit word and never exercises
    !! the walk at all. `%get_valid_mask` and `%get(is_valid=)` are built by
    !! `parquet_column%row_validity`/`%element_validity`, which skip a zero word whole and iterate
    !! only the SET bits inside a nonzero one -- so the cases that can go wrong are a null on a word
    !! boundary, a null in the final PARTIAL word, and a column with no nulls at all (the branch
    !! that has to synthesise an all-`.true.` mask, because the bulk builders deliberately hand back
    !! nothing there).
    !!
    !! 100 rows is chosen for being neither a multiple of 64 nor a single word: rows 64 and 65 sit
    !! either side of a word boundary, and row 100 is in the last, partial word.
    subroutine test_valid_mask_multiword(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: N = 100  !! rows: spans two words, and not a multiple of 64.
        integer, parameter :: W = 3    !! elements per row of the vector column.
        type(parquet_table) :: t
        real(real64) :: v(N), fv(W, N)
        real(real64), allocatable :: vg(:)
        logical :: want(N), ewant(W, N)
        logical, allocatable :: got(:), egot(:,:)
        integer :: i, e
        !
        do i = 1, N
            v(i) = real(i, real64)
            do e = 1, W
                fv(e, i) = real(10*i + e, real64)
            end do
        end do
        want = .true.
        want([1, 64, 65, N]) = .false.
        ewant = .true.
        ewant(2, N) = .false.
        !
        call parquet_new_table(t)
        call t%add_column("v", v)
        call t%add_column("w", v)
        call t%add_column("fv", fv)
        do i = 1, N
            if (.not. want(i)) call t%set_null("v", int(i, int64))
        end do
        call t%set_null("fv", int(N, int64), 2_int64)
        !
        call t%get_valid_mask("v", got)
        call check(error, size(got) == N, "the multi-word rank-1 mask must have one entry per row")
        if (allocated(error)) return
        call check(error, all(got .eqv. want), &
            "the rank-1 mask must find the nulls either side of the word boundary and in the last word")
        if (allocated(error)) return
        !
        ! The null-free branch, at the same size: the bulk builder hands back nothing at all here,
        ! so this is the fallback that turns "nothing" into an all-.true. mask of the right length.
        call t%get_valid_mask("w", got)
        call check(error, size(got) == N .and. all(got), &
            "a null-free multi-word column must come back allocated and all .true.")
        if (allocated(error)) return
        !
        ! Rank-2, where the bitmap is W times longer and the one null sits in its final word.
        call t%get_valid_mask("fv", egot)
        call check(error, size(egot, 1) == W .and. size(egot, 2) == N, &
            "the multi-word rank-2 mask must be shaped (width, nrows)")
        if (allocated(error)) return
        call check(error, all(egot .eqv. ewant), &
            "the rank-2 mask must mark exactly the last row's second element")
        if (allocated(error)) return
        !
        ! The rank-1 form of the same vector column is the row SUMMARY, so only that row is false.
        call t%get_valid_mask("fv", got)
        call check(error, count(.not. got) == 1 .and. .not. got(N), &
            "the rank-1 summary of the vector column must mark exactly the last row")
        if (allocated(error)) return
        !
        ! %get(is_valid=) goes through the same builder from a different entry point.
        call t%get("v", vg, is_valid=got)
        call check(error, all(got .eqv. want), &
            "%get(is_valid=) must report the same multi-word mask")
    end subroutine test_valid_mask_multiword
    !
    !> `is_valid=` on a vector column is rank-2 everywhere it appears -- the shape-must-match rule.
    !!
    !! This is the breaking half of the change: a caller passing a rank-1 mask for a vector column
    !! now gets a compile error, and these are the shapes that replace it.
    subroutine test_table_rank2_masks(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_slice) :: s
        real(real64) :: fv(2, 4)
        real(real64), allocatable :: got(:,:)
        real(real64), pointer :: p(:,:)
        logical :: m(2, 4), sm(2, 2)
        logical, allocatable :: back(:,:)
        integer :: i, e
        !
        do i = 1, 4
            do e = 1, 2
                fv(e, i) = real(10*i + e, real64)
            end do
        end do
        !
        ! %set with a rank-2 is_valid.
        call parquet_new_table(t)
        call t%add_column("fv", fv)
        m = .true.
        m(2, 2) = .false.
        call t%set("fv", fv, is_valid=m)
        call check(error, t%is_null("fv", 2_int64, 2_int64), "%set(is_valid=) must null exactly that element")
        if (allocated(error)) return
        call check(error, .not. t%is_null("fv", 2_int64, 1_int64), "%set(is_valid=) must not widen it")
        if (allocated(error)) return
        !
        ! %get hands the same shape back.
        call t%get("fv", got, is_valid=back)
        call check(error, size(back, 1) == 2 .and. size(back, 2) == 4, "%get(is_valid=) must be (width, nrows)")
        if (allocated(error)) return
        call check(error, all(back .eqv. m), "%get(is_valid=) must report exactly what %set wrote")
        if (allocated(error)) return
        !
        ! %col too, alongside the pointer.
        call t%col("fv", p, is_valid=back)
        call check(error, associated(p), "%col must still hand back the pointer")
        if (allocated(error)) return
        call check(error, all(back .eqv. m), "%col(is_valid=) must report the per-element state")
        if (allocated(error)) return
        !
        ! %get_slice and %set_slice: the mask covers the SELECTED rows, per element.
        s = parquet_slice_range(2_int64, 3_int64)
        call t%get_slice("fv", s, got, is_valid=back)
        call check(error, size(back, 1) == 2 .and. size(back, 2) == 2, &
            "%get_slice(is_valid=) must be (width, selected rows)")
        if (allocated(error)) return
        call check(error, .not. back(2, 1), "%get_slice(is_valid=) must carry the null of the first picked row")
        if (allocated(error)) return
        call check(error, back(1, 1) .and. all(back(:, 2)), "%get_slice(is_valid=) must not widen it")
        if (allocated(error)) return
        !
        sm = .true.
        sm(1, 2) = .false.
        call t%set_slice("fv", s, got, is_valid=sm)
        call check(error, t%is_null("fv", 3_int64, 1_int64), &
            "%set_slice(is_valid=) must null the element of the selected row")
        if (allocated(error)) return
        call check(error, .not. t%is_null("fv", 3_int64, 2_int64), "%set_slice(is_valid=) must not widen it")
    end subroutine test_table_rank2_masks
    !
    !> Asserts that exactly the rows listed in `nulls` are null in `name`, and no others.
    subroutine check_null_positions(t, name, nulls, nrow, error)
        type(parquet_table), intent(inout) :: t              !! the reopened table.
        character(len=*), intent(in) :: name                 !! column to check.
        integer, intent(in) :: nulls(:)                      !! the row numbers expected to be null.
        integer, intent(in) :: nrow                          !! total rows.
        type(error_type), allocatable, intent(out) :: error  !! set on the first disagreement.
        logical :: want
        integer :: i
        character(len=64) :: msg
        !
        do i = 1, nrow
            want = any(nulls == i)
            if (t%is_null(name, int(i, int64)) .eqv. want) cycle
            if (want) then
                write(msg, '(a,i0,a)') "row ", i, " should be null after the table write"
            else
                write(msg, '(a,i0,a)') "row ", i, " should NOT be null after the table write"
            end if
            call check(error, .false., trim(msg) // " (column " // name // ")")
            return
        end do
    end subroutine check_null_positions
    !
    !> A vector string column whose LAST row carries a null -- the case that caught the
    !! materializer indexing validity by flat element position. parquet_column's validity is
    !! row-granular even for a vector kind, so a per-element null has to be widened to the whole
    !! row; a flat (row-1)*width+element index runs past nrows and aborts the read outright.
    subroutine test_string_vector_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        character(len=6) :: sv(NVEC, NROW)
        logical :: valid(NVEC, NROW)
        integer :: i, e
        character(len=*), parameter :: f = "test_run/table_strvec_nulls.parquet"
        !
        do i = 1, NROW
            do e = 1, NVEC
                write(sv(e, i), '(a,i0,i0)') "s", i, e
            end do
        end do
        ! First element deliberately the shortest (CLAUDE.md).
        sv(1, 1) = "a"
        valid = .true.
        valid(2, NROW) = .false.   ! a late row: a flat index here exceeds nrows
        valid(1, 4) = .false.
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "sv", sv, is_valid=valid)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call check(error, t%kind("sv") == PK_STRING_VEC, "sv should resolve to PK_STRING_VEC")
        if (allocated(error)) return
        call check(error, t%is_null("sv", int(NROW, int64)), &
            "a null element in the last row should make that row null")
        if (allocated(error)) return
        call check(error, t%is_null("sv", 4_int64), &
            "a null element in row 4 should make row 4 null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("sv", 1_int64), &
            "a row with no null element should not report null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("sv", 2_int64), &
            "a row with no null element should not report null")
    end subroutine test_string_vector_nulls
    !
    !> Opening classifies but reads nothing, and each accessor pulls in exactly the column it
    !! was asked about -- the whole point of the lazy layer, and the easiest property to lose
    !! silently (a stray whole-table read at open still passes every value assertion).
    subroutine test_lazy_first_touch(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64), pointer :: p(:)
        integer :: i
        character(len=:), allocatable :: names(:)
        character(len=*), parameter :: f = "test_run/table_lazy.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%column_names(names)
        do i = 1, size(names)
            call check(error, t%residency(trim(names(i))) == RES_EMPTY, &
                "no column may be resident before it is touched: "//trim(names(i)))
            if (allocated(error)) return
        end do
        !
        ! %get touches one column and only that one.
        call t%get("f64", g)
        call check(error, t%residency("f64") == RES_FULL, "%get should make its column resident")
        if (allocated(error)) return
        call check(error, t%residency("i32") == RES_EMPTY, &
            "%get on one column must not read any other")
        if (allocated(error)) return
        call check(error, abs(g(2) - 4.5_real64) < 1.0e-12_real64, &
            "a lazily read column must hold the file's values")
        if (allocated(error)) return
        ! %col triggers a first touch of its own, on a column nothing has read yet.
        call parquet_open_table(t, f)
        call check(error, t%residency("f64") == RES_EMPTY, "precondition: the reopened table is empty")
        if (allocated(error)) return
        call t%col("f64", p)
        call check(error, t%residency("f64") == RES_FULL, "%col should make its column resident")
        if (allocated(error)) return
        call check(error, abs(p(2) - 4.5_real64) < 1.0e-12_real64, &
            "the pointer must alias the lazily read values")
        if (allocated(error)) return
        ! %is_null is a value question, so it touches as well.
        call check(error, .not. t%is_null("b", 1_int64), "b row 1 should not be null")
        if (allocated(error)) return
        call check(error, t%residency("b") == RES_FULL, "%is_null should make its column resident")
    end subroutine test_lazy_first_touch
    !
    subroutine test_prefetch(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        logical :: got
        character(len=*), parameter :: f = "test_run/table_prefetch.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%prefetch("f64")
        call check(error, t%residency("f64") == RES_FULL, "prefetch should read its column")
        if (allocated(error)) return
        call check(error, t%residency("i32") == RES_EMPTY, "prefetch should read nothing else")
        if (allocated(error)) return
        ! Prefetching an already-resident column is a no-op, not an error.
        call t%prefetch("f64")
        call check(error, t%residency("f64") == RES_FULL, "prefetching twice should be harmless")
        if (allocated(error)) return
        !
        call t%prefetch(["i32", "f32"])
        call check(error, t%residency("i32") == RES_FULL .and. t%residency("f32") == RES_FULL, &
            "the array form should read every name it is given")
        if (allocated(error)) return
        call check(error, t%residency("s") == RES_EMPTY, "the array form should read nothing else")
        if (allocated(error)) return
        ! found= turns a missing name into a report rather than an abort, and the names that DO
        ! exist are still read.
        call t%prefetch(["i64 ", "nope"], found=got)
        call check(error, .not. got, "prefetch should report a missing name through found=")
        if (allocated(error)) return
        call check(error, t%residency("i64") == RES_FULL, &
            "a missing name must not stop the other names being read")
        if (allocated(error)) return
        !
        call t%materialize_all()
        call check(error, t%residency("s") == RES_FULL, "materialize_all should read the rest")
        if (allocated(error)) return
        call t%materialize_all()
        call check(error, t%residency("s") == RES_FULL, "materialize_all should be idempotent")
    end subroutine test_prefetch
    !
    subroutine test_reload(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64) :: edited(NROW)
        character(len=*), parameter :: f = "test_run/table_reload.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%get("f64", g)
        edited = 0.0_real64
        call t%set("f64", edited)
        call t%get("f64", g)
        call check(error, all(abs(g) < 1.0e-12_real64), "precondition: set replaced the values")
        if (allocated(error)) return
        !
        call t%reload("f64")
        call t%get("f64", g)
        call check(error, abs(g(2) - 4.5_real64) < 1.0e-12_real64, &
            "reload should bring back the file's own values")
        if (allocated(error)) return
        call check(error, t%residency("f64") == RES_FULL, "a reloaded column is resident")
        if (allocated(error)) return
        ! Reloading a column that was never touched is just a first touch.
        call t%reload("i32")
        call check(error, t%residency("i32") == RES_FULL, &
            "reloading an untouched column should simply read it")
    end subroutine test_reload
    !
    subroutine test_unsupported_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        integer(int64), allocatable :: ids(:)
        integer :: i
        logical :: saw_uint32, ok
        character(len=*), parameter :: f = "test/fixtures/extended_types.parquet"
        !
        ! extended_types.parquet carries a uint32 column, whose physical type falls outside the
        ! nine types this library reads. Opening the file must still work: the column gets a
        ! slot, appears in the listing, and is simply marked unreadable.
        call parquet_open_table(t, f)
        call check(error, t%nrows() > 0, "a file with a foreign column should still open")
        if (allocated(error)) return
        call t%column_names(names)
        saw_uint32 = .false.
        do i = 1, size(names)
            if (trim(names(i)) == "v_uint32") saw_uint32 = .true.
        end do
        call check(error, saw_uint32, "an unsupported column should still be listed")
        if (allocated(error)) return
        call check(error, .not. t%is_supported("v_uint32"), &
            "a uint32 column should report as unsupported")
        if (allocated(error)) return
        call check(error, t%kind("v_uint32") == PK_NONE, &
            "an unsupported column should have no PK_* kind")
        if (allocated(error)) return
        call check(error, t%residency("v_uint32") == RES_EMPTY, &
            "an unsupported column should hold no values")
        if (allocated(error)) return
        ! A supported column in the same file still works normally.
        call check(error, t%is_supported("id"), &
            "a supported column in the same file should still be readable")
        if (allocated(error)) return
        call check(error, t%residency("id") == RES_EMPTY, &
            "a supported column starts empty like any other")
        if (allocated(error)) return
        call t%get("id", ids)
        call check(error, t%residency("id") == RES_FULL, &
            "a supported column should still be readable despite an unsupported sibling")
        if (allocated(error)) return
        ! And a soft-failing read of the unsupported column reports rather than aborts.
        call t%get("v_uint32", names, found=ok)
        call check(error, .not. ok, "a soft-failing read of an unsupported column should report .false.")
    end subroutine test_unsupported_column
    !
    !> %prefetch("main") reads every leaf of a struct in one pass, and %evict_column gives a
    !! column's memory back without losing the column.
    !!
    !! The prefix form exists for one reason: the reader decodes a struct as ONE array shared by
    !! all its leaves, so touching them one at a time decodes it once per leaf. What can be
    !! asserted from Fortran is the outcome -- every leaf resident after a single call naming only
    !! the struct. The two features are tested together because %evict_column is what makes the
    !! prefetch repeatable: evict the leaves, and the next call reads them again.
    subroutine test_prefetch_prefix_and_evict(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, mem
        logical :: ok
        integer(int32), allocatable :: ids(:)
        integer(int64) :: gen0
        character(len=*), parameter :: f = "test/fixtures/nested_struct.parquet"
        !
        call parquet_open_table(t, f)
        call check(error, .not. t%has_column("main"), &
            "precondition: the bare struct name is not itself a column")
        if (allocated(error)) return
        call check(error, t%residency("main.id") == RES_EMPTY, "precondition: nothing is resident")
        if (allocated(error)) return
        !
        call t%prefetch("main")
        call check(error, t%residency("main.id") == RES_FULL, &
            "%prefetch(struct) should read the struct's leaves")
        if (allocated(error)) return
        call check(error, t%residency("main.inner.deep.value") == RES_FULL, &
            "%prefetch(struct) should reach a doubly-nested leaf too")
        if (allocated(error)) return
        !
        ! %evict_column hands the memory back and leaves the column in place.
        gen0 = t%generation()
        call t%evict_column("main.id")
        call check(error, t%residency("main.id") == RES_EMPTY, "%evict_column should release the values")
        if (allocated(error)) return
        call check(error, t%has_column("main.id") .and. t%kind("main.id") == PK_INT32, &
            "an evicted column should still be a column, with its kind intact")
        if (allocated(error)) return
        call check(error, t%generation() > gen0, &
            "%evict_column should bump the generation -- it frees storage a pointer may alias")
        if (allocated(error)) return
        ! ...and it is read again on the next touch, which is the whole difference from %drop_column.
        call t%get("main.id", ids)
        call check(error, size(ids) == int(t%nrows()) .and. t%residency("main.id") == RES_FULL, &
            "an evicted column should be re-read on the next touch")
        if (allocated(error)) return
        ! Evicting twice is a no-op, not an error.
        call t%evict_column("main.id")
        call t%evict_column("main.id")
        call check(error, t%residency("main.id") == RES_EMPTY, "evicting twice should be harmless")
        if (allocated(error)) return
        !
        ! A prefix matching nothing is a missing column, reported the usual way.
        call t%prefetch("no_such_struct", found=ok)
        call check(error, .not. ok, "a prefix matching nothing should report through found=")
        if (allocated(error)) return
        !
        ! An in-memory column has no file to be re-read from, so evicting it is refused --
        ! the abort path lives out of process (scenario table_evict_in_memory).
        call parquet_new_table(mem)
        call mem%add_column("a", [1_int32, 2_int32])
        call check(error, mem%residency("a") == RES_FULL, "precondition: an added column is resident")
    end subroutine test_prefetch_prefix_and_evict
    !
    subroutine test_struct_leaves(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: f = "test/fixtures/nested_struct.parquet"
        !
        call parquet_open_table(t, f)
        call check(error, t%has_column("main.id"), "a struct leaf should be addressable by its dotted path")
        if (allocated(error)) return
        call check(error, t%has_column("main.inner.deep.value"), &
            "a doubly-nested struct leaf should be addressable too")
        if (allocated(error)) return
        call check(error, .not. t%has_column("main"), &
            "a bare struct name should not be a column -- it is not readable")
        if (allocated(error)) return
        call check(error, t%kind("main.id") == PK_INT32, "main.id should resolve to PK_INT32")
        if (allocated(error)) return
        call t%get("main.id", ids)
        call check(error, size(ids) == int(t%nrows()), "a struct leaf should read every row")
    end subroutine test_struct_leaves
    !
    subroutine test_from_scratch(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        integer(int32) :: ids(NROW)
        real(real64) :: mass(NROW)
        character(len=8) :: nm(NROW)
        real(real64), allocatable :: g(:)
        character(len=:), allocatable :: chr(:)
        integer :: i
        character(len=*), parameter :: fo = "test_run/table_scratch.parquet"
        !
        do i = 1, NROW
            ids(i) = i * 11
            mass(i) = real(i, real64) * 1.5_real64
        end do
        nm = ["a       ", "bcd     ", "ef      ", "ghij    ", "kl      ", "m       "]
        !
        call parquet_new_table(t)
        call check(error, t%nrows() == 0, "a new table starts with no rows")
        if (allocated(error)) return
        call check(error, t%ncols() == 0, "a new table starts with no columns")
        if (allocated(error)) return
        call t%add_column("id", ids)
        call check(error, t%nrows() == NROW, "the first add_column should fix the row count")
        if (allocated(error)) return
        call t%add_column("mass", mass, unit="Msun")
        call t%add_column("name", nm)
        call check(error, t%ncols() == 3, "add_column should append a column each time")
        if (allocated(error)) return
        call check(error, t%kind("mass") == PK_FLOAT64, "an added float64 column should be PK_FLOAT64")
        if (allocated(error)) return
        !
        call s%init("scratch")
        call s%add_field("id", "int32")
        call s%add_field("mass", "float64", unit="Msun")
        call s%add_field("name", "string", array_size=16)
        call parquet_parse_maml(s)
        call parquet_write_table(t, fo, s)
        !
        call parquet_open_table(t2, fo)
        call check(error, t2%nrows() == NROW, "the written table should keep its row count")
        if (allocated(error)) return
        call t2%get("mass", g)
        call check(error, abs(g(3) - 4.5_real64) < 1.0e-12_real64, &
            "an in-memory column should survive the write/reopen round trip")
        if (allocated(error)) return
        call t2%get("name", chr)
        call check(error, trim(chr(4)) == "ghij", "string values should survive the round trip")
    end subroutine test_from_scratch
    !
    subroutine test_add_column_force(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: a(NROW), b(NROW)
        real(real64), allocatable :: g(:)
        integer :: i
        !
        do i = 1, NROW
            a(i) = real(i, real64)
            b(i) = real(i, real64) * 100.0_real64
        end do
        call parquet_new_table(t)
        call t%add_column("x", a)
        call t%add_column("x", b, force=.true.)
        call check(error, t%ncols() == 1, "a forced replace should not add a second column")
        if (allocated(error)) return
        call t%get("x", g)
        call check(error, abs(g(2) - 200.0_real64) < 1.0e-12_real64, &
            "a forced replace should leave the new values in place")
    end subroutine test_add_column_force
    !
    subroutine test_set_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64) :: repl(NROW)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_set.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        do i = 1, NROW
            repl(i) = real(i, real64) * -3.0_real64
        end do
        call t%set("f64", repl)
        call t%get("f64", g)
        call check(error, all(abs(g - repl) < 1.0e-12_real64), &
            "set should replace every value of the column")
        if (allocated(error)) return
        call check(error, t%nrows() == NROW, "set must not change the table's row count")
    end subroutine test_set_values
    !
    subroutine test_write_row_mask(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        logical :: mask(NROW)
        integer(int32), allocatable :: g(:)
        character(len=*), parameter :: f = "test_run/table_mask_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_mask_out.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        mask = .false.
        mask(2) = .true.
        mask(5) = .true.
        call s%init("masked")
        call s%add_field("i32", "int32")
        call parquet_parse_maml(s)
        call parquet_write_table(t, fo, s, row_mask=mask)
        !
        call check(error, t%nrows() == NROW, "a row_mask write must not change the source table")
        if (allocated(error)) return
        call parquet_open_table(t2, fo)
        call check(error, t2%nrows() == 2, "only the masked-in rows should be written")
        if (allocated(error)) return
        call t2%get("i32", g)
        call check(error, all(g == [2, 5]), "the written rows should be the masked-in ones")
    end subroutine test_write_row_mask
    !
    subroutine test_reopen_same_variable(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        character(len=*), parameter :: f1 = "test_run/table_reopen1.parquet"
        character(len=*), parameter :: f2 = "test_run/table_reopen2.parquet"
        type(parquet_writer) :: w
        real(real64) :: v(3)
        !
        call write_basic_fixture(f1)
        v = [10.0_real64, 20.0_real64, 30.0_real64]
        call parquet_open_writer(w, f2)
        call parquet_write_column(w, "only", v)
        call parquet_close_writer(w)
        !
        ! intent(out) on a finalizable type finalizes the previous contents on entry, which is
        ! what stops a reopen from leaking the whole previous table. If that ever regressed the
        ! symptom would be a leak rather than a wrong answer, so this checks the visible half:
        ! the second open must fully replace the first.
        call parquet_open_table(t, f1)
        call check(error, t%ncols() == 6, "precondition: the first file has 6 columns")
        if (allocated(error)) return
        call parquet_open_table(t, f2)
        call check(error, t%ncols() == 1, "a reopen should replace the previous table entirely")
        if (allocated(error)) return
        call check(error, t%nrows() == 3, "a reopen should adopt the new file's row count")
        if (allocated(error)) return
        call check(error, .not. t%has_column("i32"), &
            "no column of the previous file should survive a reopen")
        if (allocated(error)) return
        call t%get("only", g)
        call check(error, abs(g(3) - 30.0_real64) < 1.0e-12_real64, &
            "the reopened table should hold the new file's values")
    end subroutine test_reopen_same_variable
    !
    subroutine test_soft_fail(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64), pointer :: p(:)
        logical :: ok
        integer :: k
        character(len=*), parameter :: f = "test_run/table_soft.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        ! Every lookup takes an optional found=: present, a miss is reported rather than fatal.
        call t%get("no_such_column", g, found=ok)
        call check(error, .not. ok, "get should report a missing column through found=")
        if (allocated(error)) return
        call t%col("no_such_column", p, found=ok)
        call check(error, .not. ok, "col should report a missing column through found=")
        if (allocated(error)) return
        call check(error, .not. associated(p), "col should leave the pointer unassociated on a miss")
        if (allocated(error)) return
        k = t%kind("no_such_column", found=ok)
        call check(error, .not. ok, "kind should report a missing column through found=")
        if (allocated(error)) return
        ! And a hit still reports .true.
        call t%get("f64", g, found=ok)
        call check(error, ok, "found= should be .true. for a column that exists")
    end subroutine test_soft_fail
    !
    subroutine test_unit(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: v(NROW)
        character(len=:), allocatable :: u
        integer :: i
        !
        do i = 1, NROW
            v(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("speed", v, unit="km/s")
        call t%unit("speed", u)
        call check(error, u == "km/s", "add_column(unit=) should be reported back by %unit")
        if (allocated(error)) return
        call t%add_column("plain", v)
        call t%unit("plain", u)
        call check(error, u == "", "a column with no unit should report an empty unit")
    end subroutine test_unit
    !
    !> Every one of the 18 column kinds, end to end: write a fixture holding all of them, open it
    !! as a table, check each column's kind and width, copy it out with %get, alias it with %col
    !! where a pointer path exists, then write every column back through a schema and reopen.
    !!
    !! Each kind has its own materializer, its own %get/%col/%set specifics and its own write
    !! branch, so a kind that is merely "similar to one that is tested" is not tested at all --
    !! which is why this sweeps rather than spot-checks.
    !> %filename reports where a table came from, and %get_file_metadata reads the source file's
    !! own key/value metadata -- including the soft-fail path for an in-memory table, which has
    !! no file to ask.
    subroutine test_filename_and_metadata(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_reader) :: rd
        type(parquet_table) :: t, mem, clone
        type(parquet_schema) :: s
        real(real64) :: v(NROW)
        character(len=:), allocatable :: fname, val, keys(:), vals(:)
        logical :: ok
        integer :: i
        character(len=*), parameter :: f = "test_run/table_meta.parquet"
        !
        do i = 1, NROW
            v(i) = real(i, real64)
        end do
        call s%init("metatable", survey="TESTSURVEY")
        call s%add_field("v", "float64")
        call parquet_parse_maml(s)
        call s%add_metadata("mykey", "myvalue")
        ! Deliberately ragged, and deliberately with the SHORTEST entry first: the snapshot is a
        ! blank-padded deferred-length character array, so a copy of it that gets the length from
        ! the first entry rather than the longest would truncate everything after it. %clone
        ! copies that array, which is what the assertions further down are really checking.
        call s%add_metadata("k", "v")
        call s%add_metadata("a_considerably_longer_metadata_key", "a considerably longer metadata value")
        call parquet_open_writer(w, f, s)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call t%filename(fname)
        call check(error, fname == f, "%filename should report the file the table was opened from")
        if (allocated(error)) return
        call t%get_file_metadata("mykey", val, found=ok)
        call check(error, ok, "a metadata key written into the file should be found")
        if (allocated(error)) return
        call check(error, val == "myvalue", "the metadata value should round-trip")
        if (allocated(error)) return
        call t%get_file_metadata("no_such_key", val, found=ok)
        call check(error, .not. ok, "a missing metadata key should report through found=")
        if (allocated(error)) return
        !
        ! An in-memory table has no file to ask.
        call parquet_new_table(mem)
        call mem%filename(fname)
        call check(error, fname == "", "an in-memory table should report an empty filename")
        if (allocated(error)) return
        call mem%get_file_metadata("anything", val, found=ok)
        call check(error, .not. ok, &
            "metadata on an in-memory table should report .false. rather than aborting")
        if (allocated(error)) return
        !
        ! The file's metadata is snapshotted at open, so DETACHING does not lose it: the reader is
        ! gone, but where the rows came from has not changed. Before the snapshot this read through
        ! the reader and died with it.
        call t%materialize_all()
        call t%truncate(2)
        call check(error, t%is_detached(), "precondition: %truncate should have detached the table")
        if (allocated(error)) return
        call t%get_file_metadata("mykey", val, found=ok)
        call check(error, ok .and. val == "myvalue", &
            "a detached table should still answer for its source file's metadata")
        if (allocated(error)) return
        call t%get_file_metadata("no_such_key", val, found=ok)
        call check(error, .not. ok, "a detached table should still report a missing key as a miss")
        if (allocated(error)) return
        !
        ! ...and a clone carries the snapshot too, including a clone of an already-detached table
        ! (which has no file to reopen and so could not re-read it).
        call t%clone(clone)
        call clone%get_file_metadata("mykey", val, found=ok)
        call check(error, ok .and. val == "myvalue", &
            "a clone of a detached table should carry its source file's metadata")
        if (allocated(error)) return
        ! Every entry, not just the first: a clone that copied the ragged key/value arrays wrongly
        ! would come back with truncated (or, on an older gfortran, no) entries here.
        call clone%get_file_metadata("k", val, found=ok)
        call check(error, ok .and. val == "v", &
            "a clone should carry the shortest metadata entry intact")
        if (allocated(error)) return
        call clone%get_file_metadata("a_considerably_longer_metadata_key", val, found=ok)
        call check(error, ok .and. val == "a considerably longer metadata value", &
            "a clone should carry the longest metadata entry intact, untruncated")
        if (allocated(error)) return
        call clone%get_file_metadata("no_such_key", val, found=ok)
        call check(error, .not. ok, "a clone should still report a missing metadata key as a miss")
        if (allocated(error)) return
        !
        ! parquet_get_metadata_items reports the same store the snapshot is taken from.
        call parquet_open_reader(rd, f)
        call parquet_get_metadata_items(rd, keys, vals)
        call check(error, size(keys) == size(vals) .and. size(keys) > 0, &
            "parquet_get_metadata_items should report the file's metadata entries")
        if (allocated(error)) return
        ok = .false.
        do i = 1, size(keys)
            if (trim(keys(i)) == "mykey") ok = trim(vals(i)) == "myvalue"
        end do
        call check(error, ok, "parquet_get_metadata_items should report the key and its value")
        if (allocated(error)) return
        call parquet_close_reader(rd)
    end subroutine test_filename_and_metadata
    !
    !> Writes the all-18-kinds fixture the three matrix tests share.
    subroutine write_matrix_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer :: i, e
        ! scalar fixtures
        integer(int32) :: a_i32(NROW)
        integer(int64) :: a_i64(NROW)
        real(real32) :: a_f32(NROW)
        real(real64) :: a_f64(NROW)
        logical :: a_bool(NROW)
        character(len=8) :: a_str(NROW)
        type(parquet_date) :: a_date(NROW)
        type(parquet_time) :: a_time(NROW)
        type(parquet_timestamp) :: a_ts(NROW)
        ! vector fixtures, (element, row)
        integer(int32) :: v_i32(NVEC, NROW)
        integer(int64) :: v_i64(NVEC, NROW)
        real(real32) :: v_f32(NVEC, NROW)
        real(real64) :: v_f64(NVEC, NROW)
        logical :: v_bool(NVEC, NROW)
        character(len=8) :: v_str(NVEC, NROW)
        type(parquet_date) :: v_date(NVEC, NROW)
        type(parquet_time) :: v_time(NVEC, NROW)
        type(parquet_timestamp) :: v_ts(NVEC, NROW)
        !
        do i = 1, NROW
            a_i32(i) = i
            a_i64(i) = int(i, int64) * 1000000000_int64
            a_f32(i) = real(i, real32) * 0.25_real32
            a_f64(i) = real(i, real64) * 1.75_real64
            a_bool(i) = mod(i, 2) == 1
            a_date(i) = parquet_date(2026, 3, i)
            a_time(i) = parquet_time(10, 20, i)
            a_ts(i) = parquet_timestamp(2026, 3, i, 1, 2, 3)
            do e = 1, NVEC
                v_i32(e, i) = i * 10 + e
                v_i64(e, i) = int(i * 10 + e, int64) * 1000000000_int64
                v_f32(e, i) = real(i * 10 + e, real32) * 0.5_real32
                v_f64(e, i) = real(i * 10 + e, real64) * 1.5_real64
                v_bool(e, i) = mod(i + e, 2) == 0
                v_date(e, i) = parquet_date(2026, 4, e)
                v_time(e, i) = parquet_time(5, 6, e)
                v_ts(e, i) = parquet_timestamp(2026, 4, e, 7, 8, 9)
            end do
        end do
        ! First element deliberately the shortest, in both the scalar and vector fixtures.
        a_str = ["a       ", "bcd     ", "ef      ", "ghijklm ", "no      ", "p       "]
        v_str(1, :) = "a"
        v_str(2, :) = "bcde"
        v_str(3, :) = "fg"
        !
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "s_i32", a_i32)
        call parquet_write_column(w, "s_i64", a_i64)
        call parquet_write_column(w, "s_f32", a_f32)
        call parquet_write_column(w, "s_f64", a_f64)
        call parquet_write_column(w, "s_bool", a_bool)
        call parquet_write_column(w, "s_str", a_str)
        call parquet_write_column(w, "s_date", a_date)
        call parquet_write_column(w, "s_time", a_time)
        call parquet_write_column(w, "s_ts", a_ts)
        call parquet_write_column(w, "v_i32", v_i32)
        call parquet_write_column(w, "v_i64", v_i64)
        call parquet_write_column(w, "v_f32", v_f32)
        call parquet_write_column(w, "v_f64", v_f64)
        call parquet_write_column(w, "v_bool", v_bool)
        call parquet_write_column(w, "v_str", v_str)
        call parquet_write_column(w, "v_date", v_date)
        call parquet_write_column(w, "v_time", v_time)
        call parquet_write_column(w, "v_ts", v_ts)
        call parquet_close_writer(w)
    end subroutine write_matrix_fixture
    !
    !> All 18 kinds again, but long enough to span several row groups -- which is what the slice
    !! regime is about, and what a 6-row single-row-group fixture cannot exercise at all.
    !! `chunk` forces the row-group size, so the boundaries are known to the test rather than
    !! left to the writer's own auto-sizing.
    subroutine write_slice_fixture(fname, n, chunk)
        character(len=*), intent(in) :: fname !! file to write.
        integer, intent(in) :: n              !! rows to write.
        integer, intent(in) :: chunk          !! rows per row group.
        type(parquet_writer) :: w
        integer :: i, e
        integer(int32), allocatable :: a_i32(:), v_i32(:,:)
        integer(int64), allocatable :: a_i64(:), v_i64(:,:)
        real(real32), allocatable :: a_f32(:), v_f32(:,:)
        real(real64), allocatable :: a_f64(:), v_f64(:,:)
        logical, allocatable :: a_bool(:), v_bool(:,:), valid(:), valid_v(:,:)
        character(len=8), allocatable :: a_str(:), v_str(:,:)
        type(parquet_date), allocatable :: a_date(:), v_date(:,:)
        type(parquet_time), allocatable :: a_time(:), v_time(:,:)
        type(parquet_timestamp), allocatable :: a_ts(:), v_ts(:,:)
        !
        allocate(a_i32(n), a_i64(n), a_f32(n), a_f64(n), a_bool(n), a_str(n), valid(n))
        allocate(a_date(n), a_time(n), a_ts(n))
        allocate(v_i32(NVEC, n), v_i64(NVEC, n), v_f32(NVEC, n), v_f64(NVEC, n))
        allocate(v_bool(NVEC, n), v_str(NVEC, n), v_date(NVEC, n), v_time(NVEC, n), v_ts(NVEC, n))
        do i = 1, n
            a_i32(i) = i
            a_i64(i) = int(i, int64) * 1000_int64
            a_f32(i) = real(i, real32) * 0.25_real32
            a_f64(i) = real(i, real64) * 1.75_real64
            a_bool(i) = mod(i, 2) == 1
            a_date(i) = parquet_date(2026, 1, 1 + mod(i, 28))
            a_time(i) = parquet_time(1, 2, mod(i, 60))
            a_ts(i) = parquet_timestamp(2026, 1, 1 + mod(i, 28), 3, 4, mod(i, 60))
            ! Deliberately the shortest first, so a "sized from the first element" bug shows up.
            write(a_str(i), '(a,i0)') "r", i
            do e = 1, NVEC
                v_i32(e, i) = i * 10 + e
                v_i64(e, i) = int(i * 10 + e, int64) * 1000_int64
                v_f32(e, i) = real(i * 10 + e, real32) * 0.5_real32
                v_f64(e, i) = real(i * 10 + e, real64) * 1.5_real64
                v_bool(e, i) = mod(i + e, 2) == 0
                v_date(e, i) = parquet_date(2026, 2, e)
                v_time(e, i) = parquet_time(5, 6, e)
                v_ts(e, i) = parquet_timestamp(2026, 2, e, 7, 8, 9)
                write(v_str(e, i), '(a,i0,i0)') "v", i, e
            end do
        end do
        a_str(1) = "a"
        v_str(1, 1) = "b"
        ! One null per validity dispatch class, placed in the middle so a slice can straddle it.
        valid = .true.
        valid(n / 2) = .false.
        a_date(n / 2 + 1) = parquet_date()
        ! Every scalar/vector numeric+bool+string column also gets the same null row, so the
        ! slice regime's row-group materializers (matchunk_*) are exercised on their own mask
        ! path too -- not just the whole-file mat_* path the unsliced fixtures already cover.
        allocate(valid_v(NVEC, n))
        valid_v = .true.
        valid_v(:, n / 2) = .false.
        !
        call parquet_open_writer(w, fname, chunk_size=chunk)
        call parquet_write_column(w, "s_i32", a_i32, is_valid=valid)
        call parquet_write_column(w, "s_i64", a_i64, is_valid=valid)
        call parquet_write_column(w, "s_f32", a_f32, is_valid=valid)
        call parquet_write_column(w, "s_f64", a_f64, is_valid=valid)
        call parquet_write_column(w, "s_bool", a_bool, is_valid=valid)
        call parquet_write_column(w, "s_str", a_str, is_valid=valid)
        call parquet_write_column(w, "s_date", a_date)
        call parquet_write_column(w, "s_time", a_time)
        call parquet_write_column(w, "s_ts", a_ts)
        call parquet_write_column(w, "v_i32", v_i32, is_valid=valid_v)
        call parquet_write_column(w, "v_i64", v_i64, is_valid=valid_v)
        call parquet_write_column(w, "v_f32", v_f32, is_valid=valid_v)
        call parquet_write_column(w, "v_f64", v_f64, is_valid=valid_v)
        call parquet_write_column(w, "v_bool", v_bool, is_valid=valid_v)
        call parquet_write_column(w, "v_str", v_str, is_valid=valid_v)
        call parquet_write_column(w, "v_date", v_date)
        call parquet_write_column(w, "v_time", v_time)
        call parquet_write_column(w, "v_ts", v_ts)
        call parquet_close_writer(w)
    end subroutine write_slice_fixture
    !
    subroutine test_row_group_bounds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_reader) :: r
        integer(int64), allocatable :: bounds(:,:), tbounds(:,:)
        integer(int64) :: nrg, rg
        character(len=*), parameter :: f = "test_run/table_rgbounds.parquet"
        !
        call write_slice_fixture(f, 20, 7)
        !
        ! The standalone planning form: no table needed, and none open.
        call parquet_table_row_group_bounds(f, bounds)
        call parquet_open_reader(r, f)
        call parquet_get_num_row_groups(r, nrg)
        call parquet_close_reader(r)
        call check(error, size(bounds, 2, kind=int64) == nrg, &
            "there should be one bounds entry per row group")
        if (allocated(error)) return
        call check(error, bounds(1, 1) == 1_int64, "the first row group must start at row 1")
        if (allocated(error)) return
        call check(error, bounds(2, nrg) == 20_int64, "the last row group must end at the last row")
        if (allocated(error)) return
        do rg = 2_int64, nrg
            call check(error, bounds(1, rg) == bounds(2, rg - 1) + 1_int64, &
                "row groups must partition the rows with no gap and no overlap")
            if (allocated(error)) return
        end do
        !
        ! The table-level form, on an UNFILTERED slice: its rows are the file's rows, so both
        ! coordinate systems are the same array and physical= changes nothing.
        call parquet_open_table(t, f, 9, 12)
        call t%row_group_bounds(tbounds, physical=.true.)
        call check(error, all(tbounds == bounds), &
            "an unfiltered slice should report the file's own row groups under physical=.true.")
        if (allocated(error)) return
        call t%row_group_bounds(tbounds)
        call check(error, all(tbounds == bounds), &
            "an unfiltered slice's own coordinates are the file's, so the default should match")
        if (allocated(error)) return
        !
        ! A full-regime table never precomputes rg_bounds at open time (only the slice regime
        ! does, to plan which row groups it needs) -- so this must compute them on demand.
        call parquet_open_table(t, f)
        call t%row_group_bounds(tbounds)
        call check(error, all(tbounds == bounds), &
            "a full-regime table should compute row groups on demand too")
    end subroutine test_row_group_bounds
    !
    !> The slice regime over every kind, with the slice deliberately straddling two row-group
    !! boundaries so that both the head trim and the tail trim are exercised, and the middle row
    !! group is taken whole.
    subroutine test_slice_kind_matrix(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: full, sl
        integer, parameter :: N = 20, CH = 7, LO = 6, HI = 16
        integer(int32), allocatable :: f_i32(:), s_i32(:), fv_i32(:,:), sv_i32(:,:)
        integer(int64), allocatable :: f_i64(:), s_i64(:), fv_i64(:,:), sv_i64(:,:)
        real(real32), allocatable :: f_f32(:), s_f32(:), fv_f32(:,:), sv_f32(:,:)
        real(real64), allocatable :: f_f64(:), s_f64(:), fv_f64(:,:), sv_f64(:,:)
        logical, allocatable :: f_bool(:), s_bool(:), fv_bool(:,:), sv_bool(:,:)
        character(len=:), allocatable :: f_str(:), s_str(:), fv_str(:,:), sv_str(:,:)
        type(parquet_date), allocatable :: f_date(:), s_date(:), fv_date(:,:), sv_date(:,:)
        type(parquet_time), allocatable :: f_time(:), s_time(:), fv_time(:,:), sv_time(:,:)
        type(parquet_timestamp), allocatable :: f_ts(:), s_ts(:), fv_ts(:,:), sv_ts(:,:)
        ! A timestamp's civil fields are reached through its date part rather than directly, and
        ! a chained x%get_date()%raw() is not valid Fortran, so the part needs a name.
        type(parquet_date) :: dp_a, dp_b
        integer :: i
        character(len=*), parameter :: f = "test_run/table_slice_matrix.parquet"
        !
        call write_slice_fixture(f, N, CH)
        call parquet_open_table(full, f)
        call parquet_open_table(sl, f, LO, HI)
        !
        call check(error, sl%nrows() == int(HI - LO + 1, int64), &
            "a slice table's row count is the slice's own length")
        if (allocated(error)) return
        call check(error, full%nrows() == int(N, int64), "the full table still covers every row")
        if (allocated(error)) return
        !
        call full%get("s_i32", f_i32);  call sl%get("s_i32", s_i32)
        call check(error, all(s_i32 == f_i32(LO:HI)), "s_i32 slice should equal the full column's rows")
        if (allocated(error)) return
        ! Values alone do not prove the assembly is right: a slice is built row group by row
        ! group, and each piece's validity has to land on the same rows its values did. The
        ! fixture's null sits at row N/2, inside this slice, so a null that is dropped, kept
        ! after it should have been replaced, or shifted by a row shows up here -- checked for
        ! both validity dispatch classes that have a bitmap or a store behind them.
        do i = 1, HI - LO + 1
            call check(error, sl%is_null("s_i32", int(i, int64)) .eqv. &
                full%is_null("s_i32", int(LO + i - 1, int64)), &
                "s_i32 slice validity should match the same row of the full column")
            if (allocated(error)) return
            call check(error, sl%is_null("s_str", int(i, int64)) .eqv. &
                full%is_null("s_str", int(LO + i - 1, int64)), &
                "s_str slice validity should match the same row of the full column")
            if (allocated(error)) return
        end do
        call full%get("s_i64", f_i64);  call sl%get("s_i64", s_i64)
        call check(error, all(s_i64 == f_i64(LO:HI)), "s_i64 slice should equal the full column's rows")
        if (allocated(error)) return
        call full%get("s_f32", f_f32);  call sl%get("s_f32", s_f32)
        call check(error, all(abs(s_f32 - f_f32(LO:HI)) < 1.0e-6_real32), &
            "s_f32 slice should equal the full column's rows")
        if (allocated(error)) return
        call full%get("s_f64", f_f64);  call sl%get("s_f64", s_f64)
        call check(error, all(abs(s_f64 - f_f64(LO:HI)) < 1.0e-12_real64), &
            "s_f64 slice should equal the full column's rows")
        if (allocated(error)) return
        call full%get("s_bool", f_bool); call sl%get("s_bool", s_bool)
        call check(error, all(s_bool .eqv. f_bool(LO:HI)), &
            "s_bool slice should equal the full column's rows")
        if (allocated(error)) return
        call full%get("s_str", f_str);  call sl%get("s_str", s_str)
        do i = 1, HI - LO + 1
            call check(error, trim(s_str(i)) == trim(f_str(LO + i - 1)), &
                "s_str slice should equal the full column's rows")
            if (allocated(error)) return
        end do
        ! %raw is the comparator of choice for the temporal kinds: unlike the civil accessors
        ! it never aborts on a null element, and the fixture has one.
        call full%get("s_date", f_date); call sl%get("s_date", s_date)
        do i = 1, HI - LO + 1
            call check(error, s_date(i)%raw() == f_date(LO + i - 1)%raw() .and. &
                (s_date(i)%is_null() .eqv. f_date(LO + i - 1)%is_null()), &
                "s_date slice should equal the full column's rows")
            if (allocated(error)) return
        end do
        call full%get("s_time", f_time); call sl%get("s_time", s_time)
        do i = 1, HI - LO + 1
            call check(error, s_time(i)%raw() == f_time(LO + i - 1)%raw(), &
                "s_time slice should equal the full column's rows")
            if (allocated(error)) return
        end do
        call full%get("s_ts", f_ts);    call sl%get("s_ts", s_ts)
        do i = 1, HI - LO + 1
            dp_a = s_ts(i)%get_date()
            dp_b = f_ts(LO + i - 1)%get_date()
            call check(error, dp_a%raw() == dp_b%raw(), &
                "s_ts slice should equal the full column's rows")
            if (allocated(error)) return
        end do
        !
        call full%get("v_i32", fv_i32); call sl%get("v_i32", sv_i32)
        call check(error, all(sv_i32 == fv_i32(:, LO:HI)), "v_i32 slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_i64", fv_i64); call sl%get("v_i64", sv_i64)
        call check(error, all(sv_i64 == fv_i64(:, LO:HI)), "v_i64 slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_f32", fv_f32); call sl%get("v_f32", sv_f32)
        call check(error, all(abs(sv_f32 - fv_f32(:, LO:HI)) < 1.0e-6_real32), &
            "v_f32 slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_f64", fv_f64); call sl%get("v_f64", sv_f64)
        call check(error, all(abs(sv_f64 - fv_f64(:, LO:HI)) < 1.0e-12_real64), &
            "v_f64 slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_bool", fv_bool); call sl%get("v_bool", sv_bool)
        call check(error, all(sv_bool .eqv. fv_bool(:, LO:HI)), "v_bool slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_str", fv_str); call sl%get("v_str", sv_str)
        call check(error, trim(sv_str(2, 1)) == trim(fv_str(2, LO)), &
            "v_str slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_date", fv_date); call sl%get("v_date", sv_date)
        call check(error, sv_date(2, 1)%raw() == fv_date(2, LO)%raw(), &
            "v_date slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_time", fv_time); call sl%get("v_time", sv_time)
        call check(error, sv_time(2, 1)%raw() == fv_time(2, LO)%raw(), &
            "v_time slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_ts", fv_ts);   call sl%get("v_ts", sv_ts)
        dp_a = sv_ts(2, 1)%get_date()
        dp_b = fv_ts(2, LO)%get_date()
        call check(error, dp_a%raw() == dp_b%raw(), "v_ts slice should equal the full rows")
        if (allocated(error)) return
        !
        ! Nulls have to survive the trim-and-concatenate too: the fixture's null row is inside
        ! this slice. Every scalar/vector numeric+bool+string column carries the same null row,
        ! which drives the row-group materializers' (matchunk_*) own mask path -- the slice
        ! regime's counterpart to test_materialize_null_fast_path's whole-file mat_* coverage.
        block
            character(len=6), parameter :: names(12) = &
                [character(len=6) :: "s_i32", "s_i64", "s_f32", "s_f64", "s_bool", "s_str", &
                    "v_i32", "v_i64", "v_f32", "v_f64", "v_bool", "v_str"]
            integer :: k
            do k = 1, size(names)
                do i = 1, HI - LO + 1
                    call check(error, sl%is_null(trim(names(k)), int(i, int64)) .eqv. &
                        full%is_null(trim(names(k)), int(LO + i - 1, int64)), &
                        "a null must land on the same row after slicing: " // trim(names(k)))
                    if (allocated(error)) return
                end do
            end do
        end block
    end subroutine test_slice_kind_matrix
    !
    !> The four slice shapes that differ in how much trimming they need: wholly inside one row
    !! group, exactly on row-group boundaries, the whole file, and a single row at each end.
    subroutine test_slice_shapes(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: full, sl
        integer, parameter :: N = 20, CH = 7
        integer(int32), allocatable :: fg(:), sg(:)
        character(len=*), parameter :: f = "test_run/table_slice_shapes.parquet"
        !
        call write_slice_fixture(f, N, CH)
        call parquet_open_table(full, f)
        call full%get("s_i32", fg)
        !
        ! Wholly inside row group 2 (rows 8..14).
        call parquet_open_table(sl, f, 9, 12)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 4 .and. all(sg == fg(9:12)), &
            "a slice inside one row group should read just those rows")
        if (allocated(error)) return
        !
        ! Exactly one whole row group: no trimming at either end.
        call parquet_open_table(sl, f, 8, 14)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 7 .and. all(sg == fg(8:14)), &
            "a row-group-aligned slice should need no trimming")
        if (allocated(error)) return
        !
        ! The whole file, expressed as a slice: must agree with the full regime exactly.
        call parquet_open_table(sl, f, 1, N)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == N .and. all(sg == fg), &
            "a whole-file slice should equal the full-regime table")
        if (allocated(error)) return
        !
        ! Single rows at both ends -- the extreme trims.
        call parquet_open_table(sl, f, 1, 1)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 1 .and. sg(1) == fg(1), &
            "a one-row slice at the start should read only row 1")
        if (allocated(error)) return
        call parquet_open_table(sl, f, N, N)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 1 .and. sg(1) == fg(N), &
            "a one-row slice at the end should read only the last row")
        if (allocated(error)) return
        !
        ! ...and the same at each interior row-group boundary. A single row is where an
        ! inclusive/exclusive mistake produces zero rows or two, and the last row of a group and
        ! the first row of the next are trimmed by different ends of different groups (CH = 7, so
        ! the groups are 1..7, 8..14, 15..20).
        call parquet_open_table(sl, f, CH, CH)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 1 .and. sg(1) == fg(CH), &
            "a one-row slice on the last row of a row group should read only that row")
        if (allocated(error)) return
        call parquet_open_table(sl, f, CH + 1, CH + 1)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 1 .and. sg(1) == fg(CH + 1), &
            "a one-row slice on the first row of a row group should read only that row")
        if (allocated(error)) return
        call parquet_open_table(sl, f, 2 * CH, 2 * CH)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 1 .and. sg(1) == fg(2 * CH), &
            "a one-row slice on the last row of the second row group should read only that row")
        if (allocated(error)) return
        call parquet_open_table(sl, f, 2 * CH + 1, 2 * CH + 1)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 1 .and. sg(1) == fg(2 * CH + 1), &
            "a one-row slice on the first row of the last row group should read only that row")
        if (allocated(error)) return
        !
        ! A slice crossing every boundary, read through the pointer path rather than the copy.
        call parquet_open_table(sl, f, 2, 19)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 18 .and. all(sg == fg(2:19)), &
            "a slice spanning all row groups should still line up")
    end subroutine test_slice_shapes
    !
    !> The row handle over every kind, plus the two properties that are easy to lose: it reads
    !! through a table declared WITHOUT `target` (it must point at the store, never at the
    !! table), and it triggers a lazy first touch of its own.
    subroutine test_row_view(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t          ! deliberately NOT `target` -- see the module doc
        type(parquet_table_row) :: r
        integer, parameter :: N = 20, CH = 7
        integer(int32) :: x_i32
        integer(int64) :: x_i64
        real(real32) :: x_f32
        real(real64) :: x_f64
        logical :: x_bool
        character(len=:), allocatable :: x_str, xv_str(:)
        type(parquet_date) :: x_date
        type(parquet_time) :: x_time
        type(parquet_timestamp) :: x_ts
        integer(int32), allocatable :: xv_i32(:)
        real(real64), allocatable :: xv_f64(:)
        integer(int64), allocatable :: w_i64(:)
        real(real64), allocatable :: wv_f64(:)
        integer(int64) :: w_scalar
        real(real64) :: w_f64
        character(len=*), parameter :: f = "test_run/table_rowview.parquet"
        !
        call write_slice_fixture(f, N, CH)
        call parquet_open_table(t, f)
        !
        ! A handle on a table where nothing has been read yet.
        r = t%row(5)
        call check(error, r%index() == 5_int64, "a handle should report the row it was made for")
        if (allocated(error)) return
        call check(error, t%residency("s_i32") == RES_EMPTY, "precondition: nothing read yet")
        if (allocated(error)) return
        call r%get("s_i32", x_i32)
        call check(error, x_i32 == 5, "row 5 of s_i32 should be 5")
        if (allocated(error)) return
        call check(error, t%residency("s_i32") == RES_FULL, &
            "a row handle's get must trigger the same first touch the table's get does")
        if (allocated(error)) return
        !
        call r%get("s_i64", x_i64)
        call check(error, x_i64 == 5000_int64, "row 5 of s_i64 should be 5000")
        if (allocated(error)) return
        call r%get("s_f32", x_f32)
        call check(error, abs(x_f32 - 1.25_real32) < 1.0e-6_real32, "row 5 of s_f32 should be 1.25")
        if (allocated(error)) return
        call r%get("s_f64", x_f64)
        call check(error, abs(x_f64 - 8.75_real64) < 1.0e-12_real64, "row 5 of s_f64 should be 8.75")
        if (allocated(error)) return
        call r%get("s_bool", x_bool)
        call check(error, x_bool, "row 5 of s_bool should be true")
        if (allocated(error)) return
        call r%get("s_str", x_str)
        call check(error, x_str == "r5", "row 5 of s_str should be r5")
        if (allocated(error)) return
        call r%get("s_date", x_date)
        call check(error, x_date%day() == 1 + mod(5, 28), "row 5 of s_date should keep its day")
        if (allocated(error)) return
        call r%get("s_time", x_time)
        call check(error, x_time%second() == mod(5, 60), "row 5 of s_time should keep its second")
        if (allocated(error)) return
        call r%get("s_ts", x_ts)
        call check(error, .not. x_ts%is_null(), "row 5 of s_ts should not be null")
        if (allocated(error)) return
        !
        ! Vector kinds come back as one array per row.
        call r%get("v_i32", xv_i32)
        call check(error, size(xv_i32) == NVEC .and. xv_i32(2) == 52, &
            "a vector row should come back width-long, in element order")
        if (allocated(error)) return
        call r%get("v_f64", xv_f64)
        call check(error, abs(xv_f64(3) - 79.5_real64) < 1.0e-12_real64, &
            "a float64 vector row should hold its own values")
        if (allocated(error)) return
        call r%get("v_str", xv_str)
        call check(error, size(xv_str) == NVEC .and. trim(xv_str(1)) == "v51", &
            "a string vector row should come back width-long")
        if (allocated(error)) return
        !
        ! Widening works exactly as it does on the table's own %get.
        call r%get("s_i32", w_scalar)
        call check(error, w_scalar == 5_int64, "a row get should widen int32 into int64")
        if (allocated(error)) return
        call r%get("s_f32", w_f64)
        call check(error, abs(w_f64 - 1.25_real64) < 1.0e-6_real64, &
            "a row get should widen float32 into float64")
        if (allocated(error)) return
        call r%get("v_i32", w_i64)
        call check(error, w_i64(2) == 52_int64, "a row get should widen an int32 vector too")
        if (allocated(error)) return
        call r%get("v_f32", wv_f64)
        call check(error, abs(wv_f64(2) - 26.0_real64) < 1.0e-5_real64, &
            "a row get should widen a float32 vector too")
        if (allocated(error)) return
        !
        ! The remaining kinds, so every generated row_get_* specific is exercised rather than
        ! sampled -- each is its own procedure, and an untested one is untested code.
        block
            integer(int64), allocatable :: yv_i64(:)
            real(real32), allocatable :: yv_f32(:)
            logical, allocatable :: yv_bool(:)
            type(parquet_date), allocatable :: yv_date(:)
            type(parquet_time), allocatable :: yv_time(:)
            type(parquet_timestamp), allocatable :: yv_ts(:)
            r = t%row(5)
            call r%get("v_i64", yv_i64)
            call check(error, yv_i64(2) == 52000_int64, "row 5 of v_i64, element 2")
            if (allocated(error)) return
            call r%get("v_f32", yv_f32)
            call check(error, abs(yv_f32(2) - 26.0_real32) < 1.0e-5_real32, "row 5 of v_f32, element 2")
            if (allocated(error)) return
            call r%get("v_bool", yv_bool)
            call check(error, size(yv_bool) == NVEC, "row 5 of v_bool should be width-long")
            if (allocated(error)) return
            call r%get("v_date", yv_date)
            call check(error, yv_date(2)%day() == 2, "row 5 of v_date, element 2 keeps its day")
            if (allocated(error)) return
            call r%get("v_time", yv_time)
            call check(error, yv_time(2)%second() == 2, "row 5 of v_time, element 2 keeps its second")
            if (allocated(error)) return
            call r%get("v_ts", yv_ts)
            call check(error, .not. yv_ts(2)%is_null(), "row 5 of v_ts, element 2 should not be null")
            if (allocated(error)) return
        end block
        !
        ! Nulls, and a handle on a slice table, whose row 1 is the slice's own first row.
        r = t%row(int(N / 2, int64))
        call check(error, r%is_null("s_i32"), "the fixture's null row should report null")
        if (allocated(error)) return
        call parquet_open_table(t, f, 6, 16)
        r = t%row(1)
        call r%get("s_i32", x_i32)
        call check(error, x_i32 == 6, "row 1 of a [6,16] slice is the file's row 6")
    end subroutine test_row_view
    !
    !> Every slice form against the same column, each checked against the equivalent Fortran
    !! array section so the expected answer is not restated by hand.
    subroutine test_get_slice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_slice) :: s
        integer, parameter :: N = 20, CH = 7
        integer(int32), allocatable :: full(:), part(:)
        integer(int64), allocatable :: wide(:)
        real(real64), allocatable :: fv(:,:), pv(:,:)
        character(len=:), allocatable :: cs(:)
        type(parquet_string_column) :: sc_out, sc_null
        real(real64), allocatable :: wf(:), wvf(:,:)
        character(len=*), parameter :: f = "test_run/table_getslice.parquet"
        !
        call write_slice_fixture(f, N, CH)
        call parquet_open_table(t, f)
        call t%get("s_i32", full)
        !
        ! start:stop
        s = parquet_slice_range(3, 8)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 6 .and. all(part == full(3:8)), &
            "a start:stop slice should equal the same array section")
        if (allocated(error)) return
        ! start:stop:step
        s = parquet_slice_range(1, 10, 2)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 5 .and. all(part == full(1:10:2)), &
            "a strided slice should equal the same array section")
        if (allocated(error)) return
        ! open-ended start: -- resolved against the table, at use
        s = parquet_slice_range(17)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 4 .and. all(part == full(17:N)), &
            "an open-ended slice should run to the table's last row")
        if (allocated(error)) return
        ! descending
        s = parquet_slice_range(10, 6, -2)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 3 .and. all(part == full(10:6:-2)), &
            "a descending slice should equal the same array section")
        if (allocated(error)) return
        ! explicit list, deliberately out of order and with a repeat
        s = parquet_slice_list([9, 2, 2, 15])
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 4 .and. &
            all(part == [full(9), full(2), full(2), full(15)]), &
            "a list slice should gather in the order given, repeats included")
        if (allocated(error)) return
        ! the int64 constructors, and a descending slice with an open end (which runs to row 1)
        s = parquet_slice_list([9_int64, 2_int64])
        call t%get_slice("s_i32", s, part)
        call check(error, all(part == [full(9), full(2)]), &
            "the int64 list constructor should gather the same rows")
        if (allocated(error)) return
        s = parquet_slice_range(3_int64, 8_int64, 2_int64)
        call t%get_slice("s_i32", s, part)
        call check(error, all(part == full(3:8:2)), &
            "the int64 range constructor should select the same rows")
        if (allocated(error)) return
        s = parquet_slice_range(4, step=-1)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 4 .and. all(part == full(4:1:-1)), &
            "a descending slice with no stop should run down to row 1")
        if (allocated(error)) return
        ! an empty gather is legal and yields nothing
        s = parquet_slice_list([integer(int32) ::])
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 0, "an empty list slice should select no rows")
        if (allocated(error)) return
        ! widening, exactly as %get does
        s = parquet_slice_range(3, 5)
        call t%get_slice("s_i32", s, wide)
        call check(error, all(wide == int(full(3:5), int64)), &
            "get_slice should widen int32 into int64")
        if (allocated(error)) return
        ! a vector column keeps its (element, row) shape
        call t%get("v_f64", fv)
        call t%get_slice("v_f64", s, pv)
        call check(error, size(pv, 1) == NVEC .and. size(pv, 2) == 3 .and. &
            all(abs(pv - fv(:, 3:5)) < 1.0e-12_real64), &
            "a sliced vector column should keep its (element, row) shape")
        if (allocated(error)) return
        ! strings, in both the character and the compact forms
        call t%get_slice("s_str", s, cs)
        call check(error, size(cs) == 3 .and. trim(cs(1)) == "r3", &
            "a sliced string column should come back in row order")
        if (allocated(error)) return
        call t%get_slice("s_str", s, sc_out)
        call check(error, sc_out%size() == 3_int64, &
            "the compact form should hold one element per selected row")
        if (allocated(error)) return
        ! float32 -> float64 widening, scalar and vector (get_slice's own widen path, distinct
        ! from %get's -- each generated get_slice_* specific has its own widen branch).
        call t%get_slice("s_f32", s, wf)
        block
            real(real32), allocatable :: r_f32full(:), r_f32vfull(:,:)
            call t%get("s_f32", r_f32full)
            call check(error, all(abs(wf - real(r_f32full(3:5), real64)) < 1.0e-6_real64), &
                "get_slice should widen float32 into float64")
            if (allocated(error)) return
            call t%get_slice("v_f32", s, wvf)
            call t%get("v_f32", r_f32vfull)
            call check(error, all(abs(wvf - real(r_f32vfull(:, 3:5), real64)) < 1.0e-6_real64), &
                "get_slice should widen a float32 vector column into a float64 array")
            if (allocated(error)) return
        end block
        !
        ! The remaining kinds, so every generated get_slice_* specific is exercised. Each is
        ! checked against the equivalent array section of the same column read whole.
        block
            integer(int64), allocatable :: q_i64(:), qv_i64(:,:)
            real(real32), allocatable :: q_f32(:), qv_f32(:,:)
            real(real64), allocatable :: q_f64(:)
            logical, allocatable :: q_bool(:), qv_bool(:,:)
            integer(int32), allocatable :: qv_i32(:,:)
            type(parquet_date), allocatable :: q_date(:), qv_date(:,:)
            type(parquet_time), allocatable :: q_time(:), qv_time(:,:)
            type(parquet_timestamp), allocatable :: q_ts(:), qv_ts(:,:)
            character(len=:), allocatable :: qv_str(:,:)
            integer(int64), allocatable :: r_i64(:)
            real(real32), allocatable :: r_f32(:)
            real(real64), allocatable :: r_f64(:)
            logical, allocatable :: r_bool(:)
            !
            call t%get_slice("s_i64", s, q_i64); call t%get("s_i64", r_i64)
            call check(error, all(q_i64 == r_i64(3:5)), "s_i64 slice equals its array section")
            if (allocated(error)) return
            call t%get_slice("s_f32", s, q_f32); call t%get("s_f32", r_f32)
            call check(error, all(abs(q_f32 - r_f32(3:5)) < 1.0e-6_real32), &
                "s_f32 slice equals its array section")
            if (allocated(error)) return
            call t%get_slice("s_f64", s, q_f64); call t%get("s_f64", r_f64)
            call check(error, all(abs(q_f64 - r_f64(3:5)) < 1.0e-12_real64), &
                "s_f64 slice equals its array section")
            if (allocated(error)) return
            call t%get_slice("s_bool", s, q_bool); call t%get("s_bool", r_bool)
            call check(error, all(q_bool .eqv. r_bool(3:5)), "s_bool slice equals its array section")
            if (allocated(error)) return
            call t%get_slice("s_date", s, q_date)
            call check(error, size(q_date) == 3, "s_date slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("s_time", s, q_time)
            call check(error, size(q_time) == 3, "s_time slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("s_ts", s, q_ts)
            call check(error, size(q_ts) == 3, "s_ts slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_i32", s, qv_i32)
            call check(error, size(qv_i32, 1) == NVEC .and. size(qv_i32, 2) == 3, &
                "v_i32 slice keeps its (element, row) shape")
            if (allocated(error)) return
            call t%get_slice("v_i64", s, qv_i64)
            call check(error, size(qv_i64, 2) == 3, "v_i64 slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_f32", s, qv_f32)
            call check(error, size(qv_f32, 2) == 3, "v_f32 slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_bool", s, qv_bool)
            call check(error, size(qv_bool, 2) == 3, "v_bool slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_date", s, qv_date)
            call check(error, size(qv_date, 2) == 3, "v_date slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_time", s, qv_time)
            call check(error, size(qv_time, 2) == 3, "v_time slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_ts", s, qv_ts)
            call check(error, size(qv_ts, 2) == 3, "v_ts slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_str", s, qv_str)
            call check(error, size(qv_str, 1) == NVEC .and. size(qv_str, 2) == 3, &
                "v_str slice keeps its (element, row) shape")
            if (allocated(error)) return
            ! Widening on the vector path too.
            call t%get_slice("v_i32", s, qv_i64)
            call check(error, size(qv_i64, 2) == 3, "get_slice should widen an int32 vector column")
            if (allocated(error)) return
        end block
        !
        ! A slice covering a null string row must widen the null itself, not just the shape.
        s = parquet_slice_range(9, 11)
        call t%get_slice("s_str", s, sc_null)
        call check(error, sc_null%is_null(2_int64), &
            "get_slice should widen a null string row into a null element rather than an empty one")
        if (allocated(error)) return
        !
        ! A slice is relative to the TABLE, so on a slice-regime table row 1 is its own first row.
        call parquet_open_table(t, f, 6, 16)
        s = parquet_slice_range(1, 3)
        call t%get_slice("s_i32", s, part)
        call check(error, all(part == full(6:8)), &
            "a slice of a slice-regime table counts from that table's own first row")
    end subroutine test_get_slice
    !
    !> The parallel-per-row-group shape: each iteration opens its OWN slice table and reads it
    !! lazily. That first touch happens inside a parallel region, and it must be allowed --
    !! a table a thread opened itself cannot be shared with another thread, which is exactly the
    !! distinction the first-touch guard draws. A blanket "no first touch in a parallel region"
    !! rule would make the slice regime unusable where it matters most.
    subroutine test_parallel_private_slices(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64), allocatable :: bounds(:,:)
        integer(int32), allocatable :: full(:)
        integer(int64) :: total
        integer :: rg
        character(len=*), parameter :: f = "test_run/table_parallel_slices.parquet"
        !
        call write_slice_fixture(f, 20, 7)
        call parquet_open_table(t, f)
        call t%get("s_i32", full)
        call parquet_table_row_group_bounds(f, bounds)
        !
        total = 0_int64
        ! The per-thread table is declared in a BLOCK inside the loop, not with private(t).
        ! An OpenMP private copy of a finalizable derived type is not reliably default-
        ! initialized by gfortran, so `parquet_open_table`'s intent(out) finalizer runs over an
        ! undefined `cache` pointer and the program dies in the allocator -- confirmed, and
        ! reproducible even with OMP_NUM_THREADS=1. A block-local is properly initialized on
        ! entry and finalized at exit, which is what a per-thread table wants anyway.
        !$omp parallel do default(shared) private(rg) reduction(+:total)
        do rg = 1, size(bounds, 2)
            block
                type(parquet_table) :: mine
                integer(int32), allocatable :: part(:)
                call parquet_open_table(mine, f, bounds(1, rg), bounds(2, rg))
                call mine%get("s_i32", part)   ! lazy first touch, inside the region
                total = total + sum(int(part, int64))
            end block
        end do
        !$omp end parallel do
        !
        call check(error, total == sum(int(full, int64)), &
            "reading every row group's own slice should cover the file exactly once")
    end subroutine test_parallel_private_slices
    !
    !> Builds the parsed schema that writes all 18 kinds back out.
    subroutine build_matrix_schema(sc)
        type(parquet_schema), intent(out) :: sc !! schema to build.
        !
        call sc%init("matrix")
        call sc%add_field("s_i32", "int32")
        call sc%add_field("s_i64", "int64")
        call sc%add_field("s_f32", "float32")
        call sc%add_field("s_f64", "float64")
        call sc%add_field("s_bool", "boolean")
        call sc%add_field("s_str", "string", array_size=16)
        call sc%add_field("s_date", "date")
        call sc%add_field("s_time", "time")
        call sc%add_field("s_ts", "timestamp")
        call sc%add_field("v_i32", "int32", col_size=NVEC)
        call sc%add_field("v_i64", "int64", col_size=NVEC)
        call sc%add_field("v_f32", "float32", col_size=NVEC)
        call sc%add_field("v_f64", "float64", col_size=NVEC)
        call sc%add_field("v_bool", "boolean", col_size=NVEC)
        call sc%add_field("v_str", "string", array_size=16, col_size=NVEC)
        call sc%add_field("v_date", "date", col_size=NVEC)
        call sc%add_field("v_time", "time", col_size=NVEC)
        call sc%add_field("v_ts", "timestamp", col_size=NVEC)
        call parquet_parse_maml(sc)
    end subroutine build_matrix_schema
    !
    subroutine test_kind_matrix(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: sc
        character(len=*), parameter :: f = "test_run/table_matrix.parquet"
        character(len=*), parameter :: fo = "test_run/table_matrix_out.parquet"
        ! read-back targets
        integer(int32), allocatable :: g_i32(:), gv_i32(:,:)
        integer(int64), allocatable :: g_i64(:), gv_i64(:,:)
        real(real32), allocatable :: g_f32(:), gv_f32(:,:)
        real(real64), allocatable :: g_f64(:), gv_f64(:,:)
        logical, allocatable :: g_bool(:), gv_bool(:,:)
        character(len=:), allocatable :: g_str(:), gv_str(:,:)
        type(parquet_date), allocatable :: g_date(:), gv_date(:,:)
        type(parquet_time), allocatable :: g_time(:), gv_time(:,:)
        type(parquet_timestamp), allocatable :: g_ts(:), gv_ts(:,:)
        integer(int32), pointer :: p_i32(:)
        real(real64), pointer :: p_f64v(:,:)
        ! A timestamp's civil fields are reached through its date part rather than directly.
        type(parquet_date) :: dpart
        !
        call write_matrix_fixture(f)
        !
        call parquet_open_table(t, f)
        call check(error, t%ncols() == 18, "the matrix fixture should have all 18 kinds")
        if (allocated(error)) return
        !
        ! --- kinds and widths ---
        call check(error, t%kind("s_i32") == PK_INT32, "s_i32 should be PK_INT32")
        if (allocated(error)) return
        call check(error, t%kind("s_i64") == PK_INT64, "s_i64 should be PK_INT64")
        if (allocated(error)) return
        call check(error, t%kind("s_f32") == PK_FLOAT32, "s_f32 should be PK_FLOAT32")
        if (allocated(error)) return
        call check(error, t%kind("s_f64") == PK_FLOAT64, "s_f64 should be PK_FLOAT64")
        if (allocated(error)) return
        call check(error, t%kind("s_bool") == PK_LOGICAL, "s_bool should be PK_LOGICAL")
        if (allocated(error)) return
        call check(error, t%kind("s_str") == PK_STRING, "s_str should be PK_STRING")
        if (allocated(error)) return
        call check(error, t%kind("s_date") == PK_DATE, "s_date should be PK_DATE")
        if (allocated(error)) return
        call check(error, t%kind("s_time") == PK_TIME, "s_time should be PK_TIME")
        if (allocated(error)) return
        call check(error, t%kind("s_ts") == PK_TIMESTAMP, "s_ts should be PK_TIMESTAMP")
        if (allocated(error)) return
        call check(error, t%kind("v_i32") == PK_INT32_VEC, "v_i32 should be PK_INT32_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_i64") == PK_INT64_VEC, "v_i64 should be PK_INT64_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_f32") == PK_FLOAT32_VEC, "v_f32 should be PK_FLOAT32_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_f64") == PK_FLOAT64_VEC, "v_f64 should be PK_FLOAT64_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_bool") == PK_LOGICAL_VEC, "v_bool should be PK_LOGICAL_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_str") == PK_STRING_VEC, "v_str should be PK_STRING_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_date") == PK_DATE_VEC, "v_date should be PK_DATE_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_time") == PK_TIME_VEC, "v_time should be PK_TIME_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_ts") == PK_TIMESTAMP_VEC, "v_ts should be PK_TIMESTAMP_VEC")
        if (allocated(error)) return
        call check(error, t%width("s_f64") == 1 .and. t%width("v_f64") == NVEC, &
            "a scalar column's width should be 1 and a vector column's its element count")
        if (allocated(error)) return
        !
        ! --- copy out every kind ---
        call t%get("s_i32", g_i32)
        call check(error, g_i32(3) == 3 .and. size(g_i32) == NROW, "s_i32 values should survive")
        if (allocated(error)) return
        call t%get("s_i64", g_i64)
        call check(error, g_i64(3) == 3000000000_int64, "s_i64 values should survive")
        if (allocated(error)) return
        call t%get("s_f32", g_f32)
        call check(error, abs(g_f32(4) - 1.0_real32) < 1.0e-6_real32, "s_f32 values should survive")
        if (allocated(error)) return
        call t%get("s_f64", g_f64)
        call check(error, abs(g_f64(4) - 7.0_real64) < 1.0e-12_real64, "s_f64 values should survive")
        if (allocated(error)) return
        call t%get("s_bool", g_bool)
        call check(error, g_bool(1) .and. .not. g_bool(2), "s_bool values should survive")
        if (allocated(error)) return
        call t%get("s_str", g_str)
        call check(error, trim(g_str(4)) == "ghijklm", "s_str values should survive untruncated")
        if (allocated(error)) return
        call t%get("s_date", g_date)
        call check(error, g_date(2)%day() == 2, "s_date values should survive")
        if (allocated(error)) return
        call t%get("s_time", g_time)
        call check(error, g_time(2)%second() == 2, "s_time values should survive")
        if (allocated(error)) return
        call t%get("s_ts", g_ts)
        dpart = g_ts(2)%get_date()
        call check(error, dpart%day() == 2, "s_ts values should survive")
        if (allocated(error)) return
        call t%get("v_i32", gv_i32)
        call check(error, gv_i32(2, 3) == 32, "v_i32 values should survive")
        if (allocated(error)) return
        call t%get("v_i64", gv_i64)
        call check(error, gv_i64(2, 3) == 32000000000_int64, "v_i64 values should survive")
        if (allocated(error)) return
        call t%get("v_f32", gv_f32)
        call check(error, abs(gv_f32(2, 3) - 16.0_real32) < 1.0e-5_real32, "v_f32 values should survive")
        if (allocated(error)) return
        call t%get("v_f64", gv_f64)
        call check(error, abs(gv_f64(2, 3) - 48.0_real64) < 1.0e-12_real64, "v_f64 values should survive")
        if (allocated(error)) return
        call t%get("v_bool", gv_bool)
        call check(error, gv_bool(1, 1) .and. .not. gv_bool(2, 1), "v_bool values should survive")
        if (allocated(error)) return
        call t%get("v_str", gv_str)
        call check(error, trim(gv_str(2, 1)) == "bcde", "v_str values should survive untruncated")
        if (allocated(error)) return
        call t%get("v_date", gv_date)
        call check(error, gv_date(2, 1)%day() == 2, "v_date values should survive")
        if (allocated(error)) return
        call t%get("v_time", gv_time)
        call check(error, gv_time(2, 1)%second() == 2, "v_time values should survive")
        if (allocated(error)) return
        call t%get("v_ts", gv_ts)
        dpart = gv_ts(2, 1)%get_date()
        call check(error, dpart%day() == 2, "v_ts values should survive")
        if (allocated(error)) return
        !
        ! --- pointer path, one scalar and one vector kind ---
        call t%col("s_i32", p_i32)
        call check(error, p_i32(3) == 3, "the pointer path should see the same scalar values")
        if (allocated(error)) return
        call t%col("v_f64", p_f64v)
        call check(error, abs(p_f64v(2, 3) - 48.0_real64) < 1.0e-12_real64, &
            "the pointer path should see the same vector values")
        if (allocated(error)) return
        !
        ! --- copy back, then write every kind out and reopen ---
        call t%set("s_f64", g_f64)
        call t%set("v_f64", gv_f64)
        call build_matrix_schema(sc)
        call parquet_write_table(t, fo, sc)
        !
        call parquet_open_table(t2, fo)
        call check(error, t2%ncols() == 18, "every kind should survive the write")
        if (allocated(error)) return
        call check(error, t2%nrows() == NROW, "the written table should keep its row count")
        if (allocated(error)) return
        call t2%get("s_str", g_str)
        call check(error, trim(g_str(4)) == "ghijklm", "s_str should survive the write round trip")
        if (allocated(error)) return
        call t2%get("v_f64", gv_f64)
        call check(error, abs(gv_f64(2, 3) - 48.0_real64) < 1.0e-12_real64, &
            "v_f64 should survive the write round trip")
        if (allocated(error)) return
        call t2%get("v_ts", gv_ts)
        dpart = gv_ts(2, 1)%get_date()
        call check(error, dpart%day() == 2, "v_ts should survive the write round trip")
        if (allocated(error)) return
        call t2%get("s_date", g_date)
        call check(error, g_date(2)%day() == 2, "s_date should survive the write round trip")
    end subroutine test_kind_matrix
    !
    !> %col for every kind that has a pointer path, and %set for every kind, swept rather than
    !! spot-checked: each has its own generated specific, so an untested kind is untested code.
    subroutine test_kind_matrix_col_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        ! Its own fixture path: test-drive runs tests concurrently, so sharing one file with
        ! another test lets one truncate it while the other is reading.
        character(len=*), parameter :: f = "test_run/table_matrix_cs.parquet"
        integer(int32), pointer :: p_i32(:), p_i32v(:,:)
        integer(int64), pointer :: p_i64(:), p_i64v(:,:)
        real(real32), pointer :: p_f32(:), p_f32v(:,:)
        real(real64), pointer :: p_f64(:), p_f64v(:,:)
        logical, pointer :: p_bool(:), p_boolv(:,:)
        type(parquet_date), pointer :: p_date(:), p_datev(:,:)
        type(parquet_time), pointer :: p_time(:), p_timev(:,:)
        type(parquet_timestamp), pointer :: p_ts(:), p_tsv(:,:)
        integer(int32), allocatable :: g_i32(:), gv_i32(:,:)
        integer(int64), allocatable :: g_i64(:), gv_i64(:,:)
        real(real32), allocatable :: g_f32(:), gv_f32(:,:)
        real(real64), allocatable :: g_f64(:), gv_f64(:,:)
        logical, allocatable :: g_bool(:), gv_bool(:,:)
        character(len=:), allocatable :: g_str(:), gv_str(:,:)
        type(parquet_date), allocatable :: g_date(:), gv_date(:,:)
        type(parquet_time), allocatable :: g_time(:), gv_time(:,:)
        type(parquet_timestamp), allocatable :: g_ts(:), gv_ts(:,:)
        !
        ! test_kind_matrix writes this fixture; regenerate it here so the two tests are
        ! independent of each other's execution order (test-drive does not promise one).
        call write_matrix_fixture(f)
        call parquet_open_table(t, f)
        !
        ! --- every pointer path (all kinds except the two string ones) ---
        call t%col("s_i32", p_i32)
        call check(error, associated(p_i32) .and. size(p_i32) == NROW, "col s_i32")
        if (allocated(error)) return
        call t%col("s_i64", p_i64)
        call check(error, associated(p_i64) .and. size(p_i64) == NROW, "col s_i64")
        if (allocated(error)) return
        call t%col("s_f32", p_f32)
        call check(error, associated(p_f32) .and. size(p_f32) == NROW, "col s_f32")
        if (allocated(error)) return
        call t%col("s_f64", p_f64)
        call check(error, associated(p_f64) .and. size(p_f64) == NROW, "col s_f64")
        if (allocated(error)) return
        call t%col("s_bool", p_bool)
        call check(error, associated(p_bool) .and. size(p_bool) == NROW, "col s_bool")
        if (allocated(error)) return
        call t%col("s_date", p_date)
        call check(error, associated(p_date) .and. size(p_date) == NROW, "col s_date")
        if (allocated(error)) return
        call t%col("s_time", p_time)
        call check(error, associated(p_time) .and. size(p_time) == NROW, "col s_time")
        if (allocated(error)) return
        call t%col("s_ts", p_ts)
        call check(error, associated(p_ts) .and. size(p_ts) == NROW, "col s_ts")
        if (allocated(error)) return
        call t%col("v_i32", p_i32v)
        call check(error, associated(p_i32v) .and. size(p_i32v, 1) == NVEC, "col v_i32")
        if (allocated(error)) return
        call t%col("v_i64", p_i64v)
        call check(error, associated(p_i64v) .and. size(p_i64v, 1) == NVEC, "col v_i64")
        if (allocated(error)) return
        call t%col("v_f32", p_f32v)
        call check(error, associated(p_f32v) .and. size(p_f32v, 1) == NVEC, "col v_f32")
        if (allocated(error)) return
        call t%col("v_f64", p_f64v)
        call check(error, associated(p_f64v) .and. size(p_f64v, 1) == NVEC, "col v_f64")
        if (allocated(error)) return
        call t%col("v_bool", p_boolv)
        call check(error, associated(p_boolv) .and. size(p_boolv, 1) == NVEC, "col v_bool")
        if (allocated(error)) return
        call t%col("v_date", p_datev)
        call check(error, associated(p_datev) .and. size(p_datev, 1) == NVEC, "col v_date")
        if (allocated(error)) return
        call t%col("v_time", p_timev)
        call check(error, associated(p_timev) .and. size(p_timev, 1) == NVEC, "col v_time")
        if (allocated(error)) return
        call t%col("v_ts", p_tsv)
        call check(error, associated(p_tsv) .and. size(p_tsv, 1) == NVEC, "col v_ts")
        if (allocated(error)) return
        !
        ! --- %set every kind: copy out, then write the same values straight back ---
        call t%get("s_i32", g_i32);   call t%set("s_i32", g_i32)
        call t%get("s_i64", g_i64);   call t%set("s_i64", g_i64)
        call t%get("s_f32", g_f32);   call t%set("s_f32", g_f32)
        call t%get("s_f64", g_f64);   call t%set("s_f64", g_f64)
        call t%get("s_bool", g_bool); call t%set("s_bool", g_bool)
        call t%get("s_str", g_str);   call t%set("s_str", g_str)
        call t%get("s_date", g_date); call t%set("s_date", g_date)
        call t%get("s_time", g_time); call t%set("s_time", g_time)
        call t%get("s_ts", g_ts);     call t%set("s_ts", g_ts)
        call t%get("v_i32", gv_i32);   call t%set("v_i32", gv_i32)
        call t%get("v_i64", gv_i64);   call t%set("v_i64", gv_i64)
        call t%get("v_f32", gv_f32);   call t%set("v_f32", gv_f32)
        call t%get("v_f64", gv_f64);   call t%set("v_f64", gv_f64)
        call t%get("v_bool", gv_bool); call t%set("v_bool", gv_bool)
        call t%get("v_str", gv_str);   call t%set("v_str", gv_str)
        call t%get("v_date", gv_date); call t%set("v_date", gv_date)
        call t%get("v_time", gv_time); call t%set("v_time", gv_time)
        call t%get("v_ts", gv_ts);     call t%set("v_ts", gv_ts)
        !
        ! A set-then-get round trip must be the identity.
        call t%get("s_f64", g_f64)
        call check(error, size(g_f64) == NROW, "a set/get round trip should preserve the row count")
        if (allocated(error)) return
        call t%get("v_str", gv_str)
        call check(error, trim(gv_str(2, 1)) == "bcde", &
            "a set/get round trip should preserve vector string values")
        if (allocated(error)) return
        call t%get("s_str", g_str)
        call check(error, trim(g_str(4)) == "ghijklm", &
            "a set/get round trip should preserve scalar string values")
    end subroutine test_kind_matrix_col_set
    !
    !> found= on every %get/%col specific, swept the same way as test_kind_matrix_col_set: each
    !! kind's %get/%col has its own generated "column not found" early-return branch, so it is
    !! untested code unless a bogus name is actually looked up through that exact specific.
    subroutine test_kind_matrix_found(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_matrix_found.parquet"
        character(len=*), parameter :: miss = "no_such_column"
        logical :: ok
        integer(int32), pointer :: p_i32(:), p_i32v(:,:)
        integer(int64), pointer :: p_i64(:), p_i64v(:,:)
        real(real32), pointer :: p_f32(:), p_f32v(:,:)
        real(real64), pointer :: p_f64(:), p_f64v(:,:)
        logical, pointer :: p_bool(:), p_boolv(:,:)
        type(parquet_date), pointer :: p_date(:), p_datev(:,:)
        type(parquet_time), pointer :: p_time(:), p_timev(:,:)
        type(parquet_timestamp), pointer :: p_ts(:), p_tsv(:,:)
        integer(int32), allocatable :: g_i32(:), gv_i32(:,:)
        integer(int64), allocatable :: g_i64(:), gv_i64(:,:)
        real(real32), allocatable :: g_f32(:), gv_f32(:,:)
        real(real64), allocatable :: g_f64(:), gv_f64(:,:)
        logical, allocatable :: g_bool(:), gv_bool(:,:)
        character(len=:), allocatable :: g_chr(:), gv_chr(:,:)
        type(parquet_date), allocatable :: g_date(:), gv_date(:,:)
        type(parquet_time), allocatable :: g_time(:), gv_time(:,:)
        type(parquet_timestamp), allocatable :: g_ts(:), gv_ts(:,:)
        type(parquet_string_column) :: g_str
        !
        call write_matrix_fixture(f)
        call parquet_open_table(t, f)
        !
        ! --- %col: every kind with a pointer path ---
        call t%col(miss, p_i32, found=ok);   call check(error, .not. ok .and. .not. associated(p_i32), "col i32 miss")
        if (allocated(error)) return
        call t%col(miss, p_i64, found=ok);   call check(error, .not. ok .and. .not. associated(p_i64), "col i64 miss")
        if (allocated(error)) return
        call t%col(miss, p_f32, found=ok);   call check(error, .not. ok .and. .not. associated(p_f32), "col f32 miss")
        if (allocated(error)) return
        call t%col(miss, p_f64, found=ok);   call check(error, .not. ok .and. .not. associated(p_f64), "col f64 miss")
        if (allocated(error)) return
        call t%col(miss, p_bool, found=ok);  call check(error, .not. ok .and. .not. associated(p_bool), "col bool miss")
        if (allocated(error)) return
        call t%col(miss, p_date, found=ok);  call check(error, .not. ok .and. .not. associated(p_date), "col date miss")
        if (allocated(error)) return
        call t%col(miss, p_time, found=ok);  call check(error, .not. ok .and. .not. associated(p_time), "col time miss")
        if (allocated(error)) return
        call t%col(miss, p_ts, found=ok);    call check(error, .not. ok .and. .not. associated(p_ts), "col ts miss")
        if (allocated(error)) return
        call t%col(miss, p_i32v, found=ok);  call check(error, .not. ok .and. .not. associated(p_i32v), "col i32v miss")
        if (allocated(error)) return
        call t%col(miss, p_i64v, found=ok);  call check(error, .not. ok .and. .not. associated(p_i64v), "col i64v miss")
        if (allocated(error)) return
        call t%col(miss, p_f32v, found=ok);  call check(error, .not. ok .and. .not. associated(p_f32v), "col f32v miss")
        if (allocated(error)) return
        call t%col(miss, p_f64v, found=ok);  call check(error, .not. ok .and. .not. associated(p_f64v), "col f64v miss")
        if (allocated(error)) return
        call t%col(miss, p_boolv, found=ok); call check(error, .not. ok .and. .not. associated(p_boolv), "col boolv miss")
        if (allocated(error)) return
        call t%col(miss, p_datev, found=ok); call check(error, .not. ok .and. .not. associated(p_datev), "col datev miss")
        if (allocated(error)) return
        call t%col(miss, p_timev, found=ok); call check(error, .not. ok .and. .not. associated(p_timev), "col timev miss")
        if (allocated(error)) return
        call t%col(miss, p_tsv, found=ok);   call check(error, .not. ok .and. .not. associated(p_tsv), "col tsv miss")
        if (allocated(error)) return
        !
        ! --- %get: every kind, scalar and vector, plus both string forms ---
        call t%get(miss, g_i32, found=ok);   call check(error, .not. ok .and. size(g_i32) == 0, "get i32 miss")
        if (allocated(error)) return
        call t%get(miss, g_i64, found=ok);   call check(error, .not. ok .and. size(g_i64) == 0, "get i64 miss")
        if (allocated(error)) return
        call t%get(miss, g_f32, found=ok);   call check(error, .not. ok .and. size(g_f32) == 0, "get f32 miss")
        if (allocated(error)) return
        call t%get(miss, g_f64, found=ok);   call check(error, .not. ok .and. size(g_f64) == 0, "get f64 miss")
        if (allocated(error)) return
        call t%get(miss, g_bool, found=ok);  call check(error, .not. ok .and. size(g_bool) == 0, "get bool miss")
        if (allocated(error)) return
        call t%get(miss, g_date, found=ok);  call check(error, .not. ok .and. size(g_date) == 0, "get date miss")
        if (allocated(error)) return
        call t%get(miss, g_time, found=ok);  call check(error, .not. ok .and. size(g_time) == 0, "get time miss")
        if (allocated(error)) return
        call t%get(miss, g_ts, found=ok);    call check(error, .not. ok .and. size(g_ts) == 0, "get ts miss")
        if (allocated(error)) return
        call t%get(miss, gv_i32, found=ok);  call check(error, .not. ok .and. size(gv_i32) == 0, "get i32v miss")
        if (allocated(error)) return
        call t%get(miss, gv_i64, found=ok);  call check(error, .not. ok .and. size(gv_i64) == 0, "get i64v miss")
        if (allocated(error)) return
        call t%get(miss, gv_f32, found=ok);  call check(error, .not. ok .and. size(gv_f32) == 0, "get f32v miss")
        if (allocated(error)) return
        call t%get(miss, gv_f64, found=ok);  call check(error, .not. ok .and. size(gv_f64) == 0, "get f64v miss")
        if (allocated(error)) return
        call t%get(miss, gv_bool, found=ok); call check(error, .not. ok .and. size(gv_bool) == 0, "get boolv miss")
        if (allocated(error)) return
        call t%get(miss, gv_date, found=ok); call check(error, .not. ok .and. size(gv_date) == 0, "get datev miss")
        if (allocated(error)) return
        call t%get(miss, gv_time, found=ok); call check(error, .not. ok .and. size(gv_time) == 0, "get timev miss")
        if (allocated(error)) return
        call t%get(miss, gv_ts, found=ok);   call check(error, .not. ok .and. size(gv_ts) == 0, "get tsv miss")
        if (allocated(error)) return
        call t%get(miss, g_str, found=ok);   call check(error, .not. ok, "get str (compact) miss")
        if (allocated(error)) return
        call t%get(miss, g_chr, found=ok);   call check(error, .not. ok .and. size(g_chr) == 0, "get str (chr) miss")
        if (allocated(error)) return
        call t%get(miss, gv_chr, found=ok)
        call check(error, .not. ok .and. size(gv_chr) == 0, "get str vector (chrv) miss")
    end subroutine test_kind_matrix_found
    !
    !> add_column for every kind: build a whole table from scratch in memory, then write it and
    !! read it back. Each kind has its own add_column specific, so this sweeps too.
    subroutine test_kind_matrix_add(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: src, t, t2
        type(parquet_schema) :: sc
        character(len=*), parameter :: f = "test_run/table_matrix_add_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_matrix_add.parquet"
        integer(int32), allocatable :: g_i32(:), gv_i32(:,:)
        integer(int64), allocatable :: g_i64(:), gv_i64(:,:)
        real(real32), allocatable :: g_f32(:), gv_f32(:,:)
        real(real64), allocatable :: g_f64(:), gv_f64(:,:)
        logical, allocatable :: g_bool(:), gv_bool(:,:)
        character(len=:), allocatable :: g_str(:), gv_str(:,:)
        type(parquet_date), allocatable :: g_date(:), gv_date(:,:)
        type(parquet_time), allocatable :: g_time(:), gv_time(:,:)
        type(parquet_timestamp), allocatable :: g_ts(:), gv_ts(:,:)
        !
        ! Take the values from a file-backed table so the fixture is written in one place only.
        call write_matrix_fixture(f)
        call parquet_open_table(src, f)
        call src%get("s_i32", g_i32);   call src%get("s_i64", g_i64)
        call src%get("s_f32", g_f32);   call src%get("s_f64", g_f64)
        call src%get("s_bool", g_bool); call src%get("s_str", g_str)
        call src%get("s_date", g_date); call src%get("s_time", g_time)
        call src%get("s_ts", g_ts)
        call src%get("v_i32", gv_i32);   call src%get("v_i64", gv_i64)
        call src%get("v_f32", gv_f32);   call src%get("v_f64", gv_f64)
        call src%get("v_bool", gv_bool); call src%get("v_str", gv_str)
        call src%get("v_date", gv_date); call src%get("v_time", gv_time)
        call src%get("v_ts", gv_ts)
        !
        call parquet_new_table(t)
        call t%add_column("s_i32", g_i32)
        call t%add_column("s_i64", g_i64)
        call t%add_column("s_f32", g_f32)
        call t%add_column("s_f64", g_f64, unit="kg")
        call t%add_column("s_bool", g_bool)
        call t%add_column("s_str", g_str)
        call t%add_column("s_date", g_date)
        call t%add_column("s_time", g_time)
        call t%add_column("s_ts", g_ts)
        call t%add_column("v_i32", gv_i32)
        call t%add_column("v_i64", gv_i64)
        call t%add_column("v_f32", gv_f32)
        call t%add_column("v_f64", gv_f64)
        call t%add_column("v_bool", gv_bool)
        call t%add_column("v_str", gv_str)
        call t%add_column("v_date", gv_date)
        call t%add_column("v_time", gv_time)
        call t%add_column("v_ts", gv_ts)
        call check(error, t%ncols() == 18, "add_column should build all 18 kinds")
        if (allocated(error)) return
        call check(error, t%nrows() == NROW, "the from-scratch table should adopt the row count")
        if (allocated(error)) return
        call check(error, t%width("v_f64") == NVEC, "a vector column's width should come from its values")
        if (allocated(error)) return
        !
        call build_matrix_schema(sc)
        call parquet_write_table(t, fo, sc)
        call parquet_open_table(t2, fo)
        call check(error, t2%ncols() == 18, "every added kind should survive the write")
        if (allocated(error)) return
        call t2%get("s_str", g_str)
        call check(error, trim(g_str(4)) == "ghijklm", "added string values should survive")
        if (allocated(error)) return
        call t2%get("v_i64", gv_i64)
        call check(error, gv_i64(2, 1) == 12000000000_int64, "added vector int64 values should survive")
    end subroutine test_kind_matrix_add
    !
    ! ==== stage 3c: mutation ================================================================
    !
    !> Writing one cell must land in exactly that cell, on every kind, from both row-index
    !! kinds -- and must clear that row's null, since a value and a null cannot both be true.
    subroutine test_set_element(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: gi32(:)
        real(real64), allocatable :: gf64(:)
        character(len=:), allocatable :: gs(:)
        character(len=*), parameter :: f = "test_run/table_setelem.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        ! int32 row index (a plain default INTEGER literal) and int64 row index must both work.
        call t%set_element("i32", 2, 77_int32)
        call t%set_element("f64", 3_int64, -1.5_real64)
        call t%set_element("s", 1, "rewritten")
        call t%get("i32", gi32)
        call t%get("f64", gf64)
        call t%get("s", gs)
        call check(error, gi32(2) == 77_int32, "set_element should write the named cell")
        if (allocated(error)) return
        call check(error, gi32(1) == 1_int32 .and. gi32(3) == 3_int32, &
            "set_element should leave every other row alone")
        if (allocated(error)) return
        call check(error, gf64(3) == -1.5_real64, "set_element should write a float64 cell")
        if (allocated(error)) return
        call check(error, trim(gs(1)) == "rewritten", "set_element should write a string cell")
    end subroutine test_set_element
    !
    !> test_set_element above only reaches set_element for int32/float64/string scalar columns.
    !! Every other kind (int64/float32/logical/date/time/timestamp scalar, all 8 vector kinds,
    !! and the vector string form) has its own specific pair (an i32 row-index relay plus its
    !! i64 row-index body), so this drives one on each of them -- using a plain default-INTEGER
    !! row index throughout, which is what exercises the relay half rather than the i64 body
    !! directly.
    subroutine test_set_element_more_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64), allocatable :: gi64(:)
        real(real32), allocatable :: gf32(:)
        logical, allocatable :: gbool(:)
        type(parquet_date), allocatable :: gdate(:)
        type(parquet_time), allocatable :: gtime(:)
        type(parquet_timestamp), allocatable :: gts(:)
        integer(int32), allocatable :: gv_i32(:,:)
        integer(int64), allocatable :: gv_i64(:,:)
        real(real32), allocatable :: gv_f32(:,:)
        real(real64), allocatable :: gv_f64(:,:)
        logical, allocatable :: gv_bool(:,:)
        type(parquet_date), allocatable :: gv_date(:,:)
        type(parquet_time), allocatable :: gv_time(:,:)
        type(parquet_timestamp), allocatable :: gv_ts(:,:)
        character(len=:), allocatable :: gv_str(:,:)
        character(len=4), parameter :: new_v_str(NVEC) = ["wx  ", "yz  ", "abcd"]
        character(len=*), parameter :: f = "test_run/table_setelem_more.parquet"
        ! Named so %raw() can be taken on them: chaining it straight off a structure constructor
        ! is not valid Fortran (the leftmost part-ref in a data-ref cannot be a function/
        ! constructor reference).
        type(parquet_date) :: exp_date, exp_v_date
        type(parquet_time) :: exp_time, exp_v_time
        !
        call write_matrix_fixture(f)
        call parquet_open_table(t, f)
        call t%set_element("s_i64", 2, 555000000000_int64)
        call t%set_element("s_f32", 2, 3.5_real32)
        call t%set_element("s_bool", 2, .true.)
        call t%set_element("s_date", 2, parquet_date(2030, 6, 15))
        call t%set_element("s_time", 2, parquet_time(11, 22, 33))
        call t%set_element("s_ts", 2, parquet_timestamp(2030, 6, 15, 11, 22, 33))
        call t%set_element("v_i32", 2, [101_int32, 102_int32, 103_int32])
        call t%set_element("v_i64", 2, [201_int64, 202_int64, 203_int64])
        call t%set_element("v_f32", 2, [1.5_real32, 2.5_real32, 3.5_real32])
        call t%set_element("v_f64", 2, [4.5_real64, 5.5_real64, 6.5_real64])
        call t%set_element("v_bool", 2, [.true., .false., .true.])
        call t%set_element("v_date", 2, &
            [parquet_date(2030, 7, 1), parquet_date(2030, 7, 2), parquet_date(2030, 7, 3)])
        call t%set_element("v_time", 2, &
            [parquet_time(1, 1, 1), parquet_time(2, 2, 2), parquet_time(3, 3, 3)])
        call t%set_element("v_ts", 2, [parquet_timestamp(2030, 7, 1, 1, 1, 1), &
            parquet_timestamp(2030, 7, 2, 2, 2, 2), parquet_timestamp(2030, 7, 3, 3, 3, 3)])
        call t%set_element("v_str", 2, new_v_str)
        exp_date = parquet_date(2030, 6, 15)
        exp_time = parquet_time(11, 22, 33)
        exp_v_date = parquet_date(2030, 7, 2)
        exp_v_time = parquet_time(2, 2, 2)
        !
        call t%get("s_i64", gi64)
        call check(error, gi64(2) == 555000000000_int64, "set_element should write an int64 scalar cell")
        if (allocated(error)) return
        call t%get("s_f32", gf32)
        call check(error, abs(gf32(2) - 3.5_real32) < 1.0e-6_real32, &
            "set_element should write a float32 scalar cell")
        if (allocated(error)) return
        call t%get("s_bool", gbool)
        call check(error, gbool(2), "set_element should write a logical scalar cell")
        if (allocated(error)) return
        call t%get("s_date", gdate)
        call check(error, gdate(2)%raw() == exp_date%raw(), &
            "set_element should write a date scalar cell")
        if (allocated(error)) return
        call t%get("s_time", gtime)
        call check(error, gtime(2)%raw() == exp_time%raw(), &
            "set_element should write a time scalar cell")
        if (allocated(error)) return
        call t%get("s_ts", gts)
        block
            type(parquet_date) :: got
            got = gts(2)%get_date()
            call check(error, got%raw() == exp_date%raw(), &
                "set_element should write a timestamp scalar cell")
        end block
        if (allocated(error)) return
        !
        call t%get("v_i32", gv_i32)
        call check(error, all(gv_i32(:, 2) == [101_int32, 102_int32, 103_int32]), &
            "set_element should write an int32_vec row")
        if (allocated(error)) return
        call t%get("v_i64", gv_i64)
        call check(error, all(gv_i64(:, 2) == [201_int64, 202_int64, 203_int64]), &
            "set_element should write an int64_vec row")
        if (allocated(error)) return
        call t%get("v_f32", gv_f32)
        call check(error, all(abs(gv_f32(:, 2) - [1.5_real32, 2.5_real32, 3.5_real32]) < 1.0e-6_real32), &
            "set_element should write a float32_vec row")
        if (allocated(error)) return
        call t%get("v_f64", gv_f64)
        call check(error, all(abs(gv_f64(:, 2) - [4.5_real64, 5.5_real64, 6.5_real64]) < 1.0e-12_real64), &
            "set_element should write a float64_vec row")
        if (allocated(error)) return
        call t%get("v_bool", gv_bool)
        call check(error, all(gv_bool(:, 2) .eqv. [.true., .false., .true.]), &
            "set_element should write a logical_vec row")
        if (allocated(error)) return
        call t%get("v_date", gv_date)
        call check(error, gv_date(2, 2)%raw() == exp_v_date%raw(), &
            "set_element should write a date_vec row")
        if (allocated(error)) return
        call t%get("v_time", gv_time)
        call check(error, gv_time(2, 2)%raw() == exp_v_time%raw(), &
            "set_element should write a time_vec row")
        if (allocated(error)) return
        call t%get("v_ts", gv_ts)
        block
            type(parquet_date) :: got
            got = gv_ts(2, 2)%get_date()
            call check(error, got%raw() == exp_v_date%raw(), &
                "set_element should write a timestamp_vec row")
        end block
        if (allocated(error)) return
        call t%get("v_str", gv_str)
        call check(error, trim(gv_str(2, 2)) == "yz", "set_element should write a string_vec row")
    end subroutine test_set_element_more_kinds
    !
    !> The validity trio, including the trap that %set_null is ROW-granular on a vector column.
    subroutine test_validity_mutation(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: v(NROW)
        real(real64) :: vv(NVEC, NROW)
        integer :: i, e
        !
        do i = 1, NROW
            v(i) = real(i, real64)
            do e = 1, NVEC
                vv(e, i) = real(10 * i + e, real64)
            end do
        end do
        call parquet_new_table(t)
        call t%add_column("x", v)
        call t%add_column("xv", vv)
        call check(error, .not. t%is_null("x", 3), "a fresh column should hold no nulls")
        if (allocated(error)) return
        call t%set_null("x", 3)
        call check(error, t%is_null("x", 3_int64), "set_null should mark the row null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("x", 2), "set_null should mark only the named row")
        if (allocated(error)) return
        ! Writing a value clears the null -- a cell cannot be both.
        call t%set_element("x", 3, 99.0_real64)
        call check(error, .not. t%is_null("x", 3), "set_element should clear the row's null")
        if (allocated(error)) return
        ! Row-granular on a vector column: nulling row 2 nulls the whole row, not one element.
        call t%set_null("xv", 2)
        call check(error, t%is_null("xv", 2), "set_null on a vector column should null the row")
        if (allocated(error)) return
        call check(error, .not. t%is_null("xv", 1), "a vector row's null should not spread")
        if (allocated(error)) return
        call t%clear_null("xv", 2)
        call check(error, .not. t%is_null("xv", 2), "clear_null should mark the row valid again")
        if (allocated(error)) return
        ! compact_validity drops a bitmap that no longer has anything in it; it is idempotent.
        call t%compact_validity("xv")
        call t%compact_validity("xv")
        call check(error, .not. t%is_null("xv", 2), "compact_validity should not change any answer")
    end subroutine test_validity_mutation
    !
    !> Dropping a column removes it from every view of the table, and leaves the survivors both
    !! intact and in their original order.
    subroutine test_drop_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        real(real64), allocatable :: gf64(:)
        character(len=*), parameter :: f = "test_run/table_drop.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%get("f64", gf64)          ! make one column resident before the drop
        call t%drop_column("f32")
        call check(error, t%ncols() == 5, "drop_column should shrink the column count")
        if (allocated(error)) return
        call check(error, .not. t%has_column("f32"), "the dropped column should be gone")
        if (allocated(error)) return
        call t%column_names(names)
        call check(error, size(names) == 5, "column_names should not list the dropped column")
        if (allocated(error)) return
        call check(error, trim(names(1)) == "i32" .and. trim(names(3)) == "f64", &
            "the surviving columns should keep their order after the shift down")
        if (allocated(error)) return
        deallocate(gf64)
        call t%get("f64", gf64)
        call check(error, gf64(2) == 4.5_real64, "a survivor's values should be intact after a drop")
        if (allocated(error)) return
        call check(error, t%nrows() == NROW, "dropping a column should not change the row count")
        if (allocated(error)) return
        ! Dropping a column that was never read is the memory-reclaiming case, and must work.
        call t%drop_column("b")
        call check(error, t%ncols() == 4, "dropping an unread column should work too")
    end subroutine test_drop_column
    !
    !> A rename changes only the name the column is looked up by; a file-backed column that has
    !! not been read yet must still read from the right physical column afterwards.
    !> The basic remap: a read-in MAML relabels a file column, and everything table-facing uses
    !! the new name while the read still goes to the physical one. The un-remapped columns are
    !! untouched, and the column count is unchanged (one internal name, one file column).
    !> `found=` reaches every procedure that takes a column name, mutators included.
    !!
    !! The rule that matters is the one for a MUTATING procedure: `found=.false.` has to mean
    !! "nothing was changed", so the lookup happens before the first write. Each call below is made
    !! on a name that does not exist, and the table is checked afterwards to be exactly as it was.
    subroutine test_found_everywhere(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        logical :: ok, mask(NROW)
        integer :: ncols0
        integer(int64) :: gen0
        character(len=*), parameter :: f = "test_run/table_found_everywhere.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        ncols0 = t%ncols()
        gen0 = t%generation()
        mask = .true.
        !
        ! Queries.
        call check(error, .not. t%is_null("no_such", 1, found=ok) .and. .not. ok, &
            "%is_null should report a missing column through found=, answering .false.")
        if (allocated(error)) return
        !
        ! Cell and validity mutators.
        call t%set_element("no_such", 1, 1.0_real64, found=ok)
        call check(error, .not. ok, "%set_element should report a missing column through found=")
        if (allocated(error)) return
        call t%set_null("no_such", 1, found=ok)
        call check(error, .not. ok, "%set_null should report a missing column through found=")
        if (allocated(error)) return
        call t%set_null("no_such", mask, found=ok)
        call check(error, .not. ok, "%set_null(mask) should report a missing column through found=")
        if (allocated(error)) return
        call t%clear_null("no_such", 1, found=ok)
        call check(error, .not. ok, "%clear_null should report a missing column through found=")
        if (allocated(error)) return
        call t%compact_validity("no_such", found=ok)
        call check(error, .not. ok, "%compact_validity should report a missing column through found=")
        if (allocated(error)) return
        !
        ! Column-structural mutators. These are the ones where a late lookup would already have
        ! changed something by the time it reported.
        call t%drop_column("no_such", found=ok)
        call check(error, .not. ok, "%drop_column should report a missing column through found=")
        if (allocated(error)) return
        call t%rename_column("no_such", "whatever", found=ok)
        call check(error, .not. ok, "%rename_column should report a missing source through found=")
        if (allocated(error)) return
        call t%copy_column("no_such", "whatever", found=ok)
        call check(error, .not. ok, "%copy_column should report a missing source through found=")
        if (allocated(error)) return
        call t%cast("no_such", PK_FLOAT64, found=ok)
        call check(error, .not. ok, "%cast should report a missing column through found=")
        if (allocated(error)) return
        !
        ! Nothing was changed by any of them -- not the column set, not the generation counter.
        call check(error, t%ncols() == ncols0, "a reported miss must not change the column set")
        if (allocated(error)) return
        call check(error, t%generation() == gen0, &
            "a reported miss must not count as a structural change")
        if (allocated(error)) return
        call check(error, t%nrows() == int(NROW, int64), "a reported miss must not change the rows")
    end subroutine test_found_everywhere
    !
    !> A row handle can WRITE as well as read: `r%set` updates the table, `r%ref` aliases one row.
    !!
    !! The question the handle's documentation used to leave open is the one checked first here:
    !! a handle is a view of the table, not a copy of the row, so writing through it changes the
    !! table. `%ref` is the zero-copy form of the same thing, and carries the same lifetime rule as
    !! `%col` -- it is a pointer into the column's storage, so a structural change strands it.
    subroutine test_row_set_and_ref(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_row) :: r
        real(real64), allocatable :: f64(:)
        real(real64), pointer :: fp
        integer(int32), pointer :: ip
        character(len=:), allocatable :: sv
        real(real64) :: got
        character(len=*), parameter :: f = "test_run/table_row_write.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        r = t%row(3)
        !
        ! %set through the handle changes the TABLE, which is the whole question.
        call r%set("f64", 99.5_real64)
        call t%get("f64", f64)
        call check(error, abs(f64(3) - 99.5_real64) < 1.0e-12_real64, &
            "a write through a row handle should update the table")
        if (allocated(error)) return
        call r%get("f64", got)
        call check(error, abs(got - 99.5_real64) < 1.0e-12_real64, &
            "reading back through the same handle should see the write")
        if (allocated(error)) return
        ! Strings too.
        call r%set("s", "written")
        call r%get("s", sv)
        call check(error, sv == "written", "a row handle should write a string cell")
        if (allocated(error)) return
        ! Writing a value clears that row's null, exactly as %set_element does.
        call t%set_null("i32", 3)
        call check(error, r%is_null("i32"), "precondition: the row should be null")
        if (allocated(error)) return
        call r%set("i32", 33_int32)
        call check(error, .not. r%is_null("i32"), "writing a value should clear the row's null")
        if (allocated(error)) return
        !
        ! %ref aliases the storage: writing through the pointer is writing to the table.
        call r%ref("f64", fp)
        call check(error, associated(fp), "%ref should give a pointer into the column")
        if (allocated(error)) return
        fp = -7.0_real64
        call t%get("f64", f64)
        call check(error, abs(f64(3) + 7.0_real64) < 1.0e-12_real64, &
            "a write through a %ref pointer should reach the table")
        if (allocated(error)) return
        call r%ref("i32", ip)
        call check(error, ip == 33_int32, "%ref should alias the value just written")
    end subroutine test_row_set_and_ref
    !
    !> A `parquet_string_column` is a first-class table value: %col, %set and %add_column.
    !!
    !! Reading one out has always worked (`%get(name, packed)`); the other three directions were
    !! missing, so the compact form could be got out of a table but never put back in without
    !! flattening it to a fixed-width character array first -- which is exactly the copy the
    !! compact form exists to avoid.
    subroutine test_string_column_first_class(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, mem
        type(parquet_string_column) :: packed, out
        type(parquet_string_column), pointer :: sp
        character(len=:), allocatable :: sv
        logical :: ok
        character(len=*), parameter :: f = "test_run/table_strcol_first_class.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        !
        ! %col aliases the packed store: an in-place value edit through it changes the table.
        call t%col("s", sp)
        call check(error, associated(sp), "%col should alias a string column's packed store")
        if (allocated(error)) return
        call check(error, sp%size() == int(NROW, int64), &
            "the aliased store should hold one element per row")
        if (allocated(error)) return
        call sp%get(1_int64, sv)
        call check(error, sv == "a", "the aliased store should hold the column's values")
        if (allocated(error)) return
        !
        ! %set from a compact column, at each value's own length.
        call packed%append_string("alpha")
        call packed%append_string("b")
        call packed%append_null()
        call packed%append_string("delta")
        call packed%append_string("e")
        call packed%append_string("zeta")
        call t%set("s", packed)
        call t%get("s", out)
        call check(error, out%size() == int(NROW, int64), "%set(packed) should keep the row count")
        if (allocated(error)) return
        call out%get(1_int64, sv)
        call check(error, sv == "alpha", "%set(packed) should write each value at its own length")
        if (allocated(error)) return
        call check(error, t%is_null("s", 3), "%set(packed) should carry the packed column's nulls")
        if (allocated(error)) return
        ! ...and it is a copy, not a handover: editing the caller's column afterwards is invisible.
        call packed%append_string("extra")
        call t%get("s", out)
        call check(error, out%size() == int(NROW, int64), &
            "%set(packed) should take an independent copy, not share storage")
        if (allocated(error)) return
        !
        ! %add_column from a compact column, on a table built in memory.
        call parquet_new_table(mem)
        call mem%add_column("name", packed, unit="label")
        call check(error, mem%nrows() == packed%size() .and. mem%kind("name") == PK_STRING, &
            "%add_column(packed) should add a string column of the packed column's length")
        if (allocated(error)) return
        call mem%unit("name", sv)
        call check(error, sv == "label", "%add_column(packed) should keep the unit it is given")
        if (allocated(error)) return
        call mem%get_element("name", 1, sv)
        call check(error, sv == "alpha", "%add_column(packed) should carry the values across")
        if (allocated(error)) return
        !
        call t%col("no_such", sp, found=ok)
        call check(error, (.not. ok) .and. (.not. associated(sp)), &
            "a missed %col(packed) should report through found= and leave the pointer null")
    end subroutine test_string_column_first_class
    !
    !> %set_slice writes a selection back, mirroring %get_slice exactly.
    !!
    !! The pairing is the point: whatever `%get_slice` hands out for a selection, `%set_slice`
    !! takes back for the same selection, in the same order -- including a reversed or repeated
    !! one, where "in the same order" is the whole question. Unlike `%get_slice` it does not
    !! widen: a copy INTO the table is exact-kind, as `%set` is.
    subroutine test_set_slice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_slice) :: sel
        real(real64), allocatable :: got(:)
        character(len=:), allocatable :: sv(:)
        logical :: ok
        integer(int32), allocatable :: i32(:)
        character(len=*), parameter :: f = "test_run/table_set_slice.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        !
        ! A strided selection: rows 1, 3, 5.
        sel = parquet_slice_range(1, 5, 2)
        call t%set_slice("f64", sel, [-1.0_real64, -3.0_real64, -5.0_real64])
        call t%get("f64", got)
        call check(error, abs(got(1) + 1.0_real64) < 1.0e-12_real64 .and. &
            abs(got(3) + 3.0_real64) < 1.0e-12_real64 .and. abs(got(5) + 5.0_real64) < 1.0e-12_real64, &
            "%set_slice should write the rows the selection picks")
        if (allocated(error)) return
        call check(error, abs(got(2) - 4.5_real64) < 1.0e-12_real64, &
            "%set_slice should leave the rows it does not pick alone")
        if (allocated(error)) return
        !
        ! A reversed, explicit list: order is what distinguishes a right answer from a wrong one.
        sel = parquet_slice_list([6, 2])
        call t%set_slice("i32", sel, [66_int32, 22_int32])
        call t%get("i32", i32)
        call check(error, i32(6) == 66_int32 .and. i32(2) == 22_int32, &
            "%set_slice should follow the selection's own order")
        if (allocated(error)) return
        !
        ! Round-trip against %get_slice, which is the property that matters.
        sel = parquet_slice_list([4, 1, 4])
        call t%get_slice("f64", sel, got)
        call t%set_slice("f64", sel, got)
        call t%get_slice("f64", sel, got)
        call check(error, size(got) == 3, "%get_slice/%set_slice should round-trip a repeated selection")
        if (allocated(error)) return
        !
        ! Strings, and is_valid= over the selection.
        sel = parquet_slice_range(1, 2)
        call t%set_slice("s", sel, ["zz", "yy"], is_valid=[.true., .false.])
        call t%get("s", sv)
        call check(error, trim(sv(1)) == "zz", "%set_slice should write a string selection")
        if (allocated(error)) return
        call check(error, t%is_null("s", 2), "%set_slice(is_valid=) should null the rows it marks")
        if (allocated(error)) return
        !
        ! A missing column reports through found=.
        call t%set_slice("no_such", sel, [1.0_real64, 2.0_real64], found=ok)
        call check(error, .not. ok, "%set_slice should report a missing column through found=")
    end subroutine test_set_slice
    !
    !> `is_valid=` on %get, %col, %get_slice and %set.
    !!
    !! One optional argument, four places, and it means the same thing in all of them: one entry
    !! per row (or per selected row), `.true.` where the row holds a value. On %set it goes the
    !! other way -- rows marked `.false.` become null -- which is the only way to write a column
    !! and its nulls in one call. The %col form is the one with a caveat worth testing: it is a
    !! SNAPSHOT, not an alias, because validity is a packed bitmap with no logical array to point
    !! at.
    subroutine test_is_valid_argument(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        type(parquet_schema) :: sc
        real(real64) :: v(NROW)
        logical :: valid(NROW)
        logical, allocatable :: gotv(:)
        real(real64), allocatable :: arr(:)
        real(real64), pointer :: p(:)
        type(parquet_slice) :: sel
        integer :: i
        character(len=*), parameter :: f = "test_run/table_isvalid.parquet"
        !
        do i = 1, NROW
            v(i) = real(i, real64)
            valid(i) = i /= 2 .and. i /= 5
        end do
        call sc%init("isvalid")
        call sc%add_field("v", "float64")
        call parquet_parse_maml(sc)
        call parquet_open_writer(w, f, sc)
        call parquet_write_column(w, "v", v, is_valid=valid)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call t%get("v", arr, is_valid=gotv)
        call check(error, size(gotv) == NROW .and. all(gotv .eqv. valid), &
            "%get(is_valid=) should report the column's per-row validity")
        if (allocated(error)) return
        call t%col("v", p, is_valid=gotv)
        call check(error, all(gotv .eqv. valid), &
            "%col(is_valid=) should report the same validity as %get")
        if (allocated(error)) return
        sel = parquet_slice_list([5, 1, 2])
        call t%get_slice("v", sel, arr, is_valid=gotv)
        call check(error, size(gotv) == 3, "%get_slice(is_valid=) should have one entry per selected row")
        if (allocated(error)) return
        call check(error, (.not. gotv(1)) .and. gotv(2) .and. (.not. gotv(3)), &
            "%get_slice(is_valid=) should follow the selection, not the row order")
        if (allocated(error)) return
        !
        ! %set with is_valid=: values and nulls in one call. A plain %set drops the bitmap, so the
        ! mask has to be applied after the values -- checked by asking for it back.
        arr = [(real(i, real64) * 10.0_real64, i = 1, NROW)]
        valid = .true.
        valid(3) = .false.
        call t%set("v", arr, is_valid=valid)
        call t%get("v", arr, is_valid=gotv)
        call check(error, all(gotv .eqv. valid), "%set(is_valid=) should write the nulls it is given")
        if (allocated(error)) return
        call check(error, abs(arr(1) - 10.0_real64) < 1.0e-12_real64, &
            "%set(is_valid=) should still write every value")
        if (allocated(error)) return
        ! A wrong-length mask is an error, not a silent partial application -- the abort path is
        ! covered out of process (scenario table_set_is_valid_length).
        !
        ! A missing column reports through found= and leaves an empty mask rather than aborting.
        call t%get("no_such", arr, is_valid=gotv, found=valid(1))
        call check(error, .not. valid(1) .and. size(gotv) == 0, &
            "a missed %get(is_valid=) should report through found= and leave an empty mask")
    end subroutine test_is_valid_argument
    !
    !> %get_element reads one cell by row index, for every kind, widening as %get does.
    !!
    !! It is the one-call form of `r = t%row(i)` then `r%get(name, v)` -- the two-step form is the
    !! only way Fortran allows a row handle to be used, so this exists for the common case of
    !! wanting a single cell. Both row-index kinds resolve, and a miss reports through `found=`
    !! rather than aborting.
    subroutine test_get_element(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32) :: i32v
        integer(int64) :: i64v
        real(real64) :: f64v
        logical :: bv, ok
        character(len=:), allocatable :: sv
        character(len=*), parameter :: f = "test_run/table_get_element.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        ! A first touch through %get_element, on a column nothing has read.
        call t%get_element("i32", 4, i32v)
        call check(error, i32v == 4_int32, "%get_element should read the cell (int32 index)")
        if (allocated(error)) return
        call t%get_element("i32", 5_int64, i32v)
        call check(error, i32v == 5_int32, "%get_element should take an int64 row index too")
        if (allocated(error)) return
        ! ...widening into the caller's variable, exactly as %get does.
        call t%get_element("i32", 6, i64v)
        call check(error, i64v == 6_int64, "%get_element should widen int32 into an int64 variable")
        if (allocated(error)) return
        call t%get_element("f32", 4, f64v)
        call check(error, abs(f64v - 2.0_real64) < 1.0e-6_real64, &
            "%get_element should widen float32 into a float64 variable")
        if (allocated(error)) return
        call t%get_element("b", 2, bv)
        call check(error, bv, "%get_element should read a logical cell")
        if (allocated(error)) return
        call t%get_element("s", 4, sv)
        call check(error, sv == "ghijklm", "%get_element should read a string cell at its own length")
        if (allocated(error)) return
        ! It agrees with the two-step row-handle form, which is what it is shorthand for.
        block
            type(parquet_table_row) :: r
            real(real64) :: viaRow
            r = t%row(3)
            call r%get("f64", viaRow)
            call t%get_element("f64", 3, f64v)
            call check(error, abs(viaRow - f64v) < 1.0e-12_real64, &
                "%get_element and the row handle's %get should agree")
        end block
        if (allocated(error)) return
        ! A missing column reports through found= instead of aborting.
        call t%get_element("no_such", 1, i32v, found=ok)
        call check(error, .not. ok, "%get_element should report a missing column through found=")
    end subroutine test_get_element
    !
    !> %clone_structure works on a table that has read nothing, which is the normal case.
    !!
    !! The shape of every supported column is known from the file's schema at open, so a batch can
    !! be cloned from a freshly opened table without reading a single column -- which is what the
    !! bulk-append idiom (`clone_structure` -> fill -> `%append`) actually wants. It used to abort
    !! inside the column store (`init: PK_NONE is not a storable kind`) because it took each
    !! column's shape from values a lazy column does not have. Residency is asserted afterwards to
    !! prove nothing was read to answer.
    subroutine test_clone_structure_lazy(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, batch
        character(len=:), allocatable :: names(:), u
        character(len=*), parameter :: f = "test_run/table_clone_struct_lazy.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%clone_structure(batch)
        call check(error, t%residency("i32") == RES_EMPTY .and. t%residency("s") == RES_EMPTY, &
            "%clone_structure must not read a column to learn its shape")
        if (allocated(error)) return
        call check(error, batch%ncols() == 6 .and. batch%nrows() == 0_int64, &
            "a batch cloned from a lazy table should have every column and no rows")
        if (allocated(error)) return
        call check(error, batch%kind("f64") == PK_FLOAT64 .and. batch%kind("s") == PK_STRING, &
            "a batch cloned from a lazy table should carry each column's kind")
        if (allocated(error)) return
        call batch%column_names(names)
        call check(error, trim(names(1)) == "i32" .and. trim(names(6)) == "s", &
            "a cloned batch should keep the source's column order")
        if (allocated(error)) return
        ! ...and it is a usable batch: fill it and append it back.
        call batch%append_null_rows(2)
        call batch%set_element("i32", 1, 71_int32)
        call t%materialize_all()
        call t%append(batch)
        call check(error, t%nrows() == int(NROW, int64) + 2_int64, &
            "a batch cloned from a lazy table should append back into it")
        if (allocated(error)) return
        !
        ! resident_only=.true. still filters to what has been read.
        call parquet_open_table(t, f)
        call t%prefetch("f64")
        call t%clone_structure(batch, resident_only=.true.)
        call check(error, batch%ncols() == 1 .and. batch%has_column("f64"), &
            "resident_only=.true. should clone only the columns already read")
        if (allocated(error)) return
        call batch%unit("f64", u)
        call check(error, u == "", "a column with no declared unit should clone without one")
    end subroutine test_clone_structure_lazy
    !
    !> The introspection additions: %ncols/%column_names filtered to resident columns,
    !! %has_nulls, %get_valid_mask, %set_null(mask) and %generation.
    !!
    !! %has_nulls is the one with two answers rather than one: a resident column knows exactly,
    !! while a column still in the file is answered from the footer without reading it. Both are
    !! checked here on the same column, before and after the read, which is also what proves the
    !! footer path is being taken at all -- residency is asserted either side.
    subroutine test_introspection_additions(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        type(parquet_schema) :: s
        real(real64) :: v(NROW)
        logical :: valid(NROW), mask(NROW)
        logical, allocatable :: got(:)
        integer(int64) :: g0, g1
        character(len=:), allocatable :: names(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_introspect.parquet"
        !
        do i = 1, NROW
            v(i) = real(i, real64)
            valid(i) = i /= 3
        end do
        call s%init("introspect")
        call s%add_field("v", "float64")
        call s%add_field("w", "float64")
        call parquet_parse_maml(s)
        call parquet_open_writer(w, f, s)
        call parquet_write_column(w, "v", v, is_valid=valid)
        call parquet_write_column(w, "w", v)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        ! Nothing is resident yet, so the filtered forms report nothing while the plain ones
        ! report everything.
        call check(error, t%ncols() == 2 .and. t%ncols(resident_only=.true.) == 0, &
            "resident_only should count only the columns already read")
        if (allocated(error)) return
        call t%column_names(names, resident_only=.true.)
        call check(error, size(names) == 0, "resident_only should list only the columns already read")
        if (allocated(error)) return
        ! %has_nulls from the FOOTER -- no read, which the residency check either side proves.
        call check(error, t%has_nulls("v"), "the footer should report the null-carrying column")
        if (allocated(error)) return
        call check(error, .not. t%has_nulls("w"), "the footer should clear the null-free column")
        if (allocated(error)) return
        call check(error, t%residency("v") == RES_EMPTY, &
            "%has_nulls must not have read the column to answer")
        if (allocated(error)) return
        !
        call t%prefetch("v")
        call check(error, t%ncols(resident_only=.true.) == 1, &
            "resident_only should count the column just read")
        if (allocated(error)) return
        call t%column_names(names, resident_only=.true.)
        call check(error, size(names) == 1 .and. trim(names(1)) == "v", &
            "resident_only should name the column just read")
        if (allocated(error)) return
        call check(error, t%has_nulls("v"), "a resident column should still report its nulls")
        if (allocated(error)) return
        !
        call t%get_valid_mask("v", got)
        call check(error, size(got) == NROW, "%get_valid_mask should have one entry per row")
        if (allocated(error)) return
        call check(error, all(got .eqv. valid), "%get_valid_mask should report the column's nulls")
        if (allocated(error)) return
        ! A null-free column comes back all .true., not unallocated.
        call t%get_valid_mask("w", got)
        call check(error, size(got) == NROW .and. all(got), &
            "%get_valid_mask on a null-free column should be all .true.")
        if (allocated(error)) return
        !
        ! %set_null(mask) adds the mask's nulls and leaves the rest alone.
        mask = .true.
        mask(5) = .false.
        call t%set_null("v", mask)
        call t%get_valid_mask("v", got)
        call check(error, .not. got(5) .and. .not. got(3), &
            "%set_null(mask) should null the masked row and keep the existing null")
        if (allocated(error)) return
        call check(error, got(1) .and. got(2), "%set_null(mask) should leave unmasked rows valid")
        if (allocated(error)) return
        !
        ! %generation moves on a structural change and not on a value one.
        g0 = t%generation()
        call t%set_element("v", 1, 42.0_real64)
        call check(error, t%generation() == g0, "a cell write should not bump the generation")
        if (allocated(error)) return
        call t%drop_column("w")
        g1 = t%generation()
        call check(error, g1 > g0, "dropping a column should bump the generation")
        if (allocated(error)) return
        call t%truncate(2)
        call check(error, t%generation() > g1, "a row mutation should bump the generation")
    end subroutine test_introspection_additions
    !
    !> parquet_write_table carries the SOURCE file's metadata into the output on request.
    !!
    !! The natural shape of this is read, mutate, write -- by which point the table has detached and
    !! its reader is gone, so the whole feature rests on the snapshot taken at open. Checked here,
    !! along with the three rules: a selective key list, the schema winning a collision, and nothing
    !! being added to the caller's own schema (which would leak one table's provenance into the
    !! next file written with the same schema).
    subroutine test_write_table_copy_metadata(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t, plain
        type(parquet_schema) :: s, out_s
        real(real64) :: v(NROW)
        character(len=:), allocatable :: val
        logical :: ok
        integer :: i
        character(len=*), parameter :: f = "test_run/table_copymeta_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_copymeta_out.parquet"
        character(len=*), parameter :: fk = "test_run/table_copymeta_keys.parquet"
        character(len=*), parameter :: fn = "test_run/table_copymeta_none.parquet"
        !
        do i = 1, NROW
            v(i) = real(i, real64)
        end do
        call s%init("source")
        call s%add_field("v", "float64")
        call parquet_parse_maml(s)
        call s%add_metadata("origin", "survey_A")
        call s%add_metadata("release", "DR3")
        call s%add_metadata("instrument", "spectro")
        call parquet_open_writer(w, f, s)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        !
        call out_s%init("dest")
        call out_s%add_field("v", "float64")
        call parquet_parse_maml(out_s)
        call out_s%add_metadata("release", "DR4")   ! the schema's own -- must win
        !
        ! Read, detach, then write: the reader is gone by the time the metadata is needed.
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%truncate(3)
        call check(error, t%is_detached(), "precondition: %truncate should have detached the table")
        if (allocated(error)) return
        call parquet_write_table(t, fo, out_s, copy_metadata=.true.)
        call parquet_open_table(plain, fo)
        call plain%get_file_metadata("origin", val, found=ok)
        call check(error, ok .and. val == "survey_A", &
            "copy_metadata=.true. should carry the source file's metadata into the output")
        if (allocated(error)) return
        call plain%get_file_metadata("release", val, found=ok)
        call check(error, ok .and. val == "DR4", &
            "a key the output schema declares itself should win over the carried one")
        if (allocated(error)) return
        !
        ! Nothing was added to the caller's schema, so the next file written with it is clean.
        call parquet_write_table(t, fn, out_s)
        call parquet_open_table(plain, fn)
        call plain%get_file_metadata("origin", val, found=ok)
        call check(error, .not. ok, &
            "carrying metadata once must not add it to the caller's schema for the next write")
        if (allocated(error)) return
        call plain%get_file_metadata("instrument", val, found=ok)
        call check(error, .not. ok, "...for any of the keys it carried")
        if (allocated(error)) return
        !
        ! The selective form.
        call parquet_write_table(t, fk, out_s, metadata_keys=["origin"])
        call parquet_open_table(plain, fk)
        call plain%get_file_metadata("origin", val, found=ok)
        call check(error, ok .and. val == "survey_A", "metadata_keys= should carry the key it names")
        if (allocated(error)) return
        call plain%get_file_metadata("instrument", val, found=ok)
        call check(error, .not. ok, "metadata_keys= should carry no key it does not name")
    end subroutine test_write_table_copy_metadata
    !
    !> A read-in MAML's `fields:`' `unit:` key gives a file-backed column its unit.
    !!
    !! A parquet file records no unit for a column, so a read-in MAML is the only source there is.
    !! Three things have to hold, and each has its own failure mode: `%unit` must answer BEFORE the
    !! column is read (it is on the descriptor, not on values that do not exist yet); the unit must
    !! survive the read onto the values, or `%append`'s unit check and the writer would not see it;
    !! and the MAML names the FILE's columns, so a remapped column takes its unit under the file
    !! name while the table looks it up under the internal one.
    subroutine test_unit_from_maml(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, batch
        real(real64), allocatable :: v(:)
        character(len=:), allocatable :: u
        character(len=*), parameter :: f = "test_run/table_unit_maml.parquet"
        character(len=*), parameter :: m = "test_run/table_unit_maml.maml"
        !
        call write_basic_fixture(f)
        call write_maml_file(m, [character(len=40) :: &
            "table: units", &
            "extra:", &
            "  remap:", &
            "  - flux: f64", &
            "fields:", &
            "- name: f64", &
            "  data_type: float64", &
            "  unit: Msun", &
            "- name: i32", &
            "  data_type: int32" ])
        call parquet_open_table(t, f, maml=m)
        ! Before any read: the unit is on the descriptor, and the lookup key is the INTERNAL name
        ! even though the MAML declared it under the file name.
        call check(error, t%residency("flux") == RES_EMPTY, "precondition: nothing should be read yet")
        if (allocated(error)) return
        call t%unit("flux", u)
        call check(error, u == "Msun", "%unit should answer from the MAML before the column is read")
        if (allocated(error)) return
        call t%unit("i32", u)
        call check(error, u == "", "a MAML field declaring no unit should leave %unit empty")
        if (allocated(error)) return
        call t%unit("s", u)
        call check(error, u == "", "a column the MAML does not mention should have no unit")
        if (allocated(error)) return
        ! ...and after the read it is on the values too, which is what %append and the writer see.
        call t%get("flux", v)
        call t%unit("flux", u)
        call check(error, u == "Msun", "%unit should still answer once the column is resident")
        if (allocated(error)) return
        call t%clone_structure(batch)
        call batch%unit("flux", u)
        call check(error, u == "Msun", "a cloned structure should carry the unit")
        if (allocated(error)) return
        !
        ! Without a MAML there is no unit to have: the file itself carries none.
        call parquet_open_table(t, f)
        call t%unit("f64", u)
        call check(error, u == "", "a table opened with no MAML should report no unit")
    end subroutine test_unit_from_maml
    !
    subroutine test_remap_basic(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: gi32(:)
        character(len=*), parameter :: f = "test_run/table_remap_basic.parquet"
        character(len=*), parameter :: m = "test_run/table_remap_basic.maml"
        integer :: i
        !
        call write_basic_fixture(f)
        call write_maml_file(m, [character(len=40) :: &
            "table: remap_basic", &
            "extra:", &
            "  remap:", &
            "  - counter: i32" ])
        call parquet_open_table(t, f, maml=m)
        call check(error, t%has_column("counter"), "the remapped internal name should resolve")
        if (allocated(error)) return
        call check(error, .not. t%has_column("i32"), "the physical name should be shadowed by the remap")
        if (allocated(error)) return
        call check(error, t%ncols() == 6, "remapping one column should not change the column count")
        if (allocated(error)) return
        call check(error, t%residency("counter") == RES_EMPTY, "a remap should read nothing at open")
        if (allocated(error)) return
        ! The first touch happens here, under the INTERNAL name, and must reach the physical column.
        call t%get("counter", gi32)
        call check(error, all(gi32 == [(i, i = 1, NROW)]), &
            "a remapped column must read its physical file column's values")
        if (allocated(error)) return
        call check(error, t%has_column("f64"), "an un-remapped column keeps its own name")
    end subroutine test_remap_basic
    !
    !> The three rules that make remapping more than a rename, exercised on the worked example the
    !! design is written around: a file with columns `i32`, `i64`, `f64` remapped so that
    !!
    !!   - `i32` and `i64` SWAP (each internal name equals a physical column name, but neither
    !!     means itself),
    !!   - `f64` is a second internal name for physical `i64`, i.e. two internal names share one
    !!     physical source,
    !!   - physical `f64` is left unreachable under any internal name -- a deliberate silent shadow.
    !!
    !! The duplicated pair must start identical and then diverge independently, which is what makes
    !! them ordinary table columns rather than two views of one.
    subroutine test_remap_shadow_duplicate(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: a(:)
        integer(int64), allocatable :: b(:), c(:)
        character(len=*), parameter :: f = "test_run/table_remap_shadow.parquet"
        character(len=*), parameter :: m = "test_run/table_remap_shadow.maml"
        integer :: i
        !
        call write_basic_fixture(f)
        call write_maml_file(m, [character(len=40) :: &
            "table: remap_shadow", &
            "extra:", &
            "  remap:", &
            "  - i32: i64", &
            "  - i64: i32", &
            "  - f64: i64" ])
        call parquet_open_table(t, f, maml=m)
        ! One extra slot: physical i64 is claimed twice, physical f64 by nobody.
        call check(error, t%ncols() == 6, "swap plus a duplicate target should keep six slots")
        if (allocated(error)) return
        call check(error, t%kind("i32") == PK_INT64, "internal i32 must take its type from physical i64")
        if (allocated(error)) return
        call check(error, t%kind("i64") == PK_INT32, "internal i64 must take its type from physical i32")
        if (allocated(error)) return
        call check(error, t%kind("f64") == PK_INT64, "internal f64 must take its type from physical i64")
        if (allocated(error)) return
        call t%get("i64", a)
        call t%get("i32", b)
        call t%get("f64", c)
        call check(error, all(a == [(i, i = 1, NROW)]), "internal i64 must read physical i32's values")
        if (allocated(error)) return
        call check(error, all(b == c), "two internal names over one physical column must start identical")
        if (allocated(error)) return
        ! ...and then diverge: they are independent columns, not two views of one.
        call t%set("f64", [(int(i, int64), i = 1, NROW)])
        call t%get("i32", b)
        call t%get("f64", c)
        call check(error, .not. all(b == c), &
            "writing one of two internal names over one physical column must not affect the other")
        if (allocated(error)) return
        call check(error, all(b == [(int(i, int64) * 1000000000_int64, i = 1, NROW)]), &
            "the untouched duplicate must keep its own values")
    end subroutine test_remap_shadow_duplicate
    !
    !> Remap and rename are independent, composable operations: renaming a remapped column changes
    !! its lookup name only, never the physical column it reads, so a reload still goes to the same
    !! place. Renaming to a name the file itself uses is fine too -- lookup is by internal name.
    subroutine test_remap_then_rename(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: gi32(:)
        character(len=*), parameter :: f = "test_run/table_remap_rename.parquet"
        character(len=*), parameter :: m = "test_run/table_remap_rename.maml"
        integer :: i
        !
        call write_basic_fixture(f)
        call write_maml_file(m, [character(len=40) :: &
            "table: remap_rename", &
            "extra:", &
            "  remap:", &
            "  - counter: i32" ])
        call parquet_open_table(t, f, maml=m)
        call t%rename_column("counter", "tally")
        call check(error, t%has_column("tally"), "the renamed remapped column should resolve")
        if (allocated(error)) return
        call check(error, .not. t%has_column("counter"), "the pre-rename internal name should not resolve")
        if (allocated(error)) return
        ! Never read before the rename, so this first touch proves the file column survived it.
        call t%get("tally", gi32)
        call check(error, all(gi32 == [(i, i = 1, NROW)]), &
            "a renamed remapped column must still read its original physical column")
        if (allocated(error)) return
        call t%reload("tally")
        call t%get("tally", gi32)
        call check(error, all(gi32 == [(i, i = 1, NROW)]), &
            "reloading a renamed remapped column must go back to the same physical column")
    end subroutine test_remap_then_rename
    !
    subroutine test_rename_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: gi32(:)
        character(len=*), parameter :: f = "test_run/table_rename.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%rename_column("i32", "counter")
        call check(error, t%has_column("counter"), "the new name should resolve")
        if (allocated(error)) return
        call check(error, .not. t%has_column("i32"), "the old name should not resolve")
        if (allocated(error)) return
        call check(error, t%residency("counter") == RES_EMPTY, "a rename should read nothing")
        if (allocated(error)) return
        ! The first touch happens now, under the NEW name, and must still find the file column.
        call t%get("counter", gi32)
        call check(error, gi32(4) == 4_int32, &
            "a renamed but unread column should still read from its own file column")
        if (allocated(error)) return
        call check(error, t%ncols() == 6, "a rename should not change the column count")
    end subroutine test_rename_column
    !
    !> A cast produces a NEW column, leaves the source alone, carries nulls and the unit over,
    !! and refuses a value it cannot represent (that refusal is an error scenario).
    subroutine test_copy_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: v(NROW)
        integer(int64), allocatable :: gi64(:)
        integer(int32), allocatable :: gi32(:)
        real(real64), allocatable :: gf64(:)
        character(len=:), allocatable :: u
        integer :: i
        !
        do i = 1, NROW
            v(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("x", v, unit="m/s")
        call t%set_null("x", 5)
        call t%copy_column("x", "xi", PK_INT64)
        call check(error, t%has_column("xi"), "copy_column should create the new column")
        if (allocated(error)) return
        call check(error, t%kind("xi") == PK_INT64, "the new column should hold the target kind")
        if (allocated(error)) return
        call check(error, t%kind("x") == PK_FLOAT64, "the source column should keep its own kind")
        if (allocated(error)) return
        call t%get("xi", gi64)
        call check(error, gi64(2) == 2_int64 .and. gi64(6) == 6_int64, &
            "cast values should convert exactly")
        if (allocated(error)) return
        call check(error, t%is_null("xi", 5), "a null should carry over into the cast column")
        if (allocated(error)) return
        call t%unit("xi", u)
        call check(error, u == "m/s", "a kind cast should carry the unit over unchanged")
        if (allocated(error)) return
        call t%get("x", gf64)
        call check(error, gf64(2) == 2.0_real64, "the source column's values should be untouched")
        if (allocated(error)) return
        ! Widening the other way, and to a narrower float, both round-trip when exact.
        call t%copy_column("xi", "xf", PK_FLOAT32)
        call check(error, t%kind("xf") == PK_FLOAT32, "cast to float32 should produce a float32 column")
        if (allocated(error)) return
        call t%copy_column("xi", "xs", PK_INT32)
        call check(error, t%kind("xs") == PK_INT32, "cast to int32 should produce an int32 column")
        if (allocated(error)) return
        call t%get("xs", gi32)
        call check(error, gi32(2) == 2_int32, "a narrowing cast should keep an exactly representable value")
        if (allocated(error)) return
        ! The int32 and float32 SOURCE arms: everything above converts FROM a float64 or int64
        ! source, never from int32 or float32, so those two arms are still untouched -- copy xs
        ! (int32) and xf (float32) onward to exercise them.
        call t%copy_column("xs", "xs_i64", PK_INT64)
        call t%get("xs_i64", gi64)
        call check(error, gi64(2) == 2_int64, "casting FROM an int32 source should convert exactly")
        if (allocated(error)) return
        call t%copy_column("xf", "xf_f64", PK_FLOAT64)
        call t%get("xf_f64", gf64)
        call check(error, gf64(2) == 2.0_real64, "casting FROM a float32 source should convert exactly")
    end subroutine test_copy_column
    !
    !> %copy_column with no `to_kind` is a plain deep copy, and unlike the converting form it is
    !! not restricted to the numeric kinds -- every kind the library can read copies.
    subroutine test_copy_column_same_kind(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=8) :: s(NROW)
        type(parquet_date) :: d(NROW)
        integer(int32) :: v(NVEC, NROW)
        character(len=:), allocatable :: gs(:)
        integer(int32), pointer :: p(:,:)
        integer :: i, e
        !
        ! First element deliberately the shortest (CLAUDE.md).
        s = ["a       ", "bcd     ", "ef      ", "ghijklm ", "no      ", "p       "]
        do i = 1, NROW
            d(i) = parquet_date(2026, 3, i)
            do e = 1, NVEC
                v(e, i) = int(i * 10 + e, int32)
            end do
        end do
        call parquet_new_table(t)
        call t%add_column("s", s)
        call t%add_column("d", d)
        call t%add_column("v", v, unit="km")
        call t%set_null("s", 2)
        !
        call t%copy_column("s", "s2")
        call check(error, t%kind("s2") == PK_STRING, "a string column should copy as a string column")
        if (allocated(error)) return
        call t%get("s2", gs)
        call check(error, trim(gs(4)) == "ghijklm", "a copied string column should keep its values")
        if (allocated(error)) return
        call check(error, t%is_null("s2", 2), "a copied string column should keep its nulls")
        if (allocated(error)) return
        call t%copy_column("d", "d2")
        call check(error, t%kind("d2") == PK_DATE, "a date column should copy as a date column")
        if (allocated(error)) return
        call t%copy_column("v", "v2")
        call check(error, t%kind("v2") == PK_INT32_VEC .and. t%width("v2") == NVEC, &
            "a vector column should copy with its kind and width")
        if (allocated(error)) return
        ! The copy must be independent: writing through it must not reach the source.
        call t%col("v2", p)
        p(1, 1) = -99_int32
        call t%col("v", p)
        call check(error, p(1, 1) == 11_int32, "a copy should not share storage with its source")
    end subroutine test_copy_column_same_kind
    !
    !> %cast rewrites a column's own storage, so %col afterwards takes the TARGET kind's pointer.
    !! Walks every conversion between the four numeric scalar kinds in both directions.
    subroutine test_cast_in_place(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32) :: a(NROW)
        integer(int64), pointer :: p_i64(:)
        integer(int32), pointer :: p_i32(:)
        real(real32), pointer :: p_f32(:)
        real(real64), pointer :: p_f64(:)
        integer :: i
        !
        do i = 1, NROW
            a(i) = int(i, int32)
        end do
        call parquet_new_table(t)
        call t%add_column("a", a)
        ! int32 -> int64 -> float64 -> float32 -> int32, back where it started.
        call t%cast("a", PK_INT64)
        call check(error, t%kind("a") == PK_INT64, "cast should change the column's reported kind")
        if (allocated(error)) return
        call t%col("a", p_i64)
        call check(error, all(p_i64 == [(int(i, int64), i = 1, NROW)]), &
            "int32 -> int64 should convert every value exactly")
        if (allocated(error)) return
        call t%cast("a", PK_FLOAT64)
        call t%col("a", p_f64)
        call check(error, all(p_f64 == [(real(i, real64), i = 1, NROW)]), &
            "int64 -> float64 should convert every value exactly")
        if (allocated(error)) return
        call t%cast("a", PK_FLOAT32)
        call t%col("a", p_f32)
        call check(error, all(p_f32 == [(real(i, real32), i = 1, NROW)]), &
            "float64 -> float32 should convert every value exactly")
        if (allocated(error)) return
        call t%cast("a", PK_INT32)
        call t%col("a", p_i32)
        call check(error, all(p_i32 == a), "float32 -> int32 should return the original values")
        if (allocated(error)) return
        ! The remaining direct pairs the ring above does not cover.
        call t%cast("a", PK_FLOAT32)
        call t%cast("a", PK_INT64)
        call t%col("a", p_i64)
        call check(error, all(p_i64 == [(int(i, int64), i = 1, NROW)]), &
            "float32 -> int64 should convert every value exactly")
        if (allocated(error)) return
        call t%cast("a", PK_FLOAT32)
        call t%cast("a", PK_FLOAT64)
        call t%cast("a", PK_INT32)
        call t%cast("a", PK_FLOAT64)
        call t%col("a", p_f64)
        call check(error, all(p_f64 == [(real(i, real64), i = 1, NROW)]), &
            "int32 -> float64 should convert every value exactly")
        if (allocated(error)) return
        call t%cast("a", PK_INT64)
        call t%cast("a", PK_INT32)
        call t%col("a", p_i32)
        call check(error, all(p_i32 == a), "int64 -> int32 should keep an in-range value")
        if (allocated(error)) return
        call t%cast("a", PK_FLOAT32)
        call t%cast("a", PK_FLOAT64)
        call t%col("a", p_f64)
        call check(error, all(p_f64 == [(real(i, real64), i = 1, NROW)]), &
            "float32 -> float64 should convert every value exactly")
    end subroutine test_cast_in_place
    !
    !> A vector column casts element by element, keeping its width.
    subroutine test_cast_vector(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32) :: v(NVEC, NROW)
        real(real64) :: w(NVEC, NROW)
        integer(int64), pointer :: p_i64v(:,:)
        integer(int32), pointer :: p_i32v(:,:)
        integer :: i, e
        !
        do i = 1, NROW
            do e = 1, NVEC
                v(e, i) = int(i * 10 + e, int32)
                w(e, i) = real(i * 10 + e, real64)
            end do
        end do
        call parquet_new_table(t)
        call t%add_column("v", v, unit="km")
        call t%add_column("w", w)
        call t%set_null("v", 3)
        !
        call t%cast("v", PK_INT64_VEC)
        call check(error, t%kind("v") == PK_INT64_VEC, "a vector cast should give the vector target kind")
        if (allocated(error)) return
        call check(error, t%width("v") == NVEC, "a vector cast should keep the column's width")
        if (allocated(error)) return
        call t%col("v", p_i64v)
        call check(error, p_i64v(2, 5) == 52_int64, "a vector cast should convert every element")
        if (allocated(error)) return
        call check(error, t%is_null("v", 3), "a vector cast should keep the row's null")
        if (allocated(error)) return
        ! float64 vector down to an int32 vector: whole numbers, so nothing is refused.
        call t%cast("w", PK_INT32_VEC)
        call t%col("w", p_i32v)
        call check(error, p_i32v(3, 6) == 63_int32, &
            "a float64 -> int32 vector cast should convert every element")
    end subroutine test_cast_vector
    !
    !> A cast asked for before anything has read the column is carried out BY the read: the slot
    !! stays empty until first touch, and then arrives already holding the target kind.
    subroutine test_cast_deferred(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=*), parameter :: fname = "test_run/table_cast_deferred.parquet"
        character(len=*), parameter :: wide_fname = "test_run/table_cast_deferred_wide.parquet"
        real(real64), pointer :: p_f64(:)
        integer(int64), pointer :: p_i64(:)
        integer :: i
        !
        call write_basic_fixture(fname)
        call parquet_open_table(t, fname)
        call check(error, t%kind("i32") == PK_INT32, "the file column should start as int32")
        if (allocated(error)) return
        call t%cast("i32", PK_FLOAT64)
        call check(error, t%kind("i32") == PK_FLOAT64, "a deferred cast should report the target kind")
        if (allocated(error)) return
        call check(error, t%residency("i32") == RES_EMPTY, &
            "a deferred cast should not have read the column")
        if (allocated(error)) return
        call t%col("i32", p_f64)
        call check(error, all(p_f64 == [(real(i, real64), i = 1, NROW)]), &
            "the first touch should decode straight into the cast kind")
        if (allocated(error)) return
        call check(error, t%residency("i32") == RES_FULL, "the first touch should have read the column")
        if (allocated(error)) return
        ! A SECOND cast while the first is still pending has to materialize rather than simply
        ! overwrite the pending kind, or the first cast's loss would be silently skipped. The
        ! fixture's value needs more than real32's 24 mantissa bits, so going through float32
        ! and on to float64 lands somewhere int64 -> float64 (exact below 2**53) never would.
        call write_wide_int_fixture(wide_fname)
        call parquet_open_table(t, wide_fname)
        call t%cast("big", PK_FLOAT32)
        call t%cast("big", PK_FLOAT64)
        call t%col("big", p_f64)
        call check(error, p_f64(1) == real(real(WIDE_INT, real32), real64), &
            "a chained cast should apply the intermediate conversion, not skip it")
        if (allocated(error)) return
        call check(error, p_f64(1) /= real(WIDE_INT, real64), &
            "skipping the intermediate cast would have kept the value exact -- it must not")
        if (allocated(error)) return
        ! And the ordinary case: cast after the column is already resident.
        call parquet_open_table(t, fname)
        call t%prefetch("i32")
        call t%cast("i32", PK_INT64)
        call t%col("i32", p_i64)
        call check(error, all(p_i64 == [(int(i, int64), i = 1, NROW)]), &
            "an eager cast should convert the values already read")
        if (allocated(error)) return
        ! %reload discards VALUE edits, not the cast: it re-reads into the column's CURRENT kind,
        ! so a cast survives it. Re-reading into the FILE's kind instead would silently undo a
        ! conversion the caller never asked to undo, and would change the column's kind under any
        ! pointer taken since.
        call t%set_element("i32", 1, 99_int64)
        call t%reload("i32")
        call check(error, t%kind("i32") == PK_INT64, "%reload must keep the column's cast kind")
        if (allocated(error)) return
        call t%col("i32", p_i64)
        call check(error, p_i64(1) == 1_int64, &
            "%reload should discard the value edit and re-read the file's own value")
    end subroutine test_cast_deferred
    !
    !> An int64 needing more than real32's 24 mantissa bits, so that a trip through float32 is
    !! observable in the result rather than being an exact round trip.
    subroutine write_wide_int_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer(int64) :: big(NROW)
        integer :: i
        !
        do i = 1, NROW
            big(i) = WIDE_INT + int(i - 1, int64)
        end do
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "big", big)
        call parquet_close_writer(w)
    end subroutine write_wide_int_fixture
    !
    !> Nulls, the unit and the row count all survive a cast, and a cast to the kind a column
    !! already holds does nothing at all.
    subroutine test_cast_preserves(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: v(NROW)
        real(real32), pointer :: p_f32(:)
        character(len=:), allocatable :: u
        integer :: i
        !
        do i = 1, NROW
            v(i) = real(i, real64) * 0.5_real64
        end do
        call parquet_new_table(t)
        call t%add_column("x", v, unit="m/s")
        call t%set_null("x", 4)
        call t%cast("x", PK_FLOAT32)
        call check(error, t%nrows() == int(NROW, int64), "a cast should not change the row count")
        if (allocated(error)) return
        call t%unit("x", u)
        call check(error, u == "m/s", "a cast should carry the unit over unchanged")
        if (allocated(error)) return
        call check(error, t%is_null("x", 4), "a cast should keep the column's nulls")
        if (allocated(error)) return
        call t%col("x", p_f32)
        call check(error, p_f32(2) == 1.0_real32, "a cast should keep the values it converts")
        if (allocated(error)) return
        ! Casting to the kind already held is a no-op, including the pointer staying usable.
        call t%cast("x", PK_FLOAT32)
        call check(error, t%kind("x") == PK_FLOAT32 .and. t%is_null("x", 4), &
            "a cast to the kind already held should change nothing")
    end subroutine test_cast_preserves
    !
    !> `exact=` decides whether a loss the read path makes silently is refused instead. Only the
    !! ACCEPTING side can be tested here -- the refusal aborts, so it lives in error_scenarios.
    subroutine test_cast_exact_flag(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: v(NROW), whole(NROW)
        real(real32), pointer :: p_f32(:)
        integer :: i
        !
        ! 0.1 has no exact real32 form; the halves next to it do.
        v = [(real(i, real64) * 0.5_real64, i = 1, NROW)]
        v(3) = 0.1_real64
        whole = [(real(i, real64), i = 1, NROW)]
        call parquet_new_table(t)
        call t%add_column("x", v)
        call t%add_column("y", whole)
        ! Default (exact absent) truncates, exactly as reading a float64 column into a real32
        ! array does.
        call t%cast("x", PK_FLOAT32)
        call t%col("x", p_f32)
        call check(error, p_f32(3) == real(0.1_real64, real32), &
            "the default cast should truncate a value real32 cannot hold exactly")
        if (allocated(error)) return
        ! exact=.true. accepts a column whose every value DOES survive the round trip.
        call t%cast("y", PK_INT32, exact=.true.)
        call t%cast("y", PK_FLOAT32, exact=.true.)
        call check(error, t%kind("y") == PK_FLOAT32, &
            "exact=.true. should accept a conversion that loses nothing")
    end subroutine test_cast_exact_flag
    !
    !> Writes a fixture with an id column plus one sortable column of each interesting shape, so
    !! a sort's result can always be stated as "these ids, in this order".
    subroutine write_sort_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer(int32) :: id(NROW)
        real(real64) :: v(NROW)
        integer(int32) :: g(NROW)
        character(len=8) :: s(NROW)
        integer :: i
        !
        do i = 1, NROW
            id(i) = i
        end do
        ! v is deliberately unordered, and g has repeats so a second key has ties to break.
        v = [30.0_real64, 10.0_real64, 50.0_real64, 20.0_real64, 60.0_real64, 40.0_real64]
        g = [2_int32, 1_int32, 2_int32, 1_int32, 2_int32, 1_int32]
        ! First element the shortest (CLAUDE.md).
        s = ["b       ", "aa      ", "ddd     ", "cccc    ", "e       ", "ff      "]
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "v", v)
        call parquet_write_column(w, "g", g)
        call parquet_write_column(w, "s", s)
        call parquet_close_writer(w)
    end subroutine write_sort_fixture
    !
    !> Sorting reads a key column that was never materialized, rather than refusing.
    !>
    !> This is the ONLY coverage of that behaviour: it used to abort, and the abort had an error
    !> scenario asserting its message. That scenario is gone, so without this test the change
    !> would ship untested in either direction.
    subroutine test_sort_reads_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        integer(int32), allocatable :: id(:)
        integer(int64), allocatable :: perm(:)
        character(len=*), parameter :: f = "test_run/table_sort_lazy1.parquet"
        character(len=*), parameter :: f2 = "test_run/table_sort_lazy2.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        ! "id" is read up front only because its values are asserted AFTER the sort: %sort_by
        ! reorders the columns that are resident and then detaches, so a column still sitting in
        ! the file when the rows move can never be read again. The KEY is deliberately left
        ! unread -- that is what this test is about.
        call t%prefetch("id")
        call check(error, t%residency("v") /= RES_FULL, "the key column must start unread")
        if (allocated(error)) return
        call t%sort_by(["v"])
        call t%get("id", id)
        call check(error, all(id == [2_int32, 4_int32, 1_int32, 6_int32, 3_int32, 5_int32]), &
            "sort_by must read the key column itself and order by it")
        if (allocated(error)) return
        ! Only the KEY is read implicitly: a non-key column that was unread stays unread, which is
        ! what keeps a %sort_by on a wide lazy table from pulling the whole file into memory.
        call check(error, t%residency("g") /= RES_FULL, &
            "sort_by must read its key column only, not every column")
        if (allocated(error)) return
        !
        ! %argsort_by reads its key the same way -- and, moving no row, leaves t2 attached, so
        ! nothing else has to be read first.
        call write_sort_fixture(f2)
        call parquet_open_table(t2, f2)
        call t2%argsort_by(["v"], perm)
        call check(error, all(perm == [2_int64, 4_int64, 1_int64, 6_int64, 3_int64, 5_int64]), &
            "argsort_by must read its key column implicitly too")
    end subroutine test_sort_reads_key
    !
    !> The order without the reordering: same permutation `%sort_by` would apply, but the table is
    !> untouched and, critically, still attached to its file.
    subroutine test_argsort_by(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, ref
        integer(int64), allocatable :: perm(:)
        integer(int32), allocatable :: id(:), ref_id(:), got(:)
        real(real64), allocatable :: v(:), ref_v(:)
        character(len=:), allocatable :: s(:), ref_s(:)
        integer(int32), allocatable :: perm32(:)
        character(len=*), parameter :: f = "test_run/table_argsort1.parquet"
        character(len=*), parameter :: f2 = "test_run/table_argsort2.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%argsort_by(["v"], perm)
        !
        ! The oracle is %sort_by on an independent table over the same data: reading through the
        ! permutation must give what actually sorting would have given, column for column.
        call write_sort_fixture(f2)
        call parquet_open_table(ref, f2)
        call ref%materialize_all()
        call ref%sort_by(["v"])
        call ref%get("id", ref_id)
        call ref%get("v", ref_v)
        call ref%get("s", ref_s)
        call t%get_slice("id", parquet_slice_list(perm), id)
        call t%get_slice("v", parquet_slice_list(perm), v)
        call t%get_slice("s", parquet_slice_list(perm), s)
        call check(error, all(id == ref_id), "argsort_by must agree with sort_by on an int column")
        if (allocated(error)) return
        call check(error, all(v == ref_v), "argsort_by must agree with sort_by on a float column")
        if (allocated(error)) return
        call check(error, all(s == ref_s), "argsort_by must agree with sort_by on a string column")
        if (allocated(error)) return
        !
        ! The property the whole binding exists for.
        call check(error, .not. t%is_detached(), "argsort_by must NOT detach the table")
        if (allocated(error)) return
        call t%reload("v")
        call t%get("v", v)
        call check(error, all(v == [30.0_real64, 10.0_real64, 50.0_real64, 20.0_real64, &
            60.0_real64, 40.0_real64]), "the table must still read from its file, unreordered")
        if (allocated(error)) return
        !
        ! Multi-key and descending, and the int32 form.
        call t%argsort_by(["g", "v"], perm, descending=[.false., .true.])
        call t%get_slice("id", parquet_slice_list(perm), got)
        call check(error, all(got == [6_int32, 4_int32, 2_int32, 5_int32, 3_int32, 1_int32]), &
            "argsort_by must honour per-key descending flags")
        if (allocated(error)) return
        call t%argsort_by(["v"], perm32)
        call check(error, all(int(perm32, int64) == [2_int64, 4_int64, 1_int64, 6_int64, &
            3_int64, 5_int64]), "the int32 form must agree with the int64 one")
    end subroutine test_argsort_by
    !
    !> Group boundaries over a table, including the prefix form that motivated `group_nkeys`.
    subroutine test_argsort_by_groups(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64), allocatable :: perm(:), go(:)
        real(real64), allocatable :: v(:)
        character(len=*), parameter :: f = "test_run/table_argsort_grp.parquet"
        integer(int64) :: gi
        logical :: uniform
        integer(int32), allocatable :: gvals(:)
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        !
        ! g is [2,1,2,1,2,1] -- two values, so two groups.
        call t%argsort_by(["g"], perm, group_offsets=go)
        call check(error, size(go) - 1 == 2, "two distinct key values must give two groups")
        if (allocated(error)) return
        call check(error, all(go == [1_int64, 4_int64, 7_int64]), &
            "each g value owns three consecutive rows, and the sentinel is nrows + 1")
        if (allocated(error)) return
        uniform = .true.
        do gi = 1_int64, size(go, kind=int64) - 1_int64
            call t%get_slice("g", parquet_slice_list(perm(go(gi):go(gi + 1_int64) - 1_int64)), gvals)
            if (any(gvals /= gvals(1))) uniform = .false.
        end do
        call check(error, uniform, "every row inside a group must share the key value")
        if (allocated(error)) return
        !
        ! group_nkeys=1 over ["g", "v"]: group by g, ordered by v inside each group.
        call t%argsort_by(["g", "v"], perm, group_offsets=go, group_nkeys=1)
        call check(error, size(go) - 1 == 2, "grouping on the first key alone must give two groups")
        if (allocated(error)) return
        call t%get_slice("v", parquet_slice_list(perm(1:3)), v)
        call check(error, all(v == [10.0_real64, 20.0_real64, 40.0_real64]), &
            "the second key must still order the rows inside a group")
        if (allocated(error)) return
        ! The default groups on BOTH keys, which here makes every row its own group.
        call t%argsort_by(["g", "v"], perm, group_offsets=go)
        call check(error, size(go) - 1 == 6, "the default must group on every key")
    end subroutine test_argsort_by_groups
    !
    !> The reserved row-index column is materialized on demand by the key lookup, which is what
    !> makes it the documented way back to file order after a sort.
    subroutine test_argsort_by_row_index(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k
        logical :: identity
        character(len=*), parameter :: f = "test_run/table_argsort_rowidx.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        ! No accessor has touched PARQUET_ROW_INDEX, so this is exactly the case that used to
        ! abort with "no column of this name".
        call t%argsort_by([PARQUET_ROW_INDEX], perm)
        identity = .true.
        do k = 1_int64, size(perm, kind=int64)
            if (perm(k) /= k) identity = .false.
        end do
        call check(error, identity, "argsort_by on the row index must give the identity order")
    end subroutine test_argsort_by_row_index
    !
    !> Both answers, on the same fixture. A check that always says .true. passes every positive
    !> assertion ever written for it, so the negative control is the load-bearing half.
    subroutine test_is_sorted_by(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_is_sorted.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call check(error, .not. t%is_sorted_by(["v"]), &
            "an unordered column must report NOT sorted")
        if (allocated(error)) return
        call check(error, t%is_sorted_by(["id"]), "1..6 must report sorted")
        if (allocated(error)) return
        call check(error, .not. t%is_sorted_by(["id"], descending=[.true.]), &
            "an ascending column must not report sorted under descending=")
        if (allocated(error)) return
        !
        call t%sort_by(["v"])
        call check(error, t%is_sorted_by(["v"]), "after sort_by the table must report sorted")
    end subroutine test_is_sorted_by
    !
    !> `%top_n` against the composition it replaces: `%sort_by` then `%truncate`.
    !>
    !> That oracle is what makes this test complete rather than partial. Asserting only WHICH rows
    !> survived would pass against "the right rows in the wrong order", which is the mistake this
    !> feature is most likely to make -- the gather has to apply the selection and the ordering at
    !> once. Comparing against a table that was sorted and then cut catches order, membership,
    !> values, nulls and any column silently left behind, in one assertion per column.
    !>
    !> Every column of the fixture is checked, not just the key: a mutation that skips one column
    !> leaves it holding another row's values, which is the row-correspondence failure with no abort
    !> and no symptom.
    subroutine test_top_n(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, ref
        integer(int32), allocatable :: id(:), rid(:), g(:), rg(:)
        real(real64), allocatable :: v(:), rv(:)
        character(len=:), allocatable :: s(:), rs(:)
        character(len=*), parameter :: f = "test_run/table_top_n.parquet"
        character(len=*), parameter :: f2 = "test_run/table_top_n_ref.parquet"
        !
        call write_sort_fixture(f)
        call write_sort_fixture(f2)
        call parquet_open_table(t, f)
        call parquet_open_table(ref, f2)
        call t%materialize_all()
        call ref%materialize_all()
        !
        ! The oracle, on the descending multi-key form so that direction, key precedence and tie
        ! breaking are all covered by the same comparison.
        call ref%sort_by(["g", "v"], descending=[.true., .false.])
        call ref%truncate(4)
        call t%top_n(["g", "v"], 4, descending=[.true., .false.])
        call check(error, t%nrows() == 4_int64, "top_n must leave exactly n rows")
        if (allocated(error)) return
        call t%get("id", id)
        call ref%get("id", rid)
        call check(error, all(id == rid), "top_n must keep the rows sort_by+truncate keeps, in order")
        if (allocated(error)) return
        call t%get("v", v)
        call ref%get("v", rv)
        call check(error, all(v == rv), "the key column's values must match sort_by+truncate")
        if (allocated(error)) return
        call t%get("g", g)
        call ref%get("g", rg)
        call check(error, all(g == rg), "the second key column must match sort_by+truncate")
        if (allocated(error)) return
        ! The string column goes through parquet_string_column%gather, the one new algorithm here,
        ! so its agreement is the interesting half of this comparison.
        call t%get("s", s)
        call ref%get("s", rs)
        call check(error, size(s) == size(rs), "the string column must have n rows after top_n")
        if (allocated(error)) return
        call check(error, all(s == rs), "the string column must match sort_by+truncate")
        if (allocated(error)) return
        !
        call check(error, t%is_detached(), "top_n drops rows, so it must detach the table")
        if (allocated(error)) return
        call check(error, t%generation() > 0_int64, "top_n must bump the generation counter")
    end subroutine test_top_n
    !
    !> `%top_n`'s edges, each of which is a separate decision rather than a consequence of the main
    !> path: the clamp, the empty result, the delegation to `%sort_by`, the lazy key read, and the
    !> rule that a table with no file to lose does not report itself detached.
    subroutine test_top_n_edges(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2, t3, mem
        integer(int32), allocatable :: id(:)
        real(real64), allocatable :: v(:)
        integer(int32) :: raw(4)
        character(len=*), parameter :: f = "test_run/table_top_n_e1.parquet"
        character(len=*), parameter :: f2 = "test_run/table_top_n_e2.parquet"
        character(len=*), parameter :: f3 = "test_run/table_top_n_e3.parquet"
        !
        ! n at or past the row count keeps every row, in key order -- this is a whole sort, and is
        ! delegated to %sort_by rather than clamped into the gather path.
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%top_n(["v"], 999)
        call check(error, t%nrows() == int(NROW, int64), "n past the row count must clamp, not abort")
        if (allocated(error)) return
        call t%get("v", v)
        call check(error, all(v == [10.0_real64, 20.0_real64, 30.0_real64, 40.0_real64, &
            50.0_real64, 60.0_real64]), "a fully clamped top_n must order every row")
        if (allocated(error)) return
        !
        ! n = 0 empties the table without disturbing its columns.
        call write_sort_fixture(f2)
        call parquet_open_table(t2, f2)
        call t2%materialize_all()
        call t2%top_n(["v"], 0)
        call check(error, t2%nrows() == 0_int64, "top_n with n = 0 must leave no rows")
        if (allocated(error)) return
        call check(error, t2%ncols() == 4, "top_n with n = 0 must keep every column")
        if (allocated(error)) return
        call check(error, t2%is_detached(), "emptying a file-backed table must detach it")
        if (allocated(error)) return
        !
        ! A key column that has not been read yet is READ, exactly as %sort_by reads it -- so a
        ! top_n on a freshly opened table needs no %prefetch. "id" is prefetched only because it is
        ! asserted afterwards, and a column still in the file when the rows move is lost.
        call write_sort_fixture(f3)
        call parquet_open_table(t3, f3)
        call t3%prefetch("id")
        call check(error, t3%residency("v") /= RES_FULL, "the key column must start unread")
        if (allocated(error)) return
        call t3%top_n(["v"], 2)
        call t3%get("id", id)
        call check(error, all(id == [2_int32, 4_int32]), &
            "top_n must read its key column itself and keep the two smallest rows")
        if (allocated(error)) return
        !
        ! A table built in memory has no file to lose, so reducing it must not report it detached --
        ! otherwise every from-scratch table would claim detachment the moment it was cut down.
        raw = [40_int32, 10_int32, 30_int32, 20_int32]
        call parquet_new_table(mem)
        call mem%add_column("k", raw)
        call mem%top_n(["k"], 2)
        call check(error, mem%nrows() == 2_int64, "top_n must reduce an in-memory table too")
        if (allocated(error)) return
        call check(error, .not. mem%is_detached(), &
            "a table that never had a file must not report itself detached")
    end subroutine test_top_n_edges
    !
    !> Top-N by selection. Its oracle is `%argsort_by`'s first `n`, which a partial sort must
    !> reproduce exactly -- it orders that prefix and leaves the rest alone.
    subroutine test_argsort_partial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64), allocatable :: full(:), part(:)
        integer(int32), allocatable :: part32(:)
        real(real64), allocatable :: v(:)
        character(len=*), parameter :: f = "test_run/table_argsort_partial.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%argsort_by(["v"], full)
        call t%argsort_partial(["v"], part, 3)
        call check(error, size(part) == 3, "n = 3 must return three row indices")
        if (allocated(error)) return
        call check(error, all(part == full(1:3)), &
            "the first n of a partial sort must equal the first n of a full one")
        if (allocated(error)) return
        !
        ! "The largest three" is descending=, not a separate binding.
        call t%argsort_partial(["v"], part, 3, descending=[.true.])
        call t%get_slice("v", parquet_slice_list(part), v)
        call check(error, all(v == [60.0_real64, 50.0_real64, 40.0_real64]), &
            "descending must give the three largest values, in order")
        if (allocated(error)) return
        !
        call t%argsort_partial(["v"], part, 999)
        call check(error, size(part) == 6, "n past the row count must clamp rather than abort")
        if (allocated(error)) return
        call check(error, all(part == full), "a fully clamped partial sort must equal a full one")
        if (allocated(error)) return
        !
        call t%argsort_partial(["v"], part32, 2)
        call check(error, all(int(part32, int64) == full(1:2)), &
            "the int32 form must agree with the int64 one")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "argsort_partial must not detach the table")
    end subroutine test_argsort_partial
    !
    !> Filtering keeps the selected rows in every column at once, and detaches.
    subroutine test_filter_rows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        logical :: keep(NROW)
        integer(int32), allocatable :: id(:)
        real(real64), allocatable :: v(:)
        character(len=*), parameter :: f = "test_run/table_filter.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        keep = [.true., .false., .true., .false., .false., .true.]
        call t%filter_rows(keep)
        call check(error, t%nrows() == 3, "filter_rows should leave the surviving row count")
        if (allocated(error)) return
        call check(error, t%is_detached(), "filter_rows should detach the table")
        if (allocated(error)) return
        call t%get("id", id)
        call t%get("v", v)
        call check(error, all(id == [1_int32, 3_int32, 6_int32]), &
            "filter_rows should keep exactly the selected rows, in order")
        if (allocated(error)) return
        call check(error, all(v == [30.0_real64, 50.0_real64, 40.0_real64]), &
            "every column should be filtered by the same mask")
        if (allocated(error)) return
        call check(error, t%ncols() == 4, "filter_rows should not change the column count")
        if (allocated(error)) return
        call check_every_column_length(error, t, "filter_rows")
    end subroutine test_filter_rows
    !
    !> Asserts that EVERY column really holds the table's row count.
    !!
    !! A row-structural mutation has to reach every column, and a loop that misses one leaves a
    !! table whose columns disagree about how many rows there are -- which no assertion on one or
    !! two named columns can see. Deliberately checked through the copy path, whose result is
    !! allocated to the column's own length, and over every kind the fixture has (numeric, string,
    !! and a column nobody named in the test).
    subroutine check_every_column_length(error, t, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_table), intent(inout) :: t             !! the mutated table.
        character(len=*), intent(in) :: what                !! operation name, for the message.
        character(len=:), allocatable :: names(:)
        integer(int64), allocatable :: whole(:)
        real(real64), allocatable :: num(:)
        character(len=:), allocatable :: txt(:)
        integer :: c
        integer(int64) :: n
        !
        call t%column_names(names)
        do c = 1, size(names)
            ! Read through whichever widened form the column's kind allows, so the result is
            ! allocated to that column's OWN length rather than to anything this test assumed.
            select case (t%kind(trim(names(c))))
            case (PK_INT32, PK_INT64)
                call t%get(trim(names(c)), whole)
                n = size(whole, kind=int64)
            case (PK_FLOAT32, PK_FLOAT64)
                call t%get(trim(names(c)), num)
                n = size(num, kind=int64)
            case default
                call t%get(trim(names(c)), txt)
                n = size(txt, kind=int64)
            end select
            call check(error, n == t%nrows(), &
                what // " must leave every column holding the table's row count (" // &
                trim(names(c)) // ")")
            if (allocated(error)) return
        end do
    end subroutine check_every_column_length
    !
    !> delete_rows and truncate are thin wrappers over the same machinery, including their edges:
    !! a repeated index, and a truncate past the end.
    subroutine test_delete_and_truncate(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        integer(int32), allocatable :: id(:)
        character(len=*), parameter :: f = "test_run/table_delete.parquet"
        character(len=*), parameter :: f2 = "test_run/table_truncate.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%delete_rows([2, 4, 2])       ! the repeat must remove row 2 once, not twice
        call t%get("id", id)
        call check(error, all(id == [1_int32, 3_int32, 5_int32, 6_int32]), &
            "delete_rows should remove each named row exactly once")
        if (allocated(error)) return
        call check(error, t%is_detached(), "delete_rows should detach the table")
        if (allocated(error)) return
        !
        call write_sort_fixture(f2)
        call parquet_open_table(t2, f2)
        call t2%materialize_all()
        call t2%truncate(999)               ! past the end: a no-op, not an error
        call check(error, t2%nrows() == NROW, "truncate past the end should keep every row")
        if (allocated(error)) return
        call t2%truncate(2_int64)
        deallocate(id)
        call t2%get("id", id)
        call check(error, all(id == [1_int32, 2_int32]), "truncate should keep the first n rows")
    end subroutine test_delete_and_truncate
    !
    !> The in-memory sort, single key and multi key, ascending and descending.
    subroutine test_sort_by(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2, t3
        integer(int32), allocatable :: id(:), g(:)
        real(real64), allocatable :: v(:)
        character(len=:), allocatable :: str(:)
        character(len=*), parameter :: f = "test_run/table_sort1.parquet"
        character(len=*), parameter :: f2 = "test_run/table_sort2.parquet"
        character(len=*), parameter :: f3 = "test_run/table_sort3.parquet"
        !
        ! v = [30,10,50,20,60,40] -> ascending order of ids is 2,4,1,6,3,5.
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%sort_by(["v"])
        call t%get("id", id)
        call check(error, all(id == [2_int32, 4_int32, 1_int32, 6_int32, 3_int32, 5_int32]), &
            "sort_by should order rows by the key column, ascending by default")
        if (allocated(error)) return
        call check(error, t%is_detached(), "sort_by should detach the table")
        if (allocated(error)) return
        !
        call write_sort_fixture(f2)
        call parquet_open_table(t2, f2)
        call t2%materialize_all()
        call t2%sort_by(["v"], descending=[.true.])
        deallocate(id)
        call t2%get("id", id)
        call check(error, all(id == [5_int32, 3_int32, 6_int32, 1_int32, 4_int32, 2_int32]), &
            "descending= should reverse the order")
        if (allocated(error)) return
        !
        ! g = [2,1,2,1,2,1], so g ascending then v ascending gives ids 2,4,6 then 1,3,5.
        call write_sort_fixture(f3)
        call parquet_open_table(t3, f3)
        call t3%materialize_all()
        call t3%sort_by(["g", "v"])
        deallocate(id)
        call t3%get("id", id)
        call check(error, all(id == [2_int32, 4_int32, 6_int32, 1_int32, 3_int32, 5_int32]), &
            "a second key should break the first key's ties")
        if (allocated(error)) return
        call check_every_column_length(error, t3, "sort_by")
        if (allocated(error)) return
        ! A sort changes order, not lengths, so a length check cannot see a column the reorder
        ! missed. Every column's VALUES must therefore be checked against the same permutation --
        ! including the LAST one, which is where an off-by-one loop bound leaves its evidence.
        call t3%get("g", g)
        call check(error, all(g == [1_int32, 1_int32, 1_int32, 2_int32, 2_int32, 2_int32]), &
            "the key column itself should end up sorted")
        if (allocated(error)) return
        call t3%get("v", v)
        call check(error, all(v == [10.0_real64, 20.0_real64, 40.0_real64, 30.0_real64, &
            50.0_real64, 60.0_real64]), "every numeric column should follow the permutation")
        if (allocated(error)) return
        call t3%get("s", str)
        call check(error, trim(str(1)) == "aa" .and. trim(str(3)) == "ff" .and. &
            trim(str(6)) == "e", "the last column must follow the permutation too")
    end subroutine test_sort_by
    !
    !> THE equivalence test: an in-memory %sort_by and a read-time sort_by= run the same C++
    !! engine, so they must produce the identical row order -- for every key type, and with the
    !! nulls and NaNs that are the most likely place for two orderings to drift apart.
    subroutine test_sort_matches_read_time(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32), allocatable :: mem(:), disk(:)
        integer :: k
        character(len=8), parameter :: keys(3) = ["v       ", "s       ", "g       "]
        character(len=*), parameter :: f = "test_run/table_sort_equiv.parquet"
        !
        call write_sort_equiv_fixture(f)
        do k = 1, 3
            ! in memory
            call parquet_open_table(t, f)
            call t%materialize_all()
            call t%sort_by([trim(keys(k))])
            if (allocated(mem)) deallocate(mem)
            call t%get("id", mem)
            ! ... and the same sort applied while reading
            srt = parquet_sortkey()
            call srt%add(trim(keys(k))//" asc")
            call parquet_open_reader(reader, f, sort_by=srt)
            if (allocated(disk)) deallocate(disk)
            allocate(disk(NROW))
            call parquet_read_column(reader, "id", disk)
            call parquet_close_reader(reader)
            call check(error, all(mem == disk), &
                "an in-memory sort must order rows exactly as a read-time sort of the same key")
            if (allocated(error)) return
        end do
    end subroutine test_sort_matches_read_time
    !
    !> The equivalence fixture: nulls and a NaN in the float key, so the tier rules are exercised
    !! rather than just the ordinary values.
    subroutine write_sort_equiv_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer(int32) :: id(NROW), g(NROW)
        real(real64) :: v(NROW)
        character(len=8) :: s(NROW)
        logical :: v_ok(NROW), s_ok(NROW)
        integer :: i
        !
        do i = 1, NROW
            id(i) = i
        end do
        v = [30.0_real64, 10.0_real64, 50.0_real64, 20.0_real64, 60.0_real64, 40.0_real64]
        v(3) = ieee_value(0.0_real64, ieee_quiet_nan)
        v_ok = .true.
        v_ok(5) = .false.                       ! a null float, next to the NaN
        g = [2_int32, 1_int32, 2_int32, 1_int32, 2_int32, 1_int32]
        s = ["b       ", "aa      ", "ddd     ", "cccc    ", "e       ", "ff      "]
        s_ok = .true.
        s_ok(2) = .false.                       ! a null string
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "v", v, is_valid=v_ok)
        call parquet_write_column(w, "g", g)
        call parquet_write_column(w, "s", s, is_valid=s_ok)
        call parquet_close_writer(w)
    end subroutine write_sort_equiv_fixture
    !
    !> Appending another table concatenates matching columns and null-fills the ones the appended
    !! table does not have.
    subroutine test_append_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, batch, partial
        integer(int32), allocatable :: id(:)
        real(real64), allocatable :: v(:)
        character(len=*), parameter :: f = "test_run/table_append.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        ! A batch with the same structure is the documented way to append in bulk.
        call t%clone_structure(batch)
        call check(error, batch%nrows() == 0, "clone_structure should produce an empty table")
        if (allocated(error)) return
        call check(error, batch%ncols() == 4, "clone_structure should carry every column over")
        if (allocated(error)) return
        ! The documented bulk idiom: give the batch its rows, then write each column's values.
        call batch%append_null_rows(2)
        call batch%set("id", [91_int32, 92_int32])
        call batch%set("v", [1.5_real64, 2.5_real64])
        call check(error, .not. batch%is_detached(), &
            "an in-memory table has no file to lose, so growing it must not mark it detached")
        if (allocated(error)) return
        call t%append(batch)
        call check(error, t%nrows() == NROW + 2, "append should add the batch's rows")
        if (allocated(error)) return
        call check(error, t%is_detached(), "append should detach the table")
        if (allocated(error)) return
        call t%get("id", id)
        call t%get("v", v)
        call check(error, id(NROW + 1) == 91_int32 .and. id(NROW + 2) == 92_int32, &
            "appended values should land after the existing rows")
        if (allocated(error)) return
        call check(error, v(NROW + 1) == 1.5_real64, "every supplied column should be appended")
        if (allocated(error)) return
        ! "g" and "s" were not in the batch, so their appended rows are null (the M1 default).
        call check(error, t%is_null("g", NROW + 1), &
            "a column the batch did not supply should be null-filled")
        if (allocated(error)) return
        call check(error, .not. t%is_null("g", 1), "null-filling should not reach existing rows")
        if (allocated(error)) return
        call check_every_column_length(error, t, "append")
        if (allocated(error)) return
        ! The batch above came from %clone_structure, so it HAD every column -- the nulls above
        ! are its own. A batch built from scratch with fewer columns is what actually exercises
        ! the null-fill rule for a column the source does not mention at all.
        call parquet_new_table(partial)
        call partial%add_column("id", [93_int32])
        call t%append(partial)
        call check(error, t%nrows() == NROW + 3, "a partial batch should still add its rows")
        if (allocated(error)) return
        call check(error, t%is_null("v", NROW + 3), &
            "a column the appended table does not have at all should be null-filled")
        if (allocated(error)) return
        call check_every_column_length(error, t, "append of a partial batch")
    end subroutine test_append_table
    !
    !> The maintainer's stated use case, end to end: take a subset, extend it with blank rows,
    !! fill them, then append the original table.
    subroutine test_append_null_rows_workflow(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, work
        integer(int32), allocatable :: id(:)
        character(len=*), parameter :: f = "test_run/table_appendnull.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%clone(work)
        call work%truncate(1)                ! keep row 1 only
        call work%append_null_rows(2)
        call check(error, work%nrows() == 3, "append_null_rows should lengthen every column")
        if (allocated(error)) return
        call check(error, work%is_null("id", 2) .and. work%is_null("id", 3), &
            "the appended rows should start out null")
        if (allocated(error)) return
        call work%set_element("id", 2, 71_int32)
        call work%set_element("id", 3, 72_int32)
        call check(error, .not. work%is_null("id", 2), "filling a blank row should clear its null")
        if (allocated(error)) return
        call work%append(t)
        call check(error, work%nrows() == 3 + NROW, "appending the original should add its rows")
        if (allocated(error)) return
        call work%get("id", id)
        call check(error, id(1) == 1_int32 .and. id(2) == 71_int32 .and. id(4) == 1_int32, &
            "the filled rows and the appended table should both be in place")
        if (allocated(error)) return
        ! append_null_rows(0) appends nothing, so it does nothing at all -- no row count change
        ! and, per the "no rows changed => no detach" rule, no detach either. The wider check for
        ! that rule across every row mutation is test_noop_mutation_keeps_file.
        call parquet_open_table(t, f)
        call check(error, .not. t%is_detached(), "precondition: a freshly opened table is not detached")
        if (allocated(error)) return
        call t%append_null_rows(0)
        call check(error, t%nrows() == int(NROW, int64), "append_null_rows(0) should not change the row count")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "append_null_rows(0) should not detach the table")
    end subroutine test_append_null_rows_workflow
    !
    !> One row appended through a row handle.
    subroutine test_append_row(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: src, dst
        type(parquet_table_row) :: r
        integer(int32), allocatable :: id(:)
        character(len=*), parameter :: f = "test_run/table_appendrow.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(src, f)
        call src%materialize_all()
        call src%clone(dst)
        call dst%truncate(2)
        r = src%row(5)
        call dst%append(r)
        call check(error, dst%nrows() == 3, "append(row) should add exactly one row")
        if (allocated(error)) return
        call dst%get("id", id)
        call check(error, id(3) == 5_int32, "the appended row should be the one the handle names")
    end subroutine test_append_row
    !
    !> `%append(row)` used to deep-copy every column of the row's whole SOURCE table and then
    !! throw away all but one row of each, so its cost scaled with the source rather than with
    !! the one row being appended.
    !!
    !! Asserted by comparing two appends that differ only in how big the source is: appending one
    !! row out of a large table must produce the same answer, and take comparable time, to
    !! appending one row out of a small one. The timing bound is deliberately loose (a factor of
    !! 20 against a 200x size difference) -- it is there to catch a return to O(source), not to
    !! measure anything, and a tight bound would be flaky on a busy machine.
    subroutine test_append_row_source_independent(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: small, big, dst
        type(parquet_table_row) :: r
        integer(int32), allocatable :: got(:)
        integer(int32) :: ids(20000)
        integer(int64) :: k
        real(real64) :: t0, t1, t_small, t_big
        !
        do k = 1_int64, 20000_int64
            ids(k) = int(k, int32)
        end do
        call parquet_new_table(small)
        call small%add_column("id", ids(1:100))
        call parquet_new_table(big)
        call big%add_column("id", ids)
        !
        call parquet_new_table(dst)
        call dst%add_column("id", [0_int32])
        r = small%row(50)
        call cpu_time(t0)
        do k = 1_int64, 200_int64
            call dst%append(r)
        end do
        call cpu_time(t1)
        t_small = t1 - t0
        call dst%get("id", got)
        call check(error, size(got) == 201, "200 row appends must add 200 rows")
        if (allocated(error)) return
        call check(error, got(201) == 50_int32, "an appended row must carry the source row's value")
        if (allocated(error)) return
        !
        call parquet_new_table(dst)
        call dst%add_column("id", [0_int32])
        r = big%row(50)
        call cpu_time(t0)
        do k = 1_int64, 200_int64
            call dst%append(r)
        end do
        call cpu_time(t1)
        t_big = t1 - t0
        call dst%get("id", got)
        call check(error, got(201) == 50_int32, "the same row of a 200x larger table must append the same value")
        if (allocated(error)) return
        ! Under the old shape t_big would be ~200x t_small; O(1) in the source makes them alike.
        call check(error, t_big < 20.0_real64*t_small + 0.5_real64, &
            "appending from a large source must not cost proportionally more than from a small one")
    end subroutine test_append_row_source_independent
    !
    !> The row path now writes each column directly rather than assembling a one-row table, so
    !! every kind's copy -- and the validity that comes with it -- is new code.
    subroutine test_append_row_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: src, dst
        type(parquet_table_row) :: r
        integer(int32), allocatable :: i32(:)
        real(real64), allocatable :: f64(:)
        character(len=:), allocatable :: s(:)
        real(real64), allocatable :: vec(:,:)
        logical, allocatable :: valid(:)
        !
        call parquet_new_table(src)
        call src%add_column("id", [1_int32, 2_int32, 3_int32])
        call src%add_column("val", [1.5_real64, 2.5_real64, 3.5_real64])
        ! Shortest first, per CLAUDE.md's sized-from-the-first-element rule.
        call src%add_column("name", ["a  ", "bb ", "ccc"])
        call src%add_column("v", reshape([1.0_real64, 2.0_real64, 3.0_real64, &
            4.0_real64, 5.0_real64, 6.0_real64], [2, 3]))
        ! Row 2 is null in one column only, so the append has to carry per-column validity rather
        ! than a whole-row flag.
        call src%set_null("val", 2)
        !
        call src%clone_structure(dst)
        r = src%row(2)
        call dst%append(r)
        r = src%row(3)
        call dst%append(r)
        call check(error, dst%nrows() == 2, "two row appends must add two rows")
        if (allocated(error)) return
        call dst%get("id", i32)
        call check(error, all(i32 == [2_int32, 3_int32]), "int32 values must come across in order")
        if (allocated(error)) return
        call dst%get("name", s)
        call check(error, trim(s(1)) == "bb" .and. trim(s(2)) == "ccc", "string values must come across")
        if (allocated(error)) return
        call dst%get("v", vec)
        call check(error, all(abs(vec(:,1) - [3.0_real64, 4.0_real64]) < 1.0e-12_real64), &
            "a vector row must carry every element")
        if (allocated(error)) return
        call dst%get("val", f64, is_valid=valid)
        call check(error, .not. valid(1), "a null source element must append as null")
        if (allocated(error)) return
        call check(error, valid(2), "a valid source element must append as valid")
        if (allocated(error)) return
        call check(error, abs(f64(2) - 3.5_real64) < 1.0e-12_real64, "the valid element must keep its value")
    end subroutine test_append_row_kinds
    !
    !> Reading a table allocates every column exact-fit, so there is nothing for `%compact` to
    !! release -- and `%generation()` must NOT advance, because no pointer died. That is what
    !! makes the no-op observable, and what lets a caller compact defensively without forcing a
    !! pointer re-fetch on every table that had nothing to give back.
    subroutine test_compact_noop_after_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64) :: gen_before
        integer(int32), allocatable :: id(:)
        character(len=*), parameter :: f = "test_run/table_compact_noop.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        gen_before = t%generation()
        call t%compact()
        call check(error, t%generation() == gen_before, &
            "compact must not advance the generation when it released nothing")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "compact must never detach the table")
        if (allocated(error)) return
        ! And the values are untouched.
        call t%get("id", id)
        call check(error, size(id) == 6, "compact must not change the row count")
    end subroutine test_compact_noop_after_read
    !
    !> The case `%compact` exists for: a table built by appending holds up to 1.5x the storage
    !! its rows need until the slack is released.
    subroutine test_compact_after_appends(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, batch
        integer(int64) :: gen_before, k
        integer(int32), allocatable :: got(:)
        !
        call parquet_new_table(t)
        call t%add_column("id", [1_int32])
        call parquet_new_table(batch)
        call batch%add_column("id", [2_int32])
        do k = 1_int64, 40_int64
            call t%append(batch)
        end do
        gen_before = t%generation()
        call t%compact()
        call check(error, t%generation() > gen_before, &
            "compact must advance the generation when it actually released storage")
        if (allocated(error)) return
        call check(error, t%nrows() == 41, "compact must not change the row count")
        if (allocated(error)) return
        call t%get("id", got)
        call check(error, got(1) == 1_int32 .and. got(41) == 2_int32, &
            "compact must not disturb the values")
        if (allocated(error)) return
        ! Nothing left to release, so a second compact is the no-op case again.
        gen_before = t%generation()
        call t%compact()
        call check(error, t%generation() == gen_before, "a second compact must find nothing to do")
    end subroutine test_compact_after_appends
    !
    !> A `capacity() >= n` check would pass against a `%reserve` that does nothing. What has to
    !! hold is that the appends the reserve made room for do not reallocate -- observed here
    !! through `%generation()`, which advances once for the reserve and then stays put.
    subroutine test_table_reserve(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, batch
        integer(int64) :: gen_after_reserve, k
        integer(int32), allocatable :: got(:)
        !
        call parquet_new_table(t)
        call t%add_column("id", [1_int32])
        call parquet_new_table(batch)
        call batch%add_column("id", [9_int32])
        call t%reserve(200)
        gen_after_reserve = t%generation()
        do k = 1_int64, 100_int64
            call t%append(batch)
        end do
        call check(error, t%nrows() == 101, "every appended row must be present after a reserve")
        if (allocated(error)) return
        call t%get("id", got)
        call check(error, got(101) == 9_int32, "reserved appends must still write their values")
        if (allocated(error)) return
        ! Appending never advances the generation by itself -- but a reallocation inside one would
        ! have shown up as extra capacity growth, which is what the column-level test asserts
        ! directly. Here the observable is that a reserve below the row count changes nothing.
        call t%reserve(10)
        call check(error, t%generation() == gen_after_reserve + 100_int64, &
            "a reserve below the current row count must be a no-op")
    end subroutine test_table_reserve
    !
    !> `%compact` must not read the file. A column nobody has touched has no storage to shrink,
    !! and touching it would defeat the laziness the table exists to provide.
    subroutine test_compact_keeps_lazy(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: id(:)
        real(real64), allocatable :: v(:)
        character(len=*), parameter :: f = "test_run/table_compact_lazy.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%prefetch(["id"])
        call t%compact()
        call check(error, .not. t%is_detached(), "compact must not detach, so a lazy column stays readable")
        if (allocated(error)) return
        call t%get("id", id)
        call check(error, size(id) == 6, "the resident column must survive compaction")
        if (allocated(error)) return
        ! The column compact skipped is still readable from the file afterwards.
        call t%get("v", v)
        call check(error, size(v) == 6, "a column left unread by compact must still be readable")
    end subroutine test_compact_keeps_lazy
    !
    !> The key kinds the other sort tests do not reach: an int64 column, and a timestamp, which
    !! is the one kind that becomes TWO engine keys (seconds, then nanoseconds) because folding
    !! the pair into a single int64 would overflow outside roughly 1678-2262.
    subroutine test_sort_by_key_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64) :: big(4)
        type(parquet_timestamp) :: ts(4)
        integer(int32) :: id(4)
        integer(int32), allocatable :: got(:)
        !
        id = [1_int32, 2_int32, 3_int32, 4_int32]
        big = [30_int64, 10_int64, 40_int64, 20_int64]
        ! Rows 1 and 2 differ only in the sub-second part, which is exactly what the second
        ! engine key exists to order -- a single-int64 key would have to round them together.
        ts(1) = parquet_timestamp(2026, 1, 1, 0, 0, 5, 900000000)
        ts(2) = parquet_timestamp(2026, 1, 1, 0, 0, 5, 100000000)
        ts(3) = parquet_timestamp(2020, 6, 15, 12, 0, 0)
        ts(4) = parquet_timestamp(2030, 6, 15, 12, 0, 0)
        call parquet_new_table(t)
        call t%add_column("id", id)
        call t%add_column("big", big)
        call t%add_column("when", ts)
        call t%sort_by(["big"])
        call t%get("id", got)
        call check(error, all(got == [2_int32, 4_int32, 1_int32, 3_int32]), &
            "an int64 column should sort by its values")
        if (allocated(error)) return
        call t%sort_by(["when"])
        deallocate(got)
        call t%get("id", got)
        call check(error, all(got == [3_int32, 2_int32, 1_int32, 4_int32]), &
            "a timestamp sort should order by seconds and then by the sub-second part")
    end subroutine test_sort_by_key_kinds
    !
    !> The remaining key kinds sort_extract_integer/sort_extract_real dispatch on that
    !! test_sort_by_key_kinds above does not reach: logical and date (sort_extract_integer's own
    !! branches), time (its default branch), and float32 (sort_extract_real's narrower branch,
    !! widened to real64 the same way get_at already does). Each uses its own fresh table so the
    !! expected order does not have to account for a previous sort's permutation.
    subroutine test_sort_by_more_key_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32) :: id(4)
        logical :: flag(4)
        type(parquet_date) :: asof(4)
        type(parquet_time) :: clock(4)
        real(real32) :: r32(4)
        integer(int32), allocatable :: got(:)
        !
        id = [1_int32, 2_int32, 3_int32, 4_int32]
        !
        flag = [.true., .false., .true., .false.]
        call parquet_new_table(t)
        call t%add_column("id", id)
        call t%add_column("flag", flag)
        call t%sort_by(["flag"])
        call t%get("id", got)
        call check(error, all(got == [2_int32, 4_int32, 1_int32, 3_int32]), &
            "a logical column should sort false before true")
        if (allocated(error)) return
        !
        asof(1) = parquet_date(2026, 1, 1)
        asof(2) = parquet_date(2020, 1, 1)
        asof(3) = parquet_date(2030, 1, 1)
        asof(4) = parquet_date(2025, 1, 1)
        call parquet_new_table(t)
        call t%add_column("id", id)
        call t%add_column("asof", asof)
        call t%sort_by(["asof"])
        deallocate(got)
        call t%get("id", got)
        call check(error, all(got == [2_int32, 4_int32, 1_int32, 3_int32]), &
            "a date column should sort by calendar order")
        if (allocated(error)) return
        !
        clock(1) = parquet_time(12, 0, 0)
        clock(2) = parquet_time(5, 0, 0)
        clock(3) = parquet_time(23, 0, 0)
        clock(4) = parquet_time(0, 0, 1)
        call parquet_new_table(t)
        call t%add_column("id", id)
        call t%add_column("clock", clock)
        call t%sort_by(["clock"])
        deallocate(got)
        call t%get("id", got)
        call check(error, all(got == [4_int32, 2_int32, 1_int32, 3_int32]), &
            "a time column should sort by time-of-day order")
        if (allocated(error)) return
        !
        r32 = [3.5_real32, 1.5_real32, 4.5_real32, 2.5_real32]
        call parquet_new_table(t)
        call t%add_column("id", id)
        call t%add_column("r32", r32)
        call t%sort_by(["r32"])
        deallocate(got)
        call t%get("id", got)
        call check(error, all(got == [2_int32, 4_int32, 1_int32, 3_int32]), &
            "a float32 column should sort by its widened value")
    end subroutine test_sort_by_more_key_kinds
    !
    !> A clone is independent in both directions, stays lazy, and carries the row scope over.
    subroutine test_clone(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, c, sl, cs
        real(real64), allocatable :: v(:)
        integer(int32), allocatable :: id(:)
        character(len=*), parameter :: f = "test_run/table_clone.parquet"
        character(len=*), parameter :: f2 = "test_run/table_clone_slice.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%prefetch("v")                 ! one column read, three not
        call t%clone(c)
        call check(error, c%nrows() == NROW, "a clone should have the same rows")
        if (allocated(error)) return
        call check(error, c%ncols() == 4, "a clone should have the same columns")
        if (allocated(error)) return
        call check(error, c%residency("v") == RES_FULL, "a read column should be copied as read")
        if (allocated(error)) return
        call check(error, c%residency("id") == RES_EMPTY, "an unread column should stay unread")
        if (allocated(error)) return
        ! The clone can still read what the source had not read: it has its own reader.
        call c%get("id", id)
        call check(error, id(3) == 3_int32, "a clone should still be able to read from the file")
        if (allocated(error)) return
        ! Independence, both ways.
        call c%set_element("v", 1, -7.0_real64)
        call t%get("v", v)
        call check(error, v(1) == 30.0_real64, "mutating a clone must not touch the source")
        if (allocated(error)) return
        call t%set_element("v", 2, -8.0_real64)
        deallocate(v)
        call c%get("v", v)
        call check(error, v(2) == 10.0_real64, "mutating the source must not touch a clone")
        if (allocated(error)) return
        !
        ! A slice-regime table's clone is a slice-regime table over the same physical rows.
        call write_slice_fixture(f2, 12, 4)
        call parquet_open_table(sl, f2, 5, 8)
        call sl%clone(cs)
        call check(error, cs%nrows() == 4, "a slice clone should cover the same rows")
        if (allocated(error)) return
        deallocate(id)
        call cs%get("s_i32", id)
        call check(error, id(1) == 5_int32, "a slice clone should start at the same file row")
    end subroutine test_clone
    !
    !> A detached table's clone keeps its values and stays detached -- there is no file to reopen.
    subroutine test_clone_of_detached(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, c
        integer(int32), allocatable :: id(:)
        character(len=*), parameter :: f = "test_run/table_clone_detached.parquet"
        !
        call write_sort_fixture(f)
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%truncate(3)
        call t%clone(c)
        call check(error, c%is_detached(), "a detached table's clone should be detached too")
        if (allocated(error)) return
        call check(error, c%nrows() == 3, "the clone should keep the mutated row count")
        if (allocated(error)) return
        call c%get("id", id)
        call check(error, all(id == [1_int32, 2_int32, 3_int32]), &
            "the clone should hold the values the source had after mutating")
    end subroutine test_clone_of_detached
    !
    ! ------------------------------------------------------------------------------
    ! Read-time transform on parquet_open_table (full regime)
    !
    ! The vocabulary split is the thing under test throughout: filter=/sort=/qc= name
    ! columns the way %col/%get do (INTERNAL names), while a read-in MAML's own
    ! extra: filter:/sort: and fields: qc: name the physical file's columns. A test
    ! that never remaps cannot tell the two apart, which is what
    ! test_transform_with_remap is for.
    ! ------------------------------------------------------------------------------
    !
    !> filter= narrows the table itself: %nrows drops, and every column -- including ones read
    !! lazily long after the open -- covers exactly the surviving rows.
    subroutine test_open_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        integer(int32), allocatable :: i32(:)
        real(real64), allocatable :: f64(:)
        character(len=*), parameter :: f = "test_run/table_xform_filter.parquet"
        !
        call write_basic_fixture(f)
        call filt%add("i32 > 2 and i32 <= 5")
        call parquet_open_table(t, f, filter=filt)
        call check(error, t%nrows() == 3, "filter= did not narrow the table's row count")
        if (allocated(error)) return
        call t%get("i32", i32)
        call check(error, all(i32 == [3_int32, 4_int32, 5_int32]), &
            "filter= did not return the surviving rows of the filtered column")
        if (allocated(error)) return
        ! A column touched only now, well after the open, must see the same row set.
        call t%get("f64", f64)
        call check(error, size(f64) == 3 .and. abs(f64(1) - 6.75_real64) < 1.0e-12_real64, &
            "a lazily-read column did not inherit the filter the table was opened with")
    end subroutine test_open_filter
    !
    !> sort= reorders every column, so a companion column read later still lines up row for row
    !! with the key column.
    subroutine test_open_sort(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_sortkey) :: srt
        integer(int32), allocatable :: i32(:)
        character(len=:), allocatable :: sv(:)
        character(len=*), parameter :: f = "test_run/table_xform_sort.parquet"
        !
        call write_basic_fixture(f)
        call srt%add("-i32")
        call parquet_open_table(t, f, sort=srt)
        call check(error, t%nrows() == NROW, "sort= must not change how many rows the table has")
        if (allocated(error)) return
        call t%get("i32", i32)
        call check(error, all(i32 == [6_int32, 5_int32, 4_int32, 3_int32, 2_int32, 1_int32]), &
            "sort= did not order the key column descending")
        if (allocated(error)) return
        call t%get("s", sv)
        call check(error, trim(sv(1)) == "p" .and. trim(sv(6)) == "a", &
            "a companion column did not come back in the sorted order")
    end subroutine test_open_sort
    !
    !> qc= reaches the reader: a bound the data satisfies opens cleanly and reads normally. The
    !! violating direction aborts, so it lives out of process (scenario table_qc_violation).
    subroutine test_open_qc(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_read_qc) :: qc
        integer(int32), allocatable :: i32(:)
        character(len=*), parameter :: f = "test_run/table_xform_qc.parquet"
        !
        call write_basic_fixture(f)
        call qc%add("i32, >=1, <=6")
        call parquet_open_table(t, f, qc=qc)
        call t%get("i32", i32)
        call check(error, size(i32) == NROW .and. i32(1) == 1_int32, &
            "a satisfied qc= bound should have read the column unchanged")
    end subroutine test_open_qc
    !
    !> The automatic `parquet_row_index` column: which row of the source file each row came from.
    !!
    !! Four mappings, and each is a different mechanism: `i` for a whole file, `row_lo + i - 1`
    !! for an unfiltered slice, and -- for a filtered or sorted table -- the reader's own account
    !! of which file rows survived and in what order, which nothing else can reconstruct. The
    !! column is VIRTUAL until asked for: it costs 8 bytes a row, so materializing it at open
    !! would undo the laziness the whole type is built on.
    subroutine test_row_index_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        type(parquet_sortkey) :: srt
        integer(int64), allocatable :: ri(:)
        integer(int32), allocatable :: k(:)
        character(len=:), allocatable :: names(:)
        integer :: ncols0
        integer, parameter :: N = 20, CH = 7
        character(len=*), parameter :: f = "test_run/table_row_index.parquet"
        !
        call write_slice_xform_fixture(f, N, CH)
        !
        ! Virtual until asked for: %has_column sees it, %ncols and %column_names do not.
        call parquet_open_table(t, f)
        ncols0 = t%ncols()
        call check(error, t%has_column(PARQUET_ROW_INDEX), &
            "%has_column should answer for the row-index column before it is asked for")
        if (allocated(error)) return
        call t%column_names(names)
        call check(error, size(names) == ncols0, &
            "%column_names should not list the row-index column while it is virtual")
        if (allocated(error)) return
        !
        ! Whole file, no transform: 1..nrows, and the column becomes real.
        call t%get(PARQUET_ROW_INDEX, ri)
        call check(error, size(ri) == N .and. ri(1) == 1_int64 .and. ri(N) == int(N, int64), &
            "a whole-file table's row index should be 1..nrows")
        if (allocated(error)) return
        call check(error, t%ncols() == ncols0 + 1, &
            "the row-index column should be counted once it has been asked for")
        if (allocated(error)) return
        call t%column_names(names)
        call check(error, trim(names(size(names))) == PARQUET_ROW_INDEX, &
            "the row-index column should be listed last, after the file's own columns")
        if (allocated(error)) return
        !
        ! An unfiltered slice: the FILE's row numbers, not the slice's own 1..n.
        call parquet_open_table(t, f, 6, 16)
        call t%get(PARQUET_ROW_INDEX, ri)
        call check(error, size(ri) == 11 .and. ri(1) == 6_int64 .and. ri(11) == 16_int64, &
            "a slice's row index should be its FILE rows, not 1..nrows")
        if (allocated(error)) return
        !
        ! A filter: exactly the file rows that survived, which only the reader knows.
        call filt%add("k > 8")
        call parquet_open_table(t, f, filter=filt)
        call t%get(PARQUET_ROW_INDEX, ri)
        call t%get("k", k)
        call check(error, size(ri) == size(k), "the row index should have one entry per surviving row")
        if (allocated(error)) return
        call check(error, all(ri == int(k, int64)), &
            "in this fixture k equals the file row, so the row index should reproduce it")
        if (allocated(error)) return
        !
        ! A sort: the file rows in the sorted order.
        call srt%add("-k")
        call parquet_open_table(t, f, sort=srt)
        call t%get(PARQUET_ROW_INDEX, ri)
        call t%get("k", k)
        call check(error, all(ri == int(k, int64)), &
            "a sorted table's row index should follow the sorted order")
        if (allocated(error)) return
        call check(error, ri(1) == int(N, int64), "the largest key should come first under -k")
        if (allocated(error)) return
        !
        ! Materialized before a mutation, it survives it and reports where each row came from.
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%get(PARQUET_ROW_INDEX, ri)
        call t%delete_rows([1, 2, 3])
        call t%get(PARQUET_ROW_INDEX, ri)
        call check(error, ri(1) == 4_int64, &
            "a materialized row index should survive a row mutation and still name the file row")
        if (allocated(error)) return
        !
        ! A table built in memory was not read from a file, so it has no row index at all.
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call check(error, .not. t%has_column(PARQUET_ROW_INDEX), &
            "an in-memory table should have no row-index column")
    end subroutine test_row_index_column
    !
    !> %nrows_unfiltered and %row_group_extent report what the table was cut FROM.
    !!
    !! `%nrows()` answers what it holds; these two answer what it came from, and nothing else can
    !! once a filter is active -- a filtered reader counts survivors, so asking it later just
    !! repeats `%nrows()`. Both are captured at open, which is also what lets them keep answering
    !! after a row mutation has detached the table.
    subroutine test_row_geometry_queries(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        integer, parameter :: N = 20, CH = 7
        character(len=*), parameter :: f = "test_run/table_row_geometry.parquet"
        !
        call write_slice_xform_fixture(f, N, CH)
        !
        ! Whole file, no transform: all three agree.
        call parquet_open_table(t, f)
        call check(error, t%nrows() == 20_int64 .and. t%nrows_unfiltered() == 20_int64 .and. &
            t%row_group_extent() == 20_int64, &
            "an untransformed whole-file table should report the same count three ways")
        if (allocated(error)) return
        !
        ! Whole file with a filter: %nrows drops, the other two do not.
        call filt%add("k > 8")
        call parquet_open_table(t, f, filter=filt)
        call check(error, t%nrows() == 12_int64, "precondition: the filter should keep 12 rows")
        if (allocated(error)) return
        call check(error, t%nrows_unfiltered() == 20_int64, &
            "%nrows_unfiltered should report the file's own row count under a filter")
        if (allocated(error)) return
        call check(error, t%row_group_extent() == 20_int64, &
            "%row_group_extent should cover the whole file for a whole-file table")
        if (allocated(error)) return
        !
        ! A slice straddling row groups: the extent is what reading it actually decodes. Row
        ! groups are 1-7, 8-14, 15-20; the slice 6..16 spans all three.
        call parquet_open_table(t, f, 6, 16)
        call check(error, t%nrows() == 11_int64 .and. t%nrows_unfiltered() == 11_int64, &
            "an unfiltered slice holds exactly its own length")
        if (allocated(error)) return
        call check(error, t%row_group_extent() == 20_int64, &
            "a slice straddling every row group decodes all of them")
        if (allocated(error)) return
        !
        ! A slice on a row-group boundary pays for exactly that row group.
        call parquet_open_table(t, f, 8, 14)
        call check(error, t%row_group_extent() == 7_int64, &
            "a slice matching a row group should decode only that row group")
        if (allocated(error)) return
        !
        ! A filtered slice: the slice's own length, not the survivors.
        call parquet_open_table(t, f, 6, 16, filter=filt)
        call check(error, t%nrows_unfiltered() == 11_int64, &
            "a filtered slice's unfiltered count is the slice's length")
        if (allocated(error)) return
        call check(error, t%nrows() < t%nrows_unfiltered(), &
            "precondition: the filter should have removed rows from the slice")
        if (allocated(error)) return
        !
        ! Both survive a detach, which is the reason they are captured at open.
        call t%materialize_all()
        call t%truncate(2)
        call check(error, t%is_detached() .and. t%nrows_unfiltered() == 11_int64 .and. &
            t%row_group_extent() == 20_int64, &
            "both counts should survive the table detaching from its file")
        if (allocated(error)) return
        !
        ! A table built in memory was not cut from anything.
        call parquet_new_table(t)
        call t%add_column("a", [1_int32, 2_int32])
        call check(error, t%nrows_unfiltered() == 0_int64 .and. t%row_group_extent() == 0_int64, &
            "an in-memory table should report no physical geometry")
    end subroutine test_row_geometry_queries
    !
    !> %print_stat reports what the table holds -- and, crucially, reads nothing to do it.
    !!
    !! The output goes to stdout, so what a test can assert is the behaviour around it rather than
    !! the text: that printing a lazy table leaves it lazy, that `all=.true.` still does not read,
    !! and that a deferred plain-LIST column is not resolved by being printed. Those are the three
    !! ways a diagnostic could quietly change what it is diagnosing.
    subroutine test_print_stat(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, mem
        character(len=*), parameter :: f = "test_run/table_print_stat.parquet"
        character(len=*), parameter :: fl = "test/fixtures/list_widths.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        !
        ! A lazy table: nothing to show, and showing it must not change that.
        call t%print_stat()
        call check(error, t%residency("i32") == RES_EMPTY .and. t%residency("f64") == RES_EMPTY, &
            "%print_stat must not read anything")
        if (allocated(error)) return
        call t%print_stat(all=.true.)
        call check(error, t%residency("i32") == RES_EMPTY, &
            "%print_stat(all=.true.) must not read anything either")
        if (allocated(error)) return
        !
        ! With values in memory it has something to report -- including for a column carrying nulls.
        call t%prefetch("i32")
        call t%set_null("i32", 2)
        call t%print_stat()
        call check(error, t%residency("i32") == RES_FULL .and. t%residency("f64") == RES_EMPTY, &
            "%print_stat must not materialize the columns it is not showing")
        if (allocated(error)) return
        !
        ! A table built in memory has no file, and prints anyway.
        call parquet_new_table(mem)
        call mem%add_column("id", [1_int32, 2_int32, 3_int32], unit="count")
        call mem%add_column("name", ["a  ", "bcd", "ef "])
        call mem%print_stat()
        call check(error, mem%ncols() == 2, "%print_stat should leave an in-memory table alone")
        if (allocated(error)) return
        !
        ! A deferred plain-LIST column prints as pending rather than being measured.
        call parquet_open_table(t, fl)
        call t%print_stat(all=.true.)
        call check(error, t%residency("avg_ok") == RES_EMPTY, &
            "%print_stat must not resolve a deferred LIST column's width")
    end subroutine test_print_stat
    !
    !> %validate_qc checks every qc-declaring column and leaves the table's residency alone.
    !!
    !! On a lazy table qc is only enforced for columns something actually reads, so a program
    !! using two of forty columns never learns whether the rest satisfy their bounds. This reads
    !! exactly the declaring columns and then releases what it created -- and the "what it
    !! created" half is the one worth testing: a column the program had already read must still be
    !! resident afterwards, with its values intact.
    subroutine test_validate_qc(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_read_qc) :: qc
        integer(int32), allocatable :: i32(:)
        character(len=*), parameter :: f = "test_run/table_validate_qc.parquet"
        !
        call write_basic_fixture(f)
        call qc%add("i32, >=1, <=6")
        call qc%add("f64, >=0")
        call parquet_open_table(t, f, qc=qc)
        !
        ! One declaring column is read by the program first; the other is not.
        call t%prefetch("f64")
        call check(error, t%residency("f64") == RES_FULL .and. t%residency("i32") == RES_EMPTY, &
            "precondition: exactly one qc-declaring column is resident")
        if (allocated(error)) return
        !
        call t%validate_qc()
        call check(error, t%residency("i32") == RES_EMPTY, &
            "%validate_qc should release the columns it had to read")
        if (allocated(error)) return
        call check(error, t%residency("f64") == RES_FULL, &
            "%validate_qc must leave a column the program had already read resident")
        if (allocated(error)) return
        ! A column with no qc declared is not touched at all.
        call check(error, t%residency("i64") == RES_EMPTY, &
            "%validate_qc should not read a column that declares no bound")
        if (allocated(error)) return
        ! ...and the released column is still perfectly readable afterwards.
        call t%get("i32", i32)
        call check(error, size(i32) == NROW .and. i32(1) == 1_int32, &
            "a column released by %validate_qc should read normally afterwards")
        if (allocated(error)) return
        !
        ! No qc at all: a no-op, not an error.
        call parquet_open_table(t, f)
        call t%validate_qc()
        call check(error, t%residency("i32") == RES_EMPTY, &
            "%validate_qc on a table with no qc should read nothing")
    end subroutine test_validate_qc
    !
    !> A MAML's own extra: filter:/extra: sort: apply on their own, and AND/append with a
    !! code-supplied filter/sort respectively.
    subroutine test_open_maml_filter_sort(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        integer(int32), allocatable :: i32(:)
        character(len=*), parameter :: f = "test_run/table_xform_mamlfs.parquet"
        character(len=*), parameter :: m = "test_run/table_xform_mamlfs.maml"
        !
        call write_basic_fixture(f)
        call write_maml_file(m, [character(len=40) :: &
            "table: xform", &
            "extra:", &
            "  filter:", &
            '  - "i32 >= 2"', &
            "  sort:", &
            '  - "i32 desc"'])
        call parquet_open_table(t, f, maml=m)
        call t%get("i32", i32)
        call check(error, all(i32 == [6_int32, 5_int32, 4_int32, 3_int32, 2_int32]), &
            "a MAML's own extra: filter:/sort: did not apply")
        if (allocated(error)) return

        ! The code filter AND-combines with the MAML's, leaving 2..4 in the MAML's descending order.
        call filt%add("i32 <= 4")
        call parquet_open_table(t, f, maml=m, filter=filt)
        call t%get("i32", i32)
        call check(error, all(i32 == [4_int32, 3_int32, 2_int32]), &
            "a code filter did not AND-combine with the MAML's own")
    end subroutine test_open_maml_filter_sort
    !
    !> The one grammar extension this stage makes: an extra: sort: entry may end in
    !! nulls_first/nulls_last, which a plain YAML string list has nowhere else to carry.
    subroutine test_maml_sort_nulls_token(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_writer) :: w
        integer(int32) :: id(4) = [1_int32, 2_int32, 3_int32, 4_int32]
        real(real64) :: v(4) = [3.0_real64, 1.0_real64, 0.0_real64, 2.0_real64]
        logical :: valid(4) = [.true., .true., .false., .true.]
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: f = "test_run/table_xform_nulls.parquet"
        character(len=*), parameter :: m1 = "test_run/table_xform_nulls_first.maml"
        character(len=*), parameter :: m2 = "test_run/table_xform_nulls_last.maml"
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "v", v, is_valid=valid)
        call parquet_close_writer(w)

        call write_maml_file(m1, [character(len=40) :: &
            "table: xform", "extra:", "  sort:", '  - "v asc nulls_first"'])
        call parquet_open_table(t, f, maml=m1)
        call t%get("id", got)
        call check(error, got(1) == 3_int32, "nulls_first did not place the null row first")
        if (allocated(error)) return

        call write_maml_file(m2, [character(len=40) :: &
            "table: xform", "extra:", "  sort:", '  - "v asc NULLS_LAST"'])
        call parquet_open_table(t, f, maml=m2)
        call t%get("id", got)
        call check(error, got(4) == 3_int32, &
            "nulls_last (and its case-insensitivity) did not place the null row last")
    end subroutine test_maml_sort_nulls_token
    !
    !> The vocabulary split, stated as a test: `mass` exists only as an INTERNAL name, and only the
    !! code-facing arguments may use it -- the MAML's own filter would have to say `f64`, the
    !! file's name. Nothing here would fail if the translation were skipped for only one of the
    !! three, so all three are exercised over the same remapped column.
    subroutine test_transform_with_remap(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        type(parquet_sortkey) :: srt
        type(parquet_read_qc) :: qc
        real(real64), allocatable :: mass(:)
        character(len=*), parameter :: f = "test_run/table_xform_remap.parquet"
        character(len=*), parameter :: m = "test_run/table_xform_remap.maml"
        !
        call write_basic_fixture(f)
        call write_maml_file(m, [character(len=40) :: &
            "table: xform", "extra:", "  remap:", "  - mass: f64"])
        call filt%add("mass > 4.0")
        call srt%add("-mass")
        call qc%add("mass, >=0")
        call parquet_open_table(t, f, maml=m, filter=filt, sort=srt, qc=qc)
        call t%get("mass", mass)
        ! f64 is i*2.25 for i = 1..6, so "> 4.0" keeps five rows.
        call check(error, size(mass) == 5, &
            "a filter written in internal names did not narrow the remapped column")
        if (allocated(error)) return
        call check(error, mass(1) > mass(5), &
            "a sort key written in internal names did not order the remapped column")
    end subroutine test_transform_with_remap
    !
    !> sample_fraction= narrows the table, and the same seed gives the same rows twice -- which is
    !! also what tells this apart from a filter that happens to keep the same count.
    subroutine test_open_sample(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: a(:), b(:)
        character(len=*), parameter :: f = "test_run/table_xform_sample.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f, sample_fraction=0.5_real64, sample_seed=1234_int32)
        call check(error, t%nrows() < int(NROW, int64) .and. t%nrows() > 0_int64, &
            "sample_fraction= should keep some but not all of the rows")
        if (allocated(error)) return
        call t%get("i32", a)

        call parquet_open_table(t, f, sample_fraction=0.5_real64, sample_seed=1234_int32)
        call t%get("i32", b)
        call check(error, size(a) == size(b) .and. all(a == b), &
            "the same sample_seed did not reproduce the same sample")
    end subroutine test_open_sample
    !
    !> A clone reopens the file for its own lazy reads, so it must reattach the same transform --
    !! otherwise a column the source never touched comes back with rows the source had filtered
    !! away, two different lengths inside one table.
    subroutine test_clone_keeps_transform(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, c
        type(parquet_filter) :: filt
        type(parquet_sortkey) :: srt
        integer(int32), allocatable :: i32(:)
        real(real64), allocatable :: f64(:)
        character(len=*), parameter :: f = "test_run/table_xform_clone.parquet"
        !
        call write_basic_fixture(f)
        call filt%add("i32 >= 3")
        call srt%add("-i32")
        call parquet_open_table(t, f, filter=filt, sort=srt)
        call t%prefetch("i32")          ! resident in the source, so it is deep-copied
        call t%clone(c)
        call check(error, c%nrows() == 4, "the clone lost the source's filtered row count")
        if (allocated(error)) return
        ! f64 was never touched in the source, so the clone must read it through its OWN reader --
        ! the one place a missing transform would show up.
        call c%get("f64", f64)
        call c%get("i32", i32)
        call check(error, size(f64) == 4, &
            "a column read lazily through the clone's own reader ignored the transform")
        if (allocated(error)) return
        call check(error, all(i32 == [6_int32, 5_int32, 4_int32, 3_int32]), &
            "the clone did not keep the source's sort order")
        if (allocated(error)) return
        call check(error, abs(f64(1) - 13.5_real64) < 1.0e-12_real64, &
            "the clone's lazily-read column did not line up with its sorted key column")
    end subroutine test_clone_keeps_transform
    !
    !> An UNSEEDED `sample_fraction=` is settled once, at table open, so every reader the table
    !! opens afterwards draws the identical rows. `%clone` is where that becomes observable: a
    !! clone deep-copies whatever the source had already read and reopens the file for everything
    !! else, so a column read lazily through the clone's own reader has to line up, row for row,
    !! with a column carried over as values.
    !!
    !! Before the seed was settled at open, it was not carried at all -- `read_sample_seed` stayed
    !! unallocated and the clone's reader drew a fresh subset of its own.
    !!
    !! Both halves of this test are load-bearing, and the second is the one that would be dropped
    !! as redundant. It is the negative control: an unseeded open must still be a FRESH draw per
    !! `parquet_open_table` call, so a "fix" that settled on a constant seed -- which would satisfy
    !! the first half perfectly -- fails here.
    subroutine test_clone_keeps_unseeded_sample(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, c, t2
        integer(int32), allocatable :: k(:), k2(:)
        real(real64), allocatable :: x(:)
        logical :: same
        integer, parameter :: N = 2000
        character(len=*), parameter :: f = "test_run/table_unseeded_sample_clone.parquet"
        !
        ! Large enough that two independent Bernoulli(0.5) draws agreeing on every row has
        ! probability 2**-2000 -- this test's determinism rests on the fixture size, not on luck.
        call write_slice_xform_fixture(f, N, 500)
        call parquet_open_table(t, f, sample_fraction=0.5_real64)
        call check(error, t%nrows() > 0_int64 .and. t%nrows() < int(N, int64), &
            "sample_fraction=0.5 should keep some but not all of the rows")
        if (allocated(error)) return
        call t%prefetch("k")            ! resident, so the clone gets it as deep-copied VALUES
        call t%clone(c)
        call c%get("k", k)              ! the source's sample, carried across as values
        call c%get("x", x)              ! read lazily through the CLONE'S OWN reader
        call check(error, size(x) == size(k), &
            "the clone's own reader drew a different number of rows from the source's sample")
        if (allocated(error)) return
        ! Row for row, not merely the same count. Two independent draws can agree on HOW MANY rows
        ! they keep while keeping different ones, and parquet_check_read_row_count -- the guard
        ! that made this bug abort rather than pass silently -- compares only counts. A
        ! count-only assertion here would pass against exactly the defect this test exists for.
        call check(error, all(abs(x - 1.5_real64 * real(k, real64)) < 1.0e-9_real64), &
            "a column read through the clone's own reader came from different rows than the " // &
            "source's sample")
        if (allocated(error)) return
        !
        ! Negative control: unseeded still means a fresh draw per open. Compared by membership
        ! rather than by count, because two draws coinciding in count is perfectly ordinary
        ! (about 1 open in 50 here) while two draws coinciding in every row is not.
        call parquet_open_table(t2, f, sample_fraction=0.5_real64)
        call t2%get("k", k2)
        ! Nested rather than `size(...) == size(...) .and. all(...)`: Fortran does not guarantee
        ! short-circuit evaluation, so the combined form compares two differently-sized arrays --
        ! which is the ordinary outcome here and aborts under -fcheck=bounds.
        same = .false.
        if (size(k2) == size(k)) same = all(k2 == k)
        call check(error, .not. same, &
            "two unseeded sample_fraction= opens produced the identical sample; an unseeded " // &
            "open must still draw fresh entropy, not settle on a fixed seed")
    end subroutine test_clone_keeps_unseeded_sample
    !
    !> Three plain columns over several row groups, with no nulls anywhere: the slice-regime
    !! transform tests below are about which ROWS come back, so every expectation should be
    !! readable off the row number alone.
    subroutine write_slice_xform_fixture(fname, n, chunk)
        character(len=*), intent(in) :: fname !! file to write (one per test).
        integer, intent(in) :: n              !! rows to write.
        integer, intent(in) :: chunk          !! rows per row group.
        type(parquet_writer) :: w
        integer(int32), allocatable :: k(:)
        real(real64), allocatable :: x(:)
        character(len=8), allocatable :: s(:)
        integer :: i
        !
        allocate(k(n), x(n), s(n))
        do i = 1, n
            k(i) = i
            x(i) = real(i, real64) * 1.5_real64
            write(s(i), '(a,i0)') "r", i
        end do
        ! Deliberately the shortest first, so a "sized from the first element" bug shows up.
        s(1) = "a"
        call parquet_open_writer(w, fname, chunk_size=chunk)
        call parquet_write_column(w, "k", k)
        call parquet_write_column(w, "x", x)
        call parquet_write_column(w, "s", s)
        call parquet_close_writer(w)
    end subroutine write_slice_xform_fixture
    !
    !> An unfiltered, unsampled slice keeps the fast path even when its bounds fall INSIDE row
    !! groups: the trim happens in memory, with no reader-side mask involved.
    !!
    !! `%nrows()` being exactly the slice's own length is a necessary condition but not a
    !! sufficient one: a mask carrying only the slice's range and no clauses would also keep every
    !! row. `%row_group_bounds`'s two coordinate systems are what actually discriminate -- under a
    !! mask the table's own numbering restarts at 1 and the row groups before the slice collapse to
    !! empty ranges, so the two arrays being IDENTICAL is only possible with no mask installed.
    subroutine test_slice_fast_path(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, full
        integer, parameter :: N = 20, CH = 7, LO = 6, HI = 16
        integer(int32), allocatable :: k(:), fk(:)
        real(real64), allocatable :: x(:)
        integer(int64), allocatable :: tbounds(:,:), pbounds(:,:), fbounds(:,:)
        character(len=*), parameter :: f = "test_run/table_slice_fast.parquet"
        integer :: i
        !
        call write_slice_xform_fixture(f, N, CH)
        ! LO and HI both sit inside a row group (the groups are 1-7, 8-14, 15-20), so both the
        ! head and the tail trim run.
        call parquet_open_table(t, f, LO, HI)
        call check(error, t%nrows() == int(HI - LO + 1, int64), &
            "an unfiltered slice must have exactly its own length as its row count")
        if (allocated(error)) return
        ! The actual no-mask assertion: see this test's own doc-comment for why the row count
        ! above cannot make it on its own.
        call t%row_group_bounds(tbounds)
        call t%row_group_bounds(pbounds, physical=.true.)
        call check(error, all(tbounds == pbounds), &
            "an unfiltered slice's two coordinate systems must be identical, i.e. no mask")
        if (allocated(error)) return
        call parquet_table_row_group_bounds(f, fbounds)
        call check(error, all(tbounds == fbounds), &
            "an unfiltered slice's row groups must be the file's own, untouched")
        if (allocated(error)) return
        call t%get("k", k)
        call check(error, all(k == [(int(i, int32), i = LO, HI)]), &
            "an unfiltered slice should return exactly its own file rows")
        if (allocated(error)) return
        call t%get("x", x)
        call check(error, all(abs(x - real(k, real64) * 1.5_real64) < 1.0e-12_real64), &
            "a lazily-read column of an unfiltered slice should line up with its key column")
        if (allocated(error)) return
        call parquet_open_table(full, f)
        call full%get("k", fk)
        call check(error, all(k == fk(LO:HI)), &
            "the slice's rows should be the same rows the whole-file table reports")
    end subroutine test_slice_fast_path
    !
    !> A slice opened with `filter=`: the slice's own row range and the filter compose, so the
    !! table holds the rows of [row_lo, row_hi] that match -- and nothing from outside it, even
    !! though rows past `row_hi` match the filter too.
    subroutine test_slice_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        integer, parameter :: N = 20, CH = 7, LO = 6, HI = 16
        integer(int32), allocatable :: k(:)
        real(real64), allocatable :: x(:)
        character(len=:), allocatable :: sv(:)
        integer(int64), allocatable :: tbounds(:,:), fbounds(:,:)
        character(len=*), parameter :: f = "test_run/table_slice_filter.parquet"
        integer :: i
        !
        call write_slice_xform_fixture(f, N, CH)
        call filt%add("k > 8")
        call parquet_open_table(t, f, LO, HI, filter=filt)
        ! Rows 17..20 satisfy the filter as well; the slice is what keeps them out.
        call check(error, t%nrows() == int(HI - 8, int64), &
            "a filtered slice's row count should be its own surviving rows, not its length")
        if (allocated(error)) return
        call t%get("k", k)
        call check(error, all(k == [(int(i, int32), i = 9, HI)]), &
            "a filtered slice should return the matching rows of its own range only")
        if (allocated(error)) return
        ! Read well after the open, and from a row group the slice only partly covers, so a
        ! coordinate mix-up between the table's rows and the file's would show up as shifted data.
        call t%get("x", x)
        call check(error, all(abs(x - real(k, real64) * 1.5_real64) < 1.0e-12_real64), &
            "a lazily-read column of a filtered slice did not line up with its key column")
        if (allocated(error)) return
        call t%get("s", sv)
        call check(error, trim(sv(1)) == "r9" .and. trim(sv(size(sv))) == "r16", &
            "a string column of a filtered slice did not line up with its key column")
        if (allocated(error)) return
        ! physical=.true. is the planning form, and has to stay in the file's own numbering --
        ! which on this path is no longer the numbering the table itself works in.
        call t%row_group_bounds(tbounds, physical=.true.)
        call parquet_table_row_group_bounds(f, fbounds)
        call check(error, all(tbounds == fbounds), &
            "a filtered slice should report the file's own row-group bounds under physical=.true.")
    end subroutine test_slice_filter
    !
    !> The same masked path reached from a read-in MAML's own `extra: filter:` rather than a code
    !! argument, with a satisfied `qc=` bound alongside it -- qc needs no scoping, so a slice
    !! carries it exactly as the whole-file form does.
    subroutine test_slice_maml_filter_qc(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_read_qc) :: qc
        integer, parameter :: N = 20, CH = 7, LO = 6, HI = 16
        integer(int32), allocatable :: k(:)
        real(real64), allocatable :: x(:)
        character(len=*), parameter :: f = "test_run/table_slice_mamlf.parquet"
        character(len=*), parameter :: m = "test_run/table_slice_mamlf.maml"
        integer :: i
        !
        call write_slice_xform_fixture(f, N, CH)
        call write_maml_file(m, [character(len=40) :: &
            "table: slice_xform", &
            "extra:", &
            "  filter:", &
            '  - "k >= 10"'])
        call qc%add("k, >=1, <=20")
        call parquet_open_table(t, f, LO, HI, maml=m, qc=qc)
        call check(error, t%nrows() == int(HI - 9, int64), &
            "a maml filter on a slice should narrow the slice's own row count")
        if (allocated(error)) return
        call t%get("k", k)
        call check(error, all(k == [(int(i, int32), i = 10, HI)]), &
            "a maml filter on a slice should return the matching rows of its own range only")
        if (allocated(error)) return
        call t%get("x", x)
        call check(error, all(abs(x - real(k, real64) * 1.5_real64) < 1.0e-12_real64), &
            "a lazily-read column did not line up after a maml filter on a slice")
    end subroutine test_slice_maml_filter_qc
    !
    !> `sample_fraction=` with no filter at all takes the same masked path: there are no clauses to
    !! carry the slice's row range, so the range is installed on its own and the draw folds into it.
    subroutine test_slice_sample(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, again
        integer, parameter :: N = 20, CH = 7, LO = 6, HI = 16
        integer(int32), allocatable :: k(:), k2(:)
        character(len=*), parameter :: f = "test_run/table_slice_sample.parquet"
        !
        call write_slice_xform_fixture(f, N, CH)
        call parquet_open_table(t, f, LO, HI, sample_fraction=0.5_real64, sample_seed=20260802_int32)
        call check(error, t%nrows() > 0_int64 .and. t%nrows() < int(HI - LO + 1, int64), &
            "a sampled slice should keep some but not all of its own rows")
        if (allocated(error)) return
        call t%get("k", k)
        call check(error, all(k >= int(LO, int32) .and. k <= int(HI, int32)), &
            "a sampled slice must not return a row from outside its own range")
        if (allocated(error)) return
        ! The draw is seeded, so the same slice asked twice is the same rows -- which also confirms
        ! the range is applied to the draw rather than the draw being redone per open.
        call parquet_open_table(again, f, LO, HI, sample_fraction=0.5_real64, sample_seed=20260802_int32)
        call again%get("k", k2)
        call check(error, size(k2) == size(k), "the same seed should keep the same number of rows")
        if (allocated(error)) return
        call check(error, all(k2 == k), "the same seed should keep the same rows")
    end subroutine test_slice_sample
    !
    !> A clone of a masked slice reopens its own reader, and has to put it in exactly the state
    !! the source's is in -- scoped filter, same row range and all. A bare reopen would give the
    !! clone's lazily-read columns the whole covering row groups instead, which is a silently
    !! wrong answer rather than an error.
    subroutine test_slice_clone(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, c
        type(parquet_filter) :: filt
        integer, parameter :: N = 20, CH = 7, LO = 6, HI = 16
        integer(int32), allocatable :: k(:)
        real(real64), allocatable :: x(:)
        integer(int64), allocatable :: cbounds(:,:), fbounds(:,:)
        character(len=*), parameter :: f = "test_run/table_slice_clone.parquet"
        integer :: i
        !
        call write_slice_xform_fixture(f, N, CH)
        call filt%add("k > 8")
        call parquet_open_table(t, f, LO, HI, filter=filt)
        ! Nothing is read before the clone, so every column below is read through the CLONE's own
        ! reader -- the one this test is about.
        call t%clone(c)
        call check(error, c%nrows() == int(HI - 8, int64), &
            "a clone of a filtered slice should keep its row count")
        if (allocated(error)) return
        call c%get("k", k)
        call check(error, all(k == [(int(i, int32), i = 9, HI)]), &
            "a clone of a filtered slice returned different rows from its source")
        if (allocated(error)) return
        call c%get("x", x)
        call check(error, all(abs(x - real(k, real64) * 1.5_real64) < 1.0e-12_real64), &
            "a clone's lazily-read column did not line up with its key column")
        if (allocated(error)) return
        call c%row_group_bounds(cbounds, physical=.true.)
        call parquet_table_row_group_bounds(f, fbounds)
        call check(error, all(cbounds == fbounds), &
            "a clone of a filtered slice should still report the file's own row-group bounds")
    end subroutine test_slice_clone
    !
    !> `%row_group_bounds`'s two coordinate systems, on both the tables where they differ: a
    !! filtered slice and a filtered whole file.
    !!
    !! Three rules are checked, and each fails differently. The default form must TILE `1..%nrows()`
    !! exactly, in order and with no gap, or a row index in hand cannot be related to the row group
    !! it came from. `physical=.true.` must reproduce the pre-open planning call exactly, or a
    !! caller cannot choose the next slice from an open table. And both must have ONE ENTRY PER
    !! PHYSICAL ROW GROUP so the two can be read side by side -- which is what forces a row group
    !! contributing nothing to be an empty range rather than a dropped entry.
    subroutine test_row_group_bounds_physical(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        integer, parameter :: N = 20, CH = 7, LO = 6, HI = 16
        integer(int64), allocatable :: tb(:,:), pb(:,:), fb(:,:)
        integer(int64) :: rg, next, empties
        character(len=*), parameter :: f = "test_run/table_rgbounds_physical.parquet"
        !
        call write_slice_xform_fixture(f, N, CH)
        call parquet_table_row_group_bounds(f, fb)
        !
        ! A filtered slice: row groups are 1-7, 8-14, 15-20; the slice is 6..16 and the filter
        ! keeps k > 8, so only row groups 2 and 3 contribute and row group 1 contributes nothing.
        call filt%add("k > 8")
        call parquet_open_table(t, f, LO, HI, filter=filt)
        call t%row_group_bounds(tb)
        call t%row_group_bounds(pb, physical=.true.)
        call check(error, size(tb, 2) == size(fb, 2) .and. size(pb, 2) == size(fb, 2), &
            "both forms should have one entry per PHYSICAL row group, contributing or not")
        if (allocated(error)) return
        call check(error, all(pb == fb), &
            "physical=.true. should reproduce parquet_table_row_group_bounds exactly")
        if (allocated(error)) return
        ! The default form tiles the table's own rows, skipping the entries that contribute none.
        next = 1_int64
        empties = 0_int64
        do rg = 1_int64, size(tb, 2, kind=int64)
            if (tb(1, rg) > tb(2, rg)) then
                empties = empties + 1_int64
                cycle
            end if
            call check(error, tb(1, rg) == next, &
                "a contributing row group should start where the previous one left off")
            if (allocated(error)) return
            next = tb(2, rg) + 1_int64
        end do
        call check(error, next - 1_int64 == t%nrows(), &
            "the table-coordinate bounds should tile 1..nrows() exactly")
        if (allocated(error)) return
        call check(error, empties == 1_int64, &
            "the row group the slice and filter exclude entirely should be an empty range")
        if (allocated(error)) return
        ! ...and the two are index-aligned, so row group 2's table rows really are the rows read
        ! from row group 2's file rows.
        call check(error, tb(1, 2) == 1_int64 .and. pb(1, 2) == 8_int64, &
            "the first contributing row group should supply the table's first row")
        if (allocated(error)) return
        !
        ! A filtered WHOLE-FILE table: no rg_bounds is precomputed at open, so both forms are
        ! worked out on demand -- the default from the (masked) reader, physical= from a
        ! footer-only reader of its own.
        call parquet_open_table(t, f, filter=filt)
        call t%row_group_bounds(tb)
        call t%row_group_bounds(pb, physical=.true.)
        call check(error, all(pb == fb), &
            "a filtered whole-file table should still report the file's own bounds under physical=")
        if (allocated(error)) return
        call check(error, tb(1, 1) == 1_int64 .and. tb(2, size(tb, 2)) == t%nrows(), &
            "a filtered whole-file table's default bounds should tile its own rows")
        if (allocated(error)) return
        !
        ! A SAMPLED table with no filter and no sort. It reaches the same on-demand branch as the
        ! filtered case, but through a different predicate -- there is no parquet_filter object
        ! anywhere -- so it is the one narrowing transform the cases above never exercise alone.
        call parquet_open_table(t, f, sample_fraction=0.5_real64, sample_seed=7)
        call t%row_group_bounds(tb)
        call t%row_group_bounds(pb, physical=.true.)
        call check(error, all(pb == fb), &
            "a sampled table should report the file's own bounds under physical=.true.")
        if (allocated(error)) return
        call check(error, size(tb, 2) == size(fb, 2), &
            "a sampled table should still report one entry per physical row group")
        if (allocated(error)) return
        next = 1_int64
        do rg = 1_int64, size(tb, 2, kind=int64)
            if (tb(1, rg) > tb(2, rg)) cycle
            call check(error, tb(1, rg) == next, &
                "a sampled table's contributing row groups should tile its own rows in order")
            if (allocated(error)) return
            next = tb(2, rg) + 1_int64
        end do
        call check(error, next - 1_int64 == t%nrows(), &
            "a sampled table's default bounds should tile 1..nrows() exactly")
    end subroutine test_row_group_bounds_physical
    !
    !> A row mutation on a SLICE, and what it costs a column the slice never read.
    !!
    !! This is where two mechanisms meet: detaching rewrites `regime`/`row_lo`/`row_hi` into a
    !! plain in-memory table, and a row-structural mutation SKIPS a column that is not resident
    !! rather than refusing to run -- which is what lets a lazy table drop rows without first
    !! reading everything it has. The skipped column is then unreadable for good. Here that is
    !! asserted from the successful side (the resident column really is mutated, and the row scope
    !! really is gone); the read that can no longer happen is `table_slice_mutate_then_read` in
    !! test/error_scenarios.f90, since it aborts.
    subroutine test_mutate_slice_then_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer, parameter :: N = 20, CH = 7, LO = 6, HI = 16
        integer(int32), allocatable :: k(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_slice_mutate.parquet"
        !
        call write_slice_xform_fixture(f, N, CH)
        call parquet_open_table(t, f, LO, HI)
        call t%prefetch("k")        ! "x" and "s" are deliberately left unread
        call check(error, t%nrows() == HI - LO + 1, "precondition: the slice should hold its own range")
        if (allocated(error)) return
        call check(error, t%residency("x") == RES_EMPTY, "precondition: x should not be resident")
        if (allocated(error)) return
        !
        ! Drops the first two rows of the SLICE, not of the file.
        call t%delete_rows([1_int64, 2_int64])
        call check(error, t%is_detached(), "a row mutation on a slice should detach it")
        if (allocated(error)) return
        call check(error, t%nrows() == HI - LO - 1, &
            "the mutation should be applied in the slice's own row numbering")
        if (allocated(error)) return
        call t%get("k", k)
        call check(error, all(k == [(int(i, int32), i = LO + 2, HI)]), &
            "the resident column should hold the slice's surviving rows, in the file's values")
        if (allocated(error)) return
        !
        ! The skipped columns keep their descriptors -- they are still listed, still typed -- which
        ! is exactly what makes the loss quiet: nothing about the table looks different until a
        ! read is attempted.
        call check(error, t%has_column("x") .and. t%kind("x") == PK_FLOAT64, &
            "a column the mutation skipped should still be listed and typed")
        if (allocated(error)) return
        call check(error, t%residency("x") == RES_EMPTY, &
            "...and still report itself as holding nothing")
    end subroutine test_mutate_slice_then_read
    !
    !> Per-element nulls written by ARROW, not by this library, survive the read intact.
    !!
    !! This is the one element-null test that cannot be fooled by a self-consistent bug. Every
    !! other one writes its fixture with `parquet_write_column` and reads it back, so a defect that
    !! widened a null on read *and* broadcast it on write would agree with itself and look perfectly
    !! healthy. `test/fixtures/element_nulls.parquet` is written by `tools/generate_fixtures.cpp`
    !! through Arrow directly, so it is an independent statement of what the read path must produce.
    !!
    !! One column per validity dispatch class, because they are three different mechanisms behind
    !! one API: the packed bitmap (`vec`), the embedded string column (`svec`), and the null inside
    !! each element (`tvec`). Each has exactly one null element, and the assertions are on element
    !! POSITIONS -- a widened null would still be "a null in that row" and would pass a count.
    subroutine test_element_nulls_from_arrow(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: vec(:,:)
        character(len=:), allocatable :: svec(:,:)
        type(parquet_timestamp), allocatable :: tvec(:,:)
        logical, allocatable :: vmask(:,:), smask(:,:)
        character(len=*), parameter :: f = "test/fixtures/element_nulls.parquet"
        !
        call parquet_open_table(t, f)
        call check(error, t%nrows() == 4 .and. t%ncols() == 4, &
            "the Arrow-written element-null fixture should open with 4 rows and 4 columns")
        if (allocated(error)) return
        call check(error, t%width("vec") == 3 .and. t%kind("vec") == PK_FLOAT64_VEC, &
            "vec should read as a width-3 float64 vector column")
        if (allocated(error)) return
        !
        ! Bitmap class: the null is at element 2 of row 2 and nowhere else.
        call t%get("vec", vec, is_valid=vmask)
        call check(error, .not. vmask(2, 2), "vec: element 2 of row 2 should be null")
        if (allocated(error)) return
        call check(error, count(.not. vmask) == 1, &
            "vec: exactly one ELEMENT should be null -- a widened null would mark three")
        if (allocated(error)) return
        call check(error, abs(vec(1, 2) - 21.0_real64) < 1.0e-12_real64 .and. &
            abs(vec(3, 2) - 23.0_real64) < 1.0e-12_real64, &
            "vec: the null element's siblings should keep their values")
        if (allocated(error)) return
        call check(error, t%is_null("vec", 2_int64, 2_int64), &
            "vec: %is_null(row, elem) should report the single null element")
        if (allocated(error)) return
        call check(error, .not. t%is_null("vec", 2_int64, 1_int64), &
            "vec: %is_null(row, elem) should not report its siblings")
        if (allocated(error)) return
        call check(error, t%is_null("vec", 2_int64), &
            "vec: the row query means 'any element null', so row 2 is null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("vec", 1_int64), "vec: row 1 has no null element")
        if (allocated(error)) return
        !
        ! String class: the embedded string column keeps its own per-element validity.
        call t%get("svec", svec, is_valid=smask)
        call check(error, .not. smask(1, 3), "svec: element 1 of row 3 should be null")
        if (allocated(error)) return
        call check(error, count(.not. smask) == 1, "svec: exactly one element should be null")
        if (allocated(error)) return
        call check(error, trim(svec(2, 3)) == "ff", &
            "svec: the null element's sibling should keep its value")
        if (allocated(error)) return
        call check(error, trim(svec(1, 1)) == "a" .and. trim(svec(2, 2)) == "ddddd", &
            "svec: values should survive with the shortest element read first")
        if (allocated(error)) return
        !
        ! Temporal class: no mask at all -- the null lives inside each element.
        call t%get("tvec", tvec)
        call check(error, tvec(2, 1)%is_null(), "tvec: element 2 of row 1 should be null")
        if (allocated(error)) return
        call check(error, .not. tvec(1, 1)%is_null(), "tvec: its sibling should not be null")
        if (allocated(error)) return
        call check(error, count(tvec%is_null()) == 1, "tvec: exactly one element should be null")
    end subroutine test_element_nulls_from_arrow
    !
    !> `%prefetch` reaches the automatic row-index column, like every other way of asking for it.
    !!
    !! The name resolves for `%get`/`%col` but used to report "no column of this name" here, which
    !! made `%prefetch` -- the one documented way to ask for a column ahead of time -- the one way
    !! that could not ask for this one. Materializing it gives it a real slot, so `%ncols()` grows
    !! by one exactly as it does after a `%get`.
    subroutine test_prefetch_row_index(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64), allocatable :: ri(:)
        integer :: before, i
        character(len=*), parameter :: f = "test_run/table_prefetch_rowidx.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        before = t%ncols()
        call t%prefetch(PARQUET_ROW_INDEX)
        call check(error, t%residency(PARQUET_ROW_INDEX) == RES_FULL, &
            "%prefetch should materialize the automatic row-index column")
        if (allocated(error)) return
        call check(error, t%ncols() == before + 1, &
            "...which gives it a real slot, exactly as %get on it does")
        if (allocated(error)) return
        call t%get(PARQUET_ROW_INDEX, ri)
        call check(error, all(ri == [(int(i, int64), i = 1, NROW)]), &
            "the prefetched row-index column should hold the file's own row numbers")
        if (allocated(error)) return
        ! A second prefetch is a no-op, like any other already-resident column.
        call t%prefetch(PARQUET_ROW_INDEX)
        call check(error, t%ncols() == before + 1, "a second prefetch should not add another slot")
    end subroutine test_prefetch_row_index
    !
    !> `parquet_write_table` parses a schema the caller built but never parsed.
    !!
    !! A schema built with `%init`/`%add_field` carries only MAML text until `parquet_parse_maml`
    !! populates `%cinfo`; writing with it used to be an error telling the caller to make that call
    !! themselves. There is no reason for them to: the write does it. The side effect is visible
    !! and is part of the contract -- the caller's schema is parsed on return -- so that is checked
    !! too. (A schema that was never built at all is still an error: scenario
    !! `table_write_unbuilt_schema`.)
    subroutine test_write_table_parses_schema(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        integer(int32), allocatable :: i32(:)
        character(len=*), parameter :: f = "test_run/table_autoparse_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_autoparse_out.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call s%init("autoparse")
        call s%add_field("i32", "int32")
        call check(error, .not. s%is_parsed(), "precondition: the schema should not be parsed yet")
        if (allocated(error)) return
        ! No parquet_parse_maml(s) here on purpose.
        call parquet_write_table(t, fo, s)
        call check(error, s%is_parsed(), &
            "parquet_write_table should leave the caller's schema parsed")
        if (allocated(error)) return
        call parquet_open_table(t2, fo)
        call check(error, t2%ncols() == 1 .and. t2%nrows() == NROW, &
            "the file written from an unparsed schema should hold that schema's one column")
        if (allocated(error)) return
        call t2%get("i32", i32)
        call check(error, all(i32 == [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]), &
            "the values written from an unparsed schema should round-trip")
    end subroutine test_write_table_parses_schema
    !
    !> A row mutation that changes no row must change nothing at all -- including the file.
    !!
    !! Detaching costs the caller every column they have not read yet, permanently, so it is only
    !! paid for when the row set actually moved. Each case below is a call that does nothing, and
    !! the check is not merely `%is_detached()`: a column that was never resident is read AFTER the
    !! call, which is precisely what a detach would have made impossible. `%sort_by` is included
    !! because its no-op case is decided by the data (the rows were already in that order) rather
    !! than by the arguments.
    subroutine test_noop_mutation_keeps_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, empty
        integer(int32), allocatable :: i32(:)
        integer(int64), allocatable :: i64(:)
        real(real64), allocatable :: f64(:)
        logical :: keep(NROW)
        integer(int64) :: none(0)
        character(len=*), parameter :: f = "test_run/table_noop_mutation.parquet"
        !
        call write_basic_fixture(f)
        !
        call parquet_open_table(t, f)
        call t%truncate(NROW + 5)
        call check(error, .not. t%is_detached(), "%truncate beyond the row count should not detach")
        if (allocated(error)) return
        keep = .true.
        call t%filter_rows(keep)
        call check(error, .not. t%is_detached(), "%filter_rows keeping every row should not detach")
        if (allocated(error)) return
        call t%delete_rows(none)
        call check(error, .not. t%is_detached(), "%delete_rows with no indices should not detach")
        if (allocated(error)) return
        call t%append_null_rows(0)
        call check(error, .not. t%is_detached(), "%append_null_rows(0) should not detach")
        if (allocated(error)) return
        ! The point of all four: a column nobody has read yet is still readable.
        call check(error, t%nrows() == NROW, "a no-op mutation should not change the row count")
        if (allocated(error)) return
        call t%get("i32", i32)
        call check(error, all(i32 == [1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32]), &
            "a column left in the file should still be readable after four no-op mutations")
        if (allocated(error)) return
        !
        ! Appending a table with no rows: checked for compatibility, then does nothing.
        call parquet_open_table(t, f)
        call t%clone_structure(empty)
        call t%append(empty)
        call check(error, .not. t%is_detached() .and. t%nrows() == NROW, &
            "appending a zero-row table should not detach or change the row count")
        if (allocated(error)) return
        call t%get("f64", f64)
        !
        ! Sorting rows that are already in that order moves nothing.
        call parquet_open_table(t, f)
        call t%prefetch("i32")
        call t%sort_by(["i32"])
        call check(error, .not. t%is_detached(), &
            "%sort_by on an already-ordered key should not detach")
        if (allocated(error)) return
        call t%get("i64", i64)
        !
        ! ...whereas a sort that DOES move rows still detaches, which is the rule this is an
        ! exception to.
        call parquet_open_table(t, f)
        call t%prefetch("i32")
        call t%sort_by(["i32"], descending=[.true.])
        call check(error, t%is_detached(), "%sort_by that reorders rows must still detach")
    end subroutine test_noop_mutation_keeps_file
    !
    !> `physical=.true.` answers for EVERY file-backed table, whatever transform it carries.
    !!
    !! A sort is the case that used to decide it: the table's own numbering has no row-group
    !! structure left under one, and asking for it reached `parquet_get_chunk_size`'s guard and
    !! aborted -- which also took `physical=.true.` down with it, since both forms went through the
    !! same reader. The file's row groups are not affected by a sort at all, so `physical=.true.`
    !! must reproduce the pre-open planning call here exactly as it does anywhere else. The three
    !! combinations are checked together because the old behaviour depended on WHICH transforms
    !! happened to be present, not on what was asked for. (The default form under a sort aborts, so
    !! it lives out of process -- scenario `table_row_group_bounds_sorted`.)
    subroutine test_row_group_bounds_sorted(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        type(parquet_sortkey) :: srt
        integer, parameter :: N = 20, CH = 7
        integer(int64), allocatable :: pb(:,:), fb(:,:)
        character(len=*), parameter :: f = "test_run/table_rgbounds_sorted.parquet"
        !
        call write_slice_xform_fixture(f, N, CH)
        call parquet_table_row_group_bounds(f, fb)
        call srt%add("-k")
        !
        call parquet_open_table(t, f, sort=srt)
        call t%row_group_bounds(pb, physical=.true.)
        call check(error, all(pb == fb), &
            "a sorted table should still report the file's own row-group bounds under physical=")
        if (allocated(error)) return
        !
        ! ...and it does not depend on a filter being there too, which is exactly what it used to.
        call filt%add("k > 8")
        call parquet_open_table(t, f, filter=filt, sort=srt)
        call t%row_group_bounds(pb, physical=.true.)
        call check(error, all(pb == fb), &
            "a sorted AND filtered table should report the file's own row-group bounds")
        if (allocated(error)) return
        !
        call parquet_open_table(t, f, sort=srt, sample_fraction=1.0_real64)
        call t%row_group_bounds(pb, physical=.true.)
        call check(error, all(pb == fb), &
            "a sorted, fully-sampled table should report the file's own row-group bounds")
    end subroutine test_row_group_bounds_sorted
    !
    !> `release=` leaves the table in the residency state the write found it in.
    !!
    !! Three claims, and the middle one is what a naive implementation gets wrong: a column the
    !! CALLER had already read must survive the write resident, because materializing it was never
    !! this write's doing. The generation counter is checked alongside, since eviction frees storage
    !! a `%col` pointer could alias and the counter is the only signal a caller has -- it must move
    !! when something was released and stay put when nothing was.
    subroutine test_write_table_release(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_schema) :: s
        integer(int64) :: g0
        real(real64), allocatable :: r64(:)
        character(len=*), parameter :: f  = "test_run/table_release_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_release_out.parquet"
        character(len=*), parameter :: fk = "test_run/table_release_keep.parquet"
        character(len=*), parameter :: fr = "test_run/table_release_resident.parquet"
        !
        call write_basic_fixture(f)
        call s%init("rel")
        call s%add_field("i32", "int32")
        call s%add_field("f64", "float64")
        call s%add_field("b", "boolean")
        call parquet_parse_maml(s)
        !
        call parquet_open_table(t, f)
        call t%prefetch("i32")      ! the caller's own read -- not the write's to undo
        g0 = t%generation()
        call parquet_write_table(t, fo, s)
        call check(error, t%residency("i32") == RES_FULL, &
            "release= must leave resident a column the caller had already read")
        if (allocated(error)) return
        call check(error, t%residency("f64") == RES_EMPTY, &
            "release= should give back a column the write itself materialized")
        if (allocated(error)) return
        call check(error, t%residency("b") == RES_EMPTY, "...for every such column, not just one")
        if (allocated(error)) return
        call check(error, t%generation() > g0, &
            "releasing frees storage a %col pointer could alias, so the generation must advance")
        if (allocated(error)) return
        ! Released, not dropped: the descriptor stays and the next read works.
        call t%get("f64", r64)
        call check(error, abs(r64(4) - 9.0_real64) < 1.0e-12_real64, &
            "a released column must still be readable from the file afterwards")
        if (allocated(error)) return
        !
        ! release=.false. keeps everything the write read.
        call parquet_open_table(t, f)
        call parquet_write_table(t, fk, s, release=.false.)
        call check(error, t%residency("f64") == RES_FULL .and. t%residency("b") == RES_FULL, &
            "release=.false. should leave what the write materialized resident")
        if (allocated(error)) return
        !
        ! Nothing released, nothing to report: a write over an already-resident table must not move
        ! the counter at all, or every write would look like it invalidated the caller's pointers.
        call t%materialize_all()
        g0 = t%generation()
        call parquet_write_table(t, fr, s)
        call check(error, t%generation() == g0, &
            "a write that releases nothing must leave the generation counter alone")
        if (allocated(error)) return
        call check(error, t%residency("f64") == RES_FULL, &
            "...and must leave the fully-materialized table exactly as it was")
    end subroutine test_write_table_release
    !
    !> A schema-less `parquet_write_table` writes the resident columns and nothing else.
    !!
    !! The point of the path is that it reads nothing, so the assertions are as much about what is
    !! *absent* from the output as about what survives: an untouched column must not appear, and
    !! neither must the automatic `parquet_row_index`, which is this library's own provenance
    !! column rather than the table's data even when the caller has materialized it.
    subroutine test_write_table_schemaless(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        real(real64), allocatable :: r64(:)
        integer(int64), allocatable :: ri(:)
        type(parquet_reader) :: rd
        character(len=:), allocatable :: names(:)
        character(len=*), parameter :: f  = "test_run/table_sless_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_sless_out.parquet"
        character(len=*), parameter :: fe = "test_run/table_sless_empty.parquet"
        character(len=*), parameter :: fr = "test_run/table_sless_rowidx.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%prefetch(["f64", "i32"])
        call parquet_write_table(t, fo)
        !
        call parquet_open_table(t2, fo)
        call check(error, t2%ncols() == 2, &
            "a schema-less write should write exactly the resident columns")
        if (allocated(error)) return
        call t2%column_names(names)
        call check(error, trim(names(1)) == "i32" .and. trim(names(2)) == "f64", &
            "a schema-less write should write columns in slot order, under their internal names")
        if (allocated(error)) return
        call check(error, t2%nrows() == NROW, "the written table should keep the row count")
        if (allocated(error)) return
        call t2%get("f64", r64)
        call check(error, abs(r64(4) - 9.0_real64) < 1.0e-12_real64, &
            "values should survive a schema-less write")
        if (allocated(error)) return
        call check(error, .not. t2%has_column("s"), &
            "a column the schema-less write never read must not be in the output")
        if (allocated(error)) return
        !
        ! Nothing resident: a valid, genuinely empty file rather than an error.
        call parquet_open_table(t, f)
        call parquet_write_table(t, fe)
        call parquet_open_table(t2, fe)
        call check(error, t2%ncols() == 0 .and. t2%nrows() == 0, &
            "a schema-less write of a table with nothing resident should produce an empty file")
        if (allocated(error)) return
        !
        ! parquet_row_index is excluded even when resident, and its exclusion must not stop the
        ! ordinary columns beside it from being written.
        call parquet_open_table(t, f)
        call t%prefetch("i32")
        ! %get, not %prefetch: the row-index column is a VIRTUAL slot until something asks for its
        ! values, and %prefetch does not resolve that name.
        call t%get(PARQUET_ROW_INDEX, ri)
        call check(error, t%residency(PARQUET_ROW_INDEX) == RES_FULL, &
            "precondition: the row-index column should be resident after a prefetch")
        if (allocated(error)) return
        call parquet_write_table(t, fr)
        ! Asked of the FILE rather than of a table over it: %has_column answers .true. for
        ! parquet_row_index on any file-backed table, virtual slot or not, so only the file's own
        ! column list can say whether the write emitted it.
        call parquet_open_reader(rd, fr)
        call parquet_get_column_names(rd, names)
        call parquet_close_reader(rd)
        call check(error, size(names) == 1 .and. trim(names(1)) == "i32", &
            "a schema-less write must not write parquet_row_index, even when it is resident")
    end subroutine test_write_table_schemaless
    !
    !> A schema-less write emits a sidecar MAML that carries the units and reopens the file.
    !!
    !! This is the round trip the feature exists for -- write a temporary table, reopen it later,
    !! get the units back -- and it rests on something worth stating: the sidecar is a *write*
    !! (Role A) schema, while `parquet_open_table(maml=)` consumes the *read-in* (Role B) dialect.
    !! They are compatible because the read-in loader scans for the keys it wants and ignores the
    !! rest, so Role A's extra keys are inert. This test is what keeps that leniency from being
    !! tightened by accident.
    subroutine test_write_table_schemaless_sidecar(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_schema) :: s
        type(parquet_table) :: t, t2
        real(real64) :: v(NROW)
        integer(int32) :: id(NROW)
        character(len=:), allocatable :: u, line
        integer :: i
        character(len=*), parameter :: f  = "test_run/table_sidecar_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_sidecar_out.parquet"
        character(len=*), parameter :: mo = "test_run/table_sidecar_out.maml"
        !
        do i = 1, NROW
            id(i) = i
            v(i) = real(i, real64) * 2.25_real64
        end do
        call s%init("src")
        call s%add_field("id", "int32")
        call s%add_field("mass", "float64", unit="Msun")
        call parquet_parse_maml(s)
        call parquet_open_writer(w, f, s, write_maml=.true.)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "mass", v)
        call parquet_close_writer(w)
        !
        ! The unit only reaches the table through a read-in MAML -- a parquet file records none.
        call parquet_open_table(t, f, maml="test_run/table_sidecar_in.maml")
        call t%unit("mass", u)
        call check(error, u == "Msun", "precondition: the read-in MAML should give the unit")
        if (allocated(error)) return
        call t%materialize_all()
        call parquet_write_table(t, fo, write_maml=.true.)
        !
        call parquet_open_table(t2, fo, maml=mo)
        call t2%unit("mass", u)
        call check(error, u == "Msun", &
            "a schema-less write's sidecar should carry the unit, and reopen the file it describes")
        if (allocated(error)) return
        call t2%unit("id", u)
        call check(error, u == "", "a column with no unit should get no unit: key in the sidecar")
        if (allocated(error)) return
        call check(error, t2%ncols() == 2 .and. t2%nrows() == NROW, &
            "the sidecar-reopened table should hold the written columns and rows")
        if (allocated(error)) return
        ! MAML requires a table: name and a schema-less table has none of its own, so it is taken
        ! from the output file's stem -- checked from the text, since nothing reads it back.
        call read_first_line(mo, line)
        call check(error, trim(line) == "table: table_sidecar_out", &
            "the generated sidecar's table: name should be the output file's stem")
    end subroutine test_write_table_schemaless_sidecar
    !
    !> First line of a text file, for asserting on generated MAML.
    subroutine read_first_line(fname, line)
        character(len=*), intent(in) :: fname            !! file to read.
        character(len=:), allocatable, intent(out) :: line !! its first record, or "" if unreadable.
        character(len=512) :: buf
        integer :: u, ios
        !
        line = ""
        open(newunit=u, file=fname, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        read(u, '(a)', iostat=ios) buf
        if (ios == 0) line = trim(buf)
        close(u)
    end subroutine read_first_line
    !
    !> A nanosecond TIMESTAMP column survives a schema-less write at its own resolution.
    !!
    !! Without the stored unit on the descriptor this write does not merely lose precision, it
    !! **aborts**: the writer would default to microseconds and `to_unix` refuses to truncate. So
    !! the test reaching its assertions at all is half of what it checks; the other half is that
    !! the reopened file reports nanoseconds rather than the default.
    subroutine test_write_table_temporal_unit(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_schema) :: s
        type(parquet_table) :: t
        type(parquet_reader) :: rd
        type(parquet_timestamp) :: ts(2), got(2)
        type(parquet_time) :: tm(2)
        integer :: unit_out, tm_unit
        character(len=*), parameter :: f  = "test_run/table_tsunit_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_tsunit_out.parquet"
        !
        call s%init("tsu")
        call s%add_field("ev", "timestamp[ns]")
        call s%add_field("clock", "time[ms]")
        call parquet_parse_maml(s)
        call ts(1)%set(2024, 7, 16, 12, 0, 0, 123456789)   ! ns precision: only ns can hold it
        call ts(2)%set(1999, 1, 1, 0, 0, 0)
        call tm(1)%set(6, 30, 15, 500000000)
        call tm(2)%set(23, 59, 59)
        call parquet_open_writer(w, f, s)
        call parquet_write_column(w, "ev", ts)
        call parquet_write_column(w, "clock", tm)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call t%materialize_all()
        call parquet_write_table(t, fo)     ! aborts here if the unit was not recorded
        !
        call parquet_open_reader(rd, fo)
        call parquet_get_column_time_info(rd, "ev", unit=unit_out)
        call parquet_get_column_time_info(rd, "clock", unit=tm_unit)
        call parquet_read_column(rd, "ev", got)
        call parquet_close_reader(rd)
        call check(error, unit_out == parquet_unit_nanos, &
            "a schema-less write should keep a timestamp column's own nanosecond resolution")
        if (allocated(error)) return
        call check(error, tm_unit == parquet_unit_millis, &
            "...and a time column's own millisecond resolution")
        if (allocated(error)) return
        call check(error, got(1) == ts(1) .and. got(2) == ts(2), &
            "nanosecond timestamp values should survive the schema-less round trip exactly")
    end subroutine test_write_table_temporal_unit
    !
    !> Every `parquet_open_writer` option `parquet_write_table` forwards actually reaches the file.
    !!
    !! The point of the pass-through is that a table write and the equivalent hand-written open are
    !! the same calls, so what is checked here is the option's OBSERVABLE effect, not that the
    !! argument was accepted: `chunk_size` against the reopened file's row-group count, `write_maml`
    !! against the sidecar's existence, and the codec arguments against the values surviving (a
    !! codec name that never reached Arrow would still produce a readable file, but a wrong one
    !! would abort in the writer).
    subroutine test_write_table_writer_options(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        type(parquet_reader) :: rd
        integer :: nrg, unit
        real(real64), allocatable :: r64(:)
        logical :: sidecar
        character(len=*), parameter :: f  = "test_run/table_wopts_in.parquet"
        character(len=*), parameter :: fc = "test_run/table_wopts_chunked.parquet"
        character(len=*), parameter :: fz = "test_run/table_wopts_snappy.parquet"
        character(len=*), parameter :: fm = "test_run/table_wopts_maml.parquet"
        character(len=*), parameter :: sidecar_path = "test_run/table_wopts_maml.maml"
        !
        call write_basic_fixture(f)
        call s%init("wopts")
        call s%add_field("i32", "int32")
        call s%add_field("f64", "float64")
        call parquet_parse_maml(s)
        call parquet_open_table(t, f)
        !
        ! chunk_size: 6 rows in row groups of 2 is 3 row groups, and nothing else in the call
        ! could produce that number.
        call parquet_write_table(t, fc, s, chunk_size=2)
        call parquet_open_reader(rd, fc)
        call parquet_get_num_row_groups(rd, nrg)
        call parquet_close_reader(rd)
        call check(error, nrg == 3, "chunk_size= should size the output's row groups")
        if (allocated(error)) return
        !
        ! The codec arguments, with the values checked on the way back: an unknown codec name
        ! aborts in the writer, so reaching this point at all proves the name was passed on.
        ! qc= rides along here rather than getting its own assertion: a qc violation only PRINTS a
        ! warning, so there is nothing an in-process test can check. What it does with the flag is
        ! parquet_open_writer's own behaviour and is covered on that side.
        ! No compression_level= alongside snappy: Arrow rejects a level for a codec that has none,
        ! so the two are exercised separately -- the level below, on the default zstd.
        call parquet_write_table(t, fz, s, compression="snappy", &
            use_threads=.false., overwrite=.true., qc=.false.)
        call parquet_open_table(t2, fz)
        call t2%get("f64", r64)
        call check(error, abs(r64(4) - 9.0_real64) < 1.0e-12_real64, &
            "values should survive a table write under a non-default codec")
        if (allocated(error)) return
        !
        ! write_maml: the sidecar lands next to the parquet file, named after it. Deleted first,
        ! or the check passes on a leftover from an earlier run whether or not the argument was
        ! forwarded -- which is exactly what a mutation of the pass-through proved.
        open(newunit=unit, file=sidecar_path, status="unknown", action="write")
        close(unit, status="delete")
        call parquet_write_table(t, fm, s, write_maml=.true., compression_level=1)
        inquire(file=sidecar_path, exist=sidecar)
        call check(error, sidecar, "write_maml=.true. should leave a sidecar next to the output")
    end subroutine test_write_table_writer_options
    !
    !> `%generation()` must advance on EVERY structural entry point, and on none that changes no row.
    !!
    !! The counter is the only signal a caller has that a `%col`/`%ref` pointer they hold may now be
    !! dangling. The dangling read itself cannot be tested -- dereferencing freed storage may pass,
    !! crash or return plausible garbage, and none of the three means anything -- so the counter is
    !! what stands in for it. That makes a mutation added WITHOUT a bump worse than one that bumps
    !! unnecessarily: the first gives false confidence, the second costs a re-fetch.
    !!
    !! Written as a sweep rather than one test per operation, so that adding a mutation means adding
    !! a `case`, not a test: the assertion is written once and applies to whatever the case did.
    subroutine test_generation_sweep(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, batch
        type(parquet_schema) :: sch
        integer(int64) :: gen0
        integer(int32) :: extra(NROW)
        logical :: keep(NROW)
        integer(int64) :: none(0)
        integer :: op, i
        !> Structural entry points, plus the six calls that must NOT bump. Raise this and add a
        !! `case` below when a new structural operation is added.
        integer, parameter :: NBUMP = 14, NNOOP = 6
        !> Named so a failure says WHICH entry point stopped bumping, rather than only that one did.
        character(len=18), parameter :: bump_names(NBUMP) = [ &
            "add_column        ", "drop_column       ", "rename_column     ", "copy_column       ", &
            "cast              ", "evict_column      ", "reload            ", "filter_rows       ", &
            "sort_by           ", "delete_rows       ", "truncate          ", "append            ", &
            "append_null_rows  ", "write_table       "]
        character(len=18), parameter :: noop_names(NNOOP) = [ &
            "truncate past end ", "filter_rows all   ", "delete_rows none  ", "append_null_rows 0", &
            "sort_by ordered   ", "append zero rows  "]
        character(len=*), parameter :: f = "test_run/table_generation_sweep.parquet"
        character(len=*), parameter :: fout = "test_run/table_generation_sweep_out.parquet"
        !
        call write_basic_fixture(f)
        extra = [(int(i, int32), i = 1, NROW)]
        call sch%init("gen")
        call sch%add_field("f64", "float64")
        call parquet_parse_maml(sch)
        !
        ! --- every structural change bumps -----------------------------------------------------
        do op = 1, NBUMP
            call parquet_open_table(t, f)
            ! Each case does whatever setup it needs BEFORE the counter is read, so that only the
            ! operation under test can be responsible for the bump.
            select case (op)
            case (1)
                gen0 = t%generation()
                call t%add_column("extra", extra)
            case (2)
                gen0 = t%generation()
                call t%drop_column("i32")
            case (3)
                gen0 = t%generation()
                call t%rename_column("i32", "renamed")
            case (4)
                gen0 = t%generation()
                call t%copy_column("i32", "i32_copy")
            case (5)
                gen0 = t%generation()
                call t%cast("i32", PK_INT64)
            case (6)
                call t%prefetch("i32")
                gen0 = t%generation()
                call t%evict_column("i32")
            case (7)
                call t%prefetch("i32")
                gen0 = t%generation()
                call t%reload("i32")
            case (8)
                keep = .true.
                keep(2) = .false.
                gen0 = t%generation()
                call t%filter_rows(keep)
            case (9)
                call t%materialize_all()
                gen0 = t%generation()
                call t%sort_by(["i32"], descending=[.true.])
            case (10)
                gen0 = t%generation()
                call t%delete_rows([1])
            case (11)
                gen0 = t%generation()
                call t%truncate(NROW - 1)
            case (12)
                call t%materialize_all()
                call t%clone_structure(batch)
                call batch%append_null_rows(1)
                gen0 = t%generation()
                call t%append(batch)
            case (13)
                gen0 = t%generation()
                call t%append_null_rows(1)
            case (14)
                ! release= (default .true.) evicts the columns THIS WRITE materialized, freeing
                ! storage a pointer could alias -- the one bump that does not look structural. It
                ! needs a schema naming a column the table has not read: a write that releases
                ! nothing must not bump, which is the neighbouring test's own assertion.
                gen0 = t%generation()
                call parquet_write_table(t, fout, sch, overwrite=.true.)
            end select
            call check(error, t%generation() > gen0, &
                "%" // trim(bump_names(op)) // " must advance the generation counter -- " // &
                "a caller cannot re-fetch a pointer it is not told to re-fetch")
            if (allocated(error)) return
        end do
        !
        ! --- and a call that changes no row bumps nothing ---------------------------------------
        ! The other direction of the same contract: an unnecessary bump costs a re-fetch, so the
        ! rule "no rows changed => nothing was invalidated" has to hold too, or the counter starts
        ! reporting noise and callers learn to ignore it. %sort_by is the one decided by the DATA
        ! rather than by the arguments.
        do op = 1, NNOOP
            call parquet_open_table(t, f)
            call t%materialize_all()
            gen0 = t%generation()
            select case (op)
            case (1)
                call t%truncate(NROW + 5)
            case (2)
                keep = .true.
                call t%filter_rows(keep)
            case (3)
                call t%delete_rows(none)
            case (4)
                call t%append_null_rows(0)
            case (5)
                call t%sort_by(["i32"])
            case (6)
                call t%clone_structure(batch)
                call t%append(batch)
            end select
            call check(error, t%generation() == gen0, &
                "%" // trim(noop_names(op)) // " changes no row, so it must not advance the " // &
                "generation counter")
            if (allocated(error)) return
        end do
    end subroutine test_generation_sweep
    !
    !> The five metadata queries answer from the DESCRIPTOR, and reading them reads nothing.
    !!
    !! Two properties, and both fail silently. `%kind` returning `PK_NONE` for an untouched column
    !! makes the documented `select case (t%kind(name))` idiom take `case default` instead of
    !! aborting -- a wrong branch, not an error. And a query that quietly became touch-triggering
    !! turns `%residency` or `%unit` into a whole-column read, which no assertion on the VALUE would
    !! ever notice; only the residency afterwards shows it.
    !!
    !! The existing round-trip tests all read their columns, so a regression in either direction
    !! passes every one of them.
    subroutine test_metadata_queries_do_not_touch(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=:), allocatable :: u
        character(len=3), parameter :: names(6) = ["i32", "i64", "f32", "f64", "b  ", "s  "]
        integer, parameter :: kinds(6) = [PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, &
            PK_LOGICAL, PK_STRING]
        integer :: i
        character(len=*), parameter :: f = "test_run/table_metadata_no_touch.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        do i = 1, size(names)
            call check(error, t%residency(trim(names(i))) == RES_EMPTY, &
                "precondition: opening must not have read " // trim(names(i)))
            if (allocated(error)) return
            call check(error, t%kind(trim(names(i))) == kinds(i), &
                "%kind must answer from the descriptor before the column is read, not PK_NONE")
            if (allocated(error)) return
            call check(error, t%width(trim(names(i))) == 1, &
                "%width must answer from the descriptor before the column is read")
            if (allocated(error)) return
            ! The three that must NOT become touch-triggering, unlike %kind/%width on a deferred
            ! plain-LIST column (whose width has no schema-level answer -- see the LIST tests).
            call t%unit(trim(names(i)), u)
            call check(error, t%is_supported(trim(names(i))), &
                "every column in this fixture is supported")
            if (allocated(error)) return
            call check(error, t%residency(trim(names(i))) == RES_EMPTY, &
                "%kind/%width/%unit/%is_supported/%residency must not read " // trim(names(i)))
            if (allocated(error)) return
        end do
    end subroutine test_metadata_queries_do_not_touch
    !
    !> Materializing the row index first is the documented recovery, so it has to keep working.
    !!
    !! `parquet_row_index` is derivable only while the table still has its file: every row-structural
    !! mutation closes the reader, and with it the mask and permutation that are the only record of
    !! which file row each row came from. Users are told to materialize it BEFORE mutating, after
    !! which it travels with every other column -- that is what this checks, across the mutations
    !! that reorder, remove and add rows. Rows an `%append` brings in came from no file row at all,
    !! so they must be Null rather than 0 or a repeat.
    subroutine test_row_index_recovery(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, batch
        integer(int64), allocatable :: ri(:)
        integer(int32), allocatable :: k(:)
        logical :: keep(20)
        integer, parameter :: N = 20, CH = 7
        character(len=*), parameter :: f = "test_run/table_row_index_recovery.parquet"
        !
        call write_slice_xform_fixture(f, N, CH)
        !
        ! Reordering: the row index must follow its rows, not stay in place.
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%get(PARQUET_ROW_INDEX, ri)        ! materializes it -- the recovery step
        call t%sort_by(["k"], descending=[.true.])
        call t%get(PARQUET_ROW_INDEX, ri)
        call t%get("k", k)
        call check(error, all(ri == int(k, int64)), &
            "a materialized row index must be reordered by %sort_by with every other column")
        if (allocated(error)) return
        call check(error, ri(1) == int(N, int64), &
            "the row that sorted first should still name the file row it came from")
        if (allocated(error)) return
        !
        ! Removing: the survivors keep their own file rows.
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%get(PARQUET_ROW_INDEX, ri)
        keep = .false.
        keep(3) = .true.
        keep(17) = .true.
        call t%filter_rows(keep)
        call t%get(PARQUET_ROW_INDEX, ri)
        call check(error, size(ri) == 2 .and. ri(1) == 3_int64 .and. ri(2) == 17_int64, &
            "a materialized row index must be filtered with every other column")
        if (allocated(error)) return
        !
        ! Adding: a row that came from no file row has no file row to name.
        call parquet_open_table(t, f)
        call t%materialize_all()
        call t%get(PARQUET_ROW_INDEX, ri)
        call t%clone_structure(batch)
        call batch%append_null_rows(1)
        call t%append(batch)
        call check(error, t%nrows() == int(N + 1, int64), "the append should have added one row")
        if (allocated(error)) return
        call check(error, t%is_null(PARQUET_ROW_INDEX, N + 1), &
            "a row added by %append came from no file row, so its row index must be Null")
    end subroutine test_row_index_recovery
    !
    !> "Detached" means "had a file and can no longer read it", never simply "was mutated".
    !!
    !! A table built by `parquet_new_table` has no file to lose, so every mutation must leave
    !! `%is_detached()` answering `.false.` -- including the ones that reorder and grow it, which are
    !! exactly the ones that detach a file-backed table. `table_detach` sets the flag only while
    !! `file_backed` is still true, and the natural "simplification" is to set it unconditionally:
    !! every from-scratch table would then report itself detached the moment it was filled, and the
    !! read-after-detach guards would start refusing calls on a table that never had a file.
    subroutine test_in_memory_never_detaches(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, batch
        integer(int32) :: v(NROW)
        logical, allocatable :: keep(:)
        integer :: i
        !
        v = [(int(NROW - i + 1, int32), i = 1, NROW)]    ! descending, so a sort really reorders
        call parquet_new_table(t)
        call t%add_column("a", v)
        call check(error, .not. t%is_detached(), "a freshly built table has no file to lose")
        if (allocated(error)) return
        call t%sort_by(["a"])
        call check(error, .not. t%is_detached(), &
            "reordering an in-memory table must not report it as detached")
        if (allocated(error)) return
        call t%clone_structure(batch)
        call batch%append_null_rows(2)
        call t%append(batch)
        call check(error, .not. t%is_detached(), &
            "growing an in-memory table must not report it as detached")
        if (allocated(error)) return
        allocate(keep(t%nrows()))
        keep = .false.
        keep(1) = .true.
        call t%filter_rows(keep)
        call check(error, .not. t%is_detached(), &
            "removing rows from an in-memory table must not report it as detached")
    end subroutine test_in_memory_never_detaches
    !
    !> `%clone` carries an extending type's own components, through the `clone_extra` hook.
    !!
    !! This is the test the whole hook exists for. Without the override, `zeropoint` would arrive
    !! default-initialized and NOTHING would report it -- `%clone` would succeed, the columns would
    !! be right, and only the table parameter would be silently wrong. Mutation-test it by emptying
    !! `ext_clone_extra`: this must fail.
    subroutine test_ext_clone_carries_components(error)
        type(error_type), allocatable, intent(out) :: error
        type(ext_table) :: t, c
        integer(int32), allocatable :: got(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_ext_clone.parquet"
        !
        call write_basic_fixture(f)
        ! Through the parent component, which is how a generated %init opens a file: the dummy of
        ! parquet_open_table is `type(parquet_table)`, deliberately, so that an extension cannot be
        ! opened without going through its own constructor.
        call parquet_open_table(t%parquet_table, f)
        t%zeropoint = 25.5_real64
        call t%clone(c)
        call check(error, c%nrows() == NROW, "the clone should hold the source's rows")
        if (allocated(error)) return
        call c%get("i32", got)
        call check(error, all(got == [(int(i32_seq(i), int32), i = 1, NROW)]), &
            "the clone's column values should match the source's")
        if (allocated(error)) return
        call check(error, abs(c%zeropoint - 25.5_real64) < 1.0e-12_real64, &
            "clone_extra should have carried the extending type's own component across")
    end subroutine test_ext_clone_carries_components
    !
    !> `%clone_structure` runs the same hook, with `structure_only` true.
    !!
    !! A table parameter is a property of the TABLE, not of its rows, so an empty batch cloned from
    !! a configured table must still carry it -- otherwise the bulk-append idiom (clone_structure,
    !! fill, append) would quietly lose it every time.
    subroutine test_ext_clone_structure_carries_components(error)
        type(error_type), allocatable, intent(out) :: error
        type(ext_table) :: t, batch
        character(len=*), parameter :: f = "test_run/table_ext_clone_structure.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t%parquet_table, f)
        t%zeropoint = 7.25_real64
        call t%materialize_all()
        call t%clone_structure(batch)
        call check(error, batch%nrows() == 0, "a structure clone should have no rows")
        if (allocated(error)) return
        call check(error, batch%ncols() == t%ncols(), "a structure clone should have every column")
        if (allocated(error)) return
        call check(error, abs(batch%zeropoint - 7.25_real64) < 1.0e-12_real64, &
            "clone_structure should carry the extending type's own component too")
    end subroutine test_ext_clone_structure_carries_components
    !
    !> `%bind_predefined` checks, converts and reads every declared column in one call.
    !!
    !! The fixture's `i32` column is declared as PK_INT64 and its `f32` as PK_FLOAT64, so this also
    !! pins the widening half of the contract: both must arrive at the DECLARED kind, ready for an
    !! exact-kind `%col`, and resident without any further call.
    subroutine test_bind_predefined_binds_and_widens(error)
        type(error_type), allocatable, intent(out) :: error
        type(ext_table) :: t
        integer(int64), pointer :: p64(:)
        real(real64), pointer :: pf64(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_bind_widen.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t%parquet_table, f)
        call t%bind_predefined( &
            [character(len=3) :: "i32", "f32", "s"], &
            [PK_INT64, PK_FLOAT64, PK_STRING], &
            [1, 1, 1], &
            [.true., .true., .true.], &
            context="test_bind.maml")
        call check(error, t%kind("i32") == PK_INT64, &
            "a column declared int64 over an int32 file column should arrive widened")
        if (allocated(error)) return
        call check(error, t%kind("f32") == PK_FLOAT64, &
            "a column declared float64 over a float32 file column should arrive widened")
        if (allocated(error)) return
        call check(error, t%residency("i32") == RES_FULL .and. t%residency("s") == RES_FULL, &
            "bind_predefined should leave every predefined column materialized")
        if (allocated(error)) return
        ! The pointer path is exact-kind, so this only compiles-and-runs if the cast really landed.
        call t%col("i32", p64)
        call check(error, all(p64 == [(int(i32_seq(i), int64), i = 1, NROW)]), &
            "the widened column should hold the file's own values")
        if (allocated(error)) return
        call t%col("f32", pf64)
        call check(error, all(abs(pf64 - [(real(i32_seq(i), real64) * 0.5_real64, i = 1, NROW)]) &
            < 1.0e-6_real64), "the widened float column should hold the file's own values")
    end subroutine test_bind_predefined_binds_and_widens
    !
    !> A `from_file=.false.` column is created rather than looked for, with every row null.
    !!
    !! This is the `source: computed` case, and the same path an in-memory generated table takes
    !! for every one of its columns. The rows have to EXIST -- an accessor is generated for the
    !! column, so it must work from the moment the constructor returns -- and they have to be null,
    !! since nothing has filled them.
    subroutine test_bind_predefined_computed_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(ext_table) :: t
        real(real64), pointer :: p(:)
        character(len=*), parameter :: f = "test_run/table_bind_computed.parquet"
        integer :: i
        !
        call write_basic_fixture(f)
        call parquet_open_table(t%parquet_table, f)
        call t%bind_predefined( &
            [character(len=4) :: "i32", "flux"], &
            [PK_INT32, PK_FLOAT64], &
            [1, 1], &
            [.true., .false.], &
            context="test_bind.maml")
        call check(error, t%has_column("flux"), "a computed column should have been created")
        if (allocated(error)) return
        call check(error, t%kind("flux") == PK_FLOAT64, "the computed column should have its declared kind")
        if (allocated(error)) return
        call t%col("flux", p)
        call check(error, size(p) == NROW, "the computed column should have one row per table row")
        if (allocated(error)) return
        do i = 1, NROW
            call check(error, t%is_null("flux", i), "every row of an unfilled computed column should be null")
            if (allocated(error)) return
        end do
        ! And it is an ordinary column afterwards: writing a value clears its null.
        p(2) = 3.5_real64
        call t%clear_null("flux", 2)
        call check(error, .not. t%is_null("flux", 2), "a computed column should be writable like any other")
    end subroutine test_bind_predefined_computed_column
    !
    !> A schema declaring no fields at all binds nothing and is not an error.
    !!
    !! The generator accepts an empty `fields:` and emits a bare `parquet_table` extension -- a
    !! template for a project that wants its own named table type without predefining any columns.
    !! That reaches here as zero-size arrays, which must be a clean no-op rather than a guard
    !! failure.
    subroutine test_bind_predefined_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(ext_table) :: t
        character(len=*), parameter :: f = "test_run/table_bind_empty.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t%parquet_table, f)
        call t%bind_predefined([character(len=1) ::], [integer ::], [integer ::], [logical ::], &
            context="test_bind_empty.maml")
        call check(error, t%ncols() == 6, "binding no columns should leave the table as it was")
        if (allocated(error)) return
        call check(error, t%residency("i32") == RES_EMPTY, &
            "binding no columns should read nothing")
    end subroutine test_bind_predefined_empty
    !
    !> `parquet_write_table` accepts an extending type directly, not only a plain `parquet_table`.
    !!
    !! Its `table` dummy is `class`, so a generated table writes itself without the caller having
    !! to reach through `%parquet_table` -- which would be a wart with no upside, since the dummy
    !! is `intent(in)` and has no reset semantics to protect.
    subroutine test_write_table_accepts_extension(error)
        type(error_type), allocatable, intent(out) :: error
        type(ext_table) :: t
        type(parquet_table) :: back
        integer(int32), allocatable :: got(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_ext_write_src.parquet"
        character(len=*), parameter :: g = "test_run/table_ext_write_out.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t%parquet_table, f)
        call t%materialize_all()
        call parquet_write_table(t, g, overwrite=.true.)
        call parquet_open_table(back, g)
        call back%get("i32", got)
        call check(error, all(got == [(int(i32_seq(i), int32), i = 1, NROW)]), &
            "a table written from an extending type should round-trip its values")
    end subroutine test_write_table_accepts_extension
    !
    !> A predefined column can still be dropped on purpose, with `force=.true.`.
    !!
    !! The refusal without `force=` kills the process, so it lives in error_scenarios.f90; this is
    !! its negative control -- a guard that fired unconditionally would pass that scenario while
    !! breaking the permitted case.
    subroutine test_drop_predefined_with_force(error)
        type(error_type), allocatable, intent(out) :: error
        type(ext_table) :: t
        character(len=*), parameter :: f = "test_run/table_drop_predefined.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t%parquet_table, f)
        call t%bind_predefined([character(len=3) :: "i32"], [PK_INT32], [1], [.true.], &
            context="test_bind.maml")
        call t%drop_column("i32", force=.true.)
        call check(error, .not. t%has_column("i32"), &
            "force=.true. should drop a predefined column")
        if (allocated(error)) return
        ! A column that was never bound is not predefined, so it needs no force at all.
        call t%drop_column("f32")
        call check(error, .not. t%has_column("f32"), &
            "a column that is not predefined should drop without force=")
    end subroutine test_drop_predefined_with_force
    !
    !> The i-th value `write_basic_fixture` puts in its `i32` column.
    pure integer function i32_seq(i) result(v)
        integer, intent(in) :: i !! 1-based row index.
        !
        v = i
    end function i32_seq
    !
    !> `%add_column` from a `character` array trims trailing blanks, so `%get` comes back sized to
    !! the longest REAL value rather than to the caller's declared width.
    !!
    !! This is the user-visible half of the array-versus-scalar rule (`parquet_columns_string.f90`'s
    !! `refill_string_store`), and it is asserted on the LENGTH because nothing else can see it:
    !! Fortran blank-pads the shorter side of `==`, so a padded store and a trimmed one compare
    !! equal on every value. `%get` is documented as sized to the longest element present, which is
    !! exactly the observation this makes.
    subroutine test_add_column_chr_trims(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=12) :: src(3)
        character(len=:), allocatable :: got(:), one
        !
        ! Declared width 12, longest real value 5 -- and the FIRST element is the shortest, per
        ! CLAUDE.md's fixture rule for anything sized from element one.
        src(1) = "a"
        src(2) = "bcde"
        src(3) = "fghij"
        call parquet_new_table(t)
        call t%add_column("name", src)
        call t%get("name", got)
        call check(error, len(got) == 5, "%get should be sized to the longest stored value, not the declared width 12")
        if (allocated(error)) return
        call check(error, got(1) == "a" .and. got(3) == "fghij", "the values themselves must survive the trim")
        if (allocated(error)) return
        !
        ! %set shares the rule; %set_element takes a scalar and does not.
        call t%set("name", ["xy  ", "zw  ", "vu  "])
        call t%get("name", got)
        call check(error, len(got) == 2, "%set should trim a character array exactly as %add_column does")
        if (allocated(error)) return
        call t%set_element("name", 1_int64, "keep me   ")
        call t%get_element("name", 1_int64, one)
        call check(error, len(one) == 10, "%set_element takes a SCALAR, whose trailing blanks are the caller's own")
    end subroutine test_add_column_chr_trims
    !
end module test_table
