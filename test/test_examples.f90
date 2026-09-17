!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Compiles and runs the code examples shown in README.md and the doc/pages
!> user guide, so that a change to the library's public API that silently
!> breaks a documented example is caught here rather than by a user
!> copy-pasting the guide.
!>
!> Each subroutine below mirrors one documented example as closely as possible
!> (same `use` clauses, same variable declarations, same calls), except that
!> it writes to a path under test_run/ instead of the doc's "data.parquet",
!> and is wrapped as a subroutine instead of a standalone `program` so it can
!> be driven by test-drive and checked with a round-trip read-back.
module test_examples
    use parquet
    use parquet_table_example, only : parquet_table_test
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_examples

    !> `(x - 2)**2` capped at `x <= 1`, for the facade test's `pf_minimize_cobyla` call.
    !!
    !! `pf_constrained_objective` is reached through `use parquet` alone here, which is what
    !! proves the facade re-exports it: without it this type would not compile.
    type, extends(pf_constrained_objective) :: facade_capped_square
    contains
        procedure :: eval => facade_capped_eval             !! `(x - 2)**2`.
        procedure :: n_constraints => facade_capped_count   !! one constraint.
        procedure :: constraints => facade_capped_constr    !! `x - 1 <= 0`.
    end type facade_capped_square
    !
contains
    !
    !> `(x - 2)**2`.
    function facade_capped_eval(this, x) result(f)
        class(facade_capped_square), intent(inout) :: this !! the objective
        real(real64), intent(in)                   :: x(:) !! the point
        real(real64)                               :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2

    end function facade_capped_eval
    !
    !> How many constraint values `constraints` fills.
    function facade_capped_count(this) result(m)
        class(facade_capped_square), intent(in) :: this !! the objective
        integer                                 :: m    !! one

        m = 1

    end function facade_capped_count
    !
    !> `x - 1 <= 0`.
    subroutine facade_capped_constr(this, x, c)
        class(facade_capped_square), intent(inout) :: this !! the objective
        real(real64), intent(in)                   :: x(:) !! the point
        real(real64), intent(out)                  :: c(:) !! exactly `n_constraints()` values

        c(1) = x(1) - 1.0_real64

    end subroutine facade_capped_constr
    !
    subroutine collect_tests_parquet_examples(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("README.md quick_example", test_readme_quick_example), &
            new_unittest("schema-driven write: only enabled columns are written", &
                test_schema_driven_write_enabled), &
            new_unittest("doc/pages/schema/combined-example.md example", test_combined_example), &
            new_unittest("doc/pages/schema/building-schema-in-code.md build_schema example", &
                test_build_schema_example), &
            new_unittest("maml_example2 writer produces a matching sidecar .maml", &
                test_maml_example2_sidecar_keyarray), &
            new_unittest("doc/pages/types/date-time.md datetime_quickstart example", test_datetime_quickstart_example), &
            new_unittest("doc/pages/schema/combined-example.md write_parquet_qc_example", &
                test_combined_example_qc_example), &
            new_unittest("doc/pages/schema/quality-control.md qc_read_example", test_qc_read_example), &
            new_unittest("doc/pages/types/supported-data-types.md null_values_example", &
                test_null_values_example), &
            new_unittest("doc/pages/types/string-columns.md strings_quickstart example", &
                test_strings_quickstart_example), &
            new_unittest("doc/pages/types/string-columns.md token_column example", test_token_column_example), &
            new_unittest("doc/pages/utilities/sorting.md worked examples", test_sorting_page_examples), &
            new_unittest("doc/pages/utilities/random.md random_quickstart example", &
                test_random_quickstart_example), &
            new_unittest("doc/pages/utilities/generated-tables.md generated_table_quickstart example", &
                test_generated_table_quickstart_example), &
            new_unittest("doc/pages/tables/table.md mean_mass example", test_mean_mass_example), &
            new_unittest("doc/pages/tables/table-open.md per-thread slices example", &
                test_per_thread_slices_example), &
            new_unittest("doc/pages/tables/table-write.md build_and_write example", &
                test_build_and_write_example), &
            new_unittest("doc/pages/tables/table-mutate.md rank_by_flux example", &
                test_rank_by_flux_example), &
            new_unittest("doc/pages/operating/choosing-a-module.md narrow_import example", &
                test_narrow_import_example), &
            new_unittest("doc/pages/operating/performance.md three_ways example", &
                test_three_ways_example), &
            new_unittest("doc/pages/operating/error-handling.md found_or_abort example", &
                test_found_or_abort_example), &
            new_unittest("use parquet alone reaches every layer of the library", test_facade_covers_every_layer) &
            ]
    end subroutine collect_tests_parquet_examples

    !> Mirrors doc/pages/utilities/generated-tables.md's opening `generated_table_quickstart`
    !> example, asserting the three values its trailing comment shows.
    !>
    !> The example is deliberately in-memory (`%init_empty`, no file), which is what lets it be
    !> mirrored here with no fixture at all -- and therefore with no chance of the concurrent-suite
    !> fixture collision CLAUDE.md warns about.
    !>
    !> **The negative control is the second table.** Asserting only `20.291667` would pass against
    !> a `%set` that wrote nothing and a `sum` over three zeros divided by three -- so a second
    !> table built with `%init_empty(2)` and two of the same values must give a DIFFERENT mean.
    !> That is what makes this a test of the data rather than of the arithmetic. The `%is_null`
    !> assertions are the other half: the page states that `%init_empty`'s rows start null and that
    !> `%set` clears them, and without those two lines the example would read the same whether or
    !> not the values were ever marked valid.
    subroutine test_generated_table_quickstart_example(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_test) :: t, control
        real(real64), pointer :: ra(:), one
        real(real64) :: mean, control_mean
        !
        call t%init_empty(3)
        call check(error, t%is_null("ra", 1), &
            "generated-tables.md: %init_empty's rows start null")
        if (allocated(error)) return
        call t%set("uberid", [101_int64, 102_int64, 103_int64])
        call t%set("ra", [10.5_real64, 20.25_real64, 30.125_real64])
        call check(error, .not. t%is_null("ra", 1), "generated-tables.md: %set clears the null")
        if (allocated(error)) return
        !
        ra => t%ra()
        one => t%ra(2)
        call check(error, t%nrows() == 3_int64, "generated-tables.md: nrows is 3")
        if (allocated(error)) return
        call check(error, abs(one - 20.25_real64) < 1.0e-12_real64, &
            "generated-tables.md: %ra(2) is 20.25")
        if (allocated(error)) return
        mean = sum(ra) / t%nrows()
        call check(error, abs(mean - 20.291666666666668_real64) < 1.0e-9_real64, &
            "generated-tables.md: sum(ra) / t%nrows() is 20.291667")
        if (allocated(error)) return
        !
        ! The control: two of the same values, so a mean that came from the data must differ.
        call control%init_empty(2)
        call control%set("ra", [10.5_real64, 20.25_real64])
        control_mean = sum(control%ra()) / control%nrows()
        call check(error, abs(control_mean - mean) > 1.0e-6_real64, &
            "generated-tables.md: a different table must give a different mean")
    end subroutine test_generated_table_quickstart_example

    !> Mirrors every worked example on doc/pages/utilities/sorting.md that prints a concrete
    !> result, asserting the exact values the page shows in its trailing comments.
    !>
    !> Written because that page published a WRONG result for a long time: its
    !> `call pf_rank(v, r)` example showed `[3, 1, 2, 1, 3, 3, 4, 2]`, which is the *dense*
    !> ranking, while `pf_rank`'s default method is "competition" and answers
    !> `[5, 1, 3, 1, 5, 5, 8, 3]`. Nothing caught it: no example from the whole utilities/ group
    !> was mirrored here. The rank block below is therefore the reason this test exists, and the
    !> `method="dense"` call beside it is its negative control -- without that second call the
    !> test would pass just as happily against a `pf_rank` that ignored `method=` altogether,
    !> which is precisely the failure mode that let the wrong numbers ship.
    subroutine test_sorting_page_examples(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v6(6) = [30, 10, 50, 20, 60, 40]
        integer(int32) :: vdup(6) = [5, 3, 5, 1, 5, 3]
        integer(int32) :: v9(9) = [10, 20, 20, 20, 30, 40, 40, 50, 60]
        integer(int32) :: v8(8) = [30, 10, 20, 10, 30, 30, 40, 20]
        integer(int32) :: tie4(4) = [10, 20, 20, 30]
        integer(int32) :: ma(4) = [1, 4, 6, 9], mb(5) = [2, 3, 6, 7, 10]
        integer(int32), allocatable :: perm(:), sorted(:), distinct(:), merged(:)
        integer, allocatable :: ranks(:), dense(:)
        real(real64) :: fv(2)
        integer(int32) :: val
        integer(int64) :: idx
        integer :: nuniq, lo, hi, first, last
        logical :: ok

        ! ---- the sort_quickstart example -------------------------------------------------
        call pf_argsort(v6, perm)
        call check(error, all(perm == [2, 4, 1, 6, 3, 5]), "sorting.md: pf_argsort perm")
        if (allocated(error)) return
        call pf_sort(v6, sorted)
        call check(error, all(sorted == [10, 20, 30, 40, 50, 60]), "sorting.md: pf_sort sorted")
        if (allocated(error)) return
        call pf_permute(v6, perm)
        call check(error, all(v6 == [10, 20, 30, 40, 50, 60]), "sorting.md: pf_permute in place")
        if (allocated(error)) return
        call pf_is_sorted(v6, ok)
        call check(error, ok, "sorting.md: pf_is_sorted after permuting")
        if (allocated(error)) return

        ! ---- "The index pf_nth_element reports" ------------------------------------------
        call pf_nth_element(vdup, 3, val, idx)
        call check(error, val == 3_int32, "sorting.md: pf_nth_element value at rank 3")
        if (allocated(error)) return
        call check(error, idx == 6_int64, "sorting.md: pf_nth_element reports the STABLE index")
        if (allocated(error)) return

        ! ---- "Searching a sorted array" --------------------------------------------------
        call pf_lower_bound(v9, 20_int32, lo)
        call check(error, lo == 2, "sorting.md: pf_lower_bound of 20")
        if (allocated(error)) return
        call pf_upper_bound(v9, 20_int32, hi)
        call check(error, hi == 5, "sorting.md: pf_upper_bound of 20")
        if (allocated(error)) return
        call pf_equal_range(v9, 20_int32, first, last)
        call check(error, first == 2 .and. last == 4, "sorting.md: pf_equal_range of 20")
        if (allocated(error)) return
        ! An absent target: last == first - 1, so the count is zero.
        call pf_equal_range(v9, 25_int32, first, last)
        call check(error, first == 5 .and. last == 4, "sorting.md: pf_equal_range of an absent 25")
        if (allocated(error)) return

        ! ---- "Distinct values and ranks" -------------------------------------------------
        call pf_unique_count(v8, nuniq)
        call check(error, nuniq == 4, "sorting.md: pf_unique_count")
        if (allocated(error)) return
        call pf_unique(v8, distinct)
        call check(error, all(distinct == [10, 20, 30, 40]), "sorting.md: pf_unique values")
        if (allocated(error)) return
        call pf_rank(v8, ranks)
        call check(error, all(ranks == [5, 1, 3, 1, 5, 5, 8, 3]), &
            "sorting.md: pf_rank defaults to competition ranks")
        if (allocated(error)) return
        ! The negative control for the line above: the same call with the other method must
        ! answer differently, and must give the dense ranks the page names as the contrast.
        call pf_rank(v8, dense, method="dense")
        call check(error, all(dense == [3, 1, 2, 1, 3, 3, 4, 2]), "sorting.md: pf_rank dense ranks")
        if (allocated(error)) return
        call check(error, .not. all(dense == ranks), &
            "sorting.md: method= must change the ranks, or the default is untested")
        if (allocated(error)) return
        ! The three-method table in "Tie handling in pf_rank".
        call pf_rank(tie4, ranks, method="competition")
        call check(error, all(ranks == [1, 2, 2, 4]), "sorting.md: competition ranks of 10,20,20,30")
        if (allocated(error)) return
        call pf_rank(tie4, ranks, method="dense")
        call check(error, all(ranks == [1, 2, 2, 3]), "sorting.md: dense ranks of 10,20,20,30")
        if (allocated(error)) return
        call pf_rank(tie4, ranks, method="ordinal")
        call check(error, all(ranks == [1, 2, 3, 4]), "sorting.md: ordinal ranks of 10,20,20,30")
        if (allocated(error)) return

        ! ---- float distinctness is EXACT -------------------------------------------------
        fv = [0.1_real64 + 0.2_real64, 0.3_real64]
        call pf_unique_count(fv, nuniq)
        call check(error, nuniq == 2, "sorting.md: 0.1+0.2 and 0.3 are two distinct values")
        if (allocated(error)) return

        ! ---- "Merging two sorted arrays" -------------------------------------------------
        call pf_merge(ma, mb, merged)
        call check(error, all(merged == [1, 2, 3, 4, 6, 6, 7, 9, 10]), "sorting.md: pf_merge result")
    end subroutine test_sorting_page_examples

    !> The acceptance test for the `parquet` facade module (src/parquet.f90): this whole
    !> test module's only library import is a bare `use parquet`, so naming one entity from
    !> each re-exported layer here proves that a user really does need exactly one `use`
    !> statement. If the facade ever stops re-exporting one of the sibling modules, this
    !> test stops COMPILING rather than failing an assertion -- which is the point, since a
    !> missing re-export is a build-time break for every downstream user.
    !>
    !> Layers touched, one name each: parquet_core (parquet_reader/parquet_schema),
    !> parquet_tables (parquet_table, plus both handle types parquet_table_col/parquet_table_row,
    !> which are separate `public ::` entries and so separately droppable),
    !> parquet_columns (PK_FLOAT64/parquet_kind_name),
    !> parquet_strings (parquet_string_column), parquet_temporal (parquet_timestamp),
    !> parquet_sorting (pf_argsort), parquet_healpix (pf_ang2pix_ring),
    !> parquet_integrate (pf_integrate/pf_integration_info/PF_INT_OK),
    !> parquet_interpolate (pf_interp_1d/pf_interp_2d/pf_interp),
    !> parquet_optimize (pf_minimize_scalar/pf_minimize_de/pf_minimize_multistart/
    !> pf_optimize_info/PF_OPT_OK/PF_OPT_TARGET),
    !> parquet_prima (pf_minimize_bobyqa/pf_minimize_lincoa/pf_minimize_cobyla/
    !> pf_bobyqa_solver/pf_constrained_objective),
    !> parquet_settings (parquet_get_arrow_threads /
    !> parquet_max_filter_depth), parquet_maml_base (parquet_maml_file), and the facade's own
    !> parquet_get_version.
    subroutine test_facade_covers_every_layer(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/facade_covers_every_layer.parquet"
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        type(parquet_reader) :: reader
        type(parquet_column) :: col
        type(parquet_string_column) :: sc
        type(parquet_timestamp) :: ts
        type(parquet_maml_file) :: mf
        real(real64) :: mass(4)
        real(real64), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=:), allocatable :: ver, kname
        integer(int32), allocatable :: sort_perm(:)
        integer :: i

        do i = 1, size(mass)
            mass(i) = real(i, real64) * 2.5_real64
        end do

        ! parquet_tables + parquet_columns: build a table, ask for a kind constant by name.
        call parquet_new_table(t)
        call t%add_column("mass", mass, unit="Msun")
        call check(error, t%kind("mass") == PK_FLOAT64, &
            "PK_FLOAT64 must be reachable from use parquet alone")
        if (allocated(error)) return
        call parquet_kind_name(t%kind("mass"), kname)
        call check(error, kname == "PK_FLOAT64", &
            "parquet_kind_name must be reachable from use parquet alone and name the kind")
        if (allocated(error)) return

        ! parquet_columns: the standalone column container behind the table layer.
        call col%init(PK_FLOAT64, 2_int64)
        call check(error, col%length() == 2_int64, &
            "parquet_column must be reachable from use parquet alone")
        if (allocated(error)) return
        call col%clear()

        ! parquet_tables: the two handle types. Declared in a block so that a dropped re-export
        ! breaks the BUILD here rather than only a later assertion, which is this test's whole
        ! mechanism -- its only library import is a bare `use parquet`.
        block
            type(parquet_table_col) :: c
            type(parquet_table_row) :: r
            real(real64) :: v
            call t%column("mass", c)
            call c%get(2_int64, v)
            call check(error, abs(v - mass(2)) < 1.0e-12_real64, &
                "parquet_table_col must be reachable from use parquet alone and read a cell")
            if (allocated(error)) return
            r = t%row(2_int64)
            call r%get(c, v)
            call check(error, abs(v - mass(2)) < 1.0e-12_real64, &
                "parquet_table_row must be reachable from use parquet alone and read through a handle")
            if (allocated(error)) return
        end block

        ! parquet_strings: the compact string store.
        call sc%append_string("facade")
        call check(error, sc%size() == 1_int64, &
            "parquet_string_column must be reachable from use parquet alone")
        if (allocated(error)) return

        ! parquet_toml: the configuration reader. Declared in a block for the same reason as the
        ! two table handles above -- a dropped re-export then breaks the BUILD here rather than
        ! only a later assertion, which is this test's whole mechanism.
        block
            type(pf_toml) :: conf, gen
            integer :: nproc

            call pf_toml_loads(conf, 'x = 1' // new_line("a") // '[general]' // new_line("a") // &
                'nproc = 4' // new_line("a"))
            call pf_toml_section(conf, "general", gen)
            call pf_toml_get(gen, "nproc", nproc)
            call check(error, nproc == 4, &
                "pf_toml must be reachable from use parquet alone and read a configuration value")
            if (allocated(error)) then
                call pf_toml_close(conf)
                return
            end if
            call pf_toml_close(conf)
        end block

        ! parquet_sorting: the raw-array sorting layer, whose public names are pf_*, not parquet_*.
        call pf_argsort([3_int32, 1_int32, 2_int32], sort_perm)
        call check(error, all(sort_perm == [2, 3, 1]), &
            "pf_argsort must be reachable from use parquet alone and order the values")
        if (allocated(error)) return

        ! parquet_stats: the array-statistics layer, whose public names are pf_*, not parquet_*.
        block
            integer(int64) :: n_pop
            call pf_count_valid(mass, n_pop, is_valid=[.true., .false., .true., .true.])
            call check(error, n_pop == 3_int64, &
                "pf_count_valid must be reachable from use parquet alone and exclude the null")
            if (allocated(error)) return
        end block

        ! parquet_healpix: the sphere pixelisation, whose public names are pf_*, not parquet_*.
        block
            integer(int64) :: hp_pix, hp_grid_pix
            type(pf_healpix_grid) :: hp_grid
            call pf_ang2pix_ring(4_int64, 1.0_real64, 2.0_real64, hp_pix)
            call check(error, hp_pix >= 0_int64 .and. hp_pix < 192_int64, &
                "pf_ang2pix_ring must be reachable from use parquet alone and land in range")
            if (allocated(error)) return
            ! The type and the frame selectors are separate `public ::` entries from the
            ! procedures, so each can be lost from the facade independently of them.
            call hp_grid%init(4_int64, PF_HP_RING, frame=PF_HP_DEC_NORTH)
            call hp_grid%ang2pix(1.0_real64, 2.0_real64, hp_grid_pix)
            call check(error, hp_grid_pix == hp_pix, &
                "pf_healpix_grid must be reachable from use parquet alone and agree with pf_ang2pix_ring")
            if (allocated(error)) return
        end block

        ! parquet_list / parquet_map / parquet_struct: the three container element domains. Each
        ! type is declared in a block so a dropped re-export breaks the BUILD, which is the whole
        ! mechanism of this test -- its only library import is a bare `use parquet`.
        block
            type(parquet_list_column) :: lc
            type(parquet_map_column) :: mc
            type(parquet_struct_column) :: stc
            call lc%init(PK_INT32)
            call lc%append_row([11_int32, 12_int32, 13_int32])
            call check(error, lc%size() == 1_int64, &
                "parquet_list_column must be reachable from use parquet alone and take a row")
            if (allocated(error)) return
            call mc%init(PK_INT32)
            call check(error, mc%size() == 0_int64, &
                "parquet_map_column must be reachable from use parquet alone")
            if (allocated(error)) return
            call stc%init(["id ", "nm "], [PK_INT32, PK_INT32])
            call check(error, stc%field_count() == 2, &
                "parquet_struct_column must be reachable from use parquet alone")
            if (allocated(error)) return
        end block

        ! parquet_random and parquet_sampling: the counter-based generator and the population
        ! draws. Both are pf_*-prefixed and neither has any other pin on its re-export -- the two
        ! suites that used to import the facade for this reason were narrowed in the runner split,
        ! so this is now the only thing that would fail if either `use` line left src/parquet.f90.
        block
            real(real64) :: u
            integer(int64) :: perm_at
            u = pf_random_at(12345_int64, 7_int64)
            call check(error, u >= 0.0_real64 .and. u < 1.0_real64, &
                "pf_random_at must be reachable from use parquet alone and land in [0,1)")
            if (allocated(error)) return
            perm_at = pf_random_perm_at(12345_int64, 10_int64, 3_int64)
            call check(error, perm_at >= 1_int64 .and. perm_at <= 10_int64, &
                "pf_random_perm_at must be reachable from use parquet alone and land in range")
            if (allocated(error)) return
        end block

        ! parquet_spatial: the coordinate index over plain arrays.
        block
            type(pf_spatial_index) :: idx
            real(real64) :: xs(3), ys(3), zs(3)
            integer(int64) :: near(4), nnear
            xs = [0.0_real64, 1.0_real64, 2.0_real64]
            ys = [0.0_real64, 0.0_real64, 0.0_real64]
            zs = [0.0_real64, 0.0_real64, 0.0_real64]
            call idx%build(xs, ys, zs, radius=0.5_real64)
            nnear = idx%within([0.1_real64, 0.0_real64, 0.0_real64], 0.5_real64, near)
            call check(error, nnear == 1_int64, &
                "pf_spatial_index must be reachable from use parquet alone and answer a query")
            if (allocated(error)) return
        end block

        ! parquet_index: the key-to-index lookup and the recycling index allocator. Both types are
        ! reached ONLY through the facade's bare `use parquet_index` -- no other module in this
        ! file's import graph exports them -- so dropping that one line stops this block compiling,
        ! which is the point. Before this block existed the facade could have lost parquet_index
        ! entirely with nothing failing: test_module_surface_index covers the NARROW `use
        ! parquet_index` import, and tools/check_module_footprints.sh likewise builds a per-module
        ! consumer, so neither one exercises the facade.
        block
            type(pf_index_map) :: m
            type(pf_index_pool) :: pool
            integer(int32) :: ids(4)
            integer(int64) :: slot
            ids = [70_int32, 12_int32, 41_int32, 5_int32]
            call m%build(ids)
            call check(error, m%get(41_int32) == 3_int64, &
                "pf_index_map must be reachable from use parquet alone and find a key's row")
            if (allocated(error)) return
            call check(error, m%get(99_int32) == 0_int64, &
                "pf_index_map reached through the facade must answer 0 for an absent key")
            if (allocated(error)) return
            slot = pool%get_index()
            call check(error, slot == 1_int64, &
                "pf_index_pool must be reachable from use parquet alone and hand out an index")
            if (allocated(error)) return
        end block

        ! parquet_utils: the text and path helpers.
        block
            character(len=:), allocatable :: joined
            call pf_join_path("data", "cat.parquet", joined)
            call check(error, joined == "data/cat.parquet", &
                "pf_join_path must be reachable from use parquet alone and join POSIX-style")
            if (allocated(error)) return
        end block

        ! parquet_integrate: the quadrature generic and its record types.
        block
            type(pf_integration_info) :: qinfo
            real(real64) :: quad
            quad = pf_integrate(facade_square, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=qinfo)
            call check(error, abs(quad - 1.0_real64/3.0_real64) <= 1.0e-12_real64 .and. &
                qinfo%status == PF_INT_OK, &
                "pf_integrate must be reachable from use parquet alone and integrate x*x to 1/3")
            if (allocated(error)) return
        end block

        ! parquet_interpolate: both interpolant objects and the one-shot form.
        block
            type(pf_interp_1d) :: curve
            type(pf_interp_2d) :: plane
            real(real64) :: knots(3), values(3), grid_values(3, 2)
            knots = [0.0_real64, 1.0_real64, 2.0_real64]
            values = [1.0_real64, 3.0_real64, 5.0_real64]
            call curve%init(knots, values, method="linear")
            call check(error, curve%eval(0.5_real64) == 2.0_real64 .and. &
                pf_interp(knots, values, 1.5_real64, method="linear") == 4.0_real64, &
                "pf_interp_1d and pf_interp must be reachable from use parquet alone and interpolate a line")
            if (allocated(error)) return
            grid_values = reshape([1.0_real64, 3.0_real64, 5.0_real64, 2.0_real64, 4.0_real64, 6.0_real64], [3, 2])
            call plane%init(knots, [0.0_real64, 1.0_real64], grid_values, method="linear")
            call check(error, plane%eval(0.5_real64, 0.5_real64) == 2.5_real64, &
                "pf_interp_2d must be reachable from use parquet alone and interpolate a plane")
            if (allocated(error)) return
        end block

        ! parquet_optimize: the minimisation generics, local tier and population tier, and the
        ! outcome record they share.
        block
            type(pf_optimize_info) :: minfo
            real(real64) :: xmin, fmin, xv(1), lo(1), hi(1)
            call pf_minimize_scalar(facade_offset_square, -3.0_real64, 3.0_real64, xmin, fmin, &
                                    info=minfo)
            call check(error, abs(xmin - 2.0_real64) <= 1.0e-6_real64 .and. &
                minfo%status == PF_OPT_OK, &
                "pf_minimize_scalar must be reachable from use parquet alone and minimise to x = 2")
            if (allocated(error)) return
            lo = [-3.0_real64]
            hi = [6.0_real64]
            call pf_minimize_de(facade_offset_square, lo, hi, 3_int64, xv, fmin, np=8, &
                                ftarget=1.0e-10_real64, max_gen=400, info=minfo)
            call check(error, abs(xv(1) - 2.0_real64) <= 1.0e-4_real64 .and. &
                minfo%status == PF_OPT_TARGET, &
                "pf_minimize_de must be reachable from use parquet alone and reach its target")
            if (allocated(error)) return
            call pf_minimize_multistart(facade_offset_square, lo, hi, 3_int64, xv, fmin, nstart=4, &
                                        info=minfo)
            call check(error, abs(xv(1) - 2.0_real64) <= 1.0e-3_real64 .and. minfo%nminima >= 1, &
                "pf_minimize_multistart must be reachable from use parquet alone and count a minimum")
            if (allocated(error)) return
        end block

        ! parquet_prima: the three engines and the local-solver object.
        block
            type(pf_optimize_info) :: binfo
            type(pf_bobyqa_solver) :: bsolver
            real(real64) :: xv(1), fmin, lo(1), hi(1)
            lo = [-3.0_real64]
            hi = [6.0_real64]
            xv = [5.0_real64]
            call pf_minimize_bobyqa(facade_offset_square, xv, fmin, lower=lo, upper=hi, &
                                    rhobeg=0.5_real64, rhoend=1.0e-8_real64, info=binfo)
            call check(error, abs(xv(1) - 2.0_real64) <= 1.0e-6_real64 .and. &
                binfo%status == PF_OPT_OK, &
                "pf_minimize_bobyqa must be reachable from use parquet alone and minimise to x = 2")
            if (allocated(error)) return
            bsolver%rhoend = 1.0e-8_real64
            call pf_minimize_multistart(facade_offset_square, lo, hi, 5_int64, xv, fmin, &
                                        nstart=4, solver=bsolver, info=binfo)
            call check(error, abs(xv(1) - 2.0_real64) <= 1.0e-6_real64 .and. binfo%nlimit == 0, &
                "pf_bobyqa_solver must be reachable from use parquet alone and drive the driver")
            if (allocated(error)) return
        end block

        ! parquet_prima: the two constrained engines, and `pf_constrained_objective` with them.
        ! The constraint `x <= 1` cuts off the free minimiser at 2, so both must answer 1.
        block
            type(pf_optimize_info) :: cinfo
            type(facade_capped_square) :: capped
            real(real64) :: xv(1), fmin, a_ineq(1, 1), b_ineq(1)
            a_ineq(1, 1) = 1.0_real64
            b_ineq = 1.0_real64
            xv = [0.0_real64]
            call pf_minimize_lincoa(facade_offset_square, xv, fmin, a_ineq=a_ineq, &
                                    b_ineq=b_ineq, rhobeg=0.5_real64, rhoend=1.0e-8_real64, &
                                    info=cinfo)
            call check(error, abs(xv(1) - 1.0_real64) <= 1.0e-6_real64 .and. &
                cinfo%status == PF_OPT_OK, &
                "pf_minimize_lincoa must be reachable from use parquet alone and honour x <= 1")
            if (allocated(error)) return
            xv = [0.0_real64]
            call pf_minimize_cobyla(capped, xv, fmin, rhobeg=0.5_real64, rhoend=1.0e-8_real64, &
                                    info=cinfo)
            call check(error, abs(xv(1) - 1.0_real64) <= 1.0e-5_real64 .and. &
                cinfo%status /= PF_OPT_INFEASIBLE, &
                "pf_minimize_cobyla must be reachable from use parquet alone and honour its " // &
                "own constraint")
            if (allocated(error)) return
        end block

        ! parquet_sphere: the polygon type, one draw from it, and its contract identifier.
        block
            type(pf_sky_polygon) :: poly
            real(real64) :: ra, dec
            call poly%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], &
                           [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
            call poly%random_at(20260917_int64, 1_int64, ra, dec)
            call check(error, poly%contains(ra, dec) .and. len(pf_sky_region_algorithm) > 0, &
                "pf_sky_polygon must be reachable from use parquet alone and draw inside itself")
            if (allocated(error)) return
        end block

        ! parquet_logging: the logger value type.
        block
            type(pf_logger) :: lg
            call check(error, .not. lg%enabled(PF_LEVEL_CRITICAL), &
                "pf_logger must be reachable from use parquet alone and report a fresh logger silent")
            if (allocated(error)) return
        end block

        ! parquet_expkey: the facade re-exports exactly two names from this leaf, with an `only:`
        ! list, so a rename on either side breaks here rather than at a call site nobody has.
        call check(error, parquet_debug_exp_key(0.5_real64) /= 0.0_real64, &
            "parquet_debug_exp_key must be reachable from use parquet alone")
        if (allocated(error)) return

        ! parquet_temporal: one element type, carrying its own null state.
        call ts%parse("2026-08-03T12:00:00")
        call check(error, .not. ts%is_null(), &
            "parquet_timestamp must be reachable from use parquet alone and parse a literal")
        if (allocated(error)) return

        ! parquet_settings: one procedure and one read-only constant, since the module exports
        ! both kinds and a `public ::` list can lose either independently.
        call check(error, parquet_get_arrow_threads() >= 1, &
            "parquet_get_arrow_threads must be reachable from use parquet alone")
        if (allocated(error)) return
        call check(error, parquet_max_filter_depth > 0, &
            "parquet_max_filter_depth must be reachable from use parquet alone")
        if (allocated(error)) return

        ! parquet_maml_base: the MAML file type a schema is built from.
        call check(error, .not. allocated(mf%lines), &
            "parquet_maml_file must be reachable from use parquet alone")
        if (allocated(error)) return

        ! parquet_core: schema, table write-out and the plain reader, on the same table.
        call s%init("facade")
        call s%add_field("mass", "float64", unit="Msun")
        call parquet_write_table(t, out_file, s)

        call parquet_open_table(t2, out_file)
        call t2%get("mass", got)
        call check(error, abs(got(3) - 7.5_real64) < 1.0e-12_real64, &
            "the table written through the facade should round-trip its values")
        if (allocated(error)) return

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        call check(error, nrows == 4_int64, &
            "the plain reader should agree with the table on the row count")
        if (allocated(error)) return

        ! The library's own version. It lives in the leaf module parquet_version, and `use parquet`
        ! is the ONLY module that re-exports it -- so this assertion is what would fail if that one
        ! bare `use parquet_version` line in the facade were ever dropped.
        call parquet_get_version(ver, mode="internal")
        call check(error, len(ver) > 0, &
            "parquet_get_version must be reachable through the facade")
        if (allocated(error)) return

        ! The linked Arrow version is the other half, re-exported from parquet_settings.
        call parquet_get_arrow_version(ver)
        call check(error, len(ver) > 0, &
            "parquet_get_arrow_version must be reachable through the facade")
    end subroutine test_facade_covers_every_layer

    !> Mirrors the `narrow_import` example in doc/pages/operating/choosing-a-module.md.
    !!
    !! **This mirrors the example's VALUES, not its import.** That page's example is a complete
    !! program on `use parquet_argsort` alone, and the property that the import is enough to compile
    !! it belongs to test/test_module_surface.f90's `test_module_surface_argsort`, which is the only
    !! module here with a single library `use` line. This module imports `parquet`, so it can check
    !! that the printed permutation the page shows is still the one `pf_argsort` produces -- which is
    !! the half a reader would notice first, and the half nothing else asserts.
    !> `doc/pages/operating/performance.md`'s `three_ways` program: the page's central advice, which
    !> is that the three ways to reach a cell differ in cost and not in answer.
    !>
    !> **What this pins is the "not in answer" half**, which is the claim a reader relies on when
    !> they rewrite a loop for speed. The page prints `12.0` three times; if any of the three stops
    !> agreeing, the page's advice has become a correctness trap rather than a performance tip.
    !> The costs themselves are not asserted here — a timing assertion in a test suite is a flake,
    !> and `bench/benchmark_colindex.sh` is what measures them.
    subroutine test_three_ways_example(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_col) :: c
        real(real64), pointer :: p(:)
        real(real64) :: mass(4) = [1.5_real64, 2.5_real64, 3.5_real64, 4.5_real64]
        real(real64) :: by_name, by_handle, m
        integer(int64) :: i

        call parquet_new_table(t)
        call t%add_column("mass", mass)

        by_name = 0.0_real64
        do i = 1, t%nrows()
            call t%get_element("mass", i, m)
            by_name = by_name + m
        end do
        call check(error, abs(by_name - 12.0_real64) < 1.0e-12_real64, &
            "three_ways: the name form no longer totals the 12.0 the page prints")
        if (allocated(error)) return

        call t%column("mass", c)
        by_handle = 0.0_real64
        do i = 1, t%nrows()
            call c%get(i, m)
            by_handle = by_handle + m
        end do
        call check(error, abs(by_handle - by_name) < 1.0e-12_real64, &
            "three_ways: the column handle disagrees with the name form")
        if (allocated(error)) return

        call t%col("mass", p)
        call check(error, abs(sum(p) - by_name) < 1.0e-12_real64, &
            "three_ways: the %col pointer disagrees with the name form")
    end subroutine test_three_ways_example
    !
    subroutine test_narrow_import_example(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: v(5) = [30, 10, 50, 20, 40]
        integer(int32), allocatable :: perm(:)

        call pf_argsort(v, perm)
        call check(error, size(perm) == 5, "narrow_import: pf_argsort returned the wrong size")
        if (allocated(error)) return
        ! The comment on the page reads `! 2 4 1 5 3`. If this changes, the page is wrong.
        call check(error, all(perm == [2, 4, 1, 5, 3]), &
            "narrow_import: the permutation is no longer the 2 4 1 5 3 the page prints")
        if (allocated(error)) return
        ! v is not modified -- the sentence the page's surrounding prose rests on.
        call check(error, all(v == [30, 10, 50, 20, 40]), &
            "narrow_import: pf_argsort modified its input, which the page says it never does")
    end subroutine test_narrow_import_example

    !> README.md's `## Quick example` -- the page's ONE runnable example, and its only `fortran`
    !> fence. Mirrors it: write an `id` column, reopen, read it back. The names on both sides are
    !> what makes the mirror findable; if the example on the page changes, this changes with it.
    subroutine test_readme_quick_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/readme_minimal_example.parquet"
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer(int32), allocatable :: id(:)

        call readme_write_parquet_example(out_file)

        ! The read half of the Quick example, reading the file written above.
        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)

        allocate(id(nrows))
        call parquet_read_column(reader, "id", id)

        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. id(1) == 1_int32 .and. id(3) == 3_int32, &
            "README.md quick_example did not round-trip the 'id' column correctly")
    end subroutine test_readme_quick_example

    subroutine readme_write_parquet_example(out_file)
        character(len=*), intent(in) :: out_file
        type(parquet_writer) :: writer
        integer(int32) :: id(3)
        real(real64) :: value(3)

        id = [1_int32, 2_int32, 3_int32]
        value = [10.0_real64, 20.0_real64, 30.0_real64]

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "value", value)
        call parquet_close_writer(writer)
    end subroutine readme_write_parquet_example

    !> Schema-driven write: with a `schema=`, only columns marked `is_set` are written.
    !>
    !> This was labelled a README example and is no longer one -- README carries a single `fortran`
    !> fence (the Quick example) and the section this test was named for was dissolved when README
    !> became a landing page. It is kept as a behaviour test of the path it exercises: parse a MAML,
    !> `set_column_unavailable()` everything, re-enable one column, write, read back. The pattern
    !> itself is documented in doc/pages/io/writing.md and worked through in
    !> doc/pages/schema/combined-example.md, whose own example has its own mirror below.
    subroutine test_schema_driven_write_enabled(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/readme_maml_schema_example.parquet"
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        type(parquet_reader) :: reader
        integer(int32) :: id0(3)
        integer(int32), allocatable :: id0_read(:)
        integer(int64) :: nrows

        ! call parquet_parse_maml("maml_example.maml", schema)
        call parquet_parse_maml("schemas/maml_example.maml", schema)

        ! Only write the one column this test provides data for.
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")

        id0 = [1_int32, 2_int32, 3_int32]

        ! call parquet_open_writer(writer, "data.parquet", schema)
        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        allocate(id0_read(nrows))
        call parquet_read_column(reader, "id0", id0_read)
        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. all(id0_read == id0), &
            "schema-driven write did not round-trip the enabled 'id0' column correctly")
    end subroutine test_schema_driven_write_enabled

    !> doc/pages/schema/combined-example.md's "MAML schema, vector columns and metadata".
    !> Mirrors the program's use clause and declarations verbatim (this is
    !> what caught a missing `int64` import in an earlier version of that
    !> page's example).
    subroutine test_combined_example(error)
        use iso_fortran_env, only: int32, int64, real64
        implicit none
        type(error_type), allocatable, intent(out) :: error

        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        integer(int32) :: id0(3)
        integer(int64) :: idarr(2, 3)   ! (col_size, nrows) for the "idarr" vector column

        character(len=*), parameter :: out_file = "test_run/readme_combined_example.parquet"
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer(int32), allocatable :: id0_read(:)
        integer(int64), allocatable :: idarr_read(:,:)
        character(len=:), allocatable :: meta_runtime, meta_from_maml, meta_absent

        ! Parse column definitions + table metadata from the MAML file.
        call parquet_parse_maml("schemas/maml_example.maml", schema)

        ! This schema defines more columns than we have data for in this example;
        ! disable everything, then re-enable only the columns we are about to write.
        call schema%set_column_unavailable()
        call schema%set_column_available("id0")
        call schema%set_column_available("idarr")

        ! Add an extra, run-time-only piece of metadata not present in the MAML file.
        call schema%add_metadata("generated_by", "write_parquet_combined_example")

        id0 = [1_int32, 2_int32, 3_int32]
        idarr = reshape([1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64], [2, 3])

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id0", id0)
        call parquet_write_column(writer, "idarr", idarr)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        allocate(id0_read(nrows), idarr_read(2, nrows))
        call parquet_read_column(reader, "id0", id0_read)
        call parquet_read_column(reader, "idarr", idarr_read)
        ! The page promises the file carries BOTH the key added at run time and the MAML's
        ! own table metadata. Read one of each back, plus a key that is in neither, so the
        ! two positive checks cannot pass against a getter that answers for everything.
        call parquet_get_metadata(reader, "generated_by", meta_runtime, default="<missing>")
        call parquet_get_metadata(reader, "test_scalar", meta_from_maml, default="<missing>")
        call parquet_get_metadata(reader, "no_such_key", meta_absent, default="<absent>", warn=.false.)
        call parquet_close_reader(reader)

        call check(error, nrows == 3_int64 .and. all(id0_read == id0) .and. all(idarr_read == idarr), &
            "combined-example.md did not round-trip the 'id0'/'idarr' columns correctly")
        if (allocated(error)) return
        call check(error, meta_runtime == "write_parquet_combined_example", &
            "the metadata key added at run time with schema%add_metadata did not reach the file")
        if (allocated(error)) return
        call check(error, meta_from_maml == "8.1", &
            "the source MAML's own keyarray: metadata did not reach the file")
        if (allocated(error)) return
        call check(error, meta_absent == "<absent>", &
            "negative control: a key present in neither the MAML nor the runtime additions must " // &
            "fall back to default=, otherwise the two checks above prove nothing")
    end subroutine test_combined_example

    !> Mirrors doc/pages/schema/building-schema-in-code.md's `build_schema` program: a schema built
    !! entirely in code, with no explicit parse anywhere. Asserts the round trip AND that
    !! %add_metadata's entry reaches the file -- the call order used to matter here, and the entry
    !! surviving is what shows it no longer does.
    !!
    !! **There is deliberately no parquet_parse_maml call**, and that absence is the test. The page
    !! opens with "There is no separate parse step, and no required order"; this program is the
    !! page's own, and until it dropped an added `call parquet_parse_maml(schema)` it exercised the
    !! ORDERED path instead -- so the claim was guarded by nothing and the mirror had silently
    !! stopped mirroring. Do not reintroduce the call: `tile == 42_int32` below is what fails if
    !! %add_metadata ever again needs one.
    subroutine test_build_schema_example(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: id(3) = [1_int32, 2_int32, 3_int32]
        real(real64)   :: ra(3) = [10.0d0, 20.0d0, 30.0d0]
        character(len=*), parameter :: out_file = "test_run/example_build_schema.parquet"
        integer(int32) :: id_back(3), tile
        real(real64) :: ra_back(3)

        call schema%init(table="targets", author="me", description="A schema built in code")
        call schema%add_field("id", "int32",   ucd="meta.id",   info="Object identifier")
        call schema%add_field("ra", "float64", unit="deg", ucd="pos.eq.ra", info="Right ascension", &
            qc_min=">= 0", qc_max="<= 360")

        call schema%add_metadata("SURVEY_TILE", 42_int32)

        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "id", id_back)
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_get_metadata(reader, "SURVEY_TILE", tile)
        call parquet_close_reader(reader)

        call check(error, all(id_back == id), "the in-code schema example did not round-trip its int32 column")
        if (allocated(error)) return
        call check(error, all(abs(ra_back - ra) < 1.0e-12_real64), &
            "the in-code schema example did not round-trip its float64 column")
        if (allocated(error)) return
        call check(error, tile == 42_int32, &
            "add_metadata after parquet_parse_maml should reach the written file")
    end subroutine test_build_schema_example
    !
    !> doc/pages/types/date-time.md's "datetime_quickstart" example: writes a
    !> parquet_date column (with one null) and a parquet_timestamp column (set
    !> from civil fields, an ISO-8601 string, and set_unix), reads them back,
    !> and checks the round-tripped values/null state/to_string output.
    !> doc/pages/tables/table.md's opening "mean_mass" example -- the first thing a reader of the
    !! table layer copies, and the only complete runnable program on that page.
    !!
    !! The example as printed opens a file that must already exist, so the fixture is written here
    !! first; nothing else about it changes. One deviation from the page is unavoidable: the
    !! example says `use parquet_tables`, and this module already carries `use parquet`, so the
    !! narrow import is NOT what is exercised here -- see the note in feature_doc_table.md.
    !> `doc/pages/tables/table-open.md`'s per-thread slice example: one slice per row group covers
    !! every row of the file exactly once.
    !!
    !! **Run serially here, and that is a deliberate deviation from the page.** The example is
    !! written as an `!$omp parallel do`, but test-drive runs each suite's tests inside its OWN
    !! `!$omp parallel do` (`examples` is not in `run_tester.f90`'s exclusion list), so a nested
    !! region here would be exactly the libgomp nesting hazard `feature_risks.md` Risk-104 records.
    !! Nothing is lost: the claim under test is the SLICE ARITHMETIC -- that
    !! `parquet_table_row_group_bounds` partitions the file and each slice reads its own rows --
    !! which is what the example is really teaching and is independent of who runs the loop.
    !! `test/test_table_parallel.f90` is where the threaded shape itself is exercised.
    subroutine test_per_thread_slices_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/example_per_thread_slices.parquet"
        integer, parameter :: N = 30, CH = 7
        type(parquet_writer) :: writer
        type(parquet_table) :: whole
        integer(int64), allocatable :: bounds(:,:)
        real(real64), allocatable :: all_mass(:)
        real(real64) :: total, expect, wrong
        integer :: rg, i

        ! Several row groups, so the bounds really partition rather than trivially covering.
        call parquet_open_writer(writer, out_file, chunk_size=CH)
        call parquet_write_column(writer, "mass", [(real(i, real64), i = 1, N)])
        call parquet_close_writer(writer)

        ! --- the example, verbatim apart from the filename and the serial loop ---
        call parquet_table_row_group_bounds(out_file, bounds)

        total = 0.0_real64
        do rg = 1, size(bounds, 2)
            block
                type(parquet_table) :: mine        ! NOT private(mine) -- see the page
                real(real64), allocatable :: x(:)
                call parquet_open_table(mine, out_file, bounds(1, rg), bounds(2, rg))
                call mine%get("mass", x)
                total = total + sum(x)
            end block
        end do
        ! --- end of the example ---

        call check(error, size(bounds, 2) > 1, &
            "precondition: the fixture must have several row groups, or the slices prove nothing")
        if (allocated(error)) return

        ! The whole-file read is the independent oracle: the slices must reproduce it exactly.
        call parquet_open_table(whole, out_file)
        call whole%get("mass", all_mass)
        expect = sum(all_mass)
        call check(error, abs(total - expect) < 1.0e-9_real64, &
            "one slice per row group must cover every row of the file exactly once")
        if (allocated(error)) return
        call check(error, abs(expect - real(N * (N + 1) / 2, real64)) < 1.0e-9_real64, &
            "precondition: the fixture's own total should be the hand-computed one")
        if (allocated(error)) return

        ! Negative control. Deliberately mis-cut ranges -- row group 1's bounds used twice, so its
        ! rows are counted twice and the last row group's not at all -- must give a DIFFERENT
        ! total. Without this the test passes just as happily against a slice open that ignores
        ! row_lo/row_hi and hands every slice the whole file.
        wrong = 0.0_real64
        do rg = 1, size(bounds, 2)
            block
                type(parquet_table) :: mine
                real(real64), allocatable :: x(:)
                integer :: pick
                pick = rg
                if (pick == size(bounds, 2)) pick = 1
                call parquet_open_table(mine, out_file, bounds(1, pick), bounds(2, pick))
                call mine%get("mass", x)
                wrong = wrong + sum(x)
            end block
        end do
        call check(error, abs(wrong - expect) > 1.0e-9_real64, &
            "the negative control: mis-cut ranges must NOT reproduce the whole-file total")
    end subroutine test_per_thread_slices_example

    subroutine test_mean_mass_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/example_mean_mass.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        real(real64), allocatable :: mass(:)
        real(real64) :: expect
        logical :: ok

        ! The fixture the example's "catalogue.parquet" stands for.
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "mass", &
            [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64])
        call parquet_close_writer(writer)

        ! --- the example, verbatim apart from the filename ---
        call parquet_open_table(t, out_file)
        call t%get("mass", mass)
        ! print *, "rows:", t%nrows(), " mean mass:", sum(mass) / size(mass)
        ! --- end of the example ---

        call check(error, t%nrows() == 4_int64, "the example's table should report 4 rows")
        if (allocated(error)) return
        call check(error, size(mass) == 4, "%get should hand back one value per row")
        if (allocated(error)) return
        expect = 2.5_real64
        call check(error, abs(sum(mass) / size(mass) - expect) < 1.0e-12_real64, &
            "the example's mean should be 2.5")
        if (allocated(error)) return

        ! Negative control. Without it this test passes just as happily against a %get that never
        ! read anything: every assertion above would still hold if `mass` came from somewhere else.
        ! A column the fixture does not have must report a miss and leave an EMPTY result, not a
        ! stale or undefined one -- which is the rule the page states for every reading form.
        call t%get("no_such_column", mass, found=ok)
        call check(error, .not. ok, "a column the fixture lacks should report found=.false.")
        if (allocated(error)) return
        call check(error, size(mass) == 0, &
            "the negative control: a reported miss must leave a zero-length array")
    end subroutine test_mean_mass_example
    !
    subroutine test_datetime_quickstart_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/readme_datetime_quickstart.parquet"
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_date) :: observed(3)
        type(parquet_timestamp) :: taken_at(3)
        character(len=:), allocatable :: s
        integer(int32) :: y, mo, d, h, mi, sec

        call observed(1)%set(2024, 7, 16)
        call observed(2)%set(2024, 7, 17)
        call observed(3)%set_null()                        ! a missing value

        call taken_at(1)%set(2024, 7, 16, 12, 34, 56)
        call taken_at(2)%parse("2024-07-17T08:00:00.5")     ! from an ISO-8601 string
        call taken_at(3)%set_unix(1721260800_int64, parquet_unit_seconds)

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "observed", observed)
        call parquet_write_column(writer, "taken_at", taken_at)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "observed", observed)
        call parquet_read_column(reader, "taken_at", taken_at)
        call parquet_close_reader(reader)

        call check(error, .not. observed(1)%is_null() .and. observed(1)%year() == 2024 .and. &
            observed(1)%month() == 7 .and. observed(1)%day() == 16, &
            "datetime_quickstart example: 'observed' row 1 did not round-trip correctly")
        if (allocated(error)) return

        call check(error, observed(3)%is_null(), &
            "datetime_quickstart example: 'observed' row 3 should have round-tripped as null")
        if (allocated(error)) return

        call check(error, .not. taken_at(1)%is_null(), &
            "datetime_quickstart example: 'taken_at' row 1 should not be null")
        if (allocated(error)) return

        call taken_at(1)%to_string(s)
        call check(error, s == "2024-07-16T12:34:56", &
            "datetime_quickstart example: 'taken_at' row 1 to_string mismatch, got: " // s)
        if (allocated(error)) return

        call taken_at(2)%get(y, mo, d, h, mi, sec)
        call check(error, y == 2024 .and. mo == 7 .and. d == 17 .and. h == 8, &
            "datetime_quickstart example: 'taken_at' row 2 (parsed from ISO-8601) did not round-trip correctly")
        if (allocated(error)) return

        call check(error, taken_at(3)%to_unix(parquet_unit_seconds) == 1721260800_int64, &
            "datetime_quickstart example: 'taken_at' row 3 (set_unix) did not round-trip correctly")
    end subroutine test_datetime_quickstart_example
    !
    !> doc/pages/types/supported-data-types.md's "null_values_example" (the "Null values"
    !> section): writes a float64 column with two genuine Parquet Nulls via is_valid=, then
    !> reads it back twice -- once with is_valid= alone and once with null_value= alone --
    !> and asserts the two documented outcomes differ in exactly the way the page states.
    !>
    !> The two read-backs are each other's control: the SAME column, from the same file, must
    !> yield 0.0 in the Null slots under is_valid= and -99.0 under null_value=. A build that
    !> ignored null_value= entirely, or one that filled every Null with the same constant
    !> whichever argument was passed, fails here -- where asserting only the is_valid= half
    !> would pass against both. The valid rows are asserted on both paths too, so a build
    !> that substituted the sentinel everywhere rather than only at the Nulls is caught.
    !>
    !> Deliberately NOT asserted: the strict (neither-argument) read the page mentions last,
    !> which aborts the process -- see error_scenarios.f90's read_column_with_nulls for that
    !> half, which cannot live in an in-process test.
    subroutine test_null_values_example(error)
        use iso_fortran_env, only: real64
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/doc_null_values_example.parquet"
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real64) :: flux(5), got(5), filled(5)
        logical :: written(5), present_rows(5)

        flux = [1.5_real64, 2.5_real64, 3.5_real64, 4.5_real64, 5.5_real64]
        written = [.true., .true., .false., .true., .false.]

        ! Rows 3 and 5 are written as genuine Parquet Nulls; flux(3)/flux(5) are ignored.
        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "flux", flux, is_valid=written)
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "flux", got, is_valid=present_rows)
        call parquet_read_column(reader, "flux", filled, null_value=-99.0_real64)
        call parquet_close_reader(reader)

        call check(error, all(present_rows .eqv. written), &
            "null_values_example: is_valid did not report rows 3 and 5 as the Null ones")
        if (allocated(error)) return
        ! is_valid= alone: Null slots take the type's own safe default, not the written value.
        call check(error, got(3) == 0.0_real64 .and. got(5) == 0.0_real64, &
            "null_values_example: is_valid-only read did not default the two Null slots to 0")
        if (allocated(error)) return
        ! null_value= instead: the SAME two slots take the sentinel -- the control for the above.
        call check(error, filled(3) == -99.0_real64 .and. filled(5) == -99.0_real64, &
            "null_values_example: null_value= read did not substitute -99.0 in the two Null slots")
        if (allocated(error)) return
        ! Valid rows are untouched on both paths, so a build substituting everywhere is caught.
        call check(error, got(1) == 1.5_real64 .and. got(2) == 2.5_real64 .and. got(4) == 4.5_real64, &
            "null_values_example: is_valid-only read did not round-trip the three non-Null values")
        if (allocated(error)) return
        call check(error, filled(1) == 1.5_real64 .and. filled(2) == 2.5_real64 .and. filled(4) == 4.5_real64, &
            "null_values_example: null_value= read did not round-trip the three non-Null values")
    end subroutine test_null_values_example
    !
    !> Mirrors doc/pages/schema/quality-control.md's `qc_read_example` program, the page's one
    !> complete runnable example. Same data, same qc-maml, same qc_soft=.true. read; only the file
    !> paths differ, so that this test owns them (tests in a suite run concurrently, and two sharing
    !> a fixture path is a documented source of intermittent failure).
    !>
    !> What it pins is that read-time qc in SOFT mode does not disturb the read: the page tells a
    !> reader the violation is reported and the values still arrive, so the assertion is that
    !> ra_back holds the data verbatim -- out-of-range elements included, since qc reports and
    !> never filters. The negative control is the in-range column: it must round-trip identically
    !> while producing no violation at all, which is what separates "qc left the data alone" from
    !> "qc was never active".
    subroutine test_qc_read_example(error)
        use iso_fortran_env, only: real64
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/qc_read_example.parquet"
        character(len=*), parameter :: maml_file = "test_run/qc_read_example.maml"
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        real(real64) :: ra(5), ra_back(5), ok(5), ok_back(5)
        integer :: u

        ra = [10.0_real64, 400.0_real64, 120.0_real64, -5.0_real64, 359.5_real64]
        ok = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]   ! the control: all in range

        call parquet_open_writer(writer, out_file)
        call parquet_write_column(writer, "ra", ra)
        call parquet_write_column(writer, "ok", ok)
        call parquet_close_writer(writer)

        open(newunit=u, file=maml_file, status="replace", action="write")
        write(u, '(A)') "fields:"
        write(u, '(A)') "- name: ra"
        write(u, '(A)') "  qc:"
        write(u, '(A)') "    min: 0"
        write(u, '(A)') "    max: 360"
        write(u, '(A)') "- name: ok"
        write(u, '(A)') "  qc:"
        write(u, '(A)') "    min: 0"
        write(u, '(A)') "    max: 360"
        close(u)

        ! qc_soft=.true. -- a violation warns rather than aborting, so this is testable in process.
        call parquet_open_reader(reader, out_file, &
            schema=parquet_load_qc_maml_file(maml_file), qc_soft=.true.)
        call parquet_read_column(reader, "ra", ra_back)
        call parquet_read_column(reader, "ok", ok_back)
        call parquet_close_reader(reader)

        call check(error, all(ra_back == ra), &
            "quality-control.md's qc_read_example must return every value verbatim -- a soft qc " // &
            "violation reports the out-of-range elements, it never drops or alters them")
        if (allocated(error)) return
        call check(error, all(ok_back == ok), &
            "the in-range control column must round-trip identically under the same qc schema")
    end subroutine test_qc_read_example

    !> doc/pages/schema/combined-example.md's "write_parquet_qc_example" (the page's
    !> "Nulls, quality control and compression together" section): builds the schema in
    !> code with schema%init/schema%add_field, declaring the qc bounds through add_field's
    !> qc_min/qc_max arguments, writes an out-of-range column with is_valid (one Null) and
    !> qc=.true./compression="zstd", then reads it back and checks the round-tripped
    !> values/nulls -- the WARNING itself is not asserted on (test-drive can't capture
    !> stdout), only that the write/read still succeeds and round-trips correctly despite
    !> the qc violation.
    !>
    !> qc=.true. is deliberately redundant here and is kept to mirror the page: qc defaults
    !> to present(schema), so the checks are already on. Do not "tidy" it away on either
    !> side without changing the page's prose, which explains exactly that.
    subroutine test_combined_example_qc_example(error)
        use iso_fortran_env, only: int32
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/combined_example_qc_example.parquet"
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        integer(int32) :: ra(4) = [10_int32, 400_int32, 90_int32, 200_int32]  ! 400 is out of range
        integer(int32) :: ra_read(4)
        logical :: is_valid(4) = [.true., .true., .false., .true.]           ! row 3 will be written as Null
        logical :: is_valid_read(4)

        call schema%init("qc_example_table")
        call schema%add_field("ra", "int32", qc_min=">= 0", qc_max="< 360")

        call parquet_open_writer(writer, out_file, schema, qc=.true., compression="zstd")
        call parquet_write_column(writer, "ra", ra, is_valid=is_valid)
        ! prints: WARNING: qc violation for column 'ra': declared min >= 0, max < 360, ...
        call parquet_close_writer(writer)

        call parquet_open_reader(reader, out_file)
        call parquet_read_column(reader, "ra", ra_read, is_valid=is_valid_read)
        call parquet_close_reader(reader)

        ! Row 3 was written as a genuine Null (is_valid(3)=.false.); with no null_value=
        ! passed on read, that slot comes back as the safe default (0), not the original
        ! ra(3) -- see doc/pages/types/supported-data-types.md's "Null values" section.
        call check(error, all(is_valid_read .eqv. is_valid), &
            "combined-example.md qc example did not round-trip the 'ra' column's is_valid mask correctly")
        if (allocated(error)) return
        call check(error, ra_read(1) == ra(1) .and. ra_read(2) == ra(2) .and. ra_read(3) == 0_int32 .and. &
            ra_read(4) == ra(4), &
            "combined-example.md qc example did not round-trip the 'ra' column values correctly")
    end subroutine test_combined_example_qc_example
    !
    !> doc/pages/types/string-columns.md's "strings_quickstart" example: appends two
    !> strings, a null, and an empty string to a parquet_string_column, then
    !> checks size/character_size/null_count and per-row is_null/get.
    subroutine test_strings_quickstart_example(error)
        use parquet_strings, only: parquet_string_column
        use iso_fortran_env, only: int64
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: names
        character(len=:), allocatable :: s

        call names%append_string("Alice")
        call names%append_string("Bob")
        call names%append_null()             ! a missing value (not the same as "")
        call names%append_string("")         ! an empty string

        call check(error, names%size() == 4_int64, "strings_quickstart example: unexpected size()")
        if (allocated(error)) return
        call check(error, names%character_size() == 8_int64, &
            "strings_quickstart example: unexpected character_size()")
        if (allocated(error)) return
        call check(error, names%null_count() == 1_int64, "strings_quickstart example: unexpected null_count()")
        if (allocated(error)) return

        call check(error, .not. names%is_null(1_int64), "strings_quickstart example: row 1 should not be null")
        if (allocated(error)) return
        call names%get(1_int64, s)
        call check(error, s == "Alice", "strings_quickstart example: row 1 get() mismatch, got: " // s)
        if (allocated(error)) return

        call check(error, names%is_null(3_int64), "strings_quickstart example: row 3 should be null")
        if (allocated(error)) return

        call check(error, .not. names%is_null(4_int64), "strings_quickstart example: row 4 should not be null")
        if (allocated(error)) return
        call names%get(4_int64, s)
        call check(error, s == "", "strings_quickstart example: row 4 get() should be an empty string")
    end subroutine test_strings_quickstart_example
    !
    !> doc/pages/types/string-columns.md's "token_column" example: reserves capacity,
    !> appends tokens (one stripped on append), and checks find/reverse-find
    !> and the empty-token count.
    subroutine test_token_column_example(error)
        use parquet_strings, only: parquet_string_column
        use iso_fortran_env, only: int64
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: tokens
        integer(int64) :: i, first_the, n_empty

        call tokens%reserve(1000_int64, 8000_int64)   ! rough estimate; avoids realloc churn
        call tokens%append_string("the")
        call tokens%append_string("quick")
        call tokens%append_string("the")
        call tokens%append_string("  fox  ", strip=.true.)   ! stored as "fox"

        first_the = tokens%find("the")           ! 1
        call check(error, first_the == 1_int64, "token_column example: first 'the' should be at row 1")
        if (allocated(error)) return

        call check(error, tokens%find("the", reverse=.true.) == 3_int64, &
            "token_column example: last 'the' should be at row 3")
        if (allocated(error)) return

        call check(error, tokens%find("fox") > 0, &
            "token_column example: 'fox' should be present (trimmed on append)")
        if (allocated(error)) return

        n_empty = 0
        do i = 1, tokens%size()
            if (.not. tokens%is_null(i) .and. tokens%is_empty(i)) n_empty = n_empty + 1
        end do
        call check(error, n_empty == 0_int64, "token_column example: no token should be empty")
    end subroutine test_token_column_example
    !
    !> Writes a four-column ("id", "name", "RA", "Dec") table using
    !> schemas/maml_example2.maml's schema, with write_maml=.true. so a sidecar
    !> .maml is produced alongside the parquet file. Checks that:
    !> - the schema (including list-form `ucd:` on "id"/"Dec" and the blank
    !>   array_size:/col_size: on "RA"/"Dec" defaulting to 1) parses correctly,
    !> - the keyarray: entries ("test_url"/"test_url2") come through correctly,
    !> - a few of the new top-level scalar metadata keys round-trip correctly,
    !> both in the in-memory metadata/cinfo and after reparsing the sidecar
    !> (with every field enabled and written, nothing should be pruned).
    subroutine test_maml_example2_sidecar_keyarray(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema, sidecar_schema
        integer(int32) :: id(3)
        character(len=24) :: name(3)
        real(real64) :: ra(3), dec(3)
        logical :: exists
        character(len=*), parameter :: out_file = "test_run/maml_example2.parquet"
        character(len=*), parameter :: sidecar_file = "test_run/maml_example2.maml"

        call parquet_parse_maml("schemas/maml_example2.maml", schema)

        call check_keyarray_entries(error, schema%metadata, "in-memory metadata parsed from schemas/maml_example2.maml")
        if (allocated(error)) return

        call check_field_schema(error, schema%cinfo, "in-memory cinfo parsed from schemas/maml_example2.maml")
        if (allocated(error)) return

        call check_scalar_metadata(error, schema%metadata, "in-memory metadata parsed from schemas/maml_example2.maml")
        if (allocated(error)) return

        id = [1_int32, 2_int32, 3_int32]
        name = ["Alice", "Bob  ", "Carol"]
        ra = [10.5_real64, 45.2_real64, 190.0_real64]
        dec = [-5.1_real64, 12.3_real64, 60.0_real64]

        call parquet_open_writer(writer, out_file, schema, write_maml=.true.)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "name", name)
        call parquet_write_column(writer, "RA", ra)
        call parquet_write_column(writer, "Dec", dec)
        call parquet_close_writer(writer)

        inquire(file=sidecar_file, exist=exists)
        call check(error, exists, "write_maml=.true. did not create the expected sidecar .maml file")
        if (allocated(error)) return

        call parquet_parse_maml(sidecar_file, sidecar_schema)
        call check_keyarray_entries(error, sidecar_schema%metadata, "metadata reparsed from the written sidecar .maml")
        if (allocated(error)) return

        call check_field_schema(error, sidecar_schema%cinfo, "cinfo reparsed from the written sidecar .maml")
        if (allocated(error)) return

        ! Every field was enabled and written above, so write_maml's field
        ! pruning should be a no-op here: all 4 fields should still be listed.
        call check(error, size(sidecar_schema%cinfo%col) == size(schema%cinfo%col), &
            "sidecar .maml should still list all 4 fields since none were disabled")
    end subroutine test_maml_example2_sidecar_keyarray

    subroutine check_field_schema(error, cinfo, context)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column_info), intent(in) :: cinfo
        character(len=*), intent(in) :: context
        integer :: idx

        call check(error, size(cinfo%col) == 4, "expected 4 fields (" // context // ")")
        if (allocated(error)) return

        idx = cinfo%get_column_index("id")
        call check(error, trim(cinfo%col(idx)%data_type) == "int32" .and. &
            trim(cinfo%col(idx)%ucd) == "meta.id;meta.main", &
            "unexpected schema for field 'id' (list-form ucd: should join with ';') (" // context // ")")
        if (allocated(error)) return

        idx = cinfo%get_column_index("name")
        call check(error, trim(cinfo%col(idx)%data_type) == "string" .and. cinfo%col(idx)%array_size == 24, &
            "unexpected schema for field 'name' (" // context // ")")
        if (allocated(error)) return

        idx = cinfo%get_column_index("RA")
        call check(error, trim(cinfo%col(idx)%data_type) == "float64" .and. trim(cinfo%col(idx)%unit) == "deg" .and. &
            trim(cinfo%col(idx)%ucd) == "pos.eq.ra" .and. cinfo%col(idx)%array_size == 1 .and. &
            cinfo%col(idx)%col_size == 1, &
            "unexpected schema for field 'RA' (blank array_size:/col_size: should default to 1) (" // context // ")")
        if (allocated(error)) return

        idx = cinfo%get_column_index("Dec")
        call check(error, trim(cinfo%col(idx)%data_type) == "float64" .and. trim(cinfo%col(idx)%ucd) == "pos.eq.dec", &
            "unexpected schema for field 'Dec' (single-item list-form ucd:) (" // context // ")")
    end subroutine check_field_schema

    subroutine check_scalar_metadata(error, metadata, context)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_metadata), intent(in) :: metadata
        character(len=*), intent(in) :: context
        integer :: i
        logical :: found_survey, found_version, found_date, found_maml_version
        logical :: found_depends_1, found_depends_2, found_keywords

        found_survey = .false.
        found_version = .false.
        found_date = .false.
        found_maml_version = .false.
        found_depends_1 = .false.
        found_depends_2 = .false.
        found_keywords = .false.

        do i = 1, size(metadata%items)
            select case (trim(metadata%items(i)%key))
            case ("survey")
                found_survey = .true.
                call check(error, trim(metadata%items(i)%value) == "The Big Survey", &
                    "unexpected value for 'survey' (" // context // ")")
                if (allocated(error)) return
            case ("version")
                found_version = .true.
                call check(error, trim(metadata%items(i)%value) == "1.3", &
                    "unexpected value for 'version' (" // context // ")")
                if (allocated(error)) return
            case ("date")
                found_date = .true.
                ! date: '2025-09-01' -- quoted in the source, so parquet_unquote
                ! strips the wrapping quotes.
                call check(error, trim(metadata%items(i)%value) == "2025-09-01", &
                    "unexpected value for 'date' (" // context // ")")
                if (allocated(error)) return
            case ("maml_version")
                ! Top-level keys are lower-cased while parsing, so "MAML_version:"
                ! in the source becomes the "maml_version" metadata key.
                found_maml_version = .true.
                call check(error, trim(metadata%items(i)%value) == "1.2", &
                    "unexpected value for 'maml_version' (" // context // ")")
                if (allocated(error)) return
            case ("depends_1")
                ! Each depends: list entry's survey/dataset/table/version
                ! sub-keys are combined into one semicolon-separated string.
                found_depends_1 = .true.
                call check(error, trim(metadata%items(i)%value) == "The Medium Survey;SpecZ;Spec_field_01;3.7", &
                    "unexpected value for 'depends_1' (" // context // ")")
                if (allocated(error)) return
            case ("depends_2")
                found_depends_2 = .true.
                call check(error, trim(metadata%items(i)%value) == "The Tiny Survey;Stars;Phot_South;2", &
                    "unexpected value for 'depends_2' (" // context // ")")
                if (allocated(error)) return
            case ("keywords")
                ! keywords: is a plain-string list; all its items are combined
                ! into a single semicolon-separated "keywords" entry, rather
                ! than one entry per item.
                found_keywords = .true.
                call check(error, trim(metadata%items(i)%value) == "Optional keyword tag;TopCat", &
                    "unexpected value for 'keywords' (" // context // ")")
                if (allocated(error)) return
            end select
        end do

        call check(error, found_survey, "metadata key 'survey' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_version, "metadata key 'version' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_date, "metadata key 'date' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_maml_version, "metadata key 'maml_version' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_depends_1, "metadata key 'depends_1' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_depends_2, "metadata key 'depends_2' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_keywords, "metadata key 'keywords' not found (" // context // ")")
    end subroutine check_scalar_metadata

    subroutine check_keyarray_entries(error, metadata, context)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_metadata), intent(in) :: metadata
        character(len=*), intent(in) :: context
        integer :: i
        logical :: found_url, found_url2

        found_url = .false.
        found_url2 = .false.

        do i = 1, size(metadata%items)
            if (trim(metadata%items(i)%key) == "test_url") then
                found_url = .true.
                call check(error, trim(metadata%items(i)%value) == "//example.com/data #new", &
                    "unexpected value for keyarray entry 'test_url' (" // context // ")")
                if (allocated(error)) return
                call check(error, trim(metadata%items(i)%description) == "something: and else", &
                    "unexpected comment for keyarray entry 'test_url' (" // context // ")")
                if (allocated(error)) return
            else if (trim(metadata%items(i)%key) == "test_url2") then
                found_url2 = .true.
                call check(error, trim(metadata%items(i)%value) == "http://example.com/data :#new", &
                    "unexpected value for keyarray entry 'test_url2' (" // context // ")")
                if (allocated(error)) return
                call check(error, trim(metadata%items(i)%description) == "something: and else", &
                    "unexpected comment for keyarray entry 'test_url2' (" // context // ")")
                if (allocated(error)) return
            end if
        end do

        call check(error, found_url, "keyarray entry 'test_url' not found (" // context // ")")
        if (allocated(error)) return
        call check(error, found_url2, "keyarray entry 'test_url2' not found (" // context // ")")
    end subroutine check_keyarray_entries
    !
    !> `doc/pages/utilities/random.md`'s `program random_quickstart`, mirrored.
    !!
    !! **Two assertions, and the second is the one the page exists for.** The three values pin the
    !! draw contract at three coordinates; the array equality pins that the answer does not depend
    !! on how the loop was scheduled, which is the property `random_number` cannot offer.
    !!
    !! **The negative control matters more than usual here.** A fill that ignored `i` entirely, or
    !! that returned the same value everywhere, would satisfy a plain "two runs agree" check
    !! perfectly -- so the control fills from a shifted coordinate and requires the result to
    !! DIFFER. Without it this test passes against a generator that is not addressed at all.
    !!
    !! **Both fills run serially here, and that is a deliberate deviation from the page.** The
    !! page's example is an `!$omp parallel do schedule(dynamic)`, but test-drive runs each suite's
    !! tests inside its OWN `!$omp parallel do` (`examples` is not in `run_tester.f90`'s exclusion
    !! list), so writing the page's directive here opens a NESTED region -- the same hazard
    !! `test_per_thread_slices_example` above declines for, `feature_risks.md` Risk-104.
    !!
    !! Nothing is lost, and something is gained. Nested parallelism is off by default and nothing
    !! in this project turns it on, so such a region gets a team of ONE: measured under ifx
    !! 2026.1.1, `omp_get_max_active_levels()` is 1 and the inner `omp_get_num_threads()` is 1.
    !! Written as the page writes it, this test therefore asserted schedule-independence while
    !! never running on more than one thread -- green, and proving nothing, which is the failure
    !! mode `test_runner_support.f90` calls this project's worst. The two loops below run in
    !! OPPOSITE orders, which is what actually pins order-independence here; the threaded half of
    !! the property belongs to the `random_omp` suite, which is excluded from test-drive's parallel
    !! driver precisely so its regions are not nested and which carries its own vacuity guard on
    !! the team size (`test_schedule_independence`).
    subroutine test_random_quickstart_example(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle
        integer(int32), parameter :: n = 1000000
        integer(int64) :: seed
        real(real64), allocatable :: x(:), y(:)
        integer(int32) :: i

        allocate(x(n), y(n))
        seed = 20260816_int64                     ! the seed the page's example uses

        ! The page writes this as `!$omp parallel do schedule(dynamic)`; see the note above for
        ! why it is serial here and where the threaded property is asserted instead.
        do i = 1, n
            x(i) = pf_random_at(seed, i)
        end do

        ! The three values the page prints, taken from a verbatim run of its own example.
        call check(error, x(1) == 8.9584852429081541e-2_real64, &
            "random.md: quickstart x(1) does not match the value the page prints")
        if (allocated(error)) return
        call check(error, x(500000) == 0.98486331512086944_real64, &
            "random.md: quickstart x(500000) does not match the value the page prints")
        if (allocated(error)) return
        call check(error, x(n) == 0.21019864601366756_real64, &
            "random.md: quickstart x(n) does not match the value the page prints")
        if (allocated(error)) return

        ! In the REVERSE order, so nothing about the order of the fill above can survive into `y`.
        do i = n, 1, -1
            y(i) = pf_random_at(seed, i)
        end do
        call check(error, all(x == y), &
            "random.md: the quickstart's values depend on the loop order, which is the one " // &
            "thing the page promises they do not")
        if (allocated(error)) return

        ! Negative control: a shifted coordinate must NOT reproduce the array.
        do i = 1, n
            y(i) = pf_random_at(seed, i + 1)
        end do
        call check(error, .not. all(x == y), &
            "random.md: a shifted stream index gave the same array, so the draw is not " // &
            "addressed by `i` at all and the assertions above prove nothing")
    end subroutine test_random_quickstart_example
    !
    !> `doc/pages/tables/table-write.md`'s opening `build_and_write` example: the only complete
    !! runnable program on that page, and the first thing a reader building a table from scratch
    !! copies.
    !!
    !! One deviation from the page, and it is the standing one for every mirrored example here: the
    !! output filename is under `test_run/` rather than the page's `out.parquet`, because tests in
    !! a suite run concurrently and two sharing a fixture path is a documented source of
    !! intermittent failure. Nothing else about the example changes.
    !!
    !! **What is actually asserted is the page's own claim about widths**, not merely that a file
    !! appeared. The example declares `names` as `character(len=8)` while the schema declares
    !! `array_size=32`, and the page now says the two need not agree because the array decides the
    !! values and `array_size:` decides the file's storage width. So the round trip checks that
    !! `%get` comes back sized to the longest REAL value (5, for "alpha"/"gamma") -- neither the
    !! declared 8 nor the schema's 32. A test that only compared `trim()`ed values would pass
    !! against a library that padded every name to 32 characters.
    !!
    !! **The negative control is the second table**, built from the same three names in a different
    !! order. Asserting `names(1) == "alpha"` alone would pass against a write that stored the
    !! first value three times, or against a read that ignored the row index; the control must give
    !! a DIFFERENT first name from the same code path.
    subroutine test_build_and_write_example(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: out_file = "test_run/example_build_and_write.parquet"
        character(len=*), parameter :: ctl_file = "test_run/example_build_and_write_ctl.parquet"
        type(parquet_table)  :: t, back
        type(parquet_schema) :: s
        integer(int64)   :: ids(3)
        real(real64)     :: masses(3)
        character(len=8) :: names(3)
        integer(int64), allocatable :: id_back(:)
        real(real64), allocatable :: mass_back(:)
        character(len=:), allocatable :: name_back(:)
        !
        ! ---- the example, verbatim apart from the filename ----
        ids    = [1_int64, 2_int64, 3_int64]
        masses = [1.5_real64, 2.5_real64, 3.5_real64]
        names  = ["alpha   ", "beta    ", "gamma   "]
        !
        call parquet_new_table(t)
        call t%add_column("id",   ids)
        call t%add_column("mass", masses, unit="Msun")
        call t%add_column("name", names)
        !
        call s%init("catalogue")
        call s%add_field("id",   "int64")
        call s%add_field("mass", "float64", unit="Msun")
        call s%add_field("name", "string", array_size=32)
        call parquet_write_table(t, out_file, s)
        !
        ! The example's own trailing print: 3 rows, 3 columns.
        call check(error, t%nrows() == 3_int64 .and. t%ncols() == 3, &
            "table-write.md: the example's own output line says 3 rows and 3 columns")
        if (allocated(error)) return
        !
        ! ---- the round trip ----
        call parquet_open_table(back, out_file)
        call back%get("id", id_back)
        call back%get("mass", mass_back)
        call back%get("name", name_back)
        call check(error, size(id_back) == 3 .and. all(id_back == ids), &
            "table-write.md: the written id column should read back row for row")
        if (allocated(error)) return
        call check(error, all(abs(mass_back - masses) < 1.0e-12_real64), &
            "table-write.md: the written mass column should read back row for row")
        if (allocated(error)) return
        !
        ! The width claim: sized to the longest real value, not to len(names) and not to
        ! array_size=32.
        call check(error, len(name_back) == 5, &
            "table-write.md: %get should be sized to the longest real name (5), not to the " // &
            "declared character(len=8) nor to the schema's array_size=32")
        if (allocated(error)) return
        call check(error, name_back(1) == "alpha" .and. name_back(2) == "beta " .and. &
            name_back(3) == "gamma", &
            "table-write.md: the written name column should read back row for row")
        if (allocated(error)) return
        !
        ! ---- negative control: the same names in a different order must differ ----
        call build_and_write_control(ctl_file, name_back(1), error)
    end subroutine test_build_and_write_example
    !
    !> Negative control for `test_build_and_write_example`: the same three names written in a
    !! different order, so that the assertions above are shown to be about the DATA rather than
    !! about the call sequence. Separated out only to keep the example above readable as the
    !! page prints it.
    subroutine build_and_write_control(fname, first_name, error)
        character(len=*), intent(in) :: fname             !! control fixture, its own path.
        character(len=*), intent(in) :: first_name        !! what the real example read back first.
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table)  :: t, back
        type(parquet_schema) :: s
        character(len=8) :: names(3)
        character(len=:), allocatable :: name_back(:)
        !
        names = ["gamma   ", "beta    ", "alpha   "]
        call parquet_new_table(t)
        call t%add_column("name", names)
        call s%init("catalogue")
        call s%add_field("name", "string", array_size=32)
        call parquet_write_table(t, fname, s)
        !
        call parquet_open_table(back, fname)
        call back%get("name", name_back)
        call check(error, name_back(1) /= first_name, &
            "table-write.md: reversing the names gave the same first value back, so the " // &
            "round-trip assertions above are not reading the data at all")
    end subroutine build_and_write_control
    !
    !> `doc/pages/tables/table-mutate.md`'s `rank_by_flux` example: the ranks a nulled column gets,
    !! and the reason the example passes `is_valid=` at all.
    !!
    !! **This test exists because the page was wrong before it.** The example used to call
    !! `pf_rank(flux, ranks, descending=.true.)` on a bare `%col` pointer, immediately above a
    !! sentence saying "a null gets rank 0" -- which `pf_rank` does, and that call cannot, because
    !! `%col` hands back the VALUES array and a `parquet_column` keeps its nulls in a separate
    !! bitmap. Nothing in the suite noticed, because nothing asserted the composition.
    !!
    !! **The negative control is the maskless call**, run here on the same data. Asserting only
    !! `3 0 2 0 1` would pass against a `pf_rank` that ignored `is_valid=` and happened to agree;
    !! requiring the two calls to DIFFER is what makes this a test of the page's claim rather than
    !! of `pf_rank` alone. The measured maskless answer is `3 4 2 4 1` -- plausible, and wrong for
    !! every row rather than only the two null ones, since the null slots' values participate in
    !! the ordering.
    !!
    !! No fixture: the example is in-memory (`parquet_new_table` + `%add_column`), so there is no
    !! file and no chance of the concurrent-suite path collision CLAUDE.md warns about.
    subroutine test_rank_by_flux_example(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: flux(5)
        real(real64), pointer :: fp(:)
        integer(int64), allocatable :: ranks(:), bare(:)
        logical, allocatable :: valid(:)
        !
        ! ---- the example, verbatim ----
        flux = [3.0_real64, 1.0_real64, 4.0_real64, 1.0_real64, 5.0_real64]
        call parquet_new_table(t)
        call t%add_column("flux", flux)
        call t%set_null("flux", 2_int64)
        call t%set_null("flux", 4_int64)
        !
        call t%col("flux", fp)
        call t%get_valid_mask("flux", valid)
        call pf_rank(fp, ranks, descending=.true., is_valid=valid)
        call t%add_column("flux_rank", ranks)
        !
        call check(error, size(ranks) == 5, "table-mutate.md: one rank per row")
        if (allocated(error)) return
        call check(error, all(ranks == [3_int64, 0_int64, 2_int64, 0_int64, 1_int64]), &
            "table-mutate.md: the rank_by_flux example should print 3 0 2 0 1 -- the two nulled " // &
            "rows ranked 0, the three real values ranked brightest-first")
        if (allocated(error)) return
        !
        ! The rank column really was added, and holds what was ranked.
        call check(error, t%has_column("flux_rank") .and. t%ncols() == 2, &
            "table-mutate.md: the example's %add_column should leave a second column behind")
        if (allocated(error)) return
        !
        ! ---- negative control: without the mask the answer must DIFFER ----
        call pf_rank(fp, bare, descending=.true.)
        call check(error, .not. all(bare == ranks), &
            "table-mutate.md: pf_rank gave the same answer with and without is_valid=, so the " // &
            "page's reason for passing the mask is untested and the rank-0 rule proves nothing")
        if (allocated(error)) return
        call check(error, .not. any(bare == 0_int64), &
            "table-mutate.md: the maskless call should rank the null rows as ordinary values, " // &
            "which is precisely the defect the example's is_valid= exists to avoid")
    end subroutine test_rank_by_flux_example
    !
    !> `doc/pages/operating/error-handling.md`'s `found_or_abort` program: the one affirmative claim
    !> that page makes, which is that `found=` turns an abort into a reported miss.
    !>
    !> **The negative control is the second call, not the absence of one.** A test that only checks
    !> the missing name reports `.false.` passes just as happily against a `found=` that is ignored
    !> and left `.false.` on every path -- so the present column must be shown to set it `.true.`,
    !> on the same table, through the same accessor. The two together are what pin the argument.
    !>
    !> The abort half of the page's claim -- that omitting `found=` on that first call terminates
    !> the process -- cannot be asserted here, because it would take the runner down with it; it is
    !> covered out of process by the `table_unknown_column` scenario in test/error_scenarios.f90,
    !> whose body is this example's first call with `found=` left off.
    subroutine test_found_or_abort_example(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: mass(3)
        real(real64), allocatable :: got(:)
        logical :: ok

        mass = [1.5_real64, 2.5_real64, 3.5_real64]
        call parquet_new_table(t)
        call t%add_column("mass", mass)
        !
        call t%get("flux", got, found=ok)          ! there is no "flux" column
        call check(error, .not. ok, &
            "error-handling.md: %get of an absent column with found= should report .false., " // &
            "which is the whole of what the found_or_abort example demonstrates")
        if (allocated(error)) return
        !
        ! ---- negative control: the same accessor on a column that IS there ----
        call t%get("mass", got, found=ok)
        call check(error, ok, &
            "error-handling.md: %get of a present column with found= should report .true. -- " // &
            "without this the test above passes against a found= that is never set")
        if (allocated(error)) return
        call check(error, size(got) == 3, &
            "error-handling.md: the found_or_abort example prints size(got), which the page " // &
            "shows as 3 for its three-row mass column")
    end subroutine test_found_or_abort_example
    !
    !> `(x - 2)**2`, for the facade test's `pf_minimize_scalar` call: a module procedure, because
    !! a callback in this library is never an internal one. Minimiser `2`, by inspection.
    function facade_offset_square(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2

    end function facade_offset_square

    !> `x*x`, for the facade test's `pf_integrate` call: a module procedure, because a callback
    !> in this library is never an internal one (flang cannot pass one at all).
    function facade_square(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = x*x

    end function facade_square

end module test_examples
