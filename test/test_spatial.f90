!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for `parquet_spatial`: `pf_spatial_index` and its ball, bulk and periodic queries.
!>
!> Four things shape this suite:
!>
!> * **The oracle is a brute-force O(n^2) scan, and it has to be.** Every other check available here
!>   -- a bulk form against a single query, a periodic query against a shifted one -- compares one
!>   part of the grid against another, so a defect in the cell walk itself would satisfy all of
!>   them at once. Only a scan that never touches the grid is independent of what is under test.
!> * **The answers must not depend on the cell size, and that is asserted directly.** The whole
!>   tuner is a performance mechanism, so a cell size that changes an ANSWER is a defect no
!>   benchmark would notice. `test_answers_survive_any_cell` forces cells across two orders of
!>   magnitude and demands the same rows back every time.
!> * **A probe that never runs passes every correctness test ever written for it**, because the
!>   answers do not depend on the cell. `parquet_debug_spatial_probe_count` is the negative control
!>   that separates "the probe chose well" from "the probe did not run", and both directions are
!>   asserted.
!> * **Periodic boundaries are tested by TRANSLATION INVARIANCE**, not by a face-crossing spot
!>   check. Shifting every point by an arbitrary vector modulo the box must leave every neighbour
!>   set identical up to that shift; a naive implementation passes a spot check and fails this.
!>
!> Tests here allocate their own arrays and write no files, so nothing needs a per-test fixture
!> name -- but several drive the process-global debug hooks, which is why `run_tester.f90` excludes
!> this suite from test-drive's per-test parallelism.
module test_spatial
    ! NARROW import, not `use parquet`. The facade would compile just as well and would let a
    ! future test in this file reach the C++ layer without anything saying so; naming the tier
    ! makes that a build error instead. The facade's own re-export of this tier is pinned by
    ! `test_facade_covers_every_layer` (test/test_examples.f90), which is where that claim lives.
    use parquet_spatial
    use parquet_random
    use parquet_sorting
    use iso_fortran_env, only : int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only : ieee_get_flag, ieee_set_flag, ieee_support_flag, ieee_invalid
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    implicit none
    private

    public :: collect_tests_parquet_spatial

    !> Seed for every fixture here. Fixed so a failure is reproducible.
    integer(int64), parameter :: fixture_seed = 20260825_int64

    !> The `wrap=` a NON-periodic brute-force scan takes: no wrapping on any axis.
    !>
    !> A named constant rather than an inline `[0.0_real64, 0.0_real64, 0.0_real64]` at each call
    !> site: `brute_within` takes `wrap(3)` explicit-shape, and an array CONSTRUCTOR is
    !> argument-associated to such a dummy through a compiler-created temporary -- which ifx
    !> reports as `forrtl: warning (406)`, with a traceback, on every call under the debug
    !> profile's `-check arg_temp_created`. A named array constant has an address and is passed
    !> directly. The same reasoning applies to any oracle argument added here later.
    real(real64), parameter :: NO_WRAP(3) = 0.0_real64

    !> `c / H0` in Mpc/h, the one cosmological constant the line-of-sight fixtures need.
    real(real64), parameter :: los_ch0 = 2997.9_real64

    !> The three states of `parquet_debug_set_spatial_los_walk`, named: the library's own choice
    !> per point, the covering ball for every point, the cylinder wherever it is a finite walk.
    integer, parameter :: walk_auto = 0, walk_ball = 1, walk_cyl = 2

contains

    !> Registers every test in this suite.
    subroutine collect_tests_parquet_spatial(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("ball search matches a brute-force scan", test_ball_matches_brute_force), &
            new_unittest("a query outside the cloud finds nothing", test_ball_outside_finds_nothing), &
            new_unittest("a radius covering everything returns every row", test_ball_covers_all), &
            new_unittest("a one-point cloud and coincident points build and answer", test_ball_degenerate_clouds), &
            new_unittest("within reports the true count when the buffer is short", test_within_short_buffer), &
            new_unittest("within fills distances that match the coordinates", test_within_distances), &
            new_unittest("count_within agrees with within", test_count_within_agrees), &
            new_unittest("an int32 buffer returns the same rows as an int64 one", test_within_int32_buffer), &
            new_unittest("pairs_within answers the same in int32 as in int64", &
                         test_pairs_int32_matches_int64), &
            new_unittest("pairs_within_sky and pairs_within_los answer the same in int32", &
                         test_pairs_sky_and_los_int32_match), &
            new_unittest("all_within answers the same CSR in int32 as in int64", &
                         test_all_within_int32_matches), &
            new_unittest("count_all_within answers the same counts in int32 as in int64", &
                         test_count_all_int32_matches), &
            new_unittest("the whole Friends-of-Friends chain agrees in int32 and int64", &
                         test_fof_chain_int32), &
            new_unittest("connected components accepts an int32 edge list with either nvert kind", &
                         test_components_int32_edges), &
            new_unittest("grid answers in both kinds and the int32 ceiling hook is reversible", &
                         test_grid_int32_and_ceiling_hook), &
            new_unittest("a 2D index matches a brute-force scan", test_2d_matches_brute_force), &
            new_unittest("a 2D index has exactly one cell along z", test_2d_grid_is_flat), &
            new_unittest("a flat, a collinear and a single-point cloud all build", test_degenerate_axes_build), &
            new_unittest("all_within reproduces N single queries", test_all_within_matches_singles), &
            new_unittest("all_within accepts one radius per point", test_all_within_per_point_radius), &
            new_unittest("count_all_within agrees with all_within", test_count_all_agrees), &
            new_unittest("pairs_within reproduces all_within with i < j", test_pairs_match_all_within), &
            new_unittest("pairs_within is symmetric under a per-point radius", &
                         test_pairs_per_point_radius_is_symmetric), &
            new_unittest("a uniform radius vector gives the scalar pair list", &
                         test_pairs_uniform_vector_matches_scalar), &
            new_unittest("every combine= rule matches a brute-force scan of its own definition", &
                         test_pairs_combine_rules_match_oracle), &
            new_unittest("omitting combine= is PF_LINK_MAX", test_pairs_combine_default_is_max), &
            new_unittest("a uniform radius vector gives the scalar list under every rule", &
                         test_pairs_combine_uniform_vector), &
            new_unittest("r_inner= composes with every combine= rule", test_pairs_combine_with_inner), &
            new_unittest("sky mean and sum combine angles, not chords, on both backends", &
                         test_sky_pairs_combine_matches_angle_oracle), &
            new_unittest("the sky sum rule holds inside its 45-degree ceiling", &
                         test_sky_pairs_sum_within_ceiling), &
            new_unittest("every combine= rule on the line-of-sight cylinder matches its own oracle", &
                         test_pairs_los_matches_cylinder_oracle), &
            new_unittest("omitting los= is the distance from the observer", &
                         test_pairs_los_default_los_is_distance), &
            new_unittest("a los= steeper than the distance widens the walk by its measured slope", &
                         test_pairs_los_with_slope), &
            new_unittest("pairs closer than the gap floor in los are carried by the spread alone", &
                         test_pairs_los_close_los_is_complete), &
            new_unittest("the slope is measured cleanly on a continuous redshift list", &
                         test_pairs_los_slope_is_clean), &
            new_unittest("comoving coordinates with the redshift as los= match the oracle", &
                         test_pairs_los_redshift_example), &
            new_unittest("dperp and dpar report the two separations in their two units", &
                         test_pairs_los_reports_separations), &
            new_unittest("observer= reproduces the origin's pairs on shifted coordinates", &
                         test_pairs_los_observer_offset), &
            new_unittest("within_los returns exactly the point's own cylinder", &
                         test_within_los_matches_oracle), &
            new_unittest("within_los orders by the normalised measure", &
                         test_within_los_sorted_by_normalised_measure), &
            new_unittest("within_los reports the true count when the buffer is short", &
                         test_within_los_short_buffer_keeps_true_count), &
            new_unittest("los= stays aligned through copy=.false., re-tuning and %rebuild", &
                         test_pairs_los_survives_rebuild_and_copy_false), &
            new_unittest("the cylinder walk returns exactly the ball walk's pairs under every rule", &
                         test_los_cylinder_walk_matches_ball_walk), &
            new_unittest("the union's tiebreak emits a pair in both cylinders exactly once", &
                         test_los_union_tiebreak_counts), &
            new_unittest("within_los walks the cylinder and answers as the ball does", &
                         test_within_los_walks_the_cylinder), &
            new_unittest("the cells-per-point override relaxes the ceiling and changes no answer", &
                         test_cells_per_point_override_relaxes_the_ceiling), &
            new_unittest("the line-of-sight walk is the ball when it fits a cell and the cylinder otherwise", &
                         test_los_walk_choice_follows_the_cell), &
            new_unittest("pairs_within_los emits the same list in the same order under any team", &
                         test_los_sweep_order_is_team_invariant), &
            new_unittest("the sweep's buffers grow and lose nothing", test_los_sweep_buffers_grow), &
            new_unittest("dperp and dpar travel with their pair through the copy", &
                         test_los_sweep_separations_follow_pairs), &
            new_unittest("a chunk boundary neither drops nor repeats a pair", test_los_sweep_chunk_boundaries), &
            new_unittest("segment, cylinder and cone match a brute-force scan", test_axis_matches_brute_force), &
            new_unittest("the three axis shapes accept the right points by hand", test_axis_shapes_by_hand), &
            new_unittest("a cone with equal radii is exactly the cylinder", test_cone_equal_radii_is_cylinder), &
            new_unittest("a zero-length axis is exactly the ball", test_axis_degenerate_is_a_ball), &
            new_unittest("an axis outside the cloud, and one longer than it", test_axis_outside_and_overlong), &
            new_unittest("an axis query works on a 2D index", test_axis_2d), &
            new_unittest("axis distances and a short buffer behave", test_axis_dist_and_short_buffer), &
            new_unittest("axis_point and axis_t satisfy their three invariants", &
                         test_axis_outputs_satisfy_their_invariants), &
            new_unittest("a capsule's foot clamps to the ends", test_axis_point_clamps_at_the_ends), &
            new_unittest("axis outputs on a 2D index and a zero-length axis", &
                         test_axis_point_2d_and_degenerate), &
            new_unittest("short axis buffers stay inside themselves and still correspond", &
                         test_axis_outputs_truncate_together), &
            new_unittest("a sky index matches a haversine scan at the poles and across 0h", &
                         test_sky_matches_brute_force), &
            new_unittest("sky separations come back in degrees", test_sky_distances_are_degrees), &
            new_unittest("a sky index reports its radius in degrees", test_sky_metadata), &
            new_unittest("the sky self-join reproduces N single sky queries", test_sky_bulk_matches_singles), &
            new_unittest("sky pairs and counts agree, and pairs stay symmetric", test_sky_bulk_pairs_and_counts), &
            new_unittest("an annulus is the outer ball minus the inner one", &
                         test_annulus_is_the_difference_of_two_balls), &
            new_unittest("the annulus works in bulk, in pairs and on the sky", &
                         test_annulus_in_bulk_and_on_the_sky), &
            new_unittest("sorted= orders by distance and keeps the same rows", &
                         test_sorted_orders_by_distance), &
            new_unittest("an exact tie is broken by ascending row index", test_sorted_breaks_ties_by_row), &
            new_unittest("nearest matches a full sort of every distance", &
                         test_nearest_matches_a_full_sort), &
            new_unittest("nearest handles k at and beyond the catalogue size", &
                         test_nearest_k_at_and_beyond_the_size), &
            new_unittest("the shell's starting radius changes rounds, not answers", &
                         test_nearest_shell_start_does_not_change_answers), &
            new_unittest("nearest on the sky and on a periodic index", test_nearest_sky_and_periodic), &
            new_unittest("kth_distance matches a full per-point sort", &
                         test_kth_distance_matches_a_full_sort), &
            new_unittest("kth_distance on the sky, and with coincident points", &
                         test_kth_distance_sky_and_duplicates), &
            new_unittest("a threaded kth_distance sweep equals the serial one", &
                         test_kth_distance_threaded_matches_serial), &
            new_unittest("the carried radius saves expansion rounds", &
                         test_kth_distance_carry_over_saves_rounds), &
            new_unittest("connected components on hand-built graphs", test_components_hand_built_graphs), &
            new_unittest("min_size thresholds components, and the default of 1 keeps singletons", &
                         test_components_min_size_threshold), &
            new_unittest("Friends-of-Friends matches a label-propagation scan", &
                         test_components_friends_of_friends), &
            new_unittest("a threaded bulk sweep equals the serial one", test_bulk_threaded_matches_serial), &
            new_unittest("a periodic index matches a minimum-image scan", test_periodic_matches_brute_force), &
            new_unittest("a periodic index is translation invariant", test_periodic_translation_invariant), &
            new_unittest("a periodic 2D index matches a minimum-image scan", test_periodic_2d), &
            new_unittest("a periodic grid tiles the box exactly, whatever cell was asked for", &
                test_periodic_cells_tile_the_box), &
            new_unittest("the probe runs, and a forced cell stops it", test_probe_runs_and_can_be_stopped), &
            new_unittest("every single-point query answers the same in int32 as in int64", &
                         test_single_query_int32_buffers_match), &
            new_unittest("kth_distance_sky answers the same under an int64 k", &
                         test_kth_sky_k64_matches_k32), &
            new_unittest("rebuild folds a radius, as a scalar and as a list", &
                         test_rebuild_folds_a_radius), &
            new_unittest("the work hook counts cells and points for the h it is given", &
                         test_debug_work_counts_both_halves), &
            new_unittest("an index over no points builds and answers every query", &
                         test_empty_index_answers_everything), &
            new_unittest("a periodic copy=.false. index answers as a copying one", &
                         test_periodic_copy_false_matches), &
            new_unittest("r_inner takes one value or one per point", &
                         test_per_point_inner_radii), &
            new_unittest("an empty graph answers in int32 as in int64", &
                         test_components_empty_graph_int32), &
            new_unittest("rebuild_for re-tunes through every forced and degenerate arm", &
                         test_retune_honours_every_arm), &
            new_unittest("copy=.false. reaches every bound and combine= accept", &
                         test_copy_false_reaches_every_accept), &
            new_unittest("a periodic copy=.false. index ranks a per-point radius", &
                         test_periodic_per_point_radius), &
            new_unittest("the remaining single-route branches", &
                         test_remaining_single_routes), &
            new_unittest("the bound and the los accept run in every loop", &
                         test_bound_and_los_accept_in_every_loop), &
            new_unittest("the chosen ball and cylinder routes on a copy=.false. index", &
                         test_los_chosen_walks_on_a_no_copy_index), &
            new_unittest("the same points give the same cell twice", test_probe_is_deterministic), &
            new_unittest("every cell size gives the same answers", test_answers_survive_any_cell), &
            new_unittest("an explicit cell is honoured", test_explicit_cell_is_honoured), &
            new_unittest("the two ceilings bind where they should", test_two_ceilings_bind), &
            new_unittest("a radius list collapses to its moment ratio", test_effective_radius_moment_ratio), &
            new_unittest("rebuild is a no-op on unchanged data and rebuilds on changed", test_rebuild_detects_change), &
            new_unittest("rebuild_for re-tunes without the caller's arrays", test_rebuild_for_retunes), &
            new_unittest("a distant query radius rebuilds, a near one does not", test_auto_rebuild), &
            new_unittest("a single-point query never rebuilds the index", test_single_queries_never_rebuild), &
            new_unittest("copy=.false. answers exactly as copy=.true.", test_copy_false_matches), &
            new_unittest("the metadata queries report what was built", test_metadata_queries), &
            new_unittest("the HEALPix backend answers every single-point sky query identically", &
                         test_healpix_matches_grid3d), &
            new_unittest("the HEALPix backend answers every BULK sky query identically", &
                         test_healpix_bulk_matches_grid3d), &
            new_unittest("the HEALPix backend answers nearest and kth-distance identically", &
                         test_healpix_nearest_matches_grid3d), &
            new_unittest("a HEALPix index reports its own shape and hides the grid's", &
                         test_healpix_metadata), &
            new_unittest("nside= is honoured, and the resolution probe can be stopped", &
                         test_healpix_nside_forced), &
            new_unittest("rebuild_for re-pixelates and keeps the backend", &
                         test_healpix_rebuild_for), &
            new_unittest("duplicated sky positions tie-break identically on both backends", &
                         test_healpix_duplicate_positions), &
            new_unittest("a disc outgrowing the walk's run buffer still answers identically", &
                         test_healpix_run_buffer_overflow), &
            new_unittest("rebuild_for takes degrees on a sky index, not chords", &
                         test_sky_rebuild_for_takes_degrees), &
            new_unittest("count_within_sky agrees with within_sky's own count", &
                         test_count_within_sky) &
            ]
    end subroutine collect_tests_parquet_spatial

    ! ---- Fixtures and the brute-force oracle ----

    !> A deterministic uniform cloud in `[0, side)^3`, or in `[0, side)^2` when `flat`.
    subroutine make_cloud(n, side, flat, x, y, z)
        integer(int64), intent(in) :: n !! how many points.
        real(real64), intent(in) :: side !! box side.
        logical, intent(in) :: flat !! .true. puts every point on z = 0.
        real(real64), allocatable, intent(out) :: x(:) !! x of every point.
        real(real64), allocatable, intent(out) :: y(:) !! y of every point.
        real(real64), allocatable, intent(out) :: z(:) !! z of every point.
        integer(int64) :: i

        allocate (x(n), y(n), z(n))
        do i = 1_int64, n
            x(i) = side * pf_random_at(fixture_seed, i, 1_int64)
            y(i) = side * pf_random_at(fixture_seed, i, 2_int64)
            if (flat) then
                z(i) = 0.0_real64
            else
                z(i) = side * pf_random_at(fixture_seed, i, 3_int64)
            end if
        end do
    end subroutine make_cloud

    !> A deterministic CLUSTERED cloud: tight clumps scattered through the unit box.
    !>
    !> Where `make_cloud` is uniform and so has one density everywhere, this one's local density
    !> varies by orders of magnitude -- which is what any test about a density-derived starting
    !> radius has to have, since on a uniform cloud that radius is already right everywhere.
    subroutine make_clustered_cloud(n, x, y, z)
        integer(int64), intent(in) :: n !! how many points.
        real(real64), allocatable, intent(out) :: x(:) !! x of every point.
        real(real64), allocatable, intent(out) :: y(:) !! y of every point.
        real(real64), allocatable, intent(out) :: z(:) !! z of every point.
        integer(int64) :: i, c
        real(real64) :: cx, cy, cz

        allocate (x(n), y(n), z(n))
        do i = 1_int64, n
            if (mod(i, 10_int64) == 0_int64) then
                ! One point in ten is scattered, so the clumps sit in a sparse background rather
                ! than in a vacuum -- which is where a carried-over radius has to cope with both.
                x(i) = pf_random_at(fixture_seed + 51_int64, i, 1_int64)
                y(i) = pf_random_at(fixture_seed + 51_int64, i, 2_int64)
                z(i) = pf_random_at(fixture_seed + 51_int64, i, 3_int64)
                cycle
            end if
            c = 1_int64 + mod(i, 20_int64)
            cx = pf_random_at(fixture_seed + 52_int64, c, 1_int64)
            cy = pf_random_at(fixture_seed + 52_int64, c, 2_int64)
            cz = pf_random_at(fixture_seed + 52_int64, c, 3_int64)
            x(i) = cx + 0.02_real64 * (pf_random_at(fixture_seed + 53_int64, i, 1_int64) - 0.5_real64)
            y(i) = cy + 0.02_real64 * (pf_random_at(fixture_seed + 53_int64, i, 2_int64) - 0.5_real64)
            z(i) = cz + 0.02_real64 * (pf_random_at(fixture_seed + 53_int64, i, 3_int64) - 0.5_real64)
        end do
    end subroutine make_clustered_cloud

    !> Every row within `r` of `p`, found by scanning every point. The only oracle here that does
    !> not go through the grid, and so the only one a defect in the cell walk cannot satisfy.
    subroutine brute_within(x, y, z, p, r, wrap, out, m)
        real(real64), intent(in) :: x(:) !! x of every point.
        real(real64), intent(in) :: y(:) !! y of every point.
        real(real64), intent(in) :: z(:) !! z of every point.
        real(real64), intent(in) :: p(3) !! the query point.
        real(real64), intent(in) :: r !! the search radius.
        real(real64), intent(in) :: wrap(3) !! box length per periodic axis, 0 on a free one.
        integer(int64), allocatable, intent(out) :: out(:) !! the rows found, ascending.
        integer(int64), intent(out) :: m !! how many rows were found.
        integer(int64) :: i, n
        real(real64) :: dx, dy, dz

        n = size(x, kind=int64)
        allocate (out(n))
        m = 0_int64
        do i = 1_int64, n
            dx = x(i) - p(1)
            dy = y(i) - p(2)
            dz = z(i) - p(3)
            if (wrap(1) > 0.0_real64) dx = dx - wrap(1) * anint(dx / wrap(1))
            if (wrap(2) > 0.0_real64) dy = dy - wrap(2) * anint(dy / wrap(2))
            if (wrap(3) > 0.0_real64) dz = dz - wrap(3) * anint(dz / wrap(3))
            if (dx * dx + dy * dy + dz * dz <= r * r) then
                m = m + 1_int64
                out(m) = i
            end if
        end do
    end subroutine brute_within

    !> Whether two row lists hold the same rows, in any order.
    logical function same_rows(a, na, b, nb) result(same)
        integer(int64), intent(in) :: a(:) !! the first list.
        integer(int64), intent(in) :: na !! how many entries of `a` count.
        integer(int64), intent(in) :: b(:) !! the second list.
        integer(int64), intent(in) :: nb !! how many entries of `b` count.
        integer(int64), allocatable :: p(:), q(:)

        same = .false.
        if (na /= nb) return
        if (na == 0_int64) then
            same = .true.
            return
        end if
        call pf_sort(a(1:na), p)
        call pf_sort(b(1:nb), q)
        same = all(p == q)
    end function same_rows

    !> Every row inside an axis-shaped region, found by scanning every point.
    !>
    !> **This oracle shares the accept TEST with the implementation on purpose** -- that formula is
    !> the definition of the shape, and what is under test here is the cell walk: whether a
    !> per-slab traversal finds every point a full scan would. The accept test itself is checked
    !> independently by `test_axis_shapes_by_hand`, which asserts membership for points placed at
    !> known offsets from a known axis.
    subroutine brute_axis(x, y, z, p1, p2, r1, r2, clamp, out, m)
        real(real64), intent(in) :: x(:) !! x of every point.
        real(real64), intent(in) :: y(:) !! y of every point.
        real(real64), intent(in) :: z(:) !! z of every point.
        real(real64), intent(in) :: p1(3) !! one end of the axis.
        real(real64), intent(in) :: p2(3) !! the other end.
        real(real64), intent(in) :: r1 !! radius at `p1`.
        real(real64), intent(in) :: r2 !! radius at `p2`.
        logical, intent(in) :: clamp !! .true. gives round ends, .false. flat ones.
        integer(int64), allocatable, intent(out) :: out(:) !! the rows found, ascending.
        integer(int64), intent(out) :: m !! how many rows were found.
        integer(int64) :: i, n
        real(real64) :: dv(3), q(3), w(3), dd, tp, rad, d2

        n = size(x, kind=int64)
        allocate (out(n))
        m = 0_int64
        dv = p2 - p1
        dd = dot_product(dv, dv)
        do i = 1_int64, n
            q = [x(i), y(i), z(i)] - p1
            if (dd <= 0.0_real64) then
                d2 = dot_product(q, q)
                rad = max(r1, r2)
            else
                tp = dot_product(q, dv) / dd
                if (clamp) then
                    tp = min(max(tp, 0.0_real64), 1.0_real64)
                else if (tp < 0.0_real64 .or. tp > 1.0_real64) then
                    cycle
                end if
                rad = r1 + tp * (r2 - r1)
                w = q - tp * dv
                d2 = dot_product(w, w)
            end if
            if (d2 <= rad * rad) then
                m = m + 1_int64
                out(m) = i
            end if
        end do
    end subroutine brute_axis

    !> A deterministic sky catalogue that deliberately piles points where a naive implementation
    !> breaks: at both poles and straddling 0h, plus a scattered background.
    subroutine make_sky(n, ra, dec)
        integer(int64), intent(in) :: n !! how many points; a multiple of 4 is tidiest.
        real(real64), allocatable, intent(out) :: ra(:) !! right ascension, degrees, in [0, 360).
        real(real64), allocatable, intent(out) :: dec(:) !! declination, degrees, in [-90, 90].
        integer(int64) :: i, q

        allocate (ra(n), dec(n))
        do i = 1_int64, n
            q = mod(i - 1_int64, 4_int64)
            select case (q)
            case (0)
                ! North polar cap: every right ascension, all within 3 degrees of the pole.
                ra(i) = 360.0_real64 * pf_random_at(fixture_seed, i, 21_int64)
                dec(i) = 90.0_real64 - 3.0_real64 * pf_random_at(fixture_seed, i, 22_int64)
            case (1)
                ! South polar cap.
                ra(i) = 360.0_real64 * pf_random_at(fixture_seed, i, 23_int64)
                dec(i) = -90.0_real64 + 3.0_real64 * pf_random_at(fixture_seed, i, 24_int64)
            case (2)
                ! Straddling 0h: a 6-degree band centred on it, wrapped into [0, 360).
                ra(i) = modulo(357.0_real64 + 6.0_real64 * pf_random_at(fixture_seed, i, 25_int64), 360.0_real64)
                dec(i) = -3.0_real64 + 6.0_real64 * pf_random_at(fixture_seed, i, 26_int64)
            case default
                ra(i) = 360.0_real64 * pf_random_at(fixture_seed, i, 27_int64)
                dec(i) = -60.0_real64 + 120.0_real64 * pf_random_at(fixture_seed, i, 28_int64)
            end select
        end do
    end subroutine make_sky

    !> The angular separation between two sky positions, in degrees, by the HAVERSINE formula.
    !>
    !> **Deliberately not the library's route.** The implementation converts to unit vectors and
    !> compares chords; this works in `(ra, dec)` throughout, so an error in the conversion cannot
    !> hide in both. It is also the formula an astronomer would check the answer with by hand.
    real(real64) function sky_sep(ra1, dec1, ra2, dec2) result(sep)
        real(real64), intent(in) :: ra1 !! right ascension of the first point, degrees.
        real(real64), intent(in) :: dec1 !! declination of the first point, degrees.
        real(real64), intent(in) :: ra2 !! right ascension of the second point, degrees.
        real(real64), intent(in) :: dec2 !! declination of the second point, degrees.
        real(real64), parameter :: d2r = 0.017453292519943295_real64
        real(real64) :: sd, sr, h

        sd = sin(0.5_real64 * (dec2 - dec1) * d2r)
        sr = sin(0.5_real64 * (ra2 - ra1) * d2r)
        h = sd * sd + cos(dec1 * d2r) * cos(dec2 * d2r) * sr * sr
        sep = 2.0_real64 * asin(min(sqrt(max(h, 0.0_real64)), 1.0_real64)) / d2r
    end function sky_sep

    !> Every row within `rsky` degrees of `(ra0, dec0)`, by scanning the whole catalogue.
    subroutine brute_sky(ra, dec, ra0, dec0, rsky, out, m)
        real(real64), intent(in) :: ra(:) !! right ascension of every point, degrees.
        real(real64), intent(in) :: dec(:) !! declination of every point, degrees.
        real(real64), intent(in) :: ra0 !! right ascension of the query point, degrees.
        real(real64), intent(in) :: dec0 !! declination of the query point, degrees.
        real(real64), intent(in) :: rsky !! the angular search radius, degrees.
        integer(int64), allocatable, intent(out) :: out(:) !! the rows found, ascending.
        integer(int64), intent(out) :: m !! how many rows were found.
        integer(int64) :: i, n

        n = size(ra, kind=int64)
        allocate (out(n))
        m = 0_int64
        do i = 1_int64, n
            if (sky_sep(ra0, dec0, ra(i), dec(i)) <= rsky) then
                m = m + 1_int64
                out(m) = i
            end if
        end do
    end subroutine brute_sky

    ! ---- Oracles for the Tier 2 queries ----

    !> The rows nearest `p`, in increasing distance with ties by ascending row, found by a full
    !> sort of every distance. Independent of the grid, of the expanding ball and of its cap.
    subroutine brute_nearest(x, y, z, p, wrap, rows, dists)
        real(real64), intent(in) :: x(:) !! x of every point.
        real(real64), intent(in) :: y(:) !! y of every point.
        real(real64), intent(in) :: z(:) !! z of every point.
        real(real64), intent(in) :: p(3) !! the query point.
        real(real64), intent(in) :: wrap(3) !! box length per periodic axis, 0 on a free one.
        integer(int64), allocatable, intent(out) :: rows(:) !! every row, nearest first.
        real(real64), allocatable, intent(out) :: dists(:) !! the matching distances.
        integer(int64), allocatable :: perm(:)
        real(real64), allocatable :: d(:)
        real(real64) :: dx, dy, dz
        integer(int64) :: i, n

        n = size(x, kind=int64)
        allocate (d(n))
        do i = 1_int64, n
            dx = x(i) - p(1)
            dy = y(i) - p(2)
            dz = z(i) - p(3)
            if (wrap(1) > 0.0_real64) dx = dx - wrap(1) * anint(dx / wrap(1))
            if (wrap(2) > 0.0_real64) dy = dy - wrap(2) * anint(dy / wrap(2))
            if (wrap(3) > 0.0_real64) dz = dz - wrap(3) * anint(dz / wrap(3))
            d(i) = sqrt(dx * dx + dy * dy + dz * dz)
        end do
        ! Stable, so an exact tie comes back in ascending row order -- which is the same tie-break
        ! the library contracts for, so the two lists are comparable entry by entry.
        call pf_argsort(d, perm)
        allocate (rows(n), dists(n))
        do i = 1_int64, n
            rows(i) = perm(i)
            dists(i) = d(perm(i))
        end do
    end subroutine brute_nearest

    !> Every point's distance to its `k`-th nearest OTHER point, by a full per-point sort.
    subroutine brute_kth(x, y, z, k, dist)
        real(real64), intent(in) :: x(:) !! x of every point.
        real(real64), intent(in) :: y(:) !! y of every point.
        real(real64), intent(in) :: z(:) !! z of every point.
        integer(int64), intent(in) :: k !! which neighbour to report.
        real(real64), allocatable, intent(out) :: dist(:) !! length n, in row order.
        real(real64), allocatable :: d(:), sorted_d(:)
        integer(int64) :: i, j, n, w

        n = size(x, kind=int64)
        allocate (dist(n), d(n - 1_int64))
        do i = 1_int64, n
            w = 0_int64
            do j = 1_int64, n
                if (j == i) cycle
                w = w + 1_int64
                d(w) = sqrt((x(i) - x(j)) ** 2 + (y(i) - y(j)) ** 2 + (z(i) - z(j)) ** 2)
            end do
            call pf_sort(d, sorted_d)
            dist(i) = sorted_d(k)
        end do
    end subroutine brute_kth

    !> Component labels by LABEL PROPAGATION over an O(n^2) adjacency, which shares no machinery
    !> with the union-find under test -- only the definition of "connected".
    subroutine brute_components(x, y, z, r, lab)
        real(real64), intent(in) :: x(:) !! x of every point.
        real(real64), intent(in) :: y(:) !! y of every point.
        real(real64), intent(in) :: z(:) !! z of every point.
        real(real64), intent(in) :: r !! the linking length.
        integer(int64), allocatable, intent(out) :: lab(:) !! a label per vertex; values are arbitrary.
        integer(int64) :: i, j, n, lo
        real(real64) :: d2
        logical :: moved

        n = size(x, kind=int64)
        allocate (lab(n))
        do i = 1_int64, n
            lab(i) = i
        end do
        moved = .true.
        do while (moved)
            moved = .false.
            do i = 1_int64, n
                do j = i + 1_int64, n
                    d2 = (x(i) - x(j)) ** 2 + (y(i) - y(j)) ** 2 + (z(i) - z(j)) ** 2
                    if (d2 > r * r) cycle
                    lo = min(lab(i), lab(j))
                    if (lab(i) /= lo .or. lab(j) /= lo) moved = .true.
                    lab(i) = lo
                    lab(j) = lo
                end do
            end do
        end do
    end subroutine brute_components

    !> Whether two labellings induce the same PARTITION, whatever numbers each one chose.
    logical function same_partition(a, b) result(same)
        integer(int64), intent(in) :: a(:) !! one labelling.
        integer(int64), intent(in) :: b(:) !! the other, over the same vertices.
        integer(int64) :: i, j, n

        same = .true.
        n = size(a, kind=int64)
        do i = 1_int64, n
            do j = i + 1_int64, n
                if ((a(i) == a(j)) .neqv. (b(i) == b(j))) then
                    same = .false.
                    return
                end if
            end do
        end do
    end function same_partition

    ! ---- Ball search ----

    !> Ball search against the brute-force oracle, over several radii and query points.
    subroutine test_ball_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        integer :: ir
        real(real64) :: p(3), r, radii(3)

        n = 800_int64
        radii = [0.05_real64, 0.15_real64, 0.4_real64]
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=radii)
        allocate (got(n))
        do ir = 1, 3
            r = radii(ir)
            do q = 1_int64, 25_int64
                p(1) = pf_random_at(fixture_seed + 7_int64, q, 1_int64)
                p(2) = pf_random_at(fixture_seed + 7_int64, q, 2_int64)
                p(3) = pf_random_at(fixture_seed + 7_int64, q, 3_int64)
                mg = sx%within(p, r, got)
                call brute_within(x, y, z, p, r, NO_WRAP, want, mw)
                call check(error, same_rows(got, mg, want, mw), &
                    "ball search must return exactly the rows a brute-force scan finds")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_ball_matches_brute_force

    !> A query far outside the cloud finds nothing, and does not walk off the grid doing it.
    subroutine test_ball_outside_finds_nothing(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(16), m
        type(pf_spatial_index) :: sx

        call make_cloud(200_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.1_real64)
        m = sx%within([50.0_real64, 50.0_real64, 50.0_real64], 0.1_real64, got)
        call check(error, m == 0_int64, "a query far outside the cloud must find nothing")
        if (allocated(error)) return
        m = sx%within([-50.0_real64, 0.5_real64, 0.5_real64], 0.1_real64, got)
        call check(error, m == 0_int64, "a query below the grid on one axis must find nothing")
    end subroutine test_ball_outside_finds_nothing

    !> A radius that encloses the whole cloud returns every row exactly once.
    subroutine test_ball_covers_all(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:)
        integer(int64) :: n, m
        type(pf_spatial_index) :: sx

        n = 300_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.05_real64)
        allocate (got(n))
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 10.0_real64, got)
        call check(error, m == n, "a radius enclosing the cloud must return every row")
        if (allocated(error)) return
        call check(error, same_rows(got, m, [(m, m=1_int64, n)], n), &
            "a radius enclosing the cloud must return each row exactly once")
    end subroutine test_ball_covers_all

    !> A single point, and a cloud whose points are all identical, both build and answer.
    subroutine test_ball_degenerate_clouds(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64) :: one(1), many(64)
        integer(int64) :: got(64), m
        type(pf_spatial_index) :: sx

        one = 3.0_real64
        call sx%build(one, one, one, radius=1.0_real64)
        m = sx%within([3.0_real64, 3.0_real64, 3.0_real64], 0.5_real64, got)
        call check(error, m == 1_int64, "a one-point cloud must return its single row")
        if (allocated(error)) return
        m = sx%within([9.0_real64, 3.0_real64, 3.0_real64], 0.5_real64, got)
        call check(error, m == 0_int64, "a one-point cloud must return nothing for a distant query")
        if (allocated(error)) return

        many = 2.5_real64
        call sx%build(many, many, many, radius=1.0_real64)
        m = sx%within([2.5_real64, 2.5_real64, 2.5_real64], 0.1_real64, got)
        call check(error, m == 64_int64, "coincident points must all be found at their own position")
    end subroutine test_ball_degenerate_clouds

    !> A buffer shorter than the answer is filled as far as it reaches, and the TRUE count comes
    !> back so the caller can size and retry rather than guess.
    subroutine test_within_short_buffer(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: small(4), big(400), m_small, m_big
        type(pf_spatial_index) :: sx
        real(real64) :: p(3)

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.3_real64)
        p = 0.5_real64
        m_big = sx%within(p, 0.3_real64, big)
        m_small = sx%within(p, 0.3_real64, small)
        call check(error, m_small == m_big, "a short buffer must still report the true count")
        if (allocated(error)) return
        call check(error, m_big > 4_int64, "the fixture must actually overflow the short buffer")
        if (allocated(error)) return
        call check(error, same_rows(small, 4_int64, big(1:4), 4_int64), &
            "the first entries of a short buffer must be rows the full answer also holds")
    end subroutine test_within_short_buffer

    !> Every reported distance matches the coordinates of the row it belongs to.
    subroutine test_within_distances(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(400), m, k, i
        real(real64) :: d(400), p(3), want
        type(pf_spatial_index) :: sx

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.25_real64)
        p = [0.4_real64, 0.6_real64, 0.55_real64]
        m = sx%within(p, 0.25_real64, got, dist=d)
        call check(error, m > 0_int64, "the distance fixture must find some neighbours")
        if (allocated(error)) return
        do k = 1_int64, m
            i = got(k)
            want = sqrt((x(i) - p(1))**2 + (y(i) - p(2))**2 + (z(i) - p(3))**2)
            call check(error, abs(d(k) - want) <= 1.0e-12_real64, &
                "a reported distance must match the coordinates of the row it belongs to")
            if (allocated(error)) return
            call check(error, d(k) <= 0.25_real64 + 1.0e-12_real64, &
                "a reported distance must not exceed the search radius")
            if (allocated(error)) return
        end do
    end subroutine test_within_distances

    !> `%count_within` answers exactly what `%within` counts.
    subroutine test_count_within_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(500), m, c, q
        real(real64) :: p(3)
        type(pf_spatial_index) :: sx

        call make_cloud(500_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        do q = 1_int64, 20_int64
            p(1) = pf_random_at(fixture_seed + 11_int64, q, 1_int64)
            p(2) = pf_random_at(fixture_seed + 11_int64, q, 2_int64)
            p(3) = pf_random_at(fixture_seed + 11_int64, q, 3_int64)
            m = sx%within(p, 0.2_real64, got)
            c = sx%count_within(p, 0.2_real64)
            call check(error, c == m, "count_within must agree with within's count")
            if (allocated(error)) return
        end do
    end subroutine test_count_within_agrees

    !> The int32 buffer overload returns the same rows as the int64 one.
    subroutine test_within_int32_buffer(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int32) :: got32(400)
        integer(int64) :: got64(400), m32, m64
        real(real64) :: p(3)
        type(pf_spatial_index) :: sx

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.25_real64)
        p = 0.5_real64
        m64 = sx%within(p, 0.25_real64, got64)
        m32 = sx%within(p, 0.25_real64, got32)
        call check(error, m32 == m64, "the two buffer kinds must report the same count")
        if (allocated(error)) return
        call check(error, m64 > 0_int64, "the int32-buffer fixture must find some neighbours")
        if (allocated(error)) return
        call check(error, same_rows(int(got32(1:m32), kind=int64), m32, got64, m64), &
            "the two buffer kinds must report the same rows")
    end subroutine test_within_int32_buffer

    ! ---- Two dimensions ----

    !> The 2D index -- the 3D one with `z` omitted -- against the brute-force oracle.
    subroutine test_2d_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        real(real64) :: p(2), p3(3)

        n = 700_int64
        call make_cloud(n, 1.0_real64, .true., x, y, z)
        call sx%build(x, y, radius=0.12_real64)
        allocate (got(n))
        do q = 1_int64, 30_int64
            p(1) = pf_random_at(fixture_seed + 3_int64, q, 1_int64)
            p(2) = pf_random_at(fixture_seed + 3_int64, q, 2_int64)
            p3 = [p(1), p(2), 0.0_real64]
            mg = sx%within(p, 0.12_real64, got)
            call brute_within(x, y, z, p3, 0.12_real64, NO_WRAP, want, mw)
            call check(error, same_rows(got, mg, want, mw), &
                "a 2D index must return exactly the rows a 2D brute-force scan finds")
            if (allocated(error)) return
        end do
    end subroutine test_2d_matches_brute_force

    !> The grid really is flat: `nz == 1`, so the walk's k loop runs once and no cells are wasted.
    subroutine test_2d_grid_is_flat(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: nx, ny, nz

        call make_cloud(500_int64, 1.0_real64, .true., x, y, z)
        call sx%build(x, y, radius=0.1_real64)
        call sx%grid(nx, ny, nz)
        call check(error, nz == 1_int64, "a 2D index must have exactly one cell along z")
        if (allocated(error)) return
        call check(error, nx > 1_int64 .and. ny > 1_int64, &
            "a 2D index must still divide x and y into several cells")
        if (allocated(error)) return
        call check(error, sx%ndim() == 2, "a 2D index must report two dimensions")
        if (allocated(error)) return
        call check(error, sx%cells() == nx * ny, "a 2D index's cell count must be nx*ny")
    end subroutine test_2d_grid_is_flat

    !> The three degenerate shapes the provisional bucketing's cube root cannot handle on its own:
    !> a flat cloud, a collinear one and a single repeated point. Each must build and answer.
    subroutine test_degenerate_axes_build(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64) :: x(200), y(200), zz(200)
        integer(int64) :: got(200), m, i
        type(pf_spatial_index) :: sx

        do i = 1_int64, 200_int64
            x(i) = pf_random_at(fixture_seed + 5_int64, i, 1_int64)
            y(i) = pf_random_at(fixture_seed + 5_int64, i, 2_int64)
        end do
        zz = 0.0_real64
        ! Flat: a zero-extent z, which is what makes `(bbox volume / n)^(1/3)` zero.
        call sx%build(x, y, zz, radius=0.1_real64)
        m = sx%within([0.5_real64, 0.5_real64, 0.0_real64], 0.1_real64, got)
        call check(error, m > 0_int64, "a flat 3D cloud must build and find neighbours")
        if (allocated(error)) return
        ! Collinear: two zero-extent axes.
        y = 0.0_real64
        call sx%build(x, y, zz, radius=0.1_real64)
        m = sx%within([0.5_real64, 0.0_real64, 0.0_real64], 0.1_real64, got)
        call check(error, m > 0_int64, "a collinear cloud must build and find neighbours")
        if (allocated(error)) return
        ! Every axis degenerate.
        x = 1.0_real64
        call sx%build(x, y, zz, radius=0.1_real64)
        m = sx%within([1.0_real64, 0.0_real64, 0.0_real64], 0.1_real64, got)
        call check(error, m == 200_int64, "a fully coincident cloud must find every row")
        if (allocated(error)) return
        call check(error, sx%cells() == 1_int64, "a fully coincident cloud must collapse to one cell")
    end subroutine test_degenerate_axes_build

    ! ---- Bulk queries ----

    !> The CSR self-join reproduces N separate single queries, which makes its correctness
    !> independent of the bulk two-pass machinery.
    subroutine test_all_within_matches_singles(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: offs(:), nb(:), got(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, m, s0, e0
        real(real64) :: r

        n = 600_int64
        r = 0.15_real64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=r)
        call sx%all_within(r, offs, nb)
        call check(error, size(offs, kind=int64) == n + 1_int64, "the CSR offsets must be one longer than the cloud")
        if (allocated(error)) return
        call check(error, offs(1) == 1_int64, "the CSR offsets must start at 1")
        if (allocated(error)) return
        allocate (got(n))
        do i = 1_int64, n
            m = sx%within([x(i), y(i), z(i)], r, got)
            s0 = offs(i)
            e0 = offs(i + 1_int64) - 1_int64
            call check(error, same_rows(nb(s0:e0), e0 - s0 + 1_int64, got, m), &
                "each CSR row must hold exactly what a single query for that point returns")
            if (allocated(error)) return
        end do
    end subroutine test_all_within_matches_singles

    !> A per-point radius array is applied per point, not uniformly.
    subroutine test_all_within_per_point_radius(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rr(:)
        integer(int64), allocatable :: offs(:), nb(:), got(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, m
        logical :: varies

        n = 400_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rr(n))
        do i = 1_int64, n
            rr(i) = 0.05_real64 + 0.25_real64 * pf_random_at(fixture_seed + 13_int64, i, 1_int64)
        end do
        call sx%build(x, y, z, radius=rr)
        call sx%all_within(rr, offs, nb)
        allocate (got(n))
        varies = .false.
        do i = 1_int64, n
            m = sx%within([x(i), y(i), z(i)], rr(i), got)
            call check(error, same_rows(nb(offs(i):offs(i + 1_int64) - 1_int64), &
                offs(i + 1_int64) - offs(i), got, m), &
                "a per-point radius must be applied to the point it belongs to")
            if (allocated(error)) return
            if (m /= offs(2) - offs(1)) varies = .true.
        end do
        ! Without this the test would pass just as happily against an implementation that used
        ! radii(1) for every point, since row 1 would then still agree.
        call check(error, varies, "the per-point fixture must produce different counts for different rows")
    end subroutine test_all_within_per_point_radius

    !> `%count_all_within` agrees with the CSR the same sweep would build.
    subroutine test_count_all_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: offs(:), nb(:), counts(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call sx%all_within(0.2_real64, offs, nb)
        call sx%count_all_within(0.2_real64, counts)
        call check(error, size(counts, kind=int64) == n, "count_all_within must return one count per point")
        if (allocated(error)) return
        do i = 1_int64, n
            call check(error, counts(i) == offs(i + 1_int64) - offs(i), &
                "count_all_within must agree with the CSR row length for every point")
            if (allocated(error)) return
        end do
    end subroutine test_count_all_agrees

    !> The edge list holds every neighbouring pair exactly once, with `i < j`.
    subroutine test_pairs_match_all_within(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: offs(:), nb(:), pi(:), pj(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, k, want_pairs, csr_total

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.18_real64)
        call sx%all_within(0.18_real64, offs, nb)
        call sx%pairs_within(0.18_real64, pi, pj)
        csr_total = offs(n + 1_int64) - 1_int64
        ! The CSR counts every point as its own neighbour and every pair from both ends, so the
        ! edge list must be exactly (csr_total - n) / 2 long. That is an independent count rather
        ! than a restatement of the same sweep.
        want_pairs = (csr_total - n) / 2_int64
        call check(error, size(pi, kind=int64) == want_pairs, &
            "the edge list must hold each neighbouring pair exactly once")
        if (allocated(error)) return
        call check(error, want_pairs > 0_int64, "the pair fixture must actually find some pairs")
        if (allocated(error)) return
        call check(error, all(pi < pj), "every reported pair must have i < j")
        if (allocated(error)) return
        do k = 1_int64, min(want_pairs, 200_int64)
            i = pi(k)
            call check(error, any(nb(offs(i):offs(i + 1_int64) - 1_int64) == pj(k)), &
                "every reported pair must appear in the CSR row of its lower endpoint")
            if (allocated(error)) return
        end do
    end subroutine test_pairs_match_all_within

    !> With one radius per point, the edge list is every pair within `max(r_i, r_j)` -- exactly.
    !>
    !> **This is the test that pins the symmetric rule.** A per-point radius makes "who was doing
    !> the searching" observable: emitting from the lower row with its own radius drops every pair
    !> that only the larger ball reaches, and the result still looks like a perfectly ordinary
    !> neighbour list. `want_low` counts what that rule would have produced and the fixture is
    !> asserted to separate the two, so this cannot pass against a sweep that ignores the ranking.
    subroutine test_pairs_per_point_radius_is_symmetric(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rv(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :), got(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, b, k, want_max, want_low
        real(real64) :: d2, rmax, rlow

        n = 300_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        do a = 1_int64, n
            rv(a) = 0.03_real64 + 0.22_real64 * pf_random_at(fixture_seed, a, 11_int64)
        end do
        allocate (want(n, n), got(n, n))
        want = .false.
        got = .false.
        want_max = 0_int64
        want_low = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                d2 = (x(a) - x(b))**2 + (y(a) - y(b))**2 + (z(a) - z(b))**2
                rmax = max(rv(a), rv(b))
                if (d2 <= rmax * rmax) then
                    want(a, b) = .true.
                    want_max = want_max + 1_int64
                end if
                ! The rule a sweep applies when the LOWER row always does the searching with its
                ! own radius: the negative control for everything below.
                rlow = rv(a)
                if (d2 <= rlow * rlow) want_low = want_low + 1_int64
            end do
        end do
        call check(error, want_max > want_low, &
            "the fixture must separate the symmetric rule from the lower-row-searches rule")
        if (allocated(error)) return
        call sx%build(x, y, z, radius=rv)
        call sx%pairs_within(rv, pi, pj)
        call check(error, size(pi, kind=int64) == want_max, &
            "the edge list must hold every pair within max(r_i, r_j), and no others")
        if (allocated(error)) return
        call check(error, all(pi < pj), "every reported pair must have i < j")
        if (allocated(error)) return
        do k = 1_int64, size(pi, kind=int64)
            got(pi(k), pj(k)) = .true.
        end do
        ! Set equality, not just a count: a count alone cannot tell a duplicated pair plus a
        ! missing one from the right answer.
        call check(error, all(got .eqv. want), &
            "the edge list must be exactly the pairs within max(r_i, r_j)")
    end subroutine test_pairs_per_point_radius_is_symmetric

    !> A radius vector whose entries are all equal gives the scalar form's answer, pair for pair.
    !>
    !> The ranking machinery only runs on the vector path, so without this a defect in it that
    !> happens to preserve the pair COUNT would show up nowhere: the symmetric test above varies
    !> the radii, and every other pair test takes the scalar path and never builds a rank at all.
    subroutine test_pairs_uniform_vector_matches_scalar(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rv(:)
        integer(int64), allocatable :: si(:), sj(:), vi(:), vj(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n
        real(real64), parameter :: r = 0.18_real64

        n = 400_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        rv = r
        call sx%build(x, y, z, radius=r)
        call sx%pairs_within(r, si, sj)
        call sx%pairs_within(rv, vi, vj)
        call check(error, size(si, kind=int64) > 0_int64, "the fixture must actually find some pairs")
        if (allocated(error)) return
        call check(error, size(vi, kind=int64) == size(si, kind=int64), &
            "a uniform radius vector must find the same number of pairs as the scalar form")
        if (allocated(error)) return
        call check(error, all(vi == si) .and. all(vj == sj), &
            "a uniform radius vector must give the scalar form's pair list, pair for pair")
    end subroutine test_pairs_uniform_vector_matches_scalar

    !> The bound one `PF_LINK_*` rule puts on a pair, stated independently of the library.
    !>
    !> **Deliberately written from the rule's plain-language description**, not from the product
    !> form the sweep uses: an oracle that reproduced `u_i*v_j + v_i*u_j` would agree with a wrong
    !> derivation of those terms. On the sky the arguments are DEGREES, which is the whole content
    !> of the mean and sum rules there.
    real(real64) function link_bound(a, b, rule) result(v)
        real(real64), intent(in) :: a !! one endpoint's radius, or angular radius in degrees.
        real(real64), intent(in) :: b !! the other endpoint's.
        integer, intent(in) :: rule !! one of the four `PF_LINK_*` constants.

        select case (rule)
        case (PF_LINK_MAX)
            v = max(a, b)
        case (PF_LINK_MIN)
            v = min(a, b)
        case (PF_LINK_MEAN)
            v = 0.5_real64 * (a + b)
        case default
            v = a + b
        end select
    end function link_bound

    !> Every `combine=` rule reproduces a brute-force scan applying that rule's own definition.
    !>
    !> **The fixture has to separate the four rules from one another**, or a sweep that ignored
    !> `combine=` entirely would pass four times over: the strict ordering of the four counts is
    !> asserted before any of them is compared. It must also contain pairs the searcher's OWN ball
    !> cannot reach, which is what makes the sum rule's doubled walk load-bearing rather than
    !> merely generous -- `beyond` counts those and is asserted non-zero.
    subroutine test_pairs_combine_rules_match_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rv(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :), got(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, b, k, nwant, beyond, counted(4)
        real(real64) :: d2, bnd
        integer :: rules(4), ri, rule

        rules = [PF_LINK_MIN, PF_LINK_MEAN, PF_LINK_MAX, PF_LINK_SUM]
        n = 300_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        do a = 1_int64, n
            rv(a) = 0.03_real64 + 0.22_real64 * pf_random_at(fixture_seed, a, 11_int64)
        end do
        call sx%build(x, y, z, radius=rv)
        allocate (want(n, n), got(n, n))
        do ri = 1, 4
            rule = rules(ri)
            want = .false.
            got = .false.
            nwant = 0_int64
            beyond = 0_int64
            do a = 1_int64, n - 1_int64
                do b = a + 1_int64, n
                    d2 = (x(a) - x(b))**2 + (y(a) - y(b))**2 + (z(a) - z(b))**2
                    bnd = link_bound(rv(a), rv(b), rule)
                    if (d2 <= bnd * bnd) then
                        want(a, b) = .true.
                        nwant = nwant + 1_int64
                        ! Out of reach of either endpoint's own ball, so only a widened walk finds
                        ! it. Empty for every rule but the sum.
                        if (d2 > max(rv(a), rv(b))**2) beyond = beyond + 1_int64
                    end if
                end do
            end do
            counted(ri) = nwant
            if (rule == PF_LINK_SUM) then
                call check(error, beyond > 0_int64, &
                    "the fixture must contain pairs no endpoint's own ball reaches, or the " // &
                    "sum rule's doubled walk is never exercised")
                if (allocated(error)) return
            end if
            call sx%pairs_within(rv, pi, pj, combine=rule)
            call check(error, size(pi, kind=int64) == nwant, &
                "each combine= rule must report exactly the pairs its own definition accepts")
            if (allocated(error)) return
            call check(error, all(pi < pj), "every reported pair must have i < j, whatever the rule")
            if (allocated(error)) return
            do k = 1_int64, size(pi, kind=int64)
                got(pi(k), pj(k)) = .true.
            end do
            ! Set equality, not a count: a count alone cannot tell a duplicated pair plus a
            ! missing one from the right answer.
            call check(error, all(got .eqv. want), &
                "each combine= rule's edge list must be exactly its own accepted set")
            if (allocated(error)) return
        end do
        ! Last, so that a failure above is reported against the rule that caused it. Strict, in
        ! the order min < mean < max < sum: a sweep answering one rule for all four would satisfy
        ! every assertion above and fail here.
        call check(error, counted(1) < counted(2) .and. counted(2) < counted(3) .and. &
            counted(3) < counted(4), &
            "the fixture must separate all four rules; the four pair counts must strictly increase")
    end subroutine test_pairs_combine_rules_match_oracle

    !> Omitting `combine=` is exactly `PF_LINK_MAX`, the rule the released sweep applies.
    subroutine test_pairs_combine_default_is_max(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rv(:)
        integer(int64), allocatable :: di(:), dj(:), mi(:), mj(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a

        n = 220_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        do a = 1_int64, n
            rv(a) = 0.04_real64 + 0.20_real64 * pf_random_at(fixture_seed, a, 12_int64)
        end do
        call sx%build(x, y, z, radius=rv)
        call sx%pairs_within(rv, di, dj)
        call sx%pairs_within(rv, mi, mj, combine=PF_LINK_MAX)
        call check(error, size(di, kind=int64) == size(mi, kind=int64), &
            "the default rule must return as many pairs as PF_LINK_MAX does")
        if (allocated(error)) return
        ! Element for element: both calls run the same walk in the same order, so the default
        ! must not merely agree as a set.
        call check(error, all(di == mi) .and. all(dj == mj), &
            "omitting combine= must be PF_LINK_MAX, pair for pair")
    end subroutine test_pairs_combine_default_is_max

    !> A uniform radius VECTOR gives the scalar form's answer under every rule.
    !>
    !> The vector path and the scalar path are different code -- the first ranks by radius and may
    !> carry per-candidate terms, the second ranks by row and never does -- so equal radii are the
    !> one input where the two must agree exactly. The sum rule agrees with the scalar form at
    !> TWICE the radius, which is also the statement that its walk is what it claims.
    subroutine test_pairs_combine_uniform_vector(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rv(:)
        integer(int64), allocatable :: si(:), sj(:), vi(:), vj(:)
        logical, allocatable :: want(:, :), got(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, k
        integer :: rules(4), ri
        real(real64), parameter :: rq = 0.12_real64

        rules = [PF_LINK_MAX, PF_LINK_MIN, PF_LINK_MEAN, PF_LINK_SUM]
        n = 260_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        rv = rq
        call sx%build(x, y, z, radius=rq)
        allocate (want(n, n), got(n, n))
        do ri = 1, 4
            if (rules(ri) == PF_LINK_SUM) then
                call sx%pairs_within(2.0_real64 * rq, si, sj)
            else
                call sx%pairs_within(rq, si, sj)
            end if
            call sx%pairs_within(rv, vi, vj, combine=rules(ri))
            call check(error, size(vi, kind=int64) == size(si, kind=int64), &
                "a uniform radius vector must give the scalar form's pair count")
            if (allocated(error)) return
            want = .false.
            got = .false.
            do k = 1_int64, size(si, kind=int64)
                want(si(k), sj(k)) = .true.
            end do
            do k = 1_int64, size(vi, kind=int64)
                got(vi(k), vj(k)) = .true.
            end do
            call check(error, all(got .eqv. want), &
                "a uniform radius vector must give the scalar form's pairs, for every rule")
            if (allocated(error)) return
        end do
    end subroutine test_pairs_combine_uniform_vector

    !> `r_inner=` composes with every rule: it removes the close pairs and touches nothing else.
    subroutine test_pairs_combine_with_inner(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rv(:)
        integer(int64), allocatable :: pi(:), pj(:), qi(:), qj(:)
        logical, allocatable :: want(:, :), got(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, k, dropped
        real(real64) :: d2
        integer :: rules(4), ri
        real(real64), parameter :: rin = 0.05_real64

        rules = [PF_LINK_MAX, PF_LINK_MIN, PF_LINK_MEAN, PF_LINK_SUM]
        n = 260_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        do a = 1_int64, n
            rv(a) = 0.06_real64 + 0.18_real64 * pf_random_at(fixture_seed, a, 13_int64)
        end do
        call sx%build(x, y, z, radius=rv)
        allocate (want(n, n), got(n, n))
        do ri = 1, 4
            call sx%pairs_within(rv, pi, pj, combine=rules(ri))
            call sx%pairs_within(rv, qi, qj, combine=rules(ri), r_inner=rin)
            ! The annulus result is the plain one minus exactly the pairs closer than `rin`.
            want = .false.
            dropped = 0_int64
            do k = 1_int64, size(pi, kind=int64)
                d2 = (x(pi(k)) - x(pj(k)))**2 + (y(pi(k)) - y(pj(k)))**2 + (z(pi(k)) - z(pj(k)))**2
                if (d2 < rin * rin) then
                    dropped = dropped + 1_int64
                else
                    want(pi(k), pj(k)) = .true.
                end if
            end do
            call check(error, dropped > 0_int64, &
                "the fixture must have pairs inside the inner radius, or the annulus is a no-op")
            if (allocated(error)) return
            got = .false.
            do k = 1_int64, size(qi, kind=int64)
                got(qi(k), qj(k)) = .true.
            end do
            call check(error, all(got .eqv. want), &
                "r_inner= must drop exactly the pairs closer than it, under every combine= rule")
            if (allocated(error)) return
        end do
    end subroutine test_pairs_combine_with_inner

    !> On the sky the mean and sum rules combine ANGLES, and a chord-space reading is not the same.
    !>
    !> **The precondition is the whole point of this test.** The chord is concave in the angle, so
    !> combining chords gives a strictly smaller bound than combining the angles whenever the two
    !> radii differ; `split` counts the pairs lying between the two readings, and an implementation
    !> that averaged chords would return a plausible list missing exactly those. A fixture where
    !> `split` is zero would pass against either reading and prove nothing, so it is asserted
    !> non-zero before anything else. Both backends run, since the accept test is shared and a
    !> backend that grew its own copy would differ here first.
    subroutine test_sky_pairs_combine_matches_angle_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), rv(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :), got(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, b, k, nwant, split
        real(real64) :: sep, bnd, chordwise
        integer :: rules(2), ri, bk, backends(2)
        real(real64), parameter :: d2r = 0.017453292519943295_real64

        rules = [PF_LINK_MEAN, PF_LINK_SUM]
        backends = [PF_SKY_GRID3D, PF_SKY_HEALPIX]
        n = 700_int64
        call make_sky(n, ra, dec)
        allocate (rv(n))
        do a = 1_int64, n
            rv(a) = 5.0_real64 + 35.0_real64 * pf_random_at(fixture_seed, a, 41_int64)
        end do
        allocate (want(n, n), got(n, n))
        do ri = 1, 2
            want = .false.
            nwant = 0_int64
            split = 0_int64
            do a = 1_int64, n - 1_int64
                do b = a + 1_int64, n
                    sep = sky_sep(ra(a), dec(a), ra(b), dec(b))
                    bnd = link_bound(rv(a), rv(b), rules(ri))
                    if (sep <= bnd) then
                        want(a, b) = .true.
                        nwant = nwant + 1_int64
                    end if
                    ! The same rule applied to the CHORDS and read back as an angle: what a sweep
                    ! that never converted would answer. **The disagreement is counted in either
                    ! direction**, because the chord is concave with `chord(0) = 0` and the two
                    ! consequences point opposite ways: the mean of two chords falls BELOW the
                    ! chord of the mean angle, while their sum rises ABOVE the chord of the summed
                    ! angle. A one-sided test would be vacuous for one of the two rules.
                    chordwise = 2.0_real64 * asin(min(1.0_real64, 0.5_real64 * &
                        link_bound(2.0_real64 * sin(0.5_real64 * rv(a) * d2r), &
                                   2.0_real64 * sin(0.5_real64 * rv(b) * d2r), rules(ri)))) / d2r
                    if ((sep <= bnd) .neqv. (sep <= chordwise)) split = split + 1_int64
                end do
            end do
            call check(error, split > 0_int64, &
                "the fixture must contain pairs the chord-wise and angle-wise readings judge " // &
                "differently, or this test cannot tell the two apart")
            if (allocated(error)) return
            do bk = 1, 2
                call sx%clear()
                call sx%build_sky(ra, dec, radius_deg=rv, backend=backends(bk))
                call sx%pairs_within_sky(rv, pi, pj, combine=rules(ri))
                ! The count first: a doubled emission from one backend keeps the set and changes
                ! only this number.
                call check(error, size(pi, kind=int64) == nwant, &
                    "a sky combine= rule must accept exactly the pairs its angular bound admits")
                if (allocated(error)) return
                got = .false.
                do k = 1_int64, size(pi, kind=int64)
                    got(pi(k), pj(k)) = .true.
                end do
                call check(error, all(got .eqv. want), &
                    "both sky backends must give exactly the angular rule's pairs")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_sky_pairs_combine_matches_angle_oracle

    !> The sum rule on the sky, inside its 45-degree ceiling, against the angular oracle.
    subroutine test_sky_pairs_sum_within_ceiling(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), rv(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :), got(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, b, k, nwant, beyond
        real(real64) :: sep

        n = 600_int64
        call make_sky(n, ra, dec)
        allocate (rv(n))
        do a = 1_int64, n
            ! Up to the 45-degree ceiling the rule imposes, so the doubled walk lands exactly on
            ! the 90-degree ceiling every sky query has.
            rv(a) = 15.0_real64 + 30.0_real64 * pf_random_at(fixture_seed, a, 42_int64)
        end do
        call sx%build_sky(ra, dec, radius_deg=rv)
        allocate (want(n, n), got(n, n))
        want = .false.
        nwant = 0_int64
        beyond = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                sep = sky_sep(ra(a), dec(a), ra(b), dec(b))
                if (sep <= rv(a) + rv(b)) then
                    want(a, b) = .true.
                    nwant = nwant + 1_int64
                    if (sep > max(rv(a), rv(b))) beyond = beyond + 1_int64
                end if
            end do
        end do
        call check(error, beyond > 0_int64, &
            "the fixture must contain pairs beyond either angular radius on its own")
        if (allocated(error)) return
        call sx%pairs_within_sky(rv, pi, pj, combine=PF_LINK_SUM)
        call check(error, size(pi, kind=int64) == nwant, &
            "the sky sum rule must accept exactly the pairs within deg_i + deg_j")
        if (allocated(error)) return
        got = .false.
        do k = 1_int64, size(pi, kind=int64)
            got(pi(k), pj(k)) = .true.
        end do
        call check(error, all(got .eqv. want), &
            "the sky sum rule's edge list must be exactly the pairs within deg_i + deg_j")
    end subroutine test_sky_pairs_sum_within_ceiling

    ! ---- Segment, cylinder and cone ----

    !> All three axis shapes agree with a full scan, over axes in every orientation that matters.
    !>
    !> **The orientations are the point.** The walk picks the axis the segment travels furthest
    !> along and slabs across it, so which branch runs depends on the direction -- an axis parallel
    !> to x, to y and to z each takes a different one, and a diagonal is the case a bounding-box
    !> walk would degenerate on. All four are here, plus a steep cone whose narrow end must not
    !> walk the wide end's cells.
    subroutine test_axis_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw
        integer :: c
        real(real64) :: p1(3), p2(3), r1, r2
        logical :: clamp

        n = 4000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.12_real64)
        allocate (got(n))
        do c = 1, 15
            ! Five axes x three shapes: along x, along y, along z, a body diagonal, and a short
            ! off-centre one. The shape cycles capsule / cylinder / cone.
            select case (mod(c - 1, 5) + 1)
            case (1)
                p1 = [0.1_real64, 0.5_real64, 0.5_real64]
                p2 = [0.9_real64, 0.5_real64, 0.5_real64]
            case (2)
                p1 = [0.5_real64, 0.1_real64, 0.5_real64]
                p2 = [0.5_real64, 0.9_real64, 0.5_real64]
            case (3)
                p1 = [0.5_real64, 0.5_real64, 0.1_real64]
                p2 = [0.5_real64, 0.5_real64, 0.9_real64]
            case (4)
                p1 = [0.05_real64, 0.05_real64, 0.05_real64]
                p2 = [0.95_real64, 0.95_real64, 0.95_real64]
            case default
                p1 = [0.3_real64, 0.7_real64, 0.2_real64]
                p2 = [0.45_real64, 0.6_real64, 0.35_real64]
            end select
            r1 = 0.12_real64
            r2 = 0.12_real64
            clamp = .false.
            select case ((c - 1) / 5)
            case (0)
                clamp = .true.
                mg = sx%within_segment(p1, p2, r1, got)
            case (1)
                mg = sx%within_cylinder(p1, p2, r1, got)
            case default
                r1 = 0.02_real64
                r2 = 0.22_real64
                mg = sx%within_cone(p1, p2, r1, r2, got)
            end select
            call brute_axis(x, y, z, p1, p2, r1, r2, clamp, want, mw)
            call check(error, mw > 0_int64, "every axis fixture must find some points")
            if (allocated(error)) return
            call check(error, same_rows(got, mg, want, mw), &
                "an axis-shaped query must return exactly what a full scan returns")
            if (allocated(error)) return
        end do
    end subroutine test_axis_matches_brute_force

    !> The three shapes accept the points they should, at offsets worked out by hand.
    !>
    !> **The independent check on the accept test**, which `brute_axis` deliberately shares with
    !> the implementation. Everything here is placed relative to the unit axis along x, so the
    !> expected answer is arithmetic a reader can redo: a point beyond an end is INSIDE the capsule
    !> and OUTSIDE the cylinder, which is the entire difference between them, and the cone's radius
    !> at parameter `t` is `r1 + t*(r2 - r1)`.
    subroutine test_axis_shapes_by_hand(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64) :: x(7), y(7), z(7)
        integer(int64) :: got(7), m
        type(pf_spatial_index) :: sx
        real(real64), parameter :: p1(3) = [0.0_real64, 0.0_real64, 0.0_real64]
        real(real64), parameter :: p2(3) = [1.0_real64, 0.0_real64, 0.0_real64]

        ! The axis runs from (0,0,0) to (1,0,0); the capsule and cylinder use r = 0.1 and the cone
        ! runs from 0.05 to 0.2, so its radius at parameter t is 0.05 + 0.15*t. Every offset below
        ! clears the nearest threshold by at least 4%, so nothing here rests on an exact tie.
        !
        !  1: t = 0.5, 0.05 out  -- in all three (cone radius there is 0.125)
        !  2: t = 0.5, 0.12 out  -- outside r = 0.1, inside the cone's 0.125
        !  3: 0.05 BEFORE the near end -- capsule yes; cylinder and cone reject t < 0
        !  4: 0.05 BEYOND the far end  -- capsule yes; cylinder and cone reject t > 1
        !  5: 0.2 before the near end  -- outside everything
        !  6: t = 0.1, 0.12 out  -- cone radius there is 0.065, so outside the cone too
        !  7: t = 0.9, 0.12 out  -- cone radius there is 0.185, so inside the cone only
        x = [0.5_real64, 0.5_real64, -0.05_real64, 1.05_real64, -0.2_real64, 0.1_real64, 0.9_real64]
        y = [0.05_real64, 0.12_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.12_real64, 0.12_real64]
        z = 0.0_real64
        call sx%build(x, y, z, radius=0.1_real64)

        m = sx%within_segment(p1, p2, 0.1_real64, got)
        call check(error, same_rows(got, m, [1_int64, 3_int64, 4_int64], 3_int64), &
            "the capsule must take the mid-axis point and both points just beyond the ends")
        if (allocated(error)) return

        m = sx%within_cylinder(p1, p2, 0.1_real64, got)
        call check(error, same_rows(got, m, [1_int64], 1_int64), &
            "the cylinder must reject both points beyond its flat ends")
        if (allocated(error)) return

        m = sx%within_cone(p1, p2, 0.05_real64, 0.2_real64, got)
        call check(error, same_rows(got, m, [1_int64, 2_int64, 7_int64], 3_int64), &
            "the cone must accept by its own radius at each point's parameter along the axis")
    end subroutine test_axis_shapes_by_hand

    !> `%within_cone` with `r1 == r2` is `%within_cylinder`, row for row.
    !>
    !> The cone is the general routine and the cylinder is it restricted, so this asserts the
    !> restriction rather than re-testing the walk: a defect in the radius interpolation that
    !> happened to preserve the count would still show up here.
    subroutine test_cone_equal_radii_is_cylinder(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: a(:), b(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, ma, mb
        real(real64), parameter :: p1(3) = [0.15_real64, 0.2_real64, 0.8_real64]
        real(real64), parameter :: p2(3) = [0.85_real64, 0.7_real64, 0.1_real64]

        n = 3000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.13_real64)
        allocate (a(n), b(n))
        ma = sx%within_cylinder(p1, p2, 0.13_real64, a)
        mb = sx%within_cone(p1, p2, 0.13_real64, 0.13_real64, b)
        call check(error, ma > 0_int64, "the fixture must find some points")
        if (allocated(error)) return
        call check(error, mb == ma, "a cone with equal radii must find as many points as the cylinder")
        if (allocated(error)) return
        call check(error, all(a(1:ma) == b(1:mb)), &
            "a cone with equal radii must return the cylinder's rows in the same order")
    end subroutine test_cone_equal_radii_is_cylinder

    !> A zero-length axis reduces to a ball, in all three shapes, exactly as `%within` reports it.
    !>
    !> A documented reduction rather than an accident, so it is asserted rather than assumed -- and
    !> against `%within`, which reaches the answer by a completely different walk.
    subroutine test_axis_degenerate_is_a_ball(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: ball(:), seg(:), cyl(:), cone(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mb, ms, mc, mk
        real(real64), parameter :: p(3) = [0.4_real64, 0.55_real64, 0.6_real64]

        n = 3000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64)
        allocate (ball(n), seg(n), cyl(n), cone(n))
        mb = sx%within(p, 0.15_real64, ball)
        ms = sx%within_segment(p, p, 0.15_real64, seg)
        mc = sx%within_cylinder(p, p, 0.15_real64, cyl)
        ! The cone takes the LARGER of its two radii when the axis has no length.
        mk = sx%within_cone(p, p, 0.05_real64, 0.15_real64, cone)
        call check(error, mb > 0_int64, "the fixture must find some points")
        if (allocated(error)) return
        call check(error, same_rows(seg, ms, ball, mb), &
            "a zero-length segment must return exactly the ball's rows")
        if (allocated(error)) return
        call check(error, same_rows(cyl, mc, ball, mb), &
            "a zero-length cylinder must return exactly the ball's rows")
        if (allocated(error)) return
        call check(error, same_rows(cone, mk, ball, mb), &
            "a zero-length cone must return the ball of its larger radius")
    end subroutine test_axis_degenerate_is_a_ball

    !> An axis that misses the cloud finds nothing; one far longer than the box finds the same
    !> points as one that merely spans it.
    !>
    !> The overlong case is what a bounding-box walk gets wrong: the region's AABB is then the
    !> whole grid, so the walk degenerates to a full scan exactly where the shape is most
    !> selective. Answers cannot see that, which is why the cheap cell-count check is here too.
    subroutine test_axis_outside_and_overlong(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:), long(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, mw, ml
        real(real64) :: far1(3), far2(3) !! the overlong axis's endpoints; named, see `NO_WRAP`.

        n = 3000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.1_real64)
        allocate (got(n), long(n))
        ! Wholly outside, and parallel to the cloud rather than pointing away from it.
        m = sx%within_segment([-5.0_real64, -5.0_real64, -5.0_real64], &
                              [-5.0_real64, -5.0_real64, 5.0_real64], 0.1_real64, got)
        call check(error, m == 0_int64, "an axis that misses the cloud entirely must find nothing")
        if (allocated(error)) return
        ! Spanning the cloud, then the same line extended far past it in both directions.
        m = sx%within_cylinder([0.0_real64, 0.5_real64, 0.5_real64], &
                               [1.0_real64, 0.5_real64, 0.5_real64], 0.1_real64, got)
        ! Named locals, not inline constructors: `brute_axis` takes its endpoints
        ! explicit-shape, so a constructor there costs an argument temporary and an ifx warning
        ! (406) per call -- see `NO_WRAP`. They also tie the index call and the oracle to one
        ! pair of endpoints, which is what the comparison below assumes.
        far1 = [-50.0_real64, 0.5_real64, 0.5_real64]
        far2 = [51.0_real64, 0.5_real64, 0.5_real64]
        ml = sx%within_cylinder(far1, far2, 0.1_real64, long)
        call brute_axis(x, y, z, far1, far2, 0.1_real64, 0.1_real64, .false., want, mw)
        call check(error, m > 0_int64, "the spanning cylinder must find some points")
        if (allocated(error)) return
        call check(error, same_rows(long, ml, want, mw), &
            "an axis far longer than the box must still agree with a full scan")
        if (allocated(error)) return
        call check(error, same_rows(long, ml, got, m), &
            "extending an axis past the cloud must not change which points it holds")
    end subroutine test_axis_outside_and_overlong

    !> A 2D index answers an axis query, with the third coordinate never entering it.
    subroutine test_axis_2d(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, mw
        real(real64) :: a1(3), a2(3) !! the oracle's axis endpoints; named, see `NO_WRAP`.

        n = 2000_int64
        call make_cloud(n, 1.0_real64, .true., x, y, z)
        call sx%build(x, y, radius=0.12_real64)
        allocate (got(n))
        m = sx%within_segment([0.1_real64, 0.2_real64], [0.9_real64, 0.8_real64], 0.12_real64, got)
        ! `z` is all zeros from `make_cloud`, so the 3D oracle answers the 2D question unchanged.
        ! Named locals rather than inline constructors, see `NO_WRAP`.
        a1 = [0.1_real64, 0.2_real64, 0.0_real64]
        a2 = [0.9_real64, 0.8_real64, 0.0_real64]
        call brute_axis(x, y, z, a1, a2, 0.12_real64, 0.12_real64, .true., want, mw)
        call check(error, mw > 0_int64, "the 2D axis fixture must find some points")
        if (allocated(error)) return
        call check(error, same_rows(got, m, want, mw), &
            "a 2D axis query must return exactly what a full scan returns")
    end subroutine test_axis_2d

    !> `dist` reports the distance to the axis, and a short buffer still reports the true count.
    subroutine test_axis_dist_and_short_buffer(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), dd(:)
        integer(int64), allocatable :: got(:)
        integer(int32), allocatable :: got32(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, mshort, k
        real(real64) :: want
        real(real64), parameter :: p1(3) = [0.1_real64, 0.5_real64, 0.5_real64]
        real(real64), parameter :: p2(3) = [0.9_real64, 0.5_real64, 0.5_real64]

        n = 2000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64)
        allocate (got(n), dd(n), got32(n))
        m = sx%within_cylinder(p1, p2, 0.15_real64, got, dist=dd)
        call check(error, m > 0_int64, "the fixture must find some points")
        if (allocated(error)) return
        do k = 1_int64, m
            ! The axis is parallel to x here, so the distance to it is the distance in (y, z) --
            ! computed without reference to the implementation's projection.
            want = sqrt((y(got(k)) - 0.5_real64)**2 + (z(got(k)) - 0.5_real64)**2)
            call check(error, abs(dd(k) - want) < 1.0e-12_real64, &
                "dist must report the perpendicular distance to the axis")
            if (allocated(error)) return
        end do
        ! An int32 buffer finds the same rows, and a short one still reports the true count.
        mshort = sx%within_cylinder(p1, p2, 0.15_real64, got32(1:2))
        call check(error, mshort == m, &
            "a short buffer must still report the true count so a caller can size and retry")
        if (allocated(error)) return
        mshort = sx%within_cylinder(p1, p2, 0.15_real64, got32)
        call check(error, mshort == m .and. all(int(got32(1:m), kind=int64) == got(1:m)), &
            "an int32 buffer must return the same rows as an int64 one")
    end subroutine test_axis_dist_and_short_buffer

    ! ---- Where on the axis a returned point sits ----

    !> The three `axis_point`/`axis_t` invariants, on all three shapes.
    !>
    !> **The first is the one that can be silently wrong**: `axis_point` must be the point `dist`
    !> was measured FROM, which for a capsule is the closest point on the SEGMENT and not the
    !> projection onto the infinite line. The second ties the two outputs to each other, so a clamp
    !> applied to one and not the other has nowhere to hide.
    subroutine test_axis_outputs_satisfy_their_invariants(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        real(real64), allocatable :: dd(:), ap(:,:), at(:)
        integer(int64), allocatable :: got(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, k, row
        integer :: shape_id
        real(real64) :: p1(3), p2(3), foot(3), sep, want

        n = 900_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        allocate (got(n), dd(n), ap(3, n), at(n))
        p1 = [0.15_real64, 0.20_real64, 0.30_real64]
        p2 = [0.85_real64, 0.70_real64, 0.55_real64]
        do shape_id = 1, 3
            select case (shape_id)
            case (1)
                m = sx%within_segment(p1, p2, 0.12_real64, got, dist=dd, axis_point=ap, axis_t=at)
            case (2)
                m = sx%within_cylinder(p1, p2, 0.12_real64, got, dist=dd, axis_point=ap, axis_t=at)
            case default
                m = sx%within_cone(p1, p2, 0.05_real64, 0.20_real64, got, dist=dd, axis_point=ap, axis_t=at)
            end select
            call check(error, m > 20_int64, "each axis fixture must return a useful number of points")
            if (allocated(error)) return
            do k = 1_int64, m
                row = got(k)
                sep = sqrt((x(row) - ap(1, k)) ** 2 + (y(row) - ap(2, k)) ** 2 + (z(row) - ap(3, k)) ** 2)
                call check(error, abs(sep - dd(k)) <= 1.0e-12_real64, &
                    "axis_point must be the point dist was measured from")
                if (allocated(error)) return
                call check(error, at(k) >= 0.0_real64 .and. at(k) <= 1.0_real64, &
                    "axis_t must be a normalised position in [0, 1], never signed and never past the far end")
                if (allocated(error)) return
                foot = p1 + at(k) * (p2 - p1)
                want = maxval(abs(foot - ap(1:3, k)))
                call check(error, want <= 1.0e-12_real64, &
                    "axis_point must equal p1 + axis_t*(p2 - p1), so the two outputs agree")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_axis_outputs_satisfy_their_invariants

    !> A capsule whose points lie beyond BOTH ends: the reported foot is the end itself.
    !>
    !> **This is the only case that distinguishes the clamped foot from the infinite-line
    !> projection**, and an unclamped implementation passes every other test here: it would report
    !> a point past the end, at a distance that is not `dist`, with `axis_t` outside `[0, 1]`.
    subroutine test_axis_point_clamps_at_the_ends(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64) :: x(4), y(4), z(4)
        real(real64) :: dd(4), ap(3, 4), at(4)
        integer(int64) :: got(4), m, k
        type(pf_spatial_index) :: sx
        real(real64) :: p1(3), p2(3)

        p1 = [0.0_real64, 0.0_real64, 0.0_real64]
        p2 = [1.0_real64, 0.0_real64, 0.0_real64]
        ! Two points beyond p1, two beyond p2, each offset sideways so the capsule keeps them and
        ! `dist` is a genuine distance rather than zero.
        x = [-0.30_real64, -0.10_real64, 1.10_real64, 1.30_real64]
        y = [0.10_real64, 0.20_real64, 0.20_real64, 0.10_real64]
        z = 0.0_real64
        call sx%build(x, y, z, radius=0.5_real64)
        m = sx%within_segment(p1, p2, 0.5_real64, got, dist=dd, axis_point=ap, axis_t=at, sorted=.true.)
        call check(error, m == 4_int64, "every point of this fixture is inside the capsule")
        if (allocated(error)) return
        do k = 1_int64, m
            if (x(got(k)) < 0.0_real64) then
                call check(error, at(k) == 0.0_real64, "a point before p1 must report axis_t exactly 0")
                if (allocated(error)) return
                call check(error, all(ap(1:3, k) == p1), "a point before p1 must report p1 itself as its foot")
            else
                call check(error, at(k) == 1.0_real64, "a point beyond p2 must report axis_t exactly 1")
                if (allocated(error)) return
                call check(error, all(ap(1:3, k) == p2), "a point beyond p2 must report p2 itself as its foot")
            end if
            if (allocated(error)) return
            call check(error, abs(dd(k) - sqrt((x(got(k)) - ap(1, k)) ** 2 + (y(got(k)) - ap(2, k)) ** 2)) &
                <= 1.0e-14_real64, "dist must be measured from the clamped foot, not from the axis line")
            if (allocated(error)) return
        end do
        ! The cylinder rejects every one of them, which is what makes the capsule's clamp visible
        ! at all: the two shapes differ exactly where the clamp bites.
        m = sx%within_cylinder(p1, p2, 0.5_real64, got)
        call check(error, m == 0_int64, "the cylinder must reject every point that lies beyond an end")
    end subroutine test_axis_point_clamps_at_the_ends

    !> A 2D index reports `(1:2, :)`, and a zero-length axis reports `p1` and 0 for every point.
    subroutine test_axis_point_2d_and_degenerate(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        real(real64) :: ap2(2, 64), at(64), dd(64)
        real(real64), allocatable :: ap3(:,:)
        integer(int64) :: got(64), m, k
        type(pf_spatial_index) :: sx, s3
        real(real64) :: p1(2), q1(3)

        call make_cloud(400_int64, 1.0_real64, .true., x, y, z)
        call sx%build(x, y, radius=0.2_real64)
        p1 = [0.2_real64, 0.3_real64]
        m = sx%within_segment(p1, [0.8_real64, 0.6_real64], 0.05_real64, got, dist=dd, &
            axis_point=ap2, axis_t=at)
        call check(error, m > 5_int64 .and. m <= 64_int64, &
            "the 2D capsule fixture must return points, and must fit the buffers")
        if (allocated(error)) return
        do k = 1_int64, m
            call check(error, abs(sqrt((x(got(k)) - ap2(1, k)) ** 2 + (y(got(k)) - ap2(2, k)) ** 2) - dd(k)) &
                <= 1.0e-12_real64, "a 2D axis_point must be the two-coordinate foot dist was measured from")
            if (allocated(error)) return
        end do
        ! A zero-length axis is a ball, so every foot is p1 and every parameter is 0 -- the
        ! reduction the three shapes share, carried through to the new outputs.
        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call s3%build(x, y, z, radius=0.2_real64)
        allocate (ap3(3, 64))
        q1 = [0.5_real64, 0.5_real64, 0.5_real64]
        m = s3%within_cone(q1, q1, 0.15_real64, 0.15_real64, got, dist=dd, axis_point=ap3, axis_t=at)
        call check(error, m > 0_int64, "a zero-length cone must still find the ball's points")
        if (allocated(error)) return
        do k = 1_int64, min(m, 64_int64)
            call check(error, all(ap3(1:3, k) == q1), "a zero-length axis must report p1 as every foot")
            if (allocated(error)) return
            call check(error, at(k) == 0.0_real64, "a zero-length axis must report axis_t 0 for every point")
            if (allocated(error)) return
        end do
    end subroutine test_axis_point_2d_and_degenerate

    !> Short axis buffers: nothing is written past any of them, and what comes back corresponds.
    !>
    !> **What this deliberately does NOT assert is that `dist` stopped at the common cap rather
    !> than at its own length.** Every output here is `intent(out)`, so an element the library did
    !> not write is UNDEFINED on return and reading it is not conforming. An earlier version of
    !> this test read `dist(6)` after five entries had been filled; nagfor's `-nan` had put a
    !> signalling NaN there, the comparison raised `FE_INVALID` and the test failed -- correctly,
    !> against a library that was doing exactly the right thing. The common cap is an
    !> implementation guarantee with no conforming observer.
    !>
    !> So it is checked the two ways that ARE observable: **canaries outside the sections passed
    !> in** show nothing was written past any buffer's own extent, and the entries that are
    !> returned agree with each other. Both arms below make a different buffer the shortest one, so
    !> each of the four takes a turn at being the binding constraint.
    subroutine test_axis_outputs_truncate_together(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        real(real64) :: dd(64), ap(3, 64), at(64), sentinel
        integer(int64) :: got(64), m, k, nfill
        type(pf_spatial_index) :: sx
        real(real64) :: p1(3), p2(3), foot(3)
        integer :: arm

        call make_cloud(900_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        p1 = [0.15_real64, 0.20_real64, 0.30_real64]
        p2 = [0.85_real64, 0.70_real64, 0.55_real64]
        sentinel = -99.0_real64
        do arm = 1, 2
            got = -1_int64
            dd = sentinel
            ap = sentinel
            at = sentinel
            if (arm == 1) then
                ! axis_t is the shortest at 5; every other buffer has room for 8.
                nfill = 5_int64
                m = sx%within_segment(p1, p2, 0.12_real64, got(1:8), dist=dd(1:8), &
                    axis_point=ap(:, 1:8), axis_t=at(1:5))
            else
                ! dist is the shortest at 3, so a different buffer decides the cap.
                nfill = 3_int64
                m = sx%within_segment(p1, p2, 0.12_real64, got(1:8), dist=dd(1:3), &
                    axis_point=ap(:, 1:8), axis_t=at(1:8))
            end if
            call check(error, m > 8_int64, "this fixture must overflow every buffer passed in")
            if (allocated(error)) return
            ! The canaries: everything outside the sections handed to the library still holds the
            ! sentinel, so no output ran past its own extent.
            call check(error, all(got(9:) == -1_int64), "out must not be written past its own extent")
            if (allocated(error)) return
            call check(error, all(dd(9:) == sentinel), "dist must not be written past its own extent")
            if (allocated(error)) return
            call check(error, all(ap(:, 9:) == sentinel), &
                "axis_point must not be written past its own extent")
            if (allocated(error)) return
            call check(error, all(at(9:) == sentinel), "axis_t must not be written past its own extent")
            if (allocated(error)) return
            ! And every entry up to the common cap is filled, and the four agree with each other.
            do k = 1_int64, nfill
                call check(error, got(k) >= 1_int64, "every slot up to the common cap must hold a row")
                if (allocated(error)) return
                call check(error, dd(k) /= sentinel .and. at(k) /= sentinel, &
                    "every slot up to the common cap must hold a distance and an axis position")
                if (allocated(error)) return
                foot = p1 + at(k) * (p2 - p1)
                call check(error, maxval(abs(foot - ap(1:3, k))) <= 1.0e-12_real64, &
                    "the entries returned must still satisfy axis_point == p1 + axis_t*(p2 - p1)")
                if (allocated(error)) return
                call check(error, abs(sqrt((x(got(k)) - ap(1, k)) ** 2 + (y(got(k)) - ap(2, k)) ** 2 &
                    + (z(got(k)) - ap(3, k)) ** 2) - dd(k)) <= 1.0e-12_real64, &
                    "and must still have dist measured from the reported foot")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_axis_outputs_truncate_together

    ! ---- The sky metric ----

    !> A sky index agrees with a haversine scan, at both poles and across 0h.
    !>
    !> **Those two regions are the whole reason the conversion exists.** A query written directly
    !> in `(ra, dec)` -- comparing right ascensions as if they were a Cartesian coordinate -- passes
    !> perfectly well in the middle of the sky and fails at exactly these two places, so a fixture
    !> that avoids them proves nothing. The assertions below therefore also check that the answers
    !> SPAN the discontinuity: a 0h query must return points on both sides of it, and a polar query
    !> must return points whose right ascensions are far apart.
    subroutine test_sky_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, mw, k, lowside, highside
        integer :: c
        real(real64) :: ra0, dec0, rq, lo_ra, hi_ra

        n = 6000_int64
        call make_sky(n, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.5_real64)
        allocate (got(n))
        do c = 1, 6
            rq = 1.5_real64
            select case (c)
            case (1)
                ra0 = 0.5_real64          ! just east of 0h
                dec0 = 0.0_real64
            case (2)
                ra0 = 359.5_real64        ! just west of 0h
                dec0 = 0.0_real64
            case (3)
                ra0 = 0.0_real64          ! the north pole itself
                dec0 = 90.0_real64
            case (4)
                ra0 = 217.0_real64        ! the south pole, approached from a different meridian
                dec0 = -90.0_real64
            case (5)
                ra0 = 123.4_real64        ! an ordinary place, as a control
                dec0 = 45.6_real64
                ! The scattered background is thin -- a quarter of the catalogue over the whole
                ! sky -- so a 1.5-degree circle there is usually empty. A wider one keeps the
                ! control meaningful without leaving the band the index was tuned for.
                rq = 10.0_real64
            case default
                ra0 = 88.0_real64
                dec0 = 88.5_real64        ! near, but not at, the pole
            end select
            m = sx%within_sky(ra0, dec0, rq, got)
            call brute_sky(ra, dec, ra0, dec0, rq, want, mw)
            call check(error, mw > 0_int64, "every sky fixture must find some points")
            if (allocated(error)) return
            call check(error, same_rows(got, m, want, mw), &
                "a sky query must return exactly what a haversine scan returns")
            if (allocated(error)) return
        end do

        ! The 0h query must straddle the seam, or it is not testing the seam.
        m = sx%within_sky(0.0_real64, 0.0_real64, 1.5_real64, got)
        lowside = 0_int64
        highside = 0_int64
        do k = 1_int64, m
            if (ra(got(k)) < 180.0_real64) lowside = lowside + 1_int64
            if (ra(got(k)) >= 180.0_real64) highside = highside + 1_int64
        end do
        call check(error, lowside > 0_int64 .and. highside > 0_int64, &
            "a query at 0h must find points on both sides of the wrap, or the fixture tests nothing")
        if (allocated(error)) return

        ! The polar query must gather points from right ascensions far apart, which is the other
        ! case a naive (ra, dec) comparison gets wrong.
        m = sx%within_sky(0.0_real64, 90.0_real64, 2.0_real64, got)
        call check(error, m > 0_int64, "the polar query must find points")
        if (allocated(error)) return
        lo_ra = minval(ra(got(1:m)))
        hi_ra = maxval(ra(got(1:m)))
        call check(error, hi_ra - lo_ra > 180.0_real64, &
            "a query at the pole must reach every meridian, or the fixture tests nothing")
    end subroutine test_sky_matches_brute_force

    !> `dist_deg` is an angular separation in degrees, never a chord.
    subroutine test_sky_distances_are_degrees(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), dd(:)
        integer(int64), allocatable :: got(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, m, k
        real(real64) :: want
        logical :: had_invalid, raised
        real(real64), parameter :: ra0 = 12.0_real64, dec0 = -30.0_real64, rsky = 2.0_real64

        n = 3000_int64
        call make_sky(n, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=rsky)
        allocate (got(n), dd(n))
        m = sx%within_sky(ra0, dec0, rsky, got, dist_deg=dd)
        call check(error, m > 0_int64, "the fixture must find some points")
        if (allocated(error)) return
        do k = 1_int64, m
            want = sky_sep(ra0, dec0, ra(got(k)), dec(got(k)))
            call check(error, abs(dd(k) - want) < 1.0e-9_real64, &
                "dist_deg must equal the haversine separation in degrees")
            if (allocated(error)) return
            ! A chord would be numerically smaller than the angle in degrees for any radius this
            ! size, so this also catches a conversion that was simply left out.
            call check(error, dd(k) <= rsky + 1.0e-9_real64, &
                "every reported separation must be within the requested angular radius")
            if (allocated(error)) return
        end do
        ! **A SHORT `out` beside a LONG `dist_deg`**, which is the shape that makes the chords-to-
        ! degrees conversion walk past what the scan actually wrote. Queried at 0h, where this
        ! fixture piles a band of points, so three slots really do overflow -- the sparse
        ! background around (ra0, dec0) holds barely one point at this radius.
        !
        ! **The entries past the cap cannot be read**: they are elements of an `intent(out)` array
        ! the library never assigned, so they are undefined and reading them is not conforming.
        ! What IS observable is that the call performed no invalid floating-point operation --
        ! converting an unwritten entry means `asin` of whatever was there, which under nagfor's
        ! `-nan` is a signalling NaN and raises the flag. The flag is saved and restored around the
        ! call so that a raise from anywhere else in the run is neither hidden nor blamed on this.
        if (ieee_support_flag(ieee_invalid, 0.0_real64)) then
            call ieee_get_flag(ieee_invalid, had_invalid)
            call ieee_set_flag(ieee_invalid, .false.)
        else
            had_invalid = .false. ! GCOVR_EXCL_LINE
        end if
        m = sx%within_sky(0.0_real64, 0.0_real64, rsky, got(1:3), dist_deg=dd)
        raised = .false.
        if (ieee_support_flag(ieee_invalid, 0.0_real64)) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, had_invalid .or. raised)
        end if
        call check(error, .not. raised, &
            "converting chords to degrees must stop where the SHORTEST buffer stopped; reading " // &
            "further means asin over an entry the walk never wrote")
        if (allocated(error)) return
        call check(error, m > 3_int64, "this fixture must overflow the three-slot row buffer")
        if (allocated(error)) return
        do k = 1_int64, 3_int64
            want = sky_sep(0.0_real64, 0.0_real64, ra(got(k)), dec(got(k)))
            call check(error, abs(dd(k) - want) < 1.0e-9_real64, &
                "the entries a short buffer does hold must still be converted to degrees")
            if (allocated(error)) return
        end do
    end subroutine test_sky_distances_are_degrees

    !> A sky index reports its metric, and its effective radius back in degrees.
    subroutine test_sky_metadata(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: sx, eu
        real(real64), allocatable :: x(:), y(:), z(:)

        call make_sky(1000_int64, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=1.25_real64)
        call check(error, sx%metric() == PF_METRIC_SKY, "%build_sky must set the sky metric")
        if (allocated(error)) return
        call check(error, sx%ndim() == 3, "a sky index is a 3D index over unit vectors")
        if (allocated(error)) return
        call check(error, .not. sx%is_periodic(), "a sky index has no periodic box")
        if (allocated(error)) return
        ! One radius, so the moment ratio is that radius -- and it must come back in the units it
        ! was given, not as the chord the index actually tunes on.
        call check(error, abs(sx%effective_radius() - 1.25_real64) < 1.0e-9_real64, &
            "%effective_radius must report degrees on a sky index")
        if (allocated(error)) return
        call check(error, sx%cell_size() > 0.0_real64 .and. sx%cell_size() < 2.0_real64, &
            "%cell_size stays in unit-vector space, so it cannot exceed the sphere's diameter")
        if (allocated(error)) return
        call make_cloud(500_int64, 1.0_real64, .false., x, y, z)
        call eu%build(x, y, z, radius=0.2_real64)
        call check(error, eu%metric() == PF_METRIC_EUCLIDEAN, "%build must leave the Euclidean metric")
    end subroutine test_sky_metadata

    !> `%all_within_sky` is exactly a loop of `%within_sky`, catalogue-wide.
    !>
    !> The equality test the `spatial_sky_bulk_refused` scenario asked for when the bulk sky forms
    !> landed. It runs over the same pole-and-0h fixture as the single-query test, so the bulk path
    !> is checked at the two places the conversion exists for rather than only in open sky.
    subroutine test_sky_bulk_matches_singles(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        integer(int64), allocatable :: offs(:), nb(:), one(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, m, lo, hi, total
        real(real64), parameter :: rq = 1.5_real64

        n = 3000_int64
        call make_sky(n, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=rq)
        call sx%all_within_sky(rq, offs, nb)
        call check(error, size(offs, kind=int64) == n + 1_int64, &
            "the CSR offsets must have one entry per point plus a sentinel")
        if (allocated(error)) return
        total = offs(n + 1_int64) - 1_int64
        call check(error, total >= n, "every point is its own neighbour, so the CSR cannot be shorter than n")
        if (allocated(error)) return
        allocate (one(n))
        do i = 1_int64, n
            m = sx%within_sky(ra(i), dec(i), rq, one)
            lo = offs(i)
            hi = offs(i + 1_int64) - 1_int64
            call check(error, same_rows(nb(lo:hi), hi - lo + 1_int64, one, m), &
                "each CSR row must hold exactly what a single sky query at that point returns")
            if (allocated(error)) return
        end do
    end subroutine test_sky_bulk_matches_singles

    !> The sky pair list and counts agree with the CSR, and pairs stay symmetric in degrees.
    !>
    !> **The per-point half is the one worth having.** The chord is strictly increasing in the
    !> angle, so ranking by descending chord is ranking by descending angle -- but that is an
    !> argument, and this asserts it: the edge list is compared against an O(n^2) haversine scan
    !> under `separation <= max(deg_i, deg_j)`, with a count of what the lower-row-searches rule
    !> would have produced beside it so the fixture is known to separate the two.
    subroutine test_sky_bulk_pairs_and_counts(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), rv(:)
        integer(int64), allocatable :: offs(:), nb(:), pi(:), pj(:), counts(:)
        logical, allocatable :: want(:, :), got(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, i, a, b, k, csr_total, want_max, want_low
        real(real64) :: sep
        real(real64), parameter :: rq = 1.5_real64

        n = 800_int64
        call make_sky(n, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=rq)

        ! Scalar radius: counts, CSR and the pair list must be three views of one answer.
        call sx%all_within_sky(rq, offs, nb)
        call sx%count_all_within_sky(rq, counts)
        call sx%pairs_within_sky(rq, pi, pj)
        do i = 1_int64, n
            call check(error, counts(i) == offs(i + 1_int64) - offs(i), &
                "count_all_within_sky must agree with the CSR row length for every point")
            if (allocated(error)) return
        end do
        csr_total = offs(n + 1_int64) - 1_int64
        call check(error, size(pi, kind=int64) == (csr_total - n) / 2_int64, &
            "the sky edge list must hold each neighbouring pair exactly once")
        if (allocated(error)) return
        call check(error, all(pi < pj), "every reported sky pair must have i < j")
        if (allocated(error)) return

        ! Per-point radii, in degrees, against an independent haversine oracle.
        allocate (rv(n))
        do i = 1_int64, n
            rv(i) = 0.3_real64 + 2.2_real64 * pf_random_at(fixture_seed, i, 31_int64)
        end do
        allocate (want(n, n), got(n, n))
        want = .false.
        got = .false.
        want_max = 0_int64
        want_low = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                sep = sky_sep(ra(a), dec(a), ra(b), dec(b))
                if (sep <= max(rv(a), rv(b))) then
                    want(a, b) = .true.
                    want_max = want_max + 1_int64
                end if
                if (sep <= rv(a)) want_low = want_low + 1_int64
            end do
        end do
        call check(error, want_max > want_low, &
            "the fixture must separate the symmetric rule from the lower-row-searches rule")
        if (allocated(error)) return
        call sx%pairs_within_sky(rv, pi, pj)
        call check(error, size(pi, kind=int64) == want_max, &
            "the sky edge list must hold every pair within max(deg_i, deg_j), and no others")
        if (allocated(error)) return
        do k = 1_int64, size(pi, kind=int64)
            got(pi(k), pj(k)) = .true.
        end do
        call check(error, all(got .eqv. want), &
            "the sky edge list must be exactly the pairs within max(deg_i, deg_j)")
    end subroutine test_sky_bulk_pairs_and_counts

    !> A threaded bulk sweep answers exactly as the serial one.
    !>
    !> **Skipped without OpenMP rather than passed**: both arms would run the same serial code, so
    !> the equality would hold for the wrong reason and the test would report a green that means
    !> nothing. The negative control is `parquet_debug_spatial_threads_used`, which is what
    !> separates "the threaded arm agreed" from "no team was ever opened".
    subroutine test_bulk_threaded_matches_serial(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: o1(:), n1(:), o2(:), n2(:)
        type(pf_spatial_index) :: sx
        integer :: used1, used4
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without a team both arms run the same serial sweep, so " // &
            "the equality below would hold for the wrong reason")
        return
#endif
        call make_cloud(2000_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.1_real64)
        call sx%all_within(0.1_real64, o1, n1, threads=1)
        used1 = parquet_debug_spatial_threads_used()
        call sx%all_within(0.1_real64, o2, n2, threads=4)
        used4 = parquet_debug_spatial_threads_used()
        if (used4 < 2) then
            call skip_test(error, "needs at least two processors: the affinity clamp resolved the " // &
                "four-thread arm to a team of one, so both arms ran serially")
            return
        end if
        call check(error, used1 == 1, "an explicit threads=1 must resolve to a team of one")
        if (allocated(error)) return
        call check(error, all(o1 == o2), "a threaded sweep must produce the same CSR offsets as a serial one")
        if (allocated(error)) return
        call check(error, size(n1, kind=int64) == size(n2, kind=int64), &
            "a threaded sweep must produce the same number of neighbours as a serial one")
        if (allocated(error)) return
        call check(error, all(n1 == n2), "a threaded sweep must produce the same neighbours as a serial one")
    end subroutine test_bulk_threaded_matches_serial

    ! ---- The annulus, and ordered results ----

    !> An annulus is the outer ball minus the inner one, checked against the outer ball's own rows.
    !>
    !> **The oracle is assembled from an already-tested query rather than restating the new one**:
    !> take every row within `r`, keep those whose distance reaches `r_inner`, and demand exactly
    !> that set back. A fixture with random coordinates has no point sitting exactly on the inner
    !> surface, which is where the two readings would differ -- both bounds are inclusive here, so
    !> such a point would belong to the annulus and to the inner ball alike.
    subroutine test_annulus_is_the_difference_of_two_balls(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), balld(:), annd(:)
        integer(int64), allocatable :: ballrows(:), annrows(:), plain(:)
        logical, allocatable :: mask(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mball, mann, mplain, k, q
        real(real64) :: p(3), r, rin

        n = 900_int64
        r = 0.30_real64
        rin = 0.18_real64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=r)
        allocate (ballrows(n), annrows(n), plain(n), balld(n), annd(n), mask(n))
        do q = 1_int64, 12_int64
            p(1) = pf_random_at(fixture_seed + 11_int64, q, 1_int64)
            p(2) = pf_random_at(fixture_seed + 11_int64, q, 2_int64)
            p(3) = pf_random_at(fixture_seed + 11_int64, q, 3_int64)
            mball = sx%within(p, r, ballrows, dist=balld)
            mann = sx%within(p, r, annrows, dist=annd, r_inner=rin)
            mask = .false.
            do k = 1_int64, mball
                if (balld(k) >= rin) mask(ballrows(k)) = .true.
            end do
            call check(error, count(mask) == int(mann), &
                "the annulus must hold exactly the outer ball's rows that reach the inner radius")
            if (allocated(error)) return
            do k = 1_int64, mann
                call check(error, mask(annrows(k)), "every annulus row must be one of those rows")
                if (allocated(error)) return
                call check(error, annd(k) >= rin .and. annd(k) <= r, &
                    "every annulus distance must lie between the two radii")
                if (allocated(error)) return
            end do
            call check(error, sx%count_within(p, r, r_inner=rin) == mann, &
                "count_within with an inner radius must agree with within")
            if (allocated(error)) return
            ! The negative control: a zero inner radius must leave the plain ball untouched, or
            ! every assertion above would pass just as well against a query that ignored r_inner.
            mplain = sx%within(p, r, plain, r_inner=0.0_real64)
            call check(error, same_rows(plain, mplain, ballrows, mball), &
                "r_inner = 0 must reproduce the plain ball row for row")
            if (allocated(error)) return
            call check(error, mann < mball, "this fixture must have the inner radius actually remove rows")
            if (allocated(error)) return
        end do
    end subroutine test_annulus_is_the_difference_of_two_balls

    !> The annulus in the bulk forms, in the pair sweep, and on the sky.
    subroutine test_annulus_in_bulk_and_on_the_sky(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), ra(:), dec(:), dsky(:)
        integer(int64), allocatable :: counts(:), offsets(:), neigh(:), pi(:), pj(:), qi(:), qj(:)
        integer(int64), allocatable :: rowsky(:)
        type(pf_spatial_index) :: sx, sk
        integer(int64) :: n, i, k, kept, m
        real(real64) :: p(3), r, rin, d2

        n = 700_int64
        r = 0.25_real64
        rin = 0.15_real64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=r)
        call sx%count_all_within(r, counts, r_inner=rin)
        call sx%all_within(r, offsets, neigh, r_inner=rin)
        do i = 1_int64, n
            p = [x(i), y(i), z(i)]
            call check(error, counts(i) == sx%count_within(p, r, r_inner=rin), &
                "the bulk annulus count must reproduce the single-query one for every row")
            if (allocated(error)) return
            call check(error, offsets(i + 1_int64) - offsets(i) == counts(i), &
                "the CSR row lengths must match the counts under an inner radius")
            if (allocated(error)) return
        end do
        ! The pair sweep drops exactly the pairs the inner radius excludes, and nothing else.
        call sx%pairs_within(r, pi, pj)
        call sx%pairs_within(r, qi, qj, r_inner=rin)
        kept = 0_int64
        do k = 1_int64, size(pi, kind=int64)
            d2 = (x(pi(k)) - x(pj(k))) ** 2 + (y(pi(k)) - y(pj(k))) ** 2 + (z(pi(k)) - z(pj(k))) ** 2
            if (d2 >= rin * rin) kept = kept + 1_int64
        end do
        call check(error, kept == size(qi, kind=int64), &
            "an inner radius must drop exactly the pairs closer than it")
        if (allocated(error)) return
        call check(error, size(qi, kind=int64) < size(pi, kind=int64), &
            "this fixture must have the inner radius actually drop pairs")
        if (allocated(error)) return
        ! On the sky the same rule applies in degrees, and the chord being monotone is what makes
        ! the conversion invisible to any of it.
        ! The north polar cap, which is where this fixture is dense enough for an annulus to
        ! remove a visible number of rows rather than a fraction of one.
        call make_sky(600_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=3.0_real64)
        allocate (rowsky(600), dsky(600))
        m = sk%within_sky(0.0_real64, 90.0_real64, 3.0_real64, rowsky, dist_deg=dsky, r_inner_deg=1.0_real64)
        call check(error, m > 0_int64, "the sky annulus fixture must return points")
        if (allocated(error)) return
        do k = 1_int64, m
            call check(error, dsky(k) >= 1.0_real64 .and. dsky(k) <= 3.0_real64, &
                "every sky annulus separation must lie between the two angular radii")
            if (allocated(error)) return
            call check(error, abs(sky_sep(0.0_real64, 90.0_real64, ra(rowsky(k)), dec(rowsky(k))) &
                - dsky(k)) <= 1.0e-9_real64, "a sky annulus separation must match a haversine one")
            if (allocated(error)) return
        end do
        k = sk%within_sky(0.0_real64, 90.0_real64, 3.0_real64, rowsky)
        call check(error, m < k, "this sky fixture must have the inner radius actually remove rows")
    end subroutine test_annulus_in_bulk_and_on_the_sky

    !> `sorted=` returns increasing distances, the same SET, and a machine-independent tie order.
    subroutine test_sorted_orders_by_distance(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), ra(:), dec(:)
        real(real64), allocatable :: dd(:)
        integer(int64), allocatable :: got(:), plain(:), offsets(:), neigh(:)
        type(pf_spatial_index) :: sx, sk
        integer(int64) :: n, m, mp, k, i, s0, e0
        real(real64) :: p(3), r, prev, d

        n = 900_int64
        r = 0.25_real64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=r)
        allocate (got(n), plain(n), dd(n))
        p = [0.4_real64, 0.5_real64, 0.6_real64]
        m = sx%within(p, r, got, dist=dd, sorted=.true.)
        mp = sx%within(p, r, plain)
        call check(error, m > 10_int64, "the ordering fixture must return a useful number of points")
        if (allocated(error)) return
        call check(error, same_rows(got, m, plain, mp), &
            "sorting must not change which rows come back, only their order")
        if (allocated(error)) return
        do k = 2_int64, m
            call check(error, dd(k) >= dd(k - 1_int64), "sorted distances must be non-decreasing")
            if (allocated(error)) return
        end do
        ! Ordering must work when the caller wants no distances at all, which is the case that
        ! needs a buffer of the library's own.
        got = -1_int64
        m = sx%within(p, r, got, sorted=.true.)
        prev = -1.0_real64
        do k = 1_int64, m
            d = sqrt((x(got(k)) - p(1)) ** 2 + (y(got(k)) - p(2)) ** 2 + (z(got(k)) - p(3)) ** 2)
            call check(error, d >= prev, "sorted= must order the rows even when dist= is absent")
            if (allocated(error)) return
            prev = d
        end do
        ! A CSR bulk sweep orders WITHIN each row.
        call sx%all_within(r, offsets, neigh, sorted=.true.)
        do i = 1_int64, n
            s0 = offsets(i)
            e0 = offsets(i + 1_int64) - 1_int64
            prev = -1.0_real64
            do k = s0, e0
                d = sqrt((x(neigh(k)) - x(i)) ** 2 + (y(neigh(k)) - y(i)) ** 2 + (z(neigh(k)) - z(i)) ** 2)
                call check(error, d >= prev, "each CSR row must come back ordered by increasing distance")
                if (allocated(error)) return
                prev = d
            end do
        end do
        ! And on the sky, where the walk orders chords and the caller reads degrees.
        call make_sky(600_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=5.0_real64)
        m = sk%within_sky(0.0_real64, 0.0_real64, 5.0_real64, got, dist_deg=dd, sorted=.true.)
        call check(error, m > 5_int64, "the sky ordering fixture must return points")
        if (allocated(error)) return
        do k = 2_int64, m
            call check(error, dd(k) >= dd(k - 1_int64), "sorted sky separations must be non-decreasing")
            if (allocated(error)) return
        end do
    end subroutine test_sorted_orders_by_distance

    !> Exact ties come back in ascending ROW index, with a control proving the walk did not.
    !>
    !> **Without an explicit tie-break the order among equal distances is cell order**, which
    !> depends on the tuned cell size and so on the machine -- exactly what a caller asking for a
    !> canonical order does not want. Six points at distance exactly 1 from the origin make that
    !> visible: their coordinates are integers, so the distances are equal to the last bit rather
    !> than nearly, and they are numbered against the grid's own z-major traversal so the walk
    !> produces them backwards.
    !>
    !> The filler points are not padding: with only six points the cell-count clamp puts them all
    !> in ONE cell, where the bucketing sort's stability hands them back in row order already and
    !> the control below would pass for the wrong reason.
    subroutine test_sorted_breaks_ties_by_row(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64) :: x(400), y(400), z(400), dd(400)
        integer(int64) :: got(400), unsorted(400), m, k, ntie
        type(pf_spatial_index) :: sx
        real(real64) :: q(3), rad
        logical :: walk_was_ascending

        do k = 1_int64, 394_int64
            rad = 0.85_real64 * pf_random_at(fixture_seed + 31_int64, k, 1_int64) ** (1.0_real64 / 3.0_real64)
            q(1) = 2.0_real64 * pf_random_at(fixture_seed + 31_int64, k, 2_int64) - 1.0_real64
            q(2) = 2.0_real64 * pf_random_at(fixture_seed + 31_int64, k, 3_int64) - 1.0_real64
            q(3) = 2.0_real64 * pf_random_at(fixture_seed + 31_int64, k, 4_int64) - 1.0_real64
            q = q / max(sqrt(sum(q * q)), 1.0e-12_real64)
            x(k) = rad * q(1)
            y(k) = rad * q(2)
            z(k) = rad * q(3)
        end do
        ! Numbered in REVERSE traversal order, so the walk emits 400 first and 395 last.
        x(395:400) = [0.0_real64, 0.0_real64, 1.0_real64, -1.0_real64, 0.0_real64, 0.0_real64]
        y(395:400) = [0.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, -1.0_real64, 0.0_real64]
        z(395:400) = [1.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, -1.0_real64]
        ! The cell is FORCED, so the control below cannot start passing for the wrong reason when
        ! the tuner's own choice moves: a cell coarse enough to hold several tied points at once
        ! hands them back in row order already, since the bucketing sort is stable.
        call parquet_debug_set_spatial_cell(0.5_real64)
        call sx%build(x, y, z, radius=1.0_real64)
        q = 0.0_real64
        m = sx%within(q, 1.0_real64, unsorted)
        walk_was_ascending = .true.
        do k = 2_int64, m
            if (unsorted(k) < unsorted(k - 1_int64)) walk_was_ascending = .false.
        end do
        m = sx%within(q, 1.0_real64, got, dist=dd, sorted=.true.)
        call parquet_debug_set_spatial_cell(-1.0_real64)
        call check(error, m == 400_int64, "every point of the tie fixture is within the unit ball")
        if (allocated(error)) return
        call check(error, .not. walk_was_ascending, &
            "the control: the unsorted walk must NOT already be in row order, or the tie-break is untested")
        if (allocated(error)) return
        do k = 2_int64, m
            call check(error, dd(k) >= dd(k - 1_int64), "sorted distances must be non-decreasing")
            if (allocated(error)) return
        end do
        ! The six tied rows sit at the end, all at distance exactly 1, and must come back in
        ! ascending row order however the walk found them.
        ntie = 0_int64
        do k = 1_int64, m
            if (dd(k) /= 1.0_real64) cycle
            ntie = ntie + 1_int64
            if (ntie > 1_int64) then
                call check(error, got(k) > got(k - 1_int64), &
                    "an exact tie must be broken by ascending row index, so the order is the same everywhere")
                if (allocated(error)) return
            end if
        end do
        call check(error, ntie == 6_int64, "the fixture must present exactly six exactly-tied rows")
    end subroutine test_sorted_breaks_ties_by_row

    ! ---- k nearest neighbours, and the k-th neighbour distance ----

    !> `%nearest` against a full sort of every distance, over several k and several query points.
    subroutine test_nearest_matches_a_full_sort(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), wd(:), gd(:)
        integer(int64), allocatable :: wrows(:)
        integer(int64) :: got(64), n, m, k, q, kk
        type(pf_spatial_index) :: sx
        real(real64) :: p(3), free(3)
        integer :: ik

        n = 500_int64
        free = 0.0_real64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64)
        allocate (gd(64))
        do ik = 1, 3
            select case (ik)
            case (1)
                kk = 1_int64
            case (2)
                kk = 7_int64
            case default
                kk = 40_int64
            end select
            do q = 1_int64, 8_int64
                p(1) = pf_random_at(fixture_seed + 41_int64, q, 1_int64)
                p(2) = pf_random_at(fixture_seed + 41_int64, q, 2_int64)
                p(3) = pf_random_at(fixture_seed + 41_int64, q, 3_int64)
                m = sx%nearest(p, kk, got, dist=gd)
                call check(error, m == kk, "%nearest must report min(k, %size()) as the true count")
                if (allocated(error)) return
                call brute_nearest(x, y, z, p, free, wrows, wd)
                do k = 1_int64, m
                    call check(error, got(k) == wrows(k), &
                        "%nearest must return the same rows, in the same order, as a full sort of every distance")
                    if (allocated(error)) return
                    call check(error, abs(gd(k) - wd(k)) <= 1.0e-12_real64, &
                        "%nearest's distances must match the full sort's")
                    if (allocated(error)) return
                end do
            end do
        end do
        ! A query point sitting exactly on a catalogue row must find it, at distance zero.
        p = [x(37), y(37), z(37)]
        m = sx%nearest(p, 3_int32, got, dist=gd)
        call check(error, got(1) == 37_int64 .and. gd(1) == 0.0_real64, &
            "a query coincident with a row must return that row first, at distance zero")
    end subroutine test_nearest_matches_a_full_sort

    !> `k` at and beyond the catalogue size, and a one-point index.
    subroutine test_nearest_k_at_and_beyond_the_size(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: got(200), n, m
        real(real64) :: gd(200), one(1)
        type(pf_spatial_index) :: sx, s1

        n = 120_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        m = sx%nearest([0.5_real64, 0.5_real64, 0.5_real64], n, got, dist=gd)
        call check(error, m == n, "k = n must return every row")
        if (allocated(error)) return
        ! Every row, so the last distance is the farthest point in the cloud -- which is what the
        ! expanding ball's own cap has to have reached.
        m = sx%nearest([0.5_real64, 0.5_real64, 0.5_real64], 5000_int64, got, dist=gd)
        call check(error, m == n, "k beyond the catalogue size must return every row rather than failing")
        if (allocated(error)) return
        one = 0.0_real64
        call s1%build(one, one, one, radius=1.0_real64)
        m = s1%nearest([3.0_real64, 0.0_real64, 0.0_real64], 4_int32, got, dist=gd)
        call check(error, m == 1_int64 .and. got(1) == 1_int64, &
            "a one-point index must return its single row")
        if (allocated(error)) return
        call check(error, abs(gd(1) - 3.0_real64) <= 1.0e-12_real64, &
            "and the distance to it, however far outside the cloud the query sits")
    end subroutine test_nearest_k_at_and_beyond_the_size

    !> The expanding ball's starting radius changes the ROUNDS and never the answers.
    !>
    !> **A shell that never expands passes every correctness test ever written for it**, because
    !> the answers do not depend on how many rounds it took. Forcing a deliberately tiny start
    !> against a deliberately generous one, and asserting the round counter moved, is what
    !> separates "the expansion works" from "the expansion never happened".
    subroutine test_nearest_shell_start_does_not_change_answers(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64) :: tiny_rows(12), big_rows(12), auto_rows(12), m, k
        integer(int64) :: rounds_tiny, rounds_big
        real(real64) :: p(3)
        type(pf_spatial_index) :: sx

        call make_cloud(600_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64)
        p = [0.5_real64, 0.5_real64, 0.5_real64]
        m = sx%nearest(p, 12_int32, auto_rows)
        call parquet_debug_reset_spatial_counters()
        call parquet_debug_set_spatial_shell_start(0.0005_real64)
        m = sx%nearest(p, 12_int32, tiny_rows)
        rounds_tiny = parquet_debug_spatial_shell_rounds()
        call parquet_debug_reset_spatial_counters()
        call parquet_debug_set_spatial_shell_start(0.9_real64)
        m = sx%nearest(p, 12_int32, big_rows)
        rounds_big = parquet_debug_spatial_shell_rounds()
        call parquet_debug_reset_spatial_counters()
        do k = 1_int64, 12_int64
            call check(error, tiny_rows(k) == big_rows(k) .and. tiny_rows(k) == auto_rows(k), &
                "the starting radius must change how long the search takes and never what it returns")
            if (allocated(error)) return
        end do
        call check(error, rounds_big == 1_int64, "a generous start must converge in one round")
        if (allocated(error)) return
        call check(error, rounds_tiny > rounds_big, &
            "a tiny start must take more rounds, which is what proves the ball expanded at all")
    end subroutine test_nearest_shell_start_does_not_change_answers

    !> `%nearest_sky` against a haversine sort, and `%nearest` on a periodic index.
    subroutine test_nearest_sky_and_periodic(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), x(:), y(:), z(:), wd(:), sep(:)
        integer(int64), allocatable :: wrows(:), perm(:)
        integer(int64) :: got(16), n, m, k
        real(real64) :: gd(16), p(3), box(3)
        type(pf_spatial_index) :: sk, sx

        n = 600_int64
        call make_sky(n, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=3.0_real64)
        allocate (sep(n))
        ! At the north pole, across 0h and in the sparse background alike: the last of those is
        ! what needs the shell to grow well past the radius the index was tuned for.
        do k = 1_int64, n
            sep(k) = sky_sep(0.0_real64, 90.0_real64, ra(k), dec(k))
        end do
        call pf_argsort(sep, perm)
        m = sk%nearest_sky(0.0_real64, 90.0_real64, 9_int32, got, dist_deg=gd)
        call check(error, m == 9_int64, "%nearest_sky must report the true count")
        if (allocated(error)) return
        do k = 1_int64, m
            call check(error, got(k) == perm(k), &
                "%nearest_sky must return the rows a haversine sort puts first")
            if (allocated(error)) return
            call check(error, abs(gd(k) - sep(perm(k))) <= 1.0e-9_real64, &
                "%nearest_sky must report the separation in degrees")
            if (allocated(error)) return
        end do
        do k = 1_int64, n
            sep(k) = sky_sep(200.0_real64, -75.0_real64, ra(k), dec(k))
        end do
        call pf_argsort(sep, perm)
        m = sk%nearest_sky(200.0_real64, -75.0_real64, 5_int32, got, dist_deg=gd)
        do k = 1_int64, m
            call check(error, got(k) == perm(k), &
                "%nearest_sky must be right in the sparse background too, where the ball must grow")
            if (allocated(error)) return
        end do
        ! Periodic: the minimum image decides which neighbours are nearest, and the shell is
        ! capped at half the box.
        box = 1.0_real64
        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
            box_hi=[1.0_real64, 1.0_real64, 1.0_real64])
        p = [0.02_real64, 0.98_real64, 0.5_real64]
        m = sx%nearest(p, 6_int32, got, dist=gd)
        call brute_nearest(x, y, z, p, box, wrows, wd)
        do k = 1_int64, m
            call check(error, got(k) == wrows(k), &
                "a periodic %nearest must agree with a minimum-image sort, including across the faces")
            if (allocated(error)) return
        end do
    end subroutine test_nearest_sky_and_periodic

    !> `%kth_distance` against a full per-point sort that excludes self.
    subroutine test_kth_distance_matches_a_full_sort(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), got(:), want(:), gd(:)
        integer(int64) :: n, i, m
        integer(int64) :: rows(8)
        type(pf_spatial_index) :: sx
        integer :: ik
        integer(int64) :: k

        n = 300_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        allocate (gd(8))
        do ik = 1, 3
            select case (ik)
            case (1)
                k = 1_int64
            case (2)
                k = 5_int64
            case default
                k = n - 1_int64
            end select
            call sx%kth_distance(k, got)
            call brute_kth(x, y, z, k, want)
            call check(error, size(got, kind=int64) == n, &
                "%kth_distance must report one distance per row, in the caller's row order")
            if (allocated(error)) return
            call check(error, maxval(abs(got - want)) <= 1.0e-12_real64, &
                "%kth_distance must match a per-point sort that excludes the point itself")
            if (allocated(error)) return
        end do
        ! A second, independent oracle: the (k+1)-th nearest INCLUDING self is the k-th excluding
        ! it, valid here because no two points of this fixture coincide.
        call sx%kth_distance(3_int32, got)
        do i = 1_int64, n
            m = sx%nearest([x(i), y(i), z(i)], 4_int32, rows, dist=gd)
            call check(error, abs(got(i) - gd(4)) <= 1.0e-12_real64, &
                "%kth_distance must agree with a loop of %nearest(p, k+1) taking the last distance")
            if (allocated(error)) return
        end do
    end subroutine test_kth_distance_matches_a_full_sort

    !> `%kth_distance_sky` answers in degrees, and coincident points give zero rather than failing.
    subroutine test_kth_distance_sky_and_duplicates(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), got(:)
        real(real64) :: x(6), y(6), z(6)
        real(real64), allocatable :: kd(:)
        integer(int64) :: n, i, j, k
        real(real64) :: best, sep
        type(pf_spatial_index) :: sk, sx

        n = 400_int64
        call make_sky(n, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=3.0_real64)
        call sk%kth_distance_sky(1_int32, got)
        do i = 1_int64, n
            best = 1.0e30_real64
            do j = 1_int64, n
                if (j == i) cycle
                sep = sky_sep(ra(i), dec(i), ra(j), dec(j))
                if (sep < best) best = sep
            end do
            call check(error, abs(got(i) - best) <= 1.0e-9_real64, &
                "%kth_distance_sky must report the nearest neighbour's separation in degrees")
            if (allocated(error)) return
        end do
        ! Four points share one position: self must be excluded by IDENTITY rather than by
        ! distance, or a duplicate would be mistaken for the point itself.
        x = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 1.0_real64, 2.0_real64]
        y = 0.0_real64
        z = 0.0_real64
        call sx%build(x, y, z, radius=1.0_real64)
        call sx%kth_distance(1_int32, kd)
        do k = 1_int64, 4_int64
            call check(error, kd(k) == 0.0_real64, &
                "a point with a coincident twin has a nearest neighbour at distance zero, not itself")
            if (allocated(error)) return
        end do
        call check(error, kd(5) == 1.0_real64, "and a point with no twin reports its real neighbour")
        if (allocated(error)) return
        call sx%kth_distance(4_int32, kd)
        call check(error, kd(1) == 1.0_real64, &
            "the 4th neighbour of one of four coincident points is the first point outside the clump")
    end subroutine test_kth_distance_sky_and_duplicates

    !> A threaded k-th neighbour sweep returns exactly the serial answer.
    subroutine test_kth_distance_threaded_matches_serial(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), one(:), many(:)
        type(pf_spatial_index) :: sx

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it both arms run the same serial sweep, so the " // &
            "equality below would hold for the wrong reason")
        return
#endif
        call make_clustered_cloud(1200_int64, x, y, z)
        call sx%build(x, y, z, radius=0.05_real64)
        call sx%kth_distance(4_int32, one, threads=1)
        call sx%kth_distance(4_int32, many, threads=4)
        call check(error, maxval(abs(one - many)) == 0.0_real64, &
            "a threaded k-th neighbour sweep must return exactly the serial answer, bit for bit")
    end subroutine test_kth_distance_threaded_matches_serial

    !> The radius carried from one point to the next saves expansion rounds.
    !>
    !> **This is what stops the carry-over silently ceasing to work.** It changes only the starting
    !> radius, so no answer can ever reveal that it stopped -- only a counter can, and the cost of
    !> losing it is several times the runtime with nothing failing. The fixture is CLUSTERED on
    !> purpose: on a uniform cloud the density-derived start is already right everywhere and there
    !> is nothing for a carried radius to improve on.
    subroutine test_kth_distance_carry_over_saves_rounds(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), kd(:)
        integer(int64) :: n, i, m, rounds_swept, rounds_cold
        integer(int64) :: rows(8)
        real(real64) :: gd(8)
        type(pf_spatial_index) :: sx

        n = 1200_int64
        call make_clustered_cloud(n, x, y, z)
        call sx%build(x, y, z, radius=0.05_real64)
        ! The sweep runs in stored order, so each query starts from the radius the previous one
        ! converged to. A loop of %nearest starts every query from the density-derived radius
        ! instead, which is the cold-start comparison.
        call parquet_debug_reset_spatial_counters()
        call sx%kth_distance(4_int32, kd, threads=1)
        rounds_swept = parquet_debug_spatial_shell_rounds()
        call parquet_debug_reset_spatial_counters()
        do i = 1_int64, n
            m = sx%nearest([x(i), y(i), z(i)], 5_int32, rows, dist=gd)
        end do
        rounds_cold = parquet_debug_spatial_shell_rounds()
        call parquet_debug_reset_spatial_counters()
        call check(error, rounds_cold >= n, "the control: a cold start takes at least one round per point")
        if (allocated(error)) return
        call check(error, rounds_swept < rounds_cold, &
            "carrying the converged radius from one point to the next must save expansion rounds")
    end subroutine test_kth_distance_carry_over_saves_rounds

    ! ---- Connected components ----

    !> Hand-built graphs whose components are known by inspection.
    !>
    !> **Isolated vertices sit at the END of the numbering on purpose.** That is the case a `nvert`
    !> derived from the edge list gets wrong: an isolated vertex never appears in an edge list, so
    !> `max(maxval(i), maxval(j))` would silently return a shorter `labels` array than the
    !> catalogue has rows -- a wrong answer with no symptom.
    subroutine test_components_hand_built_graphs(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        integer(int64) :: ei(9), ej(9), pi(9), pj(9)
        integer(int64), allocatable :: labels(:), other(:), sizes(:)
        integer(int64) :: ncomp, k

        ! A chain on 1-4, a star on 5-7, a two-vertex component on 8-9, and 10, 11, 12 isolated.
        ! The list carries a duplicate edge and a reversed one, both of which must be harmless.
        ei = [1_int64, 2_int64, 3_int64, 5_int64, 5_int64, 8_int64, 9_int64, 3_int64, 2_int64]
        ej = [2_int64, 3_int64, 4_int64, 6_int64, 7_int64, 9_int64, 8_int64, 2_int64, 1_int64]
        call pf_connected_components(ei, ej, 12_int32, labels, ncomp=ncomp, sizes=sizes, min_size=2)
        call check(error, size(labels, kind=int64) == 12_int64, &
            "labels must be as long as the vertex count, isolated vertices included")
        if (allocated(error)) return
        call check(error, ncomp == 3_int64, "this graph has three components of at least two vertices")
        if (allocated(error)) return
        ! Numbered by ascending vertex of first appearance, which is a contract rather than a
        ! detail: left to the union-find's own roots it would depend on tie-breaking inside it.
        call check(error, all(labels(1:4) == 1_int64), "the component containing vertex 1 must be label 1")
        if (allocated(error)) return
        call check(error, all(labels(5:7) == 2_int64), "the next component in vertex order must be label 2")
        if (allocated(error)) return
        call check(error, all(labels(8:9) == 3_int64), "and the next after that must be label 3")
        if (allocated(error)) return
        call check(error, all(labels(10:12) == 0_int64), &
            "under min_size = 2 an isolated vertex must be labelled 0")
        if (allocated(error)) return
        call check(error, size(sizes, kind=int64) == 3_int64, "sizes must be as long as ncomp")
        if (allocated(error)) return
        call check(error, all(sizes == [4_int64, 3_int64, 2_int64]), &
            "sizes must follow the same label order")
        if (allocated(error)) return
        ! Edge ORDER must not reach the answer: reversing the list exercises a different sequence
        ! of unions and so a different internal root for every component.
        do k = 1_int64, 9_int64
            pi(k) = ei(10_int64 - k)
            pj(k) = ej(10_int64 - k)
        end do
        call pf_connected_components(pi, pj, 12_int64, other, min_size=2)
        call check(error, all(other == labels), &
            "the labels must not depend on the order the edges arrived in")
        if (allocated(error)) return
        ! An int64 vertex count must agree with an int32 one.
        call pf_connected_components(ei, ej, 12_int64, other, min_size=2)
        call check(error, all(other == labels), "both vertex-count kinds must give the same labels")
    end subroutine test_components_hand_built_graphs

    !> `min_size` thresholds components, with the default of 1 as the negative control.
    subroutine test_components_min_size_threshold(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        integer(int64) :: ei(5), ej(5), none_i(0), none_j(0)
        integer(int64), allocatable :: labels(:), other(:), sizes(:)
        integer(int64) :: ncomp, k

        ! Components of size 2, 3 and 4, plus vertex 10 isolated with a self-loop on it.
        ei = [1_int64, 3_int64, 4_int64, 6_int64, 10_int64]
        ej = [2_int64, 4_int64, 5_int64, 7_int64, 10_int64]
        ei(4) = 6_int64
        ej(4) = 7_int64
        ! The DEFAULT is the textbook reading: every vertex belongs to some component, so the
        ! three groups plus the two singletons are five, and the labels leave nobody out.
        call pf_connected_components([ei(1), ei(2), ei(3), 6_int64, 7_int64, 10_int64], &
            [ej(1), ej(2), ej(3), 7_int64, 8_int64, 10_int64], 10_int64, labels, ncomp=ncomp, sizes=sizes)
        call check(error, ncomp == 5_int64, &
            "the default min_size of 1 must count every vertex: three groups plus vertices 9 and 10")
        if (allocated(error)) return
        call check(error, all(labels > 0_int64), "and must leave no vertex unlabelled")
        if (allocated(error)) return
        call check(error, sum(sizes) == 10_int64, "under the default the sizes must add up to every vertex")
        if (allocated(error)) return
        ! Passing 1 explicitly must be the same call.
        call pf_connected_components([ei(1), ei(2), ei(3), 6_int64, 7_int64, 10_int64], &
            [ej(1), ej(2), ej(3), 7_int64, 8_int64, 10_int64], 10_int64, other, ncomp=ncomp, min_size=1)
        call check(error, ncomp == 5_int64 .and. all(other == labels), &
            "min_size = 1 must be exactly what the default already does")
        if (allocated(error)) return
        ! min_size = 2 is the group-catalogue threshold, and the control that proves the argument
        ! is doing something rather than nothing.
        call pf_connected_components([ei(1), ei(2), ei(3), 6_int64, 7_int64, 10_int64], &
            [ej(1), ej(2), ej(3), 7_int64, 8_int64, 10_int64], 10_int64, labels, ncomp=ncomp, sizes=sizes, &
            min_size=2)
        call check(error, ncomp == 3_int64, "min_size = 2 must keep only the three multi-vertex components")
        if (allocated(error)) return
        call check(error, all(sizes == [2_int64, 3_int64, 3_int64]), &
            "their sizes are 2, 3 and 3, in ascending first-vertex order")
        if (allocated(error)) return
        ! **A self-loop is not company.** "Isolated" means component size 1, not "has no edge" --
        ! the two differ exactly here, and a vertex whose only edge is to itself is alone.
        call check(error, labels(10) == 0_int64, &
            "under min_size = 2 a vertex whose only edge is a self-loop is still isolated")
        if (allocated(error)) return
        call check(error, labels(9) == 0_int64, "and so is one with no edge at all")
        if (allocated(error)) return
        ! min_size = 3 drops the pair as well.
        call pf_connected_components([ei(1), ei(2), ei(3), 6_int64, 7_int64, 10_int64], &
            [ej(1), ej(2), ej(3), 7_int64, 8_int64, 10_int64], 10_int64, labels, ncomp=ncomp, min_size=3)
        call check(error, ncomp == 2_int64, "min_size = 3 must drop the two-vertex component")
        if (allocated(error)) return
        call check(error, labels(1) == 0_int64 .and. labels(2) == 0_int64, &
            "and must unlabel both its vertices")
        if (allocated(error)) return
        ! An empty edge list is every vertex on its own -- which the default now says outright.
        call pf_connected_components(none_i, none_j, 5_int64, labels, ncomp=ncomp, sizes=sizes)
        call check(error, ncomp == 5_int64, "under the default an empty edge list is five components")
        if (allocated(error)) return
        call pf_connected_components(none_i, none_j, 5_int64, other, ncomp=ncomp, min_size=2)
        call check(error, ncomp == 0_int64 .and. all(other == 0_int64), &
            "under min_size = 2 an empty edge list has no component of two or more, so every label is 0")
        if (allocated(error)) return
        do k = 1_int64, 5_int64
            call check(error, labels(k) == k, "each of which is its own vertex, numbered in vertex order")
            if (allocated(error)) return
        end do
        call check(error, all(sizes == 1_int64), "and each of size one")
        if (allocated(error)) return
        ! Zero vertices is an answer, not a failure.
        call pf_connected_components(none_i, none_j, 0_int64, labels, ncomp=ncomp, sizes=sizes)
        call check(error, size(labels) == 0 .and. ncomp == 0_int64 .and. size(sizes) == 0, &
            "an empty graph has no vertices, no components and no sizes")
    end subroutine test_components_min_size_threshold

    !> Friends-of-Friends end to end: `%pairs_within` then `pf_connected_components`.
    !>
    !> The oracle is LABEL PROPAGATION over an O(n^2) adjacency, which shares no machinery with
    !> either half of what is under test -- only the definition of "connected". Partitions are
    !> compared rather than label values, since the oracle numbers its groups differently.
    subroutine test_components_friends_of_friends(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: pi(:), pj(:), labels(:), want(:), sizes(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, ncomp, k, total

        n = 400_int64
        call make_clustered_cloud(n, x, y, z)
        call sx%build(x, y, z, radius=0.02_real64)
        call sx%pairs_within(0.02_real64, pi, pj)
        call pf_connected_components(pi, pj, n, labels, ncomp=ncomp, sizes=sizes, min_size=1)
        call brute_components(x, y, z, 0.02_real64, want)
        call check(error, same_partition(labels, want), &
            "Friends-of-Friends must recover exactly the components a label-propagation scan finds")
        if (allocated(error)) return
        call check(error, ncomp > 1_int64 .and. ncomp < n, &
            "this fixture must produce several groups rather than one blob or n singletons")
        if (allocated(error)) return
        total = 0_int64
        do k = 1_int64, ncomp
            total = total + sizes(k)
        end do
        call check(error, total == n, "under min_size = 1 the component sizes must add up to every vertex")
        if (allocated(error)) return
        do k = 1_int64, n
            call check(error, sizes(labels(k)) == count(labels == labels(k)), &
                "each reported size must be how many vertices actually carry that label")
            if (allocated(error)) return
        end do
    end subroutine test_components_friends_of_friends

    ! ---- Periodic boundaries ----

    !> A periodic index against a brute-force scan that applies the minimum image itself.
    subroutine test_periodic_matches_brute_force(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        real(real64) :: p(3), side, r, wrap(3)

        n = 700_int64
        side = 4.0_real64
        r = 0.7_real64
        wrap = side
        call make_cloud(n, side, .false., x, y, z)
        call sx%build(x, y, z, radius=r, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
            box_hi=[side, side, side])
        call check(error, sx%is_periodic(), "an index given a box must report itself periodic")
        if (allocated(error)) return
        allocate (got(n))
        do q = 1_int64, 40_int64
            ! Query points deliberately ON the faces and corners, where a non-wrapping walk gives
            ! the right answer for the interior and the wrong one here.
            p(1) = side * real(mod(q, 4_int64), kind=real64) / 4.0_real64
            p(2) = side * pf_random_at(fixture_seed + 17_int64, q, 1_int64)
            p(3) = side * pf_random_at(fixture_seed + 17_int64, q, 2_int64)
            mg = sx%within(p, r, got)
            call brute_within(x, y, z, p, r, wrap, want, mw)
            call check(error, same_rows(got, mg, want, mw), &
                "a periodic query must return exactly the rows a minimum-image scan finds")
            if (allocated(error)) return
        end do
    end subroutine test_periodic_matches_brute_force

    !> Translation invariance: shifting every point by an arbitrary vector modulo the box must
    !> leave every neighbour set identical.
    !>
    !> This is what catches a grid whose cells do not tile the box exactly -- the seam cell would
    !> be a different width, and every query crossing it wrong by a fraction of a cell. A
    !> face-crossing spot check passes against that; this does not.
    subroutine test_periodic_translation_invariant(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), x2(:), y2(:), z2(:)
        integer(int64), allocatable :: o1(:), n1(:), o2(:), n2(:)
        type(pf_spatial_index) :: a, b
        integer(int64) :: n, i
        real(real64) :: side, r, sh(3)

        n = 900_int64
        side = 3.0_real64
        r = 0.45_real64
        ! A shift that is not a whole number of cells, on purpose.
        sh = [1.2345_real64, 0.6789_real64, 2.0101_real64]
        call make_cloud(n, side, .false., x, y, z)
        allocate (x2(n), y2(n), z2(n))
        do i = 1_int64, n
            x2(i) = modulo(x(i) + sh(1), side)
            y2(i) = modulo(y(i) + sh(2), side)
            z2(i) = modulo(z(i) + sh(3), side)
        end do
        call a%build(x, y, z, radius=r, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], box_hi=[side, side, side])
        call b%build(x2, y2, z2, radius=r, box_lo=[0.0_real64, 0.0_real64, 0.0_real64], box_hi=[side, side, side])
        call a%all_within(r, o1, n1)
        call b%all_within(r, o2, n2)
        call check(error, all(o1 == o2), &
            "translating every point inside a periodic box must not change any neighbour COUNT")
        if (allocated(error)) return
        do i = 1_int64, n
            call check(error, same_rows(n1(o1(i):o1(i + 1_int64) - 1_int64), o1(i + 1_int64) - o1(i), &
                n2(o2(i):o2(i + 1_int64) - 1_int64), o2(i + 1_int64) - o2(i)), &
                "translating every point inside a periodic box must not change any neighbour SET")
            if (allocated(error)) return
        end do
    end subroutine test_periodic_translation_invariant

    !> A periodic 2D index: `box_lo`/`box_hi` of rank 2, with z left free.
    subroutine test_periodic_2d(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        real(real64) :: p(2), p3(3), side, r, wrap(3)

        n = 600_int64
        side = 2.0_real64
        r = 0.35_real64
        wrap = [side, side, 0.0_real64]
        call make_cloud(n, side, .true., x, y, z)
        call sx%build(x, y, radius=r, box_lo=[0.0_real64, 0.0_real64], box_hi=[side, side])
        allocate (got(n))
        do q = 1_int64, 25_int64
            p(1) = side * pf_random_at(fixture_seed + 23_int64, q, 1_int64)
            p(2) = side * pf_random_at(fixture_seed + 23_int64, q, 2_int64)
            p3 = [p(1), p(2), 0.0_real64]
            mg = sx%within(p, r, got)
            call brute_within(x, y, z, p3, r, wrap, want, mw)
            call check(error, same_rows(got, mg, want, mw), &
                "a periodic 2D query must return exactly the rows a minimum-image scan finds")
            if (allocated(error)) return
        end do
    end subroutine test_periodic_2d

    !> A periodic grid's cells must tile the box EXACTLY, whatever cell size was asked for.
    !>
    !> **This is asserted directly rather than through an answer**, because the answer is a poor
    !> detector of it: a grid that fails to tile folds its last partial slab into cell 0, which the
    !> wrapped walk still visits, so the exact distance test filters the strays and the query comes
    !> back right. A mutation replacing `L/nx` with the requested `h` was confirmed to survive every
    !> answer-based periodic test here, including translation invariance. The property is real -- the
    !> seam cell is a different width from every other -- and only its ARITHMETIC can see it, so the
    !> assertion is `nx * cell_x == L` to the last bit.
    subroutine test_periodic_cells_tile_the_box(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: nx, ny, nz
        real(real64) :: cx, cy, cz, box(3)
        integer :: k
        real(real64), parameter :: asked(4) = [0.7_real64, 0.31_real64, 1.1_real64, 0.4999_real64]

        box = [3.0_real64, 2.0_real64, 5.0_real64]
        call make_cloud(800_int64, 3.0_real64, .false., x, y, z)
        z = z * box(3) / 3.0_real64
        y = y * box(2) / 3.0_real64
        do k = 1, size(asked)
            ! Deliberately cells that divide NONE of the three sides, and a box that is not cubic
            ! so the three axes must each get their own answer.
            call sx%build(x, y, z, radius=0.25_real64, cell=asked(k), &
                box_lo=[0.0_real64, 0.0_real64, 0.0_real64], box_hi=box)
            call sx%grid(nx, ny, nz)
            call sx%cell_sides(cx, cy, cz)
            call check(error, real(nx, kind=real64) * cx == box(1), &
                "a periodic grid's x cells must tile the box exactly")
            if (allocated(error)) return
            call check(error, real(ny, kind=real64) * cy == box(2), &
                "a periodic grid's y cells must tile the box exactly")
            if (allocated(error)) return
            call check(error, real(nz, kind=real64) * cz == box(3), &
                "a periodic grid's z cells must tile the box exactly")
            if (allocated(error)) return
            ! And the side must be at least what was asked for -- rounding the cell count DOWN is
            ! what keeps the reach a query computes from under-covering.
            call check(error, cx >= asked(k) .and. cy >= asked(k) .and. cz >= asked(k), &
                "tiling must adjust the cell side UP, never down")
            if (allocated(error)) return
        end do
        ! Negative control: a non-periodic index must NOT adjust the side, or `cell=` would stop
        ! meaning what it says on the path where nothing has to tile.
        call sx%build(x, y, z, radius=0.25_real64, cell=0.7_real64)
        call sx%cell_sides(cx, cy, cz)
        call check(error, cx == 0.7_real64 .and. cy == 0.7_real64 .and. cz == 0.7_real64, &
            "a non-periodic index must use the cell side it was given, on every axis")
    end subroutine test_periodic_cells_tile_the_box

    ! ---- The cell-size tuner ----

    !> The probe runs on an ordinary build, and a forced cell stops it.
    !>
    !> The second half is the negative control: without it this would pass just as happily against
    !> a counter that is incremented unconditionally.
    subroutine test_probe_runs_and_can_be_stopped(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: ncand

        call parquet_debug_reset_spatial_counters()
        call make_cloud(4000_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.08_real64)
        ncand = parquet_debug_spatial_probe_count()
        call check(error, ncand >= 3_int64, "an ordinary build must evaluate at least the three bracket candidates")
        if (allocated(error)) return

        call sx%build(x, y, z, radius=0.08_real64, cell=0.1_real64)
        call check(error, parquet_debug_spatial_probe_count() == 0_int64, &
            "an explicit cell= must stop the probe running at all")
        if (allocated(error)) return

        call parquet_debug_set_spatial_cell(0.12_real64)
        call sx%build(x, y, z, radius=0.08_real64)
        call check(error, parquet_debug_spatial_probe_count() == 0_int64, &
            "a forced debug cell must stop the probe running at all")
        if (allocated(error)) return
        call check(error, abs(sx%cell_size() - 0.12_real64) <= 1.0e-12_real64, &
            "a forced debug cell must be the cell the index uses")
        call parquet_debug_reset_spatial_counters()
    end subroutine test_probe_runs_and_can_be_stopped

    !> The probe counts work rather than timing it, so the same points must give the same cell.
    subroutine test_probe_is_deterministic(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: a, b
        real(real64) :: h1, h2

        call parquet_debug_reset_spatial_counters()
        call make_cloud(3000_int64, 1.0_real64, .false., x, y, z)
        call a%build(x, y, z, radius=0.1_real64)
        h1 = a%cell_size()
        call b%build(x, y, z, radius=0.1_real64)
        h2 = b%cell_size()
        call check(error, h1 == h2, "a deterministic probe must choose the same cell for the same points")
        if (allocated(error)) return
        call check(error, h1 > 0.0_real64, "the chosen cell must be positive")
    end subroutine test_probe_is_deterministic

    !> The answers must not depend on the cell size. This is the property the whole tuner is
    !> allowed to trade against, and the one thing it must never change.
    subroutine test_answers_survive_any_cell(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw, q
        integer :: ic
        real(real64) :: cells(5), p(3), r

        n = 700_int64
        r = 0.17_real64
        cells = [0.02_real64, 0.06_real64, 0.2_real64, 0.6_real64, 3.0_real64]
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (got(n))
        do ic = 1, 5
            call sx%build(x, y, z, radius=r, cell=cells(ic))
            do q = 1_int64, 15_int64
                p(1) = pf_random_at(fixture_seed + 29_int64, q, 1_int64)
                p(2) = pf_random_at(fixture_seed + 29_int64, q, 2_int64)
                p(3) = pf_random_at(fixture_seed + 29_int64, q, 3_int64)
                mg = sx%within(p, r, got)
                call brute_within(x, y, z, p, r, NO_WRAP, want, mw)
                call check(error, same_rows(got, mg, want, mw), &
                    "the rows a query returns must not depend on the cell size the index was built with")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_answers_survive_any_cell

    !> An explicit cell is used as given, and comes back from `%cell_size` ready to pass on.
    subroutine test_explicit_cell_is_honoured(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: a, b
        integer(int64) :: nx, ny, nz
        real(real64) :: ext

        call make_cloud(1000_int64, 1.0_real64, .false., x, y, z)
        call a%build(x, y, z, radius=0.1_real64, cell=0.25_real64)
        call check(error, abs(a%cell_size() - 0.25_real64) <= 1.0e-12_real64, &
            "an explicit cell= must be the cell the index uses")
        if (allocated(error)) return
        call a%grid(nx, ny, nz)
        ! The grid must be the MINIMAL cover of the data's own bounding box -- which is very
        ! slightly under the unit cube for a random fixture, so asserting a literal count here
        ! would be asserting a property of the sample rather than of the grid.
        ext = maxval(x) - minval(x)
        call check(error, real(nx, kind=real64) * 0.25_real64 >= ext, &
            "the grid must span the data's extent along x")
        if (allocated(error)) return
        call check(error, real(nx - 1_int64, kind=real64) * 0.25_real64 <= ext, &
            "the grid must not carry a whole spare cell beyond the data's extent along x")
        if (allocated(error)) return
        call check(error, nx > 1_int64 .and. ny > 1_int64 .and. nz > 1_int64, &
            "a unit-cube cloud at cell 0.25 must divide into several cells on every axis")
        if (allocated(error)) return
        ! Handing %cell_size() straight back is the documented way to reuse a tuned cell.
        call b%build(x, y, z, radius=0.1_real64, cell=a%cell_size())
        call check(error, b%cell_size() == a%cell_size(), &
            "a cell taken from one index must reproduce itself when passed to another")
    end subroutine test_explicit_cell_is_honoured

    !> The grid obeys two ceilings, and each binds where it should: at most 0.3 OCCUPIED cells per
    !> point, and at most 4 cells of the bounding box per point (`spatial_max_cells_per_point`,
    !> `spatial_max_box_cells_per_point`). On a cloud filling its box nearly every cell is occupied,
    !> so the occupied ceiling binds and holds the box count near 0.3 per point. On a cone filling
    !> a third of its box the occupied ceiling lets the box count past 0.3 per point, which is what
    !> tells this rule from one counting box cells. On a clustered cloud, whose clumps occupy a few
    !> per cent of the box's cells, the box cap binds first. A `cell=` fine enough to ask for a
    !> million cells is coarsened under both, and one that needs no coarsening is left exactly as
    !> given. The mutations: either comparison reverted, or box cells counted where occupied cells
    !> are meant.
    subroutine test_two_ceilings_bind(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), ra(:), dec(:), d(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, occ, box, gx, gy, gz

        ! ---- a cloud filling its box ----
        n = 2000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        ! A cell this fine asks for a million cells for two thousand points.
        call sx%build(x, y, z, radius=0.1_real64, cell=0.01_real64)
        call parquet_debug_spatial_occupied_cells(sx, occ)
        call sx%grid(gx, gy, gz)
        box = gx * gy * gz
        call check(error, occ <= (3_int64 * n) / 10_int64, "occupied cells must be clamped to at most 0.3 per point")
        if (allocated(error)) return
        call check(error, box <= 4_int64 * n, "cells of the bounding box must be clamped to at most 4 per point")
        if (allocated(error)) return
        call check(error, box <= (4_int64 * n) / 10_int64, &
            "on a full box the occupied ceiling must bind, holding the box count near 0.3 per point")
        if (allocated(error)) return
        call check(error, sx%cell_size() > 0.01_real64, &
            "a clamped build must report the coarsened cell, not the one that was asked for")
        if (allocated(error)) return
        ! And the clamp must not fire when it is not needed.
        call sx%build(x, y, z, radius=0.1_real64, cell=0.25_real64)
        call check(error, abs(sx%cell_size() - 0.25_real64) <= 1.0e-12_real64, &
            "a cell that needs no clamping must be left exactly as given")
        if (allocated(error)) return
        ! ---- a cone filling a third of its box: the occupied ceiling binds, past 0.3 box cells ----
        call make_wedge(1500_int64, 100_int64, 500.0_real64, 1500.0_real64, 33_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        call sx%build(x, y, z, radius=2.0_real64)
        call parquet_debug_spatial_occupied_cells(sx, occ)
        call sx%grid(gx, gy, gz)
        box = gx * gy * gz
        call check(error, occ <= (3_int64 * n) / 10_int64, "on a cone the occupied cells must stay under 0.3 per point")
        if (allocated(error)) return
        call check(error, box <= 4_int64 * n, "on a cone the box cells must stay under 4 per point")
        if (allocated(error)) return
        call check(error, box > (3_int64 * n) / 10_int64, &
            "on a cone the box count must pass 0.3 per point: the ceiling counts occupied cells, not box cells")
        if (allocated(error)) return
        ! ---- a clustered cloud: the box cap binds, with the occupied count well under its ceiling ----
        call make_clustered_cloud(2000_int64, x, y, z)
        n = 2000_int64
        call sx%build(x, y, z, radius=0.01_real64)
        call parquet_debug_spatial_occupied_cells(sx, occ)
        call sx%grid(gx, gy, gz)
        box = gx * gy * gz
        call check(error, box <= 4_int64 * n .and. box > 2_int64 * n, &
            "on a clustered cloud the box cap must bind, leaving the box count just under 4 per point")
        if (allocated(error)) return
        call check(error, occ < (3_int64 * n) / 10_int64, &
            "on a clustered cloud the occupied count must sit under its ceiling when the box cap binds")
    end subroutine test_two_ceilings_bind

    !> A radius list collapses to `sum(r^3)/sum(r^2)`, and a single radius to itself.
    subroutine test_effective_radius_moment_ratio(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        real(real64) :: rr(3), want

        call make_cloud(500_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call check(error, abs(sx%effective_radius() - 0.2_real64) <= 1.0e-12_real64, &
            "one radius must collapse to itself")
        if (allocated(error)) return
        rr = [0.1_real64, 0.2_real64, 0.5_real64]
        want = sum(rr**3) / sum(rr**2)
        call sx%build(x, y, z, radius=rr)
        call check(error, abs(sx%effective_radius() - want) <= 1.0e-12_real64, &
            "a radius list must collapse to its cubic-over-quadratic moment ratio")
    end subroutine test_effective_radius_moment_ratio

    !> `%rebuild` is a no-op on unchanged data and a real rebuild on changed data.
    subroutine test_rebuild_detects_change(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw
        logical :: did
        real(real64) :: p(3)

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64)
        call sx%rebuild(x, y, z, rebuilt=did)
        call check(error, .not. did, "rebuild must not rebuild when every coordinate is unchanged")
        if (allocated(error)) return
        ! One element differs, and it is deliberately a SMALL change: a checksum would be entitled
        ! to collide, an element-wise comparison cannot.
        x(n / 2_int64) = x(n / 2_int64) + 0.37_real64
        call sx%rebuild(x, y, z, rebuilt=did)
        call check(error, did, "rebuild must rebuild when a single coordinate has moved")
        if (allocated(error)) return
        allocate (got(n))
        p = 0.5_real64
        mg = sx%within(p, 0.15_real64, got)
        call brute_within(x, y, z, p, 0.15_real64, NO_WRAP, want, mw)
        call check(error, same_rows(got, mg, want, mw), &
            "after a rebuild the index must answer about the NEW coordinates")
    end subroutine test_rebuild_detects_change

    !> `%rebuild_for` re-tunes over the points already stored, with the caller supplying nothing.
    subroutine test_rebuild_for_retunes(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, mg, mw
        real(real64) :: h_before, p(3)

        n = 3000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.02_real64)
        h_before = sx%cell_size()
        call sx%rebuild_for(0.4_real64)
        call check(error, sx%cell_size() /= h_before, &
            "rebuild_for a much larger radius must choose a different cell")
        if (allocated(error)) return
        call check(error, abs(sx%effective_radius() - 0.4_real64) <= 1.0e-12_real64, &
            "rebuild_for must REPLACE the recorded radius, not add to it")
        if (allocated(error)) return
        allocate (got(n))
        p = 0.5_real64
        mg = sx%within(p, 0.4_real64, got)
        call brute_within(x, y, z, p, 0.4_real64, NO_WRAP, want, mw)
        call check(error, same_rows(got, mg, want, mw), &
            "a re-tuned index must still answer exactly what a brute-force scan finds")
    end subroutine test_rebuild_for_retunes

    !> A bulk query whose radius disagrees badly with the build rebuilds; a nearby one does not.
    !>
    !> The second half is the negative control. Without it the test would pass against an
    !> implementation that rebuilt on every bulk call, which is the failure mode that would make
    !> the mechanism a performance cliff rather than a safety net.
    subroutine test_auto_rebuild(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int64), allocatable :: counts(:)
        type(pf_spatial_index) :: sx
        character(len=:), allocatable :: verb_was

        ! The re-tune below is provoked on purpose, so its once-per-index advice is noise here.
        ! `verbosity` is the control now that spatial_rebuild_warning is gone; restored at the end,
        ! and the suite runs its tests serially so nothing else sees the toggle.
        call parquet_get_verbosity(verb_was)
        call parquet_set_verbosity("silent")
        call parquet_debug_reset_spatial_counters()
        call make_cloud(3000_int64, 1.0_real64, .false., x, y, z)

        call sx%build(x, y, z, radius=0.01_real64)
        call sx%count_all_within(0.012_real64, counts)
        call check(error, parquet_debug_spatial_rebuilds() == 0_int64, &
            "a bulk query at nearly the built radius must not rebuild")
        if (allocated(error)) return

        call sx%count_all_within(0.5_real64, counts)
        call check(error, parquet_debug_spatial_rebuilds() == 1_int64, &
            "a bulk query at a radius far from the built one must rebuild exactly once")
        if (allocated(error)) return
        call check(error, sx%effective_radius() > 0.01_real64, &
            "an automatic rebuild must fold the new radius into the recorded ones")

        call parquet_set_verbosity(verb_was)
        call parquet_debug_reset_spatial_counters()
    end subroutine test_auto_rebuild

    !> A SINGLE-point query never rebuilds, however far its radius is from the built one.
    !>
    !> Only a bulk entry point may rebuild: it does so in `spatial_bulk_setup`, before its parallel
    !> region, while single queries are what callers run concurrently from their own threads, and
    !> a rebuild reallocates the arrays another thread is walking. Every single Euclidean and sky
    !> query is run at a radius that makes a bulk query rebuild, and must leave the rebuild counter,
    !> the cell and the recorded radius untouched. The closing bulk call at the same radius is the
    !> negative control: without it the test passes against an index that never rebuilds at all.
    subroutine test_single_queries_never_rebuild(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        integer(int64), parameter :: n = 3000_int64
        real(real64), allocatable :: x(:), y(:), z(:), ra(:), dec(:)
        integer(int64), allocatable :: got(:), counts(:)
        real(real64) :: p(3), p2(3), h_was, r_was
        integer(int64) :: base, m
        type(pf_spatial_index) :: sx, sk
        character(len=:), allocatable :: verb_was

        ! The closing bulk calls re-tune on purpose; their once-per-index advice is noise here.
        call parquet_get_verbosity(verb_was)
        call parquet_set_verbosity("silent")
        allocate (got(n))

        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.01_real64)
        h_was = sx%cell_size()
        r_was = sx%effective_radius()
        base = parquet_debug_spatial_rebuilds()
        p = 0.5_real64
        p2 = [0.2_real64, 0.3_real64, 0.7_real64]
        m = sx%within(p, 0.5_real64, got)
        m = sx%count_within(p, 0.5_real64)
        m = sx%within_segment(p, p2, 0.5_real64, got)
        m = sx%within_cylinder(p, p2, 0.5_real64, got)
        m = sx%within_cone(p, p2, 0.4_real64, 0.5_real64, got)
        m = sx%nearest(p, 2000_int32, got)
        call check(error, parquet_debug_spatial_rebuilds() == base, &
            "a single Euclidean query must never rebuild the index")
        if (allocated(error)) return
        call check(error, sx%cell_size() == h_was .and. sx%effective_radius() == r_was, &
            "a single Euclidean query must leave the cell and the recorded radius as built")
        if (allocated(error)) return
        call sx%count_all_within(0.5_real64, counts)
        call check(error, parquet_debug_spatial_rebuilds() == base + 1_int64, &
            "a bulk query at the same radius must rebuild, or the assertions above are vacuous")
        if (allocated(error)) return

        call make_sky(n, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=0.1_real64)
        h_was = sk%cell_size()
        r_was = sk%effective_radius()
        base = parquet_debug_spatial_rebuilds()
        m = sk%within_sky(10.0_real64, 20.0_real64, 40.0_real64, got)
        m = sk%count_within_sky(10.0_real64, 20.0_real64, 40.0_real64)
        m = sk%nearest_sky(10.0_real64, 20.0_real64, 2000_int32, got)
        call check(error, parquet_debug_spatial_rebuilds() == base, &
            "a single sky query must never rebuild the index")
        if (allocated(error)) return
        call check(error, sk%cell_size() == h_was .and. sk%effective_radius() == r_was, &
            "a single sky query must leave the cell and the recorded radius as built")
        if (allocated(error)) return
        call sk%count_all_within_sky(40.0_real64, counts)
        call check(error, parquet_debug_spatial_rebuilds() == base + 1_int64, &
            "a bulk sky query at the same radius must rebuild, or the assertions above are vacuous")
        if (allocated(error)) return

        call parquet_set_verbosity(verb_was)
    end subroutine test_single_queries_never_rebuild

    !> A no-copy index answers exactly as a copying one, in every query family.
    !>
    !> **`copy=.false.` is a second walk, not a flag.** Every loop over a cell's rows is written
    !> twice -- once reading the index's own reordered arrays and once indirecting through
    !> `self%idx` into the caller's -- so a defect in the indirect half is invisible to every test
    !> that builds the ordinary way. `%within` alone does not reach it: the ball walk, the axis
    !> walk, the expanding ball and the four bulk sweeps each carry their own copy of the pair.
    subroutine test_copy_false_matches(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable, target :: x(:), y(:), z(:), los(:), wx(:), wy(:), wz(:)
        real(real64), allocatable :: ra(:), dec(:), d(:)
        integer(int64), allocatable :: got(:), want(:), oa(:), na(:), ob(:), nb(:), fa(:), fb(:)
        integer(int64), allocatable :: ia(:), ja(:), ib(:), jb(:)
        integer(int64), allocatable :: ca(:), cb(:)
        real(real64), allocatable :: da(:), db(:)
        type(pf_spatial_index) :: a, b, la, lb
        integer(int64) :: n, nw, ma, mb, q
        real(real64) :: p(3)
        real(real64), parameter :: e1(3) = [0.2_real64, 0.2_real64, 0.2_real64]
        real(real64), parameter :: e2(3) = [0.8_real64, 0.8_real64, 0.8_real64]

        n = 600_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call a%build(x, y, z, radius=0.15_real64, cell=0.2_real64)
        call b%build(x, y, z, radius=0.15_real64, cell=0.2_real64, copy=.false.)
        allocate (got(n), want(n))
        do q = 1_int64, 20_int64
            p(1) = pf_random_at(fixture_seed + 31_int64, q, 1_int64)
            p(2) = pf_random_at(fixture_seed + 31_int64, q, 2_int64)
            p(3) = pf_random_at(fixture_seed + 31_int64, q, 3_int64)
            ma = a%within(p, 0.15_real64, want)
            mb = b%within(p, 0.15_real64, got)
            call check(error, same_rows(got, mb, want, ma), &
                "a copy=.false. index must return exactly the rows a copying one does")
            if (allocated(error)) return
        end do
        ! The AXIS walk, in all three of its shapes.
        ma = a%within_segment(e1, e2, 0.15_real64, want)
        mb = b%within_segment(e1, e2, 0.15_real64, got)
        call check(error, ma > 0_int64 .and. same_rows(got, mb, want, ma), &
            "a copy=.false. index must answer within_segment as a copying one does")
        if (allocated(error)) return
        ma = a%within_cylinder(e1, e2, 0.15_real64, want)
        mb = b%within_cylinder(e1, e2, 0.15_real64, got)
        call check(error, ma > 0_int64 .and. same_rows(got, mb, want, ma), &
            "a copy=.false. index must answer within_cylinder as a copying one does")
        if (allocated(error)) return
        ma = a%within_cone(e1, e2, 0.05_real64, 0.25_real64, want)
        mb = b%within_cone(e1, e2, 0.05_real64, 0.25_real64, got)
        call check(error, ma > 0_int64 .and. same_rows(got, mb, want, ma), &
            "a copy=.false. index must answer within_cone as a copying one does")
        if (allocated(error)) return
        ! The expanding ball, which orders its answer, so the rows must match position by position.
        ma = a%nearest(e1, 12_int64, want)
        mb = b%nearest(e1, 12_int64, got)
        call check(error, ma == 12_int64 .and. mb == ma .and. all(got(1:ma) == want(1:ma)), &
            "a copy=.false. index must answer nearest in the same order")
        if (allocated(error)) return
        ! The four bulk sweeps, each of which walks the cells itself.
        call a%all_within(0.15_real64, oa, na)
        call b%all_within(0.15_real64, ob, nb)
        call check(error, size(na, kind=int64) > 0_int64 .and. all(oa == ob) .and. all(na == nb), &
            "a copy=.false. index must answer the same CSR")
        if (allocated(error)) return
        call a%count_all_within(0.15_real64, ca)
        call b%count_all_within(0.15_real64, cb)
        call check(error, all(ca == cb), "a copy=.false. index must answer the same counts")
        if (allocated(error)) return
        call a%pairs_within(0.15_real64, ia, ja)
        call b%pairs_within(0.15_real64, ib, jb)
        call check(error, size(ia, kind=int64) > 0_int64 .and. all(ia == ib) .and. all(ja == jb), &
            "a copy=.false. index must answer the same pair list")
        if (allocated(error)) return
        call a%kth_distance(4_int64, da)
        call b%kth_distance(4_int64, db)
        call check(error, all(da == db), "a copy=.false. index must answer the same kth distances")
        if (allocated(error)) return
        ! The line-of-sight cylinder, whose sweep indirects into `self%los_s` as well. The wedge
        ! is the fixture the other line-of-sight tests use: `los` is the distance itself, so the
        ! slope is well behaved and nothing here is testing a warning.
        call make_wedge(150_int64, 40_int64, 500.0_real64, 1500.0_real64, 5_int64, ra, dec, d, wx, wy, wz)
        nw = size(wx, kind=int64)
        allocate (los(nw), fa(nw), fb(nw))
        los = d
        call la%build(wx, wy, wz, radius=35.0_real64, los=los)
        call lb%build(wx, wy, wz, radius=35.0_real64, los=los, copy=.false.)
        call la%pairs_within_los(35.0_real64, 90.0_real64, ia, ja)
        call lb%pairs_within_los(35.0_real64, 90.0_real64, ib, jb)
        call check(error, size(ia, kind=int64) > 0_int64 .and. all(ia == ib) .and. all(ja == jb), &
            "a copy=.false. index must answer the same line-of-sight pairs")
        if (allocated(error)) return
        ma = la%within_los([wx(1), wy(1), wz(1)], 35.0_real64, 90.0_real64, fa, los_p=los(1))
        mb = lb%within_los([wx(1), wy(1), wz(1)], 35.0_real64, 90.0_real64, fb, los_p=los(1))
        call check(error, ma > 0_int64 .and. ma == mb .and. same_rows(fb, mb, fa, ma), &
            "a copy=.false. index must answer the same line-of-sight cylinder")
    end subroutine test_copy_false_matches

    !> The metadata queries report what was built.
    subroutine test_metadata_queries(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: nx, ny, nz

        call check(error, .not. sx%is_built(), "a fresh index must report itself unbuilt")
        if (allocated(error)) return
        call make_cloud(1000_int64, 2.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.2_real64)
        call check(error, sx%is_built(), "a built index must report itself built")
        if (allocated(error)) return
        call check(error, sx%size() == 1000_int64, "the index must report how many points it holds")
        if (allocated(error)) return
        call check(error, sx%ndim() == 3, "a 3D index must report three dimensions")
        if (allocated(error)) return
        call check(error, sx%metric() == PF_METRIC_EUCLIDEAN, "an ordinary build must report the Euclidean metric")
        if (allocated(error)) return
        call check(error, .not. sx%is_periodic(), "an index built without a box must not report itself periodic")
        if (allocated(error)) return
        call sx%grid(nx, ny, nz)
        call check(error, sx%cells() == nx * ny * nz, "the cell count must be the product of the axis counts")
        if (allocated(error)) return
        call sx%clear()
        call check(error, .not. sx%is_built(), "a cleared index must report itself unbuilt")
        if (allocated(error)) return
        call check(error, sx%size() == 0_int64, "a cleared index must hold no points")
    end subroutine test_metadata_queries

    !> Whether two lists of `(i, j)` pairs hold the same pairs, in any order.
    !>
    !> `%pairs_within_sky` promises each close pair exactly once with `i < j`, and promises
    !> nothing about the order -- which is the walk's, and so differs between the two backends by
    !> design. Composing each pair into one sortable key and comparing the sorted keys is what
    !> compares the promise rather than the walk. `n + 1` as the stride keeps the composition
    !> injective for every row index the fixture can hold.
    logical function same_pair_set(i1, j1, i2, j2, n) result(ok)
        integer(int64), intent(in) :: i1(:) !! first list's lower row indices.
        integer(int64), intent(in) :: j1(:) !! first list's upper row indices.
        integer(int64), intent(in) :: i2(:) !! second list's lower row indices.
        integer(int64), intent(in) :: j2(:) !! second list's upper row indices.
        integer(int64), intent(in) :: n !! how many points the index holds.
        integer(int64), allocatable :: k1(:), k2(:), p1(:), p2(:)
        integer(int64) :: t

        ok = size(i1, kind=int64) == size(i2, kind=int64)
        if (.not. ok) return
        allocate (k1(size(i1)), k2(size(i2)))
        k1 = i1 * (n + 1_int64) + j1
        k2 = i2 * (n + 1_int64) + j2
        call pf_argsort(k1, p1)
        call pf_argsort(k2, p2)
        do t = 1_int64, size(k1, kind=int64)
            if (k1(p1(t)) /= k2(p2(t))) ok = .false.
        end do
    end function same_pair_set

    ! ---- The HEALPix sky backend ----
    !
    ! **These are A/B equality tests, and an A/B equality passes just as happily when both arms
    ! ran the same code.** Every one of them therefore also asserts a NEGATIVE CONTROL: that the
    ! two indexes report different backends, and that the HEALPix arm really walked pixels while
    ! the 3D arm walked none. Without that, deleting the branch in `spatial_scan` would leave the
    ! whole group green.

    !> Builds the same points twice, once per backend, and checks the pair is genuinely a pair.
    subroutine build_both(ra, dec, radius_deg, s3, sh, error)
        real(real64), intent(in) :: ra(:) !! right ascension of every point, degrees.
        real(real64), intent(in) :: dec(:) !! declination of every point, degrees.
        real(real64), intent(in) :: radius_deg !! the radius both indexes are tuned for.
        type(pf_spatial_index), intent(out) :: s3 !! the 3D-grid index.
        type(pf_spatial_index), intent(out) :: sh !! the HEALPix index.
        type(error_type), allocatable, intent(out) :: error !! set if the pair is not a pair.

        call s3%build_sky(ra, dec, radius_deg=radius_deg)
        call sh%build_sky(ra, dec, radius_deg=radius_deg, backend=PF_SKY_HEALPIX)
        call check(error, s3%backend() == PF_SKY_GRID3D, &
                   "negative control: the default backend must be the 3D grid, or the two arms are one arm")
        if (allocated(error)) return
        call check(error, sh%backend() == PF_SKY_HEALPIX, &
                   "negative control: backend=PF_SKY_HEALPIX must be recorded, or the two arms are one arm")
        if (allocated(error)) return
        call check(error, sh%nside() > 0_int64 .and. sh%npix() == 12_int64 * sh%nside() ** 2, &
                   "a HEALPix index must report a positive nside and a matching pixel count")
    end subroutine build_both

    !> Every single-point sky query returns the identical result on both backends.
    !>
    !> **`sorted=.true.` makes this an ELEMENT-FOR-ELEMENT equality rather than a set comparison.**
    !> The ordering contract -- increasing distance, ties broken by ascending row index -- is
    !> applied after the walk from the distances the walk recorded, so it is backend-independent
    !> by construction and the two arms must agree exactly, in the rows AND in the distances. A
    !> single differing element is a bug in one of the two walks, and this does not have to know
    !> which.
    subroutine test_healpix_matches_grid3d(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: s3, sh
        integer(int64), allocatable :: o3(:), oh(:)
        real(real64), allocatable :: d3(:), dh(:)
        integer(int64) :: n, k, m3, mh, pix3, pixh, hits
        real(real64) :: qra, qdec, rq
        integer :: t
        real(real64), parameter :: radii(4) = &
            [0.05_real64, 0.5_real64, 2.0_real64, 8.0_real64]

        n = 4000_int64
        call make_sky(n, ra, dec)
        allocate (o3(n), oh(n), d3(n), dh(n))
        do t = 1, size(radii)
            rq = radii(t)
            call build_both(ra, dec, rq, s3, sh, error)
            if (allocated(error)) return
            hits = 0_int64
            do k = 1_int64, 200_int64
                ! Query points drawn from the data itself as well as from open sky, so the sweep
                ! covers the polar caps and the 0h seam that `make_sky` deliberately populates.
                if (mod(k, 2_int64) == 0_int64) then
                    qra = ra(k)
                    qdec = dec(k)
                else
                    qra = 360.0_real64 * pf_random_at(fixture_seed + 71_int64, k, 1_int64)
                    qdec = -90.0_real64 + 180.0_real64 * pf_random_at(fixture_seed + 71_int64, k, 2_int64)
                end if
                m3 = s3%within_sky(qra, qdec, rq, o3, d3, sorted=.true.)
                mh = sh%within_sky(qra, qdec, rq, oh, dh, sorted=.true.)
                hits = hits + m3
                call check(error, mh, m3, "the two backends returned different neighbour counts")
                if (allocated(error)) return
                if (m3 > 0_int64) then
                    call check(error, all(oh(1:m3) == o3(1:m3)), &
                               "the two backends returned different rows, or the same rows in a different order")
                    if (allocated(error)) return
                    call check(error, all(dh(1:m3) == d3(1:m3)), &
                               "the two backends returned different distances for the same neighbours")
                    if (allocated(error)) return
                end if
            end do
            call check(error, hits > 0_int64, &
                       "the fixture returned no neighbours at all, so nothing above was compared")
            if (allocated(error)) return
        end do

        ! ---- The negative control ----
        !
        ! Every assertion above holds just as well if `spatial_scan`'s HEALPix branch were deleted
        ! and both arms ran the 3D walk. These two say the arms really are different code.
        rq = 1.0_real64
        call build_both(ra, dec, rq, s3, sh, error)
        if (allocated(error)) return
        call parquet_debug_reset_spatial_counters()
        m3 = s3%within_sky(ra(1), dec(1), rq, o3)
        pix3 = parquet_debug_spatial_pixels_visited()
        call parquet_debug_reset_spatial_counters()
        mh = sh%within_sky(ra(1), dec(1), rq, oh)
        pixh = parquet_debug_spatial_pixels_visited()
        call check(error, pix3, 0_int64, &
                   "negative control: a 3D-grid query must walk no HEALPix pixels at all")
        if (allocated(error)) return
        call check(error, pixh > 0_int64, &
                   "negative control: a HEALPix query must walk pixels, or its branch never ran")
    end subroutine test_healpix_matches_grid3d

    !> The three bulk sky sweeps agree element for element across the backends.
    subroutine test_healpix_bulk_matches_grid3d(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: s3, sh
        integer(int64), allocatable :: f3(:), n3(:), fh(:), nh(:)
        integer(int64), allocatable :: i3(:), j3(:), ih(:), jh(:), c3(:), ch(:)
        integer(int64) :: n, i, lo, hi
        real(real64), parameter :: rq = 1.5_real64

        n = 900_int64
        call make_sky(n, ra, dec)
        call build_both(ra, dec, rq, s3, sh, error)
        if (allocated(error)) return

        call s3%all_within_sky(rq, f3, n3)
        call sh%all_within_sky(rq, fh, nh)
        call check(error, all(fh == f3), "the CSR offsets differ between the backends")
        if (allocated(error)) return
        ! Row by row rather than whole-array, because a CSR row's ORDER is the walk's order and
        ! only the sorted contract pins it -- so the rows are compared as sets, which is what
        ! `%all_within_sky` actually promises.
        do i = 1_int64, n
            lo = f3(i)
            hi = f3(i + 1_int64) - 1_int64
            call check(error, same_rows(n3(lo:hi), hi - lo + 1_int64, nh(lo:hi), hi - lo + 1_int64), &
                       "a CSR neighbour row differs between the backends")
            if (allocated(error)) return
        end do

        call s3%count_all_within_sky(rq, c3)
        call sh%count_all_within_sky(rq, ch)
        call check(error, all(ch == c3), "count_all_within_sky differs between the backends")
        if (allocated(error)) return

        ! **The pair list is compared as a SET, deliberately.** `%pairs_within_sky` promises every
        ! close pair exactly once with `i < j` and promises nothing about the order they arrive
        ! in -- that order is the walk's, so it is bucket order on one backend and pixel order on
        ! the other, and they genuinely differ (measured: the same 20140 pairs, none of them at
        ! the same position). Asserting element-for-element here would be asserting an order the
        ! library does not offer.
        !
        ! The COUNT is the sharp half and is asserted first: the `min_key` ranking is what makes
        ! each pair appear exactly once, it is shared by both walks, and a HEALPix walk that had
        ! dropped it would emit every pair twice -- which the count catches immediately, where a
        ! set comparison alone would not.
        call s3%pairs_within_sky(rq, i3, j3)
        call sh%pairs_within_sky(rq, ih, jh)
        call check(error, size(ih, kind=int64), size(i3, kind=int64), &
                   "the two backends found a different number of sky pairs")
        if (allocated(error)) return
        call check(error, all(ih < jh), "every sky pair from the HEALPix backend must have i < j")
        if (allocated(error)) return
        call check(error, same_pair_set(i3, j3, ih, jh, n), &
                   "the two backends emitted different sky pairs")
    end subroutine test_healpix_bulk_matches_grid3d

    !> The expanding-shell searches agree across the backends.
    !>
    !> `%nearest_sky` and `%kth_distance_sky` reach `spatial_scan` through `spatial_shell_search`,
    !> which widens the radius until it has enough neighbours -- so this is also the only test
    !> here that drives the HEALPix walk at radii far larger than the index was tuned for, which
    !> is where a disc outgrows the walk's stack run buffer and takes its allocating fallback.
    subroutine test_healpix_nearest_matches_grid3d(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), k3(:), kh(:)
        type(pf_spatial_index) :: s3, sh
        integer(int64), allocatable :: o3(:), oh(:)
        real(real64), allocatable :: d3(:), dh(:)
        integer(int64) :: n, t, m3, mh
        real(real64) :: qra, qdec
        real(real64), parameter :: rq = 1.0_real64

        n = 1500_int64
        call make_sky(n, ra, dec)
        call build_both(ra, dec, rq, s3, sh, error)
        if (allocated(error)) return
        allocate (o3(32), oh(32), d3(32), dh(32))
        do t = 1_int64, 60_int64
            qra = 360.0_real64 * pf_random_at(fixture_seed + 72_int64, t, 1_int64)
            qdec = -90.0_real64 + 180.0_real64 * pf_random_at(fixture_seed + 72_int64, t, 2_int64)
            m3 = s3%nearest_sky(qra, qdec, 9_int32, o3, dist_deg=d3)
            mh = sh%nearest_sky(qra, qdec, 9_int32, oh, dist_deg=dh)
            call check(error, mh, m3, "the two backends found a different number of nearest neighbours")
            if (allocated(error)) return
            call check(error, all(oh(1:m3) == o3(1:m3)), &
                       "nearest_sky returned different rows on the two backends")
            if (allocated(error)) return
            call check(error, all(dh(1:m3) == d3(1:m3)), &
                       "nearest_sky returned different distances on the two backends")
            if (allocated(error)) return
        end do
        ! A pole query, where the ring arithmetic is most likely to be wrong, and one on the seam.
        m3 = s3%nearest_sky(0.0_real64, 90.0_real64, 9_int32, o3, dist_deg=d3)
        mh = sh%nearest_sky(0.0_real64, 90.0_real64, 9_int32, oh, dist_deg=dh)
        call check(error, all(oh(1:m3) == o3(1:m3)) .and. mh == m3, &
                   "a nearest_sky query centred on the north pole differed between the backends")
        if (allocated(error)) return
        m3 = s3%nearest_sky(360.0_real64, 0.0_real64, 5_int32, o3, dist_deg=d3)
        mh = sh%nearest_sky(360.0_real64, 0.0_real64, 5_int32, oh, dist_deg=dh)
        call check(error, all(oh(1:m3) == o3(1:m3)) .and. mh == m3, &
                   "a nearest_sky query on the 0h seam differed between the backends")
        if (allocated(error)) return

        call s3%kth_distance_sky(3_int32, k3)
        call sh%kth_distance_sky(3_int32, kh)
        call check(error, all(kh == k3), "kth_distance_sky differs between the backends")
    end subroutine test_healpix_nearest_matches_grid3d

    !> A HEALPix index describes itself, and reports the 3D grid's own fields as absent.
    subroutine test_healpix_metadata(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: s3, sh
        integer(int64) :: nx, ny, nz
        real(real64) :: sides(3)

        call make_sky(2000_int64, ra, dec)
        call build_both(ra, dec, 1.25_real64, s3, sh, error)
        if (allocated(error)) return
        call check(error, sh%metric() == PF_METRIC_SKY, "a HEALPix index is still a sky index")
        if (allocated(error)) return
        call check(error, sh%ndim() == 3, "a HEALPix index is still three unit-vector coordinates")
        if (allocated(error)) return
        call check(error, .not. sh%is_periodic(), "a HEALPix index has no periodic box")
        if (allocated(error)) return
        call check(error, sh%size() == s3%size(), "both backends hold the same points")
        if (allocated(error)) return
        ! `%effective_radius` is backend-independent: it reports the radius the index was given,
        ! converted back to degrees, and neither backend changes what that radius was.
        call check(error, abs(sh%effective_radius() - 1.25_real64) < 1.0e-9_real64, &
                   "%effective_radius must report degrees on a HEALPix index too")
        if (allocated(error)) return
        ! A pixel IS a bucket, so the bucket count is the pixel count.
        call check(error, sh%cells(), sh%npix(), "%cells must report the pixel count on a HEALPix index")
        if (allocated(error)) return
        ! The 3D grid's own description is reported as a value no valid grid ever has, rather than
        ! as a stale number describing a grid this index does not have -- `%cell_size`'s documented
        ! contract is that it can be passed back as `cell=`, and a pixel resolution cannot.
        call check(error, sh%cell_size(), 0.0_real64, &
                   "%cell_size must report zero on a HEALPix index, not a pixel resolution", thr=0.0_real64)
        if (allocated(error)) return
        call sh%cell_sides(sides(1), sides(2), sides(3))
        call check(error, all(sides == 0.0_real64), "%cell_sides must report zero on a HEALPix index")
        if (allocated(error)) return
        call sh%grid(nx, ny, nz)
        call check(error, nx == 0_int64 .and. ny == 0_int64 .and. nz == 0_int64, &
                   "%grid must report zero cells per axis on a HEALPix index")
        if (allocated(error)) return
        ! And the 3D index answers zero for the HEALPix fields, rather than aborting, so that
        ! reporting code can print an index's shape without first asking what backs it.
        call check(error, s3%nside(), 0_int64, "a 3D-grid index has no nside")
        if (allocated(error)) return
        call check(error, s3%npix(), 0_int64, "a 3D-grid index has no pixel count")
    end subroutine test_healpix_metadata

    !> An explicit `nside=` is honoured, and the debug override stops the probe.
    subroutine test_healpix_nside_forced(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: sx, sy
        integer(int64), allocatable :: o1(:), o2(:)
        integer(int64) :: n, m1, m2, probed
        real(real64), parameter :: rq = 1.0_real64

        n = 6000_int64
        call make_sky(n, ra, dec)
        ! **Both resolutions must sit inside the buckets-per-point cap**, or the clamp -- not the
        ! argument -- decides the answer and the test asserts the clamp instead. The cap here is
        ! `0.3 * 6000 = 1800` pixels, so nside 4 (192) and nside 8 (768) both fit and nside 16
        ! (3072) would not.
        call sx%build_sky(ra, dec, radius_deg=rq, backend=PF_SKY_HEALPIX, nside=4_int64)
        call check(error, sx%nside(), 4_int64, "nside= was not honoured")
        if (allocated(error)) return
        call check(error, parquet_debug_spatial_probe_count(), 0_int64, &
                   "an explicit nside= must stop the resolution probe, as an explicit cell= stops the cell probe")
        if (allocated(error)) return
        call sy%build_sky(ra, dec, radius_deg=rq, backend=PF_SKY_HEALPIX, nside=8_int64)
        call check(error, sy%nside(), 8_int64, "the second nside= was not honoured")
        if (allocated(error)) return
        ! **Every resolution must give the same answers.** That is what makes the resolution a
        ! tuning parameter rather than part of the result, and it is the property a wrong disc
        ! radius or a wrong bucket-range calculation would break.
        allocate (o1(n), o2(n))
        m1 = sx%within_sky(ra(3), dec(3), rq, o1)
        m2 = sy%within_sky(ra(3), dec(3), rq, o2)
        call check(error, m2, m1, "two HEALPix resolutions returned different neighbour counts")
        if (allocated(error)) return
        call check(error, same_rows(o1, m1, o2, m2), "two HEALPix resolutions returned different rows")
        if (allocated(error)) return

        ! The probe runs when nothing forces it, which is the negative control for the two
        ! assertions above -- a probe count of zero would otherwise mean nothing.
        call sx%clear()
        call sx%build_sky(ra, dec, radius_deg=rq, backend=PF_SKY_HEALPIX)
        probed = parquet_debug_spatial_probe_count()
        call check(error, probed > 1_int64, &
                   "negative control: with no nside= the resolution probe must evaluate several candidates")
        if (allocated(error)) return

        ! The debug override forces the resolution exactly as `nside=` does, which is what lets a
        ! test-sized fixture pin a resolution the probe would never pick.
        call parquet_debug_set_spatial_nside(2_int64)
        call sy%clear()
        call sy%build_sky(ra, dec, radius_deg=rq, backend=PF_SKY_HEALPIX)
        call parquet_debug_set_spatial_nside(0_int64)
        call check(error, sy%nside(), 2_int64, "parquet_debug_set_spatial_nside did not force the resolution")
        if (allocated(error)) return
        call check(error, parquet_debug_spatial_probe_count(), 0_int64, &
                   "the debug override must stop the probe exactly as nside= does")
        if (allocated(error)) return
        call sy%clear()
        call sy%build_sky(ra, dec, radius_deg=rq, backend=PF_SKY_HEALPIX)
        call check(error, parquet_debug_spatial_probe_count() > 1_int64, &
                   "negative control: clearing the override must return the resolution to the probe")
    end subroutine test_healpix_nside_forced

    !> `%rebuild_for` re-pixelates a HEALPix index and leaves it a HEALPix index.
    subroutine test_healpix_rebuild_for(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: sx, s3
        integer(int64), allocatable :: oh(:), o3(:)
        integer(int64) :: n, fine, coarse, mh, m3

        n = 20000_int64
        call make_sky(n, ra, dec)
        call sx%build_sky(ra, dec, radius_deg=0.05_real64, backend=PF_SKY_HEALPIX)
        fine = sx%nside()
        ! **DEGREES, exactly as `%build_sky`'s radius_deg= is.** `%rebuild_for` converts to the
        ! chord the index tunes on itself; see `test_sky_rebuild_for_takes_degrees`, which is what
        ! pins that and would fail if the conversion were removed.
        call sx%rebuild_for(20.0_real64)
        coarse = sx%nside()
        call check(error, sx%backend() == PF_SKY_HEALPIX, "%rebuild_for must not change the backend")
        if (allocated(error)) return
        call check(error, sx%npix(), 12_int64 * coarse ** 2, "%rebuild_for left nside and npix disagreeing")
        if (allocated(error)) return
        call check(error, coarse < fine, &
                   "re-tuning for a far larger radius must choose a coarser pixelisation, or nothing was re-tuned")
        if (allocated(error)) return
        ! And it still answers correctly afterwards, which is what a re-bucketing can break.
        allocate (oh(n), o3(n))
        call s3%build_sky(ra, dec, radius_deg=20.0_real64)
        mh = sx%within_sky(ra(7), dec(7), 20.0_real64, oh)
        m3 = s3%within_sky(ra(7), dec(7), 20.0_real64, o3)
        call check(error, mh, m3, "a re-pixelated HEALPix index disagreed with the 3D grid")
        if (allocated(error)) return
        call check(error, same_rows(oh, mh, o3, m3), "a re-pixelated HEALPix index returned different rows")
    end subroutine test_healpix_rebuild_for

    !> The walk's allocating fallback, reached by narrowing its run buffer to one column.
    !>
    !> **Unreachable without the override, which is the whole reason the override exists.** A disc
    !> arrives as six or seven runs at a tuned resolution, so overflowing the real 512-column
    !> buffer needs a disc spanning more than 256 rings -- and so a resolution the
    !> buckets-per-point cap grants only to a catalogue of about a million points. This narrows
    !> the buffer instead, which exercises the same branch on the same arithmetic at a fixture
    !> size a test can hold.
    subroutine test_healpix_run_buffer_overflow(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: s3, sh
        integer(int64), allocatable :: owide(:), onarrow(:), o3(:)
        real(real64), allocatable :: dwide(:), dnarrow(:)
        integer(int64) :: n, k, mwide, mnarrow, m3
        real(real64), parameter :: rq = 3.0_real64

        n = 3000_int64
        call make_sky(n, ra, dec)
        call build_both(ra, dec, rq, s3, sh, error)
        if (allocated(error)) return
        allocate (owide(n), onarrow(n), o3(n), dwide(n), dnarrow(n))
        do k = 1_int64, 40_int64
            ! The full buffer first, then one column, then the 3D grid: the narrowed arm must
            ! agree with BOTH, which is what tells a broken fallback from a broken walk.
            call parquet_debug_set_spatial_run_buffer(0_int64)
            mwide = sh%within_sky(ra(k), dec(k), rq, owide, dwide, sorted=.true.)
            call parquet_debug_set_spatial_run_buffer(1_int64)
            mnarrow = sh%within_sky(ra(k), dec(k), rq, onarrow, dnarrow, sorted=.true.)
            call parquet_debug_set_spatial_run_buffer(0_int64)
            m3 = s3%within_sky(ra(k), dec(k), rq, o3, sorted=.true.)
            call check(error, mnarrow, mwide, &
                       "narrowing the run buffer changed the neighbour count, so the fallback is wrong")
            if (allocated(error)) return
            call check(error, m3, mwide, "the 3D grid disagreed with the HEALPix walk on this fixture")
            if (allocated(error)) return
            call check(error, all(onarrow(1:mwide) == owide(1:mwide)), &
                       "the allocating fallback returned different rows than the stack buffer did")
            if (allocated(error)) return
            call check(error, all(dnarrow(1:mwide) == dwide(1:mwide)), &
                       "the allocating fallback returned different distances than the stack buffer did")
            if (allocated(error)) return
        end do
        ! **The negative control.** Every assertion above holds if the override did nothing at all
        ! and both arms took the stack path -- so this shows the narrowed arm really was short,
        ! by finding a query whose disc needs more than one run.
        call parquet_debug_set_spatial_run_buffer(1_int64)
        mnarrow = 0_int64
        do k = 1_int64, 40_int64
            m3 = sh%within_sky(ra(k), dec(k), rq, onarrow)
            if (m3 > 0_int64) mnarrow = mnarrow + 1_int64
        end do
        call parquet_debug_reset_spatial_counters()
        call check(error, mnarrow > 0_int64, &
                   "negative control: the narrowed sweep found nothing, so nothing above was compared")
    end subroutine test_healpix_run_buffer_overflow

    !> Exact ties: many points at identical positions must order identically on both backends.
    !>
    !> The `sorted=` contract breaks ties by ASCENDING ROW INDEX, and that tie-break is what a
    !> duplicated fixture exercises -- with every distance equal, the row order is the whole
    !> result. It is applied after the walk on both backends, so the two must agree exactly; if
    !> either walk fed it rows in an order that leaked into the answer, this is what would show it.
    subroutine test_healpix_duplicate_positions(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: s3, sh
        integer(int64), allocatable :: o3(:), oh(:)
        real(real64), allocatable :: d3(:), dh(:)
        integer(int64) :: n, i, m3, mh
        real(real64), parameter :: rq = 2.0_real64

        n = 600_int64
        allocate (ra(n), dec(n), o3(n), oh(n), d3(n), dh(n))
        do i = 1_int64, n
            ! Twelve distinct positions, fifty rows each: every neighbour list is a block of exact
            ! ties, including one exactly on a pole and one exactly on the 0h seam.
            select case (int(mod(i - 1_int64, 12_int64)))
            case (0)
                ra(i) = 0.0_real64
                dec(i) = 90.0_real64
            case (1)
                ra(i) = 0.0_real64
                dec(i) = -90.0_real64
            case (2)
                ra(i) = 0.0_real64
                dec(i) = 0.0_real64
            case default
                ra(i) = 30.0_real64 * real(mod(i - 1_int64, 12_int64), real64)
                dec(i) = -75.0_real64 + 15.0_real64 * real(mod(i - 1_int64, 12_int64), real64)
            end select
        end do
        call build_both(ra, dec, rq, s3, sh, error)
        if (allocated(error)) return
        do i = 1_int64, 12_int64
            m3 = s3%within_sky(ra(i), dec(i), rq, o3, d3, sorted=.true.)
            mh = sh%within_sky(ra(i), dec(i), rq, oh, dh, sorted=.true.)
            call check(error, mh, m3, "a duplicated-position query returned different counts")
            if (allocated(error)) return
            call check(error, m3 >= 50_int64, "the duplicated fixture returned fewer rows than one block")
            if (allocated(error)) return
            call check(error, all(oh(1:m3) == o3(1:m3)), &
                       "the tie-break ordered exact ties differently on the two backends")
            if (allocated(error)) return
        end do
    end subroutine test_healpix_duplicate_positions

    !> `%rebuild_for` takes DEGREES on a sky index, exactly as `%build_sky`'s `radius_deg=` does.
    !>
    !> **This test was inverted rather than written from scratch, and that is worth knowing.** Until
    !> 2026-09-02 `%rebuild_for` folded its argument in unconverted, so a sky index re-tuned on a
    !> CHORD -- and `call sky%rebuild_for(20.0_real64)` meaning 20 degrees silently re-tuned for 180.
    !> The review that found it proposed a test pinning the chord contract and said in as many words
    !> that if the maintainer chose to convert instead, "this test inverts rather than disappearing".
    !> It did: the degree arm is now the one that must land on 20, and the chord arm the one that
    !> must not. If `%rebuild_for` is ever changed again, this is the test that has to be rewritten.
    !>
    !> **The observable is `%effective_radius()`**, which reports in degrees on a sky index and is
    !> dominated by the far larger re-tune radius once folded, so it reads back what was asked for.
    !> The chord arm is the negative control: 2*sin(10 deg) = 0.347, which as DEGREES is 0.347 -- so
    !> an implementation that had kept the old behaviour would answer 20 there and fail.
    subroutine test_sky_rebuild_for_takes_degrees(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: deg_arm, chord_arm
        integer(int64) :: n
        real(real64) :: chord20
        real(real64), parameter :: d2r = 3.141592653589793238462643_real64 / 180.0_real64

        n = 2000_int64
        call make_sky(n, ra, dec)
        chord20 = 2.0_real64 * sin(0.5_real64 * 20.0_real64 * d2r)
        !
        call deg_arm%build_sky(ra, dec, radius_deg=0.05_real64)
        call check(error, abs(deg_arm%effective_radius() - 0.05_real64) < 1.0e-9_real64, &
            "a freshly built sky index must report the radius_deg it was given")
        if (allocated(error)) return
        call deg_arm%rebuild_for(20.0_real64)
        call check(error, abs(deg_arm%effective_radius() - 20.0_real64) < 1.0e-6_real64, &
            "%rebuild_for(20.0) on a sky index must re-tune for 20 DEGREES, so %effective_radius() reads 20")
        if (allocated(error)) return
        !
        ! Negative control: the old chord spelling must no longer mean 20 degrees. It is now read as
        ! 0.347 degrees, which is what makes the two arms distinguishable at all -- without this the
        ! assertion above would pass just as happily against the pre-fix behaviour.
        call chord_arm%build_sky(ra, dec, radius_deg=0.05_real64)
        call chord_arm%rebuild_for(chord20)
        call check(error, abs(chord_arm%effective_radius() - chord20) < 1.0e-6_real64, &
            "negative control: a chord passed to %rebuild_for is now taken as that many DEGREES")
        if (allocated(error)) return
        call check(error, chord_arm%effective_radius() < 1.0_real64, &
            "negative control: the chord arm must NOT come back at 20 degrees, or the conversion was skipped")
        if (allocated(error)) return
        !
        ! And the index still answers correctly after the re-tune, which a re-bucketing can break.
        call check(error, deg_arm%is_built() .and. deg_arm%metric() == PF_METRIC_SKY, &
            "a re-tuned sky index must still be a built sky index")
    end subroutine test_sky_rebuild_for_takes_degrees

    !> `%count_within_sky` answers exactly what `%within_sky` reports as its true count.
    !>
    !> The oracle is `%within_sky` itself rather than a brute-force scan, deliberately: the two
    !> share `sky_scan`, so what this pins is not the geometry -- `test_sky_matches_haversine`
    !> already does that -- but that the buffer-free form reaches the same walk with the same
    !> guards, including the annulus. A brute-force oracle here would re-test the geometry and
    !> leave the actual risk, a second code path drifting, untouched.
    subroutine test_count_within_sky(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:)
        type(pf_spatial_index) :: sky
        integer(int64), allocatable :: found(:)
        integer(int64) :: n, k, m_ref, m_cnt
        real(real64) :: rr
        integer :: t

        n = 3000_int64
        call make_sky(n, ra, dec)
        call sky%build_sky(ra, dec, radius_deg=1.0_real64)
        allocate (found(n))
        do t = 1, 4
            rr = 0.25_real64 * real(t, kind=real64)
            do k = 1_int64, 5_int64
                m_ref = sky%within_sky(ra(k), dec(k), rr, found)
                m_cnt = sky%count_within_sky(ra(k), dec(k), rr)
                call check(error, m_cnt, m_ref, "%count_within_sky must equal %within_sky's true count")
                if (allocated(error)) return
            end do
        end do
        ! The annulus reaches the buffer-free form too.
        m_ref = sky%within_sky(ra(3), dec(3), 1.0_real64, found, r_inner_deg=0.4_real64)
        m_cnt = sky%count_within_sky(ra(3), dec(3), 1.0_real64, r_inner_deg=0.4_real64)
        call check(error, m_cnt, m_ref, "%count_within_sky must honour r_inner_deg= as %within_sky does")
        if (allocated(error)) return
        ! Both extremes, so the agreement above is not an accident of one populated radius.
        !
        ! At r = 0 the two bindings must AGREE -- which is what this test is about -- but neither
        ! is asserted to find the query point. A sky query does not compare (ra, dec): it derives a
        ! unit vector from them by the same expression `%build_sky` used on the catalogue, and two
        ! evaluations of a transcendental expression are not required to agree to the last bit.
        ! Measured on ifx 2026.1, that expression in a bulk loop and as a scalar differ by 1-2 ulp
        ! under the -O0 debug profile -- putting this point 1.3e-14 degrees from itself, so a
        ! zero-radius ball round it is empty -- while the same source at -O2, and gfortran at
        ! either, give exactly 0. Asserting self-discovery at r = 0 therefore tests the compiler's
        ! libm, not this library, and passes or fails with the optimisation level.
        m_ref = sky%within_sky(ra(1), dec(1), 0.0_real64, found)
        m_cnt = sky%count_within_sky(ra(1), dec(1), 0.0_real64)
        call check(error, m_cnt, m_ref, "%count_within_sky must equal %within_sky at a zero radius")
        if (allocated(error)) return
        ! Self-discovery is asserted just above that round-trip error instead. 1e-9 degrees is
        ! 3.6 microarcseconds -- below any radius an astronomer would ask for, and five orders of
        ! magnitude above the 1.3e-14 degrees the conversion can cost.
        call check(error, sky%count_within_sky(ra(1), dec(1), 1.0e-9_real64) >= 1_int64, &
            "a radius above the unit-vector round-trip error must find the point itself")
        if (allocated(error)) return
        call check(error, sky%count_within_sky(ra(1), dec(1), 90.0_real64) > 0_int64, &
            "a hemisphere-wide radius must count something rather than nothing")
    end subroutine test_count_within_sky

    ! ---- Line-of-sight fixtures and the cylinder oracle ----

    !> The Einstein-de Sitter comoving distance, `2 (c/H0) (1 - 1/sqrt(1+z))`: a real distance-redshift
    !> relation with a closed form, so an analytic slope `(c/H0) (1+z)**(-3/2)` exists to compare
    !> the measured one against.
    pure elemental real(real64) function los_comoving(z) result(d)
        real(real64), intent(in) :: z !! the redshift.

        d = 2.0_real64 * los_ch0 * (1.0_real64 - 1.0_real64 / sqrt(1.0_real64 + z))
    end function los_comoving

    !> A deterministic survey wedge about an observer at the origin: `nrand` scattered points inside
    !> a footprint about ten degrees across at distances in `[d_lo, d_hi]`, plus `nspoke` radial
    !> spokes of four points each, 15 apart in distance along one line of sight, so that pairs along
    !> a line of sight exist at every depth whatever the random draw did. Returns the generating
    !> angles and distances beside the coordinates, so an oracle can work from them and never from
    !> the Cartesian coordinates the library sees.
    subroutine make_wedge(nrand, nspoke, d_lo, d_hi, stream, ra, dec, d, x, y, z)
        integer(int64), intent(in) :: nrand !! scattered points.
        integer(int64), intent(in) :: nspoke !! spokes; each adds four points along one line of sight.
        real(real64), intent(in) :: d_lo !! the nearest distance.
        real(real64), intent(in) :: d_hi !! the farthest distance.
        integer(int64), intent(in) :: stream !! a draw offset, so two fixtures differ.
        real(real64), allocatable, intent(out) :: ra(:) !! right ascension, radians.
        real(real64), allocatable, intent(out) :: dec(:) !! declination, radians.
        real(real64), allocatable, intent(out) :: d(:) !! distance from the origin.
        real(real64), allocatable, intent(out) :: x(:) !! x of every point.
        real(real64), allocatable, intent(out) :: y(:) !! y of every point.
        real(real64), allocatable, intent(out) :: z(:) !! z of every point.
        integer(int64) :: n, i, s, k
        real(real64), parameter :: half = 0.087_real64

        n = nrand + 4_int64 * nspoke
        allocate (ra(n), dec(n), d(n), x(n), y(n), z(n))
        do i = 1_int64, nrand
            ra(i) = half * (2.0_real64 * pf_random_at(fixture_seed + stream, i, 1_int64) - 1.0_real64)
            dec(i) = half * (2.0_real64 * pf_random_at(fixture_seed + stream, i, 2_int64) - 1.0_real64)
            d(i) = d_lo + (d_hi - d_lo) * pf_random_at(fixture_seed + stream, i, 3_int64)
        end do
        do s = 1_int64, nspoke
            do k = 0_int64, 3_int64
                i = nrand + 4_int64 * (s - 1_int64) + k + 1_int64
                ra(i) = half * (2.0_real64 * pf_random_at(fixture_seed + stream, s, 4_int64) - 1.0_real64)
                dec(i) = half * (2.0_real64 * pf_random_at(fixture_seed + stream, s, 5_int64) - 1.0_real64)
                d(i) = d_lo + (d_hi - d_lo - 45.0_real64) * pf_random_at(fixture_seed + stream, s, 6_int64) &
                    + 15.0_real64 * real(k, kind=real64)
            end do
        end do
        do i = 1_int64, n
            x(i) = d(i) * cos(dec(i)) * cos(ra(i))
            y(i) = d(i) * cos(dec(i)) * sin(ra(i))
            z(i) = d(i) * sin(dec(i))
        end do
    end subroutine make_wedge

    !> A deterministic redshift survey, the worked example of the design in miniature: `nrand`
    !> galaxies with redshifts in `[z_lo, z_hi]` and `nspoke` spokes of four galaxies 0.002 apart in
    !> redshift along one line of sight, placed at their Einstein-de Sitter comoving distance.
    subroutine make_redshift_survey(nrand, nspoke, z_lo, z_hi, stream, ra, dec, zred, d, x, y, z)
        integer(int64), intent(in) :: nrand !! scattered galaxies.
        integer(int64), intent(in) :: nspoke !! spokes of four galaxies each.
        real(real64), intent(in) :: z_lo !! the lowest redshift.
        real(real64), intent(in) :: z_hi !! the highest redshift.
        integer(int64), intent(in) :: stream !! a draw offset, so two fixtures differ.
        real(real64), allocatable, intent(out) :: ra(:) !! right ascension, radians.
        real(real64), allocatable, intent(out) :: dec(:) !! declination, radians.
        real(real64), allocatable, intent(out) :: zred(:) !! the redshift: the parallel coordinate.
        real(real64), allocatable, intent(out) :: d(:) !! comoving distance, Mpc/h.
        real(real64), allocatable, intent(out) :: x(:) !! x of every galaxy, Mpc/h.
        real(real64), allocatable, intent(out) :: y(:) !! y of every galaxy.
        real(real64), allocatable, intent(out) :: z(:) !! z of every galaxy.
        integer(int64) :: n, i, s, k
        real(real64), parameter :: half = 0.087_real64

        n = nrand + 4_int64 * nspoke
        allocate (ra(n), dec(n), zred(n), d(n), x(n), y(n), z(n))
        do i = 1_int64, nrand
            ra(i) = half * (2.0_real64 * pf_random_at(fixture_seed + stream, i, 1_int64) - 1.0_real64)
            dec(i) = half * (2.0_real64 * pf_random_at(fixture_seed + stream, i, 2_int64) - 1.0_real64)
            zred(i) = z_lo + (z_hi - z_lo) * pf_random_at(fixture_seed + stream, i, 3_int64)
        end do
        do s = 1_int64, nspoke
            do k = 0_int64, 3_int64
                i = nrand + 4_int64 * (s - 1_int64) + k + 1_int64
                ra(i) = half * (2.0_real64 * pf_random_at(fixture_seed + stream, s, 4_int64) - 1.0_real64)
                dec(i) = half * (2.0_real64 * pf_random_at(fixture_seed + stream, s, 5_int64) - 1.0_real64)
                zred(i) = z_lo + (z_hi - z_lo - 0.006_real64) * pf_random_at(fixture_seed + stream, s, 6_int64) &
                    + 0.002_real64 * real(k, kind=real64)
            end do
        end do
        do i = 1_int64, n
            d(i) = los_comoving(zred(i))
            x(i) = d(i) * cos(dec(i)) * cos(ra(i))
            y(i) = d(i) * cos(dec(i)) * sin(ra(i))
            z(i) = d(i) * sin(dec(i))
        end do
    end subroutine make_redshift_survey

    !> The transverse separation of two points about the origin, from the angles and distances the
    !> fixture was generated with: `2 sin(dtheta/2) (D_1 + D_2)/2`, the half-angle sine by the
    !> haversine, so it cancels nothing and shares nothing with the Cartesian coordinates.
    real(real64) function los_dperp(ra1, dec1, d1, ra2, dec2, d2) result(dp)
        real(real64), intent(in) :: ra1 !! first point's right ascension, radians.
        real(real64), intent(in) :: dec1 !! first point's declination, radians.
        real(real64), intent(in) :: d1 !! first point's distance from the origin.
        real(real64), intent(in) :: ra2 !! second point's right ascension.
        real(real64), intent(in) :: dec2 !! second point's declination.
        real(real64), intent(in) :: d2 !! second point's distance.
        real(real64) :: hav

        hav = sin(0.5_real64 * (dec2 - dec1))**2 + cos(dec1) * cos(dec2) * sin(0.5_real64 * (ra2 - ra1))**2
        dp = sqrt(hav) * (d1 + d2)
    end function los_dperp

    !> Whether separations `dp`, `dl` qualify under `rule` for lengths `(bpa, bla)` and `(bpb, blb)`,
    !> written from each rule's plain-language definition: rule 0 is the first point's own cylinder,
    !> and `PF_LINK_MAX` is the UNION of the two cylinders, never the componentwise maximum.
    logical function los_qualifies(rule, dp, dl, bpa, bla, bpb, blb) result(ok)
        integer, intent(in) :: rule !! 0, or one of the four `PF_LINK_*` constants.
        real(real64), intent(in) :: dp !! the transverse separation.
        real(real64), intent(in) :: dl !! the parallel separation.
        real(real64), intent(in) :: bpa !! the first point's transverse length.
        real(real64), intent(in) :: bla !! the first point's parallel length.
        real(real64), intent(in) :: bpb !! the second point's transverse length.
        real(real64), intent(in) :: blb !! the second point's parallel length.

        select case (rule)
        case (0)
            ok = dp <= bpa .and. dl <= bla
        case (PF_LINK_MAX)
            ok = (dp <= bpa .and. dl <= bla) .or. (dp <= bpb .and. dl <= blb)
        case (PF_LINK_MIN)
            ok = dp <= min(bpa, bpb) .and. dl <= min(bla, blb)
        case (PF_LINK_MEAN)
            ok = dp <= 0.5_real64 * (bpa + bpb) .and. dl <= 0.5_real64 * (bla + blb)
        case default
            ok = dp <= bpa + bpb .and. dl <= bla + blb
        end select
    end function los_qualifies

    !> Every pair the cylinder criterion accepts under `rule`, by scanning every pair from the
    !> fixture's own angles, distances and parallel coordinate. `want(a, b)` for `a < b`.
    subroutine brute_los_pairs(ra, dec, d, los, bp, bl, rule, want, nwant)
        real(real64), intent(in) :: ra(:) !! right ascension per point, radians.
        real(real64), intent(in) :: dec(:) !! declination per point, radians.
        real(real64), intent(in) :: d(:) !! distance from the observer per point.
        real(real64), intent(in) :: los(:) !! the parallel coordinate per point.
        real(real64), intent(in) :: bp(:) !! transverse length per point.
        real(real64), intent(in) :: bl(:) !! parallel length per point.
        integer, intent(in) :: rule !! one of the four `PF_LINK_*` constants.
        logical, intent(out) :: want(:, :) !! the accepted set, upper triangle.
        integer(int64), intent(out) :: nwant !! how many pairs were accepted.
        integer(int64) :: a, b, n
        real(real64) :: dp, dl

        n = size(ra, kind=int64)
        want = .false.
        nwant = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                dp = los_dperp(ra(a), dec(a), d(a), ra(b), dec(b), d(b))
                dl = abs(los(a) - los(b))
                if (los_qualifies(rule, dp, dl, bp(a), bl(a), bp(b), bl(b))) then
                    want(a, b) = .true.
                    nwant = nwant + 1_int64
                end if
            end do
        end do
    end subroutine brute_los_pairs

    !> The rows inside point `a`'s OWN cylinder, `a` itself included: the directed relation
    !> `%within_los` answers, asserted straight from the fixture rather than derived from a pair list.
    subroutine brute_los_own(ra, dec, d, los, a, bp, bl, out, m)
        real(real64), intent(in) :: ra(:) !! right ascension per point, radians.
        real(real64), intent(in) :: dec(:) !! declination per point, radians.
        real(real64), intent(in) :: d(:) !! distance from the observer per point.
        real(real64), intent(in) :: los(:) !! the parallel coordinate per point.
        integer(int64), intent(in) :: a !! the searching point.
        real(real64), intent(in) :: bp !! its transverse length.
        real(real64), intent(in) :: bl !! its parallel length.
        integer(int64), allocatable, intent(out) :: out(:) !! the rows found, ascending.
        integer(int64), intent(out) :: m !! how many rows were found.
        integer(int64) :: b, n

        n = size(ra, kind=int64)
        allocate (out(n))
        m = 0_int64
        do b = 1_int64, n
            if (los_qualifies(0, los_dperp(ra(a), dec(a), d(a), ra(b), dec(b), d(b)), abs(los(a) - los(b)), &
                bp, bl, bp, bl)) then
                m = m + 1_int64
                out(m) = b
            end if
        end do
    end subroutine brute_los_own

    !> Whether the pair list `(pi, pj)` is exactly the set `want`: each pair once, `i < j`, no more.
    logical function pairs_equal_set(pi, pj, want, nwant) result(same)
        integer(int64), intent(in) :: pi(:) !! the lower rows.
        integer(int64), intent(in) :: pj(:) !! the higher rows.
        logical, intent(in) :: want(:, :) !! the expected set, upper triangle.
        integer(int64), intent(in) :: nwant !! how many pairs it holds.
        logical, allocatable :: got(:, :)
        integer(int64) :: k

        same = .false.
        if (size(pi, kind=int64) /= nwant) return
        if (size(pj, kind=int64) /= nwant) return
        if (nwant == 0_int64) then
            same = .true.
            return
        end if
        if (.not. all(pi < pj)) return
        allocate (got(size(want, 1), size(want, 2)))
        got = .false.
        do k = 1_int64, nwant
            ! A duplicate would keep the set and change only the count, and the count is already
            ! right here -- so it has to be caught pair by pair.
            if (got(pi(k), pj(k))) return
            got(pi(k), pj(k)) = .true.
        end do
        same = all(got .eqv. want)
    end function pairs_equal_set

    !> Every `combine=` rule on the line-of-sight cylinder reproduces a brute-force scan of its own
    !> definition, on a survey wedge with per-point lengths whose aspect ratios spread over 5..30.
    !>
    !> Two preconditions make the fixture load-bearing. The four rules must give four different
    !> counts, or a sweep answering one rule for all four would pass -- distinct rather than ordered,
    !> because on a cylinder the mean set is NOT inside the union. And the union must disagree with
    !> the componentwise maximum on at least one pair (`feature_risks.md`): with the ratios spread, a
    !> pair can satisfy `d_perp <= max(b_perp)` through one endpoint and `d_par <= max(b_par)`
    !> through the other while lying in neither cylinder, and a sweep taking the componentwise
    !> reading would report it.
    subroutine test_pairs_los_matches_cylinder_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, b, nwant, disagree, counted(4)
        real(real64) :: dp, dl
        integer :: rules(4), ri, rj
        logical :: cmax

        rules = [PF_LINK_MIN, PF_LINK_MEAN, PF_LINK_MAX, PF_LINK_SUM]
        call make_wedge(300_int64, 40_int64, 500.0_real64, 1500.0_real64, 1_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            bp(a) = 10.0_real64 + 10.0_real64 * pf_random_at(fixture_seed, a, 31_int64)
            bl(a) = bp(a) * (5.0_real64 + 25.0_real64 * pf_random_at(fixture_seed, a, 32_int64))
        end do
        disagree = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                dp = los_dperp(ra(a), dec(a), d(a), ra(b), dec(b), d(b))
                dl = abs(d(a) - d(b))
                cmax = dp <= max(bp(a), bp(b)) .and. dl <= max(bl(a), bl(b))
                if (cmax .neqv. los_qualifies(PF_LINK_MAX, dp, dl, bp(a), bl(a), bp(b), bl(b))) &
                    disagree = disagree + 1_int64
            end do
        end do
        call check(error, disagree > 0_int64, &
            "the fixture must hold pairs the union and the componentwise maximum judge differently")
        if (allocated(error)) return
        call sx%build(x, y, z, radius=sqrt(bp**2 + bl**2))
        do ri = 1, 4
            call brute_los_pairs(ra, dec, d, d, bp, bl, rules(ri), want, nwant)
            counted(ri) = nwant
            call check(error, nwant > 0_int64, "every rule must accept some pair on this fixture")
            if (allocated(error)) return
            call sx%pairs_within_los(bp, bl, pi, pj, combine=rules(ri))
            call check(error, pairs_equal_set(pi, pj, want, nwant), &
                "each combine= rule's cylinder pair list must be exactly its own accepted set")
            if (allocated(error)) return
        end do
        do ri = 1, 3
            do rj = ri + 1, 4
                call check(error, counted(ri) /= counted(rj), &
                    "the fixture must separate all four rules; two pair counts coincide")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_pairs_los_matches_cylinder_oracle

    !> Omitting `los=` makes the parallel coordinate the distance from the observer: an index built
    !> without it and one built with `los = D` return the same pairs as sets, both equal to the
    !> oracle, and the measured slope of the second is one.
    !>
    !> Sets rather than lists: the second index measures `L` at rounding distance from one, so its
    !> walk radii differ in the last digits and two points of nearly equal radius can swap rank,
    !> which changes only which endpoint reports a pair.
    subroutine test_pairs_los_default_los_is_distance(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:), qi(:), qj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: plain, withd
        integer(int64) :: n, a, nwant
        real(real64) :: lip, spread, gap

        call make_wedge(260_int64, 30_int64, 500.0_real64, 1500.0_real64, 2_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            bp(a) = 8.0_real64 + 8.0_real64 * pf_random_at(fixture_seed, a, 33_int64)
            bl(a) = 20.0_real64 + 30.0_real64 * pf_random_at(fixture_seed, a, 34_int64)
        end do
        call plain%build(x, y, z, radius=sqrt(bp**2 + bl**2))
        call withd%build(x, y, z, radius=sqrt(bp**2 + bl**2), los=d)
        call parquet_debug_spatial_los_bounds(plain, lip, spread, gap)
        call check(error, lip == 1.0_real64 .and. spread == 0.0_real64 .and. gap == 0.0_real64, &
            "an index without los= reports L = 1, g = 0 and no gap floor")
        if (allocated(error)) return
        call parquet_debug_spatial_los_bounds(withd, lip, spread, gap)
        call check(error, abs(lip - 1.0_real64) < 1.0e-9_real64, &
            "los = D must measure a slope of one to rounding")
        if (allocated(error)) return
        call check(error, gap > 0.0_real64 .and. spread <= gap * (1.0_real64 + 1.0e-9_real64), &
            "with los = D the spread below the gap floor cannot exceed the floor itself")
        if (allocated(error)) return
        call brute_los_pairs(ra, dec, d, d, bp, bl, PF_LINK_MEAN, want, nwant)
        call check(error, nwant > 0_int64, "the fixture must hold accepted pairs")
        if (allocated(error)) return
        call plain%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MEAN)
        call withd%pairs_within_los(bp, bl, qi, qj, combine=PF_LINK_MEAN)
        call check(error, pairs_equal_set(pi, pj, want, nwant), &
            "without los= the pair list must be the oracle's with d_par = |D_i - D_j|")
        if (allocated(error)) return
        call check(error, pairs_equal_set(qi, qj, want, nwant), &
            "with los = D the pair list must be the same set")
    end subroutine test_pairs_los_default_los_is_distance

    !> A `los=` that runs at half the distance's rate has slope two, and the walk must widen by it.
    !>
    !> `los = D/2` makes a parallel length `b_par` reach `2*b_par` in distance, so the fixture is
    !> asserted to hold accepted pairs farther apart than `sqrt(b_perp**2 + b_par**2)` -- exactly
    !> the pairs a walk that dropped `L` would lose, silently (`feature_risks.md`).
    subroutine test_pairs_los_with_slope(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:), los(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, b, nwant, beyond
        real(real64) :: lip, spread, gap, s2, bnd2

        call make_wedge(260_int64, 40_int64, 500.0_real64, 1500.0_real64, 3_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            bp(a) = 8.0_real64 + 4.0_real64 * pf_random_at(fixture_seed, a, 35_int64)
            bl(a) = 12.0_real64 + 6.0_real64 * pf_random_at(fixture_seed, a, 36_int64)
        end do
        los = 0.5_real64 * d
        call sx%build(x, y, z, radius=sqrt(bp**2 + (2.0_real64 * bl)**2), los=los)
        call parquet_debug_spatial_los_bounds(sx, lip, spread, gap)
        call check(error, abs(lip - 2.0_real64) < 1.0e-9_real64, "los = D/2 must measure a slope of two")
        if (allocated(error)) return
        call brute_los_pairs(ra, dec, d, los, bp, bl, PF_LINK_MEAN, want, nwant)
        beyond = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                if (.not. want(a, b)) cycle
                s2 = (x(a) - x(b))**2 + (y(a) - y(b))**2 + (z(a) - z(b))**2
                bnd2 = (0.5_real64 * (bp(a) + bp(b)))**2 + (0.5_real64 * (bl(a) + bl(b)))**2
                if (s2 > bnd2) beyond = beyond + 1_int64
            end do
        end do
        call check(error, beyond > 0_int64, &
            "the fixture must hold accepted pairs beyond sqrt(b_perp**2 + b_par**2), or the slope is never load-bearing")
        if (allocated(error)) return
        call sx%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MEAN)
        call check(error, pairs_equal_set(pi, pj, want, nwant), &
            "with los = D/2 the pair list must be exactly the oracle's, slope-widened walk included")
    end subroutine test_pairs_los_with_slope

    !> Pairs closer than the gap floor in `los` never enter the slope; the spread `g` carries them.
    !>
    !> `los` is quantised to steps of 50 in distance, so every pair inside one step is a TIE in
    !> `los` while its distances differ by up to 50. With `b_par` far below that, `L*b_par` misses
    !> those pairs and only `g` reaches them; the fixture is asserted to hold accepted pairs beyond
    !> `sqrt(b_perp**2 + (L*b_par)**2)`, which a walk without `g` would lose. The scalar form of the
    !> query is used, so this also covers `bind_pairs_los_r0`.
    subroutine test_pairs_los_close_los_is_complete(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:), los(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, b, nwant, beyond
        real(real64) :: lip, spread, gap, s2
        real(real64), parameter :: bperp = 40.0_real64, bpar = 10.0_real64

        call make_wedge(200_int64, 60_int64, 500.0_real64, 1500.0_real64, 4_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n), los(n))
        bp = bperp
        bl = bpar
        do a = 1_int64, n
            los(a) = 50.0_real64 * floor(d(a) / 50.0_real64)
        end do
        call sx%build(x, y, z, radius=60.0_real64, los=los)
        call parquet_debug_spatial_los_bounds(sx, lip, spread, gap)
        call check(error, gap > 0.0_real64 .and. spread > lip * bpar, &
            "the fixture's ties must make the spread, not the slope, the wider bound")
        if (allocated(error)) return
        call brute_los_pairs(ra, dec, d, los, bp, bl, PF_LINK_MAX, want, nwant)
        beyond = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                if (.not. want(a, b)) cycle
                s2 = (x(a) - x(b))**2 + (y(a) - y(b))**2 + (z(a) - z(b))**2
                if (s2 > bperp**2 + (lip * bpar)**2) beyond = beyond + 1_int64
            end do
        end do
        call check(error, beyond > 0_int64, &
            "the fixture must hold accepted pairs only the spread reaches, or g is never load-bearing")
        if (allocated(error)) return
        call sx%pairs_within_los(bperp, bpar, pi, pj)
        call check(error, pairs_equal_set(pi, pj, want, nwant), &
            "pairs tied in los must all come back; the spread below the gap floor carries them")
    end subroutine test_pairs_los_close_los_is_complete

    !> On a continuous redshift list the measured slope is the analytic one, to a tolerance the
    !> plain consecutive estimator misses by orders of magnitude.
    !>
    !> A hundred thousand redshifts drawn uniformly in `[0.02, 0.2]` with Einstein-de Sitter
    !> distances. The distance is concave in the redshift, so the steepest secant over pairs at least
    !> the gap floor apart is the one from the smallest redshift to the first at or beyond
    !> `z_min + gap`, which the test recomputes from the fixture and asserts to a millionth; the
    !> analytic derivative at `z_min`, `(c/H0) (1+z)**(-3/2)`, is asserted to a hundred-thousandth,
    !> the secant's own deviation over so short a gap. The closest two of these redshifts sit about
    !> `range / n**2` apart, where the rounding of a 500-unit distance alone shifts a consecutive
    !> secant by parts in ten thousand -- the inflation the gap floor exists to keep out. The spread
    !> below the floor cannot exceed `L*gap` by more than rounding.
    subroutine test_pairs_los_slope_is_clean(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), zred(:), d(:), x(:), y(:), z(:), zs(:)
        integer(int64), allocatable :: perm(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, b
        real(real64) :: lip, spread, gap, l_ref, l_analytic

        n = 100000_int64
        call make_redshift_survey(n, 0_int64, 0.02_real64, 0.2_real64, 5_int64, ra, dec, zred, d, x, y, z)
        call sx%build(x, y, z, radius=5.0_real64, los=zred)
        call parquet_debug_spatial_los_bounds(sx, lip, spread, gap)
        call check(error, gap > 0.0_real64, "a los= index must record its gap floor")
        if (allocated(error)) return
        call check(error, abs(gap - 1.0e-5_real64 * (maxval(zred) - minval(zred))) <= 1.0e-12_real64, &
            "the gap floor is a hundred-thousandth of the range of los")
        if (allocated(error)) return
        call pf_argsort(zred, perm)
        allocate (zs(n))
        do b = 1_int64, n
            zs(b) = zred(perm(b))
        end do
        b = 2_int64
        do while (zs(b) - zs(1) < gap)
            b = b + 1_int64
        end do
        l_ref = (los_comoving(zs(b)) - los_comoving(zs(1))) / (zs(b) - zs(1))
        l_analytic = los_ch0 * (1.0_real64 + zs(1))**(-1.5_real64)
        call check(error, abs(lip - l_ref) <= 1.0e-6_real64 * l_ref, &
            "the measured slope must be the steepest secant over pairs at least the gap apart, to a millionth")
        if (allocated(error)) return
        call check(error, abs(lip - l_analytic) <= 1.0e-5_real64 * l_analytic, &
            "the measured slope must be the analytic dD/dz at the nearest redshift, to a hundred-thousandth")
        if (allocated(error)) return
        call check(error, spread <= 1.01_real64 * lip * gap, &
            "the spread below the gap floor cannot exceed L times the floor by more than rounding")
    end subroutine test_pairs_los_slope_is_clean

    !> The worked example of the design in miniature: comoving coordinates, the redshift itself as
    !> `los=`, a comoving transverse length and a redshift-interval parallel length, all four rules
    !> against the oracle; and the slope the library measured is the survey's `dD/dz` at its near
    !> edge, which is the whole conversion between the two units.
    subroutine test_pairs_los_redshift_example(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), zred(:), d(:), x(:), y(:), z(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, nwant, counted(4)
        real(real64) :: lip, spread, gap, l_analytic
        integer :: rules(4), ri

        rules = [PF_LINK_MIN, PF_LINK_MEAN, PF_LINK_MAX, PF_LINK_SUM]
        call make_redshift_survey(260_int64, 40_int64, 0.02_real64, 0.2_real64, 6_int64, ra, dec, zred, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            ! A transverse length in Mpc/h; a parallel one as a velocity of 1000..4000 km/s over c.
            bp(a) = 10.0_real64 + 20.0_real64 * pf_random_at(fixture_seed, a, 37_int64)
            bl(a) = (1000.0_real64 + 3000.0_real64 * pf_random_at(fixture_seed, a, 38_int64)) / 299792.458_real64
        end do
        call sx%build(x, y, z, radius=30.0_real64, los=zred)
        call parquet_debug_spatial_los_bounds(sx, lip, spread, gap)
        l_analytic = los_ch0 * (1.0_real64 + minval(zred))**(-1.5_real64)
        call check(error, abs(lip - l_analytic) <= 1.0e-2_real64 * l_analytic, &
            "the slope must be dD/dz at the survey's nearest redshift, to a per cent on a few hundred galaxies")
        if (allocated(error)) return
        do ri = 1, 4
            call brute_los_pairs(ra, dec, d, zred, bp, bl, rules(ri), want, nwant)
            counted(ri) = nwant
            call check(error, nwant > 0_int64, "every rule must accept some pair in the redshift survey")
            if (allocated(error)) return
            call sx%pairs_within_los(bp, bl, pi, pj, combine=rules(ri))
            call check(error, pairs_equal_set(pi, pj, want, nwant), &
                "each rule over comoving coordinates with the redshift as los= must match the oracle")
            if (allocated(error)) return
        end do
        call check(error, counted(1) < counted(4), "the intersection must be smaller than the sum of cylinders")
    end subroutine test_pairs_los_redshift_example

    !> `dperp=` and `dpar=` report the two separations per pair in their two units: the transverse
    !> one against the haversine oracle to a part in a billion, and the parallel one exactly the
    !> redshift difference. Either may be asked for alone.
    subroutine test_pairs_los_reports_separations(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), zred(:), d(:), x(:), y(:), z(:), bp(:), bl(:), dp(:), dl(:)
        integer(int64), allocatable :: pi(:), pj(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, k
        real(real64) :: ref

        call make_redshift_survey(200_int64, 40_int64, 0.02_real64, 0.2_real64, 7_int64, ra, dec, zred, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n))
        do a = 1_int64, n
            bp(a) = 10.0_real64 + 20.0_real64 * pf_random_at(fixture_seed, a, 39_int64)
            bl(a) = 0.003_real64 + 0.01_real64 * pf_random_at(fixture_seed, a, 40_int64)
        end do
        call sx%build(x, y, z, radius=30.0_real64, los=zred)
        call sx%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MEAN, dperp=dp, dpar=dl)
        call check(error, size(pi, kind=int64) > 0_int64, "the fixture must hold pairs")
        if (allocated(error)) return
        call check(error, size(dp, kind=int64) == size(pi, kind=int64) .and. size(dl, kind=int64) == size(pi, kind=int64), &
            "one transverse and one parallel separation per pair")
        if (allocated(error)) return
        do k = 1_int64, size(pi, kind=int64)
            ref = los_dperp(ra(pi(k)), dec(pi(k)), d(pi(k)), ra(pj(k)), dec(pj(k)), d(pj(k)))
            ! Relative, with an absolute floor of a tenth of a parsec: two galaxies on one spoke
            ! share a direction exactly, so the oracle's separation is zero there while the
            ! library's is the rounding of a thousand-unit coordinate, about 1e-13.
            call check(error, abs(dp(k) - ref) <= 1.0e-9_real64 * ref + 1.0e-10_real64, &
                "dperp must be the haversine transverse separation to a part in a billion")
            if (allocated(error)) return
            call check(error, dl(k) == abs(zred(pi(k)) - zred(pj(k))), &
                "dpar must be exactly the difference of the two parallel coordinates")
            if (allocated(error)) return
        end do
        ! Alone, through the scalar form.
        call sx%pairs_within_los(20.0_real64, 0.005_real64, pi, pj, dpar=dl)
        call check(error, size(dl, kind=int64) == size(pi, kind=int64), "dpar alone must still be one per pair")
        if (allocated(error)) return
        do k = 1_int64, size(pi, kind=int64)
            call check(error, dl(k) == abs(zred(pi(k)) - zred(pj(k))) .and. dl(k) <= 0.005_real64, &
                "dpar alone must be the parallel separation, inside the parallel length")
            if (allocated(error)) return
        end do
    end subroutine test_pairs_los_reports_separations

    !> `observer=` moves the origin of every line of sight: shifted coordinates with the shift as the
    !> observer reproduce the origin build's pairs and single query.
    subroutine test_pairs_los_observer_offset(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:), qi(:), qj(:), fa(:), fb(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: origin, shifted
        integer(int64) :: n, a, nwant, ma, mb
        real(real64) :: pq(3)
        real(real64), parameter :: o(3) = [300.0_real64, -200.0_real64, 50.0_real64]

        call make_wedge(240_int64, 30_int64, 500.0_real64, 1500.0_real64, 8_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n), fa(n), fb(n))
        do a = 1_int64, n
            bp(a) = 8.0_real64 + 8.0_real64 * pf_random_at(fixture_seed, a, 41_int64)
            bl(a) = 20.0_real64 + 30.0_real64 * pf_random_at(fixture_seed, a, 42_int64)
        end do
        call origin%build(x, y, z, radius=sqrt(bp**2 + bl**2))
        call shifted%build(x + o(1), y + o(2), z + o(3), radius=sqrt(bp**2 + bl**2), observer=o)
        call brute_los_pairs(ra, dec, d, d, bp, bl, PF_LINK_MAX, want, nwant)
        call check(error, nwant > 0_int64, "the fixture must hold accepted pairs")
        if (allocated(error)) return
        call origin%pairs_within_los(bp, bl, pi, pj)
        call shifted%pairs_within_los(bp, bl, qi, qj)
        call check(error, pairs_equal_set(pi, pj, want, nwant), "the origin build must match the oracle")
        if (allocated(error)) return
        call check(error, pairs_equal_set(qi, qj, want, nwant), &
            "shifted coordinates about a shifted observer must give the same pairs")
        if (allocated(error)) return
        pq = [x(1) + o(1), y(1) + o(2), z(1) + o(3)]
        ma = origin%within_los([x(1), y(1), z(1)], bp(1), bl(1), fa)
        mb = shifted%within_los(pq, bp(1), bl(1), fb)
        call check(error, ma > 0_int64 .and. same_rows(fa, ma, fb, mb), &
            "a single query about the shifted observer must find the same rows")
    end subroutine test_pairs_los_observer_offset

    !> `%within_los` with a point's own lengths returns exactly the points inside that point's OWN
    !> cylinder -- the directed relation, asserted from the fixture for every point, on an index with
    !> `los=` and on one without.
    subroutine test_within_los_matches_oracle(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), zred(:), d(:), x(:), y(:), z(:), bp(:), bl(:)
        integer(int64), allocatable :: found(:), want(:)
        type(pf_spatial_index) :: sz, sd
        integer(int64) :: n, a, m, mw, hits
        real(real64) :: p(3)

        call make_redshift_survey(200_int64, 30_int64, 0.02_real64, 0.2_real64, 9_int64, ra, dec, zred, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), found(n))
        do a = 1_int64, n
            bp(a) = 10.0_real64 + 20.0_real64 * pf_random_at(fixture_seed, a, 43_int64)
            bl(a) = 0.003_real64 + 0.01_real64 * pf_random_at(fixture_seed, a, 44_int64)
        end do
        call sz%build(x, y, z, radius=30.0_real64, los=zred)
        hits = 0_int64
        do a = 1_int64, n
            p = [x(a), y(a), z(a)]
            m = sz%within_los(p, bp(a), bl(a), found, los_p=zred(a))
            call brute_los_own(ra, dec, d, zred, a, bp(a), bl(a), want, mw)
            call check(error, m == mw .and. same_rows(found, m, want, mw), &
                "within_los with los= must return exactly the point's own cylinder")
            if (allocated(error)) return
            if (m > 1_int64) hits = hits + 1_int64
        end do
        call check(error, hits > 0_int64, "some point must have a neighbour besides itself")
        if (allocated(error)) return
        ! Without los=: the parallel coordinate is the distance, and the lengths are both Mpc/h.
        call sd%build(x, y, z, radius=30.0_real64)
        do a = 1_int64, n
            p = [x(a), y(a), z(a)]
            m = sd%within_los(p, bp(a), 30.0_real64, found)
            call brute_los_own(ra, dec, d, d, a, bp(a), 30.0_real64, want, mw)
            call check(error, m == mw .and. same_rows(found, m, want, mw), &
                "within_los without los= must return the point's own cylinder in distance")
            if (allocated(error)) return
        end do
    end subroutine test_within_los_matches_oracle

    !> `dist=` is `max(d_perp/b_perp, d_par/b_par)`, `sorted=.true.` orders by it, and the rows are
    !> the same set the unsorted call returns.
    subroutine test_within_los_sorted_by_normalised_measure(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), zred(:), d(:), x(:), y(:), z(:), dn(:), dp(:), dl(:)
        integer(int64), allocatable :: found(:), plain(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, m, mp, k, best, mbest
        real(real64) :: p(3), ref
        real(real64), parameter :: bperp = 30.0_real64, bpar = 0.01_real64

        call make_redshift_survey(200_int64, 30_int64, 0.02_real64, 0.2_real64, 10_int64, ra, dec, zred, d, x, y, z)
        n = size(x, kind=int64)
        allocate (found(n), plain(n), dn(n), dp(n), dl(n))
        call sx%build(x, y, z, radius=bperp, los=zred)
        ! The most populated cylinder, so the ordering has something to order.
        best = 1_int64
        mbest = 0_int64
        do a = 1_int64, n
            p = [x(a), y(a), z(a)]
            m = sx%within_los(p, bperp, bpar, plain, los_p=zred(a))
            if (m > mbest) then
                mbest = m
                best = a
            end if
        end do
        call check(error, mbest >= 3_int64, "some cylinder must hold at least three points")
        if (allocated(error)) return
        p = [x(best), y(best), z(best)]
        mp = sx%within_los(p, bperp, bpar, plain, los_p=zred(best))
        m = sx%within_los(p, bperp, bpar, found, los_p=zred(best), dist=dn, dperp=dp, dpar=dl, sorted=.true.)
        call check(error, m == mp .and. same_rows(found, m, plain, mp), "sorting must keep the same rows")
        if (allocated(error)) return
        do k = 1_int64, m
            ref = max(dp(k) / bperp, dl(k) / bpar)
            call check(error, abs(dn(k) - ref) <= 1.0e-12_real64, &
                "dist must be the normalised measure max(d_perp/b_perp, d_par/b_par)")
            if (allocated(error)) return
            call check(error, dn(k) <= 1.0_real64 + 1.0e-12_real64, "every reported point lies inside the cylinder")
            if (allocated(error)) return
            if (k > 1_int64) then
                call check(error, dn(k) >= dn(k - 1_int64), "sorted= must order by increasing normalised measure")
                if (allocated(error)) return
            end if
        end do
        call check(error, found(1) == best .and. dn(1) == 0.0_real64, &
            "the point itself is nearest in its own cylinder, at measure zero")
    end subroutine test_within_los_sorted_by_normalised_measure

    !> A short buffer, int32 or int64, still returns the TRUE count, and what it holds are members.
    subroutine test_within_los_short_buffer_keeps_true_count(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), zred(:), d(:), x(:), y(:), z(:)
        integer(int64), allocatable :: full(:), want(:), two(:)
        integer(int32), allocatable :: two32(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, m, m2, mw, best, mbest, k
        real(real64) :: p(3)
        real(real64), parameter :: bperp = 30.0_real64, bpar = 0.01_real64

        call make_redshift_survey(200_int64, 30_int64, 0.02_real64, 0.2_real64, 11_int64, ra, dec, zred, d, x, y, z)
        n = size(x, kind=int64)
        allocate (full(n), two(2), two32(2))
        call sx%build(x, y, z, radius=bperp, los=zred)
        best = 1_int64
        mbest = 0_int64
        do a = 1_int64, n
            p = [x(a), y(a), z(a)]
            m = sx%within_los(p, bperp, bpar, full, los_p=zred(a))
            if (m > mbest) then
                mbest = m
                best = a
            end if
        end do
        call check(error, mbest >= 3_int64, "some cylinder must hold at least three points")
        if (allocated(error)) return
        p = [x(best), y(best), z(best)]
        call brute_los_own(ra, dec, d, zred, best, bperp, bpar, want, mw)
        m2 = sx%within_los(p, bperp, bpar, two, los_p=zred(best))
        call check(error, m2 == mw, "an int64 buffer of two must still report the true count")
        if (allocated(error)) return
        do k = 1_int64, 2_int64
            call check(error, any(want(1:mw) == two(k)), "what a short buffer holds must be members of the cylinder")
            if (allocated(error)) return
        end do
        m2 = sx%within_los(p, bperp, bpar, two32, los_p=zred(best))
        call check(error, m2 == mw, "an int32 buffer of two must still report the true count")
        if (allocated(error)) return
        do k = 1_int64, 2_int64
            call check(error, any(want(1:mw) == int(two32(k), kind=int64)), &
                "what a short int32 buffer holds must be members of the cylinder")
            if (allocated(error)) return
        end do
    end subroutine test_within_los_short_buffer_keeps_true_count

    !> The parallel coordinate follows every re-bucketing: a `copy=.false.` index whose bulk call
    !> re-tunes it (the borrowed-coordinate path scatters `los` back to row order first), a
    !> `%rebuild_for`, and a `%rebuild` with moved points and new `los`. `los = D/2` here, so a `los`
    !> attached to the wrong row would change a parallel separation and break the oracle match.
    subroutine test_pairs_los_survives_rebuild_and_copy_false(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), bp(:), bl(:), los(:), d2(:), los2(:)
        real(real64), allocatable, target :: x(:), y(:), z(:)
        real(real64), allocatable :: x2(:), y2(:), z2(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: borrowed, owned
        integer(int64) :: n, a, nwant, rebuilds
        logical :: changed
        character(len=:), allocatable :: verb_was

        ! The re-tune below is provoked on purpose, so its once-per-index advice is noise here;
        ! restored at the end, and the suite runs its tests serially so nothing else sees the toggle.
        call parquet_get_verbosity(verb_was)
        call parquet_set_verbosity("silent")
        call make_wedge(220_int64, 40_int64, 500.0_real64, 1500.0_real64, 12_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            bp(a) = 8.0_real64 + 4.0_real64 * pf_random_at(fixture_seed, a, 45_int64)
            bl(a) = 12.0_real64 + 6.0_real64 * pf_random_at(fixture_seed, a, 46_int64)
        end do
        los = 0.5_real64 * d
        call brute_los_pairs(ra, dec, d, los, bp, bl, PF_LINK_MEAN, want, nwant)
        call check(error, nwant > 0_int64, "the fixture must hold accepted pairs")
        if (allocated(error)) return
        ! A deliberately poor radius hint, so that the bulk call re-tunes and re-buckets a
        ! BORROWED index -- the one route that permutes los through the old row map.
        call parquet_debug_reset_spatial_counters()
        call borrowed%build(x, y, z, radius=0.5_real64, los=los, copy=.false.)
        call borrowed%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MEAN)
        rebuilds = parquet_debug_spatial_rebuilds()
        call check(error, rebuilds > 0_int64, "the poor hint must have made the bulk call re-tune the borrowed index")
        if (allocated(error)) return
        call check(error, pairs_equal_set(pi, pj, want, nwant), &
            "a re-tuned copy=.false. index must keep los aligned with its rows")
        if (allocated(error)) return
        call borrowed%rebuild_for(60.0_real64)
        call borrowed%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MEAN)
        call check(error, pairs_equal_set(pi, pj, want, nwant), &
            "%rebuild_for on a copy=.false. index must keep los aligned")
        if (allocated(error)) return
        ! An owning index: %rebuild with unchanged data is a no-op, and with moved points and a new
        ! los it rebuilds and answers about the new data.
        call owned%build(x, y, z, radius=40.0_real64, los=los)
        call owned%rebuild(x, y, z, rebuilt=changed, los=los)
        call check(error, .not. changed, "%rebuild with the same coordinates and los must not rebuild")
        if (allocated(error)) return
        call owned%rebuild_for(60.0_real64)
        call owned%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MEAN)
        call check(error, pairs_equal_set(pi, pj, want, nwant), "%rebuild_for on an owning index must keep los aligned")
        if (allocated(error)) return
        x2 = 1.1_real64 * x
        y2 = 1.1_real64 * y
        z2 = 1.1_real64 * z
        d2 = 1.1_real64 * d
        los2 = 0.5_real64 * d2
        call owned%rebuild(x2, y2, z2, rebuilt=changed, los=los2)
        call check(error, changed, "%rebuild with moved coordinates must rebuild")
        if (allocated(error)) return
        call brute_los_pairs(ra, dec, d2, los2, bp, bl, PF_LINK_MEAN, want, nwant)
        call owned%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MEAN)
        call check(error, pairs_equal_set(pi, pj, want, nwant), &
            "after %rebuild the pairs must be those of the new coordinates and the new los")
        if (allocated(error)) return
        ! A moved los over unmoved points is a change too.
        call owned%rebuild(x2, y2, z2, rebuilt=changed, los=0.25_real64 * d2)
        call check(error, changed, "%rebuild with a changed los alone must rebuild")
        call parquet_set_verbosity(verb_was)
    end subroutine test_pairs_los_survives_rebuild_and_copy_false

    !> The pair list `(pi, pj)` as a set over `n` rows, for use as the reference another walk is
    !> held to: .false. when it holds a pair twice or one not ordered `i < j`.
    logical function pairs_to_set(pi, pj, n, want, nwant) result(ok)
        integer(int64), intent(in) :: pi(:) !! the lower rows.
        integer(int64), intent(in) :: pj(:) !! the higher rows.
        integer(int64), intent(in) :: n !! how many rows the index holds.
        logical, allocatable, intent(out) :: want(:, :) !! the set, upper triangle.
        integer(int64), intent(out) :: nwant !! how many pairs it holds.
        integer(int64) :: k

        ok = .false.
        nwant = size(pi, kind=int64)
        allocate (want(n, n))
        want = .false.
        if (size(pj, kind=int64) /= nwant) return
        do k = 1_int64, nwant
            if (pi(k) >= pj(k)) return
            if (want(pi(k), pj(k))) return
            want(pi(k), pj(k)) = .true.
        end do
        ok = .true.
    end function pairs_to_set

    !> The cylinder walk and the covering-ball walk return identical pair sets under every rule,
    !> with and without `los=`, and under both radial bounds, on a fixture with pairs in the
    !> padding band the cylinder's bounds exist for and with points so close to the observer that
    !> the ball is the shorter walk (`feature_risks.md`: the walked region must contain the
    !> accepted set).
    !>
    !> The ball walk is the Stage 2 sweep unchanged -- its own rank, its own union -- and the
    !> cylinder walk is forced too, both through `parquet_debug_set_spatial_los_walk`, since on a
    !> test-sized cell the library's own choice is the ball for every point
    !> (`test_los_walk_choice_follows_the_cell`); so the two paths share the accept test and
    !> nothing else. Three preconditions make the fixture load-bearing: accepted pairs whose partner
    !> lies OUTSIDE the emitter's unpadded geometric cylinder exist, built to order at the far end of
    !> the parallel window with the transverse separation just inside `b_perp` and at its near end
    !> likewise; the walk counters say both routes ran, six points as balls and the rest as
    !> cylinders; and the scalar form also matches the brute-force oracle, so the two walks are not
    !> merely equal but right. With `los=`, the per-point window can only narrow the global bound,
    !> so the candidates it tests are asserted not to exceed the global arm's.
    subroutine test_los_cylinder_walk_matches_ball_walk(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:), zred(:)
        real(real64), allocatable :: tra(:), tdec(:), td(:)
        integer(int64), allocatable :: bi(:), bj(:), ci(:), cj(:), base(:), part(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx, sz
        integer(int64) :: n0, n, a, k, s, nwant, ncyl, nbal, ntest, ntest_g, nband
        real(real64) :: bp0, bl0, dj, dth, perp, proj
        integer :: rules(4), ri
        real(real64), parameter :: near_ra = 0.01_real64, near_dec = 0.02_real64

        rules = [PF_LINK_MIN, PF_LINK_MEAN, PF_LINK_MAX, PF_LINK_SUM]
        bp0 = 12.0_real64
        bl0 = 20.0_real64
        call make_wedge(200_int64, 20_int64, 500.0_real64, 1500.0_real64, 21_int64, ra, dec, d, x, y, z)
        n0 = size(x, kind=int64)
        ! Six points along one line of sight at 1..3.5 from the observer: closer than half their
        ! parallel length, so each walks the covering ball, and all fifteen pairs among them
        ! qualify (d_perp = 0).
        allocate (tra(18), tdec(18), td(18), base(6), part(6))
        do k = 1_int64, 6_int64
            tra(k) = near_ra
            tdec(k) = near_dec
            td(k) = 1.0_real64 + 0.5_real64 * real(k - 1_int64, kind=real64)
        end do
        ! Six band pairs, the base row before its partner so the base emits under the scalar rank:
        ! three partners at the far end of the parallel window with d_perp at 0.995 b_perp, whose
        ! distance from the base's line of sight exceeds b_perp; three at its near end with d_perp
        ! at 0.999 b_perp, whose projection falls below D - b_par.
        do s = 1_int64, 6_int64
            k = 6_int64 + 2_int64 * (s - 1_int64) + 1_int64
            base(s) = n0 + k
            part(s) = n0 + k + 1_int64
            tra(k) = -0.05_real64 + 0.02_real64 * real(s, kind=real64)
            tdec(k) = 0.03_real64
            td(k) = 600.0_real64 + 100.0_real64 * real(s, kind=real64)
            if (s <= 3_int64) then
                dj = td(k) + 0.9_real64 * bl0
                dth = 2.0_real64 * asin(0.995_real64 * bp0 / (td(k) + dj))
            else
                dj = td(k) - 0.999_real64 * bl0
                dth = 2.0_real64 * asin(0.999_real64 * bp0 / (td(k) + dj))
            end if
            tra(k + 1_int64) = tra(k)
            tdec(k + 1_int64) = tdec(k) + dth
            td(k + 1_int64) = dj
        end do
        ra = [ra, tra]
        dec = [dec, tdec]
        d = [d, td]
        n = size(ra, kind=int64)
        x = d * cos(dec) * cos(ra)
        y = d * cos(dec) * sin(ra)
        z = d * sin(dec)
        ! The precondition, from the fixture's own geometry: the partner's distance from the base's
        ! line of sight, and its projection on it.
        nband = 0_int64
        do s = 1_int64, 6_int64
            dth = dec(part(s)) - dec(base(s))
            perp = d(part(s)) * sin(dth)
            proj = d(part(s)) * cos(dth)
            if (perp > bp0 .or. proj < d(base(s)) - bl0 .or. proj > d(base(s)) + bl0) nband = nband + 1_int64
        end do
        call check(error, nband == 6_int64, "every built pair must lie outside the base's unpadded geometric cylinder")
        if (allocated(error)) return

        ! ---- The scalar form, no los: forced ball, then the cylinder walk, then the oracle ----
        call sx%build(x, y, z, radius=bp0)
        call parquet_debug_reset_spatial_counters()
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        call parquet_debug_set_spatial_los_walk(walk_ball)
        call sx%pairs_within_los(bp0, bl0, bi, bj)
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
        call check(error, nbal == n .and. ncyl == 0_int64, "the forced walk must walk every point as a ball")
        if (allocated(error)) return
        call check(error, pairs_to_set(bi, bj, n, want, nwant), "the ball walk's list must be a set with i < j")
        if (allocated(error)) return
        do s = 1_int64, 6_int64
            call check(error, want(base(s), part(s)), "every built band pair must be accepted")
            if (allocated(error)) return
        end do
        call check(error, want(n0 + 1_int64, n0 + 6_int64), "the near points must pair with one another")
        if (allocated(error)) return
        call parquet_debug_reset_spatial_counters()
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        call sx%pairs_within_los(bp0, bl0, ci, cj)
        call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
        call check(error, nbal == 6_int64 .and. ncyl == n - 6_int64, &
            "the six near points must walk the ball and every other point the cylinder")
        if (allocated(error)) return
        call check(error, pairs_equal_set(ci, cj, want, nwant), &
            "the cylinder walk must return exactly the ball walk's pairs (scalar lengths, no los)")
        if (allocated(error)) return
        allocate (bp(n), bl(n))
        bp = bp0
        bl = bl0
        call brute_los_pairs(ra, dec, d, d, bp, bl, PF_LINK_MEAN, want, nwant)
        call check(error, pairs_equal_set(ci, cj, want, nwant), "the cylinder walk must also match the oracle")
        if (allocated(error)) return

        ! ---- Per-point lengths, no los, every rule ----
        do a = 1_int64, n
            bp(a) = 8.0_real64 + 8.0_real64 * pf_random_at(fixture_seed, a, 51_int64)
            bl(a) = 12.0_real64 + 20.0_real64 * pf_random_at(fixture_seed, a, 52_int64)
        end do
        call sx%rebuild_for(bp)
        do ri = 1, 4
            call parquet_debug_set_spatial_los_walk(walk_ball)
            call sx%pairs_within_los(bp, bl, bi, bj, combine=rules(ri))
            call parquet_debug_set_spatial_los_walk(walk_cyl)
            call check(error, pairs_to_set(bi, bj, n, want, nwant), "the ball walk's list must be a set with i < j")
            if (allocated(error)) return
            call check(error, nwant > 0_int64, "every rule must accept some pair")
            if (allocated(error)) return
            call parquet_debug_reset_spatial_counters()
            call parquet_debug_set_spatial_los_walk(walk_cyl)
            call sx%pairs_within_los(bp, bl, ci, cj, combine=rules(ri))
            call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
            call check(error, nbal > 0_int64 .and. ncyl > 0_int64, "both routes must run under every rule")
            if (allocated(error)) return
            call check(error, pairs_equal_set(ci, cj, want, nwant), &
                "the cylinder walk must return exactly the ball walk's pairs under every rule")
            if (allocated(error)) return
        end do

        ! ---- With los=, every rule, under the per-point window and under the global bound ----
        call make_redshift_survey(200_int64, 20_int64, 0.02_real64, 0.5_real64, 22_int64, ra, dec, zred, d, x, y, z)
        n = size(x, kind=int64)
        deallocate (bp, bl)
        allocate (bp(n), bl(n))
        do a = 1_int64, n
            bp(a) = 10.0_real64 + 20.0_real64 * pf_random_at(fixture_seed, a, 53_int64)
            bl(a) = 0.002_real64 + 0.01_real64 * pf_random_at(fixture_seed, a, 54_int64)
        end do
        call sz%build(x, y, z, radius=20.0_real64, los=zred)
        do ri = 1, 4
            call parquet_debug_set_spatial_los_walk(walk_ball)
            call sz%pairs_within_los(bp, bl, bi, bj, combine=rules(ri))
            call parquet_debug_set_spatial_los_walk(walk_cyl)
            call check(error, pairs_to_set(bi, bj, n, want, nwant), "the ball walk's list must be a set with i < j")
            if (allocated(error)) return
            call check(error, nwant > 0_int64, "every rule must accept some pair on the survey")
            if (allocated(error)) return
            call parquet_debug_reset_spatial_counters()
            call parquet_debug_set_spatial_los_walk(walk_cyl)
            call sz%pairs_within_los(bp, bl, ci, cj, combine=rules(ri))
            call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
            call check(error, ncyl == n .and. nbal == 0_int64, "on the survey every point must walk the cylinder")
            if (allocated(error)) return
            call check(error, pairs_equal_set(ci, cj, want, nwant), &
                "with los= the per-point window's cylinder walk must return exactly the ball walk's pairs")
            if (allocated(error)) return
            call parquet_debug_reset_spatial_counters()
            call parquet_debug_set_spatial_los_walk(walk_cyl)
            call parquet_debug_set_spatial_los_spread(.true.)
            call sz%pairs_within_los(bp, bl, ci, cj, combine=rules(ri))
            call parquet_debug_set_spatial_los_spread(.false.)
            call parquet_debug_spatial_los_walk(ncyl, nbal, ntest_g)
            call check(error, pairs_equal_set(ci, cj, want, nwant), &
                "with los= the global bound's cylinder walk must return exactly the ball walk's pairs")
            if (allocated(error)) return
            call check(error, ntest <= ntest_g, &
                "the per-point window lies inside the global bound, so it cannot test more candidates")
            if (allocated(error)) return
        end do
        call check(error, pairs_equal_set(bi, bj, want, nwant), "the sum rule's ball list must be the reference just built")
        call parquet_debug_reset_spatial_counters()
    end subroutine test_los_cylinder_walk_matches_ball_walk

    !> Under the union each endpoint walks its own cylinder, and a pair lying in BOTH cylinders is
    !> emitted exactly once, by the tiebreak's rank term (`feature_risks.md`): the pair list is
    !> exactly the oracle's, so a pair emitted twice or never would fail it. The fixture must hold
    !> pairs in exactly one cylinder and pairs in both, or the tiebreak's two booleans would not
    !> both be exercised.
    subroutine test_los_union_tiebreak_counts(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, b, nwant, nboth, none, ncyl, nbal, ntest
        real(real64) :: dp, dl
        logical :: own_a, own_b

        call make_wedge(260_int64, 40_int64, 500.0_real64, 1500.0_real64, 23_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            bp(a) = 6.0_real64 + 14.0_real64 * pf_random_at(fixture_seed, a, 55_int64)
            bl(a) = bp(a) * (3.0_real64 + 27.0_real64 * pf_random_at(fixture_seed, a, 56_int64))
        end do
        nboth = 0_int64
        none = 0_int64
        do a = 1_int64, n - 1_int64
            do b = a + 1_int64, n
                dp = los_dperp(ra(a), dec(a), d(a), ra(b), dec(b), d(b))
                dl = abs(d(a) - d(b))
                own_a = los_qualifies(0, dp, dl, bp(a), bl(a), bp(a), bl(a))
                own_b = los_qualifies(0, dp, dl, bp(b), bl(b), bp(b), bl(b))
                if (own_a .and. own_b) nboth = nboth + 1_int64
                if (own_a .neqv. own_b) none = none + 1_int64
            end do
        end do
        call check(error, nboth > 0_int64 .and. none > 0_int64, &
            "the fixture must hold pairs in both cylinders and pairs in exactly one")
        if (allocated(error)) return
        call brute_los_pairs(ra, dec, d, d, bp, bl, PF_LINK_MAX, want, nwant)
        call check(error, nwant == nboth + none, "the union's oracle count is the pairs in one cylinder or both")
        if (allocated(error)) return
        call sx%build(x, y, z, radius=bp)
        call parquet_debug_reset_spatial_counters()
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        call sx%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MAX)
        call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
        call check(error, ncyl == n, "every point must walk its own cylinder under the union")
        if (allocated(error)) return
        call check(error, pairs_equal_set(pi, pj, want, nwant), &
            "the union's cylinder walk must emit each pair exactly once: the oracle's set, no duplicate")
        call parquet_debug_reset_spatial_counters()
    end subroutine test_los_union_tiebreak_counts

    !> `%within_los` walks the cylinder when forced to -- the counter says so -- and returns what
    !> the ball walk returns, rows, measures and separations alike; a query point closer to the
    !> observer than its cylinder is long walks the ball even then, and one whose parallel window
    !> holds no stored point walks nothing and answers zero.
    subroutine test_within_los_walks_the_cylinder(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), zred(:), d(:), x(:), y(:), z(:)
        real(real64), allocatable :: d1(:), d2(:), q1(:), q2(:), l1(:), l2(:)
        integer(int64), allocatable :: f1(:), f2(:)
        type(pf_spatial_index) :: sz, sd
        integer(int64) :: n, a, m1, m2, ncyl, nbal, ntest, hits, k
        real(real64) :: p(3), zp, dp
        integer(int64) :: rows(3)

        call make_redshift_survey(200_int64, 30_int64, 0.02_real64, 0.2_real64, 24_int64, ra, dec, zred, d, x, y, z)
        n = size(x, kind=int64)
        allocate (f1(n), f2(n), d1(n), d2(n), q1(n), q2(n), l1(n), l2(n))
        call sz%build(x, y, z, radius=20.0_real64, los=zred)
        rows = [1_int64, n / 2_int64, n]
        hits = 0_int64
        do k = 1_int64, 3_int64
            a = rows(k)
            p = [x(a), y(a), z(a)]
            call parquet_debug_reset_spatial_counters()
            call parquet_debug_set_spatial_los_walk(walk_cyl)
            m1 = sz%within_los(p, 20.0_real64, 0.006_real64, f1, los_p=zred(a), dist=d1, dperp=q1, dpar=l1, sorted=.true.)
            call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
            call check(error, ncyl == 1_int64 .and. nbal == 0_int64, "a survey point's query must walk the cylinder")
            if (allocated(error)) return
            call check(error, ntest >= m1, "every reported point reached the accept test")
            if (allocated(error)) return
            call parquet_debug_set_spatial_los_walk(walk_ball)
            m2 = sz%within_los(p, 20.0_real64, 0.006_real64, f2, los_p=zred(a), dist=d2, dperp=q2, dpar=l2, sorted=.true.)
            call parquet_debug_set_spatial_los_walk(walk_cyl)
            call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
            call check(error, nbal == 1_int64, "the forced query must walk the ball")
            if (allocated(error)) return
            call check(error, m1 == m2, "the cylinder and the ball must find the same count")
            if (allocated(error)) return
            call check(error, all(f1(1:m1) == f2(1:m1)) .and. all(d1(1:m1) == d2(1:m1)) .and. &
                all(q1(1:m1) == q2(1:m1)) .and. all(l1(1:m1) == l2(1:m1)), &
                "the cylinder and the ball must report the same rows, measures and separations, in the same order")
            if (allocated(error)) return
            if (m1 > 1_int64) hits = hits + 1_int64
        end do
        call check(error, hits > 0_int64, "some query must find a neighbour besides the point itself")
        if (allocated(error)) return
        ! A query a hundredth of a unit from the observer: its near-end pad is thousands of units,
        ! so the ball is the shorter walk; its window reaches the survey's near edge, so it finds
        ! points, and the ball finds the same ones.
        dp = 0.01_real64
        p = [dp * cos(0.01_real64) * cos(0.02_real64), dp * cos(0.01_real64) * sin(0.02_real64), dp * sin(0.01_real64)]
        zp = 1.0_real64 / (1.0_real64 - 0.5_real64 * dp / los_ch0)**2 - 1.0_real64
        call parquet_debug_reset_spatial_counters()
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        m1 = sz%within_los(p, 10.0_real64, 0.03_real64, f1, los_p=zp, sorted=.true.)
        call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
        call check(error, nbal == 1_int64 .and. ncyl == 0_int64, "a query next to the observer must walk the ball")
        if (allocated(error)) return
        call check(error, m1 > 0_int64, "that query must find the survey's near edge")
        if (allocated(error)) return
        call parquet_debug_set_spatial_los_walk(walk_ball)
        m2 = sz%within_los(p, 10.0_real64, 0.03_real64, f2, los_p=zp, sorted=.true.)
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        call check(error, m1 == m2 .and. all(f1(1:m1) == f2(1:m1)), "the fallback ball must answer as the forced ball does")
        if (allocated(error)) return
        ! A parallel window holding no stored point: nothing to walk.
        call parquet_debug_reset_spatial_counters()
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        m1 = sz%within_los(p, 10.0_real64, 0.001_real64, f1, los_p=5.0_real64)
        call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
        call check(error, m1 == 0_int64 .and. ncyl == 1_int64 .and. nbal == 0_int64 .and. ntest == 0_int64, &
            "an empty parallel window is an empty cylinder, counted and not walked")
        if (allocated(error)) return
        ! Without los=: the same comparison, the parallel coordinate being the distance.
        call sd%build(x, y, z, radius=20.0_real64)
        a = rows(2)
        p = [x(a), y(a), z(a)]
        call parquet_debug_reset_spatial_counters()
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        m1 = sd%within_los(p, 20.0_real64, 30.0_real64, f1, dist=d1, sorted=.true.)
        call parquet_debug_spatial_los_walk(ncyl, nbal, ntest)
        call check(error, ncyl == 1_int64 .and. nbal == 0_int64, "without los= the query walks the cylinder too")
        if (allocated(error)) return
        call parquet_debug_set_spatial_los_walk(walk_ball)
        m2 = sd%within_los(p, 20.0_real64, 30.0_real64, f2, dist=d2, sorted=.true.)
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        call check(error, m1 == m2 .and. m1 > 1_int64, "without los= the two walks must agree on a non-trivial count")
        if (allocated(error)) return
        call check(error, all(f1(1:m1) == f2(1:m1)) .and. all(d1(1:m1) == d2(1:m1)), &
            "without los= the two walks must report the same rows and measures")
        call parquet_debug_reset_spatial_counters()
    end subroutine test_within_los_walks_the_cylinder

    !> The cells-per-point ceiling binds on a sparse survey and the test-only override relaxes it:
    !> a finer cell, more cells than the shipped ceiling allows, the same pairs; and clearing the
    !> override brings the shipped cell back (the negative control).
    subroutine test_cells_per_point_override_relaxes_the_ceiling(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:)
        integer(int64), allocatable :: pi(:), pj(:), qi(:), qj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, nwant, ncells1, ncells2, ncells3, gx, gy, gz, occ1
        real(real64) :: h1, h2, h3

        call make_wedge(360_int64, 10_int64, 500.0_real64, 1500.0_real64, 25_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        call parquet_debug_reset_spatial_counters()
        call sx%build(x, y, z, radius=5.0_real64)
        h1 = sx%cell_size()
        call sx%grid(gx, gy, gz)
        ncells1 = gx * gy * gz
        call parquet_debug_spatial_occupied_cells(sx, occ1)
        ! The precondition: the ceiling, not the radius, chose this cell.
        call check(error, occ1 <= max(1_int64, int(0.3_real64 * real(n, kind=real64), kind=int64)) .and. &
            ncells1 <= 4_int64 * n .and. h1 > 4.0_real64 * 5.0_real64, &
            "on a sparse wedge the shipped ceiling must coarsen the cell far above the radius")
        if (allocated(error)) return
        call sx%pairs_within_los(5.0_real64, 40.0_real64, pi, pj)
        call check(error, pairs_to_set(pi, pj, n, want, nwant) .and. nwant > 0_int64, "the reference list must be a non-empty set")
        if (allocated(error)) return
        call parquet_debug_set_spatial_max_cells_per_point(30.0_real64)
        call sx%rebuild_for(5.0_real64)
        h2 = sx%cell_size()
        call sx%grid(gx, gy, gz)
        ncells2 = gx * gy * gz
        call check(error, h2 < h1 .and. ncells2 > ncells1 .and. &
            ncells2 <= int(30.0_real64 * real(n, kind=real64), kind=int64), &
            "a relaxed ceiling must give a finer cell within the relaxed count")
        if (allocated(error)) return
        call sx%pairs_within_los(5.0_real64, 40.0_real64, qi, qj)
        call check(error, pairs_equal_set(qi, qj, want, nwant), "the cell size must change no pair")
        if (allocated(error)) return
        call parquet_debug_reset_spatial_counters()
        call sx%rebuild_for(5.0_real64)
        h3 = sx%cell_size()
        call sx%grid(gx, gy, gz)
        ncells3 = gx * gy * gz
        call check(error, h3 == h1 .and. ncells3 == ncells1, "clearing the override must restore the shipped ceiling's cell")
        if (allocated(error)) return
        call sx%pairs_within_los(5.0_real64, 40.0_real64, qi, qj)
        call check(error, pairs_equal_set(qi, qj, want, nwant), "the restored cell must return the same pairs")
    end subroutine test_cells_per_point_override_relaxes_the_ceiling

    !> The library's own choice of walk follows the cell: a covering ball no wider than a cell is
    !> walked as the ball, one wider than a cell as the cylinder -- the near-observer fallback
    !> aside -- and both answers equal the oracle. The fine cell is reached through the ceiling
    !> override with an explicit `cell=`, since a test-sized wedge's shipped cell dwarfs every
    !> covering ball; the two forcing modes override the rule in either direction, which is the
    !> negative control for each half. Counters are read as differences, because the reset that
    !> clears them would also clear the override.
    subroutine test_los_walk_choice_follows_the_cell(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:), tra(:), tdec(:), td(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: coarse, fine
        integer(int64) :: n, k, nwant, c0, b0, t0, c1, b1, t1
        real(real64) :: bp0, bl0, diam, h_coarse, h_fine

        bp0 = 12.0_real64
        bl0 = 20.0_real64
        diam = 2.0_real64 * sqrt(bp0 * bp0 + bl0 * bl0)
        call make_wedge(200_int64, 20_int64, 500.0_real64, 1500.0_real64, 26_int64, ra, dec, d, x, y, z)
        ! Six points along one line of sight at 1..3.5 from the observer, which walk the ball under
        ! every mode but the forced ball's own, by the length rule.
        allocate (tra(6), tdec(6), td(6))
        do k = 1_int64, 6_int64
            tra(k) = 0.01_real64
            tdec(k) = 0.02_real64
            td(k) = 1.0_real64 + 0.5_real64 * real(k - 1_int64, kind=real64)
        end do
        ra = [ra, tra]
        dec = [dec, tdec]
        d = [d, td]
        n = size(ra, kind=int64)
        x = d * cos(dec) * cos(ra)
        y = d * cos(dec) * sin(ra)
        z = d * sin(dec)
        allocate (bp(n), bl(n), want(n, n))
        bp = bp0
        bl = bl0
        call brute_los_pairs(ra, dec, d, d, bp, bl, PF_LINK_MEAN, want, nwant)
        call check(error, nwant > 0_int64, "the fixture must hold accepted pairs")
        if (allocated(error)) return
        call parquet_debug_reset_spatial_counters()
        ! ---- the shipped ceiling: a cell far wider than the covering ball, so the ball everywhere ----
        call coarse%build(x, y, z, radius=bp0)
        h_coarse = coarse%cell_size()
        call check(error, h_coarse > diam, "precondition: the shipped cell must exceed the covering ball's diameter")
        if (allocated(error)) return
        call parquet_debug_spatial_los_walk(c0, b0, t0)
        call coarse%pairs_within_los(bp0, bl0, pi, pj)
        call parquet_debug_spatial_los_walk(c1, b1, t1)
        call check(error, b1 - b0 == n .and. c1 - c0 == 0_int64, &
            "with the covering ball no wider than a cell every point must walk the ball")
        if (allocated(error)) return
        call check(error, pairs_equal_set(pi, pj, want, nwant), "the ball walk's pairs must be the oracle's")
        if (allocated(error)) return
        ! ---- a cell below the covering ball's diameter: the cylinder, the six near points aside ----
        call parquet_debug_set_spatial_max_cells_per_point(3000.0_real64)
        call fine%build(x, y, z, radius=bp0, cell=5.0_real64)
        h_fine = fine%cell_size()
        call check(error, h_fine < diam, "precondition: the override must let the cell fall below the covering diameter")
        if (allocated(error)) return
        call parquet_debug_spatial_los_walk(c0, b0, t0)
        call fine%pairs_within_los(bp0, bl0, pi, pj)
        call parquet_debug_spatial_los_walk(c1, b1, t1)
        call check(error, c1 - c0 == n - 6_int64 .and. b1 - b0 == 6_int64, &
            "with the covering ball wider than a cell every point but the six near ones must walk the cylinder")
        if (allocated(error)) return
        call check(error, pairs_equal_set(pi, pj, want, nwant), "the cylinder walk's pairs must be the oracle's")
        if (allocated(error)) return
        ! ---- the two forcings override the rule, each in its own direction ----
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        call parquet_debug_spatial_los_walk(c0, b0, t0)
        call coarse%pairs_within_los(bp0, bl0, pi, pj)
        call parquet_debug_spatial_los_walk(c1, b1, t1)
        call check(error, c1 - c0 == n - 6_int64 .and. b1 - b0 == 6_int64, &
            "the forced cylinder must ignore the cell and keep the near-observer fallback")
        if (allocated(error)) return
        call check(error, pairs_equal_set(pi, pj, want, nwant), "the forced cylinder's pairs must be the oracle's")
        if (allocated(error)) return
        call parquet_debug_set_spatial_los_walk(walk_ball)
        call parquet_debug_spatial_los_walk(c0, b0, t0)
        call fine%pairs_within_los(bp0, bl0, pi, pj)
        call parquet_debug_spatial_los_walk(c1, b1, t1)
        call check(error, b1 - b0 == n .and. c1 - c0 == 0_int64, "the forced ball must walk the ball at the fine cell too")
        if (allocated(error)) return
        call check(error, pairs_equal_set(pi, pj, want, nwant), "the forced ball's pairs must be the oracle's")
        if (allocated(error)) return
        call parquet_debug_set_spatial_los_walk(walk_auto)
        call parquet_debug_reset_spatial_counters()
    end subroutine test_los_walk_choice_follows_the_cell

    ! ---- The one-pass line-of-sight sweep: chunks, buffers and the concatenation ----

    !> The sweep concatenates its threads' pair lists chunk by chunk in stored order, so the list
    !> is identical, entry for entry, whatever the team size. The chunk is lowered to seven
    !> emitters so a few hundred points span dozens of chunks and the boundaries are crossed by
    !> more than one thread. The mutation this catches is a concatenation in THREAD order, which
    !> keeps the set and scrambles the order; the serial arm is held to the oracle so the
    !> equality cannot hold for a wrong list.
    subroutine test_los_sweep_order_is_team_invariant(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:)
        real(real64), allocatable :: dp1(:), dl1(:), dp4(:), dl4(:)
        integer(int64), allocatable :: i1(:), j1(:), i4(:), j4(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, nwant
        integer :: used4
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without a team both arms run the same serial sweep, so " // &
            "the equality below would hold for the wrong reason")
        return
#endif
        call make_wedge(300_int64, 40_int64, 500.0_real64, 1500.0_real64, 1_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            bp(a) = 10.0_real64 + 10.0_real64 * pf_random_at(fixture_seed, a, 31_int64)
            bl(a) = bp(a) * (5.0_real64 + 25.0_real64 * pf_random_at(fixture_seed, a, 32_int64))
        end do
        call sx%build(x, y, z, radius=sqrt(bp**2 + bl**2))
        call parquet_debug_reset_spatial_counters()
        call parquet_debug_set_spatial_sweep_chunk(7_int64)
        call sx%pairs_within_los(bp, bl, i1, j1, combine=PF_LINK_MEAN, dperp=dp1, dpar=dl1, threads=1)
        call sx%pairs_within_los(bp, bl, i4, j4, combine=PF_LINK_MEAN, dperp=dp4, dpar=dl4, threads=4)
        used4 = parquet_debug_spatial_threads_used()
        call parquet_debug_reset_spatial_counters()
        if (used4 < 2) then
            call skip_test(error, "needs at least two processors: the affinity clamp resolved the " // &
                "four-thread arm to a team of one, so both arms ran serially")
            return
        end if
        call brute_los_pairs(ra, dec, d, d, bp, bl, PF_LINK_MEAN, want, nwant)
        call check(error, nwant > 0_int64, "the fixture must hold pairs")
        if (allocated(error)) return
        call check(error, pairs_equal_set(i1, j1, want, nwant), "the serial sweep's list must be the oracle's set")
        if (allocated(error)) return
        call check(error, size(i4, kind=int64) == nwant, "the four-thread sweep must return as many pairs as the serial one")
        if (allocated(error)) return
        call check(error, all(i4 == i1) .and. all(j4 == j1), &
            "the four-thread sweep must return the serial sweep's pairs in the serial sweep's order")
        if (allocated(error)) return
        call check(error, all(dp4 == dp1) .and. all(dl4 == dl1), &
            "the separations must travel with their pairs whatever the team size")
    end subroutine test_los_sweep_order_is_team_invariant

    !> A thread's buffer starts at `pair_buf_first` pairs (`src/parquet_spatial_bulk.f90`) and
    !> doubles; the doubling copy must carry every pair. A clump of 120 galaxies within a few units
    !> of one another gives 7140 pairs that all qualify -- several doublings on one thread -- and
    !> every one of them is checked against the oracle. The mutation is a growth copy one element
    !> short.
    subroutine test_los_sweep_buffers_grow(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, nwant
        integer(int64), parameter :: nclump = 120_int64

        n = nclump
        allocate (ra(n), dec(n), d(n), x(n), y(n), z(n), bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            ra(a) = 0.05_real64 + 0.002_real64 * (pf_random_at(fixture_seed + 61_int64, a, 1_int64) - 0.5_real64)
            dec(a) = -0.02_real64 + 0.002_real64 * (pf_random_at(fixture_seed + 61_int64, a, 2_int64) - 0.5_real64)
            d(a) = 1000.0_real64 + 4.0_real64 * (pf_random_at(fixture_seed + 61_int64, a, 3_int64) - 0.5_real64)
        end do
        x = d * cos(dec) * cos(ra)
        y = d * cos(dec) * sin(ra)
        z = d * sin(dec)
        bp = 5.0_real64
        bl = 20.0_real64
        call brute_los_pairs(ra, dec, d, d, bp, bl, PF_LINK_MEAN, want, nwant)
        call check(error, nwant == n * (n - 1_int64) / 2_int64, "precondition: every pair of the clump must qualify")
        if (allocated(error)) return
        call check(error, nwant > 4096_int64, "precondition: the clump must overflow the first buffer at least twice")
        if (allocated(error)) return
        call sx%build(x, y, z, radius=5.0_real64)
        call parquet_debug_set_spatial_sweep_chunk(7_int64)
        call sx%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MEAN, threads=1)
        call parquet_debug_reset_spatial_counters()
        call check(error, pairs_equal_set(pi, pj, want, nwant), "every pair must survive the buffer's growth")
    end subroutine test_los_sweep_buffers_grow

    !> The separations are appended beside their pair and copied out beside it, chunk by chunk and
    !> thread by thread; the mutation is the two arrays swapped, or offset by one, in the copy.
    !> Judged against the fixture's own geometry rather than against `%within_los`, so the copy is
    !> held to an oracle that never saw a buffer.
    subroutine test_los_sweep_separations_follow_pairs(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), zred(:), d(:), x(:), y(:), z(:), bp(:), bl(:), dp(:), dl(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, k, nwant
        real(real64) :: ref

        call make_redshift_survey(200_int64, 40_int64, 0.02_real64, 0.2_real64, 7_int64, ra, dec, zred, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            bp(a) = 10.0_real64 + 20.0_real64 * pf_random_at(fixture_seed, a, 39_int64)
            bl(a) = 0.003_real64 + 0.01_real64 * pf_random_at(fixture_seed, a, 40_int64)
        end do
        call sx%build(x, y, z, radius=30.0_real64, los=zred)
        call parquet_debug_set_spatial_sweep_chunk(7_int64)
        call sx%pairs_within_los(bp, bl, pi, pj, combine=PF_LINK_MEAN, dperp=dp, dpar=dl, threads=4)
        call parquet_debug_reset_spatial_counters()
        call brute_los_pairs(ra, dec, d, zred, bp, bl, PF_LINK_MEAN, want, nwant)
        call check(error, nwant > 0_int64 .and. pairs_equal_set(pi, pj, want, nwant), &
            "the chunked, threaded sweep's list must be the oracle's set")
        if (allocated(error)) return
        call check(error, size(dp, kind=int64) == nwant .and. size(dl, kind=int64) == nwant, &
            "one transverse and one parallel separation per pair")
        if (allocated(error)) return
        do k = 1_int64, nwant
            ref = los_dperp(ra(pi(k)), dec(pi(k)), d(pi(k)), ra(pj(k)), dec(pj(k)), d(pj(k)))
            call check(error, abs(dp(k) - ref) <= 1.0e-9_real64 * ref + 1.0e-10_real64, &
                "dperp must be its own pair's transverse separation after the copy")
            if (allocated(error)) return
            call check(error, dl(k) == abs(zred(pi(k)) - zred(pj(k))), &
                "dpar must be its own pair's parallel separation after the copy")
            if (allocated(error)) return
        end do
    end subroutine test_los_sweep_separations_follow_pairs

    !> A chunk boundary is where an emitter can be dropped or swept twice; with the chunk at 1, 2,
    !> 3 and 7, under the union (whose tiebreak reads the rank inside the accept) and the mean
    !> (whose rank screens before it), every emitter is a boundary in one run or another. The set
    !> against the oracle, and the count against the set, so a repeated pair shows as well as a
    !> missing one; the mutation is an off-by-one in a chunk's last emitter.
    subroutine test_los_sweep_chunk_boundaries(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), bp(:), bl(:)
        integer(int64), allocatable :: pi(:), pj(:)
        logical, allocatable :: want(:, :)
        type(pf_spatial_index) :: sx
        integer(int64) :: n, a, nwant
        integer(int64), parameter :: chunks(4) = [1_int64, 2_int64, 3_int64, 7_int64]
        integer, parameter :: rules(2) = [PF_LINK_MAX, PF_LINK_MEAN]
        integer :: ci, ri

        call make_wedge(300_int64, 40_int64, 500.0_real64, 1500.0_real64, 1_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (bp(n), bl(n), want(n, n))
        do a = 1_int64, n
            bp(a) = 10.0_real64 + 10.0_real64 * pf_random_at(fixture_seed, a, 31_int64)
            bl(a) = bp(a) * (5.0_real64 + 25.0_real64 * pf_random_at(fixture_seed, a, 32_int64))
        end do
        call sx%build(x, y, z, radius=sqrt(bp**2 + bl**2))
        do ri = 1, 2
            call brute_los_pairs(ra, dec, d, d, bp, bl, rules(ri), want, nwant)
            call check(error, nwant > 0_int64, "the fixture must hold pairs under each rule")
            if (allocated(error)) return
            do ci = 1, 4
                call parquet_debug_set_spatial_sweep_chunk(chunks(ci))
                call sx%pairs_within_los(bp, bl, pi, pj, combine=rules(ri))
                call parquet_debug_reset_spatial_counters()
                call check(error, pairs_equal_set(pi, pj, want, nwant), &
                    "the pair list must be the oracle's set at every chunk size, under every rule")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_los_sweep_chunk_boundaries

    ! ---- int32 answers: the same answer in the caller's kind ----
    !
    ! Every one of these is a PARITY assertion, element for element, and that is the only thing
    ! that forbids the two kinds drifting apart: an int32 form does not narrow the int64 answer,
    ! it passes `out32=` where the other passes `out64=`, so the two are separate routes through
    ! `spatial_scan` and nothing else would notice one of them emitting differently
    ! (`feature_risks.md`). Element-wise rather than set-wise is right here: both kinds run the
    ! same sweep in the same order within one process, so equality is exact.

    !> `%pairs_within` answers the same in `int32` as in `int64`, on both radius forms.
    subroutine test_pairs_int32_matches_int64(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rr(:)
        integer(int32), allocatable :: i32(:), j32(:)
        integer(int64), allocatable :: i64(:), j64(:)
        type(pf_spatial_index) :: sx

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.12_real64)
        call sx%pairs_within(0.12_real64, i64, j64)
        call sx%pairs_within(0.12_real64, i32, j32)
        call check(error, size(i64, kind=int64) > 0_int64, "the pair fixture must find some pairs")
        if (allocated(error)) return
        call check(error, pairs_same_kinds(i32, j32, i64, j64), &
            "pairs_within must answer the same in int32 as in int64")
        if (allocated(error)) return
        ! The per-point form, with every optional set: a specific that drops `combine=` or
        ! `r_inner=` answers a different question, and only forwarding both makes the two agree.
        allocate (rr(size(x, kind=int64)))
        rr = 0.09_real64
        rr(1:50) = 0.16_real64
        call sx%pairs_within(rr, i64, j64, combine=PF_LINK_MEAN, r_inner=0.02_real64)
        call sx%pairs_within(rr, i32, j32, combine=PF_LINK_MEAN, r_inner=0.02_real64)
        call check(error, size(i64, kind=int64) > 0_int64, "the per-point pair fixture must find some pairs")
        if (allocated(error)) return
        call check(error, pairs_same_kinds(i32, j32, i64, j64), &
            "pairs_within must forward combine= and r_inner= from both kinds")
    end subroutine test_pairs_int32_matches_int64

    !> `%pairs_within_sky` and `%pairs_within_los` answer the same in `int32` as in `int64`.
    subroutine test_pairs_sky_and_los_int32_match(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), x(:), y(:), z(:), los(:), bp(:), bl(:), rd(:)
        integer(int32), allocatable :: i32(:), j32(:)
        integer(int64), allocatable :: i64(:), j64(:), ri(:), rj(:)
        integer(int64) :: a, n
        type(pf_spatial_index) :: sk, sx

        call make_sky(400_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=4.0_real64)
        call sk%pairs_within_sky(4.0_real64, i64, j64)
        call sk%pairs_within_sky(4.0_real64, i32, j32)
        call check(error, size(i64, kind=int64) > 0_int64, "the sky pair fixture must find some pairs")
        if (allocated(error)) return
        call check(error, pairs_same_kinds(i32, j32, i64, j64), &
            "pairs_within_sky must answer the same in int32 as in int64")
        if (allocated(error)) return
        ! The per-point sky radius is a separate specific in each kind, and the annulus is another
        ! arm again. Both must run against the SKY arrays, before the wedge below reassigns them.
        allocate (rd(size(ra, kind=int64)))
        rd = 4.0_real64
        call sk%pairs_within_sky(rd, ri, rj)
        call sk%pairs_within_sky(rd, i32, j32)
        call check(error, size(ri, kind=int64) > 0_int64, "the per-point sky pair fixture must find some pairs")
        if (allocated(error)) return
        call check(error, pairs_same_kinds(i32, j32, ri, rj), &
            "a per-point sky radius must answer the same pairs in int32 as in int64")
        if (allocated(error)) return
        call check(error, size(ri, kind=int64) == size(i64, kind=int64) .and. all(ri == i64), &
            "a uniform per-point sky radius must give the scalar form's pair list")
        if (allocated(error)) return
        call sk%pairs_within_sky(4.0_real64, ri, rj, r_inner_deg=2.0_real64)
        call check(error, size(ri, kind=int64) > 0_int64 .and. size(ri, kind=int64) < size(i64, kind=int64), &
            "a sky annulus must drop the pairs closer than its inner radius")
        if (allocated(error)) return
        ! The line-of-sight sweep reaches `spatial_scan_axis` as well as `spatial_scan`, so an
        ! `out32` forwarded to only one of the two walks would show here and nowhere else.
        call make_wedge(200_int64, 60_int64, 500.0_real64, 1500.0_real64, 4_int64, ra, dec, d, x, y, z)
        n = size(x, kind=int64)
        allocate (los(n), bp(n), bl(n))
        do a = 1_int64, n
            los(a) = d(a)
        end do
        bp = 35.0_real64
        bl = 90.0_real64
        call sx%build(x, y, z, radius=35.0_real64, los=los)
        call sx%pairs_within_los(bp, bl, i64, j64, combine=PF_LINK_MEAN)
        call sx%pairs_within_los(bp, bl, i32, j32, combine=PF_LINK_MEAN)
        call check(error, size(i64, kind=int64) > 0_int64, "the los pair fixture must find some pairs")
        if (allocated(error)) return
        call check(error, pairs_same_kinds(i32, j32, i64, j64), &
            "pairs_within_los must answer the same in int32 as in int64")
        if (allocated(error)) return
        ! One transverse and one parallel length is a separate specific from the per-point pair,
        ! in each kind -- the scalar form takes no `combine=` because with equal lengths every
        ! rule coincides, so it cannot be reached by adding an argument to the call above.
        call sx%pairs_within_los(35.0_real64, 90.0_real64, ri, rj)
        call sx%pairs_within_los(35.0_real64, 90.0_real64, i32, j32)
        call check(error, size(ri, kind=int64) > 0_int64, "the scalar los fixture must find some pairs")
        if (allocated(error)) return
        call check(error, pairs_same_kinds(i32, j32, ri, rj), &
            "scalar los lengths must answer the same in int32 as in int64")
        if (allocated(error)) return
        call check(error, size(ri, kind=int64) == size(i64, kind=int64) .and. all(ri == i64) .and. all(rj == j64), &
            "uniform length vectors under PF_LINK_MEAN must give the scalar form's pair list")
        if (allocated(error)) return
    end subroutine test_pairs_sky_and_los_int32_match

    !> `%all_within` and `%all_within_sky` answer the same CSR in `int32` as in `int64`.
    subroutine test_all_within_int32_matches(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rr(:), rd(:), ra(:), dec(:)
        integer(int32), allocatable :: o32(:), n32(:)
        integer(int64), allocatable :: o64(:), n64(:)
        type(pf_spatial_index) :: sx, sk

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.12_real64)
        call sx%all_within(0.12_real64, o64, n64)
        call sx%all_within(0.12_real64, o32, n32)
        call check(error, size(n64, kind=int64) > 0_int64, "the CSR fixture must find some neighbours")
        if (allocated(error)) return
        call check(error, csr_same_kinds(o32, n32, o64, n64), &
            "all_within must answer the same CSR in int32 as in int64")
        if (allocated(error)) return
        ! `sorted=` orders each row, which is a second route through the walk's tail.
        allocate (rr(size(x, kind=int64)))
        rr = 0.10_real64
        rr(1:50) = 0.18_real64
        call sx%all_within(rr, o64, n64, sorted=.true.)
        call sx%all_within(rr, o32, n32, sorted=.true.)
        call check(error, csr_same_kinds(o32, n32, o64, n64), &
            "all_within must answer the same sorted CSR in both kinds")
        if (allocated(error)) return
        ! `r_inner=` is its own arm in every one of these specifics rather than a flag inside one
        ! body, so an annulus reaches code the plain form never touches -- in both kinds.
        call sx%all_within(0.12_real64, o64, n64, r_inner=0.06_real64)
        call sx%all_within(0.12_real64, o32, n32, r_inner=0.06_real64)
        call check(error, size(n64, kind=int64) > 0_int64, "the annulus fixture must find some neighbours")
        if (allocated(error)) return
        call check(error, csr_same_kinds(o32, n32, o64, n64), &
            "all_within must answer the same annulus CSR in both kinds")
        if (allocated(error)) return
        call make_sky(400_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=4.0_real64)
        call sk%all_within_sky(4.0_real64, o64, n64)
        call sk%all_within_sky(4.0_real64, o32, n32)
        call check(error, size(n64, kind=int64) > 0_int64, "the sky CSR fixture must find some neighbours")
        if (allocated(error)) return
        call check(error, csr_same_kinds(o32, n32, o64, n64), &
            "all_within_sky must answer the same CSR in both kinds")
        if (allocated(error)) return
        call sk%all_within_sky(4.0_real64, o64, n64, r_inner_deg=2.0_real64)
        call sk%all_within_sky(4.0_real64, o32, n32, r_inner_deg=2.0_real64)
        call check(error, csr_same_kinds(o32, n32, o64, n64), &
            "all_within_sky must answer the same annulus CSR in both kinds")
        if (allocated(error)) return
        ! The per-point-radius sky form is a SEPARATE specific from the scalar one, with its own
        ! degrees-to-chords conversion of the whole vector; a uniform vector is what pins the two
        ! against each other, exactly as the Euclidean pair test does.
        allocate (rd(size(ra, kind=int64)))
        rd = 4.0_real64
        call sk%all_within_sky(rd, o64, n64)
        call sk%all_within_sky(rd, o32, n32)
        call check(error, csr_same_kinds(o32, n32, o64, n64), &
            "a per-point sky radius must answer the same CSR in both kinds")
        if (allocated(error)) return
        call sk%all_within_sky(4.0_real64, o64, n64)
        call check(error, csr_same_kinds(o32, n32, o64, n64), &
            "a uniform per-point sky radius must give the scalar form's CSR")
        if (allocated(error)) return
        call sk%all_within_sky(rd, o64, n64, r_inner_deg=[2.0_real64])
        call sk%all_within_sky(rd, o32, n32, r_inner_deg=[2.0_real64])
        call check(error, csr_same_kinds(o32, n32, o64, n64), &
            "a per-point sky annulus must answer the same CSR in both kinds")
    end subroutine test_all_within_int32_matches

    !> `%count_all_within` and `%count_all_within_sky` answer the same counts in both kinds.
    subroutine test_count_all_int32_matches(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rr(:), rd(:), ra(:), dec(:)
        integer(int32), allocatable :: c32(:)
        integer(int64), allocatable :: c64(:), ref(:)
        type(pf_spatial_index) :: sx, sk

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.12_real64)
        call sx%count_all_within(0.12_real64, c64)
        call sx%count_all_within(0.12_real64, c32)
        call check(error, sum(c64) > 0_int64, "the counts fixture must find some neighbours")
        if (allocated(error)) return
        call check(error, size(c32, kind=int64) == size(c64, kind=int64), &
            "count_all_within must answer the same length in both kinds")
        if (allocated(error)) return
        call check(error, all(int(c32, kind=int64) == c64), &
            "count_all_within must answer the same counts in both kinds")
        if (allocated(error)) return
        ref = c64
        ! The per-point-radius form is a separate specific in each kind; a uniform vector is what
        ! pins it against the scalar one, and the annulus is a third arm again.
        allocate (rr(size(x, kind=int64)))
        rr = 0.12_real64
        call sx%count_all_within(rr, c64)
        call sx%count_all_within(rr, c32)
        call check(error, all(c64 == ref), "a uniform radius vector must give the scalar form's counts")
        if (allocated(error)) return
        call check(error, all(int(c32, kind=int64) == c64), &
            "a per-point radius must answer the same counts in both kinds")
        if (allocated(error)) return
        call sx%count_all_within(0.12_real64, c64, r_inner=0.06_real64)
        call sx%count_all_within(0.12_real64, c32, r_inner=0.06_real64)
        call check(error, all(int(c32, kind=int64) == c64) .and. sum(c64) > 0_int64, &
            "count_all_within must answer the same annulus counts in both kinds")
        if (allocated(error)) return
        call check(error, all(c64 <= ref) .and. any(c64 < ref), &
            "an annulus must drop the points inside the inner radius and nothing else")
        if (allocated(error)) return
        call make_sky(400_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=4.0_real64)
        call sk%count_all_within_sky(4.0_real64, c64)
        call sk%count_all_within_sky(4.0_real64, c32)
        call check(error, all(int(c32, kind=int64) == c64) .and. sum(c64) > 0_int64, &
            "count_all_within_sky must answer the same counts in both kinds")
        if (allocated(error)) return
        ref = c64
        allocate (rd(size(ra, kind=int64)))
        rd = 4.0_real64
        call sk%count_all_within_sky(rd, c64)
        call sk%count_all_within_sky(rd, c32)
        call check(error, all(c64 == ref), "a uniform sky radius vector must give the scalar form's counts")
        if (allocated(error)) return
        call check(error, all(int(c32, kind=int64) == c64), &
            "a per-point sky radius must answer the same counts in both kinds")
        if (allocated(error)) return
        call sk%count_all_within_sky(4.0_real64, c64, r_inner_deg=2.0_real64)
        call sk%count_all_within_sky(4.0_real64, c32, r_inner_deg=2.0_real64)
        call check(error, all(int(c32, kind=int64) == c64) .and. sum(c64) > 0_int64, &
            "count_all_within_sky must answer the same annulus counts in both kinds")
        if (allocated(error)) return
        call sk%count_all_within_sky(rd, c64, r_inner_deg=[2.0_real64])
        call sk%count_all_within_sky(rd, c32, r_inner_deg=[2.0_real64])
        call check(error, all(int(c32, kind=int64) == c64), &
            "a per-point sky annulus must answer the same counts in both kinds")
    end subroutine test_count_all_int32_matches

    !> The whole Friends-of-Friends chain in `int32` gives the `int64` chain's answer.
    subroutine test_fof_chain_int32(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int32), allocatable :: i32(:), j32(:), lab32(:), sz32(:)
        integer(int64), allocatable :: i64(:), j64(:), lab64(:), sz64(:)
        integer(int32) :: nc32
        integer(int64) :: nc64
        type(pf_spatial_index) :: sx

        call make_clustered_cloud(600_int64, x, y, z)
        call sx%build(x, y, z, radius=0.05_real64)
        call sx%pairs_within(0.05_real64, i64, j64)
        call pf_connected_components(i64, j64, sx%size(), lab64, ncomp=nc64, sizes=sz64, min_size=2)
        call sx%pairs_within(0.05_real64, i32, j32)
        call pf_connected_components(i32, j32, int(sx%size(), kind=int32), lab32, ncomp=nc32, &
                                     sizes=sz32, min_size=2)
        call check(error, nc64 > 1_int64, "the group fixture must find more than one group")
        if (allocated(error)) return
        call check(error, int(nc32, kind=int64) == nc64, "the two chains must find the same number of groups")
        if (allocated(error)) return
        call check(error, all(int(lab32, kind=int64) == lab64), &
            "the two chains must label every vertex the same; the numbering is a contract")
        if (allocated(error)) return
        call check(error, all(int(sz32, kind=int64) == sz64), "the two chains must report the same group sizes")
    end subroutine test_fof_chain_int32

    !> `pf_connected_components` over an `int32` edge list, with either `nvert` kind.
    subroutine test_components_int32_edges(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        integer(int32) :: ei(4), ej(4), nc32
        integer(int64) :: ei64(4), ej64(4), nc64
        integer(int32), allocatable :: lab32(:), sz32(:), lab32b(:)
        integer(int64), allocatable :: lab64(:), sz64(:)

        ! Two components among the first five vertices, and vertices 6..9 isolated -- which is the
        ! case `nvert` exists for, and the one a vertex count taken from the edge list would lose.
        ei = [1_int32, 2_int32, 4_int32, 4_int32]
        ej = [2_int32, 3_int32, 5_int32, 5_int32]
        ei64 = int(ei, kind=int64)
        ej64 = int(ej, kind=int64)
        call pf_connected_components(ei, ej, 9_int32, lab32, ncomp=nc32, sizes=sz32, min_size=2)
        call pf_connected_components(ei64, ej64, 9_int64, lab64, ncomp=nc64, sizes=sz64, min_size=2)
        call check(error, size(lab32, kind=int64) == 9_int64, "labels must be length nvert, isolated vertices included")
        if (allocated(error)) return
        call check(error, int(nc32, kind=int64) == nc64 .and. nc64 == 2_int64, "min_size=2 must find exactly two groups")
        if (allocated(error)) return
        call check(error, all(int(lab32, kind=int64) == lab64), "an int32 edge list must label as the int64 one does")
        if (allocated(error)) return
        call check(error, all(int(sz32, kind=int64) == sz64), "an int32 edge list must size its groups as int64 does")
        if (allocated(error)) return
        call check(error, lab32(6) == 0_int32 .and. lab32(9) == 0_int32, "an isolated vertex must be labelled 0")
        if (allocated(error)) return
        ! The same edge list against an int64 `nvert`: the two kinds are independent by design.
        call pf_connected_components(ei, ej, 9_int64, lab32b, min_size=2)
        call check(error, all(lab32b == lab32), "nvert's kind must not change the answer")
    end subroutine test_components_int32_edges

    !> `%grid` answers the same cell counts in both kinds, and the ceiling hook is reversible.
    subroutine test_grid_int32_and_ceiling_hook(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        integer(int32) :: nx32, ny32, nz32
        integer(int64) :: nx64, ny64, nz64
        integer(int32), allocatable :: i32(:), j32(:)
        type(pf_spatial_index) :: sx

        call make_cloud(400_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.12_real64)
        call sx%grid(nx64, ny64, nz64)
        call sx%grid(nx32, ny32, nz32)
        call check(error, nx64 > 1_int64, "the grid fixture must have more than one cell along x")
        if (allocated(error)) return
        call check(error, int(nx32, kind=int64) == nx64 .and. int(ny32, kind=int64) == ny64 .and. &
                   int(nz32, kind=int64) == nz64, "grid must answer the same counts in both kinds")
        if (allocated(error)) return
        ! A ceiling above the fixture changes nothing, and the reset puts the real one back. That
        ! the hook can also make an int32 answer ABORT is what the error scenarios show: an abort
        ! cannot be observed in process, and leaving the hook lowered here would leak into every
        ! later test in this suite.
        call parquet_debug_set_spatial_int32_ceiling(1000_int64)
        call sx%pairs_within(0.12_real64, i32, j32)
        call check(error, size(i32, kind=int64) > 0_int64, "a ceiling above the row count must refuse nothing")
        if (allocated(error)) return
        call parquet_debug_reset_spatial_counters()
        call sx%grid(nx32, ny32, nz32)
        call check(error, int(nx32, kind=int64) == nx64, "resetting the counters must restore the real ceiling")
    end subroutine test_grid_int32_and_ceiling_hook

    !> Whether two pair lists agree element for element across the two kinds.
    !> Every single-point query family answers the same into an `int32` buffer as into an `int64` one.
    !>
    !> Each kind is a SEPARATE specific that forwards to the same walk, so a specific wired to the
    !> wrong output argument -- or to the wrong walk -- produces a wrong answer rather than a build
    !> error. `%within_segment` and `%within_cone` reach `spatial_scan_axis` while the others reach
    !> `spatial_scan`, which is why both walks appear here.
    subroutine test_single_query_int32_buffers_match(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), ra(:), dec(:)
        integer(int32) :: g32(64)
        integer(int64) :: g64(64), m32, m64
        real(real64) :: d32(64), d64(64)
        type(pf_spatial_index) :: sx, sk
        real(real64), parameter :: p1(3) = [0.2_real64, 0.2_real64, 0.2_real64]
        real(real64), parameter :: p2(3) = [0.8_real64, 0.8_real64, 0.8_real64]

        call make_cloud(300_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.15_real64)
        m64 = sx%within_segment(p1, p2, 0.15_real64, g64, dist=d64)
        m32 = sx%within_segment(p1, p2, 0.15_real64, g32, dist=d32)
        call check(error, m64 > 0_int64, "the segment fixture must find some points")
        if (allocated(error)) return
        call check(error, m32 == m64 .and. all(int(g32(1:m64), kind=int64) == g64(1:m64)) &
            .and. all(d32(1:m64) == d64(1:m64)), &
            "within_segment must answer the same rows and distances in both kinds")
        if (allocated(error)) return
        m64 = sx%within_cone(p1, p2, 0.05_real64, 0.20_real64, g64, dist=d64)
        m32 = sx%within_cone(p1, p2, 0.05_real64, 0.20_real64, g32, dist=d32)
        call check(error, m64 > 0_int64, "the cone fixture must find some points")
        if (allocated(error)) return
        call check(error, m32 == m64 .and. all(int(g32(1:m64), kind=int64) == g64(1:m64)) &
            .and. all(d32(1:m64) == d64(1:m64)), &
            "within_cone must answer the same rows and distances in both kinds")
        if (allocated(error)) return
        ! `%nearest` orders its answer, so an int32 specific that lost the ordering shows here.
        m64 = sx%nearest(p1, 8_int32, g64, dist=d64)
        m32 = sx%nearest(p1, 8_int32, g32, dist=d32)
        call check(error, m64 == 8_int64 .and. m32 == m64 &
            .and. all(int(g32(1:m64), kind=int64) == g64(1:m64)), &
            "nearest must answer the same rows in both kinds under an int32 k")
        if (allocated(error)) return
        m32 = sx%nearest(p1, 8_int64, g32, dist=d32)
        call check(error, m32 == m64 .and. all(int(g32(1:m64), kind=int64) == g64(1:m64)), &
            "an int64 k into an int32 buffer must answer as an int32 k into an int64 one")
        if (allocated(error)) return

        call make_sky(300_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=5.0_real64)
        ! At the north pole, where `make_sky` piles a quarter of the catalogue within 3 degrees --
        ! an annulus away from the cap finds nothing on this fixture.
        m64 = sk%within_sky(0.0_real64, 90.0_real64, 3.0_real64, g64, dist_deg=d64, r_inner_deg=1.0_real64)
        m32 = sk%within_sky(0.0_real64, 90.0_real64, 3.0_real64, g32, dist_deg=d32, r_inner_deg=1.0_real64)
        call check(error, m64 > 0_int64, "the sky annulus fixture must find some points")
        if (allocated(error)) return
        call check(error, m32 == m64 .and. all(int(g32(1:m64), kind=int64) == g64(1:m64)) &
            .and. all(d32(1:m64) == d64(1:m64)), &
            "within_sky must answer the same annulus in both kinds")
        if (allocated(error)) return
        ! All four (k kind, buffer kind) combinations of the sky nearest, against one reference.
        m64 = sk%nearest_sky(0.0_real64, 90.0_real64, 6_int32, g64, dist_deg=d64)
        call check(error, m64 == 6_int64, "the sky nearest fixture must return k rows")
        if (allocated(error)) return
        m32 = sk%nearest_sky(0.0_real64, 90.0_real64, 6_int32, g32, dist_deg=d32)
        call check(error, m32 == m64 .and. all(int(g32(1:m64), kind=int64) == g64(1:m64)) &
            .and. all(d32(1:m64) == d64(1:m64)), &
            "nearest_sky must answer the same rows in int32 as in int64 under an int32 k")
        if (allocated(error)) return
        m32 = sk%nearest_sky(0.0_real64, 90.0_real64, 6_int64, g32, dist_deg=d32)
        call check(error, m32 == m64 .and. all(int(g32(1:m64), kind=int64) == g64(1:m64)), &
            "an int64 k into an int32 sky buffer must answer the same rows")
        if (allocated(error)) return
        g64 = 0_int64
        m32 = sk%nearest_sky(0.0_real64, 90.0_real64, 6_int64, g64, dist_deg=d32)
        call check(error, m32 == m64 .and. all(d32(1:m64) == d64(1:m64)), &
            "an int64 k into an int64 sky buffer must answer the same separations")
    end subroutine test_single_query_int32_buffers_match

    !> `%kth_distance_sky` answers the same under an `int64` k as under an `int32` one.
    subroutine test_kth_sky_k64_matches_k32(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), a(:), b(:)
        type(pf_spatial_index) :: sk

        call make_sky(300_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=5.0_real64)
        call sk%kth_distance_sky(3_int32, a)
        call sk%kth_distance_sky(3_int64, b)
        call check(error, size(a, kind=int64) == size(ra, kind=int64), &
            "kth_distance_sky must answer one distance per point")
        if (allocated(error)) return
        call check(error, all(a > 0.0_real64), "the sky fixture must have no coincident points")
        if (allocated(error)) return
        call check(error, all(a == b), "an int64 k must give the int32 k's distances exactly")
    end subroutine test_kth_sky_k64_matches_k32

    !> `%rebuild` folds a `radius=` into the record, in both its scalar and its list form.
    !>
    !> The two shapes are separate specifics, and the scalar one wraps its argument into a
    !> one-element list before handing it on. On UNCHANGED data `%rebuild` folds the radius and
    !> returns without re-tuning, so the cell is not the observable -- the record is, and what
    !> reads the record is the next bulk query's decision whether to re-tune itself. A specific
    !> that dropped its radius would rebuild correctly and simply keep re-tuning later, which no
    !> answer-comparing test can see.
    subroutine test_rebuild_folds_a_radius(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), ra(:), dec(:), rv(:), counts(:)
        integer(int64), allocatable :: cnt(:)
        type(pf_spatial_index) :: sx, sk, sxy
        integer(int64) :: base, scalar_folded, list_folded
        real(real64) :: deg0, deg1
        logical :: rebuilt

        call make_cloud(2000_int64, 1.0_real64, .false., x, y, z)
        ! The control: built for a small radius, a sweep at a far larger one re-tunes the index.
        call parquet_debug_reset_spatial_counters()
        call sx%build(x, y, z, radius=0.02_real64)
        call sx%count_all_within(0.40_real64, cnt)
        base = parquet_debug_spatial_rebuilds()
        call check(error, base > 0_int64, &
            "a sweep far from the built radius must re-tune the index, or this test asserts nothing")
        if (allocated(error)) return

        call parquet_debug_reset_spatial_counters()
        call sx%build(x, y, z, radius=0.02_real64)
        call sx%rebuild(x, y, z, radius=0.40_real64, rebuilt=rebuilt)
        call check(error, .not. rebuilt, "unchanged coordinates must not report a rebuild")
        if (allocated(error)) return
        call sx%count_all_within(0.40_real64, cnt)
        scalar_folded = parquet_debug_spatial_rebuilds()
        call check(error, scalar_folded == 0_int64, &
            "a scalar radius= folded in by %rebuild must spare the next sweep its re-tune")
        if (allocated(error)) return

        call parquet_debug_reset_spatial_counters()
        call sx%build(x, y, z, radius=0.02_real64)
        allocate (rv(2))
        rv = [0.40_real64, 0.40_real64]
        call sx%rebuild(x, y, z, radius=rv)
        call sx%count_all_within(0.40_real64, cnt)
        list_folded = parquet_debug_spatial_rebuilds()
        call check(error, list_folded == 0_int64, &
            "a radius list folded in by %rebuild must spare the next sweep its re-tune")
        if (allocated(error)) return

        ! `%rebuild_for` on a SKY index takes DEGREES and converts the whole list to chords; it
        ! re-tunes outright, so there the cell is the observable.
        call make_sky(1000_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=0.2_real64)
        deg0 = sk%cell_size()
        call sk%rebuild_for([6.0_real64])
        deg1 = sk%cell_size()
        call check(error, deg1 /= deg0, "rebuild_for with a degree list must re-tune a sky index")
        if (allocated(error)) return
        ! A change in Z ALONE must be noticed: the scan compares x, then y, then z, and stops at
        ! the first difference, so a z comparison that was never written would leave the index
        ! silently stale for any move along that axis.
        call sx%build(x, y, z, radius=0.05_real64)
        z(7) = z(7) + 0.3_real64
        call sx%rebuild(x, y, z, rebuilt=rebuilt)
        call check(error, rebuilt, "a change in z alone must be seen as a change")
        if (allocated(error)) return
        ! And a 2D index rebuilds with no z to copy, which is its own arm again.
        call sxy%build(x, y, radius=0.05_real64)
        x(3) = x(3) + 0.2_real64
        call sxy%rebuild(x, y, rebuilt=rebuilt)
        call check(error, rebuilt .and. sxy%ndim() == 2, &
            "a 2D index must rebuild without a z and stay 2D")
        call parquet_debug_reset_spatial_counters()
    end subroutine test_rebuild_folds_a_radius

    !> `parquet_debug_spatial_work` reports the two counts the probe ranks by.
    !>
    !> The hook exists for `bench/benchmark_spatial.sh --mode=ab` to re-fit `A/B` on another
    !> machine, so what it must get right is the TRADE-OFF the fit is over: a larger cell visits
    !> fewer cells and distance-tests more points. Asserting only "it returns something" would
    !> pass against a hook that reported the same pair for every cell size, which is exactly the
    !> failure that would make a re-fit meaningless.
    subroutine test_debug_work_counts_both_halves(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx, unbuilt
        integer(int64) :: c_fine, p_fine, c_coarse, p_coarse

        call parquet_debug_reset_spatial_counters()
        ! An unbuilt index has no points to probe, and the hook answers that rather than walking
        ! an unallocated grid.
        c_fine = 7_int64
        p_fine = 7_int64
        call parquet_debug_spatial_work(unbuilt, 0.1_real64, [0.05_real64], c_fine, p_fine)
        call check(error, c_fine == 0_int64 .and. p_fine == 0_int64, &
            "an unbuilt index must report no cells and no points")
        if (allocated(error)) return

        call make_cloud(3000_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.05_real64)
        call parquet_debug_spatial_work(sx, 0.02_real64, [0.05_real64], c_fine, p_fine)
        call check(error, c_fine > 0_int64 .and. p_fine > 0_int64, &
            "a built index must report a positive count of both cells and points")
        if (allocated(error)) return

        call parquet_debug_spatial_work(sx, 0.40_real64, [0.05_real64], c_coarse, p_coarse)
        call check(error, c_coarse < c_fine, "a larger cell must make the walk visit fewer cells")
        if (allocated(error)) return
        call check(error, p_coarse > p_fine, "a larger cell must make the walk distance-test more points")
        if (allocated(error)) return
        ! A re-fit ranks candidates by re-running this, so the counts must be reproducible: the
        ! probe draws its query points by a fixed stride over stored rows and times nothing.
        call parquet_debug_spatial_work(sx, 0.02_real64, [0.05_real64], c_coarse, p_coarse)
        call check(error, c_coarse == c_fine .and. p_coarse == p_fine, &
            "the same index and h must give the same counts twice")
        call parquet_debug_reset_spatial_counters()
    end subroutine test_debug_work_counts_both_halves

    !> An index over NO points builds and answers every query family with an empty answer.
    !>
    !> Each bulk sweep carries its own `n == 0` arm that emits the empty structure and returns
    !> before the walk, and the tuner has its own: a density it cannot measure must not become a
    !> division by zero. An empty catalogue is a legitimate input -- a cut that happened to select
    !> nothing -- so this is an answer, not a refusal.
    subroutine test_empty_index_answers_everything(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), ra(:), dec(:)
        integer(int64), allocatable :: offs(:), nbrs(:), pi(:), pj(:), cnt(:), got(:)
        integer(int32), allocatable :: o32(:), n32(:)
        type(pf_spatial_index) :: sx, sk
        integer(int64) :: m

        ! `%kth_distance` is deliberately absent: it refuses k above %size()-1, which no k can
        ! satisfy here, so an empty index has no k-th neighbour to report rather than an empty one.
        allocate (x(0), y(0), z(0), got(4))
        call sx%build(x, y, z, radius=0.2_real64)
        call check(error, sx%is_built() .and. sx%size() == 0_int64, &
            "an empty cloud must build into an index holding no points")
        if (allocated(error)) return
        m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 0.2_real64, got)
        call check(error, m == 0_int64, "a query on an empty index must find nothing")
        if (allocated(error)) return
        call sx%all_within(0.2_real64, offs, nbrs)
        call check(error, size(offs, kind=int64) == 1_int64 .and. offs(1) == 1_int64 &
            .and. size(nbrs, kind=int64) == 0_int64, &
            "all_within on an empty index must give the empty CSR")
        if (allocated(error)) return
        call sx%all_within(0.2_real64, o32, n32)
        call check(error, size(o32, kind=int64) == 1_int64 .and. size(n32, kind=int64) == 0_int64, &
            "the int32 CSR of an empty index must be empty too")
        if (allocated(error)) return
        call sx%count_all_within(0.2_real64, cnt)
        call check(error, size(cnt, kind=int64) == 0_int64, &
            "count_all_within on an empty index must give no counts")
        if (allocated(error)) return
        call sx%pairs_within(0.2_real64, pi, pj)
        call check(error, size(pi, kind=int64) == 0_int64 .and. size(pj, kind=int64) == 0_int64, &
            "pairs_within on an empty index must give no pairs")
        if (allocated(error)) return
        allocate (ra(0), dec(0))
        call sk%build_sky(ra, dec, radius_deg=1.0_real64)
        call sk%all_within_sky(1.0_real64, offs, nbrs)
        call check(error, size(nbrs, kind=int64) == 0_int64, &
            "an empty sky index must give the empty CSR too")
        if (allocated(error)) return
        call sk%count_all_within_sky(1.0_real64, cnt)
        call check(error, size(cnt, kind=int64) == 0_int64, &
            "an empty sky index must give no counts")
        if (allocated(error)) return
        ! `%rebuild_for` re-tunes, so it reaches the tuner's own empty arm a second time.
        call sx%rebuild_for([0.5_real64])
        call check(error, sx%size() == 0_int64, "re-tuning an empty index must leave it empty")
    end subroutine test_empty_index_answers_everything

    !> A PERIODIC index answers the same with `copy=.false.` as with `copy=.true.`.
    !>
    !> The minimum-image walk is written twice for the reason the free one is -- see
    !> `test_copy_false_matches` -- and the two halves are different code: the periodic pair
    !> carries the wrap correction, so covering the free pair says nothing about it.
    subroutine test_periodic_copy_false_matches(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable, target :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:), ia(:), ja(:), ib(:), jb(:), ca(:), cb(:)
        type(pf_spatial_index) :: a, b
        integer(int64) :: n, ma, mb, q
        real(real64) :: p(3)
        real(real64), parameter :: lo(3) = [0.0_real64, 0.0_real64, 0.0_real64]
        real(real64), parameter :: hi(3) = [1.0_real64, 1.0_real64, 1.0_real64]

        n = 600_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call a%build(x, y, z, radius=0.15_real64, box_lo=lo, box_hi=hi)
        call b%build(x, y, z, radius=0.15_real64, box_lo=lo, box_hi=hi, copy=.false.)
        call check(error, b%is_periodic(), "the fixture must actually be periodic")
        if (allocated(error)) return
        allocate (got(n), want(n))
        do q = 1_int64, 20_int64
            ! Query points on the faces, where the wrap correction is what decides the answer.
            p(1) = 0.02_real64 + 0.96_real64 * real(mod(q, 2_int64), kind=real64)
            p(2) = pf_random_at(fixture_seed + 57_int64, q, 2_int64)
            p(3) = 0.01_real64
            ma = a%within(p, 0.15_real64, want)
            mb = b%within(p, 0.15_real64, got)
            call check(error, same_rows(got, mb, want, ma), &
                "a periodic copy=.false. index must return exactly the rows a copying one does")
            if (allocated(error)) return
        end do
        call check(error, ma > 0_int64, "the periodic fixture must find some points")
        if (allocated(error)) return
        call a%pairs_within(0.15_real64, ia, ja)
        call b%pairs_within(0.15_real64, ib, jb)
        call check(error, size(ia, kind=int64) > 0_int64 .and. all(ia == ib) .and. all(ja == jb), &
            "a periodic copy=.false. index must answer the same pair list")
        if (allocated(error)) return
        call a%count_all_within(0.15_real64, ca)
        call b%count_all_within(0.15_real64, cb)
        call check(error, all(ca == cb), "a periodic copy=.false. index must answer the same counts")
    end subroutine test_periodic_copy_false_matches

    !> `r_inner=` takes one value or ONE PER POINT, and the per-point form is its own arm.
    !>
    !> A single inner radius is read from element 1 for every row; a full-length vector is indexed
    !> per row. An implementation that read element 1 either way answers correctly for every
    !> uniform vector ever tested, so the fixture here deliberately varies the inner radius.
    subroutine test_per_point_inner_radii(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rr(:), rin(:), ra(:), dec(:), rd(:), din(:)
        integer(int64), allocatable :: c_uniform(:), c_varied(:), offs(:), nbrs(:)
        type(pf_spatial_index) :: sx, sk
        integer(int64) :: n

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.20_real64)
        allocate (rr(n), rin(n))
        rr = 0.20_real64
        rin = 0.10_real64
        call sx%count_all_within(rr, c_uniform, r_inner=rin)
        ! Half the rows keep the inner radius and half drop it to zero, so a per-point read and a
        ! read of element 1 cannot agree.
        rin(1:n / 2_int64) = 0.0_real64
        call sx%count_all_within(rr, c_varied, r_inner=rin)
        call check(error, sum(c_uniform) > 0_int64, "the annulus fixture must find some neighbours")
        if (allocated(error)) return
        call check(error, all(c_varied >= c_uniform), &
            "dropping a row's inner radius can only add neighbours to that row")
        if (allocated(error)) return
        call check(error, any(c_varied(1:n / 2_int64) > c_uniform(1:n / 2_int64)), &
            "a per-point inner radius must be read per point, not from element 1")
        if (allocated(error)) return
        call check(error, all(c_varied(n / 2_int64 + 1_int64:) == c_uniform(n / 2_int64 + 1_int64:)), &
            "the rows that kept their inner radius must be unchanged")
        if (allocated(error)) return
        call sx%all_within(rr, offs, nbrs, r_inner=rin)
        call check(error, size(nbrs, kind=int64) == sum(c_varied), &
            "the CSR must hold exactly the neighbours the counts promised")
        if (allocated(error)) return
        ! The sky side converts the whole inner list from degrees, which is its own loop again.
        call make_sky(400_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=4.0_real64)
        allocate (rd(400), din(400))
        rd = 4.0_real64
        din = 2.0_real64
        call sk%count_all_within_sky(rd, c_uniform, r_inner_deg=din)
        din(1:200) = 0.0_real64
        call sk%count_all_within_sky(rd, c_varied, r_inner_deg=din)
        call check(error, sum(c_uniform) > 0_int64, "the sky annulus fixture must find some neighbours")
        if (allocated(error)) return
        call check(error, any(c_varied(1:200) > c_uniform(1:200)) &
            .and. all(c_varied(201:) == c_uniform(201:)), &
            "a per-point inner angular radius must be read per point")
    end subroutine test_per_point_inner_radii

    !> `pf_connected_components` answers an empty graph in the `int32` arm as in the `int64` one.
    subroutine test_components_empty_graph_int32(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        integer(int32), allocatable :: lab32(:), sz32(:)
        integer(int32) :: nc32
        integer(int32) :: none_i(0), none_j(0)

        call pf_connected_components(none_i, none_j, 0_int64, lab32, ncomp=nc32, sizes=sz32)
        call check(error, size(lab32) == 0 .and. nc32 == 0_int32 .and. size(sz32) == 0, &
            "an empty graph must have no vertices, no components and no sizes in int32 too")
        if (allocated(error)) return
        ! The optional outputs are their own arm again: absent, the labels must still come back
        ! allocated and empty rather than unallocated.
        deallocate (lab32)
        call pf_connected_components(none_i, none_j, 0_int32, lab32)
        call check(error, allocated(lab32) .and. size(lab32) == 0, &
            "an empty graph must allocate an empty label array even with no optional outputs")
    end subroutine test_components_empty_graph_int32

    !> `%rebuild_for` re-tunes through every arm the debug hooks and an empty catalogue select.
    !>
    !> Re-tuning has four routes on each backend -- a forced resolution or cell, a measured one,
    !> and the degenerate empty case -- and `%build` reaches only the measured one. A forced value
    !> that `%build` honours but `%rebuild_for` quietly re-measured would show up as a test using
    !> the hook getting a different cell after an unrelated re-tune, which is the kind of defect
    !> that surfaces in a neighbouring test rather than its own.
    subroutine test_retune_honours_every_arm(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), ra(:), dec(:), ra0(:), dec0(:)
        real(real64), allocatable :: ex(:), ey(:), ez(:)
        type(pf_spatial_index) :: sx, sh, empty_grid, empty_sky
        real(real64) :: h

        call parquet_debug_reset_spatial_counters()
        ! A forced CELL must survive a re-tune, and must stop the probe on that route too.
        call make_cloud(2000_int64, 1.0_real64, .false., x, y, z)
        call sx%build(x, y, z, radius=0.05_real64)
        ! 0.25 over a unit box is 64 cells, well inside the 0.3-per-point ceiling that 2000
        ! points allow -- a finer forced cell would be coarsened and this would assert the clamp.
        call parquet_debug_set_spatial_cell(0.25_real64)
        call sx%rebuild_for([0.30_real64])
        h = sx%cell_size()
        call check(error, abs(h - 0.25_real64) <= 1.0e-12_real64, &
            "a forced debug cell must be the cell a re-tune uses")
        if (allocated(error)) return
        call check(error, parquet_debug_spatial_probe_count() == 0_int64, &
            "a forced debug cell must stop the probe on the re-tune route too")
        if (allocated(error)) return
        call parquet_debug_set_spatial_cell(-1.0_real64)

        ! The HEALPix counterpart: a forced RESOLUTION must survive a re-pixelation.
        call make_sky(6000_int64, ra, dec)
        call sh%build_sky(ra, dec, radius_deg=1.0_real64, backend=PF_SKY_HEALPIX)
        call parquet_debug_set_spatial_nside(4_int64)
        call sh%rebuild_for([2.0_real64])
        call check(error, sh%nside() == 4_int64, &
            "a forced debug nside must be the resolution a re-pixelation uses")
        if (allocated(error)) return
        call check(error, parquet_debug_spatial_probe_count() == 0_int64, &
            "a forced debug nside must stop the resolution probe on the re-tune route")
        if (allocated(error)) return
        call parquet_debug_set_spatial_nside(0_int64)

        ! The degenerate arm on both backends: nothing to measure a density from.
        allocate (ex(0), ey(0), ez(0), ra0(0), dec0(0))
        call empty_grid%build(ex, ey, ez, radius=0.2_real64)
        call empty_grid%rebuild_for([0.5_real64])
        call check(error, empty_grid%cell_size() > 0.0_real64, &
            "re-tuning an empty grid index must still leave it a positive cell")
        if (allocated(error)) return
        call empty_sky%build_sky(ra0, dec0, radius_deg=1.0_real64, backend=PF_SKY_HEALPIX)
        call empty_sky%rebuild_for([2.0_real64])
        call check(error, empty_sky%nside() >= 1_int64, &
            "re-pixelating an empty sky index must still leave it a resolution")
        call parquet_debug_reset_spatial_counters()
    end subroutine test_retune_honours_every_arm

    !> The remaining `copy=.false.` routes: a per-point radius, a 2D build, and the ball-walk
    !> line-of-sight accept under every `combine=` rule.
    !>
    !> `test_copy_false_matches` reaches the indirect loops with a SCALAR radius and the default
    !> rule, which leaves three whole blocks of each loop untouched: the per-candidate bound a
    !> per-point radius switches on, the `select case (lrule)` accept, and -- because the
    !> line-of-sight sweep prefers the cylinder walk -- everything the ball walk would have done
    !> instead. The walk override is what makes the last of those reachable at all.
    subroutine test_copy_false_reaches_every_accept(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable, target :: x(:), y(:), z(:), wx(:), wy(:), wz(:), los(:)
        real(real64), allocatable :: ra(:), dec(:), d(:), rv(:), bp(:), bl(:)
        integer(int64), allocatable :: ia(:), ja(:), ib(:), jb(:), oa(:), na(:), ob(:), nb(:)
        type(pf_spatial_index) :: a, b, la, lb, f2, g2
        integer(int64) :: n, nw, k
        integer, parameter :: rules(4) = [PF_LINK_MAX, PF_LINK_MIN, PF_LINK_MEAN, PF_LINK_SUM]

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        do k = 1_int64, n
            rv(k) = 0.08_real64 + 0.10_real64 * pf_random_at(fixture_seed + 71_int64, k, 5_int64)
        end do
        ! A PER-POINT radius switches on the per-candidate bound, which is its own block in every
        ! one of the four inner loops -- free and periodic, direct and indirect.
        call a%build(x, y, z, radius=0.18_real64)
        call b%build(x, y, z, radius=0.18_real64, copy=.false.)
        call a%pairs_within(rv, ia, ja)
        call b%pairs_within(rv, ib, jb)
        call check(error, size(ia, kind=int64) > 0_int64 .and. all(ia == ib) .and. all(ja == jb), &
            "a copy=.false. index must rank a per-point radius as a copying one does")
        if (allocated(error)) return
        call a%all_within(rv, oa, na, sorted=.true.)
        call b%all_within(rv, ob, nb, sorted=.true.)
        call check(error, all(oa == ob) .and. all(na == nb), &
            "a copy=.false. index must sort a per-point CSR as a copying one does")
        if (allocated(error)) return
        ! A 2D index with copy=.false. has no z to point at and must make its own.
        call f2%build(x, y, radius=0.18_real64)
        call g2%build(x, y, radius=0.18_real64, copy=.false.)
        call check(error, g2%ndim() == 2, "the fixture must build a 2D index")
        if (allocated(error)) return
        call f2%pairs_within(0.18_real64, ia, ja)
        call g2%pairs_within(0.18_real64, ib, jb)
        call check(error, size(ia, kind=int64) > 0_int64 .and. all(ia == ib) .and. all(ja == jb), &
            "a 2D copy=.false. index must answer as a copying one does")
        if (allocated(error)) return

        ! The line-of-sight accept, forced onto the BALL walk so it runs inside `spatial_scan`
        ! rather than the cylinder walk, under each rule in turn.
        call make_wedge(120_int64, 30_int64, 500.0_real64, 1500.0_real64, 7_int64, ra, dec, d, wx, wy, wz)
        nw = size(wx, kind=int64)
        allocate (los(nw), bp(nw), bl(nw))
        los = d
        do k = 1_int64, nw
            bp(k) = 20.0_real64 + 20.0_real64 * pf_random_at(fixture_seed + 73_int64, k, 6_int64)
            bl(k) = 60.0_real64 + 40.0_real64 * pf_random_at(fixture_seed + 73_int64, k, 7_int64)
        end do
        call la%build(wx, wy, wz, radius=40.0_real64, los=los)
        call lb%build(wx, wy, wz, radius=40.0_real64, los=los, copy=.false.)
        call parquet_debug_set_spatial_los_walk(walk_ball)
        do k = 1_int64, 4_int64
            call la%pairs_within_los(bp, bl, ia, ja, combine=rules(k))
            call lb%pairs_within_los(bp, bl, ib, jb, combine=rules(k))
            call check(error, size(ia, kind=int64) > 0_int64, &
                "each combine= rule must find some pairs on the ball walk")
            if (allocated(error)) exit
            call check(error, all(ia == ib) .and. all(ja == jb), &
                "a copy=.false. index must apply every combine= rule as a copying one does")
            if (allocated(error)) exit
        end do
        call parquet_debug_set_spatial_los_walk(walk_auto)
        if (allocated(error)) return
        ! `%within_los` takes the lrule == 0 arm of the same accept, on the ball walk too.
        call parquet_debug_set_spatial_los_walk(walk_ball)
        block
            integer(int64) :: fa(2000), fb(2000), ma, mb
            ma = la%within_los([wx(1), wy(1), wz(1)], bp(1), bl(1), fa, los_p=los(1))
            mb = lb%within_los([wx(1), wy(1), wz(1)], bp(1), bl(1), fb, los_p=los(1))
            call check(error, ma > 0_int64 .and. ma == mb .and. same_rows(fb, mb, fa, ma), &
                "a copy=.false. index must answer within_los on the ball walk as a copying one does")
        end block
        call parquet_debug_set_spatial_los_walk(walk_auto)
    end subroutine test_copy_false_reaches_every_accept

    !> A PERIODIC index ranks a per-point radius the same with `copy=.false.` as without.
    subroutine test_periodic_per_point_radius(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable, target :: x(:), y(:), z(:)
        real(real64), allocatable :: rv(:)
        integer(int64), allocatable :: ia(:), ja(:), ib(:), jb(:)
        type(pf_spatial_index) :: a, b
        integer(int64) :: n, k
        real(real64), parameter :: lo(3) = [0.0_real64, 0.0_real64, 0.0_real64]
        real(real64), parameter :: hi(3) = [1.0_real64, 1.0_real64, 1.0_real64]

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        do k = 1_int64, n
            rv(k) = 0.06_real64 + 0.08_real64 * pf_random_at(fixture_seed + 79_int64, k, 8_int64)
        end do
        call a%build(x, y, z, radius=0.14_real64, box_lo=lo, box_hi=hi)
        call b%build(x, y, z, radius=0.14_real64, box_lo=lo, box_hi=hi, copy=.false.)
        call a%pairs_within(rv, ia, ja)
        call b%pairs_within(rv, ib, jb)
        call check(error, size(ia, kind=int64) > 0_int64, "the periodic per-point fixture must find some pairs")
        if (allocated(error)) return
        call check(error, all(ia == ib) .and. all(ja == jb), &
            "a periodic copy=.false. index must rank a per-point radius as a copying one does")
    end subroutine test_periodic_per_point_radius

    !> The last few single-route branches: a 2D nearest, a sorted int32 tie, a sorted axis query,
    !> an annulus whose inner radii are per point against ONE outer radius, and an empty
    !> line-of-sight sweep.
    subroutine test_remaining_single_routes(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:), rin(:), ra(:), dec(:), rd(:), din(:)
        real(real64), allocatable :: ex(:), ey(:), ez(:), elos(:)
        integer(int64), allocatable :: c1(:), c2(:), pi(:), pj(:), g64(:)
        integer(int32), allocatable :: t32(:)
        real(real64), allocatable :: dd(:)
        type(pf_spatial_index) :: sx, flat, sk, empty
        integer(int64) :: n, m, k

        n = 400_int64
        call make_cloud(n, 1.0_real64, .true., x, y, z)
        ! The expanding ball scales its first radius by the DIMENSION of the cloud, and a flat
        ! one takes the disc constant rather than the sphere's.
        call flat%build(x, y, radius=0.10_real64)
        allocate (g64(n), dd(n))
        m = flat%nearest([0.5_real64, 0.5_real64], 10_int64, g64, dist=dd)
        call check(error, m == 10_int64, "a 2D nearest must return k rows")
        if (allocated(error)) return
        do k = 2_int64, m
            call check(error, dd(k) >= dd(k - 1_int64), "a 2D nearest must come back ordered")
            if (allocated(error)) return
        end do
        ! An exact tie broken by row index, reported into an INT32 buffer: the tie-break reads the
        ! rows back out of whichever buffer is in use, and the int32 read is its own line.
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        x(1:4) = 0.25_real64
        y(1:4) = 0.25_real64
        z(1:4) = 0.25_real64
        call sx%build(x, y, z, radius=0.20_real64)
        allocate (t32(n))
        m = sx%within([0.25_real64, 0.25_real64, 0.25_real64], 0.20_real64, t32, dist=dd, sorted=.true.)
        call check(error, m >= 4_int64, "the tie fixture must find its four coincident points")
        if (allocated(error)) return
        call check(error, all(t32(1:4) == [1_int32, 2_int32, 3_int32, 4_int32]), &
            "an exact tie in an int32 buffer must come back in ascending row order")
        if (allocated(error)) return
        ! A sorted AXIS query, which orders through the same machinery from a different caller.
        m = sx%within_segment([0.2_real64, 0.2_real64, 0.2_real64], [0.8_real64, 0.8_real64, 0.8_real64], &
            0.15_real64, g64, dist=dd, sorted=.true.)
        call check(error, m > 1_int64, "the sorted axis fixture must find several points")
        if (allocated(error)) return
        do k = 2_int64, m
            call check(error, dd(k) >= dd(k - 1_int64), "a sorted axis query must come back ordered")
            if (allocated(error)) return
        end do
        ! ONE outer radius against a per-point inner list: the admission check compares the whole
        ! inner list against that single outer value, which is its own arm.
        allocate (rin(n))
        rin = 0.05_real64
        rin(1:n / 2_int64) = 0.0_real64
        call sx%count_all_within(0.20_real64, c1)
        ! ONE outer radius as a one-element list, against a full inner list: the admission check
        ! compares `maxval(inner)` against `radii(1)` there, which no other shape reaches.
        call sx%count_all_within([0.20_real64], c2, r_inner=rin)
        call check(error, sum(c1) > 0_int64 .and. all(c2 <= c1) .and. any(c2 < c1), &
            "a per-point inner list against one outer radius must narrow some rows")
        if (allocated(error)) return
        ! A per-point sky radius WITH a per-point inner list, the one sky specific left.
        call make_sky(400_int64, ra, dec)
        call sk%build_sky(ra, dec, radius_deg=4.0_real64)
        allocate (rd(400), din(400))
        rd = 4.0_real64
        din = 1.5_real64
        call sk%pairs_within_sky(4.0_real64, pi, pj, r_inner_deg=1.5_real64)
        call check(error, size(pi, kind=int64) > 0_int64, "the sky annulus pair fixture must find some pairs")
        if (allocated(error)) return
        ! The per-point-radius sky pair form has its own annulus arm, which converts the inner
        ! radius separately from the outer list.
        call sk%pairs_within_sky(rd, c1, c2, r_inner_deg=1.5_real64)
        call check(error, size(c1, kind=int64) == size(pi, kind=int64) .and. all(c1 == pi), &
            "a uniform per-point sky radius must give the scalar form's annulus pairs")
        if (allocated(error)) return
        ! A sorted query with NO dist= buffer orders through the work array instead, which is a
        ! second route into the ordering and the only one a caller asking for rows alone takes.
        m = sx%within_segment([0.2_real64, 0.2_real64, 0.2_real64], [0.8_real64, 0.8_real64, 0.8_real64], &
            0.15_real64, g64, sorted=.true.)
        call check(error, m > 1_int64, "the work-ordered axis fixture must find several points")
        if (allocated(error)) return
        m = sx%within([0.25_real64, 0.25_real64, 0.25_real64], 0.20_real64, g64, sorted=.true.)
        call check(error, m >= 4_int64 .and. all(g64(1:4) == [1_int64, 2_int64, 3_int64, 4_int64]), &
            "a work-ordered ball query must still break its ties by row index")
        if (allocated(error)) return
        ! A line-of-sight sweep over NO points, which has its own empty arm.
        allocate (ex(0), ey(0), ez(0), elos(0))
        call empty%build(ex, ey, ez, radius=10.0_real64, los=elos)
        call empty%pairs_within_los(5.0_real64, 10.0_real64, pi, pj)
        call check(error, size(pi, kind=int64) == 0_int64 .and. size(pj, kind=int64) == 0_int64, &
            "a line-of-sight sweep over no points must give no pairs")
    end subroutine test_remaining_single_routes

    !> The per-candidate BOUND and the line-of-sight accept, in the loops only a `copy=.false.` or
    !> periodic index reaches.
    !>
    !> Two mechanisms are each armed by something narrower than "a per-point radius":
    !>
    !> * the bound is built only for `PF_LINK_MEAN` and `PF_LINK_SUM` -- `PF_LINK_MAX` and
    !>   `PF_LINK_MIN` need no per-candidate term, because the ranking already puts the endpoint
    !>   holding the deciding radius in the searcher's seat -- so the default rule never reaches
    !>   it however the radii vary;
    !> * the union's emit-once tiebreak is armed by the line-of-sight sweep's own second pass, so
    !>   it needs `%pairs_within_los` rather than any single query.
    !>
    !> Each has four copies (free and periodic, direct and indirect) and the ordinary tests reach
    !> one of the four.
    subroutine test_bound_and_los_accept_in_every_loop(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable, target :: x(:), y(:), z(:), wx(:), wy(:), wz(:), los(:)
        real(real64), allocatable :: ra(:), dec(:), d(:), rv(:), bp(:), bl(:)
        integer(int64), allocatable :: ia(:), ja(:), ib(:), jb(:)
        type(pf_spatial_index) :: f, fc, pa, pc, la, lb
        integer(int64) :: n, nw, k, w
        integer, parameter :: rules(4) = [PF_LINK_MAX, PF_LINK_MIN, PF_LINK_MEAN, PF_LINK_SUM]
        integer, parameter :: walks(2) = [walk_ball, walk_cyl]
        real(real64), parameter :: lo(3) = [0.0_real64, 0.0_real64, 0.0_real64]
        real(real64), parameter :: hi(3) = [1.0_real64, 1.0_real64, 1.0_real64]

        n = 500_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        allocate (rv(n))
        do k = 1_int64, n
            rv(k) = 0.05_real64 + 0.09_real64 * pf_random_at(fixture_seed + 83_int64, k, 9_int64)
        end do
        ! PF_LINK_MEAN is what builds the bound. Free, indirect.
        call f%build(x, y, z, radius=0.15_real64)
        call fc%build(x, y, z, radius=0.15_real64, copy=.false.)
        call f%pairs_within(rv, ia, ja, combine=PF_LINK_MEAN)
        call fc%pairs_within(rv, ib, jb, combine=PF_LINK_MEAN)
        call check(error, size(ia, kind=int64) > 0_int64, "the mean-rule fixture must find some pairs")
        if (allocated(error)) return
        call check(error, all(ia == ib) .and. all(ja == jb), &
            "a copy=.false. index must apply the per-candidate bound as a copying one does")
        if (allocated(error)) return
        ! Periodic, direct and indirect.
        call pa%build(x, y, z, radius=0.15_real64, box_lo=lo, box_hi=hi)
        call pc%build(x, y, z, radius=0.15_real64, box_lo=lo, box_hi=hi, copy=.false.)
        call pa%pairs_within(rv, ia, ja, combine=PF_LINK_MEAN)
        call pc%pairs_within(rv, ib, jb, combine=PF_LINK_MEAN)
        call check(error, size(ia, kind=int64) > 0_int64, "the periodic mean-rule fixture must find some pairs")
        if (allocated(error)) return
        call check(error, all(ia == ib) .and. all(ja == jb), &
            "a periodic copy=.false. index must apply the bound as a copying one does")
        if (allocated(error)) return
        call pa%pairs_within(rv, ia, ja, combine=PF_LINK_SUM)
        call pc%pairs_within(rv, ib, jb, combine=PF_LINK_SUM)
        call check(error, all(ia == ib) .and. all(ja == jb), &
            "the sum rule must agree across copy= on a periodic index too")
        if (allocated(error)) return

        ! The line-of-sight accept and the union tiebreak, on BOTH walks and every rule, against a
        ! copy=.false. index. The cylinder walk is what the sweep chooses on its own; forcing the
        ! ball as well covers the second copy of the same accept.
        call make_wedge(120_int64, 30_int64, 500.0_real64, 1500.0_real64, 11_int64, ra, dec, d, wx, wy, wz)
        nw = size(wx, kind=int64)
        allocate (los(nw), bp(nw), bl(nw))
        los = d
        do k = 1_int64, nw
            bp(k) = 20.0_real64 + 25.0_real64 * pf_random_at(fixture_seed + 89_int64, k, 3_int64)
            bl(k) = 50.0_real64 + 50.0_real64 * pf_random_at(fixture_seed + 89_int64, k, 4_int64)
        end do
        call la%build(wx, wy, wz, radius=45.0_real64, los=los)
        call lb%build(wx, wy, wz, radius=45.0_real64, los=los, copy=.false.)
        do w = 1_int64, 2_int64
            call parquet_debug_set_spatial_los_walk(walks(w))
            do k = 1_int64, 4_int64
                call la%pairs_within_los(bp, bl, ia, ja, combine=rules(k))
                call lb%pairs_within_los(bp, bl, ib, jb, combine=rules(k))
                call check(error, size(ia, kind=int64) > 0_int64, &
                    "each rule must find some pairs on each walk")
                if (allocated(error)) exit
                call check(error, all(ia == ib) .and. all(ja == jb), &
                    "a copy=.false. index must apply every rule on both walks as a copying one does")
                if (allocated(error)) exit
            end do
            if (allocated(error)) exit
        end do
        call parquet_debug_set_spatial_los_walk(walk_auto)
        if (allocated(error)) return
        ! The separations come back through the same accept, and their normalisation is its own
        ! line in each loop: `dist` under a line-of-sight query is the larger of the two ratios.
        block
            real(real64), allocatable :: dn(:), dp(:), dl(:)
            integer(int64), allocatable :: fa(:), fb(:)
            integer(int64) :: ma, mb
            allocate (dn(nw), dp(nw), dl(nw), fa(nw), fb(nw))
            call parquet_debug_set_spatial_los_walk(walk_ball)
            ma = la%within_los([wx(1), wy(1), wz(1)], bp(1), bl(1), fa, los_p=los(1), &
                dist=dn, dperp=dp, dpar=dl, sorted=.true.)
            mb = lb%within_los([wx(1), wy(1), wz(1)], bp(1), bl(1), fb, los_p=los(1), &
                dist=dn, dperp=dp, dpar=dl, sorted=.true.)
            call check(error, ma > 0_int64 .and. ma == mb .and. all(fa(1:ma) == fb(1:ma)), &
                "a copy=.false. index must order a line-of-sight query as a copying one does")
            call parquet_debug_set_spatial_los_walk(walk_auto)
        end block
    end subroutine test_bound_and_los_accept_in_every_loop

    !> The two line-of-sight routes a `copy=.false.` index reaches only by the walk CHOOSING them.
    !>
    !> Forcing the ball with the debug hook takes a different branch from the one the automatic
    !> choice takes: the forced route sweeps one ball for everything and passes no tiebreak, while
    !> the chosen route passes `los_tiebreak` so the union emits each pair once. So the tiebreak
    !> arm of the ball walk is reachable only through a fixture whose cell genuinely swallows the
    !> covering ball -- and the cylinder's own distance block only through a query that asks for
    !> `dist` on the cylinder route.
    subroutine test_los_chosen_walks_on_a_no_copy_index(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: ra(:), dec(:), d(:), tra(:), tdec(:), td(:), bp(:), bl(:)
        real(real64), allocatable, target :: x(:), y(:), z(:)
        real(real64), allocatable :: dn(:), dp(:), dl(:)
        integer(int64), allocatable :: ia(:), ja(:), ib(:), jb(:), fa(:), fb(:)
        type(pf_spatial_index) :: a, b
        integer(int64) :: n, k, ma, mb
        real(real64) :: bp0, bl0, diam

        bp0 = 12.0_real64
        bl0 = 20.0_real64
        diam = 2.0_real64 * sqrt(bp0 * bp0 + bl0 * bl0)
        call make_wedge(200_int64, 20_int64, 500.0_real64, 1500.0_real64, 26_int64, ra, dec, d, x, y, z)
        allocate (tra(6), tdec(6), td(6))
        do k = 1_int64, 6_int64
            tra(k) = 0.01_real64
            tdec(k) = 0.02_real64
            td(k) = 1.0_real64 + 0.5_real64 * real(k - 1_int64, kind=real64)
        end do
        ra = [ra, tra]
        dec = [dec, tdec]
        d = [d, td]
        n = size(ra, kind=int64)
        x = d * cos(dec) * cos(ra)
        y = d * cos(dec) * sin(ra)
        z = d * sin(dec)
        allocate (bp(n), bl(n), fa(n), fb(n), dn(n), dp(n), dl(n))
        bp = bp0
        bl = bl0
        call parquet_debug_reset_spatial_counters()
        call a%build(x, y, z, radius=bp0, los=d)
        call b%build(x, y, z, radius=bp0, los=d, copy=.false.)
        call check(error, a%cell_size() > diam, &
            "precondition: the cell must exceed the covering ball, or the walk takes the cylinder")
        if (allocated(error)) return
        ! Left on walk_auto deliberately: the point is the route the walk CHOOSES.
        call a%pairs_within_los(bp, bl, ia, ja, combine=PF_LINK_MAX)
        call b%pairs_within_los(bp, bl, ib, jb, combine=PF_LINK_MAX)
        call check(error, size(ia, kind=int64) > 0_int64, "the chosen-ball fixture must find some pairs")
        if (allocated(error)) return
        call check(error, all(ia == ib) .and. all(ja == jb), &
            "a copy=.false. index must break the union tie on the chosen ball as a copying one does")
        if (allocated(error)) return
        ! The cylinder route with `dist=`, whose separation block is its own code again.
        call parquet_debug_set_spatial_los_walk(walk_cyl)
        ma = a%within_los([x(1), y(1), z(1)], bp0, bl0, fa, los_p=d(1), dist=dn, dperp=dp, dpar=dl, sorted=.true.)
        mb = b%within_los([x(1), y(1), z(1)], bp0, bl0, fb, los_p=d(1), dist=dn, dperp=dp, dpar=dl, sorted=.true.)
        call parquet_debug_set_spatial_los_walk(walk_auto)
        call check(error, ma > 0_int64 .and. ma == mb .and. all(fa(1:ma) == fb(1:ma)), &
            "a copy=.false. index must answer the cylinder route with distances as a copying one does")
        call parquet_debug_reset_spatial_counters()
    end subroutine test_los_chosen_walks_on_a_no_copy_index

    logical function pairs_same_kinds(i32, j32, i64, j64) result(same)
        integer(int32), intent(in) :: i32(:) !! lower rows, int32 answer.
        integer(int32), intent(in) :: j32(:) !! higher rows, int32 answer.
        integer(int64), intent(in) :: i64(:) !! lower rows, int64 answer.
        integer(int64), intent(in) :: j64(:) !! higher rows, int64 answer.

        same = size(i32, kind=int64) == size(i64, kind=int64) .and. &
               size(j32, kind=int64) == size(j64, kind=int64)
        if (.not. same) return
        same = all(int(i32, kind=int64) == i64) .and. all(int(j32, kind=int64) == j64)
    end function pairs_same_kinds

    !> Whether two CSR answers agree element for element across the two kinds.
    logical function csr_same_kinds(o32, n32, o64, n64) result(same)
        integer(int32), intent(in) :: o32(:) !! offsets, int32 answer.
        integer(int32), intent(in) :: n32(:) !! neighbours, int32 answer.
        integer(int64), intent(in) :: o64(:) !! offsets, int64 answer.
        integer(int64), intent(in) :: n64(:) !! neighbours, int64 answer.

        same = size(o32, kind=int64) == size(o64, kind=int64) .and. &
               size(n32, kind=int64) == size(n64, kind=int64)
        if (.not. same) return
        same = all(int(o32, kind=int64) == o64) .and. all(int(n32, kind=int64) == n64)
    end function csr_same_kinds

end module test_spatial
