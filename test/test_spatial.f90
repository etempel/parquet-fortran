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
            new_unittest("the same points give the same cell twice", test_probe_is_deterministic), &
            new_unittest("every cell size gives the same answers", test_answers_survive_any_cell), &
            new_unittest("an explicit cell is honoured", test_explicit_cell_is_honoured), &
            new_unittest("the cell count stays under the counting-path ceiling", test_cell_count_clamped), &
            new_unittest("a radius list collapses to its moment ratio", test_effective_radius_moment_ratio), &
            new_unittest("rebuild is a no-op on unchanged data and rebuilds on changed", test_rebuild_detects_change), &
            new_unittest("rebuild_for re-tunes without the caller's arrays", test_rebuild_for_retunes), &
            new_unittest("a distant query radius rebuilds, a near one does not", test_auto_rebuild), &
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

    !> The cell count is clamped so the bucketing keeps `pf_argsort`'s counting fast path.
    subroutine test_cell_count_clamped(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable :: x(:), y(:), z(:)
        type(pf_spatial_index) :: sx
        integer(int64) :: n

        n = 2000_int64
        call make_cloud(n, 1.0_real64, .false., x, y, z)
        ! A cell this fine asks for a million cells for two thousand points; the clamp must
        ! coarsen it rather than build a grid whose bucketing loses the O(n) path.
        call sx%build(x, y, z, radius=0.1_real64, cell=0.01_real64)
        call check(error, sx%cells() <= (3_int64 * n) / 10_int64, &
            "the cell count must be clamped to at most 0.3 cells per point")
        if (allocated(error)) return
        call check(error, sx%cell_size() > 0.01_real64, &
            "a clamped build must report the coarsened cell, not the one that was asked for")
        if (allocated(error)) return
        ! And the clamp must not fire when it is not needed.
        call sx%build(x, y, z, radius=0.1_real64, cell=0.25_real64)
        call check(error, abs(sx%cell_size() - 0.25_real64) <= 1.0e-12_real64, &
            "a cell that needs no clamping must be left exactly as given")
    end subroutine test_cell_count_clamped

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
        logical :: warn_was

        warn_was = parquet_get_spatial_rebuild_warning()
        call parquet_set_spatial_rebuild_warning(.false.)
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

        call parquet_set_spatial_rebuild_warning(warn_was)
        call parquet_debug_reset_spatial_counters()
    end subroutine test_auto_rebuild

    !> A no-copy index answers exactly as a copying one.
    subroutine test_copy_false_matches(error)
        type(error_type), allocatable, intent(out) :: error !! set when an assertion fails.
        real(real64), allocatable, target :: x(:), y(:), z(:)
        integer(int64), allocatable :: got(:), want(:)
        type(pf_spatial_index) :: a, b
        integer(int64) :: n, ma, mb, q
        real(real64) :: p(3)

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
        call check(error, sky%count_within_sky(ra(1), dec(1), 0.0_real64) >= 1_int64, &
            "a zero radius still finds the point itself, which sits at separation zero")
        if (allocated(error)) return
        call check(error, sky%count_within_sky(ra(1), dec(1), 90.0_real64) > 0_int64, &
            "a hemisphere-wide radius must count something rather than nothing")
    end subroutine test_count_within_sky

end module test_spatial
