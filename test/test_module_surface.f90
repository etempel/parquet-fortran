!> Asserts that each Arrow-free entry module exposes, THROUGH THAT MODULE ALONE, every setting its
!> own code reads -- getter and setter both.
!!
!! **This suite's value is in what it does NOT import.** Every other test module in this directory
!! reaches the library through `use parquet`, which re-exports everything and so can never notice a
!! module's own surface shrinking. The visibility requirement behind the module restructuring is a
!! property of what a SINGLE `use` exports, so only a compile against that single import can see it
!! regress -- and it regresses at BUILD time, not as a failed assertion, which is the same way
!! `test_facade_covers_every_layer` (test/test_examples.f90) earns its keep.
!!
!! The rule being asserted, from feature_modules.md's section 4: *a module re-exports, get and set,
!! every knob its own code reads, including the output pair when it can emit.* A user who imports
!! one module to get one capability must be able to configure that capability without also
!! importing `parquet_settings`, which would drag in `parquet_bindings` and with it Arrow.
!!
!! **Do not add a second library import to any module in this file.** A `use parquet` anywhere here
!! silently restores everything and the suite stops testing what it exists for. One module per entry
!! module, each with exactly one library `use` line.
!!
!! Extended at each stage of the restructuring: `parquet_settings_base` today; `parquet_argsort`,
!! `parquet_sorting`, `parquet_strings` and `parquet_sampling` as their re-exports land.

!> `parquet_argsort` alone: the four sorting knobs plus the output pair, get and set.
!!
!! **One library import, and it must stay that way.** Everything this file exists to assert is a
!! property of what a single `use` exports; a second import here silently restores the names and the
!! suite goes on passing while testing nothing.
module test_module_surface_argsort
    use parquet_argsort                ! THE ONLY library import.
    use iso_fortran_env, only : int64
    implicit none
    private
    public :: check_argsort_surface

contains

    !> Round-trips every knob `parquet_argsort`'s own code reads.
    subroutine check_argsort_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first knob that failed, or "".
        character(len=:), allocatable :: tok
        integer :: n_sort
        logical :: radix, counting
        integer(int64) :: n64

        what = ""
        n_sort = parquet_get_sort_threads()
        radix = parquet_get_sort_radix_path()
        counting = parquet_get_sort_counting_path()

        call parquet_set_sort_threads(3)
        if (parquet_get_sort_threads() /= 3) what = "sort_threads"
        call parquet_set_sort_radix_path(.not. radix)
        if (what == "" .and. parquet_get_sort_radix_path() .eqv. radix) what = "sort_radix_path"
        call parquet_set_sort_counting_path(.not. counting)
        if (what == "" .and. parquet_get_sort_counting_path() .eqv. counting) what = "sort_counting_path"
        call parquet_set_sort_counting_bucket_limit(64_int64)
        n64 = parquet_get_sort_counting_bucket_limit()
        if (what == "" .and. n64 /= 64_int64) what = "sort_counting_bucket_limit"
        ! The output pair: `warn_thread_clamp` emits from this tier, so a user of it alone must be
        ! able to silence what it prints.
        call parquet_set_verbosity("silent")
        call parquet_get_verbosity(tok)
        if (what == "" .and. tok /= "silent") what = "verbosity"
        call parquet_set_verbosity("normal")
        call parquet_set_message_stream("stderr")
        call parquet_get_message_stream(tok)
        if (what == "" .and. tok /= "stderr") what = "message_stream"
        call parquet_set_message_stream("stdout")

        call parquet_set_sort_threads(n_sort)
        call parquet_set_sort_radix_path(radix)
        call parquet_set_sort_counting_path(counting)
        call parquet_set_sort_counting_bucket_limit(0_int64)
    end subroutine check_argsort_surface

end module test_module_surface_argsort

!> `parquet_index` alone: the map, the multimap, the pool, and every knob this tier's own code
!> reads.
!!
!! The build's thread cap plus the output pair (its affinity clamp warns through the same channel
!! every other tier does) and the sorting knobs, because `method="sorted"` builds through
!! `pf_argsort` and that sort answers to those rather than to `index_threads`.
module test_module_surface_index
    use parquet_index                  ! THE ONLY library import.
    use iso_fortran_env, only : int64
    implicit none
    private
    public :: check_index_surface

contains

    !> Round-trips every knob `parquet_index`'s own code reads, and exercises all three types.
    subroutine check_index_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        character(len=:), allocatable :: tok
        type(pf_index_map) :: m
        type(pf_index_multimap) :: mm
        type(pf_index_pool) :: p
        integer :: n_index, n_sort

        what = ""
        n_index = parquet_get_index_threads()
        n_sort = parquet_get_sort_threads()

        call parquet_set_index_threads(3)
        if (parquet_get_index_threads() /= 3) what = "index_threads"
        if (what == "" .and. pf_index_threads(1000000_int64) > 3) what = "pf_index_threads"
        ! The sorting knobs, reachable because a sorted build goes through pf_argsort.
        call parquet_set_sort_threads(2)
        if (what == "" .and. parquet_get_sort_threads() /= 2) what = "sort_threads"
        call parquet_set_sort_counting_bucket_limit(64_int64)
        if (what == "" .and. parquet_get_sort_counting_bucket_limit() /= 64_int64) &
            what = "sort_counting_bucket_limit"
        ! The output pair: the affinity clamp emits from this tier.
        call parquet_set_verbosity("silent")
        call parquet_get_verbosity(tok)
        if (what == "" .and. tok /= "silent") what = "verbosity"
        call parquet_set_verbosity("normal")
        call parquet_set_message_stream("stderr")
        call parquet_get_message_stream(tok)
        if (what == "" .and. tok /= "stderr") what = "message_stream"
        call parquet_set_message_stream("stdout")

        ! Both types must be usable from this import alone, which is the half a knob round trip
        ! cannot show: if either type or the component limit stopped being re-exported, this file
        ! would fail to COMPILE, which is how this test earns its keep.
        call m%build([10_int64, 11_int64, 12_int64])
        if (what == "" .and. m%get(11_int64) /= 2_int64) what = "pf_index_map%get"
        if (what == "" .and. m%ncomponents() /= 1) what = "pf_index_map%ncomponents"
        call m%get_method(tok)
        if (what == "" .and. tok /= "direct") what = "pf_index_map%get_method"
        if (what == "" .and. p%get_index() /= 1_int64) what = "pf_index_pool%get_index"
        call mm%build([10_int64, 11_int64, 10_int64])
        if (what == "" .and. mm%count(10_int64) /= 2_int64) what = "pf_index_multimap%count"
        call parquet_debug_set_index_pair_limit(0_int64)
        ! String keys reach this tier through parquet_strings, which this import must carry.
        call m%build(["k1", "k2"])
        if (what == "" .and. m%get("k2") /= 2_int64) what = "pf_index_map%get(string)"
        call mm%build(["a", "b", "a"])
        if (what == "" .and. mm%count("a") /= 2_int64) what = "pf_index_multimap%count(string)"
        call parquet_debug_set_index_string_hash_bits(0)
        if (what == "" .and. pf_index_max_components < 1) what = "pf_index_max_components"

        call parquet_set_index_threads(n_index)
        call parquet_set_sort_threads(n_sort)
        call parquet_set_sort_counting_bucket_limit(0_int64)
    end subroutine check_index_surface

end module test_module_surface_index

!> `parquet_sorting` alone: the same four sorting knobs, reached through the facade tier.
module test_module_surface_sorting
    use parquet_sorting                ! THE ONLY library import.
    use iso_fortran_env, only : int64
    implicit none
    private
    public :: check_sorting_surface

contains

    !> Round-trips the sorting knobs through `use parquet_sorting` alone.
    subroutine check_sorting_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first knob that failed, or "".
        character(len=:), allocatable :: tok
        integer :: n_sort
        logical :: radix, counting
        integer(int64) :: n64

        what = ""
        n_sort = parquet_get_sort_threads()
        radix = parquet_get_sort_radix_path()
        counting = parquet_get_sort_counting_path()

        call parquet_set_sort_threads(5)
        if (parquet_get_sort_threads() /= 5) what = "sort_threads"
        call parquet_set_sort_radix_path(.not. radix)
        if (what == "" .and. parquet_get_sort_radix_path() .eqv. radix) what = "sort_radix_path"
        call parquet_set_sort_counting_path(.not. counting)
        if (what == "" .and. parquet_get_sort_counting_path() .eqv. counting) what = "sort_counting_path"
        call parquet_set_sort_counting_bucket_limit(128_int64)
        n64 = parquet_get_sort_counting_bucket_limit()
        if (what == "" .and. n64 /= 128_int64) what = "sort_counting_bucket_limit"
        call parquet_set_verbosity("silent")
        call parquet_get_verbosity(tok)
        if (what == "" .and. tok /= "silent") what = "verbosity"
        call parquet_set_verbosity("normal")
        call parquet_set_message_stream("stderr")
        call parquet_get_message_stream(tok)
        if (what == "" .and. tok /= "stderr") what = "message_stream"
        call parquet_set_message_stream("stdout")

        call parquet_set_sort_threads(n_sort)
        call parquet_set_sort_radix_path(radix)
        call parquet_set_sort_counting_path(counting)
        call parquet_set_sort_counting_bucket_limit(0_int64)
    end subroutine check_sorting_surface

end module test_module_surface_sorting

!> `parquet_stats` alone: the array-statistics tier reduces an array through one `use`.
!!
!! **It re-exports no setting, and that is the assertion, not an omission.** The rule this file
!! exists for is that a module re-exports every knob its OWN code reads; this tier reads none yet,
!! so what a single import has to deliver is the capability itself. When a threaded reduction or
!! `%print` arrives, the knobs it then reads join this module and this check grows with them.
!!
!! **One library import, and it must stay that way.**
module test_module_surface_stats
    use parquet_stats                  ! THE ONLY library import.
    use iso_fortran_env, only : int64, real64
    implicit none
    private
    public :: check_stats_surface

contains

    !> Reduces an array through `use parquet_stats` alone, exercising each exclusion class.
    subroutine check_stats_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        real(real64) :: v(5), w(5)
        integer(int64) :: n

        what = ""
        v = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
        w = [1.0_real64, 1.0_real64, 0.0_real64, 1.0_real64, 1.0_real64]

        call pf_count_valid(v, n)
        if (n /= 5_int64) what = "pf_count_valid"
        call pf_count_valid(v, n, is_valid=[.true., .false., .true., .true., .false.])
        if (what == "" .and. n /= 3_int64) what = "pf_count_valid(is_valid=)"
        call pf_count_valid(v, n, weights=w)
        if (what == "" .and. n /= 4_int64) what = "pf_count_valid(weights=)"
    end subroutine check_stats_surface

end module test_module_surface_stats

!> `parquet_strings` alone: its thread cap and the output pair it consults.
module test_module_surface_strings
    use parquet_strings                ! THE ONLY library import.
    implicit none
    private
    public :: check_strings_surface

contains

    !> Round-trips the knobs `parquet_strings` reads.
    subroutine check_strings_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first knob that failed, or "".
        character(len=:), allocatable :: tok
        integer :: n

        what = ""
        n = parquet_get_string_threads()
        call parquet_set_string_threads(2)
        if (parquet_get_string_threads() /= 2) what = "string_threads"
        call parquet_set_verbosity("silent")
        call parquet_get_verbosity(tok)
        if (what == "" .and. tok /= "silent") what = "verbosity"
        call parquet_set_verbosity("normal")
        call parquet_set_message_stream("stderr")
        call parquet_get_message_stream(tok)
        if (what == "" .and. tok /= "stderr") what = "message_stream"
        call parquet_set_message_stream("stdout")
        call parquet_set_string_threads(n)
    end subroutine check_strings_surface

end module test_module_surface_strings

!> `parquet_sampling` alone: its two threading knobs.
module test_module_surface_sampling
    use parquet_sampling               ! THE ONLY library import.
    use iso_fortran_env, only : int64
    implicit none
    private
    public :: check_sampling_surface

contains

    !> Round-trips the knobs `parquet_sampling` reads.
    subroutine check_sampling_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first knob that failed, or "".
        integer :: n
        integer(int64) :: floor_was, n64

        what = ""
        n = parquet_get_random_threads()
        floor_was = parquet_get_random_parallel_min_elements()
        call parquet_set_random_threads(2)
        if (parquet_get_random_threads() /= 2) what = "random_threads"
        call parquet_set_random_parallel_min_elements(77_int64)
        n64 = parquet_get_random_parallel_min_elements()
        if (what == "" .and. n64 /= 77_int64) what = "random_parallel_min_elements"
        call parquet_set_random_threads(n)
        call parquet_set_random_parallel_min_elements(floor_was)
    end subroutine check_sampling_surface

end module test_module_surface_sampling

!> `parquet_spatial` alone: its own two knobs, the sorting knobs its bucketing goes through, and
!> the output pair, get and set -- plus a working index, built and queried through this one import.
!!
!! **One library import, and it must stay that way.** A user who imports this module to get
!! neighbour search must be able to cap its threads and silence its rebuild warning without also
!! importing `parquet_settings`, which would drag in `parquet_bindings` and with it Arrow.
!> `parquet_healpix` alone: the output pair, get and set, and the pixelisation itself.
!!
!! **One library import, and it must stay that way.** Everything this file exists to assert is a
!! property of what a single `use` exports; a second import here silently restores the names and the
!! suite goes on passing while testing nothing.
module test_module_surface_healpix
    use parquet_healpix                ! THE ONLY library import.
    use iso_fortran_env, only : int64, real64
    implicit none
    private
    public :: check_healpix_surface

contains

    !> Round-trips the knobs `parquet_healpix` re-exports, then uses the capability itself.
    subroutine check_healpix_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first knob that failed, or "".
        character(len=:), allocatable :: tok
        integer(int64) :: ipix, nlist, listpix(64), npix, other
        integer(int64), allocatable :: alloclist(:)
        real(real64) :: vec(3), th(4), ph(4)
        integer(int64) :: bulkpix(4)
        ! Named rather than written inline at the call: `pf_query_disc`'s centre dummy is
        ! explicit-shape `vec(3)`, so an array constructor is argument-associated through a
        ! temporary, which ifx reports as `warning (406)` under --profile debug. See CLAUDE.md's
        ! ifx-specific gotchas.
        real(real64), parameter :: north(3) = [0.0_real64, 0.0_real64, 1.0_real64]
        type(pf_healpix_grid) :: grid, south

        what = ""
        ! The output pair. This tier reads no knob of its own; it carries these because its bulk
        ! forms clamp a requested thread count and say so through the shared channel, and a program
        ! whose only import is this module must be able to quiet that without naming
        ! parquet_settings -- which would put the C++ boundary back into an Arrow-free build.
        call parquet_set_verbosity("silent")
        call parquet_get_verbosity(tok)
        if (tok /= "silent") what = "verbosity"
        call parquet_set_verbosity("normal")
        call parquet_set_message_stream("stderr")
        call parquet_get_message_stream(tok)
        if (what == "" .and. tok /= "stderr") what = "message_stream"
        call parquet_set_message_stream("stdout")

        ! And the capability itself: a re-export list that compiles while exporting no usable
        ! procedure would satisfy every assertion above. PF_HP_NEST is named deliberately -- the
        ! scheme selectors are separate `public ::` entries and so separately droppable.
        call pf_ang2pix_nest(4_int64, 1.0_real64, 2.0_real64, ipix)
        if (what == "" .and. (ipix < 0_int64 .or. ipix >= 192_int64)) what = "pf_ang2pix_nest"
        call pf_query_disc(4_int64, north, 0.5_real64, listpix, nlist, scheme=PF_HP_NEST)
        if (what == "" .and. nlist <= 0_int64) what = "pf_query_disc"

        ! Tier B, one name per group, for the same reason as above: a re-export list that compiles
        ! while exporting no usable procedure would satisfy every assertion before this point.
        npix = pf_nside2npix(4_int64)
        if (what == "" .and. npix /= 192_int64) what = "pf_nside2npix"
        if (what == "" .and. pf_npix2nside(npix) /= 4_int64) what = "pf_npix2nside"
        call pf_ang2vec(1.0_real64, 2.0_real64, vec)
        call pf_vec2pix_ring(4_int64, vec, ipix)
        if (what == "" .and. (ipix < 0_int64 .or. ipix >= npix)) what = "pf_vec2pix_ring"
        call pf_query_disc_count(4_int64, vec, 0.5_real64, nlist)
        if (what == "" .and. nlist <= 0_int64) what = "pf_query_disc_count"
        call pf_query_disc_alloc(4_int64, vec, 0.5_real64, alloclist, nlist)
        if (what == "" .and. size(alloclist) /= int(nlist)) what = "pf_query_disc_alloc"
        th = [0.5_real64, 1.0_real64, 1.5_real64, 2.0_real64]
        ph = [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64]
        call pf_ang2pix_ring_bulk(4_int64, th, ph, bulkpix, threads=2)
        if (what == "" .and. any(bulkpix < 0_int64)) what = "pf_ang2pix_ring_bulk"

        ! Tier C: the type, one binding from each group, and both frame selectors. The type and
        ! the two selectors are three separate `public ::` entries, so each is separately
        ! droppable -- and a grid whose RA/Dec layer silently used the wrong convention would
        ! still satisfy an assertion that only checked the index was in range, which is why the
        ! two frames are compared against each other rather than against a bound.
        call grid%init(4_int64, PF_HP_NEST)
        if (what == "" .and. .not. grid%is_set()) what = "pf_healpix_grid%init"
        call grid%get_npix(npix)
        if (what == "" .and. npix /= 192_int64) what = "pf_healpix_grid%get_npix"
        call grid%radec2pix(30.0_real64, 40.0_real64, ipix)
        if (what == "" .and. (ipix < 0_int64 .or. ipix >= npix)) what = "pf_healpix_grid%radec2pix"
        call grid%query_disc(vec, 0.5_real64, listpix, nlist)
        if (what == "" .and. nlist <= 0_int64) what = "pf_healpix_grid%query_disc"
        call south%init(4_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
        call south%radec2pix(30.0_real64, -40.0_real64, other)
        if (what == "" .and. other /= ipix) what = "PF_HP_DEC_SOUTH"
        call grid%init(4_int64, PF_HP_NEST, frame=PF_HP_DEC_NORTH)
        call grid%radec2pix(30.0_real64, 40.0_real64, other)
        if (what == "" .and. other /= ipix) what = "PF_HP_DEC_NORTH"
    end subroutine check_healpix_surface

end module test_module_surface_healpix

!> `parquet_sphere` alone: a polygon and its draw, a pixel draw on a grid, a conversion, and the
!> deliberate ABSENCE of any settings re-export.
!!
!! **One library import, and it must stay that way.** The module reads no knob and prints nothing, so
!! what it must carry is the vocabulary its procedures take: the HEALPix scheme and frame selectors and
!! the grid and stream types, without which a program importing only this module could not call half
!! of it. Declaring a `pf_healpix_grid` and a `pf_random_stream` here is what asserts those re-exports.
module test_module_surface_sphere
    use parquet_sphere                 ! THE ONLY library import.
    use iso_fortran_env, only : int64, real64
    implicit none
    private
    public :: check_sphere_surface

contains

    !> Uses one procedure of each family through `use parquet_sphere` alone.
    subroutine check_sphere_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        type(pf_sky_polygon) :: poly
        type(pf_healpix_grid) :: grid
        type(pf_random_stream) :: rng
        real(real64) :: ra, dec, v(3)
        integer(int64) :: ipix
        character(len=:), allocatable :: algo

        what = ""
        call poly%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call poly%random_at(20260917_int64, 1_int64, ra, dec)
        if (.not. poly%contains(ra, dec)) what = "pf_sky_polygon%random_at drew a point outside the polygon"
        call rng%seed(20260917_int64, 1_int64)
        call poly%random_next(rng, ra, dec)
        if (what == "" .and. rng%position() /= 5_int64) what = "pf_sky_polygon%random_next did not take one block"

        ! The scheme and frame selectors are separate re-exports, so each is named.
        call grid%init(8_int64, PF_HP_RING)
        v = pf_random_pixel_at(grid, 20260917_int64, 1_int64, 100_int64)
        call grid%vec2pix(v, ipix)
        if (what == "" .and. ipix /= 100_int64) what = "pf_random_pixel_at drew a point outside its pixel"
        call grid%init(8_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
        if (what == "" .and. grid%frame() /= PF_HP_DEC_SOUTH) what = "PF_HP_NEST and PF_HP_DEC_SOUTH"

        ! The conversions against values the geometry fixes, so a procedure answering nonsense is caught.
        call pf_radec2vec(90.0_real64, 0.0_real64, v, frame=PF_HP_DEC_NORTH)
        if (what == "" .and. abs(v(2) - 1.0_real64) > 1.0e-12_real64) what = "pf_radec2vec"

        ! The frozen contract identifier is a parameter, and naming it is what keeps it re-exported.
        algo = pf_sky_region_algorithm
        if (what == "" .and. len_trim(algo) == 0) what = "pf_sky_region_algorithm is empty"
        if (what == "" .and. PF_EDGE_GREAT_CIRCLE == PF_EDGE_RADEC) what = "the edge rules"
    end subroutine check_sphere_surface

end module test_module_surface_sphere

!> `parquet_skycoord` alone: every rotation, the data-driven form and the rotation object, the
!> selector tokens, the frame-free RA/Dec geometry and proper motion, the sexagesimal fields and text
!> and the CMB rest frame, and no settings knob at all.
!!
!! **One library import, and it must stay that way.** This module's row in the entry-module table
!! claims that `use parquet_skycoord` compiles a handful of Fortran files and re-exports no setting
!! -- it reads none and prints nothing -- and every procedure and selector below is a separate
!! `public ::` entry, so each is separately droppable.
module test_module_surface_skycoord
    use parquet_skycoord               ! THE ONLY library import.
    use iso_fortran_env, only : real64
    implicit none
    private
    public :: check_skycoord_surface

contains

    !> Uses every rotation, the data-driven form, the rotation object, both token procedures, the four
    !! geometry procedures, the ten sexagesimal ones and `pf_zhel2zcmb` through `use parquet_skycoord`
    !! alone, each against a value the geometry or the arithmetic fixes.
    subroutine check_skycoord_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        character(len=:), allocatable :: name, text
        real(real64) :: a, b, c, d, ra, dec, s
        integer :: h, m, sgn, deg
        logical :: ok
        type(pf_sky_rotation) :: rot

        what = ""
        ! The eight named rotations chained round a closed loop through all four systems: a procedure
        ! that compiled but answered nonsense would leave the loop open.
        call pf_icrs2gal(30.0_real64, 40.0_real64, a, b)
        call pf_gal2sgal(a, b, c, d)
        call pf_sgal2icrs(c, d, a, b)
        call pf_icrs2ecl(a, b, c, d)
        call pf_ecl2icrs(c, d, a, b)
        call pf_icrs2sgal(a, b, c, d)
        call pf_sgal2gal(c, d, a, b)
        call pf_gal2icrs(a, b, ra, dec)
        if (abs(ra - 30.0_real64) > 1.0e-9_real64 .or. abs(dec - 40.0_real64) > 1.0e-9_real64) &
            what = "the eight named rotations"
        ! The ICRS north pole in Galactic coordinates: latitude the north Galactic pole's declination.
        call pf_sky_convert(0.0_real64, 90.0_real64, PF_COORD_ICRS, PF_COORD_GALACTIC, a, b)
        if (what == "" .and. abs(b - 27.128252414968_real64) > 1.0e-9_real64) what = "pf_sky_convert"
        call pf_sky_convert(a, b, PF_COORD_GALACTIC, PF_COORD_ECLIPTIC, c, d)
        if (what == "" .and. abs(d - 66.560718661389_real64) > 1.0e-9_real64) what = "PF_COORD_ECLIPTIC"
        call pf_sky_convert(c, d, PF_COORD_SUPERGALACTIC, PF_COORD_SUPERGALACTIC, a, b)
        if (what == "" .and. (a /= c .or. b /= d)) what = "PF_COORD_SUPERGALACTIC"
        ! FK5 J2000: the ICRS pole moves the frame bias's 21.88 mas off 90, and comes back.
        call pf_icrs2fk5(0.0_real64, 90.0_real64, ra, dec)
        if (what == "" .and. abs(dec - 89.999993921679_real64) > 1.0e-9_real64) what = "pf_icrs2fk5"
        call pf_fk52icrs(ra, dec, a, b)
        if (what == "" .and. abs(b - 90.0_real64) > 1.0e-9_real64) what = "pf_fk52icrs"
        call pf_sky_convert(0.0_real64, 90.0_real64, PF_COORD_ICRS, PF_COORD_FK5, a, b)
        if (what == "" .and. abs(b - dec) > 1.0e-12_real64) what = "PF_COORD_FK5"
        ! The rotation object, prepared once: the ICRS pole at the north Galactic pole's declination.
        call rot%init(PF_COORD_ICRS, PF_COORD_GALACTIC)
        call rot%apply(0.0_real64, 90.0_real64, a, b)
        if (what == "" .and. (.not. rot%is_init() .or. abs(b - 27.128252414968_real64) > 1.0e-9_real64)) &
            what = "pf_sky_rotation"
        ! Proper motion: 3600 mas/yr due north for a year is a thousandth of a degree of declination.
        call pf_apply_pm(10.0_real64, 20.0_real64, 0.0_real64, 3600.0_real64, 1.0_real64, ra, dec)
        if (what == "" .and. (abs(ra - 10.0_real64) > 1.0e-12_real64 .or. abs(dec - 20.001_real64) > 1.0e-12_real64)) &
            what = "pf_apply_pm"

        ! The tokens, the sentinel included.
        call pf_coord_system_name(PF_COORD_ECLIPTIC, name)
        if (what == "" .and. name /= "ecliptic") what = "pf_coord_system_name"
        if (what == "" .and. pf_coord_system_from_name("Supergalactic") /= PF_COORD_SUPERGALACTIC) &
            what = "pf_coord_system_from_name"
        if (what == "" .and. pf_coord_system_from_name("nowhere") /= PF_COORD_UNKNOWN) what = "PF_COORD_UNKNOWN"

        ! The frame-free geometry: two points on the equator 90 degrees apart in right ascension are 90
        ! degrees apart on the sphere, an offset east along the equator stays on it, and due east is 90.
        if (what == "" .and. abs(pf_angdist_deg(10.0_real64, 0.0_real64, 100.0_real64, 0.0_real64) &
                                 - 90.0_real64) > 1.0e-12_real64) what = "pf_angdist_deg"
        call pf_offset_radec(10.0_real64, 0.0_real64, 90.0_real64, 5.0_real64, ra, dec)
        if (what == "" .and. (abs(ra - 15.0_real64) > 1.0e-12_real64 .or. abs(dec) > 1.0e-12_real64)) &
            what = "pf_offset_radec"
        if (what == "" .and. abs(pf_position_angle_deg(0.0_real64, 0.0_real64, 10.0_real64, 0.0_real64) &
                                 - 90.0_real64) > 1.0e-12_real64) what = "pf_position_angle_deg"

        ! The sexagesimal fields: 187.5 degrees is 12h30m exactly, and -12.5 is -12d30m.
        call pf_deg2hms(187.5_real64, h, m, s)
        if (what == "" .and. (h /= 12 .or. m /= 30 .or. s /= 0.0_real64)) what = "pf_deg2hms"
        call pf_deg2dms(-12.5_real64, sgn, deg, m, s)
        if (what == "" .and. (sgn /= -1 .or. deg /= 12 .or. m /= 30 .or. s /= 0.0_real64)) what = "pf_deg2dms"
        if (what == "" .and. pf_hms2deg(12, 30, 0.0_real64) /= 187.5_real64) what = "pf_hms2deg"
        if (what == "" .and. pf_dms2deg(-1, 12, 30, 0.0_real64) /= -12.5_real64) what = "pf_dms2deg"
        ! The text, written and read back.
        call pf_ra2str(187.5_real64, text)
        if (what == "" .and. text /= "12:30:00.000") what = "pf_ra2str"
        call pf_dec2str(-12.5_real64, text)
        if (what == "" .and. text /= "-12:30:00.00") what = "pf_dec2str"
        call pf_radec2str(187.5_real64, -12.5_real64, text, sep="hms")
        if (what == "" .and. text /= "12h30m00.000s -12d30m00.00s") what = "pf_radec2str"
        call pf_str2ra("12:30:00", ra, ok)
        if (what == "" .and. .not. (ok .and. ra == 187.5_real64)) what = "pf_str2ra"
        call pf_str2dec("-12:30:00", dec, ok)
        if (what == "" .and. .not. (ok .and. dec == -12.5_real64)) what = "pf_str2dec"
        call pf_str2radec("12 30 00 -12 30 00", ra, dec, ok)
        if (what == "" .and. .not. (ok .and. ra == 187.5_real64 .and. dec == -12.5_real64)) what = "pf_str2radec"

        ! The CMB rest frame: with no motion the redshift is unchanged, exactly.
        if (what == "" .and. pf_zhel2zcmb(10.0_real64, 20.0_real64, 0.5_real64, apex_v=0.0_real64) /= 0.5_real64) &
            what = "pf_zhel2zcmb"
    end subroutine check_skycoord_surface

end module test_module_surface_skycoord

module test_module_surface_spatial
    use parquet_spatial                ! THE ONLY library import.
    use iso_fortran_env, only : int32, int64, real64
    implicit none
    private
    public :: check_spatial_surface

contains

    !> Round-trips every knob `parquet_spatial`'s own code reads, then builds and queries an index.
    subroutine check_spatial_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first knob that failed, or "".
        character(len=:), allocatable :: tok
        type(pf_spatial_index) :: sx
        real(real64) :: x(8), y(8), z(8)
        integer(int64) :: got(8), m, ncomp
        integer(int64), allocatable :: labels(:)
        integer :: n_spatial, i
        character(len=:), allocatable :: verb

        what = ""
        n_spatial = parquet_get_spatial_threads()
        call parquet_get_verbosity(verb)

        call parquet_set_spatial_threads(3)
        if (parquet_get_spatial_threads() /= 3) what = "spatial_threads"
        ! `verbosity` is this module's control over what a rebuild says, now that the per-message
        ! knob is gone: a `use parquet_spatial` program must be able to reach it through this import
        ! alone, exactly as it reached the knob it replaced.
        call parquet_set_verbosity("silent")
        call parquet_get_verbosity(tok)
        if (what == "" .and. tok /= "silent") what = "verbosity"
        call parquet_set_verbosity(verb)
        ! The sorting knobs: %build buckets through pf_argsort, so a user of this module alone must
        ! be able to steer which path that takes.
        call parquet_set_sort_threads(2)
        if (what == "" .and. parquet_get_sort_threads() /= 2) what = "sort_threads"
        call parquet_set_sort_counting_path(.false.)
        if (what == "" .and. parquet_get_sort_counting_path()) what = "sort_counting_path"
        call parquet_set_sort_counting_path(.true.)
        call parquet_set_sort_radix_path(.false.)
        if (what == "" .and. parquet_get_sort_radix_path()) what = "sort_radix_path"
        call parquet_set_sort_radix_path(.true.)
        call parquet_set_sort_counting_bucket_limit(64_int64)
        if (what == "" .and. parquet_get_sort_counting_bucket_limit() /= 64_int64) what = "sort_counting_bucket_limit"
        call parquet_set_sort_counting_bucket_limit(0_int64)
        ! The output pair: the automatic-rebuild warning emits from this tier.
        call parquet_set_verbosity("silent")
        call parquet_get_verbosity(tok)
        if (what == "" .and. tok /= "silent") what = "verbosity"
        call parquet_set_verbosity("normal")
        call parquet_set_message_stream("stderr")
        call parquet_get_message_stream(tok)
        if (what == "" .and. tok /= "stderr") what = "message_stream"
        call parquet_set_message_stream("stdout")

        ! And the capability itself: a re-export list that compiles while exporting no usable type
        ! would satisfy every assertion above.
        do i = 1, 8
            x(i) = real(i, kind=real64)
            y(i) = 0.0_real64
            z(i) = 0.0_real64
        end do
        call sx%build(x, y, z, radius=1.5_real64)
        m = sx%within([3.0_real64, 0.0_real64, 0.0_real64], 1.5_real64, got)
        if (what == "" .and. m /= 3_int64) what = "pf_spatial_index%within"
        m = sx%nearest([3.2_real64, 0.0_real64, 0.0_real64], 2_int32, got)
        if (what == "" .and. (m /= 2_int64 .or. got(1) /= 3_int64)) what = "pf_spatial_index%nearest"
        ! The module procedure, not a binding: a facade that re-exported only the type would
        ! satisfy every assertion above and leave Friends-of-Friends needing a second import.
        call pf_connected_components([1_int64, 2_int64], [2_int64, 3_int64], 5_int64, labels, ncomp=ncomp, &
                                     min_size=2)
        if (what == "" .and. (ncomp /= 1_int64 .or. size(labels) /= 5 .or. labels(5) /= 0_int64)) &
            what = "pf_connected_components"
        if (what == "" .and. sx%metric() /= PF_METRIC_EUCLIDEAN) what = "PF_METRIC_EUCLIDEAN"

        call parquet_set_spatial_threads(n_spatial)
        call parquet_set_sort_threads(0)
    end subroutine check_spatial_surface

end module test_module_surface_spatial

!> `parquet_version` alone: the library's own version string.
!!
!! **This is the acceptance test for the leaf.** `parquet_get_version` is deliberately re-exported
!! by exactly one module, the `parquet` facade -- no tier carries it -- so `use parquet_version` is
!! the only route for a program built on `parquet_random`, `parquet_columns` or any other
!! Arrow-free entry module. If this file stops compiling, that route is gone.
module test_module_surface_version
    use parquet_version                ! THE ONLY library import.
    implicit none
    private
    public :: check_version_surface

contains

    !> Answers both forms through `use parquet_version` alone, and checks they agree.
    subroutine check_version_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        character(len=:), allocatable :: release, internal

        what = ""
        call parquet_get_version(release)
        if (len_trim(release) == 0) what = "parquet_get_version (default mode)"
        call parquet_get_version(internal, mode="internal")
        if (what == "" .and. len_trim(internal) == 0) what = "parquet_get_version(mode='internal')"
        ! Each form is checked for its own shape, never against the other: the default form comes
        ! from VERSION.txt and the internal one from cversion, and the two legitimately differ
        ! between a version bump on one side and the other (parquet_get_version remarks on it).
        if (what == "" .and. verify(trim(release), "0123456789.") /= 0) &
            what = "the default form is not a bare release number"
        if (what == "" .and. internal(1:1) /= "v") what = "mode='internal' is not v-prefixed"
        if (what == "" .and. index(internal, " (") == 0) what = "mode='internal' carries no date"
    end subroutine check_version_surface

end module test_module_surface_version

!> `parquet_io` alone: the whole read/write surface, plus the settings that govern it.
!!
!! **One library import, and it must stay that way.** This module is the acceptance test for the
!! `parquet_io` facade (src/parquet_io.f90) in exactly the way `test_facade_covers_every_layer`
!! (test/test_examples.f90) is for `parquet` -- naming an entity from each layer `parquet_io` must
!! re-export proves that a program needing only file I/O really does need this one `use` statement.
!! A dropped re-export stops this file COMPILING rather than failing an assertion, which is the
!! point: it is a build-time break for every downstream user.
!!
!! Layers touched, one name each: the writer and reader lifecycles, `parquet_schema` built both in
!! code and from MAML, `parquet_filter`, `parquet_sortkey`, `parquet_read_qc`, the metadata types
!! and queries, and the element types the calls take and return -- `parquet_string_column`,
!! `parquet_string`, `parquet_timestamp` with a `parquet_unit_*` selector, and `parquet_maml_file`.
!! Plus the settings: a `use parquet_io` program must be able to choose its writer's compression
!! and silence what the library prints without also naming `parquet_settings`.
module test_module_surface_io
    use parquet_io                     ! THE ONLY library import.
    use iso_fortran_env, only : int32, int64
    implicit none
    private
    public :: check_io_surface

contains

    !> Round-trips a file through `parquet_io` alone and touches every layer it re-exports.
    subroutine check_io_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        character(len=*), parameter :: out_file = "test_run/module_surface_io.parquet"
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_schema) :: schema, from_maml
        type(parquet_filter) :: filt
        type(parquet_sortkey) :: skey
        type(parquet_read_qc) :: rqc
        type(parquet_column_info) :: cinfo
        type(parquet_table_metadata) :: tmeta
        ! `target` is REQUIRED, not decorative: `%view` associates the returned handle's `%col`
        ! with its own `intent(in), target` dummy, and F2018 15.5.2.4 leaves that pointer
        ! UNDEFINED on return when the actual argument is not itself a target. gfortran, ifx
        ! and flang all run the handle happily; nagfor's -C=dangling (the `nagdeb` profile)
        ! aborts with "Dangling pointer SELF%COL used as argument to intrinsic function
        ! ASSOCIATED". See CLAUDE.md's note on %view_all for the same trap.
        type(parquet_string_column), target :: sc
        type(parquet_string) :: sview
        type(parquet_timestamp) :: ts
        type(parquet_maml_file) :: mf
        integer(int32) :: id(3)
        integer(int32), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=:), allocatable :: comp_was, verb_was
        character(len=:), allocatable :: names(:)
        logical :: exists
        character(len=:), allocatable :: arrow_ver

        what = ""
        id = [1_int32, 2_int32, 3_int32]

        ! The settings this module re-exports, exercised BEFORE the writer opens -- which is when
        ! the C++ mirror is taken, so it is also the only time a compression choice can apply.
        call parquet_get_default_compression(comp_was)
        call parquet_get_verbosity(verb_was)
        call parquet_set_default_compression("snappy")
        call parquet_set_verbosity(verb_was)
        if (parquet_max_filter_depth <= 0) what = "parquet_max_filter_depth"

        ! Writer lifecycle, with a schema built in code.
        call schema%init("surface", "module surface probe")
        call schema%add_field("id", "int32", info="probe column")
        call parquet_parse_maml(schema)
        call parquet_open_writer(writer, out_file, schema)
        call parquet_write_column(writer, "id", id)
        call parquet_close_writer(writer)

        ! Reader lifecycle, plus the metadata and column queries.
        call parquet_open_reader(reader, out_file)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        call parquet_read_column(reader, "id", got)
        exists = parquet_column_exists(reader, "id")
        call parquet_get_column_names(reader, names)
        call parquet_close_reader(reader)

        if (what == "" .and. nrows /= 3_int64) what = "parquet_get_nrows"
        if (what == "" .and. .not. all(got == id)) what = "parquet_read_column"
        if (what == "" .and. .not. exists) what = "parquet_column_exists"
        if (what == "" .and. size(names) /= 1) what = "parquet_get_column_names"

        ! The remaining re-exported types, named so a dropped re-export breaks the BUILD. Each is
        ! used, not merely declared: an unused declaration would still compile if the type were
        ! reachable by some other route, and there is no other route from a single `use parquet_io`.
        call filt%add("id > 0")
        if (what == "" .and. filt%n /= 1) what = "parquet_filter"
        call skey%add("id")
        if (what == "" .and. skey%n /= 1) what = "parquet_sortkey"
        call rqc%add("id, 0, 10")
        if (what == "" .and. rqc%n /= 1) what = "parquet_read_qc"
        call sc%clear()
        call sc%append_string("probe")
        sview = sc%view(1_int64)
        if (what == "" .and. sview%length() /= 5) what = "parquet_string_column"
        call ts%set_unix(0_int64, parquet_unit_micros)
        if (what == "" .and. ts%is_null()) what = "parquet_timestamp"
        mf = parquet_load_maml_file("schemas/maml_example.maml")
        if (what == "" .and. .not. allocated(mf%lines)) what = "parquet_maml_file"
        call parquet_parse_maml("schemas/maml_example.maml", from_maml)
        cinfo = from_maml%cinfo
        tmeta = from_maml%metadata
        if (what == "" .and. cinfo%get_num_fields() <= 0) what = "parquet_column_info"
        if (what == "" .and. .not. allocated(tmeta%items)) what = "parquet_table_metadata"

        ! The linked Arrow version, re-exported here from parquet_settings. Its Arrow-free
        ! counterpart parquet_get_version is deliberately NOT reachable from this module -- see
        ! test_module_surface_version above.
        call parquet_get_arrow_version(arrow_ver)
        if (what == "" .and. len_trim(arrow_ver) == 0) what = "parquet_get_arrow_version"

        call parquet_set_default_compression(comp_was)
    end subroutine check_io_surface

end module test_module_surface_io

!> `parquet_columns` alone: the column container, its kind constants and its per-cell surface.
!!
!! **One library import, and it must stay that way.** `parquet_columns` is advertised as a ten-file
!! Arrow-free import in doc/pages/operating/choosing-a-module.md, so a program that wants a typed,
!! null-aware column and nothing else must reach the whole container through this one `use`. It had
!! no such test: test_columns.f90 imports `parquet_strings` and `parquet_temporal` alongside it, so
!! a re-export dropped from `parquet_columns` would go on compiling there.
!!
!! It reads no settings, which is why this module asserts a capability rather than a knob -- and why
!! its own row in that page's settings table says "none".
module test_module_surface_columns
    use parquet_columns                ! THE ONLY library import.
    use iso_fortran_env, only : int64, real64
    implicit none
    private
    public :: check_columns_surface

contains

    !> Builds, fills, nulls and reads a column back through `use parquet_columns` alone.
    subroutine check_columns_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        type(parquet_column) :: col
        real(real64) :: v
        character(len=:), allocatable :: kname

        what = ""
        call col%init(PK_FLOAT64, 3_int64)
        if (col%kindof() /= PK_FLOAT64) what = "%kindof after init"
        if (what == "" .and. col%length() /= 3_int64) what = "%length after init"
        if (what == "" .and. col%colwidth() /= 1) what = "%colwidth on a scalar kind"
        ! parquet_kind_name is the module's own name-for-a-kind helper; naming it here is what
        ! keeps it re-exported, since nothing else in this file can reach it. A SUBROUTINE with an
        ! allocatable-character result argument, per the project-wide rule in CLAUDE.md.
        call parquet_kind_name(PK_FLOAT64, kname)
        if (what == "" .and. kname /= "PK_FLOAT64") what = "parquet_kind_name"

        call col%set_at(1_int64, 1.5_real64)
        call col%set_at(2_int64, 2.5_real64)
        call col%set_null(3_int64)
        call col%get_at(1_int64, v)
        if (what == "" .and. v /= 1.5_real64) what = "%set_at/%get_at round trip"
        if (what == "" .and. .not. col%is_null(3_int64)) what = "%set_null/%is_null"
        if (what == "" .and. col%is_null(1_int64)) what = "a written row reads as null"
        call col%clear()
    end subroutine check_columns_surface

end module test_module_surface_columns

!> `parquet_list` alone: build, fill, null, read back and copy a variable-length list column.
!!
!! **One library import, and it must stay that way.** `parquet_list` is advertised as an
!! eleven-file Arrow-free import in doc/pages/operating/choosing-a-module.md, so a program that
!! wants variable-length list storage and nothing else must reach all of it through this one
!! `use`. That is wider than it looks: the module has to re-export `parquet_column` (a list column
!! is adopted into one), the payload kind discriminators, and the three temporal element types, or
!! a caller cannot so much as declare what it is building. `test_list.f90` imports several modules
!! alongside it, so a dropped re-export would go on compiling there.
!!
!! It reads no settings, which is why this module asserts a capability rather than a knob -- and
!! why its own row in that page's settings table says "none". Keeping it that way is deliberate:
!! see the `found=`-not-`warn=` decision in feature_container_phase1.md, which turns on exactly
!! this dependency.
module test_module_surface_list
    use parquet_list                   ! THE ONLY library import.
    use iso_fortran_env, only : int32, int64
    implicit none
    private
    public :: check_list_surface

contains

    !> Builds, fills, nulls, reads and copies a list column through `use parquet_list` alone.
    subroutine check_list_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        type(parquet_list_column), target :: lc            ! `target`: %view stores a pointer to it.
        type(parquet_list_row) :: row
        type(parquet_column) :: col
        class(parquet_container_column), allocatable :: cc
        integer(int32), allocatable :: v(:)
        character(len=:), allocatable :: txt

        what = ""
        call lc%init(PK_INT32)
        if (lc%element_kind() /= PK_INT32) what = "%element_kind after init"
        call lc%append_row([1_int32, 2_int32, 3_int32])
        call lc%append_null_row()
        if (what == "" .and. lc%size() /= 2_int64) what = "%size after two appends"
        if (what == "" .and. .not. lc%is_null(2_int64)) what = "%append_null_row/%is_null"
        if (what == "" .and. lc%is_null(1_int64)) what = "a filled row reads as null"

        row = lc%view(1_int64)
        if (what == "" .and. row%length() /= 3_int64) what = "%view/%length"
        call row%get(v)
        if (what == "" .and. size(v) /= 3) what = "%get element count"
        if (what == "" .and. v(3) /= 3_int32) what = "%get values"

        ! kind_text is the abstract face's own describe-yourself binding; naming it here is what
        ! keeps `parquet_container_column` re-exported, since nothing else in this module can
        ! reach it. A SUBROUTINE with an allocatable-character result argument, per CLAUDE.md.
        call lc%kind_text(txt)
        if (what == "" .and. txt /= "list<int32>") what = "%kind_text"

        ! The whole point of the abstract face: a list column goes into a parquet_column without
        ! either side naming the other's type. Reachable from this one import or not at all.
        call lc%clone_into(cc)
        call col%adopt_container(cc)
        if (what == "" .and. col%kindof() /= PK_LIST) what = "%adopt_container kind"
        if (what == "" .and. col%length() /= 2_int64) what = "%adopt_container row count"
        call col%clear()
        call lc%clear()
    end subroutine check_list_surface

end module test_module_surface_list

!> `parquet_struct` alone: build a struct column, read a field back, and adopt it into a
!> `parquet_column` -- all through one import.
!!
!! **One library import, and it must stay that way.** A second `use` here would silently stop this
!! testing anything (`.claude/rules/code-style.md`, "Interface blocks and submodules"): the point is that everything a struct
!! column needs -- the type, the handle, the `PK_*` kinds, the abstract container face, the
!! temporal element types and the two settings knobs governing its one warning -- is reachable
!! from `use parquet_struct` and nothing else.
module test_module_surface_struct
    use parquet_struct                 ! THE ONLY library import.
    use iso_fortran_env, only : int32, int64
    implicit none
    private
    public :: check_struct_surface

contains

    !> Builds, fills, nulls, reads and adopts a struct column through `use parquet_struct` alone.
    subroutine check_struct_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        type(parquet_struct_column), target :: sc          ! `target`: %view stores a pointer to it.
        type(parquet_struct_row) :: row
        type(parquet_column) :: col
        class(parquet_container_column), allocatable :: cc
        integer(int32) :: v
        character(len=:), allocatable :: txt
        logical :: ok

        what = ""
        call sc%init(["id ", "nm "], [PK_INT32, PK_STRING])
        if (sc%field_count() /= 2) what = "%field_count after init"
        call sc%append_row()
        call sc%set_field(1, "id", 7_int32)
        call sc%append_null_row()
        if (what == "" .and. sc%size() /= 2_int64) what = "%size after two appends"
        if (what == "" .and. .not. sc%is_null(2_int64)) what = "%append_null_row/%is_null"
        if (what == "" .and. sc%is_null(1_int64)) what = "a filled row reads as null"

        row = sc%view(1_int64)
        call row%get_field("id", v, is_valid=ok)
        if (what == "" .and. .not. ok) what = "%get_field validity"
        if (what == "" .and. v /= 7_int32) what = "%get_field value"

        ! The settings knobs this module re-exports because its %field(warn=) path can emit --
        ! naming one here is what keeps the re-export from being dropped.
        call parquet_get_verbosity(txt)
        call parquet_set_verbosity(txt)

        ! kind_text is the abstract face's own describe-yourself binding; naming it here is what
        ! keeps `parquet_container_column` re-exported, since nothing else in this module can
        ! reach it. A SUBROUTINE with an allocatable-character result argument, per CLAUDE.md.
        call sc%kind_text(txt)
        if (what == "" .and. txt /= "struct<id:int32,nm:string>") what = "%kind_text"

        ! The whole point of the abstract face: a struct column goes into a parquet_column without
        ! either side naming the other's type. Reachable from this one import or not at all.
        call sc%clone_into(cc)
        call col%adopt_container(cc)
        if (what == "" .and. col%kindof() /= PK_STRUCT) what = "%adopt_container kind"
        if (what == "" .and. col%length() /= 2_int64) what = "%adopt_container row count"
        call col%clear()
        call sc%clear()
    end subroutine check_struct_surface

end module test_module_surface_struct

!> `parquet_map` alone: a map column built, filled, nulled, looked up and adopted -- proving that
!! the whole surface a narrow consumer needs, INCLUDING `parquet_column`, the `PK_*` kinds and the
!! two settings knobs governing its soft-fail warnings, is reachable from `use parquet_map` and
!! nothing else.
module test_module_surface_map
    use parquet_map                    ! THE ONLY library import.
    use iso_fortran_env, only : int32, int64
    implicit none
    private
    public :: check_map_surface

contains

    !> Builds, fills, nulls, looks up and adopts a map column through `use parquet_map` alone.
    subroutine check_map_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        type(parquet_map_column), target :: mc             ! `target`: %view stores a pointer to it.
        type(parquet_map_row) :: row
        type(parquet_column) :: col
        class(parquet_container_column), allocatable :: cc
        integer(int32) :: v
        character(len=:), allocatable :: txt
        logical :: ok

        what = ""
        call mc%init(PK_INT32)
        if (mc%element_kind() /= PK_INT32) what = "%element_kind after init"
        call mc%append_row(["id"], [7_int32])
        call mc%append_null_row()
        if (what == "" .and. mc%size() /= 2_int64) what = "%size after two appends"
        if (what == "" .and. .not. mc%is_null(2_int64)) what = "%append_null_row/%is_null"
        if (what == "" .and. mc%is_null(1_int64)) what = "a filled row reads as null"

        row = mc%view(1_int64)
        call row%get("id", v, is_valid=ok)
        if (what == "" .and. .not. ok) what = "%get validity"
        if (what == "" .and. v /= 7_int32) what = "%get value"
        if (what == "" .and. row%key_count("id") /= 1) what = "%key_count"

        ! The settings knobs this module re-exports because its soft-fail lookups can emit --
        ! naming them here is what keeps the re-export from being dropped.
        call parquet_get_verbosity(txt)
        call parquet_set_verbosity(txt)

        ! kind_text is the abstract face's own describe-yourself binding; naming it here is what
        ! keeps `parquet_container_column` re-exported, since nothing else in this module can
        ! reach it. A SUBROUTINE with an allocatable-character result argument, per CLAUDE.md.
        call mc%kind_text(txt)
        if (what == "" .and. txt /= "map<string,int32>") what = "%kind_text"

        ! The whole point of the abstract face: a map column goes into a parquet_column without
        ! either side naming the other's type. Reachable from this one import or not at all.
        call mc%clone_into(cc)
        call col%adopt_container(cc)
        if (what == "" .and. col%kindof() /= PK_MAP) what = "%adopt_container kind"
        if (what == "" .and. col%length() /= 2_int64) what = "%adopt_container row count"
        call col%clear()
        call mc%clear()
    end subroutine check_map_surface

end module test_module_surface_map

!> `parquet_random` alone: a draw, and the deliberate ABSENCE of any settings re-export.
!!
!! **One library import, and it must stay that way.** The generator is a three-file leaf and had no
!! single-import test at all -- every random suite in this directory reaches it through
!! `use parquet`, which cannot notice its own surface shrinking.
!!
!! This module is the one place in this file where **what is not exported is the point**.
!! `parquet_random` reads no setting: the thread rule and the parallel-element floor both live in
!! `parquet_sampling`, one tier up, and re-exporting them here would advertise knobs this module
!! does not consult. That absence cannot be asserted by a call, so it is asserted by this module
!! compiling with no reference to them -- which is exactly how a dropped re-export is caught
!! elsewhere in this file, run backwards.
module test_module_surface_random
    use parquet_random                 ! THE ONLY library import.
    use iso_fortran_env, only : int64, real64
    implicit none
    private
    public :: check_random_surface

contains

    !> Draws through `use parquet_random` alone and checks the two frozen identities hold.
    subroutine check_random_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        integer(int64), parameter :: seed = 20260823_int64
        real(real64) :: u, v(3)
        integer(int64) :: k, bits
        character(len=:), allocatable :: algo

        what = ""
        u = pf_random_at(seed, 1_int64)
        if (u < 0.0_real64 .or. u >= 1.0_real64) what = "pf_random_at is outside [0,1)"

        ! The contract identity, not merely a range check: pf_random_at is the top 53 bits of
        ! pf_random_bits_at for the same coordinate (see CLAUDE.md, "Each parquet_random generic
        ! reads its OWN word space"). Asserting it here means this test fails if the two ever
        ! drift apart, which a bounds check alone would sail past.
        bits = pf_random_bits_at(seed, 1_int64)
        if (what == "" .and. u /= real(ishft(bits, -11), real64) * (0.5_real64 ** 53)) &
            what = "pf_random_at is not the top 53 bits of pf_random_bits_at"

        k = pf_random_int_at(seed, 1_int64, 1_int64, 6_int64)
        if (what == "" .and. (k < 1_int64 .or. k > 6_int64)) what = "pf_random_int_at is out of range"

        ! The frozen contract identifier is a parameter, not a call -- naming it here is what
        ! keeps it re-exported, and a change to it is a change to every value above.
        algo = pf_random_algorithm
        if (what == "" .and. len_trim(algo) == 0) what = "pf_random_algorithm is empty"

        ! The points-on-a-sphere family is reachable through the same one import, and its identifier
        ! with it; a unit length is the one property any direction must have.
        v = pf_random_direction_at(seed, 1_int64)
        if (what == "" .and. abs(norm2(v) - 1.0_real64) > 1.0e-12_real64) &
            what = "pf_random_direction_at did not return a unit vector"
        algo = pf_sphere_algorithm
        if (what == "" .and. len_trim(algo) == 0) what = "pf_sphere_algorithm is empty"
    end subroutine check_random_surface

end module test_module_surface_random

!> `parquet_tables` alone: build a table, write it, open it, read it back.
!!
!! **One library import, and it must stay that way.** `parquet_tables` is the last advertised entry
!! module to get one of these, and it is the one where the gap mattered most: at 62 of the facade's
!! 66 files it re-exports a large `only:` slice of `parquet_core` and the whole of
!! `parquet_columns`, so it has more to lose than any other tier and test_table.f90 -- which imports
!! `parquet`, `parquet_columns`, `parquet_strings`, `parquet_temporal` AND `parquet_tables` -- could
!! never have noticed a name dropped from that slice.
!!
!! The round trip is the assertion's shape rather than its subject: what is being tested is that
!! `parquet_new_table`, `%add_column`, `parquet_write_table`, `parquet_open_table` and the value
!! accessors are all reachable from this single `use`, which is a BUILD-time property.
module test_module_surface_tables
    use parquet_tables                 ! THE ONLY library import.
    use iso_fortran_env, only : int64, real64
    implicit none
    private
    public :: check_tables_surface

    !> A reducer of the tables surface: `parquet_group_reducer` and the interface of its
    !! deferred binding are reachable through the one import.
    type, extends(parquet_group_reducer) :: surface_reducer
        real(real64) :: scale = 1.0_real64 !! multiplies the row sum.
    contains
        procedure :: reduce => surface_reduce !! `scale` times the sum of the group's rows.
    end type surface_reducer

contains

    !> One value per group: the sum of the group's rows (`parquet_group_reduce_i`'s shape).
    function surface_row_sum(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! their sum.
        r = real(sum(rows), real64)
        if (g < 1_int64) r = -huge(r)
    end function surface_row_sum

    !> Two values per group: the row sum and the group number (`parquet_group_apply_i`'s shape).
    subroutine surface_row_sum_two(g, rows, out)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64), intent(out) :: out(:)   !! receives the two values.
        out(1) = real(sum(rows), real64)
        out(2) = real(g, real64)
    end subroutine surface_row_sum_two

    !> The first of a group's values (`parquet_group_column_reduce_i`'s shape).
    function surface_col_first(values, is_valid, weights) result(r)
        real(real64), intent(in) :: values(:)            !! the group's values.
        logical, intent(in), optional :: is_valid(:)     !! present for a group holding a null.
        real(real64), intent(in), optional :: weights(:) !! present when weights were given.
        real(real64) :: r                                !! the first value.
        r = values(1)
        if (present(is_valid) .or. present(weights)) r = -huge(r)   ! neither is expected here
    end function surface_col_first

    !> `surface_reducer%reduce`; see the type.
    subroutine surface_reduce(self, g, rows, out)
        class(surface_reducer), intent(in) :: self !! the reducer.
        integer(int64), intent(in) :: g            !! the group number.
        integer(int64), intent(in) :: rows(:)      !! the group's rows.
        real(real64), intent(out) :: out(:)        !! receives the results.
        out(1) = self%scale * real(sum(rows), real64)
        if (size(out) > 1) out(2) = real(g, real64)
    end subroutine surface_reduce

    !> Round-trips a two-column table through `use parquet_tables` alone.
    subroutine check_tables_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        ! Its own filename: tests in a suite run concurrently and test_module_surface_io already
        ! writes test_run/module_surface_io.parquet.
        character(len=*), parameter :: out_file = "test_run/module_surface_tables.parquet"
        character(len=*), parameter :: sink_file = "test_run/module_surface_tables_sink.parquet"
        type(parquet_table) :: t, back, streamed
        type(parquet_table_index) :: ix
        type(parquet_grouping) :: grp
        type(surface_reducer) :: red
        procedure(parquet_group_reduce_i), pointer :: one => null()
        procedure(parquet_group_apply_i), pointer :: many => null()
        procedure(parquet_group_column_reduce_i), pointer :: colf => null()
        integer(int64), allocatable :: per_group_exact(:), distinct(:), per_row(:)
        type(parquet_table_writer) :: out
        integer(int64) :: ids(3), row, gn
        real(real64) :: mass(3), gbuf(1)
        real(real64), allocatable :: got(:), per_group(:), per_group_two(:, :)

        what = ""
        ids = [1_int64, 2_int64, 3_int64]
        mass = [1.5_real64, 2.5_real64, 3.5_real64]

        call parquet_new_table(t)
        call t%add_column("id", ids)
        call t%add_column("mass", mass)
        if (t%nrows() /= 3_int64) what = "%nrows on a table built in memory"
        if (what == "" .and. t%ncols() /= 2) what = "%ncols on a table built in memory"
        ! The lookup index is a parquet_tables type over parquet_index's engines, and the key
        ! conversion it runs is parquet_core's: all three reachable through this one import.
        call t%build_index("id", ix)
        call ix%find(2_int64, row)
        if (what == "" .and. row /= 2_int64) what = "%build_index/%find on a table built in memory"
        if (what == "" .and. ix%count(9_int64) /= 0_int64) what = "%count on an absent key"
        ! The grouping is a parquet_tables type over the sort engine's partition: reachable and
        ! usable through this one import.
        call t%group_by("id", grp)
        if (what == "" .and. grp%ngroups() /= 3_int64) what = "%group_by/%ngroups on a table built in memory"
        ! %apply and its callback interfaces: the two abstract interfaces name the procedure
        ! forms' shapes -- taken here through procedure pointers declared with them, so a dropped
        ! re-export of either fails this build -- and the abstract type the object form's.
        one => surface_row_sum
        many => surface_row_sum_two
        call grp%apply(one, per_group)
        if (what == "" .and. size(per_group) /= 3) what = "%apply, one-value procedure form"
        call grp%apply(many, 2, per_group_two)
        if (what == "" .and. size(per_group_two, 2) /= 3) what = "%apply, matrix procedure form"
        call grp%apply(red, per_group)
        if (what == "" .and. any(per_group /= [1.0_real64, 2.0_real64, 3.0_real64])) what = "%apply, object form"
        ! %agg in its three forms and %nunique: the statistics tier is reachable through this
        ! one import, as is the per-column callback's interface.
        call grp%agg("mass", "mean", per_group)
        if (what == "" .and. any(per_group /= mass)) what = "%agg, token form"
        call grp%agg("id", "sum", per_group_exact)
        if (what == "" .and. any(per_group_exact /= ids)) what = "%agg, exact form"
        ! The procedure itself, not the pointer `colf` (declared with the interface so a dropped
        ! re-export still fails this build): gfortran 15 resolves a procedure POINTER actual of
        ! this generic to the `character` specific and hands it an empty token
        ! (.claude/rules/fortran-gotchas.md); %apply above has no character competitor.
        colf => surface_col_first
        if (.not. associated(colf)) what = "the per-column interface's pointer did not associate"
        call grp%agg("mass", surface_col_first, per_group)
        if (what == "" .and. any(per_group /= mass)) what = "%agg, procedure form"
        call grp%nunique("mass", distinct)
        if (what == "" .and. any(distinct /= 1_int64)) what = "%nunique"
        ! %broadcast and %gather: the per-row and per-group ends of the hot loop.
        call grp%broadcast(per_group_exact, per_row)
        if (what == "" .and. any(per_row /= ids)) what = "%broadcast"
        call grp%gather("mass", 2_int64, gbuf, gn)
        if (what == "" .and. (gn /= 1_int64 .or. gbuf(1) /= mass(2))) what = "%gather"

        call parquet_write_table(t, out_file, overwrite=.true.)

        call parquet_open_table(back, out_file)
        if (what == "" .and. back%nrows() /= 3_int64) what = "%nrows after reopening"
        call back%get("mass", got)
        if (what == "" .and. size(got) /= 3) what = "%get returned the wrong size"
        if (what == "" .and. got(2) /= 2.5_real64) what = "%get did not round-trip the value"
        if (what == "" .and. back%residency("mass") /= RES_FULL) what = "%residency after %get"
        ! The output file that stays open: a parquet_tables type over the writer, reachable and
        ! usable through this one import -- open, append, close, reopen.
        call parquet_open_table_writer(out, sink_file, t, chunk_size=2)
        call out%append(t)
        if (what == "" .and. out%row_groups() /= 1_int64) what = "the sink did not flush at its threshold"
        call parquet_close_table_writer(out)
        if (what == "" .and. out%is_open()) what = "%is_open after parquet_close_table_writer"
        call parquet_open_table(streamed, sink_file)
        if (what == "" .and. streamed%nrows() /= 3_int64) what = "%nrows after reopening a sink's file"
        ! REGIME_FULL/REGIME_SLICE were named here for the same build-breaking reason as every
        ! other reference in this file, and they are gone: row 30 made them private, because
        ! `regime` is a private component and no binding exposes it, so no caller could ever obtain
        ! a value to compare against either one. Do not restore the reference without first
        ! restoring the accessor that would make the constants usable.
    end subroutine check_tables_surface

end module test_module_surface_tables

!> `parquet_utils` alone: the text and path helpers, with no other library import.
!!
!! **One library import, and it must stay that way.** `parquet_utils` reads no setting at all, so
!! unlike its neighbours here there is no knob to round-trip -- what this asserts instead is that
!! all four families are reachable and usable through the single `use`, which is the half that
!! breaks at BUILD time when a re-export is dropped.
module test_module_surface_utils
    use parquet_utils                  ! THE ONLY library import.
    use iso_fortran_env, only : int32
    implicit none
    private
    public :: check_utils_surface

contains

    !> Exercises one entry point from each family through `use parquet_utils` alone.
    subroutine check_utils_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        character(len=:), allocatable :: got, dir, stem, ext

        what = ""
        call pf_to_lower("AbC", got)
        if (got /= "abc") what = "pf_to_lower"
        call pf_to_upper("AbC", got)
        if (what == "" .and. got /= "ABC") what = "pf_to_upper"
        call pf_to_str(7_int32, got, min_width=3)
        if (what == "" .and. got /= "007") what = "pf_to_str"
        call pf_join_path("a", "b", got)
        if (what == "" .and. got /= "a/b") what = "pf_join_path"
        call pf_split_path("/d/f.txt", dir, stem, ext)
        if (what == "" .and. dir//"|"//stem//"|"//ext /= "/d|f|.txt") what = "pf_split_path"
        call pf_path_add_suffix("/d/f.txt", "_s", got)
        if (what == "" .and. got /= "/d/f_s.txt") what = "pf_path_add_suffix"
        call pf_dirname("/d/f.txt", got)
        if (what == "" .and. got /= "/d") what = "pf_dirname"
        call pf_basename("/d/f.txt", got)
        if (what == "" .and. got /= "f.txt") what = "pf_basename"
        call pf_path_ext("/d/f.txt", got)
        if (what == "" .and. got /= ".txt") what = "pf_path_ext"
        call pf_path_stem("/d/f.txt", got)
        if (what == "" .and. got /= "f") what = "pf_path_stem"
    end subroutine check_utils_surface

end module test_module_surface_utils

!> `parquet_logging` alone: configure a logger, emit through it, and ask what is enabled.
!!
!! **One library import, and it must stay that way.** This module's row in the entry-module table
!! makes two claims nothing else checks: that `use parquet_logging` compiles ONE Fortran file, and
!! that it re-exports no setting -- it is not the library's own messaging system and reads neither
!! `verbosity` nor `message_stream`, configuring itself through `pf_log_configure_from_env`
!! instead. The first claim is the footprint tool's; this asserts the second half of the pair --
!! that the whole surface really is reachable from the single `use`, which is what breaks at BUILD
!! time if a re-export is ever dropped.
!!
!! Emitting goes to a `PF_LOG_STDERR` console sink rather than a file, so the test needs no fixture
!! path -- which matters here because suites run concurrently and two tests sharing a path is a
!! documented source of intermittent failure.
module test_module_surface_logging
    use parquet_logging                ! THE ONLY library import.
    implicit none
    private
    public :: check_logging_surface

contains

    !> Exercises one entry point from each family through `use parquet_logging` alone.
    subroutine check_logging_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        type(pf_logger) :: lg
        ! Fixed-length buffers, not deferred-length allocatables: both of these procedures take a
        ! `character(len=*), intent(out)` and blank-pad it, so an unallocated allocatable would
        ! arrive with length zero and silently receive nothing.
        character(len=PF_LOG_MAX_NAME) :: nm
        integer :: lvl
        logical :: ok

        what = ""
        call lg%init(level=PF_LEVEL_WARNING)
        call lg%add_console(PF_LOG_STDERR)
        call lg%set_name("surface")
        ! The level gate is the one behaviour worth asserting rather than merely calling: it proves
        ! the threshold set above is the one being read, not that the call linked.
        if (lg%enabled(PF_LEVEL_ERROR) .neqv. .true.) what = "enabled(ERROR) at WARNING"
        if (what == "" .and. lg%enabled(PF_LEVEL_DEBUG)) what = "enabled(DEBUG) at WARNING"
        if (what == "") then
            call lg%warning("reachable through one import")
            call pf_log_push_name("inner")
            call lg%get_full_name(nm)
            if (trim(nm) /= "surface.inner") what = "push_name/get_full_name"
            call pf_log_pop_name()
        end if
        if (what == "") then
            call pf_log_level_name(PF_LEVEL_WARNING, nm)
            if (trim(nm) /= "WARNING") what = "pf_log_level_name"
        end if
        if (what == "") then
            call pf_log_level_from_name("info", lvl, ok)
            if (.not. ok .or. lvl /= PF_LEVEL_INFO) what = "pf_log_level_from_name"
        end if
        call lg%close()
    end subroutine check_logging_surface

end module test_module_surface_logging

module test_module_surface_toml
    use parquet_toml                   ! THE ONLY library import.
    implicit none
    private
    public :: check_toml_surface

contains

    !> Reads a whole configuration through `use parquet_toml` alone: parse, section, every getter
    !! family, the accumulator's sweep, and close.
    !!
    !! No file: `pf_toml_loads` is what lets this run inside a concurrently dispatched suite with
    !! no fixture path to collide on. And no `use parquet_logging` either, which is the point --
    !! the module aborts through the logger internally, so a program that only reads configuration
    !! needs one import.
    subroutine check_toml_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        type(pf_toml) :: conf, gen, ent
        type(pf_toml_strings) :: files
        character(len=:), allocatable :: text, name, one
        character(len=PF_TOML_MAX_KEY), allocatable :: keys(:)
        integer :: n, id
        real :: f
        logical :: flag

        what = ""
        text = 'title = "surface"' // new_line("a") // &
               '[general]' // new_line("a") // &
               'nproc = 3' // new_line("a") // &
               'factor = 0.5' // new_line("a") // &
               'verbose = true' // new_line("a") // &
               'files = ["a", "bcd"]' // new_line("a") // &
               '[[region]]' // new_line("a") // &
               'id = 1' // new_line("a")

        call pf_toml_loads(conf, text, name = "surface")
        call pf_toml_get(conf, "title", name)
        if (name /= "surface") what = "pf_toml_get on a root key"

        if (what == "") then
            call pf_toml_section(conf, "general", gen)
            call pf_toml_get(gen, "nproc", n)
            if (n /= 3) what = "pf_toml_get on a section key"
        end if
        if (what == "") then
            call pf_toml_get(gen, "factor", f)
            if (abs(f - 0.5) > 1.0e-6) what = "pf_toml_get for a real"
        end if
        if (what == "") then
            call pf_toml_get(gen, "verbose", flag)
            if (.not. flag) what = "pf_toml_get for a logical"
        end if
        if (what == "") then
            call pf_toml_get(gen, "absent", n, default = 42)
            if (n /= 42) what = "a default was not applied"
        end if
        if (what == "") then
            call pf_toml_get_strings(gen, "files", files)
            if (files%count() /= 2) what = "pf_toml_get_strings count"
        end if
        if (what == "") then
            call files%get(2, one)
            if (one /= "bcd" .or. files%length(2) /= 3) what = "pf_toml_strings element"
        end if
        if (what == "") then
            if (pf_toml_section_count(conf, "region") /= 1) what = "pf_toml_section_count"
        end if
        if (what == "") then
            call pf_toml_section(conf, "region", 1, ent)
            call pf_toml_get(ent, "id", id)
            if (id /= 1) what = "an array-of-tables entry"
        end if
        if (what == "") then
            if (.not. pf_toml_has(gen, "nproc")) what = "pf_toml_has"
        end if
        if (what == "") then
            call pf_toml_keys(gen, keys)
            if (size(keys) /= 4) what = "pf_toml_keys"
        end if
        if (what == "") then
            n = 77
            call pf_toml_get_opt(gen, "absent", n)
            if (n /= 77) what = "pf_toml_get_opt kept the variable"
        end if
        if (what == "") then
            call pf_toml_get_strings_opt(gen, "files", files, count = 2)
            if (files%count() /= 2) what = "pf_toml_get_strings_opt with count"
        end if
        if (what == "") then
            call pf_toml_delete(gen, "absent_key_that_was_never_there")
            call pf_toml_mark_section(gen)
        end if
        if (what == "") then
            ! The sweep must stay SILENT here -- everything above was read. If it reported, this
            ! would abort rather than fail, which is the loudest possible outcome and is fine.
            call pf_toml_check_all(conf, severity = PF_TOML_IGNORE)
            call pf_toml_check(gen)
        end if
        call pf_toml_close(conf)
    end subroutine check_toml_surface

end module test_module_surface_toml

!> `parquet_integrate` alone: the generic, both tolerance forms, the record, the info record,
!! `pf_infinity` and the `PF_INT_*` codes, and no settings knob at all.
!!
!! **One library import, and it must stay that way.** This module's row in the entry-module table
!! makes two claims nothing else checks: that `use parquet_integrate` compiles three Fortran files,
!! and that it re-exports no setting -- quadrature reads none and can print nothing, so there is
!! no knob for it to re-export.
!!
!! `pf_infinity()` is NAMED here rather than integrated over: an infinite bound is refused until
!! the outward walk lands, so a call using one would abort this whole runner. The phase that adds
!! the walk turns the assertion below into an integration over `[1, +inf)`.
module test_module_surface_integrate
    use parquet_integrate                ! THE ONLY library import.
    use iso_fortran_env, only : real64
    implicit none
    private
    public :: check_integrate_surface

contains

    !> Exercises one entry point from each family through `use parquet_integrate` alone.
    subroutine check_integrate_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        type(pf_integrate_info)   :: info
        type(pf_integrate_points) :: pts
        real(real64)                :: r, inf

        what = ""

        ! The generic, in its bare-rtol form, with the info and points records.
        r = pf_integrate(unit_square, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=info, points=pts)
        if (abs(r - 1.0_real64/3.0_real64) > 1.0e-12_real64) what = "pf_integrate"
        if (what == "" .and. .not. info%converged) what = "pf_integrate_info%converged"
        if (what == "" .and. info%status /= PF_INT_OK) what = "PF_INT_OK"
        if (what == "" .and. pts%n /= 21) what = "pf_integrate_points%n"
        if (what == "" .and. abs(sum(pts%w(1:pts%n)*pts%f(1:pts%n)) - r) > 1.0e-12_real64) &
            what = "pf_integrate_points weights"

        ! The atol= form: both tolerances by keyword, reached through this one import.
        if (what == "") then
            r = pf_integrate(unit_square, 0.0_real64, 1.0_real64, &
                             rtol=0.0_real64, atol=1.0e-10_real64)
            if (abs(r - 1.0_real64/3.0_real64) > 1.0e-10_real64) what = "pf_integrate atol="
        end if

        ! pf_infinity is reachable and is an infinity, which is all this phase can assert of it.
        if (what == "") then
            inf = pf_infinity()
            if (inf <= 0.0_real64 .or. abs(inf) <= huge(1.0_real64)) what = "pf_infinity"
        end if

        ! The remaining status codes are reachable by name through this import alone.
        if (what == "" .and. PF_INT_LIMIT == PF_INT_ROUNDOFF) what = "PF_INT_LIMIT"
        if (what == "" .and. PF_INT_BAD_INTEGRAND == PF_INT_NO_CONVERGENCE) what = "PF_INT_BAD_INTEGRAND"
        if (what == "" .and. PF_INT_DIVERGENT == PF_INT_OK) what = "PF_INT_DIVERGENT"

    end subroutine check_integrate_surface

    !> `x*x`, whose integral over the unit interval is `1/3`. A module procedure, because a
    !! callback in this library is never an internal one.
    function unit_square(x) result(f)
        real(real64), intent(in) :: x !! point at which to evaluate
        real(real64)             :: f !! the integrand value

        f = x*x

    end function unit_square

end module test_module_surface_integrate

!> `parquet_interpolate` alone: both interpolant objects and every binding each has, the one-shot
!! generic in every form, the test-only hook for each object, and no settings knob at all.
!!
!! **One library import, and it must stay that way.** This module's row in the entry-module table
!! claims that `use parquet_interpolate` compiles four Fortran files and re-exports no setting --
!! interpolation reads none and prints nothing, so there is no knob for it to re-export.
module test_module_surface_interpolate
    use parquet_interpolate              ! THE ONLY library import.
    use iso_fortran_env, only : real64
    implicit none
    private
    public :: check_interpolate_surface

contains

    !> Exercises every binding and generic through `use parquet_interpolate` alone.
    subroutine check_interpolate_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        type(pf_interp_1d) :: line, spline, knot, clamped, shape
        type(pf_interp_2d) :: plane, surface
        real(real64)       :: x(4), y(4), v(2), z(4, 4), gy(4)
        logical            :: was_uniform, x_was_uniform, y_was_uniform

        what = ""
        x = [0.0_real64, 1.0_real64, 2.0_real64, 3.0_real64]
        y = 2.0_real64*x + 1.0_real64

        ! Both methods, evaluated on a scalar and on an array.
        call line%init(x, y, method="linear")
        if (line%eval(1.5_real64) /= 4.0_real64) what = "pf_interp_1d%eval"
        if (what == "") then
            v = line%eval([0.5_real64, 2.5_real64])
            if (v(1) /= 2.0_real64 .or. v(2) /= 6.0_real64) what = "pf_interp_1d%eval (elemental)"
        end if
        if (what == "") then
            call spline%init(x, y, bc="natural", outside="extrapolate")
            if (abs(spline%eval(4.0_real64) - 9.0_real64) > 1.0e-12_real64) what = "pf_interp_1d%init"
        end if
        if (what == "" .and. .not. spline%is_initialised()) what = "pf_interp_1d%is_initialised"

        ! The derivative of either order and the integral, on a scalar and on an array.
        if (what == "") then
            v = line%derivative([0.5_real64, 2.5_real64])
            if (v(1) /= 2.0_real64 .or. v(2) /= 2.0_real64 .or. line%derivative(1.5_real64, 2) /= 0.0_real64) &
                what = "pf_interp_1d%derivative"
        end if
        if (what == "") then
            if (line%integral(0.0_real64, 2.0_real64) /= 6.0_real64) what = "pf_interp_1d%integral"
        end if

        ! Every other end condition, and the shape-preserving method.
        if (what == "") then
            call knot%init(x, y, bc="not_a_knot")
            call clamped%init(x, y, bc="clamped", slopes=[2.0_real64, 2.0_real64])
            call shape%init(x, y, method="pchip")
            if (abs(knot%eval(1.5_real64) - 4.0_real64) > 1.0e-12_real64 .or. &
                abs(clamped%eval(1.5_real64) - 4.0_real64) > 1.0e-12_real64 .or. &
                abs(shape%eval(1.5_real64) - 4.0_real64) > 1.0e-12_real64) what = "pf_interp_1d%init (bc, pchip)"
        end if
        if (what == "") then
            call spline%clear()
            if (spline%is_initialised()) what = "pf_interp_1d%clear"
        end if

        ! The one-shot generic, in both ranks.
        if (what == "") then
            if (pf_interp(x, y, 0.5_real64, method="linear") /= 2.0_real64) what = "pf_interp (scalar)"
        end if
        if (what == "") then
            v = pf_interp(x, y, [0.5_real64, 2.5_real64], method="linear")
            if (v(1) /= 2.0_real64 .or. v(2) /= 6.0_real64) what = "pf_interp (array)"
        end if

        ! The test-only hook: an evenly spaced table was bracketed by arithmetic.
        if (what == "") then
            call parquet_debug_interp_force_search(line, was_uniform)
            if (.not. was_uniform) what = "parquet_debug_interp_force_search"
        end if

        ! The grid interpolant, both methods, on a scalar pair and on arrays: `z = 1 + 2*x + 3*y`,
        ! which bilinear interpolation and both bicubic splines reproduce.
        gy = [0.0_real64, 0.5_real64, 2.0_real64, 3.0_real64]
        z = 1.0_real64 + spread(2.0_real64*x, 2, 4) + spread(3.0_real64*gy, 1, 4)
        if (what == "") then
            call plane%init(x, gy, z, method="linear")
            if (plane%eval(1.5_real64, 1.25_real64) /= 7.75_real64) what = "pf_interp_2d%eval"
        end if
        if (what == "") then
            v = plane%eval([0.5_real64, 2.5_real64], [0.25_real64, 2.0_real64])
            if (v(1) /= 2.75_real64 .or. v(2) /= 12.0_real64) what = "pf_interp_2d%eval (elemental)"
        end if
        if (what == "") then
            call surface%init(x, gy, z, bc="not_a_knot", outside="extrapolate")
            if (abs(surface%eval(4.0_real64, 3.5_real64) - 19.5_real64) > 1.0e-12_real64) what = "pf_interp_2d%init"
        end if
        if (what == "" .and. .not. surface%is_initialised()) what = "pf_interp_2d%is_initialised"
        if (what == "") then
            call surface%clear()
            if (surface%is_initialised()) what = "pf_interp_2d%clear"
        end if
        if (what == "") then
            if (pf_interp(x, gy, z, 1.5_real64, 1.25_real64, method="linear") /= 7.75_real64) what = "pf_interp (grid, scalar)"
        end if
        if (what == "") then
            v = pf_interp(x, gy, z, [0.5_real64, 2.5_real64], [0.25_real64, 2.0_real64], method="linear")
            if (v(1) /= 2.75_real64 .or. v(2) /= 12.0_real64) what = "pf_interp (grid, array)"
        end if
        if (what == "") then
            call parquet_debug_interp_force_search(plane, x_was_uniform, y_was_uniform)
            if (.not. x_was_uniform .or. y_was_uniform) what = "parquet_debug_interp_force_search (grid)"
        end if

    end subroutine check_interpolate_surface

end module test_module_surface_interpolate

module test_module_surface_optimize
    use parquet_optimize                 ! THE ONLY library import.
    use iso_fortran_env, only : real64, int64
    implicit none
    private
    public :: check_optimize_surface

    !> An objective object, so `pf_objective` itself is extended through this import alone.
    type, extends(pf_objective) :: surface_objective
        integer :: calls = 0 !! evaluations, so the caller can see `eval` ran
    contains
        procedure :: eval => surface_objective_eval !! `(x - 2)**2`, counting the call.
    end type surface_objective

    !> A CONSTRAINED objective, extended through `use parquet_optimize` alone.
    !!
    !! `optimization.md` says `pf_constrained_objective` is DECLARED in this module -- so that
    !! every engine here can see one coming and refuse it -- rather than in `parquet_prima`, which
    !! only re-exports it. No engine of this module may be handed one, so the bindings are called
    !! directly; extending the type at all is what proves the claim.
    type, extends(pf_constrained_objective) :: surface_constrained
    contains
        procedure :: eval => surface_constrained_eval  !! `(x - 2)**2`.
        procedure :: n_constraints => surface_count    !! one constraint.
        procedure :: constraints => surface_constr     !! `x - 1 <= 0`.
    end type surface_constrained

contains

    !> `(x - 2)**2`, counting the call.
    function surface_objective_eval(self, x) result(f)
        class(surface_objective), intent(inout) :: self !! the objective
        real(real64), intent(in)                :: x(:) !! the point
        real(real64)                            :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2
        self%calls = self%calls + 1

    end function surface_objective_eval

    !> Exercises one entry point from each family through `use parquet_optimize` alone.
    subroutine check_optimize_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        type(pf_optimize_info)    :: info
        type(pf_optimize_history) :: record
        type(pf_simplex_solver)   :: solver
        type(surface_objective)   :: obj
        type(surface_constrained) :: constrained
        real(real64)              :: xs, fs, x(1), fmin, lower(1), upper(1), c(1)
        real(real64), allocatable :: population(:,:)
        logical                   :: ok
        character(len=:), allocatable :: verbosity, stream
        ! The four abstract interfaces and the abstract solver type, NAMED through this import
        ! alone: every one is declared here, and no call below would reach any of them. They are
        ! nullified rather than initialised in place, because a local with an initialiser is
        ! implicitly `save` and this suite runs concurrently.
        procedure(pf_objective_eval), pointer   :: eval_p
        procedure(pf_objective_func), pointer   :: func_p
        procedure(pf_constraint_count), pointer :: count_p
        procedure(pf_constraint_eval), pointer  :: constr_p
        procedure(pf_local_run), pointer        :: run_p
        class(pf_local_solver), allocatable     :: any_solver

        what = ""

        ! Brent on a bracket, with both records and the short answer.
        ok = .false.
        call pf_minimize_scalar(offset_square, -3.0_real64, 3.0_real64, xs, fs, converged=ok, &
                                info=info, history=record)
        if (abs(xs - 2.0_real64) > 1.0e-6_real64) what = "pf_minimize_scalar"
        if (what == "" .and. .not. ok) what = "pf_minimize_scalar converged="
        if (what == "" .and. (ok .neqv. info%converged)) what = "converged= against info%converged"
        if (what == "" .and. .not. info%converged) what = "pf_optimize_info%converged"
        if (what == "" .and. info%status /= PF_OPT_OK) what = "PF_OPT_OK"
        if (what == "" .and. record%n /= info%neval) what = "pf_optimize_history%n"

        ! The simplex, from a start point and a step.
        if (what == "") then
            x = [5.0_real64]
            call pf_minimize_simplex(offset_square, x, fmin, [0.5_real64], 0.0_real64, &
                                     atol=1.0e-12_real64, info=info)
            if (abs(x(1) - 2.0_real64) > 1.0e-5_real64) what = "pf_minimize_simplex"
            if (what == "" .and. info%spread < 0.0_real64) what = "pf_optimize_info%spread"
        end if

        ! The local-solver object, which the multistart driver will run.
        if (what == "") then
            lower = [-4.0_real64]
            upper = [6.0_real64]
            x = [5.0_real64]
            obj%calls = 0
            ! An explicit step_fraction, so the vertices do not land symmetrically either side of
            ! the minimiser: the default 0.1 over this box gives a step of exactly 1 from a start
            ! of 5, which is the straddle `test_optimize.f90` covers on purpose -- correct, and
            ! not what a reachability check wants to assert.
            solver%step_fraction = 0.037_real64
            call solver%run(obj, x, fmin, lower, upper, info)
            if (abs(x(1) - 2.0_real64) > 1.0e-3_real64) what = "pf_simplex_solver%run"
            if (what == "" .and. obj%calls /= info%neval) what = "pf_objective%eval"
        end if

        ! Differential evolution over a box, with its own record and its final population.
        if (what == "") then
            lower = [-4.0_real64]
            upper = [6.0_real64]
            call pf_minimize_de(offset_square, lower, upper, 7_int64, x, fmin, np=8, &
                                ftarget=1.0e-10_real64, max_gen=400, info=info, &
                                population=population)
            if (abs(x(1) - 2.0_real64) > 1.0e-4_real64) what = "pf_minimize_de"
            if (what == "" .and. size(population, 2) /= 8) what = "pf_minimize_de population="
            if (what == "" .and. info%nonfinite /= 0) what = "pf_optimize_info%nonfinite"
        end if

        ! The multistart driver, with the solver object above and the threads the clamp allows.
        if (what == "") then
            x = [0.0_real64]
            call pf_minimize_multistart(offset_square, lower, upper, 7_int64, x, fmin, nstart=4, &
                                        solver=solver, merge_tol=1.0e-3_real64, threads=2, info=info)
            if (abs(x(1) - 2.0_real64) > 1.0e-3_real64) what = "pf_minimize_multistart"
            if (what == "" .and. info%nminima < 1) what = "pf_optimize_info%nminima"
            if (what == "" .and. info%nlimit < 0) what = "pf_optimize_info%nlimit"
        end if

        ! The team counter: public, and reachable from no other surface test. It is 1 without
        ! OpenMP and after a `threads = 1` run, so the only thing assertable here is that the name
        ! resolves and answers a team size at all.
        if (what == "" .and. parquet_debug_optimize_threads_used() < 1) then
            what = "parquet_debug_optimize_threads_used"
        end if

        ! The output pair, re-exported because the thread clamp can warn once per process.
        if (what == "") then
            call parquet_get_verbosity(verbosity)
            call parquet_set_verbosity(verbosity)
            call parquet_get_message_stream(stream)
            call parquet_set_message_stream(stream)
            if (len_trim(verbosity) == 0) what = "parquet_get_verbosity"
        end if

        ! `pf_constrained_objective`, declared HERE rather than in `parquet_prima`. No engine of
        ! this module accepts one -- handing it over is an `error stop` -- so the bindings are
        ! called directly.
        if (what == "") then
            if (constrained%n_constraints() /= 1) what = "pf_constrained_objective%n_constraints"
            if (what == "") then
                call constrained%constraints([0.5_real64], c)
                if (c(1) > 0.0_real64) what = "pf_constrained_objective%constraints"
            end if
        end if

        ! `pf_objective_func` as a NAME: a plain callback is accepted by shape, so the interface
        ! itself is only exercised by declaring something of it. The pointer is called, so the
        ! declaration is not dead.
        if (what == "") then
            func_p => offset_square
            if (abs(func_p([2.0_real64])) > 0.0_real64) what = "pf_objective_func"
        end if

        ! `pf_local_solver` and its deferred `run`, through the ABSTRACT type -- which is how
        ! `pf_minimize_multistart` reaches whichever solver it was given.
        if (what == "") then
            allocate(any_solver, source=solver)
            x = [5.0_real64]
            obj%calls = 0
            call any_solver%run(obj, x, fmin, lower, upper, info)
            if (abs(x(1) - 2.0_real64) > 1.0e-3_real64) what = "pf_local_solver%run"
        end if

        ! The three remaining interfaces, named and nothing more: no procedure in this file has a
        ! matching passed-object dummy, so declaring one of each is the only way to name them.
        nullify(eval_p)
        nullify(count_p)
        nullify(constr_p)
        nullify(run_p)
        if (what == "" .and. associated(eval_p)) what = "pf_objective_eval"
        if (what == "" .and. associated(count_p)) what = "pf_constraint_count"
        if (what == "" .and. associated(constr_p)) what = "pf_constraint_eval"
        if (what == "" .and. associated(run_p)) what = "pf_local_run"

        ! The remaining status codes are reachable by name through this import alone.
        if (what == "" .and. PF_OPT_LIMIT == PF_OPT_TARGET) what = "PF_OPT_LIMIT"
        if (what == "" .and. PF_OPT_NONFINITE == PF_OPT_ROUNDOFF) what = "PF_OPT_NONFINITE"
        if (what == "" .and. PF_OPT_INFEASIBLE == PF_OPT_OK) what = "PF_OPT_INFEASIBLE"

    end subroutine check_optimize_surface

    !> `(x - 2)**2` for the constrained type.
    function surface_constrained_eval(self, x) result(f)
        class(surface_constrained), intent(inout) :: self !! the objective
        real(real64), intent(in)                  :: x(:) !! the point
        real(real64)                              :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2

    end function surface_constrained_eval

    !> How many constraint values `constraints` fills.
    function surface_count(self) result(m)
        class(surface_constrained), intent(in) :: self !! the objective
        integer                                :: m    !! the number of constraints

        m = 1

    end function surface_count

    !> `x - 1 <= 0`.
    subroutine surface_constr(self, x, c)
        class(surface_constrained), intent(inout) :: self !! the objective
        real(real64), intent(in)                  :: x(:) !! the point
        real(real64), intent(out)                 :: c(:) !! the constraint values

        c(1) = x(1) - 1.0_real64

    end subroutine surface_constr

    !> `(x - 2)**2` in one variable, whose minimiser is `2`. A module procedure, because a
    !! callback in this library is never an internal one.
    function offset_square(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2

    end function offset_square

end module test_module_surface_optimize

module test_module_surface_prima
    use parquet_prima                    ! THE ONLY library import.
    use iso_fortran_env, only : real64
    implicit none
    private
    public :: check_prima_surface

    !> An objective object, so `pf_objective` itself is extended through this import alone --
    !! which is what proves `parquet_prima` re-exports it rather than merely naming it.
    type, extends(pf_objective) :: prima_surface_objective
        integer :: calls = 0 !! evaluations, so the caller can see `eval` ran
    contains
        procedure :: eval => prima_surface_objective_eval !! `(x - 2)**2`, counting the call.
    end type prima_surface_objective

    !> A CONSTRAINED objective, reached through this import alone: `pf_constrained_objective` and
    !! both its deferred bindings are re-exported too, which is what `pf_minimize_cobyla` below
    !! needs them to be.
    type, extends(pf_constrained_objective) :: prima_surface_constrained
        integer :: calls = 0 !! evaluations, unused but for keeping the type honest
    contains
        procedure :: eval => prima_surface_constrained_eval  !! `(x - 2)**2`.
        procedure :: n_constraints => prima_surface_count    !! one constraint.
        procedure :: constraints => prima_surface_constr     !! `x - 1 <= 0`.
    end type prima_surface_constrained

contains

    !> `(x - 2)**2`, counting the call.
    function prima_surface_objective_eval(self, x) result(f)
        class(prima_surface_objective), intent(inout) :: self !! the objective
        real(real64), intent(in)                      :: x(:) !! the point
        real(real64)                                  :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2
        self%calls = self%calls + 1

    end function prima_surface_objective_eval

    !> `(x - 2)**2` again, for the constrained type.
    function prima_surface_constrained_eval(self, x) result(f)
        class(prima_surface_constrained), intent(inout) :: self !! the objective
        real(real64), intent(in)                        :: x(:) !! the point
        real(real64)                                    :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2
        self%calls = self%calls + 1

    end function prima_surface_constrained_eval

    !> How many constraint values `constraints` fills.
    function prima_surface_count(self) result(m)
        class(prima_surface_constrained), intent(in) :: self !! the objective
        integer                                      :: m    !! the number of constraints

        m = 1

    end function prima_surface_count

    !> `x - 1 <= 0`.
    subroutine prima_surface_constr(self, x, c)
        class(prima_surface_constrained), intent(inout) :: self !! the objective
        real(real64), intent(in)                        :: x(:) !! the point
        real(real64), intent(out)                       :: c(:) !! the constraint values

        self%calls = self%calls + 0
        c(1) = x(1) - 1.0_real64

    end subroutine prima_surface_constr

    !> Exercises every entry point of `parquet_prima` through `use parquet_prima` alone.
    !!
    !! An entry point declared in the spec with no implementation links until something calls it,
    !! so this is the test that calls every one: `pf_minimize_bobyqa` in both forms,
    !! `pf_minimize_lincoa` in both forms, `pf_minimize_cobyla`, and `pf_bobyqa_solver%run`.
    subroutine check_prima_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        type(pf_optimize_info)          :: info
        type(pf_optimize_history)       :: record
        type(pf_bobyqa_solver)          :: solver
        type(prima_surface_objective)   :: obj
        type(prima_surface_constrained) :: constrained
        real(real64)                    :: x(1), fmin, lower(1), upper(1), c(1)
        real(real64)                    :: a_ineq(1, 1), b_ineq(1)
        logical                         :: ok
        ! The four abstract interfaces and the abstract solver type, NAMED through this import
        ! alone. `prima.md` says this module re-exports every shared name `parquet_optimize`
        ! declares, and these five are the ones no call below would reach: a name dropped from
        ! `parquet_prima`'s `public ::` list stops this file compiling, which is the assertion.
        ! They are nullified rather than initialised in place, because a local with an
        ! initialiser is implicitly `save` and this suite runs concurrently.
        procedure(pf_objective_eval), pointer   :: eval_p
        procedure(pf_objective_func), pointer   :: func_p
        procedure(pf_constraint_count), pointer :: count_p
        procedure(pf_constraint_eval), pointer  :: constr_p
        procedure(pf_local_run), pointer        :: run_p
        class(pf_local_solver), allocatable     :: any_solver

        what = ""
        a_ineq(1, 1) = 1.0_real64
        b_ineq = 1.0_real64

        ! BOBYQA with a plain function, with both records.
        x = [5.0_real64]
        lower = [-3.0_real64]
        upper = [8.0_real64]
        ok = .false.
        call pf_minimize_bobyqa(prima_offset_square, x, fmin, lower=lower, upper=upper, &
                                rhobeg=0.5_real64, rhoend=1.0e-8_real64, converged=ok, info=info, &
                                history=record)
        if (abs(x(1) - 2.0_real64) > 1.0e-6_real64) what = "pf_minimize_bobyqa"
        if (what == "" .and. .not. ok) what = "pf_minimize_bobyqa converged="
        if (what == "" .and. (ok .neqv. info%converged)) what = "converged= against info%converged"
        if (what == "" .and. .not. info%converged) what = "pf_optimize_info%converged"
        if (what == "" .and. info%status /= PF_OPT_OK) what = "PF_OPT_OK"
        if (what == "" .and. record%n /= info%neval) what = "pf_optimize_history%n"
        if (what == "" .and. info%rho > 1.0e-7_real64) what = "pf_optimize_info%rho"

        ! BOBYQA with an objective OBJECT, which is the re-exported `pf_objective`.
        if (what == "") then
            x = [5.0_real64]
            call pf_minimize_bobyqa(obj, x, fmin, lower=lower, upper=upper, rhobeg=0.5_real64, &
                                    rhoend=1.0e-8_real64, scale=[1.0_real64], info=info)
            if (abs(x(1) - 2.0_real64) > 1.0e-6_real64) what = "pf_objective through parquet_prima"
            if (what == "" .and. obj%calls /= info%neval) what = "the caller's own object"
        end if

        ! The solver object, which `pf_minimize_multistart` takes as a `pf_local_solver`.
        if (what == "") then
            solver%rhoend = 1.0e-8_real64
            x = [5.0_real64]
            call solver%run(obj, x, fmin, lower, upper, info)
            if (abs(x(1) - 2.0_real64) > 1.0e-5_real64) what = "pf_bobyqa_solver%run"
        end if

        ! The constrained type, reached through this import alone.
        if (what == "") then
            if (constrained%n_constraints() /= 1) what = "pf_constrained_objective%n_constraints"
            if (what == "") then
                call constrained%constraints([0.5_real64], c)
                if (c(1) > 0.0_real64) what = "pf_constrained_objective%constraints"
            end if
        end if

        ! LINCOA, both forms, with one linear constraint that binds: `x <= 1` cuts off the free
        ! minimiser at `2`, so the answer is `1`.
        if (what == "") then
            x = [0.0_real64]
            call pf_minimize_lincoa(prima_offset_square, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                    rhobeg=0.5_real64, rhoend=1.0e-8_real64, ctol=1.0e-9_real64, &
                                    info=info)
            if (abs(x(1) - 1.0_real64) > 1.0e-6_real64) what = "pf_minimize_lincoa"
            if (what == "" .and. info%cstrv > 1.0e-9_real64) what = "pf_optimize_info%cstrv"
        end if
        if (what == "") then
            x = [0.0_real64]
            call pf_minimize_lincoa(obj, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                    rhobeg=0.5_real64, rhoend=1.0e-8_real64, info=info)
            if (abs(x(1) - 1.0_real64) > 1.0e-6_real64) what = "pf_minimize_lincoa with an object"
        end if

        ! COBYLA, which is the one entry point taking a `pf_constrained_objective`. The constraint
        ! `x - 1 <= 0` binds the same way.
        if (what == "") then
            x = [0.0_real64]
            call pf_minimize_cobyla(constrained, x, fmin, rhobeg=0.5_real64, &
                                    rhoend=1.0e-8_real64, info=info)
            if (abs(x(1) - 1.0_real64) > 1.0e-5_real64) what = "pf_minimize_cobyla"
            if (what == "" .and. info%status == PF_OPT_INFEASIBLE) what = "PF_OPT_INFEASIBLE"
        end if

        ! `pf_objective_func` as a NAME, not just as a matching procedure: a plain callback is
        ! accepted by shape, so the interface itself is only exercised by declaring something of
        ! it. The pointer is called, so the declaration is not dead.
        if (what == "") then
            func_p => prima_offset_square
            if (abs(func_p([2.0_real64])) > 0.0_real64) what = "pf_objective_func"
        end if

        ! `pf_local_solver` and its deferred `run`, reached through the ABSTRACT type rather than
        ! through `pf_bobyqa_solver` directly -- which is how `pf_minimize_multistart` reaches it.
        if (what == "") then
            allocate(any_solver, source=solver)
            x = [5.0_real64]
            call any_solver%run(obj, x, fmin, lower, upper, info)
            if (abs(x(1) - 2.0_real64) > 1.0e-5_real64) what = "pf_local_solver%run"
        end if

        ! The three remaining interfaces, named and nothing more: no procedure in this file has a
        ! matching passed-object dummy, so declaring one of each is the only way to name them.
        nullify(eval_p)
        nullify(count_p)
        nullify(constr_p)
        nullify(run_p)
        if (what == "" .and. associated(eval_p)) what = "pf_objective_eval"
        if (what == "" .and. associated(count_p)) what = "pf_constraint_count"
        if (what == "" .and. associated(constr_p)) what = "pf_constraint_eval"
        if (what == "" .and. associated(run_p)) what = "pf_local_run"

        ! The remaining status codes are reachable by name through this import alone.
        if (what == "" .and. PF_OPT_LIMIT == PF_OPT_TARGET) what = "PF_OPT_LIMIT"
        if (what == "" .and. PF_OPT_NONFINITE == PF_OPT_ROUNDOFF) what = "PF_OPT_NONFINITE"
        if (what == "" .and. PF_OPT_INFEASIBLE == PF_OPT_OK) what = "PF_OPT_INFEASIBLE"

    end subroutine check_prima_surface

    !> `(x - 2)**2` in one variable, whose minimiser is `2`. A module procedure, because a
    !! callback in this library is never an internal one.
    function prima_offset_square(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2

    end function prima_offset_square

end module test_module_surface_prima

!> `parquet_cosmology` alone: both `%init` forms, one binding of each of the seven families, the
!! three free functions, and the statement that the module re-exports no setting.
module test_module_surface_cosmology
    use parquet_cosmology                ! THE ONLY library import.
    use iso_fortran_env, only : real64
    implicit none
    private
    public :: check_cosmology_surface

contains

    !> Exercises each public name through `use parquet_cosmology` alone.
    subroutine check_cosmology_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        type(pf_cosmology)            :: c, sim
        character(len=:), allocatable :: text
        real(real64), allocatable     :: masses(:)
        real(real64)                  :: z(3), d(3)

        what = ""

        ! The named form, and one binding of each family over it.
        call c%init("Planck18")
        if (.not. c%is_initialised()) what = "%init(name)"
        if (what == "" .and. abs(c%comoving_distance(1.0_real64) - 3395.63_real64) > 1.0_real64) &
            what = "%comoving_distance"
        if (what == "" .and. abs(c%age(0.0_real64) - 13.7869_real64) > 1.0e-3_real64) what = "%age"
        if (what == "" .and. abs(c%efunc(0.0_real64) - 1.0_real64) > 1.0e-12_real64) what = "%efunc"
        if (what == "" .and. abs(c%hubble(0.0_real64) - c%h0()) > 1.0e-9_real64) what = "%hubble"
        if (what == "" .and. .not. c%is_flat()) what = "%is_flat"
        if (what == "" .and. .not. c%has_massive_nu()) what = "%has_massive_nu"
        if (what == "") then
            call c%get_name(text)
            if (text /= "Planck18") what = "%get_name"
        end if
        if (what == "") then
            call c%describe(text)
            if (len(text) < 20) what = "%describe"
        end if
        if (what == "") then
            call c%m_nu(masses)
            if (size(masses) /= 3) what = "%m_nu"
        end if

        ! The elemental shape, and the inverse.
        z = [0.1_real64, 1.0_real64, 3.0_real64]
        d = c%comoving_distance(z)
        if (what == "" .and. abs(c%z_at_comoving_distance(d(2)) - 1.0_real64) > 1.0e-9_real64) &
            what = "%z_at_comoving_distance"
        if (what == "" .and. abs(c%z_at_lookback_time(c%lookback_time(2.0_real64)) - 2.0_real64) &
            > 1.0e-9_real64) what = "%z_at_lookback_time"

        ! What the universe is made of at a redshift, and the three further inverses.
        if (what == "" .and. abs(c%om(0.0_real64) - c%om0()) > 1.0e-12_real64) what = "%om"
        if (what == "" .and. abs(c%om(2.0_real64) + c%ode(2.0_real64) + c%ok(2.0_real64) &
            + c%ogamma(2.0_real64) + c%onu(2.0_real64) - 1.0_real64) > 1.0e-12_real64) &
            what = "%om/%ode/%ok/%ogamma/%onu"
        if (what == "" .and. abs(c%tcmb(1.0_real64) - 2.0_real64 * c%tcmb0()) > 1.0e-12_real64) &
            what = "%tcmb"
        if (what == "" .and. c%w(1.0_real64) /= -1.0_real64) what = "%w"
        if (what == "" .and. c%de_density_scale(1.0_real64) /= 1.0_real64) what = "%de_density_scale"
        if (what == "" .and. abs(c%critical_density(0.0_real64) - 1.2705e11_real64) &
            > 1.0e8_real64) what = "%critical_density"
        if (what == "" .and. abs(c%lookback_distance(1.0_real64) - 2433.05_real64) > 1.0_real64) &
            what = "%lookback_distance"
        if (what == "" .and. abs(c%odm0() - (c%om0() - c%ob0())) > 1.0e-15_real64) what = "%odm0"
        if (what == "" .and. abs(c%z_at_age(c%age(2.0_real64)) - 2.0_real64) > 1.0e-9_real64) &
            what = "%z_at_age"
        if (what == "" .and. abs(c%z_at_luminosity_distance(c%luminosity_distance(1.5_real64)) &
            - 1.5_real64) > 1.0e-9_real64) what = "%z_at_luminosity_distance"
        if (what == "" .and. abs(c%z_at_distmod(c%distmod(1.5_real64)) - 1.5_real64) &
            > 1.0e-9_real64) what = "%z_at_distmod"

        ! The growth of structure, and the sound horizon. Neither reads the four tabulated
        ! integrals: growth has a table of its own, filled by an ODE, and the sound horizon
        ! tabulates nothing at all, so a standalone build could break on either while every
        ! binding above still answered.
        if (what == "" .and. c%growth_factor(0.0_real64) /= 1.0_real64) what = "%growth_factor"
        if (what == "" .and. abs(c%growth_rate(0.0_real64) - 0.52191_real64) > 1.0e-3_real64) &
            what = "%growth_rate"
        if (what == "" .and. abs(c%sound_horizon(0.0_real64) - 1223.07_real64) > 1.0_real64) &
            what = "%sound_horizon"
        if (what == "" .and. abs(c%r_drag() - 147.18_real64) > 0.1_real64) what = "%r_drag"
        if (what == "" .and. abs(c%z_drag() - 1060.3_real64) > 1.0_real64) what = "%z_drag"
        if (what == "" .and. abs(c%z_eq() - 3387.4_real64) > 1.0_real64) what = "%z_eq"

        ! The parameter form, and the parameter queries.
        call sim%init(h0 = 70.0_real64, om0 = 0.3_real64, name = "my_sim")
        if (what == "" .and. abs(sim%little_h() - 0.7_real64) > 1.0e-12_real64) what = "%little_h"
        if (what == "" .and. sim%ok0() /= 0.0_real64) what = "%ok0"
        if (what == "" .and. abs(sim%zmax() - 1100.0_real64) > 1.0e-12_real64) what = "%zmax"
        if (what == "" .and. abs(sim%hubble_distance() - 4282.7494_real64) > 1.0e-3_real64) &
            what = "%hubble_distance"
        if (what == "" .and. abs(sim%hubble_time() - 13.9683_real64) > 1.0e-3_real64) &
            what = "%hubble_time"

        ! The three free functions.
        if (what == "" .and. abs(pf_z2zeta(1.0_real64) - log(2.0_real64)) > 1.0e-15_real64) &
            what = "pf_z2zeta"
        if (what == "" .and. abs(pf_zeta2z(log(2.0_real64)) - 1.0_real64) > 1.0e-15_real64) &
            what = "pf_zeta2z"
        if (what == "" .and. abs(pf_z_combine(1.0_real64, 1.0_real64) - 3.0_real64) > 1.0e-15_real64) &
            what = "pf_z_combine"

        ! The module re-exports NO setting: it reads none and prints nothing at all. There is
        ! therefore no knob to round-trip here, and that absence is the assertion.
        call c%clear()
        if (what == "" .and. c%is_initialised()) what = "%clear"

    end subroutine check_cosmology_surface

end module test_module_surface_cosmology

!> `parquet_root` alone: the generic in both forms, the callback interfaces, the growth policy
!! and its four modes, the info record and its three status codes, the history, and no settings
!! knob at all -- all fourteen public names.
!!
!! **One library import, and it must stay that way.** This module's row in the entry-module table
!! makes two claims nothing else checks: that `use parquet_root` compiles two Fortran files, and
!! that it re-exports no setting -- root finding reads none and can print nothing, so there is no
!! knob for it to re-export.
module test_module_surface_root
    use parquet_root                     ! THE ONLY library import.
    use iso_fortran_env, only : real64
    implicit none
    private
    public :: check_root_surface

    !> `x*x - 2` as an object, so the object specific is reached through this import alone.
    type, extends(pf_rootfun) :: surface_sq2
    contains
        procedure :: eval => surface_sq2_eval !! Evaluates `x*x - 2`.
    end type surface_sq2

contains

    !> Exercises each public name through `use parquet_root` alone.
    subroutine check_root_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        type(pf_root_info)         :: info
        type(pf_root_history)      :: hist
        type(pf_bracket_expansion) :: grow
        type(surface_sq2)          :: obj
        real(real64)               :: x
        ! `pf_rootfun_eval` is NAMED through this import alone, and nothing else here would
        ! reach it: an extension declares `procedure :: eval => ...` without naming the
        ! interface, so making it private compiled and passed before this line existed.
        ! `pf_rootfun_func` needs no such declaration -- passing a plain function to
        ! `pf_find_root` below fails to compile without it. Nullified rather than initialised
        ! in place, because a local with an initialiser is implicitly `save` and this suite
        ! runs concurrently.
        procedure(pf_rootfun_eval), pointer :: eval_p

        what = ""

        ! The plain-function specific, with the info record and the history.
        call pf_find_root(surface_sq2_func, 0.0_real64, 2.0_real64, x, info=info, history=hist)
        if (abs(x - sqrt(2.0_real64)) > 1.0e-14_real64) what = "pf_find_root"
        if (what == "" .and. info%status /= PF_ROOT_OK) what = "PF_ROOT_OK"
        if (what == "" .and. hist%n /= info%neval) what = "pf_root_history%n"

        ! The object specific, grown upward by the policy.
        if (what == "") then
            grow%mode = PF_EXPAND_UP
            call pf_find_root(obj, 0.0_real64, 0.01_real64, x, expand=grow, info=info)
            if (abs(x - sqrt(2.0_real64)) > 1.0e-14_real64 .or. info%nexpand < 1) &
                what = "pf_bracket_expansion"
        end if

        ! A missing sign change is a status reachable by name through this import alone.
        if (what == "") then
            call pf_find_root(surface_sq2_func, 2.0_real64, 3.0_real64, x, info=info)
            if (info%status /= PF_ROOT_NO_BRACKET) what = "PF_ROOT_NO_BRACKET"
        end if

        ! The callback interface, named and nothing more: no procedure in this module has a
        ! matching passed-object dummy, so declaring one is the only way to name it.
        nullify(eval_p)
        if (what == "" .and. associated(eval_p)) what = "pf_rootfun_eval"

        ! The remaining codes are reachable by name.
        if (what == "" .and. PF_ROOT_LIMIT == PF_ROOT_OK) what = "PF_ROOT_LIMIT"
        if (what == "" .and. (PF_EXPAND_NONE == PF_EXPAND_DOWN .or. PF_EXPAND_BOTH == PF_EXPAND_UP)) &
            what = "PF_EXPAND_*"

    end subroutine check_root_surface

    !> `x*x - 2`, whose positive root is `sqrt(2)`. A module procedure, because a callback in this
    !! library is never an internal one.
    function surface_sq2_func(x) result(y)
        real(real64), intent(in) :: x !! where to evaluate
        real(real64)             :: y !! the value there

        y = x*x - 2.0_real64

    end function surface_sq2_func

    !> Evaluates `x*x - 2` for the object form.
    function surface_sq2_eval(self, x) result(f)
        class(surface_sq2), intent(inout) :: self !! the function object, which does not change
        real(real64), intent(in)          :: x    !! where to evaluate
        real(real64)                      :: f    !! the value there

        f = surface_sq2_func(x)

    end function surface_sq2_eval

end module test_module_surface_root

!> `parquet_transform` alone: both transforms under both norms, and the two length helpers.
!!
!! **One library import, and it must stay that way.** This module's row in the entry-module table
!! makes two claims nothing else checks: that `use parquet_transform` compiles two Fortran files,
!! and that it re-exports no setting -- the transform reads none and can print nothing, so there is
!! no knob for it to re-export.
module test_module_surface_transform
    use parquet_transform                ! THE ONLY library import.
    use iso_fortran_env, only : real64
    implicit none
    private
    public :: check_transform_surface

contains

    !> Exercises each public name through `use parquet_transform` alone.
    subroutine check_transform_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        real(real64) :: x(8), y(8), back(8)
        integer      :: n

        what = ""
        x = [1.0_real64, -2.0_real64, 3.5_real64, 0.25_real64, -1.75_real64, 4.0_real64, -0.5_real64, &
             2.25_real64]

        ! The forward transform: y(1) is twice the sum, the convention's factor of two.
        call pf_dct(x, y)
        if (abs(y(1) - 2.0_real64*sum(x)) > 1.0e-13_real64) what = "pf_dct"

        ! The inverse undoes it, under the orthonormal norm too.
        if (what == "") then
            call pf_idct(y, back)
            if (maxval(abs(back - x)) > 1.0e-13_real64) what = "pf_idct"
        end if
        if (what == "") then
            call pf_dct(x, y, norm="ortho")
            call pf_idct(y, back, norm="ortho")
            if (maxval(abs(back - x)) > 1.0e-13_real64) what = "norm=""ortho"""
        end if

        ! The two length helpers.
        if (what == "") then
            if (.not. pf_is_pow2(1024) .or. pf_is_pow2(1000)) what = "pf_is_pow2"
        end if
        if (what == "") then
            n = pf_next_pow2(1000)
            if (n /= 1024) what = "pf_next_pow2"
        end if

    end subroutine check_transform_surface

end module test_module_surface_transform

!> `parquet_kde` alone: a fit, a query, a grid, and the output pair `%print` reads.
!!
!! **One library import, and it must stay that way.** This module's row in the entry-module table
!! makes two claims nothing else checks: that `use parquet_kde` compiles what the footprint file
!! says, and that it re-exports the verbosity and message-stream pair its `%print` reads -- without
!! them a program importing only this module could not silence the printer.
module test_module_surface_kde
    use parquet_kde                      ! THE ONLY library import.
    use iso_fortran_env, only : real64
    implicit none
    private
    public :: check_kde_surface

contains

    !> Exercises both estimators and the re-exported output pair through `use parquet_kde` alone.
    subroutine check_kde_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        type(pf_kde) :: k
        type(pf_kde_grid) :: g
        real(real64) :: f, cells(4)
        logical :: ok
        character(len=:), allocatable :: saved, now

        what = ""
        call k%fit([-1.0_real64, 0.0_real64, 1.0_real64], kernel="gaussian", bandwidth=1.0_real64, ok=ok)
        call k%pdf(0.0_real64, f)
        ! Three unit Gaussians at -1, 0 and 1, read at 0: (phi(1) + phi(0) + phi(1))/3, over the
        ! mass the five-sd cut keeps.
        if (.not. ok .or. abs(f - (2.0_real64*0.24197072451914337_real64 + 0.3989422804014327_real64) &
            /3.0_real64/(1.0_real64 - 5.7330314375838782e-7_real64)) > 1.0e-15_real64) what = "pf_kde%pdf"

        ! The streaming form: the same three points into four cells, each kernel inside the range,
        ! so the cells hold the whole weight.
        if (what == "") then
            call g%init(4, -2.0_real64, 2.0_real64, 0.1_real64)
            call g%add([-1.0_real64, 0.0_real64, 1.0_real64], finish=.true.)
            call g%density(cells)
            if (abs(sum(cells)*g%step() - 1.0_real64) > 1.0e-15_real64 .or. g%n_valid() /= 3) &
                what = "pf_kde_grid%density"
        end if

        ! The output pair, round-tripped: the module re-exports both halves.
        if (what == "") then
            call parquet_get_verbosity(saved)
            call parquet_set_verbosity("silent")
            call parquet_get_verbosity(now)
            call parquet_set_verbosity(saved)
            if (now /= "silent") what = "parquet_set_verbosity"
        end if
        if (what == "") then
            call parquet_get_message_stream(saved)
            call parquet_set_message_stream("stderr")
            call parquet_get_message_stream(now)
            call parquet_set_message_stream(saved)
            if (now /= "stderr") what = "parquet_set_message_stream"
        end if

    end subroutine check_kde_surface

end module test_module_surface_kde

!> `parquet_table_example` alone: the `generated_table_quickstart` example on
!! doc/pages/utilities/generated-tables.md, compiled with exactly the two `use` lines the page
!! shows.
!!
!! **This module is here for what it does NOT import, like every other one in this file, and it is
!! the only one whose subject is not an entry module.** The page's example is already mirrored, by
!! `test_generated_table_quickstart_example` in test/test_examples.f90, which asserts the three
!! values its trailing comment prints. But that module opens `use parquet`, so every name in the
!! library is in scope there and the mirror cannot notice the page's own import list becoming
!! insufficient -- the property this file exists to assert, stated in its own header above. The
!! narrowing that made it matter is the generated module's: it imports `parquet_io`,
!! `parquet_tables` and `parquet_columns` rather than the facade, so `pf_sort`, `pf_mean` and the
!! sky and numerics tiers are no longer in scope inside it, and the next line added to that
!! example may well need a third `use`.
!!
!! So this is a COMPILE test first: an example needing a name these two imports do not provide
!! stops building, the way `test_facade_covers_every_layer` does. The value assertions are here so
!! the calls cannot be optimised away, and they are the same three the page prints.
module test_module_surface_generated_table
    use parquet_table_example, only : parquet_table_test   ! THE ONLY library import.
    use iso_fortran_env, only : int64, real64
    implicit none
    private
    public :: check_generated_table_surface

contains

    !> Runs the page's quickstart with the page's own `use` list and nothing else.
    subroutine check_generated_table_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        type(parquet_table_test) :: t
        real(real64), pointer :: ra(:), one

        what = ""

        ! The example, statement for statement.
        call t%init_empty(3)
        call t%set("uberid", [101_int64, 102_int64, 103_int64])
        call t%set("ra", [10.5_real64, 20.25_real64, 30.125_real64])
        ra  => t%ra()
        one => t%ra(2)

        ! The three values its trailing comment shows: `3 20.250000 20.291667`.
        if (t%nrows() /= 3_int64) what = "%nrows() is not 3"
        if (what == "" .and. abs(one - 20.25_real64) > 1.0e-12_real64) what = "%ra(2) is not 20.25"
        if (what == "" .and. abs(sum(ra) / t%nrows() - 20.291666666666668_real64) &
            > 1.0e-9_real64) what = "sum(ra) / %nrows() is not 20.291667"
    end subroutine check_generated_table_surface

end module test_module_surface_generated_table

module test_module_surface
    use test_module_surface_generated_table, only : check_generated_table_surface
    use test_module_surface_io, only : check_io_surface
    use test_module_surface_argsort, only : check_argsort_surface
    use test_module_surface_sorting, only : check_sorting_surface
    use test_module_surface_stats, only : check_stats_surface
    use test_module_surface_strings, only : check_strings_surface
    use test_module_surface_sampling, only : check_sampling_surface
    use test_module_surface_version, only : check_version_surface
    use test_module_surface_utils, only : check_utils_surface
    use test_module_surface_integrate, only : check_integrate_surface
    use test_module_surface_interpolate, only : check_interpolate_surface
    use test_module_surface_optimize, only : check_optimize_surface
    use test_module_surface_prima, only : check_prima_surface
    use test_module_surface_cosmology, only : check_cosmology_surface
    use test_module_surface_root, only : check_root_surface
    use test_module_surface_transform, only : check_transform_surface
    use test_module_surface_kde, only : check_kde_surface
    use test_module_surface_logging, only : check_logging_surface
    use test_module_surface_toml, only : check_toml_surface
    use test_module_surface_spatial, only : check_spatial_surface
    use test_module_surface_healpix, only : check_healpix_surface
    use test_module_surface_sphere, only : check_sphere_surface
    use test_module_surface_skycoord, only : check_skycoord_surface
    use test_module_surface_columns, only : check_columns_surface
    use test_module_surface_list, only : check_list_surface
    use test_module_surface_struct, only : check_struct_surface
    use test_module_surface_map, only : check_map_surface
    use test_module_surface_random, only : check_random_surface
    use test_module_surface_index, only : check_index_surface
    use test_module_surface_tables, only : check_tables_surface
    use parquet_settings_base          ! THE ONLY library import -- see the note above.
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use iso_fortran_env, only : int64
    implicit none
    private
    public :: collect_tests_module_surface

contains

    !> Round-trips every knob the leaf holds through its own getter and setter.
    !!
    !! The round trip is deliberately not the point -- `CLAUDE.md` is explicit that set-then-get
    !! passes just as happily against a value that is stored and never read, and the per-knob
    !! observed-effect tests in test_settings.f90 are what grade the behaviour. What this asserts is
    !! that both halves of each pair are REACHABLE from this one import; the assertions exist so the
    !! calls are not optimised away, and the compile is the real test.
    subroutine check_settings_base_surface(ok, what)
        logical, intent(out) :: ok                          !! .true. when every knob round-tripped.
        character(len=:), allocatable, intent(out) :: what   !! the first knob that did not, or "".
        character(len=:), allocatable :: tok
        integer(int64) :: n64, min_elems
        integer :: n_sort, n_string, n_random
        logical :: radix, counting

        what = ""
        ! Every knob is captured and put back at the end, on EVERY path. This suite cannot call
        ! parquet_reset_settings -- that lives in parquet_settings, and importing it here would
        ! defeat the single-import property this whole file exists to assert -- so restoring by hand
        ! is the only option, and leaving a knob set would silently reconfigure later suites.
        n_sort = parquet_get_sort_threads()
        n_string = parquet_get_string_threads()
        n_random = parquet_get_random_threads()
        min_elems = parquet_get_random_parallel_min_elements()
        radix = parquet_get_sort_radix_path()
        counting = parquet_get_sort_counting_path()

        call parquet_set_sort_threads(3)
        if (what == "" .and. parquet_get_sort_threads() /= 3) what = "sort_threads"

        call parquet_set_sort_radix_path(.not. radix)
        if (what == "" .and. parquet_get_sort_radix_path() .eqv. radix) what = "sort_radix_path"

        call parquet_set_sort_counting_path(.not. counting)
        if (what == "" .and. parquet_get_sort_counting_path() .eqv. counting) what = "sort_counting_path"

        call parquet_set_sort_counting_bucket_limit(64_int64)
        n64 = parquet_get_sort_counting_bucket_limit()
        if (what == "" .and. n64 /= 64_int64) what = "sort_counting_bucket_limit"

        call parquet_set_string_threads(2)
        if (what == "" .and. parquet_get_string_threads() /= 2) what = "string_threads"

        call parquet_set_random_threads(2)
        if (what == "" .and. parquet_get_random_threads() /= 2) what = "random_threads"

        call parquet_set_random_parallel_min_elements(77_int64)
        n64 = parquet_get_random_parallel_min_elements()
        if (what == "" .and. n64 /= 77_int64) what = "random_parallel_min_elements"

        ! The output pair, which every module that can emit needs so that a user of that module
        ! alone can silence what it prints.
        call parquet_set_verbosity("silent")
        call parquet_get_verbosity(tok)
        if (what == "" .and. tok /= "silent") what = "verbosity"
        if (what == "" .and. .not. parquet_output_is_suppressed()) what = "verbosity does not suppress"
        call parquet_set_verbosity("normal")

        call parquet_set_message_stream("stderr")
        call parquet_get_message_stream(tok)
        if (what == "" .and. tok /= "stderr") what = "message_stream"
        call parquet_set_message_stream("stdout")

        call parquet_set_sort_threads(n_sort)
        call parquet_set_string_threads(n_string)
        call parquet_set_random_threads(n_random)
        call parquet_set_random_parallel_min_elements(min_elems)
        call parquet_set_sort_radix_path(radix)
        call parquet_set_sort_counting_path(counting)
        call parquet_set_sort_counting_bucket_limit(0_int64)   ! 0 == the factory default
        ok = (what == "")
    end subroutine check_settings_base_surface

    !> Registers this suite.
    subroutine collect_tests_module_surface(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("parquet_settings_base alone exposes get AND set for every knob it holds", &
                test_settings_base_surface), &
            new_unittest("parquet_index alone exposes both types and every knob it reads", &
                test_index_surface), &
            new_unittest("parquet_argsort alone exposes every sorting knob it reads", &
                test_argsort_surface), &
            new_unittest("parquet_sorting alone exposes every sorting knob it reads", &
                test_sorting_surface), &
            new_unittest("parquet_stats alone reduces an array", &
                test_stats_surface), &
            new_unittest("parquet_strings alone exposes every knob it reads", &
                test_strings_surface), &
            new_unittest("parquet_sampling alone exposes every knob it reads", &
                test_sampling_surface), &
            new_unittest("parquet_spatial alone builds an index and exposes every knob it reads", &
                test_spatial_surface), &
            new_unittest("parquet_healpix alone pixelises the sphere and exposes the output pair", &
                         test_healpix_surface), &
            new_unittest("parquet_sphere alone draws in a polygon and a pixel, and exposes no setting", &
                         test_sphere_surface), &
            new_unittest("parquet_skycoord alone converts between every system and measures the sky", &
                         test_skycoord_surface), &
            new_unittest("parquet_utils alone folds text and takes a path apart", &
                         test_utils_surface), &
            new_unittest("parquet_integrate alone integrates and hands back its record", &
                         test_integrate_surface), &
            new_unittest("parquet_interpolate alone builds both interpolants and evaluates them", &
                         test_interpolate_surface), &
            new_unittest("parquet_optimize alone minimises through both engines and a solver", &
                         test_optimize_surface), &
            new_unittest("parquet_prima alone minimises with BOBYQA and hands back its record", &
                         test_prima_surface), &
            new_unittest("parquet_root alone solves in both forms and expands a bracket", &
                         test_root_surface), &
            new_unittest("parquet_cosmology alone builds both ways and answers every family", &
                         test_cosmology_surface), &
            new_unittest("parquet_transform alone transforms, inverts and chooses a length", &
                         test_transform_surface), &
            new_unittest("parquet_kde alone fits, queries and round-trips its output pair", &
                         test_kde_surface), &
            new_unittest("parquet_logging alone configures a logger and emits through it", &
                         test_logging_surface), &
            new_unittest("parquet_toml alone reads a whole configuration", &
                         test_toml_surface), &
            new_unittest("parquet_version alone reports the library version", &
                test_version_surface), &
            new_unittest("parquet_io alone reaches every layer of the read/write API", &
                test_io_surface), &
            new_unittest("parquet_columns alone builds, nulls and reads a column", &
                test_columns_surface), &
            new_unittest("parquet_random alone draws, and exposes no setting", &
                test_random_surface), &
            new_unittest("parquet_tables alone round-trips a table through a file", &
                test_tables_surface), &
            new_unittest("parquet_list alone builds, reads and adopts a list column", &
                test_list_surface), &
            new_unittest("parquet_struct alone builds, reads and adopts a struct column", &
                test_struct_surface), &
            new_unittest("parquet_map alone builds, reads and adopts a map column", &
                test_map_surface), &
            new_unittest("generated-tables.md's quickstart compiles with the two `use` lines it shows", &
                test_generated_table_surface) ]
    end subroutine collect_tests_module_surface

    !> The test-drive wrapper over check_generated_table_surface.
    subroutine test_generated_table_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_generated_table_surface(what)
        call check(error, what == "", "generated-tables.md's quickstart did not run through its " // &
            "own two `use` lines: " // what)
    end subroutine test_generated_table_surface

    !> The test-drive wrapper over check_columns_surface.
    subroutine test_columns_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_columns_surface(what)
        call check(error, what == "", "the column was not usable through `use parquet_columns` alone: " // what)
    end subroutine test_columns_surface

    !> The test-drive wrapper over check_list_surface.
    subroutine test_list_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_list_surface(what)
        call check(error, what == "", "the list column was not usable through `use parquet_list` alone: " // what)
    end subroutine test_list_surface

    !> The test-drive wrapper over check_struct_surface.
    subroutine test_struct_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_struct_surface(what)
        call check(error, what == "", "the struct column was not usable through `use parquet_struct` alone: " // what)
    end subroutine test_struct_surface

    !> The test-drive wrapper over check_map_surface.
    subroutine test_map_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_map_surface(what)
        call check(error, what == "", "the map column was not usable through `use parquet_map` alone: " // what)
    end subroutine test_map_surface

    !> The test-drive wrapper over check_random_surface.
    subroutine test_random_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_random_surface(what)
        call check(error, what == "", "the generator was not usable through `use parquet_random` alone: " // what)
    end subroutine test_random_surface

    !> The test-drive wrapper over check_tables_surface.
    subroutine test_tables_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_tables_surface(what)
        call check(error, what == "", "the table was not usable through `use parquet_tables` alone: " // what)
    end subroutine test_tables_surface

    !> The test-drive wrapper over check_spatial_surface.
    subroutine test_spatial_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_spatial_surface(what)
        call check(error, what == "", "the spatial index was not usable through `use parquet_spatial` alone: " // what)
    end subroutine test_spatial_surface

    !> The test-drive wrapper over check_healpix_surface.
    subroutine test_healpix_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_healpix_surface(what)
        call check(error, what == "", &
            "the pixelisation was not usable through `use parquet_healpix` alone: " // what)
    end subroutine test_healpix_surface

    !> The test-drive wrapper over check_sphere_surface.
    subroutine test_sphere_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_sphere_surface(what)
        call check(error, what == "", &
            "points on the sphere were not usable through `use parquet_sphere` alone: " // what)
    end subroutine test_sphere_surface

    !> The test-drive wrapper over check_skycoord_surface.
    subroutine test_skycoord_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_skycoord_surface(what)
        call check(error, what == "", &
            "coordinate systems were not usable through `use parquet_skycoord` alone: " // what)
    end subroutine test_skycoord_surface

    !> The test-drive wrapper over check_utils_surface.
    subroutine test_utils_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_utils_surface(what)
        call check(error, what == "", "the helpers were not usable through `use parquet_utils` alone: " // what)
    end subroutine test_utils_surface

    !> The test-drive wrapper over check_integrate_surface.
    subroutine test_integrate_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_integrate_surface(what)
        call check(error, what == "", &
            "quadrature was not usable through `use parquet_integrate` alone: " // what)
    end subroutine test_integrate_surface

    !> The test-drive wrapper over check_interpolate_surface.
    subroutine test_interpolate_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_interpolate_surface(what)
        call check(error, what == "", &
            "interpolation was not usable through `use parquet_interpolate` alone: " // what)
    end subroutine test_interpolate_surface

    !> The test-drive wrapper over check_optimize_surface.
    subroutine test_optimize_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_optimize_surface(what)
        call check(error, what == "", &
            "minimisation was not usable through `use parquet_optimize` alone: " // what)
    end subroutine test_optimize_surface

    !> The test-drive wrapper over check_prima_surface.
    subroutine test_prima_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_prima_surface(what)
        call check(error, what == "", &
            "BOBYQA was not usable through `use parquet_prima` alone: " // what)
    end subroutine test_prima_surface

    !> The test-drive wrapper over check_cosmology_surface.
    subroutine test_cosmology_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_cosmology_surface(what)
        call check(error, what == "", &
            "a cosmology was not usable through `use parquet_cosmology` alone: " // what)
    end subroutine test_cosmology_surface

    !> The test-drive wrapper over check_root_surface.
    subroutine test_root_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_root_surface(what)
        call check(error, what == "", &
            "root finding was not usable through `use parquet_root` alone: " // what)
    end subroutine test_root_surface

    !> The test-drive wrapper over check_transform_surface.
    subroutine test_transform_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_transform_surface(what)
        call check(error, what == "", &
            "the transform was not usable through `use parquet_transform` alone: " // what)
    end subroutine test_transform_surface

    !> The test-drive wrapper over check_kde_surface.
    subroutine test_kde_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_kde_surface(what)
        call check(error, what == "", &
            "the estimator was not usable through `use parquet_kde` alone: " // what)
    end subroutine test_kde_surface

    !> The test-drive wrapper over check_logging_surface.
    subroutine test_logging_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_logging_surface(what)
        call check(error, what == "", "the logger was not usable through `use parquet_logging` alone: " // what)
    end subroutine test_logging_surface

    !> The test-drive wrapper over check_toml_surface.
    subroutine test_toml_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_toml_surface(what)
        call check(error, what == "", &
            "a configuration was not readable through `use parquet_toml` alone: " // what)
    end subroutine test_toml_surface

    !> The test-drive wrapper over check_version_surface.
    subroutine test_version_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_version_surface(what)
        call check(error, what == "", "the version was not reportable through `use parquet_version` alone: " // what)
    end subroutine test_version_surface

    !> The test-drive wrapper over check_io_surface.
    subroutine test_io_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_io_surface(what)
        call check(error, what == "", "a layer was not reachable through `use parquet_io` alone: " // what)
    end subroutine test_io_surface

    !> The test-drive wrapper over check_index_surface.
    subroutine test_index_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_index_surface(what)
        call check(error, what == "", &
            "something was not reachable through `use parquet_index` alone: " // what)
    end subroutine test_index_surface

    !> The test-drive wrapper over check_argsort_surface.
    subroutine test_argsort_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_argsort_surface(what)
        call check(error, what == "", "a knob was not round-trippable through `use parquet_argsort` alone: " // what)
    end subroutine test_argsort_surface

    !> The test-drive wrapper over check_sorting_surface.
    subroutine test_sorting_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_sorting_surface(what)
        call check(error, what == "", "a knob was not round-trippable through `use parquet_sorting` alone: " // what)
    end subroutine test_sorting_surface

    !> The test-drive wrapper over check_stats_surface.
    subroutine test_stats_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_stats_surface(what)
        call check(error, what == "", "a capability was not reachable through `use parquet_stats` alone: " // what)
    end subroutine test_stats_surface

    !> The test-drive wrapper over check_strings_surface.
    subroutine test_strings_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_strings_surface(what)
        call check(error, what == "", "a knob was not round-trippable through `use parquet_strings` alone: " // what)
    end subroutine test_strings_surface

    !> The test-drive wrapper over check_sampling_surface.
    subroutine test_sampling_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_sampling_surface(what)
        call check(error, what == "", "a knob was not round-trippable through `use parquet_sampling` alone: " // what)
    end subroutine test_sampling_surface

    !> The test-drive wrapper over check_settings_base_surface.
    subroutine test_settings_base_surface(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: ok
        character(len=:), allocatable :: what

        call check_settings_base_surface(ok, what)
        call check(error, ok, "a knob was not round-trippable through `use parquet_settings_base` alone: " // what)
    end subroutine test_settings_base_surface

end module test_module_surface
