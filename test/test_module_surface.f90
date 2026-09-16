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

        ! Added after the three tiers: the free RA/Dec separation. Checked against a value the
        ! geometry fixes rather than against a range -- two points on the equator 90 degrees apart
        ! in right ascension are 90 degrees apart on the sphere -- so a procedure that compiled but
        ! answered nonsense would still be caught here.
        if (what == "" .and. abs(pf_angdist_deg(10.0_real64, 0.0_real64, 100.0_real64, 0.0_real64) &
                                 - 90.0_real64) > 1.0e-12_real64) what = "pf_angdist_deg"
    end subroutine check_healpix_surface

end module test_module_surface_healpix

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
        real(real64) :: u
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

        type(pf_integration_info)   :: info
        type(pf_integration_points) :: pts
        real(real64)                :: r, inf

        what = ""

        ! The generic, in its bare-rtol form, with the info and points records.
        r = pf_integrate(unit_square, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=info, points=pts)
        if (abs(r - 1.0_real64/3.0_real64) > 1.0e-12_real64) what = "pf_integrate"
        if (what == "" .and. .not. info%converged) what = "pf_integration_info%converged"
        if (what == "" .and. info%status /= PF_INT_OK) what = "PF_INT_OK"
        if (what == "" .and. pts%n /= 21) what = "pf_integration_points%n"
        if (what == "" .and. abs(sum(pts%w(1:pts%n)*pts%f(1:pts%n)) - r) > 1.0e-12_real64) &
            what = "pf_integration_points weights"

        ! The pf_tolerance form.
        if (what == "") then
            r = pf_integrate(unit_square, 0.0_real64, 1.0_real64, &
                             pf_tolerance(rtol=0.0_real64, atol=1.0e-10_real64))
            if (abs(r - 1.0_real64/3.0_real64) > 1.0e-10_real64) what = "pf_tolerance"
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

!> `parquet_interpolate` alone: the interpolant object and every binding it has, the one-shot generic
!! in both ranks, the test-only hook, and no settings knob at all.
!!
!! **One library import, and it must stay that way.** This module's row in the entry-module table
!! claims that `use parquet_interpolate` compiles three Fortran files and re-exports no setting --
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

        type(pf_interp_1d) :: line, spline
        real(real64)       :: x(4), y(4), v(2)
        logical            :: was_uniform

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

contains

    !> `(x - 2)**2`, counting the call.
    function surface_objective_eval(this, x) result(f)
        class(surface_objective), intent(inout) :: this !! the objective
        real(real64), intent(in)                :: x(:) !! the point
        real(real64)                            :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2
        this%calls = this%calls + 1

    end function surface_objective_eval

    !> Exercises one entry point from each family through `use parquet_optimize` alone.
    subroutine check_optimize_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".

        type(pf_optimize_info)    :: info
        type(pf_optimize_history) :: record
        type(pf_simplex_solver)   :: solver
        type(surface_objective)   :: obj
        real(real64)              :: xs, fs, x(1), fmin, lower(1), upper(1)
        real(real64), allocatable :: population(:,:)
        character(len=:), allocatable :: verbosity, stream

        what = ""

        ! Brent on a bracket, with both records.
        call pf_minimize_scalar(offset_square, -3.0_real64, 3.0_real64, xs, fs, info=info, &
                                history=record)
        if (abs(xs - 2.0_real64) > 1.0e-6_real64) what = "pf_minimize_scalar"
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
                                        solver=solver, xtol=1.0e-3_real64, threads=2, info=info)
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

        ! The remaining status codes are reachable by name through this import alone.
        if (what == "" .and. PF_OPT_LIMIT == PF_OPT_TARGET) what = "PF_OPT_LIMIT"
        if (what == "" .and. PF_OPT_NONFINITE == PF_OPT_ROUNDING) what = "PF_OPT_NONFINITE"
        if (what == "" .and. PF_OPT_INFEASIBLE == PF_OPT_OK) what = "PF_OPT_INFEASIBLE"

    end subroutine check_optimize_surface

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
    function prima_surface_objective_eval(this, x) result(f)
        class(prima_surface_objective), intent(inout) :: this !! the objective
        real(real64), intent(in)                      :: x(:) !! the point
        real(real64)                                  :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2
        this%calls = this%calls + 1

    end function prima_surface_objective_eval

    !> `(x - 2)**2` again, for the constrained type.
    function prima_surface_constrained_eval(this, x) result(f)
        class(prima_surface_constrained), intent(inout) :: this !! the objective
        real(real64), intent(in)                        :: x(:) !! the point
        real(real64)                                    :: f    !! the objective value

        f = (x(1) - 2.0_real64)**2
        this%calls = this%calls + 1

    end function prima_surface_constrained_eval

    !> How many constraint values `constraints` fills.
    function prima_surface_count(this) result(m)
        class(prima_surface_constrained), intent(in) :: this !! the objective
        integer                                      :: m    !! the number of constraints

        m = 1

    end function prima_surface_count

    !> `x - 1 <= 0`.
    subroutine prima_surface_constr(this, x, c)
        class(prima_surface_constrained), intent(inout) :: this !! the objective
        real(real64), intent(in)                        :: x(:) !! the point
        real(real64), intent(out)                       :: c(:) !! the constraint values

        this%calls = this%calls + 0
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

        what = ""
        a_ineq(1, 1) = 1.0_real64
        b_ineq = 1.0_real64

        ! BOBYQA with a plain function, with both records.
        x = [5.0_real64]
        lower = [-3.0_real64]
        upper = [8.0_real64]
        call pf_minimize_bobyqa(prima_offset_square, x, fmin, lower=lower, upper=upper, &
                                rhobeg=0.5_real64, rhoend=1.0e-8_real64, info=info, &
                                history=record)
        if (abs(x(1) - 2.0_real64) > 1.0e-6_real64) what = "pf_minimize_bobyqa"
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

        ! The remaining status codes are reachable by name through this import alone.
        if (what == "" .and. PF_OPT_LIMIT == PF_OPT_TARGET) what = "PF_OPT_LIMIT"
        if (what == "" .and. PF_OPT_NONFINITE == PF_OPT_ROUNDING) what = "PF_OPT_NONFINITE"
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

module test_module_surface
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
    use test_module_surface_logging, only : check_logging_surface
    use test_module_surface_toml, only : check_toml_surface
    use test_module_surface_spatial, only : check_spatial_surface
    use test_module_surface_healpix, only : check_healpix_surface
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
            new_unittest("parquet_utils alone folds text and takes a path apart", &
                         test_utils_surface), &
            new_unittest("parquet_integrate alone integrates and hands back its record", &
                         test_integrate_surface), &
            new_unittest("parquet_interpolate alone builds an interpolant and evaluates it", &
                         test_interpolate_surface), &
            new_unittest("parquet_optimize alone minimises through both engines and a solver", &
                         test_optimize_surface), &
            new_unittest("parquet_prima alone minimises with BOBYQA and hands back its record", &
                         test_prima_surface), &
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
                test_map_surface) ]
    end subroutine collect_tests_module_surface

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
