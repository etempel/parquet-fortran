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
module test_module_surface_spatial
    use parquet_spatial                ! THE ONLY library import.
    use iso_fortran_env, only : int64, real64
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
        integer(int64) :: got(8), m
        integer :: n_spatial, i
        logical :: warn

        what = ""
        n_spatial = parquet_get_spatial_threads()
        warn = parquet_get_spatial_rebuild_warning()

        call parquet_set_spatial_threads(3)
        if (parquet_get_spatial_threads() /= 3) what = "spatial_threads"
        call parquet_set_spatial_rebuild_warning(.not. warn)
        if (what == "" .and. parquet_get_spatial_rebuild_warning() .eqv. warn) what = "spatial_rebuild_warning"
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
        if (what == "" .and. sx%metric() /= PF_METRIC_EUCLIDEAN) what = "PF_METRIC_EUCLIDEAN"

        call parquet_set_spatial_threads(n_spatial)
        call parquet_set_spatial_rebuild_warning(warn)
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
        ! The internal string is the "vX.Y.Z (date)" form of the same release, so the default
        ! form must appear inside it -- an assertion that both spellings answer, not merely that
        ! neither is empty. Under NAG the two are derived from one another, which is the case
        ! this deliberately still holds for.
        if (what == "" .and. index(internal, release) == 0) what = "the two forms disagree"
        if (what == "" .and. internal(1:1) /= "v") what = "mode='internal' is not v-prefixed"
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

contains

    !> Round-trips a two-column table through `use parquet_tables` alone.
    subroutine check_tables_surface(what)
        character(len=:), allocatable, intent(out) :: what !! the first thing that failed, or "".
        ! Its own filename: tests in a suite run concurrently and test_module_surface_io already
        ! writes test_run/module_surface_io.parquet.
        character(len=*), parameter :: out_file = "test_run/module_surface_tables.parquet"
        type(parquet_table) :: t, back
        integer(int64) :: ids(3)
        real(real64) :: mass(3)
        real(real64), allocatable :: got(:)

        what = ""
        ids = [1_int64, 2_int64, 3_int64]
        mass = [1.5_real64, 2.5_real64, 3.5_real64]

        call parquet_new_table(t)
        call t%add_column("id", ids)
        call t%add_column("mass", mass)
        if (t%nrows() /= 3_int64) what = "%nrows on a table built in memory"
        if (what == "" .and. t%ncols() /= 2) what = "%ncols on a table built in memory"

        call parquet_write_table(t, out_file, overwrite=.true.)

        call parquet_open_table(back, out_file)
        if (what == "" .and. back%nrows() /= 3_int64) what = "%nrows after reopening"
        call back%get("mass", got)
        if (what == "" .and. size(got) /= 3) what = "%get returned the wrong size"
        if (what == "" .and. got(2) /= 2.5_real64) what = "%get did not round-trip the value"
        if (what == "" .and. back%residency("mass") /= RES_FULL) what = "%residency after %get"
        ! REGIME_FULL/REGIME_SLICE were named here for the same build-breaking reason as every
        ! other reference in this file, and they are gone: row 30 made them private, because
        ! `regime` is a private component and no binding exposes it, so no caller could ever obtain
        ! a value to compare against either one. Do not restore the reference without first
        ! restoring the accessor that would make the constants usable.
    end subroutine check_tables_surface

end module test_module_surface_tables

module test_module_surface
    use test_module_surface_io, only : check_io_surface
    use test_module_surface_argsort, only : check_argsort_surface
    use test_module_surface_sorting, only : check_sorting_surface
    use test_module_surface_strings, only : check_strings_surface
    use test_module_surface_sampling, only : check_sampling_surface
    use test_module_surface_version, only : check_version_surface
    use test_module_surface_spatial, only : check_spatial_surface
    use test_module_surface_columns, only : check_columns_surface
    use test_module_surface_random, only : check_random_surface
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
            new_unittest("parquet_argsort alone exposes every sorting knob it reads", &
                test_argsort_surface), &
            new_unittest("parquet_sorting alone exposes every sorting knob it reads", &
                test_sorting_surface), &
            new_unittest("parquet_strings alone exposes every knob it reads", &
                test_strings_surface), &
            new_unittest("parquet_sampling alone exposes every knob it reads", &
                test_sampling_surface), &
            new_unittest("parquet_spatial alone builds an index and exposes every knob it reads", &
                test_spatial_surface), &
            new_unittest("parquet_version alone reports the library version", &
                test_version_surface), &
            new_unittest("parquet_io alone reaches every layer of the read/write API", &
                test_io_surface), &
            new_unittest("parquet_columns alone builds, nulls and reads a column", &
                test_columns_surface), &
            new_unittest("parquet_random alone draws, and exposes no setting", &
                test_random_surface), &
            new_unittest("parquet_tables alone round-trips a table through a file", &
                test_tables_surface) ]
    end subroutine collect_tests_module_surface

    !> The test-drive wrapper over check_columns_surface.
    subroutine test_columns_surface(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: what

        call check_columns_surface(what)
        call check(error, what == "", "the column was not usable through `use parquet_columns` alone: " // what)
    end subroutine test_columns_surface

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
