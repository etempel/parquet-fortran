!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Process-global settings for parquet-fortran: the parameters that apply to the whole library
!> rather than to one reader, writer or table, collected in a single place a program can read and
!> change.
!>
!> **What is a setting here, and what is not.** A setting may change how *fast*, how *large* or how
!> *loud* the library runs; it may never change what the library answers. A knob that would alter a
!> returned value, an ordering or a nullness is deliberately absent and will stay absent -- a
!> program-wide default for something like "where do nulls sort" would make the same call return
!> different results in different programs, with nothing at the call site to hint at it. Anything
!> that can be expressed as an argument to a specific call (a writer's `compression=`, a sort's
!> `threads=`) belongs there instead, and an explicit argument always wins over a setting.
!>
!> **Thread safety: set once, at startup.** Settings are written during program initialisation,
!> before other threads exist and before any reader/writer/table is opened. Reads are
!> unsynchronised and a concurrent write is a data race, which the library does not defend against
!> -- resizing a thread pool that other threads are using at that moment is a race regardless of
!> what this module does. In exchange, reading a setting costs nothing on any hot path.
!>
!> **Capture points differ per setting, and each one says which it is** in its own doc-comment.
!> `parquet_set_arrow_threads` resizes a pool everyone already shares, so it takes effect immediately
!> for objects opened before the call as well as after.
!>
!> The read-only limits below (`parquet_max_*`) are the caps the library enforces on filter rules,
!> sort keys and MAML lines. They are published so that code building any of those from user or
!> configuration input can check a length before tripping an `error stop`, and they are constants
!> rather than settings because loosening them would convert a guard against runaway input into a
!> way to overflow the parser's own stack.
!>
!> User guide: `doc/pages/operating/settings.md`.
module parquet_settings
    use iso_fortran_env, only: output_unit, error_unit, int32, int64
    use iso_c_binding, only: c_int, c_int64_t
    use parquet_bindings, only: parquet_set_thread_pool_capacity, parquet_get_thread_pool_capacity, &
        parquet_push_output_settings, parquet_push_performance_settings, &
        parquet_push_file_metadata_settings, &
        c_get_arrow_version, c_get_parquet_version
    ! Every knob an Arrow-free module has to reach -- state, getter AND setter -- lives in that
    ! leaf module rather than here, so that reaching it does not import parquet_bindings
    ! transitively. This module re-exports all of them, so `use parquet_settings` and `use parquet`
    ! present exactly the surface they always have. See parquet_settings_base's header for the rule
    ! that decides which module a new knob belongs in.
    use parquet_settings_base
    implicit none
    private
    !
    ! ---- Re-exported from parquet_settings_base, so this module's surface is unchanged ----
    public :: parquet_output_is_suppressed
    public :: parquet_emit_info, parquet_emit_advice, parquet_emit_warning
    public :: parquet_emit_error_context
    ! The one destination resolver. Re-exported for the same reason the channels above are: the
    ! solicited printers live in other modules and there is no package scope to share it through.
    public :: parquet_message_unit
    ! Re-exported for `parquet_tables_read`'s `prefetch_thread_count`, which is three tiers away
    ! from the leaf and can only reach it through here. Both facades hide it again; Fortran has no
    ! package scope, so this is the same shape the emit channels above already use.
    public :: parquet_clamp_to_affinity
    ! The affinity clamp's two test-only hooks. Public for the reason every Fortran-side debug
    ! hook is (CLAUDE.md): this leaf has no `bind(C)` surface to put a C++ one on.
    public :: parquet_debug_set_affinity_procs, parquet_debug_reset_affinity_warning
    public :: parquet_set_sort_threads, parquet_get_sort_threads
    public :: parquet_set_sort_radix_path, parquet_get_sort_radix_path
    public :: parquet_set_sort_counting_path, parquet_get_sort_counting_path
    public :: parquet_set_sort_counting_bucket_limit, parquet_get_sort_counting_bucket_limit
    public :: parquet_set_string_threads, parquet_get_string_threads
    public :: parquet_set_random_threads, parquet_get_random_threads
    public :: parquet_set_spatial_threads, parquet_get_spatial_threads
    public :: parquet_set_healpix_threads, parquet_get_healpix_threads
    public :: parquet_set_index_threads, parquet_get_index_threads
    public :: parquet_set_random_parallel_min_elements
    public :: parquet_get_random_parallel_min_elements
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream
    !
    ! ---- Owned by this module ----
    public :: parquet_push_settings_to_cpp
    public :: parquet_set_arrow_threads
    public :: parquet_set_file_date, parquet_get_file_date
    public :: parquet_get_arrow_threads
    public :: parquet_get_arrow_version
    public :: parquet_reset_settings
    public :: parquet_print_settings
    !
    public :: parquet_max_filter_rule_len
    public :: parquet_max_filter_depth
    public :: parquet_max_filter_nodes
    public :: parquet_max_filter_sets
    public :: parquet_max_sort_keys
    public :: parquet_max_sort_key_len
    public :: parquet_max_maml_line_len
    !
    public :: parquet_set_prefetch_threads, parquet_get_prefetch_threads
    public :: parquet_set_table_threads, parquet_get_table_threads
    public :: parquet_set_threads
    public :: parquet_set_default_compression, parquet_get_default_compression
    public :: parquet_set_default_compression_level, parquet_get_default_compression_level
    public :: parquet_set_default_use_threads, parquet_get_default_use_threads
    public :: parquet_set_target_row_group_bytes, parquet_get_target_row_group_bytes
    public :: parquet_set_statistics_prescreen, parquet_get_statistics_prescreen
    public :: parquet_settings_from_env
    !
    !> The three output channels, and the ONLY places `verbosity`/`message_stream` are read. Public
    !! here so every module that emits can reach them, `private ::` in the facade so no user sees
    !! them; see parquet_valid_compressions below for the same mechanism and the same reason.
    !
    !> Library-internal plumbing, kept out of the `use parquet` namespace by an explicit
    !! `private ::` in the facade (src/parquet.f90) -- the same mechanism that hides `c_int` and the
    !! version bindings there. They are public here only because Fortran has no package scope and
    !! the write path lives in another module.
    public :: parquet_valid_compressions, parquet_resolve_writer_compression
    !
    ! ---- Read-only limits (Tier B) ----
    !
    !> Maximum number of characters in one `parquet_filter%add` rule. Exists so that adversarial or
    !! accidentally-huge input fails as a clean `error stop` rather than as an unbounded allocation.
    integer, parameter :: parquet_max_filter_rule_len = 8192
    !> Maximum parenthesis/`not` nesting depth within one filter rule. Also bounds the C++
    !! evaluator's peak memory, which is (live operands) * nrows bytes, and keeps the recursive
    !! descent parser off its own stack limit.
    integer, parameter :: parquet_max_filter_depth = 32
    !> Maximum number of expression nodes across every `%add` call of one `parquet_filter`.
    integer, parameter :: parquet_max_filter_nodes = 1024
    !> Maximum number of value sets bound to one `parquet_filter` with `%bind`/`%add_in`, the
    !! payload behind an `in`/`not_in` clause. A sanity bound on the number of DISTINCT sets, not
    !! on any set's length, which is unbounded: one set of ten million identifiers is ordinary use,
    !! sixty-five sets in one filter is a program that has lost track of what it is building.
    integer, parameter :: parquet_max_filter_sets = 64
    !> Maximum number of keys across every `%add` call of one `parquet_sortkey`.
    integer, parameter :: parquet_max_sort_keys = 16
    !> Maximum number of characters in one `parquet_sortkey%add` key ("<column> [asc|desc]").
    integer, parameter :: parquet_max_sort_key_len = 320
    !> Maximum number of characters in one line of a MAML source file. A longer line is reported as
    !! an `error stop` naming the offending line number rather than being silently truncated.
    integer, parameter :: parquet_max_maml_line_len = 1024
    !
    !> The compression codecs parquet_open_writer accepts, and the single list both it and
    !! parquet_set_default_compression validate against -- two copies would let a codec be settable
    !! as a default but rejected as an argument, or the reverse.
    character(len=12), parameter :: parquet_valid_compressions(6) = [character(len=12) :: &
        "uncompressed", "snappy", "gzip", "zstd", "brotli", "lz4"]
    !
    !> The verbosity levels and `cfg_verbosity`/`cfg_string_threads` themselves live in
    !! `parquet_settings_base` (imported above), not here. That module is a leaf with no
    !! `parquet_bindings` import, which is what lets `parquet_strings` read those two values without
    !! dragging the Arrow/Parquet C++ stack into a program that only wanted compact string storage.
    !! Everything else about them is unchanged: this module owns their public setters and getters,
    !! writes them directly, and is still the single writer. See that module's own header.
    !
    !> The accepted tokens for the two enum knobs, as DATA rather than as a literal inside each
    !! error message.
    !!
    !! `parquet_settings_from_env` has to reject a bad token itself -- it cannot let the setter do it,
    !! because the setter's `error stop` cannot name the environment variable the value came from, and
    !! nothing can inspect a value's validity after the process has aborted. Two validators means two
    !! chances to disagree about what is accepted, so both read these arrays and both build their
    !! "expected one of: ..." text with `token_list`. `parquet_valid_compressions` is the same idea
    !! and already existed; these two are it applied to the knobs that had their vocabulary inline.
    ! `verbosity_tokens` and `stream_tokens` live in parquet_settings_base, beside the setters
    ! that validate against them; they are imported above so parquet_settings_from_env can use
    ! the same vocabulary its setter does.
    !
    !> Longest environment-variable value this module will read. A longer one aborts naming the
    !! variable rather than being silently truncated -- the same failure the MAML line-length cap
    !! exists to prevent (`.claude/rules/reader-writer.md`, "Schemas and MAML sources"), and just as invisible: a truncated
    !! codec name or verbosity token would simply look like a typo the user did not make.
    integer, parameter :: env_max_len = 4096
    !
    !> Where the library's own messages go. `message_stream` accepts exactly these two, because a
    !! Fortran unit number means nothing on the C++ side of the bind(C) boundary, where three of the
    !! library's warnings and one of its reports are printed -- see doc/pages/operating/settings.md.
    ! (`stream_stdout`/`stream_stderr` live in parquet_settings_base with the emit channels.)
    !
    !> Arrow's kUseDefaultCompressionLevel sentinel (INT_MIN): "use the codec's own default level".
    integer, parameter :: level_codec_default = -huge(0) - 1
    !> This library's own level for its own default codec, applied only when a writer is opened with
    !! no compression arguments AND no compression setting -- see parquet_resolve_writer_compression.
    integer, parameter :: level_zstd_default = 3
    !> Width of an ISO-8601 "YYYY-MM-DDTHH:MM:SS" file date, in characters.
    !!
    !! Not a free choice: `build_votable_xml` (src/parquet_wrapper.cpp) writes the same value into a
    !! `<PARAM arraysize="19" datatype="char" name="DATE">`, so a date of any other length would make
    !! the sidecar contradict its own declaration. That is why parquet_set_file_date insists on
    !! exactly this shape rather than accepting any text.
    integer, parameter :: file_date_len = 19
    !
    !> The built-in values of the three numeric C++-side knobs, i.e. what `0` resolves to and what a
    !! getter reports after a reset. Each MUST equal the corresponding global's initialiser in
    !! src/parquet_wrapper.cpp (`kSortCountingBucketLimit`, `kTargetRowGroupBytes`), which is what
    !! applies before this module has pushed anything.
    integer(int64), parameter :: target_row_group_bytes_builtin = 268435456_int64   !! 256 MiB.
    !
    ! ---- Mutable settings state ----
    !
    !> Arrow's CPU thread-pool capacity as it stood before the first `parquet_set_arrow_threads` call,
    !! so `parquet_reset_settings` can put it back. Arrow's own initial capacity is
    !! hardware-dependent, so this module cannot hold it as a constant and has to capture it. `-1`
    !! means "never set, nothing to restore", which is why resetting an untouched program is a
    !! no-op rather than a resize to some invented default. Captured on the first set only, which is
    !! race-free under this module's set-once-at-startup contract and needs no lazy-init guard.
    integer, save :: cfg_arrow_threads_initial = -1
    !
    !> Default thread count for every sort that does not name one. `0` means "auto", which is what
    !! pf_sort_threads (src/parquet_sorting_keys.f90) resolves against the OpenMP environment -- and
    !! that is the ONLY place this is read, deliberately, so a read-time `sort_by=` and a raw-array
    !! sort can never disagree about it (feature_risks.md Risk-40).
    !> Cap on the threads the table's internally-parallel %prefetch/%materialize_all may use. `0`
    !! means "auto" (as many as OpenMP offers). Read only in src/parquet_tables_read.f90, which owns
    !! the table's OpenMP plumbing.
    integer, save :: cfg_prefetch_threads = 0
    !> Cap on the threads a table's row-structural mutation (%sort_by, %filter_rows, %top_n and the
    !! calls that go through them) may use to rewrite its columns concurrently, and on the columns
    !! %print_stat's statistics pass scans at once. `0` means "auto" (as many as OpenMP offers,
    !! never more than the table has columns). Read only in src/parquet_tables_parallel.f90, which
    !! owns the table layer's OpenMP plumbing.
    integer, save :: cfg_table_threads = 0
    !> (`cfg_string_threads` lives in parquet_settings_base -- see the note at the verbosity levels
    !! above. It is written by parquet_set_string_threads below and read by parquet_string_threads.)
    !> Default compression codec for parquet_open_writer. Empty means "never set", which is what
    !! distinguishes a factory default from a deliberate choice of "zstd" -- see
    !! parquet_resolve_writer_compression for why that distinction is load-bearing.
    character(len=16), save :: cfg_default_compression = ""
    !> Default compression level. `level_codec_default` means "never set".
    integer, save :: cfg_default_compression_level = level_codec_default
    !> Default for parquet_open_writer/parquet_open_reader's `use_threads=`.
    logical, save :: cfg_default_use_threads = .true.
    !> (`cfg_verbosity` lives in parquet_settings_base too, for the same reason.)
    !> Which stream the library's own messages go to. Read only by the emit channels.
    !
    !> The five knobs below live on the C++ side, and every one of them stores `0` (numbers) for
    !! "use the built-in default". The sentinel is resolved HERE, in push_performance_settings, so
    !! parquet_wrapper.cpp receives a usable number and never has to know a default -- but its own
    !! globals still need initialisers for the window before anything is pushed, which is why the
    !! `*_builtin` parameters below must equal the initialisers of `g_sort_counting_bucket_limit`
    !! and `g_target_row_group_bytes` there (feature_risks.md Risk-42).
    !> Whether the sort's integer counting fast path may be taken at all.
    !> Whether the sort's single-key radix fast path may be taken at all.
    !!
    !! Deliberately NOT mirrored to C++ by `push_performance_settings`, unlike its counting-path
    !! neighbour: the radix path exists only in the Fortran engine, so there is nothing on the other
    !! side of the `bind(C)` boundary for a mirror to govern. Adding one would be a global that no
    !! code reads, which is the shape `feature_risks.md` Risk-42 warns about from the other end.
    !> Largest key value RANGE (not cardinality) the counting path will accept. `0` = built-in.
    !> Target size in bytes of one auto-sized row group. `0` = built-in.
    integer(int64), save :: cfg_target_row_group_bytes = 0
    !> Whether the reader screens row groups against their footer statistics before reading them.
    logical, save :: cfg_statistics_prescreen = .true.
    !> Fixed value for the `DATE` file-metadata key, or blank to read the clock (the default).
    !!
    !! Nineteen characters because that is the width the VOTable sidecar's own PARAM declares
    !! (`arraysize="19"`), so a value of any other length would make the sidecar disagree with
    !! itself. Blank is not a value the key can take -- it means "no override", and is why the
    !! push below sends an explicit flag rather than leaving C++ to read an empty string as policy.
    character(len=file_date_len), save :: cfg_file_date = ""
    !
    ! ---- Generic setters over both integer kinds ----
    !
    !> Sets the largest key value range the sort's counting fast path will accept. See
    !> parquet_set_sort_counting_bucket_limit_int64 for the full description.
    !> Sets the work floor, in elements per thread, below which a bulk permutation stays
    !> serial. See parquet_set_random_parallel_min_elements_int64 for the full description.
    !> Sets the byte size an auto-sized row group aims for. See
    !> parquet_set_target_row_group_bytes_int64 for the full description.
    interface parquet_set_target_row_group_bytes
        module procedure parquet_set_target_row_group_bytes_int32
        module procedure parquet_set_target_row_group_bytes_int64
    end interface parquet_set_target_row_group_bytes
    !
contains

    !> Resizes Arrow's global CPU thread pool -- the single pool shared by every
    !> parquet_reader/parquet_writer in this process that has use_threads enabled (the default).
    !> This is NOT a per-reader/per-writer setting: call it once, e.g. near the start of your
    !> program, before opening readers/writers on other threads -- calling it concurrently from
    !> multiple threads with different values is a race, since it resizes a pool everyone else is
    !> also using at that moment.
    !>
    !> Takes effect immediately, for readers/writers opened before the call as well as after.
    !> The first call records the previous capacity so parquet_reset_settings can restore it.
    subroutine parquet_set_arrow_threads(n)
        integer, intent(in) :: n !! new thread-pool capacity; must be >= 1.

        if (n < 1) error stop "parquet_set_arrow_threads: n must be >= 1"
        if (cfg_arrow_threads_initial < 1) cfg_arrow_threads_initial = parquet_get_arrow_threads()
        call parquet_set_thread_pool_capacity(int(n, kind=c_int))
    end subroutine parquet_set_arrow_threads

    !> Reports Arrow's current global CPU thread-pool capacity -- what parquet_set_arrow_threads last
    !> set it to, or Arrow's own hardware-derived default if it was never set. The counterpart to
    !> parquet_set_arrow_threads, and the answer to "how many threads will Arrow actually use here".
    integer function parquet_get_arrow_threads() result(n)

        n = int(parquet_get_thread_pool_capacity())
    end function parquet_get_arrow_threads

    !> Returns the version of the Arrow/Parquet C++ libraries this program is actually built
    !> against, formatted "major.minor.patch". mode absent or mode="arrow" reports the linked
    !> Arrow library's RUNTIME version -- what is loaded now, which need not be what the wrapper
    !> was compiled against; mode="parquet" reports the Parquet C++ library version the wrapper
    !> was COMPILED against. Any other mode value is an error.
    !>
    !> This is the C++ half of version reporting and so lives here, beside the other
    !> "what is Arrow doing" queries, rather than in the Arrow-free parquet_version module. For
    !> the version of parquet-fortran itself, call parquet_get_version (`use parquet_version`,
    !> or `use parquet`). Quoting both is what makes a bug report actionable.
    subroutine parquet_get_arrow_version(ver_string, mode)
        character(len=:), allocatable, intent(out) :: ver_string !! resulting version string.
        character(len=*), intent(in), optional :: mode
        !! "arrow" | "parquet"; absent = "arrow".
        integer(c_int) :: major, minor, patch
        character(len=32) :: buf
        character(len=:), allocatable :: preview
        character(len=:), allocatable :: which

        which = "arrow"
        if (present(mode)) which = mode
        select case (which)
        case ("arrow")
            call c_get_arrow_version(major, minor, patch)
        case ("parquet")
            call c_get_parquet_version(major, minor, patch)
        case default
            ! Capped preview, never the whole value -- `mode` is caller-supplied and unbounded, and
            ! ifx 2026.1.1's ERROR STOP runtime corrupts memory once the message reaches 8192 bytes.
            if (len(which) > 100) then
                preview = which(1:100) // "..."
            else
                preview = which
            end if
            error stop "parquet_get_arrow_version: invalid mode '" // preview // &
                "' (must be 'arrow' or 'parquet')"
        end select
        write(buf, '(i0,".",i0,".",i0)') major, minor, patch
        ver_string = trim(buf)
    end subroutine parquet_get_arrow_version



    !> Sets the cap on how many threads `parquet_table%prefetch`/`%materialize_all` may use to read
    !> several columns at once, each on its own reader.
    !>
    !> Read per prefetch, so it takes effect immediately. `0` restores automatic behaviour. The value
    !> is a **cap**: the table never uses more threads than OpenMP offers, so setting this higher
    !> than `OMP_NUM_THREADS` changes nothing. `1` makes the prefetch serial, which is also what
    !> happens automatically whenever the parallel path is not applicable.
    subroutine parquet_set_prefetch_threads(n)
        integer, intent(in) :: n !! thread cap, or 0 for automatic; must be >= 0.

        if (n < 0) error stop "parquet_set_prefetch_threads: n must be >= 0 (0 means automatic)"
        cfg_prefetch_threads = n
    end subroutine parquet_set_prefetch_threads

    !> Reports the prefetch thread cap, or 0 if left automatic.
    integer function parquet_get_prefetch_threads() result(n)

        n = cfg_prefetch_threads
    end function parquet_get_prefetch_threads





    ! parquet_get_random_threads and parquet_get_random_parallel_min_elements are defined in
    ! parquet_settings_base and re-exported by the `public ::` lines above, for the same reason
    ! parquet_get_string_threads is: parquet_random must stay linkable without the C++ stack.

    ! parquet_get_string_threads is defined in parquet_settings_base and re-exported by the
    ! `public ::` line above. It reports the raw setting (0 if left automatic), not the resolved
    ! count -- ask `parquet_string_threads()` for the number a bulk operation would actually use
    ! here, which additionally accounts for the OpenMP environment and for being inside a parallel
    ! region.

    !> Sets the cap on how many threads a `parquet_table`'s row-structural mutation may use to
    !> rewrite its columns concurrently -- `%sort_by`, `%filter_rows`, `%top_n`, and `%delete_rows`
    !> and `%truncate`, which go through the same loop -- and on how many columns `%print_stat`'s
    !> statistics pass scans at once, which divides its work the same way.
    !>
    !> Read per mutation, so it takes effect immediately. `0` restores automatic behaviour. The value
    !> is a **cap**: the mutation never uses more threads than OpenMP offers, and never more than the
    !> table has columns to rewrite, so setting this higher than `OMP_NUM_THREADS` changes nothing.
    !> `1` makes the mutation serial, which is also what happens automatically inside an existing
    !> parallel region, on a table with fewer than two rewritable columns, and on a table too small
    !> to be worth a thread team.
    !>
    !> **This is also the memory control.** Each thread rewriting a column holds a transient second
    !> copy of it, so `n` threads hold `n` copies rather than one. Since the count is bounded by the
    !> column count, the transient copies come to at most one extra copy of the table -- a whole-column
    !> rewrite (`%sort_by`) can therefore double the table's peak memory for the duration of the call.
    !> A program near its memory ceiling caps the threads here, which caps the copies with them.
    subroutine parquet_set_table_threads(n)
        integer, intent(in) :: n !! thread cap, or 0 for automatic; must be >= 0.

        if (n < 0) error stop "parquet_set_table_threads: n must be >= 0 (0 means automatic)"
        cfg_table_threads = n
    end subroutine parquet_set_table_threads

    !> Reports the table-mutation thread cap, or 0 if left automatic.
    integer function parquet_get_table_threads() result(n)

        n = cfg_table_threads
    end function parquet_get_table_threads

    !> Sets every thread count at once: Arrow's pool, and each per-area cap -- sorting, the table
    !> prefetch, the table mutation, one string column's bulk work, the bulk random draws and the
    !> spatial index's bulk queries.
    !>
    !> A convenience for the common case of "give this library N threads and no more", equivalent to
    !> calling `parquet_set_arrow_threads(n)` and each per-area setter in turn. It has no state of
    !> its own -- read them back individually, or with `parquet_print_settings`, and set any one of
    !> them afterwards to override just that one.
    !>
    !> **`n` must be at least 1; `0` is not accepted here even though the per-area caps take it.**
    !> `0` means "automatic" to a per-area cap, but Arrow's pool has no automatic
    !> value at all -- its starting capacity is hardware-derived and is not a number this library gets
    !> to invent. Rather than have one argument mean two different things, this takes a real thread
    !> count only; use the individual setters when you want automatic behaviour, or
    !> `parquet_reset_settings` to put everything back.
    !>
    !> **They do not all take effect at the same moment**, which is the one thing worth knowing
    !> before reaching for this. Arrow's pool is resized immediately and is shared, so readers and
    !> writers already open are affected too; the per-area caps are read per call, so they apply to
    !> work started afterwards. Setting them together does not make them simultaneous.
    !>
    !> **Adding another cap means adding it here and to the assertion in `test_set_threads`.** The
    !> name says nothing about how many "all" is, so a forgotten call is invisible -- which is how
    !> this doc-comment came to say "all four" while a fifth and a sixth cap existed. It NAMES them
    !> rather than counting them, so the next one needs one line here and nowhere else. The
    !> environment table on the settings guide page does carry a count, and that one has to move.
    subroutine parquet_set_threads(n)
        integer, intent(in) :: n !! thread count for every cap; must be >= 1.

        if (n < 1) error stop "parquet_set_threads: n must be >= 1 " // &
            "(0 means automatic to a per-area cap, but Arrow's pool " // &
            "has no automatic value; set them individually if that is what you want)"
        call parquet_set_arrow_threads(n)
        call parquet_set_sort_threads(n)
        call parquet_set_prefetch_threads(n)
        call parquet_set_table_threads(n)
        call parquet_set_string_threads(n)
        call parquet_set_random_threads(n)
        call parquet_set_spatial_threads(n)
        call parquet_set_healpix_threads(n)
        call parquet_set_index_threads(n)
    end subroutine parquet_set_threads

    !> Sets the compression codec `parquet_open_writer` uses when the caller passes no
    !> `compression=`. One of "uncompressed", "snappy", "gzip", "zstd", "brotli", "lz4"
    !> (case-insensitive); anything else aborts, using the same list the writer's own argument is
    !> checked against.
    !>
    !> Captured **at writer open**: a writer already open keeps the codec it was opened with.
    !>
    !> **Setting this changes what the default compression LEVEL means.** The library's own level 3
    !> is tuned for its own default codec, so it applies only when neither the codec argument nor
    !> this setting has been touched. Choose a codec here and the level falls back to that codec's
    !> own default unless `parquet_set_default_compression_level` says otherwise -- which is what
    !> stops a zstd-tuned level being attached to, say, snappy.
    subroutine parquet_set_default_compression(name)
        character(len=*), intent(in) :: name !! codec name, case-insensitive.
        character(len=:), allocatable :: folded, expected
        integer :: i
        logical :: ok

        call fold_ascii_lower(trim(name), folded)
        ok = .false.
        do i = 1, size(parquet_valid_compressions)
            if (folded == trim(parquet_valid_compressions(i))) then
                ok = .true.
                exit
            end if
        end do
        if (.not. ok) then
            call token_list(parquet_valid_compressions, expected)
            error stop "parquet_set_default_compression: unknown compression codec '" // &
                folded // "' (expected one of: " // expected // ")"
        end if
        cfg_default_compression = folded
    end subroutine parquet_set_default_compression

    !> Reports the codec a writer opened with no `compression=` would use -- the value set here, or
    !> "zstd" if it was never set.
    subroutine parquet_get_default_compression(name)
        character(len=:), allocatable, intent(out) :: name !! effective default codec.

        if (len_trim(cfg_default_compression) == 0) then
            name = "zstd"
        else
            name = trim(cfg_default_compression)
        end if
    end subroutine parquet_get_default_compression

    !> Pins the `DATE` file-metadata key to a fixed value, so that writing the same data twice
    !> produces BYTE-IDENTICAL files. Blank (the factory default) restores reading the clock.
    !>
    !> **What this is for.** Every file this library writes carries a creation timestamp, and that
    !> timestamp is the only thing that differs between two writes of the same data -- measured, by
    !> writing one file forty times: runs landing in the same wall-clock second are bit-identical,
    !> and runs a second apart differ in nine bytes, every one of them a seconds digit. Pinning it
    !> is therefore the whole of what byte-for-byte reproducibility needs, which is what makes this
    !> knob worth having for a regression suite, a build system or a `diff` between two runs.
    !>
    !> **It PINS the date, it does not remove it.** The key is still written, at its declared width,
    !> in all four places the timestamp appears -- the `DATE` key, the VOTable sidecar's own
    !> `DATE` PARAM, and both of those again inside the base64 `ARROW:schema` blob, which Arrow
    !> duplicates from the key-value metadata. Suppressing the key instead was considered and
    !> rejected: `DATE` is written unconditionally by design and SHADOWS a user's own
    !> `schema%add_metadata("DATE", ...)` on read, so removing it would silently promote the user's
    !> entry and change what an unrelated call answers.
    !>
    !> `text` must be blank, or exactly 19 characters in the form `YYYY-MM-DDTHH:MM:SS` with each
    !> field in range; anything else aborts. The width is not a preference -- see `file_date_len`.
    !>
    !> **Read when a WRITER IS OPENED, not when it is closed**, like every other C++-side knob:
    !> `parquet_open_writer` pushes the mirror (see `parquet_push_settings_to_cpp`). Set it before
    !> opening the writer, which is the contract doc/pages/operating/settings.md already states for
    !> settings generally.
    subroutine parquet_set_file_date(text)
        character(len=*), intent(in) :: text !! "YYYY-MM-DDTHH:MM:SS", or "" to read the clock.
        character(len=:), allocatable :: got, why

        got = trim(adjustl(text))
        if (len(got) == 0) then
            cfg_file_date = ""
            return
        end if
        call file_date_problem(got, why)
        if (len(why) > 0) then
            ! Capped preview, never the whole value: `text` is caller-supplied and unbounded, and
            ! ifx's ERROR STOP runtime corrupts the heap once the composed message reaches 8192
            ! bytes -- the same rule parquet_filter_add follows.
            if (len(got) > 100) got = got(1:100) // "..."
            error stop "parquet_set_file_date: '" // got // "' is not a valid file date -- " // &
                why // " (expected YYYY-MM-DDTHH:MM:SS, or an empty string to use the clock)"
        end if
        cfg_file_date = got
    end subroutine parquet_set_file_date

    !> Reports the pinned file date, or "" when the clock is being read (the factory default).
    !>
    !> A subroutine with an allocatable `intent(out)` rather than a function returning
    !> `character(len=:), allocatable`, which this project forbids outright -- gfortran's codegen
    !> for receiving such a result is not reliably thread-local (see CLAUDE.md). Same shape as
    !> parquet_get_verbosity and parquet_get_message_stream.
    subroutine parquet_get_file_date(text)
        character(len=:), allocatable, intent(out) :: text !! the pinned date, or "" for the clock.

        text = trim(cfg_file_date)
    end subroutine parquet_get_file_date

    !> Describes what is wrong with `got` as a file date, or returns "" when nothing is.
    !>
    !> Returns a REASON rather than a logical so the abort can say which part is wrong: "not a
    !> valid date" sends a caller looking at the whole string, where "month 13 is out of range"
    !> does not. Checks the shape first and the field ranges second, since a range message about a
    !> string that is not even the right shape would be nonsense.
    subroutine file_date_problem(got, why)
        character(len=*), intent(in) :: got  !! the candidate, already trimmed and non-empty.
        character(len=:), allocatable, intent(out) :: why !! the problem, or "" when there is none.
        integer, parameter :: digit_at(14) = [1, 2, 3, 4, 6, 7, 9, 10, 12, 13, 15, 16, 18, 19]
        character(len=32) :: buf
        integer :: k, month, day, hour, minute, second

        why = ""
        if (len(got) /= file_date_len) then
            write (buf, '(i0)') len(got)
            why = "it is " // trim(buf) // " characters long, not 19"
            return
        end if
        do k = 1, size(digit_at)
            if (got(digit_at(k):digit_at(k)) < "0" .or. got(digit_at(k):digit_at(k)) > "9") then
                write (buf, '(i0)') digit_at(k)
                why = "character " // trim(buf) // " is not a digit"
                return
            end if
        end do
        if (got(5:5) /= "-" .or. got(8:8) /= "-") then
            why = "the date separators must be '-' at characters 5 and 8"
            return
        end if
        if (got(11:11) /= "T") then
            why = "character 11 must be 'T'"
            return
        end if
        if (got(14:14) /= ":" .or. got(17:17) /= ":") then
            why = "the time separators must be ':' at characters 14 and 17"
            return
        end if
        ! Field ranges, so a shape-correct nonsense like month 99 is caught here rather than
        ! becoming a provenance field nothing can read. The day is checked against 31 only: this is
        ! a provenance stamp, not a calendar, and rejecting 2025-02-30 would mean carrying a
        ! leap-year rule in a settings module for no reader that cares.
        read (got(6:7), '(i2)') month
        read (got(9:10), '(i2)') day
        read (got(12:13), '(i2)') hour
        read (got(15:16), '(i2)') minute
        read (got(18:19), '(i2)') second
        if (month < 1 .or. month > 12) then
            why = "the month is out of the range 01-12"
        else if (day < 1 .or. day > 31) then
            why = "the day is out of the range 01-31"
        else if (hour > 23) then
            why = "the hour is out of the range 00-23"
        else if (minute > 59) then
            why = "the minute is out of the range 00-59"
        else if (second > 59) then
            why = "the second is out of the range 00-59"
        end if
    end subroutine file_date_problem

    !> Sets the compression level `parquet_open_writer` uses when the caller passes no
    !> `compression_level=`. Captured **at writer open**.
    !>
    !> The valid range is the codec's business, not this library's (zstd, gzip and brotli each accept
    !> a different one, and snappy and lz4 have no levels at all), so nothing is rejected here -- an
    !> out-of-range level is reported by the codec when a writer is actually opened with it.
    subroutine parquet_set_default_compression_level(n)
        integer, intent(in) :: n !! compression level, interpreted by the chosen codec.

        cfg_default_compression_level = n
    end subroutine parquet_set_default_compression_level

    !> Reports the compression level applied to a writer opened with **no compression arguments at
    !> all** -- the value set here, or 3 (this library's level for its own default zstd) if it was
    !> never set. Naming a codec explicitly, by argument or by
    !> `parquet_set_default_compression`, does not attach this level to it; see that procedure.
    integer function parquet_get_default_compression_level() result(n)

        n = cfg_default_compression_level
        if (n == level_codec_default) n = level_zstd_default
    end function parquet_get_default_compression_level

    !> Sets the default for `parquet_open_writer`/`parquet_open_reader`'s `use_threads=`, i.e.
    !> whether Arrow's own internal thread pool is used for a reader's or writer's column work.
    !> Captured **at open**; an explicit `use_threads=` still wins.
    subroutine parquet_set_default_use_threads(flag)
        logical, intent(in) :: flag !! .true. to use Arrow's thread pool (the factory default).

        cfg_default_use_threads = flag
    end subroutine parquet_set_default_use_threads

    !> Reports the default for `use_threads=` on a newly opened reader or writer.
    logical function parquet_get_default_use_threads() result(flag)

        flag = cfg_default_use_threads
    end function parquet_get_default_use_threads

    !> Resolves a writer's codec and compression level from the caller's optional arguments and the
    !> process-global defaults. **Library-internal plumbing** -- the facade keeps it out of the
    !> `use parquet` namespace.
    !>
    !> The whole reason this is one procedure rather than four lines in `parquet_open_writer` is the
    !> level rule, which is easy to state and easy to get wrong: **level 3 is this library's choice
    !> for its own default codec, so it applies only when the codec was defaulted all the way** --
    !> no `compression=` argument AND no `parquet_set_default_compression`. Name a codec by either
    !> route and the level falls back to that codec's own default, unless a level was named too.
    !> Collapsing that condition would silently attach a zstd-tuned level to whatever codec was
    !> chosen; `test_compression_default_level_is_conditional` (test/test_writing.f90) is the
    !> assertion that fails if it is.
    subroutine parquet_resolve_writer_compression(compression, compression_level, codec, level)
        character(len=*), intent(in), optional :: compression !! the caller's compression= argument.
        integer, intent(in), optional :: compression_level !! the caller's compression_level= argument.
        character(len=:), allocatable, intent(out) :: codec !! resolved codec name, lowercased.
        integer, intent(out) :: level !! resolved level, or the codec-default sentinel.
        logical :: codec_defaulted

        codec_defaulted = .not. present(compression) .and. len_trim(cfg_default_compression) == 0
        if (present(compression)) then
            call fold_ascii_lower(trim(compression), codec)
        else
            call parquet_get_default_compression(codec)
        end if

        level = level_codec_default
        if (codec_defaulted) level = level_zstd_default
        if (cfg_default_compression_level /= level_codec_default) level = cfg_default_compression_level
        if (present(compression_level)) level = compression_level
    end subroutine parquet_resolve_writer_compression












    !> Sets the size in BYTES an auto-sized row group aims for. Pass `0` to restore the built-in
    !> 268435456 (256 MiB).
    !>
    !> Applies only when a writer is opened without an explicit `chunk_size=`; a caller-chosen row
    !> count is never overridden. Sizing by bytes rather than by a flat row count is what makes a
    !> table of narrow int32 columns and a table of wide vector columns produce row groups of
    !> comparable size, which is what Parquet's own row-group guidance (roughly 128 MB to 1 GB) is
    !> about and what drives per-row-group compression efficiency and decode cost.
    !>
    !> Three bounds the library applies afterwards are NOT settable: a floor of 1000 rows, a ceiling
    !> of 10,000,000 rows, and the int32 element-count ceiling a vector column imposes. A target so
    !> small that even the floor would overshoot it fourfold abandons the floor rather than the
    !> target, down to a single row per row group.
    !>
    !> Available in both integer kinds; a byte target can exceed int32.
    subroutine parquet_set_target_row_group_bytes_int64(n)
        integer(int64), intent(in) :: n !! byte target, or 0 for the built-in default; must be >= 0.

        if (n < 0) error stop "parquet_set_target_row_group_bytes: n must be >= 0 " // &
            "(0 restores the built-in default)"
        cfg_target_row_group_bytes = n
    end subroutine parquet_set_target_row_group_bytes_int64

    !> int32 form of parquet_set_target_row_group_bytes_int64 -- see it for what the value means.
    subroutine parquet_set_target_row_group_bytes_int32(n)
        integer(int32), intent(in) :: n !! byte target, or 0 for the built-in default; must be >= 0.

        call parquet_set_target_row_group_bytes_int64(int(n, kind=int64))
    end subroutine parquet_set_target_row_group_bytes_int32

    !> Reports the row-group byte target -- the EFFECTIVE value, so a program that never set it is
    !> told 268435456 rather than the `0` that is stored.
    integer(int64) function parquet_get_target_row_group_bytes() result(n)

        n = cfg_target_row_group_bytes
        if (n <= 0) n = target_row_group_bytes_builtin
    end function parquet_get_target_row_group_bytes

    !> Enables or disables the reader's row-group statistics screen.
    !>
    !> With a filter active, the reader reads each row group's footer statistics first and skips the
    !> row groups the filter provably cannot match -- no column data is read for those at all. It
    !> changes how much of the file is read and nothing else: the rows returned are identical either
    !> way, which is exactly what makes an A/B comparison the right test for it and what turning it
    !> off is for.
    !>
    !> Leaving it on is right for essentially every program. Disable it only to compare the two
    !> paths, or if a file's statistics are known to be untrustworthy.
    subroutine parquet_set_statistics_prescreen(enabled)
        logical, intent(in) :: enabled !! .true. (the default) lets the reader prune row groups.

        cfg_statistics_prescreen = enabled
    end subroutine parquet_set_statistics_prescreen

    !> Reports whether the reader's row-group statistics screen is enabled.
    logical function parquet_get_statistics_prescreen() result(enabled)

        enabled = cfg_statistics_prescreen
    end function parquet_get_statistics_prescreen

    ! parquet_output_is_suppressed is defined in parquet_settings_base and re-exported by the
    ! `public ::` line above -- what every solicited print procedure (`%print_stat`,
    ! `%print_schema_info`, `parquet_string_column`'s printers) asks before writing anything. It
    ! lives there rather than here so that parquet_strings can ask it without importing
    ! parquet_bindings; see that module's header.





    !> Mirrors both output settings to the C++ side, which prints three warnings and one report of
    !> its own and cannot see Fortran module variables.
    !>
    !> **Resolved integers cross the boundary, never tokens.** The fold and the validation happen
    !> once, here in Fortran; a second string parser in parquet_wrapper.cpp is exactly the drift this
    !> arrangement exists to avoid. And the C++ side gets no setter of its own, so this is the single
    !> writer and the mirror is derived rather than an independent copy that could diverge.
    !> Refreshes the C++ side's copy of every mirrored setting. Called where Fortran is about to
    !> hand control to C++ -- `parquet_open_reader` and `parquet_open_writer` -- and by
    !> `parquet_sorting_oracle` before it drives the C++ sort engine, which is the one mirrored-knob
    !> consumer that is not downstream of an open.
    !>
    !> **Push at POINT OF USE, not point of set.** No setter mirrors to C++ any more: that is what
    !> let each knob's setter follow its state into `parquet_settings_base`, where an Arrow-free
    !> module can re-export it. The cost is one grouped call per open instead of one per set, and
    !> the behaviour is identical under the contract `doc/pages/operating/settings.md` already
    !> states -- apply settings before opening anything. Outside that contract, a knob changed while
    !> a reader is open no longer reaches C++ mid-flight.
    !>
    !> **One entry point, both groups**, so a caller cannot refresh half the mirror -- Risk-42's
    !> "one push per group, never one per knob" carried forward rather than weakened. It is
    !> `private ::`'d in the `parquet` facade, so it never reaches a user's namespace.
    subroutine parquet_push_settings_to_cpp()

        call push_output_settings()
        call push_performance_settings()
        call push_file_metadata_settings()
    end subroutine parquet_push_settings_to_cpp

    subroutine push_output_settings()

        call parquet_push_output_settings(int(cfg_verbosity, kind=c_int), &
            int(cfg_message_stream, kind=c_int))
    end subroutine push_output_settings

    !> Mirrors the four performance knobs to the C++ side, which owns the sort engine, the row-group
    !> sizing and the statistics screen.
    !>
    !> **The `0`-means-built-in sentinel is resolved here**, via the getters, so parquet_wrapper.cpp
    !> receives numbers it can use directly and holds no `x > 0 ? x : default` conditional of its
    !> own. That is what keeps each default spelled in exactly one place per side, and the C++ side's
    !> initialisers are only ever what applies before the first push.
    !>
    !> One push rather than four, so parquet_reset_settings cannot restore some knobs and leave
    !> others stale on the far side of the boundary (feature_risks.md Risk-42).
    subroutine push_performance_settings()

        call parquet_push_performance_settings( &
            int(merge(1, 0, cfg_sort_counting_path), kind=c_int), &
            int(parquet_get_sort_counting_bucket_limit(), kind=c_int64_t), &
            int(parquet_get_target_row_group_bytes(), kind=c_int64_t), &
            int(merge(1, 0, cfg_statistics_prescreen), kind=c_int))
    end subroutine push_performance_settings

    !> Mirrors the pinned file date to the C++ side, which is where the `DATE` metadata key and the
    !> VOTable sidecar are built.
    !>
    !> **The "blank means read the clock" sentinel is resolved HERE**, into an explicit flag, so
    !> parquet_wrapper.cpp is told which of the two things to do rather than being left to read an
    !> empty string as policy. That is the same rule push_performance_settings follows for its own
    !> `0`-means-built-in values, and it keeps the C++ globals' initialisers -- the only thing that
    !> applies before the first push -- equal to this module's defaults (feature_risks.md Risk-42).
    !>
    !> Its own group rather than an arm of one of the two above: it is neither an output nor a
    !> performance knob, and one push per group is what stops parquet_reset_settings restoring
    !> some knobs and leaving others stale on the far side of the boundary.
    subroutine push_file_metadata_settings()

        call parquet_push_file_metadata_settings( &
            int(merge(1, 0, len_trim(cfg_file_date) > 0), kind=c_int), &
            trim(cfg_file_date)//char(0))
    end subroutine push_file_metadata_settings

    ! ==================================================================================
    ! Environment variables
    ! ==================================================================================

    !> Applies every `PARQUET_FORTRAN_*` environment variable that is set, through the knob's own
    !> setter.
    !>
    !> **Called by you, never automatically.** Reading the environment lazily on first access would
    !> be a data race the first time two threads touched a setting, so there is no hidden call: put
    !> this near the top of your program, before other threads exist and before any reader, writer or
    !> table is opened -- the same contract every setting in this module has.
    !>
    !> One variable per knob, named `PARQUET_FORTRAN_` plus the knob's own name upper-cased, exactly
    !> as `parquet_print_settings` prints it (`PARQUET_FORTRAN_SORT_THREADS`,
    !> `PARQUET_FORTRAN_VERBOSITY`, ...). The one name worth knowing in advance is
    !> `PARQUET_FORTRAN_ARROW_THREADS`, whose setter is called `parquet_set_arrow_threads` -- the
    !> variable follows the printed name, not the setter. See `doc/pages/operating/settings.md` for the full
    !> table.
    !>
    !> **It applies over what is already set; it does not reset.** A variable that is absent leaves
    !> its knob exactly as it was, so calling this after your own `parquet_set_*` calls lets the
    !> environment override them, and calling it before lets your code win.
    !>
    !> **A variable that is set but EMPTY is treated as unset**, not as an error -- so
    !> `PARQUET_FORTRAN_VERBOSITY=` does nothing at all. Worth knowing when a variable seems to be
    !> ignored: `export PARQUET_FORTRAN_VERBOSITY=$LEVEL` with `LEVEL` itself unset produces an empty
    !> value and is silently skipped. `parquet_print_settings` is what answers "did my environment
    !> actually apply".
    !>
    !> Any other bad value **aborts**, naming the variable, the value and what was expected, rather
    !> than being ignored -- a mistyped setting that silently does nothing is the failure this whole
    !> module is built to avoid.
    subroutine parquet_settings_from_env()
        character(len=:), allocatable :: text
        logical :: got, flag
        integer :: n32
        integer(int64) :: n64

        ! PARQUET_FORTRAN_THREADS first, deliberately: it sets all three thread counts, so the three
        ! specific variables below must be able to override it. This is the ONE place in this
        ! sequence where the order is load-bearing -- everywhere else it is presentation only, and a
        ! comment further down says so.
        call env_value("PARQUET_FORTRAN_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_THREADS", text, n32)
            call parquet_set_threads(n32)
        end if
        ! The rest in parquet_print_settings' order, so the sequence and the dump read side by side.
        ! The order is NOT load-bearing anywhere, including for the compression pair, which looks
        ! like it should be: parquet_resolve_writer_compression reads cfg_default_compression and
        ! cfg_default_compression_level together at writer-open time, so neither setter disturbs the
        ! other and storing them either way round gives the same result. Verified by mutation --
        ! swapping the two changes no test. Keep the order anyway, for readability, but do not add a
        ! comment claiming a dependency that is not there.
        call env_value("PARQUET_FORTRAN_ARROW_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_ARROW_THREADS", text, n32)
            call parquet_set_arrow_threads(n32)
        end if
        call env_value("PARQUET_FORTRAN_SORT_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_SORT_THREADS", text, n32)
            call parquet_set_sort_threads(n32)
        end if
        call env_value("PARQUET_FORTRAN_PREFETCH_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_PREFETCH_THREADS", text, n32)
            call parquet_set_prefetch_threads(n32)
        end if
        call env_value("PARQUET_FORTRAN_TABLE_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_TABLE_THREADS", text, n32)
            call parquet_set_table_threads(n32)
        end if
        call env_value("PARQUET_FORTRAN_STRING_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_STRING_THREADS", text, n32)
            call parquet_set_string_threads(n32)
        end if

        call env_value("PARQUET_FORTRAN_RANDOM_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_RANDOM_THREADS", text, n32)
            call parquet_set_random_threads(n32)
        end if

        call env_value("PARQUET_FORTRAN_SPATIAL_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_SPATIAL_THREADS", text, n32)
            call parquet_set_spatial_threads(n32)
        end if

        call env_value("PARQUET_FORTRAN_HEALPIX_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_HEALPIX_THREADS", text, n32)
            call parquet_set_healpix_threads(n32)
        end if

        call env_value("PARQUET_FORTRAN_INDEX_THREADS", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_INDEX_THREADS", text, n32)
            call parquet_set_index_threads(n32)
        end if

        call env_value("PARQUET_FORTRAN_RANDOM_PARALLEL_MIN_ELEMENTS", text, got)
        if (got) then
            call env_int64("PARQUET_FORTRAN_RANDOM_PARALLEL_MIN_ELEMENTS", text, n64)
            call parquet_set_random_parallel_min_elements(n64)
        end if
        call env_value("PARQUET_FORTRAN_SORT_COUNTING_PATH", text, got)
        if (got) then
            call env_logical("PARQUET_FORTRAN_SORT_COUNTING_PATH", text, flag)
            call parquet_set_sort_counting_path(flag)
        end if
        call env_value("PARQUET_FORTRAN_SORT_RADIX_PATH", text, got)
        if (got) then
            call env_logical("PARQUET_FORTRAN_SORT_RADIX_PATH", text, flag)
            call parquet_set_sort_radix_path(flag)
        end if
        call env_value("PARQUET_FORTRAN_SORT_COUNTING_BUCKET_LIMIT", text, got)
        if (got) then
            call env_int64("PARQUET_FORTRAN_SORT_COUNTING_BUCKET_LIMIT", text, n64)
            call parquet_set_sort_counting_bucket_limit(n64)
        end if
        call env_value("PARQUET_FORTRAN_DEFAULT_COMPRESSION", text, got)
        if (got) then
            call env_require_token("PARQUET_FORTRAN_DEFAULT_COMPRESSION", text, &
                parquet_valid_compressions, "compression codec")
            call parquet_set_default_compression(text)
        end if
        call env_value("PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL", text, got)
        if (got) then
            call env_int32("PARQUET_FORTRAN_DEFAULT_COMPRESSION_LEVEL", text, n32)
            call parquet_set_default_compression_level(n32)
        end if
        call env_value("PARQUET_FORTRAN_DEFAULT_USE_THREADS", text, got)
        if (got) then
            call env_logical("PARQUET_FORTRAN_DEFAULT_USE_THREADS", text, flag)
            call parquet_set_default_use_threads(flag)
        end if
        call env_value("PARQUET_FORTRAN_TARGET_ROW_GROUP_BYTES", text, got)
        if (got) then
            call env_int64("PARQUET_FORTRAN_TARGET_ROW_GROUP_BYTES", text, n64)
            call parquet_set_target_row_group_bytes(n64)
        end if
        call env_value("PARQUET_FORTRAN_STATISTICS_PRESCREEN", text, got)
        if (got) then
            call env_logical("PARQUET_FORTRAN_STATISTICS_PRESCREEN", text, flag)
            call parquet_set_statistics_prescreen(flag)
        end if
        call env_value("PARQUET_FORTRAN_VERBOSITY", text, got)
        if (got) then
            call env_require_token("PARQUET_FORTRAN_VERBOSITY", text, verbosity_tokens, "verbosity level")
            call parquet_set_verbosity(text)
        end if
        call env_value("PARQUET_FORTRAN_FILE_DATE", text, got)
        if (got) call parquet_set_file_date(text)
        call env_value("PARQUET_FORTRAN_MESSAGE_STREAM", text, got)
        if (got) then
            call env_require_token("PARQUET_FORTRAN_MESSAGE_STREAM", text, stream_tokens, "message stream")
            call parquet_set_message_stream(text)
        end if
    end subroutine parquet_settings_from_env

    !> Reads one environment variable, reporting whether there is a value to apply.
    !>
    !> `got` is `.false.` for an unset variable AND for one set to an empty or all-blank string --
    !> the deliberate choice recorded in feature_settings_s5.md, and the single place it lives. The
    !> two ARE distinguishable (`status` is 1 for unset and 0 for empty), so this is a decision
    !> rather than a limitation.
    !>
    !> A value longer than `env_max_len` aborts instead of arriving truncated: silently shortening a
    !> codec name or a token would produce an error that looks like a typo the user never made.
    subroutine env_value(name, text, got)
        character(len=*), intent(in) :: name !! the variable's full name, for the abort message.
        character(len=:), allocatable, intent(out) :: text !! its trimmed value; "" when `got` is false.
        logical, intent(out) :: got !! .true. when there is a non-blank value to apply.
        character(len=env_max_len) :: buf
        integer :: ln, st
        character(len=32) :: cap

        got = .false.
        text = ""
        call get_environment_variable(name, buf, length=ln, status=st)
        if (st /= 0 .and. ln == 0) return           ! not set at all
        if (ln > env_max_len) then
            write (cap, '(i0)') env_max_len
            error stop "parquet_settings_from_env: " // name // " is longer than " // trim(cap) // &
                " characters; refusing to apply a truncated value"
        end if
        if (ln == 0) return                          ! set but empty: treated exactly as unset
        text = trim(adjustl(buf(1:ln)))
        if (len(text) == 0) return                   ! all blanks: same as empty
        got = .true.
    end subroutine env_value

    !> Parses a strictly-formatted integer: optional sign, then digits, then nothing else.
    !>
    !> **A list-directed `read(text, *, iostat=)` is NOT strict enough for this** and must not be
    !> substituted back in. It rejects "5abc" and "3.9" as expected, but accepts **"5 6"** with
    !> `iostat == 0`, quietly yielding 5 -- so `PARQUET_FORTRAN_SORT_THREADS="4 8"` (a stray
    !> copy-paste, or a shell variable that expanded to two words) would set the cap to 4 and report
    !> success. Verified on gfortran 15.2. A wrong value applied silently is precisely the failure
    !> this module exists to prevent, so the digits are checked by hand and the `read` only runs once
    !> the shape is known to be sound.
    !>
    !> Range is NOT checked here -- the knob's own setter does that, so an out-of-range value
    !> produces the same message it would from a direct call.
    subroutine env_int64(name, text, value)
        character(len=*), intent(in) :: name !! the variable's full name, for the abort message.
        character(len=*), intent(in) :: text !! its value, already trimmed.
        integer(int64), intent(out) :: value !! the parsed number.
        integer :: k, first, ios
        logical :: ok

        first = 1
        if (len(text) >= 1) then
            if (text(1:1) == "+" .or. text(1:1) == "-") first = 2
        end if
        ok = len(text) >= first
        do k = first, len(text)
            if (text(k:k) < "0" .or. text(k:k) > "9") then
                ok = .false.
                exit
            end if
        end do
        ios = 0
        value = 0_int64
        if (ok) read (text, *, iostat=ios) value
        if (.not. ok .or. ios /= 0) then
            call env_reject(name, text, "is not an integer")
        end if
    end subroutine env_int64

    !> `env_int64` for a knob whose setter takes a default-kind integer, with the narrowing checked
    !> rather than assumed -- an out-of-int32 value would otherwise wrap into a plausible-looking
    !> small number and be applied.
    subroutine env_int32(name, text, value)
        character(len=*), intent(in) :: name !! the variable's full name, for the abort message.
        character(len=*), intent(in) :: text !! its value, already trimmed.
        integer, intent(out) :: value !! the parsed number.
        integer(int64) :: wide

        call env_int64(name, text, wide)
        if (wide > int(huge(0), int64) .or. wide < -int(huge(0), int64) - 1_int64) then
            call env_reject(name, text, "does not fit in a default INTEGER")
        end if
        value = int(wide)
    end subroutine env_int32

    !> Parses a boolean. `true`/`false`/`1`/`0`, case-insensitively, and nothing else.
    !>
    !> `true`/`false` is what parquet_print_settings prints, so a value copied out of a settings dump
    !> goes straight back in; `1`/`0` is what a shell naturally produces. `on`/`off` and `yes`/`no`
    !> are deliberately not accepted -- the message says so, which is more useful than silently
    !> guessing what "yes" meant.
    subroutine env_logical(name, text, value)
        character(len=*), intent(in) :: name !! the variable's full name, for the abort message.
        character(len=*), intent(in) :: text !! its value, already trimmed.
        logical, intent(out) :: value !! the parsed flag.
        character(len=:), allocatable :: tok

        call fold_ascii_lower(text, tok)
        select case (tok)
        case ("true", "1")
            value = .true.
        case ("false", "0")
            value = .false.
        case default
            value = .false.
            call env_reject(name, text, "is not a boolean (expected one of: true, false, 1, 0)")
        end select
    end subroutine env_logical

    !> Aborts unless `text` is one of `tokens`, case-insensitively.
    !>
    !> The value is then handed to its setter unchanged, which validates it again against this same
    !> array -- deliberately, and it is not a redundancy worth removing. The setter's `error stop`
    !> cannot name the environment variable a value came from, and after an abort nothing can go back
    !> and add it, so the only way to report `PARQUET_FORTRAN_VERBOSITY` rather than a bare
    !> `parquet_set_verbosity` is to check first. Sharing the array is what stops the two checks
    !> disagreeing about what is accepted.
    subroutine env_require_token(name, text, tokens, what)
        character(len=*), intent(in) :: name !! the variable's full name, for the abort message.
        character(len=*), intent(in) :: text !! its value, already trimmed.
        character(len=*), intent(in) :: tokens(:) !! the accepted vocabulary.
        character(len=*), intent(in) :: what !! what the value is, for the abort message.
        character(len=:), allocatable :: tok, expected
        integer :: i

        call fold_ascii_lower(text, tok)
        do i = 1, size(tokens)
            if (tok == trim(tokens(i))) return
        end do
        call token_list(tokens, expected)
        call env_reject(name, text, "is not a valid " // what // " (expected one of: " // expected // ")")
    end subroutine env_require_token

    !> The one place an environment variable's abort message is composed, so every one of them names
    !> the variable and shows the offending value the same way.
    !>
    !> **The value is capped to a short preview.** It is caller-controlled text of unbounded length,
    !> and ifx's ERROR STOP runtime corrupts the heap once the composed message reaches 8192 bytes --
    !> so a "value too long"-style guard that echoed the whole value would crash on exactly the input
    !> that triggers it (`.claude/rules/api-conventions.md`, "Errors and diagnostics").
    subroutine env_reject(name, text, why)
        character(len=*), intent(in) :: name !! the variable's full name.
        character(len=*), intent(in) :: text !! its value, shown truncated if long.
        character(len=*), intent(in) :: why !! what is wrong with it.
        integer, parameter :: max_preview = 100

        if (len(text) > max_preview) then
            error stop "parquet_settings_from_env: " // name // "='" // text(1:max_preview) // &
                "...' " // why
        end if
        error stop "parquet_settings_from_env: " // name // "='" // text // "' " // why
    end subroutine env_reject



    !> Restores every setting to the value it had before this program changed it.
    !>
    !> Every knob returns to its factory value. For the Arrow thread pool that means the capacity
    !> captured on the first parquet_set_arrow_threads call; if it was never called, that part is a
    !> no-op rather than a resize to an invented default, since Arrow's own initial capacity is
    !> hardware-dependent and is not a number this library gets to choose.
    !>
    !> **Every setting added to this module must be reset here.** A knob that is settable but not
    !> resettable leaks across the boundary this procedure exists to draw, and nothing else would
    !> notice -- which is why the test suite asserts the factory value of each one after a reset.
    subroutine parquet_reset_settings()

        if (cfg_arrow_threads_initial >= 1) then
            call parquet_set_thread_pool_capacity(int(cfg_arrow_threads_initial, kind=c_int))
            cfg_arrow_threads_initial = -1
        end if
        cfg_sort_threads = 0
        cfg_prefetch_threads = 0
        cfg_table_threads = 0
        cfg_string_threads = 0
        cfg_random_threads = 0
        cfg_spatial_threads = 0
        cfg_healpix_threads = 0
        cfg_index_threads = 0
        cfg_random_parallel_min_elements = 1000_int64
        cfg_default_compression = ""
        cfg_default_compression_level = level_codec_default
        cfg_default_use_threads = .true.
        cfg_verbosity = verb_normal
        cfg_message_stream = stream_stdout
        cfg_sort_counting_path = .true.
        cfg_sort_radix_path = .true.
        cfg_sort_counting_bucket_limit = 0
        cfg_target_row_group_bytes = 0
        cfg_statistics_prescreen = .true.
        cfg_file_date = ""
        call parquet_push_settings_to_cpp()
    end subroutine parquet_reset_settings

    !> Writes every setting's current value, and every read-only limit, to `unit`.
    !>
    !> The quickest way to see what this library will actually do without reading the guide: what
    !> the settable knobs are set to right now, and what the caps on filter rules, sort keys and
    !> MAML lines are. The two sections are separate because the second one is not settable -- a
    !> limit shown here is a fact about the library, not something a program can change.
    !>
    !> **Always prints, whatever else has been silenced.** A settings dump that could itself be
    !> suppressed would leave a quiet program with no way to be asked why it is quiet.
    !>
    !> **That exemption is from `verbosity` only.** Where the dump goes still follows
    !> `message_stream`, like everything else this library writes: a program that routed the
    !> library's text to standard error to keep a stdout pipe clean does not want this in that pipe
    !> either. Pass `unit=` to override.
    subroutine parquet_print_settings(unit)
        integer, intent(in), optional :: unit !! where to write; default the `message_stream` setting's unit.
        integer :: u
        character(len=:), allocatable :: codec, token

        u = parquet_message_unit()
        if (present(unit)) u = unit
        write (u, '(a)') "parquet-fortran settings"
        call print_one(u, "arrow_threads", parquet_get_arrow_threads())
        call print_one(u, "sort_threads", cfg_sort_threads)
        call print_one(u, "prefetch_threads", cfg_prefetch_threads)
        call print_one(u, "table_threads", cfg_table_threads)
        call print_one(u, "string_threads", cfg_string_threads)
        call print_one(u, "random_threads", cfg_random_threads)
        call print_one(u, "spatial_threads", cfg_spatial_threads)
        call print_one(u, "healpix_threads", cfg_healpix_threads)
        call print_one(u, "index_threads", cfg_index_threads)
        call print_big(u, "random_parallel_min_elements", cfg_random_parallel_min_elements)
        call print_text(u, "sort_counting_path", merge("true ", "false", cfg_sort_counting_path))
        call print_text(u, "sort_radix_path", merge("true ", "false", cfg_sort_radix_path))
        call print_big(u, "sort_counting_bucket_limit", parquet_get_sort_counting_bucket_limit())
        call parquet_get_default_compression(codec)
        call print_text(u, "default_compression", codec)
        call print_one(u, "default_compression_level", parquet_get_default_compression_level())
        call print_text(u, "default_use_threads", merge("true ", "false", cfg_default_use_threads))
        call print_big(u, "target_row_group_bytes", parquet_get_target_row_group_bytes())
        call print_text(u, "statistics_prescreen", merge("true ", "false", cfg_statistics_prescreen))
        call parquet_get_verbosity(token)
        call print_text(u, "verbosity", token)
        call parquet_get_message_stream(token)
        call print_text(u, "message_stream", token)
        ! Printed as `(clock)` rather than as an empty field, so a dump makes it obvious that the
        ! date is being read rather than that the row failed to print. The token is not one the
        ! setter accepts -- an empty string is -- which is why it is bracketed.
        call parquet_get_file_date(token)
        if (len(token) == 0) token = "(clock)"
        call print_text(u, "file_date", token)
        write (u, '(a)') "limits (read-only)"
        call print_one(u, "parquet_max_filter_rule_len", parquet_max_filter_rule_len)
        call print_one(u, "parquet_max_filter_depth", parquet_max_filter_depth)
        call print_one(u, "parquet_max_filter_nodes", parquet_max_filter_nodes)
        call print_one(u, "parquet_max_filter_sets", parquet_max_filter_sets)
        call print_one(u, "parquet_max_sort_keys", parquet_max_sort_keys)
        call print_one(u, "parquet_max_sort_key_len", parquet_max_sort_key_len)
        call print_one(u, "parquet_max_maml_line_len", parquet_max_maml_line_len)
    end subroutine parquet_print_settings

    !> Writes one "  <name padded to 30>  <value>" line. Factored out so every row of
    !> parquet_print_settings's output is laid out by one statement rather than by n copies of a
    !> format string that would drift apart as rows are added.
    subroutine print_one(u, name, value)
        integer, intent(in) :: u !! output unit.
        character(len=*), intent(in) :: name !! setting or limit name, as documented.
        integer, intent(in) :: value !! its current value.
        character(len=30) :: padded

        padded = name
        write (u, '(a,a,1x,i0)') "  ", padded, value
    end subroutine print_one

    !> print_one's counterpart for a value that is not an integer, laid out identically so the two
    !> kinds of row line up in one dump.
    !> One `name  value` row for a setting whose value is an int64 -- the three C++-side numbers,
    !> whose byte and range targets are not bounded below huge(int32) and so cannot use print_one.
    subroutine print_big(u, name, value)
        integer, intent(in) :: u !! output unit.
        character(len=*), intent(in) :: name !! setting name, as documented.
        integer(int64), intent(in) :: value !! its current value.
        character(len=30) :: padded

        padded = name
        write (u, '(a,a,1x,i0)') "  ", padded, value
    end subroutine print_big
    subroutine print_text(u, name, value)
        integer, intent(in) :: u !! output unit.
        character(len=*), intent(in) :: name !! setting name, as documented.
        character(len=*), intent(in) :: value !! its current value.
        character(len=30) :: padded

        padded = name
        write (u, '(a,a,1x,a)') "  ", padded, trim(value)
    end subroutine print_text

end module parquet_settings ! GCOVR_EXCL_LINE
