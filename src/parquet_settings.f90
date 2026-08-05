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
!> `parquet_set_max_threads` resizes a pool everyone already shares, so it takes effect immediately
!> for objects opened before the call as well as after.
!>
!> The read-only limits below (`parquet_max_*`) are the caps the library enforces on filter rules,
!> sort keys and MAML lines. They are published so that code building any of those from user or
!> configuration input can check a length before tripping an `error stop`, and they are constants
!> rather than settings because loosening them would convert a guard against runaway input into a
!> way to overflow the parser's own stack.
!>
!> User guide: `doc/pages/settings.md`.
module parquet_settings
    use iso_fortran_env, only: output_unit
    use iso_c_binding, only: c_int
    use parquet_bindings, only: parquet_set_thread_pool_capacity, parquet_get_thread_pool_capacity
    implicit none
    private
    !
    public :: parquet_set_max_threads
    public :: parquet_get_max_threads
    public :: parquet_reset_settings
    public :: parquet_print_settings
    !
    public :: parquet_max_filter_rule_len
    public :: parquet_max_filter_depth
    public :: parquet_max_filter_nodes
    public :: parquet_max_sort_keys
    public :: parquet_max_sort_key_len
    public :: parquet_max_maml_line_len
    !
    public :: parquet_set_sort_threads, parquet_get_sort_threads
    public :: parquet_set_prefetch_threads, parquet_get_prefetch_threads
    public :: parquet_set_default_compression, parquet_get_default_compression
    public :: parquet_set_default_compression_level, parquet_get_default_compression_level
    public :: parquet_set_default_use_threads, parquet_get_default_use_threads
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
    !> Arrow's kUseDefaultCompressionLevel sentinel (INT_MIN): "use the codec's own default level".
    integer, parameter :: level_codec_default = -huge(0) - 1
    !> This library's own level for its own default codec, applied only when a writer is opened with
    !! no compression arguments AND no compression setting -- see parquet_resolve_writer_compression.
    integer, parameter :: level_zstd_default = 3
    !
    ! ---- Mutable settings state ----
    !
    !> Arrow's CPU thread-pool capacity as it stood before the first `parquet_set_max_threads` call,
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
    integer, save :: cfg_sort_threads = 0
    !> Cap on the threads the table's internally-parallel %prefetch/%materialize_all may use. `0`
    !! means "auto" (as many as OpenMP offers). Read only in src/parquet_tables_read.f90, which owns
    !! the table's OpenMP plumbing.
    integer, save :: cfg_prefetch_threads = 0
    !> Default compression codec for parquet_open_writer. Empty means "never set", which is what
    !! distinguishes a factory default from a deliberate choice of "zstd" -- see
    !! parquet_resolve_writer_compression for why that distinction is load-bearing.
    character(len=16), save :: cfg_default_compression = ""
    !> Default compression level. `level_codec_default` means "never set".
    integer, save :: cfg_default_compression_level = level_codec_default
    !> Default for parquet_open_writer/parquet_open_reader's `use_threads=`.
    logical, save :: cfg_default_use_threads = .true.
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
    subroutine parquet_set_max_threads(n)
        integer, intent(in) :: n !! new thread-pool capacity; must be >= 1.

        if (n < 1) error stop "parquet_set_max_threads: n must be >= 1"
        if (cfg_arrow_threads_initial < 1) cfg_arrow_threads_initial = parquet_get_max_threads()
        call parquet_set_thread_pool_capacity(int(n, kind=c_int))
    end subroutine parquet_set_max_threads

    !> Reports Arrow's current global CPU thread-pool capacity -- what parquet_set_max_threads last
    !> set it to, or Arrow's own hardware-derived default if it was never set. The counterpart to
    !> parquet_set_max_threads, and the answer to "how many threads will Arrow actually use here".
    integer function parquet_get_max_threads() result(n)

        n = int(parquet_get_thread_pool_capacity())
    end function parquet_get_max_threads

    !> Sets the default thread count for every sort that does not pass `threads=` explicitly --
    !> `pf_sort`/`pf_argsort` and friends, a read-time `parquet_open_reader(..., sort_by=)`, and
    !> `parquet_table%sort_by`, which all share one engine and must share one default.
    !>
    !> Read per sort call, so it takes effect immediately. `0` restores automatic behaviour (as many
    !> threads as OpenMP offers). **A setting caps the automatic answer; it never overrides the rule
    !> that an unqualified sort inside an OpenMP parallel region runs serially** -- eight threads
    !> each asking for eight more is slower than not threading at all, and a caller who set this said
    !> nothing about nesting. An explicit `threads=` is still honoured everywhere, including there.
    subroutine parquet_set_sort_threads(n)
        integer, intent(in) :: n !! thread cap, or 0 for automatic; must be >= 0.

        if (n < 0) error stop "parquet_set_sort_threads: n must be >= 0 (0 means automatic)"
        cfg_sort_threads = n
    end subroutine parquet_set_sort_threads

    !> Reports the sort thread cap, or 0 if sorting is left automatic. This is the raw setting, not
    !> the resolved count -- ask `pf_sort_threads()` for the number a sort would actually use here,
    !> which additionally accounts for the OpenMP environment and for being inside a parallel region.
    integer function parquet_get_sort_threads() result(n)

        n = cfg_sort_threads
    end function parquet_get_sort_threads

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
        character(len=:), allocatable :: folded
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
        if (.not. ok) error stop "parquet_set_default_compression: unknown compression codec '" // &
            folded // "' (expected one of: uncompressed, snappy, gzip, zstd, brotli, lz4)"
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

    !> Lowercases ASCII letters. A local copy rather than parquet_to_lower, because that one lives
    !> in parquet_core, which uses THIS module -- importing it back would be a circular dependency.
    subroutine fold_ascii_lower(text, out)
        character(len=*), intent(in) :: text !! input text.
        character(len=:), allocatable, intent(out) :: out !! text with every ASCII A-Z lowercased.
        integer :: k, ic

        out = text
        do k = 1, len(out)
            ic = iachar(out(k:k))
            if (ic >= iachar("A") .and. ic <= iachar("Z")) out(k:k) = achar(ic + 32)
        end do
    end subroutine fold_ascii_lower

    !> Restores every setting to the value it had before this program changed it.
    !>
    !> Every knob returns to its factory value. For the Arrow thread pool that means the capacity
    !> captured on the first parquet_set_max_threads call; if it was never called, that part is a
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
        cfg_default_compression = ""
        cfg_default_compression_level = level_codec_default
        cfg_default_use_threads = .true.
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
    subroutine parquet_print_settings(unit)
        integer, intent(in), optional :: unit !! output unit (default output_unit).
        integer :: u
        character(len=:), allocatable :: codec

        u = output_unit
        if (present(unit)) u = unit
        write (u, '(a)') "parquet-fortran settings"
        call print_one(u, "arrow_threads", parquet_get_max_threads())
        call print_one(u, "sort_threads", cfg_sort_threads)
        call print_one(u, "prefetch_threads", cfg_prefetch_threads)
        call parquet_get_default_compression(codec)
        call print_text(u, "default_compression", codec)
        call print_one(u, "default_compression_level", parquet_get_default_compression_level())
        call print_text(u, "default_use_threads", merge("true ", "false", cfg_default_use_threads))
        write (u, '(a)') "limits (read-only)"
        call print_one(u, "parquet_max_filter_rule_len", parquet_max_filter_rule_len)
        call print_one(u, "parquet_max_filter_depth", parquet_max_filter_depth)
        call print_one(u, "parquet_max_filter_nodes", parquet_max_filter_nodes)
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
    subroutine print_text(u, name, value)
        integer, intent(in) :: u !! output unit.
        character(len=*), intent(in) :: name !! setting name, as documented.
        character(len=*), intent(in) :: value !! its current value.
        character(len=30) :: padded

        padded = name
        write (u, '(a,a,1x,a)') "  ", padded, trim(value)
    end subroutine print_text

end module parquet_settings
