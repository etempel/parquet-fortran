!> General-purpose logging for programs built on this library: severity levels, several
!> destinations at once, a configurable line layout, and correct, readable logging from inside an
!> OpenMP parallel region.
!!
!! `parquet_logging` is a **leaf**: it imports `iso_fortran_env` and, under `#ifdef _OPENMP`,
!! `omp_lib`, and nothing else. `use parquet_logging` therefore compiles one Fortran file and
!! never crosses the C++ boundary. `check_parquet_logging_stays_arrow_free`
!! (tools/check_source_conventions.py) is what keeps that true.
!!
!! **This is NOT the library's own messaging system and does not replace it.** Every message
!! parquet-fortran itself prints goes through `parquet_emit_info`/`parquet_emit_warning`/
!! `parquet_emit_error_context` in `parquet_settings_base`, governed by the `verbosity` and
!! `message_stream` settings. No library code calls anything in this module. The two have
!! different audiences: those channels are for what *this library* says to *its* user, and this
!! module is for what *the user's program* says to *its* user.
!!
!! **Levels ascend with severity and use Python's numbers** (`PF_LEVEL_INFO = 20`), so a record is
!! emitted when `level >= threshold`. Note this is the opposite direction from the `qfeet`
!! `logging` module some callers are migrating from; every name differs too, so a mechanical
!! mix-up cannot compile.
!!
!! **`pf_logger` is a fixed-capacity value type with no allocatable components and no finalizer**,
!! and both of those are load-bearing rather than incidental. gfortran does not reliably
!! default-initialise a *finalizable* type given to an OpenMP `private()` clause, and ifx segfaults
!! on a type with *allocatable components* declared in a `block` lexically inside a parallel
!! region -- the two rules forbid opposite shapes, so a type with both can be used neither way.
!! Having neither makes a logger safe shared, safe `private()`, safe `firstprivate()` and safe
!! block-local, which is what makes the parallel patterns in the guide one-liners. The price is a
!! cap on a handful of sizes, every one of them a published `PF_LOG_MAX_*` parameter.
!!
!! **A bound is enforced at configuration time with a clean `error stop`, never by truncating a
!! record at emission time.** There is exactly one exception and it is deliberate: the two context
!! budgets saturate and warn once, because keeping the context *depth* exact matters more than the
!! last frame's text -- a dropped frame degrades a diagnostic, whereas a shifted stack would tag a
!! record with another frame's context.
!!
!! Guide: `doc/pages/utilities/logging.md`. Tests: `test/test_logging.f90` (suite name `logging`).
module parquet_logging
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64, output_unit, error_unit
    implicit none
    private

    ! ---- Severity levels (ascending with severity, Python's numbers) ----
    integer, parameter, public :: PF_LEVEL_ALL      = 0   !! Threshold admitting every record.
    integer, parameter, public :: PF_LEVEL_TRACE    = 5   !! Finer than DEBUG.
    integer, parameter, public :: PF_LEVEL_DEBUG    = 10  !! Detail useful while diagnosing.
    integer, parameter, public :: PF_LEVEL_INFO     = 20  !! Ordinary progress.
    integer, parameter, public :: PF_LEVEL_WARNING  = 30  !! Something unexpected that was survivable.
    integer, parameter, public :: PF_LEVEL_ERROR    = 40  !! An operation failed.
    integer, parameter, public :: PF_LEVEL_CRITICAL = 50  !! The program cannot continue meaningfully.
    integer, parameter, public :: PF_LEVEL_OFF      = 60  !! Threshold admitting no record at all.

    ! ---- Console streams ----
    integer, parameter, public :: PF_LOG_STDOUT = 1  !! `add_console` target: standard output.
    integer, parameter, public :: PF_LOG_STDERR = 2  !! `add_console` target: standard error.

    ! ---- Colour policy ----
    integer, parameter, public :: PF_LOG_COLOR_AUTO   = 0  !! Colour a console sink unless NO_COLOR or TERM say not to.
    integer, parameter, public :: PF_LOG_COLOR_NEVER  = 1  !! Never emit ANSI codes.
    integer, parameter, public :: PF_LOG_COLOR_ALWAYS = 2  !! Always emit ANSI codes, even to a file.

    ! ---- Threading mode ----
    integer, parameter, public :: PF_LOG_THREAD_DIRECT   = 0  !! Write each record as it happens.
    integer, parameter, public :: PF_LOG_THREAD_BUFFERED = 1  !! Collect per thread, emit at %flush.

    ! ---- Unset sentinel for a rank and for a sink's rank filter ----
    integer, parameter, public :: PF_LOG_RANK_ANY = -1  !! "No rank set" / "no rank filter".

    ! ---- Published capacities ----
    integer, parameter, public :: PF_LOG_MAX_SINKS          = 8      !! Sinks one logger may own.
    integer, parameter, public :: PF_LOG_MAX_PATH           = 512    !! `add_file` path length.
    integer, parameter, public :: PF_LOG_MAX_NAME           = 64     !! Logger name, and a name-override key.
    integer, parameter, public :: PF_LOG_MAX_NAME_RULES     = 16     !! Per-name level overrides per logger.
    integer, parameter, public :: PF_LOG_MAX_FORMAT         = 256    !! Layout template source length.
    integer, parameter, public :: PF_LOG_MAX_FORMAT_OPS     = 32     !! Parsed placeholders plus literals in one template.
    integer, parameter, public :: PF_LOG_MAX_CONTEXT        = 256    !! Rendered context bytes. SATURATES.
    integer, parameter, public :: PF_LOG_MAX_CONTEXT_DEPTH  = 8      !! Context frames whose text is kept. SATURATES.
    integer, parameter, public :: PF_LOG_MAX_LINE           = 4096   !! Rendered line before the allocatable fallback.
    integer, parameter, public :: PF_LOG_MAX_BUFFER_BYTES   = 65536  !! Per-thread collector slot, text. Default, settable.
    integer, parameter, public :: PF_LOG_MIN_BUFFER_BYTES   = 4096   !! Floor validated against set_thread_mode's slot_bytes.
    integer, parameter, public :: PF_LOG_MAX_BUFFER_RECORDS = 512    !! Per-thread collector slot, records.
    integer, parameter, public :: PF_LOG_MAX_DEDUP_KEYS     = 256    !! once=/every= table size, process-wide.
    integer, parameter, public :: PF_LOG_MAX_DEDUP_KEY      = 128    !! One dedup key: name, level and message text.
    integer, parameter, public :: PF_LOG_STR_LEN            = 32     !! Length of every `pf_str` result.

    ! ---- Built-in layout templates ----
    !> Compact layout: wall-clock time, level, message. The default for a console sink.
    character(len=*), parameter, public :: PF_LOG_FMT_BRIEF = "{time} [{level}] {message}"
    !> Full layout: date and time, level, logger name, thread, context, message. Every optional
    !> field uses the `{field|sep}` form, so a record missing one leaves no stray separator.
    character(len=*), parameter, public :: PF_LOG_FMT_FULL = &
        "{stamp} [{level}]{name| }{thread| t}{context| }: {message}"

    ! ---- ANSI colour codes, for pf_log_color ----
    character(len=*), parameter, public :: PF_LOG_C_RED     = "31"    !! ANSI code: red.
    character(len=*), parameter, public :: PF_LOG_C_GREEN   = "32"    !! ANSI code: green.
    character(len=*), parameter, public :: PF_LOG_C_YELLOW  = "33"    !! ANSI code: yellow.
    character(len=*), parameter, public :: PF_LOG_C_BLUE    = "34"    !! ANSI code: blue.
    character(len=*), parameter, public :: PF_LOG_C_MAGENTA = "35"    !! ANSI code: magenta.
    character(len=*), parameter, public :: PF_LOG_C_CYAN    = "36"    !! ANSI code: cyan.
    character(len=*), parameter, public :: PF_LOG_C_BRIGHT_RED = "91" !! ANSI code: bright red.
    character(len=*), parameter, public :: PF_LOG_C_DIM     = "2"     !! ANSI code: dim.

    public :: pf_str
    public :: pf_log_init, pf_log_add_console, pf_log_add_file, pf_log_add_unit
    public :: pf_log_set_level, pf_log_unset_level, pf_log_set_format, pf_log_set_color
    public :: pf_log_set_name
    public :: pf_log_set_rank, pf_log_set_thread_mode, pf_log_close
    public :: pf_log, pf_log_trace, pf_log_debug, pf_log_info, pf_log_warning
    public :: pf_log_error, pf_log_critical, pf_log_blank, pf_log_fatal
    public :: pf_log_enabled, pf_log_flush, pf_log_reset_dedup
    public :: pf_log_level_from_name, pf_log_level_name, pf_log_color, pf_log_elapsed
    public :: pf_log_set_context, pf_log_push_context, pf_log_pop_context
    public :: pf_log_clear_context, pf_log_context_depth
    public :: pf_log_configure_from_env

    ! ---- Private sink kinds ----
    integer, parameter :: SINK_CONSOLE = 1
    integer, parameter :: SINK_FILE    = 2
    integer, parameter :: SINK_UNIT    = 3

    ! ---- Private template field ids. 0 marks a literal run. ----
    integer, parameter :: FLD_LITERAL = 0
    integer, parameter :: FLD_DATE    = 1
    integer, parameter :: FLD_TIME    = 2
    integer, parameter :: FLD_STAMP   = 3
    integer, parameter :: FLD_ELAPSED = 4
    integer, parameter :: FLD_LEVEL   = 5
    integer, parameter :: FLD_NAME    = 6
    integer, parameter :: FLD_THREAD  = 7
    integer, parameter :: FLD_RANK    = 8
    integer, parameter :: FLD_CONTEXT = 9
    integer, parameter :: FLD_MESSAGE = 10

    character(len=1), parameter :: ESC = achar(27)
    character(len=*), parameter :: ANSI_RESET = ESC // "[0m"

    !> One parsed step of a layout template: either a literal run of the template's own text, or
    !> one field with an optional separator emitted only when the field renders non-empty. Both
    !> the literal and the separator are stored as index ranges into the sink's own `fmt`, so a
    !> parsed template costs no storage beyond the template itself.
    type :: fmt_op
        integer :: field = FLD_LITERAL  !! `FLD_*`; `FLD_LITERAL` means emit `fmt(lo:hi)` verbatim.
        integer :: lo = 1               !! First character of the literal run, in the sink's `fmt`.
        integer :: hi = 0               !! Last character of the literal run; `hi < lo` means empty.
        integer :: sep_lo = 1           !! First character of the `{field|sep}` separator.
        integer :: sep_hi = 0           !! Last character of the separator; `sep_hi < sep_lo` means none.
    end type fmt_op

    !> One destination: a console stream, a file this logger opened, or a unit the caller owns.
    !> Each carries its own threshold, layout, colour policy, flush policy and rank filter, and
    !> decides independently whether a given record reaches it.
    type :: pf_sink
        integer :: kind = 0                     !! `SINK_CONSOLE`, `SINK_FILE` or `SINK_UNIT`.
        integer :: unit = -1                    !! The unit records are written to.
        integer :: stream = 0                   !! For a console sink: `PF_LOG_STDOUT`/`PF_LOG_STDERR`.
        integer :: level = PF_LEVEL_ALL           !! This sink's own threshold.
        integer :: color = PF_LOG_COLOR_AUTO    !! Requested colour policy.
        logical :: use_color = .false.          !! Policy resolved once, at configuration time.
        logical :: do_flush = .true.            !! Whether to `flush` after every record.
        logical :: dead = .false.               !! A console sink dropped after a write failure.
        integer :: only_rank = PF_LOG_RANK_ANY  !! Emit only when the logger's rank matches.
        character(len=PF_LOG_MAX_PATH) :: path = ""    !! For a file sink: the path, for messages.
        character(len=PF_LOG_MAX_FORMAT) :: fmt = ""   !! Layout template source.
        integer :: nops = 0                            !! Parsed steps in `op`.
        type(fmt_op) :: op(PF_LOG_MAX_FORMAT_OPS)      !! The parsed template.
    end type pf_sink

    !> One per-name level override: a threshold applying to a logger name and to every name below
    !> it in dotted notation.
    type :: name_rule
        character(len=PF_LOG_MAX_NAME) :: name = ""  !! The name prefix this rule applies to.
        integer :: level = PF_LEVEL_ALL                !! The threshold for names matching it.
    end type name_rule

    !> Everything about one record except its message text, gathered once and then handed to each
    !> sink's renderer. The message stays a separate `character(len=*)` argument, since it is the
    !> one part with no bound.
    type :: rec_fields
        integer :: level = PF_LEVEL_INFO                       !! The record's severity.
        character(len=PF_LOG_MAX_NAME) :: name = ""          !! Logger name, or a per-call override.
        integer :: name_len = 0                              !! Used length of `name`.
        character(len=PF_LOG_MAX_CONTEXT) :: context = ""    !! Base context plus this thread's frames.
        integer :: context_len = 0                           !! Used length of `context`.
        integer :: thread = 0                                !! OpenMP thread number, 0 without OpenMP.
        integer :: rank = PF_LOG_RANK_ANY                    !! Caller-supplied rank, or the sentinel.
        character(len=10) :: date_s = ""                     !! `YYYY-MM-DD`.
        character(len=12) :: time_s = ""                     !! `HH:MM:SS.mmm`.
        character(len=16) :: elapsed_s = ""                  !! `H:MM:SS.mmm` since the clock origin.
        integer :: elapsed_len = 0                           !! Used length of `elapsed_s`.
    end type rec_fields

    !> A logging destination set: a threshold, a name, a rank, a threading mode and up to
    !> `PF_LOG_MAX_SINKS` sinks.
    !!
    !! **A freshly declared `pf_logger` owns no sinks and is silent**; `%enabled` answers `.false.`
    !! at every level, so a library that never configures one costs its caller a single integer
    !! comparison per logging call site. `%init` is the "give me a working logger" call.
    !!
    !! The type has no allocatable components and no finalizer, deliberately -- see this module's
    !! own header. It is therefore safe to share between threads, to name in an OpenMP `private()`
    !! or `firstprivate()` clause, and to declare inside a `block` in a parallel region. **Copying
    !! one is supported and is how `firstprivate` works**: every copy names the same units, so the
    !! copies write to the same files correctly. `%close` is idempotent across copies, but a write
    !! to a sink some copy has already closed aborts rather than going nowhere.
    !!
    !! **Emission is thread-safe; configuration is not.** Configure a logger before entering a
    !! parallel region.
    type, public :: pf_logger
        private
        integer :: level = PF_LEVEL_ALL                 !! Logger-wide threshold.
        integer :: min_level = PF_LEVEL_OFF             !! Cheapest threshold any sink accepts; cached.
        character(len=PF_LOG_MAX_NAME) :: name = ""   !! This logger's name, rendered as `{name}`.
        integer :: rank = PF_LOG_RANK_ANY             !! Caller-supplied rank, rendered as `{rank}`.
        integer :: thread_mode = PF_LOG_THREAD_DIRECT !! `PF_LOG_THREAD_DIRECT`/`_BUFFERED`.
        integer :: nsinks = 0                         !! Sinks currently attached.
        integer :: nrules = 0                         !! Per-name overrides currently set.
        type(name_rule) :: rule(PF_LOG_MAX_NAME_RULES) !! The per-name overrides.
        type(pf_sink) :: sink(PF_LOG_MAX_SINKS)        !! The sinks.
    contains
        ! ---- Configuration: single-threaded only ----
        procedure :: init => logger_init !! Clears every sink, then installs a stdout console unless console=.false.
        procedure :: add_console => logger_add_console !! Attaches a console sink on stdout or stderr.
        procedure :: add_file => logger_add_file !! Opens a file and attaches it as a sink.
        procedure :: add_unit => logger_add_unit !! Attaches a unit the caller opened and still owns.
        procedure :: set_level => logger_set_level !! Sets the logger, one sink's, or one name prefix's threshold.
        procedure :: unset_level => logger_unset_level !! Removes one per-name override, or every one.
        procedure :: set_format => logger_set_format !! Sets the layout template of one sink or of every current sink.
        procedure :: set_color => logger_set_color !! Sets the colour policy of one sink or of every current sink.
        procedure :: set_name => logger_set_name !! Sets the name rendered as `{name}` and keyed on by overrides.
        procedure :: set_rank => logger_set_rank !! Sets the rank rendered as `{rank}` and tested by a rank filter.
        procedure :: set_thread_mode => logger_set_thread_mode !! Selects direct or buffered emission.
        procedure :: close => logger_close !! Closes units this logger opened and clears every sink.
        ! ---- Emission ----
        procedure :: log => logger_log !! Emits one record at an explicit level.
        procedure :: trace => logger_trace !! Emits one record at `PF_LEVEL_TRACE`.
        procedure :: debug => logger_debug !! Emits one record at `PF_LEVEL_DEBUG`.
        procedure :: info => logger_info !! Emits one record at `PF_LEVEL_INFO`.
        procedure :: warning => logger_warning !! Emits one record at `PF_LEVEL_WARNING`.
        procedure :: error => logger_error !! Emits one record at `PF_LEVEL_ERROR`.
        procedure :: critical => logger_critical !! Emits one record at `PF_LEVEL_CRITICAL`.
        procedure :: blank => logger_blank !! Writes blank lines to every sink, with no layout.
        procedure :: fatal => logger_fatal !! Emits at CRITICAL, flushes every sink, then error stops.
        ! ---- Queries and control ----
        procedure :: enabled => logger_enabled !! Whether a record at this level would reach any sink.
        procedure :: flush => logger_flush !! Flushes every sink, and every collector slot.
    end type pf_logger

    !> Renders one scalar as text of exactly `PF_LOG_STR_LEN` characters, so that a message can be
    !> built by ordinary concatenation: `"rows " // trim(pf_str(n))`.
    !!
    !! **The result is a FIXED length, not `character(len=:), allocatable`, and that is
    !! deliberate**: this project forbids a function returning a deferred-length allocatable
    !! character (GCC PR113797, a thread-safety defect), and this helper exists to be called from
    !! inside parallel regions. The cost is a `trim` at each call site.
    !!
    !! Generic over `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)` and
    !! `logical`, each optionally with a caller-supplied `fmt` such as `'(F8.3)'`. A value that
    !! does not fit the width renders as asterisks, which is Fortran's own overflow marker.
    interface pf_str
        module procedure pf_str_i32, pf_str_i64, pf_str_r32, pf_str_r64, pf_str_log
    end interface pf_str

    ! ---- Process-global state ----

    !> The module's default logger, driving every `pf_log_*` procedure.
    !!
    !! It is declared with `implicit_console = .true.`, so it behaves as though it owned one stdout
    !! console sink at `PF_LEVEL_INFO` from the start and `use parquet_logging` followed by
    !! `call pf_log_info("hi")` prints. That is a component set in this declaration, not lazy
    !! initialisation -- there is no check-then-act on process-global state anywhere on the
    !! emission path. The first `pf_log_init` or `pf_log_add_*` clears the flag.
    type(pf_logger), save :: g_default

    !> Whether the default logger still behaves as though it owned a stdout console sink.
    !!
    !! Module-level rather than a component of `pf_logger`, so that the type needs no structure
    !! constructor and a freshly declared user logger cannot accidentally inherit the behaviour.
    !! It is written only by `pf_log_init`, `pf_log_add_*` and `pf_log_close`, all of which are
    !! configuration calls and single-threaded by contract, and it is only ever READ on the
    !! emission path -- so there is no check-then-act on process-global state anywhere.
    logical, save :: g_default_implicit = .true.

    !> The shared base context, rendered ahead of every thread's own context frames.
    !!
    !! It exists because an OpenMP `threadprivate` copy is undefined in every thread but the
    !! initial one at the start of a parallel region, and `copyin` is specified on the caller's own
    !! `!$omp parallel` line, which this module never sees. Without a shared base, a context set
    !! before a region would be visible to thread 0 and to no other thread -- worse than being
    !! visible to none, because it reads as a bug.
    character(len=PF_LOG_MAX_CONTEXT), save :: g_base_context = ""
    integer, save :: g_base_len = 0  !! Used length of `g_base_context`.

    ! ---- Per-thread context stack. Only the owning thread ever touches its own copy. ----
    character(len=PF_LOG_MAX_CONTEXT), save :: t_context = ""     !! This thread's rendered frames.
    integer, save :: t_context_len = 0                            !! Used length of `t_context`.
    integer, save :: t_context_depth = 0                          !! TRUE depth, which may exceed the kept frames.
    integer, save :: t_context_ends(PF_LOG_MAX_CONTEXT_DEPTH) = 0 !! End offset of each kept frame.
    !$omp threadprivate(t_context, t_context_len, t_context_depth, t_context_ends)

    ! ---- Process-wide once=/every= table. Every access is inside the output critical section. ----
    character(len=PF_LOG_MAX_DEDUP_KEY), save :: g_dedup_key(PF_LOG_MAX_DEDUP_KEYS) = ""
    integer(int64), save :: g_dedup_count(PF_LOG_MAX_DEDUP_KEYS) = 0_int64
    integer, save :: g_dedup_n = 0

    !> One slot of the buffered-mode collector: one thread's pending records.
    !!
    !! The store is SHARED and indexed by thread number, deliberately not `threadprivate`: a
    !! `threadprivate` buffer belongs to the thread that filled it and is unreachable from any
    !! other, so a `%flush()` issued after a parallel region would strand every worker's records
    !! silently. Only the owning thread ever appends to its own slot, so an append needs no lock.
    type :: collector_slot
        character(len=:), allocatable :: text          !! Rendered lines, packed end to end.
        integer :: used = 0                            !! Bytes of `text` in use.
        integer :: nrec = 0                            !! Records held.
        integer :: rec_unit(PF_LOG_MAX_BUFFER_RECORDS) = 0 !! Unit each record is destined for.
        integer :: rec_end(PF_LOG_MAX_BUFFER_RECORDS) = 0  !! End offset of each record in `text`.
    end type collector_slot

    type(collector_slot), allocatable, save :: g_slots(:)  !! One per thread; allocated by set_thread_mode.
    integer, save :: g_slot_bytes = 0                      !! Bytes per slot, as sized.

    ! ---- Monotonic clock origin, established at configuration time (never on the emission path) ----
    integer(int64), save :: g_clock0 = -1_int64
    logical, save :: g_clock_set = .false.

    ! ---- Once-per-process machinery warnings. See machinery_warning. ----
    logical, save :: g_warned_context = .false.
    logical, save :: g_warned_dedup = .false.
    logical, save :: g_warned_console = .false.

contains

    ! ================================================================================
    ! Internal helpers
    ! ================================================================================

    !> Reports one failure of the logging machinery itself, directly to `error_unit`, at most once
    !> per process per `flag`.
    !!
    !! **This must never route through the emission path.** The three callers -- a saturating
    !! context budget, a full dedup table, and a console sink dropped after a write failure -- can
    !! all be reached from inside the output critical section, and emitting an ordinary record
    !! there would re-enter emission with the lock already held. An OpenMP `critical` is not
    !! recursive, so that is a deadlock rather than a mess. It therefore bypasses sinks, layout,
    !! deduplication, the collector and the critical section alike.
    subroutine machinery_warning(text, flag)
        character(len=*), intent(in) :: text  !! The message, without a prefix.
        logical, intent(inout) :: flag        !! Set once this warning has been issued.

        if (flag) return
        flag = .true.
        write (error_unit, '(a)') "parquet_logging: " // text
    end subroutine machinery_warning

    !> Establishes the process-wide monotonic clock origin if it is not already set.
    !!
    !! Called only from configuration procedures and from `pf_log_elapsed`, never from the
    !! emission path -- there is deliberately no per-record check-then-act on a saved flag. Nothing
    !! is lost by that: rendering `{elapsed}` at all requires a `set_format` call, which
    !! establishes the origin on the way through.
    subroutine ensure_clock()
        integer(int64) :: c

        !$omp critical (pf_log_clock)
        if (.not. g_clock_set) then
            call system_clock(count = c)
            g_clock0 = c
            g_clock_set = .true.
        end if
        !$omp end critical (pf_log_clock)
    end subroutine ensure_clock

    !> Returns the OpenMP thread number, or 0 in a build without OpenMP.
    integer function this_thread() result(t)
#ifdef _OPENMP
        use omp_lib, only: omp_get_thread_num
#endif

        t = 0
#ifdef _OPENMP
        t = omp_get_thread_num()
#endif
    end function this_thread

    !> Renders `level` as its name, or as `Level <n>` when it is not one of the `PF_LOG_*`
    !> constants -- accepting an arbitrary integer level rather than rejecting it, which is what
    !> Python does and what makes arithmetic on a level safe.
    subroutine level_text(level, out, n)
        integer, intent(in) :: level               !! The level to render.
        character(len=*), intent(out) :: out       !! Receives the rendered name.
        integer, intent(out) :: n                  !! Used length of `out`.
        character(len=16) :: buf

        select case (level)
        case (PF_LEVEL_TRACE);    buf = "TRACE"
        case (PF_LEVEL_DEBUG);    buf = "DEBUG"
        case (PF_LEVEL_INFO);     buf = "INFO"
        case (PF_LEVEL_WARNING);  buf = "WARNING"
        case (PF_LEVEL_ERROR);    buf = "ERROR"
        case (PF_LEVEL_CRITICAL); buf = "CRITICAL"
        case (PF_LEVEL_ALL);      buf = "ALL"
        case (PF_LEVEL_OFF);      buf = "OFF"
        case default
            write (buf, '("Level ",i0)') level
        end select
        n = len_trim(buf)
        out = buf(1:n)
    end subroutine level_text

    !> The ANSI colour code a level's tag is rendered in, or an empty string for a level that is
    !> deliberately left uncoloured.
    subroutine level_color(level, code, n)
        integer, intent(in) :: level          !! The level whose tag is being coloured.
        character(len=*), intent(out) :: code !! Receives the ANSI code, e.g. `"31"`.
        integer, intent(out) :: n             !! Used length of `code`; 0 means no colour.

        select case (level)
        case (PF_LEVEL_CRITICAL); code = PF_LOG_C_BRIGHT_RED
        case (PF_LEVEL_ERROR);    code = PF_LOG_C_RED
        case (PF_LEVEL_WARNING);  code = PF_LOG_C_YELLOW
        case (PF_LEVEL_DEBUG);    code = PF_LOG_C_CYAN
        case (PF_LEVEL_TRACE);    code = PF_LOG_C_DIM
        case default;           code = ""
        end select
        n = len_trim(code)
    end subroutine level_color

    !> Resolves a sink's requested colour policy into the `use_color` flag emission reads.
    !!
    !! `PF_LOG_COLOR_AUTO` colours a console sink only, and only when `NO_COLOR` is unset (the
    !! no-color.org convention) and `TERM` is set to something other than `"dumb"`. Fortran has no
    !! standard `isatty`, so a redirected stdout still colours under `AUTO`; `NO_COLOR=1` is the
    !! documented answer and is the one users already know.
    logical function resolve_color(policy, kind) result(use_color)
        integer, intent(in) :: policy  !! `PF_LOG_COLOR_AUTO`/`_NEVER`/`_ALWAYS`.
        integer, intent(in) :: kind    !! The sink kind, since `AUTO` colours only a console.
        character(len=64) :: val
        integer :: ln, st

        select case (policy)
        case (PF_LOG_COLOR_NEVER)
            use_color = .false.
        case (PF_LOG_COLOR_ALWAYS)
            use_color = .true.
        case default
            use_color = (kind == SINK_CONSOLE)
            if (use_color) then
                call get_environment_variable("NO_COLOR", val, ln, st)
                if (st == 0) use_color = .false.
            end if
            if (use_color) then
                call get_environment_variable("TERM", val, ln, st)
                if (st /= 0) then
                    use_color = .false.
                else if (val(1:min(ln, len(val))) == "dumb") then
                    use_color = .false.
                end if
            end if
        end select
    end function resolve_color

    !> Parses one layout template into the sink's fixed plan, once, at configuration time.
    !!
    !! Recognises `{field}` and `{field|sep}`, where `sep` is a SEPARATOR PREPENDED to the field --
    !! not a default substituted for a missing one -- and is emitted only when the field renders
    !! non-empty -- the rule that stops an unnamed record leaving a stray separator mid-line.
    !! Everything outside the braces is a literal run. `error stop`s on an unknown field name, an
    !! unclosed brace, or a template needing more than `PF_LOG_MAX_FORMAT_OPS` steps.
    subroutine parse_template(sk, template)
        type(pf_sink), intent(inout) :: sk        !! The sink whose `fmt`/`op`/`nops` are set.
        character(len=*), intent(in) :: template  !! The template source.
        integer :: i, n, lo, close_at, bar, fld

        if (len(template) > PF_LOG_MAX_FORMAT) then
            error stop "pf_logger: layout template is longer than PF_LOG_MAX_FORMAT characters"
        end if
        sk%fmt = template
        sk%nops = 0
        n = len(template)
        i = 1
        do while (i <= n)
            if (template(i:i) == "{") then
                close_at = index(template(i + 1:n), "}")
                if (close_at == 0) then
                    error stop "pf_logger: layout template has an unclosed '{': " // trim(template)
                end if
                close_at = i + close_at
                bar = index(template(i + 1:close_at - 1), "|")
                if (bar == 0) then
                    lo = close_at - 1
                else
                    lo = i + bar - 1
                end if
                fld = field_id(template(i + 1:lo))
                if (fld == FLD_LITERAL) then
                    error stop "pf_logger: layout template names an unknown field '" // &
                        template(i + 1:lo) // "'"
                end if
                call push_op(sk, fld, 1, 0, merge(i + bar + 1, 1, bar > 0), &
                    merge(close_at - 1, 0, bar > 0))
                i = close_at + 1
            else
                lo = i
                do while (i <= n)
                    if (template(i:i) == "{") exit
                    i = i + 1
                end do
                call push_op(sk, FLD_LITERAL, lo, i - 1, 1, 0)
            end if
        end do
    end subroutine parse_template

    !> Appends one step to a sink's parsed template, or `error stop`s when the plan is full.
    subroutine push_op(sk, field, lo, hi, sep_lo, sep_hi)
        type(pf_sink), intent(inout) :: sk  !! The sink being built.
        integer, intent(in) :: field        !! `FLD_*`, or `FLD_LITERAL` for a literal run.
        integer, intent(in) :: lo           !! First character of the literal run.
        integer, intent(in) :: hi           !! Last character of the literal run.
        integer, intent(in) :: sep_lo       !! First character of the `{field|sep}` separator.
        integer, intent(in) :: sep_hi       !! Last character of the separator.

        if (sk%nops >= PF_LOG_MAX_FORMAT_OPS) then
            error stop "pf_logger: layout template needs more than PF_LOG_MAX_FORMAT_OPS steps"
        end if
        sk%nops = sk%nops + 1
        sk%op(sk%nops) = fmt_op(field = field, lo = lo, hi = hi, sep_lo = sep_lo, sep_hi = sep_hi)
    end subroutine push_op

    !> Maps a placeholder name to its `FLD_*` id, or `FLD_LITERAL` when the name is not one of the
    !> recognised fields.
    integer function field_id(word) result(fld)
        character(len=*), intent(in) :: word  !! The text between `{` and `}` or `|`.

        select case (word)
        case ("date");    fld = FLD_DATE
        case ("time");    fld = FLD_TIME
        case ("stamp");   fld = FLD_STAMP
        case ("elapsed"); fld = FLD_ELAPSED
        case ("level");   fld = FLD_LEVEL
        case ("name");    fld = FLD_NAME
        case ("thread");  fld = FLD_THREAD
        case ("rank");    fld = FLD_RANK
        case ("context"); fld = FLD_CONTEXT
        case ("message"); fld = FLD_MESSAGE
        case default;     fld = FLD_LITERAL
        end select
    end function field_id

    !> Appends `text(1:n)` to `out` at `pos` when `out` is present, and advances `pos` regardless.
    !! The single place the renderer writes, so that its measuring and writing passes cannot drift.
    subroutine put(out, pos, text)
        character(len=*), intent(out), optional :: out  !! Absent while measuring, present while writing.
        integer, intent(inout) :: pos                   !! Next free position; advanced by `len(text)`.
        character(len=*), intent(in) :: text            !! The text to place.

        if (len(text) > 0) then
            if (present(out)) out(pos:pos + len(text) - 1) = text
            pos = pos + len(text)
        end if
    end subroutine put

    !> Renders one record for one sink, either measuring it (`out` absent) or writing it
    !> (`out` present). One implementation for both passes, so the length this reports and the
    !> bytes it writes cannot disagree -- which is what makes the over-long fallback in
    !> `emit_to_sink` safe.
    subroutine render_line(sk, rec, msg, n, out)
        type(pf_sink), intent(in) :: sk                 !! The sink whose template is being applied.
        type(rec_fields), intent(in) :: rec             !! Everything about the record but its text.
        character(len=*), intent(in) :: msg             !! The message text.
        integer, intent(out) :: n                       !! Rendered length, in characters.
        character(len=*), intent(out), optional :: out  !! Receives the rendered line when present.
        integer :: k, pos, ln, cn
        character(len=16) :: lvl
        character(len=2) :: code
        character(len=12) :: num
        logical :: empty

        pos = 1
        do k = 1, sk%nops
            associate (o => sk%op(k))
                if (o%field == FLD_LITERAL) then
                    if (o%hi >= o%lo) call put(out, pos, sk%fmt(o%lo:o%hi))
                    cycle
                end if
                ! A field with a separator emits neither when the field itself renders empty.
                empty = .false.
                select case (o%field)
                case (FLD_NAME);    empty = rec%name_len == 0
                case (FLD_CONTEXT); empty = rec%context_len == 0
                case (FLD_RANK);    empty = rec%rank == PF_LOG_RANK_ANY
                case (FLD_ELAPSED); empty = rec%elapsed_len == 0
                end select
                if (empty) cycle
                if (o%sep_hi >= o%sep_lo) call put(out, pos, sk%fmt(o%sep_lo:o%sep_hi))
                select case (o%field)
                case (FLD_DATE)
                    call put(out, pos, rec%date_s)
                case (FLD_TIME)
                    call put(out, pos, rec%time_s)
                case (FLD_STAMP)
                    call put(out, pos, rec%date_s)
                    call put(out, pos, " ")
                    call put(out, pos, rec%time_s)
                case (FLD_ELAPSED)
                    call put(out, pos, rec%elapsed_s(1:rec%elapsed_len))
                case (FLD_LEVEL)
                    call level_text(rec%level, lvl, ln)
                    call level_color(rec%level, code, cn)
                    if (sk%use_color .and. cn > 0) then
                        call put(out, pos, ESC // "[" // code(1:cn) // "m")
                    end if
                    call put(out, pos, lvl(1:ln))
                    if (ln < 8) call put(out, pos, repeat(" ", 8 - ln))
                    if (sk%use_color .and. cn > 0) call put(out, pos, ANSI_RESET)
                case (FLD_NAME)
                    call put(out, pos, rec%name(1:rec%name_len))
                case (FLD_THREAD)
                    write (num, '(i0)') rec%thread
                    call put(out, pos, trim(num))
                case (FLD_RANK)
                    write (num, '(i0)') rec%rank
                    call put(out, pos, trim(num))
                case (FLD_CONTEXT)
                    call put(out, pos, rec%context(1:rec%context_len))
                case (FLD_MESSAGE)
                    call put(out, pos, msg)
                end select
            end associate
        end do
        n = pos - 1
    end subroutine render_line

    !> Resolves the two fields the per-name threshold decision needs: the level, and the name --
    !> the per-call `name=` when given, else the logger's own. Cheap by construction: a copy and a
    !> `len_trim`, no clock and no context.
    subroutine fill_record_name(self, level, name, rec)
        type(pf_logger), intent(in) :: self               !! The logger emitting the record.
        integer, intent(in) :: level                      !! The record's severity.
        character(len=*), intent(in), optional :: name    !! Per-call name, overriding the logger's.
        type(rec_fields), intent(out) :: rec              !! Receives the level and the name.
        integer :: n

        rec%level = level
        if (present(name)) then
            n = min(len_trim(name), PF_LOG_MAX_NAME)
            rec%name = name(1:n)
            rec%name_len = n
        else
            rec%name_len = len_trim(self%name)
            rec%name = self%name
        end if
    end subroutine fill_record_name

    !> Fills everything else: rank, thread, context and the clock fields.
    !!
    !! Split from `fill_record_name` above so that `emit_core` can apply the per-name threshold
    !! **before** paying for any of this. That ordering is what confines the cost of a per-name
    !! override that LOWERS a threshold to the records the override actually admits: without it, a
    !! single lowering rule would make every record above the new floor pay two clock reads and a
    !! context assembly before being dropped for having the wrong name.
    subroutine fill_record_rest(self, context, rec)
        type(pf_logger), intent(in) :: self                     !! The logger emitting the record.
        character(len=*), intent(in), optional :: context       !! Per-call context, overriding the ambient one.
        type(rec_fields), intent(inout) :: rec                  !! Receives the remaining fields.
        integer :: v(8), n
        integer(int64) :: c, rate
        real(real64) :: secs
        integer :: hh, mm
        character(len=16) :: buf

        rec%rank = self%rank
        rec%thread = this_thread()

        if (present(context)) then
            n = min(len_trim(context), PF_LOG_MAX_CONTEXT)
            rec%context = context(1:n)
            rec%context_len = n
        else
            call current_context(rec%context, rec%context_len)
        end if

        call date_and_time(values = v)
        write (rec%date_s, '(i4.4,"-",i2.2,"-",i2.2)') v(1), v(2), v(3)
        write (rec%time_s, '(i2.2,":",i2.2,":",i2.2,".",i3.3)') v(5), v(6), v(7), v(8)

        rec%elapsed_len = 0
        if (g_clock_set) then
            call system_clock(count = c, count_rate = rate)
            if (rate > 0_int64) then
                secs = real(c - g_clock0, real64) / real(rate, real64)
                if (secs < 0.0_real64) secs = 0.0_real64
                hh = int(secs / 3600.0_real64)
                mm = int((secs - real(hh, real64) * 3600.0_real64) / 60.0_real64)
                secs = secs - real(hh, real64) * 3600.0_real64 - real(mm, real64) * 60.0_real64
                write (buf, '(i0,":",i2.2,":",f6.3)') hh, mm, secs
                do n = 1, len_trim(buf)
                    if (buf(n:n) == " ") buf(n:n) = "0"
                end do
                rec%elapsed_len = len_trim(buf)
                rec%elapsed_s = buf
            end if
        end if
    end subroutine fill_record_rest

    !> Builds the context this thread's records carry: the shared base, then this thread's own
    !> frames. Truncation cannot occur here -- both parts were bounded when they were set.
    subroutine current_context(out, n)
        character(len=*), intent(out) :: out  !! Receives the rendered context.
        integer, intent(out) :: n             !! Used length of `out`.

        n = 0
        out = ""
        if (g_base_len > 0) then
            out(1:g_base_len) = g_base_context(1:g_base_len)
            n = g_base_len
        end if
        if (t_context_len > 0) then
            if (n > 0) then
                if (n + 1 + t_context_len <= len(out)) then
                    out(n + 1:n + 1) = " "
                    out(n + 2:n + 1 + t_context_len) = t_context(1:t_context_len)
                    n = n + 1 + t_context_len
                end if
            else
                out(1:t_context_len) = t_context(1:t_context_len)
                n = t_context_len
            end if
        end if
    end subroutine current_context

    !> Decides whether a `once=`/`every=` record is emitted this time, and records the occurrence.
    !!
    !! **The lookup and the insert are one decision and the caller holds the output critical
    !! section around it.** Splitting them lets two threads both emit a once-only record. The key
    !! is the logger name, the level and the trimmed message text -- not the context, since
    !! deduplicating across contexts is normally the entire point. When the table is full both
    !! forms degrade to "always emit", which is the safe direction, and say so once.
    logical function dedup_admits(rec, msg, once, every) result(emit)
        type(rec_fields), intent(in) :: rec   !! The record's gathered fields, for name and level.
        character(len=*), intent(in) :: msg   !! The message text.
        logical, intent(in) :: once           !! Whether `once=.true.` was requested.
        integer, intent(in) :: every          !! `every=n`, or 0 when not requested.
        character(len=PF_LOG_MAX_DEDUP_KEY) :: key
        character(len=12) :: lv
        integer :: i, slot, klen

        write (lv, '(i0)') rec%level
        key = rec%name(1:rec%name_len) // "|" // trim(lv) // "|" // msg
        klen = len(rec%name(1:rec%name_len)) + 1 + len_trim(lv) + 1 + len(msg)
        if (klen > PF_LOG_MAX_DEDUP_KEY) klen = PF_LOG_MAX_DEDUP_KEY

        slot = 0
        do i = 1, g_dedup_n
            if (g_dedup_key(i) == key) then
                slot = i
                exit
            end if
        end do
        if (slot == 0) then
            if (g_dedup_n >= PF_LOG_MAX_DEDUP_KEYS) then
                call machinery_warning("the once=/every= table is full (PF_LOG_MAX_DEDUP_KEYS); " // &
                    "further once=/every= records are emitted every time", g_warned_dedup)
                emit = .true.
                return
            end if
            g_dedup_n = g_dedup_n + 1
            slot = g_dedup_n
            g_dedup_key(slot) = key
            g_dedup_count(slot) = 0_int64
        end if
        g_dedup_count(slot) = g_dedup_count(slot) + 1_int64
        if (once) then
            emit = g_dedup_count(slot) == 1_int64
        else if (every > 1) then
            emit = mod(g_dedup_count(slot) - 1_int64, int(every, int64)) == 0_int64
        else
            emit = .true.
        end if
    end function dedup_admits

    !> Writes one rendered line to one unit, applying the I/O failure policy.
    !!
    !! **A file or caller-supplied unit aborts on failure; a console warns once and reports
    !! `failed`, so the caller can drop that sink and continue.** The asymmetry is deliberate and
    !! the console case is completely ordinary: `./prog | head` closes standard output, and a
    !! program must not die of a broken pipe inside a logging call. A full disk that silently
    !! swallowed a log file is the opposite -- it leaves the run unexplainable.
    subroutine deliver(unit, is_console, path, text, do_flush, failed)
        integer, intent(in) :: unit           !! The unit to write to.
        logical, intent(in) :: is_console     !! Whether this destination is a console stream.
        character(len=*), intent(in) :: path  !! The path, for a file sink's error message.
        character(len=*), intent(in) :: text  !! The rendered line.
        logical, intent(in) :: do_flush       !! Whether to flush after writing.
        logical, intent(out) :: failed        !! Set when a console write failed and the sink should be dropped.
        integer :: ios
        character(len=256) :: iom

        failed = .false.
        write (unit, '(a)', iostat = ios, iomsg = iom) text
        if (ios /= 0) then
            if (is_console) then
                failed = .true.
                return
            end if
            error stop "pf_logger: writing a record failed for '" // trim(path) // "': " // trim(iom)
        end if
        if (do_flush) flush (unit, iostat = ios)
    end subroutine deliver

    !> Appends one rendered line to this thread's collector slot, flushing the slot first if it
    !> does not fit. Reports `.false.` when the line cannot be buffered even by itself, which is
    !> the caller's signal to write it directly -- a record is never split and never dropped.
    logical function collector_append(unit, text) result(ok)
        integer, intent(in) :: unit           !! The unit this record is destined for.
        character(len=*), intent(in) :: text  !! The rendered line.
        integer :: t

        ok = .false.
        if (.not. allocated(g_slots)) return
        t = this_thread() + 1
        ! A thread outside the sized range -- a team grown after configuration, or a nested
        ! region -- falls back to direct emission rather than to a drop.
        if (t < 1 .or. t > size(g_slots)) return
        associate (s => g_slots(t))
            if (s%used + len(text) + 1 > g_slot_bytes .or. s%nrec >= PF_LOG_MAX_BUFFER_RECORDS) then
                call flush_slot(t)
            end if
            if (s%used + len(text) + 1 > g_slot_bytes .or. s%nrec >= PF_LOG_MAX_BUFFER_RECORDS) return
            s%text(s%used + 1:s%used + len(text)) = text
            s%used = s%used + len(text)
            s%nrec = s%nrec + 1
            s%rec_unit(s%nrec) = unit
            s%rec_end(s%nrec) = s%used
        end associate
        ok = .true.
    end function collector_append

    !> Emits every record held in one collector slot, in order, and empties it.
    subroutine flush_slot(t)
        integer, intent(in) :: t  !! Slot index, i.e. thread number plus one.
        integer :: i, lo
        logical :: failed

        if (.not. allocated(g_slots)) return
        if (t < 1 .or. t > size(g_slots)) return
        associate (s => g_slots(t))
            do i = 1, s%nrec
                if (i == 1) then
                    lo = 1
                else
                    lo = s%rec_end(i - 1) + 1
                end if
                !$omp critical (pf_log_output)
                call deliver(s%rec_unit(i), is_console_unit(s%rec_unit(i)), "", &
                    s%text(lo:s%rec_end(i)), .false., failed)
                !$omp end critical (pf_log_output)
            end do
            s%nrec = 0
            s%used = 0
        end associate
    end subroutine flush_slot

    !> Whether a unit is one of the two console streams, which decides the write-failure policy
    !> for a record replayed out of the collector, where the originating sink is no longer known.
    logical function is_console_unit(unit) result(yes)
        integer, intent(in) :: unit  !! The unit to classify.

        yes = (unit == output_unit) .or. (unit == error_unit)
    end function is_console_unit

    !> The sink the module's default logger behaves as though it owned before it is configured:
    !> stdout, `PF_LEVEL_INFO`, `PF_LOG_FMT_BRIEF`.
    !!
    !! Built per record rather than stored, so that no lazy initialisation of process-global state
    !! is needed anywhere. Parsing a 26-character template is a handful of `index` calls and is
    !! far below the cost of the `date_and_time` and `write` that follow it; and it is reached
    !! only until the first `pf_log_init`/`pf_log_add_*` call.
    subroutine default_console_sink(sk)
        type(pf_sink), intent(out) :: sk  !! Receives the implicit console sink.

        sk%kind = SINK_CONSOLE
        sk%stream = PF_LOG_STDOUT
        sk%unit = output_unit
        sk%level = PF_LEVEL_INFO
        sk%color = PF_LOG_COLOR_AUTO
        sk%use_color = resolve_color(PF_LOG_COLOR_AUTO, SINK_CONSOLE)
        sk%do_flush = .true.
        call parse_template(sk, PF_LOG_FMT_BRIEF)
    end subroutine default_console_sink

    !> Renders one record for one sink and delivers it, directly or into the collector.
    !!
    !! A line longer than `PF_LOG_MAX_LINE` is rendered into an allocatable local and emitted in
    !! full -- never truncated. That is also the one case the collector cannot hold, so it is
    !! written directly, in order, after the slot has been flushed.
    subroutine emit_to_sink(sk, rec, msg, thread_mode, died)
        type(pf_sink), intent(in) :: sk       !! The sink being written to.
        type(rec_fields), intent(in) :: rec   !! The record's gathered fields.
        character(len=*), intent(in) :: msg   !! The message text.
        integer, intent(in) :: thread_mode    !! `PF_LOG_THREAD_DIRECT` or `PF_LOG_THREAD_BUFFERED`.
        logical, intent(out) :: died          !! Set when a console write failed and the sink must be dropped.
        character(len=PF_LOG_MAX_LINE) :: line
        character(len=:), allocatable :: big
        integer :: n, m

        died = .false.
        if (sk%dead) return
        if (rec%level < sk%level) return
        if (sk%only_rank /= PF_LOG_RANK_ANY) then
            if (rec%rank /= sk%only_rank) return
        end if

        call render_line(sk, rec, msg, n)
        if (n <= PF_LOG_MAX_LINE) then
            call render_line(sk, rec, msg, m, line)
            if (thread_mode == PF_LOG_THREAD_BUFFERED) then
                if (collector_append(sk%unit, line(1:n))) return
            end if
            !$omp critical (pf_log_output)
            call deliver(sk%unit, sk%kind == SINK_CONSOLE, sk%path, line(1:n), sk%do_flush, died)
            !$omp end critical (pf_log_output)
        else
            allocate (character(len=n) :: big)
            call render_line(sk, rec, msg, m, big)
            if (thread_mode == PF_LOG_THREAD_BUFFERED) call flush_slot(this_thread() + 1)
            !$omp critical (pf_log_output)
            call deliver(sk%unit, sk%kind == SINK_CONSOLE, sk%path, big, sk%do_flush, died)
            !$omp end critical (pf_log_output)
            deallocate (big)
        end if
    end subroutine emit_to_sink

    !> The threshold a record must reach for this logger, given its name.
    !!
    !! A per-name override applies to a name equal to the rule's, and to any name below it in
    !! dotted notation -- the `getLogger('matplotlib').setLevel(...)` capability, with a small
    !! prefix table instead of a logger hierarchy. The longest matching rule wins.
    integer function effective_level(self, name, name_len) result(lev)
        type(pf_logger), intent(in) :: self    !! The logger.
        character(len=*), intent(in) :: name   !! The record's name.
        integer, intent(in) :: name_len        !! Used length of `name`.
        integer :: i, rl, best

        lev = self%level
        best = -1
        do i = 1, self%nrules
            rl = len_trim(self%rule(i)%name)
            if (rl == 0 .or. rl > name_len) cycle
            if (name(1:rl) /= self%rule(i)%name(1:rl)) cycle
            if (rl < name_len) then
                if (name(rl + 1:rl + 1) /= ".") cycle
            end if
            if (rl > best) then
                best = rl
                lev = self%rule(i)%level
            end if
        end do
    end function effective_level

    !> Recomputes the cached cheapest threshold any sink accepts, so that `%enabled` and the first
    !> line of `emit_core` are a single integer comparison rather than a walk over the sinks.
    !!
    !! **The per-name rules are folded in, which is what lets an override LOWER a threshold.** The
    !! cache has to be a true lower bound over every threshold a record could face, and a record's
    !! logger-side threshold is `effective_level`, whose range is the logger's own level together
    !! with every rule's. Taking the logger's level alone would leave this cache above a lowering
    !! rule, and the name-blind first gate in `emit_core` would then drop the record before its
    !! name was ever read -- so the rule would silently do nothing, which is how this behaved
    !! before. The exact per-name decision still happens at the second gate; this one only has to
    !! avoid excluding a record the second gate would have admitted.
    !!
    !! A sink's own threshold is *not* lowered by a rule and is still combined with `max`: a name
    !! override governs which records the LOGGER offers, never which ones a sink accepts.
    subroutine recompute_min_level(self)
        type(pf_logger), intent(inout) :: self  !! The logger whose cache is refreshed.
        integer :: i, floor_level

        if (self%nsinks == 0) then
            self%min_level = PF_LEVEL_OFF
            return
        end if
        floor_level = self%level
        do i = 1, self%nrules
            floor_level = min(floor_level, self%rule(i)%level)
        end do
        self%min_level = PF_LEVEL_OFF
        do i = 1, self%nsinks
            if (self%sink(i)%dead) cycle
            self%min_level = min(self%min_level, max(floor_level, self%sink(i)%level))
        end do
    end subroutine recompute_min_level

    !> The one path every record takes: threshold, name override, gathering, deduplication, then
    !> one rendered line per sink.
    !!
    !! The level test is first and is one integer comparison, so a record below the threshold
    !! costs essentially nothing. Rendering happens outside the output critical section; only the
    !! `write` is inside it.
    subroutine emit_core(self, level, msg, implicit, name, context, once, every)
        class(pf_logger), intent(inout) :: self            !! The logger.
        integer, intent(in) :: level                       !! The record's severity.
        character(len=*), intent(in) :: msg                !! The message text.
        logical, intent(in) :: implicit                    !! Whether an implicit stdout console applies.
        character(len=*), intent(in), optional :: name     !! Per-call name, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Per-call context, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.
        type(rec_fields) :: rec
        type(pf_sink) :: implicit_sk
        integer :: i, ev
        logical :: want_once, admit, died

        if (self%nsinks == 0) then
            if (.not. implicit) return
            if (level < max(self%level, PF_LEVEL_INFO)) return
        else
            if (level < self%min_level) return
        end if

        ! The name is resolved first and the per-name threshold applied to it, BEFORE the clock
        ! reads and the context assembly. A record dropped by a name rule therefore pays a string
        ! copy and a short prefix scan, not the whole record.
        call fill_record_name(self, level, name, rec)
        if (level < effective_level(self, rec%name, rec%name_len)) return
        call fill_record_rest(self, context, rec)

        want_once = .false.
        if (present(once)) want_once = once
        ev = 0
        if (present(every)) ev = every
        if (want_once .or. ev > 1) then
            !$omp critical (pf_log_output)
            admit = dedup_admits(rec, msg, want_once, ev)
            !$omp end critical (pf_log_output)
            if (.not. admit) return
        end if

        if (self%nsinks == 0) then
            call default_console_sink(implicit_sk)
            call emit_to_sink(implicit_sk, rec, msg, self%thread_mode, died)
            if (died) then
                call machinery_warning("writing to the console failed; console output is disabled " // &
                    "for the rest of this run", g_warned_console)
            end if
            return
        end if

        do i = 1, self%nsinks
            call emit_to_sink(self%sink(i), rec, msg, self%thread_mode, died)
            if (died) then
                self%sink(i)%dead = .true.
                call recompute_min_level(self)
                call machinery_warning("writing to a console sink failed; that sink is dropped " // &
                    "for the rest of this run", g_warned_console)
            end if
        end do
    end subroutine emit_core

    ! ================================================================================
    ! Configuration. Single-threaded only -- configure before entering a parallel region.
    ! ================================================================================

    !> Clears every sink and then installs one stdout console sink, unless `console = .false.`
    !!
    !! This is the "give me a working logger" call, the counterpart of Python's `basicConfig`,
    !! which likewise installs a stream handler. `%init(console = .false.)` is the "give me a clean
    !! slate" call. `format` applies to the console sink this call installs and does **not** become
    !! a default for sinks added later.
    subroutine logger_init(self, level, name, format, console, thread_mode)
        class(pf_logger), intent(inout) :: self             !! The logger being configured.
        integer, intent(in), optional :: level              !! Logger-wide threshold; default `PF_LEVEL_ALL`.
        character(len=*), intent(in), optional :: name      !! Name rendered as `{name}`.
        character(len=*), intent(in), optional :: format    !! Layout for the console sink installed here.
        logical, intent(in), optional :: console            !! `.false.` to install no sink at all.
        integer, intent(in), optional :: thread_mode        !! `PF_LOG_THREAD_DIRECT` or `_BUFFERED`.
        logical :: want_console

        call ensure_clock()
        call logger_close(self)
        self%level = PF_LEVEL_ALL
        self%name = ""
        self%rank = PF_LOG_RANK_ANY
        self%nrules = 0
        self%thread_mode = PF_LOG_THREAD_DIRECT
        if (present(level)) self%level = level
        if (present(name)) call logger_set_name(self, name)
        if (present(thread_mode)) call logger_set_thread_mode(self, thread_mode)
        want_console = .true.
        if (present(console)) want_console = console
        if (want_console) then
            call logger_add_console(self, format = format)
        else
            call recompute_min_level(self)
        end if
    end subroutine logger_init

    !> Attaches a console sink on standard output or standard error.
    !!
    !! A second console sink on the same stream would double every line, so it is refused --
    !! the "guard a mutating procedure against being called twice" rule this project applies
    !! generally.
    subroutine logger_add_console(self, stream, level, format, color, only_rank, sink)
        class(pf_logger), intent(inout) :: self           !! The logger being configured.
        integer, intent(in), optional :: stream           !! `PF_LOG_STDOUT` (default) or `PF_LOG_STDERR`.
        integer, intent(in), optional :: level            !! This sink's threshold; default `PF_LEVEL_ALL`.
        character(len=*), intent(in), optional :: format  !! Layout template; default `PF_LOG_FMT_BRIEF`.
        integer, intent(in), optional :: color            !! Colour policy; default `PF_LOG_COLOR_AUTO`.
        integer, intent(in), optional :: only_rank        !! Emit only when the logger's rank matches.
        integer, intent(out), optional :: sink            !! Receives this sink's id.
        integer :: s, st

        st = PF_LOG_STDOUT
        if (present(stream)) st = stream
        if (st /= PF_LOG_STDOUT .and. st /= PF_LOG_STDERR) then
            error stop "pf_logger%add_console: stream must be PF_LOG_STDOUT or PF_LOG_STDERR"
        end if
        do s = 1, self%nsinks
            if (self%sink(s)%kind == SINK_CONSOLE .and. self%sink(s)%stream == st) then
                error stop "pf_logger%add_console: a console sink on this stream is already " // &
                    "attached; a second one would double every line"
            end if
        end do
        s = new_sink(self, SINK_CONSOLE, level, format, color, only_rank, PF_LOG_FMT_BRIEF)
        self%sink(s)%stream = st
        if (st == PF_LOG_STDERR) then
            self%sink(s)%unit = error_unit
        else
            self%sink(s)%unit = output_unit
        end if
        self%sink(s)%use_color = resolve_color(self%sink(s)%color, SINK_CONSOLE)
        call recompute_min_level(self)
        if (present(sink)) sink = s
    end subroutine logger_add_console

    !> Opens `path` and attaches it as a sink.
    !!
    !! **`append` defaults to `.true.`**, matching Python's `FileHandler` and the `qfeet` logging
    !! module: truncating by default would mean a restarted pipeline silently destroying the log of
    !! the run that just failed, which is the log someone is about to want. `append = .false.`
    !! truncates. A file that cannot be opened is a clean `error stop` naming the path and the
    !! runtime's own message.
    subroutine logger_add_file(self, path, level, format, append, flush, only_rank, sink)
        class(pf_logger), intent(inout) :: self           !! The logger being configured.
        character(len=*), intent(in) :: path              !! Path to open.
        integer, intent(in), optional :: level            !! This sink's threshold; default `PF_LEVEL_ALL`.
        character(len=*), intent(in), optional :: format  !! Layout template; default `PF_LOG_FMT_FULL`.
        logical, intent(in), optional :: append           !! `.false.` to truncate; default `.true.`.
        logical, intent(in), optional :: flush            !! Flush after every record; default `.true.`.
        integer, intent(in), optional :: only_rank        !! Emit only when the logger's rank matches.
        integer, intent(out), optional :: sink            !! Receives this sink's id.
        integer :: s, u, ios
        logical :: want_append
        character(len=256) :: iom

        if (len_trim(path) > PF_LOG_MAX_PATH) then
            error stop "pf_logger%add_file: path is longer than PF_LOG_MAX_PATH characters"
        end if
        if (len_trim(path) == 0) error stop "pf_logger%add_file: path is empty"
        want_append = .true.
        if (present(append)) want_append = append
        if (want_append) then
            open (newunit = u, file = trim(path), action = "write", form = "formatted", &
                status = "unknown", position = "append", iostat = ios, iomsg = iom)
        else
            open (newunit = u, file = trim(path), action = "write", form = "formatted", &
                status = "replace", position = "rewind", iostat = ios, iomsg = iom)
        end if
        if (ios /= 0) then
            error stop "pf_logger%add_file: cannot open '" // trim(path) // "': " // trim(iom)
        end if
        s = new_sink(self, SINK_FILE, level, format, PF_LOG_COLOR_NEVER, only_rank, PF_LOG_FMT_FULL)
        self%sink(s)%unit = u
        self%sink(s)%path = path
        self%sink(s)%use_color = resolve_color(self%sink(s)%color, SINK_FILE)
        if (present(flush)) self%sink(s)%do_flush = flush
        call recompute_min_level(self)
        if (present(sink)) sink = s
    end subroutine logger_add_file

    !> Attaches a unit the caller opened and still owns. **This logger never closes it.**
    !!
    !! It is how a test captures output into a scratch file it can read back, and how a program
    !! that already manages its own output file adds logging to it. The unit is validated here;
    !! a caller that closes it later while it is still attached is a documented contract violation
    !! with no cheap runtime check -- but not a silent one either, since the next record's write
    !! fails and aborts naming the unit.
    subroutine logger_add_unit(self, unit, level, format, color, only_rank, sink)
        class(pf_logger), intent(inout) :: self           !! The logger being configured.
        integer, intent(in) :: unit                       !! An open, writable, formatted unit.
        integer, intent(in), optional :: level            !! This sink's threshold; default `PF_LEVEL_ALL`.
        character(len=*), intent(in), optional :: format  !! Layout template; default `PF_LOG_FMT_FULL`.
        integer, intent(in), optional :: color            !! Colour policy; default `PF_LOG_COLOR_AUTO`.
        integer, intent(in), optional :: only_rank        !! Emit only when the logger's rank matches.
        integer, intent(out), optional :: sink            !! Receives this sink's id.
        integer :: s
        logical :: is_open
        character(len=16) :: wr, fm
        character(len=16) :: num

        inquire (unit = unit, opened = is_open, write = wr, form = fm)
        write (num, '(i0)') unit
        if (.not. is_open) then
            error stop "pf_logger%add_unit: unit " // trim(num) // " is not connected"
        end if
        if (wr /= "YES") then
            error stop "pf_logger%add_unit: unit " // trim(num) // " is not writable"
        end if
        if (fm /= "FORMATTED") then
            error stop "pf_logger%add_unit: unit " // trim(num) // " is not a formatted unit"
        end if
        s = new_sink(self, SINK_UNIT, level, format, color, only_rank, PF_LOG_FMT_FULL)
        self%sink(s)%unit = unit
        self%sink(s)%use_color = resolve_color(self%sink(s)%color, SINK_UNIT)
        call recompute_min_level(self)
        if (present(sink)) sink = s
    end subroutine logger_add_unit

    !> Reserves and initialises one sink slot, applying the arguments every `add_*` shares.
    integer function new_sink(self, kind, level, format, color, only_rank, default_fmt) result(s)
        class(pf_logger), intent(inout) :: self           !! The logger gaining a sink.
        integer, intent(in) :: kind                       !! `SINK_CONSOLE`, `SINK_FILE` or `SINK_UNIT`.
        integer, intent(in), optional :: level            !! This sink's threshold.
        character(len=*), intent(in), optional :: format  !! Layout template.
        integer, intent(in), optional :: color            !! Colour policy.
        integer, intent(in), optional :: only_rank        !! Rank filter.
        character(len=*), intent(in) :: default_fmt       !! Template used when `format` is absent.

        call ensure_clock()
        if (self%nsinks >= PF_LOG_MAX_SINKS) then
            error stop "pf_logger: this logger already owns PF_LOG_MAX_SINKS sinks"
        end if
        self%nsinks = self%nsinks + 1
        s = self%nsinks
        call reset_sink(self%sink(s), kind)
        if (present(level)) self%sink(s)%level = level
        if (present(color)) self%sink(s)%color = color
        if (present(only_rank)) then
            if (only_rank /= PF_LOG_RANK_ANY) then
                if (self%rank == PF_LOG_RANK_ANY) then
                    error stop "pf_logger: only_rank= needs this logger's own rank, so call " // &
                        "%set_rank before adding a rank-filtered sink"
                end if
            end if
            self%sink(s)%only_rank = only_rank
        end if
        if (present(format)) then
            call parse_template(self%sink(s), format)
        else
            call parse_template(self%sink(s), default_fmt)
        end if
    end function new_sink

    !> Returns one sink slot to its default state and stamps its kind.
    !!
    !! Written out rather than using a structure constructor: nagfor requires a value for every
    !! derived-type array component in a constructor even when that component's own type is fully
    !! default-initialised, so `pf_sink(kind = kind)` is not portable.
    subroutine reset_sink(sk, kind)
        type(pf_sink), intent(out) :: sk  !! The slot to reset; `intent(out)` does the defaulting.
        integer, intent(in) :: kind       !! `SINK_CONSOLE`, `SINK_FILE` or `SINK_UNIT`.

        sk%kind = kind
    end subroutine reset_sink

    !> Sets the logger-wide threshold, one sink's threshold, or a per-name override.
    !!
    !! With neither selector it is the logger's own threshold; `sink =` addresses one sink;
    !! `name =` adds or updates an override applying to that name and to every name below it in
    !! dotted notation, which is how an application turns down a library's noise. **Passing both
    !! selectors is refused** rather than guessed at -- the combination has no useful meaning.
    !!
    !! **An override works in BOTH directions**: it may raise a name's threshold above the
    !! logger's, and it may lower one below it, which is how one subsystem is turned up to
    !! `PF_LEVEL_TRACE` while the rest of the program stays quiet. A rule is replaced by setting
    !! the same name again. **To undo one, use `%unset_level` rather than setting it back to the
    !! logger's current level** -- the latter leaves a rule behind holding a snapshot of that
    !! level, which then stops tracking the logger, and it does not free the slot.
    !!
    !! **A lowering rule has a cost, and it is bounded.** It drops the cached first-gate threshold
    !! to the new floor, so records between that floor and the logger's own level now reach the
    !! per-name test instead of being rejected by one integer comparison. That test is a string
    !! copy and a scan of at most `PF_LOG_MAX_NAME_RULES` prefixes; the clock reads and the context
    !! assembly still happen only for a record that passes it. The cost lasts until the rule is
    !! raised back, and a logger with no lowering rule pays nothing at all.
    !!
    !! A rule never lowers a **sink's** own threshold: it governs which records the logger offers,
    !! not which ones a sink accepts.
    subroutine logger_set_level(self, level, sink, name)
        class(pf_logger), intent(inout) :: self         !! The logger being configured.
        integer, intent(in) :: level                    !! The threshold to set.
        integer, intent(in), optional :: sink           !! Sink id from an `add_*` call.
        character(len=*), intent(in), optional :: name  !! Name prefix the override applies to.
        integer :: i

        if (present(sink) .and. present(name)) then
            error stop "pf_logger%set_level: sink= and name= cannot be combined"
        end if
        if (present(sink)) then
            call check_sink(self, sink, "set_level")
            self%sink(sink)%level = level
        else if (present(name)) then
            if (len_trim(name) == 0) error stop "pf_logger%set_level: name= is empty"
            if (len_trim(name) > PF_LOG_MAX_NAME) then
                error stop "pf_logger%set_level: name= is longer than PF_LOG_MAX_NAME characters"
            end if
            do i = 1, self%nrules
                if (self%rule(i)%name == name) then
                    self%rule(i)%level = level
                    ! Falls through to recompute_min_level rather than returning: a rule that
                    ! lowers a threshold has to reach the cache, or the name-blind first gate in
                    ! emit_core drops the record before the rule is consulted and the call
                    ! silently does nothing. Raising one back has to reach it too, or the floor
                    ! stays low and every record keeps paying for a rule that no longer exists.
                    exit
                end if
            end do
            if (i > self%nrules) then
                if (self%nrules >= PF_LOG_MAX_NAME_RULES) then
                    error stop "pf_logger%set_level: this logger already holds " // &
                        "PF_LOG_MAX_NAME_RULES per-name overrides"
                end if
                self%nrules = self%nrules + 1
                self%rule(self%nrules)%name = name
                self%rule(self%nrules)%level = level
            end if
        else
            self%level = level
        end if
        call recompute_min_level(self)
    end subroutine logger_set_level

    !> Removes one per-name level override, or every one, restoring the logger's own threshold for
    !> the names concerned.
    !!
    !! **This is not the same as setting the override back to the logger's current level**, and the
    !! difference is why the procedure exists. Setting it back *snapshots* that level into a rule
    !! that still exists: a later `%set_level(...)` on the logger then fails to reach the name, so a
    !! rule meant to have been undone silently makes one name diverge from the rest of the program.
    !! Removing it leaves nothing behind, and the name follows the logger again.
    !!
    !! **It also frees the slot**, which the snapshot does not. A logger holds
    !! `PF_LOG_MAX_NAME_RULES` overrides and appends a new name to the first free slot, so a
    !! long-running program that turns tracing on and off for more *distinct* names than that would
    !! otherwise abort -- having "undone" every one of them. `%init` is the only other way to clear
    !! the table, and it destroys every sink with it.
    !!
    !! With `name` absent every override is removed. That mirrors `%set_format`/`%set_color`, where
    !! an absent selector likewise means "all of them", and it is unambiguous because an override's
    !! name can never be empty -- `%set_level` refuses one.
    !!
    !! Removing an override that is not there is a no-op rather than an error, so the call is safe
    !! to make unconditionally; pass `found` when you need to know which it was.
    subroutine logger_unset_level(self, name, found)
        class(pf_logger), intent(inout) :: self          !! The logger being configured.
        character(len=*), intent(in), optional :: name   !! The override to remove; absent means every one.
        logical, intent(out), optional :: found          !! Receives whether anything was removed.
        integer :: i, k
        logical :: hit

        if (.not. present(name)) then
            hit = self%nrules > 0
            self%nrules = 0
            call recompute_min_level(self)
            if (present(found)) found = hit
            return
        end if

        ! Validated exactly as %set_level validates it, so the same name is accepted or refused by
        ! both. A malformed name is a defect at the call site, not a missing rule.
        if (len_trim(name) == 0) error stop "pf_logger%unset_level: name= is empty"
        if (len_trim(name) > PF_LOG_MAX_NAME) then
            error stop "pf_logger%unset_level: name= is longer than PF_LOG_MAX_NAME characters"
        end if

        hit = .false.
        do i = 1, self%nrules
            if (self%rule(i)%name /= name) cycle
            ! Shift the tail down rather than swapping the last entry into the hole. Lookup takes
            ! the longest match and so does not care about order, but keeping insertion order costs
            ! nothing at this size and keeps the table readable in a debugger.
            do k = i, self%nrules - 1
                self%rule(k) = self%rule(k + 1)
            end do
            self%rule(self%nrules)%name = ""
            self%rule(self%nrules)%level = PF_LEVEL_ALL
            self%nrules = self%nrules - 1
            hit = .true.
            exit
        end do
        ! Unconditional, not only when something was removed: dropping an override that LOWERED a
        ! threshold has to raise the cached floor back, or the cost of the removed rule outlives it.
        call recompute_min_level(self)
        if (present(found)) found = hit
    end subroutine logger_unset_level

    !> Sets the layout template of one sink, or of **every sink currently attached**.
    !!
    !! With no `sink =` it applies to the sinks that exist now and does **not** become a default
    !! for sinks added later: a sticky default would make two identical call sequences behave
    !! differently depending on their order. A later sink carries its own `format =` argument,
    !! which is where a per-sink default belongs.
    subroutine logger_set_format(self, template, sink)
        class(pf_logger), intent(inout) :: self     !! The logger being configured.
        character(len=*), intent(in) :: template    !! The layout template.
        integer, intent(in), optional :: sink       !! Sink id; absent means every current sink.
        integer :: i

        call ensure_clock()
        if (present(sink)) then
            call check_sink(self, sink, "set_format")
            call parse_template(self%sink(sink), template)
        else
            do i = 1, self%nsinks
                call parse_template(self%sink(i), template)
            end do
        end if
    end subroutine logger_set_format

    !> Sets the colour policy of one sink, or of every sink currently attached. Same selector rule
    !> as `%set_format`.
    subroutine logger_set_color(self, policy, sink)
        class(pf_logger), intent(inout) :: self  !! The logger being configured.
        integer, intent(in) :: policy            !! `PF_LOG_COLOR_AUTO`/`_NEVER`/`_ALWAYS`.
        integer, intent(in), optional :: sink    !! Sink id; absent means every current sink.
        integer :: i

        if (policy < PF_LOG_COLOR_AUTO .or. policy > PF_LOG_COLOR_ALWAYS) then
            error stop "pf_logger%set_color: policy must be one of PF_LOG_COLOR_AUTO/_NEVER/_ALWAYS"
        end if
        if (present(sink)) then
            call check_sink(self, sink, "set_color")
            self%sink(sink)%color = policy
            self%sink(sink)%use_color = resolve_color(policy, self%sink(sink)%kind)
        else
            do i = 1, self%nsinks
                self%sink(i)%color = policy
                self%sink(i)%use_color = resolve_color(policy, self%sink(i)%kind)
            end do
        end if
    end subroutine logger_set_color

    !> Rejects a sink id that no `add_*` call returned.
    subroutine check_sink(self, sink, who)
        class(pf_logger), intent(in) :: self  !! The logger being addressed.
        integer, intent(in) :: sink           !! The id to validate.
        character(len=*), intent(in) :: who   !! Calling procedure, for the message.
        character(len=16) :: num

        if (sink < 1 .or. sink > self%nsinks) then
            write (num, '(i0)') sink
            error stop "pf_logger%" // who // ": no sink with id " // trim(num)
        end if
    end subroutine check_sink

    !> Sets the name rendered as `{name}` and keyed on by per-name overrides.
    subroutine logger_set_name(self, name)
        class(pf_logger), intent(inout) :: self  !! The logger being configured.
        character(len=*), intent(in) :: name     !! The name, at most `PF_LOG_MAX_NAME` characters.

        if (len_trim(name) > PF_LOG_MAX_NAME) then
            error stop "pf_logger%set_name: name is longer than PF_LOG_MAX_NAME characters"
        end if
        self%name = name
    end subroutine logger_set_name

    !> Sets the rank rendered as `{rank}` and tested by a sink's `only_rank` filter.
    !!
    !! The number is whatever identity the program already has -- an MPI rank, a worker index, a
    !! chunk id -- supplied by the caller, so this adds no MPI dependency. `PF_LOG_RANK_ANY`
    !! clears it; any other negative value is refused.
    subroutine logger_set_rank(self, rank)
        class(pf_logger), intent(inout) :: self  !! The logger being configured.
        integer, intent(in) :: rank              !! The rank, or `PF_LOG_RANK_ANY` to clear it.

        if (rank < 0 .and. rank /= PF_LOG_RANK_ANY) then
            error stop "pf_logger%set_rank: a rank must be non-negative, or PF_LOG_RANK_ANY to clear it"
        end if
        self%rank = rank
    end subroutine logger_set_rank

    !> Selects direct or buffered emission, and sizes the buffered-mode collector.
    !!
    !! **Switching away from buffered mode flushes every slot first**, or the switch would strand
    !! whatever is still in flight -- the same data-loss shape a `threadprivate` store would have,
    !! reachable by a one-line configuration change.
    !!
    !! The collector is one slot per thread, sized here to `omp_get_max_threads()`. `slot_bytes`
    !! is settable because the total is that many times the slot size: half a megabyte on an
    !! eight-core machine, but tens of megabytes on a machine with hundreds of threads.
    subroutine logger_set_thread_mode(self, mode, slot_bytes)
#ifdef _OPENMP
        use omp_lib, only: omp_get_max_threads
#endif
        class(pf_logger), intent(inout) :: self     !! The logger being configured.
        integer, intent(in) :: mode                 !! `PF_LOG_THREAD_DIRECT` or `PF_LOG_THREAD_BUFFERED`.
        integer, intent(in), optional :: slot_bytes !! Bytes per thread; default `PF_LOG_MAX_BUFFER_BYTES`.
        integer :: nthreads, want, i

        if (mode /= PF_LOG_THREAD_DIRECT .and. mode /= PF_LOG_THREAD_BUFFERED) then
            error stop "pf_logger%set_thread_mode: mode must be PF_LOG_THREAD_DIRECT or " // &
                "PF_LOG_THREAD_BUFFERED"
        end if
        if (self%thread_mode == PF_LOG_THREAD_BUFFERED .and. mode == PF_LOG_THREAD_DIRECT) then
            call flush_collector()
        end if
        self%thread_mode = mode
        if (mode /= PF_LOG_THREAD_BUFFERED) return

        want = PF_LOG_MAX_BUFFER_BYTES
        if (present(slot_bytes)) want = slot_bytes
        if (want < PF_LOG_MIN_BUFFER_BYTES) then
            error stop "pf_logger%set_thread_mode: slot_bytes is below PF_LOG_MIN_BUFFER_BYTES"
        end if
        nthreads = 1
#ifdef _OPENMP
        nthreads = omp_get_max_threads()
#endif
        if (allocated(g_slots)) then
            if (size(g_slots) >= nthreads .and. g_slot_bytes == want) return
            call flush_collector()
            deallocate (g_slots)
        end if
        allocate (g_slots(nthreads))
        g_slot_bytes = want
        do i = 1, nthreads
            allocate (character(len=want) :: g_slots(i)%text)
        end do
    end subroutine logger_set_thread_mode

    !> Emits every collector slot, in slot order. A configuration-class operation: call it from
    !> one thread, outside any parallel region.
    subroutine flush_collector()
        integer :: i

        if (.not. allocated(g_slots)) return
        do i = 1, size(g_slots)
            call flush_slot(i)
        end do
    end subroutine flush_collector

    !> Closes every unit this logger opened and clears every sink, leaving the logger silent.
    !!
    !! **Never aborts.** It is a cleanup path, frequently reached while a program is already
    !! failing, and aborting here would hide the original error. It is also idempotent across
    !! copies of a logger: a unit some other copy already closed is simply skipped, so a defensive
    !! close costs nothing. A write to a sink that has been closed is a different matter and does
    !! abort, naming the path -- output being lost as it happens is not something to pass over.
    subroutine logger_close(self)
        class(pf_logger), intent(inout) :: self  !! The logger being closed.
        integer :: i, ios
        logical :: is_open

        call flush_collector()
        do i = 1, self%nsinks
            if (self%sink(i)%kind /= SINK_FILE) cycle
            inquire (unit = self%sink(i)%unit, opened = is_open)
            if (is_open) close (self%sink(i)%unit, iostat = ios)
        end do
        self%nsinks = 0
        call recompute_min_level(self)
    end subroutine logger_close

    !> Flushes every sink, and every collector slot under buffered mode.
    !!
    !! **Under buffered mode this is what recovers every thread's records**, and it must be called
    !! from one thread outside the parallel region -- which is exactly why the collector is shared
    !! rather than thread-private.
    subroutine logger_flush(self)
        class(pf_logger), intent(inout) :: self  !! The logger being flushed.
        integer :: i, ios

        call flush_collector()
        do i = 1, self%nsinks
            if (self%sink(i)%dead) cycle
            flush (self%sink(i)%unit, iostat = ios)
        end do
    end subroutine logger_flush

    !> Writes `n` blank lines (default 1) to every sink, with no layout and no level filtering.
    !!
    !! A blank separator is as useful in a log file as on a console, so unlike the `qfeet` module
    !! this reaches every sink rather than the terminal alone.
    subroutine logger_blank(self, n)
        class(pf_logger), intent(inout) :: self  !! The logger.
        integer, intent(in), optional :: n       !! How many blank lines; default 1.

        call logger_blank_impl(self, n, .false.)
    end subroutine logger_blank

    !> Writes blank lines, told explicitly whether an implicit stdout console applies -- which is
    !> true only for the module's default logger before it is configured.
    subroutine logger_blank_impl(self, n, implicit)
        class(pf_logger), intent(inout) :: self  !! The logger.
        integer, intent(in), optional :: n       !! How many blank lines; default 1.
        logical, intent(in) :: implicit          !! Whether the implicit console applies.
        integer :: i, k, count
        type(pf_sink) :: implicit_sk
        logical :: failed

        count = 1
        if (present(n)) count = n
        if (count <= 0) return
        if (self%nsinks == 0) then
            if (.not. implicit) return
            call default_console_sink(implicit_sk)
            do k = 1, count
                !$omp critical (pf_log_output)
                call deliver(implicit_sk%unit, .true., "", "", .true., failed)
                !$omp end critical (pf_log_output)
            end do
            return
        end if
        do i = 1, self%nsinks
            if (self%sink(i)%dead) cycle
            ! A blank line has no level, so a sink's threshold cannot apply to it -- but its RANK
            ! filter must, or the one thing only_rank= exists to prevent (every rank writing to the
            ! same console) comes back through the one call that has no message to filter on.
            if (self%sink(i)%only_rank /= PF_LOG_RANK_ANY) then
                if (self%rank /= self%sink(i)%only_rank) cycle
            end if
            do k = 1, count
                !$omp critical (pf_log_output)
                call deliver(self%sink(i)%unit, self%sink(i)%kind == SINK_CONSOLE, &
                    self%sink(i)%path, "", self%sink(i)%do_flush, failed)
                !$omp end critical (pf_log_output)
                if (failed) self%sink(i)%dead = .true.
            end do
        end do
    end subroutine logger_blank_impl

    !> Emits `text` at `PF_LEVEL_CRITICAL`, flushes every sink, then `error stop`s.
    !!
    !! The flush before the abort is the point: without it an unflushed file loses exactly the
    !! records describing the failure.
    subroutine logger_fatal(self, text)
        class(pf_logger), intent(inout) :: self  !! The logger.
        character(len=*), intent(in) :: text     !! The message.

        call emit_core(self, PF_LEVEL_CRITICAL, text, .false.)
        call logger_flush(self)
        error stop "pf_logger%fatal: " // text
    end subroutine logger_fatal

    !> Whether a record at `level` would reach any sink.
    !!
    !! The guard to put in front of an expensive message: `if (lg%enabled(PF_LEVEL_DEBUG)) ...`.
    !! It is one integer comparison against a cached threshold, not a walk over the sinks.
    !!
    !! **Without `name` it is a conservative hint, not an oracle.** Per-name overrides mean a
    !! record's threshold depends on its name, which this cannot see, so it can answer `.true.`
    !! for a record that is then dropped -- for a name whose override is stricter, and also for a
    !! name with no override at all once some other name has a rule lowering the cached floor.
    !! That is the safe direction -- work is wasted, output is never lost. Pass `name` to get the
    !! exact answer, which accounts for both directions of override.
    logical function logger_enabled(self, level, name) result(yes)
        class(pf_logger), intent(in) :: self            !! The logger.
        integer, intent(in) :: level                    !! The level a record would carry.
        character(len=*), intent(in), optional :: name  !! The name that record would carry.

        yes = level >= self%min_level
        if (yes .and. present(name)) then
            yes = level >= max(self%min_level, effective_level(self, name, len_trim(name)))
        end if
    end function logger_enabled

    ! ================================================================================
    ! Emission
    ! ================================================================================

    !> Renders one `integer(int32)` into `buf`, honouring `fmt` when given.
    subroutine one_i32(v, fmt, buf)
        integer(int32), intent(in) :: v                !! The value.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor.
        character(len=*), intent(out) :: buf           !! Receives the text.

        if (present(fmt)) then
            write (buf, fmt) v
        else
            write (buf, '(i0)') v
        end if
    end subroutine one_i32

    !> Renders one `integer(int64)` into `buf`, honouring `fmt` when given.
    subroutine one_i64(v, fmt, buf)
        integer(int64), intent(in) :: v                !! The value.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor.
        character(len=*), intent(out) :: buf           !! Receives the text.

        if (present(fmt)) then
            write (buf, fmt) v
        else
            write (buf, '(i0)') v
        end if
    end subroutine one_i64

    !> Renders one `real(real32)` into `buf`, honouring `fmt` when given.
    subroutine one_r32(v, fmt, buf)
        real(real32), intent(in) :: v                  !! The value.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor.
        character(len=*), intent(out) :: buf           !! Receives the text.

        if (present(fmt)) then
            write (buf, fmt) v
        else
            write (buf, '(g0)') v
        end if
    end subroutine one_r32

    !> Renders one `real(real64)` into `buf`, honouring `fmt` when given.
    subroutine one_r64(v, fmt, buf)
        real(real64), intent(in) :: v                  !! The value.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor.
        character(len=*), intent(out) :: buf           !! Receives the text.

        if (present(fmt)) then
            write (buf, fmt) v
        else
            write (buf, '(g0)') v
        end if
    end subroutine one_r64

    !> Renders one `logical` into `buf`, honouring `fmt` when given.
    subroutine one_log(v, fmt, buf)
        logical, intent(in) :: v                       !! The value.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor.
        character(len=*), intent(out) :: buf           !! Receives the text.

        if (present(fmt)) then
            write (buf, fmt) v
        else
            write (buf, '(l1)') v
        end if
    end subroutine one_log

    !> Emits one record at an explicit level through this logger.
    subroutine logger_log(self, level, text, name, context, once, every)
        class(pf_logger), intent(inout) :: self            !! The logger.
        integer, intent(in) :: level                       !! The record's severity.
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(self, level, text, .false., name = name, context = context, once = once, every = every)
    end subroutine logger_log

    !> Emits one record at `PF_LEVEL_TRACE` through this logger.
    subroutine logger_trace(self, text, name, context, once, every)
        class(pf_logger), intent(inout) :: self            !! The logger.
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(self, PF_LEVEL_TRACE, text, .false., name = name, context = context, once = once, every = every)
    end subroutine logger_trace

    !> Emits one record at `PF_LEVEL_DEBUG` through this logger.
    subroutine logger_debug(self, text, name, context, once, every)
        class(pf_logger), intent(inout) :: self            !! The logger.
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(self, PF_LEVEL_DEBUG, text, .false., name = name, context = context, once = once, every = every)
    end subroutine logger_debug

    !> Emits one record at `PF_LEVEL_INFO` through this logger.
    subroutine logger_info(self, text, name, context, once, every)
        class(pf_logger), intent(inout) :: self            !! The logger.
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(self, PF_LEVEL_INFO, text, .false., name = name, context = context, once = once, every = every)
    end subroutine logger_info

    !> Emits one record at `PF_LEVEL_WARNING` through this logger.
    subroutine logger_warning(self, text, name, context, once, every)
        class(pf_logger), intent(inout) :: self            !! The logger.
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(self, PF_LEVEL_WARNING, text, .false., name = name, context = context, once = once, every = every)
    end subroutine logger_warning

    !> Emits one record at `PF_LEVEL_ERROR` through this logger.
    subroutine logger_error(self, text, name, context, once, every)
        class(pf_logger), intent(inout) :: self            !! The logger.
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(self, PF_LEVEL_ERROR, text, .false., name = name, context = context, once = once, every = every)
    end subroutine logger_error

    !> Emits one record at `PF_LEVEL_CRITICAL` through this logger.
    subroutine logger_critical(self, text, name, context, once, every)
        class(pf_logger), intent(inout) :: self            !! The logger.
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(self, PF_LEVEL_CRITICAL, text, .false., name = name, context = context, once = once, every = every)
    end subroutine logger_critical

    ! ---- The same surface again, on the module's default logger ----

    !> Emits one record at an explicit level through the default logger.
    subroutine pf_log(level, text, name, context, once, every)
        integer, intent(in) :: level                       !! The record's severity.
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(g_default, level, text, g_default_implicit, name = name, &
            context = context, once = once, every = every)
    end subroutine pf_log

    !> Emits one record at `PF_LEVEL_TRACE` through the default logger.
    subroutine pf_log_trace(text, name, context, once, every)
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(g_default, PF_LEVEL_TRACE, text, g_default_implicit, name = name, &
            context = context, once = once, every = every)
    end subroutine pf_log_trace

    !> Emits one record at `PF_LEVEL_DEBUG` through the default logger.
    subroutine pf_log_debug(text, name, context, once, every)
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(g_default, PF_LEVEL_DEBUG, text, g_default_implicit, name = name, &
            context = context, once = once, every = every)
    end subroutine pf_log_debug

    !> Emits one record at `PF_LEVEL_INFO` through the default logger.
    subroutine pf_log_info(text, name, context, once, every)
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(g_default, PF_LEVEL_INFO, text, g_default_implicit, name = name, &
            context = context, once = once, every = every)
    end subroutine pf_log_info

    !> Emits one record at `PF_LEVEL_WARNING` through the default logger.
    subroutine pf_log_warning(text, name, context, once, every)
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(g_default, PF_LEVEL_WARNING, text, g_default_implicit, name = name, &
            context = context, once = once, every = every)
    end subroutine pf_log_warning

    !> Emits one record at `PF_LEVEL_ERROR` through the default logger.
    subroutine pf_log_error(text, name, context, once, every)
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(g_default, PF_LEVEL_ERROR, text, g_default_implicit, name = name, &
            context = context, once = once, every = every)
    end subroutine pf_log_error

    !> Emits one record at `PF_LEVEL_CRITICAL` through the default logger.
    subroutine pf_log_critical(text, name, context, once, every)
        character(len=*), intent(in) :: text               !! The message text.
        character(len=*), intent(in), optional :: name     !! Name for this record, overriding the logger's.
        character(len=*), intent(in), optional :: context  !! Context for this record, overriding the ambient one.
        logical, intent(in), optional :: once              !! Emit this record once per process and never again.
        integer, intent(in), optional :: every             !! Emit every n-th occurrence of this record.

        call emit_core(g_default, PF_LEVEL_CRITICAL, text, g_default_implicit, name = name, &
            context = context, once = once, every = every)
    end subroutine pf_log_critical

    ! ================================================================================
    ! Configuration and control on the module's default logger
    ! ================================================================================

    !> Clears the default logger's sinks and installs a stdout console unless `console = .false.`
    subroutine pf_log_init(level, name, format, console, thread_mode)
        integer, intent(in), optional :: level           !! Logger-wide threshold.
        character(len=*), intent(in), optional :: name   !! Name rendered as `{name}`.
        character(len=*), intent(in), optional :: format !! Layout for the console sink installed here.
        logical, intent(in), optional :: console         !! `.false.` to install no sink at all.
        integer, intent(in), optional :: thread_mode     !! `PF_LOG_THREAD_DIRECT` or `_BUFFERED`.

        g_default_implicit = .false.
        call g_default%init(level = level, name = name, format = format, console = console, &
            thread_mode = thread_mode)
    end subroutine pf_log_init

    !> Attaches a console sink to the default logger.
    subroutine pf_log_add_console(stream, level, format, color, only_rank, sink)
        integer, intent(in), optional :: stream           !! `PF_LOG_STDOUT` (default) or `PF_LOG_STDERR`.
        integer, intent(in), optional :: level            !! This sink's threshold.
        character(len=*), intent(in), optional :: format  !! Layout template.
        integer, intent(in), optional :: color            !! Colour policy.
        integer, intent(in), optional :: only_rank        !! Emit only when the rank matches.
        integer, intent(out), optional :: sink            !! Receives this sink's id.

        g_default_implicit = .false.
        call g_default%add_console(stream = stream, level = level, format = format, color = color, &
            only_rank = only_rank, sink = sink)
    end subroutine pf_log_add_console

    !> Opens a file and attaches it as a sink of the default logger.
    subroutine pf_log_add_file(path, level, format, append, flush, only_rank, sink)
        character(len=*), intent(in) :: path              !! Path to open.
        integer, intent(in), optional :: level            !! This sink's threshold.
        character(len=*), intent(in), optional :: format  !! Layout template.
        logical, intent(in), optional :: append           !! `.false.` to truncate; default `.true.`.
        logical, intent(in), optional :: flush            !! Flush after every record; default `.true.`.
        integer, intent(in), optional :: only_rank        !! Emit only when the rank matches.
        integer, intent(out), optional :: sink            !! Receives this sink's id.

        g_default_implicit = .false.
        call g_default%add_file(path, level = level, format = format, append = append, &
            flush = flush, only_rank = only_rank, sink = sink)
    end subroutine pf_log_add_file

    !> Attaches a caller-owned unit as a sink of the default logger.
    subroutine pf_log_add_unit(unit, level, format, color, only_rank, sink)
        integer, intent(in) :: unit                       !! An open, writable, formatted unit.
        integer, intent(in), optional :: level            !! This sink's threshold.
        character(len=*), intent(in), optional :: format  !! Layout template.
        integer, intent(in), optional :: color            !! Colour policy.
        integer, intent(in), optional :: only_rank        !! Emit only when the rank matches.
        integer, intent(out), optional :: sink            !! Receives this sink's id.

        g_default_implicit = .false.
        call g_default%add_unit(unit, level = level, format = format, color = color, &
            only_rank = only_rank, sink = sink)
    end subroutine pf_log_add_unit

    !> Sets the default logger's threshold, one of its sinks', or a per-name override.
    subroutine pf_log_set_level(level, sink, name)
        integer, intent(in) :: level                    !! The threshold to set.
        integer, intent(in), optional :: sink           !! Sink id.
        character(len=*), intent(in), optional :: name  !! Name prefix the override applies to.

        call g_default%set_level(level, sink = sink, name = name)
    end subroutine pf_log_set_level

    !> Removes one per-name override from the default logger, or every one. See `%unset_level`.
    subroutine pf_log_unset_level(name, found)
        character(len=*), intent(in), optional :: name  !! The override to remove; absent means every one.
        logical, intent(out), optional :: found         !! Receives whether anything was removed.

        call g_default%unset_level(name = name, found = found)
    end subroutine pf_log_unset_level

    !> Sets the layout of one of the default logger's sinks, or of every current sink.
    subroutine pf_log_set_format(template, sink)
        character(len=*), intent(in) :: template  !! The layout template.
        integer, intent(in), optional :: sink     !! Sink id; absent means every current sink.

        call g_default%set_format(template, sink = sink)
    end subroutine pf_log_set_format

    !> Sets the colour policy of one of the default logger's sinks, or of every current sink.
    subroutine pf_log_set_color(policy, sink)
        integer, intent(in) :: policy          !! `PF_LOG_COLOR_AUTO`/`_NEVER`/`_ALWAYS`.
        integer, intent(in), optional :: sink  !! Sink id; absent means every current sink.

        call g_default%set_color(policy, sink = sink)
    end subroutine pf_log_set_color

    !> Sets the default logger's name.
    subroutine pf_log_set_name(name)
        character(len=*), intent(in) :: name  !! The name rendered as `{name}`.

        call g_default%set_name(name)
    end subroutine pf_log_set_name

    !> Sets the default logger's rank.
    subroutine pf_log_set_rank(rank)
        integer, intent(in) :: rank  !! The rank, or `PF_LOG_RANK_ANY` to clear it.

        call g_default%set_rank(rank)
    end subroutine pf_log_set_rank

    !> Selects direct or buffered emission for the default logger.
    subroutine pf_log_set_thread_mode(mode, slot_bytes)
        integer, intent(in) :: mode                 !! `PF_LOG_THREAD_DIRECT` or `PF_LOG_THREAD_BUFFERED`.
        integer, intent(in), optional :: slot_bytes !! Bytes per thread in the collector.

        call g_default%set_thread_mode(mode, slot_bytes = slot_bytes)
    end subroutine pf_log_set_thread_mode

    !> Closes units the default logger opened and clears every sink, leaving it silent.
    subroutine pf_log_close()

        g_default_implicit = .false.
        call g_default%close()
    end subroutine pf_log_close

    !> Writes blank lines to every sink of the default logger.
    subroutine pf_log_blank(n)
        integer, intent(in), optional :: n  !! How many blank lines; default 1.

        call logger_blank_impl(g_default, n, g_default_implicit)
    end subroutine pf_log_blank

    !> Emits at `PF_LEVEL_CRITICAL` through the default logger, flushes, then `error stop`s.
    subroutine pf_log_fatal(text)
        character(len=*), intent(in) :: text  !! The message.

        call g_default%fatal(text)
    end subroutine pf_log_fatal

    !> Whether a record at `level` would reach any sink of the default logger.
    logical function pf_log_enabled(level, name) result(yes)
        integer, intent(in) :: level                    !! The level a record would carry.
        character(len=*), intent(in), optional :: name  !! The name that record would carry.

        if (g_default_implicit) then
            yes = level >= PF_LEVEL_INFO
            return
        end if
        yes = g_default%enabled(level, name = name)
    end function pf_log_enabled

    !> Flushes every sink of the default logger, and every collector slot.
    subroutine pf_log_flush()

        call g_default%flush()
    end subroutine pf_log_flush

    !> Clears the process-wide `once=`/`every=` table.
    !!
    !! **Neither `%init` nor `%close` does this**, deliberately: the table is shared by every
    !! logger, so one logger re-initialising itself must not silently un-suppress another's
    !! messages. This is the explicit reset, and it is what a test needs between cases.
    subroutine pf_log_reset_dedup()

        !$omp critical (pf_log_output)
        g_dedup_n = 0
        g_dedup_key = ""
        g_dedup_count = 0_int64
        !$omp end critical (pf_log_output)
    end subroutine pf_log_reset_dedup

    ! ================================================================================
    ! Context: a shared base, plus a per-thread stack
    ! ================================================================================

    !> Sets the shared base context, rendered ahead of every thread's own frames.
    !!
    !! **Set this outside parallel regions**, like any other configuration. It is shared rather
    !! than per-thread because an OpenMP `threadprivate` copy is undefined in every thread but the
    !! initial one at the start of a region -- so a per-thread base set before a region would be
    !! visible to thread 0 and to no other, which reads as a bug rather than as a limitation.
    subroutine pf_log_set_context(text)
        character(len=*), intent(in) :: text  !! The base tag; an empty string clears it.

        if (len_trim(text) > PF_LOG_MAX_CONTEXT) then
            error stop "pf_log_set_context: text is longer than PF_LOG_MAX_CONTEXT characters"
        end if
        g_base_context = text
        g_base_len = len_trim(text)
    end subroutine pf_log_set_context

    !> Pushes one context frame onto the **calling thread's** stack.
    !!
    !! Frames nest: a routine called from inside a context may push its own, and its `pop` removes
    !! only that frame -- it can neither see nor destroy its caller's. `{context}` renders every
    !! frame from the outermost in, after the shared base.
    !!
    !! **The depth and byte budgets saturate; they never abort and never misattribute.** Past
    !! `PF_LOG_MAX_CONTEXT_DEPTH` frames or `PF_LOG_MAX_CONTEXT` rendered bytes the frame's *text*
    !! is dropped while the *depth accounting stays exact*, so every later `pop` still removes the
    !! right frame and no record is ever tagged with another frame's context. One warning is
    !! issued the first time it happens. A missing frame degrades a diagnostic; a shifted stack
    !! would lie about which unit of work a record came from.
    subroutine pf_log_push_context(text, frame)
        character(len=*), intent(in) :: text     !! The tag for this frame.
        integer, intent(out), optional :: frame  !! Receives the depth created, for a checked `pop`.
        integer :: n, need

        t_context_depth = t_context_depth + 1
        if (present(frame)) frame = t_context_depth
        if (t_context_depth > PF_LOG_MAX_CONTEXT_DEPTH) then
            call machinery_warning("the context stack is deeper than PF_LOG_MAX_CONTEXT_DEPTH; " // &
                "deeper frames are not rendered, but the depth stays exact", g_warned_context)
            return
        end if
        n = len_trim(text)
        need = t_context_len + n
        if (t_context_len > 0) need = need + 1
        if (need > PF_LOG_MAX_CONTEXT) then
            call machinery_warning("the context is longer than PF_LOG_MAX_CONTEXT characters; " // &
                "this frame's text is not rendered, but the depth stays exact", g_warned_context)
        else
            if (t_context_len > 0) then
                t_context(t_context_len + 1:t_context_len + 1) = " "
                t_context_len = t_context_len + 1
            end if
            if (n > 0) t_context(t_context_len + 1:t_context_len + n) = text(1:n)
            t_context_len = t_context_len + n
        end if
        t_context_ends(t_context_depth) = t_context_len
    end subroutine pf_log_push_context

    !> Pops the most recent context frame from the calling thread's stack.
    !!
    !! **On an empty stack this is a no-op, not an error** -- a defensive pop in cleanup code is a
    !! reasonable thing to write, and aborting a program from a logging call is worse than doing
    !! nothing. An unmatched push is the hazard that cannot be detected here, since stealing a
    !! caller's frame is indistinguishable from removing your own; pass the `frame` token
    !! `pf_log_push_context` reported to turn that into a loud abort at the site.
    subroutine pf_log_pop_context(frame)
        integer, intent(in), optional :: frame  !! The token `pf_log_push_context` reported.
        character(len=16) :: a, b

        if (present(frame)) then
            if (frame /= t_context_depth) then
                write (a, '(i0)') frame
                write (b, '(i0)') t_context_depth
                error stop "pf_log_pop_context: frame token " // trim(a) // " does not match the " // &
                    "current context depth " // trim(b) // "; a push and a pop are unbalanced"
            end if
        end if
        if (t_context_depth <= 0) return
        t_context_depth = t_context_depth - 1
        if (t_context_depth < PF_LOG_MAX_CONTEXT_DEPTH) then
            if (t_context_depth <= 0) then
                t_context_len = 0
            else
                t_context_len = t_context_ends(t_context_depth)
            end if
        end if
    end subroutine pf_log_pop_context

    !> Resets the calling thread's context stack to empty, leaving the shared base untouched.
    !!
    !! This is a **boundary** reset, not the counterpart of `pf_log_push_context`. It is what
    !! makes a loop body self-healing: whatever a callee did or failed to undo, the next iteration
    !! starts clean.
    subroutine pf_log_clear_context()

        t_context_depth = 0
        t_context_len = 0
        t_context = ""
    end subroutine pf_log_clear_context

    !> The calling thread's current context depth, for asserting that pushes and pops balance.
    integer function pf_log_context_depth() result(depth)

        depth = t_context_depth
    end function pf_log_context_depth

    ! ================================================================================
    ! Free-standing helpers
    ! ================================================================================

    !> Converts a level name to its numeric value, accepting `"info"`, `"INFO"` or `"20"`.
    !!
    !! With `ok` present an unrecognised name reports `.false.` rather than aborting, which is what
    !! parsing a configuration file or an environment variable needs; without it, an unrecognised
    !! name is a clean `error stop`.
    subroutine pf_log_level_from_name(name, level, ok)
        character(len=*), intent(in) :: name    !! The name or number to convert.
        integer, intent(out) :: level           !! Receives the level.
        logical, intent(out), optional :: ok    !! Receives whether the conversion succeeded.
        character(len=len(name)) :: low
        integer :: i, c, ios, n

        low = name
        do i = 1, len(low)
            c = iachar(low(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) low(i:i) = achar(c + 32)
        end do
        low = adjustl(low)
        n = len_trim(low)
        level = PF_LEVEL_INFO
        if (present(ok)) ok = .true.
        select case (low(1:n))
        case ("all");      level = PF_LEVEL_ALL
        case ("trace");    level = PF_LEVEL_TRACE
        case ("debug");    level = PF_LEVEL_DEBUG
        case ("info");     level = PF_LEVEL_INFO
        case ("warning");  level = PF_LEVEL_WARNING
        case ("warn");     level = PF_LEVEL_WARNING
        case ("error");    level = PF_LEVEL_ERROR
        case ("critical"); level = PF_LEVEL_CRITICAL
        case ("fatal");    level = PF_LEVEL_CRITICAL
        case ("off");      level = PF_LEVEL_OFF
        case ("none");     level = PF_LEVEL_OFF
        case default
            if (n > 0 .and. verify(low(1:n), "0123456789") == 0) then
                read (low(1:n), *, iostat = ios) level
                if (ios == 0) return
            end if
            if (present(ok)) then
                ok = .false.
                level = PF_LEVEL_INFO
                return
            end if
            error stop "pf_log_level_from_name: '" // trim(name) // "' is not a level; expected " // &
                "one of all/trace/debug/info/warning/error/critical/off, or a number"
        end select
    end subroutine pf_log_level_from_name

    !> Renders a level as its name, or as `Level <n>` for a level that is not one of the
    !> constants. `name` should be at least 12 characters to hold every form.
    subroutine pf_log_level_name(level, name)
        integer, intent(in) :: level          !! The level to render.
        character(len=*), intent(out) :: name !! Receives the name.
        integer :: n

        call level_text(level, name, n)
    end subroutine pf_log_level_name

    !> Wraps `text` in an ANSI colour code, for a caller who wants coloured message text.
    !!
    !! A subroutine rather than a function because this project forbids a function returning a
    !! deferred-length allocatable character (GCC PR113797), and this is exactly the kind of helper
    !! that gets called from inside a parallel region.
    subroutine pf_log_color(text, code, out)
        character(len=*), intent(in) :: text                !! The text to colour.
        character(len=*), intent(in) :: code                !! An ANSI code such as `PF_LOG_C_RED`.
        character(len=:), allocatable, intent(out) :: out   !! Receives the wrapped text.

        out = ESC // "[" // trim(code) // "m" // text // ANSI_RESET
    end subroutine pf_log_color

    !> Seconds elapsed since the process-wide monotonic clock origin, from `system_clock`.
    !!
    !! The origin is established by the first configuration call or by the first call to this
    !! procedure, never on the emission path. `system_clock` is monotonic, so unlike an elapsed
    !! time derived from `date_and_time` this is immune to a wall-clock adjustment mid-run.
    subroutine pf_log_elapsed(seconds)
        real(real64), intent(out) :: seconds  !! Receives the elapsed time, in seconds.
        integer(int64) :: c, rate

        call ensure_clock()
        call system_clock(count = c, count_rate = rate)
        if (rate > 0_int64) then
            seconds = real(c - g_clock0, real64) / real(rate, real64)
        else
            seconds = 0.0_real64
        end if
        if (seconds < 0.0_real64) seconds = 0.0_real64
    end subroutine pf_log_elapsed

    !> Applies every `PF_LOG_*` environment variable that is set, to the **default logger only**.
    !!
    !! Reads `<prefix>LEVEL`, `<prefix>FILE`, `<prefix>FORMAT` and `<prefix>COLOR`, where `prefix`
    !! is the whole literal prefix including its trailing separator -- so
    !! `pf_log_configure_from_env("MYAPP_LOG_")` reads `MYAPP_LOG_LEVEL`, and this module never
    !! inserts a separator of its own, which is what makes a doubled separator impossible.
    !!
    !! **Reading is strictly additive and never destructive**: an unset variable changes nothing,
    !! a set-but-empty one is ignored, and `<prefix>FILE` adds a file sink beside whatever is
    !! already attached rather than rebuilding the sink set. It is never applied implicitly --
    !! an environment variable silently changing a program's output is not something to do behind
    !! the caller's back.
    subroutine pf_log_configure_from_env(prefix)
        character(len=*), intent(in), optional :: prefix  !! Variable-name prefix; default `"PF_LOG_"`.
        character(len=64) :: pre
        character(len=PF_LOG_MAX_PATH) :: val
        integer :: ln, st, lev
        logical :: ok

        pre = "PF_LOG_"
        if (present(prefix)) then
            if (len_trim(prefix) == 0) error stop "pf_log_configure_from_env: prefix is empty"
            pre = prefix
        end if

        call get_environment_variable(trim(pre) // "LEVEL", val, ln, st)
        if (st == 0 .and. ln > 0) then
            call pf_log_level_from_name(val(1:ln), lev, ok)
            if (.not. ok) then
                error stop "pf_log_configure_from_env: " // trim(pre) // "LEVEL is not a level: '" // &
                    val(1:ln) // "'"
            end if
            call pf_log_set_level(lev)
        end if

        call get_environment_variable(trim(pre) // "COLOR", val, ln, st)
        if (st == 0 .and. ln > 0) then
            select case (val(1:ln))
            case ("never", "NEVER", "0", "no", "NO");     call pf_log_set_color(PF_LOG_COLOR_NEVER)
            case ("always", "ALWAYS", "1", "yes", "YES"); call pf_log_set_color(PF_LOG_COLOR_ALWAYS)
            case ("auto", "AUTO");                        call pf_log_set_color(PF_LOG_COLOR_AUTO)
            case default
                error stop "pf_log_configure_from_env: " // trim(pre) // "COLOR must be " // &
                    "auto, always or never"
            end select
        end if

        ! FILE is read BEFORE FORMAT, and the order is load-bearing: FORMAT with no sink argument
        ! sets the layout of every sink that exists WHEN IT RUNS, so reading it first would leave
        ! the file sink this call is about to add -- the one sink the caller actually asked for --
        ! carrying the default layout instead of the requested one.
        call get_environment_variable(trim(pre) // "FILE", val, ln, st)
        if (st == 0 .and. ln > 0) call pf_log_add_file(val(1:ln))

        call get_environment_variable(trim(pre) // "FORMAT", val, ln, st)
        if (st == 0 .and. ln > 0) call pf_log_set_format(val(1:ln))
    end subroutine pf_log_configure_from_env

    !> Renders one `integer(int32)` for concatenation into a message. See `pf_str`.
    function pf_str_i32(value, fmt) result(res)
        integer(int32), intent(in) :: value            !! The value to render.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor, e.g. `'(i8)'`.
        character(len=PF_LOG_STR_LEN) :: res           !! The rendered text, blank padded.

        call one_i32(value, fmt, res)
        res = adjustl(res)
    end function pf_str_i32

    !> Renders one `integer(int64)` for concatenation into a message. See `pf_str`.
    function pf_str_i64(value, fmt) result(res)
        integer(int64), intent(in) :: value            !! The value to render.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor.
        character(len=PF_LOG_STR_LEN) :: res           !! The rendered text, blank padded.

        call one_i64(value, fmt, res)
        res = adjustl(res)
    end function pf_str_i64

    !> Renders one `real(real32)` for concatenation into a message. See `pf_str`.
    function pf_str_r32(value, fmt) result(res)
        real(real32), intent(in) :: value              !! The value to render.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor.
        character(len=PF_LOG_STR_LEN) :: res           !! The rendered text, blank padded.

        call one_r32(value, fmt, res)
        res = adjustl(res)
    end function pf_str_r32

    !> Renders one `real(real64)` for concatenation into a message. See `pf_str`.
    function pf_str_r64(value, fmt) result(res)
        real(real64), intent(in) :: value              !! The value to render.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor.
        character(len=PF_LOG_STR_LEN) :: res           !! The rendered text, blank padded.

        call one_r64(value, fmt, res)
        res = adjustl(res)
    end function pf_str_r64

    !> Renders one `logical` for concatenation into a message. See `pf_str`.
    function pf_str_log(value, fmt) result(res)
        logical, intent(in) :: value                   !! The value to render.
        character(len=*), intent(in), optional :: fmt  !! Format descriptor.
        character(len=PF_LOG_STR_LEN) :: res           !! The rendered text, blank padded.

        call one_log(value, fmt, res)
        res = adjustl(res)
    end function pf_str_log

end module parquet_logging ! GCOVR_EXCL_LINE
