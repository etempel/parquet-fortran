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

    !> Restores every setting to the value it had before this program changed it.
    !>
    !> For the Arrow thread pool that means the capacity captured on the first
    !> parquet_set_max_threads call; if it was never called, this is a no-op rather than a resize to
    !> an invented default, since Arrow's own initial capacity is hardware-dependent and is not a
    !> number this library gets to choose.
    subroutine parquet_reset_settings()

        if (cfg_arrow_threads_initial >= 1) then
            call parquet_set_thread_pool_capacity(int(cfg_arrow_threads_initial, kind=c_int))
            cfg_arrow_threads_initial = -1
        end if
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

        u = output_unit
        if (present(unit)) u = unit
        write (u, '(a)') "parquet-fortran settings"
        call print_one(u, "arrow_threads", parquet_get_max_threads())
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

end module parquet_settings
