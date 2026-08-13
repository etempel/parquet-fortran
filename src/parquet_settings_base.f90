!> The settings state that a module with **no C++ dependency** is allowed to read.
!!
!! **This module exists for exactly one reason: to keep `parquet_strings` linkable on its own.**
!! `parquet_strings` is documented as usable without the Arrow/Parquet C++ stack -- a project that
!! wants compact string storage and nothing else can depend on it alone. It needs two answers from
!! the library's settings, though: whether solicited output is suppressed (`verbosity`), and the
!! per-column thread cap. Taking those from `parquet_settings` directly cost it that independence,
!! because `parquet_settings` imports `parquet_bindings` in order to mirror the C++-side knobs -- so
!! linking a program whose only import was `use parquet_strings` pulled in the whole of
!! `parquet_wrapper.cpp` and, with it, Arrow. Nothing failed at compile time; the link failed with
!! the entire C++ surface undefined, which reads as a build misconfiguration rather than a
!! dependency defect.
!!
!! So the STATE lives here, in a leaf module that imports nothing but `iso_fortran_env`, and the
!! public API over it stays in `parquet_settings`, which re-exports the two readers below.
!!
!! **This is a split, not a mirror, and the difference is the whole point.** There is one copy of
!! each value: `parquet_settings`' setters write these variables directly. A pushed second copy in
!! `parquet_strings` would have been the other way to break the dependency and was rejected -- two
!! writers for one value is exactly what `CLAUDE.md`'s "Do not add a second way to set the same
!! thing" forbids, and a missed push site would leave the two disagreeing with nothing to report it.
!!
!! **Rules for anything added here.** A variable belongs in this module only if a
!! no-C++-dependency module has to read it; everything else stays in `parquet_settings`, which is
!! where a reader looks for the settings API. Whatever is added must keep this module a leaf -- it
!! may import intrinsic modules and nothing else, ever. `check_parquet_strings_stays_leaf`
!! (`tools/check_source_conventions.py`) enforces both halves by walking the `use` graph, because
!! the failure it guards is invisible until someone tries the standalone build.
module parquet_settings_base
    implicit none
    private
    !
    public :: parquet_output_is_suppressed
    public :: parquet_get_string_threads
    public :: verb_normal, verb_silent, verb_errors_only
    public :: cfg_verbosity, cfg_string_threads
    !
    !> Verbosity levels, ordered so that a `>=` test answers "is this class of output off?".
    integer, parameter :: verb_normal = 0      !! everything prints (the factory default).
    integer, parameter :: verb_silent = 1      !! informational and solicited output goes quiet.
    integer, parameter :: verb_errors_only = 2 !! warnings go quiet too; only errors survive.
    !
    !> How much the library prints. Written by `parquet_set_verbosity`/`parquet_reset_settings`
    !! (parquet_settings.f90); read by the three emit channels there and by
    !! parquet_output_is_suppressed below, which is what the solicited printers ask.
    integer, save :: cfg_verbosity = verb_normal
    !> Cap on the threads one `parquet_string_column` bulk operation may use internally. `0` means
    !! "auto" (as many as OpenMP offers). Written by `parquet_set_string_threads`
    !! (parquet_settings.f90); read only by `parquet_string_threads` (src/parquet_strings.f90),
    !! deliberately, for the same reason cfg_sort_threads has a single reader: one question asked in
    !! one place cannot give two answers (feature_risks.md Risk-40).
    integer, save :: cfg_string_threads = 0
    !
contains
    !
    !> Whether SOLICITED output -- something the caller explicitly asked to be printed, such as
    !! `%print_stat` or `parquet_string_column%print` -- should stay quiet. Distinct from the emit
    !! channels, which govern the library's own unsolicited messages.
    logical function parquet_output_is_suppressed() result(quiet)
        quiet = cfg_verbosity >= verb_silent
    end function parquet_output_is_suppressed
    !
    !> The configured cap on threads inside one string-column bulk operation; `0` means automatic.
    integer function parquet_get_string_threads() result(n)
        n = cfg_string_threads
    end function parquet_get_string_threads
    !
end module parquet_settings_base ! GCOVR_EXCL_LINE
