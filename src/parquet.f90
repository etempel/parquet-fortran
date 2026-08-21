!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> The public API of parquet-fortran, and the only module a user needs to name:
!> a single `use parquet` brings the whole library into scope.
!>
!> This module is a facade. It re-exports, unchanged, every public entity of the
!> modules that actually implement the library:
!>
!>   * `parquet_io`      -- readers, writers, schemas, MAML parsing/validation,
!>                          filters, sort keys, read-time qc, file metadata.
!>                          Itself a facade over the internal `parquet_core`.
!>   * `parquet_tables`  -- the `parquet_table` container: lazy columnar reads,
!>                          slices, row handles, mutation, table write-out.
!>   * `parquet_columns` -- the `parquet_column` foundation and the `PK_*` kind
!>                          constants.
!>   * `parquet_strings` -- `parquet_string_column`/`parquet_string`.
!>   * `parquet_temporal`-- `parquet_date`/`parquet_time`/`parquet_timestamp`
!>                          and the `parquet_unit_*`/`parquet_ns_*` constants.
!>   * `parquet_sorting` -- `pf_sort`/`pf_argsort`/`pf_permute`/`pf_is_sorted`
!>                          and the `pf_sort_keys` multi-key builder.
!>   * `parquet_random` -- counter-based random numbers: `pf_random_at` and
!>                          friends, reproducible under any OpenMP schedule.
!>   * `parquet_settings`-- process-global settings: thread caps, writer
!>                          defaults, terminal verbosity and message stream,
!>                          plus the read-only `parquet_max_*` limits.
!>   * `parquet_maml_base` -- `parquet_maml_file`, for embedded MAML schemas.
!>
!> Those modules remain individually usable (`use parquet_temporal` still works
!> and still costs less to compile against), but nothing requires it -- every
!> name they export is reachable through this one. The exception is
!> `parquet_bindings`, the raw C interop layer, which is deliberately NOT
!> re-exported: it is an implementation detail, not user API.
!>
!> The version reporting below lives here rather than in `parquet_core` so that
!> the facade owns the one piece of the API that is about the library itself.
module parquet
    use iso_c_binding, only: c_int
    use parquet_io
    use parquet_tables
    use parquet_columns
    use parquet_strings
    use parquet_temporal
    use parquet_sorting
    use parquet_random
    use parquet_sampling
    ! Only the test hook: the transform itself and its contract check are internal.
    use parquet_expkey, only: parquet_debug_exp_key, parquet_debug_set_exp_key_contract
    use parquet_settings
    ! parquet_maml_base is the one sibling imported with an `only:` list rather than in full. Its
    ! other public names (get_parquet_maml and the parquet_maml_maml_example* accessors) return
    ! THIS library's own embedded MAML test fixtures -- they were never part of the public surface,
    ! and a downstream project gets its own generated parquet_maml module from
    ! tools/generate_parquet_maml.sh instead. Only the three types below are user API.
    use parquet_maml_base, only: parquet_maml_file, parquet_maml_missing_column, parquet_maml_col_map_entry
    use parquet_bindings, only: parquet_get_arrow_version, parquet_get_parquet_version
    implicit none
    !
    ! Default accessibility is deliberately PUBLIC here, unlike every other module in this
    ! library: a bare `use <sibling>` with no `only:` list re-exports that module's whole
    ! public surface, which is exactly what a facade wants and what avoids maintaining a
    ! ~120-name `public ::` list that would go stale on every addition. The private
    ! statements below are what keep this module's own implementation details -- and the C
    ! interop layer -- out of the namespace a user gets from `use parquet`.
    private :: c_int
    private :: parquet_get_arrow_version, parquet_get_parquet_version
    private :: cversion
    ! `parquet_split_name_list` and `parquet_parse_sort_key` need NO statement here: parquet_core
    ! makes both public so the sibling parquet_tables module can share the library's one name-list
    ! tokenizer and its one sort-key direction grammar rather than keeping second copies, and
    ! `parquet_io` -- the facade this one now re-exports in place of parquet_core -- already hides
    ! them. Adding a `private ::` for either here is an ERROR, not a redundancy: the name is not
    ! accessible in this scope at all, and nagfor reports it as an implicitly-typed local.
    ! parquet_settings has to make these two public so the write path (a submodule of parquet_core,
    ! a different module) can reach them -- Fortran has no package scope. They are plumbing, not
    ! API, so the facade keeps them out of the namespace `use parquet` hands a user, exactly as it
    ! does for c_int and the version bindings above.
    private :: parquet_valid_compressions, parquet_resolve_writer_compression
    ! The three output channels and the suppression query are the same case: every module that
    ! emits has to reach them, so parquet_settings makes them public, and the facade hides them.
    private :: parquet_emit_info, parquet_emit_warning, parquet_emit_error_context
    private :: parquet_output_is_suppressed
    ! parquet_columns' typed per-cell accessor tier, hidden for the same reason again. These are
    ! how parquet_tables reaches a column's storage without a type-bound call -- which is what
    ! keeps ifx from building a runtime type descriptor in the caller's prologue on every access
    ! (feature_ifx.md). They duplicate no user-facing capability: the identical operations are
    ! already on parquet_column as %get_at/%set_at/%get_elem/%set_elem/%is_null/%set_null/
    ! %clear_null/%data_ptr/%string_column, which is what a user calls. Adding a typed accessor
    ! means adding a line here too.
    private :: parquet_column_get_at, parquet_column_set_at
    private :: parquet_column_get_elem, parquet_column_set_elem
    private :: parquet_column_data_ptr, parquet_column_string_column
    private :: parquet_column_is_null, parquet_column_set_null, parquet_column_clear_null
    !
    ! ---- The sorting tiers' internals need NO `private ::` here, and that is worth stating ----
    !
    ! `parquet_argsort` exports its engine, `sort_key_buf`, the six intrinsic extractors and the
    ! oracle plumbing so that `parquet_sorting` and `parquet_sorting_oracle` can share one copy of
    ! each rather than keeping second copies that could disagree. None of it reaches this facade:
    ! `parquet_sorting` imports the tier with a bare `use` under its own default `private`
    ! accessibility and names only the `pf_` surface, the settings and the debug hooks in its
    ! `public ::` lines, so everything else stops there. `parquet_sorting_oracle` is not imported
    ! here at all -- `parquet_debug_use_fortran_sort_engine` is reachable only by a program that
    ! names that module itself, which is what keeps the C++ engine out of every other build.
    !
    character(len=*),parameter:: cversion = "v1.5.0 (2026-08-08)" !! version info
#ifndef RELEASE_VERSION
#  define RELEASE_VERSION 0.1
#endif

contains

    !> Returns a version string. Default (mode absent): the RELEASE_VERSION build
    !> macro's bare release number; emits an informational remark first if that
    !> disagrees with cversion (a hand-maintained "vX.Y.Z (date)" string), which
    !> signals a build that skipped fpm's macro substitution or a version bump
    !> missed on one side. mode="internal" instead returns cversion verbatim.
    !> mode="arrow"/mode="parquet" return the actually-linked Arrow library's
    !> runtime version, respectively the compile-time Parquet C++ library
    !> version, each formatted "major.minor.patch". Any other mode value is an
    !> error.
    subroutine parquet_get_version(ver_string, mode)
        implicit none
        character(len=:), allocatable, intent(out) :: ver_string !! resulting version string.
        character(len=*), intent(in), optional :: mode
        !! "internal" | "arrow" | "parquet"; absent = default RELEASE_VERSION behavior.
        integer :: i
        integer(c_int) :: major, minor, patch
        character(len=32) :: buf
        !
! Accept solution from https://stackoverflow.com/questions/31649691/stringify-macro-with-gnu-gfortran
! which provides the easiest way to pass a macro to a string in Fortran complying with both
! gfortran traditional cpp and the standard cpp syntaxes
#ifdef NAGFOR
! NAG drives -fpp, a Fortran preprocessor implementing NEITHER the standard cpp `#`
! stringification operator NOR gfortran's traditional-cpp continuation trick, so no spelling of
! the macro pair below compiles here (confirmed against NAG 7.2: the first gives
! "Invalid character '#'", the second "Unrecognised statement"). Take the release number from
! cversion instead. The consequence is deliberate rather than a gap: the development-build remark
! below compares ver_string against that same substring, so it can never fire under NAG -- the
! question it asks, "did fpm substitute RELEASE_VERSION?", is one this preprocessor cannot answer.
        ver_string = cversion(2:index(cversion, " ") - 1)
#else
#  ifdef __GFORTRAN__
#    define STRINGIFY_START(X) "&
#    define STRINGIFY_END(X) &X"
#  else
#    define STRINGIFY_(X) #X
#    define STRINGIFY_START(X) &
#    define STRINGIFY_END(X) STRINGIFY_(X)
#  endif

        ver_string = STRINGIFY_START(RELEASE_VERSION)
        STRINGIFY_END(RELEASE_VERSION)
#endif
        !
        i = index(cversion, " ")
        !
        if (cversion(2:i-1) /= ver_string) then ! GCOVR_EXCL_START -- gcov attribution artifact
            ! A remark rather than a warning: it says something about how this copy of the library
            ! was BUILT, not about the caller's data, and it fires on every call in a build whose
            ! macro substitution did not happen. So it goes through the informational channel, which
            ! `verbosity="silent"` quiets while leaving real warnings alone.
            call parquet_emit_info("note: this is a development build of parquet-fortran " // &
                "(library version " // trim(cversion) // ", RELEASE_VERSION " // trim(ver_string) // ")")
        end if ! GCOVR_EXCL_STOP
        !
        if (present(mode)) then
            select case (mode)
            case ("internal")
                ver_string = trim(cversion)
            case ("arrow")
                call parquet_get_arrow_version(major, minor, patch)
                write(buf, '(i0,".",i0,".",i0)') major, minor, patch
                ver_string = trim(buf)
            case ("parquet")
                call parquet_get_parquet_version(major, minor, patch)
                write(buf, '(i0,".",i0,".",i0)') major, minor, patch
                ver_string = trim(buf)
            case default
                error stop "parquet_get_version: invalid mode '" // mode // &
                    "' (must be 'internal', 'arrow', or 'parquet')"
            end select
        else
            ver_string = trim(ver_string)
        end if
        !
    end subroutine parquet_get_version

end module parquet
