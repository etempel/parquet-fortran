!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> The version of parquet-fortran itself, and nothing else.
!>
!> This module is a LEAF: it imports `parquet_settings_base` for the library's
!> informational output channel and nothing further, so it reaches neither
!> `parquet_bindings` nor the Arrow/Parquet C++ stack. That is what lets a
!> program built on one of the Arrow-free tiers -- `parquet_random`,
!> `parquet_argsort`, `parquet_sampling`, `parquet_columns`, `parquet_strings`,
!> `parquet_temporal`, `parquet_sorting` -- report which parquet-fortran it was
!> built against, with `use parquet_version` and no other cost.
!>
!> **Only `use parquet` re-exports this procedure.** Every other entry module
!> leaves it here deliberately, so that none of them grows a dependency for a
!> string that is fixed at compile time; a program importing one of those writes
!> a second `use parquet_version` line. This is a deliberate exception to the
!> rule that each module re-exports the settings its own code reads -- a version
!> string is not a setting, and nothing in this library reads it.
!>
!> It does re-export the two OUTPUT settings, `verbosity` and `message_stream`,
!> because this module can print: `parquet_get_version` emits a remark on a
!> development build whose `RELEASE_VERSION` substitution did not happen, and a
!> program importing nothing else has to be able to quiet or redirect it. That
!> is the ordinary rule applying, not a second exception to it.
!>
!> For the version of the Arrow and Parquet C++ libraries actually linked, call
!> `parquet_get_arrow_version` instead (`parquet_settings`, and so also
!> `parquet_io` and `parquet`). That query needs the C++ boundary, which is
!> exactly what this module is defined not to have.
module parquet_version
    use parquet_settings_base, only: parquet_emit_info, &
        parquet_set_verbosity, parquet_get_verbosity, &
        parquet_set_message_stream, parquet_get_message_stream
    implicit none
    private

    public :: parquet_get_version
    !
    ! The output pair, re-exported for the same reason every other emitting tier re-exports it:
    ! this module CAN print -- parquet_get_version emits the development-build remark below -- and
    ! a program whose only import is `use parquet_version` must still be able to silence it or
    ! send it elsewhere. Reaching parquet_settings for that would put the C++ boundary back into
    ! an otherwise two-file, Arrow-free import, which is the whole thing this module exists to
    ! avoid. Costs nothing: parquet_settings_base is already imported and already in the
    ! footprint. See doc/pages/operating/choosing-a-module.md's settings table, which lists it.
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream

    !> The hand-maintained release string, "vX.Y.Z (date)". Kept in step with VERSION.txt by the
    !! release checklist in CONTRIBUTING.md; parquet_get_version compares the two and remarks when
    !! they disagree, which is how a build that skipped fpm's macro substitution announces itself.
    character(len=*), parameter :: cversion = "v2.2.0 (2026-09-03)" !! version info
#ifndef RELEASE_VERSION
#  define RELEASE_VERSION 0.1
#endif

contains

    !> Returns this library's own version string. Default (mode absent): the
    !> RELEASE_VERSION build macro's bare release number; emits an informational
    !> remark first if that disagrees with cversion (a hand-maintained
    !> "vX.Y.Z (date)" string), which signals a build that skipped fpm's macro
    !> substitution or a version bump missed on one side. mode="internal"
    !> instead returns cversion verbatim. Any other mode value is an error --
    !> in particular the linked Arrow and Parquet C++ library versions are
    !> reported by parquet_get_arrow_version, not by this procedure.
    subroutine parquet_get_version(ver_string, mode)
        implicit none
        character(len=:), allocatable, intent(out) :: ver_string !! resulting version string.
        character(len=*), intent(in), optional :: mode
        !! "internal"; absent = default RELEASE_VERSION behavior.
        integer :: i
        character(len=:), allocatable :: preview
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
            case default
                ! Only a short preview of the offending value goes into the message, never the whole
                ! (unboundedly long) text: ifx 2026.1.1's ERROR STOP runtime corrupts memory once the
                ! composed message reaches 8192 bytes, and `mode` is caller-supplied with no cap.
                if (len(mode) > 100) then
                    preview = mode(1:100) // "..."
                else
                    preview = mode
                end if
                error stop "parquet_get_version: invalid mode '" // preview // &
                    "' (must be 'internal'; for the linked Arrow/Parquet C++ library versions " // &
                    "call parquet_get_arrow_version)"
            end select
        else
            ver_string = trim(ver_string)
        end if
        !
    end subroutine parquet_get_version

end module parquet_version
