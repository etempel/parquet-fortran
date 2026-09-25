!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Tests for `parquet_cosmology_config`: the `[cosmology]` section of a TOML configuration file.
!!
!! **Every fixture carries other sections**, because that is what a run's configuration looks
!! like: each one opens with a `[general]` table and closes with at least one `[[region]]` entry,
!! so a reader or writer that reached outside `[cosmology]` fails a test rather than passing one.
!!
!! The four committed fixtures under `test/fixtures/` are READ-ONLY and several tests load the
!! same path at once, which is what `test/fixtures/toml_shared.toml` already does. Everything a
!! test needs to vary is a string literal instead (`pf_toml_loads`), which needs no unique name and
!! no cleanup; the exceptions are the round trips, which must go through a file to be a round trip
!! at all, and each of those names a file of its own under `test_run/`.
!!
!! **Everything fatal is an error scenario, not a test here** -- a missing section with no `found=`,
!! a named cosmology given parameters, a wrong-typed value, an `m_nu` of the wrong length and a
!! parameter outside its range all abort the process, so they live in `test/error_scenarios.f90`.
!! What this file asserts is the other half: that the same call does NOT abort when it should not.
!!
!! Two suites, and the split matters. `cosmology_config` runs concurrently.
!! `cosmology_config_serial` is excluded from test-drive's parallelism (see
!! `test/test_runner_support.f90`) because its tests reconfigure the process-global DEFAULT logger
!! to read the unknown-key sweep back, exactly as `toml_serial` does.
!!
!! **The two list agreements are lint checks, not tests here.** That the section's key table is
!! `%init`'s argument list, and that the eight named cosmologies this module copies are
!! `parquet_cosmology`'s own, are properties of two SOURCE files rather than of a run --
!! `check_cosmology_config_keys_match_init` and `check_cosmology_config_names_match_the_library`
!! in `tools/check_source_conventions.py`, beside every other cross-file name agreement here.
module test_cosmology_config
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use iso_fortran_env, only: int32, int64, real64
    use ieee_arithmetic, only: ieee_is_finite
    use parquet_cosmology, only: pf_cosmology
    use parquet_toml
    use parquet_cosmology_config, only: pf_cosmology_from_toml, pf_cosmology_to_toml
    use parquet_logging, only: pf_log_init, pf_log_add_file, pf_log_close, pf_log_flush, &
                               PF_LEVEL_INFO
    use test_cosmology_vectors
    implicit none
    private

    public :: collect_tests_cosmology_config, collect_tests_cosmology_config_serial

    !> The committed fixtures. Read-only, and loaded by several tests at once.
    character(len=*), parameter :: FIX_NAMED = "test/fixtures/cosmology_named.toml"
    character(len=*), parameter :: FIX_PARAMS = "test/fixtures/cosmology_params.toml"
    character(len=*), parameter :: FIX_MINIMAL = "test/fixtures/cosmology_minimal.toml"
    character(len=*), parameter :: FIX_MISSPELT = "test/fixtures/cosmology_misspelt.toml"

    !> The round trip's tolerance on the ANSWERS, not on the bits.
    !!
    !! `1e-15` would pass today and fail the first time someone puts a small parameter in a file:
    !! toml-f writes `f24.16` below `1e3`, which is sixteen digits AFTER the decimal point, so a
    !! parameter of magnitude `1e-2` -- `ob0 = 0.04897` is one -- can come back `2.7e-15` away
    !! before any cosmology is built. Measured over the 22-model grid the worst gap on
    !! `%comoving_distance`, `%age` and `%efunc` is well inside this.
    real(real64), parameter :: ROUND_TRIP_TOL = 1.0e-14_real64

contains

    !> Registers the concurrent suite.
    subroutine collect_tests_cosmology_config(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("a named cosmology in a file is the named cosmology", &
                test_named_section_is_the_named_cosmology), &
            new_unittest("a parameter section builds the object init would", &
                test_parameter_section_matches_init), &
            new_unittest("a cosmology written and read back answers the same numbers", &
                test_round_trip_answers_agree), &
            new_unittest("a flat model stays flat across the round trip", &
                test_flat_stays_flat), &
            new_unittest("a model with no ob0 round-trips, and one with ob0 keeps it", &
                test_ob0_round_trips_both_ways), &
            new_unittest("the defaults are the defaults init applies", &
                test_defaults_are_inits_defaults), &
            new_unittest("no cosmology section sets found and does not abort", &
                test_absent_section_sets_found), &
            new_unittest("writing into a document that already has the section replaces it", &
                test_writing_over_an_existing_section), &
            new_unittest("the rest of the configuration file is untouched", &
                test_the_rest_of_the_document_survives), &
            new_unittest("a named cosmology is matched without regard to case or blanks", &
                test_named_match_ignores_case_and_blanks), &
            new_unittest("a section under another section is read through its handle", &
                test_nested_section_is_reachable), &
            new_unittest("section= puts a second cosmology in a table of its own", &
                test_section_argument_names_the_table), &
            new_unittest("a context longer than the cap is carried capped and elided", &
                test_a_long_context_is_capped), &
            new_unittest("what a real64 looks like in the file, and where it stops meaning anything", &
                test_guide_toml_number_text) &
            ]

    end subroutine collect_tests_cosmology_config

    !> Registers the serial suite: these reconfigure the process-global default logger.
    subroutine collect_tests_cosmology_config_serial(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("a misspelt key is reported by check_all", &
                test_misspelt_key_is_reported), &
            new_unittest("an unknown key is reported even when every known key is present", &
                test_unknown_key_reported_beside_a_full_section) &
            ]

    end subroutine collect_tests_cosmology_config_serial

    ! ================================================================================
    ! 1. The named form
    ! ================================================================================

    !> A `[cosmology]` naming one of the eight is that cosmology, to the bit.
    !!
    !! The fixture spells it `planck18`, so a case-sensitive match fails here, and the parameter
    !! form cannot answer at all (the section sets no `h0`).
    subroutine test_named_section_is_the_named_cosmology(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        type(pf_cosmology) :: got, want
        character(len=:), allocatable :: name, what

        call pf_toml_load(conf, FIX_NAMED)
        call pf_cosmology_from_toml(conf, got)
        call pf_toml_close(conf)

        call want%init("Planck18")
        call same_model(got, want, what)
        call check(error, what == "", "the named section must build Planck18 exactly: " // what)
        if (allocated(error)) return

        call got%get_name(name)
        call check(error, name == "Planck18", &
            "%get_name must answer the CANONICAL spelling, not the file's: " // name)

    end subroutine test_named_section_is_the_named_cosmology

    !> A name is matched the way `%init` matches it: blanks ignored, case ignored.
    subroutine test_named_match_ignores_case_and_blanks(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        type(pf_cosmology) :: got, want
        character(len=:), allocatable :: what

        call pf_toml_loads(conf, '[general]' // new_line("a") // 'nproc = 1' // new_line("a") // &
                                 '[cosmology]' // new_line("a") // &
                                 'name = "  WmAp9  "' // new_line("a") // &
                                 '[[region]]' // new_line("a") // 'id = 1' // new_line("a"))
        call pf_cosmology_from_toml(conf, got)
        call pf_toml_close(conf)

        call want%init("WMAP9")
        call same_model(got, want, what)
        call check(error, what == "", "a padded, mixed-case name must select WMAP9: " // what)

    end subroutine test_named_match_ignores_case_and_blanks

    ! ================================================================================
    ! 2. The parameter form
    ! ================================================================================

    !> Every key reaches the argument it names.
    !!
    !! This is the one mistake no round trip can see, because a key written and read through the
    !! same wrong argument is symmetric: the reference is `%init` written out here, by hand.
    subroutine test_parameter_section_matches_init(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        type(pf_cosmology) :: got, want
        character(len=:), allocatable :: name, what

        call pf_toml_load(conf, FIX_PARAMS)
        call pf_cosmology_from_toml(conf, got)
        call pf_toml_close(conf)

        call want%init(h0 = 67.66_real64, om0 = 0.30966_real64, tcmb0 = 2.7255_real64, &
                       neff = 3.046_real64, &
                       m_nu = [0.0_real64, 0.0_real64, 0.06_real64], ob0 = 0.04897_real64, &
                       w0 = -0.95_real64, wa = 0.1_real64, name = "my_sim", &
                       zmax = 200.0_real64, zmin = -0.5_real64)
        call same_model(got, want, what)
        call check(error, what == "", "every key must reach the argument it names: " // what)
        if (allocated(error)) return

        call got%get_name(name)
        call check(error, name == "my_sim", "a name that is not one of the eight is a LABEL: " // name)
        if (allocated(error)) return
        call check(error, got%is_flat(), "ode0 absent must leave the model flat")

    end subroutine test_parameter_section_matches_init

    !> A section setting only `h0` and `om0` takes `%init`'s own defaults, all of them.
    !!
    !! The one that matters most is `neff`: `%init` defaults to `3.04`, while every Planck
    !! realization carries `3.046`, so a `default =` written from memory here would be silently
    !! wrong for every bare parameter file.
    subroutine test_defaults_are_inits_defaults(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        type(pf_cosmology) :: got, want
        real(real64) :: ob0
        character(len=:), allocatable :: what

        call pf_toml_load(conf, FIX_MINIMAL)
        call pf_cosmology_from_toml(conf, got)
        call pf_toml_close(conf)

        call want%init(h0 = 70.0_real64, om0 = 0.3_real64)
        call same_model(got, want, what)
        call check(error, what == "", "a minimal section must take every default init applies: " // what)
        if (allocated(error)) return

        ! Spelled out as well, so that a drift in BOTH objects at once is still caught.
        call check(error, got%neff() == 3.04_real64, "neff must default to 3.04, not 3.046")
        if (allocated(error)) return
        call check(error, got%tcmb0() == 0.0_real64, "tcmb0 must default to zero")
        if (allocated(error)) return
        call check(error, got%w0() == -1.0_real64 .and. got%wa() == 0.0_real64, &
            "w0 and wa must default to a cosmological constant")
        if (allocated(error)) return
        call check(error, got%zmax() == 1100.0_real64 .and. got%zmin() == -0.9_real64, &
            "zmax and zmin must take init's own defaults")
        if (allocated(error)) return
        call check(error, got%is_flat(), "ode0 absent must mean flat")
        if (allocated(error)) return
        ob0 = got%ob0()
        call check(error, ob0 /= ob0, "ob0 absent must leave %ob0() NaN")

    end subroutine test_defaults_are_inits_defaults

    ! ================================================================================
    ! 3. The round trip
    ! ================================================================================

    !> Every model of the reference grid survives a write, a dump, a load and a read.
    !> What a `real64` looks like in the file, and the one magnitude at which it stops meaning
    !! anything.
    !!
    !! The text is toml-f's, not this library's, and `doc/pages/utilities/configuration-files.md`
    !! states it because the limit is INVISIBLE: seventeen significant digits above `1e3` and
    !! sixteen AFTER the decimal point below it, so `h0 = 67.66` round-trips bit for bit while a
    !! value under about `5e-17` is written `0.0000000000000000` and read back as exactly zero with
    !! no diagnostic anywhere. A toml-f bump could move either half and nothing else here would
    !! fail: the round-trip tests all compare ANSWERS at `ROUND_TRIP_TOL`, which such a change
    !! would still meet.
    !!
    !! The CONTROL is the second assertion. Were the text lossless, `wa = 1e-18` would come back as
    !! `1e-18` and the "exactly zero" check would fail -- so the test cannot pass by the file
    !! simply carrying everything.
    subroutine test_guide_toml_number_text(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: doc, conf
        type(pf_cosmology) :: c, back
        character(len=*), parameter :: path = "test_run/cosmology_config_number_text.toml"
        character(len=400) :: line
        integer :: u, ios
        logical :: seen_h0

        call c%init(h0 = 67.66_real64, om0 = 0.30966_real64, wa = 1.0e-18_real64, name = "numbers")
        call pf_toml_new(doc, path)
        call pf_cosmology_to_toml(c, doc)
        call pf_toml_dump(doc, path)
        call pf_toml_close(doc)

        seen_h0 = .false.
        open (newunit = u, file = path, status = "old", action = "read")
        do
            read (u, '(a)', iostat = ios) line
            if (ios /= 0) exit
            if (index(line, "h0 = 67.6599999999999966") > 0) seen_h0 = .true.
        end do
        close (u)
        call check(error, seen_h0, "toml-f must write an h0 of 67.66 as 67.6599999999999966: " // &
            "sixteen digits after the decimal point, which is what makes it round-trip")
        if (allocated(error)) return

        call pf_toml_load(conf, path)
        call pf_cosmology_from_toml(conf, back)
        call pf_toml_close(conf)
        call check(error, back%h0() == c%h0(), "h0 must come back bit for bit")
        if (allocated(error)) return
        call check(error, back%om0() == c%om0(), "om0 must come back bit for bit")
        if (allocated(error)) return
        call check(error, back%wa() == 0.0_real64, &
            "a wa of 1e-18 must come back as exactly zero: the guide's warning that a value " // &
            "below about 5e-17 is written 0.0000000000000000 with no diagnostic")

    end subroutine test_guide_toml_number_text

    subroutine test_round_trip_answers_agree(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: doc, conf
        type(pf_cosmology) :: c, back
        character(len=:), allocatable :: path
        real(real64) :: z
        integer :: i, j, compared
        logical :: ok

        compared = 0
        do i = 1, n_cmodel
            call build(c, i)
            path = "test_run/cosmology_config_roundtrip_" // trim(cmodel_label(i)) // ".toml"

            call pf_toml_new(doc, path)
            call pf_cosmology_to_toml(c, doc)
            call pf_toml_dump(doc, path)
            call pf_toml_close(doc)

            call pf_toml_load(conf, path)
            call pf_cosmology_from_toml(conf, back)
            call pf_toml_close(conf)

            do j = 1, n_cz
                z = rv(cz_bits(j))
                ok = agrees(back%comoving_distance(z), c%comoving_distance(z), ROUND_TRIP_TOL)
                if (ok) ok = agrees(back%age(z), c%age(z), ROUND_TRIP_TOL)
                if (ok) ok = agrees(back%efunc(z), c%efunc(z), ROUND_TRIP_TOL)
                if (ieee_is_finite(c%efunc(z))) compared = compared + 1
                call check(error, ok, "model " // trim(cmodel_label(i)) // &
                    " must answer the same after a round trip through the file")
                if (allocated(error)) return
            end do
        end do

        ! The vacuity guard: the loop above is silent when every answer is a NaN on both sides.
        call check(error, compared > 300, "the round trip compared too few live redshifts to mean " // &
            "anything -- the grid or the domains moved")

    end subroutine test_round_trip_answers_agree

    !> A flat model comes back flat, which is an EXACT bit test rather than a tolerance.
    !!
    !! `%ode0()` answers the derived value for a flat model too, so a writer that does not test
    !! `%is_flat()` produces a file that rebuilds a model flat only to rounding -- and every
    !! tolerance-based assertion in this file would pass on it.
    subroutine test_flat_stays_flat(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_cosmology) :: c, back
        character(len=*), parameter :: path = "test_run/cosmology_config_flat.toml"
        integer :: i, seen

        seen = 0
        do i = 1, n_cmodel
            if (.not. cmodel_flat(i)) cycle
            call build(c, i)
            call round_trip(c, back, path)
            seen = seen + 1
            call check(error, back%is_flat(), "flat model " // trim(cmodel_label(i)) // &
                " must come back with Ok0 EXACTLY zero, so ode0 was omitted rather than written")
            if (allocated(error)) return
            call check(error, back%ok0() == 0.0_real64, "%ok0() must be exactly zero")
            if (allocated(error)) return
        end do
        call check(error, seen > 0, "no flat model was exercised -- the grid moved")

    end subroutine test_flat_stays_flat

    !> `%ob0()` is NaN when the model has none, and `nan` is not a number a file can carry back.
    subroutine test_ob0_round_trips_both_ways(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_cosmology) :: c, back
        character(len=*), parameter :: path = "test_run/cosmology_config_ob0.toml"
        real(real64) :: v
        integer :: i, with, without

        with = 0
        without = 0
        do i = 1, n_cmodel
            call build(c, i)
            call round_trip(c, back, path)
            v = back%ob0()
            if (cmodel_has_ob0(i)) then
                with = with + 1
                call check(error, v == c%ob0(), "model " // trim(cmodel_label(i)) // &
                    " must keep its ob0 across the round trip")
            else
                without = without + 1
                call check(error, v /= v, "model " // trim(cmodel_label(i)) // &
                    " has no ob0, so nothing may be written for it and %ob0() must stay NaN")
            end if
            if (allocated(error)) return
        end do
        call check(error, with > 0 .and. without > 0, &
            "both arms must be exercised -- the grid no longer has one of each")

    end subroutine test_ob0_round_trips_both_ways

    ! ================================================================================
    ! 4. The document around the section
    ! ================================================================================

    !> An absent `[cosmology]` with `found=` is an answer, not an error.
    subroutine test_absent_section_sets_found(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        logical :: there

        call pf_toml_loads(conf, '[general]' // new_line("a") // 'nproc = 1' // new_line("a"))
        call pf_cosmology_from_toml(conf, c, found = there)
        call check(error, .not. there, "found must be .false. when the section is not there")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        call check(error, .not. c%is_initialised(), &
            "an absent section must leave the cosmology unbuilt")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if

        ! The other arm of the same argument: found is .true. where the section IS there, so a
        ! `found = .false.` written unconditionally would fail here rather than pass everywhere.
        call pf_toml_close(conf)
        call pf_toml_load(conf, FIX_MINIMAL)
        call pf_cosmology_from_toml(conf, c, found = there)
        call pf_toml_close(conf)
        call check(error, there, "found must be .true. when the section IS there")
        if (allocated(error)) return
        call check(error, c%is_initialised(), "and the cosmology must then be built")

    end subroutine test_absent_section_sets_found

    !> `conf` may be a section, so `[run.cosmology]` needs no new argument.
    subroutine test_nested_section_is_reachable(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, run
        type(pf_cosmology) :: c

        call pf_toml_loads(conf, '[general]' // new_line("a") // 'nproc = 1' // new_line("a") // &
                                 '[run]' // new_line("a") // 'tag = "x"' // new_line("a") // &
                                 '[run.cosmology]' // new_line("a") // &
                                 'h0 = 70.0' // new_line("a") // 'om0 = 0.3' // new_line("a"))
        call pf_toml_section(conf, "run", run)
        call pf_cosmology_from_toml(run, c)
        call pf_toml_close(conf)

        call check(error, c%is_initialised(), "[run.cosmology] must be read through the run handle")
        if (allocated(error)) return
        call check(error, c%h0() == 70.0_real64, "and it must be the nested section's own h0")

    end subroutine test_nested_section_is_reachable

    !> A `context=` longer than `CFG_CAP` reaches the composed context capped, with an ellipsis.
    !!
    !! **The cap is exercised on the SUCCESS path, not through an abort.** `where_from` composes
    !! the file, the section and the caller's context for every read, whether or not one aborts, so
    !! a context that has to be cut is cut here -- the abort scenarios in
    !! `test/error_scenarios.f90` then only have to show that the composed text reaches the
    !! message. What can be asserted in process is that a 150-character context neither aborts nor
    !! truncates the READ: the cosmology still comes back, whole.
    !!
    !! `capped` returns a FIXED-length result and its callers `trim` it, so a context of exactly
    !! the cap and one past it take the two different arms; both are driven.
    subroutine test_a_long_context_is_capped(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        type(pf_cosmology) :: long, at_cap, short
        character(len=*), parameter :: body = '[cosmology]' // new_line("a") // &
                                              'h0 = 70.0' // new_line("a") // &
                                              'om0 = 0.3' // new_line("a")

        ! 150 characters: past the 100-character cap, so the composed context is cut and elided.
        call pf_toml_loads(conf, body, name = "run.toml")
        call pf_cosmology_from_toml(conf, long, context = repeat("abcdefghij", 15))
        call pf_toml_close(conf)

        ! Exactly the cap, which must take the other arm and not be cut.
        call pf_toml_loads(conf, body, name = "run.toml")
        call pf_cosmology_from_toml(conf, at_cap, context = repeat("abcdefghij", 10))
        call pf_toml_close(conf)

        call pf_toml_loads(conf, body, name = "run.toml")
        call pf_cosmology_from_toml(conf, short, context = "short")
        call pf_toml_close(conf)

        call check(error, long%is_initialised() .and. at_cap%is_initialised() .and. &
                   short%is_initialised(), "a long context must not stop the section being read")
        if (allocated(error)) return
        call check(error, long%h0() == 70.0_real64 .and. at_cap%h0() == 70.0_real64 .and. &
                   short%h0() == 70.0_real64, "and must not change what it reads")

    end subroutine test_a_long_context_is_capped

    !> `section=` is how one document carries more than one cosmology.
    subroutine test_section_argument_names_the_table(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: doc, conf
        type(pf_cosmology) :: first, second, back
        character(len=*), parameter :: path = "test_run/cosmology_config_two_sections.toml"

        call first%init(h0 = 70.0_real64, om0 = 0.3_real64, name = "fiducial")
        call second%init(h0 = 67.0_real64, om0 = 0.32_real64, name = "alternative")

        call pf_toml_new(doc, path)
        call pf_cosmology_to_toml(first, doc)
        call pf_cosmology_to_toml(second, doc, section = "cosmology_alt")
        call pf_toml_dump(doc, path)
        call pf_toml_close(doc)

        call pf_toml_load(conf, path)
        call pf_cosmology_from_toml(conf, back)
        call check(error, back%h0() == 70.0_real64, &
            "the default section must hold the first cosmology")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        call pf_cosmology_from_toml(conf, back, section = "cosmology_alt")
        call check(error, back%h0() == 67.0_real64, &
            "and section= must reach the second, which the first did not overwrite")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if

        ! Nothing is left unread across BOTH tables, so the default sweep must stay silent.
        call pf_toml_check_all(conf)
        call pf_toml_close(conf)

    end subroutine test_section_argument_names_the_table

    !> A second write replaces the first, key by key, leaving no key the new model does not set.
    subroutine test_writing_over_an_existing_section(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: doc, conf, sect
        type(pf_cosmology) :: curved, flat, back
        character(len=*), parameter :: path = "test_run/cosmology_config_overwrite.toml"
        logical :: there

        call curved%init(h0 = 70.0_real64, om0 = 0.3_real64, ode0 = 0.6_real64)
        call flat%init(h0 = 67.0_real64, om0 = 0.32_real64)

        call pf_toml_new(doc, path)
        call pf_cosmology_to_toml(curved, doc)
        ! The second write is the assertion: pf_toml_set alone refuses a key that already exists.
        call pf_cosmology_to_toml(flat, doc)
        call pf_toml_dump(doc, path)
        call pf_toml_close(doc)

        call pf_toml_load(conf, path)
        call pf_toml_section(conf, "cosmology", sect, required = .false., found = there)
        call check(error, there, "the section must be there after two writes")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        call check(error, .not. pf_toml_has(sect, "ode0"), &
            "the first model's ode0 must be gone: delete-then-set leaves no stale key")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        call pf_cosmology_from_toml(conf, back)
        call pf_toml_close(conf)

        call check(error, back%h0() == 67.0_real64, "the SECOND model is what the file records")
        if (allocated(error)) return
        call check(error, back%is_flat(), "and it is flat, as the second model was")

    end subroutine test_writing_over_an_existing_section

    !> Neither procedure touches a key outside its own section.
    subroutine test_the_rest_of_the_document_survives(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, ent
        type(pf_cosmology) :: c, other
        character(len=*), parameter :: path = "test_run/cosmology_config_whole_document.toml"
        character(len=:), allocatable :: text, outdir
        integer(int32) :: nproc, id
        real(real64) :: gh0
        integer :: k

        ! `h0` lives in [general] too, on purpose: a delete that reached outside its own section
        ! would take it, and a reader that read it from the wrong handle would answer 8.0.
        text = '[general]' // new_line("a") // 'nproc = 8' // new_line("a") // &
               'output_dir = "out"' // new_line("a") // 'h0 = 8.0' // new_line("a") // &
               '[cosmology]' // new_line("a") // 'h0 = 70.0' // new_line("a") // &
               'om0 = 0.3' // new_line("a") // &
               '[[region]]' // new_line("a") // 'id = 1' // new_line("a") // &
               '[[region]]' // new_line("a") // 'id = 2' // new_line("a")

        call pf_toml_loads(conf, text)
        call pf_cosmology_from_toml(conf, c)
        call check(error, c%h0() == 70.0_real64, "the cosmology must take its own section's h0")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if

        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "nproc", nproc)
        call pf_toml_get(gen, "output_dir", outdir)
        call pf_toml_get(gen, "h0", gh0)
        call check(error, outdir == "out" .and. gh0 == 8.0_real64, &
            "[general] must still read its own values, its own h0 included")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        call check(error, pf_toml_section_count(conf, "region") == 2, &
            "both [[region]] entries must still be there")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if

        ! Write a DIFFERENT cosmology into the same document and put it through a file.
        call other%init(h0 = 67.0_real64, om0 = 0.32_real64, name = "second")
        call pf_cosmology_to_toml(other, conf)
        call pf_toml_dump(conf, path)
        call pf_toml_close(conf)

        call pf_toml_load(conf, path)
        call pf_cosmology_from_toml(conf, c)
        call check(error, c%h0() == 67.0_real64, "the dump must carry the second cosmology")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "nproc", nproc)
        call pf_toml_get(gen, "output_dir", outdir)
        call pf_toml_get(gen, "h0", gh0)
        call check(error, nproc == 8 .and. outdir == "out" .and. gh0 == 8.0_real64, &
            "[general] must come through the dump unchanged, h0 included")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        call check(error, pf_toml_section_count(conf, "region") == 2, &
            "both [[region]] entries must come through the dump")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        do k = 1, 2
            call pf_toml_section(conf, "region", k, ent)
            call pf_toml_get(ent, "id", id)
            call check(error, id == k, "each [[region]] entry must keep its own id")
            if (allocated(error)) then
                call pf_toml_close(conf)
                return
            end if
        end do

        ! Nothing is left unread, so the default (fatal) sweep must stay silent. It would abort --
        ! loudly, and this test with it -- if either procedure had marked a key it did not read, or
        ! left one of its own unread.
        call pf_toml_check_all(conf)
        call pf_toml_close(conf)

    end subroutine test_the_rest_of_the_document_survives

    ! ================================================================================
    ! 5. The unknown-key sweep (serial: these reconfigure the default logger)
    ! ================================================================================

    !> A misspelt key is nobody's, so the caller's sweep reports it.
    !!
    !! This is the assertion that the reader marks only what it READS: a reader that marked every
    !! key it knows about, read or not, would silently disarm the one check that catches a misspelt
    !! optional key.
    subroutine test_misspelt_key_is_reported(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        character(len=*), parameter :: log_file = "test_run/cosmology_config_misspelt.log"
        logical :: saw_bad, saw_good

        call pf_log_init(level = PF_LEVEL_INFO, name = "cosmology-config-test", console = .false.)
        call pf_log_add_file(log_file, level = PF_LEVEL_INFO, append = .false.)

        call pf_toml_load(conf, FIX_MISSPELT)
        call pf_cosmology_from_toml(conf, c)
        call pf_toml_check_all(conf, severity = PF_TOML_WARN)
        call pf_toml_close(conf)

        call pf_log_flush()
        call pf_log_close()

        call scan_log(log_file, "[cosmology] om_0", "[cosmology] om0", saw_bad, saw_good)
        call check(error, saw_bad, "the misspelt key om_0 must be reported by the sweep")
        if (allocated(error)) return
        call check(error, .not. saw_good, &
            "and the key the reader really read must NOT be, or the sweep is blind")

    end subroutine test_misspelt_key_is_reported

    !> The negative control for the test above: a full section plus one unknown key.
    subroutine test_unknown_key_reported_beside_a_full_section(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        type(pf_cosmology) :: c
        character(len=*), parameter :: log_file = "test_run/cosmology_config_unknown.log"
        character(len=:), allocatable :: text
        logical :: saw_bad, saw_good

        ! Every key the section knows, so a check that only fires on something MISSING is caught.
        text = '[general]' // new_line("a") // 'nproc = 1' // new_line("a") // &
               '[cosmology]' // new_line("a") // &
               'name = "full"' // new_line("a") // 'h0 = 70.0' // new_line("a") // &
               'om0 = 0.3' // new_line("a") // 'ode0 = 0.6' // new_line("a") // &
               'tcmb0 = 2.7255' // new_line("a") // 'neff = 3.046' // new_line("a") // &
               'm_nu = [0.0, 0.0, 0.06]' // new_line("a") // 'ob0 = 0.048' // new_line("a") // &
               'w0 = -0.95' // new_line("a") // 'wa = 0.1' // new_line("a") // &
               'zmax = 200.0' // new_line("a") // 'zmin = -0.5' // new_line("a") // &
               'sigma8 = 0.81' // new_line("a")

        call pf_log_init(level = PF_LEVEL_INFO, name = "cosmology-config-test", console = .false.)
        call pf_log_add_file(log_file, level = PF_LEVEL_INFO, append = .false.)

        call pf_toml_loads(conf, text)
        call pf_cosmology_from_toml(conf, c)
        call pf_toml_check_all(conf, severity = PF_TOML_WARN)
        call pf_toml_close(conf)

        call pf_log_flush()
        call pf_log_close()

        call scan_log(log_file, "[cosmology] sigma8", "[cosmology] m_nu", saw_bad, saw_good)
        call check(error, saw_bad, "an unknown key must be reported even when every known key is set")
        if (allocated(error)) return
        call check(error, .not. saw_good, "and no key the reader read may be reported")
        if (allocated(error)) return
        call check(error, c%is_initialised(), "the cosmology is still built: an unknown key is the " // &
            "caller's sweep to act on, not this module's")

    end subroutine test_unknown_key_reported_beside_a_full_section

    ! ================================================================================
    ! Helpers
    ! ================================================================================

    !> The bit pattern of a stored reference value, as the double it is.
    pure elemental function rv(bits) result(x)
        integer(int64), intent(in) :: bits !! the stored pattern
        real(real64)               :: x    !! the value

        x = transfer(bits, x)

    end function rv

    !> Parameter `q` of model `i` of the reference grid.
    pure function par(i, q) result(x)
        integer, intent(in) :: i !! the model
        integer, intent(in) :: q !! the parameter, a `cp_*` selector
        real(real64)        :: x !! the value

        x = rv(cmodel_par_bits((i - 1) * n_cparam + q))

    end function par

    !> Builds model `i` of the reference grid.
    !!
    !! `ode0` and `ob0` are forwarded through UNALLOCATED allocatables where the model does not
    !! have them, which F2018 15.5.2.12 makes an absent optional argument -- the same mechanism
    !! `pf_cosmology_from_toml` uses for the three keys whose PRESENCE is the flag.
    subroutine build(c, i)
        type(pf_cosmology), intent(out) :: c !! the cosmology to build
        integer, intent(in)             :: i !! the model's index in the grid

        real(real64), allocatable :: ode0_arg, ob0_arg
        real(real64), allocatable :: masses(:)

        if (.not. cmodel_flat(i)) ode0_arg = par(i, cp_ode0)
        if (cmodel_has_ob0(i)) ob0_arg = par(i, cp_ob0)
        masses = rv(cmodel_mnu_bits(cmodel_mnu_off(i):cmodel_mnu_off(i + 1) - 1))
        call c%init(h0 = par(i, cp_h0), om0 = par(i, cp_om0), ode0 = ode0_arg, &
                    tcmb0 = par(i, cp_tcmb0), neff = par(i, cp_neff), m_nu = masses, &
                    ob0 = ob0_arg, w0 = par(i, cp_w0), wa = par(i, cp_wa), &
                    name = trim(cmodel_label(i)))

    end subroutine build

    !> Writes `c` to `path`, reads it back, and hands back what the file rebuilt.
    subroutine round_trip(c, back, path)
        type(pf_cosmology), intent(in)  :: c    !! the cosmology to write
        type(pf_cosmology), intent(out) :: back !! receives the one the file rebuilt
        character(len=*), intent(in)    :: path !! the file to go through

        type(pf_toml) :: doc, conf

        call pf_toml_new(doc, path)
        call pf_cosmology_to_toml(c, doc)
        call pf_toml_dump(doc, path)
        call pf_toml_close(doc)

        call pf_toml_load(conf, path)
        call pf_cosmology_from_toml(conf, back)
        call pf_toml_close(conf)

    end subroutine round_trip

    !> Names the first parameter of `a` that differs from `b`'s, or `""` when none does.
    !!
    !! Every comparison is EXACT: both objects are built from the same numbers, so anything less
    !! than equality is a key that took a different route.
    subroutine same_model(a, b, what)
        type(pf_cosmology), intent(in)             :: a    !! the object under test
        type(pf_cosmology), intent(in)             :: b    !! the reference object
        character(len=:), allocatable, intent(out) :: what !! the first difference, or `""`

        real(real64), allocatable :: ma(:), mb(:)
        real(real64) :: z
        integer :: j

        what = ""
        if (.not. same(a%h0(), b%h0())) what = "%h0"
        if (what == "" .and. .not. same(a%om0(), b%om0())) what = "%om0"
        if (what == "" .and. .not. same(a%ode0(), b%ode0())) what = "%ode0"
        if (what == "" .and. .not. same(a%ok0(), b%ok0())) what = "%ok0"
        if (what == "" .and. .not. same(a%tcmb0(), b%tcmb0())) what = "%tcmb0"
        if (what == "" .and. .not. same(a%neff(), b%neff())) what = "%neff"
        if (what == "" .and. .not. same(a%ob0(), b%ob0())) what = "%ob0"
        if (what == "" .and. .not. same(a%w0(), b%w0())) what = "%w0"
        if (what == "" .and. .not. same(a%wa(), b%wa())) what = "%wa"
        if (what == "" .and. .not. same(a%zmax(), b%zmax())) what = "%zmax"
        if (what == "" .and. .not. same(a%zmin(), b%zmin())) what = "%zmin"
        if (what == "" .and. (a%is_flat() .neqv. b%is_flat())) what = "%is_flat"
        if (what == "" .and. (a%has_massive_nu() .neqv. b%has_massive_nu())) what = "%has_massive_nu"
        if (what == "") then
            call a%m_nu(ma)
            call b%m_nu(mb)
            if (size(ma) /= size(mb)) then
                what = "%m_nu size"
            else if (size(ma) > 0) then
                if (.not. all(ma == mb)) what = "%m_nu"
            end if
        end if
        if (what /= "") return

        do j = 1, n_cz
            z = rv(cz_bits(j))
            if (.not. same(a%comoving_distance(z), b%comoving_distance(z))) what = "%comoving_distance"
            if (what == "" .and. .not. same(a%age(z), b%age(z))) what = "%age"
            if (what == "" .and. .not. same(a%efunc(z), b%efunc(z))) what = "%efunc"
            if (what /= "") return
        end do

    end subroutine same_model

    !> Two answers are the same number, with NaN counted as equal to NaN.
    pure function same(x, y) result(ok)
        real(real64), intent(in) :: x  !! one answer
        real(real64), intent(in) :: y  !! the other
        logical                  :: ok !! they are the same

        if (x /= x) then
            ok = (y /= y)
        else
            ok = (x == y)
        end if

    end function same

    !> `got` agrees with `want` to `tol` relative, with NaN and the infinities compared as such.
    pure function agrees(got, want, tol) result(ok)
        real(real64), intent(in) :: got  !! what the round trip answered
        real(real64), intent(in) :: want !! what the original answered
        real(real64), intent(in) :: tol  !! relative tolerance
        logical                  :: ok   !! they agree

        if (want /= want) then
            ok = got /= got
        else if (got /= got) then
            ok = .false.
        else if (.not. ieee_is_finite(want)) then
            ok = .false.
            if (.not. ieee_is_finite(got)) ok = (got > 0.0_real64) .eqv. (want > 0.0_real64)
        else if (.not. ieee_is_finite(got)) then
            ok = .false.
        else if (want == 0.0_real64) then
            ok = abs(got) <= tol
        else
            ok = abs(got - want) <= tol * abs(want)
        end if

    end function agrees

    !> Whether a log file names `bad`, and whether it names `good` (which it must not).
    subroutine scan_log(file, bad, good, saw_bad, saw_good)
        character(len=*), intent(in) :: file     !! the log to read
        character(len=*), intent(in) :: bad      !! the text that must appear
        character(len=*), intent(in) :: good     !! the text that must not
        logical, intent(out)         :: saw_bad  !! `bad` appeared
        logical, intent(out)         :: saw_good !! `good` appeared

        character(len=512) :: line
        integer :: unit, ios

        saw_bad = .false.
        saw_good = .false.
        open(newunit=unit, file=file, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, bad) > 0) saw_bad = .true.
            if (index(line, good) > 0) saw_good = .true.
        end do
        close(unit)

    end subroutine scan_log

end module test_cosmology_config
