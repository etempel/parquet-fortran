!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> The cosmology a TOML configuration file sets: the `[cosmology]` section of a run's
!! configuration file, read into a `pf_cosmology` and written back out.
!!
!! Two questions, in opposite directions. **Which cosmology should this run use?** is answered by
!! `pf_cosmology_from_toml`, so a program builds its model from the file rather than from
!! parameters compiled into it. **Which cosmology did this run use?** is answered by
!! `pf_cosmology_to_toml` for a program that built its model in code -- and, for one that read it
!! from the file, for free by `pf_toml_save`, which writes every key the run resolved including the
!! defaults nobody typed.
!!
!! ```fortran
!! use parquet_cosmology, only: pf_cosmology
!! use parquet_toml, only: pf_toml, pf_toml_load, pf_toml_check_all, pf_toml_close
!! use parquet_cosmology_config, only: pf_cosmology_from_toml
!!
!! type(pf_toml)      :: conf
!! type(pf_cosmology) :: cosmo
!!
!! call pf_toml_load(conf, "run.toml")
!! call pf_cosmology_from_toml(conf, cosmo)
!! call pf_toml_check_all(conf)          !! a misspelt key stops the run here
!! call pf_toml_close(conf)
!! ```
!!
!! **The section is one table of a larger configuration file**, beside `[general]`, `[[region]]`
!! and whatever else the run needs. Both procedures work through a section handle and touch
!! nothing outside it, so the rest of the document survives a write and the caller's
!! `pf_toml_check_all` still reports what nobody read.
!!
!! **The section has two forms, which are `%init`'s two forms.** A section whose `name` is one of
!! the eight named cosmologies and which sets no model parameter selects that cosmology; any other
!! section gives the parameters, with `name` a label. A `name` naming one of the eight BESIDE a
!! model parameter is fatal, because the two say different things and neither reading is safe.
!!
!! **Every validation is `%init`'s.** An `h0` outside its range, an `m_nu` of the wrong length, an
!! `ob0` above `om0` -- each aborts with the message `parquet_cosmology` already writes, with this
!! module's context (the file and the section) appended, so the message says where the bad number
!! came from. There is no second, divergent set of rules here, and no `stat=`: both modules this
!! one joins abort, and a configuration that cannot be read is a caller's mistake.
!!
!! **This module takes no critical section of its own, and must not be given one.** It reaches
!! `parquet_toml` only through that module's public entries, every one of which takes the single
!! named guard `parquet_toml_guard`; a second guard here would nest inside it and deadlock.
!! `%init`'s own aborts are already serialised under `parquet_cosmology`'s guard.
!!
!! **It is a tier of its own on purpose.** Putting these two procedures in `parquet_cosmology`
!! would make every consumer of a dependency-free numerical tier fetch `toml-f`; putting them in
!! `parquet_toml` would make every program that reads a configuration file compile the cosmology
!! tier, its integrator and its interpolator. Here, nothing anyone already imports grows.
!!
!! Guide page: [Configuration files with parquet_toml](../utilities/configuration-files.html).
module parquet_cosmology_config
    use iso_fortran_env, only: real64
    use parquet_cosmology, only: pf_cosmology
    use parquet_logging, only: pf_log_error, pf_log_fatal
    use parquet_toml, only: pf_toml, pf_toml_section, pf_toml_has, pf_toml_get, &
                            pf_toml_get_alloc, pf_toml_require, pf_toml_new_section, &
                            pf_toml_set, pf_toml_delete, pf_toml_path, pf_toml_filename
    implicit none
    private

    public :: pf_cosmology_from_toml, pf_cosmology_to_toml

    !> The section both procedures use when the caller names none.
    character(len=*), parameter :: CFG_SECTION = "cosmology"

    !> Longest caller-supplied text a message carries, per `.claude/rules/api-conventions.md`.
    integer, parameter :: CFG_CAP = 100

    !> Every key the `[cosmology]` section carries, in the order `%init` takes them.
    !!
    !! ONE list, used by the reader's "named beside parameters" scan and by the writer's
    !! delete-then-set pass, so the two cannot drift. `test_cosmology_config.f90` compares it with
    !! `cosmology_init_params`' dummy-argument list, read out of `src/parquet_cosmology.f90`.
    character(len=5), parameter :: CFG_KEYS(*) = [character(len=5) :: &
        "name", "h0", "om0", "ode0", "tcmb0", "neff", "m_nu", "ob0", "w0", "wa", "zmax", "zmin"]

    !> First entry of `CFG_KEYS` that describes the MODEL rather than the object.
    !!
    !! `name` is a label and `zmax`/`zmin` are tabulation, so all three are legal beside a named
    !! cosmology; everything between these two bounds is a parameter that would contradict one.
    integer, parameter :: CFG_MODEL_FIRST = 2
    !> Last entry of `CFG_KEYS` that describes the model. See `CFG_MODEL_FIRST`.
    integer, parameter :: CFG_MODEL_LAST = 10

    !> The eight named cosmologies, in `parquet_cosmology`'s own order and spelling.
    !!
    !! A COPY: the library's list is private to `parquet_cosmology` and this feature adds no public
    !! name to that module. `test_cosmology_config.f90` reads the original out of the source and
    !! fails when the two differ, so the copy cannot drift silently.
    character(len=8), parameter :: CFG_NAMED(*) = [character(len=8) :: &
        "WMAP1", "WMAP3", "WMAP5", "WMAP7", "WMAP9", "Planck13", "Planck15", "Planck18"]

    ! ---- `%init`'s own defaults, which this module must not diverge from ----------------------
    !
    ! They are private to `parquet_cosmology`, so they are restated here and pinned by
    ! `the defaults are %init's defaults`: a minimal section built through this module is compared
    ! with `call cosmo%init(h0, om0)` field by field.

    real(real64), parameter :: CFG_TCMB0 = 0.0_real64    !! `%init`'s `tcmb0` default: no radiation.
    real(real64), parameter :: CFG_NEFF = 3.04_real64    !! `%init`'s `neff` default -- NOT 3.046.
    real(real64), parameter :: CFG_W0 = -1.0_real64      !! `%init`'s `w0` default.
    real(real64), parameter :: CFG_WA = 0.0_real64       !! `%init`'s `wa` default.
    real(real64), parameter :: CFG_ZMAX = 1100.0_real64  !! `%init`'s `zmax` default.
    real(real64), parameter :: CFG_ZMIN = -0.9_real64    !! `%init`'s `zmin` default.

contains

    !> Builds the cosmology the `[cosmology]` section of `conf` describes.
    !!
    !! ```fortran
    !! call pf_cosmology_from_toml(conf, cosmo, [found], [section], [context])
    !! ```
    !!
    !! `conf` may be a whole document or a section of one, which is what makes a nested
    !! `[run.cosmology]` reachable with no extra argument: take `run` with `pf_toml_section` and
    !! pass that handle here.
    !!
    !! **`found=` decides whether an absent section is an error.** With it, an absent
    !! `[cosmology]` sets `found = .false.`, leaves `cosmo` unbuilt and prints nothing; without it,
    !! an absent section is fatal through `pf_toml_section`'s own required path and message.
    !!
    !! **Which form the section takes is decided by `name`.** One of the eight named cosmologies
    !! selects that cosmology, matched without regard to case; anything else -- including no `name`
    !! at all -- takes the parameter form, where `h0` and `om0` are required and every other key is
    !! optional with `%init`'s own default. `ode0` absent means the model is flat, `ob0` absent
    !! means `%ob0()` is NaN and `m_nu` absent means every species is massless: for those three,
    !! and only those three, the key's PRESENCE is the flag, which is why they reach `%init`
    !! through unallocated allocatables rather than through a branch.
    !!
    !! **Nothing is marked that was not read**, so the caller's `pf_toml_check_all` still reports a
    !! misspelt key -- `om_0 = 0.3` is then "a key nobody read", which is the only way to catch a
    !! misspelling of an optional key.
    subroutine pf_cosmology_from_toml(conf, cosmo, found, section, context)
        type(pf_toml), intent(in)              :: conf    !! an open document, or a section of one
        type(pf_cosmology), intent(out)        :: cosmo   !! the cosmology the section describes
        logical, intent(out), optional         :: found   !! whether the section was there
        character(len=*), intent(in), optional :: section !! section name; default `"cosmology"`
        character(len=*), intent(in), optional :: context !! appended to any abort message

        type(pf_toml) :: sect
        logical :: there, named
        character(len=:), allocatable :: label, ctx, sname
        real(real64) :: h0, om0, tcmb0, neff, w0, wa, zmax, zmin
        real(real64), allocatable :: ode0, ob0, m_nu(:)

        call section_name(section, sname)
        call pf_toml_section(conf, sname, sect, required = .not. present(found), found = there)
        if (present(found)) found = there
        if (.not. there) return

        call where_from(sect, context, ctx)

        ! The label, always read, so that a saved configuration records it whether or not the file
        ! set one. `"custom"` is `%init`'s own default and is not one of the eight, so an absent
        ! key takes the parameter form without a second test.
        call pf_toml_get(sect, "name", label, default = "custom")
        named = is_named(label)
        if (named) call refuse_named_with_parameters(sect, label, ctx)

        ! Tabulation, not model: both forms take them, and both are recorded either way.
        call pf_toml_get(sect, "zmax", zmax, default = CFG_ZMAX)
        call pf_toml_get(sect, "zmin", zmin, default = CFG_ZMIN)

        if (named) then
            call cosmo%init(label, zmax = zmax, zmin = zmin, context = ctx)
            return
        end if

        call require_the_parameters(sect, label, ctx)
        call pf_toml_get(sect, "h0", h0)
        call pf_toml_get(sect, "om0", om0)
        if (pf_toml_has(sect, "ode0")) then
            allocate (ode0)
            call pf_toml_get(sect, "ode0", ode0)
        end if
        call pf_toml_get(sect, "tcmb0", tcmb0, default = CFG_TCMB0)
        call pf_toml_get(sect, "neff", neff, default = CFG_NEFF)
        if (pf_toml_has(sect, "m_nu")) call pf_toml_get_alloc(sect, "m_nu", m_nu)
        if (pf_toml_has(sect, "ob0")) then
            allocate (ob0)
            call pf_toml_get(sect, "ob0", ob0)
        end if
        call pf_toml_get(sect, "w0", w0, default = CFG_W0)
        call pf_toml_get(sect, "wa", wa, default = CFG_WA)

        ! ONE call. `ode0`, `ob0` and `m_nu` are unallocated where the section did not set them,
        ! which F2018 15.5.2.12 makes an ABSENT optional argument -- the mechanism `row_validity`
        ! and the `mat_*` masks already use here, and what turns an eight-way cascade of nearly
        ! identical `%init` calls into this one.
        call cosmo%init(h0 = h0, om0 = om0, ode0 = ode0, tcmb0 = tcmb0, neff = neff, &
                        m_nu = m_nu, ob0 = ob0, w0 = w0, wa = wa, name = label, &
                        zmax = zmax, zmin = zmin, context = ctx)

    end subroutine pf_cosmology_from_toml

    !> Writes `cosmo` into the `[cosmology]` section of `conf`, replacing whatever it held.
    !!
    !! ```fortran
    !! call pf_cosmology_to_toml(cosmo, conf, [section])
    !! ```
    !!
    !! The section is created when the document does not have one and reused when it does, and
    !! each key is deleted before it is set -- so the call is idempotent, leaves no key the new
    !! model does not set (a flat model written over a curved one leaves no stale `ode0`), and
    !! touches nothing outside its own section.
    !!
    !! **What is written is what rebuilds the same object**: `ode0` only for a model that is not
    !! flat, `m_nu` only where a species is massive, `ob0` only where the model has one, `w0` and
    !! `wa` only where they are not a cosmological constant. `pf_cosmology_to_toml` followed by
    !! `pf_cosmology_from_toml` is a closed loop by construction.
    !!
    !! **The parameters are written, not the realization.** A model whose name is one of the eight
    !! is written WITHOUT its `name`, because a section carrying both is refused on reading and
    !! because a label cannot be trusted to mean the realization -- `%init` lets any model be
    !! labelled `"Planck18"`. What the file records is the numbers the run used, which is what
    !! survives a later change to the eight; the label is what is given up.
    subroutine pf_cosmology_to_toml(cosmo, conf, section)
        type(pf_cosmology), intent(in)         :: cosmo   !! a built cosmology
        type(pf_toml), intent(in)              :: conf    !! the document to write into
        character(len=*), intent(in), optional :: section !! section name; default `"cosmology"`

        type(pf_toml) :: sect
        character(len=:), allocatable :: label, sname
        real(real64), allocatable :: masses(:)
        real(real64) :: v
        logical :: unknown
        integer :: i

        call section_name(section, sname)
        call pf_toml_new_section(conf, sname, sect)
        do i = 1, size(CFG_KEYS)
            call pf_toml_delete(sect, trim(CFG_KEYS(i)))
        end do

        call cosmo%get_name(label)
        if (.not. is_named(label)) call pf_toml_set(sect, "name", label)

        call pf_toml_set(sect, "h0", cosmo%h0())
        call pf_toml_set(sect, "om0", cosmo%om0())
        ! `%ode0()` answers the DERIVED value for a flat model, so writing it unconditionally would
        ! produce a file that rebuilds a model flat only to rounding. `%is_flat()` is a bit test.
        if (.not. cosmo%is_flat()) call pf_toml_set(sect, "ode0", cosmo%ode0())
        call pf_toml_set(sect, "tcmb0", cosmo%tcmb0())
        call pf_toml_set(sect, "neff", cosmo%neff())
        if (cosmo%has_massive_nu()) then
            call cosmo%m_nu(masses)
            call pf_toml_set(sect, "m_nu", masses)
        end if
        ! `%ob0()` is NaN when the model has none, and `nan` is not a number TOML can carry back.
        ! The screen is its OWN statement and an inequality against itself, never an ordered
        ! comparison (`.claude/rules/fortran-gotchas.md`).
        v = cosmo%ob0()
        unknown = (v /= v)
        if (.not. unknown) call pf_toml_set(sect, "ob0", v)
        v = cosmo%w0()
        if (v /= CFG_W0) call pf_toml_set(sect, "w0", v)
        v = cosmo%wa()
        if (v /= CFG_WA) call pf_toml_set(sect, "wa", v)
        call pf_toml_set(sect, "zmax", cosmo%zmax())
        call pf_toml_set(sect, "zmin", cosmo%zmin())

    end subroutine pf_cosmology_to_toml

    ! ================================================================================
    ! Private helpers
    ! ================================================================================

    !> The section name to work on: the caller's, or `"cosmology"`.
    pure subroutine section_name(section, name)
        character(len=*), intent(in), optional     :: section !! the caller's name, if any
        character(len=:), allocatable, intent(out) :: name    !! receives the name to use

        if (present(section)) then
            name = section
        else
            name = CFG_SECTION
        end if

    end subroutine section_name

    !> Whether `label` names one of the eight, matched without regard to case.
    !!
    !! The same match `%init` makes: leading and trailing blanks are ignored, so `"  planck18 "`
    !! and `"Planck18"` name the same cosmology.
    pure function is_named(label) result(yes)
        character(len=*), intent(in) :: label !! the `name` the section carries
        logical                      :: yes   !! it is one of the eight

        integer :: i
        character(len=len(label)) :: folded

        folded = adjustl(label)
        call fold(folded)
        yes = .false.
        do i = 1, size(CFG_NAMED)
            if (trim(folded) == trim(lowered(CFG_NAMED(i)))) then
                yes = .true.
                exit
            end if
        end do

    end function is_named

    !> `text` folded to lower case in place, ASCII only.
    pure subroutine fold(text)
        character(len=*), intent(inout) :: text !! the text to fold

        integer :: i, c

        do i = 1, len(text)
            c = iachar(text(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) text(i:i) = achar(c + 32)
        end do

    end subroutine fold

    !> `name` folded to lower case, for one comparison.
    !!
    !! The result length is the DUMMY's, never a value-dependent expression: an automatic-length
    !! character result whose length depends on the value is the shape this project avoids.
    pure function lowered(name) result(out)
        character(len=*), intent(in) :: name !! a canonical spelling
        character(len=len(name))     :: out  !! it, lower case

        out = name
        call fold(out)

    end function lowered

    !> The context every abort from here carries: the file, the section, and the caller's own.
    subroutine where_from(sect, context, ctx)
        type(pf_toml), intent(in)                  :: sect    !! the section being read
        character(len=*), intent(in), optional     :: context !! the caller's context, if any
        character(len=:), allocatable, intent(out) :: ctx     !! receives the composed context

        character(len=:), allocatable :: file, path

        call pf_toml_filename(sect, file)
        call pf_toml_path(sect, path)
        ctx = "configuration file " // file // ", section [" // path // "]"
        if (present(context)) ctx = ctx // "; " // trim(capped(context))

    end subroutine where_from

    !> `text` cut to `CFG_CAP` characters, with an ellipsis where it was cut, blank-padded.
    !!
    !! A FIXED result length, so the result needs no hidden length variable and no specification
    !! expression over a value; every call site wraps it in `trim`.
    pure function capped(text) result(out)
        character(len=CFG_CAP + 3) :: out                 !! it, capped and blank-padded
        character(len=*), intent(in) :: text              !! caller-supplied text

        if (len_trim(text) > CFG_CAP) then
            out = text(1:CFG_CAP) // "..."
        else
            out = trim(text)
        end if

    end function capped

    !> Refuses a section whose `name` is one of the eight and which also sets a model parameter.
    !!
    !! The two say different things and neither reading is safe: taking the name silently ignores
    !! the parameters, and taking the parameters produces an object whose `%get_name()` claims a
    !! realization it does not have.
    subroutine refuse_named_with_parameters(sect, label, ctx)
        type(pf_toml), intent(in)    :: sect  !! the section being read
        character(len=*), intent(in) :: label !! the `name` it carries
        character(len=*), intent(in) :: ctx   !! the file and the section

        integer :: i
        character(len=:), allocatable :: offenders

        offenders = ""
        do i = CFG_MODEL_FIRST, CFG_MODEL_LAST
            if (pf_toml_has(sect, trim(CFG_KEYS(i)))) then
                if (len(offenders) > 0) offenders = offenders // ", "
                offenders = offenders // trim(CFG_KEYS(i))
            end if
        end do
        if (len(offenders) == 0) return

        call pf_log_error("pf_cosmology_from_toml: " // ctx // " names the cosmology """ // &
                          trim(capped(label)) // """ and also sets " // offenders)
        call pf_log_fatal("ERR: pf_cosmology_from_toml: a named cosmology cannot be given " // &
                          "parameters as well; drop the name to use the parameters, or drop " // &
                          "the parameters to use the name")

    end subroutine refuse_named_with_parameters

    !> `h0` and `om0` are required in the parameter form.
    !!
    !! `pf_toml_require` names them both and quotes the file, which is the right message for a
    !! section that simply forgot one. A section that sets NEITHER while carrying a `name` is a
    !! different mistake -- almost always a misspelt cosmology -- and gets a message that says so.
    subroutine require_the_parameters(sect, label, ctx)
        type(pf_toml), intent(in)    :: sect  !! the section being read
        character(len=*), intent(in) :: label !! the `name` it carries
        character(len=*), intent(in) :: ctx   !! the file and the section

        logical :: any_given

        any_given = pf_toml_has(sect, "h0")
        if (.not. any_given) any_given = pf_toml_has(sect, "om0")
        if (.not. any_given .and. pf_toml_has(sect, "name")) then
            call pf_log_error("pf_cosmology_from_toml: " // ctx // " names """ // &
                              trim(capped(label)) // """, which is not one of Planck18, Planck15, " // &
                              "Planck13, WMAP9, WMAP7, WMAP5, WMAP3 and WMAP1, and gives " // &
                              "neither h0 nor om0")
            call pf_log_fatal("ERR: pf_cosmology_from_toml: a name that is not one of the eight " // &
                              "named cosmologies is a label, so the section must give the " // &
                              "parameters h0 and om0")
        end if
        call pf_toml_require(sect, "h0;om0")

    end subroutine require_the_parameters

end module parquet_cosmology_config
