!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Binding the predefined columns a GENERATED table type declares: `%bind_predefined`.
!!
!! A generated table type (`doc/pages/utilities/generated-tables.md`) is an extension of `parquet_table`
!! carrying one accessor per column its schema declares. Those columns have to be checked against
!! the file, converted to the declared kind and read, before any accessor is called -- and all of
!! that lives HERE rather than in the generated text, deliberately:
!!
!! * a downstream project COMMITS its generated file, so a rule emitted into it is frozen at the
!!   version that generated it, whereas a rule living here is fixed by upgrading the library;
!! * the rules become testable without running the generator at all -- `test/test_table.f90`
!!   drives this procedure through a hand-written extension type;
!! * the generated file stays short enough that a user can read the module they are expected to
!!   edit.
!!
!! The generated `%init` therefore emits one call: a data table of names, kinds, widths and
!! `from_file` flags, and nothing else.
submodule (parquet_tables) parquet_tables_predefined
    implicit none
    !
contains
    !
    module procedure table_bind_predefined
        integer :: i, n, idx, nread
        character(len=:), allocatable :: to_read(:), ctx
        !
        call table_check_open(self, "bind_predefined")
        call bind_context_suffix(context, ctx)
        n = size(names)
        if (size(kinds) /= n .or. size(widths) /= n .or. size(from_file) /= n) then
            error stop EP // "bind_predefined: names, kinds, widths and from_file must all have " // &
                "the same size" // ctx
        end if
        if (present(units)) then
            if (size(units) /= n) then
                error stop EP // "bind_predefined: units, when given, must have the same size " // &
                    "as names" // ctx
            end if
        end if
        ! Nothing to do for a schema that declares no fields at all. That is a supported input --
        ! it generates a bare parquet_table extension, a template for a project that wants its own
        ! named table type without predefining any columns -- so it must not be an error here.
        if (n == 0) return
        !
        allocate(character(len=len(names)) :: to_read(n))
        nread = 0
        do i = 1, n
            if (from_file(i)) then
                call bind_one_file_column(self, names(i), kinds(i), widths(i), ctx, exact)
                nread = nread + 1
                to_read(nread) = names(i)
            else
                call bind_one_computed_column(self, names(i), kinds(i), widths(i), ctx)
            end if
        end do
        !
        ! ONE prefetch for every file column, and deliberately AFTER the casts above: %cast on a
        ! column nothing has read yet only rewrites the slot's declared kind (`cast_pending`), so
        ! the read below decodes straight into the declared kind in a single pass. Casting after
        ! reading would decode once and convert again.
        if (nread > 0) call prefetch_array(self, to_read(1:nread))
        !
        ! Marked last, in its own pass: creating a computed slot can reallocate `cache%cols`, so a
        ! slot index captured earlier in the loop above may no longer be the right one. The
        ! declared unit is applied here too, for the same reason -- and only AFTER the read, since
        ! materializing a file column copies the descriptor's unit onto the values.
        do i = 1, n
            idx = table_find(self, names(i))
            if (idx == 0) cycle
            self%cache%cols(idx)%predefined = .true.
            if (present(units)) call bind_fill_unit(self%cache%cols(idx), units(i))
        end do
    end procedure table_bind_predefined
    !
    !> Gives a column the unit its schema declares, but only when it has none of its own.
    !!
    !! Never an overwrite: a read-in MAML describes the physical file and is authoritative about
    !! what a file column holds. A computed column, and a file opened without a MAML, have no
    !! other source of a unit at all -- which is the gap this closes.
    subroutine bind_fill_unit(slot, declared)
        type(parquet_table_column), intent(inout) :: slot !! the column slot to fill in.
        character(len=*), intent(in) :: declared          !! the declared unit, or "" for none.
        character(len=:), allocatable :: have
        !
        if (len_trim(declared) == 0) return
        if (allocated(slot%unit)) then
            if (len_trim(slot%unit) > 0) return
        end if
        slot%unit = trim(declared)
        ! The values carry their own copy, which is what %append's unit check and the writer read.
        call slot%values%unit_string(have)
        if (len_trim(have) == 0) call slot%values%set_unit(trim(declared))
    end subroutine bind_fill_unit
    !
    !> Checks one file-backed predefined column and converts it to its declared kind.
    !!
    !! Order matters: the width is checked BEFORE the kind conversion, because a width mismatch
    !! means the schema and the file disagree about what the column is, and converting first would
    !! report the wrong problem.
    subroutine bind_one_file_column(self, name, want_kind, want_width, ctx, exact)
        class(parquet_table), intent(inout) :: self  !! the table.
        character(len=*), intent(in) :: name         !! the declared column name.
        integer, intent(in) :: want_kind             !! the declared PK_* kind.
        integer, intent(in) :: want_width            !! the declared col_size.
        character(len=*), intent(in) :: ctx          !! " (schema '...')" suffix, or "".
        logical, intent(in), optional :: exact       !! forwarded to %cast.
        character(len=:), allocatable :: sfx, from_txt, to_txt, want_txt, have_txt
        integer :: cur_kind, cur_width
        !
        if (table_find(self, name) == 0) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "bind_predefined: the schema declares column '" // trim(name) // &
                "' as a file column, but this file has no such column; declare it as " // &
                "`source: computed` if it is meant to be filled in by the program" // ctx // sfx
        end if
        ! %kind and %width, not the descriptor directly: a plain LIST column from a foreign writer
        ! has neither settled until its width is measured, and these are the two queries that do it.
        cur_kind = self%kind(name)
        cur_width = self%width(name)
        if (cur_width /= want_width) then
            call table_context_suffix(self%cache, name, sfx)
            call bind_int_text(want_width, want_txt)
            call bind_int_text(cur_width, have_txt)
            error stop EP // "bind_predefined: the schema declares col_size " // want_txt // &
                " for column '" // trim(name) // "', but the file holds " // have_txt // &
                " value(s) per row" // ctx // sfx
        end if
        if (cur_kind == want_kind) return
        !
        call parquet_kind_name(cur_kind, from_txt)
        call parquet_kind_name(want_kind, to_txt)
        if (.not. bind_kind_convertible(cur_kind, want_kind)) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "bind_predefined: the schema declares column '" // trim(name) // &
                "' as " // to_txt // ", but the file holds " // from_txt // ", and only the " // &
                "numeric kinds convert into one another without changing the column's width" // &
                ctx // sfx
        end if
        ! A warning, not an error: the declaration is a contract the file does not have to honour
        ! exactly, and a caller who declared the narrower kind may well have meant it. Derived from
        ! the KIND PAIR alone, before a byte is read, because a value-level check would give up the
        ! single-pass decode the deferred cast above exists for.
        if (.not. bind_conversion_is_lossless(cur_kind, want_kind)) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_emit_warning("parquet_table: bind_predefined: column '" // trim(name) // &
                "' is declared " // to_txt // " but the file holds " // from_txt // &
                "; values may lose precision or range" // ctx // sfx)
        end if
        call self%cast(name, want_kind, exact=exact)
    end subroutine bind_one_file_column
    !
    !> Creates one predefined column that has no file behind it: a `source: computed` field, or
    !! any column of a table built by the in-memory constructor.
    !!
    !! The rows exist and are all null, rather than the column being absent until something fills
    !! it, so that its accessor works from the moment `%init` returns and `%nrows()` describes it
    !! like every other column. `%init` + `%append_nulls` rather than `%init(nrows)`: a freshly
    !! `init`ed column has no null bitmap at all, which means "every row valid" -- the opposite of
    !! what an unfilled column should say.
    subroutine bind_one_computed_column(self, name, want_kind, want_width, ctx)
        class(parquet_table), intent(inout) :: self !! the table.
        character(len=*), intent(in) :: name        !! the declared column name.
        integer, intent(in) :: want_kind            !! the declared PK_* kind.
        integer, intent(in) :: want_width           !! the declared col_size.
        character(len=*), intent(in) :: ctx         !! " (schema '...')" suffix, or "".
        character(len=:), allocatable :: sfx
        integer(int64) :: nr
        integer :: idx
        !
        if (table_find(self, name) /= 0) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "bind_predefined: the schema declares column '" // trim(name) // &
                "' as `source: computed`, but the table already has a column of that name" // &
                ctx // sfx
        end if
        nr = self%nrows()
        call table_fix_nrows(self, name, nr)
        call table_new_slot(self, name, .false., idx)
        call self%cache%cols(idx)%values%init(want_kind, 0_int64, int(want_width, int32))
        if (nr > 0) call self%cache%cols(idx)%values%append_nulls(nr)
        self%cache%cols(idx)%declared_kind = want_kind
        self%cache%cols(idx)%width = int(want_width, int32)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%file_source = .false.
        self%cache%cols(idx)%supported = .true.
    end subroutine bind_one_computed_column
    !
    !> Whether `from_kind` can become `to_kind` at all: both numeric, and the same rank.
    !!
    !! Mirrors `cast_check_pair`'s own rule (`parquet_tables_mutate.f90`) rather than calling it,
    !! since that one is a contained procedure of a sibling submodule. Checking here rather than
    !! letting `%cast` refuse is what lets the message name the SCHEMA -- a user reading
    !! "only the numeric kinds convert" wants to know which declaration caused it.
    pure logical function bind_kind_convertible(from_kind, to_kind) result(ok)
        integer, intent(in) :: from_kind !! the column's current PK_* kind.
        integer, intent(in) :: to_kind   !! the declared PK_* kind.
        !
        ok = bind_is_numeric(from_kind) .and. bind_is_numeric(to_kind) .and. &
            (bind_is_vector(from_kind) .eqv. bind_is_vector(to_kind))
    end function bind_kind_convertible
    !
    !> Whether a PK_* kind is one of the convertible numeric kinds (scalar or vector).
    pure logical function bind_is_numeric(kind) result(ok)
        integer, intent(in) :: kind !! the PK_* discriminator to test.
        !
        ok = kind == PK_INT32 .or. kind == PK_INT64 .or. kind == PK_FLOAT32 .or. kind == PK_FLOAT64 &
            .or. kind == PK_INT32_VEC .or. kind == PK_INT64_VEC .or. kind == PK_FLOAT32_VEC &
            .or. kind == PK_FLOAT64_VEC
    end function bind_is_numeric
    !
    !> Whether a PK_* kind is one of the vector (per-row width > 1) numeric kinds.
    pure logical function bind_is_vector(kind) result(ok)
        integer, intent(in) :: kind !! the PK_* discriminator to test.
        !
        ok = kind == PK_INT32_VEC .or. kind == PK_INT64_VEC .or. kind == PK_FLOAT32_VEC &
            .or. kind == PK_FLOAT64_VEC
    end function bind_is_vector
    !
    !> Whether converting `from_kind` to `to_kind` preserves every value of every input.
    !!
    !! The question is "can this pair lose information for SOME value", not "is the target
    !! smaller", which is why `int64 -> float64` and `int32 -> float32` are on the lossy side: a
    !! real's mantissa is narrower than the integer it is being handed (2**53 and 2**24
    !! respectively). `int32 -> float64` is exact, because every int32 fits in a float64 mantissa.
    !! Only those three widenings, and the identity, are silent.
    pure logical function bind_conversion_is_lossless(from_kind, to_kind) result(ok)
        integer, intent(in) :: from_kind !! the column's current PK_* kind.
        integer, intent(in) :: to_kind   !! the declared PK_* kind.
        integer :: f, t
        !
        f = bind_scalar_kind(from_kind)
        t = bind_scalar_kind(to_kind)
        ok = f == t &
            .or. (f == PK_INT32 .and. t == PK_INT64) &
            .or. (f == PK_INT32 .and. t == PK_FLOAT64) &
            .or. (f == PK_FLOAT32 .and. t == PK_FLOAT64)
    end function bind_conversion_is_lossless
    !
    !> The scalar PK_* kind a (possibly vector) numeric kind is built from, so that the lossless
    !! rule above is written once instead of once per rank.
    pure integer function bind_scalar_kind(kind) result(k)
        integer, intent(in) :: kind !! the PK_* discriminator to reduce.
        !
        select case (kind)
        case (PK_INT32_VEC)
            k = PK_INT32
        case (PK_INT64_VEC)
            k = PK_INT64
        case (PK_FLOAT32_VEC)
            k = PK_FLOAT32
        case (PK_FLOAT64_VEC)
            k = PK_FLOAT64
        case default
            k = kind
        end select
    end function bind_scalar_kind
    !
    !> Builds the " (schema 'x.maml')" fragment every message here carries, or "" when no schema
    !! name was given. Separate from `table_context_suffix`, which names the file and the column:
    !! this one names the DECLARATION, which is the half a generated type's user can edit.
    !!
    !! A subroutine with an allocatable `intent(out)` argument rather than a
    !! `character(len=:), allocatable` FUNCTION, per the project-wide rule in CLAUDE.md (gfortran
    !! PR113797: the hidden length temporary such a result needs is not reliably thread-local).
    subroutine bind_context_suffix(context, sfx)
        character(len=*), intent(in), optional :: context     !! schema name, absent for none.
        character(len=:), allocatable, intent(out) :: sfx     !! " (schema '...')", or "".
        !
        if (present(context)) then
            sfx = " (schema '" // trim(context) // "')"
        else
            sfx = ""
        end if
    end subroutine bind_context_suffix
    !
    !> Renders an integer as trimmed text for a message. A subroutine for the same reason as
    !! `bind_context_suffix` above.
    subroutine bind_int_text(v, txt)
        integer, intent(in) :: v                              !! the value to render.
        character(len=:), allocatable, intent(out) :: txt     !! its decimal text, no padding.
        character(len=32) :: buf
        !
        write(buf, "(i0)") v
        txt = trim(buf)
    end subroutine bind_int_text
    !
end submodule parquet_tables_predefined
