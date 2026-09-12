!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Reading and writing TOML configuration files: a convenience layer over `toml-f`.
!!
!! `toml-f` parses TOML and this module makes it safe and convenient to *read a configuration
!! file with*. Six things it does that a bare `toml-f` call does not:
!!
!! 1. **A wrong-typed value never leaves your variable undefined.** `toml-f`'s own `get_value`
!!    reports `type_mismatch` through an optional argument nobody has to look at and leaves its
!!    `intent(out)` result untouched, so `samples = 100.0` read as an integer sets nothing at all.
!!    Every getter here checks, and stops with the offending line quoted.
!! 2. **A default is applied by this module, never by `toml-f`.** `get_value(..., default=)`
!!    *inserts* the default into the parsed document, which blinds any later check for unknown
!!    keys. Nothing here ever writes to the parsed document while reading, so `pf_toml_check` is
!!    correct whenever it is called -- ordering is not a trap.
!! 3. **Whole-array reads for strings**, which `toml-f` has no getter for.
!! 4. **Diagnostics that point at the source line**, with the caret `toml-f` can draw, from one
!!    call rather than four.
!! 5. **A key nobody read is reported** (`pf_toml_check`), which is the only way to catch a
!!    misspelt *optional* key -- it otherwise takes its default in silence.
!! 6. **A section nobody read is reported** (`pf_toml_check_all`), the same failure one level up.
!!
!! ```fortran
!! type(pf_toml) :: conf, gen
!! integer :: nproc
!! character(len=:), allocatable :: outdir
!!
!! call pf_toml_load(conf, "config.toml")
!! call pf_toml_section(conf, "general", gen)
!! call pf_toml_get(gen, "nproc", nproc, default = 1)
!! call pf_toml_get(gen, "output_dir", outdir)      !! no default: the key is required
!! call pf_toml_check_all(conf)                     !! anything unread is a mistake
!! call pf_toml_close(conf)
!! ```
!!
!! **One rule covers every getter, at every shape.** The bare call requires its key. `pf_toml_get`
!! opts out with `default =`; `pf_toml_get_opt`, `pf_toml_get_alloc_opt` and
!! `pf_toml_get_strings_opt` opt out by leaving whatever the variable already holds. `default =`
!! is offered exactly where the caller owns the size -- the scalar and fixed-length-array forms --
!! and never by `pf_toml_get_alloc` or `pf_toml_get_strings`, which take their size from the file.
!!
!! **Output goes through `parquet_logging`, not through this library's own message channels.**
!! That is deliberate and it is this module's one departure from the rest of parquet-fortran:
!! configuration diagnostics are read by the operator of the *calling* program, in that program's
!! log, beside its own startup messages. Two consequences: `parquet_verbosity` and
!! `parquet_message_stream` do **not** govern anything printed here (the logger's own levels and
!! sinks do), and a program that never calls `pf_log_init` still gets output, because the default
!! logger behaves as though it owned one stdout console sink at `PF_LEVEL_INFO`.
!!
!! **Every public procedure is safe to call from inside an OpenMP parallel region**, by
!! serialising rather than by being parallel: each takes one module-wide named critical section.
!! Configuration reading is nowhere near a hot loop, so one lock per call costs nothing, and it is
!! what makes many threads reading one file -- the case this module was asked to support -- sound.
!! See "Thread safety" on the guide page. What the guard does NOT cover is *lifetimes*: closing a
!! document while another thread is still using a handle taken from it is the caller's to
!! synchronise, exactly as the lifetime rule below says.
!!
!! **Lifetime rule: a section handle borrows from its document, so close the document last.**
!! `pf_toml_load` fills the one handle that *owns* the parsed file; every handle `pf_toml_section`
!! produces points into it. Using a section handle after `pf_toml_close` is a dangling pointer,
!! the same contract `parquet_string` (a borrowed handle into a column) already carries.
!!
!! Guide page: [Configuration files with parquet_toml](../utilities/configuration-files.html).
module parquet_toml
    use iso_fortran_env, only: int32, int64, real32, real64
    ! `len` is imported under another name on purpose: toml-f extends the intrinsic with specifics
    ! for its own types, and this module also calls the intrinsic `len` on ordinary strings.
    !
    ! NOTHING FROM toml-f IS RE-EXPORTED. The import list is explicit and the module's default
    ! accessibility is private, so `use parquet_toml` -- and `use parquet`, which re-exports this
    ! module -- puts no toml-f name into a user's namespace. That is what makes the two escape
    ! hatches (pf_toml_table, pf_toml_context) an explicit act: their caller writes its own
    ! `use tomlf, only: toml_table` line, which is the point where it leaves this wrapper behind.
    use tomlf, only: toml_table, toml_array, toml_keyval, toml_value, toml_key, toml_error, &
                     toml_context, toml_parser_config, toml_stat, toml_load, toml_loads, &
                     toml_dump, get_value, set_value, add_table, add_array, new_table, &
                     is_array_of_tables, toml_len => len
    use parquet_logging, only: pf_log_error, pf_log_warning, pf_log_fatal, pf_str, &
                               pf_log_level_from_name
    implicit none
    private

    ! ---- Status codes returned by pf_toml_load/pf_toml_loads' optional `status` ----

    !> `status`: the file parsed.
    integer, parameter, public :: PF_TOML_OK = 0
    !> `status`: the file could not be opened (absent, unreadable, a directory).
    integer, parameter, public :: PF_TOML_ERR_OPEN = 1
    !> `status`: the file was opened but is not valid TOML.
    integer, parameter, public :: PF_TOML_ERR_PARSE = 2

    ! ---- Severities, for the checks that can be told how loud to be ----

    !> A severity that says nothing at all.
    integer, parameter, public :: PF_TOML_IGNORE = 0
    !> A severity that logs a warning and carries on.
    integer, parameter, public :: PF_TOML_WARN = 1
    !> A severity that logs the problem and stops the run. The default for every check.
    integer, parameter, public :: PF_TOML_FATAL = 2

    ! ---- Input-sanity bounds ----
    !
    ! Published as read-only constants rather than settings, exactly like the parquet_max_* family:
    ! a key longer than this is a broken file, and making the cap settable would turn a guard
    ! against runaway input into a way to grow an internal buffer without limit. Exceeding either
    ! is a named, fatal failure -- never a silent truncation, which would make two distinct keys
    ! compare equal in the accumulator and so hide each other from the unknown-key sweep.

    !> Longest key name this module handles.
    integer, parameter, public :: PF_TOML_MAX_KEY = 128
    !> Longest `section.key` path this module composes for a message or the accumulator.
    integer, parameter, public :: PF_TOML_MAX_PATH = 256

    !> PRIVATE. One parsed configuration file, allocated once by `pf_toml_load` and shared, by
    !! pointer, with every section handle taken from it.
    !!
    !! Being reached through a pointer is what makes every `tbl` pointer in `pf_toml` conforming
    !! without the caller having to declare their document `target`, and it is why a section
    !! handle costs two pointers rather than a copy of the file.
    type :: pf_toml_doc
        !> Path as the caller gave it, for messages. `<string>` for a `pf_toml_loads` document
        !! that was given no name, and whatever name it was given otherwise.
        character(len=:), allocatable :: file
        !> The parsed document. Never written to while reading -- see the module header's point 2.
        type(toml_table), allocatable :: root
        !> Token context, parsed with `context_detail = 1` so that a diagnostic can point at the
        !! offending *value* rather than merely name its key.
        type(toml_context) :: ctx
        !> The shadow document: what `pf_toml_save` writes. Every getter records the value it
        !! RESOLVED here -- the file's value, or the default it applied -- so the saved file is the
        !! configuration the run actually used, with defaults made explicit. Kept separate from
        !! `root` precisely so that reading never modifies the parsed document.
        type(toml_table), allocatable :: shadow
        !> Every key path this document has been asked for: `general.log_filename`,
        !! `sregion[3].max_airmass`, or `sregion` and `sregion[3]` for an opened section. This is
        !! the automatically accumulated known-key list, and it lives here rather than on a handle
        !! so that a whole-document check still works after every section handle has gone.
        character(len=PF_TOML_MAX_PATH), allocatable :: seen(:)
        !> How many entries of `seen` are in use.
        integer :: nseen = 0
    end type pf_toml_doc

    !> A handle on one TOML table: a whole document, or one section of it.
    !!
    !! Cheap to copy -- three pointers, a name and a flag -- and every copy *borrows*; only the
    !! handle `pf_toml_load`/`pf_toml_loads`/`pf_toml_new` filled owns the document behind it.
    !!
    !! **It has no allocatable component and no finalizer, deliberately.** That is what keeps it
    !! legal both in an OpenMP `private()` clause (gfortran does not reliably initialise a private
    !! copy of a finalizable type) and as a block-local variable inside a parallel region (ifx
    !! crashes on a block-local whose type has allocatable components). The price is that
    !! `pf_toml_close` is explicit; an abandoned document leaks one configuration file's worth of
    !! memory, in a process that is about to exit anyway.
    type, public :: pf_toml
        private
        !> The shared document. Borrowed by a section handle, owned by exactly one handle.
        type(pf_toml_doc), pointer :: doc => null()
        !> This handle's own table: the parsed root, or one section of it. Null when a handle is
        !! closed -- an optional section that was not found, or a document not yet loaded.
        type(toml_table), pointer :: tbl => null()
        !> The matching table in the shadow document, where resolved values are recorded.
        type(toml_table), pointer :: shadow => null()
        !> Display path: empty at the root, `general`, `general.limits`, `sregion[3]`.
        character(len=PF_TOML_MAX_PATH) :: path = ''
        !> `.true.` only for the handle that owns the document, i.e. the one an open filled.
        logical :: owner = .false.
    end type pf_toml

    !> A list of strings read out of the configuration file, each keeping its own exact length.
    !!
    !! This is how a variable-length string list is read -- `pf_toml_get_alloc` deliberately has no
    !! `character` form, because a blank-padded array of one declared length loses each element's
    !! own length and invites `trim` bugs at every use.
    !!
    !! **It is a self-contained value**: the strings are copied out of the document, so it holds no
    !! pointer into one, stays valid after `pf_toml_close`, and copies by ordinary assignment.
    !! Having allocatable components, it must be named in an OpenMP `private()` clause rather than
    !! declared block-local inside a parallel region -- see the guide page's thread-safety section.
    type, public :: pf_toml_strings
        private
        !> Every element's bytes, packed end to end with no separator and no padding.
        character(len=1), allocatable :: buf(:)
        !> Element `i` occupies `buf(off(i) : off(i+1) - 1)`. Sized `n + 1`, so an element's length
        !! is `off(i+1) - off(i)` and a zero-length element needs no special case.
        integer, allocatable :: off(:)
        !> How many elements the list holds.
        integer :: n = 0
    contains
        procedure :: get => strings_get        !! Copies element `i` out, at its exact length.
        procedure :: count => strings_count    !! How many elements the list holds.
        procedure :: length => strings_length  !! Length of element `i`, without copying it.
    end type pf_toml_strings

    ! ---- Public procedures ----

    public :: pf_toml_load, pf_toml_loads, pf_toml_close, pf_toml_new
    public :: pf_toml_section, pf_toml_section_count, pf_toml_has_section
    public :: pf_toml_get, pf_toml_get_alloc, pf_toml_get_strings, pf_toml_get_level
    public :: pf_toml_get_opt, pf_toml_get_alloc_opt, pf_toml_get_strings_opt
    public :: pf_toml_require, pf_toml_retire, pf_toml_check, pf_toml_check_all
    public :: pf_toml_mark, pf_toml_mark_section
    public :: pf_toml_has, pf_toml_is_open, pf_toml_keys, pf_toml_path, pf_toml_filename
    public :: pf_toml_table, pf_toml_context, pf_toml_report
    public :: pf_toml_new_section, pf_toml_append_section
    public :: pf_toml_set, pf_toml_update, pf_toml_delete, pf_toml_save, pf_toml_dump

    !> Opens one section of a configuration file, as `[name]` or as entry `idx` of `[[name]]`.
    !!
    !! Both forms take the same trailing options and produce the same kind of handle:
    !!
    !! ```fortran
    !! call pf_toml_section(parent, name, sect [, required] [, found])       !! [name]
    !! call pf_toml_section(parent, name, idx, sect [, required] [, found])  !! [[name]] entry idx
    !! ```
    !!
    !! `parent` is any `type(pf_toml)` -- the document handle or another section -- so nesting is
    !! just repetition and `sect`'s display path composes (`general`, then `general.limits`).
    !! `name` is looked up literally and is never split on a dot: TOML allows a dot inside a quoted
    !! key, so `"a.b" = 1` at the root is a legal key distinct from `[a]`/`b = 1`. To reach a
    !! nested section, open its parent first.
    !!
    !! `required` defaults to `.true.`: a section you ask for is one your program needs. With
    !! `required = .false.` an absent section leaves `sect` closed, and sets `found` to `.false.`
    !! if it is present.
    !!
    !! **Reading a value from a closed handle is fatal** -- never a silent default, because that
    !! would make an absent section indistinguishable from an empty one. Guard the reads with
    !! `pf_toml_is_open`, or take the defaults from the variables by reading through
    !! `pf_toml_get_opt`, which is fatal on a closed handle just the same.
    !!
    !! **The validators are the exception and are silent on a closed handle**: `pf_toml_check`,
    !! `pf_toml_retire`, `pf_toml_mark`, `pf_toml_mark_section` and `pf_toml_section_count` all do
    !! nothing rather than stop, because a section that does not exist has no keys to sweep, none
    !! to mark and none to warn about -- and whether an optional section is present is not known
    !! before it is opened. `pf_toml_require` still stops: its keys are required and they are not
    !! there, which is a real failure at any level.
    !!
    !! `idx` is 1-based, and `required` governs it exactly as it governs an absent name: the
    !! default `.true.` makes an index outside `1 .. count` fatal, while `required = .false.`
    !! leaves `sect` closed and `found` `.false.`, so "is there an entry `idx`?" is a question a
    !! caller may ask rather than one they must already know the answer to.
    !! `pf_toml_section_count` is the other way to find out, and
    !! `do i = 1, pf_toml_section_count(...)` remains the idiom for walking every entry.
    !!
    !! A `name` that is present but is **not** an array of tables is fatal whatever `required`
    !! says, on the same reasoning as `pf_toml_section_count`: absence is a configuration choice,
    !! a name of the wrong shape is a programming error.
    !!
    !! Opening a section marks it as read, so neither `pf_toml_check` on the parent nor
    !! `pf_toml_check_all` reports it. For `[[name]]`, both the array name and the entry's own
    !! `name[idx]` path are marked, which is what lets `pf_toml_check_all` report an entry the
    !! program never opened as one line rather than as one line per key it holds.
    interface pf_toml_section
        module procedure pf_toml_section_named
        module procedure pf_toml_section_indexed
    end interface pf_toml_section

    !> Reads one configuration value into storage the caller already owns.
    !!
    !! ```fortran
    !! call pf_toml_get(sect, key, value  [, default])   !! scalar
    !! call pf_toml_get(sect, key, values [, default])   !! array, exact length match
    !! ```
    !!
    !! `value` is an `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`, `logical`
    !! or `character(len=:), allocatable` scalar; `values` is a rank-1 array of any of those six,
    !! with `character(len=*)` for the string form. In every form `key` is the key to read and
    !! `sect` the handle to read it from.
    !!
    !! **Every form requires its key unless you pass a `default`, and that rule is the same at both
    !! ranks.** An array's `default` is a rank-1 array of the same type holding exactly
    !! `size(values)` elements; any other size is fatal, in the same way and with the same shape of
    !! message as a file list of the wrong length. To leave the variable alone rather than default
    !! it, call `pf_toml_get_opt`, which is the whole of that procedure's meaning.
    !!
    !! **Never pass one variable as both `values` and `default`.** `call pf_toml_get(s, "k", x, x)`
    !! associates `x` with an `intent(out)` dummy and an `intent(in)` one, which F2018 15.5.2.13
    !! forbids and no compiler diagnoses; the "default" applied is then whatever undefining `x` on
    !! entry left behind. That call means `call pf_toml_get_opt(s, "k", x)`.
    !!
    !! **Write `default =` as a keyword argument.** Nothing else can occupy fourth position, so
    !! there is no ambiguity to resolve -- but the keyword is what makes a bare `.true.` there
    !! readable as a value rather than as a flag.
    !!
    !! An array's length must match `size(values)` exactly, in **both** directions, and a mismatch
    !! is fatal with both counts and the source line quoted. Silently using a prefix of a list that
    !! is too long pairs each value with the wrong slot, which nothing downstream could catch. A
    !! string element longer than `len(values)` is fatal for the same reason -- a silently clipped
    !! file name is the worst outcome available here.
    !!
    !! Reading a `real` accepts a TOML integer and converts it, as toml-f does; reading an
    !! `integer` does *not* accept a TOML float, and a value too large for `integer(int32)` is
    !! reported rather than wrapped.
    interface pf_toml_get
        module procedure pf_toml_get_i32, pf_toml_get_i64
        module procedure pf_toml_get_r32, pf_toml_get_r64
        module procedure pf_toml_get_log, pf_toml_get_str
        module procedure pf_toml_get_i32_arr, pf_toml_get_i64_arr
        module procedure pf_toml_get_r32_arr, pf_toml_get_r64_arr
        module procedure pf_toml_get_log_arr, pf_toml_get_str_arr
    end interface pf_toml_get

    !> Reads a list whose length the *file* decides, allocating the result to fit.
    !!
    !! ```fortran
    !! call pf_toml_get_alloc(sect, key, values [, required])
    !! ```
    !!
    !! `values` is a rank-1 `allocatable` array of `integer(int32)`, `integer(int64)`,
    !! `real(real32)`, `real(real64)` or `logical`, `sect` the handle to read from and `key` the
    !! key. There is deliberately no `character` form: a variable-shape string list is read with
    !! `pf_toml_get_strings`, which keeps each element's own length.
    !!
    !! The key is required, like every other bare getter, and no `default` is possible or offered
    !! -- the whole point of this form is that the *file* chooses the size. To read a list that may
    !! legitimately be absent, call `pf_toml_get_alloc_opt`: it leaves the variable exactly as it
    !! found it, so an unallocated one stays unallocated and `allocated(values)` answers "did the
    !! file set this key?".
    interface pf_toml_get_alloc
        module procedure pf_toml_get_alloc_i32, pf_toml_get_alloc_i64
        module procedure pf_toml_get_alloc_r32, pf_toml_get_alloc_r64
        module procedure pf_toml_get_alloc_log
    end interface pf_toml_get_alloc

    !> Reads one value **if the file sets the key**, and otherwise leaves the variable alone.
    !!
    !! ```fortran
    !! call pf_toml_get_opt(sect, key, value)    !! scalar
    !! call pf_toml_get_opt(sect, key, values)   !! array, exact length match when present
    !! ```
    !!
    !! The optional counterpart to `pf_toml_get`, over the same six types at the same two ranks:
    !! `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`, `logical` and
    !! `character` -- `character(len=:), allocatable` when scalar, `character(len=*)` when rank-1.
    !! There is no `default` argument and there must not be, because **the variable is the
    !! default**; there is no `required` argument either, since not being required is the whole
    !! procedure.
    !!
    !! This is what makes "inherit from the previous entry unless this one overrides it" two
    !! ordinary lines -- assign the inherited value, then call this for each key the entry may set:
    !!
    !! ```fortran
    !! this_region = previous_region
    !! call pf_toml_get_opt(sect, "max_airmass", this_region%max_airmass)
    !! ```
    !!
    !! **The caller must have given the variable a value.** `value` is `intent(inout)`, so passing
    !! an undefined variable is the caller's own non-conformance and a checked build
    !! (`nagfor -C=undefined`) traps on it. That is the assertion the separate name makes:
    !! `pf_toml_get` says "this program needs a value here", `pf_toml_get_opt` says "this variable
    !! already has one".
    !!
    !! Everything else is `pf_toml_get`'s. A wrong-typed value is fatal rather than left undefined;
    !! an array's length must match `size(values)` exactly when the key IS present; and the key is
    !! recorded as read whether or not the file sets it, so `pf_toml_check` stays accurate either
    !! way.
    !!
    !! The resolved value -- the file's, or the one the variable already held -- is recorded for
    !! `pf_toml_save`, with one exception: a `character(len=:), allocatable` scalar the caller left
    !! unallocated and the file does not set stays unallocated, and an absent value is written
    !! nowhere rather than as an empty string.
    interface pf_toml_get_opt
        module procedure pf_toml_get_opt_i32, pf_toml_get_opt_i64
        module procedure pf_toml_get_opt_r32, pf_toml_get_opt_r64
        module procedure pf_toml_get_opt_log, pf_toml_get_opt_str
        module procedure pf_toml_get_opt_i32_arr, pf_toml_get_opt_i64_arr
        module procedure pf_toml_get_opt_r32_arr, pf_toml_get_opt_r64_arr
        module procedure pf_toml_get_opt_log_arr, pf_toml_get_opt_str_arr
    end interface pf_toml_get_opt

    !> Reads a file-sized list **if the file sets the key**, and otherwise leaves the variable alone.
    !!
    !! ```fortran
    !! call pf_toml_get_alloc_opt(sect, key, values)
    !! ```
    !!
    !! The optional counterpart to `pf_toml_get_alloc`, over the same five types. `values` is
    !! `intent(inout)`, so an absent key leaves it exactly as it was -- **unallocated if it was
    !! unallocated**, which makes `allocated(values)` the answer to "did the file set this key?".
    !! A variable that already held a list keeps it, and that list is what `pf_toml_save` records.
    !!
    !! There is no `default` argument, for `pf_toml_get_alloc`'s own reason: the file chooses the
    !! size.
    interface pf_toml_get_alloc_opt
        module procedure pf_toml_get_alloc_opt_i32, pf_toml_get_alloc_opt_i64
        module procedure pf_toml_get_alloc_opt_r32, pf_toml_get_alloc_opt_r64
        module procedure pf_toml_get_alloc_opt_log
    end interface pf_toml_get_alloc_opt

    !> ADDS a key that is not there yet. Fatal if it already is -- use `pf_toml_update` for that.
    !!
    !! ```fortran
    !! call pf_toml_set(sect, key, value)   !! scalar or rank-1 array
    !! ```
    !!
    !! `sect` is the handle to write into, `key` the key to add and `value` an `integer(int32)`,
    !! `integer(int64)`, `real(real32)`, `real(real64)`, `logical` or `character(len=*)` scalar, or
    !! a rank-1 array of any of those six.
    !!
    !! **The split from `pf_toml_update` is the point: each checks, and neither upserts.** A typo'd
    !! key passed to an upserting writer silently creates a second parameter nobody reads; here
    !! `pf_toml_set` refuses a key that exists and `pf_toml_update` refuses one that does not, so
    !! either mistake is a named, stopped run.
    !!
    !! Note which of the two overriding a *defaulted* parameter needs: `pf_toml_set`. A default is
    !! never written into the parsed document (see the module header), so a key the program read
    !! from its default is genuinely absent and `pf_toml_update` would refuse it.
    !!
    !! Every `character(len=*)` input is **trimmed of trailing blanks** before it is stored, scalar
    !! and array alike. Fortran pads a fixed-length variable, and in an array of mixed-length names
    !! every element but the longest arrives padded; writing those blanks out verbatim would make a
    !! saved file re-read as a different value. A genuine trailing blank goes through the escape
    !! hatch.
    interface pf_toml_set
        module procedure pf_toml_set_i32, pf_toml_set_i64
        module procedure pf_toml_set_r32, pf_toml_set_r64
        module procedure pf_toml_set_log, pf_toml_set_str
        module procedure pf_toml_set_i32_arr, pf_toml_set_i64_arr
        module procedure pf_toml_set_r32_arr, pf_toml_set_r64_arr
        module procedure pf_toml_set_log_arr, pf_toml_set_str_arr
    end interface pf_toml_set

    !> CHANGES a key that is already there. Fatal if it is not -- use `pf_toml_set` for that.
    !!
    !! ```fortran
    !! call pf_toml_update(sect, key, value)   !! scalar or rank-1 array
    !! ```
    !!
    !! Arguments and types are `pf_toml_set`'s exactly, and so is the trailing-blank rule; the two
    !! differ only in which case each refuses. See `pf_toml_set` for why they are separate.
    !!
    !! This is the one public procedure that modifies the *parsed* document, and it does so as an
    !! explicit caller act rather than as a getter side effect -- the rule that keeps
    !! `pf_toml_check` honest is about getters. The updated key is marked read, so a later sweep
    !! does not report it; a `pf_toml_get` of it afterwards returns the new value; and
    !! `pf_toml_save` writes it. It does not reach back and change a variable some earlier
    !! `pf_toml_get` already filled: it changes the document, not the past.
    interface pf_toml_update
        module procedure pf_toml_update_i32, pf_toml_update_i64
        module procedure pf_toml_update_r32, pf_toml_update_r64
        module procedure pf_toml_update_log, pf_toml_update_str
        module procedure pf_toml_update_i32_arr, pf_toml_update_i64_arr
        module procedure pf_toml_update_r32_arr, pf_toml_update_r64_arr
        module procedure pf_toml_update_log_arr, pf_toml_update_str_arr
    end interface pf_toml_update

contains

    ! ================================================================================
    ! Lifecycle
    ! ================================================================================

    !> Parses a TOML file into a document handle.
    !!
    !! Without `status`, a file that cannot be opened or does not parse is **fatal**, and toml-f's
    !! own rendered diagnostic -- source line and caret -- is written to the log first, line by
    !! line. With `status` the same two failures come back as `PF_TOML_ERR_OPEN` /
    !! `PF_TOML_ERR_PARSE` and leave `doc` closed, which is what makes "try this configuration,
    !! else that one" writable. This is the only soft failure in the module; every other failure
    !! here is a programming or configuration error with no sensible recovery.
    !!
    !! The file is always parsed with `context_detail = 1`. Not optional: without it there are no
    !! value tokens, and every "points at the offending line" promise in this module quietly
    !! degrades to "names the key".
    !!
    !! `doc` is also a handle on the **root table**, so a key sitting above the first `[section]`
    !! is read straight from it -- `call pf_toml_get(doc, "title", title)` -- with the same
    !! diagnostics, the same accumulator and the same coverage by `pf_toml_check_all`.
    subroutine pf_toml_load(doc, file, status)
        type(pf_toml), intent(out) :: doc          !! Receives the document handle. Owns the file.
        character(len=*), intent(in) :: file       !! Path to the configuration file.
        integer, intent(out), optional :: status   !! `PF_TOML_OK`/`_ERR_OPEN`/`_ERR_PARSE`.

        !$omp critical (parquet_toml_guard)
        call load_impl(doc, file, "", .true., status)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_load

    !> Parses TOML held in a string, otherwise exactly as `pf_toml_load`.
    !!
    !! Two jobs beyond symmetry: it lets a program carry a built-in default configuration as a
    !! string literal, and it lets a test suite be entirely file-free -- which matters because
    !! tests run concurrently, so a file fixture needs a unique name and its own cleanup while a
    !! string literal needs neither.
    subroutine pf_toml_loads(doc, text, name, status)
        type(pf_toml), intent(out) :: doc                  !! Receives the document handle.
        character(len=*), intent(in) :: text               !! The TOML document, as text.
        character(len=*), intent(in), optional :: name     !! Name for messages. `<string>` if absent.
        integer, intent(out), optional :: status           !! `PF_TOML_OK` or `PF_TOML_ERR_PARSE`.

        !$omp critical (parquet_toml_guard)
        if (present(name)) then
            call load_impl(doc, text, name, .false., status)
        else
            call load_impl(doc, text, "", .false., status)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_loads

    !> Creates an empty document to build and later save.
    !!
    !! The result is a document handle like any other: `pf_toml_new_section` adds sections to it,
    !! `pf_toml_set` adds keys, and `pf_toml_save` writes it out.
    subroutine pf_toml_new(doc, name)
        type(pf_toml), intent(out) :: doc               !! Receives the new document handle.
        character(len=*), intent(in), optional :: name  !! Name for messages. `<new>` if absent.

        !$omp critical (parquet_toml_guard)
        if (present(name)) then
            call new_impl(doc, name)
        else
            call new_impl(doc, "")
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_new

    !> Releases a document and everything parsed from it.
    !!
    !! **Call it last.** Every section handle taken from this document borrows from it, so using
    !! one afterwards is a dangling pointer. Closing a handle that does not own its document -- a
    !! section handle -- is a programming error and is fatal, rather than silently freeing a
    !! document somebody else is still reading.
    !!
    !! A `pf_toml_strings` value is the exception and survives: it copied its strings out.
    subroutine pf_toml_close(doc)
        type(pf_toml), intent(inout) :: doc   !! The owning handle, as an open left it.

        !$omp critical (parquet_toml_guard)
        call close_impl(doc)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_close

    ! ================================================================================
    ! Sections
    ! ================================================================================

    !> `[name]`. See the `pf_toml_section` generic for what every argument means.
    subroutine pf_toml_section_named(parent, name, sect, required, found)
        type(pf_toml), intent(in) :: parent            !! Document or section to look in.
        character(len=*), intent(in) :: name           !! Section name, looked up literally.
        type(pf_toml), intent(out) :: sect             !! Receives the section handle.
        logical, intent(in), optional :: required      !! Fatal when absent. Default `.true.`.
        logical, intent(out), optional :: found        !! Whether the section was there.

        !$omp critical (parquet_toml_guard)
        call section_impl(parent, name, 0, .false., sect, required, found)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_section_named

    !> `[[name]]` entry `idx`. See the `pf_toml_section` generic for what every argument means.
    subroutine pf_toml_section_indexed(parent, name, idx, sect, required, found)
        type(pf_toml), intent(in) :: parent            !! Document or section to look in.
        character(len=*), intent(in) :: name           !! Array-of-tables name, looked up literally.
        integer, intent(in) :: idx                     !! 1-based entry index.
        type(pf_toml), intent(out) :: sect             !! Receives the section handle.
        logical, intent(in), optional :: required
        !! Fatal when `[[name]]` is absent OR `idx` is outside `1 .. count`. Default `.true.`;
        !! with `.false.` either absence leaves `sect` closed instead.
        logical, intent(out), optional :: found        !! Whether that entry was there.

        !$omp critical (parquet_toml_guard)
        call section_impl(parent, name, idx, .true., sect, required, found)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_section_indexed

    !> How many `[[name]]` entries there are: `0` when the name is absent.
    !!
    !! That zero is what makes `do i = 1, pf_toml_section_count(conf, "sregion")` the whole idiom
    !! for an optional repeated section. A `name` that is a plain `[name]` table, or a value, or a
    !! list of values, is **fatal** rather than reported as zero -- asking how many entries a
    !! single table has is a programming error, and answering it would hide the mistake.
    integer function pf_toml_section_count(parent, name) result(n)
        type(pf_toml), intent(in) :: parent      !! Document or section to look in.
        character(len=*), intent(in) :: name     !! Array-of-tables name.

        n = 0
        !$omp critical (parquet_toml_guard)
        n = count_impl(parent, name)
        !$omp end critical (parquet_toml_guard)
    end function pf_toml_section_count

    !> Whether `[name]` or `[[name]]` is present, without opening it or marking it read.
    !!
    !! It marks nothing, so a program that uses it to decide whether to open a section still gets
    !! an accurate `pf_toml_check_all`.
    logical function pf_toml_has_section(parent, name) result(yes)
        type(pf_toml), intent(in) :: parent      !! Document or section to look in.
        character(len=*), intent(in) :: name     !! Section name.

        yes = .false.
        !$omp critical (parquet_toml_guard)
        yes = has_section_impl(parent, name)
        !$omp end critical (parquet_toml_guard)
    end function pf_toml_has_section

    ! ================================================================================
    ! Reading values
    !
    ! Every specific below takes the module guard INLINE rather than delegating to a worker, and
    ! every one of their bodies is a single if/else-if/else with no RETURN in it. That is not a
    ! style choice: OpenMP makes branching out of a `critical` region non-conforming, so a `return`
    ! inside one is a defect no compiler here is obliged to diagnose. The failure paths all end in
    ! pf_log_fatal, which terminates rather than branching, so they are safe. The branchier
    ! procedures elsewhere in this module use the wrapper-plus-worker shape instead, where the
    ! worker may return freely because the critical belongs to its caller.
    ! ================================================================================

    !> `pf_toml_get` for an `integer(int32)` scalar.
    subroutine pf_toml_get_i32(sect, key, value, default)
        type(pf_toml), intent(in) :: sect                 !! Handle to read from.
        character(len=*), intent(in) :: key               !! Key to read.
        integer(int32), intent(out) :: value              !! Receives the value.
        integer(int32), intent(in), optional :: default   !! Applied when the key is absent.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a whole number")
        else if (present(default)) then
            value = default
        else
            call fail_missing(sect, key)
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_i32

    !> `pf_toml_get` for an `integer(int64)` scalar.
    subroutine pf_toml_get_i64(sect, key, value, default)
        type(pf_toml), intent(in) :: sect                 !! Handle to read from.
        character(len=*), intent(in) :: key               !! Key to read.
        integer(int64), intent(out) :: value              !! Receives the value.
        integer(int64), intent(in), optional :: default   !! Applied when the key is absent.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a whole number")
        else if (present(default)) then
            value = default
        else
            call fail_missing(sect, key)
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_i64

    !> `pf_toml_get` for a `real(real32)` scalar. A TOML integer is accepted and converted.
    subroutine pf_toml_get_r32(sect, key, value, default)
        type(pf_toml), intent(in) :: sect               !! Handle to read from.
        character(len=*), intent(in) :: key             !! Key to read.
        real(real32), intent(out) :: value              !! Receives the value.
        real(real32), intent(in), optional :: default   !! Applied when the key is absent.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a number")
        else if (present(default)) then
            value = default
        else
            call fail_missing(sect, key)
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_r32

    !> `pf_toml_get` for a `real(real64)` scalar. A TOML integer is accepted and converted.
    subroutine pf_toml_get_r64(sect, key, value, default)
        type(pf_toml), intent(in) :: sect               !! Handle to read from.
        character(len=*), intent(in) :: key             !! Key to read.
        real(real64), intent(out) :: value              !! Receives the value.
        real(real64), intent(in), optional :: default   !! Applied when the key is absent.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a number")
        else if (present(default)) then
            value = default
        else
            call fail_missing(sect, key)
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_r64

    !> `pf_toml_get` for a `logical` scalar. TOML spells these `true`/`false`; `T` and `F` are
    !> strings, so a file still using them fails here rather than quietly reading as false.
    subroutine pf_toml_get_log(sect, key, value, default)
        type(pf_toml), intent(in) :: sect            !! Handle to read from.
        character(len=*), intent(in) :: key          !! Key to read.
        logical, intent(out) :: value                !! Receives the value.
        logical, intent(in), optional :: default     !! Applied when the key is absent.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "true or false")
        else if (present(default)) then
            value = default
        else
            call fail_missing(sect, key)
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_log

    !> `pf_toml_get` for a deferred-length `character` scalar, allocated to the value's own length.
    subroutine pf_toml_get_str(sect, key, value, default)
        type(pf_toml), intent(in) :: sect                       !! Handle to read from.
        character(len=*), intent(in) :: key                     !! Key to read.
        character(len=:), allocatable, intent(out) :: value     !! Receives the value.
        character(len=*), intent(in), optional :: default       !! Applied when the key is absent.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a string")
        else if (present(default)) then
            value = default
        else
            call fail_missing(sect, key)
        end if
        if (.not. allocated(value)) value = ""
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_str

    !> `pf_toml_get` for an `integer(int32)` array of a length the caller fixes.
    subroutine pf_toml_get_i32_arr(sect, key, values, default)
        type(pf_toml), intent(in) :: sect                  !! Handle to read from.
        character(len=*), intent(in) :: key                !! Key to read.
        integer(int32), intent(out) :: values(:)           !! Receives the list.
        integer(int32), intent(in), optional :: default(:) !! Applied when the key is absent.
        type(toml_array), pointer :: arr
        integer(int32), allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of whole numbers")
            values = tmp
        else if (present(default)) then
            call check_default_size(sect, key, size(default), size(values))
            values = default
        else
            call fail_missing(sect, key)
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_i32_arr

    !> `pf_toml_get` for an `integer(int64)` array of a length the caller fixes.
    subroutine pf_toml_get_i64_arr(sect, key, values, default)
        type(pf_toml), intent(in) :: sect                  !! Handle to read from.
        character(len=*), intent(in) :: key                !! Key to read.
        integer(int64), intent(out) :: values(:)           !! Receives the list.
        integer(int64), intent(in), optional :: default(:) !! Applied when the key is absent.
        type(toml_array), pointer :: arr
        integer(int64), allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of whole numbers")
            values = tmp
        else if (present(default)) then
            call check_default_size(sect, key, size(default), size(values))
            values = default
        else
            call fail_missing(sect, key)
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_i64_arr

    !> `pf_toml_get` for a `real(real32)` array of a length the caller fixes.
    subroutine pf_toml_get_r32_arr(sect, key, values, default)
        type(pf_toml), intent(in) :: sect                !! Handle to read from.
        character(len=*), intent(in) :: key              !! Key to read.
        real(real32), intent(out) :: values(:)           !! Receives the list.
        real(real32), intent(in), optional :: default(:) !! Applied when the key is absent.
        type(toml_array), pointer :: arr
        real(real32), allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of numbers")
            values = tmp
        else if (present(default)) then
            call check_default_size(sect, key, size(default), size(values))
            values = default
        else
            call fail_missing(sect, key)
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_r32_arr

    !> `pf_toml_get` for a `real(real64)` array of a length the caller fixes.
    subroutine pf_toml_get_r64_arr(sect, key, values, default)
        type(pf_toml), intent(in) :: sect                !! Handle to read from.
        character(len=*), intent(in) :: key              !! Key to read.
        real(real64), intent(out) :: values(:)           !! Receives the list.
        real(real64), intent(in), optional :: default(:) !! Applied when the key is absent.
        type(toml_array), pointer :: arr
        real(real64), allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of numbers")
            values = tmp
        else if (present(default)) then
            call check_default_size(sect, key, size(default), size(values))
            values = default
        else
            call fail_missing(sect, key)
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_r64_arr

    !> `pf_toml_get` for a `logical` array of a length the caller fixes.
    subroutine pf_toml_get_log_arr(sect, key, values, default)
        type(pf_toml), intent(in) :: sect           !! Handle to read from.
        character(len=*), intent(in) :: key         !! Key to read.
        logical, intent(out) :: values(:)           !! Receives the list.
        logical, intent(in), optional :: default(:) !! Applied when the key is absent.
        type(toml_array), pointer :: arr
        logical, allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of true/false values")
            values = tmp
        else if (present(default)) then
            call check_default_size(sect, key, size(default), size(values))
            values = default
        else
            call fail_missing(sect, key)
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_log_arr

    !> `pf_toml_get` for a `character` array of a length the caller fixes.
    !!
    !! toml-f has no whole-array string getter, so this reads element by element. A value longer
    !! than `len(values)` is fatal rather than truncated -- a silently clipped file name is the
    !! worst outcome available here -- and an over-long `default` element is refused for the same
    !! reason.
    subroutine pf_toml_get_str_arr(sect, key, values, default)
        type(pf_toml), intent(in) :: sect                    !! Handle to read from.
        character(len=*), intent(in) :: key                  !! Key to read.
        character(len=*), intent(out) :: values(:)           !! Receives the list; blank padded.
        character(len=*), intent(in), optional :: default(:) !! Applied when the key is absent.
        type(toml_array), pointer :: arr, sarr
        character(len=:), allocatable :: one
        integer :: stat, origin, i
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        call shadow_new_array(sect, key, sarr)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            do i = 1, size(values)
                call get_value(arr, i, one, stat=stat)
                if (stat /= toml_stat%success .or. .not. allocated(one)) &
                    call fail_value(sect, key, origin, stat, "a list of strings")
                if (len(one) > len(values)) call fail_too_long(sect, key, origin, i, len(one), len(values))
                values(i) = one
                if (associated(sarr)) call set_value(sarr, i, one)
            end do
        else if (present(default)) then
            call check_default_size(sect, key, size(default), size(values))
            do i = 1, size(values)
                if (len_trim(default(i)) > len(values)) call fail_too_long(sect, key, sect%tbl%origin, i, &
                    len_trim(default(i)), len(values))
                values(i) = default(i)
                if (associated(sarr)) call set_value(sarr, i, trim(default(i)))
            end do
        else
            call fail_missing(sect, key)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_str_arr

    !> `pf_toml_get_alloc` for an `integer(int32)` list sized by the file.
    subroutine pf_toml_get_alloc_i32(sect, key, values)
        type(pf_toml), intent(in) :: sect                     !! Handle to read from.
        character(len=*), intent(in) :: key                   !! Key to read.
        integer(int32), allocatable, intent(out) :: values(:) !! Receives the list.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of whole numbers")
        else
            call fail_missing(sect, key)
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_i32

    !> `pf_toml_get_alloc` for an `integer(int64)` list sized by the file.
    subroutine pf_toml_get_alloc_i64(sect, key, values)
        type(pf_toml), intent(in) :: sect                     !! Handle to read from.
        character(len=*), intent(in) :: key                   !! Key to read.
        integer(int64), allocatable, intent(out) :: values(:) !! Receives the list.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of whole numbers")
        else
            call fail_missing(sect, key)
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_i64

    !> `pf_toml_get_alloc` for a `real(real32)` list sized by the file.
    subroutine pf_toml_get_alloc_r32(sect, key, values)
        type(pf_toml), intent(in) :: sect                   !! Handle to read from.
        character(len=*), intent(in) :: key                 !! Key to read.
        real(real32), allocatable, intent(out) :: values(:) !! Receives the list.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of numbers")
        else
            call fail_missing(sect, key)
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_r32

    !> `pf_toml_get_alloc` for a `real(real64)` list sized by the file.
    subroutine pf_toml_get_alloc_r64(sect, key, values)
        type(pf_toml), intent(in) :: sect                   !! Handle to read from.
        character(len=*), intent(in) :: key                 !! Key to read.
        real(real64), allocatable, intent(out) :: values(:) !! Receives the list.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of numbers")
        else
            call fail_missing(sect, key)
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_r64

    !> `pf_toml_get_alloc` for a `logical` list sized by the file.
    subroutine pf_toml_get_alloc_log(sect, key, values)
        type(pf_toml), intent(in) :: sect              !! Handle to read from.
        character(len=*), intent(in) :: key            !! Key to read.
        logical, allocatable, intent(out) :: values(:) !! Receives the list.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of true/false values")
        else
            call fail_missing(sect, key)
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_log

    !> Reads a list of strings, each keeping its own exact length.
    !!
    !! ```fortran
    !! type(pf_toml_strings) :: files
    !! character(len=:), allocatable :: one
    !! call pf_toml_get_strings(sect, "input_files", files)
    !! do i = 1, files%count()
    !!     call files%get(i, one)
    !! end do
    !! ```
    !!
    !! The key is required, like every other bare getter. For a list that may legitimately be
    !! absent, call `pf_toml_get_strings_opt`: it leaves `strings` exactly as it found it, and a
    !! freshly declared one answers `%count() == 0`, so the loop above is the whole idiom for an
    !! optional file list either way.
    !!
    !! `count` demands an exact number of entries and is fatal in **both** directions. That is what
    !! a list whose length must match some other setting needs -- three column names for three sky
    !! conditions is a correctness constraint, and a list of the wrong length pairs each name with
    !! the wrong condition. It is checked only when the file sets the key.
    !!
    !! The result copies its strings out of the document, so it stays valid after `pf_toml_close`.
    subroutine pf_toml_get_strings(sect, key, strings, count)
        type(pf_toml), intent(in) :: sect             !! Handle to read from.
        character(len=*), intent(in) :: key           !! Key to read.
        type(pf_toml_strings), intent(out) :: strings !! Receives the list.
        integer, intent(in), optional :: count        !! Exact number of entries the list must hold.
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call fill_strings(sect, key, strings, count)
        else
            call fail_missing(sect, key)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_strings

    !> Reads a log level by NAME and converts it to a `PF_LEVEL_*` value.
    !!
    !! The configuration file names the level -- `DEBUG`, `INFO`, `WARNING`, `ERROR`, `OFF`, and
    !! everything else `pf_log_level_from_name` accepts -- so the calling program holds no second
    !! scale and no lookup table of its own. `default` is a name too, not a number, so the accepted
    !! spellings are the same on both sides. An unrecognised name is fatal.
    !!
    !! `parquet_toml` deliberately does not re-export the `PF_LEVEL_*` constants: a program reading
    !! a level is a program configuring the logger, and it names `parquet_logging` for both.
    subroutine pf_toml_get_level(sect, key, level, default)
        type(pf_toml), intent(in) :: sect                     !! Handle to read from.
        character(len=*), intent(in) :: key                   !! Key to read.
        integer, intent(out) :: level                         !! Receives the `PF_LEVEL_*` value.
        character(len=*), intent(in), optional :: default     !! Level NAME used when the key is absent.
        character(len=:), allocatable :: name
        integer :: stat, origin
        logical :: in_file, ok

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        name = ""
        if (in_file) then
            call get_value(sect%tbl, key, name, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a log level name")
        else if (present(default)) then
            name = default
        else
            call fail_missing(sect, key)
        end if
        if (.not. allocated(name)) name = ""
        call pf_log_level_from_name(name, level, ok)
        if (.not. ok) call fail_level(sect, key, origin, name)
        if (associated(sect%shadow)) call set_value(sect%shadow, key, name)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_level

    !> `pf_toml_get_opt` for an `integer(int32)` scalar: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_i32(sect, key, value)
        type(pf_toml), intent(in) :: sect      !! Handle to read from.
        character(len=*), intent(in) :: key    !! Key to read.
        integer(int32), intent(inout) :: value !! Default in, file value out.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a whole number")
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_i32

    !> `pf_toml_get_opt` for an `integer(int64)` scalar: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_i64(sect, key, value)
        type(pf_toml), intent(in) :: sect      !! Handle to read from.
        character(len=*), intent(in) :: key    !! Key to read.
        integer(int64), intent(inout) :: value !! Default in, file value out.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a whole number")
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_i64

    !> `pf_toml_get_opt` for a `real(real32)` scalar: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_r32(sect, key, value)
        type(pf_toml), intent(in) :: sect    !! Handle to read from.
        character(len=*), intent(in) :: key  !! Key to read.
        real(real32), intent(inout) :: value !! Default in, file value out.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a number")
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_r32

    !> `pf_toml_get_opt` for a `real(real64)` scalar: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_r64(sect, key, value)
        type(pf_toml), intent(in) :: sect    !! Handle to read from.
        character(len=*), intent(in) :: key  !! Key to read.
        real(real64), intent(inout) :: value !! Default in, file value out.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a number")
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_r64

    !> `pf_toml_get_opt` for a `logical` scalar: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_log(sect, key, value)
        type(pf_toml), intent(in) :: sect   !! Handle to read from.
        character(len=*), intent(in) :: key !! Key to read.
        logical, intent(inout) :: value     !! Default in, file value out.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "true or false")
        end if
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_log

    !> `pf_toml_get_opt` for a deferred-length `character` scalar.
    !!
    !! A variable the caller left unallocated stays unallocated when the file does not set the key,
    !! so `allocated(value)` answers "does this value exist at all?" -- and nothing is recorded for
    !! `pf_toml_save`, because an absent value is not an empty string.
    subroutine pf_toml_get_opt_str(sect, key, value)
        type(pf_toml), intent(in) :: sect                     !! Handle to read from.
        character(len=*), intent(in) :: key                   !! Key to read.
        character(len=:), allocatable, intent(inout) :: value !! Default in, file value out.
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        origin = 0
        if (in_file) then
            call get_value(sect%tbl, key, value, stat=stat, origin=origin)
            if (stat /= toml_stat%success) call fail_value(sect, key, origin, stat, "a string")
            if (.not. allocated(value)) value = ""
        end if
        if (allocated(value)) then
            if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_str

    !> `pf_toml_get_opt` for an `integer(int32)` array: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_i32_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect          !! Handle to read from.
        character(len=*), intent(in) :: key        !! Key to read.
        integer(int32), intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        integer(int32), allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of whole numbers")
            values = tmp
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_i32_arr

    !> `pf_toml_get_opt` for an `integer(int64)` array: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_i64_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect          !! Handle to read from.
        character(len=*), intent(in) :: key        !! Key to read.
        integer(int64), intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        integer(int64), allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of whole numbers")
            values = tmp
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_i64_arr

    !> `pf_toml_get_opt` for a `real(real32)` array: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_r32_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect        !! Handle to read from.
        character(len=*), intent(in) :: key      !! Key to read.
        real(real32), intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        real(real32), allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of numbers")
            values = tmp
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_r32_arr

    !> `pf_toml_get_opt` for a `real(real64)` array: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_r64_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect        !! Handle to read from.
        character(len=*), intent(in) :: key      !! Key to read.
        real(real64), intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        real(real64), allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of numbers")
            values = tmp
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_r64_arr

    !> `pf_toml_get_opt` for a `logical` array: the variable stands when the key is absent.
    subroutine pf_toml_get_opt_log_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect   !! Handle to read from.
        character(len=*), intent(in) :: key !! Key to read.
        logical, intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        logical, allocatable :: tmp(:)
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            call get_value(arr, tmp, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(tmp)) &
                call fail_value(sect, key, origin, stat, "a list of true/false values")
            values = tmp
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_log_arr

    !> `pf_toml_get_opt` for a `character` array of a length the caller fixes.
    subroutine pf_toml_get_opt_str_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect            !! Handle to read from.
        character(len=*), intent(in) :: key          !! Key to read.
        character(len=*), intent(inout) :: values(:) !! Default in, file value out; blank padded.
        type(toml_array), pointer :: arr, sarr
        character(len=:), allocatable :: one
        integer :: stat, origin, i
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        call shadow_new_array(sect, key, sarr)
        if (in_file) then
            call open_list(sect, key, size(values), arr, origin)
            do i = 1, size(values)
                call get_value(arr, i, one, stat=stat)
                if (stat /= toml_stat%success .or. .not. allocated(one)) &
                    call fail_value(sect, key, origin, stat, "a list of strings")
                if (len(one) > len(values)) call fail_too_long(sect, key, origin, i, len(one), len(values))
                values(i) = one
                if (associated(sarr)) call set_value(sarr, i, one)
            end do
        else
            if (associated(sarr)) then
                do i = 1, size(values)
                    call set_value(sarr, i, trim(values(i)))
                end do
            end if
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_opt_str_arr

    !> `pf_toml_get_alloc_opt` for an `integer(int32)` list: the variable stands when the key is absent.
    subroutine pf_toml_get_alloc_opt_i32(sect, key, values)
        type(pf_toml), intent(in) :: sect                       !! Handle to read from.
        character(len=*), intent(in) :: key                     !! Key to read.
        integer(int32), allocatable, intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of whole numbers")
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_opt_i32

    !> `pf_toml_get_alloc_opt` for an `integer(int64)` list: the variable stands when the key is absent.
    subroutine pf_toml_get_alloc_opt_i64(sect, key, values)
        type(pf_toml), intent(in) :: sect                       !! Handle to read from.
        character(len=*), intent(in) :: key                     !! Key to read.
        integer(int64), allocatable, intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of whole numbers")
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_opt_i64

    !> `pf_toml_get_alloc_opt` for a `real(real32)` list: the variable stands when the key is absent.
    subroutine pf_toml_get_alloc_opt_r32(sect, key, values)
        type(pf_toml), intent(in) :: sect                     !! Handle to read from.
        character(len=*), intent(in) :: key                   !! Key to read.
        real(real32), allocatable, intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of numbers")
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_opt_r32

    !> `pf_toml_get_alloc_opt` for a `real(real64)` list: the variable stands when the key is absent.
    subroutine pf_toml_get_alloc_opt_r64(sect, key, values)
        type(pf_toml), intent(in) :: sect                     !! Handle to read from.
        character(len=*), intent(in) :: key                   !! Key to read.
        real(real64), allocatable, intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of numbers")
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_opt_r64

    !> `pf_toml_get_alloc_opt` for a `logical` list: the variable stands when the key is absent.
    subroutine pf_toml_get_alloc_opt_log(sect, key, values)
        type(pf_toml), intent(in) :: sect                !! Handle to read from.
        character(len=*), intent(in) :: key              !! Key to read.
        logical, allocatable, intent(inout) :: values(:) !! Default in, file value out.
        type(toml_array), pointer :: arr
        integer :: stat, origin
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call open_list(sect, key, -1, arr, origin)
            call get_value(arr, values, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(values)) &
                call fail_value(sect, key, origin, stat, "a list of true/false values")
        end if
        if (allocated(values)) then
            call shadow_new_array(sect, key, arr)
            if (associated(arr)) call set_value(arr, values)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_alloc_opt_log

    !> Reads a string list **if the file sets the key**, and otherwise leaves `strings` alone.
    !!
    !! The optional counterpart to `pf_toml_get_strings`; see that procedure for `count` and for
    !! what the result is. A freshly declared `strings` that the file does not set still answers
    !! `%count() == 0`, which is the same behaviour the older `required = .false.` form gave, and a
    !! `strings` that already holds a list keeps it -- which that form could not do.
    subroutine pf_toml_get_strings_opt(sect, key, strings, count)
        type(pf_toml), intent(in) :: sect               !! Handle to read from.
        character(len=*), intent(in) :: key             !! Key to read.
        type(pf_toml_strings), intent(inout) :: strings !! Default in, file value out.
        integer, intent(in), optional :: count          !! Exact number of entries the list must hold.
        logical :: in_file

        !$omp critical (parquet_toml_guard)
        call begin_read(sect, key, in_file)
        if (in_file) then
            call fill_strings(sect, key, strings, count)
        else
            call strings_to_shadow(sect, key, strings)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_get_strings_opt

    ! ---- pf_toml_strings' own bindings -------------------------------------------------------
    !
    ! These take NO guard, deliberately. The object is a self-contained copy that references no
    ! document and no module state, so there is nothing for concurrent callers to race on -- and a
    ! critical section inside %get would put a lock in the middle of a per-element loop.

    !> Copies element `idx` out at its exact length. An index out of range is fatal.
    subroutine strings_get(self, idx, value)
        class(pf_toml_strings), intent(in) :: self             !! The list.
        integer, intent(in) :: idx                             !! 1-based element index.
        character(len=:), allocatable, intent(out) :: value    !! Receives the element.
        integer :: j, n

        call strings_check_index(self, idx, "get")
        n = self%off(idx + 1) - self%off(idx)
        allocate(character(len=n) :: value)
        do j = 1, n
            value(j:j) = self%buf(self%off(idx) + j - 1)
        end do
    end subroutine strings_get

    !> How many elements the list holds. `0` for an absent optional key.
    integer function strings_count(self) result(n)
        class(pf_toml_strings), intent(in) :: self   !! The list.

        n = self%n
    end function strings_count

    !> Length of element `idx`, without copying it. An index out of range is fatal.
    integer function strings_length(self, idx) result(n)
        class(pf_toml_strings), intent(in) :: self   !! The list.
        integer, intent(in) :: idx                   !! 1-based element index.

        call strings_check_index(self, idx, "length")
        n = self%off(idx + 1) - self%off(idx)
    end function strings_length

    ! ================================================================================
    ! Validation
    ! ================================================================================

    !> Stops unless the section sets every key in a `;`-separated list.
    !!
    !! **Most programs need neither this nor a list**: a getter with no `default` and no
    !! `required = .false.` already stops on an absent key, at the point of the read, naming it.
    !! This exists for the one case that cannot work that way -- a file **somebody else owns**,
    !! which legitimately carries far more keys than you read, so an unknown-key sweep over it is
    !! meaningless while a required-key list is exactly right.
    !!
    !! Every missing key is reported before the run stops, not just the first: somebody bringing an
    !! old file forward is usually missing several, and stopping at the first turns that into as
    !! many runs as there are mistakes. Every listed key is marked read, so a later sweep does not
    !! report them.
    subroutine pf_toml_require(sect, keys)
        type(pf_toml), intent(in) :: sect       !! Section to check.
        character(len=*), intent(in) :: keys    !! Required key names, `;`-separated.

        !$omp critical (parquet_toml_guard)
        call require_impl(sect, keys)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_require

    !> Stops if a key this version has retired is still set, quoting `advice` verbatim.
    !!
    !! A retired key that is merely ignored is worse than one that is rejected: the run proceeds
    !! with a value written nowhere the user can see, and the file looks as though it still works.
    !! The key is marked read either way, so a sweep never double-reports it.
    subroutine pf_toml_retire(sect, key, advice)
        type(pf_toml), intent(in) :: sect        !! Section to check.
        character(len=*), intent(in) :: key      !! The retired key.
        character(len=*), intent(in) :: advice   !! What to do instead, quoted into the message.

        !$omp critical (parquet_toml_guard)
        call retire_impl(sect, key, advice)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_retire

    !> Reports every key this section carries that the program never asked for.
    !!
    !! This is the only thing that catches a misspelt **optional** key. A misspelt required key
    !! stops at the read; a misspelt optional one has no other symptom at all -- the reader simply
    !! does not find it, applies the default, and the run proceeds with a value the user believes
    !! they overrode.
    !!
    !! The list of keys the program asked for is accumulated automatically by every getter and
    !! every `pf_toml_section`, whether or not the key was in the file, so there is no known-key
    !! list to write down and none to keep in step with the reads.
    !!
    !! **It may be called at any point, including last.** Nothing here ever writes a default into
    !! the parsed document, so the document a sweep sees is the document the file described.
    !!
    !! `severity` is `PF_TOML_FATAL` (the default), `PF_TOML_WARN` or `PF_TOML_IGNORE`. Fatal is
    !! the default because calling this is already an opt-in act; a project that wants old files to
    !! keep running passes `PF_TOML_WARN`.
    subroutine pf_toml_check(sect, severity)
        type(pf_toml), intent(in) :: sect             !! Section to sweep.
        integer, intent(in), optional :: severity     !! How loud to be. Default `PF_TOML_FATAL`.

        !$omp critical (parquet_toml_guard)
        call check_one_impl(sect, severity)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_check

    !> Reports everything in the whole document the program never read.
    !!
    !! `pf_toml_check`'s sweep over **every** section that was opened, plus every section, every
    !! `[[name]]` entry and every root-level key that was **not**. One call at the end of a
    !! program's configuration reading is therefore enough, and it catches the one-level-up form of
    !! the misspelt-key failure: `[sregion_al]` for `[sregion_all]` is otherwise a silent no-op
    !! that leaves every sky region on its built-in default.
    !!
    !! The per-section `pf_toml_check` stays for finer-grained severity -- warn about one section's
    !! extra keys while another's are fatal.
    subroutine pf_toml_check_all(doc, severity)
        type(pf_toml), intent(in) :: doc              !! Document handle, as an open left it.
        integer, intent(in), optional :: severity     !! How loud to be. Default `PF_TOML_FATAL`.

        !$omp critical (parquet_toml_guard)
        call check_all_impl(doc, severity)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_check_all

    !> Records a key as read, for a program that read it some other way.
    !!
    !! The companion to the two escape hatches: code that reaches into the raw `toml_table` tells
    !! the accumulator what it took, so `pf_toml_check` and `pf_toml_check_all` stay accurate.
    subroutine pf_toml_mark(sect, key)
        type(pf_toml), intent(in) :: sect      !! Section the key belongs to.
        character(len=*), intent(in) :: key    !! Key to record as read.

        !$omp critical (parquet_toml_guard)
        call mark_impl(sect, key)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_mark

    !> Records a whole section as understood, so neither sweep reports anything inside it.
    !!
    !! The section-level companion to `pf_toml_mark`, for a section a program deliberately does not
    !! read: a `[developer]` block whose keys are only read when a developer mode is on, say. The
    !! alternative is to read every key unconditionally and re-apply the defaults by hand, which
    !! rebuilds exactly the hand-maintained key list this module exists to delete.
    !!
    !! **It marks recursively** -- every key, every sub-table and every `[[name]]` entry below this
    !! section -- because "this section is understood" is a statement about the section, not about
    !! its first level. Shallow marking would leave `pf_toml_check_all` reporting
    !! `[developer.advanced]` as a section nobody read, which is a partial report about a section
    !! the caller has just excused. The cost is that a misspelt *nested* section name inside a
    !! marked section is silenced too.
    !!
    !! **It is silent on a section the file does not have**, unlike a getter: whether an optional
    !! section is present is not known before it is opened, so a caller that means "ignore this
    !! section if it is there" cannot be asked to test first. The same holds for `pf_toml_check`,
    !! `pf_toml_retire`, `pf_toml_mark` and `pf_toml_section_count`; a *value* read from a closed
    !! handle is still fatal.
    !!
    !! Marking is not reading. A marked section's keys never reach the effective document, so
    !! `pf_toml_save` does not write them -- which is right, because the program did not use them.
    subroutine pf_toml_mark_section(sect)
        type(pf_toml), intent(in) :: sect      !! Section to record as understood.

        !$omp critical (parquet_toml_guard)
        call mark_section_impl(sect)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_mark_section

    ! ================================================================================
    ! Presence, escape hatches and helpers
    ! ================================================================================

    !> Whether the section carries `key`, without reading it or marking it read.
    logical function pf_toml_has(sect, key) result(yes)
        type(pf_toml), intent(in) :: sect      !! Section to look in.
        character(len=*), intent(in) :: key    !! Key to look for.

        yes = .false.
        !$omp critical (parquet_toml_guard)
        if (associated(sect%tbl)) yes = sect%tbl%has_key(key)
        !$omp end critical (parquet_toml_guard)
    end function pf_toml_has

    !> Whether this handle refers to a table at all.
    !!
    !! `.false.` for an optional section that was not found, and for a handle no open has filled.
    !! Reading from a closed handle is fatal, so this is how the reads after an optional
    !! `pf_toml_section` are guarded.
    logical function pf_toml_is_open(sect) result(yes)
        type(pf_toml), intent(in) :: sect   !! The handle.

        yes = associated(sect%tbl)
    end function pf_toml_is_open

    !> Every key this section carries, in file order.
    !!
    !! Blank-padded to `PF_TOML_MAX_KEY` so that no toml-f type has to be imported to enumerate a
    !! section. A closed handle yields a zero-length result rather than an error.
    subroutine pf_toml_keys(sect, keys)
        type(pf_toml), intent(in) :: sect                                   !! Section to enumerate.
        character(len=PF_TOML_MAX_KEY), allocatable, intent(out) :: keys(:) !! Receives the key names.

        !$omp critical (parquet_toml_guard)
        call keys_impl(sect, keys)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_keys

    !> This handle's display path: empty at the root, `general`, `general.limits`, `sregion[3]`.
    subroutine pf_toml_path(sect, path)
        type(pf_toml), intent(in) :: sect                     !! The handle.
        character(len=:), allocatable, intent(out) :: path    !! Receives the path.

        !$omp critical (parquet_toml_guard)
        path = trim(sect%path)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_path

    !> The file name behind any handle, document or section.
    !!
    !! `<string>` for a `pf_toml_loads` document given no name, `<new>` for a `pf_toml_new` one,
    !! and `<closed>` for a handle no open has filled.
    subroutine pf_toml_filename(handle, name)
        type(pf_toml), intent(in) :: handle                   !! The handle.
        character(len=:), allocatable, intent(out) :: name    !! Receives the file name.

        !$omp critical (parquet_toml_guard)
        if (associated(handle%doc)) then
            name = handle%doc%file
        else
            name = "<closed>"
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_filename

    !> ESCAPE HATCH: the raw toml-f table behind this handle.
    !!
    !! For a TOML construct this wrapper does not cover -- a datetime, a deeply nested inline
    !! table, `merge_table`. The caller declares its own pointer, which needs its own
    !! `use tomlf, only: toml_table` line: that import is the point at which you have stepped
    !! outside this module, and it is deliberately not hidden.
    !!
    !! Tell `pf_toml_mark` about anything you read this way, or the unknown-key sweep will report
    !! it. The pointer borrows from the document and dies with it, like every other handle here.
    subroutine pf_toml_table(sect, tbl)
        type(pf_toml), intent(in) :: sect                      !! The handle.
        type(toml_table), pointer, intent(out) :: tbl          !! Receives the raw table, or null.

        !$omp critical (parquet_toml_guard)
        tbl => sect%tbl
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_table

    !> ESCAPE HATCH: the raw toml-f token context behind this handle's document.
    !!
    !! The companion to `pf_toml_table`, for rendering a diagnostic of your own against the source.
    !! `pf_toml_report` does that for the common case without needing this.
    subroutine pf_toml_context(sect, ctx)
        type(pf_toml), intent(in) :: sect                       !! The handle.
        type(toml_context), pointer, intent(out) :: ctx         !! Receives the context, or null.

        !$omp critical (parquet_toml_guard)
        if (associated(sect%doc)) then
            ctx => sect%doc%ctx
        else
            nullify(ctx)
        end if
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_context

    !> Logs a message of your own, pointing at a key's line in the configuration file.
    !!
    !! For a complaint this module cannot know about -- "n_sky_conditions must be 4", "these two
    !! files must have the same number of entries" -- so that a validation message written by the
    !! calling program still shows the reader the line to edit.
    !!
    !! `severity` is `PF_TOML_FATAL` (the default), `PF_TOML_WARN` or `PF_TOML_IGNORE`. A key that
    !! is not in the file is reported without a source excerpt rather than refused.
    subroutine pf_toml_report(sect, key, message, severity)
        type(pf_toml), intent(in) :: sect             !! Section the key belongs to.
        character(len=*), intent(in) :: key           !! Key to point at.
        character(len=*), intent(in) :: message       !! What to say about it.
        integer, intent(in), optional :: severity     !! How loud to be. Default `PF_TOML_FATAL`.

        !$omp critical (parquet_toml_guard)
        call report_impl(sect, key, message, severity)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_report

    ! ================================================================================
    ! Writing
    ! ================================================================================

    !> Adds `[name]` under `parent`, or returns the handle of the one already there.
    !!
    !! This is the one place in the write side that upserts, and the asymmetry with
    !! `pf_toml_set`/`pf_toml_update` is deliberate: a section is a place to put keys rather than a
    !! value to get wrong, so handing back the existing one cannot silently overwrite anything. It
    !! works on a loaded document as well as a new one -- adding a section the file did not have.
    !! A `name` that is already a value, or an array, is fatal.
    subroutine pf_toml_new_section(parent, name, sect)
        type(pf_toml), intent(in) :: parent      !! Document or section to add to.
        character(len=*), intent(in) :: name     !! Section name.
        type(pf_toml), intent(out) :: sect       !! Receives the section handle.

        !$omp critical (parquet_toml_guard)
        call new_section_impl(parent, name, sect)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_new_section

    !> Appends one `[[name]]` entry under `parent` and returns its handle.
    !!
    !! Creates the array on first use. The new handle's display path is `name[k]` with `k` its
    !! 1-based index -- the same rendering `pf_toml_section(parent, name, idx, sect)` produces, so
    !! a path in a message means the same thing whichever side made it. A `name` that is already a
    !! plain `[name]` table, or a value, or a list of values, is fatal.
    subroutine pf_toml_append_section(parent, name, sect)
        type(pf_toml), intent(in) :: parent      !! Document or section to add to.
        character(len=*), intent(in) :: name     !! Array-of-tables name.
        type(pf_toml), intent(out) :: sect       !! Receives the new entry's handle.

        !$omp critical (parquet_toml_guard)
        call append_section_impl(parent, name, sect)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_append_section

    !> Writes the document out as the program understands it, defaults made explicit.
    !!
    !! **This writes the EFFECTIVE configuration, not a copy of the input file.** Every getter
    !! records the value it resolved -- the file's, or the default it applied -- so a saved file
    !! states what the run actually used, which the input file does not: the input does not record
    !! what the defaults were on the day it ran.
    !!
    !! Two consequences follow, and both are the point rather than side effects. A key the file
    !! sets but the program never read is **absent** from the saved file. And a key the program
    !! read from its default is **present**, with that default's value. For a verbatim round trip
    !! of the parsed document, dump it yourself through `pf_toml_table`.
    !!
    !! An existing file is overwritten.
    subroutine pf_toml_save(doc, file)
        type(pf_toml), intent(in) :: doc         !! Document handle, as an open left it.
        character(len=*), intent(in) :: file     !! Path to write to.

        !$omp critical (parquet_toml_guard)
        call save_impl(doc, file)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_save

    !> Writes the document **as parsed**, plus every `pf_toml_set`/`update`/`delete` applied.
    !!
    !! The companion to `pf_toml_save`, and the pair is the whole point of having two names.
    !! `pf_toml_save` writes the *effective* configuration -- what the program resolved, defaults
    !! made explicit, keys nobody read left out. This writes the *document* -- everything the file
    !! carried, whether or not the program read it, plus any typed edits made since. Loading a
    !! file, editing one key and writing it back out is this one; recording what a run actually
    !! used is the other.
    !!
    !! **Neither is a verbatim copy, and this one is not either.** toml-f's serialiser writes
    !! values rather than source, so comments are dropped and the key order and formatting are
    !! toml-f's. For a file people hand-edit and keep comments in, copy the file.
    !!
    !! Pass the document handle, not a section; an existing file is overwritten.
    subroutine pf_toml_dump(doc, file)
        type(pf_toml), intent(in) :: doc         !! Document handle, as an open left it.
        character(len=*), intent(in) :: file     !! Path to write to.

        !$omp critical (parquet_toml_guard)
        call dump_impl(doc, file)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_dump

    !> REMOVES a key. Does nothing if it is not there.
    !!
    !! ```fortran
    !! call pf_toml_delete(sect, key)
    !! ```
    !!
    !! The third member of the write trio, and the one that does **not** refuse the other state.
    !! `pf_toml_set` refuses a key that exists and `pf_toml_update` refuses one that does not,
    !! because each catches a caller who is wrong about the document: setting an existing key would
    !! overwrite a value the caller thinks it is introducing, and updating a missing one would
    !! introduce a value the caller thinks it is changing. Deleting is **idempotent** -- afterwards
    !! the key is not there, whether or not it was -- so there is no wrong outcome for a guard to
    !! prevent, and "remove this key if the file happens to set it" is one line.
    !!
    !! The key is removed from the parsed document *and* from the effective one, so a later
    !! `pf_toml_save` does not write back a value an earlier `pf_toml_get` had resolved. It is also
    !! recorded as dealt with, so a sweep does not then report it as unknown.
    subroutine pf_toml_delete(sect, key)
        type(pf_toml), intent(in) :: sect        !! Handle to remove the key from.
        character(len=*), intent(in) :: key      !! Key to remove.

        !$omp critical (parquet_toml_guard)
        call delete_impl(sect, key)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_delete

    !> `pf_toml_set` for an `integer(int32)` scalar.
    subroutine pf_toml_set_i32(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to add.
        integer(int32), intent(in) :: value      !! Value to store.
        !$omp critical (parquet_toml_guard)
        call write_i32(sect, key, value, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_i32

    !> `pf_toml_update` for an `integer(int32)` scalar.
    subroutine pf_toml_update_i32(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to change.
        integer(int32), intent(in) :: value      !! New value.
        !$omp critical (parquet_toml_guard)
        call write_i32(sect, key, value, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_i32

    !> `pf_toml_set` for an `integer(int64)` scalar.
    subroutine pf_toml_set_i64(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to add.
        integer(int64), intent(in) :: value      !! Value to store.
        !$omp critical (parquet_toml_guard)
        call write_i64(sect, key, value, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_i64

    !> `pf_toml_update` for an `integer(int64)` scalar.
    subroutine pf_toml_update_i64(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to change.
        integer(int64), intent(in) :: value      !! New value.
        !$omp critical (parquet_toml_guard)
        call write_i64(sect, key, value, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_i64

    !> `pf_toml_set` for a `real(real32)` scalar.
    subroutine pf_toml_set_r32(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to add.
        real(real32), intent(in) :: value        !! Value to store.
        !$omp critical (parquet_toml_guard)
        call write_r32(sect, key, value, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_r32

    !> `pf_toml_update` for a `real(real32)` scalar.
    subroutine pf_toml_update_r32(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to change.
        real(real32), intent(in) :: value        !! New value.
        !$omp critical (parquet_toml_guard)
        call write_r32(sect, key, value, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_r32

    !> `pf_toml_set` for a `real(real64)` scalar.
    subroutine pf_toml_set_r64(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to add.
        real(real64), intent(in) :: value        !! Value to store.
        !$omp critical (parquet_toml_guard)
        call write_r64(sect, key, value, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_r64

    !> `pf_toml_update` for a `real(real64)` scalar.
    subroutine pf_toml_update_r64(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to change.
        real(real64), intent(in) :: value        !! New value.
        !$omp critical (parquet_toml_guard)
        call write_r64(sect, key, value, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_r64

    !> `pf_toml_set` for a `logical` scalar.
    subroutine pf_toml_set_log(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to add.
        logical, intent(in) :: value             !! Value to store.
        !$omp critical (parquet_toml_guard)
        call write_log(sect, key, value, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_log

    !> `pf_toml_update` for a `logical` scalar.
    subroutine pf_toml_update_log(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to change.
        logical, intent(in) :: value             !! New value.
        !$omp critical (parquet_toml_guard)
        call write_log(sect, key, value, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_log

    !> `pf_toml_set` for a `character` scalar. Trailing blanks are trimmed.
    subroutine pf_toml_set_str(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to add.
        character(len=*), intent(in) :: value    !! Value to store, trailing blanks trimmed.
        !$omp critical (parquet_toml_guard)
        call write_str(sect, key, value, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_str

    !> `pf_toml_update` for a `character` scalar. Trailing blanks are trimmed.
    subroutine pf_toml_update_str(sect, key, value)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to change.
        character(len=*), intent(in) :: value    !! New value, trailing blanks trimmed.
        !$omp critical (parquet_toml_guard)
        call write_str(sect, key, value, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_str

    !> `pf_toml_set` for an `integer(int32)` array.
    subroutine pf_toml_set_i32_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect          !! Handle to write into.
        character(len=*), intent(in) :: key        !! Key to add.
        integer(int32), intent(in) :: values(:)    !! Values to store.
        !$omp critical (parquet_toml_guard)
        call write_i32_arr(sect, key, values, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_i32_arr

    !> `pf_toml_update` for an `integer(int32)` array.
    subroutine pf_toml_update_i32_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect          !! Handle to write into.
        character(len=*), intent(in) :: key        !! Key to change.
        integer(int32), intent(in) :: values(:)    !! New values.
        !$omp critical (parquet_toml_guard)
        call write_i32_arr(sect, key, values, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_i32_arr

    !> `pf_toml_set` for an `integer(int64)` array.
    subroutine pf_toml_set_i64_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect          !! Handle to write into.
        character(len=*), intent(in) :: key        !! Key to add.
        integer(int64), intent(in) :: values(:)    !! Values to store.
        !$omp critical (parquet_toml_guard)
        call write_i64_arr(sect, key, values, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_i64_arr

    !> `pf_toml_update` for an `integer(int64)` array.
    subroutine pf_toml_update_i64_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect          !! Handle to write into.
        character(len=*), intent(in) :: key        !! Key to change.
        integer(int64), intent(in) :: values(:)    !! New values.
        !$omp critical (parquet_toml_guard)
        call write_i64_arr(sect, key, values, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_i64_arr

    !> `pf_toml_set` for a `real(real32)` array.
    subroutine pf_toml_set_r32_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to add.
        real(real32), intent(in) :: values(:)    !! Values to store.
        !$omp critical (parquet_toml_guard)
        call write_r32_arr(sect, key, values, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_r32_arr

    !> `pf_toml_update` for a `real(real32)` array.
    subroutine pf_toml_update_r32_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to change.
        real(real32), intent(in) :: values(:)    !! New values.
        !$omp critical (parquet_toml_guard)
        call write_r32_arr(sect, key, values, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_r32_arr

    !> `pf_toml_set` for a `real(real64)` array.
    subroutine pf_toml_set_r64_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to add.
        real(real64), intent(in) :: values(:)    !! Values to store.
        !$omp critical (parquet_toml_guard)
        call write_r64_arr(sect, key, values, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_r64_arr

    !> `pf_toml_update` for a `real(real64)` array.
    subroutine pf_toml_update_r64_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to change.
        real(real64), intent(in) :: values(:)    !! New values.
        !$omp critical (parquet_toml_guard)
        call write_r64_arr(sect, key, values, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_r64_arr

    !> `pf_toml_set` for a `logical` array.
    subroutine pf_toml_set_log_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to add.
        logical, intent(in) :: values(:)         !! Values to store.
        !$omp critical (parquet_toml_guard)
        call write_log_arr(sect, key, values, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_log_arr

    !> `pf_toml_update` for a `logical` array.
    subroutine pf_toml_update_log_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key to change.
        logical, intent(in) :: values(:)         !! New values.
        !$omp critical (parquet_toml_guard)
        call write_log_arr(sect, key, values, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_log_arr

    !> `pf_toml_set` for a `character` array. Every element's trailing blanks are trimmed.
    subroutine pf_toml_set_str_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect           !! Handle to write into.
        character(len=*), intent(in) :: key         !! Key to add.
        character(len=*), intent(in) :: values(:)   !! Values to store, each trimmed.
        !$omp critical (parquet_toml_guard)
        call write_str_arr(sect, key, values, .true.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_set_str_arr

    !> `pf_toml_update` for a `character` array. Every element's trailing blanks are trimmed.
    subroutine pf_toml_update_str_arr(sect, key, values)
        type(pf_toml), intent(in) :: sect           !! Handle to write into.
        character(len=*), intent(in) :: key         !! Key to change.
        character(len=*), intent(in) :: values(:)   !! New values, each trimmed.
        !$omp critical (parquet_toml_guard)
        call write_str_arr(sect, key, values, .false.)
        !$omp end critical (parquet_toml_guard)
    end subroutine pf_toml_update_str_arr

    ! ================================================================================
    ! Private workers
    !
    ! NONE of these takes the module guard. Every one is reached from a public entry that already
    ! holds it, and a named critical section is NOT reentrant -- a worker that took the guard again
    ! would deadlock against itself, on one thread, with no diagnostic. They may therefore `return`
    ! freely, which is the other half of why the branchy entry points delegate here.
    ! ================================================================================

    !> Parses a file or a string into `doc`, or reports the failure. See `pf_toml_load`.
    subroutine load_impl(doc, source, name, from_file, status)
        type(pf_toml), intent(inout) :: doc          !! Receives the document handle.
        character(len=*), intent(in) :: source       !! File path, or the TOML text itself.
        character(len=*), intent(in) :: name         !! Display name for a string source; may be blank.
        logical, intent(in) :: from_file             !! Whether `source` is a path or the text.
        integer, intent(out), optional :: status     !! When present, failures are soft.
        type(toml_error), allocatable :: terr
        character(len=:), allocatable :: label
        character(len=256) :: iom
        integer :: unit, ios
        logical :: soft

        soft = present(status)
        if (soft) status = PF_TOML_OK

        if (from_file) then
            label = source
        else if (len_trim(name) > 0) then
            label = trim(name)
        else
            label = "<string>"
        end if

        allocate(doc%doc)
        doc%doc%file = label

        if (from_file) then
            ! Probe the file HERE rather than reading toml-f's message. toml-f reports "cannot
            ! open" and "not valid TOML" through one toml_error carrying the same stat, so the two
            ! are distinguishable only by message text -- and a status code decided by matching
            ! English is a status code that breaks on the next release.
            iom = ""
            open(newunit=unit, file=source, status="old", action="read", iostat=ios, iomsg=iom)
            if (ios /= 0) then
                deallocate(doc%doc)
                if (soft) then
                    status = PF_TOML_ERR_OPEN
                else
                    call pf_log_error("Cannot open configuration file: " // label)
                    call pf_log_error("... " // trim(iom))
                    call pf_log_fatal("ERR: cannot open configuration file: " // label)
                end if
                return
            end if
            close(unit)
            call toml_load(doc%doc%root, source, config=toml_parser_config(context_detail=1), &
                           context=doc%doc%ctx, error=terr)
        else
            call toml_loads(doc%doc%root, source, config=toml_parser_config(context_detail=1), &
                            context=doc%doc%ctx, error=terr)
        end if

        if (allocated(terr) .or. .not. allocated(doc%doc%root)) then
            if (.not. soft) then
                call pf_log_error("Cannot parse configuration file: " // label)
                if (allocated(terr)) call log_block(terr%message)
                call pf_log_fatal("ERR: configuration file is not valid TOML: " // label)
            end if
            deallocate(doc%doc)
            status = PF_TOML_ERR_PARSE
            return
        end if

        call finish_open(doc)
    end subroutine load_impl

    !> Builds an empty document. See `pf_toml_new`.
    !!
    !! (Coverage note: the `allocate` below never registers as "hit" in gcov even though the
    !! procedure header and every executable line after it do -- measured 4 runs on lines 2281,
    !! 2286 and 2291-2294 against `#####` on this one, with no branch between the header and it,
    !! so it cannot have been skipped. `allocate` of a pointer to a type carrying allocatable and
    !! default-initialised components emits an out-of-line initialisation block that gcov
    !! attributes elsewhere; the neighbouring `allocate(doc%doc%root)`, whose target has no such
    !! components, is attributed normally. Excluded as an artifact, not as a gap;
    !! `.claude/rules/coverage.md` carries the shape.)
    subroutine new_impl(doc, name)
        type(pf_toml), intent(inout) :: doc      !! Receives the document handle.
        character(len=*), intent(in) :: name     !! Display name; may be blank.

        allocate(doc%doc) ! GCOVR_EXCL_LINE -- gcov attribution artifact
        if (len_trim(name) > 0) then
            doc%doc%file = trim(name)
        else
            doc%doc%file = "<new>"
        end if
        allocate(doc%doc%root)
        call new_table(doc%doc%root)
        call finish_open(doc)
    end subroutine new_impl

    !> Completes an open: allocates the shadow and the accumulator, and points the handle at both.
    subroutine finish_open(doc)
        type(pf_toml), intent(inout) :: doc   !! The handle being opened.

        allocate(doc%doc%shadow)
        call new_table(doc%doc%shadow)
        allocate(doc%doc%seen(32))
        doc%doc%nseen = 0
        doc%tbl => doc%doc%root
        doc%shadow => doc%doc%shadow
        doc%path = ''
        doc%owner = .true.
    end subroutine finish_open

    !> Releases the document. See `pf_toml_close`.
    subroutine close_impl(doc)
        type(pf_toml), intent(inout) :: doc   !! The owning handle.

        if (.not. doc%owner) then
            ! A handle no open ever filled is closed already: saying so would make a defensive
            ! `call pf_toml_close(conf)` after a failed soft load fatal, which is the opposite of
            ! helpful. A handle that HAS a document but does not own it is the real mistake.
            if (.not. associated(doc%doc)) return
            call pf_log_error("pf_toml_close: this handle does not own its document.")
            call pf_log_error("... only the handle pf_toml_load, pf_toml_loads or pf_toml_new filled owns")
            call pf_log_error("... one; a section handle borrows from it, and closing that would free a")
            call pf_log_error("... document other handles are still reading.")
            call pf_log_error("... section: [" // trim(doc%path) // "] in " // doc%doc%file)
            call pf_log_fatal("ERR: pf_toml_close: not the owning handle")
        end if
        if (associated(doc%doc)) deallocate(doc%doc)
        nullify(doc%tbl)
        nullify(doc%shadow)
        doc%path = ''
        doc%owner = .false.
    end subroutine close_impl

    !> Opens `[name]` or `[[name]]` entry `idx`. See the `pf_toml_section` generic.
    subroutine section_impl(parent, name, idx, indexed, sect, required, found)
        type(pf_toml), intent(in) :: parent           !! Document or section to look in.
        character(len=*), intent(in) :: name          !! Section name.
        integer, intent(in) :: idx                    !! Entry index; ignored unless `indexed`.
        logical, intent(in) :: indexed                !! Whether this is the `[[name]]` form.
        type(pf_toml), intent(out) :: sect            !! Receives the section handle.
        logical, intent(in), optional :: required     !! Fatal when absent. Default `.true.`.
        logical, intent(out), optional :: found       !! Whether the section was there.
        type(toml_table), pointer :: tptr, sptr
        type(toml_array), pointer :: arr, sarr
        class(toml_value), pointer :: vptr
        character(len=PF_TOML_MAX_PATH) :: full, entry
        logical :: need

        need = .true.
        if (present(required)) need = required
        if (present(found)) found = .false.

        call require_open(parent, "pf_toml_section")
        call check_key_len(parent, name)
        call path_join(parent, name, full)
        call mark_path(parent%doc, full)

        ! A closed handle still knows its document and its display path, so that a read from it can
        ! name the section AND the file it was looked for in.
        sect%doc => parent%doc
        sect%owner = .false.
        sect%path = full
        nullify(sect%tbl)
        nullify(sect%shadow)
        if (indexed) then
            call path_index(parent, full, idx, entry)
            sect%path = entry
            call mark_path(parent%doc, entry)
        end if

        call parent%tbl%get(name, vptr)
        if (.not. associated(vptr)) then
            if (need) call fail_missing_section(parent, name, indexed)
            return
        end if

        if (indexed) then
            select type (vptr)
            type is (toml_array)
                arr => vptr
            class default
                call fail_not_entries(parent, name)
                return
            end select
            if (.not. is_array_of_tables(arr)) then
                call fail_not_entries(parent, name)
                return ! GCOVR_EXCL_LINE -- unreachable: `fail_not_entries` never returns
            end if
            ! Gated on `need` exactly as the absent-name case above is, so that the indexed form
            ! and the named form answer `required = .false.` the same way: absence -- of the name,
            ! or of the entry -- leaves the handle CLOSED, while a name of the wrong shape stays
            ! fatal either way. "Is there a seventh entry?" is a question a caller may legitimately
            ! ask; "how many entries does this plain table have?" is a programming error.
            if (idx < 1 .or. idx > toml_len(arr)) then
                if (need) call fail_entry_range(parent, name, idx, toml_len(arr))
                return
            end if
            call get_value(arr, idx, tptr)
        else
            select type (vptr)
            type is (toml_table)
                tptr => vptr
            class default
                call fail_not_a_section(parent, name)
                return ! GCOVR_EXCL_LINE -- unreachable: `fail_not_a_section` never returns
            end select
        end if

        sect%tbl => tptr
        if (present(found)) found = .true.

        ! The shadow section is created on demand, which is what makes a saved file carry exactly
        ! the sections the program opened.
        if (associated(parent%shadow)) then
            if (indexed) then
                call get_value(parent%shadow, name, sarr)
                do while (toml_len(sarr) < idx)
                    call add_table(sarr, sptr)
                end do
                call get_value(sarr, idx, sptr)
            else
                call get_value(parent%shadow, name, sptr)
            end if
            sect%shadow => sptr
        end if
    end subroutine section_impl

    !> How many `[[name]]` entries there are. See `pf_toml_section_count`.
    integer function count_impl(parent, name) result(n)
        type(pf_toml), intent(in) :: parent      !! Document or section to look in.
        character(len=*), intent(in) :: name     !! Array-of-tables name.
        class(toml_value), pointer :: vptr
        type(toml_array), pointer :: arr

        n = 0
        ! An absent optional section has nothing to count: see pf_toml_mark_section.
        if (.not. associated(parent%tbl)) return
        call parent%tbl%get(name, vptr)
        if (.not. associated(vptr)) return
        select type (vptr)
        type is (toml_array)
            arr => vptr
            if (.not. is_array_of_tables(arr)) then
                call fail_not_entries(parent, name)
                return ! GCOVR_EXCL_LINE -- unreachable: `fail_not_entries` never returns
            end if
            n = toml_len(arr)
        class default
            call fail_not_entries(parent, name)
        end select
    end function count_impl

    !> Whether `[name]`/`[[name]]` is there. See `pf_toml_has_section`.
    logical function has_section_impl(parent, name) result(yes)
        type(pf_toml), intent(in) :: parent      !! Document or section to look in.
        character(len=*), intent(in) :: name     !! Section name.
        class(toml_value), pointer :: vptr

        yes = .false.
        if (.not. associated(parent%tbl)) return
        call parent%tbl%get(name, vptr)
        if (.not. associated(vptr)) return
        select type (vptr)
        type is (toml_table)
            yes = .true.
        type is (toml_array)
            yes = is_array_of_tables(vptr)
        class default
            yes = .false.
        end select
    end function has_section_impl

    !> Requires every key of a `;`-separated list. See `pf_toml_require`.
    subroutine require_impl(sect, keys)
        type(pf_toml), intent(in) :: sect       !! Section to check.
        character(len=*), intent(in) :: keys    !! Required key names, `;`-separated.
        character(len=PF_TOML_MAX_PATH) :: full
        character(len=:), allocatable :: wh
        integer :: ipos, iend, nmissing

        call require_open(sect, "pf_toml_require")
        nmissing = 0
        ipos = 1
        do while (ipos <= len(keys))
            iend = index(keys(ipos:), ";")
            if (iend == 0) then
                iend = len(keys) + 1
            else
                iend = ipos + iend - 1
            end if
            if (iend > ipos) then
                call check_key_len(sect, keys(ipos:iend-1))
                call path_join(sect, keys(ipos:iend-1), full)
                call mark_path(sect%doc, full)
                if (.not. sect%tbl%has_key(keys(ipos:iend-1))) then
                    nmissing = nmissing + 1
                    call where_text(sect, keys(ipos:iend-1), wh)
                    call pf_log_error("Required key is not set: " // wh)
                end if
            end if
            ipos = iend + 1
        end do
        if (nmissing > 0) then
            call pf_log_error("... " // trim(pf_str(nmissing)) // " required key(s) missing; each is named above.")
            call pf_log_error("... configuration file: " // sect%doc%file)
            call pf_log_fatal("ERR: required config key(s) not set in [" // trim(sect%path) // "]")
        end if
    end subroutine require_impl

    !> Rejects a retired key. See `pf_toml_retire`.
    subroutine retire_impl(sect, key, advice)
        type(pf_toml), intent(in) :: sect        !! Section to check.
        character(len=*), intent(in) :: key      !! The retired key.
        character(len=*), intent(in) :: advice   !! What to do instead.
        character(len=PF_TOML_MAX_PATH) :: full
        character(len=:), allocatable :: wh

        ! An absent optional section has nothing to retire: see pf_toml_mark_section.
        if (.not. associated(sect%tbl)) return
        call check_key_len(sect, key)
        call path_join(sect, key, full)
        call mark_path(sect%doc, full)
        if (sect%tbl%has_key(key)) then
            call where_text(sect, key, wh)
            call pf_log_error(wh // " is no longer supported.")
            call pf_log_error("... " // advice)
            call pf_log_error("... configuration file: " // sect%doc%file)
            call pf_log_fatal("ERR: retired config key is still set: " // key)
        end if
    end subroutine retire_impl

    !> Sweeps one section for keys nobody read. See `pf_toml_check`.
    subroutine check_one_impl(sect, severity)
        type(pf_toml), intent(in) :: sect            !! Section to sweep.
        integer, intent(in), optional :: severity    !! How loud to be.
        type(toml_key), allocatable :: klist(:)
        character(len=PF_TOML_MAX_PATH) :: full
        character(len=:), allocatable :: wh
        integer :: sev, i, nbad

        ! An absent optional section has nothing to sweep: see pf_toml_mark_section.
        if (.not. associated(sect%tbl)) return
        sev = resolve_severity(severity, "pf_toml_check")
        nbad = 0
        call sect%tbl%get_keys(klist)
        do i = 1, size(klist)
            call path_join(sect, klist(i)%key, full)
            if (path_seen_doc(sect%doc, full)) cycle
            nbad = nbad + 1
            call where_text(sect, klist(i)%key, wh)
            call emit_line(sev, "Unknown key in the configuration file: " // wh)
        end do
        call finish_sweep(sect, sev, nbad)
    end subroutine check_one_impl

    !> Sweeps the whole document for anything nobody read. See `pf_toml_check_all`.
    subroutine check_all_impl(doc, severity)
        type(pf_toml), intent(in) :: doc             !! Document handle.
        integer, intent(in), optional :: severity    !! How loud to be.
        integer :: sev, nbad

        call require_open(doc, "pf_toml_check_all")
        sev = resolve_severity(severity, "pf_toml_check_all")
        nbad = 0
        call sweep_impl(doc, doc%tbl, "", sev, nbad)
        call finish_sweep(doc, sev, nbad)
    end subroutine check_all_impl

    !> One table's worth of `pf_toml_check_all`, recursing into every section that WAS read.
    !!
    !! A section nobody opened is reported once, as a section, and not descended into: reporting
    !! every key inside `[sregion_al]` would bury the one line that matters, which is that the
    !! section name is misspelt.
    recursive subroutine sweep_impl(doc, tbl, path, sev, nbad)
        type(pf_toml), intent(in) :: doc                    !! Document handle, for the accumulator.
        type(toml_table), pointer, intent(in) :: tbl        !! Table to sweep.
        character(len=*), intent(in) :: path                !! This table's display path.
        integer, intent(in) :: sev                          !! How loud to be.
        integer, intent(inout) :: nbad                      !! Running count of problems.
        type(toml_key), allocatable :: klist(:)
        class(toml_value), pointer :: vptr
        type(toml_table), pointer :: sub
        type(toml_array), pointer :: arr
        character(len=PF_TOML_MAX_PATH) :: full, entry
        character(len=:), allocatable :: label
        integer :: i, k, need

        call tbl%get_keys(klist)
        do i = 1, size(klist)
            need = len_trim(path) + len(klist(i)%key)
            if (len_trim(path) > 0) need = need + 1
            if (need > PF_TOML_MAX_PATH) then
                nbad = nbad + 1
                call emit_line(sev, "Path too long to check (over " // trim(pf_str(PF_TOML_MAX_PATH)) &
                    // " characters): " // klist(i)%key)
                cycle
            end if
            if (len_trim(path) > 0) then
                full = trim(path) // "." // klist(i)%key
                label = "[" // trim(path) // "] " // klist(i)%key
            else
                full = klist(i)%key
                label = klist(i)%key
            end if
            call tbl%get(klist(i)%key, vptr)
            if (.not. associated(vptr)) cycle
            select type (vptr)
            type is (toml_table)
                sub => vptr
                if (path_seen_doc(doc%doc, full)) then
                    call sweep_impl(doc, sub, full, sev, nbad)
                else
                    nbad = nbad + 1
                    call emit_line(sev, "Section never read: [" // trim(full) // "]")
                end if
            type is (toml_array)
                arr => vptr
                if (is_array_of_tables(arr)) then
                    if (path_seen_doc(doc%doc, full)) then
                        do k = 1, toml_len(arr)
                            if (len_trim(full) + 2 + len_trim(pf_str(k)) > PF_TOML_MAX_PATH) cycle
                            entry = trim(full) // "[" // trim(pf_str(k)) // "]"
                            call get_value(arr, k, sub)
                            if (.not. associated(sub)) cycle
                            if (path_seen_doc(doc%doc, entry)) then
                                call sweep_impl(doc, sub, entry, sev, nbad)
                            else
                                nbad = nbad + 1
                                call emit_line(sev, "Section never read: [[" // trim(entry) // "]]")
                            end if
                        end do
                    else
                        nbad = nbad + 1
                        call emit_line(sev, "Section never read: [[" // trim(full) // "]]")
                    end if
                else if (.not. path_seen_doc(doc%doc, full)) then
                    nbad = nbad + 1
                    call emit_line(sev, "Unknown key in the configuration file: " // label)
                end if
            class default
                if (.not. path_seen_doc(doc%doc, full)) then
                    nbad = nbad + 1
                    call emit_line(sev, "Unknown key in the configuration file: " // label)
                end if
            end select
        end do
    end subroutine sweep_impl

    !> The tail both sweeps share: the summary lines, and the abort when the severity is fatal.
    subroutine finish_sweep(handle, sev, nbad)
        type(pf_toml), intent(in) :: handle   !! Handle the sweep ran on, for the file name.
        integer, intent(in) :: sev            !! Resolved severity.
        integer, intent(in) :: nbad           !! How many problems were reported.

        if (nbad == 0) return
        call emit_line(sev, "... configuration file: " // handle%doc%file)
        call emit_line(sev, "... " // trim(pf_str(nbad)) // " item(s) the program never reads; check the spelling.")
        if (sev == PF_TOML_FATAL) then
            call pf_log_fatal("ERR: the configuration file has " // trim(pf_str(nbad)) // " item(s) nobody reads")
        end if
    end subroutine finish_sweep

    !> Records a key as read. See `pf_toml_mark`.
    subroutine mark_impl(sect, key)
        type(pf_toml), intent(in) :: sect      !! Section the key belongs to.
        character(len=*), intent(in) :: key    !! Key to record.
        character(len=PF_TOML_MAX_PATH) :: full

        ! An absent optional section has nothing to mark: see pf_toml_mark_section.
        if (.not. associated(sect%tbl)) return
        call check_key_len(sect, key)
        call path_join(sect, key, full)
        call mark_path(sect%doc, full)
    end subroutine mark_impl

    !> Records a whole section as understood. See `pf_toml_mark_section`.
    !!
    !! It must NOT call `require_open`, unlike its four neighbours: an absent optional section has
    !! nothing to mark, and a caller cannot know before opening whether the section is there. That
    !! early return is also why this is a worker rather than inline in the public entry -- a
    !! `return` from inside a `critical` region is non-conforming.
    subroutine mark_section_impl(sect)
        type(pf_toml), intent(in) :: sect      !! Section to record as understood.

        if (.not. associated(sect%tbl)) return
        call mark_tree(sect%doc, sect%tbl, sect%path)
    end subroutine mark_section_impl

    !> One table's worth of `pf_toml_mark_section`, descending into every section below it.
    !!
    !! The mirror image of `sweep_impl`: that one stops at a section nobody read, this one records
    !! everything so that no sweep ever reaches inside. A path too long to compose is skipped
    !! rather than fatal -- a marking pass exists to silence reports, so it cannot itself stop a
    !! run that would otherwise have worked.
    recursive subroutine mark_tree(doc, tbl, path)
        type(pf_toml_doc), intent(inout) :: doc             !! The document's accumulator.
        type(toml_table), pointer, intent(in) :: tbl        !! Table to mark.
        character(len=*), intent(in) :: path                !! This table's display path.
        type(toml_key), allocatable :: klist(:)
        class(toml_value), pointer :: vptr
        type(toml_table), pointer :: sub
        type(toml_array), pointer :: arr
        character(len=PF_TOML_MAX_PATH) :: full, entry
        integer :: i, k, need

        call tbl%get_keys(klist)
        do i = 1, size(klist)
            need = len_trim(path) + len(klist(i)%key)
            if (len_trim(path) > 0) need = need + 1
            if (need > PF_TOML_MAX_PATH) cycle
            if (len_trim(path) > 0) then
                full = trim(path) // "." // klist(i)%key
            else
                full = klist(i)%key
            end if
            call mark_path(doc, full)
            call tbl%get(klist(i)%key, vptr)
            if (.not. associated(vptr)) cycle
            select type (vptr)
            type is (toml_table)
                sub => vptr
                call mark_tree(doc, sub, full)
            type is (toml_array)
                arr => vptr
                if (is_array_of_tables(arr)) then
                    do k = 1, toml_len(arr)
                        if (len_trim(full) + 2 + len_trim(pf_str(k)) > PF_TOML_MAX_PATH) cycle
                        entry = trim(full) // "[" // trim(pf_str(k)) // "]"
                        call mark_path(doc, entry)
                        call get_value(arr, k, sub)
                        if (associated(sub)) call mark_tree(doc, sub, entry)
                    end do
                end if
            class default
                continue
            end select
        end do
    end subroutine mark_tree

    !> Lists a section's keys. See `pf_toml_keys`.
    subroutine keys_impl(sect, keys)
        type(pf_toml), intent(in) :: sect                                     !! Section to enumerate.
        character(len=PF_TOML_MAX_KEY), allocatable, intent(out) :: keys(:)   !! Receives the names.
        type(toml_key), allocatable :: klist(:)
        integer :: i

        if (.not. associated(sect%tbl)) then
            allocate(keys(0))
            return
        end if
        call sect%tbl%get_keys(klist)
        allocate(keys(size(klist, kind=int64)))
        do i = 1, size(klist)
            call check_key_len(sect, klist(i)%key)
            keys(i) = klist(i)%key
        end do
    end subroutine keys_impl

    !> Logs a caller's own complaint against a key's source line. See `pf_toml_report`.
    subroutine report_impl(sect, key, message, severity)
        type(pf_toml), intent(in) :: sect            !! Section the key belongs to.
        character(len=*), intent(in) :: key          !! Key to point at.
        character(len=*), intent(in) :: message      !! What to say about it.
        integer, intent(in), optional :: severity    !! How loud to be.
        class(toml_value), pointer :: vptr
        character(len=:), allocatable :: wh, diag
        integer :: sev, origin

        call require_open(sect, "pf_toml_report")
        sev = resolve_severity(severity, "pf_toml_report")
        if (sev == PF_TOML_IGNORE) return
        call where_text(sect, key, wh)
        call emit_line(sev, wh // ": " // message)
        origin = key_origin(sect, key)
        if (origin > 0) then
            diag = sect%doc%ctx%report(wh // ": " // message, origin)
            call log_block(diag)
        end if
        call emit_line(sev, "... configuration file: " // sect%doc%file)
        if (sev == PF_TOML_FATAL) call pf_log_fatal("ERR: " // wh // ": " // message)
    end subroutine report_impl

    !> Adds or reuses `[name]`. See `pf_toml_new_section`.
    subroutine new_section_impl(parent, name, sect)
        type(pf_toml), intent(in) :: parent      !! Document or section to add to.
        character(len=*), intent(in) :: name     !! Section name.
        type(pf_toml), intent(out) :: sect       !! Receives the section handle.
        class(toml_value), pointer :: vptr
        type(toml_table), pointer :: tptr, sptr
        character(len=PF_TOML_MAX_PATH) :: full

        call require_open(parent, "pf_toml_new_section")
        call check_key_len(parent, name)
        call path_join(parent, name, full)
        call mark_path(parent%doc, full)

        call parent%tbl%get(name, vptr)
        if (associated(vptr)) then
            select type (vptr)
            type is (toml_table)
                tptr => vptr
            class default
                call fail_not_a_section(parent, name)
                return ! GCOVR_EXCL_LINE -- unreachable: `fail_not_a_section` never returns
            end select
        else
            call get_value(parent%tbl, name, tptr)
        end if

        sect%doc => parent%doc
        sect%tbl => tptr
        sect%path = full
        sect%owner = .false.
        nullify(sect%shadow)
        if (associated(parent%shadow)) then
            call get_value(parent%shadow, name, sptr)
            sect%shadow => sptr
        end if
    end subroutine new_section_impl

    !> Appends one `[[name]]` entry. See `pf_toml_append_section`.
    subroutine append_section_impl(parent, name, sect)
        type(pf_toml), intent(in) :: parent      !! Document or section to add to.
        character(len=*), intent(in) :: name     !! Array-of-tables name.
        type(pf_toml), intent(out) :: sect       !! Receives the new entry's handle.
        class(toml_value), pointer :: vptr
        type(toml_table), pointer :: tptr, sptr
        type(toml_array), pointer :: arr, sarr
        character(len=PF_TOML_MAX_PATH) :: full, entry
        integer :: k

        call require_open(parent, "pf_toml_append_section")
        call check_key_len(parent, name)
        call path_join(parent, name, full)

        call parent%tbl%get(name, vptr)
        if (associated(vptr)) then
            select type (vptr)
            type is (toml_array)
                arr => vptr
                ! An EMPTY array reads as "not an array of tables" (toml-f answers on its
                ! contents), and appending a table to one is exactly how it becomes one -- so the
                ! refusal below has to spare that case or the first append to a fresh array would
                ! be rejected by the code that created it.
                if (toml_len(arr) > 0 .and. .not. is_array_of_tables(arr)) then
                    call fail_not_entries(parent, name)
                    return ! GCOVR_EXCL_LINE -- unreachable: `fail_not_entries` never returns
                end if
            class default
                call fail_not_entries(parent, name)
                return ! GCOVR_EXCL_LINE -- unreachable: `fail_not_entries` never returns
            end select
        else
            call get_value(parent%tbl, name, arr)
        end if

        call add_table(arr, tptr)
        k = toml_len(arr)
        call path_index(parent, full, k, entry)
        call mark_path(parent%doc, full)
        call mark_path(parent%doc, entry)

        sect%doc => parent%doc
        sect%tbl => tptr
        sect%path = entry
        sect%owner = .false.
        nullify(sect%shadow)
        if (associated(parent%shadow)) then
            call get_value(parent%shadow, name, sarr)
            do while (toml_len(sarr) < k)
                call add_table(sarr, sptr)
            end do
            call get_value(sarr, k, sptr)
            sect%shadow => sptr
        end if
    end subroutine append_section_impl

    !> Writes the effective document out. See `pf_toml_save`.
    subroutine save_impl(doc, file)
        type(pf_toml), intent(in) :: doc         !! Document handle.
        character(len=*), intent(in) :: file     !! Path to write to.
        type(toml_error), allocatable :: terr

        call require_open(doc, "pf_toml_save")
        if (.not. doc%owner) then
            call pf_log_error("pf_toml_save: this handle is a section, not the document.")
            call pf_log_error("... saving writes the whole configuration, so pass the handle an open filled.")
            call pf_log_fatal("ERR: pf_toml_save: not the document handle")
        end if
        call toml_dump(doc%doc%shadow, file, terr)
        if (allocated(terr)) then
            call pf_log_error("Cannot write configuration file: " // file)
            call log_block(terr%message)
            call pf_log_fatal("ERR: cannot write configuration file: " // file)
        end if
    end subroutine save_impl

    !> Writes the parsed document out. See `pf_toml_dump`.
    subroutine dump_impl(doc, file)
        type(pf_toml), intent(in) :: doc         !! Document handle.
        character(len=*), intent(in) :: file     !! Path to write to.
        type(toml_error), allocatable :: terr

        call require_open(doc, "pf_toml_dump")
        if (.not. doc%owner) then
            call pf_log_error("pf_toml_dump: this handle is a section, not the document.")
            call pf_log_error("... dumping writes the whole configuration, so pass the handle an open filled.")
            call pf_log_fatal("ERR: pf_toml_dump: not the document handle")
        end if
        call toml_dump(doc%doc%root, file, terr)
        if (allocated(terr)) then
            call pf_log_error("Cannot write configuration file: " // file)
            call log_block(terr%message)
            call pf_log_fatal("ERR: cannot write configuration file: " // file)
        end if
    end subroutine dump_impl

    !> Removes a key from the parsed AND the effective document. See `pf_toml_delete`.
    !!
    !! `toml_table%delete` is already a no-op on a key that is not there, so the tolerance the
    !! public contract promises costs nothing here. The shadow delete is the half that matters: a
    !! key an earlier getter resolved is sitting in the effective document, and skipping it would
    !! have `pf_toml_save` write back a value the caller had just removed.
    subroutine delete_impl(sect, key)
        type(pf_toml), intent(in) :: sect      !! Handle to remove the key from.
        character(len=*), intent(in) :: key    !! Key to remove.
        character(len=PF_TOML_MAX_PATH) :: full

        call require_open(sect, "pf_toml_delete")
        call check_key_len(sect, key)
        call path_join(sect, key, full)
        call mark_path(sect%doc, full)
        call sect%tbl%delete(key)
        if (associated(sect%shadow)) call sect%shadow%delete(key)
    end subroutine delete_impl

    !> `pf_toml_set`/`pf_toml_update` for an `integer(int32)` scalar.
    subroutine write_i32(sect, key, value, adding)
        type(pf_toml), intent(in) :: sect       !! Handle to write into.
        character(len=*), intent(in) :: key     !! Key.
        integer(int32), intent(in) :: value     !! Value.
        logical, intent(in) :: adding           !! `.true.` for set, `.false.` for update.

        call begin_write(sect, key, adding)
        call set_value(sect%tbl, key, value)
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
    end subroutine write_i32

    !> `pf_toml_set`/`pf_toml_update` for an `integer(int64)` scalar.
    subroutine write_i64(sect, key, value, adding)
        type(pf_toml), intent(in) :: sect       !! Handle to write into.
        character(len=*), intent(in) :: key     !! Key.
        integer(int64), intent(in) :: value     !! Value.
        logical, intent(in) :: adding           !! `.true.` for set, `.false.` for update.

        call begin_write(sect, key, adding)
        call set_value(sect%tbl, key, value)
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
    end subroutine write_i64

    !> `pf_toml_set`/`pf_toml_update` for a `real(real32)` scalar.
    subroutine write_r32(sect, key, value, adding)
        type(pf_toml), intent(in) :: sect       !! Handle to write into.
        character(len=*), intent(in) :: key     !! Key.
        real(real32), intent(in) :: value       !! Value.
        logical, intent(in) :: adding           !! `.true.` for set, `.false.` for update.

        call begin_write(sect, key, adding)
        call set_value(sect%tbl, key, value)
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
    end subroutine write_r32

    !> `pf_toml_set`/`pf_toml_update` for a `real(real64)` scalar.
    subroutine write_r64(sect, key, value, adding)
        type(pf_toml), intent(in) :: sect       !! Handle to write into.
        character(len=*), intent(in) :: key     !! Key.
        real(real64), intent(in) :: value       !! Value.
        logical, intent(in) :: adding           !! `.true.` for set, `.false.` for update.

        call begin_write(sect, key, adding)
        call set_value(sect%tbl, key, value)
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
    end subroutine write_r64

    !> `pf_toml_set`/`pf_toml_update` for a `logical` scalar.
    subroutine write_log(sect, key, value, adding)
        type(pf_toml), intent(in) :: sect       !! Handle to write into.
        character(len=*), intent(in) :: key     !! Key.
        logical, intent(in) :: value            !! Value.
        logical, intent(in) :: adding           !! `.true.` for set, `.false.` for update.

        call begin_write(sect, key, adding)
        call set_value(sect%tbl, key, value)
        if (associated(sect%shadow)) call set_value(sect%shadow, key, value)
    end subroutine write_log

    !> `pf_toml_set`/`pf_toml_update` for a `character` scalar. Trailing blanks are trimmed.
    subroutine write_str(sect, key, value, adding)
        type(pf_toml), intent(in) :: sect       !! Handle to write into.
        character(len=*), intent(in) :: key     !! Key.
        character(len=*), intent(in) :: value   !! Value, trailing blanks trimmed.
        logical, intent(in) :: adding           !! `.true.` for set, `.false.` for update.

        call begin_write(sect, key, adding)
        call set_value(sect%tbl, key, trim(value))
        if (associated(sect%shadow)) call set_value(sect%shadow, key, trim(value))
    end subroutine write_str

    !> `pf_toml_set`/`pf_toml_update` for an `integer(int32)` array.
    subroutine write_i32_arr(sect, key, values, adding)
        type(pf_toml), intent(in) :: sect          !! Handle to write into.
        character(len=*), intent(in) :: key        !! Key.
        integer(int32), intent(in) :: values(:)    !! Values.
        logical, intent(in) :: adding              !! `.true.` for set, `.false.` for update.
        type(toml_array), pointer :: arr

        call begin_write(sect, key, adding)
        call new_array_in(sect%tbl, key, arr)
        if (associated(arr)) call set_value(arr, values)
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
    end subroutine write_i32_arr

    !> `pf_toml_set`/`pf_toml_update` for an `integer(int64)` array.
    subroutine write_i64_arr(sect, key, values, adding)
        type(pf_toml), intent(in) :: sect          !! Handle to write into.
        character(len=*), intent(in) :: key        !! Key.
        integer(int64), intent(in) :: values(:)    !! Values.
        logical, intent(in) :: adding              !! `.true.` for set, `.false.` for update.
        type(toml_array), pointer :: arr

        call begin_write(sect, key, adding)
        call new_array_in(sect%tbl, key, arr)
        if (associated(arr)) call set_value(arr, values)
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
    end subroutine write_i64_arr

    !> `pf_toml_set`/`pf_toml_update` for a `real(real32)` array.
    subroutine write_r32_arr(sect, key, values, adding)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key.
        real(real32), intent(in) :: values(:)    !! Values.
        logical, intent(in) :: adding            !! `.true.` for set, `.false.` for update.
        type(toml_array), pointer :: arr

        call begin_write(sect, key, adding)
        call new_array_in(sect%tbl, key, arr)
        if (associated(arr)) call set_value(arr, values)
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
    end subroutine write_r32_arr

    !> `pf_toml_set`/`pf_toml_update` for a `real(real64)` array.
    subroutine write_r64_arr(sect, key, values, adding)
        type(pf_toml), intent(in) :: sect        !! Handle to write into.
        character(len=*), intent(in) :: key      !! Key.
        real(real64), intent(in) :: values(:)    !! Values.
        logical, intent(in) :: adding            !! `.true.` for set, `.false.` for update.
        type(toml_array), pointer :: arr

        call begin_write(sect, key, adding)
        call new_array_in(sect%tbl, key, arr)
        if (associated(arr)) call set_value(arr, values)
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
    end subroutine write_r64_arr

    !> `pf_toml_set`/`pf_toml_update` for a `logical` array.
    subroutine write_log_arr(sect, key, values, adding)
        type(pf_toml), intent(in) :: sect      !! Handle to write into.
        character(len=*), intent(in) :: key    !! Key.
        logical, intent(in) :: values(:)       !! Values.
        logical, intent(in) :: adding          !! `.true.` for set, `.false.` for update.
        type(toml_array), pointer :: arr

        call begin_write(sect, key, adding)
        call new_array_in(sect%tbl, key, arr)
        if (associated(arr)) call set_value(arr, values)
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) call set_value(arr, values)
    end subroutine write_log_arr

    !> `pf_toml_set`/`pf_toml_update` for a `character` array. Every element is trimmed.
    subroutine write_str_arr(sect, key, values, adding)
        type(pf_toml), intent(in) :: sect           !! Handle to write into.
        character(len=*), intent(in) :: key         !! Key.
        character(len=*), intent(in) :: values(:)   !! Values, each trimmed.
        logical, intent(in) :: adding               !! `.true.` for set, `.false.` for update.
        type(toml_array), pointer :: arr
        integer :: i

        call begin_write(sect, key, adding)
        call new_array_in(sect%tbl, key, arr)
        if (associated(arr)) then
            do i = 1, size(values)
                call set_value(arr, i, trim(values(i)))
            end do
        end if
        call shadow_new_array(sect, key, arr)
        if (associated(arr)) then
            do i = 1, size(values)
                call set_value(arr, i, trim(values(i)))
            end do
        end if
    end subroutine write_str_arr

    ! ---- Shared prologues -------------------------------------------------------------------

    !> Everything every getter does before it looks at a value: check the handle is open, check the
    !> key is a sane length, record the ask, and report whether the file actually sets it.
    !!
    !! **The ask is recorded whether or not the key is there**, and that is the point rather than a
    !! detail: an optional key with a default is legitimate, so a program that reads it must not
    !! then have it reported as unknown on the runs where the file *does* set it.
    subroutine begin_read(sect, key, in_file)
        type(pf_toml), intent(in) :: sect      !! Handle being read from.
        character(len=*), intent(in) :: key    !! Key being read.
        logical, intent(out) :: in_file        !! Whether the file sets this key.
        character(len=PF_TOML_MAX_PATH) :: full

        call require_open(sect, "pf_toml_get")
        call check_key_len(sect, key)
        call path_join(sect, key, full)
        call mark_path(sect%doc, full)
        in_file = sect%tbl%has_key(key)
    end subroutine begin_read

    !> Everything `pf_toml_set` and `pf_toml_update` do before they write: the existence check that
    !> separates the two, plus the open/length checks and the accumulator record.
    subroutine begin_write(sect, key, adding)
        type(pf_toml), intent(in) :: sect      !! Handle being written to.
        character(len=*), intent(in) :: key    !! Key being written.
        logical, intent(in) :: adding          !! `.true.` for set, `.false.` for update.
        character(len=PF_TOML_MAX_PATH) :: full
        character(len=:), allocatable :: wh
        logical :: exists

        if (adding) then
            call require_open(sect, "pf_toml_set")
        else
            call require_open(sect, "pf_toml_update")
        end if
        call check_key_len(sect, key)
        exists = sect%tbl%has_key(key)
        if (adding .eqv. exists) then
            call where_text(sect, key, wh)
            if (adding) then
                call pf_log_error("pf_toml_set: " // wh // " is already set.")
                call pf_log_error("... pf_toml_set ADDS a key; use pf_toml_update to change one that exists.")
            else
                call pf_log_error("pf_toml_update: " // wh // " is not set.")
                call pf_log_error("... pf_toml_update CHANGES a key; use pf_toml_set to add one that does not")
                call pf_log_error("... exist yet. Note that a key the program read from its DEFAULT counts as")
                call pf_log_error("... not set: a default is never written into the parsed document.")
            end if
            call pf_log_error("... configuration file: " // sect%doc%file)
            if (adding) then
                call pf_log_fatal("ERR: pf_toml_set: key already exists: " // key)
            else
                call pf_log_fatal("ERR: pf_toml_update: key does not exist: " // key)
            end if
        end if
        call path_join(sect, key, full)
        call mark_path(sect%doc, full)
    end subroutine begin_write

    !> Stops unless this handle refers to a table.
    subroutine require_open(sect, who)
        type(pf_toml), intent(in) :: sect      !! The handle.
        character(len=*), intent(in) :: who    !! Procedure name, for the message.

        if (associated(sect%tbl)) return
        if (associated(sect%doc)) then
            call pf_log_error(who // ": section [" // trim(sect%path) // "] was not found in " // sect%doc%file)
            call pf_log_error("... it is optional, so guard the reads with pf_toml_is_open, or open it with")
            call pf_log_error("... pf_toml_section(..., required = .true.) if the program cannot run without it.")
        else
            call pf_log_error(who // ": this handle has not been opened.")
            call pf_log_error("... call pf_toml_load, pf_toml_loads or pf_toml_new first.")
        end if
        call pf_log_fatal("ERR: parquet_toml: used a handle that is not open")
    end subroutine require_open

    !> Stops on a key longer than `PF_TOML_MAX_KEY`.
    subroutine check_key_len(sect, key)
        type(pf_toml), intent(in) :: sect      !! Handle the key belongs to, for the file name.
        character(len=*), intent(in) :: key    !! The key.

        if (len(key) <= PF_TOML_MAX_KEY) return
        call pf_log_error("Configuration key name is longer than " // trim(pf_str(PF_TOML_MAX_KEY)) &
            // " characters.")
        call pf_log_error("... key begins: " // key(1:min(len(key), 60)))
        if (associated(sect%doc)) call pf_log_error("... configuration file: " // sect%doc%file)
        call pf_log_fatal("ERR: configuration key name is too long")
    end subroutine check_key_len

    !> Stops when a rank-1 `default` does not hold exactly as many entries as it would fill.
    !!
    !! The message deliberately mirrors `fail_length`'s, so the two ways a list can be the wrong
    !! length -- the file's, and the program's own default -- read alike.
    subroutine check_default_size(sect, key, ngot, nwant)
        type(pf_toml), intent(in) :: sect      !! Handle being read from.
        character(len=*), intent(in) :: key    !! The key.
        integer, intent(in) :: ngot            !! How many entries the default has.
        integer, intent(in) :: nwant           !! How many the program needs.
        character(len=:), allocatable :: wh, head

        if (ngot == nwant) return
        call where_text(sect, key, wh)
        head = wh // ": the default has " // trim(pf_str(ngot)) // " entries and the program needs " &
            // trim(pf_str(nwant))
        call fail_tail(sect, sect%tbl%origin, head, &
            "ERR: config default has the wrong number of entries: " // key)
    end subroutine check_default_size

    !> Fills a `pf_toml_strings` from `key`'s list, which the caller has confirmed is present.
    !!
    !! Two passes -- measure, then copy -- so the packed buffer is allocated once at its exact size
    !! rather than grown. A configuration list is short enough that reading each element twice
    !! costs nothing, and a growable buffer here would be the only thing in this module that has to
    !! reason about capacity.
    !!
    !! Shared by `pf_toml_get_strings` and `pf_toml_get_strings_opt`, which differ only in what
    !! they do when the key is absent, so it deallocates first rather than relying on an
    !! `intent(out)` reset the optional form does not have.
    subroutine fill_strings(sect, key, strings, count)
        type(pf_toml), intent(in) :: sect                   !! Handle being read from.
        character(len=*), intent(in) :: key                 !! Key being read.
        type(pf_toml_strings), intent(inout) :: strings     !! Receives the list.
        integer, intent(in), optional :: count              !! Exact number of entries required.
        type(toml_array), pointer :: arr, sarr
        character(len=:), allocatable :: one
        integer :: stat, origin, i, j, n, total, pos, nwant

        nwant = -1
        if (present(count)) nwant = count
        call open_list(sect, key, nwant, arr, origin)
        n = toml_len(arr)
        if (allocated(strings%off)) deallocate(strings%off)
        if (allocated(strings%buf)) deallocate(strings%buf)
        allocate(strings%off(n + 1))
        strings%off(1) = 1
        strings%n = n
        total = 0
        do i = 1, n
            call get_value(arr, i, one, stat=stat)
            if (stat /= toml_stat%success .or. .not. allocated(one)) &
                call fail_value(sect, key, origin, stat, "a list of strings")
            total = total + len(one)
            strings%off(i + 1) = total + 1
        end do
        allocate(strings%buf(total))
        call shadow_new_array(sect, key, sarr)
        do i = 1, n
            call get_value(arr, i, one, stat=stat)
            pos = strings%off(i)
            do j = 1, len(one)
                strings%buf(pos + j - 1) = one(j:j)
            end do
            if (associated(sarr)) call set_value(sarr, i, one)
        end do
    end subroutine fill_strings

    !> Records a `pf_toml_strings`'s existing content in the effective document.
    !!
    !! What `pf_toml_get_strings_opt` does when the file does not set the key: whatever the caller
    !! already had is the value the run used, so that is what a later `pf_toml_save` must write. A
    !! variable the caller never filled has no value at all and is skipped -- writing an empty list
    !! would claim the run used one, and `allocated(%off)` is exactly the "was this ever filled"
    !! test, since a list read as genuinely empty has a one-element offset array.
    subroutine strings_to_shadow(sect, key, strings)
        type(pf_toml), intent(in) :: sect                !! Handle being read from.
        character(len=*), intent(in) :: key              !! Key being read.
        type(pf_toml_strings), intent(in) :: strings     !! The list to record.
        type(toml_array), pointer :: sarr
        character(len=:), allocatable :: one
        integer :: i, j, n

        if (.not. allocated(strings%off)) return
        call shadow_new_array(sect, key, sarr)
        if (.not. associated(sarr)) return
        do i = 1, strings%n
            n = strings%off(i + 1) - strings%off(i)
            allocate(character(len=n) :: one)
            do j = 1, n
                one(j:j) = strings%buf(strings%off(i) + j - 1)
            end do
            call set_value(sarr, i, one)
            deallocate(one)
        end do
    end subroutine strings_to_shadow

    !> Points `arr` at `key`'s list, checking that it IS a list and, when `nwant >= 0`, its length.
    subroutine open_list(sect, key, nwant, arr, origin)
        type(pf_toml), intent(in) :: sect                  !! Handle being read from.
        character(len=*), intent(in) :: key                !! Key being read.
        integer, intent(in) :: nwant                       !! Required length, or `-1` for any.
        type(toml_array), pointer, intent(out) :: arr      !! Receives the list.
        integer, intent(out) :: origin                     !! Source token of the value.
        integer :: stat

        origin = 0
        call get_value(sect%tbl, key, arr, requested=.false., stat=stat, origin=origin)
        if (.not. associated(arr)) call fail_not_list(sect, key, origin)
        if (nwant >= 0) then
            if (toml_len(arr) /= nwant) call fail_length(sect, key, origin, toml_len(arr), nwant)
        end if
    end subroutine open_list

    ! ---- The accumulator --------------------------------------------------------------------

    !> Composes `parent`'s path and `name` into a `section.key` path, stopping if it will not fit.
    subroutine path_join(parent, name, out)
        type(pf_toml), intent(in) :: parent                    !! Handle owning the base path.
        character(len=*), intent(in) :: name                   !! Key or section name to append.
        character(len=PF_TOML_MAX_PATH), intent(out) :: out    !! Receives the composed path.
        integer :: need

        need = len_trim(parent%path) + len(name)
        if (len_trim(parent%path) > 0) need = need + 1
        if (need > PF_TOML_MAX_PATH) call fail_path_too_long(parent, name, need)
        if (len_trim(parent%path) > 0) then
            out = trim(parent%path) // "." // name
        else
            out = name
        end if
    end subroutine path_join

    !> Composes `full` and `idx` into a `name[idx]` entry path, stopping if it will not fit.
    subroutine path_index(parent, full, idx, out)
        type(pf_toml), intent(in) :: parent                    !! Handle, for the error message.
        character(len=*), intent(in) :: full                   !! The array's own path.
        integer, intent(in) :: idx                             !! 1-based entry index.
        character(len=PF_TOML_MAX_PATH), intent(out) :: out    !! Receives the composed path.
        integer :: need

        need = len_trim(full) + 2 + len_trim(pf_str(idx))
        if (need > PF_TOML_MAX_PATH) call fail_path_too_long(parent, trim(full), need)
        out = trim(full) // "[" // trim(pf_str(idx)) // "]"
    end subroutine path_index

    !> Records one path as asked-for, once.
    !!
    !! Re-reading a key is legitimate -- a program may read `[sregion_all]`'s value and then each
    !! `[[sregion]]`'s override of it -- so membership is tested before appending and the list
    !! cannot grow without bound.
    subroutine mark_path(doc, path)
        type(pf_toml_doc), intent(inout) :: doc   !! The document's accumulator.
        character(len=*), intent(in) :: path      !! Path to record.
        character(len=PF_TOML_MAX_PATH), allocatable :: bigger(:)
        integer :: cap

        if (path_seen_doc(doc, path)) return
        ! The body below is unreachable: `finish_open` allocates `seen(32)` as part of every open,
        ! and every caller of this procedure sits downstream of a `require_open` that aborts on a
        ! handle no open filled. Kept as the accumulator's own precondition rather than a shared
        ! assumption about the order two procedures run in.
        if (.not. allocated(doc%seen)) then
            allocate(doc%seen(32)) ! GCOVR_EXCL_LINE -- unreachable: finish_open allocates it
            doc%nseen = 0          ! GCOVR_EXCL_LINE -- unreachable: finish_open allocates it
        end if
        cap = size(doc%seen)
        if (doc%nseen >= cap) then
            allocate(bigger(2 * cap))
            bigger(1:doc%nseen) = doc%seen(1:doc%nseen)
            call move_alloc(bigger, doc%seen)
        end if
        doc%nseen = doc%nseen + 1
        doc%seen(doc%nseen) = path
    end subroutine mark_path

    !> Whether a path has been asked for.
    logical function path_seen_doc(doc, path) result(yes)
        type(pf_toml_doc), intent(in) :: doc   !! The document's accumulator.
        character(len=*), intent(in) :: path   !! Path to look for.
        integer :: i

        yes = .false.
        ! Unreachable: `finish_open` allocates `seen` on every open, and every route here runs
        ! `require_open` first. See `mark_path` for the same reasoning.
        if (.not. allocated(doc%seen)) return ! GCOVR_EXCL_LINE -- unreachable: finish_open allocates it
        do i = 1, doc%nseen
            if (doc%seen(i) == path) then
                yes = .true.
                return
            end if
        end do
    end function path_seen_doc

    ! ---- The shadow document ----------------------------------------------------------------

    !> Replaces `key` in the shadow with a fresh, empty array, or returns null when there is none.
    !!
    !! Fresh rather than resized: a shadow array is rewritten whole on every read, and deleting
    !! first means no code here has to reason about an existing array's length or element types.
    subroutine shadow_new_array(sect, key, arr)
        type(pf_toml), intent(in) :: sect                 !! Handle being read or written.
        character(len=*), intent(in) :: key               !! Key to replace.
        type(toml_array), pointer, intent(out) :: arr     !! Receives the new array, or null.

        nullify(arr)
        ! Unreachable: a handle only reaches here past `require_open`, which aborts unless `tbl`
        ! is associated, and `finish_open`/`section_impl` give every handle that HAS a table a
        ! shadow. The null case is the not-found optional section, which `require_open` refuses.
        if (.not. associated(sect%shadow)) return ! GCOVR_EXCL_LINE -- unreachable: see above
        call sect%shadow%delete(key)
        call add_array(sect%shadow, key, arr)
    end subroutine shadow_new_array

    !> Replaces `key` in a table with a fresh, empty array.
    subroutine new_array_in(tbl, key, arr)
        type(toml_table), pointer, intent(in) :: tbl      !! Table to write into.
        character(len=*), intent(in) :: key               !! Key to replace.
        type(toml_array), pointer, intent(out) :: arr     !! Receives the new array.

        nullify(arr)
        ! Unreachable: the only actual is `sect%tbl` of a handle `begin_write` has just put
        ! through `require_open`, which aborts unless that pointer is associated. Kept because
        ! the alternative is `tbl%delete` on a null pointer.
        if (.not. associated(tbl)) return ! GCOVR_EXCL_LINE -- unreachable: require_open guarantees it
        call tbl%delete(key)
        call add_array(tbl, key, arr)
    end subroutine new_array_in

    ! ---- Diagnostics ------------------------------------------------------------------------

    !> Renders where a key lives: `[general] log_filename`, or just `log_filename` at the root.
    subroutine where_text(sect, key, out)
        type(pf_toml), intent(in) :: sect                    !! Handle the key belongs to.
        character(len=*), intent(in) :: key                  !! The key.
        character(len=:), allocatable, intent(out) :: out    !! Receives the rendered location.

        if (len_trim(sect%path) > 0) then
            out = "[" // trim(sect%path) // "] " // key
        else
            out = key
        end if
    end subroutine where_text

    !> Writes a possibly multi-line diagnostic to the log, one `pf_log_error` per line.
    !!
    !! toml-f's rendered reports span several lines. Passing one whole would print the embedded
    !! newlines raw and lose the log's own prefix on every line but the first.
    subroutine log_block(text)
        character(len=*), intent(in) :: text   !! Diagnostic, lines separated by newline characters.
        integer :: ipos, ieol

        ipos = 1
        do while (ipos <= len(text))
            ieol = index(text(ipos:), new_line("a"))
            if (ieol == 0) then
                ieol = len(text) + 1
            else
                ieol = ipos + ieol - 1
            end if
            if (ieol > ipos) call pf_log_error("  " // text(ipos:ieol-1))
            ipos = ieol + 1
        end do
    end subroutine log_block

    !> Emits one line at the given severity. `PF_TOML_IGNORE` says nothing at all.
    subroutine emit_line(sev, text)
        integer, intent(in) :: sev             !! Resolved severity.
        character(len=*), intent(in) :: text   !! The line.

        select case (sev)
        case (PF_TOML_WARN)
            call pf_log_warning(text)
        case (PF_TOML_FATAL)
            call pf_log_error(text)
        end select
    end subroutine emit_line

    !> Resolves an optional `severity` to its default and rejects a value that is not one of three.
    integer function resolve_severity(severity, who) result(sev)
        integer, intent(in), optional :: severity    !! As the caller gave it.
        character(len=*), intent(in) :: who          !! Procedure name, for the message.

        sev = PF_TOML_FATAL
        if (present(severity)) sev = severity
        if (sev == PF_TOML_IGNORE .or. sev == PF_TOML_WARN .or. sev == PF_TOML_FATAL) return
        call pf_log_error(who // ": severity must be PF_TOML_IGNORE, PF_TOML_WARN or PF_TOML_FATAL.")
        call pf_log_fatal("ERR: parquet_toml: unknown severity: " // trim(pf_str(sev)))
    end function resolve_severity

    !> The source token of `key`'s VALUE, or `0` when the file does not set it.
    !!
    !! **The three concrete arms are not a simplification waiting to happen.** `toml_table`,
    !! `toml_array` and `toml_keyval` are toml-f's only extensions of `toml_value`, so a single
    !! `class default` reading the inherited `%origin` off the polymorphic pointer would be exactly
    !! equivalent -- and nagfor 7.2 generates invalid C for it under `-C=undefined`, failing the
    !! `nagundef` profile with `no member named 'addr'`. Writing the arms out keeps the type static
    !! at every read. See CLAUDE.md, "nagfor-specific gotchas". The `class default` below is
    !! unreachable and exists only to make the construct total.
    integer function key_origin(sect, key) result(origin)
        type(pf_toml), intent(in) :: sect      !! Handle the key belongs to.
        character(len=*), intent(in) :: key    !! The key.
        class(toml_value), pointer :: vptr

        origin = 0
        if (.not. associated(sect%tbl)) return
        call sect%tbl%get(key, vptr)
        if (.not. associated(vptr)) return
        select type (vptr)
        type is (toml_keyval)
            origin = vptr%origin_value
        type is (toml_table)
            origin = vptr%origin
        type is (toml_array)
            origin = vptr%origin
        class default
            continue
        end select
    end function key_origin

    ! ---- The fatal reports -------------------------------------------------------------------
    !
    ! Every one of these ends in pf_log_fatal and so never returns. They share one tail, which is
    ! what fixes the message shape across the whole module:
    !
    !     [sregion[3]] pot_factor is not a list of numbers: config.toml
    !       --> config.toml:88:26
    !        |
    !     88 |     pot_factor = [1.0, "two"]
    !        |                        ^^^^^ [sregion[3]] pot_factor is not a list of numbers
    !        |
    !     ... configuration file: config.toml
    !     ERR: config value has the wrong type: pot_factor
    !
    ! The middle block is toml-f's own rendered report, split one log record per line -- passing it
    ! whole would print the newlines raw and lose the log prefix on every line but the first.

    !> The shared tail: headline, source excerpt when there is one, file, and the fatal line.
    subroutine fail_tail(sect, origin, headline, code)
        type(pf_toml), intent(in) :: sect           !! Handle, for the file and the token context.
        integer, intent(in) :: origin               !! Source token to point at, or `0` for none.
        character(len=*), intent(in) :: headline    !! What went wrong, already located.
        character(len=*), intent(in) :: code        !! The `ERR: ...` line the abort carries.
        character(len=:), allocatable :: diag

        call pf_log_error(headline // ": " // sect%doc%file)
        if (origin > 0) then
            diag = sect%doc%ctx%report(headline, origin)
            if (len_trim(diag) > 0) call log_block(diag)
        end if
        call pf_log_error("... configuration file: " // sect%doc%file)
        call pf_log_fatal(code)
    end subroutine fail_tail

    !> A value that is there but of the wrong type, or too large for the kind asked for.
    subroutine fail_value(sect, key, origin, stat, expected)
        type(pf_toml), intent(in) :: sect            !! Handle being read from.
        character(len=*), intent(in) :: key          !! The key.
        integer, intent(in) :: origin                !! Source token of the value.
        integer, intent(in) :: stat                  !! toml-f's own status code.
        character(len=*), intent(in) :: expected     !! What was wanted, e.g. `a whole number`.
        character(len=:), allocatable :: wh, head

        call where_text(sect, key, wh)
        if (stat == toml_stat%conversion_error) then
            head = wh // " does not fit: it must be " // expected
        else
            head = wh // " is not " // expected
        end if
        call fail_tail(sect, origin, head, "ERR: config value has the wrong type: " // key)
    end subroutine fail_value

    !> A key the program requires and the file does not set.
    subroutine fail_missing(sect, key)
        type(pf_toml), intent(in) :: sect      !! Handle being read from.
        character(len=*), intent(in) :: key    !! The absent key.
        character(len=:), allocatable :: wh, head

        call where_text(sect, key, wh)
        head = wh // " is required and the file does not set it"
        call fail_tail(sect, sect%tbl%origin, head, &
            "ERR: config key not found and no default given: " // key)
    end subroutine fail_missing

    !> A value that should have been a list and is not.
    subroutine fail_not_list(sect, key, origin)
        type(pf_toml), intent(in) :: sect      !! Handle being read from.
        character(len=*), intent(in) :: key    !! The key.
        integer, intent(in) :: origin          !! Source token of the value.
        character(len=:), allocatable :: wh, head

        call where_text(sect, key, wh)
        head = wh // " must be a list, written like [1, 2, 3]"
        call fail_tail(sect, origin, head, "ERR: config value is not a list: " // key)
    end subroutine fail_not_list

    !> A list whose length does not match the array it is read into, in either direction.
    subroutine fail_length(sect, key, origin, ngot, nwant)
        type(pf_toml), intent(in) :: sect      !! Handle being read from.
        character(len=*), intent(in) :: key    !! The key.
        integer, intent(in) :: origin          !! Source token of the value.
        integer, intent(in) :: ngot            !! How many entries the file gives.
        integer, intent(in) :: nwant           !! How many the program needs.
        character(len=:), allocatable :: wh, head

        call where_text(sect, key, wh)
        head = wh // ": the file gives " // trim(pf_str(ngot)) // " entries and the program needs " &
            // trim(pf_str(nwant))
        call fail_tail(sect, origin, head, "ERR: config list has the wrong number of entries: " // key)
    end subroutine fail_length

    !> A string list element longer than the caller's declared element length.
    subroutine fail_too_long(sect, key, origin, idx, got, cap)
        type(pf_toml), intent(in) :: sect      !! Handle being read from.
        character(len=*), intent(in) :: key    !! The key.
        integer, intent(in) :: origin          !! Source token of the value.
        integer, intent(in) :: idx             !! Which element is too long.
        integer, intent(in) :: got             !! Its length.
        integer, intent(in) :: cap             !! The caller's element length.
        character(len=:), allocatable :: wh, head

        call where_text(sect, key, wh)
        head = wh // ": entry " // trim(pf_str(idx)) // " is " // trim(pf_str(got)) &
            // " characters and will not fit in " // trim(pf_str(cap))
        call fail_tail(sect, origin, head, "ERR: config list entry is too long: " // key)
    end subroutine fail_too_long

    !> A log level name `pf_log_level_from_name` does not recognise.
    subroutine fail_level(sect, key, origin, name)
        type(pf_toml), intent(in) :: sect       !! Handle being read from.
        character(len=*), intent(in) :: key     !! The key.
        integer, intent(in) :: origin           !! Source token of the value.
        character(len=*), intent(in) :: name    !! The unrecognised name.
        character(len=:), allocatable :: wh, head

        call where_text(sect, key, wh)
        head = wh // ": unrecognised log level " // name // "; use DEBUG, INFO, WARNING, ERROR or OFF"
        call fail_tail(sect, origin, head, "ERR: unrecognised log level in the configuration file: " // name)
    end subroutine fail_level

    !> A required section the file does not have.
    subroutine fail_missing_section(parent, name, indexed)
        type(pf_toml), intent(in) :: parent     !! Handle it was looked for in.
        character(len=*), intent(in) :: name    !! Section name.
        logical, intent(in) :: indexed          !! Whether the `[[name]]` form was used.
        character(len=PF_TOML_MAX_PATH) :: full
        character(len=:), allocatable :: head

        call path_join(parent, name, full)
        if (indexed) then
            head = "The configuration file has no [[" // trim(full) // "]] entries"
        else
            head = "The configuration file has no [" // trim(full) // "] section"
        end if
        call fail_tail(parent, parent%tbl%origin, head, "ERR: config section not found: " // trim(full))
    end subroutine fail_missing_section

    !> A name that is something other than a `[name]` table.
    subroutine fail_not_a_section(parent, name)
        type(pf_toml), intent(in) :: parent     !! Handle it was looked for in.
        character(len=*), intent(in) :: name    !! The name.
        character(len=PF_TOML_MAX_PATH) :: full
        character(len=:), allocatable :: head

        call path_join(parent, name, full)
        head = trim(full) // " is a value, not a [" // trim(full) // "] section"
        call fail_tail(parent, key_origin(parent, name), head, &
            "ERR: config name is not a section: " // trim(full))
    end subroutine fail_not_a_section

    !> A name that is something other than a `[[name]]` array of tables.
    subroutine fail_not_entries(parent, name)
        type(pf_toml), intent(in) :: parent     !! Handle it was looked for in.
        character(len=*), intent(in) :: name    !! The name.
        character(len=PF_TOML_MAX_PATH) :: full
        character(len=:), allocatable :: head

        call path_join(parent, name, full)
        head = trim(full) // " is not a [[" // trim(full) // "]] array of sections"
        call fail_tail(parent, key_origin(parent, name), head, &
            "ERR: config name is not an array of sections: " // trim(full))
    end subroutine fail_not_entries

    !> A `[[name]]` entry index outside `1 .. pf_toml_section_count(...)`.
    subroutine fail_entry_range(parent, name, idx, n)
        type(pf_toml), intent(in) :: parent     !! Handle it was looked for in.
        character(len=*), intent(in) :: name    !! Array-of-tables name.
        integer, intent(in) :: idx              !! The index asked for.
        integer, intent(in) :: n                !! How many entries there are.
        character(len=PF_TOML_MAX_PATH) :: full
        character(len=:), allocatable :: head

        call path_join(parent, name, full)
        head = "[[" // trim(full) // "]] entry " // trim(pf_str(idx)) // " does not exist; the file has " &
            // trim(pf_str(n)) // " entry(s)"
        call fail_tail(parent, key_origin(parent, name), head, &
            "ERR: config section index out of range: " // trim(full))
    end subroutine fail_entry_range

    !> A composed `section.key` path longer than `PF_TOML_MAX_PATH`.
    !!
    !! Fatal rather than truncated: two distinct paths that truncate to the same text would compare
    !! equal in the accumulator, so one would hide the other from the unknown-key sweep -- which is
    !! a silent under-report, the exact failure that sweep exists to prevent.
    subroutine fail_path_too_long(parent, name, need)
        type(pf_toml), intent(in) :: parent     !! Handle the path starts from.
        character(len=*), intent(in) :: name    !! The name being appended.
        integer, intent(in) :: need             !! How many characters it would take.

        call pf_log_error("Configuration path is longer than " // trim(pf_str(PF_TOML_MAX_PATH)) &
            // " characters: it needs " // trim(pf_str(need)) // ".")
        call pf_log_error("... section: [" // trim(parent%path) // "], name: " // name(1:min(len(name), 60)))
        if (associated(parent%doc)) call pf_log_error("... configuration file: " // parent%doc%file)
        call pf_log_fatal("ERR: configuration section.key path is too long")
    end subroutine fail_path_too_long

    !> Stops on a `pf_toml_strings` element index outside `1 .. %count()`.
    subroutine strings_check_index(self, idx, who)
        class(pf_toml_strings), intent(in) :: self   !! The list.
        integer, intent(in) :: idx                   !! Index asked for.
        character(len=*), intent(in) :: who          !! Binding name, for the message.

        if (idx >= 1 .and. idx <= self%n) return
        call pf_log_error("pf_toml_strings%" // who // ": element " // trim(pf_str(idx)) // " is out of range.")
        call pf_log_error("... the list holds " // trim(pf_str(self%n)) // " element(s).")
        call pf_log_fatal("ERR: pf_toml_strings: element index out of range")
    end subroutine strings_check_index

end module parquet_toml
