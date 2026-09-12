!> Tests for `parquet_toml`: reading and writing TOML configuration files.
!!
!! **Almost every fixture is a string, not a file** (`pf_toml_loads`). That is not a shortcut: this
!! suite's tests run concurrently, so a file fixture would need a unique name per test and its own
!! cleanup, which is the whole class of failure the project's "never share a fixture file path"
!! rule exists for. A string literal needs neither. The exceptions are deliberate and few -- the
!! `pf_toml_load` path itself, the save/reload round trips, and the concurrency group.
!!
!! **The concurrency group shares ONE committed, read-only file on purpose.**
!! `test/fixtures/toml_shared.toml` is loaded by several tests at once, which is exactly the case
!! the module was asked to support: many readers, one configuration file, inside an OpenMP region.
!! Nothing writes to it, so the shared-path rule is not in play.
!!
!! **Everything fatal is an error scenario, not a test here.** A wrong-typed value, an absent
!! required key, a length mismatch, a retired key that is still set and an unknown key under the
!! default severity all abort the process, so they live in `test/error_scenarios.f90` and are
!! driven from `test/test_errors.f90`. What this file asserts about them is the other half: that
!! the same call does NOT abort when it should not, which is the negative control each of those
!! scenarios needs to mean anything. `test_check_clean_file_is_silent` is the clearest example --
!! it would abort, loudly, if the sweep reported a key the program had read.
!!
!! Two suites, and the split matters. `toml` runs concurrently. `toml_serial` is excluded from
!! test-drive's parallelism (see `test/test_runner_support.f90`) because its tests reconfigure the
!! process-global DEFAULT logger: a sink attached or closed while a sibling test is writing to it
!! is an abort, which is why `logging` and `logging_env` are excluded for the same reason.
module test_toml
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use parquet_toml
    use parquet_logging, only: pf_log_init, pf_log_add_file, pf_log_close, pf_log_flush, &
                               PF_LEVEL_WARNING, PF_LEVEL_INFO
    use iso_fortran_env, only: int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_toml, collect_tests_toml_serial

    !> The file every concurrency test loads, at the same time, on purpose.
    character(len=*), parameter :: SHARED_FIXTURE = "test/fixtures/toml_shared.toml"

contains

    !> Registers the concurrent suite.
    subroutine collect_tests_toml(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("every scalar type round-trips", test_scalar_round_trip), &
            new_unittest("a default is applied when the key is absent", test_scalar_defaults), &
            new_unittest("a real accepts a TOML integer", test_real_from_integer), &
            new_unittest("root-level keys are read from the document handle", test_root_keys), &
            new_unittest("sections nest and their paths compose", test_nested_sections), &
            new_unittest("an optional section that is absent stays closed", test_optional_section), &
            new_unittest("an optional entry index outside the range stays closed", &
                test_optional_entry_out_of_range), &
            new_unittest("section_count counts entries and answers 0 for an absent name", test_section_count), &
            new_unittest("an array of tables is read entry by entry", test_array_of_tables), &
            new_unittest("caller-sized arrays round-trip for every type", test_array_round_trip), &
            new_unittest("an absent array read with get_opt keeps its entry values", test_array_optional), &
            new_unittest("get_opt keeps the variable for every scalar type", test_get_opt_scalars), &
            new_unittest("get_opt leaves an unallocated character scalar unallocated", test_get_opt_str_unset), &
            new_unittest("a rank-1 default is applied when the key is absent", test_array_default), &
            new_unittest("get_alloc sizes the result from the file", test_get_alloc), &
            new_unittest("get_alloc_opt leaves the result unallocated when the key is absent", test_get_alloc_absent), &
            new_unittest("get_alloc_opt keeps a list the caller already held", test_get_alloc_opt_keeps_a_list), &
            new_unittest("get_opt over a list keeps the variable for every element kind", &
                test_get_opt_array_every_kind), &
            new_unittest("get_alloc sizes from the file for every element kind", &
                test_get_alloc_every_kind), &
            new_unittest("get_alloc_opt leaves an absent key unallocated for every kind", &
                test_get_alloc_opt_every_kind), &
            new_unittest("a string list keeps each element's own length", test_strings_lengths), &
            new_unittest("a string list survives closing the document", test_strings_outlive_document), &
            new_unittest("an absent optional string list has count 0", test_strings_absent), &
            new_unittest("an exact count is accepted when the list matches", test_strings_count), &
            new_unittest("a default is NEVER inserted into the parsed document", test_defaults_not_inserted), &
            new_unittest("check stays silent on a file the program fully read", test_check_clean_file_is_silent), &
            new_unittest("check stays silent after a defaulted read", test_check_silent_after_default), &
            new_unittest("check_all stays silent when everything was read", test_check_all_clean), &
            new_unittest("mark makes a hand-read key known to the sweep", test_mark), &
            new_unittest("mark_section silences a whole subtree, recursively", test_mark_section), &
            new_unittest("the validators are silent on an absent optional section", test_closed_handle_validators), &
            new_unittest("a retired key that is absent is silent", test_retire_absent), &
            new_unittest("log levels are read by name", test_get_level), &
            new_unittest("has, has_section, keys, path and filename", test_presence_helpers), &
            new_unittest("an unopened handle and a not-found section answer rather than abort", &
                test_empty_handle_queries), &
            new_unittest("the read accumulator grows past its first block", test_mark_accumulator_grows), &
            new_unittest("the escape hatches hand back the raw toml-f objects", test_escape_hatches), &
            new_unittest("loading a file that is not there is soft with status=", test_load_status_open), &
            new_unittest("text that is not TOML is soft with status=", test_load_status_parse), &
            new_unittest("a new document can be built, saved and read back", test_build_and_save), &
            new_unittest("new_section upserts, on a built and on a loaded document", test_new_section_upserts), &
            new_unittest("update changes a key and set adds one", test_set_and_update), &
            new_unittest("set and save round-trip every scalar and list kind", &
                test_set_every_kind_round_trips), &
            new_unittest("update changes a key for every scalar and list kind", &
                test_update_every_kind), &
            new_unittest("append_section builds an array of tables", test_append_section), &
            new_unittest("save writes the effective configuration", test_save_effective), &
            new_unittest("get_opt records the variable's own value for save", test_opt_reaches_the_shadow), &
            new_unittest("delete removes a key, and an absent key is a no-op", test_delete), &
            new_unittest("dump writes the parsed document, save writes the effective one", test_dump), &
            new_unittest("shared file, concurrent reader 1", test_shared_reader_1), &
            new_unittest("shared file, concurrent reader 2", test_shared_reader_2), &
            new_unittest("shared file, concurrent reader 3", test_shared_reader_3), &
            new_unittest("shared file, concurrent reader 4", test_shared_reader_4) &
            ]
    end subroutine collect_tests_toml

    !> Registers the serial suite: everything that reconfigures the process-global default logger.
    subroutine collect_tests_toml_serial(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("check_all names every shape of unread thing", &
                test_check_all_names_every_unread_shape), &
            new_unittest("a warning sweep reaches the log", test_warn_reaches_the_log), &
            new_unittest("report logs a caller's complaint against a key's line", &
                test_report_reaches_the_log) &
            ]
    end subroutine collect_tests_toml_serial

    ! ================================================================================
    ! Fixtures
    ! ================================================================================

    !> The document most tests here read. Built rather than stored so the suite needs no file.
    subroutine sample(text)
        character(len=:), allocatable, intent(out) :: text  !! Receives the TOML document.
        character(len=1) :: nl

        nl = new_line("a")
        text = 'title = "example"' // nl // &
               'count = 7' // nl // &
               '' // nl // &
               '[general]' // nl // &
               'nproc = 4' // nl // &
               'factor = 2.5' // nl // &
               'verbose = true' // nl // &
               'name = "run one"' // nl // &
               'limits = [1, 2, 3]' // nl // &
               'weights = [0.5, 1.5]' // nl // &
               'flags = [true, false]' // nl // &
               'files = ["a", "bc", "a much longer third"]' // nl // &
               'level = "WARNING"' // nl // &
               '' // nl // &
               '[general.nested]' // nl // &
               'depth = 2' // nl // &
               '' // nl // &
               '[[region]]' // nl // &
               'id = 1' // nl // &
               '[[region]]' // nl // &
               'id = 2' // nl
    end subroutine sample

    ! ================================================================================
    ! Reading scalars
    ! ================================================================================

    !> Every scalar specific reads the value the file gives, at the right type.
    subroutine test_scalar_round_trip(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text, name
        integer(int32) :: i32
        integer(int64) :: i64
        real(real32) :: r32
        real(real64) :: r64
        logical :: flag

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)

        call pf_toml_get(gen, "nproc", i32)
        call check(error, i32 == 4_int32, "nproc must read back as 4 through the int32 specific")
        if (allocated(error)) return
        call pf_toml_get(gen, "nproc", i64)
        call check(error, i64 == 4_int64, "nproc must read back as 4 through the int64 specific")
        if (allocated(error)) return
        call pf_toml_get(gen, "factor", r32)
        call check(error, abs(r32 - 2.5_real32) < 1.0e-6_real32, "factor must read back as 2.5 (real32)")
        if (allocated(error)) return
        call pf_toml_get(gen, "factor", r64)
        call check(error, abs(r64 - 2.5_real64) < 1.0e-12_real64, "factor must read back as 2.5 (real64)")
        if (allocated(error)) return
        call pf_toml_get(gen, "verbose", flag)
        call check(error, flag, "verbose must read back as .true.")
        if (allocated(error)) return
        call pf_toml_get(gen, "name", name)
        call check(error, name == "run one", "name must read back as 'run one'")
        if (allocated(error)) return
        call check(error, len(name) == 7, "a deferred-length result must be exactly the value's length")
        if (allocated(error)) return

        call pf_toml_close(conf)
    end subroutine test_scalar_round_trip

    !> An absent key takes its default, for every scalar type, and the run continues.
    subroutine test_scalar_defaults(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text, name
        integer(int32) :: i32
        integer(int64) :: i64
        real(real32) :: r32
        real(real64) :: r64
        logical :: flag

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)

        call pf_toml_get(gen, "absent_i32", i32, default = -11_int32)
        call check(error, i32 == -11_int32, "an absent int32 key must take its default")
        if (allocated(error)) return
        call pf_toml_get(gen, "absent_i64", i64, default = -12_int64)
        call check(error, i64 == -12_int64, "an absent int64 key must take its default")
        if (allocated(error)) return
        call pf_toml_get(gen, "absent_r32", r32, default = 1.5_real32)
        call check(error, abs(r32 - 1.5_real32) < 1.0e-6_real32, "an absent real32 key must take its default")
        if (allocated(error)) return
        call pf_toml_get(gen, "absent_r64", r64, default = 2.75_real64)
        call check(error, abs(r64 - 2.75_real64) < 1.0e-12_real64, "an absent real64 key must take its default")
        if (allocated(error)) return
        call pf_toml_get(gen, "absent_log", flag, default = .true.)
        call check(error, flag, "an absent logical key must take its default")
        if (allocated(error)) return
        call pf_toml_get(gen, "absent_str", name, default = "fallback")
        call check(error, name == "fallback", "an absent character key must take its default")
        if (allocated(error)) return

        call pf_toml_close(conf)
    end subroutine test_scalar_defaults

    !> A TOML integer read as a real converts, which is toml-f's own rule and is worth pinning.
    subroutine test_real_from_integer(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        real(real64) :: r64

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "nproc", r64)
        call check(error, abs(r64 - 4.0_real64) < 1.0e-12_real64, &
            "a TOML integer read as a real must convert to 4.0")
        call pf_toml_close(conf)
    end subroutine test_real_from_integer

    !> A key above the first section is read straight from the document handle.
    subroutine test_root_keys(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        character(len=:), allocatable :: text, title
        integer(int32) :: n

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_get(conf, "title", title)
        call check(error, title == "example", "a root-level key must be readable from the document handle")
        if (allocated(error)) return
        call pf_toml_get(conf, "count", n)
        call check(error, n == 7_int32, "a root-level integer must read back")
        call pf_toml_close(conf)
    end subroutine test_root_keys

    ! ================================================================================
    ! Sections
    ! ================================================================================

    !> A section of a section works, and the display path composes with a dot.
    subroutine test_nested_sections(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, nested
        character(len=:), allocatable :: text, path
        integer(int32) :: depth

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_section(gen, "nested", nested)
        call pf_toml_get(nested, "depth", depth)
        call check(error, depth == 2_int32, "a nested section's key must read back")
        if (allocated(error)) return
        call pf_toml_path(nested, path)
        call check(error, path == "general.nested", &
            "a nested section's display path must compose as general.nested, not " // path)
        if (allocated(error)) return
        call pf_toml_path(conf, path)
        call check(error, len(path) == 0, "the document handle's display path must be empty")
        call pf_toml_close(conf)
    end subroutine test_nested_sections

    !> An optional section that is not there leaves a closed handle and reports `found`.
    subroutine test_optional_section(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, missing
        character(len=:), allocatable :: text
        logical :: found

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "not_there", missing, required = .false., found = found)
        call check(error, .not. found, "found must be .false. for a section the file does not have")
        if (allocated(error)) return
        call check(error, .not. pf_toml_is_open(missing), &
            "an absent optional section must leave the handle closed")
        if (allocated(error)) return
        call check(error, pf_toml_is_open(conf), "the document handle must still be open")
        call pf_toml_close(conf)
    end subroutine test_optional_section

    !> `required = .false.` governs an out-of-range `[[name]]` index exactly as it governs an
    !> absent name: the handle comes back closed rather than the run stopping.
    !!
    !! The bare call still aborts, which `scenario_toml_entry_out_of_range` covers out of process --
    !! an abort cannot be asserted from in here. What this test pins is the half that was widened,
    !! and its **negative control is the in-range read**: without it the test would pass just as
    !! happily against an implementation that had stopped opening entries altogether, since a
    !! closed handle is exactly what that would produce too.
    subroutine test_optional_entry_out_of_range(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, ent
        character(len=:), allocatable :: text
        logical :: found
        integer :: n

        call sample(text)
        call pf_toml_loads(conf, text)
        n = pf_toml_section_count(conf, "region")
        call check(error, n >= 1, "the fixture must carry at least one [[region]] entry")
        if (allocated(error)) return

        ! Negative control first: an index that IS in range must open.
        call pf_toml_section(conf, "region", 1, ent, required = .false., found = found)
        call check(error, found, "found must be .true. for an entry the file does have")
        if (allocated(error)) return
        call check(error, pf_toml_is_open(ent), "an in-range entry must open")
        if (allocated(error)) return

        ! Past the end.
        call pf_toml_section(conf, "region", n + 1, ent, required = .false., found = found)
        call check(error, .not. found, "found must be .false. for an entry past the last one")
        if (allocated(error)) return
        call check(error, .not. pf_toml_is_open(ent), &
            "an out-of-range optional entry must leave the handle closed, not stop the run")
        if (allocated(error)) return

        ! And below the start, which is the other side of the same guard.
        call pf_toml_section(conf, "region", 0, ent, required = .false., found = found)
        call check(error, .not. found, "found must be .false. for index 0")
        if (allocated(error)) return
        call check(error, .not. pf_toml_is_open(ent), "index 0 must leave the handle closed")
        if (allocated(error)) return

        call check(error, pf_toml_is_open(conf), "the document handle must still be open")
        call pf_toml_close(conf)
    end subroutine test_optional_entry_out_of_range

    !> `[[name]]` entries are counted, and an absent name counts 0 rather than aborting.
    subroutine test_section_count(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        character(len=:), allocatable :: text

        call sample(text)
        call pf_toml_loads(conf, text)
        call check(error, pf_toml_section_count(conf, "region") == 2, &
            "the fixture has two [[region]] entries")
        if (allocated(error)) return
        call check(error, pf_toml_section_count(conf, "no_such_array") == 0, &
            "an absent [[name]] must count 0, which is what makes `do i = 1, count` the idiom")
        call pf_toml_close(conf)
    end subroutine test_section_count

    !> Each `[[name]]` entry opens by index, and its path renders as `name[k]`.
    subroutine test_array_of_tables(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, ent
        character(len=:), allocatable :: text, path
        integer(int32) :: id, i

        call sample(text)
        call pf_toml_loads(conf, text)
        do i = 1, pf_toml_section_count(conf, "region")
            call pf_toml_section(conf, "region", i, ent)
            call pf_toml_get(ent, "id", id)
            call check(error, id == i, "entry i must carry id = i")
            if (allocated(error)) return
        end do
        call pf_toml_section(conf, "region", 2, ent)
        call pf_toml_path(ent, path)
        call check(error, path == "region[2]", "an entry's display path must render as region[2], not " // path)
        call pf_toml_close(conf)
    end subroutine test_array_of_tables

    ! ================================================================================
    ! Reading arrays
    ! ================================================================================

    !> A caller-sized array of each type reads the file's list of exactly that length.
    subroutine test_array_round_trip(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer(int32) :: i32(3)
        integer(int64) :: i64(3)
        real(real32) :: r32(2)
        real(real64) :: r64(2)
        logical :: flags(2)
        character(len=24) :: files(3)

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)

        call pf_toml_get(gen, "limits", i32)
        call check(error, all(i32 == [1_int32, 2_int32, 3_int32]), "limits must read back as 1, 2, 3")
        if (allocated(error)) return
        call pf_toml_get(gen, "limits", i64)
        call check(error, all(i64 == [1_int64, 2_int64, 3_int64]), "limits must read back as int64 too")
        if (allocated(error)) return
        call pf_toml_get(gen, "weights", r32)
        call check(error, all(abs(r32 - [0.5_real32, 1.5_real32]) < 1.0e-6_real32), &
            "weights must read back as 0.5, 1.5 (real32)")
        if (allocated(error)) return
        call pf_toml_get(gen, "weights", r64)
        call check(error, all(abs(r64 - [0.5_real64, 1.5_real64]) < 1.0e-12_real64), &
            "weights must read back as 0.5, 1.5 (real64)")
        if (allocated(error)) return
        call pf_toml_get(gen, "flags", flags)
        call check(error, flags(1) .and. .not. flags(2), "flags must read back as true, false")
        if (allocated(error)) return
        ! The first element is deliberately the SHORTEST, which is the standing regression shape
        ! for a per-element length derived from element one.
        call pf_toml_get(gen, "files", files)
        call check(error, trim(files(1)) == "a", "files(1) must be 'a'")
        if (allocated(error)) return
        call check(error, trim(files(2)) == "bc", "files(2) must be 'bc', not truncated to one character")
        if (allocated(error)) return
        call check(error, trim(files(3)) == "a much longer third", &
            "files(3) must survive in full, not be clipped to the first element's length")
        call pf_toml_close(conf)
    end subroutine test_array_round_trip

    !> `pf_toml_get_opt` leaves whatever the variable already held when the key is absent.
    subroutine test_array_optional(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        real(real64) :: r64(2)

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        r64 = [9.0_real64, 8.0_real64]
        call pf_toml_get_opt(gen, "no_such_list", r64)
        call check(error, all(abs(r64 - [9.0_real64, 8.0_real64]) < 1.0e-12_real64), &
            "an absent optional list must leave the caller's own values in place")
        if (allocated(error)) return
        ! Negative control: the same call on a key the file DOES set must overwrite them.
        call pf_toml_get_opt(gen, "weights", r64)
        call check(error, all(abs(r64 - [0.5_real64, 1.5_real64]) < 1.0e-12_real64), &
            "a present list must overwrite the variable's entry values")
        call pf_toml_close(conf)
    end subroutine test_array_optional

    !> `pf_toml_get_opt` over a caller-sized list, for every element kind.
    !!
    !! `test_array_optional` establishes what the optional list form means, on `real64`. Each kind
    !! is a separate specific with its own extraction and its own copy back into the caller's
    !! array, so the property worth re-asserting per kind is the one that makes this form useful:
    !! an absent key leaves the caller's OWN entry values standing. A specific that zeroed the
    !! array before looking, or that wrote a partial result on the absent path, hands back a
    !! plausible list rather than failing -- which is why each kind is asked both questions, with
    !! the present-key read as the negative control for the absent-key one.
    subroutine test_get_opt_array_every_kind(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        block
            integer(int32) :: v(3)
            v = [-1_int32, -2_int32, -3_int32]
            call pf_toml_get_opt(gen, "no_such_list", v)
            call check(error, all(v == [-1_int32, -2_int32, -3_int32]), &
                "int32: an absent optional list must leave the caller's values")
            if (allocated(error)) return
            call pf_toml_get_opt(gen, "limits", v)
            call check(error, all(v == [1_int32, 2_int32, 3_int32]), &
                "int32: a present list must overwrite them")
        end block
        if (allocated(error)) return
        block
            integer(int64) :: v(3)
            v = [-1_int64, -2_int64, -3_int64]
            call pf_toml_get_opt(gen, "no_such_list", v)
            call check(error, all(v == [-1_int64, -2_int64, -3_int64]), &
                "int64: an absent optional list must leave the caller's values")
            if (allocated(error)) return
            call pf_toml_get_opt(gen, "limits", v)
            call check(error, all(v == [1_int64, 2_int64, 3_int64]), &
                "int64: a present list must overwrite them")
        end block
        if (allocated(error)) return
        block
            real(real32) :: v(2)
            v = [-1.0_real32, -2.0_real32]
            call pf_toml_get_opt(gen, "no_such_list", v)
            call check(error, all(abs(v - [-1.0_real32, -2.0_real32]) < 1.0e-6_real32), &
                "real32: an absent optional list must leave the caller's values")
            if (allocated(error)) return
            call pf_toml_get_opt(gen, "weights", v)
            call check(error, all(abs(v - [0.5_real32, 1.5_real32]) < 1.0e-6_real32), &
                "real32: a present list must overwrite them")
        end block
        if (allocated(error)) return
        block
            logical :: v(2)
            v = [.false., .true.]
            call pf_toml_get_opt(gen, "no_such_list", v)
            call check(error, all(v .eqv. [.false., .true.]), &
                "logical: an absent optional list must leave the caller's values")
            if (allocated(error)) return
            call pf_toml_get_opt(gen, "flags", v)
            call check(error, all(v .eqv. [.true., .false.]), &
                "logical: a present list must overwrite them")
        end block
        if (allocated(error)) return
        block
            ! Deliberately shortest first, as `test_build_and_save` is: the read side must not
            ! size every element from the first one it sees.
            character(len=24) :: v(3)
            v(1) = "kept one"
            v(2) = "kept two"
            v(3) = "kept three"
            call pf_toml_get_opt(gen, "no_such_list", v)
            call check(error, v(1) == "kept one" .and. v(3) == "kept three", &
                "character: an absent optional list must leave the caller's values")
            if (allocated(error)) return
            call pf_toml_get_opt(gen, "files", v)
            call check(error, v(1) == "a" .and. v(2) == "bc" .and. v(3) == "a much longer third", &
                "character: a present list must overwrite them, each at its own length")
        end block
        if (allocated(error)) return
        call pf_toml_close(conf)
    end subroutine test_get_opt_array_every_kind

    !> `pf_toml_get_alloc` takes its size from the file, for every element kind.
    !!
    !! `test_get_alloc` establishes the form on `int32`. The size coming from the FILE is the whole
    !! point, so each kind is read into a variable deliberately left unallocated and the length is
    !! asserted as well as the values -- a specific that sized from anything else would still fill
    !! the elements it did allocate.
    subroutine test_get_alloc_every_kind(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        block
            integer(int32), allocatable :: v(:)
            call pf_toml_get_alloc(gen, "limits", v)
            call check(error, allocated(v), "int32: get_alloc must allocate the result")
            if (allocated(error)) return
            call check(error, size(v) == 3 .and. all(v == [1_int32, 2_int32, 3_int32]), &
                "int32: get_alloc must size and fill from the file")
        end block
        if (allocated(error)) return
        block
            integer(int64), allocatable :: v(:)
            call pf_toml_get_alloc(gen, "limits", v)
            call check(error, allocated(v), "int64: get_alloc must allocate the result")
            if (allocated(error)) return
            call check(error, size(v) == 3 .and. all(v == [1_int64, 2_int64, 3_int64]), &
                "int64: get_alloc must size and fill from the file")
        end block
        if (allocated(error)) return
        block
            real(real32), allocatable :: v(:)
            call pf_toml_get_alloc(gen, "weights", v)
            call check(error, allocated(v), "real32: get_alloc must allocate the result")
            if (allocated(error)) return
            call check(error, size(v) == 2 .and. &
                all(abs(v - [0.5_real32, 1.5_real32]) < 1.0e-6_real32), &
                "real32: get_alloc must size and fill from the file")
        end block
        if (allocated(error)) return
        block
            real(real64), allocatable :: v(:)
            call pf_toml_get_alloc(gen, "weights", v)
            call check(error, allocated(v), "real64: get_alloc must allocate the result")
            if (allocated(error)) return
            call check(error, size(v) == 2 .and. &
                all(abs(v - [0.5_real64, 1.5_real64]) < 1.0e-12_real64), &
                "real64: get_alloc must size and fill from the file")
        end block
        if (allocated(error)) return
        block
            logical, allocatable :: v(:)
            call pf_toml_get_alloc(gen, "flags", v)
            call check(error, allocated(v), "logical: get_alloc must allocate the result")
            if (allocated(error)) return
            call check(error, size(v) == 2 .and. all(v .eqv. [.true., .false.]), &
                "logical: get_alloc must size and fill from the file")
        end block
        if (allocated(error)) return
        call pf_toml_close(conf)
    end subroutine test_get_alloc_every_kind

    !> `pf_toml_get_alloc_opt` for every element kind: absent leaves the variable exactly as found.
    !!
    !! `allocated(values)` is the caller's answer to "did the file set this key?", so the assertion
    !! that matters per kind is that an absent key leaves an unallocated variable UNALLOCATED --
    !! a specific that allocated a zero-length array instead would make an absent key and an empty
    !! list indistinguishable, which is the one thing this form exists to keep apart.
    subroutine test_get_alloc_opt_every_kind(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        block
            integer(int32), allocatable :: v(:)
            call pf_toml_get_alloc_opt(gen, "no_such_list", v)
            call check(error, .not. allocated(v), &
                "int32: an absent key must leave the variable unallocated")
            if (allocated(error)) return
            call pf_toml_get_alloc_opt(gen, "limits", v)
            call check(error, allocated(v), "int32: a present key must allocate it")
            if (allocated(error)) return
            call check(error, size(v) == 3 .and. all(v == [1_int32, 2_int32, 3_int32]), &
                "int32: and fill it from the file")
        end block
        if (allocated(error)) return
        block
            integer(int64), allocatable :: v(:)
            call pf_toml_get_alloc_opt(gen, "no_such_list", v)
            call check(error, .not. allocated(v), &
                "int64: an absent key must leave the variable unallocated")
            if (allocated(error)) return
            call pf_toml_get_alloc_opt(gen, "limits", v)
            call check(error, allocated(v), "int64: a present key must allocate it")
            if (allocated(error)) return
            call check(error, size(v) == 3 .and. all(v == [1_int64, 2_int64, 3_int64]), &
                "int64: and fill it from the file")
        end block
        if (allocated(error)) return
        block
            real(real32), allocatable :: v(:)
            call pf_toml_get_alloc_opt(gen, "no_such_list", v)
            call check(error, .not. allocated(v), &
                "real32: an absent key must leave the variable unallocated")
            if (allocated(error)) return
            call pf_toml_get_alloc_opt(gen, "weights", v)
            call check(error, allocated(v), "real32: a present key must allocate it")
            if (allocated(error)) return
            call check(error, size(v) == 2 .and. &
                all(abs(v - [0.5_real32, 1.5_real32]) < 1.0e-6_real32), &
                "real32: and fill it from the file")
        end block
        if (allocated(error)) return
        block
            real(real64), allocatable :: v(:)
            call pf_toml_get_alloc_opt(gen, "no_such_list", v)
            call check(error, .not. allocated(v), &
                "real64: an absent key must leave the variable unallocated")
            if (allocated(error)) return
            call pf_toml_get_alloc_opt(gen, "weights", v)
            call check(error, allocated(v) .and. size(v) == 2, &
                "real64: a present key must allocate and size it")
        end block
        if (allocated(error)) return
        block
            logical, allocatable :: v(:)
            call pf_toml_get_alloc_opt(gen, "no_such_list", v)
            call check(error, .not. allocated(v), &
                "logical: an absent key must leave the variable unallocated")
            if (allocated(error)) return
            call pf_toml_get_alloc_opt(gen, "flags", v)
            call check(error, allocated(v), "logical: a present key must allocate it")
            if (allocated(error)) return
            call check(error, size(v) == 2 .and. all(v .eqv. [.true., .false.]), &
                "logical: and fill it from the file")
        end block
        if (allocated(error)) return
        call pf_toml_close(conf)
    end subroutine test_get_alloc_opt_every_kind

    !> `pf_toml_get_alloc` takes its size from the file rather than from the caller.
    subroutine test_get_alloc(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer(int32), allocatable :: ints(:)
        real(real64), allocatable :: reals(:)
        logical, allocatable :: flags(:)

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_alloc(gen, "limits", ints)
        call check(error, allocated(ints), "get_alloc must allocate the result")
        if (allocated(error)) return
        call check(error, size(ints) == 3, "get_alloc must size the result from the file")
        if (allocated(error)) return
        call check(error, all(ints == [1_int32, 2_int32, 3_int32]), "get_alloc must read the values")
        if (allocated(error)) return
        call pf_toml_get_alloc(gen, "weights", reals)
        call check(error, size(reals) == 2, "get_alloc must size a real list from the file")
        if (allocated(error)) return
        call pf_toml_get_alloc(gen, "flags", flags)
        call check(error, size(flags) == 2, "get_alloc must size a logical list from the file")
        call pf_toml_close(conf)
    end subroutine test_get_alloc

    !> An absent optional list leaves the result UNALLOCATED, which is part of the contract.
    subroutine test_get_alloc_absent(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer(int32), allocatable :: ints(:)

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_alloc_opt(gen, "no_such_list", ints)
        call check(error, .not. allocated(ints), &
            "an absent optional list must leave get_alloc_opt's result unallocated")
        if (allocated(error)) return
        ! Negative control: the same variable, a key that IS there.
        call pf_toml_get_alloc_opt(gen, "limits", ints)
        call check(error, allocated(ints), "a present list must allocate the result")
        call pf_toml_close(conf)
    end subroutine test_get_alloc_absent

    !> `pf_toml_get_alloc_opt` leaves a result that is ALREADY allocated exactly as it found it.
    !!
    !! The SIZE assertion is the one that carries the test. Asserting only `allocated(values)`
    !! would pass against an implementation that reallocated the variable to some other length and
    !! refilled it, which is why the fixture carries two integer lists of different lengths and the
    !! negative control reads the shorter one into the same variable -- without that arm, "the
    !! variable is unchanged" is indistinguishable from "this procedure never writes at all".
    subroutine test_get_alloc_opt_keeps_a_list(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer(int32), allocatable :: ints(:)
        real(real64), allocatable :: reals(:)
        character(len=1) :: nl

        nl = new_line("a")
        text = '[general]' // nl // &
               'short = [7]' // nl // &
               'limits = [1, 2, 3]' // nl // &
               'weights = [0.5, 1.5]' // nl
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)

        call pf_toml_get_alloc(gen, "limits", ints)
        call check(error, size(ints) == 3, "the fixture's list must arrive at length 3")
        if (allocated(error)) return
        call pf_toml_get_alloc_opt(gen, "no_such_list", ints)
        call check(error, allocated(ints), "an absent key must leave an allocated list allocated")
        if (allocated(error)) return
        call check(error, size(ints) == 3, &
            "an absent key must not resize a list the caller already held")
        if (allocated(error)) return
        call check(error, all(ints == [1_int32, 2_int32, 3_int32]), &
            "an absent key must not change the contents of a list the caller already held")
        if (allocated(error)) return

        ! Negative control: a key that IS there replaces the length and the values alike.
        call pf_toml_get_alloc_opt(gen, "short", ints)
        call check(error, size(ints) == 1, "a present key must resize an already-allocated list")
        if (allocated(error)) return
        call check(error, ints(1) == 7_int32, "a present key must overwrite the values too")
        if (allocated(error)) return

        ! The page states this of the generic, not of one specific -- a second of the five.
        call pf_toml_get_alloc(gen, "weights", reals)
        call pf_toml_get_alloc_opt(gen, "no_such_list", reals)
        call check(error, allocated(reals), "the real64 specific must keep an allocated list too")
        if (allocated(error)) return
        call check(error, size(reals) == 2, "the real64 specific must keep the list's length")
        if (allocated(error)) return
        call check(error, abs(reals(1) - 0.5_real64) < 1.0e-12_real64, &
            "the real64 specific must keep the list's values")
        call pf_toml_close(conf)
    end subroutine test_get_alloc_opt_keeps_a_list

    ! ================================================================================
    ! String lists
    ! ================================================================================

    !> Each element comes back at its own exact length, with the SHORTEST element first.
    subroutine test_strings_lengths(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        type(pf_toml_strings) :: files
        character(len=:), allocatable :: text, one

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_strings(gen, "files", files)

        call check(error, files%count() == 3, "the list has three elements")
        if (allocated(error)) return
        call files%get(1, one)
        call check(error, one == "a" .and. len(one) == 1, "element 1 must be 'a', at length 1")
        if (allocated(error)) return
        call files%get(2, one)
        call check(error, one == "bc" .and. len(one) == 2, &
            "element 2 must be 'bc' at length 2 -- not sized from element 1")
        if (allocated(error)) return
        call files%get(3, one)
        call check(error, one == "a much longer third" .and. len(one) == 19, &
            "element 3 must survive in full at length 19")
        if (allocated(error)) return
        call check(error, files%length(3) == 19, "%length must answer without fetching the element")
        if (allocated(error)) return
        call check(error, files%length(1) == 1, "%length must answer 1 for the shortest element")
        call pf_toml_close(conf)
    end subroutine test_strings_lengths

    !> The list copies its strings out, so it is still readable after the document is closed.
    subroutine test_strings_outlive_document(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        type(pf_toml_strings) :: files
        character(len=:), allocatable :: text, one

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_strings(gen, "files", files)
        call pf_toml_close(conf)

        call check(error, files%count() == 3, "the count must survive pf_toml_close")
        if (allocated(error)) return
        call files%get(3, one)
        call check(error, one == "a much longer third", "the strings must survive pf_toml_close")
    end subroutine test_strings_outlive_document

    !> An absent optional list is a usable object with no elements, not an error.
    subroutine test_strings_absent(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        type(pf_toml_strings) :: files
        character(len=:), allocatable :: text
        integer :: i, n

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_strings_opt(gen, "no_such_files", files)
        call check(error, files%count() == 0, "an absent optional string list must have count 0")
        if (allocated(error)) return
        n = 0
        do i = 1, files%count()
            n = n + 1
        end do
        call check(error, n == 0, "the do-loop idiom over an absent list must run zero times")
        call pf_toml_close(conf)
    end subroutine test_strings_absent

    !> `pf_toml_get_opt` overwrites when the file sets the key and keeps the variable when it does
    !> not -- asserted for all six scalar types, each with its own negative control.
    subroutine test_get_opt_scalars(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text, name
        integer(int32) :: i32
        integer(int64) :: i64
        real(real32) :: r32
        real(real64) :: r64
        logical :: flag

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)

        ! Absent keys: every variable must come back untouched.
        i32 = 11; i64 = 12_int64; r32 = 1.5_real32; r64 = 2.5_real64; flag = .true.
        name = "kept"
        call pf_toml_get_opt(gen, "no_such_i32", i32)
        call pf_toml_get_opt(gen, "no_such_i64", i64)
        call pf_toml_get_opt(gen, "no_such_r32", r32)
        call pf_toml_get_opt(gen, "no_such_r64", r64)
        call pf_toml_get_opt(gen, "no_such_log", flag)
        call pf_toml_get_opt(gen, "no_such_str", name)
        call check(error, i32 == 11 .and. i64 == 12_int64, "an absent key must leave both integer kinds alone")
        if (allocated(error)) return
        call check(error, abs(r32 - 1.5_real32) < 1.0e-6_real32 .and. abs(r64 - 2.5_real64) < 1.0e-12_real64, &
            "an absent key must leave both real kinds alone")
        if (allocated(error)) return
        call check(error, flag .and. name == "kept", "an absent key must leave a logical and a string alone")
        if (allocated(error)) return

        ! Negative control: the same calls on keys the file DOES set must overwrite every one.
        call pf_toml_get_opt(gen, "nproc", i32)
        call pf_toml_get_opt(gen, "nproc", i64)
        call pf_toml_get_opt(gen, "factor", r32)
        call pf_toml_get_opt(gen, "factor", r64)
        call pf_toml_get_opt(gen, "verbose", flag)
        call pf_toml_get_opt(gen, "name", name)
        call check(error, i32 == 4 .and. i64 == 4_int64, "a present key must overwrite both integer kinds")
        if (allocated(error)) return
        call check(error, abs(r32 - 2.5_real32) < 1.0e-6_real32 .and. abs(r64 - 2.5_real64) < 1.0e-12_real64, &
            "a present key must overwrite both real kinds")
        if (allocated(error)) return
        call check(error, flag .and. name == "run one", "a present key must overwrite a logical and a string")
        call pf_toml_close(conf)
    end subroutine test_get_opt_scalars

    !> The one contract `pf_toml_get_opt` adds: an unallocated character scalar the file does not
    !> set stays unallocated, so `allocated()` answers "does this value exist at all?".
    subroutine test_get_opt_str_unset(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text, name

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_opt(gen, "no_such_str", name)
        call check(error, .not. allocated(name), &
            "an absent key must leave an unallocated character scalar unallocated")
        if (allocated(error)) return
        ! Negative control: a key that IS there must allocate it, at the value's own length.
        call pf_toml_get_opt(gen, "name", name)
        call check(error, allocated(name), "a present key must allocate the result")
        if (allocated(error)) return
        call check(error, name == "run one", "a present key must give the value its own length")
        call pf_toml_close(conf)
    end subroutine test_get_opt_str_unset

    !> A rank-1 `default =` fills the array when the key is absent, and is ignored when it is not.
    subroutine test_array_default(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer(int32) :: i32(3)
        integer(int64) :: i64(2)
        real(real32) :: r32(2)
        real(real64) :: r64(2)
        logical :: flags(2)
        character(len=8) :: names(3)

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)

        call pf_toml_get(gen, "no_such_list", i32, default = [7, 8, 9])
        call check(error, all(i32 == [7, 8, 9]), "an absent list must take the rank-1 default")
        if (allocated(error)) return
        ! The other two numeric kinds. Each array getter carries its own default arm and its own
        ! length check, so neither is vouched for by the int32 and real64 calls beside it.
        call pf_toml_get(gen, "no_such_list", i64, default = [70_int64, 80_int64])
        call check(error, all(i64 == [70_int64, 80_int64]), &
            "an absent int64 list must take the rank-1 default")
        if (allocated(error)) return
        call pf_toml_get(gen, "no_such_list", r32, default = [0.5_real32, 1.5_real32])
        call check(error, all(abs(r32 - [0.5_real32, 1.5_real32]) < 1.0e-6_real32), &
            "an absent real32 list must take the rank-1 default")
        if (allocated(error)) return
        call pf_toml_get(gen, "no_such_list", r64, default = [1.25_real64, 2.75_real64])
        call check(error, all(abs(r64 - [1.25_real64, 2.75_real64]) < 1.0e-12_real64), &
            "an absent real list must take the rank-1 default")
        if (allocated(error)) return
        call pf_toml_get(gen, "no_such_list", flags, default = [.true., .false.])
        call check(error, flags(1) .and. .not. flags(2), "an absent logical list must take the rank-1 default")
        if (allocated(error)) return
        call pf_toml_get(gen, "no_such_list", names, default = ["one     ", "two     ", "three   "])
        call check(error, trim(names(3)) == "three", "an absent string list must take the rank-1 default")
        if (allocated(error)) return

        ! Negative control: with the key present the default must be ignored entirely.
        call pf_toml_get(gen, "limits", i32, default = [7, 8, 9])
        call check(error, all(i32 == [1, 2, 3]), "a present list must be read rather than defaulted")
        call pf_toml_close(conf)
    end subroutine test_array_default

    !> `count` accepts a list of the right length, and does not fire for an absent optional one.
    subroutine test_strings_count(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        type(pf_toml_strings) :: files, fresh
        character(len=:), allocatable :: text, one

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_strings(gen, "files", files, count = 3)
        call check(error, files%count() == 3, "count = 3 must accept the three-element list")
        if (allocated(error)) return
        call files%get(2, one)
        call check(error, one == "bc", "the list must still be read correctly when count is given")
        if (allocated(error)) return
        ! An absent optional list is not measured against `count`: the caller already said it may
        ! not be there. A FRESH variable is used here rather than `files`, because the whole point
        ! of the optional form is that it leaves what it found -- which the next check asserts.
        call pf_toml_get_strings_opt(gen, "no_such_files", fresh, count = 3)
        call check(error, fresh%count() == 0, "count must not fire for an absent optional list")
        if (allocated(error)) return
        call pf_toml_get_strings_opt(gen, "no_such_files", files, count = 3)
        call check(error, files%count() == 3, &
            "an absent optional list must leave a variable that already held one alone")
        call pf_toml_close(conf)
    end subroutine test_strings_count

    !> `pf_toml_mark_section` silences a section and everything below it, sub-tables included.
    subroutine test_mark_section(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, reg
        character(len=:), allocatable :: text
        integer :: i, id

        call sample(text)
        call pf_toml_loads(conf, text)
        ! Read the root keys and the array of tables, but mark [general] rather than reading it.
        call pf_toml_mark(conf, "title")
        call pf_toml_mark(conf, "count")
        call pf_toml_section(conf, "general", gen)
        call pf_toml_mark_section(gen)
        do i = 1, pf_toml_section_count(conf, "region")
            call pf_toml_section(conf, "region", i, reg)
            call pf_toml_get(reg, "id", id)
        end do
        ! Would abort if [general]'s nine keys or its [general.nested] sub-table were reported,
        ! which is what makes this a test of the RECURSION rather than only of the first level.
        call pf_toml_check_all(conf)
        call check(error, .true., "check_all must be silent over a marked section and its sub-tables")
        call pf_toml_close(conf)
    end subroutine test_mark_section

    !> The five validators do nothing on the handle of an absent optional section, and still act
    !> on an open one -- the negative control without which "silent" proves nothing.
    subroutine test_closed_handle_validators(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, missing
        character(len=:), allocatable :: text
        logical :: found

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "not_there", missing, required = .false., found = found)
        call check(error, .not. found, "the fixture must not carry the optional section")
        if (allocated(error)) return
        ! None of these may abort. A section that does not exist has nothing to sweep, mark or warn
        ! about, and a caller cannot know before opening whether it is there.
        call pf_toml_check(missing)
        call pf_toml_retire(missing, "old_key", "use new_key instead")
        call pf_toml_mark(missing, "anything")
        call pf_toml_mark_section(missing)
        call check(error, pf_toml_section_count(missing, "sub") == 0, &
            "section_count on a closed parent must answer 0")
        if (allocated(error)) return
        ! Negative control: on an OPEN section the same procedures must still do their work, or
        ! five unconditional no-ops would pass everything above.
        call pf_toml_section(conf, "general", gen)
        call pf_toml_mark_section(gen)
        call pf_toml_check(gen)
        call check(error, pf_toml_section_count(conf, "region") == 2, &
            "section_count on an open parent must still count the entries")
        call pf_toml_close(conf)
    end subroutine test_closed_handle_validators

    !> A value `pf_toml_get_opt` took from the variable rather than from the file is still the
    !> value the run used, so `pf_toml_save` must write it.
    subroutine test_opt_reaches_the_shadow(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, back, gback
        character(len=:), allocatable :: text
        character(len=*), parameter :: out_file = "test_run/toml_opt_shadow.toml"
        integer :: kept

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        kept = 42
        call pf_toml_get_opt(gen, "no_such_key", kept)
        call pf_toml_save(conf, out_file)
        call pf_toml_close(conf)

        call pf_toml_load(back, out_file)
        call pf_toml_section(back, "general", gback)
        kept = 0
        call pf_toml_get(gback, "no_such_key", kept)
        call check(error, kept == 42, "save must record the value get_opt resolved from the variable")
        call pf_toml_close(back)
    end subroutine test_opt_reaches_the_shadow

    !> `pf_toml_delete` removes a key from the parsed AND the effective document, and does nothing
    !> at all when the key is not there.
    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, back, gback
        character(len=:), allocatable :: text
        character(len=*), parameter :: out_file = "test_run/toml_delete.toml"
        integer :: nproc

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        ! Read the key first, so the effective document holds it and the shadow delete is load
        ! bearing: without it, save would write the value back out.
        call pf_toml_get(gen, "nproc", nproc)
        call check(error, nproc == 4, "the fixture must set general.nproc")
        if (allocated(error)) return
        call pf_toml_delete(gen, "nproc")
        call check(error, .not. pf_toml_has(gen, "nproc"), "delete must remove the key from the document")
        if (allocated(error)) return
        ! A key that is not there: no abort, and nothing else disturbed.
        call pf_toml_delete(gen, "never_was_here")
        call pf_toml_delete(gen, "nproc")
        call check(error, pf_toml_has(gen, "factor"), "deleting an absent key must disturb nothing else")
        if (allocated(error)) return
        call pf_toml_save(conf, out_file)
        call pf_toml_close(conf)

        call pf_toml_load(back, out_file)
        call pf_toml_section(back, "general", gback)
        call check(error, .not. pf_toml_has(gback, "nproc"), &
            "a deleted key must not be written back out by save")
        call pf_toml_close(back)
    end subroutine test_delete

    !> `pf_toml_dump` writes the document as parsed, where `pf_toml_save` writes what was read.
    subroutine test_dump(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, back, gback
        character(len=:), allocatable :: text, name
        character(len=*), parameter :: out_file = "test_run/toml_dump.toml"
        integer :: nproc

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        ! Edit one key and dump WITHOUT reading anything else.
        call pf_toml_update(gen, "nproc", 16)
        call pf_toml_dump(conf, out_file)
        call pf_toml_close(conf)

        call pf_toml_load(back, out_file)
        call pf_toml_section(back, "general", gback)
        call pf_toml_get(gback, "nproc", nproc)
        call check(error, nproc == 16, "dump must carry the update through")
        if (allocated(error)) return
        ! The discriminator against save: a key nobody read is still there.
        call pf_toml_get(gback, "name", name)
        call check(error, name == "run one", &
            "dump must carry a key the program never read, which save would have dropped")
        if (allocated(error)) return
        call check(error, pf_toml_has_section(back, "general"), "dump must carry the sections too")
        if (allocated(error)) return
        call pf_toml_mark_section(gback)
        call pf_toml_mark(back, "title")
        call pf_toml_mark(back, "count")
        call pf_toml_mark_section(back)
        call pf_toml_close(back)
    end subroutine test_dump

    ! ================================================================================
    ! The property everything else rests on
    ! ================================================================================

    !> Reading a key with a default must NOT insert it into the parsed document.
    !!
    !! This is the rule that makes the unknown-key sweep correct whenever it is called. toml-f's
    !! own `get_value(..., default=)` inserts, and nothing else in this suite would notice if this
    !! module started doing the same: every value would still be right, and the sweep would simply
    !! stop reporting anything, on every file, for every project.
    subroutine test_defaults_not_inserted(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        character(len=PF_TOML_MAX_KEY), allocatable :: keys(:)
        integer(int32) :: n
        integer :: i
        logical :: seen

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "absent_with_default", n, default = 42_int32)
        call check(error, n == 42_int32, "the default must be applied")
        if (allocated(error)) return

        call pf_toml_keys(gen, keys)
        seen = .false.
        do i = 1, size(keys)
            if (trim(keys(i)) == "absent_with_default") seen = .true.
        end do
        call check(error, .not. seen, &
            "a defaulted read must leave the parsed document unchanged -- the key must NOT appear")
        if (allocated(error)) return
        ! Negative control: a key the file really does set IS in the list, so the search works.
        seen = .false.
        do i = 1, size(keys)
            if (trim(keys(i)) == "nproc") seen = .true.
        end do
        call check(error, seen, "the key list must contain a key the file does set")
        call pf_toml_close(conf)
    end subroutine test_defaults_not_inserted

    ! ================================================================================
    ! Validation, from the side that must NOT fire
    ! ================================================================================

    !> A section whose every key was read passes the default (FATAL) sweep without aborting.
    !!
    !! This is the negative control for every unknown-key error scenario: a sweep that reported
    !! something here would abort the process, and this test would fail as loudly as a test can.
    subroutine test_check_clean_file_is_silent(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text, name
        integer(int32) :: a, b

        text = 'a = 1' // new_line("a") // '[general]' // new_line("a") // 'b = 2' // new_line("a") // &
               'name = "x"' // new_line("a")
        call pf_toml_loads(conf, text)
        call pf_toml_get(conf, "a", a)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "b", b)
        call pf_toml_get(gen, "name", name)
        call pf_toml_check(gen)
        call pf_toml_check(conf)
        call check(error, a == 1_int32 .and. b == 2_int32, &
            "the sweep must not abort on a file the program fully read")
        call pf_toml_close(conf)
    end subroutine test_check_clean_file_is_silent

    !> The sweep is still correct AFTER a defaulted read, which is what rule 7.1 buys.
    subroutine test_check_silent_after_default(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer(int32) :: b, c

        text = '[general]' // new_line("a") // 'b = 2' // new_line("a")
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "b", b)
        call pf_toml_get(gen, "c", c, default = 3_int32)
        call pf_toml_check(gen)
        call check(error, b == 2_int32 .and. c == 3_int32, &
            "a sweep run after a defaulted read must still not abort")
        call pf_toml_close(conf)
    end subroutine test_check_silent_after_default

    !> `pf_toml_check_all` is silent when every section, entry and key was read.
    subroutine test_check_all_clean(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, ent
        character(len=:), allocatable :: text
        integer(int32) :: a, b, id, i

        text = 'a = 1' // new_line("a") // '[general]' // new_line("a") // 'b = 2' // new_line("a") // &
               '[[region]]' // new_line("a") // 'id = 1' // new_line("a") // &
               '[[region]]' // new_line("a") // 'id = 2' // new_line("a")
        call pf_toml_loads(conf, text)
        call pf_toml_get(conf, "a", a)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "b", b)
        do i = 1, pf_toml_section_count(conf, "region")
            call pf_toml_section(conf, "region", i, ent)
            call pf_toml_get(ent, "id", id)
        end do
        call pf_toml_check_all(conf)
        call check(error, a == 1_int32 .and. b == 2_int32 .and. id == 2_int32, &
            "check_all must not abort when the program read everything")
        call pf_toml_close(conf)
    end subroutine test_check_all_clean

    !> A key read by hand and declared with `pf_toml_mark` is not reported by the sweep.
    subroutine test_mark(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer(int32) :: b

        text = '[general]' // new_line("a") // 'b = 2' // new_line("a") // 'hand = 9' // new_line("a")
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "b", b)
        call pf_toml_mark(gen, "hand")
        call pf_toml_check(gen)
        call check(error, b == 2_int32, "a marked key must not be reported by the sweep")
        call pf_toml_close(conf)
    end subroutine test_mark

    !> A retired key the file does not set costs nothing and says nothing.
    subroutine test_retire_absent(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer(int32) :: b

        text = '[general]' // new_line("a") // 'b = 2' // new_line("a")
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_retire(gen, "old_key", "set b instead")
        call pf_toml_get(gen, "b", b)
        call pf_toml_check(gen)
        call check(error, b == 2_int32, "retiring an absent key must be silent")
        call pf_toml_close(conf)
    end subroutine test_retire_absent

    !> Every documented level name converts, and a default name is accepted the same way.
    subroutine test_get_level(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer :: level

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get_level(gen, "level", level)
        call check(error, level == PF_LEVEL_WARNING, "level = 'WARNING' must convert to PF_LEVEL_WARNING")
        if (allocated(error)) return
        call pf_toml_get_level(gen, "absent_level", level, default = "INFO")
        call check(error, level == PF_LEVEL_INFO, "an absent level key must take its default NAME")
        call pf_toml_close(conf)
    end subroutine test_get_level

    ! ================================================================================
    ! Presence and escape hatches
    ! ================================================================================

    !> The small query surface answers correctly and marks nothing.
    subroutine test_presence_helpers(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text, name
        character(len=PF_TOML_MAX_KEY), allocatable :: keys(:)

        call sample(text)
        call pf_toml_loads(conf, text, name = "unit-test")
        call pf_toml_section(conf, "general", gen)

        call check(error, pf_toml_has(gen, "nproc"), "has must find a key the file sets")
        if (allocated(error)) return
        call check(error, .not. pf_toml_has(gen, "nope"), "has must not find a key the file omits")
        if (allocated(error)) return
        call check(error, pf_toml_has_section(conf, "general"), "has_section must find [general]")
        if (allocated(error)) return
        call check(error, pf_toml_has_section(conf, "region"), "has_section must find [[region]] too")
        if (allocated(error)) return
        call check(error, .not. pf_toml_has_section(conf, "title"), &
            "has_section must answer .false. for a plain key")
        if (allocated(error)) return
        call pf_toml_filename(gen, name)
        call check(error, name == "unit-test", "filename must report the name pf_toml_loads was given")
        if (allocated(error)) return
        ! Ten, not nine: a SUB-TABLE is a key of its parent too, so `[general.nested]` is in this
        ! list beside the nine values. That is the same view the unknown-key sweep takes, which is
        ! why opening a section has to mark its name -- otherwise every nested section would be
        ! reported as an unknown key of its parent.
        call pf_toml_keys(gen, keys)
        call check(error, size(keys) == 10, "[general] carries nine values plus the nested sub-table")
        if (allocated(error)) return
        call check(error, any(keys == "nested"), "the nested sub-table must appear in its parent's key list")
        if (allocated(error)) return
        call check(error, any(keys == "nproc") .and. any(keys == "level"), &
            "the first and last of the nine values must both appear")
        call pf_toml_close(conf)
    end subroutine test_presence_helpers

    !> The two answers a handle gives when there is nothing behind it: `<closed>` for a file name,
    !> and an empty list for the keys.
    !!
    !! **A handle with no table is not the same thing as a handle with no document.** An optional
    !! section the file does not carry leaves the first but not the second, deliberately -- the
    !! handle still knows its display path and its file, so a read from it can name both. Every
    !! query therefore has to cope with it, and answering rather than aborting is what lets a
    !! program probe for an optional section and move on.
    subroutine test_empty_handle_queries(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, missing, never_opened
        character(len=:), allocatable :: text, name
        character(len=PF_TOML_MAX_KEY), allocatable :: keys(:)
        logical :: found

        call pf_toml_filename(never_opened, name)
        call check(error, name == "<closed>", &
            "a handle no open ever filled must report its file name as <closed>")
        if (allocated(error)) return

        text = '[general]' // new_line("a") // 'b = 2' // new_line("a")
        call pf_toml_loads(conf, text, name = "empty-handle-test")
        call pf_toml_section(conf, "nowhere", missing, required = .false., found = found)
        call check(error, .not. found, "the fixture must not carry the section this test asks for")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        call pf_toml_keys(missing, keys)
        call check(error, allocated(keys), "keys must come back allocated even for a section that is not there")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        call check(error, size(keys) == 0, "and empty rather than holding the parent's keys")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        ! The negative control on both halves: the same two queries against a section that IS
        ! there must answer from the file rather than with the empty answers above.
        call pf_toml_filename(missing, name)
        call check(error, name == "empty-handle-test", &
            "a not-found section still knows the file it was looked for in")
        call pf_toml_close(conf)
    end subroutine test_empty_handle_queries
    !
    !> A document with more distinct paths than the accumulator's first block still reports
    !> nothing unread.
    !!
    !! **The accumulator starts at 32 entries and doubles.** Growing it is the one operation here
    !! whose failure is silent in the direction that matters: a mark lost past the 32nd entry
    !! turns a key the program DID read into an "unknown key" the sweep reports, or -- if the
    !! copy is short -- loses an earlier one. Reading more than 32 keys and then asking for a
    !! clean sweep is what says the doubling copied everything across.
    subroutine test_mark_accumulator_grows(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=PF_TOML_MAX_KEY), allocatable :: keys(:)
        character(len=:), allocatable :: text
        character(len=8) :: key
        character(len=16) :: num
        integer(int32) :: got
        integer :: i, nbad
        !> Comfortably past 32 and past 64, so the doubling has to happen twice.
        integer, parameter :: NKEYS = 80

        text = '[general]' // new_line("a")
        do i = 1, NKEYS
            write (key, '(a,i0)') "k", i
            write (num, '(i0)') i
            text = text // trim(key) // ' = ' // trim(num) // new_line("a")
        end do
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        nbad = 0
        do i = 1, NKEYS
            write (key, '(a,i0)') "k", i
            call pf_toml_get(gen, trim(key), got)
            if (got /= int(i, int32)) nbad = nbad + 1
        end do
        call check(error, nbad, 0, "every one of the keys must read back as its own value")
        if (allocated(error)) then
            call pf_toml_close(conf)
            return
        end if
        ! The assertion the growth is actually about: a lost mark makes a key the program read
        ! look unread, so a FATAL sweep is the strong form -- it aborts rather than returning if
        ! any of the 80 marks went missing, and reaching the line below is the verdict.
        call pf_toml_check_all(conf)
        call pf_toml_keys(gen, keys)
        call check(error, size(keys) == NKEYS, &
            "the section must still carry every key the fatal sweep just accepted as read")
        call pf_toml_close(conf)
    end subroutine test_mark_accumulator_grows
    !
    !> The raw-object hatches hand back something usable, and `pf_toml_mark` keeps the sweep honest.
    !!
    !! **The `use tomlf` below is the subject, not an accident.** Receiving either hatch needs a
    !! toml-f type to declare, and naming that import is exactly the act of stepping outside the
    !! wrapper that `pf_toml_table`'s own documentation describes. It is confined to this one
    !! procedure so the rest of the suite still says nothing about toml-f's namespace.
    subroutine test_escape_hatches(error)
        use tomlf, only: toml_table, toml_context, get_value
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, never_opened
        type(toml_table), pointer :: tbl, none_tbl
        type(toml_context), pointer :: ctx, none_ctx
        character(len=:), allocatable :: text
        integer(int32) :: b
        integer :: raw
        logical :: v(6)

        text = '[general]' // new_line("a") // 'b = 2' // new_line("a") // 'raw = 5' // new_line("a")
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "b", b)
        ! The table hatch hands back the SECTION's table, not the document root: `raw` is readable
        ! through it with toml-f's own accessor, which is the whole point of the hatch -- a
        ! construct this wrapper does not cover is reached from here. A hatch that handed back the
        ! root would fail this, since `raw` is not a key of the root.
        call pf_toml_table(gen, tbl)
        ! The context belongs to the DOCUMENT, so a section handle reaches it through the document
        ! it borrows rather than holding one of its own.
        call pf_toml_context(gen, ctx)
        raw = 0
        if (associated(tbl)) call get_value(tbl, "raw", raw)
        ! Both hatches on a handle no load ever filled: null rather than a stale pointer or an
        ! abort, so a caller may ask before deciding whether there is anything to read.
        call pf_toml_table(never_opened, none_tbl)
        call pf_toml_context(never_opened, none_ctx)
        ! A key read some other way and declared with pf_toml_mark keeps the sweep accurate, which
        ! is what makes the hatches usable at all.
        call pf_toml_mark(gen, "raw")
        call pf_toml_check(gen)
        ! Every verdict is taken before the first assertion, so the document is closed on the one
        ! path out of here rather than leaked by whichever check fires first.
        v(1) = associated(tbl)
        v(2) = associated(ctx)
        v(3) = raw == 5
        v(4) = b == 2_int32
        v(5) = .not. associated(none_tbl)
        v(6) = .not. associated(none_ctx)
        call pf_toml_close(conf)

        call check(error, v(1), "the table hatch must hand back the section's table")
        if (allocated(error)) return
        call check(error, v(2), "the context hatch must hand back the document's context")
        if (allocated(error)) return
        call check(error, v(3), "a key read through the table hatch must give its file value")
        if (allocated(error)) return
        call check(error, v(4), "a hatch-read key declared with mark must pass the sweep")
        if (allocated(error)) return
        call check(error, v(5), "the table hatch on an unopened handle must come back null")
        if (allocated(error)) return
        call check(error, v(6), "the context hatch on an unopened handle must come back null")
    end subroutine test_escape_hatches

    ! ================================================================================
    ! Soft failures
    ! ================================================================================

    !> A file that is not there comes back as `PF_TOML_ERR_OPEN` when `status` is asked for.
    subroutine test_load_status_open(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        integer :: status

        call pf_toml_load(conf, "test_run/no_such_configuration_file_at_all.toml", status)
        call check(error, status == PF_TOML_ERR_OPEN, &
            "a missing file must report PF_TOML_ERR_OPEN rather than PF_TOML_ERR_PARSE")
        if (allocated(error)) return
        call check(error, .not. pf_toml_is_open(conf), "a failed load must leave the handle closed")
        if (allocated(error)) return
        ! Closing a handle no open ever filled is a no-op rather than an error, so that a
        ! defensive close after a failed soft load does not itself abort.
        call pf_toml_close(conf)
    end subroutine test_load_status_open

    !> Text that is not TOML comes back as `PF_TOML_ERR_PARSE`, and the control parses.
    subroutine test_load_status_parse(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf
        integer :: status

        call pf_toml_loads(conf, "this is not = = valid toml", status = status)
        call check(error, status == PF_TOML_ERR_PARSE, "malformed text must report PF_TOML_ERR_PARSE")
        if (allocated(error)) return
        call check(error, .not. pf_toml_is_open(conf), "a failed parse must leave the handle closed")
        if (allocated(error)) return
        call pf_toml_loads(conf, 'ok = 1', status = status)
        call check(error, status == PF_TOML_OK, "the control must parse and report PF_TOML_OK")
        if (allocated(error)) return
        call check(error, pf_toml_is_open(conf), "the control must leave the handle open")
        call pf_toml_close(conf)
    end subroutine test_load_status_parse

    ! ================================================================================
    ! Writing
    ! ================================================================================

    !> A document built from nothing saves, reloads, and gives back what was put in.
    subroutine test_build_and_save(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: doc, sect, back, bsect
        character(len=*), parameter :: out_file = "test_run/toml_build_and_save.toml"
        character(len=:), allocatable :: name
        character(len=16) :: files(3)
        type(pf_toml_strings) :: got_files
        character(len=:), allocatable :: one
        integer(int32) :: n
        real(real64) :: r
        logical :: flag
        integer(int32) :: ints(3)

        call pf_toml_new(doc, "built")
        call pf_toml_set(doc, "title", "built by a test")
        call pf_toml_new_section(doc, "general", sect)
        call pf_toml_set(sect, "nproc", 6_int32)
        call pf_toml_set(sect, "factor", 0.125_real64)
        call pf_toml_set(sect, "verbose", .false.)
        call pf_toml_set(sect, "limits", [4_int32, 5_int32, 6_int32])
        ! Deliberately blank-padded, and deliberately shortest first: the write side must trim, and
        ! the read side must not size every element from the first.
        files(1) = "a"
        files(2) = "bc"
        files(3) = "a much longer"
        call pf_toml_set(sect, "files", files)
        call pf_toml_save(doc, out_file)
        call pf_toml_close(doc)

        call pf_toml_load(back, out_file)
        call pf_toml_get(back, "title", name)
        call check(error, name == "built by a test", "a saved root key must reload")
        if (allocated(error)) return
        call pf_toml_section(back, "general", bsect)
        call pf_toml_get(bsect, "nproc", n)
        call check(error, n == 6_int32, "a saved integer must reload")
        if (allocated(error)) return
        call pf_toml_get(bsect, "factor", r)
        call check(error, abs(r - 0.125_real64) < 1.0e-12_real64, "a saved real must reload exactly")
        if (allocated(error)) return
        call pf_toml_get(bsect, "verbose", flag)
        call check(error, .not. flag, "a saved logical must reload")
        if (allocated(error)) return
        call pf_toml_get(bsect, "limits", ints)
        call check(error, all(ints == [4_int32, 5_int32, 6_int32]), "a saved integer list must reload")
        if (allocated(error)) return
        call pf_toml_get_strings(bsect, "files", got_files)
        call check(error, got_files%count() == 3, "a saved string list must reload with three elements")
        if (allocated(error)) return
        call got_files%get(1, one)
        call check(error, one == "a" .and. len(one) == 1, &
            "the write side must TRIM: a blank-padded 'a' must reload at length 1, not 16")
        if (allocated(error)) return
        call got_files%get(3, one)
        call check(error, one == "a much longer", "the longest element must reload in full")
        call pf_toml_close(back)
    end subroutine test_build_and_save

    !> `pf_toml_new_section` UPSERTS: a second call with the same name hands back the section that
    !! is already there -- on a document built from nothing and on a loaded one alike.
    !!
    !! The discriminating assertion is the round trip through TWO handles: a key set through the
    !! first must be readable through the second, and one set through the second visible through
    !! the first. Asserting only that the second call succeeds would pass against an implementation
    !! that quietly started a second, empty table beside the first -- which is exactly the failure
    !! worth pinning, because nothing else on the write side upserts and there is no sibling
    !! behaviour to fall back on.
    !!
    !! The control showing that upserting is SPECIAL rather than universal is out of process,
    !! because it aborts: `pf_toml_set` on a key that already exists is error scenario
    !! `toml_set_existing` (`test/error_scenarios.f90`).
    subroutine test_new_section_upserts(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: doc, first, second, conf, again, fresh
        character(len=:), allocatable :: text, path_one, path_two
        integer(int32) :: n

        ! A document built from nothing: the second call must not start an empty table.
        call pf_toml_new(doc, "upsert")
        call pf_toml_new_section(doc, "general", first)
        call pf_toml_set(first, "nproc", 6_int32)
        call pf_toml_new_section(doc, "general", second)
        ! `%has` before `%get`: reading an absent key is fatal, so a second handle onto a fresh
        ! empty table would abort the runner rather than fail this test.
        call check(error, pf_toml_has(second, "nproc"), &
            "a second new_section must return the section already there, not a fresh empty one")
        if (allocated(error)) return
        call pf_toml_get(second, "nproc", n)
        call check(error, n == 6_int32, "and it must carry the value the first handle set")
        if (allocated(error)) return
        call pf_toml_path(first, path_one)
        call pf_toml_path(second, path_two)
        call check(error, path_one == path_two, "both handles must report the same path")
        if (allocated(error)) return
        ! The other direction, which is what makes them one section rather than a copy of one.
        call pf_toml_set(second, "factor", 0.5_real64)
        call check(error, pf_toml_has(first, "factor"), &
            "a key set through the second handle must be visible through the first")
        if (allocated(error)) return
        call pf_toml_close(doc)

        ! A LOADED document: a section the file already carries comes back with the file's keys ...
        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_new_section(conf, "general", again)
        call check(error, pf_toml_has(again, "nproc"), &
            "new_section on a loaded document must return the file's own section, not an empty one")
        if (allocated(error)) return
        call pf_toml_get(again, "nproc", n)
        call check(error, n == 4_int32, "and it must carry the value the file set")
        if (allocated(error)) return
        ! ... and one it does not carry is added rather than refused.
        call check(error, .not. pf_toml_has_section(conf, "extra"), &
            "the fixture must not already carry the section this arm adds")
        if (allocated(error)) return
        call pf_toml_new_section(conf, "extra", fresh)
        call check(error, pf_toml_has_section(conf, "extra"), &
            "new_section must add a section the loaded file did not have")
        if (allocated(error)) return
        call pf_toml_set(fresh, "added", 1_int32)
        call check(error, pf_toml_has(fresh, "added"), "the added section must accept keys")
        call pf_toml_close(conf)
    end subroutine test_new_section_upserts

    !> `pf_toml_update` changes a key the file set; `pf_toml_set` adds one it did not.
    subroutine test_set_and_update(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text
        integer(int32) :: n
        real(real64) :: r

        text = '[general]' // new_line("a") // 'b = 2' // new_line("a")
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)

        call pf_toml_update(gen, "b", 99_int32)
        call pf_toml_get(gen, "b", n)
        call check(error, n == 99_int32, "update must change the value a later get returns")
        if (allocated(error)) return

        call pf_toml_set(gen, "added", 7.5_real64)
        call pf_toml_get(gen, "added", r)
        call check(error, abs(r - 7.5_real64) < 1.0e-12_real64, "set must add a key a later get can read")
        if (allocated(error)) return

        ! Both touched keys must be marked read, or the sweep would report the one `set` created.
        call pf_toml_check(gen)
        call check(error, n == 99_int32, "a key written through set or update must count as read")
        call pf_toml_close(conf)
    end subroutine test_set_and_update

    !> `pf_toml_set` accepts every scalar and list kind, and `pf_toml_save` writes each one back.
    !!
    !! `test_build_and_save` establishes the build-save-reload loop over the kinds a caller reaches
    !! for first; this is the rest of the matrix. The reload is what makes it an assertion rather
    !! than a smoke test: each value has to survive being RENDERED to TOML text and parsed again,
    !! so a writer emitting the wrong literal for its kind -- an integer where a float belongs, a
    !! list punctuated wrongly -- fails here rather than producing a file nobody reads back.
    !!
    !! Values are deliberately distinct per element and per kind, so an element written from the
    !! wrong slot cannot coincide with the right answer.
    subroutine test_set_every_kind_round_trips(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: doc, sect, back, bsect
        character(len=*), parameter :: out_file = "test_run/toml_set_every_kind.toml"
        integer(int64) :: i64, i64a(3)
        real(real32) :: r32, r32a(2)
        real(real64) :: r64a(2)
        logical :: loga(3)

        call pf_toml_new(doc, "kinds")
        call pf_toml_new_section(doc, "general", sect)
        call pf_toml_set(sect, "big", 9000000000_int64)
        call pf_toml_set(sect, "small", 0.25_real32)
        call pf_toml_set(sect, "bigs", [11_int64, 22_int64, 33_int64])
        call pf_toml_set(sect, "smalls", [0.5_real32, 2.25_real32])
        call pf_toml_set(sect, "wides", [1.25_real64, 3.75_real64])
        call pf_toml_set(sect, "switches", [.true., .false., .true.])
        call pf_toml_save(doc, out_file)
        call pf_toml_close(doc)

        call pf_toml_load(back, out_file)
        call pf_toml_section(back, "general", bsect)
        call pf_toml_get(bsect, "big", i64)
        call check(error, i64 == 9000000000_int64, &
            "an int64 beyond int32's range must survive the write and the reload")
        if (allocated(error)) return
        call pf_toml_get(bsect, "small", r32)
        call check(error, abs(r32 - 0.25_real32) < 1.0e-6_real32, "a real32 scalar must survive")
        if (allocated(error)) return
        call pf_toml_get(bsect, "bigs", i64a)
        call check(error, all(i64a == [11_int64, 22_int64, 33_int64]), "an int64 list must survive")
        if (allocated(error)) return
        call pf_toml_get(bsect, "smalls", r32a)
        call check(error, all(abs(r32a - [0.5_real32, 2.25_real32]) < 1.0e-6_real32), &
            "a real32 list must survive")
        if (allocated(error)) return
        call pf_toml_get(bsect, "wides", r64a)
        call check(error, all(abs(r64a - [1.25_real64, 3.75_real64]) < 1.0e-12_real64), &
            "a real64 list must survive")
        if (allocated(error)) return
        call pf_toml_get(bsect, "switches", loga)
        call check(error, all(loga .eqv. [.true., .false., .true.]), "a logical list must survive")
        call pf_toml_close(back)
    end subroutine test_set_every_kind_round_trips

    !> `pf_toml_update` changes a key that is already there, for every scalar and list kind.
    !!
    !! `test_set_and_update` establishes what update means against set; each kind is its own
    !! specific, and the assertion per kind is that a later `pf_toml_get` returns the NEW value --
    !! an update that wrote into the shadow rather than the parsed document would leave the getter
    !! answering the file's original, which is exactly the silent failure the two-function split
    !! exists to avoid.
    subroutine test_update_every_kind(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=:), allocatable :: text, str
        integer(int64) :: i64
        integer(int32) :: i32a(3)
        real(real32) :: r32
        real(real64) :: r64, r64a(2)
        logical :: log1, loga(2)
        integer(int64) :: i64a(3)
        real(real32) :: r32a(2)
        character(len=24) :: stra(3)

        call sample(text)
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)

        call pf_toml_update(gen, "nproc", 8000000000_int64)
        call pf_toml_get(gen, "nproc", i64)
        call check(error, i64 == 8000000000_int64, "update: int64 scalar")
        if (allocated(error)) return
        call pf_toml_update(gen, "factor", 0.75_real32)
        call pf_toml_get(gen, "factor", r32)
        call check(error, abs(r32 - 0.75_real32) < 1.0e-6_real32, "update: real32 scalar")
        if (allocated(error)) return
        call pf_toml_update(gen, "factor", 1.5_real64)
        call pf_toml_get(gen, "factor", r64)
        call check(error, abs(r64 - 1.5_real64) < 1.0e-12_real64, "update: real64 scalar")
        if (allocated(error)) return
        call pf_toml_update(gen, "verbose", .false.)
        call pf_toml_get(gen, "verbose", log1)
        call check(error, .not. log1, "update: logical scalar")
        if (allocated(error)) return
        call pf_toml_update(gen, "name", "run two")
        call pf_toml_get(gen, "name", str)
        call check(error, str == "run two", "update: character scalar")
        if (allocated(error)) return

        call pf_toml_update(gen, "limits", [7_int32, 8_int32, 9_int32])
        call pf_toml_get(gen, "limits", i32a)
        call check(error, all(i32a == [7_int32, 8_int32, 9_int32]), "update: int32 list")
        if (allocated(error)) return
        call pf_toml_update(gen, "limits", [70_int64, 80_int64, 90_int64])
        call pf_toml_get(gen, "limits", i64a)
        call check(error, all(i64a == [70_int64, 80_int64, 90_int64]), "update: int64 list")
        if (allocated(error)) return
        call pf_toml_update(gen, "weights", [2.5_real32, 3.5_real32])
        call pf_toml_get(gen, "weights", r32a)
        call check(error, all(abs(r32a - [2.5_real32, 3.5_real32]) < 1.0e-6_real32), &
            "update: real32 list")
        if (allocated(error)) return
        call pf_toml_update(gen, "weights", [4.25_real64, 5.75_real64])
        call pf_toml_get(gen, "weights", r64a)
        call check(error, all(abs(r64a - [4.25_real64, 5.75_real64]) < 1.0e-12_real64), &
            "update: real64 list")
        if (allocated(error)) return
        call pf_toml_update(gen, "flags", [.false., .true.])
        call pf_toml_get(gen, "flags", loga)
        call check(error, all(loga .eqv. [.false., .true.]), "update: logical list")
        if (allocated(error)) return
        stra(1) = "x"
        stra(2) = "yy"
        stra(3) = "a replacement third"
        call pf_toml_update(gen, "files", stra)
        stra = ""
        call pf_toml_get(gen, "files", stra)
        call check(error, stra(1) == "x" .and. stra(3) == "a replacement third", &
            "update: character list")
        if (allocated(error)) return
        ! Every key touched above was already in the file and is marked read by the update, so the
        ! sweep must stay silent: an update that failed to mark would report the key as unread.
        call pf_toml_get(gen, "level", str)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_close(conf)
    end subroutine test_update_every_kind

    !> `pf_toml_append_section` builds `[[name]]` entries that reload as entries.
    subroutine test_append_section(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: doc, ent, back, bent
        character(len=*), parameter :: out_file = "test_run/toml_append_section.toml"
        character(len=:), allocatable :: path
        integer(int32) :: id, i

        call pf_toml_new(doc)
        do i = 1, 3
            call pf_toml_append_section(doc, "region", ent)
            call pf_toml_set(ent, "id", i)
        end do
        call pf_toml_path(ent, path)
        call check(error, path == "region[3]", &
            "the third appended entry's path must render as region[3], not " // path)
        if (allocated(error)) return
        call pf_toml_save(doc, out_file)
        call pf_toml_close(doc)

        call pf_toml_load(back, out_file)
        call check(error, pf_toml_section_count(back, "region") == 3, &
            "three appended entries must reload as three [[region]] entries")
        if (allocated(error)) return
        do i = 1, 3
            call pf_toml_section(back, "region", i, bent)
            call pf_toml_get(bent, "id", id)
            call check(error, id == i, "entry i must reload carrying id = i")
            if (allocated(error)) return
        end do
        call pf_toml_close(back)
    end subroutine test_append_section

    !> A saved file states what the run USED: defaults present, keys nobody read absent.
    subroutine test_save_effective(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, back, bgen
        character(len=*), parameter :: out_file = "test_run/toml_save_effective.toml"
        character(len=:), allocatable :: text
        integer(int32) :: b, c, n
        logical :: found

        text = '[general]' // new_line("a") // 'b = 2' // new_line("a") // &
               'never_read = 5' // new_line("a")
        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "b", b)
        call pf_toml_get(gen, "c", c, default = 30_int32)
        call pf_toml_save(conf, out_file)
        call pf_toml_close(conf)

        call pf_toml_load(back, out_file)
        call pf_toml_section(back, "general", bgen)
        call pf_toml_get(bgen, "b", n)
        call check(error, n == 2_int32, "a key the file set and the program read must be saved")
        if (allocated(error)) return
        call check(error, pf_toml_has(bgen, "c"), &
            "a key the program read from its DEFAULT must be saved, with the default's value")
        if (allocated(error)) return
        call pf_toml_get(bgen, "c", n)
        call check(error, n == 30_int32, "the saved default must carry the default's value")
        if (allocated(error)) return
        found = pf_toml_has(bgen, "never_read")
        call check(error, .not. found, &
            "a key the file set but the program never read must be ABSENT from the saved file")
        call pf_toml_close(back)
    end subroutine test_save_effective

    ! ================================================================================
    ! Concurrency
    !
    ! These four load THE SAME committed file and read the same keys. test-drive dispatches a
    ! suite's tests with `!$omp parallel do`, so they really do run at once -- which is the point:
    ! the module's contract is that any public procedure is safe to call from inside a parallel
    ! region, delivered by serialising on one named critical section. Nothing here writes to the
    ! fixture, so the project's shared-fixture-path rule is not in play.
    !
    ! They are deliberately four near-identical tests rather than one loop: a loop would run on one
    ! thread and prove nothing about concurrency at all.
    ! ================================================================================

    !> Reads the shared fixture, concurrently with its three siblings.
    subroutine test_shared_reader_1(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.

        call read_shared_fixture(error)
    end subroutine test_shared_reader_1

    !> Reads the shared fixture, concurrently with its three siblings.
    subroutine test_shared_reader_2(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.

        call read_shared_fixture(error)
    end subroutine test_shared_reader_2

    !> Reads the shared fixture, concurrently with its three siblings.
    subroutine test_shared_reader_3(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.

        call read_shared_fixture(error)
    end subroutine test_shared_reader_3

    !> Reads the shared fixture, concurrently with its three siblings.
    subroutine test_shared_reader_4(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.

        call read_shared_fixture(error)
    end subroutine test_shared_reader_4

    !> One whole read of the shared fixture, asserted end to end. Called from four tests at once.
    subroutine read_shared_fixture(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, ent
        character(len=:), allocatable :: title, name
        integer(int32) :: nproc, id, limits(3), i
        real(real64) :: factor
        logical :: verbose

        call pf_toml_load(conf, SHARED_FIXTURE)
        call pf_toml_get(conf, "title", title)
        call check(error, title == "shared fixture", "the shared fixture's title must read back")
        if (allocated(error)) return

        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "nproc", nproc)
        call pf_toml_get(gen, "factor", factor)
        call pf_toml_get(gen, "verbose", verbose)
        call pf_toml_get(gen, "name", name)
        call pf_toml_get(gen, "limits", limits)
        call check(error, nproc == 8_int32, "nproc must be 8 on every thread")
        if (allocated(error)) return
        call check(error, abs(factor - 1.25_real64) < 1.0e-12_real64, "factor must be 1.25 on every thread")
        if (allocated(error)) return
        call check(error, verbose, "verbose must be true on every thread")
        if (allocated(error)) return
        call check(error, name == "concurrent", "name must read back on every thread")
        if (allocated(error)) return
        call check(error, all(limits == [10_int32, 20_int32, 30_int32]), &
            "limits must read back on every thread")
        if (allocated(error)) return

        call check(error, pf_toml_section_count(conf, "region") == 2, &
            "the shared fixture has two [[region]] entries, on every thread")
        if (allocated(error)) return
        do i = 1, 2
            call pf_toml_section(conf, "region", i, ent)
            call pf_toml_get(ent, "id", id)
            call pf_toml_get(ent, "label", name)
            call check(error, id == i, "entry i must carry id = i, on every thread")
            if (allocated(error)) return
            call check(error, len(name) == 5, "both labels are five characters, on every thread")
            if (allocated(error)) return
        end do

        ! The whole-document sweep, which walks the accumulator every thread has been appending to.
        call pf_toml_check_all(conf)
        call pf_toml_close(conf)
    end subroutine read_shared_fixture
    !> `pf_toml_report` puts a caller's own complaint in the log, against the key's source line.
    !!
    !! The point of this entry is the SOURCE EXCERPT: a validation message the calling program
    !! writes ("these two lists must be the same length") is only useful if it shows the reader
    !! which line to edit, and rendering that excerpt is the half no other entry in this module
    !! exercises. Both arms are driven, because they answer differently and only one of them can
    !! quote the file: a key that IS in the document carries an origin and gets the excerpt, and a
    !! key that is not is reported plainly rather than refused.
    !!
    !! `PF_TOML_IGNORE` returns before writing anything, which is asserted by its message being
    !! absent from a log that holds the other two.
    !!
    !! In the serial suite because it reconfigures the process-global default logger.
    subroutine test_report_reaches_the_log(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=*), parameter :: log_file = "test_run/toml_report.log"
        character(len=:), allocatable :: text
        character(len=512) :: line
        integer :: unit, ios
        logical :: found_known, found_absent, found_ignored, found_excerpt

        text = '[general]' // new_line("a") // 'b = 2' // new_line("a")

        call pf_log_init(level = PF_LEVEL_INFO, name = "toml-test", console = .false.)
        call pf_log_add_file(log_file, level = PF_LEVEL_INFO, append = .false.)

        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "b", ios)
        ! A key the file sets: reported with the excerpt that quotes its line.
        call pf_toml_report(gen, "b", "b must be at least ten", severity = PF_TOML_WARN)
        ! A key the file does not set: reported without one, rather than refused.
        call pf_toml_report(gen, "absent_key", "absent_key would have been checked", &
            severity = PF_TOML_WARN)
        ! And the severity that says nothing at all.
        call pf_toml_report(gen, "b", "this must never be logged", severity = PF_TOML_IGNORE)
        call pf_toml_close(conf)

        call pf_log_flush()
        call pf_log_close()

        found_known = .false.
        found_absent = .false.
        found_ignored = .false.
        found_excerpt = .false.
        open(newunit=unit, file=log_file, status="old", action="read", iostat=ios)
        call check(error, ios == 0, "the log file must have been created")
        if (allocated(error)) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, "b must be at least ten") > 0) found_known = .true.
            if (index(line, "absent_key would have been checked") > 0) found_absent = .true.
            if (index(line, "this must never be logged") > 0) found_ignored = .true.
            ! The excerpt quotes the source line itself, which no message text contains.
            if (index(line, "b = 2") > 0) found_excerpt = .true.
        end do
        close(unit)
        call check(error, found_known, "a report against a key in the file must reach the log")
        if (allocated(error)) return
        call check(error, found_excerpt, &
            "and must carry the source excerpt quoting that key's line")
        if (allocated(error)) return
        call check(error, found_absent, &
            "a report against a key the file does not set must still reach the log")
        if (allocated(error)) return
        call check(error, .not. found_ignored, "PF_TOML_IGNORE must write nothing at all")
    end subroutine test_report_reaches_the_log


    ! ================================================================================
    ! Logging integration (serial suite)
    ! ================================================================================

    !> `pf_toml_check_all`'s sweep names every shape of unread thing, each in its own words.
    !!
    !! **Only the warning severity can be asserted from inside the process.** The fatal spelling
    !! is what the `toml_unknown_key`/`toml_unknown_section` scenarios drive, and it aborts before
    !! any of these lines can be read back -- so the five arms below have no in-process witness
    !! other than this test. They are genuinely five, not one message with five call sites: a
    !! plain key, a plain array, an array-of-tables nobody opened, one ENTRY of an
    !! array-of-tables whose siblings were opened, and a path too long to name at all.
    !!
    !! **The entry arm is the one worth spelling out.** Opening `[[region]]` entry 1 marks the
    !! array, so the sweep descends into it and has to notice entry 2 separately; a sweep that
    !! stopped at the array would report nothing here and every other assertion would still pass.
    !!
    !! Like its neighbour it reconfigures the process-global default logger, which is why it is in
    !! the serial suite.
    subroutine test_check_all_names_every_unread_shape(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen, ent
        character(len=*), parameter :: log_file = "test_run/toml_sweep_all.log"
        character(len=:), allocatable :: text, longkey
        character(len=512) :: line
        integer(int32) :: b, id
        integer :: unit, ios
        logical :: saw(6)
        !> Past PF_TOML_MAX_PATH, so the sweep cannot even form this key's path. At the ROOT,
        !! where the path is empty, so the length is the key's alone and the arm is unambiguous.
        integer, parameter :: LONG = PF_TOML_MAX_PATH + 40

        allocate(character(len=LONG) :: longkey)
        longkey = repeat("z", LONG)

        ! The long key goes FIRST, before any header: in TOML every key after a header belongs to
        ! that table, and the sweep does not descend into a section nobody opened -- so a long key
        ! written last would sit inside `[[untouched]]` and never be visited at all.
        text = longkey // ' = 1' // new_line("a") // &
               '[general]' // new_line("a") // 'b = 2' // new_line("a") // &
               'stray_scalar = 5' // new_line("a") // &
               'stray_list = [1, 2, 3]' // new_line("a") // &
               '[[region]]' // new_line("a") // 'id = 1' // new_line("a") // &
               '[[region]]' // new_line("a") // 'id = 2' // new_line("a") // &
               '[[untouched]]' // new_line("a") // 'id = 3' // new_line("a")

        call pf_log_init(level = PF_LEVEL_INFO, name = "toml-sweep-test", console = .false.)
        call pf_log_add_file(log_file, level = PF_LEVEL_INFO, append = .false.)

        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "b", b)
        ! ONE of the two [[region]] entries, so the other is unread while the array is not.
        call pf_toml_section(conf, "region", 1, ent)
        call pf_toml_get(ent, "id", id)
        call pf_toml_check_all(conf, severity = PF_TOML_WARN)
        call pf_toml_close(conf)

        call pf_log_flush()
        call pf_log_close()

        saw = .false.
        open(newunit=unit, file=log_file, status="old", action="read", iostat=ios)
        call check(error, ios == 0, "the log file must have been created")
        if (allocated(error)) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, "stray_scalar") > 0) saw(1) = .true.
            if (index(line, "stray_list") > 0) saw(2) = .true.
            if (index(line, "[[untouched]]") > 0) saw(3) = .true.
            if (index(line, "[[region[2]]]") > 0) saw(4) = .true.
            if (index(line, "Path too long") > 0) saw(5) = .true.
            ! The negative controls, both of which must stay absent.
            if (index(line, "[general] b") > 0) saw(6) = .true.
            if (index(line, "region[1]") > 0 .and. index(line, "never read") > 0) saw(6) = .true.
        end do
        close(unit)

        call check(error, saw(1), "an unread scalar key must be reported as an unknown key")
        if (allocated(error)) return
        call check(error, saw(2), "an unread plain list must be reported as an unknown key too")
        if (allocated(error)) return
        call check(error, saw(3), "an array-of-tables nobody opened must be reported whole")
        if (allocated(error)) return
        call check(error, saw(4), "an unread ENTRY of an opened array-of-tables must be reported on its own")
        if (allocated(error)) return
        call check(error, saw(5), "a key whose path is too long to form must be reported as such")
        if (allocated(error)) return
        call check(error, .not. saw(6), &
            "the sweep must report neither the key nor the entry the program actually read")
    end subroutine test_check_all_names_every_unread_shape
    !
    !> A `PF_TOML_WARN` sweep really reaches the log, which is the only proof it emits at all.
    !!
    !! Everything else in this suite asserts that the module does NOT abort; this is the one test
    !! that reads what it wrote. It reconfigures the process-global default logger, which is why it
    !! lives in the serial suite.
    subroutine test_warn_reaches_the_log(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive's error slot.
        type(pf_toml) :: conf, gen
        character(len=*), parameter :: log_file = "test_run/toml_warn.log"
        character(len=:), allocatable :: text
        character(len=512) :: line
        integer(int32) :: b
        integer :: unit, ios
        logical :: found_extra, found_none

        text = '[general]' // new_line("a") // 'b = 2' // new_line("a") // &
               'mispelt_key = 5' // new_line("a")

        call pf_log_init(level = PF_LEVEL_INFO, name = "toml-test", console = .false.)
        call pf_log_add_file(log_file, level = PF_LEVEL_INFO, append = .false.)

        call pf_toml_loads(conf, text)
        call pf_toml_section(conf, "general", gen)
        call pf_toml_get(gen, "b", b)
        call pf_toml_check(gen, severity = PF_TOML_WARN)
        call pf_toml_close(conf)

        call pf_log_flush()
        call pf_log_close()

        found_extra = .false.
        found_none = .false.
        open(newunit=unit, file=log_file, status="old", action="read", iostat=ios)
        call check(error, ios == 0, "the log file must have been created")
        if (allocated(error)) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, "mispelt_key") > 0) found_extra = .true.
            if (index(line, "[general] b") > 0) found_none = .true.
        end do
        close(unit)

        call check(error, found_extra, &
            "a PF_TOML_WARN sweep must write the unknown key's name to the log")
        if (allocated(error)) return
        ! Negative control: the key the program DID read must not be reported.
        call check(error, .not. found_none, &
            "the sweep must not report a key the program read")
    end subroutine test_warn_reaches_the_log

end module test_toml
