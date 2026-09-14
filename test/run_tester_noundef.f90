!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
! https://github.com/fortran-lang/test-drive
!
! To exclude code lines from gcovr coverage report, use the following markers:
! GCOVR_EXCL_START
! GCOVR_EXCL_STOP
! GCOVR_EXCL_LINE
! GCOVR_EXCL_FUNCTION
!
!> Test driver: Arrow-free, yet NOT undef-safe.
!!
!! **Membership here cannot be derived from source — it is discovered by running the check**, which
!! is the one way this runner differs from every other (`feature_tests.md` section 6.3). Each entry
!! carries its reason at the registration below, and entries arrive and leave **by hand**: a tool
!! that promoted a suite on one green run would be deciding, on one machine and one compiler,
!! something that needs a person to look at why it changed.
!!
!! The cost of a wrong entry is asymmetric and silent: a suite parked here that would have passed
!! loses its undef coverage with nothing to report it. `tools/check_nag_undefined.sh` therefore
!! re-tries these suites and REPORTS any that now pass, without moving them.
program run_tester_noundef
    use testdrive, only : new_testsuite, testsuite_type
    use test_runner_support, only : run_tester_args, run_tester_main
    use test_sorting, only : collect_tests_parquet_sorting
    implicit none
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=:), allocatable :: suite_name, test_name
    !
    call run_tester_args(suite_name, test_name)
    !
    testsuites = [ &
        ! NOT a C++ dependency -- `test_sorting.f90` is Arrow-free and the partition check
        ! proves it. It is here because `-C=undefined` SEGFAULTS inside NAG's OWN
        ! instrumentation, in `extract_chr` (`src/parquet_argsort_kernel.f90`) at
        ! `buf(1)%data(pos + j) = values(k)(j:j)`, reached from `pf_sort` on a `character`
        ! array. The statement is correct -- `buf(1)%data` is allocated five lines above --
        ! and the suite passes under every other profile, `nagdeb` included.
        !
        ! WHAT FAILS IS THREE TESTS, NOT THE TIER. Measured by running all 117 one at a time
        ! under `-C=undefined`: 114 pass. The three that do not are `pf_sort: every type`,
        ! `character sorts on the full padded length` and `pf_unique/pf_unique_count: every
        ! type x both count kinds` -- each a CHARACTER key, each reaching `extract_chr`.
        ! Every non-character test passes, engine and threading groups included. Confirmed in
        ! SEQUENCE and not only in isolation, which is the claim that matters: with those
        ! three registrations commented out, the suite under `--profile nagundef` gives 114
        ! passed, 0 failed, exit 0. So one defect on one type costs 114 tests their undef
        ! coverage -- the crash lands at test 17 of 117 and the process dies with the rest
        ! unrun. (Beware measuring this yourself: a sweep that reports ALL 117 failing is
        ! measuring the harness. MacPorts' BSD `timeout` rejects `-s KILL`, and zsh does not
        ! word-split an unquoted variable holding a command; both give a uniform, clean-
        ! looking, wrong answer. See `.claude/rules/testing.md`'s "Error scenarios".)
        !
        ! SPLITTING THOSE THREE OUT WOULD RECOVER THE 114, AND IS DELIBERATELY NOT DONE --
        ! keeping the sorting suite whole was chosen over the coverage. Do not "tidy" this by
        ! moving the character tests into a file of their own. Should that ever be revisited,
        ! the placement rule it creates would be enforced rather than remembered:
        ! `tools/check_nag_undefined.sh` runs the undef-safe runners in full, so a character
        ! key test added to the wrong file segfaults that run.
        !
        ! DIAGNOSIS, so it is not re-derived. The faulting instruction loads a definedness-map
        ! byte and compares it against 'K' from a NULL base -- the map for the `values` dummy's
        ! CONTENTS was never passed -- and the frame shows a PRESENT argument (`proc` =
        ! "pf_sort") carrying a null map too, so the interleaved map arguments are not lining
        ! up. Same class as the `bind(C)` defect fpm.toml's note (d) describes. `-C=all` is
        ! not involved; `-C=undefined` alone reproduces it. NO SOURCE CHANGE DODGES IT:
        ! building into a local and `move_alloc`ing it in, and copying the whole element
        ! before indexing it, both fault at the same instruction, so every read of `values`
        ! faults rather than the substring specifically. It does not reduce to a synthetic
        ! (three tried, all clean: one file; four separately compiled units with the same
        ! signature; the same again with the caller also a separate module procedure), so a
        ! NAG report needs it bisected out of the real tree. Not stack exhaustion either: it
        ! reproduces at OMP_STACKSIZE=64M, ulimit -s 65520 and OMP_NUM_THREADS=1.
        new_testsuite("sorting", collect_tests_parquet_sorting) &
        ]
    !
    call run_tester_main(testsuites, suite_name, test_name)
    !
end program run_tester_noundef
