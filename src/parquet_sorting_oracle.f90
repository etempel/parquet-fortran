!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! NOT a generated file. tools/generate_parquet_sorting.py emits every other
! src/parquet_argsort*.f90 and src/parquet_sorting*.f90; this one and
! src/parquet_argsort_engine.f90 are hand-written.
!
!> The TEST-ONLY C++ sort engine, bound into the argsort tier at run time.
!!
!! **This module exists so that the rest of the sorting stack does not have to name
!! `parquet_bindings`.** `src/parquet_wrapper.cpp` carries a second, independent implementation of
!! the ordering, kept purely so the conformance tests can check the Fortran engine against it. If
!! `parquet_argsort` or `parquet_sorting` called it directly, every program that sorts anything --
!! including one whose only import is `use parquet_sampling` -- would pull `parquet_bindings`, and
!! with it the whole Arrow stack, into its `use` graph. Reaching it through procedure pointers
!! instead means a program that never imports THIS module never compiles it, and fpm prunes it away.
!!
!! **Selecting the C++ engine and binding it are one act, deliberately.** Fortran has no module
!! initialiser, so a design where something else has to remember to register these entry points
!! would fail silently the first time someone forgot -- the selector would find a null pointer, and
!! any fallback would make the A/B comparison run the Fortran engine against itself and pass. So the
!! SETTER lives here rather than beside the flag it writes: `parquet_debug_use_fortran_sort_engine`
!! binds the pointers before it changes the flag, which makes the unbound state unreachable. The
!! getter, `parquet_debug_using_fortran_sort_engine`, stays in `parquet_argsort` with the flag.
!!
!! **It also refreshes the C++ side's settings mirror**, because the C++ engine is the one consumer
!! of a mirrored knob that is not downstream of a reader or writer open -- a `pf_argsort` call needs
!! no file. Without this the A/B would compare the two engines under DIFFERENT `sort_counting_path`
!! and `sort_counting_bucket_limit` values, which still passes whenever the two happen to agree.
!!
!! No library code imports this module, it appears in no `doc/pages/` guide, and it is absent from
!! README.md's API overview -- the same conventions every other `parquet_debug_*` hook follows.
module parquet_sorting_oracle
    use, intrinsic :: iso_fortran_env, only : int64
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_int8_t
    use parquet_bindings, only : parquet_sort_builder_new, parquet_sort_builder_add_key_int64, &
        parquet_sort_builder_add_key_double, parquet_sort_builder_add_key_string, &
        parquet_sort_builder_build, parquet_sort_builder_is_sorted, parquet_sort_builder_free, &
        parquet_sort_argsort_int64, parquet_sort_argsort_double, parquet_sort_argsort_string, &
        parquet_sort_is_sorted_int64, parquet_sort_is_sorted_double, parquet_sort_is_sorted_string, &
        parquet_sort_builder_build_partial, parquet_sort_builder_nth_element, &
        parquet_sort_partial_argsort_int64, parquet_sort_partial_argsort_double, &
        parquet_sort_partial_argsort_string, parquet_sort_nth_index_int64, &
        parquet_sort_nth_index_double, parquet_sort_nth_index_string, &
        parquet_sort_builder_build_runs, parquet_sort_builder_search, parquet_sort_builder_merge
    use parquet_argsort, only : sort_key_buf, SK_REAL, SK_STR, &
        parquet_argsort_bind_oracle, parquet_argsort_select_engine
    use parquet_settings, only : parquet_push_settings_to_cpp
    !
    implicit none
    private
    !
    public :: parquet_debug_use_fortran_sort_engine
    !
    !> Error-message prefix, matching the tiers' own so a message does not depend on which module
    !! raised it.
    character(len=*), parameter :: EP = "parquet_sorting: "
    !
contains
    !
    !> Test-only switch routing `pf_argsort` and friends to the Fortran engine, or back to C++.
    !!
    !! **Binds the oracle before it selects, so the C++ engine can never be selected unbound.** That
    !! is why this procedure lives here and not beside `dbg_fortran_engine`: importing this module is
    !! the only way to reach it, and reaching it is the only way to clear the flag.
    !!
    !! Both engines answer identically -- that is what the conformance tests assert -- so this
    !! changes timing and nothing else, which is exactly why it is a debug hook rather than a
    !! setting (`parquet_settings` admits a knob only when it changes how fast, how large or how loud
    !! the library runs, never what it ANSWERS).
    subroutine parquet_debug_use_fortran_sort_engine(on)
        logical, intent(in) :: on !! .true. selects the Fortran engine.

        call parquet_argsort_bind_oracle(oracle_argsort, oracle_partial, oracle_nth, &
            oracle_is_sorted, oracle_runs, oracle_search, oracle_merge)
        ! The C++ engine reads the mirrored performance knobs and is not downstream of any reader or
        ! writer open, so this is where its copy is made current. See parquet_push_settings_to_cpp.
        call parquet_push_settings_to_cpp()
        call parquet_argsort_select_engine(on)
    end subroutine parquet_debug_use_fortran_sort_engine
    !
    !> The whole-permutation entry point. `perm` arrives identity-filled by the caller.
    subroutine oracle_argsort(keys, nrows, nthreads, proc, perm)
        type(sort_key_buf), intent(in), target :: keys(:) !! the keys, primary first.
        integer(int64), intent(in) :: nrows               !! rows each key describes.
        integer(int64), intent(in) :: nthreads            !! resolved thread count.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(inout) :: perm(:)          !! receives the 1-based permutation.
        type(c_ptr) :: builder
        integer(int64) :: status
        integer :: ik
        !
        call refresh_mirror()
        if (size(keys) == 1) then
            ! One key needs no builder at all: the one-shot entry points BORROW the buffer that was
            ! just extracted, so this saves a handle allocation and a second copy of every value.
            call engine_one_shot(keys(1), nrows, nthreads, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build(builder, nthreads, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which every selector rejects first -- kept
            ! because silently ignoring a nonzero status is how a real failure goes unnoticed.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end subroutine oracle_argsort
    !
    !> The partial sort. `perm` arrives allocated to `count` and identity-filled.
    subroutine oracle_partial(keys, nrows, count, proc, perm)
        type(sort_key_buf), intent(in), target :: keys(:) !! the keys, primary first.
        integer(int64), intent(in) :: nrows               !! rows each key describes.
        integer(int64), intent(in) :: count               !! leading entries to order.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(inout) :: perm(:)          !! receives `count` 1-based indices.
        type(c_ptr) :: builder
        integer(int64) :: status
        integer :: jk
        !
        call refresh_mirror()
        if (size(keys) == 1) then
            call engine_one_shot_partial(keys(1), nrows, count, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do jk = 1, size(keys)
            call engine_add_key(builder, keys(jk), nrows)
        end do
        status = parquet_sort_builder_build_partial(builder, count, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end subroutine oracle_partial
    !
    !> Nth-element selection.
    subroutine oracle_nth(keys, nrows, nth, proc, idx)
        type(sort_key_buf), intent(in), target :: keys(:) !! the keys, primary first.
        integer(int64), intent(in) :: nrows               !! rows each key describes.
        integer(int64), intent(in) :: nth                 !! 1-based rank wanted.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: idx                !! 1-based row index at that rank.
        type(c_ptr) :: builder
        integer :: ik
        !
        call refresh_mirror()
        if (size(keys) == 1) then
            call engine_one_shot_nth(keys(1), nrows, nth, idx)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        idx = parquet_sort_builder_nth_element(builder, nth)
        call parquet_sort_builder_free(builder)
        if (idx < 1_int64) then
            error stop EP // proc // ": the sort engine could not resolve that rank" ! GCOVR_EXCL_LINE
        end if
    end subroutine oracle_nth
    !
    !> The already-sorted test.
    subroutine oracle_is_sorted(keys, nrows, proc, answer)
        type(sort_key_buf), intent(in), target :: keys(:) !! the keys, primary first.
        integer(int64), intent(in) :: nrows               !! rows each key describes.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        logical, intent(out) :: answer                    !! .true. when already in order.
        type(c_ptr) :: builder
        integer(int64) :: res
        integer :: ik
        !
        call refresh_mirror()
        if (size(keys) == 1) then
            call engine_one_shot_is_sorted(keys(1), nrows, answer)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        res = parquet_sort_builder_is_sorted(builder)
        call parquet_sort_builder_free(builder)
        if (res < 0_int64) then
            error stop EP // proc // ": the sort engine had no key to test" ! GCOVR_EXCL_LINE
        end if
        answer = res == 1_int64
    end subroutine oracle_is_sorted
    !
    !> The grouped sort: a permutation plus the tie flags runs are built from.
    subroutine oracle_runs(keys, nrows, nthreads, gek, proc, perm, tie)
        type(sort_key_buf), intent(in), target :: keys(:) !! the keys, primary first.
        integer(int64), intent(in) :: nrows               !! rows each key describes.
        integer(int64), intent(in) :: nthreads            !! resolved thread count.
        integer(int64), intent(in) :: gek                 !! engine keys defining a group.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(inout) :: perm(:)          !! receives the 1-based permutation.
        integer(c_int8_t), intent(inout) :: tie(:)        !! 1 where a row ties the previous.
        type(c_ptr) :: builder
        integer(int64) :: status
        integer :: ik
        !
        call refresh_mirror()
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build_runs(builder, nthreads, gek, perm, tie)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end subroutine oracle_runs
    !
    !> The binary search over a sorted key.
    subroutine oracle_search(keys, nrows, n_search, upper, proc, pos)
        type(sort_key_buf), intent(in), target :: keys(:) !! the keys, primary first.
        integer(int64), intent(in) :: nrows               !! rows each key has, target included.
        integer(int64), intent(in) :: n_search            !! rows to search, target excluded.
        logical, intent(in) :: upper                      !! .true. for upper_bound.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: pos                !! 1-based insertion point.
        type(c_ptr) :: builder
        integer(c_int8_t) :: wflag
        integer :: ik
        !
        call refresh_mirror()
        wflag = merge(1_c_int8_t, 0_c_int8_t, upper)
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        pos = parquet_sort_builder_search(builder, n_search, wflag)
        call parquet_sort_builder_free(builder)
        if (pos < 1_int64) then
            error stop EP // proc // ": the sort engine had no key to search" ! GCOVR_EXCL_LINE
        end if
    end subroutine oracle_search
    !
    !> The merge of two sorted runs.
    subroutine oracle_merge(keys, nrows, na, proc, perm)
        type(sort_key_buf), intent(in), target :: keys(:) !! the keys, primary first.
        integer(int64), intent(in) :: nrows               !! rows each key describes.
        integer(int64), intent(in) :: na                  !! rows belonging to the first input.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(inout) :: perm(:)          !! receives the 1-based permutation.
        type(c_ptr) :: builder
        integer(int64) :: status
        integer :: ik
        !
        call refresh_mirror()
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_merge(builder, na, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            error stop EP // proc // ": the sort engine could not merge" ! GCOVR_EXCL_LINE
        end if
    end subroutine oracle_merge
    !
    !> Argsorts one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot(key, nrows, nthreads, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: nthreads        !! resolved thread count; 1 sorts serially.
        integer(int64), intent(inout) :: perm(:)      !! receives the 1-based permutation.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_argsort_double(nrows, key%reals, vp, df, nf, nthreads, perm)
        case (SK_STR)
            call parquet_sort_argsort_string(nrows, key%offsets, key%data, vp, df, nf, nthreads, perm)
        case default
            call parquet_sort_argsort_int64(nrows, key%ints, vp, df, nf, nthreads, perm)
        end select
    end subroutine engine_one_shot

    !
    !> Partially argsorts one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot_partial(key, nrows, count, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: count           !! leading entries to order.
        integer(int64), intent(inout) :: perm(:)      !! receives `count` 1-based indices.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_partial_argsort_double(nrows, key%reals, vp, df, nf, count, perm)
        case (SK_STR)
            call parquet_sort_partial_argsort_string(nrows, key%offsets, key%data, vp, df, nf, count, perm)
        case default
            call parquet_sort_partial_argsort_int64(nrows, key%ints, vp, df, nf, count, perm)
        end select
    end subroutine engine_one_shot_partial

    !
    !> Resolves one already-extracted key's nth index through the matching one-shot entry point.
    subroutine engine_one_shot_nth(key, nrows, nth, idx)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: nth             !! 1-based rank wanted.
        integer(int64), intent(out) :: idx            !! 1-based row index at that rank.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            idx = parquet_sort_nth_index_double(nrows, key%reals, vp, df, nf, nth)
        case (SK_STR)
            idx = parquet_sort_nth_index_string(nrows, key%offsets, key%data, vp, df, nf, nth)
        case default
            idx = parquet_sort_nth_index_int64(nrows, key%ints, vp, df, nf, nth)
        end select
    end subroutine engine_one_shot_nth

    !
    !> Tests one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot_is_sorted(key, nrows, answer)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        logical, intent(out) :: answer                !! .true. when already in order.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        integer(int64) :: res
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            res = parquet_sort_is_sorted_double(nrows, key%reals, vp, df, nf)
        case (SK_STR)
            res = parquet_sort_is_sorted_string(nrows, key%offsets, key%data, vp, df, nf)
        case default
            res = parquet_sort_is_sorted_int64(nrows, key%ints, vp, df, nf)
        end select
        answer = res == 1_int64
    end subroutine engine_one_shot_is_sorted

    !
    !> Adds one already-extracted key to a C++ builder.
    subroutine engine_add_key(builder, key, nrows)
        type(c_ptr), intent(in) :: builder            !! the builder handle.
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_builder_add_key_double(builder, key%reals, vp, df, nf)
        case (SK_STR)
            call parquet_sort_builder_add_key_string(builder, key%offsets, key%data, vp, df, nf)
        case default
            call parquet_sort_builder_add_key_int64(builder, key%ints, vp, df, nf)
        end select
    end subroutine engine_add_key

    !
    !> The three scalars every engine call needs: a pointer to the validity array (or a null
    !! pointer when the key has no nulls) and the two order flags as int8.
    subroutine key_flags(key, valid_ptr, desc_flag, nulls_flag)
        type(sort_key_buf), intent(in), target :: key   !! the key.
        type(c_ptr), intent(out) :: valid_ptr           !! its validity array, or C_NULL_PTR.
        integer(c_int8_t), intent(out) :: desc_flag     !! nonzero for descending.
        integer(c_int8_t), intent(out) :: nulls_flag    !! nonzero to place nulls first.
        !
        valid_ptr = c_null_ptr
        if (allocated(key%valid)) valid_ptr = c_loc(key%valid)
        desc_flag = merge(1_c_int8_t, 0_c_int8_t, key%descending)
        nulls_flag = merge(1_c_int8_t, 0_c_int8_t, key%nulls_first)
    end subroutine key_flags
    !
    !> Makes the C++ side's copy of every mirrored setting current, before the engine reads it.
    !!
    !! **Per dispatch, not per selection, and the difference is a wrong ANSWER rather than a stale
    !! flag.** The mirrored knobs reach C++ when a reader or writer is opened; a `pf_argsort` call
    !! opens neither, so a test that selects this engine and then changes `sort_counting_path` would
    !! otherwise drive it under the value that was current when the engine was selected. The A/B
    !! conformance comparison would then run the two engines under DIFFERENT configurations and
    !! still pass whenever they happened to agree -- the vacuous agreement the whole A/B exists to
    !! rule out.
    !!
    !! It costs one grouped `bind(C)` call per engine invocation, on a path that is TEST-ONLY: the
    !! shipped `pf_argsort` runs the Fortran engine and never reaches this module at all.
    subroutine refresh_mirror()

        call parquet_push_settings_to_cpp()
    end subroutine refresh_mirror

end module parquet_sorting_oracle ! GCOVR_EXCL_LINE
