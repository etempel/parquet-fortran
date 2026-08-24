!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Maintainer probe: are the two weighted families INDEPENDENT at matched `(seed, stream)`, rather
!! than merely different?
!!
!! Not a test and never run by `fpm test`. It reports overall agreement on the first drawn item
!! against the independent expectation, the same figure on a different stream as a control, and the
!! lowest-weight cell where a shared uniform would concentrate -- plus a reconstruction of the
!! coupled rule from public API alone, so the statistic's power is visible in the same run.
!!
!! It found the coupling recorded as Risk-123 in `feature_risks.md`. `test_families_independent` is
!! the cheap in-suite version; this is the one to reach for at 500 000 seeds.
!!
!! Build and run with `--profile release`, as for every measurement program under `app/`.
program probe_weighted_coupling
    use iso_fortran_env, only: int64, real64, output_unit
    use parquet
    implicit none
    integer, parameter :: N = 50
    integer(int64), parameter :: NSEED = 500000_int64
    real(real64) :: w(N), total, u, acc
    integer(int64) :: s, race1, tree1, naive1, both, both_diffstream, naive_both, nr1, nb1, i
    integer(int64) :: perm(N), item
    type(pf_weighted_draw) :: d, d2
    logical :: ok
    integer(int64) :: agree, agree_ds

    do i = 1, N
        w(i) = real(i, real64)
    end do
    total = sum(w)
    agree = 0; agree_ds = 0; nr1 = 0; nb1 = 0; naive_both = 0
    call d%init(w, 1_int64)                 ! %init once, then %reseed: it refuses a second %init
    call d2%init(w, 1_int64, 7_int64)
    do s = 1_int64, NSEED
        call pf_weighted_permutation(perm, w, s)
        race1 = perm(1)
        call d%reseed(s)
        call d%next(item, ok)
        tree1 = item
        if (race1 == tree1) agree = agree + 1_int64
        ! control: the tree on a DIFFERENT stream
        call d2%reseed(s, 7_int64)
        call d2%next(item, ok)
        if (race1 == item) agree_ds = agree_ds + 1_int64
        ! the pre-fix tree rule, reconstructed from the public generator: descend on the raw
        ! first uniform scaled into [0, total)
        u = pf_random_at(s, 0_int64, draw=1_int64) * total
        acc = 0.0_real64
        naive1 = N
        do i = 1, N
            acc = acc + w(i)
            if (u < acc) then
                naive1 = i
                exit
            end if
        end do
        if (race1 == 1_int64) then
            nr1 = nr1 + 1_int64
            if (tree1 == 1_int64) nb1 = nb1 + 1_int64
            if (naive1 == 1_int64) naive_both = naive_both + 1_int64
        end if
    end do
    write(output_unit,'(a,i0,a,i0,a)') "seeds                        : ", NSEED, " (n=", int(N,int64), ")"
    write(output_unit,'(a,f8.4,a)')    "agreement  same seed+stream  : ", 100.0_real64*real(agree,real64)/real(NSEED,real64), " %"
    write(output_unit,'(a,f8.4,a)')    "agreement  different stream  : ", &
        100.0_real64*real(agree_ds,real64)/real(NSEED,real64), " %"
    write(output_unit,'(a,f8.4,a)')    "independent would be         : ", 100.0_real64*sum((w/total)**2), " %"
    write(output_unit,'(a)') ""
    write(output_unit,'(a,i0)')        "race chose item 1            : ", nr1
    write(output_unit,'(a,i0,a,f7.2,a)') "  ... tree also chose it     : ", nb1, "  (", &
        100.0_real64*real(nb1,real64)/max(1.0_real64,real(nr1,real64)), " %)"
    write(output_unit,'(a,i0,a,f7.2,a)') "  ... naive rule also did    : ", naive_both, &
        "  (", 100.0_real64*real(naive_both,real64)/max(1.0_real64,real(nr1,real64)), " %)"
    write(output_unit,'(a,f7.4,a)')    "  independent expectation    :         (", 100.0_real64*w(1)/total, " %)"
end program probe_weighted_coupling
