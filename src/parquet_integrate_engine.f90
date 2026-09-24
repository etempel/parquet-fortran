!> The vendored QUADPACK engine: the 21-point Gauss-Kronrod rule, the adaptive-bisection driver
!> over a finite range, the error-ordered subinterval list and the Wynn-epsilon table.
!!
!! **Provenance.** These routines are derived from QUADPACK (R. Piessens, E. de Doncker-Kapenga,
!! C. W. Ueberhuber and D. K. Kahaner, *QUADPACK: A Subroutine Package for Automatic
!! Integration*, Springer 1983), which is in the public domain, as modernised by Jacob Williams
!! in `https://github.com/jacobwilliams/quadpack` (BSD-3-Clause), commit
!! `ab42d50794280a8e7996f2ee4cda1f8753dfde5b`. `dqk21`, `dqagse`, `dqpsrt` and `dqelg` of that
!! repository's `src/quadpack_generic.F90` became `qk21`, `qagse`, `qpsrt` and `qelg` here.
!!
!! BSD-3-Clause, as carried by the upstream repository:
!!
!! Redistribution and use in source and binary forms, with or without modification, are permitted
!! provided that the following conditions are met: (1) Redistributions of source code must retain
!! the above copyright notice, this list of conditions and the following disclaimer.
!! (2) Redistributions in binary form must reproduce the above copyright notice, this list of
!! conditions and the following disclaimer in the documentation and/or other materials provided
!! with the distribution. (3) Neither the name of the copyright holder nor the names of its
!! contributors may be used to endorse or promote products derived from this software without
!! specific prior written permission. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
!! CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
!! NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
!! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
!! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
!! THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!!
!! **A bug found here is checked against upstream before it is fixed here.** The numerical logic
!! is a transcription: the abscissae, the weights, the round-off tests, the extrapolation table
!! and the divergence test are upstream's, and a difference from upstream is a defect in this
!! file unless it is one of the deviations below.
!!
!! **Deviations from the upstream text**, each deliberate:
!!
!! 1. The integrand is a `class(pf_integrand), intent(inout)` dummy instead of
!!    `procedure(func) :: f`; every evaluation is `f%eval(x)`. QUADPACK's procedure interface has
!!    no user-data argument, and this library passes no internal procedure as an actual argument.
!! 2. Two evaluation flags travel beside the integrand. With `log_base` the rule works in
!!    `u = log x` and evaluates `f(exp(u))*exp(u)`; with `negate` it evaluates `f(-x)`, which is
!!    how the outward walk answers a range whose lower bound is `-infinity`. Both are tested once
!!    per point, in `eval_point`, and neither is visible to the adaptive logic above it.
!! 3. A real evaluation counter (`neval`, `intent(inout)`) is incremented once per `f%eval`,
!!    replacing QUADPACK's `Neval = 42*Last - 21`. The formula remains true on a finite, non-log
!!    run and `test_neval_matches_the_rule_formula` asserts it as a cross-check.
!! 4. The rule routine fills a 21-element record of abscissa, weight and value for the
!!    subinterval it was applied to; `qagse` copies those into the record slot the subinterval
!!    ends up in, so the slots `1:last` are the final partition. Nothing is copied when the
!!    caller did not ask for `points`.
!! 5. Every evaluation is screened for a non-finite value. The first one is recorded in a
!!    `bad_value` carried through the rule and the driver in place of upstream's nothing, and the
!!    routines here stop at their next check rather than letting a NaN into the partition; the
!!    driver turns it into `PF_INT_BAD_VALUE`. The screen is why `qk21` needs no `context`: it
!!    reports rather than aborts. The rule itself consumes a zero in place of every non-finite
!!    value, so no arithmetic here ever raises on one.
!! 6. `ier` is returned RAW: QUADPACK's `if (ier > 2) ier = ier - 1` shift is applied once, by the
!!    driver. The input-validity `ier = 6` cannot occur, because the driver refuses its trigger
!!    first; reaching it aborts rather than returning a status.
!! 7. `xerror` is gone. Nothing here writes, prints or reaches `error_unit`.
!! 8. Every local upstream leaves undefined on some path is initialised: `correc`, `small`,
!!    `erlarg`, `ertest`. Upstream reads `correc` when `ierro == 3` without having necessarily
!!    assigned it, which is a signalling NaN in an addition under nagfor's `-nan`.
!! 9. `wp` is `real64`; `epmach`, `uflow` and `oflow` are `epsilon`, `tiny` and `huge` parameters.
!! 10. The subinterval lists are allocatable and grow geometrically from 64 rather than being
!!     allocated at the size the budget implies (`grow_work`); `qagse` asks for the growth at the
!!     top of the loop body rather than in a `do while` header.
!! 11. `extrapolate` gates the extrapolation branch by starting `noext` at `.not. extrapolate`.
!!     With the gate closed the loop is plain adaptive bisection and the result is the partition
!!     sum, which is what the `points` record reproduces exactly.
!! 12. 132 columns, lower-case dummy names, and `!>`/`!!` doc-comments throughout; upstream's
!!     argument documentation is kept where it is still correct.
!! 13. `qagse` returns `defabs` -- the first rule application's approximation to the integral of
!!     `|f|` over the whole range it was given -- which upstream keeps as a local. The outward
!!     walk measures its round-off floor against the sum of these, exactly as qfeet measured its
!!     own against the unrefined rule's `resabs`, and nothing else reads it.
!! 14. `qelg` forms each of the table's three reciprocals `1/delta` from the difference capped at
!!     `1/tiny` in magnitude (`RECIP_CAP`), stored before it is divided into. Upstream holds
!!     `oflow` in the table as a placeholder for the element not yet computed, so the first
!!     difference of every step is about `huge` and its reciprocal subnormal: every extrapolated
!!     call raised IEEE_UNDERFLOW for a term that cannot change the answer
!!     (`test_extrapolation_raises_no_underflow`). A difference within the cap is divided into
!!     unchanged, so the table's values are upstream's wherever no reciprocal was subnormal.
!!
!! **What is deliberately NOT vendored: `dqagie` and `dqk15i`.** QUADPACK answers an infinite
!! range by the change of variable `x = a + (1 - t)/t`, bisecting in `t` over `(0, 1]` with a
!! 15-point rule. This library answers one by walking outward in `log x` a panel at a time,
!! applying `qagse` and the 21-point rule above to each panel, and the two infinite-range
!! routines were weighed against that walk and left upstream.
!!
!! The transform cannot be the only method, because of where it puts its points: everything
!! beyond about `a + 19` lands between the first rule's last two abscissae, so a unit-width
!! feature at `x = 40` is never sampled, every value read is zero, the error estimate is zero,
!! nothing is ever bisected, and the answer comes back as zero with `converged` true. The walk
!! samples 21 points per factor of e the whole way out and finds it.
!!
!! The open question was whether to vendor them ANYWAY for the tail beyond the last walked panel
!! -- a hybrid, walking near the bound and transforming far from it -- which was to be built only
!! if the walk alone cost more than two and a half times `dqagi` on the tail shapes both methods
!! handle. Measured at `rtol = 1e-10, atol = 1e-14`, the walk costs a geometric mean of 1.53
!! times `dqagi`'s count over `exp(-x)`, `x**-1.5` and `exp(-x**2)`, and is CHEAPER than `dqagi`
!! on two of those three, at 0.78 and 0.75; `x**-1.5` is the one it is dear on, at 6.11. That is
!! inside the line, so the hybrid was not built and these two routines have no reason to be here.
!! Vendoring them would mean applying every deviation above a second time -- the
!! `class(pf_integrand)` dummy, the evaluation counter, the record slots carrying the transform's
!! `1/t**2` Jacobian in the weight, the non-finite screen and the initialisations -- for roughly
!! 900 lines, to accelerate the single shape where the walk is expensive.
!!
!! The panel width those figures were measured at, and why it is one rather than two, is recorded
!! at `TAIL_STEP` in `parquet_integrate.f90`.
submodule (parquet_integrate) parquet_integrate_engine

    implicit none

    !> Relative machine accuracy, QUADPACK's `epmach`.
    real(real64), parameter :: EPMACH = epsilon(1.0_real64)
    !> The smallest positive normal magnitude, QUADPACK's `uflow`.
    real(real64), parameter :: UFLOW = tiny(1.0_real64)
    !> The largest finite magnitude, QUADPACK's `oflow`, and the non-finite screen's threshold.
    real(real64), parameter :: OFLOW = huge(1.0_real64)
    !> The largest magnitude whose reciprocal is normal, `1/tiny` (exactly `2**1022`): `qelg` caps
    !> each difference at it before dividing (deviation 14).
    real(real64), parameter :: RECIP_CAP = 1.0_real64/UFLOW

    ! The abscissae and weights are given for the interval (-1, 1). Because of symmetry only the
    ! positive abscissae and their corresponding weights are given.

    !> Weights of the 10-point Gauss rule.
    real(real64), dimension(5), parameter :: WG = [ &
        6.66713443086881375935688098933317928579e-2_real64, &
        1.49451349150580593145776339657697332403e-1_real64, &
        2.19086362515982043995534934228163192459e-1_real64, &
        2.69266719309996355091226921569469352860e-1_real64, &
        2.95524224714752870173892994651338329421e-1_real64]

    !> Abscissae of the 21-point Kronrod rule: `XGK(2)`, `XGK(4)`, ... are the abscissae of the
    !! 10-point Gauss rule, and `XGK(1)`, `XGK(3)`, ... those optimally added to it.
    real(real64), dimension(11), parameter :: XGK = [ &
        9.95657163025808080735527280689002847921e-1_real64, &
        9.73906528517171720077964012084452053428e-1_real64, &
        9.30157491355708226001207180059508346225e-1_real64, &
        8.65063366688984510732096688423493048528e-1_real64, &
        7.80817726586416897063717578345042377163e-1_real64, &
        6.79409568299024406234327365114873575769e-1_real64, &
        5.62757134668604683339000099272694140843e-1_real64, &
        4.33395394129247190799265943165784162200e-1_real64, &
        2.94392862701460198131126603103865566163e-1_real64, &
        1.48874338981631210884826001129719984618e-1_real64, &
        0.00000000000000000000000000000000000000e0_real64]

    !> Weights of the 21-point Kronrod rule, in the order of `XGK`.
    real(real64), dimension(11), parameter :: WGK = [ &
        1.16946388673718742780643960621920483962e-2_real64, &
        3.25581623079647274788189724593897606174e-2_real64, &
        5.47558965743519960313813002445801763737e-2_real64, &
        7.50396748109199527670431409161900093952e-2_real64, &
        9.31254545836976055350654650833663443900e-2_real64, &
        1.09387158802297641899210590325804960272e-1_real64, &
        1.23491976262065851077958109831074159512e-1_real64, &
        1.34709217311473325928054001771706832761e-1_real64, &
        1.42775938577060080797094273138717060886e-1_real64, &
        1.47739104901338491374841515972068045524e-1_real64, &
        1.49445554002916905664936468389821203745e-1_real64]

contains

    !> Evaluates the integrand at one abscissa of the engine's variable, screens the value and
    !! fills that point's record entry.
    !!
    !! `u` is the abscissa in the variable the rule works in and `wgt` the Kronrod weight times
    !! the half-length, before any change of variable. The record is always in the CALLER's `x`:
    !! under `log_base` the point is `exp(u)`, its weight carries the `exp(u)` of the Jacobian and
    !! its value is `f(exp(u))` rather than the `f(exp(u))*exp(u)` the rule consumes, so that
    !! `wrec*frec` is this point's contribution either way.
    !!
    !! **`negate` puts its sign on the abscissa, never on the weight.** Integrating `f` over
    !! `(-inf, b]` is integrating `g(y) = f(-y)` over `[-b, +inf)`, and a rule `sum(w*g(y))` is
    !! `sum(w*f(-y))` -- the same weights, read at the mirrored points. Reversing the caller's
    !! bounds is what absorbs the `-1` of `dx = -dy`, so a sign on the weight would make the
    !! record's weighted sum the negative of the integral it is supposed to reproduce.
    !!
    !! Cannot be `pure`: it calls the caller's `eval`, whose passed-object dummy is `intent(inout)`
    !! so that an integrand may count its own calls.
    subroutine eval_point(f, u, log_base, negate, wgt, neval, xrec, wrec, frec, fval, bad)
        class(pf_integrand), intent(inout)     :: f        !! the integrand
        real(real64), intent(in)               :: u        !! abscissa, in the engine's variable
        logical, intent(in)                    :: log_base !! the engine's variable is `log x`
        logical, intent(in)                    :: negate   !! evaluate and record at `-x`
        real(real64), intent(in)               :: wgt      !! Kronrod weight times the half-length
        integer, intent(inout)                 :: neval    !! evaluation counter, incremented
        real(real64), intent(out)              :: xrec     !! recorded abscissa, the caller's `x`
        real(real64), intent(out)              :: wrec     !! recorded weight, with the Jacobian
        real(real64), intent(out)              :: frec     !! recorded value, `f(x)`
        real(real64), intent(out)              :: fval     !! value the RULE consumes
        type(bad_value), intent(inout)         :: bad      !! set on the FIRST non-finite value

        real(real64) :: x, y, fx
        logical      :: is_bad

        if (log_base) then
            y = exp(u)
        else
            y = u
        end if
        if (negate) then
            x = -y
        else
            x = y
        end if

        fx = f%eval(x)
        neval = neval + 1

        ! The self-comparison rather than ieee_is_nan because this is the innermost loop of the
        ! whole module (fortran-gotchas.md); the magnitude test is reached only for a non-NaN, so
        ! no ordered comparison ever sees a NaN.
        is_bad = .false.
        if (fx /= fx) then
            is_bad = .true.
        else if (abs(fx) > OFLOW) then
            is_bad = .true.
        end if
        ! The FIRST bad point is the one kept: everything evaluated after it may be a consequence
        ! of whatever went wrong, and the rule goes on filling this point's record entry either
        ! way so that the record stays a complete account of what was asked of the integrand.
        if (is_bad .and. .not. bad%seen) then
            bad%seen = .true.
            bad%x = x
        end if

        xrec = x
        frec = fx
        ! `y`, not `x`: under `negate` the abscissa is mirrored but the Jacobian `dy/du` of the
        ! walk's own `y = exp(u)` is positive, and it is the Jacobian that is a weight.
        if (log_base) then
            wrec = wgt*y
        else
            wrec = wgt
        end if
        ! The rule is handed a zero in place of a non-finite value, and only the record keeps it.
        ! The rule goes on combining this point with its neighbours after the screen has fired --
        ! `Inf - Inf` in its error estimate, `min` and ordered comparisons over a NaN -- and each
        ! raises IEEE_INVALID, which under unmasked traps ends the process the status exists to
        ! keep alive. What the rule then returns is discarded: on `PF_INT_BAD_VALUE` the result is
        ! worth nothing by contract (`test_status_divergent`, `test_non_finite_value_is_reported`).
        if (is_bad) then
            fval = 0.0_real64
        else if (log_base) then
            fval = fx*y
        else
            fval = fx
        end if

    end subroutine eval_point

    module procedure qk21

        real(real64) :: centr, hlgth, dhlgth, absc, fval1, fval2, fsum, fc
        real(real64) :: resg, resk, reskh
        real(real64) :: fv1(10), fv2(10)
        integer      :: j, jtw, jtwm1, k

        centr = 0.5_real64*(a + b)
        hlgth = 0.5_real64*(b - a)
        dhlgth = abs(hlgth)

        ! Compute the 21-point Kronrod approximation to the integral, and estimate the absolute
        ! error. `k` walks the record: the centre first, then the two points of each pair.

        resg = 0.0_real64
        k = 1
        call eval_point(f, centr, log_base, negate, WGK(11)*hlgth, neval, rx(k), rw(k), rf(k), &
                        fc, bad)
        resk = WGK(11)*fc
        resabs = abs(resk)
        do j = 1, 5
            jtw = 2*j
            absc = hlgth*XGK(jtw)
            k = k + 1
            call eval_point(f, centr - absc, log_base, negate, WGK(jtw)*hlgth, neval, &
                            rx(k), rw(k), rf(k), fval1, bad)
            k = k + 1
            call eval_point(f, centr + absc, log_base, negate, WGK(jtw)*hlgth, neval, &
                            rx(k), rw(k), rf(k), fval2, bad)
            fv1(jtw) = fval1
            fv2(jtw) = fval2
            fsum = fval1 + fval2
            resg = resg + WG(j)*fsum
            resk = resk + WGK(jtw)*fsum
            resabs = resabs + WGK(jtw)*(abs(fval1) + abs(fval2))
        end do
        do j = 1, 5
            jtwm1 = 2*j - 1
            absc = hlgth*XGK(jtwm1)
            k = k + 1
            call eval_point(f, centr - absc, log_base, negate, WGK(jtwm1)*hlgth, neval, &
                            rx(k), rw(k), rf(k), fval1, bad)
            k = k + 1
            call eval_point(f, centr + absc, log_base, negate, WGK(jtwm1)*hlgth, neval, &
                            rx(k), rw(k), rf(k), fval2, bad)
            fv1(jtwm1) = fval1
            fv2(jtwm1) = fval2
            fsum = fval1 + fval2
            resk = resk + WGK(jtwm1)*fsum
            resabs = resabs + WGK(jtwm1)*(abs(fval1) + abs(fval2))
        end do
        reskh = resk*0.5_real64
        resasc = WGK(11)*abs(fc - reskh)
        do j = 1, 10
            resasc = resasc + WGK(j)*(abs(fv1(j) - reskh) + abs(fv2(j) - reskh))
        end do
        res = resk*hlgth
        resabs = resabs*dhlgth
        resasc = resasc*dhlgth
        abserr = abs((resk - resg)*hlgth)
        if (resasc /= 0.0_real64 .and. abserr /= 0.0_real64) &
            abserr = resasc*min(1.0_real64, (200.0_real64*abserr/resasc)**1.5_real64)
        if (resabs > UFLOW/(50.0_real64*EPMACH)) &
            abserr = max((EPMACH*50.0_real64)*resabs, abserr)

    end procedure qk21

    module procedure qpsrt

        real(real64) :: errmax, errmin
        integer      :: i, ibeg, ido, isucc, j, jbnd, jupbn, k

        main: block

            ! Check whether the list contains more than two error estimates.

            if (last > 2) then

                ! This part is executed only if, due to a difficult integrand, subdivision
                ! increased the error estimate. In the normal case the insert procedure should
                ! start after the nrmax-th largest error estimate.

                errmax = elist(maxerr)
                if (nrmax /= 1) then
                    ido = nrmax - 1
                    do i = 1, ido
                        isucc = iord(nrmax - 1)
                        if (errmax <= elist(isucc)) exit
                        iord(nrmax) = isucc
                        nrmax = nrmax - 1
                    end do
                end if

                ! Compute the number of elements to be maintained in descending order. This
                ! number depends on the number of subdivisions still allowed.

                jupbn = last
                if (last > (limit/2 + 2)) jupbn = limit + 3 - last
                errmin = elist(last)

                ! Insert errmax by traversing the list top-down, starting the comparison from the
                ! element elist(iord(nrmax+1)).

                jbnd = jupbn - 1
                ibeg = nrmax + 1
                if (ibeg <= jbnd) then
                    do i = ibeg, jbnd
                        isucc = iord(i)
                        if (errmax >= elist(isucc)) then
                            ! Insert errmin by traversing the list bottom-up.
                            iord(i - 1) = maxerr
                            k = jbnd
                            do j = i, jbnd
                                isucc = iord(k)
                                if (errmin < elist(isucc)) then
                                    iord(k + 1) = last
                                    exit main
                                end if
                                iord(k + 1) = isucc
                                k = k - 1
                            end do
                            iord(i) = last
                            exit main
                        end if
                        iord(i - 1) = isucc
                    end do
                end if
                iord(jbnd) = maxerr
                iord(jupbn) = last
            else
                iord(1) = 1
                iord(2) = 2
            end if

        end block main

        ! Set maxerr and ermax.
        maxerr = iord(nrmax)
        ermax = elist(maxerr)

    end procedure qpsrt

    module procedure qelg

        real(real64) :: delta1, delta2, delta3, epsinf
        real(real64) :: err1, err2, err3, e0, e1, e1abs, e2, e3
        real(real64) :: resq, ss, tol1, tol2, tol3, error
        integer      :: i, ib, ib2, ie, indx, k1, k2, k3, num, newelm
        ! Each difference capped at RECIP_CAP, stored before it is divided into (deviation 14), so
        ! an optimiser cannot split the cap into a quotient of the uncapped difference.
        real(real64), volatile :: cap1, cap2, cap3

        nres = nres + 1
        abserr = OFLOW
        res = epstab(n)
        if (n >= 3) then
            epstab(n + 2) = epstab(n)
            newelm = (n - 1)/2
            epstab(n) = OFLOW
            num = n
            k1 = n
            do i = 1, newelm
                k2 = k1 - 1
                k3 = k1 - 2
                resq = epstab(k1 + 2)
                e0 = epstab(k3)
                e1 = epstab(k2)
                e2 = resq
                e1abs = abs(e1)
                delta2 = e2 - e1
                err2 = abs(delta2)
                tol2 = max(abs(e2), e1abs)*EPMACH
                delta3 = e1 - e0
                err3 = abs(delta3)
                tol3 = max(e1abs, abs(e0))*EPMACH
                if (err2 > tol2 .or. err3 > tol3) then
                    e3 = epstab(k1)
                    epstab(k1) = e1
                    delta1 = e1 - e3
                    err1 = abs(delta1)
                    tol1 = max(e1abs, abs(e3))*EPMACH
                    ! If two elements are very close to each other, omit a part of the table by
                    ! adjusting the value of n.
                    if (err1 > tol1 .and. err2 > tol2 .and. err3 > tol3) then
                        ! Deviation 14: `delta1` is about `huge` on every step's first element, the
                        ! table holding `OFLOW` there, and its reciprocal would be subnormal. The
                        ! three errors exceed their tolerances, so none is a NaN or a zero, and
                        ! `sign` puts back each difference's own sign.
                        cap1 = sign(min(err1, RECIP_CAP), delta1)
                        cap2 = sign(min(err2, RECIP_CAP), delta2)
                        cap3 = sign(min(err3, RECIP_CAP), delta3)
                        ss = 1.0_real64/cap1 + 1.0_real64/cap2 - 1.0_real64/cap3
                        epsinf = abs(ss*e1)
                        ! Test to detect irregular behaviour in the table, and eventually omit a
                        ! part of it by adjusting the value of n.
                        if (epsinf > 0.1e-03_real64) then
                            ! Compute a new element and eventually adjust the value of res.
                            resq = e1 + 1.0_real64/ss
                            epstab(k1) = resq
                            k1 = k1 - 2
                            error = err2 + abs(resq - e2) + err3
                            if (error <= abserr) then
                                abserr = error
                                res = resq
                            end if
                            cycle
                        end if
                    end if
                    n = i + i - 1
                    exit
                else
                    ! If e0, e1 and e2 are equal to within machine accuracy, convergence is
                    ! assumed.
                    res = resq
                    abserr = err2 + err3
                    abserr = max(abserr, 5.0_real64*EPMACH*abs(res))
                    return
                end if
            end do

            ! Shift the table.
            if (n == LIMEXP) n = 2*(LIMEXP/2) - 1
            ib = 1
            if ((num/2)*2 == num) ib = 2
            ie = newelm + 1
            do i = 1, ie
                ib2 = ib + 2
                epstab(ib) = epstab(ib2)
                ib = ib2
            end do
            if (num /= n) then
                indx = num - n + 1
                do i = 1, n
                    epstab(i) = epstab(indx)
                    indx = indx + 1
                end do
            end if
            if (nres >= 4) then
                ! Compute the error estimate.
                abserr = abs(res - res3la(3)) + abs(res - res3la(2)) + abs(res - res3la(1))
                res3la(1) = res3la(2)
                res3la(2) = res3la(3)
                res3la(3) = res
            else
                res3la(nres) = res
                abserr = OFLOW
            end if
        end if
        abserr = max(abserr, 5.0_real64*EPMACH*abs(res))

    end procedure qelg

    module procedure qagse

        real(real64) :: abseps, correc, dres, ertest, resabs, reseps, res3la(3)
        integer      :: id, ierro, iroff1, iroff2, iroff3, jupbnd, k, ksgn, ktmin, nrmax
        real(real64) :: area12, erro12
        real(real64) :: area1, a1, b1, defab1, error1
        real(real64) :: area2, a2, b2, defab2, error2
        real(real64) :: rlist2(LIMEXP + 2)
        integer      :: maxerr, nres, numrl2, bad_last
        real(real64) :: errmax, erlast, area, errsum, errbnd, small, erlarg
        logical      :: extrap, noext
        real(real64) :: sx1(GK_POINTS), sw1(GK_POINTS), sf1(GK_POINTS)
        real(real64) :: sx2(GK_POINTS), sw2(GK_POINTS), sf2(GK_POINTS)

        ! Deviation 8: every local upstream may read before assigning is initialised here.
        correc = 0.0_real64
        small = 0.0_real64
        erlarg = 0.0_real64
        ertest = 0.0_real64
        res3la = 0.0_real64
        rlist2 = 0.0_real64

        ier = 0
        last = 0
        bad_last = 0
        res = 0.0_real64
        abserr = 0.0_real64
        extrapolated = .false.
        work%alist(1) = a
        work%blist(1) = b
        work%rlist(1) = 0.0_real64
        work%elist(1) = 0.0_real64

        ! Deviation 6: the driver refuses this input before the engine sees it, so reaching it is
        ! a defect in the driver rather than a status a caller should have to read.
        if (epsabs <= 0.0_real64 .and. epsrel < max(50.0_real64*EPMACH, 0.5e-28_real64)) then
            ! GCOVR_EXCL_START -- not reachable: deviation 6 above, the driver refuses this pair first.
            call integrate_abort("internal: the engine was given a tolerance pair the driver " &
                                 // "should have refused", context)
            ! GCOVR_EXCL_STOP
        end if

        main: block

            ! First approximation to the integral.

            ierro = 0
            call qk21(f, a, b, log_base, negate, res, abserr, defabs, resabs, neval, &
                      sx1, sw1, sf1, bad)

            ! Test on accuracy.

            dres = abs(res)
            errbnd = max(epsabs, epsrel*dres)
            last = 1
            work%rlist(1) = res
            work%elist(1) = abserr
            work%iord(1) = 1
            if (work%record) then
                work%rx(:, 1) = sx1
                work%rw(:, 1) = sw1
                work%rf(:, 1) = sf1
            end if

            ! Deviation 5: a non-finite value ends the application here, after the slot it filled
            ! has been written, so the driver receives a well-formed partition of one subinterval
            ! beside the status it is about to overwrite. None of the accuracy tests below means
            ! anything once a value was not a number.
            if (bad%seen) return

            if (abserr <= 100.0_real64*EPMACH*defabs .and. abserr > errbnd) ier = 2
            if (limit == 1) ier = 1
            if (ier /= 0 .or. (abserr <= errbnd .and. abserr /= resabs) .or. &
                abserr == 0.0_real64) return

            ! Initialization.

            rlist2(1) = res
            errmax = abserr
            maxerr = 1
            area = res
            errsum = abserr
            abserr = OFLOW
            nrmax = 1
            nres = 0
            numrl2 = 2
            ktmin = 0
            extrap = .false.
            noext = .not. extrapolate
            iroff1 = 0
            iroff2 = 0
            iroff3 = 0
            ksgn = -1
            if (dres >= (1.0_real64 - 50.0_real64*EPMACH)*defabs) ksgn = 1

            ! Main do-loop.

            loop: do last = 2, limit

                ! Deviation 10: the lists grow to hold slot `last` before anything writes to it.
                if (last > work%capacity) call grow_work(work, last, limit, last - 1)

                ! Bisect the subinterval with the nrmax-th largest error estimate.

                a1 = work%alist(maxerr)
                b1 = 0.5_real64*(work%alist(maxerr) + work%blist(maxerr))
                a2 = b1
                b2 = work%blist(maxerr)
                erlast = errmax
                call qk21(f, a1, b1, log_base, negate, area1, error1, resabs, defab1, neval, &
                          sx1, sw1, sf1, bad)
                call qk21(f, a2, b2, log_base, negate, area2, error2, resabs, defab2, neval, &
                          sx2, sw2, sf2, bad)

                ! Deviation 5: neither child is folded into the partition, so the lists still
                ! describe it as it stood before this bisection, one subinterval short of the
                ! loop index. `bad_last` carries that count past the loop, because a DO variable
                ! may not be redefined inside its own construct; the plain sum below the block is
                ! then over the partition that really exists.
                if (bad%seen) then
                    bad_last = last - 1
                    exit main
                end if

                ! Improve previous approximations to the integral and error, and test for
                ! accuracy.

                area12 = area1 + area2
                erro12 = error1 + error2
                errsum = errsum + erro12 - errmax
                area = area + area12 - work%rlist(maxerr)
                if (defab1 /= error1 .and. defab2 /= error2) then
                    if (abs(work%rlist(maxerr) - area12) <= 0.1e-4_real64*abs(area12) &
                        .and. erro12 >= 0.99_real64*errmax) then
                        if (extrap) iroff2 = iroff2 + 1
                        if (.not. extrap) iroff1 = iroff1 + 1
                    end if
                    if (last > 10 .and. erro12 > errmax) iroff3 = iroff3 + 1
                end if
                work%rlist(maxerr) = area1
                work%rlist(last) = area2
                errbnd = max(epsabs, epsrel*abs(area))

                ! Test for roundoff error and eventually set the error flag.

                if (iroff1 + iroff2 >= 10 .or. iroff3 >= 20) ier = 2
                if (iroff2 >= 5) ierro = 3

                ! Set the error flag in the case that the number of subintervals equals limit.

                if (last == limit) ier = 1

                ! Set the error flag in the case of bad integrand behaviour at a point of the
                ! integration range.

                if (max(abs(a1), abs(b2)) <= (1.0_real64 + 100.0_real64*EPMACH) &
                    *(abs(a2) + 1000.0_real64*UFLOW)) ier = 4

                ! Append the newly-created intervals to the list. Deviation 4: each child's
                ! record goes to the slot that child's bounds went to, so slots 1:last stay the
                ! final partition.

                if (error2 > error1) then
                    work%alist(maxerr) = a2
                    work%alist(last) = a1
                    work%blist(last) = b1
                    work%rlist(maxerr) = area2
                    work%rlist(last) = area1
                    work%elist(maxerr) = error2
                    work%elist(last) = error1
                    if (work%record) then
                        work%rx(:, maxerr) = sx2
                        work%rw(:, maxerr) = sw2
                        work%rf(:, maxerr) = sf2
                        work%rx(:, last) = sx1
                        work%rw(:, last) = sw1
                        work%rf(:, last) = sf1
                    end if
                else
                    work%alist(last) = a2
                    work%blist(maxerr) = b1
                    work%blist(last) = b2
                    work%elist(maxerr) = error1
                    work%elist(last) = error2
                    if (work%record) then
                        work%rx(:, maxerr) = sx1
                        work%rw(:, maxerr) = sw1
                        work%rf(:, maxerr) = sf1
                        work%rx(:, last) = sx2
                        work%rw(:, last) = sw2
                        work%rf(:, last) = sf2
                    end if
                end if

                ! Maintain the descending ordering in the list of error estimates and select the
                ! subinterval with the nrmax-th largest error estimate, to be bisected next.

                call qpsrt(limit, last, maxerr, errmax, work%elist, work%iord, nrmax)
                if (errsum <= errbnd) exit main
                if (ier /= 0) exit loop
                if (last == 2) then
                    small = abs(b - a)*0.375_real64
                    erlarg = errsum
                    ertest = errbnd
                    rlist2(2) = area
                else if (.not. (noext)) then
                    erlarg = erlarg - erlast
                    if (abs(b1 - a1) > small) erlarg = erlarg + erro12
                    if (.not. (extrap)) then
                        ! Test whether the interval to be bisected next is the smallest interval.
                        if (abs(work%blist(maxerr) - work%alist(maxerr)) > small) cycle loop
                        extrap = .true.
                        nrmax = 2
                    end if
                    if (ierro /= 3 .and. erlarg > ertest) then
                        ! The smallest interval has the largest error. Before bisecting, decrease
                        ! the sum of the errors over the larger intervals (erlarg) and perform
                        ! extrapolation.
                        id = nrmax
                        jupbnd = last
                        if (last > (2 + limit/2)) jupbnd = limit + 3 - last
                        do k = id, jupbnd
                            maxerr = work%iord(nrmax)
                            errmax = work%elist(maxerr)
                            if (abs(work%blist(maxerr) - work%alist(maxerr)) > small) cycle loop
                            nrmax = nrmax + 1
                        end do
                    end if

                    ! Perform extrapolation.

                    numrl2 = numrl2 + 1
                    rlist2(numrl2) = area
                    call qelg(numrl2, rlist2, reseps, abseps, res3la, nres)
                    ktmin = ktmin + 1
                    if (ktmin > 5 .and. abserr < 0.1e-02_real64*errsum) ier = 5
                    if (abseps < abserr) then
                        ktmin = 0
                        abserr = abseps
                        res = reseps
                        correc = erlarg
                        ertest = max(epsabs, epsrel*abs(reseps))
                        if (abserr <= ertest) exit loop
                    end if

                    ! Prepare the bisection of the smallest interval.

                    if (numrl2 == 1) noext = .true.
                    if (ier == 5) exit loop
                    maxerr = work%iord(1)
                    errmax = work%elist(maxerr)
                    nrmax = 1
                    extrap = .false.
                    small = small*0.5_real64
                    erlarg = errsum
                end if
            end do loop

            ! Set the final result and error estimate.

            if (abserr /= OFLOW) then
                extrapolated = .true.
                if (ier + ierro /= 0) then
                    if (ierro == 3) abserr = abserr + correc
                    if (ier == 0) ier = 3
                    if (res == 0.0_real64 .or. area == 0.0_real64) then
                        if (abserr > errsum) exit main
                        if (area == 0.0_real64) return
                    else if (abserr/abs(res) > errsum/abs(area)) then
                        exit main
                    end if
                end if

                ! Test on divergence.

                if (ksgn /= (-1) .or. max(abs(res), abs(area)) > defabs*0.01_real64) then
                    if (0.01_real64 > (res/area) .or. (res/area) > 100.0_real64 &
                        .or. errsum > abs(area)) ier = 6
                end if
                return
            end if

        end block main

        ! Compute the global integral sum. This is the whole of the gated-off path: the result is
        ! the plain sum over the final partition, which is what the `points` record reproduces.

        ! Deviation 5: a non-finite value met in the bisection loop left the partition one shorter
        ! than the loop index reached, and the sum below is over the partition, not the index.
        if (bad_last > 0) last = bad_last

        extrapolated = .false.
        res = sum(work%rlist(1:last))
        abserr = errsum

    end procedure qagse

end submodule parquet_integrate_engine
