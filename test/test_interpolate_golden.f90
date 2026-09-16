!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_interpolate_vectors.py
!
!> Golden expectations for `parquet_interpolate`, derived EXACTLY over rationals.
!!
!! **The oracle shares no arithmetic with the library.** The generator transcribes each
!! interpolant's definition over `fractions.Fraction` -- the tridiagonal solve and the segment
!! polynomials carried out exactly over the fixture's rational values -- and emits the
!! round-to-nearest double of each exact result. scipy appears only in its `--self-test`, which
!! confirms the model computes what `scipy.interpolate` computes.
!!
!! Every knot, ordinate and query here is a dyadic rational, so each literal is an exact double.
!! For a fixture `F`: `F_X` and `F_Y` are the table, `F_Q` the interior queries and `F_O` the
!! outside ones; `F_<METHOD>` holds the expectation at each of `F_Q`, and `F_<METHOD>_OUT` at each
!! of `F_O` under `outside="extrapolate"`. `P6_*` is the guide page's `x**2` example.
module test_interpolate_golden
    use iso_fortran_env, only : real64
    implicit none
    public

    !> Interior queries each fixture is evaluated at.
    integer, parameter :: GI_NQ = 25
    !> Outside queries each fixture is evaluated at: two below the table, two above.
    integer, parameter :: GI_NO = 4

    ! ---- U9: nine knots a quarter apart: the evenly spaced table, bracketed by arithmetic ----

    !> Knots in fixture U9.
    integer, parameter :: U9_N = 9
    !> Fixture U9's abscissae, strictly increasing.
    real(real64), parameter :: U9_X(U9_N) = [0.0_real64, 0.25_real64, 0.5_real64, 0.75_real64, 1.0_real64, 1.25_real64, &
        1.5_real64, 1.75_real64, 2.0_real64]
    !> Fixture U9's ordinates.
    real(real64), parameter :: U9_Y(U9_N) = [0.0_real64, -0.5625_real64, 0.625_real64, 0.6875_real64, -0.375_real64, &
        0.3125_real64, -0.125_real64, -0.25_real64, -0.0625_real64]
    !> Fixture U9's interior queries.
    real(real64), parameter :: U9_Q(GI_NQ) = [0.03125_real64, 0.09375_real64, 0.1875_real64, 0.25_real64, 0.34375_real64, &
        0.40625_real64, 0.5_real64, 0.59375_real64, 0.65625_real64, 0.75_real64, 0.8125_real64, 0.90625_real64, 1.0_real64, &
        1.0625_real64, 1.15625_real64, 1.21875_real64, 1.3125_real64, 1.375_real64, 1.46875_real64, 1.5625_real64, &
        1.625_real64, 1.71875_real64, 1.78125_real64, 1.875_real64, 1.96875_real64]
    !> Fixture U9's outside queries.
    real(real64), parameter :: U9_O(GI_NO) = [-0.25_real64, -0.03125_real64, 2.03125_real64, 2.25_real64]
    !> method="linear" at each interior query
    real(real64), parameter :: U9_LINEAR(GI_NQ) = [-0.0703125_real64, -0.2109375_real64, -0.421875_real64, -0.5625_real64, &
        -0.1171875_real64, 0.1796875_real64, 0.625_real64, 0.6484375_real64, 0.6640625_real64, 0.6875_real64, 0.421875_real64, &
        0.0234375_real64, -0.375_real64, -0.203125_real64, 0.0546875_real64, 0.2265625_real64, 0.203125_real64, 0.09375_real64, &
        -0.0703125_real64, -0.15625_real64, -0.1875_real64, -0.234375_real64, -0.2265625_real64, -0.15625_real64, &
        -0.0859375_real64]
    !> method="linear", outside="extrapolate", at each outside query
    real(real64), parameter :: U9_LINEAR_OUT(GI_NO) = [0.5625_real64, 0.0703125_real64, -0.0390625_real64, 0.125_real64]
    !> method="cubic", bc="natural" at each interior query
    real(real64), parameter :: U9_NATURAL(GI_NQ) = [-0.1339744489217542_real64, -0.37767117574745146_real64, &
        -0.59164019712467786_real64, -0.5625_real64, -0.21126653027990958_real64, 0.13464547608140992_real64, 0.625_real64, &
        0.88753221874208965_real64, 0.90578143831907565_real64, 0.6875_real64, 0.38866117811694589_real64, &
        -0.10738060435771239_real64, -0.375_real64, -0.30151142463066088_real64, 0.032285900986773981_real64, &
        0.24740042145719232_real64, 0.3193376454056977_real64, 0.20579220936119294_real64, -0.049452398241180737_real64, &
        -0.22681571949212997_real64, -0.27132401164396169_real64, -0.26431250817996937_real64, -0.23264438589823613_real64, &
        -0.16737116278534608_real64, -0.089586631538941683_real64]
    !> method="cubic", bc="natural", outside="extrapolate", at each outside query
    real(real64), parameter :: U9_NATURAL_OUT(GI_NO) = [0.5625_real64, 0.1339744489217542_real64, -0.03541336846105831_real64, &
        0.125_real64]

    ! ---- G16: sixteen knots whose spacing doubles every second knot, from 1/16 to 8 ----

    !> Knots in fixture G16.
    integer, parameter :: G16_N = 16
    !> Fixture G16's abscissae, strictly increasing.
    real(real64), parameter :: G16_X(G16_N) = [0.0_real64, 0.0625_real64, 0.125_real64, 0.25_real64, 0.375_real64, &
        0.625_real64, 0.875_real64, 1.375_real64, 1.875_real64, 2.875_real64, 3.875_real64, 5.875_real64, 7.875_real64, &
        11.875_real64, 15.875_real64, 23.875_real64]
    !> Fixture G16's ordinates.
    real(real64), parameter :: G16_Y(G16_N) = [-0.265625_real64, 0.03125_real64, 0.0625_real64, -0.171875_real64, &
        -0.03125_real64, -0.15625_real64, 0.09375_real64, 0.078125_real64, -0.203125_real64, -0.109375_real64, -0.28125_real64, &
        -0.078125_real64, -0.140625_real64, 0.171875_real64, 0.21875_real64, 0.0_real64]
    !> Fixture G16's interior queries.
    real(real64), parameter :: G16_Q(GI_NQ) = [0.373046875_real64, 1.119140625_real64, 2.23828125_real64, 2.984375_real64, &
        4.103515625_real64, 4.849609375_real64, 5.96875_real64, 7.087890625_real64, 7.833984375_real64, 8.953125_real64, &
        9.69921875_real64, 10.818359375_real64, 11.9375_real64, 12.68359375_real64, 13.802734375_real64, 14.548828125_real64, &
        15.66796875_real64, 16.4140625_real64, 17.533203125_real64, 18.65234375_real64, 19.3984375_real64, 20.517578125_real64, &
        21.263671875_real64, 22.3828125_real64, 23.501953125_real64]
    !> Fixture G16's outside queries.
    real(real64), parameter :: G16_O(GI_NO) = [-2.984375_real64, -0.373046875_real64, 24.248046875_real64, 26.859375_real64]
    !> method="linear" at each interior query
    real(real64), parameter :: G16_LINEAR(GI_NQ) = [-0.033447265625_real64, 0.08612060546875_real64, -0.1690673828125_real64, &
        -0.128173828125_real64, -0.2580413818359375_real64, -0.1822662353515625_real64, -0.0810546875_real64, &
        -0.11602783203125_real64, -0.13934326171875_real64, -0.056396484375_real64, 0.00189208984375_real64, &
        0.089324951171875_real64, 0.172607421875_real64, 0.1813507080078125_real64, 0.19446563720703125_real64, &
        0.20320892333984375_real64, 0.2163238525390625_real64, 0.204010009765625_real64, 0.17340850830078125_real64, &
        0.1428070068359375_real64, 0.122406005859375_real64, 0.09180450439453125_real64, 0.07140350341796875_real64, &
        0.040802001953125_real64, 0.01020050048828125_real64]
    !> method="linear", outside="extrapolate", at each outside query
    real(real64), parameter :: G16_LINEAR_OUT(GI_NO) = [-14.44140625_real64, -2.03759765625_real64, &
        -0.01020050048828125_real64, -0.08160400390625_real64]
    !> method="cubic", bc="natural" at each interior query
    real(real64), parameter :: G16_NATURAL(GI_NQ) = [-0.033400154660148548_real64, 0.20013952982029015_real64, &
        -0.22486962093010962_real64, -0.10845667721313783_real64, -0.30443862720186299_real64, -0.2486720196682379_real64, &
        -0.069663163399259723_real64, -0.088403800088247275_real64, -0.13888281359342333_real64, -0.12562576606466158_real64, &
        -0.064474370049561844_real64, 0.061840750498454715_real64, 0.17700217662666851_real64, 0.22320424736421968_real64, &
        0.24943384524869863_real64, 0.2461887537399437_real64, 0.22391105055689872_real64, 0.20509529451433431_real64, &
        0.17604076946638389_real64, 0.14615446484206604_real64, 0.12584064823213081_real64, 0.094894232383030949_real64, &
        0.074009586652059256_real64, 0.042410794664631983_real64, 0.010616289835706482_real64]
    !> method="cubic", bc="natural", outside="extrapolate", at each outside query
    real(real64), parameter :: G16_NATURAL_OUT(GI_NO) = [6497.4453263439891_real64, 10.329353523262089_real64, &
        -0.010616289835706482_real64, -0.084473655388822677_real64]

    ! ---- R33: thirty-three knots at irregular gaps from 1/16 to 11/16, ordinates of both signs ----

    !> Knots in fixture R33.
    integer, parameter :: R33_N = 33
    !> Fixture R33's abscissae, strictly increasing.
    real(real64), parameter :: R33_X(R33_N) = [-2.0_real64, -1.625_real64, -1.375_real64, -1.0_real64, -0.9375_real64, &
        -0.25_real64, -0.0625_real64, 0.5625_real64, 1.1875_real64, 1.375_real64, 2.0625_real64, 2.125_real64, 2.5_real64, &
        2.75_real64, 3.125_real64, 3.1875_real64, 3.875_real64, 4.0625_real64, 4.6875_real64, 5.3125_real64, 5.5_real64, &
        6.1875_real64, 6.25_real64, 6.625_real64, 6.875_real64, 7.25_real64, 7.3125_real64, 8.0_real64, 8.1875_real64, &
        8.8125_real64, 9.4375_real64, 9.625_real64, 10.3125_real64]
    !> Fixture R33's ordinates.
    real(real64), parameter :: R33_Y(R33_N) = [-0.13671875_real64, 0.00390625_real64, -0.0703125_real64, 0.18359375_real64, &
        0.171875_real64, 0.05859375_real64, 0.0078125_real64, 0.18359375_real64, -0.0078125_real64, -0.0234375_real64, &
        -0.078125_real64, -0.0078125_real64, -0.02734375_real64, 0.02734375_real64, -0.05859375_real64, -0.12109375_real64, &
        0.00390625_real64, 0.1015625_real64, -0.04296875_real64, 0.11328125_real64, -0.0234375_real64, 0.08984375_real64, &
        -0.140625_real64, -0.171875_real64, 0.16015625_real64, -0.1171875_real64, -0.08203125_real64, 0.05078125_real64, &
        0.06640625_real64, 0.12890625_real64, 0.0234375_real64, -0.0859375_real64, -0.03515625_real64]
    !> Fixture R33's interior queries.
    real(real64), parameter :: R33_Q(GI_NQ) = [-1.8076171875_real64, -1.4228515625_real64, -0.845703125_real64, &
        -0.4609375_real64, 0.1162109375_real64, 0.5009765625_real64, 1.078125_real64, 1.6552734375_real64, 2.0400390625_real64, &
        2.6171875_real64, 3.001953125_real64, 3.5791015625_real64, 4.15625_real64, 4.541015625_real64, 5.1181640625_real64, &
        5.5029296875_real64, 6.080078125_real64, 6.46484375_real64, 7.0419921875_real64, 7.619140625_real64, 8.00390625_real64, &
        8.5810546875_real64, 8.9658203125_real64, 9.54296875_real64, 10.1201171875_real64]
    !> Fixture R33's outside queries.
    real(real64), parameter :: R33_O(GI_NO) = [-3.5390625_real64, -2.1923828125_real64, 10.5048828125_real64, 11.8515625_real64]
    !> method="linear" at each interior query
    real(real64), parameter :: R33_LINEAR(GI_NQ) = [-0.0645751953125_real64, -0.0561065673828125_real64, &
        0.15674937855113635_real64, 0.093350497159090912_real64, 0.058074951171875_real64, 0.166290283203125_real64, &
        0.025683593750000001_real64, -0.045731977982954544_real64, -0.076338334517045456_real64, -0.001708984375_real64, &
        -0.0303955078125_real64, -0.049893465909090912_real64, 0.079882812499999997_real64, -0.00909423828125_real64, &
        0.064697265625_real64, -0.02295476740056818_real64, 0.0721435546875_real64, -0.15852864583333334_real64, &
        0.036651611328125_real64, -0.022793856534090908_real64, 0.051106770833333336_real64, 0.10576171875_real64, &
        0.10303344726562499_real64, -0.0380859375_real64, -0.04936634410511364_real64]
    !> method="linear", outside="extrapolate", at each outside query
    real(real64), parameter :: R33_LINEAR_OUT(GI_NO) = [-0.7138671875_real64, -0.2088623046875_real64, &
        -0.020946155894886364_real64, 0.078524502840909088_real64]
    !> method="cubic", bc="natural" at each interior query
    real(real64), parameter :: R33_NATURAL(GI_NQ) = [-0.020830323200203324_real64, -0.07159188242088485_real64, &
        0.15114239389000764_real64, 0.10274712625703469_real64, 0.031810957199449881_real64, 0.17622269075921651_real64, &
        0.018355747521553847_real64, -0.13624934698815744_real64, -0.10118870608225622_real64, -0.01460256129045524_real64, &
        0.023555229955226402_real64, -0.1791873370250617_real64, 0.10784750983524541_real64, -0.027620736213004426_real64, &
        0.14838473114658493_real64, -0.023833102173429561_real64, 0.34710456872449147_real64, -0.3857104935209183_real64, &
        0.041844897526297664_real64, 0.033657511623474218_real64, 0.050966454097462942_real64, 0.10976516114414003_real64, &
        0.13387919902407708_real64, -0.040011143654548616_real64, -0.098893196364660735_real64]
    !> method="cubic", bc="natural", outside="extrapolate", at each outside query
    real(real64), parameter :: R33_NATURAL_OUT(GI_NO) = [6.8115698416605639_real64, -0.25260717679979666_real64, &
        0.028580696364660735_real64, -1.6459202904891608_real64]

    ! ---- P6: the guide page's example, x**2 at x = 1..6 ----

    !> The page's probes.
    real(real64), parameter :: P6_PROBES(3) = [1.5_real64, 2.5_real64, 5.5_real64]
    !> The natural cubic spline at each probe: NOT the parabola, whose second derivative is not zero at the ends.
    real(real64), parameter :: P6_NATURAL(3) = [2.3421052631578947_real64, 6.2236842105263159_real64, 30.342105263157894_real64]
    !> Where the page extrapolates to.
    real(real64), parameter :: P6_FAR = 99.0_real64
    !> The natural cubic spline's end segment continued to `P6_FAR`.
    real(real64), parameter :: P6_NATURAL_FAR = -337578.4736842105_real64

end module test_interpolate_golden ! GCOVR_EXCL_LINE
