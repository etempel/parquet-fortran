!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_kde_vectors.py
!
!> Golden expectations for `parquet_kde`, derived at 50 decimal digits by `mpmath`.
!!
!! **The oracle deliberately shares no arithmetic with the library.** It sums every kernel of every
!! point over the fixture's exact rationals -- no window, no running weight sum -- and reads the
!! rules' scale from the same model `parquet_stats`' own golden file is generated from.
!!
!! Each case is one configuration of `pf_kde%fit` over the recipe's population. `KG_<name>_DEF`
!! says whether the estimate is defined; `KG_<name>_H` is the bandwidth, and `KG_<name>_PDF` and
!! `KG_<name>_CDF` the density and the distribution function at each of `KG_X`. An undefined case
!! carries zeros, and the test asserts NaN there instead.
!!
!! The population is a recipe rather than a literal -- see `kde_fixture` in test/test_kde.f90,
!! and `KG_PROBE`, which is what reports a recipe that has drifted.
module test_kde_golden
    use iso_fortran_env, only : real64
    implicit none
    public

    !> How many probe points each case is evaluated at.
    integer, parameter :: NKX = 9

    !> The first 8 values `kde_fixture` must produce: the stats oracle's recipe.
    real(real64), parameter :: KG_PROBE(8) = [-468.4931640625_real64, -445.29296875_real64, -406.6259765625_real64,  &
        -352.4921875_real64, -282.8916015625_real64, -197.82421875_real64, -97.2900390625_real64, 18.7109375_real64]

    !> The first 8 values `kde_two_component` must produce: the adaptive cases' recipe.
    real(real64), parameter :: KG_PROBE2(8) = [-281.5_real64, -268.6875_real64, 350.0_real64, -281.4375_real64, -307.0_real64,  &
        286.5625_real64, -268.5_real64, -268.4375_real64]

    !> Where every case is probed.
    real(real64), parameter :: KG_X(NKX) = [-512.0_real64, -466.0_real64, -300.0_real64, -61.25_real64, 0.0_real64,  &
        17.5_real64, 250.0_real64, 458.5_real64, 470.0_real64]

    !> Silverman's rule, the Gaussian kernel, unbounded
    logical, parameter :: KG_SILVERMAN_DEF = .true.
    real(real64), parameter :: KG_SILVERMAN_H = 130.69287952567552_real64
    real(real64), parameter :: KG_SILVERMAN_PDF(NKX) = [0.00061636225409091949_real64, 0.0008209989241088247_real64,  &
        0.0012131108416934214_real64, 0.00091337406200095374_real64, 0.00091021964936181658_real64,  &
        0.00091374825691546528_real64, 0.00083320405736011052_real64, 0.00046863706385190531_real64,  &
        0.00043970899665899885_real64]
    real(real64), parameter :: KG_SILVERMAN_CDF(NKX) = [0.058046600332253603_real64, 0.091119298507939997_real64,  &
        0.27106995522728722_real64, 0.52393126372978205_real64, 0.57960519896458229_real64, 0.59556383359443787_real64,  &
        0.80458100749700245_real64, 0.94664659190200373_real64, 0.95186974609115982_real64]

    !> Scott's rule
    logical, parameter :: KG_SCOTT_DEF = .true.
    real(real64), parameter :: KG_SCOTT_H = 153.81445983212856_real64
    real(real64), parameter :: KG_SCOTT_PDF(NKX) = [0.00062688742901624702_real64, 0.00079166388607323915_real64,  &
        0.0011321427356006026_real64, 0.00094559350903998605_real64, 0.00092283502038093901_real64,  &
        0.00091944931513646458_real64, 0.00081423010207472019_real64, 0.00046368452542471872_real64,  &
        0.00043866176529086564_real64]
    real(real64), parameter :: KG_SCOTT_CDF(NKX) = [0.071199652415947445_real64, 0.1038542144207535_real64,  &
        0.27124709153954768_real64, 0.52233229437789663_real64, 0.57944557694399723_real64, 0.59556431327552295_real64,  &
        0.80131776364498797_real64, 0.93899969160877472_real64, 0.94418825448397758_real64]

    !> Silverman's rule times adjust = 1.5
    logical, parameter :: KG_ADJUST_DEF = .true.
    real(real64), parameter :: KG_ADJUST_H = 196.03931928851327_real64
    real(real64), parameter :: KG_ADJUST_PDF(NKX) = [0.00062379952264182865_real64, 0.000740709712044037_real64,  &
        0.0010165957095251222_real64, 0.00097000502550494724_real64, 0.00093825433914607214_real64,  &
        0.00092966440109059625_real64, 0.0007728338495427518_real64, 0.00045997077714994176_real64,  &
        0.00043970578448641916_real64]
    real(real64), parameter :: KG_ADJUST_CDF(NKX) = [0.093744552766888894_real64, 0.12515664594800976_real64,  &
        0.27515179432961379_real64, 0.5180883180199799_real64, 0.5765140966511848_real64, 0.59285816438429606_real64,  &
        0.79351642268986067_real64, 0.92464199409782577_real64, 0.92981513175198882_real64]

    !> the recipe at n = 1000, Silverman's rule
    logical, parameter :: KG_N1000_DEF = .true.
    real(real64), parameter :: KG_N1000_H = 65.42561316203556_real64
    real(real64), parameter :: KG_N1000_PDF(NKX) = [0.00037455865752562242_real64, 0.00068575548829316743_real64,  &
        0.0010066971332058589_real64, 0.00097451361642633281_real64, 0.00098156984960258865_real64,  &
        0.00098068932853192679_real64, 0.0010146905786722245_real64, 0.00078883559293114168_real64,  &
        0.00071729392753839974_real64]
    real(real64), parameter :: KG_N1000_CDF(NKX) = [0.016026319386280161_real64, 0.040362554738107126_real64,  &
        0.1992126782297737_real64, 0.43381654982993639_real64, 0.49377929935818632_real64, 0.51095114636338224_real64,  &
        0.73729933942937509_real64, 0.94797239028459035_real64, 0.95663886603681103_real64]

    !> the Gaussian kernel at an explicit bandwidth of 60
    logical, parameter :: KG_GAUSS_DEF = .true.
    real(real64), parameter :: KG_GAUSS_H = 60.0_real64
    real(real64), parameter :: KG_GAUSS_PDF(NKX) = [0.00046630255687994481_real64, 0.00089362597375861812_real64,  &
        0.0015259963706953168_real64, 0.00064896873760143532_real64, 0.00084808243571330528_real64,  &
        0.00095746817210739632_real64, 0.0007803708817237325_real64, 0.00050969653009056912_real64,  &
        0.00046814632186509974_real64]
    real(real64), parameter :: KG_GAUSS_CDF(NKX) = [0.017979043595379864_real64, 0.049178437677385625_real64,  &
        0.28165882549131838_real64, 0.52415264730139421_real64, 0.56775464351044636_real64, 0.58355226937325844_real64,  &
        0.80473219368621363_real64, 0.96751804423431131_real64, 0.97314640936028252_real64]

    !> the Epanechnikov kernel at an explicit bandwidth of 60
    logical, parameter :: KG_EPAN_DEF = .true.
    real(real64), parameter :: KG_EPAN_H = 60.0_real64
    real(real64), parameter :: KG_EPAN_PDF(NKX) = [0.00049167105815076237_real64, 0.00085961463979572928_real64,  &
        0.0014741800407065324_real64, 0.00066988926025680342_real64, 0.00085083003132306449_real64,  &
        0.00091362988381794136_real64, 0.00083178483700623322_real64, 0.00046539740207290881_real64,  &
        0.00041022709609271515_real64]
    real(real64), parameter :: KG_EPAN_CDF(NKX) = [0.019736380958745903_real64, 0.049602826978956745_real64,  &
        0.2794035007531101_real64, 0.52295556302898205_real64, 0.57054510834049965_real64, 0.58595970583001966_real64,  &
        0.80758851825225986_real64, 0.96798981020780783_real64, 0.97303449129183561_real64]

    !> the cubic B-spline at an explicit bandwidth of 60
    logical, parameter :: KG_BSPL_DEF = .true.
    real(real64), parameter :: KG_BSPL_H = 60.0_real64
    real(real64), parameter :: KG_BSPL_PDF(NKX) = [0.0004723217970878097_real64, 0.0008838895607935519_real64,  &
        0.0015122016021809003_real64, 0.00066780940421974611_real64, 0.00085239115257457372_real64,  &
        0.00095228052403639578_real64, 0.00078682904376507193_real64, 0.00049799975291173193_real64,  &
        0.00045534024400635477_real64]
    real(real64), parameter :: KG_BSPL_CDF(NKX) = [0.018330057554248019_real64, 0.049384304039233981_real64,  &
        0.28115810470610442_real64, 0.52389715244565194_real64, 0.56838955484034881_real64, 0.5841806153967305_real64,  &
        0.8054486411623154_real64, 0.96762804488994636_real64, 0.9731128245795474_real64]

    !> the box kernel at an explicit bandwidth of 60
    logical, parameter :: KG_BOX_DEF = .true.
    real(real64), parameter :: KG_BOX_H = 60.0_real64
    real(real64), parameter :: KG_BOX_PDF(NKX) = [0.00045105489780439511_real64, 0.00075175816300732524_real64,  &
        0.0013531646934131854_real64, 0.00075175816300732524_real64, 0.00090210979560879023_real64,  &
        0.00090210979560879023_real64, 0.00090210979560879023_real64, 0.00045105489780439511_real64,  &
        0.00030070326520293008_real64]
    real(real64), parameter :: KG_BOX_CDF(NKX) = [0.020923339149730134_real64, 0.049523538452895526_real64,  &
        0.27629776349821084_real64, 0.52238289476769051_real64, 0.57313773086804287_real64, 0.58756317762106625_real64,  &
        0.80974565793362219_real64, 0.96856614203372371_real64, 0.9732003495938969_real64]

    !> weights mod 5, reliability: Kish's n_eff in the rule
    logical, parameter :: KG_WREL_DEF = .true.
    real(real64), parameter :: KG_WREL_H = 128.91006624172007_real64
    real(real64), parameter :: KG_WREL_PDF(NKX) = [0.0005378978586982893_real64, 0.00072911113264030494_real64,  &
        0.0011007559165210142_real64, 0.0010477922230756154_real64, 0.0011587800411476039_real64, 0.0011894113008902852_real64,  &
        0.00090340205449890363_real64, 0.00033276096248897988_real64, 0.0003057522714664343_real64]
    real(real64), parameter :: KG_WREL_CDF(NKX) = [0.048347562184264843_real64, 0.077491652101741443_real64,  &
        0.23969837289815057_real64, 0.48561761199367492_real64, 0.55310395268236712_real64, 0.57365551457600805_real64,  &
        0.841477821619661_real64, 0.96784239587372467_real64, 0.97151309523549623_real64]

    !> weights mod 5, frequency: sum(w) in the rule, the inverted-CDF quartiles
    logical, parameter :: KG_WFREQ_DEF = .true.
    real(real64), parameter :: KG_WFREQ_H = 102.2892608015711_real64
    real(real64), parameter :: KG_WFREQ_PDF(NKX) = [0.00049966722225552147_real64, 0.00075393099580330406_real64,  &
        0.0011911550833948101_real64, 0.00097170012150674069_real64, 0.0011800503052907866_real64, 0.001245106921468964_real64,  &
        0.00087268303289359634_real64, 0.00031655226074198866_real64, 0.00028425169557277512_real64]
    real(real64), parameter :: KG_WFREQ_CDF(NKX) = [0.034086600680720863_real64, 0.062857489689981325_real64,  &
        0.24143644906735839_real64, 0.47936783917145931_real64, 0.54485681139522768_real64, 0.56608287060295359_real64,  &
        0.85165675536602481_real64, 0.97574427653606144_real64, 0.97919796120212632_real64]

    !> renormalised at a lower bound, Gaussian, h = 60
    logical, parameter :: KG_REN_LO_DEF = .true.
    real(real64), parameter :: KG_REN_LO_H = 60.0_real64
    real(real64), parameter :: KG_REN_LO_PDF(NKX) = [0.0_real64, 0.0017083771475601981_real64, 0.0015397288695792358_real64,  &
        0.00065330081957571588_real64, 0.00085374366778749806_real64, 0.00096385959032064346_real64,  &
        0.00078558011667465412_real64, 0.00051309892379988644_real64, 0.00047127135412747522_real64]
    real(real64), parameter :: KG_REN_LO_CDF(NKX) = [0.0_real64, 0.006866534633698713_real64, 0.27680487504607615_real64,  &
        0.52097620809917056_real64, 0.56486926212175936_real64, 0.58077234238671971_real64, 0.8034287161920346_real64,  &
        0.9673012163858975_real64, 0.97296715271937706_real64]

    !> renormalised at both bounds, Epanechnikov, h = 60
    logical, parameter :: KG_REN_BOTH_DEF = .true.
    real(real64), parameter :: KG_REN_BOTH_H = 60.0_real64
    real(real64), parameter :: KG_REN_BOTH_PDF(NKX) = [0.0_real64, 0.0016683751053890152_real64, 0.001494532606409885_real64,  &
        0.00067913776777072451_real64, 0.00086257661154849532_real64, 0.00092624348034312496_real64,  &
        0.00084326847881304281_real64, 0.00092808162645698956_real64, 0.0_real64]
    real(real64), parameter :: KG_REN_BOTH_CDF(NKX) = [0.0_real64, 0.0066482461600199279_real64, 0.27856691610948986_real64,  &
        0.52548146411490748_real64, 0.57372803185313015_real64, 0.58935544365266201_real64, 0.81404406888068259_real64,  &
        0.99860622772340579_real64, 1.0_real64]

    !> reflected at a lower bound, B-spline, h = 60
    logical, parameter :: KG_REF_LO_DEF = .true.
    real(real64), parameter :: KG_REF_LO_H = 60.0_real64
    real(real64), parameter :: KG_REF_LO_PDF(NKX) = [0.0_real64, 0.0016946904296496843_real64, 0.0015146830873777229_real64,  &
        0.00066780940421974611_real64, 0.00085239115257457372_real64, 0.00095228052403639578_real64,  &
        0.00078682904376507193_real64, 0.00049799975291173193_real64, 0.00045534024400635477_real64]
    real(real64), parameter :: KG_REF_LO_CDF(NKX) = [0.0_real64, 0.0067792073168530063_real64, 0.28113726924816429_real64,  &
        0.52389715244565194_real64, 0.56838955484034881_real64, 0.5841806153967305_real64, 0.8054486411623154_real64,  &
        0.96762804488994636_real64, 0.9731128245795474_real64]

    !> reflected at an upper bound, box, h = 60
    logical, parameter :: KG_REF_HI_DEF = .true.
    real(real64), parameter :: KG_REF_HI_H = 60.0_real64
    real(real64), parameter :: KG_REF_HI_PDF(NKX) = [0.00045105489780439511_real64, 0.00075175816300732524_real64,  &
        0.0013531646934131854_real64, 0.00075175816300732524_real64, 0.00090210979560879023_real64,  &
        0.00090210979560879023_real64, 0.00090210979560879023_real64, 0.00090210979560879023_real64, 0.0_real64]
    real(real64), parameter :: KG_REF_HI_CDF(NKX) = [0.020923339149730134_real64, 0.049523538452895526_real64,  &
        0.27629776349821084_real64, 0.52238289476769051_real64, 0.57313773086804287_real64, 0.58756317762106625_real64,  &
        0.80974565793362219_real64, 0.99864683530658682_real64, 1.0_real64]

    !> reflected at both bounds with the kernel wider than the range: the mass normalisation carries the doubly reflected terms
    logical, parameter :: KG_REF_WIDE_DEF = .true.
    real(real64), parameter :: KG_REF_WIDE_H = 400.0_real64
    real(real64), parameter :: KG_REF_WIDE_PDF(NKX) = [0.0_real64, 0.0012020503461468884_real64, 0.0011842821209068685_real64,  &
        0.0010955038286196061_real64, 0.0010692068151005082_real64, 0.0010618460871413217_real64,  &
        0.00098138841076746098_real64, 0.00094514274796579482_real64, 0.0_real64]
    real(real64), parameter :: KG_REF_WIDE_CDF(NKX) = [0.0_real64, 0.0048077331025390786_real64, 0.20358424558912649_real64,  &
        0.47658845822722512_real64, 0.54287999420324484_real64, 0.56152655860787803_real64, 0.79830774554869699_real64,  &
        0.99858239997703624_real64, 1.0_real64]

    !> renormalised at both bounds with the kernel wider than the range, box
    logical, parameter :: KG_REN_WIDE_DEF = .true.
    real(real64), parameter :: KG_REN_WIDE_H = 600.0_real64
    real(real64), parameter :: KG_REN_WIDE_PDF(NKX) = [0.0_real64, 0.0010752688172043011_real64, 0.0010752688172043011_real64,  &
        0.0010752688172043011_real64, 0.0010752688172043011_real64, 0.0010752688172043011_real64, 0.0010752688172043011_real64,  &
        0.0010752688172043011_real64, 0.0_real64]
    real(real64), parameter :: KG_REN_WIDE_CDF(NKX) = [0.0_real64, 0.0043010752688172043_real64, 0.18279569892473119_real64,  &
        0.43951612903225806_real64, 0.5053763440860215_real64, 0.52419354838709675_real64, 0.77419354838709675_real64,  &
        0.99838709677419357_real64, 1.0_real64]

    !> Silverman's rule over the population inside the support: two points are outside [-400, 400] and leave the rule's sample too
    logical, parameter :: KG_RULE_BOUNDED_DEF = .true.
    real(real64), parameter :: KG_RULE_BOUNDED_H = 114.46940783744346_real64
    real(real64), parameter :: KG_RULE_BOUNDED_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0016712474972578318_real64,  &
        0.0010843029800294221_real64, 0.0011107041080110711_real64, 0.0011251906735834244_real64, 0.0010688468964687341_real64,  &
        0.0_real64, 0.0_real64]
    real(real64), parameter :: KG_RULE_BOUNDED_CDF(NKX) = [0.0_real64, 0.0_real64, 0.17668800273112303_real64,  &
        0.49164280659899862_real64, 0.55856237658341035_real64, 0.5781258693003648_real64, 0.84020765344626402_real64,  &
        1.0_real64, 1.0_real64]

    !> one point with an explicit bandwidth: a single bump
    logical, parameter :: KG_ONE_H_DEF = .true.
    real(real64), parameter :: KG_ONE_H_H = 10.0_real64
    real(real64), parameter :: KG_ONE_H_PDF(NKX) = [3.0949248929047378e-06_real64, 0.038673433478222503_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]
    real(real64), parameter :: KG_ONE_H_CDF(NKX) = [6.4990405617347096e-06_real64, 0.59844203564511245_real64, 1.0_real64,  &
        1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64, 1.0_real64]

    !> one point under a rule: no scale, so the estimate is undefined
    logical, parameter :: KG_ONE_RULE_DEF = .false.
    real(real64), parameter :: KG_ONE_RULE_H = 0.0_real64
    real(real64), parameter :: KG_ONE_RULE_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64]
    real(real64), parameter :: KG_ONE_RULE_CDF(NKX) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64]

    !> the adaptive kernel over the two-component recipe: Silverman's rule, the Gaussian, alpha = 0.5
    logical, parameter :: KG_ADAPT_DEF = .true.
    real(real64), parameter :: KG_ADAPT_H = 170.35715460697469_real64
    real(real64), parameter :: KG_ADAPT_PDF(NKX) = [0.00046306412939662591_real64, 0.00082542070315436383_real64,  &
        0.0021439069496393725_real64, 0.00050658865422686158_real64, 0.00035153414655044856_real64,  &
        0.0003423967167950289_real64, 0.00063363966535755526_real64, 0.00047968198641614232_real64,  &
        0.00045814078785030328_real64]
    real(real64), parameter :: KG_ADAPT_CDF(NKX) = [0.026794803464024467_real64, 0.055959116789019485_real64,  &
        0.32169144400683813_real64, 0.65933143231698987_real64, 0.68450969976616582_real64, 0.69056326348054697_real64,  &
        0.80138893844632875_real64, 0.92738650012952695_real64, 0.93277945619419145_real64]

    !> the adaptive kernel at alpha = 1 with every bandwidth capped at 120, the B-spline at an explicit bandwidth of 80,
    !! renormalised at a lower bound
    logical, parameter :: KG_ADAPT_CAP_DEF = .true.
    real(real64), parameter :: KG_ADAPT_CAP_H = 80.0_real64
    real(real64), parameter :: KG_ADAPT_CAP_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0075920457152265624_real64,  &
        3.5822038434574117e-05_real64, 0.0001066649524772942_real64, 0.00013824677774856308_real64,  &
        0.00082677113288129865_real64, 0.00050396971391289717_real64, 0.00046236374462674441_real64]
    real(real64), parameter :: KG_ADAPT_CAP_CDF(NKX) = [0.0_real64, 0.0_real64, 0.30892285057447705_real64,  &
        0.6778700985383872_real64, 0.68194317320386744_real64, 0.68407827166202329_real64, 0.79540366420162534_real64,  &
        0.95551903570864161_real64, 0.96107518469644571_real64]

    !> the adaptive kernel reflected at both bounds, Epanechnikov, h = 60
    logical, parameter :: KG_ADAPT_REF_DEF = .true.
    real(real64), parameter :: KG_ADAPT_REF_H = 60.0_real64
    real(real64), parameter :: KG_ADAPT_REF_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0062267398433207988_real64,  &
        1.2813840857919066e-05_real64, 8.6020545386965088e-05_real64, 0.00011522715238080912_real64,  &
        0.00097594368141979456_real64, 0.00084024012545246799_real64, 0.00083581979190045636_real64]
    real(real64), parameter :: KG_ADAPT_REF_CDF(NKX) = [0.0_real64, 0.0_real64, 0.29793625900539494_real64,  &
        0.66687540440497917_real64, 0.66958647244248659_real64, 0.67135137111005905_real64, 0.78020221977335202_real64,  &
        0.99037200427674033_real64, 1.0_real64]

    !> the adaptive kernel under weights mod 5, reliability: the pilot is weighted too
    logical, parameter :: KG_ADAPT_W_DEF = .true.
    real(real64), parameter :: KG_ADAPT_W_H = 188.18340621587814_real64
    real(real64), parameter :: KG_ADAPT_W_PDF(NKX) = [0.00056218576553675268_real64, 0.00089183922978888926_real64,  &
        0.0019180001994962103_real64, 0.00061796462843146532_real64, 0.00042866943587230479_real64,  &
        0.00040510387134596703_real64, 0.00058700784405582834_real64, 0.00048388299051438355_real64,  &
        0.00046614677021848255_real64]
    real(real64), parameter :: KG_ADAPT_W_CDF(NKX) = [0.039365310251200138_real64, 0.07252429283881151_real64,  &
        0.32265784137731068_real64, 0.65002710203721026_real64, 0.6811756446550804_real64, 0.688453737506587_real64,  &
        0.79718123689080411_real64, 0.91733435382500816_real64, 0.92279766474454661_real64]

    !> the ISJ rule over the two-component recipe, 1024 cells: the Gaussian, unbounded
    logical, parameter :: KG_ISJ_DEF = .true.
    real(real64), parameter :: KG_ISJ_H = 13.212291562086607_real64
    real(real64), parameter :: KG_ISJ_PDF(NKX) = [0.0_real64, 0.0_real64, 0.011333196574679258_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.00054100329135813106_real64, 2.2599567175840048e-05_real64, 1.4384792320993094e-06_real64]
    real(real64), parameter :: KG_ISJ_CDF(NKX) = [0.0_real64, 0.0_real64, 0.25931099153528708_real64,  &
        0.66666666666666663_real64, 0.66666666666666663_real64, 0.66666666666666663_real64, 0.76304995208035642_real64,  &
        0.99990160676547879_real64, 0.99999505762269791_real64]

    !> the ISJ rule under weights mod 5, reliability: Kish's n_eff, the binned mass weighted
    logical, parameter :: KG_ISJ_WREL_DEF = .true.
    real(real64), parameter :: KG_ISJ_WREL_H = 15.530249651505166_real64
    real(real64), parameter :: KG_ISJ_WREL_PDF(NKX) = [0.0_real64, 0.0_real64, 0.010757843790414308_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.00043599971488258679_real64, 3.760744391893859e-05_real64, 4.9545333735976465e-06_real64]
    real(real64), parameter :: KG_ISJ_WREL_CDF(NKX) = [0.0_real64, 0.0_real64, 0.26047507318525287_real64,  &
        0.66666666666666663_real64, 0.66666666666666663_real64, 0.66666666666666663_real64, 0.74853171328654011_real64,  &
        0.99978389613877028_real64, 0.99997721452793054_real64]

    !> the ISJ rule under weights mod 5, frequency: the replicated sample of 120 points at 48 values, whose repeated values the
    !! rule resolves -- its root lies at about 1.2 cells
    logical, parameter :: KG_ISJ_WFREQ_DEF = .true.
    real(real64), parameter :: KG_ISJ_WFREQ_H = 1.1229871836821215_real64
    real(real64), parameter :: KG_ISJ_WFREQ_PDF(NKX) = [0.0_real64, 0.0_real64, 0.012655842640159636_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]
    real(real64), parameter :: KG_ISJ_WFREQ_CDF(NKX) = [0.0_real64, 0.0_real64, 0.26121527229251723_real64,  &
        0.66666666666666663_real64, 0.66666666666666663_real64, 0.66666666666666663_real64, 0.75_real64, 1.0_real64, 1.0_real64]

    !> the ISJ rule on a grid clipped to [-331.625, 470]: the lowest point sits on the lower bound, in the half cell below the
    !! first centre, and lands whole on it
    logical, parameter :: KG_ISJ_BOUNDED_DEF = .true.
    real(real64), parameter :: KG_ISJ_BOUNDED_H = 13.197266419755225_real64
    real(real64), parameter :: KG_ISJ_BOUNDED_PDF(NKX) = [0.0_real64, 0.0_real64, 0.011373587433789061_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.00054065996256343464_real64, 2.2475446160370721e-05_real64, 2.8381170248336531e-06_real64]
    real(real64), parameter :: KG_ISJ_BOUNDED_CDF(NKX) = [0.0_real64, 0.0_real64, 0.25911178516270172_real64,  &
        0.66666666666666663_real64, 0.66666666666666663_real64, 0.66666666666666663_real64, 0.76305071941273173_real64,  &
        0.99990264279550489_real64, 1.0_real64]

    !> the ISJ rule over the recipe rounded to multiples of 8: the fixed point is not negative at one cell, so the rule finds no
    !! bandwidth and the estimate is undefined
    logical, parameter :: KG_ISJ_ROUNDED_DEF = .false.
    real(real64), parameter :: KG_ISJ_ROUNDED_H = 0.0_real64
    real(real64), parameter :: KG_ISJ_ROUNDED_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]
    real(real64), parameter :: KG_ISJ_ROUNDED_CDF(NKX) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]

    !> the ISJ rule over the recipe at n = 32: the fixed point is negative up to t = 1, so the rule finds no bandwidth and the
    !! estimate is undefined
    logical, parameter :: KG_ISJ_NO_ROOT_DEF = .false.
    real(real64), parameter :: KG_ISJ_NO_ROOT_H = 0.0_real64
    real(real64), parameter :: KG_ISJ_NO_ROOT_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]
    real(real64), parameter :: KG_ISJ_NO_ROOT_CDF(NKX) = [0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64,  &
        0.0_real64, 0.0_real64, 0.0_real64, 0.0_real64]

    !> the linear boundary kernel at a lower bound, Gaussian, h = 60
    logical, parameter :: KG_LIN_LO_DEF = .true.
    real(real64), parameter :: KG_LIN_LO_H = 60.0_real64
    real(real64), parameter :: KG_LIN_LO_PDF(NKX) = [0.0_real64, 0.00217115513796063_real64, 0.0015257161719168268_real64,  &
        0.00064639980261425932_real64, 0.0008447253115332321_real64, 0.00095367804579788405_real64,  &
        0.00077728179292040609_real64, 0.00050767890247135395_real64, 0.00046629317024827165_real64]
    real(real64), parameter :: KG_LIN_LO_CDF(NKX) = [0.0_real64, 0.0088683600258894317_real64, 0.28439499584295358_real64,  &
        0.52603628335668484_real64, 0.56946568158515298_real64, 0.58520077273490123_real64, 0.8055051589624187_real64,  &
        0.96764662366777798_real64, 0.97325270897770422_real64]

    !> the linear kernel at an upper bound, box, h = 60: a kernel whose value jumps at its own edges and whose moments kink at the
    !! correction edge
    logical, parameter :: KG_LIN_HI_BOX_DEF = .true.
    real(real64), parameter :: KG_LIN_HI_BOX_H = 60.0_real64
    real(real64), parameter :: KG_LIN_HI_BOX_PDF(NKX) = [0.00044450120964712624_real64, 0.00074083534941187705_real64,  &
        0.0013335036289413788_real64, 0.00074083534941187705_real64, 0.00088900241929425249_real64,  &
        0.00088900241929425249_real64, 0.00088900241929425249_real64, 0.0016990634024326831_real64, 0.0_real64]
    real(real64), parameter :: KG_LIN_HI_BOX_CDF(NKX) = [0.020619329503313272_real64, 0.048803976756426504_real64,  &
        0.2722832424513662_real64, 0.51479283287574862_real64, 0.56481021690564548_real64, 0.57902606637902776_real64,  &
        0.7979803039719835_real64, 0.99743414175468792_real64, 1.0_real64]

    !> the linear kernel at both bounds, Epanechnikov, h = 60
    logical, parameter :: KG_LIN_BOTH_DEF = .true.
    real(real64), parameter :: KG_LIN_BOTH_H = 60.0_real64
    real(real64), parameter :: KG_LIN_BOTH_PDF(NKX) = [0.0_real64, 0.0020504105947631111_real64, 0.0014537141942919402_real64,  &
        0.00066058927630868963_real64, 0.00083901807059563868_real64, 0.00090094608104915731_real64,  &
        0.00082023727819109866_real64, 0.0017690025763010782_real64, 0.0_real64]
    real(real64), parameter :: KG_LIN_BOTH_CDF(NKX) = [0.0_real64, 0.0084565847828891698_real64, 0.27924213668590281_real64,  &
        0.51941299791247408_real64, 0.56634186386435315_real64, 0.58154246253686837_real64, 0.80009443148623927_real64,  &
        0.99730256298730202_real64, 1.0_real64]

    !> the linear kernel at a lower bound, cubic B-spline, h = 60: its kernel's knots and its moments' knots both fall inside the
    !! zone
    logical, parameter :: KG_LIN_BSPL_DEF = .true.
    real(real64), parameter :: KG_LIN_BSPL_H = 60.0_real64
    real(real64), parameter :: KG_LIN_BSPL_PDF(NKX) = [0.0_real64, 0.0021661397194411994_real64, 0.0015080311311130344_real64,  &
        0.00066518739042232557_real64, 0.0008490444171906043_real64, 0.00094854159395056452_real64,  &
        0.00078373972427372139_real64, 0.00049604446115483888_real64, 0.00045355244587898274_real64]
    real(real64), parameter :: KG_LIN_BSPL_CDF(NKX) = [0.0_real64, 0.0088553238209571859_real64, 0.28396665753480882_real64,  &
        0.52576647058850923_real64, 0.57008418288117679_real64, 0.58581324314467087_real64, 0.80621250633695352_real64,  &
        0.96775514659353379_real64, 0.97321839144410582_real64]

    !> the linear kernel at h = 400 on [-470, 460]: the two zones meet, so every moment is two-sided and no part of the support is
    !! the plain sum
    logical, parameter :: KG_LIN_WIDE_DEF = .true.
    real(real64), parameter :: KG_LIN_WIDE_H = 400.0_real64
    real(real64), parameter :: KG_LIN_WIDE_PDF(NKX) = [0.0_real64, 0.0015885560826197653_real64, 0.0013261883025753854_real64,  &
        0.0010551103398812242_real64, 0.0010038701010049395_real64, 0.00099051062374911204_real64,  &
        0.00086414235908504201_real64, 0.00082829655193380515_real64, 0.0_real64]
    real(real64), parameter :: KG_LIN_WIDE_CDF(NKX) = [0.0_real64, 0.0063684668304980017_real64, 0.24740129702829347_real64,  &
        0.52931862672831542_real64, 0.59233896141813902_real64, 0.60978897451345782_real64, 0.82358699283823167_real64,  &
        0.99875754905699055_real64, 1.0_real64]

    !> the linear kernel under weights mod 5, reliability, at a lower bound
    logical, parameter :: KG_LIN_W_DEF = .true.
    real(real64), parameter :: KG_LIN_W_H = 60.0_real64
    real(real64), parameter :: KG_LIN_W_PDF(NKX) = [0.0_real64, 0.0014244223277884492_real64, 0.0013415215813542527_real64,  &
        0.00067440388195386352_real64, 0.0011261597115659651_real64, 0.0013428030948799332_real64,  &
        0.00063302940700476627_real64, 0.00025291470726250656_real64, 0.00020854323330675182_real64]
    real(real64), parameter :: KG_LIN_W_CDF(NKX) = [0.0_real64, 0.0057226223642557307_real64, 0.2481927661528357_real64,  &
        0.46870053738319045_real64, 0.52032400504537946_real64, 0.54192104897783633_real64, 0.86204074729521996_real64,  &
        0.98772714149587548_real64, 0.99037430415880512_real64]

    !> the linear kernel where the clip acts at a probe: the two-component recipe under a lower bound at -470, more than two
    !! bandwidths below its nearest point, so the raw estimate is negative at the probe -466 and the clipped one is exactly zero
    !! there
    logical, parameter :: KG_LIN_ZERO_DEF = .true.
    real(real64), parameter :: KG_LIN_ZERO_H = 60.0_real64
    real(real64), parameter :: KG_LIN_ZERO_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0044282391829142233_real64,  &
        3.8274852270353798e-06_real64, 4.2812735414850598e-06_real64, 9.9336369322400599e-06_real64,  &
        0.0011457637303930459_real64, 0.00042742041860258708_real64, 0.00034196408865721654_real64]
    real(real64), parameter :: KG_LIN_ZERO_CDF(NKX) = [0.0_real64, 0.0_real64, 0.28254026838515484_real64,  &
        0.65410340190434246_real64, 0.65423445317037976_real64, 0.65435254296166789_real64, 0.76416529025993329_real64,  &
        0.98305208667059962_real64, 0.98746743697775996_real64]

    !> both bounds a thousandth of a bandwidth apart (h = 9.3e5 on [-470, 460]), Gaussian: where the library forms the moments
    !! centred on the interval's midpoint and this oracle takes the plain differences at fifty digits
    logical, parameter :: KG_LIN_NARROW3_DEF = .true.
    real(real64), parameter :: KG_LIN_NARROW3_H = 930000.0_real64
    real(real64), parameter :: KG_LIN_NARROW3_PDF(NKX) = [0.0_real64, 0.0014631429275618942_real64,  &
        0.0013234745762466026_real64, 0.0011225961706461511_real64, 0.0010710619240575873_real64, 0.0010563378539084265_real64,  &
        0.00086071807763576848_real64, 0.00068529133085729543_real64, 0.0_real64]
    real(real64), parameter :: KG_LIN_NARROW3_CDF(NKX) = [0.0_real64, 0.0058593027154285899_real64, 0.23714855536296486_real64,  &
        0.52914825027177281_real64, 0.59632902941360699_real64, 0.61494377747061213_real64, 0.83780152904947414_real64,  &
        0.99897300955085677_real64, 1.0_real64]

    !> the adaptive kernel under the linear correction: the two-component recipe at a lower bound, alpha = 0.5, each point
    !! corrected at ITS OWN bandwidth and the pilot the clipped linear estimate
    logical, parameter :: KG_ADAPT_LIN_DEF = .true.
    real(real64), parameter :: KG_ADAPT_LIN_H = 60.0_real64
    real(real64), parameter :: KG_ADAPT_LIN_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0061802404363307849_real64,  &
        9.039997656138826e-06_real64, 4.1669372637916282e-05_real64, 6.0615572348971496e-05_real64,  &
        0.0010589499970346508_real64, 0.00042562772928867181_real64, 0.0003639647804346983_real64]
    real(real64), parameter :: KG_ADAPT_LIN_CDF(NKX) = [0.0_real64, 0.0_real64, 0.29385521582712942_real64,  &
        0.66550120986754502_real64, 0.66684516410733297_real64, 0.6677318247454509_real64, 0.77294027337314009_real64,  &
        0.97457356991418576_real64, 0.97910803725184281_real64]

    !> both bounds a millionth of a bandwidth apart (h = 9.3e8), Epanechnikov: the estimate is the uniform density on the support
    !! to twelve digits
    logical, parameter :: KG_LIN_NARROW6_DEF = .true.
    real(real64), parameter :: KG_LIN_NARROW6_H = 930000000.0_real64
    real(real64), parameter :: KG_LIN_NARROW6_PDF(NKX) = [0.0_real64, 0.0014631429010962531_real64,  &
        0.0013234745758770186_real64, 0.0011225961864427367_real64, 0.0010710619399386554_real64, 0.0010563378695089181_real64,  &
        0.00086071807665669825_real64, 0.0006852912946795545_real64, 0.0_real64]
    real(real64), parameter :: KG_LIN_NARROW6_CDF(NKX) = [0.0_real64, 0.0058593026080100358_real64, 0.23714855319679151_real64,  &
        0.52914825044871205_real64, 0.59632903056914222_real64, 0.61494377890180851_real64, 0.83780153264356116_real64,  &
        0.99897300960536539_real64, 1.0_real64]

    !> binned, the Gaussian kernel at h = 60, unbounded, 32 cells over [-600, 600]
    integer, parameter :: KG_BIN_GAUSS_NC = 32
    real(real64), parameter :: KG_BIN_GAUSS_XMIN = -600.0_real64
    real(real64), parameter :: KG_BIN_GAUSS_XMAX = 600.0_real64
    real(real64), parameter :: KG_BIN_GAUSS_F(KG_BIN_GAUSS_NC) = [8.2277948374215624e-05_real64, 0.00024425175306984418_real64,  &
        0.0005257285218234854_real64, 0.00086396823491576577_real64, 0.0011726422488141824_real64,  &
        0.0014216675251046901_real64, 0.0015828472948823222_real64, 0.0015859601559754636_real64, 0.0014189416676331729_real64,  &
        0.0011939950435487464_real64, 0.001040266521359214_real64, 0.00096376977430033702_real64,  &
        0.00087401528945642325_real64, 0.00074069865006304216_real64, 0.00066585942687261753_real64,  &
        0.00075669679853097968_real64, 0.00096446616118608843_real64, 0.0011269102906994613_real64,  &
        0.001139761695873756_real64, 0.0010157248397407708_real64, 0.00083702098275584009_real64,  &
        0.00072712709310866958_real64, 0.00077133799625650826_real64, 0.00089418114300484069_real64,  &
        0.00093046879973260126_real64, 0.00083305112168892133_real64, 0.00069449959768521632_real64,  &
        0.00058320732482436877_real64, 0.0004656510124864835_real64, 0.00030718131654888369_real64,  &
        0.00015121932541395608_real64, 5.2878638562178995e-05_real64]

    !> binned, the Epanechnikov kernel at h = 60, unbounded: a compact kernel, whose sampled filter differs from the kernel's own
    !! transform by far more than rounding
    integer, parameter :: KG_BIN_EPAN_NC = 32
    real(real64), parameter :: KG_BIN_EPAN_XMIN = -600.0_real64
    real(real64), parameter :: KG_BIN_EPAN_XMAX = 600.0_real64
    real(real64), parameter :: KG_BIN_EPAN_F(KG_BIN_EPAN_NC) = [9.5470962414321786e-05_real64, 0.00029749151672979799_real64,  &
        0.00052565315543831169_real64, 0.00083277938424422798_real64, 0.0011636881510416668_real64,  &
        0.0014602221292162698_real64, 0.0015634219567325035_real64, 0.001507314495400433_real64, 0.0014081157557720057_real64,  &
        0.0012583496798340549_real64, 0.0010924698998917748_real64, 0.00093590677985209232_real64, 0.000793377201140873_real64,  &
        0.00074327433092081531_real64, 0.00074493244385822515_real64, 0.00081643351765422079_real64,  &
        0.00091936722132034631_real64, 0.0010769881431502526_real64, 0.0011291835063807719_real64,  &
        0.00098505852329094511_real64, 0.00084625587628517319_real64, 0.00080170045882936504_real64,  &
        0.00082425397952741702_real64, 0.00083499489876442999_real64, 0.0008405863884379509_real64,  &
        0.00086485107774170275_real64, 0.00075808447195165941_real64, 0.0005690780573593074_real64,  &
        0.00042411869814213565_real64, 0.00029836542038690474_real64, 0.00018921426880411256_real64,  &
        6.5664316152597401e-05_real64]

    !> binned, the box kernel at h = 60, unbounded
    integer, parameter :: KG_BIN_BOX_NC = 32
    real(real64), parameter :: KG_BIN_BOX_XMIN = -600.0_real64
    real(real64), parameter :: KG_BIN_BOX_XMAX = 600.0_real64
    real(real64), parameter :: KG_BIN_BOX_F(KG_BIN_BOX_NC) = [0.0_real64, 0.00030952690972222221_real64,  &
        0.0005572265625_real64, 0.00072666666666666669_real64, 0.0012224479166666666_real64, 0.0015254470486111111_real64,  &
        0.0016521831597222222_real64, 0.0014427734375_real64, 0.0015165755208333333_real64, 0.0011758810763888889_real64,  &
        0.001156953125_real64, 0.0008716232638888889_real64, 0.00083333333333333339_real64, 0.00065357204861111106_real64,  &
        0.00076833767361111113_real64, 0.00079574652777777781_real64, 0.0008850607638888889_real64,  &
        0.0010851215277777777_real64, 0.0012698524305555555_real64, 0.001_real64, 0.00070032552083333329_real64,  &
        0.00085122395833333328_real64, 0.00083358072916666671_real64, 0.00080600260416666667_real64,  &
        0.00083333333333333339_real64, 0.00094197048611111109_real64, 0.00076371527777777776_real64,  &
        0.0005812977430555556_real64, 0.00036066406249999998_real64, 0.00033333333333333332_real64, 0.000212890625_real64,  &
        0.0_real64]

    !> binned, the cubic B-spline at h = 60, unbounded
    integer, parameter :: KG_BIN_BSPL_NC = 32
    real(real64), parameter :: KG_BIN_BSPL_XMIN = -600.0_real64
    real(real64), parameter :: KG_BIN_BSPL_XMAX = 600.0_real64
    real(real64), parameter :: KG_BIN_BSPL_F(KG_BIN_BSPL_NC) = [8.5100530149851545e-05_real64, 0.00025380253992039693_real64,  &
        0.00052756771464254962_real64, 0.00085633936089367087_real64, 0.001173354326911214_real64,  &
        0.0014275066932919283_real64, 0.0015791035164226419_real64, 0.001569772061455695_real64, 0.0014184318080671596_real64,  &
        0.0012065695355194088_real64, 0.0010532811007262265_real64, 0.00095518143158759177_real64,  &
        0.00085889157469031235_real64, 0.00073890923691962373_real64, 0.00068408899429394255_real64,  &
        0.00076866447011503377_real64, 0.00095745084618209151_real64, 0.0011147347768383806_real64,  &
        0.0011342238333000457_real64, 0.0010109932576099496_real64, 0.00083964377799435779_real64,  &
        0.00074506639490691041_real64, 0.00078116709801256045_real64, 0.00088111285907489735_real64,  &
        0.00091329968973343238_real64, 0.00083802871781086001_real64, 0.00070689256645484558_real64,  &
        0.00058304643072325431_real64, 0.00045574541765670839_real64, 0.0003057816816392872_real64,  &
        0.00015807377114842038_real64, 5.5741554566917663e-05_real64]

    !> binned and reflected at both bounds with the range EQUAL to the support and a power-of-two cell count: the unpadded path,
    !! where the cosine basis is the correction
    integer, parameter :: KG_BIN_REF_NC = 32
    real(real64), parameter :: KG_BIN_REF_XMIN = -470.0_real64
    real(real64), parameter :: KG_BIN_REF_XMAX = 460.0_real64
    real(real64), parameter :: KG_BIN_REF_F(KG_BIN_REF_NC) = [0.0016969003681387201_real64, 0.0016684108199121047_real64,  &
        0.0016509925795643006_real64, 0.0016603895382783663_real64, 0.0016546572517040126_real64, 0.0015768721647900451_real64,  &
        0.0014196397119292839_real64, 0.0012376757568228994_real64, 0.0010976455498056604_real64, 0.0010190461943514161_real64,  &
        0.00096523487326738834_real64, 0.00088786711955552886_real64, 0.00077794683847870298_real64,  &
        0.00067993256891782719_real64, 0.00065878090572764109_real64, 0.00074571185232264587_real64,  &
        0.00090843936860964154_real64, 0.0010709071353939921_real64, 0.0011618080681975286_real64,  &
        0.0011503189432226755_real64, 0.0010491647126094506_real64, 0.00090071248379833984_real64,  &
        0.0007666251186892256_real64, 0.000710359273726314_real64, 0.00075712703295699815_real64,  &
        0.00086368608854185019_real64, 0.00094717669909723332_real64, 0.0009581360050326188_real64,  &
        0.00092280612943545343_real64, 0.00090802456495806027_real64, 0.00094422219100699027_real64,  &
        0.00099138424169471881_real64]

    !> binned and reflected with the range INSIDE the support: the padded path, where the images are gathered in explicitly
    integer, parameter :: KG_BIN_REF_IN_NC = 30
    real(real64), parameter :: KG_BIN_REF_IN_XMIN = -400.0_real64
    real(real64), parameter :: KG_BIN_REF_IN_XMAX = 400.0_real64
    real(real64), parameter :: KG_BIN_REF_IN_F(KG_BIN_REF_IN_NC) = [0.0017661518842700683_real64, 0.0016874738500379234_real64,  &
        0.0015239777940987214_real64, 0.0014803601297686405_real64, 0.0014042981825423374_real64, 0.0013198623091037729_real64,  &
        0.0011684843215517742_real64, 0.001075271344582977_real64, 0.00096026725096693734_real64,  &
        0.00082536027869399716_real64, 0.00077506906866806112_real64, 0.00076146711268097658_real64,  &
        0.0006794792238989213_real64, 0.00076562730545015633_real64, 0.00080629839215959819_real64,  &
        0.00090146806996474677_real64, 0.0010162605381188897_real64, 0.0011426026692859317_real64,  &
        0.0011594791164645901_real64, 0.0010651529627076328_real64, 0.00089173485396744367_real64,  &
        0.00084565863671240871_real64, 0.00076981068896008777_real64, 0.0007969536383209512_real64,  &
        0.00083205000854378946_real64, 0.00083723990417367228_real64, 0.00082924250636339631_real64,  &
        0.00090176141239937696_real64, 0.0010171298166816443_real64, 0.0010393368512226168_real64]

    !> binned and renormalised at a lower bound: the per-cell division by the mass a kernel centred there keeps inside the support
    integer, parameter :: KG_BIN_REN_NC = 32
    real(real64), parameter :: KG_BIN_REN_XMIN = -470.0_real64
    real(real64), parameter :: KG_BIN_REN_XMAX = 460.0_real64
    real(real64), parameter :: KG_BIN_REN_F(KG_BIN_REN_NC) = [0.0016572363881470046_real64, 0.0015930042005369261_real64,  &
        0.0015964225885375472_real64, 0.0016368234924538274_real64, 0.0016517041150657732_real64, 0.0015834858813094201_real64,  &
        0.0014290913422319064_real64, 0.0012469873016773714_real64, 0.0011061679286777392_real64, 0.0010270086112434587_real64,  &
        0.00097278414455632338_real64, 0.00089481197820253321_real64, 0.00078403190538693928_real64,  &
        0.00068525097240031304_real64, 0.00066393386180502601_real64, 0.0007515447785474273_real64,  &
        0.00091554514250914818_real64, 0.0010792837252187203_real64, 0.0011708956812320334_real64,  &
        0.0011593166888128625_real64, 0.0010573712341327899_real64, 0.00090775669067765191_real64,  &
        0.00077260891220630793_real64, 0.00071581790471995281_real64, 0.00076244896572207782_real64,  &
        0.00086749457084284122_real64, 0.00094300064017377927_real64, 0.00092909320993669279_real64,  &
        0.00083729434997249612_real64, 0.00072474460137325646_real64, 0.00063200249209680211_real64,  &
        0.00055044361606181751_real64]

    !> binned under the local linear correction at both bounds: the second convolution, against the odd kernel, read back through
    !! the inverse sine transform
    integer, parameter :: KG_BIN_LIN_NC = 32
    real(real64), parameter :: KG_BIN_LIN_XMIN = -470.0_real64
    real(real64), parameter :: KG_BIN_LIN_XMAX = 460.0_real64
    real(real64), parameter :: KG_BIN_LIN_F(KG_BIN_LIN_NC) = [0.0019501378822682986_real64, 0.0016145796019486247_real64,  &
        0.0015565249457566894_real64, 0.0016059413566733168_real64, 0.0016320425134784254_real64, 0.0015658369934039298_real64,  &
        0.0014116565918589087_real64, 0.0012310494444883857_real64, 0.0010918806323464301_real64, 0.001013728081496087_real64,  &
        0.00096020385285974124_real64, 0.00088324004237033704_real64, 0.00077389260560045551_real64,  &
        0.00067638913273485035_real64, 0.00065534770042946727_real64, 0.00074182561053869913_real64,  &
        0.00090370508012880678_real64, 0.0010653261538884249_real64, 0.0011557533608122393_real64,  &
        0.0011443241108647162_real64, 0.0010436970407041619_real64, 0.00089601735053507854_real64,  &
        0.00076261688139902866_real64, 0.00070657349977801764_real64, 0.00075279278656275326_real64,  &
        0.00085759301462025631_real64, 0.00093539455883268877_real64, 0.000927003325215091_real64,  &
        0.00084683859136414454_real64, 0.00078528529189987226_real64, 0.00089210147726622845_real64,  &
        0.001369302638413479_real64]

end module test_kde_golden ! GCOVR_EXCL_LINE
