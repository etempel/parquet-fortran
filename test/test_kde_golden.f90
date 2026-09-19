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

    !> the defaults: Silverman's rule, the Gaussian kernel, unbounded
    logical, parameter :: KG_DEFAULT_DEF = .true.
    real(real64), parameter :: KG_DEFAULT_H = 130.69287952567552_real64
    real(real64), parameter :: KG_DEFAULT_PDF(NKX) = [0.00061636225409091949_real64, 0.0008209989241088247_real64,  &
        0.0012131108416934214_real64, 0.00091337406200095374_real64, 0.00091021964936181658_real64,  &
        0.00091374825691546528_real64, 0.00083320405736011052_real64, 0.00046863706385190531_real64,  &
        0.00043970899665899885_real64]
    real(real64), parameter :: KG_DEFAULT_CDF(NKX) = [0.058046600332253603_real64, 0.091119298507939997_real64,  &
        0.27106995522728722_real64, 0.52393126372978205_real64, 0.57960519896458229_real64, 0.59556383359443787_real64,  &
        0.80458100749700245_real64, 0.94664659190200373_real64, 0.95186974609115982_real64]

    !> Scott's rule
    logical, parameter :: KG_SCOTT_DEF = .true.
    real(real64), parameter :: KG_SCOTT_H = 153.92716921912896_real64
    real(real64), parameter :: KG_SCOTT_PDF(NKX) = [0.0006269100922933849_real64, 0.00079152011960188014_real64,  &
        0.001131781042812987_real64, 0.0009457156591543797_real64, 0.00092289573285295689_real64,  &
        0.00091948560503923697_real64, 0.00081412253604538117_real64, 0.00046366677846428972_real64,  &
        0.00043866002119589807_real64]
    real(real64), parameter :: KG_SCOTT_CDF(NKX) = [0.071262478766859233_real64, 0.10391428939477601_real64,  &
        0.27125172257665803_real64, 0.52232257765074697_real64, 0.57944174895300982_real64, 0.59556133519719023_real64,  &
        0.80129998768656818_real64, 0.93896203943550616_real64, 0.94415048985413585_real64]

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
    real(real64), parameter :: KG_REN_LO_PDF(NKX) = [0.0_real64, 0.0013367384489376538_real64, 0.0015647202311552184_real64,  &
        0.00064896971115428238_real64, 0.00084808244863724791_real64, 0.00095746817460533155_real64,  &
        0.0007803708817237325_real64, 0.00050969653009056912_real64, 0.00046814632186509974_real64]
    real(real64), parameter :: KG_REN_LO_CDF(NKX) = [0.0_real64, 0.0052605552770164255_real64, 0.28041801948013723_real64,  &
        0.52415263318999761_real64, 0.56775464335677228_real64, 0.58355226934178106_real64, 0.80473219368621363_real64,  &
        0.96751804423431131_real64, 0.97314640936028252_real64]

    !> renormalised at both bounds, Epanechnikov, h = 60
    logical, parameter :: KG_REN_BOTH_DEF = .true.
    real(real64), parameter :: KG_REN_BOTH_H = 60.0_real64
    real(real64), parameter :: KG_REN_BOTH_PDF(NKX) = [0.0_real64, 0.0012685904724035121_real64, 0.0014967290090745846_real64,  &
        0.00066988926025680342_real64, 0.00085083003132306449_real64, 0.00091362988381794136_real64,  &
        0.00083508033882937643_real64, 0.00078486251257870777_real64, 0.0_real64]
    real(real64), parameter :: KG_REN_BOTH_CDF(NKX) = [0.0_real64, 0.0049815926613440481_real64, 0.27884747419048245_real64,  &
        0.52295556302898205_real64, 0.57054510834049965_real64, 0.58595970583001966_real64, 0.80763269354375633_real64,  &
        0.99882790424600498_real64, 1.0_real64]

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
    real(real64), parameter :: KG_RULE_BOUNDED_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0016910175009328058_real64,  &
        0.0011180402759664446_real64, 0.001129013917835547_real64, 0.0011430746486520644_real64, 0.0011486701238141005_real64,  &
        0.0_real64, 0.0_real64]
    real(real64), parameter :: KG_RULE_BOUNDED_CDF(NKX) = [0.0_real64, 0.0_real64, 0.15230803175483573_real64,  &
        0.49005339890574579_real64, 0.55844915865837241_real64, 0.57832697660315735_real64, 0.85290365143664992_real64,  &
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
    real(real64), parameter :: KG_ADAPT_H = 113.57143640464979_real64
    real(real64), parameter :: KG_ADAPT_PDF(NKX) = [7.8458263944782792e-05_real64, 0.00032313856337138426_real64,  &
        0.0033440863699562944_real64, 9.9938180010162867e-05_real64, 0.00012844613003146899_real64,  &
        0.00015552066484258835_real64, 0.00083557373390661098_real64, 0.00050417914570348335_real64,  &
        0.00046270782753398505_real64]
    real(real64), parameter :: KG_ADAPT_CDF(NKX) = [0.0020574571960979638_real64, 0.010224863139779713_real64,  &
        0.31408941651816336_real64, 0.6689615078192207_real64, 0.67528341112729429_real64, 0.67775923896695178_real64,  &
        0.78934646051685131_real64, 0.95162442559616978_real64, 0.95718328374644368_real64]

    !> the adaptive kernel at alpha = 1 with every bandwidth capped at 120, the B-spline at an explicit bandwidth of 80,
    !! renormalised at a lower bound
    logical, parameter :: KG_ADAPT_CAP_DEF = .true.
    real(real64), parameter :: KG_ADAPT_CAP_H = 80.0_real64
    real(real64), parameter :: KG_ADAPT_CAP_PDF(NKX) = [0.0_real64, 0.0_real64, 0.0076328655127352032_real64,  &
        3.6620969238690797e-05_real64, 0.00010982629842453274_real64, 0.00014241207456504383_real64,  &
        0.00085189938351307724_real64, 0.00051928698465249755_real64, 0.00047641647529906287_real64]
    real(real64), parameter :: KG_ADAPT_CAP_CDF(NKX) = [0.0_real64, 0.0_real64, 0.25107861593961978_real64,  &
        0.6680924893905219_real64, 0.67227788169285618_real64, 0.67447687499390996_real64, 0.78918531936613479_real64,  &
        0.95416711523802578_real64, 0.95989213357644021_real64]

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
    real(real64), parameter :: KG_ADAPT_W_H = 125.45560414391876_real64
    real(real64), parameter :: KG_ADAPT_W_PDF(NKX) = [0.00014354420883252235_real64, 0.00045198858468351304_real64,  &
        0.0030044930836210623_real64, 0.00015125285868088049_real64, 0.00013410449433740173_real64,  &
        0.00015466123701314717_real64, 0.00078121218645840469_real64, 0.00055307029059431653_real64,  &
        0.0005131106717538136_real64]
    real(real64), parameter :: KG_ADAPT_W_CDF(NKX) = [0.0045529534268499462_real64, 0.01719823508602978_real64,  &
        0.31502585750787915_real64, 0.66777131118416722_real64, 0.67559587311928304_real64, 0.67811153596360652_real64,  &
        0.78160500392377685_real64, 0.94341723635839048_real64, 0.949547896433655_real64]

end module test_kde_golden ! GCOVR_EXCL_LINE
