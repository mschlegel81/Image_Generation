UNIT lightMaps;
INTERFACE
USES myPics,linAlg3d;
CONST REHASH_THRESHOLD=10;
CONST facet:array[0..161] of T_Vec3=(
(-6.351296179E-05, 8.506067395E-01, 5.258024335E-01),( 5.257368684E-01,-4.240453927E-05, 8.506472707E-01),
( 8.506343961E-01, 5.257576704E-01,-8.044831702E-06),(-5.256857276E-01,-1.095232583E-04, 8.506788611E-01),
(-8.507264853E-01, 5.256085992E-01, 1.085004333E-04),( 4.511158841E-05,-8.506553173E-01, 5.257238150E-01),
( 8.506422043E-01,-5.257450342E-01,-1.999544111E-05),(-8.506000638E-01,-5.258131623E-01, 9.655867325E-05),
(-1.355028944E-04, 8.506582379E-01,-5.257191062E-01),( 5.256599188E-01,-2.313764344E-05,-8.506947756E-01),
(-5.258418322E-01,-9.023741586E-05,-8.505823612E-01),(-2.690614019E-05,-8.506829739E-01,-5.256791115E-01),
( 3.089988232E-01, 4.999498725E-01, 8.090549111E-01),( 4.999555647E-01, 8.090297580E-01, 3.090555370E-01),
( 8.090145588E-01, 3.090074062E-01, 5.000098944E-01),(-3.090336025E-01, 4.998963773E-01, 8.090746999E-01),
(-5.000844002E-01, 8.089293242E-01, 3.091100156E-01),(-8.090167046E-01, 3.088815510E-01, 5.000841618E-01),
( 3.090421259E-01,-5.000283122E-01, 8.089898825E-01),( 5.000051856E-01,-8.090187311E-01, 3.090040088E-01),
( 8.090208769E-01,-3.090354502E-01, 4.999822974E-01),(-3.089492023E-01,-5.000537634E-01, 8.090096712E-01),
(-4.999274313E-01,-8.090459704E-01, 3.090585172E-01),(-8.089441657E-01,-3.091161549E-01, 5.000565648E-01),
( 3.089101315E-01, 5.000039935E-01,-8.090553284E-01),( 4.999269545E-01, 8.090588450E-01,-3.090255857E-01),
( 8.089818954E-01, 3.090324402E-01,-5.000472665E-01),(-3.091665208E-01, 4.999505579E-01,-8.089904189E-01),
(-5.001404285E-01, 8.089584112E-01,-3.089430928E-01),(-8.091210127E-01, 3.089066148E-01,-4.998998344E-01),
( 3.089533746E-01,-5.000457764E-01,-8.090129495E-01),( 4.999765456E-01,-8.090338111E-01,-3.090108633E-01),
( 8.089882135E-01,-3.090378344E-01,-5.000337362E-01),(-3.090821207E-01,-5.000712276E-01,-8.089480996E-01),
(-4.999834597E-01,-8.090610504E-01,-3.089283407E-01),(-8.090484738E-01,-3.091185093E-01,-4.998863041E-01),
( 2.324944035E-05,-7.763432222E-05, 1.000000000E+00),(-1.137119398E-04,-5.497220627E-05,-1.000000000E+00),
(-1.090316873E-04, 1.000000000E+00, 5.565234824E-05),( 1.864705337E-05,-1.000000000E+00, 3.293092595E-05),
( 1.000000000E+00,-2.563786438E-06,-2.426392166E-05),(-1.000000000E+00,-1.302140008E-04, 1.127467622E-04),
( 6.786791086E-01, 1.443620920E-01, 7.201071382E-01),( 5.877622366E-01, 4.306871891E-01, 6.848679781E-01),
( 7.200741768E-01, 6.787049174E-01, 1.444049627E-01),( 6.848236322E-01, 5.877758265E-01, 4.307390749E-01),
( 1.443509310E-01, 7.200542688E-01, 6.787375212E-01),( 4.306811690E-01, 6.848231554E-01, 5.878188014E-01),
(-2.337075323E-01, 8.644308448E-01, 4.451293349E-01),(-6.848877072E-01, 5.876520276E-01, 4.308062196E-01),
(-4.307617843E-01, 6.847414970E-01, 5.878549218E-01),(-8.645340800E-01, 4.449142516E-01, 2.337352484E-01),
(-4.450291097E-01, 2.335166782E-01, 8.645340204E-01),(-5.877825618E-01, 4.305892587E-01, 6.849120855E-01),
( 2.336561829E-01,-8.644927144E-01, 4.450361431E-01),( 6.848452687E-01,-5.877850056E-01, 4.306921363E-01),
( 4.307294786E-01,-6.848520041E-01, 5.877498388E-01),( 8.644922376E-01,-4.450606704E-01, 2.336110771E-01),
( 4.450612068E-01,-2.336712331E-01, 8.644757271E-01),( 5.877836347E-01,-4.307357967E-01, 6.848189831E-01),
(-6.786162853E-01,-1.444994658E-01, 7.201387286E-01),(-5.876939893E-01,-4.307879508E-01, 6.848631501E-01),
(-7.200331092E-01,-6.787355542E-01, 1.444658488E-01),(-6.847592592E-01,-5.878360271E-01, 4.307592809E-01),
(-1.443326473E-01,-7.201241255E-01, 6.786673069E-01),(-4.306351840E-01,-6.848802567E-01, 5.877859592E-01),
( 8.644723296E-01, 4.450739324E-01,-2.336596251E-01),( 4.306230843E-01, 6.848720908E-01,-5.878043175E-01),
( 6.847907901E-01, 5.878080130E-01,-4.307474196E-01),( 4.449608624E-01, 2.336203754E-01,-8.645411134E-01),
( 5.877040625E-01, 4.307288527E-01,-6.848917007E-01),( 2.335208654E-01, 8.645230532E-01,-4.450482130E-01),
(-1.445399970E-01, 7.200850844E-01,-6.786646247E-01),(-4.308646917E-01, 6.847904325E-01,-5.877224207E-01),
(-5.879120231E-01, 4.306309223E-01,-6.847747564E-01),(-6.787895560E-01, 1.442922056E-01,-7.200170159E-01),
(-7.202136517E-01, 6.785806417E-01,-1.442934573E-01),(-6.849728227E-01, 5.876842141E-01,-4.306268990E-01),
( 1.443446875E-01,-7.201430202E-01,-6.786447167E-01),( 4.306713641E-01,-6.848742366E-01,-5.877664685E-01),
( 5.877254605E-01,-4.307464063E-01,-6.848622561E-01),( 6.786292195E-01,-1.444145590E-01,-7.201436162E-01),
( 7.200854421E-01,-6.786931753E-01,-1.444040537E-01),( 6.848124266E-01,-5.877976418E-01,-4.307271838E-01),
(-8.644761443E-01,-4.451402128E-01,-2.335192114E-01),(-4.307380915E-01,-6.849025488E-01,-5.876845717E-01),
(-6.848443747E-01,-5.878486633E-01,-4.306066930E-01),(-4.451378584E-01,-2.337168604E-01,-8.644239306E-01),
(-5.878234506E-01,-4.307985306E-01,-6.847453713E-01),(-2.336405814E-01,-8.645281792E-01,-4.449753761E-01),
(-1.444384456E-01, 7.200263739E-01, 6.787484884E-01),(-1.570524424E-01, 2.660997808E-01, 9.510654211E-01),
(-1.993576552E-05, 5.202465057E-01, 8.540161252E-01),(-3.006182909E-01,-9.628840780E-05, 9.537445307E-01),
( 4.450452328E-01, 2.335829288E-01, 8.645077944E-01),( 1.570605189E-01, 2.661236525E-01, 9.510574341E-01),
( 3.006666601E-01,-5.790394425E-05, 9.537292719E-01),( 1.570889354E-01,-2.662484348E-01, 9.510177970E-01),
(-4.449854195E-01,-2.337186038E-01, 8.645019531E-01),(-1.570128798E-01,-2.662646770E-01, 9.510257840E-01),
( 1.444290429E-01,-7.201151252E-01, 6.786563396E-01),( 4.651120253E-05,-5.203626752E-01, 8.539453745E-01),
( 3.005614579E-01,-3.629681669E-05,-9.537624717E-01),( 1.569435000E-01, 2.661676109E-01,-9.510644078E-01),
(-4.451815486E-01, 2.335541397E-01,-8.644453883E-01),(-1.571959257E-01, 2.661437392E-01,-9.510294199E-01),
( 1.442666203E-01, 7.201129198E-01,-6.786932349E-01),(-1.368838712E-04, 5.203052163E-01,-8.539803624E-01),
(-1.444342285E-01,-7.201520205E-01,-6.786161661E-01),(-1.571563631E-01,-2.662655115E-01,-9.510018826E-01),
(-7.046203973E-05,-5.203826427E-01,-8.539332151E-01),(-3.007743359E-01,-7.466932584E-05,-9.536953568E-01),
( 4.449768364E-01,-2.336694896E-01,-8.645195961E-01),( 1.569719017E-01,-2.662492692E-01,-9.510368705E-01),
( 7.200636864E-01, 6.787167788E-01,-1.444015503E-01),( 2.661004961E-01, 9.510678649E-01,-1.570363790E-01),
( 5.202662349E-01, 8.540041447E-01, 1.830912515E-05),(-1.266517356E-04, 9.537429810E-01,-3.006231189E-01),
( 2.335725576E-01, 8.644789457E-01, 4.451067746E-01),( 2.661182880E-01, 9.510511160E-01, 1.571078748E-01),
(-8.548283222E-05, 9.537107944E-01, 3.007253706E-01),(-2.663022280E-01, 9.509941936E-01, 1.571405381E-01),
(-2.337777168E-01, 8.644749522E-01,-4.450067580E-01),(-2.663274705E-01, 9.510110021E-01,-1.569961011E-01),
(-7.201846242E-01, 6.785687804E-01, 1.444941908E-01),(-5.204412341E-01, 8.538974524E-01, 8.959529077E-05),
( 3.629758794E-05,-9.537296295E-01, 3.006655872E-01),( 2.662061453E-01,-9.510333538E-01, 1.570664346E-01),
( 2.336044461E-01,-8.645166159E-01,-4.450168312E-01),( 2.661883533E-01,-9.510430098E-01,-1.570381671E-01),
( 7.200959921E-01,-6.786878109E-01, 1.443766207E-01),( 5.203163028E-01,-8.539736271E-01,-1.098246912E-06),
(-7.200621367E-01,-6.787408590E-01,-1.442959458E-01),(-2.661724687E-01,-9.510540962E-01,-1.569978893E-01),
(-5.202732682E-01,-8.539998531E-01, 7.019580516E-05),(-4.888795047E-06,-9.537481666E-01,-3.006066978E-01),
(-2.335703820E-01,-8.645042777E-01, 4.450587034E-01),(-2.661472261E-01,-9.510444403E-01, 1.570991129E-01),
( 6.786845922E-01,-1.444216371E-01, 7.200899720E-01),( 9.510402083E-01,-1.570722312E-01, 2.661781907E-01),
( 8.539724946E-01,-1.771110328E-05, 5.203181505E-01),( 9.537289739E-01,-3.006677032E-01,-2.452154149E-05),
( 8.644864559E-01, 4.450594187E-01, 2.336350530E-01),( 9.510399699E-01, 1.570581794E-01, 2.661873400E-01),
(-9.537752271E-01, 3.005209565E-01, 1.129823868E-04),(-9.510321617E-01, 1.569232643E-01, 2.662947476E-01),
(-8.644262552E-01,-4.451362491E-01, 2.337112874E-01),(-9.509922862E-01,-1.571801305E-01, 2.662855983E-01),
(-6.786476374E-01, 1.442666352E-01, 7.201558948E-01),(-8.539257050E-01,-1.267290063E-04, 5.203949809E-01),
( 9.537286758E-01, 3.006686866E-01,-1.768706170E-05),( 9.510259032E-01, 1.570679247E-01,-2.662318051E-01),
( 8.644781113E-01,-4.450646341E-01,-2.336558849E-01),( 9.510261416E-01,-1.570699364E-01,-2.662298083E-01),
( 6.786237359E-01, 1.443876326E-01,-7.201541662E-01),( 8.539405465E-01,-5.928182873E-06,-5.203705430E-01),
(-6.787582040E-01,-1.444923878E-01,-7.200064063E-01),(-9.510511756E-01,-1.571778208E-01,-2.660766244E-01),
(-8.540363312E-01,-1.149288364E-04,-5.202133656E-01),(-9.536987543E-01,-3.007634580E-01, 1.061534422E-04),
(-8.645839095E-01, 4.449288249E-01,-2.335229367E-01),(-9.510910511E-01, 1.569330245E-01,-2.660786211E-01));

FACET_ADJACENCY:array[0..161,0..5] of word=(
(46,   48,  90, 118, 120,65535),
(42,   58,  94,  96, 138,65535),
(44,   66, 114, 142, 150,65535),
(52,   60,  93,  98, 148,65535),
(51,   76, 124, 144, 160,65535),
(54,   64, 100, 126, 136,65535),
(57,   82, 130, 141, 152,65535),
(62,   84, 132, 146, 159,65535),
(71,   72, 106, 117, 122,65535),
(69,   81, 102, 112, 154,65535),
(75,   87, 104, 111, 156,65535),
(78,   89, 108, 128, 135,65535),
(94,   43,  47,  46,  92,   95),
(47,   45, 118,  44, 116,  119),
(43,   45, 142,  42, 140,  143),
(90,   50,  53,  92,  91,   52),
(50,   49, 124,  48, 121,  125),
(53,   49, 148,  51, 145,  149),
(100,  56,  59,  58,  97,  101),
(56,   55, 130,  54, 127,  131),
(59,   55, 138, 140, 139,   57),
(98,   61,  65, 101,  99,   64),
(65,   63, 136,  62, 134,  137),
(61,   63, 146, 149, 147,   60),
(67,   70, 106,  69, 103,  107),
(114,  68,  67, 116, 115,   71),
(68,   70, 154,  66, 151,  155),
(73,   74, 104, 107, 105,   72),
(122,  73,  77, 125, 123,   76),
(77,   74, 160,  75, 158,  161),
(79,   80, 112,  78, 110,  113),
(128,  79,  83, 131, 129,   82),
(83,   80, 152, 155, 153,   81),
(85,   88, 108, 110, 109,   87),
(132,  86,  85, 134, 133,   89),
(86,   88, 156, 158, 157,   84),
(95,   91,  96,  97,  99,   93),
(103, 105, 111, 113, 109,  102),
(119, 115, 120, 121, 123,  117),
(127, 129, 135, 137, 133,  126),
(143, 139, 150, 151, 153,  141),
(145, 147, 159, 161, 157,  144),
(94,   43, 138, 140,   1,   14),
(94,   45,  47,  42,  14,   12),
(45,  142, 114, 116,   2,   13),
(47,  142,  14,  43,  44,   13),
(47,  118,  90,  92,   0,   12),
(118,  45,  13,  43,  46,   12),
(90,   50, 120, 121,   0,   16),
(50,   53, 124,  51,  16,   17),
(90,   53,  48,  16,  49,   15),
(124, 144, 145,   4,  49,   17),
(53,  148,  91,  93,   3,   15),
(148,  49,  17,  50,  52,   15),
(100,  56, 126, 127,   5,   19),
(56,   59, 130,  57,  19,   20),
(100,  59,  54,  19,  55,   18),
(130, 139, 141,   6,  55,   20),
(59,  138,  96,  97,   1,   18),
(138,  55,  20,  56,  58,   18),
(98,   61, 149, 148,   3,   23),
(98,   63,  65,  60,  23,   21),
(63,  146, 132, 134,   7,   22),
(65,  146,  23,  61,  62,   22),
(65,  136, 101, 100,   5,   21),
(136,  63,  22,  61,  64,   21),
(114,  68, 150, 151,   2,   26),
(68,   70, 106,  71,  24,   25),
(114,  70,  66,  26,  67,   25),
(70,  154, 102, 103,   9,   24),
(154,  26,  68,  67,  69,   24),
(106, 115, 117,   8,  67,   25),
(122,  73, 107, 106,   8,   27),
(122,  74,  77,  72,  27,   28),
(77,  104,  75,  27,  73,   29),
(104, 156, 158,  10,  74,   29),
(77,  160, 125, 124,   4,   28),
(160,  74,  29,  73,  76,   28),
(128,  79, 108, 110,  11,   30),
(128,  80,  83,  78,  30,   31),
(83,  112,  81,  30,  79,   32),
(112, 155, 154,   9,  80,   32),
(83,  152, 131, 130,   6,   31),
(152,  80,  32,  79,  82,   31),
(132,  86, 157, 159,   7,   35),
(86,   88, 108,  89,  33,   34),
(132,  88,  84,  35,  85,   34),
(88,  156, 109, 111,  10,   33),
(156,  35,  86,  85,  87,   33),
(108, 133, 135,  11,  85,   34),
(92,   48,  50,  15,   0,   46),
(92,   95,  93,  15,  52,   36),
(95,   90,  15,  46,  91,   12),
(99,   98,  52,   3,  91,   36),
(95,   96,  42,  43,  12,    1),
(96,   91,  36,  92,  94,   12),
(97,   36,  95,  94,   1,   58),
(99,  101,  96,  36,  58,   18),
(99,   60,  61,  21,  93,    3),
(101,  36,  97,  93,  98,   21),
(101,  54,  56,  18,  64,    5),
(99,   21,  97,  64, 100,   18),
(103, 113, 112,   9,  69,   37),
(105, 107, 102,  37,  69,   24),
(105, 111,  27,  74,  75,   10),
(107, 111,  37, 103, 104,   27),
(107,  24,  67,  71,  72,    8),
(105,  27, 103,  72, 106,   24),
(110,  33,  85,  89,  11,   78),
(110, 113, 111,  33,  87,   37),
(113, 108,  33,  78, 109,   30),
(37,  105, 104,  87,  10,  109),
(113,  30,  80,  81, 102,    9),
(109,  37, 110, 102, 112,   30),
(116,  66,  68,  25,   2,   44),
(116, 119, 117,  25,  71,   38),
(119, 114,  25,  44, 115,   13),
(123, 122,  71,   8, 115,   38),
(119, 120,  13,  47,  46,    0),
(120, 115,  38, 116, 118,   13),
(121,  38, 119, 118,   0,   48),
(123, 125, 120,  38,  48,   16),
(123,  72,  73,  28, 117,    8),
(125,  38, 121, 117, 122,   28),
(125,  16,  49,  51,  76,    4),
(123,  28, 121,  76, 124,   16),
(127, 137, 136,   5,  54,   39),
(129, 131, 126,  39,  54,   19),
(129, 135,  78,  79,  31,   11),
(131, 135,  39, 127, 128,   31),
(131,  19,  55,  57,  82,    6),
(129,  31, 127,  82, 130,   19),
(134,  84,  86,  34,   7,   62),
(134, 137, 135,  34,  89,   39),
(137, 132,  34,  62, 133,   22),
(39,  129, 128,  89,  11,  133),
(137,  22,  65,  64, 126,    5),
(133,  39, 134, 126, 136,   22),
(140,  20,  59,  58,   1,   42),
(140, 143, 141,  20,  57,   40),
(143, 138,  20,  42, 139,   14),
(153, 152,  57,   6, 139,   40),
(143, 150,  14,  45,  44,    2),
(150, 139,  40, 140, 142,   14),
(145, 161, 160,   4,  51,   41),
(147, 149, 144,  41,  51,   17),
(147, 159,  23,  63,  62,    7),
(149, 159,  41, 145, 146,   23),
(149,  17,  53,  52,  60,    3),
(147,  23, 145,  60, 148,   17),
(151,  40, 143, 142,   2,   66),
(153, 155, 150,  40,  66,   26),
(153,  32,  83,  82, 141,    6),
(155,  40, 151, 141, 152,   32),
(155,  26,  70,  69,  81,    9),
(153,  32, 151,  81, 154,   26),
(158,  35,  88,  87,  10,   75),
(158, 161, 159,  35,  84,   41),
(161, 156,  35,  75, 157,   29),
(41,  147, 146,  84,   7,  157),
(161,  29,  77,  76, 144,    4),
(157,  41, 158, 144, 160,   29));
TYPE
  P_lightMapElement=^T_lightMapElement;
  T_lightMapElement=object
    intPos:longint;
    colorForNormal:array of record
      n:T_Vec3;
      c:T_floatColor;
    end;

    CONSTRUCTOR createLevelless(CONST ip:longint);
    DESTRUCTOR destroy;
    FUNCTION hasEntryForNormal(CONST sampleNormal:T_Vec3; OUT col:T_floatColor):boolean;
    FUNCTION get(CONST sampleNormal:T_Vec3):T_floatColor;
    PROCEDURE setColorForNormal(CONST sampleNormal:T_Vec3; CONST col:T_floatColor);
    FUNCTION getShiftedCenter(CONST samplePoint,sampleNormal,gridCenter:T_Vec3; CONST gridSize:double):T_Vec3;
    FUNCTION getBox(CONST gridCenter:T_Vec3; CONST gridSize:double):T_boundingBox;

    //FUNCTION isValidForPointAndNormal(CONST samplePoint,sampleNormal:T_Vec3):boolean;
    //FUNCTION getTestRay(CONST direction:T_Vec3; CONST gridSize:double):T_ray;
    //PROCEDURE setColor(CONST facetIndex:longint; CONST col:T_floatColor);
    //PROCEDURE addColor(CONST facetIndex:longint; CONST col:T_floatColor);
    //PROCEDURE adaptColor(CONST facetIndex:longint; CONST col:T_floatColor);
    //FUNCTION getTotalIllumination(CONST normal:T_Vec3):T_floatColor;
  end;

  T_elementStatus=(ES_MISSING,ES_NEW,ES_OLD);

  T_lightMap=object
    gridFactor:double;
    centerpoint:T_Vec3;
    data:array of array of P_lightMapElement;
    hashMask:longint;

    CONSTRUCTOR create(CONST gridSize:double; CONST center:T_Vec3);
    DESTRUCTOR destroy;
    PROCEDURE rehashGrowing;
    FUNCTION getCollectorAt(CONST pos:T_Vec3; CONST dither:boolean; OUT status:T_elementStatus):P_lightMapElement;
    PROCEDURE clear;
  end;

FUNCTION getFacetIdx(CONST x:T_Vec3):byte;
IMPLEMENTATION
FUNCTION getFacetIdx(CONST x:T_Vec3):byte;
  CONST FACET_IDX_00:array[0.. 8] of byte=(33,34,35, 84, 85, 86, 87, 88, 89);
        FACET_IDX_01:array[0..12] of byte=(11,30,33, 78, 79, 85, 89,108,109,110,113,128,135);
        FACET_IDX_02:array[0.. 8] of byte=(30,31,32, 79, 80, 83,112,128,152);
        FACET_IDX_04:array[0..12] of byte=(10,29,35, 74, 75, 87, 88,104,111,156,157,158,161);
        FACET_IDX_05:array[0..10] of byte=(37,69,87,102,103,104,105,109,111,112,113);
        FACET_IDX_06:array[0..12] of byte=( 9,26,32, 69, 70, 80, 81,102,112,151,153,154,155);
        FACET_IDX_08:array[0.. 8] of byte=(27,28,29, 73, 74, 77,104,122,160);
        FACET_IDX_09:array[0..12] of byte=( 8,24,27, 67, 71, 72, 73,103,105,106,107,117,122);
        FACET_IDX_10:array[0.. 8] of byte=(24,25,26, 66, 67, 68, 69, 70, 71);
        FACET_IDX_16:array[0..12] of byte=( 7,22,34, 62, 63, 84, 86,132,133,134,137,146,159);
        FACET_IDX_17:array[0..10] of byte=(39,54,89,126,127,128,129,133,135,136,137);
        FACET_IDX_18:array[0..12] of byte=( 6,19,31, 55, 57, 82, 83,127,129,130,131,141,152);
        FACET_IDX_20:array[0..10] of byte=(41,51,84,144,145,146,147,157,159,160,161);
        FACET_IDX_21:array[0.. 0] of byte=(11);
        FACET_IDX_22:array[0..10] of byte=(40,57,66,139,141,142,143,150,151,152,153);
        FACET_IDX_24:array[0..12] of byte=( 4,16,28, 49, 51, 76, 77,121,123,124,125,144,160);
        FACET_IDX_25:array[0..10] of byte=(38,48,71,115,117,118,119,120,121,122,123);
        FACET_IDX_26:array[0..12] of byte=( 2,13,25, 44, 45, 66, 68,114,115,116,119,142,150);
        FACET_IDX_32:array[0.. 8] of byte=(21,22,23, 61, 63, 65, 98,136,146);
        FACET_IDX_33:array[0..12] of byte=( 5,18,21, 54, 56, 64, 65, 97, 99,100,101,126,136);
        FACET_IDX_34:array[0.. 8] of byte=(18,19,20, 54, 55, 56, 57, 58, 59);
        FACET_IDX_36:array[0..12] of byte=( 3,17,23, 52, 53, 60, 61, 93, 98,145,147,148,149);
        FACET_IDX_37:array[0..10] of byte=(36,52,58, 91, 93, 94, 95, 96, 97, 98, 99);
        FACET_IDX_38:array[0..12] of byte=( 1,14,20, 42, 43, 58, 59, 94, 96,138,139,140,143);
        FACET_IDX_40:array[0.. 8] of byte=(15,16,17, 48, 49, 50, 51, 52, 53);
        FACET_IDX_41:array[0..12] of byte=( 0,12,15, 46, 47, 48, 50, 90, 91, 92, 95,118,120);
        FACET_IDX_42:array[0.. 8] of byte=(12,13,14, 43, 45, 47, 94,118,142);

  FUNCTION fDist(CONST k:byte):double; inline;
    begin
      result:=sqr(facet[k,0]-x[0])+
              sqr(facet[k,1]-x[1])+
              sqr(facet[k,2]-x[2]);
    end;
  VAR i:byte;
      d,dMin:double;
  begin

    case byte(((1+round(x[0]*1.499))      )+
              ((1+round(x[1]*1.499)) shl 2)+
              ((1+round(x[2]*1.499)) shl 4)) of
       0: begin result:=FACET_IDX_00[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_00)-1 do begin d:=fDist(FACET_IDX_00[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_00[i]; end; end; end;
       1: begin result:=FACET_IDX_01[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_01)-1 do begin d:=fDist(FACET_IDX_01[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_01[i]; end; end; end;
       2: begin result:=FACET_IDX_02[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_02)-1 do begin d:=fDist(FACET_IDX_02[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_02[i]; end; end; end;
       4: begin result:=FACET_IDX_04[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_04)-1 do begin d:=fDist(FACET_IDX_04[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_04[i]; end; end; end;
       5: begin result:=FACET_IDX_05[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_05)-1 do begin d:=fDist(FACET_IDX_05[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_05[i]; end; end; end;
       6: begin result:=FACET_IDX_06[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_06)-1 do begin d:=fDist(FACET_IDX_06[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_06[i]; end; end; end;
       8: begin result:=FACET_IDX_08[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_08)-1 do begin d:=fDist(FACET_IDX_08[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_08[i]; end; end; end;
       9: begin result:=FACET_IDX_09[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_09)-1 do begin d:=fDist(FACET_IDX_09[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_09[i]; end; end; end;
      10: begin result:=FACET_IDX_10[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_10)-1 do begin d:=fDist(FACET_IDX_10[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_10[i]; end; end; end;
      16: begin result:=FACET_IDX_16[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_16)-1 do begin d:=fDist(FACET_IDX_16[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_16[i]; end; end; end;
      17: begin result:=FACET_IDX_17[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_17)-1 do begin d:=fDist(FACET_IDX_17[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_17[i]; end; end; end;
      18: begin result:=FACET_IDX_18[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_18)-1 do begin d:=fDist(FACET_IDX_18[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_18[i]; end; end; end;
      20: begin result:=FACET_IDX_20[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_20)-1 do begin d:=fDist(FACET_IDX_20[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_20[i]; end; end; end;
      21: begin result:=FACET_IDX_21[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_21)-1 do begin d:=fDist(FACET_IDX_21[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_21[i]; end; end; end;
      22: begin result:=FACET_IDX_22[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_22)-1 do begin d:=fDist(FACET_IDX_22[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_22[i]; end; end; end;
      24: begin result:=FACET_IDX_24[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_24)-1 do begin d:=fDist(FACET_IDX_24[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_24[i]; end; end; end;
      25: begin result:=FACET_IDX_25[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_25)-1 do begin d:=fDist(FACET_IDX_25[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_25[i]; end; end; end;
      26: begin result:=FACET_IDX_26[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_26)-1 do begin d:=fDist(FACET_IDX_26[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_26[i]; end; end; end;
      32: begin result:=FACET_IDX_32[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_32)-1 do begin d:=fDist(FACET_IDX_32[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_32[i]; end; end; end;
      33: begin result:=FACET_IDX_33[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_33)-1 do begin d:=fDist(FACET_IDX_33[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_33[i]; end; end; end;
      34: begin result:=FACET_IDX_34[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_34)-1 do begin d:=fDist(FACET_IDX_34[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_34[i]; end; end; end;
      36: begin result:=FACET_IDX_36[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_36)-1 do begin d:=fDist(FACET_IDX_36[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_36[i]; end; end; end;
      37: begin result:=FACET_IDX_37[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_37)-1 do begin d:=fDist(FACET_IDX_37[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_37[i]; end; end; end;
      38: begin result:=FACET_IDX_38[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_38)-1 do begin d:=fDist(FACET_IDX_38[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_38[i]; end; end; end;
      40: begin result:=FACET_IDX_40[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_40)-1 do begin d:=fDist(FACET_IDX_40[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_40[i]; end; end; end;
      41: begin result:=FACET_IDX_41[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_41)-1 do begin d:=fDist(FACET_IDX_41[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_41[i]; end; end; end;
      42: begin result:=FACET_IDX_42[0]; dMin:=fDist(result); for i:=1 to length(FACET_IDX_42)-1 do begin d:=fDist(FACET_IDX_42[i]); if d<dMin then begin dMin:=d; result:=FACET_IDX_42[i]; end; end; end;
      else result:=0;
    end;
  end;

CONSTRUCTOR T_lightMapElement.createLevelless(CONST ip:longint);
  begin
    setLength(colorForNormal,0);
    intPos:=ip;
  end;

DESTRUCTOR T_lightMapElement.destroy;
  begin
    setLength(colorForNormal,0);
  end;

FUNCTION T_lightMapElement.hasEntryForNormal(CONST sampleNormal:T_Vec3; OUT col:T_floatColor):boolean;
  VAR w,wTot:double;
      i:longint;
  begin
    result:=false;
    wTot:=0;
    col:=black;
    for i:=0 to length(colorForNormal)-1 do with colorForNormal[i] do begin
      w:=sampleNormal*n;
      if w>0 then begin
        w:=w*w; w:=w*w; w:=w*w;
        result:=result or (w>0.9);
        wTot:=wTot+w;
        col :=col +w*c;
      end;
    end;
    if result then col:=col*(1/wTot);
  end;

FUNCTION T_lightMapElement.get(CONST sampleNormal:T_Vec3):T_floatColor;
  VAR w,wTot:double;
      i:longint;
  begin
    wTot:=0;
    result:=black;
    for i:=0 to length(colorForNormal)-1 do with colorForNormal[i] do begin
      w:=sampleNormal*n;
      if w>0 then begin
        wTot  :=wTot  +w;
        result:=result+w*c;
      end;
    end;
    result:=result*(1/wTot);
  end;

PROCEDURE T_lightMapElement.setColorForNormal(CONST sampleNormal:T_Vec3; CONST col:T_floatColor);
  VAR i:longint;
  begin
    i:=length(colorForNormal);
    setLength(colorForNormal,i+1);
    with colorForNormal[i] do begin
      n:=sampleNormal;
      c:=col;
    end;
  end;

FUNCTION T_lightMapElement.getShiftedCenter(CONST samplePoint,sampleNormal,gridCenter:T_Vec3; CONST gridSize:double):T_Vec3;
  VAR ix,iy,iz:longint;
  begin
    ix:=intPos;
    iy:=ix div 1289;
    iz:=iy div 1289;
    result:=gridCenter+gridSize*newVector(ix mod 1289-644.5,
                                          iy mod 1289-644.5,
                                          iz         -644.5)-samplePoint;
    result:=result-sampleNormal*(sampleNormal*result)       +samplePoint;
  end;

FUNCTION T_lightMapElement.getBox(CONST gridCenter:T_Vec3; CONST gridSize:double):T_boundingBox;
  VAR ix,iy,iz:longint;
      boxCenter,boxSize:T_Vec3;
  begin
    ix:=intPos;
    iy:=ix div 1289;
    iz:=iy div 1289;
    boxCenter:=gridCenter+gridSize*newVector(ix mod 1289-644.5,
                                             iy mod 1289-644.5,
                                             iz         -644.5);
    boxSize[0]:=0.5*gridSize;
    boxSize[1]:=boxSize[0];
    boxSize[2]:=boxSize[0];
    result.create(boxCenter-boxSize,
                  boxCenter+boxSize);
  end;


PROCEDURE T_lightMap.rehashGrowing;
  VAR hk,i,j,newLength:longint;
      tmp:array of P_lightMapElement;
  begin
    hk:=0;
    for i:=0 to length(data)-1 do inc(hk,length(data));
    newLength:=length(data);
    while hk/newLength>REHASH_THRESHOLD do inc(newLength,newLength);
    if newLength>length(data) then begin
      setLength(tmp,hk);
      hk:=0;
      for i:=0 to length(data)-1 do begin
        for j:=0 to length(data[i])-1 do begin
          tmp[hk]:=data[i,j]; inc(hk);
        end;
        setLength(data[i],0);
      end;
      setLength(data,newLength);
      hashMask:=newLength-1;
      for hk:=0 to length(tmp)-1 do begin
        i:=tmp[hk]^.intPos and hashMask;
        j:=length(data[i]);
        setLength(data[i],j+1);
        data[i,j]:=tmp[hk];
      end;
      setLength(tmp,0);
    end;
  end;

CONSTRUCTOR T_lightMap.create(CONST gridSize:double; CONST center:T_Vec3);
  begin
    gridFactor:=1/gridSize;
    centerpoint:=center;
    setLength(data,1);
    setLength(data[0],0);
    hashMask:=0;
  end;

FUNCTION T_lightMap.getCollectorAt(CONST pos:T_Vec3; CONST dither:boolean; OUT status:T_elementStatus):P_lightMapElement;
  VAR intPos,hk,i,j:longint;
  begin
    status:=ES_MISSING;
    if dither then begin
      intPos:=round((pos[0]-centerpoint[0])*gridFactor+644.5-random); if (intPos<0) or (intPos>=1289) then exit(nil);
      hk:=    round((pos[1]-centerpoint[1])*gridFactor+644.5-random); if (hk    <0) or (hk    >=1289) then exit(nil); inc(intPos,hk*1289);
      hk:=    round((pos[2]-centerpoint[2])*gridFactor+644.5-random); if (hk    <0) or (hk    >=1289) then exit(nil); inc(intPos,hk*1289*1289);
    end else begin
      intPos:=round((pos[0]-centerpoint[0])*gridFactor+644); if (intPos<0) or (intPos>=1289) then exit(nil);
      hk:=    round((pos[1]-centerpoint[1])*gridFactor+644); if (hk    <0) or (hk    >=1289) then exit(nil); inc(intPos,hk*1289);
      hk:=    round((pos[2]-centerpoint[2])*gridFactor+644); if (hk    <0) or (hk    >=1289) then exit(nil); inc(intPos,hk*1289*1289);
    end;
    i:=intPos and hashMask;
    status:=ES_OLD;
    for j:=0 to length(data[i])-1 do if data[i,j]^.intPos=intPos then exit(data[i,j]);
    status:=ES_NEW;
    new(result,createLevelless(intPos));
    j:=length(data[i]);
    setLength(data[i],j+1);
    data[i,j]:=result;
    if j>=REHASH_THRESHOLD then rehashGrowing;
  end;

DESTRUCTOR T_lightMap.destroy;
  VAR i,j:longint;
  begin
    for i:=0 to length(data)-1 do begin
      for j:=0 to length(data[i])-1 do if data[i,j]<>nil then dispose(data[i,j],destroy);
      setLength(data[i],0);
    end;
    setLength(data,0);
    hashMask:=-1;
  end;

PROCEDURE T_lightMap.clear;
  VAR i,j:longint;
  begin
    for i:=0 to length(data)-1 do begin
      for j:=0 to length(data[i])-1 do if data[i,j]<>nil then dispose(data[i,j],destroy);
      setLength(data[i],0);
    end;
  end;

end.
