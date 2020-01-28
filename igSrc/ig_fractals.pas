UNIT ig_fractals;
INTERFACE
USES myColors,
     myParams,
     complex,
     mypics,
     imageContexts,
     imageGeneration;

CONST LIGHT_NORMAL_INDEX=10;
CONST JULIA_COORD_INDEX=12;
TYPE
  P_functionPerPixelViaRawDataAlgorithm=^T_functionPerPixelViaRawDataAlgorithm;
  T_functionPerPixelViaRawDataAlgorithm=object(T_functionPerPixelAlgorithm)
    temporaryRawMap:P_rawImage;
    rawMapIsOutdated:longint;

    maxDepth    :longint;
    colorSource :byte;
    colorStyle  :byte;
    colorVariant:byte;
    pseudoGamma :double;
    lightNormal :T_rgbFloatColor;

    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    PROCEDURE cleanup; virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION lightIsRelevant:boolean;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; abstract;
    FUNCTION getColor(CONST rawData:T_rgbFloatColor):T_rgbFloatColor;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_rgbFloatColor; virtual;
    PROCEDURE prepareRawMap(VAR target: T_rawImage; CONST my:longint); virtual;
    PROCEDURE execute(CONST context:P_abstractWorkflow); virtual;
  end;

  P_functionPerPixelViaRawDataJuliaAlgorithm=^T_functionPerPixelViaRawDataJuliaAlgorithm;
  T_functionPerPixelViaRawDataJuliaAlgorithm=object(T_functionPerPixelViaRawDataAlgorithm)
    julianess:double;
    juliaParam:T_Complex;
    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
  end;

FUNCTION toSphere(CONST x:T_Complex):T_rgbFloatColor; inline;
IMPLEMENTATION
USES math,mySys,sysutils,myGenerics,linAlg3d;
PROCEDURE getTriangleBase(OUT a,b,c:T_Complex);
  CONST sc:array[0..299] of T_Complex=((re:1.0;im:0.0),
(re:0.9997806834748455;im:0.020942419883356957),
(re:0.99912283009885838;im:0.04187565372919962),
(re:0.99802672842827156;im:0.06279051952931336),
(re:0.99649285924950437;im:0.08367784333231548),
(re:0.9945218953682734;im:0.10452846326765346),
(re:0.99211470131447788;im:0.12533323356430423),
(re:0.9892723329629883;im:0.14608302856241159),
(re:0.985996037070505;im:0.16676874671610226),
(re:0.9822872507286887;im:0.1873813145857246),
(re:0.97814760073380558;im:0.20791169081775931),
(re:0.97357890287316029;im:0.22835087011065572),
(re:0.96858316112863119;im:0.24868988716485474),
(re:0.9631625667976581;im:0.2689198206152657),
(re:0.9573194975320672;im:0.28903179694447156),
(re:0.9510565162951536;im:0.3090169943749474),
(re:0.9443763702374811;im:0.32886664673858323),
(re:0.93728198949189157;im:0.34857204732181518),
(re:0.92977648588825146;im:0.3681245526846779),
(re:0.9218631515885005;im:0.38751558645210293),
(re:0.91354545764260087;im:0.40673664307580015),
(re:0.90482705246601958;im:0.4257792915650726),
(re:0.8957117602394129;im:0.44463517918492745),
(re:0.8862035792312148;im:0.46329603511986167),
(re:0.87630668004386358;im:0.4817536741017152),
(re:0.8660254037844387;im:0.49999999999999994),
(re:0.8553642601605066;im:0.51802700937313018),
(re:0.84432792550201519;im:0.5358267949789965),
(re:0.8329212407100995;im:0.553391549243344),
(re:0.8211492091337041;im:0.5707135676844316),
(re:0.8090169943749473;im:0.5877852522924731),
(re:0.79652991802419637;im:0.6045991148623748),
(re:0.7836934573258398;im:0.6211477802783103),
(re:0.7705132427757892;im:0.6374239897486896),
(re:0.7569950556517564;im:0.65342060399010538),
(re:0.7431448254773943;im:0.6691306063588581),
(re:0.7289686274214115;im:0.6845471059286886),
(re:0.71447267963280336;im:0.6996633405133654),
(re:0.6996633405133655;im:0.7144726796328032),
(re:0.6845471059286887;im:0.7289686274214114),
(re:0.6691306063588583;im:0.7431448254773942),
(re:0.6534206039901056;im:0.75699505565175629),
(re:0.63742398974868986;im:0.7705132427757891),
(re:0.6211477802783105;im:0.7836934573258397),
(re:0.6045991148623749;im:0.79652991802419626),
(re:0.5877852522924732;im:0.8090169943749473),
(re:0.5707135676844318;im:0.8211492091337039),
(re:0.5533915492433442;im:0.8329212407100993),
(re:0.53582679497899677;im:0.844327925502015),
(re:0.5180270093731304;im:0.85536426016050648),
(re:0.5000000000000001;im:0.8660254037844386),
(re:0.48175367410171538;im:0.87630668004386347),
(re:0.46329603511986178;im:0.8862035792312147),
(re:0.4446351791849275;im:0.8957117602394129),
(re:0.4257792915650728;im:0.9048270524660193),
(re:0.40673664307580037;im:0.91354545764260076),
(re:0.3875155864521031;im:0.9218631515885004),
(re:0.36812455268467809;im:0.9297764858882513),
(re:0.34857204732181529;im:0.9372819894918914),
(re:0.32886664673858329;im:0.944376370237481),
(re:0.30901699437494745;im:0.9510565162951535),
(re:0.28903179694447184;im:0.9573194975320671),
(re:0.2689198206152659;im:0.9631625667976581),
(re:0.24868988716485496;im:0.96858316112863108),
(re:0.22835087011065588;im:0.97357890287316029),
(re:0.20791169081775945;im:0.97814760073380558),
(re:0.1873813145857247;im:0.9822872507286887),
(re:0.16676874671610234;im:0.985996037070505),
(re:0.14608302856241187;im:0.9892723329629882),
(re:0.12533323356430448;im:0.99211470131447776),
(re:0.10452846326765367;im:0.99452189536827329),
(re:0.08367784333231566;im:0.99649285924950437),
(re:0.06279051952931351;im:0.99802672842827156),
(re:0.041875653729199748;im:0.99912283009885838),
(re:0.02094241988335705;im:0.9997806834748455),
(re:2.83276944882399E-16;im:1.0),
(re:-0.020942419883356704;im:0.9997806834748455),
(re:-0.0418756537291994;im:0.99912283009885838),
(re:-0.06279051952931318;im:0.99802672842827156),
(re:-0.08367784333231533;im:0.99649285924950437),
(re:-0.10452846326765333;im:0.9945218953682734),
(re:-0.12533323356430415;im:0.99211470131447788),
(re:-0.14608302856241132;im:0.9892723329629883),
(re:-0.166768746716102;im:0.985996037070505),
(re:-0.1873813145857244;im:0.9822872507286888),
(re:-0.20791169081775912;im:0.97814760073380569),
(re:-0.22835087011065555;im:0.97357890287316029),
(re:-0.24868988716485463;im:0.96858316112863119),
(re:-0.26891982061526559;im:0.9631625667976581),
(re:-0.28903179694447129;im:0.9573194975320673),
(re:-0.30901699437494717;im:0.9510565162951536),
(re:-0.328866646738583;im:0.9443763702374811),
(re:-0.348572047321815;im:0.93728198949189157),
(re:-0.36812455268467775;im:0.92977648588825146),
(re:-0.3875155864521028;im:0.9218631515885005),
(re:-0.4067366430758001;im:0.91354545764260087),
(re:-0.4257792915650723;im:0.90482705246601969),
(re:-0.44463517918492723;im:0.895711760239413),
(re:-0.4632960351198615;im:0.8862035792312148),
(re:-0.481753674101715;im:0.87630668004386369),
(re:-0.49999999999999978;im:0.8660254037844388),
(re:-0.51802700937313;im:0.8553642601605068),
(re:-0.5358267949789964;im:0.84432792550201519),
(re:-0.55339154924334388;im:0.8329212407100995),
(re:-0.5707135676844316;im:0.8211492091337041),
(re:-0.587785252292473;im:0.8090169943749474),
(re:-0.6045991148623748;im:0.79652991802419637),
(re:-0.62114778027831;im:0.78369345732584017),
(re:-0.6374239897486893;im:0.77051324277578948),
(re:-0.653420603990105;im:0.7569950556517566),
(re:-0.6691306063588579;im:0.74314482547739447),
(re:-0.68454710592868839;im:0.72896862742141177),
(re:-0.6996633405133652;im:0.7144726796328037),
(re:-0.7144726796328031;im:0.6996633405133656),
(re:-0.7289686274214113;im:0.6845471059286888),
(re:-0.7431448254773941;im:0.6691306063588583),
(re:-0.75699505565175629;im:0.6534206039901056),
(re:-0.7705132427757891;im:0.63742398974868986),
(re:-0.7836934573258397;im:0.6211477802783105),
(re:-0.79652991802419626;im:0.6045991148623749),
(re:-0.8090169943749474;im:0.5877852522924732),
(re:-0.8211492091337037;im:0.5707135676844321),
(re:-0.8329212407100992;im:0.5533915492433444),
(re:-0.8443279255020148;im:0.535826794978997),
(re:-0.85536426016050637;im:0.5180270093731306),
(re:-0.86602540378443849;im:0.5000000000000003),
(re:-0.87630668004386336;im:0.4817536741017156),
(re:-0.8862035792312146;im:0.463296035119862),
(re:-0.89571176023941268;im:0.44463517918492773),
(re:-0.9048270524660193;im:0.42577929156507288),
(re:-0.91354545764260076;im:0.40673664307580043),
(re:-0.9218631515885004;im:0.38751558645210316),
(re:-0.9297764858882513;im:0.36812455268467814),
(re:-0.9372819894918914;im:0.34857204732181535),
(re:-0.944376370237481;im:0.32886664673858335),
(re:-0.9510565162951535;im:0.3090169943749475),
(re:-0.957319497532067;im:0.28903179694447206),
(re:-0.963162566797658;im:0.26891982061526615),
(re:-0.968583161128631;im:0.24868988716485524),
(re:-0.97357890287316018;im:0.22835087011065616),
(re:-0.97814760073380558;im:0.20791169081775973),
(re:-0.9822872507286886;im:0.187381314585725),
(re:-0.98599603707050487;im:0.16676874671610262),
(re:-0.9892723329629882;im:0.14608302856241193),
(re:-0.99211470131447776;im:0.12533323356430454),
(re:-0.99452189536827329;im:0.10452846326765373),
(re:-0.99649285924950437;im:0.08367784333231573),
(re:-0.99802672842827156;im:0.06279051952931358),
(re:-0.99912283009885838;im:0.04187565372919981),
(re:-0.9997806834748455;im:0.02094241988335711),
(re:-1.0;im:5.66553889764798E-16),
(re:-0.9997806834748455;im:-0.020942419883356423),
(re:-0.99912283009885838;im:-0.041875653729199117),
(re:-0.99802672842827156;im:-0.062790519529312888),
(re:-0.99649285924950437;im:-0.083677843332315038),
(re:-0.9945218953682734;im:-0.10452846326765305),
(re:-0.99211470131447788;im:-0.12533323356430387),
(re:-0.9892723329629883;im:-0.14608302856241126),
(re:-0.985996037070505;im:-0.16676874671610195),
(re:-0.9822872507286888;im:-0.18738131458572435),
(re:-0.97814760073380569;im:-0.20791169081775907),
(re:-0.97357890287316029;im:-0.22835087011065552),
(re:-0.96858316112863119;im:-0.24868988716485457),
(re:-0.9631625667976582;im:-0.26891982061526554),
(re:-0.9573194975320673;im:-0.289031796944471),
(re:-0.9510565162951537;im:-0.3090169943749469),
(re:-0.9443763702374812;im:-0.32886664673858274),
(re:-0.93728198949189168;im:-0.34857204732181474),
(re:-0.92977648588825157;im:-0.36812455268467753),
(re:-0.9218631515885006;im:-0.38751558645210255),
(re:-0.91354545764260109;im:-0.4067366430757998),
(re:-0.90482705246601969;im:-0.42577929156507227),
(re:-0.895711760239413;im:-0.44463517918492718),
(re:-0.886203579231215;im:-0.46329603511986145),
(re:-0.87630668004386369;im:-0.481753674101715),
(re:-0.8660254037844388;im:-0.49999999999999978),
(re:-0.8553642601605068;im:-0.51802700937313),
(re:-0.84432792550201519;im:-0.5358267949789964),
(re:-0.83292124071009976;im:-0.5533915492433435),
(re:-0.82114920913370437;im:-0.57071356768443116),
(re:-0.80901699437494778;im:-0.58778525229247269),
(re:-0.79652991802419659;im:-0.60459911486237439),
(re:-0.78369345732584017;im:-0.62114778027831),
(re:-0.77051324277578948;im:-0.6374239897486893),
(re:-0.7569950556517566;im:-0.653420603990105),
(re:-0.74314482547739447;im:-0.6691306063588579),
(re:-0.72896862742141177;im:-0.68454710592868839),
(re:-0.7144726796328037;im:-0.6996633405133652),
(re:-0.6996633405133656;im:-0.7144726796328031),
(re:-0.684547105928689;im:-0.7289686274214113),
(re:-0.66913060635885846;im:-0.743144825477394),
(re:-0.6534206039901057;im:-0.75699505565175629),
(re:-0.6374239897486903;im:-0.7705132427757888),
(re:-0.6211477802783105;im:-0.7836934573258396),
(re:-0.60459911486237539;im:-0.7965299180241958),
(re:-0.5877852522924732;im:-0.8090169943749473),
(re:-0.5707135676844321;im:-0.8211492091337037),
(re:-0.5533915492433442;im:-0.8329212407100993),
(re:-0.5358267949789971;im:-0.8443279255020147),
(re:-0.51802700937313029;im:-0.8553642601605066),
(re:-0.5000000000000004;im:-0.86602540378443837),
(re:-0.4817536741017161;im:-0.8763066800438631),
(re:-0.46329603511986206;im:-0.8862035792312146),
(re:-0.44463517918492823;im:-0.89571176023941246),
(re:-0.42577929156507294;im:-0.9048270524660193),
(re:-0.40673664307580087;im:-0.9135454576426005),
(re:-0.3875155864521032;im:-0.9218631515885004),
(re:-0.36812455268467859;im:-0.9297764858882511),
(re:-0.3485720473218154;im:-0.9372819894918914),
(re:-0.32886664673858385;im:-0.94437637023748089),
(re:-0.30901699437494756;im:-0.9510565162951535),
(re:-0.2890317969444721;im:-0.957319497532067),
(re:-0.2689198206152658;im:-0.9631625667976581),
(re:-0.24868988716485529;im:-0.968583161128631),
(re:-0.22835087011065666;im:-0.97357890287316007),
(re:-0.20791169081775981;im:-0.97814760073380558),
(re:-0.18738131458572552;im:-0.9822872507286886),
(re:-0.1667687467161027;im:-0.98599603707050487),
(re:-0.14608302856241245;im:-0.9892723329629882),
(re:-0.12533323356430462;im:-0.99211470131447776),
(re:-0.10452846326765423;im:-0.99452189536827329),
(re:-0.083677843332315788;im:-0.99649285924950426),
(re:-0.06279051952931408;im:-0.99802672842827156),
(re:-0.041875653729199866;im:-0.99912283009885838),
(re:-0.020942419883357617;im:-0.99978068347484539),
(re:-1.8369701987210297E-16;im:-1.0),
(re:0.02094241988335636;im:-0.9997806834748455),
(re:0.041875653729199505;im:-0.99912283009885838),
(re:0.06279051952931283;im:-0.99802672842827156),
(re:0.083677843332314539;im:-0.99649285924950437),
(re:0.104528463267653;im:-0.9945218953682734),
(re:0.12533323356430334;im:-0.992114701314478),
(re:0.14608302856241118;im:-0.9892723329629883),
(re:0.16676874671610142;im:-0.98599603707050509),
(re:0.18738131458572427;im:-0.9822872507286888),
(re:0.20791169081775857;im:-0.9781476007338058),
(re:0.22835087011065544;im:-0.9735789028731604),
(re:0.2486898871648541;im:-0.9685831611286313),
(re:0.26891982061526548;im:-0.9631625667976582),
(re:0.28903179694447095;im:-0.9573194975320673),
(re:0.30901699437494723;im:-0.9510565162951536),
(re:0.32886664673858268;im:-0.9443763702374812),
(re:0.34857204732181429;im:-0.9372819894918919),
(re:0.36812455268467748;im:-0.92977648588825157),
(re:0.3875155864521021;im:-0.9218631515885008),
(re:0.40673664307579976;im:-0.91354545764260109),
(re:0.4257792915650718;im:-0.9048270524660199),
(re:0.4446351791849271;im:-0.895711760239413),
(re:0.463296035119861;im:-0.88620357923121507),
(re:0.48175367410171493;im:-0.8763066800438638),
(re:0.49999999999999928;im:-0.8660254037844391),
(re:0.51802700937313;im:-0.8553642601605068),
(re:0.535826794978996;im:-0.8443279255020155),
(re:0.55339154924334388;im:-0.8329212407100996),
(re:0.570713567684431;im:-0.82114920913370448),
(re:0.5877852522924729;im:-0.80901699437494756),
(re:0.60459911486237439;im:-0.79652991802419659),
(re:0.6211477802783095;im:-0.78369345732584039),
(re:0.6374239897486893;im:-0.77051324277578959),
(re:0.6534206039901047;im:-0.75699505565175706),
(re:0.66913060635885779;im:-0.74314482547739458),
(re:0.684547105928688;im:-0.7289686274214121),
(re:0.6996633405133651;im:-0.7144726796328037),
(re:0.7144726796328028;im:-0.699663340513366),
(re:0.7289686274214113;im:-0.684547105928689),
(re:0.74314482547739369;im:-0.66913060635885879),
(re:0.75699505565175629;im:-0.6534206039901057),
(re:0.7705132427757888;im:-0.6374239897486903),
(re:0.7836934573258396;im:-0.6211477802783105),
(re:0.7965299180241958;im:-0.60459911486237539),
(re:0.8090169943749473;im:-0.5877852522924732),
(re:0.8211492091337037;im:-0.5707135676844321),
(re:0.832921240710099;im:-0.553391549243345),
(re:0.8443279255020147;im:-0.5358267949789971),
(re:0.8553642601605061;im:-0.51802700937313106),
(re:0.86602540378443837;im:-0.5000000000000004),
(re:0.8763066800438631;im:-0.4817536741017161),
(re:0.8862035792312145;im:-0.4632960351198621),
(re:0.89571176023941246;im:-0.44463517918492823),
(re:0.9048270524660193;im:-0.425779291565073),
(re:0.9135454576426005;im:-0.40673664307580093),
(re:0.9218631515885003;im:-0.38751558645210327),
(re:0.9297764858882511;im:-0.36812455268467864),
(re:0.9372819894918914;im:-0.34857204732181546),
(re:0.94437637023748078;im:-0.3288666467385839),
(re:0.9510565162951532;im:-0.30901699437494845),
(re:0.957319497532067;im:-0.28903179694447217),
(re:0.96316256679765777;im:-0.2689198206152667),
(re:0.968583161128631;im:-0.24868988716485535),
(re:0.97357890287316007;im:-0.22835087011065672),
(re:0.97814760073380547;im:-0.20791169081775987),
(re:0.9822872507286886;im:-0.18738131458572557),
(re:0.98599603707050487;im:-0.16676874671610276),
(re:0.9892723329629882;im:-0.1460830285624125),
(re:0.99211470131447776;im:-0.12533323356430467),
(re:0.99452189536827329;im:-0.1045284632676543),
(re:0.99649285924950426;im:-0.083677843332315857),
(re:0.9980267284282714;im:-0.06279051952931415),
(re:0.99912283009885838;im:-0.041875653729199928),
(re:0.99978068347484539;im:-0.020942419883357679));
  VAR k:longint;
  begin
    k:=random(100);
    a:=sc[k];
    b:=sc[k+100];
    c:=sc[k+200];
  end;
TYPE
  P_rawDataWorkerThreadTodo=^T_rawDataWorkerThreadTodo;
  T_rawDataWorkerThreadTodo=object(T_parallelTask)
    algorithm:P_functionPerPixelViaRawDataAlgorithm;
    y:longint;

    CONSTRUCTOR create(CONST algorithm_:P_functionPerPixelViaRawDataAlgorithm; CONST y_:longint);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

CONSTRUCTOR T_functionPerPixelViaRawDataJuliaAlgorithm.create;
  begin
    inherited create;
    addParameter('Julianess',pt_float);
    addParameter('Julia-Param',pt_2floats);
  end;

PROCEDURE T_functionPerPixelViaRawDataJuliaAlgorithm.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    julianess:=0;
    juliaParam:=0;
  end;

FUNCTION T_functionPerPixelViaRawDataJuliaAlgorithm.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+2;
  end;

PROCEDURE T_functionPerPixelViaRawDataJuliaAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case(byte(index-inherited numberOfParameters)) of
      0: begin
        if value.f0<>julianess then begin
          rawMapIsOutdated:=64;
        end;
        julianess:=value.f0;
      end;
      1: begin
        if (juliaParam.re<>value.f0) or (juliaParam.im<>value.f1) then begin
          rawMapIsOutdated:=64;
        end;
        juliaParam.re:=value.f0;
        juliaParam.im:=value.f1;
      end;
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataJuliaAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index));
    case byte(index-inherited numberOfParameters) of
      0: result.createFromValue(parameterDescription(inherited numberOfParameters  ),julianess);
      1: result.createFromValue(parameterDescription(inherited numberOfParameters+1),juliaParam.re,juliaParam.im);
    end;
  end;

TYPE
P_lyapunov         =^T_lyapunov         ;
T_lyapunov=object(T_functionPerPixelViaRawDataAlgorithm)
  sequence:array of boolean;
  parX0:double;
  PROCEDURE parseSequence(CONST s:string);
  FUNCTION sequenceAsString:string;
  CONSTRUCTOR create;
  PROCEDURE resetParameters(CONST style:longint); virtual;
  PROCEDURE cleanup; virtual;
  FUNCTION numberOfParameters:longint; virtual;
  PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
  FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
  FUNCTION parameterResetStyles:T_arrayOfString; virtual;
  FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual;
end;

PROCEDURE T_lyapunov.parseSequence(CONST s: string);
  VAR c:char;
  begin
    setLength(sequence,0);
    for c in s do case c of
      'a','A': begin setLength(sequence,length(sequence)+1); sequence[length(sequence)-1]:=false; end;
      'b','B': begin setLength(sequence,length(sequence)+1); sequence[length(sequence)-1]:=true;  end;
    end;
    if length(sequence)=0 then begin
      setLength(sequence,1);
      sequence[0]:=false;
    end;
  end;

FUNCTION T_lyapunov.sequenceAsString: string;
  CONST c:array[false..true] of char=('A','B');
  VAR b:boolean;
  begin
    result:='';
    for b in sequence do result:=result+c[b];
  end;

CONSTRUCTOR T_lyapunov.create;
  begin
    inherited create;
    addParameter('sequence',pt_string);
    addParameter('x0',pt_float);
  end;

PROCEDURE T_lyapunov.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    parX0:=0.5;
    case byte(style) of
      0: parseSequence('AB');
      1: parseSequence('BBBBBBAAAAAA');
    end;
  end;

PROCEDURE T_lyapunov.cleanup;
  begin
    inherited cleanup;
    setLength(sequence,0);
  end;

FUNCTION T_lyapunov.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+2;
  end;

PROCEDURE T_lyapunov.setParameter(CONST index: byte;
  CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters
    then inherited setParameter(index,value)
    else case(index-inherited numberOfParameters) of
      0: begin parseSequence(value.fileName); rawMapIsOutdated:=64; end;
      1: begin parX0   :=value.f0;            rawMapIsOutdated:=64; end;
    end;
  end;

FUNCTION T_lyapunov.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters
    then result:=inherited getParameter(index)
    else case(index-inherited numberOfParameters) of
      0: result:=parValue(index,sequenceAsString);
      1: result:=parValue(index,parX0);
    end;
  end;

FUNCTION T_lyapunov.parameterResetStyles: T_arrayOfString;
  begin
    result:='Standard';
    append(result,'Zircon Zity');
  end;

FUNCTION T_lyapunov.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  FUNCTION toSphereZ(CONST x:double):double; inline;
    begin
      if (isNan(x)) then exit(0);
      result:=8/(4+x*x);
    end;

  FUNCTION toSphere(CONST x:double):T_rgbFloatColor; inline;
    VAR t:double;
    begin
      if not(isValid(x)) then exit(BLACK);
      t:=4/(4+x*x);
      result[cc_red]:=x*t;
      result[cc_green]:=0;
      result[cc_blue]:=t*2;
    end;

  FUNCTION dist  (CONST x,y:T_rgbFloatColor):double; inline; begin result:=sqrt(system.sqr(x[cc_red]-y[cc_red])+system.sqr(x[cc_green]-y[cc_green])+system.sqr(x[cc_blue]-y[cc_blue])); end;
  FUNCTION sqDist(CONST x,y:T_rgbFloatColor):double; inline; begin result:=     system.sqr(x[cc_red]-y[cc_red])+system.sqr(x[cc_green]-y[cc_green])+system.sqr(x[cc_blue]-y[cc_blue]) ; end;

  FUNCTION lyapunovStep(CONST c:T_Complex; VAR x:double; CONST i:longint):double; inline;
    begin
      if sequence[i mod length(sequence)]
      then begin result:=abs(c.im*(1-2*x)); x:=c.im*x*(1-x); end
      else begin result:=abs(c.re*(1-2*x)); x:=c.re*x*(1-x); end;
      if result>1E-50 then result:=system.ln(result) else result:=-115.129;
    end;

  VAR i:longint;
      x,x1,x2:double;
      c,c1,c2:T_Complex;
      h0,h1,h2:T_Complex;
      r,s,v:T_rgbFloatColor;
      d0,d1,d2:double;
      sphereZ:double;
      normSol:T_Vec3;
  begin
    c:=xy;
    i:=0;
    result:=BLACK;
    s:=BLACK;
    v:=BLACK;
    case colorSource of
      0..2: begin
        x:=parX0;
        lyapunovStep(c,x,0);
        while (i<maxDepth) and not(isNan(x) or isInfinite(x)) do begin
          sphereZ:=lyapunovStep(c,x,i);
          result[cc_green]:=result[cc_green]+sphereZ;
          result[cc_blue ]:=result[cc_blue]*0.95+0.05*sphereZ;
          inc(i);
        end;
        result[cc_red  ]:=sphereZ;
        result[cc_green]:=result[cc_green]+(maxDepth-i)*sphereZ;
        result[cc_green]:=(result[cc_green]*(1/maxDepth));
        sphereZ:=sphereZ*0.05;
        while (i<maxDepth) do begin result[cc_blue]:=result[cc_blue]*0.95+sphereZ; inc(i); end;
        result[cc_red  ]:=arctan(result[cc_red  ])/pi+0.5;
        result[cc_green]:=arctan(result[cc_green])/pi+0.5;
        result[cc_blue ]:=arctan(result[cc_blue ])/pi+0.5;
      end;
      3..5: begin
        x:=parX0;
        lyapunovStep(c,x,0);
        while (i<maxDepth) and (x*x<1E6) do begin
          r:=toSphere(lyapunovStep(c,x,i));
          s:=s+r;
          v:=v+rgbColor(r[cc_red]*r[cc_red],r[cc_green]*r[cc_green],r[cc_blue]*r[cc_blue]);
          inc(i);
        end;
        result[cc_blue]:=i/maxDepth;
        while (i<maxDepth) and (x*x<1E10) do begin
          r:=toSphere(lyapunovStep(c,x,i));
          s:=s+r;
          v:=v+rgbColor(r[cc_red]*r[cc_red],r[cc_green]*r[cc_green],r[cc_blue]*r[cc_blue]);
          inc(i);
        end;
        result[cc_green]:=arg(x)/(2*pi); if result[cc_green]<0 then result[cc_green]:=result[cc_green]+1;
        while (i<maxDepth) and not(isNan(x) or isInfinite(x)) do begin
          r:=toSphere(lyapunovStep(c,x,i));
          s:=s+r;
          v:=v+rgbColor(r[cc_red]*r[cc_red],r[cc_green]*r[cc_green],r[cc_blue]*r[cc_blue]);
          inc(i);
        end;
        s:=s+r*(maxDepth-i);
        v:=v+rgbColor(r[cc_red]*r[cc_red],r[cc_green]*r[cc_green],r[cc_blue]*r[cc_blue])*(maxDepth-i);
        result[cc_red]:=((v[cc_red]+v[cc_green]+v[cc_blue])/maxDepth-innerProduct(s,s)*(1/(maxDepth*maxDepth)));
      end;
      6..8 : begin
        d0:=2*pi/3*random; c .re:=system.cos(d0); c .im:=system.sin(d0); c :=xy+0.5*scaler.getAbsoluteZoom*c ;
        d1:=d0+2*pi/3;     c1.re:=system.cos(d1); c1.im:=system.sin(d1); c1:=xy+0.5*scaler.getAbsoluteZoom*c1;
        d2:=d1+2*pi/3;     c2.re:=system.cos(d2); c2.im:=system.sin(d2); c2:=xy+0.5*scaler.getAbsoluteZoom*c2;

        x :=parX0; r:=BLACK; lyapunovStep(c ,x ,i);
        x1:=parX0; s:=BLACK; lyapunovStep(c1,x1,i);
        x2:=parX0; v:=BLACK; lyapunovStep(c2,x2,i);
        while not(isNan(x) or isInfinite(x)) and not(isNan(x1) or isInfinite(x1)) and not(isNan(x2) or isInfinite(x2)) and (i<maxDepth) do begin
          r:=r*0.9+toSphere(lyapunovStep(c ,x ,i))*0.1;
          s:=s*0.9+toSphere(lyapunovStep(c1,x1,i))*0.1;
          v:=v*0.9+toSphere(lyapunovStep(c2,x2,i))*0.1;
          inc(i);
        end;
        while (i<maxDepth) do begin
          r:=r*0.9+toSphere(x )*0.1;
          s:=s*0.9+toSphere(x1)*0.1;
          v:=v*0.9+toSphere(x2)*0.1;
          inc(i);
        end;
        result[cc_red]:=(dist(r,s)+dist(s,v)+dist(r,v))*0.166666666666667 ;
        result[cc_green]:=sqrt(sqDist(r,s)+sqDist(s,v)+sqDist(r,s))*0.288675134594813;
        result[cc_blue]:=max(dist(r,s),max(dist(s,v),dist(r,v)))*0.5;
      end;
      else begin
        d0:=random;    h0.re:=system.cos(d0); h0.im:=system.sin(d0); h0:=scaler.getAbsoluteZoom*0.5*h0;
        d1:=d0+2*pi/3; h1.re:=system.cos(d1); h1.im:=system.sin(d1); h1:=scaler.getAbsoluteZoom*0.5*h1;
        d2:=d1+2*pi/3; h2.re:=system.cos(d2); h2.im:=system.sin(d2); h2:=scaler.getAbsoluteZoom*0.5*h2;

        c :=xy+h0; x :=parX0; d0:=0; lyapunovStep(c ,x ,i);
        c1:=xy+h1; x1:=parX0; d1:=0; lyapunovStep(c1,x1,i);
        c2:=xy+h2; x2:=parX0; d2:=0; lyapunovStep(c2,x2,i);
        i:=0;
        while (i<maxDepth) do begin
          d0:=d0+lyapunovStep(c ,x ,i);
          d1:=d1+lyapunovStep(c1,x1,i);
          d2:=d2+lyapunovStep(c2,x2,i);
          inc(i);
        end;
        //compute and normalize normal vector:-----------------//
        normSol:=solveSystemRowVec(
                   newVector(1,h0.re,h0.im),
                   newVector(1,h1.re,h1.im),
                   newVector(1,h2.re,h2.im),
                   newVector(arctan(d0/maxDepth)/pi*pseudoGamma*(1-2*(colorVariant and 1)),
                             arctan(d1/maxDepth)/pi*pseudoGamma*(1-2*(colorVariant and 1)),
                             arctan(d2/maxDepth)/pi*pseudoGamma*(1-2*(colorVariant and 1))));
        normSol[0]:=1;
        normSol:=normed(normSol);
        result[cc_blue] :=normSol[0];
        result[cc_red]  :=normSol[1];
        result[cc_green]:=normSol[2];
        //-------------------:compute and normalize normal vector
      end;
    end;
  end;
FUNCTION newLyapunov         :P_generalImageGenrationAlgorithm; begin new(P_lyapunov         (result),create); end;

CONSTRUCTOR T_rawDataWorkerThreadTodo.create(CONST algorithm_: P_functionPerPixelViaRawDataAlgorithm; CONST y_: longint);
  begin
    inherited create;
    algorithm:=algorithm_;
    y:=y_;
  end;

DESTRUCTOR T_rawDataWorkerThreadTodo.destroy;
  begin
  end;

PROCEDURE T_rawDataWorkerThreadTodo.execute;
  begin
    algorithm^.prepareRawMap(containedIn^.image,y);
  end;

CONSTRUCTOR T_functionPerPixelViaRawDataAlgorithm.create;
  CONST sourceNames:array[0..9] of string=( 'final',
      'average',
      'floating_average',
      'path chaos',
      'final angle',
      'steps to divergence',
      'avg. chaos',
      'avg.sqr.chaos',
      'max.chaos',
      'normal');
  CONST styleNames:array[0..9] of string=( 'fire / metal',
      'water / glass',
      'spectrum / plastic',
      'trafficLight / fire',
      'earth / drugged',
      'greyscale / gold',
      'zebra / levels',
      'greenzebra / strange',
      'rainbow / window',
      'discrete / line');
  CONST variantNames:array[0..3] of string=('direct',
      'inverted',
      'parabola',
      'inv.parabola');
  begin
    inherited create;
    temporaryRawMap:=nil;
    rawMapIsOutdated:=64;
    addParameter('depth',pt_integer,0);
    addParameter('source',pt_enum,0,9)^.setEnumValues(sourceNames);
    addParameter('style',pt_enum,0,9)^.setEnumValues(styleNames);
    addParameter('variant',pt_enum,0,3)^.setEnumValues(variantNames);
    addParameter('gamma',pt_float,1E-3,1E3);
    addParameter('light normal',pt_3floats,-1,1);
    resetParameters(0);
  end;

DESTRUCTOR T_functionPerPixelViaRawDataAlgorithm.destroy;
  begin
    cleanup;
    inherited destroy;
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    maxDepth    :=10;
    colorSource :=0;
    colorStyle  :=0;
    colorVariant:=0;
    pseudoGamma :=1;
    lightNormal :=rgbColor(1,1,2)*(1/system.sqrt(6));
    rawMapIsOutdated:=64;
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.cleanup;
  begin
    if temporaryRawMap<>nil then dispose(temporaryRawMap,destroy);
    temporaryRawMap:=nil;
    rawMapIsOutdated:=64;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+6;
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  FUNCTION normedVector(CONST x:T_rgbFloatColor):T_rgbFloatColor;
    VAR len:double;
    begin
      len:=sqrt(x[cc_red]*x[cc_red]+x[cc_green]*x[cc_green]+x[cc_blue]*x[cc_blue]);
      if len<1E-10 then result:=rgbColor(0,0,1)
      else result:=x*(1/len);
    end;

  begin
    if index<inherited numberOfParameters then begin
      inherited setParameter(index,value);
      if scalerChanagedSinceCalculation then begin
        rawMapIsOutdated:=64;
      end;
    end else case byte(index-inherited numberOfParameters) of
      0: begin
        if (maxDepth<>value.i0) then begin
          rawMapIsOutdated:=64;
        end;
        maxDepth:=value.i0;
      end;
      1: begin
        if (colorSource div 3 <> value.i0 div 3) then begin
          rawMapIsOutdated:=64;
        end;
        colorSource:=value.i0;
      end;
      2: colorStyle:=value.i0;
      3: begin
           colorVariant:=value.i0;
           if colorSource>=9 then rawMapIsOutdated:=64;
         end;
      4: begin
           pseudoGamma:=value.f0;
           if colorSource>=9 then rawMapIsOutdated:=64;
         end;
      5: lightNormal:=normedVector(value.color);
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters
    then result:=inherited getParameter(index)
    else case byte(index-inherited numberOfParameters) of
      0: result:=parValue(index,maxDepth);
      1: result:=parValue(index,colorSource);
      2: result:=parValue(index,colorStyle);
      3: result:=parValue(index,colorVariant);
      4: result:=parValue(index,pseudoGamma);
    else result:=parValue(index,lightNormal);
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.lightIsRelevant: boolean;
  begin
    result:=colorSource>=9;
  end;

FUNCTION toSphere(CONST x:T_Complex):T_rgbFloatColor; inline;
  VAR t:double;
  begin
    if not(isValid(x)) then exit(BLACK);
    t:=4/(4+x.re*x.re+x.im*x.im);
    result[cc_red  ]:=x.re*t;
    result[cc_green]:=x.im*t;
    result[cc_blue ]:=t*2;
  end;

{$MACRO ON}
{$define getRawDataAt_Body:=
  FUNCTION toSphereZ(CONST x:T_Complex):double; inline;
    begin
      if not(isValid(x)) then exit(0);
      result:=8/(4+x.re*x.re+x.im*x.im);
    end;

  FUNCTION dist  (CONST x,y:T_rgbFloatColor):double; inline; begin result:=sqrt(system.sqr(x[cc_red]-y[cc_red])+system.sqr(x[cc_green]-y[cc_green])+system.sqr(x[cc_blue]-y[cc_blue])); end;
  FUNCTION sqDist(CONST x,y:T_rgbFloatColor):double; inline; begin result:=     system.sqr(x[cc_red]-y[cc_red])+system.sqr(x[cc_green]-y[cc_green])+system.sqr(x[cc_blue]-y[cc_blue]) ; end;

  VAR i:longint;
      x,x1,x2,
      c,c1,c2:T_Complex;
      h0,h1,h2:T_Complex;
      r,s,v:T_rgbFloatColor;
      d0,d1,d2:double;
      sphereZ:double;
      normSol:T_Vec3;
  begin
    c:=xy;
    i:=0;
    result:=BLACK;
    s:=BLACK;
    v:=BLACK;
    case colorSource of
      0..2: begin
        iterationStart(c,x);
        while (i<maxDepth) and (isValid(x)) do begin
          iterationStep(c,x);
          sphereZ:=toSphereZ(x);
          result[cc_green]:=result[cc_green]+sphereZ;
          result[cc_blue]:=result[cc_blue]*0.95+0.05*sphereZ;
          inc(i);
        end;
        sphereZ:=toSphereZ(x);
        result[cc_red]:=0.5*sphereZ;
        result[cc_green]:=result[cc_green]+(maxDepth-i)*sphereZ;
        result[cc_green]:=0.5*(result[cc_green]*(1/maxDepth));
        sphereZ:=sphereZ*0.05;
        while (i<maxDepth) do begin result[cc_blue]:=result[cc_blue]*0.95+sphereZ; inc(i); end;
        result[cc_blue]:=0.5*result[cc_blue];
      end;
      3..5: begin
        iterationStart(c,x);
        while (i<maxDepth) and (x.re*x.re+x.im*x.im<1E6) do begin
          iterationStep(c,x);
          r:=toSphere(x);
          s:=s+r;
          v:=v+rgbColor(r[cc_red]*r[cc_red],r[cc_green]*r[cc_green],r[cc_blue]*r[cc_blue]);
          inc(i);
        end;
        result[cc_blue]:=i/maxDepth;
        while (i<maxDepth) and (x.re*x.re+x.im*x.im<1E10) do begin
          iterationStep(c,x);
          r:=toSphere(x);
          s:=s+r;
          v:=v+rgbColor(r[cc_red]*r[cc_red],r[cc_green]*r[cc_green],r[cc_blue]*r[cc_blue]);
          inc(i);
        end;
        result[cc_green]:=arg(x)/(2*pi); if result[cc_green]<0 then result[cc_green]:=result[cc_green]+1;
        while (i<maxDepth) and isValid(x) do begin
          iterationStep(c,x);
          r:=toSphere(x);
          s:=s+r;
          v:=v+rgbColor(r[cc_red]*r[cc_red],r[cc_green]*r[cc_green],r[cc_blue]*r[cc_blue]);
          inc(i);
        end;
        s:=s+r*(maxDepth-i);
        v:=v+rgbColor(r[cc_red]*r[cc_red],r[cc_green]*r[cc_green],r[cc_blue]*r[cc_blue])*(maxDepth-i);
        result[cc_red]:=((v[cc_red]+v[cc_green]+v[cc_blue])/maxDepth-innerProduct(s,s)*(1/(maxDepth*maxDepth)));
      end;
      6..8 : begin
        getTriangleBase(c,c1,c2);
        c :=xy+0.5*scaler.getAbsoluteZoom*c ;
        c1:=xy+0.5*scaler.getAbsoluteZoom*c1;
        c2:=xy+0.5*scaler.getAbsoluteZoom*c2;

        iterationStart(c ,x ); r:=BLACK;
        iterationStart(c1,x1); s:=BLACK;
        iterationStart(c2,x2); v:=BLACK;
        while isValid(x) and isValid(x1) and isValid(x2) and (i<maxDepth) do begin
          iterationStep(c ,x );
          iterationStep(c1,x1);
          iterationStep(c2,x2);
          inc(i);
        end;
        r:=toSphere(x );
        s:=toSphere(x1);
        v:=toSphere(x2);
        result[cc_red  ]:=(dist(r,s)+dist(s,v)+dist(r,v))*0.166666666666667 ;
        result[cc_green]:=sqrt(sqDist(r,s)+sqDist(s,v)+sqDist(r,s))*0.288675134594813;
        result[cc_blue ]:=max(dist(r,s),max(dist(s,v),dist(r,v)))*0.5;
      end;
      else begin
        getTriangleBase(h0,h1,h2);
        h0:=0.5*scaler.getAbsoluteZoom*h0;
        h1:=0.5*scaler.getAbsoluteZoom*h1;
        h2:=0.5*scaler.getAbsoluteZoom*h2;

        c :=xy+h0; iterationStart(c ,x ); d0:=0;
        c1:=xy+h1; iterationStart(c1,x1); d1:=0;
        c2:=xy+h2; iterationStart(c2,x2); d2:=0;

        i:=0;
        while (i<maxDepth) do begin
          if isValid(x)  then begin iterationStep(c ,x ); d0:=d0+toSphereZ(x);  end;
          if isValid(x1) then begin iterationStep(c1,x1); d1:=d1+toSphereZ(x1); end;
          if isValid(x2) then begin iterationStep(c2,x2); d2:=d2+toSphereZ(x2); end;
          inc(i);
        end;

        //compute and normalize normal vector:-----------------//
        normSol:=solveSystemRowVec(
                   newVector(1,h0.re,h0.im),
                   newVector(1,h1.re,h1.im),
                   newVector(1,h2.re,h2.im),
                   newVector(sqrt(d0/maxDepth)*pseudoGamma*(1-2*(colorVariant and 1)),
                             sqrt(d1/maxDepth)*pseudoGamma*(1-2*(colorVariant and 1)),
                             sqrt(d2/maxDepth)*pseudoGamma*(1-2*(colorVariant and 1))));
        //solving
        // (1 dx1 dy1) (a  )   (f1)
        // (1 dx2 dy2)*(b_x) = (f2)
        // (1 dx3 dy3) (b_y)   (f3)
        //so that f is locally aproximated by f(x,y)=a + x*b_x + y*b_y
        //The (not normalized) normal vector on this function is:
        //                 ( 1 )   ( 0 )   (  1 )
        // df/dx x df/dy = ( 0 ) x ( 1 ) = (-b_x)
        //                 (b_x)   (b_y)   (-b_y)

        normSol[0]:=1;
        normSol:=normed(normSol);
        result[cc_blue] :=normSol[0];
        result[cc_red]  :=normSol[1];
        result[cc_green]:=normSol[2];
        //-------------------:compute and normalize normal vector
      end;
    end;
  end}

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getColor(CONST rawData: T_rgbFloatColor): T_rgbFloatColor;
  FUNCTION fire(x:double):T_rgbFloatColor; inline;
    begin
      if x<0 then result:=rgbColor(0,0,0)
      else if x<1/3 then result:=rgbColor(3*x,0,0)
      else if x<2/3 then result:=rgbColor(1,3*x-1,0)
      else if x<1   then result:=rgbColor(1,1,3*x-2)
      else result:=rgbColor(1,1,1);
    end;

  FUNCTION water(x:double):T_rgbFloatColor; inline;
    begin
      if x<0 then result:=rgbColor(0,0,0)
      else if x<1/2 then result:=rgbColor(0,0,2*x)
      else if x<1   then result:=rgbColor(2*x-1,2*x-1,1)
      else result:=rgbColor(1,1,1);
    end;

  FUNCTION spectrum(x:double):T_rgbFloatColor; inline;
    begin
      x:=x*6;
      if      x<0   then result:=rgbColor(0,0,0)
      else if x<0.5 then result:=rgbColor(2*x,0,0)
      else if x<1.5 then result:=rgbColor(1,x-0.5,0)
      else if x<2.5 then result:=rgbColor(1-(x-1.5),1,0)
      else if x<3.5 then result:=rgbColor(0,1,x-2.5)
      else if x<4.5 then result:=rgbColor(0,1-(x-3.5),1)
      else if x<5.5 then result:=rgbColor(x-4.5,0,1)
      else if x<6   then result:=rgbColor(1-2*(x-5.5),0,1-2*(x-5.5))
      else               result:=rgbColor(0,0,0);
    end;

  FUNCTION trafficLight(x:double):T_rgbFloatColor; inline;
    begin
      if x<0 then result:=rgbColor(0.8,0,0)
      else if x<1/2 then result:=rgbColor(1,2*x,0)
      else if x<1   then result:=rgbColor(1-(2*x-1),1,0)
      else result:=rgbColor(0,0.8,0);
    end;

  FUNCTION earth(x:double):T_rgbFloatColor; inline;
    begin
      if x<0 then result:=rgbColor(0,0,0)
      else if x<1/3 then result:=rgbColor(3*x,1.5*x,0)
      else if x<2/3 then result:=rgbColor(  1-0.3*(3*x-1),0.5+0.2*(3*x-1),0.7*(3*x-1))
      else if x<1   then result:=rgbColor(0.7+0.3*(3*x-2),0.7+0.3*(3*x-2),0.7+0.3*(3*x-2))
      else result:=rgbColor(1,1,1);
    end;

  FUNCTION greyscale(x:double):T_rgbFloatColor; inline;
    begin
      if x<0 then result:=rgbColor(0,0,0)
      else if x<1   then result:=rgbColor(x,x,x)
      else result:=rgbColor(1,1,1);
    end;

  FUNCTION zebra(x:double):T_rgbFloatColor; inline;
    VAR q:longint;
    begin
      if x<0 then x:=0 else if x>1 then x:=1;
      q:=round(128*x);
      if odd(q) then result:=rgbColor(0,0,0)
                else result:=rgbColor(1,1,1);
    end;

  FUNCTION greenzebra(x:double):T_rgbFloatColor; inline;
    VAR q:longint;
    begin
      if x<0 then x:=0 else if x>1 then x:=1;
      q:=round(128*x);
      if odd(q) then result:=rgbColor(0,x,0)
                else result:=rgbColor(1-x,1-x,1-x);
    end;

  FUNCTION rainbow(x:double):T_rgbFloatColor; inline;
    begin
      if x<0 then x:=0
      else if x>1 then x:=6
      else x:=6*x;
      if      x<1 then result:=rgbColor(1  ,x  ,0  )
      else if x<2 then result:=rgbColor(2-x,1  ,0  )
      else if x<3 then result:=rgbColor(0  ,1  ,x-2)
      else if x<4 then result:=rgbColor(0  ,4-x,1  )
      else if x<5 then result:=rgbColor(x-4,0  ,1  )
      else             result:=rgbColor(1  ,0  ,6-x);
    end;

  FUNCTION discrete(x:double):T_rgbFloatColor; inline;
    begin
      if x<0.5 then result:=BLACK
               else result:=WHITE;
    end;

  FUNCTION normalToColor_metal(CONST n:T_rgbFloatColor):T_rgbFloatColor;
    VAR diffuse,spec4,spec8,spec16:single;
    CONST colAmb   :T_rgbFloatColor=(0.1,0.1,0.1);
          colDiff  :T_rgbFloatColor=(0.3,0.3,0.3);
          colSpec05:T_rgbFloatColor=(0.6,0.0,0.0);
          colSpec08:T_rgbFloatColor=(0.0,0.6,0.0);
          colSpec16:T_rgbFloatColor=(0.0,0.0,0.6);

    begin
      diffuse :=innerProduct(lightNormal,n);
      spec4:=-(lightNormal[cc_blue]-2*n[cc_blue]*diffuse);
      if spec4<0 then begin
      result:=colAmb+colDiff*diffuse;
      end else begin
        spec4 *=spec4;
        spec4 *=spec4;
        spec8 :=spec4*spec4;
        spec16:=spec8*spec8;
        result:=colAmb+colDiff*diffuse
                      +colSpec05*spec4
                      +colSpec08*spec8
                      +colSpec16*spec16;
      end;

    end;

  FUNCTION normalToColor_glass(CONST n:T_rgbFloatColor):T_rgbFloatColor;
    VAR diffuse,spec:single;
    CONST colAmb   :T_rgbFloatColor=(0.0,0.0,0.2);
          colDiff  :T_rgbFloatColor=(0.0,0.0,0.5);
          colSpec  :T_rgbFloatColor=(0.8,0.8,0.8);
    begin
      diffuse :=innerProduct(lightNormal,n);
      spec:=-(lightNormal[cc_blue]-2*n[cc_blue]*diffuse);
      if spec<0.99 then begin
      result:=colAmb+colDiff*diffuse;
      end else begin
        result:=colAmb+colDiff*diffuse
                      +colSpec*(spec-0.99)*100;
      end;
    end;

  FUNCTION normalToColor_drugged(CONST n:T_rgbFloatColor):T_rgbFloatColor;
    CONST colAmb   :T_rgbFloatColor=(0.3,0.3,0.3);
          colDiff  :T_rgbFloatColor=(0.3,0.3,0.3);
    VAR spec,diffuse:single;
    begin
      diffuse :=innerProduct(lightNormal,n);
      spec:=-(lightNormal[cc_blue]-2*n[cc_blue]*diffuse);
      if spec<1 then
      result:=colAmb+colDiff*diffuse+hsvColor(spec,1,spec)
      else result:=BLACK;
    end;

  FUNCTION normalToColor_plastic(CONST n:T_rgbFloatColor):T_rgbFloatColor;
    CONST colAmb   :T_rgbFloatColor=(0.5,0.0,0.0);
          colDiff  :T_rgbFloatColor=(0.5,0.0,0.0);
          colSpec  :T_rgbFloatColor=(0.0,1,1);
    VAR spec,diffuse:single;
    begin
      diffuse :=innerProduct(lightNormal,n);
      spec:=-(lightNormal[cc_blue]-2*n[cc_blue]*diffuse);
      if spec<0 then result:=colAmb+colDiff*diffuse else begin
        spec:=spec*spec;
        spec:=spec*spec;
        spec:=spec*spec;
        result:=colAmb+colDiff*diffuse+colSpec*spec;
      end;
    end;

  FUNCTION normalToColor_fire(CONST n:T_rgbFloatColor):T_rgbFloatColor;
    VAR x:single;
    begin
      x:=innerProduct(lightNormal,n);
      x:=1.5+1.5*x;//(lightNormal[2]-2*n[2]*x);
      if      x>2 then begin result[cc_red]:=1; result[cc_green]:=1;   result[cc_blue]:=x-2; end
      else if x>1 then begin result[cc_red]:=1; result[cc_green]:=x-1; result[cc_blue]:=0;   end
      else             begin result[cc_red]:=x; result[cc_green]:=0;   result[cc_blue]:=0;   end;
    end;

  FUNCTION normalToColor_gold(CONST n:T_rgbFloatColor):T_rgbFloatColor;
    CONST colAmb   :T_rgbFloatColor=(0.5,0.25, 0);
          colDiff  :T_rgbFloatColor=(0.5,0.5,0);
          colSpec  :T_rgbFloatColor=(0.5,0.25,0.5);
    VAR spec,diffuse:single;
    begin
      diffuse :=innerProduct(lightNormal,n);
      spec:=-(lightNormal[cc_blue]-2*n[cc_blue]*diffuse);
      if (spec<0) then result:=colAmb+colDiff*diffuse else begin
        spec:=spec*spec;
        spec:=spec*spec;
        spec:=spec*spec;
        result:=projectedColor(colAmb+colDiff*diffuse+colSpec*spec);
      end;
    end;

  FUNCTION normalToColor_levels(CONST n:T_rgbFloatColor):T_rgbFloatColor;
    begin
      result:=WHITE*(round(5+5*innerProduct(lightNormal,n))*0.1);
    end;

  FUNCTION normalToColor_window(n:T_rgbFloatColor):T_rgbFloatColor;
    CONST colAmbient:T_rgbFloatColor=(0.1,0.05,0.0);
          colDiffuse:T_rgbFloatColor=(0.4,0.20,0.0);

    VAR diffuse:single;
        specVec:T_rgbFloatColor;
    begin
      diffuse:=innerProduct(lightNormal,n);
      specVec:=lightNormal-n*(2*diffuse);
      result:=colAmbient+colDiffuse*diffuse;
      if (specVec[cc_blue]<0) then begin
        if (abs(specVec[cc_green])<0.2 ) and (abs(specVec[cc_red])<0.2 ) and
           (abs(specVec[cc_green])>0.02) and (abs(specVec[cc_red])>0.02) then result:=result+WHITE*0.75
        else result:=result+WHITE*(0.5*specVec[cc_blue]*specVec[cc_blue]);
      end;
    end;

  FUNCTION normalToColor_line(CONST n:T_rgbFloatColor):T_rgbFloatColor;
    CONST colAmbient:T_rgbFloatColor=(0.0,0.3,0.0);
          colDiffuse:T_rgbFloatColor=(0.0,0.3,0.0);

    VAR diffuse:single;
        specVec:T_rgbFloatColor;
    begin
      diffuse:=innerProduct(lightNormal,n);
      specVec:=lightNormal-n*(2*diffuse);
      result:=colAmbient+colDiffuse*diffuse;
      if (specVec[cc_blue]<0) then begin
        specVec[cc_green]:=specVec[cc_green]+specVec[cc_red];
        if (abs(specVec[cc_green])<0.02) then result:=result-WHITE*specVec[cc_green]
        else result:=result+WHITE*(0.7*specVec[cc_blue]*specVec[cc_blue]*(1-specVec[cc_green]));
      end;
    end;

  FUNCTION normalToColor_strange(CONST n:T_rgbFloatColor):T_rgbFloatColor;
    CONST colAmbient:T_rgbFloatColor=(0.1,0.1,0.1);
          colDiffuse:T_rgbFloatColor=(0.4,0.4,0.4);

          vec1:T_rgbFloatColor=( 0                  , 0.18257418276333211,0.98319208082057976);
          vec2:T_rgbFloatColor=( 0.15811387502784748,-0.09128710059683582,0.98319208082057976);
          vec3:T_rgbFloatColor=(-0.15811389098898917,-0.0912870729513256 ,0.98319208082057976);
    VAR diffuse:single;
        specVec,spec:T_rgbFloatColor;

    begin
      diffuse:=innerProduct(lightNormal,n);
      specVec:=lightNormal-n*(2*diffuse);
      spec[cc_red  ]:=innerProduct(specVec,vec1); if spec[cc_red  ]>0 then spec[cc_red  ]:=0;
      spec[cc_green]:=innerProduct(specVec,vec2); if spec[cc_green]>0 then spec[cc_green]:=0;
      spec[cc_blue ]:=innerProduct(specVec,vec3); if spec[cc_blue ]>0 then spec[cc_blue ]:=0;
      spec[cc_red  ]:=spec[cc_red  ]*spec[cc_red  ]; spec[cc_red  ]:=spec[cc_red  ]*spec[cc_red  ];
      spec[cc_green]:=spec[cc_green]*spec[cc_green]; spec[cc_green]:=spec[cc_green]*spec[cc_green];
      spec[cc_blue ]:=spec[cc_blue ]*spec[cc_blue ]; spec[cc_blue ]:=spec[cc_blue ]*spec[cc_blue ];
      result[cc_red  ]:=colAmbient[cc_red  ]+diffuse*colDiffuse[cc_red  ]+spec[cc_red  ]*spec[cc_red  ]*0.5;
      result[cc_green]:=colAmbient[cc_green]+diffuse*colDiffuse[cc_green]+spec[cc_green]*spec[cc_green]*0.5;
      result[cc_blue ]:=colAmbient[cc_blue ]+diffuse*colDiffuse[cc_blue ]+spec[cc_blue ]*spec[cc_blue ]*0.5;
    end;

  CONST source:array[0..8] of T_colorChannel=
        (cc_red,cc_green,cc_blue,
         cc_red,cc_green,cc_blue,
         cc_red,cc_green,cc_blue);
  VAR aid:double=0;
  begin
    result:=BLACK;
    if colorSource<9 then begin
      case colorVariant of
        0: aid:=                 rawData[source[colorSource]] ;
        1: aid:=1-               rawData[source[colorSource]] ;
        2: aid:=  system.sqr(1-2*rawData[source[colorSource]]);
        3: aid:=1-system.sqr(1-2*rawData[source[colorSource]]);
      end;
      if aid>1E-10 then aid:=system.exp(system.ln(aid)*pseudoGamma)
                   else aid:=0;
      case colorStyle of
        0: result:=fire(aid);
        1: result:=water(aid);
        2: result:=spectrum(aid);
        3: result:=trafficLight(aid);
        4: result:=earth(aid);
        5: result:=greyscale(aid);
        6: result:=zebra(aid);
        7: result:=greenzebra(aid);
        8: result:=rainbow(aid);
        9: result:=discrete(aid);
      end;
    end else begin
      case colorStyle of
        0: result:=normalToColor_metal  (rawData);
        1: result:=normalToColor_glass  (rawData);
        2: result:=normalToColor_plastic(rawData);
        3: result:=normalToColor_fire   (rawData);
        4: result:=normalToColor_drugged(rawData);
        5: result:=normalToColor_gold   (rawData);
        6: result:=normalToColor_levels (rawData);
        7: result:=normalToColor_strange(rawData);
        8: result:=normalToColor_window (rawData);
        9: result:=normalToColor_line   (rawData);
      end;
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getColorAt(CONST ix, iy: longint; CONST xy: T_Complex): T_rgbFloatColor;
  begin
    result:=getColor(getRawDataAt(xy));
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.prepareRawMap(VAR target: T_rawImage; CONST my: longint);
  VAR y,x:longint;
      dat:T_rgbFloatColor;
  begin
    for y:=0 to temporaryRawMap^.dimensions.height-1 do if y and 63=my then
    for x:=0 to temporaryRawMap^.dimensions.width-1 do begin
      dat:=getRawDataAt(scaler.transform(x,y));
      temporaryRawMap^[x,y]:=dat;
      target[x,y]:=getColor(dat);
    end;
    interlockedDecrement(rawMapIsOutdated);
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.execute(CONST context: P_abstractWorkflow);
  VAR x,y:longint;
  FUNCTION todo(CONST y:longint):P_rawDataWorkerThreadTodo;
    begin new(result,create(@self,y)); end;

  begin with context^ do begin
    scaler.rescale(image.dimensions.width,image.dimensions.height);
    if previewQuality then begin
      if scalerChanagedSinceCalculation or
         (temporaryRawMap=nil) or
         (temporaryRawMap^.dimensions.width<>image.dimensions.width) or
         (temporaryRawMap^.dimensions.height<>image.dimensions.height) then rawMapIsOutdated:=64;
      if temporaryRawMap=nil then new(temporaryRawMap,create(image.dimensions.width,image.dimensions.height));
      temporaryRawMap^.resize(image.dimensions, res_dataResize);
      if rawMapIsOutdated>0 then begin
        scalerChanagedSinceCalculation:=false;
        clearQueue;
        rawMapIsOutdated:=64;
        for y:=0 to 63 do enqueue(todo(y));
        waitForFinishOfParallelTasks;
      end else begin
        for y:=0 to image.dimensions.height-1 do for x:=0 to image.dimensions.width-1 do
        image[x,y]:=getColor(temporaryRawMap^[x,y]);
      end;
    end else begin
      if temporaryRawMap<>nil then begin
        dispose(temporaryRawMap,destroy);
        temporaryRawMap:=nil;
      end;
      inherited execute(context);
    end;
  end; end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_newton3Algorithm =^T_newton3Algorithm ;
     T_newton3Algorithm=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_newton3Algorithm.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=(2/3)*x+1/(3*sqr(x)); end;
  getRawDataAt_Body;
FUNCTION newNewton3Algorithm :P_generalImageGenrationAlgorithm; begin new(P_newton3Algorithm (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_newton5Algorithm =^T_newton5Algorithm ;
     T_newton5Algorithm=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_newton5Algorithm.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=(4/5)*x+1/(5*sqr(sqr(x))); end;
  getRawDataAt_Body;
FUNCTION newNewton5Algorithm :P_generalImageGenrationAlgorithm; begin new(P_newton5Algorithm (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_bump=^T_bump;
     T_bump=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_bump.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline;
    begin
      x:=c;
      c.re:=system.sin(x.re+system.exp(abs(x))*x.im);
      c.im:=system.sin(x.im-system.exp(abs(x))*x.re);
      c:=c*system.exp(abs(x));
      x:=0;
    end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=sqr(sqr(x))+c; end;
  getRawDataAt_Body;
FUNCTION newBump             :P_generalImageGenrationAlgorithm; begin new(P_bump             (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_diperiodic=^T_diperiodic ;
     T_diperiodic=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_diperiodic.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x.re:=2*system.cos(x.re*0.5); x.im:=2*system.cos(x.im*0.5); x:=exp(x); end;
  getRawDataAt_Body;
FUNCTION newDiperiodic       :P_generalImageGenrationAlgorithm; begin new(P_diperiodic       (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_expoA=^T_expoA;
     T_expoA=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_expoA.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=0; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=exp(c*x); end;
  getRawDataAt_Body;
FUNCTION newExpoA            :P_generalImageGenrationAlgorithm; begin new(P_expoA            (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_expoB=^T_expoB;
     T_expoB=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_expoB.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=exp(x); end;
  getRawDataAt_Body;
FUNCTION newExpoB            :P_generalImageGenrationAlgorithm; begin new(P_expoB            (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_expoCancel5a=^T_expoCancel5a;
     T_expoCancel5a=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_expoCancel5a.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=ln(1+x*(1+0.5*x*(1+(1/3)*x*(1+0.25*x)))); end;
  getRawDataAt_Body;
FUNCTION newExpoCancel5a     :P_generalImageGenrationAlgorithm; begin new(P_expoCancel5a     (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_expoCancel5b=^T_expoCancel5b;
     T_expoCancel5b=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_expoCancel5b.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=ln(x); x:=(1+x*(1+0.5*x*(1+(1/3)*x*(1+0.25*x)))); end;
  getRawDataAt_Body;
FUNCTION newExpoCancel5b     :P_generalImageGenrationAlgorithm; begin new(P_expoCancel5b     (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_freakWave=^T_freakWave;
     T_freakWave=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_freakWave.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=x.re*system.cos(x.re+x.im)+c; end;
  getRawDataAt_Body;
FUNCTION newFreakWave        :P_generalImageGenrationAlgorithm; begin new(P_freakWave        (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_lnTaylor=^T_lnTaylor;
     T_lnTaylor=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_lnTaylor.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=x-1+c; x:=x*(1-x*(1/2-x*(1/3-x*(1/4-x*(1/5-x*(1/6)))))); end;
  getRawDataAt_Body;
FUNCTION newLnTaylor         :P_generalImageGenrationAlgorithm; begin new(P_lnTaylor         (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_logisticEquation=^T_logisticEquation;
     T_logisticEquation=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_logisticEquation.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=0.5; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=c*x*(1-x); end;
  getRawDataAt_Body;
FUNCTION newLogisticEquation :P_generalImageGenrationAlgorithm; begin new(P_logisticEquation (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_logisticEquation2=^T_logisticEquation2;
     T_logisticEquation2=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_logisticEquation2.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline; begin x:=c; c:=(1-julianess)*c+julianess*juliaParam;  end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=c*sqr(1-x)/x; end;
  getRawDataAt_Body;
FUNCTION newLogisticEquation2:P_generalImageGenrationAlgorithm; begin new(P_logisticEquation2(result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_mandelbrot_p4=^T_mandelbrot_p4;
     T_mandelbrot_p4=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_mandelbrot_p4.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline;begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;begin x:=sqr(sqr(x))+c; end;
  getRawDataAt_Body;
FUNCTION newMandelbrot_p4    :P_generalImageGenrationAlgorithm; begin new(P_mandelbrot_p4    (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_mbCosine=^T_mbCosine;
     T_mbCosine=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_mbCosine.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline;begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;begin x:=c*cos(x); end;
  getRawDataAt_Body;
FUNCTION newMbCosine         :P_generalImageGenrationAlgorithm; begin new(P_mbCosine         (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_mbCosine2=^T_mbCosine2;
     T_mbCosine2=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_mbCosine2.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline;begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;begin x:=c/cos(x); end;
  getRawDataAt_Body;
FUNCTION newMbCosine2        :P_generalImageGenrationAlgorithm; begin new(P_mbCosine2        (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_nondivergent=^T_nondivergent;
     T_nondivergent=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_nondivergent.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline;begin x:=1; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;begin x:=1/sqr(x)+c; end;
  getRawDataAt_Body;
FUNCTION newNondivergent     :P_generalImageGenrationAlgorithm; begin new(P_nondivergent     (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_parabola=^T_parabola;
     T_parabola=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_parabola.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;
    VAR tmp:double;
    begin
      tmp:=((x.re-1)*(x.re-1)+x.im)/abs(x);
      x.im:=-x.im*tmp;
      x.re:= x.re*tmp;
    end;
  getRawDataAt_Body;
FUNCTION newParabola         :P_generalImageGenrationAlgorithm; begin new(P_parabola         (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_sinTaylor=^T_sinTaylor;
     T_sinTaylor=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_sinTaylor.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin x:=x*(1+sqr(x)*(-0.166666666666667+sqr(x)*(+0.00833333333333333+sqr(x)*(-0.000198412698412698+2.75573192239859E-6*sqr(x)))))+c; end;
  getRawDataAt_Body;
FUNCTION newSinTaylor        :P_generalImageGenrationAlgorithm; begin new(P_sinTaylor        (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_sinus=^T_sinus;
     T_sinus=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_sinus.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin x:=sin(x+c); end;
  getRawDataAt_Body;
FUNCTION newSinus            :P_generalImageGenrationAlgorithm; begin new(P_sinus            (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_invSinus=^T_invSinus;
     T_invSinus=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_invSinus.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin x:=1/sin(x); end;
  getRawDataAt_Body;
FUNCTION newInvSinus         :P_generalImageGenrationAlgorithm; begin new(P_invSinus         (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_tul=^T_tul;
     T_tul=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_tul.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin if sqrabs(x)>1 then x:=1/x else x:=x*(c+x); end;
  getRawDataAt_Body;
FUNCTION newTul              :P_generalImageGenrationAlgorithm; begin new(P_tul              (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_tul2=^T_tul2;
     T_tul2=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_tul2.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin x:=x*x+c; x:=0.5*(x+1/x); x:=0.5*(x+1/x); end;
  getRawDataAt_Body;
FUNCTION newTul2             :P_generalImageGenrationAlgorithm; begin new(P_tul2             (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_tul3=^T_tul3;
     T_tul3=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_tul3.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=1/c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin if x.im>0 then x:=sin(x+c) else x:=exp(x+c); end;
  getRawDataAt_Body;
FUNCTION newTul3             :P_generalImageGenrationAlgorithm; begin new(P_tul3             (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_tul4=^T_tul4;
     T_tul4=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_tul4.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=1/c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin if x.re>0 then x:=1/x+c else x:=1/sqr(x)+c; end;
  getRawDataAt_Body;
FUNCTION newTul4             :P_generalImageGenrationAlgorithm; begin new(P_tul4             (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_tul5=^T_tul5;
     T_tul5=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_tul5.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline;
    CONST p0:T_Complex=(re: 0                 ;im: 1  );
          p1:T_Complex=(re: 0.8660254037844388;im:-0.5);
          p2:T_Complex=(re:-0.8660254037844388;im:-0.5);
    begin
      if x.re*x.re+x.im*x.im>1E-20 then
      x:=c/(x-p0)+c/(x-p1)+c/(x-p2);
    end;
  getRawDataAt_Body;
FUNCTION newTul5             :P_generalImageGenrationAlgorithm; begin new(P_tul5             (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_tul5i=^T_tul5i;
     T_tul5i=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_tul5i.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
    PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
    PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline;
      CONST p0:T_Complex=(re: 0                 ;im: 1  );
            p1:T_Complex=(re: 0.8660254037844388;im:-0.5);
            p2:T_Complex=(re:-0.8660254037844388;im:-0.5);
      begin
        x:=c/(complex.inverse(x-p0)+complex.inverse(x-p1)+complex.inverse(x-p2));
      end;
    getRawDataAt_Body;
FUNCTION newTul5i            :P_generalImageGenrationAlgorithm; begin new(P_tul5i            (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_unnamed1=^T_unnamed1;
     T_unnamed1=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_unnamed1.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; x.im:=c.re; x.re:=c.im; c:=-1*x; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline;
    begin
      if x.re*x.re+x.im*x.im>2 then begin
        x:=sqr(x)-c;
        if x.re>0 then x.re:= sqrt( x.re)
                  else x.re:=-sqrt(-x.re);
        if x.im>0 then x.im:= sqrt( x.im)
                  else x.im:=-sqrt(-x.im);
      end else x:=sqr(x)*x-c;
    end;
  getRawDataAt_Body;
FUNCTION newUnnamed1         :P_generalImageGenrationAlgorithm; begin new(P_unnamed1         (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_unnamed2=^T_unnamed2;
     T_unnamed2=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_unnamed2.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin if abs(c)<1 then c:=1/c; x:=c ; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline;
    VAR pow:double;
    begin
      pow:=6-abs(x);
      if pow<1 then x:=x+c else
      if pow<2 then x:=x**pow+c
               else x:=sqr(x)+c;
    end;
  getRawDataAt_Body;
FUNCTION newUnnamed2         :P_generalImageGenrationAlgorithm; begin new(P_unnamed2         (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_unnamed3=^T_unnamed3;
     T_unnamed3=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_unnamed3.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
    PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; c:=sin(II* c); end;
    PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline;
      begin
        x:=cos(x)*c/x;
      end;
    getRawDataAt_Body;
FUNCTION newUnnamed3         :P_generalImageGenrationAlgorithm; begin new(P_unnamed3         (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_weierstrass4     =^T_weierstrass4     ;
     T_weierstrass4=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_weierstrass4.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline;
    CONST p1:T_Complex=(re: 2; im:0);
          p2:T_Complex=(re: 0; im:2);
          InvAid=1/(2*2);
    VAR i0,j0:int64;
        fi,fj:double;
    begin
      fi:=InvAid*( p2.im*x.re-p2.re*x.im);
      fj:=InvAid*(-p1.im*x.re+p1.re*x.im);
      i0:=round(fi);
      j0:=round(fj);
      x.re:=x.re-i0*p1.re-j0*p2.re;
      x.im:=x.im-i0*p1.im-j0*p2.im;
      x:=1/sqr(x)
        +1/sqr(x-p1)+1/sqr(x-p2)+1/sqr(x+p1)+1/sqr(x+p2)
        +1/sqr(x-p1-p2)+1/sqr(x-p1+p2)+1/sqr(x+p1-p2)+1/sqr(x+p1+p2)
        +1/sqr(x-p1-p1)+1/sqr(x-p2-p2)+1/sqr(x+p1+p1)+1/sqr(x+p2+p2)
        +1/sqr(x-p1-p1-p2)+1/sqr(x-p2-p2-p1)+1/sqr(x+p1+p1-p2)+1/sqr(x+p2+p2-p1)+1/sqr(x-p1-p1+p2)+1/sqr(x-p2-p2+p1)+1/sqr(x+p1+p1+p2)+1/sqr(x+p2+p2+p1);
    end;
  getRawDataAt_Body;
FUNCTION newWeierstrass4     :P_generalImageGenrationAlgorithm; begin new(P_weierstrass4     (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_weierstrass6     =^T_weierstrass6     ;
     T_weierstrass6=object(T_functionPerPixelViaRawDataAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_weierstrass6.getRawDataAt(CONST xy: T_Complex): T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline;begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;
    CONST p1:T_Complex=(re: 2; im: 0        );  // 1,0
          p2:T_Complex=(re:-1; im: sqrt(3)  );  // 0,1
          p3:T_Complex=(re:-1; im:-sqrt(3)  );  //-1,-1
          p4:T_Complex=(re: 4; im: 0        );  // 2,0
          p5:T_Complex=(re: 3; im: sqrt(3)  );  // 2,1
          p6:T_Complex=(re: 3; im:-sqrt(3)  );
          p7:T_Complex=(re:-2; im: sqrt(3)*2);
          p8:T_Complex=(re: 0; im: sqrt(3)*2);
          p9:T_Complex=(re:-2; im:-sqrt(3)*2);

          InvAid=1/(2*sqrt(3));
    VAR i0,j0:int64;
        fi,fj:double;
    begin
      fi:=InvAid*( p2.im*x.re-p2.re*x.im);
      fj:=InvAid*(-p1.im*x.re+p1.re*x.im);
      i0:=round(fi);
      j0:=round(fj);
      x.re:=x.re-i0*p1.re-j0*p2.re;
      x.im:=x.im-i0*p1.im-j0*p2.im;
      x:=1/sqr(x)
        +1/sqr(x-p1)+1/sqr(x+p1)
        +1/sqr(x-p2)+1/sqr(x+p2)
        +1/sqr(x-p3)+1/sqr(x+p3)
        +1/sqr(x-p4)+1/sqr(x+p4)
        +1/sqr(x-p5)+1/sqr(x+p5)
        +1/sqr(x-p6)+1/sqr(x+p6)
        +1/sqr(x-p7)+1/sqr(x+p7)
        +1/sqr(x-p8)+1/sqr(x+p8)
        +1/sqr(x-p9)+1/sqr(x+p9);
    end;
  getRawDataAt_Body;
FUNCTION newWeierstrass6     :P_generalImageGenrationAlgorithm; begin new(P_weierstrass6     (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_sinc=^T_sinc;
     T_sinc=object(T_functionPerPixelViaRawDataJuliaAlgorithm) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_sinc.getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin x:=x+c; x:=sin(x)/x; end;
  getRawDataAt_Body;
FUNCTION newSinc             :P_generalImageGenrationAlgorithm; begin new(P_sinc           (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE
P_mandelbrot=^T_mandelbrot       ;
T_mandelbrot=object(T_functionPerPixelViaRawDataJuliaAlgorithm)
  FUNCTION parameterResetStyles:T_arrayOfString; virtual;
  PROCEDURE resetParameters(CONST style:longint); virtual;
  FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual;
end;

FUNCTION T_mandelbrot.parameterResetStyles:T_arrayOfString;
  begin
    result:='Mandelbrot Set';
    append(result,'Julia Set');
  end;

PROCEDURE T_mandelbrot.resetParameters(CONST style:longint);
  begin
    inherited resetParameters(style);
    case style of
      1: begin
           julianess:=1;
           juliaParam.re:=-0.8;
           juliaParam.im:=0.156;
         end;
    end;
  end;

FUNCTION T_mandelbrot.getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); begin x:=sqr(x)+c; end;
  getRawDataAt_Body;
FUNCTION newMandelbrot       :P_generalImageGenrationAlgorithm; begin new(P_mandelbrot       (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE
P_mandelbar        =^T_mandelbar        ;
T_mandelbar=object(T_functionPerPixelViaRawDataJuliaAlgorithm)
  FUNCTION parameterResetStyles:T_arrayOfString; virtual;
  PROCEDURE resetParameters(CONST style:longint); virtual;
  FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual;  end;

FUNCTION T_mandelbar.parameterResetStyles:T_arrayOfString;
  begin
    result:='Mandelbar';
    append(result,'Mandelbar Julia Set');
  end;

PROCEDURE T_mandelbar.resetParameters(CONST style:longint);
  begin
    inherited resetParameters(style);
    case style of
      1: begin
           julianess:=1;
           juliaParam.re:=0.2822140;
           juliaParam.im:=0.6813474;
         end;
    end;
  end;

FUNCTION T_mandelbar.getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); begin x:=sqr(x); x.im:=-x.im; x:=x+c; end;
  getRawDataAt_Body;
FUNCTION newMandelbar        :P_generalImageGenrationAlgorithm; begin new(P_mandelbar        (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE
P_burningJulia     =^T_burningJulia     ;
T_burningJulia=object(T_functionPerPixelViaRawDataJuliaAlgorithm)
  FUNCTION parameterResetStyles:T_arrayOfString; virtual;
  PROCEDURE resetParameters(CONST style:longint); virtual;
  FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual;
end;

FUNCTION T_burningJulia.parameterResetStyles: T_arrayOfString;
  begin
    result:='Burning Ship';
    append(result,'Burning Ship Julia');
  end;

PROCEDURE T_burningJulia.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    case style of
      1: begin
           julianess:=1;
           juliaParam.re:=0.591925608954895;
           juliaParam.im:=0.918404930408219;
         end;
    end;
  end;

FUNCTION T_burningJulia.getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);
    VAR x_re:double;
    begin
      x_re:=x.re*x.re-x.im*x.im+c.re;
      if (x.re<0) = (x.im<0)
        then x.im:=c.im-2*x.re*x.im
        else x.im:=c.im+2*x.re*x.im;
      x.re:=x_re;
    end;
  getRawDataAt_Body;
FUNCTION newBurningJulia     :P_generalImageGenrationAlgorithm; begin new(P_burningJulia     (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_burningJulia2    =^T_burningJulia2    ;
     T_burningJulia2=object(T_burningJulia) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual;  end;
FUNCTION T_burningJulia2.getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);
    VAR x_re:double;
    begin
      if      x.re<-0.1 then x.re:=-x.re
      else if x.re< 0.1 then x.re:=x.re*x.re*(15-x.re*x.re*500);
      if      x.im<-0.1 then x.im:=-x.im
      else if x.im< 0.1 then x.im:=x.im*x.im*(15-x.im*x.im*500);
      x_re:=c.re+x.re*x.re-x.im*x.im;
      x.im:=c.im-2*x.re*x.im;
      x.re:=x_re;
    end;
  getRawDataAt_Body;
FUNCTION newBurningJulia2    :P_generalImageGenrationAlgorithm; begin new(P_burningJulia2    (result),create); end;
//------------------------------------------------------------------------------------------------------------------
TYPE P_burningJulia3    =^T_burningJulia3    ;
     T_burningJulia3=object(T_burningJulia) FUNCTION getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor; virtual; end;
FUNCTION T_burningJulia3.getRawDataAt(CONST xy:T_Complex):T_rgbFloatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);
    VAR x_re:double;
    begin
      x_re:=c.re+x.re*x.re-x.im*x.im;
      x.im:=2*x.re*x.im;
      if      x.im<-0.1 then x.im:=-x.im
      else if x.im< 0.1 then x.im:=x.im*x.im*(15-x.im*x.im*500);
      x.im:=c.im-x.im;
      x.re:=x_re;
    end;
  getRawDataAt_Body;
FUNCTION newBurningJulia3    :P_generalImageGenrationAlgorithm; begin new(P_burningJulia3    (result),create); end;
//------------------------------------------------------------------------------------------------------------------

INITIALIZATION
registerAlgorithm('Newton (3)'                  ,@newNewton3Algorithm ,true,true,false);
registerAlgorithm('Newton (5)'                  ,@newNewton5Algorithm ,true,true,false);
registerAlgorithm('Bump'                        ,@newBump             ,true,true,false);
registerAlgorithm('Diperiodic'                  ,@newDiperiodic       ,true,true,false);
registerAlgorithm('Exponential (A)'             ,@newExpoA            ,true,true,false);
registerAlgorithm('Exponential (B)'             ,@newExpoB            ,true,true,false);
registerAlgorithm('Expo-Cancel (A)'             ,@newExpoCancel5a     ,true,true,false);
registerAlgorithm('Expo-Cancel (B)'             ,@newExpoCancel5b     ,true,true,false);
registerAlgorithm('Freak Wave'                  ,@newFreakWave        ,true,true,true);
registerAlgorithm('ln-Taylor'                   ,@newLnTaylor         ,true,true,false);
registerAlgorithm('Logistic Equation'           ,@newLogisticEquation ,true,true,false);
registerAlgorithm('Logistic Equation derivative',@newLogisticEquation2,true,true,true);
registerAlgorithm('Power-4-Mandelbrot',@newMandelbrot_p4    ,true,true,true);
registerAlgorithm('Cosine'            ,@newMbCosine         ,true,true,true);
registerAlgorithm('1/Cosine'          ,@newMbCosine2        ,true,true,true);
registerAlgorithm('Nondivergent'      ,@newNondivergent     ,true,true,true);
registerAlgorithm('Parabola'          ,@newParabola         ,true,true,false);
registerAlgorithm('sin-Taylor'        ,@newSinTaylor        ,true,true,true);
registerAlgorithm('Sinus'             ,@newSinus            ,true,true,true);
registerAlgorithm('1/Sinus'           ,@newInvSinus         ,true,true,false);
registerAlgorithm('TUL I'             ,@newTul              ,true,true,true);
registerAlgorithm('TUL II'            ,@newTul2             ,true,true,false);
registerAlgorithm('TUL III'           ,@newTul3             ,true,true,true);
registerAlgorithm('TUL IV'            ,@newTul4             ,true,true,true);
registerAlgorithm('TUL V'             ,@newTul5             ,true,true,true);
registerAlgorithm('TUL Vi'            ,@newTul5i            ,true,true,true);
registerAlgorithm('Unnamed I'         ,@newUnnamed1         ,true,true,false);
registerAlgorithm('Unnamed II'        ,@newUnnamed2         ,true,true,false);
registerAlgorithm('Unnamed III'       ,@newUnnamed3         ,true,true,false);
registerAlgorithm('Weierstrass-4'     ,@newWeierstrass4     ,true,true,false);
registerAlgorithm('Weierstrass-6'     ,@newWeierstrass6     ,true,true,false);
registerAlgorithm('Mandelbrot / Julia',@newMandelbrot       ,true,true,true);
registerAlgorithm('Mandelbar  /-Julia',@newMandelbar        ,true,true,true);
registerAlgorithm('Burning Ship /-Julia'            ,@newBurningJulia ,true,true,true);
registerAlgorithm('Burning Ship /-Julia (interp. A)',@newBurningJulia2,true,true,true);
registerAlgorithm('Burning Ship /-Julia (interp. B)',@newBurningJulia3,true,true,true);
registerAlgorithm('Lyapunov'                        ,@newLyapunov     ,true,true,false);
registerAlgorithm('Sinc'                            ,@newSinc         ,true,true,true);
end.

