UNIT ig_circlespirals;
INTERFACE
USES myColors,
     myParams,
     complex,
     mypics,
     imageContexts,
     imageGeneration;

TYPE
  P_circleSpiralAlgorithm=^T_circleSpiralAlgorithm;
  T_circleSpiralAlgorithm=object(T_scaledImageGenerationAlgorithm)
    spiralParameter:longint;
    a,b,c,d:T_Complex;
    colorStyle:byte; //0: white circles; 1: white spheres; 2: bgColored circles; 3: bgColors spheres

    backgroundImage:P_rawImage;
    CONSTRUCTOR create;
    PROCEDURE cleanup; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE execute(CONST context:P_imageGenerationContext); virtual;
  end;

IMPLEMENTATION
USES darts,math;
TYPE T_circle=record
       center:T_Complex;
       radius:double;
       color:T_rgbFloatColor;
     end;
     T_circles=array of T_circle;

     P_spheresTodo=^T_spheresTodo;
     T_spheresTodo=object(T_parallelTask)
       chunkIndex:longint;
       circlesInRange:T_circles;
       drawSpheres:boolean;

       CONSTRUCTOR create(CONST allCircles:T_circles;
                          CONST spheres:boolean;
                          CONST chunkIndex_:longint;
                          CONST target_:P_rawImage);
       DESTRUCTOR destroy; virtual;
       PROCEDURE execute; virtual;
     end;

CONSTRUCTOR T_spheresTodo.create(CONST allCircles: T_circles;
                                 CONST spheres:boolean;
                                 CONST chunkIndex_: longint;
                                 CONST target_: P_rawImage);
  VAR x0:longint=0;
      y0:longint=0;
      i,j:longint;
      c,tmp:T_circle;
  begin
    chunkIndex :=chunkIndex_;
    drawSpheres:=spheres;
    for i:=0 to chunkIndex-1 do begin
      inc(x0,CHUNK_BLOCK_SIZE);
      if x0>=target_^.dimensions.width then begin
        x0:=0;
        inc(y0,CHUNK_BLOCK_SIZE);
      end;
    end;
    setLength(circlesInRange,1);
    i:=0;
    for c in allCircles do if (c.center.re+c.radius>x0-1) and (c.center.re-c.radius<=x0+CHUNK_BLOCK_SIZE+1) and
                              (c.center.im+c.radius>y0-1) and (c.center.im-c.radius<=y0+CHUNK_BLOCK_SIZE+1) then begin
      if i>=length(circlesInRange) then setLength(circlesInRange,i*2);
      circlesInRange[i]:=c;
      j:=i;
      while (j>0) and (circlesInRange[j-1].radius>circlesInRange[j].radius) do begin
        tmp                :=circlesInRange[j];
        circlesInRange[j  ]:=circlesInRange[j-1];
        circlesInRange[j-1]:=tmp;
        dec(j);
      end;
      inc(i);
    end;
    setLength(circlesInRange,i);
  end;

DESTRUCTOR T_spheresTodo.destroy;
  begin
    setLength(circlesInRange,0);
  end;

FUNCTION getColorForPixel(CONST ix,iy:double; CONST circle:T_circle; OUT hit:boolean):T_rgbFloatColor;
  VAR x,y,r2:double;
  begin
    x:=(ix-circle.center.re)/circle.radius;
    y:=(iy-circle.center.im)/circle.radius;
    r2:=system.sqr(x)+system.sqr(y);
    hit:=r2<1;
    if hit then begin
      r2:=sqrt(1-r2);
      x:=max(0,x*0.30151134457776363-
      y         *0.30151134457776363+
      r2        *0.90453403373329089);
      y:=x*x; y*=y; y*=y; y*=y; y*=y; y*=y; y*=y; y*=y;
      x:=(x*0.8+0.2)*(1-y);
      result:=circle.color*x+WHITE*y;
    end else result:=BLACK;
  end;

PROCEDURE T_spheresTodo.execute;
  FUNCTION getColorAt(CONST x,y:double):T_rgbFloatColor;
    VAR c:T_circle;
        col:T_rgbFloatColor;
        hit:boolean;
    begin
      result:=BLACK;
      if drawSpheres then for c in circlesInRange do begin
        col:=getColorForPixel(x,y,c,hit);
        if hit then exit(col);
      end else for c in circlesInRange do begin
        if system.sqr(x-c.center.re)+
           system.sqr(y-c.center.im)<
           system.sqr(  c.radius) then exit(c.color);
      end;
    end;

  VAR chunk:T_colChunk;
      i,j,k,k0,k1:longint;
  begin
    chunk.create;
    chunk.initForChunk(containedIn^.image.dimensions.width,containedIn^.image.dimensions.height,chunkIndex);

    for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do with chunk.col[i,j] do rest:=getColorAt(chunk.getPicX(i),chunk.getPicY(j));
    while chunk.markAlias(0.5) and not(containedIn^.cancellationRequested) do
    for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do with chunk.col[i,j] do if odd(antialiasingMask) then begin
      if antialiasingMask=1 then begin
        k0:=1;
        k1:=2;
        antialiasingMask:=2;
        k1:=2*k1;
      end else begin
        k0:=antialiasingMask-1;
        k1:=k0+2;
        if k1>254 then k1:=254;
        antialiasingMask:=k1;
        k0:=2*k0;
        k1:=2*k1;
      end;
      for k:=k0 to k1-1 do rest:=rest+getColorAt(
        chunk.getPicX(i)+darts_delta[k,0],
        chunk.getPicY(j)+darts_delta[k,1]);
    end;

    containedIn^.image.copyFromChunk(chunk);
    chunk.destroy;
  end;

CONSTRUCTOR T_circleSpiralAlgorithm.create;
  CONST colorStyleNames:array[0..11] of string=('white_circles',
        'white_spheres',
        'colored_circles',
        'colored_spheres',
        'col1_c',  //4
        'col1_s',
        'col2_c',
        'col2_s',
        'col3_c',
        'col3_s',
        'col4_c',
        'col4_s');   //11
  begin
    inherited create;
    addParameter('circleShift',pt_integer,2,100);
    addParameter('mb_a',pt_2floats);
    addParameter('mb_b',pt_2floats);
    addParameter('mb_c',pt_2floats);
    addParameter('mb_d',pt_2floats);
    addParameter('style',pt_enum,0,11)^.setEnumValues(colorStyleNames);
    resetParameters(0);
    backgroundImage:=nil;
  end;

PROCEDURE T_circleSpiralAlgorithm.cleanup;
  begin
    if backgroundImage<>nil then begin
      dispose(backgroundImage,destroy);
      backgroundImage:=nil;
    end;
  end;

PROCEDURE T_circleSpiralAlgorithm.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    spiralParameter:=20;
    a:=1;
    b:=0;
    c:=0;
    d:=1;
    colorStyle:=0;
  end;

FUNCTION T_circleSpiralAlgorithm.numberOfParameters: longint;
  begin result:=inherited numberOfParameters+6; end;

PROCEDURE T_circleSpiralAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: spiralParameter:=value.i0;
      1: begin a.re:=value.f0; a.im:=value.f1; end;
      2: begin b.re:=value.f0; b.im:=value.f1; end;
      3: begin c.re:=value.f0; c.im:=value.f1; end;
      4: begin d.re:=value.f0; d.im:=value.f1; end;
      5: colorStyle:=value.i0;
    end;
  end;

FUNCTION T_circleSpiralAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index));
    case byte(index-inherited numberOfParameters) of
      0: result.createFromValue(parameterDescription(inherited numberOfParameters+0),spiralParameter);
      1: result.createFromValue(parameterDescription(inherited numberOfParameters+1),a.re,a.im);
      2: result.createFromValue(parameterDescription(inherited numberOfParameters+2),b.re,b.im);
      3: result.createFromValue(parameterDescription(inherited numberOfParameters+3),c.re,c.im);
      4: result.createFromValue(parameterDescription(inherited numberOfParameters+4),d.re,d.im);
      5: result.createFromValue(parameterDescription(inherited numberOfParameters+5),colorStyle);
    end;
  end;

PROCEDURE T_circleSpiralAlgorithm.execute(CONST context:P_imageGenerationContext);

  CONST param:array[2..100,0..1] of double=(                                          (2.890053638263965 ,2.237035759287413 ),(1.623275178749378 ,1.6907405560884021),
    (1.3261093668522155,1.3462256354851088 ),(1.20405279586423,1.1147141326916035)   ,(1.1407625758592799,0.9497695138149853),(1.1033491647933114,0.82672953941405836),
    (1.0792665862591149,0.73160150060949669),(1.0628016392740884,0.65593587595930847),(1.051024740256571,0.5943548410772794 ),(1.0422991049360375,0.5432826604016967),
    (1.035648588678088 ,0.5002534505661441),(1.030460283227332,0.4635135784648558),(1.0263329383018096,0.43178282078031266),(1.0229946132207739,0.40410524404315346),
    (1.0202555402152162,0.3797529354590343),(1.0179799400923284,0.35816201387347046),(1.016068534264215,0.33888899324403488),(1.0144473423326079,0.321580343300751),
    (1.0130603042026645,0.3059508235127951),(1.0118643010755228,0.2917677806688046),(1.0108257219110692,0.27883958203659176),(1.0099180494989857,0.2670069686382692),
    (1.0091201336563977,0.2561365044317434),(1.0084149364328363,0.2461155524220271),(1.0077886072145843,0.23684837845989873),(1.0072297920665294,0.22825309836578739),
    (1.0067291118008954,0.22025926303716836),(1.0062787632004766,0.21280593136684717),(1.0058722112335006,0.20584011984966266),(1.0055039492573286,0.19931554575046298),
    (1.005169310555847,0.19319160101507252),(1.0048643190130482,0.18743250900237313),(1.0045855698945159,0.18200662715495808),(1.0043301339877715,0.17688586698612557),
    (1.0040954800093109,0.17204520899791645),(1.0038794114027734,0.1674622948943047),(1.003680014554451,0.16311708309922363),(1.0034956161267123,0.15899155640882917),
    (1.0033247477184275,0.15506947280358802),(1.0031661164479719,0.15133615216825733),(1.0030185803503195,0.14777829302714335),(1.0028811277079226,0.14438381448138915),
    (1.0027528596122279,0.14114171939713016),(1.0026329751910392,0.13804197558546344),(1.0025207590457168,0.13507541227402209),(1.0024155705281346,0.13223362962314275),
    (1.0023168345556681,0.1295089194091527),(1.0022240337170429,0.12689419529974677),(1.0021367014657017,0.12438293139525805),(1.0020544162327125,0.12196910791505963),
    (1.0019767963198702,0.11964716307862483),(1.0019034954569845,0.11741195037257052),(1.0018341989263919,0.11525870051342742),(1.001768620173384,0.11318298751507747),
    (1.001706497834123,0.11118069835338937),(1.0016475931232707,0.10924800579094819),(1.0015916875324147,0.10738134398447215),(1.0015385807977215,0.10557738654813209),
    (1.0014880891013989,0.10383302678911041),(1.0014400434767325,0.1021453598685848),(1.0013942883907556,0.10051166667282019),(1.0013506804823025,0.098929399206173718),
    (1.001309087436268,0.09739616734108089),(1.0012693869775244,0.09590972678022),(1.0012314659701707,0.09446796810345213),(1.0011952196096994,0.09306890678717744),
    (1.0011605506972763,0.09171067409689541),(1.0011273689867357,0.090391508765114148),(1.0010955905960746,0.0891097493767391),(1.001065137476266,0.0878638273927729),
    (1.0010359369311008,0.08665226075075172),(1.0010079211825289,0.08547364798706765),(1.0009810269766286,0.08432666283220126),(1.0009551952259388,0.08321004923503586),
    (1.0009303706843502,0.082122616777091179),(1.000906501651217,0.081063236441432249),(1.0008835397017348,0.08003083670469471),(1.0008614394409334,0.07902439992380426),
    (1.0008401582789714,0.07804295899177471),(1.0008196562256284,0.07708559423950148),(1.0007998957021624,0.07615143056270182),(1.0007808413688604,0.0752396347551432),
    (1.0007624599668121,0.07434941303107428),(1.000744720172578,0.073480008721403367),(1.0007275924645656,0.07263070012955458),(1.0007110490000444,0.07180079853424573),
    (1.0006950635018521,0.07098964632758608),(1.0006796111539158,0.070196615277898458),(1.0006646685048237,0.06942110490764161),(1.0006502133787389,0.06866254097763571),
    (1.0006362247930278,0.06792037406953215),(1.0006226828820279,0.06719407825920345),(1.0006095688264387,0.066483149874309019),(1.000596864787866,0.0657871063298622),
    (1.000584553848098,0.06510548503617741),(1.000572619952716,0.06443784237395003),(1.0005610478587024,0.06378375273176964),(1.0005498230857168,0.0631428076016146),
    (1.0005389318707527,0.06251461472832453));

  VAR circlesOfImage:array of T_circle;
      picWidth :longint;
      picHeight:longint;
  PROCEDURE initCircles;
    VAR p0,p1,r0:double;
        radiusThreshold:double;
        infPoint:T_Complex;
    FUNCTION getCircle(CONST index:longint):T_circle;
      VAR t0,t1,tc:T_Complex;
          mainAxis:T_Complex;
      begin
        case colorStyle of
          4,5: if (index mod spiralParameter+spiralParameter) mod spiralParameter=0
               then result.color:=RED
               else result.color:=BLUE;
          6,7: if (index mod spiralParameter+spiralParameter) mod spiralParameter mod 2=0
               then result.color:=RED
               else result.color:=BLUE;
          8,9: if (index mod spiralParameter+spiralParameter) mod spiralParameter mod 3=0
               then result.color:=RED
               else result.color:=BLUE;
          10,11: if (index mod spiralParameter+spiralParameter) mod spiralParameter mod 4=0
                   then result.color:=RED
                   else result.color:=BLUE;
          else result.color:=WHITE;
        end;

        //circle in spiral:
        result.radius:=exp(p0*index);
        result.center.re:=result.radius*system.cos(p1*index);
        result.center.im:=result.radius*system.sin(p1*index);
        result.radius*=r0;
        if isValid(result.center) and not(isNan(result.radius)) and not(isInfinite(result.radius)) and (sqrabs(result.center-infPoint)>system.sqr(result.radius)) then begin
          //Möbius transformation:
          mainAxis:=result.center-infPoint;
          mainAxis/=abs(mainAxis);
          t0:=result.center+result.radius*mainAxis; t0:=(a*t0+b)/(c*t0+d);
          t1:=result.center-result.radius*mainAxis; t1:=(a*t1+b)/(c*t1+d);
          tc:=(a*result.center+b)/(c*result.center+d);
          if isValid(t0) and isValid(t1) and isValid(tc) then begin
            result.center:=   (t0+t1)*0.5;
            result.radius:=abs(t1-t0)*0.5;
            //sanity check: the transformed center must be inside the transformed circle; otherwise the circle is "inside out"
            if sqrabs(result.center-tc)<system.sqr(result.radius) then begin
              result.center:=scaler.mrofsnart(result.center.re,result.center.im);
              result.radius*=1/abs(scaler.getAbsoluteZoom);
            end else result.radius:=0;
          end else result.radius:=0;
        end else result.radius:=0;
      end;

    FUNCTION colorOfCircle(CONST c:T_circle):T_rgbFloatColor;
      VAR ix,iy:longint;
          n:longint=0;
      begin
        result:=BLACK;
        for iy:=max(0          ,round(c.center.im-c.radius)) to
                min(picHeight-1,round(c.center.im+c.radius)) do
        for ix:=max(0          ,round(c.center.re-c.radius)) to
                min(picWidth-1 ,round(c.center.re+c.radius)) do
        if (system.sqr(ix-c.center.re)+system.sqr(iy-c.center.im))<system.sqr(c.radius+1) then begin
          result+=context^.image.pixel[ix,iy];
          n+=1;
        end;
        if n>0 then result:=result*(1/n);
      end;

    VAR fill:longint=0;
    FUNCTION addCircle(CONST c:T_circle):boolean;
      begin
        //reject circle:
        if (isValid(c.center)) and
           (not(isNan(c.radius))) and
           (not(isInfinite(c.radius))) and
           (c.radius>radiusThreshold) and
           (c.center.re+c.radius>=0) and
           (c.center.re-c.radius<picWidth) and
           (c.center.im+c.radius>=0) and
           (c.center.im-c.radius<picHeight) then begin
          if fill>=length(circlesOfImage) then setLength(circlesOfImage,round(1.5*fill));
          circlesOfImage[fill]:=c;
          inc(fill);
          result:=true;
        end else result:=false;
      end;

    VAR i:longint;
    begin
      //Init constants:
      if context^.previewQuality
      then radiusThreshold:=0.5
      else radiusThreshold:=0.01;
      p0:=param[spiralParameter,0];
      p1:=param[spiralParameter,1];
      r0:=complex.abs(p0*system.cos(p1)-1+
                      p0*system.sin(p1)*II)/(1+p0);
      p0:=system.ln(p0);
      picWidth :=context^.image.dimensions.width;
      picHeight:=context^.image.dimensions.height;

      //möbius: T(x  )=(a*x+b)/(c*x+d)
      //        T(0  )=b/d
      //        T(inf)=a/c
      //        inf         =  (a*x+b)/(c*x+d)
      //        inf*(c*x+d) = (a*x+b)
      //             c*x+d  = 0
      //                 x  = -d/c
      infPoint:=-1*d/c;
      if not(isValid(infPoint)) then infPoint:=0;

      //initialize circles
      setLength(circlesOfImage,10);
      for i:=-100000 to 100000 do addCircle(getCircle(i));
      setLength(circlesOfImage,fill);
      //color circles (if appropriate)
      if colorStyle in [2,3] then for i:=0 to fill-1 do circlesOfImage[i].color:=colorOfCircle(circlesOfImage[i]);
    end;

  PROCEDURE quickDrawCircles;
    VAR c:T_circle;
        ix,iy:longint;
    begin
      for c in circlesOfImage do begin
        for iy:=max(0          ,round(c.center.im-c.radius)) to
                min(picHeight-1,round(c.center.im+c.radius)) do
        for ix:=max(0          ,round(c.center.re-c.radius)) to
                min(picWidth-1 ,round(c.center.re+c.radius)) do
        if (system.sqr(ix-c.center.re)+system.sqr(iy-c.center.im))<system.sqr(c.radius) then context^.image[ix,iy]:=c.color;
      end;
    end;

  PROCEDURE quickDrawSpheres;
    VAR c:T_circle;
        ix,iy:longint;
        hit:boolean;
        col:T_rgbColor;
    begin
      for c in circlesOfImage do begin
        for iy:=max(0          ,round(c.center.im-c.radius)) to
                min(picHeight-1,round(c.center.im+c.radius)) do
        for ix:=max(0          ,round(c.center.re-c.radius)) to
                min(picWidth-1 ,round(c.center.re+c.radius)) do begin
          col:=getColorForPixel(ix,iy,c,hit);
          if hit then context^.image[ix,iy]:=col;
        end;
      end;
    end;

  VAR i:longint;
      todo:P_spheresTodo;
  begin with context^ do begin
    scaler.rescale(image.dimensions.width,image.dimensions.height);
    initialize(circlesOfImage);
    initCircles;
    //TODO: Implement clear method
    for i:=0 to image.pixelCount-1 do image.rawData[i]:=BLACK;
    if previewQuality then begin
      if odd(colorStyle)
      then quickDrawSpheres
      else quickDrawCircles;
    end else begin
      clearQueue;
      scaler.rescale(image.dimensions.width,image.dimensions.height);
      scalerChanagedSinceCalculation:=false;
      image.markChunksAsPending;
      for i:=0 to image.chunksInMap-1 do begin
        new(todo,create(circlesOfImage,odd(colorStyle),i,@image));
        enqueue(todo);
      end;
      waitForFinishOfParallelTasks;
    end;
  end; end;

FUNCTION newCircleSpiralAlgorithm:P_generalImageGenrationAlgorithm;
  begin
    new(P_circleSpiralAlgorithm(result),create);
  end;

INITIALIZATION
  registerAlgorithm('Circle spiral',@newCircleSpiralAlgorithm,true,false,false);

end.

