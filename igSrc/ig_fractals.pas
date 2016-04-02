UNIT ig_fractals;
INTERFACE
USES imageGeneration,mypics,myColors,complex,myParams,math,mySys,sysutils,myTools,myGenerics;
CONST LIGHT_NORMAL_INDEX=10;
CONST JULIA_COORD_INDEX=12;
TYPE

  { T_iteration }

  P_functionPerPixelViaRawDataAlgorithm=^T_functionPerPixelViaRawDataAlgorithm;
  T_functionPerPixelViaRawDataAlgorithm=object(T_functionPerPixelAlgorithm)
    temporaryRawMap:P_rawImage;
    rawMapIsOutdated:longint;

    maxDepth    :longint;
    colorSource :byte;
    colorStyle  :byte;
    colorVariant:byte;
    pseudoGamma :double;
    lightNormal :T_floatColor;

    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    PROCEDURE cleanup; virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION lightIsRelevant:boolean;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual; abstract;
    FUNCTION getColor(CONST rawData:T_floatColor):T_floatColor;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual;
    PROCEDURE prepareRawMap(CONST my:longint); virtual;
    FUNCTION prepareImage(CONST forPreview:boolean=false; CONST waitForFinish:boolean=false):boolean; virtual;
  end;

  P_rawDataWorkerThreadTodo=^T_rawDataWorkerThreadTodo;
  T_rawDataWorkerThreadTodo=object(T_queueToDo)
    algorithm:P_functionPerPixelViaRawDataAlgorithm;
    y:longint;

    CONSTRUCTOR create(CONST algorithm_:P_functionPerPixelViaRawDataAlgorithm; CONST y_:longint);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

  { T_newton3Algorithm }

  T_newton3Algorithm=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_newton5Algorithm=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_bump=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_diperiodic=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_expoA=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_expoB=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_expoCancel5a=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_expoCancel5b=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_freakWave=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_lnTaylor=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_logisticEquation=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_logisticEquation2=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_mandelbrot_p4=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_mbCosine=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_mbCosine2=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_nondivergent=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_parabola=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_sinTaylor=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_sinus=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_tul=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_tul2=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_tul3=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_tul4=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_unnamed1=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_unnamed2=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_weierstrass4=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

  T_weierstrass6=object(T_functionPerPixelViaRawDataAlgorithm)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
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

  T_mandelbrot=object(T_functionPerPixelViaRawDataJuliaAlgorithm)
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;  end;

  T_mandelbar=object(T_functionPerPixelViaRawDataJuliaAlgorithm)
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;  end;

  T_burningJulia=object(T_functionPerPixelViaRawDataJuliaAlgorithm)
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;  end;

  T_burningJulia2=object(T_burningJulia)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;  end;

  T_burningJulia3=object(T_burningJulia)
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION getRawDataAt(CONST xy:T_Complex):T_floatColor; virtual;
  end;

FUNCTION toSphere(CONST x:T_Complex):T_floatColor; inline;
IMPLEMENTATION

{ T_iteration }

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
    algorithm^.prepareRawMap(y);
    progressQueue.logStepDone;
  end;

CONSTRUCTOR T_functionPerPixelViaRawDataAlgorithm.create;
  begin
    inherited create;
    temporaryRawMap:=nil;
    rawMapIsOutdated:=64;
    addParameter('depth',pt_integer,0);
    addParameter('source',pt_enum,0,9,
      'final',
      'average',
      'floating_average',
      'path chaos',
      'final angle',
      'steps to divergence',
      'avg. chaos',
      'avg.sqr.chaos',
      'max.chaos',
      'normal');
    addParameter('style',pt_enum,0,9,
      'fire / metal',
      'water / glass',
      'spectrum / plastic',
      'trafficLight / fire',
      'earth / drugged',
      'greyscale / gold',
      'zebra / levels',
      'greenzebra / strange',
      'rainbow / window',
      'discrete / line');
    addParameter('variant',pt_enum,0,3,
      'direct',
      'inverted',
      'parabola',
      'inv.parabola');
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
    lightNormal :=newColor(1,1,2)*(1/system.sqrt(6));
    rawMapIsOutdated:=64;
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.cleanup;
  begin
    progressQueue.ensureStop;
    if temporaryRawMap<>nil then dispose(temporaryRawMap,destroy);
    temporaryRawMap:=nil;
    rawMapIsOutdated:=64;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+6;
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  FUNCTION normedVector(CONST x:T_floatColor):T_floatColor;
    VAR len:double;
    begin
      len:=sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
      if len<1E-10 then result:=newColor(0,0,1)
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
      5: result:=parValue(index,lightNormal);
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.lightIsRelevant:boolean;
  begin
    result:=colorSource>=9;
  end;

FUNCTION toSphere(CONST x:T_Complex):T_floatColor; inline;
  VAR t:double;
  begin
    if not(isValid(x)) then exit(black);
    t:=4/(4+x.re*x.re+x.im*x.im);
    result[0]:=x.re*t;
    result[1]:=x.im*t;
    result[2]:=t*2;
  end;

{$MACRO ON}
{$define getRawDataAt_Body:=  CONST h0:T_Complex=(re: -1/3; im: -1/3);
        h1:T_Complex=(re:  2/3; im: -1/3);
        h2:T_Complex=(re: -1/3; im:  2/3);

  FUNCTION toSphereZ(CONST x:T_Complex):double; inline;
    begin
      if not(isValid(x)) then exit(0);
      result:=8/(4+x.re*x.re+x.im*x.im);
    end;

  FUNCTION dist  (CONST x,y:T_floatColor):double; inline; begin result:=sqrt(system.sqr(x[0]-y[0])+system.sqr(x[1]-y[1])+system.sqr(x[2]-y[2])); end;
  FUNCTION sqDist(CONST x,y:T_floatColor):double; inline; begin result:=     system.sqr(x[0]-y[0])+system.sqr(x[1]-y[1])+system.sqr(x[2]-y[2]) ; end;

  VAR i:longint;
      x,x1,x2,
      c,c1,c2:T_Complex;
      r,s,v:T_floatColor;
      d0,d1,d2:double;
      sphereZ:double;
  begin
    c:=xy;
    i:=0;
    result:=black;
    s:=black;
    v:=black;
    case colorSource of
      0..2: begin
        iterationStart(c,x);
        while (i<maxDepth) and (isValid(x)) do begin
          iterationStep(c,x);
          sphereZ:=toSphereZ(x);
          result[1]:=result[1]+sphereZ;
          result[2]:=result[2]*0.95+0.05*sphereZ;
          inc(i);
        end;
        sphereZ:=toSphereZ(x);
        result[0]:=0.5*sphereZ;
        result[1]:=result[1]+(maxDepth-i)*sphereZ;
        result[1]:=0.5*(result[1]*(1/maxDepth));
        sphereZ:=sphereZ*0.05;
        while (i<maxDepth) do begin result[2]:=result[2]*0.95+sphereZ; inc(i); end;
        result[2]:=0.5*result[2];
      end;
      3..5: begin
        iterationStart(c,x);
        while (i<maxDepth) and (x.re*x.re+x.im*x.im<1E6) do begin
          iterationStep(c,x);
          r:=toSphere(x);
          s:=s+r;
          v:=v+newColor(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
          inc(i);
        end;
        result[2]:=i/maxDepth;
        while (i<maxDepth) and (x.re*x.re+x.im*x.im<1E10) do begin
          iterationStep(c,x);
          r:=toSphere(x);
          s:=s+r;
          v:=v+newColor(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
          inc(i);
        end;
        result[1]:=arg(x)/(2*pi); if result[1]<0 then result[1]:=result[1]+1;
        while (i<maxDepth) and isValid(x) do begin
          iterationStep(c,x);
          r:=toSphere(x);
          s:=s+r;
          v:=v+newColor(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
          inc(i);
        end;
        s:=s+(maxDepth-i)*r;
        v:=v+(maxDepth-i)*newColor(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
        result[0]:=((v[0]+v[1]+v[2])/maxDepth-(s*s)*(1/(maxDepth*maxDepth)));
      end;
      6..8 : begin
        c :=xy+1/(scaler.getZoom*1414)*h0; iterationStart(c ,x ); r:=black;
        c1:=xy+1/(scaler.getZoom*1414)*h1; iterationStart(c1,x1); s:=black;
        c2:=xy+1/(scaler.getZoom*1414)*h2; iterationStart(c2,x2); v:=black;
        while isValid(x) and isValid(x1) and isValid(x2) and (i<maxDepth) do begin
          iterationStep(c ,x ); r:=r*0.9+0.1*toSphere(x );
          iterationStep(c1,x1); s:=s*0.9+0.1*toSphere(x1);
          iterationStep(c2,x2); v:=v*0.9+0.1*toSphere(x2);
          inc(i);
        end;
        while (i<maxDepth) do begin
          r:=r*0.9+0.1*toSphere(x );
          s:=s*0.9+0.1*toSphere(x1);
          v:=v*0.9+0.1*toSphere(x2);
          inc(i);
        end;
        result[0]:=(dist(r,s)+dist(s,v)+dist(r,v))*0.166666666666667 ;
        result[1]:=sqrt(sqDist(r,s)+sqDist(s,v)+sqDist(r,s))*0.288675134594813;
        result[2]:=max(dist(r,s),max(dist(s,v),dist(r,v)))*0.5;
      end;
      else begin
        c :=xy+h0*scaler.getAbsoluteZoom; iterationStart(c ,x ); d0:=0;
        c1:=xy+h1*scaler.getAbsoluteZoom; iterationStart(c1,x1); d1:=0;
        c2:=xy+h2*scaler.getAbsoluteZoom; iterationStart(c2,x2); d2:=0;

        i:=0;
        while (i<maxDepth) do begin
          if isValid(x)  then begin iterationStep(c ,x ); d0:=d0+toSphereZ(x);  end;
          if isValid(x1) then begin iterationStep(c1,x1); d1:=d1+toSphereZ(x1); end;
          if isValid(x2) then begin iterationStep(c2,x2); d2:=d2+toSphereZ(x2); end;
          inc(i);
        end;


        //compute and normalize normal vector:-----------------//
        d0:=sqrt(d0/maxDepth)*pseudoGamma*(1-2*(colorVariant and 1));
        d1:=sqrt(d1/maxDepth)*pseudoGamma*(1-2*(colorVariant and 1))-d0;
        d2:=sqrt(d2/maxDepth)*pseudoGamma*(1-2*(colorVariant and 1))-d0;
        d0:=1/sqrt(d1*d1+d2*d2+sqrabs(scaler.getAbsoluteZoom));//
        result[2]:=(abs(scaler.getAbsoluteZoom)*d0);           //
        result[0]:=(d1                         *d0);           //
        result[1]:=(d2                         *d0);           //
        //-------------------:compute and normalize normal vector
      end;
    end;
  end}
//FUNCTION T_functionPerPixelViaRawDataAlgorithm.getRawDataAt(CONST xy: T_Complex): T_floatColor;
//  getRawDataAt_Body;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getColor(CONST rawData: T_floatColor): T_floatColor;
  FUNCTION fire(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0,0,0)
      else if x<1/3 then result:=newColor(3*x,0,0)
      else if x<2/3 then result:=newColor(1,3*x-1,0)
      else if x<1   then result:=newColor(1,1,3*x-2)
      else result:=newColor(1,1,1);
    end;

  FUNCTION water(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0,0,0)
      else if x<1/2 then result:=newColor(0,0,2*x)
      else if x<1   then result:=newColor(2*x-1,2*x-1,1)
      else result:=newColor(1,1,1);
    end;

  FUNCTION spectrum(x:double):T_floatColor; inline;
    begin
      x:=x*6;
      if      x<0   then result:=newColor(0,0,0)
      else if x<0.5 then result:=newColor(2*x,0,0)
      else if x<1.5 then result:=newColor(1,x-0.5,0)
      else if x<2.5 then result:=newColor(1-(x-1.5),1,0)
      else if x<3.5 then result:=newColor(0,1,x-2.5)
      else if x<4.5 then result:=newColor(0,1-(x-3.5),1)
      else if x<5.5 then result:=newColor(x-4.5,0,1)
      else if x<6   then result:=newColor(1-2*(x-5.5),0,1-2*(x-5.5))
      else               result:=newColor(0,0,0);
    end;

  FUNCTION trafficLight(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0.8,0,0)
      else if x<1/2 then result:=newColor(1,2*x,0)
      else if x<1   then result:=newColor(1-(2*x-1),1,0)
      else result:=newColor(0,0.8,0);
    end;

  FUNCTION earth(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0,0,0)
      else if x<1/3 then result:=newColor(3*x,1.5*x,0)
      else if x<2/3 then result:=newColor(  1-0.3*(3*x-1),0.5+0.2*(3*x-1),0.7*(3*x-1))
      else if x<1   then result:=newColor(0.7+0.3*(3*x-2),0.7+0.3*(3*x-2),0.7+0.3*(3*x-2))
      else result:=newColor(1,1,1);
    end;

  FUNCTION greyscale(x:double):T_floatColor; inline;
    begin
      if x<0 then result:=newColor(0,0,0)
      else if x<1   then result:=newColor(x,x,x)
      else result:=newColor(1,1,1);
    end;

  FUNCTION zebra(x:double):T_floatColor; inline;
    VAR q:longint;
    begin
      if x<0 then x:=0 else if x>1 then x:=1;
      q:=round(128*x);
      if odd(q) then result:=newColor(0,0,0)
                else result:=newColor(1,1,1);
    end;

  FUNCTION greenzebra(x:double):T_floatColor; inline;
    VAR q:longint;
    begin
      if x<0 then x:=0 else if x>1 then x:=1;
      q:=round(128*x);
      if odd(q) then result:=newColor(0,x,0)
                else result:=newColor(1-x,1-x,1-x);
    end;

  FUNCTION rainbow(x:double):T_floatColor; inline;
    begin
      if x<0 then x:=0
      else if x>1 then x:=6
      else x:=6*x;
      if      x<1 then result:=newColor(1  ,x  ,0  )
      else if x<2 then result:=newColor(2-x,1  ,0  )
      else if x<3 then result:=newColor(0  ,1  ,x-2)
      else if x<4 then result:=newColor(0  ,4-x,1  )
      else if x<5 then result:=newColor(x-4,0  ,1  )
      else             result:=newColor(1  ,0  ,6-x);
    end;

  FUNCTION discrete(x:double):T_floatColor; inline;
    VAR i:byte;
    begin
      if      x<0 then i:=0
      else if x>1 then i:=15
                  else i:=byte(floor(x*16));
      case i of
        0: result:=newColor(0,0,0);
        1: result:=newColor(0.5,0,0);
        2: result:=newColor(1,0,0);
        3: result:=newColor(1,0.25,0);
        4: result:=newColor(1,0.5,0);
        5: result:=newColor(0.9,0.65,0);
        6: result:=newColor(0.8,0.8,0);
        7: result:=newColor(0.4,0.9,0);
        8: result:=newColor(0,1,0);
        9: result:=newColor(0,0.9,0.4);
       10: result:=newColor(0,0.8,0.8);
       11: result:=newColor(0,0.4,0.9);
       12: result:=newColor(0,0,1);
       13: result:=newColor(0.5,0,1);
       14: result:=newColor(1,0,1);
      else result:=newColor(1,1,1);
      end;
    end;

  FUNCTION normalToColor_metal(CONST n:T_floatColor):T_floatColor;
    VAR diffuse,spec4,spec8,spec16:single;
    CONST colAmb   :T_floatColor=(0.1,0.1,0.1);
          colDiff  :T_floatColor=(0.3,0.3,0.3);
          colSpec05:T_floatColor=(0.6,0.0,0.0);
          colSpec08:T_floatColor=(0.0,0.6,0.0);
          colSpec16:T_floatColor=(0.0,0.0,0.6);

    begin
      diffuse :=lightNormal*n;
      spec4:=-(lightNormal[2]-2*n[2]*diffuse);
      if spec4<0 then begin
      result:=colAmb+colDiff*diffuse;
      end else begin
        spec4 :=spec4*spec4;
        spec4 :=spec4*spec4;
        spec8 :=spec4*spec4;
        spec16:=spec8*spec8;
        result:=colAmb+colDiff*diffuse
                      +colSpec05*spec4
                      +colSpec08*spec8
                      +colSpec16*spec16;
      end;

    end;

  FUNCTION normalToColor_glass(CONST n:T_floatColor):T_floatColor;
    VAR diffuse,spec:single;
    CONST colAmb   :T_floatColor=(0.0,0.0,0.2);
          colDiff  :T_floatColor=(0.0,0.0,0.5);
          colSpec  :T_floatColor=(0.8,0.8,0.8);
    begin
      diffuse :=lightNormal*n;
      spec:=-(lightNormal[2]-2*n[2]*diffuse);
      if spec<0.999 then begin
      result:=colAmb+colDiff*diffuse;
      end else begin
        result:=colAmb+colDiff*diffuse
                      +colSpec*(spec-0.999)*1000;
      end;
    end;

  FUNCTION normalToColor_drugged(CONST n:T_floatColor):T_floatColor;
    CONST colAmb   :T_floatColor=(0.3,0.3,0.3);
          colDiff  :T_floatColor=(0.3,0.3,0.3);
    VAR spec,diffuse:single;
    begin
      diffuse :=lightNormal*n;
      spec:=-(lightNormal[2]-2*n[2]*diffuse);
      if spec<1 then
      result:=colAmb+colDiff*diffuse+spec*hue(spec)
      else result:=black;
    end;

  FUNCTION normalToColor_plastic(CONST n:T_floatColor):T_floatColor;
    CONST colAmb   :T_floatColor=(0.5,0.0,0.0);
          colDiff  :T_floatColor=(0.5,0.0,0.0);
          colSpec  :T_floatColor=(0.0,1,1);
    VAR spec,diffuse:single;
    begin
      diffuse :=lightNormal*n;
      spec:=-(lightNormal[2]-2*n[2]*diffuse);
      if spec<0 then result:=colAmb+colDiff*diffuse else begin
        spec:=spec*spec;
        spec:=spec*spec;
        spec:=spec*spec;
        result:=colAmb+colDiff*diffuse+colSpec*spec;
      end;
    end;

  FUNCTION normalToColor_fire(CONST n:T_floatColor):T_floatColor;
    VAR x:single;
    begin
      x:=(lightNormal*n);
      x:=1.5+1.5*x;//(lightNormal[2]-2*n[2]*x);
      if      x>2 then begin result[0]:=1; result[1]:=1;   result[2]:=x-2; end
      else if x>1 then begin result[0]:=1; result[1]:=x-1; result[2]:=0;   end
      else             begin result[0]:=x; result[1]:=0;   result[2]:=0;   end;
    end;

  FUNCTION normalToColor_gold(CONST n:T_floatColor):T_floatColor;
    CONST colAmb   :T_floatColor=(0.5,0.25, 0);
          colDiff  :T_floatColor=(0.5,0.5,0);
          colSpec  :T_floatColor=(0.5,0.25,0.5);
    VAR spec,diffuse:single;
    begin
      diffuse :=lightNormal*n;
      spec:=-(lightNormal[2]-2*n[2]*diffuse);
      if (spec<0) then result:=colAmb+colDiff*diffuse else begin
        spec:=spec*spec;
        spec:=spec*spec;
        spec:=spec*spec;
        result:=projectedColor(colAmb+colDiff*diffuse+colSpec*spec);
      end;
    end;

  FUNCTION normalToColor_levels(CONST n:T_floatColor):T_floatColor;
    begin
      result:=white*(round(5+5*(lightNormal*n))*0.1);
    end;

  FUNCTION normalToColor_window(n:T_floatColor):T_floatColor;
    CONST colAmbient:T_floatColor=(0.1,0.05,0.0);
          colDiffuse:T_floatColor=(0.4,0.20,0.0);

    VAR diffuse:single;
        specVec:T_floatColor;
    begin
      diffuse:=lightNormal*n;
      specVec:=lightNormal-n*(2*diffuse);
      result:=colAmbient+diffuse*colDiffuse;
      if (specVec[2]<0) then begin
        if (abs(specVec[1])<0.2 ) and (abs(specVec[0])<0.2 ) and
           (abs(specVec[1])>0.02) and (abs(specVec[0])>0.02) then result:=result+white*0.75
        else result:=result+white*(0.5*specVec[2]*specVec[2]);
      end;
    end;

  FUNCTION normalToColor_line(CONST n:T_floatColor):T_floatColor;
    CONST colAmbient:T_floatColor=(0.0,0.3,0.0);
          colDiffuse:T_floatColor=(0.0,0.3,0.0);

    VAR diffuse:single;
        specVec:T_floatColor;
    begin
      diffuse:=lightNormal*n;
      specVec:=lightNormal-n*(2*diffuse);
      result:=colAmbient+diffuse*colDiffuse;
      if (specVec[2]<0) then begin
        specVec[1]:=specVec[1]+specVec[0];
        if (abs(specVec[1])<0.02) then result:=result-white*specVec[2]
        else result:=result+white*(0.7*specVec[2]*specVec[2]*(1-specVec[1]));
      end;
    end;

  FUNCTION normalToColor_strange(CONST n:T_floatColor):T_floatColor;
    CONST colAmbient:T_floatColor=(0.1,0.1,0.1);
          colDiffuse:T_floatColor=(0.4,0.4,0.4);

          vec1:T_floatColor=( 0                  , 0.18257418276333211,0.98319208082057976);
          vec2:T_floatColor=( 0.15811387502784748,-0.09128710059683582,0.98319208082057976);
          vec3:T_floatColor=(-0.15811389098898917,-0.0912870729513256 ,0.98319208082057976);
    VAR diffuse:single;
        specVec,spec:T_floatColor;

    begin
      diffuse:=lightNormal*n;
      specVec:=lightNormal-n*(2*diffuse);
      spec[0]:=specVec*vec1; if spec[0]>0 then spec[0]:=0;
      spec[1]:=specVec*vec2; if spec[1]>0 then spec[1]:=0;
      spec[2]:=specVec*vec3; if spec[2]>0 then spec[2]:=0;
      spec[0]:=spec[0]*spec[0]; spec[0]:=spec[0]*spec[0];
      spec[1]:=spec[1]*spec[1]; spec[1]:=spec[1]*spec[1];
      spec[2]:=spec[2]*spec[2]; spec[2]:=spec[2]*spec[2];
      result[0]:=colAmbient[0]+diffuse*colDiffuse[0]+spec[0]*spec[0]*0.5;
      result[1]:=colAmbient[1]+diffuse*colDiffuse[1]+spec[1]*spec[1]*0.5;
      result[2]:=colAmbient[2]+diffuse*colDiffuse[2]+spec[2]*spec[2]*0.5;
    end;

  VAR aid:double;
  begin
    if colorSource<9 then begin
      case colorVariant of
        0: aid:=                 rawData[colorSource mod 3] ;
        1: aid:=1-               rawData[colorSource mod 3] ;
        2: aid:=  system.sqr(1-2*rawData[colorSource mod 3]);
        3: aid:=1-system.sqr(1-2*rawData[colorSource mod 3]);
      end;
      aid:=system.exp(system.ln(aid)*pseudoGamma);
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

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getColorAt(CONST ix,iy: longint; CONST xy: T_Complex): T_floatColor;
  begin
    result:=getColor(getRawDataAt(xy));
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.prepareRawMap(CONST my: longint);
  VAR y,x:longint;
      dat:T_floatColor;
  begin
    for y:=0 to temporaryRawMap^.height-1 do if y and 63=my then
    for x:=0 to temporaryRawMap^.width-1 do begin
      dat:=getRawDataAt(scaler.transform(x,y));
      temporaryRawMap^[x,y]:=dat;
      generationImage^[x,y]:=getColor(dat);
    end;
    interlockedDecrement(rawMapIsOutdated);
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.prepareImage(CONST forPreview: boolean; CONST waitForFinish:boolean=false):boolean;
  VAR x,y:longint;
  FUNCTION todo(CONST y:longint):P_rawDataWorkerThreadTodo;
    begin new(result,create(@self,y)); end;

  begin
    progressQueue.ensureStop;
    scaler.rescale(generationImage^.width,generationImage^.height);
    result:=false;
    if forPreview then begin
      if scalerChanagedSinceCalculation or
         (temporaryRawMap=nil) or
         (temporaryRawMap^.width<>generationImage^.width) or
         (temporaryRawMap^.height<>generationImage^.height) then rawMapIsOutdated:=64;
      if temporaryRawMap=nil then new(temporaryRawMap,create(generationImage^.width,generationImage^.height));
      temporaryRawMap^.resize(generationImage^.width,generationImage^.height, res_dataResize);
      if rawMapIsOutdated>0 then begin
        scalerChanagedSinceCalculation:=false;
        progressQueue.forceStart(et_stepCounter_parallel,64);
        rawMapIsOutdated:=64;
        for y:=0 to 63 do progressQueue.enqueue(todo(y));
        if waitForFinish then begin
          repeat until not(progressQueue.activeDeqeue);
          progressQueue.waitForEndOfCalculation;
          exit(true);
        end;
      end else begin
        for y:=0 to generationImage^.height-1 do for x:=0 to generationImage^.width-1 do
        generationImage^[x,y]:=getColor(temporaryRawMap^[x,y]);
        progressQueue.logEnd;
        exit(true);
      end;
    end else begin
      if temporaryRawMap<>nil then begin
        dispose(temporaryRawMap,destroy);
        temporaryRawMap:=nil;
      end;
      exit(inherited prepareImage(false,waitForFinish));
    end;
  end;

FUNCTION T_newton3Algorithm.getAlgorithmName: ansistring; begin result:='Newton (3)'; end;
FUNCTION T_newton3Algorithm.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=(2/3)*x+1/(3*sqr(x)); end;
  getRawDataAt_Body;

FUNCTION T_newton5Algorithm.getAlgorithmName: ansistring; begin result:='Newton (5)'; end;
FUNCTION T_newton5Algorithm.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=(4/5)*x+1/(5*sqr(sqr(x))); end;
  getRawDataAt_Body;

FUNCTION T_bump.getAlgorithmName: ansistring; begin result:='Bump'; end;
FUNCTION T_bump.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; c.re:=system.sin(x.re+system.exp(abs(x))*x.im); c.im:=system.sin(x.im-system.exp(abs(x))*x.re); c:=c*system.exp(abs(x)); x:=0; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=sqr(sqr(x))+c; end;
  getRawDataAt_Body;

FUNCTION T_diperiodic.getAlgorithmName: ansistring; begin result:='Diperiodic'; end;
  FUNCTION T_diperiodic.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x.re:=2*system.cos(x.re*0.5); x.im:=2*system.cos(x.im*0.5); x:=exp(x); end;
  getRawDataAt_Body;

FUNCTION T_expoA.getAlgorithmName: ansistring; begin result:='Exponential (A)'; end;
FUNCTION T_expoA.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=0; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=exp(c*x); end;
  getRawDataAt_Body;

FUNCTION T_expoB.getAlgorithmName: ansistring; begin result:='Exponential (B)'; end;
FUNCTION T_expoB.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=exp(x); end;
  getRawDataAt_Body;

FUNCTION T_expoCancel5a.getAlgorithmName: ansistring; begin result:='Expo-Cancel (A)'; end;
FUNCTION T_expoCancel5a.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=ln(1+x*(1+0.5*x*(1+(1/3)*x*(1+0.25*x)))); end;
  getRawDataAt_Body;

FUNCTION T_expoCancel5b.getAlgorithmName: ansistring; begin result:='Expo-Cancel (B)'; end;
FUNCTION T_expoCancel5b.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=ln(x); x:=(1+x*(1+0.5*x*(1+(1/3)*x*(1+0.25*x)))); end;
  getRawDataAt_Body;

FUNCTION T_freakWave.getAlgorithmName: ansistring; begin result:='Freak Wave'; end;
FUNCTION T_freakWave.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=x.re*system.cos(x.re+x.im)+c; end;
  getRawDataAt_Body;

FUNCTION T_lnTaylor.getAlgorithmName: ansistring; begin result:='ln-Taylor'; end;
FUNCTION T_lnTaylor.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=x-1+c; x:=x*(1-x*(1/2-x*(1/3-x*(1/4-x*(1/5-x*(1/6)))))); end;
  getRawDataAt_Body;

FUNCTION T_logisticEquation.getAlgorithmName: ansistring; begin result:='Logistic Equation'; end;
FUNCTION T_logisticEquation.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT x: T_Complex); inline; begin x:=0.5; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=c*x*(1-x); end;
  getRawDataAt_Body;

FUNCTION T_logisticEquation2.getAlgorithmName: ansistring; begin result:='Logistic Equation derivative'; end;
FUNCTION T_logisticEquation2.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline; begin x:=c*sqr(1-x)/x; end;
  getRawDataAt_Body;

FUNCTION T_mandelbrot_p4.getAlgorithmName: ansistring; begin result:='Power-4-Mandelbrot'; end;
FUNCTION T_mandelbrot_p4.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline;begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;begin x:=sqr(sqr(x))+c; end;
  getRawDataAt_Body;

FUNCTION T_mbCosine.getAlgorithmName: ansistring; begin result:='Cosine'; end;
FUNCTION T_mbCosine.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline;begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;begin x:=c*cos(x); end;
  getRawDataAt_Body;

FUNCTION T_mbCosine2.getAlgorithmName: ansistring; begin result:='1/Cosine'; end;
FUNCTION T_mbCosine2.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline;begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;begin x:=c/cos(x); end;
  getRawDataAt_Body;

FUNCTION T_nondivergent.getAlgorithmName: ansistring; begin result:='Nondivergent'; end;
FUNCTION T_nondivergent.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline;begin x:=1; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;begin x:=1/sqr(x)+c; end;
  getRawDataAt_Body;

FUNCTION T_parabola.getAlgorithmName: ansistring; begin result:='Parabola'; end;
FUNCTION T_parabola.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex); inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex); inline;
    VAR tmp:double;
    begin
      tmp:=((x.re-1)*(x.re-1)+x.im)/abs(x);
      x.im:=-x.im*tmp;
      x.re:= x.re*tmp;
    end;
  getRawDataAt_Body;

FUNCTION T_sinTaylor.getAlgorithmName: ansistring; begin result:='sin-Taylor'; end;
FUNCTION T_sinTaylor.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin x:=x*(1+sqr(x)*(-0.166666666666667+sqr(x)*(+0.00833333333333333+sqr(x)*(-0.000198412698412698+2.75573192239859E-6*sqr(x)))))+c; end;
  getRawDataAt_Body;

FUNCTION T_sinus.getAlgorithmName: ansistring; begin result:='Sinus'; end;
FUNCTION T_sinus.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin x:=sin(x+c); end;
  getRawDataAt_Body;

FUNCTION T_tul.getAlgorithmName: ansistring; begin result:='TUL I'; end;
FUNCTION T_tul.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin if sqrabs(x)>1 then x:=1/x else x:=x*(c+x); end;
  getRawDataAt_Body;

FUNCTION T_tul2.getAlgorithmName: ansistring; begin result:='TUL II'; end;
FUNCTION T_tul2.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin x:=x*x+c; x:=0.5*(x+1/x); x:=0.5*(x+1/x); end;
  getRawDataAt_Body;

FUNCTION T_tul3.getAlgorithmName: ansistring; begin result:='TUL III'; end;
FUNCTION T_tul3.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=1/c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin if x.im>0 then x:=sin(x+c) else x:=exp(x+c); end;
  getRawDataAt_Body;

FUNCTION T_tul4.getAlgorithmName: ansistring; begin result:='TUL IV'; end;
FUNCTION T_tul4.getRawDataAt(CONST xy: T_Complex): T_floatColor;
  PROCEDURE iterationStart(VAR c: T_Complex; OUT  x: T_Complex);inline; begin x:=1/c; end;
  PROCEDURE iterationStep(CONST c: T_Complex; VAR x: T_Complex);inline; begin if x.re>0 then x:=1/x+c else x:=1/sqr(x)+c; end;
  getRawDataAt_Body;

FUNCTION T_unnamed1.getAlgorithmName: ansistring; begin result:='Unnamed I'; end;
FUNCTION T_unnamed1.getRawDataAt(CONST xy: T_Complex): T_floatColor;
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

FUNCTION T_unnamed2.getAlgorithmName: ansistring; begin result:='Unnamed II'; end;
  FUNCTION T_unnamed2.getRawDataAt(CONST xy: T_Complex): T_floatColor;
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

FUNCTION T_weierstrass4.getAlgorithmName: ansistring; begin result:='Weierstrass-4'; end;
FUNCTION T_weierstrass4.getRawDataAt(CONST xy: T_Complex): T_floatColor;
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

FUNCTION T_weierstrass6.getAlgorithmName: ansistring; begin result:='Weierstrass-6'; end;
FUNCTION T_weierstrass6.getRawDataAt(CONST xy: T_Complex): T_floatColor;
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

FUNCTION T_mandelbrot.getAlgorithmName:ansistring; begin result:='Mandelbrot / Julia'; end;
FUNCTION T_mandelbrot.getRawDataAt(CONST xy:T_Complex):T_floatColor;
  PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); begin x:=sqr(x)+c; end;
  getRawDataAt_Body;

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

FUNCTION T_mandelbar.getAlgorithmName:ansistring; begin result:='Mandelbar  /-Julia'; end;
FUNCTION T_mandelbar.getRawDataAt(CONST xy:T_Complex):T_floatColor;
  PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
  PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); begin x:=sqr(x); x.im:=-x.im; x:=x+c; end;
  getRawDataAt_Body;

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

FUNCTION T_burningJulia.getAlgorithmName: ansistring; begin result:='Burning Ship /-Julia'; end;
FUNCTION T_burningJulia.getRawDataAt(CONST xy:T_Complex):T_floatColor;
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

FUNCTION T_burningJulia2.getAlgorithmName: ansistring; begin result:='Burning Ship /-Julia (interp. A)'; end;
FUNCTION T_burningJulia2.getRawDataAt(CONST xy:T_Complex):T_floatColor;
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

FUNCTION T_burningJulia3.getAlgorithmName: ansistring; begin result:='Burning Ship /-Julia (interp. B)'; end;
FUNCTION T_burningJulia3.getRawDataAt(CONST xy:T_Complex):T_floatColor;
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


VAR newton3Algorithm: T_newton3Algorithm;
    newton5Algorithm: T_newton5Algorithm;
    bump            : T_bump;
    diperiodic      : T_diperiodic;
    expoA           : T_expoA;
    expoB           : T_expoB;
    expoCancel5a    : T_expoCancel5a;
    expoCancel5b    : T_expoCancel5b;
    freakWave        :T_freakWave        ;
    lnTaylor         :T_lnTaylor         ;
    logisticEquation :T_logisticEquation ;
    logisticEquation2:T_logisticEquation2;
    mandelbrot_p4    :T_mandelbrot_p4    ;
    mbCosine         :T_mbCosine         ;
    mbCosine2        :T_mbCosine2        ;
    nondivergent     :T_nondivergent     ;
    parabola         :T_parabola         ;
    //sierpinski       :T_sierpinski       ;
    sinTaylor        :T_sinTaylor        ;
    sinus            :T_sinus            ;
    tul              :T_tul              ;
    tul2             :T_tul2             ;
    tul3             :T_tul3             ;
    tul4             :T_tul4             ;
    unnamed1         :T_unnamed1         ;
    unnamed2         :T_unnamed2         ;
    weierstrass4     :T_weierstrass4     ;
    weierstrass6     :T_weierstrass6     ;

VAR mandelbrot      : T_mandelbrot;
    mandelbar       : T_mandelbar;
    burningJulia    : T_burningJulia;
    burningJulia2   : T_burningJulia2;
    burningJulia3   : T_burningJulia3;
INITIALIZATION
  newton3Algorithm.create; registerAlgorithm(@newton3Algorithm,true,true,false);
  newton5Algorithm.create; registerAlgorithm(@newton5Algorithm,true,true,false);
  bump            .create; registerAlgorithm(@bump            ,true,true,false);
  diperiodic      .create; registerAlgorithm(@diperiodic      ,true,true,false);
  expoA           .create; registerAlgorithm(@expoA       ,true,true,false);
  expoB           .create; registerAlgorithm(@expoB       ,true,true,false);
  expoCancel5a    .create; registerAlgorithm(@expoCancel5a,true,true,false);
  expoCancel5b    .create; registerAlgorithm(@expoCancel5b,true,true,false);
  freakWave        .create; registerAlgorithm(@freakWave        ,true,true,false);
  lnTaylor         .create; registerAlgorithm(@lnTaylor         ,true,true,false);
  logisticEquation .create; registerAlgorithm(@logisticEquation ,true,true,false);
  logisticEquation2.create; registerAlgorithm(@logisticEquation2,true,true,false);
  mandelbrot_p4    .create; registerAlgorithm(@mandelbrot_p4    ,true,true,false);
  mbCosine         .create; registerAlgorithm(@mbCosine         ,true,true,false);
  mbCosine2        .create; registerAlgorithm(@mbCosine2        ,true,true,false);
  nondivergent     .create; registerAlgorithm(@nondivergent     ,true,true,false);
  parabola         .create; registerAlgorithm(@parabola         ,true,true,false);
  //sierpinski       .create; registerAlgorithm(@sierpinski       ,true,true,false);
  sinTaylor        .create; registerAlgorithm(@sinTaylor        ,true,true,false);
  sinus            .create; registerAlgorithm(@sinus            ,true,true,false);
  tul              .create; registerAlgorithm(@tul              ,true,true,false);
  tul2             .create; registerAlgorithm(@tul2             ,true,true,false);
  tul3             .create; registerAlgorithm(@tul3             ,true,true,false);
  tul4             .create; registerAlgorithm(@tul4             ,true,true,false);
  unnamed1         .create; registerAlgorithm(@unnamed1         ,true,true,false);
  unnamed2         .create; registerAlgorithm(@unnamed2         ,true,true,false);
  weierstrass4     .create; registerAlgorithm(@weierstrass4     ,true,true,false);
  weierstrass6     .create; registerAlgorithm(@weierstrass6     ,true,true,false);

  mandelbrot      .create; registerAlgorithm(@mandelbrot      ,true,true,true);
  mandelbar       .create; registerAlgorithm(@mandelbar       ,true,true,true);
  burningJulia    .create; registerAlgorithm(@burningJulia    ,true,true,true);
  burningJulia2   .create; registerAlgorithm(@burningJulia2   ,true,true,true);
  burningJulia3   .create; registerAlgorithm(@burningJulia3   ,true,true,true);

FINALIZATION
  newton3Algorithm.destroy;
  newton5Algorithm.destroy;
  bump            .destroy;
  diperiodic      .destroy;
  expoA           .destroy;
  expoB           .destroy;
  expoCancel5a    .destroy;
  expoCancel5b    .destroy;
  freakWave        .destroy;
  lnTaylor         .destroy;
  logisticEquation .destroy;
  logisticEquation2.destroy;
  mandelbrot_p4    .destroy;
  mbCosine         .destroy;
  mbCosine2        .destroy;
  nondivergent     .destroy;
  parabola         .destroy;
  //sierpinski       .destroy;
  sinTaylor        .destroy;
  sinus            .destroy;
  tul              .destroy;
  tul2             .destroy;
  tul3             .destroy;
  tul4             .destroy;
  unnamed1         .destroy;
  unnamed2         .destroy;
  weierstrass4     .destroy;
  weierstrass6     .destroy;

  mandelbrot      .destroy;
  mandelbar       .destroy;
  burningJulia    .destroy;
  burningJulia2   .destroy;
  burningJulia3   .destroy;
end.

