UNIT ig_epicycles;
INTERFACE
USES imageGeneration,myParams,mypics,myTools,complex,myColors,math,darts;
TYPE
  P_epicycle=^T_epicycle;
  T_epicycle=object(T_pixelThrowerAlgorithm)
    par_bright :double ;//=1;
    par_a :double;//=0.75;
    par_a2:double;//=0.75;
    par_b :double;//=-2;
    par_b2:double;//=-2;
    par_t0:double;//=-3.14159265359;
    par_t1:double;//= 3.14159265359;
    par_depth  :longint;//=10;

    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE prepareSlice(CONST target:P_rawImage; CONST queue:P_progressEstimatorQueue; CONST index:longint); virtual;
  end;

IMPLEMENTATION

CONSTRUCTOR T_epicycle.create;
  begin
    inherited create;
    addParameter('a',pt_floatOr2Floats);
    addParameter('b',pt_floatOr2Floats);
    addParameter('t[0]',pt_float);
    addParameter('t[1]',pt_float);
    addParameter('brightness',pt_float,0);
    addParameter('depth',pt_integer,0,100);
    resetParameters(0);
  end;

PROCEDURE T_epicycle.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    par_alpha  :=0.125;
    par_bright :=1;
    par_a :=0.666;
    par_a2:=0.666;
    par_b :=2;
    par_b2:=2;
    par_t0:=-3.14159265359;
    par_t1:= 3.14159265359;
    par_depth  :=10;
    qualityMultiplier:=1
  end;

FUNCTION T_epicycle.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+6;
  end;

PROCEDURE T_epicycle.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: begin par_a:=value.f0; par_a2:=value.f1; end;
      1: begin par_b:=value.f0; par_b2:=value.f1; end;
      2: par_t0:=value.f0;
      3: par_t1:=value.f0;
      4: par_bright:=value.f0;
      5: par_depth:=value.i0;
    end;
  end;

FUNCTION T_epicycle.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index));
    case byte(index-inherited numberOfParameters) of
      0: result:=parValue(index,par_a,par_a2);
      1: result:=parValue(index,par_b,par_b2);
      2: result:=parValue(index,par_t0);
      3: result:=parValue(index,par_t1);
      4: result:=parValue(index,par_bright);
    else result:=parValue(index,par_depth);
    end;
  end;

PROCEDURE T_epicycle.prepareSlice(CONST target:P_rawImage; CONST queue:P_progressEstimatorQueue; CONST index: longint);
  VAR i,k:longint;
      a,b,fa,fb,x,y:double;
      tempMap:array of word;
      flushFactor:double=0;

  PROCEDURE putSample(CONST x,y:double); inline;
    VAR c:T_Complex;
        j:longint;
    begin
      c:=scaler.mrofsnart(x,y);
      c.re:=c.re+darts_delta[index,0];
      c.im:=c.im+darts_delta[index,1];
      if (c.re>-0.5) and (c.re<renderTempData.maxPixelX) and
         (c.im>-0.5) and (c.im<renderTempData.maxPixelY) then begin
        j:=round(c.re)+
           round(c.im)*renderTempData.xRes;
        if tempMap[j]<65535 then inc(tempMap[j]);
      end;
    end;

  FUNCTION updatedPixel(CONST prevColor,bgColor:T_floatColor; CONST hits:word):T_floatColor; inline;
    VAR cover:double;
    begin
      cover:=1-intpower(renderTempData.antiCoverPerSample,hits);
      result:=(prevColor*renderTempData.samplesFlushed+
               white*cover+bgColor*(1-cover))*flushFactor;
    end;

  begin
    with renderTempData do if index<aaSamples then begin
      randomize;
      setLength(tempMap,xRes*yRes);
      for i:=0 to length(tempMap)-1 do tempMap[i]:=0;
      for i:=0 to timesteps-1 do begin
        a:=par_a+random*(par_a2-par_a);
        b:=par_b+random*(par_b2-par_b);
        if abs(a)>=1 then fa:=1 else fa:=1-abs(a);
        fb:=par_t0+(par_t1-par_t0)*random;
        x:=fa*system.sin(fb);
        y:=fa*system.cos(fb);
        for k:=1 to par_depth do begin
          fa:=fa*a;
          fb:=fb*b;
          x:=x+fa*system.sin(fb);
          y:=y+fa*system.cos(fb);
        end;
        putSample(x,y);
      end;
      if not(queue^.cancellationRequested) then begin
        system.enterCriticalSection(flushCs);
        flushFactor:=(1/(samplesFlushed+1));
        if hasBackground and (backgroundImage<>nil)
        then for i:=0 to yRes-1 do for k:=0 to xRes-1 do target^[k,i]:=updatedPixel(target^[k,i],backgroundImage^[k,i],tempMap[i*xRes+k])
        else for i:=0 to yRes-1 do for k:=0 to xRes-1 do target^[k,i]:=updatedPixel(target^[k,i],black                ,tempMap[i*xRes+k]);
        inc(samplesFlushed);
        system.leaveCriticalSection(flushCs);
      end;
      setLength(tempMap,0);
    end;
  end;

FUNCTION newEpicyle:P_generalImageGenrationAlgorithm; begin new(P_epicycle(result),create); end;
INITIALIZATION
  registerAlgorithm('Epicycles',@newEpicyle,true,false,false);
end.
