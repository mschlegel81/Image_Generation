UNIT ig_epicycles;
INTERFACE
USES imageGeneration,myParams,complex,myColors,math,myTools,darts;
TYPE
  P_epicycle=^T_epicycle;
  T_epicycle=object(T_scaledImageGenerationAlgorithm)
    PAR_ALPHA  :double ;//=0.125;
    PAR_BRIGHT :double ;//=1;
    PAR_a :double;//=0.75;
    PAR_a2:double;//=0.75;
    PAR_b :double;//=-2;
    PAR_b2:double;//=-2;
    PAR_t0:double;//=-3.14159265359;
    PAR_t1:double;//= 3.14159265359;
    PAR_DEPTH  :longint;//=10;
    qualityMultiplier:double; //=1

    renderTempData:record
      flushCs:TRTLCriticalSection;
      samplesFlushed:longint;

      xRes,yRes,
      aaSamples,timesteps:longint;
      coverPerSample:double;
      maxPixelX,maxPixelY:double;
      useQuality:double;
    end;
    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION prepareImage(CONST forPreview: boolean): boolean; virtual;
    PROCEDURE prepareSlice(CONST index:longint);
  end;

  { T_epicycleTodo }
  P_epicycleTodo=^T_epicycleTodo;
  T_epicycleTodo=object(T_queueToDo)
    epi:P_epicycle;
    sliceIndex:longint;

    CONSTRUCTOR create(CONST epicycle:P_epicycle; CONST index:longint);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

IMPLEMENTATION

{ T_epicycleTodo }

CONSTRUCTOR T_epicycleTodo.create(CONST epicycle: P_epicycle; CONST index: longint);
  begin
    epi:=epicycle;
    sliceIndex:=index;
  end;

DESTRUCTOR T_epicycleTodo.destroy;
  begin
  end;

PROCEDURE T_epicycleTodo.execute;
  begin
    epi^.prepareSlice(sliceIndex);
    progressQueue.logStepDone;
  end;

CONSTRUCTOR T_epicycle.create;
  begin
    inherited create;
    addParameter('a',pt_floatOr2Floats);
    addParameter('b',pt_floatOr2Floats);
    addParameter('t[0]',pt_float);
    addParameter('t[1]',pt_float);
    addParameter('brightness',pt_float,0);
    addParameter('alpha',pt_float,0);
    addParameter('depth',pt_integer,0,100);
    addParameter('quality factor',pt_float,1);
    initCriticalSection(renderTempData.flushCs);
    resetParameters(0);
  end;

PROCEDURE T_epicycle.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    PAR_ALPHA  :=0.125;
    PAR_BRIGHT :=1;
    PAR_a :=0.666;
    PAR_a2:=0.666;
    PAR_b :=2;
    PAR_b2:=2;
    PAR_t0:=-3.14159265359;
    PAR_t1:= 3.14159265359;
    PAR_DEPTH  :=10;
    qualityMultiplier:=1
  end;

FUNCTION T_epicycle.getAlgorithmName: ansistring;
  begin
    result:='Epicycles';
  end;

FUNCTION T_epicycle.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+8;
  end;

PROCEDURE T_epicycle.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: begin PAR_a:=value.f0; PAR_a2:=value.f1; end;
      1: begin PAR_b:=value.f0; PAR_b2:=value.f1; end;
      2: PAR_t0:=value.f0;
      3: PAR_t1:=value.f0;
      4: PAR_BRIGHT:=value.f0;
      5: PAR_ALPHA:=value.f0;
      6: PAR_DEPTH:=value.i0;
      7: qualityMultiplier:=value.f0;
    end;
  end;

FUNCTION T_epicycle.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index));
    case byte(index-inherited numberOfParameters) of
      0: result.createFromValue(parameterDescription(inherited numberOfParameters  ),PAR_a,PAR_a2);
      1: result.createFromValue(parameterDescription(inherited numberOfParameters+1),PAR_b,PAR_b2);
      2: result.createFromValue(parameterDescription(inherited numberOfParameters+2),PAR_t0);
      3: result.createFromValue(parameterDescription(inherited numberOfParameters+3),PAR_t1);
      4: result.createFromValue(parameterDescription(inherited numberOfParameters+4),PAR_BRIGHT);
      5: result.createFromValue(parameterDescription(inherited numberOfParameters+5),PAR_ALPHA);
      6: result.createFromValue(parameterDescription(inherited numberOfParameters+6),PAR_DEPTH);
      7: result.createFromValue(parameterDescription(inherited numberOfParameters+7),qualityMultiplier);
    end;
  end;

FUNCTION T_epicycle.prepareImage(CONST forPreview: boolean): boolean;
  VAR x,y:longint;
      todo:P_epicycleTodo;
      newAASamples:longint;
  begin
    newAASamples:=min(length(darts_delta),max(1,trunc(qualityMultiplier/PAR_ALPHA)));
    progressQueue.forceStart(et_stepCounter_parallel,newAASamples);
    for y:=0 to renderImage^.height-1 do for x:=0 to renderImage^.width-1 do renderImage^[x,y]:=black;
    scaler.rescale(renderImage^.width,renderImage^.height);

    with renderTempData do begin
      samplesFlushed:=0;
      xRes:=renderImage^.width ;
      yRes:=renderImage^.height;
      maxPixelX:=renderImage^.width -0.5;
      maxPixelY:=renderImage^.height-0.5;
      aaSamples:=newAASamples;
      useQuality:=qualityMultiplier/aaSamples;
      coverPerSample:=PAR_ALPHA/useQuality;
      timesteps:=round(useQuality*renderImage^.width*renderImage^.height);
      for x:=0 to aaSamples-1 do begin
        new(todo,create(@self,x));
        progressQueue.enqueue(todo);
      end;
    end;
    result:=false;
  end;

PROCEDURE T_epicycle.prepareSlice(CONST index: longint);
  VAR i,k:longint;
      a,b,fa,fb,x,y:double;
      tempMap:array of word;
      flushFactor:double;

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

  FUNCTION updatedPixel(CONST prevColor:T_floatColor; CONST hits:word):T_floatColor; inline;
    begin
      result[0]:=(prevColor[0]*renderTempData.samplesFlushed+
                  PAR_BRIGHT*(1-intpower(1-renderTempData.coverPerSample,hits)))*flushFactor;
      result[0]:=min(PAR_BRIGHT,max(0,result[0]));
      result[1]:=result[0];
      result[2]:=result[0];
    end;

  begin
    with renderTempData do if index<aaSamples then begin
      randomize;
      setLength(tempMap,xRes*yRes);
      for i:=0 to length(tempMap)-1 do tempMap[i]:=0;
      for i:=0 to timesteps-1 do begin
        a:=PAR_a+random*(PAR_a2-PAR_a);
        b:=PAR_b+random*(PAR_b2-PAR_b);
        if abs(a)>=1 then fa:=1 else fa:=1-abs(a);
        fb:=PAR_t0+(PAR_t1-PAR_t0)*random;
        x:=fa*system.sin(fb);
        y:=fa*system.cos(fb);
        for k:=1 to PAR_DEPTH do begin
          fa:=fa*a;
          fb:=fb*b;
          x:=x+fa*system.sin(fb);
          y:=y+fa*system.cos(fb);
        end;
        putSample(x,y);
      end;
      if not(progressQueue.cancellationRequested) then begin
        system.enterCriticalSection(flushCs);
        flushFactor:=(1/(samplesFlushed+1));
        for i:=0 to yRes-1 do for k:=0 to xRes-1 do renderImage^[k,i]:=updatedPixel(renderImage^[k,i],tempMap[i*xRes+k]);
        inc(samplesFlushed);
        system.leaveCriticalSection(flushCs);
      end;
      setLength(tempMap,0);
    end;
  end;

VAR epicycle:T_epicycle;
INITIALIZATION
  epicycle.create;
  registerAlgorithm(@epicycle,true,false,false);

FINALIZATION
  epicycle.destroy;

end.
