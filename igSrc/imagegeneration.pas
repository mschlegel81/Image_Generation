UNIT imageGeneration;
INTERFACE
USES sysutils, mypics,myGenerics,myColors,complex,math,darts,Interfaces, ExtCtrls, Graphics, types,myTools, myParams,myStringUtil,mySys;
TYPE
  P_generalImageGenrationAlgorithm=^T_generalImageGenrationAlgorithm;
  T_generalImageGenrationAlgorithm=object
    private
      parameterDescriptors:array of P_parameterDescription;
    protected
      FUNCTION addParameter(CONST name_: string;
                            CONST typ_: T_parameterType;
                            CONST minValue_: double= -infinity;
                            CONST maxValue_: double=  infinity;
                            CONST eT00: ansistring=''; CONST eT01: ansistring=''; CONST eT02: ansistring='';
                            CONST eT03: ansistring=''; CONST eT04: ansistring=''; CONST eT05: ansistring='';
                            CONST eT06: ansistring=''; CONST eT07: ansistring=''; CONST eT08: ansistring='';
                            CONST eT09: ansistring=''; CONST eT10: ansistring=''; CONST eT11: ansistring='';
                            CONST eT12: ansistring=''; CONST eT13: ansistring=''; CONST eT14: ansistring='';
                            CONST eT15: ansistring=''):P_parameterDescription;
    public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getAlgorithmName:ansistring; virtual; abstract;

    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual; abstract;
    PROCEDURE cleanup; virtual;
    FUNCTION numberOfParameters:longint; virtual;
    FUNCTION parameterDescription(CONST index:byte):P_parameterDescription;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual; abstract;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual; abstract;

    FUNCTION prepareImage(CONST forPreview:boolean=false; CONST waitForFinish:boolean=false):boolean; virtual; abstract;
    FUNCTION toString:ansistring;
    FUNCTION canParseParametersFromString(CONST s:ansistring; CONST doParse:boolean=false):boolean;

    FUNCTION parValue(CONST index:byte; CONST i0:longint; CONST i1:longint=0; CONST i2:longint=0; CONST i3:longint=0):T_parameterValue;
    FUNCTION parValue(CONST index:byte; CONST f0:double; CONST f1:double=0; CONST f2:double=0):T_parameterValue;
    FUNCTION parValue(CONST index:byte; CONST color:T_floatColor):T_parameterValue;
    FUNCTION parValue(CONST index:byte; CONST txt:ansistring; CONST sizeLimit:longint=-1):T_parameterValue;
  end;

CONST SCALER_PARAMETER_COUNT=4;
TYPE
  P_scaledImageGenerationAlgorithm=^T_scaledImageGenerationAlgorithm;
  T_scaledImageGenerationAlgorithm=object(T_generalImageGenrationAlgorithm)
    scaler:T_scaler;
    scalerChanagedSinceCalculation:boolean;

    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE panByPixels(VAR plotImage:TImage; CONST dx,dy:longint); virtual;
    PROCEDURE zoomOnPoint(VAR plotImage:TImage; CONST zoomFactor:double); virtual;
  end;

  P_functionPerPixelAlgorithm=^T_functionPerPixelAlgorithm;
  T_functionPerPixelAlgorithm=object(T_scaledImageGenerationAlgorithm)
    renderTolerance:double;
    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual; abstract;
    FUNCTION prepareImage(CONST forPreview:boolean=false; CONST waitForFinish:boolean=false):boolean; virtual;
    PROCEDURE prepareChunk(VAR chunk:T_colChunk; CONST forPreview:boolean=false); virtual;
  end;

  P_pixelThrowerAlgorithm=^T_pixelThrowerAlgorithm;

  { T_pixelThrowerAlgorithm }

  T_pixelThrowerAlgorithm=object(T_scaledImageGenerationAlgorithm)
    qualityMultiplier:double;
    par_alpha  :double ;
    hasBackground:boolean;

    renderTempData:record
      backgroundImage:P_rawImage;

      flushCs:TRTLCriticalSection;
      samplesFlushed:longint;

      xRes,yRes,
      aaSamples,timesteps:longint;
      coverPerSample:double;
      antiCoverPerSample:double;
      maxPixelX,maxPixelY:double;
      useQuality:double;
    end;

    CONSTRUCTOR create;
    PROCEDURE cleanup; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION prepareImage(CONST forPreview: boolean; CONST waitForFinish:boolean=false): boolean; virtual;
    PROCEDURE prepareSlice(CONST index:longint); virtual; abstract;
  end;

  P_workerThreadTodo=^T_workerThreadTodo;
  T_workerThreadTodo=object(T_queueToDo)
    algorithm:P_functionPerPixelAlgorithm;
    chunkIndex:longint;
    forPreview:boolean;

    CONSTRUCTOR create(CONST algorithm_:P_functionPerPixelAlgorithm;
                       CONST chunkIndex_:longint;
                       CONST forPreview_:boolean);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

  P_pixelThrowerTodo=^T_pixelThrowerTodo;
  T_pixelThrowerTodo=object(T_queueToDo)
    algorithm:P_pixelThrowerAlgorithm;
    sliceIndex:longint;

    CONSTRUCTOR create(CONST algorithm_:P_pixelThrowerAlgorithm; CONST index:longint);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

  T_algorithmMeta=record
    name:string;
    prototype:P_generalImageGenrationAlgorithm;
    hasScaler:boolean;
    hasLight :boolean;
    hasJuliaP:boolean;
  end;

PROCEDURE registerAlgorithm(CONST p:P_generalImageGenrationAlgorithm; CONST scaler,light,julia:boolean);
FUNCTION prepareImage(CONST specification:ansistring; CONST image:P_rawImage; CONST forPreview:boolean):longint;
FUNCTION isPlausibleSpecification(CONST specification:ansistring; CONST doPrepare:boolean):longint;
VAR algorithms   : array of T_algorithmMeta;
    generationImage : P_rawImage;
    progressQueue: T_progressEstimatorQueue;
IMPLEMENTATION
PROCEDURE registerAlgorithm(CONST p:P_generalImageGenrationAlgorithm; CONST scaler,light,julia:boolean);
  begin
    setLength(algorithms,length(algorithms)+1);
    with algorithms[length(algorithms)-1] do begin
      name:=p^.getAlgorithmName;
      prototype:=p;
      hasScaler:=scaler;
      hasLight :=light;
      hasJuliaP:=julia;
    end;
  end;

FUNCTION prepareImage(CONST specification:ansistring; CONST image:P_rawImage; CONST forPreview:boolean):longint;
  VAR i:longint;
      prevRenderImage:P_rawImage;
  begin
    result:=-1;
    for i:=0 to length(algorithms)-1 do if algorithms[i].prototype^.canParseParametersFromString(specification,true) then begin
      prevRenderImage:=generationImage;
      generationImage:=image;
      algorithms[i].prototype^.prepareImage(forPreview,true);
      algorithms[i].prototype^.cleanup;
      generationImage:=prevRenderImage;
      exit(i);
    end;
  end;

FUNCTION isPlausibleSpecification(CONST specification:ansistring; CONST doPrepare:boolean):longint;
  VAR i:longint;
  begin
    result:=-1;
    for i:=0 to length(algorithms)-1 do if algorithms[i].prototype^.canParseParametersFromString(specification,doPrepare) then exit(i);
  end;

CONSTRUCTOR T_pixelThrowerTodo.create(CONST algorithm_: P_pixelThrowerAlgorithm; CONST index: longint);
  begin
    algorithm:=algorithm_;
    sliceIndex:=index;
  end;

DESTRUCTOR T_pixelThrowerTodo.destroy;
  begin
  end;

PROCEDURE T_pixelThrowerTodo.execute;
  begin
    algorithm^.prepareSlice(sliceIndex);
    progressQueue.logStepDone;
  end;

CONSTRUCTOR T_pixelThrowerAlgorithm.create;
  begin
    inherited create;
    addParameter('alpha',pt_float,0);
    addParameter('quality factor',pt_float,1);
    addParameter('has background',pt_enum,0,1,'no','yes');
    initCriticalSection(renderTempData.flushCs);
  end;

PROCEDURE T_pixelThrowerAlgorithm.cleanup;
  begin
    with renderTempData do if backgroundImage<>nil then begin
      dispose(backgroundImage,destroy);
      backgroundImage:=nil;
    end;
  end;

PROCEDURE T_pixelThrowerAlgorithm.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    par_alpha:=0.125;
    qualityMultiplier:=1;
    hasBackground:=false;
  end;

FUNCTION T_pixelThrowerAlgorithm.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+3;
  end;

PROCEDURE T_pixelThrowerAlgorithm.setParameter(CONST index: byte;
  CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: par_alpha:=value.f0;
      1: qualityMultiplier:=value.f0;
      2: hasBackground:=(value.i0=1);
    end;
  end;

FUNCTION T_pixelThrowerAlgorithm.getParameter(CONST index: byte
  ): T_parameterValue;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index));
    case byte(index-inherited numberOfParameters) of
      0: result:=parValue(index,par_alpha);
      1: result:=parValue(index,qualityMultiplier);
      2: if hasBackground
         then result:=parValue(index,1)
         else result:=parValue(index,0);
    end;
  end;

FUNCTION T_pixelThrowerAlgorithm.prepareImage(CONST forPreview: boolean;
  CONST waitForFinish: boolean): boolean;
  VAR x,y:longint;
      todo:P_pixelThrowerTodo;
      newAASamples:longint;
      useQualityMultiplier:double=1;
  begin
    if forPreview and (qualityMultiplier>1)
    then useQualityMultiplier:=1
    else useQualityMultiplier:=qualityMultiplier;

    if generationImage^.width*generationImage^.height<=0 then exit(true);
    newAASamples:=min(64,max(1,trunc(useQualityMultiplier/par_alpha)));
    progressQueue.forceStart(et_stepCounter_parallel,newAASamples);
    scaler.rescale(generationImage^.width,generationImage^.height);

    with renderTempData do begin
      if hasBackground then new(backgroundImage,create(generationImage^));
      samplesFlushed:=0;
      xRes:=generationImage^.width ;
      yRes:=generationImage^.height;
      maxPixelX:=generationImage^.width -0.5;
      maxPixelY:=generationImage^.height-0.5;
      aaSamples:=newAASamples;
      useQuality:=useQualityMultiplier/aaSamples;
      coverPerSample:=par_alpha/useQuality;
      antiCoverPerSample:=1-coverPerSample;
      timesteps:=round(useQuality*generationImage^.width*generationImage^.height);
      for x:=0 to aaSamples-1 do begin
        new(todo,create(@self,x));
        progressQueue.enqueue(todo);
      end;
    end;
    for y:=0 to generationImage^.height-1 do for x:=0 to generationImage^.width-1 do generationImage^[x,y]:=black;
    if waitForFinish then begin
      repeat until not(progressQueue.activeDeqeue);
      progressQueue.waitForEndOfCalculation;
      result:=true;
    end else result:=false;
  end;

CONSTRUCTOR T_workerThreadTodo.create(CONST algorithm_: P_functionPerPixelAlgorithm; CONST chunkIndex_: longint; CONST forPreview_: boolean);
  begin
    inherited create;
    algorithm:=algorithm_;
    chunkIndex:=chunkIndex_;
    forPreview:=forPreview_;
  end;

DESTRUCTOR T_workerThreadTodo.destroy;
  begin
  end;

PROCEDURE T_workerThreadTodo.execute;
  VAR chunk:T_colChunk;
  begin
    chunk.create;
    chunk.initForChunk(generationImage^.width,generationImage^.height,chunkIndex);
    algorithm^.prepareChunk(chunk,forPreview);
    generationImage^.copyFromChunk(chunk);
    chunk.destroy;
    progressQueue.logStepDone;
  end;

CONSTRUCTOR T_generalImageGenrationAlgorithm.create;
  begin
    setLength(parameterDescriptors,0);
  end;

DESTRUCTOR T_generalImageGenrationAlgorithm.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(parameterDescriptors)-1 do if parameterDescriptors[i]<>nil then freeMem(parameterDescriptors[i],sizeOf(T_parameterDescription));
  end;

FUNCTION T_generalImageGenrationAlgorithm.addParameter(CONST name_: string;
  CONST typ_: T_parameterType; CONST minValue_: double;
  CONST maxValue_: double; CONST eT00: ansistring; CONST eT01: ansistring;
  CONST eT02: ansistring; CONST eT03: ansistring; CONST eT04: ansistring;
  CONST eT05: ansistring; CONST eT06: ansistring; CONST eT07: ansistring;
  CONST eT08: ansistring; CONST eT09: ansistring; CONST eT10: ansistring;
  CONST eT11: ansistring; CONST eT12: ansistring; CONST eT13: ansistring;
  CONST eT14: ansistring; CONST eT15: ansistring):P_parameterDescription;
  begin
    new(result,create(name_,typ_,minValue_,maxValue_,
                      eT00,eT01,eT02,eT03,
                      eT04,eT05,eT06,eT07,
                      eT08,eT09,eT10,eT11,
                      eT12,eT13,eT14,eT15));
    setLength(parameterDescriptors,length(parameterDescriptors)+1);
    parameterDescriptors[length(parameterDescriptors)-1]:=result;
  end;

FUNCTION T_generalImageGenrationAlgorithm.parameterResetStyles: T_arrayOfString;
  begin
    result:='Reset (default)';
  end;

PROCEDURE T_generalImageGenrationAlgorithm.cleanup;
  begin
  end;

FUNCTION T_generalImageGenrationAlgorithm.numberOfParameters: longint;
  begin
    result:=0;
  end;

FUNCTION T_generalImageGenrationAlgorithm.parameterDescription(CONST index: byte
  ): P_parameterDescription;
  begin
    result:=parameterDescriptors[index];
  end;

FUNCTION T_generalImageGenrationAlgorithm.toString: ansistring;
  VAR i:longint;
      p:array of array[0..1] of T_parameterValue;
  begin
    setLength(p,numberOfParameters);
    for i:=0 to numberOfParameters-1 do p[i,1]:=getParameter(i);
    resetParameters(0);
    for i:=0 to numberOfParameters-1 do p[i,0]:=getParameter(i);
    for i:=0 to numberOfParameters-1 do setParameter(i,p[i,1]);

    result:='';
    for i:=0 to numberOfParameters-1 do if not(p[i,0].strEq(p[i,1])) then
    result:=result+getParameter(i).toString(tsm_forSerialization)+';';
    result:=getAlgorithmName+'['+copy(result,1,length(result)-1)+']';
  end;

FUNCTION T_generalImageGenrationAlgorithm.canParseParametersFromString(CONST s: ansistring; CONST doParse: boolean): boolean;
  VAR parsedParameters:array of T_parameterValue;
      paramRest:ansistring;
      stringParts:T_arrayOfString;
      i,j:longint;
      match:boolean=false;
  begin
    if not(startsWith(s,getAlgorithmName+'[')) then begin
      exit(false);
    end;
    if not(endsWith(s,']')) then begin
      exit(false);
    end;
    paramRest:=copy(s,length(getAlgorithmName+'[')+1,length(s)-length(getAlgorithmName)-2);
    if trim(paramRest)='' then setLength(stringParts,0)
                          else stringParts:=split(paramRest,T_arrayOfString(';'));
    result:=true;
    setLength(parsedParameters,numberOfParameters);
    for j:=0 to numberOfParameters-1 do parsedParameters[j].createToParse(parameterDescriptors[j],'',tsm_withNiceParameterName);
    for i:=0 to length(stringParts)-1 do begin
      match:=false;
      for j:=0 to numberOfParameters-1 do if not(match) and not(parsedParameters[j].isValid) then
        match:=match or parsedParameters[j].canParse(stringParts[i],tsm_forSerialization);
      result:=result and match; //Valid only if all strings can be matched
      if not(match) then writeln(stdErr,'String part "',stringParts[i],'" could not be matched');
    end;
    if result and doParse then begin
      cleanup;
      resetParameters(0);
      for i:=0 to numberOfParameters-1 do if parsedParameters[i].isValid then setParameter(i,parsedParameters[i]);
    end;
    setLength(parsedParameters,0);
    setLength(stringParts,0);
  end;

FUNCTION T_generalImageGenrationAlgorithm.parValue(CONST index: byte; CONST i0: longint; CONST i1: longint; CONST i2: longint; CONST i3: longint): T_parameterValue;
  begin
    result.createFromValue(parameterDescriptors[index],i0,i1,i2,i3);
  end;

FUNCTION T_generalImageGenrationAlgorithm.parValue(CONST index: byte; CONST f0: double; CONST f1: double; CONST f2: double): T_parameterValue;
  begin
    result.createFromValue(parameterDescriptors[index],f0,f1,f2);
  end;

FUNCTION T_generalImageGenrationAlgorithm.parValue(CONST index: byte; CONST color: T_floatColor): T_parameterValue;
  begin
    result.createFromValue(parameterDescriptors[index],color);
  end;

FUNCTION T_generalImageGenrationAlgorithm.parValue(CONST index: byte; CONST txt: ansistring; CONST sizeLimit: longint): T_parameterValue;
  begin
    result.createFromValue(parameterDescriptors[index],txt,sizeLimit);
  end;

CONSTRUCTOR T_scaledImageGenerationAlgorithm.create;
  begin
    inherited create;
    scaler.create(100,100,0,0,1,0);
    addParameter('center x',pt_float);
    addParameter('center y',pt_float);
    addParameter('zoom',pt_float,1E-20);
    addParameter('rotation',pt_float);
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.resetParameters(CONST style: longint);
  begin
    scaler.recreate(100,100,0,0,0.2,0);
  end;

FUNCTION T_scaledImageGenerationAlgorithm.numberOfParameters: longint;
  begin
    result:=SCALER_PARAMETER_COUNT;
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index>=4 then exit;
    case index of
      0: scaler.setCenterX (value.f0);
      1: scaler.setCenterY (value.f0);
      2: scaler.setZoom    (value.f0);
      3: scaler.setRotation(value.f0);
    end;
    scalerChanagedSinceCalculation:=true;
  end;

FUNCTION T_scaledImageGenerationAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index>=4 then exit;
    case index of
      0: result:=parValue(index,scaler.getCenterX);
      1: result:=parValue(index,scaler.getCenterY);
      2: result:=parValue(index,scaler.getZoom);
      3: result:=parValue(index,scaler.getRotation);
    end;
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.panByPixels(VAR plotImage:TImage; CONST dx, dy: longint);
  VAR rectA,rectB:TRect;
  begin
    scaler.moveCenter(-dx,-dy);
    scalerChanagedSinceCalculation:=true;
    rectA.top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.top:=0-dy;
    rectB.Left:=0-dx;
    rectB.Right:=plotImage.width-dx;
    rectB.Bottom:=plotImage.height-dy;
    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
    scalerChanagedSinceCalculation:=true;
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.zoomOnPoint(VAR plotImage:TImage;  CONST zoomFactor: double);
  VAR rectA,rectB:TRect;
      cx,cy:double;
  begin
    scaler.setZoom(scaler.getZoom/zoomFactor);
    cx:=plotImage.width/2;
    cy:=plotImage.height/2;
    rectA.top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.top:=round((-cy)*zoomFactor+cy);
    rectB.Left:=round((-cx)*zoomFactor+cx);
    rectB.Right:=round((plotImage.width-cx)*zoomFactor+cx);
    rectB.Bottom:=round((plotImage.height-cy)*zoomFactor+cy);
    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
    scalerChanagedSinceCalculation:=true;
  end;

CONSTRUCTOR T_functionPerPixelAlgorithm.create;
  begin
    inherited create;
    addParameter('render tolerance',pt_float,1E-3,1E3);
  end;

PROCEDURE T_functionPerPixelAlgorithm.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    renderTolerance:=1;
  end;

FUNCTION T_functionPerPixelAlgorithm.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+1;
  end;

PROCEDURE T_functionPerPixelAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else renderTolerance:=value.f0;
  end;

FUNCTION T_functionPerPixelAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters
    then result:=inherited getParameter(index)
    else result:=parValue(index,renderTolerance);
  end;

FUNCTION T_functionPerPixelAlgorithm.prepareImage(CONST forPreview: boolean; CONST waitForFinish:boolean=false):boolean;
  VAR i:longint;
      pendingChunks:T_pendingList;

  FUNCTION todo(CONST index:longint):P_workerThreadTodo;
    begin new(result,create(@self,index,forPreview)); end;

  begin
    progressQueue.forceStart(et_stepCounter_parallel,generationImage^.chunksInMap);
    scaler.rescale(generationImage^.width,generationImage^.height);
    scalerChanagedSinceCalculation:=false;
    generationImage^.markChunksAsPending;
    pendingChunks:=generationImage^.getPendingList;
    for i:=0 to length(pendingChunks)-1 do progressQueue.enqueue(todo(pendingChunks[i]));
    if waitForFinish then begin
      repeat until not(progressQueue.activeDeqeue);
      progressQueue.waitForEndOfCalculation;
      result:=true;
    end else result:=false;
  end;

PROCEDURE T_functionPerPixelAlgorithm.prepareChunk(VAR chunk: T_colChunk; CONST forPreview: boolean);
  VAR i,j,k,k0,k1:longint;
  begin
    for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do with chunk.col[i,j] do rest:=getColorAt(chunk.getPicX(i),chunk.getPicY(j),scaler.transform(chunk.getPicX(i),chunk.getPicY(j)));
    if forPreview then exit;
    while (renderTolerance>1E-3) and chunk.markAlias(renderTolerance) and not(progressQueue.cancellationRequested) do
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
        chunk.getPicX(i),
        chunk.getPicY(j) ,
        scaler.transform(chunk.getPicX(i)+darts_delta[k,0],
                         chunk.getPicY(j)+darts_delta[k,1]));
    end;
  end;

INITIALIZATION
  progressQueue.create;
  new(generationImage,create(1,1));

FINALIZATION
  setLength(algorithms,0);
  dispose(generationImage,destroy);
  progressQueue.destroy;

end.

