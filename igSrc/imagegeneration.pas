UNIT imageGeneration;
INTERFACE
USES sysutils, mypics,myGenerics,myColors,complex,math,darts,Interfaces, ExtCtrls, Graphics, types,myTools, myParams,myStringUtil,mySys;
TYPE
  P_generalImageGenrationAlgorithm=^T_generalImageGenrationAlgorithm;
  T_generalImageGenrationAlgorithm=object
    private
      parameterDescriptors:array of P_parameterDescription;
    protected
      PROCEDURE addParameter(CONST name_: string;
                             CONST typ_: T_parameterType;
                             CONST minValue_: double= -infinity;
                             CONST maxValue_: double=  infinity;
                             CONST eT00: ansistring=''; CONST eT01: ansistring=''; CONST eT02: ansistring='';
                             CONST eT03: ansistring=''; CONST eT04: ansistring=''; CONST eT05: ansistring='';
                             CONST eT06: ansistring=''; CONST eT07: ansistring=''; CONST eT08: ansistring='';
                             CONST eT09: ansistring=''; CONST eT10: ansistring=''; CONST eT11: ansistring='';
                             CONST eT12: ansistring=''; CONST eT13: ansistring=''; CONST eT14: ansistring='';
                             CONST eT15: ansistring='');
    public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getAlgorithmName:ansistring; virtual; abstract;

    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual; abstract;

    FUNCTION numberOfParameters:longint; virtual;
    FUNCTION parameterDescription(CONST index:byte):P_parameterDescription;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual; abstract;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual; abstract;

    FUNCTION prepareImage(CONST forPreview:boolean=false):boolean; virtual; abstract;
    FUNCTION toString:ansistring;
    FUNCTION canParseParametersFromString(CONST s:ansistring):boolean;
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

  { T_functionPerPixelAlgorithm }

  T_functionPerPixelAlgorithm=object(T_scaledImageGenerationAlgorithm)
    renderTolerance:double;
    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual; abstract;
    FUNCTION prepareImage(CONST forPreview:boolean=false):boolean; virtual;
    PROCEDURE prepareChunk(VAR chunk:T_colChunk; CONST forPreview:boolean=false); virtual;
  end;

{Target Hierarchy:
   T_scaledImageGenerationAlgorithm
   +-> ifs
   +-> epicycles
   +-> bifurcation plots
   |   +-> bif_typ0
   |   +-> bif_typ1
   |   +-> bif_typ2
   |   +-> bif_typ3
   |   +-> bif_typ4
   |   +-> bif_typ5
   +-> T_functionPerPixelAlgorithm
       +-> expoClouds
       +-> funcTrees
       +-> T_functionPerPixelViaRawDataAlgorithm
           +-> T_functionPerPixelViaRawDataAlgorithmWithJulianess
           |   +-> mandelbrot
           |   +-> mandelbar
           |   +-> burningShip
           |   +-> burningShip_II
           |   +-> burningShip_III
           +-> frac_*
           +-> ...
}
T_algorithmMeta=record
  name:string;
  prototype:P_generalImageGenrationAlgorithm;
  hasScaler:boolean;
  hasLight :boolean;
  hasJuliaP:boolean;
end;

PROCEDURE registerAlgorithm(CONST p:P_generalImageGenrationAlgorithm; CONST scaler,light,julia:boolean);
VAR algorithms   : array of T_algorithmMeta;
    renderImage  : T_rawImage;
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

{ T_workerThreadTodo }

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
    chunk.initForChunk(renderImage.width,renderImage.height,chunkIndex);
    algorithm^.prepareChunk(chunk,forPreview);
    renderImage.copyFromChunk(chunk);
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

PROCEDURE T_generalImageGenrationAlgorithm.addParameter(CONST name_: string;
           CONST typ_: T_parameterType;
           CONST minValue_: double= -infinity;
           CONST maxValue_: double=  infinity;
           CONST eT00: ansistring=''; CONST eT01: ansistring=''; CONST eT02: ansistring='';
           CONST eT03: ansistring=''; CONST eT04: ansistring=''; CONST eT05: ansistring='';
           CONST eT06: ansistring=''; CONST eT07: ansistring=''; CONST eT08: ansistring='';
           CONST eT09: ansistring=''; CONST eT10: ansistring=''; CONST eT11: ansistring='';
           CONST eT12: ansistring=''; CONST eT13: ansistring=''; CONST eT14: ansistring='';
           CONST eT15: ansistring='');
  VAR newDescriptor:P_parameterDescription;
  begin
    new(newDescriptor,create(name_,typ_,minValue_,maxValue_,
                             eT00,eT01,eT02,eT03,
                             eT04,eT05,eT06,eT07,
                             eT08,eT09,eT10,eT11,
                             eT12,eT13,eT14,eT15));
    setLength(parameterDescriptors,length(parameterDescriptors)+1);
    parameterDescriptors[length(parameterDescriptors)-1]:=newDescriptor;
  end;

FUNCTION T_generalImageGenrationAlgorithm.parameterResetStyles: T_arrayOfString;
  begin
    result:='Reset (default)';
  end;

FUNCTION T_generalImageGenrationAlgorithm.numberOfParameters: longint;
  begin
    result:=0;
  end;

FUNCTION T_generalImageGenrationAlgorithm.parameterDescription(CONST index:byte):P_parameterDescription;
  begin
    result:=parameterDescriptors[index];
  end;

FUNCTION T_generalImageGenrationAlgorithm.toString:ansistring;
  VAR i:longint;
  begin
    result:=getAlgorithmName+'[';
    for i:=0 to numberOfParameters-1 do
    result:=result+getParameter(i).toString(true)+'#';
    result:=copy(result,1,length(result)-1)+']';
  end;

FUNCTION T_generalImageGenrationAlgorithm.canParseParametersFromString(CONST s:ansistring):boolean;
  VAR parsedParameters:array of T_parameterValue;
      stringParts:T_arrayOfString;
      i:longint;
  begin
    if not(startsWith(s,getAlgorithmName+'[')) then exit(false);
    if not(endsWith(s,']')) then exit(false);
    stringParts:=split(copy(s,length(getAlgorithmName)+1,length(s)-length(getAlgorithmName)-2),T_arrayOfString('#'));
    if length(stringParts)<>numberOfParameters then begin
      setLength(stringParts,0);
      exit(false);
    end;
    result:=true;
    setLength(parsedParameters,numberOfParameters);
    for i:=0 to numberOfParameters-1 do begin
      parsedParameters[i].createToParse(parameterDescription(i),stringParts[i],true);
      result:=result and parsedParameters[i].isValid;
    end;
    if result then for i:=0 to numberOfParameters-1 do setParameter(i,parsedParameters[i]);
    setLength(parsedParameters,0);
    setLength(stringParts,0);
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
      0: result.createFromValue(parameterDescription(0),scaler.getCenterX);
      1: result.createFromValue(parameterDescription(1),scaler.getCenterY);
      2: result.createFromValue(parameterDescription(2),scaler.getZoom);
      3: result.createFromValue(parameterDescription(3),scaler.getRotation);
    end;
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.panByPixels(VAR plotImage:TImage; CONST dx, dy: longint);
  VAR rectA,rectB:TRect;
  begin
    scaler.moveCenter(-dx,-dy);
    scalerChanagedSinceCalculation:=true;
    rectA.Top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.Top:=0-dy;
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
    rectA.Top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.Top:=round((-cy)*zoomFactor+cy);
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
    else result.createFromValue(parameterDescription(inherited numberOfParameters),renderTolerance,0.0);
  end;

FUNCTION T_functionPerPixelAlgorithm.prepareImage(CONST forPreview: boolean):boolean;
  VAR i:longint;
      pendingChunks:T_pendingList;

  FUNCTION todo(CONST index:longint):P_workerThreadTodo;
    begin new(result,create(@self,index,forPreview)); end;

  begin
    progressQueue.forceStart(et_stepCounter_parallel,renderImage.chunksInMap);
    scalerChanagedSinceCalculation:=false;
    renderImage.markChunksAsPending;
    pendingChunks:=renderImage.getPendingList;
    for i:=0 to length(pendingChunks)-1 do progressQueue.enqueue(todo(pendingChunks[i]));
    result:=false;
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
  renderImage.create(1,1);

FINALIZATION
  setLength(algorithms,0);
  renderImage.destroy;
  progressQueue.destroy;

end.

