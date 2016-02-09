UNIT imageGeneration;
INTERFACE
USES sysutils, mypics,myGenerics,myColors,complex,math,darts,Interfaces, ExtCtrls, Graphics, types,simplePicChunks, myTools, myParams,myStringUtil,mySys;
TYPE
  P_generalImageGenrationAlgorithm=^T_generalImageGenrationAlgorithm;
  T_generalImageGenrationAlgorithm=object
    FUNCTION getAlgorithmName:ansistring; virtual; abstract;

    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual; abstract;

    FUNCTION numberOfParameters:longint; virtual;
    FUNCTION parameterDescription(CONST index:byte):T_parameterDescription; virtual; abstract;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual; abstract;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual; abstract;

    PROCEDURE panByPixels(VAR img:TImage; CONST dx,dy:longint); virtual;
    PROCEDURE zoomOnPoint(VAR img:TImage; CONST cx,cy:longint; CONST zoomFactor:double); virtual;

    PROCEDURE prepareImage(CONST forPreview:boolean=false); virtual; abstract;
    FUNCTION toString:ansistring;
    FUNCTION canParseParametersFromString(CONST s:ansistring):boolean;
  end;

  P_colorGradientAlgorithm=^T_colorGradientAlgorithm;
  T_colorGradientAlgorithm=object(T_generalImageGenrationAlgorithm)
    c0,c1:T_floatColor;
    angle:double;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getAlgorithmName:ansistring; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    FUNCTION parameterDescription(CONST index:byte):T_parameterDescription; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE prepareImage(CONST forPreview:boolean=false); virtual;
  end;

  P_perlinNoiseAlgorithm=^T_perlinNoiseAlgorithm;
  T_perlinNoiseAlgorithm=object(T_generalImageGenrationAlgorithm)
    seed:longint;
    scaleFactor,amplitudeFactor:double;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getAlgorithmName:ansistring; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    FUNCTION parameterDescription(CONST index:byte):T_parameterDescription; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE prepareImage(CONST forPreview:boolean=false); virtual;
  end;

  { T_scaledImageGenerationAlgorithm }

  T_scaledImageGenerationAlgorithm=object(T_generalImageGenrationAlgorithm)
    scaler:T_scaler;
    scalerChanagedSinceCalculation:boolean;

    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    FUNCTION parameterDescription(CONST index:byte):T_parameterDescription; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE panByPixels(VAR plotImage:TImage; CONST dx,dy:longint); virtual;
    PROCEDURE zoomOnPoint(VAR plotImage:TImage; CONST cx,cy:longint; CONST zoomFactor:double); virtual;
  end;

  P_functionPerPixelAlgorithm=^T_functionPerPixelAlgorithm;

  { T_functionPerPixelAlgorithm }

  T_functionPerPixelAlgorithm=object(T_scaledImageGenerationAlgorithm)
    renderTolerance:double;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    FUNCTION parameterDescription(CONST index:byte):T_parameterDescription; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual; abstract;
    PROCEDURE prepareImage(CONST forPreview:boolean=false); virtual;
    PROCEDURE prepareChunk(VAR chunk:T_colChunk; CONST forPreview:boolean=false); virtual;
  end;

  { T_functionPerPixelViaRawDataAlgorithm }

  P_functionPerPixelViaRawDataAlgorithm=^T_functionPerPixelViaRawDataAlgorithm;
  T_functionPerPixelViaRawDataAlgorithm=object(T_functionPerPixelAlgorithm)
    temporaryRawMap:P_rawImage;
    rawMapIsOutdated:boolean;

    maxDepth    :longint;
    colorSource :byte;
    colorStyle  :byte;
    colorVariant:byte;
    pseudoGamma :double;

    CONSTRUCTOR create;
    PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); virtual; abstract;
    PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); virtual; abstract;
    FUNCTION getRawDataAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor;
    FUNCTION getColor(CONST rawData:T_floatColor):T_floatColor; virtual;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual;
    PROCEDURE prepareRawMap(CONST workerIndex,modul:longint); virtual;
    PROCEDURE prepareImage(CONST forPreview:boolean=false); virtual;
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


PROCEDURE registerAlgorithm(CONST p:P_generalImageGenrationAlgorithm);
VAR algorithms:array of record
                 name:string;
                 prototype:P_generalImageGenrationAlgorithm;
               end;
    renderImage:T_rawImage;
    progressor  :T_progressEstimator;
IMPLEMENTATION
VAR colorGradientAlgorithm:T_colorGradientAlgorithm;
    perlinNoiseAlgorithm  :T_perlinNoiseAlgorithm;

PROCEDURE registerAlgorithm(CONST p:P_generalImageGenrationAlgorithm);
  begin
    setLength(algorithms,length(algorithms)+1);
    algorithms[length(algorithms)-1].name:=p^.getAlgorithmName;
    algorithms[length(algorithms)-1].prototype:=p;
  end;

FUNCTION T_generalImageGenrationAlgorithm.parameterResetStyles: T_arrayOfString;
  begin
    result:='Reset (default)';
  end;

FUNCTION T_generalImageGenrationAlgorithm.numberOfParameters: longint;
  begin
    result:=0;
  end;

PROCEDURE T_generalImageGenrationAlgorithm.panByPixels(VAR img:TImage; CONST dx, dy: longint);
  begin
  end;

PROCEDURE T_generalImageGenrationAlgorithm.zoomOnPoint(VAR img:TImage; CONST cx, cy: longint; CONST zoomFactor: double);
  begin
  end;

FUNCTION T_generalImageGenrationAlgorithm.toString:ansistring;
  VAR i:longint;
  begin
    result:=getAlgorithmName+'[';
    for i:=0 to numberOfParameters-1 do
    result:=result+myParams.toString(parameterDescription(i),getParameter(i),true)+'#';
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
    for i:=0 to numberOfParameters-1 do
      result:=result and canParseParameterValue(parameterDescription(i),stringParts[i],parsedParameters[i],true);
    if result then for i:=0 to numberOfParameters-1 do setParameter(i,parsedParameters[i]);
    setLength(parsedParameters,0);
    setLength(stringParts,0);
  end;

CONSTRUCTOR T_colorGradientAlgorithm.create;  begin resetParameters(0); end;
DESTRUCTOR T_colorGradientAlgorithm.destroy;  begin end;
FUNCTION T_colorGradientAlgorithm.getAlgorithmName: ansistring; begin result:='Linear_gradient'; end;
PROCEDURE T_colorGradientAlgorithm.resetParameters(CONST style: longint);
  begin
    c0:=black;
    c1:=black;
    angle:=0;
  end;

FUNCTION T_colorGradientAlgorithm.numberOfParameters: longint;
  begin
    result:=3;
  end;

FUNCTION T_colorGradientAlgorithm.parameterDescription(CONST index: byte): T_parameterDescription;
  begin
    case index of
      0: result:=myParams.parameterDescription('Angle (degrees)',pt_float);
      1: result:=myParams.parameterDescription('Color 1',pt_color);
      2: result:=myParams.parameterDescription('Color 2',pt_color);
    end;
  end;

PROCEDURE T_colorGradientAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    case index of
      0: angle:=value.floatValue[0];
      1: c0:=value.color;
      2: c1:=value.color;
    end;
  end;

FUNCTION T_colorGradientAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    case index of
      0: result.floatValue[0]:=angle;
      1: result.color:=c0;
      2: result.color:=c1;
    end;
  end;

PROCEDURE T_colorGradientAlgorithm.prepareImage(CONST forPreview: boolean);
  VAR x,y:longint;
      nx,ny,w:single;
      dc:T_floatColor;
  begin
    progressor.logStart;
    dc:=c1-c0;
    nx:=2*system.cos(pi/180*angle)/renderImage.diagonal;
    ny:=2*system.sin(pi/180*angle)/renderImage.diagonal;
    for y:=0 to renderImage.height-1 do if not(progressor.cancellationRequested) then
    for x:=0 to renderImage.width-1 do begin
      w:=(x-renderImage.width/2)*nx+(y-renderImage.height/2)*ny;
      if      w> 1 then w:=1
      else if w<-1 then w:=0
      else w:=(w+1)*0.5;
      renderImage[x,y]:=c0+w*dc;
    end;
    progressor.logEnd;
  end;

CONSTRUCTOR T_perlinNoiseAlgorithm.create;
  begin resetParameters(0); end;

DESTRUCTOR T_perlinNoiseAlgorithm.destroy;
  begin end;

FUNCTION T_perlinNoiseAlgorithm.getAlgorithmName: ansistring;
  begin result:='Perlin_Noise'; end;

PROCEDURE T_perlinNoiseAlgorithm.resetParameters(CONST style: longint);
  begin
    if style=0 then seed:=0 else seed:=randseed;
    scaleFactor:=0.5;
    amplitudeFactor:=0.5;
  end;

FUNCTION T_perlinNoiseAlgorithm.numberOfParameters: longint;
  begin result:=3; end;

FUNCTION T_perlinNoiseAlgorithm.parameterDescription(CONST index: byte): T_parameterDescription;
  begin
    case index of
      0: result:=myParams.parameterDescription('seed',pt_integer);
      1: result:=myParams.parameterDescription('scaleFactor',pt_float,0.001,0.999);
      2: result:=myParams.parameterDescription('amplitudeFactor',pt_float,0.001,0.999);
    end;
  end;

PROCEDURE T_perlinNoiseAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    case index of
      0: seed:=value.intValue[0];
      1: scaleFactor:=value.floatValue[0];
      2: amplitudeFactor:=value.floatValue[0];
    end;
  end;

FUNCTION T_perlinNoiseAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    case index of
      0: result.intValue[0]:=seed;
      1: result.floatValue[0]:=scaleFactor;
      2: result.floatValue[0]:=amplitudeFactor;
    end;
  end;

PROCEDURE T_perlinNoiseAlgorithm.prepareImage(CONST forPreview: boolean);
  VAR perlinTable:array[0..31,0..31] of single;
      perlinLine :array of array[0..31] of single;

  PROCEDURE initPerlinTable;
    VAR i,j:longint;
    begin
      if seed=-1 then randomize
                 else randseed:=seed;
      for i:=0 to 31 do for j:=0 to 31 do perlinTable[i,j]:=random-0.5;
      randomize;
    end;

  PROCEDURE updatePerlinLine(y:double; lineIdx:longint; amplitude:single); inline;
    VAR ix,iy:longint;
        j0,j1,j2,j3:longint;
        q0,q1,q2,q3:single;
    begin
      if (lineIdx and 1)>0 then y:=-y;
      if (lineIdx and 2)>0 then amplitude:=-amplitude;
      iy:=floor(y); y:=y-iy;
      q0:=amplitude*(y*(-0.5+(1-y*0.5)*y));
      q1:=amplitude*(1+y*y*(-2.5+(3*y)*0.5));
      q2:=amplitude*(y*(0.5+(2-(3*y)*0.5)*y));
      q3:=amplitude*((-0.5+y*0.5)*y*y);
      j0:=(iy  ) and 31;
      j1:=(iy+1) and 31;
      j2:=(iy+2) and 31;
      j3:=(iy+3) and 31;
      if (lineIdx and 4)=0 then begin
        for ix:=0 to 31 do perlinLine[lineIdx,ix]:=
          perlinTable[ix,j0]*q0+
          perlinTable[ix,j1]*q1+
          perlinTable[ix,j2]*q2+
          perlinTable[ix,j3]*q3;
      end else begin
        for ix:=0 to 31 do perlinLine[lineIdx,ix]:=
          perlinTable[j0,ix]*q0+
          perlinTable[j1,ix]*q1+
          perlinTable[j2,ix]*q2+
          perlinTable[j3,ix]*q3;
      end;
    end;

    FUNCTION getSmoothValue(x:double; lineIdx:longint):single; inline;
      VAR ix:longint;
      begin
        ix:=floor(x); x:=x-ix;
        result:=perlinLine[lineIdx,(ix  ) and 31]*(x*(-0.5+(1-x*0.5)*x))   +
                perlinLine[lineIdx,(ix+1) and 31]*(1+x*x*(-2.5+(3*x)*0.5)) +
                perlinLine[lineIdx,(ix+2) and 31]*(x*(0.5+(2-(3*x)*0.5)*x))+
                perlinLine[lineIdx,(ix+3) and 31]*((-0.5+x*0.5)*x*x)       ;
      end;

  VAR xRes,yRes:longint;
      x,y,l,lMax:longint;
      scale:array of double;
      amplitude:array of double;
      aid:double;
  begin
    progressor.logStart;
    initPerlinTable;
    xRes:=renderImage.width;
    yRes:=renderImage.height;

    if scaleFactor>1 then begin
      scaleFactor:=1/scaleFactor;
      amplitudeFactor:=1/amplitudeFactor;
    end;

    aid:=0;
    setLength(amplitude,1);
    setLength(scale,1);
    amplitude[0]:=1;
    scale[0]:=1/renderImage.diagonal;
    lMax:=0;
    while (scale[lMax]<4) and (amplitude[lMax]>1E-3) do begin
      aid:=aid+amplitude[lMax];
      inc(lMax);
      setLength(scale,lMax+1);
      setLength(amplitude,lMax+1);
      scale    [lMax]:=scale    [lMax-1]/scaleFactor;
      amplitude[lMax]:=amplitude[lMax-1]*amplitudeFactor;
    end;
    setLength(perlinLine,lMax);

    for l:=0 to lMax-1 do amplitude[l]:=amplitude[l]*2/aid;
    for y:=0 to yRes-1 do if not(progressor.cancellationRequested) then begin
      for l:=0 to lMax-1 do updatePerlinLine((y-yRes*0.5)*scale[L],L,amplitude[L]);
      for x:=0 to xRes-1 do begin
        aid:=0.5;
        for l:=0 to lMax-1 do aid:=aid+getSmoothValue((x-xRes*0.5)*scale[L],L);
        if aid>1 then aid:=1
        else if aid<0 then aid:=0;
        renderImage[x,y]:=aid*white;
      end;
    end;
    setLength(perlinLine,0);
    setLength(scale,0);
    setLength(amplitude,0);
    progressor.logEnd;
  end;

CONSTRUCTOR T_scaledImageGenerationAlgorithm.create;
  begin
    scaler.create(100,100,0,0,1,0);
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.resetParameters(CONST style: longint);
  begin
    scaler.recreate(100,100,0,0,1,0);
  end;

FUNCTION T_scaledImageGenerationAlgorithm.numberOfParameters: longint;
  begin
    result:=4;
  end;

FUNCTION T_scaledImageGenerationAlgorithm.parameterDescription(CONST index: byte): T_parameterDescription;
  begin
    case index of
      0: result:=myParams.parameterDescription('center x',pt_float);
      1: result:=myParams.parameterDescription('center y',pt_float);
      2: result:=myParams.parameterDescription('zoom',pt_float,1E-20);
      3: result:=myParams.parameterDescription('rotation',pt_float);
    end;
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index>=4 then exit;
    case index of
      0: scaler.setCenterX(value.floatValue[0]);
      1: scaler.setCenterY(value.floatValue[0]);
      2: scaler.setZoom(value.floatValue[0]);
      3: scaler.setRotation(value.floatValue[0]);
    end;
    scalerChanagedSinceCalculation:=true;
  end;

FUNCTION T_scaledImageGenerationAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index>=4 then exit;
    case index of
      0: result.floatValue[0]:= scaler.getCenterX;
      1: result.floatValue[0]:= scaler.getCenterY;
      2: result.floatValue[0]:= scaler.getZoom;
      3: result.floatValue[0]:= scaler.getRotation;
    end;
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.panByPixels(VAR plotImage:TImage; CONST dx, dy: longint);
  VAR rectA,rectB:TRect;
  begin
    scaler.moveCenter(dx,dy);
    scalerChanagedSinceCalculation:=true;
    rectA.Top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.Top:=0+dy;
    rectB.Left:=0+dx;
    rectB.Right:=plotImage.width+dx;
    rectB.Bottom:=plotImage.height+dy;
    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.zoomOnPoint(VAR plotImage:TImage;  CONST cx, cy: longint; CONST zoomFactor: double);
  VAR rectA,rectB:TRect;
  begin
    scaler.chooseScreenRef(cx,cy);
    scaler.setZoom(scaler.getZoom*zoomFactor);
    rectA.Top:=0;
    rectA.Left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.Top:=round((-cy)*zoomFactor+cy);
    rectB.Left:=round((-cx)*zoomFactor+cx);
    rectB.Right:=round((plotImage.width-cx)*zoomFactor+cx);
    rectB.Bottom:=round((plotImage.height-cy)*zoomFactor+cy);
    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
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

FUNCTION T_functionPerPixelAlgorithm.parameterDescription(CONST index: byte): T_parameterDescription;
  begin
    if index<inherited numberOfParameters
    then result:=inherited parameterDescription(index)
    else result:=myParams.parameterDescription('render tolerance',pt_float,1E-3,1E3);
  end;

PROCEDURE T_functionPerPixelAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited parameterDescription(index)
    else renderTolerance:=value.floatValue[0];
  end;

FUNCTION T_functionPerPixelAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters
    then result:=inherited getParameter(index)
    else result.floatValue[0]:=renderTolerance;
  end;

TYPE T_workerThreadTask=record algorithm:P_functionPerPixelAlgorithm; chunk:T_colChunk; forPreview,needCopy:boolean; end;
     P_workerThreadTask=^T_workerThreadTask;
     T_rawDataWorkerThreadTask=record algorithm:P_functionPerPixelViaRawDataAlgorithm; workerIndex,modul:longint; end;
     P_rawDataWorkerThreadTask=^T_rawDataWorkerThreadTask;

VAR workerThreadTask       : array[0..7] of T_workerThreadTask;
    rawDataWorkerThreadTask: array[0..7] of T_rawDataWorkerThreadTask;
    renderThreadID         : array[0..7] of TThreadID;

FUNCTION prepareChunkWorkerThread(p:pointer):ptrint;
  begin
    with P_workerThreadTask(p)^ do algorithm^.prepareChunk(chunk,forPreview);
    result:=0;
  end;

FUNCTION prepareRawDataWorkerThread(p:pointer):ptrint;
  begin
    with P_rawDataWorkerThreadTask(p)^ do algorithm^.prepareRawMap(workerIndex,modul);
    result:=0;
  end;

PROCEDURE T_functionPerPixelAlgorithm.prepareImage(CONST forPreview: boolean);
  VAR threadCount:longint;
      i:longint;
      pendingChunks:T_pendingList;
      chunkCount:longint;
      chunksDone:longint=0;
      anyStarted:boolean;
      sleepTime:longint=0;
  begin
    if progressor.calculating then exit;

    for i:=0 to length(workerThreadTask)-1 do workerThreadTask[i].needCopy:=false;

    progressor.logStart;
    threadCount:=getNumberOfCPUs;
    if threadCount>length(workerThreadTask) then threadCount:=length(workerThreadTask);

    markChunksAsPending(renderImage);
    pendingChunks:=getPendingList(renderImage);
    chunkCount:=length(pendingChunks);
    for i:=0 to threadCount-1 do if length(pendingChunks)>0 then begin
      workerThreadTask[i].algorithm:=@self;
      workerThreadTask[i].forPreview:=forPreview;
      workerThreadTask[i].chunk.initForChunk(renderImage.width,renderImage.height,pendingChunks[length(pendingChunks)-1]);
      workerThreadTask[i].needCopy:=true;
      setLength(pendingChunks,length(pendingChunks)-1);
      renderThreadID[i]:=beginThread(@prepareChunkWorkerThread,@workerThreadTask[i]);
    end;
    while (length(pendingChunks)>0) and not(progressor.cancellationRequested) do begin
      anyStarted:=false;
      for i:=0 to threadCount-1 do if (length(pendingChunks)>0) and (waitForThreadTerminate(renderThreadID[i],1)=0) then begin

        inc(chunksDone);
        workerThreadTask[i].chunk.copyTo(renderImage);
        progressor.logFractionDone(chunksDone/chunkCount);

        workerThreadTask[i].chunk.initForChunk(renderImage.width,renderImage.height,pendingChunks[length(pendingChunks)-1]);
        setLength(pendingChunks,length(pendingChunks)-1);
        renderThreadID[i]:=beginThread(@prepareChunkWorkerThread,@workerThreadTask[i]);
        anyStarted:=true;
        sleepTime:=1;
      end;
      if not(anyStarted) then inc(sleepTime);
      sleep(sleepTime);
    end;
    for i:=0 to threadCount-1 do if (workerThreadTask[i].needCopy) then begin
      repeat sleep(1) until (waitForThreadTerminate(renderThreadID[i],1)=0);

      inc(chunksDone);
      workerThreadTask[i].chunk.copyTo(renderImage);
      if chunksDone<chunkCount then progressor.logFractionDone(chunksDone/chunkCount);
    end;
    progressor.logEnd;
  end;

PROCEDURE T_functionPerPixelAlgorithm.prepareChunk(VAR chunk: T_colChunk; CONST forPreview: boolean);
  VAR i,j,k,k0,k1:longint;
  begin
    for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do with chunk.col[i,j] do rest:=getColorAt(chunk.getPicX(i),chunk.getPicY(j),scaler.transform(chunk.getPicX(i),chunk.getPicY(j)));
    if forPreview then exit;
    while (renderTolerance>1E-3) and chunk.markAlias(renderTolerance) do
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

CONSTRUCTOR T_functionPerPixelViaRawDataAlgorithm.create;
  begin
    temporaryRawMap:=nil;
    rawMapIsOutdated:=false;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getRawDataAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor;
  CONST h0:T_Complex=(re:-1/3; im:-1/3);
        h1:T_Complex=(re:-1/3; im: 2/3);
        h2:T_Complex=(re: 2/3; im:-1/3);
        l0= sqrt(1/3);
        l1=-sqrt(1/3);
        l2= sqrt(1/3);

  FUNCTION toSphere(x:T_Complex):T_floatColor; inline;
    VAR t:double;
    begin
      t:=4/(4+x.re*x.re+x.im*x.im);
      result[0]:=x.re*t;
      result[1]:=x.im*t;
      result[2]:=t*2;
    end;

  FUNCTION toSphereZ(x:T_Complex):double; inline;
    begin
      result:=4/(4+x.re*x.re+x.im*x.im);
      result:=result*2;
    end;

  VAR i:longint;
      x,x1,x2,
      c,c1,c2:T_Complex;
      r,s,v:T_floatColor;
      d0,d1,d2:double;
      sphereZ:double;
  begin
    c:=xy;
    iterationStart(c,x);
    i:=0;
    result:=black;
    s:=black;
    v:=black;
    case colorSource of
      0..2: begin
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
        s:=s+(maxDepth-i)*r;
        v:=v+(maxDepth-i)*newColor(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
        result[0]:=((v[0]+v[1]+v[2])/maxDepth-(s*s)*(1/(maxDepth*maxDepth)));
        result[1]:=arg(x)/(2*pi); if result[1]<0 then result[1]:=result[1]+1;
      end;
      //to do: chaos measure scaled by diagonal, not by pixels!!!
      6..8 : begin
        while (i<maxDepth) and (isValid(x)) do begin
          iterationStep(c,x);
          r:=toSphere(x)*0.1;
          result:=result*0.9+r;
          inc(i);
        end;
        while (i<maxDepth) do begin result:=result*0.9+r; inc(i); end;
      end;
      else begin
        c :=xy+h0*scaler.getZoom; iterationStart(c ,x );
        c1:=xy+h1*scaler.getZoom; iterationStart(c1,x1);
        c2:=xy+h2*scaler.getZoom; iterationStart(c2,x2);

        d0:=0; d1:=0;  d2:=0; i:=0;
        while isValid(x) and isValid(x1) and isValid(x2) and (i<maxDepth) do begin
          iterationStep(c ,x ); d0:=d0+toSphereZ(x);
          iterationStep(c1,x1); d1:=d1+toSphereZ(x1);
          iterationStep(c2,x2); d2:=d2+toSphereZ(x2);
          inc(i);
        end;
        //compute and normalize normal vector:-----------------//
        d0:=sqrt(d0/maxDepth);                                 //
        d1:=sqrt(d1/maxDepth)-d0;                              //
        d2:=sqrt(d2/maxDepth)-d0;                              //
        d0:=1/sqrt(d1*d1+d2*d2+scaler.getZoom*scaler.getZoom); //
        result[2]:=(scaler.getZoom*d0);                        //
        result[0]:=(d1            *d0);                        //
        result[1]:=(d2            *d0);                        //
        //-------------------:compute and normalize normal vector
      end;
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getColor(CONST rawData:T_floatColor):T_floatColor; virtual;
  begin
  end;

FUNCTION T_functionPerPixelViaRawDataAlgorithm.getColorAt(CONST ix,iy:longint; CONST xy:T_Complex): T_floatColor;
  begin
    result:=getColor(getRawDataAt(ix,iy,xy));
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.prepareRawMap(CONST workerIndex,modul:longint);
  VAR i,j:longint;
  begin
    for j:=0 to temporaryRawMap^.height-1 do if j mod modul=workerIndex then
    for i:=0 to temporaryRawMap^.width-1 do temporaryRawMap^[i,j]:=getRawDataAt(i,j,scaler.transform(i,j));
  end;

PROCEDURE T_functionPerPixelViaRawDataAlgorithm.prepareImage(CONST forPreview: boolean);
  VAR i,x,y:longint;
      threadCount:longint;
  begin
    if progressor.calculating then exit;
    if forPreview then begin
      rawMapIsOutdated:=rawMapIsOutdated or (temporaryRawMap=nil);
      if temporaryRawMap=nil then new(temporaryRawMap,create(renderImage.width,renderImage.height));
      temporaryRawMap^.mutateType(rs_float);
      temporaryRawMap^.resize(renderImage.width,renderImage.height, res_dataResize);
      if rawMapIsOutdated then begin
        threadCount:=getNumberOfCPUs;
        if threadCount>length(rawDataWorkerThreadTask) then threadCount:=length(rawDataWorkerThreadTask);

        progressor.logStart;
        for i:=0 to threadCount-1 do with rawDataWorkerThreadTask[i] do begin
          modul:=threadCount;
          workerIndex:=i;
          algorithm:=@self;
          renderThreadID[i]:=beginThread(@prepareRawDataWorkerThread,@rawDataWorkerThreadTask[i]);
        end;
        for i:=0 to threadCount-1 do begin
          repeat sleep(1) until (waitForThreadTerminate(renderThreadID[i],1)=0);
          progressor.logFractionDone((i+1)/threadCount);
        end;

        rawMapIsOutdated:=false;
      end;
      for y:=0 to renderImage.height-1 do for x:=0 to renderImage.width-1 do renderImage[x,y]:=getColor(temporaryRawMap^[x,y]);
      progressor.logEnd;
    end else begin
      if temporaryRawMap<>nil then begin
        dispose(temporaryRawMap,destroy);
        temporaryRawMap:=nil;
      end;
      inherited prepareImage(false);
    end;
  end;

VAR k:longint;
INITIALIZATION
  colorGradientAlgorithm.create; registerAlgorithm(@colorGradientAlgorithm);
  perlinNoiseAlgorithm.create;   registerAlgorithm(@perlinNoiseAlgorithm);
  progressor.createSimple;
  renderImage.create(1,1);
  for k:=0 to length(workerThreadTask)-1 do workerThreadTask[k].chunk.create;

FINALIZATION
  setLength(algorithms,0);
  colorGradientAlgorithm.destroy;
  perlinNoiseAlgorithm.destroy;
  renderImage.destroy;
  for k:=0 to length(workerThreadTask)-1 do workerThreadTask[k].chunk.destroy;

end.

