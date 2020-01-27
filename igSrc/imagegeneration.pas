UNIT imageGeneration;
INTERFACE
USES math,
     myGenerics,
     complex,
     myColors,
     myParams,
     Interfaces, ExtCtrls, Graphics,
     mypics,
     imageContexts;

TYPE
  P_generalImageGenrationAlgorithm=^T_generalImageGenrationAlgorithm;
  T_generalImageGenrationAlgorithm=object(T_imageOperation)
    private
      parameterDescriptors:array of P_parameterDescription;
      name:ansistring;
    protected
      FUNCTION addParameter(CONST name_: string;
                            CONST typ_: T_parameterType;
                            CONST minValue_: double= -infinity;
                            CONST maxValue_: double=  infinity):P_parameterDescription;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy; virtual;
      FUNCTION parameterResetStyles:T_arrayOfString; virtual;
      PROCEDURE resetParameters(CONST style:longint); virtual; abstract;
      PROCEDURE cleanup; virtual;
      FUNCTION numberOfParameters:longint; virtual;
      FUNCTION parameterDescription(CONST index:byte):P_parameterDescription;
      PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual; abstract;
      FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual; abstract;
      FUNCTION toFullString():ansistring;
      FUNCTION canParseParametersFromString(CONST s:ansistring; CONST doParse:boolean=false):boolean;
      FUNCTION parValue(CONST index:byte; CONST i0:longint; CONST i1:longint=0; CONST i2:longint=0; CONST i3:longint=0):T_parameterValue;
      FUNCTION parValue(CONST index:byte; CONST f0:double; CONST f1:double=0; CONST f2:double=0):T_parameterValue;
      FUNCTION parValue(CONST index:byte; CONST color:T_rgbFloatColor):T_parameterValue;
      FUNCTION parValue(CONST index:byte; CONST txt:ansistring; CONST sizeLimit:longint=-1):T_parameterValue;
      FUNCTION dependsOnImageBefore:boolean; virtual;
      FUNCTION toString(nameMode:T_parameterNameMode):string; virtual;
      FUNCTION alterParameter(CONST newParameterString:string):boolean; virtual;
  end;

CONST SCALER_PARAMETER_COUNT=4;
TYPE
  P_scaledImageGenerationAlgorithm=^T_scaledImageGenerationAlgorithm;
  T_scaledImageGenerationAlgorithm=object(T_generalImageGenrationAlgorithm)
    scaler:T_scaler;
    scalerChanagedSinceCalculation:boolean;

    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
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
    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_rgbFloatColor; virtual; abstract;
    PROCEDURE execute(CONST context:P_abstractWorkflow); virtual;
    PROCEDURE prepareChunk(CONST context:P_abstractWorkflow; VAR chunk:T_colChunk); virtual;
  end;

  P_pixelThrowerAlgorithm=^T_pixelThrowerAlgorithm;
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
    DESTRUCTOR destroy; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE execute(CONST context:P_abstractWorkflow); virtual;
    PROCEDURE prepareSlice(CONST context:P_abstractWorkflow; CONST index:longint); virtual; abstract;
    FUNCTION dependsOnImageBefore:boolean; virtual;
  end;

  P_workerThreadTodo=^T_workerThreadTodo;
  T_workerThreadTodo=object(T_parallelTask)
    algorithm:P_functionPerPixelAlgorithm;
    chunkIndex:longint;

    CONSTRUCTOR create(CONST algorithm_:P_functionPerPixelAlgorithm;
                       CONST chunkIndex_:longint);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

  P_pixelThrowerTodo=^T_pixelThrowerTodo;
  T_pixelThrowerTodo=object(T_parallelTask)
    target:P_rawImage;
    algorithm:P_pixelThrowerAlgorithm;
    sliceIndex:longint;

    CONSTRUCTOR create(CONST algorithm_:P_pixelThrowerAlgorithm; CONST index:longint; CONST target_:P_rawImage);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

  T_constructorHelper=FUNCTION():P_generalImageGenrationAlgorithm;
  P_algorithmMeta=^T_algorithmMeta;
  T_algorithmMeta=object(T_imageOperationMeta)
    private
      fIndex:longint;
      cs:TRTLCriticalSection;
      constructorHelper:T_constructorHelper;
      primaryInstance:P_generalImageGenrationAlgorithm;
      scaler:boolean;
      light :boolean;
      juliaP:boolean;
    public
      CONSTRUCTOR create(CONST name_:string;
                         CONST constructorHelper_:T_constructorHelper;
                         CONST scaler_,light_,juliaP_:boolean);
      DESTRUCTOR destroy; virtual;
      FUNCTION prototype:P_generalImageGenrationAlgorithm;
      PROPERTY getName:string read name;
      PROPERTY hasScaler:boolean read scaler;
      PROPERTY hasLight :boolean read light;
      PROPERTY hasJuliaP:boolean read juliaP;
      //FUNCTION canParseParametersFromString(CONST s:ansistring; CONST doParse:boolean=false):boolean;
      FUNCTION parse(CONST specification:ansistring):P_imageOperation; virtual;
      FUNCTION getSimpleParameterDescription: P_parameterDescription; virtual;
      //PROCEDURE prepareImage(CONST specification:ansistring; CONST context:P_abstractWorkflow);
      //PROCEDURE prepareImageAccordingToCurrentSpecification(CONST context:P_abstractWorkflow);
      //FUNCTION prepareImageInBackground(CONST image:P_rawImage; CONST forPreview:boolean):boolean;
      PROPERTY index:longint read fIndex;
      FUNCTION getDefaultOperation:P_imageOperation; virtual;
  end;

VAR defaultGenerationStep:string;
PROCEDURE registerAlgorithm(CONST algName:ansistring; CONST p:T_constructorHelper; CONST scaler,light,julia:boolean);
//FUNCTION prepareImage(CONST specification:ansistring; CONST context:P_imageGenerationContext):boolean;
FUNCTION getAlgorithmOrNil(CONST specification:ansistring; CONST doPrepare:boolean):P_algorithmMeta;
VAR imageGenerationAlgorithms   : array of P_algorithmMeta;
IMPLEMENTATION
USES sysutils,  types, myStringUtil,mySys,darts;

PROCEDURE registerAlgorithm(CONST algName:ansistring; CONST p:T_constructorHelper; CONST scaler,light,julia:boolean);
  VAR meta:P_algorithmMeta;
      k:longint;
      def:P_imageOperation;
  begin
    new(meta,create(algName,p,scaler,light,julia));
    k:=length(imageGenerationAlgorithms);
    setLength(imageGenerationAlgorithms,k+1);
    imageGenerationAlgorithms[k]:=meta;
    meta^.fIndex:=k;
    registerOperation(meta);
    if k=0 then begin
      def:=meta^.getDefaultOperation;
      defaultGenerationStep:=def^.toString(tsm_forSerialization);
      dispose(def,destroy);
    end;
  end;
//
//FUNCTION prepareImage(CONST specification:ansistring; CONST context:P_abstractWorkflow):boolean;
//  VAR algorithm:P_algorithmMeta;
//  begin
//    algorithm:=getAlgorithmOrNil(specification,true);
//    if algorithm=nil then exit(false);
//    algorithm^.prepareImage(specification,context);
//    result:=true;
//  end;
//
FUNCTION getAlgorithmOrNil(CONST specification:ansistring; CONST doPrepare:boolean):P_algorithmMeta;
  VAR algorithm:P_algorithmMeta;
  begin
    result:=nil;
    for algorithm in imageGenerationAlgorithms do if algorithm^.prototype^.canParseParametersFromString(specification,doPrepare) then exit(algorithm);
  end;

{ T_algorithmMeta }

CONSTRUCTOR T_algorithmMeta.create(CONST name_: string;
  CONST constructorHelper_: T_constructorHelper; CONST scaler_, light_,
  juliaP_: boolean);
  begin
    initCriticalSection(cs);
    name:=name_;
    constructorHelper:=constructorHelper_;
    scaler:=scaler_;
    light:=light_;
    juliaP:=juliaP_;
    primaryInstance:=nil;
  end;

DESTRUCTOR T_algorithmMeta.destroy;
  begin
    enterCriticalSection(cs);
    leaveCriticalSection(cs);
    doneCriticalSection(cs);
    if (primaryInstance<>nil) then dispose(primaryInstance,destroy);
  end;

FUNCTION T_algorithmMeta.prototype: P_generalImageGenrationAlgorithm;
  begin
    enterCriticalSection(cs);
    if primaryInstance=nil then begin
      primaryInstance:=constructorHelper();
      primaryInstance^.name:=name;
    end;
    result:=primaryInstance;
    leaveCriticalSection(cs);
  end;

FUNCTION T_algorithmMeta.parse(CONST specification: ansistring
  ): P_imageOperation;
  VAR newOp:P_generalImageGenrationAlgorithm;
  begin
    enterCriticalSection(cs);
    try
      newOp:=constructorHelper();
      if newOp^.canParseParametersFromString(specification,true)
      then result:=newOp
      else begin
        result:=nil;
        dispose(newOp,destroy);
      end;
    finally
      leaveCriticalSection(cs);
    end;
  end;

FUNCTION T_algorithmMeta.getSimpleParameterDescription: P_parameterDescription;
  begin
    result:=nil;
  end;

FUNCTION T_algorithmMeta.getDefaultOperation: P_imageOperation;
  begin
    result:=constructorHelper();
    P_generalImageGenrationAlgorithm(result)^.resetParameters(0);
  end;

//PROCEDURE T_algorithmMeta.prepareImage(CONST specification:ansistring; CONST context:P_abstractWorkflow);
//  VAR workerInstance:P_generalImageGenrationAlgorithm;
//  begin
//    workerInstance:=constructorHelper();
//    workerInstance^.name:=name;
//    workerInstance^.canParseParametersFromString(specification,true);
//    workerInstance^.prepareImage(context,true);
//    workerInstance^.cleanup;
//    dispose(workerInstance,destroy);
//  end;
//
//PROCEDURE T_algorithmMeta.prepareImageAccordingToCurrentSpecification(CONST context:P_abstractWorkflow);
//  begin
//    prototype^.prepareImage(context,true);
//  end;
//
//FUNCTION T_algorithmMeta.prepareImageInBackground(CONST image: P_rawImage; CONST forPreview: boolean): boolean;
//  VAR context:T_abstractWorkflow;
//  begin
//    context.queue:=@defaultProgressQueue;
//    context.waitForFinish:=false;
//    context.forPreview:=forPreview;
//    context.targetImage:=image;
//    result:=prototype^.prepareImage(context);
//  end;

CONSTRUCTOR T_pixelThrowerTodo.create(CONST algorithm_: P_pixelThrowerAlgorithm; CONST index: longint; CONST target_: P_rawImage);
  begin
    algorithm:=algorithm_;
    sliceIndex:=index;
    target:=target_;
  end;

DESTRUCTOR T_pixelThrowerTodo.destroy;
  begin
  end;

PROCEDURE T_pixelThrowerTodo.execute;
  begin
    algorithm^.prepareSlice(containedIn,sliceIndex);
  end;

CONSTRUCTOR T_pixelThrowerAlgorithm.create;
  CONST no_and_yes:array[0..1] of string=('no','yes');
  begin
    inherited create;
    addParameter('alpha',pt_float,0);
    addParameter('quality factor',pt_float,1);
    addParameter('has background',pt_enum,0,1)^.setEnumValues(no_and_yes);
    initCriticalSection(renderTempData.flushCs);
  end;

PROCEDURE T_pixelThrowerAlgorithm.cleanup;
  begin
    with renderTempData do if backgroundImage<>nil then begin
      dispose(backgroundImage,destroy);
      backgroundImage:=nil;
    end;
  end;

DESTRUCTOR T_pixelThrowerAlgorithm.destroy;
  begin
    cleanup;
    inherited destroy;
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
    else if hasBackground
         then result:=parValue(index,1)
         else result:=parValue(index,0);
    end;
  end;

PROCEDURE T_pixelThrowerAlgorithm.execute(
  CONST context: P_abstractWorkflow);
  VAR x,y:longint;
      todo:P_pixelThrowerTodo;
      newAASamples:longint;
      useQualityMultiplier:double=1;
  begin with context^ do begin
    if previewQuality and (qualityMultiplier>1)
    then useQualityMultiplier:=1
    else useQualityMultiplier:=qualityMultiplier;

    if image.pixelCount<=0 then exit;
    newAASamples:=min(64,max(1,trunc(useQualityMultiplier/par_alpha)));

    clearQueue;
    scaler.rescale(image.dimensions.width,image.dimensions.height);

    with renderTempData do begin
      if hasBackground then new(backgroundImage,create(image));
      //TODO: Implement clear method
      for y:=0 to image.dimensions.height-1 do for x:=0 to image.dimensions.width-1 do image[x,y]:=BLACK;
      samplesFlushed:=0;
      xRes:=image.dimensions.width ;
      yRes:=image.dimensions.height;
      maxPixelX:=image.dimensions.width -0.5;
      maxPixelY:=image.dimensions.height-0.5;
      aaSamples:=newAASamples;
      useQuality:=useQualityMultiplier/aaSamples;
      coverPerSample:=par_alpha/useQuality;
      antiCoverPerSample:=1-coverPerSample;
      timesteps:=round(useQuality*image.pixelCount);
      for x:=0 to aaSamples-1 do begin
        new(todo,create(@self,x,@image));
        enqueue(todo);
      end;
    end;
    waitForFinishOfParallelTasks;
  end; end;

FUNCTION T_pixelThrowerAlgorithm.dependsOnImageBefore: boolean;
  begin
    result:=hasBackground;
  end;

CONSTRUCTOR T_workerThreadTodo.create(CONST algorithm_: P_functionPerPixelAlgorithm; CONST chunkIndex_: longint);
  begin
    inherited create;
    algorithm:=algorithm_;
    chunkIndex:=chunkIndex_;
  end;

DESTRUCTOR T_workerThreadTodo.destroy;
  begin
  end;

PROCEDURE T_workerThreadTodo.execute;
  VAR chunk:T_colChunk;
  begin
    chunk.create;
    chunk.initForChunk(containedIn^.image.dimensions.width,containedIn^.image.dimensions.height,chunkIndex);
    algorithm^.prepareChunk(containedIn,chunk);
    containedIn^.image.copyFromChunk(chunk);
    chunk.destroy;
  end;

CONSTRUCTOR T_generalImageGenrationAlgorithm.create;
  begin
    setLength(parameterDescriptors,0);
  end;

DESTRUCTOR T_generalImageGenrationAlgorithm.destroy;
  VAR i:longint;
  begin
    cleanup;
    for i:=0 to length(parameterDescriptors)-1 do if parameterDescriptors[i]<>nil then dispose(parameterDescriptors[i],destroy);
    setLength(parameterDescriptors,0);
  end;

FUNCTION T_generalImageGenrationAlgorithm.addParameter(CONST name_: string;
  CONST typ_: T_parameterType; CONST minValue_: double; CONST maxValue_: double
  ): P_parameterDescription;
  begin
    new(result,create(name_,typ_,minValue_,maxValue_));
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

FUNCTION T_generalImageGenrationAlgorithm.toFullString(): ansistring;
  VAR i:longint;
      p:array of array[0..1] of T_parameterValue;
  begin
    setLength(p,numberOfParameters);
    for i:=0 to numberOfParameters-1 do p[i,1]:=getParameter(i);
    resetParameters(0);
    for i:=0 to numberOfParameters-1 do p[i,0]:=getParameter(i);
    for i:=0 to numberOfParameters-1 do setParameter(i,p[i,1]);

    result:='';
    for i:=0 to numberOfParameters-1 do
    result+=getParameter(i).toString(tsm_forSerialization)+';';
    result:=name+'['+copy(result,1,length(result)-1)+']';
  end;

FUNCTION T_generalImageGenrationAlgorithm.toString(nameMode: T_parameterNameMode): string;
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
    result+=getParameter(i).toString(tsm_forSerialization)+';';
    result:=name+'['+copy(result,1,length(result)-1)+']';
  end;

FUNCTION T_generalImageGenrationAlgorithm.alterParameter(CONST newParameterString: string): boolean;
  begin
    result:=false;
  end;

FUNCTION T_generalImageGenrationAlgorithm.canParseParametersFromString(
  CONST s: ansistring; CONST doParse: boolean): boolean;
  VAR parsedParameters:array of T_parameterValue;
      paramRest:ansistring;
      stringParts:T_arrayOfString;
      i,j:longint;
      match:boolean=false;
  begin
    if not(startsWith(s,name+'[')) then begin
      exit(false);
    end;
    if not(endsWith(s,']')) then begin
      exit(false);
    end;
    paramRest:=copy(s,length(name+'[')+1,length(s)-length(name)-2);
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

FUNCTION T_generalImageGenrationAlgorithm.parValue(CONST index: byte;
  CONST i0: longint; CONST i1: longint; CONST i2: longint; CONST i3: longint
  ): T_parameterValue;
  begin
    result.createFromValue(parameterDescriptors[index],i0,i1,i2,i3);
  end;

FUNCTION T_generalImageGenrationAlgorithm.parValue(CONST index: byte;
  CONST f0: double; CONST f1: double; CONST f2: double): T_parameterValue;
  begin
    result.createFromValue(parameterDescriptors[index],f0,f1,f2);
  end;

FUNCTION T_generalImageGenrationAlgorithm.parValue(CONST index: byte;
  CONST color: T_rgbFloatColor): T_parameterValue;
  begin
    result.createFromValue(parameterDescriptors[index],color);
  end;

FUNCTION T_generalImageGenrationAlgorithm.parValue(CONST index: byte;
  CONST txt: ansistring; CONST sizeLimit: longint): T_parameterValue;
  begin
    result.createFromValue(parameterDescriptors[index],txt,sizeLimit);
  end;

FUNCTION T_generalImageGenrationAlgorithm.dependsOnImageBefore: boolean;
  begin
    result:=false;
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

DESTRUCTOR T_scaledImageGenerationAlgorithm.destroy;
  begin
    scaler.destroy;
    inherited destroy;
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
    if index>=4 then exit(parValue(index,0.0));
    case index of
      0: result:=parValue(index,scaler.getCenterX);
      1: result:=parValue(index,scaler.getCenterY);
      2: result:=parValue(index,scaler.getZoom);
    else result:=parValue(index,scaler.getRotation);
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

PROCEDURE T_functionPerPixelAlgorithm.execute(CONST context:P_abstractWorkflow);
  VAR i:longint;
      pendingChunks:T_pendingList;

  FUNCTION todo(CONST index:longint):P_workerThreadTodo;
    begin new(result,create(@self,index)); end;

  begin with context^ do begin
    clearQueue;
    scaler.rescale(image.dimensions.width,image.dimensions.height);
    scalerChanagedSinceCalculation:=false;
    image.markChunksAsPending;
    pendingChunks:=image.getPendingList;
    for i:=0 to length(pendingChunks)-1 do enqueue(todo(pendingChunks[i]));
    waitForFinishOfParallelTasks;
  end; end;

PROCEDURE T_functionPerPixelAlgorithm.prepareChunk(CONST context:P_abstractWorkflow; VAR chunk:T_colChunk);
  VAR i,j,k,k0,k1:longint;
  begin
    for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do with chunk.col[i,j] do rest:=getColorAt(chunk.getPicX(i),chunk.getPicY(j),scaler.transform(chunk.getPicX(i),chunk.getPicY(j)));
    if context^.previewQuality then exit;
    while (renderTolerance>1E-3) and chunk.markAlias(renderTolerance) and not(context^.cancellationRequested) do
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

end.

