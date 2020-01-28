UNIT generationBasics;
INTERFACE
USES pixMaps,
     mypics;
CONST
    C_workflowExtension='.WF';
    C_nullSourceOrTargetFileName='-';
    C_loadStatementName='load';
    C_resizeStatementName='resize';
TYPE
  F_simpleCallback=PROCEDURE of object;
  P_imageWorkflowConfiguration=^T_imageWorkflowConfiguration;
  T_imageWorkflowConfiguration=object
    private
      //Varying over config lifetime:
      initialImageFilename             :string;
      fInitialResolution,
      fImageSizeLimit                  :T_imageDimensions;
      cachedInitialImageWasScaled      :boolean;
      cachedInitialImage               :P_rawImage;
      onStep0Changed                   :F_simpleCallback;
      PROCEDURE clearImage;
      PROCEDURE setInitialResolution(CONST res:T_imageDimensions);
      PROCEDURE setMaximumResolution(CONST res:T_imageDimensions);
    public
      workflowFilename:string;
      intermediateResultsPreviewQuality:boolean;
      CONSTRUCTOR create(CONST step0Changed:F_simpleCallback);
      DESTRUCTOR destroy;
      PROCEDURE setDefaults;
      PROCEDURE setInitialImage     (VAR image:T_rawImage);
      PROCEDURE setInitialImage     (CONST fileName:string);
      PROCEDURE prepareImageForWorkflow(VAR image:T_rawImage);
      FUNCTION getFirstTodoStep:string;
      {Returns true if the image was modified}
      FUNCTION limitImageSize(VAR image:T_rawImage):boolean;
      FUNCTION limitedDimensionsForResizeStep(CONST tgtDim:T_imageDimensions):T_imageDimensions;
      PROPERTY sizeLimit        :T_imageDimensions read fImageSizeLimit    write setMaximumResolution;
      PROPERTY initialResolution:T_imageDimensions read fInitialResolution write setInitialResolution;
      PROPERTY initialImageName:string read initialImageFilename;
      FUNCTION associatedDirectory:string;
  end;

  P_structuredMessage=^T_structuredMessage;
  T_structuredMessage=object
    private
      fMessageCreatedAtTime:double;
      fIndicatesError:boolean;
      fStepIndex:longint;
      fMessageText:string;
      nextMessage:P_structuredMessage;
    public
      CONSTRUCTOR create(CONST message:string; CONST isError:boolean=false; CONST relatesToStep:longint=-1);
      DESTRUCTOR destroy;
      FUNCTION toString:string;
      PROPERTY messageText:string read fMessageText;
      PROPERTY stepIndex:longint read fStepIndex;
      PROPERTY indicatesError:boolean read fIndicatesError;
      PROPERTY getTime:double read fMessageCreatedAtTime;
  end;

  P_structuredMessageQueue=^T_structuredMessageQueue;
  T_structuredMessageQueue=object
    private
      queueCs:TRTLCriticalSection;
      first,last:P_structuredMessage;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION get:P_structuredMessage;
      PROCEDURE Post(CONST message:string; CONST isError:boolean=true; CONST relatesToStep:longint=-1);
  end;
VAR messageStringLengthLimit:longint=100;
IMPLEMENTATION
USES sysutils;

FUNCTION stringEllipse(CONST s:string):string;
  begin
    if length(s)>messageStringLengthLimit
    then result:=copy(s,1,messageStringLengthLimit-3)+'...'
    else result:=s;
  end;

CONSTRUCTOR T_structuredMessageQueue.create;
  begin
    initCriticalSection(queueCs);
    first:=nil;
    last:=nil;
  end;

DESTRUCTOR T_structuredMessageQueue.destroy;
  VAR m:P_structuredMessage;
  begin
    enterCriticalSection(queueCs);
    try
      m:=get;
      while m<>nil do begin
        dispose(m,destroy);
        m:=get;
      end;
    finally
      leaveCriticalSection(queueCs);
      doneCriticalSection(queueCs);
    end;
  end;

FUNCTION T_structuredMessageQueue.get: P_structuredMessage;
  begin
    enterCriticalSection(queueCs);
    try
      result:=first;
      if first <>nil then first:=first^.nextMessage;
      if result<>nil then result^.nextMessage:=nil;
    finally
      leaveCriticalSection(queueCs);
    end;
  end;

PROCEDURE T_structuredMessageQueue.Post(CONST message: string; CONST isError: boolean; CONST relatesToStep: longint);
  VAR m:P_structuredMessage;
  begin
    enterCriticalSection(queueCs);
    try
      new(m,create(message,isError,relatesToStep));
      if first=nil
      then first:=m
      else last^.nextMessage:=m;
      last:=m;
      {$ifdef debugMode}
      writeln(stdErr,'DEBUG T_structuredMessageQueue.Post: ',m^.toString);
      {$endif}
    finally
      leaveCriticalSection(queueCs);
    end;
  end;

CONSTRUCTOR T_structuredMessage.create(CONST message: string; CONST isError: boolean; CONST relatesToStep: longint);
  begin
    fMessageCreatedAtTime:=now;
    fMessageText:=message;
    fIndicatesError:=isError;
    fStepIndex:=relatesToStep;
    nextMessage:=nil;
  end;

DESTRUCTOR T_structuredMessage.destroy;
  begin
  end;

FUNCTION T_structuredMessage.toString: string;
  begin
    result:=FormatDateTime('hh:mm:ss',fMessageCreatedAtTime)+' ';
    if (fStepIndex>=0) then result+='('+intToStr(fStepIndex)+') ';
    if fIndicatesError then result+='ERROR: ';
    result:=stringEllipse(result+fMessageText);
  end;

PROCEDURE T_imageWorkflowConfiguration.clearImage;
  begin
    if cachedInitialImage<>nil then dispose(cachedInitialImage,destroy);
    cachedInitialImage:=nil;
    cachedInitialImageWasScaled:=false;
  end;

CONSTRUCTOR T_imageWorkflowConfiguration.create(
  CONST step0Changed: F_simpleCallback);
  begin
    onStep0Changed:=step0Changed;
    cachedInitialImage:=nil;
    setDefaults;
  end;

DESTRUCTOR T_imageWorkflowConfiguration.destroy;
  begin
    clearImage;
  end;

PROCEDURE T_imageWorkflowConfiguration.setDefaults;
  begin
    workflowFilename                 :='';
    initialImageFilename             :='';
    intermediateResultsPreviewQuality:=false;
    fInitialResolution               :=imageDimensions(1920,1080);
    fImageSizeLimit                  :=C_maxImageDimensions;
    clearImage;
  end;

PROCEDURE T_imageWorkflowConfiguration.setInitialResolution(
  CONST res: T_imageDimensions);
  begin
    if initialResolution=res then exit;
    initialResolution:=res;
    fImageSizeLimit:=fImageSizeLimit.max(fInitialResolution);
    if onStep0Changed<>nil then onStep0Changed();
  end;

PROCEDURE T_imageWorkflowConfiguration.setMaximumResolution(
  CONST res: T_imageDimensions);
  begin
    if sizeLimit=res then exit;
    fImageSizeLimit:=res;
    fInitialResolution:=fImageSizeLimit.min(fInitialResolution);
    if onStep0Changed<>nil then onStep0Changed();
  end;

PROCEDURE T_imageWorkflowConfiguration.setInitialImage(
  VAR image: T_rawImage);
  begin
    clearImage;
    new(cachedInitialImage,create(image));
    initialImageFilename:=C_nullSourceOrTargetFileName;
    if onStep0Changed<>nil then onStep0Changed();
  end;

PROCEDURE T_imageWorkflowConfiguration.setInitialImage(
  CONST fileName: string);
  begin
    if fileName=initialImageFilename then exit;
    clearImage;
    initialImageFilename:=fileName;
    if onStep0Changed<>nil then onStep0Changed();
  end;

PROCEDURE T_imageWorkflowConfiguration.prepareImageForWorkflow(
  VAR image: T_rawImage);
  PROCEDURE reloadInitialImage;
    begin
      new(cachedInitialImage,create(initialImageFilename));
      cachedInitialImageWasScaled:=false;
    end;
  begin
    if initialImageFilename<>'' then begin
      if initialImageFilename=C_nullSourceOrTargetFileName then begin
        //Special source without associated file
        image.copyFromPixMap(cachedInitialImage^);
        limitImageSize(image);
      end else begin
        if cachedInitialImage=nil then reloadInitialImage;
        if not(cachedInitialImage^.dimensions.fitsInto(fImageSizeLimit)) then begin
          //This block handles images being too large
          if cachedInitialImageWasScaled then reloadInitialImage;
          cachedInitialImageWasScaled:=limitImageSize(cachedInitialImage^);
        end else if cachedInitialImageWasScaled
          and not((cachedInitialImage^.dimensions.height=fImageSizeLimit.height) or
                  (cachedInitialImage^.dimensions.width =fImageSizeLimit.width)) then begin
          //This block handles images being too small after a previous scaling
          reloadInitialImage;
          cachedInitialImageWasScaled:=limitImageSize(cachedInitialImage^);
        end;
      end;
    end else begin
      image.resize(fInitialResolution,res_dataResize);
      image.drawCheckerboard;
    end;
  end;

FUNCTION T_imageWorkflowConfiguration.getFirstTodoStep: string;
  begin
    if (initialImageFilename<>'') and (initialImageFilename<>C_nullSourceOrTargetFileName)
    then result:=C_loadStatementName+':'+initialImageFilename
    else result:=C_resizeStatementName+':'+intToStr(fInitialResolution.width)+','+intToStr(fInitialResolution.height);
  end;

FUNCTION T_imageWorkflowConfiguration.limitImageSize(
  VAR image: T_rawImage): boolean;
  begin
    if image.dimensions.fitsInto(fImageSizeLimit) then exit(false);
    image.resize(fImageSizeLimit,res_fit);
    result:=true;
  end;

FUNCTION T_imageWorkflowConfiguration.limitedDimensionsForResizeStep(
  CONST tgtDim: T_imageDimensions): T_imageDimensions;
  begin
    if tgtDim.fitsInto(fImageSizeLimit) then exit(tgtDim);
    result:=fImageSizeLimit.getFittingRectangle(tgtDim.width/tgtDim.height);
  end;

FUNCTION T_imageWorkflowConfiguration.associatedDirectory: string;
  begin
    if workflowFilename=''
    then result:=paramStr(0)
    else result:=workflowFilename;
    result:=ExtractFileDir(result);
  end;

end.

