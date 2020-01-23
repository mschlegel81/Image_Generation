UNIT generationBasics;
INTERFACE
USES pixMaps,
     mypics;
CONST
    C_nullSourceOrTargetFileName='-';
TYPE
  { T_imageGenerationContextConfiguration }
  P_imageGenerationContextConfiguration=^T_imageGenerationContextConfiguration;
  T_imageGenerationContextConfiguration=object
    private
      //Fix over config lifetime:
      fRetainIntermediateResults:boolean;
      //Varying over config lifetime:
      initialImageFilename             :string;
      fInitialResolution,
      fImageSizeLimit                  :T_imageDimensions;
      cachedInitialImageWasScaled      :boolean;
      cachedInitialImage               :P_rawImage;
      PROCEDURE clearImage;
      PROCEDURE setInitialResolution(CONST res:T_imageDimensions);
      PROCEDURE setMaximumResolution(CONST res:T_imageDimensions);
    public
      workflowFilename:string;
      previewQuality,
      intermediateResultsPreviewQuality:boolean;
      CONSTRUCTOR create(CONST retainIntermediateResults_: boolean);
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
      PROPERTY retainIntermediateResults:boolean read fRetainIntermediateResults;
      PROPERTY initialImageName:string read initialImageFilename;
  end;

  P_structuredMessage=^T_structuredMessage;

  { T_structuredMessage }

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

  { T_structuredMessageQueue }

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

IMPLEMENTATION
USES sysutils;
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
    result:=FormatDateTime('yyyy-mm-dd hh:mm:ss',fMessageCreatedAtTime)+' ';
    if (fStepIndex>=0) then result+='('+intToStr(fStepIndex)+') ';
    if fIndicatesError then result+='ERROR: ';
    result+=fMessageText;
  end;

PROCEDURE T_imageGenerationContextConfiguration.clearImage;
  begin
    if cachedInitialImage<>nil then dispose(cachedInitialImage,destroy);
    cachedInitialImage:=nil;
    cachedInitialImageWasScaled:=false;
  end;

CONSTRUCTOR T_imageGenerationContextConfiguration.create(CONST retainIntermediateResults_: boolean);
  begin
    cachedInitialImage:=nil;
    setDefaults;
    fRetainIntermediateResults:=retainIntermediateResults_;
  end;

DESTRUCTOR T_imageGenerationContextConfiguration.destroy;
  begin
    clearImage;
  end;

PROCEDURE T_imageGenerationContextConfiguration.setDefaults;
  begin
    workflowFilename                 :='';
    initialImageFilename             :='';
    previewQuality                   :=false;
    intermediateResultsPreviewQuality:=false;
    fInitialResolution               :=imageDimensions(1920,1080);
    fImageSizeLimit                  :=C_maxImageDimensions;
    clearImage;
  end;

PROCEDURE T_imageGenerationContextConfiguration.setInitialResolution(CONST res: T_imageDimensions);
  begin
    initialResolution:=res;
    fImageSizeLimit:=fImageSizeLimit.max(fInitialResolution);
  end;

PROCEDURE T_imageGenerationContextConfiguration.setMaximumResolution(CONST res: T_imageDimensions);
  begin
    fImageSizeLimit:=res;
    fInitialResolution:=fImageSizeLimit.min(fInitialResolution);
  end;

PROCEDURE T_imageGenerationContextConfiguration.setInitialImage(VAR image: T_rawImage);
  begin
    clearImage;
    new(cachedInitialImage,create(image));
    initialImageFilename:=C_nullSourceOrTargetFileName;
  end;

PROCEDURE T_imageGenerationContextConfiguration.setInitialImage(CONST fileName: string);
  begin
    if fileName=initialImageFilename then exit;
    clearImage;
    initialImageFilename:=fileName;
  end;

PROCEDURE T_imageGenerationContextConfiguration.prepareImageForWorkflow(VAR image: T_rawImage);
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

FUNCTION T_imageGenerationContextConfiguration.getFirstTodoStep:string;
  begin
    if (initialImageFilename<>'') and (initialImageFilename<>C_nullSourceOrTargetFileName)
    then result:='load:'+initialImageFilename
    else result:='resize:'+intToStr(fInitialResolution.width)+','+intToStr(fInitialResolution.height);
  end;

FUNCTION T_imageGenerationContextConfiguration.limitImageSize(VAR image: T_rawImage): boolean;
  begin
    if image.dimensions.fitsInto(fImageSizeLimit) then exit(false);
    image.resize(fImageSizeLimit,res_fit);
    result:=true;
  end;

FUNCTION T_imageGenerationContextConfiguration.limitedDimensionsForResizeStep(
  CONST tgtDim: T_imageDimensions): T_imageDimensions;
  begin
    //TODO : implement me
    result:=tgtDim;
  end;

end.
