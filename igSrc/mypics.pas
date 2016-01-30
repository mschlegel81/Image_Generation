UNIT mypics;
INTERFACE
{$fputype sse3}
USES myColors,
     {$ifdef UNIX}cmem,cthreads,{$endif}
     dos,sysutils,Interfaces, ExtCtrls, Graphics, IntfGraphics, GraphType,types,myGenerics, mySys,math
     {$ifdef useExtensions} ,cmdLineParseUtil {$endif};

{$define include_interface}
TYPE
  T_rawStyle=(rs_24bit,
              rs_float,
              rs_singleColorPseudoImage);
  T_resizeStyle=(res_exact,
                 res_cropToFill,
                 res_fit);
  T_imageCombinationDataSource=(src_RGB,
                                src_HSV,
                                src_stash,
                                src_file);
  T_imageManipulationType=(
                    imt_none,
    {Image access:} imt_loadImage,imt_saveImage,imt_saveJpgWithSizeLimit, imt_stashImage, imt_unstashImage,
    {Geometry:}     imt_resize, imt_fit, imt_fill, imt_crop,
    {Combination:}  imt_add, imt_subtract, imt_multiply, imt_divide, imt_screen, imt_maxOf, imt_minOf,

                    imt_setColor);


                              //fk_gamma,fk_gammaRGB,fk_gammaHSV,
                              //fk_normalize,fk_normalizeR,fk_normalizeG,fk_normalizeB,fk_normalizeH,fk_normalizeS,fk_normalizeV,
                              //fk_compress,fk_compressR,fk_compressG,fk_compressB,fk_compressH,fk_compressS,fk_compressV,
                              //fk_project,fk_limit,fk_limitLow,fk_grey,fk_sepia,fk_tint,fk_hue,
                              //fk_invert,fk_invertR,fk_invertG,fk_invertB,fk_invertH,fk_invertS,fk_invertV,fk_abs,fk_quantize,fk_mono,
                              //fk_extract_alpha


TYPE
  P_rawImage=^T_rawImage;
  P_imageManipulationWorkflow=^T_imageManipulationWorkflow;

  { T_imageManipulationStep }

  T_imageManipulationStep=object
    imageManipulationType:T_imageManipulationType;
    combinationSource:T_imageCombinationDataSource;
    floatParam:array of double;
    intParam:array of integer;
    stringParam:shortString;

    CONSTRUCTOR create(CONST typ:T_imageManipulationType; CONST p0:double; CONST p1:double=0; CONST p2:double=0; CONST p3:double=0);
    CONSTRUCTOR create(CONST typ:T_imageManipulationType; CONST s:string);
    DESTRUCTOR destroy;

    PROCEDURE execute(CONST image:P_rawImage; CONST workflow:P_imageManipulationWorkflow);

    PROCEDURE loadFromFile(VAR handle:file);
    PROCEDURE saveToFile(VAR handle:file);
  end;

  { T_imageManipulationWorkflow }

  T_imageManipulationWorkflow=object
    private
      imageStash:array of P_rawImage;
      step:array of record
             manipulation:T_imageManipulationStep;
             intermediate:P_rawImage;
             stepMessage :ansistring;
           end;
      progress:record
        stepNumber:longint;
        stepMessage:ansistring;
        done:boolean;
      end;
      hasError:boolean;

    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;

      PROCEDURE raiseError(CONST message:ansistring);
      PROCEDURE setCurrentStepMessage(CONST message:ansistring);

      PROCEDURE clearStash;
      PROCEDURE clearIntermediate;

      PROCEDURE execute;
      PROCEDURE executeOnImage(CONST img:P_rawImage; CONST storeIntermediate:boolean; CONST skipFirst:boolean=true);

      FUNCTION executing:boolean;
      FUNCTION executingStep:longint;
      FUNCTION executingFunction:ansistring;

      PROCEDURE loadFromFile(VAR handle:file);
      PROCEDURE saveToFile(VAR handle:file);

  end;

  { T_rawImage }

  T_rawImage=object
    private
      xRes,yRes:longint;
      style   :T_rawStyle;
      datCol  :T_floatColor;
      datFloat:P_floatColor;
      dat24bit:P_24Bit;

      //Accessors:--------------------------------------------------------------
      PROCEDURE setPixel(CONST x,y:longint; CONST value:T_floatColor);
      FUNCTION getPixel(CONST x,y:longint):T_floatColor;

      PROCEDURE setPixel24Bit(CONST x,y:longint; CONST value:T_24Bit);
      FUNCTION getPixel24Bit(CONST x,y:longint):T_24Bit;
      //--------------------------------------------------------------:Accessors
      //Helper routines:--------------------------------------------------------
      PROCEDURE copyToImage(CONST srcRect:TRect; VAR destImage: TImage);
      //--------------------------------------------------------:Helper routines
    public
      CONSTRUCTOR create(CONST width_,height_:longint);
      CONSTRUCTOR create(CONST fileName:ansistring);
      CONSTRUCTOR create(CONST color:T_floatColor);
      CONSTRUCTOR create(VAR original:T_rawImage);
      DESTRUCTOR destroy;
      //Access per pixel:-------------------------------------------------------
      FUNCTION width:longint;
      FUNCTION height:longint;
      PROPERTY pixel     [x,y:longint]:T_floatColor read getPixel write setPixel; default;
      PROPERTY pixel24Bit[x,y:longint]:T_24Bit read getPixel24Bit write setPixel24Bit;
      PROCEDURE mutateType(CONST newType:T_rawStyle);
      //-------------------------------------------------------:Access per pixel
      //TImage interface:-------------------------------------------------------
      PROCEDURE copyToImage(VAR destImage: TImage);
      PROCEDURE copyFromImage(VAR srcImage: TImage);
      PROCEDURE copyFromImage(VAR srcImage: T_rawImage);
      //-------------------------------------------------------:TImage interface
      //File interface:---------------------------------------------------------
      PROCEDURE saveToFile(CONST fileName:ansistring);
      PROCEDURE loadFromFile(CONST fileName:ansistring);
      PROCEDURE saveJpgWithSizeLimit(CONST fileName:ansistring; CONST sizeLimit:SizeInt; CONST workflow:P_imageManipulationWorkflow=nil);
      //---------------------------------------------------------:File interface
      //Geometry manipulations:-------------------------------------------------
      PROCEDURE resize(CONST newWidth,newHeight:longint; CONST resizeStyle:T_resizeStyle);
      PROCEDURE flip;
      PROCEDURE flop;
      PROCEDURE rotRight;
      PROCEDURE rotLeft;
      PROCEDURE crop(CONST x0,x1,y0,y1:longint);
      //-------------------------------------------------:Geometry manipulations
      //Statistic accessors:----------------------------------------------------
      FUNCTION histogram:T_compoundHistogram;
      FUNCTION histogram(CONST x,y:longint; CONST smoothingKernel:T_arrayOfDouble):T_compoundHistogram;
      //----------------------------------------------------:Statistic accessors
  end;

F_displayErrorFunction=PROCEDURE(CONST s:ansistring);

VAR compressionQualityPercentage:longint=100;
    displayErrorFunction:F_displayErrorFunction=nil;

IMPLEMENTATION

{ T_imageManipulationWorkflow }

CONSTRUCTOR T_imageManipulationWorkflow.create;
  begin

  end;

DESTRUCTOR T_imageManipulationWorkflow.destroy;
  VAR i:longint;
  begin
    clearIntermediate;
    clearStash;
    for i:=0 to length(step)-1 do step[i].manipulation.destroy;
    setLength(step,0);
  end;

PROCEDURE T_imageManipulationWorkflow.raiseError(CONST message: ansistring);
  begin
    hasError:=true;
    if displayErrorFunction<>nil
    then displayErrorFunction(message)
    else writeln(stderr,message);
    beep;
  end;

PROCEDURE T_imageManipulationWorkflow.setCurrentStepMessage(
  CONST message: ansistring);
  begin
    if (progress.stepNumber>=0) and (progress.stepNumber<length(step)) then begin
      step[progress.stepNumber].stepMessage:=message;
      writeln(stderr,message);
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.clearIntermediate;
  VAR i:longint;
  begin
    for i:=0 to length(step)-1 do with step[i] do begin
      if intermediate<>nil then dispose(intermediate,destroy);
      intermediate:=nil;
      stepMessage:='';
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.clearStash;
  VAR i:longint;
  begin
    for i:=0 to length(imageStash)-1 do if imageStash[i]<>nil then dispose(imageStash[i],destroy);
    setLength(imageStash,0);
  end;

PROCEDURE T_imageManipulationWorkflow.execute;
  VAR img:T_rawImage;
  begin
    if length(step)=0 then exit;
    progress.stepNumber :=0;
    progress.done:=false;
    if (step[0].manipulation.imageManipulationType=imt_loadImage) then begin
      img.create(step[0].manipulation.stringParam);
    end else if (step[0].manipulation.imageManipulationType=imt_resize) then begin
      img.create(round(step[0].manipulation.floatParam[0]),
                 round(step[0].manipulation.floatParam[1]));
    end else begin
      raiseError('Workflow must begin with loading an image or defining an image with a given resolution!');
      exit;
    end;
    executeOnImage(@img,false,true);
    img.destroy;
  end;

PROCEDURE T_imageManipulationWorkflow.executeOnImage(CONST img: P_rawImage;
  CONST storeIntermediate: boolean; CONST skipFirst: boolean);
  PROCEDURE stepDone(CONST storeRecommended:boolean);
    begin
      if storeIntermediate and storeRecommended then with step[progress.stepNumber] do begin
        new(intermediate,create(img^));
        intermediate^.mutateType(rs_24bit);
      end;
      inc(progress.stepNumber);
      if progress.stepNumber<length(step) then step[progress.stepNumber].stepMessage:='';
    end;

  begin
    clearIntermediate;
    clearStash;
    progress.stepNumber :=0;
    progress.stepMessage:='';
    if skipFirst then begin
      setCurrentStepMessage('Initial image');
      stepDone(true);
    end;
    while not(hasError) and (progress.stepNumber<length(step)) do with step[progress.stepNumber] do begin
      manipulation.execute(img,@self);
      stepDone(not(manipulation.imageManipulationType in [imt_none, imt_saveImage, imt_saveJpgWithSizeLimit, imt_stashImage]));
    end;
    progress.done:=true;
    clearStash;
  end;

FUNCTION T_imageManipulationWorkflow.executing: boolean;
  begin
    result:=not(progress.done or hasError);
  end;

FUNCTION T_imageManipulationWorkflow.executingStep: longint;
  begin result:=progress.stepNumber; end;

FUNCTION T_imageManipulationWorkflow.executingFunction: ansistring;
  begin result:=progress.stepMessage; end;

PROCEDURE T_imageManipulationWorkflow.loadFromFile(VAR handle: file);
  VAR stepCount:longint=0;
      i:longint;
  begin
    clearIntermediate;
    clearStash;
    BlockRead(handle,stepCount,sizeOf(longint));
    setLength(step,stepCount);
    for i:=0 to length(step)-1 do with step[i] do begin
      manipulation.create(imt_none,'');
      manipulation.loadFromFile(handle);
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.saveToFile(VAR handle: file);
  VAR stepCount,i:longint;
  begin
    stepCount:=length(step);
    BlockWrite(handle,stepCount,sizeOf(longint));
    for i:=0 to length(step)-1 do step[i].manipulation.saveToFile(handle);
  end;

//FUNCTION tempName:string;
//  VAR i:longint;
//  begin
//    i:=-1;
//    repeat
//      inc(i);
//      result:=intToStr(i);
//      while length(result)<5 do result:='0'+result;
//      result:='temp'+result+'.bmp';
//    until not(fileExists(result));
//  end;
//
//FUNCTION tempName(prefix:string):string;
//  VAR i:longint;
//  begin
//    i:=-1;
//    repeat
//      inc(i);
//      result:=intToStr(i);
//      while length(result)<5 do result:='0'+result;
//      result:=prefix+result+'.bmp';
//    until not(fileExists(result));
//  end;

{ T_imageManipulationStep }

CONSTRUCTOR T_imageManipulationStep.create(CONST typ: T_imageManipulationType;
  CONST p0: double; CONST p1: double; CONST p2: double; CONST p3: double);
  begin
    imageManipulationType:=typ;
    floatParam[0]:=p0;
    floatParam[1]:=p1;
    floatParam[2]:=p2;
    floatParam[3]:=p3;
    stringParam:='';
  end;

CONSTRUCTOR T_imageManipulationStep.create(CONST typ: T_imageManipulationType;
  CONST s: string);
  begin
    imageManipulationType:=typ;
    floatParam[0]:=0;
    floatParam[1]:=0;
    floatParam[2]:=0;
    floatParam[3]:=0;
    stringParam:=s;
  end;

DESTRUCTOR T_imageManipulationStep.destroy;
  begin

  end;

PROCEDURE T_imageManipulationStep.execute(CONST image: P_rawImage; CONST workflow:P_imageManipulationWorkflow);
  FUNCTION i0:longint; begin result:=round(floatParam[0]); end;
  FUNCTION i1:longint; begin result:=round(floatParam[1]); end;
  FUNCTION i2:longint; begin result:=round(floatParam[2]); end;
  FUNCTION i3:longint; begin result:=round(floatParam[3]); end;

  PROCEDURE stash;
    VAR oldLength,i:longint;
    begin
      with workflow^ do if i0<0 then raiseError('Invalid stash index')
      else begin
        if i0>length(imageStash) then begin
          oldLength:=length(imageStash);
          setLength(imageStash,i0+1);
          for i:=oldLength to length(imageStash)-1 do imageStash[i]:=nil;
        end;
        if imageStash[i0]<>nil then dispose(imageStash[i0],destroy);
        new(imageStash[i0],create(image^));
      end;
    end;

  PROCEDURE unstash;
    begin
      with workflow^ do if (i0<0) or (i0>length(imageStash)) then raiseError('Invalid stash Index')
      else image^.copyFromImage(imageStash[i0]^);
    end;

  FUNCTION plausibleResolution:boolean;
    begin
      if (i0>0) and (i0<10000) and (i1>0) and (i1<10000) then result:=true
      else begin
        result:=false;
        workflow^.raiseError('Invalid resolution; Both values must be in range 1..9999.');
      end;
    end;

  PROCEDURE combine;
    FUNCTION colMult  (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=a[i]*b[i]; end;
    FUNCTION colDiv   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=a[i]/b[i]; end;
    FUNCTION colScreen(CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=1-(1-a[i])*(1-b[i]); end;
    FUNCTION colMax   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if a[i]>b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
    FUNCTION colMin   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if a[i]<b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
    VAR x,y:longint;
        other:P_rawImage;
        c1:T_floatColor;

    begin
      case combinationSource of
        src_RGB  : new(other,create(newColor(floatParam[0],floatParam[1],floatParam[2])));
        src_stash: with workflow^ do if (i0>=0) and (i0<length(imageStash))
                   then raiseError('Invalid stash Index')
                   else other:=imageStash[i0];
        src_file : new(other,create(stringParam));
        src_HSV: begin
          c1:=newColor(floatParam[0],floatParam[1],floatParam[2]);
          case imageManipulationType of
            imt_add      : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(toHSV(image^[x,y])+c1);
            imt_subtract : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(toHSV(image^[x,y])-c1);
            imt_multiply : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colMult  (toHSV(image^[x,y]),c1));
            imt_divide   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colDiv   (toHSV(image^[x,y]),c1));
            imt_screen   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colScreen(toHSV(image^[x,y]),c1));
            imt_maxOf    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colMax   (toHSV(image^[x,y]),c1));
            imt_minOf    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=fromHSV(colMin   (toHSV(image^[x,y]),c1));
          end;
          exit;
        end;
      end;
      case imageManipulationType of
        imt_add      : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=image^[x,y]+other^[x,y];
        imt_subtract : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=image^[x,y]-other^[x,y];
        imt_multiply : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMult  (image^[x,y],other^[x,y]);
        imt_divide   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colDiv   (image^[x,y],other^[x,y]);
        imt_screen   : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colScreen(image^[x,y],other^[x,y]);
        imt_maxOf    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMax   (image^[x,y],other^[x,y]);
        imt_minOf    : for y:=0 to image^.height-1 do for x:=0 to image^.width-1 do image^[x,y]:=colMin   (image^[x,y],other^[x,y]);
      end;
      if combinationSource in [src_RGB,src_file] then dispose(other,destroy);
    end;


  begin
    case imageManipulationType of
      imt_loadImage: image^.loadFromFile(stringParam);
      imt_saveImage: image^.saveToFile(stringParam);
      imt_saveJpgWithSizeLimit: image^.saveJpgWithSizeLimit(stringParam,i0,workflow);
      imt_stashImage: stash;
      imt_unstashImage: unstash;
      imt_resize: if plausibleResolution then image^.resize(i0,i1,res_exact);
      imt_fit   : if plausibleResolution then image^.resize(i0,i1,res_fit);
      imt_fill  : if plausibleResolution then image^.resize(i0,i1,res_cropToFill);
      imt_crop  : image^.crop(i0,i1,i2,i3);
      imt_add..imt_minOf: combine;
    end;
  end;

PROCEDURE T_imageManipulationStep.loadFromFile(VAR handle: file);
  begin
    BlockRead(handle,imageManipulationType,sizeOf(T_imageManipulationType));
    BlockRead(handle,floatParam,sizeOf(floatParam));
    BlockRead(handle,stringParam,sizeOf(shortString));
  end;

PROCEDURE T_imageManipulationStep.saveToFile(VAR handle: file);
  begin
    BlockWrite(handle,imageManipulationType,sizeOf(T_imageManipulationType));
    BlockWrite(handle,floatParam,sizeOf(floatParam));
    BlockWrite(handle,stringParam,sizeOf(shortString));
  end;

{ T_rawImage }

PROCEDURE T_rawImage.setPixel(CONST x, y: longint; CONST value: T_floatColor);
  begin
    if (x<0) or (x>=xRes) or (y<0) or (y>=yRes) then exit;
    if style<>rs_float then mutateType(rs_float);
    datFloat[x+y*xRes]:=value
  end;

FUNCTION T_rawImage.getPixel(CONST x, y: longint): T_floatColor;
  begin
    if (x<0) or (x>=xRes) or (y<0) or (y>=yRes) then exit(black);
    case style of
      rs_24bit   : result:=dat24bit[x+y*xRes];
      rs_float: result:=datFloat[x+y*xRes];
      else         result:=datCol;
    end;
  end;

PROCEDURE T_rawImage.setPixel24Bit(CONST x, y: longint; CONST value: T_24Bit);
  begin
    if (x<0) or (x>=xRes) or (y<0) or (y>=yRes) then exit;
    if style=rs_singleColorPseudoImage then mutateType(rs_24bit);
    case style of
      rs_24bit:    dat24bit[x+y*xRes]:=value;
      rs_float: datFloat[x+y*xRes]:=value;
    end;
  end;

FUNCTION T_rawImage.getPixel24Bit(CONST x, y: longint): T_24Bit;
  begin
    if (x<0) or (x>=xRes) or (y<0) or (y>=yRes) then exit(black24Bit);
    case style of
      rs_24bit   : result:=dat24bit[x+y*xRes];
      rs_float: result:=projectedColor(datFloat[x+y*xRes]);
      else result:=datCol;
    end;
  end;

CONSTRUCTOR T_rawImage.create(CONST width_, height_: longint);
  begin
    style:=rs_singleColorPseudoImage;
    xRes:=width_;
    yRes:=height_;
    datCol:=black;
  end;

CONSTRUCTOR T_rawImage.create(CONST fileName: ansistring);
  begin
    create(1,1);
    loadFromFile(fileName);
  end;

CONSTRUCTOR T_rawImage.create(CONST color:T_floatColor);
  begin
    style:=rs_singleColorPseudoImage;
    datCol:=color;
  end;

CONSTRUCTOR T_rawImage.create(VAR original: T_rawImage);
  begin
    style:=rs_singleColorPseudoImage;
    copyFromImage(original);
  end;

DESTRUCTOR T_rawImage.destroy;
  begin
    case style of
      rs_24bit: freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
      rs_float: freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
    end;
    style:=rs_singleColorPseudoImage;
    xRes:=0;
    yRes:=0;
  end;

FUNCTION T_rawImage.width: longint;  begin result:=xRes; end;
FUNCTION T_rawImage.height: longint; begin result:=yRes; end;

PROCEDURE T_rawImage.copyToImage(CONST srcRect: TRect; VAR destImage: TImage);
  VAR ScanLineImage,                 //image with representation as in T_24BitImage
      tempIntfImage: TLazIntfImage;  //image with representation as in TBitmap
      ImgFormatDescription: TRawImageDescription;
      x,y:longint;
      pc:T_24Bit;
      pix:PByte;
  begin
    ScanLineImage:=TLazIntfImage.create(srcRect.Right-srcRect.Left,srcRect.Bottom-srcRect.Top);
    ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(srcRect.Right-srcRect.Left,srcRect.Bottom-srcRect.Top);
    ImgFormatDescription.ByteOrder:=riboMSBFirst;
    ScanLineImage.DataDescription:=ImgFormatDescription;
    for y:=0 to srcRect.Bottom-srcRect.Top-1 do begin
      pix:=ScanLineImage.GetDataLineStart(y);
      for x:=0 to srcRect.Right-srcRect.Left-1 do begin
        pc:=getPixel24Bit(srcRect.Left+x,srcRect.Top+y);
        move(pc,(pix+3*x)^,3);
      end;
    end;
    destImage.Picture.Bitmap.width :=srcRect.Right-srcRect.Left;
    destImage.Picture.Bitmap.height:=srcRect.Bottom-srcRect.Top;
    tempIntfImage:=destImage.Picture.Bitmap.CreateIntfImage;
    tempIntfImage.CopyPixels(ScanLineImage);
    destImage.Picture.Bitmap.LoadFromIntfImage(tempIntfImage);
    tempIntfImage.free;
    ScanLineImage.free;
  end;


PROCEDURE T_rawImage.copyToImage(VAR destImage: TImage);
  begin
    copyToImage(Rect(0,0,xRes,yRes),destImage);
  end;

PROCEDURE T_rawImage.copyFromImage(VAR srcImage: TImage);
  VAR x,y:longint;
      ScanLineImage: TLazIntfImage;
      ImgFormatDescription: TRawImageDescription;
      pc:T_24Bit;
      pix:PByte;

  begin
    case style of
      rs_singleColorPseudoImage: begin
        xRes:=srcImage.width;
        yRes:=srcImage.height;
        style:=rs_24bit;
        getMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
      end;
      rs_24bit: begin
        if (xRes*yRes<>srcImage.width*srcImage.height) or (dat24bit<>nil) then begin
          if (dat24bit<>nil) then freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
          getMem(dat24bit,srcImage.width*srcImage.height*sizeOf(T_24Bit));
        end;
        xRes:=srcImage.width;
        yRes:=srcImage.height;
      end;
      rs_float:begin
        if datFloat<>nil then freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
        xRes:=srcImage.width;
        yRes:=srcImage.height;
        style:=rs_24bit;
        getMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
      end;
    end;

    ScanLineImage:=TLazIntfImage.create(xRes,yRes);
    ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(xRes,yRes);
    ImgFormatDescription.ByteOrder:=riboMSBFirst;
    ScanLineImage.DataDescription:=ImgFormatDescription;
    ScanLineImage.CopyPixels(srcImage.Picture.Bitmap.CreateIntfImage);
    for y:=0 to yRes-1 do begin
      pix:=ScanLineImage.GetDataLineStart(y);
      for x:=0 to xRes-1 do begin
        move((pix+3*x)^,pc,3);
        setPixel24Bit(x,y,pc);
      end;
    end;
    ScanLineImage.free;
  end;

PROCEDURE T_rawImage.mutateType(CONST newType: T_rawStyle);
  VAR i:longint;
      c24:T_24Bit;
  begin
    if newType=style then exit;
    case newType of
      rs_float: begin
        getMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
        if style=rs_24bit then begin
          for i:=0 to xRes*yRes-1 do datFloat[i]:=dat24bit[i];
          freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
        end else for i:=0 to xRes*yRes-1 do datFloat[i]:=datCol;
      end;
      rs_24bit: begin
        getMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
        if style=rs_float then begin
          for i:=0 to xRes*yRes-1 do dat24bit[i]:=projectedColor(datFloat[i]);
          freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
        end else begin
          c24:=projectedColor(datCol);
          for i:=0 to xRes*yRes-1 do dat24bit[i]:=c24;
        end;
      end;
      rs_singleColorPseudoImage: begin
        datCol:=black;
        if style=rs_24bit then begin
          for i:=0 to xRes*yRes-1 do datCol:=datCol+dat24bit[i];
          freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
        end else begin
          for i:=0 to xRes*yRes-1 do datCol:=datCol+datFloat[i];
          freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
        end;
        datCol:=datCol*(1/(xRes*yRes));
      end;
    end;
    style:=newType;
  end;

PROCEDURE T_rawImage.saveToFile(CONST fileName: ansistring);
  PROCEDURE storeDump;
    VAR handle:file of byte;
    begin
      assign(handle,fileName);
      rewrite(handle);
      BlockWrite(handle,xRes,sizeOf(longint));
      BlockWrite(handle,yRes,sizeOf(longint));
      BlockWrite(handle,style,sizeOf(T_rawStyle));
      case style of
        rs_24bit   :               BlockWrite(handle,PByte(dat24bit)^,xRes*yRes*sizeOf(T_24Bit));
        rs_float:               BlockWrite(handle,PByte(datFloat)^,xRes*yRes*sizeOf(T_floatColor));
        rs_singleColorPseudoImage: BlockWrite(handle,datCol,sizeOf(T_floatColor));
      end;
      close(handle);
    end;

  VAR ext:string;
      storeImg:TImage;
  begin
    ext:=uppercase(extractFileExt(fileName));
    if (ext='.JPG') or (ext='.JPEG') or (ext='.PNG') or (ext='.BMP') then begin
      storeImg:=TImage.create(nil);
      storeImg.SetInitialBounds(0,0,xRes,yRes);
      copyToImage(storeImg);
      if ext='.PNG' then storeImg.Picture.PNG.saveToFile(fileName) else
      if ext='.BMP' then storeImg.Picture.Bitmap.saveToFile(fileName)
                    else begin
        storeImg.Picture.Jpeg.CompressionQuality:=compressionQualityPercentage;
        storeImg.Picture.Jpeg.saveToFile(fileName);
      end;
      storeImg.free;
    end else storeDump;
  end;

PROCEDURE T_rawImage.loadFromFile(CONST fileName: ansistring);
  PROCEDURE restoreDump;
    VAR handle:file of byte;
    begin
      case style of
        rs_24bit:    freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
        rs_float: freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
      end;
      assign(handle,fileName);
      reset(handle);
      BlockRead(handle,xRes,sizeOf(longint));
      BlockRead(handle,yRes,sizeOf(longint));
      BlockRead(handle,style,sizeOf(T_rawStyle));
      case style of
        rs_24bit: begin
          getMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
          BlockRead(handle,PByte(dat24bit)^,xRes*yRes*sizeOf(T_24Bit));
        end;
        rs_float: begin
          getMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
          BlockRead(handle,PByte(datFloat)^,xRes*yRes*sizeOf(T_floatColor));
        end;
        rs_singleColorPseudoImage: BlockRead(handle,datCol,sizeOf(T_floatColor));
      end;
      close(handle);
    end;

  VAR ext:string;
      reStoreImg:TImage;
  begin
    if not(fileExists(fileName)) then exit;
    ext:=uppercase(extractFileExt(fileName));
    if (ext='.JPG') or (ext='.JPEG') or (ext='.PNG') or (ext='.BMP') then begin
      reStoreImg:=TImage.create(nil);
      reStoreImg.SetInitialBounds(0,0,10000,10000);
      if ext='.PNG' then reStoreImg.Picture.PNG.loadFromFile(fileName) else
      if ext='.BMP' then reStoreImg.Picture.Bitmap.loadFromFile(fileName)
                    else reStoreImg.Picture.Jpeg.loadFromFile(fileName);
      reStoreImg.SetBounds(0,0,reStoreImg.Picture.width,reStoreImg.Picture.height);
      copyFromImage(reStoreImg);
      reStoreImg.free;
    end else restoreDump;
  end;

PROCEDURE T_rawImage.copyFromImage(VAR srcImage: T_rawImage);
  VAR size:longint;
  begin
    case style of
      rs_24bit:    if dat24bit<>nil then freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
      rs_float: if datFloat<>nil then freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
    end;
    xRes:=srcImage.xRes;
    yRes:=srcImage.yRes;
    style:=srcImage.style;
    case style of
      rs_24bit: begin
        size:=xRes*yRes*sizeOf(T_24Bit);
        getMem(dat24bit,size);
        move(srcImage.dat24bit^,dat24bit^,size);
      end;
      rs_float: begin
        size:=xRes*yRes*sizeOf(T_floatColor);
        getMem(datFloat,size);
        move(srcImage.datFloat^,datFloat^,size);
      end;
      rs_singleColorPseudoImage:datCol:=srcImage.datCol;
    end;
  end;

PROCEDURE T_rawImage.saveJpgWithSizeLimit(CONST fileName: ansistring; CONST sizeLimit: SizeInt; CONST workflow:P_imageManipulationWorkflow=nil);
  VAR ext:string;
      storeImg:TImage;

  FUNCTION filesize(name:string):longint;
    VAR s:TSearchRec;
    begin
      if FindFirst(name,faAnyFile,s)=0
        then result:=s.size
        else result:=0;
      FindClose(s);
    end;

  VAR quality,lastSavedQuality:longint;
      sizes:array[0..100] of longint;

  PROCEDURE saveAtQuality(CONST quality:longint);
    begin
      storeImg.Picture.Jpeg.CompressionQuality:=quality;
      storeImg.Picture.Jpeg.saveToFile(fileName);
    end;

  FUNCTION getSizeAt(CONST quality:longint):longint;
    begin
      if quality>100 then exit(getSizeAt(100));
      if sizes[quality]<0 then begin
        saveAtQuality(quality);
        sizes[quality]:=filesize(fileName);
      end;
      result:=sizes[quality];
    end;

  begin
    if sizeLimit=0 then begin
      saveJpgWithSizeLimit(fileName,round(1677*sqrt(xRes*yRes)));
      exit;
    end;
    ext:=uppercase(extractFileExt(fileName));
    if ext<>'.JPG' then begin
      if workflow<>nil then workflow^.raiseError('Saving with size limit is only possible in JPEG format.');
      exit;
    end;
    storeImg:=TImage.create(nil);
    storeImg.SetInitialBounds(0,0,xRes,yRes);
    copyToImage(storeImg);

    lastSavedQuality:=-1;
    quality:=100;
    while (quality>4  ) and (getSizeAt(quality  )> sizeLimit) do dec(quality, 8);
    while (quality<100) and (getSizeAt(quality  )< sizeLimit) do inc(quality, 4);
    while (quality>0  ) and (getSizeAt(quality  )> sizeLimit) do dec(quality, 2);
    while (quality<100) and (getSizeAt(quality+1)<=sizeLimit) do inc(quality, 1);
    if lastSavedQuality<>quality then saveAtQuality(quality);
    if workflow<>nil then workflow^.setCurrentStepMessage('Image saved to "'+fileName+'" with quality '+intToStr(quality)+'%');
    storeImg.free;
  end;

PROCEDURE T_rawImage.resize(CONST newWidth, newHeight: longint;
  CONST resizeStyle: T_resizeStyle);
  VAR srcRect,destRect:TRect;
      dx,dy:longint;

  PROCEDURE resizeViaTImage;
    VAR srcImage,destImage:TImage;
    begin
      srcImage:=TImage.create(nil);
      srcImage.SetInitialBounds(0,0,srcRect.Right-srcRect.Left,srcRect.Bottom-srcRect.Top);
      copyToImage(srcRect,srcImage);
      destImage:=TImage.create(nil);
      destImage.SetInitialBounds(destRect.Left,destRect.Top,destRect.Right,destRect.Bottom);
      destImage.AntialiasingMode:=amOn;
      destImage.Canvas.AntialiasingMode:=amOn;
      destImage.Canvas.StretchDraw(destRect,srcImage.Picture.Graphic);
      srcImage.free;
      copyFromImage(destImage);
      destImage.free;
    end;

  begin
    case resizeStyle of
      res_exact: begin
        srcRect:=Rect(0,0,xRes,yRes);
        destRect:=Rect(0,0,newWidth,newHeight);
      end;
      res_fit: begin
        srcRect:=Rect(0,0,xRes,yRes);
        if newHeight/yRes<newWidth/xRes
        then destRect:=Rect(0,0,round(xRes/yRes*newHeight),newHeight)
        else destRect:=Rect(0,0,newWidth,round(yRes/xRes*newWidth));
      end;
      res_cropToFill: begin
        destRect:=Rect(0,0,newWidth,newHeight);
        //(xRes-dx)/(yRes-dy)=newWidth/newHeight
        //dy=0 => dx=xRes-yRes*newWidth/newHeight
        //dx=0 => dy=yRes-xRes*newHeight/newWidth
        dx:=round(xRes-yRes*newWidth/newHeight); if dx<0 then dx:=0;
        dy:=round(yRes-xRes*newHeight/newWidth); if dy<0 then dy:=0;
        srcRect:=Rect(dx shr 1,dy shr 1,xRes+(dx shr 1)-dx,yRes+(dy shr 1)-dy);
      end;
    end;
    if style=rs_singleColorPseudoImage then begin
      xRes:=destRect.Right -destRect.Left;
      yRes:=destRect.Bottom-destRect.Top;
      exit;
    end;

    resizeViaTImage;
  end;

PROCEDURE T_rawImage.flip;
  VAR x,y,y1:longint;
      temp24bit:T_24Bit;
      tempCol  :T_floatColor;
  begin
    for y:=0 to yRes shr 1 do begin
      y1:=yRes-1-y;
      if y1>y then case style of
        rs_24bit   : for x:=0 to xRes-1 do begin temp24bit:=pixel24Bit[x,y]; pixel24Bit[x,y]:=pixel24Bit[x,y1]; pixel24Bit[x,y1]:=temp24bit; end;
        rs_float: for x:=0 to xRes-1 do begin tempCol  :=pixel     [x,y]; pixel     [x,y]:=pixel     [x,y1]; pixel     [x,y1]:=tempCol  ; end;
      end;
    end;
  end;

PROCEDURE T_rawImage.flop;
  VAR x,y,x1:longint;
      temp24bit:T_24Bit;
      tempCol  :T_floatColor;
  begin
    for x:=0 to xRes shr 1 do begin
      x1:=xRes-1-x;
      if x1>x then case style of
        rs_24bit   : for y:=0 to yRes-1 do begin temp24bit:=pixel24Bit[x,y]; pixel24Bit[x,y]:=pixel24Bit[x1,y]; pixel24Bit[x1,y]:=temp24bit; end;
        rs_float: for y:=0 to yRes-1 do begin tempCol  :=pixel     [x,y]; pixel     [x,y]:=pixel     [x1,y]; pixel     [x1,y]:=tempCol  ; end;
      end;
    end;
  end;

PROCEDURE T_rawImage.rotRight;
  VAR x,y:longint;
      idx0,idx1:longint;
      temp24bit:T_24Bit;
      tempCol  :T_floatColor;
  begin
    case style of
      rs_24bit: for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
        idx0:=       x+y*xRes;
        idx1:=yRes-1-y+x*yRes;
        if idx0<idx1 then begin temp24bit:=dat24bit[idx0]; dat24bit[idx0]:=dat24bit[idx1]; dat24bit[idx1]:=temp24bit; end;
      end;
      rs_float: for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
        idx0:=       x+y*xRes;
        idx1:=yRes-1-y+x*yRes;
        if idx0<idx1 then begin tempCol:=datFloat[idx0]; datFloat[idx0]:=datFloat[idx1]; datFloat[idx1]:=tempCol; end;
      end;
    end;
  end;

PROCEDURE T_rawImage.rotLeft;
  VAR x,y:longint;
      idx0,idx1:longint;
      temp24bit:T_24Bit;
      tempCol  :T_floatColor;
  begin
    case style of
      rs_24bit: for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
        idx0:=x+y         *xRes;
        idx1:=y+(xRes-1-x)*yRes;
        if idx0<idx1 then begin temp24bit:=dat24bit[idx0]; dat24bit[idx0]:=dat24bit[idx1]; dat24bit[idx1]:=temp24bit; end;
      end;
      rs_float: for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
        idx0:=x+y         *xRes;
        idx1:=y+(xRes-1-x)*yRes;
        if idx0<idx1 then begin tempCol:=datFloat[idx0]; datFloat[idx0]:=datFloat[idx1]; datFloat[idx1]:=tempCol; end;
      end;
    end;
  end;

PROCEDURE T_rawImage.crop(CONST x0, x1, y0, y1: longint);
  VAR newData:pointer;
      newXRes,newYRes,x,y:longint;
  begin
    if (x1<=x0) or (y1<=y0) then exit;
    newXRes:=x1-x0;
    newYRes:=y1-y0;
    case style of
      rs_24bit: begin getMem(newData,newXRes*newYRes*sizeOf(T_24Bit));      for y:=y0 to y1 do for x:=x0 to x1 do P_24Bit     (newData)[(x-x0)+(y-y0)*newXRes]:=pixel24Bit[x,y]; end;
      rs_float: begin getMem(newData,newXRes*newYRes*sizeOf(T_floatColor)); for y:=y0 to y1 do for x:=x0 to x1 do P_floatColor(newData)[(x-x0)+(y-y0)*newXRes]:=pixel     [x,y]; end;
    end;
    case style of
      rs_24bit: begin freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));      dat24bit:=newData; end;
      rs_float: begin freeMem(dat24bit,xRes*yRes*sizeOf(T_floatColor)); datFloat:=newData; end;
    end;
    xRes:=newXRes;
    yRes:=newYRes;
  end;

FUNCTION T_rawImage.histogram:T_compoundHistogram;
  VAR i:longint;
  begin
    result.create;
    case style of
      rs_singleColorPseudoImage: result.putSample(datCol);
      rs_24bit: for i:=0 to xRes*yRes-1 do result.putSample(dat24bit[i]);
      rs_float: for i:=0 to xRes*yRes-1 do result.putSample(datFloat[i]);
    end;
  end;

FUNCTION T_rawImage.histogram(CONST x,y:longint; CONST smoothingKernel:T_arrayOfDouble):T_compoundHistogram;
  VAR dx,dy:longint;
      wy:double;
  begin
    result.create;
    for dy:=max(-y,-length(smoothingKernel)) to min(yRes-1-y,length(smoothingKernel)) do begin
      wy:=smoothingKernel[abs(dy)];
      for dx:=max(-x,-length(smoothingKernel)) to min(xRes-1-x,length(smoothingKernel)) do begin
        result.putSampleSmooth(pixel[y+dy,x+dx],smoothingKernel[abs(dx)]*wy);
      end;
    end;
  end;

FUNCTION getSmoothingKernel(CONST sigma:double):T_arrayOfDouble;
  VAR radius,i:longint;
      sum:double=-1;
      factor:double;
  begin
    radius:=round(3*sigma);
    if radius<2 then radius:=2;
    setLength(result,radius+1);
    for i:=0 to radius do begin
      result[i]:=exp(-0.5*sqr(i/sigma));
      sum:=sum+2*result[i];
    end;
    factor:=1/sum;
    for i:=0 to radius do result[i]:=result[i]*factor;
  end;

//procedure T_rawImage.combineImages(var other: T_rawImage;
//  const combinationType: T_combinationType);
//  FUNCTION colMult  (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=a[i]*b[i]; end;
//  FUNCTION colDiv   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=a[i]/b[i]; end;
//  function colScreen(CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=1-(1-a[i])*(1-b[i]); end;
//  function colMax   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if a[i]>b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
//  function colMin   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if a[i]<b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
//  VAR destData:P_floatColor;
//      x,y:longint;
//  begin
//    if (other.xRes<>xRes) or (other.yRes<>yRes) then begin
//      writeln(stderr,'Operation not possible; Images must have same resolution');
//      halt;
//    end;
//    getMem(destData,dataSize(rs_float));
//    case combinationType of
//      ct_add     : for y:=0 to yRes-1 do for x:=0 to xRes-1 do destData[x+y*xRes]:=getPixel(x,y)+other.getPixel(x,y);
//      ct_subtract: for y:=0 to yRes-1 do for x:=0 to xRes-1 do destData[x+y*xRes]:=getPixel(x,y)-other.getPixel(x,y);
//      ct_multiply: for y:=0 to yRes-1 do for x:=0 to xRes-1 do destData[x+y*xRes]:=colMult  (getPixel(x,y),other.getPixel(x,y));
//      ct_divide  : for y:=0 to yRes-1 do for x:=0 to xRes-1 do destData[x+y*xRes]:=colDiv   (getPixel(x,y),other.getPixel(x,y));
//      ct_screen  : for y:=0 to yRes-1 do for x:=0 to xRes-1 do destData[x+y*xRes]:=colScreen(getPixel(x,y),other.getPixel(x,y));
//      ct_max     : for y:=0 to yRes-1 do for x:=0 to xRes-1 do destData[x+y*xRes]:=colMax   (getPixel(x,y),other.getPixel(x,y));
//      ct_min     : for y:=0 to yRes-1 do for x:=0 to xRes-1 do destData[x+y*xRes]:=colMin   (getPixel(x,y),other.getPixel(x,y));
//    end;
//    freemem(data,dataSize);
//    data:=destData;
//  end;


//FUNCTION T_ByteMap   .getHistogram:T_histogram;
//  VAR i:longint;
//  begin
//    for i:=0 to 255 do result[i]:=0;
//    for i:=0 to xRes*yRes-1 do finc(result[pixelBuffer[i]]);
//  end;

//FUNCTION T_24BitImage.getHistogram(ht:T_histogramType):T_histogram;
//  VAR i,j:longint;
//      aid:T_Float;
//  begin
//    for j:=0 to 255 do result[j]:=0;
//    case ht of
//      ht_full        : for i:=0 to xRes*yRes-1 do begin j:=pixelBuffer[i,0]; result[j]:=result[j]+1;
//                                                        j:=pixelBuffer[i,1]; result[j]:=result[j]+1;
//                                                        j:=pixelBuffer[i,2]; result[j]:=result[j]+1; end;
//      ht_greyLevel   : for i:=0 to xRes*yRes-1 do begin
//                         j:=(pixelBuffer[i,0]+
//                             pixelBuffer[i,1]+
//                             pixelBuffer[i,2]) div 3;
//                         result[j]:=result[j]+1;
//                       end;
//      ht_redChannel  : for i:=0 to xRes*yRes-1 do begin j:=pixelBuffer[i,0]; result[j]:=result[j]+1; end;
//      ht_greenChannel: for i:=0 to xRes*yRes-1 do begin j:=pixelBuffer[i,1]; result[j]:=result[j]+1; end;
//      ht_blueChannel : for i:=0 to xRes*yRes-1 do begin j:=pixelBuffer[i,2]; result[j]:=result[j]+1; end;
//    end;
//    if ht=ht_full then aid:=1/(3*xRes*yRes)
//                  else aid:=1/(  xRes*yRes);
//    for i:=0 to 255 do result[i]:=result[i]*aid;
//  end;
//
//FUNCTION T_FloatMap.getHistogram(ht:T_histogramType):T_histogram;
//  PROCEDURE smoothInc(VAR h:T_histogram; index:single); inline;
//    begin
//      index:=index*255;
//      if      index<0   then h[0  ]:=h[0  ]+1
//      else if index>255 then h[255]:=h[255]+1
//      else begin
//        h[trunc(index)  ]:=h[trunc(index)  ]+(1-frac(index));
//        h[trunc(index)+1]:=h[trunc(index)+1]+   frac(index) ;
//      end;
//    end;
//
//  VAR i:longint;
//      aid:T_Float;
//  begin
//    for i:=0 to 255 do result[i]:=0;
//    case ht of
//      ht_full        : for i:=0 to xRes*yRes-1 do begin smoothInc(result,pixelBuffer[i,0]);
//                                                        smoothInc(result,pixelBuffer[i,1]);
//                                                        smoothInc(result,pixelBuffer[i,2]); end;
//      ht_greyLevel   : for i:=0 to xRes*yRes-1 do
//                         smoothInc(result,(pixelBuffer[i,0]+
//                                           pixelBuffer[i,1]+
//                                           pixelBuffer[i,2])/3);
//
//      ht_redChannel  : for i:=0 to xRes*yRes-1 do smoothInc(result,pixelBuffer[i,0]);
//      ht_greenChannel: for i:=0 to xRes*yRes-1 do smoothInc(result,pixelBuffer[i,1]);
//      ht_blueChannel : for i:=0 to xRes*yRes-1 do smoothInc(result,pixelBuffer[i,2]);
//    end;
//    if ht=ht_full then aid:=1/(3*xRes*yRes)
//                  else aid:=1/(  xRes*yRes);
//    for i:=0 to 255 do result[i]:=result[i]*aid;
//  end;
//


end.
