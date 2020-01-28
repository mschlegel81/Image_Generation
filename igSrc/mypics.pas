UNIT mypics;
INTERFACE
{$fputype sse3}
USES dos,sysutils,Interfaces,Classes, ExtCtrls, Graphics, IntfGraphics, GraphType,
     math,FPWriteJPEG,FileUtil,
     myParams,
     myGenerics, mySys,
     types,
     myColors,
     pixMaps;

{$define include_interface}

CONST CHUNK_BLOCK_SIZE =64;
      JPG_EXT='.JPG';
      BMP_EXT='.BMP';
      PNG_EXT='.PNG';
      RAW_EXT='.VRAW';
      SUPPORTED_IMAGE_TYPES:array[0..3] of string=(JPG_EXT,BMP_EXT,PNG_EXT,RAW_EXT);
TYPE
  T_resizeStyle=(res_exact,
                 res_cropToFill,
                 res_cropRotate,
                 res_fit,
                 res_fitExpand,
                 res_fitRotate,
                 res_dataResize);

  T_pendingList=array of longint;

  T_structuredHitColor=record
    rest:T_rgbFloatColor;
    antialiasingMask:byte;
  end;

  T_colChunk=object
    lastCalculatedTolerance:single;
    x0,y0:longint;
    width,height:longint;
    col:array[0..CHUNK_BLOCK_SIZE-1,0..CHUNK_BLOCK_SIZE-1] of T_structuredHitColor;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE initForChunk(CONST xRes,yRes,chunkIdx:longint);
    FUNCTION getPicX(CONST localX:longint):longint;
    FUNCTION getPicY(CONST localY:longint):longint;
    FUNCTION markAlias(CONST globalTol:single):boolean;
  end;

  T_rgbFloatMap=specialize G_pixelMap<T_rgbFloatColor>;
  T_rgbMap=specialize G_pixelMap<T_rgbColor>;
  P_floatColor=^T_rgbFloatColor;

  P_rawImage=^T_rawImage;
  T_rawImage=object(T_rgbFloatMap)
    private
      //Helper routines:--------------------------------------------------------
      PROCEDURE copyToImage(CONST srcRect:TRect; VAR destImage: TImage);
      //--------------------------------------------------------:Helper routines
    public
      CONSTRUCTOR create(CONST width_,height_:longint);
      CONSTRUCTOR create(CONST fileName:ansistring);
      CONSTRUCTOR create(VAR original:T_rawImage);
      DESTRUCTOR destroy;
      //Access per pixel:-------------------------------------------------------
      PROPERTY pixel     [x,y:longint]:T_rgbFloatColor read getPixel write setPixel; default;
      PROCEDURE multIncPixel(CONST x,y:longint; CONST factor:single; CONST increment:T_rgbFloatColor);
      //-------------------------------------------------------:Access per pixel
      //Chunk access:-----------------------------------------------------------
      FUNCTION chunksInMap:longint;
      PROCEDURE markChunksAsPending;
      FUNCTION getPendingList:T_pendingList;
      PROCEDURE copyFromChunk(VAR chunk:T_colChunk);
      //-----------------------------------------------------------:Chunk access
      PROCEDURE drawCheckerboard;
      //TImage interface:-------------------------------------------------------
      PROCEDURE copyToImage(VAR destImage: TImage);
      PROCEDURE copyFromImage(VAR srcImage: TImage);
      //-------------------------------------------------------:TImage interface
      //File interface:---------------------------------------------------------
      PROCEDURE saveToFile(CONST fileName:ansistring);
      PROCEDURE loadFromFile(CONST fileName:ansistring);
      PROCEDURE saveJpgWithSizeLimit(CONST fileName:ansistring; CONST sizeLimit:SizeInt);
      FUNCTION getJpgFileData(CONST quality:longint=100):ansistring;
      //---------------------------------------------------------:File interface
      //Geometry manipulations:-------------------------------------------------
      PROCEDURE resize(CONST tgtDim:T_imageDimensions; CONST resizeStyle:T_resizeStyle);
      PROCEDURE zoom(CONST factor:double);
      //-------------------------------------------------:Geometry manipulations
      //Statistic accessors:----------------------------------------------------
      FUNCTION histogram:T_compoundHistogram;
      FUNCTION histogram(CONST x,y:longint; CONST smoothingKernel:T_arrayOfDouble):T_compoundHistogram;
      FUNCTION histogramHSV:T_compoundHistogram;
      //----------------------------------------------------:Statistic accessors
      PROCEDURE quantize(CONST numberOfColors:longint);
      FUNCTION directionMap(CONST relativeSigma:double):T_rawImage;
      PROCEDURE lagrangeDiffusion(CONST relativeGradSigma,relativeBlurSigma:double);
      PROCEDURE lagrangeDiffusion(VAR dirMap:T_rawImage; CONST relativeBlurSigma:double; CONST changeDirection:boolean=true);
      PROCEDURE radialBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
      PROCEDURE rotationalBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
      PROCEDURE shine;
      PROCEDURE sharpen(CONST relativeSigma,factor:double);
      PROCEDURE prewittEdges;
      PROCEDURE variance(CONST relativeSigma:double);
      PROCEDURE medianFilter(CONST relativeSigma:double);
      PROCEDURE modalFilter(CONST relativeSigma:double);
      PROCEDURE sketch(CONST cover,relativeDirMapSigma,density,tolerance:double);
      //PROCEDURE paint(CONST relativeDirMapSigma,density,tolerance,curvature:double);
      PROCEDURE myFilter(CONST thresholdDistParam,param:double);
      PROCEDURE drip(CONST diffusiveness,range:double);
      //PROCEDURE encircle(CONST count:longint; CONST background:T_rgbFloatColor; CONST opacity,relativeCircleSize:double; CONST containingQueue:P_progressEstimatorQueue);
      //PROCEDURE bySpheres(CONST count:longint; CONST style:byte; CONST relativeCircleSize0,relativeCircleSize1:double; CONST containingQueue:P_progressEstimatorQueue);
      //PROCEDURE nlmFilter(CONST scanRadius:longint; CONST sigma:double; CONST queue:P_progressEstimatorQueue);
      //PROCEDURE modMedFilter(CONST queue:P_progressEstimatorQueue);
      FUNCTION rgbaSplit(CONST transparentColor:T_rgbFloatColor):T_rawImage;
      PROCEDURE halftone(CONST scale:single; CONST param:longint);
      PROCEDURE rotate(CONST angleInDegrees:double);
      PROCEDURE copyFromImageWithOffset(VAR image:T_rawImage; CONST xOff,yOff:longint);
  end;

F_displayErrorFunction=PROCEDURE(CONST s:ansistring);

VAR compressionQualityPercentage:longint=100;
//FUNCTION resize(CONST dim:T_imageDimensions; CONST newWidth,newHeight:longint; CONST resizeStyle:T_resizeStyle):T_imageDimensions;

IMPLEMENTATION
VAR globalFileLock:TRTLCriticalSection;
CONSTRUCTOR T_colChunk.create;
  begin end;

PROCEDURE T_colChunk.initForChunk(CONST xRes,yRes,chunkIdx:longint);
  VAR i,j:longint;
  begin
    x0:=0;
    y0:=0;
    for i:=0 to chunkIdx-1 do begin
      inc(x0,CHUNK_BLOCK_SIZE);
      if x0>=xRes then begin
        x0:=0;
        inc(y0,CHUNK_BLOCK_SIZE);
      end;
    end;
    width :=xRes-x0; if width >CHUNK_BLOCK_SIZE then width :=CHUNK_BLOCK_SIZE;
    height:=yRes-y0; if height>CHUNK_BLOCK_SIZE then height:=CHUNK_BLOCK_SIZE;
    for i:=0 to CHUNK_BLOCK_SIZE-1 do for j:=0 to CHUNK_BLOCK_SIZE-1 do with col[i,j] do begin
      rest:=BLACK;
      antialiasingMask:=0;
    end;
  end;

DESTRUCTOR T_colChunk.destroy;
  begin
  end;

FUNCTION T_colChunk.getPicX(CONST localX:longint):longint;
  begin
    result:=localX+x0;
  end;

FUNCTION T_colChunk.getPicY(CONST localY:longint):longint;
  begin
    result:=localY+y0;
  end;

{$PUSH}{$OPTIMIZATION OFF}
FUNCTION combinedColor(CONST struc:T_structuredHitColor):T_rgbFloatColor;
  begin
    with struc do if antialiasingMask<2
    then result:=rest
    else result:=rest*(0.5/(antialiasingMask and 254));
  end;
{$POP}

FUNCTION T_colChunk.markAlias(CONST globalTol:single):boolean;
  VAR i,j,i2,j2:longint;
      localRefFactor:single;
      localTol:single;
      localError:single;
      tempColor:array[0..CHUNK_BLOCK_SIZE-1,0..CHUNK_BLOCK_SIZE-1] of T_rgbFloatColor;

  FUNCTION getErrorAt(CONST i,j:longint):double;
    VAR c:array[-1..1,-1..1] of T_rgbFloatColor;
        di,dj,ki,kj:longint;
    begin
      if (height<3) or (width<3) then exit(1E6);
      for di:=-1 to 1 do for dj:=-1 to 1 do begin
        ki:=di+i; if ki<0 then ki:=0-ki else if ki>width-1  then ki:=2*(width -1)-ki;
        kj:=dj+j; if kj<0 then kj:=0-kj else if kj>height-1 then kj:=2*(height-1)-kj;
        c[di,dj]:=tempColor[ki,kj];
      end;
      result:=calcErr(c[-1,-1],c[0,-1],c[1,-1],
                      c[-1, 0],c[0, 0],c[1, 0],
                      c[-1,+1],c[0,+1],c[1,+1]);
    end;

  begin
    result:=false;
    for i:=0 to width-1 do for j:=0 to height-1 do tempColor[i,j]:=combinedColor(col[i,j]);

    for i:=0 to width-1 do for j:=0 to height-1 do begin
      localRefFactor:=(col[i,j].antialiasingMask and 254)/254;
      localTol:=(1+localRefFactor*localRefFactor)*globalTol;
      localError:=getErrorAt(i,j);
      if localError>localTol then begin
        for i2:=i-1 to i+1 do if (i2>=0) and (i2<width) then
        for j2:=j-1 to j+1 do if (j2>=0) and (j2<height) and not(odd(col[i2,j2].antialiasingMask)) and (col[i2,j2].antialiasingMask<254) then begin
          inc(col[i2,j2].antialiasingMask);
          result:=true;
        end;
      end;
    end;
  end;

CONSTRUCTOR T_rawImage.create(CONST width_, height_: longint);
  begin
    inherited create(width_,height_);
  end;

CONSTRUCTOR T_rawImage.create(CONST fileName: ansistring);
  begin
    inherited create(1,1);
    loadFromFile(fileName);
  end;

CONSTRUCTOR T_rawImage.create(VAR original: T_rawImage);
  begin
    inherited create(1,1);
    copyFromPixMap(original);
  end;

DESTRUCTOR T_rawImage.destroy;
  begin
    inherited destroy;
  end;

PROCEDURE T_rawImage.copyToImage(CONST srcRect: TRect; VAR destImage: TImage);
  VAR ScanLineImage,                 //image with representation as in T_24BitImage
      tempIntfImage: TLazIntfImage;  //image with representation as in TBitmap
      ImgFormatDescription: TRawImageDescription;
      x,y:longint;
      pc:T_rgbColor;
      pix:PByte;
  begin
    ScanLineImage:=TLazIntfImage.create(srcRect.Right-srcRect.Left,srcRect.Bottom-srcRect.top);
    ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(srcRect.Right-srcRect.Left,srcRect.Bottom-srcRect.top);
    ImgFormatDescription.ByteOrder:=riboMSBFirst;
    ScanLineImage.DataDescription:=ImgFormatDescription;
    for y:=0 to srcRect.Bottom-srcRect.top-1 do begin
      pix:=ScanLineImage.GetDataLineStart(y);
      for x:=0 to srcRect.Right-srcRect.Left-1 do begin
        pc:=getPixel(srcRect.Left+x,srcRect.top+y);
        move(pc,(pix+3*x)^,3);
      end;
    end;
    destImage.picture.Bitmap.setSize(srcRect.Right-srcRect.Left,srcRect.Bottom-srcRect.top);
    //destImage.picture.Bitmap.width :=srcRect.Right-srcRect.Left;
    //destImage.picture.Bitmap.height:=srcRect.Bottom-srcRect.top;
    tempIntfImage:=destImage.picture.Bitmap.CreateIntfImage;
    tempIntfImage.CopyPixels(ScanLineImage);
    destImage.picture.Bitmap.LoadFromIntfImage(tempIntfImage);
    tempIntfImage.free;
    ScanLineImage.free;
  end;

PROCEDURE T_rawImage.copyToImage(VAR destImage: TImage);
  begin
    copyToImage(rect(0,0,dim.width,dim.height),destImage);
  end;

PROCEDURE T_rawImage.copyFromImage(VAR srcImage: TImage);
  VAR x,y:longint;
      tempIntfImage,
      ScanLineImage: TLazIntfImage;
      ImgFormatDescription: TRawImageDescription;
      pc:T_rgbColor;
      pix:PByte;

  begin
    initialize(pc);
    resize(imageDimensions(srcImage.picture.width,srcImage.picture.height),res_dataResize);

    ScanLineImage:=TLazIntfImage.create(dim.width,dim.height);
    ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(dim.width,dim.height);
    ImgFormatDescription.ByteOrder:=riboMSBFirst;
    ScanLineImage.DataDescription:=ImgFormatDescription;
    tempIntfImage:=srcImage.picture.Bitmap.CreateIntfImage;
    ScanLineImage.CopyPixels(tempIntfImage);
    for y:=0 to dim.height-1 do begin
      pix:=ScanLineImage.GetDataLineStart(y);
      for x:=0 to dim.width-1 do begin
        move((pix+3*x)^,pc,3);
        setPixel(x,y,pc);
      end;
    end;
    ScanLineImage.free;
    tempIntfImage.free;
  end;

PROCEDURE T_rawImage.multIncPixel(CONST x,y:longint; CONST factor:single; CONST increment:T_rgbFloatColor);
  VAR k:longint;
  begin
    k:=x+y*dim.width;
    data[k]:=data[k]*factor+increment;
  end;

FUNCTION T_rawImage.chunksInMap: longint;
  VAR xChunks,yChunks:longint;
  begin
    xChunks:=dim.width  div CHUNK_BLOCK_SIZE; if xChunks*CHUNK_BLOCK_SIZE<dim.width  then inc(xChunks);
    yChunks:=dim.height div CHUNK_BLOCK_SIZE; if yChunks*CHUNK_BLOCK_SIZE<dim.height then inc(yChunks);
    result:=xChunks*yChunks;
  end;

PROCEDURE T_rawImage.markChunksAsPending;
  VAR x,y:longint;
  begin
    for y:=dim.height-1 downto 0 do for x:=0 to dim.width-1 do
      if ((x and 63) in [0,63]) or ((y and 63) in [0,63]) or (odd(x) xor odd(y)) and (((x and 63) in [21,42]) or ((y and 63) in [21,42]))
      then pixel[x,y]:=WHITE
      else pixel[x,y]:=BLACK;
  end;

FUNCTION T_rawImage.getPendingList: T_pendingList;
  VAR xChunks,yChunks:longint;
      x,y,cx,cy,i:longint;
      isPending:array of array of boolean;
  begin
    randomize;
    xChunks:=dim.width  div CHUNK_BLOCK_SIZE; if xChunks*CHUNK_BLOCK_SIZE<dim.width  then inc(xChunks);
    yChunks:=dim.height div CHUNK_BLOCK_SIZE; if yChunks*CHUNK_BLOCK_SIZE<dim.height then inc(yChunks);
    setLength(isPending,xChunks);
    for cx:=0 to length(isPending)-1 do begin
      setLength(isPending[cx],yChunks);
      for cy:=0 to length(isPending[cx])-1 do isPending[cx,cy]:=true;
    end;
    //scan:-----------------------------------------------------
    for y:=dim.height-1 downto 0 do begin
      cy:=y div CHUNK_BLOCK_SIZE;
      for x:=0 to dim.width-1 do begin
        cx:=x div CHUNK_BLOCK_SIZE;
        if ((x and 63) in [0,63]) or ((y and 63) in [0,63]) or (odd(x) xor odd(y)) and (((x and 63) in [21,42]) or ((y and 63) in [21,42]))
        then isPending[cx,cy]:=isPending[cx,cy] and (pixel[x,y]=WHITE)
        else isPending[cx,cy]:=isPending[cx,cy] and (pixel[x,y]=BLACK);
      end;
    end;
    //-----------------------------------------------------:scan
    //transform boolean mask to int array:----------------------
    setLength(result,0);
    for cy:=0 to length(isPending[0])-1 do
    for cx:=length(isPending)-1 downto 0 do if isPending[cx,cy] then begin
      setLength(result,length(result)+1);
      result[length(result)-1]:=cx+xChunks*cy;
    end;
    for cx:=0 to length(isPending)-1 do setLength(isPending[cx],0);
    setLength(isPending,0);
    //----------------------:transform boolean mask to int array
    //scramble result:------------------------------------------
    for i:=0 to length(result)-1 do begin
      cx:=random(length(result));
      repeat cy:=random(length(result)) until cx<>cy;
      x:=result[cx]; result[cx]:=result[cy]; result[cy]:=x;
    end;
  end;

PROCEDURE T_rawImage.copyFromChunk(VAR chunk: T_colChunk);
  VAR i,j:longint;
  begin
    for j:=0 to chunk.height-1 do for i:=0 to chunk.width-1 do with chunk.col[i,j] do
      pixel[chunk.getPicX(i),chunk.getPicY(j)]:=combinedColor(chunk.col[i,j]);
  end;

PROCEDURE T_rawImage.drawCheckerboard;
  CONST floatGrey:array[false..true] of T_rgbFloatColor=((0.6,0.6,0.6),(0.4,0.4,0.4));
  VAR i,j:longint;
  begin
    for j:=0 to dim.height-1 do for i:=0 to dim.width-1 do setPixel(i,j,floatGrey[odd(i shr 5) xor odd(j shr 5)]);
  end;

PROCEDURE T_rawImage.saveToFile(CONST fileName: ansistring);
  PROCEDURE storeDump;
    VAR handle:file of byte;
    begin
      assign(handle,fileName);
      rewrite(handle);
      BlockWrite(handle,dim.width ,sizeOf(longint));
      BlockWrite(handle,dim.height,sizeOf(longint));
      BlockWrite(handle,PByte(data)^,dataSize);
      close(handle);
    end;

  VAR ext:string;
      storeImg:TImage;
      Jpeg:TFPWriterJPEG;
      img:TLazIntfImage;
  begin
    ForceDirectories(extractFilePath(expandFileName(fileName)));
    ext:=uppercase(extractFileExt(fileName));
    if (ext=JPG_EXT) or (ext=PNG_EXT) or (ext=BMP_EXT) then begin
      enterCriticalSection(globalFileLock);
      storeImg:=TImage.create(nil);
      storeImg.SetInitialBounds(0,0,dim.width,dim.height);
      copyToImage(storeImg);
      if ext=PNG_EXT then storeImg.picture.PNG.saveToFile(fileName) else
      if ext=BMP_EXT then storeImg.picture.Bitmap.saveToFile(fileName)
                 else begin
        Jpeg:=TFPWriterJPEG.create;
        Jpeg.CompressionQuality:=100;
        img:=storeImg.picture.Bitmap.CreateIntfImage;
        img.saveToFile(fileName,Jpeg);
        img.free;
        Jpeg.free;
      end;
      storeImg.free;
      leaveCriticalSection(globalFileLock);
    end else if ext=RAW_EXT then storeDump
    else raise Exception.create('Usupported image format "'+ext+'"');
  end;

PROCEDURE T_rawImage.loadFromFile(CONST fileName: ansistring);
  VAR useFilename:ansistring;
  PROCEDURE restoreDump;
    VAR handle:file of byte;
    begin
      freeMem(data,dataSize);
      assign(handle,useFilename);
      reset(handle);
      BlockRead(handle,dim.width ,sizeOf(longint));
      BlockRead(handle,dim.height,sizeOf(longint));
      getMem(data,dataSize);
      BlockRead(handle,PByte(data)^,dataSize);
      close(handle);
    end;

  VAR ext:string;
      reStoreImg:TImage;
  begin
    if fileExists(fileName) then useFilename:=fileName
    else begin
      writeln(stdErr,'Image ',fileName,' cannot be loaded because it does not exist');
      exit;
    end;
    ext:=uppercase(extractFileExt(useFilename));
    if (ext=JPG_EXT) or (ext=PNG_EXT) or (ext=BMP_EXT) then begin
      enterCriticalSection(globalFileLock);
      reStoreImg:=TImage.create(nil);
      leaveCriticalSection(globalFileLock);
      reStoreImg.SetInitialBounds(0,0,10000,10000);
      if ext=PNG_EXT then reStoreImg.picture.PNG   .loadFromFile(useFilename) else
      if ext=BMP_EXT then reStoreImg.picture.Bitmap.loadFromFile(useFilename)
                     else reStoreImg.picture.Jpeg  .loadFromFile(useFilename);
      reStoreImg.SetBounds(0,0,reStoreImg.picture.width,reStoreImg.picture.height);
      copyFromImage(reStoreImg);
      reStoreImg.free;
    end else restoreDump;
  end;

PROCEDURE T_rawImage.saveJpgWithSizeLimit(CONST fileName:ansistring; CONST sizeLimit:SizeInt);
  VAR ext:string;
      storeImg:TImage;

  FUNCTION filesize(name:string):longint;
    VAR s:TSearchRec;
    begin
      if findFirst(name,faAnyFile,s)=0
        then result:=s.size
        else result:=0;
      findClose(s);
    end;

  VAR quality:longint;
      sizes:array[1..100] of longint;

  FUNCTION saveAtQuality(CONST quality:longint; CONST saveToFile:boolean):longint;
    VAR Jpeg:TFPWriterJPEG;
        img:TLazIntfImage;
        stream:TMemoryStream;
    begin
      Jpeg:=TFPWriterJPEG.create;
      Jpeg.CompressionQuality:=quality;
      img:=storeImg.picture.Bitmap.CreateIntfImage;
      if saveToFile then begin
        img.saveToFile(fileName,Jpeg);
        result:=filesize(fileName);
      end else begin
        stream:=TMemoryStream.create;
        img.saveToStream(stream,Jpeg);
        result:=stream.position;
        stream.free;
      end;
      img.free;
      Jpeg.free;
    end;

  FUNCTION getSizeAt(CONST quality:longint):longint;
    begin
      if quality>100 then exit(getSizeAt(100));
      if quality<1   then exit(getSizeAt(  1));
      if sizes[quality]<0 then sizes[quality]:=saveAtQuality(quality,false);
      result:=sizes[quality];
    end;

  begin
    ext:=uppercase(extractFileExt(fileName));
    if (ext<>JPG_EXT) and (ext<>'.JPEG') then raise Exception.create('Saving with size limit is only possible in JPEG format.');
    if sizeLimit=0 then begin
      saveJpgWithSizeLimit(fileName,round(1677*diagonal));
      exit();
    end;
    ForceDirectories(extractFilePath(expandFileName(fileName)));
    enterCriticalSection(globalFileLock);
    storeImg:=TImage.create(nil);
    storeImg.SetInitialBounds(0,0,dim.width,dim.height);
    copyToImage(storeImg);
    for quality:=1 to 100 do sizes[quality]:=-1;
    quality:=100;
    while (quality>  1) and (getSizeAt(quality)>sizeLimit) do dec(quality,64); if (quality<  1) then quality:=  1;
    while (quality<100) and (getSizeAt(quality)<sizeLimit) do inc(quality,32); if (quality>100) then quality:=100;
    while (quality>  1) and (getSizeAt(quality)>sizeLimit) do dec(quality,16); if (quality<  1) then quality:=  1;
    while (quality<100) and (getSizeAt(quality)<sizeLimit) do inc(quality, 8); if (quality>100) then quality:=100;
    while (quality>  1) and (getSizeAt(quality)>sizeLimit) do dec(quality, 4); if (quality<  1) then quality:=  1;
    while (quality<100) and (getSizeAt(quality)<sizeLimit) do inc(quality, 2); if (quality>100) then quality:=100;
    while (quality>  1) and (getSizeAt(quality)>sizeLimit) do dec(quality   );
    saveAtQuality(quality,true);
    storeImg.free;
    leaveCriticalSection(globalFileLock);
  end;

  FUNCTION T_rawImage.getJpgFileData(CONST quality:longint=100):ansistring;
    VAR Jpeg:TFPWriterJPEG;
        img:TLazIntfImage;
        stream:TStringStream;
        storeImg:TImage;
    begin
      storeImg:=TImage.create(nil);
      storeImg.SetInitialBounds(0,0,dim.width,dim.height);
      copyToImage(storeImg);
      Jpeg:=TFPWriterJPEG.create;
      Jpeg.CompressionQuality:=quality;
      img:=storeImg.picture.Bitmap.CreateIntfImage;
      stream:= TStringStream.create('');
      img.saveToStream(stream,Jpeg);
      img.free;
      Jpeg.free;
      storeImg.free;
      result:=stream.DataString;
      stream.free;
    end;

//FUNCTION resize(CONST dim:T_imageDimensions; CONST newWidth,newHeight:longint; CONST resizeStyle:T_resizeStyle):T_imageDimensions;
//  VAR destRect:TRect;
//  begin
//    case resizeStyle of
//      res_exact,res_dataResize,res_cropToFill,res_fitExpand,res_cropRotate: begin
//        result.width :=newWidth;
//        result.height:=newHeight;
//      end;
//      res_fit: begin
//        destRect:=getFittingRectangle(newWidth,newHeight,dim.width/dim.height);
//        result.width:=destRect.Right;
//        result.height:=destRect.Bottom;
//      end;
//      res_fitRotate: begin
//        //Pic the option resulting in the larger resolution
//        destRect:=getFittingRectangle(newWidth,newHeight,dim.width/dim.height);
//        result.width:=destRect.Right;
//        result.height:=destRect.Bottom;
//        destRect:=getFittingRectangle(newWidth,newHeight,dim.height/dim.width);
//        if destRect.Right*destRect.Bottom>result.width*result.height then begin
//          result.width:=destRect.Right;
//          result.height:=destRect.Bottom;
//        end;
//      end;
//    end;
//  end;

PROCEDURE T_rawImage.resize(CONST tgtDim:T_imageDimensions; CONST resizeStyle: T_resizeStyle);
  VAR srcRect,destRect:TRect;
      dx,dy:longint;
      destDim:T_imageDimensions;
      doRotate:boolean=false;
  PROCEDURE resizeViaTImage;
    VAR srcImage,destImage:TImage;
    begin
      srcImage:=TImage.create(nil);
      srcImage.SetInitialBounds(0,0,srcRect.Right-srcRect.Left,srcRect.Bottom-srcRect.top);
      copyToImage(srcRect,srcImage);
      destImage:=TImage.create(nil);
      destImage.SetInitialBounds(destRect.Left,destRect.top,destRect.Right,destRect.Bottom);
      destImage.AntialiasingMode:=amOn;
      destImage.Canvas.AntialiasingMode:=amOn;
      destImage.Canvas.StretchDraw(destRect,srcImage.picture.Graphic);
      srcImage.free;
      copyFromImage(destImage);
      destImage.free;
    end;

  begin
    case resizeStyle of
      res_exact,res_dataResize: begin
        if tgtDim=dim then exit;
        srcRect:=dim.toRect;
        destRect:=tgtDim.toRect;
      end;
      res_fit,res_fitExpand: begin
        srcRect:=dim.toRect;
        destRect:=tgtDim.getFittingRectangle(dim.width/dim.height).toRect;
      end;
      res_fitRotate, res_cropRotate: begin
        destRect:=tgtDim.getFittingRectangle(dim.width/dim.height).toRect;
        srcRect :=tgtDim.getFittingRectangle(dim.height/dim.width).toRect;
        if srcRect.Right*srcRect.Bottom>destRect.Right*destRect.Bottom then begin
          doRotate:=true;
          destRect:=srcRect;
          srcRect:=rect(0,0,dim.height,dim.width);
        end else srcRect:=dim.toRect;
        if resizeStyle=res_cropRotate then begin
          destRect:=tgtDim.toRect;
          if doRotate then begin
            dx:=round(dim.height-dim.width*tgtDim.width/tgtDim.height); if dx<0 then dx:=0;
            dy:=round(dim.width-dim.height*tgtDim.height/tgtDim.width); if dy<0 then dy:=0;
            srcRect:=rect(dx shr 1,dy shr 1,dim.height+(dx shr 1)-dx,dim.width+(dy shr 1)-dy);
          end else begin
            dx:=round(dim.width-dim.height*tgtDim.width/tgtDim.height); if dx<0 then dx:=0;
            dy:=round(dim.height-dim.width*tgtDim.height/tgtDim.width); if dy<0 then dy:=0;
            srcRect:=rect(dx shr 1,dy shr 1,dim.width+(dx shr 1)-dx,dim.height+(dy shr 1)-dy);
          end;
        end;
      end;
      res_cropToFill: begin
        destRect:=tgtDim.toRect;
        //(xRes-dx)/(dim.height-dy)=newWidth/newHeight
        //dy=0 => dx=xRes-dim.height*newWidth/newHeight
        //dx=0 => dy=dim.height-xRes*newHeight/newWidth
        dx:=round(dim.width-dim.height*tgtDim.width/tgtDim.height); if dx<0 then dx:=0;
        dy:=round(dim.height-dim.width*tgtDim.height/tgtDim.width); if dy<0 then dy:=0;
        srcRect:=rect(dx shr 1,dy shr 1,dim.width+(dx shr 1)-dx,dim.height+(dy shr 1)-dy);
      end;
    end;
    if doRotate then rotLeft;
    if resizeStyle=res_dataResize then begin
      destDim:=tgtDim;
      inherited resize(destDim);
    end else resizeViaTImage;
    if resizeStyle=res_fitExpand then begin
      destDim.width :=tgtDim.width -dim.width ; dx:=-(destDim.width  shr 1); inc(destDim.width ,dx+dim.width );
      destDim.height:=tgtDim.height-dim.height; dy:=-(destDim.height shr 1); inc(destDim.height,dy+dim.height);
      cropAbsolute(dx,destDim.width,dy,destDim.height);
    end;
  end;

PROCEDURE T_rawImage.zoom(CONST factor:double);
  VAR oldDim:T_imageDimensions;
      x0,x1,y0,y1:longint;
  begin
    oldDim:=dim;
    if factor>1 then begin
      crop(0.5-0.5/factor,0.5+0.5/factor,
           0.5-0.5/factor,0.5+0.5/factor);
      resize(oldDim,res_exact);
    end else begin
      //new size=old size*factor
      resize(imageDimensions(round(oldDim.width *factor),
                             round(oldDim.height*factor)),res_exact);
      //x0=round(rx0                               *dim.width)
      //  =round((0.5-0.5/ factor                 )*dim.width)
      //  =round((0.5-0.5/(dim.width/oldDim.width))*dim.width)
      //  =round(0.5*dim.width-0.5*oldDim.width);
      x0:=dim.width shr 1-oldDim.width shr 1;
      x1:= oldDim.width+x0;
      y0:=dim.height shr 1-oldDim.height shr 1;
      y1:= oldDim.height+y0;
      cropAbsolute(x0,x1,y0,y1);
    end;
  end;

FUNCTION T_rawImage.histogram: T_compoundHistogram;
  VAR i:longint;
  begin
    result.create;
    for i:=0 to pixelCount-1 do result.putSample(data[i]);
  end;

FUNCTION T_rawImage.histogram(CONST x, y: longint; CONST smoothingKernel: T_arrayOfDouble): T_compoundHistogram;
  VAR dx,dy:longint;
      wy:double;
  begin
    result.create;
    for dy:=max(-y,1-length(smoothingKernel)) to min(dim.height-y,length(smoothingKernel))-1 do begin
      wy:=smoothingKernel[abs(dy)];
      for dx:=max(-x,1-length(smoothingKernel)) to min(dim.width-x,length(smoothingKernel))-1 do
        result.putSampleSmooth(pixel[x+dx,y+dy],smoothingKernel[abs(dx)]*wy);
    end;
  end;

FUNCTION T_rawImage.histogramHSV: T_compoundHistogram;
  VAR i:longint;
      hsv:T_hsvColor;
  begin
    result.create;
    for i:=0 to pixelCount-1 do begin
      hsv:=data[i];
      result.putSample(rgbColor(hsv[hc_hue],hsv[hc_saturation],hsv[hc_value]));
    end;
  end;

PROCEDURE T_rawImage.quantize(CONST numberOfColors:longint);
  VAR i:longint;
      tree:T_colorTree;
  begin
    tree.create;
    for i:=0 to pixelCount-1 do tree.addSample(data[i]);
    tree.finishSampling(numberOfColors);
    for i:=0 to pixelCount-1 do data[i]:=tree.getQuantizedColor(data[i]);
    tree.destroy;
  end;

FUNCTION T_rawImage.directionMap(CONST relativeSigma:double):T_rawImage;
  VAR x,y:longint;

  FUNCTION normalAt(x,y:longint):T_rgbFloatColor;
    VAR dx,dy:longint;
        channel:T_colorChannel;
        n:array[-1..1,-1..1] of T_rgbFloatColor;
        w :array [0..1] of double;
    begin
      //fill stencil:--------------------------------------------//
      for dy:=-1 to 1 do for dx:=-1 to 1 do                      //
      if (y+dy>=0) and (y+dy<dim.height) and (x+dx>=0) and (x+dx<dim.width)
        then n[dx,dy]:=getPixel(x+dx,(y+dy))                     //
        else n[dx,dy]:=getPixel(x   , y    );                    //
      //----------------------------------------------:fill stencil
      result:=BLACK;
      for channel in RGB_CHANNELS do begin
        w[0]:=n[ 1,-1][channel]+3*n[ 1,0][channel]+n[ 1,1][channel]
             -n[-1,-1][channel]-3*n[-1,0][channel]-n[-1,1][channel];
        w[1]:=n[-1, 1][channel]+3*n[0, 1][channel]+n[1, 1][channel]
             -n[-1,-1][channel]-3*n[0,-1][channel]-n[1,-1][channel];
        result[cc_blue ]:=1/(1E-6+w[0]*w[0]+w[1]*w[1]);
        result[cc_red  ]:=result[cc_red  ]+result[cc_blue]*(w[0]*w[0]-w[1]*w[1]);
        result[cc_green]:=result[cc_green]+result[cc_blue]*2*w[0]*w[1];
      end;
      result[cc_blue]:=0;
    end;

  FUNCTION normedDirection(CONST d:T_rgbFloatColor):T_rgbFloatColor;
    begin
      result[cc_blue ]:=arctan2(d[cc_green],d[cc_red])/2;
      result[cc_red  ]:=-sin(result[cc_blue]);
      result[cc_green]:= cos(result[cc_blue]);
      result[cc_blue ]:=0;
    end;

  begin
    result.create(dim.width,dim.height);
    for y:=0 to dim.height-1 do for x:=0 to dim.width-1 do result[x,y]:=normalAt(x,y);
    result.blur(relativeSigma,relativeSigma);
    for x:=0 to result.pixelCount-1 do result.data[x]:=normedDirection(result.data[x]);
  end;

PROCEDURE T_rawImage.lagrangeDiffusion(CONST relativeGradSigma,relativeBlurSigma:double);
  VAR dirMap:T_rawImage;
  begin
    dirMap:=directionMap(relativeGradSigma);
    lagrangeDiffusion(dirMap,relativeBlurSigma);
    dirMap.destroy;
  end;

PROCEDURE T_rawImage.lagrangeDiffusion(VAR dirMap:T_rawImage; CONST relativeBlurSigma:double; CONST changeDirection:boolean=true);
  VAR output:T_rawImage;
      kernel:T_arrayOfDouble;
      x,y,i,k,ix,iy:longint;
      pos,dir:T_rgbFloatColor;
      colSum:T_rgbFloatColor;
      wgtSum:double;

  PROCEDURE step; inline;
    VAR d:T_rgbFloatColor;
    begin
      if changeDirection then begin d:=dirMap[ix,iy]; if d[cc_red]*dir[cc_red]+d[cc_green]*dir[cc_green] > 0 then dir:=d else dir:=d*(-1); end;
      pos:=pos+dir;
      ix:=round(pos[cc_red]);
      iy:=round(pos[cc_green]);
    end;

  begin
    kernel:=getSmoothingKernel(relativeBlurSigma/100*diagonal);
    output.create(dim.width,dim.height);
    for y:=0 to dim.height-1 do
    for x:=0 to dim.width-1 do begin
      colSum:=getPixel(x,y)*kernel[0];
      wgtSum:=              kernel[0];
      for k:=0 to 1 do begin
        ix:=x;
        iy:=y;
        pos[cc_red  ]:=x;
        pos[cc_green]:=y;
        dir:=dirMap[x,y]*(k*2-1);
        step;
        for i:=1 to length(kernel)-1 do if (ix>=0) and (ix<dim.width) and (iy>=0) and (iy<dim.height) then begin
          colSum:=colSum+data[ix+iy*dim.width]*kernel[i];
          wgtSum:=wgtSum+                      kernel[i];
          step;
        end else break;
      end;
      output[x,y]:=colSum*(1/wgtSum);
    end;
    copyFromPixMap(output);
    output.destroy;
  end;

FUNCTION cartNormalCol(CONST c:T_rgbFloatColor):T_rgbFloatColor;
  begin
    result:=c*(1/sqrt(1E-6+c[cc_red]*c[cc_red]+c[cc_green]*c[cc_green]+c[cc_blue]*c[cc_blue]));
  end;

PROCEDURE T_rawImage.radialBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
  VAR dirMap:T_rawImage;
      x,y:longint;
  begin
    dirMap.create(dim.width,dim.height);
    for y:=0 to dim.height-1 do for x:=0 to dim.width-1 do
      dirMap[x,y]:=cartNormalCol(rgbColor(x/dim.width-0.5-relativeCenterX,
                                          y/dim.height-0.5-relativeCenterY,
                                          0));
    lagrangeDiffusion(dirMap,relativeBlurSigma,false);
    dirMap.destroy;
  end;

PROCEDURE T_rawImage.rotationalBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
  VAR dirMap:T_rawImage;
      x,y:longint;
  begin
    dirMap.create(dim.width,dim.height);
    for y:=0 to dim.height-1 do for x:=0 to dim.width-1 do
      dirMap[x,y]:=cartNormalCol(rgbColor(y/dim.height-0.5-relativeCenterY,
                                         -x/dim.width+0.5+relativeCenterX,
                                          0));
    lagrangeDiffusion(dirMap,relativeBlurSigma,false);
    dirMap.destroy;
  end;

PROCEDURE T_rawImage.shine;
  VAR temp:T_rawImage;
      pt:P_floatColor;
      co,ct:T_rgbFloatColor;
      fak:double;
      x,y,ix,iy,step:longint;
      anyOverbright:boolean;
  begin
    temp.create(dim.width,dim.height);
    pt:=temp.data;
    step:=1;
    repeat
      anyOverbright:=false;
      for x:=0 to dim.width*dim.height-1 do begin
        co:=data[x];
        ct:=co;
        fak:=max(1,(co[cc_red]+co[cc_green]+co[cc_blue])*(1/3));
        co:=co*(1/fak);
        data[x]:=co;
        pt[x]:=ct-co;
        anyOverbright:=anyOverbright or (fak>1.1);
      end;
      for y:=0 to dim.height-1 do
      for x:=0 to dim.width-1 do begin
        co:=pt[x+y*dim.width];
        if co<>BLACK then begin
          co:=co*(1/(2+4*step));
          for iy:=max(0,y-step) to min(dim.height-1,y+step) do data[x+iy*dim.width]:=data[x+iy*dim.width]+co;
          for ix:=max(0,x-step) to min(dim.width-1,x+step) do data[ix+y*dim.width]:=data[ix+y*dim.width]+co;
        end;
      end;
      inc(step,step);
    until (step>diagonal*0.2) or not(anyOverbright);
    temp.destroy;
  end;

PROCEDURE T_rawImage.sharpen(CONST relativeSigma,factor:double);
  VAR blurred:T_rawImage;
      x,y:longint;
  begin
    blurred.create(self);
    blurred.blur(relativeSigma,relativeSigma);
    for y:=0 to dim.height-1 do for x:=0 to dim.width-1 do pixel[x,y]:= blurred[x,y]+(pixel[x,y]-blurred[x,y])*(1+factor);
    blurred.destroy;
  end;

PROCEDURE T_rawImage.prewittEdges;
  VAR x,y,i:longint;

  begin
    //first change to greyscale:
    for i:=0 to dim.width*dim.height-1 do data[i]:=subjectiveGrey(data[i]);
    //x-convolution to green channel
    for y:=0 to dim.height-1 do begin
      data[y*dim.width][cc_green]:=0;
      for x:=1 to dim.width-2 do data[x+y*dim.width][cc_green]:=data[x+y*dim.width+1][cc_red]
                                                         -data[x+y*dim.width-1][cc_red];
      data[dim.width-1+y*dim.width][cc_green]:=0;
    end;
    //Re-convolition to blue channel
    for x:=0 to dim.width-1 do data[x][cc_blue]:=(data[x][cc_green]+data[x+dim.width][cc_green])*0.5;
    for y:=1 to dim.height-2 do for x:=0 to dim.width-1 do
      data[x+y*dim.width][cc_blue]:=data[x+y*dim.width-dim.width][cc_green]*0.2
                                  +data[x+y*dim.width     ][cc_green]*0.6
                                  +data[x+y*dim.width+dim.width][cc_green]*0.2;
    for i:=dim.width*dim.height-dim.width to dim.width*dim.height-1 do data[i][cc_blue]:=(data[i-dim.width][cc_green]+data[i][cc_green])*0.5;
    //y-convolution to green channel
                          for x:=0 to dim.width-1 do data[x       ][cc_green]:=0;
    for y:=1 to dim.height-2 do for x:=0 to dim.width-1 do data[x+y*dim.width][cc_green]:=data[x+y*dim.width+dim.width][cc_red]-data[x+y*dim.width-dim.width][cc_red];
        for i:=dim.width*dim.height-dim.width to dim.width*dim.height-1 do data[i       ][cc_green]:=0;
    //Re-convolution to red channel
    for y:=0 to dim.height-1 do begin
      data[y*dim.width][cc_red]:=(data[y*dim.width][cc_green]+data[y*dim.width+1][cc_green])*0.5;
      for x:=1 to dim.width-2 do
        data[x+y*dim.width][cc_red]:=data[x+y*dim.width-1][cc_green]*0.2
                                   +data[x+y*dim.width  ][cc_green]*0.6
                                   +data[x+y*dim.width+1][cc_green]*0.2;
      i:=dim.width-1+y*dim.width;
      data[i][cc_red]:=(data[i-1][cc_green]+data[i][cc_green])*0.5;
    end;
    for i:=0 to dim.width*dim.height-1 do data[i]:=WHITE*sqrt(sqr(data[i][cc_red])+sqr(data[i][cc_blue]));
  end;

{local} FUNCTION pot2(CONST c:T_rgbFloatColor):T_rgbFloatColor;
  VAR i:T_colorChannel;
  begin
    for i in RGB_CHANNELS do result[i]:=sqr(c[i]);
  end;

{local} FUNCTION pot3(CONST c:T_rgbFloatColor):T_rgbFloatColor;
  VAR i:T_colorChannel;
  begin
    for i in RGB_CHANNELS do result[i]:=sqr(c[i])*c[i];
  end;

PROCEDURE T_rawImage.variance(CONST relativeSigma:double);
  VAR m2:T_rawImage;
      i:longint;
  begin
    m2.create(dim.width,dim.height);
    for i:=0 to pixelCount-1 do m2.data[i]:=pot2(data[i]);
       blur(relativeSigma,relativeSigma);
    m2.blur(relativeSigma,relativeSigma);
    for i:=0 to pixelCount-1 do data[i]:=m2.data[i]-pot2(data[i]);
    m2.destroy;
  end;

PROCEDURE T_rawImage.medianFilter(CONST relativeSigma:double);
  VAR output:T_rawImage;
      x,y:longint;
      kernel:T_arrayOfDouble;
      hist:T_compoundHistogram;
  begin
    output.create(dim.width,dim.height);
    kernel:=getSmoothingKernel(relativeSigma/100*diagonal);
    for y:=0 to dim.height-1 do for x:=0 to dim.width-1 do begin
      hist:=histogram(x,y,kernel);
      output[x,y]:=rgbColor(hist.R.median,hist.G.median,hist.B.median);
    end;
    copyFromPixMap(output);
    output.destroy;
  end;

PROCEDURE T_rawImage.modalFilter(CONST relativeSigma:double);
  VAR output:T_rawImage;
      x,y:longint;
      kernel:T_arrayOfDouble;
      hist:T_compoundHistogram;
  begin
    output.create(dim.width,dim.height);
    kernel:=getSmoothingKernel(relativeSigma/100*diagonal);
    for y:=0 to dim.height-1 do for x:=0 to dim.width-1 do begin
      hist:=histogram(x,y,kernel);
      output[x,y]:=rgbColor(hist.R.mode,hist.G.mode,hist.B.mode);
    end;
    copyFromPixMap(output);
    output.destroy;
  end;

PROCEDURE T_rawImage.sketch(CONST cover,relativeDirMapSigma,density,tolerance:double);
  VAR halfwidth:double;
      fixedDensity:double;
  PROCEDURE niceLine(CONST x0,y0,x1,y1:double; CONST color:T_rgbFloatColor; CONST alpha:double);
    VAR ix,iy:longint;
        slope:double;

    FUNCTION cover(CONST z0:double; CONST z:longint):double;
      begin
        result:=halfwidth-abs(z-z0);
        if result<0 then result:=0 else if result>1 then result:=1;
      end;

    PROCEDURE xStep; inline;
      VAR y,a:double;
          k:longint;
      begin
        y:=y0+slope*(ix-x0);
        for k:=floor(y-halfwidth) to ceil(y+halfwidth) do if (k>=0) and (k<dim.height) then begin
          a:=alpha*cover(y,k);
          if a>0 then data[ix+k*dim.width]:=data[ix+k*dim.width]*(1-a)+color*a;
        end;
      end;

    PROCEDURE yStep; inline;
      VAR x,a:double;
          k:longint;
      begin
        x:=x0+slope*(iy-y0);
        for k:=floor(x-halfwidth) to ceil(x+halfwidth) do if (k>=0) and (k<dim.width) then begin
          a:=alpha*cover(x,k);
          if a>0 then data[k+iy*dim.width]:=data[k+iy*dim.width]*(1-a)+color*a;
        end;
      end;

    begin
      if abs(x1-x0)>abs(y1-y0) then begin
        slope:=(y1-y0)/(x1-x0);
        if x1>=x0
        then for ix:=max(round(x0),0) to min(dim.width-1,round(x1)) do xStep
        else for ix:=max(round(x1),0) to min(dim.width-1,round(x0)) do xStep;
      end else if abs(y1-y0)>0 then begin
        slope:=(x1-x0)/(y1-y0);
        if y1>=y0
        then for iy:=max(round(y0),0) to min(dim.height-1,round(y1)) do yStep
        else for iy:=max(round(y1),0) to min(dim.height-1,round(y0)) do yStep;
      end else begin
        ix:=round((x0+x1)/2);
        iy:=round((y0+y1)/2);
        if (ix>=0) and (ix<dim.width) and (iy>=0) and (iy<dim.height) then
        data[ix+iy*dim.width]:=
        data[ix+iy*dim.width]*(1-alpha)+color*alpha;
      end;
    end;

  FUNCTION lev(CONST i,j:longint):longint;
    VAR k:longint;
    begin
      k:=i or j;
      if k=0 then exit(12)
      else begin
        result:=0;
        while not(odd(k)) do begin
          inc(result);
          k:=k shr 1;
        end;
      end;
    end;

  VAR temp,grad:T_rawImage;
      x,y,i,imax,k,l:longint;
      lineX,lineY:array[0..1] of double;
      lineColor:T_rgbFloatColor;
      alpha:single;
      dir:T_rgbFloatColor;

  FUNCTION isTolerable(CONST fx,fy:double):boolean; inline;
    VAR ix,iy:longint;
    begin
      ix:=round(fx); if (ix<0) or (ix>=dim.width) then exit(false);
      iy:=round(fy); if (iy<0) or (iy>=dim.height) then exit(false);
      result:=colDiff(temp[ix,iy],lineColor)<=tolerance;
    end;

  begin
    halfwidth:=diagonal/1500+0.25;
    grad:=directionMap(relativeDirMapSigma);
    temp.create(self);
    for x:=0 to dim.width*dim.height-1 do data[x]:=WHITE;
    alpha:=cover;
    fixedDensity:=density/(dim.width*dim.height)*1E6;
    if fixedDensity>1 then alpha:=exp(fixedDensity*ln(cover));
    if alpha>1 then alpha:=1;
    for l:=0 to 12 do for y:=0 to dim.height-1 do for x:=0 to dim.width-1 do if (lev(x,y)=l) and (random<fixedDensity) then begin
      lineColor:=temp[x,y]+rgbColor(random-0.5,random-0.5,random-0.5)*0.05;
      dir:=grad[x,y];
      for k:=0 to 1 do begin
        i:=0;
        imax:=round(random*diagonal*0.05);
        while (i<imax) and isTolerable(x+i*dir[cc_red],y+i*dir[cc_green]) do inc(i);
        lineX[k]:=x+i*dir[cc_red];
        lineY[k]:=y+i*dir[cc_green];
        dir:=dir*(-1);
      end;
      niceLine(lineX[0],lineY[0],lineX[1],lineY[1],lineColor,(alpha));
    end;

    temp.destroy;
    grad.destroy;
  end;

PROCEDURE T_rawImage.myFilter(CONST thresholdDistParam,param:double);
  FUNCTION combine(CONST m1,m2,m3:T_rgbFloatColor):T_rgbFloatColor;
{skew=(mean-median)/[standard deviation]
skew=(M[1]-median)/s
skew=(M[3]-3M[1]s²-M[1]³)/s³
median=M[1]-s*skew
      =M[1]-s*(M[3]-3M[1]s²-M[1]³)/s³
      =M[1]-  (M[3]-3M[1]s²-M[1]³)/s²
      =M[1]-  (M[3]-3M[1]s²-M[1]³)/s²
s=sqrt(M[2]-sqr(M[1]))
 }
    VAR sigma,weight:double;
        i:T_colorChannel;
    begin
      for i in RGB_CHANNELS do begin
        sigma:=m2[i]-m1[i]*m1[i];
        if sigma<1E-8 then result[i]:=m1[i]
        else begin
          sigma:=sqrt(sigma);
          weight:=param*sigma*arctan((m3[i]-m1[i]*m1[i]*m1[i])/(sigma*sigma*sigma)-3*m1[i]/sigma);
          result[i]:=m1[i]-weight;
        end;
      end;
    end;

  VAR m2,m3:T_rawImage;
      i:longint;
  begin
    m2.create(dim.width,dim.height);
    for i:=0 to dim.width*dim.height-1 do m2.data[i]:=pot2(data[i]);
    m3.create(dim.width,dim.height);
    for i:=0 to dim.width*dim.height-1 do m3.data[i]:=pot3(data[i]);
    blur(thresholdDistParam,thresholdDistParam);
    m2.blur(thresholdDistParam,thresholdDistParam);
    m3.blur(thresholdDistParam,thresholdDistParam);
    for i:=0 to dim.width*dim.height-1 do data[i]:=combine(data[i],m2.data[i],m3.data[i]);
    m2.destroy;
    m3.destroy;
  end;

PROCEDURE T_rawImage.drip(CONST diffusiveness,range:double);
  CONST dt=0.5;
  VAR stepCount:longint;
      delta:T_rawImage;
  PROCEDURE applyDelta;
    VAR i:longint;
    begin
      for i:=0 to pixelCount-1 do data[i]:=data[i]+delta.data[i]*dt;
    end;

  PROCEDURE computeDelta;
    VAR x,y:longint;
        v:double;
        flux:T_rgbFloatColor;
    begin
      for x:=0 to pixelCount-1 do delta.data[x]:=BLACK;
      for y:=0 to dim.height-1 do for x:=0 to dim.width-1 do begin
        v:=T_hsvColor(pixel[x,y])[hc_saturation];
        if v>1 then v:=1 else if v<0 then v:=0;
        flux:=pixel[x,y]*v;
                         delta[x,y  ]:=delta[x,y  ]-flux;
        if y<dim.height-1 then delta[x,y+1]:=delta[x,y+1]+flux;
      end;
      if diffusiveness>0 then begin;
        for y:=0 to dim.height-1 do for x:=0 to dim.width-2 do begin
          flux:=(pixel[x,y]-pixel[x+1,y])*diffusiveness;
          delta[x  ,y]:=delta[x  ,y]-flux;
          delta[x+1,y]:=delta[x+1,y]+flux;
        end;
        for y:=0 to dim.height-2 do for x:=0 to dim.width-1 do begin
          flux:=(pixel[x,y]-pixel[x,y+1])*diffusiveness;
          delta[x,y  ]:=delta[x,y  ]-flux;
          delta[x,y+1]:=delta[x,y+1]+flux;
        end;
      end;
    end;

  VAR i:longint;
  begin
    stepCount:=round(range*diagonal/dt);
    delta.create(dim.width,dim.height);
    for i:=0 to stepCount-1 do begin
      computeDelta;
      applyDelta;
    end;
    delta.destroy;
  end;

//PROCEDURE T_rawImage.encircle(CONST count:longint; CONST background:T_rgbFloatColor; CONST opacity,relativeCircleSize:double; CONST containingQueue:P_progressEstimatorQueue);
//  TYPE T_circle=record
//         cx,cy,radius,diff:double;
//         color:T_rgbFloatColor;
//       end;
//
//  FUNCTION randomCircle(CONST radius:double):T_circle;
//    begin
//      result.cx:=radius+random*(dim.width-2*radius);
//      result.cy:=radius+random*(dim.height-2*radius);
//      result.radius:=radius;
//      result.diff:=0;
//    end;
//
//  FUNCTION avgColor(VAR source:T_rawImage; CONST circle:T_circle):T_rgbFloatColor;
//    VAR sampleCount:longint=0;
//        sqrRad:double;
//        x,y:longint;
//    begin
//      sqrRad:=sqr(circle.radius);
//      result:=BLACK;
//      with circle do
//      for y:=max(0,round(cy-radius)) to min(dim.height-1,round(cy+radius)) do
//      for x:=max(0,round(cx-radius)) to min(dim.width-1,round(cx+radius)) do
//      if sqr(x-cx)+sqr(y-cy)<=sqrRad then
//      begin
//        result:=result+source[x,y];
//        inc(sampleCount);
//      end;
//      if sampleCount>0 then result:=result*(1/sampleCount);
//    end;
//
//  VAR copy:T_rawImage;
//      i,j:longint;
//      newCircle,toDraw: T_circle;
//
//  FUNCTION globalAvgDiff:double;
//    VAR i:longint;
//    begin
//      result:=0;
//      for i:=0 to dim.width*dim.height-1 do result:=result+colDiff(copy.data[i],data[i]);
//      result:=result/(dim.width*dim.height);
//    end;
//
//  PROCEDURE drawCircle(CONST circle:T_circle);
//    VAR sqrRad:double;
//        x,y,k:longint;
//        r:double;
//    begin
//      sqrRad:=sqr(circle.radius+1);
//      with circle do
//      for y:=max(0,floor(cy-radius)) to min(dim.height-1,ceil(cy+radius)) do
//      for x:=max(0,floor(cx-radius)) to min(dim.width-1,ceil(cx+radius)) do begin
//        r:=sqr(x-cx)+sqr(y-cy);
//        if r<=sqrRad then
//        begin
//          k:=x+y*dim.width;
//          r:=sqrt(r);
//          if r<radius-0.5 then r:=opacity
//          else if r>radius+0.5 then r:=0
//          else r:=(radius+0.5-r)*opacity;
//          if r>0 then data[k]:=data[k]*(1-r)+color*r;
//        end;
//
//      end;
//    end;
//
//  FUNCTION bestCircle(CONST radius:double):T_circle;
//    VAR x,y,cx,cy:longint;
//        diff:double;
//        maxDiff:double=0;
//    begin
//      if (radius>0.5*min(dim.width,dim.height)) then exit(bestCircle(0.499*min(dim.width,dim.height)));
//      for y:=round(radius) to round(dim.height-radius) do
//      for x:=round(radius) to round(dim.width-radius) do begin
//        diff:=colDiff(pixel[x,y],copy[x,y]);
//        if (diff>maxDiff) then begin
//          cx:=x;
//          cy:=y;
//          maxDiff:=diff;
//        end;
//      end;
//      result.cx:=cx;
//      result.cy:=cy;
//      result.radius:=radius;
//      result.diff:=diff;
//      result.color:=avgColor(copy,result);
//    end;
//
//  VAR radius:double;
//      circleSamples:longint=1;
//      progress:T_progressEstimatorQueue;
//      oldChildOfContainingQueue:P_progressEstimatorQueue;
//  begin
//    progress.create();
//    if containingQueue<>nil then begin
//      containingQueue^.setTemporaryChildProgress(oldChildOfContainingQueue,@progress);
//    end;
//    progress.forceStart(et_stepCounterWithoutQueue,count);
//    radius:=relativeCircleSize*diagonal;
//    copy.create(self);
//    for i:=0 to pixelCount-1 do data[i]:=background;
//    for i:=0 to count-1 do begin
//      if ((i*1000) div count<>((i-1)*1000) div count) or (radius>=0.1*diagonal) then begin
//        if progress.cancellationRequested then break;
//        radius:=max(relativeCircleSize*diagonal*min(1,1/6*globalAvgDiff),1);
//        circleSamples:=round(10000/sqr(radius));
//        if circleSamples>31 then circleSamples:=31;
//      end;
//      initialize(toDraw);
//      for j:=0 to circleSamples do begin
//        newCircle:=randomCircle(radius);
//        newCircle.color:=avgColor(copy,newCircle);
//        newCircle.diff:=colDiff(avgColor(self,newCircle),newCircle.color);
//        if (j=0) or (newCircle.diff>toDraw.diff) then toDraw:=newCircle;
//      end;
//      drawCircle(toDraw);
//      progress.logStepDone;
//    end;
//    copy.destroy;
//    progress.logEnd;
//    if containingQueue<>nil then begin
//      containingQueue^.setTemporaryChildProgress(oldChildOfContainingQueue);
//    end;
//    progress.destroy;
//  end;
//
//PROCEDURE T_rawImage.bySpheres(CONST count:longint; CONST style:byte; CONST relativeCircleSize0,relativeCircleSize1:double; CONST containingQueue:P_progressEstimatorQueue);
//  VAR copy:T_rawImage;
//
//  FUNCTION avgColor(VAR source:T_rawImage; CONST cx,cy,radius:double):T_rgbFloatColor;
//    VAR sampleCount:longint=0;
//        sqrRad:double;
//        x,y:longint;
//    begin
//      sqrRad:=sqr(radius);
//      result:=BLACK;
//      for y:=max(0,round(cy-radius)) to min(dim.height-1,round(cy+radius)) do
//      for x:=max(0,round(cx-radius)) to min(dim.width-1,round(cx+radius)) do
//      if sqr(x-cx)+sqr(y-cy)<=sqrRad then
//      begin
//        result:=result+source[x,y];
//        inc(sampleCount);
//      end;
//      if sampleCount>0 then result:=result*(1/sampleCount);
//    end;
//
//  FUNCTION getColorForPixel(CONST ix,iy:longint; CONST cx,cy,radius:double; CONST baseColor,previousColor:T_rgbFloatColor):T_rgbFloatColor;
//    FUNCTION illumination(x,y,r2:double):single;
//      VAR ambient:single;
//      begin
//        ambient:=x*0.30151134457776363-
//        y         *0.30151134457776363+
//        sqrt(1-r2)*0.90453403373329089;
//        result:=ambient*0.75+0.25;
//      end;
//
//    VAR x,y,r2:double;
//        cover:single;
//    begin
//      x:=(ix-cx)/radius;
//      y:=(iy-cy)/radius;
//      r2:=sqr(x)+sqr(y);
//      if r2<1 then begin
//        result:=baseColor*illumination(x,y,r2);
//      end else begin
//        r2:=sqrt(r2);
//        cover:=(1-(r2-1)*radius);
//        if cover<0
//        then result:=previousColor
//        else result:=baseColor    *illumination(x/r2,y/r2,1)*cover+
//                     previousColor*                  (1-cover);
//      end;
//    end;
//
//  PROCEDURE drawRandomSphere(CONST radius,avgWeight:double);
//    FUNCTION getImprovement(CONST cx,cy:double):double;
//      VAR x,y:longint;
//          prevError,newError:double;
//          avg:T_rgbFloatColor;
//      begin
//        avg:=avgColor(copy,cx,cy,radius);
//        result:=0;
//        for y:=max(0,round(cy-radius)) to min(dim.height-1,round(cy+radius)) do
//        for x:=max(0,round(cx-radius)) to min(dim.width -1,round(cx+radius)) do begin
//          prevError:=colDiff(copy[x,y],pixel[x,y]);
//          newError :=colDiff(copy[x,y],getColorForPixel(x,y,cx,cy,radius,avg*avgWeight+copy[x,y]*(1-avgWeight),pixel[x,y]));
//          result+=prevError-newError;
//        end;
//      end;
//
//    VAR x,y:longint;
//        best_cx,best_cy,best_imp:double;
//        cx,cy,imp:double;
//        avg:T_rgbFloatColor;
//        i:longint;
//    begin
//      best_imp:=-infinity;
//      for i:=0 to round(min(20,30000/sqr(radius))) do begin
//        cx:=random*(dim.width +radius)-0.5*radius;
//        cy:=random*(dim.height+radius)-0.5*radius;
//        imp:=getImprovement(cx,cy);
//        if (imp>best_imp) then begin
//          best_imp:=imp;
//          best_cx :=cx;
//          best_cy :=cy;
//        end;
//      end;
//      cx:=best_cx;
//      cy:=best_cy;
//      avg:=avgColor(copy,cx,cy,radius);
//      for y:=max(0,round(cy-radius)) to min(dim.height-1,round(cy+radius)) do
//      for x:=max(0,round(cx-radius)) to min(dim.width -1,round(cx+radius)) do
//        pixel[x,y]:=getColorForPixel(x,y,cx,cy,radius,avg*avgWeight+copy[x,y]*(1-avgWeight),pixel[x,y]);
//    end;
//
//  VAR radius,avgWeight,ra,rb:double;
//      progress:T_progressEstimatorQueue;
//      oldChildOfContainingQueue:P_progressEstimatorQueue;
//      i:longint;
//  begin
//    progress.create();
//    if containingQueue<>nil then begin
//      containingQueue^.setTemporaryChildProgress(oldChildOfContainingQueue,@progress);
//    end;
//    progress.forceStart(et_stepCounterWithoutQueue,count);
//    copy.create(self);
//    rb:=relativeCircleSize1/(relativeCircleSize0-relativeCircleSize1);
//    ra:=rb*relativeCircleSize0*diagonal;
//    if      style=0 then avgWeight:=1
//    else if style=1 then avgWeight:=0;
//    for i:=0 to pixelCount-1 do data[i]:=BLACK;
//    for i:=0 to count-1 do begin
//      //radius:=exp((1-i/(count-1))*ln(relativeCircleSize0)+
//      //               i/(count-1) *ln(relativeCircleSize1))*diagonal;
//      radius:=ra/(rb+i/(count-1));
//      case style of
//        2: avgWeight:=  i/(count-1);
//        3: avgWeight:=1-i/(count-1);
//      end;
//      drawRandomSphere(radius,avgWeight);
//      progress.logStepDone;
//    end;
//    copy.destroy;
//    progress.logEnd;
//    if containingQueue<>nil then begin
//      containingQueue^.setTemporaryChildProgress(oldChildOfContainingQueue);
//    end;
//    progress.destroy;
//  end;
//
//TYPE
//P_nlmWorkerThreadTodo=^T_nlmWorkerThreadTodo;
//T_nlmWorkerThreadTodo=object(T_queueToDo)
//  scanRadius:longint;
//  sigma:double;
//  pIn:P_floatColor;
//  y0:longint;
//  target: P_rawImage;
//  expLUT:array[0..31] of double;
//  CONSTRUCTOR create(CONST scanRadius_:longint;
//    CONST sigma_:double;
//    CONST input_:P_floatColor;
//    CONST y_:longint; CONST target_: P_rawImage);
//  PROCEDURE execute; virtual;
//end;
//
//CONSTRUCTOR T_nlmWorkerThreadTodo.create(CONST scanRadius_:longint;
//  CONST sigma_:double;
//  CONST input_:P_floatColor;
//  CONST y_:longint; CONST target_: P_rawImage);
//  VAR i:longint;
//  begin
//    scanRadius:=scanRadius_;
//    sigma     :=sigma_;
//    pIn       :=input_;
//    y0        :=y_;
//    target    :=target_;
//    for i:=0 to length(expLUT)-1 do expLUT[i]:=exp(-i*0.5/sigma);
//  end;
//
//PROCEDURE T_nlmWorkerThreadTodo.execute;
//  VAR dim:T_imageDimensions;
//  FUNCTION patchDistF(x0,y0,x1,y1:longint):double; inline;
//    CONST PATCH_KERNEL:array[-2..2,-2..2] of double=
//          (( 6.517, 9.095,10.164, 9.095, 6.517),
//           ( 9.095,12.693,14.184,12.693, 9.095),
//           (10.164,14.184, 0.0  ,14.184,10.164),
//           ( 9.095,12.693,14.184,12.693, 9.095),
//           ( 6.517, 9.095,10.164, 9.095, 6.517));
//    VAR dx,dy:longint;
//        c0,c1:T_rgbFloatColor;
//        i:longint;
//    begin
//      result:=0;
//      for dy:=max(-2,max(-y0,-y1)) to min(2,dim.height-1-max(y0,y1)) do
//      for dx:=max(-2,max(-x0,-x1)) to min(2,dim.width -1-max(x0,x1)) do begin
//        c0:=pIn[x0+dx+(y0+dy)*dim.width];
//        c1:=pIn[x1+dx+(y1+dy)*dim.width];
//        result:=result+(sqr(c0[cc_red  ]-c1[cc_red  ])
//                       +sqr(c0[cc_green]-c1[cc_green])
//                       +sqr(c0[cc_blue ]-c1[cc_blue ]))*PATCH_KERNEL[dy,dx];
//      end;
//      if isInfinite(result) or isNan(result) then exit(0);
//      i:=round(result);
//      if i<0 then i:=0 else if i>=length(expLUT) then i:=length(expLUT)-1;
//      result:=expLUT[i];
//    end;
//
//  FUNCTION filteredColorAtF(x,y:longint):T_rgbFloatColor;
//    VAR w,wTot,wMax:double;
//        dx,dy:longint;
//    begin
//      wTot:=0;
//      wMax:=0;
//      result:=myColors.BLACK;
//      for dy:=max(-scanRadius,-y) to min(scanRadius,dim.height-1-y) do
//      for dx:=max(-scanRadius,-x) to min(scanRadius,dim.width -1-x) do
//      if (dx<1-scanRadius) or (dx>scanRadius-1) or (dy<1-scanRadius) or (dy>scanRadius-1) then
//      begin
//        w:=patchDistF(x,y,x+dx,y+dy);
//        if w>wMax then wMax:=w;
//        result:=result+pIn[x+dx+(y+dy)*dim.width]*w;
//        wTot  :=wTot  +                           w;
//      end;
//      result:=result+pIn[x+y*dim.width]*wMax;
//      wTot  :=wTot  +                   wMax;
//      if wTot<1E-5 then result:=pIn[x+y*dim.width]
//                   else result:=result*(1/wTot);
//    end;
//  VAR y,x:longint;
//  begin
//    dim:=target^.dimensions;
//    for y:=y0 to min(y0+15,dim.height-1) do
//    for x:=0 to dim.width-1 do
//      target^[x,y]:=filteredColorAtF(x,y);
//    parentQueue^.logStepDone;
//    state:=fts_ready;
//  end;
//
//PROCEDURE T_rawImage.nlmFilter(CONST scanRadius:longint; CONST sigma:double; CONST queue:P_progressEstimatorQueue);
//  VAR temp :T_rawImage;
//      pIn  :P_floatColor;
//      y:longint;
//      task:P_nlmWorkerThreadTodo;
//      nlmQueue:T_progressEstimatorQueue;
//      oldChildOfContainingQueue:P_progressEstimatorQueue;
//
//  begin
//    if sigma<1E-10 then exit;
//    temp.create(self);
//    pIn:=temp.data;
//    nlmQueue.create();
//    nlmQueue.forceStart(et_stepCounter_parallel,dim.height div 16);
//    queue^.setTemporaryChildProgress(oldChildOfContainingQueue,@nlmQueue);
//    for y:=0 to dim.height-1 do if y and 15=0 then begin
//      task:=nil;
//      new(task,create(scanRadius,sigma,pIn,y,@self));
//      nlmQueue.enqueue(task);
//    end;
//    repeat until not(nlmQueue.activeDeqeue);
//    nlmQueue.waitForEndOfCalculation;
//    queue^.setTemporaryChildProgress(oldChildOfContainingQueue);
//    nlmQueue.destroy;
//    temp.destroy;
//  end;
//
//TYPE
//P_modMedWorkerThreadTodo=^T_modMedWorkerThreadTodo;
//T_modMedWorkerThreadTodo=object(T_queueToDo)
//  pIn:P_floatColor;
//  y0:longint;
//  target: P_rawImage;
//  CONSTRUCTOR create(
//    CONST input_:P_floatColor;
//    CONST y_:longint; CONST target_: P_rawImage);
//  PROCEDURE execute; virtual;
//end;
//
//CONSTRUCTOR T_modMedWorkerThreadTodo.create(
//  CONST input_:P_floatColor;
//  CONST y_:longint; CONST target_: P_rawImage);
//  begin
//    pIn       :=input_;
//    y0        :=y_;
//    target    :=target_;
//  end;
//
//PROCEDURE T_modMedWorkerThreadTodo.execute;
//  VAR dim:T_imageDimensions;
//  FUNCTION filteredColorAtF(CONST i,j:longint):T_rgbFloatColor;
//    FUNCTION pixel(CONST x,y:longint):T_rgbFloatColor;
//      begin
//        if (x<0) or (x>=dim.width) or (y<0) or (y>=dim.height)
//        then result:=BLACK
//        else result:=pIn[x+y*dim.width];
//      end;
//
//    VAR
//      adj:array[0..20] of record dist:double; col:T_rgbFloatColor; end;
//      k,di,dj:longint;
//      channel:T_colorChannel;
//    CONST
//      delta:array[0..19,0..1] of longint=((-2,-1),(-2,0),(-2,1),(-1,-2),(-1,-1),(-1,0),(-1,1),(-1,2),(0,-2),(0,-1),(0,1),(0,2),(1,-2),(1,-1),(1,0),(1,1),(1,2),(2,-1),(2,0),(2,1));
//
//    begin
//      //compute local 3x3 stencil differences
//      for k:=0 to 19 do begin
//        adj[k].dist:=0;
//        adj[k].col:=pixel(i+delta[k,0],j+delta[k,1]);
//        for di:=-1 to 1 do for dj:=-1 to 1 do
//          adj[k].dist+=colDiff(pixel(delta[k,0]+i+di,delta[k,1]+j+dj),
//                               pixel(           i+di,           j+dj));
//      end;
//      //sort by ascending distance
//      for di:=1 to 19 do for dj:=0 to di-1 do if adj[di].dist<adj[dj].dist then begin
//        adj[20]:=adj[di];
//        adj[di]:=adj[dj];
//        adj[dj]:=adj[20];
//      end;
//      //compute median of closest 11 samples
//      for di:=1 to 10 do for dj:=0 to di-1 do for channel:=cc_red to cc_blue do if adj[di].col[channel]<adj[dj].col[channel] then begin
//        adj[11].col[channel]:=adj[di].col[channel];
//        adj[di].col[channel]:=adj[dj].col[channel];
//        adj[dj].col[channel]:=adj[11].col[channel];
//      end;
//      result:=adj[5].col;
//    end;
//
//  VAR y,x:longint;
//  begin
//    dim:=target^.dimensions;
//    for y:=y0 to min(y0+15,dim.height-1) do
//    for x:=0 to dim.width-1 do
//      target^[x,y]:=filteredColorAtF(x,y);
//    parentQueue^.logStepDone;
//    state:=fts_ready;
//  end;
//
//PROCEDURE T_rawImage.modMedFilter(CONST queue:P_progressEstimatorQueue);
//  VAR temp :T_rawImage;
//      pIn  :P_floatColor;
//      y:longint;
//      task:P_modMedWorkerThreadTodo;
//      modMedQueue:T_progressEstimatorQueue;
//      oldChildOfContainingQueue:P_progressEstimatorQueue;
//
//  begin
//    temp.create(self);
//    pIn:=temp.data;
//    modMedQueue.create();
//    modMedQueue.forceStart(et_stepCounter_parallel,dim.height div 16);
//    queue^.setTemporaryChildProgress(oldChildOfContainingQueue,@modMedQueue);
//    for y:=0 to dim.height-1 do if y and 15=0 then begin
//      task:=nil;
//      new(task,create(pIn,y,@self));
//      modMedQueue.enqueue(task);
//    end;
//    repeat until not(modMedQueue.activeDeqeue);
//    modMedQueue.waitForEndOfCalculation;
//    queue^.setTemporaryChildProgress(oldChildOfContainingQueue);
//    modMedQueue.destroy;
//    temp.destroy;
//  end;
//
FUNCTION T_rawImage.rgbaSplit(CONST transparentColor:T_rgbFloatColor):T_rawImage;
  PROCEDURE rgbToRGBA(CONST c00,c01,c02,
                            c10,c11,c12,
                            c20,c21,c22,
                            transparentColor:T_rgbFloatColor; OUT rgb:T_rgbFloatColor; OUT alpha:single);
    VAR aMax,a:single;
    begin
      aMax :=abs(c00[cc_red]-transparentColor[cc_red])+abs(c00[cc_green]-transparentColor[cc_green])+abs(c00[cc_blue]-transparentColor[cc_blue]);
      a    :=abs(c01[cc_red]-transparentColor[cc_red])+abs(c01[cc_green]-transparentColor[cc_green])+abs(c01[cc_blue]-transparentColor[cc_blue]); if a    >aMax then aMax:=a;
      a    :=abs(c02[cc_red]-transparentColor[cc_red])+abs(c02[cc_green]-transparentColor[cc_green])+abs(c02[cc_blue]-transparentColor[cc_blue]); if a    >aMax then aMax:=a;
      a    :=abs(c10[cc_red]-transparentColor[cc_red])+abs(c10[cc_green]-transparentColor[cc_green])+abs(c10[cc_blue]-transparentColor[cc_blue]); if a    >aMax then aMax:=a;
      alpha:=abs(c11[cc_red]-transparentColor[cc_red])+abs(c11[cc_green]-transparentColor[cc_green])+abs(c11[cc_blue]-transparentColor[cc_blue]); if alpha>aMax then aMax:=alpha;
      a    :=abs(c12[cc_red]-transparentColor[cc_red])+abs(c12[cc_green]-transparentColor[cc_green])+abs(c12[cc_blue]-transparentColor[cc_blue]); if a    >aMax then aMax:=a;
      a    :=abs(c20[cc_red]-transparentColor[cc_red])+abs(c20[cc_green]-transparentColor[cc_green])+abs(c20[cc_blue]-transparentColor[cc_blue]); if a    >aMax then aMax:=a;
      a    :=abs(c21[cc_red]-transparentColor[cc_red])+abs(c21[cc_green]-transparentColor[cc_green])+abs(c21[cc_blue]-transparentColor[cc_blue]); if a    >aMax then aMax:=a;
      a    :=abs(c22[cc_red]-transparentColor[cc_red])+abs(c22[cc_green]-transparentColor[cc_green])+abs(c22[cc_blue]-transparentColor[cc_blue]); if a    >aMax then aMax:=a;
      if aMax>1E-3 then begin
        alpha:=max(0,min(1,alpha/aMax));
        rgb:=(c11-transparentColor*(1-alpha))*(1/alpha);
      end else begin
        rgb:=BLACK;
        alpha:=0;
      end;
    end;
  VAR x,y,xm,ym:longint;
      rgb:T_rgbFloatColor;
      alpha:single;
      source:T_rawImage;
  begin
    result.create(dim.width,dim.height);
    source.create(self);
    xm:=dim.width-1;
    ym:=dim.height-1;
    for y:=0 to ym do for x:=0 to xm do begin
      rgbToRGBA(source[max( 0,x-1),max( 0,y-1)],
                source[       x   ,max( 0,y-1)],
                source[min(xm,x+1),max( 0,y-1)],
                source[max( 0,x-1),       y   ],
                source[       x   ,       y   ],
                source[min(xm,x+1),       y   ],
                source[max( 0,x-1),min(ym,y+1)],
                source[       x   ,min(ym,y+1)],
                source[min(xm,x+1),min(ym,y+1)],
                transparentColor,rgb,alpha);
      pixel[x,y]:=rgb;
      result[x,y]:=WHITE*alpha;
    end;
  end;

PROCEDURE T_rawImage.halftone(CONST scale:single; CONST param:longint);
  VAR xRes,yRes:longint;
      temp:T_rawImage;

  FUNCTION avgSqrRad(x0,y0,rad:single):T_rgbFloatColor; inline;
    VAR x,y,k:longint;
    begin
      result:=BLACK;
      k:=0;
      for y:=max(0,floor(y0-rad)) to min(yRes-1,ceil(y0+rad)) do
      for x:=max(0,floor(x0-rad)) to min(xRes-1,ceil(x0+rad)) do
      if sqr(x-x0)+sqr(y-y0)<sqr(rad) then begin
        result:=result+pixel[x,y];
        inc(k);
      end;
      if k>0 then begin
        result:=result*(1/k);
        if result[cc_red  ]<0 then result[cc_red  ]:=0 else result[cc_red  ]:=sqr(rad*2)*result[cc_red  ]/pi;
        if result[cc_green]<0 then result[cc_green]:=0 else result[cc_green]:=sqr(rad*2)*result[cc_green]/pi;
        if result[cc_blue ]<0 then result[cc_blue ]:=0 else result[cc_blue ]:=sqr(rad*2)*result[cc_blue ]/pi;
      end;
    end;

  FUNCTION avgCover(x,y,sqrRad:single):single; inline;
    VAR k,i,j:longint;
    begin
      k:=0;
      if sqr(x-0.375)+sqr(y-0.375)<sqrRad then inc(k);
      if sqr(x-0.375)+sqr(y+0.375)<sqrRad then inc(k);
      if sqr(x+0.375)+sqr(y-0.375)<sqrRad then inc(k);
      if sqr(x+0.375)+sqr(y+0.375)<sqrRad then inc(k);
      if k=0 then result:=0 else if k=4 then result:=1
      else begin
        for i:=0 to 3 do for j:=0 to 3 do
          if sqr(x-0.375+i*0.25)+
             sqr(y-0.375+j*0.25)<sqrRad then inc(k);
        result:=(k-4)/16;
      end;
    end;

  PROCEDURE paintCircle(x0,y0:single; rad:T_rgbFloatColor);
    VAR cov,col:T_rgbFloatColor;
        x,y:longint;
        mrad:single;
    begin
      mrad:=sqrt(max(rad[cc_red],max(rad[cc_green],rad[cc_blue])));
      for y:=max(0,floor(y0-mrad)) to min(yRes-1,ceil(y0+mrad)) do
      for x:=max(0,floor(x0-mrad)) to min(xRes-1,ceil(x0+mrad)) do begin
        cov[cc_red  ]:=avgCover(x-x0,y-y0,rad[cc_red  ]);
        cov[cc_green]:=avgCover(x-x0,y-y0,rad[cc_green]);
        cov[cc_blue ]:=avgCover(x-x0,y-y0,rad[cc_blue ]);
        col:=pixel[x,y];
        col[cc_red  ]:=max(col[cc_red  ],cov[cc_red  ]);
        col[cc_green]:=max(col[cc_green],cov[cc_green]);
        col[cc_blue ]:=max(col[cc_blue ],cov[cc_blue ]);
        pixel[x,y]:=col;
      end;
    end;

  PROCEDURE paintCircle(x0,y0,rad:single; channel:T_colorChannel);
    VAR cov:single;
        col:T_rgbFloatColor;
        x,y:longint;
    begin
      for y:=max(0,floor(y0-sqrt(rad))) to min(yRes-1,ceil(y0+sqrt(rad))) do
      for x:=max(0,floor(x0-sqrt(rad))) to min(xRes-1,ceil(x0+sqrt(rad))) do begin
        cov:=avgCover(x-x0,y-y0,rad);
        col:=pixel[x,y];
        col[channel]:=max(col[channel],cov);
        pixel[x,y]:=col;
      end;
    end;

  VAR x,y:longint;
      sx:single;
      pt:P_floatColor;
  begin
    xRes:=dim.width;
    yRes:=dim.height;
    pt:=rawData;
    if param and 1=1 then for x:=0 to dim.width*dim.height-1 do pt[x]:=WHITE-pt[x];
    //analyze:
    if param and 4=0 then begin
      temp.create(ceil(xRes/scale+2),ceil(yRes/scale+2));
      if param and 2=0 then begin
        for y:=0 to temp.dim.height-1 do for x:=0 to temp.dim.width-1 do temp[x,y]:=avgSqrRad(x*scale,y*scale,0.5*scale);
      end else begin
        for y:=0 to temp.dim.height-1 do for x:=0 to temp.dim.width-1 do temp[x,y]:=rgbColor(
          avgSqrRad( x     *scale, y     *scale,0.5*scale)[cc_red  ],
          avgSqrRad( x     *scale,(y+0.5)*scale,0.5*scale)[cc_green],
          avgSqrRad((x+0.5)*scale, y     *scale,0.5*scale)[cc_blue ]);
      end;
    end else begin
      temp.create(ceil(xRes/scale+2),ceil(yRes/(scale*0.86602540378444)+2));
      if param and 2=0 then begin
        for y:=0 to temp.dim.height-1 do begin
          if odd(y) then sx:=0.5*scale else sx:=0;
          for x:=0 to temp.dim.width-1 do temp[x,y]:=avgSqrRad(sx+x*scale,y*scale*0.86602540378444,0.5*scale);
        end;
      end else begin
        for y:=0 to temp.dim.height-1 do begin
          if odd(y) then sx:=0.5*scale else sx:=0;
          for x:=0 to temp.dim.width-1 do temp[x,y]:=rgbColor(
            avgSqrRad(sx+ x      *scale, y     *scale*0.86602540378444,0.5*scale)[cc_red  ],
            avgSqrRad(sx+(x+0.25)*scale,(y+0.5)*scale*0.86602540378444,0.5*scale)[cc_green],
            avgSqrRad(sx+(x+0.5 )*scale, y     *scale*0.86602540378444,0.5*scale)[cc_blue ]);
        end;
      end;
    end;
    //:analyze
    //draw:
    pt:=rawData;
    for x:=0 to xRes*yRes-1 do pt[x]:=BLACK;
    if param and 4=0 then begin
      if param and 2=0 then begin
        for y:=0 to temp.dim.height-1 do for x:=0 to temp.dim.width-1 do paintCircle(x*scale,y*scale,temp[x,y]);
      end else begin
        for y:=0 to temp.dim.height-1 do for x:=0 to temp.dim.width-1 do begin
          paintCircle( x     *scale, y     *scale,temp[x,y][cc_red  ],cc_red  );
          paintCircle( x     *scale,(y+0.5)*scale,temp[x,y][cc_green],cc_green);
          paintCircle((x+0.5)*scale, y     *scale,temp[x,y][cc_blue ],cc_blue );
        end;
      end;
    end else begin
      if param and 2=0 then begin
        for y:=0 to temp.dim.height-1 do begin
          if odd(y) then sx:=0.5*scale else sx:=0;
          for x:=0 to temp.dim.width-1 do paintCircle(sx+x*scale,y*scale*0.86602540378444,temp[x,y]);
        end;
      end else begin
        for y:=0 to temp.dim.height-1 do for x:=0 to temp.dim.width-1 do begin
          if odd(y) then sx:=0.5*scale else sx:=0;
          paintCircle(sx+(x-0.5 )*scale, y     *scale*0.86602540378444,temp[x,y][cc_red  ],cc_red  );
          paintCircle(sx+(x-0.25)*scale,(y+0.5)*scale*0.86602540378444,temp[x,y][cc_green],cc_green);
          paintCircle(sx+(x     )*scale, y     *scale*0.86602540378444,temp[x,y][cc_blue ],cc_blue );
        end;
      end;
    end;
    temp.destroy;
    if param and 1=1 then for x:=0 to xRes*yRes-1 do pt[x]:=WHITE-pt[x];
  end;

PROCEDURE T_rawImage.rotate(CONST angleInDegrees:double);
  VAR A:array[0..1] of double;
      temp:T_rawImage;
      x,y:longint;
      cx,cy:double;
      i,j:double;

  FUNCTION smoothSafePixel(fx,fy:double):T_rgbColor;
    VAR kx,ky:array[0..1] of longint;
        w:array[0..1,0..1] of double;
        i,j:longint;
    begin
      kx[0]:=floor(fx); kx[1]:=kx[0]+1; fx-=kx[0];
      ky[0]:=floor(fy); ky[1]:=ky[0]+1; fy-=ky[0];
      w[0,0]:=(1-fx)*(1-fy);
      w[0,1]:=(1-fx)*   fy ;
      w[1,0]:=   fx *(1-fy);
      w[1,1]:=   fx *   fy ;
      result:=BLACK;
      for i:=0 to 1 do for j:=0 to 1 do if
         (kx[i]>=0) and (kx[i]<dim.width) and
         (ky[j]>=0) and (ky[j]<dim.height) then
        result+=temp.pixel[kx[i],ky[j]]*w[i,j];
    end;

  begin
    A[0]:=cos(angleInDegrees/180*pi);
    A[1]:=sin(angleInDegrees/180*pi);
    temp.create(self);
    cx:=(dim.width-1)/2;
    cy:=(dim.height-1)/2;
    for y:=dim.height-1 downto 0 do
    for x:=0 to dim.width-1 do begin
      i:=x-cx;
      j:=y-cy;
      pixel[x,y]:=smoothSafePixel(
        A[0]*i+A[1]*j+cx,
        A[0]*j-A[1]*i+cy);
    end;
    temp.destroy;
  end;

PROCEDURE T_rawImage.copyFromImageWithOffset(VAR image:T_rawImage; CONST xOff,yOff:longint);
  VAR sx,sy,tx,ty:longint;
  begin
    for sy:=0 to image.dimensions.height-1 do begin
      ty:=sy+yOff;
      if (ty>=0) and (ty<dimensions.height) then
      for sx:=0 to image.dimensions.width-1 do begin
        tx:=sx+xOff;
        if (tx>=0) and (tx<dimensions.width) then pixel[tx,ty]:=image[sx,sy];
      end;
    end;
  end;

INITIALIZATION
  initCriticalSection(globalFileLock);
FINALIZATION
  doneCriticalSection(globalFileLock);

end.
