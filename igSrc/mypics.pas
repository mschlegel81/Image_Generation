UNIT mypics;
INTERFACE
{$fputype sse3}
USES myColors,dos,sysutils,Interfaces, ExtCtrls, Graphics, IntfGraphics, GraphType,types,myGenerics, mySys,math, myParams,FPWriteJPEG,FileUtil,myTools;

{$define include_interface}

CONST CHUNK_BLOCK_SIZE =64;

TYPE
  T_resizeStyle=(res_exact,
                 res_cropToFill,
                 res_fit,
                 res_dataResize);

  T_pendingList=array of longint;

  T_structuredHitColor=record
    rest:T_floatColor;
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

  P_rawImage=^T_rawImage;
  T_rawImage=object
    private
      xRes,yRes:longint;
      datFloat:P_floatColor;

      //Accessors:--------------------------------------------------------------
      PROCEDURE setPixel(CONST x,y:longint; CONST value:T_floatColor);
      FUNCTION getPixel(CONST x,y:longint):T_floatColor;
      FUNCTION getPixel24Bit(CONST x,y:longint):T_24Bit;
      //--------------------------------------------------------------:Accessors
      //Helper routines:--------------------------------------------------------
      PROCEDURE copyToImage(CONST srcRect:TRect; VAR destImage: TImage);
      //--------------------------------------------------------:Helper routines
    public
      CONSTRUCTOR create(CONST width_,height_:longint);
      CONSTRUCTOR create(CONST fileName:ansistring);
      CONSTRUCTOR create(VAR original:T_rawImage);
      DESTRUCTOR destroy;
      //Access per pixel:-------------------------------------------------------
      FUNCTION width:longint;
      FUNCTION height:longint;
      FUNCTION diagonal:double;
      PROPERTY pixel     [x,y:longint]:T_floatColor read getPixel write setPixel; default;
      PROPERTY pixel24Bit[x,y:longint]:T_24Bit read getPixel24Bit;
      PROCEDURE multIncPixel(CONST x,y:longint; CONST factor:single; CONST increment:T_floatColor);
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
      PROCEDURE copyFromImage(VAR srcImage: T_rawImage);
      //-------------------------------------------------------:TImage interface
      //File interface:---------------------------------------------------------
      PROCEDURE saveToFile(CONST fileName:ansistring);
      PROCEDURE loadFromFile(CONST fileName:ansistring);
      FUNCTION saveJpgWithSizeLimitReturningErrorOrBlank(CONST fileName:ansistring; CONST sizeLimit:SizeInt):ansistring;
      //---------------------------------------------------------:File interface
      //Geometry manipulations:-------------------------------------------------
      PROCEDURE resize(CONST newWidth,newHeight:longint; CONST resizeStyle:T_resizeStyle);
      PROCEDURE flip;
      PROCEDURE flop;
      PROCEDURE rotRight;
      PROCEDURE rotLeft;
      PROCEDURE crop(CONST rx0,rx1,ry0,ry1:double);
      //-------------------------------------------------:Geometry manipulations
      //Statistic accessors:----------------------------------------------------
      FUNCTION histogram:T_compoundHistogram;
      FUNCTION histogram(CONST x,y:longint; CONST smoothingKernel:T_arrayOfDouble):T_compoundHistogram;
      FUNCTION histogramHSV:T_compoundHistogram;
      //----------------------------------------------------:Statistic accessors
      PROCEDURE quantize(CONST numberOfColors:longint);
      PROCEDURE blur(CONST relativeXBlur:double; CONST relativeYBlur:double);
      FUNCTION directionMap(CONST relativeSigma:double):T_rawImage;
      PROCEDURE lagrangeDiffusion(CONST relativeGradSigma,relativeBlurSigma:double);
      PROCEDURE lagrangeDiffusion(VAR dirMap:T_rawImage; CONST relativeBlurSigma:double; CONST changeDirection:boolean=true);
      PROCEDURE radialBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
      PROCEDURE rotationalBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
      PROCEDURE shine;
      PROCEDURE sharpen(CONST relativeSigma,factor:double);
      PROCEDURE prewittEdges;
      PROCEDURE variance(CONST relativeSigma:double);
      PROCEDURE blurWith(CONST relativeBlurMap:T_rawImage);
      PROCEDURE medianFilter(CONST relativeSigma:double);
      PROCEDURE modalFilter(CONST relativeSigma:double);
      PROCEDURE sketch(CONST colorCount:byte; CONST relativeDirMapSigma,density,tolerance:double);
      //PROCEDURE paint(CONST relativeDirMapSigma,density,tolerance,curvature:double);
      PROCEDURE myFilter(CONST thresholdDistParam,param:double);
      PROCEDURE drip(CONST diffusiveness,range:double);
      PROCEDURE encircle(CONST count:longint; CONST opacity,relativeCircleSize:double; CONST containingQueue:P_progressEstimatorQueue);
  end;

F_displayErrorFunction=PROCEDURE(CONST s:ansistring);

VAR compressionQualityPercentage:longint=100;
FUNCTION getFittingRectangle(CONST availableWidth,availableHeight:longint; CONST aspectRatio:double):TRect;
IMPLEMENTATION
FUNCTION getFittingRectangle(CONST availableWidth,availableHeight:longint; CONST aspectRatio:double):TRect;
  begin
    if availableHeight*aspectRatio<availableWidth
    then result:=Rect(0,0,round(availableHeight*aspectRatio),availableHeight)
    else result:=Rect(0,0,availableWidth,round(availableWidth/aspectRatio));
  end;

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
      rest:=black;
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

FUNCTION combinedColor(CONST struc:T_structuredHitColor):T_floatColor;
  begin
    with struc do if antialiasingMask<2
    then result:=rest
    else result:=rest*(0.5/(antialiasingMask and 254));
  end;

FUNCTION T_colChunk.markAlias(CONST globalTol:single):boolean;
  VAR i,j,i2,j2:longint;
      localRefFactor:single;
      localTol:single;
      localError:single;
      tempColor:array[0..CHUNK_BLOCK_SIZE-1,0..CHUNK_BLOCK_SIZE-1] of T_floatColor;

  FUNCTION getErrorAt(CONST i,j:longint):double;
    VAR c:array[-1..1,-1..1] of T_floatColor;
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

{ T_rawImage }

PROCEDURE T_rawImage.setPixel(CONST x, y: longint; CONST value: T_floatColor);
  begin
    datFloat[x+y*xRes]:=value
  end;

FUNCTION T_rawImage.getPixel(CONST x, y: longint): T_floatColor;
  begin
    result:=datFloat[x+y*xRes];
  end;

FUNCTION T_rawImage.getPixel24Bit(CONST x, y: longint): T_24Bit;
  begin
    result:=projectedColor(datFloat[x+y*xRes]);
  end;

CONSTRUCTOR T_rawImage.create(CONST width_, height_: longint);
  begin
    xRes:=width_;
    yRes:=height_;
    getMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
  end;

CONSTRUCTOR T_rawImage.create(CONST fileName: ansistring);
  begin
    create(1,1);
    loadFromFile(fileName);
  end;

CONSTRUCTOR T_rawImage.create(VAR original: T_rawImage);
  begin
    create(1,1);
    copyFromImage(original);
  end;

DESTRUCTOR T_rawImage.destroy;
  begin
    if xRes*yRes>0 then freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
    xRes:=0;
    yRes:=0;
  end;

FUNCTION T_rawImage.width: longint;  begin result:=xRes; end;
FUNCTION T_rawImage.height: longint; begin result:=yRes; end;
FUNCTION T_rawImage.diagonal: double; begin result:=sqrt(xRes*xRes+yRes*yRes); end;

PROCEDURE T_rawImage.copyToImage(CONST srcRect: TRect; VAR destImage: TImage);
  VAR ScanLineImage,                 //image with representation as in T_24BitImage
      tempIntfImage: TLazIntfImage;  //image with representation as in TBitmap
      ImgFormatDescription: TRawImageDescription;
      x,y:longint;
      pc:T_24Bit;
      pix:PByte;
  begin
    ScanLineImage:=TLazIntfImage.create(srcRect.Right-srcRect.Left,srcRect.Bottom-srcRect.top);
    ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(srcRect.Right-srcRect.Left,srcRect.Bottom-srcRect.top);
    ImgFormatDescription.ByteOrder:=riboMSBFirst;
    ScanLineImage.DataDescription:=ImgFormatDescription;
    for y:=0 to srcRect.Bottom-srcRect.top-1 do begin
      pix:=ScanLineImage.GetDataLineStart(y);
      for x:=0 to srcRect.Right-srcRect.Left-1 do begin
        pc:=getPixel24Bit(srcRect.Left+x,srcRect.top+y);
        move(pc,(pix+3*x)^,3);
      end;
    end;
    destImage.Picture.Bitmap.width :=srcRect.Right-srcRect.Left;
    destImage.Picture.Bitmap.height:=srcRect.Bottom-srcRect.top;
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
      tempIntfImage,
      ScanLineImage: TLazIntfImage;
      ImgFormatDescription: TRawImageDescription;
      pc:T_24Bit;
      pix:PByte;

  begin
    resize(srcImage.width,srcImage.height,res_dataResize);

    ScanLineImage:=TLazIntfImage.create(xRes,yRes);
    ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(xRes,yRes);
    ImgFormatDescription.ByteOrder:=riboMSBFirst;
    ScanLineImage.DataDescription:=ImgFormatDescription;
    tempIntfImage:=srcImage.Picture.Bitmap.CreateIntfImage;
    ScanLineImage.CopyPixels(tempIntfImage);
    for y:=0 to yRes-1 do begin
      pix:=ScanLineImage.GetDataLineStart(y);
      for x:=0 to xRes-1 do begin
        move((pix+3*x)^,pc,3);
        setPixel(x,y,pc);
      end;
    end;
    ScanLineImage.free;
    tempIntfImage.free;
  end;

PROCEDURE T_rawImage.multIncPixel(CONST x,y:longint; CONST factor:single; CONST increment:T_floatColor);
  VAR k:longint;
  begin
    k:=x+y*xRes;
    datFloat[k]:=datFloat[k]*factor+increment;
  end;

FUNCTION T_rawImage.chunksInMap: longint;
  VAR xChunks,yChunks:longint;
  begin
    xChunks:=xRes div CHUNK_BLOCK_SIZE; if xChunks*CHUNK_BLOCK_SIZE<xRes then inc(xChunks);
    yChunks:=yRes div CHUNK_BLOCK_SIZE; if yChunks*CHUNK_BLOCK_SIZE<yRes then inc(yChunks);
    result:=xChunks*yChunks;
  end;

PROCEDURE T_rawImage.markChunksAsPending;
  VAR x,y:longint;
  begin
    for y:=height-1 downto 0 do for x:=0 to width-1 do
      if ((x and 63) in [0,63]) or ((y and 63) in [0,63]) or (odd(x) xor odd(y)) and (((x and 63) in [21,42]) or ((y and 63) in [21,42]))
      then pixel[x,y]:=white
      else pixel[x,y]:=black;
  end;

FUNCTION T_rawImage.getPendingList: T_pendingList;
  VAR xChunks,yChunks:longint;
      x,y,cx,cy,i:longint;
      isPending:array of array of boolean;
  begin
    randomize;
    xChunks:=width  div CHUNK_BLOCK_SIZE; if xChunks*CHUNK_BLOCK_SIZE<width  then inc(xChunks);
    yChunks:=height div CHUNK_BLOCK_SIZE; if yChunks*CHUNK_BLOCK_SIZE<height then inc(yChunks);
    setLength(isPending,xChunks);
    for cx:=0 to length(isPending)-1 do begin
      setLength(isPending[cx],yChunks);
      for cy:=0 to length(isPending[cx])-1 do isPending[cx,cy]:=true;
    end;
    //scan:-----------------------------------------------------
    for y:=height-1 downto 0 do begin
      cy:=y div CHUNK_BLOCK_SIZE;
      for x:=0 to width-1 do begin
        cx:=x div CHUNK_BLOCK_SIZE;
        if ((x and 63) in [0,63]) or ((y and 63) in [0,63]) or (odd(x) xor odd(y)) and (((x and 63) in [21,42]) or ((y and 63) in [21,42]))
        then isPending[cx,cy]:=isPending[cx,cy] and (pixel[x,y]=white)
        else isPending[cx,cy]:=isPending[cx,cy] and (pixel[x,y]=black);
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
  CONST floatGrey:array[false..true] of T_floatColor=((0.6,0.6,0.6),(0.4,0.4,0.4));
  VAR i,j:longint;
  begin
    for j:=0 to yRes-1 do for i:=0 to xRes-1 do setPixel(i,j,floatGrey[odd(i shr 5) xor odd(j shr 5)]);
  end;

PROCEDURE T_rawImage.saveToFile(CONST fileName: ansistring);
  PROCEDURE storeDump;
    VAR handle:file of byte;
    begin
      assign(handle,UTF8ToSys(fileName));
      rewrite(handle);
      BlockWrite(handle,xRes,sizeOf(longint));
      BlockWrite(handle,yRes,sizeOf(longint));
      BlockWrite(handle,PByte(datFloat)^,xRes*yRes*sizeOf(T_floatColor));
      close(handle);
    end;

  VAR ext:string;
      storeImg:TImage;
      Jpeg:TFPWriterJPEG;
      img:TLazIntfImage;
  begin
    ext:=uppercase(extractFileExt(fileName));
    if (ext='.JPG') or (ext='.JPEG') or (ext='.PNG') or (ext='.BMP') then begin
      storeImg:=TImage.create(nil);
      storeImg.SetInitialBounds(0,0,xRes,yRes);
      copyToImage(storeImg);
      if ext='.PNG' then storeImg.Picture.PNG.saveToFile(fileName) else
      if ext='.BMP' then storeImg.Picture.Bitmap.saveToFile(fileName)
                    else begin
        Jpeg:=TFPWriterJPEG.create;
        Jpeg.CompressionQuality:=100;
        img:=storeImg.Picture.Bitmap.CreateIntfImage;
        img.saveToFile(UTF8ToSys(fileName),Jpeg);
        img.free;
        Jpeg.free;
      end;
      storeImg.free;
    end else storeDump;
  end;

PROCEDURE T_rawImage.loadFromFile(CONST fileName: ansistring);
  VAR useFilename:ansistring;
  PROCEDURE restoreDump;
    VAR handle:file of byte;
    begin
      freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
      assign(handle,useFilename);
      reset(handle);
      BlockRead(handle,xRes,sizeOf(longint));
      BlockRead(handle,yRes,sizeOf(longint));
      getMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
      BlockRead(handle,PByte(datFloat)^,xRes*yRes*sizeOf(T_floatColor));
      close(handle);
    end;

  VAR ext:string;
      reStoreImg:TImage;
  begin
    if fileExists(fileName) then useFilename:=fileName
    else if FileExistsUTF8(fileName) then useFilename:=UTF8ToSys(fileName)
    else begin
      writeln(stdErr,'Image ',fileName,' cannot be loaded because it does not exist');
      exit;
    end;
    ext:=uppercase(extractFileExt(useFilename));
    if (ext='.JPG') or (ext='.JPEG') or (ext='.PNG') or (ext='.BMP') then begin
      reStoreImg:=TImage.create(nil);
      reStoreImg.SetInitialBounds(0,0,10000,10000);
      if ext='.PNG' then reStoreImg.Picture.PNG   .loadFromFile(SysToUTF8(useFilename)) else
      if ext='.BMP' then reStoreImg.Picture.Bitmap.loadFromFile(SysToUTF8(useFilename))
                    else reStoreImg.Picture.Jpeg  .loadFromFile(SysToUTF8(useFilename));
      reStoreImg.SetBounds(0,0,reStoreImg.Picture.width,reStoreImg.Picture.height);
      copyFromImage(reStoreImg);
      reStoreImg.free;
    end else restoreDump;
  end;

PROCEDURE T_rawImage.copyFromImage(VAR srcImage: T_rawImage);
  VAR size:longint;
  begin
    if datFloat<>nil then freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
    xRes:=srcImage.xRes;
    yRes:=srcImage.yRes;
    size:=xRes*yRes*sizeOf(T_floatColor);
    getMem(datFloat,size);
    move(srcImage.datFloat^,datFloat^,size);
  end;

FUNCTION T_rawImage.saveJpgWithSizeLimitReturningErrorOrBlank(CONST fileName:ansistring; CONST sizeLimit:SizeInt):ansistring;
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
    VAR Jpeg:TFPWriterJPEG;
        img:TLazIntfImage;
    begin
      Jpeg:=TFPWriterJPEG.create;
      Jpeg.CompressionQuality:=quality;
      img:=storeImg.Picture.Bitmap.CreateIntfImage;
      img.saveToFile(UTF8ToSys(fileName),Jpeg);
      img.free;
      Jpeg.free;
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
    if sizeLimit=0 then exit(saveJpgWithSizeLimitReturningErrorOrBlank(fileName,round(1677*sqrt(xRes*yRes))));
    ext:=uppercase(extractFileExt(fileName));
    if ext<>'.JPG' then exit('Saving with size limit is only possible in JPEG format.');
    storeImg:=TImage.create(nil);
    storeImg.SetInitialBounds(0,0,xRes,yRes);
    copyToImage(storeImg);
    for quality:=0 to 100 do sizes[quality]:=-1;
    lastSavedQuality:=-1;
    quality:=100;
    while (quality>4  ) and (getSizeAt(quality  )> sizeLimit) do dec(quality, 8);
    while (quality<100) and (getSizeAt(quality  )< sizeLimit) do inc(quality, 4);
    while (quality>0  ) and (getSizeAt(quality  )> sizeLimit) do dec(quality, 2);
    while (quality<100) and (getSizeAt(quality+1)<=sizeLimit) do inc(quality, 1);
    if lastSavedQuality<>quality then saveAtQuality(quality);
    storeImg.free;
    result:='';
  end;

PROCEDURE T_rawImage.resize(CONST newWidth, newHeight: longint;
  CONST resizeStyle: T_resizeStyle);
  VAR srcRect,destRect:TRect;
      dx,dy:longint;

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
      destImage.Canvas.StretchDraw(destRect,srcImage.Picture.Graphic);
      srcImage.free;
      copyFromImage(destImage);
      destImage.free;
    end;

  begin
    case resizeStyle of
      res_exact,res_dataResize: begin
        srcRect:=Rect(0,0,xRes,yRes);
        destRect:=Rect(0,0,newWidth,newHeight);
        if (newWidth=xRes) and (newHeight=yRes) then exit;
      end;
      res_fit: begin
        srcRect:=Rect(0,0,xRes,yRes);
        destRect:=getFittingRectangle(newWidth,newHeight,xRes/yRes);
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
    if resizeStyle=res_dataResize then begin
      freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
      getMem(datFloat,newWidth*newHeight*sizeOf(T_floatColor));
      xRes:=newWidth;
      yRes:=newHeight;
    end else resizeViaTImage;
  end;

PROCEDURE T_rawImage.flip;
  VAR x,y,y1:longint;
      tempCol  :T_floatColor;
  begin
    for y:=0 to yRes shr 1 do begin
      y1:=yRes-1-y;
      if y1>y then for x:=0 to xRes-1 do begin tempCol  :=pixel     [x,y]; pixel     [x,y]:=pixel     [x,y1]; pixel     [x,y1]:=tempCol  ; end;
    end;
  end;

PROCEDURE T_rawImage.flop;
  VAR x,y,x1:longint;
      tempCol  :T_floatColor;
  begin
    for x:=0 to xRes shr 1 do begin
      x1:=xRes-1-x;
      if x1>x then for y:=0 to yRes-1 do begin tempCol  :=pixel     [x,y]; pixel     [x,y]:=pixel     [x1,y]; pixel     [x1,y]:=tempCol  ; end;
    end;
  end;

PROCEDURE T_rawImage.rotRight;
  VAR x,y:longint;
      temp:T_rawImage;
  begin
    temp.create(yRes,xRes);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do temp[yRes-1-y,x]:=pixel[x,y];
    copyFromImage(temp);
    temp.destroy;
  end;

PROCEDURE T_rawImage.rotLeft;
  VAR x,y:longint;
      temp:T_rawImage;
  begin
    temp.create(yRes,xRes);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do temp[y,xRes-1-x]:=pixel[x,y];
    copyFromImage(temp);
    temp.destroy;
  end;

PROCEDURE T_rawImage.crop(CONST rx0,rx1,ry0,ry1:double);
  VAR newData:P_floatColor;
      newXRes,newYRes,x,y:longint;
      x0, x1, y0, y1: longint;
  begin
    x0:=round(rx0*xRes);
    x1:=round(rx1*xRes);
    y0:=round(ry0*yRes);
    y1:=round(ry1*yRes);
    if (x1<=x0) or (y1<=y0) then exit;
    newXRes:=x1-x0;
    newYRes:=y1-y0;
    getMem(newData,newXRes*newYRes*sizeOf(T_floatColor));
    for y:=y0 to y1-1 do for x:=x0 to x1-1 do
    if (x>=0) and (x<xRes) and (y>=0) and (y<yRes)
    then newData[(x-x0)+(y-y0)*newXRes]:=pixel[x,y]
    else newData[(x-x0)+(y-y0)*newXRes]:=black;
    freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
    datFloat:=newData;
    xRes:=newXRes;
    yRes:=newYRes;
  end;

FUNCTION T_rawImage.histogram: T_compoundHistogram;
  VAR i:longint;
  begin
    result.create;
    for i:=0 to xRes*yRes-1 do result.putSample(datFloat[i]);
  end;

FUNCTION T_rawImage.histogram(CONST x, y: longint;
  CONST smoothingKernel: T_arrayOfDouble): T_compoundHistogram;
  VAR dx,dy:longint;
      wy:double;
  begin
    result.create;
    for dy:=max(-y,1-length(smoothingKernel)) to min(yRes-y,length(smoothingKernel))-1 do begin
      wy:=smoothingKernel[abs(dy)];
      for dx:=max(-x,1-length(smoothingKernel)) to min(xRes-x,length(smoothingKernel))-1 do begin
        result.putSampleSmooth(pixel[x+dx,y+dy],smoothingKernel[abs(dx)]*wy);
      end;
    end;
  end;

FUNCTION T_rawImage.histogramHSV: T_compoundHistogram;
  VAR i:longint;
  begin
    result.create;
    for i:=0 to xRes*yRes-1 do result.putSample(toHSV(datFloat[i]));
  end;

FUNCTION getSmoothingKernel(CONST sigma:double):T_arrayOfDouble;
  VAR radius,i:longint;
      sum:double=-1;
      factor:double;
  begin
    if sigma<=1E-3 then begin
      setLength(result,1);
      result[0]:=1;
      exit(result);
    end;
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

PROCEDURE T_rawImage.quantize(CONST numberOfColors:longint);
  VAR i:longint;
      tree:T_colorTree;
  begin
    tree.create;
    for i:=0 to xRes*yRes-1 do tree.addSample(datFloat[i]);
    tree.finishSampling(numberOfColors);
    for i:=0 to xRes*yRes-1 do datFloat[i]:=tree.getQuantizedColor(datFloat[i]);
    tree.destroy;
  end;

PROCEDURE T_rawImage.blur(CONST relativeXBlur: double; CONST relativeYBlur: double);
  VAR kernel:T_arrayOfDouble;
      temp:T_rawImage;
      ptmp:P_floatColor;
      x,y,z:longint;
      sum:T_floatColor;
      weight:double;
  begin
    temp.create(xRes,yRes);
    ptmp:=temp.datFloat;
    kernel:=getSmoothingKernel(relativeXBlur/100*diagonal);
    //blur in x-direction:-----------------------------------------------
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
      sum:=black; weight:=0;
      for z:=max(-x,1-length(kernel)) to min(xRes-x,length(kernel))-1 do begin
        sum   :=sum   +kernel[abs(z)]*datFloat[x+z+y*xRes];
        weight:=weight+kernel[abs(z)];
      end;
      if (x<length(kernel)) or (x>xRes-1-length(kernel))
      then ptmp[x+y*xRes]:=sum*(1/weight)
      else ptmp[x+y*xRes]:=sum;
    end;
    //-------------------------------------------------:blur in x-direction
    setLength(kernel,0);
    kernel:=getSmoothingKernel(relativeYBlur/100*diagonal);
    //blur in y-direction:---------------------------------------------------
    for x:=0 to xRes-1 do for y:=0 to yRes-1 do begin
      sum:=black; weight:=0;
      for z:=max(-y,1-length(kernel)) to min(yRes-y,length(kernel))-1 do begin
        sum   :=sum   +kernel[abs(z)]*ptmp[x+(z+y)*xRes];
        weight:=weight+kernel[abs(z)]
      end;
      if (y<length(kernel)) or (y>yRes-1-length(kernel))
      then datFloat[x+y*xRes]:=sum*(1/weight)
      else datFloat[x+y*xRes]:=sum;
    end;
    //-----------------------------------------------------:blur in y-direction
    temp.destroy;
    setLength(kernel,0);
  end;

FUNCTION T_rawImage.directionMap(CONST relativeSigma:double):T_rawImage;
  VAR x,y:longint;

  FUNCTION normalAt(x,y:longint):T_floatColor;
    VAR dx,dy,channel:longint;
        n:array[-1..1,-1..1] of T_floatColor;
        w :array [0..1] of double;
    begin
      //fill stencil:--------------------------------------------//
      for dy:=-1 to 1 do for dx:=-1 to 1 do                      //
      if (y+dy>=0) and (y+dy<yRes) and (x+dx>=0) and (x+dx<xRes) //
        then n[dx,dy]:=getPixel(x+dx,(y+dy))                     //
        else n[dx,dy]:=getPixel(x   , y    );                    //
      //----------------------------------------------:fill stencil
      result[0]:=0;
      result[1]:=0;
      result[2]:=0;
      for channel:=0 to 2 do begin
        w[0]:=n[ 1,-1][channel]+3*n[ 1,0][channel]+n[ 1,1][channel]
             -n[-1,-1][channel]-3*n[-1,0][channel]-n[-1,1][channel];
        w[1]:=n[-1, 1][channel]+3*n[0, 1][channel]+n[1, 1][channel]
             -n[-1,-1][channel]-3*n[0,-1][channel]-n[1,-1][channel];
        result[2]:=1/sqrt(1E-6+w[0]*w[0]+w[1]*w[1]);
        result[0]:=result[0]+result[2]*(w[0]*w[0]-w[1]*w[1]);
        result[1]:=result[1]+result[2]*2*w[0]*w[1];
      end;
      result[2]:=0;
    end;

  FUNCTION normedDirection(CONST d:T_floatColor):T_floatColor;
    begin
      result[2]:=arctan2(d[1],d[0])/2;
      result[0]:=-sin(result[2]);
      result[1]:= cos(result[2]);
      result[2]:=0;
    end;

  begin
    result.create(xRes,yRes);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do result[x,y]:=normalAt(x,y);
    result.blur(relativeSigma,relativeSigma);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do result[x,y]:=normedDirection(result[x,y]);
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
      pos,dir:T_floatColor;
      colSum:T_floatColor;
      wgtSum:double;

  PROCEDURE step; inline;
    VAR d:T_floatColor;
    begin
      if changeDirection then begin d:=dirMap[ix,iy]; if d[0]*dir[0]+d[1]*dir[1] > 0 then dir:=d else dir:=-1*d; end;
      pos:=pos+dir;
      ix:=round(pos[0]);
      iy:=round(pos[1]);
    end;

  begin
    kernel:=getSmoothingKernel(relativeBlurSigma/100*diagonal);
    output.create(xRes,yRes);
    for y:=0 to yRes-1 do
    for x:=0 to xRes-1 do begin
      colSum:=getPixel(x,y)*kernel[0];
      wgtSum:=              kernel[0];
      for k:=0 to 1 do begin
        ix:=x;
        iy:=y;
        pos[0]:=x;
        pos[1]:=y;
        dir:=(k*2-1)*dirMap[x,y];
        step;
        for i:=1 to length(kernel)-1 do if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
          colSum:=colSum+datFloat[ix+iy*xRes]*kernel[i];
          wgtSum:=wgtSum+                     kernel[i];
          step;
        end else break;
      end;
      output[x,y]:=colSum*(1/wgtSum);
    end;
    copyFromImage(output);
    output.destroy;
  end;

FUNCTION cartNormalCol(CONST c:T_floatColor):T_floatColor;
  begin
    result:=c*(1/sqrt(1E-6+c[0]*c[0]+c[1]*c[1]+c[2]*c[2]));
  end;

PROCEDURE T_rawImage.radialBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
  VAR dirMap:T_rawImage;
      x,y:longint;
  begin
    dirMap.create(xRes,yRes);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do
      dirMap[x,y]:=cartNormalCol(newColor(x/xRes-0.5-relativeCenterX,
                                          y/yRes-0.5-relativeCenterY,
                                          0));
    lagrangeDiffusion(dirMap,relativeBlurSigma,false);
    dirMap.destroy;
  end;

PROCEDURE T_rawImage.rotationalBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
  VAR dirMap:T_rawImage;
      x,y:longint;
  begin
    dirMap.create(xRes,yRes);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do
      dirMap[x,y]:=cartNormalCol(newColor(y/yRes-0.5-relativeCenterY,
                                         -x/xRes+0.5+relativeCenterX,
                                          0));
    lagrangeDiffusion(dirMap,relativeBlurSigma,false);
    dirMap.destroy;
  end;

PROCEDURE T_rawImage.shine;
  VAR temp:T_rawImage;
      pt:P_floatColor;
      co,ct:T_floatColor;
      fak:double;
      x,y,ix,iy,step:longint;
      anyOverbright:boolean;
  begin
    temp.create(xRes,yRes);
    pt:=temp.datFloat;
    step:=1;
    repeat
      anyOverbright:=false;
      for x:=0 to xRes*yRes-1 do begin
        co:=datFloat[x];
        ct:=co;
        fak:=max(1,(co[0]+co[1]+co[2])*(1/3));
        co:=co*(1/fak);
        datFloat[x]:=co;
        pt[x]:=ct-co;
        anyOverbright:=anyOverbright or (fak>1.1);
      end;
      for y:=0 to yRes-1 do
      for x:=0 to xRes-1 do begin
        co:=pt[x+y*xRes];
        if co<>black then begin
          co:=co*(1/(2+4*step));
          for iy:=max(0,y-step) to min(yRes-1,y+step) do datFloat[x+iy*xRes]:=datFloat[x+iy*xRes]+co;
          for ix:=max(0,x-step) to min(xRes-1,x+step) do datFloat[ix+y*xRes]:=datFloat[ix+y*xRes]+co;
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
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do pixel[x,y]:= blurred[x,y]+(pixel[x,y]-blurred[x,y])*(1+factor);
    blurred.destroy;
  end;

PROCEDURE T_rawImage.prewittEdges;
  VAR x,y,i:longint;

  begin
    //first change to greyscale:
    for i:=0 to xRes*yRes-1 do datFloat[i]:=subjectiveGrey(datFloat[i]);
    //x-convolution to green channel
    for y:=0 to yRes-1 do begin
      datFloat[y*xRes][1]:=0;
      for x:=1 to xRes-2 do datFloat[x+y*xRes][1]:=datFloat[x+y*xRes+1][0]
                                                  -datFloat[x+y*xRes-1][0];
      datFloat[xRes-1+y*xRes][1]:=0;
    end;
    //Re-convolition to blue channel
    for x:=0 to xRes-1 do datFloat[x][2]:=(datFloat[x][1]+datFloat[x+xRes][1])*0.5;
    for y:=1 to yRes-2 do for x:=0 to xRes-1 do datFloat[x+y*xRes][2]:=datFloat[x+y*xRes-xRes][1]*0.2
                                                                      +datFloat[x+y*xRes     ][1]*0.6
                                                                      +datFloat[x+y*xRes+xRes][1]*0.2;
    for i:=xRes*yRes-xRes to xRes*yRes-1 do datFloat[i][2]:=(datFloat[i-xRes][1]+datFloat[i][1])*0.5;
    //y-convolution to green channel
                          for x:=0 to xRes-1 do datFloat[x       ][1]:=0;
    for y:=1 to yRes-2 do for x:=0 to xRes-1 do datFloat[x+y*xRes][1]:=datFloat[x+y*xRes+xRes][0]-datFloat[x+y*xRes-xRes][0];
        for i:=xRes*yRes-xRes to xRes*yRes-1 do datFloat[i       ][1]:=0;
    //Re-convolution to red channel
    for y:=0 to yRes-1 do begin
      datFloat[y*xRes][0]:=(datFloat[y*xRes][1]+datFloat[y*xRes+1][1])*0.5;
      for x:=1 to xRes-2 do datFloat[x+y*xRes][0]:=datFloat[x+y*xRes-1][1]*0.2
                                                  +datFloat[x+y*xRes  ][1]*0.6
                                                  +datFloat[x+y*xRes+1][1]*0.2;
      i:=xRes-1+y*xRes;
      datFloat[i][0]:=(datFloat[i-1][1]+datFloat[i][1])*0.5;
    end;
    for i:=0 to xRes*yRes-1 do datFloat[i]:=sqrt(sqr(datFloat[i][0])+sqr(datFloat[i][2]))*white;
  end;

PROCEDURE T_rawImage.variance(CONST relativeSigma:double);
  FUNCTION pot2(CONST c:T_floatColor):T_floatColor;
    begin
      result[0]:=sqr(c[0]);
      result[1]:=sqr(c[1]);
      result[2]:=sqr(c[2]);
    end;

  VAR m2:T_rawImage;
      i:longint;
  begin
    m2.create(xRes,yRes);
    for i:=0 to xRes*yRes-1 do m2.datFloat[i]:=pot2(datFloat[i]);
       blur(relativeSigma,relativeSigma);
    m2.blur(relativeSigma,relativeSigma);
    for i:=0 to xRes*yRes-1 do datFloat[i]:=m2.datFloat[i]-pot2(datFloat[i]);
    m2.destroy;
  end;

PROCEDURE T_rawImage.blurWith(CONST relativeBlurMap:T_rawImage);
  VAR kernels:array of T_arrayOfDouble;
      kernel:T_arrayOfDouble;
      temp:T_rawImage;
      ptmp:P_floatColor;
      x,y,z:longint;
      sum:T_floatColor;
      weight:double;

  FUNCTION getKernel(CONST relativeSigma:single):T_arrayOfDouble;
    VAR index:longint;
        i:longint;
    begin
      index:=round(255*relativeSigma);
      if index<0 then index:=0;
      i:=length(kernels);
      while i<=index do begin
        setLength(kernels,i+1);
        kernels[i]:=C_EMPTY_DOUBLE_ARRAY;
        inc(i);
      end;
      if length(kernels[index])=0 then begin
        if index>0 then kernels[index]:=getSmoothingKernel(index/25500*diagonal)
                   else begin
                     setLength(kernels[index],1);
                     kernels[index][0]:=1;
                   end;
      end;
      result:=kernels[index];
    end;

  begin
    setLength(kernels,0);
    temp.create(xRes,yRes);
    ptmp:=temp.datFloat;
    //blur in x-direction:-----------------------------------------------
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
      kernel:=getKernel(relativeBlurMap[x,y][0]);
                                                        sum:=    kernel[ 0]*datFloat[x+  y*xRes]; weight:=       kernel[ 0];
      for z:=max(-x,1-length(kernel)) to -1    do begin sum:=sum+kernel[-z]*datFloat[x+z+y*xRes]; weight:=weight+kernel[-z]; end;
      for z:=1 to min(xRes-x,length(kernel))-1 do begin sum:=sum+kernel[ z]*datFloat[x+z+y*xRes]; weight:=weight+kernel[ z]; end;
      ptmp[x+y*xRes]:=sum*(1/weight);
    end;
    //-------------------------------------------------:blur in x-direction
    for x:=0 to length(kernels)-1 do setLength(kernels[x],0);
    setLength(kernels,0);
    //blur in y-direction:---------------------------------------------------
    for x:=0 to xRes-1 do for y:=0 to yRes-1 do begin
      kernel:=getKernel(relativeBlurMap[x,y][1]);
                                                        sum:=    kernel[ 0]*ptmp[x+   y *xRes]; weight:=       kernel[ 0];
      for z:=max(-y,1-length(kernel)) to -1    do begin sum:=sum+kernel[-z]*ptmp[x+(z+y)*xRes]; weight:=weight+kernel[-z]; end;
      for z:=1 to min(yRes-y,length(kernel))-1 do begin sum:=sum+kernel[ z]*ptmp[x+(z+y)*xRes]; weight:=weight+kernel[ z]; end;
      datFloat[x+y*xRes]:=sum*(1/weight);
    end;
    //-----------------------------------------------------:blur in y-direction
    temp.destroy;
    for x:=0 to length(kernels)-1 do setLength(kernels[x],0);
    setLength(kernels,0);
  end;

PROCEDURE T_rawImage.medianFilter(CONST relativeSigma:double);
  VAR output:T_rawImage;
      x,y:longint;
      kernel:T_arrayOfDouble;
      hist:T_compoundHistogram;
  begin
    output.create(xRes,yRes);
    kernel:=getSmoothingKernel(relativeSigma/100*diagonal);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
      hist:=histogram(x,y,kernel);
      output[x,y]:=newColor(hist.R.median,hist.G.median,hist.B.median);
    end;
    copyFromImage(output);
    output.destroy;
  end;

PROCEDURE T_rawImage.modalFilter(CONST relativeSigma:double);
  VAR output:T_rawImage;
      x,y:longint;
      kernel:T_arrayOfDouble;
      hist:T_compoundHistogram;
  begin
    output.create(xRes,yRes);
    kernel:=getSmoothingKernel(relativeSigma/100*diagonal);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
      hist:=histogram(x,y,kernel);
      output[x,y]:=newColor(hist.R.mode,hist.G.mode,hist.B.mode);
    end;
    copyFromImage(output);
    output.destroy;
  end;

PROCEDURE T_rawImage.sketch(CONST colorCount:byte; CONST relativeDirMapSigma,density,tolerance:double);
  PROCEDURE niceLine(CONST x0,y0,x1,y1:double; CONST color:T_floatColor; CONST alpha:double);
    VAR ix,iy:longint;
        slope:double;
    PROCEDURE xStep; inline;
      VAR y,f,a:double;
          k:longint;
      begin
        y:=y0+slope*(ix-x0);
        iy:=trunc(y); f:=frac(y);
        k:=ix+iy*xRes;
        if (iy>=0) and (iy<yRes-1) then begin
          a:=(1-f)*alpha;
          datFloat[k]:=datFloat[k]*(1-a)+color*a;
        end;
        if (iy>=-1) and (iy<yRes-2) then begin
          inc(k,xRes);
          a:=f*alpha;
          datFloat[k]:=datFloat[k]*(1-a)+color*a;
        end;
      end;

    PROCEDURE yStep; inline;
      VAR x,f,a:double;
          k:longint;
      begin
        x:=x0+slope*(iy-y0);
        ix:=trunc(x); f:=frac(x);
        k:=ix+iy*xRes;
        if (ix>=0) and (ix<xRes-1) then begin
          a:=(1-f)*alpha;
          datFloat[k]:=datFloat[k]*(1-a)+color*a;
        end;
        if (ix>=-1) and (ix<xRes-2) then begin
          inc(k);
          a:=f*alpha;
          datFloat[k]:=datFloat[k]*(1-a)+color*a;
        end;
      end;

    begin
      if abs(x1-x0)>abs(y1-y0) then begin
        slope:=(y1-y0)/(x1-x0);
        if x1>=x0
        then for ix:=max(round(x0),0) to min(xRes-1,round(x1)) do xStep
        else for ix:=max(round(x1),0) to min(xRes-1,round(x0)) do xStep;
      end else if abs(y1-y0)>0 then begin
        slope:=(x1-x0)/(y1-y0);
        if y1>=y0
        then for iy:=max(round(y0),0) to min(yRes-1,round(y1)) do yStep
        else for iy:=max(round(y1),0) to min(yRes-1,round(y0)) do yStep;
      end else begin
        ix:=round((x0+x1)/2);
        iy:=round((y0+y1)/2);
        if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then
        datFloat[ix+iy*xRes]:=
        datFloat[ix+iy*xRes]*(1-alpha)+color*alpha;
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
      lineColor:T_floatColor;
      alpha:single;
      dir:T_floatColor;

  FUNCTION isTolerable(CONST fx,fy:double):boolean; inline;
    VAR ix,iy:longint;
    begin
      ix:=round(fx); if (ix<0) or (ix>=xRes) then exit(false);
      iy:=round(fy); if (iy<0) or (iy>=yRes) then exit(false);
      result:=colDiff(temp[ix,iy],lineColor)<=tolerance;
    end;

  begin
    grad:=directionMap(relativeDirMapSigma);
    temp.create(self);
    temp.quantize(colorCount);
    for x:=0 to xRes*yRes-1 do datFloat[x]:=white;
    alpha:=0.9;
    if density>1 then alpha:=exp(density*ln(0.9));
    for l:=0 to 12 do for y:=0 to yRes-1 do for x:=0 to xRes-1 do if (lev(x,y)=l) and (random<density) then begin
      lineColor:=temp[x,y]+newColor(random-0.5,random-0.5,random-0.5)*0.05;
      dir:=grad[x,y];
      for k:=0 to 1 do begin
        i:=0;
        imax:=round(random*diagonal*0.05);
        while (i<imax) and isTolerable(x+i*dir[0],y+i*dir[1]) do inc(i);
        lineX[k]:=x+i*dir[0];
        lineY[k]:=y+i*dir[1];
        dir:=(-1)*dir;
      end;
      niceLine(lineX[0],lineY[0],lineX[1],lineY[1],lineColor,(1-alpha));
    end;
    temp.destroy;
    grad.destroy;
  end;

  {PROCEDURE T_24BitImage.bleed(maxFactor,separationValue,separationSharpness:single; style:byte);
  VAR temp1,temp2:T_24BitImage;
      pt0,pt1,pt2:P_24Bit;
      x,y:longint;

  FUNCTION color(c:T_Vec3):T_24Bit;
    VAR ww:single;
    begin
      ww:=(c[0]+c[1]+c[2])/3;                                            //brighness measure in range [0,1]
      if style>=2 then ww:=(sqr(c[0]-ww)+sqr(c[1]-ww)+sqr(c[2]-ww))*3/2; //saturation measure in range [0,1]
      ww:=0.5+separationSharpness*(ww-separationValue);
      if odd(style) then ww:=1-ww;

      if ww<0.5 then begin
        ww:=ww*2;
        result:=pt0[x+y*xRes]*(1-ww)+pt1[x+y*xRes]*(ww);
      end else begin
        ww:=ww*2-1;
        result:=pt1[x+y*xRes]*(1-ww)+pt2[x+y*xRes]*(ww);
      end;
    end;

  begin
    ProgressReporter('half blur');
    temp1.create(xRes,yRes);
    temp1.CopyFrom(self);
    temp1.blur(maxFactor/2);
    ProgressReporter('full blur');
    temp2.create(xRes,yRes);
    temp2.CopyFrom(self);
    temp2.blur(maxFactor);
    pt0:=P_24Bit(      pixelBuffer);
    pt1:=P_24Bit(temp1.pixelBuffer);
    pt2:=P_24Bit(temp2.pixelBuffer);
    for y:=0 to yRes-1 do begin
      ProgressReporter('finishing '+formatFloat('###.##',100*y/yRes)+'%');
      for x:=0 to xRes-1 do pt0[x+y*xRes]:=color(pt0[x+y*xRes]);
    end;
    temp1.destroy;
    temp2.destroy;
  end;}

  {PROCEDURE nlmFilter(VAR inOut:T_FloatMap; scanRadius:longint; sigma:single);
    VAR pOut :P_floatColor;
        temp :T_FloatMap;
        pIn  :P_floatColor;
        xRes,yRes:longint;
        expLUT:Pdouble;
        abortThreshold:double;

    PROCEDURE initLUT;
      VAR i:longint;
          v:double;
      begin
        abortThreshold:=0;
        getMem(expLUT,10000*sizeOf(double));
        for i:=0 to 9999 do begin
          v:=exp(-i*0.5/sigma);
          expLUT[i]:=v;
          abortThreshold:=abortThreshold+v;
        end;
        abortThreshold:=abortThreshold/10000;
      end;

    PROCEDURE doneLUT;
      begin
        freeMem(expLUT,10000*sizeOf(double));
      end;

    FUNCTION patchDistF(x0,y0,x1,y1:longint):double; inline;
      CONST PATCH_KERNEL:array[-3..3,-3..3] of single=
        ((0.13533528323661269,0.23587708298570001,0.32919298780790558,0.36787944117144232,0.32919298780790558,0.23587708298570001,0.13533528323661269),
         (0.23587708298570001,0.41111229050718744,0.5737534207374328 ,0.64118038842995458,0.5737534207374328 ,0.41111229050718744,0.23587708298570001),
         (0.32919298780790558,0.5737534207374328 ,0.80073740291680804,0.89483931681436977,0.80073740291680804,0.5737534207374328 ,0.32919298780790558),
         (0.36787944117144232,0.64118038842995458,0.89483931681436977,0                  ,0.89483931681436977,0.64118038842995458,0.36787944117144232),
         (0.32919298780790558,0.5737534207374328 ,0.80073740291680804,0.89483931681436977,0.80073740291680804,0.5737534207374328 ,0.32919298780790558),
         (0.23587708298570001,0.41111229050718744,0.5737534207374328 ,0.64118038842995458,0.5737534207374328 ,0.41111229050718744,0.23587708298570001),
         (0.13533528323661269,0.23587708298570001,0.32919298780790558,0.36787944117144232,0.32919298780790558,0.23587708298570001,0.13533528323661269));
      VAR dx,dy:longint;
          c0,c1:T_floatColor;
          i:longint;
      begin
        result:=0;//0.02*(sqr(x0-x1)+sqr(y0-y1));
        for dy:=max(-3,max(-y0,-y1)) to min(3,yRes-1-max(y0,y1)) do
        for dx:=max(-3,max(-x0,-x1)) to min(3,xRes-1-max(x0,x1)) do
        if (dx<>0) or (dy<>0) then begin
          c0:=pIn[x0+dx+(y0+dy)*xRes];
          c1:=pIn[x1+dx+(y1+dy)*xRes];
          result:=result+(sqr(c0[0]-c1[0])+sqr(c0[1]-c1[1])+sqr(c0[2]-c1[2]))*PATCH_KERNEL[dy,dx];
        end;
        //result>=0; result<=69.3447732736614 if colors are in normal colorspace;
        //14.4206975203973=1000/68.3447732736614
        i:=round(result/63.08629411010848E-3);
        //i:=round(result*(1000/(3*25)));
        if i<0 then i:=0 else if i>9999 then i:=9999;
        result:=expLUT[i];
      end;

    FUNCTION filteredColorAtF(x,y:longint):T_floatColor;
      VAR w,wtot,wMax:double;
          r,g,b:double;
          dx,dy:longint;
      begin
        wtot:=0;
        wMax:=0;
        r:=0;
        g:=0;
        b:=0;
        for dy:=max(-scanRadius,-y) to min(scanRadius,yRes-1-y) do
        for dx:=max(-scanRadius,-x) to min(scanRadius,xRes-1-x) do
        if (dx<1-scanRadius) or (dx>scanRadius-1) or (dy<1-scanRadius) or (dy>scanRadius-1) then begin
          w:=patchDistF(x,y,x+dx,y+dy);
          if w>wMax then wMax:=w;
          result:=pIn[x+dx+(y+dy)*xRes];
          r   :=r   +result[0]*w;
          g   :=g   +result[1]*w;
          b   :=b   +result[2]*w;
          wtot:=wtot+     w;
        end;
        result:=pIn[x+y*xRes];
        r   :=r   +result[0]*wMax;
        g   :=g   +result[1]*wMax;
        b   :=b   +result[2]*wMax;
        wtot:=wtot+          wMax;
        if wtot<1E-5 then result:=pIn[x+y*xRes]
        else begin
          wtot:=1/(wtot);
          result[0]:=r*wtot;
          result[1]:=g*wtot;
          result[2]:=b*wtot;
        end;
      end;

    VAR x,y:longint;
    begin
      pOut:=inOut.rawData;
      xRes:=inOut.width;
      yRes:=inOut.height;
      initLUT;
      temp.createCopy(inOut);
      pIn:=temp.rawData;
      for y:=0 to yRes-1 do for x:=0 to xRes-1 do
        pOut[x+y*xRes]:=filteredColorAtF(x,y);
      temp.destroy;
      doneLUT;
    end;}

PROCEDURE T_rawImage.myFilter(CONST thresholdDistParam,param:double);
  FUNCTION combine(CONST m1,m2,m3:T_floatColor):T_floatColor;
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
        i:longint;
    begin
      for i:=0 to 2 do begin
        sigma:=m2[i]-m1[i]*m1[i];
        if sigma<1E-8 then result[i]:=m1[i]
        else begin
          sigma:=sqrt(sigma);
          weight:=param*sigma*arctan((m3[i]-m1[i]*m1[i]*m1[i])/(sigma*sigma*sigma)-3*m1[i]/sigma);
          result[i]:=m1[i]-weight;
        end;
      end;
    end;

  FUNCTION pot2(CONST c:T_floatColor):T_floatColor;
    begin
      result[0]:=sqr(c[0]);
      result[1]:=sqr(c[1]);
      result[2]:=sqr(c[2]);
    end;

  FUNCTION pot3(CONST c:T_floatColor):T_floatColor;
    begin
      result[0]:=sqr(c[0])*c[0];
      result[1]:=sqr(c[1])*c[1];
      result[2]:=sqr(c[2])*c[2];
    end;

  VAR m2,m3:T_rawImage;
      i:longint;
  begin
    m2.create(xRes,yRes);
    for i:=0 to xRes*yRes-1 do m2.datFloat[i]:=pot2(datFloat[i]);
    m3.create(xRes,yRes);
    for i:=0 to xRes*yRes-1 do m3.datFloat[i]:=pot3(datFloat[i]);
    blur(thresholdDistParam,thresholdDistParam);
    m2.blur(thresholdDistParam,thresholdDistParam);
    m3.blur(thresholdDistParam,thresholdDistParam);
    for i:=0 to xRes*yRes-1 do datFloat[i]:=combine(datFloat[i],m2.datFloat[i],m3.datFloat[i]);
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
      for i:=0 to xRes*yRes-1 do datFloat[i]:=datFloat[i]+dt*delta.datFloat[i];
    end;

  PROCEDURE computeDelta;
    VAR x,y:longint;
        v:double;
        flux:T_floatColor;
    begin
      for x:=0 to xRes*yRes-1 do delta.datFloat[x]:=black;
      for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
        v:=toHSV(pixel[x,y])[1];
        if v>1 then v:=1 else if v<0 then v:=0;
        flux:=v*pixel[x,y];
                         delta[x,y  ]:=delta[x,y  ]-flux;
        if y<yRes-1 then delta[x,y+1]:=delta[x,y+1]+flux;
      end;
      if diffusiveness>0 then begin;
        for y:=0 to yRes-1 do for x:=0 to xRes-2 do begin
          flux:=(pixel[x,y]-pixel[x+1,y])*diffusiveness;
          delta[x  ,y]:=delta[x  ,y]-flux;
          delta[x+1,y]:=delta[x+1,y]+flux;
        end;
        for y:=0 to yRes-2 do for x:=0 to xRes-1 do begin
          flux:=(pixel[x,y]-pixel[x,y+1])*diffusiveness;
          delta[x,y  ]:=delta[x,y  ]-flux;
          delta[x,y+1]:=delta[x,y+1]+flux;
        end;
      end;
    end;

  VAR i:longint;
  begin
    stepCount:=round(range*diagonal/dt);
    delta.create(xRes,yRes);
    for i:=0 to stepCount-1 do begin
      computeDelta;
      applyDelta;
    end;
    delta.destroy;
  end;

PROCEDURE T_rawImage.encircle(CONST count:longint; CONST opacity,relativeCircleSize:double; CONST containingQueue:P_progressEstimatorQueue);
  TYPE T_circle=record
         cx,cy,radius,diff:double;
         color:T_floatColor;
       end;

  FUNCTION randomCircle(CONST radius:double):T_circle;
    begin
      result.cx:=radius+random*(xRes-2*radius);
      result.cy:=radius+random*(yRes-2*radius);
      result.radius:=radius;
      result.diff:=0;
    end;

  FUNCTION avgColor(VAR Source:T_rawImage; CONST circle:T_circle):T_floatColor;
    VAR sampleCount:longint=0;
        sqrRad:double;
        x,y:longint;
    begin
      sqrRad:=sqr(circle.radius);
      result:=black;
      with circle do
      for y:=max(0,round(cy-radius)) to min(yRes-1,round(cy+radius)) do
      for x:=max(0,round(cx-radius)) to min(xRes-1,round(cx+radius)) do
      if sqr(x-cx)+sqr(y-cy)<=sqrRad then
      begin
        result:=result+Source[x,y];
        inc(sampleCount);
      end;
      if sampleCount>0 then result:=result*(1/sampleCount);
    end;

  VAR copy:T_rawImage;
      i,j:longint;
      newCircle,toDraw: T_circle;

  FUNCTION globalAvgDiff:double;
    VAR i:longint;
    begin
      for i:=0 to xRes*yRes-1 do result:=result+colDiff(copy.datFloat[i],datFloat[i]);
      result:=result/(xres*yres);
    end;

  PROCEDURE drawCircle(CONST circle:T_circle);
    VAR sqrRad:double;
        x,y,k:longint;
        r:double;
    begin
      sqrRad:=sqr(circle.radius+1);
      with circle do
      for y:=max(0,floor(cy-radius)) to min(yRes-1,ceil(cy+radius)) do
      for x:=max(0,floor(cx-radius)) to min(xRes-1,ceil(cx+radius)) do begin
        r:=sqr(x-cx)+sqr(y-cy);
        if r<=sqrRad then
        begin
          k:=x+y*xRes;
          r:=sqrt(r);
          if r<radius-0.5 then r:=opacity
          else if r>radius+0.5 then r:=0
          else r:=(radius+0.5-r)*opacity;
          if r>0 then datFloat[k]:=datFloat[k]*(1-r)+r*color;
        end;

      end;
    end;

  FUNCTION bestCircle(CONST radius:double):T_circle;
    VAR x,y,cx,cy:longint;
        diff:double;
        maxDiff:double=0;
    begin
      if (radius>0.5*min(xRes,yRes)) then exit(bestCircle(0.499*min(xRes,yRes)));
      for y:=round(radius) to round(yRes-radius) do
      for x:=round(radius) to round(xRes-radius) do begin
        diff:=colDiff(pixel[x,y],copy[x,y]);
        if (diff>maxDiff) then begin
          cx:=x;
          cy:=y;
          maxDiff:=diff;
        end;
      end;
      result.cx:=cx;
      result.cy:=cy;
      result.radius:=radius;
      result.diff:=diff;
      result.color:=avgColor(copy,result);
    end;

  VAR radius:double;
      circleSamples:longint=1;
      progress:T_progressEstimatorQueue;
      oldChildOfContainingQueue:P_progressEstimatorQueue;
  begin
    progress.create(nil);
    if containingQueue<>nil then begin
      containingQueue^.setTemporaryChildProgress(oldChildOfContainingQueue,@progress);
    end;
    progress.forceStart(et_stepCounterWithoutQueue,count);
    radius:=relativeCircleSize*diagonal;
    copy.create(self);
    for i:=0 to xRes*yRes-1 do datFloat[i]:=white;
    for i:=0 to count-1 do begin
      if ((i*1000) div count<>((i-1)*1000) div count) or (radius>=0.1*diagonal) then begin
        if progress.cancellationRequested then break;
        radius:=max(relativeCircleSize*diagonal*min(1,1/6*globalAvgDiff),1);
        circleSamples:=round(10000/sqr(radius));
        if circleSamples>31 then circleSamples:=31;
      end;
      for j:=0 to circleSamples do begin
        newCircle:=randomCircle(radius);
        newCircle.color:=avgColor(copy,newCircle);
        newCircle.diff:=colDiff(avgColor(self,newCircle),newCircle.color);
        if (j=0) or (newCircle.diff>toDraw.diff) then toDraw:=newCircle;
      end;
      drawCircle(toDraw);
      progress.logStepDone;
    end;
    copy.destroy;
    progress.logEnd;
    if containingQueue<>nil then begin
      containingQueue^.setTemporaryChildProgress(oldChildOfContainingQueue);
    end;
    progress.destroy;
  end;

end.
