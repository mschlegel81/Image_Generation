UNIT mypics;
INTERFACE
{fputype sse3}
USES myColors,dos,sysutils,Interfaces, ExtCtrls, Graphics, IntfGraphics, GraphType,types,myGenerics, mySys,math, myParams,FPWriteJPEG,FileUtil;

{$define include_interface}
TYPE
  T_resizeStyle=(res_exact,
                 res_cropToFill,
                 res_fit,
                 res_dataResize);

CONST CHUNK_BLOCK_SIZE =64;

TYPE
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
      PROCEDURE saveToFile(CONST fileName:ansistring; CONST allowRetry:boolean=true);
      PROCEDURE loadFromFile(CONST fileName:ansistring);
      FUNCTION saveJpgWithSizeLimitReturningErrorOrBlank(CONST fileName:ansistring; CONST sizeLimit:SizeInt; CONST allowRetry:boolean=true):ansistring;
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
      PROCEDURE naiveEdges;
      PROCEDURE blurWith(CONST relativeBlurMap:T_rawImage);
      PROCEDURE medianFilter(CONST relativeSigma:double);
      PROCEDURE modalFilter(CONST relativeSigma:double);
      PROCEDURE niceLine(CONST x0,y0,x1,y1:double; CONST color:T_floatColor; CONST alpha:double);
      PROCEDURE sketch(CONST colorCount:byte; CONST relativeDirMapSigma,density,tolerance:double);
      //PROCEDURE paint(CONST mapGranularity:longint; CONST blurSigma,density,curvature:double; CONST colorStyle:byte);
      PROCEDURE myFilter(CONST thresholdDistParam,param:double);
  end;

F_displayErrorFunction=PROCEDURE(CONST s:ansistring);

VAR compressionQualityPercentage:longint=100;
    displayErrorFunction:F_displayErrorFunction=nil;

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

PROCEDURE T_rawImage.saveToFile(CONST fileName: ansistring; CONST allowRetry:boolean=true);
  PROCEDURE storeDump;
    VAR handle:file of byte;
    begin
      assign(handle,fileName);
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
    try
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
          img.saveToFile(fileName,Jpeg);
          storeImg.Picture.Jpeg.saveToFile(fileName);
          img.free;
          Jpeg.free;
        end;
        storeImg.free;
      end else storeDump;
    except
      if allowRetry then try
        saveToFile(UTF8ToSys(fileName),false);
      except
        saveToFile(SysToUTF8(fileName),false);
      end;
    end;
  end;

PROCEDURE T_rawImage.loadFromFile(CONST fileName: ansistring);
  VAR useFilename:ansistring;
  PROCEDURE restoreDump;
    VAR handle:file of byte;
    begin
      freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
      writeln(stdErr,'Loading image ',useFilename,' as dump');
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
      writeln(stdErr,'Loading image ',useFilename,' via TImage');
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

FUNCTION T_rawImage.saveJpgWithSizeLimitReturningErrorOrBlank(CONST fileName:ansistring; CONST sizeLimit:SizeInt; CONST allowRetry:boolean=true):ansistring;
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
      img.saveToFile(fileName,Jpeg);
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
    try
      if sizeLimit=0 then exit(saveJpgWithSizeLimitReturningErrorOrBlank(fileName,round(1677*sqrt(xRes*yRes)),allowRetry));
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
    except
      if allowRetry then try
        saveJpgWithSizeLimitReturningErrorOrBlank(UTF8ToSys(fileName),sizeLimit,false);
      except
        saveJpgWithSizeLimitReturningErrorOrBlank(SysToUTF8(fileName),sizeLimit,false);
      end;
    end;
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
      temp24bit:T_24Bit;
      tempCol  :T_floatColor;
  begin
    for y:=0 to yRes shr 1 do begin
      y1:=yRes-1-y;
      if y1>y then for x:=0 to xRes-1 do begin tempCol  :=pixel     [x,y]; pixel     [x,y]:=pixel     [x,y1]; pixel     [x,y1]:=tempCol  ; end;
    end;
  end;

PROCEDURE T_rawImage.flop;
  VAR x,y,x1:longint;
      temp24bit:T_24Bit;
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
  VAR i,j:longint;
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
      if changeDirection then begin d:=dirMap[ix,iy]; if d*dir > 0 then dir:=d else dir:=-1*d; end;
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
      for k:=0 to 1 do
        begin
          ix:=x; iy:=y; pos:=newColor(x,y,0); dir:=(k*2-1)*dirMap[x,y]; step;
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

PROCEDURE T_rawImage.naiveEdges;
  VAR output:T_rawImage;
      x,y:longint;
      maxDist:double;
  begin
    output.create(xRes,yRes);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do
      output[x,y]:=white*
      calcErr(pixel[max(0     ,x-1),max(0,y-1)],
              pixel[           x   ,max(0,y-1)],
              pixel[min(xRes-1,x+1),max(0,y-1)],
              pixel[max(0     ,x-1),y],
              pixel[           x   ,y],
              pixel[min(xRes-1,x+1),y],
              pixel[max(0     ,x-1),min(yRes-1,y+1)],
              pixel[           x   ,min(yRes-1,y+1)],
              pixel[min(xRes-1,x+1),min(yRes-1,y+1)]);
    copyFromImage(output);
    output.destroy;
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

PROCEDURE T_rawImage.niceLine(CONST x0,y0,x1,y1:double; CONST color:T_floatColor; CONST alpha:double);
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

PROCEDURE T_rawImage.sketch(CONST colorCount:byte; CONST relativeDirMapSigma,density,tolerance:double);
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
      x,y,i,k,l:longint;
      lineX,lineY:array[0..1] of double;
      lineColor:T_floatColor;
      alpha:single;
      dir:T_floatColor;
  begin
    grad:=directionMap(relativeDirMapSigma);
    temp.create(self);
    temp.quantize(colorCount);
    for x:=0 to xRes*yRes-1 do datFloat[x]:=white24Bit;
    alpha:=0.9;
    if density>1 then alpha:=exp(density*ln(0.9));
    for l:=0 to 12 do for y:=0 to yRes-1 do for x:=0 to xRes-1 do if (lev(x,y)=l) and (random<density) then begin
      lineColor:=temp[x,y]+newColor(random-0.5,random-0.5,random-0.5)*0.05;
      dir:=grad[x,y];
      for k:=0 to 1 do begin
        i:=round(random*diagonal*0.05);
        lineX[k]:=x+i*dir[0];
        lineY[k]:=y+i*dir[1];
        dir:=(-1)*dir;
      end;
      niceLine(lineX[0],lineY[0],lineX[1],lineY[1],lineColor,(1-alpha));
    end;
    temp.destroy;
    grad.destroy;
  end;

//PROCEDURE T_rawImage.paint(CONST mapGranularity:longint; CONST blurSigma,density,curvature:double; CONST colorStyle:byte);
//  VAR x,y,ix,iy,kx,ky,i,imax:longint;
//      fx,fy,vx,vy:single;
//      gradMap,cop:T_;
//      collect:T_24Bit;
//      pg,pt,pd:P_24Bit;
//
//  PROCEDURE moveInDirection(d:byte); inline;
//    VAR dx,dy:single;
//    begin
//      d:=(d+127) and 255;
//      if d<128 then begin
//        if d<64 then begin    dy:=                   0.0110485434560398 *d;
//                              dx:=1                 -0.00457645654396019*d;
//        end else begin        dy:=0.4142135623730951+0.00457645654396019*d;
//                              dx:=1.4142135623730951-0.0110485434560398 *d; end;
//      end else begin
//        if d<192 then begin   dy:=1.5857864376269048-0.00457645654396019*d;
//                              dx:=1.4142135623730951-0.0110485434560398 *d;
//        end else begin        dy:=2.8284271247461902-0.0110485434560398 *d;
//                              dx:=0.1715728752538097-0.00457645654396019*d; end;
//      end;
//      if dx*vx+dy*vy>0 then begin vx:=vx*curvature+dx*(1-curvature); vy:=vy*curvature+dy*(1-curvature); end
//                       else begin vx:=vx*curvature-dx*(1-curvature); vy:=vy*curvature-dy*(1-curvature); end;
//      dx:=1/sqrt(1E-6+vx*vx+vy*vy);
//      vx:=vx*dx;
//      vy:=vy*dx;
//      fx:=fx+vx; ix:=round(fx);
//      fy:=fy+vy; iy:=round(fy);
//    end;
//
//  PROCEDURE initDirection(d:byte); inline;
//    begin
//      d:=(d+127) and 255;
//      if d<128 then begin
//        if d<64 then begin    vy:=                   0.0110485434560398 *d;
//                              vx:=1                 -0.00457645654396019*d;
//        end else begin        vy:=0.4142135623730951+0.00457645654396019*d;
//                              vx:=1.4142135623730951-0.0110485434560398 *d; end;
//      end else begin
//        if d<192 then begin   vy:=1.5857864376269048-0.00457645654396019*d;
//                              vx:=1.4142135623730951-0.0110485434560398 *d;
//        end else begin        vy:=2.8284271247461902-0.0110485434560398 *d;
//                              vx:=0.1715728752538097-0.00457645654396019*d; end;
//      end;
//      if random<0.5 then begin
//        vx:=-vx; vy:=-vy;
//      end;
//      fx:=x; ix:=x;
//      fy:=y; iy:=y;
//    end;
//
//  PROCEDURE move; inline;
//    begin
//      fx:=fx+vx; ix:=round(fx);
//      fy:=fy+vy; iy:=round(fy);
//    end;
//
//  FUNCTION mixCol(VAR c1,c2:T_24Bit):T_24Bit; inline;
//    begin
//      result[0]:=(c1[0]*3+c2[0]) shr 2;
//      result[1]:=(c1[1]*3+c2[1]) shr 2;
//      result[2]:=(c1[2]*3+c2[2]) shr 2;
//    end;
//
//  FUNCTION mixCol2(VAR c1,c2:T_24Bit):T_24Bit; inline;
//    begin
//      result[0]:=(c1[0]+c2[0]) shr 1;
//      result[1]:=(c1[1]+c2[1]) shr 1;
//      result[2]:=(c1[2]+c2[2]) shr 1;
//    end;
//
//  PROCEDURE initColor(c:T_24Bit); inline;
//    begin
//      collect[0]:=min(255,max(0,c[0]-colorStyle+random(2*colorStyle)));
//      collect[1]:=min(255,max(0,c[1]-colorStyle+random(2*colorStyle)));
//      collect[2]:=min(255,max(0,c[2]-colorStyle+random(2*colorStyle)));
//    end;
//
//  begin
//   //if curvature>1 then curvature:=0 else
//    //if curvature>=100 then curvature:=0 else curvature:=exp(-curvature*1E-2*diagonal);
//    imax:=round(blurSigma);
//    ProgressReporter('creating gradient map');
//    gradMap.createCopy (self);
//    gradMap.gradientMap(mapGranularity);
//    cop    .createCopy (self);
//    pg:=P_24Bit(gradMap.pixelBuffer);
//    pt:=P_24Bit(cop    .pixelBuffer);
//    pd:=P_24Bit(        pixelBuffer);
//    if density<1 then for ky:=0 to xRes*yRes-1 do pd[ky]:=white24Bit;
//    for ky:=0 to yRes-1 do begin
//      ProgressReporter('finishing '+formatFloat('###.##',100*ky/yRes)+'%');
//      for kx:=0 to xRes-1 do if random<density then begin //round(density*4*xRes/sqrt(iMax)) do begin
//        x:=kx;//random(xRes);
//        y:=ky*4;
//        while y>yRes-1 do begin y:=y-yRes+1; end;
//        initColor    (pt[x+y*xRes]);
//        initDirection(pg[x+y*xRes][0]);
//        if curvature=1 then begin
//          for i:=0 to imax do if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
//            pd[ix+iy*xRes]:=mixCol(pd[ix+iy*xRes],collect);
//            move;
//          end;
//        end else begin
//          for i:=0 to imax do if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
//            pd[ix+iy*xRes]:=mixCol(pd[ix+iy*xRes],collect);
//            moveInDirection(pg[ix+iy*xRes][0]);
//          end;
//        end;
//      end;
//    end;
//    {if density>=1 then for y:=0 to yRes-1 do begin
//      ProgressReporter('polish '+formatFloat('###.##',100*y/yRes)+'%');
//      for x:=0 to xRes-1 do if dist(pd[x+y*xRes],pt[x+y*xRes])>dist(pd[x+y*xRes],white) then begin
//        initColor    (pt[x+y*xRes]);
//        initDirection(pg[x+y*xRes][0]);
//        if curvature>=1 then begin
//          for i:=0 to imax do if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
//            pd[ix+iy*xRes]:=mixCol2(pd[ix+iy*xRes],collect);
//            move;
//          end;
//        end else begin
//          for i:=0 to imax do if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
//            pd[ix+iy*xRes]:=mixCol(pd[ix+iy*xRes],collect);
//            moveInDirection(pg[ix+iy*xRes][0]);
//          end;
//        end;
//      end;
//    end;}
//    gradMap.destroy;
//    cop    .destroy;
//  end;

  {PROCEDURE diffuse(inName,outName:string; steps:longint; earlyAbort:PBoolean);
  VAR bmp,bmpOut:T_24BitBitmap;
      ping,pong,field:T_rgbaArray;
      xRes,yRes:longint;
  CONST timeStep=1;

PROCEDURE buildField;
    CONST gaussKernel:array[-5..5] of single=(
      0.0820849986238988,0.20189651799465541,0.40656965974059911,0.6703200460356393,0.90483741803595957,1,
      0.90483741803595957,0.6703200460356393,0.40656965974059911,0.20189651799465541,0.0820849986238988);
    VAR x,y,dx,dy:longint;
        c        :array [-1..1,-1..1] of T_24Bit;
        ptf      :array[-5..5] of P_rgbaColor;
        normFak  :single;

    FUNCTION getGradient:T_rgbaColor; inline;
      VAR grad:array[0..1] of single;
          sg  :array[0..1] of single;
          i:byte;
      begin
        if      y=0      then begin c[-1,-1]:=c[0,-1]; c[-1,0]:=c[0,0]; c[-1,1]:=c[0,1]; end
        else if y=yRes-1 then begin c[ 1,-1]:=c[0,-1]; c[ 1,0]:=c[0,0]; c[ 1,1]:=c[0,1]; end;
        result.r:=0;
        result.g:=0;
        for i:=0 to 2 do begin
          grad[1]:=-0.0007843137254902 *c[-1,-1][1]-0.00235294117647059*c[-1, 0][1]-0.0007843137254902 *c[-1, 1][1]
                   +0.0007843137254902 *c[ 1,-1][1]+0.00235294117647059*c[ 1, 0][1]+0.0007843137254902 *c[ 1, 1][1];
          grad[0]:=-0.0007843137254902 *c[-1,-1][1]+0.0007843137254902 *c[-1, 1][1]
                   -0.00235294117647059*c[ 0,-1][1]+0.00235294117647059*c[ 0, 1][1]
                   -0.0007843137254902 *c[ 1,-1][1]+0.0007843137254902 *c[ 1, 1][1];
          sg  [0]:=grad[0]*grad[0]-grad[1]*grad[1];
          sg  [1]:=2*grad[0]*grad[1];
          if grad[0]>1E-6 then begin
            result.r:=result.r+sg[0];
            result.g:=result.g+sg[1];
          end;
        end;
      end;

    PROCEDURE retransform(VAR c:T_rgbaColor); inline;
      VAR arg,vx,vy:single;
      begin
        //Diffusion Kernel:
        // +-------> x
        // | r g b
        // | a   a
        // | b g r
        // V
        // y
        arg:=0.5*arctan2(c.g,c.r)+pi/2;
        vx:=cos(arg);
        vy:=sin(arg);
        arg:=1/max(abs(vx),abs(vy));
        vx:=vx*arg;
        vy:=vy*arg;
        c.r:=max(0,       vx  *vy);
        c.g:=max(0,(1-abs(vx))*abs(vy));
        c.b:=max(0,      -vx  *vy);
        c.a:=max(0,(1-abs(vy))*abs(vx));
        arg:=0.5*timeStep/(c.r+c.g+c.b+c.a);
        c.r:=c.r*arg;
        c.g:=c.g*arg;
        c.b:=c.b*arg;
        c.a:=c.a*arg;
      end;

    begin
      field.create(xRes,yRes);
      bmp.optimizeForCrissCrossAccess;
      //gather local gradients:------------------------------------------//
      for y:=0 to yRes-1 do begin                                        //
        ptf[0]:=field.getLine(y);                                        //
        for x:=0 to xRes-1 do begin                                      //
          //read local stencil:--------------------------------------//  //
          if x=0 then begin                                          //  //
            for dy:=1 downto -1 do                                   //  //
            if (y+dy>=0) and (y+dy<yRes) then begin                  //  //
              for dx:=0 to 1 do                                      //  //
                c[dy,dx]:=bmp.pixel[x+dx,y+dy];                      //  //
              c[dy,-1]:=c[dy,0];                                     //  //
            end;                                                     //  //
          end else begin                                             //  //
            for dy:=1 downto -1 do                                   //  //
            if (y+dy>=0) and (y+dy<yRes) then begin                  //  //
              for dx:=-1 to 0 do c[dy,dx]:=c[dy,dx+1];               //  //
              if x<xRes-2 then c[dy,1]:=bmp.pixel[x+1,y+dy]          //  //
            end;                                                     //  //
          end;                                                       //  //
          //----------------------------------------:read local stencil  //
          ptf[0][x]:=getGradient;                                        //
        end; //for x                                                     //
      end; //for y                                                       //
      //--------------------------------------------:gather local gradients
      ////y gauss convolution (read R,G; write B,A):-------------------//
      //for y:=0 to yRes-1 do begin                                    //
        //normFak:=0;                                                  //
        //for dy:=-5 to 5 do if (y+dy>=0) and (y+dy<yRes) then begin   //
          //ptf[dy]:=field.getLine(y+dy);                              //
          //normFak:=normFak+gaussKernel[dy];                          //
        //end;                                                         //
        //normFak:=1/normFak;                                          //
        //for x:=0 to xRes-1 do begin                                  //
          //ptf[0][x].b:=0;                                            //
          //ptf[0][x].a:=0;                                            //
          //for dy:=-5 to 5 do if (y+dy>=0) and (y+dy<yRes) then begin //
            //ptf[0][x].b:=ptf[0][x].b+ptf[dy][x].r*gaussKernel[dy];   //
            //ptf[0][x].a:=ptf[0][x].a+ptf[dy][x].g*gaussKernel[dy];   //
          //end;                                                       //
          //ptf[0][x].b:=ptf[0][x].b*normFak;                          //
          //ptf[0][x].a:=ptf[0][x].a*normFak;                          //
        //end; //for x                                                 //
      //end; //for y                                                   //
      ////---------------------:y gauss convolution (read R,G; write B,A)
      ////x gauss convolution (read B,A; write R,G):-------------------//
      //for y:=0 to yRes-1 do begin                                    //
        //ptf[0]:=field.getLine(y);                                    //
        //for x:=0 to xRes-1 do begin                                  //
          //normFak:=0;                                                //
          //ptf[0][x].r:=0;                                            //
          //ptf[0][x].g:=0;                                            //
          //for dx:=-5 to 5 do if (x+dx>=0) and (x+dx<xRes) then begin //
            //ptf[0][x].r:=ptf[0][x].r+ptf[0][x+dx].b*gaussKernel[dx]; //
            //ptf[0][x].g:=ptf[0][x].g+ptf[0][x+dx].a*gaussKernel[dx]; //
            //normFak    :=normFak    +               gaussKernel[dx]; //
          //end;                                                       //
          //normFak:=1/normFak;                                        //
          //ptf[0][x].r:=ptf[0][x].r*normFak;                          //
          //ptf[0][x].g:=ptf[0][x].g*normFak;                          //
          ////retransform(ptf[0][x]);                                    //
        //end;                                                         //
      //end;                                                           //
      ////---------------------:x gauss convolution (read B,A; write R,G)
      //y gauss convolution (read R,G; write B,A):-------------------//
      for y:=0 to yRes-1 do begin                                    //
        normFak:=0;                                                  //
        for dy:=-5 to 5 do if (y+dy>=0) and (y+dy<yRes) then begin   //
          ptf[dy]:=field.getLine(y+dy);                              //
          normFak:=normFak+gaussKernel[dy];                          //
        end;                                                         //
        normFak:=1/normFak;                                          //
        for x:=0 to xRes-1 do begin                                  //
          ptf[0][x].b:=0;                                            //
          ptf[0][x].a:=0;                                            //
          for dy:=-5 to 5 do if (y+dy>=0) and (y+dy<yRes) then begin //
            ptf[0][x].b:=ptf[0][x].b+ptf[dy][x].r*gaussKernel[dy];   //
            ptf[0][x].a:=ptf[0][x].a+ptf[dy][x].g*gaussKernel[dy];   //
          end;                                                       //
          ptf[0][x].b:=ptf[0][x].b*normFak;                          //
          ptf[0][x].a:=ptf[0][x].a*normFak;                          //
        end; //for x                                                 //
      end; //for y                                                   //
      //---------------------:y gauss convolution (read R,G; write B,A)
      //x gauss convolution (read B,A; write R,G):-------------------//
      for y:=0 to yRes-1 do begin                                    //
        ptf[0]:=field.getLine(y);                                    //
        for x:=0 to xRes-1 do begin                                  //
          normFak:=0;                                                //
          ptf[0][x].r:=0;                                            //
          ptf[0][x].g:=0;                                            //
          for dx:=-5 to 5 do if (x+dx>=0) and (x+dx<xRes) then begin //
            ptf[0][x].r:=ptf[0][x].r+ptf[0][x+dx].b*gaussKernel[dx]; //
            ptf[0][x].g:=ptf[0][x].g+ptf[0][x+dx].a*gaussKernel[dx]; //
            normFak    :=normFak    +               gaussKernel[dx]; //
          end;                                                       //
          normFak:=1/normFak;                                        //
          ptf[0][x].r:=ptf[0][x].r*normFak;                          //
          ptf[0][x].g:=ptf[0][x].g*normFak;                          //
          retransform(ptf[0][x]);                                    //
        end;                                                         //
      end;                                                           //
      //---------------------:x gauss convolution (read B,A; write R,G)
    end;

  PROCEDURE stepForward(VAR inData,outData:T_rgbaArray);
    PROCEDURE incCol(VAR c,difference:T_rgbaColor; factor:single); inline;
      begin
        c.r:=c.r+difference.r*factor;
        c.g:=c.g+difference.g*factor;
        c.b:=c.b+difference.b*factor;
      end;
    VAR ptIn :array[-1..1] of P_rgbaColor;
        ptOut:                P_rgbaColor;
        ptF  :                P_rgbaColor;
        x,y:longint;
    begin
      for y:=0 to yRes-1 do begin
        ptIn[-1]:=inData .getLine(y-1);
        ptIn[ 0]:=inData .getLine(y  );
        ptIn[ 1]:=inData .getLine(y+1);
        ptOut   :=outData.getLine(y);
        ptF     :=field  .getLine(y);
        for x:=0 to xRes-1 do begin
          ptOut[x].r:=ptIn[0][x].r*(1-timestep);
          ptOut[x].g:=ptIn[0][x].g*(1-timestep);
          ptOut[x].b:=ptIn[0][x].b*(1-timestep);
          //Diffusion Kernel:
          // +-------> x
          // | r g b
          // | a   a
          // | b g r
          // V
          // y
          if (x>0     ) and (y>0     ) then incCol(ptOut[x],ptIn[-1][x-1],ptF[x].r);
          if (x<xRes-1) and (y<yRes-1) then incCol(ptOut[x],ptIn[ 1][x+1],ptF[x].r);
          if                (y>0     ) then incCol(ptOut[x],ptIn[-1][x  ],ptF[x].g);
          if                (y<yRes-1) then incCol(ptOut[x],ptIn[ 1][x  ],ptF[x].g);
          if (x<xRes-1) and (y>0     ) then incCol(ptOut[x],ptIn[-1][x+1],ptF[x].b);
          if (x>0     ) and (y<yRes-1) then incCol(ptOut[x],ptIn[ 1][x-1],ptF[x].b);
          if (x>0     )                then incCol(ptOut[x],ptIn[ 0][x-1],ptF[x].a);
          if (x<xRes-1)                then incCol(ptOut[x],ptIn[ 0][x+1],ptF[x].a);
        end; //for x
      end; //for y

      outData.saveToBMP(StringOfChar('0',4-length(intToStr(steps)))+intToStr(steps)+'.bmp');
    end;


  begin
    bmp.create(inName,-1);
    xRes:=bmp.width;
    yRes:=bmp.height;
    field.create(xRes,yRes);
    buildField;
    bmpOut.create(outName,xRes,yRes,-1);
    ping .create(xRes,yRes);
    pong .create(xRes,yRes);
    if odd(steps) then ping.copyFrom(bmp)
                  else pong.copyFrom(bmp);
    bmp.destroy;
    while steps>0 do begin
      writeln(steps,' steps remaining');
      if odd(steps) then stepForward(ping,pong)
                    else stepForward(pong,ping);
      dec(steps);
    end;
    ping .destroy;
    field.destroy;
    pong .saveToBMP(outName);
    pong .destroy;
  end;  }

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
    temp1.copyFrom(self);
    temp1.blur(maxFactor/2);
    ProgressReporter('full blur');
    temp2.create(xRes,yRes);
    temp2.copyFrom(self);
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
  VAR temp   :T_rawImage;
      pTemp  :P_floatColor;
      x,y    : longint;
      kernel :T_arrayOfDouble;

  FUNCTION localThingy(x,y:longint):T_floatColor;

{skew=(mean-median)/[standard deviation]
skew=(µ-median)/s
skew=(E(X³)-3µs²-µ³)/s³
median=µ-s*skew
      =µ-s*(E(X³)-3µs²-µ³)/s³
      =µ-  (E(X³)-3µs²-µ³)/s²
      =µ-  (E(X³)-3µs²-µ³)/s²
µ=sum/samples
s=sqrt(sum(X²)/samples-sqr(sum/samples))
 }
    VAR c:T_floatColor;
        dc,sum,sum2,sum3:array[0..2] of double;
        mean,sigma,invWeight,weight,sumOfWeights:double;
        dx,dy,i:longint;
    begin
      for i:=0 to 2 do sum[i]:=0;
      for i:=0 to 2 do sum2[i]:=0;
      for i:=0 to 2 do sum3[i]:=0;
      sumOfWeights:=0;
      for dy:=max(-y,-length(kernel)) to min(yRes-1-y,length(kernel)) do
      for dx:=max(-x,-length(kernel)) to min(xRes-1-x,length(kernel)) do begin
        c:=pTemp[x+dx+(y+dy)*xRes];
        weight:=kernel[abs(dx)]*kernel[abs(dy)];
        for i:=0 to 2 do begin
          dc[i]:=c[i]*weight;
          sum[i]:=sum[i]+dc[i];
          dc[i]:=dc[i]*c[i];
          sum2[i]:=sum2[i]+dc[i];
          dc[i]:=dc[i]*c[i];
          sum3[i]:=sum3[i]+dc[i];
        end;
        sumOfWeights:=sumOfWeights+weight;
      end;
      invWeight:=1/sumOfWeights;
      for i:=0 to 2 do begin
        mean:=sum[i]*invWeight;
        sigma:=sum2[i]*invWeight-mean*mean;
        if sigma<1E-8 then result[i]:=mean
        else begin
          sigma:=sqrt(sigma);
          weight:=param*sigma*arctan((sum3[i]*invWeight-mean*mean*mean)/(sigma*sigma*sigma)-3*mean/sigma);
          result[i]:=mean-weight;
        end;
      end;
    end;

  VAR z:longint;
  begin
    temp.create(self);
    pTemp:=temp.datFloat;
    kernel:=getSmoothingKernel(thresholdDistParam);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do
      datFloat[x+y*xRes]:=localThingy(x,y);
    temp.destroy;
    setLength(kernel,0);
  end;



end.
