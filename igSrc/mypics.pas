UNIT mypics;
INTERFACE
{fputype sse3}
USES myColors,dos,sysutils,Interfaces, ExtCtrls, Graphics, IntfGraphics, GraphType,types,myGenerics, mySys,math, myParams,FPWriteJPEG;

{$define include_interface}
TYPE
  T_rawStyle=(rs_24bit,
              rs_float);
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
      style   :T_rawStyle;
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
      CONSTRUCTOR create(VAR original:T_rawImage);
      DESTRUCTOR destroy;
      //Access per pixel:-------------------------------------------------------
      FUNCTION width:longint;
      FUNCTION height:longint;
      FUNCTION diagonal:double;
      PROPERTY pixel     [x,y:longint]:T_floatColor read getPixel write setPixel; default;
      PROPERTY pixel24Bit[x,y:longint]:T_24Bit read getPixel24Bit write setPixel24Bit;
      PROCEDURE multIncPixel(CONST x,y:longint; CONST factor:single; CONST increment:T_floatColor);
      PROCEDURE mutateType(CONST newType:T_rawStyle);
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
      PROCEDURE crop(CONST x0,x1,y0,y1:longint);
      //-------------------------------------------------:Geometry manipulations
      //Statistic accessors:----------------------------------------------------
      FUNCTION histogram:T_compoundHistogram;
      FUNCTION histogram(CONST x,y:longint; CONST smoothingKernel:T_arrayOfDouble):T_compoundHistogram;
      FUNCTION histogramHSV:T_compoundHistogram;
      //----------------------------------------------------:Statistic accessors
      PROCEDURE blur(CONST relativeXBlur:double; CONST relativeYBlur:double);
      FUNCTION directionMap(CONST relativeSigma:double):T_rawImage;
      PROCEDURE lagrangeDiffusion(CONST relativeGradSigma,relativeBlurSigma:double);
      PROCEDURE lagrangeDiffusion(VAR dirMap:T_rawImage; CONST relativeBlurSigma:double);
      PROCEDURE radialBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
      PROCEDURE rotationalBlur(CONST relativeBlurSigma,relativeCenterX,relativeCenterY:double);
      PROCEDURE shine;
      PROCEDURE sharpen(CONST relativeSigma,factor:double);
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
//    if (x<0) or (x>=xRes) or (y<0) or (y>=yRes) then exit;
    if style<>rs_float then mutateType(rs_float);
    datFloat[x+y*xRes]:=value
  end;

FUNCTION T_rawImage.getPixel(CONST x, y: longint): T_floatColor;
  begin
  //  if (x<0) or (x>=xRes) or (y<0) or (y>=yRes) then exit(black);
    case style of
      rs_24bit: result:=dat24bit[x+y*xRes];
      rs_float: result:=datFloat[x+y*xRes];
    end;
  end;

PROCEDURE T_rawImage.setPixel24Bit(CONST x, y: longint; CONST value: T_24Bit);
  begin
  //  if (x<0) or (x>=xRes) or (y<0) or (y>=yRes) then exit;
    case style of
      rs_24bit: dat24bit[x+y*xRes]:=value;
      rs_float: datFloat[x+y*xRes]:=value;
    end;
  end;

FUNCTION T_rawImage.getPixel24Bit(CONST x, y: longint): T_24Bit;
  begin
   // if (x<0) or (x>=xRes) or (y<0) or (y>=yRes) then exit(black24Bit);
    case style of
      rs_24bit: result:=dat24bit[x+y*xRes];
      rs_float: result:=projectedColor(datFloat[x+y*xRes]);
    end;
  end;

CONSTRUCTOR T_rawImage.create(CONST width_, height_: longint);
  begin
    style:=rs_24bit;
    xRes:=width_;
    yRes:=height_;
    getMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
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
    if xRes*yRes>0 then case style of
      rs_24bit: freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
      rs_float: freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
    end;
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
      ScanLineImage: TLazIntfImage;
      ImgFormatDescription: TRawImageDescription;
      pc:T_24Bit;
      pix:PByte;

  begin
    case style of
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

PROCEDURE T_rawImage.multIncPixel(CONST x,y:longint; CONST factor:single; CONST increment:T_floatColor);
  VAR k:longint;
  begin
    if style<>rs_float then mutateType(rs_float);
    k:=x+y*xRes;
    datFloat[k]:=datFloat[k]*factor+increment;
  end;

PROCEDURE T_rawImage.mutateType(CONST newType: T_rawStyle);
  VAR i:longint;
  begin
    if newType=style then exit;
    case newType of
      rs_float: begin
        getMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
        for i:=0 to xRes*yRes-1 do datFloat[i]:=dat24bit[i];
        freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
      end;
      rs_24bit: begin
        getMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
        for i:=0 to xRes*yRes-1 do dat24bit[i]:=projectedColor(datFloat[i]);
        freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor));
      end;
    end;
    style:=newType;
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
        col24Grey:array[false..true] of T_24Bit     =((153,153,153),(103,103,103));
  VAR i,j:longint;
  begin
    if style=rs_24bit
    then for j:=0 to yRes-1 do for i:=0 to xRes-1 do setPixel24Bit(i,j,col24Grey[odd(i shr 5) xor odd(j shr 5)])
    else for j:=0 to yRes-1 do for i:=0 to xRes-1 do setPixel     (i,j,floatGrey[odd(i shr 5) xor odd(j shr 5)]);
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
      end;
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
        img.saveToFile(fileName,Jpeg);
        storeImg.Picture.Jpeg.saveToFile(fileName);
        img.free;
        Jpeg.free;
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
      rs_24bit: if dat24bit<>nil then freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit));
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
    end;
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
        writeln('Filesize @',quality:3,'% is ',sizes[quality] shr 10:6,'kB');
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
    writeln('  trying to save @',sizeLimit shr 10:6,'kB');
    lastSavedQuality:=-1;
    quality:=100;
    while (quality>4  ) and (getSizeAt(quality  )> sizeLimit) do dec(quality, 8);
    while (quality<100) and (getSizeAt(quality  )< sizeLimit) do inc(quality, 4);
    while (quality>0  ) and (getSizeAt(quality  )> sizeLimit) do dec(quality, 2);
    while (quality<100) and (getSizeAt(quality+1)<=sizeLimit) do inc(quality, 1);
    if lastSavedQuality<>quality then saveAtQuality(quality);
    storeImg.free;
    writeln('Saved to ',fileName,' @',quality,'%');
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
      if newHeight*newWidth<>xRes*yRes then case style of
        rs_24bit: begin freeMem(dat24bit,xRes*yRes*sizeOf(T_24Bit)); getMem(dat24bit,newWidth*newHeight*sizeOf(T_24Bit)); end;
        rs_float: begin freeMem(datFloat,xRes*yRes*sizeOf(T_floatColor)); getMem(datFloat,newWidth*newHeight*sizeOf(T_floatColor)); end;
      end;
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

FUNCTION T_rawImage.histogram: T_compoundHistogram;
  VAR i:longint;
  begin
    result.create;
    case style of
      rs_24bit: for i:=0 to xRes*yRes-1 do result.putSample(dat24bit[i]);
      rs_float: for i:=0 to xRes*yRes-1 do result.putSample(datFloat[i]);
    end;
  end;

FUNCTION T_rawImage.histogram(CONST x, y: longint;
  CONST smoothingKernel: T_arrayOfDouble): T_compoundHistogram;
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

FUNCTION T_rawImage.histogramHSV: T_compoundHistogram;
  VAR i:longint;
  begin
    result.create;
    case style of
      rs_24bit: for i:=0 to xRes*yRes-1 do result.putSample(toHSV(dat24bit[i]));
      rs_float: for i:=0 to xRes*yRes-1 do result.putSample(toHSV(datFloat[i]));
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

PROCEDURE T_rawImage.blur(CONST relativeXBlur: double;
  CONST relativeYBlur: double);
  VAR kernel:T_arrayOfDouble;
      temp:T_rawImage;
      ptmp:P_floatColor;
      x,y,z:longint;
      sum:T_floatColor;
      weight:double;
  begin
    temp.create(xRes,yRes);
    temp.mutateType(rs_float);
    mutateType(rs_float);
    ptmp:=temp.datFloat;
    kernel:=getSmoothingKernel(relativeXBlur/100*diagonal);
    //blur in x-direction:-----------------------------------------------
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
                                                        sum:=    kernel[ 0]*datFloat[x+  y*xRes]; weight:=       kernel[ 0];
      for z:=max(-x,-length(kernel)) to -1     do begin sum:=sum+kernel[-z]*datFloat[x+z+y*xRes]; weight:=weight+kernel[-z]; end;
      for z:=1 to min(xRes-x-1,length(kernel)) do begin sum:=sum+kernel[ z]*datFloat[x+z+y*xRes]; weight:=weight+kernel[ z]; end;
      ptmp[x+y*xRes]:=sum*(1/weight);
    end;
    //-------------------------------------------------:blur in x-direction
    setLength(kernel,0);
    kernel:=getSmoothingKernel(relativeYBlur/100*diagonal);
    //blur in y-direction:---------------------------------------------------
    for x:=0 to xRes-1 do for y:=0 to yRes-1 do begin
                                                        sum:=    kernel[ 0]*ptmp[x+   y *xRes]; weight:=       kernel[ 0];
      for z:=max(-y,-length(kernel)) to -1     do begin sum:=sum+kernel[-z]*ptmp[x+(z+y)*xRes]; weight:=weight+kernel[-z]; end;
      for z:=1 to min(yRes-y-1,length(kernel)) do begin sum:=sum+kernel[ z]*ptmp[x+(z+y)*xRes]; weight:=weight+kernel[ z]; end;
      datFloat[x+y*xRes]:=sum*(1/weight);
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

PROCEDURE T_rawImage.lagrangeDiffusion(VAR dirMap:T_rawImage; CONST relativeBlurSigma:double);
  VAR output:T_rawImage;
      kernel:T_arrayOfDouble;
      x,y,i,k,ix,iy:longint;
      pos,dir:T_floatColor;
      colSum:T_floatColor;
      wgtSum:double;

  PROCEDURE step; inline;
    VAR d:T_floatColor;
    begin
      d:=dirMap[x,y]; if d*dir > 0 then dir:=d else dir:=-1*d;
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
      for k:=0 to 2 do begin
        ix:=x; iy:=y; pos:=newColor(x,y,0); dir:=(k*2-1)*dirMap[x,y]; step;
        for i:=1 to length(kernel)-1 do if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
          colSum:=colSum+pixel[ix,iy]*kernel[i];
          wgtSum:=wgtSum+             kernel[i];
          step;
        end;
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
    lagrangeDiffusion(dirMap,relativeBlurSigma);
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
    lagrangeDiffusion(dirMap,relativeBlurSigma);
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
    if style=rs_24bit then exit;
    temp.create(xRes,yRes);
    temp.mutateType(rs_float);
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

end.
