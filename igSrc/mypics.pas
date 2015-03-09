UNIT mypics;
INTERFACE
{$fputype sse3}
{define globalWand}

USES {$ifdef UNIX}cmem,cthreads,{$endif}
     dos,sysutils,math,myFiles
     {$ifndef naked},magick_wand, ImageMagick{$endif}
     {$ifdef useExtensions} ,cmdLineParseUtil {$endif};

CONST C_vrawStyle_byte         : byte = 0;
      C_vrawStyle_24bit        : byte = 1;
      C_vrawStyle_float        : byte = 2;
      C_vrawStyle_floatWithByte: byte = 3;

{$define include_interface}
{$include myColors.inc}
TYPE
  T_verbosity=(quiet,outputWithoutLineBreak,outputWithLineBreak);

  T_24BitImage=object
    //A primitive image; data is stored as RGB, y-index running from bottom to top (as in BMP file format and in OpenGl)
    //All file accesses are handled via image-magick
    //NOTE: Strangely, display via OpenGL seems to require a different byte ordering than via LCL;
    //Display is done most easily via the following OpenGl commands,
    //assuming an image object with name <name>
    //   glPixelStorei(GL_UNPACK_ALIGNMENT,1);        //one call on initialization suffices
    //   glPixelStorei(GL_UNPACK_LSB_FIRST ,GL_True); //one call on initialization suffices
    //   glRasterPos2f(0,0);
    //   glDrawPixels(<name>.width, <name>.height, GL_RGB,GL_UNSIGNED_BYTE, <name>.rawData);
    //For a halfway efficient use in Lazarus employ something like the following:
    //  USES ...,mypics,GraphType,IntfGraphics;
    //  ...
    //  PROCEDURE TForm1.drawImage(VAR pic:T_24BitImage);
    //  VAR  ScanLineImage,                 //image with representation as in T_24BitImage
    //       tempIntfImage: TLazIntfImage;  //image with representation as in TBitmap
    //       y: Integer;                    //line counter
    //       ImgFormatDescription: TRawImageDescription;
    //  begin
    //    ScanLineImage:=TLazIntfImage.Create(pic.width,pic.height);
    //    ImgFormatDescription.Init_BPP24_B8G8R8_BIO_TTB(pic.width,pic.height);
    //    ImgFormatDescription.ByteOrder:=riboMSBFirst;        //correct byte ordering
    //    ScanLineImage.DataDescription:=ImgFormatDescription;
    //    for y:=0 to pic.height-1 do
    //      move(pic.getLinePtr(y)^,ScanLineImage.GetDataLineStart(y)^,pic.width*3);
    //    Image1.Picture.Bitmap.Width:=pic.width;
    //    Image1.Picture.Bitmap.Height:=pic.height;
    //    tempIntfImage:=Image1.Picture.Bitmap.CreateIntfImage;
    //    tempIntfImage.CopyPixels(ScanLineImage);
    //    Image1.Picture.Bitmap.LoadFromIntfImage(tempIntfImage);
    //    tempIntfImage.Free;
    //    ScanLineImage.Free;
    //  end;

    private
      xRes,yRes        :longint; //image dimensions
      pixelBuffer      :P_24Bit;
      FUNCTION  getPixel(x,y:longint):T_24Bit;
      PROCEDURE setPixel(x,y:longint; color:T_24Bit);
    public
      CONSTRUCTOR create;
      CONSTRUCTOR create(newWidth,newHeight:longint); //same as: create; resize(newWidth,newHeight);
      CONSTRUCTOR create(name:string);                //same as: create; loadFromFile(name);
      CONSTRUCTOR createCopy(original:T_24BitImage);
      DESTRUCTOR  destroy;
      PROCEDURE   saveToFile  (name:string);          //saves to file with chooseable name
      PROCEDURE   saveSizeLimitedJpg(name:string);
      PROCEDURE   loadFromFile(name:string);          //loads from file and sets filename to name
      PROCEDURE   resizeDat(newWidth,newHeight:longint); //resizes the image; WARNING!!! ALL DATA WILL BE LOST!
{$ifndef naked}
      PROCEDURE   resize(newWidth,newHeight:longint);
{$endif}
      PROCEDURE   crop      (x0,x1,y0,y1:longint);
      PROCEDURE   cropResize(newWidth,newHeight:longint);

      FUNCTION    rawData:P_24Bit;                      //returns pointer to raw data (useful for display via OpenGL)

      FUNCTION    width :longint; //returns width of image (=xRes)
      FUNCTION    height:longint; //returns height of image (=yRes)
      FUNCTION    size  :longint;
      FUNCTION    diagonal:double;
      PROPERTY    pixel[x,y:longint]:T_24Bit read getPixel write setPixel; default; //pixel-wise access
      FUNCTION    getLinePtr(y:longint):pointer;                           //line wise access
      PROCEDURE   transpose;
      PROCEDURE   copyFrom(original:T_24BitImage);                         //copies complete data from original to self; if necessary resizing is employed
      PROCEDURE   copyFrom(original:T_24BitImage; FilterType:FilterTypes); //copies complete data from original to self; if necessary resizing is employed
      PROCEDURE   flip;
      PROCEDURE   flop;
      PROCEDURE   rotLeft;
      PROCEDURE   rotRight;
      PROCEDURE   resize(newWidth,newHeight:longint; style:byte);
      FUNCTION    getHistogram(ht:T_histogramType):T_histogram;
      //PROCEDURE   applyCorrection(ai:T_aiState);
      FUNCTION    averageColor:T_floatColor;
      PROCEDURE   colorspaceHSV;
      PROCEDURE   colorspaceRGB;
      PROCEDURE   setThumbnail(index:longint; VAR img:T_24BitImage);
      PROCEDURE   getThumbnail(index:longint; OUT img:T_24BitImage);
  end;

  { T_ByteMap }

  T_ByteMap=object
    private
      xRes,yRes        :longint; //image dimensions
      pixelBuffer      :PByte;
      FUNCTION  getPixel(x,y:longint):byte;
      PROCEDURE setPixel(x,y:longint; color:byte);
    public
      CONSTRUCTOR create;
      CONSTRUCTOR create(newWidth,newHeight:longint); //same as: create; resize(newWidth,newHeight);
      CONSTRUCTOR create(name:string);                //same as: create; loadFromFile(name);
      CONSTRUCTOR createCopy(original:T_ByteMap);
      PROCEDURE   copyFrom(original:T_ByteMap);
      PROCEDURE   resizeDat(newWidth,newHeight:longint);
      DESTRUCTOR  destroy;
      PROCEDURE   saveToFile  (name:string);          //saves to file with chooseable name
      PROCEDURE   loadFromFile(name:string);          //loads from file and sets filename to name
      PROCEDURE   resize(newWidth,newHeight:longint);
      PROCEDURE   setToValue(c:byte);
      FUNCTION    countEqual(c:byte):longint;
      FUNCTION    countOdd  :longint;
      FUNCTION    rawData:PByte;                      //returns pointer to raw data (useful for display via OpenGL)
      FUNCTION    width :longint; //returns width of image (=xRes)
      FUNCTION    height:longint; //returns height of image (=yRes)
      FUNCTION    size  :longint;
      FUNCTION    diagonal:double;
      PROPERTY    pixel[x,y:longint]:byte read getPixel write setPixel; default; //pixel-wise access
      FUNCTION    getHistogram:T_histogram;
  end;

  T_floatMap=object
    private
      xRes,yRes        :longint; //image dimensions
      pixelBuffer      :P_floatColor;
      FUNCTION  getPixel(x,y:longint):T_floatColor;
      PROCEDURE setPixel(x,y:longint; color:T_floatColor);

    public
      CONSTRUCTOR create;
      CONSTRUCTOR create(newWidth,newHeight:longint); //same as: create; resize(newWidth,newHeight);
      CONSTRUCTOR create(name:string);                //same as: create; loadFromFile(name);
      CONSTRUCTOR createCopy(original:T_floatMap);
      DESTRUCTOR  destroy;
      FUNCTION    rawData:P_floatColor;                      //returns pointer to raw data (useful for display via OpenGL)
      FUNCTION    width :longint; //returns width of image (=xRes)
      FUNCTION    height:longint; //returns height of image (=yRes)
      FUNCTION    size  :longint;
      FUNCTION    diagonal:double;
      PROPERTY    pixel[x,y:longint]:T_floatColor read getPixel write setPixel; default; //pixel-wise access
      FUNCTION    incPixel(x,y:longint; color:T_floatColor; alpha:single):boolean; //returns true if hit
      FUNCTION    incPixel(x,y:longint; color:T_floatColor):boolean; //returns true if hit
      FUNCTION    getLinePtr(y:longint):pointer;                           //line wise access
      FUNCTION    getHistogram(ht:T_histogramType):T_histogram;
      PROCEDURE   resizeDat(newWidth,newHeight:longint); //resizes the image; WARNING!!! ALL DATA WILL BE LOST!
      PROCEDURE   resize    (newWidth,newHeight:longint);
      PROCEDURE   resize2   (newWidth,newHeight:longint);
      PROCEDURE   resize    (newWidth,newHeight:longint; style:byte);
      PROCEDURE   crop      (x0,x1,y0,y1:longint);
      PROCEDURE   cropResize(newWidth,newHeight:longint);
      PROCEDURE   copyFrom(original:T_FloatMap);
      PROCEDURE   copyFrom(original:T_24BitImage);
      PROCEDURE   copyTo  (VAR copyDest:T_24BitImage);
      //PROCEDURE   applyCorrection(ai:T_aiState);
      PROCEDURE   saveToFile  (name:string);
      PROCEDURE   saveSizeLimitedJpg(name:string);
      PROCEDURE   loadFromFile(name:string);
      PROCEDURE   multiplyWith(factor:T_Float);
      PROCEDURE   applyOffset(value:T_floatColor);
      PROCEDURE   colorspaceHSV;
      PROCEDURE   colorspaceRGB;
      PROCEDURE   threshold(x:T_Float);
      PROCEDURE   toAbsValue;

      PROCEDURE   flip;
      PROCEDURE   flop;
      PROCEDURE   rotLeft;
      PROCEDURE   rotRight;
  end;

VAR compressionQualityPercentage:longint=100;

FUNCTION calcErr(CONST c00,c01,c02,c10,c11,c12,c20,c21,c22:T_floatColor):double;
PROCEDURE markAlias_Gamma(VAR pic:T_FloatMap; VAR aaMask:T_byteMap; tolFak:single; verbosity:T_verbosity; OUT oldSpp,newSpp:double; OUT resampling:boolean);
FUNCTION currentSpp_gamma(VAR aaMask:T_byteMap):double;

FUNCTION tempName:string;
FUNCTION tempName(prefix:string):string;

PROCEDURE convertFile(inputName,outputName:string);
PROCEDURE convertFile(inputName,outputName:string; quality:longint);
PROCEDURE convertFileWithSizeLimit(inputName,outputName:string; limitInBytes:longint);

PROCEDURE saveCompound(VAR pic:T_floatMap; VAR mask:T_byteMap; filename:string);
PROCEDURE loadCompound(VAR pic:T_floatMap; VAR mask:T_byteMap; filename:string);


{$ifdef useExtensions}
  {$include extensions.inc}
  {$include extensions_filters.inc}
  {$include extensions_colors.inc}
  {$include extensions_combines.inc}
{$endif}
{$undef include_interface}

PROCEDURE resolutionOfImage(filename:string; OUT width,height:longint);
FUNCTION myTimeToStr(dt:double):string;
PROCEDURE rgbaSplit(VAR source:T_24BitImage; CONST transparentColor:T_floatColor; OUT rgbMap:T_24BitImage; OUT alphaMap:T_ByteMap);
PROCEDURE rgbaSplit(VAR source:T_floatMap;   CONST transparentColor:T_floatColor; OUT rgbMap:T_floatMap;   OUT alphaMap:T_ByteMap);
IMPLEMENTATION
{$define include_implementation}
{$include myColors.inc}
{$ifdef useExtensions}
  {$include extensions.inc}
  {$include extensions_filters.inc}
  {$include extensions_colors.inc}
  {$include extensions_combines.inc}
{$endif}
{$undef include_implementation}
PROCEDURE rgbToRGBA(CONST c00,c01,c02,
                          c10,c11,c12,
                          c20,c21,c22,
                          transparentColor:T_floatColor; OUT rgb:T_floatColor; OUT alpha:single);
  VAR aMax,a:single;
  begin
    aMax :=abs(c00[0]-transparentColor[0])+abs(c00[1]-transparentColor[1])+abs(c00[2]-transparentColor[2]);
    a    :=abs(c01[0]-transparentColor[0])+abs(c01[1]-transparentColor[1])+abs(c01[2]-transparentColor[2]); if a    >aMax then aMax:=a;
    a    :=abs(c02[0]-transparentColor[0])+abs(c02[1]-transparentColor[1])+abs(c02[2]-transparentColor[2]); if a    >aMax then aMax:=a;
    a    :=abs(c10[0]-transparentColor[0])+abs(c10[1]-transparentColor[1])+abs(c10[2]-transparentColor[2]); if a    >aMax then aMax:=a;
    alpha:=abs(c11[0]-transparentColor[0])+abs(c11[1]-transparentColor[1])+abs(c11[2]-transparentColor[2]); if alpha>aMax then aMax:=alpha;
    a    :=abs(c12[0]-transparentColor[0])+abs(c12[1]-transparentColor[1])+abs(c12[2]-transparentColor[2]); if a    >aMax then aMax:=a;
    a    :=abs(c20[0]-transparentColor[0])+abs(c20[1]-transparentColor[1])+abs(c20[2]-transparentColor[2]); if a    >aMax then aMax:=a;
    a    :=abs(c21[0]-transparentColor[0])+abs(c21[1]-transparentColor[1])+abs(c21[2]-transparentColor[2]); if a    >aMax then aMax:=a;
    a    :=abs(c22[0]-transparentColor[0])+abs(c22[1]-transparentColor[1])+abs(c22[2]-transparentColor[2]); if a    >aMax then aMax:=a;
    if aMax>1E-3 then begin
      alpha:=alpha/aMax;
      rgb:=transparentColor-(transparentColor-c11)*(1/alpha);
    end else begin
      rgb:=black;
      alpha:=0;
    end;
  end;

PROCEDURE rgbaSplit(VAR source:T_24BitImage; CONST transparentColor:T_floatColor; OUT rgbMap:T_24BitImage; OUT alphaMap:T_ByteMap);
  VAR x,y,xm,ym:longint;
      rgb:T_floatColor;
      alpha:single;
  begin
    xm:=source.width -1;
    ym:=source.height-1;
    rgbMap  .create(source.width,source.height);
    alphaMap.create(source.width,source.height);
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
      rgbMap  [x,y]:=rgb;
      alphaMap[x,y]:=round(max(0,min(1,alpha))*255);
    end;
  end;

PROCEDURE rgbaSplit(VAR source:T_floatMap; CONST transparentColor:T_floatColor; OUT rgbMap:T_floatMap; OUT alphaMap:T_ByteMap);
  VAR x,y,xm,ym:longint;
      rgb:T_floatColor;
      alpha:single;
  begin
    xm:=source.width -1;
    ym:=source.height-1;
    rgbMap  .create(source.width,source.height);
    alphaMap.create(source.width,source.height);
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
      rgbMap  [x,y]:=rgb;
      alphaMap[x,y]:=round(max(0,min(1,alpha))*255);
    end;
  end;

FUNCTION calcErr(CONST c00,c01,c02,c10,c11,c12,c20,c21,c22:T_floatColor):double; inline;
  begin
    result:=10*(sqr(c11[0]-0.166666666666667*(c00[0]+c01[0]+c02[0]+c10[0])-0.0833333333333333*(c12[0]+c20[0]+c21[0]+c22[0]))
               +sqr(c11[1]-0.166666666666667*(c00[1]+c01[1]+c02[1]+c10[1])-0.0833333333333333*(c12[1]+c20[1]+c21[1]+c22[1]))
               +sqr(c11[2]-0.166666666666667*(c00[2]+c01[2]+c02[2]+c10[2])-0.0833333333333333*(c12[2]+c20[2]+c21[2]+c22[2])));
  end;

PROCEDURE markAlias_Gamma(VAR pic:T_FloatMap; VAR aaMask:T_byteMap; tolFak:single; verbosity:T_verbosity; OUT oldSpp,newSpp:double; OUT resampling:boolean);
  VAR x,y,xr,yr:longint;
      x2,y2:longint;
      ptIn:P_floatColor;
      ptM :PBYte;
      refineCount:longint;
      maxRefine:byte=0;
      localError:double;
      localTol:single;
      //maxTol:single;
  begin
    oldSpp:=0;
    newSpp:=0;
    tolFak:=tolFak;
    //maxTol:=tolFak*2;
    //if maxTol<0.2 then maxTol:=0.2;
    resampling:=false;
    ptIn:=pic   .rawData;
    ptM :=aaMask.rawData;
    xr:=pic.width;
    yr:=pic.height;
    refineCount:=0;
    for y:=0 to yr-1 do for x:=0 to xr-1 do begin
      if ptM[x+y*xr]=0 then oldSpp:=oldSpp+1
                       else oldSpp:=oldSpp+2*ptM[x+y*xr];
    end;
    for y:=1 to yr-2 do for x:=1 to xr-2 do begin
      localTol:=(ptM[x+y*xr]/254);
      //localTol:=tolFak+localTol*localTol*(maxTol-tolFak);
      localTol:=(1+localTol*localTol)*tolFak;
      localError:=calcErr(ptIn[x+y*xr-xr-1],ptIn[x+y*xr-xr],ptIn[x+y*xr-xr+1],
                          ptIn[x+y*xr   -1],ptIn[x+y*xr   ],ptIn[x+y*xr   +1],
                          ptIn[x+y*xr+xr-1],ptIn[x+y*xr+xr],ptIn[x+y*xr+xr+1]);
      //if ptM[x+y*xr]>refineCap then localError:=exp(-0.2*(ptM[x+y*xr]-refineCap))*localError;

      if localError>localTol then begin
        for y2:=y-1 to y+1 do for x2:=x-1 to x+1 do if not(odd(ptM[x2+y2*xr])) and (ptM[x2+y2*xr]<254) then begin
          ptM[x2+y2*xr]:=ptM[x2+y2*xr]+1;
          inc(refineCount);
          resampling:=true;
        end;
      end;
    end;
    for y:=0 to yr-1 do for x:=0 to xr-1 do begin
      if ptM[x+y*xr]=0 then newSpp:=newSpp+1
                       else begin
        newSpp:=newSpp+2*((ptM[x+y*xr]+1) and 254);
        if ptM[x+y*xr]+1>maxRefine then maxRefine:=ptM[x+y*xr]+1;
      end;
    end;
    oldSpp:=oldSpp/xr/yr;
    newSpp:=newSpp/xr/yr;
    if verbosity<>quiet then begin
      if (refineCount>99999) //too long in pixels
      or (refineCount/xr*100.0/yr>1) //more than 1%
        then write(refineCount/xr*100.0/yr:6:2,'% spp=',newSpp:7:3,'(',(maxRefine and 254)*2:3,')')
        else write(refineCount            :5, 'px spp=',newSpp:7:3,'(',(maxRefine and 254)*2:3,')');
      if verbosity=outputWithLineBreak then writeln;
    end;
  end;

FUNCTION currentSpp_gamma(VAR aaMask:T_byteMap):double;
  VAR x,y:longint;
  begin
    result:=0;
    for y:=0 to aaMask.height-1 do for x:=0 to aaMask.width-1 do begin
      if aaMask[x,y]=0 then result:=result+1
                       else result:=result+2*aaMask[x,y];
    end;
    result:=result/aaMask.width/aaMask.height;
  end;

PROCEDURE resolutionOfImage(filename:string; OUT width,height:longint);
  VAR wand: PMagickWand;
      pName:PChar;
      temp :T_24BitImage;
  begin
    if (ExtractFileExt(filename)='.vraw') then begin
      temp.create(filename);
      width:=temp.width;
      height:=temp.height;
      temp.destroy;
    end else begin

      pname:=strAlloc(length(filename)+1);
      strPCopy(pname,filename);
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickReadImage(wand,pName);
      width:=MagickGetImageWidth(wand);
      height:=MagickGetImageHeight(wand);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
      strDispose(pname);
    end;
  end;


FUNCTION tempName:string;
  VAR i:longint;
  begin
    i:=-1;
    repeat
      inc(i);
      result:=intToStr(i);
      while length(result)<5 do result:='0'+result;
      result:='temp'+result+'.bmp';
    until not(FileExists(result));
  end;

FUNCTION tempName(prefix:string):string;
  VAR i:longint;
  begin
    i:=-1;
    repeat
      inc(i);
      result:=intToStr(i);
      while length(result)<5 do result:='0'+result;
      result:=prefix+result+'.bmp';
    until not(FileExists(result));
  end;

FUNCTION meanCol(c1,c2      :T_24Bit):T_24Bit; inline;
  begin
    result[0]:=(c1[0]+c2[0]) shr 1;
    result[1]:=(c1[1]+c2[1]) shr 1;
    result[2]:=(c1[2]+c2[2]) shr 1;
  end;

FUNCTION meanCol(c1,c2,c3,c4:T_24Bit):T_24Bit; inline;
  begin
    result[0]:=(c1[0]+c2[0]+c3[0]+c4[0]) shr 2;
    result[1]:=(c1[1]+c2[1]+c3[1]+c4[1]) shr 2;
    result[2]:=(c1[2]+c2[2]+c3[2]+c4[2]) shr 2;
  end;

FUNCTION pixelIsValid(c:T_24Bit):T_Float;
  begin
    result:=1-abs(c[0]-127.5)*abs(c[1]-127.5)*abs(c[2]-127.5)*4.8246903528808678E-7;
  end;

FUNCTION colWSum(c1:T_24Bit; w1:T_Float;
                 c2:T_24Bit; w2:T_Float):T_24Bit;
  begin
    result[0]:=round(c1[0]*w1+c2[0]*w2);
    result[1]:=round(c1[1]*w1+c2[1]*w2);
    result[2]:=round(c1[2]*w1+c2[2]*w2);
  end;

FUNCTION colWSum(c1:T_24Bit; w1:T_Float;
                 c2:T_24Bit; w2:T_Float;
                 c3:T_24Bit; w3:T_Float):T_24Bit;
  begin
    result[0]:=round(c1[0]*w1+c2[0]*w2+c3[0]*w3);
    result[1]:=round(c1[1]*w1+c2[1]*w2+c3[1]*w3);
    result[2]:=round(c1[2]*w1+c2[2]*w2+c3[2]*w3);
  end;

FUNCTION colWSum(c1:T_24Bit; w1:T_Float;
                 c2:T_24Bit; w2:T_Float;
                 c3:T_24Bit; w3:T_Float;
                 c4:T_24Bit; w4:T_Float):T_24Bit;
  begin
    result[0]:=round(c1[0]*w1+c2[0]*w2+c3[0]*w3+c4[0]*w4);
    result[1]:=round(c1[1]*w1+c2[1]*w2+c3[1]*w3+c4[1]*w4);
    result[2]:=round(c1[2]*w1+c2[2]*w2+c3[2]*w3+c4[2]*w4);
  end;

FUNCTION brightness(x:T_floatColor):T_Float;
  begin
    result:=(x[0]+x[1]+x[2])/3;
  end;



PROCEDURE convertFile(inputName,outputName:string);
  VAR wand: PMagickWand;
      pName:PChar;
      temp :T_24BitImage;
  begin
    if (ExtractFileExt(inputName )='.vraw' ) or
       (ExtractFileExt(outputName)='.vraw' ) then begin
      temp.create    (inputName);
      temp.saveToFile(outputName);
      temp.destroy;
    end else begin
      {$ifndef naked}
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      //reading:---------------------------
      pname:=strAlloc(length(inputName)+1);
      strPCopy(pname,inputName);
      MagickReadImage(wand,pName);
      strDispose(pname);
      //---------------------------:reading
      //writing:---------------------------
      pname:=strAlloc(length(outputName)+1);
      strPCopy(pname,outputName);
      MagickWriteImages(wand,pName,MagickFalse);
      strDispose(pname);
      //---------------------------:writing
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$endif}
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end;
  end;

PROCEDURE convertFile(inputName,outputName:string;quality:longint);
  VAR wand: PMagickWand;
      pName:PChar;
      temp :T_24BitImage;
  begin
    if (ExtractFileExt(inputName )='.vraw' ) or
       (ExtractFileExt(outputName)='.vraw' ) then begin
      temp.create    (inputName);
      temp.saveToFile(outputName);
      temp.destroy;
    end else begin
      {$ifndef naked}
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      //reading:---------------------------
      pname:=strAlloc(length(inputName)+1);
      strPCopy(pname,inputName);
      MagickReadImage(wand,pName);
      strDispose(pname);
      //---------------------------:reading
      //writing:---------------------------
      pname:=strAlloc(length(outputName)+1);
      strPCopy(pname,outputName);
      MagickSetImageCompressionQuality(wand,quality);
      MagickWriteImages(wand,pName,MagickFalse);
      strDispose(pname);
      //---------------------------:writing
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$endif}
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end;
  end;

PROCEDURE convertFileWithSizeLimit(inputName,outputName:string; limitInBytes:longint);
  VAR wand: PMagickWand;
      pName:PChar;
      temp :T_24BitImage;
      quality:longint=100;
      newFileSize:longint;
      oldFileSize:longint=1;
  FUNCTION fileSize(name:string):longint;
    VAR s:TSearchRec;
    begin
      if FindFirst(name,faAnyFile,s)=0
        then result:=s.Size
        else result:=0;
      FindClose(s);
    end;

  begin
    if (ExtractFileExt(inputName )='.vraw' ) or
       (ExtractFileExt(outputName)='.vraw' ) then begin
      temp.create    (inputName);
      temp.saveToFile(outputName);
      temp.destroy;
    end else begin
      {$ifndef naked}
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      //reading:---------------------------
      pname:=strAlloc(length(inputName)+1);
      strPCopy(pname,inputName);
      MagickReadImage(wand,pName);
      strDispose(pname);
      //---------------------------:reading
      //writing:---------------------------
      pname:=strAlloc(length(outputName)+1);
      strPCopy(pname,outputName);
      quality:=100;
      repeat
        MagickSetImageCompressionQuality(wand,quality);
        MagickWriteImages(wand,pName,MagickFalse);
        newFileSize:=filesize(outputName);
        if quality=100 then oldFileSize:=newFileSize;
        writeln('Quality:',quality:3,'%; size:',newFileSize*100.0/oldFileSize:6:2,'%=',newFileSize:12,'bytes');
        dec(quality,20);

      until (newFileSize<limitInBytes) or (quality<=0);
      inc(quality,20+20-4); if quality>100 then quality:=100;
      repeat
        MagickSetImageCompressionQuality(wand,quality);
        MagickWriteImages(wand,pName,MagickFalse);
        newFileSize:=filesize(outputName);
        writeln('Quality:',quality:3,'%; size:',newFileSize*100.0/oldFileSize:6:2,'%=',newFileSize:12,'bytes');
        dec(quality,4);
      until (newFileSize<limitInBytes) or (quality<=0);
      inc(quality,4+4-1); if (quality>100) then quality:=100;
      repeat
        MagickSetImageCompressionQuality(wand,quality);
        MagickWriteImages(wand,pName,MagickFalse);
        newFileSize:=filesize(outputName);
        writeln('Quality:',quality:3,'%; size:',newFileSize*100.0/oldFileSize:6:2,'%=',newFileSize:12,'bytes');
        dec(quality);
      until (newFileSize<limitInBytes) or (quality<=0);
      strDispose(pname);
      //---------------------------:writing
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$endif}
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end;
  end;


//================================================================================
//bitmapFile24Bit:----------------------------------------------------------------

CONSTRUCTOR T_24BitImage.create; begin xRes:=0; yRes:=0; pixelBuffer:=nil; end;
CONSTRUCTOR T_ByteMap   .create; begin xRes:=0; yRes:=0; pixelBuffer:=nil; end;
CONSTRUCTOR T_floatMap  .create; begin xRes:=0; yRes:=0; pixelBuffer:=nil; end;

CONSTRUCTOR T_24BitImage.create(newWidth,newHeight:longint); begin xRes:=newWidth; yRes:=newHeight; getMem(pixelBuffer,xRes*yRes*3);              end;
CONSTRUCTOR T_ByteMap   .create(newWidth,newHeight:longint); begin xRes:=newWidth; yRes:=newHeight; getMem(pixelBuffer,xRes*yRes);                end;
CONSTRUCTOR T_floatMap  .create(newWidth,newHeight:longint); begin xRes:=newWidth; yRes:=newHeight; getMem(pixelBuffer,xRes*yRes*sizeOf(T_floatColor)); end;

DESTRUCTOR T_24BitImage.destroy; begin if xRes*yRes>0 then freeMem(pixelBuffer,xRes*yRes*3);               xRes:=0; yRes:=0; end;
DESTRUCTOR T_floatMap  .destroy; begin if xRes*yRes>0 then freeMem(pixelBuffer,xRes*yRes*sizeOf(T_floatColor));  xRes:=0; yRes:=0; end;
DESTRUCTOR T_ByteMap   .destroy; begin if xRes*yRes>0 then freeMem(pixelBuffer,xRes*yRes);                 xRes:=0; yRes:=0; end;

FUNCTION T_24BitImage.getPixel(x,y:longint):T_24Bit; begin if (y<0) or (y>=yRes) or (x<0) or (x>=xRes) then result:=black24Bit else result:=pixelBuffer[x+(yRes-1-y)*xRes]; end;
FUNCTION T_ByteMap   .getPixel(x,y:longint):byte;    begin if (y<0) or (y>=yRes) or (x<0) or (x>=xRes) then result:=255        else result:=pixelBuffer[x+(yRes-1-y)*xRes]; end;
FUNCTION T_FloatMap  .getPixel(x,y:longint):T_floatColor;  begin if (y<0) or (y>=yRes) or (x<0) or (x>=xRes) then result:=black      else result:=pixelBuffer[x+(yRes-1-y)*xRes]; end;

PROCEDURE T_24BitImage.setPixel(x,y:longint; color:T_24Bit); begin if not((y<0) or (y>=yRes) or (x<0) or (x>=xRes)) then pixelBuffer[x+(yRes-1-y)*xRes]:=color; end;
PROCEDURE T_ByteMap   .setPixel(x,y:longint; color:byte);    begin if not((y<0) or (y>=yRes) or (x<0) or (x>=xRes)) then pixelBuffer[x+(yRes-1-y)*xRes]:=color; end;
PROCEDURE T_FloatMap  .setPixel(x,y:longint; color:T_floatColor);  begin if not((y<0) or (y>=yRes) or (x<0) or (x>=xRes)) then pixelBuffer[x+(yRes-1-y)*xRes]:=color; end;

FUNCTION T_FloatMap  .incPixel(x,y:longint; color:T_floatColor; alpha:single):boolean;
  begin
    if not((y<0) or (y>=yRes) or (x<0) or (x>=xRes)) then begin
      x:=x+(yRes-1-y)*xRes;
      pixelBuffer[x]:=pixelBuffer[x]*(1-alpha)+color*alpha;
      result:=true;
    end else result:=false;
  end;

FUNCTION  T_FloatMap  .incPixel(x,y:longint; color:T_floatColor):boolean;
  begin
    if not((y<0) or (y>=yRes) or (x<0) or (x>=xRes)) then begin
      x:=x+(yRes-1-y)*xRes;
      pixelBuffer[x]:=pixelBuffer[x]+color;
      result:=true;
    end else result:=false;
  end;


FUNCTION T_24BitImage.width :longint; begin result:=xRes; end;
FUNCTION T_ByteMap   .width :longint; begin result:=xRes; end;
FUNCTION T_FloatMap  .width :longint; begin result:=xRes; end;

FUNCTION T_24BitImage.height:longint; begin result:=yRes; end;
FUNCTION T_ByteMap   .height:longint; begin result:=yRes; end;
FUNCTION T_FloatMap  .height:longint; begin result:=yRes; end;

FUNCTION T_24BitImage.size:longint; begin result:=xRes*yRes; end;
FUNCTION T_ByteMap   .size:longint; begin result:=xRes*yRes; end;
FUNCTION T_FloatMap  .size:longint; begin result:=xRes*yRes; end;

FUNCTION T_24BitImage.rawData:P_24Bit; begin result:=pixelBuffer; end;
FUNCTION T_ByteMap   .rawData:PByte;   begin result:=pixelBuffer; end;
FUNCTION T_FloatMap  .rawData:P_floatColor;  begin result:=pixelBuffer; end;

CONSTRUCTOR T_24BitImage.createCopy(original:T_24BitImage); begin xRes:=original.width; yRes:=original.height; getMem(pixelBuffer,xRes*yRes*sizeOf(T_24Bit)); move(original.pixelBuffer^,pixelBuffer^,xRes*yRes*sizeOf(T_24Bit)) end;
CONSTRUCTOR T_FloatMap  .createCopy(original:T_FloatMap  ); begin xRes:=original.width; yRes:=original.height; getMem(pixelBuffer,xRes*yRes*sizeOf(T_floatColor )); move(original.pixelBuffer^,pixelBuffer^,xRes*yRes*sizeOf(T_floatColor )) end;
CONSTRUCTOR T_ByteMap   .createCopy(original:T_ByteMap);    begin xRes:=original.width; yRes:=original.height; getMem(pixelBuffer,xRes*yRes                ); move(original.pixelBuffer^,pixelBuffer^,xRes*yRes                ) end;

CONSTRUCTOR T_24BitImage.create(name:string); begin xRes:=0; yRes:=0; pixelBuffer:=nil; loadFromFile(name); end;
CONSTRUCTOR T_FloatMap.  create(name:string); begin xRes:=0; yRes:=0; pixelBuffer:=nil; loadFromFile(name); end;
CONSTRUCTOR T_ByteMap.   create(name:string); begin xRes:=0; yRes:=0; pixelBuffer:=nil; loadFromFile(name); end;

PROCEDURE T_24BitImage.colorspaceHSV; VAR i:longint; begin for i:=0 to size-1 do pixelBuffer[i]:=toHSV(pixelBuffer[i]); end;
PROCEDURE T_FloatMap  .colorspaceHSV; VAR i:longint; begin for i:=0 to size-1 do pixelBuffer[i]:=toHSV(pixelBuffer[i]); end;

PROCEDURE T_24BitImage.colorspaceRGB; VAR i:longint; begin for i:=0 to size-1 do pixelBuffer[i]:=fromHSV(pixelBuffer[i]); end;
PROCEDURE T_FloatMap  .colorspaceRGB; VAR i:longint; begin for i:=0 to size-1 do pixelBuffer[i]:=fromHSV(pixelBuffer[i]); end;

FUNCTION T_ByteMap   .getHistogram:T_histogram;
  VAR i:longint;
  begin
    for i:=0 to 255 do result[i]:=0;
    for i:=0 to xRes*yREs-1 do finc(result[pixelBuffer[i]]);
  end;

FUNCTION T_24BitImage.getHistogram(ht:T_histogramType):T_histogram;
  VAR i,j:longint;
      aid:T_Float;
  begin
    for j:=0 to 255 do result[j]:=0;
    case ht of
      ht_full        : for i:=0 to xRes*yRes-1 do begin j:=pixelBuffer[i,0]; result[j]:=result[j]+1;
                                                        j:=pixelBuffer[i,1]; result[j]:=result[j]+1;
                                                        j:=pixelBuffer[i,2]; result[j]:=result[j]+1; end;
      ht_greyLevel   : for i:=0 to xRes*yRes-1 do begin
                         j:=(pixelBuffer[i,0]+
                             pixelBuffer[i,1]+
                             pixelBuffer[i,2]) div 3;
                         result[j]:=result[j]+1;
                       end;
      ht_redChannel  : for i:=0 to xRes*yRes-1 do begin j:=pixelBuffer[i,0]; result[j]:=result[j]+1; end;
      ht_greenChannel: for i:=0 to xRes*yRes-1 do begin j:=pixelBuffer[i,1]; result[j]:=result[j]+1; end;
      ht_blueChannel : for i:=0 to xRes*yRes-1 do begin j:=pixelBuffer[i,2]; result[j]:=result[j]+1; end;
    end;
    if ht=ht_full then aid:=1/(3*xRes*yRes)
                  else aid:=1/(  xRes*yRes);
    for i:=0 to 255 do result[i]:=result[i]*aid;
  end;

FUNCTION T_FloatMap.getHistogram(ht:T_histogramType):T_histogram;
  PROCEDURE smoothInc(VAR h:T_histogram; index:single); inline;
    begin
      index:=index*255;
      if      index<0   then h[0  ]:=h[0  ]+1
      else if index>255 then h[255]:=h[255]+1
      else begin
        h[trunc(index)  ]:=h[trunc(index)  ]+(1-frac(index));
        h[trunc(index)+1]:=h[trunc(index)+1]+   frac(index) ;
      end;
    end;

  VAR i:longint;
      aid:T_Float;
  begin
    for i:=0 to 255 do result[i]:=0;
    case ht of
      ht_full        : for i:=0 to xRes*yRes-1 do begin smoothInc(result,pixelBuffer[i,0]);
                                                        smoothInc(result,pixelBuffer[i,1]);
                                                        smoothInc(result,pixelBuffer[i,2]); end;
      ht_greyLevel   : for i:=0 to xRes*yRes-1 do
                         smoothInc(result,(pixelBuffer[i,0]+
                                           pixelBuffer[i,1]+
                                           pixelBuffer[i,2])/3);

      ht_redChannel  : for i:=0 to xRes*yRes-1 do smoothInc(result,pixelBuffer[i,0]);
      ht_greenChannel: for i:=0 to xRes*yRes-1 do smoothInc(result,pixelBuffer[i,1]);
      ht_blueChannel : for i:=0 to xRes*yRes-1 do smoothInc(result,pixelBuffer[i,2]);
    end;
    if ht=ht_full then aid:=1/(3*xRes*yRes)
                  else aid:=1/(  xRes*yRes);
    for i:=0 to 255 do result[i]:=result[i]*aid;
  end;

PROCEDURE T_24BitImage.resizeDat(newWidth,newHeight:longint);
  begin
    if (newWidth<>xRes) or (newHeight<>yRes) then begin
      if xRes*yRes<>0 then freeMem(pixelBuffer,xRes*yRes*sizeOf(T_24Bit));
      xRes:=newWidth;
      yRes:=newHeight;
      getMem(pixelBuffer,xRes*yRes*sizeOf(T_24Bit));
    end;
  end;

PROCEDURE T_ByteMap.resizeDat(newWidth,newHeight:longint); //resizes the image; WARNING!!! ALL DATA WILL BE LOST!
  begin
    if (newWidth<>xRes) or (newHeight<>yRes) then begin
      if xRes*yRes>0 then freeMem(pixelBuffer,xRes*yRes);
      xRes:=newWidth;
      yRes:=newHeight;
      getMem(pixelBuffer,xRes*yRes);
    end;
  end;

PROCEDURE T_FloatMap.resizeDat(newWidth,newHeight:longint);
  begin
    if (newWidth<>xRes) or (newHeight<>yRes) then begin
      if xRes*yRes<>0 then freeMem(pixelBuffer,xRes*yRes*sizeOf(T_floatColor));
      xRes:=newWidth;
      yRes:=newHeight;
      getMem(pixelBuffer,xRes*yRes*sizeOf(T_floatColor));
    end;
  end;

PROCEDURE T_24BitImage.copyFrom(original:T_24BitImage);
  VAR wand: PMagickWand;
  begin
    if (original.width=xRes) and (original.height=yRes) then
      move(original.pixelBuffer^,pixelBuffer^,xRes*yRes*3)
    else begin
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,original.width,original.height,'RGB', CharPixel,original.pixelBuffer);
      MagickResizeImage(wand, xRes,yRes, LanczosFilter, 1.0);
      MagickGetImagePixels(wand,0,0,xRes,yRes,'RGB',CharPixel,pixelBuffer);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end;
  end;

PROCEDURE T_FloatMap.copyFrom(original:T_FloatMap);
//  begin
//    if (original.width=xRes) and (original.height=yRes) then
//      move(original.pixelBuffer^,pixelBuffer^,xRes*yRes*sizeOf(T_floatColor));
//  end;
  VAR wand: PMagickWand;
  begin
    if (original.width=xRes) and (original.height=yRes) then
      move(original.pixelBuffer^,pixelBuffer^,xRes*yRes*sizeOf(T_floatColor))
    else begin
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,original.width,original.height,'RGB', FloatPixel,original.pixelBuffer);
      MagickResizeImage(wand, xRes,yRes, LanczosFilter, 1.0);
      MagickGetImagePixels(wand,0,0,xRes,yRes,'RGB',FloatPixel,pixelBuffer);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end;
  end;


PROCEDURE T_FloatMap.copyFrom(original:T_24BitImage);
  VAR i:longint;
  begin
    resizeDat(original.width,original.height);
    for i:=0 to xRes*yRes-1 do pixelbuffer[i]:=original.pixelBuffer[i];
  end;

PROCEDURE T_byteMap.copyFrom(original:T_ByteMap);
  begin
    resizeDat(original.width,original.height);
    move(original.pixelBuffer^,pixelBuffer^,xRes*yRes);
  end;

PROCEDURE T_FloatMap.copyTo  (VAR copyDest:T_24BitImage);
  VAR i:longint;
  begin
    copyDest.resizeDat(xRes,yRes);
    for i:=0 to xRes*yRes-1 do copyDest.pixelbuffer[i]:=projectedColor(pixelBuffer[i]);
  end;


PROCEDURE T_24BitImage.saveToFile(name:string);
  {$ifndef naked}
  VAR wand: PMagickWand;
      pName:PChar;
  {$endif}
  VAR f:T_File;
  begin
    if ExtractFileExt(name)='.vraw' then begin
      f.createToWrite(name);
      f.writelongint(xRes);
      f.writelongint(yRes);
      f.writeBoolean(false);
      f.writeBuf(PByte(pixelBuffer),3*xRes*yRes);
      f.destroy;
    end else begin
      {$ifndef naked}
      pname:=strAlloc(length(name)+1);
      strPCopy(pname,name);
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,xRes,yRes,'RGB', CharPixel,pixelBuffer);
      MagickFlipImage(wand);
      MagickSetImageCompressionQuality(wand,100);
      MagickSetImageCompressionQuality(wand,compressionQualityPercentage);
      MagickWriteImages(wand,pName,MagickFalse);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
      strDispose(pname);
      {$endif}
    end;
  end;

PROCEDURE T_24BitImage.saveSizeLimitedJpg(name:string);
  {$ifndef naked}
  VAR wand: PMagickWand;
      pName:PChar;
  {$endif}
  VAR quality:longint;
      oldFileSize,newFileSize:longint;
      limitInBytes:longint;

  FUNCTION fileSize(name:string):longint;
    VAR s:TSearchRec;
    begin
      if FindFirst(name,faAnyFile,s)=0
        then result:=s.Size
        else result:=0;
      FindClose(s);
    end;

  begin
    if uppercase(ExtractFileExt(name))='.JPG' then begin
      {$ifndef naked}
      pname:=strAlloc(length(name)+1);
      strPCopy(pname,name);
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,xRes,yRes,'RGB', CharPixel,pixelBuffer);
      MagickFlipImage(wand);
      limitInBytes:=round(1677*sqrt(xRes*yRes));
      quality:=100;
      repeat
        MagickSetImageCompressionQuality(wand,quality);
        MagickWriteImages(wand,pName,MagickFalse);
        newFileSize:=filesize(name);
        if quality=100 then oldFileSize:=newFileSize;
        writeln('Quality:',quality:3,'%; size:',newFileSize*100.0/oldFileSize:6:2,'%=',newFileSize:12,'bytes');
        dec(quality,20);
      until (newFileSize<limitInBytes) or (quality<=0);
      inc(quality,20+20-4); if quality>100 then begin quality:=100; end;
      repeat
        MagickSetImageCompressionQuality(wand,quality);
        MagickWriteImages(wand,pName,MagickFalse);
        newFileSize:=filesize(name);
        writeln('Quality:',quality:3,'%; size:',newFileSize*100.0/oldFileSize:6:2,'%=',newFileSize:12,'bytes');
        dec(quality,4)
      until (newFileSize<limitInBytes) or (quality<=0);
      inc(quality,4+4-1); if (quality>100) then begin quality:=100;  end;
      repeat
        MagickSetImageCompressionQuality(wand,quality);
        MagickWriteImages(wand,pName,MagickFalse);
        newFileSize:=filesize(name);
        writeln('Quality:',quality:3,'%; size:',newFileSize*100.0/oldFileSize:6:2,'%=',newFileSize:12,'bytes');
        dec(quality);
      until (newFileSize<limitInBytes) or (quality<=0);
      strDispose(pname);
      //---------------------------:writing
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$endif}
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end else saveToFile(name);
  end;

PROCEDURE T_floatMap.saveSizeLimitedJpg(name:string);
  VAR f:T_File;
      temp:T_24BitImage;
  begin
    if ExtractFileExt(name)='.vraw' then begin
      f.createToWrite(name);
      f.writelongint(xRes);
      f.writelongint(yRes);
      f.writeBoolean(true);
      f.writeBuf(PByte(pixelBuffer),sizeOf(T_floatColor)*xRes*yRes);
      f.destroy;
    end else begin
      temp.create(xRes,yRes);
      copyTo(temp);
      temp.saveSizeLimitedJpg(name);
      temp.destroy;
    end;
  end;

PROCEDURE T_FloatMap.saveToFile(name:string);
  VAR f:T_File;
      temp:T_24BitImage;
  begin
    if ExtractFileExt(name)='.vraw' then begin
      f.createToWrite(name);
      f.writelongint(xRes);
      f.writelongint(yRes);
      f.writeBoolean(true);
      f.writeBuf(PByte(pixelBuffer),sizeOf(T_floatColor)*xRes*yRes);
      f.destroy;
    end else begin
      temp.create(xRes,yRes);
      copyTo(temp);
      temp.saveToFile(name);
      temp.destroy;
    end;
  end;

PROCEDURE saveCompound(VAR pic:T_floatMap; VAR mask:T_byteMap; filename:string);
  VAR f:T_File;
  begin
    f.createToWrite(filename);
    f.writelongint(pic.width);
    f.writelongint(pic.height);
    f.writeBoolean(true);
    f.writeBuf(PByte(pic.pixelBuffer),sizeOf(T_floatColor)*pic.size);
    f.writeBuf(mask.pixelBuffer,sizeOf(byte)*mask.size);
    f.destroy;
  end;

PROCEDURE loadCompound(VAR pic:T_floatMap; VAR mask:T_byteMap; filename:string);
  VAR f:T_File;
      xRes,yRes:longint;
  begin
    f.createToRead(filename);
    xRes:=f.readlongint;
    yRes:=f.readlongint;
    if f.readBoolean then begin
      pic.resizeDat(xRes,yRes);
      f.readBuf(PByte(pic.pixelBuffer),sizeOf(T_floatColor)*xRes*yRes);
      mask.resizeDat(xRes,yRes);
      f.readBuf(mask.pixelBuffer,sizeOf(byte)*xRes*yRes);
    end;
    f.destroy;
  end;


PROCEDURE T_ByteMap.saveToFile(name:string);
 {$ifndef naked}
  VAR wand: PMagickWand;
      pName:PChar;
  {$endif}
  VAR f:T_File;
      v:P_24Bit;
      x,y:longint;
  begin
    if ExtractFileExt(name)='.vraw' then begin
      f.createToWrite(name);
      f.writelongint(xRes);
      f.writelongint(yRes);
      f.writeBoolean(false);
      getMem(v,xres*sizeOf(T_24Bit));
      for y:=0 to yres-1 do begin
        for x:=0 to xRes-1 do begin
          v[x,0]:=pixelBuffer[x+y*xRes];
          v[x,1]:=pixelBuffer[x+y*xRes];
          v[x,2]:=pixelBuffer[x+y*xRes];
        end;
        f.writeBuf(PByte(v),3*xRes);
      end;
      freeMem(v,xres*sizeOf(T_24Bit));
      f.destroy;
    end else begin
      {$ifndef naked}
      pname:=strAlloc(length(name)+1);
      strPCopy(pname,name);
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,xRes,yRes,'I', CharPixel,pixelBuffer);
      MagickFlipImage(wand);
      MagickSetImageCompressionQuality(wand,100);
      MagickSetImageCompressionQuality(wand,compressionQualityPercentage);
      MagickWriteImages(wand,pName,MagickFalse);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
      strDispose(pname);
      {$endif}
    end;
  end;

PROCEDURE T_24BitImage.loadFromFile(name:string);
  {$ifndef naked}
  VAR wand: PMagickWand;
      pName:PChar;
  {$endif}
  VAR f:T_File;
      i,j:longint;
      v:P_floatColor;
  begin
    if ExtractFileExt(name)='.vraw' then begin
      f.createToRead(name);
      i:=f.readlongint;
      j:=f.readlongint;
      resizeDat(i,j);
      if f.readBoolean then begin
        getMem(v,sizeOf(T_floatColor)*xRes);
        for i:=0 to yRes-1 do begin
          f.readBuf(PByte(v),sizeOf(T_floatColor)*xRes);
          for j:=0 to xRes-1 do pixelBuffer[i*xRes+j]:=v[j];
        end;
        freeMem(v,sizeOf(T_floatColor)*xRes);
      end else begin
        f.readBuf(PByte(pixelBuffer),3*xRes*yRes);
      end;
      f.destroy;
    end else begin
      pname:=strAlloc(length(name)+1);
      strPCopy(pname,name);
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickReadImage(wand,pName);
      MagickFlipImage(wand);
      resizeDat(MagickGetImageWidth(wand),MagickGetImageHeight(wand));
      MagickGetImagePixels(wand,0,0,xRes,yRes,'RGB',CharPixel,PByte(pixelBuffer));
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
      strDispose(pname);
    end;
  end;

PROCEDURE T_FloatMap.loadFromFile(name:string);
  PROCEDURE loadCSV(filename:ansistring);
    VAR f:text;
        i,j,k:longint;
        nextline,nextItem:ansistring;
        rowCount,colCount:longint;
    begin

      if fileExists(filename) then try
        assign(f,filename);
        reset(f);
        i:=0;
        colCount:=0; rowCount:=0;
        while not(eof(f)) do begin
          readln(f,nextLine);
          nextLine:=trim(nextline);
          j:=0;
          while length(nextLine)>0 do begin
            k:=pos(';',nextLine);
            if k<=0 then begin
              nextItem:=trim(nextLine);
              nextLine:='';
            end else begin
              nextitem:=trim(copy(nextLine,1,k-1));
              nextLine:=trim(copy(nextLine,k+1,length(nextLine)));
            end;
            inc(j);
            if j>colCount then colCount:=j;
          end;
          inc(i); rowCount:=i;
        end;
        close(f);


        resizeDat(colCount,rowCount);


        reset(f);
        i:=0;
        while not(eof(f)) do begin
          readln(f,nextLine);
          nextLine:=trim(nextline);
          j:=0;
          while length(nextLine)>0 do begin
            k:=pos(';',nextLine);
            if k<=0 then begin
              nextItem:=trim(nextLine);
              nextLine:='';
            end else begin
              nextitem:=trim(copy(nextLine,1,k-1));
              nextLine:=trim(copy(nextLine,k+1,length(nextLine)));
            end;
            pixel[j,i]:=white*strToFloatDef(nextItem,0);
            inc(j);
          end;
          inc(i);
        end;
        close(f);

      except
      end;
    end;


  VAR wand: PMagickWand;
      pName:PChar;
      f:T_File;
      v:P_24Bit;
      i,j:longint;
  begin
    if ExtractFileExt(name)='.vraw' then begin
      f.createToRead(name);
      i:=f.readlongint;
      j:=f.readlongint;
      resizeDat(i,j);
      if f.readBoolean then begin
        f.readBuf(PByte(pixelBuffer),sizeOf(T_floatColor)*xRes*yRes);
      end else begin
        getMem(v,sizeOf(T_24Bit)*xRes);
        for i:=0 to yRes-1 do begin
          f.readBuf(PByte(v),sizeOf(T_24Bit)*xRes);
          for j:=0 to xRes-1 do pixelBuffer[xRes*i+j]:=v[j];
        end;
        freeMem(v,sizeOf(T_24Bit)*xRes);
      end;
      f.destroy;
    end
    else if ExtractFileExt(name)='.csv' then loadCSV(name)
    else begin
      pname:=strAlloc(length(name)+1);
      strPCopy(pname,name);
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickReadImage(wand,pName);
      MagickFlipImage(wand);
      resizeDat(MagickGetImageWidth(wand),MagickGetImageHeight(wand));
      MagickGetImagePixels(wand,0,0,xRes,yRes,'RGB',FloatPixel,PByte(pixelBuffer));
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
      strDispose(pname);
    end;
  end;

PROCEDURE T_ByteMap.loadFromFile(name:string);
  {$ifndef naked}
  VAR wand: PMagickWand;
      pName:PChar;
  {$endif}
  VAR f:T_File;
      i,j:longint;
      v :P_floatColor;
      v2:P_24Bit;
  begin
    if ExtractFileExt(name)='.vraw' then begin
      f.createToRead(name);
      i:=f.readlongint;
      j:=f.readlongint;
      resizeDat(i,j);
      if f.readBoolean then begin
        getMem(v,sizeOf(T_floatColor)*xRes);
        for i:=0 to yRes-1 do begin
          f.readBuf(PByte(v),sizeOf(T_floatColor)*xRes);
          for j:=0 to xRes-1 do pixelBuffer[i*xRes+j]:=round(255*max(0,min(1,greyLevel(v[j]))));
        end;
        freeMem(v,sizeOf(T_floatColor)*xRes);
      end else begin
        getMem(v2,sizeOf(T_24Bit)*xRes);
        for i:=0 to yRes-1 do begin
          f.readBuf(PByte(v2),sizeOf(T_floatColor)*xRes);
          for j:=0 to xRes-1 do pixelBuffer[i*xRes+j]:=round(255*max(0,min(1,greyLevel(v2[j]))));
        end;
        freeMem(v2,sizeOf(T_24Bit)*xRes);
      end;
      f.destroy;
    end else begin
      pname:=strAlloc(length(name)+1);
      strPCopy(pname,name);
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickReadImage(wand,pName);
      MagickFlipImage(wand);
      resizeDat(MagickGetImageWidth(wand),MagickGetImageHeight(wand));
      MagickGetImagePixels(wand,0,0,xRes,yRes,'I',CharPixel,PByte(pixelBuffer));
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
      strDispose(pname);
    end;
  end;



PROCEDURE T_24BitImage.resize(newWidth,newHeight:longint);
  VAR wand: PMagickWand;
  begin
    if (xRes*yRes<>0) and ((xRes<>newWidth) or (yRes<>newHeight)) then begin
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,    xRes,yRes,'RGB', CharPixel,pixelBuffer);

      freeMem(pixelBuffer,xRes*yRes*3); xRes:=newWidth; yRes:=newHeight; getMem(pixelBuffer,xRes*yRes*3);

      MagickResizeImage    (wand,    xRes,yRes, LanczosFilter, 1.0); //LanczosFilter
      MagickGetImagePixels (wand,0,0,xRes,yRes,'RGB',CharPixel,pixelBuffer);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end else if (xRes*yRes=0) then begin
      xRes:=newWidth;
      yRes:=newHeight;
      getMem(pixelBuffer,xRes*yRes*3);
    end;
  end;

PROCEDURE T_FloatMap.resize(newWidth,newHeight:longint);
  VAR wand: PMagickWand;
  begin
    if (xRes*yRes<>0) and ((xRes<>newWidth) or (yRes<>newHeight)) then begin
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,    xRes,yRes,'RGB', FloatPixel,pixelBuffer);

      freeMem(pixelBuffer,xRes*yRes*sizeOF(T_floatColor)); xRes:=newWidth; yRes:=newHeight; getMem(pixelBuffer,xRes*yRes*sizeOF(T_floatColor));

      MagickResizeImage    (wand,    xRes,yRes, LanczosFilter, 1.0); //LanczosFilter
      MagickGetImagePixels (wand,0,0,xRes,yRes,'RGB',FloatPixel,pixelBuffer);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end else if (xRes*yRes=0) then begin
      xRes:=newWidth;
      yRes:=newHeight;
      getMem(pixelBuffer,xRes*yRes*sizeOF(T_floatColor));
    end;
  end;

PROCEDURE T_FloatMap.resize2(newWidth,newHeight:longint);
  VAR wand: PMagickWand;
  begin
    if (xRes*yRes<>0) and ((xRes<>newWidth) or (yRes<>newHeight)) then begin
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,    xRes,yRes,'RGB', FloatPixel,pixelBuffer);

      freeMem(pixelBuffer,xRes*yRes*sizeOF(T_floatColor)); xRes:=newWidth; yRes:=newHeight; getMem(pixelBuffer,xRes*yRes*sizeOF(T_floatColor));

      MagickResizeImage    (wand,    xRes,yRes, GaussianFilter, 1); //LanczosFilter
      MagickGetImagePixels (wand,0,0,xRes,yRes,'RGB',FloatPixel,pixelBuffer);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end else if (xRes*yRes=0) then begin
      xRes:=newWidth;
      yRes:=newHeight;
      getMem(pixelBuffer,xRes*yRes*sizeOF(T_floatColor));
    end;
  end;

PROCEDURE T_ByteMap.resize(newWidth,newHeight:longint);
  VAR wand: PMagickWand;
  begin
    if (xRes*yRes<>0) and ((xRes<>newWidth) or (yRes<>newHeight)) then begin
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,    xRes,yRes,'I', CharPixel,pixelBuffer);

      freeMem(pixelBuffer,xRes*yRes); xRes:=newWidth; yRes:=newHeight; getMem(pixelBuffer,xRes*yRes);

      MagickResizeImage    (wand,    xRes,yRes, LanczosFilter, 1.0); //LanczosFilter
      MagickGetImagePixels (wand,0,0,xRes,yRes,'I',CharPixel,pixelBuffer);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end else if (xRes*yRes=0) then begin
      xRes:=newWidth;
      yRes:=newHeight;
      getMem(pixelBuffer,xRes*yRes);
    end;
  end;

PROCEDURE T_24BitImage.crop(x0,x1,y0,y1:longint);
  VAR temp:T_24BitImage;
      y:longint;
      pt1,pt2:P_24Bit;
  begin
    temp.create(x1-x0,y1-y0);
    for y:=(y1-y0)-1 downto 0 do begin
      pt1:=     getLinePtr(y+y0); inc(pt1,x0);
      pt2:=temp.getLinePtr(y   );
      move(pt1^,pt2^,(x1-x0)*sizeOf(T_24Bit));
    end;
    resizeDat(x1-x0,y1-y0);
    move(temp.pixelBuffer^,pixelBuffer^,xRes*yRes*sizeOf(T_24Bit));
    temp.destroy;
  end;

PROCEDURE T_FloatMap.crop(x0,x1,y0,y1:longint);
  VAR temp:T_FloatMap;
      y:longint;
      pt1,pt2:P_floatColor;
  begin
    temp.create(x1-x0,y1-y0);
    for y:=(y1-y0)-1 downto 0 do begin
      pt1:=     getLinePtr(y+y0); inc(pt1,x0);
      pt2:=temp.getLinePtr(y   );
      move(pt1^,pt2^,(x1-x0)*sizeOf(T_floatColor));
    end;
    resizeDat(x1-x0,y1-y0);
    move(temp.pixelBuffer^,pixelBuffer^,xRes*yRes*sizeOf(T_floatColor));
    temp.destroy;
  end;

PROCEDURE T_24BitImage.cropResize(newWidth,newHeight:longint);
  VAR wand: PMagickWand;
      cropX,cropY:longint;
  begin
    if (xRes*yRes<>0) and ((xRes<>newWidth) or (yRes<>newHeight)) then begin
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      cropY:=round(yRes-newHeight*xRes/newWidth );
      cropX:=round(xRes-newWidth *yRes/newHeight);
      if newWidth/newHeight<xRes/yRes
        then crop((cropX shr 1),xRes+(cropX shr 1)-cropX,0,yRes)
        else crop(0,xRes,(cropY shr 1),yRes+(cropY shr 1)-cropY);
      wand:=NewMagickWand;
      MagickConstituteImage(wand,    xRes,yRes,'RGB', CharPixel,pixelBuffer);

      freeMem(pixelBuffer,xRes*yRes*sizeOF(T_24Bit)); xRes:=newWidth; yRes:=newHeight; getMem(pixelBuffer,xRes*yRes*sizeOF(T_24Bit));

      MagickResizeImage    (wand,    xRes,yRes, LanczosFilter, 1.0); //LanczosFilter
      MagickGetImagePixels (wand,0,0,xRes,yRes,'RGB',CharPixel,pixelBuffer);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end else if (xRes*yRes=0) then begin
      xRes:=newWidth;
      yRes:=newHeight;
      getMem(pixelBuffer,xRes*yRes*sizeOF(T_24Bit));
    end;
  end;



PROCEDURE T_FloatMap.cropResize(newWidth,newHeight:longint);
  VAR wand: PMagickWand;
      cropX,cropY:longint;
  begin
    if (xRes*yRes<>0) and ((xRes<>newWidth) or (yRes<>newHeight)) then begin
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      cropY:=round(yRes-newHeight*xRes/newWidth );
      cropX:=round(xRes-newWidth *yRes/newHeight);
      if newWidth/newHeight<xRes/yRes
        then crop((cropX shr 1),xRes+(cropX shr 1)-cropX,0,yRes)
        else crop(0,xRes,(cropY shr 1),yRes+(cropY shr 1)-cropY);
      wand:=NewMagickWand;
      MagickConstituteImage(wand,    xRes,yRes,'RGB', FloatPixel,pixelBuffer);

      freeMem(pixelBuffer,xRes*yRes*sizeOF(T_floatColor)); xRes:=newWidth; yRes:=newHeight; getMem(pixelBuffer,xRes*yRes*sizeOF(T_floatColor));

      MagickResizeImage    (wand,    xRes,yRes, LanczosFilter, 1.0); //LanczosFilter
      MagickGetImagePixels (wand,0,0,xRes,yRes,'RGB',FloatPixel,pixelBuffer);
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end else if (xRes*yRes=0) then begin
      xRes:=newWidth;
      yRes:=newHeight;
      getMem(pixelBuffer,xRes*yRes*sizeOF(T_floatColor));
    end;
  end;


PROCEDURE T_24BitImage.transpose;
  VAR temp:T_24BitImage;
      x,y:longint;
      pd,ps:P_24Bit;
  begin
    temp.create(yRes,xRes);
    pd:=P_24Bit(temp.pixelBuffer);
    ps:=P_24Bit(     pixelBuffer);
    for y:=0 to yRes-1 do
    for x:=0 to xRes-1 do pd[y+x*yRes]:=ps[x+y*xRes];
    y:=yRes;
    yRes:=xRes;
    xRes:=y;
    move(temp.pixelBuffer^,pixelBuffer^,xRes*yRes*3);
    temp.destroy;
  end;


FUNCTION T_24BitImage.diagonal:double; begin result:=sqrt(sqr(xRes)+sqr(yRes)); end;
FUNCTION T_ByteMap   .diagonal:double; begin result:=sqrt(sqr(xRes)+sqr(yRes)); end;
FUNCTION T_FloatMap  .diagonal:double; begin result:=sqrt(sqr(xRes)+sqr(yRes)); end;

FUNCTION  T_24BitImage.getLinePtr(y:longint):pointer;
  begin
    y:=yRes-1-y;
    if (y<0) or (y>=yRes) then result:= pixelBuffer     //fall back to first line if anything goes wrong...
                          else result:=(pixelBuffer+y*xRes);
  end;

FUNCTION T_FloatMap  .getLinePtr(y:longint):pointer;
  begin
    y:=yRes-1-y;
    if (y<0) or (y>=yRes) then result:= pixelBuffer     //fall back to first line if anything goes wrong...
                          else result:=(pixelBuffer+y*xRes);
  end;


PROCEDURE T_24BitImage.copyFrom(original:T_24BitImage; FilterType:FilterTypes);
  VAR wand: PMagickWand;
  begin
    if (original.width=xRes) and (original.height=yRes) then begin
      move(original.pixelBuffer^,pixelBuffer^,xRes*yRes*3)
    end else begin
      {$ifndef globalWand} MagickWandGenesis; {$endif}
      wand:=NewMagickWand;
      MagickConstituteImage(wand,original.width,original.height,'RGB', CharPixel,original.pixelBuffer);
      MagickResizeImage(wand, xRes,yRes, FilterType, 1.0)                                             ;
      MagickGetImagePixels(wand,0,0,xRes,yRes,'RGB',CharPixel,pixelBuffer)                            ;
      MagickRemoveImage(wand);
      wand := DestroyMagickWand(wand);
      {$ifndef globalWand} MagickWandTerminus; {$endif}
    end;
  end;

PROCEDURE T_24BitImage.flip;
  VAR x,y:longint;
      tmp:T_24Bit;
      pt :P_24Bit;
  begin
    pt:=(pixelBuffer);
    for y:=0 to (yRes shr 1)-1 do
    for x:=0 to xRes-1 do begin
      tmp                  :=pt[x+        y *xRes];
      pt[x+        y *xRes]:=pt[x+(yRes-1-y)*xRes];
      pt[x+(yRes-1-y)*xRes]:=tmp;
    end;
  end;

PROCEDURE T_FloatMap.flip;
  VAR x,y:longint;
      tmp:T_floatColor;
      pt :P_floatColor;
  begin
    pt:=(pixelBuffer);
    for y:=0 to (yRes shr 1)-1 do
    for x:=0 to xRes-1 do begin
      tmp                  :=pt[x+        y *xRes];
      pt[x+        y *xRes]:=pt[x+(yRes-1-y)*xRes];
      pt[x+(yRes-1-y)*xRes]:=tmp;
    end;
  end;


PROCEDURE T_24BitImage.flop;
  VAR x,y:longint;
      tmp:T_24Bit;
      pt :P_24Bit;
  begin
    pt:=(pixelBuffer);
    for y:=0 to yRes-1 do
    for x:=0 to (xRes shr 1)-1 do begin
      tmp                :=pt[       x+y*xRes];
      pt[       x+y*xRes]:=pt[xRes-1-x+y*xRes];
      pt[xRes-1-x+y*xRes]:=tmp;
    end;
  end;

PROCEDURE T_FloatMap.flop;
  VAR x,y:longint;
      tmp:T_floatColor;
      pt :P_floatColor;
  begin
    pt:=(pixelBuffer);
    for y:=0 to yRes-1 do
    for x:=0 to (xRes shr 1)-1 do begin
      tmp                :=pt[       x+y*xRes];
      pt[       x+y*xRes]:=pt[xRes-1-x+y*xRes];
      pt[xRes-1-x+y*xRes]:=tmp;
    end;
  end;


PROCEDURE T_24BitImage.rotLeft;
  VAR temp:T_24BitImage;
      x,y:longint;
      pd,ps:P_24Bit;
  begin
    temp.create(yRes,xRes);
    pd:=(temp.pixelBuffer);
    ps:=(     pixelBuffer);
    for y:=0 to yRes-1 do
    for x:=0 to xRes-1 do pd[yRes-1-y+x*yRes]:=ps[x+y*xRes];
    y:=yRes;
    yRes:=xRes;
    xRes:=y;
    move(temp.pixelBuffer^,pixelBuffer^,xRes*yRes*3);
    temp.destroy;
  end;

PROCEDURE T_FloatMap.rotLeft;
  VAR temp:T_FloatMap;
      x,y:longint;
      pd,ps:P_floatColor;
  begin
    temp.create(yRes,xRes);
    pd:=(temp.pixelBuffer);
    ps:=(     pixelBuffer);
    for y:=0 to yRes-1 do
    for x:=0 to xRes-1 do pd[yRes-1-y+x*yRes]:=ps[x+y*xRes];
    y:=yRes;
    yRes:=xRes;
    xRes:=y;
    move(temp.pixelBuffer^,pixelBuffer^,xRes*yRes*sizeOf(T_floatColor));
    temp.destroy;
  end;

PROCEDURE T_24BitImage.rotRight;
  VAR temp:T_24BitImage;
      x,y:longint;
      pd,ps:P_24Bit;
  begin
    temp.create(yRes,xRes);
    pd:=P_24Bit(temp.pixelBuffer);
    ps:=P_24Bit(     pixelBuffer);
    for y:=0 to yRes-1 do
    for x:=0 to xRes-1 do pd[y+x*yRes]:=ps[xRes-1-x+y*xRes];
    y:=yRes;
    yRes:=xRes;
    xRes:=y;
    move(temp.pixelBuffer^,pixelBuffer^,xRes*yRes*3);
    temp.destroy;
  end;


PROCEDURE T_FloatMap.rotRight;
  VAR temp:T_FloatMap;
      x,y:longint;
      pd,ps:P_floatColor;
  begin
    temp.create(yRes,xRes);
    pd:=(temp.pixelBuffer);
    ps:=(     pixelBuffer);
    for y:=0 to yRes-1 do
    for x:=0 to xRes-1 do pd[y+x*yRes]:=ps[xRes-1-x+y*xRes];
    y:=yRes;
    yRes:=xRes;
    xRes:=y;
    move(temp.pixelBuffer^,pixelBuffer^,xRes*yRes*sizeOf(T_floatColor));
    temp.destroy;
  end;

PROCEDURE T_24BitImage.resize(newWidth,newHeight:longint; style:byte);
  VAR tempW,tempH,offsetX,offsetY,y:longint;
      temp:T_24BitImage;
      pt:P_24Bit;
  begin
    case style of
      0: begin tempW:=newWidth; tempH:=newHeight; end;
      1: if xRes/newWidth>yRes/newHeight
           then begin tempW:=newWidth; tempH:=round(newWidth/xRes*yRes);    newHeight:=tempH; end
           else begin tempW:=round(newHeight/yRes*xRes);  tempH:=newHeight; newWidth :=tempW; end;
      2..4: if xRes/newWidth<yRes/newHeight
              then begin tempW:=newWidth; tempH:=round(newWidth/xRes*yRes);   end
              else begin tempW:=round(newHeight/yRes*xRes); tempH:=newHeight; end;
    end;
    temp.create(tempW,tempH);
    temp.copyFrom(self);
    resizeDat(newWidth,newHeight);

    case style of
      2: begin offsetX:=(tempW-newWidth) shr 1; offsetY:=(tempH-newHeight) shr 1; end;
      3: begin offsetX:=0;                      offsetY:=0;                       end;
      4: begin offsetX:=(tempW-newWidth);       offsetY:=(tempH-newHeight);       end;
    end;
    if style in [0,1] then begin
      pt              :=     pixelBuffer;
           pixelBuffer:=temp.pixelBuffer;
      temp.pixelBuffer:=pt;
    end else begin
      pt:=temp.pixelBuffer;
      for y:=0 to yRes-1 do
        move((pt+3*(offsetX+(y+offsetY)*tempW))^,
              (pixelBuffer+3*y*xRes)^,
              xRes*3);
    end;
    temp.destroy;
  end;

PROCEDURE T_FloatMap.resize(newWidth,newHeight:longint; style:byte);
  VAR tempW,tempH,offsetX,offsetY,y:longint;
      temp:T_FloatMap;
      pt:P_floatColor;
  begin
    case style of
      0: begin tempW:=newWidth; tempH:=newHeight; end;
      1: if xRes/newWidth>yRes/newHeight
           then begin tempW:=newWidth; tempH:=round(newWidth/xRes*yRes);    newHeight:=tempH; end
           else begin tempW:=round(newHeight/yRes*xRes);  tempH:=newHeight; newWidth :=tempW; end;
      2..4: if xRes/newWidth<yRes/newHeight
              then begin tempW:=newWidth; tempH:=round(newWidth/xRes*yRes);   end
              else begin tempW:=round(newHeight/yRes*xRes); tempH:=newHeight; end;
    end;
    temp.create(tempW,tempH);
    temp.copyFrom(self);
    resizeDat(newWidth,newHeight);

    case style of
      2: begin offsetX:=(tempW-newWidth) shr 1; offsetY:=(tempH-newHeight) shr 1; end;
      3: begin offsetX:=0;                      offsetY:=0;                       end;
      4: begin offsetX:=(tempW-newWidth);       offsetY:=(tempH-newHeight);       end;
    end;
    if style in [0,1] then begin
      pt              :=     pixelBuffer;
           pixelBuffer:=temp.pixelBuffer;
      temp.pixelBuffer:=pt;
    end else begin
      pt:=temp.pixelBuffer;
      for y:=0 to yRes-1 do
        move((pt+3*(offsetX+(y+offsetY)*tempW))^,
              (pixelBuffer+3*y*xRes)^,
              xRes*3);
    end;
    temp.destroy;

  end;

//----------------------------------------------------------------:bitmapFile24Bit
//================================================================================
//PROCEDURE T_24BitImage.applyCorrection(ai:T_aiState);
//  VAR i:longint;
//  begin
//    for i:=0 to xRes*yRes-1 do pixelBuffer[i]:=improvedPixel(ai,pixelBuffer[i]);
//  end;


//PROCEDURE T_FloatMap.applyCorrection(ai:T_aiState);
//  VAR i:longint;
//  begin
//    for i:=0 to xRes*yRes-1 do pixelBuffer[i]:=improvedPixel(ai,pixelBuffer[i]);
//  end;

PROCEDURE T_FloatMap.multiplyWith(factor:T_Float);
  VAR i:longint;
  begin
    for i:=0 to xRes*yRes-1 do pixelBuffer[i]:=pixelBuffer[i]*factor;
  end;

PROCEDURE T_FloatMap.applyOffset(value:T_floatColor);
  VAR i:longint;
  begin
    for i:=0 to xRes*yRes-1 do pixelBuffer[i]:=pixelBuffer[i]+value;
  end;

PROCEDURE T_FloatMap.threshold(x:T_Float);
  VAR i,j:longint;
  begin
    for i:=0 to xRes*yRes-1 do
    for j:=0 to 2 do pixelBuffer[i,j]:=pixelBuffer[i,j]*pixelBuffer[i,j]*pixelBuffer[i,j]*x;
  end;

PROCEDURE T_FloatMap.toAbsValue;
  VAR i,j:longint;
  begin
    for i:=0 to xRes*yRes-1 do
    for j:=0 to 2 do pixelBuffer[i,j]:=abs(pixelBuffer[i,j]);
  end;

FUNCTION T_24BitImage.averageColor:T_floatColor;
  VAR i,j:longint;
      lineAvg:T_floatColor;
  begin
    result:=black;
    for j:=0 to yRes-1 do begin
      lineAvg:=black;
      for i:=j*xRes to (j+1)*xres-1 do lineAvg:=lineAvg+P_24Bit(pixelBuffer)[i];
      result:=result+lineAvg*(1/xRes);
    end;
    result:=result*(1/yRes);
  end;

PROCEDURE T_24BitImage.setThumbnail(index:longint; VAR img:T_24BitImage);
  begin
    if (xres=128) and (yres=4096) and (index>=0) and (index<32) then begin
      img.cropResize(128,128);
      move(img.rawData^,pixelBuffer[128*index],128*128*sizeOf(T_24Bit));
    end;
  end;

PROCEDURE T_24BitImage.getThumbnail(index:longint; OUT img:T_24BitImage);
  begin
    if (xres=128) and (yres=4096) and (index>=0) and (index<32) then begin
      img.create(128,128);
      move(pixelBuffer[128*index],img.rawData^,128*128*sizeOf(T_24Bit));
    end else img.create;
  end;


FUNCTION improvedPixel(a:T_aiState; c:T_floatColor):T_floatColor;
  VAR i:longint;
  begin
    for i:=0 to 2 do begin
      c[i]:=0.01+0.98*(255*c[i]-a.a)*a.fak;
      if c[i]<1/255 then c[i]:=0
                    else c[i]:=exp(ln(c[i])*a.gamma);
    end;
    result:=c;//projectedColor(c);
  end;

FUNCTION adaptionParameters(hist:T_histogram):T_aiState;
  VAR hist2:T_histogram;
      i:longint;
      fi:T_Float;

  FUNCTION gammaOfImage:double;
    VAR i,n:longint;
        x,y,xx,xy,lx,ly:double;
    begin
      x:=0; y:=0; xx:=0; xy:=0; n:=0;
      for i:=1 to 255 do if hist2[i]>0 then begin
        inc(n);
        lx:=ln(i); ly:=ln(hist2[i]);
        x:=x+lx; xx:=xx+lx*lx;
        y:=y+ly; xy:=xy+lx*ly;
      end;
      //writeln('gammaOfImage - intermediate results');
      //writeln('  x =',x );
      //writeln('  y =',y );
      //writeln('  xx=',xx);
      //writeln('  xy=',xy);
      //writeln('  n =',n );

      result:=(xy-x*y/n)/(xx-x*x/n);
    end;

  begin
    hist:=toCumulative(hist);
    result.a:=quantileFromCum(hist,0.02);
    result.b:=quantileFromCum(hist,0.98);
    with result do fak:=1/(b-a);
    hist:=fromCumulative(hist);
    for i:=0 to 255 do hist2[i]:=0;
    //compute histogram of normalized picture:----------------------------//
    for i:=0 to 255 do begin                                              //
      fi:=max(0,min(255,(255*(0.02+0.96*(i-result.a)*result.fak))));      //
                     finc(hist2[trunc(fi)  ],(1-frac(fi))*hist[i]);       //
      if fi<255 then finc(hist2[trunc(fi)+1],   frac(fi) *hist[i]);       //
    end;                                                                  //
    //------------------------------:compute histogram of normalized picture
    hist2:=toCumulative(hist2);
    result.gamma:=1/gammaOfImage;
    if isNAN(result.gamma) or isInfinite(result.gamma) then result.gamma:=1;

  end;

PROCEDURE T_ByteMap.setToValue(c:byte);
  VAR i:longint;
  begin
    for i:=0 to xRes*yRes-1 do pixelBuffer[i]:=c;
  end;

FUNCTION T_ByteMap.countEqual(c:byte):longint;
  VAR i:longint;
  begin
    result:=0;
    for i:=0 to xRes*yRes-1 do if c=pixelBuffer[i] then inc(result);
  end;

FUNCTION T_ByteMap.countOdd:longint;
  VAR i:longint;
  begin
    result:=0;
    for i:=0 to xRes*yRes-1 do if odd(pixelBuffer[i]) then inc(result);
  end;

FUNCTION myTimeToStr(dt:double):string;
  CONST oneMinute=1/(24*60);
        oneSecond=oneMinute/60;
  begin
    if dt<oneMinute
      then begin
        result:=FormatFloat('#0.00',dt/oneSecond)+'sec';
        if length(result)<8 then result:=' '+result;
      end
    else if dt>1
      then begin
        dt:=dt*24;             result:=       formatFloat('00',floor(dt))+':';
        dt:=(dt-floor(dt))*60; result:=result+formatFloat('00',floor(dt))+':';
        dt:=(dt-floor(dt))*60; result:=result+formatFloat('00',floor(dt));
      end
    else result:=timeToStr(dt);
  end;


INITIALIZATION
  randomize;
  {$ifdef globalWand} MagickWandGenesis; {$endif}

FINALIZATION

end.
