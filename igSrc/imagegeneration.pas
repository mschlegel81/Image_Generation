UNIT imageGeneration;
INTERFACE
USES mypics,myGenerics,myColors,complex,math,darts,Interfaces, ExtCtrls, Graphics, types,simplePicChunks, myTools, myParams;
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

    PROCEDURE prepareImage(VAR img:T_rawImage; CONST forPreview:boolean=false); virtual; abstract;
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
    PROCEDURE prepareImage(VAR img:T_rawImage; CONST forPreview:boolean=false); virtual;
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
    PROCEDURE prepareImage(VAR img:T_rawImage; CONST forPreview:boolean=false); virtual;
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
  T_functionPerPixelAlgorithm=object(T_scaledImageGenerationAlgorithm)
    renderTolerance:double;

    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual; abstract;
    PROCEDURE prepareImage(VAR img:T_rawImage; CONST forPreview:boolean=false); virtual;
    PROCEDURE prepareChunk(VAR chunk:T_colChunk; CONST forPreview:boolean=false); virtual;
  end;

  { T_functionPerPixelViaRawDataAlgorithm }

  T_functionPerPixelViaRawDataAlgorithm=object(T_scaledImageGenerationAlgorithm)
    temporaryRawMap:P_rawImage;

    CONSTRUCTOR create;
    FUNCTION getRawDataAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual; abstract;
    FUNCTION getColor(CONST rawData:T_floatColor):T_floatColor; virtual; abstract;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual;
    PROCEDURE prepareRawMap(CONST workerIndex,modul:longint); virtual;
    PROCEDURE prepareImage(VAR img:T_rawImage; CONST forPreview:boolean=false); virtual;
  end;

PROCEDURE registerAlgorithm(CONST p:P_generalImageGenrationAlgorithm);
IMPLEMENTATION
VAR colorGradientAlgorithm:T_colorGradientAlgorithm;
    perlinNoiseAlgorithm  :T_perlinNoiseAlgorithm;
VAR algorithms:array of record
                 name:string;
                 prototype:P_generalImageGenrationAlgorithm;
               end;

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

CONSTRUCTOR T_colorGradientAlgorithm.create;  begin resetParameters(0); end;
DESTRUCTOR T_colorGradientAlgorithm.destroy;  begin end;
FUNCTION T_colorGradientAlgorithm.getAlgorithmName: ansistring; begin result:='Linear gradient'; end;
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
      1: result:=myParams.parameterDescription('Color #1',pt_color);
      2: result:=myParams.parameterDescription('Color #2',pt_color);
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

PROCEDURE T_colorGradientAlgorithm.prepareImage(VAR img: T_rawImage; CONST forPreview: boolean);
  VAR x,y:longint;
      nx,ny,w:single;
      dc:T_floatColor;
  begin
    dc:=c1-c0;
    nx:=2*system.cos(pi/180*angle)/img.diagonal;
    ny:=2*system.sin(pi/180*angle)/img.diagonal;
    for y:=0 to img.height-1 do begin
      for x:=0 to img.width-1 do begin
        w:=(x-img.width/2)*nx+(y-img.height/2)*ny;
        if      w> 1 then w:=1
        else if w<-1 then w:=0
        else w:=(w+1)*0.5;
        img[x,y]:=c0+w*dc;
      end;
    end;
  end;

CONSTRUCTOR T_perlinNoiseAlgorithm.create;
  begin resetParameters(0); end;

DESTRUCTOR T_perlinNoiseAlgorithm.destroy;
  begin end;

FUNCTION T_perlinNoiseAlgorithm.getAlgorithmName: ansistring;
  begin result:='Perlin Noise'; end;

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

PROCEDURE T_perlinNoiseAlgorithm.prepareImage(VAR img: T_rawImage; CONST forPreview: boolean);
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
    initPerlinTable;
    xRes:=img.width;
    yRes:=img.height;

    if scaleFactor>1 then begin
      scaleFactor:=1/scaleFactor;
      amplitudeFactor:=1/amplitudeFactor;
    end;

    aid:=0;
    setLength(amplitude,1);
    setLength(scale,1);
    amplitude[0]:=1;
    scale[0]:=1/img.diagonal;
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
    for y:=0 to yRes-1 do begin
      for l:=0 to lMax-1 do updatePerlinLine((y-yRes*0.5)*scale[L],L,amplitude[L]);
      for x:=0 to xRes-1 do begin
        aid:=0.5;
        for l:=0 to lMax-1 do aid:=aid+getSmoothValue((x-xRes*0.5)*scale[L],L);
        if aid>1 then aid:=1
        else if aid<0 then aid:=0;
        img[x,y]:=aid*white;
      end;
    end;
    setLength(perlinLine,0);
    setLength(scale,0);
    setLength(amplitude,0);
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
    rectA.top:=0;
    rectA.left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.top:=0+dy;
    rectB.left:=0+dx;
    rectB.Right:=plotImage.width+dx;
    rectB.Bottom:=plotImage.height+dy;
    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
  end;

PROCEDURE T_scaledImageGenerationAlgorithm.zoomOnPoint(VAR plotImage:TImage;  CONST cx, cy: longint; CONST zoomFactor: double);
  VAR rectA,rectB:TRect;
  begin
    scaler.chooseScreenRef(cx,cy);
    scaler.setZoom(scaler.getZoom*zoomFactor);
    rectA.top:=0;
    rectA.left:=0;
    rectA.Right:=plotImage.width;
    rectA.Bottom:=plotImage.height;
    rectB.top:=round((-cy)*zoomFactor+cy);
    rectB.left:=round((-cx)*zoomFactor+cx);
    rectB.Right:=round((plotImage.width-cx)*zoomFactor+cx);
    rectB.Bottom:=round((plotImage.height-cy)*zoomFactor+cy);
    plotImage.Canvas.CopyRect(rectA, plotImage.Canvas, rectB);
  end;


procedure T_functionPerPixelAlgorithm.prepareImage(var img: T_rawImage; const forPreview: boolean);
  begin

  end;

PROCEDURE T_functionPerPixelAlgorithm.prepareChunk(VAR chunk:T_colChunk; CONST forPreview:boolean=false);
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

constructor T_functionPerPixelViaRawDataAlgorithm.create;
  begin
    temporaryRawMap:=nil;
  end;

function T_functionPerPixelViaRawDataAlgorithm.getColorAt(CONST ix,iy:longint; CONST xy:T_Complex): T_floatColor;
  begin
    result:=getColor(getRawDataAt(ix,iy,xy));
  end;

procedure T_functionPerPixelViaRawDataAlgorithm.prepareRawMap(CONST workerIndex,modul:longint);
  VAR i,j:longint;
  begin
    for j:=0 to temporaryRawMap^.height-1 do if j mod modul=workerIndex then
    for i:=0 to temporaryRawMap^.width-1 do temporaryRawMap^[i,j]:=getRawDataAt(i,j,scaler.transform(i,j));
  end;

procedure T_functionPerPixelViaRawDataAlgorithm.prepareImage(var img: T_rawImage; const forPreview: boolean);

  begin
    if forPreview then begin
      if temporaryRawMap=nil then new(temporaryRawMap,create(img.width,img.height));
      temporaryRawMap^.mutateType(rs_float);
      temporaryRawMap^.resize(img.width,img.height, res_dataResize);
    end else begin
      if temporaryRawMap<>nil then begin
        dispose(temporaryRawMap,destroy);
        temporaryRawMap:=nil;
      end;
    end;

  end;

INITIALIZATION
  colorGradientAlgorithm.create; registerAlgorithm(@colorGradientAlgorithm);
  perlinNoiseAlgorithm.create;   registerAlgorithm(@perlinNoiseAlgorithm);

FINALIZATION
  setLength(algorithms,0);
  colorGradientAlgorithm.destroy;
  perlinNoiseAlgorithm.destroy;

end.

