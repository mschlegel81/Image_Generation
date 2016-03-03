UNIT ig_expoClouds;
INTERFACE
USES imageGeneration,complex,myColors,myParams,myGenerics,sysutils,myFiles;
TYPE
  T_parameterSet=array[0..1,0..4] of T_Complex;
  T_legacyParameterSet=array[0..1,0..4] of record re,im:single; valid:boolean; end;

  { T_expoCloud }

  T_expoCloud=object(T_functionPerPixelAlgorithm)
    hueOffset,
    saturation,
    brightness,
    limit:double;
    par:T_parameterSet;
    CONSTRUCTOR create;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION getColorAt(CONST ix,iy:longint; CONST x:T_Complex):T_floatColor; virtual;
    PROCEDURE load(CONST fileName:ansistring);
  end;

IMPLEMENTATION
CONSTRUCTOR T_expoCloud.create;
  VAR i,j:longint;
  begin
    inherited create;
    {0} addParameter('hue offset',pt_float);
    {1} addParameter('saturation',pt_float);
    {2} addParameter('brightness',pt_float);
    {3} addParameter('limit',pt_float,0);
    for i:=0 to 1 do for j:=0 to 4 do addParameter('p['+intToStr(i)+','+intToStr(j)+']',pt_2floats);
    resetParameters(0);
  end;

FUNCTION T_expoCloud.getAlgorithmName: ansistring;
  begin
    result:='Expo-Clouds';
  end;

FUNCTION T_expoCloud.parameterResetStyles: T_arrayOfString;
  begin
    result:='Reset (Zero)';
    append(result,'Randomize (1)');
    append(result,'Randomize (2)');
  end;

PROCEDURE T_expoCloud.resetParameters(CONST style: longint);
  VAR i,j:longint;
  begin
    inherited resetParameters(style);
    if style=0 then begin
      hueOffset:=0;
      saturation:=0;
      brightness:=0;
      limit:=0;
      for i:=0 to 1 do for j:=0 to 4 do par[i,j]:=0;
    end else begin
      brightness:=random/254;
      saturation:=random;
      hueOffset :=random;
      limit     :=system.exp(system.ln(1E6)*random);
      if style=1
      then for i:=0 to 1 do for j:=0 to 4 do par[i,j]:= 4*(random-0.5)+II*4*(random-0.5)
      else for i:=0 to 1 do for j:=0 to 4 do par[i,j]:=(4*(random-0.5)+II*4*(random-0.5))*system.exp(-j*0.2);
    end;
  end;

FUNCTION T_expoCloud.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+14;
  end;

PROCEDURE T_expoCloud.setParameter(CONST index: byte; CONST value: T_parameterValue);
  VAR i,j:longint;
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: hueOffset:=value.f0;
      1: saturation:=value.f0;
      2: brightness:=value.f0;
      3: limit:=value.f0;
      else begin
        i:=index-inherited numberOfParameters-4;
        j:=i mod 5;
        i:=i div 5;
        par[i,j].re:=value.f0;
        par[i,j].im:=value.f1;
      end;
    end;
  end;

FUNCTION T_expoCloud.getParameter(CONST index: byte): T_parameterValue;
  VAR i,j:longint;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index))
    else case byte(index-inherited numberOfParameters) of
      0: result:=parValue(index,hueOffset);
      1: result:=parValue(index,saturation);
      2: result:=parValue(index,brightness);
      3: result:=parValue(index,limit);
      else begin
        i:=index-inherited numberOfParameters-4;
        j:=i mod 5;
        i:=i div 5;
        result:=parValue(index,par[i,j].re,par[i,j].im);
      end;
    end;
  end;

FUNCTION T_expoCloud.getColorAt(CONST ix, iy: longint; CONST x: T_Complex): T_floatColor;
  FUNCTION recColor(p:T_Complex; depth:byte; VAR hits:longint):T_floatColor;
    begin
      result[0]:=sqrabs(p);
      if result[0]<limit then begin
        inc(hits);
        result:=fromHSV(arg(p)/(2*pi)+hueOffset,saturation,brightness*system.sqr(system.sqr((1-result[0]/limit))));
        if depth>0 then result:=result+recColor(par[0,0]+par[0,1]*p+exp(par[0,2]*(par[0,3]+par[0,4]*p)),depth-1,hits)
                                      +recColor(par[1,0]+par[1,1]*p+exp(par[1,2]*(par[1,3]+par[1,4]*p)),depth-1,hits);
      end else result:=black;
    end;

  VAR hitAlpha:longint=0;
  begin
    result:=recColor(x,8,hitAlpha);
  end;

PROCEDURE T_expoCloud.load(CONST fileName: ansistring);
  VAR f:T_file;
      i,j:longint;
  begin
    if not(fileExists(fileName)) then exit;
    resetParameters(0);
    f.createToRead(fileName);
    scaler.setCenterX(f.readSingle);
    scaler.setCenterY(f.readSingle);
    scaler.setZoom(f.readSingle);
    for i:=0 to 1 do for j:=0 to 4 do begin
      par[i,j].re:=f.readSingle;
      par[i,j].im:=f.readSingle;
    end;
    hueOffset :=f.readSingle;
    limit     :=f.readSingle;
    saturation:=f.readSingle;
    brightness:=f.readSingle;
    f.destroy;
  end;

VAR expoCloud:T_expoCloud;
INITIALIZATION
  expoCloud.create;
  registerAlgorithm(@expoCloud,true,false,false);
FINALIZATION
  expoCloud.destroy;

end.
