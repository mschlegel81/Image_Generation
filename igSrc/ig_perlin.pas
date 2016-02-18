UNIT ig_perlin;
INTERFACE
USES imageGeneration,myParams,math,myColors,myTools;
TYPE
  T_perlinNoiseAlgorithm=object(T_generalImageGenrationAlgorithm)
    seed:longint;
    scaleFactor,amplitudeFactor:double;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION getAlgorithmName:ansistring; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION prepareImage(CONST forPreview:boolean=false):boolean; virtual;
  end;

IMPLEMENTATION
CONSTRUCTOR T_perlinNoiseAlgorithm.create;
  begin
    inherited create;
    addParameter('seed',pt_integer                     );
    addParameter('scale factor',pt_float,0.001,0.999    );
    addParameter('amplitude factor',pt_float,0.001,0.999);
    resetParameters(0);
  end;

DESTRUCTOR T_perlinNoiseAlgorithm.destroy;
  begin end;

FUNCTION T_perlinNoiseAlgorithm.getAlgorithmName: ansistring;
  begin result:='Perlin Noise'; end;

PROCEDURE T_perlinNoiseAlgorithm.resetParameters(CONST style: longint);
  begin
    if style=0 then seed:=0 else seed:=randseed;
    scaleFactor:=0.6;
    amplitudeFactor:=0.8;
  end;

FUNCTION T_perlinNoiseAlgorithm.numberOfParameters: longint;
  begin result:=3; end;

PROCEDURE T_perlinNoiseAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    case index of
      0: seed:=value.i0;
      1: scaleFactor:=value.f0;
      2: amplitudeFactor:=value.f0;
    end;
  end;

FUNCTION T_perlinNoiseAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    case index of
      0: result.createFromValue(parameterDescription(0),seed);
      1: result.createFromValue(parameterDescription(1),scaleFactor);
      2: result.createFromValue(parameterDescription(2),amplitudeFactor);
    end;
  end;

FUNCTION T_perlinNoiseAlgorithm.prepareImage(CONST forPreview: boolean):boolean;
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
    progressQueue.forceStart(et_stepCounter_parallel,renderImage^.height);
    initPerlinTable;
    xRes:=renderImage^.width;
    yRes:=renderImage^.height;

    if scaleFactor>1 then begin
      scaleFactor:=1/scaleFactor;
      amplitudeFactor:=1/amplitudeFactor;
    end;

    aid:=0;
    setLength(amplitude,1);
    setLength(scale,1);
    amplitude[0]:=1;
    scale[0]:=1/renderImage^.diagonal;
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
        renderImage^[x,y]:=aid*white;
      end;
    end;
    setLength(perlinLine,0);
    setLength(scale,0);
    setLength(amplitude,0);
    progressQueue.logEnd;
    result:=true;
  end;

VAR perlinNoiseAlgorithm  :T_perlinNoiseAlgorithm;
INITIALIZATION
  perlinNoiseAlgorithm.create;   registerAlgorithm(@perlinNoiseAlgorithm,false,false,false);

FINALIZATION
  perlinNoiseAlgorithm.destroy;


end.

