UNIT ig_bifurcation;
INTERFACE
USES imageGeneration,myColors,complex,myParams,sysutils,math,darts;
TYPE

  { T_bifurcation }

  T_bifurcation=object(T_pixelThrowerAlgorithm)
    equation    :byte;
    maxDepth    :longint;
    preIt       :longint;
    colorStyle  :byte;

    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE prepareSlice(CONST index:longint); virtual;
  end;

IMPLEMENTATION

VAR bifurcation:T_bifurcation;

{ T_bifurcation }

CONSTRUCTOR T_bifurcation.create;
  begin
    inherited create;
    addParameter('Equation',pt_enum,0,6,'Feigenbaum','Feigenbaum Trf.1','Feigenbaum Trf.2','Cosine','Sinc','Cosc','XX');
    addParameter('Depth',pt_intOr2Ints,0);
    addParameter('Coloring',pt_enum,0,2,'White on black','Harsh','Fiery');
    resetParameters(0);
  end;

PROCEDURE T_bifurcation.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    equation    :=0;
    maxDepth    :=200;
    preIt       :=50;
    colorStyle  :=0;
    par_alpha   :=0.1;
  end;

FUNCTION T_bifurcation.getAlgorithmName: ansistring;
  begin
    result:='Bifurcation Plot';
  end;

FUNCTION T_bifurcation.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+3;
  end;

PROCEDURE T_bifurcation.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: equation:=value.i0;
      1: begin
           maxDepth:=max(value.i0,value.i1);
           preIt   :=min(value.i0,value.i1);
         end;
      2: colorStyle:=value.i0;
    end;
  end;

FUNCTION T_bifurcation.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index))
    else case byte(index-inherited numberOfParameters) of
      0: result:=parValue(index,equation);
      1: result:=parValue(index,preIt,maxDepth);
      2: result:=parValue(index,colorStyle);
    end;
  end;

PROCEDURE T_bifurcation.prepareSlice(CONST index: longint);
  VAR tempMap:array of word;
      x,y:longint;
      a0,a1,da:double;

  PROCEDURE iterate(CONST a_IN:double);
    PROCEDURE putPixel(CONST wx,wy:double);
      VAR C:T_Complex;
          j:longint;
      begin
        c:=scaler.mrofsnart(wx,wy);
        c.re:=c.re+darts_delta[index,0];
        c.im:=c.im+darts_delta[index,1];
        if (c.re>-0.5) and (c.re<renderTempData.maxPixelX) and
           (c.im>-0.5) and (c.im<renderTempData.maxPixelY) then begin
          j:=round(c.re)+
             round(c.im)*renderTempData.xRes;
          if tempMap[j]<65535 then inc(tempMap[j]);
        end;
      end;

    VAR x,a:double;
        k:longint;
    begin
      x:=random;
      case equation of
        1: begin a:=0.25-a_IN; if a<0 then a:=5 else a:=2*sqrt(a)+1;  end;
        2: a:=4+(1/a_IN);
        else a:=a_IN;
      end;
      case equation of
        0..2: for k:=0 to preIt-1 do x:=a*x*(1-x);
        3:    for k:=0 to preIt-1 do x:=system.cos(a*x);
        4:    for k:=0 to preIt-1 do x:=100*system.sin(a*x*0.1)/(a*x);
        5:    for k:=0 to preIt-1 do x:=system.cos(a*x*5)/(0.1+abs(a*x));
        6:    for k:=0 to preIt-1 do begin x:=a*x; x:=system.sin(x)-system.sin(x*5)/5+system.sin(x*11)/11; end;
      end;
      case equation of
        0..2: for k:=preIt to maxDepth do begin x:=a*x*(1-x);                                                   putPixel(a_IN,x); end;
        3:    for k:=preIt to maxDepth do begin x:=system.cos(a*x);                                             putPixel(a_IN,x); end;
        4:    for k:=preIt to maxDepth do begin x:=100*system.sin(a*x*0.1)/(a*x);                               putPixel(a_IN,x); end;
        5:    for k:=preIt to maxDepth do begin x:=system.cos(a*x*5)/(0.1+abs(a*x));                            putPixel(a_IN,x); end;
        6:    for k:=preIt to maxDepth do begin x:=a*x; x:=system.sin(x)-system.sin(x*5)/5+system.sin(x*11)/11; putPixel(a_IN,x); end;
      end;
    end;

  VAR flushFactor:double;
  FUNCTION updatedPixel(CONST prevColor,bgColor:T_floatColor; CONST hits:word):T_floatColor; inline;
    VAR cover:double;
        locColor:T_floatColor=(1,1,1);
    begin
      cover:=1-intpower(renderTempData.antiCoverPerSample,hits);
      case colorStyle of
        1: if cover<0.5 then cover:=0 else cover:=1;
        2: begin
          if cover<0 then cover:=0
          else if cover<1/3 then begin locColor:=newColor(1,0,0); cover:=3*cover;  end
          else if cover<2/3 then begin locColor:=newColor(1,3*cover-1,0); cover:=1; end
          else if cover<1   then begin locColor:=newColor(1,1,3*cover-2); cover:=1; end
          else cover:=1;
        end;
      end;
      result:=(prevColor*renderTempData.samplesFlushed+locColor*cover+bgColor*(1-cover))*flushFactor;
    end;

  begin
    with renderTempData do if index<aaSamples then begin
      with renderTempData do begin
        setLength(tempMap,xRes*yRes);
        for x:=0 to length(tempMap)-1 do tempMap[x]:=0;
        a0:=min(min(scaler.transform(     0,0).re,scaler.transform(     0,yRes-1).re),
                min(scaler.transform(xRes-1,0).re,scaler.transform(xRes-1,yRes-1).re));
        a1:=max(max(scaler.transform(     0,0).re,scaler.transform(     0,yRes-1).re),
                max(scaler.transform(xRes-1,0).re,scaler.transform(xRes-1,yRes-1).re));
        da:=(a1-a0)/timesteps*(maxDepth-preIt+1);
        a0:=a0+da*random;
      end;
      while a0<a1 do begin
        iterate(a0);
        a0:=a0+da;
      end;
      if not(progressQueue.cancellationRequested) then begin
        system.enterCriticalSection(flushCs);
        flushFactor:=(1/(samplesFlushed+1));
        if hasBackground and (backgroundImage<>nil)
        then for y:=0 to yRes-1 do for x:=0 to xRes-1 do generationImage^[x,y]:=updatedPixel(generationImage^[x,y],backgroundImage^[x,y],tempMap[y*xRes+x])
        else for y:=0 to yRes-1 do for x:=0 to xRes-1 do generationImage^[x,y]:=updatedPixel(generationImage^[x,y],black                ,tempMap[y*xRes+x]);
        inc(samplesFlushed);
        system.leaveCriticalSection(flushCs);
      end;
      setLength(tempMap,0);
    end;
  end;
INITIALIZATION
  bifurcation.create;
  registerAlgorithm(@bifurcation,true,false,false);
FINALIZATION
  bifurcation.destroy;
end.

