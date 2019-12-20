UNIT ig_factorTables;
INTERFACE
USES imageGeneration,myParams,mypics,myTools,complex,myColors,math,darts;
TYPE
  P_factorTable=^T_factorTable;
  T_factorTable=object(T_pixelThrowerAlgorithm)
    par_modulus0,
    par_modulus1,
    par_factor0,
    par_factor1,
    par_bright,
    par_linewidth:double ;

    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE prepareSlice(CONST target:P_rawImage; CONST queue:P_progressEstimatorQueue; CONST index:longint); virtual;
  end;

IMPLEMENTATION

CONSTRUCTOR T_factorTable.create;
  begin
    inherited create;
    addParameter('modulus',pt_floatOr2Floats);
    addParameter('factor',pt_floatOr2Floats);
    addParameter('brightness',pt_float,0);
    addParameter('lineWidth',pt_float,0);
    resetParameters(0);
  end;

PROCEDURE T_factorTable.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    par_modulus0:=255;
    par_modulus1:=255;
    par_factor0:=130;
    par_factor1:=130;
    par_bright :=1;
    par_linewidth:=0;
  end;

FUNCTION T_factorTable.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+4;
  end;

PROCEDURE T_factorTable.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: begin par_modulus0:=value.f0; par_modulus1:=value.f1; end;
      1: begin par_factor0 :=value.f0; par_factor1 :=value.f1; end;
      2: par_bright:=value.f0;
      3: par_linewidth:=value.f0;
    end;
  end;

FUNCTION T_factorTable.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index));
    case byte(index-inherited numberOfParameters) of
      0: result:=parValue(index,par_modulus0,par_modulus1);
      1: result:=parValue(index,par_factor0,par_factor1);
      3: result:=parValue(index,par_linewidth);
    else result:=parValue(index,par_bright);
    end;
  end;

PROCEDURE T_factorTable.prepareSlice(CONST target:P_rawImage; CONST queue:P_progressEstimatorQueue; CONST index: longint);
  VAR tempMap:array of word;
      flushFactor:double=0;
      offsetTab:array[0..498] of T_Complex; //499 is a prime.
      offsetIdx:longint=0;

  PROCEDURE initOffset;
    VAR j:longint;
    begin
      for j:=0 to 498 do begin
        repeat
          offsetTab[j].re:=2*random-1;
          offsetTab[j].im:=2*random-1;
        until sqrabs(offsetTab[j])<=1;
        offsetTab[j]*=par_linewidth;
      end;
    end;

  PROCEDURE putSample(CONST x,y:double); inline;
    VAR c:T_Complex;
        j:longint;
    begin
      c:=scaler.mrofsnart(x+offsetTab[offsetIdx].re,
                          y+offsetTab[offsetIdx].im);
      if offsetIdx=498 then offsetIdx:=0 else inc(offsetIdx);
      c.re:=c.re+darts_delta[index,0];
      c.im:=c.im+darts_delta[index,1];
      if (c.re>-0.5) and (c.re<renderTempData.maxPixelX) and
         (c.im>-0.5) and (c.im<renderTempData.maxPixelY) then begin
        j:=round(c.re)+
           round(c.im)*renderTempData.xRes;
        if tempMap[j]<65535 then inc(tempMap[j]);
      end;
    end;

  FUNCTION updatedPixel(CONST prevColor,bgColor:T_rgbFloatColor; CONST hits:word):T_rgbFloatColor; inline;
    VAR cover:double;
    begin
      cover:=1-intpower(renderTempData.antiCoverPerSample,hits);
      result:=(prevColor*renderTempData.samplesFlushed+
               WHITE*cover+bgColor*(1-cover))*flushFactor;
    end;

  VAR modulus,t:double;
      i,k:longint;
      k0,k1:double;
      x0,x1,y0,y1:double;
  begin
    with renderTempData do if index<aaSamples then begin
      randomize;
      initOffset;
      setLength(tempMap,xRes*yRes);
      for i:=0 to length(tempMap)-1 do tempMap[i]:=0;
      for i:=0 to (timesteps shr 4)-1 do if random<0.01 then begin
        t:=random*pi/8;
        for k:=0 to 15 do begin
          putSample(system.cos(t),
                    system.sin(t));
          t+=(pi/8);
        end;
      end else begin
        modulus:=par_modulus0+random*(par_modulus1-par_modulus0);
        k:=floor(modulus);
        if k>0 then k0:=random(k)
               else k0:=k;
        modulus:=2*pi/modulus;
        k1:=k0*(par_factor0+random*(par_factor1-par_factor0));
        k0*=modulus;
        k1*=modulus;
        t:=random/16;
        x0:=system.cos(k0); y0:=system.sin(k0);
        x1:=system.cos(k1); y1:=system.sin(k1);
        for k:=0 to 15 do begin
          putSample(x0*(1-t)+x1*t,
                    y0*(1-t)+y1*t);
          t+=1/16;
        end;
      end;
      if not(queue^.cancellationRequested) then begin
        system.enterCriticalSection(flushCs);
        flushFactor:=(1/(samplesFlushed+1));
        if hasBackground and (backgroundImage<>nil)
        then for i:=0 to yRes-1 do for k:=0 to xRes-1 do target^[k,i]:=updatedPixel(target^[k,i],backgroundImage^[k,i],tempMap[i*xRes+k])
        else for i:=0 to yRes-1 do for k:=0 to xRes-1 do target^[k,i]:=updatedPixel(target^[k,i],BLACK                ,tempMap[i*xRes+k]);
        inc(samplesFlushed);
        system.leaveCriticalSection(flushCs);
      end;
      setLength(tempMap,0);
    end;
  end;

FUNCTION newFactorTable:P_generalImageGenrationAlgorithm; begin new(P_factorTable(result),create); end;
INITIALIZATION
  registerAlgorithm('FactorTables',@newFactorTable,true,false,false);
end.
