UNIT ig_julia_fractals;
INTERFACE
USES imageGeneration,mypics,myColors,complex,myParams,math,mySys,sysutils,myTools,myGenerics,ig_fractals;
CONST JULIA_COORD_INDEX=12;
TYPE
  P_functionPerPixelViaRawDataJuliaAlgorithm=^T_functionPerPixelViaRawDataJuliaAlgorithm;
  T_functionPerPixelViaRawDataJuliaAlgorithm=object(T_functionPerPixelViaRawDataAlgorithm)
    julianess:double;
    juliaParam:T_Complex;
    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
  end;

  T_mandelbrot=object(T_functionPerPixelViaRawDataJuliaAlgorithm)
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); virtual;
    PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); virtual;
  end;

  T_mandelbar=object(T_functionPerPixelViaRawDataJuliaAlgorithm)
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); virtual;
    PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); virtual;
  end;

  T_burningJulia=object(T_functionPerPixelViaRawDataJuliaAlgorithm)
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    PROCEDURE iterationStart(VAR c:T_Complex; OUT x:T_Complex); virtual;
    PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); virtual;
  end;

  T_burningJulia2=object(T_burningJulia)
    FUNCTION getAlgorithmName:ansistring; virtual;
    PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); virtual;
  end;

  T_burningJulia3=object(T_burningJulia)
    FUNCTION getAlgorithmName:ansistring; virtual;
    PROCEDURE iterationStep(CONST c:T_Complex; VAR x:T_Complex); virtual;
  end;

IMPLEMENTATION
CONSTRUCTOR T_functionPerPixelViaRawDataJuliaAlgorithm.create;
  begin
    inherited create;
    addParameter('Julianess',pt_float);
    addParameter('Julia-Param',pt_2floats);
  end;

PROCEDURE T_functionPerPixelViaRawDataJuliaAlgorithm.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    julianess:=0;
    juliaParam:=0;
  end;

FUNCTION T_functionPerPixelViaRawDataJuliaAlgorithm.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+2;
  end;

PROCEDURE T_functionPerPixelViaRawDataJuliaAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case(byte(index-inherited numberOfParameters)) of
      0: begin julianess:=value.f0; rawMapIsOutdated:=true; end;
      1: begin juliaParam.re:=value.f0; juliaParam.im:=value.f1; rawMapIsOutdated:=true; end;
    end;
  end;

FUNCTION T_functionPerPixelViaRawDataJuliaAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index));
    case byte(index-inherited numberOfParameters) of
      0: result.createFromValue(parameterDescription(inherited numberOfParameters  ),julianess);
      1: result.createFromValue(parameterDescription(inherited numberOfParameters+1),juliaParam.re,juliaParam.im);
    end;
  end;

FUNCTION T_mandelbrot.parameterResetStyles:T_arrayOfString;
  begin
    result:='Mandelbrot Set';
    append(result,'Julia Set');
  end;

PROCEDURE T_mandelbrot.resetParameters(CONST style:longint);
  begin
    inherited resetParameters(style);
    case style of
      1: begin
           julianess:=1;
           juliaParam.re:=-0.8;
           juliaParam.im:=0.156;
         end;
    end;
  end;

FUNCTION T_mandelbrot.getAlgorithmName:ansistring; begin result:='Mandelbrot / Julia'; end;
PROCEDURE T_mandelbrot.iterationStart(VAR c:T_Complex; OUT x:T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
PROCEDURE T_mandelbrot.iterationStep(CONST c:T_Complex; VAR x:T_Complex); begin x:=sqr(x)+c; end;

FUNCTION T_mandelbar.parameterResetStyles:T_arrayOfString;
  begin
    result:='Mandelbar';
    append(result,'Mandelbar Julia Set');
  end;

PROCEDURE T_mandelbar.resetParameters(CONST style:longint);
  begin
    inherited resetParameters(style);
    case style of
      1: begin
           julianess:=1;
           juliaParam.re:=0.2822140;
           juliaParam.im:=0.6813474;
         end;
    end;
  end;

FUNCTION T_mandelbar.getAlgorithmName:ansistring; begin result:='Mandelbar  /-Julia'; end;
PROCEDURE T_mandelbar.iterationStart(VAR c:T_Complex; OUT x:T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
PROCEDURE T_mandelbar.iterationStep(CONST c:T_Complex; VAR x:T_Complex); begin x:=sqr(x); x.im:=-x.im; x:=x+c; end;


FUNCTION T_burningJulia.parameterResetStyles: T_arrayOfString;
  begin
    result:='Burning Ship';
    append(result,'Burning Ship Julia');
  end;

PROCEDURE T_burningJulia.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    case style of
      1: begin
           julianess:=1;
           juliaParam.re:=0.591925608954895;
           juliaParam.im:=0.918404930408219;
         end;
    end;
  end;

FUNCTION T_burningJulia .getAlgorithmName: ansistring; begin result:='Burning Ship /-Julia'; end;
FUNCTION T_burningJulia2.getAlgorithmName: ansistring; begin result:='Burning Ship /-Julia (interp. A)'; end;
FUNCTION T_burningJulia3.getAlgorithmName: ansistring; begin result:='Burning Ship /-Julia (interp. B)'; end;
PROCEDURE T_burningJulia.iterationStart(VAR c: T_Complex; OUT x: T_Complex); begin x:=c; c:=(1-julianess)*c+julianess*juliaParam; end;
PROCEDURE T_burningJulia.iterationStep(CONST c: T_Complex; VAR x: T_Complex);
  VAR x_re:double;
  begin
    x_re:=x.re*x.re-x.im*x.im+c.re;
    if (x.re<0) = (x.im<0)
      then x.im:=c.im-2*x.re*x.im
      else x.im:=c.im+2*x.re*x.im;
    x.re:=x_re;
  end;
PROCEDURE T_burningJulia2.iterationStep(CONST c: T_Complex; VAR x: T_Complex);
  VAR x_re:double;
  begin
    if      x.re<-0.1 then x.re:=-x.re
    else if x.re< 0.1 then x.re:=x.re*x.re*(15-x.re*x.re*500);
    if      x.im<-0.1 then x.im:=-x.im
    else if x.im< 0.1 then x.im:=x.im*x.im*(15-x.im*x.im*500);
    x_re:=c.re+x.re*x.re-x.im*x.im;
    x.im:=c.im-2*x.re*x.im;
    x.re:=x_re;
  end;
PROCEDURE T_burningJulia3.iterationStep(CONST c: T_Complex; VAR x: T_Complex);
  VAR x_re:double;
  begin
    x_re:=c.re+x.re*x.re-x.im*x.im;
    x.im:=2*x.re*x.im;
    if      x.im<-0.1 then x.im:=-x.im
    else if x.im< 0.1 then x.im:=x.im*x.im*(15-x.im*x.im*500);
    x.im:=c.im-x.im;
    x.re:=x_re;
  end;

VAR mandelbrot      : T_mandelbrot;
    mandelbar       : T_mandelbar;
    burningJulia    : T_burningJulia;
    burningJulia2   : T_burningJulia2;
    burningJulia3   : T_burningJulia3;

INITIALIZATION
  mandelbrot      .create; registerAlgorithm(@mandelbrot      ,true,true,true);
  mandelbar       .create; registerAlgorithm(@mandelbar       ,true,true,true);
  burningJulia    .create; registerAlgorithm(@burningJulia    ,true,true,true);
  burningJulia2   .create; registerAlgorithm(@burningJulia2   ,true,true,true);
  burningJulia3   .create; registerAlgorithm(@burningJulia3   ,true,true,true);

FINALIZATION
  mandelbrot      .destroy;
  mandelbar       .destroy;
  burningJulia    .destroy;
  burningJulia2   .destroy;
  burningJulia3   .destroy;
end.

