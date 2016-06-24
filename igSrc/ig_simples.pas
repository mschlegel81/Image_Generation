UNIT ig_simples;
INTERFACE
USES imageGeneration,myParams,complex,myColors,math;
TYPE
P_simpleGenerator=^T_simpleGenerator;
T_simpleGenerator=object(T_functionPerPixelAlgorithm)
  genType:byte;
  CONSTRUCTOR create;
  PROCEDURE resetParameters(CONST style:longint); virtual;
  FUNCTION numberOfParameters:longint; virtual;
  PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
  FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
  FUNCTION getColorAt(CONST ix,iy:longint; CONST xy:T_Complex):T_floatColor; virtual;
end;

IMPLEMENTATION
CONSTRUCTOR T_simpleGenerator.create;
  CONST patterns:array[0..13] of string=('Stripes 1',
      'Checkerboard 1',
      'Stripes 2',
      'Checkerboard 2',
      'Stripes 3',
      'Checkerboard 3',
      'Checkerboard 4',
      'Sinus 1',
      'Cosinus 1',
      'Sinus 2',
      'Cosinus 2',
      'Sinus 3',
      'Cosinus 4',
      'Sierpinski Triangle');
  begin
    inherited create;
    addParameter('Pattern',pt_enum,0,12)^.setEnumValues(patterns);
    resetParameters(0);
  end;

PROCEDURE T_simpleGenerator.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(style);
    renderTolerance:=1;
    genType:=1;
  end;

FUNCTION T_simpleGenerator.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters+1;
  end;

PROCEDURE T_simpleGenerator.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else genType:=value.i0;
  end;

FUNCTION T_simpleGenerator.getParameter(CONST index: byte): T_parameterValue;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index));
    result.createFromValue(parameterDescription(inherited numberOfParameters),genType);
  end;

FUNCTION T_simpleGenerator.getColorAt(CONST ix, iy: longint; CONST xy: T_Complex): T_floatColor;
{$Q-}
  FUNCTION stripe(CONST x:T_Complex):T_floatColor; inline;
    begin
      result:=(trunc((x.im-floor(x.im))*6) and 1)*white;
    end;

  FUNCTION checkerboard(CONST x:T_Complex):T_floatColor; inline;
    begin
      result:=((trunc((x.im-floor(x.im))*6)+trunc((x.re-floor(x.re))*6)  ) and 1)*white;
    end;

  FUNCTION refinedGrid(x:T_Complex):T_floatColor; inline;
    VAR i:longint=10;
    begin
      while ((x.re>1) or (x.re<-1) or
             (x.im>1) or (x.im<-1)) and (i>0) do begin
        x.re:=x.re*0.5;
        x.im:=x.im*0.5;
        dec(i);
      end;
      result:=white*((floor(x.re*16+16)+floor(x.im*16+16)) and 1);
    end;

  FUNCTION tri(CONST x:T_Complex; CONST depth:longint):T_floatColor;
    VAR i,j,k:longint;
    begin
      if (x.re<0) or (x.re>1) or (x.im<0) or (x.im>1) then result:=black
      else begin
        i:=trunc(x.re*(1 shl depth));
        j:=trunc(x.im*(1 shl depth));
        k:=depth;
        while (k>=0) do begin
          if (i and (1 shl k)=0) or (j and (1 shl k)=0)
            then dec(k)
            else k:=-10;
        end;
        if k<-1 then result:=black
                else result:=white;
      end;
    end;

  VAR c:T_Complex;
  begin
    case genType of
      0: {'Stripes 1'}      result:=stripe(xy);
      1: {'Checkerboard 1'} result:=checkerboard(xy);
      2: {'Stripes 2'}      begin c.re:=1/xy.re; c.im:=1/xy.im; result:=stripe(c); end;
      3: {'Checkerboard 2'} begin c.re:=1/xy.re; c.im:=1/xy.im; result:=checkerboard(c); end;
      4: {'Stripes 3'}      result:=stripe(1/xy);
      5: {'Checkerboard 3'} result:=checkerboard(1/xy);
      6: {'Checkerboard 4'} result:=refinedGrid(xy);
      7: {'Sinus 1'}        result:=white*(0.5+0.5*system.sin(abs(xy)));
      8: {'Cosinus 1'}      result:=white*(0.5+0.5*system.cos(abs(xy)));
      9: {'Sinus 2'}        result:=white*(0.5+0.5*system.sin(abs(xy.re*xy.re+xy.im*xy.im)));
      10: {'Cosinus 2'}     result:=white*(0.5+0.5*system.cos(abs(xy.re*xy.re+xy.im*xy.im)));
      11: {'Sinus 3'}       result:=white*(0.5+0.5*system.sin(1/abs(xy)));
      12: {'Cosinus 4'}     result:=white*(0.5+0.5*system.cos(1/abs(xy)));
      13: {'Sierpinski Triangle'} result:=tri(xy,30);
    end;
  end;

FUNCTION newSimpleGenerator:P_generalImageGenrationAlgorithm;
  begin
    new(P_simpleGenerator(result),create);
  end;

INITIALIZATION
  registerAlgorithm('Simple forms',@newSimpleGenerator,true,false,false);
end.
