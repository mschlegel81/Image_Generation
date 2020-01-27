UNIT ig_gradient;
INTERFACE
USES myColors,
     myParams,
     imageContexts,
     imageGeneration;
TYPE
P_colorGradientAlgorithm=^T_colorGradientAlgorithm;
T_colorGradientAlgorithm=object(T_generalImageGenrationAlgorithm)
  c0,c1:T_rgbFloatColor;
  angle:double;

  CONSTRUCTOR create;
  PROCEDURE resetParameters(CONST style:longint); virtual;
  FUNCTION numberOfParameters:longint; virtual;
  PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
  FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
  PROCEDURE execute(CONST context:P_abstractWorkflow); virtual;
end;

IMPLEMENTATION
USES mypics;
CONSTRUCTOR T_colorGradientAlgorithm.create;
  begin
    inherited create;
    addParameter('Angle (degrees)',pt_float);
    addParameter('Color 1'        ,pt_color);
    addParameter('Color 2'        ,pt_color);
    resetParameters(0);
  end;

PROCEDURE T_colorGradientAlgorithm.resetParameters(CONST style: longint);
  begin
    c0:=BLACK;
    c1:=WHITE;
    angle:=-45;
  end;

FUNCTION T_colorGradientAlgorithm.numberOfParameters: longint;
  begin
    result:=3;
  end;

PROCEDURE T_colorGradientAlgorithm.setParameter(CONST index: byte; CONST value: T_parameterValue);
  begin
    case index of
      0: angle:=value.f0;
      1: c0:=value.color;
      2: c1:=value.color;
    end;
  end;

FUNCTION T_colorGradientAlgorithm.getParameter(CONST index: byte): T_parameterValue;
  begin
    case index of
      0: result:=parValue(index,angle);
      1: result:=parValue(index,c0);
    else result:=parValue(index,c1);
    end;
  end;

PROCEDURE T_colorGradientAlgorithm.execute(CONST context:P_abstractWorkflow);
  VAR x,y:longint;
      nx,ny,w:single;
      dc:T_rgbFloatColor;
  begin with context^ do begin
    dc:=c1-c0;
    nx:=2*system.cos(pi/180*angle)/image.diagonal;
    ny:=2*system.sin(pi/180*angle)/image.diagonal;
    for y:=0 to image.dimensions.height-1 do
    for x:=0 to image.dimensions.width-1 do begin
      w:=(x-image.dimensions.width/2)*nx+(y-image.dimensions.height/2)*ny;
      if      w> 1 then w:=1
      else if w<-1 then w:=0
      else w:=(w+1)*0.5;
      image[x,y]:=c0+dc*w;
    end;
  end; end;

FUNCTION newColorGradientAlgorithm:P_generalImageGenrationAlgorithm;
  begin
    new(P_colorGradientAlgorithm(result),create);
  end;

INITIALIZATION
  registerAlgorithm('Linear_gradient',@newColorGradientAlgorithm,false,false,false);

end.

