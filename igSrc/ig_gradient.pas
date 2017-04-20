UNIT ig_gradient;
INTERFACE
USES imageGeneration,mypics,myParams,myColors,myTools;
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
  FUNCTION prepareImage(CONST context:T_imageGenerationContext):boolean; virtual;
end;

IMPLEMENTATION
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

FUNCTION T_colorGradientAlgorithm.prepareImage(CONST context: T_imageGenerationContext): boolean;
  VAR x,y:longint;
      nx,ny,w:single;
      dc:T_rgbFloatColor;
  begin with context do begin
    queue^.forceStart(et_stepCounter_parallel,targetImage^.height);
    dc:=c1-c0;
    nx:=2*system.cos(pi/180*angle)/targetImage^.diagonal;
    ny:=2*system.sin(pi/180*angle)/targetImage^.diagonal;
    for y:=0 to targetImage^.height-1 do
    for x:=0 to targetImage^.width-1 do begin
      w:=(x-targetImage^.width/2)*nx+(y-targetImage^.height/2)*ny;
      if      w> 1 then w:=1
      else if w<-1 then w:=0
      else w:=(w+1)*0.5;
      targetImage^[x,y]:=c0+dc*w;
    end;
    queue^.logEnd;
    result:=true;
  end; end;

FUNCTION newColorGradientAlgorithm:P_generalImageGenrationAlgorithm;
  begin
    new(P_colorGradientAlgorithm(result),create);
  end;

INITIALIZATION
  registerAlgorithm('Linear_gradient',@newColorGradientAlgorithm,false,false,false);

end.

