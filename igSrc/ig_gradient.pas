UNIT ig_gradient;
INTERFACE
USES imageGeneration,myParams,myColors,myTools;
TYPE
T_colorGradientAlgorithm=object(T_generalImageGenrationAlgorithm)
  c0,c1:T_floatColor;
  angle:double;

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
CONSTRUCTOR T_colorGradientAlgorithm.create;
  begin
    inherited create;
    addParameter('Angle (degrees)',pt_float);
    addParameter('Color 1'        ,pt_color);
    addParameter('Color 2'        ,pt_color);
    resetParameters(0);
  end;
DESTRUCTOR T_colorGradientAlgorithm.destroy;  begin end;
FUNCTION T_colorGradientAlgorithm.getAlgorithmName: ansistring; begin result:='Linear_gradient'; end;
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
      0: result.createFromValue(parameterDescription(0),angle);
      1: result.createFromValue(parameterDescription(1),c0);
      2: result.createFromValue(parameterDescription(2),c1);
    end;
  end;

FUNCTION T_colorGradientAlgorithm.prepareImage(CONST forPreview: boolean):boolean;
  VAR x,y:longint;
      nx,ny,w:single;
      dc:T_floatColor;
  begin
    progressQueue.forceStart(et_stepCounter_parallel,renderImage.height);
    dc:=c1-c0;
    nx:=2*system.cos(pi/180*angle)/renderImage.diagonal;
    ny:=2*system.sin(pi/180*angle)/renderImage.diagonal;
    for y:=0 to renderImage.height-1 do
    for x:=0 to renderImage.width-1 do begin
      w:=(x-renderImage.width/2)*nx+(y-renderImage.height/2)*ny;
      if      w> 1 then w:=1
      else if w<-1 then w:=0
      else w:=(w+1)*0.5;
      renderImage[x,y]:=c0+w*dc;
    end;
    progressQueue.logEnd;
    result:=true;
  end;

VAR colorGradientAlgorithm:T_colorGradientAlgorithm;
INITIALIZATION
  colorGradientAlgorithm.create; registerAlgorithm(@colorGradientAlgorithm,false,false,false);
FINALIZATION
  colorGradientAlgorithm.destroy;

end.

