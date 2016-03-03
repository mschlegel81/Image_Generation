UNIT ig_gradient;
INTERFACE
USES imageGeneration,myParams,myColors,myTools;
TYPE
T_colorGradientAlgorithm=object(T_generalImageGenrationAlgorithm)
  c0,c1:T_floatColor;
  angle:double;

  CONSTRUCTOR create;
  FUNCTION getAlgorithmName:ansistring; virtual;
  PROCEDURE resetParameters(CONST style:longint); virtual;
  FUNCTION numberOfParameters:longint; virtual;
  PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
  FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
  FUNCTION prepareImage(CONST forPreview:boolean=false; CONST waitForFinish:boolean=false):boolean; virtual;
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
FUNCTION T_colorGradientAlgorithm.getAlgorithmName: ansistring; begin result:='Linear_gradient'; end;
PROCEDURE T_colorGradientAlgorithm.resetParameters(CONST style: longint);
  begin
    c0:=black;
    c1:=white;
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
      2: result:=parValue(index,c1);
    end;
  end;

FUNCTION T_colorGradientAlgorithm.prepareImage(CONST forPreview: boolean; CONST waitForFinish:boolean=false):boolean;
  VAR x,y:longint;
      nx,ny,w:single;
      dc:T_floatColor;
  begin
    progressQueue.forceStart(et_stepCounter_parallel,generationImage^.height);
    dc:=c1-c0;
    nx:=2*system.cos(pi/180*angle)/generationImage^.diagonal;
    ny:=2*system.sin(pi/180*angle)/generationImage^.diagonal;
    for y:=0 to generationImage^.height-1 do
    for x:=0 to generationImage^.width-1 do begin
      w:=(x-generationImage^.width/2)*nx+(y-generationImage^.height/2)*ny;
      if      w> 1 then w:=1
      else if w<-1 then w:=0
      else w:=(w+1)*0.5;
      generationImage^[x,y]:=c0+w*dc;
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

