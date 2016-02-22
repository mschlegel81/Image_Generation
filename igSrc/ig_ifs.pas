UNIT ig_ifs;
INTERFACE
USES imageGeneration,myColors,complex,myParams;
TYPE
  T_Trafo=record
       rgb:T_floatColor;
       con:T_Complex;
       lin,
       qdr:array[0..1] of T_Complex;
     end;

  T_TrafoTriplet=array[0..2] of T_Trafo;

  { T_ifs }

  T_ifs=object(T_scaledImageGenerationAlgorithm)
    PAR_ALPHA  :double ;//=0.125;
    PAR_DEPTH  :longint;//=128;
    PAR_SEED   :byte   ;//=3;
    PAR_COLOR  :byte   ;//=0;
    PAR_BRIGHT :single ;//=1;
    PAR_SYMMEX :byte   ;//=0;
    PAR_TRAFO  :array[0..2] of T_TrafoTriplet; //=3*18=54

    CONSTRUCTOR create;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    FUNCTION prepareImage(CONST forPreview:boolean=false; CONST waitForFinish:boolean=false):boolean; virtual;
  end;

IMPLEMENTATION

PROCEDURE T_ifs.resetParameters(CONST style: longint);
  begin
    inherited resetParameters(0);
  end;

FUNCTION T_ifs.getAlgorithmName: ansistring;
begin

end;

FUNCTION T_ifs.numberOfParameters: longint;
begin

end;

PROCEDURE T_ifs.setParameter(CONST index: byte; CONST value: T_parameterValue);
begin

end;

FUNCTION T_ifs.getParameter(CONST index: byte): T_parameterValue;
begin

end;

FUNCTION T_ifs.prepareImage(CONST forPreview: boolean): boolean;
begin

end;

INITIALIZATION

FINALIZATION

end.
