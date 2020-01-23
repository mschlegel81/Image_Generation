UNIT imageManipulation;
INTERFACE
USES myParams,
     imageContexts;
TYPE
T_simpleOperationKind=(sok_inputDependent,
                       sok_inputIndependent,
                       sok_combiningStash,
                       sok_restoringStash,
                       sok_writingStash);
F_simpleImageOperation=PROCEDURE(CONST parameters:T_parameterValue; CONST context:P_imageGenerationContext);
P_simpleImageOperationMeta=^T_simpleImageOperationMeta;

{ T_simpleImageOperationMeta }

T_simpleImageOperationMeta=object(T_imageOperationMeta)
  private
    kind      :T_simpleOperationKind;
    operation:F_simpleImageOperation;
    signature:P_parameterDescription;
  public
    CONSTRUCTOR create(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation; CONST simpleOperationKind:T_simpleOperationKind);
    DESTRUCTOR destroy; virtual;
    FUNCTION parse(CONST specification:ansistring):P_imageOperation; virtual;
    FUNCTION getSimpleParameterDescription:P_parameterDescription; virtual;
  end;

P_simpleImageOperation=^T_simpleImageOperation;
T_simpleImageOperation=object(T_imageOperation)
  private
    meta      :P_simpleImageOperationMeta;
    parameters:T_parameterValue;
  public
    CONSTRUCTOR create(CONST meta_:P_simpleImageOperationMeta; CONST parameters_:T_parameterValue);
    PROCEDURE execute(CONST context:P_imageGenerationContext); virtual;
    FUNCTION getSimpleParameterValue:P_parameterValue; virtual;
    FUNCTION isSingleton:boolean; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION readsStash:string; virtual;
    FUNCTION writesStash:string; virtual;
    FUNCTION dependsOnImageBefore:boolean; virtual;
  end;

PROCEDURE registerSimpleOperation(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation; CONST kind:T_simpleOperationKind=sok_inputDependent);
IMPLEMENTATION
PROCEDURE registerSimpleOperation(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation; CONST kind:T_simpleOperationKind=sok_inputDependent);
  VAR meta:P_simpleImageOperationMeta;
  begin
    new(meta,create(cat_,sig,op,kind));
    registerOperation(meta);
  end;

{ T_simpleImageOperation }

CONSTRUCTOR T_simpleImageOperation.create(CONST meta_: P_simpleImageOperationMeta; CONST parameters_: T_parameterValue);
  begin
    meta:=meta_;
    parameters:=parameters_;
  end;

PROCEDURE T_simpleImageOperation.execute(CONST context: P_imageGenerationContext);
  begin
    meta^.operation(parameters,context);
  end;

FUNCTION T_simpleImageOperation.getSimpleParameterValue: P_parameterValue;
  begin
    result:=@parameters;
  end;

FUNCTION T_simpleImageOperation.isSingleton: boolean;
  begin
    result:=false;
  end;

DESTRUCTOR T_simpleImageOperation.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_simpleImageOperation.readsStash: string;
  begin
    if meta^.kind in [sok_combiningStash,sok_restoringStash]
    then result:=parameters.fileName
    else result:='';
  end;

FUNCTION T_simpleImageOperation.writesStash: string;
  begin
    if meta^.kind=sok_writingStash
    then result:=parameters.fileName
    else result:='';
  end;

FUNCTION T_simpleImageOperation.dependsOnImageBefore: boolean;
  begin
    result:=meta^.kind in [sok_inputDependent,sok_combiningStash,sok_writingStash];
  end;

CONSTRUCTOR T_simpleImageOperationMeta.create(CONST cat_: T_imageManipulationCategory; CONST sig: P_parameterDescription; CONST op: F_simpleImageOperation; CONST simpleOperationKind:T_simpleOperationKind);
  begin
    inherited create(sig^.getName,cat_);
    signature:=sig;
    operation:=op;
    kind:=simpleOperationKind;
  end;

DESTRUCTOR T_simpleImageOperationMeta.destroy;
  begin
    inherited destroy;
  end;

FUNCTION T_simpleImageOperationMeta.parse(CONST specification: ansistring): P_imageOperation;
  VAR value:T_parameterValue;
      op:P_simpleImageOperation;
  begin
    value.createToParse(signature,specification,tsm_forSerialization);
    if value.isValid then begin
      new(op,create(@self,value));
      result:=op;
    end else result:=nil;
  end;

FUNCTION T_simpleImageOperationMeta.getSimpleParameterDescription: P_parameterDescription;
  begin
    result:=signature;
  end;

PROCEDURE loadImage_impl(CONST parameters:T_parameterValue; CONST context:P_imageGenerationContext);
  begin
    context^.image.loadFromFile(parameters.fileName);
  end;

PROCEDURE saveImage_impl(CONST parameters:T_parameterValue; CONST context:P_imageGenerationContext);
  begin
    if parameters.description^.getType=pt_jpgNameWithSize
    then context^.image.saveJpgWithSizeLimit(parameters.fileName,parameters.i0)
    else context^.image.saveToFile(parameters.fileName);
  end;

INITIALIZATION
  registerSimpleOperation(imc_imageAccess,
                          newParameterDescription('load',pt_fileName),
                          @loadImage_impl,
                          sok_inputIndependent);
  registerSimpleOperation(imc_imageAccess,
                          newParameterDescription('save',pt_fileName)^.setDefaultValue('filename.jpg'),
                          @saveImage_impl);
  registerSimpleOperation(imc_imageAccess,
                          newParameterDescription('save',pt_jpgNameWithSize)^.setDefaultValue('image.jpg@1M'),
                          @saveImage_impl);

end.

