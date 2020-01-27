UNIT imageManipulation;
INTERFACE
USES myParams,
     pixMaps,
     imageContexts;
TYPE
T_simpleOperationKind=(sok_inputDependent,
                       sok_inputIndependent,
                       sok_combiningStash,
                       sok_restoringStash,
                       sok_writingStash);
F_simpleImageOperation=PROCEDURE(CONST parameters:T_parameterValue; CONST context:P_imageGenerationContext);
P_simpleImageOperationMeta=^T_simpleImageOperationMeta;
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
    FUNCTION getDefaultOperation:P_imageOperation; virtual;
  end;

P_simpleImageOperation=^T_simpleImageOperation;
T_simpleImageOperation=object(T_imageOperation)
  private
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
    FUNCTION toString(nameMode:T_parameterNameMode):string; virtual;
    FUNCTION alterParameter(CONST newParameterString:string):boolean; virtual;
  end;

FUNCTION registerSimpleOperation(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation; CONST kind:T_simpleOperationKind=sok_inputDependent):P_simpleImageOperationMeta;
FUNCTION canParseResolution(CONST s:string; OUT dim:T_imageDimensions):boolean;
FUNCTION canParseSizeLimit(CONST s:string; OUT size:longint):boolean;
FUNCTION getSaveStatement(CONST savingToFile:string; CONST savingWithSizeLimit:longint):string;
IMPLEMENTATION
USES generationBasics,sysutils;
VAR pd_save                :P_parameterDescription=nil;
    pd_save_with_size_limit:P_parameterDescription=nil;
    pd_resize              :P_parameterDescription=nil;

FUNCTION registerSimpleOperation(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation; CONST kind:T_simpleOperationKind=sok_inputDependent):P_simpleImageOperationMeta;
  begin
    new(result,create(cat_,sig,op,kind));
    registerOperation(result);
  end;

FUNCTION canParseResolution(CONST s: string; OUT dim: T_imageDimensions): boolean;
  VAR p:T_parameterValue;
  begin
    p.createToParse(pd_resize,s);
    dim:=imageDimensions(p.i0,p.i1);
    result:=p.isValid;
  end;

FUNCTION canParseSizeLimit(CONST s: string; OUT size: longint): boolean;
  VAR p:T_parameterValue;
  begin
    p.createToParse(pd_save_with_size_limit,'dummy.jpg@'+s);
    size:=p.i0;
    result:=p.isValid;
  end;

FUNCTION getSaveStatement(CONST savingToFile: string; CONST savingWithSizeLimit: longint): string;
  VAR p:T_parameterValue;
  begin
    if (uppercase(extractFileExt(savingToFile))=SIZE_LIMITABLE_EXTENSION) and (savingWithSizeLimit>0) then begin
      p.createFromValue(pd_save_with_size_limit,savingToFile,savingWithSizeLimit);
      result:=p.toString(tsm_forSerialization);
    end else begin
      p.createFromValue(pd_save,savingToFile);
      result:=p.toString(tsm_forSerialization);
    end;
  end;

CONSTRUCTOR T_simpleImageOperation.create(
  CONST meta_: P_simpleImageOperationMeta; CONST parameters_: T_parameterValue);
  begin
    inherited create(meta_);
    parameters:=parameters_;
  end;

PROCEDURE T_simpleImageOperation.execute(CONST context: P_imageGenerationContext
  );
  begin
    P_simpleImageOperationMeta(meta)^.operation(parameters,context);
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
    if P_simpleImageOperationMeta(meta)^.kind in [sok_combiningStash,sok_restoringStash]
    then result:=parameters.fileName
    else result:='';
  end;

FUNCTION T_simpleImageOperation.writesStash: string;
  begin
    if P_simpleImageOperationMeta(meta)^.kind=sok_writingStash
    then result:=parameters.fileName
    else result:='';
  end;

FUNCTION T_simpleImageOperation.dependsOnImageBefore: boolean;
  begin
    result:=P_simpleImageOperationMeta(meta)^.kind in [sok_inputDependent,sok_combiningStash,sok_writingStash];
  end;

FUNCTION T_simpleImageOperation.toString(nameMode: T_parameterNameMode): string;
  begin
    result:=parameters.toString(nameMode);
  end;

FUNCTION T_simpleImageOperation.alterParameter(CONST newParameterString: string): boolean;
  begin
    result:= parameters.canParse(newParameterString);
  end;

CONSTRUCTOR T_simpleImageOperationMeta.create(
  CONST cat_: T_imageManipulationCategory; CONST sig: P_parameterDescription;
  CONST op: F_simpleImageOperation;
  CONST simpleOperationKind: T_simpleOperationKind);
  begin
    inherited create(sig^.getName,cat_);
    signature:=sig;
    operation:=op;
    kind:=simpleOperationKind;
  end;

DESTRUCTOR T_simpleImageOperationMeta.destroy;
  begin
    inherited destroy;
    dispose(signature,destroy);
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

FUNCTION T_simpleImageOperationMeta.getDefaultOperation: P_imageOperation;
  VAR op:P_simpleImageOperation;
  begin
    new(op,create(@self,signature^.getDefaultParameterValue));
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
                          newParameterDescription(C_loadStatementName,pt_fileName),
                          @loadImage_impl,
                          sok_inputIndependent);
  pd_save:=
  registerSimpleOperation(imc_imageAccess,
                          newParameterDescription('save',pt_fileName)^.setDefaultValue('filename.jpg'),
                          @saveImage_impl)^.getSimpleParameterDescription;
  pd_save_with_size_limit:=
  registerSimpleOperation(imc_imageAccess,
                          newParameterDescription('save',pt_jpgNameWithSize)^.setDefaultValue('image.jpg@1M'),
                          @saveImage_impl)^.getSimpleParameterDescription;

end.

