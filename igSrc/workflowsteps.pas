UNIT workflowSteps;
INTERFACE
USES
  myParams,
  mypics,
  imageContexts;
TYPE
P_workflowStep=^T_workflowStep;

{ T_workflowStep }

T_workflowStep=object
  private
    specString   :string;
    valid        :boolean;
    operation_   :P_imageOperation;
    PROCEDURE setSpecification(CONST spec:string);
  public
    outputImage:P_rawImage;
    CONSTRUCTOR create(CONST spec:string);
    CONSTRUCTOR create(CONST op:P_imageOperation);
    DESTRUCTOR destroy;
    PROCEDURE execute(CONST context:P_abstractWorkflow);
    PROPERTY specification:string read specString write setSpecification;
    PROPERTY isValid:boolean read valid;
    PROPERTY operation:P_imageOperation read operation_;
    PROCEDURE clearOutputImage;
    PROCEDURE saveOutputImage(VAR image:T_rawImage);
    FUNCTION toStringPart(CONST configPart:boolean):string;
    FUNCTION hasComplexParameterDescription:boolean;
end;

IMPLEMENTATION
PROCEDURE T_workflowStep.setSpecification(CONST spec: string);
  VAR meta:P_imageOperationMeta;
  begin
    if specString=spec then exit;
    specString:=spec;
    if (operation_<>nil) then dispose(operation_,destroy);
    operation_:=nil;
    for meta in imageOperations do if operation_=nil then operation_:=meta^.parse(specString);
    valid:=operation_<>nil;
    clearOutputImage;
  end;

CONSTRUCTOR T_workflowStep.create(CONST spec: string);
  begin
    operation_:=nil;
    setSpecification(spec);
    outputImage:=nil;
  end;

CONSTRUCTOR T_workflowStep.create(CONST op: P_imageOperation);
  begin
    operation_:=op;
    specString:=op^.toString(tsm_forSerialization);
    valid     :=true;
    outputImage:=nil;
  end;

DESTRUCTOR T_workflowStep.destroy;
  begin
    if outputImage<>nil then dispose(outputImage,destroy);
    if (operation_<>nil) then dispose(operation_,destroy);
  end;

PROCEDURE T_workflowStep.execute(CONST context: P_abstractWorkflow);
  begin
    if valid then begin
      context^.messageQueue^.Post(specification,false,context^.currentStepIndex);
      operation_^.execute(context);
    end else begin
      context^.cancelWithError('Invalid step: '+specification);
    end;
  end;

PROCEDURE T_workflowStep.clearOutputImage;
  begin
    if outputImage<>nil then dispose(outputImage,destroy);
    outputImage:=nil;
  end;

PROCEDURE T_workflowStep.saveOutputImage(VAR image: T_rawImage);
  begin
    if outputImage=nil
    then new(outputImage,create(image))
    else outputImage^.copyFromPixMap(image);
  end;

FUNCTION T_workflowStep.toStringPart(CONST configPart: boolean): string;
  begin
    if operation=nil then begin
      if configPart
      then result:=specification
      else result:='<invalid>';
    end else begin
      if configPart
      then begin
        result:=operation^.toString(tsm_withoutParameterName);
      end else begin
        result:=operation^.meta^.getName;
      end;
    end;
  end;

FUNCTION T_workflowStep.hasComplexParameterDescription: boolean;
  begin
    result:=isValid and ((operation^.meta^.category=imc_generation)
                      or (operation^.meta^.getSimpleParameterDescription<>nil)
                     and (operation^.meta^.getSimpleParameterDescription^.subCount>0));
  end;

end.

