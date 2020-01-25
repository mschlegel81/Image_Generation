UNIT editHelper;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  StdCtrls, workflows, myParams, Grids;

TYPE

  { TEditHelperForm }

  TEditHelperForm = class(TForm)
    CancelButton: TButton;
    OKButton: TButton;
    edit: TEdit;
    GroupBox1: TGroupBox;
    ValueListEditor1: TValueListEditor;
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ValueListEditor1ValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  private
    { private declarations }
    workflow      :P_imageWorkflow;
    stepIndex     :longint;
    descriptor    :P_parameterDescription;
    parameterValue:T_parameterValue;
    oldSpecification:string;
    PROCEDURE init(CONST workflow_:P_imageWorkflow; CONST index:longint);
  end;

PROCEDURE showEditHelperForm(CONST workflow_:P_imageWorkflow; CONST index:longint);
IMPLEMENTATION
USES workflowSteps;
VAR
  EditHelperForm: TEditHelperForm=nil;

PROCEDURE showEditHelperForm(CONST workflow_: P_imageWorkflow; CONST index: longint);
  begin
    if EditHelperForm=nil
    then EditHelperForm:=TEditHelperForm.create(nil);
    EditHelperForm.init(workflow_,index);
  end;

{$R *.lfm}

PROCEDURE TEditHelperForm.ValueListEditor1ValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR newParam:T_parameterValue;
  begin
    if newValue=oldValue then exit;
    newParam.createToParse(descriptor^.getSubDescription(aRow-1),newValue);
    if newParam.isValid then begin
      descriptor^.setSubParameter(aRow-1,parameterValue,newParam);
      edit.text:=parameterValue.toString(tsm_forSerialization);
    end else newValue:=oldValue;
  end;

PROCEDURE TEditHelperForm.FormShow(Sender: TObject);
  VAR i:longint;
  begin
    edit.text:=parameterValue.toString(tsm_forSerialization);
    ValueListEditor1.RowCount:=descriptor^.subCount+1;
    for i:=0 to descriptor^.subCount-1 do begin
      ValueListEditor1.Cells[0,i+1]:=descriptor^.getSubDescription(i)^.getName;
      ValueListEditor1.Cells[1,i+1]:=descriptor^.getSubParameter(i,parameterValue).toString();
    end;
  end;

PROCEDURE TEditHelperForm.init(CONST workflow_: P_imageWorkflow; CONST index: longint);
  VAR step:P_workflowStep;
      originalParameter:P_parameterValue;
  begin
    if workflow^.executing or (index<0) or (index>=workflow^.stepCount) then exit;
    stepIndex:=index;
    step:=workflow^.step[index];
    if (step=nil) or (step^.operation=nil) then exit;
    originalParameter:=step^.operation^.getSimpleParameterValue;
    if (originalParameter=nil) or (originalParameter^.description^.subCount<=0) then exit;
    oldSpecification:=step^.specification;
    parameterValue.createCopy(originalParameter);
    if (ShowModal=mrOk) and not(parameterValue.strEq(originalParameter^)) then begin
      originalParameter^.copyFrom(parameterValue);
      workflow_^.stepChanged(index);
    end;
  end;

FINALIZATION
  if EditHelperForm<>nil then FreeAndNil(EditHelperForm);

end.

