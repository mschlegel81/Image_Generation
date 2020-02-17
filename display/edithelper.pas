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
    stepIndex     :longint;
    descriptor    :P_parameterDescription;
    parameterValue:T_parameterValue;
    oldSpecification:string;
    FUNCTION editStep(CONST workflow:P_editorWorkflow; CONST index:longint):boolean;
  end;

FUNCTION showEditHelperForm(CONST workflow:P_editorWorkflow; CONST index:longint):boolean;
IMPLEMENTATION
USES workflowSteps;
VAR
  EditHelperForm: TEditHelperForm=nil;

FUNCTION showEditHelperForm(CONST workflow: P_editorWorkflow; CONST index: longint):boolean;
  begin
    if EditHelperForm=nil
    then EditHelperForm:=TEditHelperForm.create(nil);
    result:=EditHelperForm.editStep(workflow,index);
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
      e:string;
  begin
    edit.text:=parameterValue.toString(tsm_forSerialization);
    ValueListEditor1.RowCount:=descriptor^.subCount+1;
    for i:=0 to descriptor^.subCount-1 do begin
      ValueListEditor1.Cells[0,i+1]:=descriptor^.getSubDescription(i)^.getName;
      ValueListEditor1.Cells[1,i+1]:=descriptor^.getSubParameter(i,parameterValue).toString();
      if descriptor^.getSubDescription(i)^.getType=pt_enum then with ValueListEditor1.ItemProps[i] do begin
        EditStyle:=esPickList;
        readonly:=true;
        PickList.clear;
        for e in descriptor^.getSubDescription(i)^.getEnumValues do PickList.add(e);
      end else ValueListEditor1.ItemProps[i].EditStyle:=esSimple;
    end;
  end;

FUNCTION TEditHelperForm.editStep(CONST workflow: P_editorWorkflow; CONST index: longint):boolean;
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
    descriptor:=parameterValue.description;
    if (ShowModal=mrOk) and not(parameterValue.strEq(originalParameter^)) then begin
      originalParameter^.copyFrom(parameterValue);
      workflow^.stepChanged(index);
      result:=true;
    end else result:=false;
  end;

FINALIZATION
  if EditHelperForm<>nil then FreeAndNil(EditHelperForm);

end.

