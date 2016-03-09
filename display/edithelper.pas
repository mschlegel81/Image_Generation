UNIT editHelper;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  StdCtrls, workflows,myParams, Grids;

TYPE

  { TEditHelperForm }

  TEditHelperForm = class(TForm)
    edit: TEdit;
    GroupBox1: TGroupBox;
    ValueListEditor1: TValueListEditor;
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE ValueListEditor1ValidateEntry(Sender: TObject; aCol,
      aRow: integer; CONST oldValue: string; VAR newValue: string);
  private
    { private declarations }
  public
    { public declarations }
    idx:longint;
    descriptor: P_parameterDescription;
    PROCEDURE init(CONST workflowStepIndex:longint);
  end;

VAR
  EditHelperForm: TEditHelperForm;

IMPLEMENTATION

{$R *.lfm}

{ TEditHelperForm }

PROCEDURE TEditHelperForm.ValueListEditor1ValidateEntry(Sender: TObject; aCol, aRow: integer; CONST oldValue: string; VAR newValue: string);
  VAR newParam:T_parameterValue;
  begin
    if idx<0 then exit;
    if newValue=oldValue then exit;
    newParam.createToParse(descriptor^.getSubDescription(aRow-1),newValue);
    if newParam.isValid then begin
      descriptor^.setSubParameter(aRow-1,workflow.step[idx].param,newParam);
      edit.text:=workflow.step[idx].toString();
    end else newValue:=oldValue;
  end;

PROCEDURE TEditHelperForm.FormShow(Sender: TObject);
  VAR i:longint;
  begin
    if idx<0 then exit;
    edit.text:=workflow.step[idx].toString();
    ValueListEditor1.RowCount:=descriptor^.subCount+1;
    for i:=0 to descriptor^.subCount-1 do begin
      ValueListEditor1.Cells[0,i+1]:=descriptor^.getSubDescription(i)^.name;
      ValueListEditor1.Cells[1,i+1]:=descriptor^.getSubParameter(i,workflow.step[idx].param).toString();
    end;
  end;

PROCEDURE TEditHelperForm.FormCreate(Sender: TObject);
  begin
    idx:=-1;
    descriptor:=nil;
  end;

PROCEDURE TEditHelperForm.init(CONST workflowStepIndex: longint);
  begin
    idx:=workflowStepIndex;
    if (idx<0) or (idx>=workflow.stepCount) then exit;
    descriptor:=workflow.step[idx].descriptor;
    if descriptor^.subCount<=0 then exit;
    ShowModal;
  end;

end.

