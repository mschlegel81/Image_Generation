UNIT imExModifyDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, mypics, workflows, myParams;

TYPE

  { TmodifyForm }

  TmodifyForm = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    previewButton: TButton;
    cbAllEqual: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Image1: TImage;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
  private
    { private declarations }
    FUNCTION box(CONST index:byte):TGroupBox;
    FUNCTION edit(CONST index:byte):TEdit;
    FUNCTION trackbar(CONST index:byte):TTrackBar;
  public
    step:T_imageManipulationStep;
    FUNCTION showModalFor(CONST imt:T_imageManipulationType; CONST stepIndex:longint; CONST fix:boolean=false; CONST params:P_parameterValue=nil):boolean;
    { public declarations }
  end;

VAR
  modifyForm: TmodifyForm;

IMPLEMENTATION

{$R *.lfm}

{ TmodifyForm }

FUNCTION TmodifyForm.box(CONST index: byte): TGroupBox;
  begin
    case index of
      0: result:=GroupBox1;
      1: result:=GroupBox2;
      2: result:=GroupBox3;
    end;
  end;

FUNCTION TmodifyForm.edit(CONST index: byte): TEdit;
  begin
    case index of
      0: result:=Edit1;
      1: result:=Edit2;
      2: result:=Edit3;
    end;
  end;

FUNCTION TmodifyForm.trackbar(CONST index: byte): TTrackBar;
  begin
    case index of
      0: result:=TrackBar1;
      1: result:=TrackBar2;
      2: result:=TrackBar3;
    end;
  end;

FUNCTION TmodifyForm.showModalFor(CONST imt: T_imageManipulationType; CONST stepIndex: longint; CONST fix: boolean; CONST params: P_parameterValue): boolean;
  VAR i:longint;
      doEnable:boolean;
  begin
    if fix then step.create(imt,params^)
           else step.create(imt,stepParamDescription[imt]^.getDefaultParameterValue);
    for i:=0 to 2 do begin
      doEnable:=length(stepParamDescription[imt]^.children)>i;
      box(i).enabled:=doEnable;
      if doEnable then box(i).caption:=stepParamDescription[imt]^.children[i].description^.name
                  else box(i).caption:='';
      edit(i).enabled:=doEnable;
      if doEnable then edit(i).text:=stepParamDescription[imt]^.getSubParameter(i,step.param).toString
                  else edit(i).text:='';
      trackbar(i).enabled:=doEnable;
    end;
    if ShowModal=mrOk then begin
      while workflow.stepCount>=stepIndex do workflow.remStep(workflow.stepCount-1);
      result:=workflow.addStep(step.toString);
    end else result:=false;
    step.destroy;
  end;

end.

