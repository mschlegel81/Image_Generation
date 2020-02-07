UNIT imExModifyDialog;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Buttons, mypics, workflows, myParams,math,workflowSteps,imageManipulation;

TYPE
  T_doublePair=array[0..1] of double;

  TmodifyForm = class(TForm)
    OKButton: TButton;
    CancelButton: TButton;
    cbAllEqual: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Image1: TImage;
    Panel1: TPanel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    PROCEDURE Edit1Change(Sender: TObject);
    PROCEDURE Edit2Change(Sender: TObject);
    PROCEDURE Edit3Change(Sender: TObject);
    PROCEDURE Edit4Change(Sender: TObject);
    PROCEDURE myEditingDone(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE TrackBar1Change(Sender: TObject);
    PROCEDURE TrackBar2Change(Sender: TObject);
    PROCEDURE TrackBar3Change(Sender: TObject);
    PROCEDURE TrackBar4Change(Sender: TObject);
  private
    { private declarations }
    previewInput,previewOutput:T_rawImage;
    PROCEDURE editChange(CONST index:byte);
    PROCEDURE trackbarChange(CONST index:byte);
    FUNCTION box(CONST index:byte):TGroupBox;
    FUNCTION edit(CONST index:byte):TEdit;
    FUNCTION trackbar(CONST index:byte):TTrackBar;
    PROCEDURE updateEdit(CONST index: byte);
    PROCEDURE updateTrackbar(CONST index: byte=255);
    FUNCTION trackbarValue(CONST index:byte):double;
    FUNCTION trackbarRange(CONST index:byte):T_doublePair;
    FUNCTION editValue(CONST index:byte):double;
    FUNCTION getStepValue(CONST index:byte):double;
    PROCEDURE setStepValue(CONST index:byte; CONST value:double);
  public
    dummyWorkflow:T_standaloneWorkflow;
    meta:P_simpleImageOperationMeta;
    step:T_workflowStep;
    FUNCTION showModalFor(CONST title:string; CONST manipulationMeta:P_simpleImageOperationMeta; CONST stepIndex:longint; CONST fix:boolean=false; CONST params:P_parameterValue=nil):boolean;
    { public declarations }
  end;

VAR
  modifyForm: TmodifyForm;

IMPLEMENTATION

{$R *.lfm}

{ TmodifyForm }

PROCEDURE TmodifyForm.trackbarChange(CONST index: byte);
  begin
    setStepValue(index,trackbarValue(index));
    updateEdit(index);
    if cbAllEqual.checked then begin
      setStepValue  (255-index,getStepValue(index));
      updateTrackbar(255-index);
      updateEdit    (255-index);
    end;
    myEditingDone(nil);
  end;

PROCEDURE TmodifyForm.TrackBar1Change(Sender: TObject); begin trackbarChange(0); end;
PROCEDURE TmodifyForm.TrackBar2Change(Sender: TObject); begin trackbarChange(1); end;
PROCEDURE TmodifyForm.TrackBar3Change(Sender: TObject); begin trackbarChange(2); end;
PROCEDURE TmodifyForm.TrackBar4Change(Sender: TObject); begin trackbarChange(3); end;

PROCEDURE TmodifyForm.editChange(CONST index: byte);
begin
  setStepValue(index,editValue(index));
  updateTrackbar(index);
  if cbAllEqual.checked then begin
    setStepValue  (255-index,getStepValue(index));
    updateTrackbar(255-index);
    updateEdit    (255-index);
  end;
end;

PROCEDURE TmodifyForm.Edit1Change(Sender: TObject); begin editChange(0); end;
PROCEDURE TmodifyForm.Edit2Change(Sender: TObject); begin editChange(1); end;
PROCEDURE TmodifyForm.Edit3Change(Sender: TObject); begin editChange(2); end;
PROCEDURE TmodifyForm.Edit4Change(Sender: TObject); begin editChange(3); end;

PROCEDURE TmodifyForm.myEditingDone(Sender: TObject);
  begin
    if not(step.isValid) then exit;
    dummyWorkflow.image.copyFromPixMap(previewInput);
    step.execute(@dummyWorkflow);
    dummyWorkflow.image.copyToImage(Image1);
  end;

PROCEDURE TmodifyForm.FormCreate(Sender: TObject);
  begin
    dummyWorkflow.create;
    step.create('normalize');
  end;

PROCEDURE TmodifyForm.FormDestroy(Sender: TObject);
  begin
    step.destroy;
    dummyWorkflow.destroy;
  end;

FUNCTION TmodifyForm.box(CONST index: byte): TGroupBox;
  begin
    case index of
      0: result:=GroupBox1;
      1: result:=GroupBox2;
      2: result:=GroupBox3;
      3: result:=GroupBox4;
    end;
  end;

FUNCTION TmodifyForm.edit(CONST index: byte): TEdit;
  begin
    case index of
      0: result:=Edit1;
      1: result:=Edit2;
      2: result:=Edit3;
      3: result:=Edit4;
    end;
  end;

FUNCTION TmodifyForm.trackbar(CONST index: byte): TTrackBar;
  begin
    case index of
      0: result:=TrackBar1;
      1: result:=TrackBar2;
      2: result:=TrackBar3;
      3: result:=TrackBar4;
    end;
  end;

PROCEDURE TmodifyForm.updateEdit(CONST index: byte);
  VAR i:byte;
  begin
    case index of
      0..3: if edit(index).enabled then edit(index).text:=meta^.getSimpleParameterDescription^.getSubParameter(index,step.operation^.getSimpleParameterValue^).toString;
      255-3..255: for i:=0 to 3 do if 255-i<>index then updateEdit(i);
      else        for i:=0 to 3 do                      updateEdit(i);
    end;
  end;

FUNCTION TmodifyForm.trackbarRange(CONST index: byte): T_doublePair;
  begin
    result[0]:=0;
    result[1]:=1;
    //TODO: Reimplement me
    //case step.getImageManipulationType of
    //  imt_fit,imt_resize,imt_fill    : begin result[0]:=0; result[1]:=8000; end;
    //  imt_multiplyHSV,imt_multiplyRGB: begin result[0]:=-0.5; result[1]:=2; end;
    //  imt_addHSV     ,imt_addRGB     : begin result[0]:=-0.5; result[1]:=0.5; end;
    //  imt_gammaRGB                   : begin result[0]:=0.5;  result[1]:=2; end;
    //  else begin
    //    if stepParamDescription[step.getImageManipulationType]^.children[index].description^.typ=pt_integer then begin
    //      result[0]:=stepParamDescription[step.getImageManipulationType]^.children[index].description^.minValue;
    //      result[1]:=stepParamDescription[step.getImageManipulationType]^.children[index].description^.maxValue;
    //    end else begin
    //      result[0]:=0;
    //      result[1]:=1;
    //    end;
    //  end;
    //end;
  end;

FUNCTION TmodifyForm.editValue(CONST index: byte): double;
  begin
    result:=strToFloatDef(edit(index).text,Nan);
    if isNan(result) then result:=getStepValue(index);
  end;

FUNCTION TmodifyForm.getStepValue(CONST index: byte): double;
  begin
    result:=step.operation^.getSimpleParameterValue^.getNumericParameter(index);
  end;

PROCEDURE TmodifyForm.setStepValue(CONST index: byte; CONST value: double);
  VAR i:byte;
  begin
    if index in [255-3..255] then begin
      for i:=0 to 3 do if 255-i<>index then setStepValue(i,value);
      exit;
    end;
    step.operation^.getSimpleParameterValue^.setNumericParameter(index,value);
    step.refreshSpecString;
  end;

PROCEDURE TmodifyForm.updateTrackbar(CONST index: byte);
  VAR val:double;
      range:T_doublePair;
      i:byte;
  begin
    case index of
      0..3: if trackbar(index).enabled then begin
        range:=trackbarRange(index);
        val:=getStepValue(index);
        if      val<range[0] then trackbar(index).position:=trackbar(index).min
        else if val>range[1] then trackbar(index).position:=trackbar(index).max
        else trackbar(index).position:=trackbar(index).min+round((trackbar(index).max-trackbar(index).min)/(range[1]-range[0])*(val-range[0]));
      end;
      255-3..255: for i:=0 to 3 do if 255-i<>index then updateTrackbar(i);
    else          for i:=0 to 3 do                      updateTrackbar(i);
    end;
  end;

FUNCTION TmodifyForm.trackbarValue(CONST index: byte): double;
  VAR range:T_doublePair;
  begin
    range:=trackbarRange(index);
    result:=range[0]+(range[1]-range[0])/(trackbar(index).max-trackbar(index).min)*(trackbar(index).position-trackbar(index).min);
  end;

FUNCTION TmodifyForm.showModalFor(CONST title:string; CONST manipulationMeta:P_simpleImageOperationMeta; CONST stepIndex: longint; CONST fix: boolean; CONST params: P_parameterValue): boolean;
  CONST enableAllEqualFor:set of T_imageManipulationType=[imt_addRGB,imt_multiplyRGB,imt_addHSV,imt_multiplyHSV,imt_gammaRGB,imt_lagrangeDiff];
  VAR i:longint;
      doEnable:boolean;
  begin
    meta:=manipulationMeta;
    caption:=title;
    step.destroy;
    if fix then step.create(imt,params^)
           else step.create(imt,stepParamDescription[imt]^.getDefaultParameterValue);

    if stepIndex<=0 then previewInput.create(inputImage^)
    else begin
      previewInput.create(1,1);
      workflow.renderIntermediate(stepIndex-1,previewInput);
    end;
    previewInput.resize(Image1.width,Image1.height,res_fit);
    previewOutput.create(previewInput);
    previewOutput.copyToImage(Image1);

    for i:=0 to 3 do begin
      doEnable:=length(stepParamDescription[imt]^.children)>i;
      box(i).enabled:=doEnable;
      if doEnable then box(i).caption:=stepParamDescription[imt]^.children[i].description^.name
                  else box(i).caption:='';
      edit(i).enabled:=doEnable;
      trackbar(i).enabled:=doEnable;
    end;
    for i:=0 to 3 do begin
      doEnable:=length(stepParamDescription[imt]^.children)>i;
      edit(i).text:='';
      if doEnable then begin
        if stepParamDescription[imt]^.children[i].description^.typ=pt_integer then begin
          trackbar(i).min:=round(stepParamDescription[imt]^.children[i].description^.minValue);
          trackbar(i).max:=round(stepParamDescription[imt]^.children[i].description^.maxValue);
        end else begin
          trackbar(i).min:=0;
          trackbar(i).max:=1000;
        end;
      end;
    end;
    updateEdit(7);
    updateTrackbar(7);
    cbAllEqual.checked:=false;
    cbAllEqual.enabled:=imt in enableAllEqualFor;
    if ShowModal=mrOk then begin
      while workflow.stepCount>=stepIndex+1 do workflow.remStep(workflow.stepCount-1);
      result:=workflow.addStep(step.toString);
    end else result:=false;
  end;

end.

