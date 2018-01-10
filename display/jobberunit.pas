UNIT jobberUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, Grids, workflows, myParams, mypics,myTools,myStringUtil,math;

TYPE

  { TjobberForm }

  TjobberForm = class(TForm)
    inputFileNameEdit: TFileNameEdit;
    GroupBox: TGroupBox;
    storeTodoButton: TButton;
    startButton: TButton;
    CancelButton: TButton;
    resolutionEdit: TEdit;
    sizeLimitEdit: TEdit;
    fileNameEdit: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StatusBar1: TStatusBar;
    StringGrid: TStringGrid;
    timer: TTimer;
    autoJobbingToggleBox: TToggleBox;
    PROCEDURE cancelButtonClick(Sender: TObject);
    PROCEDURE fileNameEditEditingDone(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE inputFileNameEditEditingDone(Sender: TObject);
    PROCEDURE logRadioButtonChange(Sender: TObject);
    PROCEDURE resolutionEditEditingDone(Sender: TObject);
    PROCEDURE sizeLimitEditEditingDone(Sender: TObject);
    PROCEDURE startButtonClick(Sender: TObject);
    PROCEDURE storeTodoButtonClick(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    oldXRes,oldYRes:longint;
    xRes,yRes,sizeLimit:longint;
    filenameManuallyGiven,jobStarted,
    displayedAfterFinish:boolean;
    { private declarations }
  public
    { public declarations }
    PROCEDURE init(CONST currentInput:ansistring);
    PROCEDURE updateGrid;
    PROCEDURE plausibilizeInput;
  end;

VAR
  jobberForm: TjobberForm;

IMPLEMENTATION

{$R *.lfm}

{ TjobberForm }

PROCEDURE TjobberForm.FormShow(Sender: TObject);
  begin
    //init('');
  end;

PROCEDURE TjobberForm.inputFileNameEditEditingDone(Sender: TObject);
  begin
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.logRadioButtonChange(Sender: TObject);
  begin
    updateGrid;
  end;

PROCEDURE TjobberForm.fileNameEditEditingDone(Sender: TObject);
  begin
    sizeLimitEdit.enabled:=uppercase(extractFileExt(fileNameEdit.text))='.JPG';
    filenameManuallyGiven:=fileNameEdit.text<>workflow.proposedImageFileName(resolutionEdit.text);
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    workflow.progressQueue.cancelCalculation(true);
    timer.enabled:=false;
    workflow.workflowImage.resize(oldXRes,oldYRes,res_dataResize);
  end;

PROCEDURE TjobberForm.cancelButtonClick(Sender: TObject);
  begin
    workflow.progressQueue.ensureStop;
    ModalResult:=mrCancel;
  end;

PROCEDURE TjobberForm.resolutionEditEditingDone(Sender: TObject);
  begin
    if not(filenameManuallyGiven) then fileNameEdit.text:=workflow.proposedImageFileName(resolutionEdit.text);
    jobStarted:=false;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.sizeLimitEditEditingDone(Sender: TObject);
  begin
    if not(canParseSizeLimit(sizeLimitEdit.text,sizeLimit)) then sizeLimitEdit.text:='';
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.startButtonClick(Sender: TObject);
  begin
    if workflow.workflowType=wft_manipulative
    then workflow.executeForTarget(inputFileNameEdit.fileName,sizeLimit,fileNameEdit.fileName)
    else workflow.executeForTarget(xRes,yRes                            ,sizeLimit,fileNameEdit.fileName);
    startButton.enabled:=false;
    storeTodoButton.enabled:=false;
    jobStarted:=true;
    displayedAfterFinish:=false;
  end;

PROCEDURE TjobberForm.storeTodoButtonClick(Sender: TObject);
  begin
    if workflow.workflowType=wft_manipulative
    then workflow.storeToDo(inputFileNameEdit.fileName,sizeLimit,fileNameEdit.fileName)
    else workflow.storeToDo(xRes,yRes                            ,sizeLimit,fileNameEdit.fileName);
    startButton.enabled:=false;
    storeTodoButton.enabled:=false;
    autoJobbingToggleBox.enabled:=true;
    jobStarted:=true;
    displayedAfterFinish:=true;
  end;

PROCEDURE TjobberForm.TimerTimer(Sender: TObject);
  begin
    StatusBar1.SimpleText:=workflow.progressQueue.getProgressString;
    if not(workflow.progressQueue.calculating) then begin
      if not(displayedAfterFinish) then begin
        updateGrid;
        displayedAfterFinish:=true;
      end;
      if autoJobbingToggleBox.Checked then begin
        if workflow.findAndExecuteToDo then begin
          resolutionEdit.enabled:=false;
          startButton.enabled:=false;
          fileNameEdit.enabled:=false;
          sizeLimitEdit.enabled:=false;
          displayedAfterFinish:=false;
          updateGrid;
        end else begin
          autoJobbingToggleBox.enabled:=false;
          autoJobbingToggleBox.Checked:=false;
        end;
      end;
    end else updateGrid;
  end;

PROCEDURE TjobberForm.init(CONST currentInput:ansistring);
  begin
    autoJobbingToggleBox.Checked:=false;
    inputFileNameEdit.fileName:=currentInput;
    inputFileNameEdit.enabled:=(workflow.workflowType=wft_manipulative);
    resolutionEdit   .enabled:=(workflow.workflowType=wft_generative);
    inputFileNameEdit.visible:=(workflow.workflowType=wft_manipulative);
    resolutionEdit   .visible:=(workflow.workflowType=wft_generative);
    Label1.visible:=(workflow.workflowType in [wft_manipulative,wft_generative]);
    if workflow.workflowType=wft_manipulative
    then Label1.caption:='Input:'
    else Label1.caption:='Resolution:';
    startButton.enabled:=true;
    fileNameEdit.enabled:=true;
    sizeLimitEdit.enabled:=true;
    timer.enabled:=true;
    updateGrid;
    oldXRes:=workflow.workflowImage.dimensions.width;
    oldYRes:=workflow.workflowImage.dimensions.height;
    workflow.progressQueue.ensureStop;
    fileNameEdit.text:=workflow.proposedImageFileName(resolutionEdit.text);
    filenameManuallyGiven:=false;
    jobStarted:=false;
    displayedAfterFinish:=true;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.updateGrid;
  VAR i:longint;
      log:T_progressLog;
  begin
    log:=workflow.progressQueue.log;
    StringGrid.RowCount:=1+max(workflow.stepCount,length(log));
    for i:=0 to max(workflow.stepCount,length(log))-1 do begin
      StringGrid.Cells[0,i+1]:=intToStr(i+1);

      if i<workflow.stepCount
      then StringGrid.Cells[1,i+1]:=workflow.step[i].toString()
      else StringGrid.Cells[1,i+1]:=log[i].message;

      if i<length(log) then begin
        if log[i].timeUsed>=0
        then StringGrid.Cells[2,i+1]:=myTimeToStr(log[i].timeUsed)
        else StringGrid.Cells[2,i+1]:='in progress';
      end else StringGrid.Cells[2,i+1]:='';
    end;
  end;

PROCEDURE TjobberForm.plausibilizeInput;
  begin
    startButton.enabled:=(not(jobStarted)) and
                         (not(resolutionEdit.enabled) or canParseResolution(resolutionEdit.text,xRes,yRes)) and
                         (not(inputFileNameEdit.enabled) or (fileExists(inputFileNameEdit.fileName))  or (fileExists(inputFileNameEdit.fileName))) and
                         (not(sizeLimitEdit.enabled) or canParseSizeLimit(sizeLimitEdit.text,sizeLimit) or (sizeLimitEdit.text=''));
    if (trim(sizeLimitEdit.text)='') or not(sizeLimitEdit.enabled)  then sizeLimit:=-1;
    storeTodoButton.enabled:=startButton.enabled;
  end;

end.

