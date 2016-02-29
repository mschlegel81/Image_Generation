UNIT jobberUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, Grids, workflows, myParams, mypics,myTools,myStringUtil;

TYPE

  { TjobberForm }

  TjobberForm = class(TForm)
    GroupBox: TGroupBox;
    planRadioButton: TRadioButton;
    logRadioButton: TRadioButton;
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
    PROCEDURE logRadioButtonChange(Sender: TObject);
    PROCEDURE resolutionEditEditingDone(Sender: TObject);
    PROCEDURE sizeLimitEditEditingDone(Sender: TObject);
    PROCEDURE startButtonClick(Sender: TObject);
    PROCEDURE storeTodoButtonClick(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    oldXRes,oldYRes:longint;
    xRes,yRes,sizeLimit:longint;
    filenameManuallyGiven,jobStarted:boolean;
    { private declarations }
  public
    { public declarations }
    PROCEDURE init;
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
    init;
  end;

PROCEDURE TjobberForm.logRadioButtonChange(Sender: TObject);
  begin
    updateGrid;
  end;

PROCEDURE TjobberForm.fileNameEditEditingDone(Sender: TObject);
  begin
    sizeLimitEdit.Enabled:=uppercase(extractFileExt(fileNameEdit.text))='.JPG';
    filenameManuallyGiven:=true;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    timer.Enabled:=false;
    workflowImage.resize(oldXRes,oldYRes,res_dataResize);
  end;

PROCEDURE TjobberForm.cancelButtonClick(Sender: TObject);
  begin
    workflows.progressQueue.ensureStop;
    ModalResult:=mrCancel;
  end;

PROCEDURE TjobberForm.resolutionEditEditingDone(Sender: TObject);
  begin
    if not(filenameManuallyGiven) then fileNameEdit.text:=workflow.proposedImageFileName(resolutionEdit.text);
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.sizeLimitEditEditingDone(Sender: TObject);
  begin
    if not(canParseSizeLimit(sizeLimitEdit.text,sizeLimit)) then sizeLimitEdit.text:='';
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.startButtonClick(Sender: TObject);
  begin
    workflow.executeForTarget(xRes,yRes,sizeLimit,fileNameEdit.fileName);
    startButton.Enabled:=false;
    storeTodoButton.Enabled:=false;
    jobStarted:=true;
  end;

PROCEDURE TjobberForm.storeTodoButtonClick(Sender: TObject);
  begin
    workflow.storeToDo(xRes,yRes,sizeLimit,fileNameEdit.fileName);
    startButton.Enabled:=false;
    storeTodoButton.Enabled:=false;
  end;

PROCEDURE TjobberForm.TimerTimer(Sender: TObject);
  begin
    StatusBar1.SimpleText:=progressQueue.getProgressString;
    if not(progressQueue.calculating) then begin
      if autoJobbingToggleBox.Checked then begin
        if not(workflow.findAndExecuteToDo) then begin
          autoJobbingToggleBox.Enabled:=false;
          autoJobbingToggleBox.Checked:=false;
        end;
      end;
    end else if logRadioButton.Checked then updateGrid;
  end;

PROCEDURE TjobberForm.init;
  begin
    timer.Enabled:=true;
    planRadioButton.Checked:=true;
    updateGrid;
    oldXRes:=workflowImage.width;
    oldYRes:=workflowImage.height;
    workflows.progressQueue.ensureStop;
    fileNameEdit.text:=workflow.proposedImageFileName(resolutionEdit.text);
    filenameManuallyGiven:=false;
    jobStarted:=false;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.updateGrid;
  VAR i:longint;
      log:T_progressLog;
  begin
    if planRadioButton.Checked then begin
      StringGrid.RowCount:=1+workflow.stepCount;
      for i:=0 to workflow.stepCount-1 do begin
        StringGrid.Cells[0,i+1]:=intToStr(i+1);
        StringGrid.Cells[1,i+1]:=workflow.stepText(i);
        StringGrid.Cells[2,i+1]:='';
      end;
    end else begin
      log:=progressQueue.log;
      StringGrid.RowCount:=1+length(log);
      for i:=0 to length(log)-1 do begin
        StringGrid.Cells[0,i+1]:=intToStr(i+1);
        StringGrid.Cells[1,i+1]:=log[i].message;
        if log[i].timeUsed>=0
        then StringGrid.Cells[2,i+1]:=myTimeToStr(log[i].timeUsed)
        else StringGrid.Cells[2,i+1]:='...';
      end;
    end;
  end;

PROCEDURE TjobberForm.plausibilizeInput;
  begin
    startButton.Enabled:=canParseResolution(resolutionEdit.text,xRes,yRes) and
                        (not(sizeLimitEdit.Enabled) or canParseSizeLimit(sizeLimitEdit.text,sizeLimit) or (sizeLimitEdit.text=''));
    if trim(sizeLimitEdit.text)='' then sizeLimit:=-1;
    storeTodoButton.Enabled:=startButton.Enabled;
  end;

end.

