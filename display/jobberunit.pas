UNIT jobberUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, workflows, myParams, mypics;

TYPE

  { TjobberForm }

  TjobberForm = class(TForm)
    startButton: TButton;
    cancelButton: TButton;
    resolutionEdit: TEdit;
    sizeLimitEdit: TEdit;
    fileNameEdit: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StatusBar1: TStatusBar;
    timer: TTimer;
    PROCEDURE cancelButtonClick(Sender: TObject);
    PROCEDURE fileNameEditEditingDone(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE resolutionEditEditingDone(Sender: TObject);
    PROCEDURE sizeLimitEditEditingDone(Sender: TObject);
    PROCEDURE startButtonClick(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    oldXRes,oldYRes:longint;
    xRes,yRes,sizeLimit:longint;
    filenameManuallyGiven,jobStarted:boolean;
    { private declarations }
  public
    { public declarations }
    PROCEDURE init;
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

PROCEDURE TjobberForm.fileNameEditEditingDone(Sender: TObject);
  begin
    sizeLimitEdit.Enabled:=uppercase(extractFileExt(fileNameEdit.text))='.JPG';
    filenameManuallyGiven:=true;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
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
    jobStarted:=true;
  end;

PROCEDURE TjobberForm.TimerTimer(Sender: TObject);
  begin
    StatusBar1.SimpleText:=progressQueue.getProgressString;
    if jobStarted and not(progressQueue.calculating) then ModalResult:=mrOK;
  end;

PROCEDURE TjobberForm.init;
  begin
    oldXRes:=workflowImage.width;
    oldYRes:=workflowImage.height;
    workflows.progressQueue.ensureStop;
    fileNameEdit.text:=workflow.proposedImageFileName(resolutionEdit.text);
    filenameManuallyGiven:=false;
    jobStarted:=false;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.plausibilizeInput;
  begin
    if sizeLimitEdit.text='' then sizeLimit:=-1;
    startButton.Enabled:=canParseResolution(resolutionEdit.text,xRes,yRes) and
                        (not(sizeLimitEdit.Enabled) or canParseSizeLimit(sizeLimitEdit.text,sizeLimit) or (sizeLimitEdit.text=''));
  end;

end.

