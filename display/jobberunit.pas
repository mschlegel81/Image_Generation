UNIT jobberUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, workflows, myStringUtil, myParams;

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
    Timer: TTimer;
    procedure cancelButtonClick(Sender: TObject);
    PROCEDURE fileNameEditEditingDone(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE resolutionEditEditingDone(Sender: TObject);
    PROCEDURE sizeLimitEditEditingDone(Sender: TObject);
    PROCEDURE startButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    xRes,yRes,sizeLimit:longint;
    filenameManuallyGiven:boolean;
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

procedure TjobberForm.FormShow(Sender: TObject);
  begin
    init;
  end;

procedure TjobberForm.fileNameEditEditingDone(Sender: TObject);
  begin
    sizeLimitEdit.Enabled:=uppercase(extractFileExt(fileNameEdit.text))='.JPG';
    filenameManuallyGiven:=true;
    plausibilizeInput;
  end;

procedure TjobberForm.cancelButtonClick(Sender: TObject);
  begin
    workflows.progressQueue.ensureStop;
    ModalResult:=mrCancel;
  end;

procedure TjobberForm.resolutionEditEditingDone(Sender: TObject);
  begin
    if not(filenameManuallyGiven) then fileNameEdit.text:=workflow.proposedImageFileName(resolutionEdit.text);
    plausibilizeInput;
  end;

procedure TjobberForm.sizeLimitEditEditingDone(Sender: TObject);
  begin
    if not(canParseSizeLimit(sizeLimitEdit.text,sizeLimit)) then sizeLimitEdit.text:='';
    plausibilizeInput;
  end;

procedure TjobberForm.startButtonClick(Sender: TObject);
  begin
    workflow.executeForTarget(xRes,yRes,sizeLimit,fileNameEdit.FileName);
    startButton.Enabled:=false;
  end;

procedure TjobberForm.TimerTimer(Sender: TObject);
  begin
    StatusBar1.SimpleText:=progressQueue.getProgressString;
  end;

procedure TjobberForm.init;
  begin
    workflows.progressQueue.ensureStop;
    fileNameEdit.text:=workflow.proposedImageFileName(resolutionEdit.text);
    filenameManuallyGiven:=false;
    plausibilizeInput;
  end;

procedure TjobberForm.plausibilizeInput;
  begin
    if sizeLimitEdit.Text='' then sizeLimit:=-1;
    startButton.Enabled:=canParseResolution(resolutionEdit.text,xRes,yRes) and
                        (not(sizeLimitEdit.Enabled) or canParseSizeLimit(sizeLimitEdit.text,sizeLimit) or (sizeLimitEdit.Text=''));
  end;

end.

