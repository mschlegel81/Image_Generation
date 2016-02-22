UNIT jobberUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, workflows, myStringUtil;

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
    PROCEDURE fileNameEditEditingDone(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE resolutionEditEditingDone(Sender: TObject);
    PROCEDURE sizeLimitEditEditingDone(Sender: TObject);
    PROCEDURE startButtonClick(Sender: TObject);
  private
    xRes,yRes,sizeLimit:longint;
    { private declarations }
  public
    { public declarations }
  end;

VAR
  jobberForm: TjobberForm;

IMPLEMENTATION

{$R *.lfm}

{ TjobberForm }

PROCEDURE TjobberForm.FormShow(Sender: TObject);
  begin
    resolutionEdit.EditingDone;
    fileNameEdit.text:=workflow.proposedImageFileName(resolutionEdit.text);
  end;

PROCEDURE TjobberForm.fileNameEditEditingDone(Sender: TObject);
  begin
    sizeLimitEdit.Enabled:=uppercase(extractFileExt(fileNameEdit.text))='.JPG';
  end;

PROCEDURE TjobberForm.resolutionEditEditingDone(Sender: TObject);
  begin
    if not(canParseResolution(resolutionEdit.text,xRes,yRes)) then begin
      xRes:=Screen.width;
      yRes:=Screen.height;
      resolutionEdit.text:=intToStr(xRes)+'x'+intToStr(yRes);
    end else fileNameEdit.text:=workflow.proposedImageFileName(resolutionEdit.text);
  end;

PROCEDURE TjobberForm.sizeLimitEditEditingDone(Sender: TObject);
  begin
    if not(canParseSizeLimit(sizeLimitEdit.text,sizeLimit)) then sizeLimitEdit.text:='';
  end;

PROCEDURE TjobberForm.startButtonClick(Sender: TObject);
  begin
    workflow.execute(false,false,xRes,yRes);
  end;

end.

