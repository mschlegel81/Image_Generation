UNIT jobberUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, Grids, workflows, myParams, mypics,myStringUtil,math,imageContexts,pixMaps;

TYPE

  { TjobberForm }

  TjobberForm = class(TForm)
    inputFileNameEdit: TFileNameEdit;
    GroupBox: TGroupBox;
    LogMemo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    storeTodoButton: TButton;
    startButton: TButton;
    CancelButton: TButton;
    resolutionEdit: TEdit;
    sizeLimitEdit: TEdit;
    fileNameEdit: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    timer: TTimer;
    autoJobbingToggleBox: TToggleBox;
    PROCEDURE cancelButtonClick(Sender: TObject);
    PROCEDURE fileNameEditEditingDone(Sender: TObject);
    PROCEDURE FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE inputFileNameEditEditingDone(Sender: TObject);
    PROCEDURE resolutionEditEditingDone(Sender: TObject);
    PROCEDURE sizeLimitEditEditingDone(Sender: TObject);
    PROCEDURE startButtonClick(Sender: TObject);
    PROCEDURE storeTodoButtonClick(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    editorWorkflow:P_imageGenerationContext;
    jobberWorkflow:T_imageGenerationContext;
    resolution:T_imageDimensions;
    sizeLimit:longint;
    filenameManuallyGiven,jobStarted:boolean;
    { private declarations }
  public
    { public declarations }
    PROCEDURE init(CONST wf:P_imageGenerationContext);
    PROCEDURE plausibilizeInput;
  end;

PROCEDURE showJobberForm(CONST wf:P_imageGenerationContext);
IMPLEMENTATION
//TODO: Initialize on demand
USES generationBasics;
VAR
  jobberForm: TjobberForm=nil;

PROCEDURE showJobberForm(CONST wf: P_imageGenerationContext);
  begin
    if jobberForm=nil then jobberForm:=TjobberForm.create(nil);
    jobberForm.init(wf);
    jobberForm.ShowModal;
  end;

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

PROCEDURE TjobberForm.fileNameEditEditingDone(Sender: TObject);
  begin
    sizeLimitEdit.enabled:=uppercase(extractFileExt(fileNameEdit.text))=JPG_EXT;
    filenameManuallyGiven:=fileNameEdit.text<>editorWorkflow^.proposedImageFileName(resolutionEdit.text);
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    jobberWorkflow.ensureStop;
    timer.enabled:=false;
  end;

PROCEDURE TjobberForm.cancelButtonClick(Sender: TObject);
  begin
    jobberWorkflow.ensureStop;
    ModalResult:=mrCancel;
  end;

PROCEDURE TjobberForm.resolutionEditEditingDone(Sender: TObject);
  begin
    if not(filenameManuallyGiven) then fileNameEdit.text:=editorWorkflow^.proposedImageFileName(resolutionEdit.text);
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
    jobberWorkflow.parseWorkflow(editorWorkflow^.workflowText);
    if editorWorkflow^.workflowType=wft_manipulative
    then jobberWorkflow.config.setInitialImage(inputFileNameEdit.fileName)
    else jobberWorkflow.config.initialResolution:=resolution;
    jobberWorkflow.appendSaveStep(sizeLimit,fileNameEdit.fileName);
    jobberWorkflow.executeWorkflowInBackground(false);
    startButton.enabled:=false;
    storeTodoButton.enabled:=false;
    jobStarted:=true;
  end;

PROCEDURE TjobberForm.storeTodoButtonClick(Sender: TObject);
  begin
    jobberWorkflow.parseWorkflow(editorWorkflow^.workflowText);
    if editorWorkflow^.workflowType=wft_manipulative
    then jobberWorkflow.config.setInitialImage(inputFileNameEdit.fileName)
    else jobberWorkflow.config.initialResolution:=resolution;
    jobberWorkflow.saveAsTodo(fileNameEdit.fileName,sizeLimit);
    startButton.enabled:=false;
    storeTodoButton.enabled:=false;
    autoJobbingToggleBox.enabled:=true;
    jobStarted:=true;
  end;

PROCEDURE TjobberForm.TimerTimer(Sender: TObject);
  FUNCTION findAndExecuteToDo:boolean;
    begin
      //FUNCTION T_imageManipulationWorkflow.findAndExecuteToDo: boolean;
      //  CONST maxDepth=5;
      //  VAR todoName:ansistring;
      //      depth:longint=0;
      //      root:ansistring='.';
      //  begin
      //    repeat
      //      todoName:=findDeeply(expandFileName(root),'*.todo');
      //      root:=root+'/..';
      //      inc(depth);
      //    until (depth>=maxDepth) or (todoName<>'');
      //    if todoName='' then exit(false);
      //    loadFromFile(todoName);
      //    progressQueue^.registerOnEndCallback(@findAndExecuteToDo_DONE);
      //    execute(false,false,false,1,1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH);
      //    result:=true;
      //  end;

      //TODO: Implement me
      //TODO: remember which todo was executed and delete this file when done
    end;

  PROCEDURE pollMessage;
    VAR m:P_structuredMessage;
    begin
      m:=jobberWorkflow.messageQueue.get;
      while m<>nil do begin
        LogMemo.lines.add(m^.toString);
        dispose(m,destroy);
        m:=jobberWorkflow.messageQueue.get;
      end;
    end;

  begin
    pollMessage;
    if not(jobberWorkflow.executing) then begin
      if autoJobbingToggleBox.checked then begin
        if findAndExecuteToDo then begin
          resolutionEdit.enabled:=false;
          startButton.enabled:=false;
          fileNameEdit.enabled:=false;
          sizeLimitEdit.enabled:=false;
        end else begin
          autoJobbingToggleBox.enabled:=false;
          autoJobbingToggleBox.checked:=false;
        end;
      end;
    end;
  end;

PROCEDURE TjobberForm.init(CONST wf:P_imageGenerationContext);
  begin
    editorWorkflow:=wf;
    autoJobbingToggleBox.checked:=false;
    inputFileNameEdit.fileName:=editorWorkflow^.config.initialImageName;
    inputFileNameEdit.enabled:=(editorWorkflow^.workflowType=wft_manipulative);
    resolutionEdit   .enabled:=(editorWorkflow^.workflowType=wft_generative);
    inputFileNameEdit.visible:=(editorWorkflow^.workflowType=wft_manipulative);
    resolutionEdit   .visible:=(editorWorkflow^.workflowType=wft_generative);
    Label1.visible:=(editorWorkflow^.workflowType in [wft_manipulative,wft_generative]);
    if editorWorkflow^.workflowType=wft_manipulative
    then Label1.caption:='Input:'
    else Label1.caption:='Resolution:';
    startButton.enabled:=true;
    fileNameEdit.enabled:=true;
    sizeLimitEdit.enabled:=true;
    timer.enabled:=true;
    fileNameEdit.text:=editorWorkflow^.proposedImageFileName(resolutionEdit.text);
    filenameManuallyGiven:=false;
    jobStarted:=false;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.plausibilizeInput;
  begin
    startButton.enabled:=(not(jobStarted)) and
                         (not(resolutionEdit.enabled) or canParseResolution(resolutionEdit.text,resolution)) and
                         (not(inputFileNameEdit.enabled) or (fileExists(inputFileNameEdit.fileName))  or (fileExists(inputFileNameEdit.fileName))) and
                         (not(sizeLimitEdit.enabled) or canParseSizeLimit(sizeLimitEdit.text,sizeLimit) or (sizeLimitEdit.text=''));
    if (trim(sizeLimitEdit.text)='') or not(sizeLimitEdit.enabled)  then sizeLimit:=-1;
    storeTodoButton.enabled:=startButton.enabled;
  end;

FINALIZATION
  if jobberForm<>nil then FreeAndNil(jobberForm);

end.

