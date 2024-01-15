UNIT jobberUnit;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, ComCtrls, ExtCtrls, workflows, myParams, mypics,imageContexts,pixMaps,generationBasics;

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
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormShow(Sender: TObject);
    PROCEDURE inputFileNameEditEditingDone(Sender: TObject);
    PROCEDURE resolutionEditEditingDone(Sender: TObject);
    PROCEDURE sizeLimitEditEditingDone(Sender: TObject);
    PROCEDURE startButtonClick(Sender: TObject);
    PROCEDURE storeTodoButtonClick(Sender: TObject);
    PROCEDURE TimerTimer(Sender: TObject);
  private
    editorWorkflow:P_editorWorkflow;
    jobberMessages:T_structuredMessageQueue;
    jobberWorkflow:T_simpleWorkflow;
    resolution:T_imageDimensions;
    sizeLimit:longint;
    filenameManuallyGiven,jobStarted:boolean;
    pendingTodos:TStringList;
    { private declarations }
  public
    { public declarations }
    PROCEDURE init(CONST wf:P_editorWorkflow);
    PROCEDURE plausibilizeInput;
  end;

PROCEDURE showJobberForm(CONST wf:P_editorWorkflow);
IMPLEMENTATION
USES imageManipulation,im_geometry,mySys,messages;
VAR
  jobberForm: TjobberForm=nil;

PROCEDURE showJobberForm(CONST wf: P_editorWorkflow);
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
    jobStarted:=false;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.fileNameEditEditingDone(Sender: TObject);
  begin
    sizeLimitEdit.enabled:=uppercase(extractFileExt(fileNameEdit.text))=JPG_EXT;
    filenameManuallyGiven:=fileNameEdit.text<>editorWorkflow^.proposedImageFileName(resolutionEdit.text);
    jobStarted:=false;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.FormClose(Sender: TObject; VAR CloseAction: TCloseAction);
  begin
    //jobberWorkflow.ensureStop;
    timer.enabled:=false;
  end;

PROCEDURE TjobberForm.FormCreate(Sender: TObject);
  begin
    jobberMessages.create;
    jobberMessages.messageStringLengthLimit:=1000;
    jobberWorkflow.createSimpleWorkflow(@jobberMessages);
    pendingTodos:=FindAllFiles(ExtractFileDir(paramStr(0)),'*'+lowercase(C_todoExtension));
    jobberMessages.Post('Found '+intToStr(pendingTodos.count)+' '+C_todoExtension+'-files',false,-1,0);
  end;

PROCEDURE TjobberForm.FormDestroy(Sender: TObject);
  begin
    jobberWorkflow.destroy;
    jobberMessages.destroy;
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
    jobberWorkflow.config.setDefaults;
    jobberWorkflow.parseWorkflow(editorWorkflow^.workflowText,false);
    if editorWorkflow^.workflowType=wft_manipulative
    then jobberWorkflow.config.setInitialImage(inputFileNameEdit.fileName)
    else jobberWorkflow.config.initialResolution:=resolution;
    jobberWorkflow.parseWorkflow(jobberWorkflow.todoLines(fileNameEdit.fileName,sizeLimit),false);
    jobberWorkflow.executeWorkflowInBackground(false);
    startButton.enabled:=false;
    storeTodoButton.enabled:=false;
    jobStarted:=true;
  end;

PROCEDURE TjobberForm.storeTodoButtonClick(Sender: TObject);
  begin
    jobberWorkflow.config.setDefaults;
    jobberWorkflow.parseWorkflow(editorWorkflow^.workflowText,false);
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

  FUNCTION getNextTodo(CONST allowRescan:boolean):string;
    begin
      result:='';
      while result='' do begin
        if pendingTodos.count=0 then begin
          if allowRescan then begin
            FreeAndNil(pendingTodos);
            pendingTodos:=FindAllFiles(ExtractFileDir(paramStr(0)),'*'+lowercase(C_todoExtension));
            jobberMessages.Post('Found '+intToStr(pendingTodos.count)+' '+C_todoExtension+'-files',false,-1,0);
          end;
          if pendingTodos.count=0 then exit('');
        end;
        result:=pendingTodos[0];
        if not(fileExists(result)) then result:='';
        pendingTodos.delete(0);
      end;
    end;

  FUNCTION findAndExecuteToDo:boolean;
    VAR nextTodo:string;
        allowRescan:boolean=true;
    begin
      result:=false;
      jobberMessages.postSeparator;
      repeat
        nextTodo:=getNextTodo(allowRescan);
        if nextTodo='' then exit(false);
        jobberWorkflow.readFromFile(nextTodo,false);
        if jobberWorkflow.executeAsTodo
        then result:=true
        else begin
          jobberMessages.Post('Invalid workflow encountered "'+nextTodo+'" - skipping',true,-1,0);
          allowRescan:=false;
          result:=false;
        end;
      until result;
    end;

  PROCEDURE pollMessage;
    VAR s:string;
        needScroll:boolean=false;
    begin
      LogMemo.lines.BeginUpdate;
      for s in jobberMessages.getText do begin
        LogMemo.lines.add(s);
        needScroll:=true;
      end;
      while LogMemo.lines.count>100 do LogMemo.lines.delete(0);
      LogMemo.lines.EndUpdate;
      if needScroll then begin
        LogMemo.CaretPos:=point(1,100);
        LogMemo.selStart:=length(LogMemo.text);
        LogMemo.SelLength:=0;
        LogMemo.Perform(EM_SCROLLCARET,0,0);
      end;
    end;

  begin
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
          jobberMessages.Post('No (more) TODOs can be found',false,-1,0);
        end;
      end;
    end;
    pollMessage;
  end;

PROCEDURE TjobberForm.init(CONST wf:P_editorWorkflow);
  VAR workflowType:T_workflowType;
  begin
    editorWorkflow:=wf;
    workflowType:=editorWorkflow^.workflowType;
    autoJobbingToggleBox.checked:=false;
    autoJobbingToggleBox.enabled:=true;
    inputFileNameEdit.fileName:=editorWorkflow^.config.initialImageName;
    inputFileNameEdit.enabled:=(workflowType in [wft_manipulative,wft_manipulativeWithSave]);
    resolutionEdit   .enabled:=(workflowType in [wft_generative,wft_generativeWithSave]);
    inputFileNameEdit.visible:=(workflowType in [wft_manipulative,wft_manipulativeWithSave]);
    resolutionEdit   .visible:=(workflowType in [wft_generative,wft_generativeWithSave]);
    Label1.visible:=(workflowType in [wft_manipulative,wft_generative,wft_generativeWithSave,wft_manipulativeWithSave]);
    if workflowType in [wft_manipulative,wft_manipulativeWithSave]
    then Label1.caption:='Input:'
    else Label1.caption:='Resolution:';
    startButton.enabled:=true;
    fileNameEdit.enabled:=editorWorkflow^.workflowType in [wft_generative,wft_manipulative,wft_halfFix];
    sizeLimitEdit.enabled:=editorWorkflow^.workflowType in [wft_generative,wft_manipulative,wft_halfFix];
    timer.enabled:=true;
    fileNameEdit.text:=editorWorkflow^.proposedImageFileName(resolutionEdit.text);
    filenameManuallyGiven:=false;
    jobStarted:=false;
    plausibilizeInput;
  end;

PROCEDURE TjobberForm.plausibilizeInput;
  begin
    startButton.enabled:=(editorWorkflow^.workflowType<>wft_empty_or_unknown) and
                         (not(jobStarted)) and
                         (not(resolutionEdit.enabled) or canParseResolution(resolutionEdit.text,resolution)) and
                         (not(inputFileNameEdit.enabled) or (fileExists(inputFileNameEdit.fileName))  or (fileExists(inputFileNameEdit.fileName))) and
                         (not(sizeLimitEdit.enabled) or canParseSizeLimit(sizeLimitEdit.text,sizeLimit) or (sizeLimitEdit.text=''));
    if (trim(sizeLimitEdit.text)='') or not(sizeLimitEdit.enabled)  then sizeLimit:=-1;
    storeTodoButton.enabled:=startButton.enabled;
  end;

FINALIZATION
  if jobberForm<>nil then FreeAndNil(jobberForm);

end.

