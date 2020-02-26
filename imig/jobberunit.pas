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
    todosSearched:boolean;
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

PROCEDURE TjobberForm.FormCreate(Sender: TObject);
  begin
    jobberMessages.create;
    jobberWorkflow.createSimpleWorkflow(@jobberMessages);
    pendingTodos:=nil;
    todosSearched:=false;
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
    jobberWorkflow.parseWorkflow(editorWorkflow^.workflowText);
    if editorWorkflow^.workflowType=wft_manipulative
    then jobberWorkflow.config.setInitialImage(inputFileNameEdit.fileName)
    else jobberWorkflow.config.initialResolution:=resolution;
    jobberWorkflow.appendSaveStep(fileNameEdit.fileName,sizeLimit);
    jobberWorkflow.executeWorkflowInBackground(false);
    startButton.enabled:=false;
    storeTodoButton.enabled:=false;
    jobStarted:=true;
  end;

PROCEDURE TjobberForm.storeTodoButtonClick(Sender: TObject);
  begin
    jobberWorkflow.config.setDefaults;
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
  PROCEDURE lookForTodos;
    CONST maxDepth=5;
    VAR depth:longint=0;
        root:ansistring='.';
    begin
      root:=ExtractFileDir(paramStr(0));
      repeat
        pendingTodos:=FindAllFiles(root,'*.todo');
        if (pendingTodos.count=0) then FreeAndNil(pendingTodos);
        root:=root+'/..';
        inc(depth);
      until (depth>=maxDepth) or (pendingTodos<>nil);
      todosSearched:=true;
    end;

  FUNCTION getNextTodo:string;
    begin
      result:='';
      while result='' do begin
        if (pendingTodos=nil) and not(todosSearched) then lookForTodos;
        if  pendingTodos=nil then exit('');
        result:=pendingTodos[0];
        if not(fileExists(result)) then result:='';
        pendingTodos.delete(0);
        if pendingTodos.count=0 then FreeAndNil(pendingTodos);
      end;
    end;

  FUNCTION findAndExecuteToDo:boolean;
    VAR nextTodo:string;
    begin
      result:=false;
      jobberMessages.Post('----------------------------------',false);
      repeat
        nextTodo:=getNextTodo;
        if nextTodo='' then exit(false);
        jobberWorkflow.readFromFile(nextTodo);
        if jobberWorkflow.isValid
        then result:=true
        else begin
          jobberMessages.Post('Invalid workflow encountered "'+nextTodo+'" - skipping');
          result:=false;
        end;
      until result;
      jobberWorkflow.executeAsTodo;
      result:=true;
    end;

  PROCEDURE pollMessage;
    VAR m:P_structuredMessage;
        needScroll:boolean=false;
    begin
      LogMemo.lines.BeginUpdate;
      m:=jobberMessages.get;
      while m<>nil do begin
        LogMemo.lines.add(m^.toString);
        dispose(m,destroy);
        m:=jobberMessages.get;
        needScroll:=true;
      end;
      while LogMemo.lines.count>100 do LogMemo.lines.delete(0);
      LogMemo.lines.EndUpdate;
      if needScroll then begin
        LogMemo.CaretPos:=point(1,100);
        LogMemo.SelStart:=length(LogMemo.text);
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
          jobberMessages.Post('No (more) TODOs can be found',false);
        end;
      end;
    end;
    pollMessage;
  end;

PROCEDURE TjobberForm.init(CONST wf:P_editorWorkflow);
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

