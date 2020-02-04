UNIT workflows;
INTERFACE
USES myGenerics,
     pixMaps,
     myParams,mypics,sysutils,imageGeneration,ExtCtrls,mySys,FileUtil,Dialogs,
     generationBasics,
     imageContexts,workflowSteps;

TYPE
  P_simpleWorkflow=^T_simpleWorkflow;
  T_simpleWorkflow=object(T_abstractWorkflow)
    protected
      steps: array of P_workflowStep;
      PROCEDURE headlessWorkflowExecution; virtual;
      PROCEDURE afterStep(CONST stepIndex:longint; CONST elapsed:double); virtual;
      PROCEDURE beforeAll; virtual;
      PROCEDURE afterAll ;
      PROCEDURE configChanged; virtual;
    private
      FUNCTION getStep(index:longint):P_workflowStep;

    public
      config:T_imageWorkflowConfiguration;
      CONSTRUCTOR createSimpleWorkflow(CONST messageQueue_:P_structuredMessageQueue);
      DESTRUCTOR destroy; virtual;
      PROCEDURE clear;
      PROPERTY step[index:longint]: P_workflowStep read getStep;
      FUNCTION stepCount:longint;
      FUNCTION parseWorkflow(CONST data:T_arrayOfString):boolean;
      FUNCTION workflowText:T_arrayOfString;
      FUNCTION readFromFile(CONST fileName:string):boolean;
      PROCEDURE saveToFile(CONST fileName:string);
      PROCEDURE saveAsTodo(CONST savingToFile:string; CONST savingWithSizeLimit:longint);
      PROCEDURE appendSaveStep(CONST savingToFile:string; CONST savingWithSizeLimit:longint);
      PROCEDURE executeAsTodo;

      FUNCTION workflowType:T_workflowType;
      FUNCTION proposedImageFileName(CONST resString:ansistring):string;
      FUNCTION addStep(CONST specification:string):boolean;
      PROCEDURE addStep(CONST operation:P_imageOperation);

      FUNCTION isValid: boolean; virtual;
      FUNCTION limitedDimensionsForResizeStep(CONST tgtDim:T_imageDimensions):T_imageDimensions; virtual;
  end;

  P_editorWorkflow=^T_editorWorkflow;
  T_editorWorkflow=object(T_simpleWorkflow)
    protected
      PROCEDURE beforeAll; virtual;
      PROCEDURE afterStep(CONST stepIndex:longint; CONST elapsed:double); virtual;
      PROCEDURE configChanged; virtual;
    public
      CONSTRUCTOR createEditorWorkflow(CONST messageQueue_:P_structuredMessageQueue);
      PROCEDURE stepChanged(CONST index:longint);
      PROCEDURE swapStepDown(CONST index:longint);
      PROCEDURE removeStep(CONST index:longint);
  end;

  T_generateImageWorkflow=object(T_abstractWorkflow)
    private
      relatedEditor:P_editorWorkflow;
      editingStep:longint;
      addingNewStep:boolean;
      current:P_algorithmMeta;
      PROCEDURE setAlgorithmIndex(CONST index:longint);
      FUNCTION getAlgorithmIndex:longint;
    protected
      PROCEDURE beforeAll; virtual;
      PROCEDURE headlessWorkflowExecution; virtual;
    public
      CONSTRUCTOR createOneStepWorkflow(CONST messageQueue_:P_structuredMessageQueue; CONST relatedEditor_:P_editorWorkflow);
      PROPERTY algorithmIndex:longint read getAlgorithmIndex write setAlgorithmIndex;
      PROPERTY algoritm:P_algorithmMeta read current;
      FUNCTION startEditing(CONST stepIndex:longint):boolean;
      PROCEDURE startEditingForNewStep;
      PROCEDURE confirmEditing;
      FUNCTION isValid: boolean; virtual;
      FUNCTION limitedDimensionsForResizeStep(CONST tgtDim:T_imageDimensions):T_imageDimensions; virtual;
  end;

  T_standaloneWorkflow=object(T_simpleWorkflow)
    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
  end;

IMPLEMENTATION
//These are binding uses
//Initialization of those units registers image operations
USES imageManipulation,
     im_stashing,
     im_geometry,
     im_colors,
     im_statisticOperations,
     im_filter,
     im_misc,
     ig_gradient,
     ig_perlin,
     ig_simples,
     ig_fractals,
     ig_epicycles,
     ig_ifs,
     ig_ifs2,
     ig_bifurcation,
     ig_funcTrees,
     ig_expoClouds,
     ig_factorTables,
     ig_circlespirals,
     myStringUtil;

CONSTRUCTOR T_standaloneWorkflow.create;
  VAR ownedMessageQueue:P_structuredMessageQueue;
  begin
    new(ownedMessageQueue,create);
    inherited createSimpleWorkflow(ownedMessageQueue);
  end;

DESTRUCTOR T_standaloneWorkflow.destroy;
  VAR ownedMessageQueue:P_structuredMessageQueue;
  begin
    ownedMessageQueue:=messageQueue;
    inherited destroy;
    dispose(ownedMessageQueue,destroy);
  end;

PROCEDURE T_generateImageWorkflow.beforeAll;
  begin
    enterCriticalSection(contextCS);
    enterCriticalSection(relatedEditor^.contextCS);
    try
      image.resize(relatedEditor^.config.initialResolution,res_dataResize);
      if (editingStep>0) and (editingStep-1<relatedEditor^.stepCount) and (relatedEditor^.step[editingStep-1]^.outputImage<>nil) then begin
        image.copyFromPixMap(relatedEditor^.step[editingStep-1]^.outputImage^);
        relatedEditor^.config.limitImageSize(image);
      end else image.drawCheckerboard;
    finally
      leaveCriticalSection(contextCS);
      leaveCriticalSection(relatedEditor^.contextCS);
    end;
  end;

PROCEDURE T_generateImageWorkflow.headlessWorkflowExecution;
  VAR stepStarted:double;
  begin
    stepStarted:=now;
    {$ifdef debugMode}
    writeln(stdErr,'DEBUG T_generateImageWorkflow.headlessWorkflowExecution start');
    {$endif}
    current^.prototype^.execute(@self);
    {$ifdef debugMode}
    writeln(stdErr,'DEBUG T_generateImageWorkflow.headlessWorkflowExecution finalizing');
    {$endif}
    enterCriticalSection(contextCS);
    try
      if currentExecution.workflowState=ts_evaluating
      then begin
        messageQueue^.Post('Done '+myTimeToStr(now-stepStarted),false,-1);
        currentExecution.workflowState:=ts_ready;
      end else begin
        messageQueue^.Post('Cancelled '+myTimeToStr(now-stepStarted),false,-1);
        currentExecution.workflowState:=ts_cancelled;
      end;
    finally
      leaveCriticalSection(contextCS);
      {$ifdef debugMode}
      writeln(stdErr,'DEBUG T_generateImageWorkflow.headlessWorkflowExecution done');
      {$endif}
    end;
  end;

CONSTRUCTOR T_generateImageWorkflow.createOneStepWorkflow(
  CONST messageQueue_: P_structuredMessageQueue;
  CONST relatedEditor_: P_editorWorkflow);
  begin
    inherited createContext(messageQueue_);
    relatedEditor:=relatedEditor_;
    current:=imageGenerationAlgorithms[0];
  end;

FUNCTION T_generateImageWorkflow.startEditing(CONST stepIndex: longint
  ): boolean;
  begin
    if not(relatedEditor^.step[stepIndex]^.isValid) or
       (relatedEditor^.step[stepIndex]^.operation=nil) or
       (relatedEditor^.step[stepIndex]^.operation^.meta^.category<>imc_generation) then exit(false);
    current:=P_algorithmMeta(relatedEditor^.step[stepIndex]^.operation^.meta);
    if not(current^.prototype^.canParseParametersFromString(relatedEditor^.step[stepIndex]^.specification,true)) then exit(false);
    addingNewStep:=false;
    editingStep:=stepIndex;
    result:=true;
  end;

PROCEDURE T_generateImageWorkflow.startEditingForNewStep;
  begin
    current:=imageGenerationAlgorithms[0];
    addingNewStep:=true;
    editingStep:=relatedEditor^.stepCount;
  end;

PROCEDURE T_generateImageWorkflow.confirmEditing;
  begin
    if not(isValid) then exit;
    if addingNewStep then begin
      {$ifdef debugMode}writeln(stdErr,'DEBUG T_generateImageWorkflow.confirmEditing: adding a new step');{$endif}
      relatedEditor^.addStep(current^.prototype^.toString(tsm_forSerialization));
    end else begin
      {$ifdef debugMode}writeln(stdErr,'DEBUG T_generateImageWorkflow.confirmEditing: updating step #',editingStep);{$endif}
      relatedEditor^.step[editingStep]^.specification:=current^.prototype^.toString(tsm_forSerialization);
    end;
  end;

PROCEDURE T_generateImageWorkflow.setAlgorithmIndex(CONST index: longint);
  begin
    if (index>=0) and (index<length(imageGenerationAlgorithms)) then
    current:=imageGenerationAlgorithms[index];
  end;

FUNCTION T_generateImageWorkflow.getAlgorithmIndex: longint;
  begin
    result:=current^.index;
  end;

FUNCTION T_generateImageWorkflow.isValid: boolean;
  begin
    result:=true;
  end;

FUNCTION T_generateImageWorkflow.limitedDimensionsForResizeStep(CONST tgtDim: T_imageDimensions): T_imageDimensions;
  begin
    result:=relatedEditor^.config.limitedDimensionsForResizeStep(tgtDim);
  end;

PROCEDURE T_simpleWorkflow.headlessWorkflowExecution;
  VAR stepStarted:double;
  begin
    enterCriticalSection(contextCS);
    while (currentExecution.workflowState=ts_evaluating) and (currentExecution.currentStepIndex<length(steps)) do begin
      leaveCriticalSection(contextCS);
      stepStarted:=now;
      steps[currentExecution.currentStepIndex]^.execute(@self);
      enterCriticalSection(contextCS);
      afterStep(currentExecution.currentStepIndex,now-stepStarted);
      inc(currentExecution.currentStepIndex);
    end;
    afterAll;
    leaveCriticalSection(contextCS);
  end;

CONST reportStepTimeIfLargerThan=5/(24*60*60);
PROCEDURE T_simpleWorkflow.afterStep(CONST stepIndex: longint;
  CONST elapsed: double);
  VAR accessedStash:string='';
      thereIsALaterAccess:boolean=false;
      i:longint;
  begin
    if elapsed>reportStepTimeIfLargerThan then messageQueue^.Post('Finished step after '+myTimeToStr(elapsed),false,currentStepIndex);
    begin
      accessedStash                         :=steps[stepIndex]^.operation^.readsStash;
      if accessedStash='' then accessedStash:=steps[stepIndex]^.operation^.writesStash;
      if accessedStash<>'' then begin
        //This step just accessed a stash
        //The stash can be dropped if there is no later reading access
        for i:=stepIndex+1 to length(steps)-1 do thereIsALaterAccess:=thereIsALaterAccess or (steps[i]^.operation^.readsStash=accessedStash);
        if not(thereIsALaterAccess) then stash.clearSingleStash(accessedStash);
      end;
    end;
  end;

PROCEDURE T_editorWorkflow.afterStep(CONST stepIndex: longint; CONST elapsed: double);
  begin
    if elapsed>reportStepTimeIfLargerThan then messageQueue^.Post('Finished step after '+myTimeToStr(elapsed),false,currentStepIndex);
    step[stepIndex]^.saveOutputImage(image);
  end;

PROCEDURE T_simpleWorkflow.afterAll;
  begin
    waitForFinishOfParallelTasks;
    enterCriticalSection(contextCS);
    try
      stash.clear;
      if currentExecution.workflowState in [ts_pending,ts_evaluating] then currentExecution.workflowState:=ts_ready;
      if currentExecution.workflowState in [ts_stopRequested        ] then currentExecution.workflowState:=ts_cancelled;
      case currentExecution.workflowState of
        ts_ready: messageQueue^.Post('Workflow done',false);
        ts_cancelled: messageQueue^.Post('Workflow cancelled',false,currentExecution.currentStepIndex);
      end;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION T_simpleWorkflow.getStep(index: longint): P_workflowStep;
  begin
    if (index>=0) and (index<length(steps))
    then result:=steps[index]
    else result:=nil;
  end;

CONSTRUCTOR T_simpleWorkflow.createSimpleWorkflow(
  CONST messageQueue_: P_structuredMessageQueue);
  begin
    inherited createContext(messageQueue_);
    config.create(@configChanged);
    setLength(steps,0);
  end;

DESTRUCTOR T_simpleWorkflow.destroy;
  begin
    {$ifdef debugMode}
    writeln(stdErr,'DEBUG T_simpleWorkflow.destroy (enter)');
    {$endif}
    ensureStop;
    clear;
    config.destroy;
    setLength(steps,0);
    {$ifdef debugMode}
    writeln(stdErr,'DEBUG T_simpleWorkflow.destroy (call inherited)');
    {$endif}
    inherited destroy;
    {$ifdef debugMode}
    writeln(stdErr,'DEBUG T_simpleWorkflow.destroy (exit)');
    {$endif}
  end;

FUNCTION T_simpleWorkflow.stepCount: longint;
  begin
    result:=length(steps);
  end;

PROCEDURE T_simpleWorkflow.clear;
  VAR i:longint;
  begin
    enterCriticalSection(contextCS);
    try
      inherited clear;
      for i:=0 to length(steps)-1 do dispose(steps[i],destroy);
      setLength(steps,0);
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_simpleWorkflow.beforeAll;
  begin
    enterCriticalSection(contextCS);
    try
      currentExecution.workflowState:=ts_evaluating;
      currentExecution.currentStepIndex:=0;
      config.prepareImageForWorkflow(image);
      messageQueue^.Post('Starting workflow',false)
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_editorWorkflow.beforeAll;
  VAR i:longint;
  begin
    enterCriticalSection(contextCS);
    try
      currentExecution.workflowState:=ts_evaluating;
      currentExecution.currentStepIndex:=0;
      if previewQuality<>config.intermediateResultsPreviewQuality
      then begin
        for i:=0 to length(steps)-1 do steps[i]^.clearOutputImage;
        stash.clear;
        config.intermediateResultsPreviewQuality:=previewQuality;
      end;
      with currentExecution do while (currentStepIndex<length(steps)) and (steps[currentStepIndex]^.outputImage<>nil) do inc(currentStepIndex);
      if currentExecution.currentStepIndex>0
      then image.copyFromPixMap(steps[currentExecution.currentStepIndex-1]^.outputImage^)
      else config.prepareImageForWorkflow(image);
      if currentExecution.currentStepIndex=0
      then messageQueue^.Post('Starting workflow',false)
      else messageQueue^.Post('Resuming workflow',false,currentExecution.currentStepIndex);
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION T_simpleWorkflow.parseWorkflow(CONST data: T_arrayOfString): boolean;
  VAR newSteps:array of P_workflowStep;
      i:longint;
  begin
    setLength(newSteps,length(data));
    result:=true;
    for i:=0 to length(newSteps)-1 do begin
      new(newSteps[i],create(data[i]));
      if not(newSteps[i]^.isValid) then begin
        result:=false;
        messageQueue^.Post('Invalid step: '+data[i],true,i);
      end;
    end;
    if not(result) then begin
      for i:=0 to length(newSteps)-1 do dispose(newSteps[i],destroy);
    end else begin
      clear;
      enterCriticalSection(contextCS);
      try
        setLength(steps,length(newSteps));
        for i:=0 to length(steps)-1 do steps[i]:=newSteps[i];
        setLength(newSteps,0);
      finally
        leaveCriticalSection(contextCS);
      end;
    end;
  end;

FUNCTION T_simpleWorkflow.workflowText: T_arrayOfString;
  VAR i:longint;
  begin
    enterCriticalSection(contextCS);
    try
      setLength(result,length(steps));
      for i:=0 to length(steps)-1 do result[i]:=steps[i]^.specification;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION T_simpleWorkflow.readFromFile(CONST fileName: string): boolean;
  begin
    messageQueue^.Post('Trying to parse workflow from file: '+fileName,false);
    if not(fileExists(fileName)) then begin
      messageQueue^.Post('File "'+fileName+'" does not exist');
      result:=false;
    end else begin
      result:=parseWorkflow(readFile(fileName));
      if result then config.workflowFilename:=fileName;
    end;
  end;

PROCEDURE T_simpleWorkflow.saveToFile(CONST fileName: string);
  begin
    messageQueue^.Post('Writing workflow to file: '+fileName,false);
    writeFile(fileName,workflowText);
    config.workflowFilename:=fileName;
  end;

PROCEDURE T_simpleWorkflow.saveAsTodo(CONST savingToFile: string;
  CONST savingWithSizeLimit: longint);
  VAR todoName:string;
      temporaryWorkflow:T_arrayOfString;
  begin
    temporaryWorkflow:=config.getFirstTodoStep;
    append(temporaryWorkflow,workflowText);
    append(temporaryWorkflow,getSaveStatement(savingToFile,savingWithSizeLimit));
    repeat
      todoName:='T'+intToStr(random(maxLongint))+'.todo';
    until not(fileExists(todoName));
    messageQueue^.Post('Writing todo to file: '+todoName,false);
    writeFile(todoName,temporaryWorkflow);
  end;

PROCEDURE T_simpleWorkflow.appendSaveStep(CONST savingToFile: string;
  CONST savingWithSizeLimit: longint);
  VAR k:longint;
  begin
    enterCriticalSection(contextCS);
    try
      k:=length(steps);
      setLength(steps,k+1);
      new(steps[k],create(getSaveStatement(savingToFile,savingWithSizeLimit)));
    finally
      leaveCriticalSection(contextCS);
    end;
    if not(steps[k]^.isValid) then raise Exception.create('The automatically generated save step is invalid');
  end;

PROCEDURE T_simpleWorkflow.executeAsTodo;
  begin
    addStep(deleteOp.getOperationToDeleteFile(config.workflowFilename));
    executeWorkflowInBackground(false);
  end;

PROCEDURE T_editorWorkflow.stepChanged(CONST index: longint);
  VAR i:longint;
  begin
    ensureStop;
    enterCriticalSection(contextCS);
    try
      if (index>=0) and (index<length(steps)) then step[index]^.refreshSpecString;
      //The simple approach: clear stash and restore it from output images:
      stash.clear;
      for i:=0 to index-1 do if (steps[i]^.isValid) and (steps[i]^.outputImage<>nil) and (steps[i]^.operation^.writesStash<>'') then
        stash.stashImage(steps[i]^.operation^.writesStash,steps[i]^.outputImage^);
      for i:=index to length(steps)-1 do steps[i]^.clearOutputImage;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION T_simpleWorkflow.addStep(CONST specification: string): boolean;
  VAR newStep:P_workflowStep;
  begin
    enterCriticalSection(contextCS);
    try
      new(newStep,create(specification));
      if newStep^.isValid then begin
        setLength(steps,length(steps)+1);
        steps[length(steps)-1]:=newStep;
        result:=true;
      end else begin
        messageQueue^.Post('Invalid step was rejected: '+specification,true);
        dispose(newStep,destroy);
        result:=false;
      end;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_simpleWorkflow.addStep(CONST operation: P_imageOperation);
  VAR newStep:P_workflowStep;
  begin
    enterCriticalSection(contextCS);
    try
      new(newStep,create(operation));
      setLength(steps,length(steps)+1);
      steps[length(steps)-1]:=newStep;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_editorWorkflow.swapStepDown(CONST index: longint);
  VAR tmp:P_workflowStep;
  begin
    if (index>=0) and (index<length(steps)-1) then begin
      ensureStop;
      enterCriticalSection(contextCS);
      try
        tmp           :=steps[index  ];
        steps[index  ]:=steps[index+1];
        steps[index+1]:=tmp;
        isValid; //query "isValid" to trigger validation
        stepChanged(index);
      finally
        leaveCriticalSection(contextCS);
      end;
    end;
  end;

PROCEDURE T_editorWorkflow.removeStep(CONST index: longint);
  VAR i:longint;
  begin
    if (index>=0) and (index<length(steps)) then begin
      ensureStop;
      enterCriticalSection(contextCS);
      try
        dispose(steps[index],destroy);
        for i:=index to length(steps)-2 do steps[i]:=steps[i+1];
        setLength(steps,length(steps)-1);
        isValid; //query "isValid" to trigger validation
        stepChanged(index);
      finally
        leaveCriticalSection(contextCS);
      end;
    end;
  end;

FUNCTION T_simpleWorkflow.workflowType: T_workflowType;
  begin
    if (length(steps)<=0) or not(isValid) then exit(wft_empty_or_unknown);
    if not(step[0]^.operation^.dependsOnImageBefore) then exit(wft_generative);
    result:=wft_manipulative;
  end;

FUNCTION T_simpleWorkflow.proposedImageFileName(CONST resString: ansistring
  ): string;
  VAR i:longint;
      newExt:ansistring;
  begin
    if (workflowType<>wft_generative) or (resString='')
    then newExt:=''
    else newExt:='_'+resString;
    result:=ChangeFileExt(config.workflowFilename,newExt+lowercase(JPG_EXT));
    if fileExists(result) then begin
      i:=0;
      repeat
        inc(i);
        result:=ChangeFileExt(config.workflowFilename,newExt+'_'+intToStr(i)+lowercase(JPG_EXT));
      until not(fileExists(result))
    end;
  end;

FUNCTION T_simpleWorkflow.isValid: boolean;
  VAR s:P_workflowStep;
      i,j:longint;
      stashId:string;
      writtenBeforeRead:boolean;
  begin
    //Every single step has to be valid
    for s in steps do if not(s^.isValid) then exit(false);
    //Reading stash access must not take place before writing
    result:=true;
    for i:=0 to length(steps)-1 do begin
      stashId:=steps[i]^.operation^.readsStash;
      if stashId<>'' then begin
        writtenBeforeRead:=false;
        for j:=0 to i-1 do writtenBeforeRead:=writtenBeforeRead or (steps[j]^.operation^.writesStash=stashId);
        if not(writtenBeforeRead) then begin
          messageQueue^.Post('Stash "'+stashId+'" is read before it is written',true,i);
          result:=false;
        end;
      end;
    end;
  end;

FUNCTION T_simpleWorkflow.limitedDimensionsForResizeStep(CONST tgtDim: T_imageDimensions): T_imageDimensions;
  begin
    result:=config.limitedDimensionsForResizeStep(tgtDim);
  end;

PROCEDURE T_simpleWorkflow.configChanged;
  begin
    //no op...
  end;

PROCEDURE T_editorWorkflow.configChanged;
  begin
    stepChanged(0);
  end;

CONSTRUCTOR T_editorWorkflow.createEditorWorkflow(CONST messageQueue_: P_structuredMessageQueue);
  begin
    inherited createContext(messageQueue_);
    config.create(@configChanged);
    setLength(steps,0);
  end;

end.
