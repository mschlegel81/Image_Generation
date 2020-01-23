UNIT imageContexts;
INTERFACE
USES sysutils,
     myParams,
     myStringUtil,
     myGenerics,
     pixMaps,
     mypics,
     mySys,
     Dialogs,
     imageStashes,
     generationBasics;

TYPE
  P_imageGenerationContext=^T_imageGenerationContext;

  T_imageManipulationCategory=(imc_generation,imc_imageAccess,imc_geometry,imc_colors,imc_combination,imc_statistic,imc_filter,imc_misc);
  T_workflowType=(wft_generative,wft_manipulative,wft_fixated,wft_halfFix,wft_empty_or_unknown);
  P_imageOperation=^T_imageOperation;
  P_imageOperationMeta=^T_imageOperationMeta;
  T_imageOperationMeta=object
    private
      cat:T_imageManipulationCategory;
    protected
      name:string;
    public
      CONSTRUCTOR create(CONST name_:string; CONST cat_:T_imageManipulationCategory);
      PROPERTY category:T_imageManipulationCategory read cat;
      PROPERTY getName:string read name;
      DESTRUCTOR destroy; virtual;
      FUNCTION parse(CONST specification:ansistring):P_imageOperation; virtual; abstract;
      FUNCTION getSimpleParameterDescription:P_parameterDescription; virtual; abstract;
  end;

  T_imageOperation=object
    PROCEDURE execute(CONST context:P_imageGenerationContext); virtual; abstract;
    FUNCTION getSimpleParameterValue:P_parameterValue; virtual;
    FUNCTION isSingleton:boolean; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION readsStash:string; virtual;
    FUNCTION writesStash:string; virtual;
    FUNCTION dependsOnImageBefore:boolean; virtual; abstract;
  end;

  F_errorFeedbackRoutine=PROCEDURE(CONST message:string) of object;
  T_taskState=(ts_pending,    //set on construction
               ts_evaluating, //set on dequeue
               ts_ready,      //set after evaluation
               ts_cancelled,
               ts_stopRequested);
  P_parallelTask=^T_parallelTask;

  {Abstract task}
  T_parallelTask=object
    containedIn:P_imageGenerationContext;
    nextTask   :P_parallelTask;
    state:T_taskState;
    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual; abstract;
  end;

  P_workflowStep=^T_workflowStep;
  T_workflowStep=object
    private
      specString   :string;
      valid        :boolean;
      operation_   :P_imageOperation;
      PROCEDURE setSpecification(CONST spec:string);
    public
      outputImage:P_rawImage;
      CONSTRUCTOR create(CONST spec:string);
      DESTRUCTOR destroy;
      PROCEDURE execute(CONST context:P_imageGenerationContext);
      PROPERTY specification:string read specString write setSpecification;
      PROPERTY isValid:boolean read valid;
      PROPERTY operation:P_imageOperation read operation_;
      PROCEDURE clearOutputImage;
      PROCEDURE saveOutputImage(VAR image:T_rawImage);
  end;

  T_imageGenerationContext=object
    private
      contextCS:TRTLCriticalSection;
      queue:record
        firstTask,
        lastTask:P_parallelTask;
        stepsTotal,stepsDone,
        queuedCount,
        workerCount:longint;
        queueStartedAt:double;
      end;
      steps: array of P_workflowStep;
      currentExecution:record
        currentStepIndex:longint;
        workflowState:T_taskState;
      end;
      PROCEDURE notifyWorkerStopped;
      PROCEDURE logParallelStepDone;
      PROCEDURE ensureWorkers;
      PROCEDURE headlessWorkflowExecution;
      FUNCTION getStep(index:longint):P_workflowStep;
    public
      config:T_imageGenerationContextConfiguration;
      messageQueue:T_structuredMessageQueue;
      stash:T_imageStash;
      image:T_rawImage;

      CONSTRUCTOR create(CONST retainIntermediate:boolean);
      DESTRUCTOR destroy;
      //Parellelization:
      PROCEDURE clearQueue;
      PROCEDURE enqueueAll(CONST task:P_parallelTask);
      PROCEDURE enqueue   (CONST task:P_parallelTask);
      FUNCTION  dequeue              :P_parallelTask;
      PROCEDURE waitForFinishOfParallelTasks;
      //General workflow control
      PROCEDURE ensureStop;
      PROCEDURE postStop;
      FUNCTION  executing:boolean;
      FUNCTION  cancellationRequested:boolean;
      PROCEDURE cancelWithError(CONST errorMessage:string);
      //Execution
      PROCEDURE executeWorkflow            (CONST preview:boolean);
      PROCEDURE executeWorkflowInBackground(CONST preview:boolean);

      PROCEDURE appendSaveStep(CONST sizeLimit:longint; CONST fileName:string);

      //config access
      PROCEDURE setInitialResolution(CONST res: T_imageDimensions);
      PROCEDURE setImageSizeLimit   (CONST res: T_imageDimensions);

    protected
      PROCEDURE beforeAll;
      PROCEDURE afterAll ;
    public
      PROCEDURE clear;
      FUNCTION parseWorkflow(CONST data:T_arrayOfString):boolean;
      FUNCTION workflowText:T_arrayOfString;
      FUNCTION readFromFile(CONST fileName:string):boolean;
      PROCEDURE saveToFile(CONST fileName:string);
      PROCEDURE saveAsTodo(CONST savingToFile:string; CONST savingWithSizeLimit:longint);
      //Editing
      PROPERTY step[index:longint]: P_workflowStep read getStep;
      FUNCTION stepCount:longint;
      PROCEDURE stepChanged(CONST index:longint);

      FUNCTION workflowType:T_workflowType;
      FUNCTION proposedImageFileName(CONST resString:ansistring):string;
      FUNCTION isValid:boolean;
  end;
VAR maxImageManipulationThreads:longint=1;
    maxMessageLength:longint=100;
    imageOperations:array of P_imageOperationMeta;
PROCEDURE registerOperation(CONST meta: P_imageOperationMeta);
IMPLEMENTATION
VAR globalWorkersRunning:longint=0;
PROCEDURE registerOperation(CONST meta: P_imageOperationMeta);
  begin
    setLength(imageOperations,length(imageOperations)+1);
    imageOperations[length(imageOperations)-1]:=meta;
  end;

DESTRUCTOR T_imageOperation.destroy; begin end;
FUNCTION T_imageOperation.getSimpleParameterValue: P_parameterValue; begin result:=nil; end;
FUNCTION T_imageOperation.isSingleton: boolean; begin result:=false; end;
FUNCTION T_imageOperation.readsStash: string; begin result:=''; end;
FUNCTION T_imageOperation.writesStash: string; begin result:=''; end;

PROCEDURE T_workflowStep.setSpecification(CONST spec: string);
  VAR meta:P_imageOperationMeta;
  begin
    if specString=spec then exit;
    specString:=spec;
    if (operation_<>nil) and not(operation_^.isSingleton) then dispose(operation_,destroy);
    operation_:=nil;
    for meta in imageOperations do if operation_=nil then operation_:=meta^.parse(specString);
    valid:=operation_<>nil;
    clearOutputImage;
  end;

CONSTRUCTOR T_workflowStep.create(CONST spec: string);
  begin
    operation_:=nil;
    setSpecification(spec);
    outputImage:=nil;
  end;

DESTRUCTOR T_workflowStep.destroy;
  begin
    if outputImage<>nil then dispose(outputImage,destroy);
    if (operation_<>nil) and not(operation_^.isSingleton) then dispose(operation_,destroy);
  end;

PROCEDURE T_workflowStep.execute(CONST context: P_imageGenerationContext);
  begin
    if valid then begin
      context^.messageQueue.Post(specification,false,context^.currentExecution.currentStepIndex);
      operation_^.execute(context);
    end else begin
      context^.cancelWithError('Invalid step: '+specification);
    end;
  end;

PROCEDURE T_workflowStep.clearOutputImage;
  begin
    if outputImage<>nil then dispose(outputImage,destroy);
    outputImage:=nil;
  end;

PROCEDURE T_workflowStep.saveOutputImage(VAR image:T_rawImage);
  begin
    if outputImage=nil
    then new(outputImage,create(image))
    else outputImage^.copyFromPixMap(image);
  end;

CONSTRUCTOR T_imageOperationMeta.create(CONST name_: string; CONST cat_: T_imageManipulationCategory);
  begin
    name:=name_;
    cat :=cat_;
  end;

DESTRUCTOR T_imageOperationMeta.destroy;
  begin
  end;

CONSTRUCTOR T_parallelTask.create;
  begin
    containedIn:=nil;
    nextTask   :=nil;
    state      :=ts_pending;
  end;

DESTRUCTOR T_parallelTask.destroy;
  begin
  end;

PROCEDURE T_imageGenerationContext.notifyWorkerStopped;
  begin
    enterCriticalSection(contextCS);
    try
      dec(queue.workerCount);
      interlockedDecrement(globalWorkersRunning);
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.logParallelStepDone;
  begin
    enterCriticalSection(contextCS);
    try
      inc(queue.stepsDone);
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION worker(p:pointer):ptrint; register;
  VAR task:P_parallelTask;
  begin
    result:=0;
    with P_imageGenerationContext(p)^ do begin
      task:=dequeue;
      while (task<>nil) do begin
        task^.state:=ts_evaluating;
        if currentExecution.workflowState in [ts_cancelled,ts_stopRequested]
        then result:=-1
        else begin
          task^.execute;
          logParallelStepDone;
        end;
        dispose(task,destroy);
        task:=dequeue;
      end;
      notifyWorkerStopped;
    end;
  end;

PROCEDURE T_imageGenerationContext.ensureWorkers;
  begin
    enterCriticalSection(contextCS);
    try
      while (queue.workerCount<=0) or (globalWorkersRunning<maxImageManipulationThreads) do begin
        inc(queue.workerCount);
        interLockedIncrement(globalWorkersRunning);
        beginThread(@worker,@self);
      end;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.headlessWorkflowExecution;
  VAR stepStarted:double;
  PROCEDURE afterStep(CONST index: longint);
    VAR accessedStash:string='';
        thereIsALaterAccess:boolean=false;
        i:longint;
    begin
      //TODO: post a message if the step took more than 5 seconds or so...
      if config.retainIntermediateResults
      then step[index]^.saveOutputImage(image)
      else begin
        accessedStash                         :=step[index]^.operation_^.readsStash;
        if accessedStash='' then accessedStash:=step[index]^.operation_^.writesStash;
        if accessedStash<>'' then begin
          //This step just accessed a stash
          //The stash can be dropped if there is no later reading access
          for i:=index+1 to length(steps)-1 do thereIsALaterAccess:=thereIsALaterAccess or (steps[i]^.operation_^.readsStash=accessedStash);
          if not(thereIsALaterAccess) then stash.clearSingleStash(accessedStash);
        end;
      end;
    end;

  begin
    enterCriticalSection(contextCS);
    while (currentExecution.workflowState=ts_evaluating) and (currentExecution.currentStepIndex<length(steps)) do begin
      leaveCriticalSection(contextCS);
      stepStarted:=now;
      steps[currentExecution.currentStepIndex]^.execute(@self);
      enterCriticalSection(contextCS);
      afterStep(currentExecution.currentStepIndex);
      inc(currentExecution.currentStepIndex);
    end;
    if currentExecution.workflowState=ts_evaluating then afterAll;
    leaveCriticalSection(contextCS);
  end;

FUNCTION T_imageGenerationContext.getStep(index: longint): P_workflowStep;
  begin
    if (index>=0) and (index<length(steps))
    then result:=steps[index]
    else result:=nil;
  end;

CONSTRUCTOR T_imageGenerationContext.create(CONST retainIntermediate: boolean);
  begin
    initCriticalSection(contextCS);
    config.create(retainIntermediate);
    messageQueue.create;
    with queue do begin
      firstTask     :=nil;
      lastTask      :=nil;
      queuedCount   :=0;
      workerCount   :=0;
      stepsDone     :=0;
      stepsTotal    :=0;
    end;
    stash         .create(@cancelWithError);
    image         .create(1,1);
    setLength(steps,0);
    with currentExecution do begin
      currentStepIndex:=0;
      workflowState:=ts_cancelled;
    end;
  end;

DESTRUCTOR T_imageGenerationContext.destroy;
  begin
    ensureStop;
    enterCriticalSection(contextCS);
    try
      messageQueue.destroy;
      config.destroy;
      stash.destroy;
      image.destroy;
      setLength(steps,0);
    finally
      leaveCriticalSection(contextCS);
    end;
    doneCriticalSection(contextCS);
  end;

PROCEDURE T_imageGenerationContext.clearQueue;
  begin
    enterCriticalSection(contextCS);
    with queue do while workerCount>0 do begin
      leaveCriticalSection(contextCS);
      sleep(1);
      enterCriticalSection(contextCS);
    end;
    with queue do try
      firstTask     :=nil;
      lastTask      :=nil;
      queuedCount   :=0;
      workerCount   :=0;
      stepsDone     :=0;
      stepsTotal    :=0;
      queueStartedAt:=now;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.enqueueAll(CONST task: P_parallelTask);
  begin
    enterCriticalSection(contextCS);
    with queue do try
      if firstTask=nil
      then firstTask         :=task
      else lastTask^.nextTask:=task;
      while lastTask^.nextTask<>nil do begin
        lastTask^.nextTask^.containedIn:=@self;
        lastTask:=lastTask^.nextTask;
        inc(queuedCount);
        inc(stepsTotal);
      end;
      ensureWorkers;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.enqueue(CONST task: P_parallelTask);
  begin
    enterCriticalSection(contextCS);
    with queue do try
      if firstTask=nil
      then firstTask         :=task
      else lastTask^.nextTask:=task;
      task^.containedIn:=@self;
      lastTask:=task;
      inc(queuedCount);
      ensureWorkers;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION T_imageGenerationContext.dequeue: P_parallelTask;
  begin
    enterCriticalSection(contextCS);
    with queue do try
      result:=firstTask;
      if firstTask<>nil then begin
        firstTask:=firstTask^.nextTask;
        dec(queuedCount);
      end;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.waitForFinishOfParallelTasks;
  begin
    with queue do if (workerCount>=0) then begin
      enterCriticalSection(contextCS);
      inc(workerCount);
      leaveCriticalSection(contextCS);
      worker(@self);
      if workerCount<>0 then raise Exception.create('Worker count must be zero at this point.');
    end;
  end;

PROCEDURE T_imageGenerationContext.ensureStop;
  begin
    enterCriticalSection(contextCS);
    if currentExecution.workflowState=ts_evaluating then begin
      currentExecution.workflowState:=ts_stopRequested;
      messageQueue.Post('Stopping',false,currentExecution.currentStepIndex);
    end;
    leaveCriticalSection(contextCS);
    waitForFinishOfParallelTasks;
    enterCriticalSection(contextCS);
    while not(currentExecution.workflowState in [ts_cancelled,ts_ready]) do begin
      leaveCriticalSection(contextCS);
      sleep(10);
      enterCriticalSection(contextCS);
    end;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_imageGenerationContext.postStop;
  begin
    enterCriticalSection(contextCS);
    if currentExecution.workflowState=ts_evaluating then begin
      currentExecution.workflowState:=ts_stopRequested;
      messageQueue.Post('Stopping',false,currentExecution.currentStepIndex);
    end;
    leaveCriticalSection(contextCS);
  end;

FUNCTION T_imageGenerationContext.executing: boolean;
  begin
    enterCriticalSection(contextCS);
    result:=currentExecution.workflowState in [ts_pending,ts_evaluating,ts_stopRequested];
    leaveCriticalSection(contextCS);
  end;

FUNCTION T_imageGenerationContext.cancellationRequested: boolean;
  begin
    enterCriticalSection(contextCS);
    result:=currentExecution.workflowState=ts_stopRequested;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_imageGenerationContext.cancelWithError(CONST errorMessage: string);
  begin
    enterCriticalSection(contextCS);
    if currentExecution.workflowState=ts_evaluating then begin
      currentExecution.workflowState:=ts_stopRequested;
      messageQueue.Post(errorMessage,true,currentExecution.currentStepIndex);
    end;
    leaveCriticalSection(contextCS);
  end;

//function T_imageGenerationContext.stateMessage: T_structuredMessage;
//  FUNCTION getStepMessage(CONST maxLength:longint):string;
//    VAR queueProgressString:string='';
//        queueProgress      :double=0;
//        queueTimeRemaining :double=0;
//        remainingLength:longint;
//    begin
//      with queue do if (workerCount>0) or (queuedCount>0) then begin
//        queueProgress:=stepsDone/stepsTotal;
//        // (t_End - t_Start) / stepsTotal = (now - t_start) / stepsDone
//        //  t_End                         = (now - t_start) * stepsTotal / stepsDone + t_Start
//        queueTimeRemaining:=((now-queueStartedAt)*stepsTotal/stepsDone+queueStartedAt-now)*24*60*60;
//        queueProgressString:=' '+intToStr(round(100*queueProgress))+'% (rem: '+myTimeToStr(queueTimeRemaining)+')';
//      end;
//      remainingLength:=maxLength-length(queueProgressString);
//      result:=intToStr(currentExecution.currentStepIndex+1)+'/'+intToStr(length(steps));
//      if (currentExecution.currentStepIndex>=0) and (currentExecution.currentStepIndex<length(steps))
//      then result+=steps[currentExecution.currentStepIndex]^.specification;
//      if length(result)>remainingLength-3
//      then result:=copy(result,1,remainingLength-3)+'...';
//      result+=queueProgressString
//    end;
//
//  begin
//    enterCriticalSection(contextCS);
//      result.error:=false;
//      case currentExecution.workflowState of
//        ts_pending: begin
//          result.meta:=-1;
//          result.message:='pending execution';
//        end;
//        ts_evaluating: begin
//          result.meta:=currentExecution.currentStepIndex;
//          result.message:=getStepMessage(maxMessageLength);
//        end;
//        ts_ready: begin
//          result.meta:=-1;
//          result.message:='done';
//        end;
//        ts_cancelled: begin
//          result.meta:=currentExecution.currentStepIndex;
//          result.message:='cancelled at step '+intToStr(currentExecution.currentStepIndex);
//        end;
//        ts_stopRequested: begin
//          result.meta:=currentExecution.currentStepIndex;
//          result.message:=getStepMessage(maxMessageLength-10)+'- stopping';
//        end;
//      end;
//    leaveCriticalSection(contextCS);
//  end;

PROCEDURE T_imageGenerationContext.executeWorkflow(CONST preview: boolean);
  begin
    ensureStop;
    config.previewQuality:=preview;
    beforeAll;
    headlessWorkflowExecution;
  end;

FUNCTION runWorkflow(p:pointer):ptrint; register;
  begin
    P_imageGenerationContext(p)^.headlessWorkflowExecution;
    result:=0;
  end;

PROCEDURE T_imageGenerationContext.executeWorkflowInBackground( CONST preview: boolean);
  begin
    ensureStop;
    config.previewQuality:=preview;
    beforeAll;
    beginThread(@runWorkflow,@self);
  end;

PROCEDURE T_imageGenerationContext.setInitialResolution(CONST res: T_imageDimensions);
  VAR i:longint;
  begin
    if config.initialResolution=res then exit;
    ensureStop;
    enterCriticalSection(contextCS);
    try
      for i:=0 to length(steps)-1 do steps[i]^.clearOutputImage;
      config.initialResolution:=res;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.setImageSizeLimit   (CONST res: T_imageDimensions);
  VAR i:longint;
  begin
    if config.sizeLimit=res then exit;
    ensureStop;
    enterCriticalSection(contextCS);
    try
      for i:=0 to length(steps)-1 do steps[i]^.clearOutputImage;
      config.sizeLimit:=res;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION getSaveStatement(CONST savingToFile:string; CONST savingWithSizeLimit:longint):string;
  begin
    result:='save:'+savingToFile;
    if uppercase(extractFileExt(savingToFile))=JPG_EXT
    then result+='@'+intToStr(savingWithSizeLimit);
  end;

PROCEDURE T_imageGenerationContext.appendSaveStep(CONST sizeLimit: longint;
  CONST fileName: string);
  begin
    //TODO: implement me

    //if fileName=C_nullSourceOrTargetFileName then exit;
    //if (sizeLimit>=0) and (uppercase(extractFileExt(fileName))=JPG_EXT) then begin
    //  par.createFromValue(stepParamDescription[imt_saveJpgWithSizeLimit],fileName,sizeLimit);
    //  new(saveStep,create(imt_saveJpgWithSizeLimit,par));
    //end else begin
    //  par.createFromValue(stepParamDescription[imt_saveImage],fileName);
    //  new(saveStep,create(imt_saveImage,par));
    //end;
  end;

PROCEDURE T_imageGenerationContext.beforeAll;
  VAR i:longint;
  begin
    enterCriticalSection(contextCS);
    try
      currentExecution.workflowState:=ts_evaluating;
      currentExecution.currentStepIndex:=0;
      if config.retainIntermediateResults then begin
        if config.previewQuality<>config.intermediateResultsPreviewQuality
        then begin
          for i:=0 to length(steps)-1 do steps[i]^.clearOutputImage;
          stash.clear;
          config.intermediateResultsPreviewQuality:=config.previewQuality;
        end;
        with currentExecution do while (currentStepIndex<length(steps)) and (steps[currentStepIndex]^.outputImage<>nil) do inc(currentStepIndex);
        if currentExecution.currentStepIndex>0
        then image.copyFromPixMap(steps[currentExecution.currentStepIndex-1]^.outputImage^)
        else config.prepareImageForWorkflow(image);
      end else config.prepareImageForWorkflow(image);
      if currentExecution.currentStepIndex=0
      then messageQueue.Post('Starting workflow',false)
      else messageQueue.Post('Resuming workflow',false,currentExecution.currentStepIndex);
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.afterAll;
  begin
    waitForFinishOfParallelTasks;
    enterCriticalSection(contextCS);
    try
      stash.clear;
      if currentExecution.workflowState in [ts_pending,ts_evaluating] then currentExecution.workflowState:=ts_ready;
      if currentExecution.workflowState in [ts_stopRequested        ] then currentExecution.workflowState:=ts_cancelled;
      case currentExecution.workflowState of
        ts_ready: messageQueue.Post('Workflow done',false);
        ts_cancelled: messageQueue.Post('Workflow cancelled',false,currentExecution.currentStepIndex);
      end;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.clear;
  VAR i:longint;
  begin
    ensureStop;
    enterCriticalSection(contextCS);
    try
      //TODO: Also clear other properties?
      stash.clear;
      for i:=0 to length(steps)-1 do dispose(steps[i],destroy);
      setLength(steps,0);
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION T_imageGenerationContext.parseWorkflow(CONST data: T_arrayOfString): boolean;
  VAR newSteps:array of P_workflowStep;
      i:longint;
  begin
    setLength(newSteps,length(data));
    result:=true;
    for i:=0 to length(newSteps)-1 do begin
      new(newSteps[i],create(data[i]));
      if not(newSteps[i]^.valid) then begin
        result:=false;
        messageQueue.Post('Invalid step: '+data[i],true,i);
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

FUNCTION T_imageGenerationContext.workflowText: T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,length(steps));
    for i:=0 to length(steps)-1 do result[i]:=steps[i]^.specification;
  end;

FUNCTION T_imageGenerationContext.readFromFile(CONST fileName: string): boolean;
  begin
    messageQueue.Post('Trying to parse workflow from file: '+fileName,false);
    if not(fileExists(fileName)) then begin
      messageQueue.Post('File "'+fileName+'" does not exist');
      result:=false;
    end else begin
      result:=parseWorkflow(readFile(fileName));
      if result then config.workflowFilename:=fileName;
    end;
  end;

PROCEDURE T_imageGenerationContext.saveToFile(CONST fileName: string);
  begin
    messageQueue.Post('Writing workflow to file: '+fileName,false);
    writeFile(fileName,workflowText);
    config.workflowFilename:=fileName;
  end;

PROCEDURE T_imageGenerationContext.saveAsTodo(CONST savingToFile:string; CONST savingWithSizeLimit:longint);
  VAR todoName:string;
      temporaryWorkflow:T_arrayOfString;
  begin
    temporaryWorkflow:=config.getFirstTodoStep;
    append(temporaryWorkflow,workflowText);
    append(temporaryWorkflow,getSaveStatement(savingToFile,savingWithSizeLimit));
    repeat
      todoName:='T'+intToStr(random(maxLongint))+'.todo';
    until not(fileExists(todoName));
    messageQueue.Post('Writing todo to file: '+todoName,false);
    writeFile(todoName,temporaryWorkflow);
  end;

FUNCTION T_imageGenerationContext.stepCount: longint;
  begin
    result:=length(steps);
  end;

PROCEDURE T_imageGenerationContext.stepChanged(CONST index: longint);
  VAR i:longint;
  begin
    if not(config.retainIntermediateResults) then exit;
    ensureStop;
    enterCriticalSection(contextCS);
    try
      stash.clear;
      for i:=0 to index-1 do if (steps[i]^.valid) and (steps[i]^.outputImage<>nil) and (steps[i]^.operation^.writesStash<>'') then
        stash.stashImage(steps[i]^.operation^.writesStash,steps[i]^.outputImage^);
      for i:=index to length(steps)-1 do steps[i]^.clearOutputImage;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION T_imageGenerationContext.workflowType: T_workflowType;
  begin
    if (length(steps)<=0) or not(isValid) then exit(wft_empty_or_unknown);
    if not(step[0]^.operation^.dependsOnImageBefore) then exit(wft_generative);
    result:=wft_manipulative;
  end;

FUNCTION T_imageGenerationContext.proposedImageFileName(CONST resString: ansistring): string;
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

FUNCTION T_imageGenerationContext.isValid: boolean;
  VAR s:P_workflowStep;
  begin
    for s in steps do if not(s^.valid) then exit(false);
    result:=true;
  end;

INITIALIZATION
  maxImageManipulationThreads:=getNumberOfCPUs;
end.

