UNIT imageContexts;
INTERFACE
USES sysutils,
     myParams,
     myStringUtil,
     myGenerics,
     pixMaps,
     mypics,
     mySys,
     Dialogs;

TYPE
  P_imageGenerationContext=^T_imageGenerationContext;
  F_simpleImageOperation=PROCEDURE(CONST parameters:T_parameterValue; CONST context:P_imageGenerationContext);

  T_imageManipulationCategory=(imc_generation,imc_imageAccess,imc_geometry,imc_colors,imc_combination,imc_statistic,imc_filter,imc_misc);
  P_imageOperation=^T_imageOperation;
  P_imageOperationMeta=^T_imageOperationMeta;
  T_imageOperationMeta=object
    private
      cat:T_imageManipulationCategory;
    protected
      name:string;
    public
      CONSTRUCTOR create(CONST name_:string; CONST cat_:T_imageManipulationCategory);
      DESTRUCTOR destroy; virtual;
      PROPERTY category:T_imageManipulationCategory read cat;
      PROPERTY getName:string read name;
      FUNCTION parse(CONST specification:ansistring):P_imageOperation; virtual; abstract;
  end;

  T_imageOperation=object
    PROCEDURE execute(CONST context:P_imageGenerationContext); virtual; abstract;
    FUNCTION isSingleton:boolean; virtual;
    DESTRUCTOR destroy; virtual;
    FUNCTION readsStash:string; virtual;
    FUNCTION writesStash:string; virtual;
  end;

  ////TODO: Move to separate unit
  //P_simpleImageOperationMeta=^T_simpleImageOperationMeta;
  //T_simpleImageOperationMeta=object(T_imageOperationMeta)
  //  private
  //    operation:F_simpleImageOperation;
  //    signature:P_parameterDescription;
  //  public
  //    CONSTRUCTOR create(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation);
  //    DESTRUCTOR destroy; virtual;
  //    FUNCTION parse(CONST specification:ansistring):P_imageOperation; virtual;
  //end;
  //
  //T_simpleOperationKind=(sok_simple,sok_readingStash,sok_writingStash,sok_singleton);
  //
  ////TODO: Move to separate unit
  //P_simpleImageOperation=^T_simpleImageOperation;
  //T_simpleImageOperation=object(T_imageOperation)
  //  private
  //    kind      :T_simpleOperationKind;
  //    meta      :P_simpleImageOperationMeta;
  //    parameters:T_parameterValue;
  //  public
  //    CONSTRUCTOR create(CONST meta_:P_simpleImageOperationMeta; CONST parameters_:T_parameterValue; CONST simpleOperationKind:T_simpleOperationKind=sok_simple);
  //    PROCEDURE execute(CONST context:P_imageGenerationContext); virtual; abstract;
  //    FUNCTION isSingleton:boolean; virtual;
  //    DESTRUCTOR destroy; virtual;
  //    FUNCTION readsStash:string; virtual;
  //    FUNCTION writesStash:string; virtual;
  //end;

  F_errorFeedbackRoutine=PROCEDURE(CONST message:string) of object;

  {Image stash; owned by a serial workflow}
  P_imageStash=^T_imageStash;
  T_imageStash=object
    private
      item:array of record
             img:P_rawImage;
             id:string;
           end;
      errorRoutine:F_errorFeedbackRoutine;
    public
      CONSTRUCTOR create(CONST onError:F_errorFeedbackRoutine);
      DESTRUCTOR destroy;
    private
      FUNCTION getStashIndexForId(CONST id:string; CONST allowCreation:boolean):longint;
    public
      PROCEDURE clear;
      PROCEDURE stashImage      (CONST id:string; VAR source:T_rawImage);
      PROCEDURE unstashImage    (CONST id:string; VAR target:T_rawImage);
      FUNCTION  getStashedImage (CONST id:string): P_rawImage;
      PROCEDURE clearSingleStash(CONST id:string);
  end;

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
      FUNCTION  dependsOnImageBefore:boolean;
      PROPERTY specification:string read specString write setSpecification;
      PROPERTY isValid:boolean read valid;
      PROPERTY operation:P_imageOperation read operation_;
      PROCEDURE clearOutputImage;
  end;

  T_structuredMessage=record
    error  :boolean;
    meta   :longint;
    message:string;
  end;

  { T_imageGenerationContext }

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
      errorFeedbackRoutine:F_errorFeedbackRoutine;
      steps: array of P_workflowStep;
      currentExecution:record
        previewQuality:boolean;
        currentStepIndex:longint;
        workflowState:T_taskState;
      end;
    protected
      config:record
        initialImageFilename:string;
        retainIntermediateResults:boolean;
        intermediateResultsPreviewQuality:boolean;
        initialResolution,
        imageSizeLimit   :T_imageDimensions;
        initialImage     :P_rawImage;
      end;
    private
      PROCEDURE notifyWorkerStopped;
      PROCEDURE logParallelStepDone;
      PROCEDURE ensureWorkers;
      PROCEDURE defaultErrorRoutine(CONST message:string);
      PROCEDURE headlessWorkflowExecution;
      FUNCTION getStep(index:longint):P_workflowStep;
    public
      stash:T_imageStash;
      image:T_rawImage;

      CONSTRUCTOR create(CONST onError:F_errorFeedbackRoutine; CONST retainIntermediate:boolean);
      DESTRUCTOR destroy;
      //Parellelization:
      PROCEDURE clearQueue;
      PROCEDURE enqueueAll(CONST task:P_parallelTask);
      PROCEDURE enqueue   (CONST task:P_parallelTask);
      FUNCTION  dequeue              :P_parallelTask;
      PROCEDURE waitForFinishOfParallelTasks;
      //General workflow control
      PROPERTY previewMode:boolean read currentExecution.previewQuality;
      PROCEDURE ensureStop;
      PROCEDURE postStop;
      FUNCTION  executing:boolean;
      FUNCTION  cancellationRequested:boolean;
      PROCEDURE cancelWithError(CONST errorMessage:string);
      //Execution
      FUNCTION  stateMessage:T_structuredMessage;
      PROCEDURE executeWorkflow            (CONST preview:boolean);
      PROCEDURE executeWorkflowInBackground(CONST preview:boolean);
      PROCEDURE setInitialResolution(CONST res:T_imageDimensions);
      PROCEDURE setInitialImage(CONST fileName:string);
    protected
      PROCEDURE beforeAll;
      PROCEDURE afterAll ;
      PROCEDURE beforeStep(CONST index:longint);
      PROCEDURE afterStep (CONST index:longint);
    public
      PROCEDURE clear;
      FUNCTION parseWorkflow(CONST data:T_arrayOfString):T_structuredMessage;
      FUNCTION workflowText:T_arrayOfString;
      FUNCTION readFromFile(CONST fileName:string):T_structuredMessage;
      PROCEDURE saveToFile(CONST fileName:string);
      PROPERTY step[index:longint]: P_workflowStep read getStep;
  end;

VAR maxImageManipulationThreads:longint=1;
    maxMessageLength:longint=100;

//PROCEDURE registerSimpleOperation(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation);
PROCEDURE registerOperation(CONST meta:P_imageOperationMeta);
IMPLEMENTATION
VAR globalWorkersRunning:longint=0;
    simpleOperations:array of P_imageOperationMeta;

//PROCEDURE registerSimpleOperation(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation);
//  VAR meta:P_simpleImageOperationMeta;
//  begin
//    new(meta,create(cat_,sig,op));
//    registerOperation(meta);
//  end;

PROCEDURE registerOperation(CONST meta: P_imageOperationMeta);
  begin
    setLength(simpleOperations,length(simpleOperations)+1);
    simpleOperations[length(simpleOperations)-1]:=meta;
  end;

DESTRUCTOR T_imageOperation.destroy; begin end;
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
    for meta in simpleOperations do if operation_=nil then operation_:=meta^.parse(specString);
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
    if valid then operation_^.execute(context);
  end;

FUNCTION T_workflowStep.dependsOnImageBefore: boolean;
  begin
    result:=false;
  end;

PROCEDURE T_workflowStep.clearOutputImage;
  begin
    if outputImage<>nil then dispose(outputImage,destroy);
    outputImage:=nil;
  end;

//CONSTRUCTOR T_simpleImageOperationMeta.create(
//  CONST cat_: T_imageManipulationCategory; CONST sig: P_parameterDescription;
//  CONST op: F_simpleImageOperation);
//  begin
//    inherited create(sig^.name,cat_);
//    operation:=op;
//    signature:=sig;
//  end;
//
//DESTRUCTOR T_simpleImageOperationMeta.destroy;
//  begin
//    inherited destroy;
//    dispose(signature,destroy);
//  end;
//
//FUNCTION T_simpleImageOperationMeta.parse(CONST specification: ansistring): P_imageOperation;
//  VAR simple:P_simpleImageOperationMeta;
//  begin
//    //TODO:Stub
//  end;
//
//FUNCTION T_simpleImageOperationMeta.canParseParametersFromString(CONST s: ansistring; CONST doParse: boolean): boolean;
//  VAR parameters:T_parameterValue;
//  begin
//    parameters.createToParse(signature,s,tsm_forSerialization);
//    result:=parameters.isValid;
//  end;
//
//PROCEDURE T_simpleImageOperationMeta.execute(CONST specification: string; CONST context: P_imageGenerationContext);
//  VAR parameters:T_parameterValue;
//  begin
//    parameters.createToParse(signature,specification,tsm_forSerialization);
//    if parameters.isValid
//    then operation(parameters,context)
//    else context^.cancelWithError('Invalid workflow step: '+specification);
//  end;

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

PROCEDURE T_imageGenerationContext.defaultErrorRoutine(CONST message: string);
  begin ShowMessage('Error :'+message); end;

PROCEDURE T_imageGenerationContext.headlessWorkflowExecution;
  begin
    enterCriticalSection(contextCS);
    while (currentExecution.workflowState=ts_evaluating) and (currentExecution.currentStepIndex<length(steps)) do begin
      beforeStep(currentExecution.currentStepIndex);
      leaveCriticalSection(contextCS);

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

CONSTRUCTOR T_imageGenerationContext.create(
  CONST onError: F_errorFeedbackRoutine; CONST retainIntermediate: boolean);
  begin
    initCriticalSection(contextCS);
    errorFeedbackRoutine:=onError;
    config.retainIntermediateResults:=retainIntermediate;
    if errorFeedbackRoutine=nil then errorFeedbackRoutine:=@defaultErrorRoutine;
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
    currentExecution.workflowState:=ts_cancelled;
    currentExecution.previewQuality:=false;
  end;

DESTRUCTOR T_imageGenerationContext.destroy;
  begin
    ensureStop;
    enterCriticalSection(contextCS);
    try
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
      lastTask:=lastTask^.nextTask;
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
    if currentExecution.workflowState=ts_evaluating then currentExecution.workflowState:=ts_stopRequested;
    leaveCriticalSection(contextCS);
    waitForFinishOfParallelTasks;
  end;

PROCEDURE T_imageGenerationContext.postStop;
  begin
    enterCriticalSection(contextCS);
    if currentExecution.workflowState=ts_evaluating then currentExecution.workflowState:=ts_stopRequested;
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
    if currentExecution.workflowState=ts_evaluating then currentExecution.workflowState:=ts_stopRequested;
    if errorFeedbackRoutine<>nil then errorFeedbackRoutine(errorMessage);
    leaveCriticalSection(contextCS);
  end;

FUNCTION T_imageGenerationContext.stateMessage: T_structuredMessage;
  FUNCTION getStepMessage(CONST maxLength:longint):string;
    VAR queueProgressString:string='';
        queueProgress      :double=0;
        queueTimeRemaining :double=0;
        remainingLength:longint;
    begin
      with queue do if (workerCount>0) or (queuedCount>0) then begin
        queueProgress:=stepsDone/stepsTotal;
        // (t_End - t_Start) / stepsTotal = (now - t_start) / stepsDone
        //  t_End                         = (now - t_start) * stepsTotal / stepsDone + t_Start
        queueTimeRemaining:=((now-queueStartedAt)*stepsTotal/stepsDone+queueStartedAt-now)*24*60*60;
        queueProgressString:=' '+intToStr(round(100*queueProgress))+'% (rem: '+myTimeToStr(queueTimeRemaining)+')';
      end;
      remainingLength:=maxLength-length(queueProgressString);
      result:=intToStr(currentExecution.currentStepIndex+1)+'/'+intToStr(length(steps));
      if (currentExecution.currentStepIndex>=0) and (currentExecution.currentStepIndex<length(steps))
      then result+=steps[currentExecution.currentStepIndex]^.specification;
      if length(result)>remainingLength-3
      then result:=copy(result,1,remainingLength-3)+'...';
      result+=queueProgressString
    end;

  begin
    enterCriticalSection(contextCS);
      result.error:=false;
      case currentExecution.workflowState of
        ts_pending: begin
          result.meta:=-1;
          result.message:='pending execution';
        end;
        ts_evaluating: begin
          result.meta:=currentExecution.currentStepIndex;
          result.message:=getStepMessage(maxMessageLength);
        end;
        ts_ready: begin
          result.meta:=-1;
          result.message:='done';
        end;
        ts_cancelled: begin
          result.meta:=currentExecution.currentStepIndex;
          result.message:='cancelled at step '+intToStr(currentExecution.currentStepIndex);
        end;
        ts_stopRequested: begin
          result.meta:=currentExecution.currentStepIndex;
          result.message:=getStepMessage(maxMessageLength-10)+'- stopping';
        end;
      end;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_imageGenerationContext.executeWorkflow(CONST preview: boolean);
  begin
    ensureStop;
    currentExecution.previewQuality:=preview;
    beforeAll;
    headlessWorkflowExecution;
  end;

FUNCTION runWorkflow(p:pointer):ptrint; register;
  begin
    P_imageGenerationContext(p)^.headlessWorkflowExecution;
    result:=0;
  end;

PROCEDURE T_imageGenerationContext.executeWorkflowInBackground(
  CONST preview: boolean);
  begin
    ensureStop;
    currentExecution.previewQuality:=preview;
    beforeAll;
    beginThread(@runWorkflow,@self);
  end;

PROCEDURE T_imageGenerationContext.setInitialResolution(CONST res:T_imageDimensions);
  VAR i:longint;
  begin
    if config.imageSizeLimit=res then exit;
    ensureStop;
    enterCriticalSection(contextCS);
    try
      for i:=0 to length(steps)-1 do steps[i]^.clearOutputImage;
      config.imageSizeLimit:=res;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.setInitialImage(CONST fileName: string);
  begin
    if fileName=config.initialImageFilename then exit;
    enterCriticalSection(contextCS);
    try
      config.initialImageFilename:=fileName;
      if (config.initialImage<>nil) then begin
        dispose(config.initialImage,destroy);
        config.initialImage:=nil;
      end;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.beforeAll;
  PROCEDURE tryRestoreInitialImage;
    begin

      //TODO: load initial image, if filename is given and not loaded
      //      use initial image, if "resuming" at step 0
      image.drawCheckerboard;
    end;

  VAR i:longint;
  begin
    enterCriticalSection(contextCS);
    try
      currentExecution.workflowState:=ts_evaluating;
      currentExecution.currentStepIndex:=0;
      if config.retainIntermediateResults then begin
        if currentExecution.previewQuality<>config.intermediateResultsPreviewQuality
        then for i:=0 to length(steps)-1 do steps[i]^.clearOutputImage;
        with currentExecution do while (currentStepIndex<length(steps)) and (steps[currentStepIndex]^.outputImage<>nil) do inc(currentStepIndex);
        if currentExecution.currentStepIndex>0
        then image.copyFromPixMap(steps[currentExecution.currentStepIndex-1]^.outputImage^)
        else tryRestoreInitialImage;
      end else tryRestoreInitialImage;
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
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_imageGenerationContext.beforeStep(CONST index: longint);
  begin
    //TODO: stub

  end;

PROCEDURE T_imageGenerationContext.afterStep(CONST index: longint);
  begin
    //TODO: stub

  end;

PROCEDURE T_imageGenerationContext.clear;
  VAR i:longint;
  begin
    ensureStop;
    enterCriticalSection(contextCS);
    try
      stash.clear;
      for i:=0 to length(steps)-1 do dispose(steps[i],destroy);
      setLength(steps,0);
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

FUNCTION T_imageGenerationContext.parseWorkflow(CONST data: T_arrayOfString): T_structuredMessage;
  VAR newSteps:array of P_workflowStep;
      firstInvalid:longint=-1;
      i:longint;

  begin
    setLength(newSteps,length(data));
    for i:=0 to length(newSteps)-1 do new(newSteps[i],create(data[i]));
    for i:=length(newSteps)-1 downto 0 do if not(newSteps[i]^.isValid) then firstInvalid:=i;
    if firstInvalid>=0 then begin
      result.error:=true;
      result.meta:=firstInvalid;
      result.message:='Invalid step #'+intToStr(firstInvalid)+' "'+data[firstInvalid]+'"';
      for i:=0 to length(newSteps)-1 do dispose(newSteps[i],destroy);
      exit;
    end;
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

FUNCTION T_imageGenerationContext.workflowText: T_arrayOfString;
  VAR i:longint;
  begin
    setLength(result,length(steps));
    for i:=0 to length(steps)-1 do result[i]:=steps[i]^.specification;
  end;

FUNCTION T_imageGenerationContext.readFromFile(CONST fileName: string): T_structuredMessage;
  begin
    if not(fileExists(fileName)) then begin
      result.error:=true;
      result.message:='File "'+fileName+'" does not exist';
    end else result:=parseWorkflow(readFile(fileName));
  end;

PROCEDURE T_imageGenerationContext.saveToFile(CONST fileName: string);
  begin
    writeFile(fileName,workflowText);
  end;

CONSTRUCTOR T_imageStash.create(CONST onError: F_errorFeedbackRoutine);
  begin
     errorRoutine:=onError;
     setLength(item,0);
  end;

DESTRUCTOR T_imageStash.destroy;
  begin
    clear;
  end;

FUNCTION T_imageStash.getStashIndexForId(CONST id: string; CONST allowCreation: boolean): longint;
  VAR i:longint;
  begin
    result:=-1;
    for i:=0 to length(item)-1 do if item[i].id=id then exit(i);
    if allowCreation then begin
      result:=length(item);
      setLength(item,result+1);
      item[result].id :=id;
      item[result].img:=nil;
    end;
  end;

PROCEDURE T_imageStash.clear;
  VAR i:longint;
  begin
    for i:=0 to length(item)-1 do with item[i] do begin
      if img<>nil then dispose(img,destroy);
      img:=nil;
      id:='';
    end;
    setLength(item,0);
  end;

PROCEDURE T_imageStash.stashImage(CONST id: string; VAR source: T_rawImage);
  begin
    with item[getStashIndexForId(id,true)] do begin
      if img<>nil then dispose(img,destroy);
      new(img,create(source));
    end;
  end;

PROCEDURE T_imageStash.unstashImage(CONST id: string; VAR target: T_rawImage);
  VAR i:longint;
  begin
    i:=getStashIndexForId(id,false);
    if i<0 then begin
      errorRoutine('Invalid stash  "'+id+'"');
      exit;
    end;
    with item[i] do
    if img=nil then errorRoutine('Uninitialized stash "'+id+'"')
    else target.copyFromPixMap(img^);
  end;

FUNCTION T_imageStash.getStashedImage(CONST id: string): P_rawImage;
  VAR i:longint;
  begin
    i:=getStashIndexForId(id,false);
    if i<0 then begin
      errorRoutine('Invalid stash  "'+id+'"');
      new(result,create(1,1));
      exit(result);
    end;
    with item[i] do begin
      if img=nil then errorRoutine('Uninitialized stash "'+id+'"');
      result:=img;
    end;
  end;

PROCEDURE T_imageStash.clearSingleStash(CONST id: string);
  VAR i:longint;
  begin
    i:=getStashIndexForId(id,false);
    if (i>0) then with item[i] do if img<>nil then begin
      dispose(img,destroy);
      img:=nil;
    end;
  end;

INITIALIZATION
  maxImageManipulationThreads:=getNumberOfCPUs;
end.

