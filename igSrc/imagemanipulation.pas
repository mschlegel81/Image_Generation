UNIT imageManipulation;
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

  { T_imageOperationMeta }
  P_imageOperationMeta=^T_imageOperationMeta;
  T_imageOperationMeta=object
    private
      cat:T_imageManipulationCategory;
    protected
      name:string;
    public
      CONSTRUCTOR create(CONST name_:string; CONST cat_:T_imageManipulationCategory);
      DESTRUCTOR destroy; virtual;
      FUNCTION canParseParametersFromString(CONST s:ansistring; CONST doParse:boolean=false):boolean; virtual; abstract;
      PROCEDURE execute(CONST specification:string; CONST context:P_imageGenerationContext); virtual; abstract;
      PROPERTY category:T_imageManipulationCategory read cat;
      PROPERTY getName:string read name;
  end;

  F_resolveAlternativeOperation=FUNCTION(CONST specification:string):P_imageOperationMeta;

  P_simpleImageOperationMeta=^T_simpleImageOperationMeta;
  T_simpleImageOperationMeta=object(T_imageOperationMeta)
    private
      operation:F_simpleImageOperation;
      signature:P_parameterDescription;
    public
      CONSTRUCTOR create(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation);
      DESTRUCTOR destroy; virtual;
      FUNCTION canParseParametersFromString(CONST s:ansistring; CONST doParse:boolean=false):boolean; virtual;
      PROCEDURE execute(CONST specification:string; CONST context:P_imageGenerationContext); virtual;
  end;

  F_errorFeedbackRoutine=PROCEDURE(CONST message:string) of object;

  {Image stash; owned by a serial workflow}
  P_imageStash=^T_imageStash;

  { T_imageStash }
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
      operationMeta:P_imageOperationMeta;
      PROCEDURE setSpecification(CONST spec:string);
    protected
      PROCEDURE reset; virtual;
      PROCEDURE afterSuccessfulValidation; virtual;
    public
      CONSTRUCTOR create(CONST spec:string);
      DESTRUCTOR destroy; virtual;
      PROCEDURE execute(CONST context:P_imageGenerationContext);
      FUNCTION  dependsOnImageBefore:boolean; virtual;
      PROPERTY specification:string read specString write setSpecification;
      PROPERTY isValid:boolean read valid;
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
      workflowState:T_taskState;
      steps: array of P_workflowStep;
      imageSizeLimit:T_imageDimensions;
      previewQuality:boolean;
      currentStepIndex:longint;
      PROCEDURE notifyWorkerStopped;
      PROCEDURE logParallelStepDone;
      PROCEDURE ensureWorkers;
      PROCEDURE defaultErrorRoutine(CONST message:string);
      PROCEDURE headlessWorkflowExecution;
    public
      stash:T_imageStash;
      image:T_rawImage;

      CONSTRUCTOR create(CONST onError:F_errorFeedbackRoutine);
      DESTRUCTOR destroy;
      //Parellelization:
      PROCEDURE clearQueue;
      PROCEDURE enqueueAll(CONST task:P_parallelTask);
      PROCEDURE enqueue   (CONST task:P_parallelTask);
      FUNCTION  dequeue              :P_parallelTask;
      PROCEDURE waitForFinishOfParallelTasks;
      //General workflow control
      PROPERTY previewMode:boolean read previewQuality;
      PROCEDURE ensureStop;
      PROCEDURE postStop;
      FUNCTION  executing:boolean;
      FUNCTION  cancellationRequested:boolean;
      PROCEDURE cancelWithError(CONST errorMessage:string);
      //Execution
      FUNCTION  stateMessage:T_structuredMessage;
      PROCEDURE executeWorkflow            (CONST preview:boolean);
      PROCEDURE executeWorkflowInBackground(CONST preview:boolean);
    protected
      PROCEDURE beforeAll; virtual;
      PROCEDURE afterAll ; virtual;
      PROCEDURE beforeStep(CONST index:longint); virtual;
      PROCEDURE afterStep (CONST index:longint); virtual;
    public
      PROCEDURE clear;
      FUNCTION parseWorkflow(CONST data:T_arrayOfString):T_structuredMessage;
      FUNCTION readFromFile(CONST fileName:string):T_structuredMessage;
      PROCEDURE saveToFile(CONST fileName:string);
  end;

VAR maxImageManipulationThreads:longint=1;
    maxMessageLength:longint=100;
    resolveAlternativeOperation:F_resolveAlternativeOperation=nil;

PROCEDURE registerSimpleOperation(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation);
IMPLEMENTATION
VAR globalWorkersRunning:longint=0;
    simpleOperations:array of P_simpleImageOperationMeta;

PROCEDURE registerSimpleOperation(CONST cat_:T_imageManipulationCategory; CONST sig:P_parameterDescription; CONST op:F_simpleImageOperation);
  VAR meta:P_simpleImageOperationMeta;
  begin
    new(meta,create(cat_,sig,op));
    setLength(simpleOperations,length(simpleOperations)+1);
    simpleOperations[length(simpleOperations)-1]:=meta;
  end;

PROCEDURE T_workflowStep.setSpecification(CONST spec: string);
  VAR meta:P_simpleImageOperationMeta;
  begin
    specString:=spec;
    operationMeta:=nil;
    for meta in simpleOperations do if meta^.canParseParametersFromString(spec) then begin operationMeta:=meta; break; end;
    if (operationMeta=nil) and (resolveAlternativeOperation<>nil) then operationMeta:=resolveAlternativeOperation(spec);
    if operationMeta=nil then valid:=false
    else begin
      valid:=true;
      afterSuccessfulValidation;
    end;
  end;

PROCEDURE T_workflowStep.reset;
  begin
  end;

PROCEDURE T_workflowStep.afterSuccessfulValidation;
  begin
  end;

CONSTRUCTOR T_workflowStep.create(CONST spec: string);
  begin
    setSpecification(spec);
  end;

DESTRUCTOR T_workflowStep.destroy;
  begin
  end;

PROCEDURE T_workflowStep.execute(CONST context: P_imageGenerationContext);
  begin
    if valid then operationMeta^.execute(specString,context);
  end;

FUNCTION T_workflowStep.dependsOnImageBefore: boolean;
  begin
    result:=false;
  end;

{ T_simpleImageOperationMeta }

CONSTRUCTOR T_simpleImageOperationMeta.create(CONST cat_: T_imageManipulationCategory; CONST sig: P_parameterDescription; CONST op: F_simpleImageOperation);
  begin
    inherited create(sig^.name,cat_);
    operation:=op;
    signature:=sig;
  end;

DESTRUCTOR T_simpleImageOperationMeta.destroy;
  begin
    inherited destroy;
    dispose(signature,destroy);
  end;

FUNCTION T_simpleImageOperationMeta.canParseParametersFromString(CONST s: ansistring; CONST doParse: boolean): boolean;
  VAR parameters:T_parameterValue;
  begin
    parameters.createToParse(signature,s,tsm_forSerialization);
    result:=parameters.isValid;
  end;

PROCEDURE T_simpleImageOperationMeta.execute(CONST specification: string; CONST context: P_imageGenerationContext);
  VAR parameters:T_parameterValue;
  begin
    parameters.createToParse(signature,specification,tsm_forSerialization);
    if parameters.isValid
    then operation(parameters,context)
    else context^.cancelWithError('Invalid workflow step: '+specification);
  end;

{ T_imageOperationMeta }

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
        if workflowState in [ts_cancelled,ts_stopRequested]
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
    while (workflowState=ts_evaluating) and (currentStepIndex<length(steps)) do begin
      beforeStep(currentStepIndex);
      leaveCriticalSection(contextCS);

      steps[currentStepIndex]^.execute(@self);

      enterCriticalSection(contextCS);
      afterStep(currentStepIndex);
      inc(currentStepIndex);
    end;
    if workflowState=ts_evaluating then afterAll;
    leaveCriticalSection(contextCS);
  end;

CONSTRUCTOR T_imageGenerationContext.create(
  CONST onError: F_errorFeedbackRoutine);
  begin
    initCriticalSection(contextCS);
    errorFeedbackRoutine:=onError;
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
    workflowState:=ts_cancelled;
    previewQuality:=false;
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
    if workflowState=ts_evaluating then workflowState:=ts_stopRequested;
    leaveCriticalSection(contextCS);
    waitForFinishOfParallelTasks;
  end;

PROCEDURE T_imageGenerationContext.postStop;
  begin
    enterCriticalSection(contextCS);
    if workflowState=ts_evaluating then workflowState:=ts_stopRequested;
    leaveCriticalSection(contextCS);
  end;

FUNCTION T_imageGenerationContext.executing: boolean;
  begin
    enterCriticalSection(contextCS);
    result:=workflowState in [ts_pending,ts_evaluating,ts_stopRequested];
    leaveCriticalSection(contextCS);
  end;

FUNCTION T_imageGenerationContext.cancellationRequested: boolean;
  begin
    enterCriticalSection(contextCS);
    result:=workflowState=ts_stopRequested;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_imageGenerationContext.cancelWithError(CONST errorMessage: string);
  begin
    enterCriticalSection(contextCS);
    if workflowState=ts_evaluating then workflowState:=ts_stopRequested;
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
      result:=intToStr(currentStepIndex+1)+'/'+intToStr(length(steps));
      if (currentStepIndex>=0) and (currentStepIndex<length(steps))
      then result+=steps[currentStepIndex]^.specification;
      if length(result)>remainingLength-3
      then result:=copy(result,1,remainingLength-3)+'...';
      result+=queueProgressString
    end;

  begin
    enterCriticalSection(contextCS);
      result.error:=false;
      case workflowState of
        ts_pending: begin
          result.meta:=-1;
          result.message:='pending execution';
        end;
        ts_evaluating: begin
          result.meta:=currentStepIndex;
          result.message:=getStepMessage(maxMessageLength);
        end;
        ts_ready: begin
          result.meta:=-1;
          result.message:='done';
        end;
        ts_cancelled: begin
          result.meta:=currentStepIndex;
          result.message:='cancelled at step '+intToStr(currentStepIndex);
        end;
        ts_stopRequested: begin
          result.meta:=currentStepIndex;
          result.message:=getStepMessage(maxMessageLength-10)+'- stopping';
        end;
      end;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_imageGenerationContext.executeWorkflow(CONST preview: boolean);
  begin
    ensureStop;
    previewQuality:=preview;
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
    previewQuality:=preview;
    beforeAll;
    beginThread(@runWorkflow,@self);
  end;

PROCEDURE T_imageGenerationContext.beforeAll;
  begin
    workflowState:=ts_evaluating;
    currentStepIndex:=0;
  end;

PROCEDURE T_imageGenerationContext.afterAll;
  begin
    waitForFinishOfParallelTasks;
    enterCriticalSection(contextCS);
    try
      stash.clear;
      if workflowState in [ts_pending,ts_evaluating] then workflowState:=ts_ready;
      if workflowState in [ts_stopRequested        ] then workflowState:=ts_cancelled;
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

FUNCTION T_imageGenerationContext.parseWorkflow(CONST data: T_arrayOfString
  ): T_structuredMessage;
  begin
    //TODO: Stub
    setLength(steps,length(data));

  end;

FUNCTION T_imageGenerationContext.readFromFile(CONST fileName: string
  ): T_structuredMessage;
  begin
    if not(fileExists(fileName)) then begin
      result.error:=true;
      result.message:='File "'+fileName+'" does not exist';
    end;

    //TODO: Stub

  end;

PROCEDURE T_imageGenerationContext.saveToFile(CONST fileName: string);
  begin
    //TODO: Stub

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

