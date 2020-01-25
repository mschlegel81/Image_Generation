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
      FUNCTION getDefaultOperation:P_imageOperation; virtual; abstract;
  end;

  { T_imageOperation }

  T_imageOperation=object
    protected
      fMeta:P_imageOperationMeta;
    public
      CONSTRUCTOR create(CONST meta:P_imageOperationMeta);
      PROPERTY meta:P_imageOperationMeta read fMeta;
      PROCEDURE execute(CONST context:P_imageGenerationContext); virtual; abstract;
      FUNCTION getSimpleParameterValue:P_parameterValue; virtual;
      FUNCTION isSingleton:boolean; virtual;
      DESTRUCTOR destroy; virtual;
      FUNCTION readsStash:string; virtual;
      FUNCTION writesStash:string; virtual;
      FUNCTION dependsOnImageBefore:boolean; virtual; abstract;
      FUNCTION toString(nameMode:T_parameterNameMode):string; virtual; abstract;
      FUNCTION alterParameter(CONST newParameterString:string):boolean; virtual; abstract;
  end;

  F_errorFeedbackRoutine=PROCEDURE(CONST message:string) of object;
  T_taskState=(ts_pending,    //set on construction
               ts_evaluating, //set on dequeue
               ts_ready,      //set after evaluation
               ts_cancelled,
               ts_stopRequested);
  P_parallelTask=^T_parallelTask;
  T_parallelTask=object
    containedIn:P_imageGenerationContext;
    nextTask   :P_parallelTask;
    state:T_taskState;
    CONSTRUCTOR create;
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual; abstract;
  end;

  T_imageGenerationContext=object
    protected
      contextCS:TRTLCriticalSection;
      currentExecution:record
        currentStepIndex:longint;
        workflowState:T_taskState;
      end;
    private
      queue:record
        firstTask,
        lastTask:P_parallelTask;
        stepsTotal,stepsDone,
        queuedCount,
        workerCount:longint;
        queueStartedAt:double;
      end;
      PROCEDURE notifyWorkerStopped;
      PROCEDURE logParallelStepDone;
      PROCEDURE ensureWorkers;
    public
      previewQuality:boolean;
      messageQueue:P_structuredMessageQueue;
      stash:T_imageStash;
      image:T_rawImage;

      CONSTRUCTOR create(CONST messageQueue_:P_structuredMessageQueue);
      DESTRUCTOR destroy; virtual;
      //Parellelization:
      PROCEDURE clearQueue;
      PROCEDURE enqueueAll(CONST task:P_parallelTask);
      PROCEDURE enqueue   (CONST task:P_parallelTask);
      FUNCTION  dequeue              :P_parallelTask;
      PROCEDURE waitForFinishOfParallelTasks;
      //General workflow control
      PROCEDURE ensureStop;
      PROCEDURE postStop;
      FUNCTION  executing:boolean; //TODO: Make this virtual; Only the workflow sets the state correctly...
      FUNCTION  cancellationRequested:boolean;
      PROCEDURE cancelWithError(CONST errorMessage:string);
      PROCEDURE clear;
      PROPERTY currentStepIndex:longint read currentExecution.currentStepIndex;
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

PROCEDURE cleanupOperations;
  VAR i:longint;
  begin
    for i:=0 to length(imageOperations)-1 do dispose(imageOperations[i],destroy);
    setLength(imageOperations,0);
  end;

DESTRUCTOR T_imageOperation.destroy; begin end;
CONSTRUCTOR T_imageOperation.create(CONST meta: P_imageOperationMeta); begin fMeta:=meta; end;
FUNCTION T_imageOperation.getSimpleParameterValue: P_parameterValue; begin result:=nil; end;
FUNCTION T_imageOperation.isSingleton: boolean; begin result:=false; end;
FUNCTION T_imageOperation.readsStash: string; begin result:=''; end;
FUNCTION T_imageOperation.writesStash: string; begin result:=''; end;

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

CONSTRUCTOR T_imageGenerationContext.create(CONST messageQueue_:P_structuredMessageQueue);
  begin
    initCriticalSection(contextCS);
    messageQueue:=messageQueue_;
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
      stash.destroy;
      image.destroy;
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
      messageQueue^.Post('Stopping',false,currentExecution.currentStepIndex);
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
      messageQueue^.Post('Stopping',false,currentExecution.currentStepIndex);
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
      messageQueue^.Post(errorMessage,true,currentExecution.currentStepIndex);
    end;
    leaveCriticalSection(contextCS);
  end;

PROCEDURE T_imageGenerationContext.clear;
  begin
    ensureStop;
    enterCriticalSection(contextCS);
    try
      stash.clear;
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

INITIALIZATION
  maxImageManipulationThreads:=getNumberOfCPUs;
end.

