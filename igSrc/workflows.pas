UNIT workflows;
INTERFACE
USES myGenerics,
     pixMaps,
     myParams,mypics,sysutils,imageGeneration,ExtCtrls,mySys,FileUtil,Dialogs,
     generationBasics,
     imageContexts,workflowSteps;

  //TODO: The following operations must be implemented:

  //imt_shine                  imc_misc
  //stepParamDescription[imt_shine               ]:=newParameterDescription('shine',       pt_none);
  //imt_blur                   imc_filter
  //stepParamDescription[imt_blur                ]:=newParameterDescription('blur',        pt_floatOr2Floats,0)^.setDefaultValue('0.2')^.addChildParameterDescription(spa_f0,'x',pt_float)^.addChildParameterDescription(spa_f1,'y',pt_float);
  //imt_lagrangeDiff           imc_filter
  //stepParamDescription[imt_lagrangeDiff        ]:=newParameterDescription('lagrangeDiff',pt_2floats,0)^.setDefaultValue('0.1,0.1')^.addChildParameterDescription(spa_f0,'scanScale',pt_float,0,1)^.addChildParameterDescription(spa_f1,'blurScale',pt_float,0,1);
  //imt_radialBlur             imc_filter
  //stepParamDescription[imt_radialBlur          ]:=newParameterDescription('radialBlur'  ,pt_3floats)^.setDefaultValue('1,0,0');
  //imt_rotationalBlur         imc_filter
  //stepParamDescription[imt_rotationalBlur      ]:=newParameterDescription('rotationalBlur',pt_3floats)^.setDefaultValue('1,0,0');
  //imt_sharpen                imc_filter
  //stepParamDescription[imt_sharpen             ]:=newParameterDescription('sharpen'     ,pt_2floats,0)^
  //.addChildParameterDescription(spa_f0,'scale',pt_float,0,1)^
  //.addChildParameterDescription(spa_f1,'amount',pt_float,0)^
  //.setDefaultValue('0.1,0.5');
  //imt_edges                  imc_filter
  //stepParamDescription[imt_edges               ]:=newParameterDescription('edges' ,pt_none);
  //imt_variance               imc_filter
  //stepParamDescription[imt_variance]:=newParameterDescription('variance',pt_float,0)^.setDefaultValue('0.05')^.addChildParameterDescription(spa_f0,'scale',pt_float);
  //imt_mode                   imc_filter
  //stepParamDescription[imt_mode]:=newParameterDescription('mode',pt_float,0)^.setDefaultValue('0.05')^.addChildParameterDescription(spa_f0,'scale',pt_float);
  //imt_median                 imc_filter
  //stepParamDescription[imt_median]:=newParameterDescription('median',pt_float,0)^.setDefaultValue('0.05')^.addChildParameterDescription(spa_f0,'scale',pt_float);
  //imt_pseudomedian           imc_filter
  //stepParamDescription[imt_pseudomedian]:=newParameterDescription('pseudoMedian',pt_2floats,0)^
  //  .addChildParameterDescription(spa_f0,'rel. sigma',pt_float,0)^
  //  .addChildParameterDescription(spa_f1,'param',pt_float)^
  //  .setDefaultValue('0.1,1');
  //imt_sketch                 imc_misc
  //stepParamDescription[imt_sketch]:=newParameterDescription('sketch',pt_4floats)^
  //  .setDefaultValue('1,0.1,0.8,0.2')^
  //  .addChildParameterDescription(spa_f0,'cover'          ,pt_float,0)^
  //  .addChildParameterDescription(spa_f1,'direction sigma',pt_float,0)^
  //  .addChildParameterDescription(spa_f2,'density'        ,pt_float)^
  //  .addChildParameterDescription(spa_f3,'tolerance'      ,pt_float,0);
  //imt_drip                   imc_misc
  //stepParamDescription[imt_drip]:=newParameterDescription('drip',pt_2floats,0,1)^
  //  .setDefaultValue('0.1,0.01')^
  //  .addChildParameterDescription(spa_f0,'diffusiveness',pt_float,0,1)^
  //  .addChildParameterDescription(spa_f1,'range' ,pt_float,0,1);
  //imt_encircle               imc_misc
  //stepParamDescription[imt_encircle]:=newParameterDescription('encircle',pt_1I2F,0)^
  //  .setDefaultValue('2000,0.5,0.2')^
  //  .addChildParameterDescription(spa_i0,'circle count',pt_integer,1,100000)^
  //  .addChildParameterDescription(spa_f1,'opacity' ,pt_float,0,1)^
  //  .addChildParameterDescription(spa_f2,'circle size' ,pt_float,0);
  //imt_encircleNeon           imc_misc
  //stepParamDescription[imt_encircleNeon]:=newParameterDescription('encircleNeon',pt_1I2F,0)^
  //  .setDefaultValue('2000,0.5,0.2')^
  //  .addChildParameterDescription(spa_i0,'circle count',pt_integer,1,100000)^
  //  .addChildParameterDescription(spa_f1,'opacity' ,pt_float,0,1)^
  //  .addChildParameterDescription(spa_f2,'circle size' ,pt_float,0);
  //imt_spheres                imc_misc
  //stepParamDescription[imt_spheres]:=newParameterDescription('spheres',pt_2I2F,0)^
  //  .setDefaultValue('2000,3,0.2,0.001')^
  //  .addChildParameterDescription(spa_i0,'sphere count',pt_integer,1,100000)^
  //  .addChildParameterDescription(spa_i1,'sphere style',pt_integer,0,3)^
  //  .addChildParameterDescription(spa_f2,'max size' ,pt_float,0,1)^
  //  .addChildParameterDescription(spa_f3,'min size' ,pt_float,0,1);
  //imt_gradient               imc_filter
  //stepParamDescription[imt_gradient]:=newParameterDescription('gradient',pt_float,0)^.setDefaultValue('0.1');
  //imt_direction              imc_filter
  //stepParamDescription[imt_direction]:=newParameterDescription('direction',pt_float,0)^.setDefaultValue('0.1');
  //imt_details                imc_filter
  //stepParamDescription[imt_details]:=newParameterDescription('details',pt_float,0)^.setDefaultValue('0.1');
  //imt_nlm                    imc_filter
  //stepParamDescription[imt_nlm]:=newParameterDescription('nlm',pt_1I1F,0)^
  //  .setDefaultValue('3,0.5')^
  //  .addChildParameterDescription(spa_i0,'scan radius (pixels)',pt_integer,1,10)^
  //  .addChildParameterDescription(spa_f1,'sigma',pt_float,0.001,2);
  //imt_modMed                 imc_filter
  //stepParamDescription[imt_modMed]:=newParameterDescription('modMed',pt_none);
  //imt_halftone               imc_filter
  //stepParamDescription[imt_halftone]:=newParameterDescription('halftone',pt_1I1F)^
  //  .setDefaultValue('0,0.2')^
  //  .addChildParameterDescription(spa_i0,'style',pt_integer,0,7)^
  //  .addChildParameterDescription(spa_f1,'scale',pt_float,0);
  //imt_dropAlpha              imc_misc
  //stepParamDescription[imt_dropAlpha]:=newParameterDescription('dropAlpha',pt_color);
  //imt_retainAlpha            imc_misc
  //stepParamDescription[imt_retainAlpha]:=newParameterDescription('retainAlpha',pt_color);
  //

  //
  //  PROCEDURE doDetails;
  //    VAR temp:T_rawImage;
  //        i:longint;
  //    begin
  //      temp.create(context^.workflowImage);
  //      temp.blur(param.f0,param.f0);
  //      for i:=0 to context^.workflowImage.pixelCount-1 do context^.workflowImage.rawData[i]:=context^.workflowImage.rawData[i]-temp.rawData[i];
  //      temp.destroy;
  //    end;
  //
  //  begin
  //    {$ifdef DEBUG} writeln('Step #',index,': ',toString(),' (@',context^.workflowImage.dimensions.width,'x',context^.workflowImage.dimensions.height,')'); {$endif}
  //
  //    case imageManipulationType of
  //      imt_generateImage: prepareImage(param.fileName,context);
  //      imt_saveImage: context^.workflowImage.saveToFile(expandFileName(param.fileName));
  //      imt_saveJpgWithSizeLimit: context^.workflowImage.saveJpgWithSizeLimit(expandFileName(param.fileName),param.i0);
  //      imt_stashImage: context^.stashImage(param.fileName);
  //      imt_unstashImage: context^.unstashImage(param.fileName);
  //      imt_resize: if plausibleResolution then begin
  //                   {if (index=0) then context^.workflowImage.resize(param.i0,param.i1,res_dataResize)
  //                                 else}context^.workflowImage.resize(param.i0,param.i1,res_exact);
  //                  end;
  //      imt_fit       : if plausibleResolution then context^.workflowImage.resize(param.i0,param.i1,res_fit);
  //      imt_fitExpand : if plausibleResolution then context^.workflowImage.resize(param.i0,param.i1,res_fitExpand);
  //      imt_fill      : if plausibleResolution then context^.workflowImage.resize(param.i0,param.i1,res_cropToFill);
  //      imt_fillRotate: if plausibleResolution then context^.workflowImage.resize(param.i0,param.i1,res_cropRotate);
  //      imt_fitRotate : if plausibleResolution then context^.workflowImage.resize(param.i0,param.i1,res_fitRotate);
  //      imt_crop  : context^.workflowImage.crop(param.f0,param.f1,param.f2,param.f3);
  //      imt_zoom  : context^.workflowImage.zoom(param.f0);
  //      imt_flip  : context^.workflowImage.flip;
  //      imt_flop  : context^.workflowImage.flop;
  //      imt_rotLeft : context^.workflowImage.rotLeft;
  //      imt_rotRight: context^.workflowImage.rotRight;
  //      imt_rotDegrees: context^.workflowImage.rotate(param.f0);
  //      imt_addRGB..imt_minOfStash,imt_blurWithStash: combine;
  //      imt_setColor, imt_tint, imt_project, imt_limit,imt_limitLow,imt_grey,imt_sepia,imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV,imt_unitChannelSum: colorOp;
  //      imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey,imt_compress,imt_compressV,imt_compressSat:statisticColorOp;
  //      imt_mono: monochrome;
  //      imt_quantize: context^.workflowImage.quantize(param.i0);
  //      imt_shine: context^.workflowImage.shine;
  //      imt_blur: context^.workflowImage.blur(param.f0,param.f1);
  //      imt_lagrangeDiff: context^.workflowImage.lagrangeDiffusion(param.f0,param.f1);
  //      imt_radialBlur: context^.workflowImage.radialBlur(param.f0,param.f1,param.f2);
  //      imt_rotationalBlur: context^.workflowImage.rotationalBlur(param.f0,param.f1,param.f2);
  //      imt_sharpen: context^.workflowImage.sharpen(param.f0,param.f1);
  //      imt_edges: context^.workflowImage.prewittEdges;
  //      imt_variance: context^.workflowImage.variance(param.f0);
  //      imt_median: context^.workflowImage.medianFilter(param.f0);
  //      imt_pseudomedian: context^.workflowImage.myFilter(param.f0,param.f1);
  //      imt_mode: context^.workflowImage.modalFilter(param.f0);
  //      imt_sketch: context^.workflowImage.sketch(param.f0,param.f1,param.f2,param.f3);
  //      imt_drip: context^.workflowImage.drip(param.f0,param.f1);
  //      imt_encircle: context^.workflowImage.encircle(param.i0,WHITE,param.f1,param.f2,context^.queue);
  //      imt_encircleNeon: context^.workflowImage.encircle(param.i0,BLACK,param.f1,param.f2,context^.queue);
  //      imt_spheres: context^.workflowImage.bySpheres(param.i0,param.i1,param.f2,param.f3,context^.queue);
  //      imt_direction: redefine(context^.workflowImage.directionMap(param.f0));
  //      imt_details: doDetails;
  //      imt_nlm: context^.workflowImage.nlmFilter(param.i0,param.f1,context^.queue);
  //      imt_modMed: context^.workflowImage.modMedFilter(context^.queue);
  //      imt_halftone: context^.workflowImage.halftone(param.f1*context^.workflowImage.diagonal*0.01,param.i0);
  //      imt_retainAlpha: redefine(context^.workflowImage.rgbaSplit(param.color));
  //      imt_dropAlpha: context^.workflowImage.rgbaSplit(param.color).destroy;
  //    end;
  //  end;
TYPE
  P_simpleWorkflow=^T_simpleWorkflow;

  { T_simpleWorkflow }

  T_simpleWorkflow=object(T_abstractWorkflow)
    protected
      steps: array of P_workflowStep;
      PROCEDURE headlessWorkflowExecution; virtual;
      PROCEDURE afterStep(CONST stepIndex:longint; CONST elapsed:double); virtual;
      PROCEDURE beforeAll; virtual;
      PROCEDURE afterAll ;
      PROCEDURE clear;
      PROCEDURE configChanged; virtual;
    private
      FUNCTION getStep(index:longint):P_workflowStep;

    public
      config:T_imageWorkflowConfiguration;
      CONSTRUCTOR createSimpleWorkflow(CONST messageQueue_:P_structuredMessageQueue);
      DESTRUCTOR destroy; virtual;
      PROPERTY step[index:longint]: P_workflowStep read getStep;
      FUNCTION stepCount:longint;
      FUNCTION parseWorkflow(CONST data:T_arrayOfString):boolean;
      FUNCTION workflowText:T_arrayOfString;
      FUNCTION readFromFile(CONST fileName:string):boolean;
      PROCEDURE saveToFile(CONST fileName:string);
      PROCEDURE saveAsTodo(CONST savingToFile:string; CONST savingWithSizeLimit:longint);
      PROCEDURE appendSaveStep(CONST savingToFile:string; CONST savingWithSizeLimit:longint);
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
    if currentExecution.workflowState=ts_evaluating then afterAll;
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

PROCEDURE T_editorWorkflow.stepChanged(CONST index: longint);
  VAR i:longint;
  begin
    ensureStop;
    enterCriticalSection(contextCS);
    try
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
    for i:=0 to length(steps)-1 do begin
      stashId:=steps[i]^.operation^.readsStash;
      if stashId<>'' then begin
        writtenBeforeRead:=false;
        for j:=0 to i-1 do writtenBeforeRead:=writtenBeforeRead or (steps[j]^.operation^.writesStash=stashId);
        writtenBeforeRead:=false;
      end;
      if not(writtenBeforeRead) then begin
        messageQueue^.Post('Stash "'+stashId+'" is read before it is written',true,i);
        result:=false;
      end;
    end;
    result:=true;
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
