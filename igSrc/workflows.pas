UNIT workflows;
INTERFACE
USES myParams,mypics,myColors,sysutils,myTools,imageGeneration,ExtCtrls,mySys,FileUtil,Dialogs,
     pixMaps;
CONST MAX_HEIGHT_OR_WIDTH=9999;
TYPE
  T_imageManipulationCategory=(imc_generation,imc_imageAccess,imc_geometry,imc_colors,imc_combination,imc_statistic,imc_filter,imc_misc);
  T_imageManipulationType=(
                  imt_generateImage,
  {Image access:} imt_loadImage,imt_saveImage,imt_saveJpgWithSizeLimit, imt_stashImage, imt_unstashImage,
  {Geometry:}     imt_resize, imt_fit, imt_fill,
                  imt_crop, imt_zoom,
                  imt_flip, imt_flop, imt_rotLeft, imt_rotRight,
  {Combination:}  imt_addRGB,   imt_subtractRGB,   imt_multiplyRGB,   imt_divideRGB,   imt_screenRGB,   imt_maxOfRGB,   imt_minOfRGB,
                  imt_addHSV,   imt_subtractHSV,   imt_multiplyHSV,   imt_divideHSV,   imt_screenHSV,   imt_maxOfHSV,   imt_minOfHSV,
                  imt_addStash, imt_subtractStash, imt_multiplyStash, imt_divideStash, imt_screenStash, imt_maxOfStash, imt_minOfStash,
  {per pixel color op:} imt_setColor,imt_tint,imt_project,imt_limit,imt_limitLow,imt_grey,imt_sepia,
                        imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV,imt_unitChannelSum,
  {statistic color op:} imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey, imt_compress, imt_compressV, imt_compressSat,
                        imt_mono, imt_quantize,
                        imt_shine, imt_blur,
                        imt_lagrangeDiff, imt_radialBlur, imt_rotationalBlur, imt_blurWithStash,
                        imt_sharpen,imt_edges,imt_variance,
                        imt_mode,imt_median,imt_pseudomedian,
                        imt_sketch,imt_drip,imt_encircle,imt_encircleNeon,imt_gradient,imt_direction,imt_details,imt_nlm,imt_dropAlpha,imt_retainAlpha);
  T_workflowType=(wft_generative,wft_manipulative,wft_fixated,wft_halfFix,wft_empty_or_unknown);
CONST
  imageManipulationCategory:array[T_imageManipulationType] of T_imageManipulationCategory=(
    imc_generation,
    imc_imageAccess,imc_imageAccess,imc_imageAccess,imc_imageAccess,imc_imageAccess,
    imc_geometry,imc_geometry,imc_geometry,// imt_resize..imt_fill,
    imc_geometry,imc_geometry, // imt_crop, imt_zoom
    imc_geometry,imc_geometry,imc_geometry,imc_geometry,//imt_flip..imt_rotRight,
    imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors, //imt_addRGB..imt_minOfRGB,
    imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors, //imt_addHSV..imt_minOfHSV,
    imc_combination,imc_combination,imc_combination,imc_combination,imc_combination,imc_combination,imc_combination, //imt_addStash..imt_minOfStash,
    imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,//imt_setColor..imt_sepia,
    imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,//imt_invert..imt_unitChannelSum,
    imc_statistic,imc_statistic,imc_statistic,imc_statistic,imc_statistic,imc_statistic,imc_statistic,imc_statistic,//imt_normalizeFull..imt_quantize,
    imc_misc, //imt_shine,
    imc_filter,imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter,  // imt_blur..imt_pseudomedian,
    imc_misc, imc_misc, imc_misc,imc_misc, //imt_sketch,imt_drip,imt_encircle,imt_encircleNeon,
    imc_filter,imc_filter,imc_filter, //imt_gradient,imt_direction,imt_details
    imc_filter,imc_misc,imc_misc //imt_nlm, imt_[reatain/drop]Alpha
    );
  C_workflowTypeString:array[T_workflowType] of string=('generative','manipulative','fix','half-fix','empty or unknown');
  C_nullSourceOrTargetFileName='-';

TYPE

  P_imageManipulationStepToDo=^T_imageManipulationStepToDo;
  P_imageManipulationStep=^T_imageManipulationStep;
  P_imageManipulationWorkflow=^T_imageManipulationWorkflow;
  T_imageManipulationStep=object
    private
      imageManipulationType:T_imageManipulationType;
      valid,volatile:boolean;
      index,previewXRes,previewYRes:longint;
    public
      param:T_parameterValue;
      CONSTRUCTOR create(CONST command:ansistring);
      CONSTRUCTOR create(CONST typ:T_imageManipulationType; CONST param_:T_parameterValue);
      DESTRUCTOR destroy; virtual;
      PROCEDURE execute(CONST previewMode,retainStashesAfterLastUse:boolean; CONST inQueue:P_progressEstimatorQueue; VAR targetImage:T_rawImage);
      FUNCTION expectedOutputResolution(CONST inputResolution:T_imageDimensions):T_imageDimensions;
      FUNCTION isValid:boolean;
      FUNCTION toString(CONST forProgress:boolean=false):ansistring;
      FUNCTION toStringPart(CONST valueAndNotKey:boolean):ansistring;
      FUNCTION getTodo(CONST containedIn:P_imageManipulationWorkflow; CONST previewMode:boolean; CONST stepIndexForStoringIntermediate,maxXRes,maxYRes:longint):P_imageManipulationStepToDo;
      FUNCTION alterParameter(CONST newString:ansistring):boolean;
      FUNCTION descriptor:P_parameterDescription;
      FUNCTION isGenerationStep:boolean;
      FUNCTION isCropStep:boolean;
      FUNCTION isWritingStashAccess:boolean;
      FUNCTION isReadingStashAccess:boolean;
      FUNCTION hasComplexParameterDescription:boolean;
      FUNCTION getImageManipulationType:T_imageManipulationType;
  end;

  T_imageManipulationStepToDo=object(T_queueToDo)
    manipulationStep:P_imageManipulationStep;
    previewQuality:boolean;
    stepIndex:longint;
    containingWorkflow:P_imageManipulationWorkflow;
    CONSTRUCTOR create(CONST containedIn:P_imageManipulationWorkflow; CONST step:P_imageManipulationStep; CONST preview:boolean; CONST stepIndexForStoringIntermediate:longint=-1);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

  T_imageManipulationWorkflow=object
    private
      myFileName:ansistring;
      imageStash:array of record
                   img:P_rawImage;
                   id:string;
                   lastRead,
                   firstWritten,
                   lastWritten:longint;
                 end;

      intermediate:array of P_rawImage;
      intermediatesAreInPreviewQuality:boolean;
      PROCEDURE raiseError(CONST message:ansistring);
      PROCEDURE clearStash;
      PROCEDURE clearIntermediate;
      PROCEDURE storeIntermediate(CONST index:longint);
      PROCEDURE enqueueAllAndStore(CONST sizeLimit:longint; CONST targetName:ansistring);
      PROCEDURE updateStashMetaData;
      FUNCTION stashIndexForStep(CONST step:T_imageManipulationStep; CONST allowCreation:boolean):longint;
      PROCEDURE stashImage(CONST step:T_imageManipulationStep; VAR source:T_rawImage);
      PROCEDURE unstashImage(CONST step:T_imageManipulationStep; CONST retainStashesAfterLastUse:boolean; VAR target:T_rawImage);
      FUNCTION getStashedImage(CONST step:T_imageManipulationStep; CONST retainStashesAfterLastUse:boolean; OUT disposeAfterUse:boolean):P_rawImage;
    public
      progressQueue:T_progressEstimatorQueue;
      workflowImage:T_rawImage;
      step:array of T_imageManipulationStep;
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE execute(CONST previewMode,doStoreIntermediate,skipFit:boolean; CONST xRes,yRes,maxXRes,maxYRes:longint);
      PROCEDURE executeForTarget(CONST xRes,yRes,sizeLimit:longint; CONST targetName:ansistring);
      PROCEDURE executeForTarget(CONST inputImageFileName:ansistring; CONST sizeLimit:longint; CONST targetName:ansistring);
      PROCEDURE storeToDo(CONST initialStep:T_imageManipulationStep; CONST sizeLimit:longint; CONST targetName:ansistring);
      PROCEDURE storeToDo(CONST xRes,yRes,sizeLimit:longint; CONST targetName:ansistring);
      PROCEDURE storeToDo(CONST inputImageFileName:ansistring; CONST sizeLimit:longint; CONST targetName:ansistring);
      FUNCTION findAndExecuteToDo:boolean;
      PROCEDURE findAndExecuteToDo_DONE;
      FUNCTION isTempTodo:boolean;

      FUNCTION renderIntermediate(CONST index:longint; VAR target:TImage):boolean;
      FUNCTION renderIntermediate(CONST index:longint; VAR target:T_rawImage):boolean;

      PROCEDURE clear;
      FUNCTION addStep(CONST command:ansistring):boolean;
      PROCEDURE remStep(CONST index:longint);
      FUNCTION stepCount:longint;
      PROCEDURE swapStepDown(CONST lowerIndex:longint);
      PROCEDURE stepChanged(CONST index:longint);

      FUNCTION loadFromFile(CONST fileNameUtf8:string):boolean;
      PROCEDURE saveToFile(CONST fileNameUtf8:string);

      FUNCTION associatedFile:string;
      FUNCTION associatedDir:string;
      FUNCTION proposedImageFileName(CONST resString:ansistring):string;
      FUNCTION workflowType:T_workflowType;
  end;

VAR workflow:T_imageManipulationWorkflow;
    stepParamDescription:array[T_imageManipulationType] of P_parameterDescription;
    inputImage   :P_rawImage;

FUNCTION canParseResolution(CONST s:string; OUT x,y:longint):boolean;
FUNCTION canParseSizeLimit(CONST s:string; OUT size:longint):boolean;
IMPLEMENTATION
PROCEDURE initParameterDescriptions;
  VAR imt:T_imageManipulationType;
      initFailed:boolean=false;
  begin
    for imt:=low(T_imageManipulationType) to high(T_imageManipulationType) do stepParamDescription[imt]:=nil;
    stepParamDescription[imt_generateImage]:=newParameterDescription('',pt_string); //pro forma
    stepParamDescription[imt_loadImage]:=newParameterDescription('load',pt_fileName);
    stepParamDescription[imt_saveImage]:=newParameterDescription('save',pt_fileName);
    stepParamDescription[imt_saveJpgWithSizeLimit]:=newParameterDescription('save',pt_jpgNameWithSize);
    stepParamDescription[imt_stashImage]:=newParameterDescription('stash',pt_string, 0)^.setDefaultValue('0');
    stepParamDescription[imt_unstashImage]:=newParameterDescription('unstash',pt_string, 0)^.setDefaultValue('0');
    stepParamDescription[imt_resize]:=newParameterDescription('resize',pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
      .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
      .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
      .setDefaultValue('100x100');
    stepParamDescription[imt_fit]:=newParameterDescription('fit',pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
      .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
      .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
      .setDefaultValue('100x100');
    stepParamDescription[imt_fill]:=newParameterDescription('fill',pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
      .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
      .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
      .setDefaultValue('100x100');
    stepParamDescription[imt_crop]:=newParameterDescription('crop', pt_4floats)^
      .addChildParameterDescription(spa_f0,'relative x0',pt_float)^
      .addChildParameterDescription(spa_f1,'relative x1',pt_float)^
      .addChildParameterDescription(spa_f2,'relative y0',pt_float)^
      .addChildParameterDescription(spa_f3,'relative y1',pt_float)^
      .setDefaultValue('0:1x0:1');
    stepParamDescription[imt_zoom]:=newParameterDescription('zoom', pt_float)^
      .setDefaultValue('0.5');
    stepParamDescription[imt_flip                ]:=newParameterDescription('flip',        pt_none);
    stepParamDescription[imt_flop                ]:=newParameterDescription('flop',        pt_none);
    stepParamDescription[imt_rotLeft             ]:=newParameterDescription('rotL',        pt_none);
    stepParamDescription[imt_rotRight            ]:=newParameterDescription('rotR',        pt_none);
    stepParamDescription[imt_addRGB              ]:=newParameterDescription('+RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_subtractRGB         ]:=newParameterDescription('-RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_multiplyRGB         ]:=newParameterDescription('*RGB',        pt_color)^.setDefaultValue('1')^.addRGBChildParameters;
    stepParamDescription[imt_divideRGB           ]:=newParameterDescription('/RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_screenRGB           ]:=newParameterDescription('screenRGB',   pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_maxOfRGB            ]:=newParameterDescription('maxRGB',      pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_minOfRGB            ]:=newParameterDescription('minRGB',      pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_addHSV              ]:=newParameterDescription('+HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
    stepParamDescription[imt_subtractHSV         ]:=newParameterDescription('-HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
    stepParamDescription[imt_multiplyHSV         ]:=newParameterDescription('*HSV',        pt_3floats)^.setDefaultValue('1,1,1')^.addHSVChildParameters;
    stepParamDescription[imt_divideHSV           ]:=newParameterDescription('/HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
    stepParamDescription[imt_screenHSV           ]:=newParameterDescription('screenHSV',   pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
    stepParamDescription[imt_maxOfHSV            ]:=newParameterDescription('maxHSV',      pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
    stepParamDescription[imt_minOfHSV            ]:=newParameterDescription('minHSV',      pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
    stepParamDescription[imt_addStash            ]:=newParameterDescription('+stash',      pt_string, 0)^.setDefaultValue('0');
    stepParamDescription[imt_subtractStash       ]:=newParameterDescription('-stash',      pt_string, 0)^.setDefaultValue('0');
    stepParamDescription[imt_multiplyStash       ]:=newParameterDescription('*stash',      pt_string, 0)^.setDefaultValue('0');
    stepParamDescription[imt_divideStash         ]:=newParameterDescription('/stash',      pt_string, 0)^.setDefaultValue('0');
    stepParamDescription[imt_screenStash         ]:=newParameterDescription('screenStash', pt_string, 0)^.setDefaultValue('0');
    stepParamDescription[imt_maxOfStash          ]:=newParameterDescription('maxStash',    pt_string, 0)^.setDefaultValue('0');
    stepParamDescription[imt_minOfStash          ]:=newParameterDescription('minStash',    pt_string, 0)^.setDefaultValue('0');
    stepParamDescription[imt_setColor            ]:=newParameterDescription('setRGB',      pt_color)^.setDefaultValue('0');
    stepParamDescription[imt_tint                ]:=newParameterDescription('tint',        pt_float)^.setDefaultValue('0');
    stepParamDescription[imt_project             ]:=newParameterDescription('project',     pt_none);
    stepParamDescription[imt_limit               ]:=newParameterDescription('limit',       pt_none);
    stepParamDescription[imt_limitLow            ]:=newParameterDescription('limitLow',    pt_none);
    stepParamDescription[imt_grey                ]:=newParameterDescription('grey',        pt_none);
    stepParamDescription[imt_sepia               ]:=newParameterDescription('sepia',       pt_none);
    stepParamDescription[imt_invert              ]:=newParameterDescription('invert',      pt_none);
    stepParamDescription[imt_abs                 ]:=newParameterDescription('abs',         pt_none);
    stepParamDescription[imt_gamma               ]:=newParameterDescription('gamma',       pt_float,   1E-3)^.setDefaultValue('1.3');
    stepParamDescription[imt_gammaRGB            ]:=newParameterDescription('gammaRGB',    pt_3floats, 1E-3)^.setDefaultValue('1.2,1.3,1.4')^.addRGBChildParameters;
    stepParamDescription[imt_gammaHSV            ]:=newParameterDescription('gammaHSV',    pt_3floats, 1E-3)^.setDefaultValue('1.2,1.3,1.4');
    stepParamDescription[imt_unitChannelSum      ]:=newParameterDescription('unitChannelSum',pt_none);
    stepParamDescription[imt_normalizeFull       ]:=newParameterDescription('normalize',   pt_none);
    stepParamDescription[imt_normalizeValue      ]:=newParameterDescription('normalizeV',  pt_none);
    stepParamDescription[imt_normalizeGrey       ]:=newParameterDescription('normalizeG',  pt_none);
    stepParamDescription[imt_compress            ]:=newParameterDescription('compress',pt_float,0)^.setDefaultValue('20');
    stepParamDescription[imt_compressV           ]:=newParameterDescription('compress V',pt_float,0)^.setDefaultValue('20');
    stepParamDescription[imt_compressSat         ]:=newParameterDescription('compress saturation',pt_float,0)^.setDefaultValue('20');
    stepParamDescription[imt_mono                ]:=newParameterDescription('mono',        pt_integer)^.setDefaultValue('10')^.addChildParameterDescription(spa_i0,'Color count',pt_integer,1,255);
    stepParamDescription[imt_quantize            ]:=newParameterDescription('quantize',    pt_integer)^.setDefaultValue('16')^.addChildParameterDescription(spa_i0,'Color count',pt_integer,2,255);
    stepParamDescription[imt_shine               ]:=newParameterDescription('shine',       pt_none);
    stepParamDescription[imt_blur                ]:=newParameterDescription('blur',        pt_floatOr2Floats,0)^.setDefaultValue('0.2')^.addChildParameterDescription(spa_f0,'x',pt_float)^.addChildParameterDescription(spa_f1,'y',pt_float);
    stepParamDescription[imt_lagrangeDiff        ]:=newParameterDescription('lagrangeDiff',pt_2floats,0)^.setDefaultValue('0.1,0.1')^.addChildParameterDescription(spa_f0,'scanScale',pt_float,0,1)^.addChildParameterDescription(spa_f1,'blurScale',pt_float,0,1);
    stepParamDescription[imt_radialBlur          ]:=newParameterDescription('radialBlur'  ,pt_3floats)^.setDefaultValue('1,0,0');
    stepParamDescription[imt_rotationalBlur      ]:=newParameterDescription('rotationalBlur',pt_3floats)^.setDefaultValue('1,0,0');
    stepParamDescription[imt_blurWithStash       ]:=newParameterDescription('blurWithStash',pt_string,0)^.setDefaultValue('0');
    stepParamDescription[imt_sharpen             ]:=newParameterDescription('sharpen'     ,pt_2floats,0)^
    .addChildParameterDescription(spa_f0,'scale',pt_float,0,1)^
    .addChildParameterDescription(spa_f1,'amount',pt_float,0)^
    .setDefaultValue('0.1,0.5');
    stepParamDescription[imt_edges               ]:=newParameterDescription('edges' ,pt_none);
    stepParamDescription[imt_variance]:=newParameterDescription('variance',pt_float,0)^.setDefaultValue('0.05')^.addChildParameterDescription(spa_f0,'scale',pt_float);
    stepParamDescription[imt_median]:=newParameterDescription('median',pt_float,0)^.setDefaultValue('0.05')^.addChildParameterDescription(spa_f0,'scale',pt_float);
    stepParamDescription[imt_pseudomedian]:=newParameterDescription('pseudoMedian',pt_2floats,0)^
      .addChildParameterDescription(spa_f0,'rel. sigma',pt_float,0)^
      .addChildParameterDescription(spa_f1,'param',pt_float)^
      .setDefaultValue('0.1,1');
    stepParamDescription[imt_mode]:=newParameterDescription('mode',pt_float,0)^.setDefaultValue('0.05')^.addChildParameterDescription(spa_f0,'scale',pt_float);
    stepParamDescription[imt_sketch]:=newParameterDescription('sketch',pt_4floats)^
      .setDefaultValue('1,0.1,0.8,0.2')^
      .addChildParameterDescription(spa_f0,'cover'          ,pt_float,0)^
      .addChildParameterDescription(spa_f1,'direction sigma',pt_float,0)^
      .addChildParameterDescription(spa_f2,'density'        ,pt_float)^
      .addChildParameterDescription(spa_f3,'tolerance'      ,pt_float,0);
    stepParamDescription[imt_drip]:=newParameterDescription('drip',pt_2floats,0,1)^
      .setDefaultValue('0.1,0.01')^
      .addChildParameterDescription(spa_f0,'diffusiveness',pt_float,0,1)^
      .addChildParameterDescription(spa_f1,'range' ,pt_float,0,1);
    stepParamDescription[imt_encircle]:=newParameterDescription('encircle',pt_1I2F,0)^
      .setDefaultValue('2000,0.5,0.2')^
      .addChildParameterDescription(spa_i0,'circle count',pt_integer,1,100000)^
      .addChildParameterDescription(spa_f1,'opacity' ,pt_float,0,1)^
      .addChildParameterDescription(spa_f2,'circle size' ,pt_float,0);
    stepParamDescription[imt_encircleNeon]:=newParameterDescription('encircleNeon',pt_1I2F,0)^
      .setDefaultValue('2000,0.5,0.2')^
      .addChildParameterDescription(spa_i0,'circle count',pt_integer,1,100000)^
      .addChildParameterDescription(spa_f1,'opacity' ,pt_float,0,1)^
      .addChildParameterDescription(spa_f2,'circle size' ,pt_float,0);
    stepParamDescription[imt_gradient]:=newParameterDescription('gradient',pt_float,0)^.setDefaultValue('0.1');
    stepParamDescription[imt_direction]:=newParameterDescription('direction',pt_float,0)^.setDefaultValue('0.1');
    stepParamDescription[imt_details]:=newParameterDescription('details',pt_float,0)^.setDefaultValue('0.1');
    stepParamDescription[imt_nlm]:=newParameterDescription('nlm',pt_1I1F,0)^
      .setDefaultValue('3,0.5')^
      .addChildParameterDescription(spa_i0,'scan radius (pixels)',pt_integer,1)^
      .addChildParameterDescription(spa_f1,'sigma',pt_float,0,1);
    stepParamDescription[imt_retainAlpha]:=newParameterDescription('retainAlpha',pt_color);
    stepParamDescription[imt_dropAlpha]:=newParameterDescription('dropAlpha',pt_color);
    for imt:=low(T_imageManipulationType) to high(T_imageManipulationType) do if stepParamDescription[imt]=nil then begin
      writeln(stdErr,'Missing initialization of parameterDescription[',imt,']');
      initFailed:=true;
    end;
    if initFailed then halt;
  end;

PROCEDURE cleanupParameterDescriptions;
  VAR imt:T_imageManipulationType;
  begin
    for imt:=low(T_imageManipulationType) to high(T_imageManipulationType) do dispose(stepParamDescription[imt],destroy);
  end;

FUNCTION canParseResolution(CONST s:string; OUT x,y:longint):boolean;
  VAR p:T_parameterValue;
  begin
    p.createToParse(stepParamDescription[imt_resize],s);
    x:=p.i0;
    y:=p.i1;
    result:=p.isValid;
  end;

FUNCTION canParseSizeLimit(CONST s:string; OUT size:longint):boolean;
  VAR p:T_parameterValue;
  begin
    p.createToParse(stepParamDescription[imt_saveJpgWithSizeLimit],'dummy.jpg@'+s);
    size:=p.i0;
    result:=p.isValid;
  end;

CONSTRUCTOR T_imageManipulationStepToDo.create(CONST containedIn:P_imageManipulationWorkflow; CONST step:P_imageManipulationStep; CONST preview:boolean; CONST stepIndexForStoringIntermediate:longint=-1);
  begin
    inherited create;
    manipulationStep:=step;
    previewQuality:=preview;
    stepIndex:=stepIndexForStoringIntermediate;
    containingWorkflow:=containedIn;
  end;

DESTRUCTOR T_imageManipulationStepToDo.destroy;
  begin
  end;

PROCEDURE T_imageManipulationStepToDo.execute;
  begin
    containingWorkflow^.progressQueue.logStepMessage(manipulationStep^.toString(true));
    manipulationStep^.execute(previewQuality,stepIndex>=0,@containingWorkflow^.progressQueue,containingWorkflow^.workflowImage);
    if stepIndex>=0 then workflow.storeIntermediate(stepIndex);
    if manipulationStep^.volatile then dispose(manipulationStep,destroy);
    containingWorkflow^.progressQueue.logStepDone;
  end;

CONSTRUCTOR T_imageManipulationStep.create(CONST command: ansistring);
  VAR imt:T_imageManipulationType;
  begin
    index:=-1;
    volatile:=false;
    for imt:=low(T_imageManipulationType) to high(T_imageManipulationType) do if (imt<>imt_generateImage) then begin
      param.createToParse(stepParamDescription[imt],command,tsm_withNiceParameterName);
      if param.isValid then begin
        valid:=true;
        imageManipulationType:=imt;
        exit;
      end;
    end;
    if isPlausibleSpecification(command,false)>=0 then begin
      imageManipulationType:=imt_generateImage;
      param.createFromValue(stepParamDescription[imt_generateImage],command);
      valid:=true;
      exit;
    end;
    valid:=false;
  end;

CONSTRUCTOR T_imageManipulationStep.create(CONST typ: T_imageManipulationType;
  CONST param_: T_parameterValue);
  begin
    index:=-1;
    volatile:=false;
    imageManipulationType:=typ;
    param:=param_;
    valid:=param.isValid;
  end;

DESTRUCTOR T_imageManipulationStep.destroy;
  begin
  end;

PROCEDURE T_imageManipulationStep.execute(CONST previewMode,retainStashesAfterLastUse: boolean; CONST inQueue:P_progressEstimatorQueue; VAR targetImage:T_rawImage);

  FUNCTION plausibleResolution:boolean;
    begin
      if (param.i0>0) and (param.i0<10000) and (param.i1>0) and (param.i1<10000) then result:=true
      else begin
        result:=false;
        workflow.raiseError('Invalid resolution; Both values must be in range 1..'+intToStr(MAX_HEIGHT_OR_WIDTH));
      end;
    end;

  FUNCTION rgbMult  (CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin for i in RGB_CHANNELS do result[i]:=a[i]*b[i]; end;
  FUNCTION rgbMax   (CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin for i in RGB_CHANNELS do if a[i]>b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
  FUNCTION rgbMin   (CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin for i in RGB_CHANNELS do if a[i]<b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
  PROCEDURE combine;
    FUNCTION rgbDiv   (CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin for i in RGB_CHANNELS do result[i]:=a[i]/b[i]; end;
    FUNCTION rgbScreen(CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin for i in RGB_CHANNELS do result[i]:=1-(1-a[i])*(1-b[i]); end;
    CONST RGB_OF:array[hc_hue..hc_value] of T_colorChannel=(cc_red,cc_green,cc_blue);
    FUNCTION hsvPlus  (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin for i in HSV_CHANNELS do result[i]:=a[i]+b[RGB_OF[i]]; end;
    FUNCTION hsvMinus (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin for i in HSV_CHANNELS do result[i]:=a[i]-b[RGB_OF[i]]; end;
    FUNCTION hsvMult  (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin for i in HSV_CHANNELS do result[i]:=a[i]*b[RGB_OF[i]]; end;
    FUNCTION hsvDiv   (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin for i in HSV_CHANNELS do result[i]:=a[i]/b[RGB_OF[i]]; end;
    FUNCTION hsvScreen(CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin for i in HSV_CHANNELS do result[i]:=1-(1-a[i])*(1-b[RGB_OF[i]]); end;
    FUNCTION hsvMax   (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin for i in HSV_CHANNELS do if a[i]>b[RGB_OF[i]] then result[i]:=a[i] else result[i]:=b[RGB_OF[i]]; end;
    FUNCTION hsvMin   (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin for i in HSV_CHANNELS do if a[i]<b[RGB_OF[i]] then result[i]:=a[i] else result[i]:=b[RGB_OF[i]]; end;
    VAR k:longint;
        other:P_rawImage=nil;
        rawPixels,otherPixels:P_floatColor;
        c1:T_rgbFloatColor;
        disposeOther:boolean=false;
    begin
      rawPixels:=targetImage.rawData;
      case imageManipulationType of
        imt_addStash..imt_minOfStash,imt_blurWithStash:
          begin
            other:=workflow.getStashedImage(self,retainStashesAfterLastUse,disposeOther);
            if other=nil then exit;
          end;
        imt_addRGB..imt_minOfRGB,imt_addHSV..imt_minOfHSV: begin
          c1:=param.color;
          case imageManipulationType of
            imt_addRGB      : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=          rawPixels[k]+c1;
            imt_subtractRGB : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=          rawPixels[k]-c1;
            imt_multiplyRGB : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbMult  (rawPixels[k],c1);
            imt_divideRGB   : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbDiv   (rawPixels[k],c1);
            imt_screenRGB   : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbScreen(rawPixels[k],c1);
            imt_maxOfRGB    : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbMax   (rawPixels[k],c1);
            imt_minOfRGB    : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbMin   (rawPixels[k],c1);
            imt_addHSV      : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=hsvPlus  (rawPixels[k],c1);
            imt_subtractHSV : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=hsvMinus (rawPixels[k],c1);
            imt_multiplyHSV : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=hsvMult  (rawPixels[k],c1);
            imt_divideHSV   : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=hsvDiv   (rawPixels[k],c1);
            imt_screenHSV   : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=hsvScreen(rawPixels[k],c1);
            imt_maxOfHSV    : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=hsvMax   (rawPixels[k],c1);
            imt_minOfHSV    : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=hsvMin   (rawPixels[k],c1);
          end;
          exit;
        end;
      end;
      otherPixels:=other^.rawData;
      case imageManipulationType of
        imt_addStash     : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=          rawPixels[k]+otherPixels[k];
        imt_subtractStash: for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=          rawPixels[k]-otherPixels[k];
        imt_multiplyStash: for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbMult  (rawPixels[k],otherPixels[k]);
        imt_divideStash  : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbDiv   (rawPixels[k],otherPixels[k]);
        imt_screenStash  : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbScreen(rawPixels[k],otherPixels[k]);
        imt_maxOfStash   : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbMax   (rawPixels[k],otherPixels[k]);
        imt_minOfStash   : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbMin   (rawPixels[k],otherPixels[k]);
        imt_blurWithStash: targetImage.blurWith(other^);
      end;
      if disposeOther and (other<>nil) then dispose(other,destroy);
    end;

  PROCEDURE colorOp;
    VAR k:longint;
        rawPixels:P_floatColor;
    FUNCTION unitChannelSum(CONST col:T_rgbFloatColor):T_rgbFloatColor;
      VAR sum:double=0;
          c:T_colorChannel;
      begin
        for c in RGB_CHANNELS do sum:=sum+col[c];
        if sum=0 then begin
          for c in RGB_CHANNELS do result[c]:=1/3;
        end else begin
          sum:=1/sum;
          for c in RGB_CHANNELS do result[c]:=sum*col[c];
        end;
      end;

    begin
      rawPixels:=targetImage.rawData;
      case imageManipulationType of
        imt_setColor: for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=param.color;
        imt_tint:     for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=tint(rawPixels[k],param.f0);
        imt_project:  for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=projectedColor(rawPixels[k]);
        imt_limit:    for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbMax(BLACK,rgbMin(WHITE,rawPixels[k]));
        imt_limitLow: for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=rgbMax(BLACK,             rawPixels[k] );
        imt_grey    : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=subjectiveGrey(rawPixels[k]);
        imt_sepia   : for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=         sepia(rawPixels[k]);
        imt_invert:   for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=        invert(rawPixels[k]);
        imt_abs:      for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=        absCol(rawPixels[k]);
        imt_gamma:    for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=         gamma(rawPixels[k],param.f0,param.f0,param.f0);
        imt_gammaRGB: for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=         gamma(rawPixels[k],param.f0,param.f1,param.f2);
        imt_gammaHSV: for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=      gammaHSV(rawPixels[k],param.f0,param.f1,param.f2);
        imt_unitChannelSum: for k:=0 to targetImage.pixelCount-1 do rawPixels[k]:=unitChannelSum(rawPixels[k]);
      end;
    end;

  PROCEDURE statisticColorOp;
    VAR compoundHistogram:T_compoundHistogram;
        greyHist:T_histogram;
        p0,p1:T_rgbFloatColor;
        i:longint;
        k:longint=0;
        tempHsv:T_hsvColor;
        raw:P_floatColor;

    FUNCTION normValue(CONST c:T_hsvColor):T_hsvColor;
      begin
        result:=c;
        result[hc_value]:=(result[hc_value]-p0[cc_red])*p1[cc_red];
      end;

    FUNCTION measure(CONST a,b:single):single;
      CONST a0=1/0.998;
            b0= -0.001;
      begin result:=sqr(a0-a)/3+(a0-a+b0-b)*(b0-b); end;

    FUNCTION measure(CONST a,b:T_rgbFloatColor):single;
      begin
        result:=(measure(a[cc_red  ],b[cc_red  ])*SUBJECTIVE_GREY_RED_WEIGHT+
                 measure(a[cc_green],b[cc_green])*SUBJECTIVE_GREY_GREEN_WEIGHT+
                 measure(a[cc_blue ],b[cc_blue ])*SUBJECTIVE_GREY_BLUE_WEIGHT);
      end;

    begin
      raw:=targetImage.rawData;
      case imageManipulationType of
        imt_normalizeFull: while k<4 do begin
          compoundHistogram:=targetImage.histogram;
          compoundHistogram.R.getNormalizationParams(p0[cc_red  ],p1[cc_red  ]);
          compoundHistogram.G.getNormalizationParams(p0[cc_green],p1[cc_green]);
          compoundHistogram.B.getNormalizationParams(p0[cc_blue ],p1[cc_blue ]);
          {$ifdef DEBUG} writeln('Normalization with parameters ',p0[cc_red],' ',p1[cc_red],'; measure:',measure(p0,p1)); {$endif}
          for i:=0 to targetImage.pixelCount-1 do raw[i]:=rgbMult(raw[i]-p0,p1);
          if (compoundHistogram.mightHaveOutOfBoundsValues or (measure(p0,p1)>1)) and not(inQueue^.cancellationRequested) then inc(k) else k:=4;
          compoundHistogram.destroy;
        end;
        imt_normalizeValue: while k<4 do begin
          compoundHistogram:=targetImage.histogramHSV;
          compoundHistogram.B.getNormalizationParams(p0[cc_red],p1[cc_red]);
          for i:=0 to targetImage.pixelCount-1 do raw[i]:=normValue(raw[i]);
          if (compoundHistogram.B.mightHaveOutOfBoundsValues or (measure(p0[cc_red],p1[cc_red])>1)) and not(inQueue^.cancellationRequested) then inc(k) else k:=4;
          compoundHistogram.destroy;
        end;
        imt_normalizeGrey: while k<4 do begin
          compoundHistogram:=targetImage.histogram;
          greyHist:=compoundHistogram.subjectiveGreyHistogram;
          greyHist.getNormalizationParams(p0[cc_red],p1[cc_red]);
          p0:=WHITE*p0[cc_red];
          for i:=0 to targetImage.pixelCount-1 do raw[i]:=(raw[i]-p0)*p1[cc_red];
          if (greyHist.mightHaveOutOfBoundsValues or (measure(p0[cc_red],p1[cc_red])>1)) and not(inQueue^.cancellationRequested) then inc(k) else k:=4;
          greyHist.destroy;
          compoundHistogram.destroy;
        end;
        imt_compress: begin
          compoundHistogram:=targetImage.histogram;
          greyHist:=compoundHistogram.sumHistorgram;
          greyHist.smoothen(param.f0);
          for i:=0 to targetImage.pixelCount-1 do raw[i]:=greyHist.lookup(raw[i]);
          greyHist.destroy;
          compoundHistogram.destroy;
        end;
        imt_compressV: begin
          compoundHistogram:=targetImage.histogramHSV;
          greyHist:=compoundHistogram.B;
          greyHist.smoothen(param.f0);
          for i:=0 to targetImage.pixelCount-1 do begin
            tempHsv:=raw[i];
            tempHsv[hc_value]:=greyHist.lookup(tempHsv[hc_value]);
            raw[i]:=tempHsv;
          end;
          compoundHistogram.destroy;
        end;
        imt_compressSat: begin
          compoundHistogram:=targetImage.histogramHSV;
          greyHist:=compoundHistogram.G;
          greyHist.smoothen(param.f0);
          for i:=0 to targetImage.pixelCount-1 do begin
            tempHsv:=raw[i];
            tempHsv[hc_saturation]:=greyHist.lookup(tempHsv[hc_saturation]);
            raw[i]:=tempHsv;
          end;
          compoundHistogram.destroy;
        end;
      end;
    end;

  PROCEDURE monochrome;
    VAR i:longint;
        l:T_colorChannel;
        k:longint=0;
        cSum:T_rgbFloatColor=(0,0,0);
        c:T_rgbFloatColor;
        g,invG:double;
        raw:P_floatColor;
    begin
      raw:=targetImage.rawData;
      for i:=0 to targetImage.pixelCount-1 do begin
        c:=raw[i];
        g:=greyLevel(c);
        if g>1E-3 then begin
          invG:=1/g;
          for l in RGB_CHANNELS do cSum[l]:=cSum[l]+c[l]*invG;
          inc(k);
        end;
        c[cc_red]:=g;
        raw[i]:=c;
      end;
      invG:=1/k;
      for l in RGB_CHANNELS do cSum[l]:=cSum[l]*invG;
      for i:=0 to targetImage.pixelCount-1 do begin
        c:=raw[i];
        g:=round(c[cc_red]*param.i0)/param.i0;
        for l in RGB_CHANNELS do c[l]:=g*cSum[l];
        raw[i]:=c;
      end;
    end;

  PROCEDURE redefine(newImage:T_rawImage);
    begin
      targetImage.copyFromPixMap(newImage);
      newImage.destroy;
    end;

  PROCEDURE doLoad;
    begin
      targetImage.loadFromFile(param.fileName);
      if (previewMode or retainStashesAfterLastUse) then
        targetImage.resize(previewXRes,previewYRes,res_fit);
    end;

  PROCEDURE doDetails;
    VAR temp:T_rawImage;
        i:longint;
    begin
      temp.create(targetImage);
      temp.blur(param.f0,param.f0);
      for i:=0 to targetImage.pixelCount-1 do targetImage.rawData[i]:=targetImage.rawData[i]-temp.rawData[i];
      temp.destroy;
    end;

  begin
    {$ifdef DEBUG} writeln('Step #',index,': ',toString(),' (@',targetImage.dimensions.width,'x',targetImage.dimensions.height,')'); {$endif}

    case imageManipulationType of
      imt_generateImage: prepareImage(param.fileName,@targetImage,previewMode);
      imt_loadImage: doLoad;
      imt_saveImage: targetImage.saveToFile(expandFileName(param.fileName));
      imt_saveJpgWithSizeLimit: targetImage.saveJpgWithSizeLimit(expandFileName(param.fileName),param.i0);
      imt_stashImage: workflow.stashImage(self,targetImage);
      imt_unstashImage: workflow.unstashImage(self,retainStashesAfterLastUse,targetImage);
      imt_resize: if plausibleResolution then begin
                    if (index=0) then targetImage.resize(param.i0,param.i1,res_dataResize)
                                 else targetImage.resize(param.i0,param.i1,res_exact);
                  end;
      imt_fit   : if plausibleResolution then targetImage.resize(param.i0,param.i1,res_fit);
      imt_fill  : if plausibleResolution then targetImage.resize(param.i0,param.i1,res_cropToFill);
      imt_crop  : targetImage.crop(param.f0,param.f1,param.f2,param.f3);
      imt_zoom  : targetImage.zoom(param.f0);
      imt_flip  : targetImage.flip;
      imt_flop  : targetImage.flop;
      imt_rotLeft : targetImage.rotLeft;
      imt_rotRight: targetImage.rotRight;
      imt_addRGB..imt_minOfStash,imt_blurWithStash: combine;
      imt_setColor, imt_tint, imt_project, imt_limit,imt_limitLow,imt_grey,imt_sepia,imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV,imt_unitChannelSum: colorOp;
      imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey,imt_compress,imt_compressV,imt_compressSat:statisticColorOp;
      imt_mono: monochrome;
      imt_quantize: targetImage.quantize(param.i0);
      imt_shine: targetImage.shine;
      imt_blur: targetImage.blur(param.f0,param.f1);
      imt_lagrangeDiff: targetImage.lagrangeDiffusion(param.f0,param.f1);
      imt_radialBlur: targetImage.radialBlur(param.f0,param.f1,param.f2);
      imt_rotationalBlur: targetImage.rotationalBlur(param.f0,param.f1,param.f2);
      imt_sharpen: targetImage.sharpen(param.f0,param.f1);
      imt_edges: targetImage.prewittEdges;
      imt_variance: targetImage.variance(param.f0);
      imt_median: targetImage.medianFilter(param.f0);
      imt_pseudomedian: targetImage.myFilter(param.f0,param.f1);
      imt_mode: targetImage.modalFilter(param.f0);
      imt_sketch: targetImage.sketch(param.f0,param.f1,param.f2,param.f3);
      imt_drip: targetImage.drip(param.f0,param.f1);
      imt_encircle: targetImage.encircle(param.i0,WHITE,param.f1,param.f2,inQueue);
      imt_encircleNeon: targetImage.encircle(param.i0,BLACK,param.f1,param.f2,inQueue);
      imt_direction: redefine(targetImage.directionMap(param.f0));
      imt_details: doDetails;
      imt_nlm: targetImage.nlmFilter(param.i0,param.f1);
      imt_retainAlpha: redefine(targetImage.rgbaSplit(param.color));
      imt_dropAlpha: targetImage.rgbaSplit(param.color).destroy;
    end;
  end;

FUNCTION T_imageManipulationStep.expectedOutputResolution(CONST inputResolution:T_imageDimensions):T_imageDimensions;
  begin
    case imageManipulationType of
      imt_loadImage: begin result.width:=-1; result.height:=-1; end;
      imt_resize: result:=resize(inputResolution,param.i0,param.i1,res_exact);
      imt_fit   : result:=resize(inputResolution,param.i0,param.i1,res_fit);
      imt_fill  : result:=resize(inputResolution,param.i0,param.i1,res_cropToFill);
      imt_crop  : result:=crop(inputResolution,param.f0,param.f1,param.f2,param.f3);
      imt_rotLeft,
      imt_rotRight: result:=transpose(inputResolution);
      else result:=inputResolution;
    end;
  end;

FUNCTION T_imageManipulationStep.isValid: boolean;
  begin
    result:=valid;
  end;

FUNCTION T_imageManipulationStep.toString(CONST forProgress: boolean): ansistring;
  begin
    if imageManipulationType=imt_generateImage
    then result:=param.toString(tsm_withoutParameterName)
    else result:=param.toString(tsm_withNiceParameterName);
    if forProgress and (length(result)>30) then result:=copy(result,1,27)+'...';
  end;

FUNCTION T_imageManipulationStep.toStringPart(CONST valueAndNotKey: boolean
  ): ansistring;
  begin
    if valueAndNotKey then result:=param.toString(tsm_withoutParameterName)
    else if imageManipulationType=imt_generateImage
    then result:='<GENERATE>'
    else result:=param.toString(tsm_parameterNameOnly);
  end;

FUNCTION T_imageManipulationStep.getTodo(CONST containedIn:P_imageManipulationWorkflow; CONST previewMode: boolean; CONST stepIndexForStoringIntermediate, maxXRes, maxYRes: longint): P_imageManipulationStepToDo;
  VAR todoParam:T_parameterValue;
      todoStep:P_imageManipulationStep;
  begin
    previewXRes:=maxXRes;
    previewYRes:=maxYRes;
    if (imageManipulationType in [imt_fit,imt_resize,imt_fill]) and ((param.i0>=maxXRes) or (param.i0>=maxYRes))
       and ((maxXRes<>MAX_HEIGHT_OR_WIDTH) or (maxYRes<>MAX_HEIGHT_OR_WIDTH)) then begin
      todoParam.createFromValue(stepParamDescription[imageManipulationType],
                                getFittingRectangle(maxXRes,maxYRes,param.i0/param.i1).Right,
                                getFittingRectangle(maxXRes,maxYRes,param.i0/param.i1).Bottom);
      new(todoStep,create(imageManipulationType,todoParam));
      todoStep^.volatile:=true;
      result:=todoStep^.getTodo(containedIn,previewMode,stepIndexForStoringIntermediate,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH);
    end else new(result,create(containedIn,@self,previewMode, stepIndexForStoringIntermediate));
  end;

FUNCTION T_imageManipulationStep.alterParameter(CONST newString: ansistring): boolean;
  VAR newParam:T_parameterValue;
  begin
    if imageManipulationType=imt_generateImage then begin
      result:=(isPlausibleSpecification(newString,false)>=0);
      if result then param.createFromValue(stepParamDescription[imt_generateImage],newString);
    end else begin
      newParam.createToParse(stepParamDescription[imageManipulationType],newString);
      result:=newParam.isValid;
      if result then param:=newParam;
    end;
  end;

FUNCTION T_imageManipulationStep.descriptor: P_parameterDescription;
  begin
    result:=stepParamDescription[imageManipulationType];
  end;

FUNCTION T_imageManipulationStep.isGenerationStep:boolean;
  begin
    result:=imageManipulationType in [imt_generateImage,imt_setColor];
  end;

FUNCTION T_imageManipulationStep.isCropStep:boolean;
  begin
    result:=imageManipulationType=imt_crop;
  end;

FUNCTION T_imageManipulationStep.isWritingStashAccess:boolean;
  begin
    result:=imageManipulationType=imt_stashImage;
  end;

FUNCTION T_imageManipulationStep.isReadingStashAccess:boolean;
  begin
    result:=imageManipulationType in [imt_unstashImage, imt_addStash, imt_subtractStash, imt_multiplyStash, imt_divideStash, imt_screenStash, imt_maxOfStash, imt_minOfStash, imt_blurWithStash];
  end;

FUNCTION T_imageManipulationStep.hasComplexParameterDescription:boolean;
  begin
    result:=isGenerationStep or (descriptor^.subCount>0)
  end;

FUNCTION T_imageManipulationStep.getImageManipulationType:T_imageManipulationType;
  begin
    result:=imageManipulationType;;
  end;

CONSTRUCTOR T_imageManipulationWorkflow.create;
  begin
    setLength(imageStash,0);
    setLength(step,0);
    workflowImage.create(1,1);
    progressQueue.create(@imageGeneration.defaultProgressQueue);
    clear;
  end;

DESTRUCTOR T_imageManipulationWorkflow.destroy;
  begin
    clear;
    workflowImage.destroy;
    progressQueue.destroy;
  end;

PROCEDURE T_imageManipulationWorkflow.raiseError(CONST message: ansistring);
  begin
    ShowMessage('Error :'+message);
    progressQueue.logStepMessage('Error: '+message);
    progressQueue.cancelCalculation(false);
    beep;
  end;

PROCEDURE T_imageManipulationWorkflow.clearIntermediate;
  VAR i:longint;
  begin
    for i:=0 to length(intermediate)-1 do begin
      if intermediate[i]<>nil then dispose(intermediate[i],destroy);
      intermediate[i]:=nil;
    end;
    setLength(intermediate,0);
  end;

PROCEDURE T_imageManipulationWorkflow.storeIntermediate(CONST index: longint);
  VAR i:longint;
  begin
    if (index>=0) and (index<length(step)) then begin
      i:=length(intermediate);
      while index>=i do begin
        setLength(intermediate,i+1);
        intermediate[i]:=nil;
        inc(i);
      end;
      if intermediate[index]<>nil
      then intermediate[index]^.copyFromPixMap(workflowImage)
      else new(intermediate[index],create(workflowImage));
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.clearStash;
  VAR i:longint;
  begin
    for i:=0 to length(imageStash)-1 do if imageStash[i].img<>nil then begin dispose(imageStash[i].img,destroy); imageStash[i].img:=nil; end;
  end;

PROCEDURE T_imageManipulationWorkflow.execute(CONST previewMode, doStoreIntermediate,skipFit: boolean; CONST xRes, yRes, maxXRes, maxYRes: longint);
  VAR i,iInt:longint;
      expectedDimensions:T_imageDimensions;
  begin
    updateStashMetaData;
    if doStoreIntermediate then begin
      if inputImage<>nil then begin
         expectedDimensions:=inputImage^.dimensions;
         if not(skipFit) then expectedDimensions:=resize(expectedDimensions,maxXRes,maxYRes,res_fit);
       end else begin
         expectedDimensions.width :=xRes;
         expectedDimensions.height:=yRes;
       end;
      progressQueue.ensureStop;
      if (previewMode<>intermediatesAreInPreviewQuality) then begin
        clearIntermediate;
        clearStash;
      end;
      iInt:=-1;
      for i:=0 to length(intermediate)-1 do begin
        expectedDimensions:=step[i].expectedOutputResolution(expectedDimensions);
        if (intermediate[i]<>nil) and ((intermediate[i]^.dimensions.width <>expectedDimensions.width ) or
                                       (intermediate[i]^.dimensions.height<>expectedDimensions.height)) then begin
          dispose(intermediate[i],destroy);
          intermediate[i]:=nil;
        end;
        if intermediate[i]<>nil then iInt:=i;
      end;
      if iInt<0 then begin
        if inputImage<>nil then begin
          workflowImage.copyFromPixMap(inputImage^);
          if not(skipFit) then begin
            {$ifdef DEBUG}
            writeln('Resizing input image. Original size: ',workflowImage.dimensions.width,'x',workflowImage.dimensions.height);
            {$endif}
            workflowImage.resize(maxXRes,maxYRes,res_fit);
            {$ifdef DEBUG}
            writeln('                         resized to: ',workflowImage.dimensions.width,'x',workflowImage.dimensions.height);
            {$endif}
          end;
        end else workflowImage.resize(xRes,yRes,res_dataResize);
      end else begin
        workflowImage.copyFromPixMap(intermediate[iInt]^);
        {$ifdef DEBUG}
        writeln('Resuming workflow @step #',iInt);
        {$endif}
      end;

      progressQueue.forceStart(et_commentedStepsOfVaryingCost_serial,length(step)-iInt-1);
      for i:=iInt+1 to length(step)-1 do progressQueue.enqueue(step[i].getTodo(@self,previewMode, i,maxXRes,maxYRes));
      intermediatesAreInPreviewQuality:=previewMode;
    end else begin
      progressQueue.forceStart(et_commentedStepsOfVaryingCost_serial,length(step));
      if inputImage<>nil then begin
        workflowImage.copyFromPixMap(inputImage^);
        if not(skipFit) then workflowImage.resize(maxXRes,maxYRes,res_fit);
      end else workflowImage.resize(xRes,yRes,res_dataResize);
      clearIntermediate;
      clearStash;
      for i:=0 to length(step)-1 do progressQueue.enqueue(step[i].getTodo(@self,previewMode,-1,maxXRes,maxYRes));
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.enqueueAllAndStore(CONST sizeLimit:longint; CONST targetName:ansistring);
  VAR i:longint;
      saveStep:P_imageManipulationStep;
      par:T_parameterValue;
  begin
    clearIntermediate;
    clearStash;
    updateStashMetaData;
    progressQueue.forceStart(et_commentedStepsOfVaryingCost_serial,length(step)+1);
    for i:=0 to length(step)-1 do progressQueue.enqueue(step[i].getTodo(@self,false,-1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH));
    if targetName=C_nullSourceOrTargetFileName then exit;
    if (sizeLimit>=0) and (uppercase(extractFileExt(targetName))='.JPG') then begin
      par.createFromValue(stepParamDescription[imt_saveJpgWithSizeLimit],targetName,sizeLimit);
      new(saveStep,create(imt_saveJpgWithSizeLimit,par));
    end else begin
      par.createFromValue(stepParamDescription[imt_saveImage],targetName);
      new(saveStep,create(imt_saveImage,par));
    end;
    saveStep^.volatile:=true;
    progressQueue.enqueue(saveStep^.getTodo(@self,false,-1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH));
  end;

PROCEDURE T_imageManipulationWorkflow.updateStashMetaData;
  VAR i,stashIdx:longint;
  begin
    for i:=0 to length(imageStash)-1 do with imageStash[i] do begin
      firstWritten:=maxLongint;
      lastWritten:=-1;
      lastRead:=-1;
    end;
    for i:=0 to length(step)-1 do begin
      step[i].index:=i;
      stashIdx:=stashIndexForStep(step[i],true);
      if (stashIdx>=0) and (stashIdx<length(imageStash)) then begin
        if (step[i].isReadingStashAccess) then with imageStash[stashIdx] do begin
          if i>lastRead then lastRead:=i;
        end else if (step[i].isWritingStashAccess) then with imageStash[stashIdx] do begin
          if i<firstWritten then firstWritten:=i;
          if i>lastWritten  then lastWritten:=i;
        end;
      end;
    end;
    i:=length(imageStash)-1;
    while (i>=0) and (imageStash[i].firstWritten<0) do begin
      with imageStash[i] do if img<>nil then dispose(img,destroy);
      setLength(imageStash,i);
      dec(i);
    end;
  end;

FUNCTION T_imageManipulationWorkflow.stashIndexForStep(CONST step:T_imageManipulationStep; CONST allowCreation:boolean):longint;
  VAR i:longint;
  begin
    if not(step.isWritingStashAccess) and not(step.isReadingStashAccess) then exit(-1);
    result:=length(imageStash);
    for i:=0 to length(imageStash)-1 do if (imageStash[i].id=step.param.fileName) then result:=i;
    i:=length(imageStash);
    if not(allowCreation) and (result>=i) then exit(-1);
    while allowCreation and (result>=i) do begin
      setLength(imageStash,i+1);
      with imageStash[i] do begin
        id:=step.param.fileName;
        firstWritten:=maxLongint;
        lastWritten:=-1;
        lastRead:=-1;
        img:=nil;
      end;
      inc(i);
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.stashImage(CONST step:T_imageManipulationStep; VAR source:T_rawImage);
  VAR stashIdx:longint;
  begin
    stashIdx:=stashIndexForStep(step,false);
    if (stashIdx<0) or (stashIdx>=length(imageStash)) then begin
      raiseError('Invalid stash index (step #'+intToStr(step.index+1)+' tries to write unknown stash "'+step.param.fileName+'"');
      exit;
    end;
    with imageStash[stashIdx] do
    if img=nil then new(img,create(source))
               else img^.copyFromPixMap(source);
  end;

PROCEDURE T_imageManipulationWorkflow.unstashImage(CONST step:T_imageManipulationStep; CONST retainStashesAfterLastUse:boolean; VAR target:T_rawImage);
  VAR stashed:P_rawImage;
      doDispose:boolean;
  begin
    stashed:=getStashedImage(step,retainStashesAfterLastUse,doDispose);
    if stashed=nil then begin
      raiseError('Invalid stash index (step #'+intToStr(step.index+1)+' tries to read uninitialized stash "'+step.param.fileName+'")');
      exit;
    end;
    target.copyFromPixMap(stashed^);
    if doDispose then dispose(stashed,destroy);
  end;

FUNCTION T_imageManipulationWorkflow.getStashedImage(CONST step:T_imageManipulationStep; CONST retainStashesAfterLastUse:boolean; OUT disposeAfterUse:boolean):P_rawImage;
  VAR stashIdx:longint;
  begin
    stashIdx:=stashIndexForStep(step,false);
    if (stashIdx>=0) and (stashIdx<length(imageStash)) and (imageStash[stashIdx].img<>nil) then
    with imageStash[stashIdx] do begin
      result:=img;
      disposeAfterUse:=not(retainStashesAfterLastUse) and (step.index>=imageStash[stashIdx].lastRead);
      if disposeAfterUse then img:=nil;
    end else begin
      raiseError('Invalid stash index (step #'+intToStr(step.index+1)+' tries to read uninitialized stash "'+step.param.fileName+'")');
      result:=nil;
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.executeForTarget(CONST xRes, yRes, sizeLimit: longint; CONST targetName: ansistring);
  begin
    if workflowType=wft_manipulative then exit;
    progressQueue.ensureStop;
    workflowImage.resize(xRes,yRes,res_dataResize);
    enqueueAllAndStore(sizeLimit,targetName);
  end;

PROCEDURE T_imageManipulationWorkflow.executeForTarget(CONST inputImageFileName:ansistring; CONST sizeLimit:longint; CONST targetName:ansistring);
  begin
    if (workflowType=wft_generative) or (workflowType=wft_manipulative) and not(fileExists(inputImageFileName)) and (inputImageFileName<>C_nullSourceOrTargetFileName) then exit;
    progressQueue.ensureStop;
    if (workflowType=wft_manipulative) and (inputImageFileName<>C_nullSourceOrTargetFileName) then workflowImage.loadFromFile(inputImageFileName);
    enqueueAllAndStore(sizeLimit,targetName);
  end;

PROCEDURE T_imageManipulationWorkflow.storeToDo(CONST initialStep:T_imageManipulationStep; CONST sizeLimit:longint; CONST targetName:ansistring);
   VAR todo:T_imageManipulationWorkflow;
       storeStep:T_imageManipulationStep;
       storeParam:T_parameterValue;
       i:longint;
   FUNCTION todoName:ansistring;
     begin
       repeat
         result:='T'+intToStr(random(maxLongint))+'.todo';
       until not(fileExists(result));
     end;

   begin
     todo.create;
     todo.addStep(initialStep.toString);
     //Core steps:
     for i:=0 to length(step)-1 do todo.addStep(step[i].toString);
     //Store step:
     if (sizeLimit>=0) and (uppercase(extractFileExt(targetName))='.JPG') then begin
       storeParam.createFromValue(stepParamDescription[imt_saveJpgWithSizeLimit],extractRelativePath(GetCurrentDir+DirectorySeparator,targetName),sizeLimit);
       storeStep.create(imt_saveJpgWithSizeLimit,storeParam);
     end else begin
       storeParam.createFromValue(stepParamDescription[imt_saveImage],extractRelativePath(GetCurrentDir+DirectorySeparator,targetName));
       storeStep.create(imt_saveImage,storeParam);
     end;
     todo.addStep(storeStep.toString());
     storeStep.destroy;

     todo.saveToFile(todoName);
     todo.destroy;
   end;

PROCEDURE T_imageManipulationWorkflow.storeToDo(CONST xRes, yRes, sizeLimit: longint; CONST targetName: ansistring);
  VAR newStep:T_imageManipulationStep;
      param:T_parameterValue;
  begin
    param.createFromValue(stepParamDescription[imt_resize],xRes,yRes);
    newStep.create(imt_resize,param);
    storeToDo(newStep,sizeLimit,targetName);
    newStep.destroy;
  end;

PROCEDURE T_imageManipulationWorkflow.storeToDo(CONST inputImageFileName:ansistring; CONST sizeLimit:longint; CONST targetName:ansistring);
   VAR newStep:T_imageManipulationStep;
       param:T_parameterValue;
   begin
     param.createFromValue(stepParamDescription[imt_loadImage],extractRelativePath(GetCurrentDir+DirectorySeparator,inputImageFileName));
     newStep.create(imt_loadImage,param);
     storeToDo(newStep,sizeLimit,targetName);
     newStep.destroy;
   end;

FUNCTION T_imageManipulationWorkflow.findAndExecuteToDo: boolean;
  CONST maxDepth=5;
  VAR todoName:ansistring;
      depth:longint=0;
      root:ansistring='.';
  begin
    repeat
      todoName:=findDeeply(expandFileName(root),'*.todo');
      root:=root+'/..';
      inc(depth);
    until (depth>=maxDepth) or (todoName<>'');
    if todoName='' then exit(false);
    loadFromFile(todoName);
    progressQueue.registerOnEndCallback(@findAndExecuteToDo_DONE);
    execute(false,false,false,1,1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH);
    result:=true;
  end;

PROCEDURE T_imageManipulationWorkflow.findAndExecuteToDo_DONE;
  begin
    if isTempTodo and fileExists(myFileName) then DeleteFile(myFileName);
    clear;
  end;

FUNCTION T_imageManipulationWorkflow.isTempTodo: boolean;
  begin
    result:=(uppercase(extractFileExt(myFileName))='.TODO');
  end;

FUNCTION T_imageManipulationWorkflow.renderIntermediate(CONST index: longint; VAR target: TImage): boolean;
  begin
    if (index>=0) and (index<length(intermediate)) and (intermediate[index]<>nil) then begin
      intermediate[index]^.copyToImage(target);
      result:=true;
    end else result:=false;
  end;

FUNCTION T_imageManipulationWorkflow.renderIntermediate(CONST index:longint; VAR target:T_rawImage):boolean;
  begin
    if (index>=0) and (index<length(intermediate)) and (intermediate[index]<>nil) then begin
      target.copyFromPixMap(intermediate[index]^);
      result:=true;
    end else result:=false;
  end;

PROCEDURE T_imageManipulationWorkflow.clear;
  VAR i:longint;
  begin
    progressQueue.ensureStop;
    clearIntermediate;
    clearStash;
    for i:=0 to length(step)-1 do step[i].destroy;
    setLength(step,0);
    myFileName:='';
    updateStashMetaData;
    intermediatesAreInPreviewQuality:=false;
  end;

FUNCTION T_imageManipulationWorkflow.addStep(CONST command: ansistring): boolean;
  VAR idx:longint;
  begin
    result:=false;
    idx:=length(step);
    setLength(step,idx+1);
    step[idx].create(command);
    if step[idx].isValid
    then begin
      step[idx].index:=idx;
      exit(true);
    end else begin
      step[idx].destroy;
      setLength(step,idx);
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.remStep(CONST index: longint);
  VAR i:longint;
  begin
    if (index>=0) and (index<length(step)) then begin
      progressQueue.ensureStop;
      for i:=index to length(step)-2 do step[i]:=step[i+1];
      setLength(step,length(step)-1);
      stepChanged(index);
    end;
  end;

FUNCTION T_imageManipulationWorkflow.stepCount: longint;
  begin
    result:=length(step);
  end;

PROCEDURE T_imageManipulationWorkflow.swapStepDown(CONST lowerIndex: longint);
  VAR i0,i1,it:longint;
  begin
    if (lowerIndex>=0) and (lowerIndex<length(step)-1) then begin
      progressQueue.ensureStop;
      i0:=lowerIndex;
      i1:=lowerIndex+1;
      it:=length(step);
      setLength(step,it+1);
      step[it]:=step[i0];
      step[i0]:=step[i1];
      step[i1]:=step[it];
      setLength(step,it);
      stepChanged(lowerIndex);
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.stepChanged(CONST index:longint);
  VAR i:longint;
  begin
    for i:=index to length(intermediate)-1 do if intermediate[i]<>nil then begin
      dispose(intermediate[i],destroy);
      intermediate[i]:=nil;
    end;
    for i:=0 to length(imageStash)-1 do
    with imageStash[i] do
    if (lastWritten>index) and (img<>nil) then begin
      dispose(img,destroy);
      img:=nil;
      stepChanged(firstWritten);
    end;
  end;

FUNCTION T_imageManipulationWorkflow.loadFromFile(CONST fileNameUtf8: string):boolean;
  VAR handle:text;
      nextCmd:ansistring;
  begin
    result:=true;
    try
      clear;
      myFileName:=expandFileName(fileNameUtf8);
      assign(handle,fileNameUtf8);
      reset(handle);
      while not(eof(handle)) do begin
        readln(handle,nextCmd);
        if trim(nextCmd)<>'' then result:=result and addStep(nextCmd);
      end;
      close(handle);
    except
      result:=false;
    end;
    if result then SetCurrentDir(associatedDir)
              else begin
                SetCurrentDir(ExtractFileDir(paramStr(0)));
                clear;
              end;
  end;

PROCEDURE T_imageManipulationWorkflow.saveToFile(CONST fileNameUtf8: string);
  VAR handle:text;
      i:longint;
  begin
    assign(handle,fileNameUtf8);
    rewrite(handle);
    for i:=0 to length(step)-1 do writeln(handle,step[i].toString());
    close(handle);
    myFileName:=expandFileName(fileNameUtf8);
  end;

FUNCTION T_imageManipulationWorkflow.associatedFile: string;
  begin
    result:=myFileName;
  end;

FUNCTION T_imageManipulationWorkflow.associatedDir: string;
  begin
    if myFileName='' then result:='' else result:=extractFilePath(myFileName);
  end;

FUNCTION T_imageManipulationWorkflow.proposedImageFileName(CONST resString: ansistring): string;
  VAR i:longint;
      newExt:ansistring;
  begin
    if (workflowType<>wft_generative) or (resString='')
    then newExt:=''
    else newExt:='_'+resString;
    result:=ChangeFileExt(myFileName,newExt+'.jpg');
    if fileExists(result) then begin
      i:=0;
      repeat
        inc(i);
        result:=ChangeFileExt(myFileName,newExt+'_'+intToStr(i)+'.jpg');
      until not(fileExists(result))
    end;
  end;

FUNCTION T_imageManipulationWorkflow.workflowType: T_workflowType;
  begin
    if length(step)<=0 then exit(wft_empty_or_unknown);
    if step[0].isGenerationStep then exit(wft_generative);
    if (step[0].imageManipulationType in [imt_resize,imt_loadImage]) then begin
      if (step[length(step)-1].imageManipulationType in [imt_saveImage,imt_saveJpgWithSizeLimit])
      then exit(wft_fixated)
      else exit(wft_halfFix);
    end;
    result:=wft_manipulative;
  end;

INITIALIZATION
  initParameterDescriptions;
  workflow.create;
  workflow.addStep('setRGB:0.9');
  workflow.execute(true,false,false,1,1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH);
  workflow.clear;

FINALIZATION
  workflow.destroy;
  cleanupParameterDescriptions;
  if inputImage<>nil then dispose(inputImage,destroy);
end.
