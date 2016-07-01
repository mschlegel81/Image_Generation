UNIT workflows;
INTERFACE
USES myParams,mypics,myColors,sysutils,myTools,imageGeneration,ExtCtrls,mySys,FileUtil,Dialogs;
CONST MAX_HEIGHT_OR_WIDTH=9999;
TYPE
  T_imageManipulationCategory=(imc_generation,imc_imageAccess,imc_geometry,imc_colors,imc_combination,imc_statistic,imc_filter,imc_misc);
  T_imageManipulationType=(
                  imt_generateImage,
  {Image access:} imt_loadImage,imt_saveImage,imt_saveJpgWithSizeLimit, imt_stashImage, imt_unstashImage,
  {Geometry:}     imt_resize, imt_fit, imt_fill,
                  imt_crop,
                  imt_flip, imt_flop, imt_rotLeft, imt_rotRight,
  {Combination:}  imt_addRGB,   imt_subtractRGB,   imt_multiplyRGB,   imt_divideRGB,   imt_screenRGB,   imt_maxOfRGB,   imt_minOfRGB,
                  imt_addHSV,   imt_subtractHSV,   imt_multiplyHSV,   imt_divideHSV,   imt_screenHSV,   imt_maxOfHSV,   imt_minOfHSV,
                  imt_addStash, imt_subtractStash, imt_multiplyStash, imt_divideStash, imt_screenStash, imt_maxOfStash, imt_minOfStash,
  {per pixel color op:} imt_setColor,imt_setHue,imt_tint,imt_project,imt_limit,imt_limitLow,imt_grey,imt_sepia,
                        imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV,
  {statistic color op:} imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey, imt_compress, imt_compressV, imt_compressSat,
                        imt_mono, imt_quantize,
                        imt_shine, imt_blur,
                        imt_lagrangeDiff, imt_radialBlur, imt_rotationalBlur, imt_blurWithStash,
                        imt_sharpen,imt_edges,imt_variance,
                        imt_mode,imt_median,imt_pseudomedian,
                        imt_sketch,imt_drip,imt_encircle,imt_gradient,imt_direction,imt_details);
CONST
  imageManipulationCategory:array[T_imageManipulationType] of T_imageManipulationCategory=(
    imc_generation,
    imc_imageAccess,imc_imageAccess,imc_imageAccess,imc_imageAccess,imc_imageAccess,
    imc_geometry,imc_geometry,imc_geometry,// imt_resize..imt_fill,
    imc_geometry,// imt_crop,
    imc_geometry,imc_geometry,imc_geometry,imc_geometry,//imt_flip..imt_rotRight,
    imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors, //imt_addRGB..imt_minOfRGB,
    imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors, //imt_addHSV..imt_minOfHSV,
    imc_combination,imc_combination,imc_combination,imc_combination,imc_combination,imc_combination,imc_combination, //imt_addStash..imt_minOfStash,
    imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,//imt_setColor..imt_sepia,
    imc_colors,imc_colors,imc_colors,imc_colors,imc_colors,//imt_invert..imt_gammaHSV,
    imc_statistic,imc_statistic,imc_statistic,imc_statistic,imc_statistic,imc_statistic,imc_statistic,imc_statistic,//imt_normalizeFull..imt_quantize,
    imc_misc, //imt_shine,
    imc_filter,imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter, imc_filter,  // imt_blur..imt_pseudomedian,
    imc_misc, imc_misc, imc_misc, //imt_sketch,imt_drip,imt_encircle,
    imc_filter,imc_filter,imc_filter //imt_gradient,imt_direction,imt_details
    );
TYPE

  P_imageManipulationStepToDo=^T_imageManipulationStepToDo;
  P_imageManipulationStep=^T_imageManipulationStep;

  { T_imageManipulationStep }

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
      PROCEDURE execute(CONST previewMode,retainStashesAfterLastUse:boolean);
      FUNCTION isValid:boolean;
      FUNCTION toString(CONST forProgress:boolean=false):ansistring;
      FUNCTION toStringPart(CONST valueAndNotKey:boolean):ansistring;
      FUNCTION getTodo(CONST previewMode:boolean; CONST stepIndexForStoringIntermediate,maxXRes,maxYRes:longint):P_imageManipulationStepToDo;
      FUNCTION alterParameter(CONST newString:ansistring):boolean;
      FUNCTION descriptor:P_parameterDescription;
      FUNCTION isGenerationStep:boolean;
      FUNCTION isCropStep:boolean;
      FUNCTION isWritingStashAccess:boolean;
      FUNCTION isReadingStashAccess:boolean;
      FUNCTION hasComplexParameterDescription:boolean;
  end;

  T_imageManipulationStepToDo=object(T_queueToDo)
    manipulationStep:P_imageManipulationStep;
    previewQuality:boolean;
    stepIndex:longint;
    CONSTRUCTOR create(CONST step:P_imageManipulationStep; CONST preview:boolean; CONST stepIndexForStoringIntermediate:longint=-1);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

  T_workflowType=(wft_generative,wft_manipulative,wft_fixated,wft_halfFix,wft_empty_or_unknown);
CONST
  C_workflowTypeString:array[T_workflowType] of string=('generative','manipulative','fix','half-fix','empty or unknown');
TYPE
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
      PROCEDURE stashImage(CONST step:T_imageManipulationStep; VAR Source:T_rawImage);
      PROCEDURE unstashImage(CONST step:T_imageManipulationStep; CONST retainStashesAfterLastUse:boolean; VAR target:T_rawImage);
      FUNCTION getStashedImage(CONST step:T_imageManipulationStep; CONST retainStashesAfterLastUse:boolean; OUT disposeAfterUse:boolean):P_rawImage;
    public
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

VAR progressQueue:T_progressEstimatorQueue;
    workflow:T_imageManipulationWorkflow;
    stepParamDescription:array[T_imageManipulationType] of P_parameterDescription;
    inputImage   :P_rawImage;
    workflowImage:T_rawImage;

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
    stepParamDescription[imt_flip                ]:=newParameterDescription('flip',        pt_none);
    stepParamDescription[imt_flop                ]:=newParameterDescription('flop',        pt_none);
    stepParamDescription[imt_rotLeft             ]:=newParameterDescription('rotL',        pt_none);
    stepParamDescription[imt_rotRight            ]:=newParameterDescription('rotR',        pt_none);
    stepParamDescription[imt_addRGB              ]:=newParameterDescription('+RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_subtractRGB         ]:=newParameterDescription('-RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_multiplyRGB         ]:=newParameterDescription('*RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_divideRGB           ]:=newParameterDescription('/RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_screenRGB           ]:=newParameterDescription('screenRGB',   pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_maxOfRGB            ]:=newParameterDescription('maxRGB',      pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_minOfRGB            ]:=newParameterDescription('minRGB',      pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
    stepParamDescription[imt_addHSV              ]:=newParameterDescription('+HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
    stepParamDescription[imt_subtractHSV         ]:=newParameterDescription('-HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
    stepParamDescription[imt_multiplyHSV         ]:=newParameterDescription('*HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
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
    stepParamDescription[imt_setHue              ]:=newParameterDescription('hue',         pt_float)^.setDefaultValue('0');
    stepParamDescription[imt_tint                ]:=newParameterDescription('tint',        pt_float)^.setDefaultValue('0');
    stepParamDescription[imt_project             ]:=newParameterDescription('project',     pt_none);
    stepParamDescription[imt_limit               ]:=newParameterDescription('limit',       pt_none);
    stepParamDescription[imt_limitLow            ]:=newParameterDescription('limitLow',    pt_none);
    stepParamDescription[imt_grey                ]:=newParameterDescription('grey',        pt_none);
    stepParamDescription[imt_sepia               ]:=newParameterDescription('sepia',       pt_none);
    stepParamDescription[imt_invert              ]:=newParameterDescription('invert',      pt_none);
    stepParamDescription[imt_abs                 ]:=newParameterDescription('abs',         pt_none);
    stepParamDescription[imt_gamma               ]:=newParameterDescription('gamma',       pt_float,   1E-3)^.setDefaultValue('1.3');
    stepParamDescription[imt_gammaRGB            ]:=newParameterDescription('gammaRGB',    pt_3floats, 1E-3)^.setDefaultValue('1.2,1.3,1.4');
    stepParamDescription[imt_gammaHSV            ]:=newParameterDescription('gammaHSV',    pt_3floats, 1E-3)^.setDefaultValue('1.2,1.3,1.4');
    stepParamDescription[imt_normalizeFull       ]:=newParameterDescription('normalize',   pt_none);
    stepParamDescription[imt_normalizeValue      ]:=newParameterDescription('normalizeV',  pt_none);
    stepParamDescription[imt_normalizeGrey       ]:=newParameterDescription('normalizeG',  pt_none);
    stepParamDescription[imt_compress            ]:=newParameterDescription('compress',pt_float,0)^.setDefaultValue('20');
    stepParamDescription[imt_compressV]:=newParameterDescription('compress V',pt_float,0)^.setDefaultValue('20');
    stepParamDescription[imt_compressSat]:=newParameterDescription('compress saturation',pt_float,0)^.setDefaultValue('20');
    stepParamDescription[imt_mono                ]:=newParameterDescription('mono',        pt_integer)^.setDefaultValue('10');
    stepParamDescription[imt_quantize            ]:=newParameterDescription('quantize',    pt_integer)^.setDefaultValue('16');
    stepParamDescription[imt_shine               ]:=newParameterDescription('shine',       pt_none);
    stepParamDescription[imt_blur                ]:=newParameterDescription('blur',        pt_floatOr2Floats,0)^.setDefaultValue('0.2');
    stepParamDescription[imt_lagrangeDiff        ]:=newParameterDescription('lagrangeDiff',pt_2floats,0)^.setDefaultValue('0.1,0.1');
    stepParamDescription[imt_radialBlur          ]:=newParameterDescription('radialBlur'  ,pt_3floats)^.setDefaultValue('1,0,0');
    stepParamDescription[imt_rotationalBlur      ]:=newParameterDescription('rotationalBlur',pt_3floats)^.setDefaultValue('1,0,0');
    stepParamDescription[imt_blurWithStash       ]:=newParameterDescription('blurWithStash',pt_string,0)^.setDefaultValue('0');
    stepParamDescription[imt_sharpen             ]:=newParameterDescription('sharpen'     ,pt_2floats,0)^.setDefaultValue('0.1,0.5');
    stepParamDescription[imt_edges               ]:=newParameterDescription('edges' ,pt_none);
    stepParamDescription[imt_variance]:=newParameterDescription('variance',pt_float,0)^.setDefaultValue('0.05');
    stepParamDescription[imt_median]:=newParameterDescription('median',pt_float,0)^.setDefaultValue('0.05');
    stepParamDescription[imt_pseudomedian]:=newParameterDescription('pseudoMedian',pt_2floats,0)^
      .addChildParameterDescription(spa_f0,'rel. sigma',pt_float,0)^
      .addChildParameterDescription(spa_f1,'param',pt_float)^
      .setDefaultValue('0.1,1');
    stepParamDescription[imt_mode]:=newParameterDescription('mode',pt_float,0)^.setDefaultValue('0.05');
    stepParamDescription[imt_sketch]:=newParameterDescription('sketch',pt_1I3F)^
      .setDefaultValue('20,0.1,0.8,0.2')^
      .addChildParameterDescription(spa_i0,'number of colors',pt_integer)^
      .addChildParameterDescription(spa_f1,'direction sigma' ,pt_float)^
      .addChildParameterDescription(spa_f2,'density'         ,pt_float)^
      .addChildParameterDescription(spa_f3,'tolerance'       ,pt_float);
    stepParamDescription[imt_drip]:=newParameterDescription('drip',pt_2floats,0,1)^
      .setDefaultValue('0.1,0.01')^
      .addChildParameterDescription(spa_f0,'diffusiveness',pt_float,0,1)^
      .addChildParameterDescription(spa_f1,'range' ,pt_float,0,1);
    stepParamDescription[imt_encircle]:=newParameterDescription('encircle',pt_1I2F,0)^
      .setDefaultValue('2000,0.5,0.2')^
      .addChildParameterDescription(spa_i0,'circle count',pt_integer,0)^
      .addChildParameterDescription(spa_f1,'opacity' ,pt_float,0,1)^
      .addChildParameterDescription(spa_f2,'circle size' ,pt_float,0);
    stepParamDescription[imt_gradient]:=newParameterDescription('gradient',pt_float,0)^.setDefaultValue('0.1');
    stepParamDescription[imt_direction]:=newParameterDescription('direction',pt_float,0)^.setDefaultValue('0.1');
    stepParamDescription[imt_details]:=newParameterDescription('details',pt_float,0)^.setDefaultValue('0.1');
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

CONSTRUCTOR T_imageManipulationStepToDo.create(CONST step:P_imageManipulationStep; CONST preview:boolean; CONST stepIndexForStoringIntermediate:longint=-1);
  begin
    inherited create;
    manipulationStep:=step;
    previewQuality:=preview;
    stepIndex:=stepIndexForStoringIntermediate;
  end;

DESTRUCTOR T_imageManipulationStepToDo.destroy;
  begin
  end;

PROCEDURE T_imageManipulationStepToDo.execute;
  begin
    progressQueue.logStepMessage(manipulationStep^.toString(true));
    manipulationStep^.execute(previewQuality,stepIndex>=0);
    if stepIndex>=0 then workflow.storeIntermediate(stepIndex);
    if manipulationStep^.volatile then dispose(manipulationStep,destroy);
    progressQueue.logStepDone;
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

PROCEDURE T_imageManipulationStep.execute(CONST previewMode,retainStashesAfterLastUse: boolean);

  FUNCTION plausibleResolution:boolean;
    begin
      if (param.i0>0) and (param.i0<10000) and (param.i1>0) and (param.i1<10000) then result:=true
      else begin
        result:=false;
        workflow.raiseError('Invalid resolution; Both values must be in range 1..'+intToStr(MAX_HEIGHT_OR_WIDTH));
      end;
    end;

  FUNCTION colMult  (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=a[i]*b[i]; end;
  PROCEDURE combine;
    FUNCTION colDiv   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=a[i]/b[i]; end;
    FUNCTION colScreen(CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do result[i]:=1-(1-a[i])*(1-b[i]); end;
    FUNCTION colMax   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if a[i]>b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
    FUNCTION colMin   (CONST a,b:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if a[i]<b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
    VAR x,y:longint;
        other:P_rawImage=nil;
        c1:T_floatColor;
        disposeOther:boolean=false;
    begin
      case imageManipulationType of
        imt_addStash..imt_minOfStash,imt_blurWithStash:
          begin
            other:=workflow.getStashedImage(self,retainStashesAfterLastUse,disposeOther);
            if other=nil then exit;
          end;
        //imt_addFile..imt_minOfFile : begin
        //  new(other,create(param.fileName));
        //  disposeOther:=true;
        //end;
        imt_addRGB..imt_minOfRGB: begin
          c1:=param.color;
          case imageManipulationType of
            imt_addRGB      : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=          workflowImage[x,y]+c1;
            imt_subtractRGB : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=          workflowImage[x,y]-c1;
            imt_multiplyRGB : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMult  (workflowImage[x,y],c1);
            imt_divideHSV   : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colDiv   (workflowImage[x,y],c1);
            imt_screenHSV   : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colScreen(workflowImage[x,y],c1);
            imt_maxOfHSV    : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMax   (workflowImage[x,y],c1);
            imt_minOfHSV    : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMin   (workflowImage[x,y],c1);
          end;
          exit;
        end;
        imt_addHSV..imt_minOfHSV: begin
          c1:=param.color;
          case imageManipulationType of
            imt_addHSV      : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=fromHSV(          toHSV(workflowImage[x,y])+c1);
            imt_subtractHSV : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=fromHSV(          toHSV(workflowImage[x,y])-c1);
            imt_multiplyHSV : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=fromHSV(colMult  (toHSV(workflowImage[x,y]),c1));
            imt_divideHSV   : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=fromHSV(colDiv   (toHSV(workflowImage[x,y]),c1));
            imt_screenHSV   : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=fromHSV(colScreen(toHSV(workflowImage[x,y]),c1));
            imt_maxOfHSV    : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=fromHSV(colMax   (toHSV(workflowImage[x,y]),c1));
            imt_minOfHSV    : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=fromHSV(colMin   (toHSV(workflowImage[x,y]),c1));
          end;
          exit;
        end;

      end;
      case imageManipulationType of
        imt_addStash     : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=workflowImage[x,y]+other^[x,y];
        imt_subtractStash: for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=workflowImage[x,y]-other^[x,y];
        imt_multiplyStash: for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMult  (workflowImage[x,y],other^[x,y]);
        imt_divideStash  : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colDiv   (workflowImage[x,y],other^[x,y]);
        imt_screenStash  : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colScreen(workflowImage[x,y],other^[x,y]);
        imt_maxOfStash   : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMax   (workflowImage[x,y],other^[x,y]);
        imt_minOfStash   : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMin   (workflowImage[x,y],other^[x,y]);
        imt_blurWithStash: workflowImage.blurWith(other^);
      end;
      if disposeOther and (other<>nil) then dispose(other,destroy);
    end;

  PROCEDURE colorOp;
    FUNCTION limitHigh(CONST x:T_floatColor):T_floatColor; inline; VAR i:byte; begin for i:=0 to 2 do if x[i]>1 then result[i]:=1 else result[i]:=x[i]; end;
    FUNCTION limitLow(CONST x:T_floatColor):T_floatColor;  inline; VAR i:byte; begin for i:=0 to 2 do if x[i]<0 then result[i]:=0 else result[i]:=x[i]; end;
    VAR x,y:longint;
    begin
      case imageManipulationType of
        imt_setColor: for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=param.color;
        imt_setHue:   for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=hue(workflowImage[x,y],param.f0);
        imt_tint:     for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=tint(workflowImage[x,y],param.f0);
        imt_project:  for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=projectedColor(workflowImage[x,y]);
        imt_limit:    for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=limitHigh(limitLow(workflowImage[x,y]));
        imt_limitLow: for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=          limitLow(workflowImage[x,y]);
        imt_grey    : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=    subjectiveGrey(workflowImage[x,y]);
        imt_sepia   : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=             sepia(workflowImage[x,y]);
        imt_invert:   for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=            invert(workflowImage[x,y]);
        imt_abs:      for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=            absCol(workflowImage[x,y]);
        imt_gamma:    for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=             gamma(workflowImage[x,y],param.f0,param.f0,param.f0);
        imt_gammaRGB: for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=             gamma(workflowImage[x,y],param.f0,param.f1,param.f2);
        imt_gammaHSV: for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=          gammaHSV(workflowImage[x,y],param.f0,param.f1,param.f2);
      end;
    end;

  PROCEDURE statisticColorOp;
    VAR compoundHistogram:T_compoundHistogram;
        greyHist:T_histogram;
        p0,p1:T_floatColor;
        x,y:longint;
        k:longint=0;

    FUNCTION normValue(CONST c:T_floatColor):T_floatColor;
      begin
        result:=toHSV(c);
        result[2]:=(result[2]-p0[0])*p1[0];
        result:=fromHSV(result);
      end;

    FUNCTION measure(CONST a,b:double):double;
      CONST a0=1/0.998;
            b0= -0.001;
      begin result:=sqr(a0-a)/3+(a0-a+b0-b)*(b0-b); end;
    FUNCTION measure(CONST a,b:T_floatColor):double;
      begin
        result:=(measure(a[0],b[0])+
                 measure(a[1],b[1])+
                 measure(a[2],b[2]))/3;
      end;


    begin
      case imageManipulationType of
        imt_normalizeFull: while k<4 do begin
          compoundHistogram:=workflowImage.histogram;
          compoundHistogram.R.getNormalizationParams(p0[0],p1[0]);
          compoundHistogram.G.getNormalizationParams(p0[1],p1[1]);
          compoundHistogram.B.getNormalizationParams(p0[2],p1[2]);
          {$IFDEF DEBUG} writeln('Normalization with parameters ',p0[0],' ',p1[0],'; measure:',measure(p0,p1)); {$ENDIF}
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMult(workflowImage[x,y]-p0,p1);
          if (compoundHistogram.mightHaveOutOfBoundsValues or (measure(p0,p1)>1)) and not(progressQueue.cancellationRequested) then inc(k) else k:=4;
          compoundHistogram.destroy;
        end;
        imt_normalizeValue: while k<4 do begin
          compoundHistogram:=workflowImage.histogramHSV;
          compoundHistogram.B.getNormalizationParams(p0[0],p1[0]);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=normValue(workflowImage[x,y]);
          if (compoundHistogram.B.mightHaveOutOfBoundsValues or (measure(p0[0],p1[0])>1)) and not(progressQueue.cancellationRequested) then inc(k) else k:=4;
          compoundHistogram.destroy;
        end;
        imt_normalizeGrey: while k<4 do begin
          compoundHistogram:=workflowImage.histogram;
          greyHist:=compoundHistogram.subjectiveGreyHistogram;
          greyHist.getNormalizationParams(p0[0],p1[0]);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=(workflowImage[x,y]-p0)*p1[0];
          if (greyHist.mightHaveOutOfBoundsValues or (measure(p0[0],p1[0])>1)) and not(progressQueue.cancellationRequested) then inc(k) else k:=4;
          greyHist.destroy;
          compoundHistogram.destroy;
        end;
        imt_compress: begin
          compoundHistogram:=workflowImage.histogram;
          greyHist:=compoundHistogram.sumHistorgram;
          greyHist.smoothen(param.f0);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=greyHist.lookup(workflowImage[x,y]);
          greyHist.destroy;
          compoundHistogram.destroy;
        end;
        imt_compressV: begin
          compoundHistogram:=workflowImage.histogramHSV;
          greyHist:=compoundHistogram.B;
          greyHist.smoothen(param.f0);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do begin
            p0:=toHSV(workflowImage[x,y]);
            p0[2]:=greyHist.lookup(p0[2]);
            workflowImage[x,y]:=fromHSV(p0);
          end;
          compoundHistogram.destroy;
        end;
        imt_compressSat: begin
          compoundHistogram:=workflowImage.histogramHSV;
          greyHist:=compoundHistogram.G;
          greyHist.smoothen(param.f0);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do begin
            p0:=toHSV(workflowImage[x,y]);
            p0[1]:=greyHist.lookup(p0[1]);
            workflowImage[x,y]:=fromHSV(p0);
          end;
          compoundHistogram.destroy;
        end;
      end;
    end;

  PROCEDURE monochrome;
    VAR i,j,l:longint;
        k:longint=0;
        cSum:array[0..2] of double=(0,0,0);
        c:T_floatColor;
        g,invG:double;
    begin
      for j:=0 to workflowImage.height-1 do for i:=0 to workflowImage.width-1 do begin
        c:=workflowImage[i,j];
        g:=greyLevel(c);
        if g>1E-3 then begin
          invG:=1/g;
          for l:=0 to 2 do cSum[l]:=cSum[l]+c[l]*invG;
          inc(k);
        end;
        c[0]:=g;
        workflowImage.pixel[i,j]:=c;
      end;
      invG:=1/k;
      for l:=0 to 2 do cSum[l]:=cSum[l]*invG;
      for j:=0 to workflowImage.height-1 do for i:=0 to workflowImage.width-1 do begin
        c:=workflowImage[i,j];
        g:=round(c[0]*param.i0)/param.i0;
        for l:=0 to 2 do c[l]:=g*cSum[l];
        workflowImage[i,j]:=c;
      end;
    end;

  PROCEDURE redefine(newImage:T_rawImage);
    begin
      workflowImage.copyFromImage(newImage);
      newImage.destroy;
    end;

  PROCEDURE doLoad;
    begin
      workflowImage.loadFromFile(ExpandFileNameUTF8(param.fileName));
      if (previewMode or retainStashesAfterLastUse) then
        workflowImage.resize(previewXRes,previewYRes,res_fit);
    end;

  PROCEDURE doDetails;
    VAR temp:T_rawImage;
        x,y:longint;
    begin
      temp.create(workflowImage);
      temp.blur(param.f0,param.f0);
      for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do
      workflowImage[x,y]:=workflowImage[x,y]-temp[x,y];
      temp.destroy;
    end;

  begin
    {$IFDEF DEBUG} writeln('Step #',index,': ',toString(),' (@',workflowImage.width,'x',workflowImage.height,')'); {$ENDIF}

    case imageManipulationType of
      imt_generateImage: prepareImage(param.fileName,@workflowImage,previewMode);
      imt_loadImage: doLoad;
      imt_saveImage: workflowImage.saveToFile(ExpandFileNameUTF8(param.fileName));
      imt_saveJpgWithSizeLimit: workflowImage.saveJpgWithSizeLimitReturningErrorOrBlank(ExpandFileNameUTF8(param.fileName),param.i0);
      imt_stashImage: workflow.stashImage(self,workflowImage);
      imt_unstashImage: workflow.unstashImage(self,retainStashesAfterLastUse,workflowImage);
      imt_resize: if plausibleResolution then begin
                    if (index=0) then workflowImage.resize(param.i0,param.i1,res_dataResize)
                                 else workflowImage.resize(param.i0,param.i1,res_exact);
                  end;
      imt_fit   : if plausibleResolution then workflowImage.resize(param.i0,param.i1,res_fit);
      imt_fill  : if plausibleResolution then workflowImage.resize(param.i0,param.i1,res_cropToFill);
      imt_crop  : workflowImage.crop(param.f0,param.f1,param.f2,param.f3);
      imt_flip  : workflowImage.flip;
      imt_flop  : workflowImage.flop;
      imt_rotLeft : workflowImage.rotLeft;
      imt_rotRight: workflowImage.rotRight;
      imt_addRGB..imt_minOfStash,imt_blurWithStash: combine;
      imt_setColor, imt_setHue, imt_tint, imt_project, imt_limit,imt_limitLow,imt_grey,imt_sepia,imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV: colorOp;
      imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey,imt_compress,imt_compressV,imt_compressSat:statisticColorOp;
      imt_mono: monochrome;
      imt_quantize: workflowImage.quantize(param.i0);
      imt_shine: workflowImage.shine;
      imt_blur: workflowImage.blur(param.f0,param.f1);
      imt_lagrangeDiff: workflowImage.lagrangeDiffusion(param.f0,param.f1);
      imt_radialBlur: workflowImage.radialBlur(param.f0,param.f1,param.f2);
      imt_rotationalBlur: workflowImage.rotationalBlur(param.f0,param.f1,param.f2);
      imt_sharpen: workflowImage.sharpen(param.f0,param.f1);
      imt_edges: workflowImage.prewittEdges;
      imt_variance: workflowImage.variance(param.f0);
      imt_median: workflowImage.medianFilter(param.f0);
      imt_pseudomedian: workflowImage.myFilter(param.f0,param.f1);
      imt_mode: workflowImage.modalFilter(param.f0);
      imt_sketch: workflowImage.sketch(param.i0,param.f1,param.f2,param.f3);
      imt_drip: workflowImage.drip(param.f0,param.f1);
      imt_encircle: workflowImage.encircle(param.i0,param.f1,param.f2,@progressQueue);
      imt_direction: redefine(workflowImage.directionMap(param.f0));
      imt_details: doDetails;
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

FUNCTION T_imageManipulationStep.getTodo(CONST previewMode: boolean; CONST stepIndexForStoringIntermediate, maxXRes, maxYRes: longint): P_imageManipulationStepToDo;
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
      result:=todoStep^.getTodo(previewMode,stepIndexForStoringIntermediate,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH);
    end else new(result,create(@self,previewMode, stepIndexForStoringIntermediate));
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
    result:=imageManipulationType=imt_generateImage;
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

CONSTRUCTOR T_imageManipulationWorkflow.create;
  begin
    setLength(imageStash,0);
    setLength(step,0);
    clear;
  end;

DESTRUCTOR T_imageManipulationWorkflow.destroy;
  begin
    clear;
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
      then intermediate[index]^.copyFromImage(workflowImage)
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
  begin
    updateStashMetaData;
    if doStoreIntermediate then begin
      progressQueue.ensureStop;
      if (previewMode<>intermediatesAreInPreviewQuality) then begin
        clearIntermediate;
        clearStash;
      end;
      iInt:=-1;
      for i:=0 to length(intermediate)-1 do begin
        if (intermediate[i]<>nil) and ((intermediate[i]^.width<>xRes) or (intermediate[i]^.height<>yRes)) then begin
          dispose(intermediate[i],destroy);
          intermediate[i]:=nil;
        end;
        if intermediate[i]<>nil then iInt:=i;
      end;
      if iInt<0 then begin
        if inputImage<>nil then begin
          workflowImage.copyFromImage(inputImage^);
          if not(skipFit) then workflowImage.resize(maxXRes,maxYRes,res_fit);
        end else workflowImage.resize(xRes,yRes,res_dataResize);
      end else workflowImage.copyFromImage(intermediate[iInt]^);

      progressQueue.forceStart(et_commentedStepsOfVaryingCost_serial,length(step)-iInt-1);
      for i:=iInt+1 to length(step)-1 do progressQueue.enqueue(step[i].getTodo(previewMode, i,maxXRes,maxYRes));
      intermediatesAreInPreviewQuality:=previewMode;
    end else begin
      progressQueue.forceStart(et_commentedStepsOfVaryingCost_serial,length(step));
      if inputImage<>nil then begin
        workflowImage.copyFromImage(inputImage^);
        if not(skipFit) then workflowImage.resize(maxXRes,maxYRes,res_fit);
      end else workflowImage.resize(xRes,yRes,res_dataResize);
      clearIntermediate;
      clearStash;
      for i:=0 to length(step)-1 do progressQueue.enqueue(step[i].getTodo(previewMode,-1,maxXRes,maxYRes));
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
    for i:=0 to length(step)-1 do progressQueue.enqueue(step[i].getTodo(false,-1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH));
    if (sizeLimit>=0) and (uppercase(extractFileExt(targetName))='.JPG') then begin
      par.createFromValue(stepParamDescription[imt_saveJpgWithSizeLimit],targetName,sizeLimit);
      new(saveStep,create(imt_saveJpgWithSizeLimit,par));
    end else begin
      par.createFromValue(stepParamDescription[imt_saveImage],targetName);
      new(saveStep,create(imt_saveImage,par));
    end;
    saveStep^.volatile:=true;
    progressQueue.enqueue(saveStep^.getTodo(false,-1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH));
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

PROCEDURE T_imageManipulationWorkflow.stashImage(CONST step:T_imageManipulationStep; VAR Source:T_rawImage);
  VAR stashIdx:longint;
  begin
    stashIdx:=stashIndexForStep(step,false);
    if (stashIdx<0) or (stashIdx>=length(imageStash)) then begin
      raiseError('Invalid stash index (step #'+intToStr(step.index+1)+' tries to write unknown stash "'+step.param.fileName+'"');
      exit;
    end;
    with imageStash[stashIdx] do
    if img=nil then new(img,create(Source))
               else img^.copyFromImage(Source);
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
    target.copyFromImage(stashed^);
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
    if (workflowType=wft_generative) or (workflowType=wft_manipulative) and not(fileExists(inputImageFileName)) then exit;
    progressQueue.ensureStop;
    if workflowType=wft_manipulative then workflowImage.loadFromFile(inputImageFileName);
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
         result:=ExpandFileNameUTF8('T'+intToStr(random(maxLongint))+'.todo');
       until not(fileExists(result));
     end;

   begin
     todo.create;
     todo.addStep(initialStep.toString);
     //Core steps:
     for i:=0 to length(step)-1 do todo.addStep(step[i].toString);
     //Store step:
     if (sizeLimit>=0) and (uppercase(extractFileExt(targetName))='.JPG') then begin
       storeParam.createFromValue(stepParamDescription[imt_saveJpgWithSizeLimit],extractRelativePath(GetCurrentDirUTF8+DirectorySeparator,targetName),sizeLimit);
       storeStep.create(imt_saveJpgWithSizeLimit,storeParam);
     end else begin
       storeParam.createFromValue(stepParamDescription[imt_saveImage],extractRelativePath(GetCurrentDirUTF8+DirectorySeparator,targetName));
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
     param.createFromValue(stepParamDescription[imt_loadImage],extractRelativePath(GetCurrentDirUTF8+DirectorySeparator,inputImageFileName));
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
      todoName:=findDeeply(ExpandFileNameUTF8(root),'*.todo');
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
      assign(handle,UTF8ToSys(fileNameUtf8));
      reset(handle);
      while not(eof(handle)) do begin
        readln(handle,nextCmd);
        if trim(nextCmd)<>'' then result:=result and addStep(nextCmd);
      end;
      close(handle);
    except
      result:=false;
    end;
    if result then SetCurrentDirUTF8(associatedDir)
              else begin
                SetCurrentDir(ExtractFileDir(paramStr(0)));
                clear;
              end;
  end;

PROCEDURE T_imageManipulationWorkflow.saveToFile(CONST fileNameUtf8: string);
  VAR handle:text;
      i:longint;
  begin
    assign(handle,UTF8ToSys(fileNameUtf8));
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
  progressQueue.create(@imageGeneration.defaultProgressQueue);
  workflowImage.create(1,1);
  workflow.create;
  workflow.addStep('setRGB:0.9');
  workflow.execute(true,false,false,1,1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH);
  workflow.clear;

FINALIZATION
  progressQueue.destroy;
  workflowImage.destroy;
  cleanupParameterDescriptions;
  if inputImage<>nil then dispose(inputImage,destroy);
end.
