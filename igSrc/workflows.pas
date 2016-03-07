UNIT workflows;
INTERFACE
USES myParams,mypics,myColors,sysutils,myTools,imageGeneration,ExtCtrls,mySys;
CONST MAX_HEIGHT_OR_WIDTH=9999;
TYPE
  T_imageManipulationType=(
                  imt_generateImage,
  {Image access:} imt_loadImage,imt_saveImage,imt_saveJpgWithSizeLimit, imt_stashImage, imt_unstashImage,
  {Geometry:}     imt_resize, imt_fit, imt_fill,
                  imt_crop,
                  imt_flip, imt_flop, imt_rotLeft, imt_rotRight,
  {Combination:}  imt_addRGB,   imt_subtractRGB,   imt_multiplyRGB,   imt_divideRGB,   imt_screenRGB,   imt_maxOfRGB,   imt_minOfRGB,
                  imt_addHSV,   imt_subtractHSV,   imt_multiplyHSV,   imt_divideHSV,   imt_screenHSV,   imt_maxOfHSV,   imt_minOfHSV,
                  imt_addStash, imt_subtractStash, imt_multiplyStash, imt_divideStash, imt_screenStash, imt_maxOfStash, imt_minOfStash,
                  imt_addFile,  imt_subtractFile,  imt_multiplyFile,  imt_divideFile,  imt_screenFile,  imt_maxOfFile,  imt_minOfFile,
  {per pixel color op:} imt_setColor,imt_setHue,imt_tint,imt_project,imt_limit,imt_limitLow,imt_grey,imt_sepia,
                        imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV,
  {statistic color op:} imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey, imt_compress,imt_mono, imt_quantize,
                        imt_shine, imt_blur,
                        imt_lagrangeDiff, imt_radialBlur, imt_rotationalBlur, imt_blurWithStash,
                        imt_sharpen,imt_edges,
                        imt_mode,imt_median,
                        imt_paint,imt_sketch);
                              //fk_compressR,fk_compressG,fk_compressB,fk_compressH,fk_compressS,fk_compressV,
                              //fk_extract_alpha
                              //fk_distFilter,fk_details,fk_nonlocalMeans,fk_rotBlur3,fk_rotBlur,fk_radBlur3,fk_radBlur,fk_cblur,fk_coarsen,fk_halftone,fk_median,fk_blurWith,fk_mode,fk_denoise

  P_imageManipulationStepToDo=^T_imageManipulationStepToDo;
  P_imageManipulationStep=^T_imageManipulationStep;
  T_imageManipulationStep=object
    private
      imageManipulationType:T_imageManipulationType;
      param:T_parameterValue;
      valid,volatile:boolean;
    public
      CONSTRUCTOR create(CONST command:ansistring);
      CONSTRUCTOR create(CONST typ:T_imageManipulationType; CONST param_:T_parameterValue);
      DESTRUCTOR destroy; virtual;
      PROCEDURE execute(CONST previewMode:boolean);
      FUNCTION isValid:boolean;
      FUNCTION toString(CONST forProgress:boolean=false):ansistring;
      FUNCTION toStringPart(CONST valueAndNotKey:boolean):ansistring;
      FUNCTION getTodo(CONST previewMode:boolean; CONST stepIndexForStoringIntermediate,maxXRes,maxYRes:longint):P_imageManipulationStepToDo;
      FUNCTION alterParameter(CONST newString:ansistring):boolean;
  end;

  T_imageManipulationStepToDo=object(T_queueToDo)
    manipulationStep:P_imageManipulationStep;
    previewQuality:boolean;
    stepIndex:longint;
    CONSTRUCTOR create(CONST step:P_imageManipulationStep; CONST preview:boolean; CONST stepIndexForStoringIntermediate:longint=-1);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

  T_workflowType=(wft_generative,wft_manipulative,wft_fixated,wft_empty_or_unknown);

  T_imageManipulationWorkflowType=(workflow_for_generation,workflow_for_manipulation);

  { T_imageManipulationWorkflow }

  T_imageManipulationWorkflow=object
    private
      myFileName:ansistring;
      imageStash:array of P_rawImage;

      intermediate:array of P_rawImage;
      intermediatesAreInPreviewQuality:boolean;
      PROCEDURE raiseError(CONST message:ansistring);
      PROCEDURE clearStash;
      PROCEDURE clearIntermediate;
      PROCEDURE storeIntermediate(CONST index:longint);
      PROCEDURE enqueueAllAndStore(CONST sizeLimit:longint; CONST targetName:ansistring);
    public
      step:array of T_imageManipulationStep;
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE execute(CONST previewMode,doStoreIntermediate:boolean; CONST xRes,yRes,maxXRes,maxYRes:longint);
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
      //PROCEDURE addGenerationStep(CONST command:ansistring);
      FUNCTION addStep(CONST command:ansistring):boolean;
      PROCEDURE remStep(CONST index:longint);
      FUNCTION stepCount:longint;
      PROCEDURE swapStepDown(CONST lowerIndex:longint);
      PROCEDURE stepChanged(CONST index:longint);

      PROCEDURE loadFromFile(CONST fileName:string);
      PROCEDURE saveToFile(CONST fileName:string);

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
    new(stepParamDescription[imt_generateImage       ],create('',            pt_string));
    new(stepParamDescription[imt_loadImage           ],create('load',        pt_fileName));
    new(stepParamDescription[imt_saveImage           ],create('save',        pt_fileName));
    new(stepParamDescription[imt_saveJpgWithSizeLimit],create('save',        pt_jpgNameWithSize));
    new(stepParamDescription[imt_stashImage          ],create('stash',       pt_integer, 0));
    new(stepParamDescription[imt_unstashImage        ],create('unstash',     pt_integer, 0));
    new(stepParamDescription[imt_resize              ],create('resize',      pt_2integers, 1, MAX_HEIGHT_OR_WIDTH));
    new(stepParamDescription[imt_fit                 ],create('fit',         pt_2integers, 1, MAX_HEIGHT_OR_WIDTH));
    new(stepParamDescription[imt_fill                ],create('fill',        pt_2integers, 1, MAX_HEIGHT_OR_WIDTH));
    new(stepParamDescription[imt_crop                ],create('crop',        pt_4floats));
    new(stepParamDescription[imt_flip                ],create('flip',        pt_none));
    new(stepParamDescription[imt_flop                ],create('flop',        pt_none));
    new(stepParamDescription[imt_rotLeft             ],create('rotL',        pt_none));
    new(stepParamDescription[imt_rotRight            ],create('rotR',        pt_none));
    new(stepParamDescription[imt_addRGB              ],create('+RGB',        pt_color));
    new(stepParamDescription[imt_subtractRGB         ],create('-RGB',        pt_color));
    new(stepParamDescription[imt_multiplyRGB         ],create('*RGB',        pt_color));
    new(stepParamDescription[imt_divideRGB           ],create('/RGB',        pt_color));
    new(stepParamDescription[imt_screenRGB           ],create('screenRGB',   pt_color));
    new(stepParamDescription[imt_maxOfRGB            ],create('maxRGB',      pt_color));
    new(stepParamDescription[imt_minOfRGB            ],create('minRGB',      pt_color));
    new(stepParamDescription[imt_addHSV              ],create('+HSV',        pt_3floats));
    new(stepParamDescription[imt_subtractHSV         ],create('-HSV',        pt_3floats));
    new(stepParamDescription[imt_multiplyHSV         ],create('*HSV',        pt_3floats));
    new(stepParamDescription[imt_divideHSV           ],create('/HSV',        pt_3floats));
    new(stepParamDescription[imt_screenHSV           ],create('screenHSV',   pt_3floats));
    new(stepParamDescription[imt_maxOfHSV            ],create('maxHSV',      pt_3floats));
    new(stepParamDescription[imt_minOfHSV            ],create('minHSV',      pt_3floats));
    new(stepParamDescription[imt_addStash            ],create('+stash',      pt_integer, 0));
    new(stepParamDescription[imt_subtractStash       ],create('-stash',      pt_integer, 0));
    new(stepParamDescription[imt_multiplyStash       ],create('*stash',      pt_integer, 0));
    new(stepParamDescription[imt_divideStash         ],create('/stash',      pt_integer, 0));
    new(stepParamDescription[imt_screenStash         ],create('screenStash', pt_integer, 0));
    new(stepParamDescription[imt_maxOfStash          ],create('maxStash',    pt_integer, 0));
    new(stepParamDescription[imt_minOfStash          ],create('minStash',    pt_integer, 0));
    new(stepParamDescription[imt_addFile             ],create('+file',       pt_fileName));
    new(stepParamDescription[imt_subtractFile        ],create('-file',       pt_fileName));
    new(stepParamDescription[imt_multiplyFile        ],create('*file',       pt_fileName));
    new(stepParamDescription[imt_divideFile          ],create('/file',       pt_fileName));
    new(stepParamDescription[imt_screenFile          ],create('screenFile',  pt_fileName));
    new(stepParamDescription[imt_maxOfFile           ],create('maxFile',     pt_fileName));
    new(stepParamDescription[imt_minOfFile           ],create('minFile',     pt_fileName));
    new(stepParamDescription[imt_setColor            ],create('setRGB',      pt_color));
    new(stepParamDescription[imt_setHue              ],create('hue',         pt_float));
    new(stepParamDescription[imt_tint                ],create('tint',        pt_float));
    new(stepParamDescription[imt_project             ],create('project',     pt_none));
    new(stepParamDescription[imt_limit               ],create('limit',       pt_none));
    new(stepParamDescription[imt_limitLow            ],create('limitLow',    pt_none));
    new(stepParamDescription[imt_grey                ],create('grey',        pt_none));
    new(stepParamDescription[imt_sepia               ],create('sepia',       pt_none));
    new(stepParamDescription[imt_invert              ],create('invert',      pt_none));
    new(stepParamDescription[imt_abs                 ],create('abs',         pt_none));
    new(stepParamDescription[imt_gamma               ],create('gamma',       pt_float,   1E-3));
    new(stepParamDescription[imt_gammaRGB            ],create('gammaRGB',    pt_3floats, 1E-3));
    new(stepParamDescription[imt_gammaHSV            ],create('gammaHSV',    pt_3floats, 1E-3));
    new(stepParamDescription[imt_normalizeFull       ],create('normalize',   pt_none));
    new(stepParamDescription[imt_normalizeValue      ],create('normalizeV',  pt_none));
    new(stepParamDescription[imt_normalizeGrey       ],create('normalizeG',  pt_none));
    new(stepParamDescription[imt_compress            ],create('compress',    pt_float));
    new(stepParamDescription[imt_mono                ],create('mono',        pt_integer));
    new(stepParamDescription[imt_quantize            ],create('quantize',    pt_integer));
    new(stepParamDescription[imt_shine               ],create('shine',       pt_none));
    new(stepParamDescription[imt_blur                ],create('blur',        pt_floatOr2Floats,0));
    new(stepParamDescription[imt_lagrangeDiff        ],create('lagrangeDiff',pt_2floats,0));
    new(stepParamDescription[imt_radialBlur          ],create('radialBlur'  ,pt_3floats));
    new(stepParamDescription[imt_rotationalBlur      ],create('rotationalBlur',pt_3floats));
    new(stepParamDescription[imt_blurWithStash       ],create('blurWithStash',pt_integer,0));
    new(stepParamDescription[imt_sharpen             ],create('sharpen'     ,pt_2floats,0));
    new(stepParamDescription[imt_edges               ],create('edges'       ,pt_none));
    new(stepParamDescription[imt_median              ],create('median'      ,pt_float,0));
    new(stepParamDescription[imt_mode                ],create('mode'        ,pt_float,0));
    new(stepParamDescription[imt_paint               ],create('paint'       ,pt_3floats,0));
    new(stepParamDescription[imt_sketch              ],create('sketch'      ,pt_3floats,0));
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
    manipulationStep^.execute(previewQuality);
    if stepIndex>=0 then workflow.storeIntermediate(stepIndex);
    if manipulationStep^.volatile then dispose(manipulationStep,destroy);
    progressQueue.logStepDone;
  end;

CONSTRUCTOR T_imageManipulationStep.create(CONST command: ansistring);
  VAR imt:T_imageManipulationType;
  begin
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

CONSTRUCTOR T_imageManipulationStep.create(CONST typ: T_imageManipulationType; CONST param_: T_parameterValue);
  begin
    volatile:=false;
    imageManipulationType:=typ;
    param:=param_;
    valid:=param.isValid;
  end;

DESTRUCTOR T_imageManipulationStep.destroy;
  begin
  end;

PROCEDURE T_imageManipulationStep.execute(CONST previewMode: boolean);
  PROCEDURE stash;
    VAR oldLength,i:longint;
    begin
      with workflow do begin
        if param.i0>=length(imageStash) then begin
          oldLength:=length(imageStash);
          setLength(imageStash,param.i0+1);
          for i:=oldLength to length(imageStash)-1 do imageStash[i]:=nil;
        end;
        if imageStash[param.i0]<>nil then dispose(imageStash[param.i0],destroy);
        new(imageStash[param.i0],create(workflowImage));
      end;
    end;

  PROCEDURE unstash;
    begin
      with workflow do if (param.i0<0) or (param.i0>=length(imageStash)) then raiseError('Invalid stash Index')
      else workflowImage.copyFromImage(imageStash[param.i0]^);
    end;

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
        other:P_rawImage;
        c1:T_floatColor;

    begin
      case imageManipulationType of
        imt_addStash..imt_minOfStash,imt_blurWithStash:
          with workflow do if (param.i0>=0) and (param.i0<length(imageStash))
          then other:=imageStash[param.i0]
          else begin raiseError('Invalid stash Index'); exit; end;
        imt_addFile..imt_minOfFile : new(other,create(param.fileName));
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
        imt_addStash     ,imt_addFile      : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=workflowImage[x,y]+other^[x,y];
        imt_subtractStash,imt_subtractFile : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=workflowImage[x,y]-other^[x,y];
        imt_multiplyStash,imt_multiplyFile : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMult  (workflowImage[x,y],other^[x,y]);
        imt_divideStash  ,imt_divideFile   : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colDiv   (workflowImage[x,y],other^[x,y]);
        imt_screenStash  ,imt_screenFile   : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colScreen(workflowImage[x,y],other^[x,y]);
        imt_maxOfStash   ,imt_maxOfFile    : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMax   (workflowImage[x,y],other^[x,y]);
        imt_minOfStash   ,imt_minOfFile    : for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMin   (workflowImage[x,y],other^[x,y]);
        imt_blurWithStash                  : workflowImage.blurWith(other^);
      end;
      if imageManipulationType in [imt_addFile..imt_minOfFile] then dispose(other,destroy);
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

    begin
      case imageManipulationType of
        imt_normalizeFull: while k<4 do begin
          compoundHistogram:=workflowImage.histogram;
          p0[0]:=compoundHistogram.R.percentile(0.1);
          p0[1]:=compoundHistogram.G.percentile(0.1);
          p0[2]:=compoundHistogram.B.percentile(0.1);
          p1[0]:=1/(compoundHistogram.R.percentile(99.9)-p0[0]);
          p1[1]:=1/(compoundHistogram.G.percentile(99.9)-p0[1]);
          p1[2]:=1/(compoundHistogram.B.percentile(99.9)-p0[2]);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMult(workflowImage[x,y]-p0,p1);
          if compoundHistogram.mightHaveOutOfBoundsValues and not(progressQueue.cancellationRequested) then inc(k) else k:=4;
          compoundHistogram.destroy;
        end;
        imt_normalizeValue: while k<4 do begin
          compoundHistogram:=workflowImage.histogramHSV;
          p0[0]:=compoundHistogram.B.percentile(0.1);
          p1[0]:=1/(compoundHistogram.B.percentile(99.9)-p0[0]);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=normValue(workflowImage[x,y]);
          if compoundHistogram.B.mightHaveOutOfBoundsValues and not(progressQueue.cancellationRequested) then inc(k) else k:=4;
          compoundHistogram.destroy;
        end;
        imt_normalizeGrey: while k<4 do begin
          compoundHistogram:=workflowImage.histogram;
          greyHist:=compoundHistogram.subjectiveGreyHistogram;
          p0:=greyHist.percentile(0.1)*white;
          p1[0]:=1/(greyHist.percentile(99.9)-p0[0]);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=(workflowImage[x,y]-p0)*p1[0];
          if greyHist.mightHaveOutOfBoundsValues and not(progressQueue.cancellationRequested) then inc(k) else k:=4;
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

  begin
    case imageManipulationType of
      imt_generateImage: prepareImage(param.fileName,@workflowImage,previewMode);
      imt_loadImage: workflowImage.loadFromFile(param.fileName);
      imt_saveImage: workflowImage.saveToFile(param.fileName);
      imt_saveJpgWithSizeLimit: workflowImage.saveJpgWithSizeLimitReturningErrorOrBlank(param.fileName,param.i0);
      imt_stashImage: stash;
      imt_unstashImage: unstash;
      imt_resize: if plausibleResolution then workflowImage.resize(param.i0,param.i1,res_exact);
      imt_fit   : if plausibleResolution then workflowImage.resize(param.i0,param.i1,res_fit);
      imt_fill  : if plausibleResolution then workflowImage.resize(param.i0,param.i1,res_cropToFill);
      imt_crop  : workflowImage.crop(param.f0,param.f1,param.f2,param.f3);
      imt_flip  : workflowImage.flip;
      imt_flop  : workflowImage.flop;
      imt_rotLeft : workflowImage.rotLeft;
      imt_rotRight: workflowImage.rotRight;
      imt_addRGB..imt_minOfFile,imt_blurWithStash: combine;
      imt_setColor, imt_setHue, imt_tint, imt_project, imt_limit,imt_limitLow,imt_grey,imt_sepia,imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV: colorOp;
      imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey,imt_compress:statisticColorOp;
      imt_mono: monochrome;
      imt_quantize: workflowImage.quantize(param.i0);
      imt_shine: workflowImage.shine;
      imt_blur: workflowImage.blur(param.f0,param.f1);
      imt_lagrangeDiff: workflowImage.lagrangeDiffusion(param.f0,param.f1);
      imt_radialBlur: workflowImage.radialBlur(param.f0,param.f1,param.f2);
      imt_rotationalBlur: workflowImage.rotationalBlur(param.f0,param.f1,param.f2);
      imt_sharpen: workflowImage.sharpen(param.f0,param.f1);
      imt_edges: workflowImage.naiveEdges;
      imt_median: workflowImage.medianFilter(param.f0);
      imt_mode: workflowImage.modalFilter(param.f0);
      //imt_paint: workflowImage.sketch(param.f0,param.f1,param.f2,true);
      imt_sketch: workflowImage.sketch(round(param.f0),param.f1,param.f2);
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

FUNCTION T_imageManipulationStep.toStringPart(CONST valueAndNotKey:boolean):ansistring;
  begin
    if valueAndNotKey then result:=param.toString(tsm_withoutParameterName)
    else if imageManipulationType=imt_generateImage
    then result:='<GENERATE>'
    else result:=param.toString(tsm_parameterNameOnly);
  end;

FUNCTION T_imageManipulationStep.getTodo(CONST previewMode:boolean; CONST stepIndexForStoringIntermediate,maxXRes,maxYRes: longint): P_imageManipulationStepToDo;
  VAR todoParam:T_parameterValue;
      todoStep:P_imageManipulationStep;
  begin
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

FUNCTION T_imageManipulationStep.alterParameter(CONST newString:ansistring):boolean;
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
    progressQueue.logStepMessage('Error: '+message);
    progressQueue.cancelCalculation(false);
    if displayErrorFunction<>nil
    then displayErrorFunction(message)
    else writeln(stdErr,message);
    beep;
  end;

PROCEDURE T_imageManipulationWorkflow.clearIntermediate;
  VAR i:longint;
  begin
    for i:=0 to length(intermediate)-1 do begin
      if intermediate[i]<>nil then dispose(intermediate[i],destroy);
      intermediate[i]:=nil;
    end;
    SetLength(intermediate,0);
  end;

PROCEDURE T_imageManipulationWorkflow.storeIntermediate(CONST index: longint);
  VAR i:longint;
  begin
    if (index>=0) and (index<length(step)) then begin
      i:=Length(intermediate);
      while index>=i do begin
        SetLength(intermediate,i+1);
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
    for i:=0 to length(imageStash)-1 do if imageStash[i]<>nil then dispose(imageStash[i],destroy);
    setLength(imageStash,0);
  end;

PROCEDURE T_imageManipulationWorkflow.execute(CONST previewMode, doStoreIntermediate: boolean; CONST xRes, yRes, maxXRes, maxYRes: longint);
  VAR i,iInt:longint;
  begin
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
          workflowImage.resize(maxXRes,maxYRes,res_fit);
        end else workflowImage.resize(xRes,yRes,res_dataResize);
      end else workflowImage.copyFromImage(intermediate[iInt]^);

      progressQueue.forceStart(et_commentedStepsOfVaryingCost_serial,length(step)-iInt-1);
      for i:=iInt+1 to length(step)-1 do progressQueue.enqueue(step[i].getTodo(previewMode, i,maxXRes,maxYRes));
      intermediatesAreInPreviewQuality:=previewMode;
    end else begin
      progressQueue.forceStart(et_commentedStepsOfVaryingCost_serial,length(step));
      if inputImage<>nil then begin
        workflowImage.copyFromImage(inputImage^);
        workflowImage.resize(maxXRes,maxYRes,res_fit);
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
         result:=extractFilePath(paramStr(0))+'T'+intToStr(random(maxLongint))+'.todo';
       until not(fileExists(result));
     end;

   begin
     todo.create;
     todo.addStep(initialStep.toString);
     //Core steps:
     for i:=0 to length(step)-1 do todo.addStep(step[i].toString);
     //Store step:
     if (sizeLimit>=0) and (uppercase(extractFileExt(targetName))='.JPG') then begin
       storeParam.createFromValue(stepParamDescription[imt_saveJpgWithSizeLimit],targetName,sizeLimit);
       storeStep.create(imt_saveJpgWithSizeLimit,storeParam);
     end else begin
       storeParam.createFromValue(stepParamDescription[imt_saveImage],targetName);
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
     param.createFromValue(stepParamDescription[imt_loadImage],expandFileName(inputImageFileName));
     newStep.create(imt_loadImage,param);
     storeToDo(newStep,sizeLimit,targetName);
     newStep.destroy;
   end;

FUNCTION T_imageManipulationWorkflow.findAndExecuteToDo: boolean;
  VAR todoName:ansistring;
  begin
    todoName:=findOne(extractFilePath(paramStr(0))+'*.todo');
    if todoName='' then exit(false);
    loadFromFile(todoName);
    progressQueue.registerOnEndCallback(@findAndExecuteToDo_DONE);
    execute(false,false,1,1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH);
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
    intermediatesAreInPreviewQuality:=false;
  end;

//PROCEDURE T_imageManipulationWorkflow.addGenerationStep(CONST command: ansistring);
//  VAR param:T_parameterValue;
//  begin
//    progressQueue.ensureStop;
//    if isPlausibleSpecification(command,false)<0 then exit;
//    setLength(step,length(step)+1);
//    param.createFromValue(stepParamDescription[imt_generateImage],command);
//    step[length(step)-1].create(imt_generateImage,param);
//  end;

FUNCTION T_imageManipulationWorkflow.addStep(CONST command: ansistring): boolean;
  begin
    result:=false;
    setLength(step,length(step)+1);
    step[length(step)-1].create(command);
    if step[length(step)-1].isValid
    then exit(true)
    else begin
      step[length(step)-1].destroy;
      setLength(step,length(step)-1);
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
  VAR i0,i1,it,i:longint;
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
  end;

PROCEDURE T_imageManipulationWorkflow.loadFromFile(CONST fileName: string);
  VAR handle:text;
      nextCmd:ansistring;
      allDone:boolean=false;
  begin
    try
      clear;
      myFileName:=expandFileName(fileName);
      assign(handle,fileName);
      reset(handle);
      while not(eof(handle)) do begin
        readln(handle,nextCmd);
        if trim(nextCmd)<>'' then addStep(nextCmd);
      end;
      allDone:=true;
      close(handle);
    except
      allDone:=false;
    end;
    if not(allDone) then clear;
  end;

PROCEDURE T_imageManipulationWorkflow.saveToFile(CONST fileName: string);
  VAR handle:text;
      i:longint;
  begin
    assign(handle,fileName);
    rewrite(handle);
    for i:=0 to length(step)-1 do writeln(handle,step[i].toString());
    close(handle);
    myFileName:=expandFileName(fileName);
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
    if FileExists(Result) then begin
      i:=0;
      repeat
        inc(i);
        result:=ChangeFileExt(myFileName,newExt+'_'+IntToStr(i)+'.jpg');
      until not(FileExists(result))
    end;
  end;

FUNCTION T_imageManipulationWorkflow.workflowType: T_workflowType;
  begin
    if length(step)<=0 then exit(wft_empty_or_unknown);
    if step[0].imageManipulationType = imt_generateImage then exit(wft_generative);
    if (step[0].imageManipulationType in [imt_resize,imt_loadImage]) and
       (step[length(step)-1].imageManipulationType in [imt_saveImage,imt_saveJpgWithSizeLimit]) then exit(wft_fixated);
    result:=wft_manipulative;
  end;

INITIALIZATION
  initParameterDescriptions;
  progressQueue.create(@imageGeneration.progressQueue);
  workflowImage.create(1,1);
  workflow.create;
  workflow.addStep('setRGB:0.9');
  workflow.execute(true,false,1,1,MAX_HEIGHT_OR_WIDTH,MAX_HEIGHT_OR_WIDTH);
  workflow.clear;

FINALIZATION
  progressQueue.destroy;
  workflowImage.destroy;
  cleanupParameterDescriptions;
end.
