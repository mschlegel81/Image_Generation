UNIT workflows;
INTERFACE
USES myParams,mypics,myColors,sysutils,myTools,imageGeneration,ExtCtrls;
TYPE
T_imageManipulationType=(
                  imt_generateImage,
  {Image access:} imt_loadImage,imt_saveImage,imt_saveJpgWithSizeLimit, imt_stashImage, imt_unstashImage,
  {Geometry:}     imt_resize, imt_fit, imt_fill, imt_crop, imt_flip, imt_flop, imt_rotLeft, imt_rotRight,
  {Combination:}  imt_addRGB,   imt_subtractRGB,   imt_multiplyRGB,   imt_divideRGB,   imt_screenRGB,   imt_maxOfRGB,   imt_minOfRGB,
                  imt_addHSV,   imt_subtractHSV,   imt_multiplyHSV,   imt_divideHSV,   imt_screenHSV,   imt_maxOfHSV,   imt_minOfHSV,
                  imt_addStash, imt_subtractStash, imt_multiplyStash, imt_divideStash, imt_screenStash, imt_maxOfStash, imt_minOfStash,
                  imt_addFile,  imt_subtractFile,  imt_multiplyFile,  imt_divideFile,  imt_screenFile,  imt_maxOfFile,  imt_minOfFile,
  {per pixel color op:} imt_setColor,imt_setHue,imt_tint,imt_project,imt_limit,imt_limitLow,imt_grey,imt_sepia,
                        imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV,
  {statistic color op:} imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey);
                              //fk_compress,fk_compressR,fk_compressG,fk_compressB,fk_compressH,fk_compressS,fk_compressV,
                              //fk_quantize,fk_mono,
                              //fk_extract_alpha
//fk_fblur,fk_fblur_V,fk_fblur_H,fk_distFilter,fk_sharpen,fk_details,fk_lagrangeDiff,fk_nonlocalMeans,fk_rotBlur3,fk_rotBlur,fk_radBlur3,fk_radBlur,fk_cblur,fk_coarsen,fk_halftone,fk_median,fk_blurWith,fk_mode,fk_denoise

  P_imageManipulationStepToDo=^T_imageManipulationStepToDo;
  P_imageManipulationStep=^T_imageManipulationStep;

  { T_imageManipulationStep }

  T_imageManipulationStep=object
    private
      imageManipulationType:T_imageManipulationType;
      param:T_parameterValue;
      valid:boolean;
    public
      CONSTRUCTOR create(CONST command:ansistring);
      CONSTRUCTOR create(CONST typ:T_imageManipulationType; CONST param_:T_parameterValue);
      DESTRUCTOR destroy;
      PROCEDURE execute(CONST previewMode:boolean);
      FUNCTION isValid:boolean;
      FUNCTION toString(CONST forProgress:boolean=false):ansistring;
      FUNCTION getTodo(CONST stepIndexForStoringIntermediate:longint=-1):P_imageManipulationStepToDo;
  end;

  T_imageManipulationStepToDo=object(T_queueToDo)
    manipulationStep:P_imageManipulationStep;
    stepIndex:longint;
    CONSTRUCTOR create(CONST step:P_imageManipulationStep; CONST stepIndexForStoringIntermediate:longint=-1);
    DESTRUCTOR destroy; virtual;
    PROCEDURE execute; virtual;
  end;

  { T_imageManipulationWorkflow }

  T_imageManipulationWorkflowType=(workflow_for_generation,workflow_for_manipulation);
  T_imageManipulationWorkflow=object
    private
      imageStash:array of P_rawImage;
      step:array of record
             manipulation:T_imageManipulationStep;
             intermediate:P_rawImage;
           end;

      PROCEDURE raiseError(CONST message:ansistring);
      PROCEDURE clearStash;
      PROCEDURE clearIntermediate;
      PROCEDURE storeIntermediate(CONST index:longint);
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE execute(CONST previewMode:boolean);

      FUNCTION renderIntermediate(CONST index:longint; VAR target:TImage):boolean;

      PROCEDURE clear;
      PROCEDURE addGenerationStep(CONST command:ansistring);
      PROCEDURE addStep(CONST command:ansistring);
      PROCEDURE remStep(CONST index:longint);
      PROCEDURE mutateStep(CONST index:longint; CONST command:ansistring);
      FUNCTION stepCount:longint;
      FUNCTION stepText(CONST index:longint):ansistring;
      PROCEDURE swapStepDown(CONST lowerIndex:longint);
  end;

VAR progressQueue:T_progressEstimatorQueue;
    workflowImage:T_rawImage;
    workflow:T_imageManipulationWorkflow;

IMPLEMENTATION
VAR parameterDescription:array[T_imageManipulationType] of P_parameterDescription;
PROCEDURE initParameterDescriptions;
  VAR imt:T_imageManipulationType;
      initFailed:boolean=false;
  begin
    for imt:=Low(T_imageManipulationType) to high(T_imageManipulationType) do parameterDescription[imt]:=nil;
    new(parameterDescription[imt_generateImage       ],create('gen:',        pt_fileName));
    new(parameterDescription[imt_loadImage           ],create('load:',       pt_fileName));
    new(parameterDescription[imt_saveImage           ],create('save:',       pt_fileName));
    new(parameterDescription[imt_saveJpgWithSizeLimit],create('save:',       pt_jpgNameWithSize));
    new(parameterDescription[imt_stashImage          ],create('#',           pt_integer, 0));
    new(parameterDescription[imt_unstashImage        ],create('unstash#',    pt_integer, 0));
    new(parameterDescription[imt_resize              ],create('resize',      pt_2integers, 1, 9999));
    new(parameterDescription[imt_fit                 ],create('fit',         pt_2integers, 1, 9999));
    new(parameterDescription[imt_fill                ],create('fill',        pt_2integers, 1, 9999));
    new(parameterDescription[imt_crop                ],create('crop',        pt_4integers));
    new(parameterDescription[imt_flip                ],create('flip',        pt_none));
    new(parameterDescription[imt_flop                ],create('flop',        pt_none));
    new(parameterDescription[imt_rotLeft             ],create('rotL',        pt_none));
    new(parameterDescription[imt_rotRight            ],create('rotR',        pt_none));
    new(parameterDescription[imt_addRGB              ],create('+RGB',        pt_color));
    new(parameterDescription[imt_subtractRGB         ],create('-RGB',        pt_color));
    new(parameterDescription[imt_multiplyRGB         ],create('*RGB',        pt_color));
    new(parameterDescription[imt_divideRGB           ],create('/RGB',        pt_color));
    new(parameterDescription[imt_screenRGB           ],create('screenRGB',   pt_color));
    new(parameterDescription[imt_maxOfRGB            ],create('maxRGB',      pt_color));
    new(parameterDescription[imt_minOfRGB            ],create('minRGB',      pt_color));
    new(parameterDescription[imt_addHSV              ],create('+HSV',        pt_3floats));
    new(parameterDescription[imt_subtractHSV         ],create('-HSV',        pt_3floats));
    new(parameterDescription[imt_multiplyHSV         ],create('*HSV',        pt_3floats));
    new(parameterDescription[imt_divideHSV           ],create('/HSV',        pt_3floats));
    new(parameterDescription[imt_screenHSV           ],create('screenHSV',   pt_3floats));
    new(parameterDescription[imt_maxOfHSV            ],create('maxHSV',      pt_3floats));
    new(parameterDescription[imt_minOfHSV            ],create('minHSV',      pt_3floats));
    new(parameterDescription[imt_addStash            ],create('+#',          pt_integer, 0));
    new(parameterDescription[imt_subtractStash       ],create('-#',          pt_integer, 0));
    new(parameterDescription[imt_multiplyStash       ],create('*#',          pt_integer, 0));
    new(parameterDescription[imt_divideStash         ],create('/#',          pt_integer, 0));
    new(parameterDescription[imt_screenStash         ],create('screen#',     pt_integer, 0));
    new(parameterDescription[imt_maxOfStash          ],create('max#',        pt_integer, 0));
    new(parameterDescription[imt_minOfStash          ],create('min#',        pt_integer, 0));
    new(parameterDescription[imt_addFile             ],create('+file:',      pt_fileName));
    new(parameterDescription[imt_subtractFile        ],create('-file:',      pt_fileName));
    new(parameterDescription[imt_multiplyFile        ],create('*file:',      pt_fileName));
    new(parameterDescription[imt_divideFile          ],create('/file:',      pt_fileName));
    new(parameterDescription[imt_screenFile          ],create('screenFile:', pt_fileName));
    new(parameterDescription[imt_maxOfFile           ],create('maxFile:',    pt_fileName));
    new(parameterDescription[imt_minOfFile           ],create('minFile:',    pt_fileName));
    new(parameterDescription[imt_setColor            ],create('setRGB',      pt_color));
    new(parameterDescription[imt_setHue              ],create('hue',         pt_float));
    new(parameterDescription[imt_tint                ],create('tint',        pt_float));
    new(parameterDescription[imt_project             ],create('project',     pt_none));
    new(parameterDescription[imt_limit               ],create('limit',       pt_none));
    new(parameterDescription[imt_limitLow            ],create('limitLow',    pt_none));
    new(parameterDescription[imt_grey                ],create('grey',        pt_none));
    new(parameterDescription[imt_sepia               ],create('sepia',       pt_none));
    new(parameterDescription[imt_invert              ],create('invert',      pt_none));
    new(parameterDescription[imt_abs                 ],create('abs',         pt_none));
    new(parameterDescription[imt_gamma               ],create('gamma',       pt_float,   1E-3));
    new(parameterDescription[imt_gammaRGB            ],create('gammaRGB',    pt_3floats, 1E-3));
    new(parameterDescription[imt_gammaHSV            ],create('gammaHSV',    pt_3floats, 1E-3));
    new(parameterDescription[imt_normalizeFull       ],create('normalize',   pt_none));
    new(parameterDescription[imt_normalizeValue      ],create('normalizeV',  pt_none));
    new(parameterDescription[imt_normalizeGrey       ],create('normalizeG',  pt_none));
    for imt:=Low(T_imageManipulationType) to high(T_imageManipulationType) do if parameterDescription[imt]=nil then begin
      writeln(stderr,'Missing initialization of parameterDescription[',imt,']');
      initFailed:=true;
    end;
    if initFailed then halt;
  end;

CONSTRUCTOR T_imageManipulationStepToDo.create(CONST step:P_imageManipulationStep; CONST stepIndexForStoringIntermediate:longint=-1);
  begin
    inherited create;
    manipulationStep:=step;
    stepIndex:=stepIndexForStoringIntermediate;
  end;

DESTRUCTOR T_imageManipulationStepToDo.destroy;
  begin
  end;

PROCEDURE T_imageManipulationStepToDo.execute;
  begin
    manipulationStep^.execute(stepIndex>=0);
    if stepIndex>=0 then workflow.storeIntermediate(stepIndex);
    progressQueue.logStepDone;
  end;

CONSTRUCTOR T_imageManipulationStep.create(CONST command: ansistring);
  VAR imt:T_imageManipulationType;
  begin
    for imt:=Low(T_imageManipulationType) to high(T_imageManipulationType) do begin
      param.createToParse(parameterDescription[imt],command,tsm_withNiceParameterName);
      if param.isValid then begin
        valid:=(imt<>imt_generateImage) or imageGeneration.isPlausibleSpecification(param.fileName);
        imageManipulationType:=imt;
        exit;
      end;
    end;
    valid:=false;
  end;

CONSTRUCTOR T_imageManipulationStep.create(CONST typ: T_imageManipulationType; CONST param_: T_parameterValue);
  begin
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
        if param.i0>length(imageStash) then begin
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
      with workflow do if (param.i0<0) or (param.i0>length(imageStash)) then raiseError('Invalid stash Index')
      else workflowImage.copyFromImage(imageStash[param.i0]^);
    end;

  FUNCTION plausibleResolution:boolean;
    begin
      if (param.i0>0) and (param.i0<10000) and (param.i1>0) and (param.i1<10000) then result:=true
      else begin
        result:=false;
        workflow.raiseError('Invalid resolution; Both values must be in range 1..9999.');
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
        imt_addStash..imt_minOfStash:
          with workflow do if (param.i0>=0) and (param.i0<length(imageStash))
          then raiseError('Invalid stash Index')
          else other:=imageStash[param.i0];
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
        imt_gamma:    for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=             gamma(workflowImage[x,y],param.f0,param.f1,param.f2);
        imt_gammaRGB: for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=             gamma(workflowImage[x,y],param.f0,param.f1,param.f2);
        imt_gammaHSV: for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=          gammaHSV(workflowImage[x,y],param.f0,param.f1,param.f2);
      end;
    end;

  PROCEDURE statisticColorOp;
    VAR compoundHistogram:T_compoundHistogram;
        greyHist:T_histogram;
        p0,p1:T_floatColor;
        x,y:longint;
    FUNCTION normValue(CONST c:T_floatColor):T_floatColor;
      begin
        result:=toHSV(c);
        result[2]:=(result[2]-p0[0])*p1[0];
        result:=fromHSV(result);
      end;

    begin
      case imageManipulationType of
        imt_normalizeFull: begin
          compoundHistogram:=workflowImage.histogram;
          p0[0]:=compoundHistogram.R.percentile(0.1);
          p0[1]:=compoundHistogram.G.percentile(0.1);
          p0[2]:=compoundHistogram.B.percentile(0.1);
          p1[0]:=1/(compoundHistogram.R.percentile(99.9)-p0[0]);
          p1[1]:=1/(compoundHistogram.G.percentile(99.9)-p0[1]);
          p1[2]:=1/(compoundHistogram.B.percentile(99.9)-p0[2]);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=colMult(workflowImage[x,y]-p0,p1);
          if compoundHistogram.mightHaveOutOfBoundsValues then statisticColorOp;
          compoundHistogram.destroy;
        end;
        imt_normalizeValue: begin
          compoundHistogram:=workflowImage.histogramHSV;
          p0[0]:=compoundHistogram.B.percentile(0.1);
          p1[0]:=1/(compoundHistogram.B.percentile(99.9)-p0[0]);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=normValue(workflowImage[x,y]);
          if compoundHistogram.B.mightHaveOutOfBoundsValues then statisticColorOp;
          compoundHistogram.destroy;
        end;
        imt_normalizeGrey: begin
          compoundHistogram:=workflowImage.histogram;
          greyHist:=compoundHistogram.subjectiveGreyHistogram;
          p0:=greyHist.percentile(0.1)*white;
          p1[0]:=1/(greyHist.percentile(99.9)-p0[0]);
          for y:=0 to workflowImage.height-1 do for x:=0 to workflowImage.width-1 do workflowImage[x,y]:=(workflowImage[x,y]-p0)*p1[0];
          if greyHist.mightHaveOutOfBoundsValues then statisticColorOp;
          greyHist.destroy;
          compoundHistogram.destroy;
        end;
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
      imt_crop  : workflowImage.crop(param.i0,param.i1,param.i2,param.i3);
      imt_flip  : workflowImage.flip;
      imt_flop  : workflowImage.flop;
      imt_rotLeft : workflowImage.rotLeft;
      imt_rotRight: workflowImage.rotRight;
      imt_addRGB..imt_minOfFile: combine;
      imt_setColor, imt_setHue, imt_tint, imt_project, imt_limit,imt_limitLow,imt_grey,imt_sepia,imt_invert,imt_abs,imt_gamma,imt_gammaRGB,imt_gammaHSV: colorOp;
      imt_normalizeFull,imt_normalizeValue,imt_normalizeGrey:statisticColorOp;
    end;
  end;

FUNCTION T_imageManipulationStep.isValid: boolean;
  begin
    result:=valid;
  end;

FUNCTION T_imageManipulationStep.toString(CONST forProgress: boolean
  ): ansistring;
  begin
    result:=param.toString(tsm_withNiceParameterName);
    if forProgress and (length(result)>20) then result:=copy(result,1,17)+'...';
  end;

FUNCTION T_imageManipulationStep.getTodo(CONST stepIndexForStoringIntermediate: longint): P_imageManipulationStepToDo;
  begin
    new(result,create(@self,stepIndexForStoringIntermediate));
  end;

CONSTRUCTOR T_imageManipulationWorkflow.create;
  begin
    setLength(imageStash,0);
    setLength(step,0);
  end;

DESTRUCTOR T_imageManipulationWorkflow.destroy;
  begin
    clear;
  end;

PROCEDURE T_imageManipulationWorkflow.raiseError(CONST message: ansistring);
  begin
    progressQueue.logFractionDone(0,'Error: '+message);
    progressQueue.cancelCalculation(false);
    if displayErrorFunction<>nil
    then displayErrorFunction(message)
    else writeln(stderr,message);
    beep;
  end;

PROCEDURE T_imageManipulationWorkflow.clearIntermediate;
  VAR i:longint;
  begin
    for i:=0 to length(step)-1 do with step[i] do begin
      if intermediate<>nil then dispose(intermediate,destroy);
      intermediate:=nil;
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.storeIntermediate(CONST index: longint);
  begin
    if (index>=0) and (index<length(step)) then with step[index] do begin
      if intermediate<>nil then intermediate^.copyFromImage(workflowImage)
                           else new(intermediate,create(workflowImage));
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.clearStash;
  VAR i:longint;
  begin
    for i:=0 to length(imageStash)-1 do if imageStash[i]<>nil then dispose(imageStash[i],destroy);
    setLength(imageStash,0);
  end;

PROCEDURE T_imageManipulationWorkflow.execute(CONST previewMode: boolean);
  VAR i:longint;
  begin
    progressQueue.forceStart(et_commentedStepsOfVaryingCost_serial,length(step));
    clearIntermediate;
    clearStash;
    if previewMode then for i:=0 to length(step)-1 do progressQueue.enqueue(step[i].manipulation.getTodo( i))
                   else for i:=0 to length(step)-1 do progressQueue.enqueue(step[i].manipulation.getTodo(-1));
  end;

FUNCTION T_imageManipulationWorkflow.renderIntermediate(CONST index: longint; VAR target: TImage): boolean;
  begin
    if (index>=0) and (index<length(step)) and (step[index].intermediate<>nil) then begin
      step[index].intermediate^.copyToImage(target);
      result:=true;
    end else result:=false;
  end;

PROCEDURE T_imageManipulationWorkflow.clear;
  VAR i:longint;
  begin
    progressQueue.ensureStop;
    clearIntermediate;
    clearStash;
    for i:=0 to length(step)-1 do step[i].manipulation.destroy;
    setLength(step,0);
  end;

PROCEDURE T_imageManipulationWorkflow.addGenerationStep(CONST command: ansistring);
  VAR param:T_parameterValue;
  begin
    progressQueue.ensureStop;
    if not(isPlausibleSpecification(command)) then exit;
    setLength(step,length(step)+1);
    param.createFromValue(parameterDescription[imt_generateImage],command);
    step[length(step)-1].manipulation.create(imt_generateImage,param);
    step[length(step)-1].intermediate:=nil;
  end;

PROCEDURE T_imageManipulationWorkflow.addStep(CONST command: ansistring);
  begin
    setLength(step,length(step)+1);
    step[length(step)-1].manipulation.create(command);
    if step[length(step)-1].manipulation.isValid then begin
      step[length(step)-1].intermediate:=nil;
    end else begin
      step[length(step)-1].manipulation.destroy;
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
    end;
  end;

PROCEDURE T_imageManipulationWorkflow.mutateStep(CONST index: longint; CONST command: ansistring);
  VAR newManipulation:T_imageManipulationStep;
  begin
    if (index>=0) and (index<length(step)) then begin
      newManipulation.create(command);
      if newManipulation.isValid then with step[index] do begin
        progressQueue.ensureStop;
        manipulation.destroy;
        manipulation.create(command);
      end;
      newManipulation.destroy;
    end;
  end;

FUNCTION T_imageManipulationWorkflow.stepCount: longint;
  begin
    result:=length(step);
  end;

FUNCTION T_imageManipulationWorkflow.stepText(CONST index: longint): ansistring;
  begin
    result:=step[index].manipulation.toString();
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
      for i:=lowerIndex to length(step)-1 do with step[i] do
        if intermediate<>nil then begin;
          dispose(intermediate,destroy);
          intermediate:=nil;
        end;
    end;
  end;


INITIALIZATION
  initParameterDescriptions;
  progressQueue.create(@imageGeneration.progressQueue);
  workflowImage.create(1,1);

FINALIZATION
  progressQueue.destroy;
  workflowImage.destroy;

end.
