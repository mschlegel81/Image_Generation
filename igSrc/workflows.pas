UNIT workflows;
INTERFACE
USES myGenerics,
      myParams,mypics,sysutils,imageGeneration,ExtCtrls,mySys,FileUtil,Dialogs,
     generationBasics,
     imageContexts,workflowSteps;
CONST MAX_HEIGHT_OR_WIDTH=9999;
  //TODO: The following operations must be implemented:
  //imt_stashImage             imc_imageAccess
  //stepParamDescription[imt_stashImage]:=newParameterDescription('stash',pt_string, 0)^.setDefaultValue('0');
  //imt_unstashImage           imc_imageAccess
  //stepParamDescription[imt_unstashImage]:=newParameterDescription('unstash',pt_string, 0)^.setDefaultValue('0');
  //imt_resize                 imc_geometry
  //stepParamDescription[imt_resize]:=newParameterDescription('resize',pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .setDefaultValue('100x100');
  //imt_fit                    imc_geometry
  //stepParamDescription[imt_fit]:=newParameterDescription('fit',pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .setDefaultValue('100x100');
  //imt_fill                   imc_geometry
  //stepParamDescription[imt_fill]:=newParameterDescription('fill',pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .setDefaultValue('100x100');
  //imt_fitExpand              imc_geometry
  //stepParamDescription[imt_fitExpand]:=newParameterDescription('fitExpand',pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .setDefaultValue('100x100');
  //imt_fitRotate              imc_geometry
  //stepParamDescription[imt_fitRotate]:=newParameterDescription('fitRotate',pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .setDefaultValue('100x100');
  //imt_fillRotate             imc_geometry
  //stepParamDescription[imt_fillRotate]:=newParameterDescription('fillRotate',pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
  //  .setDefaultValue('100x100');
  //imt_crop                   imc_geometry
  //stepParamDescription[imt_crop]:=newParameterDescription('crop', pt_4floats)^
  //  .addChildParameterDescription(spa_f0,'relative x0',pt_float)^
  //  .addChildParameterDescription(spa_f1,'relative x1',pt_float)^
  //  .addChildParameterDescription(spa_f2,'relative y0',pt_float)^
  //  .addChildParameterDescription(spa_f3,'relative y1',pt_float)^
  //  .setDefaultValue('0:1x0:1');
  //imt_zoom                   imc_geometry
  //stepParamDescription[imt_zoom]:=newParameterDescription('zoom', pt_float)^
  //  .setDefaultValue('0.5');
  //imt_flip                   imc_geometry
  //stepParamDescription[imt_flip                ]:=newParameterDescription('flip',        pt_none);
  //imt_flop                   imc_geometry
  //stepParamDescription[imt_flop                ]:=newParameterDescription('flop',        pt_none);
  //imt_rotLeft                imc_geometry
  //stepParamDescription[imt_rotLeft             ]:=newParameterDescription('rotL',        pt_none);
  //imt_rotRight               imc_geometry
  //stepParamDescription[imt_rotRight            ]:=newParameterDescription('rotR',        pt_none);
  //imt_rotDegrees             imc_geometry
  //stepParamDescription[imt_rotDegrees          ]:=newParameterDescription('rotate',      pt_float,-3600,3600);
  //imt_addRGB                 imc_colors
  //stepParamDescription[imt_addRGB              ]:=newParameterDescription('+RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
  //imt_subtractRGB            imc_colors
  //stepParamDescription[imt_subtractRGB         ]:=newParameterDescription('-RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
  //imt_multiplyRGB            imc_colors
  //stepParamDescription[imt_multiplyRGB         ]:=newParameterDescription('*RGB',        pt_color)^.setDefaultValue('1')^.addRGBChildParameters;
  //imt_divideRGB              imc_colors
  //stepParamDescription[imt_divideRGB           ]:=newParameterDescription('/RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
  //imt_screenRGB              imc_colors
  //stepParamDescription[imt_screenRGB           ]:=newParameterDescription('screenRGB',   pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
  //imt_maxOfRGB               imc_colors
  //stepParamDescription[imt_maxOfRGB            ]:=newParameterDescription('maxRGB',      pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
  //imt_minOfRGB               imc_colors
  //stepParamDescription[imt_minOfRGB            ]:=newParameterDescription('minRGB',      pt_color)^.setDefaultValue('0')^.addRGBChildParameters;
  //imt_addHSV                 imc_colors
  //stepParamDescription[imt_addHSV              ]:=newParameterDescription('+HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
  //imt_subtractHSV            imc_colors
  //stepParamDescription[imt_subtractHSV         ]:=newParameterDescription('-HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
  //imt_multiplyHSV            imc_colors
  //stepParamDescription[imt_multiplyHSV         ]:=newParameterDescription('*HSV',        pt_3floats)^.setDefaultValue('1,1,1')^.addHSVChildParameters;
  //imt_divideHSV              imc_colors
  //stepParamDescription[imt_divideHSV           ]:=newParameterDescription('/HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
  //imt_screenHSV              imc_colors
  //stepParamDescription[imt_screenHSV           ]:=newParameterDescription('screenHSV',   pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
  //imt_maxOfHSV               imc_colors
  //stepParamDescription[imt_maxOfHSV            ]:=newParameterDescription('maxHSV',      pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
  //imt_minOfHSV               imc_colors
  //stepParamDescription[imt_minOfHSV            ]:=newParameterDescription('minHSV',      pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters;
  //imt_addStash               imc_combination
  //stepParamDescription[imt_addStash            ]:=newParameterDescription('+stash',      pt_string, 0)^.setDefaultValue('0');
  //imt_subtractStash          imc_combination
  //stepParamDescription[imt_subtractStash       ]:=newParameterDescription('-stash',      pt_string, 0)^.setDefaultValue('0');
  //imt_multiplyStash          imc_combination
  //stepParamDescription[imt_multiplyStash       ]:=newParameterDescription('*stash',      pt_string, 0)^.setDefaultValue('0');
  //imt_divideStash            imc_combination
  //stepParamDescription[imt_divideStash         ]:=newParameterDescription('/stash',      pt_string, 0)^.setDefaultValue('0');
  //imt_screenStash            imc_combination
  //stepParamDescription[imt_screenStash         ]:=newParameterDescription('screenStash', pt_string, 0)^.setDefaultValue('0');
  //imt_maxOfStash             imc_combination
  //stepParamDescription[imt_maxOfStash          ]:=newParameterDescription('maxStash',    pt_string, 0)^.setDefaultValue('0');
  //imt_minOfStash             imc_combination
  //stepParamDescription[imt_minOfStash          ]:=newParameterDescription('minStash',    pt_string, 0)^.setDefaultValue('0');
  //imt_setColor               imc_colors
  //stepParamDescription[imt_setColor            ]:=newParameterDescription('setRGB',      pt_color)^.setDefaultValue('0');
  //imt_tint                   imc_colors
  //stepParamDescription[imt_tint                ]:=newParameterDescription('tint',        pt_float)^.setDefaultValue('0');
  //imt_project                imc_colors
  //stepParamDescription[imt_project             ]:=newParameterDescription('project',     pt_none);
  //imt_limit                  imc_colors
  //stepParamDescription[imt_limit               ]:=newParameterDescription('limit',       pt_none);
  //imt_limitLow               imc_colors
  //stepParamDescription[imt_limitLow            ]:=newParameterDescription('limitLow',    pt_none);
  //imt_grey                   imc_colors
  //stepParamDescription[imt_grey                ]:=newParameterDescription('grey',        pt_none);
  //imt_sepia                  imc_colors
  //stepParamDescription[imt_sepia               ]:=newParameterDescription('sepia',       pt_none);
  //imt_invert                 imc_colors
  //stepParamDescription[imt_invert              ]:=newParameterDescription('invert',      pt_none);
  //imt_abs                    imc_colors
  //stepParamDescription[imt_abs                 ]:=newParameterDescription('abs',         pt_none);
  //imt_gamma                  imc_colors
  //stepParamDescription[imt_gamma               ]:=newParameterDescription('gamma',       pt_float,   1E-3)^.setDefaultValue('1.3');
  //imt_gammaRGB               imc_colors
  //stepParamDescription[imt_gammaRGB            ]:=newParameterDescription('gammaRGB',    pt_3floats, 1E-3)^.setDefaultValue('1.2,1.3,1.4')^.addRGBChildParameters;
  //imt_gammaHSV               imc_colors
  //stepParamDescription[imt_gammaHSV            ]:=newParameterDescription('gammaHSV',    pt_3floats, 1E-3)^.setDefaultValue('1.2,1.3,1.4');
  //imt_unitChannelSum         imc_colors
  //imt_normalizeFull          imc_statistic
  //imt_normalizeValue         imc_statistic
  //imt_normalizeGrey          imc_statistic
  //imt_compress               imc_statistic
  //imt_compressV              imc_statistic
  //imt_compressSat            imc_statistic
  //imt_mono                   imc_statistic
  //imt_quantize               imc_statistic
  //imt_shine                  imc_misc
  //imt_blur                   imc_filter
  //imt_lagrangeDiff           imc_filter
  //imt_radialBlur             imc_filter
  //imt_rotationalBlur         imc_filter
  //imt_blurWithStash          imc_filter
  //imt_sharpen                imc_filter
  //imt_edges                  imc_filter
  //imt_variance               imc_filter
  //imt_mode                   imc_filter
  //imt_median                 imc_filter
  //imt_pseudomedian           imc_filter
  //imt_sketch                 imc_misc
  //imt_drip                   imc_misc
  //imt_encircle               imc_misc
  //imt_encircleNeon           imc_misc
  //imt_spheres                imc_misc
  //imt_gradient               imc_filter
  //imt_direction              imc_filter
  //imt_details                imc_filter
  //imt_nlm                    imc_filter
  //imt_modMed                 imc_filter
  //imt_halftone               imc_filter
  //imt_dropAlpha              imc_misc
  //imt_retainAlpha            imc_misc
  //
  //
  //
  //stepParamDescription[imt_unitChannelSum      ]:=newParameterDescription('unitChannelSum',pt_none);
  //stepParamDescription[imt_normalizeFull       ]:=newParameterDescription('normalize',   pt_none);
  //stepParamDescription[imt_normalizeValue      ]:=newParameterDescription('normalizeV',  pt_none);
  //stepParamDescription[imt_normalizeGrey       ]:=newParameterDescription('normalizeG',  pt_none);
  //stepParamDescription[imt_compress            ]:=newParameterDescription('compress',pt_float,0)^.setDefaultValue('20');
  //stepParamDescription[imt_compressV           ]:=newParameterDescription('compress V',pt_float,0)^.setDefaultValue('20');
  //stepParamDescription[imt_compressSat         ]:=newParameterDescription('compress saturation',pt_float,0)^.setDefaultValue('20');
  //stepParamDescription[imt_mono                ]:=newParameterDescription('mono',        pt_integer)^.setDefaultValue('10')^.addChildParameterDescription(spa_i0,'Color count',pt_integer,1,255);
  //stepParamDescription[imt_quantize            ]:=newParameterDescription('quantize',    pt_integer)^.setDefaultValue('16')^.addChildParameterDescription(spa_i0,'Color count',pt_integer,2,255);
  //stepParamDescription[imt_shine               ]:=newParameterDescription('shine',       pt_none);
  //stepParamDescription[imt_blur                ]:=newParameterDescription('blur',        pt_floatOr2Floats,0)^.setDefaultValue('0.2')^.addChildParameterDescription(spa_f0,'x',pt_float)^.addChildParameterDescription(spa_f1,'y',pt_float);
  //stepParamDescription[imt_lagrangeDiff        ]:=newParameterDescription('lagrangeDiff',pt_2floats,0)^.setDefaultValue('0.1,0.1')^.addChildParameterDescription(spa_f0,'scanScale',pt_float,0,1)^.addChildParameterDescription(spa_f1,'blurScale',pt_float,0,1);
  //stepParamDescription[imt_radialBlur          ]:=newParameterDescription('radialBlur'  ,pt_3floats)^.setDefaultValue('1,0,0');
  //stepParamDescription[imt_rotationalBlur      ]:=newParameterDescription('rotationalBlur',pt_3floats)^.setDefaultValue('1,0,0');
  //stepParamDescription[imt_blurWithStash       ]:=newParameterDescription('blurWithStash',pt_string,0)^.setDefaultValue('0');
  //stepParamDescription[imt_sharpen             ]:=newParameterDescription('sharpen'     ,pt_2floats,0)^
  //.addChildParameterDescription(spa_f0,'scale',pt_float,0,1)^
  //.addChildParameterDescription(spa_f1,'amount',pt_float,0)^
  //.setDefaultValue('0.1,0.5');
  //stepParamDescription[imt_edges               ]:=newParameterDescription('edges' ,pt_none);
  //stepParamDescription[imt_variance]:=newParameterDescription('variance',pt_float,0)^.setDefaultValue('0.05')^.addChildParameterDescription(spa_f0,'scale',pt_float);
  //stepParamDescription[imt_median]:=newParameterDescription('median',pt_float,0)^.setDefaultValue('0.05')^.addChildParameterDescription(spa_f0,'scale',pt_float);
  //stepParamDescription[imt_pseudomedian]:=newParameterDescription('pseudoMedian',pt_2floats,0)^
  //  .addChildParameterDescription(spa_f0,'rel. sigma',pt_float,0)^
  //  .addChildParameterDescription(spa_f1,'param',pt_float)^
  //  .setDefaultValue('0.1,1');
  //stepParamDescription[imt_mode]:=newParameterDescription('mode',pt_float,0)^.setDefaultValue('0.05')^.addChildParameterDescription(spa_f0,'scale',pt_float);
  //stepParamDescription[imt_sketch]:=newParameterDescription('sketch',pt_4floats)^
  //  .setDefaultValue('1,0.1,0.8,0.2')^
  //  .addChildParameterDescription(spa_f0,'cover'          ,pt_float,0)^
  //  .addChildParameterDescription(spa_f1,'direction sigma',pt_float,0)^
  //  .addChildParameterDescription(spa_f2,'density'        ,pt_float)^
  //  .addChildParameterDescription(spa_f3,'tolerance'      ,pt_float,0);
  //stepParamDescription[imt_drip]:=newParameterDescription('drip',pt_2floats,0,1)^
  //  .setDefaultValue('0.1,0.01')^
  //  .addChildParameterDescription(spa_f0,'diffusiveness',pt_float,0,1)^
  //  .addChildParameterDescription(spa_f1,'range' ,pt_float,0,1);
  //stepParamDescription[imt_encircle]:=newParameterDescription('encircle',pt_1I2F,0)^
  //  .setDefaultValue('2000,0.5,0.2')^
  //  .addChildParameterDescription(spa_i0,'circle count',pt_integer,1,100000)^
  //  .addChildParameterDescription(spa_f1,'opacity' ,pt_float,0,1)^
  //  .addChildParameterDescription(spa_f2,'circle size' ,pt_float,0);
  //stepParamDescription[imt_encircleNeon]:=newParameterDescription('encircleNeon',pt_1I2F,0)^
  //  .setDefaultValue('2000,0.5,0.2')^
  //  .addChildParameterDescription(spa_i0,'circle count',pt_integer,1,100000)^
  //  .addChildParameterDescription(spa_f1,'opacity' ,pt_float,0,1)^
  //  .addChildParameterDescription(spa_f2,'circle size' ,pt_float,0);
  //stepParamDescription[imt_spheres]:=newParameterDescription('spheres',pt_2I2F,0)^
  //  .setDefaultValue('2000,3,0.2,0.001')^
  //  .addChildParameterDescription(spa_i0,'sphere count',pt_integer,1,100000)^
  //  .addChildParameterDescription(spa_i1,'sphere style',pt_integer,0,3)^
  //  .addChildParameterDescription(spa_f2,'max size' ,pt_float,0,1)^
  //  .addChildParameterDescription(spa_f3,'min size' ,pt_float,0,1);
  //stepParamDescription[imt_gradient]:=newParameterDescription('gradient',pt_float,0)^.setDefaultValue('0.1');
  //stepParamDescription[imt_direction]:=newParameterDescription('direction',pt_float,0)^.setDefaultValue('0.1');
  //stepParamDescription[imt_details]:=newParameterDescription('details',pt_float,0)^.setDefaultValue('0.1');
  //stepParamDescription[imt_nlm]:=newParameterDescription('nlm',pt_1I1F,0)^
  //  .setDefaultValue('3,0.5')^
  //  .addChildParameterDescription(spa_i0,'scan radius (pixels)',pt_integer,1,10)^
  //  .addChildParameterDescription(spa_f1,'sigma',pt_float,0.001,2);
  //stepParamDescription[imt_modMed]:=newParameterDescription('modMed',pt_none);
  //stepParamDescription[imt_halftone]:=newParameterDescription('halftone',pt_1I1F)^
  //  .setDefaultValue('0,0.2')^
  //  .addChildParameterDescription(spa_i0,'style',pt_integer,0,7)^
  //  .addChildParameterDescription(spa_f1,'scale',pt_float,0);
  //stepParamDescription[imt_retainAlpha]:=newParameterDescription('retainAlpha',pt_color);
  //stepParamDescription[imt_dropAlpha]:=newParameterDescription('dropAlpha',pt_color);
  //PROCEDURE T_imageManipulationStep.execute(CONST previewMode,retainStashesAfterLastUse: boolean; CONST context:P_imageGenerationContext);
  //
  //  FUNCTION plausibleResolution:boolean;
  //    begin
  //      if (param.i0>0) and (param.i0<10000) and (param.i1>0) and (param.i1<10000) then result:=true
  //      else begin
  //        result:=false;
  //        context^.raiseError('Invalid resolution; Both values must be in range 1..'+intToStr(MAX_HEIGHT_OR_WIDTH));
  //      end;
  //    end;
  //
  //  FUNCTION rgbMult  (CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin initialize(result);  for i in RGB_CHANNELS do result[i]:=a[i]*b[i]; end;
  //  FUNCTION rgbMax   (CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin initialize(result);  for i in RGB_CHANNELS do if a[i]>b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
  //  FUNCTION rgbMin   (CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin initialize(result);  for i in RGB_CHANNELS do if a[i]<b[i] then result[i]:=a[i] else result[i]:=b[i]; end;
  //  PROCEDURE combine;
  //    FUNCTION rgbDiv   (CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin initialize(result); for i in RGB_CHANNELS do result[i]:=a[i]/b[i]; end;
  //    FUNCTION rgbScreen(CONST a,b:T_rgbFloatColor):T_rgbFloatColor; inline; VAR i:T_colorChannel; begin initialize(result); for i in RGB_CHANNELS do result[i]:=1-(1-a[i])*(1-b[i]); end;
  //    CONST RGB_OF:array[hc_hue..hc_value] of T_colorChannel=(cc_red,cc_green,cc_blue);
  //    FUNCTION hsvPlus  (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin initialize(result); for i in HSV_CHANNELS do result[i]:=a[i]+b[RGB_OF[i]]; end;
  //    FUNCTION hsvMinus (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin initialize(result); for i in HSV_CHANNELS do result[i]:=a[i]-b[RGB_OF[i]]; end;
  //    FUNCTION hsvMult  (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin initialize(result); for i in HSV_CHANNELS do result[i]:=a[i]*b[RGB_OF[i]]; end;
  //    FUNCTION hsvDiv   (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin initialize(result); for i in HSV_CHANNELS do result[i]:=a[i]/b[RGB_OF[i]]; end;
  //    FUNCTION hsvScreen(CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin initialize(result); for i in HSV_CHANNELS do result[i]:=1-(1-a[i])*(1-b[RGB_OF[i]]); end;
  //    FUNCTION hsvMax   (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin initialize(result); for i in HSV_CHANNELS do if a[i]>b[RGB_OF[i]] then result[i]:=a[i] else result[i]:=b[RGB_OF[i]]; end;
  //    FUNCTION hsvMin   (CONST a:T_hsvColor; CONST b:T_rgbFloatColor):T_hsvColor; inline; VAR i:T_hsvChannel; begin initialize(result); for i in HSV_CHANNELS do if a[i]<b[RGB_OF[i]] then result[i]:=a[i] else result[i]:=b[RGB_OF[i]]; end;
  //    VAR k:longint;
  //        other:P_rawImage=nil;
  //        rawPixels,otherPixels:P_floatColor;
  //        c1:T_rgbFloatColor;
  //        disposeOther:boolean=false;
  //    begin
  //      rawPixels:=context^.workflowImage.rawData;
  //      case imageManipulationType of
  //        imt_addStash..imt_minOfStash,imt_blurWithStash:
  //          begin
  //            //TODO: Determine if stashed image can be disposed
  //            other:=context^.getStashedImage(param.fileName);
  //            if other=nil then exit;
  //          end;
  //        imt_addRGB..imt_minOfRGB,imt_addHSV..imt_minOfHSV: begin
  //          c1:=param.color;
  //          case imageManipulationType of
  //            imt_addRGB      : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=          rawPixels[k]+c1;
  //            imt_subtractRGB : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=          rawPixels[k]-c1;
  //            imt_multiplyRGB : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbMult  (rawPixels[k],c1);
  //            imt_divideRGB   : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbDiv   (rawPixels[k],c1);
  //            imt_screenRGB   : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbScreen(rawPixels[k],c1);
  //            imt_maxOfRGB    : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbMax   (rawPixels[k],c1);
  //            imt_minOfRGB    : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbMin   (rawPixels[k],c1);
  //            imt_addHSV      : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=hsvPlus  (rawPixels[k],c1);
  //            imt_subtractHSV : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=hsvMinus (rawPixels[k],c1);
  //            imt_multiplyHSV : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=hsvMult  (rawPixels[k],c1);
  //            imt_divideHSV   : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=hsvDiv   (rawPixels[k],c1);
  //            imt_screenHSV   : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=hsvScreen(rawPixels[k],c1);
  //            imt_maxOfHSV    : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=hsvMax   (rawPixels[k],c1);
  //            imt_minOfHSV    : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=hsvMin   (rawPixels[k],c1);
  //          end;
  //          //TODO: Handle disposing of other
  //          exit;
  //        end;
  //      end;
  //      otherPixels:=other^.rawData;
  //      case imageManipulationType of
  //        imt_addStash     : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=          rawPixels[k]+otherPixels[k];
  //        imt_subtractStash: for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=          rawPixels[k]-otherPixels[k];
  //        imt_multiplyStash: for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbMult  (rawPixels[k],otherPixels[k]);
  //        imt_divideStash  : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbDiv   (rawPixels[k],otherPixels[k]);
  //        imt_screenStash  : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbScreen(rawPixels[k],otherPixels[k]);
  //        imt_maxOfStash   : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbMax   (rawPixels[k],otherPixels[k]);
  //        imt_minOfStash   : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbMin   (rawPixels[k],otherPixels[k]);
  //        imt_blurWithStash: context^.workflowImage.blurWith(other^);
  //      end;
  //      //TODO: Determine if stashed image can be disposed
  //      if disposeOther and (other<>nil) then dispose(other,destroy);
  //    end;
  //
  //  PROCEDURE colorOp;
  //    VAR k:longint;
  //        rawPixels:P_floatColor;
  //    FUNCTION unitChannelSum(CONST col:T_rgbFloatColor):T_rgbFloatColor;
  //      VAR sum:double=0;
  //          c:T_colorChannel;
  //      begin
  //        initialize(result);
  //        for c in RGB_CHANNELS do sum:=sum+col[c];
  //        if sum=0 then begin
  //          for c in RGB_CHANNELS do result[c]:=1/3;
  //        end else begin
  //          sum:=1/sum;
  //          for c in RGB_CHANNELS do result[c]:=sum*col[c];
  //        end;
  //      end;
  //
  //    begin
  //      rawPixels:=context^.workflowImage.rawData;
  //      case imageManipulationType of
  //        imt_setColor: for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=param.color;
  //        imt_tint:     for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=tint(rawPixels[k],param.f0);
  //        imt_project:  for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=projectedColor(rawPixels[k]);
  //        imt_limit:    for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbMax(BLACK,rgbMin(WHITE,rawPixels[k]));
  //        imt_limitLow: for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=rgbMax(BLACK,             rawPixels[k] );
  //        imt_grey    : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=subjectiveGrey(rawPixels[k]);
  //        imt_sepia   : for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=         sepia(rawPixels[k]);
  //        imt_invert:   for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=        invert(rawPixels[k]);
  //        imt_abs:      for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=        absCol(rawPixels[k]);
  //        imt_gamma:    for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=         gamma(rawPixels[k],param.f0,param.f0,param.f0);
  //        imt_gammaRGB: for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=         gamma(rawPixels[k],param.f0,param.f1,param.f2);
  //        imt_gammaHSV: for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=      gammaHSV(rawPixels[k],param.f0,param.f1,param.f2);
  //        imt_unitChannelSum: for k:=0 to context^.workflowImage.pixelCount-1 do rawPixels[k]:=unitChannelSum(rawPixels[k]);
  //      end;
  //    end;
  //
  //  PROCEDURE statisticColorOp;
  //    VAR compoundHistogram:T_compoundHistogram;
  //        greyHist:T_histogram;
  //        p0,p1:T_rgbFloatColor;
  //        i:longint;
  //        k:longint=0;
  //        tempHsv:T_hsvColor;
  //        raw:P_floatColor;
  //
  //    FUNCTION normValue(CONST c:T_hsvColor):T_hsvColor;
  //      begin
  //        result:=c;
  //        result[hc_value]:=(result[hc_value]-p0[cc_red])*p1[cc_red];
  //      end;
  //
  //    FUNCTION measure(CONST a,b:single):single;
  //      CONST a0=1/0.998;
  //            b0= -0.001;
  //      begin result:=sqr(a0-a)/3+(a0-a+b0-b)*(b0-b); end;
  //
  //    FUNCTION measure(CONST a,b:T_rgbFloatColor):single;
  //      begin
  //        result:=(measure(a[cc_red  ],b[cc_red  ])*SUBJECTIVE_GREY_RED_WEIGHT+
  //                 measure(a[cc_green],b[cc_green])*SUBJECTIVE_GREY_GREEN_WEIGHT+
  //                 measure(a[cc_blue ],b[cc_blue ])*SUBJECTIVE_GREY_BLUE_WEIGHT);
  //      end;
  //
  //    begin
  //      raw:=context^.workflowImage.rawData;
  //      case imageManipulationType of
  //        imt_normalizeFull: while k<4 do begin
  //          compoundHistogram:=context^.workflowImage.histogram;
  //          compoundHistogram.R.getNormalizationParams(p0[cc_red  ],p1[cc_red  ]);
  //          compoundHistogram.G.getNormalizationParams(p0[cc_green],p1[cc_green]);
  //          compoundHistogram.B.getNormalizationParams(p0[cc_blue ],p1[cc_blue ]);
  //          {$ifdef DEBUG} writeln('Normalization with parameters ',p0[cc_red],' ',p1[cc_red],'; measure:',measure(p0,p1)); {$endif}
  //          for i:=0 to context^.workflowImage.pixelCount-1 do raw[i]:=rgbMult(raw[i]-p0,p1);
  //          if (compoundHistogram.mightHaveOutOfBoundsValues or (measure(p0,p1)>1)) and not(context^.queue^.cancellationRequested) then inc(k) else k:=4;
  //          compoundHistogram.destroy;
  //        end;
  //        imt_normalizeValue: while k<4 do begin
  //          compoundHistogram:=context^.workflowImage.histogramHSV;
  //          compoundHistogram.B.getNormalizationParams(p0[cc_red],p1[cc_red]);
  //          for i:=0 to context^.workflowImage.pixelCount-1 do raw[i]:=normValue(raw[i]);
  //          if (compoundHistogram.B.mightHaveOutOfBoundsValues or (measure(p0[cc_red],p1[cc_red])>1)) and not(context^.queue^.cancellationRequested) then inc(k) else k:=4;
  //          compoundHistogram.destroy;
  //        end;
  //        imt_normalizeGrey: while k<4 do begin
  //          compoundHistogram:=context^.workflowImage.histogram;
  //          greyHist:=compoundHistogram.subjectiveGreyHistogram;
  //          greyHist.getNormalizationParams(p0[cc_red],p1[cc_red]);
  //          p0:=WHITE*p0[cc_red];
  //          for i:=0 to context^.workflowImage.pixelCount-1 do raw[i]:=(raw[i]-p0)*p1[cc_red];
  //          if (greyHist.mightHaveOutOfBoundsValues or (measure(p0[cc_red],p1[cc_red])>1)) and not(context^.queue^.cancellationRequested) then inc(k) else k:=4;
  //          greyHist.destroy;
  //          compoundHistogram.destroy;
  //        end;
  //        imt_compress: begin
  //          compoundHistogram:=context^.workflowImage.histogram;
  //          greyHist:=compoundHistogram.sumHistorgram;
  //          greyHist.smoothen(param.f0);
  //          for i:=0 to context^.workflowImage.pixelCount-1 do raw[i]:=greyHist.lookup(raw[i]);
  //          greyHist.destroy;
  //          compoundHistogram.destroy;
  //        end;
  //        imt_compressV: begin
  //          compoundHistogram:=context^.workflowImage.histogramHSV;
  //          greyHist:=compoundHistogram.B;
  //          greyHist.smoothen(param.f0);
  //          for i:=0 to context^.workflowImage.pixelCount-1 do begin
  //            tempHsv:=raw[i];
  //            tempHsv[hc_value]:=greyHist.lookup(tempHsv[hc_value]);
  //            raw[i]:=tempHsv;
  //          end;
  //          compoundHistogram.destroy;
  //        end;
  //        imt_compressSat: begin
  //          compoundHistogram:=context^.workflowImage.histogramHSV;
  //          greyHist:=compoundHistogram.G;
  //          greyHist.smoothen(param.f0);
  //          for i:=0 to context^.workflowImage.pixelCount-1 do begin
  //            tempHsv:=raw[i];
  //            tempHsv[hc_saturation]:=greyHist.lookup(tempHsv[hc_saturation]);
  //            raw[i]:=tempHsv;
  //          end;
  //          compoundHistogram.destroy;
  //        end;
  //      end;
  //    end;
  //
  //  PROCEDURE monochrome;
  //    VAR i:longint;
  //        l:T_colorChannel;
  //        k:longint=0;
  //        cSum:T_rgbFloatColor=(0,0,0);
  //        c:T_rgbFloatColor;
  //        g,invG:double;
  //        raw:P_floatColor;
  //    begin
  //      raw:=context^.workflowImage.rawData;
  //      for i:=0 to context^.workflowImage.pixelCount-1 do begin
  //        c:=raw[i];
  //        g:=greyLevel(c);
  //        if g>1E-3 then begin
  //          invG:=1/g;
  //          for l in RGB_CHANNELS do cSum[l]:=cSum[l]+c[l]*invG;
  //          inc(k);
  //        end;
  //        c[cc_red]:=g;
  //        raw[i]:=c;
  //      end;
  //      invG:=1/k;
  //      for l in RGB_CHANNELS do cSum[l]:=cSum[l]*invG;
  //      for i:=0 to context^.workflowImage.pixelCount-1 do begin
  //        c:=raw[i];
  //        g:=round(c[cc_red]*param.i0)/param.i0;
  //        for l in RGB_CHANNELS do c[l]:=g*cSum[l];
  //        raw[i]:=c;
  //      end;
  //    end;
  //
  //  PROCEDURE redefine(newImage:T_rawImage);
  //    begin
  //      context^.workflowImage.copyFromPixMap(newImage);
  //      newImage.destroy;
  //    end;
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
      config:T_imageGenerationContextConfiguration;
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

      FUNCTION isValid: boolean; virtual;
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
      FUNCTION addStep(CONST specification:string):boolean;
      PROCEDURE addStep(CONST operation:P_imageOperation);
      PROCEDURE swapStepDown(CONST index:longint);
      PROCEDURE removeStep(CONST index:longint);
  end;

  { T_generateImageWorkflow }

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
      FUNCTION startEditing(CONST stepIndex:longint):boolean;
      PROPERTY algorithmIndex:longint read getAlgorithmIndex write setAlgorithmIndex;
      PROPERTY algoritm:P_algorithmMeta read current;
      PROCEDURE startEditingForNewStep;
      PROCEDURE confirmEditing;
      FUNCTION isValid: boolean; virtual;
  end;

IMPLEMENTATION
USES ig_gradient,
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
     imageManipulation,
     myStringUtil;

PROCEDURE T_generateImageWorkflow.beforeAll;
  begin
    enterCriticalSection(contextCS);
    enterCriticalSection(relatedEditor^.contextCS);
    try
      image.resize(relatedEditor^.config.initialResolution,res_dataResize);
      if (editingStep>0) and (editingStep+1<relatedEditor^.stepCount) and (relatedEditor^.step[editingStep-1]^.outputImage<>nil) then begin
        image.copyFromPixMap(relatedEditor^.step[editingStep-1]^.outputImage^);
        relatedEditor^.config.limitImageSize(image);
      end;
    finally
      leaveCriticalSection(contextCS);
      leaveCriticalSection(relatedEditor^.contextCS);
    end;
  end;

PROCEDURE T_generateImageWorkflow.headlessWorkflowExecution;
  VAR stepStarted:double;
  begin
    stepStarted:=now;
    current^.prototype^.execute(@self);
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
      relatedEditor^.addStep(current^.prototype^.toString(tsm_forSerialization));
    end else begin
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
PROCEDURE T_simpleWorkflow.afterStep(CONST stepIndex: longint; CONST elapsed: double);
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

CONSTRUCTOR T_simpleWorkflow.createSimpleWorkflow(CONST messageQueue_: P_structuredMessageQueue);
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

PROCEDURE T_simpleWorkflow.appendSaveStep(CONST savingToFile: string; CONST savingWithSizeLimit: longint);
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

FUNCTION T_editorWorkflow.addStep(CONST specification: string): boolean;
  VAR newStep:P_workflowStep;
  begin
    enterCriticalSection(contextCS);
    try
      new(newStep,create(specification));
      if newStep^.isValid then begin
        setLength(steps,length(steps)+1);
        steps[length(steps)-1]:=newStep;
      end else messageQueue^.Post('Invalid step was rejected: '+specification,true);
    finally
      leaveCriticalSection(contextCS);
    end;
  end;

PROCEDURE T_editorWorkflow.addStep(CONST operation: P_imageOperation);
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

FUNCTION T_simpleWorkflow.proposedImageFileName(CONST resString: ansistring): string;
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
