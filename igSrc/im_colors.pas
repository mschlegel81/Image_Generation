UNIT im_colors;
INTERFACE
IMPLEMENTATION
USES imageManipulation,imageContexts,myParams,mypics,myColors,math;

{$MACRO ON}
{$define genericColorOperation:=
VAR p:P_floatColor;
    k:longint;
    col:T_rgbFloatColor;
begin
  col:=parameters.color;
  p:=context^.image.rawData;
  for k:=0 to context^.image.pixelCount-1 do begin
    singlePixelOperation;
    inc(p);
  end;
end}

PROCEDURE addRGB_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^+=col}
  genericColorOperation;

PROCEDURE subtractRGB_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^-=col}
  genericColorOperation;

PROCEDURE multiplyRGB_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^*=col}
  genericColorOperation;

PROCEDURE divideRGB_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=rgbDiv(p^,col)}
  genericColorOperation;

PROCEDURE screenRGB_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=rgbScreen(p^,col)}
  genericColorOperation;

PROCEDURE maxOfRGB_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=rgbMax(p^,col)}
  genericColorOperation;

PROCEDURE minOfRGB_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=rgbMin(p^,col)}
  genericColorOperation;

PROCEDURE setColor_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=col}
  genericColorOperation;

PROCEDURE extractChannel_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  FUNCTION extractChannel(CONST col,pixel:T_rgbFloatColor):T_rgbFloatColor; inline;
    VAR sum:double=0;
       c:T_colorChannel;
    begin
      for c in RGB_CHANNELS do sum+=pixel[c]*col[c];
      for c in RGB_CHANNELS do result[c]:=sum;
    end;
  {$define singlePixelOperation:=p^:=extractChannel(p^,col)}
  genericColorOperation;

{$define genericColorOperation:=
VAR p:P_floatColor;
    k:longint;
    col,pixel:T_hsvColor;
    c:T_hsvChannel;
begin
  col[hc_hue       ]:=parameters.f0;
  col[hc_saturation]:=parameters.f1;
  col[hc_value     ]:=parameters.f2;
  p:=context^.image.rawData;
  for k:=0 to context^.image.pixelCount-1 do begin
    pixel:=p^;
    singlePixelOperation;
    p^:=pixel;
    inc(p);
  end;
end}

PROCEDURE addHSV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=for c in HSV_CHANNELS do pixel[c]:=pixel[c]+col[c]}
  genericColorOperation;

PROCEDURE subtractHSV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=for c in HSV_CHANNELS do pixel[c]:=pixel[c]-col[c]}
  genericColorOperation;

PROCEDURE multiplyHSV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=for c in HSV_CHANNELS do pixel[c]:=pixel[c]*col[c]}
  genericColorOperation;

PROCEDURE divideHSV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=for c in HSV_CHANNELS do pixel[c]:=pixel[c]/col[c]}
  genericColorOperation;

PROCEDURE screenHSV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=for c in HSV_CHANNELS do pixel[c]:=1-(1-pixel[c])*(1-col[c])}
  genericColorOperation;

PROCEDURE maxOfHSV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=for c in HSV_CHANNELS do pixel[c]:=max(pixel[c],col[c])}
  genericColorOperation;

PROCEDURE minOfHSV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=for c in HSV_CHANNELS do pixel[c]:=min(pixel[c],col[c])}
  genericColorOperation;

{$define genericColorOperation:=
VAR p:P_floatColor;
    k:longint;
begin
  p:=context^.image.rawData;
  for k:=0 to context^.image.pixelCount-1 do begin
    singlePixelOperation;
    inc(p);
  end;
end}

PROCEDURE tint_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=tint(p^,parameters.f0)}
  genericColorOperation;

PROCEDURE project_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=projectedColor(p^)}
  genericColorOperation;

PROCEDURE limit_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=rgbMax(BLACK,rgbMin(WHITE,p^))}
  genericColorOperation;

PROCEDURE limitLow_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=rgbMax(BLACK,p^)}
  genericColorOperation;

PROCEDURE grey_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=subjectiveGrey(p^)}
  genericColorOperation;

PROCEDURE sepia_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=sepia(p^)}
  genericColorOperation;

PROCEDURE invert_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=invert(p^)}
  genericColorOperation;

PROCEDURE abs_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=absCol(p^)}
  genericColorOperation;

PROCEDURE gamma_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=gamma(p^,parameters.f0,parameters.f0,parameters.f0)}
  genericColorOperation;

PROCEDURE gammaRGB_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=gamma(p^,parameters.f0,parameters.f1,parameters.f2)}
  genericColorOperation;

PROCEDURE gammaHSV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=gammaHSV(p^,parameters.f0,parameters.f1,parameters.f2)}
  genericColorOperation;

FUNCTION unitChannelSum(CONST col:T_rgbFloatColor):T_rgbFloatColor; inline;
  VAR sum:double=0;
      c:T_colorChannel;
  begin
    initialize(result);
    for c in RGB_CHANNELS do sum:=sum+col[c];
    if sum=0 then begin
      for c in RGB_CHANNELS do result[c]:=1/3;
    end else begin
      sum:=1/sum;
      for c in RGB_CHANNELS do result[c]:=sum*col[c];
    end;
  end;

PROCEDURE unitChannelSum_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=p^:=unitChannelSum(p^)}
  genericColorOperation;

PROCEDURE dropAlpha_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.rgbaSplit(parameters.color).destroy;
  end;

PROCEDURE retainAlpha_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR temp:T_rawImage;
  begin
    temp:=context^.image.rgbaSplit(parameters.color);
    context^.image.copyFromPixMap(temp);
    temp.destroy;
  end;

INITIALIZATION
  registerSimpleOperation(imc_colors,newParameterDescription('+RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters,@addRGB_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('-RGB',        pt_color)^.setDefaultValue('0')^.addRGBChildParameters,@subtractRGB_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('*RGB',        pt_color)^.setDefaultValue('1')^.addRGBChildParameters,@multiplyRGB_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('/RGB',        pt_color)^.setDefaultValue('1')^.addRGBChildParameters,@divideRGB_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('screenRGB',   pt_color)^.setDefaultValue('0')^.addRGBChildParameters,@screenRGB_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('maxRGB',      pt_color)^.setDefaultValue('0')^.addRGBChildParameters,@maxOfRGB_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('minRGB',      pt_color)^.setDefaultValue('0')^.addRGBChildParameters,@minOfRGB_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('+HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters,@addHSV_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('-HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters,@subtractHSV_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('*HSV',        pt_3floats)^.setDefaultValue('1,1,1')^.addHSVChildParameters,@multiplyHSV_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('/HSV',        pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters,@divideHSV_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('screenHSV',   pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters,@screenHSV_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('maxHSV',      pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters,@maxOfHSV_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('minHSV',      pt_3floats)^.setDefaultValue('0,0,0')^.addHSVChildParameters,@minOfHSV_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('setRGB',      pt_color)^.setDefaultValue('0')^.addRGBChildParameters,@setColor_impl,sok_inputIndependent);
  registerSimpleOperation(imc_colors,newParameterDescription('tint',        pt_float)^.setDefaultValue('0'),@tint_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('project',     pt_none),@project_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('limit',       pt_none),@limit_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('limitLow',    pt_none),@limitLow_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('grey',        pt_none),@grey_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('sepia',       pt_none),@sepia_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('invert',      pt_none),@invert_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('abs',         pt_none),@abs_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('gamma',       pt_float,   1E-3)^.setDefaultValue('1.3'),@gamma_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('gammaRGB',    pt_3floats, 1E-3)^.setDefaultValue('1.2,1.3,1.4')^.addRGBChildParameters,@gammaRGB_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('gammaHSV',    pt_3floats, 1E-3)^.setDefaultValue('1.2,1.3,1.4'),@gammaHSV_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('unitChannelSum',pt_none),@unitChannelSum_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('extractChannel',pt_color)^.setDefaultValue('1,0,0')^.addRGBChildParameters,@extractChannel_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('dropAlpha'  ,pt_color)^.setDefaultValue('0')^.addRGBChildParameters,@dropAlpha_impl);
  registerSimpleOperation(imc_colors,newParameterDescription('retainAlpha',pt_color)^.setDefaultValue('0')^.addRGBChildParameters,@retainAlpha_impl);
end.

