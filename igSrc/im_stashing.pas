UNIT im_stashing;
INTERFACE
IMPLEMENTATION
USES imageManipulation,imageContexts,myParams,mypics,myColors,math,myGenerics,pixMaps;
PROCEDURE stash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.stash.stashImage(parameters.fileName,context^.image);
  end;

PROCEDURE unstash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.stash.unstashImage(parameters.fileName,context^.image);
  end;

{$MACRO ON}
{$define pixelwiseCombination:=
VAR fromStash:P_rawImage;
    raw0,raw1:P_floatColor;
    k:longint;
begin
  fromStash:=context^.stash.getStashedImage(parameters.fileName);
  if fromStash<>nil then begin
    raw0:=context^.image.rawData;
    raw1:=fromStash^    .rawData;
    for k:=0 to min(context^.image.pixelCount,fromStash^.pixelCount)-1 do begin
      singlePixelOperation;
      inc(raw0); inc(raw1);
    end;
  end;
end}

PROCEDURE addStash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=raw0^:=raw0^+raw1^}
  pixelwiseCombination;

PROCEDURE subtractStash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=raw0^:=raw0^-raw1^}
  pixelwiseCombination;

PROCEDURE multiplyStash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=raw0^:=raw0^*raw1^}
  pixelwiseCombination;

PROCEDURE divideStash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=raw0^:=rgbDiv(raw0^,raw1^)}
  pixelwiseCombination;

PROCEDURE screenStash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=raw0^:=rgbScreen(raw0^,raw1^)}
  pixelwiseCombination;

PROCEDURE minStash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=raw0^:=rgbMin(raw0^,raw1^)}
  pixelwiseCombination;

PROCEDURE maxStash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  {$define singlePixelOperation:=raw0^:=rgbMax(raw0^,raw1^)}
  pixelwiseCombination;

PROCEDURE blurWithStash_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR kernels:array of T_arrayOfDouble;
      kernel:T_arrayOfDouble;
      temp:T_rawImage;
      ptmp,data:P_floatColor;
      x,y,z:longint;
      sum:T_rgbFloatColor;
      weight:double;
      fromStash:P_rawImage;

  FUNCTION getKernel(CONST relativeSigma:single):T_arrayOfDouble;
    VAR index:longint;
        i:longint;
    begin
      index:=round(255*relativeSigma);
      if index<0 then index:=0;
      i:=length(kernels);
      while i<=index do begin
        setLength(kernels,i+1);
        kernels[i]:=C_EMPTY_DOUBLE_ARRAY;
        inc(i);
      end;
      if length(kernels[index])=0 then begin
        if index>0 then kernels[index]:=getSmoothingKernel(index/25500*context^.image.diagonal)
                   else begin
                     setLength(kernels[index],1);
                     kernels[index][0]:=1;
                   end;
      end;
      result:=kernels[index];
    end;
  begin
    fromStash:=context^.stash.getStashedImage(parameters.fileName);
    if fromStash=nil then exit;
    if fromStash^.dimensions<>context^.image.dimensions then begin
      context^.messageQueue^.Post('Stash has wrong resolution');
      exit;
    end;
    //TODO: It might be more elegant to process the blurring in bulk
    setLength(kernels,0);
    temp.create(context^.image.dimensions.width,context^.image.dimensions.height);
    ptmp:=temp.rawData;
    data:=context^.image.rawData;
    //blur in x-direction:-----------------------------------------------
    for y:=0 to temp.dimensions.height-1 do for x:=0 to temp.dimensions.width-1 do begin
      kernel:=getKernel(fromStash^[x,y][cc_red]);
                                                                         sum:=    data[x+  y*temp.dimensions.width]*kernel[ 0]; weight:=       kernel[ 0];
      for z:=max(-x,1-length(kernel)) to -1    do                  begin sum:=sum+data[x+z+y*temp.dimensions.width]*kernel[-z]; weight:=weight+kernel[-z]; end;
      for z:=1 to min(temp.dimensions.width-x,length(kernel))-1 do begin sum:=sum+data[x+z+y*temp.dimensions.width]*kernel[ z]; weight:=weight+kernel[ z]; end;
      ptmp[x+y*temp.dimensions.width]:=sum*(1/weight);
    end;
    //-------------------------------------------------:blur in x-direction
    for x:=0 to length(kernels)-1 do setLength(kernels[x],0);
    setLength(kernels,0);
    //blur in y-direction:---------------------------------------------------
    for x:=0 to temp.dimensions.width-1 do for y:=0 to temp.dimensions.height-1 do begin
      kernel:=getKernel(fromStash^[x,y][cc_green]);
                                                                          sum:=    ptmp[x+   y *temp.dimensions.width]*kernel[ 0]; weight:=       kernel[ 0];
      for z:=max(-y,1-length(kernel)) to -1    do                   begin sum:=sum+ptmp[x+(z+y)*temp.dimensions.width]*kernel[-z]; weight:=weight+kernel[-z]; end;
      for z:=1 to min(temp.dimensions.height-y,length(kernel))-1 do begin sum:=sum+ptmp[x+(z+y)*temp.dimensions.width]*kernel[ z]; weight:=weight+kernel[ z]; end;
      data[x+y*temp.dimensions.width]:=sum*(1/weight);
    end;
    //-----------------------------------------------------:blur in y-direction
    temp.destroy;
    for x:=0 to length(kernels)-1 do setLength(kernels[x],0);
    setLength(kernels,0);
  end;

INITIALIZATION
  registerSimpleOperation(imc_imageAccess,
                          newParameterDescription('stash',pt_string)^.setDefaultValue('0'),
                          @stash_impl,
                          sok_writingStash);
  registerSimpleOperation(imc_imageAccess,
                          newParameterDescription('unstash',pt_string)^.setDefaultValue('0'),
                          @unstash_impl,
                          sok_restoringStash);
  registerSimpleOperation(imc_combination,
                          newParameterDescription('+stash',pt_string)^.setDefaultValue('0'),
                          @addStash_impl,
                          sok_combiningStash);
  registerSimpleOperation(imc_combination,
                          newParameterDescription('-stash',pt_string)^.setDefaultValue('0'),
                          @subtractStash_impl,
                          sok_combiningStash);
  registerSimpleOperation(imc_combination,
                          newParameterDescription('*stash',pt_string)^.setDefaultValue('0'),
                          @multiplyStash_impl,
                          sok_combiningStash);
  registerSimpleOperation(imc_combination,
                          newParameterDescription('/stash',pt_string)^.setDefaultValue('0'),
                          @divideStash_impl,
                          sok_combiningStash);
  registerSimpleOperation(imc_combination,
                          newParameterDescription('screenStash',pt_string)^.setDefaultValue('0'),
                          @screenStash_impl,
                          sok_combiningStash);
  registerSimpleOperation(imc_combination,
                          newParameterDescription('maxStash',pt_string)^.setDefaultValue('0'),
                          @maxStash_impl,
                          sok_combiningStash);
  registerSimpleOperation(imc_combination,
                          newParameterDescription('minStash',pt_string)^.setDefaultValue('0'),
                          @minStash_impl,
                          sok_combiningStash);
  registerSimpleOperation(imc_filter,
                          newParameterDescription('blurWithStash',pt_string)^.setDefaultValue('0'),
                          @blurWithStash_impl,
                          sok_combiningStash);
end.

