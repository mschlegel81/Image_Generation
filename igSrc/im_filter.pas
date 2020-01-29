UNIT im_filter;
INTERFACE

IMPLEMENTATION
USES imageManipulation,imageContexts,myParams,mypics,myColors,math,myGenerics,pixMaps;
PROCEDURE blur_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.blur(parameters.f0,parameters.f1);
  end;

PROCEDURE lagrangeDiff_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.lagrangeDiffusion(parameters.f0,parameters.f1);
  end;

PROCEDURE radialBlur_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.radialBlur(parameters.f0,parameters.f1,parameters.f2);
  end;

PROCEDURE rotationalBlur_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.rotationalBlur(parameters.f0,parameters.f1,parameters.f2);
  end;

PROCEDURE sharpen_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.sharpen(parameters.f0,parameters.f1);
  end;

PROCEDURE edges_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.prewittEdges;
  end;

PROCEDURE variance_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.variance(parameters.f0);
  end;

PROCEDURE mode_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.modalFilter(parameters.f0);
  end;

PROCEDURE median_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.medianFilter(parameters.f0);
  end;

PROCEDURE pseudoMedian_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.myFilter(parameters.f0,parameters.f1);
  end;

PROCEDURE direction_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR updated:T_rawImage;
  begin
    updated:=context^.image.directionMap(parameters.f0);
    context^.image.copyFromPixMap(updated);
    updated.destroy;
  end;

PROCEDURE details_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR temp:T_rawImage;
      i:longint;
      raw:P_floatColor;
  begin
    temp.create(context^.image);
    temp.blur(parameters.f0,parameters.f0);
    raw:=context^.image.rawData;
    for i:=0 to temp.pixelCount-1 do raw[i]:=raw[i]-temp.rawData[i];
    temp.destroy;
  end;

TYPE
P_nlmWorkerThreadTodo=^T_nlmWorkerThreadTodo;
T_nlmWorkerThreadTodo=object(T_parallelTask)
  scanRadius:longint;
  sigma:double;
  pIn:P_floatColor;
  y0:longint;
  expLUT:array[0..31] of double;
  CONSTRUCTOR create(CONST scanRadius_:longint;
    CONST sigma_:double;
    CONST input_:P_floatColor;
    CONST y_:longint);
  PROCEDURE execute; virtual;
end;

CONSTRUCTOR T_nlmWorkerThreadTodo.create(CONST scanRadius_:longint;
  CONST sigma_:double;
  CONST input_:P_floatColor;
  CONST y_:longint);
  VAR i:longint;
  begin
    scanRadius:=scanRadius_;
    sigma     :=sigma_;
    pIn       :=input_;
    y0        :=y_;
    for i:=0 to length(expLUT)-1 do expLUT[i]:=exp(-i*0.5/sigma);
  end;

PROCEDURE T_nlmWorkerThreadTodo.execute;
  VAR dim:T_imageDimensions;
  FUNCTION patchDistF(x0,y0,x1,y1:longint):double; inline;
    CONST PATCH_KERNEL:array[-2..2,-2..2] of double=
          (( 6.517, 9.095,10.164, 9.095, 6.517),
           ( 9.095,12.693,14.184,12.693, 9.095),
           (10.164,14.184, 0.0  ,14.184,10.164),
           ( 9.095,12.693,14.184,12.693, 9.095),
           ( 6.517, 9.095,10.164, 9.095, 6.517));
    VAR dx,dy:longint;
        c0,c1:T_rgbFloatColor;
        i:longint;
    begin
      result:=0;
      for dy:=max(-2,max(-y0,-y1)) to min(2,dim.height-1-max(y0,y1)) do
      for dx:=max(-2,max(-x0,-x1)) to min(2,dim.width -1-max(x0,x1)) do begin
        c0:=pIn[x0+dx+(y0+dy)*dim.width];
        c1:=pIn[x1+dx+(y1+dy)*dim.width];
        result:=result+(sqr(c0[cc_red  ]-c1[cc_red  ])
                       +sqr(c0[cc_green]-c1[cc_green])
                       +sqr(c0[cc_blue ]-c1[cc_blue ]))*PATCH_KERNEL[dy,dx];
      end;
      if isInfinite(result) or isNan(result) then exit(0);
      i:=round(result);
      if i<0 then i:=0 else if i>=length(expLUT) then i:=length(expLUT)-1;
      result:=expLUT[i];
    end;

  FUNCTION filteredColorAtF(x,y:longint):T_rgbFloatColor;
    VAR w,wTot,wMax:double;
        dx,dy:longint;
    begin
      wTot:=0;
      wMax:=0;
      result:=myColors.BLACK;
      for dy:=max(-scanRadius,-y) to min(scanRadius,dim.height-1-y) do
      for dx:=max(-scanRadius,-x) to min(scanRadius,dim.width -1-x) do
      if (dx<1-scanRadius) or (dx>scanRadius-1) or (dy<1-scanRadius) or (dy>scanRadius-1) then
      begin
        w:=patchDistF(x,y,x+dx,y+dy);
        if w>wMax then wMax:=w;
        result:=result+pIn[x+dx+(y+dy)*dim.width]*w;
        wTot  :=wTot  +                           w;
      end;
      result:=result+pIn[x+y*dim.width]*wMax;
      wTot  :=wTot  +                   wMax;
      if wTot<1E-5 then result:=pIn[x+y*dim.width]
                   else result:=result*(1/wTot);
    end;
  VAR y,x:longint;
  begin
    dim:=containedIn^.image.dimensions;
    for y:=y0 to min(y0+15,dim.height-1) do
    for x:=0 to dim.width-1 do
      containedIn^.image[x,y]:=filteredColorAtF(x,y);
  end;

PROCEDURE nlm_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR temp :T_rawImage;
      pIn  :P_floatColor;
      y:longint;
      task:P_nlmWorkerThreadTodo;
  begin
    if parameters.f1<1E-10 then exit;
    temp.create(context^.image);
    pIn:=temp.rawData;
    for y:=0 to temp.dimensions.height-1 do if y and 15=0 then begin
      task:=nil;
      new(task,create(parameters.i0,parameters.f1,pIn,y));
      context^.enqueue(task);
    end;
    context^.waitForFinishOfParallelTasks;
    temp.destroy;
  end;

PROCEDURE shine_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.shine;
  end;

INITIALIZATION
  registerSimpleOperation(imc_filter,newParameterDescription('blur',pt_floatOr2Floats,0)
                                       ^.setDefaultValue('0.2')
                                       ^.addChildParameterDescription(spa_f0,'x',pt_float)
                                       ^.addChildParameterDescription(spa_f1,'y',pt_float),@blur_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('lagrangeDiff',pt_2floats,0)
                                       ^.setDefaultValue('0.1,0.1')
                                       ^.addChildParameterDescription(spa_f0,'scanScale',pt_float,0,1)
                                       ^.addChildParameterDescription(spa_f1,'blurScale',pt_float,0,1),@lagrangeDiff_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('radialBlur'  ,pt_3floats)
                                       ^.setDefaultValue('1,0,0'),@radialBlur_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('rotationalBlur',pt_3floats)
                                       ^.setDefaultValue('1,0,0'),@rotationalBlur_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('sharpen'     ,pt_2floats,0)
                                       ^.addChildParameterDescription(spa_f0,'scale',pt_float,0,1)
                                       ^.addChildParameterDescription(spa_f1,'amount',pt_float,0)
                                       ^.setDefaultValue('0.1,0.5'),@sharpen_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('edges' ,pt_none),@edges_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('variance',pt_float,0)
                                       ^.setDefaultValue('0.05')
                                       ^.addChildParameterDescription(spa_f0,'scale',pt_float),@variance_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('mode',pt_float,0)
                                       ^.setDefaultValue('0.05')
                                       ^.addChildParameterDescription(spa_f0,'scale',pt_float),@mode_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('median',pt_float,0)
                                       ^.setDefaultValue('0.05')
                                       ^.addChildParameterDescription(spa_f0,'scale',pt_float),@median_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('pseudoMedian',pt_2floats,0)
                                       ^.addChildParameterDescription(spa_f0,'rel. sigma',pt_float,0)
                                       ^.addChildParameterDescription(spa_f1,'param',pt_float)
                                       ^.setDefaultValue('0.1,1'),@pseudoMedian_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('direction',pt_float,0)
                                       ^.setDefaultValue('0.1'),@direction_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('details',pt_float,0)
                                       ^.setDefaultValue('0.1'),@details_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('nlm',pt_1I1F,0)
                                       ^.setDefaultValue('3,0.5')
                                       ^.addChildParameterDescription(spa_i0,'scan radius (pixels)',pt_integer,1,10)
                                       ^.addChildParameterDescription(spa_f1,'sigma',pt_float,0.001,2),@nlm_impl);
  registerSimpleOperation(imc_filter,newParameterDescription('shine',pt_none),@shine_impl);
end.

