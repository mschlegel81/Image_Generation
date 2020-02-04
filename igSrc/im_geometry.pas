UNIT im_geometry;
INTERFACE
USES pixMaps,imageManipulation;
CONST OP_NAME_CROP='crop';

TYPE
T_cropMeta=object(T_simpleImageOperationMeta)
  public
    CONSTRUCTOR create;
    FUNCTION getOperationToCrop(CONST x0,x1,y0,y1:double):P_simpleImageOperation;
end;

VAR cropMeta:^T_cropMeta;
FUNCTION canParseResolution(CONST s:string; OUT dim:T_imageDimensions):boolean;
IMPLEMENTATION
USES imageContexts,myParams,mypics,math;
VAR pd_resize:P_parameterDescription=nil;

FUNCTION canParseResolution(CONST s: string; OUT dim: T_imageDimensions): boolean;
  VAR p:T_parameterValue;
  begin
    p.createToParse(pd_resize,s);
    dim:=imageDimensions(p.i0,p.i1);
    result:=p.isValid;
  end;

FUNCTION targetDimensions(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow):T_imageDimensions;
  begin
    result:=context^.limitedDimensionsForResizeStep(imageDimensions(parameters.i0,parameters.i1));
  end;

PROCEDURE resize_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.resize(targetDimensions(parameters,context),res_exact); end;

PROCEDURE fit_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.resize(targetDimensions(parameters,context),res_fit); end;

PROCEDURE fill_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.resize(targetDimensions(parameters,context),res_cropToFill); end;

PROCEDURE fitExpand_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.resize(targetDimensions(parameters,context),res_fitExpand); end;

PROCEDURE fitRotate_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.resize(targetDimensions(parameters,context),res_fitRotate); end;

PROCEDURE fillRotate_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.resize(targetDimensions(parameters,context),res_cropRotate); end;

PROCEDURE crop_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.crop(parameters.f0,parameters.f1,parameters.f2,parameters.f3); end;

PROCEDURE zoom_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.zoom(parameters.f0); end;

PROCEDURE flip_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.flip; end;

PROCEDURE flop_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.flop; end;

PROCEDURE rotL_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.rotLeft; end;

PROCEDURE rotR_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.rotRight; end;

PROCEDURE rotDegrees_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.rotate(parameters.f0); end;

FUNCTION resizeParameters(CONST name:string):P_parameterDescription;
  begin
    result:=newParameterDescription(name,pt_2integers, 1, MAX_HEIGHT_OR_WIDTH)^
           .addChildParameterDescription(spa_i0,'width',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
           .addChildParameterDescription(spa_i1,'height',pt_integer,1,MAX_HEIGHT_OR_WIDTH)^
           .setDefaultValue('100x100');
  end;

CONSTRUCTOR T_cropMeta.create;
begin
  inherited create(imc_geometry,
                   newParameterDescription(OP_NAME_CROP, pt_4floats)^
                     .addChildParameterDescription(spa_f0,'relative x0',pt_float)^
                     .addChildParameterDescription(spa_f1,'relative x1',pt_float)^
                     .addChildParameterDescription(spa_f2,'relative y0',pt_float)^
                     .addChildParameterDescription(spa_f3,'relative y1',pt_float)^
                     .setDefaultValue('0:1x0:1'),
                   @crop_impl,
                   sok_inputDependent)
end;

FUNCTION T_cropMeta.getOperationToCrop(CONST x0,x1,y0,y1:double): P_simpleImageOperation;
  VAR value:T_parameterValue;
      op:P_simpleImageOperation;
  begin
    value.createFromValue(signature,x0,x1,y0,y1);
    new(op,create(@self,value));
    result:=op;
  end;

INITIALIZATION
  pd_resize:=
  registerSimpleOperation(imc_geometry,
                          resizeParameters('resize'),
                          @resize_impl,
                          sok_inputDependent)^.getSimpleParameterDescription;
  registerSimpleOperation(imc_geometry,
                          resizeParameters('fit'),
                          @fit_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          resizeParameters('fill'),
                          @fill_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          resizeParameters('fitExpand'),
                          @fitExpand_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          resizeParameters('fitRotate'),
                          @fitRotate_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          resizeParameters('fillRotate'),
                          @fillRotate_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          newParameterDescription('zoom', pt_float)^.setDefaultValue('0.5'),
                          @zoom_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          newParameterDescription('flip', pt_none),
                          @flip_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          newParameterDescription('flop', pt_none),
                          @flop_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          newParameterDescription('rotL', pt_none),
                          @rotL_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          newParameterDescription('rotR', pt_none),
                          @rotR_impl,
                          sok_inputDependent);
  registerSimpleOperation(imc_geometry,
                          newParameterDescription('rotate',pt_float,-3600,3600)^.setDefaultValue('45'),
                          @rotDegrees_impl,
                          sok_inputDependent);
  new(cropMeta,create);
  registerOperation(cropMeta);
end.

