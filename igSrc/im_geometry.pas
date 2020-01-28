UNIT im_geometry;
INTERFACE
IMPLEMENTATION
USES imageManipulation,imageContexts,myParams,mypics,math,pixMaps;
FUNCTION targetDimensions(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow):T_imageDimensions;
  begin
    result:=context^.limitedDimensionsForResizeStep(imageDimensions(parameters.i0,parameters.i1));
  end;

PROCEDURE resize_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin context^.image.resize(targetDimensions(parameters,context),res_exact); end;

//TODO: Implement this:
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

INITIALIZATION
  registerSimpleOperation(imc_geometry,
                          resizeParameters('resize'),
                          @resize_impl,
                          sok_inputDependent);
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
                          newParameterDescription('crop', pt_4floats)^
                            .addChildParameterDescription(spa_f0,'relative x0',pt_float)^
                            .addChildParameterDescription(spa_f1,'relative x1',pt_float)^
                            .addChildParameterDescription(spa_f2,'relative y0',pt_float)^
                            .addChildParameterDescription(spa_f3,'relative y1',pt_float)^
                            .setDefaultValue('0:1x0:1'),
                          @crop_impl,
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
end.

