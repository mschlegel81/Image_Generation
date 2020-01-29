UNIT im_statisticOperations;
INTERFACE

IMPLEMENTATION
USES imageManipulation,imageContexts,myParams,mypics,myColors,math;
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

PROCEDURE normalizeFull_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR k:longint=0;
      i:longint;
      compoundHistogram:T_compoundHistogram;
      p0,p1:T_rgbFloatColor;
      raw:P_floatColor;
  begin
    raw:=context^.image.rawData;
    while k<4 do begin
      compoundHistogram:=context^.image.histogram;
      compoundHistogram.R.getNormalizationParams(p0[cc_red  ],p1[cc_red  ]);
      compoundHistogram.G.getNormalizationParams(p0[cc_green],p1[cc_green]);
      compoundHistogram.B.getNormalizationParams(p0[cc_blue ],p1[cc_blue ]);
      for i:=0 to context^.image.pixelCount-1 do raw[i]:=(raw[i]-p0)*p1;
      if (compoundHistogram.mightHaveOutOfBoundsValues or (measure(p0,p1)>1)) and not(context^.cancellationRequested) then inc(k) else k:=4;
      compoundHistogram.destroy;
    end;
  end;
PROCEDURE normalizeValue_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
  //while k<4 do begin
  //          compoundHistogram:=context^.workflowImage.histogramHSV;
  //          compoundHistogram.B.getNormalizationParams(p0[cc_red],p1[cc_red]);
  //          for i:=0 to context^.workflowImage.pixelCount-1 do raw[i]:=normValue(raw[i]);
  //          if (compoundHistogram.B.mightHaveOutOfBoundsValues or (measure(p0[cc_red],p1[cc_red])>1)) and not(context^.queue^.cancellationRequested) then inc(k) else k:=4;
  //          compoundHistogram.destroy;
  //        end;
  end;

PROCEDURE normalizeGrey_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    //while k<4 do begin
      //          compoundHistogram:=context^.workflowImage.histogram;
      //          greyHist:=compoundHistogram.subjectiveGreyHistogram;
      //          greyHist.getNormalizationParams(p0[cc_red],p1[cc_red]);
      //          p0:=WHITE*p0[cc_red];
      //          for i:=0 to context^.workflowImage.pixelCount-1 do raw[i]:=(raw[i]-p0)*p1[cc_red];
      //          if (greyHist.mightHaveOutOfBoundsValues or (measure(p0[cc_red],p1[cc_red])>1)) and not(context^.queue^.cancellationRequested) then inc(k) else k:=4;
      //          greyHist.destroy;
      //          compoundHistogram.destroy;
      //        end;
  end;
PROCEDURE compress_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
      //          compoundHistogram:=context^.workflowImage.histogram;
      //          greyHist:=compoundHistogram.sumHistorgram;
      //          greyHist.smoothen(param.f0);
      //          for i:=0 to context^.workflowImage.pixelCount-1 do raw[i]:=greyHist.lookup(raw[i]);
      //          greyHist.destroy;
      //          compoundHistogram.destroy;
      //        end;
  end;
PROCEDURE compressV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    //          compoundHistogram:=context^.workflowImage.histogramHSV;
    //          greyHist:=compoundHistogram.B;
    //          greyHist.smoothen(param.f0);
    //          for i:=0 to context^.workflowImage.pixelCount-1 do begin
    //            tempHsv:=raw[i];
    //            tempHsv[hc_value]:=greyHist.lookup(tempHsv[hc_value]);
    //            raw[i]:=tempHsv;
    //          end;
    //          compoundHistogram.destroy;

  end;
PROCEDURE compressSat_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    //          compoundHistogram:=context^.workflowImage.histogramHSV;
    //          greyHist:=compoundHistogram.G;
    //          greyHist.smoothen(param.f0);
    //          for i:=0 to context^.workflowImage.pixelCount-1 do begin
    //            tempHsv:=raw[i];
    //            tempHsv[hc_saturation]:=greyHist.lookup(tempHsv[hc_saturation]);
    //            raw[i]:=tempHsv;
    //          end;
    //          compoundHistogram.destroy;

  end;

PROCEDURE mono_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR i:longint;
      l:T_colorChannel;
      k:longint=0;
      cSum:T_rgbFloatColor=(0,0,0);
      c:T_rgbFloatColor;
      g,invG:double;
      raw:P_floatColor;
  begin
    raw:=context^.image.rawData;
    for i:=0 to context^.image.pixelCount-1 do begin
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
    for i:=0 to context^.image.pixelCount-1 do begin
      c:=raw[i];
      g:=round(c[cc_red]*parameters.i0)/parameters.i0;
      for l in RGB_CHANNELS do c[l]:=g*cSum[l];
      raw[i]:=c;
    end;
  end;

PROCEDURE quantize_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR i:longint;
      tree:T_colorTree;
      raw:P_floatColor;
  begin
    raw:=context^.image.rawData;
    tree.create;
    for i:=0 to context^.image.pixelCount-1 do tree.addSample(raw[i]);
    tree.finishSampling(parameters.i0);
    for i:=0 to context^.image.pixelCount-1 do raw[i]:=tree.getQuantizedColor(raw[i]);
    tree.destroy;
  end;

//TODO: Implement custom quantization
// Color Mode: 0 - as before
//             1 - histogram based, convex
//             2 - bounding box based, convex
//             3 - fixed
// Dither Mode: 0 - none
//              1 - textbook
//              2 - line based
//              3 - grid based
PROCEDURE quantizeCustom_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR colorTable:array of T_rgbFloatColor;
  FUNCTION nearestColor(CONST pixel:T_rgbFloatColor):T_rgbFloatColor;
    VAR k      :longint;
        kBest  :longint=-1;
        dist   :double;
        minDist:double=infinity;
    begin
      for k:=0 to length(colorTable)-1 do begin
        dist:=colDiff(colorTable[k],pixel);
        if dist<minDist then begin
          minDist:=dist;
          kBest  :=k;
        end;
      end;
      result:=colorTable[k];
    end;

  begin

  end;

INITIALIZATION
registerSimpleOperation(imc_statistic,newParameterDescription('normalize',   pt_none),@normalizeFull_impl);
registerSimpleOperation(imc_statistic,newParameterDescription('normalizeV',  pt_none),@normalizeValue_impl);
registerSimpleOperation(imc_statistic,newParameterDescription('normalizeG',  pt_none),@normalizeGrey_impl);
registerSimpleOperation(imc_statistic,newParameterDescription('compress',pt_float,0)^.setDefaultValue('20'),@compress_impl);
registerSimpleOperation(imc_statistic,newParameterDescription('compress V',pt_float,0)^.setDefaultValue('20'),@compressV_impl);
registerSimpleOperation(imc_statistic,newParameterDescription('compress saturation',pt_float,0)^.setDefaultValue('20'),@compressSat_impl);
registerSimpleOperation(imc_statistic,newParameterDescription('mono',        pt_integer)^.setDefaultValue('10')^.addChildParameterDescription(spa_i0,'Color count',pt_integer,1,255),@mono_impl);
registerSimpleOperation(imc_statistic,newParameterDescription('quantize',    pt_integer)^.setDefaultValue('16')^.addChildParameterDescription(spa_i0,'Color count',pt_integer,2,255),@quantize_impl);
//registerSimpleOperation(imc_statistic,newParameterDescription('quantize',    pt_3integers)^.setDefaultValue('16,0,0')
//                                                                                        ^.addChildParameterDescription(spa_i0,'Color count',pt_integer,2,255)
//                                                                                        ^.addChildParameterDescription(spa_i1,'Color mode',pt_integer,0,3)
//                                                                                        ^.addChildParameterDescription(spa_i2,'Dither mode',pt_integer,0,3),@quantize_impl);

end.

