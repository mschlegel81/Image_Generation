UNIT im_statisticOperations;
INTERFACE

IMPLEMENTATION
USES imageManipulation,imageContexts,myParams,mypics,myColors,math,myGenerics;
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
    i:=parameters.i0; //pro forma; to use parameters
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
  VAR k:longint=0;
      compoundHistogram:T_compoundHistogram;
      raw:P_floatColor;
      i:longint;
      offset,stretch:single;
  FUNCTION normValue(CONST c:T_hsvColor):T_hsvColor; inline;
    begin
      result:=c;
      result[hc_value]:=(result[hc_value]-offset)*stretch;
    end;

  begin
    i:=parameters.i0; //pro forma; to use parameters
    raw:=context^.image.rawData;
    while k<4 do begin
      compoundHistogram:=context^.image.histogramHSV;
      compoundHistogram.B.getNormalizationParams(offset,stretch);
      for i:=0 to context^.image.pixelCount-1 do raw[i]:=normValue(raw[i]);
      if (compoundHistogram.B.mightHaveOutOfBoundsValues or (measure(offset,stretch)>1)) and not(context^.cancellationRequested) then inc(k) else k:=4;
      compoundHistogram.destroy;
    end;
  end;

PROCEDURE normalizeGrey_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR k:longint=0;
      i:longint;
      compoundHistogram:T_compoundHistogram;
      greyHist:T_histogram;
      raw:P_floatColor;
      offset:T_rgbFloatColor;
      stretch:single;
  begin
    i:=parameters.i0; //pro forma; to use parameters
    raw:=context^.image.rawData;
    while k<4 do begin
      compoundHistogram:=context^.image.histogram;
      greyHist:=compoundHistogram.subjectiveGreyHistogram;
      greyHist.getNormalizationParams(offset[cc_red],stretch);
      offset:=WHITE*offset[cc_red];
      for i:=0 to context^.image.pixelCount-1 do raw[i]:=(raw[i]-offset)*stretch;
      if (greyHist.mightHaveOutOfBoundsValues or (measure(offset[cc_red],stretch)>1)) and not(context^.cancellationRequested) then inc(k) else k:=4;
      greyHist.destroy;
      compoundHistogram.destroy;
    end;
  end;

PROCEDURE compress_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR i:longint;
      compoundHistogram:T_compoundHistogram;
      greyHist:T_histogram;
      raw:P_floatColor;
  begin
    raw:=context^.image.rawData;
    compoundHistogram:=context^.image.histogram;
    greyHist:=compoundHistogram.sumHistorgram;
    greyHist.smoothen(parameters.f0);
    for i:=0 to context^.image.pixelCount-1 do raw[i]:=greyHist.lookup(raw[i]);
    greyHist.destroy;
    compoundHistogram.destroy;
  end;

PROCEDURE compressV_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR i:longint;
      compoundHistogram:T_compoundHistogram;
      greyHist:T_histogram;
      raw:P_floatColor;
      tempHsv:T_hsvColor;
  begin
    raw:=context^.image.rawData;
    compoundHistogram:=context^.image.histogramHSV;
    greyHist:=compoundHistogram.B;
    greyHist.smoothen(parameters.f0);
    for i:=0 to context^.image.pixelCount-1 do begin
      tempHsv:=raw[i];
      tempHsv[hc_value]:=greyHist.lookup(tempHsv[hc_value]);
      raw[i]:=tempHsv;
    end;
    compoundHistogram.destroy;
  end;

PROCEDURE compressSat_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  VAR i:longint;
      compoundHistogram:T_compoundHistogram;
      greyHist:T_histogram;
      raw:P_floatColor;
      tempHsv:T_hsvColor;
  begin
    raw:=context^.image.rawData;
    compoundHistogram:=context^.image.histogramHSV;
    greyHist:=compoundHistogram.G;
    greyHist.smoothen(parameters.f0);
    for i:=0 to context^.image.pixelCount-1 do begin
      tempHsv:=raw[i];
      tempHsv[hc_saturation]:=greyHist.lookup(tempHsv[hc_saturation]);
      raw[i]:=tempHsv;
    end;
    compoundHistogram.destroy;
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

PROCEDURE quantizeCustom_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  CONST DEFAULT_COLOR_TABLE:array[0..255] of T_rgbFloatColor=(
(0,0,0),(1,1,1),(0,0,1),(0,1,0),(1,0,0),(1,1,0),(1,0,1),(0,1,1),
(0.5,0.5,0.5),(0.25,0.25,0.75),(0.75,0.75,0.75),(0,0.6875,0),(1,0.3125,0),(0.4375,0.875,0),(0.5625,0.125,0),(1,0.5625,0),
(0,0.4375,0),(0.0625,0.8125,1),(0.9375,0.1875,1),(0,0.5625,1),(1,0.4375,1),(0.5,0,0.8125),(0.5,1,0.8125),(0.6875,0.3125,1),
(0.3125,0.6875,1),(0.5625,0.625,0),(0.0625,0.125,0.5),(0.4375,0.375,0),(0.9375,0.875,0.5),(0.8125,0.625,1),(0.1875,0.375,1),(0,0.3125,0),
(1,0.6875,0),(0.625,0.25,0),(0.375,0.75,0),(0,0.875,0),(1,0.125,0),(0.8125,0.4375,0),(0.1875,0.5625,0),(0.5625,0.1875,0.9375),
(0.4375,0.8125,0.9375),(0.3125,0.0625,0.25),(0.6875,0.9375,0.25),(0.25,0.9375,0.5625),(0.75,0.0625,0.5625),(0.25,0.1875,0),(0.75,0.8125,0),(0.3125,0.5625,0.9375),
(0,0.6875,0.9375),(1,0.3125,0.9375),(0.6875,0.4375,0.9375),(0.375,1,0),(0.625,0,0),(0.3125,0.125,1),(0.6875,0.875,1),(0.75,0.5625,0.4375),
(0.25,0.4375,0.4375),(0.1875,0.8125,0.1875),(0.8125,0.1875,0.1875),(0,0.3125,0.875),(1,0.6875,0.875),(1,0.5625,0.875),(0,0.4375,0.875),(0.4375,0.3125,0.5625),
(0.5625,0.6875,0.5625),(0,0.1875,1),(1,0.8125,1),(0.125,0.625,0.5),(0.875,0.375,0.5),(0.8125,0.25,0.6875),(0.1875,0.75,0.6875),(0.5,0.625,1),
(0.0625,0.5,0.4375),(0.9375,0.5,0.4375),(0.5,0.375,1),(0.75,0.3125,0.25),(0.25,0.6875,0.25),(1,0.625,0.4375),(0,0.25,0.4375),(0,0.375,0.4375),
(0.75,0.125,1),(0.25,0.875,1),(1,0.75,0.4375),(0.25,0,0.625),(0.75,1,0.625),(0,0.9375,0.4375),(1,0.0625,0.4375),(0.25,0.3125,0.0625),
(0.75,0.6875,0.0625),(0,0.1875,0),(0.75,0,1),(0.25,1,1),(1,0.8125,0),(0.125,0.0625,0.875),(0.5625,0.4375,0.125),(0.4375,0.5625,0.125),
(0.875,0.9375,0.875),(0.375,0.25,0.125),(0.625,0.75,0.125),(0.6875,0.5,0),(0.3125,0.5,0),(0,0.875,0.75),(0.5,0.8125,0.25),(0.3125,0.625,0),
(0.625,1,0),(0.5,0.1875,0.25),(0.6875,0.375,0),(1,0.125,0.75),(0.375,0,0),(0.5,0.25,0.75),(0.0625,0.0625,0.1875),(0.5625,0.9375,1),
(0.9375,0.9375,0.1875),(0.5,0.75,0.75),(0.4375,0.0625,1),(0.125,0.75,0),(0.875,0.25,0),(0.375,0.4375,1),(0.625,0.5625,1),(0.1875,0.5,1),
(0.8125,0.5,1),(0.8125,0,0.375),(0.375,0.125,0.375),(0.1875,0.1875,0.625),(0.5,0.875,0.625),(0.8125,0.8125,0.625),(0.1875,1,0.375),(0.1875,0.375,0),
(0.8125,0.625,0),(0.1875,0.875,0.375),(0.25,0.3125,0.9375),(0.75,0.6875,0.9375),(0.6875,0.875,0.0625),(0.4375,0.9375,0.1875),(0.8125,0.125,0.375),(0.5625,0.0625,0.1875),
(1,0.4375,0.3125),(0,0.5625,0.3125),(0.5625,0.125,0.6875),(1,0.25,1),(0,0.75,1),(0.25,0.625,1),(0.75,0.375,1),(0.1875,0.125,0),
(0.3125,0.375,0.5),(0.6875,0.625,0.5),(1,0.375,0),(0,0.625,1),(1,0.375,1),(0,0.625,0),(0.4375,0.6875,0),(0.5625,0.3125,0),
(0.1875,0.9375,0),(1,0.1875,0.4375),(0.9375,0.75,1),(0.9375,0.0625,1),(0.0625,0.25,1),(0.8125,0.0625,0),(0.0625,0.9375,1),(0,0.8125,0.4375),
(0.25,0.8125,0.75),(0.75,0.1875,0.75),(0.375,0.625,0.5625),(0.875,0.75,0),(0.625,0.375,0.5625),(0.125,0.25,0),(0.375,0.1875,0.8125),(0.625,0.8125,0.75),
(0.375,0.4375,0),(0.625,0.5625,0),(0,0.75,0.4375),(1,0.25,0.4375),(0.1875,0,0.0625),(0.625,0.5,0.9375),(0.3125,0.5,0.5625),(0.8125,1,0.0625),
(0.1875,0.4375,1),(0.375,0.9375,1),(0.8125,0.5625,1),(1,0.625,1),(1,0.5,1),(0,0.5,1),(0,0.375,1),(0.625,0.0625,1),
(0,0.125,1),(0,0.125,0),(0.75,0.5,0.5),(0.0625,0,0.5),(0.875,0.875,0),(0.9375,1,0.5),(1,0.875,1),(0.4375,0.5,1),
(0.5,0.5625,0.625),(0.125,0.3125,0.4375),(0.1875,0.5625,0.5625),(0.4375,0.4375,0.5),(0.8125,0.4375,0.5625),(0.875,0.6875,0.5),(0.5,0.5,0),(0,0.1875,0.5),
(1,0.8125,0.5),(1,0,0.5),(0,1,0.5),(0.5,1,0.3125),(0.125,0.6875,0.625),(0.3125,0.75,1),(0.625,0,0.5),(0.875,0.3125,0.625),
(0.6875,0.25,1),(0,0.5,0),(0.8125,0.5625,0),(0.9375,0.5625,0.4375),(0.75,0.875,0.5625),(0.375,0.6875,0.5625),(0.5,0.6875,1),(0.5,0.3125,1),
(0.0625,0.4375,0.4375),(0.875,0.5,0),(0.1875,0.4375,0),(0.625,0.3125,0.5625),(0.625,0.1875,0.5),(0.3125,0.0625,0.75),(0.5,0,0.25),(0,0.0625,0.625),
(0.625,0.1875,0),(0.5625,0.9375,0.5),(1,0.9375,0.625),(0.375,0.8125,0.5),(0.625,0.75,1),(0.3125,0.75,0.4375),(0.6875,0.25,0.4375),(0.1875,0.125,0.75),
(0.375,0.8125,0),(0.6875,0.9375,0.75),(0.375,1,0.5625),(0.375,0.25,1),(0.4375,0.0625,0.5),(1,0.0625,0),(0,0.375,0),(1,0.625,0),
(0,0.8125,0),(1,0.1875,0),(0,0.9375,0),(0.625,0.4375,0.5625),(0.75,0.8125,1),(0.125,0.1875,0.25),(0.3125,0.1875,0.375),(0.1875,0,1),
(0.375,0.875,0.375),(0.1875,0.0625,0.5),(0.125,1,0.75),(0.8125,0.75,0.375),(0.8125,0.9375,0.5),(0.75,0.125,0),(0.1875,0.25,0.375),(0.875,0,0.75));
  CONST BOUNDS_COLOR_TABLE:array[0..255] of T_rgbFloatColor=(
  (0,0,0),(1,1,1),(0,0,1),(0,1,0),(1,0,0),(1,1,0),(1,0,1),(0,1,1),
(0.5,0.5,1),(0.5,0.75,0),(0.5,0.25,0),(1,0.625,0.5),(0,0.375,0.5),(0,0.625,0.5),(1,0.375,0.5),(0.5,0.875,1),
(0.5,0.125,1),(0.5,0,0.16666666666666666),(0.5,1,0.16666666666666666),(0.16666666666666666,0.75,1),(0.83333333333333337,0.25,1),(0.83333333333333337,0.75,1),(0.16666666666666666,0.5,0),(0.16666666666666666,0.25,1),
(0.83333333333333337,0.5,0),(1,0.875,0.5),(1,0.125,0.5),(0,0.875,0.5),(0,0.125,0.5),(0.5,0.625,1),(0.5,0.375,1),(0,0.5,1),
(1,0.5,1),(0,0.75,0),(1,0.75,0),(1,0.25,0),(0,0.25,0),(0.6666666666666666,0.875,0),(0.6666666666666666,0.125,0),(0.6666666666666666,0.625,0),
(0.6666666666666666,0.375,0),(0.5,0.25,1),(0.5,0.75,1),(0.5,0.5,0),(0.3333333333333333,0.625,0),(0.3333333333333333,0.375,0),(0.3333333333333333,0.125,0),(0.3333333333333333,0.875,0),
(0.6666666666666666,0,1),(0.6666666666666666,1,1),(0.3333333333333333,0,1),(0.3333333333333333,1,1),(0.83333333333333337,1,0.5),(0.83333333333333337,0.875,1),(0.83333333333333337,0.375,1),(0.83333333333333337,0.125,1),
(0.83333333333333337,0,0.5),(0.16666666666666666,1,0.5),(0.16666666666666666,0.625,1),(0.83333333333333337,0.625,1),(0.16666666666666666,0,0.5),(0.16666666666666666,0.125,1),(0.16666666666666666,0.875,1),(0.16666666666666666,0.375,1),
(0,0.5,0.3333333333333333),(1,0.75,0.6666666666666666),(0,0.25,0.6666666666666666),(0,0.75,0.6666666666666666),(1,0.5,0.3333333333333333),(1,0.25,0.6666666666666666),(0.3333333333333333,0,0),(0.3333333333333333,1,0),
(0.6666666666666666,1,0.3333333333333333),(0.6666666666666666,0,0.3333333333333333),(0.5,0,0.83333333333333337),(0.5,1,0.83333333333333337),(0,0.875,0),(0,0.125,0),(1,0.625,0),(0,0.375,0),
(1,0.375,0),(1,0.125,0),(0,0.625,0),(1,0.875,0),(0.3333333333333333,0.25,0),(0.3333333333333333,0.75,0),(0.3333333333333333,0.5,1),(0,0.625,1),
(0.16666666666666666,0.375,0),(1,0.375,1),(0.83333333333333337,0.75,0),(1,0.125,1),(1,0.875,1),(0,0.125,1),(0.6666666666666666,0.5,1),(0.83333333333333337,0.875,0),
(0.16666666666666666,0.25,0),(0.3333333333333333,0.375,1),(1,0.625,1),(0.6666666666666666,0.75,0),(0.3333333333333333,0.75,1),(0.6666666666666666,0.875,1),(0.16666666666666666,0.625,0),(0.6666666666666666,0.125,1),
(1,1,0.5),(0,0.875,1),(0.3333333333333333,0.875,1),(0.6666666666666666,0.625,1),(0.83333333333333337,0.625,0),(0.5,0.125,0),(0.5,0.625,0),(0.83333333333333337,0.125,0),
(0.16666666666666666,0.875,0),(0.6666666666666666,0.25,1),(0.3333333333333333,1,0.5),(0.83333333333333337,0.5,1),(0.6666666666666666,0.375,1),(0.83333333333333337,1,1),(0.83333333333333337,0,1),(0.3333333333333333,0.5,0),
(0,0.375,1),(0.83333333333333337,0.375,0),(0.16666666666666666,1,0),(0.6666666666666666,0.5,0),(0.3333333333333333,0.125,1),(0.6666666666666666,0.25,0),(0.16666666666666666,0.5,1),(0.16666666666666666,0,1),
(0,1,0.5),(0.3333333333333333,0.25,1),(0.16666666666666666,0,0),(0.5,0.375,0),(0,0,0.5),(0.5,0.875,0),(0.6666666666666666,0.75,1),(0.83333333333333337,0.25,0),
(0.16666666666666666,1,1),(0.3333333333333333,0,0.5),(0.16666666666666666,0.125,0),(0.83333333333333337,1,0),(1,0,0.5),(0.3333333333333333,0.625,1),(0.83333333333333337,0,0),(0.16666666666666666,0.75,0),
(0.5,1,0.5),(0.5,0,0.5),(0,0.25,1),(0,0.75,1),(1,0.25,1),(1,0.75,1),(0,0.75,0.3333333333333333),(0,0.5,0),
(0,0.5,0.6666666666666666),(1,0.75,0.3333333333333333),(1,0.25,0.3333333333333333),(0.6666666666666666,1,0),(1,0.5,0.6666666666666666),(1,0.5,0),(0,0.25,0.3333333333333333),(0.6666666666666666,0,0.6666666666666666),
(0.6666666666666666,1,0.6666666666666666),(0.6666666666666666,0,0),(1,0.875,0.3333333333333333),(1,0,0.3333333333333333),(0,0.375,0.3333333333333333),(0,0.875,0.3333333333333333),(0.3333333333333333,1,0.3333333333333333),(0,0,0.3333333333333333),
(0,0.125,0.3333333333333333),(0.83333333333333337,1,0.3333333333333333),(0,0.625,0.3333333333333333),(0.16666666666666666,0,0.3333333333333333),(1,0.125,0.3333333333333333),(0,1,0.3333333333333333),(1,0.375,0.3333333333333333),(1,1,0.3333333333333333),
(0.83333333333333337,0,0.3333333333333333),(0.16666666666666666,1,0.3333333333333333),(0.3333333333333333,0,0.3333333333333333),(1,0.625,0.3333333333333333),(0.6666666666666666,0,0.16666666666666666),(0,0.75,0.16666666666666666),(1,0.25,0.16666666666666666),(0.3333333333333333,0,0.16666666666666666),
(0.83333333333333337,0,0.16666666666666666),(0.5,1,0.3333333333333333),(0.16666666666666666,1,0.16666666666666666),(1,0.125,0.16666666666666666),(0,0.625,0.16666666666666666),(0,0.25,0.16666666666666666),(1,0.875,0.16666666666666666),(0,0.5,0.16666666666666666),
(0,0,0.16666666666666666),(1,0.375,0.16666666666666666),(0,1,0.16666666666666666),(0.3333333333333333,1,0.16666666666666666),(1,0.625,0.16666666666666666),(1,0.5,0.16666666666666666),(0.5,0,0.3333333333333333),(0.5,0,0),
(1,0,0.16666666666666666),(0.5,1,0),(0,0.875,0.16666666666666666),(0.16666666666666666,0,0.16666666666666666),(0.6666666666666666,1,0.16666666666666666),(0,0.375,0.16666666666666666),(1,1,0.16666666666666666),(1,0.75,0.16666666666666666),
(0.83333333333333337,1,0.16666666666666666),(0,0.125,0.16666666666666666),(0.6666666666666666,0,0.83333333333333337),(0,0.625,0.83333333333333337),(1,0.125,0.83333333333333337),(0.6666666666666666,1,0.5),(0,0.875,0.83333333333333337),(0,0.75,0.83333333333333337),
(1,0,0.6666666666666666),(0.3333333333333333,0,0.6666666666666666),(0.83333333333333337,1,0.6666666666666666),(1,1,0.83333333333333337),(0.16666666666666666,1,0.83333333333333337),(0.3333333333333333,1,0.6666666666666666),(0.3333333333333333,1,0.83333333333333337),(0,0.375,0.6666666666666666),
(1,0.5,0.83333333333333337),(1,0.375,0.83333333333333337),(0.6666666666666666,0,0.5),(0.5,0,1),(1,0.25,0.83333333333333337),(0,1,0.6666666666666666),(0.5,0,0.6666666666666666),(0,0.25,0.83333333333333337),
(0,0.375,0.83333333333333337),(0.83333333333333337,0,0.83333333333333337),(0,0.125,0.6666666666666666),(0,0.75,0.5),(1,0.75,0.83333333333333337),(1,0.125,0.6666666666666666),(0.3333333333333333,0,0.83333333333333337),(0,0.25,0.5),
(0.83333333333333337,1,0.83333333333333337),(0,0.5,0.83333333333333337),(1,0.625,0.6666666666666666),(1,0.875,0.83333333333333337),(0,0.625,0.6666666666666666),(0.83333333333333337,0,0.6666666666666666),(1,0.875,0.6666666666666666),(1,0.25,0.5),
(1,0.375,0.6666666666666666),(1,0.75,0.5),(0,0.875,0.6666666666666666),(1,1,0.6666666666666666),(0,1,0.83333333333333337),(0.6666666666666666,1,0.83333333333333337),(1,0.5,0.5),(0,0.5,0.5));
  VAR colorTable:array of T_rgbFloatColor;
  PROCEDURE standardAdaptiveColors;
    VAR i:longint;
        tree:T_colorTree;
        raw:P_floatColor;
    begin
      raw:=context^.image.rawData;
      tree.create;
      for i:=0 to context^.image.pixelCount-1 do tree.addSample(raw[i]);
      tree.finishSampling(parameters.i0);
      colorTable:=tree.colorTable;
      tree.destroy;
    end;

  PROCEDURE simpleLinearColors;
    VAR i:longint;
        raw:P_floatColor;
        tmp:T_rgbFloatColor;
        hsv_global,hsv:T_hsvColor;
        r:double=0;
        g:double=0;
        b:double=0;
    begin
      raw:=context^.image.rawData;
      for i:=0 to context^.image.pixelCount-1 do begin
        tmp:=raw[i];
        r+=tmp[cc_red];
        g+=tmp[cc_green];
        b+=tmp[cc_blue];
      end;
      i:=context^.image.pixelCount;
      hsv_global:=rgbColor(r/i,g/i,b/i);
      setLength(colorTable,parameters.i0);
      for i:=0 to length(colorTable)-1 do begin
        hsv:=hsv_global;
        r:=2*i/(length(colorTable)-1);
        if r<=1 then begin
          hsv[hc_value]:=r;
          hsv[hc_saturation]:=1;
        end else begin
          r-=1;
          hsv[hc_value]:=1;
          hsv[hc_saturation]:=1-r;
        end;
        colorTable[i]:=hsv;
      end;
    end;

  PROCEDURE medianCutColors;
    TYPE T_sample=record
           color:T_rgbColor;
           count:longint;
         end;
         T_colorList=record
           sample:array of T_sample;
           spread:T_rgbFloatColor;
           maxSpread:single;
         end;

    //Variance based spreads
    PROCEDURE updateSpreads(VAR bucket:T_colorList);
      VAR r :double=0;
          g :double=0;
          b :double=0;
          rr:double=0;
          gG:double=0;
          bb:double=0;
          count:longint=0;
          s:T_sample;
      begin
        with bucket do begin
          for s in sample do begin
            inc(count,s.count);
            r +=    s.color[cc_red  ] *s.count;
            g +=    s.color[cc_green] *s.count;
            b +=    s.color[cc_blue ] *s.count;
            rr+=sqr(s.color[cc_red  ])*s.count;
            gG+=sqr(s.color[cc_green])*s.count;
            bb+=sqr(s.color[cc_blue ])*s.count;
          end;
          spread:=rgbColor(sqrt(rr/count-sqr(r/count))*count,
                           sqrt(gG/count-sqr(g/count))*count,
                           sqrt(bb/count-sqr(b/count))*count);
          maxSpread:=max(spread[cc_red],max(spread[cc_green],spread[cc_blue]));
        end;
      end;

    PROCEDURE split(VAR list:T_colorList; OUT newList:T_colorList);
      VAR channel:T_colorChannel;
      FUNCTION partition(CONST Left,Right:longint):longint;
        VAR pivot:byte;
            i:longint;
            tmp:T_sample;
        begin
          pivot:=list.sample[Left+random(Right-Left)].color[channel];
          result:=Left;
          for i:=Left to Right-1 do if list.sample[i].color[channel]<pivot then begin
            tmp                :=list.sample[result];
            list.sample[result]:=list.sample[i];
            list.sample[i     ]:=tmp;
            inc(result);
          end;
        end;

      PROCEDURE sort(CONST Left,Right:longint);
        VAR pivotIdx:longint;
        begin
          if Left=Right then exit;
          pivotIdx:=partition(Left,Right);
          if pivotIdx>Left  then sort(Left,pivotIdx-1);
          if pivotIdx<Right then sort(pivotIdx+1,Right);
        end;

      VAR i,i0,medianCount:longint;
          popCount:longint=0;
      begin
        if list.spread[cc_red]>list.spread[cc_green] then channel:=cc_red else channel:=cc_green;
        if list.spread[cc_blue]>list.spread[channel] then channel:=cc_blue;
        sort(0,length(list.sample)-1);
        for i:=0 to length(list.sample)-1 do popCount+=list.sample[i].count;
        medianCount:=popCount shr 1;
        popCount:=0;
        i0:=0;
        while (popCount<medianCount) do begin
          popCount+=list.sample[i0].count;
          inc(i0);
        end;
        setLength(newList.sample,length(list.sample)-i0);
        for i:=0 to length(newList.sample)-1 do newList.sample[i]:=list.sample[i+i0];
        setLength(list.sample,i0);
        updateSpreads(list);
        updateSpreads(newList);
      end;

    VAR buckets:array of T_colorList;
    PROCEDURE splitOneList;
      VAR i:longint;
          toSplit:longint=0;
      begin
        for i:=1 to length(buckets)-1 do if buckets[i].maxSpread>buckets[toSplit].maxSpread then toSplit:=i;
        i:=length(buckets);
        setLength(buckets,i+1);
        writeln('Splitting bucket ',toSplit,' with ',length(buckets[toSplit].sample),' samples');
        split(buckets[toSplit],buckets[i]);
      end;

    FUNCTION firstBucket:T_colorList;
      VAR raw:P_floatColor;
          i,j:longint;
          tmp:T_rgbColor;
          arr:T_arrayOfLongint;
      begin
        raw:=context^.image.rawData;
        setLength(arr,context^.image.pixelCount);
        for i:=0 to context^.image.pixelCount-1 do begin
          tmp:=raw[i];
          arr[i]:=(tmp[cc_red] or longint(tmp[cc_green]) shl 8 or longint(tmp[cc_blue]) shl 16);
        end;
        sort(arr);
        j:=0; i:=0;
        setLength(result.sample,length(arr));
        tmp[cc_red  ]:= arr[i]         and 255;
        tmp[cc_green]:=(arr[i] shr  8) and 255;
        tmp[cc_blue ]:= arr[i] shr 16;
        result.sample[0].color:=tmp; inc(i);
        result.sample[0].count:=1;
        while i<length(arr) do begin
          if arr[i]=arr[i-1] then inc(result.sample[j].count)
          else begin
            inc(j);
            tmp[cc_red  ]:= arr[i]         and 255;
            tmp[cc_green]:=(arr[i] shr  8) and 255;
            tmp[cc_blue ]:= arr[i] shr 16;
            result.sample[j].color:=tmp;
            result.sample[j].count:=1;
          end;
          inc(i);
        end;
        setLength(arr,0);
        setLength(result.sample,j+1);
        updateSpreads(result);
      end;

    FUNCTION averageColor(CONST bucket:T_colorList):T_rgbFloatColor;
      VAR s:T_sample;
          count:longint=0;
      begin
        result:=BLACK;
        for s in bucket.sample do begin
          result+=s.color*s.count;
          count +=        s.count;
        end;
        result*=1/count;
      end;

    VAR k:longint;
    begin
      setLength(buckets,1);
      buckets[0]:=firstBucket;
      while (length(buckets)<parameters.i0) and not context^.cancellationRequested do splitOneList;
      setLength(colorTable,length(buckets));
      for k:=0 to length(buckets)-1 do begin
        colorTable[k]:=averageColor(buckets[k]);
        setLength(buckets[k].sample,0);
      end;
      setLength(buckets,0);
    end;

  PROCEDURE defaultColorTable;
    VAR k:longint;
    begin
      setLength(colorTable,parameters.i0);
      for k:=0 to length(colorTable)-1 do colorTable[k]:=DEFAULT_COLOR_TABLE[k];
    end;

  PROCEDURE boundsColorTable;
    VAR k:longint;
    begin
      setLength(colorTable,parameters.i0);
      for k:=0 to length(colorTable)-1 do colorTable[k]:=BOUNDS_COLOR_TABLE[k];
    end;

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
      result:=colorTable[kBest];
    end;

  PROCEDURE noDither;
    VAR k:longint;
        p:P_floatColor;
    begin
      p:=context^.image.rawData;
      for k:=0 to context^.image.pixelCount-1 do p[k]:=nearestColor(p[k]);
    end;

  PROCEDURE floydSteinbergDither;
    VAR x,y,xm,ym:longint;
        oldPixel,newPixel,error:T_rgbFloatColor;
    begin
      xm:=context^.image.dimensions.width -1;
      ym:=context^.image.dimensions.height-1;
      for y:=0 to ym do for x:=0 to xm do begin
        oldPixel:=context^.image[x,y]; newPixel:=nearestColor(oldPixel); context^.image[x,y]:=newPixel; error:=oldPixel-newPixel;
        if x<xm then context^.image.multIncPixel(x+1,y,1,error*(7/16));
        if y<ym then begin
          if x>0  then context^.image.multIncPixel(x-1,y+1,1,error*(3/16));
                       context^.image.multIncPixel(x  ,y+1,1,error*(5/16));
          if x<xm then context^.image.multIncPixel(x  ,y+1,1,error*(1/16));
        end;
      end;
    end;

  PROCEDURE lineBasedDither;
    VAR x,y,xm,ym:longint;
        oldPixel,newPixel,error:T_rgbFloatColor;
    begin
      // 1 -> - and +
      // 0 -> -
      // 2 -> +
      // 3 -> simple
      xm:=context^.image.dimensions.width -1;
      ym:=context^.image.dimensions.height-1;
      for y:=0 to ym do if (y and 3)=1 then for x:=0 to xm do begin
        oldPixel:=context^.image[x,y]; newPixel:=nearestColor(oldPixel); context^.image[x,y]:=newPixel; error:=(oldPixel-newPixel)*0.16666666666666666;
        if x> 0 then context^.image.multIncPixel(x-1,y-1,1,error);
                     context^.image.multIncPixel(x  ,y-1,1,error);
        if x<xm then context^.image.multIncPixel(x+1,y-1,1,error);
        if y<ym then begin
          if x> 0 then context^.image.multIncPixel(x-1,y+1,1,error);
                       context^.image.multIncPixel(x  ,y+1,1,error);
          if x<xm then context^.image.multIncPixel(x+1,y+1,1,error);
        end;
      end;
      for y:=0 to ym do case byte(y and 3) of
      0: for x:=0 to xm do begin
           oldPixel:=context^.image[x,y]; newPixel:=nearestColor(oldPixel); context^.image[x,y]:=newPixel; error:=(oldPixel-newPixel)*0.3333333333333333;
           if y>0 then begin
             if x> 0 then context^.image.multIncPixel(x-1,y-1,1,error);
                          context^.image.multIncPixel(x  ,y-1,1,error);
             if x<xm then context^.image.multIncPixel(x+1,y-1,1,error);
           end;
         end;
      2: for x:=0 to xm do begin
           oldPixel:=context^.image[x,y]; newPixel:=nearestColor(oldPixel); context^.image[x,y]:=newPixel; error:=(oldPixel-newPixel)*0.3333333333333333;
           if y<ym then begin
             if x> 0 then context^.image.multIncPixel(x-1,y+1,1,error);
                          context^.image.multIncPixel(x  ,y+1,1,error);
             if x<xm then context^.image.multIncPixel(x+1,y+1,1,error);
           end;
         end;
      end;
      for y:=0 to ym do if (y and 3)=3 then for x:=0 to xm do context^.image[x,y]:=nearestColor(context^.image[x,y]);
    end;

  PROCEDURE kochCurveDither;
    VAR n:longint;
    PROCEDURE d2xy(CONST d:longint; OUT x,y:longint);
      PROCEDURE rot(CONST n:longint; VAR x,y:longint; CONST rx,ry:longint);
        VAR tmp:longint;
        begin
          if (ry=0) then begin
            if (rx=1) then begin
              x:=n-1-x;
              y:=n-1-y;
            end;
            tmp:=x; x:=y; y:=tmp;
          end;
        end;
      VAR rx,ry,t:longint;
          s:longint=1;
      begin
        t:=d;
        x:=0;
        y:=0;
        while s<n do begin
          rx:=1 and (t shr 1);
          ry:=1 and (t xor rx);
          rot(s,x,y,rx,ry);
          x+=s*rx;
          y+=s*ry;
          t:=t shr 2;
          s*=2;
        end;
      end;

    VAR k,xm,ym,x,y:longint;
        error,oldPixel,newPixel:T_rgbFloatColor;
    begin
      xm:=context^.image.dimensions.width -1;
      ym:=context^.image.dimensions.height-1;
      n:=1;
      while (n<=xm) or (n<=ym) do n*=2;
      error:=BLACK;
      for k:=0 to sqr(n)-1 do begin
        d2xy(k,x,y);
        if (x>=0) and (x<=xm) and (y>=0) and (y<=ym) then begin
          oldPixel:=context^.image[x,y]+error;
          newPixel:=nearestColor(oldPixel);
          context^.image[x,y]:=newPixel;
          error:=(oldPixel-newPixel)*0.9;
        end else error:=BLACK;
      end;
    end;
  begin
    case byte(parameters.i1 mod 5) of
      0: standardAdaptiveColors;
      1: defaultColorTable;
      2: boundsColorTable;
      3: simpleLinearColors;
      4: medianCutColors;
    end;
    case byte(parameters.i2 and 3) of
      0: noDither;
      1: floydSteinbergDither;
      2: lineBasedDither;
      3: kochCurveDither;
    end;
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
registerSimpleOperation(imc_statistic,newParameterDescription('quantize',    pt_3integers)^.setDefaultValue('16,0,0')
                                                                                        ^.addChildParameterDescription(spa_i0,'Color count',pt_integer,2,256)
                                                                                        ^.addChildParameterDescription(spa_i1,'Color mode',pt_integer,0,4)
                                                                                        ^.addChildParameterDescription(spa_i2,'Dither mode',pt_integer,0,3),@quantizeCustom_impl);

end.

