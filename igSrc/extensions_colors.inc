{$ifdef include_interface}
TYPE T_colorManipulationKind=(fk_setRGB,
                              fk_plusRGB,fk_plusHSV,
                              fk_mult,fk_multRGB,fk_multHSV,
                              fk_gamma,fk_gammaRGB,fk_gammaHSV,
                              fk_normalize,fk_normalizeR,fk_normalizeG,fk_normalizeB,fk_normalizeH,fk_normalizeS,fk_normalizeV,
                              fk_compress,fk_compressR,fk_compressG,fk_compressB,fk_compressH,fk_compressS,fk_compressV,
                              fk_project,fk_limit,fk_limitLow,fk_grey,fk_sepia,fk_tint,fk_hue,
                              fk_invert,fk_invertR,fk_invertG,fk_invertB,fk_invertH,fk_invertS,fk_invertV,fk_abs,fk_quantize,fk_mono,
                              fk_extract_alpha);
CONST C_colorCommandAbstraction:array [T_colorManipulationKind] of T_commandAbstraction=(
  (isFile:false; leadingSign:'-'; cmdString:'RGB';         paramCount:3),//fk_setRGB
  (isFile:false; leadingSign:'+'; cmdString:'RGB';         paramCount:3),//fk_plusRGB
  (isFile:false; leadingSign:'+'; cmdString:'HSV';         paramCount:3),//fk_plusHSV
  (isFile:false; leadingSign:'*'; cmdString:'';            paramCount:1),//fk_mult
  (isFile:false; leadingSign:'*'; cmdString:'RGB';         paramCount:3),//fk_multRGB
  (isFile:false; leadingSign:'*'; cmdString:'HSV';         paramCount:3),//fk_multHSV
  (isFile:false; leadingSign:'-'; cmdString:'gamma';       paramCount:1),//fk_gamma
  (isFile:false; leadingSign:'-'; cmdString:'gammaRGB';    paramCount:3),//fk_gammaRGB
  (isFile:false; leadingSign:'-'; cmdString:'gammaHSV';    paramCount:3),//fk_gammaHSV
  (isFile:false; leadingSign:'-'; cmdString:'normalize';   paramCount:0),//fk_normalize
  (isFile:false; leadingSign:'-'; cmdString:'normalizeR';  paramCount:0),//fk_normalizeR
  (isFile:false; leadingSign:'-'; cmdString:'normalizeG';  paramCount:0),//fk_normalizeG
  (isFile:false; leadingSign:'-'; cmdString:'normalizeB';  paramCount:0),//fk_normalizeB
  (isFile:false; leadingSign:'-'; cmdString:'normalizeH';  paramCount:0),//fk_normalizeH
  (isFile:false; leadingSign:'-'; cmdString:'normalizeS';  paramCount:0),//fk_normalizeS
  (isFile:false; leadingSign:'-'; cmdString:'normalizeV';  paramCount:0),//fk_normalizeV
  (isFile:false; leadingSign:'-'; cmdString:'compress';    paramCount:1),//fk_compress
  (isFile:false; leadingSign:'-'; cmdString:'compressR';   paramCount:1),//fk_compressR
  (isFile:false; leadingSign:'-'; cmdString:'compressG';   paramCount:1),//fk_compressG
  (isFile:false; leadingSign:'-'; cmdString:'compressB';   paramCount:1),//fk_compressB
  (isFile:false; leadingSign:'-'; cmdString:'compressH';   paramCount:1),//fk_compressH
  (isFile:false; leadingSign:'-'; cmdString:'compressS';   paramCount:1),//fk_compressS
  (isFile:false; leadingSign:'-'; cmdString:'compressV';   paramCount:1),//fk_compressV
  (isFile:false; leadingSign:'-'; cmdString:'project';     paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'limit';       paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'limitlow';    paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'grey';        paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'sepia';       paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'tint';        paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'hue';         paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'invert';      paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'invertR';     paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'invertG';     paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'invertB';     paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'invertH';     paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'invertS';     paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'invertV';     paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'abs';         paramCount:0),
  (isFile:false; leadingSign:'-'; cmdString:'quantize';    paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'mono';        paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'extractAlpha'; paramCount:3));

PROCEDURE colorManipulate(manKind:T_colorManipulationKind; par1,par2,par3:T_Float; VAR inOut:T_FloatMap);
FUNCTION colorManipulate(cmd:string; VAR inOut:T_FloatMap):boolean;
PROCEDURE displayHelpOnColorManipulations;

PROCEDURE normalize(VAR inOut:T_FloatMap);
PROCEDURE normalizeChannel(VAR inOut:T_FloatMap; channel:byte);
PROCEDURE compress (VAR inOut:T_FloatMap; sigma:T_Float);
PROCEDURE compressChannel(VAR inOut:T_FloatMap; sigma:T_Float; channel:byte);
PROCEDURE quantize    (VAR inOut:T_FloatMap  ; numberOfColors:longint);
PROCEDURE monochrome(VAR inOut:T_FloatMap; levels:single);
PROCEDURE extractAlpha(VAR inOut:T_FloatMap; CONST bgR,bgG,bgB:single);

{$endif}
{$ifdef include_implementation}
PROCEDURE colorManipulate(manKind:T_colorManipulationKind; par1,par2,par3:T_Float; VAR inOut:T_FloatMap);
  FUNCTION safeGamma(x,gamma:single):single; inline;
    begin
      if      x> 1E-4 then result:= exp(ln( x)*gamma)
      else if x<-1E-4 then result:=-exp(ln(-x)*gamma)
      else 		 result:=x;
    end;

  FUNCTION limited(c:T_floatColor):T_floatColor; inline;
    begin
      if c[0]<0 then result[0]:=0 else if c[0]>1 then result[0]:=1 else result[0]:=c[0];
      if c[1]<0 then result[1]:=0 else if c[1]>1 then result[1]:=1 else result[1]:=c[1];
      if c[2]<0 then result[2]:=0 else if c[2]>1 then result[2]:=1 else result[2]:=c[2];
    end;

  FUNCTION lowLimited(c:T_floatColor):T_floatColor; inline;
    begin
      if c[0]<0 then result[0]:=0 else result[0]:=c[0];
      if c[1]<0 then result[1]:=0 else result[1]:=c[1];
      if c[2]<0 then result[2]:=0 else result[2]:=c[2];
    end;

  FUNCTION grey(c:T_floatColor):T_floatColor; inline;
    begin
      result[0]:=0.2126*c[0]+0.7152*c[1]+0.0722*c[2];
      result[1]:=result[0];
      result[2]:=result[0];
    end;

  FUNCTION sepia(c:T_floatColor):T_floatColor; inline;
    begin
      result[0]:=safeGamma(c[0],0.5);
      result[1]:=c[1];
      result[2]:=sqr(c[2]);
    end;

  FUNCTION tint(c:T_floatColor; h:single):T_floatColor; inline;
    begin
      result:=toHSV(c);
      result[0]:=h;
      result[1]:=1;
      result:=fromHSV(result);
    end;

  FUNCTION hue(c:T_floatColor; h:single):T_floatColor; inline;
    begin
      result:=toHSV(c);
      result[0]:=h;
      result:=fromHSV(result);
    end;

  VAR pt:P_floatColor;
      c :T_floatColor;
      i:longint;
  begin
    pt:=inOut.rawData;
    case manKind of
      fk_setRGB    : begin c:=newVector(par1,par2,par3); for i:=0 to inOut.size-1 do pt[i]:=c; end;
      fk_plusRGB   : begin c:=newVector(par1,par2,par3); for i:=0 to inOut.size-1 do pt[i]:=pt[i]+c; end;
      fk_plusHSV   : begin c:=newVector(par1,par2,par3); for i:=0 to inOut.size-1 do pt[i]:=fromHSV(toHSV(pt[i])+c); end;
      fk_mult      : for i:=0 to inOut.size-1 do pt[i]:=pt[i]*par1;
      fk_multRGB   : for i:=0 to inOut.size-1 do begin c:=pt[i]; c[0]:=c[0]*par1; c[1]:=c[1]*par2; c[2]:=c[2]*par3; pt[i]:=c; end;
      fk_multHSV   : for i:=0 to inOut.size-1 do begin c:=toHSV(pt[i]); c[0]:=c[0]*par1; c[1]:=c[1]*par2; c[2]:=c[2]*par3; pt[i]:=fromHSV(c); end;
      fk_gamma     : for i:=0 to inOut.size-1 do begin c:=pt[i]; c[0]:=safeGamma(c[0],par1); c[1]:=safeGamma(c[1],par1); c[2]:=safeGamma(c[2],par1); pt[i]:=c; end;
      fk_gammaRGB  : for i:=0 to inOut.size-1 do begin c:=pt[i]; c[0]:=safeGamma(c[0],par1); c[1]:=safeGamma(c[1],par2); c[2]:=safeGamma(c[2],par3); pt[i]:=c; end;
      fk_gammaHSV  : for i:=0 to inOut.size-1 do begin c:=toHSV(pt[i]); c[0]:=safeGamma(c[0],par1); c[1]:=safeGamma(c[1],par2); c[2]:=safeGamma(c[2],par3); pt[i]:=fromHSV(c); end;
      fk_normalize : normalize(inOut);
      fk_normalizeR: normalizeChannel(inOut,0);
      fk_normalizeG: normalizeChannel(inOut,1);
      fk_normalizeB: normalizeChannel(inOut,2);
      fk_normalizeH: normalizeChannel(inOut,3);
      fk_normalizeS: normalizeChannel(inOut,4);
      fk_normalizeV: normalizeChannel(inOut,5);
      fk_compress  : compress(inOut,par1);
      fk_compressR : compressChannel(inOut,par1,0);
      fk_compressG : compressChannel(inOut,par1,1);
      fk_compressB : compressChannel(inOut,par1,2);
      fk_compressH : compressChannel(inOut,par1,3);
      fk_compressS : compressChannel(inOut,par1,4);
      fk_compressV : compressChannel(inOut,par1,5);
      fk_project   : for i:=0 to inOut.size-1 do pt[i]:=fprojectedColor(pt[i]);
      fk_limit     : for i:=0 to inOut.size-1 do pt[i]:=limited(pt[i]);
      fk_limitLow  : for i:=0 to inOut.size-1 do pt[i]:=lowLimited(pt[i]);
      fk_grey      : for i:=0 to inOut.size-1 do pt[i]:=grey(pt[i]);
      fk_sepia     : for i:=0 to inOut.size-1 do pt[i]:=sepia(pt[i]);
      fk_tint      : for i:=0 to inOut.size-1 do pt[i]:=tint(pt[i],par1);
      fk_hue       : for i:=0 to inOut.size-1 do pt[i]:=hue(pt[i],par1);
      fk_invert    : for i:=0 to inOut.size-1 do pt[i]:=white-pt[i];
      fk_invertR   : for i:=0 to inOut.size-1 do begin c:=pt[i]; c[0]:=1-c[0]; pt[i]:=c; end;
      fk_invertG   : for i:=0 to inOut.size-1 do begin c:=pt[i]; c[1]:=1-c[1]; pt[i]:=c; end;
      fk_invertB   : for i:=0 to inOut.size-1 do begin c:=pt[i]; c[2]:=1-c[2]; pt[i]:=c; end;
      fk_invertH   : for i:=0 to inOut.size-1 do begin c:=toHSV(pt[i]); c[0]:=1-c[0]; pt[i]:=fromHSV(c); end;
      fk_invertS   : for i:=0 to inOut.size-1 do begin c:=toHSV(pt[i]); c[1]:=1-c[1]; pt[i]:=fromHSV(c); end;
      fk_invertV   : for i:=0 to inOut.size-1 do begin c:=toHSV(pt[i]); c[2]:=1-c[2]; pt[i]:=fromHSV(c); end;
      fk_abs       : for i:=0 to inOut.size-1 do begin c:=pt[i]; if c[0]<0 then c[0]:=-c[0]; if c[1]<0 then c[1]:=-c[1]; if c[2]<0 then c[2]:=-c[2]; pt[i]:=c; end;
      fk_quantize  : quantize(inOut,round(par1));
      fk_mono      : monochrome(inOut,(par1));
      fk_extract_alpha: extractAlpha(inOut,par1,par2,par3);
    end;

  end;

FUNCTION colorManipulate(cmd:string; VAR inOut:T_FloatMap):boolean;
  VAR fk:T_colorManipulationKind;
      ep:T_extendedParameter;
  begin
    ep:=extendedParam(cmd);
    result:=false;
    for fk:=fk_setRGB to fk_extract_alpha do if matches(ep,C_colorCommandAbstraction[fk]) then begin
      setLength(ep.floatParam,3);
      colorManipulate(fk,ep.floatParam[0],ep.floatParam[1],ep.floatParam[2],inOut);
      result:=true;
    end;
  end;

PROCEDURE displayHelpOnColorManipulations;
  begin
    writeln('Color operations');
    writeln('    -RGB<#>,<#>,<#>       sets color in RGB space');
    writeln('    +RGB<#>,<#>,<#>       adds color in RGB space');
    writeln('    +HSV<#>,<#>,<#>       adds color in HSV space');
    writeln('    *<#>                  multiplies the image with a given factor');
    writeln('    *RGB<#>,<#>,<#>       multiplies color channels with individual factors');
    writeln('    *HSV<#>,<#>,<#>       multiplies color channels with individual factors');
    writeln('    -gamma<#>             gamma correction in RGB space');
    writeln('    -gammaRGB<#>,<#>,<#>  gamma correction in RGB space with individual values');
    writeln('    -gammaHSV<#>,<#>,<#>  gamma correction in HSV space with individual values');
    writeln('    -normalize            simple normalisation');
    writeln('    -normalizeR           normalization of red channel');
    writeln('    -normalizeG           normalization of green channel');
    writeln('    -normalizeB           normalization of blue channel');
    writeln('    -normalizeH           normalization of hue channel');
    writeln('    -normalizeS           normalization of saturation channel');
    writeln('    -normalizeV           normalization of value channel');
    writeln('    -compress<#>          color compression');
    writeln('    -compressR<#>         compression of red channel');
    writeln('    -compressG<#>         compression of green channel');
    writeln('    -compressB<#>         compression of blue channel');
    writeln('    -compressH<#>         compression of hue channel');
    writeln('    -compressS<#>         compression of saturation channel');
    writeln('    -compressV<#>         compression of value channel');
    writeln('    -project              projection to normal color space');
    writeln('    -limit                limitation to normal color space');
    writeln('    -limitlow             limitation to nonnegative channel intensities');
    writeln('    -grey                 greyscale');
    writeln('    -sepia                sepia');
    writeln('    -tint<#>              tint with a given hue');
    writeln('    -hue<#>               set a given hue');
    writeln('    -invert               invert colors');
    writeln('    -invertR              invert red channel');
    writeln('    -invertG              invert green channel');
    writeln('    -invertB              invert blue channel');
    writeln('    -invertH              invert hue channel');
    writeln('    -invertS              invert saturation channel');
    writeln('    -invertV              invert value channel');
    writeln('    -abs                  absolute value');
    writeln('    -quantize<n>          quantize to n colors');
    writeln('    -mono<n>              monochrome; quantized to n brightness levels');
    writeln('    -extractAlpha<r,g,b>  extract alpha, assuming transparent color (r,g,b)');
  end;

PROCEDURE normalize(VAR inOut:T_FloatMap);
  VAR hist:T_histogram;
      lower,upper:single;
      i:longint;
      pt:P_floatColor;
      c :T_floatColor;
  begin
    pt:=inOut.rawData;
    lower:= 1E10;
    upper:=-1E10;
    for i:=0 to inOut.size-1 do begin
      c:=pt[i];
      if c[0]<lower then lower:=c[0];
      if c[1]<lower then lower:=c[1];
      if c[2]<lower then lower:=c[2];
      if c[0]>upper then upper:=c[0];
      if c[1]>upper then upper:=c[1];
      if c[2]>upper then upper:=c[2];
    end;
    if (upper>1) or (lower<0) then begin
      upper:=1/(upper-lower);
      for i:=0 to inOut.size-1 do begin
        c:=pt[i];
        c[0]:=(c[0]-lower)*upper;
        c[1]:=(c[1]-lower)*upper;
        c[2]:=(c[2]-lower)*upper;
        pt[i]:=c;
      end;
    end;
    hist:=toCumulative(smooth(inOut.getHistogram(ht_full  ),1));
    lower:=quantileFromCum(hist,0.01)/255;
    upper:=quantileFromCum(hist,0.99)/255;
    upper:=1/(upper-lower)/0.98;
    for i:=0 to inOut.size-1 do begin
      c:=pt[i];
      c[0]:=(c[0]-lower)*upper-0.01020408163265306;
      c[1]:=(c[1]-lower)*upper-0.01020408163265306;
      c[2]:=(c[2]-lower)*upper-0.01020408163265306;
      pt[i]:=c;
    end;
  end;

PROCEDURE normalizeChannel(VAR inOut:T_FloatMap; channel:byte);
  VAR hist:T_histogram;
      lower,upper:single;
      i:longint;
      pt:P_floatColor;
      c :T_floatColor;
      workInHSV:boolean;
  begin
    pt:=inOut.rawData;
    if channel>=3 then begin
      workInHSV:=true;
      channel:=channel-3;
      for i:=0 to inOut.size-1 do pt[i]:=toHSV(pt[i]);
    end else workInHSV:=false;
    lower:= 1E10;
    upper:=-1E10;
    for i:=0 to inOut.size-1 do begin
      c:=pt[i];
      if c[channel]<lower then lower:=c[channel];
      if c[channel]>upper then upper:=c[channel];
    end;
    if (upper>1) or (lower<0) then begin
      upper:=1/(upper-lower);
      for i:=0 to inOut.size-1 do begin
        c:=pt[i];
        c[channel]:=(c[channel]-lower)*upper;
        pt[i]:=c;
      end;
    end;
    case channel of
      0: hist:=toCumulative(smooth(inOut.getHistogram(ht_redChannel  ),1));
      1: hist:=toCumulative(smooth(inOut.getHistogram(ht_greenChannel),1));
      2: hist:=toCumulative(smooth(inOut.getHistogram(ht_blueChannel ),1));
    end;
    lower:=quantileFromCum(hist,0.01)/255;
    upper:=quantileFromCum(hist,0.99)/255;
    upper:=1/(upper-lower)/0.98;
    if (channel=1) and workInHSV and (lower<0) then begin
      for i:=0 to inOut.size-1 do begin
        c:=pt[i];
        c[channel]:=(c[channel]-lower)*upper-0.01020408163265306;
        if c[channel]<0 then c[channel]:=0;
        pt[i]:=c;
      end;
    end else begin
      for i:=0 to inOut.size-1 do begin
        c:=pt[i];
        c[channel]:=(c[channel]-lower)*upper-0.01020408163265306;
        pt[i]:=c;
      end;
    end;
    if workInHSV then for i:=0 to inOut.size-1 do pt[i]:=fromHSV(pt[i]);
  end;

PROCEDURE compress (VAR inOut:T_FloatMap; sigma:T_Float);
  VAR hist:T_histogram;
      i:longint;
      pt:P_floatColor;
      j :T_floatColor;
      c :T_floatColor;
  begin
    pt:=inOut.rawData;
    hist:=toCumulative(smooth(inOut.getHistogram(ht_full  ),sigma));
    for i:=0 to inOut.size-1 do begin
      j:=pt[i];
      c[0]:=smoothEntry(hist,j[0]*255);
      c[1]:=smoothEntry(hist,j[1]*255);
      c[2]:=smoothEntry(hist,j[2]*255);
      pt[i]:=c;
    end;
  end;

PROCEDURE compressChannel(VAR inOut:T_FloatMap; sigma:T_Float; channel:byte);
  VAR hist:T_histogram;
      i:longint;
      pt:P_floatColor;
      c :T_floatColor;
      workInHSV:boolean;
  begin
    pt:=inOut.rawData;
    if channel>=3 then begin
      workInHSV:=true;
      channel:=channel-3;
      for i:=0 to inOut.size-1 do pt[i]:=toHSV(pt[i]);
    end else workInHSV:=false;
    case channel of
      0: hist:=toCumulative(smooth(inOut.getHistogram(ht_redChannel  ),sigma));
      1: hist:=toCumulative(smooth(inOut.getHistogram(ht_greenChannel),sigma));
      2: hist:=toCumulative(smooth(inOut.getHistogram(ht_blueChannel ),sigma));
    end;
    for i:=0 to inOut.size-1 do begin
      c:=pt[i];
      c[channel]:=smoothEntry(hist,c[channel]*255);
      pt[i]:=c;
    end;
    if workInHSV then for i:=0 to inOut.size-1 do pt[i]:=fromHSV(pt[i]);
  end;

PROCEDURE monochrome(VAR inOut:T_FloatMap; levels:single);
  VAR pt:P_floatColor;
      i,k,j:longint;
      g:single;
      invG:double;
      c:T_floatColor;
      cSum:array[0..2] of double;
  begin
    pt:=inOut.rawData;
    for j:=0 to 2 do cSum[j]:=0; k:=0;
    for i:=0 to inOut.size-1 do begin c:=pt[i]; g:=greyLevel(c); if g>0.01 then begin invG:=1/g; for j:=0 to 2 do cSum[j]:=cSum[j]+c[j]*invG; inc(k); end; pt[i][0]:=g; end;
    invG:=1/k;
    for j:=0 to 2 do cSum[j]:=cSum[j]*invG;
    for i:=0 to inOut.size-1 do begin c:=pt[i]; g:=round(c[0]*levels)/levels; for j:=0 to 2 do c[j]:=g*cSum[j]; pt[i]:=c; end;
  end;

PROCEDURE extractAlpha(VAR inOut:T_FloatMap; CONST bgR,bgG,bgB:single);
  VAR rgbMap:T_FloatMap;
      alphaMap:T_ByteMap;
      i:longint;
      pt:P_floatColor;
      pb:PByte;
  begin
    rgbaSplit(inOut,newColor(bgR,bgG,bgB),rgbMap,alphaMap);
    pt:=inOut.rawData;
    pb:=alphaMap.rawData;
    for i:=0 to inOut.size-1 do pt[i]:=white*(pb[i]/255);
    rgbMap.destroy;
    alphaMap.destroy;
  end;

{$endif}
