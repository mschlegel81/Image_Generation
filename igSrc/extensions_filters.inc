{$ifdef include_interface}
TYPE T_filterKind=(fk_blur,fk_fblur,fk_fblur_V,fk_fblur_H,fk_distFilter,fk_sharpen,fk_details,fk_lagrangeDiff,fk_nonlocalMeans,fk_rotBlur3,fk_rotBlur,fk_radBlur3,fk_radBlur,fk_cblur,fk_coarsen,fk_halftone,fk_median,fk_blurWith,fk_myFilter1,fk_myFilter2,fk_mode,fk_denoise);
CONST C_filterCommandAbstraction:array [T_filterKind] of T_commandAbstraction=(
  (isFile:false; leadingSign:'-'; cmdString:'blur';        paramCount:1), //fk_blur
  (isFile:false; leadingSign:'-'; cmdString:'fblur';       paramCount:1), //fk_fblur
  (isFile:false; leadingSign:'-'; cmdString:'vblur';       paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'hblur';       paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'distFilter';  paramCount:2), //fk_distFilter
  (isFile:false; leadingSign:'-'; cmdString:'sharpen';     paramCount:2), //fk_sharpen
  (isFile:false; leadingSign:'-'; cmdString:'details';     paramCount:1), //fk_details
  (isFile:false; leadingSign:'-'; cmdString:'ld';          paramCount:2), //fk_lagrangeDiff
  (isFile:false; leadingSign:'-'; cmdString:'nlm';         paramCount:2), //fk_nonLocalMeans
  (isFile:false; leadingSign:'-'; cmdString:'rotBlur';     paramCount:3),
  (isFile:false; leadingSign:'-'; cmdString:'rotBlur';     paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'radBlur';     paramCount:3),
  (isFile:false; leadingSign:'-'; cmdString:'radBlur';     paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'cblur';       paramCount:2),
  (isFile:false; leadingSign:'-'; cmdString:'coarsen';     paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'halftone';    paramCount:2),
  (isFile:false; leadingSign:'-'; cmdString:'median';      paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'blurWith';    paramCount:-1),
  (isFile:false; leadingSign:'-'; cmdString:'myFilter';    paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'myFilter';    paramCount:2),
  (isFile:false; leadingSign:'-'; cmdString:'mode';        paramCount:1),
  (isFile:false; leadingSign:'-'; cmdString:'denoise';     paramCount:2));

PROCEDURE filterImage(filterKind:T_filterKind; param1,param2,param3:T_Float; VAR inOut:T_FloatMap);
FUNCTION filterImage(cmd:string; VAR inOut:T_FloatMap):boolean;
PROCEDURE displayHelpOnFilters;

//private: PROCEDURE splitImage(VAR input,base,delta:T_FloatMap);
//private: PROCEDURE joinImage(VAR base,                  delta:T_FloatMap;   VAR output:T_FloatMap  );
PROCEDURE blurImage(VAR inOut:T_FloatMap; sigma:T_Float);
PROCEDURE blurImage(VAR inOut:T_ByteMap; sigma:T_Float);
PROCEDURE fblurImage(VAR inOut:T_FloatMap; sigma:T_Float);
PROCEDURE fblurImage_v(VAR inOut:T_FloatMap; sigma:T_Float);
PROCEDURE fblurImage_h(VAR inOut:T_FloatMap; sigma:T_Float);
//private: PROCEDURE fblurImage(VAR input:T_FloatMap; sigma:T_Float; OUT output:T_floatMap);
//private: PROCEDURE blurImage(VAR input:T_FloatMap;   sigma:T_Float; OUT output:T_FloatMap);
PROCEDURE distFilter(VAR inOut:T_FloatMap; radius:longint; dist:single);
PROCEDURE sharpen(VAR inOut:T_FloatMap;   sigma,factor:T_Float);
PROCEDURE details(VAR inOut:T_FloatMap;   sigma:T_Float);

PROCEDURE directionMap(VAR input:T_FloatMap; sigma:T_Float; OUT direct:T_ByteMap);
//private: PROCEDURE directionMap(VAR input:T_FloatMap; sigma:T_Float; OUT direct,magnitude:T_byteMap);
//private: PROCEDURE lagrangeDiffusion(VAR input:T_FloatMap  ; VAR gradMap:T_byteMap; sigma:single; OUT output:T_FloatMap  );
PROCEDURE lagrangeDiffusion(VAR inOut:T_FloatMap  ; gradSigma,blurSigma:single);

PROCEDURE filterBitBuffer2_r7(VAR inOut:T_FloatMap; sigma:single);
PROCEDURE filterBitBuffer2_r6(VAR inOut:T_FloatMap; sigma:single);
PROCEDURE filterBitBuffer2_r5(VAR inOut:T_FloatMap; sigma:single);
PROCEDURE filterBitBuffer2_r4(VAR inOut:T_FloatMap; sigma:single);
PROCEDURE filterBitBuffer2_r3(VAR inOut:T_FloatMap; sigma:single);

PROCEDURE nlmFilter(VAR inOut:T_FloatMap; scanRadius:longint; sigma:single);

PROCEDURE rotBlur(VAR pic:T_FloatMap; param,centerX,centerY:single);
PROCEDURE radialBlur(VAR pic:T_FloatMap; param,centerX,centerY:single);
PROCEDURE cblur(VAR inOut:T_FloatMap; rotSigma,zoomsigma:single);
PROCEDURE coarsen(VAR inOut:T_FloatMap; param:longint);
PROCEDURE halftone(VAR inOut:T_FloatMap; scale:single; param:longint);
PROCEDURE medianFilter(VAR inOut:T_FloatMap; thresholdDistParam:single);
PROCEDURE anisotropicFilter(VAR inOut:T_FloatMap; intFileName:string);
PROCEDURE myFilter(VAR inOut:T_FloatMap; thresholdDistParam,scanSigma:single);
PROCEDURE modalFilter(VAR inOut:T_FloatMap; thresholdDistParam:single);
PROCEDURE denoise(VAR inOut:T_FloatMap; scale,threshold:single);
{$endif}
{$ifdef include_implementation}
PROCEDURE filterImage(filterKind:T_filterKind; param1,param2,param3:T_Float; VAR inOut:T_FloatMap);
  begin
    case filterKind of
      fk_blur        :blurImage(inOut,param1*1E-2*inOut.diagonal);
      fk_fblur       :fblurImage(inOut,param1*1E-2*inOut.diagonal);
      fk_fblur_V     :fblurImage_v(inOut,param1*1E-2*inOut.diagonal);
      fk_fblur_H     :fblurImage_h(inOut,param1*1E-2*inOut.diagonal);
      fk_distFilter  :distFilter(inOut,round(param1*1E-2*inOut.diagonal),param2);
      fk_sharpen     :sharpen(inOut,param1*1E-2*inOut.diagonal,param2);
      fk_details     :details(inOut,param1*1E-2*inOut.diagonal);
      fk_lagrangeDiff:lagrangeDiffusion(inOut,param1*1E-2*inOut.diagonal,param2*1E-2*inOut.diagonal);
      fk_nonlocalMeans:nlmFilter(inOut,round(param1),param2);
      fk_rotBlur : rotBlur(inOut,param1,0,0);
      fk_rotBlur3: rotBlur(inOut,param1,param2,param3);
      fk_radBlur : radialBlur(inOut,param1,0,0);
      fk_radBlur3: radialBlur(inOut,param1,param2,param3);
      fk_cblur:cblur(inOut,param1*pi/180,1+param2/100);
      fk_coarsen:coarsen(inOut,round(param1));
      fk_halftone:halftone(inOut,param2*1E-2*inOut.diagonal,round(param1));
      fk_median  :medianFilter(inOut,(param1*1E-2*inOut.diagonal));
      fk_myFilter1:myFilter(inOut,param1*1E-2*inOut.diagonal,1);
      fk_myFilter2:myFilter(inOut,param1*1E-2*inOut.diagonal,param2);
      fk_mode     :modalFilter(inOut,param1*1E-2*inOut.diagonal);
      fk_denoise: denoise(inOut,param1*1E-2*inOut.diagonal,param2);
    end;
  end;

FUNCTION filterImage(cmd:string; VAR inOut:T_FloatMap):boolean;
  VAR fk:T_filterKind;
      ep:T_extendedParameter;
  begin
    ep:=extendedParam(cmd);
    result:=false;
    for fk:=fk_blur to fk_denoise do if matches(ep,C_filterCommandAbstraction[fk]) then begin
      setLength(ep.floatParam,3);
      if fk=fk_blurWith then anisotropicFilter(inOut,ep.stringSuffix)
                        else filterImage(fk,ep.floatParam[0],ep.floatParam[1],ep.floatParam[2],inOut);
      result:=true;
    end;
  end;

PROCEDURE displayHelpOnFilters;
  begin
    writeln('Image Filters');
    writeln('  -blur<scale>                  gaussian blur (fixed point)');
    writeln('  -fblur<scale>                 gaussian blur (floating point)');
    writeln('  -vblur<scale>                 vertical gaussian blur (floating point)');
    writeln('  -hblur<scale>                 horizontal gaussian blur (floating point)');
    writeln('  -distFilter<scale><sigma>     distance filter');
    writeln('  -sharpen<scale><sigma>        sharpening');
    writeln('  -details<scale>               details of the image on given rel. scale');
    writeln('  -ld<scale><sigma>             Lagrange Diffusion');
    writeln('  -nlm<scanRadius><sigma>       Non-Local-Means filter');
    writeln('  -rotBlur<scale>[<x0,y0>]      fast rotational blur; optional blur center in relative coords');
    writeln('  -radBlur<scale>[<x0,y0>]      fast radial blur; optional blur center in relative coords');
    writeln('  -cblur<rotSigma>,<zoomSigma>  precise central blur');
    writeln('  -coarsen<par>                 centered coarsening');
    writeln('  -halftone<style>,<scale>      halftone');
    writeln('  -median<scale>                median filter');
    writeln('  -blurWith<filename>           blur using other file as input');
    writeln('  -myFilter<sigma>              filter with relative scale <sigma>');
    writeln('  -myFilter<sigma>,<factor>     filter with relative scale <sigma>; <factor> adapts the filter behavior with =0->gaussian blur');
    writeln('  -mode<relRad>                 local mode');
    writeln('  -denoise<scale><tol>          denoise with given scale and tolerance');
  end;


PROCEDURE splitImage(VAR input,base,delta:T_FloatMap);
  VAR pti,ptb,ptd:P_floatColor;
      i:longint;
  begin
    if (@delta<>@input) and (@delta<>@base) then delta.create(input.width,input.height);
    pti:=input.rawData;
    ptb:=base .rawData;
    ptd:=delta.rawData;
    for i:=0 to input.size-1 do ptd[i]:=pti[i]-ptb[i];
  end;

PROCEDURE joinImage(VAR base,            delta:T_FloatMap;   VAR output:T_FloatMap  );
  VAR pto,ptb,ptd:P_floatColor;
      i:longint;
  begin
    if @output<>@base then output.create(base.width,base.height);
    pto:=output.rawData;
    ptb:=base  .rawData;
    ptd:=delta .rawData;
    for i:=0 to base.size-1 do pto[i]:=ptb[i]+ptd[i];
  end;

PROCEDURE blurImage(VAR inOut:T_FloatMap; sigma:T_Float);
  VAR wand: PMagickWand;
  begin
    {$ifndef globalWand} MagickWandGenesis; {$endif}
    wand:=NewMagickWand;
    MagickConstituteImage(wand,inOut.width,inOut.height,'RGB', FloatPixel,inOut.rawData);
    MagickBlurImage(wand,max(2,round(sigma*3)),sigma);
    MagickGetImagePixels(wand,0,0,inOut.width,inOut.height,'RGB',FloatPixel,inOut.rawData);
    wand := DestroyMagickWand(wand);
    {$ifndef globalWand} MagickWandTerminus; {$endif}
  end;

PROCEDURE blurImage(VAR inOut:T_ByteMap; sigma:T_Float);
  VAR wand: PMagickWand;
  begin
    {$ifndef globalWand} MagickWandGenesis; {$endif}
    wand:=NewMagickWand;
    MagickConstituteImage(wand,inOut.width,inOut.height,'I',CharPixel,inOut.rawData);
    MagickBlurImage(wand,max(2,round(sigma*3)),sigma);
    MagickGetImagePixels(wand,0,0,inOut.width,inOut.height,'I',CharPixel,inOut.rawData);
    wand := DestroyMagickWand(wand);
    {$ifndef globalWand} MagickWandTerminus; {$endif}
  end;


PROCEDURE fblurImage(VAR inOut:T_FloatMap; sigma:T_Float);
  VAR xRes,yRes,x,y,z:longint;
      temp:T_FloatMap;
      pt,ptmp:P_floatColor;
      kern:Pdouble;
      rad :longint;
      sum :T_floatColor;
      weight:double;
  begin
    if sigma>0 then begin
      //intialize kernel:-----------------------//
      rad:=round(3*sigma);                      //
      if rad<2 then rad:=2;                     //
      getMem (kern,(rad+1)*sizeOf(double));     //
      for z:=0 to rad do kern[z]:=exp(-0.5*sqr(z/sigma));
      weight:=1;                                //
      for z:=1 to rad do weight:=weight+2*kern[z];
      weight:=1/weight;                         //
      for z:=0 to rad do kern[z]:=kern[z]*weight;
      //-------------------------:intialize kernel
      pt  :=inOut.rawData;
      xRes:=inOut.width;
      yRes:=inOut.height;
      temp.create(xRes,yRes);
      ptmp:=temp.rawData;
      //blur in x-direction:-----------------------------------------------
      for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
                                               sum:=    kern[ 0]*pt[x+  y*xRes]; weight:=       kern[ 0];
        for z:=max(-x,-rad) to -1     do begin sum:=sum+kern[-z]*pt[x+z+y*xRes]; weight:=weight+kern[-z]; end;
        for z:=1 to min(xRes-x-1,rad) do begin sum:=sum+kern[ z]*pt[x+z+y*xRes]; weight:=weight+kern[ z]; end;
        ptmp[x+y*xRes]:=sum*(1/weight);
      end;
      //-------------------------------------------------:blur in x-direction
      //blur in y-direction:---------------------------------------------------//
      for x:=0 to xRes-1 do for y:=0 to yRes-1 do begin                        //
                                               sum:=    kern[ 0]*ptmp[x+   y *xRes]; weight:=       kern[ 0];
        for z:=max(-y,-rad) to -1     do begin sum:=sum+kern[-z]*ptmp[x+(z+y)*xRes]; weight:=weight+kern[-z]; end;
        for z:=1 to min(yRes-y-1,rad) do begin sum:=sum+kern[ z]*ptmp[x+(z+y)*xRes]; weight:=weight+kern[ z]; end;                                                                //
        pt[x+y*xRes]:=sum*(1/weight);
      end;
      //-----------------------------------------------------:blur in y-direction
      temp.destroy;
      freeMem(kern,(rad+1)*sizeOf(double)); //finaliize kernel
    end;
  end;

PROCEDURE fblurImage_v(VAR inOut:T_FloatMap; sigma:T_Float);
  VAR xRes,yRes,x,y,z:longint;
      temp:T_FloatMap;
      pt,ptmp:P_floatColor;
      kern:Pdouble;
      rad :longint;
      sum :T_floatColor;
      weight:double;
  begin
    if sigma>0 then begin
      //intialize kernel:-----------------------//
      rad:=round(3*sigma);                      //
      if rad<2 then rad:=2;                     //
      getMem (kern,(rad+1)*sizeOf(double));
      for z:=0 to rad do kern[z]:=exp(-0.5*sqr(z/sigma));
      weight:=1;                                //
      for z:=1 to rad do weight:=weight+2*kern[z];
      weight:=1/weight;                         //
      for z:=0 to rad do kern[z]:=kern[z]*weight;
      //-------------------------:intialize kernel
      pt  :=inOut.rawData;
      xRes:=inOut.width;
      yRes:=inOut.height;
      temp.createCopy(inOut);
      ptmp:=temp.rawData;
      //blur in y-direction:---------------------------------------------------//
      for x:=0 to xRes-1 do for y:=0 to yRes-1 do begin                        //
                                               sum:=    kern[ 0]*ptmp[x+   y *xRes]; weight:=       kern[ 0];
        for z:=max(-y,-rad) to -1     do begin sum:=sum+kern[-z]*ptmp[x+(z+y)*xRes]; weight:=weight+kern[-z]; end;
        for z:=1 to min(yRes-y-1,rad) do begin sum:=sum+kern[ z]*ptmp[x+(z+y)*xRes]; weight:=weight+kern[ z]; end;                                                                //
        pt[x+y*xRes]:=sum*(1/weight);
      end;
      //-----------------------------------------------------:blur in y-direction
      temp.destroy;
      freeMem(kern,(rad+1)*sizeOf(double)); //finalize kernel
    end;
  end;


PROCEDURE fblurImage_h(VAR inOut:T_FloatMap; sigma:T_Float);
  VAR xRes,yRes,x,y,z:longint;
      temp:T_FloatMap;
      pt,ptmp:P_floatColor;
      kern:Pdouble;
      rad :longint;
      sum :T_floatColor;
      weight:double;
  begin
    if sigma>0 then begin
      //intialize kernel:-----------------------//
      rad:=round(3*sigma);                      //
      if rad<2 then rad:=2;                     //
      getMem (kern,(rad+1)*sizeOf(double));
      for z:=0 to rad do kern[z]:=exp(-0.5*sqr(z/sigma));
      weight:=1;                                //
      for z:=1 to rad do weight:=weight+2*kern[z];
      weight:=1/weight;                         //
      for z:=0 to rad do kern[z]:=kern[z]*weight;
      //-------------------------:intialize kernel
      pt  :=inOut.rawData;
      xRes:=inOut.width;
      yRes:=inOut.height;
      temp.createCopy(inOut);
      ptmp:=temp.rawData;
      //blur in x-direction:-----------------------------------------------
      for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
                                               sum:=    kern[ 0]*ptmp[x+  y*xRes]; weight:=       kern[ 0];
        for z:=max(-x,-rad) to -1     do begin sum:=sum+kern[-z]*ptmp[x+z+y*xRes]; weight:=weight+kern[-z]; end;
        for z:=1 to min(xRes-x-1,rad) do begin sum:=sum+kern[ z]*ptmp[x+z+y*xRes]; weight:=weight+kern[ z]; end;
        pt[x+y*xRes]:=sum*(1/weight);
      end;
      //-------------------------------------------------:blur in x-direction
      temp.destroy;
      freeMem(kern,(rad+1)*sizeOf(double)); //finalize kernel
    end;
  end;

PROCEDURE blurImage(VAR input:T_FloatMap;   sigma:T_Float; OUT output:T_FloatMap);
  VAR wand: PMagickWand;
  begin
    {$ifndef globalWand} MagickWandGenesis; {$endif}
    wand:=NewMagickWand;
    MagickConstituteImage(wand,input.width,input.height,'RGB', FloatPixel,input.rawData);
    MagickBlurImage(wand,max(1,round(sigma*3)),sigma);
    output.create(input.width,input.height);
    MagickGetImagePixels(wand,0,0,output.width,output.height,'RGB',FloatPixel,output.rawData);
    wand := DestroyMagickWand(wand);
    {$ifndef globalWand} MagickWandTerminus; {$endif}
  end;

PROCEDURE distFilter(VAR inOut:T_FloatMap; radius:longint; dist:single);
  VAR temp:T_FloatMap;
      pt,ps:P_floatColor;
      xRes,yRes:longint;
      x,y,ix,iy:longint;
      count    :longint;
      col,locCol,newCol:T_floatColor;
  begin
    temp.createCopy(inOut);
    xRes:=inOut.width;
    yRes:=inOut.height;
    pt  :=inOut.rawData;
    ps  :=temp .rawData;
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
      locCol:=pt[x+y*xRes];
      col   :=black;
      count:=0;
      for iy:=max(0,y-radius) to min(yRes-1,y+radius) do
      for ix:=max(0,x-radius) to min(xRes-1,x+radius) do if sqr(x-ix)+sqr(y-iy)<=radius then begin
        newCol:=ps[ix+iy*xRes];
        if abs(locCol[0]-newCol[0])+abs(locCol[1]-newCol[1])+abs(locCol[2]-newCol[2])<dist then begin
          col:=col+newCol;
          inc(count);
        end;
      end;
      pt[x+y*xRes]:=col*(1/count);
    end;
    temp.destroy;
  end;

PROCEDURE sharpen(VAR inOut:T_FloatMap;   sigma,factor:T_Float);
  VAR base  :T_FloatMap;
      pt,pBase:P_floatColor;
      i:longint;
  begin
    base.createCopy(inOut);
    fblurImage(base,sigma);
    pt   :=inOut.rawData;
    pBase:=base .rawData;
    for i:=0 to inOut.size-1 do pt[i]:=pBase[i]+(pt[i]-pBase[i])*factor;
    base.destroy;
  end;

PROCEDURE details(VAR inOut:T_FloatMap;   sigma:T_Float);
  VAR base  :T_FloatMap;
  begin
    blurImage(inOut,sigma,base);
    splitImage(inOut,base,inOut);
    inOut.applyOffset(grey);
    base.destroy;
  end;

PROCEDURE directionMap(VAR input:T_FloatMap; sigma:T_Float; OUT direct:T_ByteMap);
  VAR x,y,xRes,yRes:longint;
      pt:P_floatColor;
      pb:PByte;
      pv:P_floatColor;
      temp:T_FloatMap;

  FUNCTION normalAt(x,y:longint):T_floatColor;
    VAR dx,dy,channel:longint;
        n:array[-1..1,-1..1] of T_24Bit;
        w :array [0..1] of single;
    begin
      //fill stencil:--------------------------------------------//
      for dy:=-1 to 1 do for dx:=-1 to 1 do                      //
      if (y+dy>=0) and (y+dy<yRes) and (x+dx>=0) and (x+dx<xRes) //
        then n[dx,dy]:=pt[x+dx+(y+dy)*xRes]                      //
        else n[dx,dy]:=pt[x   + y    *xRes];                     //
      //----------------------------------------------:fill stencil
      result[0]:=0.5;
      result[1]:=0.5;
      result[2]:=0.5;
      for channel:=0 to 2 do begin
        w[0]:=n[ 1,-1][channel]+3*n[ 1,0][channel]+n[ 1,1][channel]
             -n[-1,-1][channel]-3*n[-1,0][channel]-n[-1,1][channel];
        w[1]:=n[-1, 1][channel]+3*n[0, 1][channel]+n[1, 1][channel]
             -n[-1,-1][channel]-3*n[0,-1][channel]-n[1,-1][channel];
        result[2]:=1/sqrt(1E-6+w[0]*w[0]+w[1]*w[1]);
        result[0]:=result[0]+result[2]*(w[0]*w[0]-w[1]*w[1]);
        result[1]:=result[1]+result[2]*2*w[0]*w[1];
      end;

    end;

  FUNCTION vecToDir(x:T_floatColor):byte;
    begin
      result:=round(256*(1+arctan2(x[1]-0.5,x[0]-0.5)/(2*pi))) and 255;
    end;

  begin
    xRes:=input.width;
    yRes:=input.height;
    temp  .create(xRes,yRes);
    direct.create(xRes,yRes);
    pt:=input .rawData;
    pb:=direct.rawData;
    pv:=temp  .rawData;
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do pv[x+y*xRes]:=normalAt(x,y);
    //blurImage(temp,sigma);
    gradMapBlur(temp,sigma);
    pv:=temp  .rawData;
    for x:=0 to xRes*yRes-1 do pb[x]:=vecToDir(pv[x]);
    temp.destroy;
  end;


PROCEDURE directionMap(VAR input:T_FloatMap; sigma:T_Float; OUT direct,magnitude:T_ByteMap);
  VAR x,y,xRes,yRes:longint;
      pt:P_floatColor;
      pb,pm:PByte;
      pv:P_floatColor;
      temp:T_FloatMap;

  FUNCTION normalAt(x,y:longint):T_floatColor;
    VAR dx,dy,channel:longint;
        n:array[-1..1,-1..1] of T_24Bit;
        w :array [0..1] of single;
    begin
      //fill stencil:--------------------------------------------//
      for dy:=-1 to 1 do for dx:=-1 to 1 do                      //
      if (y+dy>=0) and (y+dy<yRes) and (x+dx>=0) and (x+dx<xRes) //
        then n[dx,dy]:=pt[x+dx+(y+dy)*xRes]                      //
        else n[dx,dy]:=pt[x   + y    *xRes];                     //
      //----------------------------------------------:fill stencil
      result[0]:=0.5;
      result[1]:=0.5;
      result[2]:=0.5;
      for channel:=0 to 2 do begin
        w[0]:=n[ 1,-1][channel]+3*n[ 1,0][channel]+n[ 1,1][channel]
             -n[-1,-1][channel]-3*n[-1,0][channel]-n[-1,1][channel];
        w[1]:=n[-1, 1][channel]+3*n[0, 1][channel]+n[1, 1][channel]
             -n[-1,-1][channel]-3*n[0,-1][channel]-n[1,-1][channel];
        result[0]:=result[0]+(w[0]*w[0]-w[1]*w[1]);
        result[1]:=result[1]+2*w[0]*w[1];
      end;
    end;

  FUNCTION vecToDir(x:T_floatColor):byte;
    begin
      result:=round(256*(1+arctan2(x[1]-0.5,x[0]-0.5)/(2*pi))) and 255;
    end;

  FUNCTION vecToMag(x:T_floatColor):byte;
    begin
      result:=round(max(0,min(255,sqrt(sqr(x[1]-0.5)+sqr(x[0]-0.5))*510)));
    end;

  begin
    xRes:=input.width;
    yRes:=input.height;
    temp     .create(xRes,yRes);
    direct   .create(xRes,yRes);
    magnitude.create(xRes,yRes);
    pt:=input    .rawData;
    pb:=direct   .rawData;
    pv:=temp     .rawData;
    pm:=magnitude.rawData;
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do pv[x+y*xRes]:=normalAt(x,y);
    //blurImage(temp,sigma);
    gradMapBlur(temp,sigma);
    pv:=temp  .rawData;
    for x:=0 to xRes*yRes-1 do pb[x]:=vecToDir(pv[x]);
    for x:=0 to xRes*yRes-1 do pm[x]:=vecToMag(pv[x]);
    temp.destroy;
  end;

PROCEDURE lagrangeDiffusion(VAR input:T_FloatMap; VAR gradMap:T_ByteMap; sigma:single; OUT output:T_FloatMap);
  VAR dirTab:array[0..255,0..1] of single;
      i:longint;
      weight:PSingle;
      weightLen:longint;
      x,y,ix,iy,xRes,yRes:longint;
      fx,fy,vx,vy:single;
      collect:T_floatColor;
      colTot :single;
      pt:P_floatColor;
      pd:P_floatColor;
      pg:PByte;

  PROCEDURE moveInDirection(d:byte); inline;
    begin
      if dirTab[d,0]*vx+dirTab[d,1]*vy>0
        then begin vx:= dirTab[d,0]; vy:= dirTab[d,1]; end
        else begin vx:=-dirTab[d,0]; vy:=-dirTab[d,1]; end;
      fx:=fx+vx; ix:=round(fx);
      fy:=fy+vy; iy:=round(fy);
    end;

  begin
    output.create(input.width,input.height);

    //initialize weights:----------------------//
    if sigma<1E-10 then sigma:=1E-10;          //
    weightLen :=round(max(2,2*sigma));         //
    getMem(weight ,weightLen *sizeOf(single)); //
    weight[0]:=1;                              //
    for i:=1 to weightLen-1 do                 //
      weight[i]:=exp(-sqr(i/sigma));           //
    for i:=0 to 255 do dirTab[i,0]:=cos(pi*i/256);
    for i:=0 to 255 do dirTab[i,1]:=sin(pi*i/256);
    //------------------------:initialize weights
    pg:=gradMap.rawData;
    pt:=input  .rawData;
    pd:=output .rawData;
    xRes:=input.width; yRes:=input.height;
    for y:=0 to yRes-1 do
    for x:=0 to xRes-1 do begin
      collect:=pt[x+y*xRes]*weight[0];
      colTot :=             weight[0];
      fx:=x; ix:=x; vx:=1;
      fy:=y; iy:=y; vy:=0;
      moveInDirection((pg[x+y*xRes]+128) and 255);
      for i:=1 to weightLen-1 do if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
        collect:=collect+pt[ix+iy*xRes]*weight[i]; colTot :=colTot+weight[i]; moveInDirection((pg[x+y*xRes]+128) and 255);
      end;
      fx:=x; ix:=x; vx:=-1;
      fy:=y; iy:=y; vy:=0;
      moveInDirection((pg[x+y*xRes]+128) and 255);
      for i:=1 to weightLen-1 do if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
        collect:=collect+pt[ix+iy*xRes]*weight[i]; colTot :=colTot+weight[i]; moveInDirection((pg[x+y*xRes]+128) and 255);
      end;
      pd[x+y*xRes]:=collect*(1/colTot);
    end;
    freeMem(weight ,weightLen *sizeOf(single));
  end;

PROCEDURE lagrangeDiffusion(VAR inOut:T_FloatMap;  gradSigma,blurSigma:single);
  VAR grad:T_ByteMap;
      temp:T_FloatMap;
  begin
    directionMap     (inOut,     gradSigma,grad);
    lagrangeDiffusion(inOut,grad,blurSigma,temp);
    grad.destroy;
    inOut.copyFrom(temp);
    temp.destroy;
  end;

PROCEDURE nlmFilter(VAR inOut:T_FloatMap; scanRadius:longint; sigma:single);
  VAR pOut :P_floatColor;
      temp :T_FloatMap;
      pIn  :P_floatColor;
      xRes,yRes:longint;
      expLUT:Pdouble;
      abortThreshold:double;

  PROCEDURE initLUT;
    VAR i:longint;
        v:double;
    begin
      abortThreshold:=0;
      getMem(expLUT,10000*sizeOf(double));
      for i:=0 to 9999 do begin
        v:=exp(-i*0.5/sigma);
        expLUT[i]:=v;
        abortThreshold:=abortThreshold+v;
      end;
      abortThreshold:=abortThreshold/10000;
    end;

  PROCEDURE doneLUT;
    begin
      freeMem(expLUT,10000*sizeOf(double));
    end;

  FUNCTION patchDistF(x0,y0,x1,y1:longint):double; inline;
    CONST PATCH_KERNEL:array[-3..3,-3..3] of single=
      ((0.13533528323661269,0.23587708298570001,0.32919298780790558,0.36787944117144232,0.32919298780790558,0.23587708298570001,0.13533528323661269),
       (0.23587708298570001,0.41111229050718744,0.5737534207374328 ,0.64118038842995458,0.5737534207374328 ,0.41111229050718744,0.23587708298570001),
       (0.32919298780790558,0.5737534207374328 ,0.80073740291680804,0.89483931681436977,0.80073740291680804,0.5737534207374328 ,0.32919298780790558),
       (0.36787944117144232,0.64118038842995458,0.89483931681436977,0                  ,0.89483931681436977,0.64118038842995458,0.36787944117144232),
       (0.32919298780790558,0.5737534207374328 ,0.80073740291680804,0.89483931681436977,0.80073740291680804,0.5737534207374328 ,0.32919298780790558),
       (0.23587708298570001,0.41111229050718744,0.5737534207374328 ,0.64118038842995458,0.5737534207374328 ,0.41111229050718744,0.23587708298570001),
       (0.13533528323661269,0.23587708298570001,0.32919298780790558,0.36787944117144232,0.32919298780790558,0.23587708298570001,0.13533528323661269));
    VAR dx,dy:longint;
        c0,c1:T_floatColor;
        i:longint;
    begin
      result:=0;//0.02*(sqr(x0-x1)+sqr(y0-y1));
      for dy:=max(-3,max(-y0,-y1)) to min(3,yRes-1-max(y0,y1)) do
      for dx:=max(-3,max(-x0,-x1)) to min(3,xRes-1-max(x0,x1)) do
      if (dx<>0) or (dy<>0) then begin
        c0:=pIn[x0+dx+(y0+dy)*xRes];
        c1:=pIn[x1+dx+(y1+dy)*xRes];
        result:=result+(sqr(c0[0]-c1[0])+sqr(c0[1]-c1[1])+sqr(c0[2]-c1[2]))*PATCH_KERNEL[dy,dx];
      end;
      //result>=0; result<=69.3447732736614 if colors are in normal colorspace;
      //14.4206975203973=1000/68.3447732736614
      i:=round(result/63.08629411010848E-3);
      //i:=round(result*(1000/(3*25)));
      if i<0 then i:=0 else if i>9999 then i:=9999;
      result:=expLUT[i];
    end;

  FUNCTION filteredColorAtF(x,y:longint):T_floatColor;
    VAR w,wtot,wMax:double;
        r,g,b:double;
        dx,dy:longint;
    begin
      wtot:=0;
      wMax:=0;
      r:=0;
      g:=0;
      b:=0;
      for dy:=max(-scanRadius,-y) to min(scanRadius,yRes-1-y) do
      for dx:=max(-scanRadius,-x) to min(scanRadius,xRes-1-x) do
      if (dx<1-scanRadius) or (dx>scanRadius-1) or (dy<1-scanRadius) or (dy>scanRadius-1) then begin
        w:=patchDistF(x,y,x+dx,y+dy);
        if w>wMax then wMax:=w;
        result:=pIn[x+dx+(y+dy)*xRes];
        r   :=r   +result[0]*w;
        g   :=g   +result[1]*w;
        b   :=b   +result[2]*w;
        wtot:=wtot+     w;
      end;
      result:=pIn[x+y*xRes];
      r   :=r   +result[0]*wMax;
      g   :=g   +result[1]*wMax;
      b   :=b   +result[2]*wMax;
      wtot:=wtot+          wMax;
      if wtot<1E-5 then result:=pIn[x+y*xRes]
      else begin
        wtot:=1/(wtot);
        result[0]:=r*wtot;
        result[1]:=g*wtot;
        result[2]:=b*wtot;
      end;
    end;

  VAR x,y:longint;
  begin
    pOut:=inOut.rawData;
    xRes:=inOut.width;
    yRes:=inOut.height;
    initLUT;
    temp.createCopy(inOut);
    pIn:=temp.rawData;
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do
      pOut[x+y*xRes]:=filteredColorAtF(x,y);
    temp.destroy;
    doneLUT;
  end;

{$define NLMFilterProcedure:=blockRadius=3;
        recFrom    =sqr(2*scanRadius+1);
        kernel:array[-3..3,-3..3] of single=
          ((0.13533528323661269,0.23587708298570001,0.32919298780790558,0.36787944117144232,0.32919298780790558,0.23587708298570001,0.13533528323661269),
           (0.23587708298570001,0.41111229050718744,0.5737534207374328 ,0.64118038842995458,0.5737534207374328 ,0.41111229050718744,0.23587708298570001),
           (0.32919298780790558,0.5737534207374328 ,0.80073740291680804,0.89483931681436977,0.80073740291680804,0.5737534207374328 ,0.32919298780790558),
           (0.36787944117144232,0.64118038842995458,0.89483931681436977,0                  ,0.89483931681436977,0.64118038842995458,0.36787944117144232),
           (0.32919298780790558,0.5737534207374328 ,0.80073740291680804,0.89483931681436977,0.80073740291680804,0.5737534207374328 ,0.32919298780790558),
           (0.23587708298570001,0.41111229050718744,0.5737534207374328 ,0.64118038842995458,0.5737534207374328 ,0.41111229050718744,0.23587708298570001),
           (0.13533528323661269,0.23587708298570001,0.32919298780790558,0.36787944117144232,0.32919298780790558,0.23587708298570001,0.13533528323661269));
  TYPE nType=record col:T_24Bit; dist:single; end;
  VAR outBuf:T_FloatMap;
      xRes,yRes,x,y:longint;
      c:array[-scanRadius-blockRadius..scanRadius+blockRadius,
              -scanRadius-blockRadius..scanRadius+blockRadius] of T_24Bit;
      close:array[0..recFrom-1] of nType;

  FUNCTION filterPixel(x,y:longint):T_24Bit; inline;
    VAR dx,dy,ix,iy,k,k2,closestIdx:longint;
        Weight,wLoc,
        resR,resG,resB:single;
    begin
      k2        :=1;
      closestIdx:=1;
      close[0].dist:=0;
      close[0].col :=c[0,0];
      for dx:=-scanRadius to scanRadius do if (x+dx>=0) and (x+dx<xRes) then
      for dy:=-scanRadius to scanRadius do if (y+dy>=0) and (y+dy<yRes) and ((dx<>0) or (dy<>0)) then begin
        //block-distance [dx+/-blockradius,dy+/-blockradius] to [+/-blockradius,+/-blockradius]:-//
        weight:=0; wLoc:=0;                                                                      //
        for ix:=-blockRadius to blockRadius do if (x+dx+ix>=0) and (x+dx+ix<xRes) then           //
        for iy:=-blockRadius to blockRadius do if (y+dy+iy>=0) and (y+dy+iy<yRes) then begin     //
          wLoc  :=wLoc  +kernel[ix,iy]*sigma;                                                    //
          weight:=weight+kernel[ix,iy]*(sqr(c[ix,iy][0]-c[ix+dx,iy+dy][0])                       //
                                       +sqr(c[ix,iy][1]-c[ix+dx,iy+dy][1])                       //
                                       +sqr(c[ix,iy][2]-c[ix+dx,iy+dy][2]));                     //
        end; //for iy; end for ix                                                                //
        //---:block-distance [dx+/-blockradius,dy+/-blockradius] to [+/-blockradius,+/-blockradius]
        if wLoc>1E-5 then begin
          //writeln(weight,' ',wloc);
          close[k2].dist:=exp(-weight*6.0308629411010848E-3/wLoc);
          close[k2].col :=c[dx,dy];
          if close[k2].dist>close[closestIdx].dist then closestIdx:=k2;
          inc(k2);
        end; //if k>0
      end; //for dy; end for dx
      //writeln('computing weighted sum...');
      close[0].dist:=close[closestIdx].dist;
      weight:=0; resB:=0; resG:=0; resR:=0;
      for k:=0 to k2-1 do begin
        weight:=weight+close[k].dist;
        resB  :=resB  +close[k].dist*close[k].col[0];
        resG  :=resG  +close[k].dist*close[k].col[1];
        resR  :=resR  +close[k].dist*close[k].col[2];
      end;
      if weight<1E-5 then result:=close[0].col
      else begin
        weight:=1/weight;
        result[0]:=round(resB*weight);
        result[1]:=round(resG*weight);
        result[2]:=round(resR*weight);
      end;
    end;

  PROCEDURE initStencil(x,y:longint); inline;
    VAR dx,dy:longint;
    begin
      for dy:=scanRadius+blockRadius downto -scanRadius-blockRadius do if (y+dy>=0) and (y+dy<yRes) then begin
        for dx:=-scanRadius-blockRadius to scanRadius+blockRadius do if (x+dx>=0) and (x+dx<xRes) then begin
          c[dx,dy]:=inOut.pixel[x+dx,y+dy];
        end;
      end;
    end;

  PROCEDURE shrStencil(x,y:longint); inline;
    VAR dx,dy:longint;
    begin
      for dy:=scanRadius+blockRadius downto -scanRadius-blockRadius do if (y+dy>=0) and (y+dy<yRes) then begin
        for dx:=-scanRadius-blockRadius to scanRadius+blockRadius-1 do c[dx,dy]:=c[dx+1,dy];
        if x+scanRadius+blockRadius<xRes then begin
          c[scanRadius+blockRadius,dy]:=inOut.pixel[x+scanRadius+blockRadius,y+dy];
        end;
      end;
    end;

  begin
    xRes:=inOut.width;
    yRes:=inOut.height;
    outBuf.create(xRes,yRes);
    for y:=yRes-1 downto 0 do begin
      initStencil(0,y);
      for x:=0 to xRes-1 do begin
        outBuf.pixel[x,y]:=filterPixel(x,y);
        shrStencil(x+1,y);
      end;
    end;
    writeln;
    inOut.copyFrom(outBuf);
    outBuf.destroy;
  end}

PROCEDURE filterBitBuffer2_r7(VAR inOut:T_FloatMap; sigma:single); CONST scanRadius =7; NLMFilterProcedure;
PROCEDURE filterBitBuffer2_r6(VAR inOut:T_FloatMap; sigma:single); CONST scanRadius =6; NLMFilterProcedure;
PROCEDURE filterBitBuffer2_r5(VAR inOut:T_FloatMap; sigma:single); CONST scanRadius =5; NLMFilterProcedure;
PROCEDURE filterBitBuffer2_r4(VAR inOut:T_FloatMap; sigma:single); CONST scanRadius =4; NLMFilterProcedure;
PROCEDURE filterBitBuffer2_r3(VAR inOut:T_FloatMap; sigma:single); CONST scanRadius =3; NLMFilterProcedure;


PROCEDURE rotBlur(VAR pic:T_FloatMap; param,centerX,centerY:single);
  VAR grad:T_ByteMap;
      temp:T_FloatMap;
      x,y:longint;
  begin
    grad.create(pic.width,pic.height);
    centerX:=centerX*0.5*pic.diagonal+pic.width/2;
    centerY:=centerY*0.5*pic.diagonal+pic.height/2;
    for y:=0 to pic.height-1 do
    for x:=0 to pic.width -1 do grad[x,y]:=round(256*(1+arctan2(x-centerX,y-centerY)/(pi))) and 255+128;
    temp.createCopy(pic);
    lagrangeDiffusion(temp,grad,param*0.01*pic.diagonal,pic);
    temp.destroy;
    grad.destroy;
  end;

PROCEDURE radialBlur(VAR pic:T_FloatMap; param,centerX,centerY:single);
  VAR grad:T_ByteMap;
      temp:T_FloatMap;
      x,y:longint;
  begin
    grad.create(pic.width,pic.height);
    centerX:=centerX*0.5*pic.diagonal+pic.width/2;
    centerY:=centerY*0.5*pic.diagonal+pic.height/2;
    for y:=0 to pic.height-1 do
    for x:=0 to pic.width -1 do grad[x,y]:=round(256*(1+arctan2(x-centerX,y-centerY)/(pi))) and 255;
    temp.createCopy(pic);
    lagrangeDiffusion(temp,grad,param*0.01*pic.diagonal,pic);
    temp.destroy;
    grad.destroy;
  end;

PROCEDURE cblur(VAR inOut:T_FloatMap; rotSigma,zoomsigma:single);
  VAR temp:T_FloatMap;
      po,pt:P_floatColor;
      xRes,yRes,x,y,cx,cy,i,imax,lx,ly,j,tx0,tx1,ty0,ty1:longint;
      col :T_floatColor;
      wtot,wx,wy:single;
      refRad:array of longint;
      cosTab:PSingle;
      sinTab:PSingle;
      wgtTab:PSingle;
  begin
    temp.createCopy(inOut);
    xRes:=inOut.width;   cx:=xRes shr 1;
    yRes:=inOut.height;  cy:=yRes shr 1;
    po:=temp .rawData;
    pt:=inOut.rawData;
    zoomsigma:=ln(zoomsigma);
    if zoomsigma<0 then begin
      zoomsigma:=-zoomsigma;
      rotSigma :=-rotSigma;
    end;
    imax:=1;
    setLength(refRad,0);
    repeat
      wtot:=1/(1+exp(zoomsigma*2/imax)-2*exp(zoomsigma/imax)*cos(rotSigma/imax));
      setLength(refRad,length(refRad)+1);
      refRad[length(refRad)-1]:=round(wtot*exp(-2*zoomsigma)-0.5);
      inc(imax,imax);
    until wtot>sqr(inOut.diagonal);
    imax:=imax shr 1;
    getMem(cosTab,sizeOf(single)*(imax+imax+1));
    getMem(sinTab,sizeOf(single)*(imax+imax+1));
    getMem(wgtTab,sizeOf(single)*(imax+imax+1));
    for i:=-imax to imax do begin
      wtot          :=exp    (zoomsigma*i/imax);
      cosTab[i+imax]:=wtot*cos(rotSigma*i/imax);
      sinTab[i+imax]:=wtot*sin(rotSigma*i/imax);
      wgtTab[i+imax]:=exp(-sqr(i*3/imax));
    end;

    for y:=0 to yRes-1 do begin
      ly:=y-cy;
      for x:=0 to xRes-1 do begin
        lx:=x-cx;
        wtot:=0; col:=black;
        i:=0; while refRad[i]<lx*lx+ly*ly do inc(i);
        j:=0;
        while j<=imax+imax do begin
          //tx:=round(cx+cosTab[j]*lx+sinTab[j]*ly);
          //ty:=round(cy+cosTab[j]*ly-sinTab[j]*lx);
          wx:=cx+cosTab[j]*lx+sinTab[j]*ly;
          wy:=cy+cosTab[j]*ly-sinTab[j]*lx;
          tx0:=floor(wx);
          ty0:=floor(wy);

          if (tx0>=0) and (tx0<xRes-1) and (ty0>=0) and (ty0<yRes-1) then begin
            wx:=wx-tx0; tx1:=tx0+1; //tx0:=max(0,min(xres-1,tx0));
            wy:=wy-ty0; ty1:=ty0+1; //ty0:=max(0,min(yres-1,ty0));


            wtot:=wtot+               wgtTab[j];
            //col :=col +po[tx0+ty0*xres]*wgtTab[j];
            col :=col +((po[tx0+ty0*xRes]*(1-wx)+po[tx1+ty0*xRes]*wx)*(1-wy)
                       +(po[tx0+ty1*xRes]*(1-wx)+po[tx1+ty1*xRes]*wx)*   wy )*wgtTab[j];
          end;
          inc(j,imax shr i);
        end;
        if wtot>0 then pt[x+y*xRes]:=col*(1/wtot);
      end;
    end;
    setLength(refRad,0);
    freeMem(cosTab,sizeOf(single)*(imax+imax+1));
    freeMem(sinTab,sizeOf(single)*(imax+imax+1));
    freeMem(wgtTab,sizeOf(single)*(imax+imax+1));
    temp.destroy;
  end;

PROCEDURE coarsen(VAR inOut:T_FloatMap; param:longint);
  VAR xRes,yRes:longint;
      xc,yc:longint;
      x,y,scale:longint;
      stillBusy:boolean;

  FUNCTION averageBlock(x0,y0,dxy:longint):boolean;
    VAR x,y,k:longint;
        c:T_floatColor;
    begin
      k:=0;
      c:=black;
      for y:=max(0,y0) to min(y0+dxy,yRes)-1 do
      for x:=max(0,x0) to min(x0+dxy,xRes)-1 do begin
        inc(k);
        c:=c+inOut.pixel[x,y];
      end;
      if k>0 then begin
        result:=true;
        c:=c*(1/k);
        for y:=max(0,y0) to min(y0+dxy,yRes)-1 do
        for x:=max(0,x0) to min(x0+dxy,xRes)-1 do inOut.pixel[x,y]:=c;
      end else result:=false;
    end;


  begin
    xRes:=inOut.width;
    yRes:=inOut.height;
    xc:=xRes shr 1;
    yc:=yRes shr 1;
    scale:=2;
    repeat
      stillBusy:=false;
      for y:=-2*param to 2*param-1 do for x:=-2*param to 2*param-1 do
      if ((x<-param) or (x>=param) or (y<-param) or (y>=param)) then
        stillBusy:=averageBlock(xc+x*scale,yc+y*scale,scale) or stillBusy;
      inc(scale,scale);
    until not(stillBusy);
  end;

PROCEDURE halftone(VAR inOut:T_FloatMap; scale:single; param:longint);
  VAR xRes,yRes:longint;
      temp:T_FloatMap;

  FUNCTION avgSqrRad(x0,y0,rad:single):T_floatColor; inline;
    VAR x,y,k:longint;
    begin
      result:=black;
      k:=0;
      for y:=max(0,floor(y0-rad)) to min(yRes-1,ceil(y0+rad)) do
      for x:=max(0,floor(x0-rad)) to min(xRes-1,ceil(x0+rad)) do
      if sqr(x-x0)+sqr(y-y0)<sqr(rad) then begin
        result:=result+inOut[x,y];
        inc(k);
      end;
      if k>0 then begin
        result:=result*(1/k);
        if result[0]<0 then result[0]:=0 else result[0]:=sqr(rad*2)*result[0]/pi;
        if result[1]<0 then result[1]:=0 else result[1]:=sqr(rad*2)*result[1]/pi;
        if result[2]<0 then result[2]:=0 else result[2]:=sqr(rad*2)*result[2]/pi;
      end;
    end;

  FUNCTION avgCover(x,y,sqrRad:single):single; inline;
    VAR k,i,j:longint;
    begin
      k:=0;
      if sqr(x-0.375)+sqr(y-0.375)<sqrRad then inc(k);
      if sqr(x-0.375)+sqr(y+0.375)<sqrRad then inc(k);
      if sqr(x+0.375)+sqr(y-0.375)<sqrRad then inc(k);
      if sqr(x+0.375)+sqr(y+0.375)<sqrRad then inc(k);
      if k=0 then result:=0 else if k=4 then result:=1
      else begin
        for i:=0 to 3 do for j:=0 to 3 do
          if sqr(x-0.375+i*0.25)+
             sqr(y-0.375+j*0.25)<sqrRad then inc(k);
        result:=(k-4)/16;
      end;
    end;

  PROCEDURE paintCircle(x0,y0:single; rad:T_floatColor);
    VAR cov,col:T_floatColor;
        x,y:longint;
        mrad:single;
    begin
      mrad:=sqrt(max(rad[0],max(rad[1],rad[2])));
      for y:=max(0,floor(y0-mrad)) to min(yRes-1,ceil(y0+mrad)) do
      for x:=max(0,floor(x0-mrad)) to min(xRes-1,ceil(x0+mrad)) do begin
        cov[0]:=avgCover(x-x0,y-y0,rad[0]);
        cov[1]:=avgCover(x-x0,y-y0,rad[1]);
        cov[2]:=avgCover(x-x0,y-y0,rad[2]);
        col:=inOut[x,y];
        col[0]:=max(col[0],cov[0]);
        col[1]:=max(col[1],cov[1]);
        col[2]:=max(col[2],cov[2]);
        inOut[x,y]:=col;
      end;
    end;

  PROCEDURE paintCircle(x0,y0,rad:single; channel:byte);
    VAR cov:single;
        col:T_floatColor;
        x,y:longint;
    begin
      for y:=max(0,floor(y0-sqrt(rad))) to min(yRes-1,ceil(y0+sqrt(rad))) do
      for x:=max(0,floor(x0-sqrt(rad))) to min(xRes-1,ceil(x0+sqrt(rad))) do begin
        cov:=avgCover(x-x0,y-y0,rad);
        col:=inOut[x,y];
        col[channel]:=max(col[channel],cov);
        inOut[x,y]:=col;
      end;
    end;

  VAR x,y:longint;
      sx:single;
      pt:P_floatColor;
  begin
    xRes:=inOut.width;
    yRes:=inOut.height;
    pt:=inOut.rawData;
    if param and 1=1 then for x:=0 to inOut.size-1 do pt[x]:=white-pt[x];
    if param and 4=0 then begin
      temp.create(ceil(xRes/scale+2),ceil(yRes/scale+2));
      if param and 2=0 then begin
        for y:=0 to temp.height-1 do for x:=0 to temp.width-1 do temp[x,y]:=avgSqrRad(x*scale,y*scale,0.5*scale);
      end else begin
        for y:=0 to temp.height-1 do for x:=0 to temp.width-1 do temp[x,y]:=newVector(
          avgSqrRad( x     *scale, y     *scale,0.5*scale)[0],
          avgSqrRad( x     *scale,(y+0.5)*scale,0.5*scale)[1],
          avgSqrRad((x+0.5)*scale, y     *scale,0.5*scale)[2]);
      end;
    end else begin
      temp.create(ceil(xRes/scale+2),ceil(yRes/(scale*0.86602540378444)+2));
      if param and 2=0 then begin
        for y:=0 to temp.height-1 do begin
          if odd(y) then sx:=0.5*scale else sx:=0;
          for x:=0 to temp.width-1 do temp[x,y]:=avgSqrRad(sx+x*scale,y*scale*0.86602540378444,0.5*scale);
        end;
      end else begin
        for y:=0 to temp.height-1 do begin
          if odd(y) then sx:=0.5*scale else sx:=0;
          for x:=0 to temp.width-1 do temp[x,y]:=newVector(
            avgSqrRad(sx+ x      *scale, y     *scale*0.86602540378444,0.5*scale)[0],
            avgSqrRad(sx+(x+0.25)*scale,(y+0.5)*scale*0.86602540378444,0.5*scale)[1],
            avgSqrRad(sx+(x+0.5 )*scale, y     *scale*0.86602540378444,0.5*scale)[2]);
        end;
      end;
    end;
    //analyze:
    //:analyze
    //draw:
    pt:=inOut.rawData;
    for x:=0 to inOut.size-1 do pt[x]:=black;
    if param and 4=0 then begin
      if param and 2=0 then begin
        for y:=0 to temp.height-1 do for x:=0 to temp.width-1 do paintCircle(x*scale,y*scale,temp[x,y]);
      end else begin
        for y:=0 to temp.height-1 do for x:=0 to temp.width-1 do begin
          paintCircle( x     *scale, y     *scale,temp[x,y][0],0);
          paintCircle( x     *scale,(y+0.5)*scale,temp[x,y][1],1);
          paintCircle((x+0.5)*scale, y     *scale,temp[x,y][2],2);
        end;
      end;
    end else begin
      if param and 2=0 then begin
        for y:=0 to temp.height-1 do begin
          if odd(y) then sx:=0.5*scale else sx:=0;
          for x:=0 to temp.width-1 do paintCircle(sx+x*scale,y*scale*0.86602540378444,temp[x,y]);
        end;
      end else begin
        for y:=0 to temp.height-1 do for x:=0 to temp.width-1 do begin
          if odd(y) then sx:=0.5*scale else sx:=0;
          paintCircle(sx+(x-0.5 )*scale, y     *scale*0.86602540378444,temp[x,y][0],0);
          paintCircle(sx+(x-0.25)*scale,(y+0.5)*scale*0.86602540378444,temp[x,y][1],1);
          paintCircle(sx+(x     )*scale, y     *scale*0.86602540378444,temp[x,y][2],2);
        end;
      end;
    end;
    temp.destroy;
    pt:=inOut.rawData;
    if param and 1=1 then for x:=0 to inOut.size-1 do pt[x]:=white-pt[x];
  end;

{PROCEDURE medianFilter(VAR inout:T_FloatMap; radius:longint);
  CONST kern:array[-4..4] of single=(0.0190983005625053,0.0690983005625053,0.130901699437495,0.180901699437495,0.2,0.180901699437495,0.130901699437495,0.0690983005625053,0.0190983005625053);
  VAR xRes,yRes,x,y,dx,dy,k,i:longint;
      pt,pd:P_floatColor;
      temp:T_FloatMap;
      c24:T_24Bit;
      hist:array[0..2] of T_histogram;
      weight,weightSum,rf:single;
  begin
    write(' radius is ',radius,' ');
    rf:=pi/radius;
    xRes:=inOut.width;
    yRes:=inOut.height;
    temp.createCopy(inOut);
    pt:=temp.rawData;
    pd:=inOut.rawData;
    for y:=0 to yRes-1 do begin writeln(y);
    for x:=0 to xRes-1 do begin
      clear(hist[0]);
      clear(hist[1]);
      clear(hist[2]);
      weightSum:=0;
      for dy:=max(0,y-radius) to min(yRes-1,y+radius) do
      for dx:=max(0,x-radius) to min(xRes-1,x+radius) do if (sqr(x-dx)+sqr(y-dy)<=sqr(radius)) then begin
        c24:=pt[dx+dy*xRes];
        weight:=1+cos(rf*sqrt(sqr(x-dx)+sqr(y-dy)));
        weightSum:=weightSum+weight;
        for i:=0 to 2 do begin
          for k:=-4 to 4 do if (c24[i]+k>=0) and (c24[i]+k<=255) then
            finc(hist[i][c24[i]+k],weight*kern[k]);
        end;

      end;
      pd[x+y*xRes]:=newVector(
            quantileFromHist(hist[0],0.5*weightSum)/255,
            quantileFromHist(hist[1],0.5*weightSum)/255,
            quantileFromHist(hist[2],0.5*weightSum)/255);
    end; end;
    temp.destroy;
  end;}

PROCEDURE anisotropicFilter(VAR inOut:T_FloatMap; intFileName:string);
  VAR xRes,yRes,x,y,dx,dy,binIdx:longint;
      temp,intensity:T_FloatMap;
      pt,ptmp,pint:P_floatColor;
      bin:array[1..32] of record
        kern:Pdouble;
        rad :longint;
      end;
      sum :T_floatColor;
      weightY,weight,wSum:double;
      maxSigma:double;

  PROCEDURE initBin(index:byte; sigma:single);
    VAR z:longint;
    begin
      writeln('initializing bin ',index,' with abs. sigma ',sigma);
      with bin[index] do begin
        //intialize kernel:-----------------------//
        rad:=round(3*sigma);                      //
        if rad<2 then rad:=2;                     //
        getMem (kern,(rad+1)*sizeOf(double));         //
        for z:=0 to rad do kern[z]:=exp(-0.5*sqr(z/sigma));
        weight:=1;                                //
        for z:=1 to rad do weight:=weight+2*kern[z];
        weight:=1/weight;                         //
        for z:=0 to rad do kern[z]:=kern[z]*weight;
        //-------------------------:intialize kernel
      end;
    end;

  PROCEDURE finalizeBin(index:byte);
    begin
      with bin[index] do
        if kern<>nil then freeMem(kern,(rad+1)*sizeOf(double));
    end;

  begin
    for binIdx:=1 to 32 do bin[binIdx].kern:=nil;
    intensity.create(intFileName);
    xRes:=inOut.width;
    yRes:=inOut.height;
    temp.createCopy(inOut);
    pt  :=inOut.rawData;
    ptmp:=temp.rawData;
    pint:=intensity.rawData;
    maxSigma:=quantileFromHist(intensity.getHistogram(ht_greyLevel),0.99)/255;
    writeln('max Sigma is ',maxSigma);
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
      binIdx:=round(32*greyLevel(pint[x+y*xRes])/maxSigma);
      if binIdx>32 then binIdx:=32;
      if binIdx>0 then with bin[binIdx] do begin
        if kern=nil then initBin(binIdx,maxSigma/32*binIdx*1E-2*inOut.diagonal);
        sum:=black;
        wSum:=0;
        for dy:=max(-y,-rad) to min(yRes-y-1,rad) do begin
          weightY:=kern[abs(dy)];
          for dx:=max(-x,-rad) to min(xRes-x-1,rad) do begin
            weight:=kern[abs(dx)]*weightY;
            wSum:=wSum+weight;
            sum := sum+weight*ptmp[x+dx+(y+dy)*xRes];
          end;
        end;
        if wSum>0 then pt[x+y*xRes]:=sum*(1/wSum);
      end;
    end;

    ////blur in x-direction:-----------------------------------------------
    //for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
    //  binIdx:=round(32*greyLevel(pint[x+y*xRes])/maxSigma);
    //  if binIdx>32 then binIdx:=32;
    //  if binIdx<=0 then ptmp[x+y*xRes]:=pt[x+y*xRes]
    //  else with bin[binIdx] do begin
    //    if kern=nil then initBin(binIdx,maxSigma/32*binIdx*1E-2*inOut.diagonal);
    //                                           sum:=    kern[ 0]*pt[x+  y*xRes]; weight:=       kern[ 0];
    //    for z:=max(-x,-rad) to -1     do begin sum:=sum+kern[-z]*pt[x+z+y*xRes]; weight:=weight+kern[-z]; end;
    //    for z:=1 to min(xRes-x,rad)-1 do begin sum:=sum+kern[ z]*pt[x+z+y*xRes]; weight:=weight+kern[ z]; end;
    //    ptmp[x+y*xRes]:=sum*(1/weight);
    //  end;
    //end;
    ////-------------------------------------------------:blur in x-direction
    ////blur in y-direction:---------------------------------------------------//
    //for x:=0 to xRes-1 do for y:=0 to yRes-1 do begin                        //
    //  binIdx:=round(32*greyLevel(pint[x+y*xRes])/maxSigma);
    //  if binIdx>32 then binIdx:=32;
    //  if binIdx<=0 then pt[x+y*xRes]:=ptmp[x+y*xRes]
    //  else with bin[binIdx] do begin
    //                                           sum:=    kern[ 0]*ptmp[x+   y *xRes]; weight:=       kern[ 0];
    //    for z:=max(-y,-rad) to -1     do begin sum:=sum+kern[-z]*ptmp[x+(z+y)*xRes]; weight:=weight+kern[-z]; end;
    //    for z:=1 to min(yRes-y,rad)-1 do begin sum:=sum+kern[ z]*ptmp[x+(z+y)*xRes]; weight:=weight+kern[ z]; end;                                                                //
    //    pt[x+y*xRes]:=sum*(1/weight);
    //  end;
    //end;
    //-----------------------------------------------------:blur in y-direction
    intensity.destroy;
    for binIdx:=1 to 32 do finalizeBin(binIdx);
    temp.destroy;
  end;

PROCEDURE myFilter(VAR inOut:T_FloatMap; thresholdDistParam,scanSigma:single);
  VAR temp   :T_FloatMap; pTemp   :P_floatColor;
                          pInOut  :P_floatColor;
      xRes,yRes,x,y,rad: longint;
      smoothkern:Pdouble;

  FUNCTION localThingy(x,y:longint):T_floatColor;

{skew=(mean-median)/[standard deviation]
skew=(µ-median)/s
skew=(E(X³)-3µs²-µ³)/s³
median=µ-s*skew
      =µ-s*(E(X³)-3µs²-µ³)/s³
      =µ-  (E(X³)-3µs²-µ³)/s²
      =µ-  (E(X³)-3µs²-µ³)/s²
µ=sum/samples
s=sqrt(sum(X²)/samples-sqr(sum/samples))
 }
    VAR c:T_floatColor;
        dc,sum,sum2,sum3:array[0..2] of double;
        mean,sigma,invWeight,weight,sumOfWeights:double;
        dx,dy,i:longint;
    begin
      for i:=0 to 2 do sum[i]:=0;
      for i:=0 to 2 do sum2[i]:=0;
      for i:=0 to 2 do sum3[i]:=0;
      sumOfWeights:=0;
      for dy:=max(-y,-rad) to min(yRes-1-y,rad) do
      for dx:=max(-x,-rad) to min(xRes-1-x,rad) do begin
        c:=pTemp[x+dx+(y+dy)*xRes];
        weight:=smoothkern[abs(dx)]*smoothkern[abs(dy)];
        for i:=0 to 2 do begin
          dc[i]:=c[i]*weight;
          sum[i]:=sum[i]+dc[i];
          dc[i]:=dc[i]*c[i];
          sum2[i]:=sum2[i]+dc[i];
          dc[i]:=dc[i]*c[i];
          sum3[i]:=sum3[i]+dc[i];
        end;
        sumOfWeights:=sumOfWeights+weight;
      end;
      invWeight:=1/sumOfWeights;
      for i:=0 to 2 do begin
        mean:=sum[i]*invWeight;
        sigma:=sum2[i]*invWeight-mean*mean;
        if sigma<1E-8 then result[i]:=mean
        else begin
          sigma:=sqrt(sigma);
          weight:=scanSigma*sigma*arctan((sum3[i]*invWeight-mean*mean*mean)/(sigma*sigma*sigma)-3*mean/sigma);
          result[i]:=mean-weight;
          //if weight>0 then result:=weight*green else result:=-weight*red;

        end;
      end;
    end;

  VAR z:longint;
  begin
    xRes:=inOut.width;
    yRes:=inOut.height;
    pInOut:=inOut.rawData;
    temp.createCopy(inOut);
    pTemp:=temp.rawData;
    //initialize kernel:--------------------//
    rad:=round(3*thresholdDistParam);         //
    if rad<2 then rad:=2;                   //
    getMem(smoothkern,rad*sizeOf(double));  //
    for z:=0 to rad-1 do smoothkern[z]:=    //
      exp(-0.5*sqr(z/thresholdDistParam));  //
    //----------------------:initialize kernel
    for y:=0 to yRes-1 do
    for x:=0 to xRes-1 do
      pInOut[x+y*xRes]:=localThingy(x,y);
    temp.destroy;
    freeMem(smoothkern,rad*sizeOf(double));
  end;

PROCEDURE modalFilter(VAR inOut:T_FloatMap; thresholdDistParam:single);
  VAR temp   :T_FloatMap; pTemp   :P_floatColor;
                          pInOut  :P_floatColor;
      xRes,yRes,x,y,rad: longint;
      smoothkern:Pdouble;

  FUNCTION localMode(x,y:longint):T_floatColor;
    VAR c:T_floatColor;
        w:single;
        hist:array[0..2] of T_histogram;
        dx,dy,i,j,k:longint;
    begin
      for i:=0 to 2 do clear(hist[i]);
      for dy:=max(-y,-rad) to min(yRes-1-y,rad) do
      for dx:=max(-x,-rad) to min(xRes-1-x,rad) do begin
        w:=smoothkern[abs(dx)]*smoothkern[abs(dy)];
        c:=pTemp[x+dx+(y+dy)*xRes];
        for i:=0 to 2 do begin
          k:=round(c[i]*255);
          if (k>0) and (k<255) then begin
            hist[i][k-1]:=hist[i][k-1]+w*0.25;
            hist[i][k  ]:=hist[i][k  ]+w*0.5;
            hist[i][k+1]:=hist[i][k+1]+w*0.25;
          end else begin
            j:=k-1; if j<0 then j:=0 else if j>255 then j:=255; hist[i][j]:=hist[i][j]+w*0.25;
            j:=k  ; if j<0 then j:=0 else if j>255 then j:=255; hist[i][j]:=hist[i][j]+w*0.5;
            j:=k+1; if j<0 then j:=0 else if j>255 then j:=255; hist[i][j]:=hist[i][j]+w*0.25;
          end;
        end;
      end;
      for i:=0 to 2 do result[i]:=mainMode(hist[i])/255;
    end;

  VAR z:longint;
  begin
    xRes:=inOut.width;
    yRes:=inOut.height;
    pInOut:=inOut.rawData;
    temp.create(xRes,yRes);
    pTemp:=temp.rawData;
    for z:=0 to xRes*yRes-1 do pTemp[z]:=toHSV(pInOut[z]);
    //initialize kernel:--------------------//
    rad:=round(3*thresholdDistParam);       //
    if rad<2 then rad:=2;                   //
    getMem(smoothkern,(rad+1)*sizeOf(double));  //
    for z:=0 to rad do smoothkern[z]:=      //
      exp(-0.5*sqr(z/thresholdDistParam));  //
    //----------------------:initialize kernel
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do
      pInOut[x+y*xRes]:=fromHSV(localMode(x,y));
    temp.destroy;
    freeMem(smoothkern,(rad+1)*sizeOf(double));
  end;

PROCEDURE medianFilter(VAR inOut:T_FloatMap; thresholdDistParam:single);
  VAR temp   :T_FloatMap; pTemp   :P_floatColor;
                          pInOut  :P_floatColor;
      xRes,yRes,x,y,rad: longint;
      smoothkern:PSingle;

  FUNCTION localMode(x,y:longint):T_floatColor;
    VAR c:T_floatColor;
        w,wSum:single;
        hist:array[0..2] of T_histogram;
        dx,dy,i,k,j:longint;
    begin
      for i:=0 to 2 do clear(hist[i]);
      wSum:=0;
      for dy:=max(-y,-rad) to min(yRes-1-y,rad) do
      for dx:=max(-x,-rad) to min(xRes-1-x,rad) do begin
        w:=smoothkern[abs(dx)]*smoothkern[abs(dy)];
        c:=pTemp[x+dx+(y+dy)*xRes];
        wSum:=wSum+w;
        for i:=0 to 2 do begin
          k:=round(c[i]*255);
          if (k>0) and (k<255) then begin
            hist[i][k-1]:=hist[i][k-1]+w*0.25;
            hist[i][k  ]:=hist[i][k  ]+w*0.5;
            hist[i][k+1]:=hist[i][k+1]+w*0.25;
          end else begin
            j:=k-1; if j<0 then j:=0 else if j>255 then j:=255; hist[i][j]:=hist[i][j]+w*0.25;
            j:=k  ; if j<0 then j:=0 else if j>255 then j:=255; hist[i][j]:=hist[i][j]+w*0.5;
            j:=k+1; if j<0 then j:=0 else if j>255 then j:=255; hist[i][j]:=hist[i][j]+w*0.25;
          end;
        end;
      end;
      result:=newVector(quantileFromHist(hist[0],wSum*0.5)/255,
                        quantileFromHist(hist[1],wSum*0.5)/255,
                        quantileFromHist(hist[2],wSum*0.5)/255);
    end;

  VAR z:longint;
  begin
    xRes:=inOut.width;
    yRes:=inOut.height;
    pInOut:=inOut.rawData;
    temp.createCopy(inOut);
    pTemp:=temp.rawData;
    //initialize kernel:--------------------//
    rad:=round(3*thresholdDistParam);       //
    if rad<2 then rad:=2;                   //
    getMem(smoothkern,(rad+1)*sizeOf(single));  //
    for z:=0 to rad do smoothkern[z]:=      //
      exp(-0.5*sqr(z/thresholdDistParam));  //
    //----------------------:initialize kernel
    for y:=0 to yRes-1 do for x:=0 to xRes-1 do
      pInOut[x+y*xRes]:=localMode(x,y);
    temp.destroy;
    freeMem(smoothkern,(rad+1)*sizeOf(single));
  end;

PROCEDURE denoise(VAR inOut:T_FloatMap; scale,threshold:single);
  VAR xRes,yRes,x,y,i:longint;
      pInOut:P_floatColor;
      blurred:T_FloatMap;
      pBlurred:P_floatColor;
      mask:T_ByteMap;
      pMask:PByte;
      c:T_floatColor;
      level:single;

  FUNCTION collectAround(x,y:longint):T_floatColor;
    VAR radius,dx,dy,k:longint;
    begin
      result:=black;
      k:=0;
      radius:=1;
      while k<20 do begin
        if (y-radius>=0) then for dx:=max(-x,-radius) to min(radius,xRes-1-x) do
          if pMask[x+dx+(y-radius)*xRes]=0 then begin
            inc(k);
            result:=result+pInOut[x+dx+(y-radius)*xRes];
          end;
        for dy:=max(1-radius,-y) to min(radius-1,yRes-1-y) do begin
          if (x-radius>=0) and (pMask[x-radius+(y+dy)*xRes]=0) then begin
            inc(k);
            result:=result+pInOut[x-radius+(y+dy)*xRes];
          end;
          if (x+radius<xRes) and (pMask[x+radius+(y+dy)*xRes]=0) then begin
            inc(k);
            result:=result+pInOut[x+radius+(y+dy)*xRes];
          end;
        end;
        if (y+radius<yRes) then for dx:=max(-x,-radius) to min(radius,xRes-1-x) do
          if pMask[x+dx+(y+radius)*xRes]=0 then begin
            inc(k);
            result:=result+pInOut[x+dx+(y+radius)*xRes];
          end;
        inc(radius);
      end;
      result:=result*(1/k);
    end;

  begin
    xRes:=inOut.width;
    yRes:=inOut.height;
    pInOut:=inOut.rawData;

    blurred.createCopy(inOut);
    fblurImage(blurred,scale);
    pBlurred:=blurred.rawData;

    mask.create(xRes,yRes);
    pMask:=mask.rawData;

    for i:=0 to xRes*yRes-1 do begin
      c :=pInOut[i]-pBlurred[i];
      level:=0;
      for x:=0 to 2 do if c[x]>0 then begin
        if c[x]>level then level:=c[x];
      end else begin
        if -c[x]>level then level:=-c[x];
      end;
      if level>threshold then pMask[i]:=1
                         else pMask[i]:=0;
    end;
    blurred.destroy;

    for y:=0 to yRes-1 do for x:=0 to xRes-1 do
      if odd(pMask[x+y*xRes]) then pInOut[x+y*xRes]:=collectAround(x,y);

    mask.destroy;

  end;

{$endif}
