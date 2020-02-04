UNIT im_misc;
INTERFACE
IMPLEMENTATION
USES imageManipulation,imageContexts,myParams,mypics,myColors,math,pixMaps;

PROCEDURE sketch_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.sketch(parameters.f0,parameters.f1,parameters.f2,parameters.f3);
  end;

PROCEDURE drip_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.drip(parameters.f0,parameters.f1);
  end;

PROCEDURE encircle(VAR image:T_rawImage; CONST count:longint; CONST background:T_rgbFloatColor; CONST opacity,relativeCircleSize:double; CONST context:P_abstractWorkflow);
  TYPE T_circle=record
         cx,cy,radius,diff:double;
         color:T_rgbFloatColor;
       end;

  FUNCTION randomCircle(CONST radius:double):T_circle;
    begin
      result.cx:=radius+random*(image.dimensions.width-2*radius);
      result.cy:=radius+random*(image.dimensions.height-2*radius);
      result.radius:=radius;
      result.diff:=0;
    end;

  FUNCTION avgColor(VAR source:T_rawImage; CONST circle:T_circle):T_rgbFloatColor;
    VAR sampleCount:longint=0;
        sqrRad:double;
        x,y:longint;
    begin
      sqrRad:=sqr(circle.radius);
      result:=BLACK;
      with circle do
      for y:=max(0,round(cy-radius)) to min(image.dimensions.height-1,round(cy+radius)) do
      for x:=max(0,round(cx-radius)) to min(image.dimensions.width-1,round(cx+radius)) do
      if sqr(x-cx)+sqr(y-cy)<=sqrRad then
      begin
        result:=result+source[x,y];
        inc(sampleCount);
      end;
      if sampleCount>0 then result:=result*(1/sampleCount);
    end;

  VAR copy:T_rawImage;
      i,j:longint;
      newCircle,toDraw: T_circle;

  FUNCTION globalAvgDiff:double;
    VAR i:longint;
    begin
      result:=0;
      for i:=0 to image.pixelCount-1 do result:=result+colDiff(copy.rawData[i],image.rawData[i]);
      result/=image.pixelCount;
    end;

  PROCEDURE drawCircle(CONST circle:T_circle);
    VAR sqrRad:double;
        x,y,k:longint;
        r:double;
    begin
      sqrRad:=sqr(circle.radius+1);
      with circle do
      for y:=max(0,floor(cy-radius)) to min(image.dimensions.height-1,ceil(cy+radius)) do
      for x:=max(0,floor(cx-radius)) to min(image.dimensions.width-1,ceil(cx+radius)) do begin
        r:=sqr(x-cx)+sqr(y-cy);
        if r<=sqrRad then
        begin
          k:=x+y*image.dimensions.width;
          r:=sqrt(r);
          if r<radius-0.5 then r:=opacity
          else if r>radius+0.5 then r:=0
          else r:=(radius+0.5-r)*opacity;
          if r>0 then image.rawData[k]:=image.rawData[k]*(1-r)+color*r;
        end;

      end;
    end;

  FUNCTION bestCircle(CONST radius:double):T_circle;
    VAR x,y,cx,cy:longint;
        diff:double;
        maxDiff:double=0;
    begin
      if (radius>0.5*min(image.dimensions.width,image.dimensions.height)) then exit(bestCircle(0.499*min(image.dimensions.width,image.dimensions.height)));
      for y:=round(radius) to round(image.dimensions.height-radius) do
      for x:=round(radius) to round(image.dimensions.width-radius) do begin
        diff:=colDiff(image.pixel[x,y],copy[x,y]);
        if (diff>maxDiff) then begin
          cx:=x;
          cy:=y;
          maxDiff:=diff;
        end;
      end;
      result.cx:=cx;
      result.cy:=cy;
      result.radius:=radius;
      result.diff:=diff;
      result.color:=avgColor(copy,result);
    end;

  VAR radius:double;
      circleSamples:longint=1;
  begin
    radius:=relativeCircleSize*image.diagonal;
    copy.create(image);
    image.clearWithColor(background);
    for i:=0 to count-1 do begin
      if ((i*1000) div count<>((i-1)*1000) div count) or (radius>=0.1*image.diagonal) then begin
        if context^.cancellationRequested then break;
        radius:=max(relativeCircleSize*image.diagonal*min(1,1/6*globalAvgDiff),1);
        circleSamples:=round(10000/sqr(radius));
        if circleSamples>31 then circleSamples:=31;
      end;
      initialize(toDraw);
      for j:=0 to circleSamples do begin
        newCircle:=randomCircle(radius);
        newCircle.color:=avgColor(copy,newCircle);
        newCircle.diff:=colDiff(avgColor(image,newCircle),newCircle.color);
        if (j=0) or (newCircle.diff>toDraw.diff) then toDraw:=newCircle;
      end;
      drawCircle(toDraw);
    end;
    copy.destroy;
  end;

PROCEDURE encircle_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    encircle(context^.image,parameters.i0,WHITE,parameters.f1,parameters.f2,context);
  end;
PROCEDURE encircleNeon_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    encircle(context^.image,parameters.i0,BLACK,parameters.f1,parameters.f2,context);
  end;

PROCEDURE bySpheres(VAR image:T_rawImage; CONST count:longint; CONST style:byte; CONST relativeCircleSize0,relativeCircleSize1:double; CONST context:P_abstractWorkflow);
  VAR copy:T_rawImage;

  FUNCTION avgColor(VAR source:T_rawImage; CONST cx,cy,radius:double):T_rgbFloatColor;
    VAR sampleCount:longint=0;
        sqrRad:double;
        x,y:longint;
    begin
      sqrRad:=sqr(radius);
      result:=BLACK;
      for y:=max(0,round(cy-radius)) to min(image.dimensions.height-1,round(cy+radius)) do
      for x:=max(0,round(cx-radius)) to min(image.dimensions.width-1,round(cx+radius)) do
      if sqr(x-cx)+sqr(y-cy)<=sqrRad then
      begin
        result:=result+source[x,y];
        inc(sampleCount);
      end;
      if sampleCount>0 then result:=result*(1/sampleCount);
    end;

  FUNCTION getColorForPixel(CONST ix,iy:longint; CONST cx,cy,radius:double; CONST baseColor,previousColor:T_rgbFloatColor):T_rgbFloatColor;
    FUNCTION illumination(x,y,r2:double):single;
      VAR ambient:single;
      begin
        ambient:=x*0.30151134457776363-
        y         *0.30151134457776363+
        sqrt(1-r2)*0.90453403373329089;
        result:=ambient*0.75+0.25;
      end;

    VAR x,y,r2:double;
        cover:single;
    begin
      x:=(ix-cx)/radius;
      y:=(iy-cy)/radius;
      r2:=sqr(x)+sqr(y);
      if r2<1 then begin
        result:=baseColor*illumination(x,y,r2);
      end else begin
        r2:=sqrt(r2);
        cover:=(1-(r2-1)*radius);
        if cover<0
        then result:=previousColor
        else result:=baseColor    *illumination(x/r2,y/r2,1)*cover+
                     previousColor*                  (1-cover);
      end;
    end;

  PROCEDURE drawRandomSphere(CONST radius,avgWeight:double);
    FUNCTION getImprovement(CONST cx,cy:double):double;
      VAR x,y:longint;
          prevError,newError:double;
          avg:T_rgbFloatColor;
      begin
        avg:=avgColor(copy,cx,cy,radius);
        result:=0;
        for y:=max(0,round(cy-radius)) to min(image.dimensions.height-1,round(cy+radius)) do
        for x:=max(0,round(cx-radius)) to min(image.dimensions.width -1,round(cx+radius)) do begin
          prevError:=colDiff(copy[x,y],image.pixel[x,y]);
          newError :=colDiff(copy[x,y],getColorForPixel(x,y,cx,cy,radius,avg*avgWeight+copy[x,y]*(1-avgWeight),image.pixel[x,y]));
          result+=prevError-newError;
        end;
      end;

    VAR x,y:longint;
        best_cx,best_cy,best_imp:double;
        cx,cy,imp:double;
        avg:T_rgbFloatColor;
        i:longint;
    begin
      best_imp:=-infinity;
      for i:=0 to round(min(20,30000/sqr(radius))) do begin
        cx:=random*(image.dimensions.width +radius)-0.5*radius;
        cy:=random*(image.dimensions.height+radius)-0.5*radius;
        imp:=getImprovement(cx,cy);
        if (imp>best_imp) then begin
          best_imp:=imp;
          best_cx :=cx;
          best_cy :=cy;
        end;
      end;
      cx:=best_cx;
      cy:=best_cy;
      avg:=avgColor(copy,cx,cy,radius);
      for y:=max(0,round(cy-radius)) to min(image.dimensions.height-1,round(cy+radius)) do
      for x:=max(0,round(cx-radius)) to min(image.dimensions.width -1,round(cx+radius)) do
        image.pixel[x,y]:=getColorForPixel(x,y,cx,cy,radius,avg*avgWeight+copy[x,y]*(1-avgWeight),image.pixel[x,y]);
    end;

  VAR radius,avgWeight,ra,rb:double;
      i:longint;
  begin
    copy.create(image);
    rb:=relativeCircleSize1/(relativeCircleSize0-relativeCircleSize1);
    ra:=rb*relativeCircleSize0*image.diagonal;
    if      style=0 then avgWeight:=1
    else if style=1 then avgWeight:=0;
    image.clearWithColor(BLACK);
    for i:=0 to count-1 do begin
      if context^.cancellationRequested then break;
      //radius:=exp((1-i/(count-1))*ln(relativeCircleSize0)+
      //               i/(count-1) *ln(relativeCircleSize1))*diagonal;
      radius:=ra/(rb+i/(count-1));
      case style of
        2: avgWeight:=  i/(count-1);
        3: avgWeight:=1-i/(count-1);
      end;
      drawRandomSphere(radius,avgWeight);
    end;
    copy.destroy;
  end;

PROCEDURE spheres_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    bySpheres(context^.image,parameters.i0,parameters.i1,parameters.f2,parameters.f3,context);
  end;

PROCEDURE halftone_impl(CONST parameters:T_parameterValue; CONST context:P_abstractWorkflow);
  begin
    context^.image.halftone(parameters.f1*context^.image.diagonal*0.01,parameters.i0);
  end;

INITIALIZATION
registerSimpleOperation(imc_misc,
  newParameterDescription('sketch',pt_4floats)^
    .setDefaultValue('1,0.1,0.8,0.2')^
    .addChildParameterDescription(spa_f0,'cover'          ,pt_float,0)^
    .addChildParameterDescription(spa_f1,'direction sigma',pt_float,0)^
    .addChildParameterDescription(spa_f2,'density'        ,pt_float)^
    .addChildParameterDescription(spa_f3,'tolerance'      ,pt_float,0),
  @sketch_impl);
registerSimpleOperation(imc_misc,
  newParameterDescription('drip',pt_2floats,0,1)^
    .setDefaultValue('0.1,0.01')^
    .addChildParameterDescription(spa_f0,'diffusiveness',pt_float,0,1)^
    .addChildParameterDescription(spa_f1,'range' ,pt_float,0,1),
  @drip_impl);
registerSimpleOperation(imc_misc,
  newParameterDescription('encircle',pt_1I2F,0)^
    .setDefaultValue('2000,0.5,0.2')^
    .addChildParameterDescription(spa_i0,'circle count',pt_integer,1,100000)^
    .addChildParameterDescription(spa_f1,'opacity' ,pt_float,0,1)^
    .addChildParameterDescription(spa_f2,'circle size' ,pt_float,0),
  @encircle_impl);
registerSimpleOperation(imc_misc,
  newParameterDescription('encircleNeon',pt_1I2F,0)^
    .setDefaultValue('2000,0.5,0.2')^
    .addChildParameterDescription(spa_i0,'circle count',pt_integer,1,100000)^
    .addChildParameterDescription(spa_f1,'opacity' ,pt_float,0,1)^
    .addChildParameterDescription(spa_f2,'circle size' ,pt_float,0),
  @encircleNeon_impl);
registerSimpleOperation(imc_misc,
  newParameterDescription('spheres',pt_2I2F,0)^
    .setDefaultValue('2000,3,0.2,0.001')^
    .addChildParameterDescription(spa_i0,'sphere count',pt_integer,1,100000)^
    .addChildParameterDescription(spa_i1,'sphere style',pt_integer,0,3)^
    .addChildParameterDescription(spa_f2,'max size' ,pt_float,0,1)^
    .addChildParameterDescription(spa_f3,'min size' ,pt_float,0,1),
  @spheres_impl);
registerSimpleOperation(imc_misc,
  newParameterDescription('halftone',pt_1I1F)^
    .setDefaultValue('0,0.2')^
    .addChildParameterDescription(spa_i0,'style',pt_integer,0,7)^
    .addChildParameterDescription(spa_f1,'scale',pt_float,0),
  @halftone_impl);

end.

