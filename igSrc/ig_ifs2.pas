UNIT ig_ifs2;
INTERFACE
USES imageGeneration,myColors,myTools,complex,myParams,sysutils,myGenerics,mypics,math,darts;
TYPE
  T_elementaryTrafo=record
    color:T_rgbFloatColor;
    con:T_Complex;
    lin:array[0..1] of T_Complex;
  end;

  T_trafoInTime=record
    offset,amplitude,phaseShift:T_elementaryTrafo; //=3*4=12
  end;

  P_ifs_v2=^T_ifs_v2;
  T_ifs_v2=object(T_pixelThrowerAlgorithm)
    par_depth  :longint;//=128;
    par_seed   :byte   ;//=3;
    par_color  :byte   ;//=0;
    par_bright :single ;//=1;
    par_symmex :byte   ;//=0;
    par_trafo  :array[0..5] of T_trafoInTime; //=6*12=72

    CONSTRUCTOR create;
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE prepareSlice(CONST target:P_rawImage; CONST queue:P_progressEstimatorQueue; CONST index:longint); virtual;
  end;

IMPLEMENTATION

CONSTRUCTOR T_ifs_v2.create;
  CONST seedNames:array[0..3] of string=('Gauss','Circle','Line','Triangle');
        colorNames:array[0..13] of string=('Normal/b','Crisp/b','Fire/b','Ice/b','Rainbow/b','White/b','Orange/b',
                                            'Normal/w','Crisp/w','Fire/w','Ice/w','Rainbow/w','Black/w','Orange/w');
        postStepNames:array[0..9] of string=('None','Mirror X','Mirror Y','Mirror XY','Mirror Center','Rotate 3','Rotate 4','Rotate 5','Blur','Shift');
  VAR i:longint;
  begin
    inherited create;
    {0}addParameter('depth',pt_integer,1);
    {1}addParameter('seed type',pt_enum,0,2)^.setEnumValues(seedNames);
    {2}addParameter('coloring',pt_enum,0,10)^.setEnumValues(colorNames);
    {3}addParameter('brightness',pt_float,0);
    {4}addParameter('post-step',pt_enum,0,10)^.setEnumValues(postStepNames);
    for i:=0 to 5 do begin //Trafo-in-time-index
      {5+3i  }addParameter('Color'+intToStr(i)+'o',pt_color);
      {5+3i+1}addParameter('Color'+intToStr(i)+'a',pt_color);
      {5+3i+2}addParameter('Color'+intToStr(i)+'p',pt_3floats);
    end;
    for i:=0 to 5 do begin //Trafo-in-time-index
      {23+9i  }addParameter('con' +intToStr(i)+'o',pt_2floats);
      {23+9i+1}addParameter('con' +intToStr(i)+'a',pt_2floats);
      {23+9i+2}addParameter('con' +intToStr(i)+'p',pt_2floats);
      {23+9i+3}addParameter('lin0'+intToStr(i)+'o',pt_2floats);
      {23+9i+4}addParameter('lin0'+intToStr(i)+'a',pt_2floats);
      {23+9i+5}addParameter('lin0'+intToStr(i)+'p',pt_2floats);
      {23+9i+6}addParameter('lin1'+intToStr(i)+'o',pt_2floats);
      {23+9i+7}addParameter('lin1'+intToStr(i)+'a',pt_2floats);
      {23+9i+8}addParameter('lin1'+intToStr(i)+'p',pt_2floats);
    end;
    resetParameters(0);
  end;

FUNCTION T_ifs_v2.parameterResetStyles: T_arrayOfString;
  begin
    result:='Zero';
    append(result,'Random');
    append(result,'Zero Amplitude');   //2
    append(result,'Reduced Amplitude');//3
    append(result,'MOD: reduce all amplitudes by 1/10');//4
    append(result,'MOD: zero amplitudes and phases');//5
  end;

PROCEDURE T_ifs_v2.resetParameters(CONST style: longint);
  FUNCTION modifyParameters:boolean;
    VAR i:longint;
    begin
      result:=true;
      case style of
        4: for i:=0 to 5 do with par_trafo[i] do begin
          amplitude.color :=amplitude.color*0.9;
          amplitude.con   :=amplitude.con*0.9;
          amplitude.lin[0]:=amplitude.lin[0]*0.9;
          amplitude.lin[1]:=amplitude.lin[1]*0.9;
        end;
        5: for i:=0 to 5 do with par_trafo[i] do begin
          amplitude.color :=BLACK;
          amplitude.con   :=0;
          amplitude.lin[0]:=0;
          amplitude.lin[1]:=0;
          phaseShift.color :=BLACK;
          phaseShift.con   :=0;
          phaseShift.lin[0]:=0;
          phaseShift.lin[1]:=0;
        end;
        else result:=false;
      end;
    end;

  VAR i,f:longint;
  begin
    if modifyParameters then exit;
    inherited resetParameters(style);
    par_depth  :=128;
    par_seed   :=0;
    par_color  :=0;
    par_bright :=1;
    par_symmex :=0;
    f:=style; if f>1 then f:=1;
    for i:=0 to 5 do with par_trafo[i] do begin
      if style=0 then begin
        offset.color:=rgbColor(i and 1,i shr 1 and 1,i shr 2 and 1);
        amplitude.color:=BLACK;
        phaseShift.color:=BLACK;
      end else begin
        offset    .color:=rgbColor(random,random,random);
        phaseShift.color:=rgbColor(random,random,random);
        amplitude .color:=(phaseShift.color-offset.color)*0.5;
        offset    .color:=(phaseShift.color+offset.color)*0.5;
        phaseShift.color:=rgbColor(2*pi*random,2*pi*random,2*pi*random);
      end;
      offset    .con.re:=f*2*(0.5-random);
      offset    .con.im:=f*2*(0.5-random);
      phaseShift.con.re:=f*2*(0.5-random);
      phaseShift.con.im:=f*2*(0.5-random);
      offset    .lin[0].re:=f*sqrt(2)*(0.5-random);
      offset    .lin[0].im:=f*sqrt(2)*(0.5-random);
      phaseShift.lin[0].re:=f*sqrt(2)*(0.5-random);
      phaseShift.lin[0].im:=f*sqrt(2)*(0.5-random);
      offset    .lin[1].re:=f*sqrt(2)*(0.5-random);
      offset    .lin[1].im:=f*sqrt(2)*(0.5-random);
      phaseShift.lin[1].re:=f*sqrt(2)*(0.5-random);
      phaseShift.lin[1].im:=f*sqrt(2)*(0.5-random);

      amplitude.con   :=(phaseShift.con   -offset.con   )*0.5;
      amplitude.lin[0]:=(phaseShift.lin[0]-offset.lin[0])*0.5;
      amplitude.lin[1]:=(phaseShift.lin[1]-offset.lin[1])*0.5;
      offset.con      :=(phaseShift.con   +offset.con   )*0.5;
      offset.lin[0]   :=(phaseShift.lin[0]+offset.lin[0])*0.5;
      offset.lin[1]   :=(phaseShift.lin[1]+offset.lin[1])*0.5;

      phaseShift.con   .re:=f*2*pi*random;
      phaseShift.con   .im:=f*2*pi*random;
      phaseShift.lin[0].re:=f*2*pi*random;
      phaseShift.lin[0].im:=f*2*pi*random;
      phaseShift.lin[1].re:=f*2*pi*random;
      phaseShift.lin[1].im:=f*2*pi*random;
    end;
    case byte(style) of
      2: for i:=0 to 5 do begin
           with par_trafo[i].amplitude do begin
             color:=BLACK; con:=0; lin[0]:=0; lin[1]:=0;
           end;
           with par_trafo[i].phaseShift do begin
             color:=BLACK; con:=0; lin[0]:=0; lin[1]:=0;
           end;
         end;
      3: for i:=0 to 5 do with par_trafo[i].amplitude do begin
           color :=color *0.5*random;
           con   :=con   *0.5*random;
           lin[0]:=lin[0]*0.5*random;
           lin[1]:=lin[1]*0.5*random;
         end;
    end;
  end;

FUNCTION T_ifs_v2.getAlgorithmName: ansistring;
  begin
    result:='IFS_V2';
  end;

FUNCTION T_ifs_v2.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters + 5 + 72;
  end;

PROCEDURE T_ifs_v2.setParameter(CONST index: byte; CONST value: T_parameterValue);
  VAR i,j:longint;
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: par_depth:=value.i0;
      1: par_seed:=value.i0;
      2: par_color:=value.i0;
      3: par_bright:=value.f0;
      4: par_symmex:=value.i0;
      5..22: begin
        i:=index-inherited numberOfParameters-5;
        j:=i mod 3;
        i:=i div 3;
        case byte(j) of
          0: par_trafo[i].offset    .color:=value.color;
          1: par_trafo[i].amplitude .color:=value.color;
          2: par_trafo[i].phaseShift.color:=value.color;
        end;
      end;
      else begin
        i:=index-inherited numberOfParameters-23;
        j:=i mod 9;
        i:=i div 9;
        case byte(j) of
          0: par_trafo[i].offset    .con   :=value.f0+II*value.f1;
          1: par_trafo[i].amplitude .con   :=value.f0+II*value.f1;
          2: par_trafo[i].phaseShift.con   :=value.f0+II*value.f1;
          3: par_trafo[i].offset    .lin[0]:=value.f0+II*value.f1;
          4: par_trafo[i].amplitude .lin[0]:=value.f0+II*value.f1;
          5: par_trafo[i].phaseShift.lin[0]:=value.f0+II*value.f1;
          6: par_trafo[i].offset    .lin[1]:=value.f0+II*value.f1;
          7: par_trafo[i].amplitude .lin[1]:=value.f0+II*value.f1;
          8: par_trafo[i].phaseShift.lin[1]:=value.f0+II*value.f1;
        end;
      end;
    end;
  end;

FUNCTION T_ifs_v2.getParameter(CONST index: byte): T_parameterValue;
  VAR i,j:longint;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index))
    else case byte(index-inherited numberOfParameters) of
      0: result:=parValue(index,par_depth);
      1: result:=parValue(index,par_seed);
      2: result:=parValue(index,par_color);
      3: result:=parValue(index,par_bright);
      4: result:=parValue(index,par_symmex);
      5..22: begin
        i:=index-inherited numberOfParameters-5;
        j:=i mod 3;
        i:=i div 3;
        case byte(j) of
          0: result:=parValue(index,par_trafo[i].offset    .color);
          1: result:=parValue(index,par_trafo[i].amplitude .color);
          2: result:=parValue(index,par_trafo[i].phaseShift.color);
        end;
      end;
      else begin
        i:=index-inherited numberOfParameters-23;
        j:=i mod 9;
        i:=i div 9;
        case byte(j) of
          0: result:=parValue(index,par_trafo[i].offset    .con   .re,par_trafo[i].offset    .con   .im);
          1: result:=parValue(index,par_trafo[i].amplitude .con   .re,par_trafo[i].amplitude .con   .im);
          2: result:=parValue(index,par_trafo[i].phaseShift.con   .re,par_trafo[i].phaseShift.con   .im);
          3: result:=parValue(index,par_trafo[i].offset    .lin[0].re,par_trafo[i].offset    .lin[0].im);
          4: result:=parValue(index,par_trafo[i].amplitude .lin[0].re,par_trafo[i].amplitude .lin[0].im);
          5: result:=parValue(index,par_trafo[i].phaseShift.lin[0].re,par_trafo[i].phaseShift.lin[0].im);
          6: result:=parValue(index,par_trafo[i].offset    .lin[1].re,par_trafo[i].offset    .lin[1].im);
          7: result:=parValue(index,par_trafo[i].amplitude .lin[1].re,par_trafo[i].amplitude .lin[1].im);
          8: result:=parValue(index,par_trafo[i].phaseShift.lin[1].re,par_trafo[i].phaseShift.lin[1].im);
        end;
      end;
    end;
  end;

FUNCTION getTrafo(CONST T:T_trafoInTime; CONST time:double; OUT contractionFactor:double):T_elementaryTrafo;
  VAR c:T_colorChannel;
  FUNCTION intComplex(CONST o,a,p:T_Complex):T_Complex; inline;
    begin
      result.re:=o.re+a.re*system.sin(time-p.re);
      result.im:=o.im+a.im*system.sin(time-p.im);
    end;

  begin
    for c in RGB_CHANNELS do result.color[c]:=T.offset.color[c]+T.amplitude.color[c]*system.sin(time-T.phaseShift.color[c]);
    result.con   :=intComplex(T.offset.con   ,T.amplitude.con   ,T.phaseShift.con   );
    result.lin[0]:=intComplex(T.offset.lin[0],T.amplitude.lin[0],T.phaseShift.lin[0]);
    result.lin[1]:=intComplex(T.offset.lin[1],T.amplitude.lin[1],T.phaseShift.lin[1]);
    contractionFactor:=abs(result.lin[0].re*result.lin[1].im-result.lin[0].im*result.lin[1].re);
  end;

PROCEDURE T_ifs_v2.prepareSlice(CONST target:P_rawImage; CONST queue:P_progressEstimatorQueue; CONST index:longint);
  VAR colorToAdd:T_rgbFloatColor=(0,0,0);

  PROCEDURE setColor(CONST t:double);
    begin
      with renderTempData do case par_color of
        2,9:  colorToAdd:=rgbColor(max(0,(0.5+t*0.5)*3  ),
                                   max(0,(0.5+t*0.5)*3-1),
                                   max(0,(0.5+t*0.5)*3-2))*par_bright*coverPerSample;
        3,10: colorToAdd:=rgbColor(min(1,max(0,(0.5-t*0.5)*2-1)),
                                   min(1,max(0,(0.5-t*0.5)*2-1)),
                                   min(1,max(0,(0.5-t*0.5)*2  )))*par_bright*coverPerSample;
        4,11: colorToAdd:=hsvColor(0.5+t*0.5,1,par_bright*coverPerSample);
        5:    colorToAdd:=WHITE*par_bright*coverPerSample;
        12:   colorToAdd:=BLACK;
        6,13: colorToAdd:=rgbColor(1,0.5,0)*par_bright*coverPerSample;
        else  colorToAdd:=GREY*par_bright*coverPerSample;
      end;
    end;

  FUNCTION getRandomPoint:T_Complex;
    CONST ctp:array[0..2] of T_Complex=((re:0.5*system.sin(0*pi/3);im:0.5*system.cos(0*pi/3)),
                                        (re:0.5*system.sin(2*pi/3);im:0.5*system.cos(2*pi/3)),
                                        (re:0.5*system.sin(4*pi/3);im:0.5*system.cos(4*pi/3)));

    VAR xx:double;
    begin
      result:=II;
      case par_seed of
        0: begin
          repeat
            result.re:=2*random-1;
            result.im:=2*random-1;
            xx:=sqrabs(result);
          until (xx<1) and (xx<>0);
          result:=result*system.sqrt(-2*system.ln(xx)/xx);
        end;
        1: begin
          repeat
            result.re:=2*random-1;
            result.im:=2*random-1;
            xx:=sqrabs(result);
          until (xx<1) and (xx<>0);
          result:=result*0.5E-2;
        end;
        2: begin
          xx:=2*pi*random;
          result.re:=0.5*system.cos(xx);
          result.im:=0.5*system.sin(xx);
        end;
        3: begin
          xx:=random;
          case random(3) of
            0: result:=ctp[0]+(ctp[1]-ctp[0])*xx;
            1: result:=ctp[1]+(ctp[2]-ctp[1])*xx;
            2: result:=ctp[2]+(ctp[0]-ctp[2])*xx;
          end;
        end;
      end;
    end;

  VAR temp:T_rawImage;
      blurAid:array[0..1] of double;
  FUNCTION putPixel(px:T_Complex):boolean;
    CONST c1 =system.cos(2*pi/3); s1 =system.sin(2*pi/3);
          c2 =system.cos(4*pi/3); s2 =system.sin(4*pi/3);
          cp1=system.cos(2*pi/5); sp1=system.sin(2*pi/5);
          cp2=system.cos(4*pi/5); sp2=system.sin(4*pi/5);
          cp3=system.cos(6*pi/5); sp3=system.sin(6*pi/5);
          cp4=system.cos(8*pi/5); sp4=system.sin(8*pi/5);

    PROCEDURE put(CONST x,y:double); inline;
      VAR sx:T_Complex;
      begin
        sx:=scaler.mrofsnart(x,y);
        sx.re:=sx.re+darts_delta[index,0];
        sx.im:=sx.im+darts_delta[index,1];
        if (sx.re>-0.5) and (sx.re<renderTempData.maxPixelX) and
           (sx.im>-0.5) and (sx.im<renderTempData.maxPixelY) then begin
          temp.multIncPixel(round(sx.re),
                            round(sx.im),
                            renderTempData.antiCoverPerSample,
                            colorToAdd);
          result:=true;
        end;
      end;
    VAR i,j:longint;
    begin
      result:=false;
      if par_symmex<>9 then put(px.re,px.im);
      case par_symmex of
        1: put(-px.re,px.im);
        2: put(px.re,-px.im);
        3: begin
          put(-px.re, px.im);
          put( px.re,-px.im);
          put(-px.re,-px.im);
        end;
        4: put(-px.re,-px.im);
        5: begin
          put(c1*px.re+s1*px.im,c1*px.im-s1*px.re);
          put(c2*px.re+s2*px.im,c2*px.im-s2*px.re);
        end;
        6: begin
          put( px.im,-px.re);
          put(-px.re,-px.im);
          put(-px.im, px.re);
        end;
        7: begin
          put(cp1*px.re+sp1*px.im,cp1*px.im-sp1*px.re);
          put(cp2*px.re+sp2*px.im,cp2*px.im-sp2*px.re);
          put(cp3*px.re+sp3*px.im,cp3*px.im-sp3*px.re);
          put(cp4*px.re+sp4*px.im,cp4*px.im-sp4*px.re);
        end;
        8: for i:=0 to 1 do put(px.re*blurAid[i],px.im*blurAid[i]);
        9: for i:=-2 to 2 do for j:=-2 to 2 do put(px.re+i,px.im+j);
      end;
    end;

  VAR x,y,k:longint;
      t,dt:double;
      missedBefore:boolean;
      maxContraction:double;
      cTrafo:array[0..5] of record
               trafo:T_elementaryTrafo;
               contraction:double;
             end;
      px:T_Complex;

  FUNCTION pickTrafo:longint;
    begin
      repeat result:=random(length(cTrafo)) until random<cTrafo[result].contraction;
    end;

  begin
    with renderTempData do if index<aaSamples then begin
      with renderTempData do begin
        if hasBackground and (backgroundImage<>nil)
        then temp.create(backgroundImage^)
        else begin
          temp.create(xRes,yRes);
          if par_color in [0..6]
          then for y:=0 to yRes-1 do for x:=0 to xRes-1 do temp[x,y]:=BLACK
          else for y:=0 to yRes-1 do for x:=0 to xRes-1 do temp[x,y]:=WHITE;
        end;
        system.enterCriticalSection(flushCs);
        dt:=  2*par_depth/timesteps;
        t:=-1+dt*samplesFlushed/aaSamples;
        dt:=  dt/(1+par_depth);
        system.leaveCriticalSection(flushCs);
      end;

      while t<1 do begin
        maxContraction:=0;
        for k:=0 to 5 do begin
          cTrafo[k].trafo:=getTrafo(par_trafo[k],t*2*pi,cTrafo[k].contraction);
          if cTrafo[k].contraction>maxContraction then maxContraction:=cTrafo[k].contraction;
        end;
        if maxContraction<1E-6
        then for k:=0 to 5 do cTrafo[k].contraction:=1
        else begin
          maxContraction:=1/maxContraction;
          for k:=0 to 5 do cTrafo[k].contraction:=cTrafo[k].contraction*maxContraction;
        end;

        setColor(t);
        px:=getRandomPoint;
        missedBefore:=false;
        blurAid[0]:=1-0.5*abs(random+random-1);
        blurAid[1]:=1-0.5*abs(random+random-1);

        for k:=1 to par_depth do begin
          with cTrafo[pickTrafo].trafo do begin
            px:=con+lin[0]*px.re+lin[1]*px.im;
            case par_color of
              0,7: colorToAdd:=(colorToAdd*0.5)+(color*par_bright*coverPerSample);
              1,8: colorToAdd:=color*par_bright*coverPerSample;
            end;
          end;
          if putPixel(px) then begin
            t:=t+dt;
            missedBefore:=false;
          end else begin
            if missedBefore then break else
               missedBefore:=true;
          end;
        end;
        t:=t+dt;
      end;

      if not(queue^.cancellationRequested) then begin
        system.enterCriticalSection(flushCs);
        t:=1/(samplesFlushed+1);
        for y:=0 to yRes-1 do for x:=0 to xRes-1 do target^[x,y]:=(target^[x,y]*samplesFlushed+temp[x,y])*t;
        inc(samplesFlushed);
        system.leaveCriticalSection(flushCs);
      end;
      temp.destroy;
    end;
  end;

FUNCTION newIfsV2:P_generalImageGenrationAlgorithm; begin new(P_ifs_v2(result),create); end;
INITIALIZATION
  registerAlgorithm('IFS_V2',@newIfsV2,true,false,false);
end.
