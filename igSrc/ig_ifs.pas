UNIT ig_ifs;
INTERFACE
USES imageGeneration,myColors,complex,myParams,sysutils,myGenerics,mypics,math,myFiles,darts;
TYPE
  T_Trafo=record
       rgb:T_floatColor;
       con:T_Complex;
       lin,
       qdr:array[0..1] of T_Complex;
     end;

  T_TrafoTriplet=array[0..2] of T_Trafo;
  P_ifs=^T_ifs;
  T_ifs=object(T_pixelThrowerAlgorithm)
    par_depth  :longint;//=128;
    par_seed   :byte   ;//=3;
    par_color  :byte   ;//=0;
    par_bright :single ;//=1;
    par_symmex :byte   ;//=0;
    par_trafo  :array[0..2] of T_TrafoTriplet; //=3*18=54

    CONSTRUCTOR create;
    FUNCTION parameterResetStyles:T_arrayOfString; virtual;
    PROCEDURE resetParameters(CONST style:longint); virtual;
    FUNCTION getAlgorithmName:ansistring; virtual;
    FUNCTION numberOfParameters:longint; virtual;
    PROCEDURE setParameter(CONST index:byte; CONST value:T_parameterValue); virtual;
    FUNCTION getParameter(CONST index:byte):T_parameterValue; virtual;
    PROCEDURE prepareSlice(CONST index:longint); virtual;
    PROCEDURE load(CONST fileName:string);
  end;

IMPLEMENTATION

CONSTRUCTOR T_ifs.create;
  CONST seedNames:array[0..3] of string=('Gauss','Circle','Line','Triangle');
        colorNames:array[0..13] of string=('Normal/b','Crisp/b','Fire/b','Ice/b','Rainbow/b','White/b','Orange/b',
                                            'Normal/w','Crisp/w','Fire/w','Ice/w','Rainbow/w','Black/w','Orange/w');
        postStepNames:array[0..9] of string=('None','Mirror X','Mirror Y','Mirror XY','Mirror Center','Rotate 3','Rotate 4','Rotate 5','Blur','Shift');
  VAR i,j:longint;
  begin
    inherited create;
    {0}addParameter('depth',pt_integer,1);
    {1}addParameter('seed type',pt_enum,0,2)^.setEnumValues(seedNames);
    {2}addParameter('coloring',pt_enum,0,10)^.setEnumValues(colorNames);
    {3}addParameter('brightness',pt_float,0);
    {4}addParameter('post-step',pt_enum,0,10)^.setEnumValues(postStepNames);
    for i:=0 to 2 do //Trafo-triplet-index
    for j:=0 to 2 do //Trafo index in triplet
      {5+3i+j}addParameter('Color('+intToStr(i)+','+intToStr(j)+')',pt_color);
    for i:=0 to 2 do //Trafo-triplet-index
    for j:=0 to 2 do begin //Trafo index in triplet
      {14+(3i+j)*5+0} addParameter('con('+intToStr(i)+','+intToStr(j)+')',pt_2floats);
      {14+(3i+j)*5+1} addParameter('lin0('+intToStr(i)+','+intToStr(j)+')',pt_2floats);
      {14+(3i+j)*5+2} addParameter('lin1('+intToStr(i)+','+intToStr(j)+')',pt_2floats);
      {14+(3i+j)*5+3} addParameter('sqr0('+intToStr(i)+','+intToStr(j)+')',pt_2floats);
      {14+(3i+j)*5+4} addParameter('sqr1('+intToStr(i)+','+intToStr(j)+')',pt_2floats);
    end;
    resetParameters(0);
  end;

FUNCTION T_ifs.parameterResetStyles: T_arrayOfString;
  begin
    result:='Zero';
    append(result,'Random');
    append(result,'Looped');
    append(result,'Divergent');
    append(result,'Convergent');
    append(result,'Sharpened');
    append(result,'Div-/Convergent');
    append(result,'Sharp');
    append(result,'Modified');
  end;

OPERATOR *(x:T_Trafo; y:single):T_Trafo;
  VAR i:longint;
  begin
    result.rgb:=x.rgb*y;
                     result.con   :=x.con   *y;
    for i:=0 to 1 do result.lin[i]:=x.lin[i]*y;
    for i:=0 to 1 do result.qdr[i]:=x.qdr[i]*y;
  end;

OPERATOR +(x,y:T_Trafo):T_Trafo;
  VAR i:longint;
  begin
    result.rgb:=x.rgb+y.rgb;
                     result.con   :=x.con   +y.con;
    for i:=0 to 1 do result.lin[i]:=x.lin[i]+y.lin[i];
    for i:=0 to 1 do result.qdr[i]:=x.qdr[i]+y.qdr[i];
  end;

PROCEDURE T_ifs.resetParameters(CONST style: longint);
  VAR i,j,f:longint;
  begin
    inherited resetParameters(style);
    par_depth  :=128;
    par_seed   :=0;
    par_color  :=0;
    par_bright :=1;
    par_symmex :=0;
    f:=style; if f>1 then f:=1;
    for i:=0 to 2 do for j:=0 to 2 do with par_trafo[i,j] do begin
      if style=0
      then rgb:=newColor((i+3*j) and 1,
                         (i+3*j) shr 1 and 1,
                         (i+3*j) shr 2 and 1)
      else rgb:=newColor(random,random,random);
      con.re:=f*2*(0.5-random);
      con.im:=f*2*(0.5-random);
      lin[0].re:=f*sqrt(2)*(0.5-random);
      lin[0].im:=f*sqrt(2)*(0.5-random);
      lin[1].re:=f*sqrt(2)*(0.5-random);
      lin[1].im:=f*sqrt(2)*(0.5-random);
      qdr[0].re:=f*sqrt(2)*(0.5-random);
      qdr[0].im:=f*sqrt(2)*(0.5-random);
      qdr[1].re:=f*sqrt(2)*(0.5-random);
      qdr[1].im:=f*sqrt(2)*(0.5-random);
    end;
    case style of
      2: begin
           par_trafo[1,0]:=par_trafo[0,1];
           par_trafo[1,1]:=par_trafo[0,2];
           par_trafo[1,2]:=par_trafo[0,0];
           par_trafo[2,0]:=par_trafo[0,2];
           par_trafo[2,1]:=par_trafo[0,0];
           par_trafo[2,2]:=par_trafo[0,1];
         end;
      3: begin
           par_trafo[1,0]:=par_trafo[0,0];
           par_trafo[2,0]:=par_trafo[0,0];
         end;
      4: for i:=0 to 2 do with par_trafo[i,2] do begin con.re:=0; con.im:=0; end;
      5: for i:=0 to 2 do for j:=1 to 2 do par_trafo[i,j]:=par_trafo[i,j]*0.01+par_trafo[i,0]*0.99;
      6: begin
           par_trafo[1,0]:=par_trafo[0,0];
           par_trafo[2,0]:=par_trafo[0,0];
           par_trafo[1,2]:=par_trafo[0,2];
           par_trafo[2,2]:=par_trafo[0,2];
         end;
      7: for i:=0 to 2 do for j:=1 to 2 do par_trafo[i,j]:=par_trafo[i,0];
      8: for i:=0 to 2 do for j:=2 downto 0 do par_trafo[i,j]:=par_trafo[i,0]*((1+j)/3);
    end; //case
  end;

FUNCTION T_ifs.getAlgorithmName: ansistring;
  begin
    result:='IFS';
  end;

FUNCTION T_ifs.numberOfParameters: longint;
  begin
    result:=inherited numberOfParameters + 59;
  end;

PROCEDURE T_ifs.setParameter(CONST index: byte; CONST value: T_parameterValue);
  VAR i,j,k:longint;
  begin
    if index<inherited numberOfParameters then inherited setParameter(index,value)
    else case byte(index-inherited numberOfParameters) of
      0: par_depth:=value.i0;
      1: par_seed:=value.i0;
      2: par_color:=value.i0;
      3: par_bright:=value.f0;
      4: par_symmex:=value.i0;
      5..13: begin
        i:=index-inherited numberOfParameters-5;
        j:=i mod 3;
        i:=i div 3;
        par_trafo[i,j].rgb:=value.color;
      end;
      else begin
        k:=index-inherited numberOfParameters-14;
        j:=(k div 5) mod 3;
        i:=(k div 5) div 3;
        k:= k mod 5;
        with par_trafo[i,j] do case byte(k) of
          0: begin con.re:=value.f0; con.im:=value.f1; end;
          1: begin lin[0].re:=value.f0; lin[0].im:=value.f1; end;
          2: begin lin[1].re:=value.f0; lin[1].im:=value.f1; end;
          3: begin qdr[0].re:=value.f0; qdr[0].im:=value.f1; end;
          4: begin qdr[1].re:=value.f0; qdr[1].im:=value.f1; end;
        end;
      end;
    end;
  end;

FUNCTION T_ifs.getParameter(CONST index: byte): T_parameterValue;
  VAR i,j,k:longint;
  begin
    if index<inherited numberOfParameters then exit(inherited getParameter(index))
    else case byte(index-inherited numberOfParameters) of
      0: result:=parValue(index,par_depth);
      1: result:=parValue(index,par_seed);
      2: result:=parValue(index,par_color);
      3: result:=parValue(index,par_bright);
      4: result:=parValue(index,par_symmex);
      5..13: begin
        i:=index-inherited numberOfParameters-5;
        j:=i mod 3;
        i:=i div 3;
        result:=parValue(index,par_trafo[i,j].rgb);
      end;
      else begin
        k:=index-inherited numberOfParameters-14;
        j:=(k div 5) mod 3;
        i:=(k div 5) div 3;
        k:= k mod 5;
        with par_trafo[i,j] do case byte(k) of
          0: result:=parValue(index,con   .re,con   .im);
          1: result:=parValue(index,lin[0].re,lin[0].im);
          2: result:=parValue(index,lin[1].re,lin[1].im);
          3: result:=parValue(index,qdr[0].re,qdr[0].im);
          4: result:=parValue(index,qdr[1].re,qdr[1].im);
        end;
      end;
    end;
  end;

PROCEDURE T_ifs.prepareSlice(CONST index:longint);
  CONST abortRadius=1E3;
  FUNCTION trafoOfT(CONST t:double; CONST tt:T_TrafoTriplet):T_Trafo;
    begin
      result:=tt[0]*(((t-1)*t)*0.5)
             +tt[1]*(1-t*t        )
             +tt[2]*(((t+1)*t)*0.5);
    end;
  VAR colorToAdd:T_floatColor;

  PROCEDURE setColor(CONST t:double);
    begin
      with renderTempData do case par_color of
        2,9:  colorToAdd:=newColor(max(0,(0.5+t*0.5)*3  ),
                                   max(0,(0.5+t*0.5)*3-1),
                                   max(0,(0.5+t*0.5)*3-2))*par_bright*coverPerSample;
        3,10: colorToAdd:=newColor(min(1,max(0,(0.5-t*0.5)*2-1)),
                                   min(1,max(0,(0.5-t*0.5)*2-1)),
                                   min(1,max(0,(0.5-t*0.5)*2  )))*par_bright*coverPerSample;
        4,11: colorToAdd:=hue(0.5+t*0.5)*par_bright*coverPerSample;
        5:    colorToAdd:=white*par_bright*coverPerSample;
        12:   colorToAdd:=black;
        6,13: colorToAdd:=newColor(1,0.5,0)*par_bright*coverPerSample;
        else  colorToAdd:=grey*par_bright*coverPerSample;
      end;
    end;

  FUNCTION getRandomPoint:T_Complex;
    CONST ctp:array[0..2] of T_Complex=((re:0.5*system.sin(0*pi/3);im:0.5*system.cos(0*pi/3)),
                                        (re:0.5*system.sin(2*pi/3);im:0.5*system.cos(2*pi/3)),
                                        (re:0.5*system.sin(4*pi/3);im:0.5*system.cos(4*pi/3)));

    VAR xx:double;
    begin
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
  PROCEDURE putPixel(px:T_Complex);
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
           (sx.im>-0.5) and (sx.im<renderTempData.maxPixelY) then
          temp.multIncPixel(round(sx.re),
                            round(sx.im),
                            renderTempData.antiCoverPerSample,
                            colorToAdd);
      end;
    VAR i,j:longint;
    begin
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
      cTrafo:T_TrafoTriplet;
      px,px2:T_Complex;
  begin
    with renderTempData do if index<aaSamples then begin
      with renderTempData do begin
        if hasBackground and (backgroundImage<>nil)
        then temp.create(backgroundImage^)
        else begin
          temp.create(xRes,yRes);
          if par_color in [0..6]
          then for y:=0 to yRes-1 do for x:=0 to xRes-1 do temp[x,y]:=black
          else for y:=0 to yRes-1 do for x:=0 to xRes-1 do temp[x,y]:=white;
        end;
        system.enterCriticalSection(flushCs);
        dt:=  2*par_depth/timesteps;
        t:=-1+dt*samplesFlushed/aaSamples;
        system.leaveCriticalSection(flushCs);
      end;

      while t<1 do begin
        cTrafo[0]:=trafoOfT(t,par_trafo[0]);
        cTrafo[1]:=trafoOfT(t,par_trafo[1]);
        cTrafo[2]:=trafoOfT(t,par_trafo[2]);
        setColor(t);
        px:=getRandomPoint;
        blurAid[0]:=1-0.5*abs(random+random-1);
        blurAid[1]:=1-0.5*abs(random+random-1);

        for k:=1 to par_depth do begin
          with cTrafo[random(3)] do begin
            px2:=sqr(px);
            px:=con+lin[0]*px.re+lin[1]*px.im+qdr[0]*px2.re+qdr[1]*px2.im;
            case par_color of
              0,7: colorToAdd:=(colorToAdd*0.5)+(rgb*par_bright*coverPerSample);
              1,8: colorToAdd:=rgb*par_bright*coverPerSample;
            end;
          end;
          if (sqrabs(px)>abortRadius) then break else putPixel(px);
        end;
        t:=t+dt;
      end;

      if not(progressQueue.cancellationRequested) then begin
        system.enterCriticalSection(flushCs);
        t:=1/(samplesFlushed+1);
        for y:=0 to yRes-1 do for x:=0 to xRes-1 do generationImage^[x,y]:=(generationImage^[x,y]*samplesFlushed+temp[x,y])*t;
        inc(samplesFlushed);
        system.leaveCriticalSection(flushCs);
      end;
      temp.destroy;
    end;
  end;

PROCEDURE T_ifs.load(CONST fileName:string);
  CONST magicChars='IFSparametersV02';
  VAR f:T_file;
      s:string;
      i,j:longint;
      rcx,rcy,rcz:single;
      legacyTrafo:array[0..2,0..2] of record
         r,g,b:single;
         con:array[0..1]      of single;
         lin:array[0..1,0..1] of single;
         qdr:array[0..1,0..1] of single;
       end;

  begin
    f.createToRead(fileName);
    s:='';
    for i:=1 to length(magicChars) do s:=s+f.readChar;
    par_alpha  :=f.readSingle ;
    par_bright :=f.readSingle ;
    par_depth  :=f.readLongint;
    par_seed   :=f.readByte   ;
    par_color  :=f.readByte   ;
    par_symmex :=f.readByte   ;
    rcx        :=-f.readSingle;
    rcy        :=-f.readSingle;
    rcz        :=0.5/f.readSingle;
    f.readBuf(@legacyTrafo,sizeOf(legacyTrafo));
    scaler.setCenterX(rcx);
    scaler.setCenterY(rcy);
    scaler.setZoom   (rcz);
    f.destroy;
    for i:=0 to 2 do for j:=0 to 2 do with legacyTrafo[i,j] do begin
      par_trafo[i,j].rgb:=newColor(r,g,b);
      par_trafo[i,j].con:=con[0]+II*con[1];
      par_trafo[i,j].lin[0]:=lin[0,0]+II*lin[1,0];
      par_trafo[i,j].lin[1]:=lin[0,1]+II*lin[1,1];
      par_trafo[i,j].qdr[0]:=qdr[0,0]+II*qdr[1,0];
      par_trafo[i,j].qdr[1]:=qdr[0,1]+II*qdr[1,1];
    end;
  end;

FUNCTION newIfs:P_generalImageGenrationAlgorithm; begin new(P_ifs(result),create); end;
INITIALIZATION
  registerAlgorithm('IFS',@newIfs,true,false,false);
end.
