PROGRAM reliefs;{$MACRO ON}
{$fputype sse2}
{$define useImageMagick}
USES {$ifdef UNIX}cmem,cthreads,{$endif}
     myFiles,myPics,gl,glext,glut,sysutils,dateutils,math,complex{$ifdef Windows},windows{$endif},Process,darts,cmdLineParseUtil;
CONST
  integ:array[-1..15] of longint=(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);


VAR numberOfCPUs:longint=2;
    neededBoxWidth:longint=500;
    xRes,yRes,previewLevel{,it}:longint;
    viewScaler,currScaler,renderScaler:T_Scaler;
               currImage ,renderImage :T_floatMap;
    renderThreadID:array[0..15] of TThreadID;
    startOfCalculation:double;
    maxDepth:longint;
    job:record
      name:string;
      xRes,yRes,antiAliasing:string;
    end;
    fractalType   :byte=0;
    juliaOverride:record doOverride:boolean; value:single; end=(doOverride:false; value:0);
    juliaMode     :single=0;
    juliaParam    :T_Complex;
    lightNormal   :T_floatColor;
    materialType  :byte=0;

    fullscreenmode:boolean=false;
    viewState:byte=3;
    threadsRunning:longint;
    mouseX,mouseY,mouseDownX,mouseDownY:longint;
    aaMask:T_ByteMap;
    resampling:boolean;
    movingByMouse:boolean;
    moveLight     :boolean=false;
    repaintPending:boolean=false;
    computeNormals:boolean=true;
    renderTolerance:single=1;
    showResults:boolean=false;
    renderStepUp:longint=0;

PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif} 'display '+ps;
    tempProcess.execute;
    tempProcess.free;
  end;

PROCEDURE storeState(fileName:string);
  VAR f:T_file;
  begin
    f.createToWrite(fileName);
    f.writeByte  (fractalType);
    f.writesingle(juliaMode);
    f.writesingle(juliaParam.re);
    f.writesingle(juliaParam.im);
    f.writesingle(viewScaler.screenCenterX);
    f.writesingle(viewScaler.screenCenterY);
    f.writesingle(viewScaler.relativeZoom);
    f.writeLongint(maxDepth);
    f.writeSingle(lightNormal[0]);
    f.writeSingle(lightNormal[1]);
    f.writeSingle(lightNormal[2]);
    f.writeByte  (materialType);
    f.destroy;
  end;

FUNCTION restoreState(fileName:string):boolean;
  VAR f:T_file;
      sx,sy,z:single;
  begin
    juliaParam.valid:=true;
    if fileExists(fileName) then begin
      f.createToRead(fileName);
      fractalType     :=f.readByte;
      juliaMode       :=f.readsingle;
      juliaParam.re   :=f.readsingle;
      juliaParam.im   :=f.readsingle;
      sx              :=f.readsingle;
      sy              :=f.readsingle;
      z               :=f.readsingle;
      maxDepth        :=f.readLongint;
      lightNormal[0]  :=f.readSingle;
      lightNormal[1]  :=f.readSingle;
      lightNormal[2]  :=f.readSingle;
      materialType    :=f.readbyte;
      result:=f.allOkay and                 //data access okay
             (fractalType  in [0..5]) and    //different plausibility-checks;
             (materialType in [0..9]) and
             (maxDepth>0) and
             (z>0);
      f.destroy;
    end else result:=false;
    if not(result) then begin
      fractalType     :=0;
      juliaMode       :=0;
      juliaParam.re   :=0;
      juliaParam.im   :=0;
      sx              :=-0.75;
      sy              :=0;
      z               :=0.25;
      maxDepth        :=1;
    end;
    viewScaler.recreate(xRes,yRes,sx,sy,z);
  end;

PROCEDURE gWrite(x,y:float; s:string);
  VAR i:longint;

  begin
    glRasterpos2f(x,y);
    for i:=1 to length(s) do glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12,ord(s[i]));
  end;

PROCEDURE fetchRasterPos;
  VAR qrp:array[0..3] of longint;
  begin
    glGetIntegerv(GL_CURRENT_RASTER_POSITION, @qrp[0]);
    if qrp[0]>neededBoxWidth then neededBoxWidth:=qrp[0];
  end;

FUNCTION max(x,y:single):single; begin if x>y then result:=x else result:=y; end;
//FUNCTION min(x,y:double):double; begin if x<y then result:=x else result:=y; end;
FUNCTION max(x,y:longint):longint; begin if x>y then result:=x else result:=y; end;




PROCEDURE draw; cdecl;
  VAR ll,ur,mwc,mrc:T_Complex;
  FUNCTION editMarker(use:boolean):string;
    begin if use and odd(round(now*24*60*60*2)) then result:='_' else result:=''; end;

  begin
    glClearColor (0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    //real coordinates:
    ll:=currScaler.transform(0,currImage.height-1 );
    ur:=currScaler.transform(currImage.width-1,0);
    //screen coordinates:
    ll:=viewScaler.mrofsnart(ll);
    ur:=viewScaler.mrofsnart(ur);
    //open-gl coordinates
    ll.re:=ll.re/(xRes-1);
    ur.re:=ur.re/(xRes-1);
    ll.im:=1-ll.im/(yRes-1);
    ur.im:=1-ur.im/(yRes-1);

    glDisable (GL_BLEND);
    glEnable (GL_TEXTURE_2D);
    glBegin(GL_QUADS);

    glTexCoord2f(0.0, 0.0); glnormal3f(0,0,1); glVertex2f(ll.re,ll.im);
    glTexCoord2f(1.0, 0.0); glnormal3f(0,0,1); glVertex2f(ur.re,ll.im);
    glTexCoord2f(1.0, 1.0); glnormal3f(0,0,1); glVertex2f(ur.re,ur.im);
    glTexCoord2f(0.0, 1.0); glnormal3f(0,0,1); glVertex2f(ll.re,ur.im);
    glEnd();

    glDisable (GL_TEXTURE_2D);
    glEnable (GL_BLEND);
    glDisable(GL_LIGHTING);

    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    if (abs(mouseX-mouseDownX)>20) and (abs(mouseY-mouseDownY)>20) then begin
      if mouseX<mouseDownX then begin
        ll.re:=mouseX    /xRes; ur.re:=mouseDownX/xRes;
      end else begin
        ur.re:=mouseX    /xRes; ll.re:=mouseDownX/xRes;
      end;
      if mouseY>mouseDownY then begin
        ll.im:=1-mouseY    /yRes; ur.im:=1-mouseDownY/yRes;
      end else begin
        ur.im:=1-mouseY    /yRes; ll.im:=1-mouseDownY/yRes;
      end;
      glColor4f(0.5,0.5,0.5,0.5);
      glBegin(gl_quads);
        glVertex2f(0    ,0); glVertex2f(0    ,1);     glVertex2f(ll.re,1);     glVertex2f(ll.re,0);
        glVertex2f(1    ,0); glVertex2f(1    ,1);     glVertex2f(ur.re,1);     glVertex2f(ur.re,0);
        glVertex2f(ll.re,0); glVertex2f(ll.re,ll.im); glVertex2f(ur.re,ll.im); glVertex2f(ur.re,0);
        glVertex2f(ll.re,1); glVertex2f(ll.re,ur.im); glVertex2f(ur.re,ur.im); glVertex2f(ur.re,1);
      glEnd;
    end;

    if viewState in [1,3] then begin
      mrc.re:=0.5*(mouseX+mouseDownX);
      mrc.im:=0.5*(mouseY+mouseDownY);
      mwc:=viewScaler.transform(mrc.re,mrc.im);
      mrc.re:=  mrc.re/xRes;
      mrc.im:=1-mrc.im/yRes;

      glBegin(gl_lInes);
        glColor4f(1,1,1,0); glVertex2f(0     ,mrc.im); glColor4f(1,1,1,1); glVertex2f(mrc.re,mrc.im);
        glColor4f(1,1,1,1); glVertex2f(mrc.re,mrc.im); glColor4f(1,1,1,0); glVertex2f(1     ,mrc.im);
        glColor4f(1,1,1,0); glVertex2f(mrc.re,0);      glColor4f(1,1,1,1); glVertex2f(mrc.re,mrc.im);
        glColor4f(1,1,1,1); glVertex2f(mrc.re,mrc.im); glColor4f(1,1,1,0); glVertex2f(mrc.re,1);
      glEnd;
      glColor3f(1,1,1);
      gWrite(0,mrc.im,floatToStr(mwc.im));
      gWrite(mrc.re,0,floatToStr(mwc.re));
    end;

    if viewState in [2,3] then begin
      glColor4f(0,0,0,0.5);
      glBegin(gl_quads);
        glVertex2f(0  ,1);
        glVertex2f(0  ,1-20*14/yRes);
        glVertex2f((neededBoxWidth+10)/xRes,1-20*14/yRes);
        glVertex2f((neededBoxWidth+10)/xRes,1);
      glEnd;
      neededBoxWidth:=0;
      glColor3f(1,1,1);
      gWrite(0,1-     14/yRes,'[H]elp on/off');       fetchRasterPos;
      gWrite(0,1- 2  *14/yRes,'[C]rosshair on/off');  fetchRasterPos;
      gWrite(0,1- 3.5*14/yRes,'[+/-] zoom in/out');   fetchRasterPos;
      gWrite(0,1- 4.5*14/yRes,'[R]ecenter on mouse'); fetchRasterPos;
      gWrite(0,1- 5.5*14/yRes,'[Left mouse drag] zoom in on region'); fetchRasterPos;
      gWrite(0,1- 6.5*14/yRes,'[Right mouse drag] translate image'); fetchRasterPos;
      case fractalType of
        0: gWrite(0,1-8*14/yRes,'[T]ype Mandelbrot');
        1: gWrite(0,1-8*14/yRes,'[T]ype Mandelbar');
        2,3,4: gWrite(0,1-8*14/yRes,'[T]ype Burning Ship');
      end; fetchRasterPos;
      gWrite(0,1- 9  *14/yRes,'[J]ulianess '+intToStr(round(juliaMode*100))+'%'); fetchRasterPos;
      gWrite(0,1-10  *14/yRes,'[P]arameter'); fetchRasterPos;
      gWrite(0,1-11  *14/yRes,'[D]epth '+intToStr(maxDepth));
      case materialType of
        0: gWrite(0,1-12  *14/yRes,'[M]aterial metal');
        1: gWrite(0,1-12  *14/yRes,'[M]aterial glass');
        2: gWrite(0,1-12  *14/yRes,'[M]aterial plastic');
        3: gWrite(0,1-12  *14/yRes,'[M]aterial fire');
        4: gWrite(0,1-12  *14/yRes,'[M]aterial drugged');
        5: gWrite(0,1-12  *14/yRes,'[M]aterial gold');
        6: gWrite(0,1-12  *14/yRes,'[M]aterial levels');
        7: gWrite(0,1-12  *14/yRes,'[M]aterial perlmutt');
        8: gWrite(0,1-12  *14/yRes,'[M]aterial window');
        9: gWrite(0,1-12  *14/yRes,'[M]aterial line');

      end;
      gWrite(0,1-13.5*14/yRes,'[L]ock/unlock light position');  fetchRasterPos;
      gWrite(0,1-15  *14/yRes,'[F]ullscreen');  fetchRasterPos;
      gWrite(0,1-16  *14/yRes,'[B]itmap generation');  fetchRasterPos;
      gWrite(0,1-17  *14/yRes,'[I]nfo (see console)');  fetchRasterPos;
      gWrite(0,1-18.5*14/yRes,'[Strg+W] Quit');  fetchRasterPos;
    end;
    if viewState in [4..8] then begin
      glColor4f(0,0,0,0.5);
      glBegin(gl_quads);
        glVertex2f(0  ,1);
        glVertex2f(0  ,1-9.5*14/yRes);
        glVertex2f(neededBoxWidth/xRes,1-9.5*14/yRes);
        glVertex2f(neededBoxWidth/xRes,1);
      glEnd;
      neededBoxWidth:=0;
      glColor3f(1,1,1);
      gWrite(0,1-  14/yRes,'[N]ame  : '+job.name+editMarker(viewState=5)); fetchRasterPos;
      gWrite(0,1-2*14/yRes,'[X]-res.: '+job.xRes+editMarker(viewState=6)); fetchRasterPos;
      gWrite(0,1-3*14/yRes,'[Y]-res.: '+job.yRes+editMarker(viewState=7)); fetchRasterPos;
      gWrite(0,1-4*14/yRes,'[A]A-Tol: '+job.antiAliasing+editMarker(viewState=8)); fetchRasterPos;
      if job.name<>'' then gWrite(0,1-5.5*14/yRes,'[S]tart calculation'); fetchRasterPos;
      gWrite(0,1-6.5*14/yRes,'[C]ancel');  fetchRasterPos;
      gWrite(0,1-8*14/yRes,'Hint: Save as .job for later calculation.');  fetchRasterPos;
    end;
    //glFlush();
    glutSwapBuffers();
  end;

FUNCTION toSphere(VAR x:T_Complex):T_floatColor; inline;
  VAR t:single;
  begin
    t:=4/(4+x.re*x.re+x.im*x.im);
    if isNan(t) or isInfinite(t) then t:=0;
    x.valid:=t>0;
    result[0]:=x.re*t;
    result[1]:=x.im*t;
    result[2]:=t*2;
  end;

PROCEDURE step(VAR x,c:T_Complex; OUT riemannHeight:T_compBaseT {riemannX:T_floatColor}); inline;
  CONST thr=1E-1;
  VAR x_re:single;
      t:single;
  begin
    case fractalType of
      0: begin
           x_re:=c.re+x.re*x.re-x.im*x.im;
           x.im:=c.im+2*x.re*x.im;
           x.re:=x_re;
         end;
      1: begin
           x_re:=c.re+x.re*x.re-x.im*x.im;
           x.im:=c.im-2*x.re*x.im;
           x.re:=x_re;
         end;
      2: begin
           x_re:=c.re+x.re*x.re-x.im*x.im;
           if (x.re<0) = (x.im<0)
             then x.im:=c.im-2*x.re*x.im
             else x.im:=c.im+2*x.re*x.im;
           x.re:=x_re;
         end;
      4: begin
           if      x.re<-thr then x.re:=-x.re
           else if x.re< thr then x.re:=x.re*x.re*(3/(2*thr)-x.re*x.re/(2*thr*thr*thr));
           if      x.im<-thr then x.im:=-x.im
           else if x.im< thr then x.im:=x.im*x.im*(3/(2*thr)-x.im*x.im/(2*thr*thr*thr));
           x_re:=c.re+x.re*x.re-x.im*x.im;
           x.im:=c.im-2*x.re*x.im;
           x.re:=x_re;
         end;
      3: begin
           //if      x.re<-thr then x.re:=-x.re
           //else if x.re< thr then x.re:=x.re*x.re*(3/(2*thr)-x.re*x.re/(2*thr*thr*thr));
           //if      x.im<-thr then x.im:=-x.im
           //else if x.im< thr then x.im:=x.im*x.im*(3/(2*thr)-x.im*x.im/(2*thr*thr*thr));
           x_re:=c.re+x.re*x.re-x.im*x.im;
           x.im:=2*x.re*x.im;
           if      x.im<-thr then x.im:=-x.im
           else if x.im< thr then x.im:=x.im*x.im*(3/(2*thr)-x.im*x.im/(2*thr*thr*thr));
           x.im:=c.im-x.im;
           x.re:=x_re;
         end;
      5: x:=1/sqr(x)+c;
    end;
    t:=4/(4+x.re*x.re+x.im*x.im);
    if isNan(t) or isInfinite(t) then t:=0;
    x.valid:=t>0;
    //riemannX[0]:=x.re*t;
    //riemannX[1]:=x.im*t;
    //riemannX[2]:=t*2;
    riemannHeight:=t*2;
  end;


{FUNCTION pot(x:single; y:longint):single;
  begin
    result:=1;
    while y>0 do begin
      if odd(y) then result:=result*x;
      y:=y shr 1;
      x:=x*x;
    end;
  end; }

FUNCTION normalAt(c:T_Complex; maxDepth:longint):T_floatColor;
  CONST h0:T_Complex=(re:-1/3; im:-1/3; valid:true);
        h1:T_Complex=(re:-1/3; im: 2/3; valid:true);
        h2:T_Complex=(re: 2/3; im:-1/3; valid:true);
        l0= sqrt(1/3);
        l1=-sqrt(1/3);
        l2= sqrt(1/3);

  VAR x0,x1,x2,
      c0,c1,c2:T_Complex;
      d0,d1,d2:T_compBaseT;
      rsh     :T_compBaseT;
      i:longint;
  begin

    c0:=c+h0*renderScaler.absoluteZoom; x0:=c0; x0.valid:=true; c0:=((1-juliaMode)*c0+juliaMode*juliaParam);
    c1:=c+h1*renderScaler.absoluteZoom; x1:=c1; x1.valid:=true; c1:=((1-juliaMode)*c1+juliaMode*juliaParam);
    c2:=c+h2*renderScaler.absoluteZoom; x2:=c2; x2.valid:=true; c2:=((1-juliaMode)*c2+juliaMode*juliaParam);

    d0:=0; d1:=0;  d2:=0; i:=0;
    while (x0.valid) and (x1.valid) and (x2.valid) and (i<maxDepth) do begin
      step(x0,c0,rsh); d0:=d0+rsh;
      step(x1,c1,rsh); d1:=d1+rsh;
      step(x2,c2,rsh); d2:=d2+rsh; inc(i);
    end;
    //compute and normalize normal vector:---------------------------------------//
    d0:=sqrt(d0/maxDepth);                                                       //
    d1:=sqrt(d1/maxDepth)-d0;                                                    //
    d2:=sqrt(d2/maxDepth)-d0;                                                    //
    d0:=1/sqrt(d1*d1+d2*d2+renderScaler.absoluteZoom*renderScaler.absoluteZoom); //
    result[2]:=(renderScaler.absoluteZoom*d0);                                   //
    result[0]:=(d1                       *d0);                                   //
    result[1]:=(d2                       *d0);                                   //
    //-----------------------------------------:compute and normalize normal vector
  end;

FUNCTION normalToColor_metal(n:T_floatColor):T_floatColor;
  VAR diffuse,spec4,spec8,spec16:single;
  CONST colAmb   :T_floatColor=(0.1,0.1,0.1);
        colDiff  :T_floatColor=(0.3,0.3,0.3);
        colSpec05:T_floatColor=(0.6,0.0,0.0);
        colSpec08:T_floatColor=(0.0,0.6,0.0);
        colSpec16:T_floatColor=(0.0,0.0,0.6);

  begin
    diffuse :=lightNormal*n;
    spec4:=-(lightNormal[2]-2*n[2]*diffuse);
    if spec4<0 then begin
    result:=colAmb+colDiff*diffuse;
    end else begin
      spec4 :=spec4*spec4;
      spec4 :=spec4*spec4;
      spec8 :=spec4*spec4;
      spec16:=spec8*spec8;
      result:=colAmb+colDiff*diffuse
                    +colSpec05*spec4
                    +colSpec08*spec8
                    +colSpec16*spec16;
    end;

  end;

FUNCTION normalToColor_glass(n:T_floatColor):T_floatColor;
  VAR diffuse,spec:single;
  CONST colAmb   :T_floatColor=(0.0,0.0,0.2);
        colDiff  :T_floatColor=(0.0,0.0,0.5);
        colSpec  :T_floatColor=(0.8,0.8,0.8);


  begin
    diffuse :=lightNormal*n;
    spec:=-(lightNormal[2]-2*n[2]*diffuse);
    if spec<0.999 then begin
    result:=colAmb+colDiff*diffuse;
    end else begin
      result:=colAmb+colDiff*diffuse
                    +colSpec*(spec-0.999)*1000;
    end;

  end;

FUNCTION normalToColor_drugged(n:T_floatColor):T_floatColor;
  CONST colAmb   :T_floatColor=(0.3,0.3,0.3);
        colDiff  :T_floatColor=(0.3,0.3,0.3);
  VAR spec,diffuse:single;
  begin
    diffuse :=lightNormal*n;
    spec:=-(lightNormal[2]-2*n[2]*diffuse);
    if spec<1 then
    result:=colAmb+colDiff*diffuse+spec*hue(spec)
    else result:=black;

  end;

FUNCTION normalToColor_plastic(n:T_floatColor):T_floatColor;
  CONST colAmb   :T_floatColor=(0.5,0.0,0.0);
        colDiff  :T_floatColor=(0.5,0.0,0.0);
        colSpec  :T_floatColor=(0.0,1,1);
  VAR spec,diffuse:single;
  begin
    diffuse :=lightNormal*n;
    spec:=-(lightNormal[2]-2*n[2]*diffuse);
    if spec<0 then result:=colAmb+colDiff*diffuse else begin
      spec:=spec*spec;
      spec:=spec*spec;
      spec:=spec*spec;
      result:=colAmb+colDiff*diffuse+colSpec*spec;
    end;
  end;

FUNCTION normalToColor_fire(n:T_floatColor):T_floatColor;
  VAR x:single;
  begin
    x:=(lightNormal*n);
    x:=1.5+1.5*x;//(lightNormal[2]-2*n[2]*x);
    if      x>2 then begin result[0]:=1; result[1]:=1;   result[2]:=x-2; end
    else if x>1 then begin result[0]:=1; result[1]:=x-1; result[2]:=0;   end
    else             begin result[0]:=x; result[1]:=0;   result[2]:=0;   end;
  end;

FUNCTION normalToColor_gold(n:T_floatColor):T_floatColor;
  CONST colAmb   :T_floatColor=(0.5,0.25, 0);
        colDiff  :T_floatColor=(0.5,0.5,0);
        colSpec  :T_floatColor=(0.5,0.25,0.5);
  VAR spec,diffuse:single;
  begin
    diffuse :=lightNormal*n;
    spec:=-(lightNormal[2]-2*n[2]*diffuse);
    if (spec<0) then result:=colAmb+colDiff*diffuse else begin
      spec:=spec*spec;
      spec:=spec*spec;
      spec:=spec*spec;
      result:=projectedColor(colAmb+colDiff*diffuse+colSpec*spec);
    end;
  end;

FUNCTION normalToColor_levels(n:T_floatColor):T_floatColor;
  begin
    result:=white*(round(5+5*(lightNormal*n))*0.1);
  end;

FUNCTION normalToColor_window(n:T_floatColor):T_floatColor;
  CONST colAmbient:T_floatColor=(0.1,0.05,0.0);
        colDiffuse:T_floatColor=(0.4,0.20,0.0);

  VAR diffuse:single;
      specVec:T_floatColor;
  begin
    diffuse:=lightNormal*n;
    specVec:=lightNormal-n*(2*diffuse);
    result:=colAmbient+diffuse*colDiffuse;
    if (specVec[2]<0) then begin
      if (abs(specVec[1])<0.2 ) and (abs(specVec[0])<0.2 ) and
         (abs(specVec[1])>0.02) and (abs(specVec[0])>0.02) then result:=result+white*0.75
      else result:=result+white*(0.5*specVec[2]*specVec[2]);
    end;
  end;

FUNCTION normalToColor_line(n:T_floatColor):T_floatColor;
  CONST colAmbient:T_floatColor=(0.0,0.3,0.0);
        colDiffuse:T_floatColor=(0.0,0.3,0.0);

  VAR diffuse:single;
      specVec:T_floatColor;
  begin
    diffuse:=lightNormal*n;
    specVec:=lightNormal-n*(2*diffuse);
    result:=colAmbient+diffuse*colDiffuse;
    if (specVec[2]<0) then begin
      specVec[1]:=specVec[1]+specVec[0];
      if (abs(specVec[1])<0.02) then result:=result-white*specVec[2]
      else result:=result+white*(0.7*specVec[2]*specVec[2]*(1-specVec[1]));
    end;
  end;

FUNCTION normalToColor_strange(n:T_floatColor):T_floatColor;
  CONST colAmbient:T_floatColor=(0.1,0.1,0.1);
        colDiffuse:T_floatColor=(0.4,0.4,0.4);

        vec1:T_floatColor=( 0                  , 0.18257418276333211,0.98319208082057976);
        vec2:T_floatColor=( 0.15811387502784748,-0.09128710059683582,0.98319208082057976);
        vec3:T_floatColor=(-0.15811389098898917,-0.0912870729513256 ,0.98319208082057976);
        //vec1:T_floatColor=( 0   ,0.05773502594759839 ,0.99833194218097123);
        //vec2:T_floatColor=( 0.05,-0.02886751588789175,0.99833194218097123);
        //vec3:T_floatColor=(-0.05,-0.02886750714561381,0.99833194218097123);

  VAR diffuse:single;
      specVec,spec:T_floatColor;

  begin
    diffuse:=lightNormal*n;
    specVec:=lightNormal-n*(2*diffuse);
    spec[0]:=specVec*vec1; if spec[0]>0 then spec[0]:=0;
    spec[1]:=specVec*vec2; if spec[1]>0 then spec[1]:=0;
    spec[2]:=specVec*vec3; if spec[2]>0 then spec[2]:=0;



    spec[0]:=spec[0]*spec[0]; spec[0]:=spec[0]*spec[0];
    spec[1]:=spec[1]*spec[1]; spec[1]:=spec[1]*spec[1];
    spec[2]:=spec[2]*spec[2]; spec[2]:=spec[2]*spec[2];
    result[0]:=colAmbient[0]+diffuse*colDiffuse[0]+spec[0]*spec[0]*0.5;
    result[1]:=colAmbient[1]+diffuse*colDiffuse[1]+spec[1]*spec[1]*0.5;
    result[2]:=colAmbient[2]+diffuse*colDiffuse[2]+spec[2]*spec[2]*0.5;
  end;







PROCEDURE mouseMovePassive(x,y:longint); cdecl;
  begin
    repaintPending:=moveLight and ((mouseX<>x) or (mouseY<>y));
    mouseX:=x;
    mouseY:=y;
    mouseDownX:=x;
    mouseDownY:=y;
    if moveLight then begin
      lightNormal[0]:= 2*y/yres-1;
      lightNormal[1]:=-2*x/xres+1;
      lightNormal[2]:=0;
      lightNormal[2]:=norm(lightNormal);
      if lightNormal[2]>1 then begin
        lightNormal[0]:=lightNormal[0]/lightNormal[2];
        lightNormal[1]:=lightNormal[1]/lightNormal[2];
        lightNormal[2]:=0;
      end else begin
        lightNormal[2]:=sqrt(1-system.sqr(lightNormal[2]));
      end;
    end;

    if viewState in [1,3] then glutPostRedisplay; //draw;
  end;


{FUNCTION psqr(x:T_floatColor):single;inline;
  begin
    result:=(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
  end; }

FUNCTION prepareImage(p:pointer):ptrint;
  VAR x,y:longint;
  begin
    for y:=0 to renderImage.height-1 do if (plongint(p)^<0) or (y mod numberOfCPUs=plongint(p)^) then
    if computeNormals then for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalAt(renderScaler.transform(x,y),maxDepth)
    else case materialType of
      0: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_metal  (normalAt(renderScaler.transform(x,y),maxDepth));
      1: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_glass  (normalAt(renderScaler.transform(x,y),maxDepth));
      2: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_plastic(normalAt(renderScaler.transform(x,y),maxDepth));
      3: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_fire   (normalAt(renderScaler.transform(x,y),maxDepth));
      4: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_drugged(normalAt(renderScaler.transform(x,y),maxDepth));
      5: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_gold   (normalAt(renderScaler.transform(x,y),maxDepth));
      6: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_levels (normalAt(renderScaler.transform(x,y),maxDepth));
      7: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_strange(normalAt(renderScaler.transform(x,y),maxDepth));
      8: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_window (normalAt(renderScaler.transform(x,y),maxDepth));
      9: for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=normalToColor_line   (normalAt(renderScaler.transform(x,y),maxDepth));
    end;
    interlockedDecrement(threadsRunning);
    result:=0;
  end;

FUNCTION improveImage(p:pointer):ptrint;
  VAR x,y:longint;
      fc:T_floatColor;
      i,k0,k1:longint;
  begin
    for y:=0 to renderImage.height-1 do if (plongint(p)^<0) or (y mod numberOfCPUs=plongint(p)^) then
    for x:=0 to renderImage.width-1 do if odd(aaMask[x,y]) then begin
      if aaMask[x,y]=1 then begin
        k0:=1;
        k1:=2;
        aamask[x,y]:=2;
        k1:=2*k1;
      end else begin
        k0:=aaMask[x,y]-1;
        k1:=k0+2*(1+renderStepUp);
        if k1>254 then k1:=254;
        aamask[x,y]:=k1;
        k0:=2*k0;
        k1:=2*k1;
      end;
      fc:=renderImage.pixel[x,y]*k0;
      if computeNormals then for i:=k0 to k1-1 do fc:=fc+normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth)
      else case materialType of
        0: for i:=k0 to k1-1 do fc:=fc+normalToColor_metal  (normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
        1: for i:=k0 to k1-1 do fc:=fc+normalToColor_glass  (normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
        2: for i:=k0 to k1-1 do fc:=fc+normalToColor_plastic(normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
        3: for i:=k0 to k1-1 do fc:=fc+normalToColor_fire   (normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
        4: for i:=k0 to k1-1 do fc:=fc+normalToColor_drugged(normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
        5: for i:=k0 to k1-1 do fc:=fc+normalToColor_gold   (normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
        6: for i:=k0 to k1-1 do fc:=fc+normalToColor_levels (normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
        7: for i:=k0 to k1-1 do fc:=fc+normalToColor_strange(normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
        8: for i:=k0 to k1-1 do fc:=fc+normalToColor_window (normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
        9: for i:=k0 to k1-1 do fc:=fc+normalToColor_line   (normalAt(renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]),maxDepth));
      end;
      renderImage.pixel[x,y]:=fc*(1/k1);
    end;
    interlockedDecrement(threadsRunning);
    result:=0;
  end;

PROCEDURE startRendering;
  VAR it:longint;
  begin
    threadsRunning:=numberOfCPUs;
    startOfCalculation:=now;
    for it:=0 to numberOfCPUs-1 do
    {$ifdef UNIX}
      beginThread(@prepareImage,@integ[it],renderThreadID[it]);
    {$else}
      renderThreadID[it]:=beginThread(@prepareImage,@integ[it]);
    {$endif}

  end;

FUNCTION stillRendering:boolean;
  begin
    result:=false;
    result:=(threadsRunning>0);
  end;

PROCEDURE killRendering;
  VAR it:longint;
  begin
    for it:=0 to numberOfCPUs-1 do begin
      if waitForThreadTerminate(renderThreadID[it],1)<>0
      then killThread(          renderThreadID[it]);
    end;
    threadsRunning:=0;
  end;

PROCEDURE copyAndTransform;
  VAR pn,pc:P_floatColor;
      i:longint;
  begin
    currImage.resizeDat(renderImage.width,renderImage.height);
    pn:=renderImage.rawData;
    pc:=currImage  .rawData;
    //for i:=0 to renderImage.size-1 do pc[i]:=normalToColor(pn[i]);

    case materialType of
      0: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_metal  (pn[i]);
      1: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_glass  (pn[i]);
      2: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_plastic(pn[i]);
      3: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_fire   (pn[i]);
      4: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_drugged(pn[i]);
      5: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_gold   (pn[i]);
      6: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_levels (pn[i]);
      7: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_strange(pn[i]);
      8: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_window (pn[i]);
      9: for i:=0 to renderImage.size-1 do pc[i]:=normalToColor_line   (pn[i]);
    end;
    repaintPending:=false;
  end;

PROCEDURE update; cdecl;
  begin
    if not(stillRendering) then begin
      if previewLevel>0 then begin
        //currImage.destroy; currImage.createCopy(renderImage);
        copyAndTransform;
        currScaler:=renderScaler;
        glTexImage2D (GL_TEXTURE_2D                     ,0,GL_RGB,currImage.width,currImage.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,currImage.rawData);
        dec(previewLevel);
        glutPostRedisplay;
        renderImage.resizeDat(xRes shr previewLevel,yRes shr previewLevel);
        renderScaler:=viewScaler;
        renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
        startRendering;
      end else if previewLevel>-200 then begin
        if previewLevel=0 then writeln('full resolution ready (',(now-startOfCalculation)*24*60*60:7:2,'sec)');
        //currImage.destroy; currImage.createCopy(renderImage);
        copyAndTransform;
        currScaler:=renderScaler;
        glTexImage2D (GL_TEXTURE_2D,                     0,GL_RGB,currImage.width,currImage.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,currImage.rawData);
        glutPostRedisplay;
        dec(previewLevel);
        //if (previewLevel=-1) or (resampling) then startImproving(-1-previewLevel)
        //                                     else
        previewLevel:=-200;
      end else if previewLevel=-200 then begin
        if repaintPending then begin
          copyAndTransform;
          currScaler:=renderScaler;
          glTexImage2D (GL_TEXTURE_2D, 0,GL_RGB,currImage.width,currImage.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,currImage.rawData);
          glutPostRedisplay;
        end else sleep(100);
      end else sleep(10);
    end else if (renderScaler.relativeZoom<0.5*viewScaler.relativeZoom) or (renderScaler.relativeZoom>2*viewScaler.relativeZoom) then begin
      killRendering;
      previewLevel:=4;
      renderImage.destroy;
      renderImage.create(xRes shr previewLevel,yRes shr previewLevel);
      renderScaler:=viewScaler;
      renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
      startRendering;
    end;
    if viewState in [5..8] then glutPostRedisplay; //to make the caret blink...
  end;


PROCEDURE reshape(newXRes,newYRes:longint); cdecl;
  begin
    if (newXRes>0) and (newYRes>0) and ((newXRes<>xRes) or (newYRes<>yRes)) then begin
      mouseDownX:=0; mouseX:=0;
      mouseDownY:=0; mouseY:=0;
      viewScaler.rescale(newXRes,newYRes);
      xRes:=newxRes;
      yRes:=newyRes;
      glViewport(0, 0,xres,yres);
      glLoadIdentity;
      glOrtho(0, 1, 0, 1, -10.0, 10.0);
      glMatrixMode(GL_MODELVIEW);
      killRendering;
      previewLevel:=4;
      glutPostRedisplay();
    end;
  end;


PROCEDURE mouseMoveActive(x,y:longint); cdecl;
  begin
    mouseX:=x;
    mouseY:=y;
    if movingByMouse then begin
      viewScaler.moveCenter(mouseX-mouseDownX,mouseY-mouseDownY);
      mouseDownX:=mouseX;
      mouseDownY:=mouseY;
      glutPostRedisplay;
    end;
    glutPostRedisplay; //draw;
  end;

PROCEDURE mousePressFunc(button,state,x,y:longint); cdecl;
  begin
    if (state=glut_down) then movingByMouse:=(button=2)
    else if (state=glut_up) and movingByMouse then begin
      previewLevel:=4;
    end else if (state=glut_up) and (abs(mouseX-mouseDownX)>20) and (abs(mouseY-mouseDownY)>20) then begin
      viewScaler.recenter(viewScaler.transform((mouseX+mouseDownX)*0.5,(mouseY+mouseDownY)*0.5).re,
                          viewScaler.transform((mouseX+mouseDownX)*0.5,(mouseY+mouseDownY)*0.5).im);
      viewScaler.rezoom(viewScaler.relativeZoom*(sqrt((xRes*xRes+yRes*yRes)/
        (system.sqr(mouseX-mouseDownX)+system.sqr(mouseX-mouseDownX)))));
      previewLevel:=4;
    end;
  end;

PROCEDURE doJob(beLazy,normalsOnly:boolean);
  VAR it:longint;
      progress:record
        sppOutput,idxOutput:longint;
        oldTime,thisTime:double;
        oldSpp ,thisSpp ,newSpp :double;
        timePerSample:double;
        tolP:longint;
      end;

      useTolerance:double;

  PROCEDURE initProgress;
    begin
      with progress do begin
        idxOutput:=0;
        sppOutput:=0;
        thisTime:=now;
        thisSpp :=0;
        newSpp  :=1;

        tolP    :=0;
        useTolerance:=strToFloat(job.antiAliasing);
        while useTolerance<30 do begin
          useTolerance:=useTolerance*sqrt(2);
          inc(tolP);
        end;
      end;

    end;

  PROCEDURE stepProgress_beforeMark;
    begin
      with progress do begin
        oldSpp  :=thisSpp;
        oldTime :=thisTime;
        thisTime:=now;
      end;
      write('tol',useTolerance:5:2,' ');
    end;

  PROCEDURE decTol;
    begin
      if progress.tolP>0 then begin
        dec(progress.tolP);
        useTolerance:=useTolerance*sqrt(0.5);
        renderStepUp:=0;
      end;
    end;

  PROCEDURE stepProgress_afterMark;
    VAR sps,timeLeft:double;
        k:longint;
    begin
      with progress do begin
        timePerSample:=(oldTime-thisTime)/(oldSpp-thisSpp);
        timeLeft:=timePerSample*(newSpp-thisSpp);

        k:=round(1/(24*60*(timeLeft))-1);
        if k>renderStepUp then inc(renderStepUp)
                          else renderStepUp:=k;
        if renderStepUp<0 then renderStepUp:=0;
        if renderStepUp>99 then renderStepUp:=99;
        if (renderStepUp>0) and (tolP>0) then decTol;
        if renderStepUp>0 then write('+',renderStepUp:2,' ')
                          else write('    ');
        write(' (',mytimeToStr(timeLeft*(1+renderStepUp)),' rem. -> @',timeToStr(timeLeft*(1+renderStepUp)+thisTime),' ');

        sps:=timePerSample*(24*60*60)/(xRes*yRes);
        if      sps>=1    then write(sps:6:2    ,'s ) ')
        else if sps>=1E-3 then write(sps*1E3:6:2,'ms) ')
        else if sps>=1E-6 then write(sps*1E6:6:2,#230+'s) ') //microseconds
                          else write(sps*1E9:6:2,'ns) ');
        writeln;
      end;
    end;

  begin
    if extractFileExt(job.name)='.job' then storeState(job.name)
    else begin
      if normalsOnly then begin
        computeNormals:=true;
        renderScaler:=viewScaler;
        writeln('Computing normals');
        startOfCalculation:=now;
        killRendering;
        renderImage.resizeDat(strToInt(job.xRes),strToInt(job.yRes));
        renderScaler.rescale (strToInt(job.xRes),strToInt(job.yRes));
        previewLevel:=0;
        for it:=1 to numberOfCPUs-1 do
          {$ifdef UNIX} beginThread(@prepareImage,@integ[it],renderThreadID[it]);
          {$else}       renderThreadID[it]:=beginThread(@prepareImage,@integ[it]); {$endif}
        prepareImage(@integ[0]);
        for it:=1 to numberOfCPUs-1 do repeat sleep(1) until waitForThreadTerminate(renderThreadID[it],1)=0;
      end else begin
        computeNormals:=false;
        renderScaler:=viewScaler;
        writeln('Rendering to file ',job.name,'...');
        initProgress;
        startOfCalculation:=now;

        killRendering;
        renderImage.resizeDat(strToInt(job.xRes),strToInt(job.yRes));
        renderScaler.rescale (strToInt(job.xRes),strToInt(job.yRes));
        previewLevel:=0;
        for it:=1 to numberOfCPUs-1 do
          {$ifdef UNIX} beginThread(@prepareImage,@integ[it],renderThreadID[it]);
          {$else}       renderThreadID[it]:=beginThread(@prepareImage,@integ[it]); {$endif}
        prepareImage(@integ[0]);
        for it:=1 to numberOfCPUs-1 do repeat sleep(1) until waitForThreadTerminate(renderThreadID[it],1)=0;

        if not(belazy) then begin
          aaMask.resizeDat(renderImage.width,renderImage.height);
          aaMask.setToValue(0);

          writeln('first guess ready (',mytimeToStr(now-startOfCalculation),') ');

          stepProgress_beforeMark;
          markAlias_gamma(renderImage,aaMask,useTolerance,outputWithOutLineBreak,progress.thisSpp,progress.newSpp,resampling);
          if not(resampling) and (progress.tolp>0) then repeat
            decTol; writeln; stepProgress_beforeMark;
            markAlias_gamma(renderImage,aaMask,useTolerance,outputWithOutLineBreak,progress.thisSpp,progress.newSpp,resampling);
          until resampling or (progress.tolp=0);
          renderStepUp:=-1;
          if resampling then repeat
            stepProgress_afterMark;
            if aaMask.countOdd>1000 then begin
              for it:=1 to numberOfCPUs-1 do
                {$ifdef UNIX} beginThread(@improveImage,@integ[it],renderThreadID[it]);
                {$else}       renderThreadID[it]:=beginThread(@improveImage,@integ[it]);
                {$endif}
              improveImage(@integ[0]);
              for it:=1 to numberOfCPUs-1 do repeat sleep(1) until waitForThreadTerminate(renderThreadID[it],1)=0;
            end else improveImage(@integ[-1]);
            stepProgress_beforeMark;
            markAlias_gamma(renderImage,aaMask,useTolerance,outputWithOutLineBreak,progress.thisSpp,progress.newSpp,resampling);
            if not(resampling) and (progress.tolp>0) then repeat
              decTol; writeln; stepProgress_beforeMark;
              markAlias_gamma(renderImage,aaMask,useTolerance,outputWithOutLineBreak,progress.thisSpp,progress.newSpp,resampling);
            until resampling or (progress.tolp=0);
          until not(resampling);//aaMask.countOdd=0;
        end;
        if uppercase(extractFileExt(job.name))<>'.VRAW' then begin
          shineImage(renderImage);
          colorManipulate(fk_project,0,0,0,renderImage);
        end;
        renderImage.saveToFile(job.name);
        if showResults then backgroundDisplay(job.name);
        writeln(' done in ',(now-startOfCalculation)*24*60*60:0:3,'sec');
        currImage.destroy;
        currImage.createCopy(renderImage);
        currScaler:=         renderScaler;
        previewLevel:=-2;
        computeNormals:=true;
      end;
    end;
  end;

PROCEDURE keyboard(key:byte; x,y:longint); cdecl;
  VAR rerender:boolean;
  begin
    rerender:=false;
    if viewState<4 then case key of
      ord('m'): begin materialType:=(materialType+1) mod 10; repaintPending:=true; end;
      ord('M'): begin materialType:=(materialType+9) mod 10; repaintPending:=true; end;
      ord('l'),ord('L'): moveLight:=not(moveLight);
      ord('i'),ord('I'): writeln('julianess= ',juliamode:0:2,'; juliaParam=',juliaParam.re,'+i*',juliaParam.im);
      ord('b'),ord('B'):
        begin
          job.xRes:=intToStr(xRes);
          job.yRes:=intToStr(yRes);
          viewState:=4; glutPostRedisplay;
        end;
      ord('r'),ord('R'):
          begin
            viewScaler.recenter(viewScaler.transform(x,y).re,viewScaler.transform(x,y).im);
            previewLevel:=4;
            glutPostRedisplay; //draw;
          end;
      ord('p'),ord('P'):
          begin
            juliaParam:=viewScaler.transform(x,y);
            killRendering;
            rerender:=true;
          end;
      ord('j'),ord('J'):
          begin
            if key=ord('j') then begin juliaMode:=juliaMode+0.1; if juliaMode>1 then juliaMode:=1; end
                            else begin juliaMode:=juliaMode-0.1; if juliaMode<0 then juliaMode:=0; end;
            killRendering;
            rerender:=true;
          end;
      ord('d'): begin
            killRendering;
            if      maxDepth>=200 then inc(maxDepth,100)
            else if maxDepth>=20  then inc(maxDepth,10)
            else               inc(maxDepth);
            rerender:=true
          end;
      ord('D'): begin
            killRendering;
            if maxDepth>1 then begin
              if      maxDepth>=300 then dec(maxDepth,100)
              else if maxDepth>=30  then dec(maxDepth,10)
              else                dec(maxDepth);
              rerender:=true
            end;
          end;
      ord('c'),ord('C'):
          begin
            viewState:=viewState xor 1;
            glutPostRedisplay;
          end;
      ord('h'),ord('H'):
          begin
            viewState:=viewState xor 2;
            glutPostRedisplay;
          end;
      ord('t'),ord('T'):
          begin
            killRendering;
            if key=ord('T')
              then fractalType:=(fractalType+4) mod 5
              else fractalType:=(fractalType+1) mod 5;
            rerender:=true;
          end;
      23: //Strg+W
          begin killRendering; storeState('relief.state'); currImage.destroy; renderImage.destroy;  halt; end;
      43: //+
          begin
            viewScaler.chooseScreenRef(x,y);
            viewScaler.rezoom(viewScaler.relativeZoom*1.1);
            previewLevel:=4;
            glutPostRedisplay; //draw;
          end;
      45: //-
          begin
            viewScaler.chooseScreenRef(x,y);
            viewScaler.rezoom(viewScaler.relativeZoom/1.1);
            previewLevel:=4;
            glutPostRedisplay; //draw;
          end;
      ord('f'),ord('F'):
          begin
            fullscreenmode:=not(fullscreenmode);
            if fullscreenmode then begin
              glutfullscreen;
            end else begin
              {$ifdef Windows}
              glutReshapeWindow (GetSystemMetrics(SM_CXSCREEN) shr 1,
                                 GetSystemMetrics(SM_CYSCREEN) shr 1);
              glutPositionWindow(GetSystemMetrics(SM_CXSCREEN) shr 2,
                                 GetSystemMetrics(SM_CYSCREEN) shr 2);
              {$else}
              glutReshapeWindow (512,512);
              glutPositionWindow(50,50);
              reshape(512,512);
              {$endif}

            end;
          end;
    end else case viewState of
      4: begin
           case chr(key) of
             'C','c': viewState:=3;
             'N','n': viewState:=5;
             'X','x': viewState:=6;
             'Y','y': viewState:=7;
             'A','a': viewState:=8;
             'S','s': if job.name<>'' then begin
                        doJob(false,false);
                        rerender:=true;
                        viewState:=3;
                      end;
           end;
           glutPostRedisplay;
         end;
      5: begin
           case chr(key) of
             'a'..'z','A'..'Z','0'..'9','.','_',':','/','\':job.name:=job.name+chr(key);
             chr(8) : job.name:=copy(job.name,1,length(job.name)-1);
             chr(13): viewState:=4;
           end;
           glutPostRedisplay; //draw;
         end;
      6: begin
           case chr(key) of
             '0'..'9':job.xRes:=job.xRes+chr(key);
             chr(8) : job.xRes:=copy(job.xRes,1,length(job.xRes)-1);
             chr(13): begin
                        if (job.xRes='') or (strToInt(job.xRes)<10) then job.xRes:='10';
                        viewState:=4;
                      end;
           end;
           glutPostRedisplay; //draw;
         end;
      7: begin
           case chr(key) of
             '0'..'9':job.yRes:=job.yRes+chr(key);
             chr(8) : job.yRes:=copy(job.yRes,1,length(job.yRes)-1);
             chr(13): begin
                        if (job.yRes='') or (strToInt(job.yRes)<10) then job.yRes:='10';
                        viewState:=4;
                      end;
           end;
           glutPostRedisplay; //draw;
         end;
      8: begin
           case chr(key) of
             '0'..'9','.':job.antiAliasing:=job.antiAliasing+chr(key);
             chr(8) : job.antiAliasing:=copy(job.antiAliasing,1,length(job.antiAliasing)-1);
             chr(13): begin
                        if (job.antiAliasing='') or (strToFloatDef(job.antiAliasing,1)<0) then job.antiAliasing:='1';
                        viewState:=4;
                      end;
           end;
           glutPostRedisplay; //draw;
         end;
    end;
    if rerender then begin
      previewLevel:=4;
      renderImage.create(xRes shr previewLevel,yRes shr previewLevel);
      renderScaler:=viewScaler;
      renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
      startRendering;
    end;
    update;
  end;

FUNCTION jobbing:boolean;
  CONST cmdList:array [0..12] of T_commandAbstraction=(
    (isFile:true;  leadingSign:' '; cmdString:'';     paramCount: 0),  // 0 file (for restoreState)
    (isFile:false; leadingSign:'-'; cmdString:'';     paramCount: 2),  // 1 resolution
    (isFile:false; leadingSign:'-'; cmdString:'t';    paramCount: 1),  // 2 tolerance
    (isFile:false; leadingSign:'-'; cmdString:'x';    paramCount: 1),  // 3 screen center x
    (isFile:false; leadingSign:'-'; cmdString:'y';    paramCount: 1),  // 4 screen center y
    (isFile:false; leadingSign:'-'; cmdString:'z';    paramCount: 1),  // 5 zoom
    (isFile:false; leadingSign:'-'; cmdString:'j';    paramCount: 1),  // 6 override julianess
    (isFile:false; leadingSign:'-'; cmdString:'c';    paramCount: 1),  // 7 number of CPUs
    (isFile:false; leadingSign:'-'; cmdString:'a';    paramCount: 1),  // 8 animation steps
    (isFile:false; leadingSign:'-'; cmdString:'d';    paramCount: 0),  // 9 displayOnly
    (isFile:false; leadingSign:'-'; cmdString:'h';    paramCount: 0),  //10 help
    (isFile:false; leadingSign:'-'; cmdString:'f';    paramCount:-1),  //11 format for output
    (isFile:false; leadingSign:'-'; cmdString:'show'; paramCount: 0)); //12 show result


  PROCEDURE parseResolution(ps:string);
    begin
      ps:=copy(ps,2,length(ps)-1); //remove leading '-'
      xRes:=strToInt(copy(ps,1,pos('x',ps)-1));
      yRes:=strToInt(copy(ps,pos('x',ps)+1,length(ps)-1));
    end;

  FUNCTION nicenumber(x,xMax:longint):string;
    begin
      result:=intToStr(x);
      while length(result)<length(intToStr(xmax)) do result:='0'+result;
    end;

  VAR i,animateSteps:longint;
      jobname,destName:string;
      fmtExt :string;
      info:TSearchRec;
      displayOnly,beLazy:boolean;
    ov:array['x'..'z'] of record
      doOverride:boolean;
      value:double;
    end;
    ep:T_extendedParameter;
  begin
    displayOnly:=false;
    beLazy:=false;
    result:=false;
    jobname:='';
    fmtExt :='.jpg';
    animateSteps:=-1;
    ov['x'].doOverride:=false;
    ov['y'].doOverride:=false;
    ov['z'].doOverride:=false;
    for i:=1 to paramCount do begin
      ep:=extendedParam(i);
      case byte(matchingCmdIndex(ep,cmdList)) of
       0: jobname:=ep.cmdString; // file (for restoreState)
       1: begin xres:=ep.intParam[0]; yres:=ep.intParam[1]; end;
       2: renderTolerance:=ep.floatParam[0];
       3: with ov['x'] do begin doOverride:=true; value:=ep.floatParam[0]; end;
       4: with ov['y'] do begin doOverride:=true; value:=ep.floatParam[0]; end;
       5: with ov['z'] do begin doOverride:=true; value:=ep.floatParam[0]; end;
       6: with juliaOverride do begin doOverride:=true; value:=ep.floatParam[0]; end;
       7: numberOfCPUs:=ep.intParam[0];
       8: animateSteps:=ep.intParam[0];
       9: displayOnly:=true;
      10: begin
              writeln('List of command line parameters');
              writeln('  -h     :display help and quit');
              writeln('  -c<x>  :use x CPUs            (default: 2, minimum:1, maximum:16)');
              writeln('  -t<r>  :set tolerance to r    (default: 1,0)');
              writeln('  -<xres >x<yres> chooses resolution; default is screen resolution');
              writeln('  -f:<r> :set format to r       (for job calculation only; default: jpg)');
              writeln('  -d     :do not execute job but open and display job');
              writeln('  -a<n>  :compute animation of <n> frames');
              writeln('  -J<n>  :override julianess to value n');
              writeln('  -x<n>  :override screen center x/re');
              writeln('  -y<n>  :override screen center y/im');
              writeln('  -z<n>  :override zoom');
              writeln('  One file name given will be interpreted as a job!');
              writeln('  If several file names are given, only the last one will be employed');
              writeln('  If no file name is given, interactive mode is started.');
              result:=true;
            end;
      11: fmtExt:=ep.stringSuffix;
      12: showResults:=true;
      end;
    end;
    if pos('.',fmtExt)<1 then fmtExt:='.'+fmtExt;
    if jobname<>'' then begin
      result:=true;
      if displayonly then job.name:=jobname;
      if sysutils.findFirst(jobname,faAnyFile,info)=0 then repeat
        if (info.name<>'.') and (info.name<>'..') then begin
          destName:=ChangeFileExt(extractFilePath(jobname)+info.name,fmtExt);
          if not(fileExists(destName)) or displayOnly or (animateSteps>0) then begin
            if (extractFileExt(info.name)='.job') and restoreState(extractFilePath(jobname)+info.name) then begin
              with juliaOverride do if doOverride then juliaMode:=value;
              with ov['x'] do if not(doOverride) then value:=viewScaler.screenCenterX;
              with ov['y'] do if not(doOverride) then value:=viewScaler.screenCenterY;
              with ov['z'] do if not(doOverride) then value:=viewScaler.relativeZoom;
              viewScaler.recreate(xRes,yRes,ov['x'].value,ov['y'].value,ov['z'].value);

              job.name:=extractFilePath(jobname)+info.name;
              if not(displayOnly) then begin
                writeln('jobname: ',extractFilePath(jobname)+info.name);
                writeln('     to: ',extractFilePath(jobname)+destName,' @',xres,'x',yres);
                job.xRes:=intToStr(xres);
                job.yRes:=intToStr(yRes);
                job.name:=destName;
                job.antiAliasing:=floatToStr(renderTolerance);
                if animateSteps<=0 then doJob(beLazy,false) else if beLazy then begin
                  doJob(beLazy,true);
                  for i:=0 to animateSteps-1 do begin
                    lightNormal[0]:=system.sin(2*pi*i/animateSteps)*system.sqrt(1-system.sqr(lightNormal[2]));
                    lightNormal[1]:=system.cos(2*pi*i/animateSteps)*system.sqrt(1-system.sqr(lightNormal[2]));
                    writeln('Computing lighting for frame #',i);
                    copyAndTransform;
                    currImage.saveToFile(ChangeFileExt(extractFilePath(jobname)+info.name,nicenumber(i,animateSteps-1)+fmtExt));
                    if showResults then backgroundDisplay(ChangeFileExt(extractFilePath(jobname)+info.name,nicenumber(i,animateSteps-1)+fmtExt));
                  end;
                end else begin
                  for i:=0 to animateSteps-1 do begin
                    job.name:=ChangeFileExt(extractFilePath(jobname)+info.name,nicenumber(i,animateSteps-1)+fmtExt);
                    lightNormal[0]:=system.sin(2*pi*i/animateSteps)*system.sqrt(1-system.sqr(lightNormal[2]));
                    lightNormal[1]:=system.cos(2*pi*i/animateSteps)*system.sqrt(1-system.sqr(lightNormal[2]));
                    doJob(beLazy,false);
                  end;
                end;
              end;
            end else writeln('loading state from file "',extractFilePath(jobname)+info.name,'" failed');
          end else writeln('destination file "',destName,'" already exists');
        end;
      until sysutils.findNext(info)<>0;
      sysutils.findClose(info);
    end;
    if displayOnly then result:=false;
  end;

{$ifdef Windows}
VAR SystemInfo:SYSTEM_INFO;
{$endif}
begin
  lightNormal:=newVector(0,0,1);
  DecimalSeparator:='.';
  DefaultFormatSettings.DecimalSeparator:='.';
  {$ifdef Windows}
  getSystemInfo(SystemInfo);
  numberOfCPUs:=SystemInfo.dwNumberOfProcessors;
  {$endif}
  writeln('Open-GL fractals; by Martin Schlegel');
  writeln;
  writeln('compiled on: ',{$I %DATE%});
  writeln('         at: ',{$I %TIME%});
  writeln('FPC version: ',{$I %FPCVERSION%});
  writeln('Target CPU : ',{$I %FPCTARGET%},' (',numberOfCPUs,' threads)');
  {$ifdef Windows}
  xRes:=GetSystemMetrics(SM_CXSCREEN);
  yRes:=GetSystemMetrics(SM_CYSCREEN);
  {$else}
  xRes:=1024;
  yRes:=768;
  {$endif}
  randomize;
  viewScaler.create(xRes,yRes,0,0,1);
  job.xRes:=intToStr(xres);
  job.yRes:=intToStr(yRes);
  job.name:='';
  job.antiAliasing:='1';
  restoreState('relief.state');
  if not(jobbing) then begin



    glutInit(@argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGB);
    glutInitWindowSize(xRes shr 1,yRes shr 1);
    glutCreateWindow('Reliefs by M.S.');


    currScaler  :=viewScaler;
    renderScaler:=viewScaler;
    currImage  .create(1,1);
    renderScaler.rescale(xRes shr 5,yRes shr 5);
    renderImage .create(xRes shr 5,yRes shr 5);
    aaMask.create(xRes,yRes);
    previewLevel:=5;
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);


    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glClearColor(0.0, 0.0, 0.0, 0.0);
    glOrtho(0, 1, 0, 1, -10.0, 10.0);
    glMatrixMode(GL_MODELVIEW);
    glutDisplayFunc(@draw);
    glutIdleFunc(@update);
    glutReshapeFunc(@reshape);
    glutKeyboardFunc(@keyboard);
    glutPassiveMotionFunc(@mouseMovePassive);
    glutMotionFunc       (@mouseMoveActive );
    glutMouseFunc        (@mousePressFunc  );
    glPixelStorei(GL_UNPACK_ALIGNMENT,1);
    glPixelStorei(GL_UNPACK_LSB_FIRST ,GL_True);
    glutMainLoop();
  end;
end.
