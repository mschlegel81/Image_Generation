PROGRAM funcTrees;
{$fputype sse3}
USES {$ifdef UNIX}cmem,cthreads,{$endif}
     myFiles,mypics,gl,glext,glut,sysutils,dateutils,math,complex{$ifdef Windows},windows{$endif},darts,simplePicChunks;
CONST
  integ:array[-1..15] of longint=(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
VAR
  chunkToPrepare:array[-1..15] of longint;

VAR numberOfCPUs:longint=2;
    neededBoxWidth:longint=500;
    xRes,yRes,previewLevel{,it}:longint;


    viewScaler,currScaler,renderScaler:T_scaler;
               currImage ,renderImage :T_FloatMap;
    renderThreadID:array[0..15] of TThreadID;
    startOfCalculation:double;
    jobname:string;
    job:record
      name:string;
      xRes,yRes,antiAliasing:string;
    end;

TYPE T_parameterSet=record
                      operatorPos:array[0..3]      of T_Complex;
                      c          :array[0..4]      of T_floatColor;
                      node       :array[0..7,0..2] of T_Complex;
                    end;

VAR param:T_parameterSet;

CONST
    C_rotString:array[0..24] of string=(
      'none',              //0
      'rot2, origin, min', //1
      'rot2, center, min', //2
      'rot2, origin, avg', //3
      'rot2, center, avg', //4
      'rot2, origin, max', //5
      'rot2, center, max', //6
      'rot3, origin, min', //7
      'rot3, center, min', //8
      'rot3, origin, avg', //9
      'rot3, center, avg', //10
      'rot3, origin, max', //11
      'rot3, center, max', //12
      'rot4, origin, min', //13
      'rot4, center, min', //14
      'rot4, origin, avg', //15
      'rot4, center, avg', //16
      'rot4, origin, max', //17
      'rot4, center, max', //18
      'rot5, origin, min', //19
      'rot5, center, min', //20
      'rot5, origin, avg', //21
      'rot5, center, avg', //22
      'rot5, origin, max', //23
      'rot5, center, max');//24

VAR
    rotation:byte;
    sharpening:single=0;
    hueOffset :single=0;
    saturation:single=1;
    brightness:single=1;

    fullscreenmode:boolean=false;
    viewState:byte=3;
    threadsRunning:longint;
    mouseX,mouseY,mouseDownX,mouseDownY:longint;
    aaMask:T_ByteMap;
    movingByMouse:boolean;
    repaintPending:boolean=false;
    renderTolerance:single=1;
    latestUpdateRequest:double;

PROCEDURE storeState(fileName:string);
  VAR f:T_file;
  begin
    writeln('Storing state in ',fileName);
    f.createToWrite(fileName);
    f.writeSingle(viewScaler.screenCenterX);
    f.writeSingle(viewScaler.screenCenterY);
    f.writeSingle(viewScaler.relativeZoom);
    f.writeBuf(@param,sizeOf(param));
    f.writeSingle(hueOffset );
    f.writeSingle(rotation);
    f.writeSingle(saturation);
    f.writeSingle(brightness);
    f.writeSingle(sharpening);
    f.destroy;
  end;

PROCEDURE randomizeParams;
  CONST II:T_Complex=(re:0; im:1; valid:true);
  VAR i,j:longint;
  begin
    with param do begin
      for i:=0 to 3 do operatorPos[i]:=(-1+2*random)+II*(-1+2*random);
      for i:=0 to 4 do c[i]:=newVector(random,random,random);
      for i:=0 to 7 do for j:=0 to 2 do node[i,j]:=(-1+2*random)+II*(-1+2*random);
    end;
  end;

FUNCTION restoreState(fileName:string):boolean;
  VAR f:T_file;
      sx,sy,z:single;
      srot:single;
  begin
    if fileExists(fileName) then begin
      f.createToRead(fileName);
      sx              :=f.readSingle;
      sy              :=f.readSingle;
      z               :=f.readSingle;
      f.readBuf(@param,sizeOf(param));
      hueOffset :=f.readSingle;
      srot      :=f.readSingle;
      rotation:=round(srot) mod 25;
      saturation:=f.readSingle;
      brightness:=f.readSingle;
      result:=f.allOkay;
      sharpening:=f.readSingle;
      if result and not(f.allOkay) then sharpening:=0;
      f.destroy;
    end else result:=false;
    if not(result) then begin
      sx              :=0;
      sy              :=0;
      z               :=0.25;
      randomizeParams;
      hueOffset :=random;
    end;
    viewScaler.recreate(xRes,yRes,sx,sy,z);
  end;

PROCEDURE interpolateState(fileName:string; weight:single);
  VAR f:T_file;
  p2:T_parameterSet;
      h2,s2,b2,sh2:single;
      i,j:longint;
  begin
    if fileExists(fileName) then begin
      f.createToRead(fileName);
      f.readSingle;
      f.readSingle;
      f.readSingle;
      f.readBuf(@p2,sizeOf(p2));
      h2 :=f.readSingle;
      f.readSingle;
      s2 :=f.readSingle;
      b2 :=f.readSingle;
      sh2:=f.readSingle;
      if f.allOkay then begin
        hueOffset :=hueOffset *(1-weight)+h2 *weight;
        saturation:=saturation*(1-weight)+s2 *weight;
        brightness:=brightness*(1-weight)+b2 *weight;
        sharpening:=sharpening*(1-weight)+sh2*weight;
        for i:=0 to length(p2.operatorPos)-1 do
          param.operatorPos[i]:=param.operatorPos[i]*(1-weight)+p2.operatorPos[i]*weight;
        for i:=0 to length(p2.c)-1 do
          param.c[i]:=param.c[i]*(1-weight)+p2.c[i]*weight;
        for i:=0 to 7 do for j:=0 to 2 do
          param.node[i,j]:=param.node[i,j]*(1-weight)+p2.node[i,j]*weight;
      end;
      f.destroy;
    end;
  end;

FUNCTION mixedColorAt(x:T_Complex):T_floatColor;
  FUNCTION colorAt(x:T_Complex):T_floatColor;
    FUNCTION weightedOp(VAR x,y:T_Complex; w0,w1,w2,w3:single):T_Complex; inline;
      begin
        w3:=w3*1/(0.1+system.sqr(y.re)+system.sqr(y.im));
        result.re:=(x.re     +y.re     )*w0
                  +(x.re     -y.re     )*w1
                  +(x.re*y.re-x.im*y.im)*w2
                  +(x.im*y.re+x.re*y.im)*w3;
        result.im:=(x.im     +y.im     )*w0
                  +(x.im     -y.im     )*w1
                  +(x.re*y.im+x.re*y.im)*w2
                  +(x.im*y.re-x.re*y.im)*w3;
        result.valid:=true;
      end;

    VAR leaf      :array[0..7] of T_Complex;
        innerNode :array[0..6] of T_Complex;
        w         :array[0..6,0..3] of single;
        i,j       :byte;
        minDist   :single;
    begin
      with param do for i:=0 to 6 do begin
        innerNode[i]:=node[i,0]+
                      node[i,1]*x.re +
                      node[i,2]*x.im;
        minDist:=system.exp(-0.18393972058572116*(system.sqr(innerNode[i].re)+system.sqr(innerNode[i].im)));
        innerNode[i].re:=innerNode[i].re*minDist;
        innerNode[i].im:=innerNode[i].im*minDist;
        w[i,0]:=1-system.sqr(innerNode[i].re-operatorPos[0].re)+system.sqr(innerNode[i].im-operatorPos[0].im);
        w[i,1]:=1-system.sqr(innerNode[i].re-operatorPos[1].re)+system.sqr(innerNode[i].im-operatorPos[1].im);
        w[i,2]:=1-system.sqr(innerNode[i].re-operatorPos[2].re)+system.sqr(innerNode[i].im-operatorPos[2].im);
        w[i,3]:=1-system.sqr(innerNode[i].re-operatorPos[3].re)+system.sqr(innerNode[i].im-operatorPos[3].im);
        minDist:=1/(1E-6+w[i,0]+w[i,1]+w[i,2]+w[i,3]);
        w[i,0]:=w[i,0]*minDist;
        w[i,1]:=w[i,1]*minDist;
        w[i,2]:=w[i,2]*minDist;
        w[i,3]:=w[i,3]*minDist;
      end;

      with param do for j:=0 to 3 do begin
        for i:=0 to 7 do leaf[(i+j) and 7]:=node[i,0]+(node[i,1]*(x.re))+(node[i,2]*(x.im));
        innerNode[0]:=weightedOp(leaf     [0],leaf     [1],w[0,0],w[0,1],w[0,2],w[0,3]);
        innerNode[1]:=weightedOp(leaf     [2],leaf     [3],w[1,0],w[1,1],w[1,2],w[1,3]);
        innerNode[2]:=weightedOp(innerNode[0],innerNode[1],w[2,0],w[2,1],w[2,2],w[2,3]);
        innerNode[3]:=weightedOp(leaf     [4],leaf     [5],w[3,0],w[3,1],w[3,2],w[3,3]);
        innerNode[4]:=weightedOp(innerNode[2],innerNode[3],w[4,0],w[4,1],w[4,2],w[4,3]);
        innerNode[5]:=weightedOp(leaf     [6],leaf     [7],w[5,0],w[5,1],w[5,2],w[5,3]);
        innerNode[6]:=weightedOp(innerNode[4],innerNode[5],w[6,0],w[6,1],w[6,2],w[6,3]);
        minDist:=system.exp(-0.18393972058572116*(system.sqr(innerNode[6].re)+system.sqr(innerNode[6].im)));
        innerNode[6].re:=innerNode[6].re*minDist;
        innerNode[6].im:=innerNode[6].im*minDist;
        x:=innerNode[6];
      end;
      with param do begin


        result:=c[0]+(c[1]*(           innerNode[6].re ))+(
                      c[2]*(           innerNode[6].im ))+(
                      c[3]*(system.sqr(innerNode[6].re)))+(
                      c[4]*(system.sqr(innerNode[6].im)));
        result[0]:=result[0]+hueOffset;
        result[1]:=result[1]*saturation;
        result[2]:=result[2]*brightness;
        result:=fromHSV(result);
      end;
    end;

  FUNCTION colMin(c1,c2:T_floatColor):T_floatColor;
    begin
      if c1[0]<c2[0] then result[0]:=c1[0] else result[0]:=c2[0];
      if c1[1]<c2[1] then result[1]:=c1[1] else result[1]:=c2[1];
      if c1[2]<c2[2] then result[2]:=c1[2] else result[2]:=c2[2];
    end;

  FUNCTION colMax(c1,c2:T_floatColor):T_floatColor;
    begin
      if c1[0]>c2[0] then result[0]:=c1[0] else result[0]:=c2[0];
      if c1[1]>c2[1] then result[1]:=c1[1] else result[1]:=c2[1];
      if c1[2]>c2[2] then result[2]:=c1[2] else result[2]:=c2[2];
    end;

  CONST rot72 :T_Complex=(re:system.cos(2*pi/5); im:system.sin(2*pi/5); valid:true);
        rot90 :T_Complex=(re:0; im:1; valid:true);
        rot120:T_Complex=(re:system.cos(2*pi/3); im:system.sin(2*pi/3); valid:true);
        rot144:T_Complex=(re:system.cos(4*pi/5); im:system.sin(4*pi/5); valid:true);
        rot216:T_Complex=(re:system.cos(6*pi/5); im:system.sin(6*pi/5); valid:true);
        rot240:T_Complex=(re:system.cos(4*pi/3); im:system.sin(4*pi/3); valid:true);
        rot270:T_Complex=(re:0; im:-1; valid:true);
        rot288:T_Complex=(re:system.cos(8*pi/5); im:system.sin(8*pi/5); valid:true);


  VAR c:T_Complex;
  begin
    if rotation in [2,4,6,8,10,12,14,16,18,20,22,24] then begin
      c.re:=renderScaler.screenCenterX;
      c.im:=renderScaler.screenCenterY;
    end;


    case rotation of
       0: result:=colorAt(x);
       1: result:=colMin(colorAt(x),colorAt(-1*x   )); //'rot2, origin, min',
       2: result:=colMin(colorAt(x),colorAt(c-(x-c))); //'rot2, center, min',
       3: result:=0.5*  (colorAt(x)+colorAt(-1*x   ));//'rot2, origin, avg',
       4: result:=0.5*  (colorAt(x)+colorAt(c-(x-c)));//'rot2, center, avg',
       5: result:=colMax(colorAt(x),colorAt(-1*x   ));//'rot2, origin, max',
       6: result:=colMax(colorAt(x),colorAt(c-(x-c)));//'rot2, center, max',
       7: result:=colMin(colorAt(x),colMin(colorAt(   x   *rot120),colorAt(   x   *rot240))); //'rot3, origin, min',
       8: result:=colMin(colorAt(x),colMin(colorAt(c+(x-c)*rot120),colorAt(c+(x-c)*rot240)));//'rot3, center, min',
       9: result:=(1/3)*(colorAt(x)+       colorAt(   x   *rot120)+colorAt(   x   *rot240));
      10: result:=(1/3)*(colorAt(x)+       colorAt(c+(x-c)*rot120)+colorAt(c+(x-c)*rot240));
      11: result:=colMax(colorAt(x),colMax(colorAt(   x   *rot120),colorAt(   x   *rot240)));//'rot3, origin, max',
      12: result:=colMax(colorAt(x),colMax(colorAt(c+(x-c)*rot120),colorAt(c+(x-c)*rot240)));//'rot3, center, max',
      13: result:=colMin(colorAt(x),colMin(colorAt(   x   *rot90),colMin(colorAt(   x   *-1),colorAt(   x   *rot270))));
      14: result:=colMin(colorAt(x),colMin(colorAt(c+(x-c)*rot90),colMin(colorAt(c+(x-c)*-1),colorAt(c+(x-c)*rot270))));
      15: result:=0.25* (colorAt(x)       +colorAt(   x   *rot90)       +colorAt(   x   *-1)+colorAt(   x   *rot270));
      16: result:=0.25* (colorAt(x)       +colorAt(c+(x-c)*rot90)       +colorAt(c+(x-c)*-1)+colorAt(c+(x-c)*rot270));
      17: result:=colMax(colorAt(x),colMax(colorAt(   x   *rot90),colMax(colorAt(   x   *-1),colorAt(   x   *rot270))));
      18: result:=colMax(colorAt(x),colMax(colorAt(c+(x-c)*rot90),colMax(colorAt(c+(x-c)*-1),colorAt(c+(x-c)*rot270))));

      19: result:=colMin(colorAt(x),colMin(colorAt(   x   *rot72),colMin(colorAt(   x   *rot144),colMin(colorAt(   x   *rot216),colorAt(   x   *rot288)))));
      20: result:=colMin(colorAt(x),colMin(colorAt(c+(x-c)*rot72),colMin(colorAt(c+(x-c)*rot144),colMin(colorAt(c+(x-c)*rot216),colorAt(c+(x-c)*rot288)))));
      21: result:=0.2*  (colorAt(x)       +colorAt(   x   *rot72)       +colorAt(   x   *rot144)       +colorAt(   x   *rot216)+colorAt(   x   *rot288));
      22: result:=0.2*  (colorAt(x)       +colorAt(c+(x-c)*rot72)       +colorAt(c+(x-c)*rot144)       +colorAt(c+(x-c)*rot216)+colorAt(c+(x-c)*rot288));
      23: result:=colMax(colorAt(x),colMax(colorAt(   x   *rot72),colMax(colorAt(   x   *rot144),colMax(colorAt(   x   *rot216),colorAt(   x   *rot288)))));
      24: result:=colMax(colorAt(x),colMax(colorAt(c+(x-c)*rot72),colMax(colorAt(c+(x-c)*rot144),colMax(colorAt(c+(x-c)*rot216),colorAt(c+(x-c)*rot288)))));
    end;
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
FUNCTION max(x,y:longint):longint; begin if x>y then result:=x else result:=y; end;




PROCEDURE draw; cdecl;
  VAR ll,ur,mwc,mrc:T_Complex;
  FUNCTION editMarker(use:boolean):string;
    begin if use and odd(round(now*24*60*60*2)) then result:='_' else result:=''; end;

  begin
    //light setup

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
    glBegin(gl_quads);

      glTexCoord2f(0.0, 0.0); glnormal3f(0,0,1); glVertex2f(ll.re,ll.im);
      glTexCoord2f(1.0, 0.0); glnormal3f(0,0,1); glVertex2f(ur.re,ll.im);
      glTexCoord2f(1.0, 1.0); glnormal3f(0,0,1); glVertex2f(ur.re,ur.im);
      glTexCoord2f(0.0, 1.0); glnormal3f(0,0,1); glVertex2f(ll.re,ur.im);


      //glMultiTexCoord2fARB(GL_TEXTURE0_ARB, 0,0); glMultiTexCoord2fARB(GL_TEXTURE0_ARB, 0,0); glVertex2f(ll.re,ll.im);
      //glMultiTexCoord2fARB(GL_TEXTURE0_ARB, 1,0); glMultiTexCoord2fARB(GL_TEXTURE0_ARB, 1,0); glVertex2f(ur.re,ll.im);
      //glMultiTexCoord2fARB(GL_TEXTURE0_ARB, 1,1); glMultiTexCoord2fARB(GL_TEXTURE0_ARB, 1,1); glVertex2f(ur.re,ur.im);
      //glMultiTexCoord2fARB(GL_TEXTURE0_ARB, 0,1); glMultiTexCoord2fARB(GL_TEXTURE0_ARB, 0,1); glVertex2f(ll.re,ur.im);
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
      gWrite(0,1- 8*14/yRes,'[A] Brightness: '+formatFloat('0.##',brightness));
      gWrite(0,1- 9*14/yRes,'[S]aturation: '+formatFloat('0.##',saturation));
      gWrite(0,1-10*14/yRes,'[Y] rotation: '+C_rotString[rotation]);
      gWrite(0,1-11*14/yRes,'[X] Hue: '+formatFloat('0.##',hueOffset));
      gWrite(0,1-12*14/yRes,'Shar[P]ness: '+formatFloat('0.##',sharpening));

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
      gWrite(0,1-8*14/yRes,'Hint: Save as .ftj for later calculation.');  fetchRasterPos;
    end;
    //glFlush();
    glutSwapBuffers();
  end;

PROCEDURE mouseMovePassive(x,y:longint); cdecl;
  begin
    mouseX:=x;
    mouseY:=y;
    mouseDownX:=x;
    mouseDownY:=y;
    if viewState in [1,3] then glutPostRedisplay; //draw;
  end;

FUNCTION prepareImage(p:pointer):ptrint;
  VAR x,y:longint;
  begin
    for y:=0 to renderImage.height-1 do if (plongint(p)^<0) or (y mod numberOfCPUs=plongint(p)^) then begin
      for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=mixedColorAt(renderScaler.transform(x,y));
    end;
    interlockedDecrement(threadsRunning);
    result:=0;
  end;

VAR samplingStatistics,
    currentSamplingStatistics:T_samplingStatistics;

FUNCTION prepareChunk(p:pointer):ptrint;
  VAR chunk:T_colChunk;
      i,j,k,k0,k1:longint;
  begin
    chunk.create;
    chunk.initForChunk(renderImage.width,renderImage.height,plongint(p)^);
    for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do with chunk.col[i,j] do
      rest:=mixedColorAt(renderScaler.transform(chunk.getPicX(i),chunk.getPicY(j)));
    while (renderTolerance>1E-3) and chunk.markAlias(renderTolerance) do
      for i:=0 to chunk.width-1 do for j:=0 to chunk.height-1 do with chunk.col[i,j] do if odd(antialiasingMask) then begin
        if antialiasingMask=1 then begin
          k0:=1;
          k1:=2;
          antialiasingMask:=2;
          k1:=2*k1;
        end else begin
          k0:=antialiasingMask-1;
          k1:=k0+2;
          if k1>254 then k1:=254;
          antialiasingMask:=k1;
          k0:=2*k0;
          k1:=2*k1;
        end;
        for k:=k0 to k1-1 do rest:=rest+mixedColorAt(renderScaler.transform(
          chunk.getPicX(i)+darts_delta[k,0],
          chunk.getPicY(j)+darts_delta[k,1]));
      end;
    mergeSamplingStatistics(samplingStatistics       ,chunk.getSamplingStatistics);
    mergeSamplingStatistics(currentSamplingStatistics,chunk.getSamplingStatistics);
    chunk.copyTo(renderImage);
    chunk.destroy;
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

PROCEDURE update; cdecl;
  PROCEDURE copyAndTransform(postprocess:boolean);
    begin
      currImage.resizeDat(renderImage.width,renderImage.height);
      currImage.copyFrom(renderImage);
      if postprocess then begin
        if abs(sharpening)>1E-2 then sharpen(currImage,2E-3*currImage.diagonal,1+sharpening);
        shineImage(currImage);
        colorManipulate(fk_project,0,0,0,currImage);
      end;
      repaintPending:=false;
    end;

  begin
    if not(stillRendering) then begin
      if (now-latestUpdateRequest>1/(24*60*60)) or
         (renderScaler.relativeZoom<0.5*viewScaler.relativeZoom) or
         (renderScaler.relativeZoom>2  *viewScaler.relativeZoom)then begin
        latestUpdateRequest:=now+1;
        previewLevel:=4;
        killRendering;
        renderImage.create(xRes shr previewLevel,yRes shr previewLevel);
        renderScaler:=viewScaler;
        renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
        startRendering;
      end else if previewLevel>0 then begin
        copyAndTransform(false);
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
        copyAndTransform(true);
        currScaler:=renderScaler;
        glTexImage2D (GL_TEXTURE_2D,                     0,GL_RGB,currImage.width,currImage.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,currImage.rawData);
        glutPostRedisplay;
        dec(previewLevel);
        previewLevel:=-200;
      end else if previewLevel=-200 then begin
        if repaintPending then begin
          //copyAndTransform;
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
      xRes:=newXRes;
      yRes:=newYRes;
      glViewport(0, 0,xRes,yRes);
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

PROCEDURE doJob;
  VAR timeOfLastProgressOutput:double;
      lastProgressOutput:double;
      progTime:array[0..31] of record t,p:double; end;
      startOfCalculation:double;

  PROCEDURE initProgress(CONST initialProg:double);
    VAR i:longint;
    begin
      samplingStatistics:=zeroSamplingStatistics;
      currentSamplingStatistics:=zeroSamplingStatistics;
      if initialProg>=0.001 then writeln('@',initialProg*100:0:2,'%');
      startOfCalculation:=now;
      timeOfLastProgressOutput:=now;
      lastProgressOutput:=0;
      for i:=0 to 31 do begin progTime[i].p:=initialProg; progTime[i].t:=now; end;
    end;

  PROCEDURE stepProgress(prog:double);
    VAR i:longint;
        total:double;
        remaining:double;
    begin
      for i:=0 to 30 do progTime[i]:=progTime[i+1];
      with progTime[31] do begin
        t:=now;
        p:=prog;
      end;
      if ((now-timeOfLastProgressOutput)*24*60*60>5) or (prog>=lastProgressOutput+0.1) then begin
        timeOfLastProgressOutput:=now;
        lastProgressOutput:=prog;
        total:=(progTime[31].t-progTime[0].t)/(progTime[31].p-progTime[0].p);
        remaining:=(1-prog)*total;
        writeln(100*prog:5:2,'% total: ',myTimeToStr(now-startOfCalculation+remaining),
                                ' rem: ',myTimeToStr(remaining),
                            ' ready @: ',copy(timeToStr(now+remaining),1,5),' curr:',string(currentSamplingStatistics),'; avg:',string(samplingStatistics));
        currentSamplingStatistics:=zeroSamplingStatistics;
      end;
    end;

  VAR it:longint;
      pendingChunks:T_pendingList;
      chunkCount:longint;
      chunksDone:longint=0;
      anyStarted:boolean;
      sleepTime:longint=0;

  begin
    if extractFileExt(job.name)='.ftj' then begin storeState(job.name); exit; end;
    startOfCalculation:=now;

    renderScaler:=viewScaler;
    killRendering;
    renderImage.resizeDat(strToInt(job.xRes),strToInt(job.yRes));
    renderScaler.rescale (strToInt(job.xRes),strToInt(job.yRes));

    markChunksAsPending(renderImage);
    chunkCount:=chunksInMap(strToInt(job.xRes),strToInt(job.yRes));
    pendingChunks:=getPendingList(renderImage);
    chunksDone:=chunkCount-length(pendingChunks);
    initProgress(chunksDone/chunkCount);
    for it:=0 to numberOfCPUs-1 do if length(pendingChunks)>0 then begin
      chunkToPrepare[it]:=pendingChunks[length(pendingChunks)-1];
      setLength(pendingChunks,length(pendingChunks)-1);
      {$ifdef UNIX} beginThread(@prepareChunk,@chunkToPrepare[it],renderThreadID[it]);
      {$else}       renderThreadID[it]:=beginThread(@prepareChunk,@chunkToPrepare[it]);
      {$endif}
    end else chunkToPrepare[it]:=-1;
    while length(pendingChunks)>0 do begin
      anyStarted:=false;
      for it:=0 to numberOfCPUs-1 do if (length(pendingChunks)>0) and (waitForThreadTerminate(renderThreadID[it],1)=0) then begin
        inc(chunksDone);
        chunkToPrepare[it]:=pendingChunks[length(pendingChunks)-1];
        setLength(pendingChunks,length(pendingChunks)-1);
        {$ifdef UNIX} beginThread(@prepareChunk,@chunkToPrepare[it],renderThreadID[it]);
        {$else}       renderThreadID[it]:=beginThread(@prepareChunk,@chunkToPrepare[it]);
        {$endif}
        anyStarted:=true;
        sleepTime:=1;
      end;
      if anyStarted then stepProgress(chunksDone/chunkCount)
                    else inc(sleepTime,1);
      sleep(sleepTime);
    end;
    writeln('waiting for the rest...');
    for it:=0 to numberOfCPUs-1 do if (chunkToPrepare[it]>=0) then begin
      repeat sleep(100) until (waitForThreadTerminate(renderThreadID[it],1)=0);
      inc(chunksDone);
      if chunksDone<chunkCount then stepProgress(chunksDone/chunkCount);
    end;

    if uppercase(extractFileExt(job.name))<>'.VRAW' then begin
      shineImage(renderImage);
      colorManipulate(fk_project,0,0,0,renderImage);
    end;
    renderImage.saveToFile(job.name);

    writeln;
    writeln(job.name,' created in ',myTimeToStr(now-startOfCalculation));
    currImage.destroy;
    currImage.createCopy(renderImage);
    currScaler:=         renderScaler;
    previewLevel:=-2;
  end;

PROCEDURE keyboard(key:byte; x,y:longint); cdecl;
  begin
    if viewState<4 then case key of
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
      ord('L'),ord('l'):
          begin
            killRendering;
            randomizeParams;
            brightness:=random;
            saturation:=random;
            hueOffset :=random;
            rotation:=0;
            previewLevel:=4;
            renderImage.create(xRes shr previewLevel,yRes shr previewLevel);
            renderScaler:=viewScaler;
            renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
            startRendering;
          end;
      ord('a'): begin brightness:=brightness+0.05; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('A'): begin brightness:=brightness-0.05; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('s'): begin saturation:=saturation+0.1; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('S'): begin saturation:=saturation-0.1; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('x'): begin hueOffset :=hueOffset+0.01; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('X'): begin hueOffset :=hueOffset-0.01; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('y'): begin rotation:=(rotation+ 1) mod 25; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('Y'): begin rotation:=(rotation+24) mod 25; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('p'): begin sharpening:=sharpening+0.1; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('P'): begin sharpening:=sharpening-0.1; latestUpdateRequest:=now; glutPostRedisplay; end;

      23: //Strg+W
          begin killRendering; storeState('funcTrees.state'); currImage.destroy; renderImage.destroy;  halt; end;
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
                        doJob;
                        glutPostRedisplay;
                        previewLevel:=4;
                        renderImage.create(xRes shr previewLevel,yRes shr previewLevel);
                        renderScaler:=viewScaler;
                        renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
                        startRendering;
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
    //update;
  end;

FUNCTION jobbing:boolean;
  PROCEDURE parseResolution(ps:string);
    begin
      ps:=copy(ps,2,length(ps)-1); //remove leading '-'
      xRes:=strToInt(copy(ps,1,pos('x',ps)-1));
      yRes:=strToInt(copy(ps,pos('x',ps)+1,length(ps)-1));
    end;

  FUNCTION nicenumber(x,xMax:longint):string;
    begin
      result:=intToStr(x);
      while length(result)<length(intToStr(xMax)) do result:='0'+result;
    end;

  VAR i:longint;
      secondaryJob,destName:string;
      fmtExt :string;
      displayOnly:boolean;
      interpolationWeight:single=1;
  begin
    displayOnly:=false;
    result:=false;
    jobname:='';
    fmtExt :='.jpg';
    for i:=1 to paramCount do if paramStr(i)[1]='-' then begin
      if      paramStr(i)[2] in ['1'..'9'] then parseResolution(paramStr(i))
      else if paramStr(i)[2]='t' then renderTolerance    :=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2))
      else if paramStr(i)[2]='c' then numberOfCPUs       :=strToInt  (copy(paramStr(i),3,length(paramStr(i))-2))
      else if paramStr(i)[2]='f' then fmtExt             :=           copy(paramStr(i),3,length(paramStr(i))-2)
      else if paramStr(i)[2]='i' then interpolationWeight:=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2))
      else if paramStr(i)[2]='d' then displayOnly:=true
      else if paramStr(i)[2]='h' then begin
                                        writeln('List of command line parameters');
                                        writeln('  -h    :display help and quit');
                                        writeln('  -c<x> :use x CPUs            (default: 2, minimum:1, maximum:32)');
                                        writeln('  -t<r> :set tolerance to r    (default: 1,0)');
                                        writeln('  -<xres>x<yres> chooses resolution; default is screen resolution');
                                        writeln('  -f<r> :set format to r       (for job calculation only; default: jpg)');
                                        writeln('  -i<w> :interpolate with weight w to next jobname');
                                        writeln('  -d    :do not execute job but open and display job');
                                        writeln('  One file name given will be interpreted as a job!');
                                        writeln('  If several file names are given, only the last one will be employed');
                                        writeln('  If no file name is given, interactive mode is started.');
                                        result:=true;
                                      end;
    end else begin
      if jobname='' then jobname:=paramStr(i)
                    else secondaryJob:=paramStr(i);
    end;
    if pos('.',fmtExt)<1 then fmtExt:='.'+fmtExt;
    if jobname<>'' then begin
      result:=true;
      destName:=ChangeFileExt(jobname,fmtExt);
      if not(fileExists(destName)) or displayOnly then begin
        if restoreState(jobname) then begin
          if secondaryJob<>'' then interpolateState(secondaryJob,interpolationWeight);
          job.name:=extractFilePath(jobname);
          if not(displayOnly) then begin
            begin
              writeln('jobname: ',extractFilePath(jobname));
              writeln('     to: ',destName,' @',xRes,'x',yRes);
              job.xRes:=intToStr(xRes);
              job.yRes:=intToStr(yRes);
              job.name:=destName;
              job.antiAliasing:=floatToStr(renderTolerance);
              doJob;
            end;
          end;
        end else writeln('loading state from file "',extractFilePath(jobname),'" failed');
      end else writeln('destination file "',destName,'" already exists');
    end;
    if displayOnly then result:=false
    else if jobname='' then restoreState('funcTrees.state');
  end;

{$ifdef Windows}
VAR SystemInfo:SYSTEM_INFO;
{$endif}
begin
  DefaultFormatSettings.DecimalSeparator:='.';
  {$ifdef Windows}
  getSystemInfo(SystemInfo);
  numberOfCPUs:=SystemInfo.dwNumberOfProcessors;
  {$endif}
  randomize;
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
  viewScaler.create(xRes,yRes,0,0,1);
  job.xRes:=intToStr(xRes);
  job.yRes:=intToStr(yRes);
  job.name:='';
  job.antiAliasing:='1';
  if not(jobbing) then begin
    glutInit(@argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGB);
    glutInitWindowSize(xRes shr 1,yRes shr 1);
    glutCreateWindow('funcTrees by M.S.');


    currScaler  :=viewScaler;
    renderScaler:=viewScaler;
    currImage  .create(1,1);
    renderScaler.rescale(xRes shr 5,yRes shr 5);
    renderImage .create(xRes shr 5,yRes shr 5);
    aaMask.create(xRes,yRes);
    previewLevel:=5;
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    //startRendering;

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
    latestUpdateRequest:=now;
    glutMainLoop();
  end;
end.
