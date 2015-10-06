PROGRAM bifurcation;{$MACRO ON}
{$fputype sse3}

USES {$ifdef UNIX}cmem,cthreads,{$endif}
     myPics,gl,glext,glut,sysutils,dateutils,math,complex{$ifdef Windows},windows{$endif},Process,cmdLineParseUtil,darts;
CONST
  integ:array[-1..15] of longint=(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);

VAR numberOfCPUs:longint=2;                   //number of CPUs used
    renderThreadID:array[0..15] of TThreadID; //IDs of render threads
    threadsRunning:longint=0;                 //number of render threads running

    neededBoxWidth:longint=500;               //menu width
    fullscreenmode:boolean=false;
    viewState:byte=3;                         //view state - determines what is drawn apart from the resulting image
    xRes,yRes:longint;                        //current window size

    viewScaler,currScaler,renderScaler:T_Scaler;   //scalers for viewing, current result and current rendering
               currImage ,renderImage ,backgroundImage:T_floatMap; //images from current result and current rendering
    useBackground:boolean=false;


    startOfCalculation:double;
    previewLevel:longint=0;

    job:record
      name,xRes,yRes,quality:string;
    end;

    maxDepth    :longint=1;
    colorStyle  :byte=0;
    alpha       :single=0.1;
    quality     :single=1;

    mouseX,mouseY,mouseDownX,mouseDownY:longint;
    movingByMouse:boolean;
    repaintPending:boolean=false;
    latestUpdateRequest:double;
    showComputedImage:boolean=false;

{$ifdef typ0}
{$define windowTitle:='Feigenbaum fractal'}
{$define iterationInitialization:=begin end}
{$define iterationStep:=x:=a*x*(1-x)}
{$endif}

{$ifdef typ1}
{$define windowTitle:='Feigenbaum derivate'}
{$define iterationInitialization:=begin a:=4+(1/a) end}
{$define iterationStep:=x:=a*x*(1-x)}
{$endif}

{$ifdef typ2}
{$define windowTitle:='Cosine bifurcation'}
{$define iterationInitialization:=begin end}
{$define iterationStep:=x:=system.cos(a*x)}
{$endif}
PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif} 'display '+ps;
    tempProcess.execute;
    tempProcess.free;
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
        glVertex2f(0                       ,1);
        glVertex2f(0                       ,1-17*14/yRes);
        glVertex2f((neededBoxWidth+10)/xRes,1-17*14/yRes);
        glVertex2f((neededBoxWidth+10)/xRes,1);
      glEnd;
      neededBoxWidth:=0;
      glColor3f(1,1,1);
      gWrite(0,1-     14/yRes,'[H]elp on/off');       fetchRasterPos;
      gWrite(0,1- 2.5*14/yRes,'[+/-] zoom in/out');   fetchRasterPos;
      gWrite(0,1- 3.5*14/yRes,'[R]ecenter on mouse'); fetchRasterPos;
      gWrite(0,1- 4.5*14/yRes,'[Left mouse drag] zoom in on region'); fetchRasterPos;
      gWrite(0,1- 5.5*14/yRes,'[Right mouse drag] translate image'); fetchRasterPos;
      gWrite(0,1- 7  *14/yRes,'[D]epth '+intToStr(maxDepth));
      gWrite(0,1- 8  *14/yRes,'[A]lpha '+formatFloat('0.000',alpha));
      gWrite(0,1- 9  *14/yRes,'[C]oloring '+intToStr(colorStyle));
      gWrite(0,1-10  *14/yRes,'[Q]uality '+formatFloat('0.000',quality));
      //gWrite(0,1-11  *14/yRes,'[G]amma '+formatFloat('0.000',pseudoGamma));
      gWrite(0,1-12.5*14/yRes,'[F]ullscreen');  fetchRasterPos;
      gWrite(0,1-13.5*14/yRes,'[B]itmap generation');  fetchRasterPos;
      gWrite(0,1-14.5*14/yRes,'[I]nfo (see console)');  fetchRasterPos;
      gWrite(0,1-16  *14/yRes,'[Strg+W] Quit');  fetchRasterPos;
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
//      gWrite(0,1-4*14/yRes,'[A]A-Tol: '+job.antiAliasing+editMarker(viewState=8)); fetchRasterPos;
      if job.name<>'' then gWrite(0,1-5.5*14/yRes,'[S]tart calculation'); fetchRasterPos;
      gWrite(0,1-6.5*14/yRes,'[C]ancel');  fetchRasterPos;
      gWrite(0,1-8*14/yRes,'Hint: Save as .job for later calculation.');  fetchRasterPos;
    end;
    //glFlush();
    glutSwapBuffers();
  end;

FUNCTION max(x0,x1,x2:single):single; inline;
  begin
    if x0>x1     then result:=x0
                 else result:=x1;
    if x2>result then result:=x2;
  end;


PROCEDURE mouseMovePassive(x,y:longint); cdecl;
  begin
    mouseX:=x;
    mouseY:=y;
    mouseDownX:=x;
    mouseDownY:=y;
    if viewState in [1,3] then glutPostRedisplay;
  end;


FUNCTION prepareImage(p:pointer):ptrint;
  VAR iy,k,kx,samples:longint;
      x,a:single;
      pAlpha:single;
  begin
    samples:=round(quality*2/maxdepth*xres);
    if samples<1 then samples:=1;
//    palpha:=alpha/quality;
    palpha:=alpha*xres/(samples*maxdepth*0.5);
    if palpha>1 then pAlpha:=1;



    for iy:=0 to renderImage.height-1 do if (iy mod numberOfCPUs=plongint(p)^) then begin
      a:=renderScaler.transform(iy+darts_delta[previewLevel,0],0).re;
      iterationInitialization;
      for kx:=1 to samples do begin
        x:=random;
        for k:=0 to maxDepth shr 1 do begin
          iterationStep;
        end;
        for k:=(maxDepth shr 1)+1 to maxDepth do begin
          iterationStep;
          renderImage.incPixel(round(renderScaler.mrofsnart(0,x).im+darts_delta[previewLevel,1]),iy,white*pAlpha);
        end;
      end;
    end;
    interlockedDecrement(threadsRunning);
    result:=0;
  end;

PROCEDURE copyAndTransform;
  FUNCTION limit(x:T_floatColor;ix,iy:longint):T_floatColor; inline;
    begin
      if useBackground then begin
        if x[0]<0 then x:=backgroundImage[ix,iy]
                  else if x[0]>1 then x:=white
                  else x:=(x[0]*white)+(1-x[0])*backgroundImage[ix,iy];
      end else begin
        if x[0]<0 then x[0]:=0 else if x[0]>1 then x[0]:=1;
        x[1]:=x[0];
        x[2]:=x[0];
      end;
      result:=x;
    end;

  FUNCTION fire(x:T_floatColor;ix,iy:longint):T_floatColor; inline;
    begin
      if x[0]<0 then result:=black
      else if x[0]<1/3 then result:=newVector(3*x[0],0,0)
      else if x[0]<2/3 then result:=newVector(1,3*x[0]-1,0)
      else if x[0]<1   then result:=newVector(1,1,3*x[0]-2)
      else result:=white;
    end;

  FUNCTION threshold(x:T_floatColor;ix,iy:longint):T_floatColor; inline;
    begin
      if useBackground then begin
        if x[0]<0.5 then result:=backgroundImage[ix,iy]
                    else result:=white;
      end else begin
        if x[0]<0.5 then result:=black
                    else result:=white;
      end;
    end;



  VAR x,y:longint;
      wr,wc:single;
  begin
    if previewLevel=0 then begin
      currImage.resizeDat(renderImage.height,renderImage.width);
      case colorStyle of
        0: for y:=0 to currImage.height-1 do for x:=0 to currImage.width -1 do currImage[x,y]:=limit    (renderImage[y,x],x,y);
        1: for y:=0 to currImage.height-1 do for x:=0 to currImage.width -1 do currImage[x,y]:=fire     (renderImage[y,x],x,y);
        2: for y:=0 to currImage.height-1 do for x:=0 to currImage.width -1 do currImage[x,y]:=threshold(renderImage[y,x],x,y);
      end;
    end else begin
      wr:=           1/(previewLevel+1);
      wc:=previewLevel/(previewLevel+1);
      case colorStyle of
        0: for y:=0 to currImage.height-1 do for x:=0 to currImage.width -1 do currImage[x,y]:=currImage[x,y]*wc+limit    (renderImage[y,x],x,y) *wr;
        1: for y:=0 to currImage.height-1 do for x:=0 to currImage.width -1 do currImage[x,y]:=currImage[x,y]*wc+fire     (renderImage[y,x],x,y)*wr;
        2: for y:=0 to currImage.height-1 do for x:=0 to currImage.width -1 do currImage[x,y]:=currImage[x,y]*wc+threshold(renderImage[y,x],x,y)*wr;
      end;
    end;
    //writeln('level ',previewLevel);
    inc(previewLevel);
  end;

PROCEDURE startRendering;
  VAR it:longint;
      pt:P_floatColor;
  begin
    if previewLevel=0 then startOfCalculation:=now;
    pt:=renderImage.rawData;
    for it:=0 to renderImage.size-1 do pt[it]:=black;
    threadsRunning:=numberOfCPUs;
    for it:=0 to numberOfCPUs-1 do
    {$ifdef UNIX}
      beginThread(@prepareImage,@integ[it],renderThreadID[it]);
    {$else}
      renderThreadID[it]:=beginThread(@prepareImage,@integ[it]);
    {$endif}
  end;

FUNCTION stillRendering:boolean;
  begin
    //result:=false;
    //for i:=0 to numberOfCPUs-1 do result:=result or (waitForThreadTerminate(renderThreadID[i],1)<>0);
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
  begin
    if not(stillRendering) then begin
      if (now-latestUpdateRequest>1/(24*60*60)) or
         (renderScaler.relativeZoom<0.5*viewScaler.relativeZoom) or
         (renderScaler.relativeZoom>2  *viewScaler.relativeZoom)then begin
        latestUpdateRequest:=now+1;
        previewLevel:=0;
        killRendering;
        renderImage.create(yRes,xres);
        renderScaler:=viewScaler;
        renderScaler.rescale(xres,yres);
        startRendering;
        repaintPending:=true;
      end else if repaintPending then begin
        copyAndTransform;
        currScaler:=renderScaler;
        glTexImage2D (GL_TEXTURE_2D,0,GL_RGB,currImage.width,currImage.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,currImage.rawData);
        glutPostRedisplay;
        repaintPending:=false;
      end else
      if (previewLevel<32) and (now-latestUpdateRequest<0) then begin
        startRendering;
        repaintPending:=true;
      end else if previewLevel=32 then begin
        writeln((now-startOfCalculation)*24*60*60:0:2,'sec');
        inc(previewLevel);
      end;
    end;
    if viewState in [5..8]
      then glutPostRedisplay; //to make the caret blink...
    sleep(10);
  end;


PROCEDURE reshape(newXRes,newYRes:longint); cdecl;
  begin
    if (newXRes>0) and (newYRes>0) and ((newXRes<>xRes) or (newYRes<>yRes)) then begin
      killRendering;
      mouseDownX:=0; mouseX:=0;
      mouseDownY:=0; mouseY:=0;
      viewScaler.rescale(newXRes,newYRes);
      xRes:=newxRes;
      yRes:=newyRes;
      glViewport(0, 0,xres,yres);
      glLoadIdentity;
      glOrtho(0, 1, 0, 1, -10.0, 10.0);
      glMatrixMode(GL_MODELVIEW);
      latestUpdateRequest:=now;
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
      latestUpdateRequest:=now;
    end else if (state=glut_up) and (abs(mouseX-mouseDownX)>20) and (abs(mouseY-mouseDownY)>20) then begin
      viewScaler.recenter(viewScaler.transform((mouseX+mouseDownX)*0.5,(mouseY+mouseDownY)*0.5).re,
                          viewScaler.transform((mouseX+mouseDownX)*0.5,(mouseY+mouseDownY)*0.5).im);
      viewScaler.rezoom(viewScaler.relativeZoom*(sqrt((xRes*xRes+yRes*yRes)/
        (system.sqr(mouseX-mouseDownX)+system.sqr(mouseX-mouseDownX)))));
      latestUpdateRequest:=now;
    end;
  end;

PROCEDURE doJob;
  VAR it:longint;
      pt:P_floatColor;
  begin
    renderScaler:=viewScaler;
    writeln('Rendering to file ',job.name,'...');
    startOfCalculation:=now;
    killRendering;
    renderImage.resizeDat(strToInt(job.yRes),strToInt(job.xRes));
    renderScaler.rescale (strToInt(job.xRes),strToInt(job.yRes));
    previewLevel:=0;
    while previewLevel<32 do begin
      pt:=renderImage.rawData;
      for it:=0 to renderImage.size-1 do pt[it]:=black;
      for it:=1 to numberOfCPUs-1 do
        {$ifdef UNIX} beginThread(@prepareImage,@integ[it],renderThreadID[it]);
        {$else}       renderThreadID[it]:=beginThread(@prepareImage,@integ[it]); {$endif}
      prepareImage(@integ[0]);
      for it:=1 to numberOfCPUs-1 do repeat sleep(1) until waitForThreadTerminate(renderThreadID[it],1)=0;
      copyAndTransform;
    end;

    currImage.saveToFile(job.name);
    if showComputedImage then backgroundDisplay(job.name);
    writeln(' done in ',(now-startOfCalculation)*24*60*60:0:3,'sec');
    currScaler:=         renderScaler;
    //previewLevel:=-2;
    //glutpostredisplay;
  end;

PROCEDURE keyboard(key:byte; x,y:longint); cdecl;
  VAR rerender:boolean;
  begin
    rerender:=false;
    if viewState<4 then case key of
      //ord('m'): begin materialType:=(materialType+1) mod 10; repaintPending:=true; end;
      //ord('M'): begin materialType:=(materialType+9) mod 10; repaintPending:=true; end;
      ord('b'),ord('B'):
        begin
          job.xRes:=intToStr(xRes);
          job.yRes:=intToStr(yRes);
          viewState:=4; glutPostRedisplay;
        end;
      ord('r'),ord('R'):
          begin
            viewScaler.recenter(viewScaler.transform(x,y).re,viewScaler.transform(x,y).im);
            //previewLevel:=4;
            glutPostRedisplay; //draw;
          end;
      ord('q'): begin quality:=quality*sqrt(2);   rerender:=true; end;
      ord('Q'): begin quality:=quality*sqrt(0.5); rerender:=true; end;
      ord('a'): begin alpha:=alpha*1.1; rerender:=true; end;
      ord('A'): begin alpha:=alpha/1.1; rerender:=true; end;
      ord('c'): begin colorStyle:=(colorStyle+1) mod 3; rerender:=true; end;
      ord('C'): begin colorStyle:=(colorStyle+2) mod 3; rerender:=true; end;
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
      ord('h'),ord('H'):
          begin
            viewState:=viewState xor 2;
            glutPostRedisplay;
          end;
      ord('i'),ord('I'):
          begin
            writeln(paramStr(0),' -C',colorStyle,' -d',maxDepth,' -x',floatToStr(viewScaler.screenCenterX),' -y',floatToStr(viewScaler.screenCenterY),' -z',floatToStr(viewScaler.relativeZoom),' -',xres,'x',yres,' -q',quality);
          end;

      23: //Strg+W
          begin killRendering; renderImage.destroy; currImage.destroy; halt; end;
      43: //+
          begin
            viewScaler.chooseScreenRef(x,y);
            viewScaler.rezoom(viewScaler.relativeZoom*1.1);
            rerender:=true;
            latestUpdateRequest:=now;
            //glutpostredisplay; //draw;
          end;
      45: //-
          begin
            viewScaler.chooseScreenRef(x,y);
            viewScaler.rezoom(viewScaler.relativeZoom/1.1);
            rerender:=true;
            latestUpdateRequest:=now;
            //glutpostredisplay; //draw;
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
             '0'..'9','.':job.quality:=job.quality+chr(key);
             chr(8) : job.quality:=copy(job.quality,1,length(job.quality)-1);
             chr(13): begin
                        if (job.quality='') or (strToFloatDef(job.quality,1)<0) then job.quality:='1';
                        viewState:=4;
                      end;
           end;
           glutPostRedisplay; //draw;
         end;
    end;
    if rerender then begin
      latestUpdateRequest:=now;
    end;
    glutPostRedisplay;
    //update;
  end;

FUNCTION jobbing:boolean;
  CONST cmdList:array [0..14] of T_commandAbstraction=(
    (isFile:true;  leadingSign:' '; cmdString:'';     paramCount: 0),  //0 file (for output)
    (isFile:false; leadingSign:'-'; cmdString:'';     paramCount: 2),  //1 resolution
    (isFile:false; leadingSign:'-'; cmdString:'t';    paramCount:12),  //2 tolerance
    (isFile:false; leadingSign:'-'; cmdString:'x';    paramCount: 1),  //3 screen center x
    (isFile:false; leadingSign:'-'; cmdString:'y';    paramCount: 1),  //4 screen center y
    (isFile:false; leadingSign:'-'; cmdString:'z';    paramCount: 1),  //5 zoom
    (isFile:false; leadingSign:'-'; cmdString:'a';    paramCount: 1),  //6 alpha
    (isFile:false; leadingSign:'-'; cmdString:'c';    paramCount: 1),  //7 number of CPUs
    (isFile:false; leadingSign:'-'; cmdString:'C';    paramCount: 1),  //8 coloring
    (isFile:false; leadingSign:'-'; cmdString:'d';    paramCount: 1),  //9  iteration depth
    (isFile:false; leadingSign:'-'; cmdString:'f';    paramCount: 0),  //10 force rendering
    (isFile:false; leadingSign:'-'; cmdString:'i';    paramCount: 0),  //11 force interactive mode
    (isFile:false; leadingSign:'-'; cmdString:'show'; paramCount: 0),  //12 show result
    (isFile:false; leadingSign:'-'; cmdString:'q';    paramCount: 1),  //13 quality
    (isFile:false; leadingSign:'-'; cmdString:'b';    paramCount:-1)); //14 background image

  VAR i:longint;
      destName     :string='';
      screenCenterX:double=0;
      screenCenterY:double=0;
      zoom         :double=0.1;
      forceRendering:boolean=false;
      goInteractive :boolean=false;
      ep:T_extendedParameter;
  begin
    result:=false;
    for i:=1 to paramCount do begin
      ep:=extendedParam(i);
      case byte(matchingCmdIndex(ep,cmdList)) of
        0: destName:=ep.cmdString;
        1: begin xres:=ep.intParam[0]; yres:=ep.intParam[1]; end;
        3: screenCenterX:=ep.floatParam[0];
        4: screenCenterY:=ep.floatParam[0];
        5: zoom         :=ep.floatParam[0];
        6: alpha        :=ep.floatParam[0];
        7: numberOfCPUs :=ep.intParam[0];
        8: colorStyle   :=ep.intParam[0];
        9: maxDepth     :=ep.intParam[0];
       10: forceRendering:=true;
       11: goInteractive:=true;
       12: showComputedImage:=true;
       13: quality:=ep.floatParam[0];
       14: begin
             if fileExists(ep.stringSuffix) then begin
               backgroundImage.create(ep.stringSuffix);
               useBackground:=true;
               xres:=backgroundImage.width;
               yres:=backgroundImage.height;
             end;
           end;
      else begin
             writeln('List of command line parameters');
             writeln('  -h    :display help and quit');
             writeln('  -c<x> :use x CPUs            (minimum:1, maximum:16)');
             writeln('  -t<r> :set tolerance to r    (default: 1.0)');
             writeln('  -<xres>x<yres> chooses resolution; default is screen resolution');
             writeln('  -f<r> :set format to r       (for job calculation only; default: jpg)');
             writeln('  -x<#> :x position of view');
             writeln('  -y<#> :y position of view');
             writeln('  -z<#> :zoom view');
             writeln('  -g<#> :gamma value before coloring');
             writeln('  -C<s>,<c>,<v> :Coloring <s>tyle, <c>oloring and <v>ariant');
             writeln('  -d<#> :iteration depth');
             writeln('  -f    :force overwriting of existing files');
             writeln('  -i    :enter interactive mode after rendering');
             writeln('  -show :show computed result via display');
             writeln('  One file name given will be interpreted as a output file!');
             writeln('  If several file names are given, only the last one will be employed');
             writeln('  If no file name is given, interactive mode is started.');
             result:=true;
           end;
      end;
    end;

    renderScaler.recreate(xres,yres,screenCenterX,screenCenterY,zoom);
    viewScaler  .recreate(xres,yres,screenCenterX,screenCenterY,zoom);
    if destName<>'' then begin
      result:=not(goInteractive);
      if not(fileExists(destName)) or forceRendering then begin
        writeln('rendering to: ',destName,' @',xres,'x',yres);
        job.xRes:=intToStr(xres);
        job.yRes:=intToStr(yRes);
        job.name:=destName;
        doJob;
      end else writeln('destination file "',destName,'" already exists');
    end;
  end;

{$ifdef Windows}
VAR SystemInfo:SYSTEM_INFO;
{$endif}
begin
  DecimalSeparator:='.';
  DefaultFormatSettings.DecimalSeparator:='.';
  {$ifdef Windows}
  getSystemInfo(SystemInfo);
  numberOfCPUs:=SystemInfo.dwNumberOfProcessors;
  {$endif}
  randomize;

  {$ifdef Windows}
  xRes:=GetSystemMetrics(SM_CXSCREEN)-200;
  yRes:=GetSystemMetrics(SM_CYSCREEN)-200;
  {$else}
  xRes:=1024;
  yRes:=768;
  {$endif}
  viewScaler.create(xRes,yRes,0,0,1);
  job.xRes:=intToStr(xres);
  job.yRes:=intToStr(yRes);
  job.name:='';
  job.quality:='1';
  currScaler  :=viewScaler;
  renderScaler:=viewScaler;
  renderScaler.rescale(xRes,yRes);
  renderImage .create(yRes,xRes);
  currImage   .create(0,0);


  if not(jobbing) then begin
    writeln('Bifurcation plots; by Martin Schlegel');
    writeln;
    writeln('compiled on: ',{$I %DATE%});
    writeln('         at: ',{$I %TIME%});
    writeln('FPC version: ',{$I %FPCVERSION%});
    writeln('Target CPU : ',{$I %FPCTARGET%},' (',numberOfCPUs,' threads)');

    glutInit(@argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGB);
    glutInitWindowSize(xRes,yRes);
    glutCreateWindow(windowTitle);
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
    latestUpdateRequest:=now+10/(24*60*60);
    glutMainLoop();
  end;
end.
