PROGRAM expoClouds;
{$MACRO ON}
{$fputype sse3}
{$define useImageMagick}
USES {$ifdef UNIX}cmem,cthreads,{$endif}
     myFiles,myPics,gl,glext,glut,sysutils,dateutils,math,complex{$ifdef Windows},windows{$endif},Process,darts;
CONST
  integ:array[-1..15] of longint=(-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);

VAR numberOfCPUs:longint=2;
    neededBoxWidth:longint=500;
    xRes,yRes,previewLevel:longint;

    renderStepUp:longint=0;

    viewScaler,currScaler,renderScaler:T_Scaler;
               currImage ,renderImage :T_floatMap;
    renderThreadID:array[0..15] of TThreadID;
    startOfCalculation:double;

    job:record
      name:string;
      xRes,yRes,antiAliasing:string;
    end;

    useBackground:boolean=false;
    background:T_FloatMap;

    param:array[0..1,0..4] of T_complex;
    upperlimit:single=1E3;
    hueOffset :single=0;
    saturation:single=1;
    brightness:single=1;
    sharpening:single=0;
    showResult:boolean=false;

    fullscreenmode:boolean=false;
    viewState:byte=3;
    threadsRunning:longint;
    mouseX,mouseY,mouseDownX,mouseDownY:longint;
    aaMask:T_ByteMap;
    resampling:boolean;
    movingByMouse:boolean;
    repaintPending:boolean=false;
    renderTolerance:single=1;
    latestUpdateRequest:double;


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
      i,j:longint;
  begin
    writeln('Storing state in ',fileName);
    f.createToWrite(fileName);
    f.writesingle(viewScaler.screenCenterX);
    f.writesingle(viewScaler.screenCenterY);
    f.writesingle(viewScaler.relativeZoom);
    for i:=0 to 1 do for j:=0 to 4 do begin
      f.writeSingle(param[i,j].re);
      f.writeSingle(param[i,j].im);
    end;
    f.writeSingle(hueOffset );
    f.writeSingle(upperlimit);
    f.writeSingle(saturation);
    f.writeSingle(brightness);
    f.writeSingle(sharpening);
    f.destroy;
  end;

FUNCTION restoreState(fileName:string):boolean;
  VAR f:T_file;
      sx,sy,z:single;
      i,j:longint;
  begin
    if fileExists(fileName) then begin
      f.createToRead(fileName);
      sx              :=f.readsingle;
      sy              :=f.readsingle;
      z               :=f.readsingle;
      for i:=0 to 1 do for j:=0 to 4 do begin
        param[i,j].re:=f.readSingle;
        param[i,j].im:=f.readSingle;
        param[i,j].valid:=true;
      end;
      hueOffset :=f.readSingle;
      upperlimit:=f.readSingle;
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
      for i:=0 to 1 do for j:=0 to 4 do begin
        param[i,j].re:=random-0.5;
        param[i,j].im:=random-0.5;
        param[i,j].valid:=true;
      end;
      hueOffset :=random;
      upperlimit:=system.exp(system.ln(1E6)*random);
    end;
    viewScaler.recreate(xRes,yRes,sx,sy,z);
  end;

FUNCTION ColorAt(bgColor:T_floatColor; p:T_complex):T_floatColor; inline;
  FUNCTION recColor(p:T_Complex; depth:byte; VAR hits:longint):T_floatColor;
    begin
      result[0]:=sqrabs(p);
      if result[0]<upperLimit then begin
        inc(hits);
        result:=fromHSV(arg(p)/(2*pi)+hueOffset,saturation,brightness*system.sqr(system.sqr((1-result[0]/upperlimit))));
        if depth>0 then result:=result+recColor(param[0,0]+param[0,1]*p+exp(param[0,2]*(param[0,3]+param[0,4]*p)),depth-1,hits)
                                      +recColor(param[1,0]+param[1,1]*p+exp(param[1,2]*(param[1,3]+param[1,4]*p)),depth-1,hits);
      end else result:=black;
    end;

  VAR hitAlpha:longint=0;
  begin
    result:=recColor(p,8,hitAlpha);
    result:=result+(1-hitAlpha/255)*bgColor;
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
    glBegin(GL_QUADS);

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
      gWrite(0,1- 8*14/yRes,'[A] Brightness: '+formatFloat('0.##',brightness*254));
      gWrite(0,1- 9*14/yRes,'[S]aturation: '+formatFloat('0.##',saturation));
      gWrite(0,1-10*14/yRes,'[Y] Limit: '+formatFloat('#E##',upperlimit));
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
      gWrite(0,1-8*14/yRes,'Hint: Save as .ecj for later calculation.');  fetchRasterPos;
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


{FUNCTION psqr(x:T_floatColor):single;inline;
  begin
    result:=(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
  end; }

FUNCTION prepareImage(p:pointer):ptrint;
  VAR x,y:longint;
  begin
    for y:=0 to renderImage.height-1 do if (plongint(p)^<0) or (y mod numberOfCPUs=plongint(p)^) then begin
      if useBackground
        then for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=colorAt(background[x,y],renderScaler.transform(x,y))
        else for x:=0 to renderImage.width -1 do renderImage.pixel[x,y]:=colorAt(black          ,renderScaler.transform(x,y));
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
      case aamask[x,y] of
        1: begin k0:= 1; k1:=  4; end;
        3: begin k0:= 4; k1:=  8; end;
        5: begin k0:= 8; k1:= 16; end;
        7: begin k0:=16; k1:= 32; end;
        9: begin k0:=32; k1:= 64; end;
       11: begin k0:=64; k1:=128; end;
      end;
      fc:=renderImage.pixel[x,y]*k0;
      if useBackground
        then for i:=k0 to k1-1 do fc:=fc+colorAt(background[x,y],renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]))
        else for i:=k0 to k1-1 do fc:=fc+colorAt(black          ,renderScaler.transform(x+darts_delta[i,0],y+darts_delta[i,1]));
      aamask[x,y]:=aamask[x,y]+1;
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

PROCEDURE doJob;
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
    if extractFileExt(job.name)='.ecj' then begin storeState(job.name); exit; end;

    initProgress;
    renderScaler:=viewScaler;
    writeln('Rendering to file...');
    startOfCalculation:=now;
    killRendering;
    renderImage.resizeDat(strToInt(job.xRes),strToInt(job.yRes));
    renderScaler.rescale (strToInt(job.xRes),strToInt(job.yRes));
    previewLevel:=0;
    for it:=1 to numberOfCPUs-1 do
      {$ifdef UNIX}
      beginThread(@prepareImage,@integ[it],renderThreadID[it]);
      {$else}
      renderThreadID[it]:=beginThread(@prepareImage,@integ[it]);
      {$endif}
    prepareImage(@integ[0]);
    for it:=1 to numberOfCPUs-1 do repeat sleep(1) until waitForThreadTerminate(renderThreadID[it],1)=0;
    aaMask.resizeDat(renderImage.width,renderImage.height);
    aaMask.setToValue(0);
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
          {$ifdef UNIX}
          beginThread(@improveImage,@integ[it],renderThreadID[it]);
          {$else}
          renderThreadID[it]:=beginThread(@improveImage,@integ[it]);
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
    if uppercase(extractFileExt(job.name))<>'.VRAW' then begin
      if abs(sharpening)>1E-2 then sharpen(renderImage,2E-3*renderImage.diagonal,1+sharpening);
      shineImage(renderImage);
      colorManipulate(fk_project,0,0,0,renderImage);
    end;
    renderImage.saveToFile(job.name);
    if showResult then backgroundDisplay(job.name);
    writeln(' done in ',(now-startOfCalculation)*24*60*60:0:3,'sec');
    currImage.destroy;
    currImage.createCopy(renderImage);
    currScaler:=         renderScaler;
    previewLevel:=-2;
    glutPostRedisplay;
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
            if key=ord('L') then for x:=0 to 1 do for y:=0 to 4 do begin
              param[x,y].re:=4*(random-0.5)*system.exp(-y*0.2);
              param[x,y].im:=4*(random-0.5)*system.exp(-y*0.2);
              param[x,y].valid:=true;
            end else  for x:=0 to 1 do for y:=0 to 4 do begin
              param[x,y].re:=4*(random-0.5);
              param[x,y].im:=4*(random-0.5);
              param[x,y].valid:=true;
            end;
            brightness:=random/254;
            saturation:=random;
            hueOffset :=random;
            upperlimit:=system.exp(system.ln(1E6)*random);
            previewLevel:=4;
            renderImage.create(xRes shr previewLevel,yRes shr previewLevel);
            renderScaler:=viewScaler;
            renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
            startRendering;
          end;
      ord('a'): begin brightness:=brightness*1.1; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('A'): begin brightness:=brightness/1.1; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('s'): begin saturation:=saturation+0.1; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('S'): begin saturation:=saturation-0.1; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('x'): begin hueOffset :=hueOffset+0.01; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('X'): begin hueOffset :=hueOffset-0.01; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('y'): begin upperlimit:=upperlimit*1.2; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('Y'): begin upperlimit:=upperlimit/1.2; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('p'): begin sharpening:=sharpening+0.1; latestUpdateRequest:=now; glutPostRedisplay; end;
      ord('P'): begin sharpening:=sharpening-0.1; latestUpdateRequest:=now; glutPostRedisplay; end;

      23: //Strg+W
          begin killRendering; storeState('expoClouds.state'); currImage.destroy; renderImage.destroy;  halt; end;
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
      while length(result)<length(intToStr(xmax)) do result:='0'+result;
    end;

  PROCEDURE loadBackground(bgFileName:string);
    begin
      if fileExists(bgFileName) then begin
        background.create(bgFileName);
        useBackground:=true;
      end;
    end;

  VAR i,k,animateSteps:longint;
      jobname,destName:string;
      fmtExt :string;
      info:TSearchRec;
      displayOnly:boolean;
      copyp:array [0..1,0..4] of T_Complex;
  begin
    animateSteps:=0;
    displayOnly:=false;
    result:=false;
    jobname:='';
    fmtExt :='.jpg';
    for i:=1 to paramCount do if paramStr(i)[1]='-' then begin
      if      paramStr(i)[2] in ['1'..'9'] then parseResolution(paramStr(i))
      else if paramStr(i)[2]='a' then animateSteps   :=strToInt  (copy(paramStr(i),3,length(paramStr(i))-2))
      else if paramStr(i)[2]='b' then loadBackground             (copy(paramStr(i),3,length(paramStr(i))-2))
      else if paramStr(i)[2]='t' then renderTolerance:=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2))
      else if paramStr(i)[2]='c' then numberOfCPUs   :=strToInt  (copy(paramStr(i),3,length(paramStr(i))-2))
      else if paramStr(i)[2]='f' then fmtExt         :=           copy(paramStr(i),3,length(paramStr(i))-2)
      else if paramStr(i)[2]='d' then displayOnly:=true
      else if paramStr(i)[2]='s' then showResult:=true
      else if paramStr(i)[2]='h' then begin
                                        writeln('List of command line parameters');
                                        writeln('  -h    :display help and quit');
                                        writeln('  -c<x> :use x CPUs            (default: 2, minimum:1, maximum:32)');
                                        writeln('  -t<r> :set tolerance to r    (default: 1,0)');
                                        writeln('  -<xres>x<yres> chooses resolution; default is screen resolution');
                                        writeln('  -f<r> :set format to r       (for job calculation only; default: jpg)');
                                        writeln('  -d    :do not execute job but open and display job');
                                        writeln('  One file name given will be interpreted as a job!');
                                        writeln('  If several file names are given, only the last one will be employed');
                                        writeln('  If no file name is given, interactive mode is started.');
                                        result:=true;
                                      end;
    end else jobname:=paramStr(i);
    if pos('.',fmtExt)<1 then fmtExt:='.'+fmtExt;
    if jobname<>'' then begin
      result:=true;
      if sysutils.findFirst(jobname,faAnyFile,info)=0 then repeat
        if (info.name<>'.') and (info.name<>'..') then begin
          destName:=ChangeFileExt(extractFilePath(jobname)+info.name,fmtExt);
          if not(fileExists(destName)) or displayOnly then begin
            if (extractFileExt(info.name)='.ecj') and restoreState(extractFilePath(jobname)+info.name) then begin
              job.name:=extractFilePath(jobname)+info.name;
              if not(displayOnly) then begin
                if animateSteps>1 then begin
                  writeln('jobname: ',extractFilePath(jobname)+info.name,' to ',animateSteps,' frames');
                  job.xRes:=intToStr(xres);
                  job.yRes:=intToStr(yRes);
                  job.antiAliasing:=floatToStr(renderTolerance);
                  copyp:=param;
                  for i:=0 to animateSteps-1 do begin
                    job.name:=ChangeFileExt(extractFilePath(jobname)+info.name,nicenumber(i,animateSteps-1)+fmtExt);
                    writeln('jobname: ',extractFilePath(jobname)+info.name);
                    writeln('     to: ',job .name,' @',xres,'x',yres);
                    for k:=0 to 4 do param[0,k]:=copyp[0,k]+(copyp[1,k]-copyp[0,k])*(0.25-0.25*cos(2*pi*i/animateSteps));
                    for k:=0 to 4 do param[1,k]:=copyp[1,k]+(copyp[0,k]-copyp[1,k])*(0.25-0.25*cos(2*pi*i/animateSteps));
                    hueOffset:=hueOffset+1/animateSteps;
                    doJob;
                  end;

                end else begin
                  writeln('jobname: ',extractFilePath(jobname)+info.name);
                  writeln('     to: ',destName,' @',xres,'x',yres);
                  job.xRes:=intToStr(xres);
                  job.yRes:=intToStr(yRes);
                  job.name:=destName;
                  job.antiAliasing:=floatToStr(renderTolerance);
                  doJob;
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
  DecimalSeparator:='.';
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
  job.xRes:=intToStr(xres);
  job.yRes:=intToStr(yRes);
  job.name:='';
  job.antiAliasing:='1';


  glutInit(@argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGB);
  glutInitWindowSize(xRes shr 1,yRes shr 1);
  glutCreateWindow('ExpoClouds by M.S.');

  restoreState('expoClouds.state');
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
  if not(jobbing) then glutMainLoop();
end.
