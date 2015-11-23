PROGRAM epicycles;
{$MACRO ON}
{define recurseDraw}
{fputype sse2}
USES {$ifdef UNIX}cmem,cthreads,{$endif}gl,glut,sysutils,dateutils,math,complex,mypics,Process;
CONST initialWidth =1024;
      initialHeight=1024;
VAR bmp    :T_FloatMap; //the image itself
    scaler :T_scaler;   //a scaler
    statusLine:array[0..7] of string; //status
    statusMode:byte=1;
    calcTime:double=0;
    totalSamples:longint=0;
    hitSamples:longint=0;

    quietMode:boolean=false;
    alpha:single=0.01;

    a:double=0.5;
    b:double=4;
    t0:double=-pi;
    t1:double= pi;
    t0Hit:double;
    t1Hit:double;
    pt:P_floatColor;
    renderThreadID:TThreadID;
    qualityControl:T_compBaseT=1;
    rerender:boolean=true;
    idling  :boolean=true;
    logColor:boolean=false;

    displayresults:boolean=false;
    waitWhenDone  :boolean=false;
    lastAInc,lastADec:double;
    lastReshape:double;

    summandsNeeded:longint;//=ceil(ln(0.5*scaler.absoluteZoom/(1-abs(a)))/ln(abs(a)));

PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif} 'display '+ps;
    tempProcess.execute;
    tempProcess.free;
  end;

PROCEDURE makeLogscale;
  VAR q,la:double;
      i:longint;
  begin
    if logColor then begin
      la:=1/system.ln(alpha);
      q:=quantile(toCumulative(bmp.getHistogram(ht_redChannel)),0.99)/256;
      q:=0.99/system.ln(1+system.ln(1-q)*la);
      if q<1 then q:=1;
      for i:=0 to bmp.size-1 do pt[i]:=white*(q*system.ln(1+system.ln(1-pt[i,0])*la));
    end;
  end;

PROCEDURE getSample(t:double; VAR sx,sy:longint); inline;
  VAR x,y,fa,fb:double;
      sc:T_Complex;
      k:longint;
  begin
    inc(totalSamples);
    fa:=1-abs(a);
    fb:=t;
    x:=fa*system.sin(fb);
    y:=fa*system.cos(fb);
    for k:=1 to summandsNeeded do begin
      fa:=fa*a;
      fb:=fb*b;
      x:=x+fa*system.sin(fb);
      y:=y+fa*system.cos(fb);
    end;
    sc:=scaler.mrofsnart(x,y); //real to screen
    sx:=round(sc.re);
    sy:=round(sc.im);
  end;

PROCEDURE getASample(t:double; OUT x,y:double);
  VAR fa,fb:double;
      k:longint;
  begin
    fa:=1;
    fb:=t;
    x:=fa*system.sin(fb);
    y:=fa*system.cos(fb);
    for k:=1 to 50 do begin
      fa:=fa*a;
      fb:=fb*b;
      x:=x+fa*system.cos(fb);
      y:=y+fa*system.sin(fb);
    end;
  end;

FUNCTION getDSample(t:double; summands:longint):double;
  CONST twoPi=2*pi;
  VAR x,y,fa,fb:double;
      k:longint;
  begin
    fa:=1;
    fb:=t;
    x:= system.cos(fb);
    y:=-system.sin(fb);
    for k:=1 to summands do begin
      fa:=fa*a*b;
      fb:=fb*b;
      while (fb> twoPi) do fb:=fb-twoPi;
      while (fb<-twoPi) do fb:=fb+twoPi;
      x:=x+fa*system.cos(fb);
      y:=y-fa*system.sin(fb);
    end;
    result:=sqrt(x*x+y*y);
  end;



PROCEDURE measureLength;
  CONST imax=10000;
  VAR i:longint;
      sum:double;
  begin
    sum:=0;
    for i:=0 to imax-1 do sum:=sum+getDSample(t0+(t1-t0)*(i+0.5)/imax,1000);
    if not(quietMode) then writeln('length=',(sum*(t1-t0)/imax)/(2*pi),'*2*pi');
  end;

{$ifdef recurseDraw}
PROCEDURE refine(t0:double; x0,y0:longint; t1:double; x1,y1,depth:longint);
  VAR t2:double;
      x2,y2:longint;
  begin
    if not((x0<-0.1*bmp.width ) and (x1<-0.1*bmp.width ) or
           (x0> 1.1*bmp.width ) and (x1> 1.1*bmp.width ) or
           (y0<-0.1*bmp.height) and (y1<-0.1*bmp.height) or
           (y0> 1.1*bmp.height) and (y1> 1.1*bmp.height)) and (depth>0) then begin
      t2:=(t0+t1)*0.5;
      getSample(t2,x2,y2);
      refine(t0,x0,y0,t2,x2,y2,depth-1);
      refine(t2,x2,y2,t1,x1,y1,depth-1);
    end else //if bmp.incPixel((x0+x1) shr 1,(y0+y1) shr 1,white,alpha) then inc(hitSamples);
             if bmp.incPixel(x0,y0,white,alpha) then inc(hitSamples);
  end;
{$endif}

FUNCTION renderThread(p:pointer):ptrint;
  VAR t,dt:double;
      sx0,sy0,sx1,sy1:longint;
      sampleCount,i,maxNaive:longint;
      hit:array[0..4095] of byte;
  begin
    repeat
      t:=now; totalSamples:=0; hitSamples:=0;
      summandsNeeded:=ceil(system.ln(0.5*scaler.absoluteZoom/(1-abs(a)))/system.ln(abs(a)));
      for sampleCount:=0 to bmp.size-1 do pt[sampleCount]:=black;
      sampleCount:=0;
      rerender:=false;
      {$ifdef recurseDraw}
      maxNaive:=8*4096;
      while (maxNaive>8) and (bmp.size/(qualityControl*maxNaive)<1) do maxNaive:=maxNaive shr 1;
      dt:=(t1-t0)/(maxNaive-1);
      for i:=0 to 4095 do hit[i]:=0;
      for i:=0 to maxNaive-1 do begin
        getSample(t0+dt*i,sx0,sy0);
        if bmp.incPixel(sx0,sy0,white,alpha) then begin
          inc(hitSamples);
          hit[i shr 3]:=hit[i shr 3] or (1 shl (i and 7));
        end;
      end;
      calcTime:=now-t;
      idling:=false;
      sampleCount:=ceil(system.ln(bmp.size/(qualityControl*hitSamples))/system.ln(2));
      if not(quietMode) then writeln(hitSamples/bmp.size:0:5,' ',totalSamples/bmp.size:0:5);
      for i:=0 to maxNaive-2 do if not(rerender) and
        (((hit[ i    shr 3] and (1 shl ( i    and 7)))>0) or
         ((hit[(i+1) shr 3] and (1 shl ((i+1) and 7)))>0)) then begin
        getSample(t0+dt* i   ,sx0,sy0);
        getSample(t0+dt*(i+1),sx1,sy1);
        refine(t0+dt* i   ,sx0,sy0,t0+dt*(i+1),sx1,sy1,sampleCount);
        calcTime:=now-t;
        idling:=false;
      end;
      if not(quietMode) then writeln(hitSamples/bmp.size:0:5,' ',totalSamples/bmp.size:0:5);
      {$else}
      relQ:=1024*1024*qualityControl;
      firstHit:=t0;
      lastHit_:=t1;
      runCounter:=0;
      repeat
        lastHit:=lastHit_;
        if runCounter=0 then begin
        dt :=(t1-t0)/round(bmp.size/relQ);
        t0_:=firstHit;
        end else begin
          dt :=2*relQ*(t1-t0)/bmp.size;
          t0_:=firstHit+dt*0.5;
        end;
        alreadyHit:=false;
        try
        if runCounter<10 then while (t0_<lastHit) and not(rerender)  do begin
          getSample(t0_,sx0,sy0);
          bmp.incPixel(round(sx0),round(sy0),white,alpha);
          t0_:=t0_+dt;
        end else while (t0_<lastHit) and not(rerender)  do begin
          getSample(t0_,sx0,sy0);
          if bmp.incPixel(round(sx0),round(sy0),white,alpha) then begin
            lastHit_:=t0_+4*dt;
            if not(alreadyHit) then begin firstHit:=t0_-4*dt; alreadyHit:=true; end;
          end;
          t0_:=t0_+dt;
        end;
        except relQ:=relQ*2; dec(runCounter); end;
        relQ:=relQ*0.5; inc(runCounter);
        if runCounter>=10 then begin
          if firstHit<t0 then firstHit:=t0; t0Hit:=firstHit;
          if lastHit_>t1 then lastHit_:=t1; t1Hit:=lastHit_;
        end;
      calcTime:=now-t;
      idling:=false;
      until (runCounter>=21) or rerender;
      {$endif}



      if not(rerender) then begin
        if not(quietMode) then writeln('calc done; ',summandsNeeded);
        makeLogscale;
        idling:=false;
        //if a<0.99 then begin a:=a+0.01; rerender:=true; end;
      end;
      while not(rerender) do sleep(100);
    until false;
    result:=0;
  end;

PROCEDURE update; cdecl;
  begin
    sleep(10);
    if not(idling) then glutPostRedisplay;
  end;


PROCEDURE postRecalculation;
//  VAR i:longint;
  begin
    calcTime:=0;
    rerender:=true;
    t0Hit:=t1;
    t1Hit:=t0;

  end;



PROCEDURE reshape(newXRes,newYRes:longint); cdecl;
  begin
    if (now-lastReshape)>0.1/(24*60*60) then begin
      if not(quietMode) then writeln('    reshaping to ',newXRes,'x',newYRes);
      suspendThread(renderThreadID);
      bmp.destroy;
      bmp   .create(newXRes,newYRes);
      pt:=bmp.rawData;
      scaler.rescale(newXRes,newYRes);
      postRecalculation;
      resumeThread(renderThreadID);
      lastReshape:=now;
    end else if not(quietMode) then writeln('not reshaping to ',newXRes,'x',newYRes);
  end;

PROCEDURE draw; cdecl;
  PROCEDURE updateStatus;
    begin
      statusLine[0]:='a='+floatToStr(a);
      statusLine[1]:='b='+floatToStr(b);
      statusLine[2]:='t0='+floatToStr(t0);
      statusLine[3]:='t1='+floatToStr(t1);
      statusLine[4]:='Q='+floatToStr(qualityControl);
      statusLine[6]:=currToStr(calcTime/oneSecond)+'sec.';
      statusLine[7]:=currToStr((totalSamples*0.5E-6*oneSecond)/calcTime)+'Msamples/sec.';
    end;

  VAR i,j:longint;
  begin
    updateStatus;
    glRasterPos2i(0, 0);
    suspendThread(renderThreadID);
    glDrawPixels(bmp.width, bmp.height, GL_RGB,GL_Float, bmp.rawData);
    glColor3f(1,1,1);
    if statusMode<>0 then for i:=0 to 7 do begin
      glRasterPos2i(5,15*(8-i)); for j:=1 to length(statusLine[i]) do glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12 ,ord(statusLine[i][j]));
    end;
    glutSwapBuffers();
    idling:=true;
    resumeThread(renderThreadID);

  end;

PROCEDURE generateFile(fileName:string; xRes,yRes:longint);
  VAR oldW,oldH:longint;
      sum:T_FloatMap;
      j,k,maxNaive:longint;
      darts:T_darts;
      ps:P_floatColor;
      startTime:double;
  VAR t0_,dt:double;
      sx0,sy0,sx1,sy1,i,sampleCount:longint;
      macroShift:double;
      tothitSamples:longint;
      hit:array[0..4095] of byte;
  begin
    suspendThread(renderThreadID);
    summandsNeeded:=ceil(system.ln(0.5*scaler.absoluteZoom/(1-abs(a)))/system.ln(abs(a)));
    oldH:=bmp.height;
    oldW:=bmp.width;
    if (xRes<>oldW) or (yRes<>oldH) then begin
      bmp.destroy;
      bmp.create(xRes,yRes); pt:=bmp.rawData;
      scaler.rescale(xRes,yRes);
    end;
    darts.create(16);
    sum.create(bmp.width,bmp.height); ps:=sum.rawData;
    if not(quietMode) then write('generating ',fileName);
    startTime:=now;
    for j:=0 to bmp.size-1 do ps[j]:=black;
    totalSamples:=0;
    tothitSamples:=0;
    for k:=0 to 15 do begin
      macroShift:=(random-0.5); //(k-7)/16*(t1-t0)/round(bmp.size/(1024*qualityControl))/1024;
      if not(quietMode) then write('.');
      scaler.moveCenter(darts[k,0],darts[k,1]);
      for j:=0 to bmp.size-1 do pt[j]:=black;
      {$ifdef recurseDraw}
      hitSamples:=0;
      maxNaive:=8*4096;
      while (maxNaive>8) and (bmp.size/(qualityControl*maxNaive)<1) do maxNaive:=maxNaive shr 1;
      dt:=(t1-t0)/(maxNaive-1);
      t0_:=t0+macroShift*dt;
      for i:=0 to 4095 do hit[i]:=0;
      for i:=0 to maxNaive-1 do begin
        getSample(t0_+dt*i,sx0,sy0);
        if bmp.incPixel(sx0,sy0,white,alpha) then begin
          inc(hitSamples);
          hit[i shr 3]:=hit[i shr 3] or (1 shl (i and 7));
        end;
      end;
      sampleCount:=ceil(system.ln(bmp.size/(qualityControl*hitSamples))/system.ln(2));
      for i:=0 to maxNaive-2 do if
        (((hit[ i    shr 3] and (1 shl ( i    and 7)))>0) or
         ((hit[(i+1) shr 3] and (1 shl ((i+1) and 7)))>0)) then begin
        getSample(t0_+dt* i   ,sx0,sy0);
        getSample(t0_+dt*(i+1),sx1,sy1);
        refine(t0_+dt* i   ,sx0,sy0,t0_+dt*(i+1),sx1,sy1,sampleCount);
      end;
      tothitSamples:=tothitSamples+hitSamples;

      //dt :=(t1-t0)/round(bmp.size/(1024*qualityControl));
      //t0_:=t0+macroShift;
      //getSample(t0_,sx1,sy1);
      //while (t0_<t1+macroShift) do begin
      //  t0_:=t0_+dt;
      //  sx0:=sx1;
      //  sy0:=sy1;
      //  getSample(t0_,sx1,sy1);
      //  refine(t0_-dt,sx0,sy0,t0_,sx1,sy1,10);
      //end;
      {$else}
      relQ:=1024*1024*qualityControl;
      firstHit:=t0+macroShift;
      lastHit_:=t1+macroShift;
      runCounter:=0;
      repeat
        lastHit:=lastHit_;
        if runCounter=0 then begin
        dt :=(t1-t0)/round(bmp.size/relQ);
        t0_:=firstHit;
        end else begin
          dt :=2*relQ*(t1-t0)/bmp.size;
          t0_:=firstHit+dt*0.5;
        end;
        alreadyHit:=false;
        try
        if runCounter<10 then while (t0_<lastHit) do begin
          getSample(t0_,sx0,sy0);
          if bmp.incPixel(round(sx0),round(sy0),white,alpha) then inc(hitSamples);
          t0_:=t0_+dt;
        end else while (t0_<lastHit) do begin
          getSample(t0_,sx0,sy0);
          if bmp.incPixel(round(sx0),round(sy0),white,alpha) then begin
            inc(hitSamples);
            lastHit_:=t0_+4*dt;
            if not(alreadyHit) then begin firstHit:=t0_-4*dt; alreadyHit:=true; end;
          end;
          t0_:=t0_+dt;
        end;
        except relQ:=relQ*2; dec(runCounter); end;
        relQ:=relQ*0.5; inc(runCounter);
        if runCounter>=10 then begin
          if firstHit<t0+macroShift then firstHit:=t0+macroShift;
          if lastHit_>t1+macroShift then lastHit_:=t1+macroShift;
        end;
      until (runCounter>=21);
      tothitSamples:=hitSamples;
      {$endif}


       { relQ:=1024*1024*qualityControl;
        firstHit:=t0+macroShift;
        lastHit_:=t1+macroShift;
        runCounter:=0;
        repeat
          lastHit:=lastHit_;
          if runCounter=0 then begin
            dt :=relQ*(t1-t0)/bmp.size;
            t0_:=firstHit;
          end else begin
            dt :=2*relQ*(t1-t0)/bmp.size;
            t0_:=firstHit+dt*0.5;
          end;
          alreadyHit:=false;
          if runCounter<10 then while (t0_<lastHit) do begin
            getSample(t0_,sx0,sy0);
            bmp.incPixel(round(sx0),round(sy0),white,alpha);
            t0_:=t0_+dt;
          end else while (t0_<lastHit) do begin
            getSample(t0_,sx0,sy0);
            if bmp.incPixel(round(sx0),round(sy0),white,alpha) then begin
              lastHit_:=t0_+4*dt;
              if not(alreadyHit) then begin firstHit:=t0_-4*dt; alreadyHit:=true; end;
            end;
            t0_:=t0_+dt;
          end;
          relQ:=relQ*0.5; inc(runCounter);
          if runCounter>=10 then begin
            if firstHit<t0+macroShift then firstHit:=t0+macroShift;
            if lastHit_>t1+macroShift then lastHit_:=t1+macroShift;
          end;
        until runCounter>=21; }

      makeLogscale;
      for j:=0 to bmp.size-1 do ps[j]:=ps[j]+pt[j];
      scaler.moveCenter(-darts[k,0],-darts[k,1]);
    end;
    sum.multiplyWith(1/16);
    sum.saveToFile(fileName);
    if not(quietMode) then writeln('done (',(now-startTime)*24*60*60:0:2,'sec) ',totalSamples/sum.size:0:2,'spp / ',tothitSamples/sum.size:0:2,'spp (hit)');
    sum.destroy;
    darts.destroy;
    if (xRes<>oldW) or (yRes<>oldH) then begin
      bmp.destroy;
      bmp.create(oldW,oldH); pt:=bmp.rawData;
      scaler.rescale(oldW,oldH);
      postRecalculation;
    end else idling:=false;
    if waitWhenDone then begin write('Press enter...'); readln; end;
    if displayresults then backgroundDisplay(fileName);
    resumeThread(renderThreadID);
  end;

PROCEDURE keyboard(key:byte; x,y:longint); cdecl;
  PROCEDURE createBitmap(askResolution:boolean);
    VAR fileName:string;
        newW,newH:longint;
    begin
      writeln('Creating Bitmap.');

      if askResolution then begin
        write('xres='); readln(newW);
        write('yres='); readln(newH);
      end else begin
        writeln('xres=',bmp.width);  newW:=bmp.width;
        writeln('yres=',bmp.height); newH:=bmp.height;
      end;
      write('file name: '); readln(fileName);
      generateFile(fileName,newW,newH);
    end;

  CONST TT=0.05/(24*60*60);
  begin
    case key of
     ord('a'): begin if (now-lastAInc)<TT then a:=a+0.01 else a:=a+0.001; if a> 0.999 then a:= 0.999; postRecalculation; lastAInc:=now; lastADec:=0; end;
     ord('A'): begin if (now-lastADec)<TT then A:=a-0.01 else A:=a-0.001; if a<-0.999 then a:=-0.999; postRecalculation; lastAInc:=0; lastADec:=now; end;
     ord('b'): begin b:=b+1; postRecalculation; lastAInc:=0; lastADec:=0; end;
     ord('B'): begin B:=B-1; postRecalculation; lastAInc:=0; lastADec:=0; end;
     ord('t'): begin t0:=(3*t0-t1)/2; t1:=(4*t1-t0)/3;  postRecalculation; end;
     ord('T'): begin t1:=(3*t1+t0)/4; t0:=(2*t0+t1)/3;  postRecalculation; end;
     ord('q'): begin qualityControl:=qualityControl*1.2; idling:=false; end;
     ord('Q'): begin qualityControl:=qualityControl/1.2; idling:=false; end;
      32: {Space} begin inc(statusMode); if statusMode>1 then statusMode:=0; end;
     ord('s'),ord('S'): createBitmap(key=ord('S')); {B}
     ord('r'),ord('R'): begin t0:=0;     t1:=2*pi;  postRecalculation; end;
     ord('x'),ord('X'): begin t0:=t0Hit; t1:=t1Hit; postRecalculation; end;
     ord('l'),ord('L'): measureLength;
      23: //Strg+W
          halt;
      43: //+
          begin
            scaler.chooseScreenRef(x,y);
            scaler.rezoom(scaler.relativeZoom*1.1);
            postRecalculation;
            if not(quietMode) then writeln('x=',scaler.screenCenterX,' y=',scaler.screenCenterY,' z=',scaler.relativeZoom);
          end;
      45: //-
          begin
            scaler.chooseScreenRef(x,y);
            scaler.rezoom(scaler.relativeZoom/1.1);
            postRecalculation;
            if not(quietMode) then writeln('x=',scaler.screenCenterX,' y=',scaler.screenCenterY,' z=',scaler.relativeZoom);
          end;
    end;
    //writeln(key);

    update;
  end;

FUNCTION jobbing:boolean;
  VAR xRes,yRes:longint;
      x,y,z:T_compBaseT;
      spawnCount:longint=0;
      spawned:array of TProcess;
      i:longint;
      fileName:string='';
      jobCommand:string='';
      overrideParams:string='';
      jobFileName:string='';
      jobFileHandle:text;

  PROCEDURE initSpawns;
    VAR i:longint;
    begin
      if spawnCount<0 then spawnCount:=0;
      setLength(spawned,spawnCount);
      for i:=0 to length(spawned)-1 do
        spawned[i] :=TProcess.create(nil);
    end;

  PROCEDURE syncSpawns;
    VAR i:longint;
        alreadyWaiting:boolean=false;
    begin
      for i:=0 to length(spawned)-1 do begin
        if spawned[i].running then begin
          if not(alreadyWaiting) then write('waiting for spawn');
          write(' #',i);
          alreadyWaiting:=true;
        end;
        while spawned[i].running do sleep(10);
      end;
      if alreadyWaiting then writeln;
    end;

  PROCEDURE doneSpawns;
    VAR i:longint;
    begin
      syncSpawns;
      for i:=0 to length(spawned)-1 do spawned[i].free;
    end;

  FUNCTION duplicateMyself(jobToProcess:string):boolean;
    VAR i:longint;
        sleepingTime:longint=1;
    begin
      if spawnCount=0 then result:=false
      else repeat
        result:=false;
        i:=0;
        while not(result) and (i<length(spawned)) do begin
          if spawned[i].running then inc(i)
          else begin
            result:=true;
            if copy(jobToProcess,1,3)='im '
              then spawned[i].CommandLine:={$ifdef UNIX}'./'+{$endif}jobToProcess
              else spawned[i].CommandLine:=paramStr(0)+' '+jobToProcess+' -quiet'+overrideParams;
            spawned[i].execute;
            writeln('spawn #',i,' processing: ',jobToProcess);
          end;
        end;
        if not(result) then sleep(sleepingTime);
        if sleepingTime<100 then inc(sleepingTime);
      until result;
    end;

  PROCEDURE parseResolution(ps:string);
    begin
      ps:=copy(ps,2,length(ps)-1); //remove leading '-'
      xRes:=strToInt(copy(ps,1,pos('x',ps)-1));
      yRes:=strToInt(copy(ps,pos('x',ps)+1,length(ps)-1));
    end;

  begin
    result:=false;
    xRes:=0;
    yRes:=0;
    x:=scaler.screenCenterX;
    y:=scaler.screenCenterY;
    z:=scaler.relativeZoom;
    for i:=1 to paramCount do
    if (paramStr(i)[1]='-') and (paramStr(i)[2] in ['1'..'9']) then begin parseResolution(paramStr(i));                                          overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,2)='a='                         then begin a:=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2));              overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,2)='b='                         then begin b:=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2));              overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,2)='x='                         then begin x:=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2));              overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,2)='y='                         then begin y:=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2));              overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,2)='z='                         then begin z:=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2));              overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,2)='c='                         then begin alpha:=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2));          overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,2)='q='                         then begin qualityControl:=strToFloat(copy(paramStr(i),3,length(paramStr(i))-2)); overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,3)='t0='                        then begin t0:=strToFloat(copy(paramStr(i),4,length(paramStr(i))-3));             overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,3)='t1='                        then begin t1:=strToFloat(copy(paramStr(i),4,length(paramStr(i))-3));             overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,3)='dt='                        then begin t1:=t0+strToFloat(copy(paramStr(i),4,length(paramStr(i))-3));          overrideParams:=overrideParams+' '+paramStr(i); end
    else if paramStr(i)          ='-show'                      then begin displayresults:=true;                                                  overrideParams:=overrideParams+' '+paramStr(i); end
    else if paramStr(i)          ='-wait'                      then       waitWhenDone  :=true
    else if paramStr(i)          ='-log'                       then begin logColor      :=true;                                                  overrideParams:=overrideParams+' '+paramStr(i); end
    else if paramStr(i)          ='-quiet'                     then begin quietMode     :=true;                                                  overrideParams:=overrideParams+' '+paramStr(i); end
    else if copy(paramStr(i),1,6)='-spawn'                     then       spawnCount    :=strToInt(copy(paramStr(i),7,length(paramStr(i))-6))
    else if copy(paramStr(i),1,5)='-job:'                      then       jobFileName   :=copy(paramStr(i),6,length(paramStr(i))-5)
    else if copy(paramStr(i),1,2)='-h' then begin
                                      writeln('List of command line parameters');
                                      writeln('  -h      :display help and quit');
                                      writeln('  a=#     :set a value (default: 0.5)');
                                      writeln('  b=#     :set b value (default: 4)');
                                      writeln('  x=#     :set screen center x (default: 0)');
                                      writeln('  y=#     :set screen center y (default: 0)');
                                      writeln('  c=#     :set cover of samples (default: 0.01)');
                                      writeln('  q=#     :set quality (default: 1; smaller is better/slower)');
                                      writeln('  t0=#    :set start time of curve (default: -pi)');
                                      writeln('  t1=#    :set end time of curve (default: pi)');
                                      writeln('  dt=#    :set t1=t0+#');
                                      writeln('  -log    :logarithmic color scaling');
                                      writeln('  -show   :display created file(s)');
                                      writeln('  -quiet  :do not produce console output');
                                      writeln('  -job:#  :set job file');
                                      writeln('  -spawn# :spawn # copies and act as arbiter');
                                      writeln('  -<xres>x<yres> chooses resolution; default is screen resolution');
                                      writeln('  One file name given will be interpreted as output file!');
                                      writeln('  If insufficient input is given, interactive mode is started.');
                                      result:=true;
                                    end
    else fileName:=paramStr(i);
    if jobFileName='' then begin
      scaler.rezoom(z);
      scaler.recenter(x,y);
      if (fileName<>'') and (xRes<>0) and (yRes<>0)  then begin
        if quietMode then waitWhenDone:=false;
        if (a>=1) or (a<=-1) then writeln('No... |a|>1 will not work at all!')
        else begin
          generateFile(fileName,xRes,yRes);
        end;
        result:=true;
      end;
    end else if fileExists(jobFileName) then begin
      if spawnCount=0 then spawnCount:=1;
      assign(jobFileHandle,jobFileName);
      reset(jobFileHandle);
      initSpawns;
      while not(eof(jobFileHandle)) do begin
        readln(jobFileHandle,jobCommand);
        if (trim(jobCommand)='sync') then syncSpawns else
        if (trim(jobCommand)<>'') then duplicateMyself(jobCommand);
      end;
      close(jobFileHandle);
      doneSpawns;
      result:=true;
    end;

  end;


begin
  DecimalSeparator:='.';
  DefaultFormatSettings.DecimalSeparator:='.';

  lastReshape:=0;
  randomize;
  bmp   .create(initialWidth,initialHeight);
  pt:=bmp.rawData;
  scaler.create(initialWidth,initialHeight,0,0,0.35);

  if not(jobbing) then begin
    if not(quietMode) then writeln('Weierstrass-Epicycles; by Martin Schlegel');
    if not(quietMode) then writeln;
    if not(quietMode) then writeln('compiled on: ',{$I %DATE%});
    if not(quietMode) then writeln('         at: ',{$I %TIME%});
    if not(quietMode) then writeln('FPC version: ',{$I %FPCVERSION%});
    if not(quietMode) then writeln('Target CPU : ',{$I %FPCTARGET%});


//    renderThreadID:=beginThread(@renderThread);
    glutInit(@argc, argv);
    glutInitWindowSize(initialWidth,initialHeight);
    glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGB);
    glutCreateWindow('Weierstrass-Epicycles by M.S.');
    glClearColor(0.0, 0.0, 0.0, 0.0);
    glOrtho(0, initialWidth, 0, initialHeight, -10.0, 10.0);
    glMatrixMode(GL_MODELVIEW);
    glutDisplayFunc(@draw);
    glutIdleFunc(@update);
    glutReshapeFunc(@reshape);
    glutKeyboardFunc(@keyboard);
    {$ifdef UNIX}
    renderThreadID:=beginThread(@renderThread,nil,renderThreadID);
    {$else}
    renderThreadID:=beginThread(@renderThread);
    {$endif}

    glutMainLoop();
  end;
  bmp.destroy;
end.
