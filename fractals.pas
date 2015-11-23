PROGRAM fractals;
{$MACRO ON}
{$fputype sse3}
USES {$ifdef UNIX}cmem,cthreads,{$endif}
     myFiles,mypics,gl,glext,glut,sysutils,math,complex{$ifdef Windows},windows{$endif},Process,cmdLineParseUtil,darts,simplePicChunks;
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
    maxDepth:longint=1;
    job:record
      name:string;
      xRes,yRes,antiAliasing:string;
    end;
    colorSource :byte=0;
    colorStyle  :byte=0;
    colorVariant:byte=0;
    pseudoGamma :single=1;
    deltaTransformDone:boolean=false;

    fullscreenmode:boolean=false;
    viewState:byte=3;
    threadsRunning:longint;
    mouseX,mouseY,mouseDownX,mouseDownY:longint;
    movingByMouse:boolean;
    repaintPending:boolean=false;
    latestUpdateRequest:double;
    renderTolerance:single=1;
    showComputedImage:boolean=false;
    rotFactor:T_Complex;

{$include frac.inc}

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
    glBegin(gl_quads);

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
      gWrite(0,1- 8  *14/yRes,'Color[S]ource '+intToStr(colorSource));
      gWrite(0,1- 9  *14/yRes,'[C]oloring '+intToStr(colorStyle));
      gWrite(0,1-10  *14/yRes,'Color[V]ariant '+intToStr(colorVariant));
      gWrite(0,1-11  *14/yRes,'[G]amma '+formatFloat('0.000',pseudoGamma));
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
      gWrite(0,1-4*14/yRes,'[A]A-Tol: '+job.antiAliasing+editMarker(viewState=8)); fetchRasterPos;
      if job.name<>'' then gWrite(0,1-5.5*14/yRes,'[S]tart calculation'); fetchRasterPos;
      gWrite(0,1-6.5*14/yRes,'[C]ancel');  fetchRasterPos;
      gWrite(0,1-8*14/yRes,'Hint: Save as .job for later calculation.');  fetchRasterPos;
    end;
    //glFlush();
    glutSwapBuffers();
  end;

FUNCTION gamma(x:double):double; inline;
  begin
    result:=system.exp(system.ln(x)*pseudoGamma);
  end;

FUNCTION toSphere(x:T_Complex):T_floatColor; inline;
  VAR t:double;
  begin
    t:=4/(4+x.re*x.re+x.im*x.im);
    //if isNAN(t) or isInfinite(t) then t:=0;
    result[0]:=x.re*t;
    result[1]:=x.im*t;
    result[2]:=t*2;
  end;

FUNCTION toSphereZ(x:T_Complex):double; inline;
  begin
    result:=4/(4+x.re*x.re+x.im*x.im);
    //if isNAN(result) or isInfinite(result)
    //  then result:=0 else
    result:=result*2;
  end;


FUNCTION greenzebra(x:double):T_floatColor; inline;
  VAR q:longint;
  begin
    if x<0 then x:=0 else if x>1 then x:=1;
    q:=round(128*x);
    if odd(q) then result:=newVector(0,x,0)
              else result:=newVector(1-x,1-x,1-x);
  end;

FUNCTION zebra(x:double):T_floatColor; inline;
  VAR q:longint;
  begin
    if x<0 then x:=0 else if x>1 then x:=1;
    q:=round(128*x);
    if odd(q) then result:=newVector(0,0,0)
              else result:=newVector(1,1,1);
  end;


FUNCTION greyscale(x:double):T_floatColor; inline;
  begin
    if x<0 then result:=newVector(0,0,0)
    else if x<1   then result:=newVector(x,x,x)
    else result:=newVector(1,1,1);
  end;

FUNCTION fire(x:double):T_floatColor; inline;
  begin
    if x<0 then result:=newVector(0,0,0)
    else if x<1/3 then result:=newVector(3*x,0,0)
    else if x<2/3 then result:=newVector(1,3*x-1,0)
    else if x<1   then result:=newVector(1,1,3*x-2)
    else result:=newVector(1,1,1);
  end;

FUNCTION water(x:double):T_floatColor; inline;
  begin
    if x<0 then result:=newVector(0,0,0)
    else if x<1/2 then result:=newVector(0,0,2*x)
    else if x<1   then result:=newVector(2*x-1,2*x-1,1)
    else result:=newVector(1,1,1);
  end;

FUNCTION earth(x:double):T_floatColor; inline;
  begin
    if x<0 then result:=newVector(0,0,0)
    else if x<1/3 then result:=newVector(3*x,1.5*x,0)
    else if x<2/3 then result:=newVector(  1-0.3*(3*x-1),0.5+0.2*(3*x-1),0.7*(3*x-1))
    else if x<1   then result:=newVector(0.7+0.3*(3*x-2),0.7+0.3*(3*x-2),0.7+0.3*(3*x-2))
    else result:=newVector(1,1,1);
  end;

FUNCTION spectrum(x:double):T_floatColor; inline;
  begin
    x:=x*6;
    if      x<0   then result:=newVector(0,0,0)
    else if x<0.5 then result:=newVector(2*x,0,0)
    else if x<1.5 then result:=newVector(1,x-0.5,0)
    else if x<2.5 then result:=newVector(1-(x-1.5),1,0)
    else if x<3.5 then result:=newVector(0,1,x-2.5)
    else if x<4.5 then result:=newVector(0,1-(x-3.5),1)
    else if x<5.5 then result:=newVector(x-4.5,0,1)
    else if x<6   then result:=newVector(1-2*(x-5.5),0,1-2*(x-5.5))
    else               result:=newVector(0,0,0);
  end;

FUNCTION trafficLight(x:double):T_floatColor; inline;
  begin
    if x<0 then result:=newVector(0.8,0,0)
    else if x<1/2 then result:=newVector(1,2*x,0)
    else if x<1   then result:=newVector(1-(2*x-1),1,0)
    else result:=newVector(0,0.8,0);
  end;

FUNCTION rainbow(x:double):T_floatColor; inline;
  begin
    if x<0 then x:=0
    else if x>1 then x:=6
    else x:=6*x;
    if      x<1 then result:=newVector(1  ,x  ,0  )
    else if x<2 then result:=newVector(2-x,1  ,0  )
    else if x<3 then result:=newVector(0  ,1  ,x-2)
    else if x<4 then result:=newVector(0  ,4-x,1  )
    else if x<5 then result:=newVector(x-4,0  ,1  )
    else             result:=newVector(1  ,0  ,6-x);
  end;

FUNCTION discrete(x:double):T_floatColor; inline;
  VAR i:byte;
  begin
    if      x<0 then i:=0
    else if x>1 then i:=15
                else i:=byte(floor(x*16));
    case i of
      0: result:=newVector(0,0,0);
      1: result:=newVector(0.5,0,0);
      2: result:=newVector(1,0,0);
      3: result:=newVector(1,0.25,0);
      4: result:=newVector(1,0.5,0);
      5: result:=newVector(0.9,0.65,0);
      6: result:=newVector(0.8,0.8,0);
      7: result:=newVector(0.4,0.9,0);
      8: result:=newVector(0,1,0);
      9: result:=newVector(0,0.9,0.4);
     10: result:=newVector(0,0.8,0.8);
     11: result:=newVector(0,0.4,0.9);
     12: result:=newVector(0,0,1);
     13: result:=newVector(0.5,0,1);
     14: result:=newVector(1,0,1);
    else result:=newVector(1,1,1);
    end;
  end;

FUNCTION valueTripletAt(screenX,screenY:double):T_floatColor; inline;
  VAR i:longint;
      x,c:T_Complex;
      r,s,v:T_floatColor;
      sphereZ:double;
  begin
    c:=renderScaler.transform(screenX,screenY,rotFactor);
    iterationStart;
    x.valid:=true;
    i:=0;
    result:=black;
    s:=black;
    v:=black;
    case colorSource of
      0..2: begin
              while (i<maxDepth) and (x.valid) do begin
                iterationStep;
                x.valid:=x.re*x.re+x.im*x.im<1E10;
                //x.valid:=not(isNan(x.re)) and not(isNan(x.im)) and not(isInfinite(x.re)) and not(isInfinite(x.im));
                sphereZ:=toSphereZ(x);
                result[1]:=result[1]+sphereZ;
                result[2]:=result[2]*0.95+0.05*sphereZ;
                inc(i);
              end;
              sphereZ:=toSphereZ(x);
              result[0]:=0.5*sphereZ;
              result[1]:=result[1]+(maxDepth-i)*sphereZ;
              result[1]:=0.5*(result[1]*(1/maxDepth));
              sphereZ:=sphereZ*0.05;
              while (i<maxDepth) do begin result[2]:=result[2]*0.95+sphereZ; inc(i); end;
              result[2]:=0.5*result[2];
            end;
      3..5: begin
              while (i<maxDepth) and (x.re*x.re+x.im*x.im<1E6) do begin
                iterationStep;
                r:=toSphere(x);
                s:=s+r;
                v:=v+newVector(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
                inc(i);
              end;
              result[2]:=i/maxDepth;
              while (i<maxDepth) and (x.re*x.re+x.im*x.im<1E10) do begin
                iterationStep;
//                x.valid:=x.re*x.re+x.im*x.im<1E10;
                r:=toSphere(x);
                s:=s+r;
                v:=v+newVector(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
                inc(i);
              end;
              s:=s+(maxDepth-i)*r;
              v:=v+(maxDepth-i)*newVector(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
              result[0]:=((v[0]+v[1]+v[2])/maxDepth-(s*s)*(1/(maxDepth*maxDepth)));
              result[1]:=arg(x)/(2*pi); if result[1]<0 then result[1]:=result[1]+1;
            end;
            //to do: chaos measure scaled by diagonal, not by pixels!!!
      6..8 : begin
              while (i<maxDepth) and (x.valid) do begin
                iterationStep;
                x.valid:=x.re*x.re+x.im*x.im<1E10;
                r:=toSphere(x)*0.1;
                result:=result*0.9+r;
                inc(i);
              end;
              while (i<maxDepth) do begin result:=result*0.9+r; inc(i); end;
            end;
    end;
  end;

FUNCTION colorAt(screenX,screenY:double; sampleIndex:longint):T_floatColor;
  VAR i:longint;
      x,c:T_Complex;
      aid:double;
      r,s,v:T_floatColor;

  begin
    {$define iterInit:=
    c:=renderScaler.transform(screenX,screenY,rotFactor);
    iterationStart;
    i:=0;
    aid:=0;
    s:=black;
    v:=black}
    case colorSource of
      0: begin
           iterInit;
           while (i<maxDepth) and (x.valid) do begin
             iterationStep;
             x.valid:=x.re*x.re+x.im*x.im<1E10;
             inc(i);
           end;
           aid:=0.5*toSphereZ(x);
         end;
      1: begin
           iterInit;
           while (i<maxDepth) and (x.valid) do begin
             iterationStep;
             x.valid:=x.re*x.re+x.im*x.im<1E10;
             aid:=aid+toSphereZ(x);
             inc(i);
           end;
           aid:=aid+(maxDepth-i)*toSphereZ(x);
           aid:=0.5*(aid*(1/maxDepth));
         end;
      2: begin
           iterInit;
           while (i<maxDepth) and (x.valid) do begin
             iterationStep;
             x.valid:=x.re*x.re+x.im*x.im<1E10;
             aid:=aid*0.95+toSphereZ(x)*0.05;
             inc(i);
           end;
           v[2]:=toSphereZ(x)*0.05;
           while (i<maxDepth) do begin aid:=aid*0.95+v[2]; inc(i); end;
           aid:=0.5*aid;
         end;
      3: begin
           iterInit;
           while (i<maxDepth) and (x.valid) do begin
             iterationStep;
             x.valid:=x.re*x.re+x.im*x.im<1E10;
             r:=toSphere(x);
             s:=s+r;
             v:=v+newVector(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
             inc(i);
           end;
           s:=s+(maxDepth-i)*r;
           v:=v+(maxDepth-i)*newVector(r[0]*r[0],r[1]*r[1],r[2]*r[2]);
           aid:=((v[0]+v[1]+v[2])/maxDepth-(s*s)*(1/(maxDepth*maxDepth)));
         end;
      4: begin
           iterInit;
           while (i<maxDepth) and (x.valid) do begin
             iterationStep;
             x.valid:=x.re*x.re+x.im*x.im<1E10;
             inc(i);
           end;
           aid:=arg(x)/(2*pi);
           if aid<0 then aid:=aid+1;
         end;
      5: begin
           iterInit;
           while (i<maxDepth) and (x.valid) and (x.re*x.re+x.im*x.im<1E6) do begin
             iterationStep;
             x.valid:=x.re*x.re+x.im*x.im<1E10;
             inc(i);
           end;
           aid:=i/maxDepth;
         end;
      6: begin
           aid:=1;//renderScaler.screenDiagonal*1E-3;
           result:=valueTripletAt(screenX    ,screenY    );
           s     :=valueTripletAt(screenX+aid,screenY    );
           v     :=valueTripletAt(screenX    ,screenY-aid);
           aid:=0.25*dist(result,s)+
                0.25*dist(result,v);
         end;
      7: begin
           aid:=1;//renderScaler.screenDiagonal*1E-3;
           result:=valueTripletAt(screenX    ,screenY    );
           s     :=valueTripletAt(screenX+aid,screenY    );
           v     :=valueTripletAt(screenX    ,screenY-aid);
           aid:=sqrt(0.125*sqDist(result,s)+
                     0.125*sqDist(result,v));
         end;
      8: begin
           aid:=1;//renderScaler.screenDiagonal*1E-3;
           //result:=valueTripletAt(screenX                                    ,screenY    );
           //s     :=valueTripletAt(screenX+aid*chaosOrientation[sampleIndex,0],screenY+aid*chaosOrientation[sampleIndex,1]);
           //v     :=valueTripletAt(screenX+aid*chaosOrientation[sampleIndex,1],screenY-aid*chaosOrientation[sampleIndex,0]);
           result:=valueTripletAt(screenX    ,screenY    );
           s     :=valueTripletAt(screenX+aid,screenY    );
           v     :=valueTripletAt(screenX    ,screenY-aid);

           aid:=math.max(0.5*dist(result,s),
                         0.5*dist(result,v));
         end;
    end;
    case colorVariant of
      0: aid:=gamma(                 aid );
      1: aid:=gamma(1-               aid );
      2: aid:=gamma(  system.sqr(1-2*aid));
      3: aid:=gamma(1-system.sqr(1-2*aid));
    end;
    case colorStyle of
      0: result:=fire(aid);
      1: result:=water(aid);
      2: result:=spectrum(aid);
      3: result:=trafficLight(aid);
      4: result:=earth(aid);
      5: result:=greyscale(aid);
      6: result:=zebra(aid);
      7: result:=greenzebra(aid);
      8: result:=rainbow(aid);
      9: result:=discrete(aid);
    end;
  end;


FUNCTION max(x0,x1,x2:single):single; inline;
  begin
    if x0>x1     then result:=x0
                 else result:=x1;
    if x2>result then result:=x2;
  end;

PROCEDURE copyAndTransform;
  VAR pc,pt:P_floatColor;
      i,k,xRes,yRes,x,y:longint;
      cha:array[-1..1] of T_floatColor;
  begin
    currImage.resizeDat(renderImage.width,renderImage.height);
    pt:=renderImage.rawData;
    pc:=currImage.rawData;
    k:=colorSource mod 3;
    if (colorSource>=6) and not(deltaTransformDone) then begin
      move(pt^,pc^,renderImage.size*sizeOf(T_floatColor));
      xRes:=renderImage.width;
      yRes:=renderImage.height;
      for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin
        i:=random(4);
        cha[-1]:=pc[math.max(0,x-1)+y*xRes];
        cha[ 0]:=pc[x              +y*xRes];
        cha[ 1]:=pc[x+math.max(0,y-1)*xRes];
        pt[x+y*xRes]:=newVector(  0.25*dist(cha[0],cha[-1])+   0.25*dist(cha[0],cha[ 1]),
                          sqrt(0.125*sqDist(cha[0],cha[-1])+0.125*sqDist(cha[0],cha[ 1])),
                          math.max(0.5*dist(cha[0],cha[-1]),    0.5*dist(cha[0],cha[ 1])));
      end;
      deltaTransformDone:=true;
    end;

    case colorStyle of
      0: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=fire(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=fire(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=fire(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=fire(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
      1: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=water(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=water(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=water(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=water(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
      2: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=spectrum(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=spectrum(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=spectrum(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=spectrum(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
      3: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=trafficLight(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=trafficLight(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=trafficLight(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=trafficLight(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
      4: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=earth(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=earth(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=earth(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=earth(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
      5: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=greyscale(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=greyscale(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=greyscale(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=greyscale(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
      6: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=zebra(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=zebra(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=zebra(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=zebra(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
      7: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=greenzebra(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=greenzebra(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=greenzebra(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=greenzebra(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
      8: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=rainbow(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=rainbow(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=rainbow(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=rainbow(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
      9: case colorVariant of
           0: for i:=0 to currImage.size-1 do pc[i]:=discrete(gamma(                 pt[i][k] ));
           1: for i:=0 to currImage.size-1 do pc[i]:=discrete(gamma(1-               pt[i][k] ));
           2: for i:=0 to currImage.size-1 do pc[i]:=discrete(gamma(  system.sqr(1-2*pt[i][k])));
           3: for i:=0 to currImage.size-1 do pc[i]:=discrete(gamma(1-system.sqr(1-2*pt[i][k])));
         end;
    end;
    repaintPending:=false;
  end;


PROCEDURE mouseMovePassive(x,y:longint); cdecl;
  begin
    mouseX:=x;
    mouseY:=y;
    mouseDownX:=x;
    mouseDownY:=y;
    if viewState in [1,3] then glutPostRedisplay;
  end;

FUNCTION prepareData(p:pointer):ptrint;
  VAR x,y:longint;
  begin
    for y:=0 to renderImage.height-1 do if (plongint(p)^<0) or (y mod numberOfCPUs=plongint(p)^) then
    for x:=0 to renderImage.width-1 do renderImage.pixel[x,y]:=valueTripletAt(x,y);
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
      rest:=colorAt(chunk.getPicX(i),chunk.getPicY(j),0);
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
        for k:=k0 to k1-1 do rest:=rest+colorAt(
          chunk.getPicX(i)+darts_delta[k,0],
          chunk.getPicY(j)+darts_delta[k,1],k);
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
      beginThread(@prepareData,@integ[it],renderThreadID[it]);
    {$else}
      renderThreadID[it]:=beginThread(@prepareData,@integ[it]);
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
        deltaTransformDone:=false;
      end else if previewLevel>0 then begin
        copyAndTransform;
        currScaler:=renderScaler;
        glTexImage2D (GL_TEXTURE_2D                     ,0,GL_RGB,currImage.width,currImage.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,currImage.rawData);
        dec(previewLevel);
        glutPostRedisplay;
        killRendering;
        renderImage.resizeDat(xRes shr previewLevel,yRes shr previewLevel);
        renderScaler:=viewScaler;
        renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
        startRendering;
        deltaTransformDone:=false;
      end else if previewLevel>-200 then begin
        if previewLevel=0 then writeln('full resolution ready (',(now-startOfCalculation)*24*60*60:7:2,'sec)');
        copyAndTransform;
        currScaler:=renderScaler;
        glTexImage2D (GL_TEXTURE_2D,0,GL_RGB,currImage.width,currImage.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,currImage.rawData);
        glutPostRedisplay;
        dec(previewLevel);
        previewLevel:=-200;
      end else if previewLevel=-200 then begin
        if repaintPending then begin
          copyAndTransform;
          currScaler:=renderScaler;
          glTexImage2D (GL_TEXTURE_2D, 0,GL_RGB,currImage.width,currImage.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,currImage.rawData);
          glutPostRedisplay;
        end else sleep(10);
      end else sleep(1);
    end else if (renderScaler.relativeZoom<0.5*viewScaler.relativeZoom) or (renderScaler.relativeZoom>2*viewScaler.relativeZoom) then begin
      latestUpdateRequest:=now-1;
      //killRendering;
      //previewLevel:=4;
      //renderImage.destroy;
      //renderImage.create(xRes shr previewLevel,yRes shr previewLevel);
      //renderScaler:=viewScaler;
      //renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
      //startRendering;
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
      latestUpdateRequest:=now-1/(24*60*60);
      //killRendering;
      //previewLevel:=4;
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
    renderScaler:=viewScaler;
    writeln('Rendering to file ',job.name,'...');
    startOfCalculation:=now;
    killRendering;
    renderImage.resizeDat(strToInt(job.xRes),strToInt(job.yRes));
    renderScaler.rescale (strToInt(job.xRes),strToInt(job.yRes));
    previewLevel:=0;

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
    renderImage.saveToFile(job.name);
    if showComputedImage then backgroundDisplay(job.name);
    writeln('done in ',myTimeToStr(now-startOfCalculation));
    currImage.destroy;
    currImage.createCopy(renderImage);
    currScaler:=         renderScaler;
    previewLevel:=-2;
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
            previewLevel:=4;
            glutPostRedisplay; //draw;
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
      ord('h'),ord('H'):
          begin
            viewState:=viewState xor 2;
            glutPostRedisplay;
          end;
      ord('i'),ord('I'):
          begin
            writeln(paramStr(0),' -C',colorSource,',',colorStyle,',',colorVariant,' -d',maxDepth,' -x',floatToStr(viewScaler.screenCenterX),' -y',floatToStr(viewScaler.screenCenterY),' -z',floatToStr(viewScaler.relativeZoom),' -g',floatToStr(pseudoGamma),' -',xRes,'x',yRes);
          end;
      ord('s'): begin colorSource :=(colorSource+1) mod 9; if colorSource in [0,3,6] then rerender:=true else repaintPending:=true; end;
      ord('S'): begin colorSource :=(colorSource+8) mod 9; if colorSource in [2,5,8] then rerender:=true else repaintPending:=true; end;
      ord('c'): begin colorStyle  :=(colorStyle+1) mod 10; repaintPending:=true; end;
      ord('C'): begin colorStyle  :=(colorStyle+9) mod 10; repaintPending:=true; end;
      ord('v'): begin colorVariant:=(colorVariant+1) mod 4; repaintPending:=true; end;
      ord('V'): begin colorVariant:=(colorVariant+3) mod 4; repaintPending:=true; end;
      ord('g'): begin pseudoGamma:=pseudoGamma*1.1; repaintPending:=true; end;
      ord('G'): begin pseudoGamma:=pseudoGamma/1.1; repaintPending:=true; end;

      23: //Strg+W
          begin killRendering; currImage.destroy; renderImage.destroy;  halt; end;
      43: //+
          begin
            viewScaler.chooseScreenRef(x,y);
            viewScaler.rezoom(viewScaler.relativeZoom*1.1);
            rerender:=true;
            //previewLevel:=4;
            //glutpostredisplay; //draw;
          end;
      45: //-
          begin
            viewScaler.chooseScreenRef(x,y);
            viewScaler.rezoom(viewScaler.relativeZoom/1.1);
            rerender:=true;
            //previewLevel:=4;
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
      latestUpdateRequest:=now;
      //previewLevel:=4;
      //killRendering;
      //renderImage.create(xRes shr previewLevel,yRes shr previewLevel);
      //renderScaler:=viewScaler;
      //renderScaler.rescale(xRes shr previewLevel,yRes shr previewLevel);
      //startRendering;
    end;
    glutPostRedisplay;
    //update;
  end;

FUNCTION jobbing:boolean;
  CONST cmdList:array [0..13] of T_commandAbstraction=(
    (isFile:true;  leadingSign:' '; cmdString:'';     paramCount: 0),  //0 file (for output)
    (isFile:false; leadingSign:'-'; cmdString:'';     paramCount: 2),  //1 resolution
    (isFile:false; leadingSign:'-'; cmdString:'t';    paramCount: 1),  //2 tolerance
    (isFile:false; leadingSign:'-'; cmdString:'x';    paramCount: 1),  //3 screen center x
    (isFile:false; leadingSign:'-'; cmdString:'y';    paramCount: 1),  //4 screen center y
    (isFile:false; leadingSign:'-'; cmdString:'z';    paramCount: 1),  //5 zoom
    (isFile:false; leadingSign:'-'; cmdString:'g';    paramCount: 1),  //6 gamma
    (isFile:false; leadingSign:'-'; cmdString:'c';    paramCount: 1),  //7 number of CPUs
    (isFile:false; leadingSign:'-'; cmdString:'C';    paramCount: 3),  //8 coloring
    (isFile:false; leadingSign:'-'; cmdString:'d';    paramCount: 1),  //9  iteration depth
    (isFile:false; leadingSign:'-'; cmdString:'f';    paramCount: 0),  //10 force rendering
    (isFile:false; leadingSign:'-'; cmdString:'i';    paramCount: 0),  //11 force interactive mode
    (isFile:false; leadingSign:'-'; cmdString:'show'; paramCount: 0),  //12 show result
    (isFile:false; leadingSign:'-'; cmdString:'rot';  paramCount: 1)); //13 rotation in degrees

  VAR i:longint;
      destName     :string='';
      screenCenterX:double=0;
      screenCenterY:double=0;
      zoom         :double=0.1;
      forceRendering:boolean=false;
      goInteractive :boolean=false;
      ep:T_extendedParameter;
      rotAngle:double=0;
  begin
    result:=false;
    for i:=1 to paramCount do begin
      ep:=extendedParam(i);
      case byte(matchingCmdIndex(ep,cmdList)) of
        0: destName:=ep.cmdString;
        1: begin
             xRes:=ep.intParam[0]; yRes:=ep.intParam[1];
           end;
        2: renderTolerance:=ep.floatParam[0];
        3: screenCenterX:=ep.floatParam[0];
        4: screenCenterY:=ep.floatParam[0];
        5: zoom         :=ep.floatParam[0];
        6: pseudoGamma  :=ep.floatParam[0];
        7: numberOfCPUs :=ep.intParam[0];
        8: begin colorSource:=ep.intParam[0]; colorStyle:=ep.intParam[1]; colorVariant:=ep.intParam[2]; end;
        9: maxDepth     :=ep.intParam[0];
       10: forceRendering:=true;
       11: goInteractive:=true;
       12: showComputedImage:=true;
       13: rotAngle:=pi/180*ep.floatParam[0];
      else begin
             writeln('List of command line parameters');
             writeln('  -h      :display help and quit');
             writeln('  -c<x>   :use x CPUs            (minimum:1, maximum:16)');
             writeln('  -t<r>   :set tolerance to r    (default: 1.0)');
             writeln('  -<xres>x<yres> chooses resolution; default is screen resolution');
             writeln('  -f<r>   :set format to r       (for job calculation only; default: jpg)');
             writeln('  -x<#>   :x position of view');
             writeln('  -y<#>   :y position of view');
             writeln('  -z<#>   :zoom view');
             writeln('  -g<#>   :gamma value before coloring');
             writeln('  -C<s>,<c>,<v> :Coloring <s>tyle, <c>oloring and <v>ariant');
             writeln('  -d<#>   :iteration depth');
             writeln('  -f      :force overwriting of existing files');
             writeln('  -i      :enter interactive mode after rendering');
             writeln('  -rot<x> :rotate by x degrees');
             writeln('  -show   :show computed result via display');
             writeln('  One file name given will be interpreted as a output file!');
             writeln('  If several file names are given, only the last one will be employed');
             writeln('  If no file name is given, interactive mode is started.');
             result:=true;
           end;
      end;
    end;
    writeln('config:');
    writeln('  output file: ',destName,' @',xRes,'x',yRes);
    writeln('  CPUs: ',numberOfCPUs);
    writeln('  view: cx=',floatToStr(screenCenterX),'; cy=',floatToStr(screenCenterY),'; zoom=',floatToStr(zoom),'; rotation=',rotAngle*180/pi:0:2,' degrees');
    writeln('  tolerance: ',floatToStr(renderTolerance));
    writeln('  depth: ',maxDepth);
    writeln('  coloring (source/style/variant):',colorSource,',',colorStyle,',',colorVariant);
    writeln('  gamma: ',floatToStr(pseudoGamma));
    renderScaler.recreate(xRes,yRes,screenCenterX,screenCenterY,zoom);
    viewScaler  .recreate(xRes,yRes,screenCenterX,screenCenterY,zoom);
    if destName<>'' then begin
      result:=not(goInteractive);
      if not(fileExists(destName)) or forceRendering then begin
        rotFactor.re:=system.cos(rotAngle);
        rotFactor.im:=system.sin(rotAngle);
        job.xRes:=intToStr(xRes);
        job.yRes:=intToStr(yRes);
        job.name:=destName;
        job.antiAliasing:=floatToStr(renderTolerance);
        doJob;
      end else writeln('destination file "',destName,'" already exists');
    end;
    rotFactor:=1;
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
  currScaler  :=viewScaler;
  renderScaler:=viewScaler;
  currImage  .create(1,1);
  renderScaler.rescale(xRes shr 5,yRes shr 5);
  renderImage .create(xRes shr 5,yRes shr 5);
//  aaMask.create(xRes,yRes);
  previewLevel:=5;

  if not(jobbing) then begin
    writeln('Open-GL fractals; by Martin Schlegel');
    writeln;
    writeln('compiled on: ',{$I %DATE%});
    writeln('         at: ',{$I %TIME%});
    writeln('FPC version: ',{$I %FPCVERSION%});
    writeln('Target CPU : ',{$I %FPCTARGET%},' (',numberOfCPUs,' threads)');
    rotFactor:=1;
    glutInit(@argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGB);
    glutInitWindowSize(xRes shr 1,yRes shr 1);
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
    latestUpdateRequest:=now;
    glutMainLoop();
  end;
end.
