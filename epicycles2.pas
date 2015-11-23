PROGRAM ifs3;
{$MACRO ON}
{fputype sse3}

USES {$ifdef UNIX}cmem,cthreads,{$endif}
     gl,glut,
     sysutils,dateutils{$ifdef Windows},windows{$endif},math,mypics,myFiles,Process,complex,cmdLineParseUtil;

CONST
  DEF_a='0.75';
  DEF_b='-2';
  DEF_t0='-3.14159265359';
  DEF_t1='3.14159265359';
  DEF_x=0;
  DEF_y=0;
  DEF_z=0.25;
  DEF_brt=1;
  DEF_alpha=0.125;
  DEF_depth=10;
  DEF_qual =1;

VAR //***RESOLUTION DEPENDENT VALUES***
    xRes,yRes:longint;
    aaSamples:longint;

    fullscreenmode:boolean=false;
    //***RESOLUTION DEPENDENT VALUES***
    //********NON-GL RENDERING*********
    picReady:byte=0; //0..63: not fully rendered yet; 64: not displayed yet; 65: everything done
    viewScaler:T_scaler;
    pic,aidPic: T_FloatMap;
    qualityMultiplier:single=1;
    picPointer:P_floatColor;
    //********NON-GL RENDERING*********
    //********IMAGE PARAMETERS*********
    PAR_ALPHA  :single =0.125;
    PAR_DEPTH  :longint=10;
    PAR_INVERT :boolean=false;
    PAR_BRIGHT :single =1;
    PAR_SCALER :T_scaler;
    PAR_a :double=0.75; PAR_a2:double=0.75;
    PAR_b :double=-2;   PAR_b2:double=-2;
    PAR_t0:double=-3.14159265359;
    PAR_t1:double= 3.14159265359;
    //********IMAGE PARAMETERS*********
    startOfRendering:double;
    //***********MENU******************
    showMenu :boolean=true;
    menuState:byte=0;
    MEN_a :string='0.75';
    MEN_b :string='-2';
    MEN_t0:string='-3.14159265359';
    MEN_t1:string='3.14159265359';
    //***********MENU******************

    movingByMouse:boolean=false;
    mouseX,mouseY:longint;         //mouse position
    mouseDownX,mouseDownY:longint;         //mouse position
    lastRezoom:double;

CONST
  rerenderTimeout:double=1/(24*60*60);

PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif}'display '+ps;
    tempProcess.execute;
    tempProcess.free;
  end;

PROCEDURE nonGLRendering(fileGeneration:boolean);
  VAR colorToDraw:T_floatColor;
      nonglAnticover:single;

  CONST aaDart:array[0..63,0..1] of single=
   (( 0.00000000000000E+000, 0.00000000000000E+000),( 4.66709509724751E-001, 4.48971625184640E-001),
    (-4.41653138957918E-001,-4.54999464796856E-001),( 4.99635291285813E-001,-3.91108292387799E-001),
    (-4.08388263313100E-001, 4.63035760913044E-001),(-2.38370760343969E-002,-4.78709440678358E-001),
    ( 1.02372416295111E-002, 4.90799038205296E-001),(-4.33835300849751E-001, 1.19605443906039E-002),
    ( 4.21083272201941E-001, 1.33044233080000E-002),( 2.23091034218669E-001, 2.50794170424342E-001),
    (-1.76511492580175E-001, 2.48841341119260E-001),(-2.35688341315836E-001,-2.23150082165375E-001),
    ( 1.34486772119999E-001,-2.46899318648502E-001),( 2.35968082444742E-001,-4.88652518950403E-001),
    (-4.98525694943965E-001, 2.43373495759442E-001),( 2.36256010597572E-001, 4.86677768640220E-001),
    ( 3.39074678020552E-001,-2.47143527725711E-001),(-2.11629112716764E-001,-1.94954546168447E-003),
    ( 1.87434356892481E-001,-5.16466586850584E-002),(-2.46451322222129E-001,-4.37366404570639E-001),
    ( 4.19718118850142E-001, 2.22282170550898E-001),(-2.03129755333066E-001, 4.39941374585033E-001),
    (-4.74314134567976E-001,-1.88841851428151E-001),( 3.84971636813134E-002, 2.76546730194241E-001),
    (-3.23980259709060E-002,-2.34243197599426E-001),( 1.16398761281744E-001, 1.17142841685563E-001),
    (-2.94518326409161E-001, 1.39904246898368E-001),( 2.60449622757733E-001, 1.01185372797772E-001),
    (-3.86597693897784E-001,-2.97946913633496E-001),(-1.21737705310807E-001, 1.06292497832328E-001),
    ( 4.71638285322115E-001,-1.42086681909859E-001),( 3.20373702794313E-001, 3.70055218925700E-001),
    ( 1.31682120729238E-001, 3.93485445529223E-001),(-2.85847663646564E-001, 3.36150016635656E-001),
    (-9.06911764759571E-002, 3.50781324552372E-001),( 8.20129592902959E-002,-3.99883346399292E-001),
    ( 2.79737100703642E-001,-3.64039894891903E-001),(-8.97914790548384E-002,-3.57168691698462E-001),
    ( 7.20953498966992E-002,-1.33910990785807E-001),(-2.97148112673312E-001,-9.64671922847629E-002),
    (-7.50387692824006E-003, 1.61576115991920E-001),(-3.87318772030994E-001, 2.08940146258101E-001),
    ( 4.06666415045038E-001,-4.72583469934762E-001),(-4.95034943334758E-001, 3.61385176423937E-001),
    ( 3.00419270992279E-001,-4.80285687372089E-002),(-4.82721248641610E-001,-8.41795306187123E-002),
    (-8.93469981383532E-002,-8.23726681992412E-002),( 4.84574421774596E-001,-2.67918060533702E-001),
    ( 4.94015270611271E-001, 3.02904317621142E-001),( 1.76550942007452E-001,-3.37729463586584E-001),
    ( 3.25736629310995E-001, 1.91490679048002E-001),(-3.45574117032811E-001,-3.98570352699608E-001),
    ( 4.78053389815614E-001, 1.37959696585313E-001),(-4.30047455010936E-001, 1.11224441323429E-001),
    (-2.03867033589631E-001,-3.50401686737314E-001),( 2.44178372668102E-001,-2.61477439198643E-001),
    (-3.95055548753590E-001, 3.17402333021164E-001),(-4.72994905896485E-001,-3.57694393256679E-001),
    ( 3.52780097164214E-001, 9.42691743839532E-002),(-3.37592936819419E-001,-1.83457758044824E-001),
    (-1.47056137211621E-003, 3.60773974796757E-001),( 1.10879454296082E-002,-3.42214415315539E-001),
    ( 3.89123832341284E-001,-3.71353844180703E-001),(-2.82449391437694E-001, 2.34517166623846E-001));

VAR xChunk,yChunk:T_Chunk;
    cChunk:array[0..1023] of T_floatColor;
    chunkFill:word=0;

  PROCEDURE flushChunk; inline;
    VAR ix,iy,k:longint;
    begin
      PAR_SCALER.mrofsnart(xChunk,yChunk,chunkFill);
      for k:=0 to chunkFill-1 do begin
        ix:=round(aaDart[picReady,0]+xChunk[k]);
        iy:=round(aaDart[picReady,1]+yChunk[k]);
        if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
          ix:=ix+iy*xRes;
          picPointer[ix]:=picPointer[ix]*nonglAnticover+cChunk[k];
        end;
      end;
      chunkFill:=0;
    end;

  PROCEDURE myVertex2f(x,y:single); inline;
    VAR ix,iy,k:longint;
    begin
      xChunk[chunkFill]:= x;
      yChunk[chunkFill]:=-y;
      cChunk[chunkFill]:=colorToDraw;
      inc(chunkFill);
      if chunkFill>=1024 then begin
        PAR_SCALER.mrofsnart(xChunk,yChunk,chunkFill);
        for k:=0 to chunkFill-1 do begin
          ix:=round(aaDart[picReady,0]+xChunk[k]);
          iy:=round(aaDart[picReady,1]+yChunk[k]);
          if (ix>=0) and (ix<xRes) and (iy>=0) and (iy<yRes) then begin
            ix:=ix+iy*xRes;
            picPointer[ix]:=picPointer[ix]*nonglAnticover+cChunk[k];
          end;
        end;
        chunkFill:=0;
      end;
    end;

  PROCEDURE myColor4f(r,g,b,a:single); inline;
    begin
      //nonGLCover:=a;
      nonglAnticover:=1-a;
      colorToDraw[0]:=r*a;
      colorToDraw[1]:=g*a;
      colorToDraw[2]:=b*a;
    end;

  FUNCTION mySin(t:double):double;
    begin
      t:=t/(2*pi);
      t:=t-floor(t);
      if t<0.5 then result:=1-16*system.sqr(t-0.25)
               else result:=system.sqr(t-0.75)*16-1;
    end;

  FUNCTION myCos(t:double):double;
    begin
      t:=t/(2*pi)+0.25;
      t:=t-floor(t);
      if t<0.5 then result:=1-16*system.sqr(t-0.25)
               else result:=system.sqr(t-0.75)*16-1;
    end;

  VAR i,k,timesteps :longint;
      pt     :P_floatColor;
      fa,fb,x,y,a,b:single;
      coverPerSample:single; //position and color of sample

  begin
    aaSamples:=min(64,max(1,trunc(qualityMultiplier/PAR_ALPHA)));
    if picReady<aaSamples then begin

      qualityMultiplier:=qualityMultiplier/aaSamples;
      pt        :=pic   .rawData;
      picPointer:=aidPic.rawData;
      if picReady=0 then begin
        pt:=pic     .rawData; for i:=0 to pic.size-1      do pt[i]:=black;
      end;

      if PAR_INVERT
        then for i:=0 to aidPic.size-1 do picPointer[i]:=white
        else for i:=0 to aidPic.size-1 do picPointer[i]:=black;

      coverPerSample:=PAR_ALPHA/(qualityMultiplier);
      if PAR_INVERT then myColor4f(1-PAR_BRIGHT,1-PAR_BRIGHT,1-PAR_BRIGHT,coverPerSample)
                    else myColor4f(PAR_BRIGHT,PAR_BRIGHT,PAR_BRIGHT,coverPerSample);
      timesteps:=round(qualityMultiplier*xRes*yRes);
      for i:=0 to timesteps-1 do begin
        a:=PAR_a+random*(PAR_a2-PAR_a);
        b:=PAR_b+random*(PAR_b2-PAR_b);
        if abs(a)>=1 then fa:=1 else fa:=1-abs(a);
        fb:=PAR_t0+(PAR_t1-PAR_t0)*random;
        x:=fa*system.sin(fb);
        y:=fa*system.cos(fb);
        for k:=1 to PAR_DEPTH do begin
          fa:=fa*a;
          fb:=fb*b;
          x:=x+fa*system.sin(fb);
          y:=y+fa*system.cos(fb);
        end;
        myVertex2f(x,y);
      end;
      flushChunk;


      if fileGeneration
        then for i:=1 to pic.size-1 do pt[i]:= pt[i]                     +picPointer[i]*(1/aaSamples)
        else for i:=1 to pic.size-1 do pt[i]:=(pt[i]*(picReady/aaSamples)+picPointer[i]*(1/aaSamples))*(aaSamples/(picReady+1));
      qualityMultiplier:=qualityMultiplier*aaSamples;
      inc(picReady);
      if (picReady=aaSamples) then picReady:=64;
    end else if picReady<64 then picReady:=64;
  end;

PROCEDURE drawMenu;
  VAR font:pointer;

  PROCEDURE gWrite(x,y:float; s:string);
    VAR i:longint;
    begin
      glRasterpos2f(x,y);
      for i:=1 to length(s) do glutBitmapCharacter(font,ord(s[i]));
    end;

  VAR onePixel,lineheight,menuWidth:double;

  begin
    glLoadIdentity;
    glOrtho(0,xRes, yRes,0, -10.0, 10.0);
    onePixel  :=1;
    if xRes<1000 then begin
      font:=GLUT_BITMAP_HELVETICA_10;
      lineheight:=-10*1.2*onePixel;
      menuWidth :=10* 15*onePixel;
    end else if xRes<2000 then begin
      font:=GLUT_BITMAP_HELVETICA_12;
      lineheight:=-12*1.2*onePixel;
      menuWidth :=12* 15*onePixel;
    end else begin
      font:=GLUT_BITMAP_HELVETICA_18;
      lineheight:=-18*1.2*onePixel;
      menuWidth :=18* 15*onePixel;
    end;
    if PAR_INVERT then glColor4f(0.9,0.9,0.9,0.5) else glColor4f(0.1,0.1,0.1,0.5);
    glBegin(gl_quads);
      glVertex2f(0          ,0);
      glVertex2f(0+menuWidth,0);
      glVertex2f(0+menuWidth,0-lineheight*17);
      glVertex2f(0          ,0-lineheight*17);
    glEnd;


    if PAR_INVERT then glColor4f(0,0,0,1) else glColor4f(1,1,1,1);
    gWrite(5*onePixel,-     lineheight,'GL IFS by Martin Schlegel');
    gWrite(5*onePixel,- 2.5*lineheight,'a[V]erage cover: '+formatFloat('0.000',PAR_ALPHA));
    gWrite(5*onePixel,- 3.5*lineheight,'b[R]ightness: '+formatFloat('0.000',PAR_BRIGHT));
    gWrite(5*onePixel,- 4.5*lineheight,'[D]epth: '+intToStr(PAR_DEPTH));
    gWrite(5*onePixel,- 5.5*lineheight,'[C]olor type: '+BoolToStr(PAR_INVERT,'inverted','normal'));

    gWrite(5*onePixel,- 7  *lineheight,'[A]='+MEN_a+BoolToStr(menuState=1,'_',''));
    gWrite(5*onePixel,- 8  *lineheight,'[B]='+MEN_b+BoolToStr(menuState=2,'_',''));
    gWrite(5*onePixel,- 9  *lineheight,'t[0]='+MEN_t0+BoolToStr(menuState=3,'_',''));
    gWrite(5*onePixel,-10  *lineheight,'t[1]='+MEN_t1+BoolToStr(menuState=4,'_',''));

    gWrite(5*onePixel,-11.5*lineheight,'[Q]uality: '+formatFloat(BoolToStr(qualityMultiplier>0.5,'0.0','0.000'),qualityMultiplier));
    gWrite(5*onePixel,-12.5*lineheight,'Toggle [F]ullscreen');
    gWrite(5*onePixel,-13.5*lineheight,'[Ctrl+W] Quit');
    if (picReady<64) then begin
      glColor4f(min(1,2-picReady/aaSamples*2) ,min(1,picReady/aaSamples*2),0,1);
      glBegin(gl_quads);
        glVertex2f((xRes shr 1)-0.6*menuWidth,(yRes shr 1)-0.6*lineheight);
        glVertex2f((xRes shr 1)+0.6*menuWidth,(yRes shr 1)-0.6*lineheight);
        glVertex2f((xRes shr 1)+0.6*menuWidth,(yRes shr 1)+0.6*lineheight);
        glVertex2f((xRes shr 1)-0.6*menuWidth,(yRes shr 1)+0.6*lineheight);
      glEnd;

      glColor4f(0,0,0,1);
      gWrite((xRes shr 1)-0.55*menuWidth,(yRes shr 1)-0.5*lineheight,'RENDERING IN PROGRESS '+intToStr(picReady)+'/'+intToStr(aaSamples));
    end;
  end;

PROCEDURE draw; cdecl;
  VAR ll,ur:T_Complex;

  begin
    glLoadIdentity;
    glOrtho(0,1, 0,1, -10.0, 10.0);
    glClearColor (0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    //real coordinates:
    ll:=PAR_SCALER.transform(0,yRes-1);
    ur:=PAR_SCALER.transform(xRes-1,0);
    //screen coordinates:
    ll:=viewScaler.mrofsnart(ll);
    ur:=viewScaler.mrofsnart(ur);
    //ll:=PAR_Scaler.mrofsnart(ll);
    //ur:=PAR_Scaler.mrofsnart(ur);
    //open-gl coordinates
    ll.re:=ll.re/(xRes-1);
    ur.re:=ur.re/(xRes-1);
    ll.im:=1-(yRes-ll.im)/(yRes-1);
    ur.im:=1-(yRes-ur.im)/(yRes-1);

    glDisable (GL_BLEND);
    glEnable (GL_TEXTURE_2D);
    glBegin(gl_quads);
    glColor3f(1,1,1);
    glTexCoord2f(0.0, 1.0); glnormal3f(0,0,1); glVertex2f(ll.re,ll.im);
    glTexCoord2f(1.0, 1.0); glnormal3f(0,0,1); glVertex2f(ur.re,ll.im);
    glTexCoord2f(1.0, 0.0); glnormal3f(0,0,1); glVertex2f(ur.re,ur.im);
    glTexCoord2f(0.0, 0.0); glnormal3f(0,0,1); glVertex2f(ll.re,ur.im);
    glEnd();
    glDisable (GL_TEXTURE_2D);
    glEnable (GL_BLEND);
    if showMenu then drawMenu;
    glutSwapBuffers();
  end;


PROCEDURE update; cdecl;
  begin
    if now-lastRezoom>rerenderTimeout then begin
      PAR_SCALER:=viewScaler;
      picReady:=0;
      lastRezoom:=now+1;
    end;

    if (picReady<64) then begin
      if picReady=0 then startOfRendering:=now;
      nonGLRendering(false);
      if picReady=64 then
        writeln('rendered in ',(now-startOfRendering)/oneSecond:0:3,'sec ',xRes,'x',yRes,'@Q',qualityMultiplier:0:2);
      glTexImage2D(GL_TEXTURE_2D,0,GL_RGB,pic.width,pic.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,pic.rawData);
      glutPostRedisplay;
    end;
  end;

PROCEDURE reshape(newXRes,newYRes:longint); cdecl;
  begin
    writeln('reshape ',newXRes,'x',newYRes);
    if (newXRes>0) and (newYRes>0) and ((newXRes<>xRes) or (newYRes<>yRes)) then begin

      xRes:=newXRes;
      yRes:=newYRes;
      pic     .resizeDat(xRes,yRes);
      aidPic  .resizeDat(xRes,yRes);
      viewScaler.rescale(xRes,yRes);
      PAR_SCALER.rescale(xRes,yRes);
      glViewport(0, 0,xRes,yRes);
      glLoadIdentity;
      glOrtho(0,1,0,1,-10,10);
      glMatrixMode(GL_MODELVIEW);
      if PAR_INVERT then glClearColor(1,1,1,0) else glClearColor(0,0,0,0);
      picReady:=0;
      glutPostRedisplay();
    end;
  end;

PROCEDURE mouseMoveActive(x,y:longint); cdecl;
  begin
    mouseX:=x;
    mouseY:=y;
    if movingByMouse then begin
      viewScaler.moveCenter(mouseX-mouseDownX,mouseDownY-mouseY);
      mouseDownX:=mouseX;
      mouseDownY:=mouseY;
      glutPostRedisplay;
    end;
  end;

PROCEDURE mousePressFunc(button,state,x,y:longint); cdecl;
  begin
    if (state=glut_down) then begin
      movingByMouse:=(button=2);
      mouseDownX:=x;
      mouseDownY:=y;

    end else if (state=glut_up) and movingByMouse then begin
      PAR_SCALER:=viewScaler;
      picReady:=0;
    end else if (state=glut_up) and (abs(mouseX-mouseDownX)>20) and (abs(mouseY-mouseDownY)>20) then begin
      viewScaler.recenter(viewScaler.transform((mouseX+mouseDownX)*0.5,(mouseY+mouseDownY)*0.5).re,
                          viewScaler.transform((mouseX+mouseDownX)*0.5,(mouseY+mouseDownY)*0.5).im);
      viewScaler.rezoom(viewScaler.relativeZoom*(sqrt((xRes*xRes+yRes*yRes)/
        (system.sqr(mouseX-mouseDownX)+system.sqr(mouseX-mouseDownX)))));
      picReady:=0;
    end;
  end;


PROCEDURE keyboard(key:byte; x,y:longint); cdecl;
  FUNCTION modifyString(VAR s:string; c:char; VAR state:byte):boolean;
    VAR i,j:longint;
        containsDot:boolean;
        pic:string;
    begin
      result:=false;
      containsDot:=pos('.',s)>0;
      case c of
        #8 : s:=copy(s,1,length(s)-1);
        '0'..'9': s:=s+c;
        '.': if not(containsDot) then s:=s+c;
        '-': if s='' then s:='-' else begin
               if containsDot
                 then begin
                   i:=length(s)-pos('.',s);
                   pic:='0.';
                   for j:=1 to i do pic:=pic+'0';
                 end else begin
                   i:=0;
                   pic:='0';
                 end;
               s:=formatFloat(pic,strToFloatDef(s,0)-system.exp(system.ln(0.1)*i));
               result:=true;
             end;
        '+': begin
               if containsDot
                 then begin
                   i:=length(s)-pos('.',s);
                   pic:='0.';
                   for j:=1 to i do pic:=pic+'0';
                 end else begin
                   i:=0;
                   pic:='0';
                 end;
               s:=formatFloat(pic,strToFloatDef(s,0)+system.exp(system.ln(0.1)*i));
               result:=true;
             end;
        #13: begin
               result:=true;
               state:=0;
             end;
      end;
    end;

  begin
    {$define rerender:=picReady:=0;}
    case menuState of
      0: case key of
        23: halt;
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
        ord('C'),ord('c'): begin PAR_INVERT:=not(PAR_INVERT); rerender; end;
        ord('v'): begin PAR_ALPHA:=(PAR_ALPHA*sqrt(sqrt(2))); lastRezoom:=now; glutPostRedisplay; end;
        ord('V'): begin PAR_ALPHA:=(PAR_ALPHA/sqrt(sqrt(2))); lastRezoom:=now; glutPostRedisplay; end;
        ord('d'): begin inc(PAR_DEPTH);                                       lastRezoom:=now; glutPostRedisplay; end;
        ord('D'): begin dec(PAR_DEPTH); if PAR_DEPTH=0 then PAR_DEPTH:=1 else lastRezoom:=now; glutPostRedisplay; end;
        ord('m'),ord('M'): showMenu:=not(showMenu);
        ord('+'): begin
                    viewScaler.chooseScreenRef(x,yRes-1-y);
                    viewScaler.rezoom(viewScaler.relativeZoom*1.05);
                    lastRezoom:=now; glutPostRedisplay;
                  end;
        ord('-'): begin
                    viewScaler.chooseScreenRef(x,yRes-1-y);
                    viewScaler.rezoom(viewScaler.relativeZoom/1.05);
                    lastRezoom:=now; glutPostRedisplay;
                  end;
        ord('r'): begin PAR_BRIGHT:=PAR_BRIGHT*sqrt(sqrt(2)); lastRezoom:=now; glutPostRedisplay; end;
        ord('R'): begin PAR_BRIGHT:=PAR_BRIGHT/sqrt(sqrt(2)); lastRezoom:=now; glutPostRedisplay; end;
        ord('q'): begin qualityMultiplier:=qualityMultiplier*sqrt(2); lastRezoom:=now; glutPostRedisplay; end;
        ord('Q'): begin qualityMultiplier:=qualityMultiplier/sqrt(2); lastRezoom:=now; glutPostRedisplay; end;
        ord('A'),ord('a'): begin menuState:=1; glutPostRedisplay; end;
        ord('B'),ord('b'): begin menuState:=2; glutPostRedisplay; end;
        ord('0')         : begin menuState:=3; glutPostRedisplay; end;
        ord('1')         : begin menuState:=4; glutPostRedisplay; end;
        ord('i'),ord('I'): begin
          write('-',xRes,'x',yRes);
          if MEN_a<>DEF_a then write(' -a',MEN_a);
          if MEN_b<>DEF_b then write(' -b',MEN_b);
          if MEN_t0<>DEF_t0 then write(' -t0=',MEN_t0);
          if MEN_t1<>DEF_t1 then write(' -t1=',MEN_t1);
          write(' -x',floatToStr(viewScaler.screenCenterX),
          ' -y',floatToStr(viewScaler.screenCenterY),' -z',floatToStr(viewScaler.relativeZoom));
          if PAR_BRIGHT<>DEF_brt then write(' -brt',floatToStr(PAR_BRIGHT));
          if PAR_ALPHA<>DEF_alpha then write(' -alpha',floatToStr(PAR_ALPHA));
          if PAR_DEPTH<>DEF_depth then write(' -d',PAR_DEPTH);
          if qualityMultiplier<>DEF_qual then write(' -q',floatToStr(qualityMultiplier));
          writeln;
        end;
      end;
      {MenuState} 1: if modifyString(MEN_a,chr(key),menuState) then begin
        PAR_a:=strToFloat(MEN_a);
        PAR_a2:=PAR_a;
        rerender;
      end else glutPostRedisplay;
      {MenuState} 2: if modifyString(MEN_b,chr(key),menuState) then begin
        PAR_b:=strToFloat(MEN_b);
        PAR_b2:=PAR_b;
        rerender;
      end else glutPostRedisplay;
      {MenuState} 3: if modifyString(MEN_t0,chr(key),menuState) then begin
        PAR_t0:=strToFloat(MEN_t0);
        rerender;
      end else glutPostRedisplay;
      {MenuState} 4: if modifyString(MEN_t1,chr(key),menuState) then begin
        PAR_t1:=strToFloat(MEN_t1);
        rerender;
      end else glutPostRedisplay;
    end;
    update;
  end;

FUNCTION jobbing:boolean;
  CONST cmdList:array [0..15] of T_commandAbstraction=(
    (isFile:true;  leadingSign:' '; cmdString:'';       paramCount: 0),  //0 file (for output)
    (isFile:false; leadingSign:'-'; cmdString:'';       paramCount: 2),  //1 resolution
    (isFile:false; leadingSign:'-'; cmdString:'a';      paramCount: 1),  //2
    (isFile:false; leadingSign:'-'; cmdString:'b';      paramCount: 1),  //3
    (isFile:false; leadingSign:'-'; cmdString:'t';      paramCount: 2),  //4
    (isFile:false; leadingSign:'-'; cmdString:'x';      paramCount: 1),  //5
    (isFile:false; leadingSign:'-'; cmdString:'y';      paramCount: 1),  //6
    (isFile:false; leadingSign:'-'; cmdString:'z';      paramCount: 1),  //7
    (isFile:false; leadingSign:'-'; cmdString:'brt';    paramCount: 1),  //8
    (isFile:false; leadingSign:'-'; cmdString:'alpha';  paramCount: 1),  //9
    (isFile:false; leadingSign:'-'; cmdString:'invert'; paramCount: 0),  //10
    (isFile:false; leadingSign:'-'; cmdString:'d';      paramCount: 1),  //11
    (isFile:false; leadingSign:'-'; cmdString:'q';      paramCount: 1),  //12
    (isFile:false; leadingSign:'-'; cmdString:'force';  paramCount: 0),  //13
    (isFile:false; leadingSign:'-'; cmdString:'a';      paramCount: 2),  //14
    (isFile:false; leadingSign:'-'; cmdString:'b';      paramCount: 2));  //15



  VAR i:longint;
      destName     :string='';
      screenCenterX:double=0;
      screenCenterY:double=0;
      zoom         :double=0.25;
      forceRendering:boolean=false;
      ep:T_extendedParameter;
      lastProgressOutput:double;
  begin
    result:=false;
    for i:=1 to paramCount do begin
      ep:=extendedParam(i);
      case byte(matchingCmdIndex(ep,cmdList)) of
        0: destName:=ep.cmdString;
        1: begin xRes:=ep.intParam[0]; yRes:=ep.intParam[1]; end;
        2: begin PAR_a :=ep.floatParam[0]; MEN_a:=floatToStr(PAR_a); PAR_a2:=PAR_a; end;
        3: begin PAR_b :=ep.floatParam[0]; MEN_b:=floatToStr(PAR_b); PAR_b2:=PAR_b; end;
       14: begin PAR_a :=ep.floatParam[0]; MEN_a:=floatToStr(PAR_a); PAR_a2:=ep.floatParam[1]; end;
       15: begin PAR_b :=ep.floatParam[0]; MEN_b:=floatToStr(PAR_b); PAR_b2:=ep.floatParam[1]; end;
        4: if ep.intParam[0]=0 then begin PAR_t0:=ep.floatParam[1]; MEN_t0:=floatToStr(PAR_t0); end
                               else begin PAR_t1:=ep.floatParam[1]; MEN_t1:=floatToStr(PAR_t1); end;
        5: screenCenterX:=ep.floatParam[0];
        6: screenCenterY:=ep.floatParam[0];
        7: zoom         :=ep.floatParam[0];
        8: PAR_BRIGHT   :=ep.floatParam[0];
        9: PAR_ALPHA    :=ep.floatParam[0];
       10: PAR_INVERT   :=true;
       11: PAR_DEPTH    :=ep.intParam[0];
       12: qualityMultiplier:=ep.floatParam[0];
       13: forceRendering:=true;
      end;
    end;

    viewScaler.recreate(xRes,yRes,screenCenterX,screenCenterY,zoom);
    PAR_SCALER.recreate(xRes,yRes,screenCenterX,screenCenterY,zoom);
    if destName<>'' then begin
      result:=true;
      if not(fileExists(destName)) or forceRendering then begin
        writeln('rendering to: ',destName,' @',xRes,'x',yRes);
        pic   .resizeDat(xRes,yRes);
        aidPic.resizeDat(xRes,yRes);
        picReady:=0;
        lastProgressOutput:=now;
        startOfRendering:=now;
        while picReady<64 do begin
          if (picReady>0) and (now-lastProgressOutput>=oneSecond) then begin
            writeln('      progress: ',picReady/aaSamples*100:0:0,'%; total: ',(now-startOfRendering)*(aaSamples/picReady)/oneSecond:0:2,'sec; remaining: ',(now-startOfRendering)*(aaSamples/picReady-1)/oneSecond:0:2,'sec');
            lastProgressOutput:=now;
          end;
          nonGLRendering(true);
        end;
        pic.saveToFile(destName);
        writeln('done in ',(now-startOfRendering)/oneSecond:0:2,'sec');
      end else writeln('destination file "',destName,'" already exists');
    end else result:=false;
  end;


begin
  randomize;
  SetExceptionMask([ exInvalidOp,
  exDenormalized,
  exZeroDivide,
  exOverflow,
  exUnderflow,
  exPrecision]);


  DecimalSeparator:='.';
  DefaultFormatSettings.DecimalSeparator:='.';
  {$ifdef Windows}
  xRes:=GetSystemMetrics(SM_CXSCREEN);
  yRes:=GetSystemMetrics(SM_CYSCREEN);
  {$else}
  xRes:=1024;
  yRes:=768;
  {$endif}
  viewScaler.create(xRes,yRes,0,0,0.25);
  PAR_SCALER.create(xRes,yRes,0,0,0.25);
  pic   .create(xRes,yRes);
  aidPic.create(xRes,yRes);

  writeln('Epicycles; by Martin Schlegel');
  if not(jobbing) then begin
    writeln;
    writeln('compiled on: ',{$I %DATE%});
    writeln('         at: ',{$I %TIME%});
    writeln('FPC version: ',{$I %FPCVERSION%});
    writeln('Target CPU : ',{$I %FPCTARGET%});

    glutInit(@argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE+GLUT_RGBA);
    glutInitWindowSize(xRes shr 1,yRes shr 1);
    glutCreateWindow('Epicycles by M.S.');

    glClearColor(0,0,0,0);
    glOrtho(0, 1, 0, 1, -10.0, 10.0);
    glMatrixMode(GL_MODELVIEW);
    glutDisplayFunc(@draw);
    glutIdleFunc(@update);
    glutReshapeFunc(@reshape);
    glutKeyboardFunc(@keyboard);
    glutMotionFunc       (@mouseMoveActive );
    glutMouseFunc        (@mousePressFunc  );

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glEnable (GL_BLEND);
    glDisable(GL_DITHER);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glDisable (GL_DEPTH_TEST);
    lastRezoom:=now+1;

    glutMainLoop();
  end;

end.
