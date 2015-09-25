PROGRAM ifs3;
{$MACRO ON}
{$fputype sse3}

USES {$ifdef UNIX}cmem,cthreads,{$endif}
     {$ifndef jobberMode}gl,glut,{$endif}
     sysutils,dateutils{$ifdef Windows},windows{$endif},math,mypics,myfiles,Process,complex;

CONST abortRadius=1E3;
      magicChars='IFSparametersV02';

TYPE T_Trafo=record
       r,g,b:single;
       con:array[0..1]      of single;
       lin:array[0..1,0..1] of single;
       qdr:array[0..1,0..1] of single;
     end;
     T_TrafoTriplet=array[0..2] of T_Trafo;

VAR //***RESOLUTION DEPENDENT VALUES***
    xRes,yRes:longint;
    aasamples:longint;

    {$ifndef jobberMode} fullscreenmode:boolean=false; {$endif}
    //***RESOLUTION DEPENDENT VALUES***
    //***BACKGROUND***
    useBackground:boolean=false;
    bgPic:T_floatMap;
    //***BACKGROUND***
    //********NON-GL RENDERING*********
    picReady:byte=0; //0..63: not fully rendered yet; 64: not displayed yet; 65: everything done
    viewScaler:T_Scaler;
    pic,aidPic: T_floatMap;
    qualityMultiplier:single=1;
    //scaleX,scaleY,offsetX,offsetY:single;
    picPointer:P_floatColor;
    loadedFromFile:string='';

    //noisetable:array[0..31,0..31] of single;
    //********NON-GL RENDERING*********
    //********IMAGE PARAMETERS*********
    PAR_ALPHA  :single =0.125;
    PAR_DEPTH  :longint=128;
    PAR_SEED   :byte   =3;
    PAR_COLOR  :byte   =0;
    PAR_BRIGHT :single =1;
    PAR_SYMMEX :byte   =0;
    PAR_SCALER :T_scaler;
    //rcx,rcy,rcz:single;
    PAR_TRAFO  :array[0..2] of T_TrafoTriplet;
    //********IMAGE PARAMETERS*********

    startOfRendering:double;
    {$ifndef jobberMode}
    showMenu :boolean=true;

    movingByMouse:boolean=false;
    mouseX,mouseY:longint;         //mouse position
    mouseDownX,mouseDownY:longint;         //mouse position
    lastRezoom:double;
    {$endif}
    //t:double;


{$ifndef jobberMode}
CONST
  rerenderTimeout:double=1/(24*60*60);
{$endif}


PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif}'display '+ps;
    tempProcess.execute;
    tempProcess.free;
  end;

PROCEDURE initBackground(fileName:string);
  begin
    useBackground:=true;
    if fileExists(fileName) then begin
      try
        bgPic.create(fileName);
      except
        useBackground:=false;
      end;
    end else useBackground:=false;
    if useBackground then writeln('Background successfully loaded from ',fileName,'; resolution ',bgPic.width,'x',bgPic.height)
                     else writeln('Loading of background from ',fileName,' failed!');
  end;

PROCEDURE saveParameters(fileName:string);
  VAR f:T_file;
      i:longint;
  begin
    f.createToWrite(fileName);
    for i:=1 to length(magicChars) do f.writeChar(magicChars[i]);
    f.writeSingle (PAR_ALPHA  );
    f.writeSingle (PAR_BRIGHT );
    f.writeLongint(PAR_DEPTH  );
    f.writeByte   (PAR_SEED   );
    f.writeByte   (PAR_COLOR  );
    f.writeByte   (PAR_SYMMEX );
    f.writesingle(-PAR_SCALER.screenCenterX);
    f.writesingle( PAR_SCALER.screenCenterY);
    f.writesingle(0.5/PAR_SCALER.relativeZoom );
    f.writeBuf(@PAR_TRAFO,sizeOf(PAR_TRAFO));
    f.destroy;
  end;

FUNCTION loadParameters(fileName:string):boolean;
  VAR f:T_file;
      s:string;
      i:longint;
      rcx,rcy,rcz:single;
  begin
    loadedFromFile:=fileName;
    f.createToRead(fileName);
    s:='';
    for i:=1 to length(magicChars) do s:=s+f.readChar;
    //result:=(s=magicChars);
    result:=true;
    PAR_ALPHA  :=f.readSingle ; result:=result and (PAR_ALPHA>0);
    PAR_BRIGHT :=f.readSingle ; result:=result and (PAR_BRIGHT>0);
    PAR_DEPTH  :=f.readLongint; result:=result and (PAR_DEPTH>=1);
    PAR_SEED   :=f.readByte   ; result:=result and (PAR_SEED  in [0..3]);
    PAR_COLOR  :=f.readByte   ; result:=result and (PAR_COLOR in [0..13]);
    PAR_SYMMEX :=f.readByte   ; result:=result and (PAR_SYMMEX in [0..9]);
    rcx        :=-f.readSingle;
    rcy        := f.readSingle;
    rcz        :=0.5/f.readSingle;
    f.readBuf(@PAR_TRAFO,sizeOf(PAR_TRAFO));
    result:=(f.allokay) and result;
    PAR_SCALER.recreate(xres,yres,rcx,rcy,rcz);
    viewScaler:=PAR_SCALER;
    f.destroy;
  end;

OPERATOR *(x:T_Trafo; y:single):T_Trafo;
  VAR i,j:longint;
  begin
    result.r:=x.r*y;
    result.g:=x.g*y;
    result.b:=x.b*y;
    for i:=0 to 1 do                  result.con[i  ]:=x.con[i  ]*y;
    for i:=0 to 1 do for j:=0 to 1 do result.lin[i,j]:=x.lin[i,j]*y;
    for i:=0 to 1 do for j:=0 to 1 do result.qdr[i,j]:=x.qdr[i,j]*y;
  end;

OPERATOR +(x,y:T_Trafo):T_Trafo;
  VAR i,j:longint;
  begin
    result.r:=x.r+y.r;
    result.g:=x.g+y.g;
    result.b:=x.b+y.b;
    for i:=0 to 1 do                  result.con[i  ]:=x.con[i  ]+y.con[i];
    for i:=0 to 1 do for j:=0 to 1 do result.lin[i,j]:=x.lin[i,j]+y.lin[i,j];
    for i:=0 to 1 do for j:=0 to 1 do result.qdr[i,j]:=x.qdr[i,j]+y.qdr[i,j];
  end;

OPERATOR -(x,y:T_Trafo):T_Trafo;
  VAR i,j:longint;
  begin
    result.r:=x.r-y.r;
    result.g:=x.g-y.g;
    result.b:=x.b-y.b;
    for i:=0 to 1 do                  result.con[i  ]:=x.con[i  ]-y.con[i];
    for i:=0 to 1 do for j:=0 to 1 do result.lin[i,j]:=x.lin[i,j]-y.lin[i,j];
    for i:=0 to 1 do for j:=0 to 1 do result.qdr[i,j]:=x.qdr[i,j]-y.qdr[i,j];
  end;

OPERATOR +(x,y:T_TrafoTriplet):T_TrafoTriplet;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
    result[2]:=x[2]+y[2];
  end;

OPERATOR -(x,y:T_TrafoTriplet):T_TrafoTriplet;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
    result[2]:=x[2]-y[2];
  end;

OPERATOR *(x:T_TrafoTriplet; y:single):T_TrafoTriplet;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
    result[2]:=x[2]*y;
  end;


FUNCTION trafoOfT(t:double; TT:T_TrafoTriplet):T_Trafo;
  begin
    result:=TT[0]*(((t-1)*t)*0.5)
           +TT[1]*(1-t*t        )
           +TT[2]*(((t+1)*t)*0.5);
  end;

PROCEDURE randomColorOnly;
  VAR i,j:longint;
  begin
    picReady:=0;
    for i:=0 to 2 do for j:=0 to 2 do with PAR_TRAFO[i,j] do begin
      r:=random; g:=random; b:=random;
    end;
  end;

PROCEDURE resaturate(factor:single);
  VAR i,j:longint;
      c:T_floatColor;
  begin
    picReady:=0;
    for i:=0 to 2 do for j:=0 to 2 do with PAR_TRAFO[i,j] do begin
      c:=toHSV(newVector(r,g,b));
      c[1]:=c[1]*factor;
      c:=fromHSV(c);
      r:=c[0]; g:=c[1]; b:=c[2];
    end;
  end;

PROCEDURE randomParameters(style:byte);
  VAR i,j:longint;
  begin
    picReady:=0;
    repeat
      for i:=0 to 2 do for j:=0 to 2 do with PAR_TRAFO[i,j] do begin
        r:=random; g:=random; b:=random;
        con[0  ]:=2*(0.5-random);
        con[1  ]:=2*(0.5-random);
        lin[0,0]:=sqrt(2)*(0.5-random);
        lin[0,1]:=sqrt(2)*(0.5-random);
        lin[1,0]:=sqrt(2)*(0.5-random);
        lin[1,1]:=sqrt(2)*(0.5-random);
        qdr[0,0]:=sqrt(2)*(0.5-random);
        qdr[0,1]:=sqrt(2)*(0.5-random);
        qdr[1,0]:=sqrt(2)*(0.5-random);
        qdr[1,1]:=sqrt(2)*(0.5-random);
      end;
      case style of
        1: begin
             PAR_TRAFO[1,0]:=PAR_TRAFO[0,1];
             PAR_TRAFO[1,1]:=PAR_TRAFO[0,2];
             PAR_TRAFO[1,2]:=PAR_TRAFO[0,0];
             PAR_TRAFO[2,0]:=PAR_TRAFO[0,2];
             PAR_TRAFO[2,1]:=PAR_TRAFO[0,0];
             PAR_TRAFO[2,2]:=PAR_TRAFO[0,1];
           end;
        2: begin
             PAR_TRAFO[1,0]:=PAR_TRAFO[0,0];
             PAR_TRAFO[2,0]:=PAR_TRAFO[0,0];
           end;
        3: for i:=0 to 2 do with PAR_TRAFO[i,2] do begin con[0  ]:=0; con[1  ]:=0; end;
        4: for i:=0 to 2 do for j:=1 to 2 do PAR_TRAFO[i,j]:=PAR_TRAFO[i,j]*0.01+PAR_TRAFO[i,0]*0.99;
        5: begin
             PAR_TRAFO[1,0]:=PAR_TRAFO[0,0];
             PAR_TRAFO[2,0]:=PAR_TRAFO[0,0];
             PAR_TRAFO[1,2]:=PAR_TRAFO[0,2];
             PAR_TRAFO[2,2]:=PAR_TRAFO[0,2];
           end;
        6: for i:=0 to 2 do for j:=1 to 2 do PAR_TRAFO[i,j]:=PAR_TRAFO[i,0];
        7: for i:=0 to 2 do for j:=2 downto 0 do PAR_TRAFO[i,j]:=PAR_TRAFO[i,0]*((1+j)/3);
      255: for i:=0 to 2 do for j:=0 to 2 do with PAR_TRAFO[i,j] do begin
             r:=0; g:=0; b:=0;
             con[0  ]:=0;
             con[1  ]:=0;
             lin[0,0]:=0;
             lin[0,1]:=0;
             lin[1,0]:=0;
             lin[1,1]:=0;
             qdr[0,0]:=0;
             qdr[0,1]:=0;
             qdr[1,0]:=0;
             qdr[1,1]:=0;
           end;
      end; //case
    until true; //analyze(false);
    viewScaler:=PAR_SCALER;
  end;


PROCEDURE randomAnimation(frames:longint);
  VAR counterSize:byte;
      prefix:string;
  FUNCTION fileName(index:longint):string;
    begin
      result:=intToStr(index);
      while length(result)<countersize do result:='0'+result;
      result:=prefix+result+'.param';
    end;

  VAR i,j:longint;
      wx,wy:single;
      center,axis1,axis2:record
        PAR_ALPHA  :single ;
        PAR_DEPTH  :longint;
        PAR_SEED   :byte   ;
        PAR_COLOR  :byte   ;
        PAR_BRIGHT :single ;
        PAR_SYMMEX :byte   ;
        rcx,rcy,rcz:single;
        PAR_TRAFO  :array[0..2] of T_TrafoTriplet;
      end;

  begin
    write('ANIMATION PREFIX= '); readln(prefix);
    counterSize:=length(intToStr(frames-1));
    center.PAR_ALPHA :=PAR_ALPHA ;
    center.PAR_DEPTH :=PAR_DEPTH ;
    center.PAR_SEED  :=PAR_SEED  ;
    center.PAR_COLOR :=PAR_COLOR ;
    center.PAR_BRIGHT:=PAR_BRIGHT;
    center.PAR_SYMMEX:=PAR_SYMMEX;
    center.rcx       :=PAR_SCALER.screenCenterX;
    center.rcy       :=PAR_SCALER.screenCenterY;
    center.rcz       :=PAR_SCALER.relativeZoom;
    center.PAR_TRAFO :=PAR_TRAFO ;
    for i:=0 to 2 do for j:=0 to 2 do with axis1.PAR_TRAFO[i,j] do begin
      r:=0.5-random; g:=0.5-random; b:=0.5-random;
      con[0  ]:=2*(0.5-random);
      con[1  ]:=2*(0.5-random);
      lin[0,0]:=sqrt(2)*(0.5-random);
      lin[0,1]:=sqrt(2)*(0.5-random);
      lin[1,0]:=sqrt(2)*(0.5-random);
      lin[1,1]:=sqrt(2)*(0.5-random);
      qdr[0,0]:=sqrt(2)*(0.5-random);
      qdr[0,1]:=sqrt(2)*(0.5-random);
      qdr[1,0]:=sqrt(2)*(0.5-random);
      qdr[1,1]:=sqrt(2)*(0.5-random);
    end;
    for i:=0 to 2 do for j:=0 to 2 do with axis2.PAR_TRAFO[i,j] do begin
      r:=0.5-random; g:=0.5-random; b:=0.5-random;
      con[0  ]:=2*(0.5-random);
      con[1  ]:=2*(0.5-random);
      lin[0,0]:=sqrt(2)*(0.5-random);
      lin[0,1]:=sqrt(2)*(0.5-random);
      lin[1,0]:=sqrt(2)*(0.5-random);
      lin[1,1]:=sqrt(2)*(0.5-random);
      qdr[0,0]:=sqrt(2)*(0.5-random);
      qdr[0,1]:=sqrt(2)*(0.5-random);
      qdr[1,0]:=sqrt(2)*(0.5-random);
      qdr[1,1]:=sqrt(2)*(0.5-random);
    end;

    PAR_ALPHA   :=center.PAR_ALPHA ;
    PAR_DEPTH   :=center.PAR_DEPTH;
    PAR_SEED    :=center.PAR_SEED;
    PAR_COLOR   :=center.PAR_COLOR;
    PAR_BRIGHT  :=center.PAR_BRIGHT;
    PAR_SYMMEX  :=center.PAR_SYMMEX;

    for i:=0 to frames-1 do begin
      wx:=system.sin(2*pi*i/frames)*0.2;
      wy:=system.cos(2*pi*i/frames)*0.2;
      PAR_TRAFO[0]:=center.PAR_TRAFO[0]+axis1.PAR_TRAFO[0]*wx+axis2.PAR_TRAFO[0]*wy;
      PAR_TRAFO[1]:=center.PAR_TRAFO[1]+axis1.PAR_TRAFO[1]*wx+axis2.PAR_TRAFO[1]*wy;
      PAR_TRAFO[2]:=center.PAR_TRAFO[2]+axis1.PAR_TRAFO[2]*wx+axis2.PAR_TRAFO[2]*wy;
      saveParameters(fileName(i));
    end;
    PAR_TRAFO:=center.par_Trafo;
  end;

PROCEDURE nonGLRendering(performShining:boolean);
  CONST ctp:array[0..2,0..1] of single=((0.5*system.sin(0*pi/3),0.5*system.cos(0*pi/3)),
                                        (0.5*system.sin(2*pi/3),0.5*system.cos(2*pi/3)),
                                        (0.5*system.sin(4*pi/3),0.5*system.cos(4*pi/3)));
        c1 =system.cos(2*pi/3); s1 =system.sin(2*pi/3);
        c2 =system.cos(4*pi/3); s2 =system.sin(4*pi/3);
        cp1=system.cos(2*pi/5); sp1=system.sin(2*pi/5);
        cp2=system.cos(4*pi/5); sp2=system.sin(4*pi/5);
        cp3=system.cos(6*pi/5); sp3=system.sin(6*pi/5);
        cp4=system.cos(8*pi/5); sp4=system.sin(8*pi/5);

  VAR colorToDraw:T_floatColor;
      nonglAnticover:single;

  CONST aadart:array[0..63,0..1] of single=
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

  VAR i,j    :longint;
      pt     :P_floatColor;
      pb     :P_floatColor;
      pbX,pbY:longint;
      t:double;
      x,y,xx,yy,x0,y0:single;
      cr,cg,cb,coverPerSample:single; //position and color of sample
      cTrafo:T_TrafoTriplet;                          //current transformation set
      dt:double;                                      //time step
      hueAid:single;
      blurAid1,blurAid2:single;

  begin
    aaSamples:=min(64,max(1,trunc(qualityMultiplier/PAR_ALPHA)));
    if picReady<aaSamples then begin

      qualityMultiplier:=qualityMultiplier/aaSamples;
      pt        :=pic   .rawData;
      picPointer:=aidPic.rawData;
      if picReady=0 then begin
        pt:=pic     .rawData; for i:=0 to pic.size-1      do pt[i]:=black;
      end;

      if useBackground then begin
        pb :=bgPic.rawData;
        pbX:=bgPic.width;
        pbY:=bgPic.height;
        for j:=0 to min(pbY,yRes)-1 do
        for i:=0 to min(pbX,xRes)-1 do
          picPointer[i+j*xRes]:=pb[i+j*pbX];
        if PAR_COLOR in [0..6] then begin
          for j:=0 to yRes-1 do for i:=pbX to xRes-1 do picPointer[i+j*xRes]:=black;
          for j:=pbY to yRes-1 do for i:=0 to xRes-1 do picPointer[i+j*xRes]:=black;
        end else begin
          for j:=0 to yRes-1 do for i:=pbX to xRes-1 do picPointer[i+j*xRes]:=black;
          for j:=pbY to yRes-1 do for i:=0 to xRes-1 do picPointer[i+j*xRes]:=black;
        end;
      end else begin
        if PAR_COLOR in [0..6]
          then for i:=0 to aidPic.size-1 do picPointer[i]:=black
          else for i:=0 to aidPic.size-1 do picPointer[i]:=white;
      end;
      t:=-1+2*PAR_DEPTH/(qualityMultiplier*xRes*yRes)/aaSamples*picReady;
      dt:=  2*PAR_DEPTH/(qualityMultiplier*xRes*yRes);
      coverPerSample:=PAR_ALPHA/(qualityMultiplier);
      while t<1 do begin
        t:=t+dt;
        cTrafo[0]:=trafoOfT(t,PAR_TRAFO[0]);
        cTrafo[1]:=trafoOfT(t,PAR_TRAFO[1]);
        cTrafo[2]:=trafoOfT(t,PAR_TRAFO[2]);
        case PAR_COLOR of
          2,9: myColor4f(PAR_BRIGHT*max(0,(0.5+t*0.5)*3  ),
                         PAR_BRIGHT*max(0,(0.5+t*0.5)*3-1),
                         PAR_BRIGHT*max(0,(0.5+t*0.5)*3-2),coverPerSample);
          3,10: myColor4f(PAR_BRIGHT*min(1,max(0,(0.5-t*0.5)*2-1)),
                       PAR_BRIGHT*min(1,max(0,(0.5-t*0.5)*2-1)),
                       PAR_BRIGHT*min(1,max(0,(0.5-t*0.5)*2  )),coverPerSample);
          4,11: begin
               hueAid:=0.5+t*0.5;
               while hueAid<0 do hueAid:=hueAid+1;
               while hueAid>1 do hueAid:=hueAid-1;
               hueAid:=6*hueAid;
               if hueAid<1      then myColor4f(PAR_BRIGHT*(1       ),PAR_BRIGHT*(hueAid  ),            0        ,coverPerSample)
               else if hueAid<2 then myColor4f(PAR_BRIGHT*(2-hueAid),PAR_BRIGHT*(1       ),            0        ,coverPerSample)
               else if hueAid<3 then myColor4f( 0                   ,PAR_BRIGHT*(1       ),PAR_BRIGHT*(hueAid-2),coverPerSample)
               else if hueAid<4 then myColor4f( 0                   ,PAR_BRIGHT*(4-hueAid),PAR_BRIGHT*(1       ),coverPerSample)
               else if hueAid<5 then myColor4f(PAR_BRIGHT*(hueAid-4),            0        ,PAR_BRIGHT*(1       ),coverPerSample)
               else                  myColor4f(PAR_BRIGHT*(1       ),            0        ,PAR_BRIGHT*(6-hueAid),coverPerSample);
             end;
          5: myColor4f(PAR_BRIGHT,PAR_BRIGHT,PAR_BRIGHT,coverPerSample);
         12: myColor4f(0,0,0,coverPerSample);
          6,13: myColor4f(PAR_BRIGHT,PAR_BRIGHT*0.5,0,coverPerSample);
        end; //case PAR_COLOR

        cr:=0.5; cg:=0.5; cb:=0.5;
        case PAR_SEED of
          0: begin
               repeat
                 x:=2*random-1; y:=2*random-1; xx:=x*x+y*y;
               until (xx<1) and (xx<>0);
               xx:=system.sqrt(-2*system.ln(xx)/xx); x0:=x*xx; y0:=y*xx;
             end;
          1: begin
               repeat
                 x:=2*random-1; y:=2*random-1; xx:=x*x+y*y;
               until (xx<1) and (xx<>0);
               x0:=x*0.5E-2;
               y0:=y*0.5E-2;
             end;
          2: begin x0:=2*pi*random; y0:=0.5*system.sin(x0); x0:=0.5*system.cos(x0); end;
          3: case random(3) of
               0: begin x0:=random; y0:=ctp[0,1]+(ctp[1,1]-ctp[0,1])*x0; x0:=ctp[0,0]+(ctp[1,0]-ctp[0,0])*x0; end;
               1: begin x0:=random; y0:=ctp[1,1]+(ctp[2,1]-ctp[1,1])*x0; x0:=ctp[1,0]+(ctp[2,0]-ctp[1,0])*x0; end;
               2: begin x0:=random; y0:=ctp[2,1]+(ctp[0,1]-ctp[2,1])*x0; x0:=ctp[2,0]+(ctp[0,0]-ctp[2,0])*x0; end;
             end;
        end; //case

        blurAid1:=1-0.5*abs(random+random-1);
        blurAid2:=1-0.5*abs(random+random-1);
        for j:=1 to PAR_DEPTH do begin
          with ctrafo[random(3)] do begin
            x :=x0;
            y :=y0;
            xx:=(x*x-y*y);
            yy:=(2*x*y);
            x0:=(con[0  ]+lin[0,0]*x+lin[0,1]*y+qdr[0,0]*xx+qdr[0,1]*yy);
            y0:=(con[1  ]+lin[1,0]*x+lin[1,1]*y+qdr[1,0]*xx+qdr[1,1]*yy);
            if PAR_COLOR in [0,7] then begin
              cr:=cr*0.5+r*0.5; cg:=cg*0.5+g*0.5; cb:=cb*0.5+b*0.5;
              myColor4f(PAR_BRIGHT*cr,PAR_BRIGHT*cg,PAR_BRIGHT*cb,coverPerSample);
            end else if PAR_COLOR in [1,8] then
              myColor4f(PAR_BRIGHT*r,PAR_BRIGHT*g,PAR_BRIGHT*b,coverPerSample);
          end;
          if (x0*x0+y0*y0>abortRadius) then break else
          case PAR_SYMMEX of
            0: myVertex2f(x0,y0);
            1: begin
                 myVertex2f(( x0),(y0));
                 myVertex2f((-x0),(y0));
               end;
            2: begin
                 myVertex2f((x0),( y0));
                 myVertex2f((x0),(-y0));
               end;
            3: begin
                 myVertex2f(( x0),( y0));
                 myVertex2f((-x0),( y0));
                 myVertex2f(( x0),(-y0));
                 myVertex2f((-x0),(-y0));
               end;
            4: begin
                 myVertex2f(( x0),( y0));
                 myVertex2f((-x0),(-y0));
               end;
            5: begin
                 myVertex2f((         x0),(         y0));
                 myVertex2f((c1*x0+s1*y0),(c1*y0-s1*x0));
                 myVertex2f((c2*x0+s2*y0),(c2*y0-s2*x0));
               end;
            6: begin
                 myVertex2f(( x0),( y0));
                 myVertex2f(( y0),(-x0));
                 myVertex2f((-x0),(-y0));
                 myVertex2f((-y0),( x0));
               end;
            7: begin
                 myVertex2f(( x0       ),(         y0));
                 myVertex2f((cp1*x0+sp1*y0),(cp1*y0-sp1*x0));
                 myVertex2f((cp2*x0+sp2*y0),(cp2*y0-sp2*x0));
                 myVertex2f((cp3*x0+sp3*y0),(cp3*y0-sp3*x0));
                 myVertex2f((cp4*x0+sp4*y0),(cp4*y0-sp4*x0));
               end;
            8: begin
                 myVertex2f((x0         ),(y0         ));
                 myVertex2f((x0*blurAid1),(y0*blurAid1));
                 myVertex2f((x0*blurAid2),(y0*blurAid2));
               end;
            9: begin
                 myVertex2f((x0-2),(y0-2)); myVertex2f((x0-1),(y0-2)); myVertex2f((x0),(y0-2)); myVertex2f((x0+1),(y0-2)); myVertex2f((x0+2),(y0-2));
                 myVertex2f((x0-2),(y0-1)); myVertex2f((x0-1),(y0-1)); myVertex2f((x0),(y0-1)); myVertex2f((x0+1),(y0-1)); myVertex2f((x0+2),(y0-1));
                 myVertex2f((x0-2),(y0  )); myVertex2f((x0-1),(y0  )); myVertex2f((x0),(y0  )); myVertex2f((x0+1),(y0  )); myVertex2f((x0+2),(y0  ));
                 myVertex2f((x0-2),(y0+1)); myVertex2f((x0-1),(y0+1)); myVertex2f((x0),(y0+1)); myVertex2f((x0+1),(y0+1)); myVertex2f((x0+2),(y0+1));
                 myVertex2f((x0-2),(y0+2)); myVertex2f((x0-1),(y0+2)); myVertex2f((x0),(y0+2)); myVertex2f((x0+1),(y0+2)); myVertex2f((x0+2),(y0+2));
               end;
          end;
        end;
      end;
      flushChunk;
      {$ifndef jobberMode}
      for i:=1 to pic.size-1 do pt[i]:=(pt[i]*(picReady/aaSamples)+picPointer[i]*(1/aaSamples))*(aaSamples/(picReady+1));
      {$else}
      for i:=1 to pic.size-1 do pt[i]:= pt[i]+picPointer[i]*(1/aaSamples);
      {$endif}
      qualityMultiplier:=qualityMultiplier*aaSamples;
      inc(picReady);
      if (picReady=aaSamples) then begin
        picReady:=64;
        if performShining then begin
          aidPic.destroy;
          shineImage(pic);
          colorManipulate(fk_project,0,0,0,pic);


          aidPic.create(pic.width,pic.height);
        end;
      end;
    end else if picReady<64 then picReady:=64;
  end;

{$ifndef jobberMode}
PROCEDURE drawMenu;
  VAR font:pointer;

  PROCEDURE gWrite(x,y:float; s:string);
    VAR i:longint;
    begin
      glRasterpos2f(x,y);
      for i:=1 to length(s) do glutBitmapCharacter(font,ord(s[i]));
    end;

  VAR onePixel,lineHeight,menuWidth:double;

  begin
    glLoadIdentity;
    glOrtho(0,xres, yres,0, -10.0, 10.0);
    onePixel  :=1;
    if xRes<1000 then begin
      font:=GLUT_BITMAP_HELVETICA_10;
      lineHeight:=-10*1.2*onePixel;
      menuWidth :=10* 15*onePixel;
    end else if xRes<2000 then begin
      font:=GLUT_BITMAP_HELVETICA_12;
      lineHeight:=-12*1.2*onePixel;
      menuWidth :=12* 15*onePixel;
    end else begin
      font:=GLUT_BITMAP_HELVETICA_18;
      lineHeight:=-18*1.2*onePixel;
      menuWidth :=18* 15*onePixel;
    end;
    if PAR_COLOR in [0..6] then glColor4f(0.1,0.1,0.1,0.5)
                           else glColor4f(0.9,0.9,0.9,0.5);
    glBegin(gl_quads);
      glvertex2f(0          ,0);
      glvertex2f(0+menuWidth,0);
      glvertex2f(0+menuWidth,0-lineHeight*17);
      glvertex2f(0          ,0-lineHeight*17);
    glEnd;


    if PAR_COLOR in [0..6] then glColor4f(1,1,1,1)
                           else glColor4f(0,0,0,1);
    gWrite(5*onePixel,-     lineheight,'GL IFS by Martin Schlegel');
    gWrite(5*onePixel,- 2.5*lineheight,'[A]verage cover: '+formatFloat('0.000',PAR_ALPHA));
    gWrite(5*onePixel,- 3.5*lineheight,'[B]rightness: '+formatFloat('0.000',PAR_BRIGHT));
    gWrite(5*onePixel,- 4.5*lineheight,'[D]epth: '+intToStr(PAR_DEPTH));
    gWrite(5*onePixel,- 5.5*lineheight,'[S]eed type: '+intToStr(PAR_SEED));
    gWrite(5*onePixel,- 6.5*lineheight,'[C]olor type: '+intToStr(PAR_COLOR));

    //gWrite(sx0+5*onePixel,sy1- 7.5*lineheight,'[+/-] zoom level: '+intToStr(PAR_QUANTIL));
    case PAR_SYMMEX of
      0: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (none)');
      1: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (mirror X)');
      2: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (mirror Y)');
      3: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (mirror XY)');
      4: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (rotate 2)');
      5: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (rotate 3)');
      6: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (rotate 4)');
      7: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (rotate 5)');
      8: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (blur)');
      9: gWrite(5*onePixel,- 8.5*lineheight,'Symmetry [G]roup: '+intToStr(PAR_SYMMEX)+' (translate)');
    end; //case
    gWrite(5*onePixel,- 10*lineheight,'[Q]uality: '+formatFloat('0.0',qualityMultiplier));
    //case renderViaPic of
    //  1: gWrite(5*onePixel,-11*lineheight,'[R]endering: offline+display');
    //  2: gWrite(5*onePixel,-11*lineheight,'[R]endering: offline');
    //end;
    gWrite(5*onePixel,-12  *lineheight,'Toggle [F]ullscreen');
    gWrite(5*onePixel,-13.5*lineheight,'[1..5,N]ext image');
    gWrite(5*onePixel,-14.5*lineheight,'[X] next coloring');
    gWrite(5*onePixel,-15.5*lineheight,'[Space] Toggle Saver Mode');
    gWrite(5*onePixel,-16.5*lineheight,'[Ctrl+W] Quit');
    if (picReady<64) then begin
      glColor4f(min(1,2-picReady/aaSamples*2) ,min(1,picReady/aaSamples*2),0,1);
      glBegin(gl_quads);
        glvertex2f((xres shr 1)-0.6*menuWidth,(yres shr 1)-0.6*lineHeight);
        glvertex2f((xres shr 1)+0.6*menuWidth,(yres shr 1)-0.6*lineHeight);
        glvertex2f((xres shr 1)+0.6*menuWidth,(yres shr 1)+0.6*lineHeight);
        glvertex2f((xres shr 1)-0.6*menuWidth,(yres shr 1)+0.6*lineHeight);
      glEnd;

      glColor4f(0,0,0,1);
      gWrite((xres shr 1)-0.55*menuWidth,(yres shr 1)-0.5*lineHeight,'RENDERING IN PROGRESS '+intToStr(picready)+'/'+intToStr(aasamples));
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
    ll:=PAR_SCALER.transform(0,yres-1);
    ur:=PAR_SCALER.transform(xres-1,0);
    //screen coordinates:
    ll:=viewScaler.mrofsnart(ll);
    ur:=viewScaler.mrofsnart(ur);
    //ll:=PAR_Scaler.mrofsnart(ll);
    //ur:=PAR_Scaler.mrofsnart(ur);
    //open-gl coordinates
    ll.re:=ll.re/(xRes-1);
    ur.re:=ur.re/(xRes-1);
    ll.im:=1-(yres-ll.im)/(yRes-1);
    ur.im:=1-(yres-ur.im)/(yRes-1);

    glDisable (GL_BLEND);
    glEnable (GL_TEXTURE_2D);
    glBegin(GL_QUADS);
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
      nonGLRendering(true);
      if picReady=64 then writeln('rendered in ',myTimeToStr(now-startOfRendering),' (offline+display) ',xres,'x',yres,'@Q',qualityMultiplier:0:2);
      glTexImage2D(GL_TEXTURE_2D,0,GL_RGB,pic.width,pic.height,0,GL_RGB,{GL_UNSIGNED_BYTE}GL_Float,pic.rawData);
      glutPostRedisplay;
    end;
  end;
{$endif}

PROCEDURE reshape(newXRes,newYRes:longint); cdecl;
  begin

    {$ifndef jobberMode}
    writeln('reshape ',newXRes,'x',newYRes);
    if (newXRes>0) and (newYRes>0) and ((newXRes<>xRes) or (newYRes<>yRes)) then begin {$endif}

      xRes:=newxRes;
      yRes:=newyRes;
      pic     .resizeDat(xRes,yRes);
      aidPic  .resizeDat(xRes,yRes);
      viewScaler.rescale(xRes,yRes);
      PAR_SCALER.rescale(xRes,yRes);
      {$ifndef jobberMode}
      glViewport(0, 0,xres,yres);
      glLoadIdentity;
      glOrtho(0,1,0,1,-10,10);
      glMatrixMode(GL_MODELVIEW);
      if PAR_COLOR in [0..6] then glClearColor(0,0,0,0) else glClearColor(1,1,1,0);
      picReady:=0;
      glutPostRedisplay();
      {$endif}
    {$ifndef jobberMode} end; {$endif}
  end;
{$ifndef jobberMode}

//PROCEDURE mouseMovePassive(x,y:longint); cdecl;
//  begin
//    mx:=x; my:=y;
//  end;

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
  VAR fileName:string;
  begin
    {$define rerender:=picReady:=0;}
    case key of
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
      ord('c'): begin PAR_COLOR:=(PAR_COLOR+1) mod 14; rerender; end;
      ord('C'): begin PAR_COLOR:=(PAR_COLOR+13) mod 14; rerender; end;
      ord('x'),ord('X'): begin randomColorOnly; rerender; end;
      ord('s'): begin PAR_SEED :=(PAR_SEED +1) mod 4; lastRezoom:=now; glutPostRedisplay; end;
      ord('S'): begin PAR_SEED :=(PAR_SEED +3) mod 4; lastRezoom:=now; glutPostRedisplay; end;
      ord('a'): begin PAR_ALPHA:=(PAR_ALPHA*sqrt(sqrt(2))); lastRezoom:=now; glutPostRedisplay; end;
      ord('A'): begin PAR_ALPHA:=(PAR_ALPHA/sqrt(sqrt(2))); lastRezoom:=now; glutPostRedisplay; end;
      ord('d'): begin PAR_DEPTH:=(PAR_DEPTH+PAR_DEPTH);                                   lastRezoom:=now; glutPostRedisplay; end;
      ord('D'): begin PAR_DEPTH:=(PAR_DEPTH shr 1); if PAR_DEPTH=0 then PAR_DEPTH:=1 else lastRezoom:=now; glutPostRedisplay; end;
      ord('m'),ord('M'): showMenu:=not(showMenu);
      ord('+'): begin
                  viewScaler.chooseScreenRef(x,yres-1-y);
                  viewScaler.rezoom(viewScaler.relativeZoom*1.05);
                  lastRezoom:=now; glutPostRedisplay;
                end;
      ord('-'): begin
                  viewScaler.chooseScreenRef(x,yres-1-y);
                  viewScaler.rezoom(viewScaler.relativeZoom/1.05);
                  lastRezoom:=now; glutPostRedisplay;
                end;
      ord('n'),ord('N'):  begin randomParameters(random(6)); rerender; end;
      ord('1')..ord('8'): begin randomParameters(key-ord('1')); rerender; end;
      ord('r'): begin viewScaler.recenter(viewScaler.transform(x,y).re,viewScaler.transform(x,yRes-1-y).im); rerender; end;
      ord('R'): begin viewScaler.recenter(0,0); rerender; end;
      ord('g'): begin PAR_SYMMEX:=(PAR_SYMMEX+1) mod 10; lastRezoom:=now; glutPostRedisplay; end;
      ord('G'): begin PAR_SYMMEX:=(PAR_SYMMEX+9) mod 10; lastRezoom:=now; glutPostRedisplay; end;
      ord('b'): begin PAR_BRIGHT:=PAR_BRIGHT*sqrt(sqrt(2)); lastRezoom:=now; glutPostRedisplay; end;
      ord('B'): begin PAR_BRIGHT:=PAR_BRIGHT/sqrt(sqrt(2)); lastRezoom:=now; glutPostRedisplay; end;
      ord('L'),ord('l'):randomAnimation(50);
      ord('q'): begin qualityMultiplier:=qualityMultiplier*sqrt(2); lastRezoom:=now; glutPostRedisplay; end;
      ord('Q'): begin qualityMultiplier:=qualityMultiplier/sqrt(2); if qualityMultiplier<1 then qualityMultiplier:=1 else lastRezoom:=now; glutPostRedisplay; end;
      ord('p'): begin
        if loadedFromFile='' then begin
          x:=0; while (fileExists('ifs'+intToStr(x)+'.png')) or (fileExists('ifs'+intToStr(x)+'.param')) do inc(x);
          pic.saveToFile('ifs'+intToStr(x)+'.png');
          writeln('saved ifs'+intToStr(x)+'.param and ifs'+intToStr(x)+'.png');
          backgroundDisplay('ifs'+intToStr(x)+'.png');
          saveParameters('ifs'+intToStr(x)+'.param');
        end else begin
          saveParameters(loadedFromFile);
          writeln('saved ',loadedFromFile);
        end;
      end;
      ord('P'): begin
        write('filename (without extension): '); readln(fileName);
        pic.saveToFile(fileName+'.png');
        writeln('saved '+fileName+'.param and '+fileName+'.png');
        backgroundDisplay(fileName+'.png');
        saveParameters(fileName+'.param');
      end;
      ord('o'),ord('O'): begin
        x:=0; while (fileExists('ifs_screenshot'+intToStr(x)+'.png')) do inc(x);
                 pic.saveToFile('ifs_screenshot'+intToStr(x)+'.png');
              writeln('saved '+('ifs_screenshot'+intToStr(x)+'.png'));
        backgroundDisplay('ifs_screenshot'+intToStr(x)+'.png');
      end;
      ord('Y'): begin resaturate(1/1.1); lastRezoom:=now; glutPostRedisplay; end;
      ord('y'): begin resaturate(  1.1); lastRezoom:=now; glutPostRedisplay; end;
    end;
    update;
  end;

PROCEDURE parseNonJobber;
  VAR i:longint;
  begin
    randomParameters(random(5));
    for i:=1 to paramCount do
      if      (copy(paramStr(i),1,2)='-b') then initBackground(copy(paramStr(i),3,length(paramStr(i))-2))
      else if fileExists(paramStr(i)) and loadParameters(paramStr(i)) then begin end;
  end;
{$else}
PROCEDURE jobbing;
  VAR info:TSearchRec;
      filesToCheck:array of string;
      i:longint;
      enforceRender   :boolean=false;
      enforceRendering:boolean;
      showResult:boolean=false;
      fileExt:string;
      lastProgressOutput:double;
      inputError:boolean;
      quietMode:boolean=false;

  PROCEDURE parseResolution(ps:string);
    begin
      ps:=copy(ps,2,length(ps)-1); //remove leading '-'
      try
        xRes:=strToInt(copy(ps,1,pos('x',ps)-1));
        yRes:=strToInt(copy(ps,pos('x',ps)+1,length(ps)-1));
        if not (quietMode) then writeln('Resolution set to ',xres,'x',yres);
      except inputError:=true; end;
    end;

  PROCEDURE parseQuality(ps:string);
    begin
      ps:=copy(ps,3,length(ps)-2); //remove leading '-q'
      try
        qualityMultiplier:=strToFloat(ps);
        if not (quietMode) then writeln('Quality set to ',qualityMultiplier:0:1);
      except inputError:=true; end;
    end;

  VAR shine:boolean;

  begin
    if paramCount=0 then begin
      writeln('IFS jobber...');
      writeln;
      writeln('Command line options:');
      writeln('  -<xres>x<yres> chooses resolution; default is screen resolution');
      writeln('  -q<N> chooses quality; <N> is a float with a "." as decimal separator');
      writeln('  -t<ext> chooses the extension; <ext> may contain the leading dot');
      writeln('  -b<filename> chooses a background image');
      writeln('  -show display result after computation');
      writeln('  -quiet no console output');
      writeln('  -force compute image even if result seems to exist');
      writeln('  If a filename is given, image parameters from this file are loaded and');
      writeln('  the image is rendered. If no name is given, all images not previously');
      writeln('  rendered are created.');
      writeln;
      writeln('Example: ifsjobber -1024x768 -q4.2 -tjpg');
      writeln('  - resolution is 1024x768; quality is 4.2; file type is .jpg');
      writeln('  - the program will look for all .param files for which no .jpg file');
      writeln('    of equal name is found and render it.');
      inputError:=true;
    end else inputError:=false;

    fileExt:='.png';
    shine:=true;
    setLength(filesToCheck,0);
    for i:=1 to paramCount do begin
      if       paramStr(i)='-show'                                    then showResult:=true
      else if  paramStr(i)='-quiet'                                   then quietmode:=true
      else if  paramStr(i)='-force'                                   then enforceRender:=true
      else if  paramStr(i)='-shine'                                   then shine:=true
      else if (paramStr(i)[1]='-') and (paramStr(i)[2] in ['1'..'9']) then parseResolution(paramStr(i))
      else if (paramStr(i)[1]='-') and (paramStr(i)[2]='q')           then parseQuality   (paramStr(i))
      else if (paramStr(i)[1]='-') and (paramStr(i)[2] in ['f','t'])  then begin
        fileExt:=copy(paramStr(i),3,length(paramStr(i))-2);
        if not (quietMode) then writeln('Result file extension set to "',fileExt,'"');
        shine:=(uppercase(fileExt)<>'VRAW') and (uppercase(extractFileExt(fileExt))<>'.VRAW');
      end else if (paramStr(i)[1]='-') and (paramStr(i)[2]='b')           then begin
        initBackground(copy(paramStr(i),3,length(paramStr(i))-2));
        inputError:=inputError or not(useBackground);
      end else if (paramStr(i)[1]='-') then begin
        if not (quietMode) then writeln('Unrecognized option "',paramStr(i),'"');
        inputError:=true;
      end else begin
        setLength(filesToCheck,length(filesToCheck)+1);
        filesToCheck[length(filesToCheck)-1]:=paramStr(i);
      end;
    end;
    if pos('.',fileext)=0 then fileExt:='.'+fileExt;
    reshape(xres,yres);
    if length(filesToCheck)=0 then begin
      setLength(filesToCheck,1);
      filesToCheck[0]:='*.param';
    end;
    if not (quietMode) then begin
      if shine then writeln('Shining enabled')
               else writeln('Shining disabled');
    end;
    if not(inputError) then for i:=0 to length(filesToCheck)-1 do begin
      enforceRendering:=enforceRender or (pos('*',filesToCheck[i])=0);
      if findFirst(filesToCheck[i],faAnyFile,info)=0 then repeat
        if ((info.Attr and faDirectory)<>faDirectory)
        and loadParameters(extractFilePath(filesToCheck[i])+info.name) and (enforceRendering or not(fileExists(ChangeFileExt(extractFilePath(filesToCheck[i])+info.name,fileExt)))) then begin
            if not(quietMode) then writeln('parameters loaded from: ',extractFilePath(filesToCheck[i])+info.name);
            if not(quietMode) then                writeln('         creating file: ',ChangeFileExt(extractFilePath(filesToCheck[i])+info.name,fileExt),' @',xres,'x',yres);
//            if not(quietMode) and not(shine) then writeln('         and shine map: ',ChangeFileExt(ChangeFileExt(ExtractFilePath(filesToCheck[i])+info.Name,fileExt),'_shine.vraw'),' @',xres,'x',yres);
            startOfRendering:=now;
            lastProgressOutput:=now;
            picReady:=0;
            while picReady<64 do begin
              if (picReady>0) and (now-lastProgressOutput>=oneSecond) then begin
                if not (quietMode) then writeln('      progress: ',picReady/aaSamples*100:2:0,'%; total: ',myTimeToStr((now-startOfRendering)*(aaSamples/picReady)),'; remaining: ',myTimeToStr((now-startOfRendering)*(aaSamples/picReady-1)));
                lastProgressOutput:=now;
              end;
              nonGLRendering(shine);
            end;
            pic.saveToFile(ChangeFileExt(extractFilePath(filesToCheck[i])+info.name,fileExt));
            //if not(shine) then shinePic.saveToFile(ChangeFileExt(ChangeFileExt(ExtractFilePath(filesToCheck[i])+info.Name,fileExt),'_shine.vraw'));
            if quietMode then writeln(ChangeFileExt(extractFilePath(filesToCheck[i])+info.name,fileExt),' created in ',myTimeToStr(now-startOfRendering))
                         else writeln('done in ',myTimeToStr(now-startOfRendering));
            if showResult then backgroundDisplay(ChangeFileExt(extractFilePath(filesToCheck[i])+info.name,fileExt));
        end;
      until (findNext(info)<>0);
    end;
  end;

{$endif}

begin
  randomize;
  randomParameters(0);

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
  viewScaler.create(xres,yres,0,0,1);
  PAR_SCALER.create(xres,yres,0,0,1);
  pic   .create(xRes,yRes);
  aidPic.create(xRes,yRes);
  {$ifdef jobberMode}
  jobbing;
  {$else}
  parseNonJobber;
  writeln('IFS; by Martin Schlegel');
  writeln;
  writeln('compiled on: ',{$I %DATE%});
  writeln('         at: ',{$I %TIME%});
  writeln('FPC version: ',{$I %FPCVERSION%});
  writeln('Target CPU : ',{$I %FPCTARGET%});
  writeln;
  writeln('Use a previously generated .param file as command line parameter');
  writeln('in order to view and modify the stored image description.');

  glutInit(@argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE+GLUT_RGBA);
  glutInitWindowSize(xRes shr 1,yRes shr 1);
  glutCreateWindow('IFS by M.S.');

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
  {$endif}
end.
