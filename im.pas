PROGRAM im;
USES {$ifdef UNIX}cmem,cthreads,{$endif}mypics,sysutils,math,Process,cmdLineParseUtil,displayUtil;
VAR pic:T_FloatMap;
    inputReady:boolean=false;
    lastImage:string='';
    firstImage:string='';
    progStart:double;
    stepStart:double;
    verboseMode:boolean=false;
    sizeLimit:longint=maxLongint;

PROCEDURE displayHelp;
  begin
    displayHelpOnColorManipulations;
    displayHelpOnFilters;
    displayHelpOnImageCombinations;
    writeln('General');
    writeln('  -verbose      display processing details');
    writeln('  -show         show current image (wait for Esc)');
    writeln('  -show<n>      show current image for n seconds');
    writeln('  -reload       reload first input');
    writeln('  -quality<n>   set quality for lossy formats (0<=n<=100)');
    writeln('  -sizeLimit<n> set size limit for jpg');
    writeln('  -erasure      erasure effect (b/w)');
    writeln('Geometry');
    writeln('  -<res>    resize image to given resolution (e.g. 800x600)');
    writeln('  -fit<res> fit image to given resolution');
    writeln('  -cr<res>  crop-resize image to given resolution');
    writeln('  -crop<x0>:<x1>x<y0>:<y1>  crop image');
    writeln('  -enlarge<xRes,yRes,r,g,b> enlarge image adding a frame of color (r,g,b)');
    writeln('  -flip     flip image');
    writeln('  -flop     flop image');
    writeln('  -rotL     rotate left by 90 degrees');
    writeln('  -rotR     rotate right by 90 degrees');
    writeln('  -rot<n>   rotate by n degrees');
    writeln('Generation');
    writeln('  -PERLIN<s>,<f>,<k>  Generate perlin noise with factors s and f and random seed k');
    writeln('  -shine              distribute overbright pixels');
    writeln('  -GRAD<v0>,<v1>,<angle>                      create a grey gradient');
    writeln('  -GRAD<r0>,<g0>,<b0>,<r1>,<g1>,<b1>,<angle>  create a color gradient');
    writeln('  -NOISE              classical, naive noise');
  end;

PROCEDURE erasure;
  VAR x,y,g:longint;
  begin
    for y:=0 to pic.height-1 do for x:=0 to pic.width-1 do begin
      g:=round(7*greyLevel(pic[x,y]));
      if g<0 then g:=0 else if g>6 then g:=6;
      case g of
        0:                                          pic[x,y]:=black;
        1: if (y and 7)=0 then pic[x,y]:=white else pic[x,y]:=black;
        2: if (y and 3)=0 then pic[x,y]:=white else pic[x,y]:=black;
        3: if (y and 1)=0 then pic[x,y]:=white else pic[x,y]:=black;
        4: if (y and 3)<3 then pic[x,y]:=white else pic[x,y]:=black;
        5: if (y and 7)<7 then pic[x,y]:=white else pic[x,y]:=black;
        6:                     pic[x,y]:=white;
      end;
    end;
  end;
  
PROCEDURE generateNoise;
  VAR pt:P_floatColor;
      i:longint;
  begin
    pt:=pic.rawData;
    for i:=0 to pic.size-1 do pt[i]:=random*white;
  end;

PROCEDURE generateGradient(r0,g0,b0,r1,g1,b1,angle:single);
  VAR x,y:longint;
      c0,c1:T_floatColor;
      nx,ny,w:single;
  begin
    c0:=newVector(r0,g0,b0);
    c1:=newVector(r1,g1,b1)-c0;
    nx:=2*cos(pi/180*angle)/pic.diagonal;
    ny:=2*sin(pi/180*angle)/pic.diagonal;
    for y:=0 to pic.height-1 do
    for x:=0 to pic.width-1 do begin
      w:=(x-pic.width/2)*nx+
         (y-pic.height/2)*ny;
      if w>1 then w:=1
      else if w<-1 then w:=0
      else w:=(w+1)*0.5;
      pic[x,y]:=c0+w*c1;
    end;
  end;

PROCEDURE generatePerlinNoise(scaleFactor,amplitudeFactor:single; seed:longint);
  VAR perlinTable:array[0..31,0..31] of single;
      perlinLine :array of array[0..31] of single;

  PROCEDURE initPerlinTable;
    VAR i,j:longint;
    begin
      if seed=-1 then randomize
                 else randseed:=seed;
      for i:=0 to 31 do for j:=0 to 31 do perlinTable[i,j]:=random-0.5;
      randomize;
    end;

  PROCEDURE updatePerlinLine(y:double; lineIdx:longint; amplitude:single); inline;
    VAR ix,iy:longint;
        j0,j1,j2,j3:longint;
        q0,q1,q2,q3:single;
    begin
      if (lineIdx and 1)>0 then y:=-y;
      if (lineIdx and 2)>0 then amplitude:=-amplitude;
      iy:=floor(y); y:=y-iy;
      q0:=amplitude*(y*(-0.5+(1-y*0.5)*y));
      q1:=amplitude*(1+y*y*(-2.5+(3*y)*0.5));
      q2:=amplitude*(y*(0.5+(2-(3*y)*0.5)*y));
      q3:=amplitude*((-0.5+y*0.5)*y*y);
      j0:=(iy  ) and 31;
      j1:=(iy+1) and 31;
      j2:=(iy+2) and 31;
      j3:=(iy+3) and 31;
      if (lineIdx and 4)=0 then begin
        for ix:=0 to 31 do perlinLine[lineIdx,ix]:=
          perlinTable[ix,j0]*q0+
          perlinTable[ix,j1]*q1+
          perlinTable[ix,j2]*q2+
          perlinTable[ix,j3]*q3;
      end else begin
        for ix:=0 to 31 do perlinLine[lineIdx,ix]:=
          perlinTable[j0,ix]*q0+
          perlinTable[j1,ix]*q1+
          perlinTable[j2,ix]*q2+
          perlinTable[j3,ix]*q3;
      end;
    end;

    FUNCTION getSmoothValue(x:double; lineIdx:longint):single; inline;
      VAR ix:longint;
      begin
        ix:=floor(x); x:=x-ix;
        result:=perlinLine[lineIdx,(ix  ) and 31]*(x*(-0.5+(1-x*0.5)*x))   +
                perlinLine[lineIdx,(ix+1) and 31]*(1+x*x*(-2.5+(3*x)*0.5)) +
                perlinLine[lineIdx,(ix+2) and 31]*(x*(0.5+(2-(3*x)*0.5)*x))+
                perlinLine[lineIdx,(ix+3) and 31]*((-0.5+x*0.5)*x*x)       ;
      end;

  VAR xRes,yRes:longint;
      x,y,l,lMax:longint;
      scale:array of double;
      amplitude:array of double;
      aid:double;
  begin
    initPerlinTable;
    xRes:=pic.width;
    yRes:=pic.height;

    if scaleFactor>1 then begin
      scaleFactor:=1/scaleFactor;
      amplitudeFactor:=1/amplitudeFactor;
    end;

    aid:=0;
    setLength(amplitude,1);
    setLength(scale,1);
    amplitude[0]:=1;
    scale[0]:=1/pic.diagonal;
    lMax:=0;
    while (scale[lMax]<4) and (amplitude[lMax]>1E-3) do begin
      aid:=aid+amplitude[lMax];
      inc(lMax);
      setLength(scale,lMax+1);
      setLength(amplitude,lMax+1);
      scale    [lMax]:=scale    [lMax-1]/scaleFactor;
      amplitude[lMax]:=amplitude[lMax-1]*amplitudeFactor;
    end;
    setLength(perlinLine,lMax);


    for l:=0 to lMax-1 do amplitude[l]:=amplitude[l]*2/aid;
    for y:=0 to yRes-1 do begin
      for l:=0 to lMax-1 do updatePerlinLine((y-yRes*0.5)*scale[L],L,amplitude[L]);
      for x:=0 to xRes-1 do begin
        aid:=0.5;
        for l:=0 to lMax-1 do aid:=aid+getSmoothValue((x-xRes*0.5)*scale[L],L);
        if aid>1 then aid:=1
        else if aid<0 then aid:=0;
        pic[x,y]:=aid*white;
      end;
    end;
    setLength(perlinLine,0);
    setLength(scale,0);
    setLength(amplitude,0);
  end;


PROCEDURE rotateImage(angle:single);
  VAR xRes,yRes,x,y,tx,ty:longint;
      s,c,ox,oy,wx,wy,
      fx,fy:single;
      pt,po:P_floatColor;
      aid:T_FloatMap;
      loc:array[0..1,0..1] of T_floatColor;
  begin
    xRes:=pic.width;
    yRes:=pic.height;
    angle:=angle/180*pi;
    s:=sin(angle);
    c:=cos(angle);
    ox:=xRes/2;
    oy:=yRes/2;
    aid.createCopy(pic);
    pt:=pic.rawData;
    po:=aid.rawData;
    for y:=0 to yRes-1 do begin fy:=y-oy;
      for x:=0 to xRes-1 do begin
        fx:=x-ox;
        wx:=c*fx-s*fy+ox;
        wy:=c*fy+s*fx+oy;
        tx:=floor(wx); wx:=wx-tx;
        ty:=floor(wy); wy:=wy-ty;
        if (tx>= 0) and (tx<xRes-1) and (ty>= 0) and (ty<yRes-1) then begin
          loc[0,0]:=po[tx+   ty   *xRes];
          loc[1,0]:=po[tx+1+ ty   *xRes];
          loc[0,1]:=po[tx+  (ty+1)*xRes];
          loc[1,1]:=po[tx+1+(ty+1)*xRes];
        end else begin
          if (tx>= 0) and (tx<xRes  ) and (ty>= 0) and (ty<yRes  ) then loc[0,0]:=po[tx+   ty   *xRes] else loc[0,0]:=pt[x+y*xRes];
          if (tx>=-1) and (tx<xRes-1) and (ty>= 0) and (ty<yRes  ) then loc[1,0]:=po[tx+1+ ty   *xRes] else loc[1,0]:=pt[x+y*xRes];
          if (tx>= 0) and (tx<xRes  ) and (ty>=-1) and (ty<yRes-1) then loc[0,1]:=po[tx+  (ty+1)*xRes] else loc[0,1]:=pt[x+y*xRes];
          if (tx>=-1) and (tx<xRes-1) and (ty>=-1) and (ty<yRes-1) then loc[1,1]:=po[tx+1+(ty+1)*xRes] else loc[1,1]:=pt[x+y*xRes];
        end;
        pt[x+y*xRes]:=(loc[0,0]*(1-wx)+loc[1,0]*wx)*(1-wy)+
                      (loc[0,1]*(1-wx)+loc[1,1]*wx)*   wy;
      end;
    end;
    aid.destroy;
  end;

PROCEDURE mpl(aim_megapixels:single);
  VAR factor:single;
  begin
    factor:=1E6*aim_megapixels/pic.size;
    if factor<1 then begin
      factor:=sqrt(factor);
      pic.resize(round(pic.width*factor),round(pic.height*factor));
    end;
  end;

PROCEDURE periodize(fraction:single);
  VAR temp:T_FloatMap;
      x,y,nx,ny,dx,dy:longint;
      wx,wy:single;
  begin
    nx:=round(pic.width *(0.5*fraction)); dx:=pic.width -nx-1;
    ny:=round(pic.height*(0.5*fraction)); dy:=pic.height-ny-1;
    temp.createCopy(pic);
    pic.resizeDat(dx+1,dy+1);
    for y:=0 to ny-1 do begin
      wy:=0.5-0.5*cos(y/ny*pi);
      for x:=0 to nx-1 do begin
        wx:=0.5-0.5*cos(x/nx*pi);
        pic[x,y]:=(temp[x,y   ]*(wx)+temp[x+dx,y   ]*(1-wx))*(  wy)+
                  (temp[x,y+dy]*(wx)+temp[x+dx,y+dy]*(1-wx))*(1-wy);
      end;
      for x:=nx to dx do  pic[x,y]:=temp[x,y]*(wy)+temp[x,y+dy]*(1-wy);
    end;
    for y:=ny to dy do begin
      for x:=0 to nx-1 do begin
        wx:=0.5-0.5*cos(x/nx*pi);
        pic[x,y]:=(temp[x,y   ]*(wx)+temp[x+dx,y   ]*(1-wx));
      end;
      for x:=nx to dx  do pic[x,y]:= temp[x,y];
    end;
    temp.destroy;
  end;

PROCEDURE enlargeImage(CONST newXRes,newYRes:longint; CONST bgR,bgG,bgB:single);
  VAR temp:T_FloatMap;
      pt:P_floatColor;
      i,j,offI,offJ:longint;
  begin
    temp.create(newXRes,newYRes);
    pt:=temp.rawData;
    for i:=0 to temp.size-1 do pt[i]:=newColor(bgR,bgG,bgB);
    offI:=(pic.width -temp.width ) div 2;
    offJ:=(pic.height-temp.height) div 2;
    for j:=0 to temp.height-1 do
    if (j+offJ>=0) and (j+offJ<pic.height) then
    for i:=0 to temp.width-1 do
    if (i+offI>=0) and (i+offI<pic.width) then temp[i,j]:=pic[i+offI,j+offJ];
    pic.destroy;
    pic.createCopy(temp);
    temp.destroy;
  end;

PROCEDURE parseCommandLine;
  CONST C_command:array[0..23] of T_commandAbstraction=
  ((isFile:false; leadingSign:'-'; cmdString:'verbose'; paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'show';    paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'show';    paramCount:1),
   (isFile:true;  leadingSign:' '; cmdString:'';        paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'reload';  paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'';        paramCount:2), //resize
   (isFile:false; leadingSign:'-'; cmdString:'fit';     paramCount:2),
   (isFile:false; leadingSign:'-'; cmdString:'cr';      paramCount:2),
   (isFile:false; leadingSign:'-'; cmdString:'crop';    paramCount:4),
   (isFile:false; leadingSign:'-'; cmdString:'quality'; paramCount:1),
   (isFile:false; leadingSign:'-'; cmdString:'PERLIN';  paramCount:3),
   (isFile:false; leadingSign:'-'; cmdString:'flip';    paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'flop';    paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'shine';   paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'NOISE';   paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'rot';     paramCount:1),
   (isFile:false; leadingSign:'-'; cmdString:'rotL';    paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'rotR';    paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'GRAD';    paramCount:7),
   (isFile:false; leadingSign:'-'; cmdString:'GRAD';    paramCount:3),
   (isFile:false; leadingSign:'-'; cmdString:'enlarge'; paramCount:5),
   (isFile:false; leadingSign:'-'; cmdString:'sizeLimit'; paramCount:0),
   (isFile:false; leadingSign:'-'; cmdString:'sizeLimit'; paramCount:1),
   (isFile:false; leadingSign:'-'; cmdString:'erasure'; paramCount:0)); //23
  FUNCTION sizeToInt(s:string):longint;
    begin
      s:=trim(s);
      case s[length(s)] of
        'k','K': begin result:=strToInt(copy(s,1,length(s)-1)) shl 10; if verboseMode then write('size limit set to ',strToInt(copy(s,1,length(s)-1)),'kB'); end;
        'm','M': begin result:=strToInt(copy(s,1,length(s)-1)) shl 20; if verboseMode then write('size limit set to ',strToInt(copy(s,1,length(s)-1)),'MB'); end;
        'g','G': begin result:=strToInt(copy(s,1,length(s)-1)) shl 30; if verboseMode then write('size limit set to ',strToInt(copy(s,1,length(s)-1)),'GB'); end;
        else     begin result:=strToInt(copy(s,1,length(s)-1));        if verboseMode then write('size limit set to ',strToInt(copy(s,1,length(s)-1)),'B');  end;
      end;
    end;

  VAR i,cIdx:longint;
      ep:T_extendedParameter;
  begin
    verboseMode:=gotParam(C_command[0]);
    for i:=1 to paramCount do begin
      stepStart:=now;
      ep:=extendedParam(i);
      if verboseMode then write('  im: ',paramStr(i));
      cIdx:=matchingCmdIndex(ep,C_command);
      case cIdx of
        0: begin end;
        1: displayImage(pic,-10);
        2: displayImage(pic,max(0,round(ep.floatParam[0]*1000)));
        3: if not(inputReady) then begin
             lastImage:=paramStr(i);
             try pic.create(paramStr(i)); if firstImage='' then firstImage:=paramStr(i); inputReady:=true; except halt end;
             if verboseMode then write(' loaded');
           end else begin
             lastImage:=paramStr(i);
             if sizeLimit=maxLongint then pic.saveToFile(paramStr(i))
             else if sizeLimit=0     then begin writeln; pic.saveSizeLimitedJpg(paramStr(i)); write('      '); end
                                     else begin writeln; pic.saveSizeLimitedJpg(paramStr(i),sizeLimit); write('      '); end;
             if verboseMode then write(' saved');
           end;
        4: if firstImage<>'' then begin pic.loadFromFile(firstImage); end else halt;
        5: if inputReady then begin
             pic.resize(ep.intParam[0],ep.intParam[1]);
           end else begin
             pic.create(ep.intParam[0],ep.intParam[1]);
             inputReady:=true;
           end;
        6: if inputReady then begin
             pic.resize(ep.intParam[0],ep.intParam[1],1);
           end else begin beep; halt; end;
        7: if inputReady then begin
             pic.cropResize(ep.intParam[0],ep.intParam[1]);
           end else begin beep; halt; end;
        8: if inputReady then begin
             pic.crop(ep.intParam[0],ep.intParam[1],ep.intParam[2],ep.intParam[3]);
           end else begin beep; halt; end;
        9: compressionQualityPercentage:=ep.intParam[0];
       10: generatePerlinNoise(ep.floatParam[0],ep.floatParam[1],ep.intParam[2]);
       11: pic.flip;
       12: pic.flop;
       13: shineImage(pic);
       14: generateNoise;
       15: rotateImage(ep.floatParam[0]);
       16: pic.rotLeft;
       17: pic.rotRight;
       18: generateGradient(ep.floatParam[0],ep.floatParam[1],ep.floatParam[2],ep.floatParam[3],ep.floatParam[4],ep.floatParam[5],ep.floatParam[6]);
       19: generateGradient(ep.floatParam[0],ep.floatParam[0],ep.floatParam[0],ep.floatParam[1],ep.floatParam[1],ep.floatParam[1],ep.floatParam[2]);
       20: enlargeImage(ep.intParam[0],ep.intParam[1],ep.floatParam[2],ep.floatParam[3],ep.floatParam[4]);
       21: begin sizeLimit:=0; if verboseMode then write('Automatic size limit set'); end;
       22: sizeLimit:=sizeToInt(ep.stringSuffix);
       23: erasure;
      else begin
             if not(colorManipulate(paramStr(i),pic)) and
                not(filterImage    (paramStr(i),pic)) and
                not(combineImage   (paramStr(i),pic)) then begin
                  if verboseMode
                    then write(' UNKNOWN COMMAND!')
                    else write('im: UNKNOWN COMMAND: ',paramStr(i));
                  beep;
                  readln;
                  halt;
                end;
           end;
      end;
      if verboseMode then writeln('  ',myTimeToStr(now-stepStart));
    end; //for i
    if inputReady then pic.destroy;
    if paramCount=0 then displayHelp;
    if verboseMode then writeln('  im: DONE ',myTimeToStr(now-progStart));
  end;

begin
  DefaultFormatSettings.DecimalSeparator:='.';
  progStart:=now;
  parseCommandLine;
end.

