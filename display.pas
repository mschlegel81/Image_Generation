PROGRAM display;
USES {$ifdef UNIX}cmem,cthreads,{$endif}gl,glut,sysutils,mypics{$ifndef UNIX},windows{$endif},uniqueinstanceraw,cmdLineParseUtil;

TYPE T_picInfo=record
       pic:T_24BitImage;
       originalH,originalW:longint;
       loaded:boolean;
       displayOrder:longint;
       filename,resString:string;
       filetime:longint;
     end;

VAR maxCache:longint=1000; //cache size in megabytes
    animSleep:longint=50;  //sleep time in milliseconds
    xres,yres:longint;
    imgW,imgH:longint;
    listOfImages:array of T_picInfo;
    displayCounter:longint=0;
    positionInList:longint=0;
    fullscreenmode:boolean=false;
    animationMode :boolean=false;
    reverseAnim   :boolean=false;
    enlargeSmall  :boolean=false;
    repeatSmall   :boolean=false;
    precacheAll   :boolean=false;
    currentlyDisplayedFile:string;
    currentlyDisplayedFileTime:longint;
    
    lastDisplay:double;
    fpsMeasure:record
      count:longint;
      countSince:double;
      txt:string;
    end;

FUNCTION prepareImage(index:longint):boolean;
  VAR i:longint;
  begin
    if length(listOfImages)>0 then begin
      result:=true;

      while index< 0                    do inc(index,length(listOfImages));
      while index>=length(listOfImages) do dec(index,length(listOfImages));
      with listOfImages[index] do begin
        if not(loaded) then begin
          if fileExists(filename) then begin
            pic.create(filename);
            filetime:=fileage(filename);
            resString:=intToStr(pic.width)+'x'+intToStr(pic.height);
            originalH:=pic.height;
            originalW:=pic.width;
            if (pic.width>xres) or (pic.height>yres) then pic.resize(xres,yres,1);
            loaded:=true;
          end else begin
            for i:=index to length(listOfImages)-2 do listOfImages[i]:=listOfImages[i+1];
            setLength(listOfImages,length(listOfImages)-1);
            result:=false;
          end;
        end;
      end;
    end else result:=false;
  end;

PROCEDURE disposeImage(index:longint);
  begin
    if length(listOfImages)>0 then begin
      while index< 0                    do inc(index,length(listOfImages));
      while index>=length(listOfImages) do dec(index,length(listOfImages));
      with listOfImages[index] do if loaded then begin

        pic.destroy;
        loaded:=false;
      end;
    end;
  end;

FUNCTION cleanup:boolean;
  VAR i,id,lc:longint;
      cachedPixels,cachedMegabytes:longint;
  begin
    result:=false;
    repeat
      lc:=0;
      cachedPixels:=0;
      id:=-1;
      for i:=0 to length(listOfImages)-1 do if listOfImages[i].loaded then begin
        cachedPixels:=cachedPixels+listOfImages[i].pic.size;
        inc(lc);
        if (id=-1) or (listOfImages[i].displayOrder<listOfImages[id].displayOrder) then id:=i;
      end;
      cachedMegabytes:=(cachedPixels shr 20)*3;
      if cachedMegabytes>maxCache then begin disposeImage(id); result:=true; end;
    until (cachedMegabytes<=maxCache) or (lc<=1);
  end;

PROCEDURE displayImage(newPos:longint);
  VAR title:shortstring;
  begin
    repeat
      if newPos>=length(listOfImages) then newPos:=0;
      if newPos<0 then newPos:=length(listOfImages)-1;
      positionInList:=newPos;
    until prepareImage(newPos) or (length(listOfImages)=0);
    if (newPos>=0) and (newPos<length(listOfImages)) then with listOfImages[newPos] do begin
      inc(displayCounter);
      displayOrder:=displayCounter;
      if (pic.width<xres) and (pic.height<yres) and ((originalW>=xres) or (originalH>=yres)) or (fileage(filename)<>filetime) then begin
        try
          pic.loadFromFile(filename);
          filetime:=fileage(filename);        
        except
          filetime:=0;
          pic.destroy;
          pic.create(1,1);
        end;
      end;
      if (pic.width>xres) or (pic.height>yres) then pic.resize(xres,yres,1);
      imgW:=pic.width;
      imgH:=pic.height;
      currentlyDisplayedFile:=filename;
      currentlyDisplayedFileTime:=filetime;
      glTexImage2D (GL_TEXTURE_2D,0,GL_RGB,imgW,imgH,0,GL_RGB,GL_UNSIGNED_BYTE,pic.rawData);
      title:=listOfImages[positionInList].filename+'  @'+listOfImages[positionInList].resString;
      if animationmode then title:=title+' '+fpsMeasure.txt+#0
                       else title:=title+#0;
      glutSetWindowTitle(@title[1]);
    end;
    cleanup;
    glutpostredisplay;
  end;

FUNCTION appList(relevantName:string; triggerDisplay:boolean):longint;
  FUNCTION alreadyEnlisted(filename:string):boolean;
    VAR i:longint;
    begin
      i:=0;
      while (i<length(listOfImages)) and (listOfImages[i].filename<>filename) do inc(i);
      result:=i<length(listOfImages);
    end;

{$NOTE Modify this function if you want to open other file types as well.}
  FUNCTION isImage(s:string):boolean;
    begin
      s:=uppercase(s);
      result:=(s='.BMP')  or (s='.JPG')
           or (s='.PNG')  or (s='.ICO')
           or (s='.VRAW') or (s='.GIF')
           or (s='.CSV')  or (s='.JP2');
    end;

  VAR info   :TSearchRec;
      oneFile:boolean;
      i      :longint;
  begin
    result:=0;
    oneFile:=(pos('*',relevantName)=0) and (pos('?',relevantName)=0);
    if findFirst(relevantName,faAnyFile,info)=0 then repeat
      if ((info.attr and faDirectory)<>faDirectory)
      and isImage(ExtractFileExt(info.name)) and not(alreadyEnlisted(ExtractFilePath(relevantName)+info.name)) then begin
        setLength(listOfImages,length(listOfImages)+1);
        i:=length(listOfImages)-2;
        while (i>=0) and (listOfImages[i].filename>ExtractFilePath(relevantName)+info.name) do begin
          listOfImages[i+1]:=listOfImages[i];
          dec(i);
        end;
        with listOfImages[i+1] do begin
          filename  :=ExtractFilePath(relevantName)+info.name;
          resString :='';
          loaded    :=false;
        end;
        if oneFile then begin
          if triggerDisplay then displayImage(i+1)
                            else positionInList:=i+1;
        end;
        result:=i+1;
      end;
    until (findNext(info)<>0);
    sysutils.findClose(info);
  end;

PROCEDURE clearlist;
  VAR i:longint;
  begin
    for i:=0 to length(listOfImages)-1 do with listOfImages[i] do if loaded then begin
      pic.destroy;
      loaded:=false;
    end;
    setLength(listOfImages,0);
  end;

FUNCTION indexOfEntry(name:string):longint;
  begin
    result:=0;
    while (result<length(listOfImages)) and (listOfImages[result].filename<>name) do inc(result);
    if result>=length(listOfImages) then result:=-1;
  end;

PROCEDURE reshape(new_xres,new_yres:longint); cdecl;
  begin
    xres:=new_xres;
    yres:=new_yres;
    glViewport(0, 0,xres,yres);
    glLoadIdentity;
    glOrtho(0,xres,0,yres,-10,10);
    glMatrixMode(GL_MODELVIEW);
    glutPostRedisplay();
  end;

PROCEDURE draw; cdecl;
  VAR restX,restY:longint;
      factor:single;
  PROCEDURE drawString(x,y:longint; txt:string);
    VAR i:longint;
    begin
      glRasterpos2f(x,y);
      for i:=1 to length(txt) do glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12,ord(txt[i]));
    end;

  begin
    inc(fpsMeasure.count);

    if (imgW>xres) or (imgH>yres) or ((positioninList>=0) and ((imgW=0) or (imgH=0)) or (fileage(currentlyDisplayedFile)<>currentlyDisplayedFileTime)) then displayImage(positionInList);
    lastDisplay:=now;
    if enlargeSmall then begin
      if xres*imgH>imgW*yres then factor:=yres/imgH
                             else factor:=xres/imgW;

      restX:=(xRes-round(imgW*factor)) shr 1;
      restY:=(yres-round(imgH*factor)) shr 1;
      glClear(GL_COLOR_BUFFER_BIT);
      glBegin (GL_QUADS);
        glColor3f(0  ,0  ,0  ); glVertex2f(0                ,0);
        glColor3f(0.5,0.5,0.5); glVertex2f(restX            ,restY);
                                glVertex2f(restX            ,restY+imgH*factor);
        glColor3f(0  ,0  ,0  ); glVertex2f(0                ,yres);
                                glVertex2f(0                ,yres      );
        glColor3f(0.5,0.5,0.5); glVertex2f(restX            ,restY+imgH*factor);
                                glVertex2f(restX+imgW*factor,restY+imgH*factor);
        glColor3f(0  ,0  ,0  ); glVertex2f(xres             ,yres);
                                glVertex2f(xres             ,yres      );
        glColor3f(0.5,0.5,0.5); glVertex2f(restX+imgW*factor,restY+imgH*factor);
                                glVertex2f(restX+imgW*factor,restY     );
        glColor3f(0  ,0  ,0  ); glVertex2f(xres             ,0         );
                                glVertex2f(xres             ,0         );
        glColor3f(0.5,0.5,0.5); glVertex2f(restX+imgW*factor,restY     );
                                glVertex2f(restX            ,restY     );
        glColor3f(0  ,0  ,0  ); glVertex2f(0                ,0);
      glEnd();
      glColor3f(1,1,1);
      glEnable(GL_TEXTURE_2D);
      glBegin (GL_QUADS);
        glTexCoord2f(0.0, 0.0); glVertex2f(restX+          0,restY+   0);
        glTexCoord2f(1.0, 0.0); glVertex2f(restX+imgW*factor,restY+   0);
        glTexCoord2f(1.0, 1.0); glVertex2f(restX+imgW*factor,restY+imgH*factor);
        glTexCoord2f(0.0, 1.0); glVertex2f(restX+          0,restY+imgH*factor);
      glEnd();
      glDisable (GL_TEXTURE_2D);
    end else if repeatSmall then begin
      glColor3f(1,1,1);
      glEnable(GL_TEXTURE_2D);
      glBegin (GL_QUADS);
        glTexCoord2f(0.0      , 0.0); glVertex2f(0   ,0);
        glTexCoord2f(xres/imgW, 0.0); glVertex2f(xres,0);
        glTexCoord2f(xres/imgW, yres/imgH); glVertex2f(xres,yres);
        glTexCoord2f(0.0      , yres/imgH); glVertex2f(0   ,yres);
      glEnd();
      glDisable (GL_TEXTURE_2D);
    end else begin
      restX:=(xRes-imgW) shr 1;
      restY:=(yres-imgH) shr 1;
      glClear(GL_COLOR_BUFFER_BIT);
      glBegin (GL_QUADS);
        glColor3f(0  ,0  ,0  ); glVertex2f(0    ,0);
        glColor3f(0.5,0.5,0.5); glVertex2f(restX,restY);
                                glVertex2f(restX,restY+imgH);
        glColor3f(0  ,0  ,0  ); glVertex2f(0    ,yres);
                                glVertex2f(0         ,yres      );
        glColor3f(0.5,0.5,0.5); glVertex2f(restX     ,restY+imgH);
                                glVertex2f(restX+imgW,restY+imgH);
        glColor3f(0  ,0  ,0  ); glVertex2f(xres      ,yres);
                                glVertex2f(xres      ,yres      );
        glColor3f(0.5,0.5,0.5); glVertex2f(restX+imgW,restY+imgH);
                                glVertex2f(restX+imgW,restY     );
        glColor3f(0  ,0  ,0  ); glVertex2f(xres      ,0         );
                                glVertex2f(xres      ,0         );
        glColor3f(0.5,0.5,0.5); glVertex2f(restX+imgW,restY     );
                                glVertex2f(restX     ,restY     );
        glColor3f(0  ,0  ,0  ); glVertex2f(0         ,0);
      glEnd();
      glColor3f(1,1,1);
      glEnable(GL_TEXTURE_2D);
      glBegin (GL_QUADS);
        glTexCoord2f(0.0, 0.0); glVertex2f(restX+   0,restY+   0);
        glTexCoord2f(1.0, 0.0); glVertex2f(restX+imgW,restY+   0);
        glTexCoord2f(1.0, 1.0); glVertex2f(restX+imgW,restY+imgH);
        glTexCoord2f(0.0, 1.0); glVertex2f(restX+   0,restY+imgH);
      glEnd();
      glDisable (GL_TEXTURE_2D);
    end;


    if fullscreenmode then begin
      if animationmode then drawString(3,35,fpsMeasure.txt);
      if (positioninlist>=0) and (positionInList<length(listOfImages)) then drawString(3,20,listOfImages[positionInList].filename);
      if (positioninlist>=0) and (positionInList<length(listOfImages)) then drawString(3, 5,listOfImages[positionInList].resString);
    end;
    glutSwapBuffers();
  end;

PROCEDURE doPrecache;
  VAR i:longint;
  begin
    i:=0;
    while precacheAll and (i<length(listOfImages)) and not(cleanup) do begin
      prepareImage(i);
      inc(i);
    end;
  end;

PROCEDURE parseParameter(s:string; triggerDisplay:boolean);
  CONST cmdList:array[0..9] of   T_commandAbstraction=(
   (isFile:true;  leadingSign:' '; cmdString:'';        paramCount:-1),  // 0
   (isFile:false; leadingSign:'-'; cmdString:'animate'; paramCount: 1),  // 1
   (isFile:false; leadingSign:'-'; cmdString:'stop';    paramCount: 0),  // 2
   (isFile:false; leadingSign:'-'; cmdString:'enlarge'; paramCount: 0),  // 3
   (isFile:false; leadingSign:'-'; cmdString:'kill';    paramCount: 0),  // 4
   (isFile:false; leadingSign:'-'; cmdString:'only';    paramCount: 0),  // 5
   (isFile:false; leadingSign:'-'; cmdString:'repeat';  paramCount: 0),  // 6
   (isFile:false; leadingSign:'-'; cmdString:'norepeat';paramCount: 0),  // 7
   (isFile:false; leadingSign:'-'; cmdString:'precache';paramCount: 0),  // 8
   (isFile:false; leadingSign:'-'; cmdString:'cache';   paramCount: 1)); // 9

  VAR newIdx:longint;
      ep:T_extendedParameter;
  begin
    if copy(s,1,1)='*' then with ep do begin
      isFile:=true;
      leadingSign:=' ';
      cmdString:=s;
    end else ep:=extendedParam(s);
    case matchingCmdIndex(ep,cmdList) of
      0: begin newIdx:=indexOfEntry(s); if newIdx>=0 then disposeImage(newIdx) else appList(expandFileName(s),triggerDisplay); end;
      1: begin animationMode:=true; animSleep:=ep.intParam[0]; end;
      2: animationMode:=false;
      3: enlargeSmall:=true;
      4: halt;
      5: clearList;
      6: repeatSmall:=true;
      7: repeatSmall:=false;
      8: precacheAll:=true;
      9: maxCache:=ep.intParam[0];
    else if not(triggerDisplay) then begin
          writeln('DISPLAY - v1.2 by Martin Schlegel');
          writeln('  usage: display [options] [filenames] [-h]');
          writeln('  options: -animate##   do animation with a ##millisecond sleep between frames');
          writeln('           -cache##     cache up to ##MB data');
          writeln('           -enlarge     enlarge small images');
          writeln('           -kill        kill running instance');
          writeln('           -only        clear list of images');
          writeln('           -repeat      repeat small images');
          writeln('           -norepeat    do not repeat small images');
          writeln('           -precache    precache as many images as possible');
          writeln('           -help        display this help');
        end;
    end;
  end;

FUNCTION wantHelp:boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=1 to paramcount do result:=result or (copy(paramstr(i),1,2)='-h');
  end;

PROCEDURE idleFunc; cdecl;
  VAR i:longint;
      caughtName,subName:string;
      gotAMessage:boolean;
  begin
    try
      gotAMessage:=FIPCServer.peekMessage(10,true);
    except
      gotAMessage:=false;
    end;
    if gotAMessage then begin
      caughtName:=FIPCServer.stringMessage;
      while length(caughtName)>0 do begin
        subName   :=copy(caughtName,1,pos('|',caughtName)-1);
        caughtName:=copy(caughtName,  pos('|',caughtName)+1,length(caughtName));
        parseParameter(subname,true);
      end;
      doPrecache;
      glutpostredisplay;
    end else begin
      if animationMode then begin
        if reverseAnim then displayImage(positionInList-1)
                       else displayImage(positionInList+1);
        i:=animSleep-round(24*60*60*1000*(now-lastDisplay));
        if i>0 then sleep(i);

        if (now-fpsMeasure.countSince)*24*60*60>1 then begin
          fpsMeasure.txt:=formatFloat('00.00',fpsMeasure.count/((now-fpsMeasure.countSince)*24*60*60))+'FPS';
          fpsMeasure.count:=0;
          fpsMeasure.countSince:=now;
        end;
        glutpostredisplay;
      end else begin
        sleep(100);
        if ((now-lastDisplay)*24*60*60>60) or (fileage(currentlyDisplayedFile)<>currentlyDisplayedFileTime) then
          glutPostRedisplay;
      end;
    end;
  end;

PROCEDURE keyboard(key:byte; x,y:longint); cdecl;
  VAR fullX,fullY:longint;
  begin
    if key=27 then begin
      if fullscreenMode then begin
        fullX:=xres;
        fullY:=yres;
        fullscreenmode:=not(fullscreenmode);
        if (imgW<fullX-5) and (imgH<fullY-50) and (imgW>100) and (imgH>100) then begin
          glutReshapeWindow (imgW,imgH);
          glutPositionWindow((fullX-imgW) shr 1,
                             (fullY-imgH) shr 1);
        end else begin
          glutReshapeWindow (fullX shr 1,
                             fullY shr 1);
          glutPositionWindow(fullX shr 2,
                             fullY shr 2);
        end;
      end else halt;
    end
    else if key=32 then displayImage(positionInList+1)
    else if key= 8 then begin displayImage(positionInList-1); if animationMode then reverseAnim:=not(reverseAnim); end
    else if key=13 then begin disposeImage(positionInList); displayImage(positionInList); end
    else if key in [ord('a'),ord('A')] then animationMode:=not(animationMode)
    else if key in [ord('f'),ord('F')] then begin
      fullscreenmode:=not(fullscreenmode);
      if fullscreenmode then begin
        glutfullscreen;
      end else begin
        fullX:=xres;
        fullY:=yres;
        if (imgW<fullX-5) and (imgH<fullY-50) and (imgW>100) and (imgH>100) then begin
          glutReshapeWindow (imgW,imgH);
          glutPositionWindow((fullX-imgW) shr 1,
                             (fullY-imgH) shr 1);
        end else begin
          glutReshapeWindow (fullX shr 1,
                             fullY shr 1);
          glutPositionWindow(fullX shr 2,
                             fullY shr 2);
        end;
      end;
    end;


  end;

begin
  if not(InstanceRunning('display',true)) then begin
    DefaultFormatSettings.DecimalSeparator:='.';
    setLength(listOfImages,0);
    positionInList:=0;
    for xRes:=1 to paramCount do parseParameter(paramstr(xres),false);
    if length(listOfImages)=0 then appList(expandFileName('*.*'),false);

    fpsMeasure.count:=0;
    fpsMeasure.countSince:=now;
    lastDisplay:=now;

    xRes:={$ifdef UNIX}500;{$else}GetSystemMetrics(SM_CXSCREEN);{$endif}
    yRes:={$ifdef UNIX}500;{$else}GetSystemMetrics(SM_CYSCREEN)-50;{$endif}
    doPrecache;

    glutInit(@argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGB);
    glutInitWindowSize(xres,yres);
    glutCreateWindow('Display by M.S.');

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

    glClearColor(0.5, 0.5, 0.5, 0.0);
    glOrtho(0, 1, 0, 1, -10.0, 10.0);
    glMatrixMode(GL_MODELVIEW);
    glutDisplayFunc(@draw);
    glutIdleFunc(@idlefunc);
    glutReshapeFunc(@reshape);
    glutKeyboardFunc(@keyboard);

    glPixelStorei(GL_UNPACK_ALIGNMENT,1);
    glPixelStorei(GL_UNPACK_LSB_FIRST ,GL_True);
    //glutfullscreen;
    glutMainLoop();
  end else if wantHelp then parseParameter('-h',false);
end.
