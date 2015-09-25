UNIT displayUtil;
INTERFACE
USES gl,glut,sysutils,mypics{$ifndef UNIX},windows{$endif};
PROCEDURE displayImage(VAR img:T_floatMap; delayInMilliseconds:longint);

IMPLEMENTATION
VAR xres,yres:longint;
    imgW,imgH:longint;
    fullscreenMode:boolean=false;
    glutIsUpAndRunning:boolean=false;
    picToDisplay:T_FloatMap;
    waitingForInput:boolean=false;

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
      end else waitingForInput:=false;
    end
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
    glutPostRedisplay;
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
    glTexImage2D (GL_TEXTURE_2D,0,GL_RGB,imgW,imgH,0,GL_RGB,GL_FLOAT,picToDisplay.rawData);
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
    glutSwapBuffers();
  end;

FUNCTION initGlut(p:pointer):ptrint;
  begin
    glutInit(@argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE + GLUT_RGB);
    glutInitWindowSize(xres,yres);
    glutCreateWindow('');
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glClearColor(0.5, 0.5, 0.5, 0.0);
    glOrtho(0, 1, 0, 1, -10.0, 10.0);
    glMatrixMode(GL_MODELVIEW);
    glutDisplayFunc(@draw);
    glutReshapeFunc(@reshape);
    glutKeyboardFunc(@keyboard);
    glPixelStorei(GL_UNPACK_ALIGNMENT,1);
    glPixelStorei(GL_UNPACK_LSB_FIRST ,GL_True);
    glutPostRedisplay;
    glutMainLoop();
    result:=0;
  end;

PROCEDURE displayImage(VAR img:T_floatMap; delayInMilliseconds:longint);
  begin
    picToDisplay.destroy;
    picToDisplay.createCopy(img);
    if (picToDisplay.width>xres) or (picToDisplay.height>yres) then picToDisplay.resize(xres,yres,1);
    imgW:=picToDisplay.width;
    imgH:=picToDisplay.height;
    if not(glutIsUpAndRunning) then begin
      xres:=imgW;
      yres:=imgH;
      beginThread(@initGlut);
    end else glutPostRedisplay;
    glutIsUpAndRunning:=true;
    if delayInMilliseconds<=0 then begin
      waitingForInput:=true;
      while waitingForInput do sleep(10);
    end else sleep(delayInMilliseconds);
  end;

INITIALIZATION
  xRes:={$ifdef UNIX}500;{$else}GetSystemMetrics(SM_CXSCREEN);{$endif}
  yRes:={$ifdef UNIX}500;{$else}GetSystemMetrics(SM_CYSCREEN);{$endif}
  picToDisplay.create(1,1);
FINALIZATION
  while waitingForInput do sleep(10);
end.
