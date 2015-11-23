UNIT displayUtil;
INTERFACE
USES gl,glut,sysutils,mypics{$ifndef UNIX},windows{$endif};
PROCEDURE displayImage(VAR img:T_FloatMap; delayInMilliseconds:longint);

IMPLEMENTATION
VAR xRes,yRes:longint;
    imgW,imgH:longint;
    fullscreenmode:boolean=false;
    glutIsUpAndRunning:boolean=false;
    picToDisplay:T_FloatMap;
    waitingForInput:boolean=false;

PROCEDURE reshape(new_xres,new_yres:longint); cdecl;
  begin
    xRes:=new_xres;
    yRes:=new_yres;
    glViewport(0, 0,xRes,yRes);
    glLoadIdentity;
    glOrtho(0,xRes,0,yRes,-10,10);
    glMatrixMode(GL_MODELVIEW);
    glutPostRedisplay();
  end;

PROCEDURE keyboard(key:byte; x,y:longint); cdecl;
  VAR fullX,fullY:longint;
  begin
    if key=27 then begin
      if fullscreenmode then begin
        fullX:=xRes;
        fullY:=yRes;
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
        fullX:=xRes;
        fullY:=yRes;
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
    glTexImage2D (GL_TEXTURE_2D,0,GL_RGB,imgW,imgH,0,GL_RGB,GL_Float,picToDisplay.rawData);
    if xRes*imgH>imgW*yRes then factor:=yRes/imgH
                           else factor:=xRes/imgW;

    restX:=(xRes-round(imgW*factor)) shr 1;
    restY:=(yRes-round(imgH*factor)) shr 1;
    glClear(GL_COLOR_BUFFER_BIT);
    glBegin (gl_quads);
      glColor3f(0  ,0  ,0  ); glVertex2f(0                ,0);
      glColor3f(0.5,0.5,0.5); glVertex2f(restX            ,restY);
                              glVertex2f(restX            ,restY+imgH*factor);
      glColor3f(0  ,0  ,0  ); glVertex2f(0                ,yRes);
                              glVertex2f(0                ,yRes      );
      glColor3f(0.5,0.5,0.5); glVertex2f(restX            ,restY+imgH*factor);
                              glVertex2f(restX+imgW*factor,restY+imgH*factor);
      glColor3f(0  ,0  ,0  ); glVertex2f(xRes             ,yRes);
                              glVertex2f(xRes             ,yRes      );
      glColor3f(0.5,0.5,0.5); glVertex2f(restX+imgW*factor,restY+imgH*factor);
                              glVertex2f(restX+imgW*factor,restY     );
      glColor3f(0  ,0  ,0  ); glVertex2f(xRes             ,0         );
                              glVertex2f(xRes             ,0         );
      glColor3f(0.5,0.5,0.5); glVertex2f(restX+imgW*factor,restY     );
                              glVertex2f(restX            ,restY     );
      glColor3f(0  ,0  ,0  ); glVertex2f(0                ,0);
    glEnd();
    glColor3f(1,1,1);
    glEnable(GL_TEXTURE_2D);
    glBegin (gl_quads);
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
    glutInitWindowSize(xRes,yRes);
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

PROCEDURE displayImage(VAR img:T_FloatMap; delayInMilliseconds:longint);
  begin
    picToDisplay.destroy;
    picToDisplay.createCopy(img);
    if (picToDisplay.width>xRes) or (picToDisplay.height>yRes) then picToDisplay.resize(xRes,yRes,1);
    imgW:=picToDisplay.width;
    imgH:=picToDisplay.height;
    if not(glutIsUpAndRunning) then begin
      xRes:=imgW;
      yRes:=imgH;
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
