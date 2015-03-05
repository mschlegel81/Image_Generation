PROGRAM superpose;
USES mypics,sysutils,gl,glut,windows;
VAR xres,yres:longint;
    pic:T_24BitImage;
    state:record
      p,v:array[0..2,0..1] of single;
      hue:byte;
    end;

PROCEDURE initState;
  VAR i:longint;
  begin
    randomize;
    with state do begin
      for i:=0 to 2 do p[i,0]:=random(xRes);
      for i:=0 to 2 do p[i,1]:=random(yRes);
      for i:=0 to 2 do v[i,0]:=(random-0.5);
      for i:=0 to 2 do v[i,1]:=(random-0.5);
    end;

  end;

PROCEDURE render;
  VAR amp:single;
      baseCol:T_floatColor;
      x,y,k:longint;
      col:T_24Bit;
      pt:P_24Bit;
  begin
    pt:=pic.rawData;
    k:=0;
    for y:=0 to yres-1 do
    for x:=0 to xres-1 do begin
      baseCol:=hue(state.hue/256);
      with state do
      amp:=(sin(0.6*sqrt(sqr(x-p[0,0])+sqr(y-p[0,1])))
           +sin(0.6*sqrt(sqr(x-p[1,0])+sqr(y-p[1,1])))
           +sin(0.6*sqrt(sqr(x-p[2,0])+sqr(y-p[2,1]))))*42.5+127.5;
      col[0]:=round(amp*baseCol[0]);
      col[1]:=round(amp*baseCol[1]);
      col[2]:=round(amp*baseCol[2]);
      pt[k]:=col; inc(k);
    end;
    glTexImage2D (GL_TEXTURE_2D,0,GL_RGB,xRes,yRes,0,GL_RGB,GL_UNSIGNED_BYTE,pic.rawData);
  end;

PROCEDURE draw; cdecl;
  begin
    glClear(GL_COLOR_BUFFER_BIT);
    glEnable(GL_TEXTURE_2D);
    glBegin (GL_QUADS);
      glTexCoord2f(0.0, 0.0); glVertex2f(  0,0);
      glTexCoord2f(1.0, 0.0); glVertex2f(1,0);
      glTexCoord2f(1.0, 1.0); glVertex2f(1,1);
      glTexCoord2f(0.0, 1.0); glVertex2f(0,1);
    glEnd();
    glutSwapBuffers();
  end;

PROCEDURE idleFunc; cdecl;
  VAR i,j:longint;
  begin
    with state do begin
      for i:=0 to 2 do for j:=0 to 1 do p[i,j]:=p[i,j]+v[i,j];
      for i:=0 to 2 do begin
      if (p[i,0]<0) and (v[i,0]<0) or (p[i,0]>=xRes) and (v[i,0]>0) then begin
        v[i,0]:=-v[i,0];
        v[i,1]:=v[i,1]+0.5-random;
      end;
      if (p[i,1]<0) and (v[i,1]<0) or (p[i,1]>=yRes) and (v[i,1]>0) then begin
        v[i,1]:=-v[i,1];
        v[i,0]:=v[i,0]+0.5-random;
      end;
      end;
      inc(hue);
    end;
//    sleep(10);
    render;
    glutpostredisplay;
  end;

PROCEDURE keyboard(key:byte; x,y:longint); cdecl;
  begin
    halt;
  end;

begin

  DefaultFormatSettings.DecimalSeparator:='.';
  xRes:=GetSystemMetrics(SM_CXSCREEN);
  yRes:=GetSystemMetrics(SM_CYSCREEN);
  pic.create(xres,yres);
  initState;
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
  glutIdleFunc(@idlefunc);
//  glutReshapeFunc(@reshape);
  glutKeyboardFunc(@keyboard);

  glPixelStorei(GL_UNPACK_ALIGNMENT,1);
  glPixelStorei(GL_UNPACK_LSB_FIRST ,GL_True);
  glutfullscreen;
  glutMainLoop();

end.