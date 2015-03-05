UNIT zbuf;
INTERFACE
{$fputype sse3}
USES {$ifdef UNIX}cmem,cthreads,{$endif}mypics,math;
TYPE
  T_baseType=single;
  T_zcol=record z:T_baseType; col:T_floatColor; end;
  P_zcol=^T_zcol;

  T_zbufferedMap=object
    private
      xres,yres:longint;
      data:P_zcol;

    public
      CONSTRUCTOR create(newWidth,newHeight:longint);
      CONSTRUCTOR createCopy(VAR original:T_zBufferedMap);
      DESTRUCTOR destroy;
      PROCEDURE clear(color:T_floatColor; z:T_baseType);
      PROCEDURE copyToFloatMap(OUT pic:T_Floatmap);
      PROCEDURE saveBitmap(filename:string);
      PROCEDURE saveBitmap(filename:string; distanceFalloff:single; fogColor:T_floatColor);
      PROCEDURE saveBitmap(z0,z1:single; filename:string; distanceFalloff:single; fogColor:T_floatColor);

      PROCEDURE incBitmap(VAR pic:T_FloatMap; distanceFalloff:single; fogColor:T_floatColor; project:boolean);
      FUNCTION width  :longint;
      FUNCTION height :longint;
      FUNCTION rawData:P_zCol;
  end;

  T_3DProjection=object
    a,invA:array[0..2,0..2] of T_baseType;
    screenCenterX,screenCenterY:T_baseType;
    screenWidth,screenHeight:longint;
    zoomFactor:T_baseType;
    eye:T_floatColor;
    CONSTRUCTOR create(eyepoint,lookat:T_floatColor; xres,yres:longint; viewAngle:single);
    PROCEDURE   reinit(eyepoint,lookat:T_floatColor; xres,yres:longint; viewAngle:single);
    PROCEDURE   reinit(eyepoint,lookat:T_floatColor; xres,yres:longint; viewAngle:single; subpixelshiftX,subpixelshiftY:single);
    DESTRUCTOR destroy;
    PROCEDURE realToScreen(p:T_floatColor; OUT screenX,screenY,screenZ:single);
    PROCEDURE screenToReal(OUT p:T_floatColor; screenX,screenY,screenZ:T_baseType);
    PROCEDURE screenToReal(OUT p:T_floatColor; screenX,screenY:T_baseType);
    PROCEDURE screenToRealLevel(OUT p:T_floatColor; screenX,screenY,levelZ:T_baseType);
    FUNCTION throwPixelToMap(position,color:T_floatColor; mapdata:P_zCol):boolean;
    FUNCTION throwPixelToMap(position,color:T_floatColor; mapdata:P_zCol; OUT sx,sy:single):boolean;
    PROCEDURE throwPixelToMap(position,color:T_floatColor; mapdata:P_zCol; OUT sx,sy:single; OUT onScreen,foreground:boolean);

    FUNCTION specFactor(position,normal,light:T_floatColor):single;
//    FUNCTION throwPixelToMap(position,color:T_floatColor; mapdata:P_zCol; OUT sx,sy,screenz:single):boolean;
  end;


IMPLEMENTATION
CONSTRUCTOR T_zbufferedMap.create(newWidth,newHeight:longint);
  begin
    xres:=newWidth;
    yres:=newHeight;
    getMem(data,sizeOf(T_zCol)*xres*yres);
  end;

CONSTRUCTOR T_zbufferedMap.createCopy(VAR original:T_zBufferedMap);
  begin
    xres:=original.width;
    yres:=original.height;
    getMem(data,sizeOf(T_zCol)*xres*yres);
    move(original.rawData^,data^,sizeOf(T_zCol)*xres*yres);
  end;


DESTRUCTOR T_zbufferedMap.destroy;
  begin
    freeMem(data,sizeOf(T_zCol)*xres*yres);
    xres:=0;
    yres:=0;
  end;

PROCEDURE T_zbufferedMap.clear(color:T_floatColor; z:T_baseType);
  VAR i:longint;
      val:T_zCol;
  begin
    val.col:=color;
    val.z  :=z;
    for i:=0 to xres*yres-1 do data[i]:=val;
  end;

PROCEDURE T_zbufferedMap.copyToFloatMap(OUT pic:T_Floatmap);
  VAR i:longint;
      pt:P_floatColor;
  begin
    pic.create(xres,yres);
    pt:=pic.rawData;
    for i:=0 to xres*yres-1 do pt[i]:=data[i].col;
  end;

PROCEDURE T_zbufferedMap.saveBitmap(filename:string);
  VAR i:longint;
      pt:P_floatColor;
      pic:T_Floatmap;
  begin
    pic.create(xres,yres);
    pt:=pic.rawData;
    for i:=0 to xres*yres-1 do pt[i]:=(data[i].col);
    pic.saveToFile(filename);
    pic.destroy;
  end;

PROCEDURE T_zbufferedMap.saveBitmap(filename:string; distanceFalloff:single; fogColor:T_floatColor);
  VAR i:longint;
      pt:P_floatColor;
      pic:T_Floatmap;
  begin
    pic.create(xres,yres);
    pt:=pic.rawData;
    for i:=0 to xres*yres-1 do pt[i]:=(fogColor+(data[i].col-fogColor)*exp(data[i].z*distanceFalloff));
    pic.saveToFile(filename);
    pic.destroy;
  end; 
  
PROCEDURE T_zbufferedMap.saveBitmap(z0,z1:single; filename:string; distanceFalloff:single; fogColor:T_floatColor);
  VAR i:longint;
      pt:P_floatColor;
      pic:T_Floatmap;
  begin
    pic.create(xres,yres);
    pt:=pic.rawData;
    for i:=0 to xres*yres-1 do if (data[i].z>=z0) and (data[i].z<z1) then pt[i]:=(fogColor+(data[i].col-fogColor)*exp(data[i].z*distanceFalloff))
                                                                     else pt[i]:=black;
    pic.saveToFile(filename);
    pic.destroy;
  end;


PROCEDURE T_zbufferedMap.incBitmap(VAR pic:T_FloatMap; distanceFalloff:single; fogColor:T_floatColor; project:boolean);
  VAR i:longint;
      pt:P_floatColor;
  begin
    if (pic.width=xres) and (pic.height=yres) then begin
      pt:=pic.rawData;
      if project then begin
        if distanceFalloff>=-1E-10
          then for i:=0 to xres*yres-1 do pt[i]:=pt[i]+projectedColor (          data[i].col                                         ) //projectedColor
          else for i:=0 to xres*yres-1 do pt[i]:=pt[i]+projectedColor (fogColor+(data[i].col-fogColor)*exp(data[i].z*distanceFalloff));//projectedColor
      end else begin
        if distanceFalloff>=-1E-10
          then for i:=0 to xres*yres-1 do pt[i]:=pt[i]+(          data[i].col                                         ) //
          else for i:=0 to xres*yres-1 do pt[i]:=pt[i]+(fogColor+(data[i].col-fogColor)*exp(data[i].z*distanceFalloff));//
      end;
    end;
  end;

FUNCTION T_zbufferedMap.width  :longint; begin result:=xres; end;
FUNCTION T_zbufferedMap.height :longint; begin result:=yres; end;
FUNCTION T_zbufferedMap.rawData:P_zCol;  begin result:=data; end;

CONSTRUCTOR T_3DProjection.create(eyepoint,lookat:T_floatColor; xres,yres:longint; viewAngle:single);
  begin
    reinit(eyepoint,lookat,xres,yres,viewAngle,0,0);
  end;

PROCEDURE T_3DProjection.reinit(eyepoint,lookat:T_floatColor; xres,yres:longint; viewAngle:single; subpixelshiftX,subpixelshiftY:single);
  VAR d:T_floatColor;
      aid:T_baseType;
  begin
    viewAngle:=viewAngle*pi/180;
    eye:=eyepoint;
    d:=lookat-eyepoint;
    d:=normed(d);//d*(1/norm(d));
    aid:=1/system.sqrt(d[0]*d[0]+d[2]*d[2]);
    a[0,0]:=d[2]*aid;       a[0,1]:=0;     a[0,2]:=-d[0]*aid;
    a[1,0]:=-d[0]*d[1]*aid; a[1,1]:=1/aid; a[1,2]:=-d[1]*d[2]*aid;
    a[2,0]:=d[0];           a[2,1]:=d[1];  a[2,2]:= d[2];
    
    invA[0,0]:= d[2]*aid; invA[0,1]:=-d[0]*d[1]*aid; invA[0,2]:=d[0];
    invA[1,0]:= 0;        invA[1,1]:=1/aid;          invA[1,2]:=d[1];
    invA[2,0]:=-d[0]*aid; invA[2,1]:=-d[1]*d[2]*aid; invA[2,2]:=d[2];

    screenWidth :=xres; screenCenterX:=xres/2+subpixelshiftX;
    screenHeight:=yres; screenCenterY:=yres/2+subpixelshiftY;
    zoomFactor:=system.sqrt(xres*xres+yres*yres)/(2*sin(viewAngle)/cos(viewAngle));
  end;

PROCEDURE T_3DProjection.reinit(eyepoint,lookat:T_floatColor; xres,yres:longint; viewAngle:single);
  begin
    reinit(eyepoint,lookat,xres,yres,viewAngle,0,0);
  end;


DESTRUCTOR T_3DProjection.destroy; begin end;

PROCEDURE T_3DProjection.realToScreen(p:T_floatColor; OUT screenX,screenY,screenZ:single);
  VAR floatX,floatY:T_baseType;
  begin
    p:=p-eye;
    floatX :=a[0,0]*p[0]+a[0,1]*p[1]+a[0,2]*p[2];
    floatY :=a[1,0]*p[0]+a[1,1]*p[1]+a[1,2]*p[2];
    screenZ:=a[2,0]*p[0]+a[2,1]*p[1]+a[2,2]*p[2];
    screenX:=round(screenCenterX+zoomFactor*floatX/screenZ);
    screenY:=round(screenCenterY+zoomFactor*floatY/screenZ);
  end;

PROCEDURE T_3DProjection.screenToReal(OUT p:T_floatColor; screenX,screenY,screenZ:T_baseType);
  begin
    screenX:=(screenX-screenCenterX)*screenZ/zoomFactor;
    screenY:=(screenY-screenCenterY)*screenZ/zoomFactor;
    p[0]:=inva[0,0]*screenX+inva[0,1]*screenY+inva[0,2]*screenZ+eye[0];
    p[1]:=inva[1,0]*screenX+inva[1,1]*screenY+inva[1,2]*screenZ+eye[1];
    p[2]:=inva[2,0]*screenX+inva[2,1]*screenY+inva[2,2]*screenZ+eye[2];
  end;

PROCEDURE T_3DProjection.screenToReal(OUT p:T_floatColor; screenX,screenY:T_baseType);
  VAR screenZ:T_baseType;
  begin
    screenX:=(screenX-screenCenterX)/zoomFactor;
    screenY:=(screenY-screenCenterY)/zoomFactor;
    p[0]:=inva[0,0]*screenX+inva[0,1]*screenY+inva[0,2];
    p[1]:=inva[1,0]*screenX+inva[1,1]*screenY+inva[1,2];
    p[2]:=inva[2,0]*screenX+inva[2,1]*screenY+inva[2,2];
    screenZ:=-eye[1]/p[1];
    p:=screenZ*p+eye;
  end;

PROCEDURE T_3DProjection.screenToRealLevel(OUT p:T_floatColor; screenX,screenY,levelZ:T_baseType);
  begin
    screenX:=(screenX-screenCenterX)/zoomFactor;
    screenY:=(screenY-screenCenterY)/zoomFactor;
    p[0]:=inva[0,0]*screenX+inva[0,1]*screenY+inva[0,2];
    p[1]:=inva[1,0]*screenX+inva[1,1]*screenY+inva[1,2];
    p[2]:=inva[2,0]*screenX+inva[2,1]*screenY+inva[2,2];
    levelZ:=(levelZ-eye[1])/p[1];
    p:=levelZ*p+eye;
  end;

FUNCTION T_3DProjection.throwPixelToMap(position,color:T_floatColor; mapdata:P_zCol):boolean;
  VAR floatX,floatY,screenZ:T_baseType;
      ix,iy:longint;
  begin
    result:=false;
    position:=position-eye;
    floatX :=a[0,0]*position[0]+a[0,1]*position[1]+a[0,2]*position[2];
    floatY :=a[1,0]*position[0]+a[1,1]*position[1]+a[1,2]*position[2];
    screenZ:=a[2,0]*position[0]+a[2,1]*position[1]+a[2,2]*position[2];
    if screenZ>1E-3 then begin
      ix     :=round(screenCenterX+zoomFactor*floatX/screenZ);
      iy     :=round(screenCenterY+zoomFactor*floatY/screenZ);
      if (ix>=0) and (ix<screenWidth) and (iy>=0) and (iy<screenHeight) then begin
        ix:=ix+iy*screenWidth;
        result:=true;
        if screenZ<mapdata[ix].z then begin
          mapdata[ix].z  :=screenZ;
          mapdata[ix].col:=color;
        end;
      end;
    end;
  end;

FUNCTION T_3DProjection.throwPixelToMap(position,color:T_floatColor; mapdata:P_zCol; OUT sx,sy:single):boolean;
  VAR floatX,floatY,screenZ:T_baseType;
      ix,iy:longint;
  begin
    result:=false;
    position:=position-eye;
    floatX :=a[0,0]*position[0]+a[0,1]*position[1]+a[0,2]*position[2];
    floatY :=a[1,0]*position[0]+a[1,1]*position[1]+a[1,2]*position[2];
    screenZ:=a[2,0]*position[0]+a[2,1]*position[1]+a[2,2]*position[2];
    if screenZ>1E-3 then begin
      sx:=screenCenterX+zoomFactor*floatX/screenZ; ix:=round(sx);
      sy:=screenCenterY+zoomFactor*floatY/screenZ; iy:=round(sy);
      if (ix>=0) and (ix<screenWidth) and (iy>=0) and (iy<screenHeight) then begin
        ix:=ix+iy*screenWidth;
        result:=true;
        if screenZ<mapdata[ix].z then begin
          mapdata[ix].z  :=screenZ;
          mapdata[ix].col:=color;
        end;
      end;
    end else begin
      sx:=screenCenterX+zoomFactor*floatX/screenZ;
      sy:=screenCenterY+zoomFactor*floatY/screenZ;
    end;
  end;
  
PROCEDURE T_3DProjection.throwPixelToMap(position,color:T_floatColor; mapdata:P_zCol; OUT sx,sy:single; OUT onScreen,foreground:boolean);
  VAR floatX,floatY,screenZ:T_baseType;
      ix,iy:longint;
  begin
    onScreen:=false; foreground:=false;
    position:=position-eye;
    floatX :=a[0,0]*position[0]+a[0,1]*position[1]+a[0,2]*position[2];
    floatY :=a[1,0]*position[0]+a[1,1]*position[1]+a[1,2]*position[2];
    screenZ:=a[2,0]*position[0]+a[2,1]*position[1]+a[2,2]*position[2];
    if screenZ>1E-6 then begin
      sx:=screenCenterX+zoomFactor*floatX/screenZ; ix:=round(sx); 
      sy:=screenCenterY+zoomFactor*floatY/screenZ; iy:=round(sy);
      if (ix>=0) and (ix<screenWidth) and (iy>=0) and (iy<screenHeight) then begin
        ix:=ix+iy*screenWidth;
        onScreen:=true;
        if screenZ<mapdata[ix].z then begin
          mapdata[ix].z  :=screenZ;
          mapdata[ix].col:=color;
          foreground:=true;
        end;
      end;
    end else begin
      sx:=screenCenterX+zoomFactor*floatX/screenZ;
      sy:=screenCenterY+zoomFactor*floatY/screenZ;
    end;
  end;

FUNCTION T_3DProjection.specFactor(position,normal,light:T_floatColor):single;
  begin
    position:=normed(eye-position);
    position[1]:=-position[1];
    result:=position*(light-(normal*(2*(light*normal)))); //Phong specular highlight
  end;


end.
