UNIT zbuf;
INTERFACE
{$fputype sse3}
USES {$ifdef UNIX}cmem,cthreads,{$endif}mypics,math;
TYPE
  T_Vec3D=array[0..2] of double;
  T_screenCoord=record
    x,y:longint;
    z:double;
    outBits:byte;
  end;

  T_zCol=record z:double; col:T_floatColor; end;
  P_zCol=^T_zCol;

  T_triangleNode=record
    {Coordinate Space:} u,v:double;
    {World Space:}      worldPos:T_Vec3D;
    {Screen Space:}     screenPos:T_screenCoord;
  end;

  F_surfaceFunction=FUNCTION(CONST u,v:double):T_Vec3D;
  F_lightingFunction=FUNCTION(CONST position,normal,outgoing:T_Vec3D):T_floatColor;

  T_zbufferedMap=object
    private
      xRes,yRes:longint;
      data:P_zCol;
    public
      CONSTRUCTOR create(CONST newWidth,newHeight:longint);
      CONSTRUCTOR createCopy(VAR original:T_zbufferedMap);
      DESTRUCTOR destroy;
      PROCEDURE clear(CONST color:T_floatColor; CONST z:double);
      PROCEDURE copyToFloatMap(OUT pic:T_FloatMap);
      PROCEDURE saveBitmap(CONST fileName:string);
      PROCEDURE saveBitmap(CONST fileName:string; CONST distanceFalloff:single; CONST fogColor:T_floatColor);
      PROCEDURE saveBitmap(CONST z0,z1:single; CONST fileName:string; CONST distanceFalloff:single; CONST fogColor:T_floatColor);

      PROCEDURE incBitmap(VAR pic:T_FloatMap; CONST distanceFalloff:single; CONST fogColor:T_floatColor; CONST project:boolean);
      FUNCTION width  :longint;
      FUNCTION height :longint;
      FUNCTION rawData:P_zCol;
  end;

  T_3DProjection=object
    a:array[0..2,0..2] of double;
    screenCenterX,screenCenterY:double;
    screenWidth,screenHeight:longint;
    zoomFactor:double;
    eye:T_Vec3D;
    CONSTRUCTOR create(CONST eyepoint,lookat:T_Vec3D; CONST xRes,yRes:longint; CONST viewAngle:single);
    PROCEDURE   reinit(CONST eyepoint,lookat:T_Vec3D; CONST xRes,yRes:longint; CONST viewAngle:single);
    PROCEDURE   reinit(CONST eyepoint,lookat:T_Vec3D; CONST xRes,yRes:longint; CONST viewAngle:single; CONST subpixelshiftX,subpixelshiftY:single);
    DESTRUCTOR destroy;
    FUNCTION realToScreen(CONST p:T_Vec3D):T_screenCoord;
    FUNCTION newNode(CONST u,v:double; CONST surface:F_surfaceFunction):T_triangleNode;
    FUNCTION nodeBetween(CONST node0,node1:T_triangleNode; CONST surface:F_surfaceFunction):T_triangleNode;
    FUNCTION colorOfTriangle(CONST node0,node1,node2:T_triangleNode; CONST lighting:F_lightingFunction):T_floatColor;
  end;

PROCEDURE renderGeometry(VAR map:T_zbufferedMap;
                         VAR projection:T_3DProjection;
                         CONST surface:F_surfaceFunction;
                         CONST u0,u1,v0,v1:double;
                         CONST initSteps:longint;
                         CONST lighting:F_lightingFunction;
                         CONST triangleSizeLimit:double;
                         CONST depth:longint);

IMPLEMENTATION
OPERATOR - (CONST x,y:T_Vec3D):T_Vec3D;
  begin
    result[0]:=x[0]-y[0];
    result[1]:=x[1]-y[1];
    result[2]:=x[2]-y[2];
  end;

OPERATOR + (CONST x,y:T_Vec3D):T_Vec3D;
  begin
    result[0]:=x[0]+y[0];
    result[1]:=x[1]+y[1];
    result[2]:=x[2]+y[2];
  end;

OPERATOR * (CONST x,y:T_Vec3D):double;
  begin
    result:=x[0]*y[0]+x[1]*y[1]+x[2]*y[2];
  end;

OPERATOR * (CONST x:T_Vec3D; CONST y:double):T_Vec3D;
  begin
    result[0]:=x[0]*y;
    result[1]:=x[1]*y;
    result[2]:=x[2]*y;
  end;

FUNCTION normed(CONST x:T_Vec3D):T_Vec3D;
  VAR aid:double;
  begin
    aid:=x[0]*x[0]+x[1]*x[1]+x[2]*x[2];
    if aid>=1E-15 then begin
      aid:=1/sqrt(aid);
      result[0]:=x[0]*aid;
      result[1]:=x[1]*aid;
      result[2]:=x[2]*aid;
    end else begin
      result[0]:=0;
      result[1]:=0;
      result[2]:=0;
    end;
  end;

CONSTRUCTOR T_zbufferedMap.create(CONST newWidth,newHeight:longint);
  begin
    xRes:=newWidth;
    yRes:=newHeight;
    getMem(data,sizeOf(T_zCol)*xRes*yRes);
  end;

CONSTRUCTOR T_zbufferedMap.createCopy(VAR original:T_zbufferedMap);
  begin
    xRes:=original.width;
    yRes:=original.height;
    getMem(data,sizeOf(T_zCol)*xRes*yRes);
    move(original.rawData^,data^,sizeOf(T_zCol)*xRes*yRes);
  end;

DESTRUCTOR T_zbufferedMap.destroy;
  begin
    freeMem(data,sizeOf(T_zCol)*xRes*yRes);
    xRes:=0;
    yRes:=0;
  end;

PROCEDURE T_zbufferedMap.clear(CONST color:T_floatColor; CONST z:double);
  VAR i:longint;
      val:T_zCol;
  begin
    val.col:=color;
    val.z  :=z;
    for i:=0 to xRes*yRes-1 do data[i]:=val;
  end;

PROCEDURE T_zbufferedMap.copyToFloatMap(OUT pic:T_FloatMap);
  VAR i:longint;
      pt:P_floatColor;
  begin
    pic.create(xRes,yRes);
    pt:=pic.rawData;
    for i:=0 to xRes*yRes-1 do pt[i]:=data[i].col;
  end;

PROCEDURE T_zbufferedMap.saveBitmap(CONST fileName:string);
  VAR i:longint;
      pt:P_floatColor;
      pic:T_FloatMap;
  begin
    pic.create(xRes,yRes);
    pt:=pic.rawData;
    for i:=0 to xRes*yRes-1 do pt[i]:=(data[i].col);
    pic.saveToFile(fileName);
    pic.destroy;
  end;

PROCEDURE T_zbufferedMap.saveBitmap(CONST fileName:string; CONST distanceFalloff:single; CONST fogColor:T_floatColor);
  VAR i:longint;
      pt:P_floatColor;
      pic:T_FloatMap;
  begin
    pic.create(xRes,yRes);
    pt:=pic.rawData;
    for i:=0 to xRes*yRes-1 do pt[i]:=(fogColor+(data[i].col-fogColor)*exp(data[i].z*distanceFalloff));
    pic.saveToFile(fileName);
    pic.destroy;
  end;

PROCEDURE T_zbufferedMap.saveBitmap(CONST z0,z1:single; CONST fileName:string; CONST distanceFalloff:single; CONST fogColor:T_floatColor);
  VAR i:longint;
      pt:P_floatColor;
      pic:T_FloatMap;
  begin
    pic.create(xRes,yRes);
    pt:=pic.rawData;
    for i:=0 to xRes*yRes-1 do if (data[i].z>=z0) and (data[i].z<z1) then pt[i]:=(fogColor+(data[i].col-fogColor)*exp(data[i].z*distanceFalloff))
                                                                     else pt[i]:=BLACK;
    pic.saveToFile(fileName);
    pic.destroy;
  end;

PROCEDURE T_zbufferedMap.incBitmap(VAR pic:T_FloatMap; CONST distanceFalloff:single; CONST fogColor:T_floatColor; CONST project:boolean);
  VAR i:longint;
      pt:P_floatColor;
  begin
    if (pic.width=xRes) and (pic.height=yRes) then begin
      pt:=pic.rawData;
      if project then begin
        if distanceFalloff>=-1E-10
          then for i:=0 to xRes*yRes-1 do pt[i]:=pt[i]+projectedColor (          data[i].col                                         ) //projectedColor
          else for i:=0 to xRes*yRes-1 do pt[i]:=pt[i]+projectedColor (fogColor+(data[i].col-fogColor)*exp(data[i].z*distanceFalloff));//projectedColor
      end else begin
        if distanceFalloff>=-1E-10
          then for i:=0 to xRes*yRes-1 do pt[i]:=pt[i]+(          data[i].col                                         ) //
          else for i:=0 to xRes*yRes-1 do pt[i]:=pt[i]+(fogColor+(data[i].col-fogColor)*exp(data[i].z*distanceFalloff));//
      end;
    end;
  end;

FUNCTION T_zbufferedMap.width  :longint; begin result:=xRes; end;
FUNCTION T_zbufferedMap.height :longint; begin result:=yRes; end;
FUNCTION T_zbufferedMap.rawData:P_zCol;  begin result:=data; end;

CONSTRUCTOR T_3DProjection.create(CONST eyepoint,lookat:T_Vec3D; CONST xRes,yRes:longint; CONST viewAngle:single);
  begin
    reinit(eyepoint,lookat,xRes,yRes,viewAngle,0,0);
  end;

PROCEDURE T_3DProjection.reinit(CONST eyepoint,lookat:T_Vec3D; CONST xRes,yRes:longint; CONST viewAngle:single; CONST subpixelshiftX,subpixelshiftY:single);
  VAR d:T_Vec3D;
      aid:double;
      viewAngleInRad:double;
  begin
    viewAngleInRad:=viewAngle*pi/180;
    eye:=eyepoint;
    d:=normed(lookat-eyepoint);
    aid:=1/system.sqrt(d[0]*d[0]+d[2]*d[2]);
    a[0,0]:= d[2]*aid;       a[0,1]:=0;     a[0,2]:=-d[0]*aid;
    a[1,0]:=-d[0]*d[1]*aid;  a[1,1]:=1/aid; a[1,2]:=-d[1]*d[2]*aid;
    a[2,0]:= d[0];           a[2,1]:=d[1];  a[2,2]:= d[2];

    screenWidth :=xRes; screenCenterX:=xRes/2+subpixelshiftX;
    screenHeight:=yRes; screenCenterY:=yRes/2+subpixelshiftY;
    zoomFactor:=system.sqrt(xRes*xRes+yRes*yRes)/(2*sin(viewAngleInRad)/cos(viewAngleInRad));
  end;

PROCEDURE T_3DProjection.reinit(CONST eyepoint,lookat:T_Vec3D; CONST xRes,yRes:longint; CONST viewAngle:single);
  begin
    reinit(eyepoint,lookat,xRes,yRes,viewAngle,0,0);
  end;

DESTRUCTOR T_3DProjection.destroy; begin end;

FUNCTION T_3DProjection.realToScreen(CONST p:T_Vec3D):T_screenCoord;
  VAR floatX,floatY:double;
      q:T_Vec3D;
  begin
    q:=p-eye;
    floatX  :=a[0,0]*q[0]+a[0,1]*q[1]+a[0,2]*q[2];
    floatY  :=a[1,0]*q[0]+a[1,1]*q[1]+a[1,2]*q[2];
    result.z:=a[2,0]*q[0]+a[2,1]*q[1]+a[2,2]*q[2];
    if result.z>1E-10 then begin
      result.x:=round(screenCenterX+zoomFactor*floatX/result.z);
      result.y:=round(screenCenterY+zoomFactor*floatY/result.z);
      result.outBits:=0;
      if result.x<0             then inc(result.outBits);
      if result.x>=screenWidth  then inc(result.outBits,2);
      if result.y<0             then inc(result.outBits,4);
      if result.y>=screenHeight then inc(result.outBits,8);
    end else begin
      result.x:=0;
      result.y:=0;
      result.outBits:=16;
    end;
  end;

FUNCTION T_3DProjection.newNode(CONST u,v:double; CONST surface:F_surfaceFunction):T_triangleNode;
  begin
    result.u:=u;
    result.v:=v;
    result.worldPos:=surface(u,v);
    result.screenPos:=realToScreen(result.worldPos);
  end;

FUNCTION T_3DProjection.nodeBetween(CONST node0,node1:T_triangleNode; CONST surface:F_surfaceFunction):T_triangleNode;
  begin
    result:=newNode(0.5*(node0.u+node1.u),
                    0.5*(node0.v+node1.v),surface);
  end;

FUNCTION T_3DProjection.colorOfTriangle(CONST node0,node1,node2:T_triangleNode; CONST lighting:F_lightingFunction):T_floatColor;
  VAR centerPoint,
      leg0,leg1,
      surfaceNormal,
      outgoing:T_Vec3D;
  begin
    centerPoint[0]:=0.33333333333333333*(node0.worldPos[0]+node1.worldPos[0]+node2.worldPos[0]);
    centerPoint[1]:=0.33333333333333333*(node0.worldPos[1]+node1.worldPos[1]+node2.worldPos[1]);
    centerPoint[2]:=0.33333333333333333*(node0.worldPos[2]+node1.worldPos[2]+node2.worldPos[2]);
    leg0:=node1.worldPos-node0.worldPos;
    leg1:=node2.worldPos-node0.worldPos;
    surfaceNormal[0]:=leg0[1]*leg1[2]-leg0[2]*leg1[1];
    surfaceNormal[1]:=leg0[2]*leg1[0]-leg0[0]*leg1[2];
    surfaceNormal[2]:=leg0[0]*leg1[1]-leg0[1]*leg1[0];
    surfaceNormal:=normed(surfaceNormal);
    outgoing:=normed(centerPoint-eye);
    outgoing:=outgoing-surfaceNormal*(2*(outgoing*surfaceNormal));
    surfaceNormal[1]:=-surfaceNormal[1];
    result:=lighting(centerPoint,surfaceNormal,outgoing);
  end;

PROCEDURE renderGeometry(VAR map:T_zbufferedMap;
                         VAR projection:T_3DProjection;
                         CONST surface:F_surfaceFunction;
                         CONST u0,u1,v0,v1:double;
                         CONST initSteps:longint;
                         CONST lighting:F_lightingFunction;
                         CONST triangleSizeLimit:double;
                         CONST depth:longint);
  PROCEDURE renderTriangle(CONST N0,N1,N2:T_triangleNode; CONST lim:longint);

    PROCEDURE draw(CONST a,b,c:T_screenCoord; CONST col:T_floatColor);
      PROCEDURE line(x0,x1,y:longint; CONST z0,z1:double);
        VAR i:longint;
            z:double;
            zSlope:double;
            pt:P_zCol;
        begin
          if x0=x1 then begin
            pt:=map.rawData+(x0+y*map.xRes);
            if (1/z0<pt^.z) then begin
              pt^.z  :=1/z0;
              pt^.col:=col;
            end;
            exit;
          end else if x0>x1 then begin i:=x0; x0:=x1; x1:=i; end;
          zSlope:=(z1-z0)/(x1-x0);
          z:=z0;
          pt:=map.rawData+(x0+y*map.xRes);
          for i:=x0 to x1 do begin
            if (i>=0) and (i<map.xRes)
            and (1/z<pt^.z)
            then begin
              pt^.z  :=1/z;
              pt^.col:=col;
            end;
            inc(pt);
            z:=z+zSlope;
          end;
        end;

      VAR v:array[0..3] of record
              x,y:longint;
              z:double;
            end;
          i,j:longint;
          xSlope0,xSlope1,
          zSlope0,zSlope1:double;
          curX0,curX1,curZ0,curZ1:double;
      begin
        V[0].x:=round(a.x); V[0].y:=round(a.y); V[0].z:=1/a.z;
        V[1].x:=round(b.x); V[1].y:=round(b.y); V[1].z:=1/b.z;
        V[2].x:=round(c.x); V[2].y:=round(c.y); V[2].z:=1/c.z;
        for i:=1 to 2 do for j:=0 to i-1 do
        if v[j].y>v[i].y then begin
          v[3]:=v[i]; v[i]:=v[j]; v[j]:=v[3];
        end;

        if (V[1].y>V[0].y) then begin
          curX0:=  V[0].x; xSlope0:=(  V[1].x-  V[0].x)/(V[1].y-V[0].y);
          curX1:=  V[0].x; xSlope1:=(  V[2].x-  V[0].x)/(V[2].y-V[0].y);
          curZ0:=1/V[0].z; zSlope0:=(1/V[1].z-1/V[0].z)/(V[1].y-V[0].y);
          curZ1:=1/V[0].z; zSlope1:=(1/V[2].z-1/V[0].z)/(V[2].y-V[0].y);
          for j:=V[0].y to V[1].y do begin
            if (j>=0) and (j<map.yRes) and
               (curX0<maxLongint) and (curX0>-maxLongint) and
               (curX1<maxLongint) and (curX1>-maxLongint) then
              line(round(curX0),round(curX1),j,curZ0,curZ1);
            curX0:=curX0+xSlope0; curZ0:=curZ0+zSlope0;
            curX1:=curX1+xSlope1; curZ1:=curZ1+zSlope1;
          end;
        end else if (V[0].y>=0) and (V[0].y<map.yRes) then line(V[0].x,V[1].x,V[0].y,V[0].z,V[1].z);

        if (V[2].y>V[1].y) then begin
          curX0:=  V[2].x; xSlope0:=(  V[1].x-  V[2].x)/(V[2].y-V[1].y);
          curX1:=  V[2].x; xSlope1:=(  V[0].x-  V[2].x)/(V[2].y-V[0].y);
          curZ0:=1/V[2].z; zSlope0:=(1/V[1].z-1/V[2].z)/(V[2].y-V[1].y);
          curZ1:=1/V[2].z; zSlope1:=(1/V[0].z-1/V[2].z)/(V[2].y-V[0].y);
          for j:=V[2].y downto V[1].y do begin
            if (j>=0) and (j<map.yRes) and
               (curX0<maxLongint) and (curX0>-maxLongint) and
               (curX1<maxLongint) and (curX1>-maxLongint) then
              line(round(curX0),round(curX1),j,curZ0,curZ1);
            curX0:=curX0+xSlope0; curZ0:=curZ0+zSlope0;
            curX1:=curX1+xSlope1; curZ1:=curZ1+zSlope1;
          end;
        end else if (V[2].y>=0) and (V[2].y<map.yRes) then line(V[2].x,V[1].x,V[2].y,V[2].z,V[1].z);

        //if V[2].y<=V[1].y then exit;
        //xSlope0:=(  V[2].x-  V[1].x)/(V[2].y-V[1].y);
        //zSlope0:=(1/V[2].z-1/V[1].z)/(V[2].y-V[1].y);
        //for j:=V[1].y to V[2].y do begin
        //  if (j>=0) and (j<map.yres) and
        //     (curX0<maxlongint) and (curX0>-maxlongint) and
        //     (curX1<maxlongint) and (curX1>-maxlongint) then
        //    line(round(curX0),round(curX1),j,curZ0,curZ1);
        //  curX0:=curX0+xSlope0; curZ0:=curZ0+zSlope0;
        //  curX1:=curX1+xSlope1; curZ1:=curZ1+zSlope1;
        //end;
      end;

  PROCEDURE draw(CONST N:T_triangleNode; CONST col:T_floatColor); inline;
    VAR pixel:P_zCol;
        dx,dy:longint;
        x,y:longint;
    begin
      x:=round((N0.screenPos.x+N1.screenPos.x+N2.screenPos.x)/3);
      y:=round((N0.screenPos.y+N1.screenPos.y+N2.screenPos.y)/3);

      if N.screenPos.outBits<>0 then exit;
      for dx:=max(0,x) to min(map.xRes-1,x) do
      for dy:=max(0,y) to min(map.yRes-1,y) do begin
        pixel:=map.rawData+(dx+dy*map.xRes);
        if N.screenPos.z<pixel^.z then begin
          pixel^.z:=N.screenPos.z;
          pixel^.col:=col;
        end;
      end;
    end;

    VAR d01,d02,d12:double;
        NN:T_triangleNode;
        col:T_floatColor;
    begin
      if N0.screenPos.outBits and
         N1.screenPos.outBits and
         N2.screenPos.outBits>0 then exit;
      d01:=sqr(N0.screenPos.x-N1.screenPos.x)+sqr(N0.screenPos.y-N1.screenPos.y);
      d02:=sqr(N0.screenPos.x-N2.screenPos.x)+sqr(N0.screenPos.y-N2.screenPos.y);
      d12:=sqr(N2.screenPos.x-N1.screenPos.x)+sqr(N2.screenPos.y-N1.screenPos.y);
      if (d01<triangleSizeLimit) and
         (d02<triangleSizeLimit) and
         (d12<triangleSizeLimit) or (lim<=0) then begin
        col:=projection.colorOfTriangle(N0,N1,N2,lighting);
        draw(N0.screenPos,N1.screenPos,N2.screenPos ,col);
        //draw(N0,col);
      end else begin
        if d01>d02 then begin
          if d01>d12 then begin
            NN:=projection.nodeBetween(N0,N1,surface);
            renderTriangle(N0,NN,N2,lim-1);
            renderTriangle(NN,N1,N2,lim-1);
          end else begin
            NN:=projection.nodeBetween(N1,N2,surface);
            renderTriangle(N1,NN,N0,lim-1);
            renderTriangle(NN,N2,N0,lim-1);
          end;
        end else begin
          if d02>d12 then begin
            NN:=projection.nodeBetween(N0,N2,surface);
            renderTriangle(N2,NN,N1,lim-1);
            renderTriangle(NN,N0,N1,lim-1);
          end else begin
            NN:=projection.nodeBetween(N1,N2,surface);
            renderTriangle(N1,NN,N0,lim-1);
            renderTriangle(NN,N2,N0,lim-1);
          end;
        end;
      end;
    end;

  VAR i,j:longint;
  begin
    if initSteps<=1 then begin
      renderTriangle(projection.newNode(u0,v0,surface),
                     projection.newNode(u1,v0,surface),
                     projection.newNode(u1,v1,surface),depth);
      renderTriangle(projection.newNode(u0,v0,surface),
                     projection.newNode(u1,v1,surface),
                     projection.newNode(u0,v1,surface),depth);
    end else begin
      for i:=0 to initSteps-1 do
      for j:=0 to initSteps-1 do begin
        renderTriangle(projection.newNode(u0+(u1-u0)*(i+0)/initSteps,v0+(v1-v0)*(j+0)/initSteps,surface),
                       projection.newNode(u0+(u1-u0)*(i+1)/initSteps,v0+(v1-v0)*(j+0)/initSteps,surface),
                       projection.newNode(u0+(u1-u0)*(i+1)/initSteps,v0+(v1-v0)*(j+1)/initSteps,surface),depth);
        renderTriangle(projection.newNode(u0+(u1-u0)*(i+0)/initSteps,v0+(v1-v0)*(j+0)/initSteps,surface),
                       projection.newNode(u0+(u1-u0)*(i+1)/initSteps,v0+(v1-v0)*(j+1)/initSteps,surface),
                       projection.newNode(u0+(u1-u0)*(i+0)/initSteps,v0+(v1-v0)*(j+1)/initSteps,surface),depth);
      end;
    end;
  end;

INITIALIZATION
  SetExceptionMask([ exInvalidOp,  exDenormalized,  exZeroDivide,  exOverflow,  exUnderflow,  exPrecision]);

end.
