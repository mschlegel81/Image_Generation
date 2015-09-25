PROGRAM makeMountains;
USES {$ifdef UNIX}cmem,cthreads,{$endif}zbuf,mypics,process,sysutils,math,complex;
{$fputype sse3}

{define showProgress}
{define showTempImages}
CONST xres=2048; yres=1152;
      //xres=800; yres=600;
      //xres=4000; yres=3000;
      //light:T_floatColor=(sqrt(1/2),sqrt(1/2),0);
      //light:T_floatColor=(0,1,0);
     mapSize=7;

VAR perlinMap:array[0..mapSize,0..mapSize] of single;


VAR zmap:T_zbufferedMap;
    proj:T_3DProjection;
    pt  :P_zCol;
    sampleCount:longint;
    refineThreshold:single=1.5;
    recurseThreshold:longint=1;
    i:longint;
    maxImproveRuns:longint=4;

    phase:single=0;
    light:T_floatColor;

    complexity:longint=0;
    expectedHeight:single=0;

    heightSamples:longint=1;
    heightSum    :single =0;

    totalTimeResampling:double;
    totalTimePolishing:double;

PROCEDURE initMap(seed:longint);
  VAR i,j:longint;
  begin
    randseed:=seed;
    for i:=0 to mapsize do for j:=0 to mapsize do perlinMap[i,j]:=random-0.5;
  end;

FUNCTION smooth(cx,cy:single):single; inline;
  VAR ix,iy:longint;
      i0,i1,i2,i3,
      j0,j1,j2,j3:longint;
      q0,q1,q2,q3:single;

  begin
    ix:=floor(cx); cx:=cx-ix;
    iy:=floor(cy); cy:=cy-iy;
    q0:=cy*(-0.5+(1-cy*0.5)*cy); q1:=1+cy*cy*(-2.5+(3*cy)*0.5); q2:=cy*(0.5+(2-(3*cy)*0.5)*cy); q3:=(-0.5+cy*0.5)*cy*cy;
    i0:=(ix  ) and mapSize;      i1:=(ix+1) and mapSize;        i2:=(ix+2) and mapSize;         i3:=(ix+3) and mapSize;
    j0:=(iy  ) and mapSize;      j1:=(iy+1) and mapSize;        j2:=(iy+2) and mapSize;         j3:=(iy+3) and mapSize;
    result:=(perlinMap[i0,j0]*q0+perlinMap[i0,j1]*q1+perlinMap[i0,j2]*q2+perlinMap[i0,j3]*q3)*(cx*(-0.5+(1-cx*0.5)*cx))+
            (perlinMap[i1,j0]*q0+perlinMap[i1,j1]*q1+perlinMap[i1,j2]*q2+perlinMap[i1,j3]*q3)*(1+cx*cx*(-2.5+(3*cx)*0.5))+
            (perlinMap[i2,j0]*q0+perlinMap[i2,j1]*q1+perlinMap[i2,j2]*q2+perlinMap[i2,j3]*q3)*(cx*(0.5+(2-(3*cx)*0.5)*cx))+
            (perlinMap[i3,j0]*q0+perlinMap[i3,j1]*q1+perlinMap[i3,j2]*q2+perlinMap[i3,j3]*q3)*((-0.5+cx*0.5)*cx*cx);
  end;

{FUNCTION mapValue(cx,cy:single):single; inline;
  VAR ix,iy:longint;
      i0,i1,
      j0,j1:longint;
      q0,q1:single;

  begin
    ix:=floor(cx); cx:=cx-ix;
    iy:=floor(cy); cy:=cy-iy;
    q0:=1-cy; q1:=cy;
    i0:=(ix  ) and mapSize;      i1:=(ix+1) and mapSize;
    j0:=(iy  ) and mapSize;      j1:=(iy+1) and mapSize;
    result:=(perlinMap[i0,j0]*q0+perlinMap[i0,j1]*q1)*(1-cx)+
            (perlinMap[i1,j0]*q0+perlinMap[i1,j1]*q1)*(  cx);
  end;  }

FUNCTION facet(cx,cy:single):single; inline;
  VAR ix,iy:longint;
      i0,i1,
      j0,j1:longint;
      q0,q1:single;

  begin
    ix:=floor(cx); cx:=cx-ix;
    iy:=floor(cy); cy:=cy-iy;
    i0:=(ix  ) and mapSize;      i1:=(ix+1) and mapSize;
    j0:=(iy  ) and mapSize;      j1:=(iy+1) and mapSize;
    if cx>cy then begin
      result:=perlinMap[i0,j0]+(perlinMap[i1,j0]-perlinMap[i0,j0])*cx
                              +(perlinMap[i1,j1]-perlinMap[i1,j0])*cy;
    end else begin
      result:=perlinMap[i0,j0]+(perlinMap[i0,j1]-perlinMap[i0,j0])*cy
                              +(perlinMap[i1,j1]-perlinMap[i0,j1])*cx;
    end;
  end;


PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif} 'display '+ps;
    tempProcess.execute;
    tempProcess.free;
  end;

PROCEDURE save(fname:string);
  begin
    zmap.saveBitmap(fname,-0.04,black);

    backgroundDisplay(fname);
    sleep(100);

  end;


FUNCTION throwPixel(cx,cy:single; OUT screenX,screenY:single; OUT foreground:boolean):boolean; inline;
  VAR rad,{radX,}radY,amplitude:single;
      p,n:T_floatColor;
      k,kk:longint;
      inter:single;
      sample:array[0..2] of T_floatColor;
  begin
    inc(sampleCount);


    sample[0,0]:=cx+1E-5; sample[0,1]:=cy     ; sample[0,2]:=0;
    sample[1,0]:=cx     ; sample[1,1]:=cy+1E-5; sample[1,2]:=0;
    sample[2,0]:=cx     ; sample[2,1]:=cy     ; sample[2,2]:=0;
    amplitude:=1;

    kk:=ceil(cx+4); inter:=(cx+5-kk); if kk>complexity then kk:=complexity;
    for k:=0 to kk-1 do begin
      sample[0,2]:=sample[0,2]+facet(sample[0,0],sample[0,1])*amplitude;
      sample[1,2]:=sample[1,2]+facet(sample[1,0],sample[1,1])*amplitude;
      sample[2,2]:=sample[2,2]+facet(sample[2,0],sample[2,1])*amplitude;
      amplitude:=amplitude*0.5;
      sample[0,0]:=sample[0,0]*2; sample[0,1]:=sample[0,1]*2;
      sample[1,0]:=sample[1,0]*2; sample[1,1]:=sample[1,1]*2;
      sample[2,0]:=sample[2,0]*2; sample[2,1]:=sample[2,1]*2;
    end;
    if kk>=0 then begin
    amplitude:=amplitude*inter;
    sample[0,2]:=sample[0,2]+facet(sample[0,0],sample[0,1])*amplitude;
    sample[1,2]:=sample[1,2]+facet(sample[1,0],sample[1,1])*amplitude;
    sample[2,2]:=sample[2,2]+facet(sample[2,0],sample[2,1])*amplitude;
    end;

    {inter:=(cx+0.5)*0.2; if inter>0.6 then inter:=0.6;
     if inter<0 then begin
      sample[0,2]:=smooth(sample[0,0],sample[0,1]);
      sample[1,2]:=smooth(sample[1,0],sample[1,1]);
      sample[2,2]:=smooth(sample[2,0],sample[2,1]);
    end else for k:=0 to complexity do begin
      sample[0,2]:=sample[0,2]+smooth(sample[0,0],sample[0,1])*amplitude;
      sample[1,2]:=sample[1,2]+smooth(sample[1,0],sample[1,1])*amplitude;
      sample[2,2]:=sample[2,2]+smooth(sample[2,0],sample[2,1])*amplitude;
      amplitude:=amplitude*inter;
      sample[0,0]:=sample[0,0]*2; sample[0,1]:=sample[0,1]*2;
      sample[1,0]:=sample[1,0]*2; sample[1,1]:=sample[1,1]*2;
      sample[2,0]:=sample[2,0]*2; sample[2,1]:=sample[2,1]*2;
    end; }

   {inter:=(cx+0.5);
    if inter>=1 then begin
      for k:=0 to complexity do begin
        sample[0,2]:=sample[0,2]+facet(sample[0,0],sample[0,1])*amplitude;
        sample[1,2]:=sample[1,2]+facet(sample[1,0],sample[1,1])*amplitude;
        sample[2,2]:=sample[2,2]+facet(sample[2,0],sample[2,1])*amplitude;
        amplitude:=amplitude/2;
        sample[0,0]:=sample[0,0]*2; sample[0,1]:=sample[0,1]*2;
        sample[1,0]:=sample[1,0]*2; sample[1,1]:=sample[1,1]*2;
        sample[2,0]:=sample[2,0]*2; sample[2,1]:=sample[2,1]*2;
      end;
    end else if inter<=0 then begin
      for k:=0 to complexity do begin
        sample[0,2]:=sample[0,2]+smooth(sample[0,0],sample[0,1])*amplitude;
        sample[1,2]:=sample[1,2]+smooth(sample[1,0],sample[1,1])*amplitude;
        sample[2,2]:=sample[2,2]+smooth(sample[2,0],sample[2,1])*amplitude;
        amplitude:=amplitude/2;
        sample[0,0]:=sample[0,0]*2; sample[0,1]:=sample[0,1]*2;
        sample[1,0]:=sample[1,0]*2; sample[1,1]:=sample[1,1]*2;
        sample[2,0]:=sample[2,0]*2; sample[2,1]:=sample[2,1]*2;
      end;
    end else begin
      for k:=0 to complexity do begin
        sample[0,2]:=sample[0,2]+(facet(sample[0,0],sample[0,1])*inter+smooth(sample[0,0],sample[0,1])*(1-inter))*amplitude;
        sample[1,2]:=sample[1,2]+(facet(sample[1,0],sample[1,1])*inter+smooth(sample[1,0],sample[1,1])*(1-inter))*amplitude;
        sample[2,2]:=sample[2,2]+(facet(sample[2,0],sample[2,1])*inter+smooth(sample[2,0],sample[2,1])*(1-inter))*amplitude;
        amplitude:=amplitude/2;
        sample[0,0]:=sample[0,0]*2; sample[0,1]:=sample[0,1]*2;
        sample[1,0]:=sample[1,0]*2; sample[1,1]:=sample[1,1]*2;
        sample[2,0]:=sample[2,0]*2; sample[2,1]:=sample[2,1]*2;
      end;
    end;  }
    n[1]:=1E-5;
    n[0]:=sample[0,2]-sample[2,2];
    n[2]:=sample[1,2]-sample[2,2];
    p[0]:=cx;
    p[2]:=cy;
    p[1]:=sample[2,2];


       {    n[1]:=1E-5;
           p[0]:=cx+1E-5; p[2]:=cy; p[1]:=0;
           amplitude:=1;
           for k:=0 to complexity do begin
             p[1]:=p[1]+mapValue(p[0],p[2])*amplitude;
             amplitude:=-amplitude/2; p[0]:=p[0]*2; p[2]:=p[2]*2;
           end;
           n[0]:=p[1];

           p[0]:=cx; p[2]:=cy+1E-5; p[1]:=0;
           amplitude:=1;
           for k:=0 to complexity do begin
             p[1]:=p[1]+mapValue(p[0],p[2])*amplitude;
             amplitude:=-amplitude/2; p[0]:=p[0]*2; p[2]:=p[2]*2;
           end;
           n[2]:=p[1];

           p[0]:=cx; p[2]:=cy; p[1]:=0;
           amplitude:=1;
           for k:=0 to complexity do begin
             p[1]:=p[1]+mapValue(p[0],p[2])*amplitude;
             amplitude:=-amplitude/2; p[0]:=p[0]*2; p[2]:=p[2]*2;
           end;
           p[0]:=cx; p[2]:=cy;

           n[0]:=n[0]-p[1];
           n[2]:=n[2]-p[1];}

           n:=normed(n);
           rad:=((light*n)*0.5+0.5);
           radY:=proj.specFactor(p,n,light);
           if radY>0 then for k:=0 to 4 do radY:=radY*radY
                     else rady:=0;
           n[0]:=max(0,0.5*rad-p[1])+radY*0.5;
           n[1]:=          rad      +radY;
           n[2]:=max(0,0.5*rad-p[1])+radY;

           proj.throwPixelToMap(p,n,pt,screenX,screenY,result,foreground);



    heightSamples:=heightSamples+1;
    heightSum:=heightSum+p[1];
    foreground:=foreground or not(result);
  end;




FUNCTION throwPixel(cx,cy:single):boolean; inline;
  VAR foreground:boolean;
  begin result:=throwPixel(cx,cy,cx,cy,foreground); end;

FUNCTION resample:longint;
  VAR ix,iy,runs:longint;
      n0,n1,n2,n3:boolean;
      p0,p1:T_floatColor;
      startTime:double;
  begin
    startTime:=now;
    {$ifdef showProgress} write('R'); {$endif}
    runs:=0;
    repeat
      result:=0;
      for iy:=0 to yres-1 do for ix:=0 to xres-1 do if pt[ix+iy*xres].z>1E19 then begin
        n0:=(ix>0     ) and (pt[ix-1+iy  *xres].z<1E19);
        n1:=(ix<xres-1) and (pt[ix+1+iy  *xres].z<1E19);
        if n0 and n1 then begin
          proj.screenToReal(p0,ix-1,iy,pt[ix-1+iy*xres].z);
          proj.screenToReal(p1,ix+1,iy,pt[ix+1+iy*xres].z);
          p0:=p0+(p1-p0)*random;
          throwPixel(p0[0],p0[2]);
          inc(result);
        end else begin
          n2:=(iy>0     ) and (pt[ix+(iy-1)*xres].z<1E19);
          n3:=(iy<yres-1) and (pt[ix+(iy+1)*xres].z<1E19);
          if n2 and n3 then begin
            proj.screenToReal(p0,ix,iy-1,pt[ix+(iy-1)*xres].z);
            proj.screenToReal(p1,ix,iy+1,pt[ix+(iy+1)*xres].z);
            p0:=p0+(p1-p0)*random;
            throwPixel(p0[0],p0[2]);
            inc(result);
          end else if n0 then begin
            proj.screenToReal(p0,ix-random,iy,pt[ix-1+iy*xres].z);
            throwPixel(p0[0],p0[2]);
            inc(result);
          end else if n1 then begin
            proj.screenToReal(p0,ix+random,iy,pt[ix+1+iy*xres].z);
            throwPixel(p0[0],p0[2]);
            inc(result);
          end else if n3 then begin
            proj.screenToReal(p0,ix,iy+random,pt[ix+(iy+1)*xres].z);
            throwPixel(p0[0],p0[2]);
            inc(result);
          end; //if n2 and n3
        end; //if n0 and n1
      end; //for
      inc(runs);
    until (runs>=maxImproveRuns) or (result+result<xres);
    totalTimeResampling:=totalTimeResampling+now-startTime;
  end;

FUNCTION polish:longint;
  CONST thr=1E-6;
  VAR ix,iy,runs:longint;
      n0,n1,n2,n3:boolean;
      p0,p1:T_floatColor;
      zhere:single;
      startTime:double;
  begin
    startTime:=now;
    {$ifdef showProgress} write('P'); {$endif}
    runs:=0;
    repeat
      result:=0;
      for iy:=0 to yres-1 do for ix:=0 to xres-1 do begin
        zhere:=pt[ix+iy*xres].z;
        n0:=(ix>0     ) and (pt[ix-1+iy  *xres].z<zhere-thr);
        n1:=(ix<xres-1) and (pt[ix+1+iy  *xres].z<zhere-thr);
        if n0 and n1 then begin
          proj.screenToReal(p0,ix-1,iy,pt[ix-1+iy*xres].z);
          proj.screenToReal(p1,ix+1,iy,pt[ix+1+iy*xres].z);
          p0:=p0+(p1-p0)*random;
          throwPixel(p0[0],p0[2]);
          inc(result);
        end else begin
          n2:=(iy>0     ) and (pt[ix+(iy-1)*xres].z<zhere-thr);
          n3:=(iy<yres-1) and (pt[ix+(iy+1)*xres].z<zhere-thr);
          if n2 and n3 then begin
            proj.screenToReal(p0,ix,iy-1,pt[ix+(iy-1)*xres].z);
            proj.screenToReal(p1,ix,iy+1,pt[ix+(iy+1)*xres].z);
            p0:=p0+(p1-p0)*random;
            throwPixel(p0[0],p0[2]);
            inc(result);
          end; //if n2 and n3
        end; //if n0 and n1
      end; //for
      inc(runs);
    until (runs>=maxImproveRuns) or (result<xres+yres);
    totalTimePolishing:=totalTimePolishing+now-startTime;
  end;


PROCEDURE throwPixels(quality:single; improve:boolean);
  VAR ix,x0,x1,jx,jy,k:longint;
      thisY,stepY:single;
      maxYDiff,maxXDiff:single;
      sc:array[0..8,0..8] of record x,y:single; ready,foreground:boolean; end;
      p:T_floatColor;
      thrown,thrownInLine:boolean;
      startTime:double;

  FUNCTION someBackground(kx0,kx1,ky0,ky1:longint):boolean;
    VAR xMax,xMin,yMax,yMin,x,y:longint;
    begin
      xMax:=min(xres-1,ceil (max(max(sc[kx0,ky0].x,sc[kx0,ky1].x),
                                 max(sc[kx1,ky0].x,sc[kx1,ky1].x))));
      xMin:=max(0,     floor(min(min(sc[kx0,ky0].x,sc[kx0,ky1].x),
                                 min(sc[kx1,ky0].x,sc[kx1,ky1].x))));
      yMax:=min(yres-1,ceil (max(max(sc[kx0,ky0].y,sc[kx0,ky1].y),
                                 max(sc[kx1,ky0].y,sc[kx1,ky1].y))));
      yMin:=max(0,     floor(min(min(sc[kx0,ky0].y,sc[kx0,ky1].y),
                                 min(sc[kx1,ky0].y,sc[kx1,ky1].y))));
      result:=sc[kx0,ky0].foreground or sc[kx1,ky0].foreground or sc[kx0,ky1].foreground or sc[kx1,ky1].foreground;
      for y:=yMin to yMax do for x:=xMin to xMax do result:=result or (pt[x+y*xres].z>1E19);
    end;

  PROCEDURE renderQuad(kx0,kx1,ky0,ky1:longint);
    VAR kx,ky,jx,jy,jxs,jys:longint; fx,fy:single;

    begin
      jxs:=ceil(sqrt(max(system.sqr(sc[kx0,ky0].x-sc[kx1,ky0].x)+system.sqr(sc[kx0,ky0].y-sc[kx1,ky0].y),
                         system.sqr(sc[kx0,ky1].x-sc[kx1,ky1].x)+system.sqr(sc[kx0,ky1].y-sc[kx1,ky1].y))/refineThreshold));
      jys:=ceil(sqrt(max(system.sqr(sc[kx0,ky0].x-sc[kx0,ky1].x)+system.sqr(sc[kx0,ky0].y-sc[kx0,ky1].y),
                         system.sqr(sc[kx1,ky0].x-sc[kx1,ky1].x)+system.sqr(sc[kx1,ky0].y-sc[kx1,ky1].y))/refineThreshold));
      if ((jxs>recurseThreshold) and (jys>recurseThreshold)) and (kx1>kx0+1) and someBackground(kx0,kx1,ky0,ky1) then begin
//      if ((jxs>recurseThreshold) and (jys>recurseThreshold)) and (kx1>kx0+1) and (sc[kx0,ky0].foreground or sc[kx1,ky0].foreground or sc[kx0,ky1].foreground or sc[kx1,ky1].foreground) then begin
//      if ((jxs>recurseThreshold) and (jys>recurseThreshold)) and (kx1>kx0+1) then begin
        kx:=(kx0+kx1) shr 1;
        ky:=(ky0+ky1) shr 1;
        if not(sc[kx0,ky ].ready) then begin proj.screenToRealLevel(p,(ix+kx0*0.125)/quality,(thisy+ky *0.125*stepY),expectedHeight); throwPixel(p[0],p[2],sc[kx0,ky ].x,sc[kx0,ky ].y,sc[kx0,ky ].foreground); sc[kx0,ky ].ready:=true; end;
        if not(sc[kx ,ky0].ready) then begin proj.screenToRealLevel(p,(ix+kx *0.125)/quality,(thisy+ky0*0.125*stepY),expectedHeight); throwPixel(p[0],p[2],sc[kx ,ky0].x,sc[kx ,ky0].y,sc[kx ,ky0].foreground); sc[kx ,ky0].ready:=true; end;
        if not(sc[kx ,ky ].ready) then begin proj.screenToRealLevel(p,(ix+kx *0.125)/quality,(thisy+ky *0.125*stepY),expectedHeight); throwPixel(p[0],p[2],sc[kx ,ky ].x,sc[kx ,ky ].y,sc[kx ,ky ].foreground); sc[kx ,ky ].ready:=true; end;
        if not(sc[kx ,ky1].ready) then begin proj.screenToRealLevel(p,(ix+kx *0.125)/quality,(thisy+ky1*0.125*stepY),expectedHeight); throwPixel(p[0],p[2],sc[kx ,ky1].x,sc[kx ,ky1].y,sc[kx ,ky1].foreground); sc[kx ,ky1].ready:=true; end;
        if not(sc[kx1,ky ].ready) then begin proj.screenToRealLevel(p,(ix+kx1*0.125)/quality,(thisy+ky *0.125*stepY),expectedHeight); throwPixel(p[0],p[2],sc[kx1,ky ].x,sc[kx1,ky ].y,sc[kx1,ky ].foreground); sc[kx1,ky ].ready:=true; end;
        renderQuad(kx0,kx,ky0,ky);
        renderQuad(kx,kx1,ky0,ky);
        renderQuad(kx0,kx,ky,ky1);
        renderQuad(kx,kx1,ky,ky1);
     end else if someBackground(kx0,kx1,ky0,ky1) then begin
//     end else if sc[kx0,ky0].foreground or sc[kx1,ky0].foreground or sc[kx0,ky1].foreground or sc[kx1,ky1].foreground then begin
//     end else begin
       for jy:=0 to jys-1 do begin
         fy:=(ky0+jy*(ky1-ky0)/jys)*0.125;
         for jx:=0 to jxs-1 do if (jx>0) or (jy>0) then begin
           fx:=(kx0+jx*(kx1-kx0)/jxs)*0.125;
           proj.screenToRealLevel(p,(ix+fx)/quality,thisY+fy*stepY,expectedHeight);
           throwPixel(p[0],p[2]);
         end;
       end;
      end;
    end;

  begin
    expectedHeight:=0.5*(proj.eye[1]+min(heightSum/heightSamples,proj.eye[1]-1E-3));

//    writeln('expectedHeight=',expectedHeight:0:5);
    heightSamples:=0;
    heightSum    :=0;
    totalTimePolishing:=0;
    totalTimeResampling:=0;
    startTime:=now;
    sampleCount:=0;
    x0:=0; x1:=round(xres*quality);
    thisY:=-5*yres;
    stepY:=1/quality;
    repeat
      maxYDiff:=0;
      maxXDiff:=0;
      {$ifdef showProgress} write('S'); {$endif}
      thrownInLine:=false;
      k:=(x0+x1) shr 1;
      ix:=k;
      repeat
        //if odd(ix) then begin
        for jx:=0 to 8 do for jy:=0 to 8 do sc[jy,jx].ready:=((jx=0) or (jx=8)) and ((jy=0) or (jy=8));
        proj.screenToRealLevel(p, ix   /quality,thisY      ,expectedHeight); thrown:=throwPixel(p[0],p[2],sc[0,0].x,sc[0,0].y,sc[0,0].foreground);
        proj.screenToRealLevel(p,(ix+1)/quality,thisY      ,expectedHeight); if      throwPixel(p[0],p[2],sc[8,0].x,sc[8,0].y,sc[8,0].foreground) then thrown:=true;
        proj.screenToRealLevel(p, ix   /quality,thisY+stepY,expectedHeight); if      throwPixel(p[0],p[2],sc[0,8].x,sc[0,8].y,sc[0,8].foreground) then thrown:=true;
        proj.screenToRealLevel(p,(ix+1)/quality,thisY+stepY,expectedHeight); if      throwPixel(p[0],p[2],sc[8,8].x,sc[8,8].y,sc[8,8].foreground) then thrown:=true;
        if thrown then begin
          thrownInLine:=true; if ix<x0 then x0:=ix;
          renderQuad(0,8,0,8);
//          maxXDiff:=maxXDiff+max(system.sqr(sc[0,0].x-sc[8,0].x)+system.sqr(sc[0,0].y-sc[8,0].y),
//                                 system.sqr(sc[0,8].x-sc[8,8].x)+system.sqr(sc[0,8].y-sc[8,8].y));
//          maxYDiff:=maxYDiff+max(system.sqr(sc[0,0].x-sc[0,8].x)+system.sqr(sc[0,0].y-sc[0,8].y),
//                                 system.sqr(sc[8,0].x-sc[8,8].x)+system.sqr(sc[8,0].y-sc[8,8].y));
          maxXDiff:=max(maxXDiff,max(system.sqr(sc[0,0].x-sc[8,0].x)+system.sqr(sc[0,0].y-sc[8,0].y),
                                     system.sqr(sc[0,8].x-sc[8,8].x)+system.sqr(sc[0,8].y-sc[8,8].y)));
          maxYDiff:=max(maxYDiff,max(system.sqr(sc[0,0].x-sc[0,8].x)+system.sqr(sc[0,0].y-sc[0,8].y),
                                     system.sqr(sc[8,0].x-sc[8,8].x)+system.sqr(sc[8,0].y-sc[8,8].y)));
        end;
        dec(ix);
      until not(thrown) and (sc[0,0].x<0) and (ix<=x0);
      ix:=k+1;
      repeat
        //if odd(ix) then begin
        for jx:=0 to 8 do for jy:=0 to 8 do sc[jy,jx].ready:=((jx=0) or (jx=8)) and ((jy=0) or (jy=8));
        proj.screenToRealLevel(p, ix   /quality,thisY      ,expectedHeight); thrown:=throwPixel(p[0],p[2],sc[0,0].x,sc[0,0].y,sc[0,0].foreground);
        proj.screenToRealLevel(p,(ix+1)/quality,thisY      ,expectedHeight); if      throwPixel(p[0],p[2],sc[8,0].x,sc[8,0].y,sc[8,0].foreground) then thrown:=true;
        proj.screenToRealLevel(p, ix   /quality,thisY+stepY,expectedHeight); if      throwPixel(p[0],p[2],sc[0,8].x,sc[0,8].y,sc[0,8].foreground) then thrown:=true;
        proj.screenToRealLevel(p,(ix+1)/quality,thisY+stepY,expectedHeight); if      throwPixel(p[0],p[2],sc[8,8].x,sc[8,8].y,sc[8,8].foreground) then thrown:=true;
        if thrown then begin
          thrownInLine:=true; if ix>x1 then x1:=ix;
          renderQuad(0,8,0,8);
          //maxXDiff:=maxXDiff+max(system.sqr(sc[0,0].x-sc[8,0].x)+system.sqr(sc[0,0].y-sc[8,0].y),
          //                       system.sqr(sc[0,8].x-sc[8,8].x)+system.sqr(sc[0,8].y-sc[8,8].y));
          //maxYDiff:=maxYDiff+max(system.sqr(sc[0,0].x-sc[0,8].x)+system.sqr(sc[0,0].y-sc[0,8].y),
          //                       system.sqr(sc[8,0].x-sc[8,8].x)+system.sqr(sc[8,0].y-sc[8,8].y));
          maxXDiff:=max(maxXDiff,max(system.sqr(sc[0,0].x-sc[8,0].x)+system.sqr(sc[0,0].y-sc[8,0].y),
                                     system.sqr(sc[0,8].x-sc[8,8].x)+system.sqr(sc[0,8].y-sc[8,8].y)));
          maxYDiff:=max(maxYDiff,max(system.sqr(sc[0,0].x-sc[0,8].x)+system.sqr(sc[0,0].y-sc[0,8].y),
                                     system.sqr(sc[8,0].x-sc[8,8].x)+system.sqr(sc[8,0].y-sc[8,8].y)));
        end;
        inc(ix);
      until not(thrown) and (sc[8,8].x>xres) and (ix>=x1);
      if improve and thrownInLine then resample;
      thisY:=thisY+stepY;
      if thrownInLine then begin
//        if      maxYDiff>1.2*maxXDiff then stepY:=stepY*0.5
//        else if maxYDiff<0.9*maxXDiff then stepY:=stepY*1.2;
        stepY:=stepY*sqrt((1E-3+maxXDiff)/(1E-3+maxYDiff));
        if stepY<0.5*quality then stepY:=0.5*quality;
      end;

      {$ifdef showTempImages} writeln('stepsize is ',stepY:0:5,' ',x0,' ',x1); if thrownInLine then begin save('temp.vraw'); readln; end; {$endif}
    until not(thrownInLine) and (sc[8,8].y>yres);
    if improve then polish;
    writeln(sampleCount/(xres*yres):0:5,'spp ',(now-startTime)*24*60*60:0:3,'sec R',totalTimeResampling*24*60*60:0:3,'sec P',totalTimePolishing*24*60*60:0:3,'sec');
    {$ifdef showTempImages} readln; {$endif}
  end;


PROCEDURE testAntiAlias(subcomplex:longint; name:string; translateX,translateY,rotate,distToEye:single);
  CONST numberOfSamples=16;
  VAR pic:T_FloatMap;
      i:longint;

      darts:T_darts;
  begin
    writeln('generating file ',name);
    pic.create(xres,yres);
    light:=normed(newVector(1,2,0));
    recurseThreshold:=0;
    maxImproveRuns:=0;
    refineThreshold:=0.87;
    darts.create(numberOfSamples);
    for i:=0 to numberOfSamples-1 do begin



      proj.reInit(distToEye*newVector(system.sin(rotate*pi/2{+0.3}),0.4,system.cos(rotate*pi/2{+0.3}))+newVector(translateX,0,translateY), //eyepoint
                                                                                                   newVector(translateX,0,translateY), //look-at
                                                                                                   xres,yres,45,darts[i,0],darts[i,1]);
      zmap.clear(black,1E20);
      write(i:3,' ');
      throwPixels(1/16,false);
      zmap.incBitmap(pic,-0.04,black,extractFileExt(name)<>'.vraw');
      save('intermediate'+intToStr(i)+'.vraw');
    end;
    pic.multiplyWith(1/numberOfSamples);
    pic.saveToFile(name);
    backgroundDisplay(name);
    pic.destroy;
    darts.destroy;
  end;

FUNCTION twoDigit(i:longint):string;
  begin result:=intToStr(i); if i<10 then result:='0'+result; end;
CONST zoom:array[0..49] of single=(0.25            ,0.24316373685307,0.2365144116814 ,0.23004691265622,0.22375626773199,
                                   0.21763764082403,0.21168632809063,0.20589775431689,0.20026746939741,0.19479114491512,
                                   0.1894645708138 ,0.18428365216139,0.17924440600198,0.1743429582938 ,0.16957554093096,
                                   0.16493848884661,0.16042823719536,0.1560413186127 ,0.15177436054938,0.14762408267869,
                                   0.14358729437463,0.13966089225903,0.13584185781576,0.13212725507017,0.12851422833201,
                                   0.125           ,0.12158186842654,0.1182572058407 ,0.11502345632811,0.111878133866  ,
                                   0.10881882041202,0.10584316404532,0.10294887715845,0.1001337346987 ,0.09739557245756,
                                   0.0947322854069 ,0.09214182608069,0.08962220300099,0.0871714791469 ,0.08478777046548,
                                   0.08246924442331,0.08021411859768,0.07802065930635,0.07588718027469,0.07381204133935,
                                   0.07179364718731,0.06983044612951,0.06792092890788,0.06606362753509,0.064257114166);

begin
  initMap(0);
  randomize;
  zmap.create(xres,yres); pt:=zmap.rawData;
  proj.create(newVector(0.3,0.3,0.3),newVector(0,0,0),xres,yres,45);
  refineThreshold:=1;

  phase:=0; complexity:=10;
  for i:=10 to 10 do begin
    complexity:=i;
    {if not(fileExists('mountains'+twoDigit(i)+'.vraw')) then} testAntiAlias(0,'mountains'+twoDigit(i)+'.vraw',0,0,0,2);
  end;



  proj.destroy;
  zmap.destroy;
end.
