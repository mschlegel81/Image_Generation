PROGRAM orography;
USES {$ifdef UNIX}cmem,cthreads,{$endif}zbuf,mypics,process,sysutils,math;
{$fputype sse3}
{$define showProgress}
{define showTempImages}
CONST xres=7620;
      yres=5080;
      //light:T_floatColor=(sqrt(1/2),sqrt(1/2),0);
      //light:T_floatColor=(0,1,0);
      expectedHeight=0.25;

VAR zmap:T_zbufferedMap;
    proj:T_3DProjection;
    pt  :P_zCol;
    sampleCount:longint;
    refineThreshold:single=1.5;
    recurseThreshold:longint=1;

    specPow:longint=0;
    maxImproveRuns:longint=4;

    phase:single=0;
    light:T_floatColor;

    scenIndex:byte=0;

PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.Create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif} 'display '+ps;
    tempProcess.execute;
    tempProcess.Free;
  end;

PROCEDURE save(fname:string);
  begin
    zmap.saveBitmap(fname,-0.04,white*0.7);
    backgroundDisplay(fname);
    sleep(200);
  end;




FUNCTION throwPixel(cx,cy:single; OUT screenX,screenY:single):boolean; inline;
  CONST a=0.2;
        b=10;
  VAR rad,radX,radY,amplitude:single;
      p,n:T_floatColor;
      kx,ky,k:longint;
      doItRed:boolean;

  begin
    inc(sampleCount);
    case scenIndex of
      0: begin
           rad :=sqrt(       cx*cx+cy*cy);
           radX:=sqrt(sqr(cx+1E-3)+cy*cy);
           radY:=sqrt(cx*cx+sqr(cy+1E-3));
           p[0]:=cx;
           p[1]:=a*cos(phase+b*rad)/(1+rad*rad);
           p[2]:=cy;
           n[0]:=a*cos(phase+b*radX)/(1+radX*radX)-p[1];//+1E-4*(0.5-random);
           n[1]:=1E-3;
           n[2]:=a*cos(phase+b*radY)/(1+radY*radY)-p[1];//+1E-4*(0.5-random);
           n:=normed(n);
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY>0 then for kx:=0 to specpow do radY:=radY*radY
                     else radY:=0;
           result:=proj.throwPixelToMap(p,blue*(0.5+0.5*rad)+white*rady,pt,screenX,screenY);
         end;
      1,2: begin
           p[0]:=cx;
           p[2]:=cy;
           n[1]:=1E-3;
           kx:=round(cx); cx:=(cx-kx)*2;
           ky:=round(cy); cy:=(cy-ky)*2;
           rad :=(       cx*cx+cy*cy); if rad >1 then rad :=1;
           radX:=(sqr(cx+1E-3)+cy*cy); if radX>1 then radX:=1;
           radY:=(cx*cx+sqr(cy+1E-3)); if radY>1 then radY:=1;
           if odd(kx) xor odd(ky) xor odd(scenIndex) then begin
             p[1]:=0.5*sqrt(1-rad);
             n[0]:=0.5*sqrt(1-radX)-p[1];
             n[2]:=0.5*sqrt(1-radY)-p[1];
           end else begin
             p[1]:=-0.5*sqrt(1-rad);
             n[0]:=-0.5*sqrt(1-radX)-p[1];
             n[2]:=-0.5*sqrt(1-radY)-p[1];
           end;
           n:=normed(n);
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY<0.99 then radY:=0;
           result:=proj.throwPixelToMap(p,white*(0.25+0.25*rad)+white*rady,pt,screenX,screenY);
         end;
      3: begin
           p[0]:=cx;
           p[2]:=cy;
           n[1]:=1E-3;
           p[1]:=0;
           n[0]:=0;
           n[2]:=0;
           amplitude:=0.5;
           doItRed:=true;
           for k:=0 to 5 do begin
             kx:=round(cx); cx:=(cx-kx)*2;
             ky:=round(cy); cy:=(cy-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(       cx*cx+cy*cy); if rad >1 then rad :=1 else doItRed:=odd(k);
               radX:=(sqr(cx+1E-3)+cy*cy); if radX>1 then radX:=1 else doItRed:=odd(k);
               radY:=(cx*cx+sqr(cy+1E-3)); if radY>1 then radY:=1 else doItRed:=odd(k);



               rad:=amplitude*sqrt(1-rad);
               p[1]:=p[1]+rad;
               n[0]:=n[0]+amplitude*sqrt(1-radX)-rad;
               n[2]:=n[2]+amplitude*sqrt(1-radY)-rad;
             end;
             amplitude:=amplitude/4;
             cx:=cx*2+1;
             cy:=cy*2+1;
           end;
           doItRed:=false;

           n:=normed(n);
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY>0 then for k:=0 to 4 do radY:=radY*radY
                     else rady:=0;
           if doItRed then result:=proj.throwPixelToMap(p,red  *(0.5 +0.5 *rad)+white*rady,pt,screenX,screenY)
                      else result:=proj.throwPixelToMap(p,white*(0.25+0.25*rad)+white*rady,pt,screenX,screenY);
         end;
      4: begin
           n[1]:=1E-5;
           p[0]:=cx+1E-5; p[2]:=cy; p[1]:=0;
           amplitude:=0.5;
           for k:=0 to 10 do begin
             kx:=round(p[0]); p[0]:=(p[0]-kx)*2;
             ky:=round(p[2]); p[2]:=(p[2]-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(p[0]*p[0]+p[2]*p[2]);       
               if rad<1 then p[1]:=p[1]+(sqrt(1-rad))*amplitude;
             end;
             amplitude:=amplitude/4; p[0]:=p[0]*2+1; p[2]:=p[2]*2+1;
           end;
           n[0]:=p[1];
           
           p[0]:=cx; p[2]:=cy+1E-5; p[1]:=0;
           amplitude:=0.5;
           for k:=0 to 10 do begin
             kx:=round(p[0]); p[0]:=(p[0]-kx)*2;
             ky:=round(p[2]); p[2]:=(p[2]-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(p[0]*p[0]+p[2]*p[2]);       
               if rad<1 then p[1]:=p[1]+(sqrt(1-rad))*amplitude;
             end;
             amplitude:=amplitude/4; p[0]:=p[0]*2+1; p[2]:=p[2]*2+1;
           end;
           n[2]:=p[1];

           p[0]:=cx; p[2]:=cy; p[1]:=0;
           amplitude:=0.5;
           for k:=0 to 10 do begin
             kx:=round(p[0]); p[0]:=(p[0]-kx)*2;
             ky:=round(p[2]); p[2]:=(p[2]-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(p[0]*p[0]+p[2]*p[2]);       
               if rad<1 then p[1]:=p[1]+(sqrt(1-rad))*amplitude;
             end;
             amplitude:=amplitude/4; p[0]:=p[0]*2+1; p[2]:=p[2]*2+1;
           end;
           p[0]:=cx; p[2]:=cy; 
           
           n[0]:=n[0]-p[1];
           n[2]:=n[2]-p[1];

           n:=normed(n);
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY>0 then for k:=0 to 2 do radY:=radY*radY
                     else rady:=0;
           result:=proj.throwPixelToMap(p,white  *(0.25 +0.25 *rad)+radY*red+radY*radY*green+radY*radY*radY*blue,pt,screenX,screenY);
         end;

      end;//case





  end;




FUNCTION throwPixel(cx,cy:single):boolean; inline;
  begin result:=throwPixel(cx,cy,cx,cy); end;

FUNCTION resample:longint;
  VAR ix,iy:longint;
      n0,n1,n2,n3:boolean;
      p0,p1:T_floatColor;
  begin
    {$ifdef showProgress} write('R'); {$endif}
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
  end;

FUNCTION polish:longint;
  CONST thr=1E-6;
  VAR ix,iy:longint;
      n0,n1,n2,n3:boolean;
      p0,p1:T_floatColor;
      zhere:single;
  begin
    {$ifdef showProgress} write('P'); {$endif}
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
  end;


PROCEDURE throwPixels(quality:single; improve:boolean);
  VAR ix,iy,x0,x1,jx,jy,k:longint;
      fx,fy:single;
      sc:array[0..8,0..8] of record x,y:single; ready:boolean; end;
      p:T_floatColor;
      thrown,thrownInLine:boolean;
      startTime:double;

  PROCEDURE renderQuad(kx0,kx1,ky0,ky1:longint);
    VAR kx,ky,jx,jy,jxs,jys:longint;
    begin
      jxs:=ceil(sqrt(max(sqr(sc[kx0,ky0].x-sc[kx1,ky0].x)+sqr(sc[kx0,ky0].y-sc[kx1,ky0].y),
                         sqr(sc[kx0,ky1].x-sc[kx1,ky1].x)+sqr(sc[kx0,ky1].y-sc[kx1,ky1].y))/refineThreshold));
      jys:=ceil(sqrt(max(sqr(sc[kx0,ky0].x-sc[kx0,ky1].x)+sqr(sc[kx0,ky0].y-sc[kx0,ky1].y),
                         sqr(sc[kx1,ky0].x-sc[kx1,ky1].x)+sqr(sc[kx1,ky0].y-sc[kx1,ky1].y))/refineThreshold));
      if ((jxs>recurseThreshold) or (jys>recurseThreshold)) and (kx1>kx0+1) then begin
        kx:=(kx0+kx1) shr 1;
        ky:=(ky0+ky1) shr 1;
        if not(sc[kx0,ky ].ready) then begin proj.screenToRealLevel(p,(ix+kx0*0.125)/quality,(iy+ky *0.125)/quality,expectedHeight); throwPixel(p[0],p[2],sc[kx0,ky ].x,sc[kx0,ky ].y); sc[kx0,ky ].ready:=true; end;
        if not(sc[kx ,ky0].ready) then begin proj.screenToRealLevel(p,(ix+kx *0.125)/quality,(iy+ky0*0.125)/quality,expectedHeight); throwPixel(p[0],p[2],sc[kx ,ky0].x,sc[kx ,ky0].y); sc[kx ,ky0].ready:=true; end;
        if not(sc[kx ,ky ].ready) then begin proj.screenToRealLevel(p,(ix+kx *0.125)/quality,(iy+ky *0.125)/quality,expectedHeight); throwPixel(p[0],p[2],sc[kx ,ky ].x,sc[kx ,ky ].y); sc[kx ,ky ].ready:=true; end;
        if not(sc[kx ,ky1].ready) then begin proj.screenToRealLevel(p,(ix+kx *0.125)/quality,(iy+ky1*0.125)/quality,expectedHeight); throwPixel(p[0],p[2],sc[kx ,ky1].x,sc[kx ,ky1].y); sc[kx ,ky1].ready:=true; end;
        if not(sc[kx1,ky ].ready) then begin proj.screenToRealLevel(p,(ix+kx1*0.125)/quality,(iy+ky *0.125)/quality,expectedHeight); throwPixel(p[0],p[2],sc[kx1,ky ].x,sc[kx1,ky ].y); sc[kx1,ky ].ready:=true; end;
        renderQuad(kx0,kx,ky0,ky);
        renderQuad(kx,kx1,ky0,ky);
        renderQuad(kx0,kx,ky,ky1);
        renderQuad(kx,kx1,ky,ky1);
      end else begin
        for jy:=0 to jys-1 do begin
          fy:=(ky0+jy*(ky1-ky0)/jys)*0.125;
          for jx:=0 to jxs-1 do if (jx>0) or (jy>0) then begin
            fx:=(kx0+jx*(kx1-kx0)/jxs)*0.125;
            proj.screenToRealLevel(p,(ix+fx)/quality,(iy+fy)/quality,expectedHeight);
            throwPixel(p[0],p[2]);
          end;
        end;
      end;
    end;

  begin
    startTime:=now;
    sampleCount:=0;
    x0:=0; x1:=round(xres*quality);
    iy:=0;
    repeat
      {$ifdef showProgress} write('s'); {$endif}
      dec(iy);
      thrownInLine:=false;
      k:=(x0+x1) shr 1;
      ix:=k;
      repeat
        for jx:=0 to 8 do for jy:=0 to 8 do sc[jy,jx].ready:=((jx=0) or (jx=8)) and ((jy=0) or (jy=8));
        proj.screenToRealLevel(p, ix   /quality, iy   /quality,expectedHeight); thrown:=throwPixel(p[0],p[2],sc[0,0].x,sc[0,0].y);
        proj.screenToRealLevel(p,(ix+1)/quality, iy   /quality,expectedHeight); if      throwPixel(p[0],p[2],sc[8,0].x,sc[8,0].y) then thrown:=true;
        proj.screenToRealLevel(p, ix   /quality,(iy+1)/quality,expectedHeight); if      throwPixel(p[0],p[2],sc[0,8].x,sc[0,8].y) then thrown:=true;
        proj.screenToRealLevel(p,(ix+1)/quality,(iy+1)/quality,expectedHeight); if      throwPixel(p[0],p[2],sc[8,8].x,sc[8,8].y) then thrown:=true;
         if thrown then begin renderQuad(0,8,0,8); thrownInLine:=true; x0:=ix; end;
        dec(ix);
      until not(thrown) and (sc[0,0].x<0);
      ix:=k+1;
      repeat
        for jx:=0 to 8 do for jy:=0 to 8 do sc[jy,jx].ready:=((jx=0) or (jx=8)) and ((jy=0) or (jy=8));
        proj.screenToRealLevel(p, ix   /quality, iy   /quality,expectedHeight); thrown:=throwPixel(p[0],p[2],sc[0,0].x,sc[0,0].y);
        proj.screenToRealLevel(p,(ix+1)/quality, iy   /quality,expectedHeight); if      throwPixel(p[0],p[2],sc[8,0].x,sc[8,0].y) then thrown:=true;
        proj.screenToRealLevel(p, ix   /quality,(iy+1)/quality,expectedHeight); if      throwPixel(p[0],p[2],sc[0,8].x,sc[0,8].y) then thrown:=true;
        proj.screenToRealLevel(p,(ix+1)/quality,(iy+1)/quality,expectedHeight); if      throwPixel(p[0],p[2],sc[8,8].x,sc[8,8].y) then thrown:=true;
        if thrown then begin renderQuad(0,8,0,8); thrownInLine:=true; x0:=ix; end;
        inc(ix);
      until not(thrown) and (sc[8,8].x>xres);

      {$ifdef showTempImages} save('temp.vraw'); {$endif}
      k:=0; if improve and thrownInLine  then repeat inc(k) until (resample<xres+xres) or (k>maxImproveRuns);
    until not(thrownInLine) and (sc[0,0].y<0) and (sc[8,0].y<0) and (sc[0,8].y<0) and (sc[8,8].y<0);

    iy:=0;
    repeat
      {$ifdef showProgress} write('S'); {$endif}
      thrownInLine:=false;
      k:=(x0+x1) shr 1;
      ix:=k;
      repeat
        for jx:=0 to 8 do for jy:=0 to 8 do sc[jy,jx].ready:=((jx=0) or (jx=8)) and ((jy=0) or (jy=8));
        proj.screenToRealLevel(p, ix   /quality, iy   /quality,expectedHeight); thrown:=throwPixel(p[0],p[2],sc[0,0].x,sc[0,0].y);
        proj.screenToRealLevel(p,(ix+1)/quality, iy   /quality,expectedHeight); if      throwPixel(p[0],p[2],sc[8,0].x,sc[8,0].y) then thrown:=true;
        proj.screenToRealLevel(p, ix   /quality,(iy+1)/quality,expectedHeight); if      throwPixel(p[0],p[2],sc[0,8].x,sc[0,8].y) then thrown:=true;
        proj.screenToRealLevel(p,(ix+1)/quality,(iy+1)/quality,expectedHeight); if      throwPixel(p[0],p[2],sc[8,8].x,sc[8,8].y) then thrown:=true;
        if thrown then begin renderQuad(0,8,0,8); thrownInLine:=true; x0:=ix; end;
        dec(ix);
      until not(thrown) and (sc[0,0].x<0);
      ix:=k+1;
      repeat
        for jx:=0 to 8 do for jy:=0 to 8 do sc[jy,jx].ready:=((jx=0) or (jx=8)) and ((jy=0) or (jy=8));
        proj.screenToRealLevel(p, ix   /quality, iy   /quality,expectedHeight); thrown:=throwPixel(p[0],p[2],sc[0,0].x,sc[0,0].y);
        proj.screenToRealLevel(p,(ix+1)/quality, iy   /quality,expectedHeight); if      throwPixel(p[0],p[2],sc[8,0].x,sc[8,0].y) then thrown:=true;
        proj.screenToRealLevel(p, ix   /quality,(iy+1)/quality,expectedHeight); if      throwPixel(p[0],p[2],sc[0,8].x,sc[0,8].y) then thrown:=true;
        proj.screenToRealLevel(p,(ix+1)/quality,(iy+1)/quality,expectedHeight); if      throwPixel(p[0],p[2],sc[8,8].x,sc[8,8].y) then thrown:=true;
        if thrown then begin renderQuad(0,8,0,8); thrownInLine:=true; x1:=ix; end;
        inc(ix);
      until not(thrown) and (sc[8,8].x>xres);
      k:=0; if improve and thrownInLine then repeat inc(k) until (resample<xres+xres) or (k>maxImproveRuns);
      inc(iy);
      {$ifdef showTempImages} save('temp.vraw'); {$endif}
    until not(thrownInLine) and (sampleCount>xres*yres);
    k:=0; if improve then repeat inc(k) until (polish<xres) or (k>=maxImproveRuns);
    writeln(sampleCount/(xres*yres):0:5,'spp ',(now-startTime)*24*60*60:0:3,'sec');
  end;



PROCEDURE animate;
  VAR i:longint;
  begin
    maxImproveRuns:=10;
    recurseThreshold:=16;
    light:=normed(newVector(1,1,0));
    specPow:=6;
    scenIndex:=3;
    for i:=1 to 100 do begin
      proj.reInit(newVector(1.2+i*0.01,0.5+i*0.01,1.2+i*0.01),newVector(1,0.5,1),xres,yres,45);
      zmap.clear(black,1E20);
      write(i:3,' '); throwPixels(1/32,false);
      save('bumps'+copy(intToStr(1000+i),2,3)+'.jpg');
    end;
  end;

PROCEDURE testAntiAlias;
  VAR //pic:T_FloatMap;
      i:longint;
  begin
    //pic.create(xres,yres);
    recurseThreshold:=6;
    light:=normed(newVector(1,1,0));
    specPow:=6;
    phase:=0;
    scenIndex:=4;
    maxImproveRuns:=0;
    for i:=0 to 15 do begin

      proj.reInit(0.05*newVector(1,0.5,1.2)+newVector(1.25,0.5,1),newVector(1.25,0.5,1),xres,yres,45,random-0.5,random-0.5);
      zmap.clear(black,1E20);
      write(i:3,' ');
      throwPixels(1/8,true);

    //  zmap.incBitmap(pic,-0.01,white*0.7);
      save('r7620s'+copy(intToStr(100+i),2,2)+'.vraw');
    end;
    //pic.multiplyWith(1/16);
    //pic.saveToFile('repetition_7620.vraw');
    //pic.destroy;
  end;

begin
  randomize;
  zmap.create(xres,yres); pt:=zmap.rawData;
  proj.create(newVector(0.3,0.3,0.3),newVector(0,0,0),xres,yres,45);
  refineThreshold:=0.5;
//  specPow:=6;
//  animate;
   testAntiAlias;

{                     zmap.clear(black,1E20); throwPixels(1/16,false); save('spec0.jpg');
  maxImproveRuns:=1; zmap.clear(black,1E20); throwPixels(1/16,true); save('spec1.jpg');
  maxImproveRuns:=2; zmap.clear(black,1E20); throwPixels(1/16,true); save('spec2.jpg');
  maxImproveRuns:=4; zmap.clear(black,1E20); throwPixels(1/16,true); save('spec3.jpg');
  maxImproveRuns:=8; zmap.clear(black,1E20); throwPixels(1/16,true); save('spec4.jpg');
  maxImproveRuns:=16; zmap.clear(black,1E20); throwPixels(1/16,true); save('spec5.jpg');
  maxImproveRuns:=32; zmap.clear(black,1E20); throwPixels(1/16,true); save('spec6.jpg');
  maxImproveRuns:=64; zmap.clear(black,1E20); throwPixels(1/16,true); save('spec7.jpg');
  maxImproveRuns:=128; zmap.clear(black,1E20); throwPixels(1/16,true); save('spec8.jpg');
  maxImproveRuns:=256; zmap.clear(black,1E20); throwPixels(1/16,true); save('spec9.jpg');   }



  //recurseThreshold:=1; zmap.clear(black,1E20); throwPixels(1/16,false); save('ref1.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true); save('ref1i.jpg');
  //recurseThreshold:=2; zmap.clear(black,1E20); throwPixels(1/16,false); save('ref2.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true); save('ref2i.jpg');
  //recurseThreshold:=3; zmap.clear(black,1E20); throwPixels(1/16,false); save('ref3.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true); save('ref3i.jpg');
  //recurseThreshold:=4; zmap.clear(black,1E20); throwPixels(1/16,false); save('ref4.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true); save('ref4i.jpg');
  //recurseThreshold:=5; zmap.clear(black,1E20); throwPixels(1/16,false); save('ref5.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true); save('ref5i.jpg');
  //recurseThreshold:=6; zmap.clear(black,1E20); throwPixels(1/16,false); save('ref6.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true); save('ref6i.jpg');
  //recurseThreshold:=7; zmap.clear(black,1E20); throwPixels(1/16,false); save('ref7.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true); save('ref7i.jpg');
  //recurseThreshold:=8; zmap.clear(black,1E20); throwPixels(1/16,false); save('ref8.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true); save('ref8i.jpg');
  //recurseThreshold:=9; zmap.clear(black,1E20); throwPixels(1/16,false); save('ref9.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true); save('ref9i.jpg');
  //writeln;

  //readln;
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.9,false); save('oro19.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.8,false); save('oro18.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.7,false); save('oro17.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.6,false); save('oro16.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.5,false); save('oro15.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.4,false); save('oro14.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.3,false); save('oro13.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.2,false); save('oro12.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.1,false); save('oro11.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.09,false); save('oro10.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.08,false); save('oro09.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.07,false); save('oro08.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.06,false); save('oro07.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.05,false); save('oro06.jpg');
 // proj.reinit(newVector(sqrt(1/2),1,sqrt(1/2)),newVector(0,0,0),xres,yres,45); zmap.clear(black,1E20); throwPixels(0.04,false); save('oro05.jpg');


//  zmap.clear(black,1E20); throwPixels(1/16,false);  zmap.saveBitmap('orography1a.jpg'); backgroundDisplay('orography1a.jpg'); zmap.clear(black,1E20); throwPixels(1/16,true);  zmap.saveBitmap('orography1b.jpg'); backgroundDisplay('orography1b.jpg');
//  zmap.clear(black,1E20); throwPixels(1/8 ,false);  zmap.saveBitmap('orography2a.jpg'); backgroundDisplay('orography2a.jpg'); zmap.clear(black,1E20); throwPixels(1/8 ,true);  zmap.saveBitmap('orography2b.jpg'); backgroundDisplay('orography2b.jpg');
//  zmap.clear(black,1E20); throwPixels(1/4 ,false);  zmap.saveBitmap('orography3a.jpg'); backgroundDisplay('orography3a.jpg'); zmap.clear(black,1E20); throwPixels(1/4 ,true);  zmap.saveBitmap('orography3b.jpg'); backgroundDisplay('orography3b.jpg');
//  zmap.clear(black,1E20); throwPixels(1/2 ,false);  zmap.saveBitmap('orography4a.jpg'); backgroundDisplay('orography4a.jpg'); zmap.clear(black,1E20); throwPixels(1/2 ,true);  zmap.saveBitmap('orography4b.jpg'); backgroundDisplay('orography4b.jpg');
//  zmap.clear(black,1E20); throwPixels(1   ,false);  zmap.saveBitmap('orography5a.jpg'); backgroundDisplay('orography5a.jpg'); zmap.clear(black,1E20); throwPixels(1   ,true);  zmap.saveBitmap('orography5b.jpg'); backgroundDisplay('orography5b.jpg');
//  zmap.clear(black,1E20); throwPixels(2   ,false);  zmap.saveBitmap('orography6a.jpg'); backgroundDisplay('orography6a.jpg'); zmap.clear(black,1E20); throwPixels(2   ,true);  zmap.saveBitmap('orography6b.jpg'); backgroundDisplay('orography6b.jpg');
  proj.destroy;
  zmap.destroy;
end.
