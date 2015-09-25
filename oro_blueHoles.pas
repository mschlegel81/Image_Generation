PROGRAM orography;
USES {$ifdef UNIX}cmem,cthreads,{$endif}zbuf,mypics,sysutils,math,complex,cmdLineParseUtil;
{$fputype sse3}
VAR //xres=2048; yres=1152;
      xres:longint=800; yres:longint=600;
      resultFileName:string='';
      //light:T_floatColor=(sqrt(1/2),sqrt(1/2),0);
      //light:T_floatColor=(0,1,0);


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
    complexity:longint=0;
    expectedHeight:single=0;

    heightSamples:longint=1;
    heightSum    :single =0;

    totalTimeResampling:double;
    totalTimePolishing:double;

PROCEDURE save(fname:string);
  begin
    zmap.saveBitmap(fname,-0.01,black);

    sleep(100);

  end;


FUNCTION throwPixel(cx,cy:single; OUT screenX,screenY:single; OUT foreground:boolean):boolean; inline;
  CONST a=0.2;
        b=10;

  PROCEDURE cube(x,y:single; OUT p,n:T_floatColor);
    begin
      if (x<-1) or (x>1) or (y<-1) or (y>1) then begin
        //non-cube
        p[0]:=x; p[1]:=0; p[2]:=y;
        n[0]:=0; n[1]:=1; n[2]:=0;
      end else if x<-0.5 then begin
        if y<-0.5     then begin p[0]:=-1; p[1]:=4+4*x; p[2]:=-1;   n[0]:=-1; n[1]:=0; n[2]:=0; end
        else if y<0.5 then begin p[0]:=-1; p[1]:=4+4*x; p[2]:=2*y;  n[0]:=-1; n[1]:=0; n[2]:=0; end
        else               begin p[0]:=-1; p[1]:=4+4*x; p[2]:=1;    n[0]:=-1; n[1]:=0; n[2]:=0; end;
      end else if x<0.5 then begin
        if y<-0.5     then begin p[0]:=2*x; p[1]:=4+4*y; p[2]:=-1;  n[0]:=0; n[1]:=0; n[2]:=-1; end
        else if y<0.5 then begin p[0]:=2*x; p[1]:=2;     p[2]:=2*y; n[0]:=0; n[1]:=1; n[2]:= 0; end
        else               begin p[0]:=2*x; p[1]:=4-4*y; p[2]:=1;   n[0]:=0; n[1]:=0; n[2]:= 1; end;
      end else begin
        if y<-0.5     then begin p[0]:=1; p[1]:=4-4*x; p[2]:=-1;    n[0]:=1; n[1]:=0; n[2]:=0; end
        else if y<0.5 then begin p[0]:=1; p[1]:=4-4*x; p[2]:=2*y;   n[0]:=1; n[1]:=0; n[2]:=0; end
        else               begin p[0]:=1; p[1]:=4-4*x; p[2]:=1;     n[0]:=1; n[1]:=0; n[2]:=0; end;
      end;
    end;

  FUNCTION recCube(x,y:single; depth:byte; VAR p,n:T_floatColor):boolean;
    begin
      if (x>-1) and (x<1) and (y>-1) and (y<1) then begin
        result:=true;
        if x<-0.5 then begin
          if y<-0.5     then begin p[0]:=-1; p[1]:=4+4*x; p[2]:=-1;   n[0]:=-1; n[1]:=0; n[2]:=0; end
          else if y<0.5 then begin p[0]:=-1; p[1]:=4+4*x; p[2]:=2*y;  n[0]:=-1; n[1]:=0; n[2]:=0; end
          else               begin p[0]:=-1; p[1]:=4+4*x; p[2]:=1;    n[0]:=-1; n[1]:=0; n[2]:=0; end;
        end else if x<0.5 then begin
          if y<-0.5     then begin p[0]:=2*x; p[1]:=4+4*y; p[2]:=-1;  n[0]:=0; n[1]:=0; n[2]:=-1; end
          else if y<0.5 then begin p[0]:=2*x; p[1]:=2;     p[2]:=2*y; n[0]:=0; n[1]:=1; n[2]:= 0; end
          else               begin p[0]:=2*x; p[1]:=4-4*y; p[2]:=1;   n[0]:=0; n[1]:=0; n[2]:= 1; end;
        end else begin
          if y<-0.5     then begin p[0]:=1; p[1]:=4-4*x; p[2]:=-1;    n[0]:=1; n[1]:=0; n[2]:=0; end
          else if y<0.5 then begin p[0]:=1; p[1]:=4-4*x; p[2]:=2*y;   n[0]:=1; n[1]:=0; n[2]:=0; end
          else               begin p[0]:=1; p[1]:=4-4*x; p[2]:=1;     n[0]:=1; n[1]:=0; n[2]:=0; end;
        end;
      end else if (depth>0) and (x>-3) and (x<3) and (y>-3) and (y<3) then begin
        //recursive call and conditional transformation of updated coordinates
        if recCube(2*x+3,2*y,depth-1,p,n) then begin result:=true; p[0]:=0.5*(p[0]-3); p[1]:=0.5*p[1]; p[2]:=0.5*p[2];     end else
        if recCube(2*x-3,2*y,depth-1,p,n) then begin result:=true; p[0]:=0.5*(p[0]+3); p[1]:=0.5*p[1]; p[2]:=0.5*p[2];     end else
        if recCube(2*x,2*y+3,depth-1,p,n) then begin result:=true; p[0]:=0.5*p[0];     p[1]:=0.5*p[1]; p[2]:=0.5*(p[2]-3); end else
        if recCube(2*x,2*y-3,depth-1,p,n) then begin result:=true; p[0]:=0.5*p[0];     p[1]:=0.5*p[1]; p[2]:=0.5*(p[2]+3); end else
        result:=false;
      end else result:=false;
    end;

  FUNCTION recBump(x,y:single; depth:byte):single;
    CONST dx:array[0..2] of single=(system.cos(0),system.cos(2*pi/3),system.cos(4*pi/3));
          dy:array[0..2] of single=(system.sin(0),system.sin(2*pi/3),system.sin(4*pi/3));
    VAR rad:single;
    begin
      rad:=x*x+y*y;
      if rad<1 then result:=system.sqr(1-rad) else result:=0;
      if (depth>0) and ((rad<system.sqr(phase)) or (rad<1)) then result:=result+0.5*(recBump(2*x+phase*dx[0],2*y+phase*dy[0],depth-1)
                                                                           +recBump(2*x+phase*dx[1],2*y+phase*dy[1],depth-1)
                                                                           +recBump(2*x+phase*dx[2],2*y+phase*dy[2],depth-1));
    end;

//  FUNCTION pyramid(x,y:single; depth:byte):single;
//    VAR i:longint;
//        z:single;
//    begin
////      result:=max(0,min(1,(1-max(abs(x),abs(y)))));
//    if (x>-1) and (x<1) and (y>-1) and (y<1) then begin
      //result:=max(0,1-max(abs(x),abs(y)));
      //
//      if (depth>0) then
////        result:=result+0.5*(pyramid(2*x-1,2*y-1,depth-1)
//                           +pyramid(2*x+1,2*y-1,depth-1)
//                           +pyramid(2*x-1,2*y+1,depth-1)
//                           +pyramid(2*x+1,2*y+1,depth-1)*phase);
//          //result:=result+phase*(pyramid(x+y-1,y-x-1,depth-1)
//          //                     +pyramid(x+y+1,y-x-1,depth-1)
//          //                     +pyramid(x+y-1,y-x+1,depth-1)
//          //                      +pyramid(x+y+1,y-x+1,depth-1));
//      end else result:=0;
//    end;

  VAR rad,radX,radY,amplitude:single;
      p,n:T_floatColor;
      kx,ky,k:longint;
      sample:array[0..2] of T_floatColor;

  begin
    inc(sampleCount);
    case scenIndex of
      0: begin
           rad :=sqrt(       cx*cx+cy*cy);
           radX:=sqrt(system.sqr(cx+1E-3)+cy*cy);
           radY:=sqrt(cx*cx+system.sqr(cy+1E-3));
           p[0]:=cx;
           p[1]:=a*system.cos(phase+b*rad)/(1+rad*rad);
           p[2]:=cy;
           n[0]:=a*system.cos(phase+b*radX)/(1+radX*radX)-p[1];//+1E-4*(0.5-random);
           n[1]:=1E-3;
           n[2]:=a*system.cos(phase+b*radY)/(1+radY*radY)-p[1];//+1E-4*(0.5-random);
           n:=normed(n);
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY>0 then for kx:=0 to specpow do radY:=radY*radY
                     else radY:=0;
           proj.throwPixelToMap(p,blue*(0.5+0.5*rad)+white*rady,pt,screenX,screenY,result,foreground);
         end;
      1,2: begin
           sample[0,0]:=cx; sample[1,0]:=cx+1E-2; sample[2,0]:=cx;
           sample[0,2]:=cy; sample[1,2]:=cy;      sample[2,2]:=cy+1E-2;
           for k:=0 to 2 do begin
             rad :=sample[k,0]*sample[k,0]+sample[k,2]*sample[k,2];
             if rad>1 then sample[k,1]:=0
             else begin
               radX:=2/(1+rad);
               sample[k,0]:=sample[k,0]*radX;
               sample[k,2]:=sample[k,2]*radX;
               sample[k,1]:=sqrt(max(0,1-rad*radX*radX));
             end;
           end;
           p:=sample[0];
           n:=(cross(sample[1]-sample[0],sample[2]-sample[0]));
           if scenindex=1 then n[1]:=-n[1]
                          else p[1]:=-p[1];


           rad:=n[0]*n[0]+n[1]*n[1]+n[2]*n[2];
           if rad>1E-12 then n:=n*(1/sqrt(rad));
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY<0.99 then radY:=0;
           proj.throwPixelToMap(p,white*(0.25+0.25*rad)+white*rady,pt,screenX,screenY,result,foreground);
         end;
      3: begin
           n[1]:=1E-1;
           for k:=0 to complexity do n[1]:=n[1]*0.25;

           p[0]:=cx+n[1]; p[2]:=cy; p[1]:=0;
           amplitude:=0.5;
           for k:=0 to complexity do begin
             kx:=round(p[0]); p[0]:=(p[0]-kx)*2;
             ky:=round(p[2]); p[2]:=(p[2]-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(p[0]*p[0]+p[2]*p[2]);
               if rad<1 then p[1]:=p[1]+(0.5-0.5*system.cos(pi*(1-sqrt(rad))))*amplitude;
             end;
             amplitude:=amplitude*0.25; p[0]:=p[0]*2+phase; p[2]:=p[2]*2;
           end;
           n[0]:=p[1];

           p[0]:=cx; p[2]:=cy+n[1]; p[1]:=0;
           amplitude:=0.5;
           for k:=0 to complexity do begin
             kx:=round(p[0]); p[0]:=(p[0]-kx)*2;
             ky:=round(p[2]); p[2]:=(p[2]-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(p[0]*p[0]+p[2]*p[2]);
               if rad<1 then p[1]:=p[1]+(0.5-0.5*system.cos(pi*(1-sqrt(rad))))*amplitude;
             end;
             amplitude:=amplitude*0.25; p[0]:=p[0]*2+phase; p[2]:=p[2]*2;
           end;
           n[2]:=p[1];

           p[0]:=cx; p[2]:=cy; p[1]:=0;
           amplitude:=0.5;
           for k:=0 to complexity do begin
             kx:=round(p[0]); p[0]:=(p[0]-kx)*2;
             ky:=round(p[2]); p[2]:=(p[2]-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(p[0]*p[0]+p[2]*p[2]);
               if rad<1 then p[1]:=p[1]+(0.5-0.5*system.cos(pi*(1-sqrt(rad))))*amplitude;
             end;
             amplitude:=amplitude*0.25; p[0]:=p[0]*2+phase; p[2]:=p[2]*2;
           end;
           p[0]:=cx; p[2]:=cy;

           n[0]:=n[0]-p[1];
           n[2]:=n[2]-p[1];

           n:=normed(n);
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY>0 then for k:=0 to 2 do radY:=radY*radY
                     else rady:=0;
           proj.throwPixelToMap(p,white  *(0.25 +0.25 *rad)+newVector(radY,radY*radY,radY*radY*radY),pt,screenX,screenY,result,foreground);
         end;
      4: begin
           n[1]:=1E-5;
           p[0]:=cx+1E-5; p[2]:=cy; p[1]:=0;
           amplitude:=0.5;
           for k:=0 to complexity do begin
             kx:=round(p[0]); p[0]:=(p[0]-kx)*2;
             ky:=round(p[2]); p[2]:=(p[2]-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(p[0]*p[0]+p[2]*p[2]);
               if rad<1 then p[1]:=p[1]+(sqrt(1-rad))*amplitude;
             end;
             amplitude:=amplitude/4; p[0]:=p[0]*2+phase; p[2]:=p[2]*2+phase;
           end;
           n[0]:=p[1];

           p[0]:=cx; p[2]:=cy+1E-5; p[1]:=0;
           amplitude:=0.5;
           for k:=0 to complexity do begin
             kx:=round(p[0]); p[0]:=(p[0]-kx)*2;
             ky:=round(p[2]); p[2]:=(p[2]-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(p[0]*p[0]+p[2]*p[2]);
               if rad<1 then p[1]:=p[1]+(sqrt(1-rad))*amplitude;
             end;
             amplitude:=amplitude/4; p[0]:=p[0]*2+phase; p[2]:=p[2]*2+phase;
           end;
           n[2]:=p[1];

           p[0]:=cx; p[2]:=cy; p[1]:=0;
           amplitude:=0.5;
           for k:=0 to complexity do begin
             kx:=round(p[0]); p[0]:=(p[0]-kx)*2;
             ky:=round(p[2]); p[2]:=(p[2]-ky)*2;
             if odd(kx) and odd(ky) then begin
               rad :=(p[0]*p[0]+p[2]*p[2]);
               if rad<1 then p[1]:=p[1]+(sqrt(1-rad))*amplitude;
             end;
             amplitude:=amplitude/4; p[0]:=p[0]*2+phase; p[2]:=p[2]*2+phase;
           end;
           p[0]:=cx; p[2]:=cy;

           n[0]:=n[0]-p[1];
           n[2]:=n[2]-p[1];

           n:=normed(n);
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY>0 then for k:=0 to 2 do radY:=radY*radY
                     else rady:=0;
           proj.throwPixelToMap(p,white  *(0.25 +0.25 *rad)+radY*red+radY*radY*green+radY*radY*radY*blue,pt,screenX,screenY,result,foreground);
         end;
      5: begin
           p[0]:=cx; p[1]:=0; p[2]:=cy;
           n[0]:=0;  n[1]:=1; n[2]:=0;
           reccube(cx,cy,complexity,p,n);
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY>0 then for k:=0 to 2 do radY:=radY*radY
                     else rady:=0;
           proj.throwPixelToMap(p,white  *(0.25 +0.25 *rad)+radY*red+radY*radY*green+radY*radY*radY*blue,pt,screenX,screenY,result,foreground);
         end;

      6: begin
           n[1]:=1E-3;
           p[0]:=cx; p[2]:=cy;
           p[1]:=0.5*system.sqr(1-4*phase*(system.sqr(cx     -round(cx     ))+system.sqr(cy     -round(cy     ))));
           n[0]:=0.5*system.sqr(1-4*phase*(system.sqr(cx+n[1]-round(cx+n[1]))+system.sqr(cy     -round(cy     ))))-p[1];
           n[2]:=0.5*system.sqr(1-4*phase*(system.sqr(cx     -round(cx     ))+system.sqr(cy+n[1]-round(cy+n[1]))))-p[1];
           //p[1]:=pyramid(cx,cy     ,complexity);
           //n[0]:=pyramid(cx+n[1],cy,complexity)-p[1];
           //n[2]:=pyramid(cx,cy+n[1],complexity)-p[1];
           sample[0,0]:=0;
           sample[0,1]:=p[1]; if sample[0,1]>1 then sample[0,1]:=1;
           sample[0,2]:=0.2*(1-sample[0,1]);

           n:=normed(n);
           rad:=light*n;
           radY:=proj.specFactor(p,n,light);
           if radY>0.99 then radY:=3
                        else radY:=0;
           proj.throwPixelToMap(p,sample[0]*(0.5 +0.5 *rad)+white*radY,pt,screenX,screenY,result,foreground);
         end;
      7: begin
           rad :=1/(0.1+system.sqr(cx-phase                   )+system.sqr(cy                         ));
           radX:=1/(0.1+system.sqr(cx-phase*system.cos(2*pi/3))+system.sqr(cy-phase*system.sin(2*pi/3)));
           radY:=1/(0.1+system.sqr(cx-phase*system.cos(4*pi/3))+system.sqr(cy-phase*system.sin(4*pi/3)));
           n:=normed(newVector(2*(cx-phase                   )*rad *rad +
                               2*(cx-phase*system.cos(2*pi/3))*radX*radX-
                               2*(cx-phase*system.cos(4*pi/3))*radY*radY,1,
                               2*(cy                         )*rad *rad +
                               2*(cy-phase*system.sin(2*pi/3))*radX*radX-
                               2*(cy-phase*system.sin(4*pi/3))*radY*radY));
           p[0]:=cx;
           p[1]:=-rad-radX+radY;
           p[2]:=cy;
           kx:=round(cx);
           ky:=round(cy);
           if (abs(cx-kx)>0.499) or (abs(cy-ky)>0.499) then begin sample[0]:=white; p[1]:=p[1]+1E-3; end else sample[0]:=blue*min(1,max(p[1],-p[1]));

           radY:=proj.specFactor(p,n,light);
           if radY>0 then for k:=0 to 5 do radY:=radY*radY
                     else rady:=0;
           sample[1,0]:=2*radY;
           sample[1,1]:=radY;
           sample[1,2]:=0;

           proj.throwPixelToMap(p,sample[0]*(0.5 +0.5 *(light*n))+sample[1],pt,screenX,screenY,result,foreground);
         end;
      8: begin
           p:=newVector(cx,recBump(cx     ,cy     ,complexity),cy);
           n[0]:=          recBump(cx+1E-6,cy     ,complexity)-p[1];
           n[2]:=          recBump(cx     ,cy+1E-6,complexity)-p[1];
           n[1]:=1E-6;
           n:=normed(n);
           sample[0,0]:=0.5+(light*n);
           sample[0,1]:=0.5*sample[0,0];
           radY:=proj.specFactor(p,n,light);
           if radY>0 then begin
             radY:=radY*radY; radY:=radY*radY; radY:=radY*radY; radY:=radY*radY; radY:=radY*radY;
             sample[1,2]:=    radY;
             sample[1,1]:=0.5*radY+sample[0,1];
             sample[1,0]:=         sample[0,0];
           end else begin
             sample[1,2]:=0;
             sample[1,1]:=sample[0,1];
             sample[1,0]:=sample[0,0];
           end;
           proj.throwPixelToMap(p,sample[1],pt,screenX,screenY,result,foreground);
         end;

      end;//case

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

PROCEDURE testAntiAlias(scenIdx:byte; name:string; distortLevel:single; distToEye:single);
  CONST numberOfSamples=16;
  VAR pic:T_FloatMap;
      i:longint;
      distort:T_floatColor;
      darts:T_darts;
  begin
    pic.create(xres,yres);
    light:=normed(newVector(1,1,0));
    specPow:=6;
    scenIndex:=scenIdx;
    recurseThreshold:=0;
    maxImproveRuns:=2;
    refineThreshold:=0.8;
    darts.create(numberOfSamples);
    for i:=0 to numberOfSamples-1 do begin
      repeat
        distort:=newVector(random-0.5,random-0.5,random-0.5);
      until norm(distort)<0.5;
      distort:=distortLevel*distort;
      //
      //proj.reInit(newVector(2,1,-2.4)+distort,newVector(0,0,0)+distort,xres,yres,45,darts[i,0],darts[i,1]);
      //
      ////proj.reInit(distToEye*newVector(1,2,0.5)+newVector(0,-0.11995201919232307  ,0)+distort, //eyepoint
      //                                         newVector(0,-0.11995201919232307  ,0)+distort, //look-at
      //                                        xres,yres,45,darts[i,0],darts[i,1]);
      //proj.reInit(distToEye*newVector(-1,0,0)+newVector(0,0.01,0)+distort, //eyepoint
      //                                        newVector(0,0,0)+distort, //look-at
      //                                        xres,yres,45,darts[i,0],darts[i,1]);

      proj.reInit(distToEye*newVector(-1.3,0.7,0.5)+newVector(0,0,0)+distort, //eyepoint
                                                  newVector(0,0,0)+distort, //look-at
                                                  xres,yres,45,darts[i,0],darts[i,1]);

      zmap.clear(black,1E20);
      write(i:3,' ');
      throwPixels(1/16,true);
      zmap.incBitmap(pic,-0.01,black,extractFileExt(name)<>'.vraw');
    end;
    pic.multiplyWith(1/numberOfSamples);
    pic.saveToFile(name);
    pic.destroy;
    darts.destroy;
  end;

CONST cmdList:array [0..2] of T_commandAbstraction=(
    (isFile:true;  leadingSign:' '; cmdString:'';     paramCount: 0),  //0 file (for output)
    (isFile:false; leadingSign:'-'; cmdString:'';     paramCount: 2),  //1 resolution
    (isFile:false; leadingSign:'-'; cmdString:'p';    paramCount: 1)); //2 phase
VAR i:longint;
    ep:T_extendedParameter;
begin
  for i:=1 to paramCount do begin
    ep:=extendedParam(i);
    case byte(matchingCmdIndex(ep,cmdList)) of
      0: resultFileName:=ep.cmdString;
      1: begin xres:=ep.intParam[0]; yres:=ep.intParam[1]; end;
      2: phase:=ep.floatParam[0];
    end;
  end;
  if resultFileName<>'' then begin
    randomize;
    zmap.create(xres,yres); pt:=zmap.rawData;
    complexity:=6;
    proj.create(newVector(0.3,0.3,0.3),newVector(0,0,0),xres,yres,45);
    refineThreshold:=1;
    testAntiAlias(7,resultFileName,5E-3,6);
    proj.destroy;
    zmap.destroy;
  end;
end.
