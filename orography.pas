PROGRAM orography;
USES {$ifdef UNIX}cmem,cthreads,{$endif}zbuf,mypics,process,sysutils,math,complex;
{$fputype sse3}

{define showProgress}
{define showTempImages}
CONST //xres=2048; yres=1152;
      xres=800; yres=600;
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

PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif} 'display '+ps;
    tempProcess.execute;
    tempProcess.Free;
  end;

PROCEDURE save(fname:string);
  begin
    zmap.saveBitmap(fname,-0.01,black);

    backgroundDisplay(fname);
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


PROCEDURE animate;
  VAR i:longint;
  begin
    maxImproveRuns:=1000;
    recurseThreshold:=4;
    refineThreshold:=1;
    light:=normed(newVector(1,1,0));
    specPow:=8;
    scenIndex:=4;
    complexity:=7;
    for i:=200 to 300 do begin
      proj.reInit((0.05+0.01*i)*newVector(1,0.5+0.01*i,1.2*(1-i/100))+newVector(1.25,0.5,1),newVector(1.25,0.5,1),xres,yres,45*min(1,3-i*0.01));
      zmap.clear(black,1E20);
      write(i:3,' '); throwPixels(1/8,false);
      save('animate'+copy(intToStr(1000+i),2,3)+'.jpg');
    end;
  end;

PROCEDURE aaanimate;
  VAR i,j:longint;
      pic:T_FloatMap;
      darts:T_darts;
      eyepoint:T_floatColor;
      openingAngle:single;
      subComplex:longint;
      fogColor:T_floatColor;
      fogDens :single;
  begin
    fogColor:=white*0.7;
    fogDens :=-0.01;
    pic.create(xres,yres);
    darts.create(8);
    recurseThreshold:=1;
    refineThreshold:=1;
    light:=normed(newVector(1,1,0));
    specPow:=8;
    scenIndex:=4;
    complexity:=11;

    subComplex:=0;
    openingAngle:=8;
    phase:=1;

    for i:=0 to 365 do begin
      eyepoint:=(0.05+0.01*i)*newVector(1,0.5+0.01*i,1.2*(1-i/100))+newVector(1.25,0.49,1);
      if i>100 then phase:=phase-0.01;
      if eyepoint[1]>1.5 then eyepoint[1]:=1.5;
      if i>260 then eyepoint[1]:=eyepoint[1]-(i-260)*0.02;
      if eyepoint[1]<0.3 then eyepoint[1]:=0.3;
      if eyepoint[0]>4 then eyepoint[0]:=4;

      if not(fileExists ('Q'+copy(intToStr(1000+i),2,3)+'.jpg')) then begin
        pic.multiplyWith(0);
        for j:=0 to 7 do begin
          if j<subComplex then inc(complexity);
          write(i:3,'/',j,'(',complexity,') ');
          proj.reInit(eyepoint,newVector(1.25,0.49,1),xres,yres,openingAngle,darts[j,0],darts[j,1]);
          zmap.clear(black,1E20);
          throwPixels(1/4        ,false);
          zmap.incBitmap(pic,fogDens,fogColor,true);
          if j<subComplex then dec(complexity);

 //         save('intermediate'+intToStr(j)+'.vraw');
 //         readln;
        end;
        pic.multiplyWith(1/8);
        pic.saveToFile   ('Q'+copy(intToStr(1000+i),2,3)+'.jpg');
        backgroundDisplay('Q'+copy(intToStr(1000+i),2,3)+'.jpg');
      end;
      if (i>=250) and (i<290) then fogDens:=fogDens*1.1;
      if (i>=290) then begin
        fogDens:=fogDens*0.7;
        fogColor:=fogColor*0.95;
      end;

      if openingAngle<45 then openingAngle:=openingAngle+0.2;
      if subComplex>0 then dec(subComplex);
      if ((i mod 30)=29) and (complexity>0) then begin dec(complexity); subComplex:=7; end;
    end;
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
    maxImproveRuns:=1;
    refineThreshold:=0.87;
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

      proj.reInit(distToEye*newVector(-1.3,1,0.4)+newVector(0,0,0)+distort, //eyepoint
                                                  newVector(0,0,0)+distort, //look-at
                                                  xres,yres,45,darts[i,0],darts[i,1]);

      zmap.clear(black,1E20);
      write(i:3,' ');
      throwPixels(1/16,false);
//      zmap.incBitmap(pic,-0.01,white*0.7);
      zmap.incBitmap(pic,-0.01,black,ExtractFileExt(name)<>'.vraw');
      save('intermediate'+intToStr(i)+'.vraw');
    end;
    pic.multiplyWith(1/numberOfSamples);
    pic.saveToFile(name);
    backgroundDisplay(name);
    pic.destroy;
    darts.destroy;
  end;

PROCEDURE animateScene8;
  VAR i,j:longint;
      pic:T_FloatMap;
      darts:T_darts;
      eyepoint:T_floatColor;
      openingAngle:single;
      subComplex:longint;
      fogColor:T_floatColor;
      fogDens :single;

  begin
    fogColor:=blue;
    fogDens :=-0.01;
    pic.create(xres,yres);
    darts.create(8);
    recurseThreshold:=1;
    refineThreshold:=1;
    light:=normed(newVector(1,1,0));
    scenIndex:=8;
    complexity:=11;

    subComplex:=0;
    openingAngle:=45;
    phase:=3;

    for i:=0 to 500 do begin
      eyepoint:=newVector(-3.01+0.01*i,0.01*(i+1),0.01*i);
      if i>=200 then phase:=phase-0.01;
      if not(fileExists ('T'+copy(intToStr(1000+i),2,3)+'.jpg')) then begin
        pic.multiplyWith(0);
        for j:=0 to 7 do begin
          if j<subComplex then inc(complexity);
          write(i:3,'/',j,'(',complexity,') ');
          if i>=190 then openingAngle:=openingAngle-0.02;
          eyepoint:=newVector(-3.01+0.01*(i+j/8),0.02*((i+j/8)+0.5),0.01*(i+j/8));


          proj.reInit(eyepoint,newVector(0,0,0),xres,yres,openingAngle,darts[j,0],darts[j,1]);
          zmap.clear(black,1E20);
          if      i<=10 then throwPixels(1/2 ,false)
          else if i<=20 then throwPixels(1/4 ,false)
          else if i<=30 then throwPixels(1/8 ,false)
                        else throwPixels(1/16,false);
          zmap.incBitmap(pic,fogDens,fogColor,true);
          if j<subComplex then dec(complexity);
        end;
        pic.multiplyWith(1/8);
        pic.saveToFile   ('T'+copy(intToStr(1000+i),2,3)+'.jpg');
        backgroundDisplay('T'+copy(intToStr(1000+i),2,3)+'.jpg');
      end else if i>=190 then openingAngle:=openingAngle-0.16;
    end;
  end;

begin
  randomize;
  zmap.create(xres,yres); pt:=zmap.rawData;
  proj.create(newVector(0.3,0.3,0.3),newVector(0,0,0),xres,yres,45);
  refineThreshold:=1;
//  specPow:=6;
//  aaanimate;
//  scenIndex:=6;
//  complexity:=6;
//  phase:=1.3;  testAntiAlias(0,'scene0.jpg',5E-3,1);
//  phase:=1.3;  testAntiAlias(1,'scene1.jpg',5E-3,1);
//  phase:=1.3;  testAntiAlias(2,'scene2.jpg',5E-3,1);
//  phase:=0; complexity:=6; testAntiAlias(3,'scene3.jpg',5E-3,1);
//  phase:=0; complexity:=6; testAntiAlias(4,'scene4.jpg',5E-3,1);
  phase:=0.01;  testAntiAlias(5,'scene5.jpg',5E-3,5);
//  phase:=1.3;  testAntiAlias(6,'scene6.jpg',5E-3,1);
//  phase:=1.3;  testAntiAlias(7,'scene6.jpg',5E-3,1);
//  phase:=1.3;  testAntiAlias(8,'scene6.jpg',5E-3,1);
//  phase:=1.3;  testAntiAlias(9,'scene6.jpg',5E-3,1);
{  phase:=0.0;  testAntiAlias(6,'pyra00.vraw',1E-2,1);
  phase:=0.1;  testAntiAlias(6,'pyra01.vraw',1E-2,1);
  phase:=0.2;  testAntiAlias(6,'pyra02.vraw',1E-2,1);
  phase:=0.3;  testAntiAlias(6,'pyra03.vraw',1E-2,1);
  phase:=0.4;  testAntiAlias(6,'pyra04.vraw',1E-2,1);
  phase:=0.5;  testAntiAlias(6,'pyra05.vraw',1E-2,1);
  phase:=0.6;  testAntiAlias(6,'pyra06.vraw',1E-2,1);
  phase:=0.7;  testAntiAlias(6,'pyra07.vraw',1E-2,1);
  phase:=0.8;  testAntiAlias(6,'pyra08.vraw',1E-2,1);
  phase:=0.9;  testAntiAlias(6,'pyra09.vraw',1E-2,1);
  phase:=1.0;  testAntiAlias(6,'pyra10.vraw',1E-2,1);
  phase:=1.1;  testAntiAlias(6,'pyra11.vraw',1E-2,1);
  phase:=1.2;  testAntiAlias(6,'pyra12.vraw',1E-2,1);
  phase:=1.3;  testAntiAlias(6,'pyra13.vraw',1E-2,1);
  phase:=1.4;  testAntiAlias(6,'pyra14.vraw',1E-2,1);
  phase:=1.5;  testAntiAlias(6,'pyra15.vraw',1E-2,1);
  phase:=1.6;  testAntiAlias(6,'pyra16.vraw',1E-2,1);
  phase:=1.7;  testAntiAlias(6,'pyra17.vraw',1E-2,1);
  phase:=1.8;  testAntiAlias(6,'pyra18.vraw',1E-2,1);   }

  //phase:=1.2;  testAntiAlias(6,'shineAgain.vraw',1E-3,2);


  //phase:=0.01; complexity:=0;  testAntiAlias(5,'scene5a.vraw',5E-3,3);
  //phase:=0.01; complexity:=1;  testAntiAlias(5,'scene5b.vraw',5E-3,1);
  //phase:=0.01; complexity:=2;  testAntiAlias(5,'scene5c.vraw',5E-3,1);
  //phase:=0.01; complexity:=3;  testAntiAlias(5,'scene5d.vraw',5E-3,1);
  //phase:=0.01; complexity:=4;  testAntiAlias(5,'scene5e.vraw',5E-3,1);
  //phase:=0.01; complexity:=5;  testAntiAlias(5,'scene5f.vraw',5E-3,1);
  //phase:=0.01; complexity:=6;  testAntiAlias(5,'scene5g.vraw',0,1);
  //phase:=0.01; complexity:=7;  testAntiAlias(5,'scene5h.vraw',0,1);

  //phase:=3.0; complexity:=10;  testAntiAlias(8,'scene8.jpg',0,3.01);

  //animateScene8;
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv00q.vraw',0,0.25 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv50q.vraw',0,0.0625           );
  //
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv01q.vraw',0,0.24316373685307 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv02q.vraw',0,0.2365144116814  );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv03q.vraw',0,0.23004691265622 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv04q.vraw',0,0.22375626773199 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv05q.vraw',0,0.21763764082403 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv06q.vraw',0,0.21168632809063 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv07q.vraw',0,0.20589775431689 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv08q.vraw',0,0.20026746939741 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv09q.vraw',0,0.19479114491512 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv10q.vraw',0,0.1894645708138  );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv11q.vraw',0,0.18428365216139 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv12q.vraw',0,0.17924440600198 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv13q.vraw',0,0.1743429582938  );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv14q.vraw',0,0.16957554093096 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv15q.vraw',0,0.16493848884661 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv16q.vraw',0,0.16042823719536 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv17q.vraw',0,0.1560413186127  );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv18q.vraw',0,0.15177436054938 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv19q.vraw',0,0.14762408267869 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv20q.vraw',0,0.14358729437463 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv21q.vraw',0,0.13966089225903 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv22q.vraw',0,0.13584185781576 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv23q.vraw',0,0.13212725507017 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv24q.vraw',0,0.12851422833201 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv25q.vraw',0,0.125            );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv26q.vraw',0,0.12158186842654 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv27q.vraw',0,0.1182572058407  );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv28q.vraw',0,0.11502345632811 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv29q.vraw',0,0.111878133866   );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv30q.vraw',0,0.10881882041202 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv31q.vraw',0,0.10584316404532 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv32q.vraw',0,0.10294887715845 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv33q.vraw',0,0.1001337346987  );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv34q.vraw',0,0.09739557245756 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv35q.vraw',0,0.0947322854069  );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv36q.vraw',0,0.09214182608069 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv37q.vraw',0,0.08962220300099 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv38q.vraw',0,0.0871714791469  );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv39q.vraw',0,0.08478777046548 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv40q.vraw',0,0.08246924442331 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv41q.vraw',0,0.08021411859768 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv42q.vraw',0,0.07802065930635 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv43q.vraw',0,0.07588718027469 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv44q.vraw',0,0.07381204133935 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv45q.vraw',0,0.07179364718731 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv46q.vraw',0,0.06983044612951 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv47q.vraw',0,0.06792092890788 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv48q.vraw',0,0.06606362753509 );
  //   phase:=0; complexity:=10; testAntiAlias(4,'scaleInv49q.vraw',0,0.064257114166   );
  //
  //
  //
  //
  //
  //

//    phase:=0; complexity:= 6+1; testAntiAlias(3,'fractality00b.vraw',0.1E-4,0.8**00);
    //phase:=0; complexity:=12+1; testAntiAlias(3,'fractality40b.vraw',0.1E-4,0.8**40);
//    phase:=0; complexity:= 6+1; testAntiAlias(3,'fractality01b.vraw',0.1E-4,0.8**01);
    //phase:=0; complexity:=12+1; testAntiAlias(3,'fractality39b.vraw',0.1E-4,0.8**39);
//    phase:=0; complexity:= 6+1; testAntiAlias(3,'fractality02xxx.vraw',0.1E-4,0.8**02);
//    phase:=0; complexity:=12+1; testAntiAlias(3,'fractality38b.vraw',0.1E-4,0.8**38);
//            phase:=0; complexity:= 6+1; testAntiAlias(3,'fractality03b.vraw',0.1E-4,0.8**03);
//    phase:=0; complexity:= 7+1; testAntiAlias(3,'fractality04b.vraw',0.1E-4,0.8**04);
//    phase:=0; complexity:= 7+1; testAntiAlias(3,'fractality05b.vraw',0.1E-4,0.8**05);
//    phase:=0; complexity:=12+1; testAntiAlias(3,'fractality37b.vraw',0.1E-4,0.8**37);
//    phase:=0; complexity:= 7+1; testAntiAlias(3,'fractality06b.vraw',0.1E-4,0.8**06);
//    phase:=0; complexity:=12+1; testAntiAlias(3,'fractality36b.vraw',0.1E-4,0.8**36);
//    phase:=0; complexity:= 7+1; testAntiAlias(3,'fractality07b.vraw',0.1E-4,0.8**07);
//    phase:=0; complexity:=12+1; testAntiAlias(3,'fractality35b.vraw',0.1E-4,0.8**35);
//    phase:=0; complexity:= 7+1; testAntiAlias(3,'fractality08b.vraw',0.1E-4,0.8**08);
//    phase:=0; complexity:=11+1; testAntiAlias(3,'fractality34b.vraw',0.1E-4,0.8**34);
//    phase:=0; complexity:= 7+1; testAntiAlias(3,'fractality09b.vraw',0.1E-4,0.8**09);
//    phase:=0; complexity:=11+1; testAntiAlias(3,'fractality33b.vraw',0.1E-4,0.8**33);
//    phase:=0; complexity:= 8+1; testAntiAlias(3,'fractality10b.vraw',0.1E-4,0.8**10);
//    phase:=0; complexity:=11+1; testAntiAlias(3,'fractality32b.vraw',0.1E-4,0.8**32);
//    phase:=0; complexity:= 8+1; testAntiAlias(3,'fractality11b.vraw',0.1E-4,0.8**11);
//    phase:=0; complexity:=11+1; testAntiAlias(3,'fractality31b.vraw',0.1E-4,0.8**31);
//    phase:=0; complexity:= 8+1; testAntiAlias(3,'fractality12b.vraw',0.1E-4,0.8**12);
//    phase:=0; complexity:=11+1; testAntiAlias(3,'fractality30b.vraw',0.1E-4,0.8**30);
//    phase:=0; complexity:= 8+1; testAntiAlias(3,'fractality13b.vraw',0.1E-4,0.8**13);
//    phase:=0; complexity:=11+1; testAntiAlias(3,'fractality29b.vraw',0.1E-4,0.8**29);
//    phase:=0; complexity:= 8+1; testAntiAlias(3,'fractality14b.vraw',0.1E-4,0.8**14);
//    phase:=0; complexity:=11+1; testAntiAlias(3,'fractality28b.vraw',0.1E-4,0.8**28);
//    phase:=0; complexity:= 8+1; testAntiAlias(3,'fractality15b.vraw',0.1E-4,0.8**15);
//    phase:=0; complexity:=10+1; testAntiAlias(3,'fractality27b.vraw',0.1E-4,0.8**27);
//    phase:=0; complexity:= 9+1; testAntiAlias(3,'fractality16b.vraw',0.1E-4,0.8**16);
//    phase:=0; complexity:=10+1; testAntiAlias(3,'fractality26b.vraw',0.1E-4,0.8**26);
//    phase:=0; complexity:= 9+1; testAntiAlias(3,'fractality17b.vraw',0.1E-4,0.8**17);
//    phase:=0; complexity:=10+1; testAntiAlias(3,'fractality25b.vraw',0.1E-4,0.8**25);
//    phase:=0; complexity:= 9+1; testAntiAlias(3,'fractality18b.vraw',0.1E-4,0.8**18);
//    phase:=0; complexity:=10+1; testAntiAlias(3,'fractality24b.vraw',0.1E-4,0.8**24);
//    phase:=0; complexity:= 9+1; testAntiAlias(3,'fractality19b.vraw',0.1E-4,0.8**19);
//    phase:=0; complexity:=10+1; testAntiAlias(3,'fractality23b.vraw',0.1E-4,0.8**23);
//    phase:=0; complexity:= 9+1; testAntiAlias(3,'fractality20b.vraw',0.1E-4,0.8**20);
//    phase:=0; complexity:=10+1; testAntiAlias(3,'fractality22b.vraw',0.1E-4,0.8**22);
//    phase:=0; complexity:= 9+1; testAntiAlias(3,'fractality21b.vraw',0.1E-4,0.8**21);
//
//
//






 { light:=normed(newVector(1,1,0));
  proj.reInit(newVector(0,3,1),newVector(10,0,0),xres,yres,45);
  //animate;
  complexity:=7;
  testAntiAlias(6,'lensblur0.png',1E-4);
  testAntiAlias(4,'lensblur1.png',2E-4);
  testAntiAlias(4,'lensblur2.png',4E-4);
  testAntiAlias(4,'lensblur3.png',8E-4);
  testAntiAlias(4,'lensblur4.png',1.6E-3);
  testAntiAlias(4,'lensblur5.png',3.2E-3);
  testAntiAlias(4,'lensblur6.png',6.4E-3);}



{  phase:=0.0; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple0.jpg');
  phase:=0.1; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple1.jpg');
  phase:=0.2; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple2.jpg');
  phase:=0.3; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple3.jpg');
  phase:=0.4; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple4.jpg');
  phase:=0.5; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple5.jpg');
  phase:=0.6; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple6.jpg');
  phase:=0.7; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple7.jpg');
  phase:=0.8; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple8.jpg');
  phase:=0.9; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple9.jpg');
  phase:=1.0; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple10.jpg');
  phase:=1.1; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple11.jpg');
  phase:=1.2; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple12.jpg');
  phase:=1.3; zmap.clear(black,1E20); throwPixels(1/32,true); save('temple13.jpg');}
{  phase:=1.4; zmap.clear(black,1E20); throwPixels(1/16,true); save('temple14.jpg');
  phase:=1.5; zmap.clear(black,1E20); throwPixels(1/16,true); save('temple15.jpg');
  phase:=1.6; zmap.clear(black,1E20); throwPixels(1/16,true); save('temple16.jpg');
  phase:=1.7; zmap.clear(black,1E20); throwPixels(1/16,true); save('temple17.jpg');
  phase:=1.8; zmap.clear(black,1E20); throwPixels(1/16,true); save('temple18.jpg');
  phase:=1.9; zmap.clear(black,1E20); throwPixels(1/16,true); save('temple19.jpg');
  phase:=2.0; zmap.clear(black,1E20); throwPixels(1/16,true); save('temple20.jpg');}




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
