PROGRAM simplistic;
USES {$ifdef UNIX}cmem,cthreads,{$endif}myfiles,sysutils,zbuf,mypics,math,complex,Process;
VAR fractalType:byte=4;
    juliaMode     :single=0;
    juliaParam    :T_Complex;
    lightNormal   :T_floatColor;
    maxDepth      :longint=10;
    materialType  :byte;
    lookAt        :T_floatColor;

PROCEDURE backgroundDisplay(ps:string);
  VAR tempProcess:TProcess;
  begin
    tempProcess :=TProcess.create(nil);
    tempProcess.CommandLine :={$ifdef UNIX}'./'+{$endif}'display '+ps;
    tempProcess.execute;
    tempProcess.free;
  end;

PROCEDURE normalAndheightAt(cx,cy:single; OUT position,normal:T_floatColor);
  CONST h0:T_Complex=(re:-1/3; im:-1/3; valid:true);
        h1:T_Complex=(re:-1/3; im: 2/3; valid:true);
        h2:T_Complex=(re: 2/3; im:-1/3; valid:true);
//        l0= sqrt(1/3);
//        l1=-sqrt(1/3);
//        l2= sqrt(1/3);

  FUNCTION f(x:T_Complex):single; inline;
    VAR aid:single;
    begin
      aid:=x.re*x.re+x.im*x.im;
      if x.re>1.5 then x.re:=x.re-3 else if x.re<-1.5 then x.re:=x.re+3;
      if x.re>1.5 then x.re:=x.re-3 else if x.re<-1.5 then x.re:=x.re+3;
      if x.re>1.5 then x.re:=x.re-3 else if x.re<-1.5 then x.re:=x.re+3;
      if x.im>1.5 then x.im:=x.im-3 else if x.im<-1.5 then x.im:=x.im+3;
      if x.im>1.5 then x.im:=x.im-3 else if x.im<-1.5 then x.im:=x.im+3;
      if x.im>1.5 then x.im:=x.im-3 else if x.im<-1.5 then x.im:=x.im+3;
      result:=(x.re*x.re+x.im*x.im);
      if result<1 then result:=(1-result)
                  else result:=0.1*system.sin(sqrt(aid));
    end;

  VAR x0,x1,x2,
      c,c0,c1,c2:T_Complex;
      d0,d1,d2:T_compBaseT;
      rsh     :T_compBaseT;
      i:longint;
  begin
    c.re:=cx; c.im:=cy; c.valid:=true;


    c0:=c+h0*1E-3; d0:=f(c0);
    c1:=c+h1*1E-3; d1:=f(c1);
    c2:=c+h2*1E-3; d2:=f(c2);
    //compute and normalize normal vector:-//
    d1:=d1-d0;                             //
    d2:=d2-d0;                             //
    position[0]:=cx;                       //
    position[1]:=d0;                       //
    position[2]:=cy;                       //
    d0:=1/sqrt(d1*d1+d2*d2+1E-3*1E-3);     //
    normal[1]:=(1E-3*d0);                  //
    normal[2]:=(d1  *d0);                  //
    normal[0]:=(d2  *d0);                  //
    //---:compute and normalize normal vector
  end;


FUNCTION normalToColor_metal(n,toEye:T_floatColor):T_floatColor;
  VAR diffuse,spec4,spec8,spec16:single;
  CONST colAmb   :T_floatColor=(0.1,0.1,0.1);
        colDiff  :T_floatColor=(0.3,0.3,0.3);
        colSpec05:T_floatColor=(0.6,0.0,0.0);
        colSpec08:T_floatColor=(0.0,0.6,0.0);
        colSpec16:T_floatColor=(0.0,0.0,0.6);

  begin
    diffuse :=lightNormal*n;
    toEye:=toEye*(1/norm(toEye)); //normalized direction toEye
    n:=2*diffuse*lightNormal-n;
    spec4:=(toEye*n);
    if spec4<0 then begin
      result:=colAmb+colDiff*diffuse;
    end else begin
      spec4 :=spec4*spec4;
      spec4 :=spec4*spec4;
      spec8 :=spec4*spec4;
      spec16:=spec8*spec8;
      result:=colAmb+colDiff*diffuse
                    +colSpec05*spec4
                    +colSpec08*spec8
                    +colSpec16*spec16;
    end;

  end;

PROCEDURE renderAt(eyepoint:T_floatColor; quality:single;  fname:string; goLazy:boolean);
  CONST xres=800;
        yres=600;
  VAR ix,iy,ix0,ix1,iy0,iy1:longint;
      zpic:T_zbufferedMap;
      pt:P_zcol;
      proj:T_3DProjection;
      sampled,outerSampled:boolean;

  FUNCTION renderQuad_lazy(x0,x1,y0,y1:single):boolean;
    VAR p,n:array[0..4] of T_floatColor;
        sc:array[0..3,0..1] of single;
        xsteps,ysteps,ix,iy:longint;
    begin
      proj.screenToRealLevel(p[0],x0,y0,lookat[1]); normalAndheightAt(p[0][0],p[0][2],p[0],n[0]); result:=proj.throwPixelToMap(p[0],normalToColor_metal(n[0],eyepoint-p[0]),pt,sc[0,0],sc[0,1]);
      proj.screenToRealLevel(p[1],x1,y0,lookat[1]); normalAndheightAt(p[1][0],p[1][2],p[1],n[1]); result:=proj.throwPixelToMap(p[1],normalToColor_metal(n[1],eyepoint-p[1]),pt,sc[1,0],sc[1,1]) or result;
      proj.screenToRealLevel(p[2],x0,y1,lookat[1]); normalAndheightAt(p[2][0],p[2][2],p[2],n[2]); result:=proj.throwPixelToMap(p[2],normalToColor_metal(n[2],eyepoint-p[2]),pt,sc[2,0],sc[2,1]) or result;
      proj.screenToRealLevel(p[3],x1,y1,lookat[1]); normalAndheightAt(p[3][0],p[3][2],p[3],n[3]); result:=proj.throwPixelToMap(p[3],normalToColor_metal(n[3],eyepoint-p[3]),pt,sc[3,0],sc[3,1]) or result;
      if result then begin
        xsteps:=1+ceil(max(2*sqrt(system.sqr(sc[0,0]-sc[1,0])+system.sqr(sc[0,1]-sc[1,1])),
                           2*sqrt(system.sqr(sc[2,0]-sc[3,0])+system.sqr(sc[2,1]-sc[3,1])))); if xsteps>256 then xsteps:=256;
        ysteps:=1+ceil(max(2*sqrt(system.sqr(sc[0,0]-sc[2,0])+system.sqr(sc[0,1]-sc[2,1])),
                           2*sqrt(system.sqr(sc[1,0]-sc[3,0])+system.sqr(sc[1,1]-sc[3,1])))); if ysteps>256 then ysteps:=256;
        for ix:=1 to xsteps-1 do
        for iy:=1 to ysteps-1 do begin
          p[4]:=(p[0]*(1-ix/xsteps)+p[1]*(ix/xsteps))*(1-iy/ysteps)
               +(p[2]*(1-ix/xsteps)+p[3]*(ix/xsteps))*(  iy/ysteps);
          n[4]:=(n[0]*(1-ix/xsteps)+n[1]*(ix/xsteps))*(1-iy/ysteps)
               +(n[2]*(1-ix/xsteps)+n[3]*(ix/xsteps))*(  iy/ysteps);
          proj.throwPixelToMap(p[4],normalToColor_metal(n[4],eyepoint-p[4]),pt);
        end;
      end;
    end;

  FUNCTION renderQuad(x0,x1,y0,y1:single):boolean;
    VAR p,n:T_floatColor;
        sc:array[0..3,0..1] of single;
        xsteps,ysteps,ix,iy:longint;
    begin
      proj.screenToRealLevel(p,x0,y0,lookat[1]); normalAndheightAt(p[0],p[2],p,n); result:=proj.throwPixelToMap(p,normalToColor_metal(n,eyepoint-p),pt,sc[0,0],sc[0,1]);
      proj.screenToRealLevel(p,x1,y0,lookat[1]); normalAndheightAt(p[0],p[2],p,n); result:=proj.throwPixelToMap(p,normalToColor_metal(n,eyepoint-p),pt,sc[1,0],sc[1,1]) or result;
      proj.screenToRealLevel(p,x0,y1,lookat[1]); normalAndheightAt(p[0],p[2],p,n); result:=proj.throwPixelToMap(p,normalToColor_metal(n,eyepoint-p),pt,sc[2,0],sc[2,1]) or result;
      proj.screenToRealLevel(p,x1,y1,lookat[1]); normalAndheightAt(p[0],p[2],p,n); result:=proj.throwPixelToMap(p,normalToColor_metal(n,eyepoint-p),pt,sc[3,0],sc[3,1]) or result;
      if result then begin
        xsteps:=1+ceil(max(2*sqrt(system.sqr(sc[0,0]-sc[1,0])+system.sqr(sc[0,1]-sc[1,1])),
                           2*sqrt(system.sqr(sc[2,0]-sc[3,0])+system.sqr(sc[2,1]-sc[3,1])))); if xsteps>256 then xsteps:=256;
        ysteps:=1+ceil(max(2*sqrt(system.sqr(sc[0,0]-sc[2,0])+system.sqr(sc[0,1]-sc[2,1])),
                           2*sqrt(system.sqr(sc[1,0]-sc[3,0])+system.sqr(sc[1,1]-sc[3,1])))); if ysteps>256 then ysteps:=256;
        for ix:=1 to xsteps-1 do
        for iy:=1 to ysteps-1 do begin
          proj.screenToRealLevel(p,x0+ix*(x1-x0)/xsteps,
                                   y0+iy*(y1-y0)/ysteps,lookat[1]);
          normalAndheightAt(p[0],p[2],p,n);
          proj.throwPixelToMap(p,normalToColor_metal(n,eyepoint-p),pt);
        end;
      end;
    end;

  begin
    writeln('creating image "',fname,'"');
    zpic.create(xres,yres);
    proj.create(eyepoint,lookat,xres,yres,45);
    zpic.clear(black,1E20);
    pt:=zpic.rawdata;
    ix0:=round(0.2*xres*quality);
    ix1:=round(0.8*xres*quality);
    iy0:=round(0.2*yres*quality);
    iy1:=round(0.8*yres*quality);
    if goLazy then begin
      for iy:=iy0 to iy1 do for ix:=ix0 to ix1 do renderQuad_lazy(ix /quality,(ix +1)/quality,iy /quality,(iy +1)/quality);
      repeat
        outerSampled:=false;
        repeat inc(ix1); sampled:=false; for iy:=iy0 to iy1 do if renderQuad_lazy(ix1/quality,(ix1+1)/quality,iy /quality,(iy +1)/quality) then begin sampled:=true; outerSampled:=true; end; until not(sampled);
        repeat dec(ix0); sampled:=false; for iy:=iy0 to iy1 do if renderQuad_lazy(ix0/quality,(ix0+1)/quality,iy /quality,(iy +1)/quality) then begin sampled:=true; outerSampled:=true; end; until not(sampled);
        repeat inc(iy1); sampled:=false; for ix:=ix0 to ix1 do if renderQuad_lazy(ix /quality,(ix +1)/quality,iy1/quality,(iy1+1)/quality) then begin sampled:=true; outerSampled:=true; end; until not(sampled);
        repeat dec(iy0); sampled:=false; for ix:=ix0 to ix1 do if renderQuad_lazy(ix /quality,(ix +1)/quality,iy0/quality,(iy0+1)/quality) then begin sampled:=true; outerSampled:=true; end; until not(sampled);
      until not(outerSampled);
    end else begin
      for iy:=iy0 to iy1 do for ix:=ix0 to ix1 do renderQuad(ix /quality,(ix +1)/quality,iy /quality,(iy +1)/quality);
      repeat
        outerSampled:=false;
        repeat inc(ix1); sampled:=false; for iy:=iy0 to iy1 do if renderQuad(ix1/quality,(ix1+1)/quality,iy /quality,(iy +1)/quality) then begin sampled:=true; outerSampled:=true; end; until not(sampled);
        repeat dec(ix0); sampled:=false; for iy:=iy0 to iy1 do if renderQuad(ix0/quality,(ix0+1)/quality,iy /quality,(iy +1)/quality) then begin sampled:=true; outerSampled:=true; end; until not(sampled);
        repeat inc(iy1); sampled:=false; for ix:=ix0 to ix1 do if renderQuad(ix /quality,(ix +1)/quality,iy1/quality,(iy1+1)/quality) then begin sampled:=true; outerSampled:=true; end; until not(sampled);
        repeat dec(iy0); sampled:=false; for ix:=ix0 to ix1 do if renderQuad(ix /quality,(ix +1)/quality,iy0/quality,(iy0+1)/quality) then begin sampled:=true; outerSampled:=true; end; until not(sampled);
      until not(outerSampled);
    end;
    zpic.saveBitmap(fname);
    backgroundDisplay(fname);
    zpic.destroy;
  end;

VAR i:longint;
begin
  lightNormal:=newVector(sqrt(1/3),sqrt(1/3),sqrt(1/3));
  lookat:=newVector(0,1,0);
  juliaParam :=0;
  SetExceptionMask([ exInvalidOp,
  exDenormalized,
  exZeroDivide,
  exOverflow,
  exUnderflow,
  exPrecision]);


  for i:=0 to 199 do if (i and 7) in [0]       then renderAt(lookat+0.1*(i+1)*newVector(2*system.sin(2*pi*i/200),1+0.01*i,2*system.cos(2*pi*i/200)),1/2,'sinus'+copy(intToStr(i+1000),2,3)+'.jpg',false);
  for i:=0 to 199 do if (i and 7) in [4]       then renderAt(lookat+0.1*(i+1)*newVector(2*system.sin(2*pi*i/200),1+0.01*i,2*system.cos(2*pi*i/200)),1/2,'sinus'+copy(intToStr(i+1000),2,3)+'.jpg',false);
  for i:=0 to 199 do if (i and 7) in [2,6]     then renderAt(lookat+0.1*(i+1)*newVector(2*system.sin(2*pi*i/200),1+0.01*i,2*system.cos(2*pi*i/200)),1/2,'sinus'+copy(intToStr(i+1000),2,3)+'.jpg',false);
  for i:=0 to 199 do if (i and 7) in [1,3,5,7] then renderAt(lookat+0.1*(i+1)*newVector(2*system.sin(2*pi*i/200),1+0.01*i,2*system.cos(2*pi*i/200)),1/2,'sinus'+copy(intToStr(i+1000),2,3)+'.jpg',false);

  //for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),1/16,'zbt'+copy(intToStr(i+1000),2,3)+'qA.jpg');
  //for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),1/8 ,'zbt'+copy(intToStr(i+1000),2,3)+'qB.jpg');
  //for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),1/4 ,'zbt'+copy(intToStr(i+1000),2,3)+'qC.jpg');
  //for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),1/2 ,'zbt'+copy(intToStr(i+1000),2,3)+'qD.jpg');
  //for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),1/1 ,'zbt'+copy(intToStr(i+1000),2,3)+'qE.jpg');

{  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),0.25,'zbt'+copy(intToStr(i+1000),2,3)+'q0.jpg');
  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),0.5 ,'zbt'+copy(intToStr(i+1000),2,3)+'q1.jpg');
  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),1   ,'zbt'+copy(intToStr(i+1000),2,3)+'q2.jpg');
  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),2   ,'zbt'+copy(intToStr(i+1000),2,3)+'q3.jpg');
  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),4   ,'zbt'+copy(intToStr(i+1000),2,3)+'q4.jpg');
  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),8   ,'zbt'+copy(intToStr(i+1000),2,3)+'q5.jpg');
  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),16  ,'zbt'+copy(intToStr(i+1000),2,3)+'q6.jpg');
  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),32  ,'zbt'+copy(intToStr(i+1000),2,3)+'q7.jpg');
  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),64  ,'zbt'+copy(intToStr(i+1000),2,3)+'q8.jpg');
  for i:=31 to 31 do renderAt(newVector(2*system.sin(2*pi*i/200),1.3+i*0.02,2*system.cos(2*pi*i/200)),128 ,'zbt'+copy(intToStr(i+1000),2,3)+'q9.jpg'); }
end.
