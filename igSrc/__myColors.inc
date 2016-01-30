{$ifdef include_interface}
TYPE
  T_Float     =single;
  T_24Bit     =array[0..2] of byte;
  P_24Bit     =^T_24Bit;
  T_floatColor=array[0..2] of T_Float;
  P_floatColor=^T_floatColor;

CONST
   //Color definitions in RGB (correct for openGL)
   red    :T_floatColor=(1,0,0);
   green  :T_floatColor=(0,1,0);
   blue   :T_floatColor=(0,0,1);
   cyan   :T_floatColor=(0,1,1);
   yellow :T_floatColor=(1,1,0);
   magenta:T_floatColor=(1,0,1);
   white  :T_floatColor=(1,1,1);
   black  :T_floatColor=(0,0,0);
   grey   :T_floatColor=(0.5,0.5,0.5);

   red24Bit   :T_24Bit=(255,0,0);
   green24Bit :T_24Bit=(0,255,0);
   blue24Bit  :T_24Bit=(0,0,255);
   white24Bit :T_24Bit=(255,255,255);
   black24Bit :T_24Bit=(0,0,0);

VAR maxB_:single=0;

OPERATOR =(x,y:T_24Bit):boolean; inline;
OPERATOR :=(x:T_24Bit):T_floatColor; inline;
OPERATOR :=(x:T_floatColor ):T_24Bit; inline;
FUNCTION newVector(x,y,z:T_Float):T_floatColor; inline;
FUNCTION newColor(x,y,z:T_Float):T_floatColor; inline;
FUNCTION toString(x:T_floatColor):string;
OPERATOR +(x,y:T_floatColor): T_floatColor; inline;
OPERATOR -(x,y:T_floatColor): T_floatColor; inline;
OPERATOR *(x:T_floatColor; y:T_Float): T_floatColor; inline;
OPERATOR *(y:T_Float; x:T_floatColor): T_floatColor; inline;
OPERATOR *(x,y:T_floatColor): T_Float; inline;
FUNCTION cross(x,y:T_floatColor):T_floatColor; inline;
FUNCTION getOverbright(VAR x:T_floatColor):T_floatColor;
FUNCTION getMax1Overbright(VAR x:T_floatColor):T_floatColor;
FUNCTION dist(x,y:T_floatColor): T_Float; inline;
FUNCTION sqDist(x,y:T_floatColor): T_Float; inline;
FUNCTION absDist(x,y:T_floatColor): T_Float; inline;
FUNCTION norm(x:T_floatColor): T_Float; inline;
FUNCTION normed(x:T_floatColor): T_floatColor; inline;
FUNCTION sqNorm(x:T_floatColor): T_Float; inline;
FUNCTION absNorm(x:T_floatColor): T_Float; inline;
FUNCTION projectedColor(x:T_floatColor):T_24Bit;
FUNCTION fprojectedColor(x:T_floatColor):T_floatColor;

FUNCTION greyLevel(x:T_floatColor):T_Float;
FUNCTION greyLevel(x:T_24Bit):T_Float;
FUNCTION hue(x:T_Float):T_floatColor;
FUNCTION toHSV(x:T_floatColor):T_floatColor;
FUNCTION fromHSV(x:T_floatColor):T_floatColor;
FUNCTION fromHSV(H,S,V:single):T_floatColor;

//FUNCTION toCumulative  (hist:T_histogram):T_histogram;
//FUNCTION fromCumulative(hist:T_histogram):T_histogram;
//FUNCTION smoothKernel  (sigma:T_Float):T_histogram;
//FUNCTION smooth        (hist,kernel:T_histogram):T_histogram;
//PROCEDURE smoothInc(VAR hist:T_histogram; incIdx,incWgt:single); inline;
//FUNCTION smooth        (hist:T_histogram; sigma:T_Float):T_histogram;
//FUNCTION smoothModeCnt (hist:T_histogram; modeCount:longint):T_histogram;
//FUNCTION quantileFromCum (hist:T_histogram; q:T_Float):T_Float;
//FUNCTION quantileFromHist(hist:T_histogram; q:T_Float):T_Float;
//FUNCTION variance      (hist:T_histogram):T_Float;
//FUNCTION smoothEntry   (hist:T_histogram; index:T_Float):T_Float;
//PROCEDURE clear(VAR hist:T_histogram);
//FUNCTION nearestMode(VAR hist:T_histogram; x:byte):byte;
//FUNCTION mainMode(VAR hist:T_histogram):byte;
//
//PROCEDURE writeToFile(hist:T_histogram; fname:string);
//PROCEDURE writeToFile(h1,h2,h3:T_histogram; fname:string);

{$endif}
{$ifdef include_implementation}

OPERATOR =(x,y:T_24Bit):boolean; inline;
  begin
    result:=(x[0]=y[0])
        and (x[1]=y[1])
        and (x[2]=y[2]);
  end;

OPERATOR :=(x:T_24Bit):T_floatColor;
  begin
    result[0]:=x[0]/255;
    result[1]:=x[1]/255;
    result[2]:=x[2]/255;
  end;

OPERATOR :=(x:T_floatColor):T_24Bit;
  begin
    result[0]:=round(min(255,max(0,x[0]*255)));
    result[1]:=round(min(255,max(0,x[1]*255)));
    result[2]:=round(min(255,max(0,x[2]*255)));
  end;

FUNCTION newVector(x,y,z:T_Float):T_floatColor;
  begin
    result[0] :=x;
    result[1] :=y;
    result[2] :=z;
  end;

FUNCTION newColor(x,y,z:T_Float):T_floatColor;
  begin
    result[0] :=x;
    result[1] :=y;
    result[2] :=z;
  end;


FUNCTION toString(x:T_floatColor):string;
  begin
    result:='('+formatFloat('0.000',x[0])+','+formatFloat('0.000',x[1])+','+formatFloat('0.000',x[2])+')';
  end;

OPERATOR +(x,y:T_floatColor):T_floatColor; begin result[0]:=x[0]+y[0]; result[1]:=x[1]+y[1]; result[2]:=x[2]+y[2]; end;
OPERATOR -(x,y:T_floatColor):T_floatColor; begin result[0]:=x[0]-y[0]; result[1]:=x[1]-y[1]; result[2]:=x[2]-y[2]; end;
OPERATOR *(x:T_floatColor; y:T_Float):T_floatColor; begin result[0]:=x[0]*y; result[1]:=x[1]*y; result[2]:=x[2]*y; end;
OPERATOR *(y:T_Float; x:T_floatColor):T_floatColor; begin result[0]:=x[0]*y; result[1]:=x[1]*y; result[2]:=x[2]*y; end;
OPERATOR *(x,y:T_floatColor):T_Float; begin result:=x[0]*y[0]+x[1]*y[1]+x[2]*y[2]; end;
FUNCTION cross(x,y:T_floatColor):T_floatColor; inline;
  begin
    result[0]:=x[1]*y[2]-x[2]*y[1];
    result[1]:=x[2]*y[0]-x[0]*y[2];
    result[2]:=x[0]*y[1]-x[1]*y[0];
  end;
FUNCTION getOverbright(VAR x:T_floatColor):T_floatColor;
  VAR b:single;
  begin
    if x[0]>x[1] then b:=x[0]  //find brightest channel
                 else b:=x[1];
    if x[2]>b    then b:=x[2];
    if b<1.002 then result:=black else begin //if brightest channel is darker than 1, there is no overbrightness
      result:=x*(1-1/b);  //else result is
      x     :=x-result;
    end;
    if x[0]<0 then x[0]:=0;
    if x[1]<0 then x[1]:=0;
    if x[2]<0 then x[2]:=0;
  end;

FUNCTION getMax1Overbright(VAR x:T_floatColor):T_floatColor;
  VAR b:single;
  begin
    if x[0]>x[1] then b:=x[0]  //find brightest channel
                 else b:=x[1];
    if x[2]>b    then b:=x[2];
    if b>maxB_ then maxB_:=b;
    if b>2 then b:=2;
    if b<1.002 then result:=black else begin //if brightest channel is darker than 1, there is no overbrightness
      result:=x*(1-1/b);  //else result is
      x     :=x-result;
    end;
  end;

FUNCTION dist(x, y: T_floatColor): T_Float;
begin
  result := sqrt(sqr(x[0] - y[0]) + sqr(x[1] - y[1]) + sqr(x[2] - y[2]));
end;

FUNCTION sqDist(x, y: T_floatColor): T_Float;
begin
  result :=system. sqr(x[0] - y[0]) + system.sqr(x[1] - y[1]) + system.sqr(x[2] - y[2]);
end;

FUNCTION absDist(x, y: T_floatColor): T_Float;
begin
  result :=abs(x[0] - y[0]) + abs(x[1] - y[1]) + abs(x[2] - y[2]);
end;

FUNCTION absNorm(x: T_floatColor): T_Float;
begin
  result :=abs(x[0]) + abs(x[1]) + abs(x[2]);
end;

FUNCTION norm(x: T_floatColor): T_Float;
begin result :=sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]); end;

FUNCTION normed(x: T_floatColor): T_floatColor;
  begin
    result[2]:=1/sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
    result[0]:=x[0]*result[2];
    result[1]:=x[1]*result[2];
    result[2]:=x[2]*result[2];
  end;


FUNCTION sqNorm(x: T_floatColor): T_Float;
begin result :=(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]); end;

FUNCTION projectedColor(x:T_floatColor):T_24Bit;
  VAR k1,k2,k3,j:longint;
      aid:T_Float;
  begin
    k1:=0; k2:=1; k3:=2;
    if x[k2]<x[k1] then begin j:=k2; k2:=k1; k1:=j; end;
    if x[k3]<x[k1] then begin j:=k3; k3:=k1; k1:=j; end;
    if x[k3]<x[k2] then begin j:=k3; k3:=k2; k2:=j; end;
    //now we have x[k1]<=x[k2]<=x[k3]
    if x[k1]<0 then begin //if darkest channel is underbright...
      //distribute brightness:-//
      aid:=0.5*(x[k1]);        //
      x[k1]:=0;                //
      x[k2]:=x[k2]+aid;        //
      x[k3]:=x[k3]+aid;        //
      //---:distribute brightness
      if x[k2]<0 then begin //if originally second darkest channel is underbright...
        x[k3]:=max(0,x[k3]+x[k2]);
        x[k2]:=0;
      end;
    end; //if brightest channel is overbright...
    if x[k3]>1 then begin //if brightest channel is overbright...
      //distribute brightness:-//
      aid:=0.5*(x[k3]-1);      //
      x[k3]:=1;                //
      x[k2]:=x[k2]+aid;        //
      x[k1]:=x[k1]+aid;        //
      //---:distribute brightness
      if x[k2]>1 then begin //if originally second brightest channel is overbright...
        x[k1]:=min(1,x[k1]+x[k2]-1);
        x[k2]:=1;
      end;
    end; //if brightest channel is overbright...
    //now we have 0<=x[i]<=1 for all channels i
    if x[0]<0 then x[0]:=0;
    if x[1]<0 then x[1]:=0;
    if x[2]<0 then x[2]:=0;

    result[0]:=round(x[0]*255);
    result[1]:=round(x[1]*255);
    result[2]:=round(x[2]*255);
  end;

FUNCTION fprojectedColor(x:T_floatColor):T_floatColor;
  VAR k1,k2,k3,j:longint;
      aid:T_Float;
  begin
    k1:=0; k2:=1; k3:=2;
    if x[k2]<x[k1] then begin j:=k2; k2:=k1; k1:=j; end;
    if x[k3]<x[k1] then begin j:=k3; k3:=k1; k1:=j; end;
    if x[k3]<x[k2] then begin j:=k3; k3:=k2; k2:=j; end;
    //now we have x[k1]<=x[k2]<=x[k3]
    if x[k1]<0 then begin //if darkest channel is underbright...
      //distribute brightness:-//
      aid:=0.5*(x[k1]);        //
      x[k1]:=0;                //
      x[k2]:=x[k2]+aid;        //
      x[k3]:=x[k3]+aid;        //
      //---:distribute brightness
      if x[k2]<0 then begin //if originally second darkest channel is underbright...
        x[k3]:=max(0,x[k3]+x[k2]);
        x[k2]:=0;
      end;
    end; //if brightest channel is overbright...
    if x[k3]>1 then begin //if brightest channel is overbright...
      //distribute brightness:-//
      aid:=0.5*(x[k3]-1);      //
      x[k3]:=1;                //
      x[k2]:=x[k2]+aid;        //
      x[k1]:=x[k1]+aid;        //
      //---:distribute brightness
      if x[k2]>1 then begin //if originally second brightest channel is overbright...
        x[k1]:=min(1,x[k1]+x[k2]-1);
        x[k2]:=1;
      end;
    end; //if brightest channel is overbright...
    if x[0]<0 then x[0]:=0;
    if x[1]<0 then x[1]:=0;
    if x[2]<0 then x[2]:=0;
    //now we have 0<=x[i]<=1 for all channels i
    result[0]:=x[0];
    result[1]:=x[1];
    result[2]:=x[2];
  end;

FUNCTION greyLevel(x:T_floatColor):T_Float;  begin result:=(x[0]+x[1]+x[2])/ 3;      end;
FUNCTION greyLevel(x:T_24Bit):T_Float; begin result:=(x[0]+x[1]+x[2])/(3*255); end;

FUNCTION hue(x:T_Float):T_floatColor;
  begin
    while x<0 do x:=x+1;
    while x>1 do x:=x-1;
    x:=6*x;
    if x<1      then result:=newColor(1  ,x  ,0  )
    else if x<2 then result:=newColor(2-x,1  ,0  )
    else if x<3 then result:=newColor(0  ,1  ,x-2)
    else if x<4 then result:=newColor(0  ,4-x,1  )
    else if x<5 then result:=newColor(x-4,0  ,1  )
    else             result:=newColor(1  ,0  ,6-x);
  end;

FUNCTION toHSV(x:T_floatColor):T_floatColor;
  VAR mc:byte;
  begin
    if x[0]>x[1]      then begin result[2]:=x[0]; mc:=0; end
                      else begin result[2]:=x[1]; mc:=1; end;
    if x[2]>result[2] then begin result[2]:=x[2]; mc:=2; end;
    //result[2] now holds the brightest component of x
    if x[0]<x[1]      then result[1]:=x[0]
                      else result[1]:=x[1];
    if x[2]<result[1] then result[1]:=x[2];
    if result[1]=result[2] then mc:=3;
    //result[1] now holds the darkest component of x
    case mc of
      0: result[0]:=(  (x[1]-x[2])/(result[2]-result[1]))/6;
      1: result[0]:=(2+(x[2]-x[0])/(result[2]-result[1]))/6;
      2: result[0]:=(4+(x[0]-x[1])/(result[2]-result[1]))/6;
      3: result[0]:=0;
    end;
    if mc=3 then result[1]:=0
            else result[1]:=(result[2]-result[1])/result[2];
    while result[0]<0 do result[0]:=result[0]+1;
    while result[0]>1 do result[0]:=result[0]-1;
  end;

FUNCTION fromHSV(H,S,V:single):T_floatColor;
  VAR hi:byte;
      p,q,t:T_Float;
  begin
    while H<0 do H:=H+1;
    while H>1 do H:=H-1;

    hi:=trunc(H*6); H:=H*6-hi;
    p:=V*(1-S         );
    q:=V*(1-S*   H );
    t:=V*(1-S*(1-H));
    case hi of
      0,6: result:=newColor(V,t,p);
      1  : result:=newColor(q,V,p);
      2  : result:=newColor(p,V,t);
      3  : result:=newColor(p,q,V);
      4  : result:=newColor(t,p,V);
      5  : result:=newColor(V,p,q);
    end;
  end;


FUNCTION fromHSV(x:T_floatColor):T_floatColor;
  VAR hi:byte;
      p,q,t:T_Float;
  begin
    while x[0]<0 do x[0]:=x[0]+1;
    while x[0]>1 do x[0]:=x[0]-1;

    hi:=trunc(x[0]*6); x[0]:=x[0]*6-hi;
    p:=x[2]*(1-x[1]         );
    q:=x[2]*(1-x[1]*   x[0] );
    t:=x[2]*(1-x[1]*(1-x[0]));
    case hi of
      0,6: result:=newColor(x[2],t,p);
      1  : result:=newColor(q,x[2],p);
      2  : result:=newColor(p,x[2],t);
      3  : result:=newColor(p,q,x[2]);
      4  : result:=newColor(t,p,x[2]);
      5  : result:=newColor(x[2],p,q);
    end;
  end;

//FUNCTION toCumulative  (hist:T_histogram):T_histogram;
//  VAR i:longint;
//  begin
//    result[0]:=hist[0];
//    for i:=1 to 255 do result[i]:=hist[i]+result[i-1];
//  end;
//
//FUNCTION fromCumulative(hist:T_histogram):T_histogram;
//  VAR i:longint;
//  begin
//    for i:=255 downto 1 do result[i]:=hist[i]-hist[i-1];
//    result[0]:=hist[0];
//  end;
//
//FUNCTION smoothKernel  (sigma:T_Float):T_histogram;
//  VAR i:longint;
//
//  begin
//    if sigma<1e-3 then sigma:=1E-3;
//    for i:=0 to 255 do result[i]:=exp(-sqr(i/sigma));
//
//    sigma:=0;
//    for i:=0 to 255 do sigma:=sigma+result[i];
//    if sigma>0 then sigma:=1/sigma
//    else sigma:=0;
//    for i:=0 to 255 do result[i]:=result[i]*sigma;
//  end;
//
//PROCEDURE smoothInc(VAR hist:T_histogram; incIdx,incWgt:single);
//  VAR i:longint;
//  begin
//    for i:=max(0  ,floor(incIdx-2)) to
//           min(255,ceil (incIdx+2)) do begin
//      hist[i]:=hist[i]+incWgt*(2-abs(i-incIdx));
//    end;
//  end;
//
//FUNCTION smooth        (hist,kernel:T_histogram):T_histogram;
//  VAR i,j,jMax:longint;
//  begin
//    jMax:=0;
//    while (jMax<255) and (kernel[jMax]>1E-4) do inc(jMax);
//    for j:=0 to 255 do begin
//      result[j]:=0;
//      i:=0;
//      for i:=max(-j,-jMax) to min(255-j,jMax) do
//        result[j]:=result[j]+hist[j+i]*kernel[abs(j)];
//    end;
//  end;
//
//
//FUNCTION smooth(hist:T_histogram; sigma:T_Float):T_histogram;
//  VAR i,j :longint;
//      kern:T_histogram;
//  begin
//    if sigma<1e-3 then sigma:=1E-3;
//    for i:=0 to 255 do kern[i]:=exp(-sqr(i/sigma));
//    for j:=0 to 255 do begin
//      result[j]:=0;
//      for i:=0 to 255 do result[j]:=result[j]+hist[i]*kern[abs(i-j)];
//    end;
//    //normalization:----------------------------------
//    kern[0]:=0;
//    for i:=0 to 255 do kern[0]:=kern[0]+result[i];
//    if kern[0]>0 then kern[0]:=1/kern[0]
//    else kern[0]:=0;
//    for i:=0 to 255 do result[i]:=result[i]*kern[0];
//    //----------------------------------:normalization
//  end;

//FUNCTION smoothModeCnt (hist:T_histogram; modeCount:longint):T_histogram;
//  VAR i,j,numberOfModes:longint;
//      kern:T_histogram;
//      sigma:single;
//  begin
//    sigma:=0.001;
//
//    repeat
//      for i:=0 to 255 do kern[i]:=exp(-sqr(i/sigma));
//      for j:=0 to 255 do begin
//        result[j]:=0;
//        for i:=0 to 255 do result[j]:=result[j]+hist[i]*kern[abs(i-j)];
//      end;
//      numberOfModes:=0;
//      if result[  0]>result[  1] then inc(numberOfModes);
//      if result[255]>result[254] then inc(numberOfModes);
//      for i:=1 to 254 do if (result[i]>result[i-1]) and (result[i]>result[i+1]) then inc(numberOfModes);
//      sigma:=sigma*1.4;
//    until numberOfModes<=modeCount;
//    writeln('numberOfModes=',numberOfModes,' of ',modeCount);
//    //normalization:----------------------------------
//    kern[0]:=0;
//    for i:=0 to 255 do kern[0]:=kern[0]+result[i];
//    if kern[0]>0 then kern[0]:=1/kern[0]
//    else kern[0]:=0;
//    for i:=0 to 255 do result[i]:=result[i]*kern[0];
//    //----------------------------------:normalization
//  end;
//
//
//FUNCTION quantileFromHist(hist:T_histogram; q:T_Float):T_Float;
//  VAR i:longint;
//  begin
//    i:=0;
//    while (i<255) and (hist[i]<q) do begin
//      hist[i+1]:=hist[i]+hist[i+1];
//      inc(i);
//    end;
//    if i<1 then result:=0
//           else result:=i-1+(q-hist[i-1])/(hist[i]-hist[i-1]);
//  end;
//
//FUNCTION  quantileFromCum(hist:T_histogram; q:T_Float):T_Float;
//  VAR i:longint;
//  begin
//    i:=0;
//    while hist[i]<q do inc(i);
//    result:=i-1+(q-hist[i-1])/(hist[i]-hist[i-1]);
//  end;
//
//FUNCTION variance      (hist:T_histogram):T_Float;
//  VAR i:longint;
//      mean:T_Float;
//  begin
//    //normalize histogram:----------------------//
//    mean:=0;                                    //
//    for i:=0 to 255 do mean:=mean+hist[i];      //
//    if (mean<0.999) or (mean>1.001) then begin  //
//      mean:=1/mean;                             //
//      for i:=0 to 255 do hist[i]:=hist[i]*mean; //
//    end;                                        //
//    //------------------------:normalize histogram
//    //compute mean:--------------------------//
//    mean:=0;                                 //
//    for i:=0 to 255 do mean:=mean+i*hist[i]; //
//    //----------------------------:compute mean
//    //compute variance:------------------------------------//
//    result:=0;                                             //
//    for i:=0 to 255 do result:=result+sqr(i-mean)*hist[i]; //
//    //--------------------------------------:compute variance
//  end;
//
//FUNCTION smoothEntry   (hist:T_histogram; index:T_Float):T_Float;
//  begin
//    if      index<=0   then result:=hist[0]
//    else if index>=255 then result:=hist[255]
//    else result:=hist[trunc(index)  ]*(1-frac(index))+
//                 hist[trunc(index)+1]*   frac(index);
//  end;

//PROCEDURE clear(VAR hist:T_histogram); VAR i:longint; begin for i:=0 to 255 do hist[i]:=0; end;

//FUNCTION nearestMode(VAR hist:T_histogram; x:byte):byte;
//  VAR found:boolean;
//  begin
//    found:=false;
//    repeat
//      if      (x>0)   and (hist[x-1]>hist[x]) then dec(x)
//      else if (x<255) and (hist[x+1]>hist[x]) then inc(x)
//      else found:=true;
//    until found;
//    result:=x;
//  end;
//
//FUNCTION mainMode(VAR hist:T_histogram):byte;
//  VAR i:longint;
//  begin
//    result:=0;
//    for i:=1 to 255 do if hist[i]>hist[result] then result:=i;
//  end;

//
//PROCEDURE writeToFile(hist:T_histogram; fname:string);
//  VAR f:text;
//      i:longint;
//  begin
//    assign(f,fname);
//    rewrite(f);
//    for i:=0 to 255 do writeln(f,i,' ',hist[i]);
//    close(f);
//  end;
//
//PROCEDURE writeToFile(h1,h2,h3:T_histogram; fname:string);
//  VAR f:text;
//      i:longint;
//  begin
//    assign(f,fname);
//    rewrite(f);
//    for i:=0 to 255 do writeln(f,i,' ',h1[i],' ',h2[i],' ',h3[i]);
//    close(f);
//  end;

{$endif}
