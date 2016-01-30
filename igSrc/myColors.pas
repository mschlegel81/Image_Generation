UNIT myColors;
INTERFACE
USES math;
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

OPERATOR =(CONST x,y:T_24Bit):boolean; inline;
OPERATOR :=(CONST x:T_24Bit):T_floatColor; inline;
OPERATOR :=(CONST x:T_floatColor ):T_24Bit; inline;
FUNCTION newColor(CONST x,y,z:T_Float):T_floatColor; inline;
OPERATOR +(CONST x,y:T_floatColor): T_floatColor; inline;
OPERATOR -(CONST x,y:T_floatColor): T_floatColor; inline;
OPERATOR *(CONST x:T_floatColor; CONST y:T_Float): T_floatColor; inline;
OPERATOR *(CONST y:T_Float; CONST x:T_floatColor): T_floatColor; inline;
OPERATOR *(CONST x,y:T_floatColor): T_Float; inline;
FUNCTION getOverbright(VAR x:T_floatColor):T_floatColor;
FUNCTION projectedColor(x:T_floatColor):T_24Bit;

//FUNCTION greyLevel(CONST x:T_floatColor):T_Float;
//FUNCTION greyLevel(CONST x:T_24Bit):T_Float;
FUNCTION hue(x:T_Float):T_floatColor;
FUNCTION toHSV(CONST x:T_floatColor):T_floatColor;
FUNCTION fromHSV(x:T_floatColor):T_floatColor;
FUNCTION fromHSV(H,S,V:single):T_floatColor;

CONST
   HISTOGRAM_ADDITIONAL_SPREAD=128;

TYPE

  { T_histogram }


  T_histogram=object
    private
      isIncremental:boolean;
      bins:array [-HISTOGRAM_ADDITIONAL_SPREAD..255+HISTOGRAM_ADDITIONAL_SPREAD] of single;
      PROCEDURE switch;
      PROCEDURE incBin(CONST index:longint; CONST increment:single);
    public
      CONSTRUCTOR create;
      CONSTRUCTOR createSmoothingKernel(CONST sigma:single);
      DESTRUCTOR destroy;
      PROCEDURE clear;
      PROCEDURE putSample(CONST value:single; CONST weight:single=1);
      PROCEDURE putSampleSmooth(CONST value:single; CONST weight:single=1);
      PROCEDURE smoothen(CONST sigma:single);
      PROCEDURE smoothen(CONST kernel:T_histogram);

      FUNCTION percentile(CONST percent:single):single;
      FUNCTION median:single;
      FUNCTION mightHaveOutOfBoundsValues:boolean;
      FUNCTION mode:single;
  end;

  { T_compoundHistogram }

  T_compoundHistogram=object
    R,G,B:T_histogram;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE putSample(CONST value:T_floatColor; CONST weight:single=1);
    PROCEDURE putSampleSmooth(CONST value:T_floatColor; CONST weight:single=1);
    PROCEDURE smoothen(CONST sigma:single);
    PROCEDURE smoothen(CONST kernel:T_histogram);
  end;

  T_quantizationTreeSample=record sum:T_floatColor; count:longint; end;

  T_quantizationTree=object
    sample:array of T_quantizationTreeSample;
    //CONSTRUCTOR create;


  end;

IMPLEMENTATION
//FUNCTION limited(c:T_floatColor):T_floatColor; inline;
//  begin
//    if c[0]<0 then result[0]:=0 else if c[0]>1 then result[0]:=1 else result[0]:=c[0];
//    if c[1]<0 then result[1]:=0 else if c[1]>1 then result[1]:=1 else result[1]:=c[1];
//    if c[2]<0 then result[2]:=0 else if c[2]>1 then result[2]:=1 else result[2]:=c[2];
//  end;
//
//FUNCTION lowLimited(c:T_floatColor):T_floatColor; inline;
//  begin
//    if c[0]<0 then result[0]:=0 else result[0]:=c[0];
//    if c[1]<0 then result[1]:=0 else result[1]:=c[1];
//    if c[2]<0 then result[2]:=0 else result[2]:=c[2];
//  end;
//
//FUNCTION subjectiveGrey(c:T_floatColor):T_floatColor; inline;
//  begin
//    result[0]:=0.2126*c[0]+0.7152*c[1]+0.0722*c[2];
//    result[1]:=result[0];
//    result[2]:=result[0];
//  end;
//
//FUNCTION sepia(c:T_floatColor):T_floatColor; inline;
//  begin
//    result[0]:=safeGamma(c[0],0.5);
//    result[1]:=c[1];
//    result[2]:=sqr(c[2]);
//  end;

//FUNCTION tint(c:T_floatColor; h:single):T_floatColor; inline;
//  begin
//    result:=toHSV(c);
//    result[0]:=h;
//    result[1]:=1;
//    result:=fromHSV(result);
//  end;
//
//FUNCTION hue(c:T_floatColor; h:single):T_floatColor; inline;
//  begin
//    result:=toHSV(c);
//    result[0]:=h;
//    result:=fromHSV(result);
//  end;


OPERATOR =(CONST x,y:T_24Bit):boolean; inline;
  begin
    result:=(x[0]=y[0])
        and (x[1]=y[1])
        and (x[2]=y[2]);
  end;

OPERATOR :=(CONST x:T_24Bit):T_floatColor;
  begin
    result[0]:=x[0]/255;
    result[1]:=x[1]/255;
    result[2]:=x[2]/255;
  end;

OPERATOR :=(CONST x:T_floatColor):T_24Bit;
  begin
    result[0]:=round(min(255,max(0,x[0]*255)));
    result[1]:=round(min(255,max(0,x[1]*255)));
    result[2]:=round(min(255,max(0,x[2]*255)));
  end;

FUNCTION newColor(CONST x,y,z:T_Float):T_floatColor;
  begin
    result[0] :=x;
    result[1] :=y;
    result[2] :=z;
  end;

OPERATOR +(CONST x,y:T_floatColor):T_floatColor; begin result[0]:=x[0]+y[0]; result[1]:=x[1]+y[1]; result[2]:=x[2]+y[2]; end;
OPERATOR -(CONST x,y:T_floatColor):T_floatColor; begin result[0]:=x[0]-y[0]; result[1]:=x[1]-y[1]; result[2]:=x[2]-y[2]; end;
OPERATOR *(CONST x:T_floatColor; CONST y:T_Float):T_floatColor; begin result[0]:=x[0]*y; result[1]:=x[1]*y; result[2]:=x[2]*y; end;
OPERATOR *(CONST y:T_Float; CONST x:T_floatColor):T_floatColor; begin result[0]:=x[0]*y; result[1]:=x[1]*y; result[2]:=x[2]*y; end;
OPERATOR *(CONST x,y:T_floatColor):T_Float; begin result:=x[0]*y[0]+x[1]*y[1]+x[2]*y[2]; end;

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

//FUNCTION greyLevel(CONST x:T_floatColor):T_Float;  begin result:=(x[0]+x[1]+x[2])/ 3;      end;
//FUNCTION greyLevel(CONST x:T_24Bit):T_Float; begin result:=(x[0]+x[1]+x[2])/(3*255); end;

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

FUNCTION toHSV(CONST x:T_floatColor):T_floatColor;
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

{ T_compoundHistogram }

CONSTRUCTOR T_compoundHistogram.create;
  begin
    r.create;
    g.create;
    b.create;
  end;

DESTRUCTOR T_compoundHistogram.destroy;
  begin
    r.destroy;
    g.destroy;
    b.destroy;
  end;

PROCEDURE T_compoundHistogram.putSample(CONST value: T_floatColor; CONST weight:single=1);
  begin
    r.putSample(value[0],weight);
    g.putSample(value[1],weight);
    b.putSample(value[2],weight);
  end;

PROCEDURE T_compoundHistogram.putSampleSmooth(CONST value:T_floatColor; CONST weight:single=1);
  begin
    r.putSampleSmooth(value[0],weight);
    g.putSampleSmooth(value[1],weight);
    b.putSampleSmooth(value[2],weight);
  end;

PROCEDURE T_compoundHistogram.smoothen(CONST sigma: single);
  VAR kernel:T_histogram;
  begin
    kernel.createSmoothingKernel(sigma);
    smoothen(kernel);
    kernel.destroy;
  end;

PROCEDURE T_compoundHistogram.smoothen(CONST kernel: T_histogram);
  begin
    r.smoothen(kernel);
    g.smoothen(kernel);
    b.smoothen(kernel);
  end;

{ T_histogram }

PROCEDURE T_histogram.switch;
  VAR tmp:double;
      i:longint;
  begin
    if isIncremental then begin
      for i:=Low(bins)+1 to high(bins) do bins[i]:=bins[i]+bins[i-1];
    end else begin
      for i:=high(bins) downto Low(bins)+1 do bins[i]:=bins[i]-bins[i-1];
    end;
    isIncremental:=not(isIncremental);
  end;

CONSTRUCTOR T_histogram.create;
  begin
    clear;
  end;

CONSTRUCTOR T_histogram.createSmoothingKernel(CONST sigma: single);
  VAR i:longint;
      s:double;
  begin
    clear;
    if sigma<1E-3 then s:=1E3 else s:=1/sigma;
    for i:=-HISTOGRAM_ADDITIONAL_SPREAD to HISTOGRAM_ADDITIONAL_SPREAD do bins[i]:=exp(-sqr(i*s));
  end;

DESTRUCTOR T_histogram.destroy;
  begin end;//Pro forma destructor

PROCEDURE T_histogram.clear;
  VAR i:longint;
  begin
    isIncremental:=false;
    for i:=Low(bins) to high(bins) do bins[i]:=0;
  end;

PROCEDURE T_histogram.incBin(CONST index: longint; CONST increment: single);
  VAR i:longint;
  begin
    if      index<Low (bins) then i:=Low(bins)
    else if index>high(bins) then i:=high(bins)
    else i:=index;
    bins[i]:=bins[i]+increment;
  end;

PROCEDURE T_histogram.putSample(CONST value: single; CONST weight: single);
  begin
    if isIncremental then switch;
    if isNan(value) or isInfinite(value) then exit;
    incBin(round(value*255),weight);
  end;

PROCEDURE T_histogram.putSampleSmooth(CONST value: single; CONST weight: single
  );
  VAR i:longint;
  begin
    if isIncremental then switch;
    if isNan(value) or isInfinite(value) then exit;
    i:=round(value*255);
    incBin(i-1,weight*0.25);
    incBin(i  ,weight*0.5 );
    incBin(i+1,weight*0.25);
  end;

PROCEDURE T_histogram.smoothen(CONST sigma: single);
  VAR kernel:T_histogram;
  begin
    if isIncremental then switch;
    kernel.createSmoothingKernel(sigma);
    smoothen(kernel);
    kernel.destroy;
  end;

PROCEDURE T_histogram.smoothen(CONST kernel: T_histogram);
  VAR temp:T_histogram;
      i,j:longint;
      sum1,sum2:double;
  begin
    if isIncremental then switch;
    temp.create;
    for i:=0 to length(bins)-1 do begin
      sum1:=0;
      sum2:=0;
      for j:=1-length(bins) to length(bins)-1 do if (i+j>=0) and (i+j<length(bins)) then begin
        sum1:=sum1+kernel.bins[abs(j)]*bins[i+j];
        sum2:=sum2+kernel.bins[abs(j)];
      end;
      temp.bins[i]:=sum1/sum2;
    end;
    temp.destroy;
  end;

FUNCTION T_histogram.percentile(CONST percent: single): single;
  VAR absVal:single;
      i:longint;
  begin
    if not(isIncremental) then switch;
    absVal:=percent/100*bins[high(bins)];
    if bins[Low(bins)]>absVal then exit(Low(bins)/255);
    for i:=Low(bins)+1 to high(bins) do if (bins[i-1]<=absVal) and (bins[i]>absVal) then result:=i/255;
    result:=(high(bins)+1)/255;
  end;

FUNCTION T_histogram.median: single;
  begin
    result:=percentile(50);
  end;

FUNCTION T_histogram.mightHaveOutOfBoundsValues: boolean;
  begin
    result:=(bins[Low(bins)]>0) or (bins[high(bins)]>0);
  end;

FUNCTION T_histogram.mode: single;
  VAR i:longint;
      ir:longint=Low(bins);
  begin
    if isIncremental then switch;
    for i:=Low(bins)+1 to high(bins) do if bins[i]>bins[ir] then ir:=i;
    result:=ir/255;
  end;

end.
