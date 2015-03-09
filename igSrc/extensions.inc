{$ifdef include_interface}
TYPE T_smoothSample=record
                      count:longint;
                      sum:T_floatColor;
                    end;
     T_colorTree=array of T_smoothSample;
//Color operations:------------------------------------------

PROCEDURE quantize2   (VAR inOut:T_FloatMap  ; numberOfColors,filterRadius:longint);
PROCEDURE shineImage  (VAR inOut:T_FloatMap);
//-------------------------------------------:Color operations
//Filters:----------------------------------------------------
PROCEDURE sketch(VAR inOut:T_FloatMap; gradSigma,density,tolerance,alpha:single; onWhite:boolean);

//----------------------------------------------------:Filters

{$endif}
{$ifdef include_implementation}
PROCEDURE finishTree(VAR tree:T_colorTree; colors:longint);
  VAR i,j,k:longint;
      temp:T_smoothSample;
      minDist,newDist:double;
  begin
    //sort tree by number of samples:-----------------------------------------
    for i:=0 to length(tree)-2 do for j:=i+1 to length(tree)-1 do
    if tree[i].count<tree[j].count then begin
      temp:=tree[i]; tree[i]:=tree[j]; tree[j]:=temp;
    end;
    //-----------------------------------------:sort tree by number of samples
    j:=length(tree)-1;
    while (j>0) and (tree[j].count=0) do dec(j);
    setLength(tree,j+1);
    //merge tree to obtain desired color count:-------------------------------
    while length(tree)>colors do begin
      j:=length(tree)-1;
      minDist:=1E20;
      for i:=0 to length(tree)-2 do begin
        newDist:=sqDist(tree[i].sum*(1/tree[i].count),tree[j].sum*(1/tree[j].count));
        if (newDist<minDist) then begin minDist:=newDist; k:=i; end;
      end;
      tree[k].sum   :=tree[k].sum   +tree[j].sum;
      tree[k].count :=tree[k].count +tree[j].count;
      setLength(tree,length(tree)-1);
      while (k>0) and (tree[k  ].count>tree[k-1].count) do begin
        temp:=tree[k]; tree[k]:=tree[k-1]; tree[k-1]:=temp; dec(k);
      end;
    end;
    //-------------------------------:merge tree to obtain desired color count
    //transform color sums to average colors:----------
    for i:=0 to length(tree)-1 do with tree[i] do
      if count>0 then sum:=sum*(1/count);
    //----------:transform color sums to average colors
  end;

PROCEDURE initTree(VAR tree:T_colorTree);
  VAR i:longint;
  begin
    setLength(tree,16*16*16);
    for i:=0 to 16*16*16-1 do with tree[i] do begin
      sum:=black; count:=0;
    end;
  end;

FUNCTION getQuantizedColor(VAR tree:T_colorTree; c:T_floatColor):T_floatColor; inline;
  VAR newDist,dist1:double;
      col1  :T_floatColor;
      i:longint;
  begin
    dist1:=sqDist(c,tree[0].sum); col1:=tree[0].sum;
    for i:=1 to length(tree)-1 do begin
      newDist:=sqDist(c,tree[i].sum);
      if newDist<dist1 then begin dist1:=newDist; col1:=tree[i].sum;  end;
    end;
    result:=col1;
  end;

FUNCTION getQuantizedColorIndex(VAR tree:T_colorTree; c:T_floatColor):longint; inline;
  VAR newDist,dist1:double;
      i:longint;
  begin
    dist1:=sqDist(c,tree[0].sum); result:=0;
    for i:=1 to length(tree)-1 do begin
      newDist:=sqDist(c,tree[i].sum);
      if newDist<dist1 then begin dist1:=newDist; result:=i;  end;
    end;
  end;

PROCEDURE addSample(VAR tree:T_colorTree; c:T_24Bit); inline;
  begin
    with tree[(c[0] shr 4)+16*((c[1] shr 4)+16*(c[2] shr 4))] do begin inc(count); sum:=sum+c; end;
  end;

PROCEDURE quantize(VAR inOut:T_FloatMap  ; numberOfColors:longint);
  VAR pt:P_floatColor;
      tree:T_colorTree;
      i:longint;
  begin
    pt:=inOut.rawData;
    initTree(tree);
    for i:=0 to inOut.size-1 do addSample(tree,pt[i]);
    finishTree(tree,numberOfColors);
    for i:=0 to inOut.size-1 do pt[i]:=getQuantizedColor(tree,pt[i]);
    setLength(tree,0);
  end;

PROCEDURE quantize2(VAR inOut:T_FloatMap  ; numberOfColors,filterRadius:longint);
  VAR locCount:array [0..255] of word;
      pt:P_floatColor;
      pb:PByte;
      indexMap:T_ByteMap;
      tree:T_colorTree;
      xRes,yRes,
      i,j,
      x,y,ix,iy:longint;
  begin
    pt:=inOut.rawData;
    xRes:=inout.width;
    yRes:=inout.height;
    initTree(tree);
    for i:=0 to inOut.size-1 do addSample(tree,pt[i]);
    finishTree(tree,numberOfColors);
    indexMap.create(inout.width,inout.height);
    pb:=indexMap.rawData;
    for i:=0 to inOut.size-1 do pb[i]:=getQuantizedColorIndex(tree,pt[i]);
    for y:=0 to yRes-1 do
    for x:=0 to xRes-1 do begin
      for i:=0 to numberOfColors-1 do locCount[i]:=0;
      for iy:=max(0,y-filterRadius) to min(yRes-1,y+filterRadius) do
      for ix:=max(0,x-filterRadius) to min(xRes-1,x+filterRadius) do
      if sqr(ix-x)+sqr(iy-y)<=filterRadius*filterRadius then inc(locCount[pb[ix+iy*xRes]]);
      j:=0;
      for i:=1 to numberOfColors-1 do if locCount[i]>locCount[j] then j:=i;
      pt[x+y*xRes]:=tree[j].sum;
    end;
    indexMap.destroy;
    setLength(tree,0);
  end;

PROCEDURE gradMapBlur(VAR inout:T_FloatMap; sigma:T_Float);
  VAR xRes,yRes,x,y,z,c:longint;
      pt  :P_floatColor;
      kern:PSingle;
      rad :longint;
      sum :single;
  begin
    if sigma>0 then begin
      //intialize kernel:------------------------//
      rad:=round(3*sigma);                       //
      if rad<2 then rad:=2;                      //
      getMem (kern,rad*sizeOf(single));          //
      sum:=0;                                    //
      for z:=0 to rad-1 do begin                 //
        kern[z]:=exp(-sqr(z/sigma));             //
        if z=0 then sum:=sum+  kern[z]           //
               else sum:=sum+2*kern[z];          //
      end;                                       //
      sum:=1/sum;                                //
      for z:=0 to rad-1 do kern[z]:=kern[z]*sum; //
      //--------------------------:intialize kernel
      pt  :=inout.rawData;
      xRes:=inout.width;
      yRes:=inout.height;
      for c:=0 to 1 do begin
        //1D folding in x-direction:--------------------------------------------//
        for y:=0 to yRes-1 do for x:=0 to xRes-1 do begin                       //
                                           sum:=    kern[ 0]*pt[x+  y*xRes][c]; //
          for z:=max(-x,-rad) to -1     do sum:=sum+kern[-z]*pt[x+z+y*xRes][c]; //
          for z:=1 to min(xRes-x,rad)-1 do sum:=sum+kern[ z]*pt[x+z+y*xRes][c]; //
          pt[x+y*xRes][2]:=sum;                                                 //
        end;                                                                    //
        //----------------------------------------------:1D folding in x-direction
        //for x:=0 to xRes-1 do for y:=0 to yRes-1 do pt[x+y*xRes][c]:=pt[x+y*xRes][2];
        //1D folding in y-direction:----------------------------------------------//
        for x:=0 to xRes-1 do for y:=0 to yRes-1 do begin                         //
                                           sum:=    kern[ 0]*pt[x+   y *xRes][2]; //
          for z:=max(-y,-rad) to -1     do sum:=sum+kern[-z]*pt[x+(z+y)*xRes][2]; //
          for z:=1 to min(yRes-y,rad)-1 do sum:=sum+kern[ z]*pt[x+(z+y)*xRes][2]; //
          pt[x+y*xRes][c]:=sum;                                                   //
        end;                                                                      //
        //------------------------------------------------:1D folding in y-direction
      end;
      for z:=0 to xres*yres-1 do pt[z][2]:=0.5;
      freeMem(kern,rad*sizeOf(single)); //finaliize kernel
    end;
  end;




{$MACRO ON}



PROCEDURE shineImage(VAR inOut:T_FloatMap);
  VAR temp:T_FloatMap;
      po,pt:P_floatColor;
      co,ct:T_floatColor;
      fak:double;
      xres,yres,x,y,ix,iy,step:longint;
      anyOverbright:boolean;
  begin
    xres:=inOut.width;
    yres:=inOut.height;

    temp.create(xres,yres);
    po:=inOut .rawData;
    pt:=temp.rawData;
    step:=1;
    repeat
      anyOverbright:=false;
      for x:=0 to inOut.size-1 do begin
        co:=po[x];
        ct:=co;
        fak:=max(1,(co[0]+co[1]+co[2])*(1/3));
        co:=co*(1/fak);
        po[x]:=co;
        pt[x]:=ct-co;
        anyOverbright:=anyOverbright or (fak>1.1);
      end;
      for y:=0 to yres-1 do
      for x:=0 to xres-1 do begin
        co:=pt[x+y*xres];
        if co<>black then begin
          co:=co*(1/(2+4*step));
          for iy:=max(0,y-step) to min(yres-1,y+step) do po[x+iy*xres]:=po[x+iy*xres]+co;
          for ix:=max(0,x-step) to min(xres-1,x+step) do po[ix+y*xres]:=po[ix+y*xres]+co;
        end;
      end;
      inc(step,step);
    until (step>inOut.diagonal*0.2) or not(anyOverbright);
    temp.destroy;
  end;

PROCEDURE sketch(VAR inOut:T_FloatMap; gradSigma,density,tolerance,alpha:single; onWhite:boolean);
  VAR dirTab:array[0..255,0..1] of single;
  PROCEDURE getDirection(d:byte; OUT dx,dy:single); inline;
    begin
      if random<0.5 then begin dx:=-dirTab[d,0]; dy:=-dirTab[d,1]; end
                    else begin dx:= dirTab[d,0]; dy:= dirTab[d,1]; end;
    end;

  VAR temp:T_FloatMap;
      x,y,i,ix,iy,xRes,yRes:longint;
      lineColor:T_floatColor;
      vx,vy,wx,wy:single;
      pt,po:P_floatColor;
      pg,pc:PByte;
      coverMap,grad:T_ByteMap;
      pset:boolean;
      totalCover:double;

  FUNCTION smoothPixel(x,y:longint; w:single; col:T_floatColor):boolean; inline;
    begin
      if (x>=0) and (x<xRes) and (y>=0) and (y<yRes) and (dist(po[x+y*xRes],col)<=tolerance) then begin
        x:=x+y*xRes;
        pt[x]:=          pt[x]+(col-pt[x])*w;
        totalCover:=totalCover+(255-pc[x])*w;
        pc[x]:=    round(pc[x]+(255-pc[x])*w);
        result:=true;
      end else result:=false;
    end;

  begin
    directionMap(inout,gradSigma,grad);
    if alpha>1 then alpha:=1;
    if density>1 then density:=1;
    totalCover:=0;
    for i:=0 to 255 do begin
      dirTab[i,0]:=cos(pi/2+pi*i/256);
      dirTab[i,1]:=sin(pi/2+pi*i/256);
      if abs(dirTab[i,0])>abs(dirTab[i,1]) then begin
        dirTab[i,1]:=dirTab[i,1]/dirTab[i,0];
        dirTab[i,0]:=1;
      end else begin
        dirTab[i,0]:=dirTab[i,0]/dirTab[i,1];
        dirTab[i,1]:=1;
      end;
    end;
    xRes:=inOut.width;
    yRes:=inOut.height;
    temp.createCopy(inOut);
    coverMap.create(xRes,yRes);
    po:=temp    .rawData;
    pt:=inOut   .rawData;
    pg:=grad    .rawData;
    pc:=coverMap.rawData;
    if onWhite then for x:=0 to xRes*yRes-1 do pt[x]:=white24Bit
               else for x:=0 to xRes*yRes-1 do pt[x]:=po[x];
    for x:=0 to xRes*yRes-1 do pc[x]:=0;
    density:=density/(0.5*inOut.diagonal*0.05);
    while totalCover<0.025*xRes*yRes*inOut.diagonal*density*255.0 do begin
      for y:=0 to yRes-1 do for x:=0 to xRes-1 do if random<density then begin
        getDirection(pg[x+y*xRes],vx,vy);
        lineColor:=po[x+y*xRes]+newVector(random-0.5,random-0.5,random-0.5)*0.1*tolerance;
        for i:=0 to round(random*inOut.diagonal*0.05*alpha) do begin
          wx:=x+i*vx; ix:=trunc(wx); wx:=wx-ix;
          wy:=y+i*vy; iy:=trunc(wy); wy:=wy-iy;
          pset:=smoothPixel(ix  ,iy  ,(1-wx)*(1-wy)*alpha,lineColor);
          pset:=smoothPixel(ix+1,iy  ,(  wx)*(1-wy)*alpha,lineColor) or pset;
          pset:=smoothPixel(ix  ,iy+1,(1-wx)*(  wy)*alpha,lineColor) or pset;
          pset:=smoothPixel(ix+1,iy+1,(  wx)*(  wy)*alpha,lineColor) or pset;
          if not(pset) then break;
        end;
      end;
    end;
    grad.destroy;
    temp.destroy;
    coverMap.destroy;
  end;
{$endif}
