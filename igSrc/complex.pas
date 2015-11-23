UNIT complex;
{$MACRO ON}
INTERFACE
USES {$ifdef UNIX}cmem,cthreads,{$endif}dos,sysutils,math,mypics;
TYPE
  T_compBaseT ={$ifdef doubleAccuracy} double; {$else} single; {$endif}
  T_Chunk     =array[0..1023] of T_compBaseT;
  T_Complex   =record re,im:T_compBaseT; valid:boolean; end;
  P_Complex   =^T_Complex;
{$ifdef doubleAccuracy}
{$WARNING T_compBaseT should be chosen depending on the main program. It's value is DOUBLE.}
{$else}
{$WARNING T_compBaseT should be chosen depending on the main program. It's value is SINGLE.}
{$endif}



CONST
   C_fileExtension:array[0..3] of string[4]=('.bmp','.jpg','.gif','.png');
   upperThreshold=1E38;  //1E38 for single; 1E150 for double
   lowerThreshold=1/upperThreshold;
   II:T_Complex=(re:0; im:1; valid:true);
   C_invalidComplex:T_Complex=(re:Nan; im:Nan; valid:false);

TYPE
{  T_darts=object
    delta:P_Complex;
    count:longint;

    CONSTRUCTOR create(numberOfDarts:longint);
    CONSTRUCTOR createOverlapping(numberOfDarts:longint);
    DESTRUCTOR  destroy;
    FUNCTION getD(i,j:longint):T_compBaseT;
    PROPERTY d[i,j:longint]:T_compBaseT read getD; default;
  end;}

  T_scaler=object

    absoluteZoom,invAbsoluteZoom:T_compBaseT;
    offsetX,offsetY:T_compBaseT;

    relativeZoom ,
    screenRefX,screenRefY:T_compBaseT;
    worldRefX,worldRefY:T_compBaseT;
    screenWidth  ,screenHeight:longint;
    FUNCTION screenCenterX:T_compBaseT;
    FUNCTION screenCenterY:T_compBaseT;

    PROCEDURE recalc;
    CONSTRUCTOR create  (width,height:longint; centerX,centerY,zoom:T_compBaseT);
    PROCEDURE   recreate(width,height:longint; centerX,centerY,zoom:T_compBaseT);
    DESTRUCTOR  destroy;
    FUNCTION    transform(CONST x,y:T_compBaseT; CONST rotateBy:T_Complex):T_Complex;
    FUNCTION    transform(x,y:T_compBaseT   ):T_Complex;
    FUNCTION    transform(x  :T_Complex ):T_Complex;
    PROCEDURE   mrofsnart(VAR x,y:T_Chunk; chunkFill:word);
    FUNCTION    mrofsnart(x,y:T_compBaseT   ):T_Complex;
    FUNCTION    mrofsnart(x  :T_Complex ):T_Complex;
    PROCEDURE   rescale (newWidth,newHeight:longint);
    PROCEDURE   recenter(newX,newY:T_compBaseT);
    PROCEDURE   moveCenter(dx,dy:T_compBaseT);
    PROCEDURE   rezoom  (newZoom  :T_compBaseT);
    PROCEDURE   chooseScreenRef(x,y:T_compBaseT);
    PROCEDURE   chooseWorldRef (x,y:T_compBaseT);
    FUNCTION    screenDiagonal:T_compBaseT;
  end;

PROCEDURE drawGrid(VAR img:T_FloatMap; gridColor:T_floatColor; gridCover:T_compBaseT; scaler:T_scaler);
OPERATOR :=(x:T_compBaseT):T_Complex; inline;

OPERATOR +(x,y:T_Complex):T_Complex; inline;
OPERATOR -(x,y:T_Complex):T_Complex; inline;
OPERATOR *(x,y:T_Complex):T_Complex; inline;
OPERATOR /(x,y:T_Complex):T_Complex; inline;
OPERATOR **(x:T_Complex; y:longint):T_Complex; inline;
OPERATOR **(x:T_Complex; y:T_compBaseT):T_Complex; inline;
OPERATOR **(x,y:T_Complex):T_Complex; inline;
FUNCTION abs(x:T_compBaseT):T_compBaseT; inline;
FUNCTION abs(x:T_Complex):T_compBaseT; inline;
FUNCTION arg(x:T_Complex):T_compBaseT; inline;
FUNCTION sqr(x:T_Complex):T_Complex; inline;
FUNCTION sqrabs(x:T_Complex):T_compBaseT; inline;
FUNCTION exp(x:T_Complex):T_Complex; inline;
FUNCTION ln (x:T_Complex):T_Complex; inline;
FUNCTION sin(x:T_Complex):T_Complex; inline;
FUNCTION cos(x:T_Complex):T_Complex; inline;
FUNCTION tan(x:T_Complex):T_Complex; inline;

IMPLEMENTATION

FUNCTION tempName:string;
  VAR i:longint;
  begin
    repeat
      i:=random(maxLongint);
      result:='temp'+intToStr(i)+'.bmp';
    until not(fileExists(result));
  end;

//T_rgbaArray:--------------------------------------------------------------------
PROCEDURE drawGrid(VAR img:T_FloatMap; gridColor:T_floatColor; gridCover:T_compBaseT; scaler:T_scaler);
  VAR maxYLabelWidth:longint;
      hLine,vLine:array of record
                    position:longint;
                    text :string;
                    txtCover,lineCover:T_compBaseT;
                  end;

  PROCEDURE prepareLines;
    FUNCTION shorterString(x:T_compBaseT; L10Max,L10Min:longint):string;
      VAR numString1:string;
      begin
        if (x<-1E3) or (x>1E3) then result:='' else begin
          //make exponential representation:----------------------//
          str(x*system.exp(-L10Max*system.ln(10)):0:(L10Max-L10Min),numString1);//
          numString1:=numString1+'E'+intToStr(L10Max);            //
          //------------------------:make exponential representation
          //make simple representation:-------------------------------------------------//
          if L10Min<0 then begin                                                        //
            str(x:0:-L10Min,result);                                                    //
            while result[length(result)]='0' do result:=copy(result,1,length(result)-1);//
            if result[length(result)]='.' then result:=copy(result,1,length(result)-1); //
          end else str(x:0:0,result);                                                   //
          if result='-0' then result:='0';                                              //
          //---------------------------------------------------:make simple representation
          if length(numString1)<length(result) then result:=numString1;  //return shorter representation
        end;
      end;



    VAR i,n,l10Max,l10Min:longint;
        xyMin,xyMax,p    :T_Complex;
        cover1,cover5,tCover1,tCover5,step,range:T_compBaseT;
        tlc,ttC:T_compBaseT;
        TL10:longint;
    begin
      xyMin:=scaler.transform(0,img.height-1);       //real coordinates of lower left screen corner
      xyMax:=scaler.transform(img.width-1,0);       //real coordinates of upper right screen corner

      //try
        range :=system.ln(max(xyMax.re-xyMin.re,
                              xyMax.im-xyMin.im))/system.ln(10);               //logarithmic range
      //except range:=maxLongint; end;
      if range<maxLongint then begin
        l10Min:=floor(range);                                    //necessary decimals (for main steps)
        l10Max:=ceil(system.ln(max(max(abs(xyMax.re),abs(xyMax.im)),    //maximum exponent
                            max(abs(xyMin.re),abs(xyMin.im)))));
        step  :=system.exp(l10Min*system.ln(10));                              //(main) grid width
        //determine line cover and text cover for secondary grid lines and labels:---//
        cover1 :=gridCover*min(1,max(0,(system.ln((xyMax.re-xyMin.re)/step)-system.ln( 5))/(system.ln(1)-system.ln( 5)))); //
        cover5 :=gridCover*min(1,max(0,(system.ln((xyMax.re-xyMin.re)/step)-system.ln(10))/(system.ln(5)-system.ln(10)))); //
        tCover1:=gridCover*min(1,max(0,(system.ln((xyMax.re-xyMin.re)/step)-system.ln( 2))/(system.ln(1)-system.ln( 2)))); //
        tCover5:=gridCover*min(1,max(0,(system.ln((xyMax.re-xyMin.re)/step)-system.ln( 5))/(system.ln(2)-system.ln( 5)))); //
        //---:determine line cover and text cover for secondary grid lines and labels//
        maxYLabelWidth:=0;
        //quantize origin:------------------//
        xyMin.re:=floor(xyMin.re/step)*step;//
        xyMin.im:=floor(xyMin.im/step)*step;//
        //--------------------:quantize origin
        setLength(hLine,0);
        setLength(vLine,0);
        for i:=0 to 200 do begin
          //compute temporary linecover, textcover and decimals:------------------------//
          if      i mod 10=0 then begin tlc:=gridCover; ttC:=gridCover; TL10:=L10Min;   end  //
          else if i mod  5=0 then begin tlc:=cover5;    ttC:=tCover5;   TL10:=L10Min-1; end  //
          else                    begin tlc:=cover1;    ttC:=tCover1;   TL10:=L10Min-1; end; //
          //--------------------------:compute temporary linecover, textcover and decimals
          if tlc>0 then begin //if line is visible:----------------------------//
            p:=scaler.mrofsnart(xyMin.re+i*step/10,xyMin.im+i*step/10);        //
            if (p.re>=0) and (p.re<img.width) then begin                       //
              //generate vertical line data:-------------------------------//  //
              n:=length(vLine); setLength(vLine,n+1);                      //  //
              with vLine[n] do begin                                       //  //
                position:=round(p.re); lineCover:=tlc; txtCover :=ttC;     //  //
                if txtCover>0                                              //  //
                  then text:=shorterString(xyMin.re+i*step/10,l10Max,TL10) //  //
                  else text:='';                                           //  //
              end; //with                                                  //  //
              //---------------------------------:generate vertical line data  //
            end; //if                                                          //
            if (p.im>=0) and (p.im<img.height) then begin                      //
              //generate horizontal line data:-----------------------------//  //
              n:=length(hLine); setLength(hLine,n+1);                      //  //
              with hLine[n] do begin                                       //  //
                position:=round(p.im); lineCover:=tlc; txtCover :=ttC;     //  //
                if txtCover>0                                              //  //
                  then text:=shorterString(xyMin.im+i*step/10,l10Max,TL10) //  //
                  else text:='';                                           //  //
                maxYLabelWidth:=max(maxYLabelWidth,length(text));          //  //
              end; //with                                                  //  //
              //-------------------------------:generate horizontal line data  //
            end; //if                                                          //
          end; //---------------------------------------------:if line is visible
        end; //for i:=0 to 200
        maxYLabelWidth:=maxYLabelWidth*6+1;
      end else begin
        setLength(hLine,0);
        setLength(vLine,0);
      end;
    end;

  PROCEDURE drawLines;
    PROCEDURE plotNum(x,y:longint; s:string; centered:boolean; cover:T_compBaseT);
      PROCEDURE smallDigit(x,y:longint; d:byte); inline;
        CONST digit:array[0..12,0..8,0..4] of byte=
              (((0,1,1,1,0),(1,0,0,0,1),(1,0,0,0,1),(1,0,0,0,1),(1,0,0,0,1),(1,0,0,0,1),(1,0,0,0,1),(1,0,0,0,1),(0,1,1,1,0)),
               ((0,0,0,1,0),(0,0,1,1,0),(0,1,0,1,0),(0,0,0,1,0),(0,0,0,1,0),(0,0,0,1,0),(0,0,0,1,0),(0,0,0,1,0),(0,0,0,1,0)),
               ((0,1,1,1,0),(1,0,0,0,1),(0,0,0,0,1),(0,0,0,0,1),(0,0,0,1,0),(0,0,1,0,0),(0,1,0,0,0),(1,0,0,0,0),(1,1,1,1,1)),
               ((0,1,1,1,0),(1,0,0,0,1),(0,0,0,0,1),(0,0,0,1,0),(0,0,1,0,0),(0,0,0,1,0),(0,0,0,0,1),(1,0,0,0,1),(0,1,1,1,0)),
               ((0,0,0,1,0),(0,0,1,0,0),(0,1,0,0,0),(1,0,0,1,0),(1,1,1,1,1),(0,0,0,1,0),(0,0,0,1,0),(0,0,0,1,0),(0,0,0,1,0)),
               ((1,1,1,1,1),(1,0,0,0,0),(1,0,0,0,0),(1,1,1,1,0),(0,0,0,0,1),(0,0,0,0,1),(0,0,0,0,1),(1,0,0,0,1),(0,1,1,1,0)),
               ((0,1,1,1,0),(1,0,0,0,1),(1,0,0,0,0),(1,0,0,0,0),(1,1,1,1,0),(1,0,0,0,1),(1,0,0,0,1),(1,0,0,0,1),(0,1,1,1,0)),
               ((1,1,1,1,1),(0,0,0,0,1),(0,0,0,0,1),(0,0,0,1,0),(0,0,1,0,0),(0,0,1,0,0),(0,0,1,0,0),(0,0,1,0,0),(0,0,1,0,0)),
               ((0,1,1,1,0),(1,0,0,0,1),(1,0,0,0,1),(1,0,0,0,1),(0,1,1,1,0),(1,0,0,0,1),(1,0,0,0,1),(1,0,0,0,1),(0,1,1,1,0)),
               ((0,1,1,1,0),(1,0,0,0,1),(1,0,0,0,1),(1,0,0,0,1),(0,1,1,1,1),(0,0,0,0,1),(0,0,0,0,1),(1,0,0,0,1),(0,1,1,1,0)),
               ((1,1,1,1,1),(1,0,0,0,0),(1,0,0,0,0),(1,0,0,0,0),(1,1,1,0,0),(1,0,0,0,0),(1,0,0,0,0),(1,0,0,0,0),(1,1,1,1,1)),
               ((0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0),(1,1,1,1,0),(0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0)),
               ((0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0),(0,0,0,0,0),(0,1,1,0,0),(0,1,1,0,0)));
        VAR dx,dy:longint;
        begin
          for dy:=0 to 8 do
          for dx:=0 to 4 do if digit[d,dy,dx]=1 then
            img.pixel[x+dx,y+dy]:=(gridColor*cover)+img.pixel[x+dx,y+dy]*(1-cover);
        end;

      VAR i:byte;
      begin
        if centered then dec(x,length(s)*3);
        for i:=1 to length(s) do case s[i] of
          '0': smallDigit(x+(i-1)*6,y,0);
          '1': smallDigit(x+(i-1)*6,y,1);
          '2': smallDigit(x+(i-1)*6,y,2);
          '3': smallDigit(x+(i-1)*6,y,3);
          '4': smallDigit(x+(i-1)*6,y,4);
          '5': smallDigit(x+(i-1)*6,y,5);
          '6': smallDigit(x+(i-1)*6,y,6);
          '7': smallDigit(x+(i-1)*6,y,7);
          '8': smallDigit(x+(i-1)*6,y,8);
          '9': smallDigit(x+(i-1)*6,y,9);
          'E',
          'e': smallDigit(x+(i-1)*6,y,10);
          '-': smallDigit(x+(i-1)*6,y,11);
          '.': smallDigit(x+(i-1)*6,y,12);
        end;
      end;

    VAR i,j,k:longint;
    begin
      //vertical grid lines/labels:---------------------------------------------------------//
      for i:=0 to length(vLine)-1 do with vLine[i] do if position>maxYLabelWidth then begin //
        if text='' then begin                                                               //
          for j:=0 to img.height-1 do                                                       //
            img.pixel[position,j]:=gridColor*lineCover+img.pixel[position,j]*(1-lineCover);
        end else begin                                                                      //
          plotNum(position,1,text,true,txtCover);                                           //
          for j:=11 to img.height-1 do                                                      //
            img.pixel[position,j]:=gridColor*lineCover+img.pixel[position,j]*(1-lineCover);
        end;                                                                                //
      end else begin                                                                        //
        j:=0;                                                                               //
        for k:=0 to length(hLine)-1 do begin                                                //
          if hLine[k].text<>'' then begin                                                   //
            while j<hLine[k].position-5 do begin                                            //
              img.pixel[position,j]:=gridColor*lineCover+img.pixel[position,j]*(1-lineCover);
              inc(j);                                                                       //
            end;                                                                            //
            j:=hLine[k].position+6;                                                         //
          end;                                                                              //
        end;                                                                                //
        while j<img.height-1 do begin                                                       //
          img.pixel[position,j]:=gridColor*lineCover+img.pixel[position,j]*(1-lineCover);
          inc(j);                                                                           //
        end;                                                                                //
      end; setLength(vLine,0);                                                              //
      //-----------------------------------------------------------:vertical grid lines/labels
      //horizontal grid lines/labels:--------------------------------------------//
      for i:=0 to length(hLine)-1 do with hLine[i] do if position>12 then begin  //
        if text='' then begin                                                    //
          for j:=0 to img.width-1 do                                             //
            img.pixel[j,position]:=gridColor*lineCover+img.pixel[j,position]*(1-lineCover);
        end else begin                                                           //
          plotNum(1,position-4,text,false,txtCover);                             //
          for j:=6*length(text)+1 to img.width-1 do                              //
            img.pixel[j,position]:=gridColor*lineCover+img.pixel[j,position]*(1-lineCover);
        end;                                                                     //
      end; setLength(hLine,0);                                                   //
      //----------------------------------------------:horizontal grid lines/labels
    end;

  begin
    prepareLines;
    drawLines;
  end;

FUNCTION abs(x:T_compBaseT):T_compBaseT;
  begin
    if x>0 then result:=x else result:=-x;
  end;

OPERATOR :=(x:T_compBaseT):T_Complex;
  begin
    result.re:=x; result.im:=0; result.valid:=true;
  end;

OPERATOR +(x,y:T_Complex):T_Complex;
  begin
    result.re:=x.re+y.re;
    result.im:=x.im+y.im;
  end;

OPERATOR -(x,y:T_Complex):T_Complex;
  begin
    result.re:=x.re-y.re;
    result.im:=x.im-y.im;
  end;

OPERATOR *(x,y:T_Complex):T_Complex;
  begin
    result.re:=x.re*y.re-x.im*y.im;
    result.im:=x.re*y.im+x.im*y.re;
  end;

OPERATOR /(x,y:T_Complex):T_Complex;
  begin
    //result.im:=1/result.im;
    result.im:=1/(y.re*y.re+y.im*y.im);
    result.re:=(x.re*y.re+x.im*y.im)*result.im;
    result.im:=(x.im*y.re-x.re*y.im)*result.im;
  end;

OPERATOR **(x,y:T_Complex):T_Complex;
  begin
    //result:=exp(ln(x)*y);
    {WARNING Implement manual overflow check!}
    //writeln('(',x.re,'+',x.im,'*i)**(',y.re,'+',y.im,'*i)');
    result.re:=x.re*x.re+x.im*x.im;
    //writeln('system.ln...');
    result.re:=0.5*system.ln(result.re); //abs(result.re)@post<=max(-ln(lowerThreshold),ln(upperThreshold)) -> no risk
    result.im:=arctan2(x.im,x.re);                 //abs(result.im)@post<=pi                                          -> no risk
    x.re:=result.re*y.re-result.im*y.im;
    x.im:=result.im*y.re+result.re*y.im;
    //writeln('system.exp...');
    result.im:=system.exp(x.re);
    result.re:=system.cos(x.im)*result.im;
    result.im:=system.sin(x.im)*result.im;
  end;

OPERATOR **(x:T_Complex; y:T_compBaseT):T_Complex;
  begin
    //result:=exp(ln(x)*y);
    {WARNING Implement manual overflow check!}
    //writeln('(',x.re,'+',x.im,'*i)**',y);
    result.re:=x.re*x.re+x.im*x.im;
    //writeln('system.ln...');
    result.re:=0.5*system.ln(result.re); //abs(result.re)@post<=max(-ln(lowerThreshold),ln(upperThreshold)) -> no risk
    result.im:=arctan2(x.im,x.re);                 //abs(result.im)@post<=pi                                          -> no risk
    x.re:=result.re*y;
    x.im:=result.im*y;
    //writeln('system.exp...');
    result.im:=system.exp(x.re);
    result.re:=system.cos(x.im)*result.im;
    result.im:=system.sin(x.im)*result.im;
  end;



OPERATOR **(x:T_Complex; y:longint):T_Complex;
  //Note: This implementation is 100% equivalent to the
  //      academical recursive implementation, but it
  //      avoids the recursion, thus reducing both stack
  //      usage and overhead due to function calls.
  //      Computational cost is in log(y)
  VAR k  :longint;
  begin
    if y=0 then result:=1
    else begin
      if y<0 then begin x:=1/x; y:=-y; end;
      result:=1;
      k:=1;
      while y>0 do begin
        if (y and k)=k then begin
          result:=result*x;
          dec(y,k);
        end;
        k:=k+k;
        x:=sqr(x);
      end;
    end;
  end;

FUNCTION abs(x:T_Complex):T_compBaseT;
  begin
    result:=sqrt(x.re*x.re+x.im*x.im);
  end;

FUNCTION arg(x:T_Complex):T_compBaseT ;
  begin
    result:=arctan2(x.im,x.re);
  end;

FUNCTION sqr(x:T_Complex):T_Complex;
  begin
    result.re:=x.re*x.re-x.im*x.im;
    result.im:=2*x.re*x.im;
  end;

FUNCTION sqrabs(x:T_Complex):T_compBaseT; inline;
  begin result:=x.re*x.re+x.im*x.im; end;

FUNCTION exp(x:T_Complex):T_Complex;
  begin
    result.im:=system.exp(x.re);
    result.re:=system.cos(x.im)*result.im;
    result.im:=system.sin(x.im)*result.im;
  end;

FUNCTION ln (x:T_Complex):T_Complex;
  begin
    {WARNING Implement manual overflow check!}
    result.re:=0.5*system.ln(x.re*x.re+x.im*x.im);
    result.im:=arctan2(x.im,x.re);
  end;

FUNCTION sin(x:T_Complex):T_Complex;
  begin
    //result:=(exp(i*x)-exp(-i*x))/(2*i);

    //result:=exp(i*x) --------------------//
    result.im:=system.exp(-x.im);          //
    result.re:=system.cos( x.re)*result.im;//
    result.im:=system.sin( x.re)*result.im;//
    //-------------------------------------//
    //result:=result-1/result ------------------------//
    x.im:=1/(result.re*result.re+result.im*result.im);//
    result.re:=result.re-result.re*x.im;              //
    result.im:=result.im+result.im*x.im;              //
    //------------------------------------------------//
    //result:=result/(2*i)=-0.5*i*result //
    x.re:=      0.5*result.im;           //
    result.im:=-0.5*result.re;           //
    result.re:=x.re;                     //
    //-----------------------------------//
  end;

FUNCTION cos(x:T_Complex):T_Complex;
  begin
    //result:=(exp(i*x)+exp(-i*x))/2;


    //result:=exp(i*x) --------------------//
    result.im:=system.exp(-x.im);          //
    result.re:=system.cos( x.re)*result.im;//
    result.im:=system.sin( x.re)*result.im;//
    //-------------------------------------//
    //result:=result+1/result ------------------------//
    x.im:=1/(result.re*result.re+result.im*result.im);//
    result.re:=result.re+result.re*x.im;              //
    result.im:=result.im-result.im*x.im;              //
    //------------------------------------------------//
    //result:=result/(2)=-0.5*result //
    result.re:=result.re*0.5;        //
    result.im:=result.im*0.5;        //
    //-------------------------------//

  end;

FUNCTION tan(x:T_Complex):T_Complex;
  begin
    result:=sin(x)/cos(x);
  end;

//T_scaler:======================================================================================================================================
CONSTRUCTOR T_scaler.create(width,height:longint; centerX,centerY,zoom:T_compBaseT);
  begin
    recreate(width,height,centerX,centerY,zoom);
  end;

PROCEDURE T_scaler.recreate(width,height:longint; centerX,centerY,zoom:T_compBaseT);
  begin
    relativeZoom :=zoom;
    worldRefX:=centerX;  screenRefX:=width*0.5;
    worldRefY:=centerY;  screenRefY:=height*0.5;
    screenWidth  :=width;
    screenHeight :=height;
    recalc;
  end;

DESTRUCTOR  T_scaler.destroy; begin end;

PROCEDURE T_scaler.recalc;
  begin
    invAbsoluteZoom:=(relativeZoom*screenDiagonal);
    absoluteZoom:=1/invAbsoluteZoom;
    offsetX:=worldRefX-absoluteZoom*screenRefX;
    offsetY:=worldRefY+absoluteZoom*screenRefY;
  end;

FUNCTION T_scaler.transform(CONST x,y:T_compBaseT; CONST rotateBy:T_Complex):T_Complex;
  VAR aid:T_Complex;
  begin
    result.re:=x-screenWidth*0.5;
    result.im:=y-screenHeight*0.5;
    aid:=result*rotateBy;
    result.re:= (aid.re+screenWidth *0.5)*absoluteZoom+offsetX;
    result.im:=-(aid.im+screenHeight*0.5)*absoluteZoom+offsetY;
    result.valid:=true;
  end;

FUNCTION T_scaler.transform(x,y:T_compBaseT   ):T_Complex;
  begin
    result.re:= x*absoluteZoom+offsetX;
    result.im:=-y*absoluteZoom+offsetY;
    result.valid:=true;
  end;

FUNCTION T_scaler.transform(x:T_Complex   ):T_Complex;
  begin
    result.re:= x.re*absoluteZoom+offsetX;
    result.im:=-x.im*absoluteZoom+offsetY;
    result.valid:=true;
  end;

PROCEDURE T_scaler.mrofsnart(VAR x,y:T_Chunk; chunkFill:word);
  VAR i:longint;
  begin
    for i:=0 to chunkFill-1 do begin
      x[i]:=(x[i]-offsetX)*invAbsoluteZoom;
      y[i]:=(offsetY-y[i])*invAbsoluteZoom;
    end;
  end;

FUNCTION T_scaler.mrofsnart(x,y:T_compBaseT   ):T_Complex;
  begin
    result.re:=(x-offsetX)*invAbsoluteZoom;
    result.im:=(offsetY-y)*invAbsoluteZoom;
    result.valid:=true;
  end;

FUNCTION T_scaler.mrofsnart(x:T_Complex   ):T_Complex;
  begin
    result.re:=(x.re-offsetX)*invAbsoluteZoom;
    result.im:=(offsetY-x.im)*invAbsoluteZoom;
    result.valid:=true;
  end;

FUNCTION T_scaler.screenCenterX:T_compBaseT;
  begin
    result:=screenWidth*0.5*absoluteZoom+offsetX;
  end;

FUNCTION T_scaler.screenCenterY:T_compBaseT;
  begin
    result:=-screenHeight*0.5*absoluteZoom+offsetY;
  end;


PROCEDURE T_scaler.rescale (newWidth,newHeight:longint);
  begin
    chooseScreenRef(screenWidth*0.5,screenHeight*0.5);
    screenWidth  :=newWidth;
    screenHeight :=newHeight;
    screenRefX:=screenWidth*0.5;
    screenRefY:=screenHeight*0.5;
    recalc;
  end;

PROCEDURE T_scaler.recenter(newX,newY:T_compBaseT);
  begin
    chooseScreenRef(screenWidth*0.5,screenHeight*0.5);
    worldRefX:=newX;
    worldRefY:=newY;
    recalc;
  end;

PROCEDURE T_scaler.moveCenter(dx,dy:T_compBaseT);
  begin
    screenRefX:=screenRefX+dx;
    screenRefY:=screenRefY+dy;
    recalc;
  end;

PROCEDURE T_scaler.rezoom  (newZoom  :T_compBaseT);
  begin
    relativeZoom :=newZoom;
    recalc;
  end;

PROCEDURE T_scaler.chooseScreenRef(x,y:T_compBaseT);
  VAR worldRef:T_Complex;
  begin
    worldRef:=transform(x,y);
    worldRefX:=worldRef.re;  screenRefX:=x;
    worldRefY:=worldRef.im;  screenRefY:=y;
    recalc;
  end;

PROCEDURE T_scaler.chooseWorldRef (x,y:T_compBaseT);
  VAR screenRef:T_Complex;
  begin
    screenRef:=mrofsnart(x,y);
    worldRefX:=x;  screenRefX:=screenRef.re;
    worldRefY:=y;  screenRefY:=screenRef.im;
    recalc;
  end;

FUNCTION T_scaler.screenDiagonal:T_compBaseT;
  begin
    result:=sqrt(screenWidth*screenWidth+screenHeight*screenHeight);
  end;
//======================================================================================================================================:T_scaler
{CONSTRUCTOR T_darts.create(numberOfDarts:longint);
  FUNCTION pDist(x,y:T_Complex):T_compBaseT; inline;
    begin
      result:=system.sqr(frac(x.re-y.re))+system.sqr(frac(x.im-y.im));
    end;

  VAR tol:T_compBaseT;
      r,i:longint;
      accept:boolean;
  begin
    count:=numberOfDarts;
    if count<1 then count:=1;
    getMem(delta,sizeOf(T_Complex)*count);
    delta[0]:=0; delta[0].valid:=true; tol:=1;
    for r:=1 to count-1 do begin
      repeat
        delta[r].re:=random-0.5;
        delta[r].im:=random-0.5;
        delta[r].valid:=true;
        accept:=true;
        i:=0;
        while (i<r) and (pDist(delta[r],delta[i])>tol) do inc(i);
        tol:=tol*0.9999;
      until i>=r;
    end;
  end;

CONSTRUCTOR T_darts.createOverlapping(numberOfDarts:longint);
  FUNCTION sDist(x,y:T_Complex):T_compBaseT; inline;
    begin
      result:=system.sqr(x.re-y.re)+system.sqr(x.im-y.im);
    end;

  VAR tol:T_compBaseT;
      r,i:longint;
      accept:boolean;
  begin
    count:=numberOfDarts;
    if count<1 then count:=1;
    getMem(delta,sizeOf(T_Complex)*count);
    delta[0]:=0; delta[0].valid:=true; tol:=1;
    r:=1;
    while r<count do begin
      repeat
        repeat
          delta[r].re:=random-0.5;
          delta[r].im:=random-0.5;
        until system.sqr(delta[r].re)+system.sqr(delta[r].im)<0.25;
        delta[r].re:=delta[r].re*sqrt(2);
        delta[r].im:=delta[r].im*sqrt(2);
        delta[r].valid:=true;
        accept:=true;
        for i:=0 to r-1 do accept:=accept and (sDist(delta[r],delta[i])>tol);
        tol:=tol*0.9999;
      until accept;
      inc(r);
    end;
  end;

DESTRUCTOR  T_darts.destroy;
  begin
    freeMem(delta,sizeOf(T_Complex)*count);
    count:=0;
  end;

FUNCTION T_darts.getD(i,j:longint):T_compBaseT;
  begin
    if j=0 then result:=delta[i].re
           else result:=delta[i].im;
  end;          }

INITIALIZATION
  randomize;

end.
