UNIT complex;
{$MACRO ON}
INTERFACE
USES sysutils,math;
TYPE
  T_Complex   =record re,im:double; end;
  P_Complex   =^T_Complex;

CONST
   upperThreshold=1E50;
   lowerThreshold=1/upperThreshold;
   II:T_Complex=(re:0; im:1);
   C_invalidComplex:T_Complex=(re:Nan; im:Nan);

TYPE

  { T_scaler }

  T_scaler=object
    private
      absoluteZoom,invAbsoluteZoom:double;
      offsetX,offsetY:double;
      rotationInDegrees:double;
      screenWidth  ,screenHeight:longint;

      relativeZoom ,
      screenRefX,screenRefY:double;
      worldRefX,worldRefY:double;
      rotationAsComplex:T_Complex;

      FUNCTION screenCenterX:double;
      FUNCTION screenCenterY:double;
      PROCEDURE recalc;


    public
      FUNCTION getCenterX:double;
      PROCEDURE setCenterX(CONST value:double);
      FUNCTION getCenterY:double;
      PROCEDURE setCenterY(CONST value:double);
      FUNCTION getZoom:double;
      PROCEDURE setZoom(CONST value:double);
      FUNCTION getRotation:double;
      PROCEDURE setRotation(CONST value:double);


      CONSTRUCTOR create  (CONST width,height:longint; CONST centerX,centerY,zoom,rotation:double);
      PROCEDURE   recreate(CONST width,height:longint; CONST centerX,centerY,zoom,rotation:double);
      DESTRUCTOR  destroy;
      FUNCTION    transform(CONST x,y:double   ):T_Complex;
      FUNCTION    mrofsnart(CONST x,y:double   ):T_Complex;
      PROCEDURE   rescale (CONST newWidth,newHeight:longint);

      PROCEDURE   chooseScreenRef(CONST x,y:double);
      PROCEDURE   moveCenter(CONST dx,dy:double);

      FUNCTION getAbsoluteZoom:double;

      //PROCEDURE   chooseWorldRef (CONST x,y:double);
      //FUNCTION    screenDiagonal:double;
  end;

OPERATOR :=(CONST x:double):T_Complex; inline;

OPERATOR +(CONST x,y:T_Complex):T_Complex; inline;
OPERATOR -(CONST x,y:T_Complex):T_Complex; inline;
OPERATOR *(CONST x,y:T_Complex):T_Complex; inline;
OPERATOR /(CONST x,y:T_Complex):T_Complex; inline;
OPERATOR **(x:T_Complex; y:longint):T_Complex; inline;
OPERATOR **(x:T_Complex; CONST y:double):T_Complex; inline;
OPERATOR **(x:T_Complex; CONST y:T_Complex):T_Complex; inline;
FUNCTION abs(CONST x:double):double; inline;
FUNCTION abs(CONST x:T_Complex):double; inline;
FUNCTION arg(CONST x:T_Complex):double; inline;
FUNCTION sqr(CONST x:T_Complex):T_Complex; inline;
FUNCTION sqrabs(CONST x:T_Complex):double; inline;
FUNCTION exp(CONST x:T_Complex):T_Complex; inline;
FUNCTION ln (CONST x:T_Complex):T_Complex; inline;
FUNCTION sin(x:T_Complex):T_Complex; inline;
FUNCTION cos(x:T_Complex):T_Complex; inline;
FUNCTION tan(CONST x:T_Complex):T_Complex; inline;
FUNCTION isValid(CONST c:T_Complex):boolean; inline;

IMPLEMENTATION
FUNCTION abs(CONST x:double):double;
  begin
    if x>0 then result:=x else result:=-x;
  end;

OPERATOR :=(CONST x:double):T_Complex;
  begin
    result.re:=x; result.im:=0;
  end;

OPERATOR +(CONST x,y:T_Complex):T_Complex;
  begin
    result.re:=x.re+y.re;
    result.im:=x.im+y.im;
  end;

OPERATOR -(CONST x,y:T_Complex):T_Complex;
  begin
    result.re:=x.re-y.re;
    result.im:=x.im-y.im;
  end;

OPERATOR *(CONST x,y:T_Complex):T_Complex;
  begin
    result.re:=x.re*y.re-x.im*y.im;
    result.im:=x.re*y.im+x.im*y.re;
  end;

OPERATOR /(CONST x,y:T_Complex):T_Complex;
  begin
    result.im:=1/(y.re*y.re+y.im*y.im);
    result.re:=(x.re*y.re+x.im*y.im)*result.im;
    result.im:=(x.im*y.re-x.re*y.im)*result.im;
  end;

OPERATOR **(x:T_Complex; CONST y:T_Complex):T_Complex;
  begin
    //result:=exp(ln(x)*y);
    result.re:=x.re*x.re+x.im*x.im;
    result.re:=0.5*system.ln(result.re);
    result.im:=arctan2(x.im,x.re);
    x.re:=result.re*y.re-result.im*y.im;
    x.im:=result.im*y.re+result.re*y.im;
    result.im:=system.exp(x.re);
    result.re:=system.cos(x.im)*result.im;
    result.im:=system.sin(x.im)*result.im;
  end;

OPERATOR **(x:T_Complex; CONST y:double):T_Complex;
  begin
    //result:=exp(ln(x)*y);
    result.re:=x.re*x.re+x.im*x.im;
    result.re:=0.5*system.ln(result.re);
    result.im:=arctan2(x.im,x.re);
    x.re:=result.re*y;
    x.im:=result.im*y;
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

FUNCTION abs(CONST x:T_Complex):double;
  begin
    result:=sqrt(x.re*x.re+x.im*x.im);
  end;

FUNCTION arg(CONST x:T_Complex):double ;
  begin
    result:=arctan2(x.im,x.re);
  end;

FUNCTION sqr(CONST x:T_Complex):T_Complex;
  begin
    result.re:=x.re*x.re-x.im*x.im;
    result.im:=2*x.re*x.im;
  end;

FUNCTION sqrabs(CONST x:T_Complex):double; inline;
  begin result:=x.re*x.re+x.im*x.im; end;

FUNCTION exp(CONST x:T_Complex):T_Complex;
  begin
    result.im:=system.exp(x.re);
    result.re:=system.cos(x.im)*result.im;
    result.im:=system.sin(x.im)*result.im;
  end;

FUNCTION ln (CONST x:T_Complex):T_Complex;
  begin
    result.re:=0.5*system.ln(x.re*x.re+x.im*x.im);
    result.im:=arctan2(x.im,x.re);
  end;

FUNCTION sin(x:T_Complex):T_Complex;
  begin
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

FUNCTION tan(CONST x:T_Complex):T_Complex;
  begin
    result:=sin(x)/cos(x);
  end;

FUNCTION isValid(CONST c:T_Complex):boolean; inline;
  begin
    result:=not(isNan     (c.re) or
                isInfinite(c.re) or
                isNan     (c.im) or
                isInfinite(c.im));
  end;

//T_scaler:======================================================================================================================================
CONSTRUCTOR T_scaler.create(CONST width, height: longint; CONST centerX, centerY, zoom, rotation: double);
  begin
    recreate(width,height,centerX,centerY,zoom,rotation);
  end;

PROCEDURE T_scaler.recreate(CONST width, height: longint; CONST centerX, centerY, zoom, rotation: double);
  begin
    relativeZoom:=zoom;
    worldRefX   :=centerX;
    worldRefY   :=centerY;
    screenRefX  :=width*0.5;
    screenRefY  :=height*0.5;
    screenWidth :=width;
    screenHeight:=height;
    rotationInDegrees:=rotation;
    recalc;
  end;

DESTRUCTOR T_scaler.destroy; begin end;

PROCEDURE T_scaler.recalc;
  begin
    invAbsoluteZoom:=(relativeZoom*sqrt(screenWidth*screenWidth+screenHeight*screenHeight));
    absoluteZoom:=1/invAbsoluteZoom;
    offsetX:=worldRefX-absoluteZoom*screenRefX;
    offsetY:=worldRefY+absoluteZoom*screenRefY;
    rotationAsComplex.re:=system.cos(pi/180*rotationInDegrees);
    rotationAsComplex.im:=system.sin(pi/180*rotationInDegrees);
  end;

FUNCTION T_scaler.getCenterX: double;
  begin
    result:=screenWidth*0.5*absoluteZoom+offsetX;
  end;

PROCEDURE T_scaler.setCenterX(CONST value: double);
  begin
    chooseScreenRef(screenWidth*0.5,screenHeight*0.5);
    worldRefX:=value;
    recalc;
  end;

FUNCTION T_scaler.getCenterY: double;
  begin
    result:=-screenHeight*0.5*absoluteZoom+offsetY;
  end;

PROCEDURE T_scaler.setCenterY(CONST value: double);
  begin
    chooseScreenRef(screenWidth*0.5,screenHeight*0.5);
    worldRefY:=value;
    recalc;
  end;

FUNCTION T_scaler.getZoom: double;
  begin
    result:=relativeZoom;
  end;

PROCEDURE T_scaler.setZoom(CONST value: double);
  begin
    relativeZoom:=value;
    recalc;
  end;

FUNCTION T_scaler.getRotation: double;
  begin
    result:=rotationInDegrees;
  end;

PROCEDURE T_scaler.setRotation(CONST value: double);
  begin
    rotationInDegrees:=value;
    recalc;
  end;

FUNCTION T_scaler.transform(CONST x, y: double): T_Complex;
  VAR aid:T_Complex;
  begin
    if rotationInDegrees=0 then begin
      result.re:= x*absoluteZoom+offsetX;
      result.im:=-y*absoluteZoom+offsetY;
      exit(result);
    end;
    result.re:=x-screenWidth*0.5;
    result.im:=y-screenHeight*0.5;
    aid:=result*rotationAsComplex;
    result.re:= (aid.re+screenWidth *0.5)*absoluteZoom+offsetX;
    result.im:=-(aid.im+screenHeight*0.5)*absoluteZoom+offsetY;
  end;

FUNCTION T_scaler.mrofsnart(CONST x, y: double): T_Complex;
  VAR aid:T_Complex;
  begin
    if rotationInDegrees=0 then begin
      result.re:=(x-offsetX)*invAbsoluteZoom;
      result.im:=(offsetY-y)*invAbsoluteZoom;
      exit(result);
    end;
    aid.re:=(x-offsetX)*invAbsoluteZoom-screenWidth *0.5;
    aid.im:=(offsetY-y)*invAbsoluteZoom-screenHeight*0.5;
    result:=aid/rotationAsComplex;
    result.re:=result.re+screenWidth *0.5;
    result.im:=result.im+screenHeight*0.5;
  end;

FUNCTION T_scaler.screenCenterX: double;
  begin
    result:=screenWidth*0.5*absoluteZoom+offsetX;
  end;

FUNCTION T_scaler.screenCenterY: double;
  begin
    result:=-screenHeight*0.5*absoluteZoom+offsetY;
  end;


PROCEDURE T_scaler.rescale(CONST newWidth, newHeight: longint);
  begin
    chooseScreenRef(screenWidth*0.5,screenHeight*0.5);
    screenWidth  :=newWidth;
    screenHeight :=newHeight;
    screenRefX:=screenWidth*0.5;
    screenRefY:=screenHeight*0.5;
    recalc;
  end;

PROCEDURE T_scaler.moveCenter(CONST dx, dy: double);
  begin
    screenRefX:=screenRefX+dx;
    screenRefY:=screenRefY+dy;
    recalc;
  end;

FUNCTION T_scaler.getAbsoluteZoom:double;
  begin

  end;

PROCEDURE T_scaler.chooseScreenRef(CONST x, y: double);
  VAR worldRef:T_Complex;
  begin
    worldRef:=transform(x,y);
    worldRefX:=worldRef.re;  screenRefX:=x;
    worldRefY:=worldRef.im;  screenRefY:=y;
    recalc;
  end;

INITIALIZATION
  randomize;

end.
