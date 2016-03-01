UNIT complex;
{$MACRO ON}
INTERFACE
USES sysutils,math;
TYPE
  T_Complex   =record re,im:double; end;
  P_Complex   =^T_Complex;

CONST
   II:T_Complex=(re:0; im:1);
   C_invalidComplex:T_Complex=(re:Nan; im:Nan);

TYPE

  { T_scaler }

  T_scaler=object
    private
      //input
      relativeZoom,
      rotation:double;
      worldCenter:T_Complex;
      screenWidth  ,screenHeight:longint;
      //derived:
      zoomRot,invZoomRot:T_Complex;
      screenCenter:T_Complex;

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

      CONSTRUCTOR create  (CONST width,height:longint; CONST centerX,centerY,zoom,rotationInDegrees:double);
      PROCEDURE   recreate(CONST width,height:longint; CONST centerX,centerY,zoom,rotationInDegrees:double);
      DESTRUCTOR  destroy;
      FUNCTION    transform(CONST x,y:double   ):T_Complex;
      FUNCTION    mrofsnart(CONST x,y:double   ):T_Complex;
      PROCEDURE   rescale (CONST newWidth,newHeight:longint);
      PROCEDURE   recenter(CONST newCenter:T_Complex);
      PROCEDURE   moveCenter(CONST dx,dy:double);
      FUNCTION getAbsoluteZoom:T_Complex;
      FUNCTION getPositionString(CONST x,y:double; CONST Separator:ansistring='+i*'):ansistring;
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
  begin result:=x.re*x.re+x.im*x.im; if isNan(result) then result:=infinity; end;

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
CONSTRUCTOR T_scaler.create(CONST width, height: longint; CONST centerX,centerY, zoom, rotationInDegrees: double);
  begin
    recreate(width,height,centerX,centerY,zoom,rotationInDegrees);
  end;

PROCEDURE T_scaler.recreate(CONST width, height: longint; CONST centerX,centerY, zoom, rotationInDegrees: double);
  begin
    screenWidth :=width;
    screenHeight:=height;
    worldCenter.re:=centerX;
    worldCenter.im:=centerY;
    relativeZoom:=zoom;
    rotation:=rotationInDegrees*pi/180;
    recalc;
  end;

DESTRUCTOR T_scaler.destroy; begin end;

PROCEDURE T_scaler.recalc;
  begin
    zoomRot.re:=system.cos(rotation);
    zoomRot.im:=system.sin(rotation);
    zoomRot:=zoomRot/(relativeZoom*sqrt(screenWidth*screenWidth+screenHeight*screenHeight));
    invZoomRot:=1/zoomRot;
    screenCenter.re:=0.5*screenWidth;
    screenCenter.im:=0.5*screenHeight;
  end;

FUNCTION T_scaler.getCenterX: double;
  begin
    result:=worldCenter.re;
  end;

PROCEDURE T_scaler.setCenterX(CONST value: double);
  begin
    worldCenter.re:=value;
    recalc;
  end;

FUNCTION T_scaler.getCenterY: double;
  begin
    result:=worldCenter.im;
  end;

PROCEDURE T_scaler.setCenterY(CONST value: double);
  begin
    worldCenter.im:=value;
    recalc;
  end;

PROCEDURE T_scaler.recenter(CONST newCenter:T_Complex);
  begin
    worldCenter:=newCenter;
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
    result:=rotation/pi*180;
  end;

PROCEDURE T_scaler.setRotation(CONST value: double);
  begin
    rotation:=value/180*pi;
    while rotation<-pi do rotation:=rotation+2*pi;
    while rotation> pi do rotation:=rotation-2*pi;
    recalc;
  end;

FUNCTION T_scaler.transform(CONST x, y: double): T_Complex;
  begin
    result.re:=x;
    result.im:=screenHeight-y;
    result:=(result-screenCenter)*zoomRot+worldCenter;
  end;

FUNCTION T_scaler.mrofsnart(CONST x, y: double): T_Complex;
  begin
    result.re:=x;
    result.im:=y;
    result:=(result-worldCenter)*invZoomRot+screenCenter;
    result.im:=screenHeight-result.im;
  end;

PROCEDURE T_scaler.rescale(CONST newWidth, newHeight: longint);
  begin
    screenWidth:=newWidth;
    screenHeight:=newHeight;
    recalc;
  end;

PROCEDURE T_scaler.moveCenter(CONST dx, dy: double);
  VAR delta:T_Complex;
  begin
    delta.re:= dx;
    delta.im:=-dy;
    worldCenter:=worldCenter+delta*zoomRot;
    recalc;
  end;

FUNCTION T_scaler.getAbsoluteZoom: T_Complex;
  begin
    result:=zoomRot;
  end;

FUNCTION T_scaler.getPositionString(CONST x, y: double; CONST Separator: ansistring): ansistring;
  VAR p:T_Complex;
  begin
    p:=transform(x,y);
    result:=floatToStr(p.re)+Separator+floatToStr(p.im);
  end;


INITIALIZATION
  randomize;

end.
