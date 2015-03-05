{$define iterationStart:=if abs(c)<1 then c:=1/c; x:=c  }
{$define iterationStep:=x:=f(x,c)}
CONST windowTitle='x[0]=c; x[i+1]=cos(x[i]+c)';

FUNCTION f(x,c:T_Complex):T_Complex;
  VAR pow:double;
  begin
    pow:=6-abs(x);
    if pow<1 then result:=x+c else
    if pow<2 then result:=x**pow+c
             else result:=sqr(x)+c;

  end;