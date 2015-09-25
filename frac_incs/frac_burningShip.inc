{$define iterationStart:=x:=c }
{$define iterationStep:=burningShipStep(x,c)}
CONST windowTitle='Burning Ship';

PROCEDURE burningShipStep(VAR x,c:T_Complex); inline;
  VAR x_re:double;
  begin
    x_re:=c.re+x.re*x.re-x.im*x.im;
    if (x.re<0) = (x.im<0)
      then x.im:=c.im-2*x.re*x.im
      else x.im:=c.im+2*x.re*x.im;
    x.re:=x_re;
  end;
