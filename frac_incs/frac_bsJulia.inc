{$define iterationStart:=x:=c }
{$define iterationStep:=burningShipStep(x,c)}
CONST windowTitle='Julia set on Burning Ship basis';

PROCEDURE burningShipStep(VAR x,c:T_Complex); inline;
  VAR x_re:double;
  begin
    x_re:=x.re*x.re-x.im*x.im+ 5.91925608954895E-001;
    if (x.re<0) = (x.im<0)
      then x.im:=9.18404930408219E-001-2*x.re*x.im
      else x.im:=9.18404930408219E-001+2*x.re*x.im;
    x.re:=x_re;
  end;
