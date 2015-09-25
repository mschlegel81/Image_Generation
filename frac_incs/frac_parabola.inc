{$define iterationStart:=x:=c}
{$define iterationStep:=x:=F(X)}
CONST windowTitle='Parabola';

FUNCTION F(x:T_Complex):T_Complex;
  begin
//    if odd(x.metaInfo) then result:=x else begin
//      result.metaInfo:=0;
      result.re:=((x.re-1)*(x.re-1)+x.im)/abs(x);
      result.im:=-x.im*result.re;
      result.re:= x.re*result.re;
//      if (result.re<-upperThreshold) or (result.re>upperThreshold) or (result.im<-upperThreshold) or (result.im>upperThreshold) then result.metaInfo:=1;
//    end;

  end;
