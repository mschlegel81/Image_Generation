{$define iterationStart:=x:=c; x.im:=c.re; x.re:=c.im; c:=-1*x;}
{$define iterationStep:=x:=f(x,c)}
CONST windowTitle='x[0]=c; x[i+1]=cos(x[i]+c)';

FUNCTION f(x,c:T_Complex):T_Complex;
  begin
    if x.re*x.re+x.im*x.im>2 then begin
      result:=sqr(x)-c;
      if result.re>0 then result.re:= sqrt( result.re)
                     else result.re:=-sqrt(-result.re);
      if result.im>0 then result.im:= sqrt( result.im)
                     else result.im:=-sqrt(-result.im);
    end else result:=sqr(x)*x-c;

  end;
