{$define iterationStart:=x:=1/c }
{$define iterationStep:=x:=step(x,c)}
CONST windowTitle='TUL';

FUNCTION step(x,c:T_Complex):T_Complex;
  begin
    if x.im>0 then result:=sin(x+c)
              else result:=exp(x+c);
  end;
