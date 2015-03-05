{$define iterationStart:=x:=1/c }
{$define iterationStep:=x:=step(x,c)}
CONST windowTitle='TUL';

FUNCTION step(x,c:T_Complex):T_Complex;
  begin
    if sqrabs(x)>1 then result:=1/x
                   else result:=x*(c+x);
  end;
