{$define iterationStart:=x:=1/c }
{$define iterationStep:=x:=step(x,c)}
CONST windowTitle='TUL';

FUNCTION step(x,c:T_Complex):T_Complex;
  begin
    if x.re>0 then result:=1/x+c
              else result:=1/sqr(x)+c;
  end;
