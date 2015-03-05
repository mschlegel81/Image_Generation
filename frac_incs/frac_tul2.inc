{$define iterationStart:=x:=c }
{$define iterationStep:=x:=step(x,c)}
CONST windowTitle='TUL2';

FUNCTION step(x,c:T_Complex):T_Complex;
  begin
    result:=x*x+c;
    result:=0.5*(result+1/result);
    result:=0.5*(result+1/result);
    //               else result:=x*sqr(x)+c;
  end;
