{$define iterationStart:=x:=c }
{$define iterationStep:=x:=fw(x,c)}
CONST windowTitle='Freak Wave';

FUNCTION fw(x,c:T_Complex):T_Complex; inline;
  begin
    result:=x.re*system.cos(x.re+x.im)+c;
  end;

{
     2+2    -3-3
        2  -3
-1-1 -1   X   1 1+1
        3  -2
     3+3    -2-2                  }
