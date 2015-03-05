{$define iterationStart:=x:=c }
{$define iterationStep:=x:=step(x)}
CONST windowTitle='x[k+1]:=0.2822140+0.6813474i+bar(x[k]²)';

FUNCTION step(x:T_Complex):T_Complex; inline;
  begin
    result.re:=0.2822140+x.re*x.re-x.im*x.im;
    result.im:=0.6813474-2*x.re*x.im;
  end;
