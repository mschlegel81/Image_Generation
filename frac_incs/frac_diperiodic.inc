{$define iterationStart:=x:=c}
{$define iterationStep:=x.re:=2*system.cos(x.re*0.5); x.im:=2*system.cos(x.im*0.5); x:=exp(x)}
CONST windowTitle='x[0]=c; x[i+1]=exp(P(x[i]))';