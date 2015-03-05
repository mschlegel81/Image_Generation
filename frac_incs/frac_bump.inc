{$define iterationStart:=x:=c; c.re:=system.sin(x.re+system.exp(abs(x))*x.im); c.im:=system.sin(x.im-system.exp(abs(x))*x.re); c:=c*system.exp(abs(x)); x:=0}
{$define iterationStep:=x:=sqr(sqr(x))+c; }
CONST windowTitle='x[0]=0; x[i+1]=exp(x[i])';
