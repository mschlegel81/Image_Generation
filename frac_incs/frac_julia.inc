{$define iterationStart:=x:=c}
{$define iterationStep:=x:=sqr(x); x.re:=x.re-0.8; x.im:=x.im+0.156}
CONST windowTitle='x[0]=c; x[i+1]=x[i]²-0.8+0.156i';
