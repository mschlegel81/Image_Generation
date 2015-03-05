{$define iterationStart:=x:=c}
{$define iterationStep:=x:=(4/5)*x+1/(5*sqr(sqr(x)))}
CONST windowTitle='x[0]=0; x[i+1]=(4/5)x[i]+1/(5x[i]^4); 5th root newton iteration';