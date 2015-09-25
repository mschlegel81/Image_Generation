{$define iterationStart:=x:=c}
{$define iterationStep:=x:=x-1+c; x:=x*(1-x*(1/2-x*(1/3-x*(1/4-x*(1/5-x*(1/6))))))}
CONST windowTitle='x[0]=c; x[i+1]=ln(x[i]+c)';
