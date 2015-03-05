{$define iterationStart:=x:=c}
{$define iterationStep:=x:=ln(1+x*(1+0.5*x*(1+(1/3)*x*(1+0.25*x))))}
CONST windowTitle='x[0]=c; x[i+1]=ln(exp(x)+O(x^5))';