{$define iterationStart:=x:=c}
{$define iterationStep:=x:=ln(x); x:=(1+x*(1+0.5*x*(1+(1/3)*x*(1+0.25*x))))}
CONST windowTitle='x[0]=c; x[i+1]=exp(ln x)+O((ln x)^5)';
