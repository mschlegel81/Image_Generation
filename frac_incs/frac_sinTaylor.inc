{$define iterationStart:=x:=c}
{$define iterationStep:=x:=x*(1+sqr(x)*(-0.166666666666667+sqr(x)*(+0.00833333333333333+sqr(x)*(-0.000198412698412698+2.75573192239859E-6*sqr(x)))))+c}
CONST windowTitle='x[0]=c; x[i+1]=sin(x[i]+c)';
