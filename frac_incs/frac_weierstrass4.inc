{$define iterationStart:=x:=c }
{$define iterationStep:=x:=WeierP(x)}
CONST windowTitle='Weierstrass p; period1=2; period2=2i';

FUNCTION WeierP(x:T_Complex):T_Complex;
  CONST p1:T_Complex=(re: 2; im:0; valid:true);
        p2:T_Complex=(re: 0; im:2; valid:true);
        InvAid=1/(2*2);
  VAR i0,j0:longint;
      fi,fj:double;
  begin
    //if x.valid then result:=x else begin
      fi:=InvAid*( p2.im*x.re-p2.re*x.im);
      fj:=InvAid*(-p1.im*x.re+p1.re*x.im);
      if (abs(fi)<1E10) and (abs(fj)<1E10) then begin
        i0:=round(fi);
        j0:=round(fj);
        x.re:=x.re-i0*p1.re-j0*p2.re;
        x.im:=x.im-i0*p1.im-j0*p2.im;
        result:=1/sqr(x)
               +1/sqr(x-p1)+1/sqr(x-p2)+1/sqr(x+p1)+1/sqr(x+p2)
               +1/sqr(x-p1-p2)+1/sqr(x-p1+p2)+1/sqr(x+p1-p2)+1/sqr(x+p1+p2)
               +1/sqr(x-p1-p1)+1/sqr(x-p2-p2)+1/sqr(x+p1+p1)+1/sqr(x+p2+p2)
               +1/sqr(x-p1-p1-p2)+1/sqr(x-p2-p2-p1)+1/sqr(x+p1+p1-p2)+1/sqr(x+p2+p2-p1)+1/sqr(x-p1-p1+p2)+1/sqr(x-p2-p2+p1)+1/sqr(x+p1+p1+p2)+1/sqr(x+p2+p2+p1);
               //+1/sqr(x-p1-p1-p2-p2)+1/sqr(x+p1+p1-p2-p2)+1/sqr(x-p1-p1+p2+p2)+1/sqr(x+p1+p1+p2+p2);
      end else result.valid:=false;
    //end;
  end;