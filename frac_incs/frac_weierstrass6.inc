{$define iterationStart:=x:=c }
{$define iterationStep:=x:=WeierP(x)}
CONST windowTitle='Weierstrass p; period1=2; period2=-1+sqrt(-3)';

FUNCTION WeierP(x:T_Complex):T_Complex;
  CONST p1:T_Complex=(re: 2; im: 0;         valid:true);  // 1,0
        p2:T_Complex=(re:-1; im: sqrt(3);   valid:true);  // 0,1
        p3:T_Complex=(re:-1; im:-sqrt(3);   valid:true);  //-1,-1
        p4:T_Complex=(re: 4; im: 0      ;   valid:true);  // 2,0
        p5:T_Complex=(re: 3; im: sqrt(3);   valid:true);  // 2,1
        p6:T_Complex=(re: 3; im:-sqrt(3);   valid:true);
        p7:T_Complex=(re:-2; im: sqrt(3)*2; valid:true);
        p8:T_Complex=(re: 0; im: sqrt(3)*2; valid:true);
        p9:T_Complex=(re:-2; im:-sqrt(3)*2; valid:true);

        InvAid=1/(2*sqrt(3));
  VAR i0,j0:longint;
      fi,fj:double;
  begin
    //if not(x.valid) then result:=x else begin
      fi:=InvAid*( p2.im*x.re-p2.re*x.im);
      fj:=InvAid*(-p1.im*x.re+p1.re*x.im);
      //if (abs(fi)<1E10) and (abs(fj)<1E10) then begin
        i0:=round(fi);
        j0:=round(fj);
        x.re:=x.re-i0*p1.re-j0*p2.re;
        x.im:=x.im-i0*p1.im-j0*p2.im;
        //result:=1/sqr(x)+1/sqr(x-p1)+1/sqr(x-p2)+1/sqr(x+p1)+1/sqr(x+p2)+1/sqr(x-p1-p2)+1/sqr(x+p1+p2);
        result:=1/sqr(x)
               +1/sqr(x-p1)+1/sqr(x+p1)
               +1/sqr(x-p2)+1/sqr(x+p2)
               +1/sqr(x-p3)+1/sqr(x+p3)
               +1/sqr(x-p4)+1/sqr(x+p4)
               +1/sqr(x-p5)+1/sqr(x+p5)
               +1/sqr(x-p6)+1/sqr(x+p6)
               +1/sqr(x-p7)+1/sqr(x+p7)
               +1/sqr(x-p8)+1/sqr(x+p8)
               +1/sqr(x-p9)+1/sqr(x+p9);
      //end else result.valid:=false;
    //end;
  end;

{
     2+2    -3-3
        2  -3
-1-1 -1   X   1 1+1
        3  -2
     3+3    -2-2                  }
