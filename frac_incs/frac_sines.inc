{$define iterationStart:=x:=c }
{$define iterationStep:=x:=sines(c,maxdepth); i:=maxlongint-1}
CONST windowTitle='Julia set on Burning Ship basis';

FUNCTION sines(VAR x:T_Complex; variant:longint):double; inline;
  begin
    case byte(variant mod 12) of
      1:result:=0.5+0.5*system.sin(abs(x)       );
      2:result:=0.5+0.5*system.sin(abs(x)-pi/2  );
      3:result:=0.5+0.5*system.sin(abs(x)-pi    );
      4:result:=0.5+0.5*system.sin(abs(x)-3*pi/2);

      5:result:=0.5+0.5*system.sin(abs(x.re*x.re+x.im*x.im)       );
      6:result:=0.5+0.5*system.sin(abs(x.re*x.re+x.im*x.im)-pi/2  );
      7:result:=0.5+0.5*system.sin(abs(x.re*x.re+x.im*x.im)-pi    );
      8:result:=0.5+0.5*system.sin(abs(x.re*x.re+x.im*x.im)-3*pi/2);

      9:result:=0.5+0.5*system.sin(1/abs(x)       );
     10:result:=0.5+0.5*system.sin(1/abs(x)-pi/2  );
     11:result:=0.5+0.5*system.sin(1/abs(x)-pi    );
      0:result:=0.5+0.5*system.sin(1/abs(x)-3*pi/2);
    end;
  end;
