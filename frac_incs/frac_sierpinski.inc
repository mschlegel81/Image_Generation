{$define iterationStart:=x:=c }
{$define iterationStep:=x:=tri(c,i)}
CONST windowTitle='Sierpinski Triangle';

FUNCTION tri(x:T_Complex; depth:longint):T_Complex;
  VAR i,j,k:longint;
  begin
    if (x.re<0) or (x.re>1) or (x.im<0) or (x.im>1) then result:=0
    else begin
      if depth>30 then depth:=30;
      i:=trunc(x.re*(1 shl depth));
      j:=trunc(x.im*(1 shl depth));
      k:=depth;
      while (k>=0) do begin
        if (i and (1 shl k)=0) or (j and (1 shl k)=0)
          then dec(k)
          else k:=-10;
      end;
      if k<-1 then result:=0
              else result:=1000;
    end;
  end;