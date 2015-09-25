{$define iterationStart:=x:=c; if maxDepth<7 then begin if maxDepth>=5 then c:=1/c else if maxDepth>=3 then begin c.re:=1/c.re; c.im:=1/c.im; end; end }
{$define iterationStep:=x:=stripe(c,i)}
CONST windowTitle='Stripes';

FUNCTION stripe(x:T_Complex; VAR depth:longint):T_Complex;
  VAR i:longint;
  begin
    depth:=maxDepth;
    case byte(maxDepth) of
      1,3,5: begin
           case byte(trunc((x.im-floor(x.im))*6)) of
              0: begin result.re:= 1E-6; result.im:= 0; end;
              1: begin result.re:=-5E+5; result.im:= 0.86602540378444E+6; end;
              2: begin result.re:=-5E-7; result.im:=-0.86602540378444E-6; end;
              3: begin result.re:= 1E+6; result.im:= 0; end;
              4: begin result.re:=-5E-7; result.im:= 0.86602540378444E-6; end;
              5: begin result.re:=-5E+5; result.im:=-0.86602540378444E+6; end;
            end;
         end;
      2,4,6: begin
           case byte((trunc((x.im-floor(x.im))*6)+trunc((x.re-floor(x.re))*6)) mod 6) of
              0: begin result.re:= 1E-6; result.im:= 0; end;
              1: begin result.re:=-5E+5; result.im:= 0.86602540378444E+6; end;
              2: begin result.re:=-5E-7; result.im:=-0.86602540378444E-6; end;
              3: begin result.re:= 1E+6; result.im:= 0; end;
              4: begin result.re:=-5E-7; result.im:= 0.86602540378444E-6; end;
              5: begin result.re:=-5E+5; result.im:=-0.86602540378444E+6; end;
            end;
         end;
      7: begin
           i:=10;
           while ((x.re>1) or (x.re<-1) or
                  (x.im>1) or (x.im<-1)) and (i>0) do begin
             x.re:=x.re*0.5;
             x.im:=x.im*0.5;
             dec(i);
           end;
             case (floor(x.re*16+16)+floor(x.im*16+16)) mod 6 of
              0: begin result.re:= 1E-6; result.im:= 0; end;
              1: begin result.re:=-5E+5; result.im:= 0.86602540378444E+6; end;
              2: begin result.re:=-5E-7; result.im:=-0.86602540378444E-6; end;
              3: begin result.re:= 1E+6; result.im:= 0; end;
              4: begin result.re:=-5E-7; result.im:= 0.86602540378444E-6; end;
              5: begin result.re:=-5E+5; result.im:=-0.86602540378444E+6; end;
             end;

//       --||--||--||--||
//       --||--||--||--||
//       ||--||--||--||--
//       ||--||--||--||--
//       --||#O#O#O#O--||
//       --||O#O#O#O#--||
//       ||--#O    #O||--
//       ||--O#    O#||--
//       --||#O    #O--||
//       --||O#    O#--||
//       ||--#O#O#O#O||--
//       ||--O#O#O#O#||--
//       --||--||--||--||
//       --||--||--||--||
//       ||--||--||--||--
//       ||--||--||--||--
         end;

      else result:=0;
    end;

  end;
