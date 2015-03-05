PROGRAM testDarts;
USES darts;
VAR seed    :longint;
    bestSeed:longint=0;
    dist    :double;
    bestDist:double=-1;
    i0:longint=1;
    i1:longint=12;

begin
  repeat
    bestDist:=-1;
    for seed:=0+100*i0 to round(20000/i1)+100*i0 do begin
      dist:=initDarts(seed,bestDist,i0,i1);
      if dist>bestDist then begin
        bestDist:=dist;
        bestSeed:=seed;
        writeln(seed:30,' ',dist,' ',1/sqrt((1+i1)*dist));
      end;
    end;
    initDarts(bestSeed,bestDist,i0,i1);
    writeConstantDarts(i1);

    i0:=i1+1;
    i1:=i1+16;
    if i1>1015 then i1:=1015;
  until i0>=1016;



end.