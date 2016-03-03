{$ifdef include_interface}
TYPE T_imageCombinationKind=(ic_add,ic_mult,ic_min,ic_max,ic_screen,ic_back,ic_watermark,ic_sign,ic_divide);
CONST sigFileName='mssignature.png';
CONST C_combineCommandAbstraction:array [T_imageCombinationKind] of T_commandAbstraction=(
  (isFile:false; leadingSign:'+'; cmdString:'file';     paramCount:-1),
  (isFile:false; leadingSign:'*'; cmdString:'file';     paramCount:-1),
  (isFile:false; leadingSign:'-'; cmdString:'min';      paramCount:-1),
  (isFile:false; leadingSign:'-'; cmdString:'max';      paramCount:-1),
  (isFile:false; leadingSign:'-'; cmdString:'screen';   paramCount:-1),
  (isFile:false; leadingSign:'-'; cmdString:'back';     paramCount:-1),
  (isFile:false; leadingSign:'-'; cmdString:'watermark';paramCount: 0),
  (isFile:false; leadingSign:'-'; cmdString:'sign';     paramCount: 0),
  (isFile:false; leadingSign:'/'; cmdString:'file';     paramCount:-1));
PROCEDURE combineImage(combKind:T_imageCombinationKind; fileName:string; VAR inOut:T_rawImage);
FUNCTION combineImage(cmd:string; VAR inOut:T_rawImage):boolean;
PROCEDURE displayHelpOnImageCombinations;

{$endif}
{$ifdef include_implementation}

PROCEDURE combineImage(combKind:T_imageCombinationKind; fileName:string; VAR inOut:T_rawImage);
  VAR pic2:T_rawImage;
      pt,pt2:P_floatColor;
      i,j:longint;
      c1,c2:T_floatColor;
      w:single;
  begin
    pic2.create(fileName);
    pt :=inOut.rawData;
    pt2:=pic2.rawData;
    case combKind of
      ic_add: for i:=0 to min(inOut.size,pic2.size)-1 do pt[i]:=pt[i]+pt2[i];
      ic_max: for i:=0 to min(inOut.size,pic2.size)-1 do begin
           c1:=pt [i];
           c2:=pt2[i];
           for j:=0 to 2 do if c2[j]>c1[j] then c1[j]:=c2[j];
           pt[i]:=c1;
         end;
      ic_min: for i:=0 to min(inOut.size,pic2.size)-1 do begin
           c1:=pt [i];
           c2:=pt2[i];
           for j:=0 to 2 do if c2[j]<c1[j] then c1[j]:=c2[j];
           pt[i]:=c1;
         end;
      ic_back: for i:=0 to min(inOut.size,pic2.size)-1 do begin
           c1:=pt [i];
           c2:=pt2[i];
                           w:=c2[0];
           if c2[1]<w then w:=c2[1];
           if c2[2]<w then w:=c2[2];
           if 1    <w then w:=1;
           if 0    >w then w:=0;
           pt[i]:=c2+(c1-c2)*w;
         end;
      ic_mult: for i:=0 to min(inOut.size,pic2.size)-1 do begin
           c1:=pt [i];
           c2:=pt2[i];
           c1[0]:=c1[0]*c2[0];
           c1[1]:=c1[1]*c2[1];
           c1[2]:=c1[2]*c2[2];
           pt[i]:=c1;
         end;
      ic_divide: for i:=0 to min(inOut.size,pic2.size)-1 do begin
           c1:=pt [i];
           c2:=pt2[i];
           c1[0]:=c1[0]/c2[0];
           c1[1]:=c1[1]/c2[1];
           c1[2]:=c1[2]/c2[2];
           pt[i]:=c1;
         end;
      ic_screen: for i:=0 to min(inOut.size,pic2.size)-1 do begin
           c1:=pt [i];
           c2:=pt2[i];
           c1[0]:=1-(1-c1[0])*(1-c2[0]);
           c1[1]:=1-(1-c1[1])*(1-c2[1]);
           c1[2]:=1-(1-c1[2])*(1-c2[2]);
           pt[i]:=c1;
         end;
    end;
    pic2.destroy;
  end;

FUNCTION combineImage(cmd:string; VAR inOut:T_rawImage):boolean;
  VAR fk:T_imageCombinationKind;
      ep:T_extendedParameter;
  begin
    ep:=extendedParam(cmd);
    result:=false;
    for fk:=ic_add to ic_divide do if matches(ep,C_combineCommandAbstraction[fk]) then begin
      combineImage(fk,ep.stringSuffix,inOut);
      result:=true;
    end;
  end;

PROCEDURE displayHelpOnImageCombinations;
  begin
    writeln('Image combinations');
    writeln('  +file:<filename> ');
    writeln('  *file:<filename> ');
    writeln('  /file:<filename> ');
    writeln('  -min:<filename> ');
    writeln('  -max:<filename> ');
    writeln('  -screen:<filename> ');
    writeln('  -back:<filename> ');
  end;

{$endif}
