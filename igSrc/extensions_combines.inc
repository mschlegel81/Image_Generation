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
PROCEDURE combineImage(combKind:T_imageCombinationKind; fileName:string; VAR inOut:T_FloatMap);
FUNCTION combineImage(cmd:string; VAR inOut:T_FloatMap):boolean;
PROCEDURE displayHelpOnImageCombinations;

PROCEDURE signImage(VAR pic:T_floatMap);
PROCEDURE watermarkImage(VAR pic:T_floatMap);
{$endif}
{$ifdef include_implementation}

PROCEDURE combineImage(combKind:T_imageCombinationKind; fileName:string; VAR inOut:T_FloatMap);
  VAR pic2:T_FloatMap;
      pt,pt2:P_floatColor;
      i,j:longint;
      c1,c2:T_floatColor;
      w:single;
  begin
    case combKind of
      ic_watermark:watermarkImage(inOut);
      ic_sign     :signImage(inOut);
      else begin
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
    end;
  end;

FUNCTION combineImage(cmd:string; VAR inOut:T_FloatMap):boolean;
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
    writeln('  -watermark');
    writeln('  -sign');
  end;

PROCEDURE signImage(VAR pic:T_floatMap);
  VAR sig,blurredSig:T_ByteMap;
      factor:single;
      x,y,dx,dy:longint;
      markColor,backColor:T_floatColor;
      levelWhite,levelBlack:double;
  begin
    sig.create(sigFileName);
    factor:=sqrt(pic.size)/sqrt(sig.size)/15;
    sig.resize(round(sig.width*factor),round(sig.height*factor));
    blurredSig.createCopy(sig);
    blurImage(blurredSig,blurredSig.diagonal*0.01);

    dx:=sig.width -pic.width;
    dy:=sig.height-pic.height;
    levelWhite:=0;
    levelBlack:=0;
    for y:=0 to sig.height-1 do for x:=0 to sig.width -1 do begin
      factor:=sig.pixel[x,y]/255;
      if factor>=1/255 then begin
        levelWhite:=levelWhite+absDist(pic.pixel[x-dx,y-dy],pic.pixel[x-dx,y-dy]*(1-factor)+white*factor);
        levelBlack:=levelBlack+absDist(pic.pixel[x-dx,y-dy],pic.pixel[x-dx,y-dy]*(1-factor)+black*factor);
      end;
    end;
    if levelWhite>levelBlack
      then begin markColor:=white; backColor:=black; end
      else begin markColor:=black; backColor:=white; end;

    for y:=0 to sig.height-1 do for x:=0 to sig.width -1 do begin
      factor  :=3*blurredSig.pixel[x,y]/255;
      if factor>1/255 then pic.pixel[x-dx,y-dy]:=(pic.pixel[x-dx,y-dy])*(1-factor)+backColor*factor;

      factor  :=sig.pixel[x,y]/255;
      if factor>1/255 then pic.pixel[x-dx,y-dy]:=(pic.pixel[x-dx,y-dy])*(1-factor)+markColor*factor;
    end;
    sig.destroy;
    blurredSig.destroy;
  end;

PROCEDURE watermarkImage(VAR pic:T_floatMap);
  VAR sig:T_ByteMap;
      factor:single;
      x,y,dx,dy:longint;
  begin
    sig.create(sigFileName);
    if pic.width/sig.width>pic.height/sig.height
      then factor:=pic.height/sig.height
      else factor:=pic.width/sig.width;
    sig.resize(round(sig.width*factor),round(sig.height*factor));
    dx:=(pic.width -sig.width )  shr 1;
    dy:=(pic.height-sig.height) shr 1;
    for y:=0 to sig.height-1 do for x:=0 to sig.width -1 do begin
      factor:=sig.pixel[x,y]/510;
      if factor>=1/510 then pic.pixel[x+dx,y+dy]:=pic.pixel[x+dx,y+dy]*(1-factor)+white*((0.25+0.5*greyLevel(pic.pixel[x+dx,y+dy]))*factor);
    end;
    sig.destroy;
  end;

{$endif}
