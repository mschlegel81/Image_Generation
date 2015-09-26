UNIT picChunks;
INTERFACE
USES myPics,myFiles,linAlg3D,math,sysutils;
CONST SHADOWMASK_NONE  =0;  GEOM_HIT_UNKNOWN=0;
      SHADOWMASK_LIGHT =1;  GEOM_HIT_ALL    =1;
      SHADOWMASK_SHADOW=2;  GEOM_HIT_NONE   =2;
      SHADOWMASK_BOTH  =3;  GEOM_HIT_BOTH   =3;
      SPECMASK_NONE  = 0;
      SPECMASK_LIGHT = 4;
      SPECMASK_SHADOW= 8;
      SPECMASK_BOTH  =12;

      CHUNK_BLOCK_SIZE =64;

TYPE
  T_samplingStatistics=record
    totalSamples,
    totalPixels:longint;
  end;

TYPE
  //41Bytes + shadow bytes
  T_structuredHitColor=record
    pathOrAmbient:record
      col:T_FloatColor;
      weight:single;
      scan:boolean;
    end;
    direct:array of record
      col:T_FloatColor;
      sampleCount:longint;
      shadowByte:byte;
    end;
    rest:T_FloatColor;
    antialiasingMask:byte;
  end;

  T_pendingList=array of longint;

  T_colChunk=object
    lastCalculatedTolerance:single;
    x0,y0:longint;
    width,height:longint;
    col:array[0..CHUNK_BLOCK_SIZE-1,0..CHUNK_BLOCK_SIZE-1] of T_structuredHitColor;
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE initForChunk(CONST xRes,yRes,chunkIdx,lightSourceCount:longint);
    PROCEDURE copyTo(VAR map:T_floatMap);
    FUNCTION getPicX(CONST localX:longint):longint;
    FUNCTION getPicY(CONST localY:longint):longint;
    FUNCTION markAlias(CONST globalTol:single):boolean;
    PROCEDURE diffuseShadowMasks;
    FUNCTION getSamplingStatistics:T_samplingStatistics;
  end;

FUNCTION combinedColor(CONST struc:T_structuredHitColor):T_floatColor;
FUNCTION chunksInMap(CONST xRes,yRes:longint):longint;
PROCEDURE markChunksAsPending(VAR map:T_floatMap);
FUNCTION getPendingList(VAR map:T_floatMap):T_pendingList;
FUNCTION getPendingListForRepair(VAR map:T_floatMap):T_pendingList;

OPERATOR := (x:T_samplingStatistics):string;
FUNCTION zeroSamplingStatistics:T_samplingStatistics;
PROCEDURE mergeSamplingStatistics(VAR x:T_samplingStatistics; CONST y:T_samplingStatistics);

IMPLEMENTATION
OPERATOR := (x:T_samplingStatistics):string;
  begin
    with x do if totalSamples>0 then begin
      result:=formatFloat('###0.000',totalSamples/totalPixels);
      while length(result)<8 do result:=' '+result;
    end else result:='   -.---';
  end;

FUNCTION zeroSamplingStatistics:T_samplingStatistics;
  begin
    with result do begin
      totalSamples:=0;
      totalPixels :=0;
    end;
  end;

PROCEDURE mergeSamplingStatistics(VAR x:T_samplingStatistics; CONST y:T_samplingStatistics);
  begin
    inc(x.totalSamples,y.totalSamples);
    inc(x.totalPixels ,y.totalPixels );
    if (x.totalSamples<0) or (x.totalPixels<0) then begin
      x.totalSamples:=((x.totalSamples-y.totalSamples) shr 1)+y.totalSamples;
      x.totalPixels :=((x.totalPixels -y.totalPixels ) shr 1)+y.totalPixels;
    end;
  end;

FUNCTION chunksInMap(CONST xRes,yRes:longint):longint;
  VAR xChunks,yChunks:longint;
  begin
    xChunks:=xRes div CHUNK_BLOCK_SIZE; if xChunks*CHUNK_BLOCK_SIZE<xRes then inc(xChunks);
    yChunks:=yRes div CHUNK_BLOCK_SIZE; if yChunks*CHUNK_BLOCK_SIZE<yRes then inc(yChunks);
    result:=xChunks*yChunks;
  end;

PROCEDURE markChunksAsPending(VAR map:T_floatMap);
  VAR x,y:longint;
  begin
    for y:=map.height-1 downto 0 do for x:=0 to map.width-1 do
      if ((x and 63) in [0,63]) or ((y and 63) in [0,63]) or (odd(x) xor odd(y)) and (((x and 63) in [21,42]) or ((y and 63) in [21,42]))
      then map[x,y]:=white
      else map[x,y]:=black;
  end;

FUNCTION getPendingList(VAR map:T_floatMap):T_pendingList;
  VAR xChunks,yChunks:longint;
      x,y,cx,cy,i:longint;
      isPending:array of array of boolean;
  begin
    randomize;
    xChunks:=map.width  div CHUNK_BLOCK_SIZE; if xChunks*CHUNK_BLOCK_SIZE<map.width  then inc(xChunks);
    yChunks:=map.height div CHUNK_BLOCK_SIZE; if yChunks*CHUNK_BLOCK_SIZE<map.height then inc(yChunks);
    setLength(isPending,xChunks);
    for cx:=0 to length(isPending)-1 do begin
      setLength(isPending[cx],yChunks);
      for cy:=0 to length(isPending[cx])-1 do isPending[cx,cy]:=true;
    end;
    //scan:-----------------------------------------------------
    for y:=map.height-1 downto 0 do begin
      cy:=y div CHUNK_BLOCK_SIZE;
      for x:=0 to map.width-1 do begin
        cx:=x div CHUNK_BLOCK_SIZE;
        if ((x and 63) in [0,63]) or ((y and 63) in [0,63]) or (odd(x) xor odd(y)) and (((x and 63) in [21,42]) or ((y and 63) in [21,42]))
        then isPending[cx,cy]:=isPending[cx,cy] and (map[x,y]=white)
        else isPending[cx,cy]:=isPending[cx,cy] and (map[x,y]=black);
      end;
    end;
    //-----------------------------------------------------:scan
    //transform boolean mask to int array:----------------------
    setLength(result,0);
    for cy:=0 to length(isPending[0])-1 do
    for cx:=length(isPending)-1 downto 0 do if isPending[cx,cy] then begin
      setLength(result,length(result)+1);
      result[length(result)-1]:=cx+xChunks*cy;
    end;
    for cx:=0 to length(isPending)-1 do setLength(isPending[cx],0);
    setLength(isPending,0);
    //----------------------:transform boolean mask to int array
    //scramble result:------------------------------------------
    for i:=0 to length(result)-1 do begin
      cx:=random(length(result));
      repeat cy:=random(length(result)) until cx<>cy;
      x:=result[cx]; result[cx]:=result[cy]; result[cy]:=x;
    end;
  end;

FUNCTION getPendingListForRepair(VAR map:T_floatMap):T_pendingList;
  VAR xChunks,yChunks:longint;
      x,y,cx,cy,i:longint;
      isPending:array of array of longint;
  begin
    xChunks:=map.width  div CHUNK_BLOCK_SIZE; if xChunks*CHUNK_BLOCK_SIZE<map.width  then inc(xChunks);
    yChunks:=map.height div CHUNK_BLOCK_SIZE; if yChunks*CHUNK_BLOCK_SIZE<map.height then inc(yChunks);
    setLength(isPending,xChunks);
    for cx:=0 to length(isPending)-1 do begin
      setLength(isPending[cx],yChunks);
      for cy:=0 to length(isPending[cx])-1 do isPending[cx,cy]:=0;
    end;
    //scan:-----------------------------------------------------
    for y:=map.height-1 downto 0 do begin
      cy:=y div CHUNK_BLOCK_SIZE;
      for x:=0 to map.width-1 do begin
        cx:=x div CHUNK_BLOCK_SIZE;
        if ((x and 63) in [0,63]) or ((y and 63) in [0,63]) or (odd(x) xor odd(y)) and (((x and 63) in [21,42]) or ((y and 63) in [21,42]))
        then begin if map[x,y]=white then begin inc(isPending[cx,cy],4); isPending[cx,cy]:=isPending[cx,cy] or 1 end; end
        else begin if map[x,y]=black then begin inc(isPending[cx,cy],4); isPending[cx,cy]:=isPending[cx,cy] or 2 end; end;
      end;
    end;
    //-----------------------------------------------------:scan
    //transform boolean mask to int array:----------------------
    setLength(result,0);
    writeln;
    for i:=1 to 3+4*CHUNK_BLOCK_SIZE*CHUNK_BLOCK_SIZE do if (i and 3<>3) then begin
      x:=0;
      for cy:=0 to length(isPending[0])-1 do
      for cx:=length(isPending)-1 downto 0 do begin
        if isPending[cx,cy]=i then begin
          setLength(result,length(result)+1);
          result[length(result)-1]:=cx+xChunks*cy;
          inc(x);
        end;
      end;
      if x>0 then writeln(x,' blocks with ',i*25/CHUNK_BLOCK_SIZE/CHUNK_BLOCK_SIZE:0:3,'% probability (',i div 4,' pixels)');
    end;
    writeln('-------------------------------');
    for i:=1 to 3+4*CHUNK_BLOCK_SIZE*CHUNK_BLOCK_SIZE do if (i and 3=3) then begin
      x:=0;
      for cy:=0 to length(isPending[0])-1 do
      for cx:=length(isPending)-1 downto 0 do begin
        if isPending[cx,cy]=i then begin
          setLength(result,length(result)+1);
          result[length(result)-1]:=cx+xChunks*cy;
          inc(x);
        end;
      end;
      if x>0 then writeln(x,' blocks with ',i*25/CHUNK_BLOCK_SIZE/CHUNK_BLOCK_SIZE:0:3,'% probability (',i div 4,' pixels)');
    end;
    writeln('-------------------------------');
    for cx:=0 to length(isPending)-1 do setLength(isPending[cx],0);
    setLength(isPending,0);
    //----------------------:transform boolean mask to int array
  end;

FUNCTION getPathPart(CONST struc:T_structuredHitColor):T_floatColor; inline;
  begin
    with struc do if pathOrAmbient.weight>1E-6 then result:=pathOrAmbient.col*(1/pathOrAmbient.weight) else result:=black;
  end;

FUNCTION getDirectPart(CONST struc:T_structuredHitColor):T_floatColor; inline;
  VAR i:longint;
  begin
    result:=black;
    for i:=0 to length(struc.direct)-1 do with struc.direct[i] do if sampleCount>0 then begin
      result:=result+col*(1/sampleCount);
    end;
  end;

FUNCTION getRestPart(CONST struc:T_structuredHitColor):T_floatColor; inline;
  begin
    with struc do if antialiasingMask<2
    then result:=rest
    else result:=rest*(0.5/(antialiasingMask and 254));
  end;

FUNCTION combinedColor(CONST struc:T_structuredHitColor):T_floatColor;
  begin
    result:=getPathPart(struc)+getDirectPart(struc)+getRestPart(struc);
  end;

CONSTRUCTOR T_colChunk.create;
  begin end;

PROCEDURE T_colChunk.initForChunk(CONST xRes,yRes,chunkIdx,lightSourceCount:longint);
  VAR i,j,k:longint;
  begin
    x0:=0;
    y0:=0;
    for i:=0 to chunkIdx-1 do begin
      inc(x0,CHUNK_BLOCK_SIZE);
      if x0>=xRes then begin
        x0:=0;
        inc(y0,CHUNK_BLOCK_SIZE);
      end;
    end;
    width :=xRes-x0; if width >CHUNK_BLOCK_SIZE then width :=CHUNK_BLOCK_SIZE;
    height:=yRes-y0; if height>CHUNK_BLOCK_SIZE then height:=CHUNK_BLOCK_SIZE;
    for i:=0 to CHUNK_BLOCK_SIZE-1 do for j:=0 to CHUNK_BLOCK_SIZE-1 do with col[i,j] do begin
      with pathOrAmbient do begin
        col:=black;
        weight:=0;
        scan:=true;
      end;
      setLength(direct,lightSourceCount);
      for k:=0 to length(direct)-1 do with direct[k] do begin
        col:=black;
        shadowByte:=SHADOWMASK_NONE;
        sampleCount:=0;
      end;
      rest:=black;
      antialiasingMask:=0;
    end;
  end;

DESTRUCTOR T_colChunk.destroy;
  VAR i,j:longint;
  begin
   for i:=0 to CHUNK_BLOCK_SIZE-1 do for j:=0 to CHUNK_BLOCK_SIZE-1 do with col[i,j] do setLength(direct,0);
  end;

PROCEDURE T_colChunk.copyTo(VAR map:T_floatMap);
  VAR i,j:longint;
  begin
    for j:=0 to height-1 do for i:=0 to width-1 do with col[i,j] do
      map[getPicX(i),getPicY(j)]:=combinedColor(col[i,j]);
  end;

FUNCTION T_colChunk.getPicX(CONST localX:longint):longint;
  begin
    result:=localX+x0;
  end;

FUNCTION T_colChunk.getPicY(CONST localY:longint):longint;
  begin
    result:=localY+y0;
  end;

FUNCTION T_colChunk.markAlias(CONST globalTol:single):boolean;
  VAR i,j,i2,j2:longint;
      localRefFactor:single;
      localTol:single;
      localError:single;
      pathScanCount:longint;
      tempColor:array[0..CHUNK_BLOCK_SIZE-1,0..CHUNK_BLOCK_SIZE-1] of T_FloatColor;

  FUNCTION getErrorAt(CONST i,j:longint):double;
    VAR c:array[-1..1,-1..1] of T_floatColor;
        di,dj,ki,kj:longint;
    begin
      if (height<3) or (width<3) then exit(1E6);
      for di:=-1 to 1 do for dj:=-1 to 1 do begin
        ki:=di+i; if ki<0 then ki:=0-ki else if ki>width-1  then ki:=2*(width -1)-ki;
        kj:=dj+j; if kj<0 then kj:=0-kj else if kj>height-1 then kj:=2*(height-1)-kj;
        c[di,dj]:=tempColor[ki,kj];
      end;
      result:=calcErr(c[-1,-1],c[0,-1],c[1,-1],
                      c[-1, 0],c[0, 0],c[1, 0],
                      c[-1,+1],c[0,+1],c[1,+1]);
    end;

  begin
    result:=false;
    for i:=0 to width-1 do for j:=0 to height-1 do begin
      tempColor[i,j]:=getPathPart(col[i,j]);
      col[i,j].pathOrAmbient.scan:=false;
    end;
    pathScanCount:=0;
    for i:=0 to width-1 do for j:=0 to height-1 do begin
      localRefFactor:=(col[i,j].antialiasingMask and 254)/254;
      localTol:=(1+localRefFactor*localRefFactor)*globalTol;
      localError:=getErrorAt(i,j);
      if localError>localTol then begin
        for i2:=i-1 to i+1 do if (i2>=0) and (i2<width) then
        for j2:=j-1 to j+1 do if (j2>=0) and (j2<height) and not(odd(col[i2,j2].antialiasingMask)) and (col[i2,j2].antialiasingMask<254) then begin
          inc(col[i2,j2].antialiasingMask);
          col[i2,j2].pathOrAmbient.scan:=true;
          inc(pathScanCount);
          result:=true;
        end;
      end;
    end;
    for i:=0 to width-1 do for j:=0 to height-1 do tempColor[i,j]:=getDirectPart(col[i,j])+getRestPart(col[i,j]);
    for i:=0 to width-1 do for j:=0 to height-1 do begin
      localRefFactor:=(col[i,j].antialiasingMask and 254)/254;
      localTol:=(1+localRefFactor*localRefFactor)*globalTol;
      localError:=getErrorAt(i,j);
      if localError>localTol then begin
        for i2:=i-1 to i+1 do if (i2>=0) and (i2<width) then
        for j2:=j-1 to j+1 do if (j2>=0) and (j2<height) and not(odd(col[i2,j2].antialiasingMask)) and (col[i2,j2].antialiasingMask<254) then begin
          inc(col[i2,j2].antialiasingMask);
          result:=true;
        end;
      end;
    end;
  end;

PROCEDURE T_colChunk.diffuseShadowMasks;
  VAR mask:array[0..CHUNK_BLOCK_SIZE-1,0..CHUNK_BLOCK_SIZE-1] of byte;
      i,j,i2,j2,k:longint;
  begin
    for k:=0 to length(col[0,0].direct)-1 do begin
      for i:=0 to width-1 do for j:=0 to height-1 do begin
        mask[i,j]:=SHADOWMASK_NONE;
        for i2:=i-1 to i+1 do if (i2>=0) and (i2<width) then
        for j2:=j-1 to j+1 do if (j2>=0) and (j2<height) then mask[i,j]:=mask[i,j] or col[i,j].direct[k].shadowByte;
      end;
      for i:=0 to width-1 do for j:=0 to height-1 do col[i,j].direct[k].shadowByte:=mask[i,j];
    end;
  end;

FUNCTION T_colChunk.getSamplingStatistics:T_samplingStatistics;
  VAR i,j,s:longint;
  begin
    result:=zeroSamplingStatistics;
    for i:=0 to width-1 do for j:=0 to height-1 do begin
      s:=2*(col[i,j].antialiasingMask and 254);
      if s=0 then s:=1;
      inc(result.totalSamples,s);
      inc(result.totalPixels);
    end;
  end;

end.
