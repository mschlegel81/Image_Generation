UNIT simplePicChunks;
INTERFACE
USES myPics,myFiles,math,sysutils;
CONST CHUNK_BLOCK_SIZE =64;

TYPE
  T_samplingStatistics=record
    totalSamples,
    totalPixels:longint;
  end;

TYPE
  //41Bytes + shadow bytes
  T_structuredHitColor=record
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
    PROCEDURE initForChunk(CONST xRes,yRes,chunkIdx:longint);
    PROCEDURE copyTo(VAR map:T_floatMap);
    FUNCTION getPicX(CONST localX:longint):longint;
    FUNCTION getPicY(CONST localY:longint):longint;
    FUNCTION markAlias(CONST globalTol:single):boolean;
    FUNCTION getSamplingStatistics:T_samplingStatistics;
  end;

FUNCTION combinedColor(CONST struc:T_structuredHitColor):T_floatColor;
FUNCTION chunksInMap(CONST xRes,yRes:longint):longint;
PROCEDURE markChunksAsPending(VAR map:T_floatMap);
FUNCTION getPendingList(VAR map:T_floatMap):T_pendingList;

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

FUNCTION combinedColor(CONST struc:T_structuredHitColor):T_floatColor;
  begin
    with struc do if antialiasingMask<2
    then result:=rest
    else result:=rest*(0.5/(antialiasingMask and 254));
  end;

CONSTRUCTOR T_colChunk.create;
  begin end;

PROCEDURE T_colChunk.initForChunk(CONST xRes,yRes,chunkIdx:longint);
  VAR i,j:longint;
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
      rest:=black;
      antialiasingMask:=0;
    end;
  end;

DESTRUCTOR T_colChunk.destroy;
  begin
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
      tempColor:array[0..CHUNK_BLOCK_SIZE-1,0..CHUNK_BLOCK_SIZE-1] of T_FloatColor;

  FUNCTION getErrorAt(CONST i,j:longint):double;
    VAR c:array[-1..1,-1..1] of T_floatColor;
        di,dj,ki,kj:longint;
    begin
      if (height<3) or (width<3) then exit(1E6);
      for di:=-1 to 1 do for dj:=-1 to 1 do begin
        ki:=di+i; if ki<0 then ki:=0-ki else if ki>width-1  then ki:=width -1-ki;
        kj:=dj+j; if kj<0 then kj:=0-kj else if kj>height-1 then kj:=height-1-kj;
        c[di,dj]:=tempColor[ki,kj];
      end;
      result:=calcErr(c[-1,-1],c[0,-1],c[1,-1],
                      c[-1, 0],c[0, 0],c[1, 0],
                      c[-1,+1],c[0,+1],c[1,+1]);
    end;

  begin
    result:=false;
    for i:=0 to width-1 do for j:=0 to height-1 do tempColor[i,j]:=combinedColor(col[i,j]);

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
