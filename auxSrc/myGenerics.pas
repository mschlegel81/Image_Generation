UNIT myGenerics;

{mode objfpc}{H+}

INTERFACE
USES math,sysutils,myFiles;
TYPE
  GENERIC G_list<ENTRY_TYPE>=object
    TYPE ENTRY_TYPE_ARRAY=array of ENTRY_TYPE;
    private VAR
      entry:ENTRY_TYPE_ARRAY;
      sortedUntilIndex:longint;
      isUnique:boolean;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      FUNCTION contains(CONST value:ENTRY_TYPE):boolean;
      FUNCTION indexOf(CONST value:ENTRY_TYPE):longint;
      PROCEDURE add(CONST value:ENTRY_TYPE);
      PROCEDURE remValue(CONST value:ENTRY_TYPE);
      PROCEDURE remValues(CONST values:ENTRY_TYPE_ARRAY);
      PROCEDURE remIndex(CONST index:longint);
      PROCEDURE addArr(CONST values:ENTRY_TYPE_ARRAY);
      PROCEDURE clear;
      PROCEDURE sort;
      PROCEDURE unique;
      FUNCTION size:longint;
      FUNCTION getEntry(CONST index:longint):ENTRY_TYPE;
      PROPERTY element[index:longint]:ENTRY_TYPE read getEntry; default;
      FUNCTION elementArray:ENTRY_TYPE_ARRAY;
  end;

  GENERIC G_sparseArray<ENTRY_TYPE>=object
    private
      TYPE INDEXED_ENTRY=record index:longint; value:ENTRY_TYPE; end;
      VAR map:array of array of INDEXED_ENTRY;
      hashMask:longint;
      entryCount:longint;
      FUNCTION indexInMap(CONST mapIdx,searchIdx:longint):longint;
      PROCEDURE rehash(CONST grow:boolean);
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROCEDURE add(CONST index:longint; CONST value:ENTRY_TYPE);
      FUNCTION containsIndex(CONST index:longint; OUT value:ENTRY_TYPE):boolean;
      FUNCTION remove(CONST index:longint):boolean;
      FUNCTION size:longint;
      FUNCTION getEntry(CONST iterator:longint):ENTRY_TYPE;
      FUNCTION getIndex(CONST iterator:longint):longint;
  end;

  T_arrayOfString=array of string;

  { G_stringIndexedMap }
  GENERIC G_stringKeyMap<VALUE_TYPE>=object
    TYPE VALUE_TYPE_ARRAY=array of VALUE_TYPE;
         KEY_VALUE_PAIR=record
           hash:longint;
           key:string;
           value:VALUE_TYPE;
         end;
    private VAR
      entryCount:longint;
      rebalanceFac:double;
      bitMask:longint;
      bucket:array of array of KEY_VALUE_PAIR;
      PROCEDURE rehash(grow:boolean);
    public
      CONSTRUCTOR create(rebalanceFactor:double);
      CONSTRUCTOR create();
      DESTRUCTOR destroy;
      FUNCTION containsKey(CONST key:string; OUT value:VALUE_TYPE):boolean;
      FUNCTION get(CONST key:string):VALUE_TYPE;
      PROCEDURE put(CONST key:string; CONST value:VALUE_TYPE);
      PROCEDURE dropKey(CONST key:string);
      PROCEDURE clear;
      FUNCTION keySet:T_arrayOfString;
      FUNCTION valueSet:VALUE_TYPE_ARRAY;
      FUNCTION size:longint;
  end;

  T_listOfString=specialize G_list<string>;
  T_listOfDoubles=specialize G_list<double>;
  
  //CONSTRUCTOR G_hashMap.create(hashFunc:HASH_FUNC; rebalanceFactor:double);
  //CONSTRUCTOR G_hashMap.create(hashFunc:HASH_FUNC);
  //PROCEDURE G_hashMap.rehash(grow:boolean);
  //DESTRUCTOR G_hashMap.destroy;
  //FUNCTION G_hashMap.containsKey(key:KEY_TYPE; OUT value:VALUE_TYPE):boolean;
  //PROCEDURE G_hashMap.put(key:KEY_TYPE; value:VALUE_TYPE);
  //PROCEDURE G_hashMap.dropKey(key:KEY_TYPE);
  //PROCEDURE G_hashMap.clear;
  //FUNCTION G_hashMap.keySet:KEY_TYPE_ARRAY;
  //FUNCTION G_hashMap.valueSet:VALUE_TYPE_ARRAY;
  //FUNCTION G_hashMap.size:longint;


  { T_indexSet }

  T_indexSet=object(T_serializable)
    private
      mask:array of byte;
      PROCEDURE setEntry(index:longint; value:boolean);
      FUNCTION  getEntry(index:longint):boolean;
    public
      CONSTRUCTOR create;
      DESTRUCTOR destroy;
      PROPERTY entry[index:longint]:boolean read getEntry write setEntry; default;
      FUNCTION maxEntryIndex:longint;
      FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual; overload; //liest die Inhalte des Objektes aus einer bereits geöffneten Datei und gibt true zurück gdw. kein Fehler auftrat
      PROCEDURE saveToFile(VAR F:T_file);           virtual; overload; //schreibt die Inhalte des Objektes in eine bereits geöffnete Datei
      FUNCTION extractFrom(VAR s:T_arrayOfString):T_arrayOfString;
      FUNCTION equals(CONST s:T_indexSet):boolean;
      FUNCTION lesser(CONST s:T_indexSet):boolean;
  end;

  P_setOfPointers=^T_setOfPointers;
  T_setOfPointers=object
    dat:array of array of pointer;
    hashMask:longint;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION contains(CONST p:pointer):boolean;
    PROCEDURE put(CONST p:pointer);
  end;



FUNCTION hashOfAnsiString(x:ansistring):longint; inline;

IMPLEMENTATION

FUNCTION hashOfAnsiString(x:ansistring):longint; inline;
  VAR i:longint;
  begin
    {$Q-}
    result:=length(x);
    for i:=1 to length(x) do result:=result*31+ord(x[i]);
  end;

CONSTRUCTOR G_list.create;
  begin clear; end;

DESTRUCTOR G_list.destroy;
  begin clear; end;

FUNCTION G_list.contains(CONST value:ENTRY_TYPE):boolean;
  begin result:=indexOf(value)>=0; end;

FUNCTION G_list.indexOf(CONST value:ENTRY_TYPE):longint;
  VAR i0,i1:longint;
  begin
    i0:=0;
    i1:=sortedUntilIndex-1;
    while i1>=i0 do begin
      result:=(i0+i1) shr 1;
      if      entry[result]<value then i0:=result+1
      else if entry[result]>value then i1:=result-1
      else exit(result);
    end;
    for i0:=sortedUntilIndex to length(entry)-1 do
      if entry[i0]=value then exit(i0);
    result:=-1;
  end;

PROCEDURE G_list.add(CONST value:ENTRY_TYPE);
  VAR i:longint;
  begin
    i:=length(entry);
    setLength(entry,i+1);
    entry[i]:=value;
    isUnique:=false;
  end;

PROCEDURE G_list.remIndex(CONST index:longint);
  VAR i:longint;
  begin
    if (index>=0) and (index<length(entry)) then begin
      if index<sortedUntilIndex then dec(sortedUntilIndex);
      for i:=index to length(entry)-2 do entry[i]:=entry[i+1];
      setLength(entry,length(entry)-1);
    end;
  end;

PROCEDURE G_list.remValue(CONST value:ENTRY_TYPE);
  VAR i:longint;
  begin
    i:=indexOf(value);
    while i>=0 do begin
      remIndex(i);
      i:=indexOf(value);
    end;
  end;

PROCEDURE G_list.remValues(CONST values:ENTRY_TYPE_ARRAY);
  VAR i:longint;
  begin
    for i:=0 to length(values)-1 do remValue(values[i]);
  end;

PROCEDURE G_list.addArr(CONST values:ENTRY_TYPE_ARRAY);
  VAR i,i0:longint;
  begin
    i0:=length(entry);
    setLength(entry,length(entry)+length(values));
    for i:=0 to length(values)-1 do entry[i0+i]:=values[i];
    isUnique:=false;
  end;

PROCEDURE G_list.clear;
  begin
    setLength(entry,0);
    sortedUntilIndex:=0;
    isUnique:=true;
  end;

PROCEDURE G_list.sort;
  VAR scale    :longint;
      i,j0,j1,k:longint;
      temp     :ENTRY_TYPE_ARRAY;
  begin
if sortedUntilIndex<length(entry) then begin
      scale:=1;
      setLength(temp,length(entry)-sortedUntilIndex);
      while scale<length(temp) do begin
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while i<length(temp) do begin
          j0:=sortedUntilIndex+i;
          j1:=sortedUntilIndex+i+scale;
          k :=i;
          while (j0<sortedUntilIndex+i+scale) and (j1<sortedUntilIndex+i+scale+scale) and (j1<length(entry)) do begin
            if entry[j0]<=entry[j1]
              then begin temp[k]:=entry[j0]; inc(k); inc(j0); end
              else begin temp[k]:=entry[j1]; inc(k); inc(j1); end;
          end;
          while (j0<sortedUntilIndex+i+scale)       and (j0<length(entry)) do begin temp[k]:=entry[j0]; inc(k); inc(j0); end;
          while (j1<sortedUntilIndex+i+scale+scale) and (j1<length(entry)) do begin temp[k]:=entry[j1]; inc(k); inc(j1); end;
          inc(i,scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale,scale);
        if (scale<length(temp)) then begin
          //The following is equivalent to the above with swapped roles of "list" and "temp".
          //While making the code a little more complicated it avoids unnecessary copys.
          //merge lists of size [scale] to lists of size [scale+scale]:---------------
          i:=0;
          while i<length(temp) do begin
            j0:=i;
            j1:=i+scale;
            k :=sortedUntilIndex+i;
            while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(temp)) do begin
              if temp[j0]<=temp[j1]
                then begin entry[k]:=temp[j0]; inc(k); inc(j0); end
                else begin entry[k]:=temp[j1]; inc(k); inc(j1); end;
            end;
            while (j0<i+scale)       and (j0<length(temp)) do begin entry[k]:=temp[j0]; inc(k); inc(j0); end;
            while (j1<i+scale+scale) and (j1<length(temp)) do begin entry[k]:=temp[j1]; inc(k); inc(j1); end;
            inc(i,scale+scale);
          end;
          //---------------:merge lists of size [scale] to lists of size [scale+scale]
          inc(scale,scale);
        end else for k:=0 to length(temp)-1 do entry[sortedUntilIndex+k]:=temp[k];
      end;
      setLength(temp,length(entry));
      for k:=0 to length(entry)-1 do temp[k]:=entry[k];
      j0:=0;
      j1:=sortedUntilIndex;
      k:=0;
      while (j0<sortedUntilIndex) and (j1<length(entry)) do begin
        if temp[j0]<=temp[j1]
          then begin entry[k]:=temp[j0]; inc(k); inc(j0); end
          else begin entry[k]:=temp[j1]; inc(k); inc(j1); end;
      end;
      while (j0<sortedUntilIndex) do begin entry[k]:=temp[j0]; inc(k); inc(j0); end;
      while (j1<length(temp))     do begin entry[k]:=temp[j1]; inc(k); inc(j1); end;
      sortedUntilIndex:=length(entry);
    end;
  end;

PROCEDURE G_list.unique;
  VAR i,j:longint;
  begin
    if not(isUnique) then begin
      sort;
      j:=1;
      for i:=1 to length(entry)-1 do if entry[i]<>entry[i-1] then begin
        entry[j]:=entry[i]; inc(j);
      end;
      setLength(entry,j);
      sortedUntilIndex:=length(entry);
    end;
    isUnique:=true;
  end;

FUNCTION G_list.size:longint;
  begin result:=length(entry); end;

FUNCTION G_list.getEntry(CONST index:longint):ENTRY_TYPE;
  begin result:=entry[index]; end;

FUNCTION G_list.elementArray:ENTRY_TYPE_ARRAY;
  begin result:=entry; end;


{CONSTRUCTOR G_hashMap.create(hashFunc:HASH_FUNC; rebalanceFactor:double);
  begin
    hash:=hashFunc;
    setLength(bucket,1);
    bitMask:=0;
    rebalanceFac:=rebalanceFactor;
    entryCount:=0;
  end;

CONSTRUCTOR G_hashMap.create(hashFunc:HASH_FUNC);
  begin
    if hashFunc=nil then Raise Exception.create ('Hash func is nil!');
    hash:=hashFunc;
    setLength(bucket,1);
    bitMask:=0;
    rebalanceFac:=4;
    entryCount:=0;
  end;

PROCEDURE G_hashMap.rehash(grow:boolean);
  VAR i,i0,j,k,c0,c1,newMask:longint;
      temp:array of KEY_VALUE_PAIR;
  begin
    if grow then begin
      i0:=length(bucket);
      setLength(bucket,i0+i0);
      newMask:=i0+i0-1;
      for i:=0 to i0-1 do begin
        temp:=bucket[i];
        setLength(bucket[i+i0],length(bucket[i]));
        c0:=0;
        c1:=0;
        for j:=0 to length(temp)-1 do begin
          k:=temp[j].hash and newMask;
          if k=i then begin
            bucket[i][c0]:=temp[j];
            inc(c0);
          end else begin
            bucket[k][c1]:=temp[j];
            inc(c1);
          end;
        end;
        setLength(bucket[i   ],c0);
        setLength(bucket[i+i0],c1);
      end;
      bitMask:=newMask;
    end else begin
      i0:=length(bucket) shr 1;
      newMask:=i0-1;
      for i:=0 to i0-1 do
      for j:=0 to length(bucket[i0+i])-1 do begin
        setLength(bucket[i],length(bucket[i])+1);
        bucket[i][length(bucket[i])-1]:=bucket[i0+i][j];
      end;
    end;
  end;


DESTRUCTOR G_hashMap.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(bucket)-1 do setLength(bucket[i],0);
    setLength(bucket,0);
  end;

FUNCTION G_hashMap.containsKey(key:KEY_TYPE; OUT value:VALUE_TYPE):boolean;
  VAR i,j:longint;
  begin
    i:=hash(key) and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if (j<length(bucket[i])) then begin
      value:=bucket[i][j].value;
      result:=true;
    end else result:=false;
  end;

PROCEDURE G_hashMap.put(key:KEY_TYPE; value:VALUE_TYPE);
  VAR i,j,h:longint;
  begin
    h:=hash(key);
    i:=h and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if j>=length(bucket[i]) then begin
      setLength(bucket[i],j+1);
      bucket[i][j].key:=key;
      bucket[i][j].hash:=h;
      bucket[i][j].value:=value;
      inc(entryCount);
      if entryCount>length(bucket)*rebalanceFac then rehash(true);
    end else begin
      bucket[i][j].value:=value;
    end;
  end;

PROCEDURE G_hashMap.dropKey(key:KEY_TYPE);
  VAR i,j:longint;
  begin
    i:=hash(key) and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if j<length(bucket[i]) then begin
      while j<length(bucket[i])-1 do begin
        bucket[i][j]:=bucket[i][j+1];
        inc(j);
      end;
      setLength(bucket[i],length(bucket[i])-1);
      dec(entryCount);
      if entryCount<0.4*length(bucket)*rebalanceFac then rehash(false);
    end;
  end;

PROCEDURE G_hashMap.clear;
  VAR i:longint;
  begin
    for i:=0 to length(bucket)-1 do setLength(bucket[i],0);
    setLength(bucket,1);
    bitMask:=0;
    entryCount:=0;
  end;

FUNCTION G_hashMap.keySet:KEY_TYPE_ARRAY;
  VAR k,i,j:longint;
  begin
    setLength(result,entryCount);
    k:=0;
    for i:=0 to length(bucket)-1 do
    for j:=0 to length(bucket[i])-1 do begin
      result[k]:=bucket[i][j].key;
      inc(k);
    end;
  end;

FUNCTION G_hashMap.valueSet:VALUE_TYPE_ARRAY;
  VAR k,i,j:longint;
  begin
    setLength(result,entryCount);
    k:=0;
    for i:=0 to length(bucket)-1 do
    for j:=0 to length(bucket[i])-1 do begin
      result[k]:=bucket[i][j].value;
      inc(k);
    end;
  end;

FUNCTION G_hashMap.size:longint;
  begin
    result:=entryCount;
  end;}

CONSTRUCTOR G_sparseArray.create;
  begin
    hashMask:=0;
    entryCount:=0;
    setLength(map,1);
    setLength(map[0],0);
  end;

DESTRUCTOR G_sparseArray.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(map)-1 do setLength(map[i],0);
    setLength(map,0);
  end;

FUNCTION G_sparseArray.indexInMap(CONST mapIdx,searchIdx:longint):longint;
  VAR i0:longint;
  begin
    for i0:=0 to length(map[mapIdx])-1 do if map[mapIdx,i0].index=searchIdx then exit(i0);
    result:=-1;
  end;

PROCEDURE G_sparseArray.add(CONST index:longint; CONST value:ENTRY_TYPE);
  VAR hash:longint;
      i:longint;
  begin
    hash:=index and hashMask;
    i:=indexInMap(hash,index);
    if i<0 then begin
      i:=length(map[hash]);
      setLength(map[hash],i+1);
      map[hash,i].index:=index;
      inc(entryCount);
      map[hash,i].value:=value;
      if length(map) shl 4<entryCount then rehash(true);
    end else map[hash,i].value:=value;
  end;

FUNCTION G_sparseArray.containsIndex(CONST index:longint; OUT value:ENTRY_TYPE):boolean;
  VAR hash:longint;
      i:longint;
  begin
    hash:=index and hashMask;
    i:=indexInMap(hash,index);
    if i<0 then result:=false
    else begin
      result:=true;
      value:=map[hash,i].value;
    end;
  end;

FUNCTION G_sparseArray.remove(CONST index:longint):boolean;
  VAR hash:longint;
      i,j:longint;
  begin
    hash:=index and hashMask;
    i:=indexInMap(hash,index);
    if i<0 then result:=false
    else begin
      for j:=i to length(map[hash])-2 do map[hash][j]:=map[hash][j+1];
      setLength(map[hash],length(map[hash])-1);
      dec(entryCount);
      result:=true;
      if length(map) shl 3>entryCount then rehash(false);
    end;
  end;

PROCEDURE G_sparseArray.rehash(CONST grow:boolean);
  VAR i,k,j0,j1,oldLen:longint;
  begin
    if (length(map)>1) and not(grow) then begin
      //merge
      k:=length(map) shr 1;
      for i:=0 to k-1 do begin
        j0:=length(map[i]);
        setLength(map[i],length(map[i])+length(map[i+k]));
        for j1:=0 to length(map[i+k])-1 do map[i][j1+j0]:=map[i+k][j1];
        setLength(map[i+k],0);
      end;
      setLength(map,k);
      hashMask:=k-1;
    end else if grow then begin
      //split
      oldLen:=length(map);
      setLength(map,oldLen+oldLen);
      hashMask:=(oldLen+oldLen)-1;
      for i:=0 to oldLen-1 do begin
        j0:=0;
        setLength(map[i+oldLen],length(map[i])); j1:=0;
        for k:=0 to length(map[i])-1 do begin
          if (hashMask and map[i][k].index)=i
            then begin map[i       ][j0]:=map[i][k]; inc(j0); end
            else begin map[i+oldLen][j1]:=map[i][k]; inc(j1); end;
        end;
        setLength(map[i],j0);
        setLength(map[i+oldLen],j1);
      end;
    end;
  end;

FUNCTION G_sparseArray.size:longint;
  begin result:=entryCount;end;

FUNCTION G_sparseArray.getIndex(CONST iterator:longint):longint;
  VAR i,k:longint;
  begin
    k:=iterator;
    i:=0;
    while (i<length(map)) and (k>=length(map[i])) do begin
      dec(k,length(map[i]));
      inc(i);
    end;
    if (i<length(map)) then result:=map[i,k].index
                       else result:=-1;
  end;

FUNCTION G_sparseArray.getEntry(CONST iterator:longint):ENTRY_TYPE;
  VAR i,k:longint;
  begin
    k:=iterator;
    i:=0;
    while (i<length(map)) and (k>=length(map[i])) do begin
      dec(k,length(map[i]));
      inc(i);
    end;
    if (i<length(map)) then result:=map[i,k].value
                       else result:=map[0,0].value;
  end;

PROCEDURE T_indexSet.setEntry(index:longint; value:boolean);
  VAR i,j:longint;
      m:byte;
  begin
    if index>=0 then begin
      i:=index shr 3;
      m:=1 shl (index and 7);
      if value then begin
        if i>=length(mask) then begin
          j:=length(mask);
          setLength(mask,i+1);
          while j<length(mask) do begin
            mask[j]:=0;
            inc(j);
          end;
        end;
        mask[i]:=mask[i] or m;
      end else begin
        if i<length(mask) then mask[i]:=mask[i] and not(m);
        if (i=length(mask)-1) and (mask[i]=0) then setLength(mask,length(mask)-1);
      end;
    end;
  end;

FUNCTION  T_indexSet.getEntry(index:longint):boolean;
  VAR i:longint;
      m:byte;
  begin
    i:=index shr 3;
    m:=1 shl (index and 7);
    result:=(i<length(mask)) and (mask[i] and m>0);
  end;

CONSTRUCTOR T_indexSet.create;
  begin
    setLength(mask,0);
  end;

DESTRUCTOR T_indexSet.destroy;
  begin
    setLength(mask,0);
  end;

FUNCTION T_indexSet.maxEntryIndex:longint;
  begin
    result:=length(mask) shl 3-1;
  end;

FUNCTION  T_indexSet.loadFromFile(VAR F:T_file):boolean;
  VAR i,len:longint;
  begin
    len:=f.readLongint;
    if len<0 then exit(false);
    setLength(mask,len);
    for i:=0 to len-1 do mask[i]:=f.readByte;
    result:=true;
  end;

PROCEDURE T_indexSet.saveToFile(VAR F:T_file);
  VAR i:longint;
  begin
    f.writelongint(length(mask));
    for i:=0 to length(mask)-1 do f.writeByte(mask[i]);
  end;

FUNCTION T_indexSet.extractFrom(VAR s:T_arrayOfString):T_arrayOfString;
  VAR i,iMax:longint;
  begin
    iMax:=maxEntryIndex;
    if length(s)<=iMax then iMax:=length(s)-1;
    setLength(result,0);
    for i:=0 to iMax do if getEntry(i) then begin
      setLength(result,length(result)+1);
      result[length(result)-1]:=s[i];
    end;
  end;

FUNCTION T_indexSet.equals(CONST s: T_indexSet): boolean;
  VAR i:longint;
  begin
    result:=length(mask)=length(s.mask);
    i:=0;
    while result and (i<length(mask)) do begin
      result:=result and (mask[i]=s.mask[i]);
      inc(i);
    end;
  end;

FUNCTION T_indexSet.lesser(CONST s: T_indexSet): boolean;
  VAR i:longint;
  begin
    for i:=0 to min(length(mask),length(s.mask))-1 do begin
      if      mask[i]<s.mask[i] then exit(true)
      else if mask[i]>s.mask[i] then exit(false);
    end;
    result:= length(s.mask)>length(mask);
  end;

PROCEDURE G_stringKeyMap.rehash(grow: boolean);
  VAR i,i0,j,k,c0,c1,newMask:longint;
      temp:array of KEY_VALUE_PAIR;
  begin
    if grow then begin
      i0:=length(bucket);
      setLength(bucket,i0+i0);
      newMask:=i0+i0-1;
      for i:=0 to i0-1 do begin
        temp:=bucket[i];
        setLength(bucket[i+i0],length(bucket[i]));
        c0:=0;
        c1:=0;
        for j:=0 to length(temp)-1 do begin
          k:=temp[j].hash and newMask;
          if k=i then begin
            bucket[i][c0]:=temp[j];
            inc(c0);
          end else begin
            bucket[k][c1]:=temp[j];
            inc(c1);
          end;
        end;
        setLength(bucket[i   ],c0);
        setLength(bucket[i+i0],c1);
      end;
      bitMask:=newMask;
    end else begin
      i0:=length(bucket) shr 1;
      newMask:=i0-1;
      for i:=0 to i0-1 do
      for j:=0 to length(bucket[i0+i])-1 do begin
        setLength(bucket[i],length(bucket[i])+1);
        bucket[i][length(bucket[i])-1]:=bucket[i0+i][j];
      end;
    end;
  end;

CONSTRUCTOR G_stringKeyMap.create(rebalanceFactor: double);
begin
  setLength(bucket,1);
  bitMask:=0;
  rebalanceFac:=rebalanceFactor;
  entryCount:=0;
end;

CONSTRUCTOR G_stringKeyMap.create();
begin
  create(4);
end;

DESTRUCTOR G_stringKeyMap.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(bucket)-1 do setLength(bucket[i],0);
    setLength(bucket,0);
  end;

FUNCTION G_stringKeyMap.containsKey(CONST key: string; OUT value: VALUE_TYPE): boolean;
  VAR i,j:longint;
  begin
    i:=hashOfAnsiString(key) and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if (j<length(bucket[i])) then begin
      value:=bucket[i][j].value;
      result:=true;
    end else result:=false;
  end;

FUNCTION G_stringKeyMap.get(CONST key: string): VALUE_TYPE;
  begin
    containsKey(key,result);
  end;

PROCEDURE G_stringKeyMap.put(CONST key: string; CONST value: VALUE_TYPE);
  VAR i,j,h:longint;
  begin
    h:=hashOfAnsiString(key);
    i:=h and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if j>=length(bucket[i]) then begin
      setLength(bucket[i],j+1);
      bucket[i][j].key:=key;
      bucket[i][j].hash:=h;
      bucket[i][j].value:=value;
      inc(entryCount);
      if entryCount>length(bucket)*rebalanceFac then rehash(true);
    end else begin
      bucket[i][j].value:=value;
    end;
  end;

PROCEDURE G_stringKeyMap.dropKey(CONST key: string);
  VAR i,j:longint;
  begin
    i:=hashOfAnsiString(key) and bitMask;
    j:=0;
    while (j<length(bucket[i])) and (bucket[i][j].key<>key) do inc(j);
    if j<length(bucket[i]) then begin
      while j<length(bucket[i])-1 do begin
        bucket[i][j]:=bucket[i][j+1];
        inc(j);
      end;
      setLength(bucket[i],length(bucket[i])-1);
      dec(entryCount);
      if entryCount<0.4*length(bucket)*rebalanceFac then rehash(false);
    end;
  end;

PROCEDURE G_stringKeyMap.clear;
  VAR i:longint;
  begin
    for i:=0 to length(bucket)-1 do setLength(bucket[i],0);
    setLength(bucket,1);
    bitMask:=0;
    entryCount:=0;
  end;

FUNCTION G_stringKeyMap.keySet: T_arrayOfString;
  VAR k,i,j:longint;
  begin
    setLength(result,entryCount);
    k:=0;
    for i:=0 to length(bucket)-1 do
    for j:=0 to length(bucket[i])-1 do begin
      result[k]:=bucket[i][j].key;
      inc(k);
    end;
  end;

FUNCTION G_stringKeyMap.valueSet: VALUE_TYPE_ARRAY;
  VAR k,i,j:longint;
  begin
    setLength(result,entryCount);
    k:=0;
    for i:=0 to length(bucket)-1 do
    for j:=0 to length(bucket[i])-1 do begin
      result[k]:=bucket[i][j].value;
      inc(k);
    end;
  end;

FUNCTION G_stringKeyMap.size: longint;
  begin
    result:=entryCount;
  end;

CONSTRUCTOR T_setOfPointers.create;
  begin
    setLength(dat,1);
    setLength(dat[0],0);
    hashMask:=0;
  end;

DESTRUCTOR T_setOfPointers.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(dat)-1 do setLength(dat[i],0);
    setLength(dat,0);
    hashMask:=-1;
  end;

FUNCTION T_setOfPointers.contains(CONST p:pointer):boolean;
  VAR i,j:longint;
  begin
    i:=qword(p) and hashMask;
    for j:=0 to length(dat[i])-1 do if dat[i,j]=p then exit(true);
    result:=false;
  end;

PROCEDURE T_setOfPointers.put(CONST p:pointer);
  VAR h,i,j,k,l,oldLen:longint;
  begin
    i:=qword(p) and hashMask;
    for j:=0 to length(dat[i])-1 do if dat[i,j]=p then exit;
    j:=length(dat[i]);
    setLength(dat[i],j+1);
    dat[i,j]:=p;
    if (length(dat[i])>32) and (length(dat)<200000) then begin
      oldLen:=length(dat);
      setLength(dat,oldLen+oldLen);
      hashMask:=oldLen+oldLen-1;
      for i:=0 to oldLen-1 do begin
        k:=0;
        for j:=0 to length(dat[i])-1 do begin
          h:=longint(dat[i,j]) and hashMask;
          if h=i then begin
            dat[i,k]:=dat[i,j];
            inc(k);
          end else begin
            l:=length(dat[h]);
            setLength(dat[h],l+1);
            dat[h,l]:=dat[i,j];
          end;
        end;
        setLength(dat[i],k);
      end;
    end;
  end;

end.

