UNIT dbTags;

{$mode objfpc}{$H+}

INTERFACE

USES
  sysutils,myFiles,myGenerics,Classes;

FUNCTION tagIndex(tagString:string; allowCreation:boolean):longint;
PROCEDURE getTags(VAR indexes:T_indexSet; strings:TStrings);
FUNCTION getTagsForList(VAR indexes:T_indexSet):ansistring;
PROCEDURE getTagsForDropDown(strings:TStrings);
FUNCTION loadTagsFromFile(VAR f:T_file):boolean;
PROCEDURE saveTagsToFile(VAR f:T_file);
IMPLEMENTATION
VAR tagList:array of ansistring;

FUNCTION tagIndex(tagString: string; allowCreation: boolean): longint;
begin
  result:=0;
  while (result<length(tagList)) and (tagList[result]<>tagString) do inc(result);
  if result>=length(tagList) then begin
    if allowCreation then begin
      setLength(tagList,result+1);
      tagList[result]:=tagString;
    end else result:=-1;
  end;
end;

PROCEDURE getTags(VAR indexes: T_indexSet; strings: TStrings);
VAR i,imax:longint;
begin
  imax:=indexes.maxEntryIndex;
  if length(tagList)<=imax then imax:=length(tagList)-1;
  strings.clear;
  for i:=0 to imax do if indexes[i] then strings.add(tagList[i]);
end;

FUNCTION getTagsForList(VAR indexes: T_indexSet): ansistring;
VAR i,imax:longint;
begin
  imax:=indexes.maxEntryIndex;
  if length(tagList)<=imax then imax:=length(tagList)-1;
  result:='';
  for i:=0 to imax do if indexes[i] then result:=result+BoolToStr(result='','',', ')+tagList[i];
end;

PROCEDURE getTagsForDropDown(strings: TStrings);
VAR i:longint;
begin
  strings.clear;
  for i:=0 to length(tagList)-1 do strings.append('#'+tagList[i]);
end;

FUNCTION loadTagsFromFile(VAR f: T_file): boolean;
VAR i,len:longint;
begin
  len:=f.readLongint;
  if len<0 then result:=false
  else begin
    result:=true;
    setLength(tagList,len);
    for i:=0 to len-1 do tagList[i]:=f.readAnsiString;
  end;
end;

PROCEDURE saveTagsToFile(VAR f: T_file);
VAR i:longint;
begin
  f.writeLongint(length(tagList));
  for i:=0 to length(tagList)-1 do f.writeAnsiString(tagList[i]);
end;

end.

