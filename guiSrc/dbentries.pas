UNIT dbEntries;

{$mode objfpc}{$H+}

INTERFACE

USES
  myFiles,dbFiles,myGenerics,Graphics,dbTags,Classes,sysutils,queues;
CONST
  C_lineBreakChar=#13;
  catalogueFileName='catalogue.dat';
TYPE
  T_sortCriterion=(sc_none,
                   sc_filename_asc,
                   sc_filename_desc,
                   sc_name_asc,
                   sc_name_desc,
                   sc_tags_asc,
                   sc_tags_desc,
                   sc_firstChange_asc,
                   sc_firstChange_desc,
                   sc_lastChange_asc,
                   sc_lastChange_desc,
                   sc_resolution_asc,
                   sc_resolution_desc);

  P_dbEntry=^T_dbEntry;

  { T_dbEntry }

  T_dbEntry=object(T_serializable)
    givenName:string;
    comment  :ansistring;
    files    :array of P_fileInfo;
    tags     :T_indexSet;


    //for GUI-interaction:
    marked:boolean; //non-persistent
    //automatic by files:
    minAge,maxAge:double; //non-persistent
    markedForDeletion:boolean; //non-persistent
    markedForMerge:boolean; //non-persistent

    CONSTRUCTOR create(info:P_FileInfo);
    CONSTRUCTOR createAndLoad(VAR f:T_file);
    DESTRUCTOR destroy;
    PROCEDURE updateAutomaticFields;
    FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual; overload;
    PROCEDURE saveToFile(VAR F:T_file);           virtual; overload;
    PROCEDURE addTag(tagString:ansistring);
    PROCEDURE removeTag(tagString:ansistring);
    PROCEDURE mergeWith(VAR otherEntry:T_dbEntry);
    FUNCTION dropFile(fileIndex:longint):P_fileInfo;
    PROCEDURE dropDeleteFile(fileIndex:longint);
    PROCEDURE swapFiles(i0,i1:longint);
    //FUNCTION  newFileMatches(fileInfo:T_fileInfo):boolean;
    PROCEDURE getFileList(s:TStrings);
    PROCEDURE addNewFileWithoutTouchingTags(fileInfo:P_fileInfo);
    PROCEDURE addNewFile(fileInfo:P_fileInfo);
    PROCEDURE dropNonexistentFiles;
    PROCEDURE dropThumbnails;
    FUNCTION containsFileLike(s:string):boolean;
    PROCEDURE showPrimary();
    FUNCTION fileName:string;
    FUNCTION moveToDirectory(newDirectory:string):boolean;
    FUNCTION thumbState:T_thumbnailState;
    FUNCTION getThumb:TPicture;
    FUNCTION resolution:longint;
    FUNCTION resolutionString:string;
  end;

PROCEDURE clearFilter;
PROCEDURE reapplyFilter;
PROCEDURE sort(criterion: T_sortCriterion; externalCall:boolean);
PROCEDURE filter(filter:ansistring);
PROCEDURE addNewEntryFromDroppedFile(fileInfo: P_fileInfo);
PROCEDURE addNewEntryFromNewFile(fileInfo: P_fileInfo);
PROCEDURE performMerge;
PROCEDURE performDelete;
PROCEDURE dropNonexistentFiles;
PROCEDURE scanForNewFiles;
PROCEDURE dropEmptyEntries;
PROCEDURE storeDB;
VAR filteredEntries:array of P_dbEntry;
IMPLEMENTATION
VAR allEntries:array of P_dbEntry;
    lastFilter:string='';
    lastSortCriterion:T_sortCriterion=sc_none;
    entriesByMatchName:specialize G_stringKeyMap<P_dbEntry>;

PROCEDURE ensureEntriesByMatchName;
  VAR i,j:longint;
  begin
    if entriesByMatchName.size=0 then begin
      for i:=0 to length(allEntries)-1 do if (allEntries[i]<>nil) and not(allEntries[i]^.markedForDeletion) then
        for j:=0 to length(allEntries[i]^.files)-1 do
          entriesByMatchName.put(allEntries[i]^.files[j]^.getMatchName,allEntries[i]);
    end;
  end;

{ T_dbEntry }

CONSTRUCTOR T_dbEntry.create(info: P_FileInfo);
  begin
    givenName:=info^.getGroupName;
    comment  :='';
    setLength(files,0);
    tags     .create;
    setLength(files,1);
    files[0]:=info;
    updateAutomaticFields;
    entriesByMatchName.put(info^.getMatchName,@self);
  end;

CONSTRUCTOR T_dbEntry.createAndLoad(VAR f: T_file);
  begin
    givenName:='';
    comment  :='';
    setLength(files,0);
    tags     .create;
    loadFromFile(f);
  end;

DESTRUCTOR T_dbEntry.destroy;
  VAR i:longint;
  begin
    tags.destroy;
    for i:=0 to length(files)-1 do dispose(files[i],destroy);
    setLength(files,0);
    entriesByMatchName.clear;
  end;

PROCEDURE T_dbEntry.updateAutomaticFields;
  VAR firstFile:boolean;
      i:longint;
      newAge:double;
  begin
    firstFile:=true;
    for i:=0 to length(files)-1 do if (firstFile) then begin
      minAge:=files[i]^.getAge;
      maxAge:=minAge;
      firstFile:=false;
    end else begin
      newAge:=files[i]^.getAge;
      if newAge<minAge then minAge:=newAge;
      if newAge>maxAge then maxAge:=newAge;
    end;
    if firstFile then begin
      minAge:=0;
      maxAge:=0;
    end;
  end;

FUNCTION T_dbEntry.loadFromFile(VAR F: T_file): boolean;
  VAR i,fileCount:longint;
      fileInfo:P_fileInfo;
  begin
    fileCount:=f.readLongint;
    result:=(fileCount>=0) and tags.loadFromFile(f);
    if result then begin
      for i:=0 to fileCount-1 do begin
        new(fileInfo,create(''));
        fileInfo^.loadFromFile(f);
        addNewFileWithoutTouchingTags(fileInfo);
      end;
      givenName:=f.readAnsiString;
      comment:=f.readAnsiString;
    end;
  end;

PROCEDURE T_dbEntry.saveToFile(VAR F: T_file);
  VAR i,j:longint;
  begin
    //remove duplicate files:
    for i:=1 to length(files)-1 do begin
      j:=0;
      while (j<i) and ((files[j]=nil) or (files[j]^.getNameAsString<>files[i]^.getNameAsString)) do inc(j);
      if j<i then begin
        writeln('dropping file with duplicate "',files[i]^.getNameAsString,'"');
        dispose(files[i],destroy);
        files[i]:=nil;
      end;
    end;
    j:=0;
    for i:=0 to length(files)-1 do if files[i]<>nil then begin
      files[j]:=files[i];
      inc(j);
    end;
    f.writeLongint(j);
    tags.saveToFile(f);
    for i:=0 to j-1 do files[i]^.saveToFile(f);
    if j<>length(files) then setLength(files,j);
    f.writeAnsiString(givenName);
    f.writeAnsiString(comment);
  end;

PROCEDURE T_dbEntry.addTag(tagString: ansistring);
  begin
    while (length(tagString)>1) and (tagString[1]='#') do tagString:=copy(tagString,2,length(tagString)-1);
    if length(tagString)>0 then tags[tagIndex(tagString,true)]:=true;
  end;

PROCEDURE T_dbEntry.removeTag(tagString: ansistring);
  begin
    tags[tagIndex(tagString,false)]:=false;
  end;

PROCEDURE T_dbEntry.mergeWith(VAR otherEntry: T_dbEntry);
  VAR i,i0:longint;
  begin
    comment:=comment+C_lineBreakChar+otherEntry.comment;

    i0:=length(files);
    setLength(files,i0+length(otherEntry.files));
    for i:=0 to length(otherEntry.files)-1 do
      files[i+i0]:=otherEntry.files[i];
    setLength(otherEntry.files,0);

    for i:=0 to otherEntry.tags.maxEntryIndex do
      tags[i]:=tags[i] or otherEntry.tags[i];
  end;

FUNCTION T_dbEntry.dropFile(fileIndex: longint): P_fileInfo;
  VAR i:longint;
  begin
    if (fileIndex>=0) and (fileIndex<length(files)) then begin
      if (length(files)=1) then markedForDeletion:=true;
      result:=files[fileIndex];
      for i:=fileIndex to length(files)-2 do files[i]:=files[i+1];
      setLength(files,length(files)-1);
      updateAutomaticFields;
      entriesByMatchName.clear;
      if result<>nil then result^.dropThumbnail;
    end;
  end;

PROCEDURE T_dbEntry.dropDeleteFile(fileIndex: longint);
  VAR i:longint;
  begin
    if (fileIndex>=0) and (fileIndex<length(files)) then begin
      if (length(files)=1) then markedForDeletion:=true;
      files[fileIndex]^.delete;
      dispose(files[fileIndex],destroy);
      for i:=fileIndex to length(files)-2 do files[i]:=files[i+1];
      setLength(files,length(files)-1);
      updateAutomaticFields;
      entriesByMatchName.clear;
    end;
  end;

PROCEDURE T_dbEntry.swapFiles(i0, i1: longint);
  VAR temp:P_fileInfo;
  begin
    if (i0>=0) and (i0<length(files)) and (i1>=0) and (i1<length(files)) then begin
      temp:=files[i0];
      files[i0]:=files[i1];
      files[i1]:=temp;
    end;
  end;

//FUNCTION T_dbEntry.newFileMatches(fileInfo: T_fileInfo): boolean;
//  VAR i:longint;
//  begin
//    for i:=0 to length(files)-1 do
//      if files[i]^.matches(fileInfo) then exit(true);
//    result:=false;
//  end;

PROCEDURE T_dbEntry.getFileList(s: TStrings);
  VAR i:longint;
      desc:string;
  begin
    while s.count>length(files) do s.Delete(s.count-1);
    for i:=0 to length(files)-1 do begin
      desc:=files[i]^.getNameAsString+files[i]^.getInfoString;
      if i>=s.count then s.append(desc)
                    else s[i]:=   desc;
    end;
  end;

PROCEDURE T_dbEntry.addNewFileWithoutTouchingTags(fileInfo: P_fileInfo);
  begin
    ensureEntriesByMatchName;
    if fileInfo^.isExistent then begin
      entriesByMatchName.put(fileInfo^.getMatchName,@self);
      setLength(files,length(files)+1);
      files[length(files)-1]:=fileInfo;
      updateAutomaticFields;
    end else dispose(fileInfo,destroy);
  end;

PROCEDURE T_dbEntry.addNewFile(fileInfo: P_fileInfo);
  VAR ext:string;
      i:longint;
  begin
    ensureEntriesByMatchName;
    if fileInfo^.isExistent then begin
      entriesByMatchName.put(fileInfo^.getMatchName,@self);
      addNewFileWithoutTouchingTags(fileInfo);
      ext:=fileInfo^.getNormalizedExtension;
      for i:=0 to length(C_extMap)-1 do if C_extMap[i].ext=ext then
        addTag(C_extMap[i].tag);
    end else dispose(fileInfo,destroy);
  end;

//procedure T_dbEntry.dropThumb;
//  begin
//    if FileExists(getThumbName) then DeleteFile(getThumbName);
//    if thumb.loaded then begin
//      thumb.Picture.Clear;
//      thumb.loaded:=false;
//    end;
//  end;
//
//procedure T_dbEntry.loadThumb;
//  begin
//    if FileExists(getThumbName) then try
//      if thumb.picture=nil then thumb.picture:=TPicture.Create;
//      thumb.Picture.LoadFromFile(getThumbName);
//      thumb.loaded:=true;
//    except
//      thumb.Picture.Clear;
//      thumb.loaded:=false;
//    end;
//  end;

PROCEDURE T_dbEntry.dropNonexistentFiles;
  VAR i,j:longint;
  begin
    j:=0;
    for i:=0 to length(files)-1 do
      if files[i]^.isExistent then begin
        if i<>j then files[j]:=files[i];
        inc(j);
      end else begin
        dispose(files[i],destroy);
      end;
    setLength(files,j);
    updateAutomaticFields;
  end;

PROCEDURE T_dbEntry.dropThumbnails;
  VAR i:longint;
  begin
    for i:=0 to length(files)-1 do files[i]^.dropThumbnail;
    thumbState;
  end;

FUNCTION T_dbEntry.containsFileLike(s: string): boolean;
  VAR i:longint;
  begin
    result:=false;
    s:=uppercase(s);
    for i:=0 to length(files)-1 do result:=result or (pos(s,uppercase(files[i]^.getNameAsString))>0);
  end;

PROCEDURE T_dbEntry.showPrimary;
  VAR i:longint;
  begin
    i:=0;
    while (i<length(files)) and not(files[i]^.isImage) do inc(i);
    if i<length(files) then files[i]^.showFile();
  end;

FUNCTION T_dbEntry.fileName: string;
  begin
    if length(files)>0 then result:=files[0]^.getPath
                       else result:='';
  end;

FUNCTION T_dbEntry.moveToDirectory(newDirectory: string): boolean;
  VAR i:longint;
      newPath:array of T_structuredPath;
  begin
    newDirectory:=extractRelativePath(extractFilePath(paramStr(0)),newDirectory);
    if newDirectory[length(newDirectory)]<>DirectorySeparator then newDirectory:=newDirectory+DirectorySeparator;
    setLength(newPath,length(files));
    result:=true;
    for i:=0 to length(files)-1 do begin
      newPath[i]:=newDirectory+files[i]^.getPath.fileName;
      result:=result and files[i]^.canMoveTo(newPath[i]);
    end;
    if result then for i:=0 to length(files)-1 do files[i]^.moveTo(newPath[i]);
    setLength(newPath,0);
  end;

FUNCTION T_dbEntry.thumbState: T_thumbnailState;
  VAR i:longint;
  begin
    i:=0;
    while (i<length(files)) and not(files[i]^.isImage) do inc(i);
    if (i<length(files)) then result:=files[i]^.getThumbState
                         else result:=ts_unknown;
  end;

FUNCTION T_dbEntry.getThumb: TPicture;
  VAR i:longint;
  begin
    i:=0;
    while (i<length(files)) and not(files[i]^.isImage) do inc(i);
    if (i>=Length(files)) then exit(nil);
    result:=files[i]^.getThumb;
  end;

FUNCTION T_dbEntry.resolution: longint;
  VAR i,rx,ry:longint;
  begin
    result:=-1;
    for i:=0 to length(files)-1 do begin
      files[i]^.getResolution(rx,ry);
      if rx*ry>result then result:=rx*ry;
    end;
  end;

FUNCTION T_dbEntry.resolutionString: string;
  VAR i,rx,ry,r:longint;
  begin
    r:=-1; result:='';
    for i:=0 to length(files)-1 do begin
      files[i]^.getResolution(rx,ry);
      if rx*ry>r then begin
        r:=rx*ry;
        result:=intToStr(rx)+'x'+intToStr(ry)+' ('+formatFloat('#0.00',rx*1E-6*ry)+'MP)';
      end;
    end;
  end;

PROCEDURE clearFilter;
  VAR i,j:longint;
  begin
    setLength(filteredEntries,length(allEntries));
    j:=0;
    for i:=0 to length(allEntries)-1 do if allEntries[i]^.markedForDeletion then begin
      dispose(allEntries[i],destroy);
    end else begin
      if i<>j then allEntries[j]:=allEntries[i];
      filteredEntries[j]:=allEntries[j];
      inc(j);
    end;
    setLength(allEntries,j);
    setLength(filteredEntries,j);
    sort(lastSortCriterion,false);
  end;

PROCEDURE filter(filter: ansistring);
  VAR i,j:longint;
      tagToFilter:longint;
  begin
    if (length(filter)>1) and (filter[1]='#') then begin
      tagToFilter:=tagIndex(copy(filter,2,length(filter)-1),false);
      filter:='';
    end else begin
      tagToFilter:=-1;
      filter:=uppercase(filter);
    end;
    j:=0;
    for i:=0 to length(filteredEntries)-1 do
      if not(filteredEntries[i]^.markedForDeletion) and
         ((tagToFilter=-1) or (filteredEntries[i]^.tags[tagToFilter])) and
         ((filter='') or (pos(filter,uppercase(filteredEntries[i]^.givenName))>0)
                      or filteredEntries[i]^.containsFileLike(filter)
                      or (pos(filter,uppercase(filteredEntries[i]^.comment))>0)) then begin
      filteredEntries[j]:=filteredEntries[i];
      inc(j);
    end;
    setLength(filteredEntries,j);
  end;

PROCEDURE addNewEntryFromDroppedFile(fileInfo: P_fileInfo);
  begin
    fileInfo^.dropThumbnail;
    setLength(allEntries,length(allEntries)+1);
    allEntries[length(allEntries)-1]:=nil;
    new(allEntries[length(allEntries)-1],create(fileInfo));
  end;

PROCEDURE addNewEntryFromNewFile(fileInfo: P_fileInfo);
  VAR matchingEntry:P_dbEntry;
  begin
    ensureEntriesByMatchName;
    if entriesByMatchName.containsKey(fileInfo^.getMatchName,matchingEntry)
      then matchingEntry^.addNewFile(fileInfo)
      else addNewEntryFromDroppedFile(fileInfo);
  end;

PROCEDURE performMerge;
  VAR i,i0:longint;
  begin
    i0:=-1;
    for i:=0 to length(allEntries)-1 do if allEntries[i]^.markedForMerge then begin
      if i0=-1 then i0:=i else begin
        allEntries[i0]^.mergeWith(allEntries[i]^);
        allEntries[i]^.markedForDeletion:=true;
      end;
      allEntries[i]^.markedForMerge:=false;
    end;
    filter(lastFilter);
    if lastSortCriterion<>sc_none then sort(lastSortCriterion,false);
  end;

PROCEDURE performDelete;
  VAR i,j:longint;
  begin
    j:=0;
    for i:=0 to length(allEntries)-1 do if allEntries[i]^.markedForDeletion then begin
      dispose(allEntries[i],destroy);
    end else begin
      if i<>j then allEntries[j]:=allEntries[i];
      inc(j);
    end;
    setLength(allEntries,j);
    reapplyFilter;
  end;

PROCEDURE dropNonexistentFiles;
  VAR i:longint;
  begin
    for i:=0 to length(allEntries)-1 do allEntries[i]^.dropNonexistentFiles;
    performDelete;
    reapplyFilter;
  end;

PROCEDURE scanForNewFiles;
  VAR i:longint;
      found:T_fileInfoList;
      anyChange:boolean;
  begin
    //if entriesByGroupName.;

    found:=dbFiles.scanForNewFiles(anyChange);
    if anyChange then begin
      dropNonexistentFiles;
      for i:=0 to length(found)-1 do addNewEntryFromNewFile(found[i]);
    end;
    reapplyFilter;
  end;

PROCEDURE dropEmptyEntries;
  VAR i:longint;
  begin
    for i:=0 to length(allEntries)-1 do if length(allEntries[i]^.files)=0 then allEntries[i]^.markedForDeletion:=true;
    reapplyFilter;
  end;

PROCEDURE reapplyFilter;
  begin
    filter(lastFilter);
  end;

PROCEDURE sort(criterion: T_sortCriterion; externalCall:boolean);
  FUNCTION leq(x,y:P_dbEntry):boolean;
    begin
      case criterion of
        sc_filename_asc : result:=x^.fileName<=y^.fileName;
        sc_filename_desc: result:=x^.fileName>=y^.fileName;
        sc_name_asc :         result:=uppercase(x^.givenName)<=uppercase(y^.givenName);
        sc_name_desc:         result:=uppercase(x^.givenName)>=uppercase(y^.givenName);
        sc_tags_asc:          result:=not(y^.tags.lesser(x^.tags));
        sc_tags_desc:         result:=not(x^.tags.lesser(y^.tags));
        sc_firstChange_asc:   result:=x^.MinAge<=y^.MinAge;
        sc_firstChange_desc:  result:=x^.MinAge>=y^.MinAge;
        sc_lastChange_asc:    result:=x^.MaxAge<=y^.MaxAge;
        sc_lastChange_desc:   result:=x^.MaxAge>=y^.MaxAge;
        sc_resolution_asc:    result:=x^.resolution>=y^.resolution;
        sc_resolution_desc:   result:=x^.resolution<=y^.resolution;
      end;
    end;

  VAR scale    :longint;
      i,j0,j1,k:longint;
      temp     :array of P_dbEntry;
  begin
    if externalCall and (criterion=lastSortCriterion) then case criterion of
      sc_filename_asc : criterion:=sc_filename_desc;
      sc_filename_desc: criterion:=sc_filename_asc;
      sc_name_asc :         criterion:=sc_name_desc;
      sc_name_desc:         criterion:=sc_name_asc;
      sc_tags_asc:          criterion:=sc_tags_desc;
      sc_tags_desc:         criterion:=sc_tags_asc;
      sc_firstChange_asc:   criterion:=sc_firstChange_desc;
      sc_firstChange_desc:  criterion:=sc_firstChange_asc;
      sc_lastChange_asc:    criterion:=sc_lastChange_desc;
      sc_lastChange_desc:   criterion:=sc_lastChange_asc;
      sc_resolution_asc:    criterion:=sc_resolution_desc;
      sc_resolution_desc:   criterion:=sc_resolution_asc;
    end;
    lastSortCriterion:=criterion;

    scale:=1;
    setLength(temp,length(filteredEntries));
    while scale<length(filteredEntries) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<length(filteredEntries) do begin
        j0:=i;
        j1:=i+scale;
        k :=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(filteredEntries)) do begin
          if leq(filteredEntries[j0],filteredEntries[j1])
            then begin temp[k]:=filteredEntries[j0]; inc(k); inc(j0); end
            else begin temp[k]:=filteredEntries[j1]; inc(k); inc(j1); end;
        end;
        while (j0<i+scale)       and (j0<length(filteredEntries)) do begin temp[k]:=filteredEntries[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(filteredEntries)) do begin temp[k]:=filteredEntries[j1]; inc(k); inc(j1); end;
        inc(i,scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale,scale);
      if (scale<length(filteredEntries)) then begin
        //The following is equivalent to the above with swapped roles of "filteredEntries" and "temp".
        //While making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while i<length(filteredEntries) do begin
          j0:=i;
          j1:=i+scale;
          k :=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(filteredEntries)) do begin
            if leq(temp[j0],temp[j1])
              then begin filteredEntries[k]:=temp[j0]; inc(k); inc(j0); end
              else begin filteredEntries[k]:=temp[j1]; inc(k); inc(j1); end;
          end;
          while (j0<i+scale)       and (j0<length(filteredEntries)) do begin filteredEntries[k]:=temp[j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(filteredEntries)) do begin filteredEntries[k]:=temp[j1]; inc(k); inc(j1); end;
          inc(i,scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale,scale);
      end else for k:=0 to length(filteredEntries)-1 do filteredEntries[k]:=temp[k];
    end;
    setLength(temp,0);
  end;

PROCEDURE storeDB;
  VAR f:T_file;
      i:longint;
  begin
    f.createToWrite(catalogueFileName);
    saveTagsToFile(f);
    f.writeLongint(length(allEntries));
    for i:=0 to length(allEntries)-1 do allEntries[i]^.saveToFile(f);
    f.destroy;
  end;

VAR f:T_file;
    i,cnt:longint;
INITIALIZATION
  entriesByMatchName.create;
  setLength(allEntries,0);
  if fileExists(catalogueFileName) then begin
    f.createToRead(catalogueFileName);
    loadTagsFromFile(f);
    cnt:=f.readLongint;
    for i:=0 to cnt-1 do begin
      setLength(allEntries,i+1);
      allEntries[i]:=nil;
      new(allEntries[i],createAndLoad(f));
    end;
    f.destroy;
  end;
  scanForNewFiles;
  clearFilter;

FINALIZATION
  pendingTasks.dropPending;
  while numberOfBusyThreads>0 do sleep(1);
  queues.evaluationFunction:=nil;
  queues.disposeFunction:=nil;

  performDelete;
  dropNonexistentFiles;
  storeDB;
  for i:=0 to length(allEntries)-1 do begin
    dispose(allEntries[i],destroy);
  end;
  entriesByMatchName.destroy;

end.

