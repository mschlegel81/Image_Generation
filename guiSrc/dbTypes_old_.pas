UNIT dbTypes;
INTERFACE
USES myFiles,FileUtil,sysutils,mypics,myGenerics,dispatcher,classes,Graphics,dbFiles,dbEntries;


//TYPE



  { T_fileInfo }



  { T_listOfEntries }

  //T_listOfEntries=object(T_serializable)
  //  private
  //    list:array of P_dbEntry;
  //    FUNCTION getEntry(index:longint):P_dbEntry;
  //  public
  //    PROPERTY entry[index:longint]:P_dbEntry read getEntry; default;
  //    CONSTRUCTOR create;
  //    PROCEDURE disposeAll;
  //    DESTRUCTOR destroy;
  //    FUNCTION  loadFromFile(VAR F:T_File):boolean; virtual; overload;
  //    PROCEDURE saveToFile(VAR F:T_File);           virtual; overload;
  //    PROCEDURE fillFrom(VAR otherList:T_listOfEntries);
  //    PROCEDURE filterFrom(VAR otherList:T_listOfEntries; filter:ansistring);
  //    PROCEDURE refilter(filter:ansistring; VAR tagList:T_tagList);
  //    PROCEDURE sort(criterion:T_sortCriterion);
  //    FUNCTION  size:longint;
  //    PROCEDURE addNewEntry(fileInfo:P_fileInfo; VAR tagList:T_tagList);
  //    PROCEDURE dropDispose(index:longint);
  //end;

  { T_fileDB }
  //P_fileDB=^T_fileDB;
  //T_fileDB=object(T_serializable)
  //
  //  entry:T_listOfEntries;
  //  tagList:T_tagList;
  //  lastRescan:double;
  //  filteredEntry:T_listOfEntries;
  //
  //  currentSortCriterion:T_sortCriterion;
  //  filteredEntryEqualsEntry:boolean;
  //
  //  CONSTRUCTOR create;
  //  DESTRUCTOR destroy;
  //  PROCEDURE saveToFile;
  //  FUNCTION  loadFromFile(VAR F:T_File):boolean; virtual; overload;
  //  PROCEDURE saveToFile(VAR F:T_File);           virtual; overload;
  //  FUNCTION rescan:boolean;
  //  PROCEDURE 3mbnails(VAR dispatcher:T_dispatcher);
  //  PROCEDURE cleanupThumbnails;
  //  PROCEDURE sort(criterion:T_sortCriterion);
  //  PROCEDURE filter(by:string);
  //  PROCEDURE filterByFile(name:String);
  //  PROCEDURE clearFilter;
  //  PROCEDURE performMerge;
  //  PROCEDURE addWithoutMerging(f:P_fileInfo);
  //end;


IMPLEMENTATION

{ T_listOfEntries }

//function T_listOfEntries.getEntry(index: longint): P_dbEntry;
//  begin
//    if (index>=0) and (index<length(list))
//      then result:=list[index]
//      else result:=nil;
//  end;
//
//constructor T_listOfEntries.create;
//  begin
//    setLength(list,0);
//  end;
//
//PROCEDURE T_listOfEntries.disposeAll;
//  VAR i:longint;
//  begin
//    for i:=0 to length(list)-1 do dispose(list[i],destroy);
//    setLength(list,0);
//  end;
//
//destructor T_listOfEntries.destroy;
//  begin
//    setLength(list,0);
//  end;
//
//function T_listOfEntries.loadFromFile(var F: T_File): boolean;
//  VAR i,c:longint;
//  begin
//    c:=f.readLongint;
//    if c<0 then exit(false);
//    setLength(list,c);
//    for i:=0 to length(list)-1 do begin
//      new(list[i],createToLoad);
//      list[i]^.loadFromFile(f);
//    end;
//    result:=true;
//  end;
//
//procedure T_listOfEntries.saveToFile(var F: T_File);
//  VAR i:longint;
//  begin
//    f.writeLongint(length(list));
//    for i:=0 to length(list)-1 do list[i]^.saveToFile(f);
//  end;
//
//procedure T_listOfEntries.fillFrom(var otherList: T_listOfEntries);
//  VAR i,j:longint;
//  begin
//    setLength(list,length(otherList.list));
//    j:=0;
//    for i:=0 to length(list)-1 do begin
//      if not(otherList.list[i]^.markedForDeletion) then begin
//        list[j]:=otherList.list[i];
//        inc(j);
//      end;
//    end;
//  end;


//PROCEDURE T_listOfEntries.addNewEntry(fileInfo: P_fileInfo; VAR tagList: T_tagList);
//  begin
//    setLength(list,length(list)+1);
//    new(list[length(list)-1],create(fileInfo,tagList));
//  end;
//
//procedure T_listOfEntries.dropDispose(index: longint);
//  VAR i:longint;
//  begin
//    if (index>=0) and (index<length(list)) then begin
//      dispose(list[index],destroy);
//      for i:=index to length(list)-2 do list[i]:=list[i+1];
//      setLength(list,length(list)-1);
//    end;
//  end;

{ T_fileDB }

//constructor T_fileDB.create;
//  begin
//    lastRescan:=0;
//    tagList.create;
//    entry.create;
//    filteredEntry.create;
//    loadFromFile('catalogue.dat');
//  end;
//
//destructor T_fileDB.destroy;
//  begin
//    tagList.destroy;
//    filteredEntry.destroy;
//    entry.disposeAll;
//    entry.destroy;
//  end;
//
//procedure T_fileDB.saveToFile;
//  begin
//    saveToFile('catalogue.dat');
//  end;
//
//function T_fileDB.loadFromFile(var F: T_File): boolean;
//  begin
//    result:=tagList.loadFromFile(f) and entry.loadFromFile(f);
//    filteredEntry.fillFrom(entry);
//    filteredEntryEqualsEntry:=true;
//  end;
//
//procedure T_fileDB.saveToFile(var F: T_File);
//  VAR i:longint;
//  begin
//    i:=0;
//    while i<entry.size do if entry[i]^.markedForDeletion then entry.dropDispose(i) else inc(i);
//    tagList.saveToFile(f);
//    entry.saveToFile(f);
//  end;
//
//FUNCTION T_fileDB.rescan:boolean;
//  VAR found:T_fileInfoList;
//      filenames:TStrings;
//      i,j:longint;
//      changesFound:boolean;
//  begin
//    lastRescan:=now;
//    found:=scanForNewFiles(result);
//    if result then begin
//      for i:=0 to entry.size-1 do entry[i]^.dropNonexistentFiles;
//      for i:=0 to length(found)-1 do begin
//        j:=0;
//        while (j<entry.size) and not(entry[j]^.newFileMatches(found[i]^)) do inc(j);
//        if j<entry.size
//          then entry[j]^.addNewFile(found[i],tagList)
//          else begin
//            entry.addNewEntry(found[i],tagList);
//            result:=true;
//          end;
//      end;
//      filteredEntry.fillFrom(entry);
//      filteredEntryEqualsEntry:=true;
//    end;
//  end;
//
//PROCEDURE T_fileDB.generateThumbnails(VAR dispatcher:T_dispatcher);
//  VAR i:longint;
//  begin
//    for i:=0 to entry.size-1 do entry[i]^.generateThumbnail(dispatcher);
//  end;
//
//PROCEDURE T_fileDB.cleanupThumbnails;
//  VAR inDB :T_listOfString;
//      filenames:TStrings;
//      i:longint;
//  begin
//    inDB.create;
//    for i:=0 to entry.size-1 do inDB.add(entry[i]^.getThumbName);
//    inDB.unique;
//    filenames:=FindAllFiles(C_parentPath[pp_thumbnails]);
//    for i:=0 to filenames.Count-1 do if not(inDB.contains(filenames[i])) then DeleteFile(filenames[i]);
//    inDB.destroy;
//  end;
//
//procedure T_fileDB.sort(criterion: T_sortCriterion);
//  begin
//    if criterion=currentSortCriterion then case currentSortCriterion of
//      sc_commonPrefix_asc : criterion:=sc_commonPrefix_desc;
//      sc_commonPrefix_desc: criterion:=sc_commonPrefix_asc ;
//      sc_name_asc         : criterion:=sc_name_desc;
//      sc_name_desc        : criterion:=sc_name_asc;
//      sc_tags_asc         : criterion:=sc_tags_desc;
//      sc_tags_desc        : criterion:=sc_tags_asc;
//      sc_firstChange_asc  : criterion:=sc_firstChange_desc;
//      sc_firstChange_desc : criterion:=sc_firstChange_asc;
//      sc_lastChange_asc   : criterion:=sc_lastChange_desc;
//      sc_lastChange_desc  : criterion:=sc_lastChange_asc;
//    end;
//    currentSortCriterion:=criterion;
//    if filteredEntryEqualsEntry then begin
//      entry.sort(criterion);
//      filteredEntry.fillFrom(entry);
//    end else begin
//      filteredEntry.sort(criterion);
//    end;
//  end;
//
//procedure T_fileDB.filter(by: string);
//  begin
//    if filteredEntryEqualsEntry
//      then filteredEntry.filterFrom(entry,by,tagList)
//      else filteredEntry.refilter        (by,tagList);
//    filteredEntryEqualsEntry:=false;
//  end;
//
//procedure T_fileDB.filterByFile(name: String);
//  VAR i,j:longint;
//  begin
//    setLength(filteredEntry.list,entry.size);
//    j:=0;
//    for i:=0 to entry.size-1 do if entry[i]^.containsFileLike(name) then begin
//      filteredEntry.list[j]:=entry[i];
//      inc(j);
//    end;
//    setLength(filteredEntry.list,j);
//    filteredEntryEqualsEntry:=false;
//  end;
//
//procedure T_fileDB.clearFilter;
//  VAR i:longint;
//  begin
//    i:=0;
//    while i<entry.size do if entry[i]^.markedForDeletion then entry.dropDispose(i) else inc(i);
//    entry.sort(currentSortCriterion);
//    filteredEntry.fillFrom(entry);
//    filteredEntryEqualsEntry:=true;
//  end;
//
//procedure T_fileDB.performMerge;
//  VAR i,i0:longint;
//  begin
//    i0:=-1;
//    for i:=0 to entry.size-1 do if entry[i]^.markedForMerge then begin
//      if i0=-1 then i0:=i else begin
//        entry[i0]^.mergeWith(entry[i]^);
//        entry[i]^.markedForDeletion:=true;
//      end;
//      entry[i]^.markedForMerge:=false;
//    end;
//    filteredEntry.refilter('',tagList);
//  end;
//
//procedure T_fileDB.addWithoutMerging(f: P_fileInfo);
//  begin
//    setLength(entry.list,length(entry.list)+1);
//    new(entry.list[length(entry.list)-1],create(f,tagList));
//    filteredEntryEqualsEntry:=false;
//  end;
//
//
//{ T_dbEntry }
//
//CONSTRUCTOR T_dbEntry.createToLoad;
//  begin
//    setLength(files,0);
//    givenName:='';
//    comment:='';
//    tags.create;
//    marked:=false;
//
//    minAge:=0;
//    maxAge:=0;
//
//    markedForDeletion:=false;
//    markedForMerge:=false;
//
//    thumb.picture:=nil;
//    thumb.loaded:=false;
//  end;
//
//CONSTRUCTOR T_dbEntry.create(info: P_FileInfo; VAR taglist: T_tagList);
//  begin
//    createToLoad;
//    addNewFile(info,taglist);
//    id:=nextEntryId;
//    inc(nextEntryId);
//    givenName:=info^.getGroupName;
//  end;
//
//destructor T_dbEntry.destroy;
//  VAR i:longint;
//  begin
//    for i:=0 to length(files)-1 do dispose(files[i],destroy);
//    setLength(files,0);
//    tags.destroy;
//  end;
//
//procedure T_dbEntry.updateAutomaticFields;
//  VAR firstFile:boolean;
//      i:longint;
//      newAge:double;
//  begin
//    firstFile:=true;
//    for i:=0 to length(files)-1 do if (firstFile) then begin
//      minAge:=files[i]^.getAge;
//      maxAge:=minAge;
//      firstFile:=false;
//    end else begin
//      newAge:=files[i]^.getAge;
//      if newAge<minAge then minAge:=newAge;
//      if newAge>maxAge then maxAge:=newAge;
//    end;
//    if firstFile then begin
//      minAge:=0;
//      maxAge:=0;
//    end;
//  end;
//
//function T_dbEntry.loadFromFile(var F: T_File): boolean;
//  VAR i,fileCount:longint;
//      fileInfo:P_fileInfo;
//  begin
//    id:=f.readLongint;
//    if id>=nextEntryId then nextEntryId:=id+1;
//    fileCount:=f.readLongint;
//    result:=(fileCount>=0) and tags.loadFromFile(f);
//    if result then begin
//      for i:=0 to fileCount-1 do begin
//        new(fileInfo,create(''));
//        fileInfo^.loadFromFile(f);
//        addNewFileWithoutTouchingTags(fileInfo);
//      end;
//      givenName:=f.readAnsiString;
//      comment:=f.readAnsiString;
//      thumb.createdForFileName:=f.readAnsiString;
//      thumb.createdForFileTime:=f.readDouble;
//    end;
//  end;
//
//procedure T_dbEntry.saveToFile(var F: T_File);
//  VAR i:longint;
//  begin
//    f.writeLongint(id);
//    f.writeLongint(length(files));
//    tags.saveToFile(f);
//    for i:=0 to length(files)-1 do files[i]^.saveToFile(f);
//    f.writeAnsiString(givenName);
//    f.writeAnsiString(comment);
//    f.writeAnsiString(thumb.createdForFileName);
//    f.writeDouble(thumb.createdForFileTime);
//  end;
//
//procedure T_dbEntry.checkExistence;
//  VAR i,j:longint;
//  begin
//    i:=0;
//    while i<length(files) do begin
//      if files[i]^.isExistent then inc(i)
//      else begin
//        dispose(files[i],destroy);
//        for j:=i to length(files)-2 do files[j]:=files[j+1];
//        setLength(files,length(files)-1);
//      end;
//    end;
//  end;
//
//FUNCTION T_dbEntry.getThumbName:string;
//  begin
//    result:=C_parentPath[pp_thumbnails]+DirectorySeparator+FormatFloat('00000000',id)+'.jpg';
//  end;
//
//{procedure T_dbEntry.changeCommonPrefix(newCommonPrefix: string);
//  VAR i,len:longint;
//      renamePossible:boolean;
//      newname:string;
//  begin
//    len:=length(string(commonPart));
//    renamePossible:=true;
//    for i:=0 to length(input)-1 do begin
//      newname:=input[i].getPath+input[i].nameWithNewPrefix(len,newCommonPrefix);
//      renamePossible:=renamePossible and not(FileExists(newname));
//    end;
//    for i:=0 to length(images)-1 do begin
//      newname:=images[i].getPath+images[i].nameWithNewPrefix(len,newCommonPrefix);
//      renamePossible:=renamePossible and not(FileExists(newname));
//    end;
//    if renamePossible then begin
//      for i:=0 to length(input)-1 do input[i].rename(input[i].nameWithNewPrefix(len,newCommonPrefix));
//      for i:=0 to length(images)-1 do images[i].rename(images[i].nameWithNewPrefix(len,newCommonPrefix));
//      updateAutomaticFields;
//    end;
//  end;     }
//
//procedure T_dbEntry.generateThumbnail(var dispatcher: T_dispatcher);
//  VAR i:longint;
//  begin
//    i:=0;
//    while (i<length(files)) and not(files[i]^.isImage) do inc(i);
//    if i>=length(files) then begin
//      //no image exists
//      if FileExists(getThumbName) then DeleteFile(getThumbName);
//      exit;
//    end;
//
//    with thumb do
//      if not(FileExists(getThumbName))
//      or (createdForFileTime<>files[i]^.getAge)
//      or (createdForFileName<>files[i]^.getNameAsString) then begin
//      ensurePath(getThumbName);
//      if FileExists(getThumbName) then DeleteFile(getThumbName);
//      thumb.loaded:=false;
//      if thumb.picture<>nil then thumb.picture.Clear;
//      createdForFileName:=files[i]^.getNameAsString;
//      createdForFileTime:=files[i]^.getAge;
//      dispatcher.appendTask('im '+files[i]^.getNameAsString+' -cr'+intToStr(C_thumbnailMaxWidthHeight)+'x'+intToStr(C_thumbnailMaxWidthHeight)+' '+getThumbName,false);
//    end;
//  end;
//
////procedure T_dbEntry.moveFileToImages(fileIndex: longint);
////  VAR j:longint;
////  begin
////    if (fileIndex>=0) and (fileIndex<length(input)) then begin
////      input[fileIndex].moveTo(pp_images)
////      setLength(images,length(images)+1);
////      images[length(images)-1]:=input[fileIndex];
////      for j:=fileIndex to length(input)-2 do input[j]:=input[j+1];
////      setLength(input,length(input)-1);
////    end;
////  end;
////
////procedure T_dbEntry.moveFileToInput(fileIndex: longint);
////  VAR j:longint;
////  begin
////    if (fileIndex>=0) and (fileIndex<length(images)) then begin
////      images[fileIndex].moveToPath(C_inputPath+DirectorySeparator+images[fileIndex].getSubPath);
////      setLength(input,length(input)+1);
////      input[length(input)-1]:=images[fileIndex];
////      for j:=fileIndex to length(images)-2 do images[j]:=images[j+1];
////      setLength(images,length(images)-1);
////    end;
////  end;
//
//procedure T_dbEntry.mergeWith(otherEntry: T_dbEntry);
//  VAR i,i0:longint;
//  begin
//    comment:=comment+C_lineBreakChar+otherEntry.comment;
//
//    i0:=length(files);
//    setLength(files,i0+length(otherEntry.files));
//    for i:=0 to length(otherEntry.files)-1 do
//      files[i+i0]:=otherEntry.files[i];
//    setLength(otherEntry.files,0);
//
//    for i:=0 to otherEntry.tags.maxEntryIndex do
//      tags[i]:=tags[i] or otherEntry.tags[i];
//  end;
//
//FUNCTION replacePath(s:string; path,fname:string):string;
//  begin
//    while pos('%path',s)>0 do
//      s:=copy(s,1,pos('%path',s)-1)+path+copy(s,pos('%path',s)+5,length(s));
//    while pos('%file',s)>0 do
//      s:=copy(s,1,pos('%file',s)-1)+fname+copy(s,pos('%file',s)+5,length(s));
//    result:=s;
//  end;
//
////procedure T_dbEntry.generateImage(parameters: string; VAR dispatcher:T_dispatcher);
////  VAR ext:ansistring;
////      i:longint;
////  begin
////    if length(input)>0 then begin
////      ext:=input[0].getNormalizedExtension;
////      i:=0;
////      while (i<length(C_extToTagMap)) and (C_extToTagMap[i,0]<>ext) do inc(i);
////      if i<length(C_extToTagMap) then
////        dispatcher.appendTask(replacePath(C_extToTagMap[i,2],input[0].getPath,ExtractFileName(input[0].getName))+' '+parameters,true);
////    end;
////  end;
//
//FUNCTION T_dbEntry.dropFile(fileIndex: longint; delete: boolean):P_fileInfo;
//  VAR i:longint;
//  begin
//    if (fileIndex>=0) and (fileIndex<length(files)) then begin
//      if delete then begin
//        files[fileIndex]^.delete;
//        dispose(files[fileIndex],destroy);
//      end else result:=files[fileIndex];
//      for i:=fileIndex to length(files)-2 do files[i]:=files[i+1];
//      setLength(files,length(files)-1);
//      updateAutomaticFields;
//    end;
//  end;
//
////FUNCTION T_dbEntry.dropImageFile(fileIndex: longint; delete: boolean):T_fileInfo;
////  VAR i:longint;
////  begin
////    if (fileIndex>=0) and (fileIndex<length(images)) then begin
////      if delete then begin
////        images[fileIndex].delete;
////        images[fileIndex].destroy;
////      end else result:=images[fileIndex];
////      for i:=fileIndex to length(images)-2 do images[i]:=images[i+1];
////      setLength(images,length(images)-1);
////      updateAutomaticFields;
////      writeln('image ',Result.filename,' dropped from ',givenName);
////    end;
////  end;
//
////procedure T_dbEntry.swapInputFiles(i0, i1: longint);
////  VAR temp:T_fileInfo;
////  begin
////    if (i0>=0) and (i0<length(input)) and (i1>=0) and (i1<length(input)) then begin
////      temp:=input[i0];
////      input[i0]:=input[i1];
////      input[i1]:=temp;
////    end;
////  end;
////
//PROCEDURE T_dbEntry.swapFiles(i0, i1: longint);
//  VAR temp:P_fileInfo;
//  begin
//    if (i0>=0) and (i0<length(files)) and (i1>=0) and (i1<length(files)) then begin
//      temp:=files[i0];
//      files[i0]:=files[i1];
//      files[i1]:=temp;
//    end;
//  end;
//
//function T_dbEntry.newFileMatches(fileInfo: T_fileInfo): boolean;
//  VAR i:longint;
//  begin
//    for i:=0 to length(files)-1 do
//      if files[i]^.matches(fileInfo) then exit(true);
//    result:=false;
//  end;
//
//FUNCTION T_dbEntry.containsFile(fileInfo: P_fileInfo): boolean;
//  VAR i:longint;
//  begin
//    result:=false;
//    for i:=0 to length(files)-1 do result:=result or (files[i]=fileInfo);
//  end;
//
////FUNCTION nameWithRes(VAR f:T_fileInfo):string;
////  begin
////    result:=f.filename;
////    if f.isImage then result:=result+' @'+f.getResolutionString;
////  end;
//
////PROCEDURE T_dbEntry.getInputFileList(s:TStrings);
////  VAR i:longint;
////  begin
////    while s.Count>length(input) do s.Delete(s.Count-1);
////    for i:=0 to length(input)-1 do
////      if i>=s.Count then s.Append(nameWithRes(input[i]))
////                    else s[i]:=   nameWithRes(input[i]);
////  end;
////
//PROCEDURE T_dbEntry.getFileList(s:TStrings);
//  VAR i:longint;
//      desc:string;
//  begin
//    while s.Count>length(files) do s.Delete(s.Count-1);
//    for i:=0 to length(files)-1 do begin
//      desc:=files[i]^.getNameAsString;
//      if files[i]^.isImage then desc:=desc+' @'+files[i]^.getResolutionString;
//      if i>=s.Count then s.Append(desc)
//                    else s[i]:=   desc;
//    end;
//  end;
//
//PROCEDURE T_dbEntry.addNewFileWithoutTouchingTags(fileInfo: P_fileInfo);
//  begin
//    if fileInfo^.isExistent then begin
//      setLength(files,length(files)+1);
//      files[length(files)-1]:=fileInfo;
//      updateAutomaticFields;
//    end;
//  end;
//
//PROCEDURE T_dbEntry.addNewFile(fileInfo: P_fileInfo; VAR taglist: T_tagList);
//  VAR ext:string;
//      i:longint;
//  begin
//    if fileInfo^.isExistent then begin
//      addNewFileWithoutTouchingTags(fileInfo);
//      ext:=fileInfo^.getNormalizedExtension;
//      for i:=0 to length(C_extToTagMap)-1 do if C_extToTagMap[i,0]=ext then
//        addTag(C_extToTagMap[i,1],taglist);
//    end;
//  end;
//
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
//
//procedure T_dbEntry.dropNonexistentFiles;
//  VAR i,j:longint;
//  begin
//    j:=0;
//    for i:=0 to length(files)-1 do
//      if files[i]^.isExistent then begin
//        if i<>j then files[j]:=files[i];
//        inc(j);
//      end else dispose(files[i],destroy);
//    setLength(files,j);
//    updateAutomaticFields;
//  end;
//
////procedure T_dbEntry.returnToWorkInProgress;
////  VAR i:longint;
////  begin
////    for i:=0 to length(input)-1 do begin input[i].moveToPath(C_workInProgressPath);input[i].destroy; end;
////    setLength(input,0);
////    for i:=0 to length(images)-1 do begin images[i].moveToPath(C_workInProgressPath); images[i].destroy; end;
////    setLength(images,0);
////  end;
//
////procedure T_dbEntry.copyToWorkInProgress;
////  VAR i:longint;
////  begin
////    for i:=0 to length(input)-1 do input[i].copyToPath(C_workInProgressPath);
////  end;
//
//function T_dbEntry.containsFileLike(s: string): boolean;
//  VAR i:longint;
//  begin
//    result:=false;
//    s:=UpperCase(s);
//    for i:=0 to length(files)-1 do result:=result or (pos(s,uppercase(files[i]^.getNameAsString))>0);
//  end;
//
//procedure T_dbEntry.showPrimary(var dispatcher: T_dispatcher);
//  VAR i:longint;
//  begin
//    i:=0;
//    while (i<length(files)) and not(files[i]^.isImage) do inc(i);
//    if i<length(files) then files[i]^.showFile(dispatcher);
//  end;
//
//FUNCTION T_dbEntry.filename: string;
//  BEGIN
//    if length(files)>0 then result:=files[0]^.getPath
//                       else result:='';
//  END;
//
//{ T_tagList }
//
//constructor T_tagList.create;
//  begin
//    setLength(list,0);
//  end;
//
//destructor T_tagList.destroy;
//  begin
//    setLength(list,0);
//  end;
//
//function T_tagList.loadFromFile(var F: T_File): boolean;
//  VAR i,len:longint;
//  begin
//    len:=f.readLongint;
//    if len<0 then result:=false
//    else begin
//      result:=true;
//      setLength(list,len);
//      for i:=0 to len-1 do list[i]:=f.readAnsiString;
//    end;
//  end;
//
//procedure T_tagList.saveToFile(var F: T_File);
//  VAR i:longint;
//  begin
//    f.writeLongint(length(list));
//    for i:=0 to length(list)-1 do f.writeAnsiString(list[i]);
//  end;
//

end.
