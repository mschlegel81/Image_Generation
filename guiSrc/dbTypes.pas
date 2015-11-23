UNIT dbTypes;
INTERFACE
USES myFiles,FileUtil,sysutils,mypics,myGenerics,dispatcher,Classes,Graphics;
TYPE T_parentPath=(pp_images,pp_input,pp_thumbnails,pp_wip,pp_none);
CONST C_imageExt:array[0..4] of string=('.BMP','.JPG','.PNG','.ICO','.GIF');
      C_parentPath:array[T_parentPath] of string=(
        'images'+DirectorySeparator,
        'input'+DirectorySeparator,
        'thumbnails'+DirectorySeparator,
        'wip'+DirectorySeparator,'');

      C_lineBreakChar=#13;
      C_extToTagMap  :array[0..6,0..3] of string=
        (('.BAT'  ,'manipulated'  ,'cd %path && %file ','"c:\Program Files (x86)\ConTEXT\ConTEXT.exe" %path%file '),
         ('.PAS'  ,'stand alone'  ,'cd %path && runpas %file ','"c:\Program Files (x86)\ConTEXT\ConTEXT.exe" %path%file '),
         ('.EXE'  ,'stand alone'  ,'cd %path && %file',''),
         ('.PARAM','ifs'          ,'ifsjobber %path%file' ,'ifs %path%file'),
         ('.JOB'  ,'relief'       ,'reliefs %path%file'   ,'reliefs %path%file -d'),
         ('.ECJ'  ,'expo cloud'   ,'expoClouds %path%file','expoClouds %path%file -d'),
         ('.FTJ'  ,'function tree','functrees %path%file' ,'functrees %path%file -d'));
      C_thumbnailMaxWidthHeight=128;

TYPE

  T_sortCriterion=(sc_none,
                   sc_commonPrefix_asc,
                   sc_commonPrefix_desc,
                   sc_name_asc,
                   sc_name_desc,
                   sc_tags_asc,
                   sc_tags_desc,
                   sc_firstChange_asc,
                   sc_firstChange_desc,
                   sc_lastChange_asc,
                   sc_lastChange_desc);

  { T_tagList }
  P_tagList=^T_tagList;
  T_tagList=object(T_serializable)
    private
      list:array of string;
    public
    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual; overload;
    PROCEDURE saveToFile(VAR F:T_file);           virtual; overload;
    FUNCTION tagIndex(tagString:string; allowCreation:boolean):longint;
    PROCEDURE getTags(VAR indexes:T_indexSet; strings:TStrings);
    FUNCTION getTagsForList(VAR indexes:T_indexSet):ansistring;
    PROCEDURE getTagsForDropDown(strings:TStrings);
  end;

  { T_fileInfo }

  T_fileInfo=object(T_serializable)
    private
      parentPath:T_parentPath;
      subPath,name:ansistring;
      xRes,yRes:longint; //-2: never checked; -1: no image
      lastCheckedAtAge:double;
    public
    CONSTRUCTOR create(filePath:ansistring);
    DESTRUCTOR destroy;


    //FUNCTION getName:ansistring;
    //FUNCTION getAge:double;
    //FUNCTION isExistent:boolean;
    //FUNCTION isImage:boolean;
    //PROCEDURE getResolution(OUT width,height:longint);
    //FUNCTION getResolutionString:string;
    //FUNCTION  loadFromFile(VAR F:T_File):boolean; virtual; overload;
    //PROCEDURE saveToFile(VAR F:T_File);           virtual; overload;
    //FUNCTION commonPrefix(otherPrefix:string):string;
    //FUNCTION nameWithNewPrefix(oldPrefixLength:longint; newPrefix:string):string;
    //PROCEDURE delete;
    //PROCEDURE moveToPath(path:ansistring);
    //PROCEDURE copyToPath(path:ansistring);
    //PROCEDURE rename(nameWithoutPath:ansistring);
    //FUNCTION getPath:ansistring;
    //FUNCTION getSubPath:ansistring;
    //FUNCTION getNormalizedExtension:ansistring;
    //FUNCTION getGroupName:string;
    //PROCEDURE showFile(VAR dispatcher:T_dispatcher);
  end;

  P_dbEntry=^T_dbEntry;

  { T_dbEntry }

  T_dbEntry=object(T_serializable)
    givenName:string;
    comment:ansistring;
    images:array of T_fileInfo;
    input :array of T_fileInfo;
    tags  :T_indexSet;
    thumb:record
      createdForFileName:string;
      createdForFileTime:double;
      loaded:boolean;
      picture:TPicture;
    end;

    //for GUI-interaction:
    marked:boolean;
    //automatic by files:
    minAge,maxAge:double;
    commonPrefix:string;
    markedForDeletion:boolean;
    markedForMerge:boolean;

    CONSTRUCTOR create(info:T_fileInfo; VAR tagList:T_tagList);
    CONSTRUCTOR createToLoad;
    DESTRUCTOR destroy;
    PROCEDURE updateAutomaticFields;
    FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual; overload;
    PROCEDURE saveToFile(VAR F:T_file);           virtual; overload;
    PROCEDURE checkExistence;
    FUNCTION getThumbName:string;
    PROCEDURE changeCommonPrefix(newCommonPrefix:string);
    PROCEDURE generateThumbnail(VAR dispatcher:T_dispatcher);
    PROCEDURE moveFileToImages(fileIndex:longint);
    PROCEDURE moveFileToInput (fileIndex:longint);
    PROCEDURE addTag(tagString:ansistring; VAR tagList:T_tagList);
    PROCEDURE removeTag(tagString:ansistring; VAR tagList:T_tagList);
    PROCEDURE mergeWith(otherEntry:T_dbEntry);
    PROCEDURE generateImage(parameters:string; VAR dispatcher:T_dispatcher);
    FUNCTION dropInputFile(fileIndex:longint; delete:boolean):T_fileInfo;
    FUNCTION dropImageFile(fileIndex:longint; delete:boolean):T_fileInfo;
    PROCEDURE swapInputFiles(i0,i1:longint);
    PROCEDURE swapImageFiles(i0,i1:longint);
    FUNCTION  newFileMatches(fileInfo:T_fileInfo):boolean;
    FUNCTION containsFile(fileInfo:T_fileInfo):boolean;
    PROCEDURE getInputFileList(s:TStrings);
    PROCEDURE getImageFileList(s:TStrings);
    PROCEDURE addNewFileWithoutTouchingTags(VAR fileInfo:T_fileInfo);
    PROCEDURE addNewFile(VAR fileInfo:T_fileInfo; VAR tagList:T_tagList);
    PROCEDURE dropThumb;
    PROCEDURE loadThumb;
    PROCEDURE dropNonexistentFiles;
    PROCEDURE returnToWorkInProgress;
    PROCEDURE copyToWorkInProgress;
    FUNCTION containsFileLike(s:string):boolean;
    PROCEDURE showPrimary(VAR dispatcher:T_dispatcher);
  end;

  { T_listOfEntries }

  T_listOfEntries=object(T_serializable)
    private
      list:array of P_dbEntry;
      FUNCTION getEntry(index:longint):P_dbEntry;
    public
      PROPERTY entry[index:longint]:P_dbEntry read getEntry; default;
      CONSTRUCTOR create;
      PROCEDURE disposeAll;
      DESTRUCTOR destroy;
      FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual; overload;
      PROCEDURE saveToFile(VAR F:T_file);           virtual; overload;
      PROCEDURE fillFrom(VAR otherList:T_listOfEntries);
      PROCEDURE filterFrom(VAR otherList:T_listOfEntries; filter:ansistring; VAR tagList:T_tagList);
      PROCEDURE refilter(filter:ansistring; VAR tagList:T_tagList);
      PROCEDURE sort(criterion:T_sortCriterion);
      FUNCTION  size:longint;
      PROCEDURE addNewEntry(fileInfo:T_fileInfo; VAR tagList:T_tagList);
      PROCEDURE dropDispose(index:longint);
  end;

  { T_fileDB }
  P_fileDB=^T_fileDB;
  T_fileDB=object(T_serializable)
    entry:T_listOfEntries;
    tagList:T_tagList;
    lastRescan:double;
    filteredEntry:T_listOfEntries;

    currentSortCriterion:T_sortCriterion;
    filteredEntryEqualsEntry:boolean;

    CONSTRUCTOR create;
    DESTRUCTOR destroy;
    PROCEDURE saveToFile;
    FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual; overload;
    PROCEDURE saveToFile(VAR F:T_file);           virtual; overload;
    FUNCTION rescan:boolean;
    PROCEDURE generateThumbnails(VAR dispatcher:T_dispatcher);
    PROCEDURE cleanupThumbnails;
    PROCEDURE sort(criterion:T_sortCriterion);
    PROCEDURE filter(by:string);
    PROCEDURE filterByFile(name:string);
    PROCEDURE clearFilter;
    PROCEDURE performMerge;
    PROCEDURE addWithoutMerging(f:T_fileInfo);
  end;

IMPLEMENTATION

{ T_listOfEntries }

FUNCTION T_listOfEntries.getEntry(index: longint): P_dbEntry;
  begin
    if (index>=0) and (index<length(list))
      then result:=list[index]
      else result:=nil;
  end;

CONSTRUCTOR T_listOfEntries.create;
  begin
    setLength(list,0);
  end;

PROCEDURE T_listOfEntries.disposeAll;
  VAR i:longint;
  begin
    for i:=0 to length(list)-1 do dispose(list[i],destroy);
    setLength(list,0);
  end;

DESTRUCTOR T_listOfEntries.destroy;
  begin
    setLength(list,0);
  end;

FUNCTION T_listOfEntries.loadFromFile(VAR F: T_file): boolean;
  VAR i,c:longint;
  begin
    c:=f.readLongint;
    if c<0 then exit(false);
    setLength(list,c);
    for i:=0 to length(list)-1 do begin
      new(list[i],createToLoad);
      list[i]^.loadFromFile(f);
    end;
    result:=true;
  end;

PROCEDURE T_listOfEntries.saveToFile(VAR F: T_file);
  VAR i:longint;
  begin
    f.writeLongint(length(list));
    for i:=0 to length(list)-1 do list[i]^.saveToFile(f);
  end;

PROCEDURE T_listOfEntries.fillFrom(VAR otherList: T_listOfEntries);
  VAR i,j:longint;
  begin
    setLength(list,length(otherList.list));
    j:=0;
    for i:=0 to length(list)-1 do begin
      if not(otherList.list[i]^.markedForDeletion) then begin
        list[j]:=otherList.list[i];
        inc(j);
      end;
    end;
  end;

PROCEDURE T_listOfEntries.filterFrom(VAR otherList: T_listOfEntries; filter: ansistring; VAR tagList:T_tagList);
  VAR i,j:longint;
      tagToFilter:longint;
  begin
    if (length(filter)>1) and (filter[1]='#') then begin
      tagToFilter:=tagList.tagIndex(copy(filter,2,length(filter)-1),false);
      filter:='';
    end else begin
      tagToFilter:=-1;
      filter:=uppercase(filter);
    end;
    setLength(list,otherList.size);
    j:=0;
    for i:=0 to otherList.size-1 do
      if (not(otherList[i]^.markedForDeletion)) and
         ((tagToFilter=-1) or (otherList[i]^.tags[tagToFilter])) and
         ((filter='') or (pos(filter,uppercase(otherList[i]^.givenName))>0)
                      or (pos(filter,uppercase(otherList[i]^.commonPrefix))>0)
                      or (pos(filter,uppercase(otherList[i]^.comment))>0)) then begin
      list[j]:=otherList[i];
      inc(j);
    end;
    setLength(list,j);
  end;

PROCEDURE T_listOfEntries.refilter(filter: ansistring; VAR tagList:T_tagList);
  VAR i,j:longint;
      tagToFilter:longint;
  begin
    if (length(filter)>1) and (filter[1]='#') then begin
      tagToFilter:=tagList.tagIndex(copy(filter,2,length(filter)-1),false);
      filter:='';
    end else begin
      tagToFilter:=-1;
      filter:=uppercase(filter);
    end;
    j:=0;
    for i:=0 to length(list)-1 do
      if not(list[i]^.markedForDeletion) and
         ((tagToFilter=-1) or (list[i]^.tags[tagToFilter])) and
         ((filter='') or (pos(filter,uppercase(list[i]^.givenName))>0)
                      or (pos(filter,uppercase(list[i]^.commonPrefix))>0)
                      or (pos(filter,uppercase(list[i]^.comment))>0)) then begin
      list[j]:=list[i];
      inc(j);
    end;
    setLength(list,j);
  end;

PROCEDURE T_listOfEntries.sort(criterion: T_sortCriterion);
  FUNCTION leq(x,y:P_dbEntry):boolean;
    begin
      case criterion of
        sc_commonPrefix_asc : result:=uppercase(x^.commonPrefix)<=uppercase(y^.commonPrefix);
        sc_commonPrefix_desc: result:=uppercase(x^.commonPrefix)>=uppercase(y^.commonPrefix);
        sc_name_asc :         result:=uppercase(x^.givenName)<=uppercase(y^.givenName);
        sc_name_desc:         result:=uppercase(x^.givenName)>=uppercase(y^.givenName);
        sc_tags_asc:          result:=not(y^.tags.lesser(x^.tags));
        sc_tags_desc:         result:=not(x^.tags.lesser(y^.tags));
        sc_firstChange_asc:   result:=x^.minAge<=y^.minAge;
        sc_firstChange_desc:  result:=x^.minAge>=y^.minAge;
        sc_lastChange_asc:    result:=x^.maxAge<=y^.maxAge;
        sc_lastChange_desc:   result:=x^.maxAge>=y^.maxAge;
      end;
    end;

  VAR scale    :longint;
      i,j0,j1,k:longint;
      temp     :array of P_dbEntry;
  begin
    scale:=1;
    setLength(temp,length(list));
    while scale<length(list) do begin
      //merge lists of size [scale] to lists of size [scale+scale]:---------------
      i:=0;
      while i<length(list) do begin
        j0:=i;
        j1:=i+scale;
        k :=i;
        while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(list)) do begin
          if leq(list[j0],list[j1])
            then begin temp[k]:=list[j0]; inc(k); inc(j0); end
            else begin temp[k]:=list[j1]; inc(k); inc(j1); end;
        end;
        while (j0<i+scale)       and (j0<length(list)) do begin temp[k]:=list[j0]; inc(k); inc(j0); end;
        while (j1<i+scale+scale) and (j1<length(list)) do begin temp[k]:=list[j1]; inc(k); inc(j1); end;
        inc(i,scale+scale);
      end;
      //---------------:merge lists of size [scale] to lists of size [scale+scale]
      inc(scale,scale);
      if (scale<length(list)) then begin
        //The following is equivalent to the above with swapped roles of "list" and "temp".
        //While making the code a little more complicated it avoids unnecessary copys.
        //merge lists of size [scale] to lists of size [scale+scale]:---------------
        i:=0;
        while i<length(list) do begin
          j0:=i;
          j1:=i+scale;
          k :=i;
          while (j0<i+scale) and (j1<i+scale+scale) and (j1<length(list)) do begin
            if leq(temp[j0],temp[j1])
              then begin list[k]:=temp[j0]; inc(k); inc(j0); end
              else begin list[k]:=temp[j1]; inc(k); inc(j1); end;
          end;
          while (j0<i+scale)       and (j0<length(list)) do begin list[k]:=temp[j0]; inc(k); inc(j0); end;
          while (j1<i+scale+scale) and (j1<length(list)) do begin list[k]:=temp[j1]; inc(k); inc(j1); end;
          inc(i,scale+scale);
        end;
        //---------------:merge lists of size [scale] to lists of size [scale+scale]
        inc(scale,scale);
      end else for k:=0 to length(list)-1 do list[k]:=temp[k];
    end;
    setLength(temp,0);
  end;

FUNCTION T_listOfEntries.size:longint;
  begin
    result:=length(list);
  end;

PROCEDURE T_listOfEntries.addNewEntry(fileInfo: T_fileInfo; VAR tagList:T_tagList);
  begin
    setLength(list,length(list)+1);
    new(list[length(list)-1],create(fileInfo,tagList));
  end;

PROCEDURE T_listOfEntries.dropDispose(index: longint);
  VAR i:longint;
  begin
    if (index>=0) and (index<length(list)) then begin
      dispose(list[index],destroy);
      for i:=index to length(list)-2 do list[i]:=list[i+1];
      setLength(list,length(list)-1);
    end;
  end;

{ T_fileDB }

CONSTRUCTOR T_fileDB.create;
  begin
    lastRescan:=0;
    tagList.create;
    entry.create;
    filteredEntry.create;
    loadFromFile('catalogue.dat');
  end;

DESTRUCTOR T_fileDB.destroy;
  begin
    tagList.destroy;
    filteredEntry.destroy;
    entry.disposeAll;
    entry.destroy;
  end;

PROCEDURE T_fileDB.saveToFile;
  begin
    saveToFile('catalogue.dat');
  end;

FUNCTION T_fileDB.loadFromFile(VAR F: T_file): boolean;
  begin
    result:=tagList.loadFromFile(f) and entry.loadFromFile(f);
    filteredEntry.fillFrom(entry);
    filteredEntryEqualsEntry:=true;
  end;

PROCEDURE T_fileDB.saveToFile(VAR F: T_file);
  VAR i:longint;
  begin
    i:=0;
    while i<entry.size do if entry[i]^.markedForDeletion then entry.dropDispose(i) else inc(i);
    tagList.saveToFile(f);
    entry.saveToFile(f);
  end;

FUNCTION T_fileDB.rescan:boolean;
  VAR found:array of string;
      s:TSearchRec;
      i,j:longint;
      f:T_fileInfo;
  begin
    result:=false;
    lastRescan:=now;
    for i:=0 to entry.size-1 do entry[i]^.dropNonexistentFiles;

    setLength(found,0);
    if FindFirst(C_inputPath+DirectorySeparator+'*',faAnyFile,s)=0 then repeat
      if (s.Attr and faDirectory)<>faDirectory then begin
        setLength(found,length(found)+1);
        found[length(found)-1]:=C_inputPath+DirectorySeparator+s.name;
      end;
    until findNext(s)<>0;
    FindClose(s);
    if FindFirst(C_imagePath+DirectorySeparator+'*',faAnyFile,s)=0 then repeat
      if (s.Attr and faDirectory)<>faDirectory then begin
        setLength(found,length(found)+1);
        found[length(found)-1]:=C_imagePath+DirectorySeparator+s.name;
      end;
    until findNext(s)<>0;
    FindClose(s);

    for i:=0 to length(found)-1 do begin
      f.create(found[i]);
      j:=0;
      while (j<entry.size) and not(entry[j]^.containsFile(f)) do inc(j);
      if j<entry.size then f.destroy else begin
        j:=0;
        while (j<entry.size) and not(entry[j]^.newFileMatches(f)) do inc(j);
        if j<entry.size
          then entry[j]^.addNewFile(f,tagList)
          else begin
            entry.addNewEntry(f,tagList);
            result:=true;
          end;
      end;
    end;
    if result then begin
      filteredEntry.fillFrom(entry);
      filteredEntryEqualsEntry:=true;
    end;
  end;

PROCEDURE T_fileDB.generateThumbnails(VAR dispatcher:T_dispatcher);
  VAR i:longint;
  begin
    for i:=0 to entry.size-1 do entry[i]^.generateThumbnail(dispatcher);
  end;

PROCEDURE T_fileDB.cleanupThumbnails;
  VAR inDB :T_listOfString;
      s:TSearchRec;
      i:longint;
      fname:string;
  begin
    inDB.create;
    for i:=0 to entry.size-1 do inDB.add(entry[i]^.getThumbName);
    inDB.unique;
    if FindFirst(C_thumbnailPath+DirectorySeparator+'*.*',faAnyFile,s)=0 then repeat
      fname:=C_thumbnailPath+DirectorySeparator+s.name;
      if not(inDB.contains(fname)) then DeleteFile(fname);
    until findNext(s)<>0;
    FindClose(s);
    inDB.destroy;
  end;

PROCEDURE T_fileDB.sort(criterion: T_sortCriterion);
  begin
    if criterion=currentSortCriterion then case currentSortCriterion of
      sc_commonPrefix_asc : criterion:=sc_commonPrefix_desc;
      sc_commonPrefix_desc: criterion:=sc_commonPrefix_asc ;
      sc_name_asc         : criterion:=sc_name_desc;
      sc_name_desc        : criterion:=sc_name_asc;
      sc_tags_asc         : criterion:=sc_tags_desc;
      sc_tags_desc        : criterion:=sc_tags_asc;
      sc_firstChange_asc  : criterion:=sc_firstChange_desc;
      sc_firstChange_desc : criterion:=sc_firstChange_asc;
      sc_lastChange_asc   : criterion:=sc_lastChange_desc;
      sc_lastChange_desc  : criterion:=sc_lastChange_asc;
    end;
    currentSortCriterion:=criterion;
    if filteredEntryEqualsEntry then begin
      entry.sort(criterion);
      filteredEntry.fillFrom(entry);
    end else begin
      filteredEntry.sort(criterion);
    end;
  end;

PROCEDURE T_fileDB.filter(by: string);
  begin
    if filteredEntryEqualsEntry
      then filteredEntry.filterFrom(entry,by,tagList)
      else filteredEntry.refilter        (by,tagList);
    filteredEntryEqualsEntry:=false;
  end;

PROCEDURE T_fileDB.filterByFile(name: string);
  VAR i,j:longint;
  begin
    setLength(filteredEntry.list,entry.size);
    j:=0;
    for i:=0 to entry.size-1 do if entry[i]^.containsFileLike(name) then begin
      filteredEntry.list[j]:=entry[i];
      inc(j);
    end;
    setLength(filteredEntry.list,j);
    filteredEntryEqualsEntry:=false;
  end;

PROCEDURE T_fileDB.clearFilter;
  VAR i:longint;
  begin
    i:=0;
    while i<entry.size do if entry[i]^.markedForDeletion then entry.dropDispose(i) else inc(i);
    entry.sort(currentSortCriterion);
    filteredEntry.fillFrom(entry);
    filteredEntryEqualsEntry:=true;
  end;

PROCEDURE T_fileDB.performMerge;
  VAR i,i0:longint;
  begin
    i0:=-1;
    for i:=0 to entry.size-1 do if entry[i]^.markedForMerge then begin
      if i0=-1 then i0:=i else begin
        entry[i0]^.mergeWith(entry[i]^);
        entry[i]^.markedForDeletion:=true;
      end;
      entry[i]^.markedForMerge:=false;
    end;
    filteredEntry.refilter('',tagList);
  end;

PROCEDURE T_fileDB.addWithoutMerging(f: T_fileInfo);
  begin
    setLength(entry.list,length(entry.list)+1);
    new(entry.list[length(entry.list)-1],create(f,tagList));
    filteredEntryEqualsEntry:=false;
  end;

{ T_dbEntry }

CONSTRUCTOR T_dbEntry.createToLoad;
  begin
    setLength(input,0);
    givenName:='';
    comment:='';
    setLength(images,0);
    tags.create;
    marked:=false;

    minAge:=0;
    maxAge:=0;
    commonPrefix:='';

    markedForDeletion:=false;
    markedForMerge:=false;

    thumb.picture:=nil;
    thumb.loaded:=false;
  end;

CONSTRUCTOR T_dbEntry.create(info:T_fileInfo; VAR tagList:T_tagList);
  begin
    createToLoad;
    addNewFile(info,tagList);
    givenName:=commonPrefix;
  end;

DESTRUCTOR T_dbEntry.destroy;
  VAR i:longint;
  begin
    for i:=0 to length(input)-1 do input[i].destroy;
    setLength(input,0);
    for i:=0 to length(images)-1 do images[i].destroy;
    tags.destroy;
  end;

PROCEDURE T_dbEntry.updateAutomaticFields;
  VAR firstFile:boolean;
      i:longint;
      newAge:double;
  begin
    firstFile:=true;
    for i:=0 to length(input)-1 do if (firstFile) then begin
      minAge:=input[i].getAge;
      maxAge:=minAge;
      commonPrefix:=input[i].getGroupName;
      firstFile:=false;
    end else begin
      newAge:=input[i].getAge;
      if newAge<minAge then minAge:=newAge;
      if newAge>maxAge then maxAge:=newAge;
      commonPrefix:=input[i].commonPrefix(commonPrefix);
    end;
    for i:=0 to length(images)-1 do if (firstFile) then begin
      minAge:=images[i].getAge;
      maxAge:=minAge;
      commonPrefix:=images[i].getGroupName;
      firstFile:=false;
    end else begin
      newAge:=images[i].getAge;
      if newAge<minAge then minAge:=newAge;
      if newAge>maxAge then maxAge:=newAge;
      commonPrefix:=images[i].commonPrefix(commonPrefix);
    end;
    if firstFile then begin
      minAge:=0;
      maxAge:=0;
      commonPrefix:='';
    end;
  end;

FUNCTION T_dbEntry.loadFromFile(VAR F: T_file): boolean;
  VAR i,fileCount:longint;
      fileInfo:T_fileInfo;
  begin
    fileCount:=f.readLongint;
    result:=(fileCount>=0) and tags.loadFromFile(f);
    if result then begin
      for i:=0 to fileCount-1 do begin
        fileInfo.create('');
        fileInfo.loadFromFile(f);
        addNewFileWithoutTouchingTags(fileInfo);
      end;
      givenName:=f.readAnsiString;
      comment:=f.readAnsiString;
      thumb.createdForFileName:=f.readAnsiString;
      thumb.createdForFileTime:=f.readDouble;
    end;
  end;

PROCEDURE T_dbEntry.saveToFile(VAR F: T_file);
  VAR i:longint;
  begin
    f.writeLongint(length(input)+length(images));
    tags.saveToFile(f);
    for i:=0 to length(input)-1 do input[i].saveToFile(f);
    for i:=0 to length(images)-1 do images[i].saveToFile(f);
    f.writeAnsiString(givenName);
    f.writeAnsiString(comment);
    f.writeAnsiString(thumb.createdForFileName);
    f.writeDouble(thumb.createdForFileTime);
  end;

PROCEDURE T_dbEntry.checkExistence;
  VAR i,j:longint;
  begin
    i:=0;
    while i<length(input) do begin
      if input[i].isExistent then inc(i)
      else begin
        input[i].destroy;
        for j:=i to length(input)-2 do input[j]:=input[j+1];
        setLength(input,length(input)-1);
      end;
    end;

    i:=0;
    while i<length(images) do begin
      if images[i].isExistent then inc(i)
      else begin
        images[i].destroy;
        for j:=i to length(images)-2 do images[j]:=images[j+1];
        setLength(images,length(images)-1);
      end;
    end;
  end;

FUNCTION T_dbEntry.getThumbName:string;
  begin
    result:=C_thumbnailPath+DirectorySeparator+commonPrefix+'.jpg';
  end;

PROCEDURE T_dbEntry.changeCommonPrefix(newCommonPrefix: string);
  VAR i,len:longint;
      renamePossible:boolean;
      newName:string;
  begin
    len:=length(commonPrefix);
    renamePossible:=true;
    for i:=0 to length(input)-1 do begin
      newName:=input[i].getPath+input[i].nameWithNewPrefix(len,newCommonPrefix);
      renamePossible:=renamePossible and not(fileExists(newName));
    end;
    for i:=0 to length(images)-1 do begin
      newName:=images[i].getPath+images[i].nameWithNewPrefix(len,newCommonPrefix);
      renamePossible:=renamePossible and not(fileExists(newName));
    end;
    if renamePossible then begin
      for i:=0 to length(input)-1 do input[i].rename(input[i].nameWithNewPrefix(len,newCommonPrefix));
      for i:=0 to length(images)-1 do images[i].rename(images[i].nameWithNewPrefix(len,newCommonPrefix));
      updateAutomaticFields;
    end;
  end;

PROCEDURE T_dbEntry.generateThumbnail(VAR dispatcher: T_dispatcher);
  VAR i:longint;
  begin
    i:=0;
    while (i<length(images)) and not(images[i].isImage) do inc(i);
    if i>=length(images) then exit; //no image exists

    with thumb do
      if not(fileExists(getThumbName))
      or (createdForFileTime<>images[i].getAge)
      or (createdForFileName<>images[i].getName) then begin
      createdForFileName:=images[i].getName;
      createdForFileTime:=images[i].getAge;
      dispatcher.appendTask('im '+images[i].getName+' -cr'+intToStr(C_thumbnailMaxWidthHeight)+'x'+intToStr(C_thumbnailMaxWidthHeight)+' '+getThumbName,false);
    end;
  end;

PROCEDURE T_dbEntry.moveFileToImages(fileIndex: longint);
  VAR j:longint;
  begin
    if (fileIndex>=0) and (fileIndex<length(input)) then begin
      input[fileIndex].moveToPath(C_imagePath+DirectorySeparator+input[fileIndex].getSubPath);
      setLength(images,length(images)+1);
      images[length(images)-1]:=input[fileIndex];
      for j:=fileIndex to length(input)-2 do input[j]:=input[j+1];
      setLength(input,length(input)-1);
    end;
  end;

PROCEDURE T_dbEntry.moveFileToInput(fileIndex: longint);
  VAR j:longint;
  begin
    if (fileIndex>=0) and (fileIndex<length(images)) then begin
      images[fileIndex].moveToPath(C_inputPath+DirectorySeparator+images[fileIndex].getSubPath);
      setLength(input,length(input)+1);
      input[length(input)-1]:=images[fileIndex];
      for j:=fileIndex to length(images)-2 do images[j]:=images[j+1];
      setLength(images,length(images)-1);
    end;
  end;

PROCEDURE T_dbEntry.addTag(tagString: ansistring; VAR tagList: T_tagList);
  begin
    while (length(tagString)>1) and (tagString[1]='#') do tagString:=copy(tagString,2,length(tagString)-1);
    if length(tagString)>0 then tags[tagList.tagIndex(tagString,true)]:=true;
  end;

PROCEDURE T_dbEntry.removeTag(tagString: ansistring; VAR tagList: T_tagList);
  begin
    tags[tagList.tagIndex(tagString,false)]:=false;
  end;

PROCEDURE T_dbEntry.mergeWith(otherEntry: T_dbEntry);
  VAR i,i0:longint;
  begin
    comment:=comment+C_lineBreakChar+otherEntry.comment;

    i0:=length(images);
    setLength(images,i0+length(otherEntry.images));
    for i:=0 to length(otherEntry.images)-1 do
      images[i+i0]:=otherEntry.images[i];
    setLength(otherEntry.images,0);

    i0:=length(input);
    setLength(input,i0+length(otherEntry.input));
    for i:=0 to length(otherEntry.input)-1 do
      input[i+i0]:=otherEntry.input[i];
    setLength(otherEntry.input,0);

    for i:=0 to otherEntry.tags.maxEntryIndex do
      tags[i]:=tags[i] or otherEntry.tags[i];
  end;

FUNCTION replacePath(s:string; path,fname:string):string;
  begin
    while pos('%path',s)>0 do
      s:=copy(s,1,pos('%path',s)-1)+path+copy(s,pos('%path',s)+5,length(s));
    while pos('%file',s)>0 do
      s:=copy(s,1,pos('%file',s)-1)+fname+copy(s,pos('%file',s)+5,length(s));
    result:=s;
  end;


PROCEDURE T_dbEntry.generateImage(parameters: string; VAR dispatcher:T_dispatcher);
  VAR ext:ansistring;
      i:longint;
  begin
    if length(input)>0 then begin
      ext:=input[0].getNormalizedExtension;
      i:=0;
      while (i<length(C_extToTagMap)) and (C_extToTagMap[i,0]<>ext) do inc(i);
      if i<length(C_extToTagMap) then
        dispatcher.appendTask(replacePath(C_extToTagMap[i,2],input[0].getPath,extractFileName(input[0].getName))+' '+parameters,true);
    end;
  end;

FUNCTION T_dbEntry.dropInputFile(fileIndex: longint; delete: boolean):T_fileInfo;
  VAR i:longint;
  begin
    if (fileIndex>=0) and (fileIndex<length(input)) then begin
      if delete then begin
        input[fileIndex].delete;
        input[fileIndex].destroy;
      end else result:=input[fileIndex];
      for i:=fileIndex to length(input)-2 do input[i]:=input[i+1];
      setLength(input,length(input)-1);
      updateAutomaticFields;
    end;
  end;

FUNCTION T_dbEntry.dropImageFile(fileIndex: longint; delete: boolean):T_fileInfo;
  VAR i:longint;
  begin
    if (fileIndex>=0) and (fileIndex<length(images)) then begin
      if delete then begin
        images[fileIndex].delete;
        images[fileIndex].destroy;
      end else result:=images[fileIndex];
      for i:=fileIndex to length(images)-2 do images[i]:=images[i+1];
      setLength(images,length(images)-1);
      updateAutomaticFields;
      writeln('image ',result.fileName,' dropped from ',givenName);
    end;
  end;

PROCEDURE T_dbEntry.swapInputFiles(i0, i1: longint);
  VAR temp:T_fileInfo;
  begin
    if (i0>=0) and (i0<length(input)) and (i1>=0) and (i1<length(input)) then begin
      temp:=input[i0];
      input[i0]:=input[i1];
      input[i1]:=temp;
    end;
  end;

PROCEDURE T_dbEntry.swapImageFiles(i0, i1: longint);
  VAR temp:T_fileInfo;
  begin
    if (i0>=0) and (i0<length(images)) and (i1>=0) and (i1<length(images)) then begin
      temp:=images[i0];
      images[i0]:=images[i1];
      images[i1]:=temp;
    end;
  end;

FUNCTION T_dbEntry.newFileMatches(fileInfo: T_fileInfo): boolean;
  VAR i:longint;
      otherPrefix:string;
  begin
    otherPrefix:=fileInfo.getGroupName;
    i:=1;
    while (i<=length(commonPrefix)) and (i<=length(otherPrefix)) and (uppercase(commonPrefix[i])=uppercase(otherPrefix[i])) do inc(i);
    dec(i);
    result:=i>=0.47*(length(otherPrefix)+length(commonPrefix));
  end;

FUNCTION T_dbEntry.containsFile(fileInfo:T_fileInfo):boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=0 to length(images)-1 do result:=result or (images[i].fileName=fileInfo.fileName);
    for i:=0 to length(input)-1 do result:=result or (input[i].fileName=fileInfo.fileName);
  end;

FUNCTION nameWithRes(VAR f:T_fileInfo):string;
  begin
    result:=f.fileName;
    if f.isImage then result:=result+' @'+f.getResolutionString;
  end;

PROCEDURE T_dbEntry.getInputFileList(s:TStrings);
  VAR i:longint;
  begin
    while s.count>length(input) do s.delete(s.count-1);
    for i:=0 to length(input)-1 do
      if i>=s.count then s.append(nameWithRes(input[i]))
                    else s[i]:=   nameWithRes(input[i]);
  end;

PROCEDURE T_dbEntry.getImageFileList(s:TStrings);
  VAR i:longint;
  begin
    while s.count>length(images) do s.delete(s.count-1);
    for i:=0 to length(images)-1 do
      if i>=s.count then s.append(nameWithRes(images[i]))
                    else s[i]:=   nameWithRes(images[i]);
  end;

PROCEDURE T_dbEntry.addNewFileWithoutTouchingTags(VAR fileInfo: T_fileInfo);
  VAR newAge:double;
      nameIscommonPrefix:boolean;
  begin
    if fileInfo.isExistent then begin
      if (fileInfo.getPath=C_imagePath+DirectorySeparator) then begin
        setLength(images,length(images)+1);
        images[length(images)-1]:=fileInfo;
      end else begin
        if (fileInfo.getPath<>C_inputPath+DirectorySeparator) then fileInfo.moveToPath(C_inputPath);
        setLength(input,length(input)+1);
        input[length(input)-1]:=fileInfo;
      end;

      //first file:
      if (length(images)+length(input)=1) then begin
        minAge:=fileInfo.getAge;
        maxAge:=minAge;
        commonPrefix:=fileInfo.getGroupName;
      end else begin
        newAge:=fileInfo.getAge;
        if newAge<minAge then minAge:=newAge;
        if newAge>maxAge then maxAge:=newAge;
        nameIscommonPrefix:=(commonPrefix=givenName);
        commonPrefix:=fileInfo.commonPrefix(commonPrefix);
        if nameIscommonPrefix then givenName:=commonPrefix;
      end;
    end;
  end;

PROCEDURE T_dbEntry.addNewFile(VAR fileInfo: T_fileInfo; VAR tagList:T_tagList);
  VAR ext:string;
      i:longint;
  begin
    if fileInfo.isExistent then begin
      addNewFileWithoutTouchingTags(fileInfo);
      ext:=fileInfo.getNormalizedExtension;
      for i:=0 to length(C_extToTagMap)-1 do if C_extToTagMap[i,0]=ext then
        addTag(C_extToTagMap[i,1],tagList);
    end;
  end;

PROCEDURE T_dbEntry.dropThumb;
  begin
    if fileExists(getThumbName) then DeleteFile(getThumbName);
    if thumb.loaded then begin
      thumb.picture.clear;
      thumb.loaded:=false;
    end;
  end;

PROCEDURE T_dbEntry.loadThumb;
  begin
    if fileExists(getThumbName) then try
      if thumb.picture=nil then thumb.picture:=TPicture.create;
      thumb.picture.loadFromFile(getThumbName);
      thumb.loaded:=true;
    except
      thumb.picture.clear;
      thumb.loaded:=false;
    end;
  end;

PROCEDURE T_dbEntry.dropNonexistentFiles;
  VAR i,j:longint;
  begin
    i:=0;
    while i<length(input) do if input[i].isExistent then inc(i) else begin
      input[i].destroy;
      for j:=i to length(input)-2 do input[j]:=input[j+1];
      setLength(input,length(input)-1);
    end;
    i:=0;
    while i<length(images) do if images[i].isExistent then inc(i) else begin
      images[i].destroy;
      for j:=i to length(images)-2 do images[j]:=images[j+1];
      setLength(images,length(images)-1);
    end;
    updateAutomaticFields;
  end;

PROCEDURE T_dbEntry.returnToWorkInProgress;
  VAR i:longint;
  begin
    for i:=0 to length(input)-1 do begin input[i].moveToPath(C_workInProgressPath);input[i].destroy; end;
    setLength(input,0);
    for i:=0 to length(images)-1 do begin images[i].moveToPath(C_workInProgressPath); images[i].destroy; end;
    setLength(images,0);
  end;

PROCEDURE T_dbEntry.copyToWorkInProgress;
  VAR i:longint;
  begin
    for i:=0 to length(input)-1 do input[i].copyToPath(C_workInProgressPath);
  end;

FUNCTION T_dbEntry.containsFileLike(s: string): boolean;
  VAR i:longint;
  begin
    result:=false;
    s:=uppercase(s);
    for i:=0 to length(input)-1 do result:=result or (pos(s,uppercase(input[i].fileName))>0);
    if not(result) then for i:=0 to length(images)-1 do result:=result or (pos(s,uppercase(images[i].fileName))>0);
  end;

PROCEDURE T_dbEntry.showPrimary(VAR dispatcher: T_dispatcher);
  begin
    if length(images)>0 then images[0].showFile(dispatcher);
  end;

{ T_tagList }

CONSTRUCTOR T_tagList.create;
  begin
    setLength(list,0);
  end;

DESTRUCTOR T_tagList.destroy;
  begin
    setLength(list,0);
  end;

FUNCTION T_tagList.loadFromFile(VAR F: T_file): boolean;
  VAR i,len:longint;
  begin
    len:=f.readLongint;
    if len<0 then result:=false
    else begin
      result:=true;
      setLength(list,len);
      for i:=0 to len-1 do list[i]:=f.readAnsiString;
    end;
  end;

PROCEDURE T_tagList.saveToFile(VAR F: T_file);
  VAR i:longint;
  begin
    f.writeLongint(length(list));
    for i:=0 to length(list)-1 do f.writeAnsiString(list[i]);
  end;

FUNCTION T_tagList.tagIndex(tagString:string; allowCreation:boolean):longint;
  begin
    result:=0;
    while (result<length(list)) and (list[result]<>tagString) do inc(result);
    if result>=length(list) then begin
      if allowCreation then begin
        setLength(list,result+1);
        list[result]:=tagString;
      end else result:=-1;
    end;
  end;

PROCEDURE T_tagList.getTags(VAR indexes:T_indexSet; strings:TStrings);
  VAR i,imax:longint;
  begin
    imax:=indexes.maxEntryIndex;
    if length(list)<=imax then imax:=length(list)-1;
    strings.clear;
    for i:=0 to imax do if indexes[i] then strings.add(list[i]);
  end;

FUNCTION T_tagList.getTagsForList(VAR indexes:T_indexSet):ansistring;
  VAR i,imax:longint;
  begin
    imax:=indexes.maxEntryIndex;
    if length(list)<=imax then imax:=length(list)-1;
    result:='';
    for i:=0 to imax do if indexes[i] then result:=result+BoolToStr(result='','',', ')+list[i];
  end;

PROCEDURE T_tagList.getTagsForDropDown(strings: TStrings);
  VAR i:longint;
  begin
    strings.clear;
    for i:=0 to length(list)-1 do strings.append('#'+list[i]);
  end;


CONSTRUCTOR T_fileInfo.create(filePath:ansistring);
  FUNCTION startsWith(s:string):boolean;
    begin
      result:=copy(uppercase(filePath),1,length(s))=uppercase(s);
    end;

  VAR directory,fileName:string;
  begin
     directory:=extractFilePath(filePath);
     fileName :=extractFileName(fileName);





    fileName:=name;
    xRes:=-2;
    yRes:=-2;
    lastCheckedAtAge:=0;
  end;

DESTRUCTOR T_fileInfo.destroy;
  begin
  end;

FUNCTION T_fileInfo.getName:ansistring;
  begin
    result:=fileName;
  end;

FUNCTION T_fileInfo.getAge:double;
  begin
    fileAge(fileName,result);
  end;

FUNCTION T_fileInfo.isExistent:boolean;
  begin
    result:=fileExists(fileName);
  end;

FUNCTION T_fileInfo.isImage:boolean;
  VAR ext:string;
  begin
    if xRes=-1 then result:=false
    else if xRes>=0 then result:=true
    else begin
      ext:=getNormalizedExtension;
      result:=(ext='.BMP')  or (ext='.JPG')
           or (ext='.PNG')  or (ext='.ICO')
                            or (ext='.GIF');
    end;
  end;

PROCEDURE T_fileInfo.getResolution(OUT width,height:longint);
  begin
    if xRes=-1 then begin
      width:=0;
      height:=0;
    end else begin
      if (xRes=-2) or (getAge>lastCheckedAtAge) then begin
        xRes:=-2;
        if isImage
          then resolutionOfImage(fileName,xRes,yRes)
          else begin xRes:=-1; yRes:=-1; end;
        lastCheckedAtAge:=getAge;
      end;
      width:=xRes;
      height:=yRes;
    end;
  end;

FUNCTION T_fileInfo.getResolutionString:string;
  VAR x,y:longint;
  begin
    getResolution(x,y);
    result:=intToStr(x)+'x'+intToStr(y);
  end;

FUNCTION  T_fileInfo.loadFromFile(VAR F:T_file):boolean;
  begin
    fileName:=f.readAnsiString;
    xRes:=f.readLongint;
    yRes:=f.readLongint;
    lastCheckedAtAge:=f.readDouble;
    result:=true;
  end;

PROCEDURE T_fileInfo.saveToFile(VAR F:T_file);
  begin
    f.writeAnsiString(fileName);
    f.writeLongint(xRes);
    f.writeLongint(yRes);
    f.writeDouble(lastCheckedAtAge);
  end;

FUNCTION T_fileInfo.getGroupName:string;
  FUNCTION stripResolutionSuffix(s:string):string;
    VAR i:longint;
    begin
      i:=length(s);
      while (i>=1) and (s[i] in ['0'..'9']) do dec(i);
      if (i>=1) and (i<length(s)) and (s[i+1] in ['0'..'9']) and (s[i]='x') then begin
        dec(i);
        while (i>=1) and (s[i] in ['0'..'9']) do dec(i);
        while (i>=1) and (i<length(s)) and (s[i+1] in ['0'..'9']) and (s[i]='_') do dec(i);
      end else i:=length(s);
      result:=copy(s,1,i);

      if uppercase(copy(result,length(result)-2,3))='_LQ' then result:=copy(result,1,length(result)-3);
    end;

  begin
    result:=stripResolutionSuffix(ExtractFileNameOnly(fileName));
  end;

PROCEDURE T_fileInfo.showFile(VAR dispatcher: T_dispatcher);
  VAR ext:string;
      i:longint;
  begin
    if isImage then begin
      dispatcher.startImmediate(fileName,false);
    end else begin
      ext:=uppercase(extractFileExt(fileName));
      i:=0;
      while (i<length(C_extToTagMap)) and (ext<>C_extToTagMap[i,0]) do inc(i);
      if i<length(C_extToTagMap) then
        dispatcher.startImmediate(replacePath(C_extToTagMap[i,3],getPath,extractFileName(fileName)),true);
    end;
  end;

FUNCTION T_fileInfo.commonPrefix(otherPrefix:string):string;
  VAR i:longint;
  begin
    result:=getGroupName;
    i:=1;
    while (i<=length(result)) and (i<=length(otherPrefix)) and (uppercase(result[i])=uppercase(otherPrefix[i])) do inc(i);
    result:=copy(result,1,i-1);
  end;

FUNCTION T_fileInfo.nameWithNewPrefix(oldPrefixLength: longint; newPrefix: string): string;
  begin
    result:=newPrefix+copy(ExtractFileNameOnly(fileName),oldPrefixLength+1,65535)+extractFileExt(fileName);
  end;

PROCEDURE T_fileInfo.delete;
  begin
    DeleteFile(fileName);
  end;

PROCEDURE T_fileInfo.moveToPath(path:ansistring);
  FUNCTION stripCounterSuffix(s:string):string;
    VAR i:longint;
    begin
      i:=length(s);
      if (i>2) and (s[i] in ['0'..'9']) and (s[i-1]='_') then result:=copy(s,1,length(s)-1) else result:=s;
    end;

  VAR rawName,newName,fileExt:ansistring;
      i:longint;
  begin
    fileExt:=extractFileExt(fileName);
    rawName:=stripCounterSuffix(ExtractFileNameOnly(fileName));
    writeln('splitting "',fileName,'" to "',rawName,'" and "',fileExt,'"');
    newName:=path+DirectorySeparator+rawName+fileExt;
    writeln('newname is "',newName,'"');
    if uppercase(newName)<>fileName then begin
      if fileExists(newName) then begin
        i:=-1;
        repeat
          inc(i);
          writeln('newname is "',newName,'"');
          newName:=path+DirectorySeparator+rawName+'_'+intToStr(i)+fileExt;
        until not(fileExists(newName));
      end;
      if CopyFile(fileName,newName,true) then begin
        delete;
        fileName:=newName;
      end;
    end;
  end;

PROCEDURE T_fileInfo.copyToPath(path: ansistring);
  FUNCTION stripCounterSuffix(s:string):string;
    VAR i:longint;
    begin
      i:=length(s);
      if (i>2) and (s[i] in ['0'..'9']) and (s[i-1]='_') then result:=copy(s,1,length(s)-1) else result:=s;
    end;

  VAR rawName,newName,fileExt:ansistring;
      i:longint;
  begin
    fileExt:=extractFileExt(fileName);
    rawName:=stripCounterSuffix(ExtractFileNameOnly(fileName));
    newName:=path+DirectorySeparator+rawName+fileExt;
    if uppercase(newName)<>fileName then begin
      if fileExists(newName) then begin
        i:=-1;
        repeat
          inc(i);
          newName:=path+DirectorySeparator+rawName+'_'+intToStr(i)+fileExt;
        until not(fileExists(newName));
      end;
      CopyFile(fileName,newName,true);
    end;
  end;

PROCEDURE T_fileInfo.rename(nameWithoutPath: ansistring);
  VAR path,newName:ansistring;
  begin
    path:=extractFilePath(fileName);
    newName:=path+nameWithoutPath;
    if (uppercase(newName)<>fileName) and not(fileExists(newName)) then begin
      if CopyFile(fileName,newName,true) then begin
        delete;
        fileName:=newName;
      end;
    end;
  end;

FUNCTION T_fileInfo.getPath:ansistring;
  begin
    result:=extractFilePath(fileName);
  end;

FUNCTION T_fileInfo.getSubPath: ansistring;
  begin
    result:=getPath;
    if      copy(result,1,length(C_imagePath)+1)=C_imagePath+DirectorySeparator then result:=copy(result,length(C_imagePath)+2,length(result))
    else if copy(result,1,length(C_inputPath)+1)=C_inputPath+DirectorySeparator then result:=copy(result,length(C_inputPath)+2,length(result));
  end;

FUNCTION T_fileInfo.getNormalizedExtension: ansistring;
  begin
    result:=uppercase(extractFileExt(fileName));
  end;

INITIALIZATION
  if not DirectoryExists(C_thumbnailPath) then CreateDir(C_thumbnailPath);
  if not DirectoryExists(C_imagePath) then CreateDir(C_imagePath);
  if not DirectoryExists(C_inputPath) then CreateDir(C_inputPath);
  if not DirectoryExists(C_workInProgressPath) then CreateDir(C_workInProgressPath);
end.
