UNIT dbFiles;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils,myFiles,mypics,FileUtil,myGenerics,Graphics,windows,queues;
TYPE T_parentPath=(pp_none,pp_images,pp_thumbnails,pp_wip,pp_deleted);
     T_structuredPath=record
       parentPath:T_parentPath;
       subPath   :string;
       fileName  :string;
     end;
     T_thumbnailState=(ts_unknown,ts_unavailable,ts_nonexistent,ts_generating,ts_unloaded,ts_ready);

     T_executableCall=record
       cmd:string;
       param:string;
     end;

CONST C_defaultFileTimeIfNotFound=0;
      C_thumbnailMaxWidthHeight=128;
      C_previewMaxWidthHeight=512;
      C_parentPath:array[T_parentPath] of string=(
        '',
        'images',
        'thumbnails',
        'wip',
        'deleted');
      PATH_TO_EDITOR='"c:\Program Files (x86)\ConTEXT\ConTEXT.exe"';

      C_extMap  :array[0..6] of record
        ext:string;
        tag:string;
        viewCall:T_executableCall;
      end   =
        ((ext:'.BAT'  ; tag:'manipulated'  ; viewCall:(cmd:PATH_TO_EDITOR;   param:'"%path%file"')),
         (ext:'.PAS'  ; tag:'stand alone'  ; viewCall:(cmd:'cmd';            param:'/C start cmd /C "%path%file" ')),
         (ext:'.EXE'  ; tag:'stand alone'  ; viewCall:(cmd:'';               param:'')),
         (ext:'.PARAM'; tag:'ifs'          ; viewCall:(cmd:'ifs.exe';        param:'"%path%file"')),
         (ext:'.JOB'  ; tag:'relief'       ; viewCall:(cmd:'reliefs.exe';    param:'"%path%file" -d')),
         (ext:'.ECJ'  ; tag:'expo cloud'   ; viewCall:(cmd:'expoClouds.exe'; param:'"%path%file" -d')),
         (ext:'.FTJ'  ; tag:'function tree'; viewCall:(cmd:'functrees.exe';  param:'"%path%file" -d')));


      C_imageExt:array[0..4] of string=('.BMP','.JPG','.PNG','.ICO','.GIF');
      C_sourceExt:array[0..1] of string=('.BAT','.PAS');

OPERATOR :=(x:string):T_structuredPath;
OPERATOR :=(x:T_structuredPath):string;
FUNCTION modParent(sPath:T_structuredPath; parentPath:T_parentPath):T_structuredPath;
FUNCTION moveFile(oldPath,newPath:T_structuredPath):boolean;
FUNCTION CopyFile(oldPath,newPath:T_structuredPath):boolean;
PROCEDURE ensurePath(sPath:T_structuredPath);
FUNCTION commonPath(p1,p2:T_structuredPath):T_structuredPath;
FUNCTION stripExtAndResolutionSuffix(s:string):string;
//PROCEDURE generateThumbnails();
PROCEDURE generateRandomThumbnail(count:longint);

TYPE

  { T_fileInfo }

  P_fileInfo=^T_fileInfo;
  T_fileInfo=object(T_serializable)
    private
      id:longint;
      sPath:T_structuredPath;
      xRes,yRes:longint; //-2: never checked; -1: no image
      lastCheckedAtAge:double;
      thumb:record
        createdForFileTime:double;
        state:T_thumbnailState; //non-persistent
        picture:TPicture; //non-persistent
      end;
    public
    CONSTRUCTOR create(filePath:ansistring);
    DESTRUCTOR destroy;
    PROCEDURE normalizePath;
    FUNCTION getPath:T_structuredPath;
    FUNCTION getNameAsString:ansistring;
    FUNCTION getAge:double;
    FUNCTION isExistent:boolean;
    FUNCTION isImage:boolean;
    FUNCTION isSource:boolean;
    FUNCTION getResolution(OUT width,height:longint):longint;
    FUNCTION  loadFromFile(VAR F:T_file):boolean; virtual; overload;
    PROCEDURE saveToFile(VAR F:T_file);           virtual; overload;
    FUNCTION  copyTo(newPath: T_structuredPath):boolean;
    FUNCTION  moveTo(newPath: T_structuredPath):boolean;
    FUNCTION  moveTo(newParent:T_parentPath):boolean;
    FUNCTION  canMoveTo(newPath:T_structuredPath):boolean;
    PROCEDURE delete;
    FUNCTION getNormalizedExtension: ansistring;
    FUNCTION getGroupName:ansistring;
    FUNCTION getMatchName:ansistring;
    PROCEDURE showFile();
    FUNCTION getInfoString:string;
    PROCEDURE generateThumbnail();
    FUNCTION getThumbName:string;
    FUNCTION getPreviewName:string;
    FUNCTION getThumbState:T_thumbnailState;
    FUNCTION getThumb:TPicture;
    PROCEDURE dropThumbnail;
  end;

  T_fileInfoList=array of P_fileInfo;

OPERATOR =(x,y:T_fileInfo):boolean;
FUNCTION scanForNewFiles(OUT anyChange:boolean):T_fileInfoList;
FUNCTION lastRescan:double;
FUNCTION getPendingCount:longint;

IMPLEMENTATION
VAR allFiles:specialize G_stringKeyMap<P_fileInfo>;
    nextFileId:longint=0;
    lastScanHash:longint=maxLongint;
    lastRescanTime:double=0;

FUNCTION getPendingCount:longint; begin result:=pendingTasks.queuedCount; end;

PROCEDURE updateResolutionAndCalcImages(CONST p:pointer);
  VAR img:T_24BitImage;
  begin
    with P_fileInfo(p)^ do if isImage and FileExistsUTF8(getNameAsString) then begin
      if thumb.picture<>nil then thumb.picture.clear;
      ensurePath(getThumbName);
      ensurePath(getPreviewName);
      if sysutils.fileExists(getThumbName) then sysutils.DeleteFile(getThumbName);
      if sysutils.fileExists(getPreviewName) then sysutils.DeleteFile(getPreviewName);
      lastCheckedAtAge:=getAge;
      thumb.createdForFileTime:=lastCheckedAtAge;
      img.create(UTF8ToSys(getNameAsString));
      xRes:=img.width;
      yRes:=img.height;
      if (xRes>0) and (yRes>0) then begin
        img.resize(C_previewMaxWidthHeight,C_previewMaxWidthHeight,1);
        compressionQualityPercentage:=90;
        img.saveToFile(getPreviewName);
        img.cropResize(C_thumbnailMaxWidthHeight,C_thumbnailMaxWidthHeight);
        compressionQualityPercentage:=90;
        img.saveToFile(getThumbName);
        thumb.state:=ts_unloaded;
      end else thumb.state:=ts_unavailable;
      img.destroy;
    end;
  end;

PROCEDURE disposeFunction (p:pointer);
  begin
  end;

PROCEDURE startCalculationOrAddToPending(fileInfo:P_fileInfo);
  begin
    fileInfo^.thumb.state:=ts_generating;
    pendingTasks.enqueue(fileInfo);
  end;

PROCEDURE disposeFileInfo(VAR f: P_fileInfo);
  begin
    while (f^.thumb.state=ts_generating) do sleep(10);
    dispose(f,destroy);
  end;

OPERATOR:=(x: string): T_structuredPath;
  begin
    result.parentPath:=pp_none;
    if copy(x,1,length(C_parentPath[pp_images    ]))=C_parentPath[pp_images    ] then result.parentPath:=pp_images;
    if copy(x,1,length(C_parentPath[pp_thumbnails]))=C_parentPath[pp_thumbnails] then result.parentPath:=pp_thumbnails;
    if copy(x,1,length(C_parentPath[pp_wip       ]))=C_parentPath[pp_wip       ] then result.parentPath:=pp_wip;
    x:=copy(x,length(C_parentPath[result.parentPath])+1,length(x));
    result.subPath:=extractFilePath(x);
    result.fileName:=extractFileName(x);
  end;

OPERATOR:=(x: T_structuredPath): string;
  begin
    result:=C_parentPath[x.parentPath]+x.subPath+x.fileName;
  end;

FUNCTION modParent(sPath:T_structuredPath; parentPath:T_parentPath):T_structuredPath;
  begin
    result:=sPath;
    result.parentPath:=parentPath;
  end;

FUNCTION moveFile(oldPath, newPath: T_structuredPath): boolean;
  begin
    if string(oldPath)=string(newPath) then exit(true);
    if CopyFile(oldPath,newPath) then begin
      sysutils.DeleteFile(UTF8ToSys(oldPath));
      result:=true;
    end else result:=false;
  end;

FUNCTION CopyFile(oldPath, newPath: T_structuredPath): boolean;
  begin
    if FileExistsUTF8(oldPath) then begin
      ensurePath(UTF8ToSys(newPath));
      result:=FileUtil.CopyFile(oldPath,newPath,true);
    end else result:=false;
  end;

PROCEDURE ensurePath(sPath: T_structuredPath);
  VAR path:string;
      newDir:string;
      ensuredDir:string;
      p:longint;
  begin
    ensuredDir:='';
    path:=C_parentPath[sPath.parentPath]+sPath.subPath;
    if path[length(path)]=DirectorySeparator then path:=copy(path,1,length(path)-1);
    while path<>'' do begin
      p:=pos(DirectorySeparator,path);
      if p<=0 then begin
        newDir:=path;
        path:='';
      end else begin
        newDir:=copy(path,1,p-1);
        path:=copy(path,p+1,length(path)-p);
      end;
      if ensuredDir='' then ensuredDir:=newDir
                       else ensuredDir:=ensuredDir+DirectorySeparator+newDir;
      CreateDir(ensuredDir);
    end;
  end;

FUNCTION commonPath(p1, p2: T_structuredPath): T_structuredPath;
  VAR n:longint;
  begin
    if p1.parentPath=p2.parentPath
      then result.parentPath:=p1.parentPath
      else result.parentPath:=pp_none;
    if p1.subPath=p2.subPath
      then result.subPath:=p1.subPath
      else result.subPath:=DirectorySeparator;
    p1.fileName:=stripExtAndResolutionSuffix(p1.fileName);
    p2.fileName:=stripExtAndResolutionSuffix(p2.fileName);
    n:=1;
    while (n<=length(p1.fileName)) and
          (n<=length(p2.fileName)) and (uppercase(p1.fileName[n])=uppercase(p2.fileName[n])) do inc(n);
    result.fileName:=copy(p1.fileName,1,n+1);
  end;

FUNCTION stripExtAndResolutionSuffix(s:string):string;
  VAR i:longint;
  begin
    s:=ChangeFileExt(s,'');
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

PROCEDURE generateRandomThumbnail(count:longint);
  VAR fileList:array of P_fileInfo;
      i,k:longint;
  begin
    fileList:=allFiles.valueSet;
    i:=0;
    for k:=1 to count do begin
      while (i<length(fileList)) and not(fileList[i]^.getThumbState in [ts_unknown,ts_nonexistent,ts_generating]) do inc(i);
      if (i<length(fileList)) then inc(i);
    end;
  end;

OPERATOR=(x, y: T_fileInfo): boolean;
  begin
    result:=(x.sPath.parentPath=y.sPath.parentPath)
        and (x.sPath.fileName  =y.sPath.fileName)
        and (x.sPath.subPath   =y.sPath.subPath);
  end;

FUNCTION scanForNewFiles(OUT anyChange: boolean): T_fileInfoList;
  VAR thisHash:longint;
      fileNames:TStrings;
      i:longint;
      fileInfoDummy:P_fileInfo;
  begin
    lastRescanTime:=now;
    thisHash:=0;
    fileNames:=FindAllFiles(C_parentPath[pp_images]);
    {$Q-}
    for i:=0 to fileNames.count-1 do thisHash:=thisHash*31+hashOfAnsiString(fileNames[i]);
    {$Q+}
    setLength(result,0);
    if thisHash=lastScanHash then anyChange:=false
    else begin
      anyChange:=true;
      lastScanHash:=thisHash;
      for i:=0 to fileNames.count-1 do
        if not(allFiles.containsKey(fileNames[i],fileInfoDummy)) then begin
          setLength(result,length(result)+1);
          new(result[length(result)-1],create(fileNames[i]));
        end;
    end;
    fileNames.destroy;
  end;

FUNCTION lastRescan: double;
  begin result:=lastRescanTime; end;

CONSTRUCTOR T_fileInfo.create(filePath:ansistring);
  begin
    sPath:=filePath;
    xRes:=-2;
    yRes:=-2;
    lastCheckedAtAge:=0;
    if filePath<>'' then begin
      normalizePath;
      allFiles.put(filePath,@self);
      id:=nextFileId; inc(nextFileId);
    end;
    thumb.state:=ts_unknown;
  end;

DESTRUCTOR T_fileInfo.destroy;
  begin
    //while thumb.state=ts_generating do begin
    //  writeln('waiting for task on ',getNameAsString);
    //  sleep(100);
    //end;
    if thumb.picture<>nil then thumb.picture.destroy;
    allFiles.dropKey(ansistring(sPath));
  end;

PROCEDURE T_fileInfo.normalizePath;
  begin
    sPath:=extractRelativePath(extractFilePath(paramStr(0)),sPath);
  end;

FUNCTION T_fileInfo.getPath: T_structuredPath;
  begin result:=sPath; end;

FUNCTION T_fileInfo.getNameAsString: ansistring;
  begin result:=sPath; end;

FUNCTION T_fileInfo.getAge:double;
  begin
    if FileExistsUTF8(sPath)
      then fileAge(UTF8ToSys(sPath),result)
      else result:=C_defaultFileTimeIfNotFound;
  end;

FUNCTION T_fileInfo.isExistent:boolean;
  begin
    result:=FileExistsUTF8(sPath);
  end;

FUNCTION T_fileInfo.isImage:boolean;
  VAR ext:string;
      i:longint;
  begin
    if xRes=-1 then result:=false
    else if xRes>=0 then result:=true
    else begin
      ext:=getNormalizedExtension;
      for i:=0 to length(C_imageExt)-1 do if ext=C_imageExt[i] then exit(true);
      result:=false;
    end;
  end;

FUNCTION T_fileInfo.isSource: boolean;
  VAR i:longint;
      ext:string;
  begin
    ext:=getNormalizedExtension;
    for i:=0 to length(C_sourceExt)-1 do if ext=C_sourceExt[i] then exit(true);
    result:=false;
  end;

FUNCTION T_fileInfo.getResolution(OUT width, height: longint): longint;
  FUNCTION fileLines(fileName:string):longint;
    VAR handle:text;
        line:string;
    begin
      if fileExists(fileName) then begin
        AssignFile(handle,fileName);
        reset(handle);
        result:=0;
        repeat
          readln(handle,line);
          inc(result);
        until eof(handle);
        CloseFile(handle);
      end else result:=0;
    end;

  begin
    if xRes=-1 then begin
      width:=0;
      height:=yRes;
    end else begin
      if (xRes=-2) or (getAge>lastCheckedAtAge) then begin
        xRes:=-2;
        if isImage then begin
          if not(thumb.state=ts_generating) then startCalculationOrAddToPending(@self);
        end else if isSource then begin
          xRes:=-1;
          yRes:=fileLines(sPath);
          lastCheckedAtAge:=getAge;
        end else begin
          xRes:=-1;
          yRes:=filesize(string(sPath));
          lastCheckedAtAge:=getAge;
        end;
      end;
      width:=xRes;
      height:=yRes;
    end;
    result:=xRes*yRes;
    if result<0 then result:=0;
  end;

FUNCTION T_fileInfo.getInfoString:string;
  VAR x,y:longint;
  begin
    getResolution(x,y);
    if isImage then result:=' @'+intToStr(x)+'x'+intToStr(y)
    else if isSource then result:=' ('+intToStr(y)+'LOC)'
    else                  result:=' ('+intToStr(y)+'bytes)';
  end;

PROCEDURE T_fileInfo.generateThumbnail();
  CONST tenSeconds=10/(24*60*60);
  begin
    if not(isImage) then begin
      thumb.state:=ts_unavailable;
      if sysutils.fileExists(getThumbName) then sysutils.DeleteFile(getThumbName);
      if sysutils.fileExists(getPreviewName) then sysutils.DeleteFile(getPreviewName);
    end else with thumb do if (state<>ts_generating) and (not(fileExists(getThumbName))
    or not(fileExists(getPreviewName))
    or (abs(createdForFileTime-getAge)>tenSeconds)) then begin
      startCalculationOrAddToPending(@self);
    end else if state<>ts_ready then state:=ts_unloaded;
  end;

FUNCTION T_fileInfo.getThumbName: string;
  begin
    result:=C_parentPath[pp_thumbnails]+DirectorySeparator+formatFloat('0000000',id)+'T.jpg';
  end;

FUNCTION T_fileInfo.getPreviewName: string;
  begin
    result:=C_parentPath[pp_thumbnails]+DirectorySeparator+formatFloat('0000000',id)+'P.jpg';
  end;

FUNCTION T_fileInfo.getThumbState: T_thumbnailState;
  begin
    with thumb do if (state in [ts_nonexistent,ts_generating]) and fileExists(getThumbName) then state:=ts_unloaded
    else if thumb.state in [ts_unknown,ts_nonexistent] then generateThumbnail();
    result:=thumb.state;
  end;

FUNCTION T_fileInfo.getThumb: TPicture;
  begin
    with thumb do begin
      if (state=ts_unloaded) and fileExists(getThumbName) then begin
        if picture=nil then picture:=TPicture.create;
        try
          picture.loadFromFile(getThumbName);
          state:=ts_ready;
        except
          state:=ts_unloaded;
          picture.clear;
        end;
      end;
      result:=picture;
    end;
  end;

PROCEDURE T_fileInfo.dropThumbnail;
  begin
    with thumb do begin
      if picture<>nil then picture.clear;
      sysutils.DeleteFile(getThumbName);
      sysutils.DeleteFile(getPreviewName);
      state:=ts_unknown;
      createdForFileTime:=C_defaultFileTimeIfNotFound;
    end;
  end;

FUNCTION  T_fileInfo.loadFromFile(VAR F:T_file):boolean;
  begin
    id:=f.readLongint;
    if id>=nextFileId then nextFileId:=id+1;
    sPath:=f.readAnsiString;
    xRes:=f.readLongint;
    yRes:=f.readLongint;
    lastCheckedAtAge:=f.readDouble;
    thumb.createdForFileTime:=f.readDouble;

    thumb.state:=ts_unknown;
    result:=true;
    allFiles.put(sPath,@self);
  end;

PROCEDURE T_fileInfo.saveToFile(VAR F:T_file);
  begin
    f.writeLongint(id);
    f.writeAnsiString(sPath);
    f.writeLongint(xRes);
    f.writeLongint(yRes);
    f.writeDouble(lastCheckedAtAge);
    f.writeDouble(thumb.createdForFileTime);
  end;

FUNCTION T_fileInfo.copyTo(newPath: T_structuredPath): boolean;
  begin
    result:=CopyFile(sPath,newPath);
  end;

FUNCTION T_fileInfo.moveTo(newPath: T_structuredPath): boolean;
  begin
    result:=moveFile(sPath,newPath);
    if result then sPath:=newPath;
  end;

FUNCTION T_fileInfo.moveTo(newParent: T_parentPath): boolean;
  VAR newPath:T_structuredPath;
  begin
    newPath:=modParent(sPath,newParent);
    if (newParent=pp_wip) then newPath.subPath:=DirectorySeparator;
    result:=moveTo(newPath);
  end;

FUNCTION T_fileInfo.canMoveTo(newPath: T_structuredPath): boolean;
  begin
    result:=(string(newPath)=string(sPath)) or not(FileExistsUTF8(string(newPath)));
  end;

PROCEDURE T_fileInfo.delete;
  begin
    dropThumbnail;
    moveTo(pp_deleted);
  end;

FUNCTION T_fileInfo.getGroupName: ansistring;
  begin
    result:=stripExtAndResolutionSuffix(sPath.fileName);
  end;

FUNCTION T_fileInfo.getMatchName:ansistring;
  begin
    result:=uppercase(sPath.subPath+stripExtAndResolutionSuffix(sPath.fileName));
  end;

FUNCTION T_fileInfo.getNormalizedExtension: ansistring;
  begin
    result:=uppercase(extractFileExt(sPath.fileName));
  end;

PROCEDURE T_fileInfo.showFile();
  FUNCTION replacePath(s:string; path,fname:string):string;
  begin
    path:=UTF8ToSys(path);
    fname:=UTF8ToSys(fname);
    while pos('%path%file',s)>0 do
      s:=copy(s,1,pos('%path%file',s)-1)+path+fname+copy(s,pos('%path%file',s)+10,length(s));
    while pos('%path',s)>0 do
      s:=copy(s,1,pos('%path',s)-1)+path+copy(s,pos('%path',s)+5,length(s));
    while pos('%file',s)>0 do
      s:=copy(s,1,pos('%file',s)-1)+fname+copy(s,pos('%file',s)+5,length(s));
    result:=s;
  end;

  VAR ext:string;
      i:longint;
      com:T_executableCall;
  begin
    if isImage then begin
      ExecuteProcess('cmd','/C start cmd /C "'+UTF8ToSys(getNameAsString)+'"');
    end else begin
      ext:=getNormalizedExtension;
      i:=0;
      while (i<length(C_extMap)) and (ext<>C_extMap[i].ext) do inc(i);
      if i<length(C_extMap) then
        com:=C_extMap[i].viewCall;
        if (com.cmd<>'') then begin
          ExecuteProcess(com.cmd,replacePath(com.param,C_parentPath[sPath.parentPath]+sPath.subPath,getPath.fileName));
        end;
      //  mydispatcher.startImmediate(replacePath(C_extToTagMap[i,3],C_parentPath[sPath.parentPath]+sPath.subPath,getPath.filename),true,ppHigh);
    end;
  end;

INITIALIZATION
  queues.evaluationFunction:=@updateResolutionAndCalcImages;
  queues.disposeFunction:=@disposeFunction;
  allFiles.create;

FINALIZATION
  allFiles.destroy;

end.

