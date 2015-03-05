UNIT sysTools;
INTERFACE
USES process,classes,sysutils;
TYPE multiSearchRec=array of TSearchRec;
     T_log=object
       private
         immediateAppender:boolean;
         fname:ansistring;
         handle:text;
       public
       CONSTRUCTOR createLinewiseLog(filename:ansistring; appendToExistingFile:boolean);
       CONSTRUCTOR createNormalLog(filename:ansistring; appendToExistingFile:boolean);
       DESTRUCTOR destroy;
       PROCEDURE logLine(l:ansistring);
     end;

FUNCTION runCommand(commandToExecute:string; OUT output:TStringList):boolean;
FUNCTION simpleOutputOfCommand(commandToExecute:string):string;
FUNCTION runCommandAndReturnConcatenatedOutput(commandToExecute:string):ansistring;
FUNCTION windowsUserName:string;
FUNCTION windowsLocalAppData:string;
FUNCTION windowsProgramData:string;
FUNCTION windowsWorkload:longint;
FUNCTION isValidFilename(CONST Filename: String; CONST requirePathExistence:boolean=true) : Boolean;
FUNCTION dateToSortable(t:TDateTime):ansistring;
FUNCTION findAll(searchPattern:string):multiSearchRec;
PROCEDURE deleteMyselfOnExit;

//FUNCTION spawnProcessInNewConsole(commandToExecute:string):longint;
//PROCEDURE checkSpawns;
OPERATOR +(x,y:multiSearchRec):multiSearchRec;
IMPLEMENTATION
OPERATOR +(x,y:multiSearchRec):multiSearchRec;
  VAR i:longint;
  begin
    setLength(result,length(x)+length(y));
    for i:=0 to length(x)-1 do result[i]:=x[i];
    for i:=0 to length(y)-1 do result[i+length(x)]:=y[i];
  end;

FUNCTION isValidFilename(CONST Filename: String; CONST requirePathExistence:boolean=true) : Boolean;
  CONST ForbiddenChars  : set of Char = ['<', '>', '|', '"', '\', ':', '*', '?'];
  VAR i:integer;
      name,path:string;
  begin
    if requirePathExistence then begin
      path:=ExtractFilePath(filename);
      name:=ExtractFileName(filename);
      result:=(name<>'') and (DirectoryExists(path));
      for i:=1 to length(name)-1 do result:=result and not(name[i] in ForbiddenChars) and not(name[i]='\');
    end else begin
      name:=Filename;
      result:=(filename<>'');
      for i:=1 to length(name)-1 do result:=result and not(name[i] in ForbiddenChars);
    end;
  end;

FUNCTION runCommand(commandToExecute:string; OUT output:TStringList):boolean;
  CONST READ_BYTES = 2048;
  VAR memStream: TMemoryStream;
      tempProcess: TProcess;
      n: longint;
      BytesRead: longint;
  begin
    memStream := TMemoryStream.create;
    BytesRead := 0;
    tempProcess := TProcess.create(nil);
    tempProcess.commandLine:='cmd /C '+commandToExecute;
    tempProcess.Options := [poUsePipes,poStderrToOutPut];
    tempProcess.ShowWindow:=swoHIDE;
    //tempProcess.ShowWindow:=swoNone;
    try
      tempProcess.Execute;
      while tempProcess.Running do begin
        memStream.SetSize(BytesRead + READ_BYTES);
        n := tempProcess.Output.Read((memStream.Memory + BytesRead)^, READ_BYTES);
        if n>0  then Inc(BytesRead, n) else Sleep(10);
      end;
      repeat
        memStream.SetSize(BytesRead + READ_BYTES);
        n := tempProcess.Output.Read((memStream.Memory + BytesRead)^, READ_BYTES);
        if n > 0 then Inc(BytesRead, n);
      until n <= 0;
      result:=(tempProcess.ExitStatus=0);
    except
      result:=false;
    end;
    tempProcess.Free;
    memStream.SetSize(BytesRead);
    output := TStringList.create;
    output.LoadFromStream(memStream);
    memStream.Free;
  end;

FUNCTION simpleOutputOfCommand(commandToExecute:string):string;
  VAR resultStrings:TStringlist;
  begin
    if runCommand(commandToExecute,resultStrings) and (resultStrings.Count>0)
      then result:=resultStrings[0]
      else result:='';
    resultStrings.Free;
  end;

FUNCTION runCommandAndReturnConcatenatedOutput(commandToExecute:string):ansistring;
  VAR resultStrings:TStringlist;
      i:longint;
  begin
    result:='';
    if runCommand(commandToExecute,resultStrings) then begin
      for i:=0 to resultStrings.Count-1 do begin
        if i>0 then result:=result+chr(10);
        result:=result+resultStrings[i];
      end;
    end;
    resultStrings.Free;
  end;

FUNCTION windowsUserName:string;
  begin result:=simpleOutputOfCommand('echo %USERNAME%'); end;

FUNCTION windowsLocalAppData:string;
  begin result:=simpleOutputOfCommand('echo %LOCALAPPDATA%'); end;

FUNCTION windowsProgramData:string;
  begin result:=simpleOutputOfCommand('echo %PROGRAMDATA%'); end;

VAR lastTimeCheckedForWorkload:double=0;
    lastWorkload:longint=100;
    workloadPollRunning:boolean=false;

FUNCTION continuouslyPollWorkload(p:pointer): ptrint;
  VAR tmp:longint;
  VAR resultStrings:TStringlist;
      i:longint;
  begin
    while true do begin
      tmp:=-1;    
      if runCommand('wmic cpu get loadpercentage',resultStrings) then begin
        for i:=0 to resultStrings.Count-1 do if (tmp<0) then tmp:=strToIntDef(trim(resultStrings[i]),-1);
        resultStrings.free;
      end;      
      if tmp<0 then lastWorkload:=0
               else lastWorkload:=tmp;
      sleep(1000);
    end;
  end;
    
FUNCTION windowsWorkload:longint;
  begin
    if not(workloadPollRunning) then begin
      workloadPollRunning:=true;
      beginThread(@continuouslyPollWorkload);
    end;
    result:=lastWorkload;
  end;

FUNCTION dateToSortable(t:TDateTime):ansistring;
  begin
    DateTimeToString(result,'YYYYMMDD_HHmmss',t);
  end;

FUNCTION findAll(searchPattern:string):multiSearchRec;
  VAR s:TSearchRec;
  begin
    setLength(result,0);
    if FindFirst(searchPattern,faAnyFile,s)=0 then repeat
      setLength(result,length(result)+1);
      result[length(result)-1]:=s;
    until FindNext(s)<>0;
    FindClose(s);
  end;

PROCEDURE deleteMyselfOnExit;
  VAR handle:text;
      batName:string;
      counter:longint;
      proc:TProcess;
  begin
    counter:=0;
    repeat
      batName:=paramstr(0)+'delete'+intToStr(counter)+'.bat';
      inc(counter);
    until not(fileExists(batName));
    assign(handle,batName);
    rewrite(handle);
    writeln(handle,':Repeat');
    writeln(handle,'@ping -n 2 127.0.0.1 > NUL');
    writeln(handle,'@del "',paramstr(0),'"');
    writeln(handle,'@if exist "',paramstr(0),'" goto Repeat');
    writeln(handle,'@del %0');
    close(handle);
    proc:=TProcess.create(nil);
    proc.CommandLine:='cmd /C '+batName;
    proc.execute;
  end;

CONSTRUCTOR T_log.createLinewiseLog(filename:ansistring; appendToExistingFile:boolean);
  begin
    immediateAppender:=true;
    fname:=filename;
    assign(handle,fname);
    if appendToExistingFile then begin
      if not(FileExists(filename)) then begin
        rewrite(handle);
        close(handle);
      end;
    end else begin
      if FileExists(filename) then begin
        rewrite(handle);
        close(handle);
      end;
    end;
  end;

CONSTRUCTOR T_log.createNormalLog(filename:ansistring; appendToExistingFile:boolean);
  begin
    immediateAppender:=false;
    fname:=filename;
    assign(handle,fname);
    if appendToExistingFile and FileExists(filename)
      then append(handle)
      else rewrite(handle);
  end;

DESTRUCTOR T_log.destroy;
  begin
    if not(immediateAppender) then close(handle);
  end;

PROCEDURE T_log.logLine(l:ansistring);
  begin
    if immediateAppender then append(handle);
    writeln(l);
    writeln(handle,l);
    if immediateAppender then close(handle);
  end;

end.
