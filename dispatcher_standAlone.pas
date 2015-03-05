PROGRAM dispatcher_standAlong;
USES sysutils,Process,uniqueinstanceraw,windows,dispatcher;
VAR myDispatcher:T_dispatcher;
    cmdLineParams:array of ansistring;

PROCEDURE parseCommandLine;
  //FUNCTION replace(full,sub,replacement:ansistring):ansistring;
  //  VAR i:longint;
  //  begin
  //    result:='';
  //    i:=pos(sub,full);
  //    while i>0 do begin
  //      result:=result+copy(full,1,i-1)+replacement;
  //      full:=copy(full,i+length(sub),length(full));
  //      i:=pos(sub,full);
  //    end;
  //    result:=result+full;
  //  end;

  VAR i:longint;
      readText:ansistring;
  begin
    readText:='';
    for i:=0 to length(cmdLineParams)-1 do readText:=readText+cmdLineParams[i]+' ';
    myDispatcher.appendTask(readText,true,ppIdle);
    writeln('task "',readText,'" pending');
  end;

PROCEDURE copyCommandLine;
  VAR i:longint;
  begin
    setLength(cmdLineParams,paramcount);
    for i:=1 to paramcount do cmdLineParams[i-1]:=paramstr(i);
    parseCommandLine;
  end;

FUNCTION catchCommandLine:boolean;
  VAR caughtName,subName:ansistring;
  begin
    if FIPCServer.peekMessage(10,true) then begin
      setLength(cmdLineParams,0);
      caughtName:=FIPCServer.stringMessage;
      while length(caughtName)>0 do begin
        subName   :=copy(caughtName,1,pos('|',caughtName)-1);
        caughtName:=copy(caughtName,  pos('|',caughtName)+1,length(caughtName));
        setLength(cmdLineParams,length(cmdLineParams)+1);
        cmdLineParams[length(cmdLineParams)-1]:=subname;
      end;
      parseCommandLine;
      result:=true;
    end else result:=false;
  end;

VAR sleepTime:longint=0;
begin
  if not(InstanceRunning('dispatcher',true)) then begin
    myDispatcher.create(4);
    copyCommandLine;
    repeat
      if myDispatcher.startSpawns or catchCommandLine then sleepTime:=10 else if sleepTime<5000 then inc(sleepTime,10);
      sleep(sleepTime);
    until false;
  end else halt(1);
end.
