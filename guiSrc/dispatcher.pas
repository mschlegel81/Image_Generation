UNIT dispatcher;
INTERFACE
USES sysutils,Process,myFiles;
TYPE P_dispatcher=^T_dispatcher;
     T_dispatcher=object
       private
         toDoList:array of record
                    cmd:ansistring;
                    ownConsole:boolean;
                  end;
         spawned:array of record
                   proc:TProcess;
                   cmd :ansistring;
                 end;
         barrierEncountered:boolean;
         aimCount :longint;
         PROCEDURE dropFirstToDo;
       public
         CONSTRUCTOR create(procs:longint);
         DESTRUCTOR destroy;
         PROCEDURE appendTask(taskCommand:ansistring; ownConsole:boolean);
         PROCEDURE appendBarrier;

         PROCEDURE limitSpawns(maxCount:longint);
         FUNCTION anySpawnRunning:boolean;
         PROCEDURE startImmediate(taskCommand:ansistring; ownConsole:boolean);
         FUNCTION startSpawns:boolean;

         FUNCTION pendingCount:longint;
         FUNCTION runningCount:longint;

         FUNCTION numberOfCPUs:longint;
         PROCEDURE cancelPendingTasks;

         PROCEDURE printToDoList;
     end;

IMPLEMENTATION
PROCEDURE T_dispatcher.dropFirstToDo;
  VAR i:longint;
  begin
    for i:=0 to length(toDoList)-2 do toDoList[i]:=toDoList[i+1];
    setLength(toDoList,length(toDoList)-1);
  end;

CONSTRUCTOR T_dispatcher.create(procs:longint);
  begin
    barrierEncountered:=false;
    setLength(spawned,0);
    aimCount:=procs;
  end;

DESTRUCTOR T_dispatcher.destroy;
  VAR i:longint;
  begin
    setLength(toDoList,0);
    for i:=0 to length(spawned)-1 do spawned[i].proc.free;
  end;

PROCEDURE T_dispatcher.appendTask(taskCommand:ansistring; ownConsole:boolean);
  begin
    setLength(toDoList,length(toDoList)+1);
    toDoList[length(toDoList)-1].cmd:=taskCommand;
    toDoList[length(toDoList)-1].ownConsole:=ownConsole;
  end;

PROCEDURE T_dispatcher.appendBarrier;
  begin
    appendTask('sync',false);
  end;

PROCEDURE T_dispatcher.limitSpawns(maxCount:longint);
  begin
    if maxCount<0 then maxCount:=0;
    aimCount:=maxCount;
  end;

FUNCTION T_dispatcher.anySpawnRunning:boolean;
  VAR i:longint;
  begin
    result:=false;
    for i:=0 to length(spawned)-1 do result:=result or spawned[i].proc.running;
  end;

FUNCTION titleOf(s:string):string;
  begin
    if length(s)>80 then s:=copy(s,1,80)+'...';
    if pos('&',s)>0 then result:='"'+s+'"'
                    else result:=s;
  end;

PROCEDURE T_dispatcher.startImmediate(taskCommand:ansistring; ownConsole:boolean);
  VAR i:longint;
  begin
    i:=0;
    while (i<length(spawned)) and (spawned[i].proc.running) do inc(i);
    if i>=length(spawned) then begin
      setLength(spawned,i+1);
      spawned[i].proc:=TProcess.create(nil);
      if ownConsole then spawned[i].proc.options:=spawned[i].proc.options + [poNewConsole];
    end;
    if ownConsole then begin
      spawned[i].proc.options:=spawned[i].proc.options + [poNewConsole];
      spawned[i].proc.ShowWindow:=swoMinimize;
    end else begin
      spawned[i].proc.options:=spawned[i].proc.options - [poNewConsole];
      spawned[i].proc.ShowWindow:=swoShowDefault;
    end;
    if poNewConsole in spawned[i].proc.options
      then spawned[i].proc.CommandLine:='cmd /C title '+'S'+intToStr(i)+': '+titleOf(taskCommand)+' & '+taskCommand
      else spawned[i].proc.CommandLine:='cmd /C title '+extractFileName(ChangeFileExt(paramStr(0),''))+' & '+taskCommand;
    spawned[i].cmd:=taskCommand;
    spawned[i].proc.execute;
    writeln('spawn #',i,' processing: ',taskCommand);
  end;

FUNCTION T_dispatcher.startSpawns:boolean;
  begin
    result:=false;
    if barrierEncountered and not(anySpawnRunning) then begin
      writeln('syncinc...');
      barrierEncountered:=false;
    end;
    while not(barrierEncountered) and (runningCount<aimCount) and (length(toDoList)>0) do begin
      if toDoList[0].cmd='sync' then begin
        result:=true;
        barrierEncountered:=true;
        dropFirstToDo;
      end else begin
        result:=true;
        startImmediate(toDoList[0].cmd,toDoList[0].ownConsole);
        dropFirstToDo;
      end;
    end;
  end;

FUNCTION T_dispatcher.pendingCount:longint;
  begin
    result:=length(toDoList);
  end;

FUNCTION T_dispatcher.runningCount:longint;
  VAR i:longint;
  begin
    result:=0;
    for i:=0 to length(spawned)-1 do if spawned[i].proc.running then inc(result);
  end;

FUNCTION T_dispatcher.numberOfCPUs:longint;
  begin
    result:=length(spawned);
  end;

PROCEDURE T_dispatcher.cancelPendingTasks;
  begin
    setLength(toDoList,0);
  end;

PROCEDURE T_dispatcher.printToDoList;
  VAR i,lMax:longint;
      txt:ansistring;
      someRunning:boolean=false;
  begin
    execute('cmd /C cls');
    lMax:=3;
    for i:=0 to length(toDoList)-1 do if length(toDoList[i].cmd)>lMax then lMax:=length(toDoList[i].cmd);
    for i:=0 to length(spawned)-1 do if (spawned[i].proc.running) then begin
      if (length(spawned[i].cmd)+2>lMax) then lMax:=length(spawned[i].cmd)+2;
      someRunning:=true;
    end;
    if lMax>120 then lMax:=120;
    inc(lMax,4);
    if someRunning or (length(toDoList)>0) then writeln;
    if someRunning then begin
      txt:='active:'; while length(txt)<lMax-1 do txt:=txt+'-'; txt:=txt+'+';  writeln(txt);
      for i:=0 to length(spawned)-1 do if (spawned[i].proc.running) then begin
        txt:='|S'+intToStr(i)+' '+copy(spawned[i].cmd,1,118); while length(txt)<lMax-1 do txt:=txt+' '; txt:=txt+'|'; writeln(txt);
      end;
      txt:=':active'; while length(txt)<lMax-1 do txt:='-'+txt; txt:='+'+txt; writeln(txt);
    end;
    if length(toDoList)>0 then begin
      txt:='pending:'; while length(txt)<lMax-1 do txt:=txt+'-'; txt:=txt+'+';  writeln(txt);
      for i:=0 to length(toDoList)-1 do if i<30 then begin
        txt:='| '+copy(toDoList[i].cmd,1,120); while length(txt)<lMax-1 do txt:=txt+' '; txt:=txt+'|'; writeln(txt);
      end;
      if length(toDoList)>=30 then begin
        txt:='| ...and '+intToStr(length(toDoList)-30)+' more';
        while length(txt)<lMax-1 do txt:=txt+' '; txt:=txt+'|'; writeln(txt);
      end;
      txt:=':pending'; while length(txt)<lMax-1 do txt:='-'+txt; txt:='+'+txt; writeln(txt);
    end;
  end;

end.
