UNIT metaDispatcher;
INTERFACE
USES sysTools,sysutils,UniqueInstanceRaw;
CONST C_DISPATCHER_INSTANCE_NAME='DISPATCHER';
      C_PATH_TO_DISPATCHER_EXECUTABLE='c:\dispatcher.exe';
TYPE
     T_dispatcherCommand=(dc_sync,dc_waitForIdle,dc_ownWindow,dc_pipeOut,dc_silent,dc_0t,dc_1t,dc_2t,dc_3t,dc_4t,dc_5t,dc_6t,dc_7t,dc_8t);
     T_stringArray=array of string;
CONST
     C_dispatcherCommandString:array[T_dispatcherCommand] of string=(
      '#sync',
      '#waitForIdle',
      '#ownWindow',
      '#pipeOut',
      '#silent',
      '#0_threads',
      '#1_threads',
      '#2_threads',
      '#3_threads',
      '#4_threads',
      '#5_threads',
      '#6_threads',
      '#7_threads',
      '#8_threads');

FUNCTION split(s:string; splitter:char):T_stringArray;
FUNCTION join(s:T_stringArray; splitter:char):string;
FUNCTION trimUnescape(s:string):string;
PROCEDURE appendTaskFromCmdLineParams;
PROCEDURE appendTask(par:T_stringArray);
PROCEDURE appendTask(par:string);
OPERATOR :=(x:T_dispatcherCommand):string;

PROCEDURE appendTask(CONST task:T_task);
IMPLEMENTATION


FUNCTION split(s: string; splitter: char): T_stringArray;
  VAR i,i0:longint;
      escaped:boolean=false;
  begin
    setLength(result,0);
    i0:=0;
    for i:=1 to length(s) do begin
      if s[i]='"' then escaped:=not(escaped);
      if (s[i]=splitter) and not(escaped) then begin
        setLength(result,length(result)+1);
        result[length(result)-1]:=trimUnescape(copy(s,i0+1,i-i0-1));
        //ignore empty elements, i.e. handle successive splitters as one splitter
        if result[length(result)-1]='' then setLength(result,length(result)-1);
        i0:=i;
      end;
    end;
  end;

FUNCTION join(s: T_stringArray; splitter: char): string;
  VAR i:longint;
  begin
    if length(s)=0 then exit('');
    result:=s[0];
    for i:=1 to length(s)-1 do result:=result+splitter+s[i];
  end;

FUNCTION trimUnescape(s: string): string;
  begin
    result:=trim(s);
    if (result[1]='"') and (result[length(result)]='"') then result:=copy(result,2,length(result)-2);
    result:=trim(result);
  end;

PROCEDURE appendTaskFromCmdLineParams;
  VAR i:longint;
      s:T_stringArray;
  begin
    setLength(s,paramCount);
    for i:=1 to paramCount do s[i-1]:=paramStr(i);
    appendTask(s);
  end;

PROCEDURE appendTask(par: T_stringArray);
  VAR tempProcess:TProcess;
  begin
    not(InstanceRunning(C_DISPATCHER_INSTANCE_NAME,false)) then begin
      try
        tempProcess :=TProcess.create(nil);
        tempProcess.CommandLine :=C_DISPATCHER_INSTANCE_NAME+' '+join(par,'|');
        tempProcess.options:=tempProcess.options+[poWaitOnExit];
        tempProcess.execute;
        result:=tempProcess.exitStatus;
        tempProcess.free;
      except
      end;
    end;

  end;

PROCEDURE appendTask(par: string);
  begin
    appendTask(split(par),' ');
  end;

OPERATOR:=(x: T_dispatcherCommand): string;
  begin
    result:=C_dispatcherCommandString[x];
  end;



end.

