UNIT UniqueInstanceRaw;
{$mode objfpc}{$H+}

INTERFACE

uses
 {$ifdef UNIX}cmem,cthreads,{$endif}
  Classes, SysUtils, simpleipc;
  
  FUNCTION InstanceRunning(CONST Identifier: String; SendParameters: Boolean = false): Boolean;

  FUNCTION InstanceRunning: Boolean;
VAR
  FIPCServer: TSimpleIPCServer;
IMPLEMENTATION

uses
  SimpleIPCWrapper;

CONST
  BaseServerId = 'tuniqueinstance_';
  Separator = '|';



FUNCTION GetFormattedParams: String;
VAR
  i: Integer;
begin
  result := '';
  {$ifdef expandFileNames}
  for i := 1 to ParamCount do if paramstr(i)[1]='-'
    then result := result +                paramstr(i)  + Separator
    else result := result + expandFileName(ParamStr(i)) + Separator;
  {$else}
  for i := 1 to ParamCount do result := result + paramstr(i)  + Separator;
  {$endif}
end;
  
FUNCTION InstanceRunning(CONST Identifier: String; SendParameters: Boolean = false): Boolean;

  FUNCTION GetServerId: String;
  begin
    if Identifier <> '' then
      result := BaseServerId + Identifier
    else
      result := BaseServerId + ExtractFileName(ParamStr(0));
  end;
  
VAR
  Client: TSimpleIPCClient;
  
begin
  Client := TSimpleIPCClient.create(nil);
  with Client do
  try
    ServerId := GetServerId;
    result := IsServerRunning(Client);
    if not result then
    begin
      //It's the first instance. Init the server
      if FIPCServer = nil then
        FIPCServer := TSimpleIPCServer.create(nil);
      FIPCServer.ServerID := ServerId;
      FIPCServer.Global := true;
      InitServer(FIPCServer);
    end
    else
      // an instance already exists
      if SendParameters then
      begin
        Active := true;
        SendStringMessage(ParamCount, GetFormattedParams);
      end;
  finally
    Free;
  end;
end;

FUNCTION InstanceRunning: Boolean;
begin
  result := InstanceRunning('');
end;

FINALIZATION
  FIPCServer.Free;

end.

