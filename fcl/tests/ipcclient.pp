{$mode objfpc}
{$h+}
program ipcclient;

uses simpleipc;

begin
  With TSimpleIPCClient.Create(Nil) do
    try
      ServerID:='ipcserver';
      If (ParamCount>0) then
        ServerInstance:=Paramstr(1);
      Active:=True;
      SendStringMessage('Testmessage from client');
      Active:=False;
    finally
      Free;
    end;
end.
