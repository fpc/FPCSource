{$mode objfpc}
{$h+}
program ipcclient;

uses sysutils,simpleipc;

Var
  I,Count : Integer;

begin
  Count:=1;
  With TSimpleIPCClient.Create(Nil) do
    try
      ServerID:='ipcserver';
      If (ParamCount>0) then
        ServerInstance:=Paramstr(1);
      if ParamCount>1 then
        Count:=StrToIntDef(ParamStr(2),1);  
      Active:=True;
      for I:=1 to Count do
        SendStringMessage(Format('Testmessage %d from client',[i]));
      Active:=False;
    finally
      Free;
    end;
end.
