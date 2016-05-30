{$mode objfpc}
{$h+}
program ipcclient;

uses sysutils,simpleipc;

Var
  I,Count : Integer;
  DoStop : Boolean;

begin
  Count:=1;
  With TSimpleIPCClient.Create(Nil) do
    try
      ServerID:='ipcserver';
      If (ParamCount>0) then
        begin
        DoStop:=(ParamStr(1)='-s') or (paramstr(1)='--stop');
        if DoStop then
          ServerInstance:=Paramstr(2)
        else  
          ServerInstance:=Paramstr(1);
        if (Not DoStop) and (ParamCount>1) then
          Count:=StrToIntDef(ParamStr(2),1);  
        end;  
      Active:=True;
      if DoStop then
        SendStringMessage('stop')
      else  for I:=1 to Count do
        SendStringMessage(Format('Testmessage %d from client',[i]));
      Active:=False;
    finally
      Free;
    end;
end.
