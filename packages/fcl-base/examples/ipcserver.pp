{$mode objfpc}
{$h+}
program ipccerver;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  simpleipc;

Var
  Srv : TSimpleIPCServer;
  S : String;

begin
  Srv:=TSimpleIPCServer.Create(Nil);
  Try
    Srv.ServerID:='ipcserver';
    Srv.Global:=True;
    Srv.StartServer;
    Writeln('Server started. Listening for messages');
    Repeat
      If Srv.PeekMessage(1,True) then
        begin
        S:=Srv.StringMessage;
        Writeln('Received message : ',S);
        end
      else
        Sleep(10);
    Until CompareText(S,'stop')=0;
  Finally
    Srv.Free;
  end;
end.

