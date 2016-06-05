{$mode objfpc}
{$h+}
program ipcserver;

{$APPTYPE CONSOLE}

uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils,
  Classes,
  simpleipc;

Type
  TApp = Class(TObject)  
    Srv : TSimpleIPCServer;
    DoStop : Boolean;
    Procedure MessageQueued(Sender : TObject);
    procedure Run;
    Procedure PrintMessage;
  end;

Procedure TApp.PrintMessage;

Var
  S : String;
 
begin
  S:=Srv.StringMessage;
  Writeln('Received message : ',S);
  DoStop:=DoStop or (S='stop');
end;

Procedure TApp.MessageQueued(Sender : TObject);

begin
  Srv.ReadMessage;
  PrintMessage;
end;


Procedure TApp.Run;
  
Var
  S : String;
  Threaded : Boolean;

begin
  Srv:=TSimpleIPCServer.Create(Nil);
  Try
    S:= ParamStr(1);
    Threaded:=(S='-t') or (S='--threaded');
    Srv.ServerID:='ipcserver';
    Srv.Global:=True;
    if Threaded then
      Srv.OnMessageQueued:=@MessageQueued;
    Srv.StartServer(Threaded);
    
    Writeln('Server started. Listening for messages. Send "stop" message to stop server.');
    Repeat
      If Threaded then
        begin
        Sleep(10);
        CheckSynchronize;
        end
      else if Srv.PeekMessage(10,True) then
        PrintMessage
      else
        Sleep(10);
    Until DoStop;
  Finally
    Srv.Free;
  end;
end;

begin
  With TApp.Create do
    try
      Run
    finally
      Free;
    end;    
end.

