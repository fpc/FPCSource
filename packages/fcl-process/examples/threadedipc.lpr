program ThreadedIPC;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  SysUtils, Classes, Math, FGL, SimpleIPC;

const
  ServerUniqueID = '39693DC0-BD8B-4AAD-9D9B-387D37CD59FD';
  ServerTimeout = 5000;
  ClientDelayMin = 500;
  ClientDelayMax = 3000;
  ClientCount = 10;

var
  ServerThreaded: Boolean = True;

type
  TServerMessageHandler = class
  public
    procedure HandleMessage(Sender: TObject);
    procedure HandleMessageQueued(Sender: TObject);
  end;

procedure TServerMessageHandler.HandleMessage(Sender: TObject);
begin
  WriteLn(TSimpleIPCServer(Sender).StringMessage);
end;

procedure TServerMessageHandler.HandleMessageQueued(Sender: TObject);
begin
  TSimpleIPCServer(Sender).ReadMessage;
end;

procedure ServerWorker;
var
  Server: TSimpleIPCServer;
  MessageHandler: TServerMessageHandler;
begin
  WriteLn(Format('Starting server #%x', [GetThreadID]));
  MessageHandler := TServerMessageHandler.Create;
  Server := TSimpleIPCServer.Create(nil);
  try
    Server.ServerID := ServerUniqueID;
    Server.Global := True;
    Server.OnMessage := @MessageHandler.HandleMessage;
    Server.OnMessageQueued := @MessageHandler.HandleMessageQueued;
    Server.StartServer(ServerThreaded);
    if ServerThreaded then
      Sleep(ServerTimeout)
    else
      while Server.PeekMessage(ServerTimeout, True) do ;
  except on E: Exception do
    WriteLn('Server error: ' + E.Message);
  end;
  Server.Free;
  MessageHandler.Free;
  WriteLn(Format('Finished server #%x', [GetThreadID]));
end;

procedure ClientWorker;
var
  Client: TSimpleIPCClient;
  Message: String;
begin
  WriteLn(Format('Starting client #%x', [GetThreadID]));
  Client := TSimpleIPCClient.Create(nil);
  try
    Client.ServerID := ServerUniqueID;
    while not Client.ServerRunning do
      Sleep(100);
    Client.Active := True;
    Sleep(RandomRange(ClientDelayMin, ClientDelayMax));
    Message := Format('Hello from client #%x', [GetThreadID]);
    Client.SendStringMessage(Message);
  except on E: Exception do
    WriteLn('Client error: ' + E.Message);
  end;
  Client.Free;
  WriteLn(Format('Finished client #%x', [GetThreadID]));
end;

type
  TThreadList = specialize TFPGObjectList<TThread>;

var
  I: Integer;
  Thread: TThread;
  Threads: TThreadList;

begin
  Randomize;
  WriteLn('Threaded server: ' + BoolToStr(ServerThreaded, 'YES', 'NO'));
  Threads := TThreadList.Create(True);
  try
    Threads.Add(TThread.CreateAnonymousThread(@ServerWorker));
    for I := 1 to ClientCount do
      Threads.Add(TThread.CreateAnonymousThread(@ClientWorker));
    for Thread in Threads do
    begin
      Thread.FreeOnTerminate := False;
      Thread.Start;
    end;
    for Thread in Threads do
      Thread.WaitFor;
  finally
    Threads.Free;
  end;
end.

