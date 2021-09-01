program wsserver;

uses
  {$ifdef unix} cthreads, cwstring, {$endif}
  custapp, sysutils, jsonparser, fpjson, syncobjs, classes,
  fpwebsocket, fpwebsocketclient, fpwebsocketserver, fpcustwsserver, wschat;

Type

  { TWSApp }

  TWSApp = class(TCustomApplication)
  private
    FSrv : TWebSocketServer;
    FChat : TWebsocketChat;
    procedure DoChatLog(Sender: TObject; const Msg: String);
  Public
    constructor create(aOwner : TComponent); override;
    destructor destroy; override;
    procedure DoRun; override;
  end;

{ TWSApp }

procedure TWSApp.DoChatLog(Sender: TObject; const Msg: String);
begin
  Writeln(Msg);
end;

constructor TWSApp.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FSrv:=TWebSocketServer.Create(Self);
  FChat:=TWebsocketChat.Create(Self);
  FChat.WebsocketServer:=FSrv;
  FChat.OnLog:=@DoChatLog;
  // Must do this here, because the events are protected
  FSrv.OnMessageReceived:=@FChat.DoMessageReceived;
  FSrv.OnControlReceived:=@FChat.DoControlReceived;
  FSrv.OnDisconnect:=@FChat.DoDisconnect;
end;

destructor TWSApp.destroy;
begin
  FreeAndNil(FChat);
  FreeAndNil(FSrv);
  inherited destroy;
end;


procedure TWSApp.DoRun;


begin
  CheckOptions('p:t:ahw:i:m:',['port:','threadmode','accept-threaded','wait:','idle:','mask:']);
  case GetOptionValue('t','threadmode') of
    'pool'   : FSrv.ThreadMode:=wtmThreadPool;
    'thread' : FSrv.ThreadMode:=wtmThread;
    'none'   : FSrv.ThreadMode:=wtmNone;
  else
    FSrv.ThreadMode:=wtmThread;
  end;
  // Will not return till stopped.
  FSrv.ThreadedAccept:=HasOption('a','accept-threaded');
  if HasOption('w','wait') then
    FSrv.MessageWaitTime:=StrToIntDef(GetOptionValue('w','wait'),DefaultWaitTime);
  if HasOption('i','idle') then
    FSrv.AcceptIdleTimeout:=StrToIntDef(GetOptionValue('i','idle'),DefaultAcceptTimeout);
  FSrv.Active:=True;
  FSrv.OutgoingFrameMask:=StrToIntDef(GetOptionValue('m','mask'),0);
  if Not FSrv.ThreadedAccept then
    Terminate
  else
    While Not Terminated do
      Sleep(10)
end;

begin
  With TWSApp.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.

