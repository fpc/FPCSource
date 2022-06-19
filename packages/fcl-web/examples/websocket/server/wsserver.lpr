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
    procedure Usage(const aError: string);
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


procedure TWSApp.Usage(const aError : string);

begin
  if aError<>'' then
    Writeln('Error: ',aError);
  Writeln('Usage ',ExtractFileName(Paramstr(0)), ' [options] ');
  Writeln('Where options is one or more of:');
  Writeln('-h --help             This help text');
  Writeln('-p --port=PORT        Set port to listen on');
  Writeln('-t --threadmode=MODE  Set the threading mode');
  Writeln('                      MODE is one of:');
  Writeln('                      * threaded');
  Writeln('                      * none');
  Writeln('                      * pool');
  Writeln('-w --wait=TIME        Set wait time in milliseconds');
  Writeln('-i --idle=TIME        Set idle time in milliseconds');
  Writeln('-m --mask=MASK        Set outgoing frame mask (a 16-bit integer)');
  ExitCode:=Ord(aError<>'');
end;

procedure TWSApp.DoRun;


Var
  S : String;

begin
  S:=CheckOptions('p:t:ahw:i:m:',['port:','threadmode','accept-threaded','help','wait:','idle:','mask:']);
  if (S<>'') or HasOption('h','help') then
    begin
    Terminate;
    Usage(S);
    exit;
    end;
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
  FSrv.Port:=6060;
  FSrv.Active:=True;
  FSrv.OutgoingFrameMask:=StrToIntDef(GetOptionValue('m','mask'),0);
  if Not FSrv.ThreadedAccept then
    Terminate
  else
    While Not Terminated do
      Sleep(10);
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

