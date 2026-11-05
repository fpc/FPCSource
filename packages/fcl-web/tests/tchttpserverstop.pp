{ Tests stopping a TFPHttpServer that runs in a thread: Active:=False from another thread or from a request handler. }
unit tchttpserverstop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fphttpserver, fphttpclient, ssockets;

type

  { TStopTestServerThread }

  TStopTestServerThread = class(TThread)
  private
    FServer: TFPHttpServer;
    FDone: Boolean;
    FError: String;
    procedure DoRequest(Sender: TObject; var aRequest: TFPHTTPConnectionRequest;
      var aResponse: TFPHTTPConnectionResponse);
  protected
    procedure Execute; override;
  public
    // Create a suspended thread with a server on aPort, with the given idle timeout and thread mode
    constructor Create(aPort: Word; aIdleTimeout: Cardinal; aThreadMode: TThreadMode);
    destructor Destroy; override;
    // The server; Active is set to True in Execute
    property Server: TFPHttpServer read FServer;
    // True when Server.Active:=True has returned
    property Done: Boolean read FDone;
    // Message of an exception raised by Server.Active:=True
    property Error: String read FError;
  end;

  { TTestHTTPServerStop }

  TTestHTTPServerStop = class(TTestCase)
  private
    FThread: TStopTestServerThread;
    FPort: Word;
    procedure StartServer(aIdleTimeout: Cardinal; aThreadMode: TThreadMode);
    function Get(const aPath: String): String;
    procedure WaitServerReady;
    function WaitDone(aTimeoutMS: Integer): Boolean;
    procedure Poke;
    procedure CheckStops(const aMsg: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStopBlockingAccept;
    procedure TestStopBlockingAcceptThreaded;
    procedure TestStopIdleTimeout;
    procedure TestStopFromHandler;
    procedure TestStopFromHandlerIdleTimeout;
    procedure TestRestartSamePort;
  end;

implementation

uses
  Sockets;

const
  StopTimeoutMS = 5000;


// Return a TCP port that is free on the loopback interface
function FreePort: Word;

var
  lSocket: TSocket;
  lAddr: TInetSockAddr;
  lLen: TSockLen;

begin
  Result:=0;
  lSocket:=fpSocket(AF_INET, SOCK_STREAM, 0);
  if lSocket<0 then
    Exit;
  try
    lAddr:=Default(TInetSockAddr);
    lAddr.sin_family:=AF_INET;
    lAddr.sin_port:=0;
    lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
    if fpBind(lSocket, @lAddr, SizeOf(lAddr))<>0 then
      Exit;
    lLen:=SizeOf(lAddr);
    if fpGetSockName(lSocket, @lAddr, @lLen)=0 then
      Result:=NToHs(lAddr.sin_port);
  finally
    CloseSocket(lSocket);
  end;
end;


{ TStopTestServerThread }

constructor TStopTestServerThread.Create(aPort: Word; aIdleTimeout: Cardinal; aThreadMode: TThreadMode);

begin
  inherited Create(True);
  FServer:=TFPHttpServer.Create(nil);
  FServer.Port:=aPort;
  FServer.Address:='127.0.0.1';
  FServer.AcceptIdleTimeout:=aIdleTimeout;
  FServer.ThreadMode:=aThreadMode;
  FServer.OnRequest:=@DoRequest;
end;


destructor TStopTestServerThread.Destroy;

begin
  inherited Destroy;
  FreeAndNil(FServer);
end;


procedure TStopTestServerThread.DoRequest(Sender: TObject; var aRequest: TFPHTTPConnectionRequest;
  var aResponse: TFPHTTPConnectionResponse);

begin
  aResponse.ContentType:='text/plain';
  if aRequest.PathInfo='/stop' then
    begin
    aResponse.Content:='stopping';
    FServer.Active:=False;
    end
  else
    aResponse.Content:='pong';
end;


procedure TStopTestServerThread.Execute;

begin
  try
    FServer.Active:=True;
  except
    on E: Exception do
      FError:=E.ClassName+': '+E.Message;
  end;
  FDone:=True;
end;


{ TTestHTTPServerStop }

procedure TTestHTTPServerStop.SetUp;

begin
  FThread:=nil;
  FPort:=FreePort;
  AssertTrue('a free port was found', FPort<>0);
end;


procedure TTestHTTPServerStop.TearDown;

begin
  if Assigned(FThread) then
    begin
    if not FThread.Done then
      begin
      FThread.Server.Active:=False;
      Poke;
      WaitDone(StopTimeoutMS);
      end;
    if FThread.Done then
      FreeAndNil(FThread);
    end;
end;


procedure TTestHTTPServerStop.StartServer(aIdleTimeout: Cardinal; aThreadMode: TThreadMode);

begin
  FThread:=TStopTestServerThread.Create(FPort, aIdleTimeout, aThreadMode);
  FThread.Start;
  WaitServerReady;
end;


function TTestHTTPServerStop.Get(const aPath: String): String;

var
  lClient: TFPHTTPClient;

begin
  lClient:=TFPHTTPClient.Create(nil);
  try
    lClient.ConnectTimeout:=2000;
    lClient.IOTimeout:=2000;
    Result:=Trim(lClient.Get(Format('http://127.0.0.1:%d%s', [FPort, aPath])));
  finally
    lClient.Free;
  end;
end;


procedure TTestHTTPServerStop.WaitServerReady;

var
  lStart: QWord;
  lReply: String;

begin
  lStart:=GetTickCount64;
  lReply:='';
  repeat
    try
      lReply:=Get('/ping');
    except
      on Exception do
        begin
        if FThread.Done then
          Fail('server stopped before it answered: '+FThread.Error);
        Sleep(20);
        end;
    end;
  until (lReply<>'') or (GetTickCount64-lStart>StopTimeoutMS);
  AssertEquals('server answers /ping', 'pong', lReply);
end;


function TTestHTTPServerStop.WaitDone(aTimeoutMS: Integer): Boolean;

var
  lStart: QWord;

begin
  lStart:=GetTickCount64;
  while not FThread.Done and (GetTickCount64-lStart<QWord(aTimeoutMS)) do
    Sleep(10);
  Result:=FThread.Done;
end;


procedure TTestHTTPServerStop.Poke;

var
  lSocket: TInetSocket;

begin
  try
    lSocket:=TInetSocket.Create('127.0.0.1', FPort, 1000);
    lSocket.Free;
  except
    on Exception do ;
  end;
end;


procedure TTestHTTPServerStop.CheckStops(const aMsg: String);

begin
  AssertTrue(aMsg+': Active:=True returns within '+IntToStr(StopTimeoutMS)+' ms', WaitDone(StopTimeoutMS));
  AssertEquals(aMsg+': no exception in the server thread', '', FThread.Error);
  AssertFalse(aMsg+': server is no longer active', FThread.Server.Active);
end;


procedure TTestHTTPServerStop.TestStopBlockingAccept;

begin
  StartServer(0, tmNone);
  FThread.Server.Active:=False;
  CheckStops('AcceptIdleTimeout=0');
end;


procedure TTestHTTPServerStop.TestStopBlockingAcceptThreaded;

begin
  StartServer(0, tmThread);
  FThread.Server.Active:=False;
  CheckStops('AcceptIdleTimeout=0, tmThread');
end;


procedure TTestHTTPServerStop.TestStopIdleTimeout;

begin
  StartServer(100, tmNone);
  FThread.Server.Active:=False;
  CheckStops('AcceptIdleTimeout=100');
end;


procedure TTestHTTPServerStop.TestStopFromHandler;

begin
  StartServer(0, tmNone);
  AssertEquals('/stop is answered', 'stopping', Get('/stop'));
  CheckStops('stop in handler, AcceptIdleTimeout=0');
end;


procedure TTestHTTPServerStop.TestStopFromHandlerIdleTimeout;

begin
  StartServer(100, tmNone);
  AssertEquals('/stop is answered', 'stopping', Get('/stop'));
  CheckStops('stop in handler, AcceptIdleTimeout=100');
end;


procedure TTestHTTPServerStop.TestRestartSamePort;

begin
  StartServer(0, tmNone);
  FThread.Server.Active:=False;
  CheckStops('first run');
  FreeAndNil(FThread);
  StartServer(0, tmNone);
  FThread.Server.Active:=False;
  CheckStops('second run on the same port');
end;


initialization
  RegisterTest(TTestHTTPServerStop);
end.
