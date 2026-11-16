{ Tests TFPHttpServer with an incomplete request body and its handling of accept errors. }
unit tchttpserverbody;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fphttpserver, ssockets;

type

  { TBodyTestServer }

  TBodyTestServer = class(TFPHttpServer)
  public
    // DoAcceptError with a new exception of type aCode
    function AcceptErrorAction(aCode: TSocketErrorType): TAcceptErrorAction;
  end;

  { TBodyTestServerThread }

  TBodyTestServerThread = class(TThread)
  private
    FServer: TBodyTestServer;
    FHandlerCalls: Integer;
    FBody: String;
    procedure DoRequest(Sender: TObject; var aRequest: TFPHTTPConnectionRequest;
      var aResponse: TFPHTTPConnectionResponse);
  protected
    procedure Execute; override;
  public
    // Create a suspended thread with a server on aPort
    constructor Create(aPort: Word);
    destructor Destroy; override;
    // The server; Active is set to True in Execute
    property Server: TBodyTestServer read FServer;
    // Number of requests that reached the handler
    property HandlerCalls: Integer read FHandlerCalls;
    // Body of the last request that reached the handler
    property Body: String read FBody;
  end;

  { TTestHTTPServerBody }

  TTestHTTPServerBody = class(TTestCase)
  private
    FThread: TBodyTestServerThread;
    FPort: Word;
    procedure StartServer;
    procedure StopServer;
    function SendRaw(const aRequest: String; aShutdownWrite: Boolean): String;
  protected
    procedure TearDown; override;
  published
    procedure TestCompleteBody;
    procedure TestIncompleteBody;
    procedure TestAcceptErrorWhileActive;
    procedure TestOtherErrorWhileActive;
    procedure TestAcceptErrorWhileInactive;
    procedure TestServerAnswersAfterAcceptError;
    procedure TestApplicationExceptionExitCode;
  end;

implementation

uses
  fpsockets, Sockets, fphttpapp;

const
  StartTimeoutMS = 5000;


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


{ TBodyTestServer }

function TBodyTestServer.AcceptErrorAction(aCode: TSocketErrorType): TAcceptErrorAction;

var
  lError: ESocketError;

begin
  Result:=aeaRaise;
  lError:=ESocketError.Create('test');
  lError.Code:=aCode;
  try
    DoAcceptError(Self,Default(TFPSocket),lError,Result);
  finally
    lError.Free;
  end;
end;


{ TBodyTestServerThread }

constructor TBodyTestServerThread.Create(aPort: Word);

begin
  inherited Create(True);
  FServer:=TBodyTestServer.Create(nil);
  FServer.Port:=aPort;
  FServer.Address:='127.0.0.1';
  FServer.AcceptIdleTimeout:=100;
  FServer.RequestReadTimeout:=5000;
  FServer.OnRequest:=@DoRequest;
end;


destructor TBodyTestServerThread.Destroy;

begin
  inherited Destroy;
  FreeAndNil(FServer);
end;


procedure TBodyTestServerThread.DoRequest(Sender: TObject; var aRequest: TFPHTTPConnectionRequest;
  var aResponse: TFPHTTPConnectionResponse);

begin
  Inc(FHandlerCalls);
  FBody:=aRequest.Content;
  aResponse.ContentType:='text/plain';
  aResponse.Content:='ok';
end;


procedure TBodyTestServerThread.Execute;

begin
  try
    FServer.Active:=True;
  except
    // the test sees a server that did not start
  end;
end;


{ TTestHTTPServerBody }

procedure TTestHTTPServerBody.StartServer;

var
  lStart: QWord;

begin
  FPort:=FreePort;
  AssertTrue('A free port', FPort<>0);
  FThread:=TBodyTestServerThread.Create(FPort);
  FThread.Start;
  lStart:=GetTickCount64;
  while not FThread.Server.Active and (GetTickCount64-lStart<StartTimeoutMS) do
    Sleep(10);
  AssertTrue('Server started', FThread.Server.Active);
  Sleep(50);
end;


procedure TTestHTTPServerBody.StopServer;

begin
  if not Assigned(FThread) then
    Exit;
  FThread.Server.Active:=False;
  FThread.WaitFor;
  FreeAndNil(FThread);
end;


// Sends aRequest on a new connection and returns everything the server sends until it closes
function TTestHTTPServerBody.SendRaw(const aRequest: String; aShutdownWrite: Boolean): String;

var
  lSocket: TInetSocket;
  lBuf: array[0..4095] of AnsiChar;
  lCount: Integer;

begin
  Result:='';
  lSocket:=TInetSocket.Create('127.0.0.1',FPort);
  try
    lSocket.IOTimeout:=5000;
    if aRequest<>'' then
      lSocket.WriteBuffer(aRequest[1],Length(aRequest));
    if aShutdownWrite then
      fpShutdown(lSocket.Handle,SHUT_WR);
    repeat
      lCount:=lSocket.Read(lBuf,SizeOf(lBuf));
      if lCount>0 then
        Result:=Result+Copy(lBuf,0,lCount);
    until lCount<=0;
  finally
    lSocket.Free;
  end;
end;


procedure TTestHTTPServerBody.TearDown;

begin
  StopServer;
end;


procedure TTestHTTPServerBody.TestCompleteBody;

var
  lResponse: String;

begin
  StartServer;
  lResponse:=SendRaw('POST /x HTTP/1.1'#13#10'Host: localhost'#13#10'Connection: close'#13#10+
                     'Content-Length: 3'#13#10#13#10'abc',True);
  AssertTrue('Response status 200: '+Copy(lResponse,1,40),Pos('HTTP/1.1 200',lResponse)=1);
  AssertEquals('Handler called',1,FThread.HandlerCalls);
  AssertEquals('Body','abc',FThread.Body);
end;


procedure TTestHTTPServerBody.TestIncompleteBody;

var
  lResponse: String;

begin
  StartServer;
  lResponse:=SendRaw('POST /x HTTP/1.1'#13#10'Host: localhost'#13#10'Connection: close'#13#10+
                     'Content-Length: 10'#13#10#13#10'abc',True);
  AssertEquals('Handler not called for an incomplete body',0,FThread.HandlerCalls);
  AssertTrue('No response or status 400: '+Copy(lResponse,1,40),
             (lResponse='') or (Pos('HTTP/1.1 400',lResponse)=1));
  AssertTrue('Server still active',FThread.Server.Active);
end;


procedure TTestHTTPServerBody.TestAcceptErrorWhileActive;

begin
  StartServer;
  AssertTrue('A failed accept is ignored while active',
             FThread.Server.AcceptErrorAction(seAcceptFailed)=aeaIgnore);
end;


procedure TTestHTTPServerBody.TestOtherErrorWhileActive;

begin
  StartServer;
  AssertTrue('Another socket error is ignored while active',
             FThread.Server.AcceptErrorAction(seIOTimeOut)=aeaIgnore);
end;


procedure TTestHTTPServerBody.TestAcceptErrorWhileInactive;

var
  lServer: TBodyTestServer;

begin
  lServer:=TBodyTestServer.Create(nil);
  try
    AssertTrue('An accept error stops an inactive server',
               lServer.AcceptErrorAction(seAcceptFailed)=aeaStop);
  finally
    lServer.Free;
  end;
end;


procedure TTestHTTPServerBody.TestServerAnswersAfterAcceptError;

var
  lResponse: String;

begin
  StartServer;
  FThread.Server.AcceptErrorAction(seAcceptFailed);
  lResponse:=SendRaw('GET /x HTTP/1.1'#13#10'Host: localhost'#13#10'Connection: close'#13#10#13#10,False);
  AssertTrue('Response status 200 after an accept error: '+Copy(lResponse,1,40),
             Pos('HTTP/1.1 200',lResponse)=1);
end;


procedure TTestHTTPServerBody.TestApplicationExceptionExitCode;

var
  lApp: THTTPApplication;

begin
  lApp:=THTTPApplication.Create(nil);
  try
    AssertEquals('Exit code after an unhandled exception',1,lApp.ExceptionExitCode);
  finally
    lApp.Free;
  end;
end;

initialization
  RegisterTest(TTestHTTPServerBody);
end.
