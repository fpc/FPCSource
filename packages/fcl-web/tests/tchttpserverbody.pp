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
    FReadTimeout: Integer;
    FHeaderTimeout: Integer;
    FBodyMinRate: Integer;
    FKeepConnections: Boolean;
    procedure StartServer;
    procedure StopServer;
    function Connect: TInetSocket;
    function SendRaw(const aRequest: String; aShutdownWrite: Boolean): String;
    function ReadAll(aSocket: TInetSocket): String;
    function ReadResponse(aSocket: TInetSocket): String;
    function SendSlowly(aSocket: TInetSocket; const aHead: String; aChunk, aCount, aDelayMS: Integer): Integer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCompleteBody;
    procedure TestIncompleteBody;
    procedure TestAcceptErrorWhileActive;
    procedure TestOtherErrorWhileActive;
    procedure TestAcceptErrorWhileInactive;
    procedure TestServerAnswersAfterAcceptError;
    procedure TestApplicationExceptionExitCode;
    procedure TestApplicationRequestLimits;
    procedure TestSlowHeadersCutOff;
    procedure TestSlowHeadersWithoutLimit;
    procedure TestIdleConnectionClosedQuietly;
    procedure TestKeepAlivePauseLongerThanHeaderTimeout;
    procedure TestSlowBodyRefused;
    procedure TestFastBodyAccepted;
    procedure TestSlowBodyWithoutMinRate;
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
  FThread.Server.RequestReadTimeout:=FReadTimeout;
  FThread.Server.RequestHeaderTimeout:=FHeaderTimeout;
  FThread.Server.RequestBodyMinRate:=FBodyMinRate;
  if FKeepConnections then
    begin
    FThread.Server.ThreadMode:=tmThread;
    FThread.Server.KeepConnections:=True;
    FThread.Server.KeepConnectionTimeout:=10000;
    end;
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


// A connection to the test server; writes do not raise SIGPIPE
function TTestHTTPServerBody.Connect: TInetSocket;

begin
  Result:=TInetSocket.Create('127.0.0.1',FPort);
  Result.IOTimeout:=10000;
{$if defined(FreeBSD) or defined(Linux)}
  Result.WriteFlags:=MSG_NOSIGNAL;
{$endif}
end;


// Everything the server sends until it closes the connection
function TTestHTTPServerBody.ReadAll(aSocket: TInetSocket): String;

var
  lBuf: array[0..4095] of AnsiChar;
  lCount: Integer;

begin
  Result:='';
  repeat
    lCount:=aSocket.Read(lBuf,SizeOf(lBuf));
    if lCount>0 then
      Result:=Result+Copy(lBuf,0,lCount);
  until lCount<=0;
end;


// One response: the headers and Content-Length bytes of body
function TTestHTTPServerBody.ReadResponse(aSocket: TInetSocket): String;

var
  lChar: AnsiChar;
  lPos, lLength: Integer;

begin
  Result:='';
  while (Pos(#13#10#13#10,Result)=0) and (aSocket.Read(lChar,1)=1) do
    Result:=Result+lChar;
  lLength:=0;
  lPos:=Pos('Content-Length:',Result);
  if lPos>0 then
    lLength:=StrToIntDef(Trim(Copy(Result,lPos+15,Pos(#13#10,Copy(Result,lPos,MaxInt))-16)),0);
  while (lLength>0) and (aSocket.Read(lChar,1)=1) do
    begin
    Result:=Result+lChar;
    Dec(lLength);
    end;
end;


// Sends aHead, then aCount chunks of aChunk bytes aDelayMS apart; stops early when the server
// answers or closes. Returns the number of chunks sent.
function TTestHTTPServerBody.SendSlowly(aSocket: TInetSocket; const aHead: String; aChunk, aCount, aDelayMS: Integer): Integer;

var
  lData: String;

begin
  Result:=0;
  aSocket.WriteBuffer(aHead[1],Length(aHead));
  lData:=StringOfChar('a',aChunk);
  while Result<aCount do
    begin
    if aSocket.CanRead(aDelayMS) then
      Exit;
    try
      if aSocket.Write(lData[1],aChunk)<>aChunk then
        Exit;
    except
      Exit;
    end;
    Inc(Result);
    end;
end;


// Sends aRequest on a new connection and returns everything the server sends until it closes
function TTestHTTPServerBody.SendRaw(const aRequest: String; aShutdownWrite: Boolean): String;

var
  lSocket: TInetSocket;

begin
  lSocket:=Connect;
  try
    if aRequest<>'' then
      lSocket.WriteBuffer(aRequest[1],Length(aRequest));
    if aShutdownWrite then
      fpShutdown(lSocket.Handle,SHUT_WR);
    Result:=ReadAll(lSocket);
  finally
    lSocket.Free;
  end;
end;


procedure TTestHTTPServerBody.SetUp;

begin
  FReadTimeout:=5000;
  FHeaderTimeout:=DefaultRequestHeaderTimeout;
  FBodyMinRate:=0;
  FKeepConnections:=False;
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

procedure TTestHTTPServerBody.TestApplicationRequestLimits;

var
  lApp: THTTPApplication;

begin
  lApp:=THTTPApplication.Create(nil);
  try
    AssertEquals('Default RequestReadTimeout',DefaultRequestReadTimeout,lApp.RequestReadTimeout);
    AssertEquals('Default RequestHeaderTimeout',DefaultRequestHeaderTimeout,lApp.RequestHeaderTimeout);
    AssertEquals('Default RequestBodyMinRate',DefaultRequestBodyMinRate,lApp.RequestBodyMinRate);
    lApp.RequestReadTimeout:=1234;
    lApp.RequestHeaderTimeout:=2345;
    lApp.RequestBodyMinRate:=3456;
    AssertEquals('RequestReadTimeout reaches the server',1234,lApp.HTTPHandler.HTTPServer.RequestReadTimeout);
    AssertEquals('RequestHeaderTimeout reaches the server',2345,lApp.HTTPHandler.HTTPServer.RequestHeaderTimeout);
    AssertEquals('RequestBodyMinRate reaches the server',3456,lApp.HTTPHandler.HTTPServer.RequestBodyMinRate);
    AssertEquals('RequestReadTimeout on the handler',1234,lApp.HTTPHandler.RequestReadTimeout);
  finally
    lApp.Free;
  end;
end;


procedure TTestHTTPServerBody.TestSlowHeadersCutOff;

var
  lSocket: TInetSocket;
  lStart, lElapsed: QWord;
  lSent: Integer;
  lResponse: String;

begin
  FReadTimeout:=1000;
  FHeaderTimeout:=1500;
  StartServer;
  lSocket:=Connect;
  try
    lStart:=GetTickCount64;
    lSent:=SendSlowly(lSocket,'GET /x HTTP/1.1'#13#10'Host: localhost'#13#10'X-Slow: ',1,50,200);
    lElapsed:=GetTickCount64-lStart;
    lResponse:=ReadAll(lSocket);
  finally
    lSocket.Free;
  end;
  AssertTrue(Format('Connection ended before all bytes were sent (%d of 50)',[lSent]),lSent<50);
  AssertTrue(Format('Connection ended after the header time-out (%d ms)',[lElapsed]),
             (lElapsed>=1300) and (lElapsed<4000));
  AssertEquals('Handler not called',0,FThread.HandlerCalls);
  AssertTrue('No response or status 408: '+Copy(lResponse,1,40),
             (lResponse='') or (Pos('HTTP/1.1 408',lResponse)=1));
end;


procedure TTestHTTPServerBody.TestSlowHeadersWithoutLimit;

var
  lSocket: TInetSocket;
  lSent: Integer;
  lResponse: String;

begin
  FReadTimeout:=1000;
  FHeaderTimeout:=0;
  StartServer;
  lSocket:=Connect;
  try
    lSent:=SendSlowly(lSocket,'GET /x HTTP/1.1'#13#10'Host: localhost'#13#10'Connection: close'#13#10'X-Slow: ',1,10,200);
    lSocket.WriteBuffer(#13#10#13#10,4);
    lResponse:=ReadAll(lSocket);
  finally
    lSocket.Free;
  end;
  AssertEquals('All bytes sent',10,lSent);
  AssertTrue('Response status 200: '+Copy(lResponse,1,40),Pos('HTTP/1.1 200',lResponse)=1);
  AssertEquals('Handler called',1,FThread.HandlerCalls);
end;


procedure TTestHTTPServerBody.TestIdleConnectionClosedQuietly;

var
  lSocket: TInetSocket;
  lStart, lElapsed: QWord;
  lResponse: String;

begin
  FReadTimeout:=500;
  FHeaderTimeout:=200;
  StartServer;
  lSocket:=Connect;
  try
    lStart:=GetTickCount64;
    lResponse:=ReadAll(lSocket);
    lElapsed:=GetTickCount64-lStart;
  finally
    lSocket.Free;
  end;
  AssertEquals('No response on an idle connection','',lResponse);
  AssertTrue(Format('Idle connection closed after the read time-out (%d ms)',[lElapsed]),
             (lElapsed>=400) and (lElapsed<3000));
  AssertEquals('Handler not called',0,FThread.HandlerCalls);
end;


procedure TTestHTTPServerBody.TestKeepAlivePauseLongerThanHeaderTimeout;

const
  Request = 'GET /x HTTP/1.1'#13#10'Host: localhost'#13#10#13#10;

var
  lSocket: TInetSocket;
  lFirst, lSecond: String;

begin
  FReadTimeout:=3000;
  FHeaderTimeout:=500;
  FKeepConnections:=True;
  StartServer;
  lSocket:=Connect;
  try
    lSocket.WriteBuffer(Request[1],Length(Request));
    lFirst:=ReadResponse(lSocket);
    Sleep(1000);
    lSocket.WriteBuffer(Request[1],Length(Request));
    lSecond:=ReadResponse(lSocket);
  finally
    lSocket.Free;
  end;
  AssertTrue('First response status 200: '+Copy(lFirst,1,40),Pos('HTTP/1.1 200',lFirst)=1);
  AssertTrue('Second response status 200 after a pause: '+Copy(lSecond,1,40),Pos('HTTP/1.1 200',lSecond)=1);
  AssertEquals('Handler called twice',2,FThread.HandlerCalls);
end;


procedure TTestHTTPServerBody.TestSlowBodyRefused;

var
  lSocket: TInetSocket;
  lStart, lElapsed: QWord;
  lSent: Integer;

begin
  FReadTimeout:=500;
  FBodyMinRate:=1000;
  StartServer;
  lSocket:=Connect;
  try
    lStart:=GetTickCount64;
    lSent:=SendSlowly(lSocket,'POST /x HTTP/1.1'#13#10'Host: localhost'#13#10'Content-Length: 10000'#13#10#13#10,50,200,100);
    lElapsed:=GetTickCount64-lStart;
    ReadAll(lSocket);
  finally
    lSocket.Free;
  end;
  AssertTrue(Format('Connection ended before the body was sent (%d of 200 chunks)',[lSent]),lSent<200);
  AssertTrue(Format('Connection ended within 5 s (%d ms)',[lElapsed]),lElapsed<5000);
  AssertEquals('Handler not called',0,FThread.HandlerCalls);
end;


procedure TTestHTTPServerBody.TestFastBodyAccepted;

var
  lResponse: String;

begin
  FReadTimeout:=500;
  FBodyMinRate:=1000;
  StartServer;
  lResponse:=SendRaw('POST /x HTTP/1.1'#13#10'Host: localhost'#13#10'Connection: close'#13#10+
                     'Content-Length: 10000'#13#10#13#10+StringOfChar('b',10000),False);
  AssertTrue('Response status 200: '+Copy(lResponse,1,40),Pos('HTTP/1.1 200',lResponse)=1);
  AssertEquals('Body length',10000,Length(FThread.Body));
end;


procedure TTestHTTPServerBody.TestSlowBodyWithoutMinRate;

var
  lSocket: TInetSocket;
  lSent: Integer;
  lResponse: String;

begin
  FReadTimeout:=1000;
  FBodyMinRate:=0;
  StartServer;
  lSocket:=Connect;
  try
    lSent:=SendSlowly(lSocket,'POST /x HTTP/1.1'#13#10'Host: localhost'#13#10'Connection: close'#13#10+
                      'Content-Length: 1000'#13#10#13#10,50,20,100);
    lResponse:=ReadAll(lSocket);
  finally
    lSocket.Free;
  end;
  AssertEquals('All chunks sent',20,lSent);
  AssertTrue('Response status 200: '+Copy(lResponse,1,40),Pos('HTTP/1.1 200',lResponse)=1);
  AssertEquals('Body length',1000,Length(FThread.Body));
end;

initialization
  RegisterTest(TTestHTTPServerBody);
end.
