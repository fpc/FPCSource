{ Tests the request headers sent by TFPHTTPClient, captured by a raw loopback listener. }
unit tchttpclientheaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets, fpcunit, testregistry, fphttpclient;

type

  { TCaptureThread }

  TCaptureThread = class(TThread)
  private
    FListener: TSocket;
    FPort: Word;
    FRequest: String;
  protected
    procedure Execute; override;
  public
    // Create a suspended thread listening on 127.0.0.1 on a free port
    constructor Create;
    destructor Destroy; override;
    // Port the listener is bound to
    property Port: Word read FPort;
    // Request line and headers of the first request received
    property Request: String read FRequest;
  end;

  { TTestHTTPClientHeaders }

  TTestHTTPClientHeaders = class(TTestCase)
  private
    FThread: TCaptureThread;
    FClient: TFPHTTPClient;
    function SendGet: String;
    function HeaderLines(const aRequest, aName: String): TStringArray;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHostFromURL;
    procedure TestUserHostSentOnce;
    procedure TestUserHostLowerCase;
    procedure TestCookiesAndRequestCookies;
    procedure TestRequestCookiesOnly;
    procedure TestNoCookieLine;
  end;

implementation

const
  cResponse = 'HTTP/1.1 200 OK'#13#10'Content-Length: 0'#13#10'Connection: close'#13#10#13#10;


{ TCaptureThread }

constructor TCaptureThread.Create;

var
  lAddr: TInetSockAddr;
  lLen: TSockLen;

begin
  inherited Create(True);
  FListener:=fpSocket(AF_INET,SOCK_STREAM,0);
  if FListener<0 then
    raise Exception.Create('Cannot create listening socket');
  lAddr:=Default(TInetSockAddr);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_port:=0;
  lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
  if fpBind(FListener,@lAddr,SizeOf(lAddr))<>0 then
    raise Exception.Create('Cannot bind listening socket');
  if fpListen(FListener,1)<>0 then
    raise Exception.Create('Cannot listen');
  lLen:=SizeOf(lAddr);
  fpGetSockName(FListener,@lAddr,@lLen);
  FPort:=NToHs(lAddr.sin_port);
end;


destructor TCaptureThread.Destroy;

begin
  fpShutdown(FListener,SHUT_RDWR);
  CloseSocket(FListener);
  inherited Destroy;
end;


procedure TCaptureThread.Execute;

var
  lConn: TSocket;
  lBuf: array[0..1023] of AnsiChar;
  lCount: Integer;

begin
  lConn:=fpAccept(FListener,nil,nil);
  if lConn<0 then
    Exit;
  try
    repeat
      lCount:=fpRecv(lConn,@lBuf,SizeOf(lBuf),0);
      if lCount>0 then
        FRequest:=FRequest+Copy(lBuf,0,lCount);
    until (lCount<=0) or (Pos(#13#10#13#10,FRequest)>0);
    fpSend(lConn,PAnsiChar(cResponse),Length(cResponse),0);
  finally
    CloseSocket(lConn);
  end;
end;


{ TTestHTTPClientHeaders }

procedure TTestHTTPClientHeaders.SetUp;

begin
  FThread:=TCaptureThread.Create;
  FClient:=TFPHTTPClient.Create(nil);
  FClient.IOTimeout:=5000;
end;


procedure TTestHTTPClientHeaders.TearDown;

begin
  FreeAndNil(FClient);
  FreeAndNil(FThread);
end;


function TTestHTTPClientHeaders.SendGet: String;

begin
  FThread.Start;
  FClient.Get('http://127.0.0.1:'+IntToStr(FThread.Port)+'/');
  FThread.WaitFor;
  Result:=FThread.Request;
end;


function TTestHTTPClientHeaders.HeaderLines(const aRequest, aName: String): TStringArray;

var
  lLines: TStringArray;
  lLine: String;

begin
  Result:=[];
  lLines:=aRequest.Split([#13#10]);
  for lLine in lLines do
    if SameText(Copy(lLine,1,Length(aName)+1),aName+':') then
      Result:=Concat(Result,[lLine]);
end;


procedure TTestHTTPClientHeaders.TestHostFromURL;

var
  lHosts: TStringArray;

begin
  lHosts:=HeaderLines(SendGet,'Host');
  AssertEquals('One Host header without a user Host header',1,Length(lHosts));
  AssertEquals('Host header built from the URL','Host: 127.0.0.1:'+IntToStr(FThread.Port),lHosts[0]);
end;


procedure TTestHTTPClientHeaders.TestUserHostSentOnce;

var
  lHosts: TStringArray;

begin
  FClient.AddHeader('Host','example.org');
  lHosts:=HeaderLines(SendGet,'Host');
  AssertEquals('One Host header with a user Host header',1,Length(lHosts));
  AssertEquals('The user Host header is sent','Host: example.org',lHosts[0]);
end;


procedure TTestHTTPClientHeaders.TestUserHostLowerCase;

var
  lHosts: TStringArray;

begin
  FClient.RequestHeaders.Add('host: example.org');
  lHosts:=HeaderLines(SendGet,'Host');
  AssertEquals('One Host header with a lowercase user host header',1,Length(lHosts));
  AssertEquals('The lowercase user host header is sent','host: example.org',lHosts[0]);
end;


procedure TTestHTTPClientHeaders.TestCookiesAndRequestCookies;

var
  lCookies: TStringArray;

begin
  FClient.Cookies.Add('a=1');
  FClient.Cookies.Add('b=2');
  with FClient.RequestCookies.Add do
    begin
    Name:='c';
    Value:='3';
    end;
  with FClient.RequestCookies.Add do
    begin
    Name:='d';
    Value:='4';
    end;
  lCookies:=HeaderLines(SendGet,'Cookie');
  AssertEquals('One Cookie header',1,Length(lCookies));
  AssertEquals('Cookie header has both cookie sources','Cookie: a=1; b=2; c=3; d=4',lCookies[0]);
end;


procedure TTestHTTPClientHeaders.TestRequestCookiesOnly;

var
  lCookies: TStringArray;

begin
  with FClient.RequestCookies.Add do
    begin
    Name:='c';
    Value:='3';
    end;
  with FClient.RequestCookies.Add do
    begin
    Name:='d';
    Value:='4';
    end;
  lCookies:=HeaderLines(SendGet,'Cookie');
  AssertEquals('One Cookie header with only RequestCookies',1,Length(lCookies));
  AssertEquals('Cookie header has the RequestCookies','Cookie: c=3; d=4',lCookies[0]);
end;


procedure TTestHTTPClientHeaders.TestNoCookieLine;

begin
  AssertEquals('No Cookie header without cookies',0,Length(HeaderLines(SendGet,'Cookie')));
end;


initialization
  RegisterTest(TTestHTTPClientHeaders);
end.
