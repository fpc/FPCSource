{ Tests the response limits of TFPHTTPClient against a local TFPHttpServer. }
unit tchttpclientlimits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fphttpserver, fphttpclient;

type

  { TLimitTestServerThread }

  TLimitTestServerThread = class(TThread)
  private
    FServer: TFPHttpServer;
    FDone: Boolean;
    procedure DoRequest(Sender: TObject; var aRequest: TFPHTTPConnectionRequest;
      var aResponse: TFPHTTPConnectionResponse);
  protected
    procedure Execute; override;
  public
    // Create a suspended thread with a server on 127.0.0.1:aPort
    constructor Create(aPort: Word);
    destructor Destroy; override;
    // The server; Active is set to True in Execute
    property Server: TFPHttpServer read FServer;
    // True when Server.Active:=True has returned
    property Done: Boolean read FDone;
  end;

  { TTestHTTPClientLimits }

  TTestHTTPClientLimits = class(TTestCase)
  private
    FThread: TLimitTestServerThread;
    FPort: Word;
    FClient: TFPHTTPClient;
    function URL(aSize: Integer; aChunked: Boolean = False): String;
    procedure WaitServerReady;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDefaultMaxResponseSizeIsUnlimited;
    procedure TestLimitsArePublished;
    procedure TestMaxResponseSizeAbove2GiB;
    procedure TestResponseAboveLimitRaises;
    procedure TestResponseAtLimitSucceeds;
    procedure TestDefaultReadsWholeResponse;
    procedure TestChunkedAboveLimitRaises;
    procedure TestChunkedAtLimitSucceeds;
    procedure TestChunkedDefaultReadsWholeResponse;
    procedure TestMemoryStreamNearPtrIntLimitRaises;
    procedure TestChunkedMemoryStreamNearPtrIntLimitRaises;
{$IF SizeOf(PtrInt)=4}
    procedure TestStringAboveAddressSpaceRaises;
{$ENDIF}
  end;

implementation

uses
  Sockets, TypInfo;

const
  cBodySize = 100000;
  cChunkSize = 1000;
  cStartTimeoutMS = 5000;


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


{ TLimitTestServerThread }

constructor TLimitTestServerThread.Create(aPort: Word);

begin
  inherited Create(True);
  FServer:=TFPHttpServer.Create(nil);
  FServer.Port:=aPort;
  FServer.Address:='127.0.0.1';
  FServer.OnRequest:=@DoRequest;
end;


destructor TLimitTestServerThread.Destroy;

begin
  inherited Destroy;
  FreeAndNil(FServer);
end;


procedure TLimitTestServerThread.DoRequest(Sender: TObject; var aRequest: TFPHTTPConnectionRequest;
  var aResponse: TFPHTTPConnectionResponse);

var
  lSize, lChunk: Integer;
  lBody: String;

begin
  lSize:=StrToIntDef(aRequest.QueryFields.Values['size'], 0);
  aResponse.ContentType:='application/octet-stream';
  if aRequest.QueryFields.Values['chunked']='1' then
    begin
    lBody:='';
    while lSize>0 do
      begin
      lChunk:=cChunkSize;
      if lChunk>lSize then
        lChunk:=lSize;
      lBody:=lBody+IntToHex(lChunk, 1)+#13#10+StringOfChar('x', lChunk)+#13#10;
      Dec(lSize, lChunk);
      end;
    lBody:=lBody+'0'#13#10#13#10;
    aResponse.SetCustomHeader('Transfer-Encoding', 'chunked');
    end
  else
    lBody:=StringOfChar('x', lSize);
  aResponse.FreeContentStream:=True;
  aResponse.ContentStream:=TStringStream.Create(lBody);
  if aRequest.QueryFields.Values['claim']<>'' then
    aResponse.ContentLength:=StrToInt64(aRequest.QueryFields.Values['claim']);
end;


procedure TLimitTestServerThread.Execute;

begin
  try
    FServer.Active:=True;
  except
    on Exception do ;
  end;
  FDone:=True;
end;


{ TTestHTTPClientLimits }

procedure TTestHTTPClientLimits.SetUp;

begin
  FPort:=FreePort;
  AssertTrue('a free port was found', FPort<>0);
  FThread:=TLimitTestServerThread.Create(FPort);
  FThread.Start;
  FClient:=TFPHTTPClient.Create(nil);
  FClient.IOTimeout:=5000;
  FClient.ConnectTimeout:=2000;
  WaitServerReady;
end;


procedure TTestHTTPClientLimits.TearDown;

var
  lStart: QWord;

begin
  FreeAndNil(FClient);
  FThread.Server.Active:=False;
  lStart:=GetTickCount64;
  while not FThread.Done and (GetTickCount64-lStart<cStartTimeoutMS) do
    Sleep(10);
  if FThread.Done then
    FreeAndNil(FThread);
end;


function TTestHTTPClientLimits.URL(aSize: Integer; aChunked: Boolean): String;

begin
  Result:=Format('http://127.0.0.1:%d/data?size=%d', [FPort, aSize]);
  if aChunked then
    Result:=Result+'&chunked=1';
end;


procedure TTestHTTPClientLimits.WaitServerReady;

var
  lStart: QWord;
  lReply: String;

begin
  lStart:=GetTickCount64;
  lReply:='';
  repeat
    try
      lReply:=FClient.Get(URL(1));
    except
      on Exception do
        Sleep(20);
    end;
  until (lReply<>'') or (GetTickCount64-lStart>cStartTimeoutMS);
  AssertEquals('server answers', 'x', lReply);
end;


procedure TTestHTTPClientLimits.TestDefaultMaxResponseSizeIsUnlimited;

begin
  AssertEquals('default MaxResponseSize is 0, no limit', 0, FClient.MaxResponseSize);
end;


procedure TTestHTTPClientLimits.TestLimitsArePublished;

begin
  AssertTrue('MaxResponseSize is published', IsPublishedProp(FClient, 'MaxResponseSize'));
  AssertTrue('MaxHeaderLineLength is published', IsPublishedProp(FClient, 'MaxHeaderLineLength'));
  AssertTrue('MaxHeaderCount is published', IsPublishedProp(FClient, 'MaxHeaderCount'));
end;


procedure TTestHTTPClientLimits.TestMaxResponseSizeAbove2GiB;

const
  c3GiB = Int64(3) * 1024 * 1024 * 1024;

begin
  FClient.MaxResponseSize:=c3GiB;
  AssertEquals('MaxResponseSize keeps a value above 2 GiB', c3GiB, FClient.MaxResponseSize);
end;


procedure TTestHTTPClientLimits.TestResponseAboveLimitRaises;

var
  lRaised: Boolean;

begin
  FClient.MaxResponseSize:=cBodySize div 2;
  lRaised:=False;
  try
    FClient.Get(URL(cBodySize));
  except
    on EHTTPClient do
      lRaised:=True;
  end;
  AssertTrue('a response above MaxResponseSize raises EHTTPClient', lRaised);
end;


procedure TTestHTTPClientLimits.TestResponseAtLimitSucceeds;

begin
  FClient.MaxResponseSize:=cBodySize;
  AssertEquals('a response of exactly MaxResponseSize bytes is read', cBodySize,
    Length(FClient.Get(URL(cBodySize))));
end;


procedure TTestHTTPClientLimits.TestDefaultReadsWholeResponse;

begin
  AssertEquals('with the default limit the whole response is read', cBodySize,
    Length(FClient.Get(URL(cBodySize))));
end;


procedure TTestHTTPClientLimits.TestChunkedAboveLimitRaises;

var
  lRaised: Boolean;

begin
  FClient.MaxResponseSize:=cBodySize div 2;
  lRaised:=False;
  try
    FClient.Get(URL(cBodySize, True));
  except
    on EHTTPClient do
      lRaised:=True;
  end;
  AssertTrue('a chunked response above MaxResponseSize raises EHTTPClient', lRaised);
end;


procedure TTestHTTPClientLimits.TestChunkedAtLimitSucceeds;

begin
  FClient.MaxResponseSize:=cBodySize;
  AssertEquals('chunk framing does not count towards MaxResponseSize', cBodySize,
    Length(FClient.Get(URL(cBodySize, True))));
end;


procedure TTestHTTPClientLimits.TestChunkedDefaultReadsWholeResponse;

begin
  AssertEquals('with the default limit the whole chunked response is read', cBodySize,
    Length(FClient.Get(URL(cBodySize, True))));
end;


procedure TTestHTTPClientLimits.TestMemoryStreamNearPtrIntLimitRaises;

var
  lStream: TMemoryStream;
  lRaised: Boolean;

begin
  lStream:=TMemoryStream.Create;
  try
    lStream.Position:=High(PtrInt)-(cChunkSize div 2);
    lRaised:=False;
    try
      FClient.Get(URL(cBodySize), lStream);
    except
      on E: EHTTPClient do
        lRaised:=E.ClassType=EHTTPClient;
    end;
    AssertTrue('content that cannot fit below High(PtrInt) raises EHTTPClient', lRaised);
    AssertEquals('nothing is written to the memory stream', 0, lStream.Size);
  finally
    lStream.Free;
  end;
end;


procedure TTestHTTPClientLimits.TestChunkedMemoryStreamNearPtrIntLimitRaises;

var
  lStream: TMemoryStream;
  lRaised: Boolean;

begin
  lStream:=TMemoryStream.Create;
  try
    lStream.Position:=High(PtrInt)-(cChunkSize div 2);
    lRaised:=False;
    try
      FClient.Get(URL(cBodySize, True), lStream);
    except
      on E: EHTTPClient do
        lRaised:=E.ClassType=EHTTPClient;
    end;
    AssertTrue('chunked content that cannot fit below High(PtrInt) raises EHTTPClient', lRaised);
    AssertEquals('nothing is written to the memory stream', 0, lStream.Size);
  finally
    lStream.Free;
  end;
end;


{$IF SizeOf(PtrInt)=4}
procedure TTestHTTPClientLimits.TestStringAboveAddressSpaceRaises;

var
  lRaised: Boolean;

begin
  lRaised:=False;
  try
    FClient.Get(URL(10)+'&claim=3221225472');
  except
    on E: EHTTPClient do
      lRaised:=E.ClassType=EHTTPClient;
  end;
  AssertTrue('a string Get with a Content-Length above High(PtrInt) raises EHTTPClient', lRaised);
end;
{$ENDIF}


initialization
  RegisterTest(TTestHTTPClientLimits);
end.
