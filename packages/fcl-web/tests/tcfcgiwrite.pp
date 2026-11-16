{ Tests TFCgiHandler writing to a connection that the client has reset, and terminating TFCgiHandler.Run. }
unit tcfcgiwrite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, fpcunit, testregistry, sockets, httpdefs, httproute, custfcgi;

type

  { TTestFCgiHandler }

  TTestFCgiHandler = class(TFCgiHandler)
  public
    // Waits for one request on the listening socket and handles it
    procedure ServeOne;
    // DoFastCGIWrite
    function Write(aHandle: THandle; const aBuf; aCount: Integer; out aErrorCode: Integer): Integer;
  end;

  { TTestFCgiWrite }

  TTestFCgiWrite = class(TTestCase)
  private
    FHandler: TTestFCgiHandler;
    FPort: Word;
    FStarted, FResetDone: TEvent;
    FBigRoute, FSmallRoute: THTTPRoute;
    FHandlerError: String;
    FHandlerDone: Boolean;
    procedure HandleBig(aRequest: TRequest; aResponse: TResponse);
    procedure HandleSmall(aRequest: TRequest; aResponse: TResponse);
    function Connect: TSocket;
    procedure SendRequest(aSocket: TSocket; const aPath: String; aKeepConnection: Boolean = False);
    function ReadResponse(aSocket: TSocket): String;
    function WaitForThread(aThread: TThread; aSeconds: Integer): Boolean;
    // Terminates the handler and checks that aThread, running the handler, ends
    procedure CheckTerminates(aThread: TThread);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestWriteToResetSocket;
    procedure TestClientResetDuringResponse;
    procedure TestServesAfterReset;
    procedure TestTerminateBeforeConnection;
    procedure TestTerminateAfterRequest;
    procedure TestTerminateOnKeptConnection;
  end;

implementation

{$IFDEF UNIX}
uses
  BaseUnix;
{$ENDIF}

const
  cRequestID = 1;
  cBigSize = 8*1024*1024;

type

  { TServeThread }

  TServeThread = class(TThread)
  private
    FHandler: TTestFCgiHandler;
    FCount: Integer;
    FError: String;
  protected
    procedure Execute; override;
  public
    constructor Create(aHandler: TTestFCgiHandler; aCount: Integer);
    property Error: String read FError;
  end;

  { TRunThread }

  TRunThread = class(TThread)
  private
    FHandler: TTestFCgiHandler;
    FError: String;
  protected
    procedure Execute; override;
  public
    constructor Create(aHandler: TTestFCgiHandler);
    property Error: String read FError;
  end;

  { TWriteThread }

  TWriteThread = class(TThread)
  private
    FHandler: TTestFCgiHandler;
    FSocket: TSocket;
    FResult, FErrorCode: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(aHandler: TTestFCgiHandler; aSocket: TSocket);
    property WriteResult: Integer read FResult;
    property ErrorCode: Integer read FErrorCode;
  end;

{ Unit helpers }

// Closes aSocket so that the peer gets a reset
procedure ResetSocket(aSocket: TSocket);

var
  lLinger: TLinger;

begin
  lLinger.l_onoff:=1;
  lLinger.l_linger:=0;
  fpSetSockOpt(aSocket,SOL_SOCKET,SO_LINGER,@lLinger,SizeOf(lLinger));
  CloseSocket(aSocket);
end;


// Makes a receive on aSocket fail after aSeconds without data
procedure SetReceiveTimeout(aSocket: TSocket; aSeconds: Integer);

var
{$IFDEF WINDOWS}
  lTimeout: DWORD;
{$ELSE}
  lTimeout: TTimeVal;
{$ENDIF}

begin
{$IFDEF WINDOWS}
  lTimeout:=aSeconds*1000;
{$ELSE}
  lTimeout.tv_sec:=aSeconds;
  lTimeout.tv_usec:=0;
{$ENDIF}
  fpSetSockOpt(aSocket,SOL_SOCKET,SO_RCVTIMEO,@lTimeout,SizeOf(lTimeout));
end;


// A port on 127.0.0.1 that is free at the moment of the call
function FreePort: Word;

var
  lSocket: TSocket;
  lAddr: TInetSockAddr;
  lLen: TSockLen;

begin
  lSocket:=fpSocket(AF_INET,SOCK_STREAM,0);
  FillChar(lAddr,SizeOf(lAddr),0);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
  fpBind(lSocket,@lAddr,SizeOf(lAddr));
  lLen:=SizeOf(lAddr);
  fpGetSockName(lSocket,@lAddr,@lLen);
  Result:=NToHs(lAddr.sin_port);
  CloseSocket(lSocket);
end;


// A FastCGI record of type aType for cRequestID with aContent
function FCGIRecord(aType: Byte; const aContent: RawByteString): RawByteString;

begin
  Result:=AnsiChar(1)+AnsiChar(aType)+AnsiChar(0)+AnsiChar(cRequestID)
    +AnsiChar(Length(aContent) shr 8)+AnsiChar(Length(aContent) and $FF)+AnsiChar(0)+AnsiChar(0)+aContent;
end;


// A FastCGI name-value pair with names and values shorter than 128 bytes
function FCGIParam(const aName, aValue: RawByteString): RawByteString;

begin
  Result:=AnsiChar(Length(aName))+AnsiChar(Length(aValue))+aName+aValue;
end;


// Reads exactly aCount bytes from aSocket; False when the connection ends first
function ReadExact(aSocket: TSocket; var aBuf; aCount: Integer): Boolean;

var
  P: PByte;
  lRead: Integer;

begin
  P:=@aBuf;
  while aCount>0 do
    begin
    lRead:=fpRecv(aSocket,P,aCount,0);
    if lRead<=0 then
      Exit(False);
    Inc(P,lRead);
    Dec(aCount,lRead);
    end;
  Result:=True;
end;


{ TTestFCgiHandler }

procedure TTestFCgiHandler.ServeOne;

var
  lRequest: TRequest;
  lResponse: TResponse;

begin
  if WaitForRequest(lRequest,lResponse) then
    DoHandleRequest(lRequest,lResponse);
end;


function TTestFCgiHandler.Write(aHandle: THandle; const aBuf; aCount: Integer; out aErrorCode: Integer): Integer;

begin
  Result:=DoFastCGIWrite(aHandle,aBuf,aCount,aErrorCode);
end;


{ TServeThread }

constructor TServeThread.Create(aHandler: TTestFCgiHandler; aCount: Integer);

begin
  FHandler:=aHandler;
  FCount:=aCount;
  inherited Create(False);
end;


procedure TServeThread.Execute;

var
  I: Integer;

begin
  try
    for I:=1 to FCount do
      FHandler.ServeOne;
  except
    on E: Exception do
      FError:=E.ClassName+': '+E.Message;
  end;
end;


{ TRunThread }

constructor TRunThread.Create(aHandler: TTestFCgiHandler);

begin
  FHandler:=aHandler;
  inherited Create(False);
end;


procedure TRunThread.Execute;

begin
  try
    FHandler.Run;
  except
    on E: Exception do
      FError:=E.ClassName+': '+E.Message;
  end;
end;


{ TWriteThread }

constructor TWriteThread.Create(aHandler: TTestFCgiHandler; aSocket: TSocket);

begin
  FHandler:=aHandler;
  FSocket:=aSocket;
  inherited Create(False);
end;


procedure TWriteThread.Execute;

var
  lBuf: array[0..65535] of Byte;
  I: Integer;

begin
  FillChar(lBuf,SizeOf(lBuf),0);
  for I:=1 to 10 do
    begin
    FResult:=FHandler.Write(THandle(FSocket),lBuf,SizeOf(lBuf),FErrorCode);
    if FResult<0 then
      Exit;
    end;
end;


{ TTestFCgiWrite }

procedure TTestFCgiWrite.HandleBig(aRequest: TRequest; aResponse: TResponse);

begin
  try
    FStarted.SetEvent;
    FResetDone.WaitFor(5000);
    Sleep(200);
    aResponse.ContentType:='text/plain';
    aResponse.Content:=StringOfChar('x',cBigSize);
    aResponse.SendContent;
    FHandlerDone:=True;
  except
    on E: Exception do
      begin
      FHandlerError:=E.Message;
      Raise;
      end;
  end;
end;


procedure TTestFCgiWrite.HandleSmall(aRequest: TRequest; aResponse: TResponse);

begin
  aResponse.ContentType:='text/plain';
  aResponse.Content:='small ok';
end;


function TTestFCgiWrite.Connect: TSocket;

var
  lAddr: TInetSockAddr;
  I: Integer;

begin
  FillChar(lAddr,SizeOf(lAddr),0);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
  lAddr.sin_port:=HToNs(FPort);
  for I:=1 to 50 do
    begin
    Result:=fpSocket(AF_INET,SOCK_STREAM,0);
    if fpConnect(Result,@lAddr,SizeOf(lAddr))=0 then
      begin
      SetReceiveTimeout(Result,10);
      Exit;
      end;
    CloseSocket(Result);
    Sleep(100);
    end;
  Fail('Cannot connect to the FastCGI handler on port '+IntToStr(FPort));
end;


procedure TTestFCgiWrite.SendRequest(aSocket: TSocket; const aPath: String; aKeepConnection: Boolean);

var
  lData, lParams: RawByteString;

begin
  lParams:=FCGIParam('REQUEST_METHOD','GET')+FCGIParam('SCRIPT_NAME','')+FCGIParam('PATH_INFO',aPath)
    +FCGIParam('REQUEST_URI',aPath)+FCGIParam('QUERY_STRING','')+FCGIParam('SERVER_PROTOCOL','HTTP/1.1');
  lData:=FCGIRecord(1,AnsiChar(0)+AnsiChar(1)+AnsiChar(Ord(aKeepConnection))+StringOfChar(AnsiChar(0),5))
    +FCGIRecord(4,lParams)+FCGIRecord(4,'')+FCGIRecord(5,'');
  AssertEquals('Request sent',Length(lData),fpSend(aSocket,@lData[1],Length(lData),0));
end;


function TTestFCgiWrite.ReadResponse(aSocket: TSocket): String;

var
  lHeader: array[0..7] of Byte;
  lContent: RawByteString;
  lLen: Integer;

begin
  Result:='';
  while ReadExact(aSocket,lHeader,SizeOf(lHeader)) do
    begin
    lLen:=lHeader[4] shl 8+lHeader[5]+lHeader[6];
    SetLength(lContent,lLen);
    if (lLen>0) and not ReadExact(aSocket,lContent[1],lLen) then
      Break;
    if lHeader[1]=6 then
      Result:=Result+Copy(lContent,1,lHeader[4] shl 8+lHeader[5]);
    if lHeader[1]=3 then
      Break;
    end;
end;


function TTestFCgiWrite.WaitForThread(aThread: TThread; aSeconds: Integer): Boolean;

var
  lEnd: QWord;

begin
  lEnd:=GetTickCount64+QWord(aSeconds)*1000;
  while not aThread.Finished and (GetTickCount64<lEnd) do
    Sleep(20);
  Result:=aThread.Finished;
end;


procedure TTestFCgiWrite.CheckTerminates(aThread: TThread);

begin
  FHandler.Terminate;
  if not WaitForThread(aThread,3) then
    begin
    // The thread still uses the handler: leak both
    FHandler:=nil;
    Fail('TFCgiHandler.Run does not end after Terminate');
    end;
  AssertEquals('Run ends without exception','',TRunThread(aThread).Error);
end;


procedure TTestFCgiWrite.SetUp;

begin
  FStarted:=TEvent.Create(nil,True,False,'');
  FResetDone:=TEvent.Create(nil,True,False,'');
  FHandlerError:='';
  FHandlerDone:=False;
  FPort:=FreePort;
  FHandler:=TTestFCgiHandler.Create(nil);
  FHandler.Address:='127.0.0.1';
  FHandler.Port:=FPort;
  FBigRoute:=HTTPRouter.RegisterRoute('/big',@HandleBig);
  FSmallRoute:=HTTPRouter.RegisterRoute('/small',@HandleSmall);
end;


procedure TTestFCgiWrite.TearDown;

begin
  HTTPRouter.DeleteRoute(FBigRoute);
  HTTPRouter.DeleteRoute(FSmallRoute);
  FreeAndNil(FHandler);
  FreeAndNil(FStarted);
  FreeAndNil(FResetDone);
end;


procedure TTestFCgiWrite.TestWriteToResetSocket;

var
  lListen, lServer, lClient: TSocket;
  lAddr: TInetSockAddr;
  lLen: TSockLen;
  lThread: TWriteThread;

begin
  lListen:=fpSocket(AF_INET,SOCK_STREAM,0);
  FillChar(lAddr,SizeOf(lAddr),0);
  lAddr.sin_family:=AF_INET;
  lAddr.sin_addr:=StrToNetAddr('127.0.0.1');
  fpBind(lListen,@lAddr,SizeOf(lAddr));
  lLen:=SizeOf(lAddr);
  fpGetSockName(lListen,@lAddr,@lLen);
  fpListen(lListen,1);
  lClient:=fpSocket(AF_INET,SOCK_STREAM,0);
  fpConnect(lClient,@lAddr,SizeOf(lAddr));
  lServer:=fpAccept(lListen,nil,nil);
  CloseSocket(lListen);
  ResetSocket(lClient);
  Sleep(200);
  lThread:=TWriteThread.Create(FHandler,lServer);
  if not WaitForThread(lThread,5) then
    Fail('DoFastCGIWrite does not return on a reset connection');
  try
    AssertEquals('DoFastCGIWrite fails on a reset connection',-1,lThread.WriteResult);
    AssertTrue('DoFastCGIWrite returns the socket error',lThread.ErrorCode<>0);
  finally
    lThread.Free;
    CloseSocket(lServer);
  end;
end;


procedure TTestFCgiWrite.TestClientResetDuringResponse;

var
  lThread: TServeThread;
  lClient: TSocket;

begin
  lThread:=TServeThread.Create(FHandler,1);
  lClient:=Connect;
  SendRequest(lClient,'/big');
  AssertTrue('Handler started',FStarted.WaitFor(5000)=wrSignaled);
  ResetSocket(lClient);
  FResetDone.SetEvent;
  if not WaitForThread(lThread,10) then
    Fail('The handler does not end the request when the client resets the connection');
  try
    AssertEquals('No exception leaves the request handling','',lThread.Error);
    AssertEquals('Writing to a reset connection raises no exception in the handler','',FHandlerError);
    AssertTrue('The handler runs to its end',FHandlerDone);
  finally
    lThread.Free;
  end;
end;


procedure TTestFCgiWrite.TestServesAfterReset;

var
  lThread: TServeThread;
  lClient: TSocket;

begin
  lThread:=TServeThread.Create(FHandler,2);
  lClient:=Connect;
  SendRequest(lClient,'/big');
  AssertTrue('Handler started',FStarted.WaitFor(5000)=wrSignaled);
  ResetSocket(lClient);
  FResetDone.SetEvent;
  lClient:=Connect;
  try
    SendRequest(lClient,'/small');
    AssertTrue('The next request is answered',Pos('small ok',ReadResponse(lClient))>0);
  finally
    CloseSocket(lClient);
  end;
  if not WaitForThread(lThread,10) then
    Fail('The handler does not end');
  try
    AssertEquals('No exception leaves the request handling','',lThread.Error);
  finally
    lThread.Free;
  end;
end;


procedure TTestFCgiWrite.TestTerminateBeforeConnection;

var
  lThread: TRunThread;

begin
  lThread:=TRunThread.Create(FHandler);
  Sleep(300);
  CheckTerminates(lThread);
  lThread.Free;
end;


procedure TTestFCgiWrite.TestTerminateAfterRequest;

var
  lThread: TRunThread;
  lClient: TSocket;

begin
  lThread:=TRunThread.Create(FHandler);
  lClient:=Connect;
  try
    SendRequest(lClient,'/small');
    AssertTrue('Request answered',Pos('small ok',ReadResponse(lClient))>0);
  finally
    CloseSocket(lClient);
  end;
  CheckTerminates(lThread);
  lThread.Free;
end;


procedure TTestFCgiWrite.TestTerminateOnKeptConnection;

var
  lThread: TRunThread;
  lClient: TSocket;
  lByte: Byte;

begin
  lThread:=TRunThread.Create(FHandler);
  lClient:=Connect;
  try
    SendRequest(lClient,'/small',True);
    AssertTrue('Request answered',Pos('small ok',ReadResponse(lClient))>0);
    CheckTerminates(lThread);
    AssertTrue('The kept connection is closed',fpRecv(lClient,@lByte,1,0)<=0);
  finally
    CloseSocket(lClient);
  end;
  lThread.Free;
end;


initialization
  RegisterTest(TTestFCgiWrite);
end.
