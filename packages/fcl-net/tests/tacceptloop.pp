program tacceptloop;

{ An exception raised while serving one client must not end TSocketServer.StartAccepting (issue #42062):
  the failing client sees EOF, the error is reported to OnAcceptSocketError and the next client is served,
  unless the error handler answers aeaStop. Build with -gh; expect PASS and 0 unfreed memory blocks. }

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils, Classes, fpsockets, ssockets, sslbase, sslsockets, opensslsockets;

const
  cBasePort = 28480;
  cHello: AnsiString = 'hello';

type
  TFailPoint = (fpHandlerHook, fpConnectQuery, fpBadCertificate, fpOnConnect);

  TServerThread = class(TThread)
  private
    FServer: TInetServer;
    FFailPoint: TFailPoint;
    FStopOnError: Boolean;
    FCalls: Integer;
    FReported: String;
    FEnded: Boolean;
    function FirstClient: Boolean;
    procedure DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);
    procedure DoConnectQuery(Sender: TObject; aSocket: TFPSocket; var aAllow: Boolean);
    procedure DoConnect(Sender: TObject; aData: TSocketStream);
    procedure DoAcceptError(Sender: TObject; aSocket: TFPSocket; E: Exception; var aAction: TAcceptErrorAction);
  public
    // Creates a server on aPort that fails at aFailPoint for the first client and starts the thread
    constructor Create(aPort: Word; aFailPoint: TFailPoint; aStopOnError: Boolean);
    // Stops the server if it still runs, waits for the thread and frees the server
    destructor Destroy; override;
    // Runs the accept loop
    procedure Execute; override;
    // Exception classes reported to OnAcceptSocketError, separated by spaces
    property Reported: String read FReported;
    // True once StartAccepting has returned
    property Ended: Boolean read FEnded;
  end;


constructor TServerThread.Create(aPort: Word; aFailPoint: TFailPoint; aStopOnError: Boolean);

begin
  FFailPoint:=aFailPoint;
  FStopOnError:=aStopOnError;
  FServer:=TInetServer.Create('127.0.0.1',aPort);
  FServer.ReuseAddress:=True;
  FServer.OnCreateClientSocketHandler:=@DoCreateHandler;
  FServer.OnConnectSocketQuery:=@DoConnectQuery;
  FServer.OnConnect:=@DoConnect;
  FServer.OnAcceptSocketError:=@DoAcceptError;
  inherited Create(False);
end;


destructor TServerThread.Destroy;

begin
  if not Finished then
    begin
    FServer.StopAccepting(True);
    WaitFor;
    end;
  FServer.Free;
  inherited Destroy;
end;


function TServerThread.FirstClient: Boolean;

begin
  Result:=FCalls=1;
end;


procedure TServerThread.DoConnectQuery(Sender: TObject; aSocket: TFPSocket; var aAllow: Boolean);

begin
  Inc(FCalls);
  aAllow:=True;
  if FirstClient and (FFailPoint=fpConnectQuery) then
    raise EInOutError.Create('connect query failed for the first client');
end;


procedure TServerThread.DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);

var
  lSSL: TSSLSocketHandler;

begin
  aHandler:=nil;
  if FirstClient and (FFailPoint=fpHandlerHook) then
    raise EInOutError.Create('handler hook failed for the first client');
  if FirstClient and (FFailPoint=fpBadCertificate) then
    begin
    lSSL:=TOpenSSLSocketHandler.Create;
    lSSL.CertificateData.Certificate.Value:=TEncoding.ASCII.GetBytes('not a certificate');
    lSSL.CertificateData.PrivateKey.Value:=TEncoding.ASCII.GetBytes('not a key');
    aHandler:=lSSL;
    end
  else
    aHandler:=TSocketHandler.Create;
end;


procedure TServerThread.DoConnect(Sender: TObject; aData: TSocketStream);

begin
  if FirstClient and (FFailPoint=fpOnConnect) then
    begin
    aData.Free;
    raise EInOutError.Create('OnConnect failed for the first client');
    end;
  aData.WriteBuffer(cHello[1],Length(cHello));
  aData.Free;
end;


procedure TServerThread.DoAcceptError(Sender: TObject; aSocket: TFPSocket; E: Exception; var aAction: TAcceptErrorAction);

begin
  FReported:=Trim(FReported+' '+E.ClassName);
  if FStopOnError then
    aAction:=aeaStop;
end;


procedure TServerThread.Execute;

begin
  try
    FServer.StartAccepting;
  except
    on E: Exception do
      FReported:=Trim(FReported+' escaped:'+E.ClassName);
  end;
  FEnded:=True;
end;


// Connects to aPort and reads; returns 'eof', 'got N bytes' or 'no data'
function TryClient(aPort: Word): String;

var
  lSocket: TInetSocket;
  lBuffer: array[0..15] of AnsiChar;
  lCount: Integer;

begin
  try
    lSocket:=TInetSocket.Create('127.0.0.1',aPort,TSocketHandler.Create);
    try
      lSocket.Connect;
      lSocket.IOTimeout:=1500;
      lCount:=lSocket.Read(lBuffer,SizeOf(lBuffer));
      if lCount>0 then
        Result:='got '+IntToStr(lCount)+' bytes'
      else if lCount=0 then
        Result:='eof'
      else
        Result:='no data';
    finally
      lSocket.Free;
    end;
  except
    on E: Exception do
      Result:=E.ClassName+': '+E.Message;
  end;
end;


// Runs one case; True if the client results and the reported errors are as expected
function RunCase(aPort: Word; aFailPoint: TFailPoint; aStopOnError: Boolean; const aName, aExpectReported: String): Boolean;

var
  lServer: TServerThread;
  lFirst, lSecond: String;
  lEnded: Boolean;

begin
  lServer:=TServerThread.Create(aPort,aFailPoint,aStopOnError);
  try
    Sleep(200);
    lFirst:=TryClient(aPort);
    Sleep(300);
    lEnded:=lServer.Ended;
    if aStopOnError then
      lSecond:='(not tried)'
    else
      lSecond:=TryClient(aPort);
    Writeln(aName,': client 1: ',lFirst,'; server ended: ',BoolToStr(lEnded,True),'; client 2: ',lSecond,'; reported: ',lServer.Reported);
    Result:=(lFirst='eof') and (lServer.Reported=aExpectReported);
    if aStopOnError then
      Result:=Result and lEnded
    else
      Result:=Result and not lEnded and (lSecond='got '+IntToStr(Length(cHello))+' bytes');
  finally
    lServer.Free;
  end;
end;


var
  lOK: Boolean;

begin
  lOK:=RunCase(cBasePort,fpHandlerHook,False,'handler hook raises','EInOutError');
  lOK:=RunCase(cBasePort+1,fpConnectQuery,False,'connect query raises','EInOutError') and lOK;
  lOK:=RunCase(cBasePort+2,fpBadCertificate,False,'invalid certificate','ESSL') and lOK;
  lOK:=RunCase(cBasePort+3,fpOnConnect,False,'OnConnect raises','EInOutError') and lOK;
  lOK:=RunCase(cBasePort+4,fpHandlerHook,True,'handler hook raises, aeaStop','EInOutError') and lOK;
  if lOK then
    Writeln('PASS')
  else
    begin
    Writeln('FAIL');
    ExitCode:=1;
    end;
end.
