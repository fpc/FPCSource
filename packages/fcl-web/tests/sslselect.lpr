program sslselect;

{ CanRead on a TLS socket must report plaintext buffered by the TLS library (issue #42060),
  and TFPHttpServer over HTTPS must answer requests that arrive as one TLS record of more than 4096 bytes.
  usage: sslselect [runs, default 5] [openssl|gnutls, default openssl; gnutls on Unix only] }

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SysUtils, DateUtils, ssockets, sslbase, sslsockets, opensslsockets,
  {$ifdef unix}gnutlssockets,{$endif}
  fphttpserver, httpdefs;

const
  ProbePort = 44391;
  HttpPort = 44392;
  ServerReadTimeout = 3000;
  ProbeSize = 6000;
  ProbeRead = 4096;
  BodySizes: array[0..2] of Integer = (1000, 6000, 12000);

type
  TProbeServer = class(TThread)
  private
    FServer: TInetServer;
    FReport: String;
    FPending: Integer;
    FReadable: Boolean;
    procedure DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);
    procedure DoConnect(Sender: TObject; aData: TSocketStream);
  public
    // Creates the listening server on ProbePort and starts the thread
    constructor Create;
    // Frees the listening server
    destructor Destroy; override;
    // Accepts connections until StopServer is called
    procedure Execute; override;
    // Stops accepting connections
    procedure StopServer;
    // Text describing what the server saw, empty until a client was handled
    property Report: String read FReport;
    // BytesAvailable after reading ProbeRead bytes
    property Pending: Integer read FPending;
    // CanRead(1000) after reading ProbeRead bytes
    property Readable: Boolean read FReadable;
  end;

  THttpServerThread = class(TThread)
  private
    FServer: TFPHttpServer;
    FLastError: String;
    procedure DoRequest(Sender: TObject; var aRequest: TFPHTTPConnectionRequest;
      var aResponse: TFPHTTPConnectionResponse);
    procedure DoRequestError(Sender: TObject; E: Exception);
  public
    // Creates an HTTPS server on HttpPort and starts the thread
    constructor Create;
    // Frees the HTTP server
    destructor Destroy; override;
    // Runs the server until StopServer is called
    procedure Execute; override;
    // Deactivates the server
    procedure StopServer;
    // Last exception passed to OnRequestError; reading and clearing is not synchronised
    property LastError: String read FLastError write FLastError;
  end;

var
  gHandlerClass: TSSLSocketHandlerClass;
  gCertificate, gPrivateKey: TBytes;


// Creates a handler of the selected TLS library with the test certificate
function NewHandler: TSSLSocketHandler;

begin
  Result:=gHandlerClass.Create;
  Result.CertificateData.HostName:='localhost';
  Result.CertificateData.Certificate.Value:=gCertificate;
  Result.CertificateData.PrivateKey.Value:=gPrivateKey;
end;


// Generates the self-signed certificate used by both servers
procedure CreateCertificate;

var
  lHandler: TSSLSocketHandler;
  lCertAndKey: TCertAndKey;

begin
  lHandler:=gHandlerClass.Create;
  try
    lHandler.CertGenerator.HostName:='localhost';
    lCertAndKey:=lHandler.CertGenerator.CreateCertificateAndKey;
  finally
    lHandler.Free;
  end;
  gCertificate:=lCertAndKey.Certificate;
  gPrivateKey:=lCertAndKey.PrivateKey;
end;


// Opens a TLS client connection to 127.0.0.1:aPort
function ConnectTo(aPort: Word): TInetSocket;

var
  lHandler: TSSLSocketHandler;

begin
  lHandler:=gHandlerClass.Create;
  lHandler.CertificateData.HostName:='localhost';
  Result:=TInetSocket.Create('127.0.0.1',aPort,lHandler);
  Result.IOTimeout:=15000;
  Result.Connect;
end;


constructor TProbeServer.Create;

begin
  FServer:=TInetServer.Create('127.0.0.1',ProbePort,NewHandler);
  FServer.ReuseAddress:=True;
  FServer.OnCreateClientSocketHandler:=@DoCreateHandler;
  FServer.OnConnect:=@DoConnect;
  inherited Create(False);
end;


destructor TProbeServer.Destroy;

begin
  if not Finished then
    begin
    StopServer;
    WaitFor;
    end;
  FServer.Free;
  inherited Destroy;
end;


procedure TProbeServer.DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);

begin
  aHandler:=NewHandler;
end;


procedure TProbeServer.DoConnect(Sender: TObject; aData: TSocketStream);

var
  lBuffer: array[0..ProbeRead-1] of Byte;
  lGot, lTotal: Integer;

begin
  try
    lTotal:=0;
    repeat
      lGot:=aData.Read(lBuffer[lTotal],ProbeRead-lTotal);
      if lGot>0 then
        Inc(lTotal,lGot);
    until (lGot<=0) or (lTotal=ProbeRead);
    FPending:=aData.Handler.BytesAvailable;
    FReadable:=aData.CanRead(1000);
    FReport:=Format('read=%d BytesAvailable=%d CanRead(1000)=%s',[lTotal,FPending,BoolToStr(FReadable,True)]);
  except
    on E: Exception do
      FReport:='server exception: '+E.ClassName+': '+E.Message;
  end;
  aData.Free;
end;


procedure TProbeServer.Execute;

begin
  FServer.StartAccepting;
end;


procedure TProbeServer.StopServer;

begin
  FServer.StopAccepting(True);
end;


constructor THttpServerThread.Create;

begin
  FServer:=TFPHttpServer.Create(nil);
  FServer.CertificateData.HostName:='localhost';
  FServer.CertificateData.Certificate.Value:=gCertificate;
  FServer.CertificateData.PrivateKey.Value:=gPrivateKey;
  FServer.Port:=HttpPort;
  FServer.Address:='127.0.0.1';
  FServer.UseSSL:=True;
  FServer.ThreadMode:=tmThread;
  FServer.KeepConnections:=False;
  FServer.RequestReadTimeout:=ServerReadTimeout;
  FServer.OnRequest:=@DoRequest;
  FServer.OnRequestError:=@DoRequestError;
  inherited Create(False);
end;


destructor THttpServerThread.Destroy;

begin
  if not Finished then
    begin
    StopServer;
    WaitFor;
    end;
  FServer.Free;
  inherited Destroy;
end;


procedure THttpServerThread.DoRequest(Sender: TObject; var aRequest: TFPHTTPConnectionRequest;
  var aResponse: TFPHTTPConnectionResponse);

begin
  aResponse.Code:=200;
  aResponse.Content:=Format('received %d bytes',[Length(aRequest.Content)]);
end;


procedure THttpServerThread.DoRequestError(Sender: TObject; E: Exception);

begin
  FLastError:=E.ClassName+': '+E.Message;
end;


procedure THttpServerThread.Execute;

begin
  FServer.Active:=True;
end;


procedure THttpServerThread.StopServer;

begin
  FServer.Active:=False;
end;


// Runs the probe; True when buffered plaintext is reported as readable
function RunProbe: Boolean;

var
  lServer: TProbeServer;
  lClient: TInetSocket;
  lData: AnsiString;
  lWait: Integer;

begin
  Result:=False;
  lServer:=TProbeServer.Create;
  try
    Sleep(200);
    try
      lClient:=ConnectTo(ProbePort);
      try
        lData:=StringOfChar('x',ProbeSize);
        lClient.WriteBuffer(lData[1],Length(lData));
        lWait:=0;
        while (lServer.Report='') and (lWait<300) do
          begin
          Sleep(10);
          Inc(lWait);
          end;
      finally
        lClient.Free;
      end;
      Writeln('probe: client sent ',ProbeSize,' bytes in one write; server ',lServer.Report);
      Result:=(lServer.Pending>0) and lServer.Readable;
    except
      on E: Exception do
        Writeln('probe: client exception: ',E.ClassName,': ',E.Message);
    end;
  finally
    lServer.Free;
  end;
end;


// Posts a body of aSize bytes in one write and returns the status line and body start
function HttpPost(aSize: Integer): String;

var
  lClient: TInetSocket;
  lRequest, lResponse: AnsiString;
  lBuffer: array[0..4095] of AnsiChar;
  lCount, lPos: Integer;

begin
  lRequest:='POST /x HTTP/1.1'#13#10'Host: localhost'#13#10+
    'Content-Length: '+IntToStr(aSize)+#13#10+
    'Connection: close'#13#10#13#10+StringOfChar('b',aSize);
  lResponse:='';
  try
    lClient:=ConnectTo(HttpPort);
    try
      lClient.WriteBuffer(lRequest[1],Length(lRequest));
      repeat
        lCount:=lClient.Read(lBuffer,SizeOf(lBuffer));
        if lCount>0 then
          lResponse:=lResponse+Copy(lBuffer,1,lCount);
      until lCount<=0;
    finally
      lClient.Free;
    end;
    lPos:=Pos(#13#10,lResponse);
    if lPos>0 then
      Result:=Copy(lResponse,1,lPos-1)
    else if lResponse='' then
      Result:='(connection closed, no response)'
    else
      Result:=lResponse;
    lPos:=Pos(#13#10#13#10,lResponse);
    if lPos>0 then
      Result:=Result+' | '+Trim(Copy(lResponse,lPos+4,60));
  except
    on E: Exception do
      Result:='client exception: '+E.ClassName+': '+E.Message;
  end;
end;


// Runs aRuns POSTs per body size; True when all of them were answered with the full body
function RunHttp(aRuns: Integer): Boolean;

var
  lServer: THttpServerThread;
  lSize, lRun, lOK: Integer;
  lLine: String;

begin
  Result:=True;
  lServer:=THttpServerThread.Create;
  try
    Sleep(300);
    for lSize in BodySizes do
      begin
      lOK:=0;
      lServer.LastError:='';
      for lRun:=1 to aRuns do
        begin
        lLine:=HttpPost(lSize);
        if Pos(Format('received %d bytes',[lSize]),lLine)>0 then
          Inc(lOK)
        else if lOK+1=lRun then
          Writeln('http: body=',lSize,' client saw: ',lLine);
        end;
      Writeln(Format('http: body=%5d  %d/%d answered 200 with the full body',[lSize,lOK,aRuns]));
      if lServer.LastError<>'' then
        Writeln(Format('http: body=%5d  server OnRequestError: %s',[lSize,lServer.LastError]));
      if lOK<>aRuns then
        Result:=False;
      end;
    lServer.StopServer;
    lServer.WaitFor;
  finally
    lServer.Free;
  end;
end;


var
  lRuns: Integer;
  lLibrary: String;
  lProbeOK, lHttpOK: Boolean;

begin
  lRuns:=StrToIntDef(ParamStr(1),5);
  lLibrary:=LowerCase(ParamStr(2));
  if lLibrary='' then
    lLibrary:='openssl';
  if lLibrary='openssl' then
    gHandlerClass:=TOpenSSLSocketHandler
{$ifdef unix}
  else if lLibrary='gnutls' then
    gHandlerClass:=TGNUTLSSocketHandler
{$endif}
  else
    begin
    Writeln('unknown TLS library: ',lLibrary);
    Halt(2);
    end;
  TSSLSocketHandler.SetDefaultHandlerClass(gHandlerClass);
  Writeln('TLS handler: ',gHandlerClass.ClassName);
  CreateCertificate;
  lProbeOK:=RunProbe;
  lHttpOK:=RunHttp(lRuns);
  if lProbeOK and lHttpOK then
    Writeln('PASS')
  else
    begin
    Writeln('FAIL');
    ExitCode:=1;
    end;
end.
