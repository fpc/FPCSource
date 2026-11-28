{ TSSLContext.ActivateServerSNI with three virtual hosts on a local TLS server: certificate served and
  acknowledgement of the SNI name per client name. Exit code is the number of failed checks. }
program tsniserver;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads, baseunix,{$endif}
  Classes, SysUtils, dynlibs, ctypes, ssockets, sslbase, openssl, fpopenssl;

const
  ServerPort = 44397;

type
  TSslGetSession = function(aSSL: PSSL): Pointer; cdecl;
  TSslSessionGet0Hostname = function(aSession: Pointer): PAnsiChar; cdecl;

  { TVHostServer }

  TVHostServer = class(TThread)
  private
    FServer: TInetServer;
    FHosts: TTlsExtCtx;
    procedure DoConnect(Sender: TObject; aData: TSocketStream);
  protected
    procedure Execute; override;
  public
    // Creates a TLS server on 127.0.0.1:aPort with virtual hosts main.test, alpha.test and beta.test
    constructor Create(aPort: Word);
    // Stops accepting and frees the server and the contexts
    destructor Destroy; override;
  end;

var
  gFailures: Integer;
  gClientCtx: TSSLContext;
  gGetSession: TSslGetSession;
  gGetHostname: TSslSessionGet0Hostname;


// Context with a generated certificate and key for aHost
function CreateHostContext(const aHost: String): TSSLContext;

var
  lGen: TOpenSSLX509Certificate;
  lCertAndKey: TCertAndKey;
  lData: TSSLData;

begin
  lGen:=TOpenSSLX509Certificate.Create;
  try
    lGen.HostName:=aHost;
    lCertAndKey:=lGen.CreateCertificateAndKey;
  finally
    lGen.Free;
  end;
  Result:=TSSLContext.Create(stAny);
  lData:=TSSLData.Create;
  try
    lData.Value:=lCertAndKey.Certificate;
    if Result.UseCertificate(lData)<>1 then
      raise Exception.Create('Cannot use the certificate for '+aHost);
    lData.Value:=lCertAndKey.PrivateKey;
    if Result.UsePrivateKey(lData)<>1 then
      raise Exception.Create('Cannot use the key for '+aHost);
  finally
    lData.Free;
  end;
end;


{ TVHostServer }

constructor TVHostServer.Create(aPort: Word);

const
  Names: array[0..2] of String = ('main.test','alpha.test','beta.test');

var
  I: Integer;

begin
  SetLength(FHosts,Length(Names));
  for I:=0 to High(Names) do
    begin
    FHosts[I].CTX:=CreateHostContext(Names[I]);
    FHosts[I].domains:=[Names[I]];
    end;
  FHosts[0].CTX.ActivateServerSNI(FHosts);
  FServer:=TInetServer.Create('127.0.0.1',aPort);
  FServer.ReuseAddress:=True;
  FServer.OnConnect:=@DoConnect;
  inherited Create(False);
end;


destructor TVHostServer.Destroy;

var
  I: Integer;

begin
  if not Finished then
    begin
    FServer.StopAccepting(True);
    WaitFor;
    end;
  FServer.Free;
  for I:=0 to High(FHosts) do
    FHosts[I].CTX.Free;
  inherited Destroy;
end;


procedure TVHostServer.DoConnect(Sender: TObject; aData: TSocketStream);

var
  lSSL: TSSL;

begin
  lSSL:=TSSL.Create(FHosts[0].CTX);
  try
    lSSL.SetFd(aData.Handle);
    if lSSL.Accept=1 then
      lSSL.Shutdown;
  finally
    lSSL.Free;
    aData.Free;
  end;
end;


procedure TVHostServer.Execute;

begin
  try
    FServer.StartAccepting;
  except
    on E: Exception do
      Writeln('server exception ',E.ClassName,': ',E.Message);
  end;
end;


// Records the outcome of one check
procedure Check(aOK: Boolean; const aMessage: String);

begin
  if aOK then
    Writeln('ok    ',aMessage)
  else
    begin
    Writeln('FAIL  ',aMessage);
    Inc(gFailures);
    end;
end;


// Host name of the client session: set only when the server acknowledged the SNI name
function SessionHostname(aSSL: TSSL): String;

var
  lSession: Pointer;

begin
  Result:='';
  lSession:=gGetSession(aSSL.SSL);
  if lSession<>nil then
    Result:=StrPas(gGetHostname(lSession));
end;


// Connects with SNI name aName ('' for none) and checks the certificate subject and the acknowledgement
procedure CheckHost(const aName, aExpectedHost: String; aExpectAck: Boolean);

var
  lSocket: TInetSocket;
  lSSL: TSSL;
  lSubject, lLabel: String;
  lAcked: Boolean;

begin
  if aName='' then
    lLabel:='no SNI name'
  else
    lLabel:='SNI name '+aName;
  lSocket:=TInetSocket.Create('127.0.0.1',ServerPort);
  try
    lSSL:=TSSL.Create(gClientCtx);
    try
      lSSL.SetFd(lSocket.Handle);
      if aName<>'' then
        lSSL.SetTlsextHostName(aName);
      if lSSL.Connect<>1 then
        begin
        Check(False,lLabel+': handshake failed');
        Exit;
        end;
      lSubject:=lSSL.PeerSubject;
      lAcked:=SessionHostname(lSSL)<>'';
      lSSL.Shutdown;
    finally
      lSSL.Free;
    end;
  finally
    lSocket.Free;
  end;
  Check(Pos('CN='+aExpectedHost,lSubject)>0,lLabel+': certificate of '+aExpectedHost+' (got '+lSubject+')');
  if aExpectAck then
    Check(lAcked,lLabel+': name acknowledged')
  else
    Check(not lAcked,lLabel+': name not acknowledged');
end;


var
  lServer: TVHostServer;

begin
  {$ifdef unix}
  fpSignal(SIGPIPE,SignalHandler(SIG_IGN));
  {$endif}
  if not InitSSLInterface then
    begin
    Writeln('OpenSSL not available');
    Halt(1);
    end;
  gGetSession:=TSslGetSession(GetProcedureAddress(SSLLibHandle,'SSL_get_session'));
  gGetHostname:=TSslSessionGet0Hostname(GetProcedureAddress(SSLLibHandle,'SSL_SESSION_get0_hostname'));
  if not (Assigned(gGetSession) and Assigned(gGetHostname)) then
    begin
    Writeln('OpenSSL 1.1.1 or later needed');
    Halt(1);
    end;
  gClientCtx:=TSSLContext.Create(stAny);
  lServer:=TVHostServer.Create(ServerPort);
  try
    Sleep(200);
    CheckHost('main.test','main.test',True);
    CheckHost('alpha.test','alpha.test',True);
    CheckHost('beta.test','beta.test',True);
    CheckHost('gamma.test','main.test',False);
    CheckHost('','main.test',False);
    CheckHost('beta.test','beta.test',True);
  finally
    lServer.Free;
    gClientCtx.Free;
  end;
  Writeln(gFailures,' failure(s)');
  Halt(gFailures);
end.
