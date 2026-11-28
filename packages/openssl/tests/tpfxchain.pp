{ TSSLContext.LoadPFX with a PKCS#12 file that contains a CA chain: the chain is installed on the context,
  a client that trusts only the root CA accepts the server, and repeated loads do not leak memory.
  Exit code is the number of failed checks. }
program tpfxchain;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads, baseunix,{$endif}
  Classes, SysUtils, dynlibs, ctypes, base64, ssockets, sslbase, sslsockets, openssl, fpopenssl, opensslsockets;

const
  ServerPort = 44400;
  PFXPassword = 'secret';

  { Root of the chain in ChainPFXBase64 }
  RootCACert =
    '-----BEGIN CERTIFICATE-----'+LineEnding+
    'MIIBqzCCAVGgAwIBAgIUZyR/zZa7E4OoUi8IziBu0KdtqnswCgYIKoZIzj0EAwIw'+LineEnding+
    'IjEgMB4GA1UEAwwXRlBDIFBGWCBjaGFpbiB0ZXN0IHJvb3QwIBcNMjYxMTI4MTQx'+LineEnding+
    'NTE5WhgPMjEyNjExMDQxNDE1MTlaMCIxIDAeBgNVBAMMF0ZQQyBQRlggY2hhaW4g'+LineEnding+
    'dGVzdCByb290MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEwApTk2kwkkiK4uoM'+LineEnding+
    'f51oLF5utF7vGdIvVHPhTnZAnM2jBakybQkmU+dMLG6P5cpqG+k1eHfLrvYpVQEm'+LineEnding+
    'SLIP8KNjMGEwHQYDVR0OBBYEFDp8zZV+qJ/KnDvTqk6cu+6HpMCnMB8GA1UdIwQY'+LineEnding+
    'MBaAFDp8zZV+qJ/KnDvTqk6cu+6HpMCnMA8GA1UdEwEB/wQFMAMBAf8wDgYDVR0P'+LineEnding+
    'AQH/BAQDAgEGMAoGCCqGSM49BAMCA0gAMEUCIQCPHVjJbjLCbACXh+IyB36OIcaT'+LineEnding+
    'piYOnSE5kwZUta50mgIgAoSixX+QA+cuPn5SnvD9iQxSTYPE0+arY6do6zc5lww='+LineEnding+
    '-----END CERTIFICATE-----'+LineEnding;

  { PKCS#12, password PFXPassword: key and certificate for localhost and 127.0.0.1, issued by an intermediate CA,
    with the intermediate and the root CA as chain }
  ChainPFXBase64 =
    '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';

type
  TSkNum = function(aStack: Pointer): cint; cdecl;

  { TPFXServer }

  TPFXServer = class(TThread)
  private
    FServer: TInetServer;
    FPFX: TBytes;
    procedure DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);
    procedure DoConnect(Sender: TObject; aData: TSocketStream);
  protected
    procedure Execute; override;
  public
    // Creates a TLS server on 127.0.0.1:aPort that uses the PKCS#12 data aPFX
    constructor Create(aPort: Word; const aPFX: TBytes);
    // Stops accepting and frees the server
    destructor Destroy; override;
  end;

var
  gFailures: Integer;


{ TPFXServer }

constructor TPFXServer.Create(aPort: Word; const aPFX: TBytes);

begin
  FPFX:=aPFX;
  FServer:=TInetServer.Create('127.0.0.1',aPort,TOpenSSLSocketHandler.Create);
  FServer.ReuseAddress:=True;
  FServer.OnCreateClientSocketHandler:=@DoCreateHandler;
  FServer.OnConnect:=@DoConnect;
  inherited Create(False);
end;


destructor TPFXServer.Destroy;

begin
  if not Finished then
    begin
    FServer.StopAccepting(True);
    WaitFor;
    end;
  FServer.Free;
  inherited Destroy;
end;


procedure TPFXServer.DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);

var
  lHandler: TOpenSSLSocketHandler;

begin
  lHandler:=TOpenSSLSocketHandler.Create;
  lHandler.CertificateData.PFX.Value:=FPFX;
  lHandler.CertificateData.KeyPassword:=PFXPassword;
  aHandler:=lHandler;
end;


procedure TPFXServer.DoConnect(Sender: TObject; aData: TSocketStream);

begin
  aData.Free;
end;


procedure TPFXServer.Execute;

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


// The PKCS#12 test data as bytes
function PFXBytes: TBytes;

var
  lData: RawByteString;

begin
  lData:=DecodeStringBase64(ChainPFXBase64);
  Result:=[];
  SetLength(Result,Length(lData));
  if Length(lData)>0 then
    Move(lData[1],Result[0],Length(lData));
end;


// Loads the PKCS#12 data into a new context and checks the result and the installed chain
procedure CheckChainInstalled(const aPFX: TBytes);

var
  lCtx: TSSLContext;
  lStack: Pointer;
  lSkNum: TSkNum;
  lCount: Integer;

begin
  lSkNum:=TSkNum(GetProcedureAddress(SSLUtilHandle,'OPENSSL_sk_num'));
  lCtx:=TSSLContext.Create(stAny);
  try
    Check(lCtx.LoadPFX(aPFX,PFXPassword)>0,'LoadPFX succeeds');
    lStack:=nil;
    SslCTXCtrl(lCtx.CTX,SSL_CTRL_GET_CHAIN_CERTS,0,@lStack);
    if not Assigned(lSkNum) then
      Check(lStack<>nil,'chain installed on the context')
    else
      begin
      if lStack=nil then
        lCount:=0
      else
        lCount:=lSkNum(lStack);
      Check(lCount=2,'chain of 2 certificates installed on the context (got '+IntToStr(lCount)+')');
      end;
  finally
    lCtx.Free;
  end;
end;


{$ifdef linux}
// Resident memory of this process in KB
function ResidentKB: Int64;

var
  lList: TStringList;
  I: Integer;

begin
  Result:=-1;
  lList:=TStringList.Create;
  try
    lList.LoadFromFile('/proc/self/status');
    for I:=0 to lList.Count-1 do
      if Pos('VmRSS:',lList[I])=1 then
        Result:=StrToInt64(Trim(StringReplace(Copy(lList[I],7,100),'kB','',[])));
  finally
    lList.Free;
  end;
end;


// Loads the PKCS#12 data 1000 times and checks that resident memory stays almost flat
procedure CheckNoLeak(const aPFX: TBytes);

var
  lCtx: TSSLContext;
  I: Integer;
  lBefore, lGrowth: Int64;

begin
  for I:=1 to 100 do
    begin
    lCtx:=TSSLContext.Create(stAny);
    lCtx.LoadPFX(aPFX,PFXPassword);
    lCtx.Free;
    end;
  lBefore:=ResidentKB;
  for I:=1 to 1000 do
    begin
    lCtx:=TSSLContext.Create(stAny);
    lCtx.LoadPFX(aPFX,PFXPassword);
    lCtx.Free;
    end;
  lGrowth:=ResidentKB-lBefore;
  Check(lGrowth<1024,'1000 loads grow resident memory by less than 1 MB (got '+IntToStr(lGrowth)+' KB)');
end;
{$endif}


// Connects with a client that trusts only the root CA and checks that the server is accepted
procedure CheckClientVerifies(const aCAFile: String);

var
  lHandler: TOpenSSLSocketHandler;
  lSocket: TInetSocket;
  lError: String;

begin
  lError:='';
  lHandler:=TOpenSSLSocketHandler.Create;
  lHandler.VerifyPeerCert:=True;
  lHandler.CertificateData.CertCA.FileName:=aCAFile;
  lSocket:=TInetSocket.Create('localhost',ServerPort,lHandler);
  try
    try
      lSocket.Connect;
    except
      on E: Exception do
        lError:=E.Message+' ['+lHandler.GetLastErrorDescription+']';
    end;
  finally
    lSocket.Free;
  end;
  Check(lError='','client that trusts only the root CA accepts the server '+lError);
end;


var
  lPFX: TBytes;
  lCAFile: String;
  lList: TStringList;
  lServer: TPFXServer;

begin
  {$ifdef unix}
  fpSignal(SIGPIPE,SignalHandler(SIG_IGN));
  {$endif}
  if not InitSSLInterface then
    begin
    Writeln('OpenSSL not available');
    Halt(1);
    end;
  lPFX:=PFXBytes;
  CheckChainInstalled(lPFX);
  {$ifdef linux}
  CheckNoLeak(lPFX);
  {$endif}
  lCAFile:=IncludeTrailingPathDelimiter(GetTempDir(False))+'tpfxchain'+IntToStr(GetProcessID)+'.pem';
  lList:=TStringList.Create;
  try
    lList.Text:=RootCACert;
    lList.SaveToFile(lCAFile);
  finally
    lList.Free;
  end;
  lServer:=TPFXServer.Create(ServerPort,lPFX);
  try
    Sleep(200);
    CheckClientVerifies(lCAFile);
  finally
    lServer.Free;
    DeleteFile(lCAFile);
  end;
  Writeln(gFailures,' failure(s)');
  Halt(gFailures);
end.
