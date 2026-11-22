{ TOpenSSLSocketHandler with VerifyPeerCert against local TLS servers: host name and IP address checks,
  during the handshake and with VerifyPeerHost. Exit code is the number of failed checks. }
program thostcheck;

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads, baseunix,{$endif}
  Classes, SysUtils, ssockets, sslbase, sslsockets, openssl, opensslsockets;

const
  LocalhostPort = 44395;
  OtherPort = 44396;

  CACert =
    '-----BEGIN CERTIFICATE-----'+LineEnding+
    'MIIBqTCCAU+gAwIBAgIUatl0aWKlTW87sHVq75IQ/sf6RgwwCgYIKoZIzj0EAwIw'+LineEnding+
    'ITEfMB0GA1UEAwwWRlBDIGhvc3QgY2hlY2sgdGVzdCBDQTAgFw0yNjExMjIxNTM4'+LineEnding+
    'MDlaGA8yMTI2MTAyOTE1MzgwOVowITEfMB0GA1UEAwwWRlBDIGhvc3QgY2hlY2sg'+LineEnding+
    'dGVzdCBDQTBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABJGknPGPxMm5zIhW76ZA'+LineEnding+
    '3ckx/JarD5SX1N7FksCtMgE6+VuBhM70/GVqyukfrQPg3lIFa3CabDtzMBBldutx'+LineEnding+
    'WfijYzBhMB0GA1UdDgQWBBQvGW3//o9xg1h9GiX+Eol8hytlozAfBgNVHSMEGDAW'+LineEnding+
    'gBQvGW3//o9xg1h9GiX+Eol8hytlozAPBgNVHRMBAf8EBTADAQH/MA4GA1UdDwEB'+LineEnding+
    '/wQEAwIBBjAKBggqhkjOPQQDAgNIADBFAiEAv+kugvOoelAqC2JrtbcDSX4FMf/1'+LineEnding+
    'GsE8Jb907t1IhEcCIDHJV4F3GxwJ+xZNHtIeFVqIkGJ4WeGDmAX3thD0ZgYY'+LineEnding+
    '-----END CERTIFICATE-----'+LineEnding;

  OtherCACert =
    '-----BEGIN CERTIFICATE-----'+LineEnding+
    'MIIBszCCAVmgAwIBAgIUZh45C6Dam8DH03GnqQ76epfEj4kwCgYIKoZIzj0EAwIw'+LineEnding+
    'JjEkMCIGA1UEAwwbRlBDIGhvc3QgY2hlY2sgdW5yZWxhdGVkIENBMCAXDTI2MTEy'+LineEnding+
    'MjE1MzgwOVoYDzIxMjYxMDI5MTUzODA5WjAmMSQwIgYDVQQDDBtGUEMgaG9zdCBj'+LineEnding+
    'aGVjayB1bnJlbGF0ZWQgQ0EwWTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAATb4kxc'+LineEnding+
    '9nUhCS/ny7PuA6u8fClqufD+n6790T9Pk7OAEo0Tj25Ns6uG5L9EHWs8JutQ7W67'+LineEnding+
    'la7c74mU+40XHIsno2MwYTAdBgNVHQ4EFgQUYs8423xf3VQES2oJJdqru8Q59bAw'+LineEnding+
    'HwYDVR0jBBgwFoAUYs8423xf3VQES2oJJdqru8Q59bAwDwYDVR0TAQH/BAUwAwEB'+LineEnding+
    '/zAOBgNVHQ8BAf8EBAMCAQYwCgYIKoZIzj0EAwIDSAAwRQIgJ8FFl1FHpLjSds2T'+LineEnding+
    'wCdUiJVXXodNP7LK48TPC2+Ke/8CIQCKEKjGRkNotfodjKApFBr58GFjBamijiMU'+LineEnding+
    'PUrJP8QtLA=='+LineEnding+
    '-----END CERTIFICATE-----'+LineEnding;

  { Subject alternative names DNS:localhost and IP:127.0.0.1, issued by CACert }
  LocalhostCert =
    '-----BEGIN CERTIFICATE-----'+LineEnding+
    'MIIBtzCCAV2gAwIBAgIUYuwJ7TZ0V9/kiMsn6D+bNI72STEwCgYIKoZIzj0EAwIw'+LineEnding+
    'ITEfMB0GA1UEAwwWRlBDIGhvc3QgY2hlY2sgdGVzdCBDQTAgFw0yNjExMjIxNTM4'+LineEnding+
    'MDlaGA8yMTI2MTAyOTE1MzgwOVowFDESMBAGA1UEAwwJbG9jYWxob3N0MFkwEwYH'+LineEnding+
    'KoZIzj0CAQYIKoZIzj0DAQcDQgAEQnrBOMYbDKKfReH2x95Yk76xDfVBV6eV9zNJ'+LineEnding+
    'hK8rPImJrwdpLzJ/U4+F5sma1lU1aH/gVaHA9p+/c3rZvhMaiKN+MHwwGgYDVR0R'+LineEnding+
    'BBMwEYIJbG9jYWxob3N0hwR/AAABMAkGA1UdEwQCMAAwEwYDVR0lBAwwCgYIKwYB'+LineEnding+
    'BQUHAwEwHQYDVR0OBBYEFESAeQ0B2pFjlgM9Fd05MxfK5pxRMB8GA1UdIwQYMBaA'+LineEnding+
    'FC8Zbf/+j3GDWH0aJf4SiXyHK2WjMAoGCCqGSM49BAMCA0gAMEUCIQCOD7uqYrCp'+LineEnding+
    'sExdlV/qge2tdZcEUbK6w9XqWjKWfgdRDQIgNI8HF96P/TZ5Ho14u8y8yVgiBxGM'+LineEnding+
    'DaHd/c7oCh3uHck='+LineEnding+
    '-----END CERTIFICATE-----'+LineEnding;

  LocalhostKey =
    '-----BEGIN EC PRIVATE KEY-----'+LineEnding+
    'MHcCAQEEIBcG06tCwcQbewvkD318/LekcHUp2z+iQk8AGx8s4VNdoAoGCCqGSM49'+LineEnding+
    'AwEHoUQDQgAEQnrBOMYbDKKfReH2x95Yk76xDfVBV6eV9zNJhK8rPImJrwdpLzJ/'+LineEnding+
    'U4+F5sma1lU1aH/gVaHA9p+/c3rZvhMaiA=='+LineEnding+
    '-----END EC PRIVATE KEY-----'+LineEnding;

  { Subject alternative name DNS:other.test, issued by CACert }
  OtherCert =
    '-----BEGIN CERTIFICATE-----'+LineEnding+
    'MIIBszCCAVmgAwIBAgIUYuwJ7TZ0V9/kiMsn6D+bNI72STIwCgYIKoZIzj0EAwIw'+LineEnding+
    'ITEfMB0GA1UEAwwWRlBDIGhvc3QgY2hlY2sgdGVzdCBDQTAgFw0yNjExMjIxNTM4'+LineEnding+
    'MDlaGA8yMTI2MTAyOTE1MzgwOVowFTETMBEGA1UEAwwKb3RoZXIudGVzdDBZMBMG'+LineEnding+
    'ByqGSM49AgEGCCqGSM49AwEHA0IABCtOLDsae0AYGYNJHjc/J0OcQb+Ab7bhsiXv'+LineEnding+
    'vvHkxFlfJLu2uGqxwiTvYUhbri0k+EGT0kB932ZHScFbYRKmVxajeTB3MBUGA1Ud'+LineEnding+
    'EQQOMAyCCm90aGVyLnRlc3QwCQYDVR0TBAIwADATBgNVHSUEDDAKBggrBgEFBQcD'+LineEnding+
    'ATAdBgNVHQ4EFgQUr+X2MpaXY5On9G/6lnTEOCRD9TEwHwYDVR0jBBgwFoAULxlt'+LineEnding+
    '//6PcYNYfRol/hKJfIcrZaMwCgYIKoZIzj0EAwIDSAAwRQIhAJs7QGxH9DQKf3Qj'+LineEnding+
    'x/ewYVLJ5qD0HmSc80IUE1WPv8ahAiAwB914/v8xhT93F4vlRhFjASLAbfZVCo+/'+LineEnding+
    '0cOT7S2akA=='+LineEnding+
    '-----END CERTIFICATE-----'+LineEnding;

  OtherKey =
    '-----BEGIN EC PRIVATE KEY-----'+LineEnding+
    'MHcCAQEEILVFrob0kX8qjNndRsQOQuYq6crHPtopA6g1jxXYL04eoAoGCCqGSM49'+LineEnding+
    'AwEHoUQDQgAEK04sOxp7QBgZg0keNz8nQ5xBv4BvtuGyJe++8eTEWV8ku7a4arHC'+LineEnding+
    'JO9hSFuuLST4QZPSQH3fZkdJwVthEqZXFg=='+LineEnding+
    '-----END EC PRIVATE KEY-----'+LineEnding;

type

  { TCertServer }

  TCertServer = class(TThread)
  private
    FServer: TInetServer;
    FCertFile: String;
    FKeyFile: String;
    procedure DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);
    procedure DoConnect(Sender: TObject; aData: TSocketStream);
  protected
    procedure Execute; override;
  public
    // Creates a TLS server on 127.0.0.1:aPort with the given certificate and key files and starts accepting
    constructor Create(aPort: Word; const aCertFile, aKeyFile: String);
    // Stops accepting and frees the server
    destructor Destroy; override;
  end;

var
  gDir: String;
  gFailures: Integer;


{ TCertServer }

constructor TCertServer.Create(aPort: Word; const aCertFile, aKeyFile: String);

begin
  FCertFile:=aCertFile;
  FKeyFile:=aKeyFile;
  FServer:=TInetServer.Create('127.0.0.1',aPort,TOpenSSLSocketHandler.Create);
  FServer.ReuseAddress:=True;
  FServer.OnCreateClientSocketHandler:=@DoCreateHandler;
  FServer.OnConnect:=@DoConnect;
  inherited Create(False);
end;


destructor TCertServer.Destroy;

begin
  if not Finished then
    begin
    FServer.StopAccepting(True);
    WaitFor;
    end;
  FServer.Free;
  inherited Destroy;
end;


procedure TCertServer.DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);

var
  lHandler: TOpenSSLSocketHandler;

begin
  lHandler:=TOpenSSLSocketHandler.Create;
  lHandler.CertificateData.Certificate.FileName:=FCertFile;
  lHandler.CertificateData.PrivateKey.FileName:=FKeyFile;
  aHandler:=lHandler;
end;


procedure TCertServer.DoConnect(Sender: TObject; aData: TSocketStream);

begin
  aData.Free;
end;


procedure TCertServer.Execute;

begin
  try
    FServer.StartAccepting;
  except
    on E: Exception do
      Writeln('server exception ',E.ClassName,': ',E.Message);
  end;
end;


// Writes aText to file aName in gDir and returns the full file name
function WriteFile(const aName, aText: String): String;

var
  lList: TStringList;

begin
  Result:=gDir+aName;
  lList:=TStringList.Create;
  try
    lList.Text:=aText;
    lList.SaveToFile(Result);
  finally
    lList.Free;
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


// Creates a verifying client handler that trusts aCAFile
function CreateClientHandler(const aCAFile: String): TOpenSSLSocketHandler;

begin
  Result:=TOpenSSLSocketHandler.Create;
  Result.VerifyPeerCert:=True;
  Result.CertificateData.CertCA.FileName:=aCAFile;
end;


// Connects to aHost:aPort trusting aCAFile and checks whether the connection is accepted as expected
procedure CheckConnect(const aHost: String; aPort: Word; const aCAFile: String; aExpectAccept: Boolean;
  const aMessage: String);

var
  lSocket: TInetSocket;
  lAccepted: Boolean;
  lError: String;

begin
  lError:='';
  lSocket:=TInetSocket.Create(aHost,aPort,CreateClientHandler(aCAFile));
  try
    try
      lSocket.Connect;
      lAccepted:=True;
    except
      on E: Exception do
        begin
        lAccepted:=False;
        lError:=E.Message+' ['+lSocket.Handler.GetLastErrorDescription+']';
        end;
    end;
  finally
    lSocket.Free;
  end;
  if lAccepted=aExpectAccept then
    Check(True,aMessage)
  else
    Check(False,aMessage+' (accepted='+BoolToStr(lAccepted,True)+' '+lError+')');
end;


// Connects to the localhost server and checks VerifyPeerHost for several names
procedure CheckVerifyPeerHost(const aCAFile: String);

var
  lHandler: TOpenSSLSocketHandler;
  lSocket: TInetSocket;

begin
  lHandler:=CreateClientHandler(aCAFile);
  lSocket:=TInetSocket.Create('localhost',LocalhostPort,lHandler);
  try
    try
      lSocket.Connect;
    except
      on E: Exception do
        begin
        Check(False,'VerifyPeerHost: connect failed: '+E.Message);
        Exit;
        end;
    end;
    Check(lHandler.VerifyPeerHost('localhost'),'VerifyPeerHost accepts localhost');
    Check(lHandler.VerifyPeerHost('LocalHost.'),'VerifyPeerHost accepts LocalHost. (case and trailing dot)');
    Check(lHandler.VerifyPeerHost('127.0.0.1'),'VerifyPeerHost accepts IP address 127.0.0.1');
    Check(not lHandler.VerifyPeerHost('other.test'),'VerifyPeerHost refuses other.test');
    Check(Pos('other.test',lHandler.SSLLastErrorString)>0,'VerifyPeerHost error message names the host');
    Check(not lHandler.VerifyPeerHost('sub.localhost'),'VerifyPeerHost refuses sub.localhost');
    Check(not lHandler.VerifyPeerHost('127.0.0.2'),'VerifyPeerHost refuses IP address 127.0.0.2');
    Check(not lHandler.VerifyPeerHost('::1'),'VerifyPeerHost refuses IP address ::1');
    Check(not lHandler.VerifyPeerHost(''),'VerifyPeerHost refuses an empty host');
  finally
    lSocket.Free;
  end;
end;


var
  lCA, lOtherCA: String;
  lLocalhostServer, lOtherServer: TCertServer;

begin
  {$ifdef unix}
  fpSignal(SIGPIPE,SignalHandler(SIG_IGN));
  {$endif}
  gDir:=IncludeTrailingPathDelimiter(GetTempDir(False))+'thostcheck'+IntToStr(GetProcessID)+PathDelim;
  ForceDirectories(gDir);
  lCA:=WriteFile('ca.pem',CACert);
  lOtherCA:=WriteFile('otherca.pem',OtherCACert);
  lLocalhostServer:=TCertServer.Create(LocalhostPort,WriteFile('localhost.pem',LocalhostCert),
    WriteFile('localhost.key',LocalhostKey));
  lOtherServer:=TCertServer.Create(OtherPort,WriteFile('other.pem',OtherCert),WriteFile('other.key',OtherKey));
  try
    Sleep(200);
    CheckConnect('localhost',LocalhostPort,lCA,True,'localhost certificate accepted for localhost');
    CheckConnect('127.0.0.1',LocalhostPort,lCA,True,'localhost certificate accepted for IP address 127.0.0.1');
    CheckConnect('localhost',LocalhostPort,lOtherCA,False,'localhost certificate refused with an unrelated CA');
    CheckConnect('localhost',OtherPort,lCA,False,'other.test certificate refused for localhost');
    CheckConnect('127.0.0.1',OtherPort,lCA,False,'other.test certificate refused for IP address 127.0.0.1');
    CheckVerifyPeerHost(lCA);
  finally
    lLocalhostServer.Free;
    lOtherServer.Free;
    DeleteFile(gDir+'ca.pem');
    DeleteFile(gDir+'otherca.pem');
    DeleteFile(gDir+'localhost.pem');
    DeleteFile(gDir+'localhost.key');
    DeleteFile(gDir+'other.pem');
    DeleteFile(gDir+'other.key');
    RemoveDir(gDir);
  end;
  Writeln(gFailures,' failure(s)');
  Halt(gFailures);
end.
