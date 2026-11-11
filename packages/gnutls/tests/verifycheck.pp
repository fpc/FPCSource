program verifycheck;

{ A GnuTLS client with VerifyPeerCert=True must refuse a self-signed server certificate,
  with VerifyPeerCert=False it must connect, and the server must survive the refused handshake.
  Build with -gh; expect PASS and 0 unfreed memory blocks. }

{$mode objfpc}{$H+}

uses
  {$ifdef unix}cthreads,{$endif}
  Classes, SysUtils, ssockets, sslbase, sslsockets, gnutlssockets;

const
  cPort = 44393;

type
  TServer = class(TThread)
  private
    FServer: TInetServer;
    procedure DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);
    procedure DoConnect(Sender: TObject; aData: TSocketStream);
  public
    // Creates a TLS server on cPort and starts the thread
    constructor Create;
    // Stops the server, waits for the thread and frees the server
    destructor Destroy; override;
    // Accepts connections until the server is stopped
    procedure Execute; override;
  end;

var
  gCertAndKey: TCertAndKey;


// Creates a GnuTLS handler with the self-signed test certificate
function NewServerHandler: TSSLSocketHandler;

begin
  Result:=TGNUTLSSocketHandler.Create;
  Result.CertificateData.HostName:='localhost';
  Result.CertificateData.Certificate.Value:=gCertAndKey.Certificate;
  Result.CertificateData.PrivateKey.Value:=gCertAndKey.PrivateKey;
end;


constructor TServer.Create;

begin
  FServer:=TInetServer.Create('127.0.0.1',cPort,NewServerHandler);
  FServer.ReuseAddress:=True;
  FServer.OnCreateClientSocketHandler:=@DoCreateHandler;
  FServer.OnConnect:=@DoConnect;
  inherited Create(False);
end;


destructor TServer.Destroy;

begin
  if not Finished then
    begin
    FServer.StopAccepting(True);
    WaitFor;
    end;
  FServer.Free;
  inherited Destroy;
end;


procedure TServer.DoCreateHandler(Sender: TObject; out aHandler: TSocketHandler);

begin
  aHandler:=NewServerHandler;
end;


procedure TServer.DoConnect(Sender: TObject; aData: TSocketStream);

begin
  aData.Free;
end;


procedure TServer.Execute;

begin
  FServer.StartAccepting;
end;


// Connects a GnuTLS client to the server; returns 'connected' or 'refused: <message>'
function TryConnect(aVerify: Boolean): String;

var
  lHandler: TSSLSocketHandler;
  lSocket: TInetSocket;

begin
  lHandler:=TGNUTLSSocketHandler.Create;
  lHandler.CertificateData.HostName:='localhost';
  lHandler.VerifyPeerCert:=aVerify;
  lSocket:=TInetSocket.Create('127.0.0.1',cPort,lHandler);
  try
    try
      lSocket.Connect;
      Result:='connected';
    except
      on E: Exception do
        Result:='refused: '+E.Message;
    end;
  finally
    lSocket.Free;
  end;
end;


var
  lGenerator: TSSLSocketHandler;
  lServer: TServer;
  lPlain, lVerified, lAfter: String;

begin
  lGenerator:=TGNUTLSSocketHandler.Create;
  try
    lGenerator.CertGenerator.HostName:='localhost';
    gCertAndKey:=lGenerator.CertGenerator.CreateCertificateAndKey;
  finally
    lGenerator.Free;
  end;
  lServer:=TServer.Create;
  try
    Sleep(200);
    lPlain:=TryConnect(False);
    lVerified:=TryConnect(True);
    lAfter:=TryConnect(False);
    Sleep(200);
  finally
    lServer.Free;
  end;
  Writeln('VerifyPeerCert=False: ',lPlain);
  Writeln('VerifyPeerCert=True:  ',lVerified);
  Writeln('after the refusal:    ',lAfter);
  if (lPlain='connected') and (Copy(lVerified,1,8)='refused:') and (lAfter='connected') then
    Writeln('PASS')
  else
    begin
    Writeln('FAIL');
    ExitCode:=1;
    end;
end.
