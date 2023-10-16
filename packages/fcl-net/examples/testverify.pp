{
  Program to demonstrate verification of a certificate.
  Created by Bernd K. for issue:
  https://gitlab.com/freepascal.org/fpc/source/-/issues/39998
}
program testverify;

uses
  Sysutils, Classes, sockets, ssockets, sslsockets, openssl, opensslsockets;


type

  { TApp }

  TApp = class
    Sock: TInetSocket;
    SSLHandler: TSSLSocketHandler;
    constructor Create;
    destructor Destroy; override;
    procedure OnVerify(Sender: TObject; var Allow: Boolean);
  end;

var
  App: TApp;

{ TApp }

constructor TApp.Create;
begin
  SSLHandler := TSSLSocketHandler.GetDefaultHandler;
  SSLHandler.OnVerifyCertificate := @OnVerify;
  //SSLHandler.VerifyPeerCert := True;
  Sock := TInetSocket.Create('test.mosquitto.org', 8883, 1000, SSLHandler);

  writeln('begin connect');
  Sock.Connect;
  writeln('end connect');

end;

destructor TApp.Destroy;
begin
  Sock.Free;
  inherited Destroy;
end;

procedure TApp.OnVerify(Sender: TObject; var Allow: Boolean);
var
  S: TOpenSSLSocketHandler;
begin
  Writeln('OnVerify');
  S := Sender as TOpenSSLSocketHandler;
  writeln('cert assigned: ', Assigned(S.SSL.PeerCertificate));
  writeln('cert info:     ', S.SSL.CertInfo);
end;

begin
  App := TApp.Create;
  App.Free;
end.
