{
    HTTPS server using a certificate and private key loaded from PEM files.
    Usage: sslhttpserver CERTFILE KEYFILE [PORT] [KEYPASSWORD]
}
program sslhttpserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, opensslsockets, fphttpserver;

type

  { TSSLServer }

  TSSLServer = class(TFPHTTPServer)
  protected
    procedure HandleRequest(var aRequest: TFPHTTPConnectionRequest;
      var aResponse: TFPHTTPConnectionResponse); override;
  end;


procedure TSSLServer.HandleRequest(var aRequest: TFPHTTPConnectionRequest;
  var aResponse: TFPHTTPConnectionResponse);

begin
  aResponse.ContentType:='text/plain';
  aResponse.Content:='Hello over HTTPS, you asked for '+aRequest.URL;
end;


var
  lServer: TSSLServer;

begin
  if ParamCount<2 then
    begin
    Writeln('Usage: ',ExtractFileName(ParamStr(0)),' CERTFILE KEYFILE [PORT] [KEYPASSWORD]');
    Writeln('Create a test certificate with:');
    Writeln('  openssl req -x509 -newkey rsa:2048 -nodes -days 365 -keyout key.pem -out cert.pem -subj "/CN=localhost"');
    Halt(1);
    end;
  lServer:=TSSLServer.Create(nil);
  try
    lServer.Port:=StrToIntDef(ParamStr(3),8443);
    lServer.UseSSL:=True;
    lServer.CertificateData.Certificate.FileName:=ParamStr(1);
    lServer.CertificateData.PrivateKey.FileName:=ParamStr(2);
    lServer.CertificateData.KeyPassword:=ParamStr(4);
    Writeln('Listening on https://localhost:',lServer.Port,'/');
    lServer.Active:=True;
  finally
    lServer.Free;
  end;
end.
