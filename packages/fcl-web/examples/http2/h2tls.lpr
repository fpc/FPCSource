program h2tls;

{ Minimal HTTP/2-over-TLS (ALPN) server example.

  Similar to examples/http2/h2cleartext.lpr: 
  a TFPHttpServer whose OnRequest fills a normal response, 
  plus a TFPHTTP2Handler wired to that server, but with TLS enabled. 
  
  With the handler Active:
  
  the handler advertises ALPN `h2,http/1.1` on the server's CertificateData (only when the list is empty), 
  so a TLS client that selects  `h2` during the handshake is served over HTTP/2 through the SAME OnRequest pipeline;
  
  a client that selects `http/1.1` (or sends no ALPN) is served unchanged HTTP/1.1.
  The seam is gated on the registered handler AND a negotiated `h2` (NFR1/FR16).

  A self-signed certificate is auto-generated for the configured host name (set
  CertificateData.HostName), so the example runs with no external cert files.

  Build (from packages/fcl-web):
    fpc -Mobjfpc -Sh -Fusrc/base -Fusrc/http2 -Fusrc/namespaced -Fu../fcl-net/src \
        -Fu../openssl/src -Fu../fcl-base/src -Fusrc/hpack examples/http2/h2tls.lpr

  Run:
    ./h2tls -p 4433
    
  Then:
    curl -vk --http2 https://localhost:4433/        # negotiated h2, HTTP/2 200
    curl -vk --http1.1 https://localhost:4433/       # HTTP/1.1 200 (ALPN http/1.1, regression)
    # open https://localhost:4433/ in a browser (accept the self-signed cert) -> h2 in devtools

}

uses
  {$ifdef unix} cthreads, {$endif}
  custapp, sysutils, classes,
  // opensslsockets must be used to enable SSL
  opensslsockets,
  httpdefs, fphttpserver, fphttp2server;

Type

  { TH2TLSApp }

  TH2TLSApp = class(TCustomApplication)
  private
    FSrv : TFPHttpServer;
    FH2  : TFPHTTP2Handler;
    procedure DoRequest(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
    procedure Usage(const aError: string);
  Public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    procedure DoRun; override;
  end;

{ TH2TLSApp }

procedure TH2TLSApp.DoRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
begin
  // The shared handler: serves both HTTP/1.1 and HTTP/2 (TLS-ALPN) requests.
  AResponse.Code := 200;
  AResponse.Content := 'Hello HTTP/2';
end;

constructor TH2TLSApp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSrv := TFPHttpServer.Create(Self);
  FSrv.OnRequest := @DoRequest;
  // Turn on TLS and configure a host name for the auto-generated self-signed cert.
  // (For a real deployment set CertificateData.Certificate / PrivateKey from files.)
  FSrv.UseSSL := True;
  FSrv.CertificateData.HostName := 'localhost';
  // Leave CertificateData.ALPNProtocols UNSET: the handler installs 'h2,http/1.1'
  // on Active (demonstrating AC#1). Set it explicitly here to show the honor-user-
  // list path instead.
  FH2 := TFPHTTP2Handler.Create(Self);
  FH2.WebServer := FSrv;
end;

destructor TH2TLSApp.Destroy;
begin
  // Deactivate the handler (unregisters the seam + restores ALPN) before teardown.
  if Assigned(FH2) then
    FH2.Active := False;
  FreeAndNil(FH2);
  FreeAndNil(FSrv);
  inherited Destroy;
end;

procedure TH2TLSApp.Usage(const aError : string);
begin
  if aError <> '' then
    Writeln('Error: ', aError);
  Writeln('Usage ', ExtractFileName(Paramstr(0)), ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help        This help text');
  Writeln('-p --port=PORT   Set port to listen on (default 4433)');
  ExitCode := Ord(aError <> '');
end;

procedure TH2TLSApp.DoRun;
Var
  S : String;
begin
  S := CheckOptions('hp:', ['help', 'port:']);
  if (S <> '') or HasOption('h', 'help') then
    begin
    Terminate;
    Usage(S);
    exit;
    end;
  FSrv.Port := StrToIntDef(GetOptionValue('p', 'port'), 4433);
  // Activate the HTTP/2 handler first: this registers the HTTP/2 trigger AND advertises
  // ALPN `h2,http/1.1` on the (shared) CertificateData. 
  FH2.Active := True;
  Writeln('Listening on port ', FSrv.Port, ' (curl -vk --http2 https://localhost:', FSrv.Port, '/ )');
  FSrv.Active := True;   // blocks until the server is stopped
  Terminate;
end;

begin
  With TH2TLSApp.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.
