program h2cleartext;

{ Minimal cleartext HTTP/2 (prior-knowledge) server example.

  Similar to examples/websocket/server/wsserver.lpr: 
  a TFPHttpServer whose OnRequest fills a normal response, plus a TFPHTTP2Handler wired to that server. 
  
  With the handler Active:
  a prior-knowledge client (curl --http2-prior-knowledge) is served over HTTP/2 through the SAME OnRequest pipeline; 
  an ordinary HTTP/1.1 client is served unchanged.

  Build (from packages/fcl-web):
    fpc -Mobjfpc -Sh -Fusrc/base -Fusrc/http2 -Fusrc/namespaced -Fusrc/hpack examples/http2/h2cleartext.lpr
  Run:
    ./h2cleartext -p 8080
  Then:
    curl -v --http2-prior-knowledge http://localhost:8080/   # HTTP/2 200
    curl -v http://localhost:8080/                           # HTTP/1.1 200 (regression)
}

uses
  {$ifdef unix} cthreads, {$endif}
  custapp, sysutils, classes,
  httpdefs, fphttpserver, fphttp2server;

Type

  { TH2App }

  TH2App = class(TCustomApplication)
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

{ TH2App }

procedure TH2App.DoRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
begin
  // The shared handler: serves both HTTP/1.1 and HTTP/2 (prior-knowledge) requests.
  AResponse.Code := 200;
  AResponse.Content := 'Hello HTTP/2';
end;

constructor TH2App.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSrv := TFPHttpServer.Create(Self);
  FSrv.OnRequest := @DoRequest;
  FH2 := TFPHTTP2Handler.Create(Self);
  FH2.WebServer := FSrv;
end;

destructor TH2App.Destroy;
begin
  // Deactivate the handler (unregisters the seam) before tearing down the server.
  if Assigned(FH2) then
    FH2.Active := False;
  FreeAndNil(FH2);
  FreeAndNil(FSrv);
  inherited Destroy;
end;

procedure TH2App.Usage(const aError : string);
begin
  if aError <> '' then
    Writeln('Error: ', aError);
  Writeln('Usage ', ExtractFileName(Paramstr(0)), ' [options]');
  Writeln('Where options is one or more of:');
  Writeln('-h --help        This help text');
  Writeln('-p --port=PORT   Set port to listen on (default 8080)');
  ExitCode := Ord(aError <> '');
end;

procedure TH2App.DoRun;
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
  FSrv.Port := StrToIntDef(GetOptionValue('p', 'port'), 8080);
  // Activate the HTTP/2 handler FIRST: this registers the prior-knowledge ingress
  // seam on the server. The server's Active:=True then blocks in the accept loop.
  FH2.Active := True;
  Writeln('Listening on port ', FSrv.Port, ' (curl --http2-prior-knowledge http://localhost:', FSrv.Port, '/ )');
  FSrv.Active := True;   // blocks until the server is stopped
  Terminate;
end;

begin
  With TH2App.Create(Nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.
