program httpget;

{$mode objfpc}{$H+}
{$DEFINE USEGNUTLS}

uses
  SysUtils, Classes, fphttpclient, ssockets,
{$IFNDEF USEGNUTLS}
  fpopenssl, opensslsockets,
{$else}
  gnutls, gnutlssockets,
{$endif}
  sslsockets;

Type

  { TTestApp }

  TTestApp = Class(Tobject)
  private
    procedure DoHaveSocketHandler(Sender: TObject; AHandler: TSocketHandler);
    procedure DoVerifyCertificate(Sender: TObject; AHandler: TSSLSocketHandler; var aAllow: Boolean);
    procedure DoProgress(Sender: TObject; Const ContentLength, CurrentPos : Int64);
    procedure DoHeaders(Sender : TObject);
    procedure DoPassword(Sender: TObject; var RepeatRequest: Boolean);
    procedure ShowRedirect(ASender : TObject; Const ASrc : String; Var ADest : String);
    Procedure Run;
  end;

procedure TTestApp.DoHeaders(Sender : TObject);

Var
  I : Integer;

begin
  Writeln('Response headers received:');
  With (Sender as TFPHTTPClient) do
    For I:=0 to ResponseHeaders.Count-1 do
      Writeln(ResponseHeaders[i]);
end;

procedure TTestApp.DoProgress(Sender: TObject; const ContentLength, CurrentPos: Int64);
begin
  If (ContentLength=0) then
    Writeln('Reading headers : ',CurrentPos,' Bytes.')
  else If (ContentLength=-1) then
    Writeln('Reading data (no length available) : ',CurrentPos,' Bytes.')
  else
    Writeln('Reading data : ',CurrentPos,' Bytes of ',ContentLength);
end;

procedure TTestApp.DoPassword(Sender: TObject; var RepeatRequest: Boolean);

Var
  H,UN,PW : String;
  P : Integer;
begin
  With TFPHTTPClient(Sender) do
    begin
    H:=GetHeader(ResponseHeaders,'WWW-Authenticate');
    end;
  P:=Pos('realm',LowerCase(H));
  if (P>0) then
    begin
    P:=Pos('"',H);
    Delete(H,1,P);
    P:=Pos('"',H);
    H:=Copy(H,1,Pos('"',H)-1);
    end;
  Writeln('Authorization required. Remote site says: ',H);
  Write('Enter username (empty quits): ');
  ReadLn(UN);
  RepeatRequest:=(UN<>'');
  if RepeatRequest then
    begin
    Write('Enter password: ');
    Readln(PW);
    TFPHTTPClient(Sender).UserName:=UN;
    TFPHTTPClient(Sender).Password:=PW;
    end;
end;

procedure TTestApp.ShowRedirect(ASender: TObject; const ASrc: String;
  var ADest: String);

begin
  Writeln('Following redirect from ',ASrc,'  ==> ',ADest);
end;  


procedure TTestApp.Run;

begin
  if (ParamCount<>2) then
    begin
    writeln('Usage : ',ExtractFileName(ParamStr(0)), ' URL filename');
    Halt(1);
    end;
  With TFPHTTPClient.Create(Nil) do
    try
      AllowRedirect:=True;
      OnRedirect:=@ShowRedirect;
      OnPassword:=@DoPassword;
      OnDataReceived:=@DoProgress;
      OnHeaders:=@DoHeaders;
      VerifySSlCertificate:=True;
      OnVerifySSLCertificate:=@DoVerifyCertificate;
      AfterSocketHandlerCreate:=@DoHaveSocketHandler;
      { Set this if you want to try a proxy.
      Proxy.Host:='195.207.46.20';
      Proxy.Port:=8080;
      }
      Get(ParamStr(1),ParamStr(2));
    finally
      Free;
    end;
end;

procedure TTestApp.DoHaveSocketHandler(Sender: TObject; AHandler: TSocketHandler);

Var
  SSLHandler :  TSSLSocketHandler absolute aHandler;

begin
  if (aHandler is TSSLSocketHandler) then
    begin
    SSLHandler.CertificateData.TrustedCertsDir:='/etc/ssl/certs/';
    end
end;

procedure TTestApp.DoVerifyCertificate(Sender: TObject; AHandler: TSSLSocketHandler; var aAllow: Boolean);

Var
  S : String;

begin
  Writeln('SSL Certificate verification requested, allowing');
  S:=TEncoding.ASCII.GetAnsiString( aHandler.CertificateData.Certificate.Value);
  Writeln('Cert: ',S);
  aAllow:=True;
end;

begin
  With TTestApp.Create do
    try
      Run;
    finally
      Free;
    end;
end.
