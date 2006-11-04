{$mode objfpc}
{$h+}
unit pkglnet;

interface

uses
  SysUtils, Classes,
  lnet, lftp, lhttp, pkgdownload;

Type

  { TLNetDownloader }

  TLNetDownloader = Class(TBasePackageDownloader)
   private
    FQuit: Boolean;
    FFTP: TLFTPClient;
    FHTTP: TLHTTPClient;
    FOutStream: TStream;
   protected
    // callbacks
    function OnHttpClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
      ASize: Integer): Integer;
    procedure OnLNetDisconnect(aSocket: TLSocket);
    procedure OnHttpDoneInput(aSocket: TLHTTPClientSocket);
    procedure OnLNetError(const msg: string; aSocket: TLSocket);
    // overrides
    procedure FTPDownload(Const URL : String; Dest : TStream); override;
    procedure HTTPDownload(Const URL : String; Dest : TStream); override;
   public
    constructor Create(AOwner : TComponent); override;
  end;

implementation

uses
  pkgmessages, uriparser;

{ TLNetDownloader }

function TLNetDownloader.OnHttpClientInput(ASocket: TLHTTPClientSocket;
  ABuffer: pchar; ASize: Integer): Integer;
begin
  Result:=FOutStream.Write(aBuffer[0], aSize);
end;

procedure TLNetDownloader.OnLNetDisconnect(aSocket: TLSocket);
begin
  FQuit:=True;
end;

procedure TLNetDownloader.OnHttpDoneInput(aSocket: TLHTTPClientSocket);
begin
  ASocket.Disconnect;
  FQuit:=True;
end;

procedure TLNetDownloader.OnLNetError(const msg: string; aSocket: TLSocket);
begin
  Error(msg);
  FQuit:=True;
end;

procedure TLNetDownloader.FTPDownload(const URL: String; Dest: TStream);
begin
  inherited FTPDownload(URL, Dest); // TODO
end;

procedure TLNetDownloader.HTTPDownload(const URL: String; Dest: TStream);
var
  URI: TURI;
begin
  FOutStream:=Dest;
  { parse aURL }
  URI := ParseURI(URL);
  
  if URI.Port = 0 then
    URI.Port := 80;

  FHTTP.Host := URI.Host;
  FHTTP.Method := hmGet;
  FHTTP.Port := URI.Port;
  FHTTP.URI := '/' + URI.Document;
  Writeln(FHTTP.Host + FHTTP.URI);
  FHTTP.SendRequest;

  FQuit:=False;
  while not FQuit do
    FHTTP.CallAction;
  FOutStream:=nil; // to be sure
end;

constructor TLNetDownloader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FFTP:=TLFTPClient.Create(Self);
  FFTP.OnError:=@OnLNetError;

  FHTTP:=TLHTTPClient.Create(Self);
  FHTTP.Timeout := 1000; // go by 1s times if nothing happens
  FHTTP.OnDisconnect := @OnLNetDisconnect;
  FHTTP.OnDoneInput := @OnHttpDoneInput;
  FHTTP.OnError := @OnLNetError;
  FHTTP.OnInput := @OnHttpClientInput;
end;

initialization
  DownloaderClass:=TLNetDownloader;

end.
