{$mode objfpc}
{$h+}
unit pkglnet;

interface

uses
  SysUtils, Classes, StrUtils,
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
  pkgmessages;

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
  aURL, Host, URI, FileName, AltFileName: string;
  index, Port: integer;
begin
  FOutStream:=Dest;
  { parse aURL }
  aURL := URL;
  if not (Copy(aURL, 1, 7) = 'http://') then begin
    Error('URL should start with http://.');
    Exit;
  end;

  index := PosEx('/', aURL, 8);
  if index = 0 then begin
    aURL := aURL + '/index.html';
    index := PosEx('/', aURL, 8);
  end;

  Host := Copy(aURL, 8, index-8);
  URI := Copy(aURL, index, Length(aURL) + 1 - index);
  index := Pos(':', Host);
  if index > 0 then
  begin
    Port := StrToIntDef(Copy(Host, index+1, Length(Host)-index), -1);
    if (Port < 0) or (Port > 65535) then begin
      Error('Port number out of range.');
      Exit;
    end;
    SetLength(Host, index-1);
  end else
    Port := 80;

  index := RPos('/', URI);
  if index > 0 then
    FileName := Copy(URI, index+1, Length(URI)-index);
  if Length(FileName) = 0 then
    FileName := 'index.html';

  FHTTP.Host := Host;
  FHTTP.Method := hmGet;
  FHTTP.Port := Port;
  FHTTP.URI := URI;
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
