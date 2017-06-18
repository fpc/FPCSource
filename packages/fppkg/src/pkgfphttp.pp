{$mode objfpc}
{$h+}
unit pkgfphttp;

interface

uses Classes,pkgdownload,pkgoptions,fprepos;

Type

  { TFPHTTPDownloader }

  TFPHTTPDownloader = Class(TBaseDownloader)
  Protected
    function HTTPDownload(Const URL : String; Dest : TStream): Boolean; override;
 end;

implementation

uses
  sysutils,fphttpclient, pkgglobals, pkgmessages;

function TFPHTTPDownloader.HTTPDownload(Const URL: String; Dest: TStream): Boolean;

begin
  Result := False;
  With TFPHTTPClient.Create(Nil) do
    try
      AllowRedirect := True;
      Get(URL,Dest);
      Dest.Position:=0;
      Result := True;
    finally
      Free;
    end;
end;

initialization
  RegisterDownloader('FPC',TFPHTTPDownloader);
end.
