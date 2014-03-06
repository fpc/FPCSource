{$mode objfpc}
{$h+}
unit pkgfphttp;

interface

uses Classes,pkgdownload,pkgoptions,fprepos;

Type
  TFPHTTPDownloader = Class(TBaseDownloader)
  Protected
    Procedure HTTPDownload(Const URL : String; Dest : TStream); override;
 end;

implementation

uses
  sysutils,fphttpclient, pkgglobals, pkgmessages;

Procedure TFPHTTPDownloader.HTTPDownload(Const URL : String; Dest : TStream);

begin
  With TFPHTTPClient.Create(Nil) do
    try
      Get(URL,Dest);
      Dest.Position:=0;
    finally
      Free;
    end;
end;

initialization
  RegisterDownloader('FPC',TFPHTTPDownloader);
end.
