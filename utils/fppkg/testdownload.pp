program testdownload;

uses
  Classes,
  pkgwget, // Including this sets the Downloaderclass. Replace with downloader you want...
  pkgdownload;

Var
  F : TFileStream;

begin
  F:=TFileStream.Create('fpc.html',fmcreate);
  Try
    With DownloaderClass.Create(Nil) do
      try
        Download('http://www.freepascal.org',F);
      Finally
        Free;
      end;
  finally
    F.Free;
  end;
end.