{$mode objfpc}
{$h+}
unit pkgwget; 

interface

uses Classes,pkgdownload;

Type 
  TWGetDownloader = Class(TBasePackageDownloader)
  Private 
    FWGet : String;
  Protected
    Procedure WGetDownload(Const URL : String; Dest : TStream); virtual;
    Procedure FTPDownload(Const URL : String; Dest : TStream); override;
    Procedure HTTPDownload(Const URL : String; Dest : TStream); override;
 Public
    Property WGet : String Read FWGet Write FWGet; 
 end;   

implementation

uses process,pkgmessages;

Procedure TWGetDownloader.WGetDownload(Const URL : String; Dest : TStream);

Var
  Buffer : Array[0..4096] of byte;
  Count : Integer;
  
begin
  With TProcess.Create(Self) do
    try
      CommandLine:=WGet+' -q --output-document=- '+url;
      Options:=[poUsePipes,poNoConsole];
      Execute; 
      While Running do
        begin
        Count:=Output.Read(Buffer,SizeOf(Buffer));
        If (Count>0) then
          Dest.WriteBuffer(Buffer,Count);
        end;
      If (ExitStatus<>0) then
        Error(SErrWGetDownloadFailed,[ExitStatus]);
    finally
      Free;
    end;
end;

Procedure TWGetDownloader.FTPDownload(Const URL : String; Dest : TStream);

begin
  WGetDownload(URL,Dest);
end;

Procedure TWGetDownloader.HTTPDownload(Const URL : String; Dest : TStream); 

begin
  WGetDownload(URL,Dest);
end;

end.