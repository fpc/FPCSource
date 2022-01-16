{$mode objfpc}
{$h+}
unit pkgwget;

interface

uses Classes,pkgdownload,pkgoptions,fprepos;

Type

  { TWGetDownloader }

  TWGetDownloader = Class(TBaseDownloader)
  Private
    FWGet : String;
  Protected
    Constructor Create(AOwner: TComponent); override;
    function WGetDownload(Const URL : String; Dest : TStream): Boolean; virtual;
    function FTPDownload(Const URL : String; Dest : TStream): Boolean; override;
    function HTTPDownload(Const URL : String; Dest : TStream): Boolean; override;
 Public
    Property WGet : String Read FWGet Write FWGet;
 end;

implementation

uses
  sysutils,process,
  pkgglobals,
  pkgmessages;

Constructor TWGetDownloader.Create(AOwner: TComponent);

begin
  Inherited;
  wget:='wget';
end;


function TWGetDownloader.WGetDownload(Const URL: String; Dest: TStream): Boolean;

Var
  Buffer : Array[0..4096] of byte;
  Count : Integer;

begin
  Result := False;
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
        Error(SErrDownloadFailed,['WGET',URL,Format('exit status %d',[ExitStatus])])
      else
        Result := True;
    finally
      Free;
    end;
end;

function TWGetDownloader.FTPDownload(Const URL: String; Dest: TStream): Boolean;

begin
  Result := WGetDownload(URL,Dest);
end;

function TWGetDownloader.HTTPDownload(Const URL: String; Dest: TStream): Boolean;

begin
  Result := WGetDownload(URL,Dest);
end;

initialization
  RegisterDownloader('wget',TWGetDownloader);
end.
