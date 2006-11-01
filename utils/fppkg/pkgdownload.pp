unit pkgdownload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pkghandler;
  
Type

  { TBasePackageDownloader }

  TBasePackageDownloader = Class(TPackageHandler)
  Protected
    // Needs overriding.
    Procedure FTPDownload(Const URL : String; Dest : TStream); Virtual;
    Procedure HTTPDownload(Const URL : String; Dest : TStream); Virtual;
    Procedure FileDownload(Const URL : String; Dest : TStream); Virtual;
  Public
    Procedure Download(Const URL,DestFileName : String);
    Procedure Download(Const URL : String; Dest : TStream);
  end;
  TBasePackageDownloaderClass = Class of TBasePackageDownloader;

Var
  DownloaderClass : TBasePackageDownloaderClass;

implementation

uses pkgmessages,uriparser;

{ TBasePackageDownloader }

procedure TBasePackageDownloader.FTPDownload(const URL: String; Dest: TStream);
begin
  Error(SErrNoFTPDownload);
end;

procedure TBasePackageDownloader.HTTPDownload(const URL: String; Dest: TStream);
begin
  Error(SErrNoHTTPDownload);
end;

procedure TBasePackageDownloader.FileDownload(const URL: String; Dest: TStream);

Var
  URI : TURI;
  FN : String;
  F : TFileStream;
  
begin
  URI:=ParseURI(URL);
  FN:=URI.Path+'/'+URI.Document;
  If Not FileExists(FN) then
    Error(SErrNoSuchFile,[FN]);
  F:=TFileStream.Create(FN,fmOpenRead);
  Try
    Dest.CopyFrom(F,0);
  Finally
    F.Free;
  end;
end;

procedure TBasePackageDownloader.Download(const URL, DestFileName: String);

Var
  F : TFileStream;

begin
  If FileExists(DestFileName) and BackupFiles then
    BackupFile(DestFileName);
  F:=TFileStream.Create(DestFileName,fmCreate);
  Try
    Download(URL,F);
  Finally
    F.Free;
  end;
end;

procedure TBasePackageDownloader.Download(const URL: String; Dest: TStream);

Var
  URI : TURI;
  P : String;
  
begin
  URI:=ParseURI(URL);
  P:=URI.Protocol;
  If CompareText(P,'ftp')=0 then
    FTPDownload(URL,Dest)
  else if CompareText(P,'http')=0 then
    HTTPDownload(URL,Dest)
  else if CompareText(P,'file')=0 then
    FileDownload(URL,Dest)
  else
    Error(SErrUnknownProtocol,[P]);
end;

initialization
  // Default value.
  DownloaderClass := TBasePackageDownloader;

end.

