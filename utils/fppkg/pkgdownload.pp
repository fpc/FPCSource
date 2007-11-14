unit pkgdownload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pkghandler;

Type

  { TBaseDownloader }

  TBaseDownloader = Class(TComponent)
  Private
    FBackupFile : Boolean;
  Protected
    // Needs overriding.
    Procedure FTPDownload(Const URL : String; Dest : TStream); Virtual;
    Procedure HTTPDownload(Const URL : String; Dest : TStream); Virtual;
    Procedure FileDownload(Const URL : String; Dest : TStream); Virtual;
  Public
    Procedure Download(Const URL,DestFileName : String);
    Procedure Download(Const URL : String; Dest : TStream);
    Property BackupFiles : Boolean Read FBackupFile Write FBackupFile;
  end;
  TBaseDownloaderClass = Class of TBaseDownloader;

  { TDownloadPackage }

  TDownloadPackage = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

Var
  DownloaderClass : TBaseDownloaderClass;

procedure DownloadFile(const RemoteFile,LocalFile:String);


implementation

uses
  uriparser,
  pkgglobals,
  pkgmessages;


procedure DownloadFile(const RemoteFile,LocalFile:String);
begin
  with DownloaderClass.Create(nil) do
    try
      Download(RemoteFile,LocalFile);
    finally
      Free;
    end;
end;


{ TBaseDownloader }

procedure TBaseDownloader.FTPDownload(const URL: String; Dest: TStream);
begin
  Error(SErrNoFTPDownload);
end;

procedure TBaseDownloader.HTTPDownload(const URL: String; Dest: TStream);
begin
  Error(SErrNoHTTPDownload);
end;

procedure TBaseDownloader.FileDownload(const URL: String; Dest: TStream);

Var
  FN : String;
  F : TFileStream;

begin
  URIToFilename(URL,FN);
  If Not FileExists(FN) then
    Error(SErrNoSuchFile,[FN]);
  F:=TFileStream.Create(FN,fmOpenRead);
  Try
    Dest.CopyFrom(F,0);
  Finally
    F.Free;
  end;
end;

procedure TBaseDownloader.Download(const URL, DestFileName: String);

Var
  F : TFileStream;

begin
  If FileExists(DestFileName) and BackupFiles then
    BackupFile(DestFileName);
  try
    F:=TFileStream.Create(DestFileName,fmCreate);
    try
      Download(URL,F);
    finally
      F.Free;
    end;
  except
    DeleteFile(DestFileName);
    raise;
  end;
end;

procedure TBaseDownloader.Download(const URL: String; Dest: TStream);

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


{ TDownloadPackage }

function TDownloadPackage.Execute(const Args:TActionArgs):boolean;
begin
  with DownloaderClass.Create(nil) do
    try
      Log(vCommands,SLogDownloading,[PackageRemoteArchive,PackageLocalArchive]);
      Download(PackageRemoteArchive,PackageLocalArchive);
    finally
      Free;
    end;
end;


initialization
  // Default value.
  DownloaderClass := TBaseDownloader;

  RegisterPkgHandler('downloadpackage',TDownloadPackage);
end.

