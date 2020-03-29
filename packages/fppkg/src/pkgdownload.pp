unit pkgDownload;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pkghandler, pkgFppkg;

Type

  { TBaseDownloader }

  TBaseDownloader = Class(TComponent)
  Private
    FBackupFile : Boolean;
  Protected
    // Needs overriding.
    function FTPDownload(Const URL : String; Dest : TStream): Boolean; Virtual;
    function HTTPDownload(Const URL : String; Dest : TStream): Boolean; Virtual;
    function FileDownload(Const URL : String; Dest : TStream): Boolean; Virtual;
  Public
    function Download(Const URL,DestFileName : String): Boolean;
    function Download(Const URL : String; Dest : TStream): Boolean;
    Property BackupFiles : Boolean Read FBackupFile Write FBackupFile;
  end;
  TBaseDownloaderClass = Class of TBaseDownloader;

  { TDownloadPackage }

  TDownloadPackage = Class(TPackagehandler)
  Public
    function Execute: Boolean;override;
  end;

procedure RegisterDownloader(const AName:string;Downloaderclass:TBaseDownloaderClass);
function GetDownloader(const AName:string):TBaseDownloaderClass;

function DownloadFile(const RemoteFile,LocalFile:String; PackageManager: TpkgFPpkg): Boolean;


implementation

uses
  contnrs,
  uriparser,
  fprepos,
  pkgglobals,
  pkgoptions,
  pkgmessages,
  pkgrepos;

var
  DownloaderList  : TFPHashList;

procedure RegisterDownloader(const AName:string;Downloaderclass:TBaseDownloaderClass);
begin
  if DownloaderList.Find(AName)<>nil then
    begin
      Error('Downloader already registered');
      exit;
    end;
  DownloaderList.Add(AName,Downloaderclass);
end;


function GetDownloader(const AName:string):TBaseDownloaderClass;
begin
  result:=TBaseDownloaderClass(DownloaderList.Find(AName));
  if result=nil then
    Error('Downloader %s not supported',[AName]);
end;


function DownloadFile(const RemoteFile,LocalFile:String; PackageManager: TpkgFPpkg): Boolean;
var
  DownloaderClass : TBaseDownloaderClass;
begin
  DownloaderClass:=GetDownloader(PackageManager.Options.GlobalSection.Downloader);
  with DownloaderClass.Create(nil) do
    try
      Result := Download(RemoteFile,LocalFile);
    finally
      Free;
    end;
end;


{ TBaseDownloader }

function TBaseDownloader.FTPDownload(Const URL: String; Dest: TStream): Boolean;
begin
  Error(SErrNoFTPDownload);
  Result := False;
end;

function TBaseDownloader.HTTPDownload(Const URL: String; Dest: TStream): Boolean;
begin
  Error(SErrNoHTTPDownload);
  Result := False;
end;

function TBaseDownloader.FileDownload(Const URL: String; Dest: TStream): Boolean;

Var
  FN : String;
  F : TFileStream;

begin
  Result := False;
  URIToFilename(URL,FN);
  If Not FileExists(FN) then
    Error(SErrNoSuchFile,[FN]);
  F:=TFileStream.Create(FN,fmOpenRead);
  Try
    Dest.CopyFrom(F,0);
    Result := True;
  Finally
    F.Free;
  end;
end;

function TBaseDownloader.Download(Const URL, DestFileName: String): Boolean;

Var
  F : TFileStream;

begin
  Result := False;
  If FileExists(DestFileName) and BackupFiles then
    BackupFile(DestFileName);
  try
    F:=TFileStream.Create(DestFileName,fmCreate);
    try
      Result := Download(URL,F);
    finally
      F.Free;
    end;
  finally
    if not Result then
      DeleteFile(DestFileName);
  end;
end;

function TBaseDownloader.Download(Const URL: String; Dest: TStream): Boolean;

Var
  URI : TURI;
  P : String;

begin
  Result := False;
  URI:=ParseURI(URL);
  P:=URI.Protocol;
  If CompareText(P,'ftp')=0 then
    Result := FTPDownload(URL,Dest)
  else if (CompareText(P,'http')=0) or (CompareText(P,'https')=0) then
    Result := HTTPDownload(URL,Dest)
  else if CompareText(P,'file')=0 then
    Result := FileDownload(URL,Dest)
  else
    begin
      Error(SErrUnknownProtocol,[P, URL]);
      Result := False;
    end;
end;


{ TDownloadPackage }

function TDownloadPackage.Execute: Boolean;
var
  DownloaderClass : TBaseDownloaderClass;
  P : TFPPackage;
  RemoteArchive: string;
begin
  Result := False;
  P:=PackageManager.PackageByName(PackageName, pkgpkAvailable);
  DownloaderClass:=GetDownloader(PackageManager.Options.GlobalSection.Downloader);
  if Assigned(DownloaderClass) then
    begin
      with DownloaderClass.Create(nil) do
        try
          RemoteArchive := PackageManager.PackageRemoteArchive(P);
          if RemoteArchive <> '' then
            begin
              Log(llCommands,SLogDownloading,[RemoteArchive,PackageManager.PackageLocalArchive(P)]);
              pkgglobals.log(llProgress,SProgrDownloadPackage,[P.Name, P.Version.AsString]);

              // Force the existing of the archives-directory if it is being used
              if (P.Name<>CurrentDirPackageName) and (P.Name<>CmdLinePackageName) then
                ForceDirectories(PackageManager.Options.GlobalSection.ArchivesDir);

              Download(RemoteArchive,PackageManager.PackageLocalArchive(P));
              Result := True;
            end
          else
            Error(SErrDownloadPackageFailed);
        finally
          Free;
        end;
    end;
end;


initialization
  DownloaderList:=TFPHashList.Create;
  RegisterDownloader('base',TBaseDownloader);
  RegisterPkgHandler('downloadpackage',TDownloadPackage);
finalization
  FreeAndNil(DownloaderList);
end.

