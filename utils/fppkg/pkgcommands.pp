unit pkgcommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler;

type
  { TCommandUpdate }

  TCommandUpdate = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

  { TCommandDownload }

  TCommandDownload = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

  { TCommandUnzip }

  TCommandUnzip = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

  { TCommandBuild }

  TCommandBuild = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


  { TCommandInstall }

  TCommandInstall = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


implementation

uses
  pkgmessages,
  pkgglobals,
  pkgoptions,
  pkgdownload;

function TCommandUpdate.Execute(const Args:TActionArgs):boolean;
begin
  DownloadFile(Defaults.RemotePackagesFile,Defaults.LocalPackagesFile);
end;


function TCommandDownload.Execute(const Args:TActionArgs):boolean;
begin
  if not assigned(CurrentPackage) then
    Error(SErrNoPackageSpecified);
  if not FileExists(PackageLocalArchive) then
    ExecuteAction(CurrentPackage,'downloadpackage',Args);
end;


function TCommandUnzip.Execute(const Args:TActionArgs):boolean;
begin
  if not assigned(CurrentPackage) then
    Error(SErrNoPackageSpecified);
  ExecuteAction(CurrentPackage,'unziparchive',Args);
end;


function TCommandBuild.Execute(const Args:TActionArgs):boolean;
begin
  if assigned(CurrentPackage) then
    begin
      if not DirectoryExists(PackageBuildPath) then
        ExecuteAction(CurrentPackage,'unziparchive',Args);
    end;
  ExecuteAction(CurrentPackage,'fpmakebuild',Args);
end;


function TCommandInstall.Execute(const Args:TActionArgs):boolean;
begin
  ExecuteAction(CurrentPackage,'build',Args);
  ExecuteAction(CurrentPackage,'fpmakeinstall',Args);
end;


initialization
  RegisterPkgHandler('update',TCommandUpdate);
  RegisterPkgHandler('download',TCommandDownload);
  RegisterPkgHandler('unzip',TCommandUnzip);
  RegisterPkgHandler('build',TCommandBuild);
  RegisterPkgHandler('install',TCommandInstall);
end.
