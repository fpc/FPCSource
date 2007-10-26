unit pkgcommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler;

implementation

uses
  pkgmessages,
  pkgglobals,
  pkgoptions,
  pkgdownload,
  pkgrepos;

type
  { TCommandAddConfig }

  TCommandAddConfig = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

  { TCommandUpdate }

  TCommandUpdate = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

  { TCommandAvail }

  TCommandAvail = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

  { TCommandScanPackages }

  TCommandScanPackages = Class(TPackagehandler)
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


function TCommandAddConfig.Execute(const Args:TActionArgs):boolean;
begin
{
  Log(vInfo,SLogGeneratingCompilerConfig,[S]);
  Options.InitCompilerDefaults(Args[2]);
  Options.SaveCompilerToFile(S);
}
  Result:=true;
end;


function TCommandUpdate.Execute(const Args:TActionArgs):boolean;
begin
  DownloadFile(Options.RemotePackagesFile,Options.LocalPackagesFile);
  LoadLocalRepository;
  Result:=true;
end;


function TCommandAvail.Execute(const Args:TActionArgs):boolean;
begin
  ListRepository;
  Result:=true;
end;


function TCommandScanPackages.Execute(const Args:TActionArgs):boolean;
begin
  RebuildRepository;
  ListRepository;
  SaveRepository;
  Result:=true;
end;


function TCommandDownload.Execute(const Args:TActionArgs):boolean;
begin
  if not assigned(CurrentPackage) then
    Error(SErrNoPackageSpecified);
  if not FileExists(PackageLocalArchive) then
    ExecuteAction(CurrentPackage,'downloadpackage',Args);
  Result:=true;
end;


function TCommandUnzip.Execute(const Args:TActionArgs):boolean;
begin
  if not assigned(CurrentPackage) then
    Error(SErrNoPackageSpecified);
  ExecuteAction(CurrentPackage,'unziparchive',Args);
  Result:=true;
end;


function TCommandBuild.Execute(const Args:TActionArgs):boolean;
begin
  if assigned(CurrentPackage) then
    begin
      if not DirectoryExists(PackageBuildPath) then
        ExecuteAction(CurrentPackage,'unziparchive',Args);
    end;
  ExecuteAction(CurrentPackage,'fpmakebuild',Args);
  Result:=true;
end;


function TCommandInstall.Execute(const Args:TActionArgs):boolean;
begin
  ExecuteAction(CurrentPackage,'build',Args);
  ExecuteAction(CurrentPackage,'fpmakeinstall',Args);
  Result:=true;
end;


initialization
  RegisterPkgHandler('update',TCommandUpdate);
  RegisterPkgHandler('avail',TCommandAvail);
  RegisterPkgHandler('scan',TCommandScanPackages);
  RegisterPkgHandler('download',TCommandDownload);
  RegisterPkgHandler('unzip',TCommandUnzip);
  RegisterPkgHandler('build',TCommandBuild);
  RegisterPkgHandler('install',TCommandInstall);
end.
