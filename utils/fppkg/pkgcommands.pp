unit pkgcommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler;

implementation

uses
  zipper,
  pkgmessages,
  pkgglobals,
  pkgoptions,
  pkgdownload,
  pkgrepos,
  fprepos;

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

  { TCommandAllAvail }

  TCommandAllAvail = Class(TPackagehandler)
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

  { TCommandCompile }

  TCommandCompile = Class(TPackagehandler)
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

  { TCommandArchive }

  TCommandArchive = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

  { TCommandInstallDependencies }

  TCommandInstallDependencies = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


function TCommandAddConfig.Execute(const Args:TActionArgs):boolean;
begin
{
  Log(vlInfo,SLogGeneratingCompilerConfig,[S]);
  Options.InitCompilerDefaults(Args[2]);
  Options.SaveCompilerToFile(S);
}
  Result:=true;
end;


function TCommandUpdate.Execute(const Args:TActionArgs):boolean;
var
  PackagesURL :  String;
begin
  // Download mirrors.xml
  Log(vlCommands,SLogDownloading,[GlobalOptions.RemoteMirrorsURL,GlobalOptions.LocalMirrorsFile]);
  DownloadFile(GlobalOptions.RemoteMirrorsURL,GlobalOptions.LocalMirrorsFile);
  LoadLocalMirrors;
  // Download packages.xml
  PackagesURL:=GetRemoteRepositoryURL(PackagesFileName);
  Log(vlCommands,SLogDownloading,[PackagesURL,GlobalOptions.LocalPackagesFile]);
  DownloadFile(PackagesURL,GlobalOptions.LocalPackagesFile);
  // Read the repository again
  LoadLocalRepository;
  LoadLocalStatus;
  Result:=true;
end;


function TCommandAllAvail.Execute(const Args:TActionArgs):boolean;
begin
  ListLocalRepository(true);
  Result:=true;
end;


function TCommandAvail.Execute(const Args:TActionArgs):boolean;
begin
  ListLocalRepository(false);
  Result:=true;
end;


function TCommandScanPackages.Execute(const Args:TActionArgs):boolean;
begin
  RebuildRemoteRepository;
  ListRemoteRepository;
  SaveRemoteRepository;
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
Var
  BuildDir : string;
  ArchiveFile : String;
begin
  BuildDir:=PackageBuildPath;
  ArchiveFile:=PackageLocalArchive;
  if not assigned(CurrentPackage) then
    Error(SErrNoPackageSpecified);
  if not FileExists(ArchiveFile) then
    ExecuteAction(CurrentPackage,'downloadpackage');
  { Create builddir, remove it first if needed }
  if DirectoryExists(BuildDir) then
    DeleteDir(BuildDir);
  ForceDirectories(BuildDir);
  SetCurrentDir(BuildDir);
  { Unzip Archive }
  With TUnZipper.Create do
    try
      Log(vlCommands,SLogUnzippping,[ArchiveFile]);
      OutputPath:=PackageBuildPath;
      UnZipAllFiles(ArchiveFile);
    Finally
      Free;
    end;
  Result:=true;
end;


function TCommandCompile.Execute(const Args:TActionArgs):boolean;
begin
  if assigned(CurrentPackage) then
    begin
      ExecuteAction(CurrentPackage,'installdependencies',Args);
      ExecuteAction(CurrentPackage,'unzip',Args);
    end;
  ExecuteAction(CurrentPackage,'fpmakecompile',Args);
  Result:=true;
end;


function TCommandBuild.Execute(const Args:TActionArgs):boolean;
begin
  if assigned(CurrentPackage) then
    begin
      ExecuteAction(CurrentPackage,'installdependencies',Args);
      ExecuteAction(CurrentPackage,'unzip',Args);
    end;
  ExecuteAction(CurrentPackage,'fpmakebuild',Args);
  Result:=true;
end;


function TCommandInstall.Execute(const Args:TActionArgs):boolean;
begin
  if assigned(CurrentPackage) then
    ExecuteAction(CurrentPackage,'build',Args);
  ExecuteAction(CurrentPackage,'fpmakeinstall',Args);
  // Update local status file
  if assigned(CurrentPackage) then
    begin
      CurrentPackage.InstalledVersion.Assign(CurrentPackage.Version);
      SaveLocalStatus;
    end;
  Result:=true;
end;


function TCommandArchive.Execute(const Args:TActionArgs):boolean;
begin
  ExecuteAction(CurrentPackage,'fpmakearchive',Args);
  Result:=true;
end;


function TCommandInstallDependencies.Execute(const Args:TActionArgs):boolean;
var
  i : Integer;
  D : TFPDependency;
  DepPackage : TFPPackage;
  L : TStringList;
  status : string;
begin
  if not assigned(CurrentPackage) then
    Error(SErrNoPackageSpecified);
  // List dependencies
  L:=TStringList.Create;
  for i:=0 to CurrentPackage.Dependencies.Count-1 do
    begin
      D:=CurrentPackage.Dependencies[i];
      DepPackage:=CurrentRepository.PackageByName(D.PackageName);
      // Need installation?
      if (DepPackage.InstalledVersion.Empty) or
         (DepPackage.InstalledVersion.CompareVersion(D.MinVersion)<0) then
        begin
          if DepPackage.Version.CompareVersion(D.MinVersion)<0 then
            status:='Not Available!'
          else
            status:='Updating';
          L.Add(DepPackage.Name);
        end
      else
        status:='OK';
      Log(vlDebug,SDbgPackageDependency,
          [D.PackageName,D.MinVersion.AsString,DepPackage.InstalledVersion.AsString,DepPackage.Version.AsString,status]);
    end;
  // Install needed updates
  for i:=0 to L.Count-1 do
    begin
      DepPackage:=CurrentRepository.PackageByName(L[i]);
      if DepPackage.Version.CompareVersion(D.MinVersion)<0 then
        Error(SErrNoPackageAvailable,[D.PackageName,D.MinVersion.AsString]);
      ExecuteAction(DepPackage,'install');
    end;
  FreeAndNil(L);
  Result:=true;
end;


initialization
  RegisterPkgHandler('update',TCommandUpdate);
  RegisterPkgHandler('allavail',TCommandAllAvail);
  RegisterPkgHandler('avail',TCommandAvail);
  RegisterPkgHandler('scan',TCommandScanPackages);
  RegisterPkgHandler('download',TCommandDownload);
  RegisterPkgHandler('unzip',TCommandUnzip);
  RegisterPkgHandler('compile',TCommandCompile);
  RegisterPkgHandler('build',TCommandBuild);
  RegisterPkgHandler('install',TCommandInstall);
  RegisterPkgHandler('archive',TCommandArchive);
  RegisterPkgHandler('installdependencies',TCommandInstallDependencies);
end.
