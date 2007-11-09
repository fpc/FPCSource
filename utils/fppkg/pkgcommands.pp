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
      Log(vCommands,SLogUnzippping,[ArchiveFile]);
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
      ExecuteAction(CurrentPackage,'unzip',Args);
      ExecuteAction(CurrentPackage,'installdependencies',Args);
    end;
  ExecuteAction(CurrentPackage,'fpmakecompile',Args);
  Result:=true;
end;


function TCommandBuild.Execute(const Args:TActionArgs):boolean;
begin
  if assigned(CurrentPackage) then
    begin
      ExecuteAction(CurrentPackage,'unzip',Args);
      ExecuteAction(CurrentPackage,'installdependencies',Args);
    end;
  ExecuteAction(CurrentPackage,'fpmakebuild',Args);
  Result:=true;
end;


function TCommandInstall.Execute(const Args:TActionArgs):boolean;
begin
  if assigned(CurrentPackage) then
    ExecuteAction(CurrentPackage,'build',Args);
  ExecuteAction(CurrentPackage,'fpmakeinstall',Args);
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
      if (not DepPackage.InstalledVersion.Empty) and
         (DepPackage.InstalledVersion.CompareVersion(D.MinVersion)<0) then
        begin
          if DepPackage.Version.CompareVersion(D.MinVersion)<0 then
            status:='Required Version Not Available!'
          else
            status:='Needs update';
          L.Add(DepPackage.Name);
        end
      else
        status:='OK';
      Log(vDebug,'Dependency '+D.PackageName+'-'+D.MinVersion.AsString+' ('+status+')');
    end;
  // Install needed updates
  for i:=0 to L.Count-1 do
    begin
      DepPackage:=CurrentRepository.PackageByName(L[i]);
      ExecuteAction(DepPackage,'install');
    end;

  FreeAndNil(L);
  Result:=true;
end;


initialization
  RegisterPkgHandler('update',TCommandUpdate);
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
