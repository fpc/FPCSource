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
    Procedure Execute;override;
  end;

  { TCommandUpdate }

  TCommandUpdate = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandListPackages }

  TCommandListPackages = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandScanPackages }

  TCommandScanPackages = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandDownload }

  TCommandDownload = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandUnzip }

  TCommandUnzip = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandCompile }

  TCommandCompile = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandBuild }

  TCommandBuild = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandInstall }

  TCommandInstall = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandClean }

  TCommandClean = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandArchive }

  TCommandArchive = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandInstallDependencies }

  TCommandInstallDependencies = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandFixBroken }

  TCommandFixBroken = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;


procedure TCommandAddConfig.Execute;
begin
{
  Log(vlInfo,SLogGeneratingCompilerConfig,[S]);
  Options.InitCompilerDefaults(Args[2]);
  Options.SaveCompilerToFile(S);
}
end;


procedure TCommandUpdate.Execute;
var
  PackagesURL :  String;
begin
  // Download and load mirrors.xml
  // This can be skipped when a custom RemoteRepository is configured
  if (GlobalOptions.RemoteMirrorsURL<>'') and
     (GlobalOptions.RemoteRepository='auto') then
    begin
      Log(vlCommands,SLogDownloading,[GlobalOptions.RemoteMirrorsURL,GlobalOptions.LocalMirrorsFile]);
      DownloadFile(GlobalOptions.RemoteMirrorsURL,GlobalOptions.LocalMirrorsFile);
      LoadLocalAvailableMirrors;
    end;
  // Download packages.xml
  PackagesURL:=GetRemoteRepositoryURL(PackagesFileName);
  Log(vlCommands,SLogDownloading,[PackagesURL,GlobalOptions.LocalPackagesFile]);
  DownloadFile(PackagesURL,GlobalOptions.LocalPackagesFile);
  // Read the repository again
  LoadLocalAvailableRepository;
  // no need to log errors again
  FindInstalledPackages(CompilerOptions,False);
end;


procedure TCommandListPackages.Execute;
begin
  ListPackages;
end;


procedure TCommandScanPackages.Execute;
begin
  { nothing, already handled in fppkg.pp as special case
    before the local fppkg directory is processed }
end;


procedure TCommandDownload.Execute;
var
  P : TFPPackage;
begin
  if PackageName='' then
    Error(SErrNoPackageSpecified);
  P:=AvailableRepository.PackageByName(PackageName);
  if not FileExists(PackageLocalArchive(P)) then
    ExecuteAction(PackageName,'downloadpackage');
end;


procedure TCommandUnzip.Execute;
Var
  BuildDir : string;
  ArchiveFile : String;
  P : TFPPackage;
begin
  if PackageName='' then
    Error(SErrNoPackageSpecified);
  P:=AvailableRepository.PackageByName(PackageName);
  BuildDir:=PackageBuildPath(P);
  ArchiveFile:=PackageLocalArchive(P);
  if not FileExists(ArchiveFile) then
    ExecuteAction(PackageName,'downloadpackage');
  { Create builddir, remove it first if needed }
  if DirectoryExists(BuildDir) then
    DeleteDir(BuildDir);
  ForceDirectories(BuildDir);
  SetCurrentDir(BuildDir);
  { Unzip Archive }
  With TUnZipper.Create do
    try
      Log(vlCommands,SLogUnzippping,[ArchiveFile]);
      OutputPath:=PackageBuildPath(P);
      UnZipAllFiles(ArchiveFile);
    Finally
      Free;
    end;
end;


procedure TCommandCompile.Execute;
begin
  if PackageName<>'' then
    begin
      // For local files we need the information inside the zip to get the
      // dependencies
      if (PackageName=CmdLinePackageName) then
        begin
          ExecuteAction(PackageName,'unzip');
          ExecuteAction(PackageName,'installdependencies');
        end
      else
        if (PackageName=CurrentDirPackageName) then
          begin
            ExecuteAction(PackageName,'installdependencies');
          end
      else
        begin
          ExecuteAction(PackageName,'installdependencies');
          ExecuteAction(PackageName,'unzip');
        end;
    end;
  ExecuteAction(PackageName,'fpmakecompile');
end;


procedure TCommandBuild.Execute;
begin
  if PackageName<>'' then
    begin
      // For local files we need the information inside the zip to get the
      // dependencies
      if (PackageName=CmdLinePackageName) then
        begin
          ExecuteAction(PackageName,'unzip');
          ExecuteAction(PackageName,'installdependencies');
        end
      else
        if (PackageName=CurrentDirPackageName) then
          begin
            ExecuteAction(PackageName,'installdependencies');
          end
      else
        begin
          ExecuteAction(PackageName,'installdependencies');
          ExecuteAction(PackageName,'unzip');
        end;
    end;
  ExecuteAction(PackageName,'fpmakebuild');
end;


procedure TCommandInstall.Execute;
var
  UFN,S : String;
  P   : TFPPackage;
begin
  if PackageName<>'' then
    begin
      ExecuteAction(PackageName,'build');
      ExecuteAction(PackageName,'fpmakeinstall');
      if (PackageName=CmdLinePackageName) or (PackageName=CurrentDirPackageName) then
        begin
          // Load package name from manifest
          if not FileExists(ManifestFileName) then
            ExecuteAction(PackageName,'fpmakemanifest');
          P:=LoadManifestFromFile(ManifestFileName);
          S:=P.Name;
          FreeAndNil(P);
        end
      else
        S:=PackageName;
      P:=InstalledRepository.FindPackage(S);
      if not assigned(P) then
        P:=InstalledRepository.AddPackage(S);
      if GlobalOptions.InstallGlobal then
        UFN:=CompilerOptions.GlobalUnitDir
      else
        UFN:=CompilerOptions.LocalUnitDir;
      UFN:=IncludeTrailingPathDelimiter(UFN)+S+PathDelim+UnitConfigFileName;
      LoadUnitConfigFromFile(P,UFN);
    end
  else
    ExecuteAction(PackageName,'fpmakeinstall');
end;


procedure TCommandClean.Execute;
begin
  ExecuteAction(PackageName,'fpmakeclean');
end;


procedure TCommandArchive.Execute;
begin
  ExecuteAction(PackageName,'fpmakearchive');
end;


procedure TCommandInstallDependencies.Execute;
var
  i : Integer;
  MissingDependency,
  D : TFPDependency;
  P,
  InstalledP,
  AvailP : TFPPackage;
  L : TStringList;
  status : string;
  FreeManifest : boolean;
begin
  if PackageName='' then
    Error(SErrNoPackageSpecified);
  FreeManifest:=false;
  // Load dependencies for local packages
  if (PackageName=CmdLinePackageName) or (PackageName=CurrentDirPackageName) then
    begin
      ExecuteAction(PackageName,'fpmakemanifest');
      P:=LoadManifestFromFile(ManifestFileName);
      FreeManifest:=true;
    end
  else
    P:=AvailableRepository.PackageByName(PackageName);
  // Find and List dependencies
  MissingDependency:=nil;
  L:=TStringList.Create;
  for i:=0 to P.Dependencies.Count-1 do
    begin
      D:=P.Dependencies[i];
      if (CompilerOptions.CompilerOS in D.OSes) and
         (CompilerOptions.CompilerCPU in D.CPUs) then
        begin
          InstalledP:=InstalledRepository.FindPackage(D.PackageName);
          // Need installation?
          if not assigned(InstalledP) or
             (InstalledP.Version.CompareVersion(D.MinVersion)<0) then
            begin
              AvailP:=AvailableRepository.FindPackage(D.PackageName);
              if not assigned(AvailP) or
                 (AvailP.Version.CompareVersion(D.MinVersion)<0) then
                begin
                  status:='Not Available!';
                  MissingDependency:=D;
                end
              else
                begin
                  status:='Updating';
                  L.Add(D.PackageName);
                end;
            end
          else
            begin
              if PackageIsBroken(InstalledP) then
                begin
                  status:='Broken, recompiling';
                  L.Add(D.PackageName);
                end
              else
                status:='OK';
            end;
          Log(vlInfo,SLogPackageDependency,
              [D.PackageName,D.MinVersion.AsString,PackageInstalledVersionStr(D.PackageName),
               PackageAvailableVersionStr(D.PackageName),status]);
        end
      else
        Log(vlDebug,SDbgPackageDependencyOtherTarget,[D.PackageName,MakeTargetString(CompilerOptions.CompilerCPU,CompilerOptions.CompilerOS)]);
    end;
  // Give error on first missing dependency
  if assigned(MissingDependency) then
    Error(SErrNoPackageAvailable,[MissingDependency.PackageName,MissingDependency.MinVersion.AsString]);
  // Install needed updates
  for i:=0 to L.Count-1 do
    ExecuteAction(L[i],'install');
  FreeAndNil(L);
  if FreeManifest then
    FreeAndNil(P);
end;


procedure TCommandFixBroken.Execute;
var
  i : integer;
  SL : TStringList;
begin
  SL:=TStringList.Create;
  repeat
    FindBrokenPackages(SL);
    if SL.Count=0 then
      break;
    for i:=0 to SL.Count-1 do
      begin
        ExecuteAction(SL[i],'build');
        ExecuteAction(SL[i],'install');
      end;
  until false;
  FreeAndNil(SL);
end;


initialization
  RegisterPkgHandler('update',TCommandUpdate);
  RegisterPkgHandler('list',TCommandListPackages);
  RegisterPkgHandler('scan',TCommandScanPackages);
  RegisterPkgHandler('download',TCommandDownload);
  RegisterPkgHandler('unzip',TCommandUnzip);
  RegisterPkgHandler('compile',TCommandCompile);
  RegisterPkgHandler('build',TCommandBuild);
  RegisterPkgHandler('install',TCommandInstall);
  RegisterPkgHandler('clean',TCommandClean);
  RegisterPkgHandler('archive',TCommandArchive);
  RegisterPkgHandler('installdependencies',TCommandInstallDependencies);
  RegisterPkgHandler('fixbroken',TCommandFixBroken);
end.
