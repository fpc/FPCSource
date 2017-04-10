unit pkgcommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pkghandler, fpmkunit;

implementation

uses
  zipper,
  pkgmessages,
  pkgglobals,
  pkgoptions,
  pkgdownload,
  pkgrepos,
  pkgFppkg,
  fpxmlrep,
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
  protected
    function ForceInstall: Boolean; virtual;
  Public
    Procedure Execute;override;
  end;

  { TCommandInstallForced }

  TCommandInstallForced = Class(TCommandInstall)
  protected
    function ForceInstall: Boolean; override;
  end;


  { TCommandUnInstall }

  TCommandUnInstall = Class(TPackagehandler)
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

  { TCommandListSettings }

  TCommandListSettings = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

  { TCommandInfo }

  TCommandInfo = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;

var
  DependenciesDepth: integer;

{ TCommandInstallForced }

function TCommandInstallForced.ForceInstall: Boolean;
begin
  Result := True;
end;

{ TCommandInfo }

procedure TCommandInfo.Execute;
var
  P : TFPPackage;
  S : string;
  I : Integer;
begin
  if PackageName='' then
    Error(SErrNoPackageSpecified);
  P:=PackageManager.PackageByName(PackageName, pkgpkAvailable);

  log(llProgres,SLogPackageInfoName,[P.Name]);
  S := P.Email;
  if S <> '' then
    S := '<' + S +'>';
  log(llProgres,SLogPackageInfoAuthor,[P.Author, S]);
  log(llProgres,SLogPackageInfoVersion,[P.Version.AsString]);
  log(llProgres,SLogPackageInfoCategory,[P.Category]);
  log(llProgres,SLogPackageInfoWebsite,[P.HomepageURL]);
  log(llProgres,SLogPackageInfoLicense,[P.License]);

  log(llProgres,SLogPackageInfoOSes,[OSesToString(P.OSes)]);
  log(llProgres,SLogPackageInfoCPUs,[CPUSToString(P.CPUs)]);

  log(llProgres,SLogPackageInfoDescription1);
  log(llProgres,SLogPackageInfoDescription2,[P.Description]);

  if P.Dependencies.Count>0 then
    begin
      log(llProgres,SLogPackageInfoDependencies1,[]);
      for I := 0 to P.Dependencies.Count-1 do
        begin
          if not P.Dependencies[I].MinVersion.Empty then
            S := '('+P.Dependencies[I].MinVersion.AsString+')'
          else
            S := '';
          log(llProgres,SLogPackageInfoDependencies2,[P.Dependencies[I].PackageName,S]);
        end;
    end;
end;

{ TCommandUnInstall }

procedure TCommandUnInstall.Execute;
var
  AvailP: TFPPackage;
begin
  if PackageName<>'' then
    begin
      if (PackageName=CmdLinePackageName) then
        begin
          ExecuteAction(PackageName,'unzip');
        end
      else if (PackageName<>CurrentDirPackageName) then
        begin
          AvailP:=PackageManager.FindPackage(PackageName, pkgpkAvailable);
          if Assigned(AvailP) then
            begin
              if AvailP.PackagesStructure.UnzipBeforeUse then
                ExecuteAction(PackageName,'unzip');
            end;
        end;
    end;
  ExecuteAction(PackageName,'fpmakeuninstall');
end;

{ TCommandListSettings }

procedure TCommandListSettings.Execute;
begin
  PackageManager.Options.LogValues(llProgres);
  PackageManager.CompilerOptions.LogValues(llProgres,'');
  PackageManager.FPMakeCompilerOptions.LogValues(llProgres,'fpmake-building ');
end;


procedure TCommandAddConfig.Execute;
begin
{
  Log(llInfo,SLogGeneratingCompilerConfig,[S]);
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
  if (PackageManager.Options.GlobalSection.RemoteMirrorsURL<>'') and
     (PackageManager.Options.GlobalSection.RemoteRepository='auto') then
    begin
      Log(llCommands,SLogDownloading,[PackageManager.Options.GlobalSection.RemoteMirrorsURL,PackageManager.Options.GlobalSection.LocalMirrorsFile]);
      DownloadFile(PackageManager.Options.GlobalSection.RemoteMirrorsURL,PackageManager.Options.GlobalSection.LocalMirrorsFile, PackageManager);
      PackageManager.LoadLocalAvailableMirrors;
    end;
  // Download packages.xml
  PackagesURL:=PackageManager.GetRemoteRepositoryURL(PackagesFileName);
  Log(llCommands,SLogDownloading,[PackagesURL,PackageManager.Options.GlobalSection.LocalPackagesFile]);
  DownloadFile(PackagesURL,PackageManager.Options.GlobalSection.LocalPackagesFile,PackageManager);
  // Read the repository again
  PackageManager.ScanAvailablePackages;
  // no need to log errors again
  PackageManager.ScanPackages;
end;


procedure TCommandListPackages.Execute;
begin
  ListPackages(PackageManager.Options.CommandLineSection.ShowLocation);
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
  P:=PackageManager.PackageByName(PackageName, pkgpkAvailable);
  if not FileExists(PackageManager.PackageLocalArchive(P)) then
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
  P:=PackageManager.PackageByName(PackageName, pkgpkAvailable);
  BuildDir:=PackageManager.PackageBuildPath(P);
  ArchiveFile:=PackageManager.PackageLocalArchive(P);
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
      Log(llCommands,SLogUnzippping,[ArchiveFile]);
      OutputPath:=PackageManager.PackageBuildPath(P);
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
      if (PackageName=CmdLinePackageName) or (PackageName=URLPackageName) then
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
var
  P: TFPPackage;
begin
  if PackageName<>'' then
    begin
      // For local files we need the information inside the zip to get the
      // dependencies
      if (PackageName=CmdLinePackageName) or (PackageName=URLPackageName) then
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
          P:=PackageManager.FindPackage(PackageName, pkgpkAvailable);
          if Assigned(P) then
            begin
              if P.PackagesStructure.UnzipBeforeUse then
                ExecuteAction(PackageName,'unzip');
            end;
          ExecuteAction(PackageName,'installdependencies');
        end;
    end;
  ExecuteAction(PackageName,'fpmakebuild');
end;

function TCommandInstall.ForceInstall: Boolean;
begin
  Result := False;
end;

procedure TCommandInstall.Execute;

var
  S : String;
  P   : TFPPackage;
  InstallRepo: TFPRepository;

  function GetFpmFilename(APackage: TFPPackage): string;
  var
    ConfFile: string;
  begin
    Result := '';
    if Assigned(InstallRepo.DefaultPackagesStructure) then
      begin
        ConfFile := InstallRepo.DefaultPackagesStructure.GetConfigFileForPackage(APackage);
        if not FileExistsLog(ConfFile) then
          begin
            // If there is no fpm-file, search for an (obsolete, pre-2.7.x)
            // fpunits.cfg-file
            ConfFile := IncludeTrailingPathDelimiter(Result)+APackage.Name+PathDelim+UnitConfigFileName;
            if FileExistsLog(ConfFile) then
              Result := ConfFile;
          end
        else
          Result := ConfFile;
      end;
  end;

var
  UFN : String;
  AvailPackage: TFPPackage;
begin
  if PackageName<>'' then
    begin
      ExecuteAction(PackageName,'build');

      AvailPackage := PackageManager.FindPackage(PackageName, pkgpkAvailable);
      InstallRepo := PackageManager.GetInstallRepository(AvailPackage);
      case InstallRepo.DefaultPackagesStructure.IsInstallationNeeded(AvailPackage) of
        fpinInstallationNeeded:
          begin
            if Assigned(AvailPackage) then
              S := AvailPackage.GetDebugName
            else
              S := PackageName;
            log(llDebug,SDbgPackageInstallRequired,[S, InstallRepo.RepositoryName]);
            ExecuteAction(PackageName,'fpmakeinstall');
          end;
        fpinInstallationImpossible:
          Error(SErrInstallationImpossible,[PackageName, InstallRepo.RepositoryName]);
        else if ForceInstall then
          begin
            log(llDebug,SDbgForcePackageInstall,[P.Name]);
            ExecuteAction(PackageName,'fpmakeinstall');
          end;
      end;

      if (PackageName=CmdLinePackageName) or (PackageName=CurrentDirPackageName) or
         (PackageName=URLPackageName) then
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

      if Assigned(InstallRepo) then
        begin
          P := InstallRepo.FindPackage(S);
          if not Assigned(P) then
            P := InstallRepo.AddPackage(S);
          if Assigned(P) then
            begin
              UFN:=GetFpmFilename(P);
              if UFN<>'' then
                begin
                  P.LoadUnitConfigFromFile(UFN);
                  if P.IsFPMakeAddIn then
                    AddFPMakeAddIn(P);
                end;
            end;
        end;
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

  function PackageVersionStr(APackage: TFPPackage): string;
  begin
    if Assigned(APackage) then
      Result := APackage.Version.AsString
    else
      Result := 'N/A';
  end;

var
  i : Integer;
  MissingDependency,
  D : TFPDependency;
  P,
  InstalledP,
  AvailP : TFPPackage;
  PackNr: integer;
  ManifestPackages : TFPPackages;
  X : TFPXMLRepositoryHandler;
  L : TStringList;
  status : string;
begin
  if PackageName='' then
    Error(SErrNoPackageSpecified);
  ManifestPackages:=nil;
  // Load dependencies for local packages
  if (PackageName=CmdLinePackageName) or (PackageName=CurrentDirPackageName) or
     (PackageName=URLPackageName) then
    begin
      ExecuteAction(PackageName,'fpmakemanifest');
      ManifestPackages:=TFPPackages.Create(TFPPackage);
      X:=TFPXMLRepositoryHandler.Create;
      try
        X.LoadFromXml(ManifestPackages,ManifestFileName);
      finally
        X.Free;
      end;
      if ManifestPackages.Count>0 then
        begin
          PackNr:=0;
          P := ManifestPackages[PackNr];
        end
      else
        begin
          ManifestPackages.Free;
          Error(SErrManifestNoSinglePackage,[ManifestFileName]);
        end;
    end
  else
    P:=PackageManager.PackageByName(PackageName, pkgpkBoth);

  MissingDependency:=nil;
  while assigned(P) do
    begin
      // Find and List dependencies
      L:=TStringList.Create;
      for i:=0 to P.Dependencies.Count-1 do
        begin
          D:=P.Dependencies[i];
          if not ((PackageManager.CompilerOptions.CompilerOS in D.OSes) and (PackageManager.CompilerOptions.CompilerCPU in D.CPUs)) then
            Log(llDebug,SDbgPackageDependencyOtherTarget,[D.PackageName,MakeTargetString(PackageManager.CompilerOptions.CompilerCPU,PackageManager.CompilerOptions.CompilerOS)])
          // Skip dependencies that are available within the fpmake-file itself
          else if not (assigned(ManifestPackages) and assigned(ManifestPackages.FindPackage(D.PackageName))) then
            begin
              AvailP := nil;
              InstalledP:=PackageManager.FindPackage(D.PackageName, pkgpkInstalled);
              // Need installation?
              if not assigned(InstalledP) or
                 (InstalledP.Version.CompareVersion(D.MinVersion)<0) then
                begin
                  AvailP:=PackageManager.FindPackage(D.PackageName, pkgpkAvailable);
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
                  if PackageManager.PackageIsBroken(InstalledP, nil) then
                    begin
                      status:='Broken, recompiling';
                      L.Add(D.PackageName);
                    end
                  else
                    status:='OK';
                end;
              Log(llInfo,SLogPackageDependency,
                  [D.PackageName,D.MinVersion.AsString,PackageVersionStr(InstalledP),
                   PackageVersionStr(AvailP),status])
            end
        end;
      if assigned(ManifestPackages) and (PackNr<ManifestPackages.Count-1)  then
        begin
          inc(PackNr);
          P := ManifestPackages[PackNr]
        end
      else
        p := nil;
    end;
  // Give error on first missing dependency
  if assigned(MissingDependency) then
    Error(SErrNoPackageAvailable,[MissingDependency.PackageName,MissingDependency.MinVersion.AsString]);
  // Install needed updates
  if L.Count > 0 then
    begin
      if DependenciesDepth=0 then
        pkgglobals.Log(llProgres,SProgrInstallDependencies);
      inc(DependenciesDepth);

      for i:=0 to L.Count-1 do
        ExecuteAction(L[i],'install-req');

      dec(DependenciesDepth);
      if DependenciesDepth=0 then
        pkgglobals.Log(llProgres,SProgrDependenciesInstalled);
    end;
  FreeAndNil(L);
  if assigned(ManifestPackages) then
    ManifestPackages.Free;
end;


procedure TCommandFixBroken.Execute;
var
  i : integer;
  SL : TStringList;
  BreakLoop : Boolean;
begin
  SL:=TStringList.Create;
  BreakLoop := false;
  repeat
    PackageManager.FindBrokenPackages(SL);
    if SL.Count=0 then
      break;
    pkgglobals.Log(llProgres,SProgrReinstallDependent);
    for i:=0 to SL.Count-1 do
      begin
        ExecuteAction(SL[i],'build');
        ExecuteAction(SL[i],'install-req');
        if PackageManager.PackageIsBroken(PackageManager.PackageByName(SL[i], pkgpkInstalled), nil) then
          begin
            BreakLoop := true;
            pkgglobals.Log(llWarning, SWarnBrokenAfterReinstall, [SL[i]]);
          end;
      end;
  until BreakLoop;
  FreeAndNil(SL);
end;


initialization
  DependenciesDepth:=0;
  RegisterPkgHandler('update',TCommandUpdate);
  RegisterPkgHandler('list',TCommandListPackages);
  RegisterPkgHandler('scan',TCommandScanPackages);
  RegisterPkgHandler('download',TCommandDownload);
  RegisterPkgHandler('unzip',TCommandUnzip);
  RegisterPkgHandler('compile',TCommandCompile);
  RegisterPkgHandler('build',TCommandBuild);
  RegisterPkgHandler('install',TCommandInstallForced);
  RegisterPkgHandler('install-req',TCommandInstall);
  RegisterPkgHandler('uninstall',TCommandUnInstall);
  RegisterPkgHandler('clean',TCommandClean);
  RegisterPkgHandler('archive',TCommandArchive);
  RegisterPkgHandler('installdependencies',TCommandInstallDependencies);
  RegisterPkgHandler('fixbroken',TCommandFixBroken);
  RegisterPkgHandler('listsettings',TCommandListSettings);
  RegisterPkgHandler('info',TCommandInfo);
end.
