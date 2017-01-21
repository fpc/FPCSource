unit pkgFppkg;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  fprepos,
  pkgmessages,
  pkgglobals,
  pkgoptions,
  pkgPackagesStructure;

type

  { TpkgFPpkg }

  TpkgPackageKind = (pkgpkInstalled, pkgpkAvailable, pkgpkBoth);
  TpkgFPpkg = class(TComponent)
  private
    FFPMakeRepositoryList: TComponentList;
    FRepositoryList: TComponentList;
    FOptions: TFppkgOptions;
    FCompilerOptions: TCompilerOptions;
    FFpmakeCompilerOptions: TCompilerOptions;
    FCurrentRemoteRepositoryURL: String;
    function IncludeRepositoryTypeForPackageKind(ARepositoryType: TFPRepositoryType;
      APackageKind: TpkgPackageKind): Boolean;
    procedure ScanPackagesOnDisk(ACompilerOptions: TCompilerOptions; APackageKind: TpkgPackageKind; ARepositoryList: TComponentList);
    function  FindPackage(ARepositoryList: TComponentList; APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;

    function  SelectRemoteMirror:string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeGlobalOptions(CfgFile: string);
    procedure InitializeCompilerOptions;
    procedure ScanAvailablePackages;
    procedure ScanPackages;

    function PackageIsBroken(APackage: TFPPackage; ARepository: TFPRepository): Boolean;

    function FPMakeRepoFindPackage(APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;
    function FindPackage(APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;
    function PackageByName(APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;

    function FindRepository(ARepositoryName: string): TFPRepository;
    function RepositoryByName(ARepositoryName: string): TFPRepository;

    function GetInstallRepository(APackage: TFPPackage): TFPRepository;
    function PackageLocalArchive(APackage:TFPPackage): String;
    function PackageBuildPath(APackage:TFPPackage):String;

    function GetRemoteRepositoryURL(const AFileName:string):string;
    function PackageRemoteArchive(APackage:TFPPackage): String;

    procedure ScanInstalledPackagesForAvailablePackages;

    property Options: TFppkgOptions read FOptions;
    property CompilerOptions: TCompilerOptions read FCompilerOptions;
    property FpmakeCompilerOptions: TCompilerOptions read FFpmakeCompilerOptions;
    property FPMakeRepositoryList: TComponentList read FFPMakeRepositoryList;
    property RepositoryList: TComponentList read FRepositoryList;
  public

  end;

implementation

uses
  pkgrepos;

{ TpkgFPpkg }

constructor TpkgFPpkg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := TFppkgOptions.Create;
  FCompilerOptions := TCompilerOptions.Create;
  FFpmakeCompilerOptions := TCompilerOptions.Create;
  FRepositoryList := TComponentList.Create(False);
  FFPMakeRepositoryList := TComponentList.Create(False);
end;

destructor TpkgFPpkg.Destroy;
begin
  FFPMakeRepositoryList.Free;
  FRepositoryList.Free;
  FCompilerOptions.Free;
  FFpmakeCompilerOptions.Free;
  FOptions.Free;
  inherited Destroy;
end;

function TpkgFPpkg.IncludeRepositoryTypeForPackageKind(ARepositoryType: TFPRepositoryType;
  APackageKind: TpkgPackageKind): Boolean;
begin
  Result := ((APackageKind=pkgpkInstalled) and (ARepositoryType = fprtInstalled)) or
    ((APackageKind=pkgpkAvailable) and (ARepositoryType = fprtAvailable)) or
    (APackageKind=pkgpkBoth);
end;

procedure TpkgFPpkg.ScanPackagesOnDisk(ACompilerOptions: TCompilerOptions;
  APackageKind: TpkgPackageKind; ARepositoryList: TComponentList);
var
  i: Integer;
  RepoOption: TFppkgRepositoryOptionSection;
  Repo: TFPRepository;
begin
  FOptions.BindToCompilerOptions(ACompilerOptions);
  for i := 0 to FOptions.SectionList.Count -1 do
    begin
      if FOptions.SectionList[i] is TFppkgRepositoryOptionSection then
        begin
          RepoOption := TFppkgRepositoryOptionSection(FOptions.SectionList[i]);
          if IncludeRepositoryTypeForPackageKind(RepoOption.GetRepositoryType, APackageKind) then
            begin
              Repo := RepoOption.InitRepository(Self, ACompilerOptions);
              if Assigned(Repo) then
                begin
                  ARepositoryList.Add(Repo);
                  Repo.DefaultPackagesStructure.AddPackagesToRepository(Repo);
                end;
            end;
        end;
    end;
end;

procedure TpkgFPpkg.InitializeGlobalOptions(CfgFile: string);
var
  GeneratedConfig: boolean;
  FirstRepoConf: TFppkgOptionSection;
begin
  GeneratedConfig:=false;
  // First try specified config file
  if (CfgFile<>'') then
    begin
      if not FileExists(cfgfile) then
        Error(SErrNoSuchFile,[cfgfile]);
    end
  else
    begin
      // Now try if a local config-file exists
      cfgfile:=GetAppConfigFile(False,False);
      if not FileExists(cfgfile) then
        begin
          // If not, try to find a global configuration file
          cfgfile:=GetAppConfigFile(True,False);
          if not FileExists(cfgfile) then
            begin
              // Create a new configuration file
              if not IsSuperUser then // Make a local, not global, configuration file
                cfgfile:=GetAppConfigFile(False,False);
              ForceDirectories(ExtractFilePath(cfgfile));
              FOptions.SaveToFile(cfgfile);
              GeneratedConfig:=true;
            end;
        end;
    end;
  // Load file or create new default configuration
  if not GeneratedConfig then
    begin
      FOptions.LoadFromFile(cfgfile);
    end;
  FOptions.CommandLineSection.CompilerConfig:=FOptions.GlobalSection.CompilerConfig;
  if FOptions.GlobalSection.InstallRepository <> '' then
    FOptions.CommandLineSection.InstallRepository:=FOptions.GlobalSection.InstallRepository
  else
    begin
      FirstRepoConf :=  FOptions.GetSectionByName('Repository');
      if Assigned(FirstRepoConf) then
        FOptions.CommandLineSection.InstallRepository := (FirstRepoConf as TFppkgRepositoryOptionSection).RepositoryName;
    end;
  // Tracing of what we've done above, need to be done after the verbosity is set
  if GeneratedConfig then
    pkgglobals.Log(llDebug,SLogGeneratingGlobalConfig,[cfgfile])
  else
    pkgglobals.Log(llDebug,SLogLoadingGlobalConfig,[cfgfile]);
  // Log configuration
  FOptions.LogValues(llDebug);
end;

procedure TpkgFPpkg.InitializeCompilerOptions;
var
  S : String;
begin
  // Load default compiler config
  S:=FOptions.GlobalSection.CompilerConfigDir+FOptions.CommandLineSection.CompilerConfig;
  FCompilerOptions.UpdateLocalRepositoryOption(FOptions);
  if FileExists(S) then
    begin
      pkgglobals.Log(llDebug,SLogLoadingCompilerConfig,[S]);
      FCompilerOptions.LoadCompilerFromFile(S)
    end
  else
    begin
      // Generate a default configuration if it doesn't exists
      if FOptions.GlobalSection.CompilerConfig='default' then
        begin
          pkgglobals.Log(llDebug,SLogGeneratingCompilerConfig,[S]);
          FCompilerOptions.InitCompilerDefaults;
          FCompilerOptions.SaveCompilerToFile(S);
          if FCompilerOptions.SaveInifileChanges then
            FCompilerOptions.SaveCompilerToFile(S);
        end
      else
        Error(SErrMissingCompilerConfig,[S]);
    end;
  // Log compiler configuration
  FCompilerOptions.LogValues(llDebug,'');
  // Load FPMake compiler config, this is normally the same config as above
  S:=FOptions.GlobalSection.CompilerConfigDir+FOptions.GlobalSection.FPMakeCompilerConfig;
  FFPMakeCompilerOptions.UpdateLocalRepositoryOption(FOptions);
  if FileExists(S) then
    begin
      pkgglobals.Log(llDebug,SLogLoadingFPMakeCompilerConfig,[S]);
      FFPMakeCompilerOptions.LoadCompilerFromFile(S);
      if FFPMakeCompilerOptions.SaveInifileChanges then
        FFPMakeCompilerOptions.SaveCompilerToFile(S);
    end
  else
    Error(SErrMissingCompilerConfig,[S]);
  // Log compiler configuration
  FFPMakeCompilerOptions.LogValues(llDebug,'fpmake-building');
end;

procedure TpkgFPpkg.ScanAvailablePackages;
var
  Repo: TFPRepository;
  InstPackages: TFPCustomPackagesStructure;
begin
  if (FOptions.GlobalSection.RemoteMirrorsURL<>'') or
    ((FOptions.GlobalSection.RemoteRepository<>'') and (FOptions.GlobalSection.RemoteRepository<>'auto')) then
    begin
      // In case of a re-scan (for example after an update), remove the old list
      Repo := FindRepository('Available');
      if Assigned(Repo) then
        begin
          RepositoryList.Remove(Repo);
          Repo.Free;
        end;

      Repo := TFPRepository.Create(Self);
      FRepositoryList.Add(Repo);
      Repo.RepositoryName := 'Available';
      Repo.Description := 'Packages available for download';
      Repo.RepositoryType := fprtAvailable;
      InstPackages := TFPRemotePackagesStructure.Create(Self, FOptions);
      InstPackages.AddPackagesToRepository(Repo);
      Repo.DefaultPackagesStructure := InstPackages;
    end;
end;

procedure TpkgFPpkg.ScanPackages;
begin
  // There is no need to scan for available packages and add them to the
  // FPMakeRepositoryList. Beside that it could lead to problems
  // when the scan of one of the available-repositories tries to compile an
  // fpmake-executable. (Like TFPUninstalledSourcesAvailablePackagesStructure does)
  ScanPackagesOnDisk(FFpmakeCompilerOptions, pkgpkInstalled, FPMakeRepositoryList);

  CheckFPMakeDependencies;

  ScanPackagesOnDisk(FCompilerOptions, pkgpkBoth, RepositoryList);
  ScanAvailablePackages;
end;

function TpkgFPpkg.PackageIsBroken(APackage: TFPPackage; ARepository: TFPRepository): Boolean;
var
  j, i, ThisRepositoryIndex: Integer;
  Dependency: TFPDependency;
  Repository: TFPRepository;
  DepPackage: TFPPackage;
begin
  result:=false;

  // We should only check for dependencies in this repository, or repositories
  // with a lower priority.
  ThisRepositoryIndex := -1;
  for i := RepositoryList.Count -1 downto 0 do
    begin
      if RepositoryList.Items[i] = ARepository then
        ThisRepositoryIndex := i;
    end;

  for j:=0 to APackage.Dependencies.Count-1 do
    begin
      Dependency:=APackage.Dependencies[j];
      if (CompilerOptions.CompilerOS in Dependency.OSes) and
         (CompilerOptions.CompilerCPU in Dependency.CPUs) then
        begin
          for i := ThisRepositoryIndex downto 0 do
            begin
              Repository := RepositoryList.Items[i] as TFPRepository;
              DepPackage := Repository.FindPackage(Dependency.PackageName);
              if Assigned(DepPackage) then
                Break;
            end;

          if assigned(DepPackage) then
            begin
              if (Dependency.RequireChecksum<>$ffffffff) and (DepPackage.Checksum<>Dependency.RequireChecksum) then
                begin
                  log(llInfo,SLogPackageChecksumChanged,[APackage.Name,ARepository.RepositoryName,Dependency.PackageName,Repository.RepositoryName]);
                  result:=true;
                  exit;
                end;
            end
          else
            begin
              log(llDebug,SDbgObsoleteDependency,[APackage.Name,Dependency.PackageName]);
              result:=true;
              exit;
            end;
        end;
    end;
end;

function TpkgFPpkg.FPMakeRepoFindPackage(APackageName: string;
  APackageKind: TpkgPackageKind): TFPPackage;
begin
  Result := FindPackage(FPMakeRepositoryList, APackageName, APackageKind);
end;

function TpkgFPpkg.FindPackage(APackageName: string;
  APackageKind: TpkgPackageKind): TFPPackage;
begin
  Result := FindPackage(RepositoryList, APackageName, APackageKind);
end;

function TpkgFPpkg.FindPackage(ARepositoryList: TComponentList; APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;
var
  i: Integer;
  Repo: TFPRepository;
begin
  Result := nil;
  for i := ARepositoryList.Count-1 downto 0 do
    begin
      Repo := ARepositoryList.Items[i] as TFPRepository;
      if IncludeRepositoryTypeForPackageKind(Repo.RepositoryType, APackageKind) then
        begin
          Result := repo.FindPackage(APackageName);
          if Assigned(Result) then
            Break;
        end;
    end;
end;

function TpkgFPpkg.SelectRemoteMirror: string;
var
  i,j : Integer;
  Bucket,
  BucketCnt : Integer;
  M : TFPMirror;
begin
  Result:='';
  M:=nil;
  if assigned(AvailableMirrors) then
   begin
     // Create array for selection
     BucketCnt:=0;
     for i:=0 to AvailableMirrors.Count-1 do
       inc(BucketCnt,AvailableMirrors[i].Weight);
     // Select random entry
     Bucket:=Random(BucketCnt);
     M:=nil;
     for i:=0 to AvailableMirrors.Count-1 do
       begin
         for j:=0 to AvailableMirrors[i].Weight-1 do
           begin
             if Bucket=0 then
               begin
                 M:=AvailableMirrors[i];
                 break;
               end;
             Dec(Bucket);
           end;
         if assigned(M) then
           break;
       end;
    end;
  if assigned(M) then
    begin
      log(llInfo,SLogSelectedMirror,[M.Name]);
      Result:=M.URL;
    end
  else
    Error(SErrFailedToSelectMirror);
end;

function TpkgFPpkg.PackageByName(APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;
var
  ErrStr: string;
begin
  Result := FindPackage(APackageName, APackageKind);
  If Result=Nil then
    begin
      case APackageKind of
        pkgpkInstalled : ErrStr:=SErrMissingInstallPackage;
        pkgpkAvailable : ErrStr:=SErrMissingAvailablePackage;
        pkgpkBoth      : ErrStr:=SErrMissingPackage;
      end;
    Raise EPackage.CreateFmt(ErrStr,[APackageName]);
    end;
end;

function TpkgFPpkg.FindRepository(ARepositoryName: string): TFPRepository;
var
  i: Integer;
  Repo: TFPRepository;
begin
  Result := nil;
  for i := FRepositoryList.Count-1 downto 0 do
    begin
      Repo := FRepositoryList.Items[i] as TFPRepository;
      if Repo.RepositoryName = ARepositoryName then
        begin
          Result := Repo;
          Break;
        end;
    end;
end;

function TpkgFPpkg.RepositoryByName(ARepositoryName: string): TFPRepository;
begin
  Result := FindRepository(ARepositoryName);
  If Result=Nil then
    Raise EPackage.CreateFmt(SErrMissingInstallRepo,[ARepositoryName]);
end;

function TpkgFPpkg.GetInstallRepository(APackage: TFPPackage): TFPRepository;
var
  InstRepositoryName: string;
  Repo: TFPRepository;
begin
  Result := GFPpkg.RepositoryByName(GFPpkg.Options.CommandLineSection.InstallRepository);
  if Assigned(APackage) and Assigned(APackage.Repository) and Assigned(APackage.Repository.DefaultPackagesStructure) then
    begin
      InstRepositoryName := APackage.Repository.DefaultPackagesStructure.InstallRepositoryName;
      if (InstRepositoryName<>'') then
        begin
          Repo := FindRepository(InstRepositoryName);
          if Assigned(Repo) then
            Result := Repo;
        end;
    end;
end;

function TpkgFPpkg.PackageLocalArchive(APackage: TFPPackage): String;
begin
  if APackage.Name=CurrentDirPackageName then
    Error(SErrNoPackageSpecified)
  else if APackage.Name=CmdLinePackageName then
    Result:=APackage.LocalFileName
  else
    Result:=Options.GlobalSection.ArchivesDir+APackage.FileName;
end;

procedure TpkgFPpkg.ScanInstalledPackagesForAvailablePackages;
var
  i: Integer;
  Repo, AvailableRepo: TFPRepository;
  AvailStruc: TFPOriginalSourcePackagesStructure;
begin
  for i := FRepositoryList.Count-1 downto 0 do
    begin
      Repo := FRepositoryList.Items[i] as TFPRepository;

      AvailableRepo := TFPRepository.Create(Self);
      FRepositoryList.Add(AvailableRepo);
      AvailableRepo.RepositoryType := fprtAvailable;
      AvailableRepo.RepositoryName := Repo.RepositoryName + '_source';
      AvailableRepo.Description := Repo.Description + ' (original sources)';
      AvailStruc := TFPOriginalSourcePackagesStructure.Create(Self, Repo);
      AvailStruc.AddPackagesToRepository(AvailableRepo);
      AvailableRepo.DefaultPackagesStructure := AvailStruc;
    end;
end;

function TpkgFPpkg.PackageBuildPath(APackage: TFPPackage): String;
begin
  if (APackage.Name=CmdLinePackageName) or (APackage.Name=URLPackageName) then
    Result:=Options.GlobalSection.BuildDir+ChangeFileExt(ExtractFileName(APackage.LocalFileName),'')
  else if Assigned(APackage.PackagesStructure) and (APackage.PackagesStructure.GetBuildPathDirectory(APackage)<>'') then
    Result:=APackage.PackagesStructure.GetBuildPathDirectory(APackage)
  else
    Result:=Options.GlobalSection.BuildDir+APackage.Name;
end;

function TpkgFPpkg.GetRemoteRepositoryURL(const AFileName: string): string;
begin
  if FCurrentRemoteRepositoryURL='' then
    begin
      if Options.GlobalSection.RemoteRepository='auto' then
        FCurrentRemoteRepositoryURL:=SelectRemoteMirror
      else
        FCurrentRemoteRepositoryURL:=Options.GlobalSection.RemoteRepository;
    end;
  result := FCurrentRemoteRepositoryURL;
  if result <> '' then
    begin
      if result[length(result)]<>'/' then
        result := result + '/';
      Result:=Result+CompilerOptions.CompilerVersion+'/'+AFileName;
    end;
end;

function TpkgFPpkg.PackageRemoteArchive(APackage: TFPPackage): String;
begin
  if APackage.Name=CurrentDirPackageName then
    Error(SErrNoPackageSpecified)
  else if APackage.Name=CmdLinePackageName then
    Error(SErrPackageIsLocal);
  if APackage.DownloadURL<>'' then
    Result:=APackage.DownloadURL
  else
    Result:=GetRemoteRepositoryURL(APackage.FileName);
end;

end.

