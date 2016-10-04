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
    procedure ScanPackagesOnDisk(ACompilerOptions: TCompilerOptions; ARepositoryList: TComponentList);
    function  FindPackage(ARepositoryList: TComponentList; APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeGlobalOptions(CfgFile: string);
    procedure InitializeCompilerOptions;
    procedure ScanAvailablePackages;
    procedure ScanPackages;

    function FPMakeRepoFindPackage(APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;
    function FindPackage(APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;
    function PackageByName(APackageName: string; APackageKind: TpkgPackageKind): TFPPackage;

    function FindRepository(ARepositoryName: string): TFPRepository;
    function RepositoryByName(ARepositoryName: string): TFPRepository;

    procedure ScanInstalledPackagesForAvailablePackages;

    property Options: TFppkgOptions read FOptions;
    property CompilerOptions: TCompilerOptions read FCompilerOptions;
    property FpmakeCompilerOptions: TCompilerOptions read FFpmakeCompilerOptions;
    property FPMakeRepositoryList: TComponentList read FFPMakeRepositoryList;
    property RepositoryList: TComponentList read FRepositoryList;
  public

  end;

implementation

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

procedure TpkgFPpkg.ScanPackagesOnDisk(ACompilerOptions: TCompilerOptions;
  ARepositoryList: TComponentList);
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
          Repo := RepoOption.InitRepository(Self, ACompilerOptions);
          if Assigned(Repo) then
            begin
              ARepositoryList.Add(Repo);
              Repo.DefaultPackagesStructure.AddPackagesToRepository(Repo);
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
  ScanPackagesOnDisk(FFpmakeCompilerOptions, FPMakeRepositoryList);
  ScanPackagesOnDisk(FCompilerOptions, RepositoryList);
  ScanAvailablePackages;
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
      if ((APackageKind=pkgpkInstalled) and (Repo.RepositoryType = fprtInstalled)) or
        ((APackageKind=pkgpkAvailable) and (Repo.RepositoryType = fprtAvailable)) or
        (APackageKind=pkgpkBoth) then
        begin
          Result := repo.FindPackage(APackageName);
          if Assigned(Result) then
            Break;
        end;
    end;
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

end.

