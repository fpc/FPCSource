unit pkgUninstalledSourcesRepository;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpmkunit,
  fpTemplate,
  pkgoptions,
  pkgglobals,
  pkgmessages,
  fprepos,
  pkgrepos,
  pkghandler,
  pkgPackagesStructure;

type

  { TFppkgUninstalledSourceRepositoryOptionSection }

  TFppkgUninstalledSourceRepositoryOptionSection = class(TFppkgRepositoryOptionSection)
  public
    constructor Create(AnOptionParser: TTemplateParser); override;
    function InitRepository(AParent: TComponent; ACompilerOptions: TCompilerOptions): TFPRepository; override;

    function GetRepositoryType: TFPRepositoryType; override;
  end;

  { TFPUninstalledSourcesAvailablePackagesStructure }

  TFPUninstalledSourcesAvailablePackagesStructure = class(TFPCustomFileSystemPackagesStructure)
  public
    function AddPackagesToRepository(ARepository: TFPRepository): Boolean; override;
    function GetBuildPathDirectory(APackage: TFPPackage): string; override;
  end;


  { TFppkgUninstalledRepositoryOptionSection }

  TFppkgUninstalledRepositoryOptionSection = class(TFppkgRepositoryOptionSection)
  private
    FSourceRepositoryName: string;
  public
    function InitRepository(AParent: TComponent; ACompilerOptions: TCompilerOptions): TFPRepository; override;

    procedure AddKeyValue(const AKey, AValue: string); override;
    procedure LogValues(ALogLevel: TLogLevel); override;
    function GetRepositoryType: TFPRepositoryType; override;
    property SourceRepositoryName: string read FSourceRepositoryName write FSourceRepositoryName;
  end;

  { TFPUninstalledSourcesPackagesStructure }

  TFPUninstalledSourcesPackagesStructure = class(TFPCustomFileSystemPackagesStructure)
  private
    FSourceRepositoryName: string;
  public
    function AddPackagesToRepository(ARepository: TFPRepository): Boolean; override;
    function IsInstallationNeeded(APackage: TFPPackage): TFPInstallationNeeded; override;
    function GetBaseInstallDir: string; override;
    function GetConfigFileForPackage(APackage: TFPPackage): string; override;
    property SourceRepositoryName: string read FSourceRepositoryName write FSourceRepositoryName;
  end;


implementation

const
  KeyScanForUnits      = 'ScanForUnits';
  KeyUnitPath          = 'UnitPath';
  KeySourceRepository  = 'SourceRepository';

  SLogSourceRepository = '  SourceRepository:%s';

{ TFPUninstalledSourcesPackagesStructure }

function TFPUninstalledSourcesPackagesStructure.AddPackagesToRepository(ARepository: TFPRepository): Boolean;

  procedure LoadPackagefpcFromFile(APackage:TFPPackage;const AFileName: String);
  Var
    L : TStrings;
    V : String;
  begin
    L:=TStringList.Create;
    Try
      ReadIniFile(AFileName,L);
      V:=L.Values['version'];
      APackage.Version.AsString:=V;
    Finally
      L.Free;
    end;
  end;

var
  SRD : TSearchRec;
  SRF : TSearchRec;
  P  : TFPPackage;
  UF,UD : String;
begin
  Result:=false;
  log(llDebug,SLogFindInstalledPackages,[FPath]);
  if FindFirst(FPath+AllFiles,faDirectory,SRD)=0 then
    begin
      repeat
          // Try new .fpm-file
          UD:=FPath+SRD.Name+PathDelim;

          if FindFirst(UD+'*'+FpmkExt,faAnyFile,SRF)=0 then
            begin
              repeat
                UF := UD+SRF.Name;
                P:=ARepository.AddPackage(ChangeFileExt(SRF.Name,''));
                P.LoadUnitConfigFromFile(UF);
                P.PackagesStructure:=Self;
                if P.IsFPMakeAddIn then
                  AddFPMakeAddIn(P);
              until FindNext(SRF)<>0;
            end;
          FindClose(SRF);
      until FindNext(SRD)<>0;
    end;
  FindClose(SRF);

  Result:=true;
end;

function TFPUninstalledSourcesPackagesStructure.IsInstallationNeeded(APackage: TFPPackage): TFPInstallationNeeded;
begin
  if APackage.Repository.RepositoryName=SourceRepositoryName then
    Result := fpinNoInstallationNeeded
  else
    Result := fpinInstallationImpossible;
end;

function TFPUninstalledSourcesPackagesStructure.GetBaseInstallDir: string;
begin
  Result := FPath;
end;

function TFPUninstalledSourcesPackagesStructure.GetConfigFileForPackage(APackage: TFPPackage): string;
begin
  if APackage.SourcePath<>'' then
    Result := IncludeTrailingPathDelimiter(APackage.SourcePath)
  else
    Result := IncludeTrailingPathDelimiter(GetBaseInstallDir)+APackage.Name+PathDelim;

  Result := Result +APackage.Name+'-'+GFPpkg.CompilerOptions.CompilerTarget+FpmkExt;
end;

{ TFppkgUninstalledRepositoryOptionSection }

function TFppkgUninstalledRepositoryOptionSection.InitRepository(AParent: TComponent;
  ACompilerOptions: TCompilerOptions): TFPRepository;
var
  InstPackages: TFPUninstalledSourcesPackagesStructure;
begin
  if Path <> '' then
    begin
      Result := TFPRepository.Create(AParent);
      Result.RepositoryType := GetRepositoryType;
      Result.RepositoryName := RepositoryName;
      Result.Description := Description;
      InstPackages := TFPUninstalledSourcesPackagesStructure.Create(AParent, Path, ACompilerOptions);
      InstPackages.InstallRepositoryName := InstallRepositoryName;
      InstPackages.SourceRepositoryName := SourceRepositoryName;
      Result.DefaultPackagesStructure := InstPackages;
    end;
end;

procedure TFppkgUninstalledRepositoryOptionSection.AddKeyValue(const AKey, AValue: string);
begin
   if SameText(AKey,KeySourceRepository) then
    SourceRepositoryName := AValue
  else
    inherited AddKeyValue(AKey, AValue);
end;

procedure TFppkgUninstalledRepositoryOptionSection.LogValues(ALogLevel: TLogLevel);
begin
  inherited LogValues(ALogLevel);
  log(ALogLevel,SLogSourceRepository,[FSourceRepositoryName]);
end;

function TFppkgUninstalledRepositoryOptionSection.GetRepositoryType: TFPRepositoryType;
begin
  Result := fprtInstalled;
end;

{ TFppkgUninstalledSourceRepositoryOptionSection }

constructor TFppkgUninstalledSourceRepositoryOptionSection.Create(AnOptionParser: TTemplateParser);
begin
  inherited Create(AnOptionParser);
end;

function TFppkgUninstalledSourceRepositoryOptionSection.InitRepository(AParent: TComponent;
  ACompilerOptions: TCompilerOptions): TFPRepository;
var
  InstPackages: TFPUninstalledSourcesAvailablePackagesStructure;
begin
  if Path <> '' then
    begin
      Result := TFPRepository.Create(AParent);
      Result.RepositoryType := GetRepositoryType;
      Result.RepositoryName := RepositoryName;
      Result.Description := Description;
      InstPackages := TFPUninstalledSourcesAvailablePackagesStructure.Create(AParent, Path, ACompilerOptions);
      InstPackages.InstallRepositoryName := InstallRepositoryName;
      Result.DefaultPackagesStructure := InstPackages;
    end;
end;

function TFppkgUninstalledSourceRepositoryOptionSection.GetRepositoryType: TFPRepositoryType;
begin
  Result := fprtAvailable;
end;

{ TFPUninstalledSourcesPackagesStructure }

function TFPUninstalledSourcesAvailablePackagesStructure.AddPackagesToRepository(ARepository: TFPRepository): Boolean;

var
  SR : TSearchRec;
  AManifestFile, AFPMakeFile: String;
  i: Integer;
  TempPackagesStructure: TFPTemporaryDirectoryPackagesStructure;
  TempRepo: TFPRepository;
  PackageName: string;
begin
  Result:=false;

  TempPackagesStructure := TFPTemporaryDirectoryPackagesStructure.Create(Owner, '', FCompilerOptions);
  TempRepo := TFPRepository.Create(Owner);
  TempRepo.RepositoryName := 'TempScanUninstPackages';
  TempRepo.Description := 'Temp list of packages during scanning of source-packages';
  TempRepo.RepositoryType := fprtAvailable;
  TempRepo.DefaultPackagesStructure := TempPackagesStructure;
  TempPackagesStructure.AddPackagesToRepository(TempRepo);
  GFPpkg.RepositoryList.Add(TempRepo);

  try
    log(llDebug,SLogFindInstalledPackages,[FPath]);
    if FindFirst(FPath+AllFiles,faDirectory,SR)=0 then
      begin
        repeat
          if ((SR.Attr and faDirectory)=faDirectory) and (SR.Name<>'.') and (SR.Name<>'..') then
            begin
              AFPMakeFile := FPath+SR.Name+PathDelim+FPMakePPFile;
              if FileExistsLog(AFPMakeFile) then
                begin
                  AManifestFile := FPath+SR.Name+PathDelim+ManifestFile;
                  if not FileExists(AManifestFile) or (FileAge(AManifestFile) < FileAge(AFPMakeFile)) then
                    begin
                      // (Re-)create manifest
                      try
                        TempPackagesStructure.SetTempPath(FPath+SR.Name);
                        PackageName :=  SR.Name + '_create_manifest';
                        TempPackagesStructure.TempPackageName := PackageName;
                        pkghandler.ExecuteAction(PackageName,'fpmakemanifest');
                      except
                        on E: Exception do
                          begin
                            log(llWarning, SLogFailedToCreateManifest ,[AFPMakeFile, E.Message]);
                            Continue;
                          end;
                      end;
                    end;
                  ARepository.AddPackagesFromManifestFile(AManifestFile);
                  for i := 0 to ARepository.PackageCount -1 do
                    if ARepository.Packages[i].SourcePath = '' then
                      ARepository.Packages[i].SourcePath := SR.Name;
                end
            end;
        until FindNext(SR)<>0;
      end;
    FindClose(SR);
  finally
    GFPpkg.RepositoryList.Remove(TempRepo);
    TempRepo.Free;
    TempPackagesStructure.Free;
  end;
  for i := 0 to ARepository.PackageCount -1 do
    ARepository.Packages[i].PackagesStructure := Self;

  Result:=true;
end;

function TFPUninstalledSourcesAvailablePackagesStructure.GetBuildPathDirectory(APackage: TFPPackage): string;
begin
  if APackage.SourcePath<>'' then
    Result := FPath+APackage.SourcePath
  else
    Result := FPath+APackage.Name
end;

end.

