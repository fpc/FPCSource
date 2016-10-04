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
  private
  public
    constructor Create(AnOptionParser: TTemplateParser); override;
    procedure AddKeyValue(const AKey, AValue: string); override;
    function InitRepository(AParent: TComponent; ACompilerOptions: TCompilerOptions): TFPRepository; override;
  end;

  { TFPUninstalledSourcesAvailablePackagesStructure }

  TFPUninstalledSourcesAvailablePackagesStructure = class(TFPCustomFileSystemPackagesStructure)
  public
    function AddPackagesToRepository(ARepository: TFPRepository): Boolean; override;
    function GetBuildPathDirectory(APackage: TFPPackage): string; override;
  end;

implementation

const
  KeyScanForUnits = 'ScanForUnits';
  KeyUnitPath     = 'UnitPath';

{ TFppkgUninstalledSourceRepositoryOptionSection }

constructor TFppkgUninstalledSourceRepositoryOptionSection.Create(AnOptionParser: TTemplateParser);
begin
  inherited Create(AnOptionParser);
end;

procedure TFppkgUninstalledSourceRepositoryOptionSection.AddKeyValue(const AKey, AValue: string);
begin
  inherited AddKeyValue(AKey, AValue);
end;

function TFppkgUninstalledSourceRepositoryOptionSection.InitRepository(AParent: TComponent;
  ACompilerOptions: TCompilerOptions): TFPRepository;
var
  InstPackages: TFPUninstalledSourcesAvailablePackagesStructure;
begin
  if Path <> '' then
    begin
      Result := TFPRepository.Create(AParent);
      Result.RepositoryType := fprtAvailable;
      Result.RepositoryName := RepositoryName;
      Result.Description := Description;
      InstPackages := TFPUninstalledSourcesAvailablePackagesStructure.Create(AParent, Path, ACompilerOptions);
      Result.DefaultPackagesStructure := InstPackages;
    end;
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
  Result := FPath+APackage.Name;
end;


end.

