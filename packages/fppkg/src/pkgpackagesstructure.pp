unit pkgPackagesStructure;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fprepos,
  fpxmlrep,
  pkgoptions;

type

  { TFPRemotePackagesStructure }

  TFPRemotePackagesStructure = class(TFPCustomPackagesStructure)
  public
    class function GetRepositoryOptionSectionClass: TFppkgRepositoryOptionSectionClass; override;

    function UnzipBeforeUse: Boolean; override;
    function AddPackagesToRepository(ARepository: TFPRepository): Boolean; override;
  end;

  { TFPCustomFileSystemPackagesStructure }

  TFPCustomFileSystemPackagesStructure = class(TFPCustomPackagesStructure)
  private
    FPath: string;
  protected
    function GetPath: string; virtual;
    procedure SetPath(AValue: string); virtual;
  public
    property Path: string read GetPath write SetPath;
  end;

  { TFPInstalledPackagesStructure }

  TFPInstalledPackagesStructure = class(TFPCustomFileSystemPackagesStructure)
  private
    FPrefix: string;
  public
    class function GetRepositoryOptionSectionClass: TFppkgRepositoryOptionSectionClass; override;
    procedure InitializeWithOptions(ARepoOptionSection: TFppkgRepositoryOptionSection; AnOptions: TFppkgOptions; ACompilerOptions: TCompilerOptions); override;
    function AddPackagesToRepository(ARepository: TFPRepository): Boolean; override;
    function GetUnitDirectory(APackage: TFPPackage): string; override;
    function GetPrefix: string; override;
    function GetBaseInstallDir: string; override;
    // The prefix is used on installing packages
    property Prefix: string read FPrefix write FPrefix;
  end;

  { TFPCurrentDirectoryPackagesStructure }

  TFPCurrentDirectoryPackagesStructure = class(TFPCustomFileSystemPackagesStructure)
  protected
    procedure SetPath(AValue: string); override;
  public
    function AddPackagesToRepository(ARepository: TFPRepository): Boolean; override;
    function GetBuildPathDirectory(APackage: TFPPackage): string; override;
  end;

  { TFPOriginalSourcePackagesStructure }

  TFPOriginalSourcePackagesStructure = class(TFPCustomPackagesStructure)
  private
    FOriginalRepository: TFPRepository;
  public
    constructor Create(AOwner: TComponent; OriginalRepository: TFPRepository);
    function AddPackagesToRepository(ARepository: TFPRepository): Boolean; override;
    function GetBuildPathDirectory(APackage: TFPPackage): string; override;
  end;

  { TFPTemporaryDirectoryPackagesStructure }

  TFPTemporaryDirectoryPackagesStructure = class(TFPCustomFileSystemPackagesStructure)
  private
    FPackage: TFPPackage;
    function GetTempPackageName: string;
    procedure SetTempPackageName(AValue: string);
  public
    function AddPackagesToRepository(ARepository: TFPRepository): Boolean; override;
    function GetBuildPathDirectory(APackage: TFPPackage): string; override;
    procedure SetTempPath(APath: string);
    property TempPackageName: string read GetTempPackageName write SetTempPackageName;
  end;

implementation

uses
  fpmkunit,
  pkgmessages,
  pkgrepos,
  pkgglobals;

{ TFPCustomFileSystemPackagesStructure }

function TFPCustomFileSystemPackagesStructure.GetPath: string;
begin
  Result := FPath;
end;

procedure TFPCustomFileSystemPackagesStructure.SetPath(AValue: string);
begin
  FPath := AValue;
end;

{ TFPTemporaryDirectoryPackagesStructure }

function TFPTemporaryDirectoryPackagesStructure.GetTempPackageName: string;
begin
  Result := FPackage.Name;
end;

procedure TFPTemporaryDirectoryPackagesStructure.SetTempPackageName(AValue: string);
begin
  FPackage.Name := AValue;
end;

function TFPTemporaryDirectoryPackagesStructure.AddPackagesToRepository(ARepository: TFPRepository): Boolean;
begin
  Result := True;
  FPackage := ARepository.AddPackage('');
  FPackage.PackagesStructure := Self;
end;

function TFPTemporaryDirectoryPackagesStructure.GetBuildPathDirectory(APackage: TFPPackage): string;
begin
  Result := FPath;
end;

procedure TFPTemporaryDirectoryPackagesStructure.SetTempPath(APath: string);
begin
  FPath := APath;
end;

{ TFPOriginalSourcePackagesStructure }

constructor TFPOriginalSourcePackagesStructure.Create(AOwner: TComponent;
  OriginalRepository: TFPRepository);
begin
  inherited Create(Owner);
  FOriginalRepository := OriginalRepository;
end;

function TFPOriginalSourcePackagesStructure.AddPackagesToRepository(
  ARepository: TFPRepository): Boolean;
var
  i: Integer;
  OrgPackage: TFPPackage;
  P: TFPPackage;
begin
  Result := True;
  for i := 0 to FOriginalRepository.PackageCount -1 do
    begin
      OrgPackage := FOriginalRepository.Packages[i];
      if (OrgPackage.SourcePath<>'') and DirectoryExists(OrgPackage.SourcePath) then
        begin
          P:=ARepository.AddPackage(OrgPackage.Name);
          P.PackagesStructure:=Self;
          P.Assign(OrgPackage);
        end;
    end;
end;

function TFPOriginalSourcePackagesStructure.GetBuildPathDirectory(
  APackage: TFPPackage): string;
begin
  Result:=APackage.SourcePath;
end;

{ TFPCurrentDirectoryPackagesStructure }

procedure TFPCurrentDirectoryPackagesStructure.SetPath(AValue: string);
begin
  if AValue = '' then
    AValue := GetCurrentDir;
  inherited SetPath(AValue);
end;

function TFPCurrentDirectoryPackagesStructure.AddPackagesToRepository(
  ARepository: TFPRepository): Boolean;
var
  Package: TFPPackage;
begin
  Result := True;
  Package := ARepository.AddPackage(CurrentDirPackageName);
  Package.PackagesStructure := Self;
end;

function TFPCurrentDirectoryPackagesStructure.GetBuildPathDirectory(APackage: TFPPackage): string;
begin
  Result := FPath;
end;

{ TFPRemotePackagesStructure }

class function TFPRemotePackagesStructure.GetRepositoryOptionSectionClass: TFppkgRepositoryOptionSectionClass;
begin
  Result := nil;
end;

function TFPRemotePackagesStructure.UnzipBeforeUse: Boolean;
begin
  Result := True;
end;

function TFPRemotePackagesStructure.AddPackagesToRepository(ARepository: TFPRepository): Boolean;
var
  S : String;
  X : TFPXMLRepositoryHandler;
  i: Integer;
begin
  Result := True;
  // Repository
  S:=FOptions.GlobalSection.LocalPackagesFile;
  log(llDebug,SLogLoadingPackagesFile,[S]);
  if not FileExists(S) then
    exit;
  try
    X:=TFPXMLRepositoryHandler.Create;
    With X do
      try
        LoadFromXml(ARepository,S);
      finally
        Free;
      end;
    for i := 0 to ARepository.PackageCount -1 do
      ARepository.Packages[i].PackagesStructure := Self;
  except
    on E : Exception do
      begin
        Log(llError,E.Message);
        Error(SErrCorruptPackagesFile,[S]);
      end;
  end;
end;

{ TFPInstalledPackagesStructure }

class function TFPInstalledPackagesStructure.GetRepositoryOptionSectionClass: TFppkgRepositoryOptionSectionClass;
begin
  Result := TFppkgRepositoryOptionSection;
end;

procedure TFPInstalledPackagesStructure.InitializeWithOptions(
  ARepoOptionSection: TFppkgRepositoryOptionSection; AnOptions: TFppkgOptions;
  ACompilerOptions: TCompilerOptions);
var
  RepoOptSection: TFppkgRepositoryOptionSection;
begin
  inherited InitializeWithOptions(ARepoOptionSection, AnOptions, ACompilerOptions);
  RepoOptSection := ARepoOptionSection as TFppkgRepositoryOptionSection;
  Prefix := RepoOptSection.Prefix;
  InstallRepositoryName := RepoOptSection.InstallRepositoryName;
  Path := RepoOptSection.Path;
end;

function TFPInstalledPackagesStructure.AddPackagesToRepository(ARepository: TFPRepository): Boolean;

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
  SR : TSearchRec;
  P  : TFPPackage;
  UF,UD : String;
  FpmkDir : String;
  UnitDir: String;
begin
  Result:=false;
  FpmkDir:=IncludeTrailingPathDelimiter(FPath)+'fpmkinst'+PathDelim+FCompilerOptions.CompilerTarget+PathDelim;
  if FindFirst(IncludeTrailingPathDelimiter(FpmkDir)+'*'+FpmkExt,faDirectory,SR)=0 then
    begin
      log(llDebug,SLogFindInstalledPackages,[FpmkDir]);
      repeat
        if ((SR.Attr and faDirectory)=0) then
          begin
            // Try new .fpm-file
            UF:=FpmkDir+SR.Name;
            P:=ARepository.AddPackage(ChangeFileExt(SR.Name,''));
            P.LoadUnitConfigFromFile(UF);
            P.PackagesStructure:=Self;
            if P.IsFPMakeAddIn then
              AddFPMakeAddIn(P);
          end;
      until FindNext(SR)<>0;
    end;
  FindClose(SR);

  // Search for non-fpmkunit packages
  UnitDir:=IncludeTrailingPathDelimiter(FPath)+'units'+PathDelim+FCompilerOptions.CompilerTarget+PathDelim;
  if FindFirst(IncludeTrailingPathDelimiter(UnitDir)+AllFiles,faDirectory,SR)=0 then
    begin
      log(llDebug,SLogFindInstalledPackages,[UnitDir]);
      repeat
        if ((SR.Attr and faDirectory)=faDirectory) and (SR.Name<>'.') and (SR.Name<>'..') then
          begin
            UD:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(UnitDir)+SR.Name);
            // Try new fpunits.cfg
            UF:=UD+UnitConfigFileName;
            if FileExists(UF) then
              begin
                if not Assigned(ARepository.FindPackage(SR.Name)) then
                  begin
                    P:=ARepository.AddPackage(SR.Name);
                    P.PackagesStructure:=Self;
                    P.LoadUnitConfigFromFile(UF);
                    if P.IsFPMakeAddIn then
                      AddFPMakeAddIn(P);
                  end;
              end
            else
              begin
                // Try Old style Package.fpc
                UF:=UD+'Package.fpc';
                if FileExists(UF) then
                  begin
                    if not Assigned(ARepository.FindPackage(SR.Name)) then
                      begin
                        P:=ARepository.AddPackage(SR.Name);
                        P.PackagesStructure:=Self;
                        LoadPackagefpcFromFile(P,UF);
                      end;
                  end;
              end;
          end;
      until FindNext(SR)<>0;
    end;
  FindClose(SR);

  Result:=true;
end;

function TFPInstalledPackagesStructure.GetUnitDirectory(APackage: TFPPackage): string;
begin
  Result:=IncludeTrailingPathDelimiter(FPath)+'units'+PathDelim+FCompilerOptions.CompilerTarget+PathDelim+APackage.Name+PathDelim;
end;

function TFPInstalledPackagesStructure.GetPrefix: string;
begin
  Result:=IncludeTrailingPathDelimiter(FPrefix);
end;

function TFPInstalledPackagesStructure.GetBaseInstallDir: string;
begin
  Result:=FPath;
end;

initialization
  TFPCustomPackagesStructure.RegisterPackagesStructureClass(TFPRemotePackagesStructure);
  TFPCustomPackagesStructure.RegisterPackagesStructureClass(TFPInstalledPackagesStructure);
end.

