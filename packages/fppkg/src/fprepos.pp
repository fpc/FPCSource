{
    This file is part of the Free Pascal Utilities
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
unit fprepos;

interface

uses
  classes,sysutils,
  contnrs,
  fpmkunit,
  streamcoll;

Const
  StreamVersion   : Integer = 1;
  StreamSignature = $FEEF;


type

  { TFPDependency }

  TFPDependency = Class(TStreamCollectionItem)
  private
    FOSes : TOSES;
    FCPUs : TCPUS;
    FMinVersion: TFPVersion;
    FPackageName: String;
    FRequireChecksum : cardinal;
    procedure SetMinVersion(const AValue: TFPVersion);
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    Procedure LoadFromStream(Stream : TStream; Streamversion : Integer); override;
    Procedure SaveToStream(Stream : TStream); override;
    Procedure Assign(Source : TPersistent); override;
    Property OSes : TOSes Read FOSes Write FOses;
    Property CPUs : TCPUs Read FCPUs Write FCPUs;
  Published
    Property PackageName : String Read FPackageName Write FPackageName;
    Property MinVersion : TFPVersion Read FMinVersion Write SetMinVersion;
    Property RequireChecksum : Cardinal Read FRequireChecksum Write FRequireChecksum;
  end;

  { TFPDepencencies }

  TFPDependencies = Class(TStreamCollection)
  private
    function GetDependency(Index : Integer): TFPDependency;
    procedure SetDependency(Index : Integer; const AValue: TFPDependency);
  public
    Function AddDependency(const APackageName : String; const AMinVersion : String = '') : TFPDependency;
    Property Dependencies[Index : Integer] : TFPDependency Read GetDependency Write SetDependency;default;
  end;

  { TFPPackage }

  TFPPackage = Class(TStreamCollectionItem)
  private
    FAuthor: String;
    FCategory: String;
    FDescription: String;
    FEmail: String;
    FFPMakeOptionsString: string;
    FKeywords: String;
    FRecompileBroken: boolean;
    FSourcePath: string;
    FInstalledLocally: boolean;
    FIsFPMakeAddIn: boolean;
    FLicense: String;
    FName: String;
    FHomepageURL: String;
    FDownloadURL: String;
    FFileName: String;
    FSupport: String;
    FUnusedVersion: TFPVersion;
    FVersion: TFPVersion;
    FDependencies : TFPDependencies;
    FOSes : TOSES;
    FCPUs : TCPUS;
    // Installation info
    FChecksum : cardinal;
    FLocalFileName : String;
    function GetFileName: String;
    procedure SetName(const AValue: String);
    procedure SetUnusedVersion(const AValue: TFPVersion);
    procedure SetVersion(const AValue: TFPVersion);
  protected
    procedure LoadUnitConfigFromStringlist(Const AStringList: TStrings); virtual;
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    Procedure LoadFromStream(Stream : TStream; Streamversion : Integer); override;
    Procedure SaveToStream(Stream : TStream); override;
    procedure LoadUnitConfigFromFile(Const AFileName: String);
    Procedure Assign(Source : TPersistent); override;
    Function AddDependency(Const APackageName : String; const AMinVersion : String = '') : TFPDependency;
    Property Dependencies : TFPDependencies Read FDependencies;
    // Only for installed packages: (is false for packages which are installed globally)
    Property InstalledLocally : boolean read FInstalledLocally write FInstalledLocally;
    Property UnusedVersion : TFPVersion Read FUnusedVersion Write SetUnusedVersion;
    Property RecompileBroken : boolean read FRecompileBroken write FRecompileBroken;
    Property OSes : TOSes Read FOSes Write FOses;
    Property CPUs : TCPUs Read FCPUs Write FCPUs;
  Published
    Property Name : String Read FName Write SetName;
    Property Author : String Read FAuthor Write FAuthor;
    Property Version : TFPVersion Read FVersion Write SetVersion;
    Property License : String Read FLicense Write FLicense;
    Property Description : String Read FDescription Write FDescription;
    Property Support : String Read FSupport Write FSupport;
    Property Keywords : String Read FKeywords Write FKeywords;
    Property Category : String Read FCategory Write FCategory;
    Property HomepageURL : String Read FHomepageURL Write FHomepageURL;
    Property DownloadURL : String Read FDownloadURL Write FDownloadURL;
    Property FileName : String Read GetFileName Write FFileName;
    Property Email : String Read FEmail Write FEmail;
    Property Checksum : Cardinal Read FChecksum Write FChecksum;
    Property IsFPMakeAddIn : boolean read FIsFPMakeAddIn write FIsFPMakeAddIn;
    // These properties are used to re-compile the package, when it's dependencies are changed.
    Property SourcePath : string read FSourcePath write FSourcePath;
    Property FPMakeOptionsString : string read FFPMakeOptionsString write FFPMakeOptionsString;
    // Manual package from commandline not in official repository
    Property LocalFileName : String Read FLocalFileName Write FLocalFileName;
  end;

  { TFPPackages }

  TFPPackages = Class(TStreamCollection)
  private
    FVersion : Integer;
    function GetPackage(Index : Integer): TFPPackage;
    procedure SetPackage(Index : Integer; const AValue: TFPPackage);
  Protected
    Function CurrentStreamVersion : Integer; override;
  Public
    Function IndexOfPackage(const APackageName : String) : Integer;
    Function FindPackage(const APackageName : String) : TFPPackage;
    Function PackageByName(const APackageName : String) : TFPPackage;
    Function AddPackage(const APackageName : string) : TFPPackage;
    Property StreamVersion : Integer Read FVersion Write FVersion;
    Property Packages [Index : Integer] : TFPPackage Read GetPackage Write SetPackage; default;
  end;
  TFPPackagesClass = class of TFPPackages;

  { TFPRepository }

  TFPRepository = Class(TComponent)
  Private
    FMaxDependencyLevel : Integer;
    FBackUpFiles: Boolean;
    FFileName: String;
    function GetPackage(Index : Integer): TFPPackage;
    function GetPackageCount: Integer;
  Protected
    FPackages : TFPPackages;
    procedure CreatePackages; virtual;
    Procedure BackupFile(const AFileName : String); virtual;
    Procedure DoGetPackageDependencies(const APackageName : String; List : TStringList; Level : Integer); virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    // Loading and Saving repository. Own format.
    Procedure LoadFromStream(Stream : TStream); Virtual;
    Procedure SaveToStream(Stream : TStream); Virtual;
    Procedure LoadFromFile(const AFileName : String);
    Procedure SaveToFile(const AFileName : String);
    Procedure Save;
    // Package management
    Function IndexOfPackage(const APackageName : String) : Integer;
    Function FindPackage(const APackageName : String) : TFPPackage;
    Function PackageByName(const APackageName : String) : TFPPackage;
    Procedure DeletePackage(Index : Integer);
    Procedure RemovePackage(const APackageName : string);
    Function AddPackage(const APackageName : string) : TFPPackage;
    // Dependencies
    Procedure GetPackageDependencies(const APackageName : String; List : TObjectList; Recurse : Boolean);
    // Properties
    Property FileName : String Read FFileName;
    Property Packages[Index : Integer] : TFPPackage Read GetPackage; default;
    Property PackageCount : Integer Read GetPackageCount;
    Property BackupFiles : Boolean Read FBackUpFiles Write FBackupFiles;
    Property MaxDependencyLevel : Integer Read FMaxDependencyLevel Write FMaxDependencyLevel;
    Property PackageCollection : TFPPackages Read FPackages;
  end;
  TFPRepositoryClass = class of TFPRepository;


  { TFPMirror }

  TFPMirror = Class(TStreamCollectionItem)
  private
    FContact: String;
    FName: String;
    FURL: String;
    FWeight: Integer;
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    Procedure LoadFromStream(Stream : TStream; Streamversion : Integer); override;
    Procedure SaveToStream(Stream : TStream); override;
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Name : String Read FName Write FName;
    Property URL : String Read FURL Write FURL;
    Property Contact : String Read FContact Write FContact;
    Property Weight : Integer Read FWeight Write FWeight;
  end;

  { TFPMirrors }

  TFPMirrors = Class(TStreamCollection)
  private
    FVersion : Integer;
    function GetMirror(Index : Integer): TFPMirror;
    procedure SetMirror(Index : Integer; const AValue: TFPMirror);
  Protected
    Function CurrentStreamVersion : Integer; override;
  Public
    Function IndexOfMirror(const AMirrorName : String) : Integer;
    Function FindMirror(const AMirrorName : String) : TFPMirror;
    Function MirrorByName(const AMirrorName : String) : TFPMirror;
    Function AddMirror(const AMirrorName : string) : TFPMirror;
    Property StreamVersion : Integer Read FVersion Write FVersion;
    Property Mirrors [Index : Integer] : TFPMirror Read GetMirror Write SetMirror; default;
  end;

  EPackage = Class(Exception);
  EMirror = Class(Exception);

Const
  // Max level of dependency searching before we decide it's a circular dependency.
  DefaultMaxDependencyLevel = 15;

Function MakeTargetString(CPU : TCPU;OS: TOS) : String;
Procedure StringToCPUOS(S : String; Var CPU : TCPU; Var OS: TOS);


Implementation

uses
  typinfo,
  pkgglobals,
  uriparser;

const
  // Keys for unit config
  KeyName     = 'Name';
  KeyVersion  = 'Version';
  KeyChecksum = 'Checksum';
  KeyNeedLibC = 'NeedLibC';
  KeyDepends  = 'Depends';
  KeyAddIn    = 'FPMakeAddIn';
  KeySourcePath = 'SourcePath';
  KeyFPMakeOptions = 'FPMakeOptions';
  KeyCPU      = 'CPU';
  KeyOS       = 'OS';

ResourceString
  SErrInvalidCPU           = 'Invalid CPU name : "%s"';
  SErrInvalidOS            = 'Invalid OS name : "%s"';
  SErrInvalidMode          = 'Invalid compiler mode : "%s"';
  SErrInvalidTarget        = 'Invalid compiler target: %s';
  SErrPackageNotFound      = 'Package "%s" not found.';
  SErrInvalidRepositorySig = 'Invalid repository stream. Stream signature incorrect';
  SErrBackupFailed         = 'Failed to back up file "%s" to "%s".';
  SErrNoFileName           = 'No filename for repository specified.';
  SErrDuplicatePackageName = 'Duplicate package name : "%s"';
  SErrMaxLevelExceeded     = 'Maximum number of dependency levels exceeded (%d) at package "%s".';
  SErrMirrorNotFound       = 'Mirror "%s" not found.';


Function MakeTargetString(CPU : TCPU;OS: TOS) : String;
begin
  Result:=CPUToString(CPU)+'-'+OSToString(OS);
end;


Procedure StringToCPUOS(S : String; Var CPU : TCPU; Var OS: TOS);
Var
  P : integer;
begin
  P:=Pos('-',S);
  If (P=0) then
    Raise EPackage.CreateFmt(SErrInvalidTarget,[S]);
  CPU:=StringToCPU(Copy(S,1,P-1));
  OS:=StringToOs(Copy(S,P+1,Length(S)-P));
end;


{ TFPPackage }

procedure TFPPackage.SetVersion(const AValue: TFPVersion);
begin
  if FVersion=AValue then
    exit;
  FVersion.Assign(AValue);
end;


constructor TFPPackage.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVersion:=TFPVersion.Create;
  FUnusedVersion:=TFPVersion.Create;
  FChecksum:=$ffffffff;
  FOSes:=AllOSes;
  FCPUs:=AllCPUs;
  FDependencies:=TFPDependencies.Create(TFPDependency);
end;


destructor TFPPackage.Destroy;
begin
  FreeAndNil(FDependencies);
  FreeAndNil(FVersion);
  FreeAndNil(FUnusedVersion);
  inherited Destroy;
end;


procedure TFPPackage.SetName(const AValue: String);
begin
  If (AValue<>FName) and (AValue<>'') then
    If (Collection<>Nil) and (Collection is TFPPackages) then
      // do not check while loading, this would slow down a lot !!
      if (not TFPPackages(Collection).Streaming) then
        If TFPPackages(Collection).IndexOfPackage(AValue)<>-1 then
          Raise EPackage.CreateFmt(SErrDuplicatePackageName,[AValue]);
  FName:=AValue;
end;


procedure TFPPackage.SetUnusedVersion(const AValue: TFPVersion);
begin
  if FUnusedVersion=AValue then
    exit;
  FUnusedVersion.Assign(AValue);
end;


function TFPPackage.GetFileName: String;
var
  URI : TURI;
begin
  if FFileName='' then
    begin
      URI:=ParseURI(DownloadURL);
      Result:=URI.Document;
    end
  else
    Result:=FFileName;
end;


procedure TFPPackage.LoadFromStream(Stream: TStream; Streamversion : Integer);
Var
  B : Boolean;
  O : TOSes;
  C : TCPUs;
  I,J,Count : Integer;
begin
  Version.AsString:=ReadString(Stream);
  Name:=ReadString(Stream);
  Author:=ReadString(Stream);
  License:=ReadString(Stream);
  Description:=ReadString(Stream);
  HomepageURL:=ReadString(Stream);
  DownloadURL:=ReadString(Stream);
  FileName:=ReadString(Stream);
  Email:=ReadString(Stream);
  Count:=ReadInteger(Stream);
  O:=[];
  For I:=1 to Count do
    begin
      J:=GetEnumValue(TypeInfo(TOS),ReadString(Stream));
      If (J<>-1) then
        Include(O,TOS(J));
    end;
  OSEs:=O;
  Count:=ReadInteger(Stream);
  C:=[];
  For I:=1 to Count do
    begin
      J:=GetEnumValue(TypeInfo(TCPU),ReadString(Stream));
      If (J<>-1) then
        Include(C,TCPU(J));
    end;
  CPUS:=C;
  FDependencies.Clear;
  B:=ReadBoolean(Stream);
  If B then
    FDependencies.LoadFromStream(Stream);
end;


procedure TFPPackage.SaveToStream(Stream: TStream);
Var
  Count : Integer;
  O : TOS;
  C : TCPU;
begin
  WriteString(Stream,Version.AsString);
  WriteString(Stream,Name);
  WriteString(Stream,Author);
  WriteString(Stream,License);
  WriteString(Stream,Description);
  WriteString(Stream,HomepageURL);
  WriteString(Stream,DownloadURL);
  WriteString(Stream,FileName);
  WriteString(Stream,Email);
  { Write it like this, makes error checking easier when reading. }
  // OSes
  Count:=0;
  For O:=Low(TOS) to High(TOS) do
    If O in OSes then
      Inc(Count);
  WriteInteger(Stream,Count);
  For O:=Low(TOS) to High(TOS) do
    If O in OSes then
      WriteString(Stream,GetEnumName(TypeInfo(TOS),Ord(O)));
  // CPUs
  Count:=0;
  For C:=Low(TCPU) to High(TCPU) do
    If C in CPUS then
      Inc(Count);
  WriteInteger(Stream,Count);
  For C:=Low(TCPU) to High(TCPU) do
    If C in CPUS then
      WriteString(Stream,GetEnumName(TypeInfo(TCPU),Ord(C)));
  WriteBoolean(Stream,FDependencies.Count>0);
  If FDependencies.Count>0 then
    FDependencies.SaveToStream(Stream);
end;

procedure TFPPackage.LoadUnitConfigFromStringlist(const AStringList: TStrings);
var
  L2 : TStrings;
  VOS : TOS;
  VCPU : TCPU;
  i,k : Integer;
  DepChecksum : Cardinal;
  DepName : String;
  D : TFPDependency;
begin
  With AStringList do
    begin
      Version.AsString:=Values[KeyVersion];
      SourcePath:=Values[KeySourcePath];
      FPMakeOptionsString:=Values[KeyFPMakeOptions];
      Checksum:=Cardinal(StrToInt64Def(Values[KeyChecksum],$ffffffff));
      VCPU:=StringToCPU(Values[KeyCPU]);
      VOS:=StringToOS(Values[KeyOS]);
      OSes:=[VOS];
      CPUs:=[VCPU];
      L2:=TStringList.Create;
      L2.CommaText:=Values[KeyDepends];
      for i:=0 to L2.Count-1 do
        begin
          DepName:=L2[i];
          k:=Pos('|',DepName);
          if k>0 then
            begin
              DepChecksum:=StrToInt(Copy(DepName,k+1,Length(DepName)-k));
              DepName:=Copy(DepName,1,k-1);
            end
          else
            DepChecksum:=$ffffffff;
          D:=nil;
          for k:=0 to Dependencies.Count-1 do
            begin
              D:=Dependencies[k];
              if D.PackageName=DepName then
                break;
              D:=nil;
            end;
          if not assigned(D) then
            D:=AddDependency(DepName,'');
          D.RequireChecksum:=DepChecksum;
        end;
      FreeAndNil(L2);
      //NeedLibC:=Upcase(Values[KeyNeedLibC])='Y';
      IsFPMakeAddIn:=Upcase(Values[KeyAddIn])='Y';
    end;
end;

procedure TFPPackage.LoadUnitConfigFromFile(const AFileName: String);
var
  L : TStrings;
begin
  L:=TStringList.Create;
  Try
    ReadIniFile(AFileName,L);
    LoadUnitConfigFromStringlist(L);
  Finally
    L.Free;
  end;
end;


procedure TFPPackage.Assign(Source: TPersistent);
Var
  P : TFPPackage;
begin
  if Source is TFPPackage then
    begin
      P:=Source as TFPPackage;
      // This creates trouble if P has the same owning collection !!
      If P.Collection<>Collection then
        Name:=P.Name;
      Author:=P.Author;
      Version:=P.Version;
      Description:=P.Description;
      HomepageURL:=P.HomepageURL;
      DownloadURL:=P.DownloadURL;
      SourcePath:=P.SourcePath;
      FPMakeOptionsString:=P.FPMakeOptionsString;
      InstalledLocally:=P.InstalledLocally;
      OSes:=P.OSes;
      CPUs:=P.CPUs;
      FileName:=P.FileName;
      Checksum:=P.Checksum;
      Dependencies.Clear;
      Dependencies.Assign(P.Dependencies);
    end
  else
    inherited Assign(Source);
end;


function TFPPackage.AddDependency(Const APackageName : String; const AMinVersion : String = ''): TFPDependency;
begin
  Result:=Dependencies.AddDependency(APackageName,AMinVersion);
end;


{ TFPPackages }

function TFPPackages.GetPackage(Index : Integer): TFPPackage;
begin
  Result:=TFPPackage(Items[Index])
end;

procedure TFPPackages.SetPackage(Index : Integer; const AValue: TFPPackage);
begin
   Items[Index]:=AValue;
end;

function TFPPackages.CurrentStreamVersion: Integer;
begin
  Result:=FVersion;
end;

function TFPPackages.IndexOfPackage(const APackageName: String): Integer;


begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetPackage(Result).Name,APackageName)<>0) do
    Dec(Result);
end;

function TFPPackages.FindPackage(const APackageName: String): TFPPackage;

Var
  I : Integer;

begin
  I:=IndexOfPackage(APackageName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetPackage(I);
end;

function TFPPackages.PackageByName(const APackageName: String): TFPPackage;
begin
  Result:=FindPackage(APackageName);
  If Result=Nil then
    Raise EPackage.CreateFmt(SErrPackageNotFound,[APackageName]);
end;

function TFPPackages.AddPackage(const APackageName: string): TFPPackage;

begin
  Result:=Add as TFPPackage;
  Try
    Result.Name:=APackageName;
  Except
    Result.Free;
    Raise;
  end;
end;


{ TFPRepository }

function TFPRepository.GetPackage(Index : Integer): TFPPackage;

begin
  Result:=FPackages[Index];
end;

function TFPRepository.GetPackageCount: Integer;
begin
  Result:=FPackages.Count;
end;

constructor TFPRepository.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CreatePackages;
  FMaxDependencyLevel:=DefaultMaxDependencyLevel;
end;

procedure TFPRepository.CreatePackages;

begin
  FPackages:=TFPPackages.Create(TFPPackage);
  FPackages.StreamVersion:=StreamVersion;
end;

procedure TFPRepository.BackupFile(const AFileName: String);

Var
  S : String;

begin
  S:=AFileName+'.bak';
  if not RenameFile(AFileName,S) then
    Raise EPackage.CreateFmt(SErrBackupFailed,[AFileName,S]);
end;


destructor TFPRepository.Destroy;
begin
  FreeAndNil(FPackages);
  inherited Destroy;
end;

procedure TFPRepository.LoadFromStream(Stream: TStream);

Var
  I : Integer;
  V : Integer;


begin
  Stream.ReadBuffer(I,SizeOf(Integer));
  If (I<>StreamSignature) then
    Raise EPackage.Create(SErrInvalidRepositorySig);
  Stream.ReadBuffer(V,SizeOf(V));
  FPackages.LoadFromStream(Stream);
end;

procedure TFPRepository.SaveToStream(Stream: TStream);

Var
  i : Integer;

begin
  I:=StreamSignature;
  Stream.WriteBuffer(I,SizeOf(Integer));
  I:=StreamVersion;
  Stream.WriteBuffer(I,SizeOf(Integer));
  FPackages.SaveToStream(Stream);
end;

procedure TFPRepository.LoadFromFile(const AFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmopenRead);
  Try
    LoadFromStream(F);
    FFileName:=AFileName;
  Finally
    F.Free;
  end;
end;

procedure TFPRepository.SaveToFile(const AFileName: String);

Var
  F : TFileStream;

begin
  If FileExists(AFileName) and BackupFiles then
    BackupFile(AFileName);
  F:=TFileStream.Create(AFileName,fmCreate);
  Try
    SaveToStream(F);
    FFileName:=AFileName;
  Finally
    F.Free;
  end;
end;

procedure TFPRepository.Save;
begin
  If (FFileName='') then
     Raise EPackage.Create(SErrNoFileName);
  SaveToFile(FFileName);
end;


function TFPRepository.IndexOfPackage(const APackageName: String): Integer;
begin
  Result:=FPackages.IndexOfPackage(APackageName);
end;


function TFPRepository.FindPackage(const APackageName: String): TFPPackage;
begin
  Result:=FPackages.FindPackage(APackageName);
end;


function TFPRepository.PackageByName(const APackageName: String): TFPPackage;
begin
  Result:=FPackages.PackageByName(APackageName);
end;


procedure TFPRepository.RemovePackage(const APackageName: string);
begin
  PackageByName(APackageName).Free;
end;


procedure TFPRepository.DeletePackage(Index : Integer);
begin
  GetPackage(Index).Free;
end;


function TFPRepository.AddPackage(const APackageName: string): TFPPackage;
begin
  Result:=FPackages.AddPackage(APackageName);
end;


procedure TFPRepository.DoGetPackageDependencies(const APackageName: String; List: TStringList; Level: Integer);
Var
  P : TFPPackage;
  D2,D1 : TFPDependency;
  i,J : Integer;
begin
  // If too many levels, bail out
  If (Level>FMaxDependencyLevel) then
    Raise EPackage.CreateFmt(SErrMaxLevelExceeded,[Level,APackageName]);
  // Check if it is a known package.
  P:=FindPackage(APackageName);
  If not Assigned(P) then
    exit;
  For I:=0 to P.Dependencies.Count-1 do
    begin
      D1:=P.Dependencies[i];
      J:=List.IndexOf(APackageName);
      If J=-1 then
        begin
          // Dependency not yet in list.
          D2:=TFPDependency.Create(Nil);
          D2.Assign(D1);
          List.AddObject(D2.PackageName,D2);
        end
      else
        begin
          // Dependency already in list, compare versions.
          D2:=List.Objects[J] as TFPDependency;
          If D1.MinVersion.CompareVersion(D2.MinVersion)>0 then
            D2.MinVersion.Assign(D1.MinVersion);
        end;
      // If it was already in the list, we no longer recurse.
      If (Level>=0) and (J=-1) Then
        DoGetPackageDependencies(D2.PackageName,List,Level+1);
    end;
end;


procedure TFPRepository.GetPackageDependencies(const APackageName: String; List: TObjectList; Recurse: Boolean);
Var
  L : TStringList;
  I : Integer;
begin
  L:=TStringList.Create;
  Try
    L.Sorted:=True;
    DoGetPackageDependencies(APackageName,L,Ord(Recurse)-1);
    For I:=0 to L.Count-1 do
      List.Add(L.Objects[i]);
  Finally
    // Freeing a stringlist does not free the objects.
    L.Free;
  end;
end;


{ TFPDependency }

procedure TFPDependency.SetMinVersion(const AValue: TFPVersion);
begin
  FMinVersion.Assign(AValue);
end;


constructor TFPDependency.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FMinVersion:=TFPVersion.Create;
  FOSes:=AllOSes;
  FCPUs:=AllCPUs;
  FRequireChecksum:=$ffffffff;
end;


destructor TFPDependency.Destroy;
begin
  FreeAndNil(FMinVersion);
  inherited Destroy;
end;


procedure TFPDependency.LoadFromStream(Stream: TStream; Streamversion: Integer);
begin
  PackageName:=ReadString(Stream);
  MinVersion.AsString:=ReadString(Stream)
end;


procedure TFPDependency.SaveToStream(Stream: TStream);
begin
  WriteString(Stream,PackageName);
  WriteString(Stream,MinVersion.AsString);
end;


procedure TFPDependency.Assign(Source: TPersistent);
var
  S : TFPDependency;
begin
  If Source is TFPDependency then
    begin
      S:=Source as TFPDependency;
      FPackageName:=S.PackageName;
      FMinVersion.Assign(S.MinVersion);
      FOSes:=S.OSes;
      FCPUs:=S.CPUs;
    end
  else
    inherited Assign(Source);
end;


{ TFPDependencies }

function TFPDependencies.GetDependency(Index : Integer): TFPDependency;
begin
  Result:=TFPDependency(Items[Index]);
end;


procedure TFPDependencies.SetDependency(Index : Integer; const AValue: TFPDependency);
begin
  Items[Index]:=AValue;
end;


function TFPDependencies.AddDependency(const APackageName: String; const AMinVersion: String): TFPDependency;
begin
  Result:=Add as TFPDependency;
  Result.PackageName:=APackageName;
  If (AMinVersion<>'') then
    Result.MinVersion.AsString:=AMinVersion;
end;


{ TFPMirror }

constructor TFPMirror.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  Weight:=100;
end;


destructor TFPMirror.Destroy;
begin
  inherited Destroy;
end;

procedure TFPMirror.LoadFromStream(Stream: TStream; Streamversion : Integer);
begin
  Name:=ReadString(Stream);
  URL:=ReadString(Stream);
  Contact:=ReadString(Stream);
  Weight:=ReadInteger(Stream);
end;

procedure TFPMirror.SaveToStream(Stream: TStream);
begin
  WriteString(Stream,Name);
  WriteString(Stream,URL);
  WriteString(Stream,Contact);
  WriteInteger(Stream,Weight);
end;

procedure TFPMirror.Assign(Source: TPersistent);
Var
  P : TFPMirror;
begin
  if Source is TFPMirror then
    begin
    P:=Source as TFPMirror;
    // This creates trouble if P has the same owning collection !!
    If P.Collection<>Collection then
      Name:=P.Name;
    URL:=P.URL;
    Contact:=P.Contact;
    Weight:=P.Weight;
    end
  else
    inherited Assign(Source);
end;

{ TFPMirrors }

function TFPMirrors.GetMirror(Index : Integer): TFPMirror;
begin
  Result:=TFPMirror(Items[Index])
end;

procedure TFPMirrors.SetMirror(Index : Integer; const AValue: TFPMirror);
begin
   Items[Index]:=AValue;
end;

function TFPMirrors.CurrentStreamVersion: Integer;
begin
  Result:=FVersion;
end;

function TFPMirrors.IndexOfMirror(const AMirrorName: String): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetMirror(Result).Name,AMirrorName)<>0) do
    Dec(Result);
end;

function TFPMirrors.FindMirror(const AMirrorName: String): TFPMirror;
Var
  I : Integer;
begin
  I:=IndexOfMirror(AMirrorName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetMirror(I);
end;

function TFPMirrors.MirrorByName(const AMirrorName: String): TFPMirror;
begin
  Result:=FindMirror(AMirrorName);
  If Result=Nil then
    Raise EMirror.CreateFmt(SErrMirrorNotFound,[AMirrorName]);
end;

function TFPMirrors.AddMirror(const AMirrorName: string): TFPMirror;
begin
  Result:=Add as TFPMirror;
  Try
    Result.Name:=AMirrorName;
  Except
    Result.Free;
    Raise;
  end;
end;

end.
