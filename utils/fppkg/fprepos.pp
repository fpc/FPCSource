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

uses classes,sysutils,streamcoll,contnrs,fpmktype;

Const 
  StreamVersion   : Integer = 1;
  StreamSignature = $FEEF;

Type
  { TFPVersion }

  TFPVersion = Class(TPersistent)
  private
    FMajor: Word;
    FMinor: Word;
    FRelease: Word;
    FSuffix: string;
    function GetAsString: String;
    function GetEmpty: Boolean;
    procedure SetAsString(const AValue: String);
  Public
   Procedure Assign(Source : TPersistent); override;
   Property AsString : String Read GetAsString Write SetAsString;
   Function CompareVersion(AVersion : TFPVersion) : Integer;
   Function SameVersion(AVersion : TFPVersion) : Boolean;
   Property Empty : Boolean Read GetEmpty;
  Published
   Property Release : Word Read FRelease Write FRelease;
   Property Major : Word Read FMajor Write FMajor;
   Property Minor : Word Read FMinor Write FMinor;
   Property Suffix : string Read FSuffix Write FSuffix;
  end;
  
  { TFPDependency }

  TFPDependency = Class(TStreamCollectionItem)
  private
    FMinVersion: TFPVersion;
    FPackageName: String;
    procedure SetMinVersion(const AValue: TFPVersion);
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    Procedure LoadFromStream(Stream : TStream; Streamversion : Integer); override;
    Procedure SaveToStream(Stream : TStream); override;
    Procedure Assign(Source : TPersistent); override;
  Published
    Property PackageName : String Read FPackageName Write FPackageName;
    Property MinVersion : TFPVersion Read FMinVersion Write SetMinVersion;
  end;
  
  { TFPDepencencies }

  { TFPDependencies }

  TFPDependencies = Class(TStreamCollection)
  private
    function GetDependency(Index : Integer): TFPDependency;
    procedure SetDependency(Index : Integer; const AValue: TFPDependency);
  public
    Function AddDependency(Const APackageName : String; AMinVersion : String = '') : TFPDependency;
    Property Dependencies[Index : Integer] : TFPDependency Read GetDependency Write SetDependency;default;
  end;

  { TFPPackage }

  TFPPackage = Class(TStreamCollectionItem)
  private
    FAuthor: String;
    FDescription: String;
    FEmail: String;
    FLicense: String;
    FName: String;
    FURL: String;
    FVersion: TFPVersion;
    FInstalledVersion: TFPVersion;
    FDependencies : TFPDependencies;
    FOSes : TOSES;
    FCPUs : TCPUS;
    function GetDependencies: TFPDependencies;
    function GetHasDependencies: Boolean;
    procedure SetName(const AValue: String);
    procedure SetVersion(const AValue: TFPVersion);
  Protected
    Function CreateDependencies : TFPDependencies; virtual;
  Public
    Constructor Create(ACollection : TCollection); override;
    Destructor Destroy; override;
    Procedure LoadFromStream(Stream : TStream; Streamversion : Integer); override;
    Procedure SaveToStream(Stream : TStream); override;
    Procedure Assign(Source : TPersistent); override;
    Function AddDependency(Const APackageName : String; AMinVersion : String = '') : TFPDependency;
    Property HasDependencies : Boolean Read GetHasDependencies;
    Property Dependencies : TFPDependencies Read GetDependencies;
  Published
    Property Name : String Read FName Write SetName;
    Property Author : String Read FAuthor Write FAuthor;
    Property Version : TFPVersion Read FVersion Write SetVersion;
    Property InstalledVersion : TFPVersion Read FInstalledVersion Write FInstalledVersion;
    Property License : String Read FLicense Write FLicense;
    Property Description : String Read FDescription Write FDescription;
    Property URL : String Read FURL Write FURL;
    Property Email : String Read FEmail Write FEmail;
    Property OSes : TOSes Read FOSes Write FOses;
    Property CPUs : TCPUs Read FCPUs Write FCPUs;
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
    Function IndexOfPackage(PackageName : String) : Integer;
    Function FindPackage(PackageName : String) : TFPPackage;
    Function PackageByName(PackageName : String) : TFPPackage;
    Function AddPackage(PackageName : string) : TFPPackage;
    Property StreamVersion : Integer Read FVersion Write FVersion;
    Property Packages [Index : Integer] : TFPPackage Read GetPackage Write SetPackage; default;
  end;

  { TFPRepository }

  TFPRepository = Class(TComponent)
  Private
    FMaxDependencyLevel : Integer;
    FBackUpFiles: Boolean;
    FFileName: String;
    FPackageCount: Integer;
    FPackages : TFPPackages;
    function GetPackage(Index : Integer): TFPPackage;
    function GetPackageCount: Integer;
  Protected 
    Property PackageCollection : TFPPackages Read FPackages;    
    procedure CreatePackages; virtual;
    Procedure BackupFile(AFileName : String); virtual;
    Procedure DoGetPackageDependencies(PackageName : String; List : TStringList; Level : Integer); virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    // Loading and Saving repository. Own format.
    Procedure LoadFromStream(Stream : TStream); Virtual;
    Procedure SaveToStream(Stream : TStream); Virtual;
    Procedure LoadFromFile(AFileName : String);
    Procedure SaveToFile(AFileName : String);
    Procedure Save;
    // Loading and Saving version numbers: List of Name=Value pairs.
    Procedure LoadVersionsFromStream(Stream : TStream); virtual;
    Procedure SaveVersionsToStream(Stream : TStream; InstalledVersions : Boolean); virtual;
    Procedure LoadVersionsFromFile(AFileName : String);
    Procedure SaveVersionsToFile(AFileName : String; InstalledVersions : Boolean);
    // Package management
    Function IndexOfPackage(PackageName : String) : Integer;
    Function FindPackage(PackageName : String) : TFPPackage;
    Function PackageByName(PackageName : String) : TFPPackage;
    Procedure DeletePackage(Index : Integer);
    Procedure RemovePackage(PackageName : string);
    Function AddPackage(PackageName : string) : TFPPackage;
    // Dependencies
    Procedure GetPackageDependencies(PackageName : String; List : TObjectList; Recurse : Boolean);
    // Properties
    Property FileName : String Read FFileName;
    Property Packages[Index : Integer] : TFPPackage Read GetPackage; default;
    Property PackageCount : Integer Read GetPackageCount;
    Property BackupFiles : Boolean Read FBackUpFiles Write FBackupFiles;
    Property MaxDependencyLevel : Integer Read FMaxDependencyLevel Write FMaxDependencyLevel;
  end;

  EPackage = Class(Exception);

Const
  // Max level of dependency searching before we decide it's a circular dependency.
  DefaultMaxDependencyLevel = 15;

Implementation

uses typinfo;

ResourceString

  SErrPackageNotFound      = 'Package "%s" not found.';
  SErrInvalidRepositorySig = 'Invalid repository stream. Stream signature incorrect';
  SErrBackupFailed         = 'Failed to back up file "%s" to "%s".';
  SErrNoFileName           = 'No filename for repository specified.';
  SErrDuplicatePackageName = 'Duplicate package name : "%s"';
  SErrMaxLevelExceeded     = 'Maximum number of dependency levels exceeded (%d) at package "%s".';

  
{ TFPVersion }

function TFPVersion.GetAsString: String;
begin
  Result:=Format('%d.%d.%d',[Release,Major,Minor]);
  If (Suffix<>'') then
    Result:=Result+'-'+Suffix;
end;

function TFPVersion.GetEmpty: Boolean;
begin
  Result:=(Release=0) and (Major=0) and (Minor=0) and (Suffix='');
end;

procedure TFPVersion.SetAsString(const AValue: String);

  Function NextDigit(sep : Char; var V : string) : integer;
  
  Var
    P : Integer;
  
  begin
    P:=Pos(Sep,V);
    If (P=0) then
      P:=Length(V)+1;
    Result:=StrToIntDef(Copy(V,1,P-1),-1);
    If Result<>-1 then
      Delete(V,1,P)
    else
      Result:=0;
  end;

Var
  P : Integer;
  V : String;
  
begin
  Release:=0;
  Major:=0;
  Minor:=0;
  Suffix:='';
  V:=AValue;
  Release:=NextDigit('.',V);
  Major:=NextDigit('.',V);
  Minor:=NextDigit('-',V);
  P:=Pos('-',V);
  If (P<>0) then
    Delete(V,1,P);
  Suffix:=V;
end;

procedure TFPVersion.Assign(Source: TPersistent);

Var
  V : TFPVersion;

begin
  if Source is TFPVersion then
    begin
    V:=Source as TFPVersion;
    Release:=V.Release;
    Major:=V.Major;
    Minor:=V.Minor;
    Suffix:=V.Suffix;
    end
  else
    inherited Assign(Source);
end;

function TFPVersion.CompareVersion(AVersion: TFPVersion): Integer;
begin
  Result:=Release-AVersion.Release;
  If (Result=0) then
    begin
    Result:=Major-AVersion.Major;
    if (Result=0) then
      begin
      Result:=Minor-AVersion.Minor;
      If (Result=0) then
        Result:=CompareText(Suffix,AVersion.Suffix);
      end;
    end;
end;

function TFPVersion.SameVersion(AVersion: TFPVersion): Boolean;
begin
  Result:=CompareVersion(AVersion)=0;
end;

{ TFPPackage }

procedure TFPPackage.SetVersion(const AValue: TFPVersion);
begin
  if FVersion=AValue then
    exit;
  FVersion.Assign(AValue);
end;

Function TFPPackage.CreateDependencies : TFPDependencies;
begin
  Result:=TFPDependencies.Create(TFPDependency);
end;

constructor TFPPackage.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FVersion:=TFPVersion.Create;
  FInstalledVersion:=TFPVersion.Create;
end;


destructor TFPPackage.Destroy;
begin
  FreeAndNil(FDependencies);
  FreeAndNil(FVersion);
  FreeAndNil(FInstalledVersion);
  inherited Destroy;
end;

procedure TFPPackage.SetName(const AValue: String);

Var
  I : Integer;

begin
  If (AValue<>FName) and (AValue<>'') then
    If (Collection<>Nil) and (Collection is TFPPackages) then
      // do not check while loading, this would slow down a lot !!
      if (not TFPPackages(Collection).Streaming) then
        If TFPPackages(Collection).IndexOfPackage(AValue)<>-1 then
          Raise EPackage.CreateFmt(SErrDuplicatePackageName,[AValue]);
  FName:=AValue;
end;

function TFPPackage.GetDependencies: TFPDependencies;
begin
  If Not Assigned(FDependencies) then
    FDependencies:=CreateDependencies;
  Result:=FDependencies;
end;

function TFPPackage.GetHasDependencies: Boolean;
begin
  Result:=Assigned(FDependencies) and (FDependencies.Count>0);
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
  URL:=ReadString(Stream);
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
  FreeAndNil(FDependencies);
  B:=ReadBoolean(Stream);
  If B then
    begin
    FDependencies:=CreateDependencies;
    FDependencies.LoadFromStream(Stream);
    end
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
  WriteString(Stream,URL);
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
  WriteBoolean(Stream,HasDependencies);
  If HasDependencies then
    FDependencies.SaveToStream(Stream);
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
    URL:=P.URL;
    InstalledVersion:=P.Installedversion;
    If P.HasDependencies then
      Dependencies.Assign(P.Dependencies)
    else
      FreeAndNil(FDependencies);
    end
  else
    inherited Assign(Source);
end;

function TFPPackage.AddDependency(const APackageName: String;
  AMinVersion: String): TFPDependency;
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

function TFPPackages.IndexOfPackage(PackageName: String): Integer;


begin
  Result:=Count-1;
  While (Result>=0) and (CompareText(GetPackage(Result).Name,PackageName)<>0) do
    Dec(Result);
end;

function TFPPackages.FindPackage(PackageName: String): TFPPackage;

Var
  I : Integer;

begin
  I:=IndexOfPackage(PackageName);
  If (I=-1) then
    Result:=Nil
  else
    Result:=GetPackage(I);
end;

function TFPPackages.PackageByName(PackageName: String): TFPPackage;
begin
  Result:=FindPackage(PackageName);
  If Result=Nil then
    Raise EPackage.CreateFmt(SErrPackageNotFound,[PackageName]);
end;

function TFPPackages.AddPackage(PackageName: string): TFPPackage;

begin
  Result:=Add as TFPPackage;
  Try
    Result.Name:=PackageName;
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

procedure TFPRepository.BackupFile(AFileName: String);

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

procedure TFPRepository.LoadFromFile(AFileName: String);

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

procedure TFPRepository.SaveToFile(AFileName: String);

Var
  F : TFileStream;
  S : String;

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

procedure TFPRepository.LoadVersionsFromStream(Stream: TStream);

Var
  L : TStrings;
  I : Integer;
  N,V : String;

begin
  L:=TStringList.Create;
  Try
    L.LoadFromStream(Stream);
    For I:=0 to L.Count-1 do
      begin
      L.GetNameValue(I,N,V);
      If (N<>'') and (V<>'') then
        PackageByName(N).InstalledVersion.AsString:=V;
      end;
  Finally
    L.Free;
  end;
end;

procedure TFPRepository.SaveVersionsToStream(Stream: TStream;InstalledVersions : Boolean);

Var
  L : TStrings;
  I : Integer;

begin
  L:=TStringList.Create;
  Try
    If InstalledVersions then
      For I:=0 to PackageCount-1 do
        With Packages[i] do
          L.Add(Name+'='+InstalledVersion.AsString)
    else
      For I:=0 to PackageCount-1 do
        With Packages[i] do
         L.Add(Name+'='+Version.AsString);
    L.SaveToStream(Stream);
  Finally
    L.Free;
  end;
end;

procedure TFPRepository.LoadVersionsFromFile(AFileName: String);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmOpenRead);
  Try
    LoadVersionsFromStream(F);
  Finally
    F.Free;
  end;
end;

procedure TFPRepository.SaveVersionsToFile(AFileName: String; InstalledVersions : Boolean);

Var
  F : TFileStream;

begin
  If FileExists(AFileName) and BackupFiles then
    BackupFile(AFileName);
  F:=TFileStream.Create(AFileName,fmCreate);
  Try
    SaveVersionsToStream(F,InstalledVersions);
  Finally
    F.Free;
  end;
end;

function TFPRepository.IndexOfPackage(PackageName: String): Integer;
begin
  Result:=FPackages.IndexOfPackage(PackageName);
end;

function TFPRepository.FindPackage(PackageName: String): TFPPackage;
begin
  Result:=FPackages.FindPackage(PackageName);
end;

function TFPRepository.PackageByName(PackageName: String): TFPPackage;
begin
  Result:=FPackages.PackageByName(PackageName);
end;

procedure TFPRepository.RemovePackage(PackageName: string);
begin
  PackageByName(PackageName).Free;
end;

procedure TFPRepository.DeletePackage(Index : Integer);
begin
  GetPackage(Index).Free;
end;

function TFPRepository.AddPackage(PackageName: string): TFPPackage;

begin
  Result:=FPackages.AddPackage(PackageName);
end;

procedure TFPRepository.DoGetPackageDependencies(PackageName: String;
  List: TStringList; Level: Integer);
Var
  P : TFPPackage;
  D2,D1 : TFPDependency;
  i,J : Integer;

begin
  // If too many levels, bail out
  If (Level>FMaxDependencyLevel) then
    Raise EPackage.CreateFmt(SErrMaxLevelExceeded,[Level,PackageName]);
  // Check if it is a known package.
  P:=FindPackage(PackageName);
  If Assigned(P) and P.HasDependencies then
    For I:=0 to P.Dependencies.Count-1 do
      begin
      D1:=P.Dependencies[i];
      J:=List.IndexOf(PackageName);
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

procedure TFPRepository.GetPackageDependencies(PackageName: String;
  List: TObjectList; Recurse: Boolean);

Var
  L : TStringList;
  I : Integer;
  
begin
  L:=TStringList.Create;
  Try
    L.Sorted:=True;
    DoGetPackageDependencies(PackageName,L,Ord(Recurse)-1);
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
end;

destructor TFPDependency.Destroy;
begin
  FreeAndNil(FMinVersion);
  inherited Destroy;
end;

procedure TFPDependency.LoadFromStream(Stream: TStream; Streamversion: Integer
  );
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
begin
  If Source is TFPDependency then
    With Source as TFPDependency do
      begin
      Self.PackageName:=PackageName;
      Self.MinVersion:=MinVersion;
      end
  else
    inherited Assign(Source);
end;

{ TFPDependencies }

function TFPDependencies.GetDependency(Index : Integer): TFPDependency;
begin
  Result:=TFPDependency(Items[Index]);
end;

procedure TFPDependencies.SetDependency(Index : Integer;
  const AValue: TFPDependency);
begin
  Items[Index]:=AValue;
end;

function TFPDependencies.AddDependency(const APackageName: String;
  AMinVersion: String): TFPDependency;
begin
  Result:=Add as TFPDependency;
  Result.PackageName:=APackageName;
  If (AMinVersion<>'') then
    Result.MinVersion.AsString:=AMinVersion;
end;

end.
