unit pkgrepos;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,Classes,
  fprepos,pkgoptions,
  pkgFppkg,
  fpmkunit;

function GetRemoteRepositoryURL(const AFileName:string):string;

procedure LoadLocalAvailableMirrors;
function LoadManifestFromFile(const AManifestFN:string):TFPPackage;
procedure FindInstalledPackages(ACompilerOptions:TCompilerOptions;showdups:boolean=true);
Procedure AddFPMakeAddIn(APackage: TFPPackage);
function  FindBrokenPackages(SL:TStrings):Boolean;
procedure CheckFPMakeDependencies;
procedure ListPackages(const ShowGlobalAndLocal: boolean);
procedure InitializeFppkg;

procedure ClearRemoteRepository;

procedure SetDefaultRepositoryClass(ARepositoryClass: TFPRepositoryClass);

var
  AvailableMirrors    : TFPMirrors;
  GFPpkg: TpkgFppkg;


implementation

uses
  zipper,
  fpxmlrep,
  pkgglobals,
  pkgmessages,
  pkgPackagesStructure;

resourcestring
  SErrRepositoryClassAlreadyAssigned = 'Default repository class is already assigned.';

var
  CurrentRemoteRepositoryURL : String;
  RepositoryClass : TFPRepositoryClass;

procedure SetDefaultRepositoryClass(ARepositoryClass: TFPRepositoryClass);
begin
  if assigned(RepositoryClass) then
    raise exception.Create(SErrRepositoryClassAlreadyAssigned);
  RepositoryClass:=ARepositoryClass;
end;

function GetDefaultRepositoryClass: TFPRepositoryClass;
begin
  if not assigned(RepositoryClass) then
    SetDefaultRepositoryClass(TFPRepository);
  result := RepositoryClass;
end;

{*****************************************************************************
                           Mirror Selection
*****************************************************************************}

procedure LoadLocalAvailableMirrors;
var
  S : String;
  X : TFPXMLMirrorHandler;
begin
  if assigned(AvailableMirrors) then
    AvailableMirrors.Free;
  AvailableMirrors:=TFPMirrors.Create(TFPMirror);

  // Repository
  S:=GFPpkg.Options.GlobalSection.LocalMirrorsFile;
  log(llDebug,SLogLoadingMirrorsFile,[S]);
  if not FileExists(S) then
    exit;
  try
    X:=TFPXMLMirrorHandler.Create;
    With X do
      try
        LoadFromXml(AvailableMirrors,S);
      finally
        Free;
      end;
  except
    on E : Exception do
      begin
        Log(llError,E.Message);
        Error(SErrCorruptMirrorsFile,[S]);
      end;
  end;
end;


function SelectRemoteMirror:string;
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


function GetRemoteRepositoryURL(const AFileName:string):string;
begin
  if CurrentRemoteRepositoryURL='' then
    begin
      if GFPpkg.Options.GlobalSection.RemoteRepository='auto' then
        CurrentRemoteRepositoryURL:=SelectRemoteMirror
      else
        CurrentRemoteRepositoryURL:=GFPpkg.Options.GlobalSection.RemoteRepository;
    end;
  result := CurrentRemoteRepositoryURL;
  if result[length(result)]<>'/' then
    result := result + '/';
  Result:=Result+GFPpkg.CompilerOptions.CompilerVersion+'/'+AFileName;
end;


{*****************************************************************************
                           Local Repository
*****************************************************************************}

function LoadManifestFromFile(const AManifestFN:string):TFPPackage;
var
  X : TFPXMLRepositoryHandler;
  NewPackages : TFPPackages;
  NewP : TFPPackage;
begin
  result:=nil;
  NewPackages:=TFPPackages.Create(TFPPackage);
  X:=TFPXMLRepositoryHandler.Create;
  try
    X.LoadFromXml(NewPackages,AManifestFN);
    if NewPackages.Count=1 then
      begin
        NewP:=NewPackages[0];
        result:=TFPPackage.Create(nil);
        // Copy contents
        result.Assign(NewP);
      end
    else
      Error(SErrManifestNoSinglePackage,[AManifestFN]);
  finally
    X.Free;
    NewPackages.Free;
  end;
end;


procedure FindInstalledPackages(ACompilerOptions:TCompilerOptions;showdups:boolean=true);
begin
  GFPpkg.ScanPackages;
end;


Procedure AddFPMakeAddIn(APackage: TFPPackage);
begin
  log(llDebug,SLogFoundFPMakeAddin,[APackage.Name]);
  setlength(FPMKUnitDeps,length(FPMKUnitDeps)+1);
  FPMKUnitDeps[high(FPMKUnitDeps)].package:=APackage.Name;
  FPMKUnitDeps[high(FPMKUnitDeps)].reqver:=APackage.Version.AsString;
  FPMKUnitDeps[high(FPMKUnitDeps)].def:='HAS_PACKAGE_'+APackage.Name;
  FPMKUnitDeps[high(FPMKUnitDeps)].available:=true;
end;


function FindBrokenPackages(SL:TStrings):Boolean;
var
  i,j,k : integer;
  P : TFPPackage;
  Repo: TFPRepository;
begin
  SL.Clear;
  for i:=0 to GFPpkg.RepositoryList.Count-1 do
    begin
      Repo := TFPRepository(GFPpkg.RepositoryList[i]);
      if Repo.RepositoryType = fprtInstalled then
        begin
          for j := 0 to Repo.PackageCount-1 do
            begin
              P := Repo.Packages[j];
              if P.IsPackageBroken then
                SL.Add(P.Name)
              else
                begin
                  // It could be that a package is broken in one repository,
                  // but that this problem is 'fixed' in a repository with an higher
                  // priority
                  k := SL.IndexOf(P.Name);
                  if k > -1 then
                    SL.Delete(k);
                end;
            end;
        end;
    end;
  Result:=(SL.Count>0);
end;


procedure CheckFPMakeDependencies;
var
  i : Integer;
  P,AvailP : TFPPackage;
  AvailVerStr : string;
  ReqVer : TFPVersion;
begin
  // Reset availability
  for i:=0 to high(FPMKUnitDeps) do
    FPMKUnitDeps[i].available:=false;
  // Not version check needed in Recovery mode, we always need to use
  // the internal bootstrap procedure
  if GFPpkg.Options.CommandLineSection.RecoveryMode then
    exit;
  // Check for fpmkunit dependencies
  for i:=0 to high(FPMKUnitDeps) do
    begin
      P:=GFPpkg.FPMakeRepoFindPackage(FPMKUnitDeps[i].package, pkgpkInstalled);
      if P<>nil then
        begin
          AvailP:=GFPpkg.FindPackage(FPMKUnitDeps[i].package, pkgpkAvailable);
          if AvailP<>nil then
            AvailVerStr:=AvailP.Version.AsString
          else
            AvailVerStr:='<not available>';
          ReqVer:=TFPVersion.Create;
          try
            ReqVer.AsString:=FPMKUnitDeps[i].ReqVer;
            log(llDebug,SLogFPMKUnitDepVersion,[P.Name,ReqVer.AsString,P.Version.AsString,AvailVerStr]);
            if ReqVer.CompareVersion(P.Version)<=0 then
              FPMKUnitDeps[i].available:=true
            else
              log(llDebug,SLogFPMKUnitDepTooOld,[FPMKUnitDeps[i].package]);
          finally
            ReqVer.Free;
          end;
        end
      else
        log(llDebug,SLogFPMKUnitDepTooOld,[FPMKUnitDeps[i].package]);
    end;
end;


{*****************************************************************************
                           Local Available Repository
*****************************************************************************}

procedure ListPackages(const ShowGlobalAndLocal: boolean);

  procedure AddPackageToLine(APackage: TFPPackage; CheckIsBroken: Boolean; var Line: string);
  var
    PackageVersion: string;
  begin
    if Assigned(APackage) then
      begin
        PackageVersion := APackage.Version.AsString;
        if CheckIsBroken and APackage.IsPackageBroken then
          PackageVersion := PackageVersion + ' (B)';
      end
    else
      PackageVersion := '-';
    Line :=  Line + Format(' %-14s', [PackageVersion])
  end;

var
  i,j : integer;
  SL : TStringList;
  PackageName : string;
  Repo: TFPRepository;
  Package: TFPPackage;
  Header: string;
  Line: string;
begin
  SL:=TStringList.Create;
  SL.Sorted:=true;
  SL.Duplicates:=dupIgnore;

  Header := Format('%-20s', ['Name']);
  for i:=0 to GFPpkg.RepositoryList.Count-1 do
    begin
      Repo := TFPRepository(GFPpkg.RepositoryList[i]);
      Header := Header + Format(' %-14s', [Repo.RepositoryName]);
      for j:=0 to Repo.PackageCount-1 do
        begin
          SL.Add(Repo.Packages[j].Name);
        end;
    end;
  if ShowGlobalAndLocal then
    WriteLn(Header)
  else
    Writeln(Format('%-20s %-14s %-14s',['Name','Installed','Available']));

  for i:=0 to SL.Count-1 do
    begin
      PackageName:=SL[i];
      if (PackageName<>CmdLinePackageName) and (PackageName<>CurrentDirPackageName) then
        begin
          Line:=Format('%-20s', [PackageName]);
          if ShowGlobalAndLocal then
            begin
              for j:=0 to GFPpkg.RepositoryList.Count-1 do
                begin
                  Repo := TFPRepository(GFPpkg.RepositoryList[j]);
                  Package := Repo.FindPackage(PackageName);
                  AddPackageToLine(Package, Repo.RepositoryType = fprtInstalled, Line);
                end
            end
          else
            begin
              Package := GFPpkg.FindPackage(PackageName, pkgpkInstalled);
              AddPackageToLine(Package, True, Line);
              Package := GFPpkg.FindPackage(PackageName, pkgpkAvailable);
              AddPackageToLine(Package, False, Line);
            end;
        end;
      WriteLn(Line);
    end;
  FreeAndNil(SL);
end;


procedure InitializeFppkg;
begin
  if Assigned(GFPpkg) then
    GFPpkg.Free;
  GFPpkg := TpkgFPpkg.Create(nil);
end;

procedure ClearRemoteRepository;
begin
  CurrentRemoteRepositoryURL := '';
end;

initialization
  GFPpkg := nil;
  AvailableMirrors := nil;
finalization
  AvailableMirrors.Free;
  GFPpkg.Free;
end.
