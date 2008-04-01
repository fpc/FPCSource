unit pkgrepos;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,Classes,
  fprepos,pkgoptions;

function GetRemoteRepositoryURL(const AFileName:string):string;

procedure LoadLocalMirrors;
procedure LoadLocalRepository;
function  LoadOrCreatePackage(const AName:string):TFPPackage;
procedure LoadUnitConfigFromFile(APackage:TFPPackage;const AFileName: String);
function  LoadPackageManifest(const AManifestFN:string):TFPPackage;
procedure FindInstalledPackages(ACompilerOptions:TCompilerOptions;showdups:boolean=true);
function  PackageIsBroken(APackage:TFPPackage):boolean;
function  FindBrokenPackages(SL:TStrings):Boolean;
procedure CheckFPMakeDependencies;
procedure ListLocalRepository(all:boolean=false);

procedure ListRemoteRepository;
procedure RebuildRemoteRepository;
procedure SaveRemoteRepository;

var
  CurrentMirrors    : TFPMirrors;
  CurrentRepository : TFPRepository;


implementation

uses
  zipper,
  fpxmlrep,
  pkgglobals,
  pkgmessages;

{*****************************************************************************
                           Mirror Selection
*****************************************************************************}

var
  CurrentRemoteRepositoryURL : String;

procedure LoadLocalMirrors;
var
  S : String;
  X : TFPXMLMirrorHandler;
begin
  if assigned(CurrentMirrors) then
    CurrentMirrors.Free;
  CurrentMirrors:=TFPMirrors.Create(TFPMirror);

  // Repository
  S:=GlobalOptions.LocalMirrorsFile;
  Log(vlDebug,SLogLoadingMirrorsFile,[S]);
  if not FileExists(S) then
    exit;
  try
    X:=TFPXMLMirrorHandler.Create;
    With X do
      try
        LoadFromXml(CurrentMirrors,S);
      finally
        Free;
      end;
  except
    on E : Exception do
      begin
        Log(vlError,E.Message);
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
  if assigned(CurrentMirrors) then
   begin
     // Create array for selection
     BucketCnt:=0;
     for i:=0 to CurrentMirrors.Count-1 do
       inc(BucketCnt,CurrentMirrors[i].Weight);
     // Select random entry
     Bucket:=Random(BucketCnt);
     M:=nil;
     for i:=0 to CurrentMirrors.Count-1 do
       begin
         for j:=0 to CurrentMirrors[i].Weight-1 do
           begin
             if Bucket=0 then
               begin
                 M:=CurrentMirrors[i];
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
      Log(vlInfo,SLogSelectedMirror,[M.Name]);
      Result:=M.URL;
    end
  else
    Error(SErrFailedToSelectMirror);
end;


function GetRemoteRepositoryURL(const AFileName:string):string;
begin
  if CurrentRemoteRepositoryURL='' then
    begin
      if GlobalOptions.RemoteRepository='auto' then
        CurrentRemoteRepositoryURL:=SelectRemoteMirror
      else
        CurrentRemoteRepositoryURL:=GlobalOptions.RemoteRepository;
    end;
  Result:=CurrentRemoteRepositoryURL+AFileName;
end;


{*****************************************************************************
                           Local Repository
*****************************************************************************}

procedure ReadIniFile(Const AFileName: String;L:TStrings);
Var
  F : TFileStream;
  Line : String;
  I,P,PC : Integer;
begin
  F:=TFileStream.Create(AFileName,fmOpenRead);
  Try
    L.LoadFromStream(F);
    // Fix lines.
    For I:=L.Count-1 downto 0 do
      begin
        Line:=L[I];
        P:=Pos('=',Line);
        PC:=Pos(';',Line);  // Comment line.
        If (P=0) or ((PC<>0) and (PC<P)) then
          L.Delete(I)
        else
          L[i]:=Trim(System.Copy(Line,1,P-1)+'='+Trim(System.Copy(Line,P+1,Length(Line)-P)));
      end;
  Finally
    F.Free;
  end;
end;


procedure LoadLocalRepository;
var
  S : String;
  X : TFPXMLRepositoryHandler;
begin
  if assigned(CurrentRepository) then
    CurrentRepository.Free;
  CurrentRepository:=TFPRepository.Create(Nil);
  // Repository
  S:=GlobalOptions.LocalPackagesFile;
  Log(vlDebug,SLogLoadingPackagesFile,[S]);
  if not FileExists(S) then
    exit;
  try
    X:=TFPXMLRepositoryHandler.Create;
    With X do
      try
        LoadFromXml(CurrentRepository,S);
      finally
        Free;
      end;
  except
    on E : Exception do
      begin
        Log(vlError,E.Message);
        Error(SErrCorruptPackagesFile,[S]);
      end;
  end;
end;


function LoadOrCreatePackage(const AName:string):TFPPackage;
begin
  result:=CurrentRepository.FindPackage(AName);
  if not assigned(result) then
    begin
      result:=CurrentRepository.AddPackage(AName);
      result.IsLocalPackage:=true;
    end;
end;


function LoadPackageManifest(const AManifestFN:string):TFPPackage;
var
  X : TFPXMLRepositoryHandler;
  i : integer;
  DoAdd : Boolean;
  NewP : TFPPackage;
  NewPackages : TFPPackages;
begin
  result:=nil;
  NewPackages:=TFPPackages.Create(TFPPackage);
  X:=TFPXMLRepositoryHandler.Create;
  try
    X.LoadFromXml(NewPackages,AManifestFN);
    // Update or Add packages to repository
    for i:=0 to NewPackages.Count-1 do
      begin
        NewP:=NewPackages[i];
        DoAdd:=True;
        result:=CurrentRepository.FindPackage(NewP.Name);
        if assigned(result) then
          begin
            if NewP.Version.CompareVersion(result.Version)<0 then
              begin
                Writeln(Format('Ignoring package %s-%s (old %s)',[NewP.Name,NewP.Version.AsString,result.Version.AsString]));
                DoAdd:=False;
              end
            else
              Writeln(Format('Updating package %s-%s (old %s)',[NewP.Name,NewP.Version.AsString,result.Version.AsString]));
          end
        else
          result:=CurrentRepository.PackageCollection.AddPackage(NewP.Name);
        // Copy contents
        if DoAdd then
          result.Assign(NewP);
      end;
  finally
    X.Free;
    NewPackages.Free;
  end;
end;


procedure LoadUnitConfigFromFile(APackage:TFPPackage;const AFileName: String);
Var
  L,DepSL : TStrings;
  DepName,
  V : String;
  DepChecksum : Cardinal;
  i,j,k : integer;
  D : TFPDependency;
begin
  L:=TStringList.Create;
  Try
    ReadIniFile(AFileName,L);
{$warning TODO Maybe check also CPU-OS}
    // Read fpunits.conf
    V:=L.Values['version'];
    APackage.InstalledVersion.AsString:=V;
    V:=L.Values['checksum'];
    if V<>'' then
      APackage.InstalledChecksum:=StrToInt(V)
    else
      APackage.InstalledChecksum:=$ffffffff;
    // Load dependencies
    V:=L.Values['depends'];
    DepSL:=TStringList.Create;
    DepSL.CommaText:=V;
    for i:=0 to DepSL.Count-1 do
      begin
        DepName:=DepSL[i];
        k:=Pos('|',DepName);
        if k>0 then
          begin
            DepChecksum:=StrToInt(Copy(DepName,k+1,Length(DepName)-k));
            DepName:=Copy(DepName,1,k-1);
          end
        else
          DepChecksum:=$ffffffff;
        D:=nil;
        for j:=0 to APackage.Dependencies.Count-1 do
          begin
            D:=APackage.Dependencies[j];
            if D.PackageName=DepName then
              break;
            D:=nil;
          end;
        if not assigned(D) then
          D:=APackage.AddDependency(DepName,'');
        D.RequireChecksum:=DepChecksum;
      end;
    DepSL.Free;
  Finally
    L.Free;
  end;
end;


procedure FindInstalledPackages(ACompilerOptions:TCompilerOptions;showdups:boolean=true);

  procedure LogDuplicatePackages(APackage:TFPPackage;const AFileName: String);
  begin
    // Log packages found in multiple locations (local and global) ?
    if not APackage.InstalledVersion.Empty then
      begin
        if showdups then
          Log(vlDebug,SDbgPackageMultipleLocations,[APackage.Name,ExtractFilePath(AFileName)]);
      end;
  end;

  procedure LoadPackagefpcFromFile(APackage:TFPPackage;const AFileName: String);
  Var
    L : TStrings;
    V : String;
  begin
    L:=TStringList.Create;
    Try
      ReadIniFile(AFileName,L);
      V:=L.Values['version'];
      APackage.InstalledVersion.AsString:=V;
    Finally
      L.Free;
    end;
  end;

  function CheckUnitDir(const AUnitDir:string):boolean;
  var
    SR : TSearchRec;
    P  : TFPPackage;
    UD,UF : String;
  begin
    Result:=false;
    if FindFirst(IncludeTrailingPathDelimiter(AUnitDir)+AllFiles,faDirectory,SR)=0 then
      begin
        Log(vlDebug,SLogFindInstalledPackages,[AUnitDir]);
        repeat
          if ((SR.Attr and faDirectory)=faDirectory) and (SR.Name<>'.') and (SR.Name<>'..') then
            begin
              UD:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(AUnitDir)+SR.Name);
              // Try new fpunits.conf
              UF:=UD+UnitConfigFileName;
              if FileExistsLog(UF) then
                begin
                  P:=LoadOrCreatePackage(SR.Name);
                  LogDuplicatePackages(P,UF);
                  LoadUnitConfigFromFile(P,UF)
                end
              else
                begin
                  // Try Old style Package.fpc
                  UF:=UD+'Package.fpc';
                  if FileExistsLog(UF) then
                    begin
                      P:=LoadOrCreatePackage(SR.Name);
                      LogDuplicatePackages(P,UF);
                      LoadPackagefpcFromFile(P,UF);
                    end;
                end;
            end;
        until FindNext(SR)<>0;
      end;
  end;

begin
  CurrentRepository.ClearStatus;
  // First scan the global directory
  // The local directory will overwrite the versions
  if ACompilerOptions.GlobalUnitDir<>'' then
    CheckUnitDir(ACompilerOptions.GlobalUnitDir);
  if ACompilerOptions.LocalUnitDir<>'' then
    CheckUnitDir(ACompilerOptions.LocalUnitDir);
end;


function PackageIsBroken(APackage:TFPPackage):boolean;
var
  j : integer;
  D : TFPDependency;
  DepPackage : TFPPackage;
begin
  result:=false;
  for j:=0 to APackage.Dependencies.Count-1 do
    begin
      D:=APackage.Dependencies[j];
      if (CompilerOptions.CompilerOS in D.OSes) and
         (CompilerOptions.CompilerCPU in D.CPUs) then
        begin
          DepPackage:=CurrentRepository.FindPackage(D.PackageName);
          // Don't stop on missing dependencies
          if assigned(DepPackage) then
            begin
              if (DepPackage.InstalledChecksum<>D.RequireChecksum) then
                begin
                  Log(vlInfo,SLogPackageChecksumChanged,[APackage.Name,D.PackageName]);
                  result:=true;
                  exit;
                end;
            end
          else
            Log(vlDebug,SDbgObsoleteDependency,[D.PackageName]);
        end;
    end;
end;


function FindBrokenPackages(SL:TStrings):Boolean;
var
  i : integer;
  P : TFPPackage;
begin
  SL.Clear;
  for i:=0 to CurrentRepository.PackageCount-1 do
    begin
      P:=CurrentRepository.Packages[i];
      // Process only installed packages
      if not P.InstalledVersion.Empty then
        begin
          if PackageIsBroken(P) then
            SL.Add(P.Name);
        end;
    end;
  Result:=(SL.Count>0);
end;


procedure CheckFPMakeDependencies;
var
  i : Integer;
  P : TFPPackage;
  ReqVer : TFPVersion;
begin
  // Reset availability
  for i:=1 to FPMKUnitDepCount do
    FPMKUnitDepAvailable[i]:=false;
  // Not version check needed in Recovery mode, we always need to use
  // the internal bootstrap procedure
  if GlobalOptions.RecoveryMode then
    exit;
  // Check for fpmkunit dependencies
  for i:=1 to FPMKUnitDepCount do
    begin
      P:=CurrentRepository.FindPackage(FPMKUnitDeps[i].package);
      if P<>nil then
        begin
          ReqVer:=TFPVersion.Create;
          ReqVer.AsString:=FPMKUnitDeps[i].ReqVer;
          Log(vlDebug,SLogFPMKUnitDepVersion,[P.Name,ReqVer.AsString,P.InstalledVersion.AsString,P.Version.AsString]);
          if ReqVer.CompareVersion(P.InstalledVersion)<=0 then
            FPMKUnitDepAvailable[i]:=true
          else
            Log(vlDebug,SLogFPMKUnitDepTooOld,[FPMKUnitDeps[i].package]);
        end
      else
        Log(vlDebug,SLogFPMKUnitDepTooOld,[FPMKUnitDeps[i].package]);
    end;
end;


procedure ListLocalRepository(all:boolean=false);
var
  P : TFPPackage;
  i : integer;
  SL : TStringList;
begin
  SL:=TStringList.Create;
  SL.Sorted:=true;
  for i:=0 to CurrentRepository.PackageCount-1 do
    begin
      P:=CurrentRepository.Packages[i];
      if all or (P.Version.CompareVersion(P.InstalledVersion)>0) then
        SL.Add(Format('%-20s %-12s %-12s',[P.Name,P.InstalledVersion.AsString,P.Version.AsString]));
    end;
  Writeln(Format('%-20s %-12s %-12s',['Name','Installed','Available']));
  for i:=0 to SL.Count-1 do
    Writeln(SL[i]);
  FreeAndNil(SL);
end;


{*****************************************************************************
                           Remote Repository
*****************************************************************************}

procedure ListRemoteRepository;
var
  P : TFPPackage;
  i : integer;
  SL : TStringList;
begin
  SL:=TStringList.Create;
  SL.Sorted:=true;
  for i:=0 to CurrentRepository.PackageCount-1 do
    begin
      P:=CurrentRepository.Packages[i];
      SL.Add(Format('%-20s %-12s %-20s',[P.Name,P.Version.AsString,P.FileName]));
    end;
  Writeln(Format('%-20s %-12s %-20s',['Name','Available','FileName']));
  for i:=0 to SL.Count-1 do
    Writeln(SL[i]);
  FreeAndNil(SL);
end;


procedure RebuildRemoteRepository;

var
  i : integer;
  ArchiveSL : TStringList;
  ManifestSL : TStringList;
begin
  if assigned(CurrentRepository) then
    CurrentRepository.Free;
  CurrentRepository:=TFPRepository.Create(Nil);
  try
    ManifestSL:=TStringList.Create;
    ManifestSL.Add(ManifestFileName);
    { Find all archives }
    ArchiveSL:=TStringList.Create;
    SearchFiles(ArchiveSL,'*.zip');
    if ArchiveSL.Count=0 then
      Error('No archive files found');
    { Process all archives }
    for i:=0 to ArchiveSL.Count-1 do
      begin
        Writeln('Processing ',ArchiveSL[i]);
        { Unzip manifest.xml }
        With TUnZipper.Create do
          try
            Log(vlCommands,SLogUnzippping,[ArchiveSL[i]]);
            OutputPath:='.';
            UnZipFiles(ArchiveSL[i],ManifestSL);
          Finally
            Free;
          end;
        { Load manifest.xml }
        if FileExists(ManifestFileName) then
          begin
            LoadPackageManifest(ManifestFileName);
            DeleteFile(ManifestFileName);
          end
        else
          Writeln('No manifest found in archive ',ArchiveSL[i]);

      end;
  finally
    ArchiveSL.Free;
    ManifestSL.Free;
  end;
end;


procedure SaveRemoteRepository;
var
  X : TFPXMLRepositoryHandler;
begin
  // Repository
  Writeln('Saving repository in packages.xml');
  X:=TFPXMLRepositoryHandler.Create;
  With X do
    try
      SaveToXml(CurrentRepository,'packages.xml');
    finally
      Free;
    end;
end;

end.
