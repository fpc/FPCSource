unit pkgrepos;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,Classes,
  fprepos,pkgoptions;

function GetRemoteRepositoryURL(const AFileName:string):string;

procedure LoadLocalMirrors;
procedure LoadLocalRepository;
procedure FindInstalledPackages(ACompilerOptions:TCompilerOptions);
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


procedure FindInstalledPackages(ACompilerOptions:TCompilerOptions);

  function LoadOrCreatePackage(const AName:string):TFPPackage;
  begin
    result:=CurrentRepository.FindPackage(AName);
    if not assigned(result) then
      begin
        result:=CurrentRepository.AddPackage(AName);
        result.IsLocalPackage:=true;
      end;
  end;

  procedure LoadUnitConfigFromFile(APackage:TFPPackage;const AFileName: String);
  Var
    L : TStrings;
    V : String;
  begin
    L:=TStringList.Create;
    Try
      ReadIniFile(AFileName,L);
{$warning TODO Maybe check also CPU-OS}
{$warning TODO Add date to check recompile}
      V:=L.Values['version'];
      APackage.InstalledVersion.AsString:=V;
    Finally
      L.Free;
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
                  LoadUnitConfigFromFile(P,UF)
                end
              else
                begin
                  // Try Old style Package.fpc
                  UF:=UD+'Package.fpc';
                  if FileExistsLog(UF) then
                    begin
                      P:=LoadOrCreatePackage(SR.Name);
                      LoadPackagefpcFromFile(P,UF);
                    end;
                end;
            end;
        until FindNext(SR)<>0;
      end;
  end;

begin
  CurrentRepository.ClearStatus;
  if ACompilerOptions.LocalUnitDir<>'' then
    CheckUnitDir(ACompilerOptions.LocalUnitDir);
  if ACompilerOptions.GlobalUnitDir<>'' then
    CheckUnitDir(ACompilerOptions.GlobalUnitDir);
end;


procedure CheckFPMakeDependencies;
var
  i : Integer;
  P : TFPPackage;
  ReqVer : TFPVersion;
begin
  // Check for fpmkunit dependencies
  for i:=1 to FPMKUnitDepCount do
    begin
      FPMKUnitDepAvailable[i]:=false;
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
begin
  Writeln(Format('%-20s %-12s %-12s',['Name','Installed','Available']));
  for i:=0 to CurrentRepository.PackageCount-1 do
    begin
      P:=CurrentRepository.Packages[i];
      if all or (P.Version.CompareVersion(P.InstalledVersion)>0) then
        begin
          Writeln(Format('%-20s %-12s %-12s',[P.Name,P.InstalledVersion.AsString,P.Version.AsString]));
        end;
    end;
end;


{*****************************************************************************
                           Remote Repository
*****************************************************************************}

procedure ListRemoteRepository;
var
  P : TFPPackage;
  i : integer;
begin
  Writeln(Format('%-20s %-12s %-20s',['Name','Available','FileName']));
  for i:=0 to CurrentRepository.PackageCount-1 do
    begin
      P:=CurrentRepository.Packages[i];
      Writeln(Format('%-20s %-12s %-20s',[P.Name,P.Version.AsString,P.FileName]));
    end;
end;


procedure RebuildRemoteRepository;

  procedure AddPackage(const AManifestFN:string);
  var
    X : TFPXMLRepositoryHandler;
    i : integer;
    DoAdd : Boolean;
    P,NewP : TFPPackage;
    NewPackages : TFPPackages;
  begin
    NewPackages:=TFPPackages.Create(TFPPackage);
    X:=TFPXMLRepositoryHandler.Create;
    try
      X.LoadFromXml(NewPackages,AManifestFN);
      DeleteFile(ManifestFileName);
      // Update or Add packages to repository
      for i:=0 to NewPackages.Count-1 do
        begin
          NewP:=NewPackages[i];
          DoAdd:=True;
          P:=CurrentRepository.FindPackage(NewP.Name);
          if assigned(P) then
            begin
              if NewP.Version.CompareVersion(P.Version)<0 then
                begin
                  Writeln(Format('Ignoring package %s-%s (old %s)',[NewP.Name,NewP.Version.AsString,P.Version.AsString]));
                  DoAdd:=False;
                end
              else
                Writeln(Format('Updating package %s-%s (old %s)',[NewP.Name,NewP.Version.AsString,P.Version.AsString]));
            end
          else
            P:=CurrentRepository.PackageCollection.AddPackage(NewP.Name);
          // Copy contents
          if DoAdd then
            P.Assign(NewP);
        end;
    finally
      X.Free;
      NewPackages.Free;
    end;
  end;

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
          AddPackage(ManifestFileName)
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



initialization
end.
