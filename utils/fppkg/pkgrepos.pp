unit pkgrepos;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,
  fprepos;

function GetRemoteRepositoryURL(const AFileName:string):string;

procedure LoadLocalMirrors;
procedure LoadLocalRepository;
procedure LoadLocalStatus;
procedure SaveLocalStatus;
procedure LoadFPMakeLocalStatus;
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
  pkgoptions,
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


procedure LoadLocalStatus;
var
  S : String;
begin
  S:=GlobalOptions.LocalVersionsFile(GlobalOptions.CompilerConfig);
  Log(vlDebug,SLogLoadingStatusFile,[S]);
  CurrentRepository.ClearStatus;
  if FileExists(S) then
    CurrentRepository.LoadStatusFromFile(S);
end;


procedure SaveLocalStatus;
var
  S : String;
begin
  S:=GlobalOptions.LocalVersionsFile(GlobalOptions.CompilerConfig);
  Log(vlDebug,SLogSavingStatusFile,[S]);
  CurrentRepository.SaveStatusToFile(S);
end;


procedure LoadFPMakeLocalStatus;
var
  i : Integer;
  S : String;
  P : TFPPackage;
  ReqVer : TFPVersion;
begin
  S:=GlobalOptions.LocalVersionsFile(GlobalOptions.FPMakeCompilerConfig);
  Log(vlDebug,SLogLoadingStatusFile,[S]);
  CurrentRepository.ClearStatus;
  if FileExists(S) then
    CurrentRepository.LoadStatusFromFile(S);
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
var
  X : TFPXMLRepositoryHandler;
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
            X:=TFPXMLRepositoryHandler.Create;
            With X do
              try
                LoadFromXml(CurrentRepository.PackageCollection,ManifestFileName);
              finally
                Free;
              end;
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



initialization
end.
