unit pkgrepos;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,
  fprepos;

procedure LoadLocalRepository;
procedure SaveRepository;
procedure ListRepository;
procedure RebuildRepository;

var
  CurrentRepository : TFPRepository;


implementation

uses
  zipper,
  fpxmlrep,
  pkgglobals,
  pkgoptions,
  pkgmessages;


procedure LoadLocalRepository;
var
  S : String;
  X : TFPXMLRepositoryHandler;
begin
  if assigned(CurrentRepository) then
    CurrentRepository.Free;
  CurrentRepository:=TFPRepository.Create(Nil);
  // Repository
  Log(vDebug,SLogLoadingPackagesFile,[Options.LocalPackagesFile]);
  if FileExists(Options.LocalPackagesFile) then
    begin
      X:=TFPXMLRepositoryHandler.Create;
      With X do
        try
          LoadFromXml(CurrentRepository,Options.LocalPackagesFile);
        finally
          Free;
        end;
    end;
  // Versions
  S:=Options.LocalVersionsFile(Options.CurrentCompilerConfig);
  Log(vDebug,SLogLoadingVersionsFile,[S]);
  if FileExists(S) then
    CurrentRepository.LoadStatusFromFile(S);
end;


procedure SaveRepository;
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


procedure ListRepository;
var
  P : TFPPackage;
  i : integer;
begin
  for i:=0 to CurrentRepository.PackageCount-1 do
    begin
      P:=CurrentRepository.Packages[i];
      Writeln(Format('%-20s %-20s',[P.Name,P.FileName]));
    end;
end;


procedure RebuildRepository;
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
    ManifestSL.Add(DefaultManifestFile);
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
            Log(vCommands,SLogUnzippping,[ArchiveSL[i]]);
            OutputPath:='.';
            UnZipFiles(ArchiveSL[i],ManifestSL);
          Finally
            Free;
          end;
        { Load manifest.xml }
        if FileExists(DefaultManifestFile) then
          begin
            X:=TFPXMLRepositoryHandler.Create;
            With X do
              try
                LoadFromXml(CurrentRepository.PackageCollection,DefaultManifestFile);
              finally
                Free;
              end;
            DeleteFile(DefaultManifestFile);
          end
        else
          Writeln('No manifest found in archive ',ArchiveSL[i]);
      end;
  finally
    ArchiveSL.Free;
    ManifestSL.Free;
  end;
end;

initialization
end.
