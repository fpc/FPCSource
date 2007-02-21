unit pkgarchive;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,pkghandler;

type
  { TUnzipArchive }

  TUnzipArchive = Class(TPackagehandler)
  Private
    Procedure UnzipArchive;
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


  { TCreateArchive }

  TCreateArchive = Class(TPackagehandler)
  Private
    Procedure CreateArchive;
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


implementation

uses
  fprepos,
  fpxmlrep,
  zipper,
  uriparser,
  pkgglobals,
  pkgmessages;

Procedure DeleteDir(const ADir:string);
const
{$ifdef unix}
  AllFiles='*';
{$else}
  AllFiles='*.*';
{$endif}
var
  Info : TSearchRec;
begin
  if FindFirst(ADir+PathDelim+AllFiles,faAnyFile, Info)=0 then
    try
      repeat
        if (Info.Attr and faDirectory)=faDirectory then
          begin
            if (Info.Name<>'.') and (Info.Name<>'..') then
              DeleteDir(ADir+PathDelim+Info.Name)
          end
        else
          DeleteFile(ADir+PathDelim+Info.Name);
      until FindNext(Info)<>0;
    finally
      FindClose(Info);
    end;
end;


{ TUnzipArchive }

Procedure TUnzipArchive.UnzipArchive;
Var
  BuildDir : string;
  ArchiveFile : String;
begin
  ArchiveFile:=PackageLocalArchive;
  BuildDir:=PackageBuildPath;
  { Download file if it doesn't exists yet }
  if not FileExists(ArchiveFile) then
    ExecuteAction(CurrentPackage,'downloadpackage');
  { Create builddir, remove it first if needed }
  if DirectoryExists(BuildDir) then
    DeleteDir(BuildDir);
  ForceDirectories(BuildDir);
  SetCurrentDir(BuildDir);
  { Unzip Archive }
  With TUnZipper.Create do
    try
      Log(vCommands,SLogUnzippping,[ArchiveFile]);
      OutputPath:=PackageBuildPath;
      UnZipAllFiles(ArchiveFile);
    Finally
      Free;
    end;
end;


function TUnzipArchive.Execute(const Args:TActionArgs):boolean;
begin
{$warning TODO Check arguments}
  UnzipArchive;
  result:=true;
end;


{ TCreateArchive }

procedure TCreateArchive.CreateArchive;
var
  P : TFPPackage;
  PS : TFPPackages;
  X : TFPXMLRepositoryHandler;
  i : integer;
begin
  if assigned(CurrentPackage) then
    Error(SErrOnlyLocalDir);
  { Generate manifest.xml if it doesn't exists yet }
  if not FileExists(PackageManifestFile) then
    ExecuteAction(CurrentPackage,'fpmakemanifest');


  PS:=TFPPackages.Create(TFPPackage);
  X:=TFPXMLRepositoryHandler.Create;
  With X do
    try
      LoadFromXml(PS,PackageManifestFile);
    finally
      Free;
    end;

  for i:=0 to PS.Count-1 do
    begin
      P:=PS[i];
      Writeln(P.Name);
      Writeln(P.FileName);
    end;
end;


function TCreateArchive.Execute(const Args: TActionArgs): boolean;
begin
  CreateArchive;
  Result:=true;
end;


initialization
  RegisterPkgHandler('unziparchive',TUnzipArchive);
  RegisterPkgHandler('createarchive',TCreateArchive);
end.
