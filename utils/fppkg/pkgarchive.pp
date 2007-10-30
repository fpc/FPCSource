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


{$ifdef OLDCREATEARCHIVE}
  { TCreateArchive }

  TCreateArchive = Class(TPackagehandler)
  Private
    Procedure CreateArchive;
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;
{$endif OLDCREATEARCHIVE}


implementation

uses
  fprepos,
  fpxmlrep,
  zipper,
  uriparser,
  pkgglobals,
  pkgmessages;

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


{$ifdef OLDCREATEARCHIVE}
{ TCreateArchive }

procedure TCreateArchive.CreateArchive;
var
  P : TFPPackage;
  PS : TFPPackages;
  X : TFPXMLRepositoryHandler;
  SL : TStringList;
begin
  if assigned(CurrentPackage) then
    Error(SErrOnlyLocalDir);
  { Generate manifest.xml if it doesn't exists yet }
  if not FileExists(PackageManifestFile) then
    ExecuteAction(CurrentPackage,'fpmakemanifest');
  { Load manifest.xml }
  PS:=TFPPackages.Create(TFPPackage);
  X:=TFPXMLRepositoryHandler.Create;
  With X do
    try
      LoadFromXml(PS,PackageManifestFile);
    finally
      Free;
    end;
  { Create archive, currently support only 1 file per package, this
    can be enhanced in the future if needed }
  if PS.Count<>1 then
    Error('Only one package supported per manifest');
  P:=PS[0];
  { Unzip Archive }
  With TZipper.Create do
    try
      Log(vCommands,SLogZippping,[P.FileName]);
{$warning TODO replace with files from manifest}
      try
        SL:=TStringList.Create;
        SearchFiles(SL,AllFiles);
        if SL.Count=0 then
          Error('No files found');
        ZipFiles(P.FileName,SL);
      finally
        SL.Free;
      end;
    Finally
      Free;
    end;
  P.Free;
end;

function TCreateArchive.Execute(const Args: TActionArgs): boolean;
begin
  CreateArchive;
  Result:=true;
end;
{$endif OLDCREATEARCHIVE}


initialization
  RegisterPkgHandler('unziparchive',TUnzipArchive);
{$ifdef OLDCREATEARCHIVE}
  RegisterPkgHandler('createarchive',TCreateArchive);
{$endif OLDCREATEARCHIVE}
end.
