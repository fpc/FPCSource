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



implementation

uses
  zipper,
  uriparser,
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
          DeleteDir(ADir+PathDelim+Info.Name)
        else
          DeleteFile(ADir+PathDelim+Info.Name);
      until FindNext(Info)<>0;
    finally
      FindClose(Info);
    end;
end;


{ TFPMakeCompiler }

Procedure TUnzipArchive.UnzipArchive;
Var
  BuildDir : string;
  ArchiveFile : String;
begin
  ArchiveFile:=PackageArchive;
  BuildDir:=PackageBuildPath;
  { Remove existing builddir }
  if DirectoryExists(BuildDir) then
    DeleteDir(BuildDir);
  { Unzip Archive }
//  SetCurrentDir(PackageBuildPath);
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


initialization
  RegisterPkgHandler('unziparchive',TUnzipArchive);
end.
