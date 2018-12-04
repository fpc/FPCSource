unit pas2jswebcompiler;

{$mode objfpc}

interface

uses
  Classes, SysUtils, pas2jsfs, pasuseanalyzer, pas2jscompiler, FPPJsSrcMap, webfilecache;

Type

  { TPas2JSWebcompiler }

  TPas2JSWebcompiler = Class(TPas2JSCompiler)
  private
    function GetWebFS: TPas2JSWebFS;
  Protected
    function DoWriteJSFile(const DestFilename: String; aWriter: TPas2JSMapper): Boolean; override;
    function CreateSetOfCompilerFiles(keyType: TKeyCompareType): TPasAnalyzerKeySet; override;
    function CreateFS : TPas2JSFS; override;
  Public
    Property WebFS : TPas2JSWebFS read GetWebFS;
  end;

implementation

uses js;

function Pas2jsCompilerFile_FilenameToKeyName(Item: Pointer): String;
var
  aFile: TPas2jsCompilerFile absolute Item;
begin
  Result:=LowerCase(aFile.PasFilename);
end;

function PtrUnitnameToKeyName(Item: Pointer): String;
var
  aUnitName: string absolute Item;
begin
  Result:=LowerCase(aUnitName);
end;

function Pas2jsCompilerFile_UnitnameToKeyName(Item: Pointer): String;
var
  aFile: TPas2jsCompilerFile absolute Item;
begin
  Result:=LowerCase(aFile.PasUnitName);
end;

function PtrFilenameToKeyName(FilenameAsPtr: Pointer): string;
var
  Filename: String absolute FilenameAsPtr;
begin
  Result:=LowerCase(Filename);
end;


{ TPas2JSWebcompiler }

function TPas2JSWebcompiler.GetWebFS: TPas2JSWebFS;
begin
  Result:=TPas2JSWebFS(FS)
end;

function TPas2JSWebcompiler.DoWriteJSFile(const DestFilename: String; aWriter: TPas2JSMapper): Boolean;

Var
  S : String;
  T : String;

begin
//  Writeln('aWriter',AWriter.BufferLength,', array size ',Length(AWriter.Buffer));
  S:=TJSArray(AWriter.Buffer).Join('');
//  Writeln('TPas2JSWebcompiler.DoWriteJSFile(',DestFileName,') (',Length(S),' chars): ',S);
  WebFS.SetFileContent(DestFileName,S);
  Result:=True;
end;

function TPas2JSWebcompiler.CreateSetOfCompilerFiles(keyType: TKeyCompareType): TPasAnalyzerKeySet;
begin
  Case keyType of
    kcFileName:
      Result:=TPasAnalyzerKeySet.Create(@Pas2jsCompilerFile_FilenameToKeyName,@PtrFilenameToKeyName);
    kcUnitName:
      Result:=TPasAnalyzerKeySet.Create(@Pas2jsCompilerFile_UnitnameToKeyName,@PtrUnitnameToKeyName);
  else
    Raise EPas2jsFS.CreateFmt('Internal Unknown key type: %d',[Ord(KeyType)]);
  end;
end;

function TPas2JSWebcompiler.CreateFS: TPas2JSFS;
begin
  Result:=TPas2JSWebFS.Create;
end;

end.

