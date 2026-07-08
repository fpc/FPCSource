{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Test helper: materialises embedded fixtures into a temp directory

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit UtstFixtures;

{ Test helper: writes embedded fixture sources into a unique temporary
  directory and deletes the whole directory on Free. }

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  { Owns a unique temp directory; writes fixtures into it and removes it (and
    everything written) on Free. One instance per test, freed in a finally. }
  TTempFixtures = class
  private
    FDir: string;
    FFiles: TStringList;
  public
    // Creates a unique, empty temporary directory.
    constructor Create;
    // Deletes every written fixture and removes the directory.
    destructor Destroy; override;
    // Writes aLines (one element per source line) to <dir>/aName and returns the
    // full path. Line i+1 of the fixture == aLines[i].
    function Add(const aName: string; const aLines: array of string): string;
    // The temp directory (the resolver's base directory for cross-unit fixtures).
    property Dir: string read FDir;
  end;


implementation

constructor TTempFixtures.Create;

var
  lBase: string;

begin
  inherited Create;
  FFiles := TStringList.Create;
  // GetTempFileName yields a unique path (and touches the file); reuse the name
  // for a directory instead.
  lBase := GetTempFileName;
  DeleteFile(lBase);
  FDir := lBase;
  if not CreateDir(FDir) then
    raise Exception.CreateFmt('UtstFixtures: cannot create temp dir %s', [FDir]);
end;


destructor TTempFixtures.Destroy;

var
  i: Integer;

begin
  for i := 0 to FFiles.Count - 1 do
    DeleteFile(FFiles[i]);
  RemoveDir(FDir);
  FFiles.Free;
  inherited Destroy;
end;


function TTempFixtures.Add(const aName: string;
  const aLines: array of string): string;

var
  lText: TStringList;
  i: Integer;

begin
  Result := IncludeTrailingPathDelimiter(FDir) + aName;
  lText := TStringList.Create;
  try
    for i := Low(aLines) to High(aLines) do
      lText.Add(aLines[i]);
    lText.SaveToFile(Result);
  finally
    lText.Free;
  end;
  FFiles.Add(Result);
end;


end.
