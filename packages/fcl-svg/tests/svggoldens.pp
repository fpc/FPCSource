{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Golden file comparison shared by the fcl-svg test suites.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit svggoldens;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpcUnit.Test;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpcunit;
{$ENDIF FPC_DOTTEDUNITS}

// The directory of the golden files, from SVG_GOLDEN_DIR or
// tests/goldens.
function GoldenDir: String;
// The directory of the test documents, from SVG_DATA_DIR or tests/data.
function DataDir: String;
// True when SVG_UPDATE_GOLDENS requests that the golden files are
// rewritten.
function UpdatingGoldens: Boolean;
// Compares lines against the golden file of that name, or rewrites the
// file when updating.
procedure AssertGolden(aTest: TAssert; const aName: String; aActual: TStrings);
// Compares text against the golden file of that name, or rewrites the
// file when updating.
procedure AssertGoldenText(aTest: TAssert; const aName, aActual: String);

implementation

function GoldenDir: String;

begin
  Result := GetEnvironmentVariable('SVG_GOLDEN_DIR');
  if Result = '' then
    Result := 'tests' + PathDelim + 'goldens';
  Result := IncludeTrailingPathDelimiter(Result);
end;


function DataDir: String;

begin
  Result := GetEnvironmentVariable('SVG_DATA_DIR');
  if Result = '' then
    Result := 'tests' + PathDelim + 'data';
  Result := IncludeTrailingPathDelimiter(Result);
end;


function UpdatingGoldens: Boolean;

begin
  Result := GetEnvironmentVariable('SVG_UPDATE_GOLDENS') <> '';
end;


procedure AssertGolden(aTest: TAssert; const aName: String; aActual: TStrings);

var
  lFileName: String;
  lExpected: TStringList;
  I, lCount: Integer;

begin
  lFileName := GoldenDir + aName + '.log';
  if UpdatingGoldens then
    begin
    ForceDirectories(GoldenDir);
    aActual.SaveToFile(lFileName);
    Exit;
    end;
  aTest.AssertTrue(Format('golden file %s is missing; rerun with SVG_UPDATE_GOLDENS=1 to create it',
    [lFileName]), FileExists(lFileName));
  lExpected := TStringList.Create;
  try
    lExpected.LoadFromFile(lFileName);
    lCount := lExpected.Count;
    if aActual.Count < lCount then
      lCount := aActual.Count;
    for I := 0 to lCount - 1 do
      if lExpected[I] <> aActual[I] then
        begin
        aActual.SaveToFile(lFileName + '.actual');
        aTest.AssertEquals(Format('%s line %d matches the golden log', [aName, I + 1]),
          lExpected[I], aActual[I]);
        end;
    if lExpected.Count <> aActual.Count then
      begin
      aActual.SaveToFile(lFileName + '.actual');
      aTest.AssertEquals(Format('%s has the same number of lines as the golden log',
        [aName]), lExpected.Count, aActual.Count);
      end;
  finally
    lExpected.Free;
  end;
end;


procedure AssertGoldenText(aTest: TAssert; const aName, aActual: String);

var
  lLines: TStringList;

begin
  lLines := TStringList.Create;
  try
    lLines.Text := aActual;
    AssertGolden(aTest, aName, lLines);
  finally
    lLines.Free;
  end;
end;


end.
