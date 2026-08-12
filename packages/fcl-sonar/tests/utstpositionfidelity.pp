{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for scanner position fidelity across includes and conditionals

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstPositionFidelity;

{ Position-fidelity test. }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PScanner, FpSonar.Ingest, UtstFixtures;

type
  { Position-fidelity test: include + excluded-conditional positions are exact. }
  TPositionFidelityTest = class(TTestCase)
  private
    function ScanMain: TFpSonarTokenArray;
    // Index of the first identifier token whose Text equals aName, or -1.
    function IndexOfIdent(const aTokens: TFpSonarTokenArray;
      const aName: string): Integer;
  published
    procedure IncludeTokenCarriesIncludeFileAndRow;
    procedure TokenAfterExcludedBranchCarriesTrueRow;
  end;


implementation

const
  // Embedded position-fidelity fixtures (Approach A): line i+1 == [i].
  // Row/col of tokens are asserted exactly — do NOT reformat these.

  cPosMain: array[0..22] of string = (
    'unit PosMain;',
    '',
    '{ Position-fidelity fixture. Pulls posinc.inc in via a',
    '  relative {$include} (resolved against this file''s directory) and contains a',
    '  {$ifdef}-excluded block. The scanner must report include tokens against',
    '  posinc.inc with their in-include rows, and the token after the excluded',
    '  branch (cAfter) with its true row in THIS file. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'const',
    '{$include posinc.inc}',
    '',
    '{$ifdef NEVER_DEFINED}',
    '  cExcluded = 1;',
    '{$endif}',
    '  cAfter = 2;',
    '',
    'implementation',
    '',
    'end.');

  cPosInc: array[0..2] of string = (
    '// included fragment for the position-fidelity test. Tokens here',
    '// must report FileName ending in posinc.inc and their row WITHIN this file.',
    '  cFromInclude = 7;');

function TPositionFidelityTest.ScanMain: TFpSonarTokenArray;

var
  lFix: TTempFixtures;
  lScanner: TFpSonarScanner;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    // posinc.inc must be co-located so the relative {$include} resolves via the
    // scanner's BaseDirectory (set from the opened file's path).
    lFix.Add('posinc.inc', cPosInc);
    lPath := lFix.Add('posmain.pas', cPosMain);
    lScanner := TFpSonarScanner.Create;
    try
      Result := lScanner.ScanFile(lPath, 'OBJFPC',
        ['FPC', 'CPUX86_64', 'UNIX', 'LINUX'], []);
    finally
      lScanner.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TPositionFidelityTest.IndexOfIdent(const aTokens: TFpSonarTokenArray;
  const aName: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := Low(aTokens) to High(aTokens) do
    if (aTokens[i].Kind = tkIdentifier) and
       SameText(aTokens[i].Text, aName) then
      begin
        Result := i;
        Exit;
      end;
end;


procedure TPositionFidelityTest.IncludeTokenCarriesIncludeFileAndRow;

const
  cInIncludeRow = 3;
  cInIncludeCol = 3;

var
  lTokens: TFpSonarTokenArray;
  lIdx: Integer;

begin
  lTokens := ScanMain;

  // cFromInclude is declared inside posinc.inc — its token must carry the
  // include's FileName and its row WITHIN the include, not the main file.
  lIdx := IndexOfIdent(lTokens, 'cFromInclude');
  AssertTrue('cFromInclude token must be present (include resolved)', lIdx >= 0);
  AssertTrue('cFromInclude FileName ends in posinc.inc',
    Pos('posinc.inc', lTokens[lIdx].FileName) > 0);
  AssertEquals('cFromInclude row within the include', cInIncludeRow,
    lTokens[lIdx].Row);
  AssertEquals('cFromInclude col within the include', cInIncludeCol,
    lTokens[lIdx].Col);
end;


procedure TPositionFidelityTest.TokenAfterExcludedBranchCarriesTrueRow;

const
  cAfterRow = 19;
  cAfterCol = 3;

var
  lTokens: TFpSonarTokenArray;
  lIdx: Integer;

begin
  lTokens := ScanMain;

  // cExcluded sits in a {$ifdef NEVER_DEFINED} branch — it must NOT be emitted.
  AssertEquals('excluded-branch token must not be emitted', -1,
    IndexOfIdent(lTokens, 'cExcluded'));

  // cAfter follows the excluded block — its row must be its true row in the
  // main file (the excluded lines do not shift it).
  lIdx := IndexOfIdent(lTokens, 'cAfter');
  AssertTrue('cAfter token must be present', lIdx >= 0);
  AssertTrue('cAfter FileName ends in posmain.pas',
    Pos('posmain.pas', lTokens[lIdx].FileName) > 0);
  AssertEquals('cAfter true row in the main file', cAfterRow,
    lTokens[lIdx].Row);
  AssertEquals('cAfter col in the main file', cAfterCol, lTokens[lIdx].Col);
end;


initialization
  RegisterTest(TPositionFidelityTest);

end.
