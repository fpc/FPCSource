{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the trivia-on scanner adapter (exact token positions)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstScanner;

{ Position-asserting test for the trivia-on scanner adapter }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  PScanner, FpSonar.Ingest, UtstFixtures, UtstCoreFixtures;

type
  { Scanner adapter test: trivia tokens carry exact positions. }
  TScannerTest = class(TTestCase)
  private
    function ScanFixture: TFpSonarTokenArray;
    // Returns the first token of aKind whose Text contains aTextPart, or -1.
    function IndexOfComment(const aTokens: TFpSonarTokenArray;
      const aTextPart: string): Integer;
    // Asserts at least one token of aKind sits exactly at aRow:aCol.
    procedure AssertTokenAt(const aTokens: TFpSonarTokenArray; aKind: TToken;
      aRow, aCol: Integer; const aMsg: string);
  published
    procedure YieldsCommentsWithExactPosition;
    procedure YieldsWhitespaceAndLineEndingWithExactPosition;
    procedure IsBeginTokenClassifiesBeginKeyword;
  end;


implementation

function TScannerTest.ScanFixture: TFpSonarTokenArray;

var
  lFix: TTempFixtures;
  lScanner: TFpSonarScanner;

begin
  lFix := TTempFixtures.Create;
  try
    lScanner := TFpSonarScanner.Create;
    try
      Result := lScanner.ScanFile(
        lFix.Add('scannerfixture.pas', cScannerFixture), 'OBJFPC',
        ['FPC', 'CPUX86_64', 'UNIX', 'LINUX']);
    finally
      lScanner.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TScannerTest.IndexOfComment(const aTokens: TFpSonarTokenArray;
  const aTextPart: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := Low(aTokens) to High(aTokens) do
    if (aTokens[i].Kind = tkComment) and (Pos(aTextPart, aTokens[i].Text) > 0) then
      begin
        Result := i;
        Exit;
      end;
end;


procedure TScannerTest.AssertTokenAt(const aTokens: TFpSonarTokenArray;
  aKind: TToken; aRow, aCol: Integer; const aMsg: string);

var
  i: Integer;

begin
  for i := Low(aTokens) to High(aTokens) do
    if (aTokens[i].Kind = aKind) and (aTokens[i].Row = aRow) and
       (aTokens[i].Col = aCol) then
      Exit;
  Fail(aMsg + Format(' (no matching token at %d:%d)', [aRow, aCol]));
end;


procedure TScannerTest.YieldsCommentsWithExactPosition;

var
  lTokens: TFpSonarTokenArray;
  lLine, lBlock: Integer;

begin
  lTokens := ScanFixture;

  // Line comment '// a line comment' starts at row 5, col 1.
  lLine := IndexOfComment(lTokens, 'a line comment');
  AssertTrue('line comment token must be present', lLine >= 0);
  AssertEquals('line comment row', 5, lTokens[lLine].Row);
  AssertEquals('line comment col', 1, lTokens[lLine].Col);

  // Block comment '  { a block comment }' starts after 2-space indent at col 3.
  lBlock := IndexOfComment(lTokens, 'a block comment');
  AssertTrue('block comment token must be present', lBlock >= 0);
  AssertEquals('block comment row', 9, lTokens[lBlock].Row);
  AssertEquals('block comment col', 3, lTokens[lBlock].Col);
end;


procedure TScannerTest.YieldsWhitespaceAndLineEndingWithExactPosition;

var
  lTokens: TFpSonarTokenArray;

begin
  lTokens := ScanFixture;

  // Leading indentation before the block comment: whitespace token at row 9,
  // col 1 (the two spaces preceding the comment at col 3).
  AssertTokenAt(lTokens, tkWhitespace, 9, 1,
    'leading-indentation whitespace token expected');

  // Blank line 7 (between 'interface' and 'const'): line-ending at row 7, col 1.
  AssertTokenAt(lTokens, tkLineEnding, 7, 1,
    'blank-line line-ending token expected');
end;


procedure TScannerTest.IsBeginTokenClassifiesBeginKeyword;

var
  lBegin, lOther: TFpSonarToken;

begin
  { IsBegin is True only for the `begin` reserved word; another
    keyword (here `interface`) is rejected. The NoLegacyInitializationSection
    rule uses this to tell a legacy begin..end. unit body from an explicit
    initialization section without naming PScanner/tkbegin (chokepoint). }
  lBegin.Kind := tkbegin;
  lOther.Kind := tkinterface;
  AssertTrue('begin token is classified as a begin token', lBegin.IsBegin);
  AssertFalse('interface keyword is not a begin token', lOther.IsBegin);
end;


initialization
  RegisterTest(TScannerTest);

end.
