{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Windows batch file highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.batch;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.batch, unittest.syntaxfixture;

type

  { TTestBatchHighlighter }

  TTestBatchHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestRemComment;
  end;

implementation

class function TTestBatchHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('batch bat cmd',
    '@echo off' + #13#10 + 'if exist x echo yes',
    'if', '"if rem"', 'rem if');
end;


procedure TTestBatchHighlighter.TestRemComment;

begin
  AssertRole('batch', 'REM if exist', 'if', shComment);
end;

initialization
  RegisterTest(TTestBatchHighlighter);
end.
