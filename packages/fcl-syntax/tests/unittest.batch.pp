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
    procedure TestRemMixedCase;
    procedure TestRemPrefixOfWord;
    procedure TestRemCutOff;
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


procedure TTestBatchHighlighter.TestRemMixedCase;

begin
  AssertRole('batch', 'Rem if exist', 'if', shComment);
  AssertRole('batch', 'rEm if exist', 'if', shComment);
  AssertRole('batch', 'rem if exist', 'if', shComment);
end;


procedure TTestBatchHighlighter.TestRemPrefixOfWord;

begin
  AssertRole('batch', 'remark' + #13#10 + 'if exist x', 'if', shKeyword);
  AssertRole('batch', 'REMARK' + #13#10 + 'if exist x', 'if', shKeyword);
end;


procedure TTestBatchHighlighter.TestRemCutOff;

begin
  AssertRole('batch', 're', 're', shDefault);
  AssertRole('batch', 'if x' + #13#10 + 'Re', 'Re', shDefault);
end;

initialization
  RegisterTest(TTestBatchHighlighter);
end.
