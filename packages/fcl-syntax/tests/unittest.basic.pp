{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    BASIC highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.basic;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.basic, unittest.syntaxfixture;

type

  { TTestBasicHighlighter }

  TTestBasicHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestRemComment;
    procedure TestRemMixedCase;
    procedure TestDoubledQuote;
  end;

implementation

class function TTestBasicHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('basic vb freebasic',
    'Function f() As Integer' + #10 + 'Return 42' + #10 + 'End Function',
    'Function', '"Function rem"', ''' Function');
end;


procedure TTestBasicHighlighter.TestRemComment;

begin
  AssertRole('basic', 'REM Function', 'Function', shComment);
end;


procedure TTestBasicHighlighter.TestRemMixedCase;

begin
  AssertRole('basic', 'Rem Function', 'Function', shComment);
  AssertRole('basic', 'x = 1 rem Function', 'Function', shComment);
  AssertRole('basic', 'Remove Function', 'Function', shKeyword);
end;


procedure TTestBasicHighlighter.TestDoubledQuote;

begin
  AssertRole('basic', '"Function "" rem"', 'rem', shStrings);
end;

initialization
  RegisterTest(TTestBasicHighlighter);
end.
