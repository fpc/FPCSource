{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Diff highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.diff;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.diff, unittest.syntaxfixture;

type

  { TTestDiffHighlighter }

  TTestDiffHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestAdded;
    procedure TestRemoved;
    procedure TestHunk;
    procedure TestAddedHeaderLookalike;
  end;

implementation

class function TTestDiffHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('diff patch',
    '--- a/file' + #10 + '+++ b/file' + #10 + '@@ -1 +1 @@' + #10 + '-old' + #10 + '+new',
    '', '+"--- #"', '\ No newline at end of file');
end;


procedure TTestDiffHighlighter.TestAdded;

begin
  AssertRole('diff', '+new' + #10 + '-old' + #10 + '@@ -1 +1 @@', '+new', shStrings);
end;


procedure TTestDiffHighlighter.TestRemoved;

begin
  AssertRole('diff', '-old', '-old', shError);
end;


procedure TTestDiffHighlighter.TestHunk;

begin
  AssertRole('diff', '+new' + #10 + '-old' + #10 + '@@ -1 +1 @@', '@@', shSection);
end;


procedure TTestDiffHighlighter.TestAddedHeaderLookalike;

begin
  AssertRole('diff', '+--- header-looking text', '---', shStrings);
end;

initialization
  RegisterTest(TTestDiffHighlighter);
end.
