{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Makefile highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.makefile;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.makefile, unittest.syntaxfixture;

type

  { TTestMakefileHighlighter }

  TTestMakefileHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestNestedInterpolation;
    procedure TestParenthesesInInterpolation;
    procedure TestDollarRunFast;
  end;

implementation

class function TTestMakefileHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('makefile make',
    'include config.mk' + #10 + 'all: main.o' + #10 + #9 + '$(CC) -o $@ $<',
    'include', 'x = $(include #)', '# include');
end;


procedure TTestMakefileHighlighter.TestNestedInterpolation;

begin
  AssertRole('makefile', 'x = $(outer ${inner}) # include', 'inner', shInterpolation);
end;


procedure TTestMakefileHighlighter.TestParenthesesInInterpolation;

begin
  AssertRole('makefile', 'x = $(outer (inner) end)', 'end', shInterpolation);
end;


procedure TTestMakefileHighlighter.TestDollarRunFast;

begin
  AssertFast('makefile', StringOfChar('$', 1024 * 1024));
end;

initialization
  RegisterTest(TTestMakefileHighlighter);
end.
