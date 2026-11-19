{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    D highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.d;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.d, unittest.syntaxfixture;

type

  { TTestDHighlighter }

  TTestDHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestNestedComment;
  end;

implementation

class function TTestDHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('d',
    'module example; int f() { return 42; }',
    'module', '"module //"', '// module');
end;


procedure TTestDHighlighter.TestNestedComment;

begin
  AssertRole('d', '/+ outer /+ module +/ still +/ module x;', 'still', shComment);
end;

initialization
  RegisterTest(TTestDHighlighter);
end.
