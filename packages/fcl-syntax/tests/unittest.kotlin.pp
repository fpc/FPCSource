{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Kotlin highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.kotlin;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.kotlin, unittest.syntaxfixture;

type

  { TTestKotlinHighlighter }

  TTestKotlinHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestNestedComment;
  end;

implementation

class function TTestKotlinHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('kotlin kt',
    'fun f(): Int { return 42 }',
    'fun', '"fun //"', '// fun');
end;


procedure TTestKotlinHighlighter.TestNestedComment;

begin
  AssertRole('kotlin', '/* outer /* fun */ still */ fun f() {}', 'still', shComment);
end;

initialization
  RegisterTest(TTestKotlinHighlighter);
end.
