{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Modula-2 highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.modula2;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.modula2, unittest.syntaxfixture;

type

  { TTestModula2Highlighter }

  TTestModula2Highlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestNestedComment;
  end;

implementation

class function TTestModula2Highlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('modula-2 modula2',
    'MODULE Example; BEGIN END Example.',
    'MODULE', '"MODULE (*"', '(* MODULE *)');
end;


procedure TTestModula2Highlighter.TestNestedComment;

begin
  AssertRole('modula-2', '(* (* MODULE *) still *) MODULE X;', 'still', shComment);
end;

initialization
  RegisterTest(TTestModula2Highlighter);
end.
