{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Oberon highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.oberon;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.oberon, unittest.syntaxfixture;

type

  { TTestOberonHighlighter }

  TTestOberonHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestNestedComment;
  end;

implementation

class function TTestOberonHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('oberon',
    'MODULE Example; BEGIN END Example.',
    'MODULE', '"MODULE (*"', '(* MODULE *)');
end;


procedure TTestOberonHighlighter.TestNestedComment;

begin
  AssertRole('oberon', '(* (* MODULE *) still *) MODULE X;', 'still', shComment);
end;

initialization
  RegisterTest(TTestOberonHighlighter);
end.
