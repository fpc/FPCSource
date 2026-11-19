{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    TOML highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.toml;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.toml, unittest.syntaxfixture;

type

  { TTestTomlHighlighter }

  TTestTomlHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestMultilineString;
    procedure TestQuotedString;
    procedure TestComment;
    procedure TestKey;
  end;

implementation

class function TTestTomlHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('toml',
    '[example]' + #10 + 'name = true' + #10 + 'value = 42',
    '', '"true #"', '# true');
end;


procedure TTestTomlHighlighter.TestMultilineString;

begin
  AssertRole('toml', 'value = """true #' + #10 + 'text"""', 'true', shStrings);
end;


procedure TTestTomlHighlighter.TestQuotedString;

begin
  AssertRole('toml', '"true #"', 'true', shStrings);
end;


procedure TTestTomlHighlighter.TestComment;

begin
  AssertRole('toml', '# true', 'true', shComment);
end;


procedure TTestTomlHighlighter.TestKey;

begin
  AssertRole('toml', 'name = true', 'name', shKey);
end;

initialization
  RegisterTest(TTestTomlHighlighter);
end.
