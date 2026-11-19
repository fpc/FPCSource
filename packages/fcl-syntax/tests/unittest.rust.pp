{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Rust highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.rust;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.rust, unittest.syntaxfixture;

type

  { TTestRustHighlighter }

  TTestRustHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestRawString;
    procedure TestLifetime;
    procedure TestUnicodeEscape;
    procedure TestHexEscape;
  end;

implementation

class function TTestRustHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('rust rs',
    'fn f() -> u32 { let x = 0xff; x }',
    'fn', '"fn //"', '// fn');
end;


procedure TTestRustHighlighter.TestRawString;

begin
  AssertRole('rust', 'r##"fn // " #"##', 'fn', shRawString);
end;


procedure TTestRustHighlighter.TestLifetime;

begin
  AssertRole('rust', '''life: loop { let x = ''é''; }', 'loop', shKeyword);
end;


procedure TTestRustHighlighter.TestUnicodeEscape;

begin
  AssertRole('rust', '''\u{1F600}''', '\u', shCharacters);
end;


procedure TTestRustHighlighter.TestHexEscape;

begin
  AssertRole('rust', '''\x41''', '\x', shCharacters);
end;

initialization
  RegisterTest(TTestRustHighlighter);
end.
