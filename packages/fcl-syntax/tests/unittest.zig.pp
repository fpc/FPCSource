{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Zig highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.zig;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.zig, unittest.syntaxfixture;

type

  { TTestZigHighlighter }

  TTestZigHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestMultilineString;
  end;

implementation

class function TTestZigHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('zig',
    'pub fn f() u32 { return 42; }',
    'fn', '"fn //"', '// fn');
end;


procedure TTestZigHighlighter.TestMultilineString;

begin
  AssertRole('zig', '\\fn // text', 'fn', shRawString);
end;

initialization
  RegisterTest(TTestZigHighlighter);
end.
