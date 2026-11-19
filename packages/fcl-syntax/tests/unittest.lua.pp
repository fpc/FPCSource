{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Lua highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.lua;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.lua, unittest.syntaxfixture;

type

  { TTestLuaHighlighter }

  TTestLuaHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestLongComment;
    procedure TestLongString;
    procedure TestUnfinishedLongBracketFast;
  end;

implementation

class function TTestLuaHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('lua',
    'local function f(x) return x + 42 end',
    'local', '"local --"', '-- local');
end;


procedure TTestLuaHighlighter.TestLongComment;

begin
  AssertRole('lua', '--[==[local ]=] still]==] local x', 'still', shComment);
end;


procedure TTestLuaHighlighter.TestLongString;

begin
  AssertRole('lua', '[=[local -- ]=]', 'local', shRawString);
end;


procedure TTestLuaHighlighter.TestUnfinishedLongBracketFast;

begin
  AssertFast('lua', '[' + StringOfChar('=', 1024 * 1024 - 1));
end;

initialization
  RegisterTest(TTestLuaHighlighter);
end.
