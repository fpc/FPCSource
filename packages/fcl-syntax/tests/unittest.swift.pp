{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Swift highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.swift;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.swift, unittest.syntaxfixture;

type

  { TTestSwiftHighlighter }

  TTestSwiftHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestRawString;
    procedure TestHashRunFast;
  end;

implementation

class function TTestSwiftHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('swift',
    'func f() -> Int { return 42 }',
    'func', '"func //"', '// func');
end;


procedure TTestSwiftHighlighter.TestRawString;

begin
  AssertRole('swift', '#"func //"#', 'func', shRawString);
end;


procedure TTestSwiftHighlighter.TestHashRunFast;

begin
  AssertFast('swift', StringOfChar('#', 1024 * 1024));
end;

initialization
  RegisterTest(TTestSwiftHighlighter);
end.
