{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    C# highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.csharp;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.csharp, unittest.syntaxfixture;

type

  { TTestCSharpHighlighter }

  TTestCSharpHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestVerbatimString;
    procedure TestRawString;
    procedure TestQuoteRunsFast;
  end;

implementation

class function TTestCSharpHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('csharp cs c#',
    'class X { int f() { return 42; } }',
    'class', '"class //"', '// class');
end;


procedure TTestCSharpHighlighter.TestVerbatimString;

begin
  AssertRole('csharp', '@"class // "" text"', 'class', shStrings);
end;


procedure TTestCSharpHighlighter.TestRawString;

begin
  AssertRole('csharp', '"""class /* """', 'class', shRawString);
end;


procedure TTestCSharpHighlighter.TestQuoteRunsFast;

begin
  AssertFast('csharp', StringOfChar('"', 524288) + 'x' + StringOfChar('"', 524287) + 'y');
end;

initialization
  RegisterTest(TTestCSharpHighlighter);
end.
