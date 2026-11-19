{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    C++ highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.cpp;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.cpp, unittest.syntaxfixture;

type

  { TTestCppHighlighter }

  TTestCppHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestRawString;
  end;

implementation

class function TTestCppHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('cpp c++ cc h hpp',
    'class X { public: auto f() { return 42; } };',
    'class', '"class //"', '// class');
end;


procedure TTestCppHighlighter.TestRawString;

begin
  AssertRole('cpp', 'R"tag(class // " /*)tag"', 'class', shRawString);
end;

initialization
  RegisterTest(TTestCppHighlighter);
end.
