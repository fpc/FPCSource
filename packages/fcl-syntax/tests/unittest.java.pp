{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Java highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.java;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.java, unittest.syntaxfixture;

type

  { TTestJavaHighlighter }

  TTestJavaHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestAnnotation;
    procedure TestTextBlock;
  end;

implementation

class function TTestJavaHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('java',
    'class X { int f() { return 42; } }',
    'class', '"class //"', '// class');
end;


procedure TTestJavaHighlighter.TestAnnotation;

begin
  AssertRole('java', '@Override class X {}', '@Override', shAttribute);
end;


procedure TTestJavaHighlighter.TestTextBlock;

begin
  AssertRole('java', '"""class //' + #10 + 'text"""', 'class', shStrings);
end;

initialization
  RegisterTest(TTestJavaHighlighter);
end.
