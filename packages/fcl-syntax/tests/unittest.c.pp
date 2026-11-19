{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    C highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.c;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.c, unittest.syntaxfixture;

type

  { TTestCHighlighter }

  TTestCHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestDirective;
    procedure TestNumbers;
    procedure TestTypeIsBuiltin;
  end;

implementation

class function TTestCHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('c',
    'int f(void) { return 0x2a; }',
    'return', '"return //"', '// return');
end;


procedure TTestCHighlighter.TestDirective;

begin
  AssertRole('c', '#include <x.h>', '#include', shDirective);
end;


procedure TTestCHighlighter.TestNumbers;

begin
  AssertRole('c', 'x=0x2a+0b10+1.5e-2;', '0x2a', shNumbers);
  AssertRole('c', 'x=0x2a+0b10+1.5e-2;', '0b10', shNumbers);
  AssertRole('c', 'x=0x2a+0b10+1.5e-2;', '1.5e-2', shNumbers);
end;


procedure TTestCHighlighter.TestTypeIsBuiltin;

begin
  AssertRole('c', 'int f(void);', 'int', shKey);
end;

initialization
  RegisterTest(TTestCHighlighter);
end.
