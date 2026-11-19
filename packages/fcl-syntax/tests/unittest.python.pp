{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Python highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.python;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.python, unittest.syntaxfixture;

type

  { TTestPythonHighlighter }

  TTestPythonHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestDecorator;
    procedure TestRawString;
    procedure TestMatrixOperator;
  end;

implementation

class function TTestPythonHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('python py',
    'def f(x: int):' + #10 + '    return x + 42',
    'def', '"def #"', '# def');
end;


procedure TTestPythonHighlighter.TestDecorator;

begin
  AssertRole('python', '@pkg.decorator' + #10 + 'def f(): pass', '@pkg.decorator', shAttribute);
end;


procedure TTestPythonHighlighter.TestRawString;

begin
  AssertRole('python', 'r"""def #' + #10 + 'text"""', 'def', shRawString);
end;


procedure TTestPythonHighlighter.TestMatrixOperator;

begin
  AssertRole('python', 'x @ matrix', '@', shOperator);
end;

initialization
  RegisterTest(TTestPythonHighlighter);
end.
