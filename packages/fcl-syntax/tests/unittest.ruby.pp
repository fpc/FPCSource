{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Ruby highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.ruby;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.ruby, unittest.syntaxfixture;

type

  { TTestRubyHighlighter }

  TTestRubyHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestPercentLiteral;
    procedure TestHeredoc;
    procedure TestPlainHeredoc;
    procedure TestDashHeredoc;
    procedure TestAppendOperator;
    procedure TestShiftByNumber;
    procedure TestUnterminatedHeredoc;
  end;

implementation

class function TTestRubyHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('ruby rb',
    'def f(x)' + #10 + '  return x + 42' + #10 + 'end',
    'def', '"def #"', '# def');
end;


procedure TTestRubyHighlighter.TestPercentLiteral;

begin
  AssertRole('ruby', '%q{def # {nested}}', 'def', shStrings);
end;


procedure TTestRubyHighlighter.TestHeredoc;

begin
  AssertRole('ruby', '<<~TEXT' + #10 + 'def #' + #10 + 'TEXT', 'def', shStrings);
end;


procedure TTestRubyHighlighter.TestPlainHeredoc;

begin
  AssertRole('ruby', 'x = <<TEXT' + #10 + 'def #' + #10 + 'TEXT' + #10 + 'def f', 'def', shStrings);
  AssertRole('ruby', 'x = <<TEXT' + #10 + 'def #' + #10 + 'TEXT' + #10 + 'def f', 'def', shKeyword, 2);
end;


procedure TTestRubyHighlighter.TestDashHeredoc;

begin
  AssertRole('ruby', 'x = <<-TEXT' + #10 + 'def #' + #10 + '  TEXT' + #10 + 'def f', 'def', shStrings);
  AssertRole('ruby', 'x = <<-TEXT' + #10 + 'def #' + #10 + '  TEXT' + #10 + 'def f', 'def', shKeyword, 2);
end;


procedure TTestRubyHighlighter.TestAppendOperator;

begin
  AssertRole('ruby', 'a = b << c' + #10 + 'def f', 'def', shKeyword);
end;


procedure TTestRubyHighlighter.TestShiftByNumber;

begin
  AssertRole('ruby', 'a = 1 << 4' + #10 + 'def f', 'def', shKeyword);
  AssertRole('ruby', 'a = 1 <<4' + #10 + 'def f', 'def', shKeyword);
end;


procedure TTestRubyHighlighter.TestUnterminatedHeredoc;

begin
  AssertRole('ruby', 'list <<item' + #10 + 'def f', 'def', shKeyword);
end;

initialization
  RegisterTest(TTestRubyHighlighter);
end.
