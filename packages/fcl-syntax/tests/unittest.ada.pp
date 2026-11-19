{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Ada highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.ada;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.ada, unittest.syntaxfixture;

type

  { TTestAdaHighlighter }

  TTestAdaHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestAttribute;
    procedure TestBasedNumber;
  end;

implementation

class function TTestAdaHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('ada',
    'procedure Example is begin null; end Example;',
    'procedure', '"procedure --"', '-- procedure');
end;


procedure TTestAdaHighlighter.TestAttribute;

begin
  AssertRole('ada', 'X''First + ''A''', 'First', shDefault);
  AssertRole('ada', 'X''First + ''A''', '''A''', shCharacters);
end;


procedure TTestAdaHighlighter.TestBasedNumber;

begin
  AssertRole('ada', 'X := 16#ff#;', '16#ff#', shNumbers);
end;

initialization
  RegisterTest(TTestAdaHighlighter);
end.
