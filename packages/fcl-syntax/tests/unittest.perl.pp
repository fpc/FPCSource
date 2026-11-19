{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Perl highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.perl;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.perl, unittest.syntaxfixture;

type

  { TTestPerlHighlighter }

  TTestPerlHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestQuoteOperator;
  end;

implementation

class function TTestPerlHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('perl pl',
    'sub f { my $x = 42; return $x; }',
    'sub', '"sub #"', '# sub');
end;


procedure TTestPerlHighlighter.TestQuoteOperator;

begin
  AssertRole('perl', 'qq{my # {nested}}', 'my', shStrings);
end;

initialization
  RegisterTest(TTestPerlHighlighter);
end.
