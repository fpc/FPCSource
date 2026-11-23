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
    procedure TestHeredoc;
    procedure TestQuotedHeredocAfterSpace;
    procedure TestShiftByNumber;
    procedure TestShiftByVariable;
    procedure TestShiftBeforeIdentifier;
    procedure TestUnterminatedHeredoc;
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


procedure TTestPerlHighlighter.TestHeredoc;

begin
  AssertRole('perl', 'print <<EOT;' + #10 + 'my #' + #10 + 'EOT' + #10 + 'my $x;', 'my', shStrings);
  AssertRole('perl', 'print <<EOT;' + #10 + 'my #' + #10 + 'EOT' + #10 + 'my $x;', 'my', shKeyword, 2);
end;


procedure TTestPerlHighlighter.TestQuotedHeredocAfterSpace;

begin
  AssertRole('perl', 'print << "EOT";' + #10 + 'my #' + #10 + 'EOT' + #10, 'my', shStrings);
end;


procedure TTestPerlHighlighter.TestShiftByNumber;

begin
  AssertRole('perl', 'my $mask = 1 << 4;' + #10 + 'my $other = 2;', 'my', shKeyword, 2);
  AssertRole('perl', 'my $mask = 1 <<4;' + #10 + 'my $other = 2;', 'my', shKeyword, 2);
  AssertToken('perl', 'my $mask = 1 << 4;', '4', shNumbers);
end;


procedure TTestPerlHighlighter.TestShiftByVariable;

begin
  AssertRole('perl', 'my $y = $x << $n;' + #10 + 'my $z;', 'my', shKeyword, 2);
end;


procedure TTestPerlHighlighter.TestShiftBeforeIdentifier;

begin
  AssertRole('perl', 'my $y = 1 << length;' + #10 + 'my $z;', 'my', shKeyword, 2);
end;


procedure TTestPerlHighlighter.TestUnterminatedHeredoc;

begin
  AssertRole('perl', 'print <<EOT;' + #10 + 'my $x;', 'my', shKeyword);
end;

initialization
  RegisterTest(TTestPerlHighlighter);
end.
