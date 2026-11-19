{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Fortran highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.fortran;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.fortran, unittest.syntaxfixture;

type

  { TTestFortranHighlighter }

  TTestFortranHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestDoubleExponent;
  end;

implementation

class function TTestFortranHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('fortran',
    'program example' + #10 + 'integer :: x' + #10 + 'x = 42' + #10 + 'end program',
    'program', '"program !"', '! program');
end;


procedure TTestFortranHighlighter.TestDoubleExponent;

begin
  AssertRole('fortran', 'x = 1.0D+2 ! program', '1.0D+2', shNumbers);
end;

initialization
  RegisterTest(TTestFortranHighlighter);
end.
