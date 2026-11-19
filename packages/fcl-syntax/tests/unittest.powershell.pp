{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    PowerShell highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.powershell;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.powershell, unittest.syntaxfixture;

type

  { TTestPowerShellHighlighter }

  TTestPowerShellHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestHereString;
  end;

implementation

class function TTestPowerShellHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('powershell ps1',
    'function f { param($x) return $x + 42 }',
    'function', '"function #"', '# function');
end;


procedure TTestPowerShellHighlighter.TestHereString;

begin
  AssertRole('powershell', '@"' + #10 + 'function #' + #10 + '"@', 'function', shStrings);
end;

initialization
  RegisterTest(TTestPowerShellHighlighter);
end.
