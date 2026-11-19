{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Dockerfile highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.dockerfile;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.dockerfile, unittest.syntaxfixture;

type

  { TTestDockerfileHighlighter }

  TTestDockerfileHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestInstructionAtLineStart;
  end;

implementation

class function TTestDockerfileHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('dockerfile',
    'FROM example' + #10 + 'RUN echo hello',
    'FROM', '"FROM #"', '# FROM');
end;


procedure TTestDockerfileHighlighter.TestInstructionAtLineStart;

begin
  AssertRole('dockerfile', 'RUN echo FROM', 'FROM', shDefault);
  AssertRole('dockerfile', 'RUN echo FROM', 'RUN', shKeyword);
end;

initialization
  RegisterTest(TTestDockerfileHighlighter);
end.
