{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    TypeScript highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.typescript;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.typescript, unittest.syntaxfixture;

type

  { TTestTypeScriptHighlighter }

  TTestTypeScriptHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestTemplate;
  end;

implementation

class function TTestTypeScriptHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('typescript ts tsx',
    'interface X { value: number }; const x = 42;',
    'interface', '"interface //"', '// interface');
end;


procedure TTestTypeScriptHighlighter.TestTemplate;

begin
  AssertRole('typescript', '`interface // ${x}`', 'interface', shTemplate);
end;

initialization
  RegisterTest(TTestTypeScriptHighlighter);
end.
