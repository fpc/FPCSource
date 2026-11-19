{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    YAML highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.yaml;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.yaml, unittest.syntaxfixture;

type

  { TTestYamlHighlighter }

  TTestYamlHighlighter = class(TSyntaxFixtureTestCase)
  protected
    class function Fixture: TSyntaxFixture; override;
  published
    procedure TestBlockScalar;
    procedure TestHashInValue;
    procedure TestQuotedString;
    procedure TestComment;
    procedure TestKey;
  end;

implementation

class function TTestYamlHighlighter.Fixture: TSyntaxFixture;

begin
  Result := MakeFixture('yaml yml',
    'name: true' + #10 + 'items: [1, 2]',
    '', '"name #"', '# name');
end;


procedure TTestYamlHighlighter.TestBlockScalar;

begin
  AssertRole('yaml', 'key: |' + #10 + '  # true' + #10 + 'other: false', '# true', shStrings);
end;


procedure TTestYamlHighlighter.TestHashInValue;

begin
  AssertRole('yaml', 'url: https://example.test/#part', '#part', shStrings);
end;


procedure TTestYamlHighlighter.TestQuotedString;

begin
  AssertRole('yaml', '"true #"', 'true', shStrings);
end;


procedure TTestYamlHighlighter.TestComment;

begin
  AssertRole('yaml', '# true', 'true', shComment);
end;


procedure TTestYamlHighlighter.TestKey;

begin
  AssertRole('yaml', 'name: true', 'name', shKey);
end;

initialization
  RegisterTest(TTestYamlHighlighter);
end.
