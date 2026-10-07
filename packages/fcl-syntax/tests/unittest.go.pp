{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Go highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.go;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.go;

type
  TTestGoHighlighter = class(TTestCase)
  private
    FTokens: TSyntaxTokenArray;
    function DoGoHighlighting(const source: string): TSyntaxTokenArray;
    function HasToken(const aText: string; aKind: TSyntaxHighlightKind): Boolean;
    function KindOf(const aText: string): TSyntaxHighlightKind;
    function TokenCount(aKind: TSyntaxHighlightKind): Integer;
  published
    procedure TestKeywords;
    procedure TestBuiltins;
    procedure TestIdentifierIsNotKeyword;
    procedure TestInterpretedString;
    procedure TestStringEscapes;
    procedure TestRawString;
    procedure TestRuneLiteral;
    procedure TestLineComment;
    procedure TestBlockComment;
    procedure TestBuildDirective;
    procedure TestNumbers;
    procedure TestNumberSeparators;
    procedure TestImaginary;
    procedure TestOperators;
    procedure TestEveryOperatorForm;
    procedure TestTypeConstraintTilde;
    procedure TestShortVariableDeclaration;
    procedure TestChannelOperator;
    procedure TestSymbols;
    procedure TestUnicodeIdentifier;
    procedure TestEmpty;
    procedure TestFunction;
  end;

implementation

function TTestGoHighlighter.DoGoHighlighting(const source: string): TSyntaxTokenArray;
var
  highlighter: TGoSyntaxHighlighter;
begin
  highlighter := TGoSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(source);
    FTokens := Result;
  finally
    highlighter.Free;
  end;
end;


function TTestGoHighlighter.HasToken(const aText: string; aKind: TSyntaxHighlightKind): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(FTokens) do
    if (FTokens[i].Text = aText) and (FTokens[i].Kind = aKind) then
      Exit(True);
end;


function TTestGoHighlighter.KindOf(const aText: string): TSyntaxHighlightKind;
var
  i: Integer;
begin
  Result := shInvalid;
  for i := 0 to High(FTokens) do
    if FTokens[i].Text = aText then
      Exit(FTokens[i].Kind);
end;


function TTestGoHighlighter.TokenCount(aKind: TSyntaxHighlightKind): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(FTokens) do
    if FTokens[i].Kind = aKind then
      Inc(Result);
end;


procedure TTestGoHighlighter.TestKeywords;
begin
  DoGoHighlighting('package main');
  AssertTrue('package is a keyword', HasToken('package', shKeyword));
  DoGoHighlighting('for i := range x');
  AssertTrue('for is a keyword', HasToken('for', shKeyword));
  AssertTrue('range is a keyword', HasToken('range', shKeyword));
  DoGoHighlighting('go func() {}');
  AssertTrue('go is a keyword', HasToken('go', shKeyword));
  DoGoHighlighting('fallthrough');
  AssertTrue('fallthrough is a keyword', HasToken('fallthrough', shKeyword));
end;


procedure TTestGoHighlighter.TestBuiltins;
begin
  DoGoHighlighting('var x int = len(s)');
  AssertTrue('int is predeclared', HasToken('int', shKeyword));
  AssertTrue('len is predeclared', HasToken('len', shKeyword));
  DoGoHighlighting('if err != nil');
  AssertTrue('nil is predeclared', HasToken('nil', shKeyword));
  DoGoHighlighting('b := true');
  AssertTrue('true is predeclared', HasToken('true', shKeyword));
end;


procedure TTestGoHighlighter.TestIdentifierIsNotKeyword;
begin
  DoGoHighlighting('format');
  AssertEquals('format is an identifier', Ord(shDefault), Ord(KindOf('format')));
  DoGoHighlighting('mapping');
  AssertEquals('mapping is an identifier', Ord(shDefault), Ord(KindOf('mapping')));
  DoGoHighlighting('intValue');
  AssertEquals('intValue is an identifier', Ord(shDefault), Ord(KindOf('intValue')));
end;


procedure TTestGoHighlighter.TestInterpretedString;
begin
  DoGoHighlighting('s := "hello"');
  AssertTrue('string literal', HasToken('"hello"', shStrings));
end;


procedure TTestGoHighlighter.TestStringEscapes;
begin
  DoGoHighlighting('s := "a\"b"');
  AssertTrue('escaped quote stays inside the literal', HasToken('"a\"b"', shStrings));
end;


procedure TTestGoHighlighter.TestRawString;
begin
  DoGoHighlighting('s := `line'#10'next`');
  AssertTrue('raw string spans lines', HasToken('`line'#10'next`', shRawString));
end;


procedure TTestGoHighlighter.TestRuneLiteral;
begin
  DoGoHighlighting('r := ''a''');
  AssertTrue('rune literal', HasToken('''a''', shCharacters));
  DoGoHighlighting('r := ''\n''');
  AssertTrue('escaped rune literal', HasToken('''\n''', shCharacters));
end;


procedure TTestGoHighlighter.TestLineComment;
begin
  DoGoHighlighting('x := 1 // count');
  AssertTrue('line comment', HasToken('// count', shComment));
end;


procedure TTestGoHighlighter.TestBlockComment;
begin
  DoGoHighlighting('/* a'#10'b */ x');
  AssertTrue('block comment spans lines', HasToken('/* a'#10'b */', shComment));
end;


procedure TTestGoHighlighter.TestBuildDirective;
begin
  DoGoHighlighting('//go:build linux');
  AssertTrue('go directive', HasToken('//go:build linux', shDirective));
  DoGoHighlighting('// +build linux');
  AssertTrue('legacy build tag', HasToken('// +build linux', shDirective));
  DoGoHighlighting('// gopher');
  AssertTrue('ordinary comment', HasToken('// gopher', shComment));
end;


procedure TTestGoHighlighter.TestNumbers;
begin
  DoGoHighlighting('x := 42');
  AssertTrue('decimal', HasToken('42', shNumbers));
  DoGoHighlighting('x := 0xFF');
  AssertTrue('hexadecimal', HasToken('0xFF', shNumbers));
  DoGoHighlighting('x := 0b1010');
  AssertTrue('binary', HasToken('0b1010', shNumbers));
  DoGoHighlighting('x := 0o755');
  AssertTrue('octal', HasToken('0o755', shNumbers));
  DoGoHighlighting('x := 3.14e-2');
  AssertTrue('float with exponent', HasToken('3.14e-2', shNumbers));
end;


procedure TTestGoHighlighter.TestNumberSeparators;
begin
  DoGoHighlighting('x := 1_000_000');
  AssertTrue('digit separators belong to the number', HasToken('1_000_000', shNumbers));
end;


procedure TTestGoHighlighter.TestImaginary;
begin
  DoGoHighlighting('x := 3i');
  AssertTrue('imaginary literal', HasToken('3i', shNumbers));
end;


procedure TTestGoHighlighter.TestOperators;
begin
  DoGoHighlighting('a &^= b');
  AssertTrue('and not assign', HasToken('&^=', shOperator));
  DoGoHighlighting('a << 2');
  AssertTrue('shift left', HasToken('<<', shOperator));
  DoGoHighlighting('a ... b');
  AssertTrue('ellipsis', HasToken('...', shOperator));
  DoGoHighlighting('a != b');
  AssertTrue('not equal', HasToken('!=', shOperator));
end;


procedure TTestGoHighlighter.TestEveryOperatorForm;
const
  Operators: array[0..37] of string = (
    '+', '-', '*', '/', '%', '&', '|', '^', '<<', '>>',
    '&^', '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=',
    '>>=', '&^=', '&&', '||', '<-', '++', '--', '==', '<', '>',
    '=', '!', '!=', '<=', '>=', ':=', '...', '~');
var
  i: Integer;
  lSource: string;
begin
  for i := 0 to High(Operators) do
    begin
    lSource := 'a ' + Operators[i] + ' b';
    DoGoHighlighting(lSource);
    AssertTrue(Operators[i] + ' is one operator token in ' + lSource,
               HasToken(Operators[i], shOperator));
    end;
end;


procedure TTestGoHighlighter.TestTypeConstraintTilde;
begin
  DoGoHighlighting('type Number interface { ~int | ~float64 }');
  AssertTrue('tilde is an operator', HasToken('~', shOperator));
  AssertEquals('nothing is invalid', 0, TokenCount(shInvalid));
end;


procedure TTestGoHighlighter.TestShortVariableDeclaration;
begin
  DoGoHighlighting('x := 1');
  AssertTrue('short declaration is one operator', HasToken(':=', shOperator));
end;


procedure TTestGoHighlighter.TestChannelOperator;
begin
  DoGoHighlighting('v := <-ch');
  AssertTrue('receive operator', HasToken('<-', shOperator));
end;


procedure TTestGoHighlighter.TestSymbols;
begin
  DoGoHighlighting('f(a, b)');
  AssertTrue('open parenthesis', HasToken('(', shSymbol));
  AssertTrue('comma', HasToken(',', shSymbol));
end;


procedure TTestGoHighlighter.TestUnicodeIdentifier;
begin
  DoGoHighlighting('héllo := 1');
  AssertTrue('accented identifier stays whole', HasToken('héllo', shDefault));
end;


procedure TTestGoHighlighter.TestEmpty;
begin
  AssertEquals('empty source gives no tokens', 0, Length(DoGoHighlighting('')));
end;


procedure TTestGoHighlighter.TestFunction;
begin
  DoGoHighlighting('func add(a, b int) int { return a + b }');
  AssertTrue('func', HasToken('func', shKeyword));
  AssertTrue('return', HasToken('return', shKeyword));
  AssertTrue('int', HasToken('int', shKeyword));
  AssertTrue('name is an identifier', KindOf('add') = shDefault);
end;

initialization
  RegisterTest(TTestGoHighlighter);
end.
