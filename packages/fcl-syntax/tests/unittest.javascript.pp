{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Javascript highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit unittest.javascript;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.javascript;

type
  TTestJavaScriptHighlighter = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    function DoJavaScriptHighlighting(const source: string): TSyntaxTokenArray;
  published
    procedure TestJavaScriptKeywords;
    procedure TestJavaScriptStrings;
    procedure TestJavaScriptNumbers;
    procedure TestJavaScriptComments;
    procedure TestJavaScriptOperators;
    procedure TestJavaScriptSymbols;
    procedure TestJavaScriptIdentifiers;
    procedure TestJavaScriptRegexLiterals;
    procedure TestJavaScriptTemplateLiterals;
    procedure TestJavaScriptNumericFormats;
    procedure TestComplexJavaScriptFunction;
    procedure TestJavaScriptContextSensitive;
  end;

implementation

procedure TTestJavaScriptHighlighter.SetUp;
begin
end;

procedure TTestJavaScriptHighlighter.TearDown;
begin
  // Nothing to do
end;

function TTestJavaScriptHighlighter.DoJavaScriptHighlighting(const source: string): TSyntaxTokenArray;
var
  highlighter: TJavaScriptSyntaxHighlighter;
begin
  highlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(source);
  finally
    highlighter.Free;
  end;
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptKeywords;
const
  Keywords: array[0..19] of string = (
    'var', 'let', 'const', 'function', 'if', 'else', 'for', 'while', 'do', 'switch',
    'case', 'default', 'break', 'continue', 'return', 'try', 'catch', 'finally', 'throw', 'new'
  );
var
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  for i := 0 to High(Keywords) do
    begin
    tokens := DoJavaScriptHighlighting(Keywords[i]);
    AssertEquals('Should have 1 token for ' + Keywords[i], 1, Length(tokens));
    AssertEquals('Token should be ' + Keywords[i], Keywords[i], tokens[0].Text);
    AssertEquals(Keywords[i] + ' should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));
    end;
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptStrings;
var
  tokens: TSyntaxTokenArray;
begin
  // Test single-quoted string
  tokens := DoJavaScriptHighlighting('''hello world''');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be single-quoted string', '''hello world''', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));

  // Test double-quoted string
  tokens := DoJavaScriptHighlighting('"hello world"');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be double-quoted string', '"hello world"', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));

  // Test string with escapes
  tokens := DoJavaScriptHighlighting('"hello\nworld"');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be escaped string', '"hello\nworld"', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptNumbers;
var
  tokens: TSyntaxTokenArray;
begin
  // Test integer
  tokens := DoJavaScriptHighlighting('123');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be integer', '123', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test decimal
  tokens := DoJavaScriptHighlighting('123.45');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be decimal', '123.45', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test hex number
  tokens := DoJavaScriptHighlighting('0xFF');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be hex', '0xFF', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptComments;
var
  tokens: TSyntaxTokenArray;
begin
  // Test single-line comment
  tokens := DoJavaScriptHighlighting('// This is a comment');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be single-line comment', '// This is a comment', tokens[0].Text);
  AssertEquals('Token should be comment', Ord(shComment), Ord(tokens[0].Kind));

  // Test multi-line comment
  tokens := DoJavaScriptHighlighting('/* This is a comment */');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be multi-line comment', '/* This is a comment */', tokens[0].Text);
  AssertEquals('Token should be comment', Ord(shComment), Ord(tokens[0].Kind));

  // Test multi-line comment with newlines
  tokens := DoJavaScriptHighlighting('/* Line 1' + #10 + 'Line 2 */');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be multi-line comment with newlines', '/* Line 1' + #10 + 'Line 2 */', tokens[0].Text);
  AssertEquals('Token should be comment', Ord(shComment), Ord(tokens[0].Kind));
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptOperators;
var
  tokens: TSyntaxTokenArray;
begin
  // Test strict equality
  tokens := DoJavaScriptHighlighting('===');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be strict equality', '===', tokens[0].Text);
  AssertEquals('Token should be operator', Ord(shOperator), Ord(tokens[0].Kind));

  // Test inequality
  tokens := DoJavaScriptHighlighting('!==');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be strict inequality', '!==', tokens[0].Text);
  AssertEquals('Token should be operator', Ord(shOperator), Ord(tokens[0].Kind));

  // Test arrow function
  tokens := DoJavaScriptHighlighting('=>');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be arrow operator', '=>', tokens[0].Text);
  AssertEquals('Token should be operator', Ord(shOperator), Ord(tokens[0].Kind));

  // Test logical AND
  tokens := DoJavaScriptHighlighting('&&');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be logical AND', '&&', tokens[0].Text);
  AssertEquals('Token should be operator', Ord(shOperator), Ord(tokens[0].Kind));
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptSymbols;
var
  tokens: TSyntaxTokenArray;
begin
  // Test parentheses
  tokens := DoJavaScriptHighlighting('(');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be opening paren', '(', tokens[0].Text);
  AssertEquals('Token should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));

  // Test braces
  tokens := DoJavaScriptHighlighting('{');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be opening brace', '{', tokens[0].Text);
  AssertEquals('Token should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));

  // Test semicolon
  tokens := DoJavaScriptHighlighting(';');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be semicolon', ';', tokens[0].Text);
  AssertEquals('Token should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));

  // Test comma
  tokens := DoJavaScriptHighlighting(',');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be comma', ',', tokens[0].Text);
  AssertEquals('Token should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptIdentifiers;
var
  tokens: TSyntaxTokenArray;
begin
  // Test simple identifier
  tokens := DoJavaScriptHighlighting('myVariable');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be identifier', 'myVariable', tokens[0].Text);
  AssertEquals('Token should be default', Ord(shDefault), Ord(tokens[0].Kind));

  // Test identifier with underscore
  tokens := DoJavaScriptHighlighting('_private');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be underscore identifier', '_private', tokens[0].Text);
  AssertEquals('Token should be default', Ord(shDefault), Ord(tokens[0].Kind));

  // Test identifier with dollar sign
  tokens := DoJavaScriptHighlighting('$element');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be dollar identifier', '$element', tokens[0].Text);
  AssertEquals('Token should be default', Ord(shDefault), Ord(tokens[0].Kind));
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptRegexLiterals;
var
  tokens: TSyntaxTokenArray;
begin
  // Test simple regex
  tokens := DoJavaScriptHighlighting('/[a-z]+/gi');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be regex', '/[a-z]+/gi', tokens[0].Text);
  AssertEquals('Token should be regex type', Ord(shRegex), Ord(tokens[0].Kind));

  // Test regex with escape sequences
  tokens := DoJavaScriptHighlighting('/\\d+\\.\\d*/');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be escaped regex', '/\\d+\\.\\d*/', tokens[0].Text);
  AssertEquals('Token should be regex type', Ord(shRegex), Ord(tokens[0].Kind));
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptTemplateLiterals;
var
  tokens: TSyntaxTokenArray;
begin
  // Test simple template literal
  tokens := DoJavaScriptHighlighting('`hello world`');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be template literal', '`hello world`', tokens[0].Text);
  AssertEquals('Token should be raw string type', Ord(shRawString), Ord(tokens[0].Kind));

  // Test template literal with interpolation
  tokens := DoJavaScriptHighlighting('`hello ${name}`');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be template with interpolation', '`hello ${name}`', tokens[0].Text);
  AssertEquals('Token should be raw string type', Ord(shRawString), Ord(tokens[0].Kind));
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptNumericFormats;
var
  tokens: TSyntaxTokenArray;
begin
  // Test scientific notation
  tokens := DoJavaScriptHighlighting('1.23e-4');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be scientific notation', '1.23e-4', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test binary number
  tokens := DoJavaScriptHighlighting('0b1010');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be binary number', '0b1010', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test octal number
  tokens := DoJavaScriptHighlighting('0o755');
  AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
  AssertEquals('First token should be number type', Ord(shNumbers), Ord(tokens[0].Kind));
end;

procedure TTestJavaScriptHighlighter.TestComplexJavaScriptFunction;
var
  tokens: TSyntaxTokenArray;
  jsCode: string;
  i: Integer;
  hasKeywords, hasStrings, hasSymbols, hasIdentifiers: Boolean;
begin
  jsCode := 'function greet(name) { return `Hello ${name}!`; }';
  tokens := DoJavaScriptHighlighting(jsCode);

  AssertTrue('Should have multiple tokens', Length(tokens) > 5);

  // Check that we have different token types
  hasKeywords := False;
  hasStrings := False;
  hasSymbols := False;
  hasIdentifiers := False;

  for i := 0 to High(tokens) do
    case tokens[i].Kind of
      shKeyword: hasKeywords := True;
      shRawString: hasStrings := True;
      shSymbol: hasSymbols := True;
      shDefault: hasIdentifiers := True;
    end;


  AssertTrue('Should contain keyword tokens', hasKeywords);
  AssertTrue('Should contain string tokens', hasStrings);
  AssertTrue('Should contain symbol tokens', hasSymbols);
  AssertTrue('Should contain identifier tokens', hasIdentifiers);

  // First token should be 'function' keyword
  AssertEquals('First token should be function', 'function', tokens[0].Text);
  AssertEquals('First token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));

  // Should contain template literal
  for i := 0 to High(tokens) do
    if tokens[i].Kind = shRawString then
      begin
      AssertEquals('Should have template literal', '`Hello ${name}!`', tokens[i].Text);
      Break;
      end;
end;

procedure TTestJavaScriptHighlighter.TestJavaScriptContextSensitive;
var
  tokens: TSyntaxTokenArray;
begin
  // Test that context-sensitive features work at least partially
  // Note: Full context sensitivity for regex vs division is complex
  // and may not be fully implemented in all cases

  // Test assignment context - this should work well
  tokens := DoJavaScriptHighlighting('var x = 42;');
  AssertTrue('Should have multiple tokens', Length(tokens) >= 5);

  // Should have var keyword
  AssertEquals('First token should be var', 'var', tokens[0].Text);
  AssertEquals('First token should be keyword', Ord(shKeyword), Ord(tokens[0].Kind));

  // Should have identifier
  AssertEquals('Second token should be space', ' ', tokens[1].Text);
  AssertEquals('Third token should be identifier', 'x', tokens[2].Text);
  AssertEquals('Third token should be default', Ord(shDefault), Ord(tokens[2].Kind));

  // Should have assignment and number
  AssertEquals('Fourth token should be space', ' ', tokens[3].Text);
  AssertEquals('Fifth token should be assignment', '=', tokens[4].Text);
  AssertEquals('Fifth token should be operator', Ord(shOperator), Ord(tokens[4].Kind));
end;

initialization
  RegisterTest(TTestJavaScriptHighlighter);
end.
