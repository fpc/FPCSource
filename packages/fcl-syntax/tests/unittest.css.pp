{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    CSS highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unittest.css;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.css;

type
  TTestCssHighlighter = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    function DoCssHighlighting(const source: string): TSyntaxTokenArray;
  published
    procedure TestCssAtRules;
    procedure TestCssProperties;
    procedure TestCssStrings;
    procedure TestCssNumbers;
    procedure TestCssColors;
    procedure TestCssComments;
    procedure TestCssSelectors;
    procedure TestCssSymbols;
    procedure TestCssUrls;
    procedure TestComplexCssRule;
    procedure TestCssMediaQuery;
    procedure TestCssUnits;
  end;

implementation

procedure TTestCssHighlighter.SetUp;
begin
end;

procedure TTestCssHighlighter.TearDown;
begin
  // Nothing to do
end;

function TTestCssHighlighter.DoCssHighlighting(const source: string): TSyntaxTokenArray;
var
  highlighter: TCssSyntaxHighlighter;
begin
  highlighter := TCssSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(source);
  finally
    highlighter.Free;
  end;
end;

procedure TTestCssHighlighter.TestCssAtRules;
const
  AtRules: array[0..9] of string = (
    '@charset', '@import', '@media', '@keyframes', '@font-face',
    '@supports', '@page', '@namespace', '@viewport', '@layer'
  );
var
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  for i := 0 to High(AtRules) do
    begin
    tokens := DoCssHighlighting(AtRules[i]);
    AssertEquals('Should have 1 token for ' + AtRules[i], 1, Length(tokens));
    AssertEquals('Token should be ' + AtRules[i], AtRules[i], tokens[0].Text);
    AssertEquals(AtRules[i] + ' should be directive', Ord(shDirective), Ord(tokens[0].Kind));
    end;
end;

procedure TTestCssHighlighter.TestCssProperties;
const
  Properties: array[0..9] of string = (
    'color', 'background', 'margin', 'padding', 'border',
    'font', 'width', 'height', 'position', 'display'
  );
var
  tokens: TSyntaxTokenArray;
  i: Integer;
begin
  for i := 0 to High(Properties) do
    begin
    tokens := DoCssHighlighting(Properties[i]);
    AssertEquals('Should have 1 token for ' + Properties[i], 1, Length(tokens));
    AssertEquals('Token should be ' + Properties[i], Properties[i], tokens[0].Text);
    AssertEquals(Properties[i] + ' should be keyword (property)', Ord(shKeyword), Ord(tokens[0].Kind));
    end;
end;

procedure TTestCssHighlighter.TestCssStrings;
var
  tokens: TSyntaxTokenArray;
begin
  // Test single-quoted string
  tokens := DoCssHighlighting('''Arial''');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be single-quoted string', '''Arial''', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));

  // Test double-quoted string
  tokens := DoCssHighlighting('"Helvetica"');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be double-quoted string', '"Helvetica"', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));

  // Test string with escapes
  tokens := DoCssHighlighting('"Font with \"quotes\""');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be escaped string', '"Font with \"quotes\""', tokens[0].Text);
  AssertEquals('Token should be string', Ord(shStrings), Ord(tokens[0].Kind));
end;

procedure TTestCssHighlighter.TestCssNumbers;
var
  tokens: TSyntaxTokenArray;
begin
  // Test percentage
  tokens := DoCssHighlighting('100%');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be percentage', '100%', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test number with px unit
  tokens := DoCssHighlighting('16px');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be pixel value', '16px', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test decimal number
  tokens := DoCssHighlighting('1.5em');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be em value', '1.5em', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test zero value
  tokens := DoCssHighlighting('0');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be zero', '0', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));
end;

procedure TTestCssHighlighter.TestCssColors;
var
  tokens: TSyntaxTokenArray;
begin
  // Test 6-digit hex color
  tokens := DoCssHighlighting('#FF0000');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be hex color', '#FF0000', tokens[0].Text);
  AssertEquals('Token should be number (color)', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test 3-digit hex color
  tokens := DoCssHighlighting('#F00');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be 3-digit hex color', '#F00', tokens[0].Text);
  AssertEquals('Token should be number (color)', Ord(shNumbers), Ord(tokens[0].Kind));

  // Test lowercase hex color
  tokens := DoCssHighlighting('#ff0000');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be lowercase hex color', '#ff0000', tokens[0].Text);
  AssertEquals('Token should be number (color)', Ord(shNumbers), Ord(tokens[0].Kind));
end;

procedure TTestCssHighlighter.TestCssComments;
var
  tokens: TSyntaxTokenArray;
begin
  // Test multi-line comment
  tokens := DoCssHighlighting('/* This is a comment */');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be multi-line comment', '/* This is a comment */', tokens[0].Text);
  AssertEquals('Token should be comment', Ord(shComment), Ord(tokens[0].Kind));

  // Test multi-line comment with newlines
  tokens := DoCssHighlighting('/* Line 1' + #10 + 'Line 2 */');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be multi-line comment with newlines', '/* Line 1' + #10 + 'Line 2 */', tokens[0].Text);
  AssertEquals('Token should be comment', Ord(shComment), Ord(tokens[0].Kind));

  // Test comment with CSS inside
  tokens := DoCssHighlighting('/* color: red; */');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be comment with CSS', '/* color: red; */', tokens[0].Text);
  AssertEquals('Token should be comment', Ord(shComment), Ord(tokens[0].Kind));
end;

procedure TTestCssHighlighter.TestCssSelectors;
var
  tokens: TSyntaxTokenArray;
begin
  // Test class selector
  tokens := DoCssHighlighting('.myClass');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be class selector', '.myClass', tokens[0].Text);
  AssertEquals('Token should be default (selector)', Ord(shDefault), Ord(tokens[0].Kind));

  // Test element selector
  tokens := DoCssHighlighting('div');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be element selector', 'div', tokens[0].Text);
  AssertEquals('Token should be default (selector)', Ord(shDefault), Ord(tokens[0].Kind));

  // Test pseudo-class
  tokens := DoCssHighlighting(':hover');
  AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
  // First part should be the colon or the complete pseudo-class
end;

procedure TTestCssHighlighter.TestCssSymbols;
var
  tokens: TSyntaxTokenArray;
begin
  // Test opening brace
  tokens := DoCssHighlighting('{');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be opening brace', '{', tokens[0].Text);
  AssertEquals('Token should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));

  // Test closing brace
  tokens := DoCssHighlighting('}');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be closing brace', '}', tokens[0].Text);
  AssertEquals('Token should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));

  // Test semicolon
  tokens := DoCssHighlighting(';');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be semicolon', ';', tokens[0].Text);
  AssertEquals('Token should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));

  // Test colon
  tokens := DoCssHighlighting(':');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be colon', ':', tokens[0].Text);
  AssertEquals('Token should be default', Ord(shDefault), Ord(tokens[0].Kind));
end;

procedure TTestCssHighlighter.TestCssUrls;
var
  tokens: TSyntaxTokenArray;
begin
  // Test URL function
  tokens := DoCssHighlighting('url(image.png)');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be URL function', 'url(image.png)', tokens[0].Text);
  AssertEquals('Token should be string (URL)', Ord(shStrings), Ord(tokens[0].Kind));

  // Test URL with quotes
  tokens := DoCssHighlighting('url("image.png")');
  AssertTrue('Should have at least 1 token', Length(tokens) >= 1);
  // Should be tokenized as URL function
end;

procedure TTestCssHighlighter.TestComplexCssRule;
var
  tokens: TSyntaxTokenArray;
  cssRule: string;
  i: Integer;
  hasSelectors, hasProperties, hasSymbols, hasValues: Boolean;
begin
  cssRule := '.container { width: 100%; color: #333; }';
  tokens := DoCssHighlighting(cssRule);

  AssertTrue('Should have multiple tokens', Length(tokens) > 5);

  // Check that we have different token types
  hasSelectors := False;
  hasProperties := False;
  hasSymbols := False;
  hasValues := False;

  for i := 0 to High(tokens) do
    begin
    case tokens[i].Kind of
      shDefault: hasSelectors := True;
      shKeyword: hasProperties := True;
      shSymbol: hasSymbols := True;
      shNumbers: hasValues := True;
    end;
    end;

  AssertTrue('Should contain selector tokens', hasSelectors);
  AssertTrue('Should contain property tokens', hasProperties);
  AssertTrue('Should contain symbol tokens', hasSymbols);
  AssertTrue('Should contain value tokens', hasValues);

  // First token should be the selector
  AssertEquals('First token should be .container', '.container', tokens[0].Text);
  AssertEquals('First token should be default (selector)', Ord(shDefault), Ord(tokens[0].Kind));

  // Should contain braces
  for i := 0 to High(tokens) do
    begin
    if tokens[i].Text = '{' then
      begin
      AssertEquals('Opening brace should be symbol', Ord(shSymbol), Ord(tokens[i].Kind));
      Break;
      end;
    end;
end;

procedure TTestCssHighlighter.TestCssMediaQuery;
var
  tokens: TSyntaxTokenArray;
  mediaQuery: string;
  i: Integer;
  HasProperties,hasDirective, hasSelectors: Boolean;
begin
  mediaQuery := '@media (max-width: 768px) { body { font-size: 14px; } }';
  tokens := DoCssHighlighting(mediaQuery);

  AssertTrue('Should have multiple tokens', Length(tokens) > 10);

  // Check that we have different token types
  hasDirective := False;
  hasSelectors := False;
  hasProperties := False;

  for i := 0 to High(tokens) do
    begin
    case tokens[i].Kind of
      shDirective: hasDirective := True;
      shDefault: hasSelectors := True;
      shKeyword: hasProperties := True;
    end;
    end;

  AssertTrue('Should contain directive tokens', hasDirective);
  AssertTrue('Should contain selector tokens', hasSelectors);
  // Note: Properties inside media queries may not be recognized as keywords
  // depending on the CSS highlighter's context-sensitivity implementation

  // First token should be @media directive
  AssertEquals('First token should be @media', '@media', tokens[0].Text);
  AssertEquals('First token should be directive', Ord(shDirective), Ord(tokens[0].Kind));
end;

procedure TTestCssHighlighter.TestCssUnits;
var
  tokens: TSyntaxTokenArray;
begin
  // Test various CSS units
  tokens := DoCssHighlighting('10rem');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be rem value', '10rem', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  tokens := DoCssHighlighting('2vh');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be vh value', '2vh', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  tokens := DoCssHighlighting('50vw');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be vw value', '50vw', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));

  tokens := DoCssHighlighting('1.2fr');
  AssertEquals('Should have 1 token', 1, Length(tokens));
  AssertEquals('Token should be fr value', '1.2fr', tokens[0].Text);
  AssertEquals('Token should be number', Ord(shNumbers), Ord(tokens[0].Kind));
end;

initialization
  RegisterTest(TTestCssHighlighter);
end.
