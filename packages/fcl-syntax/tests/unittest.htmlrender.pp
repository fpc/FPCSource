{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    HTML renderer unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit unittest.htmlrender;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.javascript, syntax.css, syntax.html,
  syntax.htmlrender;

type
  TTestHtmlRenderer = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    renderer: THtmlSyntaxRenderer;
    function RenderToString(const tokens: TSyntaxTokenArray): string;
    function RenderToStringList(const tokens: TSyntaxTokenArray): TStringList;
  published
    procedure TestJavaScriptRendering;
    procedure TestCssRendering;
    procedure TestHtmlRendering;
    procedure TestEmbeddedHtmlRendering;
    procedure TestStringOutput;
    procedure TestSpecialCharacterEscaping;
    procedure TestEmptyTokenArray;
    procedure TestClassNameGeneration;
    procedure TestCategoryMapping;
    procedure TestHtmlEscaping;
    procedure TestTokensWithoutCategory;
    procedure TestComplexJavaScript;
    procedure TestNoDefaultSpanOption;
    procedure TestMultilineRendering;
    procedure TestPreserveLineStructureOption;
    procedure TestExtraClassesProperty;
  end;

implementation

procedure TTestHtmlRenderer.SetUp;
begin
  renderer := THtmlSyntaxRenderer.Create;
end;

procedure TTestHtmlRenderer.TearDown;
begin
  renderer.Free;
end;

function TTestHtmlRenderer.RenderToString(const tokens: TSyntaxTokenArray): string;
begin
  Result:='';
  renderer.RenderTokensToString(tokens, Result);
end;

function TTestHtmlRenderer.RenderToStringList(const tokens: TSyntaxTokenArray): TStringList;
begin
  Result := TStringList.Create;
  renderer.RenderTokens(tokens, Result);
end;

procedure TTestHtmlRenderer.TestJavaScriptRendering;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  output: string;

begin
  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    tokens := jsHighlighter.Execute( 'function test() { return "hello"; }');
    output := RenderToString(tokens);

    // Check for essential elements
    AssertTrue('Should contain function keyword', Pos('<span class="keyword keyword-javascript">function</span>', output) > 0);
    AssertTrue('Should contain string literal', Pos('<span class="strings strings-javascript">&quot;hello&quot;</span>', output) > 0);
    AssertTrue('Should contain return keyword', Pos('<span class="keyword keyword-javascript">return</span>', output) > 0);
    AssertTrue('Should contain symbols', Pos('<span class="symbol symbol-javascript">', output) > 0);

  finally
    jsHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestCssRendering;
var
  cssHighlighter: TCssSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  output: string;
begin
  cssHighlighter := TCssSyntaxHighlighter.Create;
  try
    tokens := cssHighlighter.Execute( 'body { color: #FF0000; font-size: 16px; }');
    output := RenderToString(tokens);

    // Check for essential CSS elements
    AssertTrue('Should contain color property', Pos('<span class="keyword keyword-css">color</span>', output) > 0);
    AssertTrue('Should contain hex color', Pos('<span class="numbers numbers-css">#FF0000</span>', output) > 0);
    AssertTrue('Should contain pixel value', Pos('<span class="numbers numbers-css">16px</span>', output) > 0);
    AssertTrue('Should contain CSS symbols', Pos('<span class="symbol symbol-css">{</span>', output) > 0);

  finally
    cssHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestHtmlRendering;
var
  htmlHighlighter: THtmlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  output: string;
begin
  htmlHighlighter := THtmlSyntaxHighlighter.Create;
  try
    tokens := htmlHighlighter.Execute( '<div class="container">&lt;Hello&gt;</div>');
    output := RenderToString(tokens);

    // Check for essential HTML elements
    AssertTrue('Should contain div keyword', Pos('<span class="keyword keyword-html">div</span>', output) > 0);
    AssertTrue('Should contain class attribute', Pos('<span class="default default-html default-htmlattribute">class</span>', output) > 0);
    AssertTrue('Should contain string value', Pos('<span class="strings strings-html">&quot;container&quot;</span>', output) > 0);
    AssertTrue('Should contain HTML symbols', Pos('<span class="symbol symbol-html">&lt;</span>', output) > 0);
    AssertTrue('Should contain escaped entities', Pos('<span class="escape escape-html">&amp;lt;</span>', output) > 0);

  finally
    htmlHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestEmbeddedHtmlRendering;
var
  htmlHighlighter: THtmlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  output: string;
begin
  htmlHighlighter := THtmlSyntaxHighlighter.Create;
  try
    tokens := htmlHighlighter.Execute( '<style>body { color: red; }</style><script>alert("test");</script>');
    output := RenderToString(tokens);

    // Check for embedded CSS
    // Writeln('output ',output);
    AssertTrue('Should contain CSS color property', Pos('<span class="keyword keyword-embeddedcss">color</span>', output) > 0);
    AssertTrue('Should contain CSS category', Pos('embeddedcss', output) > 0);

    // Check for embedded JavaScript
    AssertTrue('Should contain JS alert function', Pos('<span class="default default-embeddedjs">alert</span>', output) > 0);
    AssertTrue('Should contain JS category', Pos('embeddedjs', output) > 0);

    // Check for HTML tags
    AssertTrue('Should contain style tag', Pos('<span class="keyword keyword-html">style</span>', output) > 0);
    AssertTrue('Should contain script tag', Pos('<span class="keyword keyword-html">script</span>', output) > 0);

  finally
    htmlHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestStringOutput;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  output: string;
begin
  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    tokens := jsHighlighter.Execute( 'var x = 42;');
    output := RenderToString(tokens);

    // Should be single continuous string without line breaks
    AssertTrue('Should not contain line breaks', Pos(#10, output) = 0);
    AssertTrue('Should not contain line breaks', Pos(#13, output) = 0);

    // Should contain all expected elements in sequence
    AssertTrue('Should contain var keyword', Pos('<span class="keyword keyword-javascript">var</span>', output) > 0);
    AssertTrue('Should contain number', Pos('<span class="numbers numbers-javascript">42</span>', output) > 0);
    AssertTrue('Should contain operator', Pos('<span class="operator operator-javascript">=</span>', output) > 0);

  finally
    jsHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestSpecialCharacterEscaping;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  output: string;
begin
  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    tokens := jsHighlighter.Execute( 'var html = "<div>\"Hello & Welcome\"</div>";');
    output := RenderToString(tokens);

    // Check that special characters are properly escaped
    AssertTrue('Should escape < character', Pos('&lt;', output) > 0);
    AssertTrue('Should escape > character', Pos('&gt;', output) > 0);
    AssertTrue('Should escape & character', Pos('&amp;', output) > 0);
    AssertTrue('Should escape " character', Pos('&quot;', output) > 0);

    // Should not contain unescaped special characters in content
    AssertFalse('Should not contain raw < in content', Pos('>"<', output) > 0);
    AssertFalse('Should not contain raw > in content', Pos('>>', output) > 0);

  finally
    jsHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestEmptyTokenArray;
var
  tokens: TSyntaxTokenArray;
  output: string;
  stringList: TStringList;
begin
  tokens:=[];
  SetLength(tokens, 0);

  // Test string output
  output := RenderToString(tokens);
  AssertEquals('Empty token array should produce empty string', '', output);

  // Test string list output
  stringList := RenderToStringList(tokens);
  try
    AssertEquals('Empty token array should produce empty string list', 0, stringList.Count);
  finally
    stringList.Free;
  end;
end;

procedure TTestHtmlRenderer.TestClassNameGeneration;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  output: string;
begin
  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    tokens := jsHighlighter.Execute( 'var');
    output := RenderToString(tokens);

    // Should have both base class and category-specific class
    AssertTrue('Should contain base keyword class', Pos('class="keyword keyword-javascript"', output) > 0);

  finally
    jsHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestCategoryMapping;
var
  cssHighlighter: TCssSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  output: string;
begin
  cssHighlighter := TCssSyntaxHighlighter.Create;
  try
    tokens := cssHighlighter.Execute( 'color');
    output := RenderToString(tokens);

    // Should map CSS category correctly
    AssertTrue('Should contain CSS category', Pos('keyword-css', output) > 0);

  finally
    cssHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestHtmlEscaping;
var
  tokens: TSyntaxTokenArray;
  output: string;
begin
  // Create a simple token with special characters
  tokens:=[];
  SetLength(tokens, 1);
  tokens[0] := TSyntaxToken.Create('<script>alert("test");</script>',shDefault);
  output := RenderToString(tokens);

  // All HTML special characters should be escaped
  AssertTrue('Should escape <script>', Pos('&lt;script&gt;', output) > 0);
  AssertTrue('Should escape quotes', Pos('&quot;test&quot;', output) > 0);
  AssertTrue('Should escape closing tag', Pos('&lt;/script&gt;', output) > 0);
end;

procedure TTestHtmlRenderer.TestTokensWithoutCategory;
var
  tokens: TSyntaxTokenArray;
  output: string;
begin
  tokens:=[];
  // Create tokens without category
  SetLength(tokens, 2);
  tokens[0] := TSyntaxToken.Create('hello',shKeyword);
  tokens[1] := TSyntaxToken.Create('world',shDefault);
  output := RenderToString(tokens);

  // Should have basic class names without category suffix
  AssertTrue('Should contain keyword class without category', Pos('<span class="keyword">hello</span>', output) > 0);
  AssertTrue('Should contain default class without category', Pos('<span class="default">world</span>', output) > 0);
end;

procedure TTestHtmlRenderer.TestComplexJavaScript;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  stringList: TStringList;
  output, fullOutput: string;
begin
  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    tokens := jsHighlighter.Execute( 'function add(a, b) { return a + b; }');

    // Test string list output
    stringList := RenderToStringList(tokens);
    try
      AssertEquals('Should have exactly one output line', 1, stringList.Count);

      // Get the single line of output
      fullOutput := stringList[0];

      AssertTrue('Should contain function keyword', Pos('keyword keyword-javascript">function', fullOutput) > 0);
      AssertTrue('Should contain return keyword', Pos('keyword keyword-javascript">return', fullOutput) > 0);
      AssertTrue('Should contain operator', Pos('operator operator-javascript">+', fullOutput) > 0);

    finally
      stringList.Free;
    end;

    // Compare with single string output
    output := RenderToString(tokens);
    AssertEquals('String and concatenated string list should be equal', fullOutput, output);

  finally
    jsHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestNoDefaultSpanOption;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  outputDefault, outputNoSpan: string;
begin
  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    tokens := jsHighlighter.Execute( 'var x = 42;');

    // Test default behavior (with default spans)
    renderer.Options := [];
    outputDefault := RenderToString(tokens);

    // Test with hroNoDefaultSpan option
    renderer.Options := [hroNoDefaultSpan];
    outputNoSpan := RenderToString(tokens);

    // Default behavior should contain default spans
    AssertTrue('Default output should contain default spans',
      Pos('<span class="default default-javascript">', outputDefault) > 0);

    // With hroNoDefaultSpan, default tokens should not be wrapped in spans
    AssertFalse('NoDefaultSpan output should not contain default spans',
      Pos('<span class="default default-javascript">', outputNoSpan) > 0);

    // Both outputs should still contain non-default spans
    AssertTrue('Default output should contain keyword spans',
      Pos('<span class="keyword keyword-javascript">var</span>', outputDefault) > 0);
    AssertTrue('NoDefaultSpan output should contain keyword spans',
      Pos('<span class="keyword keyword-javascript">var</span>', outputNoSpan) > 0);

    // Both outputs should still contain number spans
    AssertTrue('Default output should contain number spans',
      Pos('<span class="numbers numbers-javascript">42</span>', outputDefault) > 0);
    AssertTrue('NoDefaultSpan output should contain number spans',
      Pos('<span class="numbers numbers-javascript">42</span>', outputNoSpan) > 0);

    // NoDefaultSpan output should contain raw whitespace and identifier text
    AssertTrue('NoDefaultSpan output should contain raw whitespace', Pos(' x ', outputNoSpan) > 0);

    // Verify that default tokens are still HTML-escaped even when not wrapped in spans
    renderer.Options := [hroNoDefaultSpan];
    tokens := jsHighlighter.Execute( 'var html = "<test>";');
    outputNoSpan := RenderToString(tokens);

    // Should contain escaped < and > characters even in unwrapped default tokens
    AssertTrue('Should escape < character in unwrapped default tokens', Pos('&lt;', outputNoSpan) > 0);
    AssertTrue('Should escape > character in unwrapped default tokens', Pos('&gt;', outputNoSpan) > 0);

  finally
    jsHighlighter.Free;
    // Reset options to default for other tests
    renderer.Options := [];
  end;
end;

procedure TTestHtmlRenderer.TestMultilineRendering;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  output: string;
  multilineCode: string;
begin
  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    // Test multiline JavaScript code
    multilineCode := 'function test() {' + #10 + '  return "hello";' + #10 + '}';
    tokens := jsHighlighter.Execute( multilineCode);

    // Default behavior should preserve newlines in single line output
    renderer.Options := [];
    output := RenderToString(tokens);

    // Should contain the newlines from original code
    AssertTrue('Should contain newline characters', Pos(#10, output) > 0);
    AssertTrue('Should contain function keyword', Pos('<span class="keyword keyword-javascript">function</span>', output) > 0);
    AssertTrue('Should contain return keyword', Pos('<span class="keyword keyword-javascript">return</span>', output) > 0);
    AssertTrue('Should contain indentation spaces', Pos('  ', output) > 0);

  finally
    jsHighlighter.Free;
  end;
end;

procedure TTestHtmlRenderer.TestPreserveLineStructureOption;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  stringListDefault, stringListPreserved: TStringList;
  outputDefault, outputPreserved: string;
  multilineCode: string;
  i: Integer;
begin
  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    multilineCode := 'function test() {' + #10 + '  return "hello";' + #10 + '}';
    tokens := jsHighlighter.Execute( multilineCode);

    // Test default behavior (single line)
    renderer.Options := [];
    stringListDefault := RenderToStringList(tokens);
    try
      AssertEquals('Default should have 1 line', 1, stringListDefault.Count);
      outputDefault := stringListDefault[0];
      AssertTrue('Default output should contain newlines', Pos(#10, outputDefault) > 0);
    finally
      stringListDefault.Free;
    end;

    // Test with hroPreserveLineStructure (multiple lines)
    renderer.Options := [hroPreserveLineStructure];
    stringListPreserved := RenderToStringList(tokens);
    try
      AssertTrue('Preserved should have multiple lines', stringListPreserved.Count > 1);
      AssertEquals('Should have exactly 3 lines', 3, stringListPreserved.Count);

      // First line should contain function declaration
      AssertTrue('First line should contain function',
        Pos('<span class="keyword keyword-javascript">function</span>', stringListPreserved[0]) > 0);

      // Second line should contain return statement with indentation
      AssertTrue('Second line should contain return',
        Pos('<span class="keyword keyword-javascript">return</span>', stringListPreserved[1]) > 0);
      AssertTrue('Second line should contain indentation',
        Pos('  ', stringListPreserved[1]) > 0);

      // Third line should contain closing brace
      AssertTrue('Third line should contain closing brace',
        Pos('<span class="symbol symbol-javascript">}</span>', stringListPreserved[2]) > 0);

      // When concatenated, should be equivalent to single-line version
      outputPreserved := '';
      for i := 0 to stringListPreserved.Count - 1 do
        begin
        if i > 0 then
          outputPreserved := outputPreserved + #10;
        outputPreserved := outputPreserved + stringListPreserved[i];
        end;

      AssertEquals('Concatenated preserved output should equal default output',
        outputDefault, outputPreserved);

    finally
      stringListPreserved.Free;
    end;

    // Test combination with hroNoDefaultSpan
    renderer.Options := [hroPreserveLineStructure, hroNoDefaultSpan];
    stringListPreserved := RenderToStringList(tokens);
    try
      AssertTrue('Combined options should have multiple lines', stringListPreserved.Count > 1);

      // Should not contain default spans
      AssertFalse('Should not contain default spans with combined options',
        Pos('<span class="default default-javascript">', stringListPreserved.Text) > 0);

      // Should still contain keyword spans
      AssertTrue('Should still contain keyword spans with combined options',
        Pos('<span class="keyword keyword-javascript">function</span>', stringListPreserved.Text) > 0);

    finally
      stringListPreserved.Free;
    end;

  finally
    jsHighlighter.Free;
    // Reset options to default for other tests
    renderer.Options := [];
  end;
end;

procedure TTestHtmlRenderer.TestExtraClassesProperty;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  outputDefault, outputWithExtra: string;
begin
  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    tokens := jsHighlighter.Execute( 'var x = 42;');

    // Test default behavior (no extra classes)
    renderer.ExtraClasses := '';
    outputDefault := RenderToString(tokens);

    // Test with single extra class
    renderer.ExtraClasses := 'my-extra-class';
    outputWithExtra := RenderToString(tokens);

    // Default output should not contain extra class
    AssertFalse('Default output should not contain extra class',
      Pos('my-extra-class', outputDefault) > 0);

    // Output with extra classes should contain the extra class
    AssertTrue('Output with extra classes should contain my-extra-class',
      Pos('my-extra-class', outputWithExtra) > 0);

    // Should contain extra class in keyword spans
    AssertTrue('Should contain extra class in keyword spans',
      Pos('<span class="keyword keyword-javascript my-extra-class">var</span>', outputWithExtra) > 0);

    // Should contain extra class in number spans
    AssertTrue('Should contain extra class in number spans',
      Pos('<span class="numbers numbers-javascript my-extra-class">42</span>', outputWithExtra) > 0);

    // Should contain extra class in operator spans
    AssertTrue('Should contain extra class in operator spans',
      Pos('<span class="operator operator-javascript my-extra-class">=</span>', outputWithExtra) > 0);

    // Test with multiple extra classes
    renderer.ExtraClasses := 'class1 class2 class3';
    outputWithExtra := RenderToString(tokens);

    // Should contain all extra classes
    AssertTrue('Should contain all extra classes',
      Pos('class1 class2 class3', outputWithExtra) > 0);

    AssertTrue('Should contain multiple extra classes in keyword spans',
      Pos('<span class="keyword keyword-javascript class1 class2 class3">var</span>', outputWithExtra) > 0);

    // Test empty extra classes (should behave like default)
    renderer.ExtraClasses := '';
    outputWithExtra := RenderToString(tokens);
    AssertEquals('Empty extra classes should equal default output',
      outputDefault, outputWithExtra);

    // Test extra classes with hroNoDefaultSpan option
    renderer.Options := [hroNoDefaultSpan];
    renderer.ExtraClasses := 'no-default-extra';
    outputWithExtra := RenderToString(tokens);

    // Should not contain extra classes for default tokens (they're not wrapped)
    AssertFalse('Should not contain extra classes for unwrapped default tokens',
      Pos('<span class="default default-javascript no-default-extra">', outputWithExtra) > 0);

    // But should contain extra classes for non-default tokens
    AssertTrue('Should contain extra classes in non-default spans with hroNoDefaultSpan',
      Pos('<span class="keyword keyword-javascript no-default-extra">var</span>', outputWithExtra) > 0);

  finally
    jsHighlighter.Free;
    // Reset properties to default for other tests
    renderer.Options := [];
    renderer.ExtraClasses := '';
  end;
end;

initialization
  RegisterTest(TTestHtmlRenderer);
end.
