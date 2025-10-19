program test_htmlrender;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syntax.highlighter, syntax.javascript, syntax.css,
  syntax.html, syntax.htmlrender;

procedure TestJavaScriptRendering;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  renderer: THtmlSyntaxRenderer;
  tokens: TSyntaxTokenArray;
  output: TStringList;
  jsCode: string;
  i: Integer;
begin
  WriteLn('Testing JavaScript HTML Rendering:');
  WriteLn('==================================');

  jsCode := 'function test() { return "hello"; }';

  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  renderer := THtmlSyntaxRenderer.Create;
  output := TStringList.Create;
  try
    tokens := jsHighlighter.Execute(jsCode);

    WriteLn('Original JavaScript: ', jsCode);
    WriteLn;

    renderer.RenderTokens(tokens, output);

    WriteLn('Rendered HTML:');
    for i := 0 to output.Count - 1 do
      WriteLn(output[i]);

  finally
    output.Free;
    renderer.Free;
    jsHighlighter.Free;
  end;

  WriteLn;
  WriteLn;
end;

procedure TestCssRendering;
var
  cssHighlighter: TCssSyntaxHighlighter;
  renderer: THtmlSyntaxRenderer;
  tokens: TSyntaxTokenArray;
  output: TStringList;
  cssCode: string;
  i: Integer;
begin
  WriteLn('Testing CSS HTML Rendering:');
  WriteLn('===========================');

  cssCode := 'body { color: #FF0000; font-size: 16px; }';

  cssHighlighter := TCssSyntaxHighlighter.Create;
  renderer := THtmlSyntaxRenderer.Create;
  output := TStringList.Create;
  try
    tokens := cssHighlighter.Execute(cssCode);

    WriteLn('Original CSS: ', cssCode);
    WriteLn;

    renderer.RenderTokens(tokens, output);

    WriteLn('Rendered HTML:');
    for i := 0 to output.Count - 1 do
      WriteLn(output[i]);

  finally
    output.Free;
    renderer.Free;
    cssHighlighter.Free;
  end;

  WriteLn;
  WriteLn;
end;

procedure TestHtmlRendering;
var
  htmlHighlighter: THtmlSyntaxHighlighter;
  renderer: THtmlSyntaxRenderer;
  tokens: TSyntaxTokenArray;
  output: TStringList;
  htmlCode: string;
  i: Integer;
begin
  WriteLn('Testing HTML HTML Rendering:');
  WriteLn('============================');

  htmlCode := '<div class="container">&lt;Hello&gt;</div>';

  htmlHighlighter := THtmlSyntaxHighlighter.Create;
  renderer := THtmlSyntaxRenderer.Create;
  output := TStringList.Create;
  try
    tokens := htmlHighlighter.Execute(htmlCode);

    WriteLn('Original HTML: ', htmlCode);
    WriteLn;

    renderer.RenderTokens(tokens, output);

    WriteLn('Rendered HTML:');
    for i := 0 to output.Count - 1 do
      WriteLn(output[i]);

  finally
    output.Free;
    renderer.Free;
    htmlHighlighter.Free;
  end;

  WriteLn;
  WriteLn;
end;

procedure TestEmbeddedHtmlRendering;
var
  htmlHighlighter: THtmlSyntaxHighlighter;
  renderer: THtmlSyntaxRenderer;
  tokens: TSyntaxTokenArray;
  output: TStringList;
  htmlCode: string;
  i: Integer;
begin
  WriteLn('Testing Embedded HTML Rendering:');
  WriteLn('================================');

  htmlCode := '<style>body { color: red; }</style><script>alert("test");</script>';

  htmlHighlighter := THtmlSyntaxHighlighter.Create;
  renderer := THtmlSyntaxRenderer.Create;
  output := TStringList.Create;
  try
    tokens := htmlHighlighter.Execute(htmlCode);

    WriteLn('Original HTML: ', htmlCode);
    WriteLn;

    renderer.RenderTokens(tokens, output);

    WriteLn('Rendered HTML:');
    for i := 0 to output.Count - 1 do
      WriteLn(output[i]);

  finally
    output.Free;
    renderer.Free;
    htmlHighlighter.Free;
  end;

  WriteLn;
  WriteLn;
end;

procedure TestStringOutput;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  renderer: THtmlSyntaxRenderer;
  tokens: TSyntaxTokenArray;
  output: string;
  jsCode: string;
begin
  WriteLn('Testing String Output:');
  WriteLn('=====================');

  jsCode := 'var x = 42;';

  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  renderer := THtmlSyntaxRenderer.Create;
  try
    tokens := jsHighlighter.Execute(jsCode);

    WriteLn('Original JavaScript: ', jsCode);
    WriteLn;

    renderer.RenderTokensToString(tokens, output);

    WriteLn('Rendered HTML (single string):');
    WriteLn(output);

  finally
    renderer.Free;
    jsHighlighter.Free;
  end;

  WriteLn;
  WriteLn;
end;

procedure TestSpecialCharacters;
var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  renderer: THtmlSyntaxRenderer;
  tokens: TSyntaxTokenArray;
  output: TStringList;
  jsCode: string;
  i: Integer;
begin
  WriteLn('Testing Special Character Escaping:');
  WriteLn('===================================');

  jsCode := 'var html = "<div>\"Hello & Welcome\"</div>";';

  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  renderer := THtmlSyntaxRenderer.Create;
  output := TStringList.Create;
  try
    tokens := jsHighlighter.Execute(jsCode);

    WriteLn('Original JavaScript: ', jsCode);
    WriteLn;

    renderer.RenderTokens(tokens, output);

    WriteLn('Rendered HTML (with escaping):');
    for i := 0 to output.Count - 1 do
      WriteLn(output[i]);

  finally
    output.Free;
    renderer.Free;
    jsHighlighter.Free;
  end;

  WriteLn;
  WriteLn;
end;

begin
  WriteLn('HTML Syntax Renderer Test');
  WriteLn('=========================');
  WriteLn;

  TestJavaScriptRendering;
  TestCssRendering;
  TestHtmlRendering;
  TestEmbeddedHtmlRendering;
  TestStringOutput;
  TestSpecialCharacters;

  WriteLn('Test completed. Press Enter to exit.');
  ReadLn;
end.