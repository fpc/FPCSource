program test_extraclasses;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syntax.highlighter, syntax.javascript, syntax.htmlrender;

var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  renderer: THtmlSyntaxRenderer;
  tokens: TSyntaxTokenArray;
  output: string;
  jsCode: string;
begin
  WriteLn('Testing ExtraClasses Property:');
  WriteLn('=============================');

  jsCode := 'var x = 42;';
  WriteLn('Original JavaScript: ', jsCode);
  WriteLn;

  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  renderer := THtmlSyntaxRenderer.Create;
  try
    tokens := jsHighlighter.Execute(jsCode);

    // Test default (no extra classes)
    renderer.ExtraClasses := '';
    renderer.RenderTokensToString(tokens, output);
    WriteLn('Default (no extra classes):');
    WriteLn(output);
    WriteLn;

    // Test with single extra class
    renderer.ExtraClasses := 'syntax-highlight';
    renderer.RenderTokensToString(tokens, output);
    WriteLn('With single extra class "syntax-highlight":');
    WriteLn(output);
    WriteLn;

    // Test with multiple extra classes
    renderer.ExtraClasses := 'code-block theme-dark line-1';
    renderer.RenderTokensToString(tokens, output);
    WriteLn('With multiple extra classes "code-block theme-dark line-1":');
    WriteLn(output);
    WriteLn;

    // Test with hroNoDefaultSpan option
    renderer.Options := [hroNoDefaultSpan];
    renderer.ExtraClasses := 'no-default-spans';
    renderer.RenderTokensToString(tokens, output);
    WriteLn('With hroNoDefaultSpan + extra classes "no-default-spans":');
    WriteLn(output);
    WriteLn;

  finally
    renderer.Free;
    jsHighlighter.Free;
  end;

  WriteLn('Press Enter to exit.');
  ReadLn;
end.