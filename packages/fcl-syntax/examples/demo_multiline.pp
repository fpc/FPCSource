program test_multiline;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syntax.highlighter, syntax.javascript, syntax.htmlrender;

var
  jsHighlighter: TJavaScriptSyntaxHighlighter;
  renderer: THtmlSyntaxRenderer;
  tokens: TSyntaxTokenArray;
  output: TStringList;
  jsCode: string;
  i: Integer;
begin
  WriteLn('Testing Multiline JavaScript Rendering:');
  WriteLn('=======================================');

  jsCode := 'function test() {' + #10 + '  return "hello";' + #10 + '}';

  WriteLn('Original JavaScript:');
  WriteLn(jsCode);
  WriteLn;

  jsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  renderer := THtmlSyntaxRenderer.Create;
  output := TStringList.Create;
  try
    tokens := jsHighlighter.Execute(jsCode);

    WriteLn('Tokens found: ', Length(tokens));
    for i := 0 to High(tokens) do begin
      WriteLn('Token ', i, ': "', StringReplace(tokens[i].Text, #10, '\n', [rfReplaceAll]), '" Kind: ', Ord(tokens[i].Kind));
    end;
    WriteLn;

    // Test default behavior
    renderer.Options := [];
    renderer.RenderTokens(tokens, output);

    WriteLn('Default Rendering (', output.Count, ' lines):');
    for i := 0 to output.Count - 1 do
      WriteLn('Line ', i, ': ', output[i]);

    WriteLn;

    // Test with hroPreserveLineStructure
    output.Clear;
    renderer.Options := [hroPreserveLineStructure];
    renderer.RenderTokens(tokens, output);

    WriteLn('Preserved Line Structure (', output.Count, ' lines):');
    for i := 0 to output.Count - 1 do
      WriteLn('Line ', i, ': ', output[i]);

    WriteLn;

    // Test with both options
    output.Clear;
    renderer.Options := [hroPreserveLineStructure, hroNoDefaultSpan];
    renderer.RenderTokens(tokens, output);

    WriteLn('Preserved + No Default Spans (', output.Count, ' lines):');
    for i := 0 to output.Count - 1 do
      WriteLn('Line ', i, ': ', output[i]);

  finally
    output.Free;
    renderer.Free;
    jsHighlighter.Free;
  end;

  WriteLn;
  WriteLn('Press Enter to exit.');
  ReadLn;
end.