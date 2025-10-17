program test_asm;

{$mode objfpc}{$H+}

uses
  SysUtils, syntax.highlighter, syntax.pascal;

procedure TestASM;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  highlighter: TSyntaxHighlighter;
begin
  highlighter := TSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute('asm'#13#10'end');
  finally
    highlighter.Free;
  end;

  WriteLn('Testing ASM block: ''asm'#13#10'end''');
  WriteLn('Token count: ', Length(tokens));
  WriteLn;

  for i := 0 to High(tokens) do begin
    WriteLn('Token ', i, ': "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind));
  end;
end;

begin
  WriteLn('ASM Block Test');
  WriteLn('==============');
  WriteLn;

  TestASM;

  WriteLn;
  WriteLn('Test completed. Press Enter to exit.');
  ReadLn;
end.