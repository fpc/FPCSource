program test_compatibility;

{$mode objfpc}{$H+}

uses
  SysUtils, syntax.highlighter, syntax.pascal;

var
  tokens: TSyntaxTokenArray;
begin
  WriteLn('Testing backward compatibility function...');

  tokens := DoPascalHighlighting('begin end');

  if (Length(tokens) = 3) and (tokens[0].Kind = shKeyword) and (tokens[2].Kind = shKeyword) then
    WriteLn('PASS - Backward compatibility function works correctly')
  else
    WriteLn('FAIL - Backward compatibility function not working');

  WriteLn('Test completed.');
end.