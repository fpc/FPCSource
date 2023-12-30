program tfuncref55;

{$mode objfpc}
{$modeswitch functionreferences}

procedure Test;
var
  a: Char;

  function DoTest(aArg: LongInt): String;
  begin
    if aArg > 0 then
      Result := DoTest(aArg - 1) + a
    else
      Result := a;
  end;

var
  func: reference to function(aArg: LongInt): String;
begin
  a := 'a';
  func := @DoTest;
  if func(4) <> 'aaaaa' then
    Halt(1);
  a := 'b';
  if func(6) <> 'bbbbbbb' then
    Halt(2);
end;

begin
  Test;
end.
