program tw10933;

{$MODE DELPHI}

var
  s: string[3] = 'ABC';

procedure Foo(buf: PAnsiChar; expected: AnsiChar);
begin
  WriteLn(buf^);
  if buf^ <> expected then
    Halt(1);
end;

function ClassNameShort(): PShortString;
begin
  Result := @s;
end;

begin
  Foo(@ClassNameShort()^[1], 'A');
  Foo(@ClassNameShort()^[2], 'B');
  Foo(@ClassNameShort()^[3], 'C');
end.
