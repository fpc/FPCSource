{ %CPU=aarch64 }
{ %OPT=-O2 -Oofastmath }
{ Currency div/mod require the ordinal Currency representation. Restrict
  this test to the architecture on which this path has been validated. }
program tw41865i;
{$mode objfpc}
{$inline on}

const
  inputs: array[0..7] of Int64 = (100000, -100000, 100002, -100002,
    2000, -2000, 0, 30000);
  quotients: array[0..7] of Int64 = (30000, -30000, 30000, -30000,
    0, 0, 0, 10000);
  remainders: array[0..7] of Int64 = (10000, -10000, 10002, -10002,
    2000, -2000, 0, 0);

function Identity(const c: Currency): Currency; inline;
begin
  Result := c;
end;

procedure Check(const c: Currency; expected: Int64);
begin
  if PInt64(@c)^ <> expected then
    begin
      WriteLn('FAIL: Got ', PInt64(@c)^, ', expected ', expected);
      Halt(1);
    end;
end;

var
  a, b: Currency;
  i: Integer;
begin
  b := 3;
  for i := Low(inputs) to High(inputs) do
    begin
      PInt64(@a)^ := inputs[i];
      Check(Identity(a div b), quotients[i]);
      Check(Identity(a mod b), remainders[i]);
      Check(Identity(a div b) + 1, quotients[i] + 10000);
      Check(Identity(a mod b) + 1, remainders[i] + 10000);
    end;
  WriteLn('ok');
end.
