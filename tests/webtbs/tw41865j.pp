{ %CPU=aarch64 }
{ Currency modulo literals must be scaled before integer simplification. }
program tw41865j;
{$mode objfpc}
const
  Inputs: array[0..9] of Int64 = (-100006,-100002,-6,-2,-1,1,2,6,100002,100006);
var
  A, B: Currency;
  I: Integer;
  Expected: Int64;
procedure Check(const Value: Currency);
begin
  if PInt64(@Value)^ <> Expected then
    begin
      WriteLn('FAIL: Got ', PInt64(@Value)^, ', expected ', Expected);
      Halt(1);
    end;
end;
begin
  for I := Low(Inputs) to High(Inputs) do
    begin
      PInt64(@A)^ := Inputs[I];
      Expected := Inputs[I] mod 10000;
      Check(A mod 1);
      Check(A mod -1);
      Check(A mod Currency(1));
      Check(A mod Currency(-1));
      B := 1;
      Check(A mod B);
      B := -1;
      Check(A mod B);
    end;
  WriteLn('ok');
end.
