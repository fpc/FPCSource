program isprg;

{$mode objfpc}

uses ischild, iscontrol, isbase;

var
  Value: TBase;
begin
  Value:=TChild.Create;
  Test(Value);
  if not (Value is TChild) then
    Halt(3);
  if Value.Value<>42 then
    Halt(2);
  Value.Free;
end.
