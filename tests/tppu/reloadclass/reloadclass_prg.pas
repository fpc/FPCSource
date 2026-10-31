program reloadclass_prg;

{$mode objfpc}

uses reloadclass_child, reloadclass_control, reloadclass_base;

var
  Value: TBase;
begin
  Value:=TBase.Create;
  Test(Value);
  if Value.Value<>42 then
    Halt(2);
  Value.Free;
end.