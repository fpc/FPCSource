unit iscontrol;

{$mode objfpc}

interface

uses isbase, ischild;

const Factor = 1;

procedure Test(Value: TBase);

implementation

procedure Test(Value: TBase);
begin
  if not (Value is TChild) then
    Halt(1);
  Value.Value:=42;
end;

end.
