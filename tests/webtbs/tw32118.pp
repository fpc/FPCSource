{ %NORUN }

program tw32118;
{$MODE OBJFPC}
type
  generic TTestClass<SomeTemplate> = class
    procedure Test;
  end;

procedure TTestClass.Test;
var
  i : SomeTemplate;
begin
  for i := 1 to 10 do WriteLn(i);
end;

begin
end.
