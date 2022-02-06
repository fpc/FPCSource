{ %FAIL }

{ an anonymous function referencing a local variable can not be assigned to a
  method variable }

program tanonfunc11;

{$mode objfpc}
{$modeswitch anonymousfunctions}

type
  TTestMethod = function: LongInt of object;

  TTest = class
    f: LongInt;
    procedure Test;
  end;

procedure TTest.Test;
var
  tm: TTestMethod;
  i: LongInt;
begin
  tm := function: LongInt begin Result := i * f; end;
end;

begin

end.
