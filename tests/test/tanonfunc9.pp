{ %FAIL }

{ an anonymous function referencing Self can not be assigned to a procedure
  variable }

program tanonfunc9;

{$mode objfpc}
{$modeswitch anonymousfunctions}

type
  TTestFunc = function: LongInt;

  TTest = class
    f: LongInt;
    procedure Test;
  end;

procedure TTest.Test;
var
  tf: TTestFunc;
begin
  tf := function: LongInt begin Result := f; end;
end;

begin

end.
