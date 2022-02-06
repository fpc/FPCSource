{ %FAIL }

{ an anonymous function referencing a local parent variable can not be assigned
  to a procedure variable }

program tanonfunc10;

{$mode objfpc}
{$modeswitch anonymousfunctions}

type
  TTestFunc = function: LongInt;

procedure Test;
var
  i: LongInt;
  tf: TTestFunc;
begin
  tf := function: LongInt begin Result := i; end;
end;

begin

end.
