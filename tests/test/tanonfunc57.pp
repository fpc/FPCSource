{ %CPU=i8086 }

program tanonfunc57;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

type
  TFarFunc = function: LongInt; far;
  TNearFunc = function: LongInt; near;

var
  f: TFarFunc;
  n: TNearFunc;
begin
  f := function: LongInt far begin Result := 42; end;
  n := function: LongInt near begin Result := 21; end;

  if f() <> 42 then
    Halt(1);
  if n() <> 21 then
    Halt(2);
end.

