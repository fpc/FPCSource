{ %FAIL }
{ %CPU=i8086 }

program tanonfunc58;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

type
  TFarFunc = function: LongInt; far;

var
  f: TFarFunc;
begin
  f := function: LongInt near begin Result := 42; end;
end.

