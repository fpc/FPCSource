{ %FAIL }
{ %CPU=i8086 }

program tanonfunc59;

{$mode objfpc}{$H+}
{$modeswitch anonymousfunctions}

type
  TNearFunc = function: LongInt; near;

var
  f: TNearFunc;
begin
  f := function: LongInt far begin Result := 42; end;
end.

