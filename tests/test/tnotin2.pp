{ %FAIL }

{ "not in" requires modeswitch reorderedoperators }
program tnotin2;

{$mode objfpc}{$H+}

var
  B: byte;
begin
  B:=1;
  if B not in [2,3] then ;
end.
