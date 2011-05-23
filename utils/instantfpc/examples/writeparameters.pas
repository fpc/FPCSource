#!/usr/bin/env instantfpc
var
  i: Integer;
begin
  for i:=0 to ParamCount do writeln(ParamStr(i));
end.

