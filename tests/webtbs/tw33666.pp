program Project1;

{$mode objfpc}{$H+}
{$Codepage UTF8}

type
  CP437String = type ansistring(437);

var
  s_cp437_1: CP437String;
begin
  s_cp437_1 := '║'; //<--- buggy
  if (length(s_cp437_1)<> 1) or
     (ord(s_cp437_1[1])<> 186) then
    halt(1);
end.
