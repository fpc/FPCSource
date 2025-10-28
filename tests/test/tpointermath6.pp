{ %FAIL }

program tpointermath6;

{$mode objfpc}
{$pointermath on}

var
  pc: PCardinal;
  pb: PByte;
  b: Boolean;
begin
  b := pc < pb;
end.

