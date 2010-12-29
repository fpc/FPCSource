{ %norun }
program tpointermath3;

{$MODE FPC}
type
  PByte = ^Byte;
var
  PB: PByte;
begin
  // in FPC/ObjFPC mode pointer math is ON by default
  PB := PB + 1;
  WriteLn(PB[1]);
end.

