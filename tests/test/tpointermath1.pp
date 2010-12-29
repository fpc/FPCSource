{ %norun }
program tpointermath1;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
type
  {$POINTERMATH ON}
  PByte = ^Byte; // Pointer arithmetic can be applied to types declared with PointerMath ON
  {$POINTERMATH OFF}
  PInteger = ^Integer;
var
  PB: PByte;
  PI: PInteger;
begin
  // This checks even with PointerMath=Off it still can be used for types declared with PointerMath=On
  PB := PB + 1;
  WriteLn(PB[1]);
  {$POINTERMATH ON}
  // It can also be used if explicitely declared in the code
  PI := PI + 1;
  WriteLn(PI[1]);
end.

