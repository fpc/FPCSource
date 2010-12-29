{ %fail }
program tpointermath2;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
type
  PByte = ^Byte;
var
  PB: PByte;
begin
  // Pointer math must fail in delphi mode without pointermath directive
  PB := PB + 1;
end.

