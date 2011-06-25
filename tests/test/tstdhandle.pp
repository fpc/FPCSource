{ %TARGET=win64 }
uses
  Windows;

{ The STD_xxx_HANDLE constants remain 32-bit in 64-bit Windows. }
{$warnings off} // unreachable code warnings when things are correct.
begin
  if STD_INPUT_HANDLE > High(DWORD) then Halt(1);
  if STD_OUTPUT_HANDLE > High(DWORD) then Halt(2);
  if STD_ERROR_HANDLE > High(DWORD) then Halt(3);
  Halt(0);
end.