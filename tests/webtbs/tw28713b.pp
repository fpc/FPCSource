{ %OPT=-O3 }
{$mode objfpc}
// Compiled with option -O3 for Win32-I386

type
  PWordArray = ^TWordArray;
  TWordArray = array [0..1023] of PtrUInt;

  TMyclass = class
    LoByte,HiByte:Byte
  end;

var
  Buffer:TWordArray;
  OldMousePos:LongInt = 0;
  ScreenBuffer:Pointer = @Buffer;

procedure Show(ScreenBuffer:Pointer);
begin
  TMyClass(PWordArray(ScreenBuffer)^[OldMousePos]).HiByte:=(not
  TMyClass(PWordArray(ScreenBuffer)^[OldMousePos]).HiByte)and $7F
  // he forgets to write the result into the array
end;

begin
  TMyClass(Buffer[0]):=TMyClass.Create;
  Show(ScreenBuffer);
  if TMyClass(Buffer[0]).HiByte<>$7F then
    halt(1);
  writeln('ok');
end.
