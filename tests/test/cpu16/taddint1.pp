{ %cpu=i8086 }

{
  Test for additions on CPUs with a 16-bit native integer.
  This test is Turbo Pascal 7 compatible.
}

{ Turn range checking off, as this test causes overflows intentionally to
  test the size and signedness of the integer that's being used. }
{$R-}

procedure Error;
begin
  Writeln('Error!');
  halt(1);
end;

var
  s8, s8_2: shortint;
  u8, u8_2: byte;
  s16, s16_2: integer;
  u16, u16_2: word;
  s32, s32_2: longint;
begin
  Writeln(' signed8 + signed8 -> signed16 ');
  s8 := 127; s8_2 := 127;
  s32 := s8 + s8_2;
  if s32 <> 254 then
    Error;
  s32 := s8 + s8_2 + s8 + s8_2;
  if s32 <> 508 then
    Error;
  s32 := s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2 +
         s8 + s8_2 + s8 + s8_2 + s8 + s8_2 + s8 + s8_2;
  if s32 <> -32008 then
    Error;

  Writeln(' signed8 + signed16 -> signed16 ');
  s16 := 32767; s8 := 127;
  s32 := s16 + s8;
  if s32 <> -32642 then
    Error;
  s32 := s8 + s16;
  if s32 <> -32642 then
    Error;

  Writeln(' signed16 + signed16 -> signed16 ');
  s16 := 32767; s16_2 := 32767;
  s32 := s16 + s16_2;
  if s32 <> -2 then
    Error;

  Writeln(' unsigned8 + unsigned8 -> signed16 ');
  u8 := 255; u8_2 := 255;
  s32 := u8 + u8_2;
  if s32 <> 510 then
    Error;
  s32 := u8 + u8_2 + u8 + u8_2;
  if s32 <> 1020 then
    Error;
  s32 := u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2 +
         u8 + u8_2 + u8 + u8_2 + u8 + u8_2 + u8 + u8_2;
  if s32 <> -30856 then
    Error;

  Writeln(' unsigned8 + unsigned16 -> unsigned16 ');
  u16 := 65535; u8 := 255;
  s32 := u16 + u8;
  if s32 <> 254 then
    Error;
  s32 := u8 + u16;
  if s32 <> 254 then
    Error;
  u16 := 32768; u8 := 255;
  s32 := u16 + u8;
  if s32 <> 33023 then
    Error;
  s32 := u8 + u16;
  if s32 <> 33023 then
    Error;

  Writeln(' unsigned16 + unsigned16 -> unsigned16 ');
  u16 := 32767; u16_2 := 1;
  s32 := u16 + u16_2;
  if s32 <> 32768 then
    Error;
  u16 := 65535; u16_2 := 65535;
  s32 := u16 + u16_2;
  if s32 <> 65534 then
    Error;

  Writeln(' signed8 + unsigned8 -> signed16 ');
  s8 := 127; u8 := 255;
  s32 := s8 + u8;
  if s32 <> 382 then
    Error;
  s32 := s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 +
         s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8 + s8 + u8;
  if s32 <> -30774 then
    Error;

  Writeln(' unsigned8 + signed16 -> signed16 ');
  u8 := 255; s16 := 32767;
  s32 := u8 + s16;
  if s32 <> -32514 then
    Error;
  s32 := s16 + u8;
  if s32 <> -32514 then
    Error;

  Writeln(' signed8 + unsigned16 -> signed32 ');
  s8 := 127; u16 := 65535;
  s32 := s8 + u16;
  if s32 <> 65662 then
    Error;
  s32 := u16 + s8;
  if s32 <> 65662 then
    Error;

  Writeln(' signed16 + unsigned16 -> signed32 ');
  s16 := 32767; u16 := 65535;
  s32 := s16 + u16;
  if s32 <> 98302 then
    Error;
  s32 := u16 + s16;
  if s32 <> 98302 then
    Error;

  Writeln(' signed32 + signed8 -> signed32 ');
  s32_2 := 1000000; s8 := 127;
  s32 := s32_2 + s8;
  if s32 <> 1000127 then
    Error;
  s32 := s8 + s32_2;
  if s32 <> 1000127 then
    Error;

  Writeln(' signed32 + unsigned8 -> signed32 ');
  s32_2 := 1000000; u8 := 255;
  s32 := s32_2 + u8;
  if s32 <> 1000255 then
    Error;
  s32 := u8 + s32_2;
  if s32 <> 1000255 then
    Error;

  Writeln(' signed32 + signed16 -> signed32 ');
  s32_2 := 1000000; s16 := 32767;
  s32 := s32_2 + s16;
  if s32 <> 1032767 then
    Error;
  s32 := s16 + s32_2;
  if s32 <> 1032767 then
    Error;

  Writeln(' signed32 + unsigned16 -> signed32 ');
  s32_2 := 1000000; u16 := 65535;
  s32 := s32_2 + u16;
  if s32 <> 1065535 then
    Error;
  s32 := u16 + s32_2;
  if s32 <> 1065535 then
    Error;

  Writeln(' signed32 + signed32 -> signed32 ');
  s32_2 := 1000000000;
  s32 := s32_2 + s32_2;
  if s32 <> 2000000000 then
    Error;
  s32 := s32_2 + s32_2 + s32_2;
  if s32 <> -1294967296 then
    Error;
end.
