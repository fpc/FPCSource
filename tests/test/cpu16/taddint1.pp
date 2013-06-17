{ %cpu=i8086 }

{
  Test for additions on CPUs with a 16-bit native integer.
  This test is Turbo Pascal 7 compatible.
}

{ Turn range checking off, as this test causes overflows intentionally to
  test the size and signedness of the integer that's being used. }
{$R-}

var
  ErrorCode: Integer;

procedure Error(code : integer);
begin
  Writeln('Error: ',code,'!');
  ErrorCode := code;
end;

var
  s8, s8_2: shortint;
  u8, u8_2: byte;
  s16, s16_2: integer;
  u16, u16_2: word;
  s32, s32_2: longint;
begin
  ErrorCode := 0;

  Writeln(' signed8 + signed8 -> signed16 ');
  s8 := 127; s8_2 := 127;
  s32 := s8 + s8_2;
  if s32 <> 254 then
    Error(1);
  s32 := s8 + s8_2 + s8 + s8_2;
  if s32 <> 508 then
    Error(2);
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
    Error(3);

  Writeln(' signed8 + signed16 -> signed16 ');
  s16 := 32767; s8 := 127;
  s32 := s16 + s8;
  if s32 <> -32642 then
    Error(4);
  s32 := s8 + s16;
  if s32 <> -32642 then
    Error(5);

  Writeln(' signed16 + signed16 -> signed16 ');
  s16 := 32767; s16_2 := 32767;
  s32 := s16 + s16_2;
  if s32 <> -2 then
    Error(6);

  Writeln(' unsigned8 + unsigned8 -> signed16 ');
  u8 := 255; u8_2 := 255;
  s32 := u8 + u8_2;
  if s32 <> 510 then
    Error(7);
  s32 := u8 + u8_2 + u8 + u8_2;
  if s32 <> 1020 then
    Error(8);
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
    begin
      Error(9);
      writeln('s32=',s32, '$',hexstr(s32,8));
    end;

  Writeln(' unsigned8 + unsigned16 -> unsigned16 ');
  u16 := 65535; u8 := 255;
  s32 := u16 + u8;
  if s32 <> 254 then
    Error(10);
  s32 := u8 + u16;
  if s32 <> 254 then
    Error(11);
  u16 := 32768; u8 := 255;
  s32 := u16 + u8;
  if s32 <> 33023 then
    Error(12);
  s32 := u8 + u16;
  if s32 <> 33023 then
    Error(13);

  Writeln(' unsigned16 + unsigned16 -> unsigned16 ');
  u16 := 32767; u16_2 := 1;
  s32 := u16 + u16_2;
  if s32 <> 32768 then
    Error(14);
  u16 := 65535; u16_2 := 65535;
  s32 := u16 + u16_2;
  if s32 <> 65534 then
    Error(15);

  Writeln(' signed8 + unsigned8 -> signed16 ');
  s8 := 127; u8 := 255;
  s32 := s8 + u8;
  if s32 <> 382 then
    Error(16);
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
    Error(17);

  Writeln(' unsigned8 + signed16 -> signed16 ');
  u8 := 255; s16 := 32767;
  s32 := u8 + s16;
  if s32 <> -32514 then
    Error(18);
  s32 := s16 + u8;
  if s32 <> -32514 then
    Error(19);

  Writeln(' signed8 + unsigned16 -> signed32 ');
  s8 := 127; u16 := 65535;
  s32 := s8 + u16;
  if s32 <> 65662 then
    Error(20);
  s32 := u16 + s8;
  if s32 <> 65662 then
    Error(21);

  Writeln(' signed16 + unsigned16 -> signed32 ');
  s16 := 32767; u16 := 65535;
  s32 := s16 + u16;
  if s32 <> 98302 then
    Error(22);
  s32 := u16 + s16;
  if s32 <> 98302 then
    Error(23);

  Writeln(' signed32 + signed8 -> signed32 ');
  s32_2 := 1000000; s8 := 127;
  s32 := s32_2 + s8;
  if s32 <> 1000127 then
    Error(24);
  s32 := s8 + s32_2;
  if s32 <> 1000127 then
    Error(25);

  Writeln(' signed32 + unsigned8 -> signed32 ');
  s32_2 := 1000000; u8 := 255;
  s32 := s32_2 + u8;
  if s32 <> 1000255 then
    Error(26);
  s32 := u8 + s32_2;
  if s32 <> 1000255 then
    Error(27);

  Writeln(' signed32 + signed16 -> signed32 ');
  s32_2 := 1000000; s16 := 32767;
  s32 := s32_2 + s16;
  if s32 <> 1032767 then
    Error(28);
  s32 := s16 + s32_2;
  if s32 <> 1032767 then
    Error(29);

  Writeln(' signed32 + unsigned16 -> signed32 ');
  s32_2 := 1000000; u16 := 65535;
  s32 := s32_2 + u16;
  if s32 <> 1065535 then
    Error(30);
  s32 := u16 + s32_2;
  if s32 <> 1065535 then
    Error(31);

  Writeln(' signed32 + signed32 -> signed32 ');
  s32_2 := 1000000000;
  s32 := s32_2 + s32_2;
  if s32 <> 2000000000 then
    Error(32);
  s32 := s32_2 + s32_2 + s32_2;
  if s32 <> -1294967296 then
    Error(33);

  Halt(ErrorCode);
end.
