program tb0627;

{ tests for correctness various simplifications, done by the compiler }

{$mode objfpc}

procedure Check(i64, expected_value: Int64);
begin
  if i64<>expected_value then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

procedure Check(qw, expected_value: QWord);
begin
  if qw<>expected_value then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

procedure Check(b, expected_value: Boolean);
begin
  if b<>expected_value then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

var
  s8, s8_2: ShortInt;
  s16, s16_2: SmallInt;
  s32, s32_2: LongInt;
  s64, s64_2: Int64;
  u8, u8_2: Byte;
  u16, u16_2: Word;
  u32, u32_2: LongWord;
  u64, u64_2: QWord;
  I: Integer;
begin
  for I := 1 to 100 do
  begin
    u8 := Random(256);
    s32 := LongInt(Random($ffffffff));

    { many of these are now optimized at various -O settings }
    Check(s32-s32,0);
    Check(s32 xor s32,0);
    Check(s32=s32, True);
    Check(s32<=s32, True);
    Check(s32>=s32, True);
    Check(s32<>s32, False);
    Check(s32<s32, False);
    Check(s32>s32, False);
    Check(0*s32, 0);
    Check(s32*0, 0);
    Check(0 and s32, 0);
    Check(s32 and 0, 0);
    Check(0 shr s32, 0);
    Check(0 shl s32, 0);
    Check(SarShortInt(0, u8), 0);
    Check(SarShortInt(ShortInt($ff), u8), -1);
    Check(SarSmallInt(0, u8), 0);
    Check(SarSmallInt(SmallInt($ffff), u8), -1);
    Check(SarLongInt(0, u8), 0);
    Check(SarLongInt(LongInt($ffffffff), u8), -1);
    Check(SarInt64(0, u8), 0);
    Check(SarInt64(Int64($ffffffffffffffff), u8), -1);
    Check(RorByte(0, u8), 0);
    Check(RolByte(0, u8), 0);
    Check(RorWord(0, u8), 0);
    Check(RolWord(0, u8), 0);
    Check(RorDWord(0, u8), 0);
    Check(RolDWord(0, u8), 0);
    Check(RorQWord(0, u8), 0);
    Check(RolQWord(0, u8), 0);
    Check(RorByte($ff, u8), $ff);
    Check(RolByte($ff, u8), $ff);
    Check(RorWord($ffff, u8), $ffff);
    Check(RolWord($ffff, u8), $ffff);
    Check(RorDWord($ffffffff, u8), $ffffffff);
    Check(RolDWord($ffffffff, u8), $ffffffff);
    Check(RorQWord(QWord($ffffffffffffffff), u8), QWord($ffffffffffffffff));
    Check(RolQWord(QWord($ffffffffffffffff), u8), QWord($ffffffffffffffff));
  end;
end.
