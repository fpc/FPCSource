{ %OPT=-O3 }

program tb0627a;

{ tests for correctness various simplifications, done by the compiler }

{ this test checks that these simplifications aren't done at -O3 level
  or less, when they will remove an expression that has side effects }

{$mode objfpc}

var
  SideEffectsHappened: Boolean = False;

function s32_SideEffects: LongInt;
begin
  SideEffectsHappened := True;
  Result := LongInt(Random($ffffffff));
end;

function u8_SideEffects: Byte;
begin
  SideEffectsHappened := True;
  Result := Random(256);
end;

procedure CheckSideEffectsHappened;
begin
  if not SideEffectsHappened then
  begin
    Writeln('Error! Side effects were removed!');
    Halt(1);
  end;
  SideEffectsHappened := False;
end;

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

procedure NoCheck(i64, expected_value: Int64);
begin
  if i64<>expected_value then
    Write('.')
  else
    Write(',');
end;

procedure NoCheck(qw, expected_value: QWord);
begin
  if qw<>expected_value then
    Write('.')
  else
    Write(',');
end;

procedure NoCheck(b, expected_value: Boolean);
begin
  if b<>expected_value then
    Write('.')
  else
    Write(',');
end;

var
  I: Integer;
begin
  for I := 1 to 100 do
  begin
    { many of these are now optimized at various -O settings }
    NoCheck(s32_SideEffects-s32_SideEffects,0); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects xor s32_SideEffects,0); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects=s32_SideEffects, True); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects<=s32_SideEffects, True); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects>=s32_SideEffects, True); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects<>s32_SideEffects, False); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects<s32_SideEffects, False); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects>s32_SideEffects, False); CheckSideEffectsHappened;
    NoCheck(0*s32_SideEffects, 0); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects*0, 0); CheckSideEffectsHappened;
    NoCheck(0 and s32_SideEffects, 0); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects and 0, 0); CheckSideEffectsHappened;
    NoCheck(0 shr s32_SideEffects, 0); CheckSideEffectsHappened;
    NoCheck(0 shl s32_SideEffects, 0); CheckSideEffectsHappened;
    NoCheck(SarShortInt(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(SarShortInt(ShortInt($ff), u8_SideEffects), -1); CheckSideEffectsHappened;
    NoCheck(SarSmallInt(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(SarSmallInt(SmallInt($ffff), u8_SideEffects), -1); CheckSideEffectsHappened;
    NoCheck(SarLongInt(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(SarLongInt(LongInt($ffffffff), u8_SideEffects), -1); CheckSideEffectsHappened;
    NoCheck(SarInt64(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(SarInt64(Int64($ffffffffffffffff), u8_SideEffects), -1); CheckSideEffectsHappened;
    NoCheck(RorByte(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(RolByte(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(RorWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(RolWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(RorDWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(RolDWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(RorQWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(RolQWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
    NoCheck(RorByte($ff, u8_SideEffects), $ff); CheckSideEffectsHappened;
    NoCheck(RolByte($ff, u8_SideEffects), $ff); CheckSideEffectsHappened;
    NoCheck(RorWord($ffff, u8_SideEffects), $ffff); CheckSideEffectsHappened;
    NoCheck(RolWord($ffff, u8_SideEffects), $ffff); CheckSideEffectsHappened;
    NoCheck(RorDWord($ffffffff, u8_SideEffects), $ffffffff); CheckSideEffectsHappened;
    NoCheck(RolDWord($ffffffff, u8_SideEffects), $ffffffff); CheckSideEffectsHappened;
    NoCheck(RorQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckSideEffectsHappened;
    NoCheck(RolQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckSideEffectsHappened;
  end;
  Writeln('Ok!');
end.
