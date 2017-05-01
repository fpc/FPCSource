{ %OPT=-O4 }

program tb0627b;

{ tests for correctness various simplifications, done by the compiler }

{ we allow some expressions with side effects to be removed at the -O4
  optimization level, but we still disallow removing them for some of
  the simplificationss. This test checks that behaviour. }

{$mode objfpc}

{$ifdef CPU64}
  {$define CPU_HAS_INLINE_ROX_QWORD}
{$endif}

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

procedure CheckNoSideEffectsHappened;
begin
  if SideEffectsHappened then
  begin
    Writeln('Error! Side effects were expected to be removed, by they were not!');
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

    { we don't remove side effects for these: }
    NoCheck(s32_SideEffects-s32_SideEffects,0); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects xor s32_SideEffects,0); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects=s32_SideEffects, True); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects<=s32_SideEffects, True); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects>=s32_SideEffects, True); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects<>s32_SideEffects, False); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects<s32_SideEffects, False); CheckSideEffectsHappened;
    NoCheck(s32_SideEffects>s32_SideEffects, False); CheckSideEffectsHappened;

    { we remove side effects for these: }
      Check(0*s32_SideEffects, 0); CheckNoSideEffectsHappened;
      Check(s32_SideEffects*0, 0); CheckNoSideEffectsHappened;
      Check(0 and s32_SideEffects, 0); CheckNoSideEffectsHappened;
      Check(s32_SideEffects and 0, 0); CheckNoSideEffectsHappened;
      Check(0 shr s32_SideEffects, 0); CheckNoSideEffectsHappened;
      Check(0 shl s32_SideEffects, 0); CheckNoSideEffectsHappened;
      Check(SarShortInt(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(SarShortInt(ShortInt($ff), u8_SideEffects), -1); CheckNoSideEffectsHappened;
      Check(SarSmallInt(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(SarSmallInt(SmallInt($ffff), u8_SideEffects), -1); CheckNoSideEffectsHappened;
      Check(SarLongInt(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(SarLongInt(LongInt($ffffffff), u8_SideEffects), -1); CheckNoSideEffectsHappened;
      Check(SarInt64(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(SarInt64(Int64($ffffffffffffffff), u8_SideEffects), -1); CheckNoSideEffectsHappened;
      Check(RorByte(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RolByte(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RorWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RolWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RorDWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RolDWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
{$ifdef CPU_HAS_INLINE_ROX_QWORD}
      Check(RorQWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RolQWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
{$else CPU_HAS_INLINE_ROX_QWORD}
      Check(RorQWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(RolQWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
{$endif CPU_HAS_INLINE_ROX_QWORD}
      Check(RorByte($ff, u8_SideEffects), $ff); CheckNoSideEffectsHappened;
      Check(RolByte($ff, u8_SideEffects), $ff); CheckNoSideEffectsHappened;
      Check(RorWord($ffff, u8_SideEffects), $ffff); CheckNoSideEffectsHappened;
      Check(RolWord($ffff, u8_SideEffects), $ffff); CheckNoSideEffectsHappened;
      Check(RorDWord($ffffffff, u8_SideEffects), $ffffffff); CheckNoSideEffectsHappened;
      Check(RolDWord($ffffffff, u8_SideEffects), $ffffffff); CheckNoSideEffectsHappened;
{$ifdef CPU_HAS_INLINE_ROX_QWORD}
      Check(RorQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckNoSideEffectsHappened;
      Check(RolQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckNoSideEffectsHappened;
{$else CPU_HAS_INLINE_ROX_QWORD}
      Check(RorQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckSideEffectsHappened;
      Check(RolQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckSideEffectsHappened;
{$endif CPU_HAS_INLINE_ROX_QWORD}
  end;
  Writeln('Ok!');
end.
