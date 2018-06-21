{ %OPT=-O4 }

program tb0627b;

{ tests for correctness various simplifications, done by the compiler }

{ we allow some expressions with side effects to be removed at the -O4
  optimization level, but we still disallow removing them for some of
  the simplificationss. This test checks that behaviour. }

{$mode objfpc}

{ unfortunately, for this test, we need to mirror some defines from systemh.inc :( }

{ if the test fails on some platform, please first check whether these defines
  are up to date with the ones in the rtl }

{------------------------------------------------------------------------------}

{$ifdef FPC_HAS_INTERNAL_ROX}

{$if defined(cpux86_64) or defined(cpui386) or defined(cpui8086)}
{$define FPC_HAS_INTERNAL_ROX_BYTE}
{$define FPC_HAS_INTERNAL_ROX_WORD}
{$endif defined(cpux86_64) or defined(cpui386) or defined(cpui8086)}

{$if defined(cpux86_64) or defined(cpui386) or defined(cpuarm) or defined(cpupowerpc) or defined(cpupowerpc64) or defined(cpuaarch64)}
{$define FPC_HAS_INTERNAL_ROX_DWORD}
{$endif defined(cpux86_64) or defined(cpui386) or defined(cpuarm) or defined(cpupowerpc) or defined(cpupowerpc64) or defined(cpuaarch64)}

{$if defined(cpux86_64) or defined(cpupowerpc64) or defined(cpuaarch64)}
{$define FPC_HAS_INTERNAL_ROX_QWORD}
{$define FPC_HAS_INTERNAL_ROX_ASSIGN_QWORD}
{$endif defined(cpux86_64) or defined(cpupowerpc64) or defined(cpuaarch64)}

{$endif FPC_HAS_INTERNAL_ROX}

{$ifdef FPC_HAS_INTERNAL_SAR}

{$if defined(cpux86_64) or defined(cpui386) or defined(cpui8086) or defined(cpumips) or defined(cpumipsel) or defined(cpusparc)}
{$define FPC_HAS_INTERNAL_SAR_BYTE}
{$define FPC_HAS_INTERNAL_SAR_WORD}
{$endif defined(cpux86_64) or defined(cpui386) or defined(cpui8086) or defined(cpumips) or defined(cpumipsel) or defined(cpusparc)}

{ currently, all supported CPUs have an internal 32 bit sar implementation }
{ $if defined(cpux86_64) or defined(cpui386) or defined(cpuarm) or defined(cpupowerpc) or defined(cpupowerpc64) or defined(cpumips) or defined(cpumipsel)}
{$define FPC_HAS_INTERNAL_SAR_DWORD}
{ $endif defined(cpux86_64) or defined(cpui386) or defined(cpuarm) or defined(cpupowerpc) or defined(cpupowerpc64) or defined(cpumips) or defined(cpumipsel)}

{$if defined(cpux86_64) or defined(cpupowerpc64) or defined(cpuaarch64)}
{$define FPC_HAS_INTERNAL_SAR_QWORD}
{$define FPC_HAS_INTERNAL_SAR_ASSIGN_QWORD}
{$endif defined(cpux86_64) or defined(cpupowerpc64) or defined(cpuaarch64)}

{$endif FPC_HAS_INTERNAL_SAR}

{------------------------------------------------------------------------------}

{ Explicitly disable overflow and range checks }
{$Q-}
{$R-}

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
  Write('+');
  if i64<>expected_value then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

procedure Check(qw, expected_value: QWord);
begin
  Write('+');
  if qw<>expected_value then
  begin
    Writeln('Error!');
    Halt(1);
  end;
end;

procedure Check(b, expected_value: Boolean);
begin
  Write('+');
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
{$ifdef FPC_HAS_INTERNAL_SAR_BYTE}
      Check(SarShortInt(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(SarShortInt(ShortInt($ff), u8_SideEffects), -1); CheckNoSideEffectsHappened;
{$else FPC_HAS_INTERNAL_SAR_BYTE}
      Check(SarShortInt(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(SarShortInt(ShortInt($ff), u8_SideEffects), -1); CheckSideEffectsHappened;
{$endif FPC_HAS_INTERNAL_SAR_BYTE}
{$ifdef FPC_HAS_INTERNAL_SAR_WORD}
      Check(SarSmallInt(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(SarSmallInt(SmallInt($ffff), u8_SideEffects), -1); CheckNoSideEffectsHappened;
{$else FPC_HAS_INTERNAL_SAR_WORD}
      Check(SarSmallInt(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(SarSmallInt(SmallInt($ffff), u8_SideEffects), -1); CheckSideEffectsHappened;
{$endif FPC_HAS_INTERNAL_SAR_WORD}
{$ifdef FPC_HAS_INTERNAL_SAR_DWORD}
      Check(SarLongInt(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(SarLongInt(LongInt($ffffffff), u8_SideEffects), -1); CheckNoSideEffectsHappened;
{$else FPC_HAS_INTERNAL_SAR_DWORD}
      Check(SarLongInt(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(SarLongInt(LongInt($ffffffff), u8_SideEffects), -1); CheckSideEffectsHappened;
{$endif FPC_HAS_INTERNAL_SAR_DWORD}
      { SAR_QWORD is always handled inline in the compiler, regardless of
        the FPC_HAS_INTERNAL_SAR_QWORD define }
//{$ifdef FPC_HAS_INTERNAL_SAR_QWORD}
      Check(SarInt64(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(SarInt64(Int64($ffffffffffffffff), u8_SideEffects), -1); CheckNoSideEffectsHappened;
//{$else FPC_HAS_INTERNAL_SAR_QWORD}
//      Check(SarInt64(0, u8_SideEffects), 0); CheckSideEffectsHappened;
//      Check(SarInt64(Int64($ffffffffffffffff), u8_SideEffects), -1); CheckSideEffectsHappened;
//{$endif FPC_HAS_INTERNAL_SAR_QWORD}
{$ifdef FPC_HAS_INTERNAL_ROX_BYTE}
      Check(RorByte(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RolByte(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RorByte($ff, u8_SideEffects), $ff); CheckNoSideEffectsHappened;
      Check(RolByte($ff, u8_SideEffects), $ff); CheckNoSideEffectsHappened;
{$else FPC_HAS_INTERNAL_ROX_BYTE}
      Check(RorByte(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(RolByte(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(RorByte($ff, u8_SideEffects), $ff); CheckSideEffectsHappened;
      Check(RolByte($ff, u8_SideEffects), $ff); CheckSideEffectsHappened;
{$endif FPC_HAS_INTERNAL_ROX_BYTE}
{$ifdef FPC_HAS_INTERNAL_ROX_WORD}
      Check(RorWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RolWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RorWord($ffff, u8_SideEffects), $ffff); CheckNoSideEffectsHappened;
      Check(RolWord($ffff, u8_SideEffects), $ffff); CheckNoSideEffectsHappened;
{$else FPC_HAS_INTERNAL_ROX_WORD}
      Check(RorWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(RolWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(RorWord($ffff, u8_SideEffects), $ffff); CheckSideEffectsHappened;
      Check(RolWord($ffff, u8_SideEffects), $ffff); CheckSideEffectsHappened;
{$endif FPC_HAS_INTERNAL_ROX_WORD}
{$ifdef FPC_HAS_INTERNAL_ROX_DWORD}
      Check(RorDWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RolDWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RorDWord($ffffffff, u8_SideEffects), $ffffffff); CheckNoSideEffectsHappened;
      Check(RolDWord($ffffffff, u8_SideEffects), $ffffffff); CheckNoSideEffectsHappened;
{$else FPC_HAS_INTERNAL_ROX_DWORD}
      Check(RorDWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(RolDWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(RorDWord($ffffffff, u8_SideEffects), $ffffffff); CheckSideEffectsHappened;
      Check(RolDWord($ffffffff, u8_SideEffects), $ffffffff); CheckSideEffectsHappened;
{$endif FPC_HAS_INTERNAL_ROX_DWORD}
{$ifdef FPC_HAS_INTERNAL_ROX_QWORD}
      Check(RorQWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RolQWord(0, u8_SideEffects), 0); CheckNoSideEffectsHappened;
      Check(RorQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckNoSideEffectsHappened;
      Check(RolQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckNoSideEffectsHappened;
{$else FPC_HAS_INTERNAL_ROX_QWORD}
      Check(RorQWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(RolQWord(0, u8_SideEffects), 0); CheckSideEffectsHappened;
      Check(RorQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckSideEffectsHappened;
      Check(RolQWord(QWord($ffffffffffffffff), u8_SideEffects), QWord($ffffffffffffffff)); CheckSideEffectsHappened;
{$endif FPC_HAS_INTERNAL_ROX_QWORD}
  end;
  Writeln('Ok!');
end.
