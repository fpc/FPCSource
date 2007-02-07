program SetTests;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

{$R+}
{$Q+}

uses
  SysUtils;

var
  u8      : Byte;
  s8      : ShortInt;
  u16     : Word;
  s16     : SmallInt;
  u32     : LongWord;
  s32     : LongInt;
  u64     : QWord;
  s64     : Int64;

  LargeSet : set of Byte;
  SmallSet : set of 0..31;

  Error    : Boolean;

procedure CheckResult(const s: string; aIs, aExpected: Boolean); overload;
begin
  if aIs <> aExpected then begin
    WriteLn(s, aIs, ' <> ', aExpected, '   * * * ERROR * * * ERROR * * * ERROR * * *');
    Error := True;
  end else
    WriteLn(s, aIs);
end;

procedure CheckResult(const s: string; aIs: Boolean); overload;
begin
  WriteLn(s, aIs, ' <> EXCEPTION   * * * ERROR * * * ERROR * * * ERROR * * *');
  Error := True;
end;


begin
  Error := False;

  WriteLn('--- Variable against constant set [0, 2, 8..20, 99..192] ---' );
  WriteLn;

  u8  := 100;
  s8  := 100;
  u16 := 100;
  s16 := 100;
  u32 := 100;
  s32 := 100;
  u64 := 100;
  s64 := 100;

  WriteLn('100, should be true');
  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20, 99..192], True);
  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20, 99..192], True);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20, 99..192], True);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20, 99..192], True);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20, 99..192], True);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20, 99..192], True);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20, 99..192], True);
  CheckResult('s64 -> ', s64 in [0, 2, 8..20, 99..192], True);
  WriteLn;

  u8  := 98;
  s8  := 98;
  u16 := 98;
  s16 := 98;
  u32 := 98;
  s32 := 98;
  u64 := 98;
  s64 := 98;

  WriteLn('98, should be false');
  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20, 99..192], False);
  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20, 99..192], False);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20, 99..192], False);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20, 99..192], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20, 99..192], False);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20, 99..192], False);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20, 99..192], False);
  CheckResult('s64 -> ', s64 in [0, 2, 8..20, 99..192], False);
  WriteLn;

  u8  := 193;
//  s8  := 193;
  u16 := 193;
  s16 := 193;
  u32 := 193;
  s32 := 193;
  u64 := 193;
  s64 := 193;

  WriteLn('193, should be false');
  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20, 99..192], False);
//  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20, 99..192], False);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20, 99..192], False);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20, 99..192], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20, 99..192], False);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20, 99..192], False);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20, 99..192], False);
  CheckResult('s64 -> ', s64 in [0, 2, 8..20, 99..192], False);
  WriteLn;

//  u8  := 256;
//  s8  := 256;
  u16 := 256;
  s16 := 256;
  u32 := 256;
  s32 := 256;
  u64 := 256;
  s64 := 256;

  WriteLn('256, should be false');
//  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20, 99..192], False);
//  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20, 99..192], False);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20, 99..192], False);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20, 99..192], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20, 99..192], False);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20, 99..192], False);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20, 99..192], False);
  CheckResult('s64 -> ', s64 in [0, 2, 8..20, 99..192], False);
  WriteLn;

  u8  := High(u8);
  s8  := High(s8);
  u16 := High(u16);
  s16 := High(s16);
  u32 := High(u32);
  s32 := High(s32);
  u64 := High(u64);
  s64 := High(s64);

  WriteLn('High(type), s8 should be true, u64/s64 should cause range check');
  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20, 99..192], False);
  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20, 99..192], True);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20, 99..192], False);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20, 99..192], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20, 99..192], False);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20, 99..192], False);
  try
    CheckResult('u64 -> ', u64 in [0, 2, 8..20, 99..192]);
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
    CheckResult('s64 -> ', s64 in [0, 2, 8..20, 99..192]);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  u8  := Low(u8);
  s8  := Low(s8);
  u16 := Low(u16);
  s16 := Low(s16);
  u32 := Low(u32);
  s32 := Low(s32);
  u64 := Low(u64);
  s64 := Low(s64);

  WriteLn('Low(type), all unsigned true, all signed false, except s64 -> range check error');
  CheckResult('u8  -> ',  u8 in [0, 2, 8..20, 99..192], True);
  CheckResult('s8  -> ',  s8 in [0, 2, 8..20, 99..192], False);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20, 99..192], True);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20, 99..192], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20, 99..192], True);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20, 99..192], False);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20, 99..192], True);

  try
    CheckResult('s64 -> ', s64 in [0, 2, 8..20, 99..192]);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- Variable against set of byte with value [0, 2, 8..20, 99..192] ---' );
  WriteLn;

  LargeSet := [0, 2, 8..20, 99..192];

  u8  := 100;
  s8  := 100;
  u16 := 100;
  s16 := 100;
  u32 := 100;
  s32 := 100;
  u64 := 100;
  s64 := 100;

  WriteLn('100, should be true');
  CheckResult(' u8 -> ',  u8 in LargeSet, True);
  CheckResult(' s8 -> ',  s8 in LargeSet, True);
  CheckResult('u16 -> ', u16 in LargeSet, True);
  CheckResult('s16 -> ', s16 in LargeSet, True);
  CheckResult('u32 -> ', u32 in LargeSet, True);
  CheckResult('s32 -> ', s32 in LargeSet, True);
  CheckResult('u64 -> ', u64 in LargeSet, True);
  CheckResult('s64 -> ', s64 in LargeSet, True);
  WriteLn;

  u8  := 98;
  s8  := 98;
  u16 := 98;
  s16 := 98;
  u32 := 98;
  s32 := 98;
  u64 := 98;
  s64 := 98;

  WriteLn('98, should be false');
  CheckResult(' u8 -> ',  u8 in LargeSet, False);
  CheckResult(' s8 -> ',  s8 in LargeSet, False);
  CheckResult('u16 -> ', u16 in LargeSet, False);
  CheckResult('s16 -> ', s16 in LargeSet, False);
  CheckResult('u32 -> ', u32 in LargeSet, False);
  CheckResult('s32 -> ', s32 in LargeSet, False);
  CheckResult('u64 -> ', u64 in LargeSet, False);
  CheckResult('s64 -> ', s64 in LargeSet, False);
  WriteLn;

  u8  := 193;
//  s8  := 193;
  u16 := 193;
  s16 := 193;
  u32 := 193;
  s32 := 193;
  u64 := 193;
  s64 := 193;

  WriteLn('193, should be false');
  CheckResult(' u8 -> ',  u8 in LargeSet, False);
//  CheckResult(' s8 -> ',  s8 in LargeSet, False);
  CheckResult('u16 -> ', u16 in LargeSet, False);
  CheckResult('s16 -> ', s16 in LargeSet, False);
  CheckResult('u32 -> ', u32 in LargeSet, False);
  CheckResult('s32 -> ', s32 in LargeSet, False);
  CheckResult('u64 -> ', u64 in LargeSet, False);
  CheckResult('s64 -> ', s64 in LargeSet, False);
  WriteLn;

//  u8  := 256;
//  s8  := 256;
  u16 := 256;
  s16 := 256;
  u32 := 256;
  s32 := 256;
  u64 := 256;
  s64 := 256;

  WriteLn('256, should be false');
//  CheckResult(' u8 -> ',  u8 in LargeSet, False);
//  CheckResult(' s8 -> ',  s8 in LargeSet, False);
  CheckResult('u16 -> ', u16 in LargeSet, False);
  CheckResult('s16 -> ', s16 in LargeSet, False);
  CheckResult('u32 -> ', u32 in LargeSet, False);
  CheckResult('s32 -> ', s32 in LargeSet, False);
  CheckResult('u64 -> ', u64 in LargeSet, False);
  CheckResult('s64 -> ', s64 in LargeSet, False);
  WriteLn;

  u8  := High(u8);
  s8  := High(s8);
  u16 := High(u16);
  s16 := High(s16);
  u32 := High(u32);
  s32 := High(s32);
  u64 := High(u64);
  s64 := High(s64);

  WriteLn('High(type), s8 should be true, u64/s64 should cause range check');
  CheckResult(' u8 -> ',  u8 in LargeSet, False);
  CheckResult(' s8 -> ',  s8 in LargeSet, True);
  CheckResult('u16 -> ', u16 in LargeSet, False);
  CheckResult('s16 -> ', s16 in LargeSet, False);
  CheckResult('u32 -> ', u32 in LargeSet, False);
  CheckResult('s32 -> ', s32 in LargeSet, False);
  try
    CheckResult('u64 -> ', u64 in LargeSet);
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
    CheckResult('s64 -> ', s64 in LargeSet);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  u8  := Low(u8);
  s8  := Low(s8);
  u16 := Low(u16);
  s16 := Low(s16);
  u32 := Low(u32);
  s32 := Low(s32);
  u64 := Low(u64);
  s64 := Low(s64);

  WriteLn('Low(type), all unsigned true, all signed false, except s64 -> range check error');
  CheckResult('u8  -> ',  u8 in LargeSet, True);
  CheckResult('s8  -> ',  s8 in LargeSet, False);
  CheckResult('u16 -> ', u16 in LargeSet, True);
  CheckResult('s16 -> ', s16 in LargeSet, False);
  CheckResult('u32 -> ', u32 in LargeSet, True);
  CheckResult('s32 -> ', s32 in LargeSet, False);
  CheckResult('u64 -> ', u64 in LargeSet, True);

  try
    CheckResult('s64 -> ', s64 in LargeSet);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- constant value against constant set [0, 2, 8..20, 99..192] ---' );
  WriteLn;

  WriteLn('100, should be true');
  CheckResult('100 -> ', 100 in [0, 2, 8..20, 99..192], True);
  WriteLn;

  WriteLn('98, should be false');
  CheckResult(' 98 -> ', 98 in [0, 2, 8..20, 99..192], False);
  WriteLn;

  WriteLn('193, should be false');
  CheckResult('193 -> ', 193 in [0, 2, 8..20, 99..192], False);
  WriteLn;

  WriteLn('256, should be false');
  CheckResult('256 -> ', 256 in [0, 2, 8..20, 99..192], False);
  WriteLn;

  WriteLn('High(type), s8 should be true, u64/s64 should cause range check at compile time');
  CheckResult(' u8 -> ', High(u8)  in [0, 2, 8..20, 99..192], False);
  CheckResult(' s8 -> ', High(s8)  in [0, 2, 8..20, 99..192], True);
  CheckResult('u16 -> ', High(u16) in [0, 2, 8..20, 99..192], False);
  CheckResult('s16 -> ', High(s16) in [0, 2, 8..20, 99..192], False);
  CheckResult('u32 -> ', High(u32) in [0, 2, 8..20, 99..192], False);
  CheckResult('s32 -> ', High(s32) in [0, 2, 8..20, 99..192], False);
  try
//    CheckResult('u64 -> ', High(u64) in [0, 2, 8..20, 99..192]);
    WriteLn('u64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
//    CheckResult('s64 -> ', High(s64) in [0, 2, 8..20, 99..192]);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('Low(type), all unsigned true, all signed false, except s64 -> range check error at compile time');
  CheckResult('u8  -> ',  Low(u8) in [0, 2, 8..20, 99..192], True);
  CheckResult('s8  -> ',  Low(s8) in [0, 2, 8..20, 99..192], False);
  CheckResult('u16 -> ', Low(u16) in [0, 2, 8..20, 99..192], True);
  CheckResult('s16 -> ', Low(s16) in [0, 2, 8..20, 99..192], False);
  CheckResult('u32 -> ', Low(u32) in [0, 2, 8..20, 99..192], True);
  CheckResult('s32 -> ', Low(s32) in [0, 2, 8..20, 99..192], False);
  CheckResult('u64 -> ', Low(u64) in [0, 2, 8..20, 99..192], True);

  try
//    CheckResult('s64 -> ', Low(s64) in [0, 2, 8..20, 99..192]);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- constant value against set of byte with value [0, 2, 8..20, 99..192] ---' );
  WriteLn;

  LargeSet := [0, 2, 8..20, 99..192];

  WriteLn('100, should be true');
  CheckResult('100 -> ', 100 in LargeSet, True);
  WriteLn;

  WriteLn('98, should be false');
  CheckResult(' 98 -> ',  98 in LargeSet, False);
  WriteLn;

  WriteLn('193, should be false');
  CheckResult('193 -> ', 193 in LargeSet, False);
  WriteLn;

  WriteLn('256, should be false');
  CheckResult('256 -> ', 256 in LargeSet, False);
  WriteLn;

  WriteLn('High(type), s8 should be true, u64/s64 should cause range check at compile time');
  CheckResult(' u8 -> ',  High(u8) in LargeSet, False);
  CheckResult(' s8 -> ',  High(s8) in LargeSet, True);
  CheckResult('u16 -> ', High(u16) in LargeSet, False);
  CheckResult('s16 -> ', High(s16) in LargeSet, False);
  CheckResult('u32 -> ', High(u32) in LargeSet, False);
  CheckResult('s32 -> ', High(s32) in LargeSet, False);
  try
//    CheckResult('u64 -> ', High(u64) in LargeSet);
    WriteLn('u64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
//    CheckResult('s64 -> ', High(s64) in LargeSet);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;


  WriteLn('Low(type), all unsigned true, all signed false, except s64 -> range check error at compile time');
  CheckResult(' u8 -> ',  Low(u8) in LargeSet, True);
  CheckResult(' s8 -> ',  Low(s8) in LargeSet, False);
  CheckResult('u16 -> ', Low(u16) in LargeSet, True);
  CheckResult('s16 -> ', Low(s16) in LargeSet, False);
  CheckResult('u32 -> ', Low(u32) in LargeSet, True);
  CheckResult('s32 -> ', Low(s32) in LargeSet, False);
  CheckResult('u64 -> ', Low(u64) in LargeSet, True);
  try
//    CheckResult('s64 -> ', Low(s64) in LargeSet);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- Variable against constant set [0, 2, 8..20] ---' );
  WriteLn;

  u8  := 10;
  s8  := 10;
  u16 := 10;
  s16 := 10;
  u32 := 10;
  s32 := 10;
  u64 := 10;
  s64 := 10;

  WriteLn('10, should be true');
  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20], True);
  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20], True);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20], True);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20], True);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20], True);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20], True);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20], True);
  CheckResult('s64 -> ', s64 in [0, 2, 8..20], True);
  WriteLn;

  u8  := 7;
  s8  := 7;
  u16 := 7;
  s16 := 7;
  u32 := 7;
  s32 := 7;
  u64 := 7;
  s64 := 7;

  WriteLn('7, should be false');
  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20], False);
  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20], False);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20], False);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20], False);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20], False);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20], False);
  CheckResult('s64 -> ', s64 in [0, 2, 8..20], False);
  WriteLn;

  u8  := 30;
  s8  := 30;
  u16 := 30;
  s16 := 30;
  u32 := 30;
  s32 := 30;
  u64 := 30;
  s64 := 30;

  WriteLn('30, should be false');
  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20], False);
  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20], False);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20], False);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20], False);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20], False);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20], False);
  CheckResult('s64 -> ', s64 in [0, 2, 8..20], False);
  WriteLn;

//  u8  := 256;
//  s8  := 256;
  u16 := 256;
  s16 := 256;
  u32 := 256;
  s32 := 256;
  u64 := 256;
  s64 := 256;

  WriteLn('256, should be false');
//  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20], False);
//  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20], False);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20], False);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20], False);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20], False);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20], False);
  CheckResult('s64 -> ', s64 in [0, 2, 8..20], False);
  WriteLn;

  u8  := High(u8);
  s8  := High(s8);
  u16 := High(u16);
  s16 := High(s16);
  u32 := High(u32);
  s32 := High(s32);
  u64 := High(u64);
  s64 := High(s64);

  WriteLn('High(type), should be false, u64/s64 should cause range check');
  CheckResult(' u8 -> ',  u8 in [0, 2, 8..20], False);
  CheckResult(' s8 -> ',  s8 in [0, 2, 8..20], False);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20], False);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20], False);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20], False);
  try
    CheckResult('u64 -> ', u64 in [0, 2, 8..20]);
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
    CheckResult('s64 -> ', s64 in [0, 2, 8..20]);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  u8  := Low(u8);
  s8  := Low(s8);
  u16 := Low(u16);
  s16 := Low(s16);
  u32 := Low(u32);
  s32 := Low(s32);
  u64 := Low(u64);
  s64 := Low(s64);

  WriteLn('Low(type), all unsigned true, all signed false, except s64 -> range check error');
  CheckResult('u8  -> ',  u8 in [0, 2, 8..20], True);
  CheckResult('s8  -> ',  s8 in [0, 2, 8..20], False);
  CheckResult('u16 -> ', u16 in [0, 2, 8..20], True);
  CheckResult('s16 -> ', s16 in [0, 2, 8..20], False);
  CheckResult('u32 -> ', u32 in [0, 2, 8..20], True);
  CheckResult('s32 -> ', s32 in [0, 2, 8..20], False);
  CheckResult('u64 -> ', u64 in [0, 2, 8..20], True);

  try
    CheckResult('s64 -> ', s64 in [0, 2, 8..20]);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- Variable against set of 0..31 with value [0, 2, 8..20] ---' );
  WriteLn;

  SmallSet := [0, 2, 8..20];

  u8  := 10;
  s8  := 10;
  u16 := 10;
  s16 := 10;
  u32 := 10;
  s32 := 10;
  u64 := 10;
  s64 := 10;

  WriteLn('10, should be true');
  CheckResult(' u8 -> ',  u8 in SmallSet, True);
  CheckResult(' s8 -> ',  s8 in SmallSet, True);
  CheckResult('u16 -> ', u16 in SmallSet, True);
  CheckResult('s16 -> ', s16 in SmallSet, True);
  CheckResult('u32 -> ', u32 in SmallSet, True);
  CheckResult('s32 -> ', s32 in SmallSet, True);
  CheckResult('u64 -> ', u64 in SmallSet, True);
  CheckResult('s64 -> ', s64 in SmallSet, True);
  WriteLn;

  u8  := 7;
  s8  := 7;
  u16 := 7;
  s16 := 7;
  u32 := 7;
  s32 := 7;
  u64 := 7;
  s64 := 7;

  WriteLn('7, should be false');
  CheckResult(' u8 -> ',  u8 in SmallSet, False);
  CheckResult(' s8 -> ',  s8 in SmallSet, False);
  CheckResult('u16 -> ', u16 in SmallSet, False);
  CheckResult('s16 -> ', s16 in SmallSet, False);
  CheckResult('u32 -> ', u32 in SmallSet, False);
  CheckResult('s32 -> ', s32 in SmallSet, False);
  CheckResult('u64 -> ', u64 in SmallSet, False);
  CheckResult('s64 -> ', s64 in SmallSet, False);
  WriteLn;

  u8  := 30;
  s8  := 30;
  u16 := 30;
  s16 := 30;
  u32 := 30;
  s32 := 30;
  u64 := 30;
  s64 := 30;

  WriteLn('30, should be false');
  CheckResult(' u8 -> ',  u8 in SmallSet, False);
  CheckResult(' s8 -> ',  s8 in SmallSet, False);
  CheckResult('u16 -> ', u16 in SmallSet, False);
  CheckResult('s16 -> ', s16 in SmallSet, False);
  CheckResult('u32 -> ', u32 in SmallSet, False);
  CheckResult('s32 -> ', s32 in SmallSet, False);
  CheckResult('u64 -> ', u64 in SmallSet, False);
  CheckResult('s64 -> ', s64 in SmallSet, False);
  WriteLn;

//  u8  := 256;
//  s8  := 256;
  u16 := 256;
  s16 := 256;
  u32 := 256;
  s32 := 256;
  u64 := 256;
  s64 := 256;

  WriteLn('256, should be false');
//  CheckResult(' u8 -> ',  u8 in SmallSet, False);
//  CheckResult(' s8 -> ',  s8 in SmallSet, False);
  CheckResult('u16 -> ', u16 in SmallSet, False);
  CheckResult('s16 -> ', s16 in SmallSet, False);
  CheckResult('u32 -> ', u32 in SmallSet, False);
  CheckResult('s32 -> ', s32 in SmallSet, False);
  CheckResult('u64 -> ', u64 in SmallSet, False);
  CheckResult('s64 -> ', s64 in SmallSet, False);
  WriteLn;

  u8  := High(u8);
  s8  := High(s8);
  u16 := High(u16);
  s16 := High(s16);
  u32 := High(u32);
  s32 := High(s32);
  u64 := High(u64);
  s64 := High(s64);

  WriteLn('High(type), should be false, u64/s64 should cause range check');
  CheckResult(' u8 -> ',  u8 in SmallSet, False);
  CheckResult(' s8 -> ',  s8 in SmallSet, False);
  CheckResult('u16 -> ', u16 in SmallSet, False);
  CheckResult('s16 -> ', s16 in SmallSet, False);
  CheckResult('u32 -> ', u32 in SmallSet, False);
  CheckResult('s32 -> ', s32 in SmallSet, False);
  try
    CheckResult('u64 -> ', u64 in SmallSet);
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
    CheckResult('s64 -> ', s64 in SmallSet);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  u8  := Low(u8);
  s8  := Low(s8);
  u16 := Low(u16);
  s16 := Low(s16);
  u32 := Low(u32);
  s32 := Low(s32);
  u64 := Low(u64);
  s64 := Low(s64);

  WriteLn('Low(type), all unsigned true, all signed false, except s64 -> range check error');
  CheckResult('u8  -> ',  u8 in SmallSet, True);
  CheckResult('s8  -> ',  s8 in SmallSet, False);
  CheckResult('u16 -> ', u16 in SmallSet, True);
  CheckResult('s16 -> ', s16 in SmallSet, False);
  CheckResult('u32 -> ', u32 in SmallSet, True);
  CheckResult('s32 -> ', s32 in SmallSet, False);
  CheckResult('u64 -> ', u64 in SmallSet, True);

  try
    CheckResult('s64 -> ', s64 in SmallSet);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- constant value against constant set [0, 2, 8..20] ---' );
  WriteLn;

  WriteLn('10, should be true');
  CheckResult('10 -> ', 10 in [0, 2, 8..20], True);
  WriteLn;

  WriteLn('7, should be false');
  CheckResult(' 7 -> ', 7 in [0, 2, 8..20], False);
  WriteLn;

  WriteLn('30, should be false');
  CheckResult('30 -> ', 30 in [0, 2, 8..20], False);
  WriteLn;

  WriteLn('256, should be false');
  CheckResult('256 -> ', 256 in [0, 2, 8..20], False);
  WriteLn;

  WriteLn('High(type), should be false, u64/s64 should cause range check at compile time');
  CheckResult(' u8 -> ', High(u8)  in [0, 2, 8..20], False);
  CheckResult(' s8 -> ', High(s8)  in [0, 2, 8..20], False);
  CheckResult('u16 -> ', High(u16) in [0, 2, 8..20], False);
  CheckResult('s16 -> ', High(s16) in [0, 2, 8..20], False);
  CheckResult('u32 -> ', High(u32) in [0, 2, 8..20], False);
  CheckResult('s32 -> ', High(s32) in [0, 2, 8..20], False);
  try
//    CheckResult('u64 -> ', High(u64) in [0, 2, 8..20]);
    WriteLn('u64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
//    CheckResult('s64 -> ', High(s64) in [0, 2, 8..20]);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('Low(type), all unsigned true, all signed false, except s64 -> range check error at compile time');
  CheckResult('u8  -> ',  Low(u8) in [0, 2, 8..20], True);
  CheckResult('s8  -> ',  Low(s8) in [0, 2, 8..20], False);
  CheckResult('u16 -> ', Low(u16) in [0, 2, 8..20], True);
  CheckResult('s16 -> ', Low(s16) in [0, 2, 8..20], False);
  CheckResult('u32 -> ', Low(u32) in [0, 2, 8..20], True);
  CheckResult('s32 -> ', Low(s32) in [0, 2, 8..20], False);
  CheckResult('u64 -> ', Low(u64) in [0, 2, 8..20], True);

  try
//    CheckResult('s64 -> ', Low(s64) in [0, 2, 8..20]);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- constant value against set of 0..31 with value [0, 2, 8..20] ---' );
  WriteLn;

  SmallSet := [0, 2, 8..20];

  WriteLn('10, should be true');
  CheckResult('10 -> ', 10 in SmallSet, True);
  WriteLn;

  WriteLn('7, should be false');
  CheckResult(' 7 -> ',  7 in SmallSet, False);
  WriteLn;

  WriteLn('30, should be false');
  CheckResult('30 -> ', 30 in SmallSet, False);
  WriteLn;

  WriteLn('256, should be false');
  CheckResult('256 -> ', 256 in SmallSet, False);
  WriteLn;

  WriteLn('High(type), all false, u64/s64 should cause range check at compile time');
  CheckResult(' u8 -> ',  High(u8) in SmallSet, False);
  CheckResult(' s8 -> ',  High(s8) in SmallSet, False);
  CheckResult('u16 -> ', High(u16) in SmallSet, False);
  CheckResult('s16 -> ', High(s16) in SmallSet, False);
  CheckResult('u32 -> ', High(u32) in SmallSet, False);
  CheckResult('s32 -> ', High(s32) in SmallSet, False);
  try
//    CheckResult('u64 -> ', High(u64) in SmallSet);
    WriteLn('u64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
//    CheckResult('s64 -> ', High(s64) in SmallSet);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;


  WriteLn('Low(type), all unsigned true, all signed false, except s64 -> range check error at compile time');
  CheckResult(' u8 -> ',  Low(u8) in SmallSet, True);
  CheckResult(' s8 -> ',  Low(s8) in SmallSet, False);
  CheckResult('u16 -> ', Low(u16) in SmallSet, True);
  CheckResult('s16 -> ', Low(s16) in SmallSet, False);
  CheckResult('u32 -> ', Low(u32) in SmallSet, True);
  CheckResult('s32 -> ', Low(s32) in SmallSet, False);
  CheckResult('u64 -> ', Low(u64) in SmallSet, True);
  try
//    CheckResult('s64 -> ', Low(s64) in SmallSet);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- Variable against constant set [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41] ---' );
  WriteLn;

  u8  := 25;
  s8  := 25;
  u16 := 25;
  s16 := 25;
  u32 := 25;
  s32 := 25;
  u64 := 25;
  s64 := 25;

  WriteLn('25, should be true');
  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], True);
  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], True);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], True);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], True);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], True);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], True);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], True);
  CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], True);
  WriteLn;

  u8  := 26;
  s8  := 26;
  u16 := 26;
  s16 := 26;
  u32 := 26;
  s32 := 26;
  u64 := 26;
  s64 := 26;

  WriteLn('26, should be false');
  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  WriteLn;

  u8  := 49;
  s8  := 49;
  u16 := 49;
  s16 := 49;
  u32 := 49;
  s32 := 49;
  u64 := 49;
  s64 := 49;

  WriteLn('49, should be false');
  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  WriteLn;

//  u8  := 256;
//  s8  := 256;
  u16 := 256;
  s16 := 256;
  u32 := 256;
  s32 := 256;
  u64 := 256;
  s64 := 256;

  WriteLn('256, should be false');
//  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
//  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  WriteLn;

  u8  := High(u8);
  s8  := High(s8);
  u16 := High(u16);
  s16 := High(s16);
  u32 := High(u32);
  s32 := High(s32);
  u64 := High(u64);
  s64 := High(s64);

  WriteLn('High(type), should be false, u64/s64 should cause range check');
  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  try
    CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41]);
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
    CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41]);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  u8  := Low(u8);
  s8  := Low(s8);
  u16 := Low(u16);
  s16 := Low(s16);
  u32 := Low(u32);
  s32 := Low(s32);
  u64 := Low(u64);
  s64 := Low(s64);

  WriteLn('Low(type), all false, except s64 -> range check error');
  CheckResult('u8  -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s8  -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);

  try
    CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41]);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- constant value against constant set [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41] ---' );
  WriteLn;

  WriteLn('25, should be true');
  CheckResult('25 -> ', 25 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], True);
  WriteLn;

  WriteLn('26, should be false');
  CheckResult(' 26 -> ', 26 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  WriteLn;

  WriteLn('49, should be false');
  CheckResult('49 -> ', 49 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  WriteLn;

  WriteLn('256, should be false');
  CheckResult('256 -> ', 256 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  WriteLn;

  WriteLn('High(type), should be false, u64/s64 should cause range check at compile time');
  CheckResult(' u8 -> ', High(u8)  in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult(' s8 -> ', High(s8)  in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u16 -> ', High(u16) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s16 -> ', High(s16) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u32 -> ', High(u32) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s32 -> ', High(s32) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  try
//    CheckResult('u64 -> ', High(u64) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
    WriteLn('u64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
//    CheckResult('s64 -> ', High(s64) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('Low(type), all false, except s64 -> range check error at compile time');
  CheckResult('u8  -> ',  Low(u8) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s8  -> ',  Low(s8) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u16 -> ', Low(u16) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s16 -> ', Low(s16) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u32 -> ', Low(u32) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('s32 -> ', Low(s32) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
  CheckResult('u64 -> ', Low(u64) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);

  try
//    CheckResult('s64 -> ', Low(s64) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41], False);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- Variable against constant set [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29] ---' );
  WriteLn;

  u8  := 25;
  s8  := 25;
  u16 := 25;
  s16 := 25;
  u32 := 25;
  s32 := 25;
  u64 := 25;
  s64 := 25;

  WriteLn('25, should be true');
  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], True);
  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], True);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], True);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], True);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], True);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], True);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], True);
  CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], True);
  WriteLn;

  u8  := 26;
  s8  := 26;
  u16 := 26;
  s16 := 26;
  u32 := 26;
  s32 := 26;
  u64 := 26;
  s64 := 26;

  WriteLn('26, should be false');
  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  WriteLn;

  u8  := 30;
  s8  := 30;
  u16 := 30;
  s16 := 30;
  u32 := 30;
  s32 := 30;
  u64 := 30;
  s64 := 30;

  WriteLn('30, should be false');
  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  WriteLn;

//  u8  := 256;
//  s8  := 256;
  u16 := 256;
  s16 := 256;
  u32 := 256;
  s32 := 256;
  u64 := 256;
  s64 := 256;

  WriteLn('256, should be false');
//  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
//  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  WriteLn;

  u8  := High(u8);
  s8  := High(s8);
  u16 := High(u16);
  s16 := High(s16);
  u32 := High(u32);
  s32 := High(s32);
  u64 := High(u64);
  s64 := High(s64);

  WriteLn('High(type), should be false, u64/s64 should cause range check');
  CheckResult(' u8 -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult(' s8 -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  try
    CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29]);
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
    CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29]);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  u8  := Low(u8);
  s8  := Low(s8);
  u16 := Low(u16);
  s16 := Low(s16);
  u32 := Low(u32);
  s32 := Low(s32);
  u64 := Low(u64);
  s64 := Low(s64);

  WriteLn('Low(type), all false, except s64 -> range check error');
  CheckResult('u8  -> ',  u8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s8  -> ',  s8 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u16 -> ', u16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s16 -> ', s16 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u32 -> ', u32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s32 -> ', s32 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u64 -> ', u64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);

  try
    CheckResult('s64 -> ', s64 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29]);
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('--- constant value against constant set [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29] ---' );
  WriteLn;

  WriteLn('25, should be true');
  CheckResult('25 -> ', 25 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], True);
  WriteLn;

  WriteLn('26, should be false');
  CheckResult(' 26 -> ', 26 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  WriteLn;

  WriteLn('30, should be false');
  CheckResult('30 -> ', 30 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  WriteLn;

  WriteLn('256, should be false');
  CheckResult('256 -> ', 256 in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  WriteLn;

  WriteLn('High(type), should be false, u64/s64 should cause range check at compile time');
  CheckResult(' u8 -> ', High(u8)  in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult(' s8 -> ', High(s8)  in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u16 -> ', High(u16) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s16 -> ', High(s16) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u32 -> ', High(u32) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s32 -> ', High(s32) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  try
//    CheckResult('u64 -> ', High(u64) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
    WriteLn('u64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('u64 -> ', E.Classname,': ',E.Message);
  end;
  try
//    CheckResult('s64 -> ', High(s64) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  WriteLn('Low(type), all false, except s64 -> range check error at compile time');
  CheckResult('u8  -> ',  Low(u8) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s8  -> ',  Low(s8) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u16 -> ', Low(u16) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s16 -> ', Low(s16) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u32 -> ', Low(u32) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('s32 -> ', Low(s32) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
  CheckResult('u64 -> ', Low(u64) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);

  try
//    CheckResult('s64 -> ', Low(s64) in [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29], False);
    WriteLn('s64 -> Error: range check error while evaluating constants');
  except
    on E: Exception do
      WriteLn('s64 -> ', E.Classname,': ',E.Message);
  end;
  WriteLn;

  if Error then begin
    WriteLn('* * * ERROR * * * ERROR * * * ERROR * * * ERROR * * * ERROR * * * ERROR * * *');
    Halt(1);
  end else
    Halt(0);
end.
