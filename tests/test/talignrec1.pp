{ %OPT=-O2 }
program talignrec1;

{ Tests to see if constants and local variables of an aligned array type are correctly positioned in memory }

type
{$IFNDEF FPC}
  UIntPtr = NativeUInt;
{$ENDIF FPC}

  { An array of 4 chars aligned to a 32-byte boundary }
  TAlignedRecord = packed record
    I: LongInt; { Should start at byte 0 }
    B: Byte;    { Should start at byte 4 }
    S: Single;  { Should start at byte 5 }
  end align 32;
  
const
  TestConst: TAlignedRecord = (I: $1234; B: $56; S: 7.8);
  
var
  FirstEntry: TAlignedRecord;
  X: Byte;
  SecondEntry: TAlignedRecord;  
  ThirdEntry: TAlignedRecord;  
begin
{ not all targets do a 32 byte alignment of variables or constants properly, if they don't, just halt with 0 }
{$if defined(CPUX86_64) or defined(CPUAARCH64) or defined(CPUI386) or defined(CPUARM)}
{$if defined(linux) or defined(darwin) or defined(win32) or defined(win64)}
{$else}
  halt(0);
{$endif}
{$else}
  halt(0);
{$endif}

  if (UIntPtr(@TestConst) mod $20) <> 0 then
    begin
      WriteLn('FAIL: TestConst is not on a 32-byte boundary (address = $', HexStr(@TestConst), ')');
      Halt(1);
    end;

  if (UIntPtr(@FirstEntry) mod $20) <> 0 then
    begin
      WriteLn('FAIL: FirstEntry is not on a 32-byte boundary (address = $', HexStr(@FirstEntry), ')');
      Halt(1);
    end;

  if (UIntPtr(@SecondEntry) mod $20) <> 0 then
    begin
      WriteLn('FAIL: SecondEntry is not on a 32-byte boundary (address = $', HexStr(@SecondEntry), ')');
      Halt(1);
    end;

  if (UIntPtr(@ThirdEntry) mod $20) <> 0 then
    begin
      WriteLn('FAIL: ThirdEntry is not on a 32-byte boundary (address = $', HexStr(@ThirdEntry), ')');
      Halt(1);
    end;
    
  X := Byte(UIntPtr(@(TestConst.I)) mod $20);
  if X <> 0 then
    begin
      WriteLn('FAIL: TAlignedRecord.I starts at byte ', X, ' instead of 0');
      Halt(1);
    end;
    
  X := Byte(UIntPtr(@(TestConst.B)) mod $20);
  if X <> 4 then
    begin
      WriteLn('FAIL: TAlignedRecord.B starts at byte ', X, ' instead of 4');
      Halt(1);
    end;
    
  X := Byte(UIntPtr(@(TestConst.S)) mod $20);
  if X <> 5 then
    begin
      WriteLn('FAIL: TAlignedRecord.S starts at byte ', X, ' instead of 5');
      Halt(1);
    end;

  FirstEntry := TestConst;
  SecondEntry := TestConst;
  ThirdEntry := TestConst;
  
  { Check to see if FirstEntry's values are correctly assigned }
  if FirstEntry.I <> TestConst.I then
    begin
      WriteLn('FAIL: FirstEntry.I contains $', HexStr(FirstEntry.I, 4), ' rather than ', HexStr(TestConst.I, 4));
      Halt(1);
    end;
  
  if FirstEntry.B <> TestConst.B then
    begin
      WriteLn('FAIL: FirstEntry.b contains $', HexStr(FirstEntry.B, 2), ' rather than ', HexStr(TestConst.B, 2));
      Halt(1);
    end;
  
  if FirstEntry.S <> TestConst.S then
    begin
      WriteLn('FAIL: FirstEntry.b contains $', FirstEntry.S, ' rather than ', TestConst.S);
      Halt(1);
    end;
  
  { Check to see if SecondEntry's values are correctly assigned }
  if SecondEntry.I <> TestConst.I then
    begin
      WriteLn('FAIL: SecondEntry.I contains $', HexStr(SecondEntry.I, 4), ' rather than ', HexStr(TestConst.I, 4));
      Halt(1);
    end;
  
  if SecondEntry.B <> TestConst.B then
    begin
      WriteLn('FAIL: SecondEntry.b contains $', HexStr(SecondEntry.B, 2), ' rather than ', HexStr(TestConst.B, 2));
      Halt(1);
    end;
  
  if SecondEntry.S <> TestConst.S then
    begin
      WriteLn('FAIL: SecondEntry.b contains $', SecondEntry.S, ' rather than ', TestConst.S);
      Halt(1);
    end;
  
  { Check to see if ThirdEntry's values are correctly assigned }
  if ThirdEntry.I <> TestConst.I then
    begin
      WriteLn('FAIL: ThirdEntry.I contains $', HexStr(ThirdEntry.I, 4), ' rather than ', HexStr(TestConst.I, 4));
      Halt(1);
    end;
  
  if ThirdEntry.B <> TestConst.B then
    begin
      WriteLn('FAIL: ThirdEntry.b contains $', HexStr(ThirdEntry.B, 2), ' rather than ', HexStr(TestConst.B, 2));
      Halt(1);
    end;
  
  if ThirdEntry.S <> TestConst.S then
    begin
      WriteLn('FAIL: ThirdEntry.b contains $', ThirdEntry.S, ' rather than ', TestConst.S);
      Halt(1);
    end;

  WriteLn('ok');
end.