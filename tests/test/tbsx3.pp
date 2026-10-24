{ Overloaded Bsf and Bsr: same results as the size-suffixed functions, for variables and constants of each unsigned type }
program tbsx3;

{$mode objfpc}

var
  gErrors: Integer = 0;


procedure Check(const aName: string; aActual, aExpected: QWord);

begin
  if aActual<>aExpected then
    begin
    Writeln(aName,': ',aActual,' expected ',aExpected,' ERROR');
    Inc(gErrors);
    end;
end;


// Compares Bsf/Bsr with the suffixed functions for single bits, bit pairs and all-ones patterns of each size.
procedure CheckAllBits;

var
  i: Integer;
  lB: Byte;
  lW: Word;
  lD: DWord;
  lQ: QWord;

begin
  for i:=0 to 7 do
    begin
    lB:=Byte(1) shl i;
    Check('Bsf Byte single bit', Bsf(lB), i);
    Check('Bsr Byte single bit', Bsr(lB), i);
    lB:=Byte($FF) shl i;
    Check('Bsf Byte ones', Bsf(lB), BsfByte(lB));
    Check('Bsr Byte ones', Bsr(lB), BsrByte(lB));
    end;
  for i:=0 to 15 do
    begin
    lW:=Word(1) shl i;
    Check('Bsf Word single bit', Bsf(lW), i);
    Check('Bsr Word single bit', Bsr(lW), i);
    lW:=lW or 1;
    Check('Bsf Word pair', Bsf(lW), BsfWord(lW));
    Check('Bsr Word pair', Bsr(lW), BsrWord(lW));
    end;
  for i:=0 to 31 do
    begin
    lD:=DWord(1) shl i;
    Check('Bsf DWord single bit', Bsf(lD), i);
    Check('Bsr DWord single bit', Bsr(lD), i);
    lD:=lD or $80000000;
    Check('Bsf DWord pair', Bsf(lD), BsfDWord(lD));
    Check('Bsr DWord pair', Bsr(lD), BsrDWord(lD));
    end;
  for i:=0 to 63 do
    begin
    lQ:=QWord(1) shl i;
    Check('Bsf QWord single bit', Bsf(lQ), i);
    Check('Bsr QWord single bit', Bsr(lQ), i);
    lQ:=lQ or 1;
    Check('Bsf QWord pair', Bsf(lQ), BsfQWord(lQ));
    Check('Bsr QWord pair', Bsr(lQ), BsrQWord(lQ));
    end;
end;


var
  lB: Byte;
  lW: Word;
  lD: DWord;
  lQ: QWord;
  lC: Cardinal;
  lP: PtrUInt;
  lN: NativeUInt;

begin
  CheckAllBits;

  lB:=0;
  lW:=0;
  lD:=0;
  lQ:=0;
  Check('Bsf Byte zero', Bsf(lB), 255);
  Check('Bsr Byte zero', Bsr(lB), 255);
  Check('Bsf Word zero', Bsf(lW), 255);
  Check('Bsr Word zero', Bsr(lW), 255);
  Check('Bsf DWord zero', Bsf(lD), 255);
  Check('Bsr DWord zero', Bsr(lD), 255);
  Check('Bsf QWord zero', Bsf(lQ), 255);
  Check('Bsr QWord zero', Bsr(lQ), 255);

  lW:=$0100;
  Check('Bsr Word above the low byte', Bsr(lW), 8);
  lD:=$00010000;
  Check('Bsf DWord above the low word', Bsf(lD), 16);
  lQ:=QWord($100000000);
  Check('Bsf QWord above the low dword', Bsf(lQ), 32);
  Check('Bsr QWord above the low dword', Bsr(lQ), 32);

  lC:=$00F00000;
  Check('Bsf Cardinal', Bsf(lC), 20);
  Check('Bsr Cardinal', Bsr(lC), 23);
  lP:=PtrUInt(1) shl (SizeOf(PtrUInt)*8-1);
  Check('Bsf PtrUInt', Bsf(lP), SizeOf(PtrUInt)*8-1);
  Check('Bsr PtrUInt', Bsr(lP), SizeOf(PtrUInt)*8-1);
  lN:=6;
  Check('Bsf NativeUInt', Bsf(lN), 1);
  Check('Bsr NativeUInt', Bsr(lN), 2);

  Check('Bsf Byte constant', Bsf(Byte($A0)), 5);
  Check('Bsr Byte constant', Bsr(Byte($A0)), 7);
  Check('Bsf Word constant', Bsf(Word($0300)), 8);
  Check('Bsr Word constant', Bsr(Word($0300)), 9);
  Check('Bsf DWord constant', Bsf(DWord($80000000)), 31);
  Check('Bsr DWord constant', Bsr(DWord($00000001)), 0);
  Check('Bsf QWord constant', Bsf(QWord($8000000000000000)), 63);
  Check('Bsr QWord constant', Bsr(High(QWord)), 63);
  Check('Bsf zero constant', Bsf(Byte(0)), 255);
  Check('Bsr zero constant', Bsr(QWord(0)), 255);
  Check('Bsr untyped Byte-range constant', Bsr(200), 7);
  Check('Bsf untyped Word-range constant', Bsf($8000), 15);

  if gErrors=0 then
    Writeln('ok');
  Halt(gErrors);
end.
