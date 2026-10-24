{ Math.DivMod for QWord and Int64, and overload selection next to the 32-bit versions }
program tdivmod64;

{$mode objfpc}

uses
  Math;

var
  gErrors: Integer = 0;


procedure CheckQWord(const aName: string; aDividend, aDivisor, aResult, aRemainder: QWord);

var
  lResult, lRemainder: QWord;

begin
  DivMod(aDividend,aDivisor,lResult,lRemainder);
  if (lResult=aResult) and (lRemainder=aRemainder) then
    Writeln(aName,': ',lResult,' ',lRemainder,' OK')
  else
    begin
    Writeln(aName,': ',lResult,' ',lRemainder,' expected ',aResult,' ',aRemainder,' ERROR');
    Inc(gErrors);
    end;
end;


procedure CheckInt64(const aName: string; aDividend, aDivisor, aResult, aRemainder: Int64);

var
  lResult, lRemainder: Int64;

begin
  DivMod(aDividend,aDivisor,lResult,lRemainder);
  if (lResult=aResult) and (lRemainder=aRemainder) then
    Writeln(aName,': ',lResult,' ',lRemainder,' OK')
  else
    begin
    Writeln(aName,': ',lResult,' ',lRemainder,' expected ',aResult,' ',aRemainder,' ERROR');
    Inc(gErrors);
    end;
end;


procedure CheckTrue(const aName: string; aValue: Boolean);

begin
  if aValue then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR');
    Inc(gErrors);
    end;
end;


var
  lQ, lQR: QWord;
  lI, lIR: Int64;
  lL, lLR: LongInt;
  lD, lDR: DWord;
  lW, lWR: Word;

begin
  CheckQWord('QWord small', 100, 7, 14, 2);
  CheckQWord('QWord dividend below divisor', 5, 7, 0, 5);
  CheckQWord('QWord High div 10', High(QWord), 10, QWord(1844674407370955161), 5);
  CheckQWord('QWord High div High', High(QWord), High(QWord), 1, 0);
  CheckQWord('QWord High div 1', High(QWord), 1, High(QWord), 0);
  CheckQWord('QWord divisor above 2^63', High(QWord), QWord($8000000000000001), 1, QWord($7FFFFFFFFFFFFFFE));
  CheckQWord('QWord 2^32 boundary', QWord($100000000), QWord($FFFFFFFF), 1, 1);

  CheckInt64('Int64 positive', 9, 5, 1, 4);
  CheckInt64('Int64 negative dividend', -9, 5, -1, -4);
  CheckInt64('Int64 negative divisor', 9, -5, -1, 4);
  CheckInt64('Int64 both negative', -9, -5, 1, -4);
  CheckInt64('Int64 exact negative', -10, 5, -2, 0);
  CheckInt64('Int64 High div 10', High(Int64), 10, 922337203685477580, 7);
  CheckInt64('Int64 Low div 1', Low(Int64), 1, Low(Int64), 0);
  CheckInt64('Int64 Low div 2', Low(Int64), 2, -4611686018427387904, 0);
  CheckInt64('Int64 Low div 3', Low(Int64), 3, -3074457345618258602, -2);
  CheckInt64('Int64 Low div -2', Low(Int64), -2, 4611686018427387904, 0);
  CheckInt64('Int64 Low div High', Low(Int64), High(Int64), -1, -1);
  CheckInt64('Int64 High div Low', High(Int64), Low(Int64), 0, High(Int64));
  CheckInt64('Int64 Low div Low', Low(Int64), Low(Int64), 1, 0);

  DivMod(-9,5,lI,lIR);
  CheckTrue('LongInt constants with Int64 variables', (lI=-1) and (lIR=-4));
  DivMod(High(QWord),16,lQ,lQR);
  CheckTrue('Constants with QWord variables', (lQ=QWord($0FFFFFFFFFFFFFFF)) and (lQR=15));
  DivMod(-9,5,lL,lLR);
  CheckTrue('LongInt overload still selected', (lL=-1) and (lLR=-4));
  DivMod(123456,23,lD,lDR);
  CheckTrue('DWord overload still selected', (lD=5367) and (lDR=15));
  DivMod($ffff,65,lW,lWR);
  CheckTrue('Word overload still selected', (lW=1008) and (lWR=15));

  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
