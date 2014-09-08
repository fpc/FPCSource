{$mode objfpc}
uses
  math;
{ tests:
  procedure DivMod(Dividend: LongInt; Divisor: Word;  var Result, Remainder: Word);
  procedure DivMod(Dividend: LongInt; Divisor: Word; var Result, Remainder: SmallInt);
  procedure DivMod(Dividend: DWord; Divisor: DWord; var Result, Remainder: DWord);
  procedure DivMod(Dividend: LongInt; Divisor: LongInt; var Result, Remainder: LongInt);
}
procedure doerror(i : LongInt);
  begin
    writeln('Error: ',i);
	halt(1);
  end;


var
  QuotientWord,RemainderWord : Word;
  QuotientSmallInt,RemainderSmallInt : SmallInt;
  QuotientDWord,RemainderDWord : DWord;
  QuotientLongInt,RemainderLongInt : LongInt;

begin
  DivMod($ffff,65,QuotientWord,RemainderWord);
  if QuotientWord<>1008 then
    doerror(1);
  if RemainderWord<>15 then
    doerror(2);
	
  DivMod($ffff,65,QuotientSmallInt,RemainderSmallInt);
  if QuotientSmallInt<>1008 then
    doerror(1001);
  if RemainderSmallInt<>15 then
    doerror(2);
	
  DivMod($ffff,65,QuotientDWord,RemainderDWord);
  if QuotientDWord<>1008 then
    doerror(2001);
  if RemainderDWord<>15 then
    doerror(2002);

  DivMod(123456,23,QuotientDWord,RemainderDWord);
  if QuotientDWord<>5367 then
    doerror(2003);
  if RemainderDWord<>15 then
    doerror(2004);

  DivMod($ffff,65,QuotientLongInt,RemainderLongInt);
  if QuotientLongInt<>1008 then
    doerror(3001);
  if RemainderLongInt<>15 then
    doerror(3002);

  DivMod(123456,23,QuotientLongInt,RemainderLongInt);
  if QuotientLongInt<>5367 then
    doerror(3003);
  if RemainderLongInt<>15 then
    doerror(3004);
	
  DivMod(-9, 5, QuotientLongInt,RemainderLongInt);
  if QuotientLongInt<>-1 then
    doerror(3005);
  if RemainderLongInt<>-4 then
    doerror(3006);
	
  DivMod(-9, -5, QuotientLongInt,RemainderLongInt);
  if QuotientLongInt<>1 then
    doerror(3007);
  if RemainderLongInt<>-4 then
    doerror(3008);
	
end.
	
