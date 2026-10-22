{ Math.DivMod with LongInt dividends: negative values and Low(LongInt), compared with div and mod }
program tdivmod32;

{$mode objfpc}

uses
  Math;

const
  Dividends: array[0..9] of LongInt = (0, 1, -1, 9, -9, 65535, -65536, 123456789, High(LongInt), Low(LongInt));
  Divisors: array[0..7] of LongInt = (1, 2, -2, 7, -7, 65535, High(LongInt), Low(LongInt));

var
  gErrors: Integer = 0;


procedure CheckLongInt(const aName: string; aDividend, aDivisor, aResult, aRemainder: LongInt);

var
  lResult, lRemainder: LongInt;

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


procedure CheckSmallInt(const aName: string; aDividend: LongInt; aDivisor: Word; aResult, aRemainder: SmallInt);

var
  lResult, lRemainder: SmallInt;

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


procedure CheckWord(const aName: string; aDividend: LongInt; aDivisor: Word; aResult, aRemainder: Word);

var
  lResult, lRemainder: Word;

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


// Compares the LongInt DivMod with div and mod for all combinations of Dividends and Divisors.
procedure CheckTable;

var
  i, j: Integer;
  lResult, lRemainder: LongInt;
  lFailed: Integer;

begin
  lFailed:=0;
  for i:=Low(Dividends) to High(Dividends) do
    for j:=Low(Divisors) to High(Divisors) do
      begin
      DivMod(Dividends[i],Divisors[j],lResult,lRemainder);
      if (lResult<>Dividends[i] div Divisors[j]) or (lRemainder<>Dividends[i] mod Divisors[j]) then
        begin
        Writeln('Table ',Dividends[i],' ',Divisors[j],': ',lResult,' ',lRemainder,' expected ',
          Dividends[i] div Divisors[j],' ',Dividends[i] mod Divisors[j],' ERROR');
        Inc(lFailed);
        end;
      end;
  if lFailed=0 then
    Writeln('Table matches div and mod OK')
  else
    Inc(gErrors,lFailed);
end;


begin
  CheckLongInt('LongInt negative dividend', -9, 5, -1, -4);
  CheckLongInt('LongInt negative divisor', 9, -5, -1, 4);
  CheckLongInt('LongInt both negative', -9, -5, 1, -4);
  CheckLongInt('LongInt High div 10', High(LongInt), 10, 214748364, 7);
  CheckLongInt('LongInt Low div 5', Low(LongInt), 5, -429496729, -3);
  CheckLongInt('LongInt Low div 1', Low(LongInt), 1, Low(LongInt), 0);
  CheckLongInt('LongInt Low div 2', Low(LongInt), 2, -1073741824, 0);
  CheckLongInt('LongInt Low div -2', Low(LongInt), -2, 1073741824, 0);
  CheckLongInt('LongInt Low div High', Low(LongInt), High(LongInt), -1, -1);
  CheckLongInt('LongInt High div Low', High(LongInt), Low(LongInt), 0, High(LongInt));
  CheckLongInt('LongInt Low div Low', Low(LongInt), Low(LongInt), 1, 0);
  CheckTable;

  CheckSmallInt('SmallInt negative dividend', -9, 5, -1, -4);
  CheckSmallInt('SmallInt -30000 div 7', -30000, 7, -4285, -5);
  CheckSmallInt('SmallInt High', 32767, 1, 32767, 0);
  CheckSmallInt('SmallInt Low(LongInt) div 65535', Low(LongInt), 65535, -32768, -32768);
  CheckSmallInt('SmallInt Low(LongInt) div 5, low 16 bits of the quotient', Low(LongInt), 5, 26215, -3);

  CheckWord('Word negative dividend', -9, 5, 65535, 65532);
  CheckWord('Word -1 div 65535', -1, 65535, 0, 65535);
  CheckWord('Word Low(LongInt) div 65535', Low(LongInt), 65535, 32768, 32768);
  CheckWord('Word Low(LongInt) div 5, low 16 bits', Low(LongInt), 5, 26215, 65533);

  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
