{ UCS4StringToUnicodeString/UCS4StringToWideString replace surrogate code points by '?', issue #33613 }
program tucs4surrogate;

{$mode objfpc}{$h+}

// Build a UCS4String from aCodes with the terminating #0
function MakeUCS4(const aCodes: array of UCS4Char): UCS4String;

var
  lIndex: Integer;

begin
  SetLength(Result,Length(aCodes)+1);
  for lIndex:=0 to High(aCodes) do
    Result[lIndex]:=aCodes[lIndex];
  Result[Length(aCodes)]:=0;
end;


// Halt with aCode when aValue differs from aExpected
procedure Check(const aValue, aExpected: UnicodeString; aCode: Integer);

begin
  if aValue<>aExpected then
    begin
    writeln('check ',aCode,' failed, length ',Length(aValue));
    halt(aCode);
    end;
end;


var
  lW: WideString;

begin
  Check(UCS4StringToUnicodeString(MakeUCS4([$41,$D800,$42])),'A?B',1);
  Check(UCS4StringToUnicodeString(MakeUCS4([$DFFF])),'?',2);
  Check(UCS4StringToUnicodeString(MakeUCS4([$DBFF,$DC00])),'??',3);
  Check(UCS4StringToUnicodeString(MakeUCS4([$D7FF,$E000])),#$D7FF#$E000,4);
  Check(UCS4StringToUnicodeString(MakeUCS4([$1F600])),#$D83D#$DE00,5);
  Check(UCS4StringToUnicodeString(MakeUCS4([$110000])),'?',6);
  lW:=UCS4StringToWideString(MakeUCS4([$43,$DC00]));
  Check(lW,'C?',7);
  writeln('ok');
end.
