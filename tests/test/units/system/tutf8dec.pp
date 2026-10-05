{ Tests Utf8ToUnicode on 4-byte lead bytes and invalid sequences, issue #33692 }
program tutf8dec;

{$mode objfpc}{$h+}

{ Halts with aCode+1 when the count-only call returns a different length,
  aCode+2 when the decoded length differs and aCode+3 when the decoded
  characters differ from aExpected }
procedure Check(aCode: LongInt; const aSource: RawByteString; const aExpected: UnicodeString);

var
  lBuf: array[0..31] of UnicodeChar;
  lLen, lCount: SizeUInt;
  lResult: UnicodeString;

begin
  lCount:=Utf8ToUnicode(nil,0,PAnsiChar(aSource),Length(aSource));
  lLen:=Utf8ToUnicode(@lBuf[0],Length(lBuf),PAnsiChar(aSource),Length(aSource));
  if lCount<>lLen then
    halt(aCode+1);
  if lLen<>SizeUInt(Length(aExpected)+1) then
    halt(aCode+2);
  SetString(lResult,PUnicodeChar(@lBuf[0]),lLen-1);
  if lResult<>aExpected then
    halt(aCode+3);
end;


begin
  Check(10,#$61#$F0#$80#$80#$E1#$80#$C2#$62,'a???b');
  Check(20,#$61#$F1#$80#$80#$E1#$80#$C2#$62,'a???b');
  Check(30,#$61#$F4#$80#$80#$E1#$80#$C2#$62,'a???b');
  Check(40,#$61#$F0#$9F#$98#$80#$62,'a'#$D83D#$DE00'b');
  Check(50,#$61#$F4#$8F#$BF#$BF#$62,'a'#$DBFF#$DFFF'b');
  Check(60,#$61#$F4#$90#$80#$80#$62,'a?b');
  Check(70,#$61#$F5#$80#$80#$80#$62,'a?b');
  Check(80,#$61#$F0#$80#$80#$62,'a?b');
  Check(90,#$61#$E0#$80#$80#$62,'a?b');
  writeln('ok');
end.
