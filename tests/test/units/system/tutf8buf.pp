{ UnicodeToUtf8 and Utf8ToUnicode with exact-fit and too small buffers, issue #41354 }
program tutf8buf;

{$mode objfpc}{$h+}

{ Halts with aCode+1 when the result differs, aCode+2 when the converted bytes
  differ from aExpected, aCode+3 when the terminating #0 is missing and aCode+4
  when bytes at or beyond aMax were written }
procedure CheckToUtf8(aCode: LongInt; const aSource: UnicodeString; aMax: SizeUInt; const aExpected: RawByteString);

var
  lBuf: array[0..15] of Byte;
  lRes: SizeUInt;
  i: LongInt;

begin
  FillChar(lBuf,SizeOf(lBuf),$FF);
  lRes:=UnicodeToUtf8(PAnsiChar(@lBuf[0]),aMax,PUnicodeChar(aSource),Length(aSource));
  if aMax=0 then
    begin
    if lRes<>0 then
      halt(aCode+1);
    end
  else
    begin
    if lRes<>SizeUInt(Length(aExpected)+1) then
      halt(aCode+1);
    for i:=1 to Length(aExpected) do
      if lBuf[i-1]<>Ord(aExpected[i]) then
        halt(aCode+2);
    if lBuf[Length(aExpected)]<>0 then
      halt(aCode+3);
    end;
  for i:=aMax to High(lBuf) do
    if lBuf[i]<>$FF then
      halt(aCode+4);
end;


{ Same checks as CheckToUtf8 for Utf8ToUnicode }
procedure CheckToUnicode(aCode: LongInt; const aSource: RawByteString; aMax: SizeUInt; const aExpected: UnicodeString);

var
  lBuf: array[0..15] of Word;
  lRes: SizeUInt;
  i: LongInt;

begin
  FillChar(lBuf,SizeOf(lBuf),$FF);
  lRes:=Utf8ToUnicode(PUnicodeChar(@lBuf[0]),aMax,PAnsiChar(aSource),Length(aSource));
  if aMax=0 then
    begin
    if lRes<>0 then
      halt(aCode+1);
    end
  else
    begin
    if lRes<>SizeUInt(Length(aExpected)+1) then
      halt(aCode+1);
    for i:=1 to Length(aExpected) do
      if lBuf[i-1]<>Ord(aExpected[i]) then
        halt(aCode+2);
    if lBuf[Length(aExpected)]<>0 then
      halt(aCode+3);
    end;
  for i:=aMax to High(lBuf) do
    if lBuf[i]<>$FFFF then
      halt(aCode+4);
end;


var
  lTwo, lThree, lFour: UnicodeString;

begin
  lTwo:=#$0100#$0100;
  lThree:=#$3400#$3400;
  lFour:=#$D83D#$DE00;

  CheckToUtf8(10,'ab',0,'');
  CheckToUtf8(20,'ab',1,'');
  CheckToUtf8(30,'ab',2,'a');
  CheckToUtf8(40,'ab',3,'ab');
  CheckToUtf8(50,lTwo,5,#$C4#$80#$C4#$80);
  CheckToUtf8(60,lTwo,4,#$C4#$80);
  CheckToUtf8(70,lTwo,3,#$C4#$80);
  CheckToUtf8(80,lTwo,2,'');
  CheckToUtf8(90,lThree,7,#$E3#$90#$80#$E3#$90#$80);
  CheckToUtf8(100,lThree,6,#$E3#$90#$80);
  CheckToUtf8(110,lThree,4,#$E3#$90#$80);
  CheckToUtf8(120,lThree,3,'');
  CheckToUtf8(130,lFour,5,#$F0#$9F#$98#$80);
  CheckToUtf8(140,lFour,4,'');
  CheckToUtf8(150,'a'+lFour,6,'a'#$F0#$9F#$98#$80);
  CheckToUtf8(160,'a'+lFour,5,'a');

  CheckToUnicode(210,'ab',0,'');
  CheckToUnicode(220,'ab',1,'');
  CheckToUnicode(230,'ab',2,'a');
  CheckToUnicode(240,'ab',3,'ab');
  CheckToUnicode(250,'a'#$C4#$80,3,'a'#$0100);
  CheckToUnicode(260,'a'#$C4#$80,2,'a');
  CheckToUnicode(270,#$E3#$90#$80,2,#$3400);
  CheckToUnicode(280,#$E3#$90#$80,1,'');
  CheckToUnicode(290,#$F0#$9F#$98#$80,3,lFour);
  CheckToUnicode(300,#$F0#$9F#$98#$80,2,'');
  CheckToUnicode(310,'a'#$F0#$9F#$98#$80,4,'a'+lFour);
  CheckToUnicode(320,'a'#$F0#$9F#$98#$80,3,'a');

  if UnicodeToUtf8(nil,0,PUnicodeChar(lThree),Length(lThree))<>7 then
    halt(401);
  if Utf8ToUnicode(nil,0,PAnsiChar(RawByteString(#$F0#$9F#$98#$80)),4)<>3 then
    halt(402);
  writeln('ok');
end.
