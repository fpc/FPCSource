{ Format %t pads and truncates by display width, issue #41585 }
program tformatt;

{$mode objfpc}{$h+}
{$codepage utf8}

uses
{$ifdef unix}
  cwstring,
{$endif}
  SysUtils;

var
  gOldFunc: TStringDisplayWidthFunc;


// Halts with aCode when aActual differs from aExpected
procedure Check(aCode: Integer; const aActual, aExpected: UnicodeString);

begin
  if aActual<>aExpected then
    begin
    writeln('Failed ',aCode,': got "',UTF8Encode(aActual),'", expected "',UTF8Encode(aExpected),'"');
    halt(aCode);
    end;
end;


// Halts with aCode when the bytes of aActual differ from those of aExpected
procedure CheckBytes(aCode: Integer; const aActual: RawByteString; const aExpected: UTF8String);

begin
  if (Length(aActual)<>Length(aExpected)) or
     ((Length(aActual)>0) and not CompareMem(@aActual[1],@aExpected[1],Length(aActual))) then
    begin
    writeln('Failed ',aCode,': got "',aActual,'", expected "',aExpected,'"');
    halt(aCode);
    end;
end;


// Counts two columns for every UTF-16 code unit
function DoubleWidth(const aText: UnicodeString; aMaxWidth: SizeInt; out aWidth: SizeInt): SizeInt;

begin
  Result:=Length(aText);
  if (aMaxWidth>=0) and (Result>aMaxWidth div 2) then
    Result:=aMaxWidth div 2;
  aWidth:=2*Result;
end;


var
  lFace, lMixed, lEAcute: UnicodeString;
  lUtf8: UTF8String;

begin
  DefaultSystemCodePage:=CP_UTF8;
  lEAcute:=UnicodeString(#$00E9);
  lFace:=UnicodeString(#$D83D#$DE00);
  lMixed:='a'+lFace+'b';

  Check(1,UnicodeFormat('[%5t]',[lEAcute]),'[    '+lEAcute+']');
  Check(2,UnicodeFormat('[%3t]',[lFace]),'[  '+lFace+']');
  Check(3,UnicodeFormat('[%-4t]',[lFace]),'['+lFace+'   ]');
  Check(4,UnicodeFormat('[%.2t]',[lMixed]),'[a'+lFace+']');
  Check(5,UnicodeFormat('[%.1t]',[lFace+'x']),'['+lFace+']');
  Check(6,UnicodeFormat('[%5.2t]',[lMixed]),'[   a'+lFace+']');
  Check(7,UnicodeFormat('[%*t]',[6,'ab']),'[    ab]');
  Check(8,UnicodeFormat('[%1:t%0:t]',['x','y']),'[yx]');
  Check(9,UnicodeFormat('[%3t]',['abcde']),'[abcde]');
  Check(10,UnicodeFormat('[%t]',['']),'[]');
  Check(11,UnicodeFormat('[%5s]',[lFace]),'[   '+lFace+']');
  Check(12,WideFormat('[%3t]',[WideString(lFace)]),'[  '+lFace+']');

  lUtf8:='éé';
  CheckBytes(20,Format('[%4t]',[lUtf8]),'[  éé]');
  CheckBytes(21,Format('[%.1t]',[lUtf8]),'[é]');
  CheckBytes(22,Format('[%-3t|%2t]',['é',lUtf8]),'[é  |éé]');
  CheckBytes(23,Format('[%4s]',[lUtf8]),'[éé]');

  gOldFunc:=StringDisplayWidthFunc;
  StringDisplayWidthFunc:=@DoubleWidth;
  Check(30,UnicodeFormat('[%5t]',['ab']),'[ ab]');
  Check(31,UnicodeFormat('[%.3t]',['abc']),'[a]');
  Check(32,UnicodeFormat('[%5s]',['ab']),'[   ab]');
  StringDisplayWidthFunc:=nil;
  Check(40,UnicodeFormat('[%3t]',[lFace]),'[  '+lFace+']');
  Check(41,UnicodeFormat('[%.1t]',[lFace+'x']),'['+lFace+']');
  CheckBytes(42,Format('[%.1t]',[lUtf8]),'[é]');
  StringDisplayWidthFunc:=gOldFunc;
  writeln('ok');
end.
