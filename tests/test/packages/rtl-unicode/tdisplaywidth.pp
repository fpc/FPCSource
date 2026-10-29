{ Format %t with grapheme clusters and East Asian width from unicodedisplaywidth, issue #41585 }
program tdisplaywidth;

{$mode objfpc}{$h+}

uses
{$ifdef unix}
  cwstring,
{$endif}
  SysUtils, unicodedisplaywidth;


// Halts with aCode when aActual differs from aExpected
procedure Check(aCode: Integer; const aActual, aExpected: UnicodeString);

begin
  if aActual<>aExpected then
    begin
    writeln('Failed ',aCode,': got "',UTF8Encode(aActual),'", expected "',UTF8Encode(aExpected),'"');
    halt(aCode);
    end;
end;


// Halts with aCode when aActual differs from aExpected
procedure CheckInt(aCode: Integer; aActual, aExpected: SizeInt);

begin
  if aActual<>aExpected then
    begin
    writeln('Failed ',aCode,': got ',aActual,', expected ',aExpected);
    halt(aCode);
    end;
end;


var
  lCombined, lCJK, lFace, lDegree: UnicodeString;

begin
  lCombined:=UnicodeString('e'#$0301);
  lCJK:=UnicodeString(#$4E2D#$6587);
  lFace:=UnicodeString(#$D83D#$DE00);
  lDegree:=UnicodeString(#$00B0);

  CheckInt(1,StringDisplayWidth(lCombined),1);
  CheckInt(2,StringDisplayWidth(lCJK),4);
  CheckInt(3,StringDisplayWidth(lFace),2);
  CheckInt(4,StringDisplayWidth('abc'),3);
  CheckInt(5,StringDisplayWidth(''),0);
  CheckInt(6,ExtendedGraphemeClusterDisplayWidth(lCombined),1);

  Check(10,UnicodeFormat('[%3t]',[lCombined]),'[  '+lCombined+']');
  Check(11,UnicodeFormat('[%.1t]',[lCombined+'x']),'['+lCombined+']');
  Check(12,UnicodeFormat('[%6t]',[lCJK]),'[  '+lCJK+']');
  Check(13,UnicodeFormat('[%-6t]',[lCJK]),'['+lCJK+'  ]');
  Check(14,UnicodeFormat('[%.3t]',[lCJK]),'['+Copy(lCJK,1,1)+']');
  Check(15,UnicodeFormat('[%.1t]',[lCJK]),'[]');
  Check(16,UnicodeFormat('[%4t]',[lFace]),'[  '+lFace+']');

  Check(20,UnicodeFormat('[%3t]',[lDegree]),'[  '+lDegree+']');
  AmbiguousDisplayWidth:=2;
  Check(21,UnicodeFormat('[%3t]',[lDegree]),'[ '+lDegree+']');
  AmbiguousDisplayWidth:=1;
  writeln('ok');
end.
