{ fpwidestring upper/lower case conversion for a single-byte code page (CP1252) }
program tcasecpfpwidestring;

{$mode objfpc}{$h+}

uses
  fpwidestring, cp1252, sysutils;

const
  cMixed = 'aBc'#$C4#$F6#$DC'1x'#$C9;
  cUpper = 'ABC'#$C4#$D6#$DC'1X'#$C9;
  cLower = 'abc'#$E4#$F6#$FC'1x'#$E9;

procedure Check(aCode: LongInt; const aActual, aExpected: RawByteString);

begin
  if Length(aActual)<>Length(aExpected) then
    begin
    writeln(aCode,': length ',Length(aActual),', expected ',Length(aExpected));
    halt(aCode);
    end;
  if CompareByte(PAnsiChar(aActual)^,PAnsiChar(aExpected)^,Length(aExpected))<>0 then
    halt(aCode+1);
end;


var
  lLong, lLongUpper: AnsiString;
  i: Integer;

begin
  DefaultSystemCodePage:=1252;
  Check(10,widestringmanager.UpperAnsiStringProc(cMixed),cUpper);
  Check(20,widestringmanager.LowerAnsiStringProc(cMixed),cLower);
  Check(30,AnsiUpperCase(cLower),cUpper);
  Check(40,AnsiLowerCase(cUpper),cLower);
  Check(50,AnsiUpperCase('a'),'A');
  Check(60,AnsiLowerCase(#$C4),#$E4);
  Check(70,AnsiUpperCase('a'#0'b'),'A'#0'B');
  lLong:='';
  lLongUpper:='';
  for i:=1 to 100 do
    begin
    lLong:=lLong+cLower;
    lLongUpper:=lLongUpper+cUpper;
    end;
  Check(80,AnsiUpperCase(lLong),lLongUpper);
  Check(90,AnsiLowerCase(lLongUpper),lLong);
  writeln('ok');
end.
