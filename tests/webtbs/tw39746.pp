{ fpwidestring UTF-8 upper/lower case conversion adds no trailing #0 }
program tw39746;

{$mode objfpc}{$h+}

uses
  fpwidestring, sysutils;

const
  cUpperUmlauts = #$C3#$84#$C3#$96#$C3#$9C;
  cLowerUmlauts = #$C3#$A4#$C3#$B6#$C3#$BC;
  cUpperIDot = 'ABC'#$C4#$B0'XX';

procedure Check(aCode: LongInt; const aActual, aExpected: RawByteString);

begin
  if Length(aActual)<>Length(aExpected) then
    begin
    writeln(aCode,': length ',Length(aActual),', expected ',Length(aExpected));
    halt(aCode);
    end;
  if CompareByte(PAnsiChar(aActual)^,PAnsiChar(aExpected)^,Length(aExpected))<>0 then
    begin
    writeln(aCode,': "',aActual,'", expected "',aExpected,'"');
    halt(aCode+1);
    end;
end;


var
  lResult: AnsiString;

begin
  DefaultSystemCodePage:=CP_UTF8;
  Check(10,AnsiLowerCase('A'),'a');
  Check(20,widestringmanager.LowerAnsiStringProc('A'),'a');
  Check(30,widestringmanager.UpperAnsiStringProc('a'),'A');
  Check(40,widestringmanager.LowerAnsiStringProc('ABCIXX'),'abcixx');
  Check(50,AnsiLowerCase(cUpperUmlauts),cLowerUmlauts);
  Check(60,AnsiUpperCase(AnsiLowerCase(cUpperUmlauts)),cUpperUmlauts);
  Check(70,AnsiLowerCase(AnsiUpperCase(AnsiLowerCase(cUpperUmlauts))),cLowerUmlauts);
  Check(80,AnsiUpperCase(cLowerUmlauts),cUpperUmlauts);
  lResult:=widestringmanager.LowerAnsiStringProc(cUpperIDot);
  if Pos(#0,lResult)<>0 then
    halt(90);
  if (Copy(lResult,1,3)<>'abc') or (Copy(lResult,Length(lResult)-1,2)<>'xx') then
    halt(91);
  writeln('ok');
end.
