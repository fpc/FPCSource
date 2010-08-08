program t5;
{$ifdef fpc}{$mode objfpc}{$h+}{$endif}

uses sysutils;

var
  frec: TFloatRec;
  code: Integer;
  
const
  posinf: Extended = 1.0/0.0;
  neginf: Extended = -1.0/0.0;
  nan: Extended = 0.0/0.0;

begin
  code := 0;
  FloatToDecimal(frec, posinf, fvExtended, 15, 15);
  if (frec.Exponent <> 32767) or frec.Negative or (frec.Digits[0] <> #0) then
  begin
    writeln('Positive infinity test failed');
    code := code or 1;
  end;
  
  FloatToDecimal(frec, neginf, fvExtended, 15, 15);
  if (frec.Exponent <> 32767) or (not frec.Negative) or (frec.Digits[0] <> #0) then
  begin
    writeln('Negative infinity test failed');
    code := code or 2;
  end;
  
  FloatToDecimal(frec, nan, fvExtended, 15, 15);
  if (frec.Exponent <> -32768) or (frec.Digits[0] <> #0) then
  begin
    writeln('NaN test failed');
    code := code or 4;
  end;
  Halt(Code);
end.