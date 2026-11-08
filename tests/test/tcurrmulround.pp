{ %skipcpu=jvm }
{ The product of two Currency values is rounded to the nearest ten-thousandth, a tie to even, on every
  target: where Currency is an Int64 (Win64, for example) as where it is an x87 value, and as Delphi does }
program tcurrmulround;

{$mode objfpc}

const
  kA = Currency(1.2345);
  kTie = Currency(0.0003);
  kWhole = Currency(2);

var
  gExit: Integer = 0;
  gA, gB: Currency;
  gD: Double;

procedure Check(aValue: Currency; aExpected: Int64; const aWhat: string);
var
  lRaw: Int64;
begin
  Inc(gExit);
  lRaw := PInt64(@aValue)^;
  if lRaw <> aExpected then
    begin
      Writeln(aWhat, ': ', lRaw, ', expected ', aExpected);
      Halt(gExit);
    end;
end;

procedure Pair(const aA, aB: Currency; aExpected: Int64; const aWhat: string);
var
  lA, lB: Currency;
begin
  lA := aA;
  lB := aB;
  Check(lA * lB, aExpected, aWhat);
  Check(-lA * lB, -aExpected, '-(' + aWhat + ')');
  Check(lA * -lB, -aExpected, aWhat + ' negated right');
  Check(-lA * -lB, aExpected, aWhat + ' both negated');
end;

begin
  Pair(1.2345, 1.2345, 15240, '1.2345 * 1.2345 = 1.52399025');
  Pair(0.0049, 0.1, 5, '0.0049 * 0.1 = 0.00049');
  Pair(0.0041, 0.1, 4, '0.0041 * 0.1 = 0.00041');
  Pair(0.0001, 0.5, 0, '0.0001 * 0.5 = 0.00005, a tie, to even 0');
  Pair(0.0003, 0.5, 2, '0.0003 * 0.5 = 0.00015, a tie, to even 2');
  Pair(0.0005, 0.5, 2, '0.0005 * 0.5 = 0.00025, a tie, to even 2');
  Pair(0.0007, 0.5, 4, '0.0007 * 0.5 = 0.00035, a tie, to even 4');
  Pair(0.0001, 0.0001, 0, '0.0001 * 0.0001 = 0.00000001');
  Pair(2.5, 1.5, 37500, '2.5 * 1.5 = 3.75');
  Pair(123456.789, 1000, 1234567890000, '123456.789 * 1000');
  { the product needs more than 64 bits before the rescaling }
  Pair(922337203.6854, 1000.0001, 9223372959191204, '922337203.6854 * 1000.0001 = 922337295919.12036854');
  { products of constants }
  Check(kA * kWhole, 24690, 'the constant product 1.2345 * 2');
  Check(kTie * kWhole, 6, 'the constant product 0.0003 * 2');
  { a product stored in a Double (issue #39779): 0.4 where Currency is an Int64, 0.399966 from the x87;
    both are 0.4 as a Currency again }
  gA := 0.3738;
  gB := 1.07;
  gD := gA * gB;
  Check(gD, 4000, '0.3738 * 1.07 stored in a Double');
  Writeln('ok');
end.
