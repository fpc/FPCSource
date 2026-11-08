{ %skipcpu=jvm }
{ As tcurrmulround, with overflow checking on, which selects fpc_mul_currency_checkoverflow where Currency
  is an Int64: the product is rounded to the nearest ten-thousandth, a tie to even, and a product that the
  rounding carries past High(Int64) is an overflow, while the same product negated is Low(Int64). }
program tcurrmulround2;

{$mode objfpc}
{$Q+}

uses
  SysUtils;

var
  gExit: Integer = 0;

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

{$ifdef FPC_CURRENCY_IS_INT64}
function FromRaw(aRaw: Int64): Currency;
begin
  PInt64(@Result)^ := aRaw;
end;

var
  lA, lB, lC: Currency;
  lOverflow: Boolean;
{$endif}

begin
  Pair(1.2345, 1.2345, 15240, '1.2345 * 1.2345 = 1.52399025');
  Pair(0.0049, 0.1, 5, '0.0049 * 0.1 = 0.00049');
  Pair(0.0041, 0.1, 4, '0.0041 * 0.1 = 0.00041');
  Pair(0.0001, 0.5, 0, '0.0001 * 0.5 = 0.00005, a tie, to even 0');
  Pair(0.0003, 0.5, 2, '0.0003 * 0.5 = 0.00015, a tie, to even 2');
  Pair(2.5, 1.5, 37500, '2.5 * 1.5 = 3.75');
  { the product needs more than 64 bits before the rescaling }
  Pair(922337203.6854, 1000.0001, 9223372959191204, '922337203.6854 * 1000.0001 = 922337295919.12036854');
{$ifdef FPC_CURRENCY_IS_INT64}
  { 614891469123651.7205 * 1.5 = 922337203685477.58075, raw 9223372036854775807.5: a tie, which rounds
    to the even 2^63, one past High(Int64) }
  lA := FromRaw(6148914691236517205);
  lB := 1.5;
  lOverflow := False;
  try
    lC := lA * lB;
  except
    on EIntOverflow do
      lOverflow := True;
  end;
  Inc(gExit);
  if not lOverflow then
    begin
      Writeln('rounding past High(Int64): no overflow, raw ', PInt64(@lC)^);
      Halt(gExit);
    end;
  { negated, the same tie rounds to -2^63, which is Low(Int64) }
  Check(-lA * lB, Low(Int64), 'rounding to Low(Int64)');
{$endif}
  Writeln('ok');
end.
