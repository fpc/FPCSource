{ Val into double and single returns the correctly rounded value; samples from issue #29531 }
program tval7;

{$mode objfpc}{$H+}

{$R-}
{$Q-}

type
  TDoubleCase = record
    S: ShortString;
    Bits: QWord;
  end;

  TSingleCase = record
    S: ShortString;
    Bits: DWord;
  end;

const
  cReporterCases: array[0..40] of TDoubleCase = (
    (S: '1.584563250377521E29'; Bits: $4600000000040001),
    (S: '3.169126500755042E29'; Bits: $4610000000040001),
    (S: '2.854495725694287E45'; Bits: $4960000020000001),
    (S: '2.208559357534439E71'; Bits: $4EC0000040000001),
    (S: '4.417118715068878E71'; Bits: $4ED0000040000001),
    (S: '4.383627059105937E-193'; Bits: $1800000200000001),
    (S: '1.957195664071263E-294'; Bits: $02F4000000000001),
    (S: '3.914391328142526E-294'; Bits: $0304000000000001),
    (S: '1.250000000004549E-1'; Bits: $3FC0000000004005),
    (S: '2.500000000009098E-1'; Bits: $3FD0000000004005),
    (S: '5.68434222621747E-14'; Bits: $3D30000010100001),
    (S: '1.136868445243494E-13'; Bits: $3D40000010100001),
    (S: '2.273736890486988E-13'; Bits: $3D50000010100001),
    (S: '3.835229384285829E-93'; Bits: $2CC0000008040001),
    (S: '3.009265630590755E-36'; Bits: $3890000008400001),
    (S: '6.000600000008E265'; Bits: $771DC68491C306E1),
    (S: '8.007E248'; Bits: $739CA1254F8DF33B),
    (S: '6.0000000002E299'; Bits: $7E2CAB7BD66B0E4B),
    (S: '8.0000000002E285'; Bits: $7B4AE64521FA3CC7),
    (S: '5.000000000400007E160'; Bits: $614C73892ED5C299),
    (S: '9.005E275'; Bits: $793A025D96FF0A07),
    (S: '4.000000000006E257'; Bits: $756AA3D4DF50DAFB),
    (S: '4.00000004E127'; Bits: $5A6D8BA7FA0EC0DD),
    (S: '2.00000002E127'; Bits: $5A5D8BA7FA0EC0DD),
    (S: '6.000000000003E299'; Bits: $7E2CAB7BD667034B),
    (S: '1.001000001E285'; Bits: $7B1AED280502EFD7),
    (S: '8.000000000000002E298'; Bits: $7DFE94C85C298C4F),
    (S: '5.5000008E293'; Bits: $7CEB8EAC748C06E3),
    (S: '5.0004E210'; Bits: $6BAE6B7E77207067),
    (S: '8.0000000000001E277'; Bits: $79A20D4C2FA8A071),
    (S: '3.0000000001E299'; Bits: $7E1CAB7BD66B0E4B),
    (S: '4.00000000000005E277'; Bits: $79920D4C2FA8A071),
    (S: '1.142983031131382E-100'; Bits: $2B2FFFF7FFFFFF7B),
    (S: '5.648006361379507E219'; Bits: $6D8FFFF7FEFFFFFB),
    (S: '4.737888884325779E226'; Bits: $6EFFFFF6FFFFFFFB),
    (S: '8.043012710792554E-87'; Bits: $2E0FFFF3FFFFFFFB),
    (S: '4.313575011698527E68'; Bits: $4E2FFFF7FFFFFFFB),
    (S: '7.111162215780465E103'; Bits: $557FBFF7FFFFFFFB),
    (S: '5.688929772624372E104'; Bits: $55AFBFF7FFFFFFFB),
    (S: '4.596522290942083E-187'; Bits: $193FFFEFFFFFFFF9),
    (S: '6.469030023958787E-173'; Bits: $1C2FFFEFFFFFDFFB)
  );

  cDoubleCases: array[0..17] of TDoubleCase = (
    (S: '9007199254740993.0000000001'; Bits: $4340000000000001),
    (S: '9007199254740993'; Bits: $4340000000000000),
    (S: '-9007199254740995'; Bits: $C340000000000002),
    (S: '2.2250738585072011e-308'; Bits: $000FFFFFFFFFFFFF),
    (S: '2.2250738585072012e-308'; Bits: $0010000000000000),
    (S: '4.9406564584124654E-324'; Bits: $0000000000000001),
    (S: '2.4703282292062327E-324'; Bits: $0000000000000000),
    (S: '2.4703282292062328E-324'; Bits: $0000000000000001),
    (S: '1.7976931348623157E308'; Bits: $7FEFFFFFFFFFFFFF),
    (S: '1.7976931348623158E308'; Bits: $7FEFFFFFFFFFFFFF),
    (S: '1.7976931348623159E308'; Bits: $7FF0000000000000),
    (S: '1e400'; Bits: $7FF0000000000000),
    (S: '-1e400'; Bits: $FFF0000000000000),
    (S: '1e-400'; Bits: $0000000000000000),
    (S: '-0'; Bits: $8000000000000000),
    (S: '0.1'; Bits: $3FB999999999999A),
    (S: '.5'; Bits: $3FE0000000000000),
    (S: '7E-1'; Bits: $3FE6666666666666)
  );

  cSingleCases: array[0..13] of TSingleCase = (
    (S: '16777217.000000000000001'; Bits: $4B800001),
    (S: '16777217'; Bits: $4B800000),
    (S: '-16777219'; Bits: $CB800002),
    (S: '3.4028235677973366e38'; Bits: $7F7FFFFF),
    (S: '3.4028236e38'; Bits: $7F800000),
    (S: '1.4012984643e-45'; Bits: $00000001),
    (S: '7.006492321624085e-46'; Bits: $00000000),
    (S: '7.006492321624086e-46'; Bits: $00000001),
    (S: '1e39'; Bits: $7F800000),
    (S: '1e-46'; Bits: $00000000),
    (S: '0.1'; Bits: $3DCCCCCD),
    (S: '1.17549435e-38'; Bits: $00800000),
    (S: '8.589973e9'; Bits: $50000026),
    (S: '1.584563250377521E29'; Bits: $70000000)
  );

var
  gErrors: Integer = 0;

// Reports a failed check
procedure Fail(const aMsg: string);

begin
  Writeln('FAIL: ', aMsg);
  Inc(gErrors);
end;


// Returns aValue in decimal
function IntStr(aValue: Int64): string;

begin
  Str(aValue, Result);
end;


// Checks one conversion result against the expected bits
procedure CheckResult(const aWhat, aStr: string; aCode: ValSInt; aBits, aExpected: QWord; aDigits: Integer);

begin
  if aCode <> 0 then
    Fail('Val(''' + aStr + ''', ' + aWhat + ') must succeed, code ' + IntStr(aCode))
  else if aBits <> aExpected then
    Fail('Val(''' + aStr + ''', ' + aWhat + '): expected $' + HexStr(aExpected, aDigits) + ', got $' + HexStr(aBits, aDigits));
end;


// Checks Val into a double from shortstring, ansistring and unicodestring
procedure CheckDouble(const aStr: ShortString; aBits: QWord);

var
  lDouble: Double;
  lCode: ValSInt;
  lAnsi: AnsiString;
  lUnicode: UnicodeString;

begin
  Val(aStr, lDouble, lCode);
  CheckResult('double from shortstring', aStr, lCode, PQWord(@lDouble)^, aBits, 16);
  lAnsi := aStr;
  Val(lAnsi, lDouble, lCode);
  CheckResult('double from ansistring', aStr, lCode, PQWord(@lDouble)^, aBits, 16);
  lUnicode := UnicodeString(aStr);
  Val(lUnicode, lDouble, lCode);
  CheckResult('double from unicodestring', aStr, lCode, PQWord(@lDouble)^, aBits, 16);
end;


// Checks Val into a single from shortstring, ansistring and unicodestring
procedure CheckSingle(const aStr: ShortString; aBits: DWord);

var
  lSingle: Single;
  lCode: ValSInt;
  lAnsi: AnsiString;
  lUnicode: UnicodeString;

begin
  Val(aStr, lSingle, lCode);
  CheckResult('single from shortstring', aStr, lCode, PDWord(@lSingle)^, aBits, 8);
  lAnsi := aStr;
  Val(lAnsi, lSingle, lCode);
  CheckResult('single from ansistring', aStr, lCode, PDWord(@lSingle)^, aBits, 8);
  lUnicode := UnicodeString(aStr);
  Val(lUnicode, lSingle, lCode);
  CheckResult('single from unicodestring', aStr, lCode, PDWord(@lSingle)^, aBits, 8);
end;


// Checks the error position Val reports for malformed input
procedure CheckCode(const aStr: ShortString; aCode: ValSInt);

var
  lDouble: Double;
  lSingle: Single;
  lCode: ValSInt;

begin
  Val(aStr, lDouble, lCode);
  if lCode <> aCode then
    Fail('Val(''' + aStr + ''', double): expected code ' + IntStr(aCode) + ', got ' + IntStr(lCode));
  Val(aStr, lSingle, lCode);
  if lCode <> aCode then
    Fail('Val(''' + aStr + ''', single): expected code ' + IntStr(aCode) + ', got ' + IntStr(lCode));
end;


var
  i: Integer;
  lZeros: ShortString;

begin
  for i := Low(cReporterCases) to High(cReporterCases) do
    CheckDouble(cReporterCases[i].S, cReporterCases[i].Bits);
  for i := Low(cDoubleCases) to High(cDoubleCases) do
    CheckDouble(cDoubleCases[i].S, cDoubleCases[i].Bits);
  for i := Low(cSingleCases) to High(cSingleCases) do
    CheckSingle(cSingleCases[i].S, cSingleCases[i].Bits);
  lZeros := StringOfChar('0', 200);
  CheckDouble('9007199254740993.' + lZeros + '1', $4340000000000001);
  CheckDouble('9007199254740993.' + lZeros, $4340000000000000);
  CheckSingle('16777217.' + lZeros + '1', $4B800001);
  CheckDouble('Inf', $7FF0000000000000);
  CheckDouble('-Inf', $FFF0000000000000);
  CheckSingle('Inf', $7F800000);
  CheckCode('', 1);
  CheckCode('+', 2);
  CheckCode('1e', 3);
  CheckCode('1.5x', 4);
  CheckCode('1.5 ', 4);
  CheckCode('- 1', 2);
  CheckCode('$10', 1);
  if gErrors > 0 then
  begin
    Writeln(gErrors, ' errors');
    Halt(1);
  end;
  Writeln('ok');
end.
