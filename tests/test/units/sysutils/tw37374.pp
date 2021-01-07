program formatfloat_test;

uses
  SysUtils//, MyFormatFloat
  ;

var
  fails: Integer = 0;
  testCount: Integer = 0;

  procedure Test(AFormat: String; AValue: Double; AResult: String);
  var
    s: String;
  begin
    s := FormatFloat(AFormat, AValue);
    if s <> AResult then
    begin
      WriteLn('Format(' + AFormat + ', ', AValue:0:6, ') --> ', s, '; SHOULD BE: ', AResult);
      inc(fails);
    end;
    inc(testCount);
  end;

const
  VALUES: array[0..4] of Double = (0, 0.00001234, 0.123456, 1.23456, 1123.4567);
var
  fmt: String;

begin
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator := ',';

  fmt := '0.00';
  Test(fmt, VALUES[0], '0.00');
  Test(fmt, VALUES[1], '0.00');
  Test(fmt, VALUES[2], '0.12');
  Test(fmt, VALUES[3], '1.23');
  Test(fmt, VALUES[4], '1123.46');
  Test(fmt, -VALUES[1], '-0.00');
  Test(fmt, -VALUES[2], '-0.12');
  Test(fmt, -VALUES[3], '-1.23');
  Test(fmt, -VALUES[4], '-1123.46');

  fmt := '.00';
  Test(fmt, VALUES[0], '.00');
  Test(fmt, VALUES[1], '.00');
  Test(fmt, VALUES[2], '.12');
  Test(fmt, VALUES[3], '1.23');
  Test(fmt, VALUES[4], '1123.46');
  Test(fmt, -VALUES[1], '-.00');
  Test(fmt, -VALUES[2], '-.12');
  Test(fmt, -VALUES[3], '-1.23');
  Test(fmt, -VALUES[4], '-1123.46');

  fmt := '0.00000###';
  Test(fmt, VALUES[0], '0.00000');
  Test(fmt, VALUES[1], '0.00001234');
  Test(fmt, VALUES[2], '0.123456');
  Test(fmt, VALUES[3], '1.23456');
  Test(fmt, VALUES[4], '1123.45670');
  Test(fmt, -VALUES[1], '-0.00001234');
  Test(fmt, -VALUES[2], '-0.123456');
  Test(fmt, -VALUES[3], '-1.23456');
  Test(fmt, -VALUES[4], '-1123.45670');

  fmt := '000';
  Test(fmt, VALUES[0], '000');
  Test(fmt, VALUES[1], '000');
  Test(fmt, VALUES[2], '000');
  Test(fmt, VALUES[3], '001');
  Test(fmt, VALUES[4], '1123');
  Test(fmt, -VALUES[1], '-000');
  Test(fmt, -VALUES[2], '-000');
  Test(fmt, -VALUES[3], '-001');
  Test(fmt, -VALUES[4], '-1123');

  fmt := '0.00E+00';
  Test(fmt, VALUES[0], '0.00E+00');
  Test(fmt, VALUES[1], '1.23E-05');
  Test(fmt, VALUES[2], '1.23E-01');
  Test(fmt, VALUES[3], '1.23E+00');
  Test(fmt, VALUES[4], '1.12E+03');
  Test(fmt, -VALUES[1], '-1.23E-05');
  Test(fmt, -VALUES[2], '-1.23E-01');
  Test(fmt, -VALUES[3], '-1.23E+00');
  Test(fmt, -VALUES[4], '-1.12E+03');

  fmt := '0.00E-00';
  Test(fmt, VALUES[0], '0.00E00');
  Test(fmt, VALUES[1], '1.23E-05');
  Test(fmt, VALUES[2], '1.23E-01');
  Test(fmt, VALUES[3], '1.23E00');
  Test(fmt, VALUES[4], '1.12E03');
  Test(fmt, -VALUES[1], '-1.23E-05');
  Test(fmt, -VALUES[2], '-1.23E-01');
  Test(fmt, -VALUES[3], '-1.23E00');
  Test(fmt, -VALUES[4], '-1.12E03');

  fmt := '0.00 EUR';
  Test(fmt, VALUES[0], '0.00 EUR');
  Test(fmt, VALUES[1], '0.00 EUR');
  Test(fmt, VALUES[2], '0.12 EUR');
  Test(fmt, VALUES[3], '1.23 EUR');
  Test(fmt, VALUES[4], '1123.46 EUR');
  Test(fmt, -VALUES[1], '-0.00 EUR');
  Test(fmt, -VALUES[2], '-0.12 EUR');
  Test(fmt, -VALUES[3], '-1.23 EUR');
  Test(fmt, -VALUES[4], '-1123.46 EUR');

  fmt := '0.00 "EUR"';
  Test(fmt, VALUES[0], '0.00 EUR');
  Test(fmt, VALUES[1], '0.00 EUR');
  Test(fmt, VALUES[2], '0.12 EUR');
  Test(fmt, VALUES[3], '1.23 EUR');
  Test(fmt, VALUES[4], '1123.46 EUR');
  Test(fmt, -VALUES[1], '-0.00 EUR');
  Test(fmt, -VALUES[2], '-0.12 EUR');
  Test(fmt, -VALUES[3], '-1.23 EUR');
  Test(fmt, -VALUES[4], '-1123.46 EUR');

  fmt := '0.00"E+00"';
  Test(fmt, VALUES[0], '0.00E+00');
  Test(fmt, VALUES[1], '0.00E+00');
  Test(fmt, VALUES[2], '0.12E+00');
  Test(fmt, VALUES[3], '1.23E+00');
  Test(fmt, VALUES[4], '1123.46E+00');
  Test(fmt, -VALUES[1], '-0.00E+00');
  Test(fmt, -VALUES[2], '-0.12E+00');
  Test(fmt, -VALUES[3], '-1.23E+00');
  Test(fmt, -VALUES[4], '-1123.46E+00');

  fmt := '#,##0.0';
  Test(fmt, VALUES[0], '0.0');
  Test(fmt, VALUES[1], '0.0');
  Test(fmt, VALUES[2], '0.1');
  Test(fmt, VALUES[3], '1.2');
  Test(fmt, VALUES[4], '1,123.5');
  Test(fmt, -VALUES[1], '-0.0');
  Test(fmt, -VALUES[2], '-0.1');
  Test(fmt, -VALUES[3], '-1.2');
  Test(fmt, -VALUES[4], '-1,123.5');

  fmt := ',0.0';
  Test(fmt, VALUES[0], '0.0');
  Test(fmt, VALUES[1], '0.0');
  Test(fmt, VALUES[2], '0.1');
  Test(fmt, VALUES[3], '1.2');
  Test(fmt, VALUES[4], '1,123.5');
  Test(fmt, -VALUES[1], '-0.0');
  Test(fmt, -VALUES[2], '-0.1');
  Test(fmt, -VALUES[3], '-1.2');
  Test(fmt, -VALUES[4], '-1,123.5');

  fmt := '#,##0.00;(#,##0.00);zero';
  Test(fmt, VALUES[0], 'zero');
  Test(fmt, VALUES[1], '0.00');
  Test(fmt, VALUES[2], '0.12');
  Test(fmt, VALUES[3], '1.23');
  Test(fmt, VALUES[4], '1,123.46');
  Test(fmt, -VALUES[1], '(0.00)');
  Test(fmt, -VALUES[2], '(0.12)');
  Test(fmt, -VALUES[3], '(1.23)');
  Test(fmt, -VALUES[4], '(1,123.46)');

  fmt := '#,##0.00 EUR;(#,##0.00 EUR);zero';
  Test(fmt, VALUES[0], 'zero');
  Test(fmt, VALUES[1], '0.00 EUR');
  Test(fmt, VALUES[2], '0.12 EUR');
  Test(fmt, VALUES[3], '1.23 EUR');
  Test(fmt, VALUES[4], '1,123.46 EUR');
  Test(fmt, -VALUES[1], '(0.00 EUR)');
  Test(fmt, -VALUES[2], '(0.12 EUR)');
  Test(fmt, -VALUES[3], '(1.23 EUR)');
  Test(fmt, -VALUES[4], '(1,123.46 EUR)');

  fmt := 'EUR #,##0.00;(EUR #,##0.00);-';
  Test(fmt, VALUES[0], '-');
  Test(fmt, VALUES[1], 'EUR 0.00');
  Test(fmt, VALUES[2], 'EUR 0.12');
  Test(fmt, VALUES[3], 'EUR 1.23');
  Test(fmt, VALUES[4], 'EUR 1,123.46');
  Test(fmt, -VALUES[1], '(EUR 0.00)');
  Test(fmt, -VALUES[2], '(EUR 0.12)');
  Test(fmt, -VALUES[3], '(EUR 1.23)');
  Test(fmt, -VALUES[4], '(EUR 1,123.46)');

  WriteLn(testCount, ' tests executed.');
  if fails = 0 then
    WriteLn('All tests passed.')
  else
    halt(1);
end.

