{ Test for FloatToStr and CurrToStr functions. }

uses sysutils;

const
  MaxCurrency : currency = 922337203685477.5807;
  MinCurrency : currency = -922337203685477.5807;

var
  ErrCount: longint;

procedure CheckResult(const s, ref: string);
begin
  if s <> ref then
    begin
      writeln('Got      : ', s);
      writeln('Should be: ', ref);
      Inc(ErrCount);
    end;
end;

var
  e: extended;
  d: double;
  s: single;
  c: currency;
begin
  e:=1234567890123.4;
  d:=12345.12345;
  s:=12345.12;
  c:=12345.1234;
  CheckResult(FloatToStrF(e,ffExponent,15,1), '1'+DecimalSeparator+'23456789012340E+12');
  CheckResult(FloatToStrF(d,ffExponent,11,0), '1'+DecimalSeparator+'2345123450E+4');
  CheckResult(FloatToStrF(s,ffExponent,8,0), '1'+DecimalSeparator+'2345120E+4');
  CheckResult(FloatToStrF(s,ffExponent,8,7), '1'+DecimalSeparator+'2345120E+0004');
  CheckResult(FloatToStrF(e,ffExponent,8,3), '1'+DecimalSeparator+'2345679E+012');
  CheckResult(FloatToStrF(c,ffExponent,10,0), '1'+DecimalSeparator+'234512340E+4');
  CheckResult(FloatToStrF(c,ffExponent,11,2), '1'+DecimalSeparator+'2345123400E+04');
  CheckResult(FloatToStrF(c,ffExponent,10,4), '1'+DecimalSeparator+'234512340E+0004');
  CheckResult(FloatToStrF(-12345.12345,ffExponent,11,0), '-1'+DecimalSeparator+'2345123450E+4');
  CheckResult(FloatToStrF(-0.00000123,ffGeneral,15,0), '-1'+DecimalSeparator+'23E-6');
  CheckResult(FloatToStrF(-12345.12345,ffGeneral,7,0), '-12345'+DecimalSeparator+'12');
  CheckResult(CurrToStr(-12345.1234), '-12345'+DecimalSeparator+'1234');
  CheckResult(CurrToStr(MaxCurrency), '922337203685477'+DecimalSeparator+'5807');
  CheckResult(CurrToStr(MinCurrency), '-922337203685477'+DecimalSeparator+'5807');
  NegCurrFormat:=8;
  CheckResult(FloatToStrF(-12345.1234,ffCurrency,19,4), '-12' + ThousandSeparator + '345'+DecimalSeparator+'1234 ' + CurrencyString);
  CheckResult(FloatToStrF(MinCurrency,ffCurrency,19,4), '-922' + ThousandSeparator + '337' + ThousandSeparator + '203' + ThousandSeparator + '685' + ThousandSeparator + '477'+DecimalSeparator+'5807 ' + CurrencyString);
  if ErrCount > 0 then
    begin
      writeln('Test failed. Errors: ', ErrCount);
      Halt(1);
    end
  else
    writeln('Test completed.');
end.
