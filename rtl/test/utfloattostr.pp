unit utfloattostr;

{$mode objfpc}
{$h+}
interface

{ Test for FloatToStr and CurrToStr functions. }

uses sysutils;

implementation

uses punit, utrtl;

const
  MaxCurrency : currency = 922337203685477.5807;
  MinCurrency : currency = -922337203685477.5807;

var
  ErrCount: longint;

Function CheckVal(nr,step,cycle : Integer; f: Extended) : Boolean;
var
  s,v1,v2,tn: string;
  f1: Extended;

begin
  TN:='Cycle nr '+intToStr(Nr)+' step :'+INtToStr(Step)+' cycle : '+IntToStr(Cycle)+' : ';
  Result:=True;
  s := FloatToStr(f);
  f1 := StrToFloat(s);
  if (f<>f1) and (Abs(f-f1)/Abs(f) > 6e-15) then
    begin
    Str(Abs(f-f1)/Abs(f),v1);
    Str(f,V2);
    Fail(TN+'Error (Double):'+V1+ ' Input:'+V2+' Output:'+s);
    Exit(False);
    end;
  f := Single(f);
  s := FloatToStr(Single(f));
  f1 := StrToFloat(s);
  if (f<>f1) and (Abs(f-f1)/Abs(f) > 6e-10) then
    begin
    Str(Abs(f-f1)/Abs(f),v1);
    Str(f,V2);
    Fail(TN+'Error (Single):'+V1+ ' Input:'+v2+' Output:'+s);
    Exit(False);
    end;
end;

Function Cycle(nr,step : Integer; f: Extended) : Boolean;

var
  i: Integer;
begin
  Result:=True;
  for i := 1 to 50 do
    begin
    if not CheckVal(nr,step,i,f) then exit(False);
    if not CheckVal(nr,step,i,-f) then exit(False);
    f := f/10;
    end;
end;

Function CycleInc(Nr : Integer; f, increment: Extended) : Boolean;

var
  i: Integer;
begin
  Result:=True;
  if not Cycle(NR,-1,f) then Exit(False);
  for i := 0 to 30 do
    begin
    if not Cycle(Nr,I,f+increment) then exit(False);
    if not Cycle(Nr,I,f-increment) then exit(False);
    increment := increment/10;
    end;
end;

Function CheckResult(Nr : Integer; const s, ref: string) : Boolean;
begin
  Result:=AssertEquals('Test '+IntToStr(Nr),Ref,S);
end;

Function TestFloatToStr : String;

var
  e: extended;
  d: double;
  s: single;
  c: currency;
  i: Integer;
  tests: array [0..4] of Double = (123456789123456789., 1e20, 1.6e20, 5e20, 9e20);
  CS,DSep,TSep : String;

begin
  Result:='';
  DSep:=DefaultFormatSettings.DecimalSeparator;
  TSep:=DefaultFormatSettings.ThousandSeparator;
  e:=1234567890123.4;
  d:=12345.12345;
  s:=12345.12;
  c:=12345.1234;
  if not CheckResult(1,FloatToStrF(e,ffExponent,15,1), '1'+DSep+'23456789012340E+12') then exit;
  If not CheckResult(2,FloatToStrF(d,ffExponent,11,0), '1'+DSep+'2345123450E+4') then exit;
  If not CheckResult(3,FloatToStrF(s,ffExponent,8,0), '1'+DSep+'2345120E+4') then exit;
  If not CheckResult(4,FloatToStrF(s,ffExponent,8,7), '1'+DSep+'2345120E+0004') then exit;
  If not CheckResult(5,FloatToStrF(e,ffExponent,8,3), '1'+DSep+'2345679E+012') then exit;
  If not CheckResult(6,FloatToStrF(c,ffExponent,10,0), '1'+DSep+'234512340E+4') then exit;
  If not CheckResult(7,FloatToStrF(c,ffExponent,11,2), '1'+DSep+'2345123400E+04') then exit;
  If not CheckResult(8,FloatToStrF(c,ffExponent,10,4), '1'+DSep+'234512340E+0004') then exit;
  If not CheckResult(9,FloatToStrF(-12345.12345,ffExponent,11,0), '-1'+DSep+'2345123450E+4') then exit;
  If not CheckResult(10,FloatToStrF(-0.00000123,ffGeneral,15,0), '-1'+DSep+'23E-6') then exit;
  If not CheckResult(11,FloatToStrF(-12345.12345,ffGeneral,7,0), '-12345'+DSep+'12') then exit;
  If not CheckResult(12,CurrToStr(-12345.1234), '-12345'+DSep+'1234') then exit;
  If not CheckResult(13,CurrToStr(MaxCurrency), '922337203685477'+DSep+'5807') then exit;
  If not CheckResult(14,CurrToStr(MinCurrency), '-922337203685477'+DSep+'5807') then exit;
  DefaultFormatSettings.NegCurrFormat:=8;
  CS:=DefaultFormatSettings.CurrencyString;
  If not CheckResult(15,FloatToStrF(-12345.1234,ffCurrency,19,4), '-12' + TSep + '345'+DSep+'1234 ' + CS) then exit;
  If not CheckResult(16,FloatToStrF(MinCurrency,ffCurrency,19,4), '-922' + TSep + '337' + TSep + '203' + Tsep + '685' + Tsep + '477'+DSep+'5807 ' + CS) then exit;
  for i := 0 to High(tests) do
    begin
    e := tests[i];
    if not CycleInc(I*10+1,e,1e20) then exit;
    if not CycleInc(I*10+2,e,9e20) then exit;
    if not CycleInc(I*10+3,e,e) then exit;
    if not CycleInc(I*10+3,e,e/2) then exit;
    if not CycleInc(I*10+3,e,e/3) then exit;
    end;
end;

Function TestFormatFloat : TTestString;

Var
  CT : Integer;

  Function Check(aCount : Integer; AExpected,AActual : String): Boolean;

  begin
    Result:=AssertEquals('Check '+IntToStr(aCount),AExpected,AActual);
    CT:=aCount;
  end;


  function TestIt(CR : Extended; Fmt,Expected : String) : Boolean;

  begin
    Result:=Check(CT+1,Expected,FormatFloat(Fmt,CR));
  end;

begin
  Result:='';
  DefaultFormatSettings.ThousandSeparator:=',';
  DefaultFormatSettings.DecimalSeparator:='.';

  if not Check(1,'1.23',FormatFloat('#.##',1.23)) then exit;
  If not Check(3,'1.23',FormatFloat('0.##',1.23)) then exit;
  If not Check(5,'1.23',FormatFloat('#.0#',1.23)) then exit;
  If not Check(7,'1.2',FormatFloat('#.0#',1.2)) then exit;
  If not Check(9,'1.23',FormatFloat('0.0#',1.23)) then exit;
  If not Check(11,'1.23',FormatFloat('0.00',1.23)) then exit;
  If not Check(11,'001.23',FormatFloat('000.00',1.23)) then exit;
  If not Check(13,'1.20',FormatFloat('0.00',1.2)) then exit;

  If not Check(14,'1235',FormatFloat('#####',1234.567)) then exit;
  If not Check(15,'01235',FormatFloat('00000',1234.567)) then exit;
  If not Check(16,'1235',FormatFloat('0',1234.567)) then exit;
  If not Check(17,'1,235',FormatFloat('#,##0',1234.567)) then exit;
  If not Check(18,'1,235',FormatFloat(',0',1234.567)) then exit;
  // Include the decimal value
  If not Check(19,'1234.567',FormatFloat('0.####', 1234.567)) then exit;
  If not Check(20,'1234.5670',FormatFloat('0.0000', 1234.567)) then exit;
  // IsScientific format
  If not Check(22,'1.2345670E+03',FormatFloat('0.0000000E+00', 1234.567)) then exit;
  If not Check(23,'1.2345670E03',FormatFloat('0.0000000E-00', 1234.567)) then exit;
  If not Check(24,'1.234567E3',FormatFloat('#.#######E-##', 1234.567)) then exit;

  // Include freeform text
  If not Check(25,'Value = 1234.6',FormatFloat('"Value = "0.0', 1234.567)) then exit;

  // Different formatting for negative numbers
  If not Check(26,'-1234.6',FormatFloat('0.0', -1234.567)) then exit;
  If not Check(27,'1234.6 DB',FormatFloat('0.0 "CR";0.0 "DB"', -1234.567)) then exit;
  If not Check(28,'1234.6 CR',FormatFloat('0.0 "CR";0.0 "DB"',  1234.567)) then exit;

  // Different format for zero value
  If not Check(29,'0.0',FormatFloat('0.0', 0.0)) then exit;
  If not Check(30,'Nothing',FormatFloat('0.0;-0.0;"Nothing"', 0.0)) then exit;
  If not Check(-30,'Nothing',formatfloat('0.0;-0.0;"Nothing"', 0.0)) then exit;
  // Thousand separators
  // bug 30950
  If not Check(31,'449,888.06',FormatFloat('#,###,##0.00', 449888.06)) then exit;
  // Bug  29781
  If not Check(32,'2,222.00',FormatFloat('###,##0.00', 2222.0)) then exit;
  // tw10519
  if not check(33, '5.22480E+0004', FormatFloat('0.00000E+0000',52247.9532745)) then exit;
  // tw11711
  if not check(34,'-001.000',formatFloat('000.000',-1)) then exit;
  // tw13552
  DefaultFormatSettings.ThousandSeparator:=#0;
  if not Check(35,'1000.00',formatfloat('#,0.00',1000.0)) then exit;
  DefaultFormatSettings.ThousandSeparator:=',';
  // tw15308
  if not Check(36,'1.0500E+002',formatFloat('0.0000E+000', 1.05e2)) then exit;
  if not Check(37,'1.0600E+002',formatFloat('0.0000E+000', 1.06e2)) then exit;
  // tw 12385
  If not Testit(1234.567,'00000000.00','00001234.57') then exit;
  If not Testit(-1234.567,'00000000.00','-00001234.57') then exit;
  If not Testit(-1234.567,'000.00','-1234.57') then exit;
  If not Testit(-1,'000.000','-001.000') then exit;
  If not Testit(-80,'#,##0.00','-80.00') then exit;
  If not Testit(-140,'#,##0.00','-140.00') then exit;
  If not Testit(140,'#,##0.00','140.00') then exit;
  If not Testit(80,'#,##0.00','80.00') then exit;
  If not Testit(-2.45,'#,##0.00','-2.45') then exit;
  If not Testit(-1400,'#,##0.00','-1,400.00') then exit;
  If not Testit(-1400,'##,##0.00','-1,400.00') then exit;
  // tw13076
  if not TestIt(-10,'###,###,##0.00','-10.00') then exit;
end;

begin
  SysutilsTest('testfloattostr',@TestFloatToStr);
  SysutilsTest('TestFormatFloat',@TestFormatFloat);
end.
