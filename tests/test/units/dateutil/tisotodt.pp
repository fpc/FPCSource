program Project1;
{$mode objfpc}
uses
  DateUtils;

var
  ErrorCount: Integer = 0;
  ExpectedErrorCount: Integer = 0;

procedure Test(s: String; const Comment: String = '');
var
  dt: TDateTime;
begin
  if Comment <> '' then
    inc(ExpectedErrorCount);

  Write(s:35, ' ---> ');
  try
    dt := ISO8601ToDate(s, true);
    WriteLn(dt:0:15);
  except
    WriteLn('ERROR  (', Comment, ')');
    inc(ErrorCount);
  end;
end;

begin
  WriteLn('This test tries to decode a variety of ISO8601-formatted date/time strings.');
  WriteLn('When the conversion was not successful the text ''ERROR'' appears.');
  WriteLn;
  WriteLn('PART 1: The following tests are expected to produce no errors.');

  // 1 - Test string with separators, dot decimal separator.
  Test('2021-05-22T13:57:49.191021Z');
  Test('2021-05-22T13:57:49.191Z');
  Test('2021-05-22T13:57:49.19Z');
  Test('2021-05-22T13:57:49.1Z');
  Test('2021-05-22T13:57:49.Z');
  Test('2021-05-22T13:57:49Z');
  Test('2021-05-22T13:57Z');
  Test('2021-05-22T13Z');
  WriteLn;

  // 2 - Test string without separators
  Test('20210522T135749.191021Z');
  Test('20210522T135749.191Z');
  Test('20210522T135749.19Z');
  Test('20210522T135749.1Z');
  Test('20210522T135749.Z');
  Test('20210522T135749Z');
  Test('20210522T1357Z');
  Test('20210522T13Z');
  WriteLn;

  // 3 - Fractional seconds, with separators, comma decimal separator.
  Test('2021-05-22T13:57:49,191021Z');
  Test('2021-05-22T13:57:49,191Z');
  Test('2021-05-22T13:57:49,19Z');
  Test('2021-05-22T13:57:49,1Z');
  Test('2021-05-22T13:57:49,Z');
  WriteLn;

  // 4 - Fractional seconds, no separators, comma decimal separator.
  Test('20210522T135749,191021Z');
  Test('20210522T135749,191Z');
  Test('20210522T135749,19Z');
  Test('20210522T135749,1Z');
  Test('20210522T135749,Z');
  WriteLn;

  // 5 - like 1, but positive time zone offset hh:nn
  Test('2021-05-22T13:57:49.191021+02:00');
  Test('2021-05-22T13:57:49.191+02:00');
  Test('2021-05-22T13:57:49.19+02:00');
  Test('2021-05-22T13:57:49.1+02:00');
  Test('2021-05-22T13:57:49.+02:00');
  Test('2021-05-22T13:57:49+02:00');
  Test('2021-05-22T13:57+02:00');
  Test('2021-05-22T13+02:00');
  WriteLn;

  // 6 - like 1, but negative time zone offset hh:nn
  Test('2021-05-22T13:57:49.191021-02:00');
  Test('2021-05-22T13:57:49.191-02:00');
  Test('2021-05-22T13:57:49.19-02:00');
  Test('2021-05-22T13:57:49.1-02:00');
  Test('2021-05-22T13:57:49.-02:00');
  Test('2021-05-22T13:57:49-02:00');
  Test('2021-05-22T13:57-02:00');
  Test('2021-05-22T13-02:00');
  WriteLn;

  // 7 - like 1, but positive time zone offset hhnn
  Test('2021-05-22T13:57:49.191021+0200');
  Test('2021-05-22T13:57:49.191+0200');
  Test('2021-05-22T13:57:49.19+0200');
  Test('2021-05-22T13:57:49.1+0200');
  Test('2021-05-22T13:57:49.+0200');
  Test('2021-05-22T13:57:49+0200');
  Test('2021-05-22T13:57+0200');
  Test('2021-05-22T13+0200');
  WriteLn;

  // 8 - like 1, but negative time zone offset hhnn
  Test('2021-05-22T13:57:49.191021-0200');
  Test('2021-05-22T13:57:49.191-0200');
  Test('2021-05-22T13:57:49.19-0200');
  Test('2021-05-22T13:57:49.1-0200');
  Test('2021-05-22T13:57:49.-0200');
  Test('2021-05-22T13:57:49-0200');
  Test('2021-05-22T13:57-0200');
  Test('2021-05-22T13-0200');
  WriteLn;

  // 9 - like 1, but positive time zone offset hh
  Test('2021-05-22T13:57:49.191021+02');
  Test('2021-05-22T13:57:49.191+02');
  Test('2021-05-22T13:57:49.19+02');
  Test('2021-05-22T13:57:49.1+02');
  Test('2021-05-22T13:57:49.+02');
  Test('2021-05-22T13:57:49+02');
  Test('2021-05-22T13:57+02');
  Test('2021-05-22T13+02');
  WriteLn;

  // 10 - like 1, but negative time zone offset hh
  Test('2021-05-22T13:57:49.191021-02');
  Test('2021-05-22T13:57:49.191-02');
  Test('2021-05-22T13:57:49.19-02');
  Test('2021-05-22T13:57:49.1-02');
  Test('2021-05-22T13:57:49.-02');
  Test('2021-05-22T13:57:49-02');
  Test('2021-05-22T13:57-02');
  Test('2021-05-22T13-02');
  WriteLn;

  // 11 - like 1, no Z
  Test('2021-05-22T13:57:49.191021');
  Test('2021-05-22T13:57:49.191');
  Test('2021-05-22T13:57:49.19');
  Test('2021-05-22T13:57:49.1');
  Test('2021-05-22T13:57:49.');
  Test('2021-05-22T13:57:49');
  Test('2021-05-22T13:57');
  Test('2021-05-22T13');
  Test('20210522T13');
  WriteLn;

  // 12 - Date only
  Test('2021-05-22');
  Test('2021-05');
  Test('2021/05/22');
  Test('2021/05');
  Test('2021');
  WriteLn;

  // 13 - Date only, no separator
  Test('20210522');

  // 14 - Time only, UTC
  Test('T13:57:49.191021Z');
  Test('T13:57:49.191Z');
  Test('T13:57:49.19Z');
  Test('T13:57:49.1Z');
  Test('T13:57:49.Z');
  Test('T13:57:49Z');
  Test('T13:57Z');
  Test('T13Z');
  WriteLn;

  // 15 - Time only, timezone hh:nn
  Test('T13:57:49.191021-02:00');
  Test('T13:57:49.191-02:00');
  Test('T13:57:49.19-02:00');
  Test('T13:57:49.1-02:00');
  Test('T13:57:49.-02:00');
  Test('T13:57:49-02:00');
  Test('T13:57-02:00');
  Test('T13-02:00');
  WriteLn;

  // 16 - Time only, timezone hhnn
  Test('T13:57:49.191021-0200');
  Test('T13:57:49.191-0200');
  Test('T13:57:49.19-0200');
  Test('T13:57:49.1-0200');
  Test('T13:57:49.-0200');
  Test('T13:57:49-0200');
  Test('T13:57-0200');
  Test('T13-0200');
  WriteLn;

  // 17 - Time only, timezone hh
  Test('T13:57:49.191021-02');
  Test('T13:57:49.191-02');
  Test('T13:57:49.19-02');
  Test('T13:57:49.1-02');
  Test('T13:57:49.-02');
  Test('T13:57:49-02');
  Test('T13:57-02');
  Test('T13-02');

  if ErrorCount = 0 then
    WriteLn('No error found in part 1 of the test (0 errors expected)')
  else
    begin
    WriteLn(ErrorCount, ' errors found in part 1 of the test (0 errors expected)');
    Halt(1);
    end;

  WriteLn('PART 2: The following tests are expected to produce errors');
  ErrorCount := 0;
  ExpectedErrorCount := 0;
  Test('21-05-22T13:57:49.191021Z', '2-digit year');
  Test('210522T13:57:49.191021Z', '2-digit year');
  Test('202105', '6-digit date YYYYMM');
  Test('210502', '6-digit date with two-digit year YYMMDD');
  Test('20210522X13:57:491Z', 'wrong "T" separator');

  WriteLn(ErrorCount, ' errors found in part 2 of the test (', ExpectedErrorCount, ' errors expected).');
  WriteLn;
  If ErrorCount<>ExpectedErrorCount then
    Halt(2);
  

end.
