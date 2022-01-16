program testscandatetime;

{$mode objfpc}{$H+}

// Run all tests, display all output and count failed tests.
// If undefined, display only failures and halt on first failed test.
{.$define RunAllTests}

uses
  SysUtils, DateUtils;

{$IfDef RunAllTests}
var
  CountTotal: Integer = 0;
  CountFailed: Integer = 0;
{$EndIf}

function TestScanDateTime(
  const FormatSettings: TFormatSettings;
  const FormatStr, DateTimeStr: String;
  const TestDateTime: TDateTime;
  const ShouldFail: Boolean = False): Boolean;
const
  SPACE = '  ';
var
  DT: TDateTime;
  Msg, Error: String;
begin
  Error := '';
  try
    DT := ScanDateTime(FormatStr, DateTimeStr, FormatSettings);
  except on E: Exception do
    Error := 'EXCEPTION: ' + E.Message;
  end;
  if ShouldFail then
    Result := (Error <> '')
  else
    Result := (Error = '') and SameDateTime(DT, TestDateTime);
  Msg := BoolToStr(Result, 'OK', 'FAIL') + SPACE +
    '[' + FormatStr + ']' + SPACE + '[' + DateTimeStr + ']' + SPACE;
  if Error <> '' then
    Msg := Msg + Error
  else
    Msg := Msg + DateTimeToStr(DT) + ' ' + BoolToStr(Result, '==', '<>') +
      ' ' + DateTimeToStr(TestDateTime);
  {$IfDef RunAllTests}
  WriteLn(Msg);
  Inc(CountTotal);
  if not Result then
    Inc(CountFailed);
  {$Else}
  if not Result then
  begin
    WriteLn(Msg);
    Halt(1);
  end;
  {$EndIf}
end;


// Reconstruct an accurate full year so our 2 digit year tests won't fail in future.
function GetFullYear(const F: TFormatSettings; TwoDigitYear: Integer): Integer;
var
  Century: Integer;
begin
  Result := TwoDigitYear;
  if (Result >= 0) and (Result < 100) then
  begin
    Century := YearOf(Now) - F.TwoDigitYearCenturyWindow;
    Inc(Result, Century div 100 * 100);
    if (F.TwoDigitYearCenturyWindow > 0) and (Result < Century) then
      Inc(Result, 100);
  end;
end;

const
  // Use static format settings so we can run tests independantly of system locale!
  Fmt: TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'before-noon'; // testing custom AM/PM string
    TimePMString: 'after-noon';  // testing custom AM/PM string
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );

begin
  TestScanDateTime(Fmt, 'hh:nn:ss', '10:20:30', EncodeTime(10, 20, 30, 0));
  TestScanDateTime(Fmt, 'yyyy-mm-dd hh:nn:ss', '2006-10-06 10:20:30', EncodeDateTime(2006, 10, 6, 10, 20, 30, 0));

  TestScanDateTime(Fmt, 'hhnnss', '102030', EncodeTime(10, 20, 30, 0));
  TestScanDateTime(Fmt, 'yyyymmddhhnnss', '20061006102030', EncodeDateTime(2006, 10, 6, 10, 20, 30, 0));
  TestScanDateTime(Fmt, 'yymmddhhnnss', '061006102030', EncodeDateTime(GetFullYear(Fmt, 6), 10, 6, 10, 20, 30, 0));

  TestScanDateTime(Fmt, 'y-m-d', '6-5-3', EncodeDate(GetFullYear(Fmt, 6), 5, 3));
  TestScanDateTime(Fmt, 'yy-mm-dd', '06-5-3', EncodeDate(GetFullYear(Fmt, 6), 5, 3));
  TestScanDateTime(Fmt, 'yy-mmm-dd', '06-mar-3', EncodeDate(GetFullYear(Fmt, 6), 3, 3));
  TestScanDateTime(Fmt, 'yy-mmmm-dd', '06-mar-3', EncodeDate(GetFullYear(Fmt, 6), 3, 3), True);  // should fail "mar" is not a long name.

  TestScanDateTime(Fmt, 'yy-mm-dd hh:nn:ss am/pm', '06-5-3 05:23:12 pm', EncodeDateTime(GetFullYear(Fmt, 6), 5, 3, 17, 23, 12, 0));
  TestScanDateTime(Fmt, 'mm-dd hh:nn:ss am/pm yyyy', '5-3 05:23:12 pm 2015', EncodeDateTime(2015, 5, 3, 17, 23, 12, 0));
  TestScanDateTime(Fmt, 'hh:nn:ss a/p', '05:23:12 a', EncodeTime(5, 23, 12, 0));
  TestScanDateTime(Fmt, 'hh:nn:ss a/p', '05:23:12 p', EncodeTime(17, 23, 12, 0));
  TestScanDateTime(Fmt, 'hh:nn:ss ampm yyyy-mm-dd', '05:23:12 '+Fmt.TimeAMString+' 2015-02-01', EncodeDateTime(2015, 2, 1, 5, 23, 12, 0));
  TestScanDateTime(Fmt, 'hh:nn:ss ampm yyyy-mm-dd', '05:23:12 '+Fmt.TimePMString+' 2015-02-01', EncodeDateTime(2015, 2, 1, 17, 23, 12, 0));

  TestScanDateTime(Fmt, 'mmmm dd??, yyyy','March 4rd, 2006', EncodeDate(2006,3,4));
  TestScanDateTime(Fmt, 'mmmm dd?, yyyy','March 4rd, 2006', EncodeDate(2006,3,4), True);  // should fail one "?" too few causes "yyyy" mismatch

  TestScanDateTime(Fmt, '''yy-mm-dd'' "yy-mm-dd" yy-mm-dd hh:nn:ss am/pm','yy-mm-dd yy-mm-dd 06-5-3 05:23:12 pm', EncodeDateTime(GetFullYear(Fmt, 6), 5, 3, 17, 23, 12, 0));
  TestScanDateTime(Fmt, #9'yy'#9'mm'#9'dd'#9,'  08     10         12  ', EncodeDate(GetFullYear(Fmt, 8),10,12));

  TestScanDateTime(Fmt, 'yyyy-mm-dd', '2015-02-28', EncodeDate(2015, 2, 28));
  TestScanDateTime(Fmt, 'd mmmm yyyy', '1 November 2015', EncodeDate(2015, 11, 1));
  TestScanDateTime(Fmt, 'd mmmm yyyy', '1 June 2015', EncodeDate(2015, 06, 1));
  TestScanDateTime(Fmt, 'd" "mmmm" "yyyy', '1 June 2015', EncodeDate(2015, 06, 1));

  TestScanDateTime(Fmt, 'dddddd tt', '15 December 2015 16:17:18', EncodeDateTime(2015, 12, 15, 16, 17, 18, 0));
  TestScanDateTime(Fmt, 'tt ddddd', '16:17:18 15-12-15', EncodeDateTime(GetFullYear(Fmt, 15), 12, 15, 16, 17, 18, 0));
  TestScanDateTime(Fmt, 't ddddd', '16:17 15-12-15', EncodeDateTime(GetFullYear(Fmt, 15), 12, 15, 16, 17, 0, 0));
  TestScanDateTime(Fmt, 'dddddd hh:mm:ss', '15 December 2015 16:17:18', EncodeDateTime(2015, 12, 15, 16, 17, 18, 0));
  TestScanDateTime(Fmt, 'd" "mmmm" "yyyy tt', '15 December 2015 16:17:18', EncodeDateTime(2015, 12, 15, 16, 17, 18, 0));

  TestScanDateTime(Fmt, 'yyyy dd mmmm', '2015 1 June', EncodeDate(2015, 6, 1));
  TestScanDateTime(Fmt, 'yyyy dd mmmm', '2015 1 November', EncodeDate(2015, 11, 1));

  {$IfDef RunAllTests}
  WriteLn;
  WriteLn('Failed ', CountFailed, ' of ', CountTotal, ' tests.');
  if CountFailed > 0 then
    Halt(1);
  {$EndIf}
end.

