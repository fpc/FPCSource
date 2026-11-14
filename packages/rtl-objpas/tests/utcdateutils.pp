{ Tests of the DateUtils unit: ScanDateTime, ISO 8601 conversion, increments, spans and ranges.
  Includes the tests of tests/test/units/dateutil. }
unit utcdateutils;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, DateUtils, fpcunit, testregistry;

type

  { TTestScanDateTime }

  TTestScanDateTime = class(TTestCase)
  private
    FFmt: TFormatSettings;
    procedure CheckScan(const aPattern, aValue: string; aExpected: TDateTime);
    procedure CheckScanFails(const aPattern, aValue: string);
    procedure CheckRoundTrip(const aPattern: string; aValue: TDateTime);
    function FullYear(aTwoDigitYear: Integer): Integer;
  protected
    procedure SetUp; override;
  published
    // hh, h, nn, n, ss, s, zzz and z
    procedure TestTimeTokens;
    // yyyy, yyy, yy, y, mm, m, dd and d
    procedure TestNumericDateTokens;
    // Two-digit years with and without century window
    procedure TestTwoDigitYear;
    // mmm and mmmm
    procedure TestMonthNames;
    // ddd and dddd match a day name and do not set the day
    procedure TestDayNames;
    // A repeated token takes the value of its last occurrence
    procedure TestRepeatedTokens;
    // Missing year, month or day
    procedure TestPartialDate;
    // m and mm after h or hh are minutes
    procedure TestMinutesAfterHour;
    // ddddd, dddddd, t, tt and c
    procedure TestFormatTokens;
    // am/pm, a/p and ampm
    procedure TestAMPM;
    // Date and time separators
    procedure TestSeparators;
    // Quotes, ?, tab and other literal characters
    procedure TestLiterals;
    // StartPos, trailing characters and an empty pattern
    procedure TestPositions;
    // Input that does not match the pattern
    procedure TestErrors;
    // ScanDateTime reverses FormatDateTime
    procedure TestRoundTrip;
    // Cases of tests/test/units/dateutil/testscandatetime.pas
    procedure TestScanDateTimeProgram;
  end;

  { TTestScanDateTimeDefault }

  TTestScanDateTimeDefault = class(TTestCase)
  private
    FSaved: TFormatSettings;
    procedure CheckScan(const aPattern, aValue: string; aExpected: TDateTime);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // ScanDateTime without format settings uses DefaultFormatSettings
    procedure TestDefaultFormatSettings;
    // am/pm cases of tests/test/units/dateutil/test_scandatetime_ampm.pas
    procedure TestAMPMProgram;
    // a/p cases of tests/test/units/dateutil/test_scandatetime_ampm.pas
    procedure TestAPProgram;
    // ampm cases of tests/test/units/dateutil/test_scandatetime_ampm.pas
    procedure TestAMPMStringsProgram;
  end;

  { TTestISO8601 }

  TTestISO8601 = class(TTestCase)
  private
    procedure CheckISO(const aISO: string; aExpected: TDateTime);
    procedure CheckISOFails(const aISO: string);
    procedure CheckTimes(const aPrefix, aSuffix: string; aDate: TDateTime; aShiftHours: Integer;
      aCompact, aComma: Boolean);
    procedure CheckOffset(const aISO, aExpectedUTC: string; aExpectedOffset: Double);
    procedure CheckOffsetFails(const aISO: string);
  published
    // Date and time with separators, UTC
    procedure TestExtendedUTC;
    // Date and time without separators, UTC
    procedure TestBasicUTC;
    // Comma as decimal separator
    procedure TestCommaFraction;
    // Time zone offsets +hh:nn, -hh:nn, +hhnn, -hhnn, +hh and -hh
    procedure TestTimeZones;
    // Date and time without time zone
    procedure TestNoTimeZone;
    // Date only
    procedure TestDateOnly;
    // Time only
    procedure TestTimeOnly;
    // Strings that are not ISO 8601
    procedure TestInvalid;
    // TryISO8601ToDate with the time zone offset of the string
    procedure TestTimeOffset;
    // ISO8601ToDateDef
    procedure TestDefault;
  end;

  { TTestIncDateTime }

  TTestIncDateTime = class(TTestCase)
  private
    procedure CheckInc(const aName: string; aActual: TDateTime; const aExpected: string; aWithMs: Boolean = False);
  published
    // IncWeek around 1899-12-30
    procedure TestIncWeek;
    // IncDay around 1899-12-30
    procedure TestIncDay;
    // IncHour around 1899-12-30
    procedure TestIncHour;
    // IncMinute around 1899-12-30
    procedure TestIncMinute;
    // IncSecond around 1899-12-30
    procedure TestIncSecond;
    // IncMilliSecond around 1899-12-30
    procedure TestIncMilliSecond;
    // Increments by a Double amount
    procedure TestIncDouble;
  end;

  { TTestDateTimeSpan }

  TTestDateTimeSpan = class(TTestCase)
  published
    // DaysBetween, HoursBetween and MinutesBetween across the zero date
    procedure TestBetween;
    // DaysBetween in steps of two days across the zero date
    procedure TestDaysBetweenSteps;
    // A zero increment and an increment and its inverse return the value
    procedure TestIncRoundTrip;
    // Increments across the zero date
    procedure TestIncAcrossZero;
    // DaySpan and HourSpan with negative values
    procedure TestSpans;
  end;

  { TTestDateTimeRange }

  TTestDateTimeRange = class(TTestCase)
  published
    // Out of range values raise EConvertError
    procedure TestOutOfRange;
    // DateTimeToTimeStamp at the limits of the range
    procedure TestTimeStampLimits;
    // UnixToDateTime and DateTimeToUnix far from 1970
    procedure TestUnixDateTime;
  end;

implementation

uses
  Math;

const
  cDateTimeFmt = 'yyyy-mm-dd hh:nn:ss.zzz';
  cEps = 1e-9;

// Format aValue for messages
function DTStr(aValue: TDateTime): string;

begin
  Result:=FormatDateTime(cDateTimeFmt,aValue);
end;


{ TTestScanDateTime }

procedure TTestScanDateTime.SetUp;

begin
  FFmt:=DefaultFormatSettings;
  FFmt.DateSeparator:='-';
  FFmt.TimeSeparator:=':';
  FFmt.ShortDateFormat:='d/m/y';
  FFmt.LongDateFormat:='dd" "mmmm" "yyyy';
  FFmt.TimeAMString:='before-noon';
  FFmt.TimePMString:='after-noon';
  FFmt.ShortTimeFormat:='hh:nn';
  FFmt.LongTimeFormat:='hh:nn:ss';
  FFmt.ShortMonthNames[1]:='Jan'; FFmt.ShortMonthNames[2]:='Feb'; FFmt.ShortMonthNames[3]:='Mar';
  FFmt.ShortMonthNames[4]:='Apr'; FFmt.ShortMonthNames[5]:='May'; FFmt.ShortMonthNames[6]:='Jun';
  FFmt.ShortMonthNames[7]:='Jul'; FFmt.ShortMonthNames[8]:='Aug'; FFmt.ShortMonthNames[9]:='Sep';
  FFmt.ShortMonthNames[10]:='Oct'; FFmt.ShortMonthNames[11]:='Nov'; FFmt.ShortMonthNames[12]:='Dec';
  FFmt.LongMonthNames[1]:='January'; FFmt.LongMonthNames[2]:='February'; FFmt.LongMonthNames[3]:='March';
  FFmt.LongMonthNames[4]:='April'; FFmt.LongMonthNames[5]:='May'; FFmt.LongMonthNames[6]:='June';
  FFmt.LongMonthNames[7]:='July'; FFmt.LongMonthNames[8]:='August'; FFmt.LongMonthNames[9]:='September';
  FFmt.LongMonthNames[10]:='October'; FFmt.LongMonthNames[11]:='November'; FFmt.LongMonthNames[12]:='December';
  FFmt.ShortDayNames[1]:='Sun'; FFmt.ShortDayNames[2]:='Mon'; FFmt.ShortDayNames[3]:='Tue';
  FFmt.ShortDayNames[4]:='Wed'; FFmt.ShortDayNames[5]:='Thu'; FFmt.ShortDayNames[6]:='Fri';
  FFmt.ShortDayNames[7]:='Sat';
  FFmt.LongDayNames[1]:='Sunday'; FFmt.LongDayNames[2]:='Monday'; FFmt.LongDayNames[3]:='Tuesday';
  FFmt.LongDayNames[4]:='Wednesday'; FFmt.LongDayNames[5]:='Thursday'; FFmt.LongDayNames[6]:='Friday';
  FFmt.LongDayNames[7]:='Saturday';
  FFmt.TwoDigitYearCenturyWindow:=50;
end;


procedure TTestScanDateTime.CheckScan(const aPattern, aValue: string; aExpected: TDateTime);

var
  lActual: TDateTime;

begin
  try
    lActual:=ScanDateTime(aPattern,aValue,FFmt);
  except
    on E: Exception do
      Fail(Format('[%s] [%s] raises %s: %s',[aPattern,aValue,E.ClassName,E.Message]));
  end;
  AssertEquals(Format('[%s] [%s] gives %s',[aPattern,aValue,DTStr(aExpected)]),aExpected,lActual,cEps);
end;


procedure TTestScanDateTime.CheckScanFails(const aPattern, aValue: string);

var
  lActual: TDateTime;

begin
  try
    lActual:=ScanDateTime(aPattern,aValue,FFmt);
  except
    on E: EConvertError do
      Exit;
  end;
  Fail(Format('[%s] [%s] raises EConvertError, got %s',[aPattern,aValue,DTStr(lActual)]));
end;


procedure TTestScanDateTime.CheckRoundTrip(const aPattern: string; aValue: TDateTime);

begin
  CheckScan(aPattern,FormatDateTime(aPattern,aValue,FFmt),aValue);
end;


function TTestScanDateTime.FullYear(aTwoDigitYear: Integer): Integer;

var
  lCentury: Integer;

begin
  Result:=aTwoDigitYear;
  lCentury:=YearOf(Now)-FFmt.TwoDigitYearCenturyWindow;
  Inc(Result,lCentury div 100 * 100);
  if (FFmt.TwoDigitYearCenturyWindow>0) and (Result<lCentury) then
    Inc(Result,100);
end;


procedure TTestScanDateTime.TestTimeTokens;

begin
  CheckScan('hh:nn:ss','10:20:30',EncodeTime(10,20,30,0));
  CheckScan('h:n:s','1:2:3',EncodeTime(1,2,3,0));
  CheckScan('h:n:s','01:02:03',EncodeTime(1,2,3,0));
  CheckScan('hhnnss','102030',EncodeTime(10,20,30,0));
  CheckScan('hh:nn:ss.zzz','10:20:30.456',EncodeTime(10,20,30,456));
  CheckScan('hh:nn:ss.z','10:20:30.7',EncodeTime(10,20,30,7));
  CheckScan('hh:nn:ss.zzz','23:59:59.999',EncodeTime(23,59,59,999));
  CheckScan('hh','00',0);
  CheckScan('hh:nn','10:20',EncodeTime(10,20,0,0));
  CheckScan('nn:ss','20:30',EncodeTime(0,20,30,0));
  CheckScan('hhnnsszzz','102030456',EncodeTime(10,20,30,456));
end;


procedure TTestScanDateTime.TestNumericDateTokens;

begin
  CheckScan('yyyy-mm-dd','2015-02-28',EncodeDate(2015,2,28));
  CheckScan('yyyy-m-d','2015-2-8',EncodeDate(2015,2,8));
  CheckScan('yyyymmdd','20151215',EncodeDate(2015,12,15));
  CheckScan('dd-mm-yyyy','15-12-2015',EncodeDate(2015,12,15));
  CheckScan('yyy-mm-dd','206-05-03',EncodeDate(206,5,3));
  CheckScan('yyyy-mm-dd','2000-02-29',EncodeDate(2000,2,29));
  CheckScan('yyyy-mm-dd hh:nn:ss','2006-10-06 10:20:30',EncodeDateTime(2006,10,6,10,20,30,0));
  CheckScan('yyyymmddhhnnss','20061006102030',EncodeDateTime(2006,10,6,10,20,30,0));
  CheckScan('yyyy-mm-dd','0001-01-01',EncodeDate(1,1,1));
  CheckScan('yyyy-mm-dd','9999-12-31',EncodeDate(9999,12,31));
end;


procedure TTestScanDateTime.TestTwoDigitYear;

begin
  CheckScan('yy-mm-dd','06-5-3',EncodeDate(FullYear(6),5,3));
  CheckScan('y-m-d','6-5-3',EncodeDate(FullYear(6),5,3));
  CheckScan('yy-mm-dd','99-12-31',EncodeDate(FullYear(99),12,31));
  CheckScan('yymmddhhnnss','061006102030',EncodeDateTime(FullYear(6),10,6,10,20,30,0));
  FFmt.TwoDigitYearCenturyWindow:=0;
  CheckScan('yy-mm-dd','06-5-3',EncodeDate(YearOf(Now) div 100 * 100 + 6,5,3));
  CheckScan('yy-mm-dd','99-5-3',EncodeDate(YearOf(Now) div 100 * 100 + 99,5,3));
end;


procedure TTestScanDateTime.TestMonthNames;

begin
  CheckScan('yy-mmm-dd','06-mar-3',EncodeDate(FullYear(6),3,3));
  CheckScan('yyyy-mmm-dd','2006-MAR-03',EncodeDate(2006,3,3));
  CheckScan('yyyy-mmm-dd','2006-Dec-03',EncodeDate(2006,12,3));
  CheckScan('d mmmm yyyy','1 November 2015',EncodeDate(2015,11,1));
  CheckScan('d mmmm yyyy','1 june 2015',EncodeDate(2015,6,1));
  CheckScan('mmmm d yyyy','SEPTEMBER 30 2015',EncodeDate(2015,9,30));
  CheckScan('yyyy dd mmmm','2015 1 June',EncodeDate(2015,6,1));
  CheckScan('yyyy dd mmmm','2015 1 November',EncodeDate(2015,11,1));
  CheckScanFails('yy-mmmm-dd','06-mar-3');
  CheckScanFails('yyyy-mmm-dd','2006-Xyz-03');
end;


procedure TTestScanDateTime.TestDayNames;

begin
  CheckScan('ddd dd-mm-yyyy','Tue 15-12-2015',EncodeDate(2015,12,15));
  CheckScan('dd-mm-yyyy ddd','15-12-2015 Tue',EncodeDate(2015,12,15));
  CheckScan('dd-mm-yyyy ddd','15-12-2015 Sat',EncodeDate(2015,12,15));
  CheckScan('dddd, d mmmm yyyy','Tuesday, 15 December 2015',EncodeDate(2015,12,15));
  CheckScan('d mmmm yyyy, dddd','15 December 2015, sunday',EncodeDate(2015,12,15));
  CheckScan('ddd hh:nn','Mon 10:20',EncodeTime(10,20,0,0));
  CheckScanFails('ddd dd-mm-yyyy','Xyz 15-12-2015');
  CheckScanFails('dddd dd-mm-yyyy','Tue 15-12-2015');
end;


procedure TTestScanDateTime.TestRepeatedTokens;

begin
  CheckScan('hh hh','10 11',EncodeTime(11,0,0,0));
  CheckScan('hh:nn hh:nn','10:20 11:30',EncodeTime(11,30,0,0));
  CheckScan('nn nn','20 30',EncodeTime(0,30,0,0));
  CheckScan('ss ss','20 30',EncodeTime(0,0,30,0));
  CheckScan('zzz zzz','200 300',EncodeTime(0,0,0,300));
  CheckScan('hh:mm hh:mm','10:20 11:30',EncodeTime(11,30,0,0));
  CheckScan('yyyy-mm-dd yyyy','2015-12-15 2016',EncodeDate(2016,12,15));
  CheckScan('yyyy-mm-dd dd','2015-12-15 16',EncodeDate(2015,12,16));
  CheckScan('yyyy-mm-dd mm','2015-12-15 11',EncodeDate(2015,11,15));
  CheckScan('tt tt','10:20:30 11:21:31',EncodeTime(11,21,31,0));
end;


procedure TTestScanDateTime.TestPartialDate;

var
  lYear: Integer;

begin
  lYear:=YearOf(Date);
  CheckScan('yyyy','2015',EncodeDate(2015,1,1));
  CheckScan('mm-yyyy','12-2015',EncodeDate(2015,12,1));
  CheckScan('mmmm yyyy','June 2015',EncodeDate(2015,6,1));
  CheckScan('dd-mm','15-12',EncodeDate(lYear,12,15));
  CheckScan('mmmm','June',EncodeDate(lYear,6,1));
  CheckScan('dd','15',EncodeDate(lYear,1,15));
  CheckScan('yyyy-dd','2015-15',EncodeDate(2015,1,15));
  CheckScan('yyyy hh:nn','2015 10:20',EncodeDateTime(2015,1,1,10,20,0,0));
  CheckScan('dd-mm hh:nn','15-12 10:20',EncodeDateTime(lYear,12,15,10,20,0,0));
  CheckScan('hh:nn:ss','10:20:30',EncodeTime(10,20,30,0));
  CheckScan('ddd','Mon',0);
end;


procedure TTestScanDateTime.TestMinutesAfterHour;

begin
  CheckScan('hh:mm','10:20',EncodeTime(10,20,0,0));
  CheckScan('hh:m','10:5',EncodeTime(10,5,0,0));
  CheckScan('hhmm','1020',EncodeTime(10,20,0,0));
  CheckScan('hh:mm:ss','10:20:30',EncodeTime(10,20,30,0));
  CheckScan('dddddd hh:mm:ss','15 December 2015 16:17:18',EncodeDateTime(2015,12,15,16,17,18,0));
  CheckScan('mm-dd hh:mm','12-15 10:20',EncodeDateTime(YearOf(Date),12,15,10,20,0,0));
  CheckScanFails('hh:mmm','10:Jan');
end;


procedure TTestScanDateTime.TestFormatTokens;

begin
  CheckScan('ddddd','15-12-15',EncodeDate(FullYear(15),12,15));
  CheckScan('dddddd','15 December 2015',EncodeDate(2015,12,15));
  CheckScan('t','16:17',EncodeTime(16,17,0,0));
  CheckScan('tt','16:17:18',EncodeTime(16,17,18,0));
  CheckScan('dddddd tt','15 December 2015 16:17:18',EncodeDateTime(2015,12,15,16,17,18,0));
  CheckScan('tt ddddd','16:17:18 15-12-15',EncodeDateTime(FullYear(15),12,15,16,17,18,0));
  CheckScan('t ddddd','16:17 15-12-15',EncodeDateTime(FullYear(15),12,15,16,17,0,0));
  CheckScan('c','15-12-15 16:17:18',EncodeDateTime(FullYear(15),12,15,16,17,18,0));
  CheckScan('c','15-12-15',EncodeDate(FullYear(15),12,15));
  CheckScan('c','15-12-1516:17:18',EncodeDateTime(FullYear(15),12,15,16,17,18,0));
  CheckScan('"at" c','at 15-12-15 16:17:18',EncodeDateTime(FullYear(15),12,15,16,17,18,0));
end;


procedure TTestScanDateTime.TestAMPM;

begin
  CheckScan('hh:nn:ss am/pm','05:23:12 pm',EncodeTime(17,23,12,0));
  CheckScan('hh:nn:ss am/pm','05:23:12 AM',EncodeTime(5,23,12,0));
  CheckScan('hh:nn am/pm','12:00 am',0);
  CheckScan('hh:nn am/pm','12:30 am',EncodeTime(0,30,0,0));
  CheckScan('hh:nn am/pm','12:00 pm',EncodeTime(12,0,0,0));
  CheckScan('hh:nn am/pm','00:30 pm',EncodeTime(0,30,0,0));
  CheckScan('hh:nn am/pm','11:59 pm',EncodeTime(23,59,0,0));
  CheckScan('hh:nn:ss a/p','05:23:12 a',EncodeTime(5,23,12,0));
  CheckScan('hh:nn:ss a/p','05:23:12 P',EncodeTime(17,23,12,0));
  CheckScan('hh:nn:ss ampm','05:23:12 before-noon',EncodeTime(5,23,12,0));
  CheckScan('hh:nn:ss ampm','05:23:12 AFTER-NOON',EncodeTime(17,23,12,0));
  CheckScan('am/pm hh:nn','pm 01:30',EncodeTime(13,30,0,0));
  CheckScan('a/p hh:nn','a 12:30',EncodeTime(0,30,0,0));
  CheckScan('hh:nn am/pm hh','01:30 pm 02',EncodeTime(14,30,0,0));
  CheckScan('hh:nn:ss.zzz am/pm','11:59:59.999 pm',EncodeTime(23,59,59,999));
  CheckScan('hh:nn ax','01:30 ax',EncodeTime(1,30,0,0));
  CheckScanFails('hh:nn am/pm','13:00 pm');
  CheckScanFails('hh:nn am/pm','13:00 am');
  CheckScanFails('hh:nn am/pm','01:00 xm');
  CheckScanFails('hh:nn a/p','01:00 x');
  CheckScanFails('hh:nn a/p','01:00 ');
  CheckScanFails('hh:nn ampm','01:00 pm');
end;


procedure TTestScanDateTime.TestSeparators;

begin
  CheckScan('yyyy/mm/dd','2015-12-15',EncodeDate(2015,12,15));
  CheckScanFails('yyyy/mm/dd','2015/12/15');
  FFmt.DateSeparator:='.';
  FFmt.TimeSeparator:='h';
  CheckScan('dd/mm/yyyy','15.12.2015',EncodeDate(2015,12,15));
  CheckScan('hh:nn','10h20',EncodeTime(10,20,0,0));
  CheckScan('hh:mm','10h20',EncodeTime(10,20,0,0));
  CheckScanFails('hh:nn','10:20');
end;


procedure TTestScanDateTime.TestLiterals;

begin
  CheckScan('''yy-mm-dd'' "yy-mm-dd" yy-mm-dd hh:nn:ss am/pm','yy-mm-dd yy-mm-dd 06-5-3 05:23:12 pm',
    EncodeDateTime(FullYear(6),5,3,17,23,12,0));
  CheckScan('"hh" hh','hh 10',EncodeTime(10,0,0,0));
  CheckScan('''it''''s'' hh','its 10',EncodeTime(10,0,0,0));
  CheckScan('mmmm dd??, yyyy','March 4rd, 2006',EncodeDate(2006,3,4));
  CheckScan('?hh','x10',EncodeTime(10,0,0,0));
  CheckScan(#9'yy'#9'mm'#9'dd'#9,'  08     10         12  ',EncodeDate(FullYear(8),10,12));
  CheckScan('yyyy'#9'mm','2015'#13#10'12',EncodeDate(2015,12,1));
  CheckScan('yyyy'#9'mm','201512',EncodeDate(2015,12,1));
  CheckScan('yyyy-mm-dd'#9,'2015-12-15',EncodeDate(2015,12,15));
  CheckScan('d" "mmmm" "yyyy','1 June 2015',EncodeDate(2015,6,1));
  CheckScan('yyyy-mm-dd"T"hh:nn','2015-12-15T10:20',EncodeDateTime(2015,12,15,10,20,0,0));
  CheckScan('yyyy-mm-ddT','2015-12-1510:20',EncodeDateTime(2015,12,15,10,20,0,0));
  CheckScan('hh:nn x','10:20 x',EncodeTime(10,20,0,0));
  CheckScanFails('mmmm dd?, yyyy','March 4rd, 2006');
  CheckScanFails('hh:nn x','10:20 y');
end;


procedure TTestScanDateTime.TestPositions;

begin
  AssertEquals('StartPos 3 skips 2 characters',EncodeTime(10,20,0,0),ScanDateTime('hh:nn','xx10:20',FFmt,3),cEps);
  AssertEquals('StartPos without format settings',EncodeTime(10,20,0,0),ScanDateTime('hh:nn','xx10:20',3),cEps);
  CheckScan('yyyy','2015abc',EncodeDate(2015,1,1));
  CheckScan('hh:nn','10:20:30',EncodeTime(10,20,0,0));
  CheckScan('yyyy','20151',EncodeDate(2015,1,1));
  CheckScan('','abc',0);
  CheckScanFails('hh','');
end;


procedure TTestScanDateTime.TestErrors;

begin
  CheckScanFails('hh','ab');
  CheckScanFails('hh:nn','10-20');
  CheckScanFails('yyyy-mm-dd','2015-02');
  CheckScanFails('yyyy-mm-dd','2015-02-30');
  CheckScanFails('yyyy-mm-dd','2015-13-01');
  CheckScanFails('yyyy-mm-dd','2015-00-01');
  CheckScanFails('yyyy-mm-dd','2015-01-00');
  CheckScanFails('hh:nn:ss','10:20:');
  CheckScanFails('"x"hh','y10');
  CheckScanFails('c','15-12-15 16:17');
end;


procedure TTestScanDateTime.TestRoundTrip;

const
  cPatterns: array[0..8] of string = (
    'yyyy-mm-dd hh:nn:ss.zzz', 'dd mmmm yyyy hh:nn', 'ddd, d mmm yyyy hh:nn:ss', 'c', 'dddddd tt',
    'yyyymmddhhnnsszzz', 'hh:nn am/pm d/m/yyyy', 'dddd d mmmm yyyy h:n:s a/p', 'yyyy-mm-dd"T"hh:nn:ss ampm');

var
  lValues: array[0..4] of TDateTime;
  lValue: TDateTime;
  lPattern: string;

begin
  lValues[0]:=EncodeDateTime(2000,2,29,0,0,0,0);
  lValues[1]:=EncodeDateTime(2015,12,15,16,17,18,0);
  lValues[2]:=EncodeDateTime(1999,1,1,12,0,0,0);
  lValues[3]:=EncodeDateTime(2024,7,4,23,59,59,0);
  lValues[4]:=EncodeDateTime(2030,10,31,0,30,0,0);
  for lPattern in cPatterns do
    for lValue in lValues do
      if (Pos('zzz',lPattern)>0) or (Pos('s',lPattern)>0) or (Pos('c',lPattern)>0) or (SecondOf(lValue)=0) then
        CheckRoundTrip(lPattern,lValue);
  CheckRoundTrip('yyyy-mm-dd hh:nn:ss.zzz',EncodeDateTime(2024,7,4,23,59,59,999));
  CheckRoundTrip('yyyymmddhhnnsszzz',EncodeDateTime(2024,7,4,0,0,0,1));
end;


procedure TTestScanDateTime.TestScanDateTimeProgram;

begin
  CheckScan('hh:nn:ss','10:20:30',EncodeTime(10,20,30,0));
  CheckScan('yyyy-mm-dd hh:nn:ss','2006-10-06 10:20:30',EncodeDateTime(2006,10,6,10,20,30,0));
  CheckScan('hhnnss','102030',EncodeTime(10,20,30,0));
  CheckScan('yyyymmddhhnnss','20061006102030',EncodeDateTime(2006,10,6,10,20,30,0));
  CheckScan('yymmddhhnnss','061006102030',EncodeDateTime(FullYear(6),10,6,10,20,30,0));
  CheckScan('y-m-d','6-5-3',EncodeDate(FullYear(6),5,3));
  CheckScan('yy-mm-dd','06-5-3',EncodeDate(FullYear(6),5,3));
  CheckScan('yy-mmm-dd','06-mar-3',EncodeDate(FullYear(6),3,3));
  CheckScanFails('yy-mmmm-dd','06-mar-3');
  CheckScan('yy-mm-dd hh:nn:ss am/pm','06-5-3 05:23:12 pm',EncodeDateTime(FullYear(6),5,3,17,23,12,0));
  CheckScan('mm-dd hh:nn:ss am/pm yyyy','5-3 05:23:12 pm 2015',EncodeDateTime(2015,5,3,17,23,12,0));
  CheckScan('hh:nn:ss a/p','05:23:12 a',EncodeTime(5,23,12,0));
  CheckScan('hh:nn:ss a/p','05:23:12 p',EncodeTime(17,23,12,0));
  CheckScan('hh:nn:ss ampm yyyy-mm-dd','05:23:12 '+FFmt.TimeAMString+' 2015-02-01',EncodeDateTime(2015,2,1,5,23,12,0));
  CheckScan('hh:nn:ss ampm yyyy-mm-dd','05:23:12 '+FFmt.TimePMString+' 2015-02-01',EncodeDateTime(2015,2,1,17,23,12,0));
  CheckScan('mmmm dd??, yyyy','March 4rd, 2006',EncodeDate(2006,3,4));
  CheckScanFails('mmmm dd?, yyyy','March 4rd, 2006');
  CheckScan('''yy-mm-dd'' "yy-mm-dd" yy-mm-dd hh:nn:ss am/pm','yy-mm-dd yy-mm-dd 06-5-3 05:23:12 pm',
    EncodeDateTime(FullYear(6),5,3,17,23,12,0));
  CheckScan(#9'yy'#9'mm'#9'dd'#9,'  08     10         12  ',EncodeDate(FullYear(8),10,12));
  CheckScan('yyyy-mm-dd','2015-02-28',EncodeDate(2015,2,28));
  CheckScan('d mmmm yyyy','1 November 2015',EncodeDate(2015,11,1));
  CheckScan('d mmmm yyyy','1 June 2015',EncodeDate(2015,6,1));
  CheckScan('d" "mmmm" "yyyy','1 June 2015',EncodeDate(2015,6,1));
  CheckScan('dddddd tt','15 December 2015 16:17:18',EncodeDateTime(2015,12,15,16,17,18,0));
  CheckScan('tt ddddd','16:17:18 15-12-15',EncodeDateTime(FullYear(15),12,15,16,17,18,0));
  CheckScan('t ddddd','16:17 15-12-15',EncodeDateTime(FullYear(15),12,15,16,17,0,0));
  CheckScan('dddddd hh:mm:ss','15 December 2015 16:17:18',EncodeDateTime(2015,12,15,16,17,18,0));
  CheckScan('d" "mmmm" "yyyy tt','15 December 2015 16:17:18',EncodeDateTime(2015,12,15,16,17,18,0));
  CheckScan('yyyy dd mmmm','2015 1 June',EncodeDate(2015,6,1));
  CheckScan('yyyy dd mmmm','2015 1 November',EncodeDate(2015,11,1));
end;


{ TTestScanDateTimeDefault }

procedure TTestScanDateTimeDefault.SetUp;

const
  cMonths: array[1..12] of string = ('January','February','March','April','May','June','July','August',
    'September','October','November','December');

var
  i: Integer;

begin
  FSaved:=DefaultFormatSettings;
  for i:=1 to 12 do
    begin
    DefaultFormatSettings.LongMonthNames[i]:=cMonths[i];
    DefaultFormatSettings.ShortMonthNames[i]:=Copy(cMonths[i],1,3);
    end;
  DefaultFormatSettings.TimeSeparator:=':';
  DefaultFormatSettings.DateSeparator:='-';
  DefaultFormatSettings.TimeAMString:='AM';
  DefaultFormatSettings.TimePMString:='PM';
end;


procedure TTestScanDateTimeDefault.TearDown;

begin
  DefaultFormatSettings:=FSaved;
end;


procedure TTestScanDateTimeDefault.CheckScan(const aPattern, aValue: string; aExpected: TDateTime);

var
  lActual: TDateTime;

begin
  try
    lActual:=ScanDateTime(aPattern,aValue);
  except
    on E: Exception do
      Fail(Format('[%s] [%s] raises %s: %s',[aPattern,aValue,E.ClassName,E.Message]));
  end;
  AssertEquals(Format('[%s] [%s] gives %s',[aPattern,aValue,DTStr(aExpected)]),aExpected,lActual,cEps);
end;


procedure TTestScanDateTimeDefault.TestDefaultFormatSettings;

begin
  CheckScan('yyyy/mm/dd hh:nn','2015-12-15 10:20',EncodeDateTime(2015,12,15,10,20,0,0));
  DefaultFormatSettings.DateSeparator:='.';
  DefaultFormatSettings.LongMonthNames[12]:='Dezember';
  CheckScan('dd/mm/yyyy','15.12.2015',EncodeDate(2015,12,15));
  CheckScan('d mmmm yyyy','15 Dezember 2015',EncodeDate(2015,12,15));
end;


procedure TTestScanDateTimeDefault.TestAMPMProgram;

const
  cPat = 'mmmm dd??, yyyy, hh:nn am/pm';
  cPatMs = 'mmmm dd??, yyyy, hh:nn:ss.zzz am/pm';

begin
  CheckScan(cPat,'April 2nd, 2014, 12:00 am',EncodeDateTime(2014,4,2,0,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 12:00 AM',EncodeDateTime(2014,4,2,0,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 12:01 am',EncodeDateTime(2014,4,2,0,1,0,0));
  CheckScan(cPat,'April 2nd, 2014, 01:00 am',EncodeDateTime(2014,4,2,1,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 11:00 am',EncodeDateTime(2014,4,2,11,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 11:59 am',EncodeDateTime(2014,4,2,11,59,0,0));
  CheckScan(cPatMs,'April 2nd, 2014, 11:59:59.999 am',EncodeDateTime(2014,4,2,11,59,59,999));
  CheckScan(cPat,'April 2nd, 2014, 12:00 pm',EncodeDateTime(2014,4,2,12,0,0,0));
  CheckScan(cPatMs,'April 2nd, 2014, 12:00:00.001 pm',EncodeDateTime(2014,4,2,12,0,0,1));
  CheckScan(cPat,'April 2nd, 2014, 01:00 pm',EncodeDateTime(2014,4,2,13,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 01:01 pm',EncodeDateTime(2014,4,2,13,1,0,0));
  CheckScan(cPat,'April 2nd, 2014, 11:00 pm',EncodeDateTime(2014,4,2,23,0,0,0));
  CheckScan(cPatMs,'April 2nd, 2014, 11:59:59.999 pm',EncodeDateTime(2014,4,2,23,59,59,999));
  try
    ScanDateTime(cPat,'April 2nd, 2014, 13:00 pm');
    Fail('13:00 pm raises EConvertError');
  except
    on E: EConvertError do ;
  end;
end;


procedure TTestScanDateTimeDefault.TestAPProgram;

const
  cPat = 'mmmm dd??, yyyy, hh:nn a/p';
  cPatMs = 'mmmm dd??, yyyy, hh:nn:ss.zzz a/p';

begin
  CheckScan(cPat,'April 2nd, 2014, 12:00 a',EncodeDateTime(2014,4,2,0,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 12:00 A',EncodeDateTime(2014,4,2,0,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 12:01 a',EncodeDateTime(2014,4,2,0,1,0,0));
  CheckScan(cPat,'April 2nd, 2014, 01:00 a',EncodeDateTime(2014,4,2,1,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 11:00 a',EncodeDateTime(2014,4,2,11,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 11:59 a',EncodeDateTime(2014,4,2,11,59,0,0));
  CheckScan(cPatMs,'April 2nd, 2014, 11:59:59.999 a',EncodeDateTime(2014,4,2,11,59,59,999));
  CheckScan(cPat,'April 2nd, 2014, 12:00 p',EncodeDateTime(2014,4,2,12,0,0,0));
  CheckScan(cPatMs,'April 2nd, 2014, 12:00:00.001 p',EncodeDateTime(2014,4,2,12,0,0,1));
  CheckScan(cPat,'April 2nd, 2014, 01:00 p',EncodeDateTime(2014,4,2,13,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 01:01 p',EncodeDateTime(2014,4,2,13,1,0,0));
  CheckScan(cPat,'April 2nd, 2014, 11:00 p',EncodeDateTime(2014,4,2,23,0,0,0));
  CheckScan(cPatMs,'April 2nd, 2014, 11:59:59.999 p',EncodeDateTime(2014,4,2,23,59,59,999));
end;


procedure TTestScanDateTimeDefault.TestAMPMStringsProgram;

const
  cPat = 'mmmm dd??, yyyy, hh:nn ampm';
  cPatMs = 'mmmm dd??, yyyy, hh:nn:ss.zzz ampm';

begin
  DefaultFormatSettings.TimeAMString:='vorm';
  DefaultFormatSettings.TimePMString:='nachm';
  CheckScan(cPat,'April 2nd, 2014, 12:00 vorm',EncodeDateTime(2014,4,2,0,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 12:00 VORM',EncodeDateTime(2014,4,2,0,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 12:01 vorm',EncodeDateTime(2014,4,2,0,1,0,0));
  CheckScan(cPat,'April 2nd, 2014, 01:00 vorm',EncodeDateTime(2014,4,2,1,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 11:00 vorm',EncodeDateTime(2014,4,2,11,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 11:59 vorm',EncodeDateTime(2014,4,2,11,59,0,0));
  CheckScan(cPatMs,'April 2nd, 2014, 11:59:59.999 vorm',EncodeDateTime(2014,4,2,11,59,59,999));
  CheckScan(cPat,'April 2nd, 2014, 12:00 nachm',EncodeDateTime(2014,4,2,12,0,0,0));
  CheckScan(cPatMs,'April 2nd, 2014, 12:00:00.001 nachm',EncodeDateTime(2014,4,2,12,0,0,1));
  CheckScan(cPat,'April 2nd, 2014, 01:00 nachm',EncodeDateTime(2014,4,2,13,0,0,0));
  CheckScan(cPat,'April 2nd, 2014, 01:01 nachm',EncodeDateTime(2014,4,2,13,1,0,0));
  CheckScan(cPat,'April 2nd, 2014, 11:00 nachm',EncodeDateTime(2014,4,2,23,0,0,0));
  CheckScan(cPatMs,'April 2nd, 2014, 11:59:59.999 nachm',EncodeDateTime(2014,4,2,23,59,59,999));
  CheckScan(cPat,'April 3rd, 2014, 12:00 nachm',EncodeDateTime(2014,4,3,12,0,0,0));
end;


{ TTestISO8601 }

type
  TISOTime = record
    Extended, Basic: string;
    Hour, Minute, Second: Word;
    Fraction: Double;
  end;

const
  cISOTimes: array[0..7] of TISOTime = (
    (Extended: '13:57:49.191021'; Basic: '135749.191021'; Hour: 13; Minute: 57; Second: 49; Fraction: 0.191021),
    (Extended: '13:57:49.191'; Basic: '135749.191'; Hour: 13; Minute: 57; Second: 49; Fraction: 0.191),
    (Extended: '13:57:49.19'; Basic: '135749.19'; Hour: 13; Minute: 57; Second: 49; Fraction: 0.19),
    (Extended: '13:57:49.1'; Basic: '135749.1'; Hour: 13; Minute: 57; Second: 49; Fraction: 0.1),
    (Extended: '13:57:49.'; Basic: '135749.'; Hour: 13; Minute: 57; Second: 49; Fraction: 0),
    (Extended: '13:57:49'; Basic: '135749'; Hour: 13; Minute: 57; Second: 49; Fraction: 0),
    (Extended: '13:57'; Basic: '1357'; Hour: 13; Minute: 57; Second: 0; Fraction: 0),
    (Extended: '13'; Basic: '13'; Hour: 13; Minute: 0; Second: 0; Fraction: 0));

  cISOFractionCount = 5;

procedure TTestISO8601.CheckISO(const aISO: string; aExpected: TDateTime);

var
  lActual: TDateTime;

begin
  AssertTrue(aISO+' is accepted',TryISO8601ToDate(aISO,lActual,True));
  AssertEquals(aISO+' gives '+DTStr(aExpected),aExpected,lActual,cEps);
  AssertEquals(aISO+' ISO8601ToDate gives '+DTStr(aExpected),aExpected,ISO8601ToDate(aISO,True),cEps);
end;


procedure TTestISO8601.CheckISOFails(const aISO: string);

var
  lActual: TDateTime;

begin
  AssertFalse(aISO+' is rejected',TryISO8601ToDate(aISO,lActual,True));
  try
    ISO8601ToDate(aISO,True);
    Fail(aISO+' ISO8601ToDate raises EConvertError');
  except
    on E: EConvertError do ;
  end;
end;


procedure TTestISO8601.CheckTimes(const aPrefix, aSuffix: string; aDate: TDateTime; aShiftHours: Integer;
  aCompact, aComma: Boolean);

var
  i, lCount: Integer;
  lTime: string;
  lExpected: TDateTime;

begin
  if aComma then
    lCount:=cISOFractionCount
  else
    lCount:=Length(cISOTimes);
  for i:=0 to lCount-1 do
    begin
    if aCompact then
      lTime:=cISOTimes[i].Basic
    else
      lTime:=cISOTimes[i].Extended;
    if aComma then
      lTime:=StringReplace(lTime,'.',',',[]);
    lExpected:=aDate+EncodeTime(cISOTimes[i].Hour,cISOTimes[i].Minute,cISOTimes[i].Second,0)
               +cISOTimes[i].Fraction*OneSecond+aShiftHours*OneHour;
    CheckISO(aPrefix+lTime+aSuffix,lExpected);
    end;
end;


procedure TTestISO8601.CheckOffset(const aISO, aExpectedUTC: string; aExpectedOffset: Double);

var
  lDate, lPlain: TDateTime;
  lOffset: Double;

begin
  AssertTrue(aISO+' is accepted',TryISO8601ToDate(aISO,lDate,lOffset));
  AssertEquals(aISO+' UTC',aExpectedUTC,FormatDateTime('yyyy-mm-dd hh:nn:ss',lDate));
  AssertEquals(aISO+' offset',aExpectedOffset,lOffset,1e-9);
  AssertTrue(aISO+' is accepted without offset',TryISO8601ToDate(aISO,lPlain));
  AssertEquals(aISO+' gives the same date without offset',lDate,lPlain,0);
  AssertTrue(aISO+' is accepted with ReturnUTC=False',TryISO8601ToDate(aISO,lDate,lOffset,False));
  AssertEquals(aISO+' offset with ReturnUTC=False',aExpectedOffset,lOffset,1e-9);
end;


procedure TTestISO8601.CheckOffsetFails(const aISO: string);

var
  lDate: TDateTime;
  lOffset: Double;

begin
  lOffset:=99;
  AssertFalse(aISO+' is rejected',TryISO8601ToDate(aISO,lDate,lOffset));
  AssertEquals(aISO+' offset is 0 when rejected',0,lOffset,0);
end;


procedure TTestISO8601.TestExtendedUTC;

begin
  CheckTimes('2021-05-22T','Z',EncodeDate(2021,5,22),0,False,False);
end;


procedure TTestISO8601.TestBasicUTC;

begin
  CheckTimes('20210522T','Z',EncodeDate(2021,5,22),0,True,False);
end;


procedure TTestISO8601.TestCommaFraction;

begin
  CheckTimes('2021-05-22T','Z',EncodeDate(2021,5,22),0,False,True);
  CheckTimes('20210522T','Z',EncodeDate(2021,5,22),0,True,True);
end;


procedure TTestISO8601.TestTimeZones;

begin
  CheckTimes('2021-05-22T','+02:00',EncodeDate(2021,5,22),-2,False,False);
  CheckTimes('2021-05-22T','-02:00',EncodeDate(2021,5,22),2,False,False);
  CheckTimes('2021-05-22T','+0200',EncodeDate(2021,5,22),-2,False,False);
  CheckTimes('2021-05-22T','-0200',EncodeDate(2021,5,22),2,False,False);
  CheckTimes('2021-05-22T','+02',EncodeDate(2021,5,22),-2,False,False);
  CheckTimes('2021-05-22T','-02',EncodeDate(2021,5,22),2,False,False);
end;


procedure TTestISO8601.TestNoTimeZone;

begin
  CheckTimes('2021-05-22T','',EncodeDate(2021,5,22),0,False,False);
  CheckISO('20210522T13',EncodeDateTime(2021,5,22,13,0,0,0));
  CheckISO('2021-05-22 13:57:49',EncodeDateTime(2021,5,22,13,57,49,0));
end;


procedure TTestISO8601.TestDateOnly;

begin
  CheckISO('2021-05-22',EncodeDate(2021,5,22));
  CheckISO('2021-05',EncodeDate(2021,5,1));
  CheckISO('2021/05/22',EncodeDate(2021,5,22));
  CheckISO('2021/05',EncodeDate(2021,5,1));
  CheckISO('2021',EncodeDate(2021,1,1));
  CheckISO('20210522',EncodeDate(2021,5,22));
end;


procedure TTestISO8601.TestTimeOnly;

begin
  CheckTimes('T','Z',0,0,False,False);
  CheckTimes('T','-02:00',0,2,False,False);
  CheckTimes('T','-0200',0,2,False,False);
  CheckTimes('T','-02',0,2,False,False);
end;


procedure TTestISO8601.TestInvalid;

begin
  CheckISOFails('');
  CheckISOFails('21-05-22T13:57:49.191021Z');
  CheckISOFails('210522T13:57:49.191021Z');
  CheckISOFails('202105');
  CheckISOFails('210502');
  CheckISOFails('20210522X13:57:491Z');
  CheckISOFails('not a date');
  CheckISOFails('2021-13-22');
  CheckISOFails('2021-05-22T25:00:00Z');
end;


procedure TTestISO8601.TestTimeOffset;

begin
  CheckOffset('2024-01-01T12:34:56+01:00','2024-01-01 11:34:56',1);
  CheckOffset('2024-01-01T12:34:56-05:30','2024-01-01 18:04:56',-5.5);
  CheckOffset('2024-01-01T12:34:56+0100','2024-01-01 11:34:56',1);
  CheckOffset('2024-01-01T12:34:56+01','2024-01-01 11:34:56',1);
  CheckOffset('2024-01-01T12:34:56.5+05:45','2024-01-01 06:49:56',5.75);
  CheckOffset('2024-01-01T00:30:00+02:00','2023-12-31 22:30:00',2);
  CheckOffset('2024-01-01T23:30:00-02:00','2024-01-02 01:30:00',-2);
  CheckOffset('2024-01-01T12:34:56Z','2024-01-01 12:34:56',0);
  CheckOffset('2024-01-01T12:34:56','2024-01-01 12:34:56',0);
  CheckOffset('2024-01-01','2024-01-01 00:00:00',0);
  CheckOffset('T12:00:00+02:00','1899-12-30 10:00:00',2);
  CheckOffsetFails('');
  CheckOffsetFails('2024-01-01T12:34:56+1:00');
  CheckOffsetFails('not a date');
end;


procedure TTestISO8601.TestDefault;

begin
  AssertEquals('Valid string',EncodeDate(2021,5,22),ISO8601ToDateDef('2021-05-22',-1),cEps);
  AssertEquals('Invalid string gives the default',-1,ISO8601ToDateDef('2021-xx-22',-1),0);
end;


{ TTestIncDateTime }

procedure TTestIncDateTime.CheckInc(const aName: string; aActual: TDateTime; const aExpected: string; aWithMs: Boolean);

begin
  if aWithMs then
    AssertEquals(aName,aExpected,FormatDateTime(cDateTimeFmt,aActual))
  else
    AssertEquals(aName,aExpected,FormatDateTime('yyyy-mm-dd hh:nn:ss',aActual));
end;


procedure TTestIncDateTime.TestIncWeek;

begin
  CheckInc('1899-12-31 +1 week',IncWeek(EncodeDateTime(1899,12,31,0,0,0,0),1),'1900-01-07 00:00:00');
  CheckInc('1899-12-31 -52 weeks',IncWeek(EncodeDateTime(1899,12,31,0,0,0,0),-52),'1899-01-01 00:00:00');
  CheckInc('1899-12-31 -10000 weeks',IncWeek(EncodeDateTime(1899,12,31,0,0,0,0),-10000),'1708-05-06 00:00:00');
  CheckInc('1899-12-31 -9999 weeks',IncWeek(EncodeDateTime(1899,12,31,0,0,0,0),-9999),'1708-05-13 00:00:00');
  CheckInc('1899-12-31 -9998 weeks',IncWeek(EncodeDateTime(1899,12,31,0,0,0,0),-9998),'1708-05-20 00:00:00');
  CheckInc('1899-12-30 +1 week',IncWeek(EncodeDateTime(1899,12,30,0,0,0,0),1),'1900-01-06 00:00:00');
  CheckInc('1899-12-30 -52 weeks',IncWeek(EncodeDateTime(1899,12,30,0,0,0,0),-52),'1898-12-31 00:00:00');
  CheckInc('1899-12-30 -10000 weeks',IncWeek(EncodeDateTime(1899,12,30,0,0,0,0),-10000),'1708-05-05 00:00:00');
  CheckInc('1899-12-30 -9999 weeks',IncWeek(EncodeDateTime(1899,12,30,0,0,0,0),-9999),'1708-05-12 00:00:00');
  CheckInc('1899-12-30 -9998 weeks',IncWeek(EncodeDateTime(1899,12,30,0,0,0,0),-9998),'1708-05-19 00:00:00');
  CheckInc('1899-12-30 -1171 weeks',IncWeek(EncodeDateTime(1899,12,30,0,0,0,0),-1171),'1877-07-21 00:00:00');
end;


procedure TTestIncDateTime.TestIncDay;

begin
  CheckInc('1899-12-31 +1 day',IncDay(EncodeDateTime(1899,12,31,0,0,0,0),1),'1900-01-01 00:00:00');
  CheckInc('1877-07-26 +8193 days',IncDay(EncodeDateTime(1877,7,26,0,0,0,0),8193),'1899-12-31 00:00:00');
  CheckInc('1877-07-26 06:00 +8193 days',IncDay(EncodeDateTime(1877,7,26,6,0,0,0),8193),'1899-12-31 06:00:00');
  CheckInc('1899-12-31 -1 day',IncDay(EncodeDateTime(1899,12,31,0,0,0,0),-1),'1899-12-30 00:00:00');
  CheckInc('1899-12-31 -8192 days',IncDay(EncodeDateTime(1899,12,31,0,0,0,0),-8192),'1877-07-27 00:00:00');
  CheckInc('1899-12-31 -8193 days',IncDay(EncodeDateTime(1899,12,31,0,0,0,0),-8193),'1877-07-26 00:00:00');
  CheckInc('1899-12-30 -1 day',IncDay(EncodeDateTime(1899,12,30,0,0,0,0),-1),'1899-12-29 00:00:00');
  CheckInc('1899-12-30 06:30 -1 day',IncDay(EncodeDateTime(1899,12,30,6,30,0,0),-1),'1899-12-29 06:30:00');
  CheckInc('1899-12-30 -8193 days',IncDay(EncodeDateTime(1899,12,30,0,0,0,0),-8193),'1877-07-25 00:00:00');
  CheckInc('1899-12-30 06:00 -8193 days',IncDay(EncodeDateTime(1899,12,30,6,0,0,0),-8193),'1877-07-25 06:00:00');
end;


procedure TTestIncDateTime.TestIncHour;

var
  lDay: Word;

begin
  for lDay:=21 to 31 do
    CheckInc(Format('1899-12-%d 23:00 +1 hour',[lDay]),IncHour(EncodeDateTime(1899,12,lDay,23,0,0,0),1),
      FormatDateTime('yyyy-mm-dd hh:nn:ss',EncodeDate(1899,12,lDay)+1));
  CheckInc('1899-12-20 23:05 +1 hour',IncHour(EncodeDateTime(1899,12,20,23,5,0,0),1),'1899-12-21 00:05:00');
  CheckInc('1899-12-29 15:00 +9 hours',IncHour(EncodeDateTime(1899,12,29,15,0,0,0),9),'1899-12-30 00:00:00');
  CheckInc('1899-01-01 15:00 +1 hour',IncHour(EncodeDateTime(1899,1,1,15,0,0,0),1),'1899-01-01 16:00:00');
  for lDay:=19 to 31 do
    CheckInc(Format('1899-12-%d 01:00 -1 hour',[lDay]),IncHour(EncodeDateTime(1899,12,lDay,1,0,0,0),-1),
      Format('1899-12-%d 00:00:00',[lDay]));
  CheckInc('1899-12-18 01:05 -1 hour',IncHour(EncodeDateTime(1899,12,18,1,5,0,0),-1),'1899-12-18 00:05:00');
end;


procedure TTestIncDateTime.TestIncMinute;

var
  lDay: Word;

begin
  CheckInc('1899-12-30 23:20 +40 minutes',IncMinute(EncodeDateTime(1899,12,30,23,20,0,0),40),'1899-12-31 00:00:00');
  for lDay:=21 to 31 do
    CheckInc(Format('1899-12-%d 23:00 +60 minutes',[lDay]),IncMinute(EncodeDateTime(1899,12,lDay,23,0,0,0),60),
      FormatDateTime('yyyy-mm-dd hh:nn:ss',EncodeDate(1899,12,lDay)+1));
  CheckInc('1899-12-20 23:05:01 +60 minutes',IncMinute(EncodeDateTime(1899,12,20,23,5,1,0),60),'1899-12-21 00:05:01');
  CheckInc('1898-12-30 01:20 +40 minutes',IncMinute(EncodeDateTime(1898,12,30,1,20,0,0),40),'1898-12-30 02:00:00');
  for lDay:=19 to 31 do
    CheckInc(Format('1899-12-%d 01:00 -60 minutes',[lDay]),IncMinute(EncodeDateTime(1899,12,lDay,1,0,0,0),-60),
      Format('1899-12-%d 00:00:00',[lDay]));
  CheckInc('1899-12-18 01:05:01 -60 minutes',IncMinute(EncodeDateTime(1899,12,18,1,5,1,0),-60),'1899-12-18 00:05:01');
end;


procedure TTestIncDateTime.TestIncSecond;

var
  lDay: Word;

begin
  for lDay:=21 to 31 do
    CheckInc(Format('1899-12-%d 23:00 +3600 seconds',[lDay]),IncSecond(EncodeDateTime(1899,12,lDay,23,0,0,0),3600),
      FormatDateTime('yyyy-mm-dd hh:nn:ss',EncodeDate(1899,12,lDay)+1));
  CheckInc('1899-12-20 23:05:01 +3600 seconds',IncSecond(EncodeDateTime(1899,12,20,23,5,1,0),3600),'1899-12-21 00:05:01');
  CheckInc('1899-12-20 23:05:01 +59 seconds',IncSecond(EncodeDateTime(1899,12,20,23,5,1,0),59),'1899-12-20 23:06:00');
  CheckInc('1899-12-29 23:59:42 +18 seconds',IncSecond(EncodeDateTime(1899,12,29,23,59,42,0),18),'1899-12-30 00:00:00');
  CheckInc('1899-12-30 23:59:33 +27 seconds',IncSecond(EncodeDateTime(1899,12,30,23,59,33,0),27),'1899-12-31 00:00:00');
  for lDay:=19 to 31 do
    CheckInc(Format('1899-12-%d 01:00 -3600 seconds',[lDay]),IncSecond(EncodeDateTime(1899,12,lDay,1,0,0,0),-3600),
      Format('1899-12-%d 00:00:00',[lDay]));
  CheckInc('1899-12-18 01:05:01 -3600 seconds',IncSecond(EncodeDateTime(1899,12,18,1,5,1,0),-3600),'1899-12-18 00:05:01');
end;


procedure TTestIncDateTime.TestIncMilliSecond;

var
  lDay: Word;

begin
  CheckInc('1899-12-29 23:59:59.999 +1 ms',IncMilliSecond(EncodeDateTime(1899,12,29,23,59,59,999),1),'1899-12-30 00:00:00.000',True);
  CheckInc('1899-12-30 23:59:59.999 +1 ms',IncMilliSecond(EncodeDateTime(1899,12,30,23,59,59,999),1),'1899-12-31 00:00:00.000',True);
  CheckInc('1899-12-31 23:59:59.999 +1 ms',IncMilliSecond(EncodeDateTime(1899,12,31,23,59,59,999),1),'1900-01-01 00:00:00.000',True);
  for lDay:=21 to 31 do
    CheckInc(Format('1899-12-%d 23:00 +3600000 ms',[lDay]),IncMilliSecond(EncodeDateTime(1899,12,lDay,23,0,0,0),3600000),
      FormatDateTime(cDateTimeFmt,EncodeDate(1899,12,lDay)+1),True);
  CheckInc('1899-12-20 01:00:00.700 +300 ms',IncMilliSecond(EncodeDateTime(1899,12,20,1,0,0,700),300),'1899-12-20 01:00:01.000',True);
  CheckInc('1900-01-01 23:59:59.100 +900 ms',IncMilliSecond(EncodeDateTime(1900,1,1,23,59,59,100),900),'1900-01-02 00:00:00.000',True);
  CheckInc('1899-12-31 -1 ms',IncMilliSecond(EncodeDateTime(1899,12,31,0,0,0,0),-1),'1899-12-30 23:59:59.999',True);
  CheckInc('1899-12-30 -1 ms',IncMilliSecond(EncodeDateTime(1899,12,30,0,0,0,0),-1),'1899-12-29 23:59:59.999',True);
  CheckInc('1899-12-29 -1 ms',IncMilliSecond(EncodeDateTime(1899,12,29,0,0,0,0),-1),'1899-12-28 23:59:59.999',True);
  for lDay:=19 to 31 do
    CheckInc(Format('1899-12-%d 01:00 -3600000 ms',[lDay]),IncMilliSecond(EncodeDateTime(1899,12,lDay,1,0,0,0),-3600000),
      Format('1899-12-%d 00:00:00.000',[lDay]),True);
  CheckInc('1899-12-30 -30 ms',IncMilliSecond(EncodeDateTime(1899,12,30,0,0,0,0),-30),'1899-12-29 23:59:59.970',True);
  CheckInc('1899-12-30 00:00:01.029 -30 ms',IncMilliSecond(EncodeDateTime(1899,12,30,0,0,1,29),-30),'1899-12-30 00:00:00.999',True);
end;


procedure TTestIncDateTime.TestIncDouble;

var
  lPos, lNeg, lNegMs: TDateTime;

begin
  lPos:=EncodeDateTime(2024,1,1,6,0,0,0);
  lNeg:=EncodeDateTime(1899,12,25,6,0,0,0);
  lNegMs:=EncodeDateTime(1899,12,25,6,0,0,100);
  CheckInc('IncDay +1.5',IncDay(lPos,1.5),'2024-01-02 18:00:00.000',True);
  CheckInc('IncDay -1.5',IncDay(lPos,-1.5),'2023-12-30 18:00:00.000',True);
  CheckInc('IncDay +0.25 before 1899-12-30',IncDay(lNeg,0.25),'1899-12-25 12:00:00.000',True);
  CheckInc('IncDay -0.25 before 1899-12-30',IncDay(lNeg,-0.25),'1899-12-25 00:00:00.000',True);
  CheckInc('IncDay +5.5 to after 1899-12-30',IncDay(lNeg,5.5),'1899-12-30 18:00:00.000',True);
  CheckInc('IncDay -0.5 to before 1899-12-30',IncDay(EncodeDateTime(1899,12,30,6,0,0,0),-0.5),'1899-12-29 18:00:00.000',True);
  CheckInc('IncHour +1.5',IncHour(lPos,1.5),'2024-01-01 07:30:00.000',True);
  CheckInc('IncHour +1.5 before 1899-12-30',IncHour(lNeg,1.5),'1899-12-25 07:30:00.000',True);
  CheckInc('IncHour -6.5 before 1899-12-30',IncHour(lNeg,-6.5),'1899-12-24 23:30:00.000',True);
  CheckInc('IncHour -1.5 to before 1899-12-30',IncHour(EncodeDateTime(1899,12,30,1,0,0,0),-1.5),'1899-12-29 23:30:00.000',True);
  CheckInc('IncMinute +1.5',IncMinute(lPos,1.5),'2024-01-01 06:01:30.000',True);
  CheckInc('IncMinute +1.5 before 1899-12-30',IncMinute(EncodeDateTime(1899,12,20,23,59,0,0),1.5),'1899-12-21 00:00:30.000',True);
  CheckInc('IncMinute -0.5 before 1899-12-30',IncMinute(lNeg,-0.5),'1899-12-25 05:59:30.000',True);
  CheckInc('IncSecond +0.5',IncSecond(lPos,0.5),'2024-01-01 06:00:00.500',True);
  CheckInc('IncSecond +0.5 before 1899-12-30',IncSecond(lNeg,0.5),'1899-12-25 06:00:00.500',True);
  CheckInc('IncSecond -1.25 before 1899-12-30',IncSecond(lNeg,-1.25),'1899-12-25 05:59:58.750',True);
  CheckInc('IncMilliSecond +1500.0',IncMilliSecond(lPos,1500.0),'2024-01-01 06:00:01.500',True);
  CheckInc('IncMilliSecond -200.0 before 1899-12-30',IncMilliSecond(lNegMs,-200.0),'1899-12-25 05:59:59.900',True);
  AssertEquals('IncMilliSecond +0.5 span',0.5,MilliSecondSpan(lPos,IncMilliSecond(lPos,0.5)),1e-3);
  AssertEquals('IncMilliSecond +0.5 span before 1899-12-30',0.5,MilliSecondSpan(lNeg,IncMilliSecond(lNeg,0.5)),1e-3);
  CheckInc('IncDay integer amount',IncDay(lNeg,2),'1899-12-27 06:00:00.000',True);
  CheckInc('IncHour integer amount',IncHour(lNeg,-7),'1899-12-24 23:00:00.000',True);
end;


{ TTestDateTimeSpan }

procedure TTestDateTimeSpan.TestBetween;

begin
  AssertEquals('DaysBetween crossing zero',2,DaysBetween(1,IncDay(1,-2)));
  AssertEquals('DaysBetween crossing zero, reversed',2,DaysBetween(IncDay(1,-2),1));
  AssertEquals('DaysBetween positive',2,DaysBetween(1,IncDay(1,2)));
  AssertEquals('DaysBetween positive, reversed',2,DaysBetween(IncDay(1,2),1));
  AssertEquals('DaysBetween negative',2,DaysBetween(IncDay(1,-10),IncDay(1,-8)));
  AssertEquals('DaysBetween negative, reversed',2,DaysBetween(IncDay(1,-8),IncDay(1,-10)));
  AssertEquals('HoursBetween crossing zero',2,HoursBetween(0.25,IncHour(0.25,-2)));
  AssertEquals('HoursBetween crossing zero, reversed',2,HoursBetween(IncHour(0.25,-2),0.25));
  AssertEquals('HoursBetween positive',2,HoursBetween(0.25,IncHour(0.25,2)));
  AssertEquals('HoursBetween positive, reversed',2,HoursBetween(IncHour(0.25,2),0.25));
  AssertEquals('HoursBetween negative',2,HoursBetween(-1.25,IncHour(-1.25,-2)));
  AssertEquals('HoursBetween negative, reversed',2,HoursBetween(IncHour(-1.25,-2),-1.25));
  AssertEquals('MinutesBetween crossing zero',2,MinutesBetween(0.25,IncMinute(0.25,-2)));
  AssertEquals('MinutesBetween crossing zero, reversed',2,MinutesBetween(IncMinute(0.25,-2),0.25));
  AssertEquals('MinutesBetween positive',2,MinutesBetween(0.25,IncMinute(0.25,2)));
  AssertEquals('MinutesBetween positive, reversed',2,MinutesBetween(IncMinute(0.25,2),0.25));
  AssertEquals('MinutesBetween -1.25',2,MinutesBetween(-1.25,IncMinute(-1.25,-2)));
  AssertEquals('MinutesBetween -1.25, reversed',2,MinutesBetween(IncMinute(-1.25,-2),-1.25));
  AssertEquals('MinutesBetween -0.25',2,MinutesBetween(-0.25,IncMinute(-0.25,-2)));
  AssertEquals('MinutesBetween -0.25, reversed',2,MinutesBetween(IncMinute(-0.25,-2),-0.25));
  AssertEquals('MinutesBetween -1.0',2,MinutesBetween(-1.0,IncMinute(-1.0,-2)));
  AssertEquals('MinutesBetween -1.0, reversed',2,MinutesBetween(IncMinute(-1.0,-2),-1.0));
end;


procedure TTestDateTimeSpan.TestDaysBetweenSteps;

var
  lLow, lNext: TDateTime;

begin
  lLow:=-3.75;
  while lLow<3 do
    begin
    lNext:=IncDay(lLow,2);
    AssertEquals(Format('DaysBetween(%.2f, %.2f)',[lLow,lNext]),2,DaysBetween(lLow,lNext));
    AssertEquals(Format('DaysBetween(%.2f, %.2f)',[lNext,lLow]),2,DaysBetween(lNext,lLow));
    lLow:=lNext;
    end;
end;


procedure TTestDateTimeSpan.TestIncRoundTrip;

const
  cValues: array[0..7] of TDateTime = (-1.0, -1.25, -1.5, -1.75, -2.0, -2.5, -10.999, 1.25);

var
  lValue: TDateTime;

begin
  for lValue in cValues do
    begin
    AssertEquals(Format('IncDay(%.3f, 0)',[lValue]),lValue,IncDay(lValue,0),1e-9);
    AssertEquals(Format('IncHour(%.3f, 0)',[lValue]),lValue,IncHour(lValue,0),1e-9);
    AssertEquals(Format('IncDay(IncDay(%.3f, -3), 3)',[lValue]),lValue,IncDay(IncDay(lValue,-3),3),1e-9);
    end;
end;


procedure TTestDateTimeSpan.TestIncAcrossZero;

begin
  AssertEquals('IncDay(1.25, -3)',-2.25,IncDay(1.25,-3),1e-9);
  AssertEquals('IncDay(-2.25, 3)',1.25,IncDay(-2.25,3),1e-9);
  AssertEquals('IncHour(-1.25, 6)',-1.5,IncHour(-1.25,6),1e-9);
  AssertEquals('IncHour(-1.5, -6)',-1.25,IncHour(-1.5,-6),1e-9);
end;


procedure TTestDateTimeSpan.TestSpans;

begin
  AssertEquals('DaySpan(-1.25, 0.25)',1.0,DaySpan(-1.25,0.25),1e-9);
  AssertEquals('DaySpan(-2.75, 1.75)',3.0,DaySpan(-2.75,1.75),1e-9);
  AssertEquals('DaySpan(-2.0, -1.0)',1.0,DaySpan(-2.0,-1.0),1e-9);
  AssertEquals('HourSpan(-1.25, -1.5)',6.0,HourSpan(-1.25,-1.5),1e-9);
  AssertEquals('HourSpan(-1.75, 0.25)',12.0,HourSpan(-1.75,0.25),1e-9);
end;


{ TTestDateTimeRange }

// True when DateTimeToStr of aValue raises EConvertError
function StrRaisesConvertError(aValue: TDateTime): Boolean;

begin
  Result:=False;
  try
    DateTimeToStr(aValue);
  except
    on EConvertError do
      Result:=True;
  end;
end;


// True when DateTimeToTimeStamp of aValue raises EConvertError
function StampRaisesConvertError(aValue: TDateTime): Boolean;

begin
  Result:=False;
  try
    DateTimeToTimeStamp(aValue);
  except
    on EConvertError do
      Result:=True;
  end;
end;


procedure TTestDateTimeRange.TestOutOfRange;

var
  lDT: TDateTime;

begin
  lDT:=IncHour(EncodeDateTime(2000,12,31,23,59,59,0),High(Int64));
  AssertTrue('DateTimeToStr of IncHour by High(Int64) raises EConvertError',StrRaisesConvertError(lDT));
  AssertTrue('DateTimeToTimeStamp of IncHour by High(Int64) raises EConvertError',StampRaisesConvertError(lDT));
  AssertTrue('DateTimeToTimeStamp of a large negative value raises EConvertError',StampRaisesConvertError(-lDT));
  AssertTrue('DateTimeToTimeStamp of MaxDateTime+1 raises EConvertError',StampRaisesConvertError(MaxDateTime+1));
  AssertTrue('DateTimeToTimeStamp of MinDateTime-1 raises EConvertError',StampRaisesConvertError(MinDateTime-1));
  AssertFalse('DateTimeToStr of a valid value does not raise',StrRaisesConvertError(EncodeDateTime(2000,12,31,23,59,59,0)));
end;


procedure TTestDateTimeRange.TestTimeStampLimits;

var
  lStamp: TTimeStamp;

begin
  lStamp:=DateTimeToTimeStamp(EncodeDateTime(9999,12,31,23,59,59,999));
  AssertEquals('Date of 9999-12-31 23:59:59.999',3652059,lStamp.Date);
  AssertEquals('Time of 9999-12-31 23:59:59.999',86399999,lStamp.Time);
  lStamp:=DateTimeToTimeStamp(EncodeDateTime(1,1,1,0,0,0,0));
  AssertEquals('Date of 0001-01-01',1,lStamp.Date);
  AssertEquals('Time of 0001-01-01',0,lStamp.Time);
  lStamp:=DateTimeToTimeStamp(EncodeDateTime(1,1,1,12,0,0,0));
  AssertEquals('Date of 0001-01-01 12:00',1,lStamp.Date);
  AssertEquals('Time of 0001-01-01 12:00',43200000,lStamp.Time);
end;


procedure TTestDateTimeRange.TestUnixDateTime;

begin
  AssertEquals('UnixToDateTime(15796372693)','2470-07-26 09:18:13.000',DTStr(UnixToDateTime(15796372693)));
  AssertEquals('DateTimeToUnix and back for 2345-12-12 04:45:49','2345-12-12 04:45:49.000',
    DTStr(UnixToDateTime(DateTimeToUnix(EncodeDateTime(2345,12,12,4,45,49,0)))));
  AssertEquals('DateTimeToUnix of 1970-01-01',0,DateTimeToUnix(EncodeDate(1970,1,1)));
  AssertEquals('UnixToDateTime(0)','1970-01-01 00:00:00.000',DTStr(UnixToDateTime(0)));
end;


initialization
  RegisterTests([TTestScanDateTime, TTestScanDateTimeDefault, TTestISO8601, TTestIncDateTime, TTestDateTimeSpan,
    TTestDateTimeRange]);
end.
