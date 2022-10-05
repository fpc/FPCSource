program TestProject;

{$mode objfpc}
uses
  SysUtils, DateUtils;

type
  TBetweenFunc = function (const dt1, dt2: TDateTime): Int64;

// Re-implement here because we need an Int64 result.
function DaysBetween(const dt1, dt2: TDateTime): Int64;
begin
  Result := DateUtils.DaysBetween(dt1, dt2);
end;

// Performs the test
procedure Test(Caption: String; BetweenFunc: TBetweenFunc; dt1, dt2: TDateTime;
  Expected: Integer);
var
  res: Integer;
begin
  // Run the provided *Between function
  res := BetweenFunc(dt1, dt2);
  Write(Format('    %s(%s [%.6f], %s [%.6f]) = %d, expected: %d', [
    Caption, DateTimeToStr(dt1), dt1, dateTimeToStr(dt2), dt2, res, expected]));

  // Compare result with expected value
  if res = expected then
    WriteLn
  else
   begin
    WriteLn(' ---> ERROR');
    exitcode:=1;
   end;
end;

begin
  exitcode:=0;
  // ---------------------------------------------------------------------------
  // Testing DaysBetween
  // ---------------------------------------------------------------------------
  Writeln('DaysBetween');

  // Crossing zero date
  WriteLn('- Crossing zero date');
  Test('DaysBetween', @DaysBetween, 1, IncDay(1, -2), 2);
  Test('DaysBetween', @DaysBetween, IncDay(1, -2), 1, 2);

  // all positive
  WriteLn('- All dates positive');
  Test('DaysBetween', @DaysBetween, 1, IncDay(1, +2), 2);
  Test('DaysBetween', @DaysBetween, IncDay(1, +2), 1, 2);

  // all negative
  WriteLn('- All dates negative');
  Test('DaysBetween', @DaysBetween, IncDay(1, -10), IncDay(1, -8), 2);
  Test('DaysBetween', @DaysBetween, IncDay(1, -8), IncDay(1, -10), 2);

  // ---------------------------------------------------------------------------
  // Testing HoursBetween
  // ---------------------------------------------------------------------------
  WriteLn;
  WriteLn('HoursBetween');
  // Crossing zero date
  WriteLn('- Crossing zero date');
  Test('HoursBetween', @HoursBetween, 0.25, IncHour(0.25, -2), 2);
  Test('HoursBetween', @HoursBetween, IncHour(0.25, -2), 0.25, 2);

  // all positive
  WriteLn('- All dates positive');
  Test('HoursBetween', @HoursBetween, 0.25, IncHour(0.25, +2), 2);
  Test('HoursBetween', @HoursBetween, IncHour(0.25, +2), 0.25, 2);

  // all negative
  WriteLn('- All dates negative');
  Test('HoursBetween', @HoursBetween, -1.25, IncHour(-1.25, -2), 2);
  Test('HoursBetween', @HoursBetween, IncHour(-1.25, -2), -1.25, 2);

  // ---------------------------------------------------------------------------
  // Testing MinutesBetween
  // ---------------------------------------------------------------------------
  WriteLn;
  WriteLn('MinutesBetween');
  // Crossing zero date
  WriteLn('- Crossing zero date');
  Test('MinutesBetween', @MinutesBetween, 0.25, IncMinute(0.25, -2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(0.25, -2), 0.25, 2);

  // all positive
  WriteLn('- All dates positive');
  Test('MinutesBetween', @MinutesBetween, 0.25, IncMinute(0.25, +2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(0.25, +2), 0.25, 2);

  // all negative
  WriteLn('- All dates negative');
  Test('MinutesBetween', @MinutesBetween, -1.25, IncMinute(-1.25, -2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(-1.25, -2), -1.25, 2);
  Test('MinutesBetween', @MinutesBetween, -0.25, IncMinute(-0.25, -2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(-0.25, -2), -0.25, 2);
  Test('MinutesBetween', @MinutesBetween, -1.0, IncMinute(-1.0, -2), 2);
  Test('MinutesBetween', @MinutesBetween, IncMinute(-1.0, -2), -1.0, 2);

//  WriteLn('Press ENTER to close...');
//  ReadLn;
end.

