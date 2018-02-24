uses
  SysUtils, DateUtils;
const
  Year = {$I %DATEYEAR%};
  Month = {$I %DATEMONTH%};
  Day = {$I %DATEDAY%};
  Hour = {$I %TIMEHOUR%};
  Minute = {$I %TIMEMINUTE%};
  Second = {$I %TIMESECOND%};
var
  Date, Time, DateTime: TDateTime;
begin
  Date := EncodeDate(Year, Month, Day);
  Time := EncodeTime(Hour, Minute, Second, 0);
  DateTime := ComposeDateTime(Date, Time);
  WriteLn('Built at: ', DateTimeToStr(DateTime));
  WriteLn('Time ago: ', SecondsBetween(DateTime, Now), ' seconds');
end.
