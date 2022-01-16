uses
  sysutils;

procedure Error(const S: string);
begin
  Writeln(S);
  Halt(1);
end;

var
  st: TSystemTime;
begin
  FillChar(st,SizeOf(st),$FF);
  GetLocalTime(st);
  Writeln('Local time: ',st.Year,'-',st.Month,'-',st.Day,' ',st.Hour,':',st.Minute,':',st.Second,'.',st.Millisecond);
  Writeln('Day of week: ', st.DayOfWeek);
  if (st.Year<1) or (st.Year>9999) then
    Error('Invalid year');
  if (st.Month<1) or (st.Month>12) then
    Error('Invalid month');
  if (st.Day<1) or (st.Day>MonthDays[IsLeapYear(st.Year),st.Month]) then
    Error('Invalid day');
  if (st.Hour<0) or (st.Hour>23) then
    Error('Invalid hour');
  if (st.Minute<0) or (st.Minute>59) then
    Error('Invalid minute');
  { 60 seconds can be valid, due to the existance of leap seconds }
  if (st.Second<0) or (st.Second>60) then
    Error('Invalid second');
  if (st.Millisecond<0) or (st.Millisecond>999) then
    Error('Invalid millisecond');
  if (st.DayOfWeek<0) or (st.DayOfWeek>6) then
    Error('Invalid day of week');
  if st.DayOfWeek<>(DayOfWeek(EncodeDate(st.Year,st.Month,st.Day))-1) then
    Error('Wrong day of week');
  Writeln('Ok');
end.
