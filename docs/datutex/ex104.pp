Program Example104;

{ This program demonstrates the NthDayOfWeek function }

Uses SysUtils,DateUtils;

Begin
  Write('Today is the ',NthDayOfWeek(Today),'-th ');
  Writeln(formatdateTime('dddd',Today),' of the month.');
End.