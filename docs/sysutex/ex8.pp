Program Example8;

{ This program demonstrates the DayOfWeek function }

Uses sysutils;

Begin
  Writeln ('Today''s day is ',LongDayNames[DayOfWeek(Date)]);
End.