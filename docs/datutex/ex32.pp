Program Example32;

{ This program demonstrates the StartOfTheWeek function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"First day of this week : "dd mmmm yyyy';

Begin
  Writeln(FormatDateTime(Fmt,StartOfTheWeek(Today)));
End.