Program Example33;

{ This program demonstrates the EndOfTheWeek function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"last day of this week : "dd mmmm yyyy';

Begin
  Writeln(FormatDateTime(Fmt,EndOfTheWeek(Today)));
End.