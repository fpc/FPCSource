Program Example28;

{ This program demonstrates the StartOfTheMonth function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"First day of this month : "dd mmmm yyyy';

Begin
  Writeln(FormatDateTime(Fmt,StartOfTheMonth(Today)));
End.