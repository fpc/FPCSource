Program Example29;

{ This program demonstrates the EndOfTheMonth function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"last day of this month : "dd mmmm yyyy';

Begin
  Writeln(FormatDateTime(Fmt,EndOfTheMonth(Today)));
End.