Program Example27;

{ This program demonstrates the EndOfAYear function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"Last day of this year : "dd mmmm yyyy';

Begin
  Writeln(FormatDateTime(Fmt,EndOfAYear(YearOf(Today))));
End.