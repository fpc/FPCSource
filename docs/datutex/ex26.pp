Program Example26;

{ This program demonstrates the StartOfAYear function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"First day of this year : "dd mmmm yyyy';

Begin
  Writeln(FormatDateTime(Fmt,StartOfAYear(YearOf(Today))));
End.