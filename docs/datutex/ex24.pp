Program Example24;

{ This program demonstrates the StartOfTheYear function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"First day of this year : "dd mmmm yyyy';

Begin
  Writeln(FormatDateTime(Fmt,StartOfTheYear(Today)));
End.