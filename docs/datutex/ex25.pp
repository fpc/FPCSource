Program Example25;

{ This program demonstrates the EndOfTheYear function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"Last day of this year : "dd mmmm yyyy';

Begin
  Writeln(FormatDateTime(Fmt,EndOfTheYear(Today)));
End.