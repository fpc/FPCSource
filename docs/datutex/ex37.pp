Program Example37;

{ This program demonstrates the EndOfTheDay function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"End of the day : "dd mmmm yyyy hh:nn:ss';


Begin
  Writeln(FormatDateTime(Fmt,EndOfTheDay(Today)));
End.