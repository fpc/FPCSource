Program Example36;

{ This program demonstrates the StartOfTheDay function }

Uses SysUtils,DateUtils;

Const
  Fmt = '"Start of the day : "dd mmmm yyyy hh:nn:ss';


Begin
  Writeln(FormatDateTime(Fmt,StartOfTheDay(Today)));
End.