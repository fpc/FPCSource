Program Example91;

{ This program demonstrates the RecodeMinute function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;

Begin
  S:=FormatDateTime(Fmt,RecodeMinute(Now,0));
  Writeln('This moment in the first minute of the hour: ',S);
End.