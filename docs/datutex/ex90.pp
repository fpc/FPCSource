Program Example90;

{ This program demonstrates the RecodeHour function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;

Begin
  S:=FormatDateTime(Fmt,RecodeHour(Now,0));
  Writeln('Today, in the first hour : ',S);
End.