Program Example95;

{ This program demonstrates the RecodeTime function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;

Begin
  S:=FormatDateTime(Fmt,RecodeTime(Now,8,0,0,0));
  Writeln('Today, 8 AM : ',S);
End.
