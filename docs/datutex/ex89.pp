Program Example89;

{ This program demonstrates the RecodeDay function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;

Begin
  S:=FormatDateTime(Fmt,RecodeDay(Now,1));
  Writeln('This moment on the first of the month : ',S);
End.