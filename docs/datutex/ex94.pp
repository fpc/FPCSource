Program Example94;

{ This program demonstrates the RecodeDate function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;

Begin
  S:=FormatDateTime(Fmt,RecodeDate(Now,2001,1,1));
  Writeln('This moment on the first of the millenium : ',S);
End.