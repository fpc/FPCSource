Program Example87;

{ This program demonstrates the RecodeYear function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;

Begin
  S:=FormatDateTime(Fmt,RecodeYear(Now,1999));
  Writeln('This moment in 1999 : ',S);
End.