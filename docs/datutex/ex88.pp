Program Example88;

{ This program demonstrates the RecodeMonth function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;

Begin
  S:=FormatDateTime(Fmt,RecodeMonth(Now,5));
  Writeln('This moment in May : ',S);
End.