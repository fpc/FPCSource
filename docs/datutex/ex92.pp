Program Example92;

{ This program demonstrates the RecodeSecond function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;

Begin
  S:=FormatDateTime(Fmt,RecodeSecond(Now,0));
  Writeln('This moment, seconds stripped : ',S);
End.