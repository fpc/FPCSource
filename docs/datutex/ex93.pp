Program Example93;

{ This program demonstrates the RecodeMilliSecond function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss.zzz';

Var
  S : AnsiString;

Begin
  S:=FormatDateTime(Fmt,RecodeMilliSecond(Now,0));
  Writeln('This moment, milliseconds stripped : ',S);
End.