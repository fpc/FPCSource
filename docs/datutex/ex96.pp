Program Example96;

{ This program demonstrates the RecodeDateTime function }

Uses SysUtils,DateUtils;

Const
  Fmt = 'dddd dd mmmm yyyy hh:nn:ss';

Var
  S : AnsiString;
  D : TDateTime ;

Begin
  D:=Now;
  D:=RecodeDateTime(D,2000,2,RecodeLeaveFieldAsIs,0,0,0,0);
  S:=FormatDateTime(Fmt,D);
  Writeln('This moment in februari 2000 : ',S);
End.